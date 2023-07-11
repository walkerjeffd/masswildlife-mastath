tar_option_set(packages = c("tidyverse", "janitor", "sf", "units", "glue", "terra"))

targets_obs <- list(
  tar_target(obs_day, {
    temp_day |>
      rowwise() |>
      mutate(
        station_id = str_c(provider, station_id, sep = ":"),
        data = list(filter(data, year(date) >= 1994, year(date) <= 2022)),
        n_day = nrow(data)
      ) |>
      rename(station_name = name) |>
      filter(n_day > 0) |>
      select(-type) |>
      ungroup()
  }),
  tar_target(obs_day_tbl_stn, {
    obs_day |>
      unnest(data) |>
      mutate(
        provider = case_when(
          provider == "HAYDEN" ~ "USGS_Conte",
          provider == "MAFW_RQ" ~ "MAFW",
          TRUE ~ provider
        )
      ) |>
      group_by(source, provider) |>
      summarise(
        n_stations = length(unique(station_id)),
        n_days = n(),
        start_year = min(year(date)),
        end_year = max(year(date)),
        .groups = "drop"
      ) |>
      relocate(source, .after = everything())
  }),
  tar_target(obs_day_map_stn, {
    x <- obs_day |>
      rowwise() |>
      mutate(n_day = nrow(data), n_year = length(unique(year(data$date)))) |>
      select(station_id, source, n_day, n_year)
    obs_stn |>
      left_join(
        x,
        by = "station_id"
      ) |>
      ggplot() +
      geom_sf(data = climate_huc8_simp, fill = NA) +
      geom_sf(
        data = ~ filter(., source == "EcoSHEDS"),
        aes(size = n_year, fill = source), shape = 21, alpha = 0.35
      ) +
      geom_sf(
        data = ~ filter(., source == "USGS-NWIS"),
        aes(size = n_year, fill = source), shape = 21, alpha = 0.35
      ) +
      geom_sf(
        data = ~ filter(., !source %in% c("EcoSHEDS", "USGS-NWIS")),
        aes(size = n_year, fill = source), shape = 21, alpha = 0.75
      ) +
      scale_size_area("# Years", limits = c(1, NA), breaks = c(1, 5, 10, 15, 20)) +
      labs(fill = "Data Source") +
      guides(
        fill = guide_legend(override.aes = list(size = 3))
      ) +
      # geom_sf(aes(color = source, size = n_day)) +
      theme_void()
  }),
  tar_target(obs_day_tbl_stn_file, {
    fname <- "data/obs-stn.csv"
    write_csv(obs_day_tbl_stn, fname)
    fname
  }, format = "file"),
  tar_target(obs_stn, {
    obs_day |>
      select(station_id, latitude, longitude) |>
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
      st_transform(crs = crs_ma_state_plane)
  }),
  tar_target(obs_streamstats, {
    x <- streamstats_data |>
      filter(!is.na(workspaceID)) |>
      rowwise() |>
      mutate(
        point = list(featurecollection[[1]]$feature$features[[1]]),
        basin = list(featurecollection[[2]]$feature$features[[1]])
      )
    x_point <- jsonlite::toJSON(list(type = "FeatureCollection", features = x$point), auto_unbox = TRUE, pretty = TRUE) |>
      st_read(quiet = TRUE) |>
      bind_cols(station_id = x$station_id) |>
      st_as_sf() |>
      st_transform(crs = crs_ma_state_plane) |>
      nest_by(station_id, .key = "streamstats_point") |>
      ungroup()
    x_basin <- jsonlite::toJSON(list(type = "FeatureCollection", features = x$basin), auto_unbox = TRUE, pretty = TRUE) |>
      st_read(quiet = TRUE) |>
      bind_cols(station_id = x$station_id) |>
      st_as_sf() |>
      st_transform(crs = crs_ma_state_plane) |>
      nest_by(station_id, .key = "streamstats_basin") |>
      ungroup()
    full_join(x_point, x_basin, by = "station_id")
  }),
  tar_target(obs_streamstats_basin, {
    obs_streamstats |>
      select(station_id, streamstats_basin) |>
      unnest(streamstats_basin) |>
      st_as_sf() |>
      mutate(
        area_km2 = abs(units::drop_units(st_area(geometry)) / 1e6)
      )
  }),
  tar_target(obs_daymet_basin, {
    # basin-wide mean daily values
    basins <- obs_streamstats_basin |>
      select(station_id) |>
      st_transform(crs = daymet_crs)

    tibble(
      file = daymet_files,
      name = str_remove(basename(file), ".tif"),
      data = map(file, \(x) extract_daymet(terra::rast(x), basins))
    ) |>
      select(data) |>
      unnest(data) |>
      pivot_wider() |>
      nest_by(station_id, .key = "daymet_basin") |>
      ungroup()
  }),
  tar_target(obs_daymet_stn, {
    # daily values at each station
    x <- st_drop_geometry(obs_stn)
    latlon <- x[, c("longitude", "latitude")]
    xy <- as.matrix(latlon)
    p <- terra::vect(xy, crs="+proj=longlat +datum=WGS84")
    y <- map_df(daymet_files, function (filename) {
      r <- terra::rast(filename)
      x <- terra::extract(r, terra::project(p, terra::crs(r)), bind = TRUE)
      as.data.frame(x) |>
        mutate(id = row_number()) |>
        pivot_longer(-id, names_sep = "_", names_to = c("param", "year", "jday")) |>
        mutate(
          date = ymd(str_c(year, "0101")) + days(as.numeric(jday) - 1)
        ) |>
        select(id, date, param, value)
    })
    y |>
      pivot_wider(names_from = "param") |>
      nest_by(id, .key = "daymet_stn") |>
      ungroup() |>
      bind_cols(station_id = x$station_id) |>
      select(station_id, daymet_stn)
  }),
  tar_target(obs_stn_dams, {
    keep <- c("MADEP:W1139 [7066]", "MADEP:W0384 [7056]")
    st_filter(obs_stn, st_buffer(gis_dams, 100)) |>
      filter(!station_id %in% keep)
  }),
  tar_target(obs_stn_waterbodies, {
    keep <- c(
      "MADEP:W1819 [6945]", "USGS_Conte:Gates Brook [11903]",
      "MADEP:W1282 [6831]", "MADEP:W1818 [6971]"
    )
    waterbodies <- nhdplushr_waterbody |>
      filter(AreSqKM > 1, !FTYPE %in% c(466))
    obs_stn |>
      ungroup() |>
      st_filter(st_buffer(waterbodies, 100)) |>
      filter(!station_id %in% keep)
  }),
  tar_target(obs_nhdplushr_flowline, {
    # snap stations to NHDPlusHR flowlines
    x <- obs_stn
    nearest_index <- st_nearest_feature(x, nhdplushr_flowline)
    x$nearest_index <- nearest_index
    x$comid <- nhdplushr_flowline$COMID[nearest_index]
    x$flowline_distance_m <- units::drop_units(st_distance(x, nhdplushr_flowline[nearest_index, ], by_element = TRUE))

    vaa <- nhdplushr_flowline |>
      st_drop_geometry() |>
      select(COMID, FTYPE, FCODE, TtDASKM, AreSqKM, StrmOrd, Slope, MnElvSm, MxElvSm, ArboltS)

    x |>
      mutate(geom_stn = geometry) |>
      st_drop_geometry() |>
      rowwise() |>
      mutate(
        geom_flowline = nhdplushr_flowline$geometry[nearest_index],
        geom_nearest_points = st_nearest_points(geom_stn, geom_flowline),
        geom_snap = st_cast(geom_nearest_points, "POINT")[2],
        snap_distance_m = units::drop_units(st_distance(geom_stn, geom_snap, by_element = TRUE))
      ) |>
      ungroup() |>
      select(-nearest_index, -flowline_distance_m) |>
      left_join(vaa, by = c("comid" = "COMID")) |>
      relocate(starts_with("geom_"), .after = everything())
  }),
  tar_target(obs_nhdplushr_flowline_plot_distance, {
    obs_nhdplushr_flowline |>
      ggplot(aes(snap_distance_m)) +
      stat_ecdf() +
      scale_x_log10(breaks = c(0.01, 0.1, 1, 10, 100, 1000), labels = scales::comma, expand = expansion()) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::percent, expand = expansion()) +
      coord_cartesian(xlim = c(0.1, NA)) +
      labs(
        x = "snap distance (m)",
        y = "Cumulative Percentile",
        title = "Snap Distances to NHDPlusHR Flowlines"
      ) +
      theme_minimal()
  }),
  tar_target(obs_nhdplushr_flowline_plot_area, {
    obs_nhdplushr_flowline |>
      ggplot(aes(TtDASKM)) +
      stat_ecdf() +
      scale_x_log10(breaks = c(0.1, 1, 10, 100, 1000, 1e4), labels = scales::comma, expand = expansion()) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::percent, expand = expansion()) +
      coord_cartesian() +
      labs(
        x = "Drainage Area (km2)",
        y = "Cumulative Percentile",
        title = "Drainage Areas of Snapped NHDPlusHR Flowlines"
      ) +
      theme_minimal()
  }),
  tar_target(obs_compare_drainage_area, {
    x_streamstats <- obs_streamstats_basin |>
      select(station_id) |>
      st_drop_geometry()
    x_nhdplushr <- obs_nhdplushr_flowline |>
      st_drop_geometry() |>
      select(station_id, area_km2 = TtDASKM)
    x_nhdplusv2 <- obs_nhdplusv2_flowline |>
      st_drop_geometry() |>
      select(station_id, area_km2 = TOT_BASIN_AREA)
    bind_rows(
      streamstats = x_streamstats,
      nhdplushr = x_nhdplushr,
      nhdplusv2 = x_nhdplusv2,
      .id = "dataset"
    ) |>
      pivot_wider(names_from = "dataset", values_from = "area_km2")
  }),
  tar_target(obs_compare_drainage_area_splot, {
    obs_compare_drainage_area |>
      filter(nhdplushr < 1e4) |>
      GGally::ggpairs(columns = 2:4)
  }),
  tar_target(obs_nhdplusv2_flowline, {
    # snap stations to NHDPlusV2 flowlines
    x <- obs_stn
    nearest_index <- st_nearest_feature(x, nhdplusv2_flowline)
    x$nearest_index <- nearest_index
    x$comid <- nhdplusv2_flowline$COMID[nearest_index]
    x$flowline_distance_m <- units::drop_units(st_distance(x, nhdplusv2_flowline[nearest_index, ], by_element = TRUE))

    x |>
      mutate(geom_stn = geometry) |>
      st_drop_geometry() |>
      rowwise() |>
      mutate(
        geom_flowline = nhdplusv2_flowline$geometry[nearest_index],
        geom_nearest_points = st_nearest_points(geom_stn, geom_flowline),
        geom_snap = st_cast(geom_nearest_points, "POINT")[2],
        snap_distance_m = units::drop_units(st_distance(geom_stn, geom_snap, by_element = TRUE))
      ) |>
      ungroup() |>
      select(-nearest_index, -flowline_distance_m) |>
      left_join(
        nhdplusv2_flowline |>
          st_drop_geometry(),
        by = c("comid" = "COMID")
      ) |>
      relocate(starts_with("geom_"), .after = everything())
  }),
  tar_target(obs_plot_flowline_plot_distance, {
    obs_nhdplusv2_flowline |>
      ggplot(aes(snap_distance_m)) +
      stat_ecdf() +
      scale_x_log10(breaks = c(0.01, 0.1, 1, 10, 100, 1000), labels = scales::comma, expand = expansion()) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::percent, expand = expansion()) +
      coord_cartesian(xlim = c(0.1, NA)) +
      labs(
        x = "snap distance (m)",
        y = "Cumulative Percentile",
        title = "Snap Distances to NHDPlusV2 Flowlines"
      ) +
      theme_minimal()
  }),
  tar_target(obs_nhdplusv2_flowline_plot_area, {
    obs_nhdplusv2_flowline |>
      ggplot(aes(TOT_BASIN_AREA)) +
      stat_ecdf() +
      scale_x_log10(breaks = c(0.1, 1, 10, 100, 1000, 1e4), labels = scales::comma, expand = expansion()) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::percent, expand = expansion()) +
      coord_cartesian() +
      labs(
        x = "Drainage Area (km2)",
        y = "Cumulative Percentile",
        title = "Drainage Areas of Snapped NHDPlusV2 Flowlines"
      ) +
      theme_minimal()
  })
)
