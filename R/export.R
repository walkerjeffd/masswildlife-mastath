# export dataset

targets_export <- list(
  tar_target(export_flowlines, {
    x_tair <- climate_proj |>
      filter(season == "summer") |>
      select(basin_id, year, starts_with("RCP85")) |>
      pivot_longer(-c(basin_id, year), names_to = c("scenario", "prob"), names_sep = "_") |>
      unite(name, c(scenario, year, prob)) |>
      mutate(name = str_c("TAIR_SUM_", name)) |>
      pivot_wider()
    gis_flowline |>
      st_transform(crs = 4326) |>
      select(all_of(names(reg_inp))) |>
      left_join(
        nhdplusv2_prism |>
          filter(month == 7) |>
          select(COMID, TOT_TAV7100_JUL = TOT_TAV7100),
        by = "COMID"
      ) |>
      left_join(
        app_dataset |>
          rename(BASE = BASE_Q10) |>
          select(-c(BASE_Q50, BASE_Q90)) |>
          rename_with(\(x) str_c("TEMP_JUL_", x), .cols = -c("COMID", "basin_id")),
        by = "COMID"
      ) |>
      left_join(
        climate_basins |>
          select(-huc8_name),
        by = "basin_id"
      ) |>
      left_join(x_tair, by = "basin_id") |>
      relocate(starts_with("TEMP_"), geometry, .after = everything()) |>
      relocate(basin_id, basin, huc8, .after = "COMID") |>
      rename(GEOMETRY = geometry, TOT_NLCD11_BARREN = TOT_NLCD11_BARRON) |>
      rename_with(toupper) |>
      rename(geometry = GEOMETRY) |>
      st_transform(4326)
  }),
  tar_target(export_flowlines_gpkg_file, {
    fname <- "data/export/mastath-flowlines.gpkg"
    st_write(export_flowlines, fname, append = FALSE)
    fname
  }, format = "file"),
  # tar_target(export_flowlines_shp_file, {
  #   fname <- "data/export/mastath-flowlines.shp"
  #   st_write(export_flowlines, fname, append = FALSE)
  #   fname
  # }, format = "file"),
  tar_target(export_flowlines_csv_file, {
    fname <- "data/export/mastath-output.csv"
    export_flowlines |>
      st_drop_geometry() |>
      as_tibble() |>
      write_csv(fname)
    fname
  }, format = "file"),
  tar_target(export_obs_stn, {
    inp_day |>
      semi_join(lom_inp, by = "station_id") |>
      select(station_id, station_name, latitude, longitude, total_area_km2) |>
      left_join(
        obs_nhdplusv2_flowline |>
          st_drop_geometry() |>
          select(station_id, comid, snap_distance_m),
        by = "station_id"
      ) |>
      relocate(comid, .after = "station_id")
  }),
  tar_target(export_obs_stn_file, {
    fname <- "data/export/mastath-obs-stn.csv"
    write_csv(export_obs_stn, fname)
    fname
  }, format = "file"),
  tar_target(export_obs_day, {
    inp_day |>
      semi_join(lom_inp, by = "station_id") |>
      select(station_id, data) |>
      unnest(data) |>
      arrange(station_id, date)
  }),
  tar_target(export_obs_day_file, {
    fname <- "data/export/mastath-obs-day.csv"
    write_csv(export_obs_day, fname)
    fname
  }, format = "file"),
  tar_target(export_obs_week, {
    inp_week |>
      semi_join(lom_inp, by = "station_id") |>
      select(station_id, data) |>
      unnest(data) |>
      arrange(station_id, date)
  }),
  tar_target(export_obs_week_file, {
    fname <- "data/export/mastath-obs-week.csv"
    write_csv(export_obs_week, fname)
    fname
  }, format = "file")
)