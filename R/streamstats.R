
targets_streamstats <- list(
  tar_target(streamstats_params, {
    res <- httr::GET("https://streamstats.usgs.gov/streamstatsservices/parameters.json?rcode=MA")
    body <- httr::content(res, as = "parsed", encoding = "UTF-8")
    bind_rows(body$parameters) |>
      select(-ID)
  }),
  tar_target(streamstats_stn_file, "data/streamstats/stations.csv", format = "file"),
  tar_target(streamstats_stn, {
    read_csv(streamstats_stn_file, show_col_types = FALSE) |>
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  }),
  tar_target(streamstats_data_file, "data/streamstats/streamstats.rds", format = "file"),
  tar_target(streamstats_data, {
    read_rds(streamstats_data_file) |>
      unnest_wider(streamstats) |>
      filter(!duplicated(station_id))
  }),
  tar_target(streamstats_failed, {
    # 2x CTDEEP stations in CT (outside MA boundary)
    # 2x MADEP sites in tidal Mystic, Malden Rivers
    # Conte:Obear Brook Upper, no flowline?
    streamstats_data |>
      filter(is.na(workspaceID)) |>
      select(station_id, latitude, longitude) |>
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  })
  # tar_target(streamstats_daymet_plot_ts_2022, {
  #   streamstats_daymet |>
  #     unnest(daymet_basin) |>
  #     filter(year(date) == 2022, !is.na(tmin)) |>
  #     pivot_longer(-c(station_id, date)) |>
  #     group_by(date, name) |>
  #     summarise(
  #       min = min(value),
  #       mean = mean(value),
  #       max = max(value),
  #       .groups = "drop"
  #     ) |>
  #     ggplot(aes(date)) +
  #     geom_ribbon(aes(ymin = min, ymax = max), fill = "gray", alpha = 0.3) +
  #     geom_line(aes(y = mean)) +
  #     facet_wrap(vars(name), scales = "free_y", ncol = 1) +
  #     theme_minimal()
  # })
)