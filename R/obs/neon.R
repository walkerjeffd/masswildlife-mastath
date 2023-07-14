targets_neon <- list(
  # tar_target(neon_stn, {
  #   tribble(
  #     ~station_id, ~name,       ~type,    ~latitude, ~longitude,
  #     "HOPB",      "Hop Brook", "stream", 42.471941, -72.329526
  #   )
  # }),
  tar_target(neon_temp_stn_file, file.path(data_dir, "obs", "neon", "NEON_temp-surfacewater", "NEON.D01.HOPB.DP1.20053.001.2017-07.basic.20230127T120753Z.RELEASE-2023", "NEON.D01.HOPB.DP1.20053.001.sensor_positions.20221205T011106Z.csv"), format = "file"),
  tar_target(neon_temp_stn_raw, read_csv(neon_temp_stn_file, col_types = cols(.default = col_character()))),
  tar_target(neon_temp_stn, {
    neon_temp_stn_raw |>
      transmute(
        station_id = "HOPB",
        sensor_id = str_remove(HOR.VER, ".100"),
        latitude = parse_number(referenceLatitude),
        longitude = parse_number(referenceLongitude)
      ) |>
      distinct()
  }),
  tar_target(neon_temp_files, list.files(file.path(data_dir, "obs", "neon", "NEON_temp-surfacewater"), recursive = TRUE, full.names = TRUE, pattern = "NEON.D01.HOPB.DP1.20053.001.*TSW_30min"), format = "file"),
  tar_target(neon_temp_raw, {
    tibble(
      filepath = neon_temp_files,
      filename = basename(neon_temp_files)
    ) |>
      rowwise() |>
      mutate(
        sensor_id = str_split_1(filename, "\\.")[[7]],
        data = list({
          read_csv(filepath, show_col_types = FALSE) |>
            mutate()
        })
      )
  }),
  tar_target(neon_temp_sensor_30min, {
    neon_temp_raw |>
      select(sensor_id, data) |>
      unnest(data) |>
      filter(finalQF == 0) |>
      select(
        sensor_id,
        start_datetime = startDateTime, end_datetime = endDateTime,
        min_temp_c = surfWaterTempMinimum, mean_temp_c = surfWaterTempMean,
        max_temp_c = surfWaterTempMaximum
      ) |>
      mutate(
        across(ends_with("datetime"), \(x) with_tz(x, "US/Eastern"))
      ) |>
      filter(!is.na(mean_temp_c)) |>
      nest_by(sensor_id)
  }),
  tar_target(neon_temp_sensor_day, {
    neon_temp_sensor_30min |>
      mutate(
        data = list({
          data |>
            group_by(date = as_date(start_datetime)) |>
            summarise(
              n_values = n(),
              min_temp_c = min(min_temp_c),
              mean_temp_c = mean(mean_temp_c),
              max_temp_c = max(max_temp_c)
            )
        })
      )
  }),
  tar_target(neon_temp_sensor_day_plot, {
    neon_temp_sensor_day |>
      unnest(data) |>
      ggplot(aes(date, mean_temp_c)) +
      geom_line(aes(color = sensor_id))
      # facet_wrap(vars(sensor), ncol = 1)
  }),
  tar_target(neon_temp_day, {
    # only keep 112, which has 4-year continuous period, and is closer than 111
    # to flow gage (temps at 111 and 112 differ in 2020)
    neon_temp_stn |>
      filter(sensor_id == "112") |>
      left_join(neon_temp_sensor_day, by = "sensor_id") |>
      select(-sensor_id) |>
      mutate(
        source = "NEON",
        provider = "NEON",
        name = "Hop Brook (Temp)",
        type = "stream",
        .before = everything()
      ) |>
      relocate(station_id, .after = "provider")
  }),
  tar_target(neon_flow_stn_file, file.path(data_dir, "obs", "neon", "NEON_discharge-continuous", "NEON.D01.HOPB.DP4.00130.001.2017-07.basic.20230127T120753Z.RELEASE-2023", "NEON.D01.HOPB.DP4.00130.001.sensor_positions.20230118T012115Z.csv"), format = "file"),
  tar_target(neon_flow_stn_raw, read_csv(neon_flow_stn_file, col_types = cols(.default = col_character()))),
  tar_target(neon_flow_stn, {
    neon_flow_stn_raw |>
      transmute(
        station_id = "HOPB",
        sensor_id = str_remove(HOR.VER, ".100"),
        latitude = parse_number(referenceLatitude),
        longitude = parse_number(referenceLongitude)
      ) |>
      distinct()
  }),
  tar_target(neon_flow_files, list.files(file.path(data_dir, "obs", "neon", "NEON_discharge-continuous"), recursive = TRUE, full.names = TRUE, pattern = "NEON.D01.HOPB.DP4.00130.001.*csd_continuousDischarge"), format = "file"),
  tar_target(neon_flow_raw, {
    tibble(
      filepath = neon_flow_files,
      filename = basename(neon_flow_files)
    ) |>
      rowwise() |>
      mutate(
        sensor_id = str_split_1(filename, "\\.")[[7]],
        data = list({
          read_csv(filepath, show_col_types = FALSE)
        })
      )
  }),
  tar_target(neon_flow_1min, {
    neon_flow_raw |>
      select(sensor_id, data) |>
      unnest(data) |>
      filter(is.na(dischargeFinalQF) | dischargeFinalQF == 0) |>
      transmute(
        sensor_id,
        datetime = with_tz(endDate, "US/Eastern"),
        flow_cfs = change_units(maxpostDischarge, "L/sec", "ft^3/sec")
      ) |>
      filter(!is.na(flow_cfs)) |>
      nest_by(sensor_id)
  }),
  tar_target(neon_flow_day, {
    x <- neon_flow_1min |>
      mutate(
        data = list({
          data |>
            group_by(date = as_date(datetime)) |>
            summarise(
              n_values = n(),
              flow_cfs = min(flow_cfs)
            )
        })
      )
    neon_flow_stn |>
      left_join(x, by = "sensor_id") |>
      select(-sensor_id) |>
      mutate(
        source = "NEON",
        provider = "NEON",
        name = "Hop Brook (Flow)",
        type = "stream",
        .before = everything()
      ) |>
      relocate(station_id, .after = "provider")
  }),
  tar_target(neon_flow_day_plot, {
    neon_flow_day |>
      unnest(data) |>
      ggplot(aes(date, flow_cfs)) +
      geom_line()
  })
)


