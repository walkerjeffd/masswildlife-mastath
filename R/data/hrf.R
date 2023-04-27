tar_option_set(packages = c("tidyverse", "janitor", "units"))

targets_hrf <- list(
  tar_target(hrf_stn, {
    tribble(
      ~station_id, ~name,                        ~type,
      "NB",        "Nelson Brook - Big Weir",    "stream",
      "NL",        "Nelson Brook - Little Weir", "stream",
      "BL",        "Bigelow Brook - Lower Pipe", "stream",
      "BU",        "Bigelow Brook - Upper Pipe", "stream",
      "BGS",       "Black Gum Swamp", "wetland",
      "BVS",       "Beaver Swamp", "wetland",
      "NT",        "Nelson Brook - Total",       "stream"
    ) |>
      mutate(
        latitude = 42.54,
        longitude = -72.19
      )
  }),
  tar_target(hrf_day_file, file.path(data_dir, "hrf", "files", "hf070-03-daily.csv"), format = "file"),
  tar_target(hrf_day, {
    read_csv(hrf_day_file, show_col_types = FALSE) |>
      distinct() |>
      select(-starts_with("f."), -jd) |>
      pivot_longer(-date, names_to = c("station_id", "name"), names_sep = "\\.") |>
      pivot_wider(values_fn = mean) |>
      transmute(
        date,
        station_id = toupper(station_id),
        flow_cfs = change_units(dis, "L/sec", "ft3/sec"),
        temp_c = wt
      )
  }),
  tar_target(hrf_15min_file, file.path(data_dir, "hrf", "files", "hf070-04-15min.csv"), format = "file"),
  tar_target(hrf_15min, {
    read_csv(hrf_15min_file, show_col_types = FALSE) |>
      distinct() |>
      select(-starts_with("f."), -jd) |>
      pivot_longer(-datetime, names_to = c("station_id", "name"), names_sep = "\\.") |>
      pivot_wider(values_fn = mean) |>
      transmute(
        datetime = with_tz(datetime + hours(5), tz = "US/Eastern"),
        station_id = toupper(station_id),
        flow_cfs = change_units(dis, "L/sec", "ft3/sec"),
        temp_c = wt
      )
  }),
  tar_target(hrf, {
    hrf_15min |>
      nest_by(station_id) |>
      left_join(hrf_stn, by = "station_id") |>
      relocate(data, .after = everything()) |>
      mutate(
        provider = "HRF",
        source = "HRF",
        .before = everything()
      )
  }),
  tar_target(hrf_temp, {
    hrf |>
      mutate(
        data = list({
          data |>
            select(datetime, temp_c) |>
            filter(!is.na(temp_c))
        })
      ) |>
      filter(nrow(data) > 0)
  }),
  tar_target(hrf_flow, {
    hrf |>
      mutate(
        data = list({
          data |>
            select(datetime, flow_cfs) |>
            filter(!is.na(flow_cfs))
        })
      ) |>
      filter(nrow(data) > 0, station_id != "NT")
  }),
  tar_target(hrf_flow_day, {
    hrf_flow |>
      mutate(
        data = list({
          data |>
            group_by(date = as_date(datetime)) |>
            summarise(flow_cfs = mean(flow_cfs))
        })
      )
  })
  # tar_target(hrf_flow_day, {
  #   hrf_day |>
  #     select(-temp_c) |>
  #     filter(!is.na(flow_cfs)) |>
  #     nest_by(station_id) |>
  #     left_join(hrf_stn, by = "station_id") |>
  #     relocate(data, .after = everything())
  # })
)