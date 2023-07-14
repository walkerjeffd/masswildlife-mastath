tar_option_set(packages = c("tidyverse", "janitor", "sf", "units"))

targets_hoorwa <- list(
  tar_target(hoorwa_data_files, list.files(file.path(data_dir, "obs", "hoorwa", "data"), full.names = TRUE), format = "file"),
  tar_target(hoorwa_data, {
    x <- tibble(
      file = hoorwa_data_files,
      filename = basename(file),
      station_id = map_chr(filename, ~ str_split(., "\\+", simplify = TRUE)[[1]][1]),
      data = map(file, function (x) {
        read_csv(x, col_types = cols(.default = col_character())) |>
          clean_names()
      })
    ) |>
      select(station_id, data) |>
      unnest(data) |>
      transmute(
        station_id,
        datetime = lubridate::parse_date_time(
          coalesce(
            date_time_edt,
            date_time_gmt_04_00,
            date_time
          ),
          orders = c("mdy_HMS", "mdy_HM")
        ),
        temp_c = coalesce(
          as.numeric(ch_1_temperature_c),
          as.numeric(temperature_c),
          change_units(as.numeric(temp_f), "degF", "degC")
        )
      ) |>
      filter(!is.na(temp_c))
  }),
  tar_target(hoorwa_data_plot, {
    hoorwa_data |>
      ggplot(aes(datetime, temp_c)) +
      geom_line() +
      facet_wrap(vars(station_id))
  }),
  tar_target(hoorwa_stn_shp, file.path(data_dir, "obs", "hoorwa", "stations", "stations.shp"), format = "file"),
  tar_target(hoorwa_stn, {
    st_read(hoorwa_stn_shp) |>
      transmute(station_id = Station_ID, name = Waterbody, type = "stream") |>
      st_transform(crs = 4326) |>
      st_zm() |>
      mutate(
        longitude = st_coordinates(geometry)[,1],
        latitude = st_coordinates(geometry)[,2]
      ) |>
      st_drop_geometry() |>
      as_tibble()
  }),
  tar_target(hoorwa, {
    hoorwa_stn |>
      mutate(
        provider = "HOORWA",
        source = "HOORWA",
        .before = everything()
      ) |>
      left_join(
        nest_by(hoorwa_data, station_id),
        by = "station_id"
      )
  })
)