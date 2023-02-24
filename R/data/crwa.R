tar_option_set(packages = c("tidyverse", "janitor", "units"))

targets_crwa <- list(
  tar_target(crwa_xlsx_file, file.path(data_dir, "crwa", "DEPupload_2022_ConductivityTemp.xlsx"), format = "file"),
  tar_target(crwa_raw, {
    readxl::read_excel(crwa_xlsx_file) |>
      clean_names()
  }),
  tar_target(crwa_stn, {
    crwa_raw |>
      distinct(station_id, water_body, station_description, latitude, longitude) |>
      transmute(
        station_id,
        name = str_c(water_body, station_description, sep = " - "),
        type = factor("stream"),
        latitude,
        longitude
      )
  }),
  tar_target(crwa_data, {
    crwa_raw |>
      filter(analyte == "Temperature") |>
      transmute(
        station_id,
        datetime = force_tz(sample_date, tz = "US/Eastern"),
        temp_c = change_units(result, "degF", "degC")
      )
  }),
  tar_target(crwa, {
    crwa_stn |>
      mutate(
        provider = "CRWA",
        source = "CRWA",
        .before = everything()
      ) |>
      left_join(
        nest_by(crwa_data, station_id),
        by = "station_id"
      )
  })
)