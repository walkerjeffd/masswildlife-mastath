tar_option_set(packages = c("tidyverse", "janitor", "units"))

targets_irwa <- list(
  tar_target(irwa_howlett_stn_xlsx_file, file.path("data", "obs", "irwa", "howlett", "Howlett Monitoring Sites.xlsx"), format = "file"),
  tar_target(irwa_howlett_stn, {
    readxl::read_excel(irwa_howlett_stn_xlsx_file) |>
      clean_names() |>
      transmute(
        station_id = site,
        name = water_body,
        # description = site_description,
        type = "stream",
        latitude,
        longitude
      )
  }),
  tar_target(irwa_howlett_data_xlsx_files, list.files(file.path("data", "obs", "irwa", "howlett", "data"), full.names = TRUE, pattern = ".xlsx$"), format = "file"),
  tar_target(irwa_howlett_data, {
    tibble(
      file = irwa_howlett_data_xlsx_files,
      filename = basename(file),
      data = map(file, function (x) {
        readxl::read_excel(x) |>
          clean_names()
      })
    ) |>
      unnest(data) |>
      transmute(
        station_id = site,
        datetime = with_tz(coalesce(date_time_gmt_0400, date_time_gmt_04_00) + hours(4), tz = "US/Eastern"),
        temp_c = change_units(temp_f, "degF", "degC")
      ) |>
      filter(!is.na(temp_c), !is.na(datetime))
  }),
  tar_target(irwa_howlett, {
    irwa_howlett_stn |>
      inner_join(
        irwa_howlett_data |>
          nest_by(station_id) |>
          filter(station_id != "HB"),
        by = "station_id"
      )
  }),
  tar_target(irwa_ipswich_xlsx_file, file.path("data", "obs", "irwa", "ipswich", "Ipswich-Parker Continuous Temp. Data.xlsx"), format = "file"),
  tar_target(irwa_ipswich_xlsx_raw, {
    readxl::read_excel(irwa_ipswich_xlsx_file) |>
      clean_names() |>
      filter(!is.na(result))
  }),
  tar_target(irwa_ipswich_stn, {
    irwa_ipswich_xlsx_raw |>
      filter(!is.na(station_id)) |>
      mutate(across(c(latitude, longitude), \(x) round(x, digits = 6))) |>
      distinct(
        station_id,
        name = water_body,
        # description = station_description,
        type = "stream",
        latitude,
        longitude
      )
  }),
  tar_target(irwa_ipswich_data, {
    irwa_ipswich_xlsx_raw |>
      filter(!is.na(station_id)) |>
      mutate(
        sample_date = format(sample_date, "%Y-%m-%d"),
        sample_time = coalesce(format(sample_time, "%H:%M:%S"), map_chr(sample_id, \(x) str_c(str_split_1(x, " ")[2:3], collapse = " ")))
      ) |>
      transmute(
        station_id,
        datetime = force_tz(
          ymd_hms(str_c(sample_date, sample_time, sep = " ")),
          tz = "US/Eastern"
        ),
        temp_c = result
      )
  }),
  tar_target(irwa_ipswich, {
    irwa_ipswich_stn |>
      inner_join(
        irwa_ipswich_data |>
          bind_rows(filter(irwa_howlett_data, station_id == "HB")) |>
          arrange(station_id, datetime) |>
          nest_by(station_id),
        by = "station_id"
      )
  }),
  tar_target(irwa, {
    bind_rows(irwa_howlett, irwa_ipswich) |>
      mutate(
        provider = "IRWA",
        source = "IRWA",
        .before = everything()
      )
  })
)