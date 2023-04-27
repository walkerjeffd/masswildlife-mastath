tar_option_set(packages = c("tidyverse", "janitor", "units"))

targets_pie_lter <- list(
  tar_target(pie_lter_stn_file, file.path(data_dir, "pie-lter", "stations.csv")),
  tar_target(pie_lter_stn, {
    read_csv(pie_lter_stn_file, show_col_types = FALSE) |>
      rename(station_id = code) |>
      mutate(type = "stream", name = station_id)
  }),
  tar_target(pie_lter_csv_files, list.files(file.path(data_dir, "pie-lter", "files"), full.names = TRUE), format = "file"),
  tar_target(pie_lter_csv, {
    tibble(
      filepath = pie_lter_csv_files,
      filename = basename(filepath)
    ) |>
      rowwise() |>
      mutate(
        data = list(read_csv(filepath, col_types = cols(.default = col_character()), na = c("#NUM!", "#VALUE!")))
      )
  }),
  tar_target(pie_lter_cond, {
    pie_lter_csv |>
      filter(str_detect(filename, "Cond-")) |>
      mutate(
        station_id = str_split_1(str_remove_all(filename, "WAT-"), "-")[[1]],
        station_id = str_remove_all(station_id, "Cond")
      ) |>
      unnest(data) |>
      select(station_id, Date, Time, Temp) |>
      clean_names() |>
      transmute(
        station_id,
        datetime = case_when(
          str_starts(time, "0\\.") ~ ymd_hm(str_c(date, "00:00", sep = " ")) + minutes(floor(parse_number(time) * 24 * 60)),
          str_detect(date, "-") & nchar(time) > 5 ~ ymd_hms(str_c(date, time, sep = " ")),
          str_detect(date, "-") ~ ymd_hm(str_c(date, time, sep = " ")),
          nchar(time) > 5 ~ mdy_hms(str_c(date, time, sep = " ")),
          TRUE ~ mdy_hm(str_c(date, time, sep = " "))
        ),
        datetime = with_tz(datetime + hours(5), tz = "US/Eastern"),
        temp_c = parse_number(temp)
      ) |>
      filter(!is.na(temp_c))
  }),
  tar_target(pie_lter_do, {
    pie_lter_csv |>
      filter(str_detect(filename, "DO-")) |>
      mutate(
        station_id = str_split_1(str_remove_all(filename, "WAT-"), "-")[[1]]
      ) |>
      unnest(data) |>
      select(station_id, Date, Time, Temp) |>
      clean_names() |>
      transmute(
        station_id,
        datetime = case_when(
          str_starts(time, "0\\.") ~ ymd_hm(str_c(date, "00:00", sep = " ")) + minutes(floor(parse_number(time) * 24 * 60)),
          str_detect(date, "-") & nchar(time) > 5 ~ ymd_hms(str_c(date, time, sep = " ")),
          str_detect(date, "-") ~ ymd_hm(str_c(date, time, sep = " ")),
          nchar(time) > 5 ~ mdy_hms(str_c(date, time, sep = " ")),
          TRUE ~ mdy_hm(str_c(date, time, sep = " "))
        ),
        datetime = with_tz(datetime + hours(5), tz = "US/Eastern"),
        temp_c = parse_number(temp)
      ) |>
      filter(!is.na(temp_c))
  }),
  tar_target(pie_lter_ysi, {
    pie_lter_csv |>
      filter(str_starts(filename, "WAT-YSI")) |>
      mutate(
        station_id = str_split_1(str_remove_all(filename, "WAT-YSI-"), "-")[[1]]
      ) |>
      unnest(data) |>
      select(station_id, Date, Time, Temp, Depth, Flow, Discharge) |>
      mutate(across(c(Temp, Depth, Flow, Discharge), parse_number)) |>
      clean_names() |>
      transmute(
        station_id,
        datetime = case_when(
          str_starts(time, "0\\.") ~ ymd_hm(str_c(date, "00:00", sep = " ")) + minutes(floor(parse_number(time) * 24 * 60)),
          str_detect(date, "-") & nchar(time) > 5 ~ ymd_hms(str_c(date, time, sep = " ")),
          str_detect(date, "-") ~ ymd_hm(str_c(date, time, sep = " ")),
          nchar(time) > 5 ~ mdy_hms(str_c(date, time, sep = " ")),
          TRUE ~ mdy_hm(str_c(date, time, sep = " "))
        ),
        datetime = with_tz(datetime + hours(5), tz = "US/Eastern"),
        temp_c = temp,
        flow_cfs = change_units(coalesce(flow, discharge), "L/sec", "ft3/sec")
      ) |>
      filter(!is.na(temp_c), !is.na(flow_cfs))
  }),
  tar_target(pie_lter, {
    bind_rows(
      do = pie_lter_do,
      cond = pie_lter_cond,
      ysi = pie_lter_ysi,
      .id = "dataset"
    ) |>
      mutate(
        station_id = case_when(
          station_id == "Swamp" ~ "Swamp-CedarSwamp",
          station_id == "Forest" ~ "Forest-CartCreek",
          station_id == "Urban" ~ "Urban-SawMillBrk",
          TRUE ~ station_id
        )
      ) |>
      filter(
        dataset != "do",
        station_id %in% c("CentralStDamParker", "SylvaniaDamIpswich", "Swamp-CedarSwamp") | dataset == "ysi"
      ) |>
      select(-dataset) |>
      mutate(
        provider = "PIE-LTER",
        source = "PIE-LTER",
        .before = everything()
      ) |>
      nest_by(provider, source, station_id) |>
      left_join(pie_lter_stn, by = "station_id") |>
      relocate(data, .after = everything())
  }),
  tar_target(pie_lter_temp, {
    pie_lter |>
      mutate(
        data = list({
          data |>
            select(-flow_cfs) |>
            filter(!is.na(temp_c))
        })
      )
  }),
  tar_target(pie_lter_flow_day, {
    pie_lter |>
      mutate(
        data = list({
          data |>
            select(-temp_c) |>
            filter(!is.na(flow_cfs)) |>
            group_by(date = as_date(datetime)) |>
            summarise(flow_cfs = mean(flow_cfs))
        })
      ) |>
      filter(nrow(data) > 0)
  }),
  tar_target(pie_lter_day, {
    pie_lter |>
      mutate(
        data = list({
          data |>
            group_by(date = as_date(datetime)) |>
            summarise(across(c(temp_c, flow_cfs), \(x) mean(x, na.rm = TRUE)))
        })
      )
  }),
  tar_target(pie_lter_day_plot, {
    pie_lter_day |>
      unnest(data) |>
      pivot_longer(c(temp_c, flow_cfs), names_to = "var") |>
      ggplot(aes(date, value)) +
      geom_line() +
      facet_grid(vars(var), vars(station_id), scales = "free_y")
  })
)