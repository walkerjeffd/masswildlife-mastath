# export all daily values for NECASC

source("_targets.R")

obs <- tar_read(memo_stn_paired) |>
  st_drop_geometry() |>
  transmute(
    station_id = str_c(temp_provider, temp_station_id, sep = ":"),
    station_name = temp_name,
    flow_station_id,
    flow_station_name = flow_name,
    distance_m,
    flow_data = list(filter(flow_data, !is.na(flow_cfs))),
    temp_data = list(temp_data),
    data = list({
      inner_join(flow_data, temp_data, by = "date") |>
        select(date, flow_cfs, min_temp_c, mean_temp_c, max_temp_c)
    })
  ) |>
  filter(distance_m < 100)

obs_paired <- obs |>
  select(station_id, data) |>
  mutate(
    data = list({
      data |>
        filter(!is.na(flow_cfs), !is.na(mean_temp_c))
    }),
    paired_count = nrow(data),
    paired_start = min(data$date),
    paired_end = max(data$date)
  )

temp_stn <- tar_read(temp_day) |>
  transmute(
    station_id = str_c(provider, station_id, sep = ":"),
    latitude,
    longitude
  )

stn <- obs |>
  select(station_id, station_name, flow_station_id, flow_station_name) |>
  left_join(
    temp_stn, by = "station_id"
  ) |>
  left_join(
    obs_paired |>
      select(-data),
    by = "station_id"
  )

stopifnot(
  all(!duplicated(stn$station_id)),
  all(!is.na(stn$latitude)),
  all(!is.na(stn$longitude))
)

values <- obs |>
  select(station_id, data) |>
  unnest(data) |>
  arrange(station_id, date) |>
  print()

values |>
  ggplot(aes(yday(date), mean_temp_c)) +
  geom_point(size = 0.2, alpha = 0.1)

values |>
  ggplot(aes(log10(pmax(flow_cfs, 0.01)), mean_temp_c)) +
  geom_point(size = 0.5, alpha = 0.2) +
  facet_wrap(vars(station_id))

summary(values)

write_csv(stn, "notes/20230905/ma-obs-paired-stn.csv")
write_csv(values, "notes/20230905/ma-obs-paired-day.csv")
