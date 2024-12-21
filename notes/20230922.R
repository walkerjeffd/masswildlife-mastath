# export daymet for obs stations

source("_targets.R")
tar_load(obs_daymet_stn)
tar_load(obs_day)

obs_day <- tar_read(obs_day) |>
  select(station_id, data) |>
  rowwise() |>
  mutate(
    start = min(year(data$date)),
    end = max(year(data$date))
  ) |>
  select(-data)

obs_daymet_stn |>
  left_join(obs_day, by = "station_id") |>
  rowwise() |>
  mutate(
    daymet_stn = list({
      daymet_stn |>
        filter(year(date) >= start, year(date) <= end)
    })
  ) |>
  select(-start, -end) |>
  unnest(daymet_stn) |>
  write_csv("notes/20230922/mastath-stn-daymet.csv")

