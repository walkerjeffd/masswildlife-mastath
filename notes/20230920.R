# export train/test split for NECASC

source("_targets.R")
tar_load(xgb_lom)

xgb_lom |>
  unnest_wider(gof) |>
  rowwise() |>
  mutate(
    start_year = min(data_week$year),
    end_year = max(data_week$year),
    n_year = length(unique(data_week$year)),
    n_week = nrow(data_week)
  ) |>
  ungroup() |>
  select(split, station_id, comid, start_year, end_year, n_year, n_week, RMSE, R2, NSE) |>
  write_csv("notes/20230920/mastath-gof-week.csv")
