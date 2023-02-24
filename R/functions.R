change_units <- function(x, from, to) {
  drop_units(set_units(set_units(x, from, mode = "standard"), to, mode = "standard"))
}

group_stations <- function (ids, max_stations = 10) {
  tibble(station_id = ids) |>
    mutate(station_group = ceiling(row_number() / max_stations)) |>
    group_by(station_group) |>
    tar_group()
}
