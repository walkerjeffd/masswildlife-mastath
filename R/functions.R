

# thermal -----------------------------------------------------------------

thermal_class_jul <- function (x) {
  # beauchene2014
  cut(x, c(-Inf, 18.45, 22.3, Inf), labels = c("Cold", "Cool", "Warm"), right = FALSE)
}

thermal_class_colors <- RColorBrewer::brewer.pal(5, "RdYlBu")[c(2,4,5)] |>
  setNames(nm = c("Warm", "Cool", "Cold"))

prism_labels <- c(
  "TOT_TAV7100" = "1971-2000 (30yr Normal)",
  "TOT_TAV_2016" = "2016",
  "TOT_TAV_2017" = "2017"
)

# utils -------------------------------------------------------------------

labeller_lom <- ggplot2::as_labeller(
  c(a="param: alpha", b="param: beta", c="param: gamma", m="param: mu"),
  ggplot2::label_parsed
)

change_units <- function(x, from, to) {
  drop_units(set_units(set_units(x, from, mode = "standard"), to, mode = "standard"))
}

group_stations <- function (ids, max_stations = 10) {
  tibble(station_id = ids) |>
    mutate(station_group = ceiling(row_number() / max_stations)) |>
    group_by(station_group) |>
    tar_group()
}


#' Get water year component of datetime(s)
#'
#' A water year is a 12-month period beginning in some month other than January.
#' The water year is designated by the calendar year in which it ends (e.g.
#' water year 2007 ends on 2007-09-30 if the starting month is October)
#'
#' @param x vector of datetime objects
#' @param start_month integer specifying first month of water year (default = 10 for October)
#'
#' @return vector of water years as decimal numbers
#' @export
#'
#' @examples
#' x <- seq.Date(from = as.Date("2007-09-30"), to = as.Date("2007-10-02"), by = "day")
#' water_year(x)
water_year <- function (x, start_month = 10) {
  if (start_month == 1) {
    return(lubridate::year(x))
  } else {
    return(
      ifelse(
        lubridate::month(x) >= start_month,
        lubridate::year(x) + 1,
        lubridate::year(x)
      )
    )
  }
}

# daymet ------------------------------------------------------------------

extract_daymet <- function (rast, poly) {
  exactextractr::exact_extract(rast, poly, 'mean') |>
    as_tibble() |>
    bind_cols(station_id = poly$station_id) |>
    pivot_longer(-station_id, names_to = c("name", "year", "jday"), names_pattern = "mean.(.+)_(.+)_(.+)") |>
    mutate(across(c(year, jday), as.numeric)) |>
    nest_by(station_id, name, year) |>
    mutate(
      data = list({
        x <- tibble(
          date = ymd(str_c(year, "0101")) + days(data$jday - 1),
          value = data$value
        )
        if (leap_year(year)) {
          x <- x |>
            add_row(
              date = ymd(str_c(year, "1231")),
              value = data$value[[365]]
            )
        }
        x
      })
    ) |>
    ungroup() |>
    select(-year) |>
    unnest(data)
}


# lom model ---------------------------------------------------------------

lom4 <- function (x, a, b, c, m) {
  m + (a - m) / (1 + exp(c * (b - x)))
}

lom3 <- function (x, a, b, c) {
  a / (1 + exp(c * (b - x)))
}

fit_lom4 <- function (x) {
  nls(mean_temp_c ~ lom4(mean_airtemp_c, a, b, c, m), start = list(a = 25, b = 12, c = 0.1, m = 0), data = x)
}

fit_lom3 <- function (x) {
  nls(mean_temp_c ~ lom3(mean_airtemp_c, a, b, c), start = list(a = 25, b = 12, c = 0.1), data = x)
}

gof_tibble <- function (sim, obs) {
  z <- hydroGOF::gof(sim, obs, digits = 5)
  pivot_wider(as_tibble(as.data.frame(z), rownames = "name"), values_from = V1)
}

