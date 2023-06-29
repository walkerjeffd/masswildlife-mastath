source("_targets.R")
library(tidyterra)
library(terra)
library(exactextractr)

tar_load(c(crs_ma_state_plane, gis_state))

# compute daymet mean by streamstats basin --------------------------------

daymet_tmin <- rast("data/daymet/tmin.tif")
st_crs(daymet_tmin)

ggplot() +
  geom_spatraster(data = daymet_tmin, aes(fill = tmin_1994_1)) +
  geom_sf(data = gis_state, fill = NA) +
  coord_sf(crs = crs_ma_state_plane) +
  scale_fill_viridis_c()

streamstats <- tar_read(streamstats_basin) |>
  select(-streamstats) |>
  ungroup() |>
  st_as_sf() |>
  st_transform(crs = st_crs(daymet_tmin)) |>
  head()

ggplot() +
  geom_sf(data = streamstats, fill = "orangered") +
  geom_sf(data = gis_state, fill = NA) +
  coord_sf(crs = crs_ma_state_plane)

extract_tmin <- exact_extract(daymet_tmin, streamstats, 'mean') |>
  as_tibble() |>
  bind_cols(station_id = streamstats$station_id)


streamstats_tmin <- extract_daymet(rast("data/daymet/tmin.tif"), streamstats)

map_df(c("tmin", "tmax", "prcp"), \(x) extract_daymet(rast(glue("data/daymet/{x}.tif")), streamstats)) |>
  pivot_wider()

extract_daymet <- function (rast, poly) {
  exact_extract(rast, poly, 'mean') |>
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