tar_option_set(packages = c("tidyverse", "janitor", "sf", "units"))

targets_gis <- list(
  tar_target(crs_ma_state_plane, 26986),
  tar_target(gis_state, {
    USAboundaries::us_states(states = "MA", resolution = "high") |>
      st_transform(crs = crs_ma_state_plane)
  }),
  tar_target(gis_state_map, {
    ggplot() +
      geom_sf(data = gis_state)
  }),
  tar_target(gis_dams, {
    dams::nid_subset |>
      filter(!is.na(longitude), !is.na(latitude)) |>
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
      st_transform(crs = crs_ma_state_plane) |>
      st_filter(gis_state)
  }),
  tar_target(gis_flowline, {
    nhdplusv2_flowline |>
      st_intersection(gis_state) |>
      st_cast("MULTILINESTRING")
  })
)
