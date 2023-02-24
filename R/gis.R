tar_option_set(packages = c("tidyverse", "janitor", "sf", "units"))

targets_gis <- list(
  tar_target(gis_states, {
    USAboundaries::us_states(states = "MA", resolution = "low")
  }),
  tar_target(gis_map, {
    ggplot() +
      geom_sf(
        data = gis_states
      )
  })
)