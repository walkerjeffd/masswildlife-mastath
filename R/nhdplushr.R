# shapefiles generated from scripts/fetch-nhdplushr.R

tar_option_set(packages = c("tidyverse", "janitor", "sf", "units", "glue", "terra"))

targets_nhdplushr <- list(
  tar_target(nhdplushr_flowline_file, "data/nhdplushr/nhdflowline.shp", format = "file"),
  tar_target(nhdplushr_flowline, {
    st_read(nhdplushr_flowline_file) |>
      st_transform(crs = crs_ma_state_plane)
  }),
  tar_target(nhdplushr_waterbody_file, "data/nhdplushr/nhdwaterbody.shp", format = "file"),
  tar_target(nhdplushr_waterbody, {
    st_read(nhdplushr_waterbody_file) |>
      st_transform(crs = crs_ma_state_plane)
  }),
  tar_target(nhdplushr_huc8_file, "data/nhdplushr/wbdhu8.shp", format = "file"),
  tar_target(nhdplushr_huc8, {
    st_read(nhdplushr_huc8_file) |>
      st_transform(crs = crs_ma_state_plane)
  }),
  tar_target(nhdplushr_huc12_file, "data/nhdplushr/wbdhu12.shp", format = "file"),
  tar_target(nhdplushr_huc12, {
    st_read(nhdplushr_huc12_file) |>
      st_transform(crs = crs_ma_state_plane)
  })
)
