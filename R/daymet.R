tar_option_set(packages = c("tidyverse", "janitor", "sf", "units", "glue", "terra"))

targets_daymet <- list(
  tar_target(daymet_params, c("tmin", "tmax", "prcp")),
  tar_target(daymet_files, file.path("data", "daymet", str_c(daymet_params, ".tif")), format = "file"),
  tar_target(daymet_crs, st_crs(rast(daymet_files[[1]])))
)