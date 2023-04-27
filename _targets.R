library(targets)

# load all targets
invisible(sapply(list.files("R", pattern = ".R$", full.names = TRUE), source))
invisible(sapply(list.files("R/data", pattern = ".R$", full.names = TRUE), source))

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("tidyverse", "janitor", "sf", "units", "glue", "patchwork"))

# load packages into session
if (interactive()) {
  sapply(tar_option_get("packages"), require, character.only = TRUE)
}

list(
  tar_target(data_dir, "/Users/jeff/Dropbox/Work/masswildlife/data/"),
  targets_crwa,
  targets_hoorwa,
  targets_irwa,
  targets_wqx,
  targets_ecosheds,
  targets_pie_lter,
  targets_hrf,
  targets_nwis_temp,
  targets_nwis_flow,

  targets_temp,
  targets_flow,

  targets_gis
)
