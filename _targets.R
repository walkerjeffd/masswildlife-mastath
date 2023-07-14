library(targets)

# load all targets
invisible(sapply(list.files("R", pattern = ".R$", full.names = TRUE), source))
invisible(sapply(list.files("R/obs", pattern = ".R$", full.names = TRUE), source))

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("tidyverse", "janitor", "sf", "units", "glue", "patchwork", "arrow", "tidymodels"))

# load packages into session
if (interactive()) {
  sapply(tar_option_get("packages"), require, character.only = TRUE)
}

list(
  tar_target(data_dir, "data"),
  targets_crwa,
  targets_hoorwa,
  targets_irwa,
  targets_wqx,
  targets_ecosheds,
  targets_pie_lter,
  targets_hrf,
  targets_neon,
  targets_nwis_temp,
  targets_nwis_flow,

  targets_temp,
  targets_flow,

  targets_gis,
  targets_nhdplusv2,
  targets_nhdplusv2_prism,
  targets_nhdplusv2_attrs,
  targets_nhdplushr,
  targets_streamstats,
  targets_daymet,
  targets_climate,

  targets_obs,
  targets_inp,

  targets_lom,
  targets_xgb,
  targets_reg,

  targets_app,
  targets_output,
  targets_memo
)
