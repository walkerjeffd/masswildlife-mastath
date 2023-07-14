# export datasets for web app

tar_option_set(packages = c("tidyverse", "janitor", "sf", "units", "glue", "patchwork", "arrow", "tidymodels"))

targets_app <- list(
  tar_target(app_dir, "data/output/app/"),
  tar_target(app_basins, {
    climate_huc8 |>
      st_simplify(preserveTopology = TRUE, dTolerance = 100) |>
      st_transform(crs = 4326) |>
      select(-objectid)
  }),
  tar_target(app_basins_file, {
    fname <- file.path(app_dir, "basins.geojson")
    if (file.exists(fname)) {
      unlink(fname)
    }
    st_write(app_basins, fname, layer_options = c("COORDINATE_PRECISION=5", "ID_FIELD=id"))
    fname
  }, format = "file"),
  tar_target(app_flowlines, {
    gis_flowline |>
      st_transform(crs = 4326) |>
      select(COMID) |>
      left_join(
        app_dataset,
        by = "COMID"
      ) |>
      mutate(
        across(
          -c(COMID, GNIS_NAME, basin_id, geometry),
          \(x) round(x, digits = 2)
        )
      )
  }),
  tar_target(app_flowlines_file, {
    fname <- file.path(app_dir, "flowlines.geojson")
    if (file.exists(fname)) {
      unlink(fname)
    }
    st_write(app_flowlines, fname, layer_options = c("COORDINATE_PRECISION=5", "ID_FIELD=COMID"))

    fname
  }, format = "file"),
  tar_target(app_dataset, {
    reg_pred_climate_basin |>
      select(COMID, basin_id, year, prob, value = .pred) |>
      mutate(
        year = fct_recode(year, BASE = "Baseline")
      ) |>
      unite(name, c("year", "prob")) |>
      pivot_wider() |>
      select(-BASE_Q10, -BASE_Q90) |>
      rename(BASE = BASE_Q50) |>
      left_join(
        nhdplusv2_flowline |>
          st_drop_geometry() |>
          select(COMID, GNIS_NAME),
        by = "COMID"
      ) |>
      relocate(GNIS_NAME, .after = COMID)
  })
)