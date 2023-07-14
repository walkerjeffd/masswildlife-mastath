# output dataset

targets_output <- list(
  tar_target(output_flowlines, {
    x_tair <- climate_proj |>
      filter(season == "summer") |>
      select(basin_id, year, starts_with("RCP85")) |>
      pivot_longer(-c(basin_id, year), names_to = c("scenario", "prob"), names_sep = "_") |>
      unite(name, c(scenario, year, prob)) |>
      mutate(name = str_c("TAIR_SUM_", name)) |>
      pivot_wider()
    gis_flowline |>
      st_transform(crs = 4326) |>
      select(all_of(names(reg_inp))) |>
      left_join(
        nhdplusv2_prism |>
          filter(month == 7) |>
          select(COMID, TOT_TAV7100_JUL = TOT_TAV7100),
        by = "COMID"
      ) |>
      left_join(
        app_dataset |>
          rename_with(\(x) str_c("TEMP_JUL_", x), .cols = -c("COMID", "GNIS_NAME", "basin_id")),
        by = "COMID"
      ) |>
      left_join(
        climate_basins |>
          select(-huc8_name),
        by = "basin_id"
      ) |>
      left_join(x_tair, by = "basin_id") |>
      relocate(starts_with("TEMP_"), geometry, .after = everything()) |>
      relocate(GNIS_NAME, basin_id, basin, huc8, .after = "COMID") |>
      rename(GEOMETRY = geometry) |>
      rename_with(toupper) |>
      rename(geometry = GEOMETRY) |>
      st_transform(4326)
  }),
  tar_target(output_flowlines_gpkg_file, {
    fname <- "data/output/dataset/mastath-flowlines.gpkg"
    st_write(output_flowlines, fname, append = FALSE)
    fname
  }, format = "file"),
  tar_target(output_flowlines_csv_file, {
    fname <- "data/output/dataset/mastath-flowlines.csv"
    output_flowlines |>
      st_drop_geometry() |>
      as_tibble() |>
      write_csv(fname)
    fname
  }, format = "file"),
  tar_target(output_obs_stn, {
    inp_day |>
      semi_join(lom_inp, by = "station_id") |>
      select(station_id, station_name, latitude, longitude, total_area_km2) |>
      left_join(
        obs_nhdplusv2_flowline |>
          st_drop_geometry() |>
          select(station_id, comid, snap_distance_m),
        by = "station_id"
      ) |>
      relocate(comid, .after = "station_id")
  }),
  tar_target(output_obs_stn_file, {
    fname <- "data/output/dataset/mastath-obs-stn.csv"
    write_csv(output_obs_stn, fname)
    fname
  }, format = "file"),
  tar_target(output_obs_day, {
    inp_day |>
      semi_join(lom_inp, by = "station_id") |>
      select(station_id, data) |>
      unnest(data) |>
      arrange(station_id, date)
  }),
  tar_target(output_obs_day_file, {
    fname <- "data/output/dataset/mastath-obs-day.csv"
    write_csv(output_obs_day, fname)
    fname
  }, format = "file"),
  tar_target(output_obs_week, {
    inp_week |>
      semi_join(lom_inp, by = "station_id") |>
      select(station_id, data) |>
      unnest(data) |>
      arrange(station_id, date)
  }),
  tar_target(output_obs_week_file, {
    fname <- "data/output/dataset/mastath-obs-week.csv"
    write_csv(output_obs_week, fname)
    fname
  }, format = "file"),
  tar_target(output_xgb_file, {
    fname <- "data/output/rds/xgb-model.rds"
    write_rds(xgb_fit, fname)
    fname
  }, format = "file"),
  tar_target(output_lom_file, {
    fname <- "data/output/rds/lom-model.rds"
    write_rds(lom_fit, fname)
    fname
  }, format = "file"),
  tar_target(output_obs_rds_file, {
    fname <- "data/output/rds/obs.rds"
    list(
      stn = obs_stn,
      data = obs_day
    ) |>
      write_rds(fname)
    fname
  })
)