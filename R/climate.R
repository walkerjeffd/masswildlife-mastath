# climate projections
# https://resilientma-mapcenter-mass-eoeea.hub.arcgis.com/

tar_option_set(packages = c("tidyverse", "janitor", "sf", "units", "glue", "patchwork", "arrow", "tidymodels"))

targets_climate <- list(
  tar_target(climate_huc8_file, "data/climate/WBD_HUC8_for_climate_projections.shp", format = "file"),
  tar_target(climate_huc8, {
    st_read(climate_huc8_file) |>
      st_transform(crs = crs_ma_state_plane) |>
      st_make_valid() |>
      st_simplify(10, preserveTopology = TRUE) |>
      clean_names() |>
      select(-starts_with("shape"), -huc8_state) |>
      left_join(
        select(climate_proj, basin_id, basin),
        by = "basin_id"
      )
  }),
  tar_target(climate_huc8_simp, {
    st_simplify(climate_huc8, 100, preserveTopology = TRUE)
  }),
  tar_target(climate_huc8_flowline, {
    x_climate <- select(climate_huc8, basin_id, huc8_name)
    x_intersect <- nhdplusv2_flowline |>
      select(COMID) |>
      st_intersection(x_climate) |>
      filter(!duplicated(COMID))
    x_missing <- nhdplusv2_flowline |>
      select(COMID) |>
      filter(!COMID %in% x_intersect$COMID)
    nearest_index <- st_nearest_feature(x_missing, x_climate)
    x_missing$basin_id <- x_climate$basin_id[nearest_index]
    bind_rows(x_intersect, x_missing) |>
      mutate(length_m = drop_units(st_length(geometry)))
  }),
  tar_target(climate_proj_file, "data/climate/Climate_Projections_Dataset_Hub.xlsx", format = "file"),
  tar_target(climate_proj, {
    x <- readxl::read_excel(climate_proj_file) |>
      select(-starts_with("Shape")) |>
      rename(Basin = Basin...6) |>
      select(-starts_with("Basin..."))
    x_baseline <- x |>
      select(OBJECTID, starts_with("Baseline")) |>
      pivot_longer(-OBJECTID, names_to = c("scenario", "season", "year"), names_sep = "_") |>
      pivot_wider(names_from = "scenario") |>
      rename(baseline = Baseline)
    x_rcp <- x |>
      select(OBJECTID, starts_with("RCP")) |>
      rename(RCP8_5_90pct_fall_2090 = RCP8_5_90pct_2090) |>
      pivot_longer(-OBJECTID, names_to = c("rpc1", "rpc2", "prob", "season", "year"), names_sep = "_") |>
      unite(scenario, c("rpc1", "rpc2"), sep = "") |>
      mutate(prob = paste0("Q", parse_number(prob))) |>
      unite(scenario, c("scenario", "prob")) |>
      pivot_wider(names_from = "scenario")
    x_all <- x_baseline |>
      left_join(x_rcp, by = c("OBJECTID", "season", "year"))
    x |>
      clean_names() |>
      select(objectid, basin_id, basin, huc8, huc8_name) |>
      left_join(x_all, by = c("objectid" = "OBJECTID")) |>
      select(-objectid) |>
      mutate(
        year = parse_number(year),
        baseline = change_units(baseline, "degF", "degC"),
        across(c(starts_with("RCP")), \(x) x * 5 / 9)
      )
  }),
  tar_target(climate_proj_plot, {
    x_rcp <- climate_proj |>
      select(basin, season, year, baseline, starts_with("RCP")) |>
      pivot_longer(-c(basin, season, year, baseline), names_to = c("scenario", "prob"), names_sep = "_", values_to = "delta") |>
      mutate(value = delta + baseline) |>
      pivot_longer(c(delta, value)) |>
      pivot_wider(names_from = "prob")
    x_rcp |>
      filter(
        season == "summer",
        name == "delta"
      ) |>
      mutate(scenario = fct_rev(scenario)) |>
      ggplot(aes(year)) +
      geom_ribbon(aes(ymin = Q10, ymax = Q90, group = scenario), alpha = 0.2) +
      geom_line(aes(y = Q50, color = scenario)) +
      geom_point(aes(y = Q50, color = scenario)) +
      scale_color_brewer("Emissions Scenario", palette = "Set1") +
      ylim(0, NA) +
      facet_wrap(vars(basin)) +
      labs(
        x = "Year (Center of 30-yr Averaging Periods)",
        y = "Projected Air Temp. Increase (degC)"
      ) +
      theme_bw()
  }),
  tar_target(climate_basins, {
    climate_proj |>
      distinct(basin_id, basin, huc8, huc8_name)
  })
)