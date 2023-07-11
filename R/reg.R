# regional predictions

tar_option_set(packages = c("tidyverse", "janitor", "sf", "units", "glue", "patchwork", "arrow", "tidymodels"))

targets_reg <- list(
  tar_target(reg_inp, {
    # all flowlines (area km2 < 1e4)
    # with attributes
    nhdplusv2_flowline |>
      st_drop_geometry() |>
      select(COMID, any_of(names(xgb_inp))) |>
      filter(TOT_BASIN_AREA < 1e4) |>
      na.omit() |>
      as_tibble()
  }),
  tar_target(reg_pred_xgb, {
    xgb_fit |>
      select(term, final_fit) |>
      mutate(
        data = list({
          x <- as.matrix(select(reg_inp, -COMID))
          p <- predict(extract_fit_parsnip(final_fit), x)
          bind_cols(reg_inp, p)
        }),
        xgb_pred = list(select(data, COMID, .pred))
      )
  }),
  tar_target(reg_pred_xgb_plot_hist, {
    reg_pred_xgb |>
      select(term, xgb_pred) |>
      unnest(xgb_pred) |>
      ggplot(aes(.pred)) +
      geom_histogram() +
      facet_wrap(vars(term), scales = "free_x")
  }),
  tar_target(reg_pred_xgb_plot_violin, {
    reg_pred_xgb |>
      select(term, xgb_pred) |>
      unnest(xgb_pred) |>
      ggplot(aes(.pred, 1)) +
      geom_violin(aes(fill = term), draw_quantiles = 0.5) +
      geom_jitter(
        data = xgb_inp,
        aes(x = value),
        size = 0.5, alpha = 0.2
      ) +
      scale_fill_brewer("Parameter", palette = "Set1", guide = "none") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
      facet_wrap(vars(term), scales = "free_x", ncol = 1, labeller = labeller_lom, strip.position = "left") +
      labs(x = "Estimated Value") +
      theme_bw() +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12)
      )
  }),
  tar_target(reg_pred, {
    prism <- nhdplusv2_prism |>
      pivot_longer(-c(COMID, month), values_to = "mean_airtemp_c") |>
      filter(month %in% 6:8) |>
      nest_by(COMID, .key = "prism")
    reg_pred_xgb |>
      select(term, xgb_pred) |>
      unnest(xgb_pred) |>
      pivot_wider(names_from = "term", values_from = ".pred") |>
      left_join(prism, by = "COMID") |>
      rowwise() |>
      mutate(
        pred = list({
          prism |>
            mutate(
              .pred = map_dbl(mean_airtemp_c, ~ lom3(., a, b, c)),
              thermal_jul = map_vec(.pred, thermal_class_jul)
            )
        })
      )
  }),
  tar_target(reg_pred_map_param, {
    x <- reg_pred |>
      select(COMID, a, b, c) |>
      pivot_longer(-COMID)
    gis_flowline |>
      select(COMID) |>
      crossing(name = c("a", "b", "c")) |>
      left_join(x, by = c("COMID", "name")) |>
      nest_by(name) |>
      mutate(
        plot = list({
          data |>
            st_as_sf() |>
            ggplot() +
            geom_sf(data = climate_huc8_simp, fill = NA, alpha = 0.1) +
            geom_sf(aes(color = value)) +
            scale_color_viridis_c("Estimate") +
            labs(title = labeller_lom(name)[[1]][[1]]) +
            theme_void()
        })
      ) |>
      pull(plot) |>
      wrap_plots(nrow = 2)

  }),
  tar_target(reg_pred_plot_hist, {
    reg_pred |>
      select(COMID, pred) |>
      unnest(pred) |>
      filter(name == "TOT_TAV7100") |>
      mutate(month = map_chr(month, ~ month.abb[[.]])) |>
      ggplot(aes(.pred, month, fill = month)) +
      geom_violin(draw_quantiles = 0.5) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
      scale_fill_brewer(palette = "Set1", guide = "none") +
      labs(x = "Predicted Mean Water Temp (degC)", y = NULL, subtitle = "1971-2000 Air Temp") +
      theme_bw()
  }),
  tar_target(reg_pred_map_temp_mon, {
    x <- reg_pred |>
      select(COMID, pred) |>
      unnest(pred) |>
      filter(name == "TOT_TAV7100")
    nhdplusv2_flowline |>
      select(COMID) |>
      crossing(month = 6:8) |>
      st_as_sf() |>
      left_join(x, by = c("COMID", "month")) |>
      mutate(
        month = map_chr(month, ~ month.abb[[.]]),
        month = fct_rev(month)
      ) |>
      ggplot() +
      geom_sf(data = gis_state, fill = NA) +
      geom_sf(aes(color = .pred)) +
      scale_color_viridis_c() +
      labs(subtitle = "Predicted Monthly July Water Temp based on 1971-2000 Mean Air Temp") +
      facet_wrap(vars(month)) +
      theme_void()
  }),
  tar_target(reg_pred_map_jul_30yr, {
    x <- reg_pred |>
      select(COMID, pred) |>
      unnest(pred) |>
      filter(month == 7, name == "TOT_TAV7100")
    y <- gis_flowline |>
      select(COMID) |>
      st_as_sf() |>
      left_join(x, by = c("COMID"))
    p1 <- y |>
      ggplot() +
      geom_sf(data = climate_huc8_simp, fill = NA, alpha = 0.25) +
      geom_sf(aes(color = fct_rev(cut(.pred, breaks = seq(0, 50, by = 2))))) +
      scale_color_brewer("Water Temp\n(degC)", palette = "RdYlBu", na.value = "grey80") +
      labs(title = "a) Predicted July Water Temp.") +
      theme_void()
    p2 <- y |>
      ggplot() +
      geom_sf(data = climate_huc8_simp, fill = NA, alpha = 0.25) +
      geom_sf(aes(color = thermal_jul)) +
      scale_color_manual("Thermal Class", values = thermal_class_colors, na.value = "grey80") +
      labs(title = "b) Thermal Classification") +
      theme_void()
    wrap_plots(list(p1, p2), ncol = 1)
  }),
  tar_target(reg_pred_map_jul, {
    x <- reg_pred |>
      select(COMID, pred) |>
      unnest(pred) |>
      filter(month == 7) |>
      mutate(name = factor(name, levels = c("TOT_TAV7100", "TOT_TAV_2016", "TOT_TAV_2017")))
    y <- gis_flowline |>
      select(COMID) |>
      crossing(name = levels(x$name)) |>
      mutate(name = factor(name, levels = levels(x$name))) |>
      st_as_sf() |>
      left_join(x, by = c("COMID", "name"))
    p1 <- y |>
      ggplot() +
      geom_sf(data = climate_huc8_simp, fill = NA, alpha = 0.25) +
      geom_sf(aes(color = fct_rev(cut(.pred, breaks = seq(0, 50, by = 2))))) +
      scale_color_brewer("Water Temp\n(degC)", palette = "RdYlBu", na.value = "grey80") +
      labs(title = "a) Predicted July Water Temp.") +
      facet_wrap(vars(name), ncol = 1, labeller = labeller(name = prism_labels)) +
      theme_void()
    p2 <- y |>
      ggplot() +
      geom_sf(data = climate_huc8_simp, fill = NA, alpha = 0.25) +
      geom_sf(aes(color = thermal_jul)) +
      scale_color_manual("Thermal Class", values = thermal_class_colors, na.value = "grey80") +
      labs(title = "b) Thermal Classification") +
      facet_wrap(vars(name), ncol = 1, labeller = labeller(name = prism_labels)) +
      theme_void()
    wrap_plots(list(p1, p2), nrow = 1)
  }),
  tar_target(reg_pred_plot_basin, {
    reg_pred |>
      select(COMID, pred) |>
      unnest(pred) |>
      filter(month == 7, name == "TOT_TAV7100") |>
      select(COMID, thermal_jul) |>
      left_join(
        climate_huc8_flowline |>
          mutate(
            length_m = units::drop_units(st_length(geometry))
          ) |>
          st_drop_geometry() |>
          select(basin_id, COMID, length_m),
        by = "COMID"
      ) |>
      group_by(basin_id, thermal_jul) |>
      summarise(
        n = n(),
        length_m = sum(length_m), .groups = "drop_last"
      ) |>
      mutate(
        frac_length_m = length_m / sum(length_m)
      ) |>
      ungroup() |>
      complete(
        basin_id, thermal_jul,
        fill = list(n = 0, length_m = 0, frac_length_m = 0)
      ) |>
      select(basin_id, thermal_jul, frac_length_m) |>
      left_join(select(climate_basins, basin_id, basin), by = "basin_id") |>
      pivot_wider(names_from = "thermal_jul", values_from = "frac_length_m") |>
      arrange(Cold) |>
      mutate(basin = fct_inorder(basin)) |>
      pivot_longer(-c(basin_id, basin)) |>
      mutate(name = factor(name, levels = names(thermal_class_colors))) |>
      ggplot(aes(value, basin)) +
      geom_col(aes(fill = name), position = position_stack()) +
      geom_vline(xintercept = c(0.2, 0.4, 0.6, 0.8), linewidth = 0.5, alpha = 0.3) +
      scale_fill_manual("Thermal Class", values = thermal_class_colors) +
      scale_x_continuous(breaks = scales::pretty_breaks(), expand = expansion(), label = scales::percent) +
      labs(x = "% River Miles", y = NULL) +
      theme_bw()
  }),
  tar_target(reg_pred_plot_streamorder, {
    x <- reg_pred_climate_basin |>
      left_join(
        select(nhdplusv2_enhd_nhdplusatts, comid, streamorder = streamorde),
        by = c("COMID" = "comid")
      ) |>
      group_by(streamorder, year, prob, thermal_jul) |>
      summarise(
        n = n(),
        length_m = sum(length_m), .groups = "drop_last"
      ) |>
      mutate(
        frac_length_m = length_m / sum(length_m)
      ) |>
      ungroup() |>
      complete(
        year, prob, thermal_jul,
        fill = list(n = 0, length_m = 0, frac_length_m = 0)
      ) |>
      arrange(year, prob) |>
      mutate(thermal_jul = fct_rev(thermal_jul)) |>
      filter(prob == "Q50", year == "Baseline", streamorder < 6)
    p1 <- x |>
      ggplot(aes(factor(streamorder), length_m / 1e3)) +
      geom_col(aes(fill = thermal_jul), position = position_stack()) +
      geom_hline(yintercept = seq(0, 1, by = 0.1), color = "gray50", alpha = 0.2, linewidth = 0.5) +
      scale_fill_manual("Thermal Class", values = thermal_class_colors) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand = expansion(c(0, 0.05)), labels = scales::comma) +
      labs(
        x = "Stream Order", y = "River Miles (km)",
        title = "a) River Miles (km)"
      ) +
      theme_bw() +
      theme(
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
      )
    p2 <- x |>
      ggplot(aes(factor(streamorder), length_m)) +
      geom_col(aes(fill = thermal_jul), position = position_fill()) +
      geom_hline(yintercept = seq(0, 1, by = 0.1), color = "gray50", alpha = 0.2, linewidth = 0.5) +
      scale_fill_manual("Thermal Class", values = thermal_class_colors) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::percent, expand = expansion()) +
      labs(
        x = "Stream Order", y = "% Total River Miles",
        title = "b) % Total River Miles"
      ) +
      theme_bw() +
      theme(
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
      )
    wrap_plots(list(p1, p2)) +
      plot_layout(guides = "collect")
  }),
  tar_target(reg_pred_climate, {
    x_climate_proj <- climate_proj |>
      filter(season == "summer") |>
      select(basin_id, season, year, starts_with("RCP")) |>
      pivot_longer(-c(basin_id, season, year), names_to = c("scenario", "prob"), names_sep = "_") |>
      pivot_wider(names_from = "prob")
    climate <- climate_huc8_flowline |>
      st_drop_geometry() |>
      select(COMID, basin_id) |>
      left_join(x_climate_proj, by = "basin_id", relationship = "many-to-many") |>
      nest_by(COMID, .key = "climate")
    reg_pred |>
      left_join(climate, by = "COMID") |>
      rowwise() |>
      mutate(
        climate = list({
          x_prism <- prism |>
            filter(name == "TOT_TAV7100") |>
            select(-name)
          x_climate <- climate |>
            pivot_longer(starts_with("Q"), names_to = "prob", values_to = "delta") |>
            select(year, scenario, prob, delta)
          crossing(x_prism, x_climate) |>
            mutate(fcst_mean_airtemp_c = mean_airtemp_c + delta)
        }),
        pred = list({
          climate |>
            mutate(
              .pred = map_dbl(fcst_mean_airtemp_c, ~ lom3(., a, b, c)),
              thermal_jul = map_vec(.pred, thermal_class_jul)
            )
        })
      )
  }),
  tar_target(reg_pred_climate_map_data, {
    x <- reg_pred_climate |>
      select(COMID, pred) |>
      unnest(pred) |>
      filter(scenario == "RCP85", month == "7") |>
      select(COMID, year, prob, mean_airtemp_c, delta, fcst_mean_airtemp_c, .pred, thermal_jul) |>
      mutate(
        year = factor(year),
        prob = factor(prob)
      )
    gis_flowline |>
      select(COMID) |>
      crossing(
        year = levels(x$year),
        prob = levels(x$prob)
      ) |>
      st_as_sf() |>
      left_join(x, by = c("COMID", "year", "prob"))
  }),
  tar_target(reg_pred_climate_map_temp, {
    reg_pred_climate_map_data |>
      ggplot() +
      geom_sf(data = climate_huc8_simp, fill = NA, alpha = 0.25) +
      geom_sf(aes(color = fct_rev(cut(.pred, breaks = seq(0, 50, by = 2))))) +
      scale_color_brewer("Water Temp\n(degC)", palette = "RdYlBu", na.value = "grey80") +
      facet_grid(vars(year), vars(prob), labeller = label_both) +
      theme_void() +
      theme(strip.text = element_text(size = 16))
  }),
  tar_target(reg_pred_climate_map_thermal, {
    reg_pred_climate_map_data |>
      ggplot() +
      geom_sf(data = climate_huc8_simp, fill = NA, alpha = 0.25) +
      geom_sf(aes(color = thermal_jul)) +
      scale_color_manual("Thermal Class", values = thermal_class_colors, na.value = "grey80") +
      facet_grid(vars(year), vars(prob), labeller = label_both) +
      theme_void() +
      theme(strip.text = element_text(size = 16))
  }),
  tar_target(reg_pred_climate_map_temp_q50, {
    reg_pred_climate_map_data |>
      filter(prob == "Q50") |>
      ggplot() +
      geom_sf(data = climate_huc8_simp, fill = NA, alpha = 0.25) +
      geom_sf(aes(color = fct_rev(cut(.pred, breaks = seq(0, 50, by = 2))))) +
      scale_color_brewer("Water Temp\n(degC)", palette = "RdYlBu", na.value = "grey80") +
      facet_wrap(vars(year)) +
      theme_void() +
      theme(strip.text = element_text(size = 14))
  }),
  tar_target(reg_pred_climate_map_thermal_q50, {
    reg_pred_climate_map_data |>
      filter(prob == "Q50") |>
      ggplot() +
      geom_sf(data = climate_huc8_simp, fill = NA, alpha = 0.25) +
      geom_sf(aes(color = thermal_jul)) +
      scale_color_manual("Thermal Class", values = thermal_class_colors, na.value = "grey80") +
      facet_wrap(vars(year)) +
      theme_void() +
      theme(strip.text = element_text(size = 14))
  }),
  tar_target(reg_pred_climate_basin, {
    x_proj <- reg_pred_climate |>
      select(COMID, pred) |>
      unnest(pred) |>
      filter(
        month == 7,
        scenario == "RCP85"
      ) |>
      select(COMID, year, prob, .pred, thermal_jul) |>
      mutate(year = as.character(year))
    x_base <- reg_pred |>
      select(COMID, pred) |>
      unnest(pred) |>
      filter(
        month == 7,
        name == "TOT_TAV7100"
      ) |>
      select(-month, -name, -mean_airtemp_c) |>
      crossing(
        prob = unique(x_proj$prob)
      ) |>
      mutate(year = "Baseline")
    bind_rows(
      x_base,
      x_proj
    ) |>
      left_join(
        climate_huc8_flowline |>
          mutate(
            length_m = units::drop_units(st_length(geometry))
          ) |>
          st_drop_geometry() |>
          select(basin_id, COMID, length_m),
        by = "COMID"
      ) |>
      select(basin_id, COMID, length_m, year, prob, .pred, thermal_jul) |>
      mutate(
        year = factor(year, levels = c("Baseline", sort(unique(x_proj$year))))
      )
  }),
  tar_target(reg_pred_climate_plot_q50, {
    reg_pred_climate_basin |>
      filter(prob == "Q50") |>
      group_by(basin_id, year, prob, thermal_jul) |>
      summarise(
        n = n(),
        length_m = sum(length_m), .groups = "drop_last"
      ) |>
      mutate(
        frac_length_m = length_m / sum(length_m)
      ) |>
      ungroup() |>
      complete(
        basin_id, year, prob, thermal_jul,
        fill = list(n = 0, length_m = 0, frac_length_m = 0)
      ) |>
      arrange(basin_id, year, prob) |>
      left_join(climate_basins, by = "basin_id") |>
      mutate(thermal_jul = fct_rev(thermal_jul)) |>
      ggplot(aes(year, frac_length_m)) +
      geom_col(aes(fill = thermal_jul), position = position_stack()) +
      geom_hline(yintercept = c(0.25, 0.5, 0.75), alpha = 0.2, linewidth = 0.5) +
      scale_fill_manual("Thermal Class", values = thermal_class_colors) +
      scale_y_continuous(labels = scales::percent, expand = expansion()) +
      facet_wrap(vars(basin)) +
      labs(
        x = "Prediction Period", y = "% Total River Miles"
        # subtitle = "Scenario: RCP8.5 (Q50)"
      ) +
      theme_bw() +
      theme(
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
      )
  }),
  tar_target(reg_pred_climate_plot_state, {
    reg_pred_climate_basin |>
      group_by(year, prob, thermal_jul) |>
      summarise(
        n = n(),
        length_m = sum(length_m), .groups = "drop_last"
      ) |>
      mutate(
        frac_length_m = length_m / sum(length_m)
      ) |>
      ungroup() |>
      complete(
        year, prob, thermal_jul,
        fill = list(n = 0, length_m = 0, frac_length_m = 0)
      ) |>
      arrange(year, prob) |>
      mutate(thermal_jul = fct_rev(thermal_jul)) |>
      ggplot(aes(year, length_m)) +
      geom_col(aes(fill = thermal_jul), position = position_fill()) +
      geom_hline(yintercept = seq(0, 1, by = 0.1), color = "gray50", alpha = 0.2, linewidth = 0.5) +
      scale_fill_manual("Thermal Class", values = thermal_class_colors) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::percent, expand = expansion()) +
      labs(
        x = "Prediction Period", y = "% Total River Miles"
      ) +
      facet_wrap(vars(prob), labeller = labeller(prob = c(
        "Q10" = "10th Percentile",
        "Q50" = "Median",
        "Q90" = "90th Percentile"
      ))) +
      theme_bw() +
      theme(
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
      )
  }),
  tar_target(reg_pred_climate_plot_streamorder, {
    reg_pred_climate_basin |>
      left_join(
        select(nhdplusv2_enhd_nhdplusatts, comid, streamorder = streamorde),
        by = c("COMID" = "comid")
      ) |>
      group_by(streamorder, year, prob, thermal_jul) |>
      summarise(
        n = n(),
        length_m = sum(length_m), .groups = "drop_last"
      ) |>
      mutate(
        frac_length_m = length_m / sum(length_m)
      ) |>
      ungroup() |>
      complete(
        streamorder, year, prob, thermal_jul,
        fill = list(n = 0, length_m = 0, frac_length_m = 0)
      ) |>
      arrange(year, prob) |>
      mutate(thermal_jul = fct_rev(thermal_jul)) |>
      filter(prob == "Q50", streamorder < 6) |>
      ggplot(aes(year, length_m)) +
      geom_col(aes(fill = thermal_jul), position = position_fill()) +
      geom_hline(yintercept = seq(0, 1, by = 0.1), color = "gray50", alpha = 0.2, linewidth = 0.5) +
      scale_fill_manual("Thermal Class", values = thermal_class_colors) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10), labels = scales::percent, expand = expansion()) +
      labs(
        x = "Prediction Period", y = "% Total River Miles"
      ) +
      facet_wrap(vars(streamorder), labeller = label_both) +
      theme_bw() +
      theme(
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
      )
  }),
  tar_target(reg_pred_climate_plot_basin, {
    reg_pred_climate_basin |>
      group_by(basin_id, year, prob, thermal_jul) |>
      summarise(
        n = n(),
        length_m = sum(length_m), .groups = "drop_last"
      ) |>
      mutate(
        frac_length_m = length_m / sum(length_m)
      ) |>
      ungroup() |>
      complete(
        basin_id, year, prob, thermal_jul,
        fill = list(n = 0, length_m = 0, frac_length_m = 0)
      ) |>
      arrange(basin_id, year, prob) |>
      left_join(select(climate_basins, basin_id, basin), by = "basin_id") |>
      filter(prob == "Q50") |>
      ggplot(aes(year, length_m)) +
      geom_col(aes(fill = thermal_jul), position = position_fill(), alpha = 0.9, width = 0.8) +
      scale_fill_manual("Thermal Class", values = thermal_class_colors) +
      scale_y_continuous(labels = scales::percent) +
      labs(
        x = "Prediction Period", y = "% Total River Miles"
      ) +
      facet_wrap(vars(basin)) +
      theme_bw() +
      theme(
        strip.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
      )
  }),
  tar_target(reg_pred_climate_map_delta_2050, {
    x_2050 <- reg_pred_climate |>
      select(COMID, pred) |>
      unnest(pred) |>
      filter(month == 7, year == 2050, scenario == "RCP85", prob == "Q50") |>
      select(COMID, .pred)
    x_base <- reg_pred |>
      select(COMID, pred) |>
      unnest(pred) |>
      filter(month == 7, name == "TOT_TAV7100") |>
      select(COMID, .pred)
    x <- bind_rows(
      base = x_base,
      p2050 = x_2050,
      .id = "name"
    ) |>
      pivot_wider(values_from = ".pred") |>
      mutate(delta = p2050 - base)
    gis_flowline |>
      select(COMID) |>
      left_join(x, by = "COMID") |>
      ggplot() +
      geom_sf(data = climate_huc8_simp, fill = NA, alpha = 0.25) +
      geom_sf(aes(color = fct_rev(cut(delta, breaks = seq(0, 3, by = 0.5))))) +
      scale_color_brewer("Change in\nWater Temp\n(degC)", palette = "RdYlBu", na.value = "grey80") +
      # geom_sf(aes(color = delta)) +
      # scale_color_distiller("Change in\nWater Temp\n(degC)", palette = "RdYlBu", na.value = "grey80") +
      theme_void()
  })
)

