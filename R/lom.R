tar_option_set(packages = c("tidyverse", "janitor", "sf", "units", "glue", "patchwork", "arrow"))

# non-linear (logistic) air-temp model from Mohseni et al (1998)
# fit to weekly input data
targets_lom <- list(
  tar_target(lom_exclude_stations, c("NRWA:LPB48 [11226]", "NRWA:Wbtrib 49 (2) [11902]")),
  tar_target(lom_inp, {
    inp_week |>
      filter(
        n_week >= 25,
        frac_week_summer >= 0.3,
        frac_week_summer <= 0.8
      )
  }),
  tar_target(lom_fit, {
    lom_inp |>
      filter(!station_id %in% lom_exclude_stations) |>
      mutate(
        LOM4 = list(possibly(fit_lom4)(data)),
        LOM3 = list(possibly(fit_lom3)(data))
      ) |>
      pivot_longer(c(LOM4, LOM3), names_to = "model", values_to = "fit") |>
      rowwise() |>
      mutate(
        augment = list(broom::augment(fit)),
        stats = list(broom::glance(fit)),
        coef = list(broom::tidy(fit)),
        gof = list(possibly(gof_tibble)(augment$.fitted, augment$mean_temp_c))
      )
  }),
  tar_target(lom_gof, {
    lom_fit |>
      unnest(gof) |>
      select(model, NSE, KGE, R2, RMSE) |>
      pivot_longer(-model) |>
      mutate(
        name = factor(name, levels = c("RMSE", "R2", "NSE", "KGE"))
      ) |>
      ggplot(aes(value, model)) +
      geom_violin(aes(fill = model), draw_quantiles = c(0.5)) +
      geom_jitter(size = 0.5, alpha = 0.2) +
      scale_fill_brewer(palette = "Set1", guide = "none") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
      facet_wrap(vars(name), scales = "free_x", strip.position = "bottom") +
      labs(x = NULL, y = NULL) +
      theme_bw() +
      theme(
        strip.placement = "outside",
        strip.background = element_blank()
      )
  }),
  tar_target(lom_resid, {
    lom_fit |>
      select(model, augment) |>
      unnest(augment) |>
      ggplot(aes(.fitted, .resid)) +
      geom_point(size = 0.5, alpha = 0.1) +
      geom_hline(yintercept = 0, alpha = 0.5) +
      geom_vline(xintercept = 0, alpha = 0.5) +
      facet_wrap(vars(model)) +
      labs(
        x = "Predicted Water Temp (degC)",
        y = "Residual (Obs - Pred) Water Temp (degC)"
      ) +
      theme_bw() +
      theme(
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 12)
      )
  }),
  tar_target(lom_gof_pairs, {
    lom_fit |>
      select(model, gof) |>
      unnest(gof) |>
      select(model, NSE, KGE, R2, RMSE) |>
      GGally::ggpairs(mapping = aes(color = model, alpha = 0.5)) +
      scale_color_brewer(palette = "Set1")
  }),
  tar_target(lom_global_gof, {
    lom_fit |>
      ungroup() |>
      mutate(n_station = length(unique(station_id))) |>
      rowwise() |>
      select(model, n_station, augment) |>
      unnest(augment) |>
      nest_by(n_station, model) |>
      mutate(
        gof = list(possibly(gof_tibble)(data$.fitted, data$mean_temp_c)),
        n_weeks = nrow(data)
      ) |>
      unnest(gof) |>
      select(-data) |>
      pivot_longer(-model) |>
      pivot_wider(names_from = "model") |>
      filter(name %in% c("ME", "MAE", "RMSE", "R2", "NSE", "KGE", "n_weeks", "n_station")) |>
      mutate(
        name = factor(name, levels = c("n_station", "n_weeks", "ME", "MAE", "RMSE", "R2", "NSE", "KGE"))
      ) |>
      arrange(name)
  }),
  tar_target(lom_global_gof_file, {
    fname <- "data/output/report/lom-global-gof.csv"
    lom_global_gof |>
      write_csv(fname)
    fname
  }, format = "file"),
  tar_target(lom_coef_violin, {
    lom_fit |>
      unnest(coef) |>
      ggplot(aes(estimate, y = model)) +
      # geom_boxplot(aes(fill = model)) +
      geom_violin(aes(fill = model), draw_quantiles = 0.5) +
      geom_jitter(size = 0.5, alpha = 0.2) +
      scale_fill_brewer(palette = "Set1", guide = "none") +
      facet_wrap(vars(term), scales = "free_x", ncol = 2, labeller = labeller_lom) +
      labs(x = "Estimated Value", y = NULL) +
      theme_bw() +
      theme(
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 12)
      )
  }),
  tar_target(lom_coef_pairs, {
    lom_fit |>
      unnest(coef) |>
      select(station_id, model, term, estimate) |>
      pivot_wider(names_from = "term", values_from = "estimate") |>
      select(-station_id) |>
      GGally::ggpairs(mapping = aes(color = model, fill = model, alpha = 0.5), labeller = as_labeller(c(model="model", a="param: alpha", b="param: beta", c="param: gamma", m="param: mu"), label_parsed)) +
      scale_color_brewer(palette = "Set1") +
      scale_fill_brewer(palette = "Set1") +
      theme_bw() +
      theme(
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 12)
      )
  }),
  tar_target(lom_coef_splot_model, {
    lom_fit |>
      unnest(coef) |>
      select(station_id, model, term, estimate) |>
      pivot_wider(names_from = "term", values_from = "estimate") |>
      filter(b < 40) |>
      pivot_longer(-c(station_id, model), names_to = "term", values_to = "estimate") |>
      pivot_wider(names_from = "model", values_from = "estimate") |>
      filter(term != "m") |>
      ggplot(aes(LOM3, LOM4)) +
      geom_abline() +
      geom_point() +
      facet_wrap(vars(term), scales = "free") +
      theme(aspect.ratio = 1)
  }),
  tar_target(lom_curves, {
    lom_fit |>
      unnest(augment) |>
      select(station_id, model, mean_airtemp_c, .fitted, mean_temp_c) |>
      ggplot(aes(mean_airtemp_c, .fitted)) +
      geom_vline(xintercept = 0, alpha = 0.5) +
      geom_hline(yintercept = 0, alpha = 0.5) +
      geom_point(aes(y = mean_temp_c), size = 0.5, alpha = 0.05) +
      geom_line(aes(group = interaction(model, station_id), color = model), alpha = 0.15) +
      scale_color_brewer(palette = "Set1", guide = "none") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
      labs(x = "Weekly Mean Air Temp. (degC)", y = "Predicted Weekly Mean Water Temp. (degC)") +
      facet_wrap(vars(model)) +
      theme_bw() +
      theme(
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 12)
      )
  }),
  tar_target(lom_coef_map_model, {
    lom_fit |>
      unnest(coef) |>
      select(station_id, model, term, estimate) |>
      pivot_wider(names_from = "term", values_from = "estimate") |>
      filter(b < 40) |>
      pivot_longer(-c(station_id, model), names_to = "term", values_to = "estimate") |>
      left_join(select(obs_stn, station_id), by = "station_id") |>
      st_as_sf() |>
      nest_by(term) |>
      mutate(
        plot = list({
          data |>
            ggplot() +
            geom_sf(data = gis_state, fill = NA) +
            geom_sf(aes(color = estimate)) +
            scale_color_viridis_c() +
            facet_grid(vars(model), vars()) +
            labs(title = term) +
            theme_minimal()
        })
      ) |>
      pull(plot) |>
      wrap_plots(nrow = 2)
  }),
  tar_target(lom_coef_map, {
    lom_fit |>
      filter(model == "LOM3") |>
      unnest(coef) |>
      select(station_id, model, term, estimate) |>
      pivot_wider(names_from = "term", values_from = "estimate") |>
      filter(b < 40) |>
      pivot_longer(-c(station_id, model), names_to = "term", values_to = "estimate") |>
      left_join(select(obs_stn, station_id), by = "station_id") |>
      st_as_sf() |>
      nest_by(term, .keep = TRUE) |>
      mutate(
        plot = list({
          new.lab = as_labeller(c(model="model", a="param: alpha", b="param: beta", c="param: gamma", m="param: mu"), label_parsed)
          data |>
            ggplot() +
            geom_sf(data = gis_state, fill = NA) +
            geom_sf(aes(color = estimate)) +
            scale_color_viridis_c(new.lab(term)[[1]][[1]]) +
            # labs(title = term) +
            facet_wrap(vars(term), labeller = new.lab) +
            theme_bw() +
            theme(
              strip.background = element_blank(),
              strip.text = element_text(size = 12, face = "bold")
            )
        })
      ) |>
      pull(plot) |>
      wrap_plots(nrow = 2)
  }),
  tar_target(lom_coef_splot_area, {
    lom_fit |>
      filter(model == "LOM3") |>
      unnest(coef) |>
      select(station_id, model, total_area_km2, n_week, term, estimate) |>
      pivot_wider(names_from = "term", values_from = "estimate") |>
      filter(b < 40) |>
      pivot_longer(c(a, b, c), names_to = "term", values_to = "estimate") |>
      ggplot(aes(total_area_km2, estimate)) +
      geom_point(aes(color = n_week)) +
      scale_x_log10() +
      scale_color_viridis_c(trans = "log10") +
      facet_wrap(vars(term), scales = "free_y") +
      theme_minimal()
  })
  # tar_target(lom_coef_extreme, {
  #   lom_fit |>
  #     filter(model == "lom3") |>
  #     unnest(coef) |>
  #     select(station_id, model, data, augment, term, estimate) |>
  #     pivot_wider(names_from = "term", values_from = "estimate") |>
  #     arrange(desc(b)) |>
  #     head(4) |>
  #     rowwise() |>
  #     mutate(
  #       plot_ts = list({
  #         data |>
  #           ggplot(aes(date)) +
  #           geom_point(aes(y = mean_airtemp_c, color = "air")) +
  #           geom_point(aes(y = mean_temp_c, color = "water")) +
  #           # geom_line(aes(y = .fitted)) +
  #           scale_color_brewer(palette = "Set1") +
  #           labs(title = station_id, subtitle = glue("b = {round(b, digits = 2)}")) +
  #           theme_minimal()
  #       }),
  #       plot_splot = list({
  #         data |>
  #           ggplot(aes(mean_airtemp_c, mean_temp_c)) +
  #           geom_point(aes(shape = week < 30)) +
  #           geom_line(
  #             data = augment,
  #             aes(y = .fitted)
  #           ) +
  #           # scale_color_brewer(palette = "Set1", labels = c("TRUE" = "Rising", "FALSE" = "Falling")) +
  #           labs(title = station_id, subtitle = glue("b = {round(b, digits = 2)}")) +
  #           theme_minimal()
  #       }),
  #       plot = list({
  #         wrap_plots(list(plot_ts, plot_splot), ncol = 1)
  #       })
  #     ) |>
  #     pull(plot) |>
  #     wrap_plots(nrow = 1) +
  #     plot_layout(guides = "collect")
  # }),
  # tar_target(lom_coef_pca_splot, {
  #   lom_fit |>
  #     filter(model == "lom3") |>
  #     left_join(
  #       obs_nhdplusv2_flowline |>
  #         select(station_id, comid),
  #       by = "station_id"
  #     ) |>
  #     left_join(
  #       nhdplusv2_pca$data,
  #       by = c("comid" = "COMID")
  #     ) |>
  #     unnest(coef) |>
  #     select(-where(is.list)) |>
  #     select(station_id, term, estimate, PC1, PC2) |>
  #     pivot_wider(names_from = "term", values_from = "estimate") |>
  #     filter(b < 40) |>
  #     pivot_longer(c(a, b, c), names_to = "term", values_to = "estimate") |>
  #     nest_by(term) |>
  #     mutate(
  #       plot = list({
  #         data |>
  #           ggplot(aes(PC1, PC2)) +
  #           geom_point(aes(color = estimate)) +
  #           scale_color_viridis_c() +
  #           labs(title = term)
  #       })
  #     ) |>
  #     pull(plot) |>
  #     wrap_plots()
  # }),
  # tar_target(lom_coef_pca_eff, {
  #   lom_fit |>
  #     filter(model == "lom3") |>
  #     left_join(
  #       obs_nhdplusv2_flowline |>
  #         select(station_id, comid),
  #       by = "station_id"
  #     ) |>
  #     left_join(
  #       nhdplusv2_pca$data,
  #       by = c("comid" = "COMID")
  #     ) |>
  #     unnest(coef) |>
  #     select(-where(is.list)) |>
  #     select(station_id, term, estimate, PC1, PC2) |>
  #     pivot_wider(names_from = "term", values_from = "estimate") |>
  #     filter(b < 40) |>
  #     pivot_longer(c(a, b, c), names_to = "term", values_to = "estimate") |>
  #     pivot_longer(c(PC1, PC2), names_to = "PC.name", values_to = "PC.value") |>
  #     ggplot(aes(PC.value, estimate)) +
  #     geom_point() +
  #     facet_grid(vars(term), vars(PC.name), scales = "free_y")
  # }),
  # tar_target(lom_coef_lm, {
  #   lom_fit |>
  #     filter(model == "lom3") |>
  #     left_join(
  #       obs_nhdplusv2_flowline |>
  #         select(station_id, comid),
  #       by = "station_id"
  #     ) |>
  #     left_join(
  #       nhdplusv2_pca$data,
  #       by = c("comid" = "COMID")
  #     ) |>
  #     unnest(coef) |>
  #     select(-where(is.list)) |>
  #     select(station_id, term, estimate, PC1, PC2, PC3, PC4) |>
  #     pivot_wider(names_from = "term", values_from = "estimate") |>
  #     filter(b < 40) |>
  #     pivot_longer(c(a, b, c), names_to = "term", values_to = "estimate") |>
  #     nest_by(term) |>
  #     mutate(
  #       lm_fit = list(lm(estimate ~ PC1 + PC2, data = data)),
  #       lm_coef = list(broom::tidy(lm_fit)),
  #       lm_glance = list(broom::glance(lm_fit)),
  #       lm_augment = list(broom::augment(lm_fit)),
  #       plot = list({
  #         data |>
  #           ggplot(aes(PC1, PC2)) +
  #           geom_point(aes(color = estimate)) +
  #           scale_color_viridis_c() +
  #           labs(title = term) +
  #           theme_minimal()
  #       })
  #     )
  # }),
  # tar_target(lom_coef_lm_splot, {
  #   lom_coef_lm |>
  #     select(term, lm_augment) |>
  #     unnest(lm_augment) |>
  #     ungroup() |>
  #     nest_by(term) |>
  #     mutate(
  #       plot = list({
  #         data |>
  #           ggplot(aes(.fitted, estimate)) +
  #           geom_blank(
  #             data = data |>
  #               mutate(.fitted = estimate)
  #           ) +
  #           geom_abline() +
  #           geom_point() +
  #           geom_smooth(method = "lm", formula = y ~ x) +
  #           labs(title = term) +
  #           theme_minimal()
  #       })
  #     ) |>
  #     pull(plot) |>
  #     wrap_plots()
  # })
)