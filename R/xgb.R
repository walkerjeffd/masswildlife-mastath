# regional XGBoost model
tar_option_set(packages = c("tidyverse", "janitor", "sf", "units", "glue", "patchwork", "tidymodels"))

targets_xgb <- list(
  tar_target(xgb_inp, {
    lom_fit |>
      filter(model == "LOM3") |>
      unnest(coef) |>
      select(term, station_id, value = estimate) |>
      left_join(
        obs_nhdplusv2_flowline |>
          select(station_id, comid),
        by = "station_id"
      ) |>
      left_join(
        nhdplusv2_attrs |>
          select(-starts_with(c("ACC_", "CAT_"))),
        by = c("comid" = "COMID")
      )
  }),
  tar_target(xgb_spec, {
    boost_tree(
      trees = tune(),
      tree_depth = tune(),
      min_n = tune(),
      loss_reduction = tune(),
      sample_size = tune(),
      mtry = tune(),
      # learn_rate = 0.05
      learn_rate = tune()
    ) %>%
      set_engine("xgboost") %>%
      set_mode("regression")
  }),
  tar_target(xgb_fit, {
    doParallel::registerDoParallel()
    xgb_inp |>
      nest_by(term) |>
      mutate(
        split = list({
          set.seed(1933)
          initial_split(data, prop = )
        }),
        train = list(training(split)),
        test = list(testing(split)),
        rec = list({
          recipe(value ~ ., data = train) |>
            update_role(comid, station_id, new_role = "ID")
        }),
        wf = list({
          workflow() |>
            add_model(xgb_spec) |>
            add_recipe(rec)
        }),
        folds = list({
          set.seed(1935)
          vfold_cv(train)
        }),
        grid = list({
          set.seed(1937)
          grid_latin_hypercube(
            trees(),
            tree_depth(),
            min_n(),
            loss_reduction(),
            sample_size = sample_prop(),
            finalize(mtry(), train),
            learn_rate(),
            # learn_rate(range = c(-4, -2)),
            size = 100
          )
        }),
        res = list({
          set.seed(1939)
          tune_grid(
            wf,
            resamples = folds,
            grid = grid,
            control = control_grid(save_pred = TRUE)
          )
        }),
        best_rmse = list(select_best(res, "rmse")),
        final_wf = list(finalize_workflow(
          wf,
          best_rmse
        )),
        final_fit = list(last_fit(final_wf, split)),
        final_pred = list(collect_predictions(final_fit))
      )
  }),
  tar_target(xgb_plot_metrics, {
    xgb_fit |>
      mutate(
        plot_metrics = list({
          res %>%
            collect_metrics() %>%
            # select(-std_err) |>
            # pivot_wider(names_from = ".metric", values_from = "mean") |>
            # filter(!is.na(rsq), !is.na(rmse)) |>
            # arrange(rmse) |>
            # head(20) |>
            # pivot_longer(c(rmse, rsq), names_to = ".metric", values_to = "mean") |>
            select(.metric, mean, mtry:sample_size) %>%
            pivot_longer(
              mtry:sample_size,
              values_to = "value",
              names_to = "parameter"
            ) %>%
            ggplot(aes(value, mean)) +
            geom_point(alpha = 0.8, show.legend = FALSE) +
            facet_grid(.metric ~ parameter, scales = "free") +
            labs(x = NULL, title = term) +
            theme_bw()
        })
      ) |>
      pull(plot_metrics) |>
      wrap_plots()
  }),
  tar_target(xgb_plot_vip, {
    xgb_fit |>
      mutate(
        plot_vip = list({
          final_fit %>%
            extract_fit_parsnip() |>
            vip::vip(geom = "col") +
            labs(title = labeller_lom(term)[[1]][[1]]) +
            theme_bw()
        })
      ) |>
      pull(plot_vip) |>
      wrap_plots()
  }),
  tar_target(xgb_final_pred, {
    xgb_fit |>
      mutate(
        final_pred = list({
          x <- as.matrix(select(train, -station_id, -comid, -value))
          pred <- predict(extract_fit_parsnip(final_fit), x)
          train_pred <- bind_cols(
            train,
            pred
          ) |>
            select(.pred, value)
          bind_rows(
            test = final_pred,
            train = train_pred,
            .id = "split"
          ) |>
            mutate(split = fct_rev(split))
        })
      )
  }),
  tar_target(xgb_final_gof, {
    xgb_final_pred |>
      select(term, data = final_pred) |>
      unnest(data) |>
      ungroup() |>
      nest_by(term, split) |>
      mutate(
        n = nrow(data),
        gof = list(gof_tibble(data$.pred, data$value))
      ) |>
      unnest(gof) |>
      unite(name, c("term", "split")) |>
      select(name, n, ME, MAE, RMSE, R2) |>
      pivot_longer(-name, names_to = "stat") |>
      pivot_wider()
  }),
  tar_target(xgb_final_gof_file, {
    fname <- "data/output/report/xgb-final-gof.csv"
    xgb_final_gof |>
      write_csv(fname)
    fname
  }, format = "file"),
  tar_target(xgb_plot_final_splot, {
    xgb_final_pred |>
      mutate(
        plot = list({
          final_pred |>
            ggplot(aes(.pred, value)) +
            geom_abline(linetype = "dashed") +
            geom_point(aes(color = split)) +
            geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
            geom_blank(aes(min(c(.pred, value)), max(c(.pred, value)))) +
            scale_color_brewer(palette = "Set1", guide = "none") +
            labs(title = labeller_lom(term)[[1]][[1]], x = "Predicted", y = "Observed") +
            facet_wrap(vars(split), ncol = 1, labeller = label_both) +
            theme_bw() +
            theme(
              aspect.ratio = 1,
              strip.background = element_blank(),
              strip.text = element_text(size = 12)
            )
        })
      ) |>
      pull(plot) |>
      wrap_plots()
  }),
  tar_target(xgb_lom, {
    xgb_fit |>
      mutate(
        data = list({
          bind_rows(
            train = train,
            test = test,
            .id = "split"
          )
        }),
        mat = list({
          as.matrix(select(data, -split, -station_id, -value, -comid))
        }),
        pred = list({
          x <- predict(extract_fit_parsnip(final_fit), mat)
          bind_cols(
            x,
            data
          ) |>
            select(split, station_id, comid, value, .pred) |>
            mutate(.resid = value - .pred)
        })
      ) |>
      select(term, pred) |>
      unnest(pred) |>
      ungroup() |>
      nest_by(split, station_id, comid) |>
      mutate(
        pred_param = list({
          data |>
            select(name = term, value) |>
            deframe()
        })
      ) |>
      left_join(
        inp_week |>
          select(station_id, data_week = data),
        by = "station_id"
      ) |>
      mutate(
        data_week = list({
          a <- pred_param[["a"]]
          b <- pred_param[["b"]]
          c <- pred_param[["c"]]
          data_week |>
            mutate(
              .pred = map_dbl(mean_airtemp_c, \(x) lom3(x, a, b, c))
            )
        }),
        gof = list(gof_tibble(data_week$.pred, data_week$mean_temp_c))
      )
  }),
  tar_target(xgb_lom_global_gof, {
    xgb_lom |>
      ungroup() |>
      unnest(data_week) |>
      select(split, station_id, .pred, value = mean_temp_c) |>
      nest_by(split) |>
      mutate(
        n_stations = length(unique(data$station_id)),
        n_values = nrow(data),
        gof = list(gof_tibble(data$.pred, data$value))) |>
      unnest(gof) |>
      select(split, n_stations, n_values, ME, MAE, R2, RMSE, NSE, KGE) |>
      ungroup() |>
      pivot_longer(-split) |>
      pivot_wider(names_from = "split") |>
      select(name, train, test)
  }),
  tar_target(xgb_lom_global_gof_file, {
    fname <- "data/output/report/xgb-global-gof.csv"
    xgb_lom_global_gof |>
      write_csv(fname)
    fname
  }, format = "file"),
  tar_target(xgb_lom_plot_gof, {
    xgb_lom |>
      ungroup() |>
      unnest(gof) |>
      select(split, station_id, R2, RMSE, NSE, KGE) |>
      pivot_longer(c(R2, RMSE, NSE, KGE)) |>
      mutate(
        name = factor(name, levels = c("RMSE", "R2", "NSE", "KGE"))
      ) |>
      ggplot(aes(value, split)) +
      geom_violin(aes(fill = split), draw_quantiles = c(0.5)) +
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
  tar_target(xgb_lom_plot_splot, {
    xgb_lom |>
      ungroup() |>
      unnest(data_week) |>
      ggplot(aes(.pred, mean_temp_c)) +
      geom_abline(linetype = "dashed") +
      geom_point(aes(color = split), alpha = 0.5, size = 0.5) +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
      geom_blank(aes(min(c(.pred, mean_temp_c)), max(c(.pred, mean_temp_c)))) +
      scale_color_brewer(palette = "Set1", guide = "none") +
      labs(x = "Predicted Weekly Water Temp. (degC)", y = "Observed Weekly Water Temp. (degC)") +
      facet_wrap(vars(split), ncol = 2, labeller = label_both) +
      theme_bw() +
      theme(
        aspect.ratio = 1,
        strip.background = element_blank(),
        strip.text = element_text(size = 12)
      )
  })
)