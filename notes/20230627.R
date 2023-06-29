source("_targets.R")

# fit lom slope (c) -------------------------------------------------------
tar_load(lom_fit)
lom_coef <- lom_fit |>
  filter(model == "lom3", station_id != "NRWA:LPB48 [11226]") |>
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

lom_coef_a <- lom_coef |>
  filter(term == "a") |>
  select(-term)
lom_coef_b <- lom_coef |>
  filter(term == "b") |>
  select(-term)
lom_coef_c <- lom_coef |>
  filter(term == "c") |>
  select(-term)

library(tidymodels)
show_engines("rand_forest")

set.seed(192)
rand_forest(mtry = 10, trees = 2000) %>%
  set_engine("ranger") %>%
  set_mode("regression") %>%
  fit(value ~ ., data = select(lom_coef_a, -comid, -station_id))
rand_forest(mtry = 10, trees = 2000) %>%
  set_engine("ranger") %>%
  set_mode("regression") %>%
  fit(value ~ ., data = select(lom_coef_b, -comid, -station_id))
rand_forest(mtry = 10, trees = 2000) %>%
  set_engine("ranger") %>%
  set_mode("regression") %>%
  fit(value ~ ., data = select(lom_coef_c, -comid, -station_id))

show_engines("boost_tree")

boost_tree(mtry = 10, trees = 2000) %>%
  set_engine("xgboost") %>%
  set_mode("regression") %>%
  fit(value ~ ., data = select(lom_coef_a, -comid, -station_id))
boost_tree(mtry = 10, trees = 2000) %>%
  set_engine("xgboost") %>%
  set_mode("regression") %>%
  fit(value ~ ., data = select(lom_coef_b, -comid, -station_id))
boost_tree(mtry = 10, trees = 2000) %>%
  set_engine("xgboost") %>%
  set_mode("regression") %>%
  fit(value ~ ., data = select(lom_coef_c, -comid, -station_id))

# https://juliasilge.com/blog/xgboost-tune-volleyball/
lom_coef_c_split <- initial_split(select(lom_coef_c, -station_id, -comid))
lom_coef_c_train <- training(lom_coef_c_split)
lom_coef_c_test <- testing(lom_coef_c_split)

xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune(),
  learn_rate = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")
xgb_spec

xgb_grid_c <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), lom_coef_c_train),
  learn_rate(),
  size = 30
)
xgb_grid_c

xgb_wf <- workflow() %>%
  add_formula(value ~ .) %>%
  add_model(xgb_spec)
xgb_wf

set.seed(123)
lom_coef_c_folds <- vfold_cv(lom_coef_c_train)
lom_coef_c_folds

doParallel::registerDoParallel()
set.seed(234)
xgb_res <- tune_grid(
  xgb_wf,
  resamples = lom_coef_c_folds,
  grid = xgb_grid_c,
  control = control_grid(save_pred = TRUE)
)
xgb_res

collect_metrics(xgb_res)
xgb_res %>%
  collect_metrics() %>%
  select(.metric, mean, mtry:sample_size) %>%
  pivot_longer(
    mtry:sample_size,
    values_to = "value",
    names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_grid(.metric ~ parameter, scales = "free") +
  labs(x = NULL, y = "AUC") +
  theme_bw()

show_best(xgb_res, "rmse")
show_best(xgb_res, "rsq")

best_rmse <- select_best(xgb_res, "rmse")
best_rmse

final_xgb <- finalize_workflow(
  xgb_wf,
  best_rmse
)

final_xgb
library(vip)

final_xgb %>%
  fit(data = lom_coef_c_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

final_res <- last_fit(final_xgb, lom_coef_c_split)
collect_metrics(final_res)

final_res %>%
  collect_predictions() |>
  ggplot(aes(.pred, value)) +
  geom_abline() +
  geom_point() +
  geom_blank(aes(min(c(.pred, value)), max(c(.pred, value)))) +
  theme(aspect.ratio = 1)

# now with a
lom_coef_a_split <- initial_split(select(lom_coef_a, -station_id, -comid))
lom_coef_a_train <- training(lom_coef_a_split)
lom_coef_a_test <- testing(lom_coef_a_split)

xgb_grid_a <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), lom_coef_a_train),
  learn_rate(),
  size = 30
)
xgb_grid_a

set.seed(123)
lom_coef_a_folds <- vfold_cv(lom_coef_a_train)
lom_coef_a_folds

doParallel::registerDoParallel()
set.seed(234)
xgb_res_a <- tune_grid(
  xgb_wf,
  resamples = lom_coef_a_folds,
  grid = xgb_grid_a,
  control = control_grid(save_pred = TRUE)
)
xgb_res_a

collect_metrics(xgb_res_a)
xgb_res_a %>%
  collect_metrics() %>%
  select(.metric, mean, mtry:sample_size) %>%
  pivot_longer(
    mtry:sample_size,
    values_to = "value",
    names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_grid(.metric ~ parameter, scales = "free") +
  labs(x = NULL, y = "AUC") +
  theme_bw()

show_best(xgb_res_a, "rmse")
show_best(xgb_res_a, "rsq")

best_rmse_a <- select_best(xgb_res_a, "rmse")
best_rmse_a

final_xgb_a <- finalize_workflow(
  xgb_wf,
  best_rmse_a
)

final_xgb_a

final_xgb_a %>%
  fit(data = lom_coef_a_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

final_res_a <- last_fit(final_xgb_a, lom_coef_a_split)
collect_metrics(final_res_a)

final_res_a %>%
  extract_fit_parsnip() %>%
  vip()

final_res_a %>%
  collect_predictions() |>
  ggplot(aes(.pred, value)) +
  geom_abline() +
  geom_point() +
  geom_blank(aes(min(c(.pred, value)), max(c(.pred, value)))) +
  theme(aspect.ratio = 1)

# now with b
lom_coef_b_split <- initial_split(select(lom_coef_b, -station_id, -comid))
lom_coef_b_train <- training(lom_coef_b_split)
lom_coef_b_test <- testing(lom_coef_b_split)

xgb_grid_b <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), lom_coef_b_train),
  learn_rate(),
  size = 30
)
xgb_grid_b

set.seed(123)
lom_coef_b_folds <- vfold_cv(lom_coef_b_train)
lom_coef_b_folds

doParallel::registerDoParallel()
set.seed(234)
xgb_res_b <- tune_grid(
  xgb_wf,
  resamples = lom_coef_b_folds,
  grid = xgb_grid_b,
  control = control_grid(save_pred = TRUE)
)
xgb_res_b

collect_metrics(xgb_res_b)
xgb_res_b %>%
  collect_metrics() %>%
  select(.metric, mean, mtry:sample_size) %>%
  pivot_longer(
    mtry:sample_size,
    values_to = "value",
    names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_grid(.metric ~ parameter, scales = "free") +
  labs(x = NULL) +
  theme_bw()

show_best(xgb_res_b, "rmse")
show_best(xgb_res_b, "rsq")

best_rmse_b <- select_best(xgb_res_b, "rmse")
best_rmse_b

final_xgb_b <- finalize_workflow(
  xgb_wf,
  best_rmse_b
)

final_xgb_b

final_xgb_b %>%
  fit(data = lom_coef_b_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

final_res_b <- last_fit(final_xgb_b, lom_coef_b_split)
collect_metrics(final_res_b)

final_res_b %>%
  extract_fit_parsnip() %>%
  vip()

library(xgboost)
z <- final_res_b |>
  extract_fit_engine()
xgb.plot.tree(model = z, trees = 990:995)

final_res_b %>%
  collect_predictions() |>
  ggplot(aes(.pred, value)) +
  geom_abline() +
  geom_point() +
  geom_blank(aes(min(c(.pred, value)), max(c(.pred, value)))) +
  theme(aspect.ratio = 1)


# update model for rf ------------------------------------------------------------

rf_spec <- rand_forest(
  trees = 1000,
  min_n = tune(),
  mtry = tune(),
) |>
  set_mode("regression") |>
  set_engine("ranger", importance = "impurity")

rf_wf <- xgb_wf |>
  update_model(rf_spec)

rf_grid_b <- grid_latin_hypercube(
  min_n(),
  finalize(mtry(), lom_coef_b_train),
  size = 30
)
rf_grid_b

set.seed(234)
rf_res_b <- tune_grid(
  rf_wf,
  resamples = lom_coef_b_folds,
  grid = rf_grid_b,
  control = control_grid(save_pred = TRUE)
)
rf_res_b
collect_metrics(rf_res_b)

rf_res_b %>%
  collect_metrics() %>%
  select(.metric, mean, mtry:min_n) %>%
  pivot_longer(
    mtry:min_n,
    values_to = "value",
    names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_grid(.metric ~ parameter, scales = "free") +
  labs(x = NULL) +
  theme_bw()

show_best(rf_res_b, "rmse")
show_best(rf_res_b, "rsq")

best_rmse_b <- select_best(rf_res_b, "rmse")
best_rmse_b

final_rf_b <- finalize_workflow(
  rf_wf,
  best_rmse_b
)
final_rf_b

final_rf_b %>%
  fit(data = lom_coef_b_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

final_res_b <- last_fit(final_rf_b, lom_coef_b_split)
collect_metrics(final_res_b)

final_res_b %>%
  extract_fit_parsnip() %>%
  vip()


rf_grid_a <- grid_latin_hypercube(
  min_n(),
  finalize(mtry(), lom_coef_a_train),
  size = 30
)
rf_grid_a

set.seed(234)
rf_res_a <- tune_grid(
  rf_wf,
  resamples = lom_coef_a_folds,
  grid = rf_grid_a,
  control = control_grid(save_pred = TRUE)
)
rf_res_a
collect_metrics(rf_res_a)

rf_res_a %>%
  collect_metrics() %>%
  select(.metric, mean, mtry:min_n) %>%
  pivot_longer(
    mtry:min_n,
    values_to = "value",
    names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean)) +
  geom_point(alpha = 0.8, show.legend = FALSE) +
  facet_grid(.metric ~ parameter, scales = "free") +
  labs(x = NULL) +
  theme_bw()

show_best(rf_res_a, "rmse")
show_best(rf_res_a, "rsq")

best_rmse_a <- select_best(rf_res_a, "rmse")
best_rmse_a

final_rf_a <- finalize_workflow(
  rf_wf,
  best_rmse_a
)
final_rf_a

final_rf_a %>%
  fit(data = lom_coef_a_train) %>%
  pull_workflow_fit() %>%
  vip(geom = "point")

final_res_a <- last_fit(final_rf_a, lom_coef_a_split)
collect_metrics(final_res_a)

final_res_a %>%
  extract_fit_parsnip() %>%
  vip()

pred_a_inp <- st_drop_geometry(nhdplusv2_flowline) |>
  select(COMID, any_of(names(lom_coef_a))) |>
  filter(!is.na(TOT_ELEV_MEAN), !is.na(TOT_NLCD11_AGRI_CROP))

pred_a <- final_res_a |>
  extract_fit_engine() |>
  predict(newdata = as.matrix(select(pred_a_inp, -COMID)))
bind_cols(
  pred_a_inp,
  pred_a = pred_a
) |>
  ggplot(aes(pred_a)) +
  geom_histogram()

lom_coef_a |>
  ggplot(aes(value)) +
  geom_histogram()

nhdplusv2_flowline |>
  left_join(
    bind_cols(
      pred_a_inp,
      pred_a = pred_a
    ),
    by = "COMID"
  ) |>
  mapview(zcol = "pred_a")
  # ggplot(aes(pred_a)) +
  # geom_sf(aes(color = pred_a)) +
  # scale_color_viridis_c()


# fit xgb to each parameter -----------------------------------------------

xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune(),
  learn_rate = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")
xgb_spec

xgb_wf <- workflow() %>%
  add_formula(value ~ .) %>%
  add_model(xgb_spec)
xgb_wf

doParallel::registerDoParallel()
lom_coef_xgb_fit <- lom_coef |>
  mutate(FRAC_NID_STORAGE2013 = TOT_NID_STORAGE2013 / TOT_BASIN_AREA) |>
  nest_by(term) |>
  mutate(
    split = list(initial_split(select(data, -station_id, -comid))),
    train = list(training(lom_coef_c_split)),
    test = list(testing(lom_coef_c_split)),
    folds = list({
      set.seed(123)
      vfold_cv(train)
    }),
    grid = list({
      grid_latin_hypercube(
        tree_depth(),
        min_n(),
        loss_reduction(),
        sample_size = sample_prop(),
        finalize(mtry(), train),
        learn_rate(),
        size = 30
      )
    }),
    res = list({
      set.seed(234)
      tune_grid(
        xgb_wf,
        resamples = folds,
        grid = grid,
        control = control_grid(save_pred = TRUE)
      )
    }),
    metrics_plot = list({
      res %>%
        collect_metrics() %>%
        select(.metric, mean, mtry:sample_size) %>%
        pivot_longer(
          mtry:sample_size,
          values_to = "value",
          names_to = "parameter"
        ) %>%
        ggplot(aes(value, mean)) +
        geom_point(alpha = 0.8, show.legend = FALSE) +
        facet_grid(.metric ~ parameter, scales = "free") +
        labs(x = NULL) +
        theme_bw()
    }),
    best_rmse = list(select_best(res, "rmse")),
    final_wf = list(finalize_workflow(
      xgb_wf,
      best_rmse
    )),
    final_fit = list(last_fit(final_wf, split)),
    final_pred = list(collect_predictions(final_fit))
  )

lom_coef_xgb_fit |>
  mutate(
    vip = list({
      final_fit %>%
        extract_fit_parsnip() |>
        vip(geom = "point") +
        labs(title = term)
    }),
    final_splot = list({
      final_pred |>
        ggplot(aes(.pred, value)) +
        geom_abline() +
        geom_point() +
        geom_blank(aes(min(c(.pred, value)), max(c(.pred, value)))) +
        labs(title = term, x = "sim", y = "obs") +
        theme(aspect.ratio = 1)
    })
  ) |>
  pull(vip) |>
  wrap_plots()

# predictions for each parameter across all flowlines
nhdplusv2_flowline_pred <- st_drop_geometry(nhdplusv2_flowline) |>
  select(COMID, any_of(names(lom_coef_a))) |>
  filter(!is.na(TOT_ELEV_MEAN), !is.na(TOT_NLCD11_AGRI_CROP)) |>
  mutate(FRAC_NID_STORAGE2013 = TOT_NID_STORAGE2013 / TOT_BASIN_AREA) |>
  select(COMID, any_of(names(lom_coef_xgb_fit$data[[1]])))

lom_coef_xgb_pred <- lom_coef_xgb_fit |>
  select(term, final_fit) |>
  mutate(pred = list({
    x <- final_fit |>
      extract_fit_engine() |>
      predict(newdata = as.matrix(select(nhdplusv2_flowline_pred, -COMID)))
    tibble(
      COMID = nhdplusv2_flowline_pred$COMID,
      value = x
    )
  })) |>
  select(-final_fit) |>
  unnest(pred) |>
  pivot_wider(names_from = "term") |>
  rowwise() |>
  mutate(
    curve = list({
      tibble(
        mean_airtemp_c = seq(-10, 30, by = 1),
        mean_temp_c = map_dbl(mean_airtemp_c, \(x) lom(x, a, b, c))
      )
    })
  )

lom_coef_xgb_pred |>
  unnest(curve) |>
  ggplot(aes(mean_airtemp_c, mean_temp_c)) +
  geom_line(aes(group = COMID), alpha = 0.05) +
  geom_point(
    data = lom_fit |>
      select(data) |>
      unnest(data),
    size = 0.5, alpha = 0.05, color = "deepskyblue"
  )

nhdplusv2_flowline |>
  left_join(
    lom_coef_xgb_pred |>
      unnest(curve) |>
      filter(mean_airtemp_c %in% c(20, 25)) |>
      select(COMID, mean_airtemp_c, mean_temp_c) |>
      pivot_wider(names_from = "mean_airtemp_c", values_from = "mean_temp_c") |>
      transmute(
        COMID,
        sensitivity = (`25` - `20`) / 5
      ),
    # lom_coef_xgb_pred |>
    #   unnest(curve) |>
    #   filter(mean_airtemp_c %in% c(20)) |>
    #   select(COMID, mean_airtemp_c, mean_temp_c),
    by = "COMID"
  ) |>
  mutate(
    sensitivity = if_else(TOT_BASIN_AREA < 1005, sensitivity, NA_real_)
  ) |>
  ggplot() +
  geom_sf(data = gis_state, fill = NA) +
  geom_sf(aes(color = sensitivity)) +
  scale_color_viridis_c() +
  theme_void()


# relate weekly and daily temps -------------------------------------------

tar_load(c(inp_day, inp_week))

inp_seas <- inp_day |>
  semi_join(inp_week, by = "station_id") |>
  select(station_id, data) |>
  unnest(data) |>
  select(station_id, date, mean_temp_c) |>
  filter(month(date) %in% c(6:8)) |>
  mutate(year = year(date)) |>
  nest_by(station_id, year) |>
  filter(nrow(data) >= 85) |>
  mutate(
    mean_jja = mean(data$mean_temp_c),
    mean_jul = mean(filter(data, month(date) == 7)$mean_temp_c),
    max = max(data$mean_temp_c)
  ) |>
  ungroup()
inp_seas |>
  select(mean_jja, mean_jul, max) |>
  GGally::ggpairs()

inp_seas |>
  ggplot(aes(year, mean_jul)) +
  geom_line(aes(group = station_id)) +
  geom_point() +
  geom_ribbon(aes(xmin = -Inf, xmax = Inf, ymin = 18.45, ymax = 22.3), alpha = 0.5)

inp_seas |>
  arrange(desc(mean_jul)) |>
  head(6) |>
  unnest(data) |>
  ggplot(aes(yday(date), mean_temp_c)) +
  geom_line(aes(color = station_id, group = interaction(station_id, year)))

inp_seas |>
  mutate(
    class = case_when(
      mean_jul < 18.45 ~ "cold",
      mean_jul <= 22.3 ~ "cool",
      TRUE ~ "warm"
    ),
    class = factor(class, levels = c("cold", "cool", "warm"))
  ) |>
  nest_by(station_id) |>
  mutate(
    n_year = nrow(data),
    class = list({
      count(data, class) |>
        pivot_wider(names_from = "class", names_prefix = "n_", values_from = "n")
    })
  ) |>
  unnest(class) |>
  mutate(across(starts_with("n_"), \(x) coalesce(x, 0))) |>
  mutate(
    frac_cold = n_cold / n_year,
    frac_cool = n_cool / n_year
  ) |>
  arrange(desc(frac_cold), desc(n_year)) |>
  head(12) |>
  unnest(data) |>
  ggplot(aes(year, mean_jul)) +
  geom_point() +
  geom_line(aes(color = station_id)) +
  geom_hline(yintercept = 18.45)

  arrange(desc(mean_jul)) |>
  head(6) |>
  unnest(data) |>
  ggplot(aes(yday(date), mean_temp_c)) +
  geom_line(aes(color = station_id, group = interaction(station_id, year)))


inp_day |>
  filter(str_detect(station_id, "11226")) |>
  unnest(data) |>
  arrange(date) |>
  ggplot(aes(date, mean_temp_c)) +
  geom_line(aes(color = "water")) +
  geom_line(aes(y = mean_airtemp_c, color = "air"))

obs_stn |>
  filter(str_detect(station_id, "11226")) |>
  mapview()