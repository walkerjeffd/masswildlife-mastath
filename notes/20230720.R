source("_targets.R")


x_pred <- tar_read(reg_pred_xgb) |>
  select(term, data) |>
  unnest(data) |>
  mutate(split = "pred")

x_fit_obs <- tar_read(xgb_lom) |>
  ungroup() |>
  select(split, station_id, comid, data) |>
  unnest(data) |>
  select(split, station_id, COMID=comid, term, .pred = value) |>
  left_join(
    tar_read(reg_inp) |>
      select(COMID, any_of(names(x_pred))),
    by = c("COMID")
  ) |>
  select(all_of(names(x_pred))) |>
  mutate(split = str_c(split, "_obs"))

x_fit_pred <- tar_read(xgb_lom) |>
  ungroup() |>
  select(split, station_id, comid, data) |>
  unnest(data) |>
  select(split, station_id, COMID=comid, term, .pred) |>
  left_join(
    tar_read(reg_inp) |>
      select(COMID, any_of(names(x_pred))),
    by = c("COMID")
  ) |>
  select(all_of(names(x_pred))) |>
  mutate(split = str_c(split, "_pred"))

x <- bind_rows(x_pred, x_fit_obs, x_fit_pred) |>
  transmute(
    split,
    term,
    TOT_BFI = pmax(TOT_BFI, 40),
    TOT_CNPY11_BUFF100,
    TOT_NLCD11_RIP50_FOREST,
    TOT_NLCD11_WATER,
    `log10(TOT_NID_STORAGE2013)` = log10(pmax(TOT_NID_STORAGE2013, 1)),
    TOT_BASIN_SLOPE,
    `log10(TOT_BASIN_AREA)` = log10(pmax(TOT_BASIN_AREA, 0.01)),
    `log10(TOT_BASIN_AREA) ` = `log10(TOT_BASIN_AREA)`,
    TOT_ELEV_MEAN,
    .pred
  ) |>
  pivot_longer(-c(split, term, .pred, `log10(TOT_BASIN_AREA)`))

p <- x |>
  filter(split == "pred") |>
  ggplot(aes(value, .pred)) +
  geom_point(aes(color = pmax(`log10(TOT_BASIN_AREA)`, 0.01)), size = 0.75, alpha = 0.5) +
  scale_color_viridis_c("Basin Area\nlog10(km2)") +
  facet_grid(vars(term), vars(name), scales = "free", switch = "both") +
  labs(
    x = NULL, y = NULL,
    subtitle = "a = alpha (max temp), b = beta (inflection point), c = gamma (slope at inflection)\nblue circle = TRAIN, red circle = TEST"
  ) +
  theme_minimal() +
  theme(
    aspect.ratio = 1,
    strip.placement = "outside",
    strip.text = element_text(face = "bold", size = 10)
  )

p_pred <- p +
  labs(
    title = "XGBoost Effect Plots"
  )
p_fit_obs <- p +
  geom_point(
    data = filter(x, split == "train_obs"),
    shape = 21, fill = NA, color = "deepskyblue"
  ) +
  geom_point(
    data = filter(x, split == "test_obs"),
    shape = 21, fill = NA, color = "orangered"
  ) +
  labs(
    title = "XGBoost Effect Plots w/ Observed Values from Train/Test"
  )
p_fit_pred <- p +
  geom_point(
    data = filter(x, split == "train_pred"),
    shape = 21, fill = NA, color = "deepskyblue"
  ) +
  geom_point(
    data = filter(x, split == "test_pred"),
    shape = 21, fill = NA, color = "orangered"
  ) +
  labs(
    title = "XGBoost Effect Plots w/ Predicted Values from Train/Test"
  )

p_pred
p_fit_obs
p_fit_pred

p_fit <- wrap_plots(
  p_fit_pred,
  p_fit_obs,
  ncol = 1
) +
  plot_layout(guides = "collect")
ggsave("notes/20230720/mastath-effects.png", plot = p_fit, width = 20, height = 17)
