source("_targets.R")


# monthly vs weekly air temp ----------------------------------------------

tar_load(obs_nhdplusv2_flowline)

# n days per month by week for weighting
df_wk_mon <- tibble(
  date = seq.Date(ymd(20000101), ymd(20221231), by = 1),
  year = year(date),
  week = week(date),
  month = month(date)
) |>
  count(year, week, month)

set.seed(1210)
tair_daymet <- tar_read(obs_daymet_basin) |>
  sample_frac(size = 0.25) |>
  rowwise() |>
  rename(data = daymet_basin) |>
  mutate(
    data = list({
      data |>
        filter(year(date) >= 2000, month(date) %in% 5:9)
    })
  )
tair_day <- tair_daymet |>
  transmute(
    station_id,
    data_day = list({
      data |>
        transmute(
          date,
          year = year(date),
          month = month(date),
          week = week(date),
          tmean = (tmin + tmax) / 2
        )
        # group_by(year, week) |>
        # mutate(
        #   week_start_date = min(date),
        #   week_start_month = month(week_start_date),
        #   week_end_date = max(date),
        #   week_end_month = month(week_end_date)
        # ) |>
        # ungroup()
    })
  )

tair <- tair_day |>
  mutate(
    data_wk = list({
      data_day |>
        group_by(year, week) |>
        summarise(
          tmean_wk = mean(tmean),
          .groups = "drop"
        ) |>
        left_join(df_wk_mon, by = c("year", "week")) |>
        group_by(year, month) |>
        mutate(
          tmean_wk_mon = sum(tmean_wk * n) / sum(n)
        )
    }),
    data_mon = list({
      x_wk <- data_wk |>
        group_by(year, month) |>
        summarise(
          tmean_wk_mon = sum(tmean_wk * n) / sum(n),
          .groups = "drop"
        )
      x_day <- data_day |>
        group_by(year = year(date), month = month(date)) |>
        summarise(
          tmean_day_mon = mean(tmean),
          .groups = "drop"
        )

      x_wk |>
        left_join(x_day, by = c("year", "month"))
    }),
    data_wk_mon = list({
      data_wk |>
        # select(year, month, week, tmean_wk, n) |>
        left_join(
          select(data_mon, year, month, tmean_day_mon),
          by = c("year", "month")
        )
    })
  )

tair$data_wk_mon[[1]] |>
  ggplot(aes(tmean_wk_mon, tmean_day_mon)) +
  geom_abline() +
  geom_point()

tair$data_wk_mon[[1]] |>
  ggplot(aes(tmean_wk_mon, tmean_wk)) +
  geom_abline() +
  geom_point()

# mean(weekly mean weighted by n(weekly days | month) | month) = mean(daily mean | month)
tair |>
  select(station_id, data_mon) |>
  unnest(data_mon) |>
  ggplot(aes(tmean_wk_mon, tmean_day_mon)) +
  geom_abline() +
  geom_point(size = 0.5, alpha = 0.05)

tair |>
  select(station_id, data_wk_mon) |>
  unnest(data_wk_mon) |>
  filter(month == 7) |>
  ggplot(aes(tmean_wk_mon, tmean_wk)) +
  geom_abline() +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_point(
    data = ~ filter(., station_id == tair$station_id[[1]], year == 2020),
    color = "red"
  ) +
  # scale_color_viridis_c(trans = "log10") +
  facet_wrap(vars(month)) +
  coord_equal() +
  theme_bw()


tair |>
  left_join(
    obs_nhdplusv2_flowline |>
      select(station_id, TOT_BASIN_AREA, TOT_ELEV_MEAN),
    by = "station_id"
  ) |>
  select(station_id, data_wk_mon, TOT_BASIN_AREA, TOT_ELEV_MEAN) |>
  unnest(data_wk_mon) |>
  filter(month %in% 6:9) |>
  ggplot(aes(tmean_wk_mon, tmean_wk)) +
  geom_point(aes(color = TOT_ELEV_MEAN), size = 0.5, alpha = 0.2) +
  scale_color_viridis_c(trans = "log10") +
  facet_wrap(vars(month)) +
  theme_bw()


# quantile regression -----------------------------------------------------

tair |>
  head(1) |>
  select(station_id, data_wk_mon) |>
  unnest(data_wk_mon) |>
  filter(month == 8) |>
  group_by(station_id,year, month) |>
  summarise(
    across(starts_with("tmean"), mean),
    .groups = "drop"
  ) |>
  ggplot(aes(tmean_day_mon, tmean_wk)) +
  geom_abline() +
  geom_point()

tair |>
  head(1) |>
  select(station_id, data_wk_mon) |>
  unnest(data_wk_mon) |>
  filter(month == 8) |>
  ggplot(aes(tmean_day_mon, tmean_wk)) +
  geom_abline() +
  geom_point()

rq_inp |>
  head(1) |>
  filter(month %in% 6:8) |>
  unnest(data) |>
  ggplot(aes(tmean_day_mon, tmean_wk)) +
  geom_point(aes(color = factor(year)), size = 0.5, alpha = 0.2) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(vars(month)) +
  theme_bw()

rq_inp <- tair |>
  unnest(data_wk_mon) |>
  select(station_id, year, month, tmean_wk, n, tmean_day_mon) |>
  left_join(
    obs_nhdplusv2_flowline |>
      select(station_id, TOT_BASIN_AREA, TOT_ELEV_MEAN),
    by = "station_id"
  ) |>
  mutate(TOT_BASIN_AREA = log10(TOT_BASIN_AREA)) |>
  na.omit() |>
  nest_by(month)
rq_inp |>
  filter(month %in% 6:8) |>
  unnest(data) |>
  ggplot(aes(tmean_day_mon, tmean_wk)) +
  geom_point(aes(color = factor(year)), size = 0.5, alpha = 0.2) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(vars(month)) +
  theme_bw()

rq_inp_7 <- rq_inp |>
  filter(month == 7) |>
  unnest(data)
rq_fit_7 <- quantreg::rq(
  tmean_wk ~ tmean_day_mon,
  data = rq_inp_7,
  weights = rq_inp_7$n,
  tau = c(0.1, 0.5, 0.9)
)

# x_npqr_7 <- quantreg::lprq(
#   rq_inp_7$tmean_day_mon,
#   rq_inp_7$tmean_wk,
#   h = 1,
#   tau = c(0.1, 0.5, 0.9)
# )
# plot(x_npqr_7$xx, x_npqr_7$fv)

rq_fit <- rq_inp |>
  mutate(
    rq_fit = list({
      quantreg::rq(
        tmean_wk ~ tmean_day_mon,
        data = data,
        weights = data$n,
        tau = c(0.5, 0.75, 0.9, 0.95)
      )
    }),
    rq_augment = list(broom::augment(rq_fit))
  )

rq_fit |>
  filter(month %in% c(6:8)) |>
  mutate(
    plot = list({
      data |>
        ggplot(aes(tmean_day_mon, tmean_wk)) +
        geom_abline(alpha = 0.5, linetype = "dashed") +
        geom_point(size = 0.5, alpha = 0.2) +
        # geom_point(
        #   data = rq_augment,
        #   aes(color = fct_rev(.tau), y = .fitted),
        #   size = 0.5, alpha = 0.5
        # ) +
        geom_line(
          data = rq_augment,
          aes(color = fct_rev(.tau), y = .fitted),
          linewidth = 1
        ) +
        geom_blank(
          aes(x = min(tmean_day_mon, tmean_wk), y = min(tmean_day_mon, tmean_wk))
        ) +
        geom_blank(
          aes(x = max(tmean_day_mon, tmean_wk), y = max(tmean_day_mon, tmean_wk))
        ) +
        scale_color_brewer("Quantile", palette = "RdYlBu") +
        labs(
          x = "Monthly Mean Air Temp (degC)",
          y = "Weekly Mean Air Temp (degC)",
          title = month.abb[month]
        ) +
        theme_bw() +
        theme(aspect.ratio = 1)
    })
  ) |>
  pull(plot) |>
  wrap_plots() +
  plot_layout(guides = "collect")