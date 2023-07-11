# model input

targets_inp <- list(
  tar_target(inp_day, {
    x <- obs_day |>
      left_join(
        obs_nhdplusv2_flowline |>
          select(station_id, snap_distance_m, total_area_km2 = TOT_BASIN_AREA),
        by = "station_id"
      )

    # snap_distance_m <= 100 m
    x |>
      ggplot(aes(snap_distance_m)) +
      stat_ecdf() +
      coord_cartesian(xlim = c(0, 500))

    # total_area_km2 <= 10,000 km2
    x |>
      filter(total_area_km2 > 1e3) |>
      arrange(desc(total_area_km2))

    x |>
      filter(
        !station_id %in% obs_stn_dams$station_id,
        !station_id %in% obs_stn_waterbodies$station_id,
        snap_distance_m <= 100,
        total_area_km2 <= 1e4
      ) |>
      left_join(obs_daymet_stn, by = "station_id") |>
      rowwise() |>
      mutate(
        data = list({
          data |>
            left_join(daymet_stn, by = "date") |>
            transmute(
              date, n_values,
              min_temp_c, mean_temp_c, max_temp_c,
              min_airtemp_c = tmin,
              max_airtemp_c = tmax,
              mean_airtemp_c = (tmin + tmax) / 2,
              prcp_mm = prcp
            ) |>
            filter(
              !(mean_temp_c == 0 & yday(date) >= 100 & yday(date) <= 300)
            )
        })
      ) |>
      select(-daymet_stn)
  }),
  tar_target(inp_day_plot_seas, {
    inp_day |>
      select(station_id, data) |>
      unnest(data) |>
      ggplot(aes(ymd(20001231) + days(yday(date)), mean_temp_c)) +
      geom_point(size = 0.5, alpha = 0.05) +
      geom_smooth(se = FALSE) +
      scale_x_date(date_breaks = "2 months", date_labels = "%b", expand = expansion()) +
      scale_y_continuous(breaks = scales::pretty_breaks(6), expand = expansion(c(0, 0.05)), limits = c(0, NA)) +
      labs(
        x = "Day of Year",
        y = "Water Temp. (degC)"
      ) +
      theme_bw() +
      theme(plot.margin = margin(3, 5, 3, 3, unit = "mm"))
  }),
  tar_target(inp_week, {
    inp_day |>
      select(-n_day) |>
      mutate(
        data = list({
          data |>
            group_by(year = year(date), week = week(date)) |>
            summarise(
              median_n_values_day = median(n_values),
              n_values = sum(n_values),
              n_days = n(),
              mean_temp_c = mean(mean_temp_c),
              mean_airtemp_c = mean(mean_airtemp_c),
              .groups = "drop"
            ) |>
            mutate(
              date = ymd(str_c(year, "0101")) + days(7 * (week - 1)),
              .before = everything()
            ) |>
            filter(n_days == 7)
        }),
        n_week = nrow(data),
        n_week_summer = sum(data$week %in% 18:43),
        frac_week_summer = n_week_summer / n_week
      ) |>
      filter(n_week > 0)
  }),
  tar_target(inp_week_plot_range, {
    # minimum # weeks = 25
    # plot max(mean_temp_c) - min(mean_temp_c) vs n_week
    inp_week |>
      mutate(
        min_value = min(data$mean_temp_c),
        max_value = max(data$mean_temp_c),
        delta_value = max_value - min_value
      ) |>
      ggplot(aes(n_week, delta_value)) +
      geom_point() +
      geom_vline(xintercept = 25, linetype = "dashed", color = "orangered") +
      xlim(0, 200) +
      labs(x = "# Weeks of Data", y = "Range of Weekly Mean Water Temp. (degC)\n(Max - Min)") +
      theme_bw()
  }),
  tar_target(inp_week_plot_summer, {
    # min/max week to define summer = [18, 43]
    inp_week |>
      filter(n_week >= 25) |>
      unnest(data) |>
      ggplot(aes(factor(week), mean_temp_c)) +
      geom_ribbon(
        # data = tibble(),
        aes(xmin = factor(18), xmax = factor(43), ymin = -Inf, ymax = Inf),
        alpha = 0.5, fill = "black"
      ) +
      annotate(
        geom = "rect",
        xmin = factor(18), xmax = factor(43),
        ymin = -Inf, ymax = Inf,
        fill = "goldenrod",
        alpha = 0.2
      ) +
      geom_boxplot(alpha = 0.5, size = 0.5) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      labs(x = "Week", y = "Weekly Mean Water Temp. (degC)") +
      theme_bw()
  }),
  tar_target(inp_week_plot_summer_frac, {
    # min fraction of all weeks in summer = [0.4, 0.9]
    inp_week |>
      filter(n_week >= 25) |>
      arrange(frac_week_summer) |>
      ggplot(aes(n_week, frac_week_summer)) +
      annotate(
        geom = "rect",
        xmin = -Inf, xmax = Inf,
        ymin = 0.3, ymax = 0.8,
        fill = "goldenrod",
        alpha = 0.2
      ) +
      geom_point() +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 8), limits = c(0, 1), labels = scales::percent) +
      labs(x = "# Weeks of Data", y = "Fraction of Weekly Values\nDuring Summer (Weeks 18-43)") +
      theme_bw()
  }),
  tar_target(inp_week_plot_ts, {
    inp_week |>
      unnest(data) |>
      ggplot(aes(date, mean_temp_c)) +
      geom_point(size = 0.5, alpha = 0.5)
  }),
  tar_target(inp_week_plot_seas, {
    inp_week |>
      unnest(data) |>
      ggplot(aes(week, mean_temp_c)) +
      geom_jitter(height = 0, size = 0.5, alpha = 0.5)
  }),
  tar_target(inp_week_plot_splot, {
    inp_week |>
      filter(
        n_week >= 25,
        frac_week_summer >= 0.3,
        frac_week_summer <= 0.8
      ) |>
      left_join(
        obs_streamstats_basin |>
          st_drop_geometry() |>
          select(station_id, area_km2),
        by = "station_id"
      ) |>
      unnest(data) |>
      ggplot(aes(mean_airtemp_c, mean_temp_c)) +
      geom_hline(yintercept = 0, alpha = 0.5) +
      geom_vline(xintercept = 0, alpha = 0.5) +
      geom_point(aes(color = area_km2), size = 0.5, alpha = 0.2) +
      scale_color_viridis_c("Drainage Area\n(km2)", trans = "log10") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      labs(
        x = "Weekly Basin-wide Mean Air Temp. (degC)",
        y = "Weekly Mean Water Temp. (degC)"
      ) +
      theme_bw()
  }),
  tar_target(inp_week_map_exclude, {
    x <- obs_stn |>
      inner_join(
        select(inp_week, provider, station_id, n_week, frac_week_summer),
        by = "station_id"
      ) |>
      mutate(
        exclude = case_when(
          n_week < 25 ~ "FAIL: < 25 Weeks",
          frac_week_summer < 0.3 ~ "FAIL: < 30% Summer",
          frac_week_summer > 0.8 ~ "FAIL: > 80% Summer",
          TRUE ~ "PASS"
        ),
        exclude = fct_relevel(factor(exclude), "PASS", "FAIL: < 25 Weeks")
      )

    x |>
      ggplot() +
      geom_sf(data = gis_state, fill = NA) +
      geom_sf(aes(color = exclude), alpha = 0.5) +
      scale_color_brewer("QAQC Screening", palette = "Set1") +
      facet_wrap(vars(exclude)) +
      theme_bw() +
      theme(
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 12)
      )
  })
)