targets_flow <- list(
  tar_target(flow_day, bind_rows(nwis_flow_day, pie_lter_flow_day, hrf_flow_day)),
  tar_target(flow_plot_period, {
    flow_day |>
      rowwise() |>
      mutate(
        min_date = min(data$date),
        max_date = max(data$date),
        n_days = max_date - min_date
      ) |>
      arrange(desc(min_date), n_days) |>
      mutate(station_id = fct_inorder(station_id)) |>
      ggplot(aes()) +
      geom_segment(aes(x = min_date, xend = max_date, y = station_id, yend = station_id)) +
      facet_grid(vars(provider), vars(), space = "free_y", scales = "free_y") +
      labs(x = "date") +
      theme_bw() +
      theme(axis.text.y = element_text(size = 6))
  })
)