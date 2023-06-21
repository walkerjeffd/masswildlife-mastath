targets_temp <- list(
  tar_target(temp_grab, bind_rows(wqx)),
  tar_target(temp_inst, bind_rows(hoorwa, crwa, irwa, nwis_temp, pie_lter_temp, hrf_temp)),
  tar_target(temp_day, {
    temp_inst |>
      rowwise() |>
      mutate(
        data = list({
          x <- data
          if (!"strata" %in% names(x)) {
            x$strata <- NA_character_
          }
          x |>
            group_by(strata, date = as_date(datetime)) |>
            summarise(
              n_values = n(),
              min_temp_c = min(temp_c),
              mean_temp_c = mean(temp_c),
              max_temp_c = max(temp_c),
              .groups = "drop"
            )
        })
      ) |>
      bind_rows(ecosheds_day, neon_temp_day)
  }),
  tar_target(temp_plot_period, {
    temp_day |>
      rowwise() |>
      mutate(
        min_date = min(data$date),
        max_date = max(data$date),
        n_days = max_date - min_date
      ) |>
      arrange(min_date, n_days) |>
      mutate(station_id = fct_inorder(station_id)) |>
      ggplot(aes()) +
      geom_segment(aes(x = min_date, xend = max_date, y = station_id, yend = station_id)) +
      facet_grid(vars(provider), vars(), space = "free_y", scales = "free_y") +
      labs(x = "date")
  })
)