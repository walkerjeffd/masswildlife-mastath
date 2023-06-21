targets_obs <- list(
  tar_target(obs_day, {
    bind_rows(
      flow = flow_day,
      temp = temp_day,
      .id = "dataset"
    ) |>
      rowwise() |>
      mutate(
        data = list({
          data |>
            filter(year(date) >= 1994, year(date) <= 2022)
        })
      ) |>
      filter(nrow(data) > 0) |>
      arrange(source) |>
      mutate(
        source = fct_inorder(source)
      )
  })
)