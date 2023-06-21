tar_option_set(packages = c("tidyverse", "janitor", "sf", "units", "glue", "patchwork"))

targets_memo_data <- list(
  tar_target(memo_data_tbl_obs_tally_file, {
    filename <- "memo/data/obs-tally.csv"
    obs_day |>
      tabyl(source, dataset) |>
      write_csv(filename)
    filename
  }, format = "file"),
  tar_target(memo_data_fig_map_stn, {
    x <- obs_day |>
      rowwise() |>
      mutate(
        n_year = length(unique(year(data$date)))
      ) |>
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
    ggplot() +
      geom_sf(data = gis_states, fill = NA) +
      geom_sf(
        data = filter(x, source == "EcoSHEDS"),
        aes(size = n_year, fill = source), shape = 21, alpha = 0.35
      ) +
      geom_sf(
        data = filter(x, source == "USGS-NWIS"),
        aes(size = n_year, fill = source), shape = 21, alpha = 0.35
      ) +
      geom_sf(
        data = filter(x, !source %in% c("EcoSHEDS", "USGS-NWIS")),
        aes(size = n_year, fill = source), shape = 21, alpha = 0.5
      ) +
      scale_fill_discrete(drop = FALSE) +
      labs(fill = "Data Source", size = "# Years\nof Data") +
      facet_wrap(vars(dataset), ncol = 1, labeller = labeller(dataset = c(
        "flow" = "Streamflow Stations",
        "temp" = "Temperature Stations"
      ))) +
      guides(
        fill = guide_legend(override.aes = list(size = 5))
      ) +
      theme_bw()
  }),
  tar_target(memo_data_fig_map_stn_file, {
    filename <- file.path("memo", "data", "fig-map-stn.png")
    ggsave(filename, plot = memo_data_fig_map_stn, width = 8, height = 8)
    filename
  }, format = "file"),
  tar_target(memo_data_obs_tally_year, {
    obs_day |>
      filter(nrow(data) > 0) |>
      rowwise() |>
      mutate(
        data = list(
          data |>
            mutate(year = year(date)) |>
            count(year)
        ),
        start = min(data$year),
        end = max(data$year),
        n_years = length(unique(data$year))
      ) |>
      arrange(desc(start), n_years) |>
      mutate(station_id = fct_inorder(station_id))
  }),
  tar_target(memo_data_fig_cdf_years, {
    memo_data_obs_tally_year |>
      ungroup() |>
      ggplot(aes(n_years)) +
      stat_ecdf(pad = TRUE, direction = "hv", aes(color = dataset)) +
      scale_color_brewer("Station Type", palette = "Set1", labels = c("flow" = "Streamflow", "temp" = "Temperature")) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 12)) +
      scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.05)), breaks = scales::pretty_breaks(n = 12)) +
      labs(x = "# Years of Data", y = "Cumulative Frequency") +
      theme_bw()
  }),
  tar_target(memo_data_fig_cdf_years_file, {
    filename <- file.path("memo", "data", "fig-cdf-years.png")
    ggsave(filename, plot = memo_data_fig_cdf_years, width = 6, height = 4)
    filename
  }, format = "file"),
  tar_target(memo_data_fig_period, {
    memo_data_obs_tally_year |>
      unnest(data) |>
      filter(dataset == "temp") |>
      # filter(n_years >= 5) |>
      arrange(station_id) |>
      ggplot(aes(year, station_id)) +
      geom_tile(aes()) +
      scale_x_continuous(breaks = seq(1994, 2022, by = 2)) +
      labs(x = "Year", y = "Station ID", fill = "# Days") +
      theme_bw() +
      theme(
        axis.text.y = element_text(size = 4),
        panel.grid.major.y = element_blank()
      )
  }),
  tar_target(memo_data_stn_paired, {
    # NAD83 / Massachusetts Mainland
    # https://epsg.io/26986
    # meters
    sf_flow <- obs_day |>
      ungroup() |>
      filter(dataset == "flow") |>
      select(-dataset) |>
      rename_with(\(x) str_c("flow_", x), -c(latitude, longitude)) |>
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
      st_transform(crs = 26986)
    sf_temp <- obs_day |>
      ungroup() |>
      filter(dataset == "temp") |>
      select(-dataset) |>
      rename_with(\(x) str_c("temp_", x), -c(latitude, longitude)) |>
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
      st_transform(crs = 26986)

    nearest <- st_nearest_feature(sf_flow, sf_temp)
    distances <- st_distance(sf_flow$geometry, sf_temp[nearest,]$geometry, by_element = TRUE)

    sf_flow |>
      rename(flow_geometry = geometry) |>
      bind_cols(
        rename(sf_temp[nearest,], temp_geometry = geometry),
        distance_m = drop_units(distances)
      ) |>
      rowwise() |>
      mutate(
        data = list({
          flow_data |>
            inner_join(temp_data, by = "date", multiple = "all")
        }),
        n_paired_values = nrow(data)
      ) |>
      filter(n_paired_values > 100)
  }),
  tar_target(memo_data_fig_stn_paired_distance, {
    memo_data_stn_paired |>
      ggplot(aes(distance_m)) +
      stat_ecdf(pad = TRUE) +
      scale_x_continuous(expand = expansion(0.01), breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(
        "Cumulative Frequency",
        labels = scales::percent_format(accuracy = 1),
        breaks = scales::pretty_breaks(n = 10),
        expand = expansion(), limits = c(0, 1)
      ) +
      labs(x = "Distance from Flow Station to\nNearest Temperature Station (m)") +
      theme_bw()
  }),
  tar_target(memo_data_fig_stn_paired_distance_file, {
    filename <- file.path("memo", "data", "fig-stn-paired-distance.png")
    ggsave(filename, plot = memo_data_fig_stn_paired_distance, width = 6, height = 4)
    filename
  }, format = "file"),
  tar_target(memo_data_fig_stn_paired_duration, {
    memo_data_stn_paired |>
      mutate(
        n_paired_years = length(unique(year(data$date)))
      ) |>
      filter(distance_m <= 100) |>
      ggplot(aes(n_paired_years)) +
      stat_ecdf(pad = TRUE) +
      scale_x_continuous(limits = c(0, NA), expand = expansion(0.01), breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(
        "Cumulative Frequency",
        labels = scales::percent_format(accuracy = 1),
        breaks = scales::pretty_breaks(n = 10),
        expand = expansion(), limits = c(0, 1)
      ) +
      labs(x = "# Years of Overlapping Data") +
      theme_bw()
  }),
  tar_target(memo_data_fig_stn_paired_duration_file, {
    filename <- file.path("memo", "data", "fig-stn-paired-duration.png")
    ggsave(filename, plot = memo_data_fig_stn_paired_duration, width = 6, height = 4)
    filename
  }, format = "file"),
  tar_target(memo_data_fig_map_stn_paired, {
    memo_data_stn_paired |>
      mutate(n_paired_years = length(unique(year(data$date)))) |>
      filter(distance_m < 100) |>
      select(-temp_geometry) |>
      st_transform(crs = 4326) |>
      ggplot() +
      geom_sf(data = gis_states, fill = NA) +
      geom_sf(
        aes(size = n_paired_years, fill = flow_source), shape = 21, alpha = 0.5
      ) +
      scale_fill_discrete(drop = TRUE) +
      labs(fill = "Data Source", size = "# Years\nof Data") +
      guides(
        fill = guide_legend(override.aes = list(size = 5))
      ) +
      theme_bw()
  }),
  tar_target(memo_data_fig_map_stn_paired_file, {
    filename <- file.path("memo", "data", "fig-map-stn-paired.png")
    ggsave(filename, plot = memo_data_fig_map_stn_paired, width = 8, height = 4)
    filename
  }, format = "file"),
  tar_target(memo_data_fig_ts_paired, {
    x <- memo_data_stn_paired |>
      filter(distance_m < 100) |>
      rowwise() |>
      mutate(
        plot = list({
          data |>
            select(date, flow_cfs, mean_temp_c) |>
            pivot_longer(-date) |>
            ggplot(aes(date, value)) +
            geom_line() +
            facet_wrap(vars(name), scales = "free_y", ncol = 1, strip.position = "left", labeller = labeller(name = c(
              "flow_cfs" = "Flow (cfs)",
              "mean_temp_c" = "Mean Temp. (degC)"
            ))) +
            labs(
              y = NULL,
              subtitle = glue::glue("Flow Station: {flow_source}:{flow_station_id}\nTemp. Station: {temp_source}:{temp_station_id}\nPOR: {min(data$date)} to {max(data$date)}")
            ) +
            theme_bw() +
            theme(
              strip.background = element_blank(),
              strip.placement = "outside",
              strip.text = element_text(size = 10)
            )
        })
      )
  }),
  tar_target(memo_data_fig_ts_paired_file, {
    x <- memo_data_fig_ts_paired |>
      arrange(desc(n_paired_values))
    filename <- "memo/data/ts-paired.pdf"
    pdf(filename, width = 17, height = 11)
    for (i in seq(1, nrow(x), by = 9)) {
      p <- wrap_plots(x$plot[i:min(nrow(x), i+8)])
      print(p)
    }
    dev.off()
    filename
  }, format = "file")
)