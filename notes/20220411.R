source("_targets.R")

tar_load(c(gis_state, flow_day, temp_day))

# map of all flow stations, color by source, size by n
flow_day |>
  mutate(
    source = fct_inorder(source)
  ) |>
  rowwise() |>
  mutate(n_day = nrow(data)) |>
  select(-data) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  ggplot() +
  geom_sf(data = gis_state, fill = NA) +
  geom_sf(aes(color = source, size = n_day), alpha = 0.5) +
  scale_size_continuous(trans = "log10", range = c(0.25, 5)) +
  theme_minimal()

# only stations with data since 2005 (start of temp data)
flow_day_2005 <- flow_day |>
  rowwise() |>
  mutate(
    data = list(filter(data, date >= ymd(20050101)))
  ) |>
  filter(nrow(data) > 0) |>
  mutate(
    start = min(data$date),
    end = max(data$date)
  ) |>
  ungroup()

# segment plot of flow periods
flow_day_2005 |>
  rowwise() |>
  mutate(
    min_date = min(data$date),
    max_date = max(data$date),
    n_days = max_date - min_date
  ) |>
  arrange(desc(min_date), max_date) |>
  mutate(station_id = fct_inorder(station_id)) |>
  ggplot(aes()) +
  geom_segment(aes(x = min_date, xend = max_date, y = station_id, yend = station_id)) +
  facet_grid(vars(provider), vars(), space = "free_y", scales = "free_y") +
  labs(x = "date") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 6))

# map of 2005+ flow stations, color by source, size by n
flow_day_2005 |>
  mutate(
    source = fct_inorder(source)
  ) |>
  rowwise() |>
  mutate(n_day = nrow(data)) |>
  select(-data) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  ggplot() +
  geom_sf(data = gis_state, fill = NA) +
  geom_sf(aes(color = source, size = n_day), alpha = 0.5) +
  scale_size_continuous(trans = "log10", range = c(0.25, 5)) +
  theme_minimal()

# annual values
flow_wyr_2005 <- flow_day_2005 |>
  rowwise() |>
  mutate(
    data = list(
      data |>
        filter(!is.na(flow_cfs)) |>
        mutate(wyear = water_year(date)) |>
        group_by(wyear) |>
        summarise(
          n_day = n(),
          min = min(flow_cfs),
          median = median(flow_cfs),
          max = max(flow_cfs)
        ) |>
        filter(n_day >= 300)
    )
  ) |>
  filter(nrow(data) > 0) |>
  mutate(
    min_wyear = min(data$wyear),
    max_wyear = max(data$wyear),
    n_wyears = max_wyear - min_wyear
  )

# line plot of annual median flow
flow_wyr_2005 |>
  select(station_id, data) |>
  unnest(data) |>
  ggplot(aes(wyear, median)) +
  geom_line(aes(group = station_id)) +
  geom_point() +
  scale_y_log10() +
  labs(x = "water year", y = "median annual flow") +
  theme_bw()

# map of long-term median flow
flow_wyr_2005 |>
  mutate(
    source = fct_inorder(source)
  ) |>
  rowwise() |>
  mutate(median_flow_cfs = median(data$median)) |>
  select(-data) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  ggplot() +
  geom_sf(data = gis_state, fill = NA) +
  geom_sf(aes(color = median_flow_cfs, size = n_wyears), alpha = 0.5) +
  scale_color_viridis_c(trans = "log10") +
  scale_size_continuous(range = c(0.25, 5)) +
  theme_minimal()

# cumul dist of longterm median
flow_wyr_2005 |>
  mutate(
    source = fct_inorder(source)
  ) |>
  rowwise() |>
  mutate(median_flow_cfs = median(data$median)) |>
  select(-data) |>
  ggplot(aes(median_flow_cfs)) +
  stat_ecdf(pad = FALSE) +
  scale_y_continuous("Cumul. Percentile", labels = scales::percent_format(accuracy = 1), breaks = scales::pretty_breaks(n = 10), expand = expansion(), limits = c(0, 1)) +
  scale_x_log10("Long-term Median Flow (cfs)") +
  theme_bw()


# co-occurance of flow and temp -------------------------------------------

# NAD83 / Massachusetts Mainland
# https://epsg.io/26986
# meters

sf_flow <- flow_day_2005 |>
  select(flow_id = station_id, flow_name = name, flow_type = type, latitude, longitude) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(crs = 26986)
sf_temp <- temp_day |>
  ungroup() |>
  transmute(
    temp_id = str_c(provider, station_id, sep = ":"),
    temp_name = name,
    temp_type = type,
    latitude, longitude
  ) |> # merge provider and station into temp_id
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(crs = 26986)

nearest <- st_nearest_feature(sf_flow, sf_temp)
distances <- st_distance(sf_flow$geometry, sf_temp[nearest,]$geometry, by_element = TRUE)

flow_nearest_temp <- sf_flow |>
  rename(flow_geometry = geometry) |>
  bind_cols(
    rename(sf_temp[nearest,], temp_geometry = geometry),
    distance_m = drop_units(distances)
  )

flow_nearest_temp |>
  filter(distance_m < 336) |>
  ggplot() +
  geom_sf(data = gis_state, fill = NA) +
  geom_sf(aes(geometry = flow_geometry, color = "flow"), alpha = 0.5, size = 3) +
  geom_sf(aes(geometry = temp_geometry, color = "temp"), alpha = 0.5, size = 3) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()

flow_nearest_temp |>
  filter(distance_m < 335) |>
  ggplot() +
  geom_sf(aes(geometry = flow_geometry, color = "flow"), alpha = 0.5, size = 3) +
  geom_sf(aes(geometry = temp_geometry, color = "temp"), alpha = 0.5, size = 3)

# cumul dist of pairwise distances
flow_nearest_temp |>
  ggplot(aes(distance_m)) +
  stat_ecdf(pad = FALSE) +
  geom_vline(
    xintercept = median(flow_nearest_temp$distance_m),
    linetype = "dashed"
  ) +
  scale_y_continuous("Cumul. Percentile", labels = scales::percent_format(accuracy = 1), breaks = scales::pretty_breaks(n = 10), expand = expansion(), limits = c(0, 1)) +
  coord_cartesian(xlim = c(0, 5000)) +
  annotate(label = glue("{round(median(flow_nearest_temp$distance_m))} m"), x = median(flow_nearest_temp$distance_m), y = 0.5, geom = "text", hjust = -0.1) +
  labs(x = "Distance (m)") +
  theme_bw()

sum(flow_nearest_temp$distance_m < 1)

flow_nearest_temp_data <- flow_nearest_temp |>
  filter(distance_m < 336) |>
  st_drop_geometry() |>
  select(flow_id, flow_name, temp_id, temp_name, distance_m) |>
  left_join(
    select(flow_day, flow_id = station_id, flow_name = name, flow_data = data),
    by = c("flow_id", "flow_name")
  ) |>
  left_join(
    transmute(temp_day, temp_id = str_c(provider, station_id, sep = ":"), temp_name = name, temp_data = list(data)),
    by = c("temp_id", "temp_name")
  ) |>
  rowwise() |>
  mutate(
    data = list(
      select(flow_data, date, flow_cfs) |>
        inner_join(select(temp_data, date, temp_c = mean_temp_c), by = "date", multiple = "all")
    )
  ) |>
  filter(nrow(data) > 0) |>
  mutate(
    plot = list({
      data |>
        pivot_longer(-date) |>
        ggplot(aes(date, value)) +
        geom_line() +
        facet_wrap(vars(name), scales = "free_y", ncol = 1) +
        labs(
          y = NULL,
          subtitle = glue("flow: {flow_id} ({flow_name})\ntemp: {temp_id}\npor: {min(data$date)} to {max(data$date)}")
        )
    }),
    start = min(data$date),
    end = max(data$date),
    n_days = as.numeric(end - start)
  ) |>
  arrange(desc(n_days))

pdf("pdf/flow-temp-day-ts.pdf", width = 17, height = 11)
for (i in seq(1, nrow(flow_nearest_temp_data), by = 9)) {
  p <- wrap_plots(flow_nearest_temp_data$plot[i:min(nrow(flow_nearest_temp_data), i+8)])
  print(p)
}
dev.off()
