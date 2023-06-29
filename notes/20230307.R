source("_targets.R")

gis_state <- tar_read(gis_state)
temp_day <- tar_read(temp_day)

# station tally
temp_day |>
  mutate(
    source = fct_inorder(source)
  ) |>
  arrange(source, provider) |>
  mutate(provider = fct_inorder(provider)) |>
  tabyl(provider, source) |>
  adorn_totals(where = "row")
  # write_csv("~/count-provider-source.csv")

# map of stations, color by source, size by n
temp_day |>
  mutate(
    source = fct_inorder(source)
  ) |>
  mutate(n_day = nrow(data)) |>
  select(-data) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  ggplot() +
  geom_sf(data = gis_state, fill = NA) +
  geom_sf(aes(color = source, size = n_day), alpha = 0.5) +
  scale_size_continuous(trans = "log10", range = c(0.25, 5)) +
  theme_minimal()

# map of stations, size by # years
temp_day |>
  mutate(
    source = fct_inorder(source),
    n_year = data |>
      filter(month(date) %in% 7:8) |>
      distinct(year = year(date)) |>
      nrow()
  ) |>
  ggplot(aes(n_year)) +
  stat_ecdf(direction = "hv") +
  scale_y_continuous("Cumul. Percentile", labels = scales::percent_format(accuracy = 1), breaks = scales::pretty_breaks(n = 10), expand = expansion()) +
  scale_x_continuous("# Years of July-Aug Data", breaks = scales::pretty_breaks(n = 10)) +
  theme_bw()

# map of stations with at least 5 years of jul-aug data
temp_day |>
  mutate(
    source = fct_inorder(source),
    n_year = data |>
      filter(month(date) %in% 7:8) |>
      distinct(year = year(date)) |>
      nrow()
  ) |>
  filter(n_year >= 5) |>
  select(-data) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  ggplot() +
  geom_sf(data = gis_state, fill = NA) +
  geom_sf(aes(color = source, size = n_year), alpha = 0.5) +
  theme_minimal()

# plot of all daily data
x <- temp_day |>
  ungroup() |>
  # sample_frac(size = 0.05) |>
  select(provider, source, station_id, data) |>
  unnest(data) |>
  filter(is.na(flagged) | !flagged)
x |>
  ggplot(aes(ymd(20001231) + days(yday(date)), mean_temp_c)) +
  geom_line(aes(group = interaction(provider, source, station_id, year(date))), alpha = 0.025) +
  scale_x_date(date_labels = "%b", breaks = ymd(20010101) + months(0:12), expand = expansion()) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(x = "Day of Year", y = "Daily Mean Temp (C)") +
  theme_bw()

# distribution of max summer by station-year
x |>
  filter(month(date) %in% 7:8) |>
  group_by(provider, source, station_id, year = year(date)) |>
  summarise(
    `max(mean_temp_c)` = max(mean_temp_c),
    .groups = "drop"
  ) |>
  ggplot(aes(`max(mean_temp_c)`)) +
  stat_ecdf(pad = FALSE) +
  scale_y_continuous("Cumul. Percentile", labels = scales::percent_format(accuracy = 1), breaks = scales::pretty_breaks(n = 10), expand = expansion(), limits = c(0, 1)) +
  scale_x_continuous("July/Aug Max(mean daily) Temp (C)", breaks = scales::pretty_breaks(n = 10)) +
  coord_cartesian(xlim = c(10, 30)) +
  theme_bw()


# annual timeseries of max july-aug temp
x |>
  filter(month(date) %in% 7:8) |>
  group_by(provider, source, station_id, year = year(date)) |>
  summarise(
    `max(mean_temp_c)` = max(mean_temp_c),
    .groups = "drop"
  ) |>
  filter(`max(mean_temp_c)` > 0) |>
  ggplot(aes(year, `max(mean_temp_c)`)) +
  geom_line(aes(group = interaction(provider, source, station_id)), alpha = 0.5) +
  geom_point(size = 1, alpha = 0.5) +
  scale_x_continuous("Year", breaks = scales::pretty_breaks(n = 12)) +
  scale_y_continuous("July/Aug Max(mean daily) Temp (C)", breaks = scales::pretty_breaks(n = 10)) +
  theme_bw()

# map of long-term mean(max july temp)
x |>
  filter(month(date) %in% 7:8) |>
  distinct() |>
  group_by(provider, source, station_id, year = year(date)) |>
  summarise(
    `max(mean_temp_c)` = max(mean_temp_c),
    .groups = "drop_last"
  ) |>
  filter(`max(mean_temp_c)` > 10) |>
  summarise(
    `mean(max(mean_temp_c))` = mean(`max(mean_temp_c)`),
    .groups = "drop"
  ) |>
  left_join(
    temp_day |>
      select(provider, source, station_id, latitude, longitude) |>
      group_by(provider, source, station_id) |>
      filter(row_number() == 1),
    by = c("provider", "source", "station_id")
  ) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  ggplot() +
  geom_sf(data = gis_state, fill = NA) +
  geom_sf(aes(color = `mean(max(mean_temp_c))`), alpha = 0.75, size = 4) +
  scale_color_viridis_c("Max July\nTemp (C)", option = "H") +
  theme_minimal()
