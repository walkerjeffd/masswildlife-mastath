# model development
# - snap site to nhdplushr
# - lom by site
# - lom params vs nhdplushr.vaa

source("_targets.R")
library(mapview)
library(patchwork)
library(broom)
library(hydroGOF)

tar_load(c(obs_temp_daymet, gis_dams))

# 1: snap to nhdplushr -------------------------------------------------------
crs_masp <- 26986
flowline <- st_read("/Users/jeff/Dropbox/Work/masswildlife/gis/nhdplushr/nhdflowline.shp") |>
  st_transform(crs = crs_masp)
waterbody <- st_read("/Users/jeff/Dropbox/Work/masswildlife/gis/nhdplushr/nhdwaterbody.shp") |>
  st_transform(crs = crs_masp)

stn1 <- obs_temp_daymet |>
  filter(!str_detect(name, "Air Temp")) |>
  mutate(
    data_week = list({
      data_day |>
        filter(!(mean_tair_c >= 10 & mean_temp_c < 2)) |>
        group_by(year = year(date), week = week(date)) |>
        summarise(
          date = min(date),
          n_values = sum(n_values),
          across(-c(n_values, date), mean),
          .groups = "drop"
        )
    })
  ) |>
  ungroup() |>
  mutate(station_id = str_c(provider, station_id, sep = ":")) |>
  select(-provider, -source, -type) |>
  rename(station_name = name) |>
  relocate(n_day, .before = data_day) |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
  st_transform(crs = crs_masp)


# 2: filter out waterbodies > 1 km2, within 1km of dams ------------------------
stn1_exclude_dams <- stn1 |>
  st_filter(st_buffer(st_transform(gis_dams, crs = st_crs(stn1)), 100))

waterbody_exclude <- waterbody |>
  filter(AreSqKM > 1, !FTYPE %in% c(466))

stn1_exclude_waterbody <- stn1 |>
  filter(!station_id %in% stn1_exclude_dams) |>
  st_filter(st_buffer(waterbody_exclude, 100))

bind_rows(
  waterbody = stn1_exclude_waterbody,
  dams = stn1_exclude_dams,
  .id = "reason"
) |>
  select(-starts_with("data_")) |>
  mapview(zcol = "reason")

stn_exclude <- setdiff(
  unique(c(stn1_exclude_waterbody$station_id, stn1_exclude_dams$station_id)),
  c(
    "MADEP:W1819 [6945]", "USGS_Conte:Gates Brook [11903]",
    "MADEP:W1282 [6831]", "MADEP:W1818 [6971]",
    "MADEP:W1139 [7066]", "MADEP:W0384 [7056]"
  )
)

stn2 <- stn1 |>
  filter(
    !station_id %in% stn_exclude
  )

# 3: find nearest flowline ---------------------------------------------------
stn3 <- stn2
nearest_index <- st_nearest_feature(stn3, flowline)
stn3$nearest_index <- nearest_index
stn3$comid <- flowline$COMID[nearest_index]
stn3$distance_m <- units::drop_units(st_distance(stn3, flowline[nearest_index, ], by_element = TRUE))

# distribution of snapping distance
stn3 |>
  ggplot(aes(distance_m)) +
  scale_x_log10() +
  geom_histogram()


# 4: snap to nearest flowline ------------------------------------------------
stn4 <- stn3 |>
  rowwise() |>
  mutate(
    geom_flowline = flowline$geometry[nearest_index],
    nearest_points = st_nearest_points(geometry, geom_flowline),
    geom_snap = st_cast(nearest_points, "POINT")[2],
    snap_distance_m = units::drop_units(st_distance(geometry, geom_snap, by_element = TRUE))
  ) |>
  ungroup()


# 5: add nhdplushr vaa --------------------------------------------------------
flowline_vaa <- flowline |>
  st_drop_geometry() |>
  select(COMID, FTYPE, FCODE, TtDASKM, AreSqKM, StrmOrd, Slope, MnElvSm, MxElvSm, ArboltS)
stn5 <- stn4 |>
  left_join(
    flowline_vaa,
    by = c("comid" = "COMID")
  )


# 6: filter by vaa --------------------------------------------------------
stn6 <- stn5 |>
  filter(
    distance_m < 100,
    TtDASKM < 1e4,
    StrmOrd > 0
  )

stn6 |>
  ggplot(aes(as.character(StrmOrd))) +
  stat_count()

stn6 |>
  ggplot(aes(as.character(StrmOrd), TtDASKM)) +
  geom_boxplot()

stn6 |>
  filter(StrmOrd < 4, TtDASKM > 250) |>
  select(-starts_with("data_"), -starts_with("geom_")) |>
  mapview()

stn6 |>
  select(-starts_with("data_"), -starts_with("geom_")) |>
  mapview(zcol = "FTYPE")

stn6 |>
  ggplot(aes(ArboltS, TtDASKM)) +
  geom_point()

stn6 |>
  ggplot(aes()) +
  geom_sf(aes(color = (MnElvSm + MxElvSm) / 2 / 100)) +
  scale_color_viridis_c() +
  theme_void()


# fit lom -----------------------------------------------------------------

lom <- function (x, m, a, b, c) {
  m + (a - m) / (1 + exp(c * (b - x)))
}

fit_lom <- function (x) {
  nls(mean_temp_c ~ lom(mean_tair_c, m, a, b, c), start = list(m = 0, a = 25, b = 12, c = 0.1), data = x)
}

stn_lom <- stn6 |>
  rowwise() |>
  mutate(
    lom_fit = list(possibly(fit_lom)(data_week)),
    lom_augment = list(augment(lom_fit)),
    lom_glance = list(glance(lom_fit)),
    lom_tidy = list(tidy(lom_fit))
  )

# data where model failed
stn_lom |>
  filter(is.null(lom_fit)) |>
  arrange(desc(n_day)) |>
  head(24) |>
  mutate(
    plot = list({
      data_week |>
        ggplot(aes(date)) +
        geom_line(aes(y = mean_tair_c, color = "air")) +
        geom_line(aes(y = mean_temp_c, color = "water")) +
        scale_color_brewer(palette = "Set1") +
        labs(title = station_id, subtitle = station_name)
    })
  ) |>
  pull(plot) |>
  wrap_plots() +
  plot_layout(guides = "collect")

# data where sigma is high
stn_lom |>
  filter(!is.null(lom_fit)) |>
  select(station_id, station_name, lom_glance, data_week) |>
  unnest(lom_glance) |>
  arrange(desc(sigma)) |>
  head(12) |>
  rowwise() |>
  mutate(
    plot_ts = list({
      data_week |>
        ggplot(aes(date)) +
        geom_line(aes(y = mean_tair_c, color = "air")) +
        geom_line(aes(y = mean_temp_c, color = "water")) +
        scale_color_brewer(palette = "Set1") +
        labs(title = station_id, subtitle = station_name)
    }),
    plot_splot = list({
      data_week |>
        ggplot(aes(mean_tair_c, mean_temp_c)) +
        geom_point(size = 0.5, alpha = 0.5) +
        labs(title = station_id, subtitle = station_name)
    })
  ) |>
  pull(plot_splot) |>
  wrap_plots() +
  plot_layout(guides = "collect")

# sigma vs nobs
# need at least 25 data points
stn_lom |>
  filter(!is.null(lom_fit)) |>
  select(station_id, station_name, lom_glance, lom_tidy) |>
  unnest(lom_glance) |>
  unnest(lom_tidy) |>
  ggplot(aes(nobs, std.error)) +
  geom_point() +
  xlim(0, 100) +
  facet_wrap(vars(term), scales = "free_y")

# need at least 25 data points
stn_lom |>
  filter(!is.null(lom_fit)) |>
  select(station_id, station_name, lom_glance, lom_tidy) |>
  unnest(lom_glance) |>
  unnest(lom_tidy) |>
  filter(nobs >= 25) |>
  ggplot(aes(nobs, std.error)) +
  geom_point() +
  facet_wrap(vars(term), scales = "free_y")

stn_lom |>
  filter(!is.null(lom_fit)) |>
  unnest(lom_glance) |>
  filter(nobs >= 25) |>
  ggplot(aes(sigma)) +
  geom_histogram()


# spatial mlr -----------------------------------------------------------

mlr_inp <- stn_lom |>
  filter(!is.null(lom_fit), nrow(data_week) >= 25) |>
  select(-data_mon, -starts_with("geom_"), -nearest_points) |>
  unnest(lom_glance)

mlr_inp |>
  unnest(lom_tidy) |>
  st_drop_geometry() |>
  mutate(MeanElvSm = (MnElvSm + MxElvSm) / 2) |>
  transmute(
    station_id,
    `log10(nobs)` = log10(nobs),
    StrmOrd,
    Slope,
    MeanElvSm,
    `log10(TtDASKM)` = log10(TtDASKM),
    longitude,
    latitude,
    term,
    estimate
  ) |>
  pivot_longer(c(StrmOrd, Slope, MeanElvSm, `log10(TtDASKM)`, longitude, latitude)) |>
  ggplot(aes(value, estimate, color = `log10(nobs)`)) +
  geom_point(size = 0.5) +
  scale_color_viridis_c() +
  facet_grid(vars(term), vars(name), scales = "free")


# pca ---------------------------------------------------------------------

# Perform PCA on the data frame
pca_result <- mlr_inp |>
  st_drop_geometry() |>
  mutate(MeanElvSm = (MnElvSm + MxElvSm) / 2) |>
  transmute(
    Slope,
    MeanElvSm,
    `log10(TtDASKM)` = log10(TtDASKM)
  ) |>
  prcomp(scale. = TRUE)
pca_result
summary(pca_result)

mlr_pca <- mlr_inp |>
  bind_cols(as_tibble(pca_result$x))

# PC1: large basins, low channel slope/elev
# PC2: small basin, low elev
mlr_pca |>
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color = log10(TtDASKM)))


# attach sites to nhdplusv2 -----------------------------------------------

crs_masp <- 26986
nhdv2_flowline <- tar_read(nhd_flowlines) |>
  st_transform(crs = crs_masp) |>
  st_zm()

mapview(nhdv2_flowline, zcol = "streamorde")

mlr_nhdv2_1 <- mlr_inp |>
  select(station_id, station_name, data_day, data_week, starts_with("lom_"), nhdhr_distance_m = distance_m, nhdhr_TtDASKM = TtDASKM)

# join to nearest flowline
mlr_nhdv2_2 <- mlr_nhdv2_1
nearest_index <- st_nearest_feature(mlr_nhdv2_2, nhdv2_flowline)
mlr_nhdv2_2$nearest_index <- nearest_index
mlr_nhdv2_2$comid <- nhdv2_flowline$comid[nearest_index]
mlr_nhdv2_2$distance_m <- units::drop_units(st_distance(mlr_nhdv2_2, nhdv2_flowline[nearest_index, ], by_element = TRUE))

# distribution of snapping distance
mlr_nhdv2_2 |>
  ggplot(aes(distance_m)) +
  scale_x_log10() +
  geom_histogram()

# snap to nearest flowline
mlr_nhdv2_3 <- mlr_nhdv2_2 |>
  rowwise() |>
  mutate(
    geom_flowline = nhdv2_flowline$geometry[nearest_index],
    nearest_points = st_nearest_points(geometry, geom_flowline),
    geom_snap = st_cast(nearest_points, "POINT")[2],
    snap_distance_m = units::drop_units(st_distance(geometry, geom_snap, by_element = TRUE))
  ) |>
  ungroup()

mlr_nhdv2_3 |>
  ggplot(aes(distance_m)) +
  scale_x_log10() +
  geom_histogram()

mlr_nhdv2_3 |>
  ggplot(aes(nhdhr_distance_m, distance_m)) +
  geom_abline() +
  geom_point(aes(color = log10(nhdhr_TtDASKM))) +
  scale_color_viridis_c()

unlink("gis/mlr_nhdv2_3.shp")
mlr_nhdv2_3 |>
  select(-starts_with("lom_"), -data_day, -data_week, -starts_with("geom_"), -nearest_points) |>
  mutate(comid = as.character(comid)) |>
  st_write("gis/mlr_nhdv2_3.shp")

# exclude distance_m > 100
mlr_nhdv2_4 <- mlr_nhdv2_3 |>
  filter(distance_m < 100)

# add attrs
mlr_nhdv2_5 <- mlr_nhdv2_4 |>
  left_join(
    nhdv2_flowline |>
      st_drop_geometry(),
    by = "comid"
  )

# pca
pca_nhdv2 <- mlr_nhdv2_5 |>
  filter(!is.na(tot_elev_min)) |>
  st_drop_geometry() |>
  select(starts_with("tot_")) |>
  select(-tot_elev_min, -tot_elev_max, -tot_ndams2013, -tot_nid_storage2013, -tot_major2013, -tot_stream_length, -tot_bfi) |>
  mutate(
    tot_norm_storage2013 = log10(tot_norm_storage2013 + 1),
    tot_basin_area = log10(tot_basin_area)
  ) |>
  prcomp(scale. = TRUE)
pca_nhdv2
summary(pca_nhdv2)

mlr_nhdv2_pca <- mlr_nhdv2_5 |>
  filter(!is.na(tot_elev_min)) |>
  bind_cols(as_tibble(pca_nhdv2$x))

# PC1: large basins, urban
# PC2: large basins, rural
# PC3: small basins
mlr_nhdv2_pca |>
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color = log10(tot_basin_area)))
mlr_nhdv2_pca |>
  ggplot(aes(PC1, PC3)) +
  geom_point(aes(color = log10(tot_basin_area)))

mlr_nhdv2_pca |>
  select(PC1) |>
  mapview(zcol = "PC1")
mlr_nhdv2_pca |>
  select(PC2) |>
  mapview(zcol = "PC2")
mlr_nhdv2_pca |>
  select(PC3) |>
  mapview(zcol = "PC3")

# PC1 has warmer highest, smaller slopes
mlr_nhdv2_pca |>
  st_drop_geometry() |>
  select(starts_with("PC"), lom_tidy) |>
  unnest(lom_tidy) |>
  select(PC1, PC2, PC3, term, estimate) |>
  pivot_longer(starts_with("PC")) |>
  ggplot(aes(value, estimate)) +
  geom_point() +
  facet_grid(vars(term), vars(name), scales = "free")

# lom estimates vs nhdv2 attr
mlr_nhdv2_5 |>
  st_drop_geometry() |>
  transmute(
    station_id, station_name, lom_tidy,
    tot_impv11,
    tot_norm_storage2013 = log10(tot_norm_storage2013 + 1),
    tot_basin_area = log10(tot_basin_area),
    tot_basin_slope,
    tot_elev_mean,
    tot_stream_slope
  ) |>
  unnest(lom_tidy) |>
  pivot_longer(starts_with("tot_"))

mlr_nhdv2_5 |>
  unnest(lom_tidy) |>
  nest_by(term) |>
  mutate(
    plot = list({
      data |>
        ggplot() +
        geom_sf(aes(color = estimate)) +
        scale_color_viridis_c() +
        labs(title = term)
    })
  ) |>
  pull(plot) |>
  wrap_plots()

mlr_nhdv2_5 |>
  unnest(lom_tidy) |>
  filter(term == "m") |>
  select(-nearest_points, -starts_with(c("geom_", "data_", "lom_"))) |>
  mapview(zcol = "estimate")

mlr_nhdv2_5 |>
  unnest(lom_tidy) |>
  filter(term == "c") |>
  arrange(estimate) |>
  head(12) |>
  rowwise() |>
  mutate(
    plot_ts = list({
      data_week |>
        ggplot(aes(date)) +
        geom_line(aes(y = mean_tair_c, color = "air")) +
        geom_line(aes(y = mean_temp_c, color = "water")) +
        scale_color_brewer(palette = "Set1") +
        labs(title = station_id, subtitle = station_name)
    }),
    plot_splot = list({
      data_day |>
        ggplot(aes(mean_tair_c, mean_temp_c)) +
        geom_point(aes(color = month(date) >= 8), size = 0.5, alpha = 0.5) +
        scale_color_brewer(palette = "Set1") +
        geom_line(data = lom_augment, aes(y = .fitted)) +
        labs(title = station_id, subtitle = station_name)
    })
  ) |>
  pull(plot_splot) |>
  wrap_plots() +
  plot_layout(guides = "collect")

# nash sutcliffe (NSE)
mlr_nhdv2_5_gof <- mlr_nhdv2_5 |>
  rowwise() |>
  mutate(
    gof = list({
      z <- hydroGOF::gof(lom_augment$.fitted, lom_augment$mean_temp_c)
      pivot_wider(as_tibble(as.data.frame(z), rownames = "name"), values_from = V1)
    })
  ) |>
  unnest(gof)

mlr_nhdv2_5_gof |>
  select(-nearest_points, -starts_with(c("geom_", "data_", "lom_"))) |>
  mapview(zcol = "NSE")

mlr_nhdv2_5_gof |>
  rowwise() |>
  mutate(nobs = nrow(data_week)) |>
  ggplot(aes(nobs, NSE)) +
  geom_point()

mlr_nhdv2_5_gof |>
  arrange(desc(NSE)) |>
  head(12) |>
  rowwise() |>
  mutate(
    plot_ts = list({
      data_week |>
        ggplot(aes(date)) +
        geom_line(aes(y = mean_tair_c, color = "air")) +
        geom_line(aes(y = mean_temp_c, color = "water")) +
        scale_color_brewer(palette = "Set1") +
        labs(title = station_id, subtitle = station_name)
    }),
    plot_splot = list({
      data_day |>
        ggplot(aes(mean_tair_c, mean_temp_c)) +
        geom_point(aes(color = week(date) > 30), size = 0.5, alpha = 0.5) +
        geom_line(data = lom_augment, aes(y = .fitted)) +
        scale_color_brewer(palette = "Set1") +
        labs(title = station_id, subtitle = station_name)
    })
  ) |>
  pull(plot_splot) |>
  wrap_plots() +
  plot_layout(guides = "collect")

mlr_nhdv2_5 |>
  unnest(lom_tidy) |>
  ggplot(aes(estimate)) +
  geom_boxplot() +
  geom_jitter(aes(y = 0), alpha = 0.2, size = 0.5, width = 0) +
  facet_wrap(vars(term), scales = "free_x", ncol = 1) +
  theme_minimal()

# NEXT: seasonal stats model (mean max weekly)
