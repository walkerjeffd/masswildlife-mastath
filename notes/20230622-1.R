source("_targets.R")
library(broom)
library(patchwork)

tar_load(c(obs_temp_flowline, nhd_flowlines))
tar_load(gis_state)

obs_temp_flowline |>
  select(provider, source, station_id, name, type, n_day, latitude, longitude, comid, distance_m) |>
  st_write("gis/obs_temp_flowline.shp")

x <- obs_temp_flowline |>
  left_join(st_drop_geometry(nhd_flowlines), by = "comid")

x |>
  unnest(data_week) |>
  ggplot(aes(mean_tair_c, mean_temp_c)) +
  geom_point(aes(color = tot_elev_mean), alpha = 0.25, size = 0.5) +
  ylim(0, NA) +
  scale_color_viridis_c() +
  theme_minimal()

x |>
  unnest(data_week) |>
  mutate(headwaters = streamorde <= 2) |>
  filter(!is.na(headwaters)) |>
  ggplot(aes(mean_tair_c, mean_temp_c)) +
  # geom_point(aes(color = tot_elev_mean), alpha = 0.25, size = 0.5) +
  geom_point(aes(color = log10(pmax(tot_basin_area, 1))), alpha = 0.25, size = 0.5) +
  ylim(0, NA) +
  scale_color_viridis_c() +
  theme_minimal()

x |>
  unnest(data_week) |>
  mutate(headwaters = streamorde <= 2) |>
  filter(!is.na(headwaters)) |>
  ggplot(aes(mean_tair_c, mean_temp_c)) +
  geom_point(aes(color = tot_elev_mean), alpha = 0.25, size = 0.5) +
  ylim(0, NA) +
  scale_color_viridis_c() +
  facet_wrap(vars(headwaters), ncol = 1) +
  theme_minimal()

x |>
  unnest(data_week) |>
  mutate(headwaters = streamorde <= 2) |>
  filter(!is.na(headwaters)) |>
  ggplot(aes(mean_tair_c, mean_temp_c, color = pmin(slope, 0.05))) +
  geom_point(aes(), alpha = 0.25, size = 0.5) +
  ylim(0, NA) +
  scale_color_viridis_c() +
  theme_minimal()

x |>
  unnest(data_week) |>
  mutate(headwaters = streamorde <= 2) |>
  filter(!is.na(headwaters)) |>
  ggplot(aes(mean_tair_c, mean_temp_c, color = pmin(tot_bfi, 60))) +
  geom_point(aes(), alpha = 0.25, size = 0.5) +
  ylim(0, NA) +
  scale_color_viridis_c() +
  theme_minimal()

x |>
  unnest(data_week) |>
  ggplot(aes(mean_tair_c, mean_temp_c, color = streamorde)) +
  geom_point(aes(), alpha = 0.25, size = 0.5) +
  ylim(0, NA) +
  scale_color_viridis_c() +
  labs(x = "air temp (degC)", y = "water temp (degC)") +
  theme_minimal()

x |>
  unnest(data_week) |>
  ggplot(aes(mean_tair_c, mean_temp_c, color = week >= 30)) +
  geom_point(aes(), alpha = 0.25, size = 0.5) +
  ylim(0, NA) +
  # scale_color_viridis_c() +
  labs(x = "air temp (degC)", y = "water temp (degC)") +
  theme_minimal()


x |>
  unnest(data_week) |>
  ggplot(aes(mean_tair_c, mean_temp_c, color = year)) +
  geom_point(aes(), alpha = 0.25, size = 0.5) +
  ylim(0, NA) +
  scale_color_viridis_c() +
  labs(x = "air temp (degC)", y = "water temp (degC)") +
  theme_minimal()

st_drop_geometry(nhd_flowlines) |>
  ggplot(aes(factor(streamorde), slope)) +
  geom_boxplot()

x |>
  unnest(data_week) |>
  mutate(headwaters = streamorde <= 2) |>
  filter(!is.na(headwaters)) |>
  ggplot(aes(mean_tair_c, mean_temp_c, color = headwaters)) +
  geom_point(aes(), alpha = 0.25, size = 0.5) +
  geom_smooth(se = FALSE) +
  ylim(0, NA) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()


x |>
  unnest(data_week) |>
  ggplot(aes(mean_tair_c, mean_temp_c, color = log10(acc_nid_storage2013 + 1))) +
  geom_point(aes(), alpha = 0.25, size = 0.5) +
  ylim(0, NA) +
  scale_color_viridis_c() +
  theme_minimal()


# fit nonlinear -----------------------------------------------------------

# Define the non-linear model function
model_func <- function(x, m, a, b, c) {
  m + (a - m) / (1 + exp(c * (b - x)))
}

x_inp <- x |>
  unnest(data_week)

# Fit the non-linear model to the data
fit <- nls(mean_temp_c ~ model_func(mean_tair_c, m, a, b, c), start = list(m = 0, a = 20, b = 15, c = 45), data = x_inp)

# Print the estimated parameters
print(summary(fit)$parameters)

print(fit)

augment(fit) |>
  ggplot(aes(mean_tair_c)) +
  geom_point(aes(y = mean_temp_c), size = 0.5, alpha = 0.05) +
  geom_line(aes(y = .fitted), color = "deepskyblue")


# multi-models ------------------------------------------------------------


# midpoint of warming/cooling
x |>
  unnest(data_week) |>
  ggplot(aes(week, mean_temp_c)) +
  geom_point() +
  geom_smooth()

y_headwater_season <- x |>
  st_drop_geometry() |>
  filter(distance_m <= 100, !is.na(streamorde)) |>
  unnest(data_week) |>
  mutate(
    headwaters = streamorde <= 1,
    season = if_else(week <= 30, "rise", "fall")
  ) |>
  nest_by(headwaters, season) |>
  mutate(
    fit = list({
      nls(mean_temp_c ~ model_func(mean_tair_c, m, a, b, c), start = list(m = 0, a = 20, b = 15, c = 45), data = data)
    }),
    aug = list(augment(fit))
  )

y_headwater_season |>
  select(headwaters, season, aug) |>
  unnest(aug) |>
  ggplot(aes(mean_tair_c)) +
  geom_point(aes(y = mean_temp_c), size = 0.5, alpha = 0.05) +
  geom_line(aes(y = .fitted, color = season), linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(vars(headwaters), labeller = "label_both")

y_headwater_season |>
  select(headwaters, season, aug) |>
  unnest(aug) |>
  ggplot(aes(mean_tair_c)) +
  geom_point(aes(y = mean_temp_c), size = 0.5, alpha = 0.05) +
  geom_line(aes(y = .fitted, color = headwaters)) +
  facet_wrap(vars(season), labeller = "label_both")


# models by site ----------------------------------------------------------

fit_nls <- function(x) {
  nls(mean_temp_c ~ model_func(mean_tair_c, m, a, b, c), start = list(m = 0, a = 25, b = 12, c = 0.1), data = x)
}

y_site <- x |>
  filter(distance_m <= 100) |>
  rowwise() |>
  mutate(
    fit = list(possibly(fit_nls)(data_week))
  )

y_site |>
  filter(!is.null(fit)) |>
  mutate(
    aug = list(augment(fit))
  ) |>
  unnest(aug) |>
  ggplot(aes(mean_tair_c)) +
  geom_point(aes(y = mean_temp_c), size = 0.5, alpha = 0.05) +
  geom_line(aes(y = .fitted, group = str_c(source, station_id, sep = ":"), color = streamorde), alpha = 0.25) +
  scale_color_viridis_c()

glance(y_site$fit[[100]])
tidy(y_site$fit[[100]])

y_site_tidy <- y_site |>
  filter(!is.null(fit)) |>
  mutate(
    tidy = list(tidy(fit))
  ) |>
  unnest(tidy)

y_site_tidy |>
  ggplot(aes(factor(streamorde), estimate)) +
  geom_boxplot() +
  facet_wrap(vars(term), scales = "free_y")

# {cat,tot}_elev_mean
# {cat,tot}_stream_slope
# tot_nid_storage2013
y_site_tidy |>
  ggplot(aes(log10(tot_basin_area), estimate)) +
  # ggplot(aes(slope, estimate)) +
  # ggplot(aes(log10(tot_nid_storage2013 + 1), estimate)) +
  geom_point(size = 0.5) +
  scale_color_viridis_c() +
  facet_wrap(vars(term), scales = "free_y") +
  theme_minimal()

y_site_tidy |>
  ggplot(aes(estimate)) +
  stat_ecdf() +
  facet_wrap(vars(term), scales = "free_x")

y_site_tidy |>
  nest_by(term) |>
  mutate(
    plot = list({
      ggplot(data) +
        geom_sf(data = gis_state, fill = NA) +
        geom_sf(aes(color = estimate)) +
        scale_color_viridis_c() +
        labs(title = term) +
        theme_void()
    })
  ) |>
  pull(plot) |>
  wrap_plots()


# streamstats -------------------------------------------------------------

get_streamstats_watershed <- function (latitude, longitude) {
  res <- httr::GET(
    "https://streamstats.usgs.gov/streamstatsservices/watershed.geojson",
    query = list(
      rcode = "MA",
      xlocation = longitude,
      ylocation = latitude,
      crs = 4326,
      includeparameters = TRUE,
      includeflowtypes = TRUE,
      includefeatures = TRUE,
      simplify = TRUE
    )
  )
  content(res, encoding = "UTF-8")
}

get_streamstats_parameters <- function (workspaceID) {
  res <- httr::GET(
    "https://streamstats.usgs.gov/streamstatsservices/parameters.json",
    query = list(
      rcode = "MA",
      workspaceID = workspaceID,
      includeparameters = TRUE
    )
  )
  body <- content(res, encoding = "UTF-8")
  list(
    messages = body$messages,
    parameters = bind_rows(body$parameters)
  )
}

y_streamstats_1 <- y_site |>
  filter(tot_basin_area > 10, tot_basin_area < 20, distance_m < 10) |>
  head() |>
  mutate(
    streamstats = list({
      watershed <- get_streamstats_watershed(latitude, longitude)
      parameters <- get_streamstats_parameters(watershed$workspaceID)
      list(
        watershed = watershed,
        parameters = parameters
      )
    })
  )


y_streamstats_1 |>
  select(provider, station_id, station_name = name, totdasqkm, streamstats) |>
  unnest_wider(streamstats) |>
  unnest_wider(parameters) |>
  select(-messages) |>
  rowwise() |>
  mutate(parameters = list(pivot_wider(select(parameters, code, value), names_from = "code"))) |>
  unnest(parameters) |>
  # select(totdasqkm, DRNAREA) |>
  mutate(DRNAREA = drop_units(set_units(set_units(DRNAREA, "mi^2"), "km^2"))) |>
  st_as_sf() |>
  select(station_id) |>
  head(1) |>
  mapview()

fc <- y_streamstats_1$streamstats[[1]]$watershed$featurecollection
leaflet() |>
  addTiles() |>
  addGeoJSON(fc[[2]]$feature) |>
  addGeoJSON(fc[[1]]$feature) |>
  addPolylines(data = st_cast(st_transform(st_zm(fl), crs = 4326), "LINESTRING"), color = "red")

y_streamstats_1$streamstats[[1]]

watershed <- get_streamstats_watershed(42.71867, -73.09479)
parameters <- get_streamstats_parameters(watershed$workspaceID)

leaflet() |>
  addTiles() |>
  addGeoJSON(watershed$featurecollection[[2]]$feature) |>
  addGeoJSON(watershed$featurecollection[[1]]$feature)
  # addGeoJSON(fl)

tar_load(nhd_sf_flowlines)

fl <- nhd_sf_flowlines |>
  filter(COMID == y_streamstats_1$comid[[1]])


library(httr)
url <- "https://streamstats.usgs.gov/streamstatsservices/watershed.geojson?rcode=NY&xlocation=-74.524&ylocation=43.939&crs=4326&includeparameters=false&includeflowtypes=false&includefeatures=true&simplify=true"
params <- list(
  rcode = "MA",
  xlocation = -72.67065,
  ylocation = 42.70342,
  crs = 4326,
  includeparameters = TRUE,
  includeflowtypes = TRUE,
  includefeatures = TRUE,
  simplify = TRUE
)
watershed_res <- httr::GET(url, query = params)
watershed_body <- content(watershed_res, encoding = "UTF-8")

watershed_body$workspaceID
watershed_body$featurecollection[[1]] # point
watershed_body$featurecollection[[2]] # basin
bind_rows(watershed_body$parameters)
print(str_c(watershed_body$messages, sep = "\n"))

# map
library(leaflet)
leaflet() |>
  addTiles() |>
  addGeoJSON(watershed_body$featurecollection[[2]]$feature) |>
  addGeoJSON(watershed_body$featurecollection[[1]]$feature)

# compute basin params
parameters_res <- httr::GET(
  "https://streamstats.usgs.gov/streamstatsservices/parameters.json",
  query = list(
    rcode = "MA",
    workspaceID = watershed_body$workspaceID,
    includeparameters = TRUE
  )
)
parameters_body <- content(parameters_res, encoding = "UTF-8")
parameters_df <- bind_rows(parameters_body$parameters)
parameters_df


# compute basin params
flow_res <- httr::GET(
  "https://streamstats.usgs.gov/streamstatsservices/flowstatistics.json",
  query = list(
    rcode = "MA",
    workspaceID = watershed_body$workspaceID,
    includeflowtypes = TRUE
  )
)
flow_body <- content(flow_res, encoding = "UTF-8")
flow_df <- bind_rows(flow_body$flow)
flow_df



# snap stations to nhdplushr ----------------------------------------------

crs_masp <- 26986

nhdplushr_flowlines <- st_read("/Users/jeff/Dropbox/Work/masswildlife/gis/nhdplushr/nhdflowline.shp") |>
  st_transform(crs = crs_masp)
stn <- obs_temp_flowline |>
  select(-comid, -distance_m) |>
  st_transform(crs = crs_masp) |>
  select(-starts_with("data_"))

# step 1: find nearest flowline
stn_nearest_index <- st_nearest_feature(stn, nhdplushr_flowlines)
stn$flowline_index <- stn_nearest_index
stn$comid <- nhdplushr_flowlines$COMID[stn_nearest_index]
stn$distance_m <- units::drop_units(st_distance(stn, nhdplushr_flowlines[stn_nearest_index,], by_element = TRUE))
stn

# step 2: snap to flowline
stn_snap <- stn |>
  mutate(
    flowline_geometry = nhdplushr_flowlines$geometry[flowline_index],
    nearest_points = st_nearest_points(geometry, flowline_geometry),
    snap_geometry = st_cast(nearest_points, "POINT")[2],
    snap_distance_m = units::drop_units(st_distance(geometry, snap_geometry, by_element = TRUE))
  ) |>
  ungroup() |>
  mutate(
    TtDASKM = nhdplushr_flowlines$TtDASKM[flowline_index]
  )


unlink("gis/stn.shp")
stn_snap |>
  select(provider, source, station_id, name, type, n_day, latitude, longitude, comid, distance_m) |>
  mutate(comid = as.character(comid)) |>
  st_write("gis/stn.shp")

unlink("gis/stn_snap.shp")
stn_snap |>
  st_drop_geometry() |>
  rename() |>
  select(provider, source, station_id, name, type, n_day, latitude, longitude, comid, distance_m, geometry = snap_geometry) |>
  st_as_sf() |>
  mutate(comid = as.character(comid)) |>
  st_write("gis/stn_snap.shp")

unlink("gis/stn_snap_edge.shp")
stn_snap |>
  st_drop_geometry() |>
  rename() |>
  select(provider, source, station_id, name, type, n_day, latitude, longitude, comid, distance_m, geometry = nearest_points) |>
  st_as_sf() |>
  mutate(comid = as.character(comid)) |>
  st_write("gis/stn_snap_edge.shp")

stn_snap |>
  filter(distance_m < 100, TtDASKM < 1e4) |>
  rowwise() |>
  mutate(
    station_id = str_c(provider, station_id, sep = ":"),
    snap_latitude = st_coordinates(st_transform(snap_geometry, crs = 4326))[2],
    snap_longitude = st_coordinates(st_transform(snap_geometry, crs = 4326))[1]
  ) |>
  select(station_id, latitude = snap_latitude, longitude = snap_longitude) |>
  st_drop_geometry() |>
  write_csv("data/streamstats/stations.csv")
