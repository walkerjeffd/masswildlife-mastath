source("_targets.R")

tar_load(gis_state)
gis_dir <- "/Users/jeff/Dropbox/Work/masswildlife/gis/nhdplushr"

download_dir <- nhdplusTools::download_nhdplushr(gis_dir, c("0106", "0107", "0108", "0109", "0110", "0202"))

wbdhu8 <- st_make_valid(nhdplusTools::get_nhdplushr(download_dir, layers = "WBDHU8")$WBDHU8)
wbdhu12 <- st_make_valid(nhdplusTools::get_nhdplushr(download_dir, layers = "WBDHU12")$WBDHU12)
nhdflowline <- nhdplusTools::get_nhdplushr(download_dir, layers = "NHDFlowline")$NHDFlowline

wbdhu8_ma <- st_filter(wbdhu8, st_transform(gis_state, crs = st_crs(wbdhu8)))
wbdhu12_ma <- st_filter(wbdhu12, st_transform(gis_state, crs = st_crs(wbdhu12)))
nhdflowline_ma <- st_filter(nhdflowline, st_transform(gis_state, crs = st_crs(nhdflowline)))

st_simplify(wbdhu8_ma, dTolerance = 100) |>
  ggplot() +
  geom_sf() +
  geom_sf(data = gis_state, fill = NA)

st_write(st_cast(wbdhu8_ma, "MULTIPOLYGON"), file.path(gis_dir, "wbdhu8.shp"))
st_write(st_cast(wbdhu12_ma, "MULTIPOLYGON"), file.path(gis_dir, "wbdhu12.shp"))
st_write(st_cast(nhdflowline_ma, "MULTILINESTRING"), file.path(gis_dir, "nhdflowline.shp"))
