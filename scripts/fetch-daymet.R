library(tidyverse)
library(targets)
library(sf)
library(terra)
library(glue)

tar_load(gis_state)

tile_outlines <- daymetr::tile_outlines |>
    st_filter(gis_state)

ggplot(tile_outlines) +
  geom_sf(data = gis_state) +
  geom_sf(fill = NA) +
  theme_void()

daymet_dir <- "data/daymet"
daymet_dir_nc <- file.path(daymet_dir, "nc")

params <- c("tmin", "tmax", "prcp")
start <- 1994
end <- 2022

# download netcdf tiles
for (param in params) {
  daymetr::download_daymet_tiles(
    tiles = daymet_tile_outlines$TileID,
    start = start,
    end = end,
    param = param,
    path = daymet_dir_nc
  )
  Sys.sleep(5)
}

# convert netcdf to tif
daymetr::nc2tif(path = daymet_dir_nc, overwrite = FALSE)

# merge tiles and save to tif
walk(params, function (param) {
  x <- map(start:end, function (yr) {
    rs <- map(list.files(daymet_dir_nc, glue("{param}_{yr}_[0-9]+.tif$"), full.names = TRUE), rast)
    rc <- sprc(rs)
    x <- merge(rc)
    names(x) <- str_replace_all(names(x), glue("{param}_"), glue("{param}_{yr}_"))
    x
  })
  y <- do.call(c, x)
  terra::writeRaster(y, file.path(daymet_dir, glue("{param}.tif")), overwrite = TRUE)
})
