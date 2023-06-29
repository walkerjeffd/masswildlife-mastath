library(tidyverse)
library(httr)
library(glue)
library(sf)

get_streamstats <- function (latitude, longitude) {
  res <- GET(
    "https://streamstats.usgs.gov/streamstatsservices/watershed.geojson",
    query = list(
      rcode = "MA",
      xlocation = longitude,
      ylocation = latitude,
      crs = 4326,
      includeparameters = TRUE,
      includefeatures = TRUE,
      simplify = TRUE
    )
  )
  body <- content(res, encoding = "UTF-8")
  Sys.sleep(1)
  features <- body$featurecollection[[2]]$feature$features
  if (length(features) == 0) {
    stop("no features")
  }
  body
}

x <- read_csv("data/streamstats/stations.csv", show_col_types = FALSE) |>
  mutate(
    streamstats = pmap(
      list(latitude, longitude),
      possibly(insistently(get_streamstats, rate = rate_delay(5, max_times = 3), quiet = FALSE)),
      .progress = TRUE
    )
  )

write_rds(x, "data/streamstats/streamstats.rds")