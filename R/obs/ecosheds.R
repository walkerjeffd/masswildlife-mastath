tar_option_set(packages = c("tidyverse", "janitor", "units", "sf", "bit64"))

ecosheds_connect <- function () {
  dotenv::load_dot_env()
  DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("ECOSHEDS_DB_HOST"),
    port = Sys.getenv("ECOSHEDS_DB_PORT"),
    dbname = Sys.getenv("ECOSHEDS_DB_DBNAME"),
    user = Sys.getenv("ECOSHEDS_DB_USER"),
    password = Sys.getenv("ECOSHEDS_DB_PASSWORD"),
  )
}

ecosheds_download <- function (stations) {
  con <- ecosheds_connect()

  location_ids <- stations$station_id
  series_sql <- glue::glue_sql("select * from series where location_id in ({location_ids*}) and reviewed and value_count > 10", .con = con)
  series <- DBI::dbGetQuery(con, series_sql)

  values_sql <- glue::glue_sql("select
    	v.series_id,
    	date_trunc('day', v.datetime at time zone 'US/Eastern')::date as date,
    	count(v.value) as n_values,
    	min(v.value) as min_temp_c,
    	avg(v.value) as mean_temp_c,
    	max(v.value) as max_temp_c,
    	bool_or(v.flagged) as flagged,
    	string_agg(distinct v.flags, ';') as flags
    from get_values_flags_series_ids('{<series$id*>}') v
    group by v.series_id, date
    order by v.series_id, date",
    .con = con, .open = "<", .close = ">"
  )
  values <- DBI::dbGetQuery(con, values_sql) |>
    as_tibble() |>
    left_join(
      series |>
        select(series_id = id, series_reviewed = reviewed, ecosheds_id = location_id),
      by = "series_id"
    )

  DBI::dbDisconnect(con)
  values
}

targets_ecosheds <- list(
  tar_target(ecosheds_stn_all, {
    con <- ecosheds_connect()
    sql <- "select a.name as provider, l.id as ecosheds_id, l.name as station_id, l.description as name, latitude, longitude
from locations l
left join agencies a on l.agency_id=a.id;"
    x <- DBI::dbGetQuery(con, sql) |>
      as_tibble()
    DBI::dbDisconnect(con)
    x
  }),
  tar_target(ecosheds_stn_filtered, {
    ecosheds_stn_all |>
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
      mutate(
        longitude = st_coordinates(geometry)[,1],
        latitude = st_coordinates(geometry)[,2]
      ) |>
      st_transform(crs = crs_ma_state_plane) |>
      st_filter(gis_state) |>
      st_drop_geometry()
  }),
  tar_target(ecosheds_stn_grouped, group_stations(ecosheds_stn_filtered$ecosheds_id, max_stations = 10), iteration = "group"),
  # tar_target(ecosheds_data, ecosheds_download(ecosheds_stn_grouped), pattern = map(ecosheds_stn_grouped)),
  # tar_target(ecosheds_data_file, {
  #   fname <- "data/obs/ecosheds.rds"
  #   write_rds(ecosheds_data, fname)
  #   fname
  # }, format = "file"),
  # NOTE: ecosheds data was manually cached to an rds file using the previous
  #       two targets to avoid repeat database pulls
  tar_target(ecosheds_data_file, "data/obs/ecosheds.rds", format = "file"),
  tar_target(ecosheds_data, read_rds(ecosheds_data_file)),
  tar_target(ecosheds_stn, {
    ecosheds_stn_filtered |>
      filter(
        ecosheds_id %in% ecosheds_data$ecosheds_id,
        !str_starts(name, "Air Temp")
      )
  }),
  tar_target(ecosheds, {
    ecosheds_stn |>
      mutate(
        station_id = glue::glue("{station_id} [{ecosheds_id}]"),
        source = "EcoSHEDS", .before = everything()
      ) |>
      left_join(
        nest_by(ecosheds_data, ecosheds_id),
        by = "ecosheds_id"
      ) |>
      select(-ecosheds_id)
  }),
  tar_target(ecosheds_day, {
    ecosheds |>
      rowwise() |>
      mutate(
        data = list({
          data |>
            filter(!flagged)
        })
      ) |>
      filter(nrow(data) > 0) |>
      mutate(
        data = list({
          data |>
            group_by(date) |>
            summarise(
              n_series = n(),
              n_values = as.integer(sum(n_values)),
              min_temp_c = min(min_temp_c),
              mean_temp_c = mean(mean_temp_c),
              max_temp_c = max(max_temp_c)
            )
        })
      )
  })
)