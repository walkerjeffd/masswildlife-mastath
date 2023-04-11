
nwis_download_flow <- function (stations, startDate = "1980-01-01", endDate = "2022-12-31") {
  cat("ids: ", paste0(stations$station_id, sep = ","), "\n")
  dataRetrieval::readNWISdv(
    siteNumbers = stations$station_id,
    parameterCd = "00060",
    startDate = startDate,
    endDate = endDate
  ) |>
    as_tibble()
}

targets_nwis_flow <- list(
  tar_target(nwis_flow_stn_raw, {
    dataRetrieval::whatNWISsites(
      stateCd = "ma",
      parameterCd = "00060",
      hasDataTypeCd = "dv"
    ) |>
      as_tibble()
  }),
  tar_target(nwis_flow_stn, {
    nwis_flow_stn_raw |>
      filter(site_tp_cd %in% c("ST")) |>
      transmute(
        station_id = site_no,
        name = station_nm,
        type = fct_recode(site_tp_cd, stream = "ST"),
        latitude = dec_lat_va,
        longitude = dec_long_va
      )
  }),
  tar_target(nwis_flow_stn_grouped, group_stations(nwis_flow_stn$station_id, max_stations = 10), iteration = "group"),
  tar_target(nwis_flow_data_raw, nwis_download_flow(nwis_flow_stn_grouped), pattern = map(nwis_flow_stn_grouped)),
  tar_target(nwis_flow_data, {
    nwis_flow_data_raw |>
      dataRetrieval::renameNWISColumns() |>
      select(
        station_id = site_no,
        date = Date,
        flow_cfs = Flow,
        flow_flag = Flow_cd
      )
  }),
  tar_target(nwis_flow, {
    nwis_flow_stn |>
      mutate(
        source = "NWIS-dv",
        provider = "USGS",
        .before = everything()
      ) |>
      inner_join(
        nest_by(nwis_flow_data, station_id),
        by = "station_id"
      )
  })
)
