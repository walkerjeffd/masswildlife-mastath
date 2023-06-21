
nwis_download_temp <- function (stations, startDate = "1980-01-01", endDate = "2022-12-31") {
  cat("ids: ", paste0(stations$station_id, sep = ","), "\n")
  dataRetrieval::readNWISuv(
    siteNumbers = stations$station_id,
    parameterCd = "00010",
    startDate = startDate,
    endDate = endDate
  ) |>
    as_tibble()
}

targets_nwis_temp <- list(
  tar_target(nwis_temp_stn_raw, {
    dataRetrieval::whatNWISsites(
      stateCd = "ma",
      parameterCd = "00010",
      hasDataTypeCd = "iv"
    ) |>
      as_tibble()
  }),
  tar_target(nwis_temp_stn, {
    nwis_temp_stn_raw |>
      filter(site_tp_cd %in% c("ST", "LK")) |>
      transmute(
        station_id = site_no,
        name = station_nm,
        type = fct_recode(site_tp_cd, stream = "ST", lake = "LK"),
        latitude = dec_lat_va,
        longitude = dec_long_va
      )
  }),
  tar_target(nwis_temp_stn_grouped, group_stations(nwis_temp_stn$station_id, max_stations = 1), iteration = "group"),
  tar_target(nwis_temp_data_raw, nwis_download_temp(nwis_temp_stn_grouped), pattern = map(nwis_temp_stn_grouped)),
  tar_target(nwis_temp_data, {
    nwis_temp_data_raw |>
      dataRetrieval::renameNWISColumns() |>
      mutate(dateTime = with_tz(dateTime, tzone = "US/Eastern")) |>
      select(
        station_id = site_no,
        datetime = dateTime,
        NA_Wtemp_Inst = Wtemp_Inst,
        TOP_Wtemp_Inst,
        BOTTOM_Wtemp_Inst,
        GATE.HOUSE_Wtemp_Inst
      ) |>
      pivot_longer(-c(station_id, datetime), names_to = "strata", names_pattern = "(.*)_Wtemp_Inst", values_to = "temp_c", values_drop_na = TRUE) |>
      mutate(strata = na_if(strata, "NA")) |>
      relocate(strata, .before = "datetime")
  }),
  tar_target(nwis_temp, {
    nwis_temp_stn |>
      mutate(
        source = "USGS-NWIS",
        provider = "USGS",
        .before = everything()
      ) |>
      left_join(
        nest_by(nwis_temp_data, station_id),
        by = "station_id"
      )
  })
)
