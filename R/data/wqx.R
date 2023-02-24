tar_option_set(packages = c("tidyverse", "janitor", "sf", "units"))

wqx_download <- function (stations, characteristics) {
  cat("ids: ", paste0(stations$station_id, collapse = ","), "\n")
  dataRetrieval::readWQPdata(
    siteid = stations$station_id,
    characteristicName = characteristics
  ) |>
    as_tibble() |>
    mutate(ResultMeasureValue = as.character(ResultMeasureValue))
}

targets_wqx <- list(
  tar_target(wqx_characteristics, c("Temperature", "Temperature, water", "Temperature, water, deg F")),
  tar_target(wqx_inventory, {
    dataRetrieval::whatWQPdata(
      statecode = "US:25",
      sampleMedia = c("water", "Water"),
      characteristicName = wqx_characteristics
    ) |>
      as_tibble()
  }),
  tar_target(wqx_inventory_filtered, {
    wqx_inventory |>
      filter(
        ResolvedMonitoringLocationTypeName %in% c(
          "Lake, Reservoir, Impoundment", "Spring", "Stream"
        ),
        activityCount >= 10
      )
  }),
  tar_target(
    wqx_stn_grouped,
    group_stations(wqx_inventory_filtered$MonitoringLocationIdentifier, max_stations = 100),
    iteration = "group"
  ),
  tar_target(
    wqx_data_raw,
    wqx_download(wqx_stn_grouped, wqx_characteristics),
    pattern = map(wqx_stn_grouped),
    error = "continue"
  ),
  tar_target(wqx_data, {
    wqx_data_raw |>
      filter(
        ActivityTypeCode %in% c("Field Msr/Obs", "Field Msr/Obs-Portable Data Logger", "Not determined", "Sample-Routine"),
        !is.na(ResultMeasureValue),
        ResultMeasureValue != "## (Censored)"
      ) |>
      transmute(
        station_id = MonitoringLocationIdentifier,
        datetime = ymd_hms(paste(ActivityStartDate, coalesce(ActivityStartTime.Time, "12:00:00"), sep = " "), tz = "US/Eastern"),
        temp_c = if_else(
          is.na(ResultMeasure.MeasureUnitCode) | ResultMeasure.MeasureUnitCode == "deg C",
          parse_number(ResultMeasureValue),
          change_units(parse_number(ResultMeasureValue), "degF", "degC")
        ),
        depth_m = case_when(
          ActivityDepthHeightMeasure.MeasureUnitCode %in% c("ft", "feet") ~ change_units(ActivityDepthHeightMeasure.MeasureValue, "ft", "m"),
          TRUE ~ ActivityDepthHeightMeasure.MeasureValue
        )
      ) |>
      filter(temp_c < 35)
  }),
  tar_target(wqx_stn_raw, {
    dataRetrieval::whatWQPsites(siteid = unique(wqx_data$station_id)) |>
      as_tibble()
  }),
  tar_target(wqx_stn, {
    wqx_stn_raw |>
      left_join(
        wqx_inventory_filtered |>
          select(MonitoringLocationIdentifier, ResolvedMonitoringLocationTypeName),
        by = "MonitoringLocationIdentifier"
      ) |>
      transmute(
        source = "WQX",
        provider = OrganizationIdentifier,
        station_id = MonitoringLocationIdentifier,
        name = MonitoringLocationName,
        type = ResolvedMonitoringLocationTypeName,
        type = fct_recode(type, stream = "Stream", lake = "Lake, Reservoir, Impoundment"),
        latitude = as.numeric(LatitudeMeasure),
        longitude = as.numeric(LongitudeMeasure)
      )
  }),
  tar_target(wqx, {
    wqx_stn |>
      left_join(
        nest_by(wqx_data, station_id),
        by = "station_id"
      ) |>
      mutate(
        interval = "discrete",
        .before = everything()
      )
  })
)
