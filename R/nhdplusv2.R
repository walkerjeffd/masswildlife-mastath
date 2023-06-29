tar_option_set(packages = c("tidyverse", "janitor", "sf", "units", "glue", "patchwork", "arrow"))

targets_nhdplusv2_prism <- list(
  # tar_target(nhdplusv2_prism_1617_file, {})
  tar_target(nhdplusv2_prism_tav7100_file, file.path(nhdplusv2_attrs_dir, "climate", "prism-tav7100", "TAV7100_TOT_CONUS.TXT"), format = "file"),
  tar_target(nhdplusv2_prism_tav7100, {
    read_csv(nhdplusv2_prism_tav7100_file, show_col_types = FALSE) |>
      mutate(COMID = as.integer(COMID)) |>
      semi_join(nhdplusv2_sf_flowlines, by = "COMID") |>
      select(-TOT_NODATA)
  }),
  tar_target(nhdplusv2_prism_tav1617_files, list.files(file.path(nhdplusv2_attrs_dir, "climate", "prism-tav1617"), "*.TXT", full.names = TRUE), format = "file"),
  tar_target(nhdplusv2_prism_tav1617, {
    map_df(nhdplusv2_prism_tav1617_files, function (x) {
      read_csv(x, show_col_types = FALSE) |>
        semi_join(nhdplusv2_sf_flowlines, by = "COMID") |>
        pivot_longer(-c(COMID, ID))
    }) |>
      mutate(COMID = as.integer(COMID)) |>
      pivot_wider() |>
      select(-ID)
  }),
  tar_target(nhdplusv2_prism, {
    bind_rows(
      nhdplusv2_prism_tav7100 |>
        pivot_longer(-COMID, names_to = c("name1", "name2", "month"), names_sep = "_") |>
        unite(name, c("name1", "name2")),
      nhdplusv2_prism_tav1617 |>
        pivot_longer(-COMID, names_to = c("name1", "name2", "month", "year"), names_sep = "_") |>
        unite(name, c("name1", "name2", "year"))
    ) |>
      pivot_wider() |>
      mutate(month = map_dbl(month, ~ which(toupper(month.abb) == .)))
  })
)

targets_nhdplusv2_attrs <- list(
  tar_target(nhdplusv2_attrs_dir, file.path(data_dir, "nhd", "nhdplusv2_attributes")),
  tar_target(nhdplusv2_attrs_bfi_file, file.path(nhdplusv2_attrs_dir, "hydrology", "bfi", "BFI_CONUS.txt"), format = "file"),
  tar_target(nhdplusv2_attrs_bfi, {
    read_csv(nhdplusv2_attrs_bfi_file, show_col_types = FALSE) |>
      mutate(COMID = as.integer(COMID)) |>
      semi_join(nhdplusv2_sf_flowlines, by = "COMID") |>
      select(COMID, ends_with("BFI"))
  }),
  tar_target(nhdplusv2_attrs_nid_file, file.path(nhdplusv2_attrs_dir, "hydrology-modification", "nid", "NID_2013_CONUS.txt"), format = "file"),
  tar_target(nhdplusv2_attrs_nid, {
    read_csv(nhdplusv2_attrs_nid_file, show_col_types = FALSE) |>
      mutate(COMID = as.integer(COMID)) |>
      semi_join(nhdplusv2_sf_flowlines, by = "COMID")
  }),
  tar_target(nhdplusv2_attrs_impv_file, file.path(nhdplusv2_attrs_dir, "land-cover", "impv11", "IMPV11_CONUS.txt"), format = "file"),
  tar_target(nhdplusv2_attrs_impv, {
    read_csv(nhdplusv2_attrs_impv_file, show_col_types = FALSE) |>
      mutate(COMID = as.integer(COMID)) |>
      semi_join(nhdplusv2_sf_flowlines, by = "COMID") |>
      select(COMID, ends_with("IMPV11"))
  }),
  tar_target(nhdplusv2_attrs_cnpy11rip50_file, file.path(nhdplusv2_attrs_dir, "land-cover", "cnpy11-rip50", "CNPY11_BUFF100_CONUS.txt"), format = "file"),
  tar_target(nhdplusv2_attrs_cnpy11rip50, {
    read_csv(nhdplusv2_attrs_cnpy11rip50_file, show_col_types = FALSE) |>
      mutate(COMID = as.integer(COMID)) |>
      semi_join(nhdplusv2_sf_flowlines, by = "COMID") |>
      select(COMID, ends_with("BUFF100"))
  }),
  tar_target(nhdplusv2_attrs_nlcd11_files, list.files(file.path(nhdplusv2_attrs_dir, "land-cover", "nlcd11"), "*.TXT", full.names = TRUE), format = "file"),
  tar_target(nhdplusv2_attrs_nlcd11, {
    # https://www.mrlc.gov/data/legends/national-land-cover-database-2011-nlcd2011-legend
    x <- map_df(nhdplusv2_attrs_nlcd11_files, function (x) {
      read_csv(x, show_col_types = FALSE) |>
        semi_join(nhdplusv2_sf_flowlines, by = "COMID") |>
        select(-ends_with("NODATA")) |>
        pivot_longer(-COMID)
    }) |>
      mutate(COMID = as.integer(COMID))
    x |>
      separate(name, into = c("basin", "dataset", "code")) |>
      pivot_wider(names_from = "code") |>
      mutate(
        WATER = `11` + `12`,
        DEV_OPEN = `21`,
        DEV_LOW = `22`,
        DEV_MED = `23`,
        DEV_HIGH = `24`,
        DEV_TOT = DEV_LOW + DEV_MED + DEV_HIGH,
        BARRON = `31`,
        FOREST = `41` + `42` + `43`,
        SHRUB = `52`,
        GRASS = `71`,
        AGRI_HAY = `81`,
        AGRI_CROP = `82`,
        AGRI_TOT = AGRI_HAY + AGRI_CROP,
        WETLAND = `90` + `95`
      ) |>
      select(COMID, basin, dataset, WATER:WETLAND) |>
      pivot_longer(-c(COMID, basin, dataset)) |>
      unite(name, c(basin, dataset, name)) |>
      pivot_wider()
  }),
  tar_target(nhdplusv2_attrs_nlcd11rip50_files, list.files(file.path(nhdplusv2_attrs_dir, "land-cover", "nlcd11-rip50"), "*.txt", full.names = TRUE), format = "file"),
  tar_target(nhdplusv2_attrs_nlcd11rip50, {
    x <- map_df(nhdplusv2_attrs_nlcd11rip50_files, function (x) {
      read_csv(x, show_col_types = FALSE) |>
        semi_join(nhdplusv2_sf_flowlines, by = "COMID") |>
        select(-ends_with("NODATA")) |>
        pivot_longer(-COMID)
    }) |>
      mutate(COMID = as.integer(COMID))

    x |>
      separate(name, into = c("basin", "dataset", "code", "buf")) |>
      pivot_wider(names_from = "code") |>
      mutate(
        WATER = `11` + `12`,
        DEV_OPEN = `21`,
        DEV_LOW = `22`,
        DEV_MED = `23`,
        DEV_HIGH = `24`,
        DEV_TOT = DEV_LOW + DEV_MED + DEV_HIGH,
        BARRON = `31`,
        FOREST = `41` + `42` + `43`,
        SHRUB = `52`,
        GRASS = `71`,
        AGRI_HAY = `81`,
        AGRI_CROP = `82`,
        AGRI_TOT = AGRI_HAY + AGRI_CROP,
        WETLAND = `90` + `95`
      ) |>
      select(COMID, basin, dataset, buf, WATER:WETLAND) |>
      pivot_longer(-c(COMID, basin, dataset, buf)) |>
      unite(name, c(basin, dataset, buf, name)) |>
      pivot_wider()
  }),
  tar_target(nhdplusv2_attrs_basin_files, list.files(file.path(nhdplusv2_attrs_dir, "topographic", "basin"), "*.TXT", full.names = TRUE), format = "file"),
  tar_target(nhdplusv2_attrs_basin, {
    map_df(nhdplusv2_attrs_basin_files, function (x) {
      read_csv(x, show_col_types = FALSE) |>
        semi_join(nhdplusv2_sf_flowlines, by = "COMID") |>
        pivot_longer(-COMID)
    }) |>
      mutate(COMID = as.integer(COMID)) |>
      pivot_wider()
  }),
  tar_target(nhdplusv2_attrs, {
    nhdplusv2_attrs_bfi |>
      full_join(nhdplusv2_attrs_impv, by = "COMID") |>
      full_join(nhdplusv2_attrs_cnpy11rip50, by = "COMID") |>
      full_join(nhdplusv2_attrs_nlcd11, by = "COMID") |>
      full_join(nhdplusv2_attrs_nlcd11rip50, by = "COMID") |>
      full_join(nhdplusv2_attrs_nid, by = "COMID") |>
      full_join(nhdplusv2_attrs_basin, by = "COMID")
  })
)

targets_nhdplusv2 <- list(
  tar_target(nhdplusv2_root_gis, file.path(data_dir, "../gis", "nhdplusv2")),
  tar_target(nhdplusv2_sf_flowlines, {
    bind_rows(
      st_read(file.path(nhdplusv2_root_gis, "NHDPlusNE", "NHDPlus01", "NHDSnapshot", "Hydrography", "NHDFlowline.shp")),
      st_read(file.path(nhdplusv2_root_gis, "NHDPlusMA", "NHDPlus02", "NHDSnapshot", "Hydrography", "NHDFlowline.shp"))
    ) |>
      st_filter(nhdplusv2_sf_wbd) |>
      filter(FTYPE %in% c("ArtificialPath", "StreamRiver"))
  }),
  tar_target(nhdplusv2_sf_catchments, {
    bind_rows(
      st_read(file.path(nhdplusv2_root_gis, "NHDPlusNE", "NHDPlus01", "NHDPlusCatchment", "Catchment.shp")),
      st_read(file.path(nhdplusv2_root_gis, "NHDPlusMA", "NHDPlus02", "NHDPlusCatchment", "Catchment.shp"))
    ) |>
      st_make_valid() |>
      filter(FEATUREID %in% nhdplusv2_sf_flowlines$COMID)
  }),
  tar_target(nhdplusv2_sf_wbd, {
    x <- bind_rows(
      st_read(file.path(nhdplusv2_root_gis, "NHDPlusNE", "NHDPlus01", "WBDSnapshot", "WBD", "WBD_Subwatershed.shp")),
      st_read(file.path(nhdplusv2_root_gis, "NHDPlusMA", "NHDPlus02", "WBDSnapshot", "WBD", "WBD_Subwatershed.shp"))
    ) |>
      sf::st_make_valid()
    st_filter(x, st_transform(gis_state, crs = st_crs(x)))
  }),
  tar_target(nhdplusv2_sf, {
    list(
      wbd = nhdplusv2_sf_wbd,
      catchments = nhdplusv2_sf_catchments,
      flowlines = nhdplusv2_sf_flowlines
    )
  }),
  tar_target(nhdplusv2_gis_export, {
    base_dir <- "data/nhdplusv2"
    filenames <- c()
    for (x in names(nhdplusv2_sf)) {
      filename <- file.path(base_dir, paste0(x, ".shp"))
      if (file.exists(filename)) {
        unlink(filename)
      }
      st_write(nhdplusv2_sf[[x]], filename)
      filenames <- c(filenames, filename)
    }
    filenames
  }, format = "file"),

  tar_target(nhdplusv2_enhd_nhdplusatts_file, file.path(data_dir, "nhd", "enhd_nhdplusatts", "enhd_nhdplusatts.parquet"), format = "file"),
  tar_target(nhdplusv2_enhd_nhdplusatts, {
    read_parquet(nhdplusv2_enhd_nhdplusatts_file) |>
      filter(comid %in% nhdplusv2_sf_flowlines$COMID) |>
      as_tibble()
  }),
  tar_target(nhdplusv2_vaa, {
    x <- nhdplusv2_enhd_nhdplusatts |>
      select(comid, areasqkm, totdasqkm, streamleve, streamorde, slope, roughness) |>
      mutate(slope = if_else(slope < 0, NA_real_, slope))
    names(x) <- toupper(names(x))
    x
  }),
  tar_target(nhdplusv2_flowline, {
    nhdplusv2_sf_flowlines |>
      select(COMID, GNIS_ID, GNIS_NAME, FTYPE) |>
      left_join(nhdplusv2_attrs, by = "COMID") |>
      left_join(nhdplusv2_vaa, by = "COMID") |>
      st_transform(crs = crs_ma_state_plane) |>
      st_zm()
  }),
  tar_target(nhdplusv2_pca_inp, {
    nhdplusv2_flowline |>
      st_drop_geometry() |>
      as_tibble() |>
      select(
        COMID, TOT_IMPV11, TOT_NID_STORAGE2013, TOT_BASIN_AREA, TOT_BASIN_SLOPE, TOT_ELEV_MEAN, TOT_STREAM_SLOPE
      ) |>
      filter(!is.na(TOT_ELEV_MEAN)) |>
      mutate(
        TOT_STREAM_SLOPE = if_else(TOT_STREAM_SLOPE < 0, NA_real_, TOT_STREAM_SLOPE),
        TOT_STREAM_SLOPE = coalesce(TOT_STREAM_SLOPE, mean(TOT_STREAM_SLOPE, na.rm = TRUE)),
        TOT_BASIN_AREA = log10(pmax(TOT_BASIN_AREA, 0.01)),
        TOT_NID_STORAGE2013 = log10(pmax(TOT_NID_STORAGE2013, 1)),
        TOT_STREAM_SLOPE = log10(pmax(TOT_STREAM_SLOPE, 0.0001))
      ) |>
      select(
        COMID, TOT_IMPV11, TOT_BASIN_AREA, TOT_BASIN_SLOPE, TOT_ELEV_MEAN
      )
  }),
  tar_target(nhdplusv2_pca_inp_hist, {
    nhdplusv2_pca_inp |>
      pivot_longer(-COMID) |>
      ggplot(aes(value)) +
      geom_histogram(bins = 30) +
      facet_wrap(vars(name), scales = "free_x")
  }),
  tar_target(nhdplusv2_pca, {
    pca <- nhdplusv2_pca_inp |>
      select(-COMID) |>
      prcomp(scale. = TRUE)
    data <- bind_cols(
      nhdplusv2_pca_inp,
      pca$x
    )
    list(
      pca = pca,
      data = data
    )
  }),
  tar_target(nhdplusv2_pca_plot, {
    nhdplusv2_pca$data |>
      ggplot(aes(PC1, PC2)) +
      geom_point(aes(color = TOT_BASIN_AREA)) +
      scale_color_viridis_c() +
      theme_minimal()
  }),
  tar_target(nhdplusv2_pca_map, {
    nhdplusv2_flowline |>
      left_join(
        nhdplusv2_pca$data |>
          select(COMID, starts_with("PC")),
        by = "COMID"
      ) |>
      select(COMID, PC1, PC2) |>
      pivot_longer(c(PC1, PC2)) |>
      ggplot() +
      geom_sf(data = select(gis_state, -name), fill = NA) +
      geom_sf(aes(color = value)) +
      scale_color_viridis_c() +
      facet_wrap(vars(name), ncol = 1) +
      theme_void()
  })
)

