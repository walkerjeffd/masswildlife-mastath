source("_targets.R")


# PCA(nhdplushr attr) -----------------------------------------------------------

tar_load(c(nhd_attrs, nhd_sf_flowlines))

tar_load(nhdplusv2_attrs)
nhd_tot <- nhdplusv2_attrs |>
  transmute(
    COMID,
    TOT_BASIN_AREA = log10(pmax(TOT_BASIN_AREA, 0.01)),
    TOT_BASIN_SLOPE,
    TOT_CNPY11_BUFF100,
    TOT_ELEV_MEAN,
    TOT_NID_STORAGE2013 = log10(TOT_NORM_STORAGE2013 + 1),
    TOT_NLCD11_AGRI_TOT,
    TOT_NLCD11_DEV_TOT,
    TOT_NLCD11_FOREST,
    TOT_NLCD11_RIP50_FOREST,
    TOT_NLCD11_WATER,
    TOT_NLCD11_WETLAND
  ) |>
  filter(!is.na(TOT_NLCD11_WATER))
select(nhd_tot, -COMID) |>
  GGally::ggpairs()
nhd_tot_pca <- prcomp(select(nhd_tot, -COMID), scale. = TRUE)
nhd_tot <- bind_cols(nhd_tot, nhd_tot_pca$x)
nhd_tot_pca
summary(nhd_tot_pca)


# PC1: urban/coastal to forest/rural
# PC2: mainstem to headwater
nhd_tot |>
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color = TOT_NLCD11_FOREST), size = 0.5) +
  scale_color_viridis_c() +
  theme_minimal()

# representativeness of stations
# use mlr_nhdv2_4 from 20230622-2.R
nhd_tot |>
  ggplot(aes(PC1, PC2)) +
  geom_point(aes(color = TOT_NLCD11_FOREST), size = 0.5) +
  geom_point(
    data = filter(nhd_tot, COMID %in% mlr_nhdv2_4$comid),
    size = 1, color = "orangered", shape = 21
  ) +
  scale_color_viridis_c() +
  theme_minimal()

# color by param
mlr_nhdv2_4_pca <- mlr_nhdv2_4 |>
  st_drop_geometry() |>
  select(station_id, comid, lom_tidy) |>
  unnest(lom_tidy) |>
  left_join(nhd_tot, by = c("comid" = "COMID")) |>
  nest_by(term) |>
  mutate(
    lm_fit = list(lm(estimate ~ PC1 + PC2 + PC3, data = data)),
    lm_tidy = list(tidy(lm_fit)),
    lm_glance = list(glance(lm_fit)),
    lm_augment = list(augment(lm_fit)),
    plot = list({
      data |>
        ggplot(aes(PC1, PC2)) +
        geom_point(aes(color = estimate)) +
        scale_color_viridis_c() +
        labs(title = term) +
        theme_minimal()
    })
  )

mlr_nhdv2_4_pca$lm_fit |>
  set_names(nm = mlr_nhdv2_4_pca$term) |>
  map(summary)

mlr_nhdv2_4_pca |>
  pull(plot) |>
  wrap_plots()

mlr_nhdv2_4_pca |>
  select(term, lm_augment) |>
  unnest(lm_augment) |>
  ggplot(aes(.fitted, estimate)) +
  geom_abline() +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  facet_wrap(vars(term), scales = "free")
