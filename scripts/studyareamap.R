### --- Study area map ---
# Katie Florko
# started Feb 20, 2026

library(ggplot2)
library(sf)
library(viridis)

nmcherds_merged <- st_read("G:/Shared drives/WSC_Yukon/CEtool_component3/demogData/nmc_herds_updated/nmc_herds_updated_merged.shp")
canPoly <- st_read("G:/Shared drives/WSC_Yukon/CEtool_component3/data_models_new/Fine_Models/data/lpr_000b16a_e.shp")
ytbc <- subset(canPoly, canPoly$PREABBR %in% c('B.C.', 'Y.T.', 'N.W.T.'))
ytbc.wgs <- st_transform(ytbc, 4326)


ggplot() +
  geom_sf(data = ytbc.wgs, fill = "gray95", color = "black") +
  geom_sf(data = nmcherds_merged, fill = "lightblue", color = "blue", alpha = 0.6) +
  coord_sf(xlim = c(-141.01807, -118), ylim = c(55, 68), expand = FALSE)




ggplot() +
  geom_sf(data = ytbc.wgs, fill = "gray95", color = "black") +
  geom_sf(data = nmcherds_demo,
          aes(fill = years_with_counts, color = years_with_counts),
          alpha = 0.5) +
  scale_fill_viridis_c(
    option = "plasma",
    na.value = "white",
    guide = guide_colourbar(
      direction = "horizontal",
      order = 1,
      barheight = unit(0.3, "cm")   # slimmer bar
    )
  ) +
  scale_color_viridis_c(
    option = "plasma",
    na.value = "grey50",
    guide = guide_colourbar(
      direction = "horizontal",
      order = 1,
      barheight = unit(0.3, "cm")   # match fill bar height
    )
  ) +
  geom_sf(data = morts_lastfix_sf,
          aes(shape = "Mortality location"),
          color = "black", size = 1, stroke = 1) +
  scale_shape_manual(
    values = c("Mortality location" = 4),
    guide = guide_legend(
      title = NULL,
      order = 2,
      override.aes = list(size = 1, stroke = 1)
    )
  ) +
  coord_sf(xlim = c(-141, -118), ylim = c(55, 68), expand = FALSE) +
  theme_minimal() +
  labs(
    fill = "Years with counts",
    color = "Years with counts",
    title = "Telemetry records of mortality",
    x = NULL, y = NULL
  ) +
  theme(
    legend.position = c(0.98, 0.98),
    legend.justification = c("right", "top"),
    legend.box = "vertical",
    legend.box.just = "right",
    legend.background = element_rect(fill = alpha("white", 0.8), color = NA),
    legend.spacing.y = unit(0.1, "cm")
  )