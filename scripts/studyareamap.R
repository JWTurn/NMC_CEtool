### --- Study area map ---
# Katie Florko
# started Feb 20, 2026

library(ggplot2)
library(sf)
library(viridis)
library(rnaturalearth)

# --- World boundaries in WGS84 ---
world_wgs84 <- ne_countries(scale = "large", returnclass = "sf")
# (Natural Earth comes in EPSG:4326 already, so no transform needed)

# --- Province/territories boundaries ---
prov_wgs84 <- ne_states(country = "canada", returnclass = "sf")

# --- Get herd data ----
nmcherds <- st_read("G:/Shared drives/WSC_Yukon/CEtool_component3/demogData/nmc_herds_updated/nmc_herds_updated_merged.shp")
nmcherds <- st_transform(nmcherds, crs = st_crs(prov_wgs84))
nmcherds <- st_cast(nmcherds, "LINESTRING") # convert to linestring so we can adjust alpha
nmcherds_poly <- nmcherds %>% st_cast("POLYGON")


ggplot() +
  geom_sf(data = world_wgs84, fill = "grey90", color = "grey80", size = 0.3) +
  geom_sf(data = prov_wgs84, fill = NA, color = "grey80", size = 0.3) +
  geom_sf(data = nmcherds_poly, 
          aes(fill = Type), 
          color = "white", alpha = 0.6) +
  scale_fill_viridis_d(
    name = "Type",
    #limits = c(0, 1),
    option = "viridis",
    guide =  guide_legend(title.position = "top", direction = "horizontal")
  ) +
  coord_sf(crs = 4326,
           xlim = c(-142.5, -119),
           ylim = c(55, 68)) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    legend.position = c(0, 0),
    legend.justification = c(0, 0)
  )




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