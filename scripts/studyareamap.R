library(ggplot2)
library(sf)
library(viridis)
library(rnaturalearth)
library(dplyr)
library(ggspatial)
library(grid)

# ---------------------------
# 1. Load world + provinces (WGS84)
# ---------------------------
world_wgs84 <- ne_countries(scale = "large", returnclass = "sf")
prov_wgs84  <- ne_states(country = "canada", returnclass = "sf")
canada_wgs84 <- prov_wgs84

# ---------------------------
# 2. Load herd data
# ---------------------------
nmcherds <- st_read("G:/Shared drives/WSC_Yukon/CEtool_component3/demogData/nmc_herds_updated/nmc_herds_updated_merged.shp")
nmcherds$HERD_FINAL <- dplyr::coalesce(nmcherds$HERD, nmcherds$HERD_NAME)

# ---------------------------
# 3. Load towns (WGS84)
# ---------------------------
towns <- st_read("G:/Shared drives/WSC_Yukon/CEtool_component3/data_models_new/Fine_Models/data/raw/towns.shp")

# ---------------------------
# 4. Define main map extent (WGS84 → 3578)
# ---------------------------
main_bbox_wgs84 <- st_as_sfc(
  st_bbox(
    c(xmin = -142.5, xmax = -120, ymin = 55, ymax = 66),
    crs = st_crs(4326)
  )
)

crs_main <- 3578   # Canada Albers Equal Area

world_3578    <- st_transform(world_wgs84,  crs_main)
prov_3578     <- st_transform(prov_wgs84,   crs_main)
nmcherds_3578 <- st_transform(nmcherds,     crs_main)
towns_3578    <- st_transform(towns,        crs_main)

main_bbox_3578 <- st_transform(main_bbox_wgs84, crs_main)
bb <- st_bbox(main_bbox_3578)

# ---------------------------
# 5. Town labels (NOW PROJECTED CORRECTLY)
# ---------------------------
coords_3578 <- st_coordinates(towns_3578)

town_labels <- towns_3578 %>%
  mutate(
    x = coords_3578[,1],
    y = coords_3578[,2],
    
    # Offsets now in METERS (EPSG:3578)
    x = case_when(
      `Town.Name` == "Fort Simpson" ~ x + 0,
      `Town.Name` == "Fort St John" ~ x + 0,
      `Town.Name` == "Fort Nelson"  ~ x + 18000,
      `Town.Name` == "Whitehorse"   ~ x - 14000,
      TRUE ~ x
    ),
    y = case_when(
      `Town.Name` == "Fort Simpson" ~ y - 10000,
      `Town.Name` == "Fort St John" ~ y - 10000,
      `Town.Name` == "Fort Nelson"  ~ y - 10000,
      `Town.Name` == "Whitehorse"   ~ y - 10000,
      TRUE ~ y
    )
  ) %>%
  st_drop_geometry()

# ---------------------------
# Province labels (Yukon, NWT, BC)
# ---------------------------

province_labels_wgs84 <- data.frame(
  name = c("Yukon", "Northwest\nTerritories", "British Columbia"),
  lon  = c(-136.6, -120, -125),
  lat  = c(65.6,   64,  55.8)
)

province_labels_sf <- st_as_sf(
  province_labels_wgs84,
  coords = c("lon", "lat"),
  crs = 4326
) %>%
  st_transform(crs_main)


# ---------------------------
# 6. Build inset map (NO ROTATION)
# ---------------------------
inset_map <- ggplot() +
  geom_sf(data = st_transform(canada_wgs84, crs_main),
          fill = "grey90", color = "white") +
  geom_sf(data = main_bbox_3578,
          fill = "#6A00A8FF",
          color = "#6A00A8FF",
          alpha = 0.4,
          linewidth = 1) +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)
  )

# ---------------------------
# 7. Graticule (true projected grid lines)
# ---------------------------
grat <- st_graticule(
  lon = seq(-180, 180, by = 5),
  lat = seq(  40,  90, by = 5)
) %>%
  st_transform(crs_main)

# ---------------------------
# 8. Final map 
# ---------------------------
ggplot() +
  geom_sf(data = world_3578, fill = "grey95", color = "grey70", linewidth = 0.3) +
  geom_sf(data = prov_3578,  fill = NA,      color = "grey70", linewidth = 0.3) +
  
  # True projected graticules
  geom_sf(data = grat, color = "grey70", linewidth = 0.5, linetype = "dotted") +
  
  geom_sf(data = nmcherds_3578, fill = "#6A00A8FF", color = "white", alpha = 0.2) +
  geom_sf_text(data = nmcherds_3578, aes(label = HERD_FINAL),
               color = "#6A00A8FF", size = 3) +
  geom_sf(data = towns_3578, size = 1, color = "black") +
  geom_text(data = town_labels, aes(x = x, y = y, label = Town.Name), size = 3) +
  
  annotation_scale(location = "bl", width_hint = 0.25, text_cex = 0.7) +
  
  # Smaller inset, above scale bar
  annotation_custom(
    grob = ggplotGrob(inset_map),
    xmin = bb["xmin"],
    xmax = bb["xmin"] + (bb["xmax"] - bb["xmin"]) * 0.26,
    ymin = bb["ymin"] + (bb["ymax"] - bb["ymin"]) * 0.12,
    ymax = bb["ymin"] + (bb["ymax"] - bb["ymin"]) * 0.34
  ) +
  # North arrow
  annotation_north_arrow(
    location = "tl",
    which_north = "true",
    style = north_arrow_fancy_orienteering,
    height = unit(1.2, "cm"),
    width  = unit(1.2, "cm")
  ) +
  # Province/Territory labels
  geom_sf_text(
    data = province_labels_sf,
    aes(label = name),
    color = "grey20",
    size = 5,
    fontface = "bold", alpha = 0.5
  ) +
  coord_sf(
    xlim  = c(bb["xmin"], bb["xmax"]),
    ylim  = c(bb["ymin"], bb["ymax"]),
    expand = FALSE
  ) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.border = element_rect(color = "grey40", fill = NA, linewidth = 0.5))

