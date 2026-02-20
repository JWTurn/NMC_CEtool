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
nmcherds$HERD_FINAL <- dplyr::coalesce(nmcherds$HERD, nmcherds$HERD_NAME)

ggplot() +
  geom_sf(data = world_wgs84, fill = "grey90", color = "grey80", size = 0.3) +
  geom_sf(data = prov_wgs84, fill = NA, color = "grey80", size = 0.3) +
  geom_sf(data = nmcherds, 
          fill = "#6A00A8FF", color = "white", alpha = 0.3) +
  geom_sf_text(
    data = (nmcherds),
    aes(label = HERD_FINAL),
    color = "#6A00A8FF", 
    size = 3) +
  coord_sf(crs = 4326,
           xlim = c(-142.5, -121),
           ylim = c(56, 66)) +
  theme_minimal() +
  theme(
    axis.title = element_blank())

