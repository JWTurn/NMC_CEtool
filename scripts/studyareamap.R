### --- Study area map ---
# Katie Florko
# started Feb 20, 2026

library(ggplot2)
library(sf)
library(viridis)
library(rnaturalearth)
library(dplyr)

# --- World boundaries in WGS84 ---
world_wgs84 <- ne_countries(scale = "large", returnclass = "sf")

# --- Province/territories boundaries ---
prov_wgs84 <- ne_states(country = "canada", returnclass = "sf")

# --- Get herd data ----
nmcherds <- st_read("G:/Shared drives/WSC_Yukon/CEtool_component3/demogData/nmc_herds_updated/nmc_herds_updated_merged.shp")
nmcherds <- st_transform(nmcherds, crs = st_crs(prov_wgs84))

# Combine HERD and HERD_NAME into one clean label column
nmcherds$HERD_FINAL <- dplyr::coalesce(nmcherds$HERD, nmcherds$HERD_NAME)

# --- Get and prep town data ---
towns <- st_read("G:/Shared drives/WSC_Yukon/CEtool_component3/data_models_new/Fine_Models/data/raw/towns.shp")

# Extract coordinates + apply manual nudges
town_labels <- towns %>%
  mutate(
    x = st_coordinates(.)[,1],
    y = st_coordinates(.)[,2],
    x = case_when(
      `Town.Name` == "Fort Simpson" ~ x + 0,
      `Town.Name` == "Fort St John" ~ x + 0,
      `Town.Name` == "Fort Nelson"  ~ x + 1.8,
      `Town.Name` == "Whitehorse"   ~ x - 1.4,
      TRUE ~ x
    ),
    y = case_when(
      `Town.Name` == "Fort Simpson" ~ y - 0.2,
      `Town.Name` == "Fort St John" ~ y - 0.2,
      `Town.Name` == "Fort Nelson"  ~ y - 0.2,
      `Town.Name` == "Whitehorse"   ~ y - 0.2,
      TRUE ~ y
    )
  ) %>%
  st_drop_geometry()   # <-- critical so labels use your adjusted x/y

# --- Plot! ---
ggplot() +
  geom_sf(data = world_wgs84, fill = "grey95", color = "grey80", size = 0.3) +
  geom_sf(data = prov_wgs84, fill = NA, color = "grey80", size = 0.3) +
  geom_sf(
    data = nmcherds,
    fill = "#6A00A8FF",
    color = "white",
    alpha = 0.2
  ) +
  geom_sf_text(
    data = nmcherds,
    aes(label = HERD_FINAL),
    color = "#6A00A8FF",
    size = 3
  ) +
  geom_sf(data = towns, size = 1, color = "black") +
  geom_text(
    data = town_labels,
    aes(x = x, y = y, label = Town.Name),
    size = 3
  ) +
  coord_sf(
    crs = 4326,
    xlim = c(-142.5, -121),
    ylim = c(56, 66)
  ) +
  theme_minimal() +
  theme(axis.title = element_blank())


