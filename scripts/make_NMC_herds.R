require(sf)
require(data.table)
require(terra)


### Input data ----
raw <- file.path('data', 'raw')
derived <- file.path('data', 'derived')


bcHerds <- st_read(file.path(raw, 'NMC_herds', '2023_NorthMtnCaribouBC_comb.shp'))
ytHerds <- st_read(file.path(raw, 'NMC_herds', 'YukonCaribouHerds_7May2024', 'YukonCaribouHerds_7May2024.shp'))

plot(ytHerds$geometry)
summary(as.factor(ytHerds$Type))
ytHerdsVect <- vect(ytHerds)
ytNMCherds <- subset(ytHerdsVect, ytHerdsVect$Type == 'Woodland')

bc.proj <-project(vect(bcHerds), ytNMCherds)
nmc <- rbind(ytNMCherds, bc.proj)
# ytNMCherds <- ytHerds |>
#   filter(Type == 'Woodland')

writeVector(nmc, file.path(raw, 'NMC_herds', 'nmc_herds_updated.shp'))


## katie update: merge duplicate herd polygons 
# Read shapefile
nmcherds <- st_read(file.path(raw, 'nmc_herds_updated.shp'))

# 1. Find herds with duplicates (excluding NA)
dupe_herds <- nmcherds %>%
  st_drop_geometry() %>%
  filter(!is.na(HERD)) %>%
  count(HERD) %>%
  filter(n > 1) %>%
  pull(HERD)

# 2. Dissolve only those herds
nmcherds_merged <- nmcherds %>%
  # Split into: duplicate herds to merge, and everything else
  filter(HERD %in% dupe_herds & !is.na(HERD)) %>%
  group_by(HERD) %>%
  summarise(.groups = "drop") %>%
  bind_rows(
    nmcherds %>%
      filter(!(HERD %in% dupe_herds & !is.na(HERD)))  # keep others as-is
  )


# Get centroids and extract coords
nmcherds_centroids <- nmcherds %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  rename(x = X, y = Y) %>%
  bind_cols(nmcherds %>% st_drop_geometry())

# plot the before and after
map1 <- ggplot() +
  geom_sf(data = ytbc.wgs, fill = "gray95", color = "black") +
  geom_sf(data = nmcherds, fill = "lightblue", color = "blue", alpha = 0.6) +
  geom_text(
    data = nmcherds_centroids,
    aes(x = x, y = y, label = HERD),
    size = 3, color = "blue"
  ) +
  coord_sf(xlim = c(-141.01807, -118), ylim = c(55, 68), expand = FALSE) +
  ggtitle("before")

map2 <- ggplot() +
  geom_sf(data = ytbc.wgs, fill = "gray95", color = "black") +
  geom_sf(data = nmcherds_merged, fill = "lightblue", color = "blue", alpha = 0.6) +
  coord_sf(xlim = c(-141.01807, -118), ylim = c(55, 68), expand = FALSE) + ggtitle("after")

map1+map2

st_write(nmcherds_merged, file.path(raw, "nmc_herds_updated_merged.shp"))
