### Updating Yukon disturbance layers ====
# Peter Baker
# Started: April 30 2025

require('data.table')
require('amt')
require('dplyr')
require('tidyr')
require('sf')
require(ggplot2)
require(terra)
require(foreach)


### Input data ----
raw <- file.path('data','raw')
derived <- file.path('data','derived')

roads.yt <- file.path(raw, 'NMC_LinearFeatures_Reclassified_final_20240517', 'NMC_LinearFeatures_Reclassified_final.shp')
polys.yt <- file.path(raw, 'NMC_ArealFeatures_Reclassified_final_20240517', 'NMC_ArealFeatures_Reclassified_final.shp')

upd_mines <- file.path(raw, 'Past_Present_Quartz_Mines', 'Past_Present_Quartz_Mines.shp')

yt.lf <- st_read(roads.yt)
yt.poly <- st_read(polys.yt)
yt.newmines <- st_read(upd_mines)

#Create new attributes
yt.lf$year_start <- NA
yt.lf$year_end <- NA
yt.lf$year_source <- NA
yt.poly$year_start <- NA
yt.poly$year_end <- NA
yt.poly$year_source <- NA

### Starting with updating poly layers from yt.newmines info

#Need to find where points from new mines overlap with poly's from the YT disturb layer
#Ensure crs is the same
if (st_crs(yt.newmines) != st_crs(yt.poly)) {
  yt.poly <- st_transform(yt.poly, crs = st_crs(yt.newmines))
}

# Perform spatial join: which points intersect which polygons
# Each point will be matched to the polygon(s) it intersects
joined <- st_join(yt.poly, yt.newmines, join = st_intersects, left = FALSE)

# `joined` now has attributes from both polygons and intersecting points
joined <- joined %>%
  mutate(year_start = Production) %>%
  mutate(year_end = Producti_1) %>%
  mutate(year_source = as.character(year_source)) %>%
  mutate(year_source = ifelse(is.na(year_source), "Meghan Hornseth", year_source))

attrs_to_transfer <- joined %>%
  st_drop_geometry() %>%
  select(REF_ID, year_start, year_end, year_source)

# Join the year info back into yt.poly
yt.poly <- yt.poly %>%
  left_join(attrs_to_transfer, by = "REF_ID") %>%
  select(-year_end.x, -year_start.x, -year_source.x) %>%
  rename(year_end = year_end.y, year_start = year_start.y, year_source = year_source.y)

#If year end is 0 (currently ongoing), make current calendar year
yt.poly <- yt.poly %>%
  mutate(year_end = ifelse(year_end == 0, year(Sys.Date()), year_end))

#All other information in the disturbance layers is pre-2010. So updating all others with 2009 for year_start, and NA for year_end.
yt.poly <- yt.poly %>%
  mutate(year_start = if_else(is.na(year_start), 2009L, year_start))


### Now to update from other disturbance data recieved via email from Yukon
## The Fire Break (around Whitehorse. built in 2023 and 2024? Not in our mapping (Image dates 2022), but we could likely get a shapefile for this)
  # Need shapefile


## Whistlebend (subdivision in north end of Whitehorse between Alaska Highway and Yukon River)
# Lets see if it's already in the disturb layer Step: 1 find coords
lon <- -135.10611
lat <- 60.77675

# Step 2: Create an sf POINT object
pt <- st_sfc(st_point(c(lon, lat)), crs = 4326)  # WGS 84

# Step 3: Reproject the point to match the shapefile CRS
pt_transformed <- st_transform(pt, st_crs(yt.poly))

# Step 4: Check for intersections
overlapping_polys <- yt.poly[st_intersects(yt.poly, pt_transformed, sparse = FALSE), ]
print(overlapping_polys)
# No overlaps - will create own polygon in QGIS if needed - Not necessary, not near caribou


## Densification of housing in Southern Lakes (Kelsey can probably speak to this better than I can)
  # Need more detail


## Coffee Exploration footprint - started 2007ish, expansion of exploration footprint after 2012, before 2019
lon <- -139.34367
lat <- 62.88583

# Step 2: Create an sf POINT object
pt <- st_sfc(st_point(c(lon, lat)), crs = 4326)  # WGS 84

# Step 3: Reproject the point to match the shapefile CRS
pt_transformed <- st_transform(pt, st_crs(yt.poly))

# Step 4: Check for intersections
overlapping_polys <- yt.poly[st_intersects(yt.poly, pt_transformed, sparse = FALSE), ]

#Take overlapping layer and apply updated dates from email: year_start 2007 year_end 2019
ref_id_to_update <- overlapping_polys$REF_ID

# Step 4: Update year_start and year_end for matching REF_ID
yt.poly <- yt.poly %>%
  mutate(
    year_start = if_else(REF_ID == ref_id_to_update, 2007L, year_start),
    year_end   = if_else(REF_ID == ref_id_to_update, 2019L, year_end),
    year_source = if_else(REF_ID == ref_id_to_update, 'Megan Hornseth', year_source),
  )

## Clear Creek Road built in 2015/2016 - led to a proliferation of mining activity afterwards. 
 #Found coords for the road, let's see if it's in the yt.lf file
lon <- -137.37961
lat <- 63.83136

# Step 2: Create point in WGS 84
pt <- st_sfc(st_point(c(lon, lat)), crs = 4326)
pt_transformed <- st_transform(pt, st_crs(yt.lf))

# Check for nearby geometries within a few meters
nearby <- yt.lf[st_is_within_distance(yt.lf, pt_transformed, dist = 2, sparse = FALSE)[,1], ] #Got it

ref_id_to_update <- nearby$REF_ID

yt.lf <- yt.lf %>%
  mutate(
    year_start = if_else(REF_ID == ref_id_to_update, 2015L, year_start),
    year_source = as.character(year_source),
    year_source = if_else(REF_ID == ref_id_to_update, 'Megan Hornseth', year_source),
  )

yt.lf <- yt.lf %>%
  mutate(year_start = if_else(is.na(year_start), 2009L, year_start))



### Save updated files
st_write(yt.poly, file.path(raw, 'NMC_ArealFeatures_Reclassified_final_20240517', 'NMC_ArealFeatures_Reclassified_final.shp'), append = FALSE)
st_write(yt.lf, file.path(raw, 'NMC_LinearFeatures_Reclassified_final_20240517', 'NMC_LinearFeatures_Reclassified_final.shp'), append = FALSE)


