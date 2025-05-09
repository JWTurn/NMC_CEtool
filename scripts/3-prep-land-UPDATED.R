### Prepare Landcover data ====
# Julie Turner & Peter Baker
# revamped: April 2025

require('data.table')
require(reproducible)
require('amt')
require('dplyr')
require('tidyr')
require('sf')
require(terra)

lapply(dir('R', '*.R', full.names = TRUE), source)
## Input data ----
raw <- file.path('data','raw')
derived <- file.path('data','derived')
# I like setting a folder shared across projects that share large files so I don't have to download many times
# could use raw if want to keep with this project
canada <- file.path('C:','Users', 'peteb', 'OneDrive', 'Documents', 'BP', 'Yukoncfx', 'reproducibleInputs')


## make study area ----
# area for extraction
sArea <- file.path(derived, 'prepped', 'clean_NMC_20kmBuff.shp')
# over extending to capture all features they may be responding to and account for edge effects
studyArea.buff <- buffer(vect(sArea), 100000)

#plot(studyArea.buff)

# make one continuous study area
studyArea.sf <- st_as_sf(studyArea.buff)
studyArea.ch <- st_convex_hull(studyArea.sf)
#st_write(studyArea.ch, file.path(derived, 'NMC_studyArea_extended.shp'))

studyArea_extendedLandscape <- vect(file.path(derived, 'NMC_studyArea_extended.shp'))

#plot(studyArea_extendedLandscape)

## template rasters ----
# make template rasters at needed resolutions

#30mx30m
rasterToMatch_extendedLandscape30m <- terra::rast(studyArea_extendedLandscape, res = c(30,30))
rasterToMatch_extendedLandscape30m [] <- 1
rasterToMatch_extendedLandscape30m  <- terra::mask(rasterToMatch_extendedLandscape30m, studyArea_extendedLandscape)

#250mx250m
rasterToMatch_extendedLandscape250m <- terra::rast(studyArea_extendedLandscape, res = c(250,250))
rasterToMatch_extendedLandscape250m [] <- 1
rasterToMatch_extendedLandscape250m  <- terra::mask(rasterToMatch_extendedLandscape250m, studyArea_extendedLandscape)

#1kmx1km
rasterToMatch_extendedLandscape1000m <- terra::aggregate(rasterToMatch_extendedLandscape250m, 4)

#5kmx5km
rasterToMatch_extendedLandscape5000m <- terra::aggregate(rasterToMatch_extendedLandscape1000m, 5)

## Harvest ----


harvURL <- "https://opendata.nfis.org/downloads/forest_change/CA_Forest_Harvest_1985-2020.zip"


# update to larger resolutions
harv30m <- reproducible::prepInputs(url = harvURL,
                                    destinationPath = canada,
                                    to = rasterToMatch_extendedLandscape30m,
                                    fun = 'terra::rast',
                                    method = 'near') |>
  Cache()
harv30m[harv30m==0]<-NA
writeRaster(harv30m, file.path(derived, 'nmc_harv30m_1985-2020.tif'))

#harv30m <- rast(file.path(derived, 'nmc_harv30m_1985-2020.tif'))
#plot(harv30m)

harv250m <- reproducible::prepInputs(url = harvURL,
                                     destinationPath = canada,
                                     to = rasterToMatch_extendedLandscape250m,
                                     fun = 'terra::rast',
                                     method = 'average') |>
  Cache()
harv250m[harv250m==0]<-NA
writeRaster(harv250m, file.path(derived, 'nmc_harv250m_1985-2020.tif'))
#harv250m <- rast(file.path(derived, 'nmc_harv250m_1985-2020.tif'))
#plot(harv250m)

harv1000m <- reproducible::prepInputs(url = harvURL,
                                     destinationPath = canada,
                                     to = rasterToMatch_extendedLandscape1000m,
                                     fun = 'terra::rast',
                                     method = 'average') |>
  Cache()
harv1000m[harv1000m==0]<-NA
writeRaster(harv1000m, file.path(derived, 'nmc_harv1000m_1985-2020.tif'))
#harv1000m <- rast(file.path(derived, 'nmc_harv1000m_1985-2020.tif'))
#plot(harv1000m)

harv5000m <- reproducible::prepInputs(url = harvURL,
                                     destinationPath = canada,
                                     to = rasterToMatch_extendedLandscape5000m,
                                     fun = 'terra::rast',
                                     method = 'average') |>
  Cache()
harv5000m[harv5000m==0]<-NA
writeRaster(harv5000m, file.path(derived, 'nmc_harv5000m_1985-2020.tif'))
#harv5000m <- rast(file.path(derived, 'nmc_harv5000m_1985-2020.tif'))
#plot(harv5000m)


##Just checkin'
#harv30m
#cellFromRowCol(harv30m, 7, 10)
#summary(harv30m)

## Calculating average distance to harvest for each year of harvest at the larger resolutions (1000, and 5000)

dist_results <- list()

# Loop over years
for (year in 2012:2020) {
  message("Processing year ", year)
  
  # Binary raster: 1 where harvest occurred that year, NA elsewhere
  harvest_bin <- harv30m == year
  harvest_bin[harvest_bin == 0] <- NA
  
  # Compute Euclidean distance
  dist_rast <- distance(harvest_bin)
  
  # Resample to coarser resolutions
  dist_1000 <- resample(dist_rast, harv1000m, method = "average")
  dist_5000 <- resample(dist_rast, harv5000m, method = "average")
  
  # Save all rasters, even if they contain only NA
  writeRaster(dist_1000, file.path(derived, paste0("harvdist_1000m_", year, ".tif")),
              filetype = "GTiff", overwrite = TRUE)
  writeRaster(dist_5000, file.path(derived, paste0("harvdist_5000m_", year, ".tif")),
              filetype = "GTiff", overwrite = TRUE)
  
  message("  Done: ", year)
}

#harv1000m_2012 <- rast(file.path(derived, 'harvdist_1000m_2012.tif'))
#harv1000m_2015 <- rast(file.path(derived, 'harvdist_1000m_2015.tif'))

#plot(harv1000m_2012)
#plot(harv1000m_2015)

#QC - Making sure they aren't just all NAs
#raster_folder <- "C:/Users/peteb/OneDrive/Documents/BP/Yukoncfx/data/derived"

## List all .tif files
#raster_files <- list.files(raster_folder, pattern = "\\.tif$", full.names = TRUE)

# Check each raster
#for (f in raster_files) {
  #r <- rast(f)
  #if (all(is.na(values(r)))) {
    #message("All NA: ", basename(f))
  #} else {
    #message("Has data: ", basename(f))
  #}
#}



## Making presence/absence rasters of harvest (active) in cell per year for 1000 and 5000 res

for (year in 2012:2020) {
  message("Processing year ", year)
  
  # Binary raster: 1 = harvest that year, NA = no harvest
  harvest_bin <- harv30m == year
  harvest_bin[harvest_bin == 0] <- NA
  
  # Replace NA with 0 for aggregation (presence/absence)
  harvest_pa <- classify(harvest_bin, cbind(NA, 0))  # all NAs become 0
  
  # Aggregate
  pa_1000 <- aggregate(harvest_pa, fact = res(harv1000m) / res(harv30m), fun = max, na.rm = TRUE)
  pa_5000 <- aggregate(harvest_pa, fact = res(harv5000m) / res(harv30m), fun = max, na.rm = TRUE)
  
  # Save rasters
  writeRaster(pa_1000, file.path(derived, paste0("harvpres_1000m_", year, ".tif")),
              filetype = "GTiff", overwrite = TRUE)
  writeRaster(pa_5000, file.path(derived, paste0("harvpres_5000m_", year, ".tif")),
              filetype = "GTiff", overwrite = TRUE)
  
  message("  Done: ", year)
}

#Checkin plots
#harv1000m_2012 <- rast(file.path(derived, 'harvpres_1000m_2012.tif'))
#harv1000m_2015 <- rast(file.path(derived, 'harvpres_1000m_2015.tif'))
#harv1000m_2020 <- rast(file.path(derived, 'harvpres_1000m_2020.tif'))

#plot(harv1000m_2012)
#plot(harv1000m_2015)
#plot(harv1000m_2020)



## Landcover ----

#IF PRE-DOWNLOADING ZIP FILE, SEE BELOW THIS CODE

#landURL <- "https://opendata.nfis.org/downloads/forest_change/CA_forest_VLCE2_2010.zip"

# Source necessary R scripts
source("C:/Users/peteb/NMC_CEtool_PB/R/make_landforest_prop.R")
source("C:/Users/peteb/NMC_CEtool_PB/R/aggregate_landforest.R")

# Define years for which we need data
historicLandYears <- 2012:2022

# Buffer value for spatial operations
buffer <- 750

# List of rasters to match, with window sizes of 250m, 1000m, and 5000m
rtms <- list(
  rasterToMatch_extendedLandscape250m,
  rasterToMatch_extendedLandscape1000m,
  rasterToMatch_extendedLandscape5000m
)

# Name the rasters for easier reference
names(rtms) <- c("window250", "agg1000", "agg5000")

# Define corresponding functions for each raster window size
rtmsFuns <- list(
  window250 = paste0('make_landforest_prop(targetFile = targetFile, trast = rtm, buff = ', buffer, ', where2save = saveName)'),
  agg1000 = 'aggregate_landforest(targetFile = targetFile, trast = rtm, where2save = saveName)',
  agg5000 = 'aggregate_landforest(targetFile = targetFile, trast = rtm, where2save = saveName)'
)

# Convert historicLandYears to named vector for easier reference
names(historicLandYears) <- paste0('year_', historicLandYears)

# Main processing loop for each raster and each historic year
propLand <- Map(rtmname = names(rtms), rtm = rtms, rtmFun = rtmsFuns[names(rtms)],
                function(rtm, rtmname, rtmFun) {
                  
                  Map(nn = names(historicLandYears),ii=as.character(historicLandYears), function(nn,ii){
                    path <- file.path(derived, 'prop_land', rtmname) |> checkPath(create = TRUE)
                    saveName <- file.path(path, paste0('propLand_', ii, '.tif'))
                    reproducible::prepInputs(
                      url = paste0("https://opendata.nfis.org/downloads/forest_change/CA_forest_VLCE2_", ii, ".zip"),
                      targetFile = paste0('CA_forest_VLCE2_',ii, '.tif'),
                      destinationPath = canada, # end preprocess
                      fun = eval(parse(text = rtmFun)),
                      # make_landforest_prop = make_landforest_prop,
                      rtmFun = rtmFun,#, # end process, doing postprocess within fun
                      overwrite = TRUE
                      #where2save = file.path(path, paste0('propLand_', ii, '.tif'))
                    ) |> Cache()
                  })
                }
)#|> Cache()


## Human disturbances -----
# Features to disentangle
## LFs: paved road, unpaved road, other lf
## Polys: quartz, placer, other mines, all other polygons (not including harvest)

#TEST CODE
# Paths to your shapefiles
poly_path <- "path/to/polygon_disturbance.shp"
line_path <- "path/to/linear_features.shp"

# Load vectors
poly_sf <- st_read(poly_path)
line_sf <- st_read(line_path)

# Reproject to match base raster
poly_sf <- st_transform(poly_sf, crs(rasterToMatch_extendedLandscape30m))
line_sf <- st_transform(line_sf, crs(rasterToMatch_extendedLandscape30m))

# Define years, resolutions, categories
years <- 2012:2024
poly_categories <- c("quartz", "placer", "othermines", "otherpoly")
line_categories <- c("paved", "unpaved", "otherlf")

# Reference rasters
r30 <- rasterToMatch_extendedLandscape30m
r1000 <- rasterToMatch_extendedLandscape1000m
r5000 <- rasterToMatch_extendedLandscape5000m


# TODO need distance to at larger resolutions per year for each feature
for (year in 2012:2024) {
  for (cat in c("quartz", "placer", "othermines", "otherpoly", "paved", "unpaved", "otherlf")) {
    message("Processing distance-to for ", cat, " in ", year)
    
    # Filter features
    if (cat %in% c("quartz", "placer", "othermines", "otherpoly")) {
      feat <- poly_sf %>% filter(category == cat, year == !!year)
    } else {
      feat <- line_sf %>% filter(category == cat, year == !!year)
    }
    
    if (nrow(feat) == 0) next
    
    # Rasterize binary layer at 30m
    bin_ras <- terra::rasterize(feat, rasterToMatch_extendedLandscape30m, field = NULL, touches = TRUE)
    bin_ras[!is.na(bin_ras)] <- 1
    
    # Distance
    dist_ras <- terra::distance(bin_ras)
    
    # Resample to coarse resolutions
    dist_1000 <- terra::resample(dist_ras, rasterToMatch_extendedLandscape1000m, method = "average")
    dist_5000 <- terra::resample(dist_ras, rasterToMatch_extendedLandscape5000m, method = "average")
    
    # Save
    writeRaster(dist_1000, file.path(derived, paste0(cat, "_distance_1000m_", year, ".tif")), filetype = "GTiff", overwrite = TRUE)
    writeRaster(dist_5000, file.path(derived, paste0(cat, "_distance_5000m_", year, ".tif")), filetype = "GTiff", overwrite = TRUE)
  }
}



# TODO need presence/absence of each feature per year at large resolution
for (year in 2012:2024) {
  for (cat in c("quartz", "placer", "othermines", "otherpoly", "paved", "unpaved", "otherlf")) {
    message("Processing presence/absence for ", cat, " in ", year)
    
    # Filter features by category and year
    if (cat %in% c("quartz", "placer", "othermines", "otherpoly")) {
      feat <- poly_sf %>% filter(category == cat, year == !!year)
    } else {
      feat <- line_sf %>% filter(category == cat, year == !!year)
    }
    
    if (nrow(feat) == 0) next
    
    # Rasterize to 30m
    bin_ras <- terra::rasterize(feat, rasterToMatch_extendedLandscape30m, field = NULL, touches = TRUE)
    bin_ras[!is.na(bin_ras)] <- 1
    
    # Aggregate to 1000m and 5000m
    pa_1000 <- terra::aggregate(bin_ras, fact = res(rasterToMatch_extendedLandscape1000m) / res(rasterToMatch_extendedLandscape30m), fun = max, na.rm = TRUE)
    pa_5000 <- terra::aggregate(bin_ras, fact = res(rasterToMatch_extendedLandscape5000m) / res(rasterToMatch_extendedLandscape30m), fun = max, na.rm = TRUE)
    
    # Save rasters
    writeRaster(pa_1000, file.path(derived, paste0(cat, "_presence_1000m_", year, ".tif")), filetype = "GTiff", overwrite = TRUE)
    writeRaster(pa_5000, file.path(derived, paste0(cat, "_presence_5000m_", year, ".tif")), filetype = "GTiff", overwrite = TRUE)
  }
}


# TODO need each lf density per year at larger resolutions
for (year in 2012:2024) {
  for (cat in c("paved", "unpaved", "otherlf")) {
    message("Processing presence/absence and density for ", cat, " in ", year)
    
    # Filter features by category and year
    feat <- line_sf %>% filter(category == cat, year == !!year)
    if (nrow(feat) == 0) next
    
    # Rasterize as presence/absence (1 for road, 0 for no road)
    bin_ras <- terra::rasterize(feat, rasterToMatch_extendedLandscape30m, fun = "lengths", background = 0)
    bin_ras[bin_ras > 0] <- 1  # Ensure presence (1) for any road, absence (0) otherwise
    
    # Aggregate the binary presence/absence to the larger resolutions (1000m, 5000m)
    dens1000 <- terra::aggregate(bin_ras, fact = res(rasterToMatch_extendedLandscape1000m) / res(rasterToMatch_extendedLandscape30m), fun = mean, na.rm = TRUE)
    dens5000 <- terra::aggregate(bin_ras, fact = res(rasterToMatch_extendedLandscape5000m) / res(rasterToMatch_extendedLandscape30m), fun = mean, na.rm = TRUE)
    
    # Save the results
    writeRaster(dens1000, file.path(derived, paste0(cat, "_roadPresenceAvg_1000m_", year, ".tif")), filetype = "GTiff", overwrite = TRUE)
    writeRaster(dens5000, file.path(derived, paste0(cat, "_roadPresenceAvg_5000m_", year, ".tif")), filetype = "GTiff", overwrite = TRUE)
  }
}


# pseudo set up density (may be presence/absences?)
roadDensity250m <- reproducible::prepInputs(url = ROADS,
                                            destinationPath = canada, # maybe change to raw
                                            to = rasterToMatch_extendedLandscape250m,
                                            fun = 'terra::vect',
                                            method = 'average') |>
  Cache()
# if that doesn't give average density -- bring in at 30m, then average when aggregating up


## FIRES ----
# TODO this needs to be updated as above, but this is the old code

## FIRES #####
## this has been run already
# combine nbac (which seems to be more error checked) with older fires from NFDB
# TODO replace with URL
fires <- vect(file.path(canada, 'fire_nbac_1972_2023', 'nbac_1972_2023_20240530.shp'))
fires.proj <- project(fires, crs)
fires.old <- vect(file.path(canada, 'NFDB_poly', 'NFDB_poly_20210707.shp'))
fires.old.proj <- project(fires.old, crs)
fires.pre <- subset(fires.old.proj, fires.old.proj$YEAR<1972)
# crop to study area for computational ease
nmc.fires.proj <- crop(fires.proj, ext(vect(sArea.ext)))
nmc.fires.pre <- crop(fires.pre, ext(vect(sArea.ext)))
# union old and new fires
gc()
nmc.fires.all <- union(nmc.fires.pre, nmc.fires.proj)
# clean up the year associated with fire
gc()
d.fires.all <- as.data.frame(nmc.fires.all)
nmc.fires.all$year_burn <- nmc.fires.all$YEAR
nmc.fires.all$REP_YEAR <- as.integer(gsub("/.*$", "", nmc.fires.all$REP_DATE))
# default to year already in DF, but if that's missing, use year from report date
nmc.fires.all$YEAR <- ifelse(!is.na(nmc.fires.all$year_burn), nmc.fires.all$year_burn,
                             nmc.fires.all$REP_YEAR)

# NOTE: there are still some years that = -9999 that only have a decade associated with them
## current plan is to set them to NA in analysis since it's unknown and not too many

writeVector(nmc.fires.all, file.path(raw, 'fires', 'fires_1920_2023.shp'), overwrite = T)

# make rasters from polygonal fires by year
gc()

make_fire_rast(layer = file.path(raw, 'fires', 'fires_1920_2023.shp'),
               rast = land, minyr = minyr,
               where2save = file.path(raw, 'fires'))
