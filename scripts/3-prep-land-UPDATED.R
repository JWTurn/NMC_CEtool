### Prepare Landcover data ====
# Julie Turner & Peter Baker
# revamped: April 2025
# revamped: Sept 2025 (florko)

require('data.table')
require(reproducible)
require('amt')
require('dplyr')
require('tidyr')
require('sf')
require(terra)
require('elevatr')

lapply(dir('R', '*.R', full.names = TRUE), source)
## Input data ----
raw <- file.path('data','raw')
derived <- file.path('data','derived')
canada <- file.path('C:','Users', 'peteb', 'OneDrive', 'Documents', 'BP', 'Yukoncfx', 'reproducibleInputs')
roads.yt <- file.path(raw, 'NMC_LinearFeatures_Reclassified_final_20240517', 'NMC_LinearFeatures_Reclassified_final.shp')
polys.yt <- file.path(raw, 'NMC_ArealFeatures_Reclassified_final_20240517', 'NMC_ArealFeatures_Reclassified_final.shp')


## make study area ----
# area for extraction
sArea <- file.path(derived, 'prepped', 'clean_NMC_20kmBuff.shp')

# over extending to capture all features they may be responding to and account for edge effects
studyArea.buff <- buffer(vect(sArea), 100000)
crs <- st_crs(3154)$wkt
canPoly <- st_read(file.path(canada, 'CanadaPoly', 'lpr_000b16a_e.shp'))

# really using YT, BC, and NWT because of transboundary herds
ytbc <- subset(canPoly, canPoly$PREABBR %in% c('B.C.', 'Y.T.', 'N.W.T.'))
ytbc.wgs <- st_transform(ytbc, crs)

bcnt <- vect(subset(ytbc.wgs, ytbc.wgs$PREABBR %in% c('B.C.', 'N.W.T.')))
nt <- vect(subset(ytbc.wgs, ytbc.wgs$PREABBR %in% c('N.W.T.')))
#plot(studyArea.buff)

# make one continuous study area
studyArea.sf <- st_as_sf(studyArea.buff)
studyArea.ch <- st_convex_hull(studyArea.sf)
#st_write(studyArea.ch, file.path(derived, 'NMC_studyArea_extended.shp'))

studyArea_extendedLandscape <- vect(file.path(derived, 'NMC_studyArea_extended.shp'))

#plot(studyArea_extendedLandscape)

## Template rasters ----
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
                                     method = 'max') |>
  Cache()
harv250m[harv250m==0]<-NA
writeRaster(harv250m, file.path(derived, 'nmc_harv250m_1985-2020.tif'), overwrite = TRUE)
#harv250m <- rast(file.path(derived, 'nmc_harv250m_1985-2020.tif'))
#plot(harv250m)

harv1000m <- reproducible::prepInputs(url = harvURL,
                                     destinationPath = canada,
                                     to = rasterToMatch_extendedLandscape1000m,
                                     fun = 'terra::rast',
                                     method = 'max') |>
  Cache()
harv1000m[harv1000m==0]<-NA
writeRaster(harv1000m, file.path(derived, 'nmc_harv1000m_1985-2020.tif'), overwrite = TRUE)
#harv1000m <- rast(file.path(derived, 'nmc_harv1000m_1985-2020.tif'))
#plot(harv1000m)

harv5000m <- reproducible::prepInputs(url = harvURL,
                                     destinationPath = canada,
                                     to = rasterToMatch_extendedLandscape5000m,
                                     fun = 'terra::rast',
                                     method = 'max') |>
  Cache()
harv5000m[harv5000m==0]<-NA
writeRaster(harv5000m, file.path(derived, 'nmc_harv5000m_1985-2020.tif'), overwrite = TRUE)
#harv5000m <- rast(file.path(derived, 'nmc_harv5000m_1985-2020.tif'))
#plot(harv5000m)

#hist(harv30m, breaks = 50, main = "Harvest Year Distribution")
##Just checkin'
#harv30m
#summary(harv30m)


## Base Harv rasters with year of harv ----
#Generating base rasters with year of harvest for each resolution and year (cumulative rasters to update cells if harvested again in cell)
#For ts_harv

years <- 2012:2020

# Each raster stores the year of harvest in each cell (0 = no harvest, will be set to NA)
resolutions <- list(
  "harv250" = harv250m,     # 250m resolution raster
  "harv1000" = harv1000m,   # 1000m resolution raster
  "harv5000" = harv5000m    # 5000m resolution raster
)

# Loop through each resolution
for (res_name in names(resolutions)) {
  
  original_rast <- resolutions[[res_name]]
 
  out_dir <- file.path(derived, res_name)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Loop through each target year
  for (yr in years) {
    message("Processing ", res_name, " for year ", yr)
    
    # Keep only harvests that occurred up to and including the current year
    # All cells harvested after 'yr' will be masked out (set to NA)
    masked_rast <- mask(original_rast, original_rast <= yr, maskvalue = FALSE)
    
    out_path <- file.path(out_dir, paste0(res_name, "_", yr, ".tif"))
    writeRaster(masked_rast, out_path, overwrite = TRUE)
  }
}

## Dist to harv ----
## Calculating average distance to harvest (in current year or year before) for each year and resolutions (250, 1000, and 5000)

dist_results <- list()

# Define output folders for each resolution
dir_250 <- file.path(derived, "harv_dist_250m")
dir_1000 <- file.path(derived, "harv_dist_1000m")
dir_5000 <- file.path(derived, "harv_dist_5000m")

# Create directories if they don't exist
dir.create(dir_250, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_1000, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_5000, recursive = TRUE, showWarnings = FALSE)

# Loop over years
for (year in 2012:2020) {
  message("Processing year ", year)
  
  # Binary raster: 1 where harvest occurred this year or the year before, NA elsewhere
  harvest_bin <- (harv30m == year | harv30m == (year - 1))
  harvest_bin[harvest_bin == 0] <- NA
  
  # Compute Euclidean distance
  dist_rast <- distance(harvest_bin)
  
  # Resample to coarser resolutions
  dist_250 <- resample(dist_rast, harv250m, method = "average")
  dist_1000 <- resample(dist_rast, harv1000m, method = "average")
  dist_5000 <- resample(dist_rast, harv5000m, method = "average")
  
  # Save rasters into their corresponding folders
  writeRaster(dist_250, file.path(dir_250, paste0("harvdist_250m_", year, ".tif")),
              filetype = "GTiff", overwrite = TRUE)
  writeRaster(dist_1000, file.path(dir_1000, paste0("harvdist_1000m_", year, ".tif")),
              filetype = "GTiff", overwrite = TRUE)
  writeRaster(dist_5000, file.path(dir_5000, paste0("harvdist_5000m_", year, ".tif")),
              filetype = "GTiff", overwrite = TRUE)
  
  message("  Done: ", year)
}

#QC
#dist250 <- rast(file.path(derived, "harvdist_250m_2015.tif"))
#dist1000 <- rast(file.path(derived, "harvdist_1000m_2015.tif"))
#dist5000 <- rast(file.path(derived, "harvdist_5000m_2015.tif"))

#plot(dist250, main = "Distance to Harvest (250m, 2015)")
#plot(dist1000, main = "Distance to Harvest (1000m, 2015)")
#plot(dist5000, main = "Distance to Harvest (5000m, 2015)")

#global(dist250, fun = c("min", "max", "mean", "sd"), na.rm = TRUE)
#global(dist1000, fun = c("min", "max", "mean", "sd"), na.rm = TRUE)
#global(dist5000, fun = c("min", "max", "mean", "sd"), na.rm = TRUE)


## P/A harv ----
# Making presence/absence rasters of harvest in recent two years for each year and res

# Define output folders for each resolution
dir_250 <- file.path(derived, "harv_pa_250m")
dir_1000 <- file.path(derived, "harv_pa_1000m")
dir_5000 <- file.path(derived, "harv_pa_5000m")

# Create directories if they don't exist
dir.create(dir_250, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_1000, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_5000, recursive = TRUE, showWarnings = FALSE)

for (year in 2012:2020) {
  message("Processing presence/absence for harvest in year ", year)
  
  # Define output files
  out_file_250m <- file.path(derived, "harv_pa_250m", paste0("harvpres_250m_", year, ".tif"))
  out_file_1000m <- file.path(derived, "harv_pa_1000m", paste0("harvpres_1000m_", year, ".tif"))
  out_file_5000m <- file.path(derived, "harv_pa_5000m", paste0("harvpres_5000m_", year, ".tif"))
  
  # Create binary raster: 1 = harvest this year or last year, 0 = no harvest
  harvest_bin <- harv30m == year | harv30m == (year - 1)
  harvest_bin[!is.na(harvest_bin)] <- 1
  harvest_bin[is.na(harvest_bin)] <- 0
  
  # Resample to coarser resolutions using max (presence in any pixel = 1)
  message("  Resampling to 250m...")
  pa_250 <- resample(harvest_bin, harv250m, method = "max")
  writeRaster(pa_250, out_file_250m, filetype = "GTiff", overwrite = TRUE)
  
  message("  Resampling to 1000m...")
  pa_1000 <- resample(harvest_bin, harv1000m, method = "max")
  writeRaster(pa_1000, out_file_1000m, filetype = "GTiff", overwrite = TRUE)
  
  message("  Resampling to 5000m...")
  pa_5000 <- resample(harvest_bin, harv5000m, method = "max")
  writeRaster(pa_5000, out_file_5000m, filetype = "GTiff", overwrite = TRUE)
  
  message("  Done with year ", year)
}


#QC
#harv1000m_2012 <- rast(file.path(derived, 'harvpres_1000m_2012.tif'))
#harv5000m_2015 <- rast(file.path(derived, 'harvpres_5000m_2015.tif'))
#harv250m_2020 <- rast(file.path(derived, 'harvpres_250m_2020.tif'))

#plot(harv1000m_2012)
#plot(harv5000m_2015)
#plot(harv250m_2020)

#unique(values(harv1000m_2012))
#unique(values(harv5000m_2015))
#unique(values(harv250m_2020))



#---- Density of harv ----
# Define output folders for each resolution
dir_250 <- file.path(derived, "harv_dens_250m")
dir_1000 <- file.path(derived, "harv_dens_1000m")
dir_5000 <- file.path(derived, "harv_dens_5000m")

# Create directories if they don't exist
dir.create(dir_250, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_1000, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_5000, recursive = TRUE, showWarnings = FALSE)


# Making presence/absence rasters of harvest in recent two years for each year and res

for (year in 2012:2020) {
  message("Processing density for harvest in year ", year)
  
  # Define output files
  out_file_250m <- file.path(derived, "harv_dens_250m", paste0("harvdens_250m_", year, ".tif"))
  out_file_1000m <- file.path(derived, "harv_dens_1000m", paste0("harvdens_1000m_", year, ".tif"))
  out_file_5000m <- file.path(derived, "harv_dens_5000m", paste0("harvdens_5000m_", year, ".tif"))
  
  # Create binary raster: 1 = harvest this year or last year, 0 = no harvest
  harvest_bin <- harv30m == year | harv30m == (year - 1)
  harvest_bin[!is.na(harvest_bin)] <- 1
  harvest_bin[is.na(harvest_bin)] <- 0
  
  # Resample to coarser resolutions using average
  message("  Resampling to 250m...")
  dens_250 <- resample(harvest_bin, harv250m, method = "average")
  writeRaster(dens_250, out_file_250m, filetype = "GTiff", overwrite = TRUE)
  
  message("  Resampling to 1000m...")
  dens_1000 <- resample(harvest_bin, harv1000m, method = "average")
  writeRaster(dens_1000, out_file_1000m, filetype = "GTiff", overwrite = TRUE)
  
  message("  Resampling to 5000m...")
  dens_5000 <- resample(harvest_bin, harv5000m, method = "average")
  writeRaster(dens_5000, out_file_5000m, filetype = "GTiff", overwrite = TRUE)
  
  message("  Done with year ", year)
}

## Landcover ----
#landURL <- "https://opendata.nfis.org/downloads/forest_change/CA_forest_VLCE2_2010.zip"

# Define years for which we need data
historicLandYears <- 2012:2022

# Buffer value for spatial operations
buffer <- 500

# List of rasters to match, with window sizes of 250m, 1000m, and 5000m
rtms <- list(
  rasterToMatch_extendedLandscape250m,
  rasterToMatch_extendedLandscape1000m,
  rasterToMatch_extendedLandscape5000m
)

names(rtms) <- c("window250", "agg1000", "agg5000")


# Define corresponding functions for each raster window size with dummy code to make each unique for Cache()
#250 is the moving window to account for the squares around them
rtmsFuns <- list(
  window250 = paste0(
    'make_landforest_prop(targetFile = targetFile, trast = rtm, buff = ', buffer,
    ', where2save = saveName); dummy_tag <- "window250"'
  ),
  agg1000 = 'aggregate_landforest(targetFile = targetFile, trast = rtm, where2save = saveName); dummy_tag <- "agg1000"',
  agg5000 = 'aggregate_landforest(targetFile = targetFile, trast = rtm, where2save = saveName); dummy_tag <- "agg5000"'
)


names(historicLandYears) <- paste0('year_', historicLandYears)

#Main processing loop for each raster resolution and each year
propLand <- Map(rtmname = names(rtms), rtm = rtms, rtmFun = rtmsFuns[names(rtms)],
                function(rtm, rtmname, rtmFun) {
                  
                  Map(nn = names(historicLandYears), ii = as.character(historicLandYears), function(nn, ii) {
                    path <- file.path(derived, 'prop_land', rtmname) |> checkPath(create = TRUE)
                    saveName <- file.path(path, paste0('propLand_', ii, '.tif'))
                    
                    if (file.exists(saveName)) {
                      message("[✔] Skipping ", rtmname, " year ", ii, " — already exists at: ", saveName)
                      return(NULL)
                    }
                    
                    message("[→] Processing ", rtmname, " for year ", ii)
                    
                    reproducible::prepInputs(
                      url = paste0("https://opendata.nfis.org/downloads/forest_change/CA_forest_VLCE2_", ii, ".zip"),
                      targetFile = paste0("CA_forest_VLCE2_", ii, ".tif"),
                      destinationPath = canada,
                      fun = eval(parse(text = rtmFun)),
                      overwrite = TRUE
                    ) |> Cache()
                  })
                })


#Extract-out diff raster layers X year into their own files
input_dir <- file.path(derived, "prop_land")
output_dir <- file.path(derived, "prop_land", "prop_land_split")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

years <- 2012:2022
res_map <- list(window250 = "250", agg1000 = "1000", agg5000 = "5000")

for (res in names(res_map)) {
  res_suffix <- res_map[[res]]
  
  for (yr in years) {
    
    input_file <- file.path(input_dir, res, paste0("propLand_", yr, ".tif"))
    
    if (!file.exists(input_file)) {
      message("[!] Skipping missing file: ", input_file)
      next
    }
    
    rstack <- rast(input_file)
    layer_names <- names(rstack)
    
    for (i in seq_along(layer_names)) {
      layer <- rstack[[i]]
      layer_name <- layer_names[i]
      
      output_file <- file.path(output_dir, paste0(layer_name, "_", res_suffix, "_", yr, ".tif"))
      
      writeRaster(layer, filename = output_file, overwrite = TRUE)
      message("[✔] Saved: ", output_file)
    }
  }
}



## Human disturbances -----

# Features to disentangle
## LFs: paved road, unpaved road, other lf
## Polys: quartz, placer, other mines, all other polygons (not including harvest)


## BC ----
#Upload new BC data - shapefiles manually extracted from the FGDB file provided by Castlys using QGIS, saved in a folder in 'raw' called 'BC_disturb'

bc.roads <- st_read(file.path(raw, 'BC_disturb', '20250501_finallayers__transportation_roads.gpkg'))
bc.rail <- st_read(file.path(raw, 'BC_disturb', '20250501_finallayers__transportation_rail.gpkg'))
bc.tl <- st_read(file.path(raw, 'BC_disturb', '20250501_finallayers__utilities_corridor.gpkg'))
bc.pipeline <- st_read(file.path(raw, 'BC_disturb', '20250501_finallayers__oilgas_pipelines.gpkg'))
bc.seismic <- st_read(file.path(raw, 'BC_disturb', '20250501_finallayers__oilgas_seismic.gpkg'))
bc.trail <- st_read(file.path(raw, 'BC_disturb', '20250501_finallayers__transportation_trails.gpkg'))
bc.dam <- st_read(file.path(raw, 'BC_disturb', '20250501_finallayers__urbanrural_dams.gpkg'))

#As per the metadata, paved roads are those where Industry = Transportation or Urban/Rural
bc.paved <- bc.roads %>%
  filter(Industry %in% c("Transportation", "Urban/Rural"))


bc.unpaved <- bc.roads %>%
  filter(!Industry %in% c("Transportation", "Urban/Rural"))


bc.paved.proj <- project(vect(bc.paved), crs)
bc.unpaved.proj <- project(vect(bc.unpaved), crs)
bc.rail.proj <- project(vect(bc.rail), crs)


bc.tl.proj <- project(vect(bc.tl), crs)
bc.pipeline.proj <- project(vect(bc.pipeline), crs)
bc.seismic.proj <- project(vect(bc.seismic), crs)
bc.dam.proj <- project(vect(bc.dam), crs)
bc.trail.proj <- project(vect(bc.trail), crs)

bc.paved.rail <- union(bc.paved.proj, bc.rail.proj)

bc.other <- union(union(bc.tl.proj, bc.pipeline.proj), bc.trail.proj)
bc.other <- union(bc.other, bc.dam.proj)
bc.other <- union(bc.other, bc.seismic.proj)

#Mines (poly's)
bc.mineral <- st_read(file.path(raw, 'BC_disturb', '20250501_finallayers__mining_mining.gpkg'))
bc.placer <- st_read(file.path(raw, 'BC_disturb', '20250501_finallayers__placermining_placermining.gpkg'))
bc.minesother <- st_read(file.path(raw, 'BC_disturb', '20250501_finallayers__transportation_sandgravel.gpkg'))

#Correcting names
names(bc.mineral) <- names(bc.placer)
names(bc.minesother)[names(bc.minesother) == "OBJECTID_1"] <- "OBJECTID"
names(bc.minesother)[names(bc.minesother) == "Capture_Date"] <- "CaptureDate"
common_cols <- intersect(names(bc.minesother), names(bc.placer))
common_cols #Now we're matching
bc.minesother <- bc.minesother[, common_cols]
names(bc.minesother) <- names(bc.mineral)

unique(st_geometry_type(bc.mineral)) #Invalid geometries in here preventing the reprojection
bc.mineral <- st_cast(bc.mineral, "MULTIPOLYGON") #Making them all MULTIPOLYGON

unique(st_geometry_type(bc.placer))
bc.placer <- st_cast(bc.placer, "MULTIPOLYGON")

unique(st_geometry_type(bc.minesother))
bc.minesother <- st_cast(bc.minesother, "MULTIPOLYGON")

bc.mineral.proj <- project(vect(bc.mineral), crs)
bc.placer.proj <- project(vect(bc.placer), crs)
bc.minesother.proj <- project(vect(bc.minesother), crs)

bc.mineral.proj$feature_type <- "othermines" 
bc.placer.proj$feature_type <- "placer"
bc.minesother.proj$feature_type <- "othermines"

bc.mines <- union(bc.mineral.proj, bc.placer.proj)
bc.mines <- union(bc.mines, bc.minesother.proj)

head(bc.mines)
table(bc.mines$feature_type)

#Adding year column using Work_Date_First column, if not available, use Work_Date_Date if >2012 (other ones are done later)
df <- as.data.frame(bc.mines)

df$year_1 <- as.numeric(format(as.Date(df$Work_Date_First, format = "%Y-%m-%d"), "%Y"))
df$year_2 <- as.numeric(format(as.Date(df$Work_Date_First_1, format = "%Y-%m-%d"), "%Y"))
df$year_3 <- as.numeric(format(as.Date(df$Work_Date_First_2, format = "%Y-%m-%d"), "%Y"))

df$year <- dplyr::coalesce(df$year_1, df$year_2, df$year_3)

last_1 <- as.numeric(format(as.Date(df$Work_Date_Last, format = "%Y-%m-%d"), "%Y"))
last_2 <- as.numeric(format(as.Date(df$Work_Date_Last_1, format = "%Y-%m-%d"), "%Y"))
last_3 <- as.numeric(format(as.Date(df$Work_Date_Last_2, format = "%Y-%m-%d"), "%Y"))

fill_2012 <- is.na(df$year) & (
  (!is.na(last_1) & last_1 <= 2012) |
    (!is.na(last_2) & last_2 <= 2012) |
    (!is.na(last_3) & last_3 <= 2012)
)

df$year[fill_2012] <- 2012

df$year[!is.na(df$year) & df$year < 2012] <- 2012

df <- df[, !(names(df) %in% c("year_1", "year_2", "year_3"))]

values(bc.mines) <- df

table(bc.mines$year)

#Poly's that aren't mines (or harvest)
bc.ag <- st_read(file.path(raw, 'BC_disturb', '20250501_finallayers__agriculture_agriculture.gpkg'))
bc.ogbuild <- st_read(file.path(raw, 'BC_disturb', '20250501_finallayers__oilgas_building.gpkg'))
bc.ogwells <- st_read(file.path(raw, 'BC_disturb', '20250501_finallayers__oilgas_wellsites.gpkg'))
bc.airstrip <- st_read(file.path(raw, 'BC_disturb', '20250501_finallayers__transportation_airstrip.gpkg'))
bc.urbuild <- st_read(file.path(raw, 'BC_disturb', '20250501_finallayers__urbanrural_buildings.gpkg'))
bc.urlandfill <- st_read(file.path(raw, 'BC_disturb', '20250501_finallayers__urbanrural_landfills.gpkg'))
bc.urrec <- st_read(file.path(raw, 'BC_disturb', '20250501_finallayers__urbanrural_recreation.gpkg'))
bc.urresidence <- st_read(file.path(raw, 'BC_disturb', '20250501_finallayers__urbanrural_residential.gpkg'))

unique(st_geometry_type(bc.ogbuild)) #Invalid geometries in here preventing the reprojection
bc.ogbuild <- st_cast(bc.ogbuild, "MULTIPOLYGON") #Making them all MULTIPOLYGON

unique(st_geometry_type(bc.ogwells)) #Invalid geometries in here preventing the reprojection
bc.ogwells <- st_cast(bc.ogwells, "MULTIPOLYGON") #Making them all MULTIPOLYGON

bc.ag.proj <- project(vect(bc.ag), crs)
bc.ogbuild.proj <- project(vect(bc.ogbuild), crs)
bc.ogwells.proj <- project(vect(bc.ogwells), crs)
bc.airstrip.proj <- project(vect(bc.airstrip), crs)
bc.urbuild.proj <- project(vect(bc.urbuild), crs)
bc.urlandfill.proj <- project(vect(bc.urlandfill), crs)
bc.urrec.proj <- project(vect(bc.urrec), crs)
bc.urresidence.proj <- project(vect(bc.urresidence), crs)

#Combining all otherpolys
bc.union <- rbind(bc.ag.proj, bc.ogbuild.proj)
bc.union <- rbind(bc.union, bc.ogwells.proj)
bc.union <- rbind(bc.union, bc.airstrip.proj)
bc.union <- rbind(bc.union, bc.urbuild.proj)
bc.union <- rbind(bc.union, bc.urlandfill.proj)
bc.union <- rbind(bc.union, bc.urrec.proj)
bc.union <- rbind(bc.union, bc.urresidence.proj)

bc.polys <- bc.union

## Yukon -----
#Loading old data first
yt.lf <- st_read(roads.yt)
yt.lf.ls <- summary(as.factor(yt.lf$TYPE_DI))
yt.lf.ls
yt.poly <- st_read(polys.yt)
yt.poly.ls <- list(unique(yt.poly$RC_Dstr))
summary(as.factor(yt.poly$RC_Dstr))
yt.poly.ls

paved.yt <- project(vect(dplyr::filter(yt.lf, RECLS_D %in% c('Highway', 'Railway'))), crs)
unpaved.yt <- project(vect(dplyr::filter(yt.lf, LF_TYPE == 'Unpaved Road')), crs)
other.yt <- project(vect(dplyr::filter(yt.lf, LF_TYPE == 'Other LF' & RECLS_D != 'Railway')), crs)
miningpolys.yt <- project(vect(dplyr::filter(yt.poly, RC_Dstr %in% c('Quartz Mining', 'Placer Mining'))), crs)
miningpolys.yt$feature_type <- ifelse(miningpolys.yt$RC_Dstr == "Placer Mining", "placer", "quartz")
yt.otherpolys <- project(vect(dplyr::filter(yt.poly, !(RC_Dstr %in% c('Quartz Mining', 'Placer Mining',
                                                                      'Harvest')))), crs)
#Now cropping old ones to exclude updated areas
mask.poly <- st_read(file.path(raw, 'YT_disturb', 'new_disturb_outline3.shp'))
mask.poly <- st_transform(mask.poly, crs)
mask.poly.vect <- vect(mask.poly)

paved.yt_cropped   <- erase(paved.yt, mask.poly.vect)
unpaved.yt_cropped <- erase(unpaved.yt, mask.poly.vect)
other.yt_cropped   <- erase(other.yt, mask.poly.vect)
miningpolys.yt_cropped     <- erase(miningpolys.yt, mask.poly.vect)
yt.otherpolys_cropped <- erase(yt.otherpolys, mask.poly.vect)

summary(as.factor(miningpolys.yt_cropped$RC_Dstr))
yt_placer_cropped <- miningpolys.yt_cropped[miningpolys.yt_cropped$RC_Dstr == "Placer Mining", ]
yt_quartz_cropped <- miningpolys.yt_cropped[miningpolys.yt_cropped$RC_Dstr == "Quartz Mining", ]


#Checking paved, looks good!
plot(paved.yt, col = "gray", main = "Before erase")
plot(mask.poly.vect, add = TRUE, border = "red")

plot(paved.yt_cropped, col = "green", main = "After erase")
plot(mask.poly.vect, add = TRUE, border = "red")

#Time to load in new areas, 5 new areas called WP1-5
WP1_lf   <- st_read(file.path(raw, 'YT_disturb', 'WP1_lf.shp'))
WP1_poly   <- st_read(file.path(raw, 'YT_disturb', 'WP1_poly.shp'))
WP1_lf <- st_transform(WP1_lf, crs)
WP1_poly <- st_transform(WP1_poly, crs)
WP1_lf <- vect(WP1_lf)
WP1_poly <- vect(WP1_poly)

summary(as.factor(WP1_poly$TYPE_DISTU))

WP1_paved   <- WP1_lf[WP1_lf$TYPE_DISTU %in% c("Highway", "Railway"), ]
WP1_unpaved <- WP1_lf[WP1_lf$TYPE_DISTU == "Unpaved Road", ]
WP1_other   <- WP1_lf[!(WP1_lf$TYPE_DISTU %in% c("Highway", "Railway", "Unpaved Road")), ]

WP1_othermines <- WP1_poly[WP1_poly$TYPE_DISTU %in% c("Mining"),]
WP1_placer <- WP1_poly[WP1_poly$TYPE_DISTU %in% c("Placer Mining - Minor", "Placer Mining - Significant"),]
WP1_quartz <- WP1_poly[WP1_poly$TYPE_DISTU %in% c("Quartz Mining - Minor", "Quartz Mining - Significant"),]
WP1_otherpolys <- WP1_poly[!(WP1_poly$TYPE_DISTU %in% c("Placer Mining - Minor", "Placer Mining - Significant", "Forestry", "Quartz Mining - Minor", "Quartz Mining - Significant", "Mining")),]

#WP2
WP2_lf   <- st_read(file.path(raw, 'YT_disturb', 'WP2_lf.shp'))
WP2_poly   <- st_read(file.path(raw, 'YT_disturb', 'WP2_poly.shp'))
WP2_lf <- st_transform(WP2_lf, crs)
WP2_poly <- st_transform(WP2_poly, crs)
WP2_lf <- vect(WP2_lf)
WP2_poly <- vect(WP2_poly)

summary(as.factor(WP2_lf$TYPE_DISTU))

WP2_paved   <- WP2_lf[WP2_lf$TYPE_DISTU %in% c("Highway", "Railway"), ]
WP2_unpaved <- WP2_lf[WP2_lf$TYPE_DISTU == "Unpaved Road", ]
WP2_other   <- WP2_lf[!(WP2_lf$TYPE_DISTU %in% c("Highway", "Railway", "Unpaved Road")), ]

WP2_othermines <- WP2_poly[WP2_poly$TYPE_DISTU %in% c("Mining"),]
WP2_placer <- WP2_poly[WP2_poly$TYPE_DISTU %in% c("Placer Mining - Minor", "Placer Mining - Significant"),]
WP2_quartz <- WP2_poly[WP2_poly$TYPE_DISTU %in% c("Quartz Mining - Minor", "Quartz Mining - Significant"),]
WP2_otherpolys <- WP2_poly[!(WP2_poly$TYPE_DISTU %in% c("Placer Mining - Minor", "Placer Mining - Significant", "Forestry", "Quartz Mining - Minor", "Quartz Mining - Significant", "Mining")),]

#WP3
WP3_lf   <- st_read(file.path(raw, 'YT_disturb', 'WP3_lf.shp'))
WP3_poly   <- st_read(file.path(raw, 'YT_disturb', 'WP3_poly.shp'))
WP3_lf <- st_transform(WP3_lf, crs)
WP3_poly <- st_transform(WP3_poly, crs)
WP3_lf <- vect(WP3_lf)
WP3_poly <- vect(WP3_poly)

summary(as.factor(WP3_poly$TYPE_DISTU))

WP3_paved   <- WP3_lf[WP3_lf$TYPE_DISTU %in% c("Highway", "Railway"), ]
WP3_unpaved <- WP3_lf[WP3_lf$TYPE_DISTU == "Unpaved Road", ]
WP3_other   <- WP3_lf[!(WP3_lf$TYPE_DISTU %in% c("Highway", "Railway", "Unpaved Road")), ]

WP3_othermines <- WP3_poly[WP3_poly$TYPE_DISTU %in% c("Mining"),]
WP3_placer <- WP3_poly[WP3_poly$TYPE_DISTU %in% c("Placer Mining - Minor", "Placer Mining - Significant"),]
WP3_quartz <- WP3_poly[WP3_poly$TYPE_DISTU %in% c("Quartz Mining - Minor", "Quartz Mining - Significant"),]
WP3_otherpolys <- WP3_poly[!(WP3_poly$TYPE_DISTU %in% c("Placer Mining - Minor", "Placer Mining - Significant", "Forestry", "Quartz Mining - Minor", "Quartz Mining - Significant", "Mining")),]

#WP4
WP4_lf   <- st_read(file.path(raw, 'YT_disturb', 'WP4_lf.shp'))
WP4_poly   <- st_read(file.path(raw, 'YT_disturb', 'WP4_poly.shp'))
WP4_lf <- st_transform(WP4_lf, crs)
WP4_poly <- st_transform(WP4_poly, crs)
WP4_lf <- vect(WP4_lf)
WP4_poly <- vect(WP4_poly)

summary(as.factor(WP4_poly$TYPE_DISTU))

WP4_paved   <- WP4_lf[WP4_lf$TYPE_DISTU %in% c("Highway", "Railway"), ]
WP4_unpaved <- WP4_lf[WP4_lf$TYPE_DISTU == "Unpaved Road", ]
WP4_other   <- WP4_lf[!(WP4_lf$TYPE_DISTU %in% c("Highway", "Railway", "Unpaved Road")), ]

WP4_othermines <- WP4_poly[WP4_poly$TYPE_DISTU %in% c("Mining"),]
WP4_placer <- WP4_poly[WP4_poly$TYPE_DISTU %in% c("Placer Mining - Minor", "Placer Mining - Significant"),]
WP4_quartz <- WP4_poly[WP4_poly$TYPE_DISTU %in% c("Quartz Mining - Minor", "Quartz Mining - Significant"),]
WP4_otherpolys <- WP4_poly[!(WP4_poly$TYPE_DISTU %in% c("Placer Mining - Minor", "Placer Mining - Significant", "Forestry", "Quartz Mining - Minor", "Quartz Mining - Significant", "Mining")),]


#WP5
WP5_lf   <- st_read(file.path(raw, 'YT_disturb', 'WP5_lf.shp'))
WP5_poly   <- st_read(file.path(raw, 'YT_disturb', 'WP5_poly.shp'))
WP5_lf <- st_transform(WP5_lf, crs)
WP5_poly <- st_transform(WP5_poly, crs)
WP5_lf <- vect(WP5_lf)
WP5_poly <- vect(WP5_poly)

summary(as.factor(WP5_poly$TYPE_DISTU))

WP5_paved   <- WP5_lf[WP5_lf$TYPE_DISTU %in% c("Highway", "Railway"), ]
WP5_unpaved <- WP5_lf[WP5_lf$TYPE_DISTU == "Unpaved Road", ]
WP5_other   <- WP5_lf[!(WP5_lf$TYPE_DISTU %in% c("Highway", "Railway", "Unpaved Road")), ]

WP5_othermines <- WP5_poly[WP5_poly$TYPE_DISTU %in% c("Mining"),]
WP5_placer <- WP5_poly[WP5_poly$TYPE_DISTU %in% c("Placer Mining - Minor", "Placer Mining - Significant"),]
WP5_quartz <- WP5_poly[WP5_poly$TYPE_DISTU %in% c("Quartz Mining - Minor", "Quartz Mining - Significant"),]
WP5_otherpolys <- WP5_poly[!(WP5_poly$TYPE_DISTU %in% c("Placer Mining - Minor", "Placer Mining - Significant", "Forestry", "Quartz Mining - Minor", "Quartz Mining - Significant", "Mining")),]

#Combining new 
yt_paved_new <- rbind(
  WP1_paved,
  WP2_paved,
  WP3_paved,
  WP4_paved,
  WP5_paved
)

yt_unpaved_new <- rbind(
  WP1_unpaved,
  WP2_unpaved,
  WP3_unpaved,
  WP4_unpaved,
  WP5_unpaved
)

yt_other_new <- rbind(
  WP1_other,
  WP2_other,
  WP3_other,
  WP4_other,
  WP5_other
)

yt_othermines_new <- rbind(
  WP1_othermines,
  WP2_othermines,
  WP3_othermines,
  WP4_othermines,
  WP5_othermines
)

yt_placer_new <- rbind(
  WP1_placer,
  WP2_placer,
  WP3_placer,
  WP4_placer,
  WP5_placer
)

yt_quartz_new <- rbind(
  WP1_quartz,
  WP2_quartz,
  WP3_quartz,
  WP4_quartz,
  WP5_quartz
)

yt_otherpolys_new <- rbind(
  WP1_otherpolys,
  WP2_otherpolys,
  WP3_otherpolys,
  WP4_otherpolys,
  WP5_otherpolys
)

#Now combine old with new
yt_paved_comb <- rbind(yt_paved_new, paved.yt_cropped)
yt_unpaved_comb <-rbind(yt_unpaved_new, unpaved.yt_cropped)
yt_otherlf_comb <-rbind(yt_other_new, other.yt_cropped)
yt_placer_comb <- rbind(yt_placer_new, yt_placer_cropped)
yt_quartz_comb <- rbind(yt_quartz_new, yt_quartz_cropped)
yt_othermines_comb <- yt_othermines_new #No 'othermines' in old data
yt_otherpolys_comb <- rbind(yt_otherpolys_new, yt.otherpolys_cropped)

#Adding year and feature_type to all mines to match BC and NWT, other variables are done together later
#Add year in there
head(yt_othermines_comb)
yt_othermines_comb$feature_type <- "othermines"
head(yt_othermines_comb)
df <- as.data.frame(yt_othermines_comb)
df$year <- 2012 #No dates available, so assigning 2012 to all
values(yt_othermines_comb) <- df
table(yt_othermines_comb$year)

#Placer
head(yt_placer_comb)
yt_placer_comb$feature_type <- "placer"
table(yt_placer_comb$year)
table(yt_placer_comb$yr_strt)
df <- as.data.frame(yt_placer_comb)
df$year <- as.numeric(df$yr_strt)
df$year[is.na(df$year)] <- 2012
df$year[df$year < 2012] <- 2012
values(yt_placer_comb) <- df
table(yt_placer_comb$year)

#Quartz
head(yt_quartz_comb)
yt_quartz_comb$feature_type <- "quartz"
table(yt_quartz_comb$year)
table(yt_quartz_comb$yr_strt)
df <- as.data.frame(yt_quartz_comb)
df$year <- as.numeric(df$yr_strt)
df$year[is.na(df$year)] <- 2012
df$year[df$year < 2012] <- 2012
values(yt_quartz_comb) <- df
table(yt_quartz_comb$year)



## NWT ----
#Linear features
nt.roads <- st_read(file.path(raw, 'NWT_disturb', 'transportation_roads.gpkg'))
nt.pipeline <- st_read(file.path(raw, 'NWT_disturb', 'oilgas_piplines.gpkg'))
nt.seismic <- st_read(file.path(raw, 'NWT_disturb', 'oilgas_seismic.gpkg'))
nt.trail <- st_read(file.path(raw, 'NWT_disturb', 'transportation_trails.gpkg'))

nt.roads.proj <- project(vect(nt.roads), crs)
nt.pipeline.proj <- project(vect(nt.pipeline), crs)
nt.seismic.proj <- project(vect(nt.seismic), crs)
nt.trail.proj <- project(vect(nt.trail), crs)

paved.nt <- nt.roads.proj #All roads in the dataset were paved
other.nt <- union(nt.pipeline.proj, nt.seismic.proj)
other.nt <- union(other.nt, nt.trail.proj)


#Poly's
nt.minesbuild <- st_read(file.path(raw, 'NWT_disturb', 'mining_buildings.gpkg'))
nt.minesclear <- st_read(file.path(raw, 'NWT_disturb', 'mining_clearings.gpkg'))
nt.mines <- st_read(file.path(raw, 'NWT_disturb', 'mining_mining.gpkg'))
nt.wells <- st_read(file.path(raw, 'NWT_disturb', 'oilgas_wellsites.gpkg'))
nt.airstrip <- st_read(file.path(raw, 'NWT_disturb', 'transportation_airstrip.gpkg'))
nt.urbuilds <- st_read(file.path(raw, 'NWT_disturb', 'urbanrural_buildings.gpkg'))
nt.urrec <- st_read(file.path(raw, 'NWT_disturb', 'urbanrural_recreation.gpkg'))

nt.minesclear.proj <- project(vect(nt.minesclear), crs)
nt.minesbuild.proj <- project(vect(nt.minesbuild), crs)
nt.minesclear.proj <- project(vect(nt.minesclear), crs)
nt.mines.proj <- project(vect(nt.mines), crs)
nt.wells.proj <- project(vect(nt.wells), crs)
nt.airstrip.proj <- project(vect(nt.airstrip), crs)
nt.urbuilds.proj <- project(vect(nt.urbuilds), crs)
nt.urrec.proj <- project(vect(nt.urrec), crs)

nt.mines <- nt.mines.proj

nt.mines$feature_type <- "othermines"
#Get year in there
head(nt.mines)
df <- as.data.frame(nt.mines)
df$year <- 2012 #No dates available, so assigning 2012 to all
values(nt.mines) <- df
table(nt.mines$year)

nt.otherpolys <- rbind(
  nt.minesbuild.proj,
  nt.minesclear.proj,
  nt.wells.proj,
  nt.airstrip.proj,
  nt.urbuilds.proj,
  nt.urrec.proj
)


#Combining everything together

#Paved
#Combining paved and rail into single shapefile to save
paved.rail.all <- rbind(yt_paved_comb, paved.nt, bc.paved.rail)
paved.rail.all$feature_type <- "paved"

#Add years according to year start, or year end if unavailable
head(paved.rail.all)
table(paved.rail.all$RoadYear) #I looked at all year related columns to see which ones had data to use
df <- as.data.frame(paved.rail.all)
df$year_road <- as.numeric(df$RoadYear)
df$year_start <- as.numeric(df$yr_strt)
df$year_dist <- as.numeric(df$EstYearDist)
df$year <- dplyr::coalesce(df$year_road, df$year_start, df$year_dist)
df <- df[, !(names(df) %in% c("year_road", "year_start", "year_dist"))]
df$year[df$year < 2012] <- 2012
values(paved.rail.all) <- df
table(paved.rail.all$year)
writeVector(paved.rail.all, file.path(raw, 'linear_features', 'roadpaved.shp'), overwrite = TRUE)

#Unpaved
unpaved.all <- rbind(yt_unpaved_comb, bc.unpaved.proj)
unpaved.all$feature_type <- "unpaved"
head(unpaved.all)
table(unpaved.all$EstYearDist)
df <- as.data.frame(unpaved.all)
df$year_road <- as.numeric(df$RoadYear)
df$year_start <- as.numeric(df$yr_strt)
df$year <- dplyr::coalesce(df$year_road, df$year_start)
df <- df[, !(names(df) %in% c("year_road", "year_start"))]
df$year[df$year < 2012] <- 2012
values(unpaved.all) <- df
table(unpaved.all$year)
writeVector(unpaved.all, file.path(raw, 'linear_features', 'roadunpaved.shp'), overwrite = TRUE)

gc()

#Other lfs
other.all <- rbind(yt_otherlf_comb, other.nt, bc.other)
other.all$feature_type <- "otherlf"
head(other.all)
table(other.all$COMMISSIONED_YEAR)
df <- as.data.frame(other.all)
df$year_road <- as.numeric(df$ASRC_YEAR)
df$year_start <- as.numeric(df$yr_strt)
df$year_datemin <- as.numeric(df$COMMISSIONED_YEAR)
df$year <- dplyr::coalesce(df$year_road, df$year_start, df$year_datemin)
df <- df[, !(names(df) %in% c("year_road", "year_start", "year_datemin"))]
df$year[df$year < 2012] <- 2012
df$year[is.na(df$year)] <- 2012 #About 200,000 without a date, should assume they have been there since at least 2012
values(other.all) <- df
table(other.all$year)
writeVector(other.all, file.path(raw, 'linear_features', 'lfother.shp'), overwrite = TRUE)

gc()


#Mining polys
miningpolys.all <- rbind(yt_placer_comb, yt_othermines_comb, yt_quartz_comb, nt.mines, bc.mines)
table(miningpolys.all$year)
table(miningpolys.all$feature_type)
writeVector(miningpolys.all, file.path(raw, 'polygonal_features', 'miningpolys.shp'), overwrite = T)



#Other polys
anthropolys.all <- rbind(yt_otherpolys_comb, nt.otherpolys, bc.polys)
anthropolys.all$feature_type <- "otherpoly"
head(anthropolys.all)
table(anthropolys.all$PROJECT_ESTABLISHED_DATE)
df <- as.data.frame(anthropolys.all)
df$year_road <- as.numeric(df$yr_strt)
df$year_start <- as.numeric(format(as.Date(df$OPERATIONS_EFFECTIVE_DATE, format = "%Y-%m-%d"), "%Y"))
df$year_datemin <- as.numeric(format(as.Date(df$PROJECT_ESTABLISHED_DATE, format = "%Y-%m-%d"), "%Y"))
df$year <- dplyr::coalesce(df$year_road, df$year_start, df$year_datemin)
df <- df[, !(names(df) %in% c("year_road", "year_start", "year_datemin"))]
df$year[df$year < 2012] <- 2012
df$year[is.na(df$year)] <- 2012 #Assign 2012 to ones with no data
values(anthropolys.all) <- df
table(anthropolys.all$year)
writeVector(anthropolys.all, file.path(raw, 'polygonal_features', 'anthropolys.shp'), overwrite = T)

#Load back in and combine all lfs and polys
gc()
unpaved <- vect(file.path(raw, 'linear_features', 'roadunpaved.shp'))
paved <- vect(file.path(raw, 'linear_features', 'roadpaved.shp'))
otherpoly <- vect(file.path(raw, 'polygonal_features', 'anthropolys.shp'))
miningpoly <- vect(file.path(raw, 'polygonal_features', 'miningpolys.shp'))
lfother <- vect(file.path(raw, 'linear_features', 'lfother.shp'))

table(miningpoly$feature_t0)

#Combine all lf's into single shp
table(unpaved$year)
table(paved$year)
table(lfother$year)

names(paved)
table(unpaved$feature_t0) 
table(paved$feature_t0)
table(lfother$feature_t0)

gc()

sf_paved   <- st_as_sf(paved)
sf_unpaved <- st_as_sf(unpaved)
sf_lfother <- st_as_sf(lfother)

sf_all <- bind_rows(sf_paved, sf_unpaved, sf_lfother)


line_sf <- vect(sf_all)
names(line_sf)
table(line_sf$year)
table(line_sf$feature_t0)

terra::writeVector(line_sf, file.path(derived, "combined_linear_features.shp"), overwrite = TRUE)
#line_sf <- vect(file.path(derived, "combined_linear_features.shp"))

#Looking good! Phew! Now combine all poly's
names(otherpoly)
table(miningpoly$year)
names(otherpoly)[names(otherpoly) == "year_1"] <- "year"
table(otherpoly$year)

table(miningpoly$feature_t0) 
table(otherpoly$feature_t0)

gc()

sf_otherpoly   <- st_as_sf(otherpoly)
sf_miningpoly  <- st_as_sf(miningpoly)

sf_poly_all <- dplyr::bind_rows(sf_otherpoly, sf_miningpoly)

poly_sf <- terra::vect(sf_poly_all)

table(poly_sf$year)           
table(poly_sf$feature_t0)   

terra::writeVector(poly_sf, file.path(derived, "combined_poly_features.shp"), overwrite = TRUE)
poly_sf <- vect(file.path(derived, "combined_poly_features.shp"))
#Looking good, year and feature type carried over! Now for the re-rasterization


# STARTING RASTER CALCULATIONS
#For all of the below, I set the code up so that features persist into future years (2012 road will be on 2013 raster and so on, but 2014 road will not be on 2012 raster)
# Reproject to match base raster
poly_sf <- terra::project(poly_sf, crs(rasterToMatch_extendedLandscape30m))
line_sf <- terra::project(line_sf, crs(rasterToMatch_extendedLandscape30m))

#Files too large leading to memory crashes - taking only the necessary columns
names(poly_sf)[names(poly_sf) == "year_1"] <- "year"
poly_sf <- poly_sf[, c("feature_t0", "year")]
line_sf <- line_sf[, c("feature_t0", "year")]

table(line_sf$feature_t0)

# Define years, resolutions, categories
years <- 2012:2024
poly_categories <- c("quartz", "placer", "othermines", "otherpoly")
line_categories <- c("paved", "unpaved", "otherlf")

res_levels <- c("250m", "1000m", "5000m")
types <- c("dist", "pa", "dens")

#Create directories
for (type in types) {
  for (res in res_levels) {
    dir.create(file.path(derived, paste0(type, "_disturb_", res)), recursive = TRUE, showWarnings = FALSE)
  }
}

gc()

# ---- DISTANCE-TO ------
for (cat in c(line_categories, poly_categories)) {
  message("======= Processing feature type: ", cat, " =======")
  
  for (year in years) {
    message("→ Year ", year)
    
    # Define output paths
    out_file_250m  <- file.path(derived, "dist_disturb_250m",  paste0(cat, "_distance_250m_", year, ".tif"))
    out_file_1000m <- file.path(derived, "dist_disturb_1000m", paste0(cat, "_distance_1000m_", year, ".tif"))
    out_file_5000m <- file.path(derived, "dist_disturb_5000m", paste0(cat, "_distance_5000m_", year, ".tif"))
    
    # Skip if all output files exist
    if (file.exists(out_file_250m) && file.exists(out_file_1000m) && file.exists(out_file_5000m)) {
      message("  All output files exist for this year and category — skipping.")
      next
    }
    
    # Subset feature data for year/category
    if (cat %in% line_categories) {
      feat <- line_sf[line_sf$feature_t0 == cat & line_sf$year <= year, ]
    } else {
      feat <- poly_sf[poly_sf$feature_t0 == cat & poly_sf$year <= year, ]
    }
    
    
    # Write temp vector to disk and reload (helps avoid memory errors)
    temp_vec_file <- paste0("temp_", cat, "_", year, ".gpkg")
    writeVector(feat, temp_vec_file, overwrite = TRUE)
    feat <- vect(temp_vec_file)
    
    # Rasterize to binary 30m raster (where NA is no feature present, 1 is feature present)
    bin_ras_30m <- terra::rasterize(feat, rasterToMatch_extendedLandscape30m, touches = TRUE)
    bin_ras_30m[!is.na(bin_ras_30m)] <- 1
    rm(feat)
    gc()
    
    # Compute Euclidean distance (assigning each cell with a distance to the nearest cell with a value of 1)
    dist_ras_30m <- terra::distance(bin_ras_30m)
    rm(bin_ras_30m)
    gc()
    
    # Write distance rasters at 3 resolutions (averaging up from the 30m base raster)
    if (!file.exists(out_file_250m)) {
      message("  Writing 250m raster...")
      dist_ras_250m <- terra::resample(dist_ras_30m, rasterToMatch_extendedLandscape250m, method = "average")
      writeRaster(dist_ras_250m, out_file_250m, filetype = "GTiff", overwrite = TRUE)
      rm(dist_ras_250m)
      gc()
    } else {
      message("  250m raster already exists — skipping.")
    }
    
    if (!file.exists(out_file_1000m)) {
      message("  Writing 1000m raster...")
      dist_ras_1000m <- terra::resample(dist_ras_30m, rasterToMatch_extendedLandscape1000m, method = "average")
      writeRaster(dist_ras_1000m, out_file_1000m, filetype = "GTiff", overwrite = TRUE)
      rm(dist_ras_1000m)
      gc()
    } else {
      message("  1000m raster already exists — skipping.")
    }
    
    if (!file.exists(out_file_5000m)) {
      message("  Writing 5000m raster...")
      dist_ras_5000m <- terra::resample(dist_ras_30m, rasterToMatch_extendedLandscape5000m, method = "average")
      writeRaster(dist_ras_5000m, out_file_5000m, filetype = "GTiff", overwrite = TRUE)
      rm(dist_ras_5000m)
      gc()
    } else {
      message("  5000m raster already exists — skipping.")
    }
    
    # Final cleanup
    rm(dist_ras_30m)
    unlink(temp_vec_file)
    terra::tmpFiles(remove = TRUE)
    gc()
  }
}


# ---- PRESENCE/ABSENCE ------
gc()
for (cat in c(line_categories, poly_categories)) {
  message("======= Processing presence/absence for: ", cat, " =======")
  
  for (year in years) {
    message("→ Year ", year)
    
    # Define output files
    out_file_250m   <- file.path("data", "derived", "pa_disturb_250m",  paste0(cat, "_pa_250m_", year, ".tif"))
    out_file_1000m  <- file.path("data", "derived", "pa_disturb_1000m", paste0(cat, "_pa_1000m_", year, ".tif"))
    out_file_5000m  <- file.path("data", "derived", "pa_disturb_5000m", paste0(cat, "_pa_5000m_", year, ".tif"))
    
    # Skip if all outputs already exist
    if (file.exists(out_file_250m) && file.exists(out_file_1000m) && file.exists(out_file_5000m)) {
      message("  Skipping ", cat, " for year ", year, " (files already exist)")
      next
    }
    
    # Subset feature data for year/category
    if (cat %in% line_categories) {
      feat <- line_sf[line_sf$feature_t0 == cat & line_sf$year <= year, ]
    } else {
      feat <- poly_sf[poly_sf$feature_t0 == cat & poly_sf$year <= year, ]
    }
    
    # Write temp vector to disk and reload (helps avoid memory errors)
    temp_vec_file <- paste0("temp_", cat, "_", year, ".gpkg")
    writeVector(feat, temp_vec_file, overwrite = TRUE)
    feat <- vect(temp_vec_file)
    
    # Make base raster of the feature, where 1 = present, 0 = absent
    bin_ras_30m <- terra::rasterize(feat, rasterToMatch_extendedLandscape30m, touches = TRUE)
    bin_ras_30m[!is.na(bin_ras_30m)] <- 1
    bin_ras_30m[is.na(bin_ras_30m)] <- 0  # Make 0 = absent
    rm(feat)
    gc()
    
    # Scale up to the different resolutions, using max
    message("  Writing 250m raster...")
    bin_250 <- terra::resample(bin_ras_30m, rasterToMatch_extendedLandscape250m, method = "max")
    writeRaster(bin_250, out_file_250m, filetype = "GTiff", overwrite = TRUE)
    rm(bin_250)
    gc()
    
    message("  Writing 1000m raster...")
    bin_1000 <- terra::resample(bin_ras_30m, rasterToMatch_extendedLandscape1000m, method = "max")
    writeRaster(bin_1000, out_file_1000m, filetype = "GTiff", overwrite = TRUE)
    rm(bin_1000)
    gc()
    
    message("  Writing 5000m raster...")
    bin_5000 <- terra::resample(bin_ras_30m, rasterToMatch_extendedLandscape5000m, method = "max")
    writeRaster(bin_5000, out_file_5000m, filetype = "GTiff", overwrite = TRUE)
    rm(bin_5000)
    gc()
    
    rm(bin_ras_30m)
    unlink(temp_vec_file)
    terra::tmpFiles(remove = TRUE)
    gc()
  }
}

# ----- DENSITY (FOR LF DISTURBANCE) -----
gc()
for (cat in c(line_categories)) {
  message("======= Processing density for: ", cat, " =======")
  
  for (year in years) {
    message("→ Year ", year)
    
    #Define output paths
    out_file_250m   <- file.path("data", "derived", "dens_disturb_250m",  paste0(cat, "_dens_250m_", year, ".tif"))
    out_file_1000m  <- file.path("data", "derived", "dens_disturb_1000m", paste0(cat, "_dens_1000m_", year, ".tif"))
    out_file_5000m  <- file.path("data", "derived", "dens_disturb_5000m", paste0(cat, "_dens_5000m_", year, ".tif"))
  
    
    if (cat %in% line_categories) {
      feat <- line_sf[line_sf$feature_t0 == cat & line_sf$year <= year, ]
    } else {
      feat <- poly_sf[poly_sf$feature_t0 == cat & poly_sf$year <= year, ]
    }
    
    # Write temp vector to disk and reload (helps avoid memory errors)
    temp_vec_file <- paste0("temp_", cat, "_", year, ".gpkg")
    writeVector(feat, temp_vec_file, overwrite = TRUE)
    feat <- vect(temp_vec_file)
    
    #Make base raster of the feature, where 1 = present, 0 = absent
    bin_ras_30m <- terra::rasterize(feat, rasterToMatch_extendedLandscape30m, touches = TRUE)
    bin_ras_30m[!is.na(bin_ras_30m)] <- 1 #Present
    bin_ras_30m[is.na(bin_ras_30m)] <- 0  # Make 0 = absent
    rm(feat)
    gc()
    
    #Resample to each coarser resolution
    message("  Writing 250m raster...")
    bin_250 <- terra::resample(bin_ras_30m, rasterToMatch_extendedLandscape250m, method = "average")
    writeRaster(bin_250, out_file_250m, filetype = "GTiff", overwrite = TRUE)
    rm(bin_250)
    gc()
    
    message("  Writing 1000m raster...")
    bin_1000 <- terra::resample(bin_ras_30m, rasterToMatch_extendedLandscape1000m, method = "average")
    writeRaster(bin_1000, out_file_1000m, filetype = "GTiff", overwrite = TRUE)
    rm(bin_1000)
    gc()
    
    message("  Writing 5000m raster...")
    bin_5000 <- terra::resample(bin_ras_30m, rasterToMatch_extendedLandscape5000m, method = "average")
    writeRaster(bin_5000, out_file_5000m, filetype = "GTiff", overwrite = TRUE)
    rm(bin_5000)
    gc()
    
    rm(bin_ras_30m)
    unlink(temp_vec_file)
    terra::tmpFiles(remove = TRUE)
    gc()
  }
}


# ---- DENSITY (FOR POLYGONAL DISTURBANCE) ------
gc()
for (cat in poly_categories) {
  message("======= Processing polygonal density for: ", cat, " =======")
  
  for (year in years) {
    message("→ Year ", year)
    
    # Define output file paths
    out_file_250m   <- file.path("data", "derived", "dens_disturb_250m",  paste0(cat, "_dens_250m_", year, ".tif"))
    out_file_1000m  <- file.path("data", "derived", "dens_disturb_1000m", paste0(cat, "_dens_1000m_", year, ".tif"))
    out_file_5000m  <- file.path("data", "derived", "dens_disturb_5000m", paste0(cat, "_dens_5000m_", year, ".tif"))
    
    # Subset polygon features for current category and year
    feat <- poly_sf[poly_sf$feature_t0 == cat & poly_sf$year <= year, ]
    
    # Save and reload from disk to avoid memory issues
    temp_vec_file <- file.path(tempdir(), paste0("temp_poly_", cat, "_", year, ".gpkg"))
    writeVector(feat, temp_vec_file, overwrite = TRUE)
    feat <- vect(temp_vec_file)
    
    # Rasterize to 30m, presence = 1, absence = 0
    bin_ras_30m <- terra::rasterize(feat, rasterToMatch_extendedLandscape30m, touches = TRUE)
    bin_ras_30m[!is.na(bin_ras_30m)] <- 1
    bin_ras_30m[is.na(bin_ras_30m)] <- 0
    rm(feat)
    gc()
    
    # Aggregate to coarser resolutions using average 
    message("  Writing 250m raster...")
    bin_250 <- terra::resample(bin_ras_30m, rasterToMatch_extendedLandscape250m, method = "average")
    writeRaster(bin_250, out_file_250m, filetype = "GTiff", overwrite = TRUE)
    rm(bin_250)
    gc()
    
    message("  Writing 1000m raster...")
    bin_1000 <- terra::resample(bin_ras_30m, rasterToMatch_extendedLandscape1000m, method = "average")
    writeRaster(bin_1000, out_file_1000m, filetype = "GTiff", overwrite = TRUE)
    rm(bin_1000)
    gc()
    
    message("  Writing 5000m raster...")
    bin_5000 <- terra::resample(bin_ras_30m, rasterToMatch_extendedLandscape5000m, method = "average")
    writeRaster(bin_5000, out_file_5000m, filetype = "GTiff", overwrite = TRUE)
    rm(bin_5000)
    gc()
    
    rm(bin_ras_30m)
    unlink(temp_vec_file)
    terra::tmpFiles(remove = TRUE)
    gc()
  }
}



#Re-pull for 2024 data
## FIRES ----

#NBACURL <- "https://cwfis.cfs.nrcan.gc.ca/downloads/nbac/nbac_1972_2023_20240530_shp.zip"
#NFDBURL <- "https://cwfis.cfs.nrcan.gc.ca/datamart/download/nfdbpoly/NFDB_poly.zip"


#Load NBAC (1972–2023) and NFDB (pre-1972) shapefiles
fires <- vect(file.path(canada, 'fire_nbac_1972_2023', 'nbac_1972_2023_20240530.shp'))
fires.old <- vect(file.path(canada, 'NFDB_poly', 'NFDB_poly_20210707.shp'))


# katie update
# Define URLs and local paths
NBACURL <- "https://cwfis.cfs.nrcan.gc.ca/downloads/nbac/NBAC_1972to2024_20250506_shp.zip"
NFDBURL <- "https://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/NFDB_poly.zip"

nbac_zip <- file.path(raw, "fires", "nbac_1972_2024.zip")
nfdb_zip <- file.path(raw, "fires", "nfdb_poly.zip")

nbac_dir <- file.path(raw, "fires", "fire_nbac_1972_2024")
nfdb_dir <- file.path(raw, "fires", "NFDB_poly")

# Download if missing
if (!file.exists(nbac_zip)) download.file(NBACURL, nbac_zip)
if (!file.exists(nfdb_zip)) download.file(NFDBURL, nfdb_zip)

# Unzip if needed
if (!dir.exists(nbac_dir)) unzip(nbac_zip, exdir = nbac_dir)
if (!dir.exists(nfdb_dir)) unzip(nfdb_zip, exdir = nfdb_dir)

# Define shapefile paths
nbac_shp <- file.path(nbac_dir, "NBAC_1972to2024_20250506.shp")
nfdb_shp <- file.path(nfdb_dir, "NFDB_poly_20210707.shp")

# Load and reproject
fires <- vect(nbac_shp)
fires.old <- vect(nfdb_shp)

#-- katie update end


#Reproject to match study area CRS
fires.proj <- project(fires, crs)
fires.old.proj <- project(fires.old, crs)

#Subset only pre-1972 fires from NFDB
fires.pre <- subset(fires.old.proj, fires.old.proj$YEAR < 1972)

#Crop new and old fires to study area extent
nmc.fires.proj <- crop(fires.proj, ext(studyArea_extendedLandscape))
nmc.fires.pre  <- crop(fires.pre, ext(studyArea_extendedLandscape))

#Merge pre-1972 and post-1972 fire polygons
gc()
nmc.fires.all <- union(nmc.fires.pre, nmc.fires.proj)

#Clean up and standardize YEAR field
gc()
d.fires.all <- as.data.frame(nmc.fires.all)

#Try to keep original YEAR field, but fall back to REP_DATE if needed
nmc.fires.all$year_burn <- nmc.fires.all$YEAR
nmc.fires.all$REP_YEAR  <- as.integer(gsub("/.*$", "", nmc.fires.all$REP_DATE)) 
nmc.fires.all$YEAR <- ifelse(!is.na(nmc.fires.all$year_burn),
                             nmc.fires.all$year_burn,
                             nmc.fires.all$REP_YEAR)
#Save cleaned full dataset
writeVector(nmc.fires.all, file.path(raw, 'fires', 'fires_1920_2024.shp'), overwrite = TRUE)


#Clean Up Invalid YEARs using FIRE_ID

# Load cleaned shapefile
fires_clean <- vect(file.path(raw, "fires", "fires_1920_2024.shp"))

# Extract likely year from FIRE_ID where available (e.g., "1985-FIRE_12")
fire_ids <- as.character(fires_clean$FIRE_ID)
year_guess <- stringr::str_extract(fire_ids, "^19[0-9]{2}")

# Replace invalid year values (-9999 etc)
invalid_years <- fires_clean$YEAR %in% c(-9999, -2147483648)

fires_clean$YEAR[invalid_years] <- ifelse(!is.na(year_guess[invalid_years]),
                                          as.integer(year_guess[invalid_years]),
                                          NA)
# Save cleaned version
writeVector(fires_clean, file.path(raw, "fires", "fires_1920_2024_cleaned.shp"), overwrite = TRUE)


#Reproject to Different Raster Grids (250m, 1000m, 5000m)
# Create aligned vector layers at each resolution
fire250m <- reproducible::prepInputs(
  targetFile = "fires_1920_2024_cleaned.shp",
  destinationPath = file.path(raw, "fires"),
  fun = "terra::vect",
  to = rasterToMatch_extendedLandscape250m
) |> Cache()

writeVector(fire250m, file.path(derived, "fires_1920_2024_250m.shp"))

fire1000m <- reproducible::prepInputs(
  targetFile = "fires_1920_2024_cleaned.shp",
  destinationPath = file.path(raw, "fires"),
  fun = "terra::vect",
  to = rasterToMatch_extendedLandscape1000m
) |> Cache()

writeVector(fire1000m, file.path(derived, "fires_1920_2024_1000m.shp"))

fire5000m <- reproducible::prepInputs(
  targetFile = "fires_1920_2024_cleaned.shp",
  destinationPath = file.path(raw, "fires"),
  fun = "terra::vect",
  to = rasterToMatch_extendedLandscape5000m
) |> Cache()

writeVector(fire5000m, file.path(derived, "fires_1920_2024_5000m.shp"))

gc()


#Create directories for yearly fire rasters
dir.create(file.path(derived, "fireRasters250m"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(derived, "fireRasters1000m"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(derived, "fireRasters5000m"), recursive = TRUE, showWarnings = FALSE)


# ----- Fire rasters yearly -----

# Rasterize to 250m grid
make_fire_rast(
  layer = file.path(derived, "fires_1920_2024_250m.shp"),
  rast = rasterToMatch_extendedLandscape250m,
  minyr = 2012,
  where2save = file.path(derived, "fireRasters250m")
)

# Rasterize to 1000m grid
make_fire_rast(
  layer = file.path(derived, "fires_1920_2024_1000m.shp"),
  rast = rasterToMatch_extendedLandscape1000m,
  minyr = 2012,
  where2save = file.path(derived, "fireRasters1000m")
)

# Rasterize to 5000m grid
make_fire_rast(
  layer = file.path(derived, "fires_1920_2024_5000m.shp"),
  rast = rasterToMatch_extendedLandscape5000m,
  minyr = 2012,
  where2save = file.path(derived, "fireRasters5000m")
)


#---- Forest age ----
#Load in forest age data
forageURL <- "https://opendata.nfis.org/downloads/forest_change/CA_forest_age_2019.zip"

# Had to manually unzip the file to get tiff, for some reason here to get it to work
unzip(file.path(raw, "CA_forest_age_2019.zip"), exdir = file.path(raw, "CA_forest_age_2019_unzipped"))
unzipped_dir <- file.path(raw, "CA_forest_age_2019_unzipped")
tif <- "CA_forest_age_2019.tif"

# Load 30m resolution forest age raster using reproducible workflow
forage_raster <- prepInputs(
  archive = file.path(raw, "CA_forest_age_2019.zip"),
  targetFile = tif,
  destinationPath = unzipped_dir,
  fun = terra::rast,
  method = "near" 
) |> Cache()

# Reproject to match 30m base raster
forage30m <- terra::project(forage_raster, rasterToMatch_extendedLandscape30m, method = "near")
forage30m[forage30m == 0] <- NA
writeRaster(forage30m, file.path(derived, 'nmc_forest_age30.tif'))

# Resample forest age raster at coarser resolutions for later use

# Load forest age again (for average aggregation) for scale-up
forage_raster_scaleup <- prepInputs(
  archive = file.path(raw, "CA_forest_age_2019.zip"),
  targetFile = tif,
  destinationPath = unzipped_dir,
  fun = terra::rast,
  method = "average"  # Using average for scaling up
) |> Cache()

# Resample to 250m resolution
forage250m <- terra::project(forage_raster_scaleup, rasterToMatch_extendedLandscape250m, method = "average")
forage250m[forage250m == 0] <- NA
writeRaster(forage250m, file.path(derived, 'nmc_forest_age_250.tif'))

# Resample to 1000m resolution
forage1000m <- terra::project(forage_raster_scaleup, rasterToMatch_extendedLandscape1000m, method = "average")
forage1000m[forage1000m == 0] <- NA
writeRaster(forage1000m, file.path(derived, 'nmc_forest_age_1000.tif'))

# Resample to 5000m resolution
forage5000m <- terra::project(forage_raster_scaleup, rasterToMatch_extendedLandscape5000m, method = "average")
forage5000m[forage5000m == 0] <- NA
writeRaster(forage5000m, file.path(derived, 'nmc_forest_age_5000.tif'))

#---- Yearly forest age rasters ----
#Years we want data for
years <- 2012:2024

# Store resampled rasters in a named list
ages <- list(
  age250 = forage250m,
  age1000 = forage1000m,
  age5000 = forage5000m
)

# Loop over resolutions and years. 
for (res in names(ages)) {
  age2019 <- ages[[res]]  # Starting point: forest age in 2019
  
  res_outdir <- file.path(raw, res)  # Output folder for that resolution
  dir.create(res_outdir, recursive = TRUE, showWarnings = FALSE)
  
  for (yr in years) {
    delta_years <- 2019 - yr  # Difference in years between target year and 2019
    age_year <- age2019 - delta_years  # Subtract elapsed years to get age in year of interest
    age_year[age_year < 0] <- NA  # No negative ages — set to NA
    
    outname <- file.path(res_outdir, paste0("forest_age_", yr, ".tif"))
    writeRaster(age_year, outname, overwrite = TRUE)  # Save raster
  }
}

#Checking
#forage250_2012 <- rast(file.path(raw, 'age250', 'forest_age_2012.tif'))
#plot(forage250_2012)

#forage250_2024 <- rast(file.path(raw, 'age250', 'forest_age_2024.tif'))
#plot(forage250_2024)

#forage5000_2012 <- rast(file.path(raw, 'age5000', 'forest_age_2012.tif'))
#plot(forage5000_2012)


#---- Elevation ----
# Paths
elev_dir <- file.path(derived, "elevation")
dir.create(elev_dir, showWarnings = FALSE)

# Reproject study area to WGS84 for elevatr
studyArea_wgs84 <- project(studyArea_extendedLandscape, "EPSG:4326")
studyArea_sf <- st_as_sf(studyArea_wgs84)

# Set cache path
options("reproducible.cachePath" = file.path(elev_dir))

# Download elevation once (z = 10 ≈ ~30m resolution)
elev_raw <- Cache(get_elev_raster,
                  locations = studyArea_sf,
                  z = 10,
                  clip = "locations")

elev_base <- rast(elev_raw)

# Function to reproject, resample, save, and cache
prep_elevation <- function(rtm, filename) {
  elev_proj <- project(elev_base, crs(rtm))
  elev_resampled <- resample(elev_proj, rtm, method = "bilinear")
  elev_path <- file.path(elev_dir, filename)
  writeRaster(elev_resampled, elev_path, overwrite = TRUE)
  
  # Use prepInputs to cache and reload
  prepInputs(
    targetFile = filename,
    destinationPath = elev_dir,
    fun = "terra::rast"
  )
}

# Run for all three resolutions
elev250m <- prep_elevation(rasterToMatch_extendedLandscape250m, "elevation_250m.tif")
elev1000m <- prep_elevation(rasterToMatch_extendedLandscape1000m, "elevation_1000m.tif")
elev5000m <- prep_elevation(rasterToMatch_extendedLandscape5000m, "elevation_5000m.tif")

