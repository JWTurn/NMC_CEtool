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


#Generating base rasters with year of harvest for each resolution and year (cumulative rasters to update cells if harvested again in cell)

years <- 2012:2020

resolutions <- list(
  "harv250" = harv250m,
  "harv1000" = harv1000m,
  "harv5000" = harv5000m
)

for (res_name in names(resolutions)) {
  out_dir <- file.path(derived, res_name) 
  
  original_rast <- resolutions[[res_name]]
  
  cumulative_rast <- classify(original_rast, matrix(c(-Inf, Inf, NA), ncol = 3, byrow = TRUE))
  
  for (yr in years) {
    masked_rast <- mask(original_rast, original_rast <= yr, maskvalue = FALSE)
    harvested_this_year <- masked_rast == yr
    
    values_to_update <- which(values(harvested_this_year) == 1)
    if (length(values_to_update) > 0) {
      v <- values(cumulative_rast)
      v[values_to_update] <- yr
      values(cumulative_rast) <- v
    }
    
    out_path <- file.path(out_dir, paste0("harv_", yr, ".tif"))
    writeRaster(cumulative_rast, out_path, overwrite = TRUE)
  }
}


## Calculating average distance to harvest for each year of harvest at the larger resolutions (250, 1000, and 5000)

dist_results <- list()

# Define output folders for each resolution
dir_250 <- file.path(derived, "harv250")
dir_1000 <- file.path(derived, "harv1000")
dir_5000 <- file.path(derived, "harv5000")

# Create directories if they don't exist
dir.create(dir_250, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_1000, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_5000, recursive = TRUE, showWarnings = FALSE)

# Loop over years
for (year in 2012:2020) {
  message("Processing year ", year)
  
  # Binary raster: 1 where harvest occurred that year, NA elsewhere
  harvest_bin <- harv30m == year
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


## Making presence/absence rasters of harvest (active) in cell per year for 1000 and 5000 res

for (year in 2012:2020) {
  message("Processing year ", year)
  
  # Binary raster: 1 = harvest that year, NA = no harvest
  harvest_bin <- harv30m == year
  harvest_bin[harvest_bin == 0] <- NA
  
  # Replace NA with 0 for presence/absence aggregation
  harvest_pa <- classify(harvest_bin, cbind(NA, 0))  # all NAs become 0, so 1 = harvest, 0 = no harvest
  
  # Aggregate to coarser resolutions using max (presence if any pixel in block was harvested)
  pa_250 <- aggregate(harvest_pa, fact = res(harv250m) / res(harv30m), fun = max, na.rm = TRUE)
  pa_1000 <- aggregate(harvest_pa, fact = res(harv1000m) / res(harv30m), fun = max, na.rm = TRUE)
  pa_5000 <- aggregate(harvest_pa, fact = res(harv5000m) / res(harv30m), fun = max, na.rm = TRUE)
  
  # Save rasters in appropriate folders
  writeRaster(pa_250, file.path(dir_250, paste0("harvpres_250m_", year, ".tif")),
              filetype = "GTiff", overwrite = TRUE)
  writeRaster(pa_1000, file.path(dir_1000, paste0("harvpres_1000m_", year, ".tif")),
              filetype = "GTiff", overwrite = TRUE)
  writeRaster(pa_5000, file.path(dir_5000, paste0("harvpres_5000m_", year, ".tif")),
              filetype = "GTiff", overwrite = TRUE)
  
  message("  Done: ", year)
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

## Landcover ----

#landURL <- "https://opendata.nfis.org/downloads/forest_change/CA_forest_VLCE2_2010.zip"

# Source necessary R scripts
#source("C:/Users/peteb/NMC_CEtool_PB/R/make_landforest_prop.R")
#source("C:/Users/peteb/NMC_CEtool_PB/R/aggregate_landforest.R")

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


## ROADS ----
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

#Adding year column using Work_Date_First column, if not available, use Work_Date_Date if >2012
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

bc.union <- union(bc.ag.proj, bc.ogbuild.proj)
bc.union <- union(bc.union, bc.ogwells.proj)
bc.union <- union(bc.union, bc.airstrip.proj)
bc.union <- union(bc.union, bc.urbuild.proj)
bc.union <- union(bc.union, bc.urlandfill.proj)
bc.union <- union(bc.union, bc.urrec.proj)
bc.union <- union(bc.union, bc.urresidence.proj)

bc.polys <- bc.union

#Yukon- UPDATE WITH NEW DATA WHEN IN
yt.lf <- st_read(roads.yt)
yt.lf.ls <- summary(as.factor(yt.lf$TYPE_DI))
yt.lf.ls
yt.poly <- st_read(polys.yt)
yt.poly.ls <- list(unique(yt.poly$RC_Dstr))
summary(as.factor(yt.poly$RC_Dstr))
yt.poly.ls


#NWT
#Linear features
nt.roads <- st_read(file.path(raw, 'NWT_disturb', 'transportation_roads.gpkg'))
nt.pipeline <- st_read(file.path(raw, 'NWT_disturb', 'oilgas_piplines.gpkg'))
nt.seismic <- st_read(file.path(raw, 'NWT_disturb', 'oilgas_seismic.gpkg'))
nt.trail <- st_read(file.path(raw, 'NWT_disturb', 'transportation_trails.gpkg'))

nt.roads.proj <- project(vect(nt.roads), crs)
nt.pipeline.proj <- project(vect(nt.pipeline), crs)
nt.seismic.proj <- project(vect(nt.seismic), crs)
nt.trail.proj <- project(vect(nt.trail), crs)

paved.nt <- nt.roads.proj
other.nt <- union(nt.pipeline.proj, nt.seismic.proj)
other.nt <- union(other.nt, nt.trail.proj)


#Combining paved and unpaved into single shapefile to save
paved.yt <- project(vect(dplyr::filter(yt.lf, RECLS_D %in% c('Highway', 'Railway'))), crs)
paved.rail.all <- rbind(paved.yt, paved.nt, bc.paved.rail)
paved.rail.all$feature_type <- "paved"

#Add years according to year start, or year end if unavailable
head(paved.rail.all)
table(paved.rail.all$RoadYear)
df <- as.data.frame(paved.rail.all)
df$year_road <- as.numeric(df$RoadYear)
df$year_start <- as.numeric(df$yr_strt)
df$year_datemin <- as.numeric(format(as.Date(df$datemin, format = "%Y-%m-%d"), "%Y"))
df$year <- dplyr::coalesce(df$year_road, df$year_start, df$year_datemin)
df <- df[, !(names(df) %in% c("year_road", "year_start", "year_datemin"))]
df$year[df$year < 2012] <- 2012
values(paved.rail.all) <- df
table(paved.rail.all$feature_type)
writeVector(paved.rail.all, file.path(raw, 'linear_features', 'roadpaved.shp'), overwrite = TRUE)

unpaved.yt <- project(vect(dplyr::filter(yt.lf, LF_TYPE == 'Unpaved Road')), crs)
unpaved.all <- rbind(unpaved.yt, unpaved.nt, bc.unpaved.proj)
unpaved.all$feature_type <- "unpaved"
head(unpaved.all)
table(unpaved.all$RoadYear)
df <- as.data.frame(unpaved.all)
df$year_road <- as.numeric(df$RoadYear)
df$year_start <- as.numeric(df$yr_strt)
df$year_datemin <- as.numeric(format(as.Date(df$datemin, format = "%Y-%m-%d"), "%Y"))
df$year <- dplyr::coalesce(df$year_road, df$year_start, df$year_datemin)
df <- df[, !(names(df) %in% c("year_road", "year_start", "year_datemin"))]
df$year[df$year < 2012] <- 2012
values(unpaved.all) <- df
table(unpaved.all$year)
writeVector(unpaved.all, file.path(raw, 'linear_features', 'roadunpaved.shp'), overwrite = TRUE)

gc()

other.yt <- project(vect(dplyr::filter(yt.lf, LF_TYPE == 'Other LF' & RECLS_D != 'Railway')), crs)
other.all <- rbind(other.yt, other.nt, bc.other)
other.all$feature_type <- "otherlf"
head(other.all)
table(other.all$SOURCE_DATE)
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
otherlf <- vect(file.path(raw, 'linear_features', 'lfother.shp'))

#Removing strange seismic anomaly
remove_ids <- c(
  65792, 65793, 65794, 65795, 65796, 65797, 65798, 65799, 65800, 65801, 65802,
  65803, 65804, 65805, 65806, 65807, 65808, 65809, 65810, 65811, 65812, 65813,
  65814, 65815, 65816, 65817, 65818, 65819, 65820, 65821, 65822, 65823, 65824,
  65825, 65826, 65827, 65828, 65829, 65834, 65835, 65836, 65837, 65838, 65839,
  65840, 65841, 65842, 65843, 65844, 65845, 65846, 65847, 65848, 65849, 65850,
  65851, 65852, 65853, 65854, 65855, 65856, 65857, 65858, 65859, 65860, 65861,
  65862, 65863, 65864, 65865, 65866, 65867, 65868, 65869, 65870, 66257, 66258,
  66259, 66260, 66261, 66262, 66263, 66264, 66265, 66266, 66267, 66268, 66269,
  66270, 66271, 66272, 66273, 66274, 66275, 66276, 66277, 66278, 66279, 66280,
  66281, 66282, 66283, 66284, 66285, 66286, 66287, 66288, 66289, 66290, 66291,
  66292, 66293, 66294, 66295, 66296, 66297, 66298, 66299
)


otherlf <- otherlf[ !otherlf$OBJECTID %in% remove_ids, ]


writeVector(otherlf, file.path(raw, 'linear_features', 'lfother.shp'), overwrite = TRUE)

gc()


## POLYGONAL ANTHRO DISTURBANCE 

#New poly NWT Data
nt.minesbuild <- st_read(file.path(raw, 'NWT_disturb', 'mining_buildings.gpkg'))
nt.minesclear <- st_read(file.path(raw, 'NWT_disturb', 'mining_clearings.gpkg'))
nt.mines <- st_read(file.path(raw, 'NWT_disturb', 'mining_mining.gpkg'))
nt.wells <- st_read(file.path(raw, 'NWT_disturb', 'oilgas_wellsites.gpkg'))
nt.airstrip <- st_read(file.path(raw, 'NWT_disturb', 'transportation_airstrip.gpkg'))
nt.urbuilds <- st_read(file.path(raw, 'NWT_disturb', 'urbanrural_buildings.gpkg'))
nt.urrec <- st_read(file.path(raw, 'NWT_disturb', 'urbanrural_recreation.gpkg'))

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

nt.otherpolys <- union(nt.minesbuild.proj, nt.minesclear)
nt.otherpolys <- union(nt.otherpolys, nt.wells.proj)
nt.otherpolys <- union(nt.otherpolys, nt.airstrip.proj)
nt.otherpolys <- union(nt.otherpolys, nt.urbuilds.proj)
nt.otherpolys <- union(nt.otherpolys, nt.urrec.proj)

#yt.poly.proj <- project(vect(yt.poly), crs)
miningpolys.yt <- project(vect(dplyr::filter(yt.poly, RC_Dstr %in% c('Quartz Mining', 'Placer Mining'))), crs)
miningpolys.yt$feature_type <- ifelse(miningpolys.yt$RC_Dstr == "Placer Mining", "placer", "quartz")
#Add year in there
head(miningpolys.yt)
table(miningpolys.yt$yr_strt)
df <- as.data.frame(miningpolys.yt)
df$year <- as.numeric(df$yr_strt)
df$year[is.na(df$year)] <- 2012
df$year[df$year < 2012] <- 2012
values(miningpolys.yt) <- df
table(miningpolys.yt$year)

#Add all mining poly's together
miningpolys.all <- rbind(miningpolys.yt, nt.mines, bc.mines)
table(miningpolys.all$year)
table(miningpolys.all$feature_type)
writeVector(miningpolys.all, file.path(raw, 'polygonal_features', 'miningpolys.shp'), overwrite = T)
writeVector(miningpolys.yt, file.path(raw, 'polygonal_features', 'mining_nmc_yt.shp'), overwrite = T)

yt.otherpolys <- project(vect(dplyr::filter(yt.poly, !(RC_Dstr %in% c('Quartz Mining', 'Placer Mining',
                                                                         'Harvest')))), crs)

anthropolys.all <- rbind(yt.otherpolys, nt.otherpolys, bc.polys)
anthropolys.all$feature_type <- "otherpoly"
head(anthropolys.all)
table(anthropolys.all$IMAGE_D)
df <- as.data.frame(anthropolys.all)
df$year_road <- as.numeric(df$yr_strt)
df$year_start <- as.numeric(format(as.Date(df$OPERATIONS_EFFECTIVE_DATE, format = "%Y-%m-%d"), "%Y"))
df$year_datemin <- as.numeric(format(as.Date(df$PROJECT_ESTABLISHED_DATE, format = "%Y-%m-%d"), "%Y"))
df$year_image <- as.numeric(format(as.Date(df$IMAGE_D, format = "%Y-%m-%d"), "%Y"))
df$year <- dplyr::coalesce(df$year_road, df$year_start, df$year_datemin, df$year_image)
df <- df[, !(names(df) %in% c("year_road", "year_start", "year_datemin", "year_image"))]
df$year[df$year < 2012] <- 2012
df$year[is.na(df$year)] <- 2012 #Assign 2012 to ones with no data
values(anthropolys.all) <- df
table(anthropolys.all$year)

writeVector(anthropolys.all, file.path(raw, 'polygonal_features', 'anthropolys.shp'), overwrite = T)


unpaved <- vect(file.path(raw, 'linear_features', 'roadunpaved.shp'))
paved <- vect(file.path(raw, 'linear_features', 'roadpaved.shp'))
roads <- union(paved, unpaved)
otherpoly <- vect(file.path(raw, 'polygonal_features', 'anthropolys.shp'))
miningpoly <- vect(file.path(raw, 'polygonal_features', 'miningpolys.shp'))
anthro.all <- c(roads, otherpoly)
lfother <- vect(file.path(raw, 'linear_features', 'lfother.shp'))


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
line_sf <- vect(file.path(derived, "combined_linear_features.shp"))

#Looking good! Phew! Now combine all poly's

table(miningpoly$year)
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
#For all of the below, I set the code up so that features persist into future years (2012 road will be on 2013 raster)
# Reproject to match base raster
poly_sf <- terra::project(poly_sf, crs(rasterToMatch_extendedLandscape30m))
line_sf <- terra::project(line_sf, crs(rasterToMatch_extendedLandscape30m))

#Files too large leading to memory crashes - taking only the necessary columns
poly_sf <- poly_sf[, c("feature_t0", "year")]
line_sf <- line_sf[, c("feature_t0", "year")]

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
    
 
    out_file_250m   <- file.path("data", "derived", "dist_disturb_250m",  paste0(cat, "_distance_250m_", year, ".tif"))
    out_file_1000m  <- file.path("data", "derived", "dist_disturb_1000m", paste0(cat, "_distance_1000m_", year, ".tif"))
    out_file_5000m  <- file.path("data", "derived", "dist_disturb_5000m", paste0(cat, "_distance_5000m_", year, ".tif"))
    
    if (file.exists(out_file_250m) && file.exists(out_file_1000m) && file.exists(out_file_5000m)) {
      message("  Skipping (already done)")
      next
    }
    
    if (cat %in% line_categories) {
      feat <- line_sf[line_sf$feature_t0 == cat & line_sf$year <= year, ]
    } else {
      feat <- poly_sf[poly_sf$feature_t0 == cat & poly_sf$year <= year, ]
    }
    
    
    temp_vec_file <- paste0("temp_", cat, "_", year, ".gpkg")
    writeVector(feat, temp_vec_file, overwrite = TRUE)
    feat <- vect(temp_vec_file)
    
    bin_ras_30m <- terra::rasterize(feat, rasterToMatch_extendedLandscape30m, touches = TRUE)
    bin_ras_30m[!is.na(bin_ras_30m)] <- 1
    rm(feat)
    gc()
    
    dist_ras_30m <- terra::distance(bin_ras_30m)
    rm(bin_ras_30m)
    gc()
    
    if (!file.exists(out_file_250m)) {
      message("  Writing 250m raster...")
      dist_ras_250m <- terra::resample(dist_ras_30m, rasterToMatch_extendedLandscape250m, method = "average")
      writeRaster(dist_ras_250m, out_file_250m, filetype = "GTiff", overwrite = TRUE)
      rm(dist_ras_250m)
      gc()
    } else {
      message("  Skipping 250m raster — already exists.")
    }
    
    if (!file.exists(out_file_1000m)) {
      message("  Writing 1000m raster...")
      dist_ras_1000m <- terra::resample(dist_ras_30m, rasterToMatch_extendedLandscape1000m, method = "average")
      writeRaster(dist_ras_1000m, out_file_1000m, filetype = "GTiff", overwrite = TRUE)
      rm(dist_ras_1000m)
      gc()
    } else {
      message("  Skipping 1000m raster — already exists.")
    }
    
    if (!file.exists(out_file_5000m)) {
      message("  Writing 5000m raster...")
      dist_ras_5000m <- terra::resample(dist_ras_30m, rasterToMatch_extendedLandscape5000m, method = "average")
      writeRaster(dist_ras_5000m, out_file_5000m, filetype = "GTiff", overwrite = TRUE)
      rm(dist_ras_5000m)
      gc()
    } else {
      message("  Skipping 5000m raster — already exists.")
    }
    
    rm(dist_ras_30m)
    unlink(temp_vec_file)
    terra::tmpFiles(remove = TRUE)
    gc()
  }
}


# ---- PRESENCE/ABSENCE ------

for (cat in c(line_categories, poly_categories)) {
  message("======= Processing presence/absence for: ", cat, " =======")
  
  for (year in years) {
    message("→ Year ", year)
    
    out_file_250m   <- file.path("data", "derived", "pa_disturb_250m",  paste0(cat, "_pa_250m_", year, ".tif"))
    out_file_1000m  <- file.path("data", "derived", "pa_disturb_1000m", paste0(cat, "_pa_1000m_", year, ".tif"))
    out_file_5000m  <- file.path("data", "derived", "pa_disturb_5000m", paste0(cat, "_pa_5000m_", year, ".tif"))
    
    if (file.exists(out_file_250m) && file.exists(out_file_1000m) && file.exists(out_file_5000m)) {
      message("  Skipping (already done)")
      next
    }
    
    if (cat %in% line_categories) {
      feat <- line_sf[line_sf$feature_t0 == cat & line_sf$year <= year, ]
    } else {
      feat <- poly_sf[poly_sf$feature_t0 == cat & poly_sf$year <= year, ]
    }
    
    temp_vec_file <- paste0("temp_", cat, "_", year, ".gpkg")
    writeVector(feat, temp_vec_file, overwrite = TRUE)
    feat <- vect(temp_vec_file)
    
    bin_ras_30m <- terra::rasterize(feat, rasterToMatch_extendedLandscape30m, touches = TRUE)
    bin_ras_30m[!is.na(bin_ras_30m)] <- 1
    bin_ras_30m[is.na(bin_ras_30m)] <- 0  # Make 0 = absent
    rm(feat)
    gc()

    if (!file.exists(out_file_250m)) {
      bin_250 <- terra::resample(bin_ras_30m, rasterToMatch_extendedLandscape250m, method = "max")
      bin_250 <- round(bin_250)  # Round to 0 or 1
      bin_250[is.na(bin_250)] <- 0
      writeRaster(bin_250, out_file_250m, filetype = "GTiff", overwrite = TRUE)
      rm(bin_250)
      gc()
    } else {
      message("  Skipping 250m raster — already exists.")
    }
    
    if (!file.exists(out_file_1000m)) {
      bin_1000 <- terra::resample(bin_ras_30m, rasterToMatch_extendedLandscape1000m, method = "max")
      bin_1000 <- round(bin_1000)
      bin_1000[is.na(bin_1000)] <- 0
      writeRaster(bin_1000, out_file_1000m, filetype = "GTiff", overwrite = TRUE)
      rm(bin_1000)
      gc()
    } else {
      message("  Skipping 1000m raster — already exists.")
    }

    if (!file.exists(out_file_5000m)) {
      bin_5000 <- terra::resample(bin_ras_30m, rasterToMatch_extendedLandscape5000m, method = "max")
      bin_5000 <- round(bin_5000)
      bin_5000[is.na(bin_5000)] <- 0
      writeRaster(bin_5000, out_file_5000m, filetype = "GTiff", overwrite = TRUE)
      rm(bin_5000)
      gc()
    } else {
      message("  Skipping 5000m raster — already exists.")
    }
    
    rm(bin_ras_30m)
    unlink(temp_vec_file)
    terra::tmpFiles(remove = TRUE)
    gc()
  }
}


# ---- DENSITY (FOR LF ONLY) ------ (basically avg of p/a rasters instead of max)

for (cat in c(line_categories)) {
  message("======= Processing density for: ", cat, " =======")
  
  for (year in years) {
    message("→ Year ", year)
    
    out_file_250m   <- file.path("data", "derived", "dens_disturb_250m",  paste0(cat, "_dens_250m_", year, ".tif"))
    out_file_1000m  <- file.path("data", "derived", "dens_disturb_1000m", paste0(cat, "_dens_1000m_", year, ".tif"))
    out_file_5000m  <- file.path("data", "derived", "dens_disturb_5000m", paste0(cat, "_dens_5000m_", year, ".tif"))
    
    if (file.exists(out_file_250m) && file.exists(out_file_1000m) && file.exists(out_file_5000m)) {
      message("  Skipping (already done)")
      next
    }
    
    if (cat %in% line_categories) {
      feat <- line_sf[line_sf$feature_t0 == cat & line_sf$year <= year, ]
    } else {
      feat <- poly_sf[poly_sf$feature_t0 == cat & poly_sf$year <= year, ]
    }
    
    
    temp_vec_file <- paste0("temp_", cat, "_", year, ".gpkg")
    writeVector(feat, temp_vec_file, overwrite = TRUE)
    feat <- vect(temp_vec_file)
    
    bin_ras_30m <- terra::rasterize(feat, rasterToMatch_extendedLandscape30m, touches = TRUE)
    bin_ras_30m[!is.na(bin_ras_30m)] <- 1
    bin_ras_30m[is.na(bin_ras_30m)] <- 0  # Make 0 = absent
    rm(feat)
    gc()
    
    if (!file.exists(out_file_250m)) {
      bin_250 <- terra::resample(bin_ras_30m, rasterToMatch_extendedLandscape250m, method = "average")
      bin_250[is.na(bin_250)] <- 0
      writeRaster(bin_250, out_file_250m, filetype = "GTiff", overwrite = TRUE)
      rm(bin_250)
      gc()
    } else {
      message("  Skipping 250m raster — already exists.")
    }
    
    if (!file.exists(out_file_1000m)) {
      bin_1000 <- terra::resample(bin_ras_30m, rasterToMatch_extendedLandscape1000m, method = "average")
      bin_1000[is.na(bin_1000)] <- 0
      writeRaster(bin_1000, out_file_1000m, filetype = "GTiff", overwrite = TRUE)
      rm(bin_1000)
      gc()
    } else {
      message("  Skipping 1000m raster — already exists.")
    }
    
    if (!file.exists(out_file_5000m)) {
      bin_5000 <- terra::resample(bin_ras_30m, rasterToMatch_extendedLandscape5000m, method = "average")
      bin_5000[is.na(bin_5000)] <- 0
      writeRaster(bin_5000, out_file_5000m, filetype = "GTiff", overwrite = TRUE)
      rm(bin_5000)
      gc()
    } else {
      message("  Skipping 5000m raster — already exists.")
    }
    
    rm(bin_ras_30m)
    unlink(temp_vec_file)
    terra::tmpFiles(remove = TRUE)
    gc()
  }
}



## FIRES ----

#NBACURL <- "https://cwfis.cfs.nrcan.gc.ca/downloads/nbac/nbac_1972_2023_20240530_shp.zip"
#NFDBURL <- "https://cwfis.cfs.nrcan.gc.ca/datamart/download/nfdbpoly/NFDB_poly.zip"



## FIRES #####
# combine nbac (which seems to be more error checked) with older fires from NFDB
# TODO replace with URL once we need to update data
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


writeVector(nmc.fires.all, file.path(raw, 'fires', 'fires_1920_2023.shp'), overwrite = T)
#fires_1920_2023 <- vect(file.path(raw, "fires", "fires_1920_2023.shp"))

fires <- as.data.frame(fires_1920_2023)

#Clean up years. Use info from fire_ID if available, otherwise set to NA
library(stringr)

fires_clean <- fires_1920_2023

fire_ids <- as.character(fires_clean$FIRE_ID)
year_guess <- str_extract(fire_ids, "^19[0-9]{2}")

invalid_years <- fires_clean$YEAR %in% c(-9999, -2147483648)

fires_clean$YEAR[invalid_years] <- ifelse(!is.na(year_guess[invalid_years]),
                                          as.integer(year_guess[invalid_years]),
                                          NA)
writeVector(fires_clean, file.path(canada, "fires_1920_2023_cleaned.shp"), overwrite = TRUE)
#plot(fires_clean)
#fires <- as.data.frame(fires_clean)

#Resolution rasterization
fire250m <- reproducible::prepInputs(
  targetFile = "fires_1920_2023_cleaned.shp",
  destinationPath = canada,
  fun = "terra::vect",
  to = rasterToMatch_extendedLandscape250m
) |> Cache()

writeVector(fire250m, file.path(derived, "fires_1920_2023_250m.shp"))


fire1000m <- reproducible::prepInputs(
  targetFile = "fires_1920_2023_cleaned.shp",
  destinationPath = canada,
  fun = "terra::vect",
  to = rasterToMatch_extendedLandscape1000m
) |> Cache()

writeVector(fire1000m, file.path(derived, "fires_1920_2023_1000m.shp"))


fire5000m <- reproducible::prepInputs(
  targetFile = "fires_1920_2023_cleaned.shp",
  destinationPath = canada,
  fun = "terra::vect",
  to = rasterToMatch_extendedLandscape5000m
) |> Cache()

writeVector(fire5000m, file.path(derived, "fires_1920_2023_5000m.shp"))


#source("C:/Users/peteb/NMC_CEtool_PB/R/make_fire_rast.R")
gc()

dir.create(file.path(derived, "fireRasters250m"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(derived, "fireRasters1000m"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(derived, "fireRasters5000m"), recursive = TRUE, showWarnings = FALSE)

make_fire_rast(
  layer = file.path(derived, "fires_1920_2023_250m.shp"),
  rast = rasterToMatch_extendedLandscape250m,
  minyr = 2012,
  where2save = file.path(derived, "fireRasters250m")
)

make_fire_rast(
  layer = file.path(derived, "fires_1920_2023_1000m.shp"),
  rast = rasterToMatch_extendedLandscape1000m,
  minyr = 2012,
  where2save = file.path(derived, "fireRasters1000m")
)

make_fire_rast(
  layer = file.path(derived, "fires_1920_2023_5000m.shp"),
  rast = rasterToMatch_extendedLandscape5000m,
  minyr = 2012,
  where2save = file.path(derived, "fireRasters5000m")
)


#### Forest age #####
forageURL <- "https://opendata.nfis.org/downloads/forest_change/CA_forest_age_2019.zip"

# Had to manually unzip the file to get tiff, for some reason.
unzip(file.path(raw, "CA_forest_age_2019.zip"), exdir = file.path(raw, "CA_forest_age_2019_unzipped"))
unzipped_dir <- file.path(raw, "CA_forest_age_2019_unzipped")
tif <- "CA_forest_age_2019.tif"

forage_raster <- prepInputs(
  archive = file.path(raw, "CA_forest_age_2019.zip"),
  targetFile = tif,
  destinationPath = unzipped_dir,
  fun = terra::rast,
  method = "near"
) |> Cache()

forage30m <- terra::project(forage_raster, rasterToMatch_extendedLandscape30m, method = "near")
forage30m[forage30m==0]<-NA
writeRaster(forage30m, file.path(derived, 'nmc_forest_age30.tif'))
#forage30m <- rast(file.path(derived, 'nmc_forest_age30.tif'))
#plot(forage30m)

#Scale up

forage_raster_scaleup <- prepInputs(
  archive = file.path(raw, "CA_forest_age_2019.zip"),
  targetFile = tif,
  destinationPath = unzipped_dir,
  fun = terra::rast,
  method = "average"
) |> Cache()

forage250m <- terra::project(forage_raster_scaleup, rasterToMatch_extendedLandscape250m, method = "average")
forage250m[forage250m==0]<-NA
writeRaster(forage250m, file.path(derived, 'nmc_forest_age_250.tif'))
#forage250m <- rast(file.path(derived, 'nmc_forest_age_250.tif'))
#plot(forage250m)

forage1000m <- terra::project(forage_raster_scaleup, rasterToMatch_extendedLandscape1000m, method = "average")
forage1000m[forage1000m==0]<-NA
writeRaster(forage1000m, file.path(derived, 'nmc_forest_age_1000.tif'))
#forage1000m <- rast(file.path(derived, 'nmc_forest_age_1000.tif'))
#plot(forage1000m)

forage5000m <- terra::project(forage_raster_scaleup, rasterToMatch_extendedLandscape5000m, method = "average")
forage5000m[forage5000m==0]<-NA
writeRaster(forage5000m, file.path(derived, 'nmc_forest_age_5000.tif'))
#forage5000m <- rast(file.path(derived, 'nmc_forest_age_5000.tif'))
#plot(forage5000m)

#Make rasters per year per res
years <- 2012:2024

ages <- list(
  age250 = forage250m,
  age1000 = forage1000m,
  age5000 = forage5000m
)

for (res in names(ages)) {
  age2019 <- ages[[res]]
  
  res_outdir <- file.path(raw, res)
  dir.create(res_outdir, recursive = TRUE, showWarnings = FALSE)
  
  for (yr in years) {
    delta_years <- 2019 - yr
    age_year <- age2019 - delta_years
    age_year[age_year < 0] <- NA  
    
    outname <- file.path(res_outdir, paste0("forest_age_", yr, ".tif"))
    writeRaster(age_year, outname, overwrite = TRUE)
  }
}

#Checking
#forage250_2012 <- rast(file.path(raw, 'age250', 'forest_age_2012.tif'))
#plot(forage250_2012)

#forage250_2024 <- rast(file.path(raw, 'age250', 'forest_age_2024.tif'))
#plot(forage250_2024)

#forage5000_2012 <- rast(file.path(raw, 'age5000', 'forest_age_2012.tif'))
#plot(forage5000_2012)




#Stuff for making plots
# Check CRSs
old_lf <- vect("C:/Users/peteb/OneDrive/Desktop/old_lf_combined.gpkg")
crs(old_lf)
crs(line_sf)

old_lf_proj <- project(old_lf, crs(line_sf))
old_lf_cropped <- crop(old_lf_proj, ext(line_sf))
old_lf_clipped <- intersect(old_lf_proj, line_sf)
terra::writeVector(old_lf_clipped, "C:/Users/peteb/OneDrive/Desktop/old_lf_clipped.shp", overwrite = TRUE)


old_poly <- vect("C:/Users/peteb/OneDrive/Desktop/old_poly_combined.gpkg")
old_poly_proj <- project(old_poly, crs(poly_sf))
old_poly_cropped <- crop(old_poly_proj, ext(poly_sf))
old_poly_clipped <- intersect(old_poly_proj, poly_sf)
terra::writeVector(old_poly_clipped, "C:/Users/peteb/OneDrive/Desktop/old_poly_clipped.shp", overwrite = TRUE)


boundary <- vect("C:/Users/peteb/OneDrive/Desktop/boundary.gpkg")

old_lf <- vect("C:/Users/peteb/OneDrive/Desktop/oldlfproper.gpkg")
old_poly <- vect("C:/Users/peteb/OneDrive/Desktop/oldpolyproper.gpkg")
old_paved <- vect("C:/Users/peteb/OneDrive/Desktop/roadpaved.shp")
old_unpaved <- vect("C:/Users/peteb/OneDrive/Desktop/roadunpaved.shp")
old_lfother <- vect("C:/Users/peteb/OneDrive/Desktop/lfother.shp")
old_mining <- vect("C:/Users/peteb/OneDrive/Desktop/miningpolys.shp")
old_anthropoly <- vect("C:/Users/peteb/OneDrive/Desktop/anthropolys.shp")

crs(boundary)
crs(old_paved)
boundary <- project(boundary, crs(old_paved))

pavedclipped <- intersect(old_paved, boundary)
unpavedclipped <- intersect(old_unpaved, boundary)
lfotherclipped <- intersect(old_lfother, boundary)
miningclipped <- intersect(old_mining, boundary)
anthroclipped <- intersect(old_anthropoly, boundary)

head(miningclipped)
placer_mining <- miningclipped[miningclipped$RC_Disturb == "Placer Mining", ]
quartz_mining <- miningclipped[miningclipped$RC_Disturb == "Quartz Mining", ]

