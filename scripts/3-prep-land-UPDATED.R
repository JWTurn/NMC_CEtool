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
roads.nt <- file.path(raw, 'CA_rd_data', 'canvec', 'canvec_50K_NT_Transport_shp', 'canvec_50K_NT_Transport')
resmgmt <- file.path(raw, 'Canvec-ResourceManagement', 'canvec_50K_CA_Res_MGT_shp', 'canvec_50K_CA_Res_MGT')

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
buffer <- 750

# List of rasters to match, with window sizes of 250m, 1000m, and 5000m
rtms <- list(
  rasterToMatch_extendedLandscape250m,
  rasterToMatch_extendedLandscape1000m,
  rasterToMatch_extendedLandscape5000m
)

names(rtms) <- c("window250", "agg1000", "agg5000")


# Define corresponding functions for each raster window size with dummy code to make each unique for Cache()
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

#Yukon
yt.lf <- st_read(roads.yt)
yt.lf.ls <- summary(as.factor(yt.lf$TYPE_DI))
yt.lf.ls
yt.poly <- st_read(polys.yt)
yt.poly.ls <- list(unique(yt.poly$RC_Dstr))
summary(as.factor(yt.poly$RC_Dstr))
yt.poly.ls

#NWT
roadseg.nt <- file.path(roads.nt, 'road_segment_1.shp')
trackseg.nt <- file.path(roads.nt, 'track_segment_1.shp')
trail.nt <- file.path(roads.nt, 'trail_1.shp')

# convert to crs
roadseg.proj <- project(vect(roadseg.nt), crs)
trackseg.proj <- project(vect(trackseg.nt), crs)
trail.proj <- project(vect(trail.nt), crs)

summary(as.factor(roadseg.proj$ispaved_en))
summary(as.factor(trackseg.proj$classif_en))
summary(as.factor(trackseg.proj$mode_en))
summary(as.factor(trackseg.proj$use_en))

paved.nt <- subset(roadseg.proj, roadseg.proj$ispaved_en == 'True')
nt.paved.rail <- union(trackseg.proj, paved.nt)
unpaved.nt <- subset(roadseg.proj, roadseg.proj$ispaved_en == 'False')

other.nt <- trail.proj

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
table(paved.rail.all$year)
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

# other LFs
commline <- project(vect(file.path(resmgmt, 'communication_line_1.shp')), crs)
pipeline <- project(vect(file.path(resmgmt, 'pipeline_1.shp')), crs)
powerline <- project(vect(file.path(resmgmt, 'power_line_1.shp')), crs)

# right now doing this NT where we dont have data
commline.crop <- crop(commline, nt)
pipeline.crop <- crop(pipeline, nt)
powerline.crop <- crop(powerline, nt)

resmgmt.lf <- union(union(union(commline.crop, pipeline.crop), powerline.crop))
lf.resmgmt.trail <- union(resmgmt.lf, other.nt)
other.yt <- project(vect(dplyr::filter(yt.lf, LF_TYPE == 'Other LF' & RECLS_D != 'Railway')), crs)
other.all <- rbind(other.yt, lf.resmgmt.trail, bc.other)
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

gc()


## POLYGONAL ANTHRO DISTURBANCE - CANVEC RESOURCE MANAGMENT ----
aggr <- vect(file.path(resmgmt, 'aggregate_2.shp')) # poly
#oilgas <- vect(file.path(resmgmt, 'oil_gas_site_0.shp')) # point
oilgas2 <- vect(file.path(resmgmt, 'oil_gas_site_2.shp')) # poly
#ore <- vect(file.path(resmgmt, 'ore_0.shp')) # point
ore2 <- vect(file.path(resmgmt, 'ore_2.shp')) # poly
peat <- vect(file.path(resmgmt, 'peat_2.shp')) # poly
#well <- vect(file.path(resmgmt, 'petroleum_well_0.shp')) # point

rsmgt.mines <- union(aggr, ore2)
nt.mines <- crop(project(rsmgt.mines, crs), nt)
nt.mines$feature_type <- "othermines"
rsmgt.otherpolys <- union(peat, oilgas2)
nt.otherpolys <- crop(project(rsmgt.otherpolys, crs), nt)

#yt.poly.proj <- project(vect(yt.poly), crs)
miningpolys.yt <- project(vect(dplyr::filter(yt.poly, RC_Dstr %in% c('Quartz Mining', 'Placer Mining'))), crs)
miningpolys.yt$feature_type <- ifelse(miningpolys.yt$RC_Dstr == "Placer Mining", "placer", "quartz")
miningpolys.all <- rbind(miningpolys.yt, nt.mines, bc.mines)
writeVector(miningpolys.all, file.path(raw, 'polygonal_features', 'miningpolys.shp'), overwrite = T)
writeVector(miningpolys.yt, file.path(raw, 'polygonal_features', 'mining_nmc_yt.shp'), overwrite = T)

yt.otherpolys <- project(vect(dplyr::filter(yt.poly, !(RC_Dstr %in% c('Quartz Mining', 'Placer Mining',
                                                                         'Harvest')))), crs)

anthropolys.all <- rbind(yt.otherpolys, nt.otherpolys, bc.polys)
anthropolys.all$feature_type <- "otherpoly"
writeVector(anthropolys.all, file.path(raw, 'polygonal_features', 'anthropolys.shp'), overwrite = T)


unpaved <- vect(file.path(raw, 'linear_features', 'roadunpaved.shp'))
paved <- vect(file.path(raw, 'linear_features', 'roadpaved.shp'))
roads <- union(paved, unpaved)
anthropolys <- vect(file.path(raw, 'polygonal_features', 'anthropolys.shp'))
anthro.all <- c(roads, anthropolys)


# STARTING RASTER CALCULATIONS


# Reproject to match base raster
poly_sf <- st_transform(poly_sf, crs(rasterToMatch_extendedLandscape30m))
line_sf <- st_transform(line_sf, crs(rasterToMatch_extendedLandscape30m))


# Define years, resolutions, categories
years <- 2012:2024
poly_categories <- c("quartz", "placer", "othermines", "otherpoly")
line_categories <- c("paved", "unpaved", "otherlf")


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
    dist_250 <- terra::resample(dist_ras, rasterToMatch_extendedLandscape250m, method = "average")
    dist_1000 <- terra::resample(dist_ras, rasterToMatch_extendedLandscape1000m, method = "average")
    dist_5000 <- terra::resample(dist_ras, rasterToMatch_extendedLandscape5000m, method = "average")
    
    # Save
    writeRaster(dist_250, file.path(derived, paste0(cat, "_distance_250m_", year, ".tif")), filetype = "GTiff", overwrite = TRUE)
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
    pa_250 <- terra::aggregate(bin_ras, fact = res(rasterToMatch_extendedLandscape250m) / res(rasterToMatch_extendedLandscape30m), fun = max, na.rm = TRUE)
    pa_1000 <- terra::aggregate(bin_ras, fact = res(rasterToMatch_extendedLandscape1000m) / res(rasterToMatch_extendedLandscape30m), fun = max, na.rm = TRUE)
    pa_5000 <- terra::aggregate(bin_ras, fact = res(rasterToMatch_extendedLandscape5000m) / res(rasterToMatch_extendedLandscape30m), fun = max, na.rm = TRUE)
    
    # Save rasters
    writeRaster(pa_250, file.path(derived, paste0(cat, "_presence_250m_", year, ".tif")), filetype = "GTiff", overwrite = TRUE)
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
    
    # Aggregate the binary presence/absence to the larger resolutions
    dens250 <- terra::aggregate(bin_ras, fact = res(rasterToMatch_extendedLandscape250m) / res(rasterToMatch_extendedLandscape30m), fun = mean, na.rm = TRUE)
    dens1000 <- terra::aggregate(bin_ras, fact = res(rasterToMatch_extendedLandscape1000m) / res(rasterToMatch_extendedLandscape30m), fun = mean, na.rm = TRUE)
    dens5000 <- terra::aggregate(bin_ras, fact = res(rasterToMatch_extendedLandscape5000m) / res(rasterToMatch_extendedLandscape30m), fun = mean, na.rm = TRUE)
    
    # Save the results
    writeRaster(dens250, file.path(derived, paste0(cat, "_roadPresenceAvg_250m_", year, ".tif")), filetype = "GTiff", overwrite = TRUE)
    writeRaster(dens1000, file.path(derived, paste0(cat, "_roadPresenceAvg_1000m_", year, ".tif")), filetype = "GTiff", overwrite = TRUE)
    writeRaster(dens5000, file.path(derived, paste0(cat, "_roadPresenceAvg_5000m_", year, ".tif")), filetype = "GTiff", overwrite = TRUE)
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