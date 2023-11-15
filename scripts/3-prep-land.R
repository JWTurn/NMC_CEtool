### Prepare Landcover data ====
# Julie Turner
# Started: November 02 2023

require('data.table')
require('amt')
require('dplyr')
require('tidyr')
require('sf')
require(ggplot2)
require(terra)
require(foreach)

lapply(dir('R', '*.R', full.names = TRUE), source)

### Input data ----
raw <- file.path('data','raw')
derived <- file.path('data','derived')

canada <- file.path('C:','Users', 'julie', 'Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')
#canada <- file.path('~/Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')

dat <- readRDS(file.path(derived, 'clean_NMC.RDS'))
sArea <- file.path(derived, 'prepped', 'clean_NMC_20kmBuff.shp')
studyArea <- vect(sArea)

## PREP ----
crs <- st_crs(3154)$wkt
maxyr <- lubridate::year(max(dat$t_, na.rm = T))
minyr <- lubridate::year(min(dat$t_, na.rm = T))
buff.diam <- 700

## LAND ----
# median SL 453, so buffer rounded down -- this was before I fixed a couple things in data cleaning
# median sl w/ 13hr fix rate 716, round down to 700
# median SL for all data not rareified 453.0335 
# median SL w/ 8hr fix rate 360.3391
# 2019 is still the last year of landcover data
make_landforest_prop(studyArea = sArea, crs, buff = buff.diam, startyr = minyr, endyr = 2019)

# template for future rasterizations
land <- file.path(raw, 'prop_land', '700', 'prop_mixed_2012.tif')

## ROADS ----

roads <- st_read(file.path('data', 'raw-data', 'wbi_nrn.shp'))
rail <- st_read(file.path('data', 'raw-data', 'wbi_rail.shp'))

paved <- dplyr::filter(roads, PAVSTATUS %in% 'Paved')
#writeVector(vect(paved), file.path('data', 'raw-data', 'wbi_pave_roads.shp'), overwrite = T)

## HARVEST ----
harv <- rast(file.path(canada, 'CA_Forest_Harvest_1985-2020', 'CA_Forest_Harvest_1985-2020.tif'))
harv <- project(harv, rast(land), method = 'near')
harv.crop <- crop(harv, ext(studyArea))
harv.crop[harv.crop==0]<-NA
writeRaster(harv.crop, file.path(raw, 'nmc_harv_1985-2020.tif'), overwrite =T)

## FOREST AGE ----
gc()
fage <- rast(file.path(canada, 'CA_forest_age_2019', 'CA_forest_age_2019.tif'))
fage <- project(fage, rast(land), method = 'near')
fage.crop <- crop(fage, ext(studyArea))
writeRaster(fage.crop, file.path(raw, 'nmc_forestage_2019.tif'), overwrite =T)

### MEAN FOREST AGE IN BUFFER ----
gc()
Buff <- focalMat(rast(land), d=buff.diam, type = 'circle')
mean.fage <- focal(fage.crop, Buff, na.rm = TRUE, pad = TRUE, padValue = 0, fun = 'mean')
writeRaster(mean.fage, file.path(raw, 'nmc_forestage_meanbuff700_2019.tif'), overwrite =T)

## FIRES #####
## this has been run already
fires <- vect(file.path(canada, 'fire_nbac_1986_to_2020', 'nbac_1986_to_2020_20210810.shp'))
fires.proj <- project(fires, crs)
writeVector(fires.proj, file.path(raw, 'fire_nbac_1986_to_2020', 'fires_1986_2020.shp'))

# make rasters from polygonal fires by year
gc()
make_fire_rast(file.path(canada, 'fire_nbac_1986_to_2020', 'nbac_1986_to_2020_20210810.shp'),
               land)

