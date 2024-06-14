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
#require(qgisprocess)

# list of all qgis algorithms
#algs <- qgis_algorithms()

lapply(dir('R', '*.R', full.names = TRUE), source)

### Input data ----
raw <- file.path('data','raw')
derived <- file.path('data','derived')

#canada <- file.path('C:','Users', 'julie', 'Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')
canada <- file.path('~/Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')
# roads.bc <- file.path(canada, 'CA_rd_data', 'canvec', 'canvec_50K_BC_Transport_shp', 'canvec_50K_BC_Transport')
# roads.yt <- file.path(canada, 'CA_rd_data', 'canvec', 'canvec_50K_YT_Transport_shp', 'canvec_50K_YT_Transport')
roads.bc <- file.path(raw, 'disturbances', 'bc', 'BC_CEF_Integrated_Roads_2021.gdb', 'BC_CE_Integrated_Roads_2021_20210805.gdb')
roads.yt <- file.path(raw, 'disturbances', 'yukon', 'Surface_Disturbance_Linear_Features.shp', 'Surface_Disturbance_Linear_Features.shp')
roads.nt <- file.path(canada, 'CA_rd_data', 'canvec', 'canvec_50K_NT_Transport_shp', 'canvec_50K_NT_Transport')

polys.bc <- file.path(raw, 'disturbances', 'bc', 'BC_CEF_Human_Disturbance_2021.gdb', 'BC_CEF_Human_Disturbance_2021.gdb')
polys.yt <- file.path(raw, 'disturbances', 'yukon', 'Surface_Disturbance_Areal_Features.shp', 'Surface_Disturbance_Areal_Features.shp')

resmgmt <- file.path(canada, 'Canvec-ResourceManagement', 'canvec_50K_CA_Res_MGT_shp', 'canvec_50K_CA_Res_MGT')

dat <- readRDS(file.path(derived, 'clean_NMC.RDS'))
sArea <- file.path(derived, 'prepped', 'clean_NMC_20kmBuff.shp')
studyPts <- vect(sArea)
herds <- vect(file.path(raw, 'NMC_herds', 'nmc_herds_all.shp'))
plot(herds)
# neeed 50km more east and 120 km more south
herd.ext <- ext(herds)
# copy herd ext and adjust east and south bounds (xmin, xmax, ymin, ymax)
herds.plus <- ext(c(407570.036466947, 1608648.39373763 + 50000, 6325864.05963461 - 120000, 7312174.16133225))
studyArea <- crop(studyPts, herds.plus)
plot(studyArea)
writeVector(studyArea, file.path(derived, 'prepped', 'clean_NMC_20kmBuff_crop.shp'))

## PREP ----
crs <- st_crs(3154)$wkt
maxyr <- lubridate::year(max(dat$t_, na.rm = T))
minyr <- lubridate::year(min(dat$t_, na.rm = T))
buff.diam <- 700

## LAND ----
# make a static cropped landcover for plotting purposes
yrs = 2019
land <- rast(file.path(canada, 'Landcover_1984-2019', paste0('CA_forest_VLCE2_', yrs), 
                  paste0('CA_forest_VLCE2_', yrs, '.tif')))
land.proj <- project(land, crs, method = 'near')
land.crop <- crop(land.proj, sArea)
writeRaster(land.crop, file.path(raw, 'nmc_landcover_2019.tif'))
# median SL 453, so buffer rounded down -- this was before I fixed a couple things in data cleaning
# median sl w/ 13hr fix rate 716, round down to 700
# median SL for all data not rareified 453.0335 
# median SL w/ 8hr fix rate 360.3391
# 2019 is still the last year of landcover data
make_landforest_prop(studyArea = sArea, crs, buff = buff.diam, startyr = minyr, endyr = 2019)

# template for future rasterizations
land <- file.path(raw, 'prop_land', '700', 'prop_needleleaf_2012.tif')

## ROADS ----
### jurisdictional data ----
bc.roads.layers <- st_layers(roads.bc)
bc.roads <- st_read(roads.bc, layer = 'integrated_roads_2021')
unique(as.factor(bc.roads$DRA_ROAD_CLASS))
unique(as.factor(bc.roads$DRA_ROAD_SURFACE))

bc.disturb.layers <- st_layers(polys.bc)
bc.disturb <- st_read(polys.bc)
bc.disturb.rail <- filter(bc.disturb, CEF_DISTURB_GROUP_RANK %in% c(2) &
                          CEF_DISTURB_SUB_GROUP_RANK %in% c(1, 2))


bc.disturb.lf <- filter(bc.disturb, CEF_DISTURB_GROUP_RANK %in% c(3, 4, 5, 8) &
                          CEF_DISTURB_SUB_GROUP_RANK %in% c(1, 2))
bc.disturb.oil <- filter(bc.disturb, CEF_DISTURB_GROUP_RANK %in% c(3, 8) &
                          CEF_DISTURB_SUB_GROUP_RANK %in% c(1))


plot(bc.disturb.lf$Shape)
bc.disturb.lf

yt.lf <- st_read(roads.yt)
yt.lf.ls <- summary(as.factor(yt.lf$DISTURB))

yt.poly <- st_read(polys.yt)
yt.poly.ls <- list(unique(yt.poly$DISTURB))
summary(as.factor(yt.poly$DISTURB))
yt.poly.ls

############
### BC ----
roadseg.bc <- file.path(roads.bc, 'road_segment_1.shp')
trackseg.bc <- file.path(roads.bc, 'track_segment_1.shp')
trail.bc <- file.path(roads.bc, 'trail_1.shp')

### YT ----
roadseg.yt <- file.path(roads.yt, 'road_segment_1.shp')
trackseg.yt <- file.path(roads.yt, 'track_segment_1.shp')
trail.yt <- file.path(roads.yt, 'trail_1.shp')

### NT ----
# including for those near border
# don't know what to do about the border of Alaska...
roadseg.nt <- file.path(roads.nt, 'road_segment_1.shp')
trackseg.nt <- file.path(roads.nt, 'track_segment_1.shp')
trail.nt <- file.path(roads.nt, 'trail_1.shp')

### ALL ----
# use qgis algorithms to merge, then turn into `sf` object
qgis_vectmerge <- qgis_function("native:mergevectorlayers")

roadseg.qgis <- qgis_vectmerge(LAYERS = list(roadseg.bc, roadseg.yt, roadseg.nt))
roadseg.all <- st_as_sf(roadseg.qgis)

trackseg.qgis <- qgis_vectmerge(LAYERS = list(trackseg.bc, trackseg.yt, trackseg.nt))
trackseg.all <- st_as_sf(trackseg.qgis)

trail.qgis <- qgis_vectmerge(LAYERS = list(trail.bc, trail.yt, trail.nt))
trail.all <- st_as_sf(trail.qgis)


summary(as.factor(roadseg.all$surfdsc_en))
# Dirt       Flexible         Gravel Not Identified          Rigid 
# 84           4115           4065         273444             72 
summary(as.factor(roadseg.all$ispaved_en))

summary(as.factor(trackseg.all$classif_en))
summary(as.factor(trackseg.all$mode_en))
summary(as.factor(trackseg.all$use_en))

summary(as.factor(trail.all$map_sel_en))


gc()
# convert to crs
roadseg.proj <- st_transform(roadseg.all, crs)
trackseg.proj <- st_transform(trackseg.all, crs)
trail.proj <- st_transform(trail.all, crs)

# save these
st_write(roadseg.proj, file.path(raw, 'linear_features', 'roadseg.shp'), append = FALSE)
st_write(trackseg.proj, file.path(raw, 'linear_features', 'trackseg.shp'), append = FALSE)
st_write(trail.proj, file.path(raw, 'linear_features', 'trail.shp'), append = FALSE)


# separate paved from unpaved roads
paved.all <- dplyr::filter(roadseg.proj, ispaved_en == "True")
unpaved.all <- dplyr::filter(roadseg.proj, ispaved_en == "False")

st_write(paved.all, file.path(raw, 'linear_features', 'roadpaved.shp'), append = FALSE)
st_write(unpaved.all, file.path(raw, 'linear_features', 'roadunpaved.shp'), append = FALSE)

# combine paved/rail
## TODO THIS ISN'T WORKING
paved <- file.path(raw, 'linear_features', 'roadpaved.shp')
rail <- file.path(raw, 'linear_features', 'trackseg.shp')
roadrail.qgis <- qgis_vectmerge(LAYERS = list(paved, rail))
roadrail <- st_as_sf(roadrail.qgis)
st_write(roadrail, file.path(raw, 'linear_features', 'roadpaved_rail.shp'), append = FALSE)

# combine unpaved/trails
unpaved <- file.path(raw, 'linear_features', 'roadunpaved.shp')
trail <- file.path(raw, 'linear_features', 'trail.shp')
unpavedtrail.qgis <- qgis_vectmerge(LAYERS = list(unpaved, trail))
unpavedtrail <- st_as_sf(unpavedtrail.qgis)
st_write(unpavedtrail, file.path(raw, 'linear_features', 'roadunpaved_trail.shp'), append = FALSE)

## POLYGONAL ANTHRO DISTURBANCE - CANVEC RESOURCE MANAGMENT ----
aggr <- file.path(resmgmt, 'aggregate_2.shp') # poly
oilgas <- file.path(resmgmt, 'oil_gas_site_0.shp') # point
oilgas2 <- file.path(resmgmt, 'oil_gas_site_2.shp') # poly
ore <- file.path(resmgmt, 'ore_0.shp') # point
ore2 <- file.path(resmgmt, 'ore_2.shp') # poly
peat <- file.path(resmgmt, 'peat_2.shp') # poly
well <- file.path(resmgmt, 'petroleum_well_0.shp') # point

anthropolys_qgis <- qgis_vectmerge(LAYERS = list(aggr, oilgas2, ore2, peat))
anthropolys_sf <- st_as_sf(anthropolys_qgis) %>% st_transform(crs = crs)
anthropolys.nmc <- crop(vect(anthropolys_sf), studyArea)
writeVector(anthropolys.nmc, file.path(raw, 'anthroDisturbance', 'anthro_polys.shp'))

commline <- file.path(resmgmt, 'communication_line_1.shp')
pipeline <- file.path(resmgmt, 'pipeline_1.shp')
powerline <- file.path(resmgmt, 'power_line_1.shp')

anthrolines_qgis <- qgis_vectmerge(LAYERS = list(commline, pipeline, powerline))
anthrolines_sf <- st_as_sf(anthrolines_qgis) %>% st_transform(crs = crs)
anthrolines.nmc <- crop(vect(anthrolines_sf), studyArea)
writeVector(anthrolines.nmc, file.path(raw, 'anthroDisturbance', 'antro_lines.shp'))


# Polygonal disturbance Zone
gc()
Buff <- focalMat(rast(land), d=buff.diam, type = 'circle')
# rasterize polys
anthropolys.rast <- rasterize(anthropolys.nmc, rast(land))
anthropolys.rast[is.na(anthropolys.rast)]<-0
mean.anthropolys <- focal(anthropolys.rast, Buff, na.rm = TRUE, pad = TRUE, padValue = 0, fun = 'mean')
writeRaster(mean.anthropolys, file.path(raw, 'anthroDisturbance', 'antro_polys_prop700.tif'))


## HARVEST ----
harv <- rast(file.path(canada, 'CA_Forest_Harvest_1985-2020', 'CA_Forest_Harvest_1985-2020.tif'))
rland <- rast(land)
harv.proj <- project(harv, rland, method = 'near')
harv.crop <- crop(harv.proj, ext(studyArea))
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
# combine nbac (which seems to be more error checked) with older fires from NFDB
fires <- vect(file.path(canada, 'fire_nbac_1986_to_2022', 'nbac_1986_to_2022_20230630.shp'))
fires.proj <- project(fires, crs)
fires.old <- vect(file.path(canada, 'NFDB_poly', 'NFDB_poly_20210707.shp'))
fires.old.proj <- project(fires.old, crs)
fires.pre <- subset(fires.old.proj, fires.old.proj$YEAR<1986)
# crop to study area for computational ease
nmc.fires.proj <- crop(fires.proj, rland)
nmc.fires.pre <- crop(fires.pre, rland)
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

writeVector(nmc.fires.all, file.path(raw, 'fires', 'fires_1920_2022.shp'), overwrite = T)

# make rasters from polygonal fires by year
gc()

make_fire_rast(layer = file.path(raw, 'fires', 'fires_1920_2022.shp'),
               rast = land, minyr = minyr,
               where2save = file.path(raw, 'fires'))

