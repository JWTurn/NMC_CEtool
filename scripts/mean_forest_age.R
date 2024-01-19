# === Mean forest age -------------------------------------
# Julie Turner
# started 14 December 2023

#### Packages ####
libs <- c('Require', 'reproducible', 'data.table', 'terra','sf',
          'ggplot2', 'viridis', 'tidyterra', 'patchwork')
lapply(libs, Require::Require, character.only = TRUE)

### functions ----
geomean <- function(x){
  exp(mean(log(x[x>0]), na.rm = T))
}

### Input data ----
raw <-  file.path('data', 'raw')
derived <- file.path('data', 'derived')
canada <- file.path('~/Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')


### layers -----
fires <- rast(file.path(raw, 'fires', 'fires_2019.tif'))
harv <- rast(file.path(raw, 'nmc_harv_1985-2020.tif'))

studyArea <- vect(file.path(derived, 'prepped', 'clean_NMC_20kmBuff.shp'))

fage <- rast(file.path(canada, 'CA_forest_age_2019', 'CA_forest_age_2019.tif'))
fage <- project(fage, fires)
fage.crop <- crop(fage, fires)


fage.unk.fires <- mask(fage.crop, fires, inverse = T)
writeRaster(fage.unk.fires , file.path(raw, 'f_age-fires_2019.tif'), overwrite = T)

fage.unk.harv <- mask(fage.crop, harv, inverse = T)
writeRaster(fage.unk.fires.harv , file.path(raw, 'f_age-harv_2019.tif'))

fage.unk.fires.harv <- mask(fage.unk.fires, harv, inverse = T)
writeRaster(fage.unk.fires.harv , file.path(raw, 'f_age-fires-harv_2019.tif'))

global(fage.unk.fires.harv, fun=geomean)
# global
# CA_forest_age_2019 75.58803
