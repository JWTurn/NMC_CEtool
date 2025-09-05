### Download Data ====
# Julie Turner
# Started: July 14 2023

# Modified by Peter Baker April 23 2025
# Modified by Katie Florko Sept 4 2025
.libPaths()

# require(Require)
# Require('reproducible')
install.packages(move2)
install.packages(sf)
library(move2)
library(data.table)
library(sf)


require('move2')
require('data.table')
require('sf')

### Input data ----
raw <- file.path('data', 'raw')
derived <- file.path('data', 'derived')

# cPath <- file.path(tempdir(), "cache")

# YT DATA ----
# This will have to be run once if first time using `move2`
movebank_store_credentials("username", "password")

# study names to fill in loop
# had to go through names on movebank and accept terms for each...
dsNames <-c('Aishihik Caribou', 'Bonnet Plume caribou - Yukon Collars', 'Clear Creek caribou', 
            'Coal River caribou', 'Hart River caribou', 
            'Klaza caribou', 'Kluane caribou',
            'Little Rancheria caribou - B.C. collars NO SHARING', 'Little Rancheria caribou - Yukon collars',
            'Pelly Caribou Herds' ,'South Nahanni caribou', 'Southern Lakes caribou - Updated V4', 
            'Tay River caribou', 'Wolf Lake caribou') 

# aishihik <- movebank_download_study('Aishihik Caribou')
# clearcreek <- movebank_download_study('Clear Creek caribou')
# coalriver <- movebank_download_study('Coal River caribou')

# make a list of all data from Movebank
# TODO: update this with prepInputs()
# LAST downloaded Sept 2025

move <- list()
for (ds in 1:length(dsNames)) {
  #hh = 1
  move[[ds]]<-movebank_download_study(study =dsNames[[ds]], )
gc()
  }

saveRDS(move, file.path(raw, 'YTdata_DL.RDS'))
#move <- readRDS(file.path(raw, 'YTdata_DL.RDS'))

move.meta <- list()
for (ds in 1:length(dsNames)) {
  #hh = 1
  move.meta[[ds]]<-movebank_download_deployment(study =dsNames[[ds]])
}

saveRDS(move.meta, file.path(raw, 'YTmetadata_DL.RDS'))
#move.meta <- readRDS(file.path(raw, 'YTmetadata_DL.RDS'))

# pull just the data
hab <-c('Aishihik', 'BonnetPlume', 'ClearCreek', 
        'CoalRiver', 'HartRiver', 
        'Klaza', 'Kluane',
        'LittleRancheriaBC', 'LittleRancheriaYT',
        'Pelly', 'SouthNahanni', 'SouthernLakes',
        'TayRiver', 'WolfLake') 

# convert from long/lat to NAD83/Canada Atlas Lambert (need units to be m)
crs <- st_crs(4326)$wkt
outcrs <- st_crs(3979)
# can't get the projection right -> wait hold on to long/lat as long as possible
move.combo <- data.table()
# move.combo <- rbindlist(lapply(1:length(hab), function(hh){
#   move.combo[,.(area=hab[[hh]], dat=list(setDT(
#     sfheaders::sf_to_df(
#       st_transform(move[[hh]], outcrs), fill=T))))]
# })
# )

move.combo <- rbindlist(lapply(1:length(hab), function(hh){
  move.combo[,.(area=hab[[hh]], dat=list(setDT(
    sfheaders::sf_to_df(
      move[[hh]], fill=T))))]
}))

# move.combo <- rbindlist(lapply(1:length(hab), function(hh){
#   setDT(move[[hh]])[,area:=hab[[hh]]]
#   }), fill=T)

# gathering just the columns needed

move.expand <- rbindlist(lapply(1:length(hab), function(hh){
  move.combo$dat[[hh]][,area := hab[[hh]]]
  }), fill = T)

# gathering metadata
metadat <- rbindlist(move.meta, fill =T)

#Fixed to include Southern Lakes and Clear Creek where they had diff names for ID
move.clean <- move.expand[, .(
  area,
  id = fifelse(
    area %in% c("SouthernLakes", "ClearCreek") | is.na(individual_local_identifier),
    as.character(deployment_id),
    as.character(individual_local_identifier)
  ),
  gps_dop, gps_fix_type_raw, migration_stage,
  gps_satellite_count, gps_twilight, import_marked_outlier,
  manually_marked_outlier, visible,
  datetime = timestamp,
  longitude = x,
  latitude = y
)]

meta.clean <- metadat[, .(
  id = fifelse(
    study_site %in% c("Clear Creek", "Carcross", "Ibex", "Laberge") | is.na(individual_local_identifier),
    as.character(deployment_id),
    as.character(individual_local_identifier)
  ),
  deploy_on_timestamp,
  deploy_off_timestamp,
  duty_cycle,
  individual_number_of_deployments,
  deployment_end_type
)]

yt.clean <- merge(move.clean, meta.clean, by = 'id')
yt.clean[,`:=`(deploy_start = min(datetime), 
               deploy_end = max(datetime)), by = .(id)]

# save 'clean' data 
#saveRDS(yt.clean, paste0(derived, 'data_all_yt.RDS'))
#yt.clean <- readRDS(file.path(derived, 'data_all_yt.RDS'))

#unique(yt.clean$area)


# BC DATA ----
bc <- fread(file.path(raw, 'BC_NMC_Telemetry.csv'))

# convert from long/lat to NAD83/Canada Atlas Lambert (need units to be m)

# sfbc <- st_as_sf(bc, coords = c('longitude', 'latitude'),
#                   crs = crs)
# outbc <- st_transform(sfbc, outcrs)
# bc.trans <- setDT(sfheaders::sf_to_df(outbc, fill = T))
bc.trans <- bc
bc.trans[,`:=`(deploy_start = min(acquisition_date), 
                           deploy_end = max(acquisition_date)), by = .(wlh_id)]

bc.clean <- bc.trans[,.(prov = 'bc', area = population_unit, id=as.character(wlh_id), 
                        deployment_end_type = ifelse(isTRUE(mortality), dead, NA), 
                        deploy_start, deploy_end,
                        datetime = acquisition_date, longitude, latitude)]


#Data from Mike Noonan
bcmn <- fread(file.path(raw, 'nmc_combined_data_cleaned.csv'))

bcmn.trans <- bcmn
bcmn.trans[,`:=`(deploy_start = min(Acquisition_Date), 
               deploy_end = max(Acquisition_Date)), by = .(WLH_ID)]

#Need to change 0s to FALSE in mortality of Noonan data
bcmn.trans$mortality <- as.logical(bcmn.trans$mortality)

bcmn.clean <- bcmn.trans[,.(prov = 'bc', area = Pop_Unit, id=as.character(WLH_ID), 
                        deployment_end_type = ifelse(isTRUE(mortality), dead, NA), 
                        deploy_start, deploy_end,
                        datetime = Acquisition_Date, Longitude, Latitude)]

setnames(bcmn.clean, old = c("Latitude", "Longitude"), new = c("latitude", "longitude"))

# COMBO DATA ----
yt.clean <- readRDS(file.path(derived, 'data_all_yt.RDS'))
yt.clean[,prov := 'yt']

nmc.clean <- rbind(yt.clean, bc.clean, bcmn.clean, fill = T)

# save combo 'clean' data 
saveRDS(nmc.clean, file.path(derived, 'prepDat_nmc.RDS'))

