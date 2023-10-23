### Prepare Data ====
# Julie Turner
# Started: July 14 2023

# require(Require)
# Require('reproducible')
require('move2')
require('data.table')
require('sf')

### Input data ----
raw <- 'data/raw/'
derived <- 'data/derived/'

# cPath <- file.path(tempdir(), "cache")

# YT DATA ----
# This will have to be run once if first time using `move2`
# movebank_store_credentials("myUserName", "myPassword")

# study names to fill in loop
# had to go through names on movebank and accept terms for each...
dsNames <-c('Aishihik Caribou', 'Clear Creek caribou', 
            'Coal River caribou', 'Hart River caribou', 
            'Klaza caribou', 'Kluane caribou',
            'Little Rancheria caribou - B.C. collars NO SHARING', 'Little Rancheria caribou - Yukon collars',
            'Nahanni caribou',# 'Southern Lakes caribou - Updated', # KB said this one is currently corrupted
            'Tay River caribou', 'Wolf Lake caribou') 

# aishihik <- movebank_download_study('Aishihik Caribou')
# clearcreek <- movebank_download_study('Clear Creek caribou')
# coalriver <- movebank_download_study('Coal River caribou')

# make a list of all data from Movebank
# TODO: update this with prepInputs()
# LAST Tried 20 OCT 2023 -- Ignored Southern Lakes because it's corrupted
move <- list()
for (ds in 1:length(dsNames)) {
  #hh = 1
  move[[ds]]<-movebank_download_study(study =dsNames[[ds]])
}
#saveRDS(move, file.path(raw, 'YTdata_DL.RDS'))
move <- readRDS(file.path(raw, 'YTdata_DL.RDS'))

move.meta <- list()
for (ds in 1:length(dsNames)) {
  #hh = 1
  move.meta[[ds]]<-movebank_download_deployment(study =dsNames[[ds]])
}
#saveRDS(move.meta, file.path(raw, 'YTmetadata_DL.RDS'))
move.meta <- readRDS(file.path(raw, 'YTmetadata_DL.RDS'))

# pull just the data
hab <-c('Aishihik', 'ClearCreek', 
        'CoalRiver', 'HartRiver', 
        'Klaza', 'Kluane',
        'LittleRancheriaBC', 'LittleRancheriaYT',
        'Nahanni',# 'SouthernLakes',
        'TayRiver', 'WolfLake') 

move.combo <- data.table()

# convert from long/lat to NAD83/Canada Atlas Lambert (need units to be m)
crs <- st_crs(4326)$wkt
outcrs <- st_crs(3979)

move.combo <- rbindlist(lapply(1:length(hab), function(hh){
  move.combo[,.(area=hab[[hh]], dat=list(setDT(
    sfheaders::sf_to_df(
      st_transform(move[[hh]], outcrs), fill=T))))]
})
)


# gathering just the columns needed

move.expand <- rbindlist(lapply(1:length(hab), function(hh){
  move.combo$dat[[hh]][,area := hab[[hh]]]
  }), fill = T)

# gathering metadata
metadat <- rbindlist(move.meta, fill =T)


move.clean <- move.expand[,.(area, id=as.character(individual_local_identifier), 
                   gps_dop, gps_fix_type_raw, migration_stage,
                   gps_satellite_count, gps_twilight, import_marked_outlier,
                   manually_marked_outlier, visible,
                   datetime = timestamp, x, y)]

meta.clean <- metadat[,.(id=as.character(individual_local_identifier), 
                         deploy_on_timestamp, deploy_off_timestamp, duty_cycle,
                         individual_number_of_deployments, deployment_end_type)]

yt.clean <- merge(move.clean, meta.clean, by = 'id')
yt.clean[,`:=`(deploy_start = min(datetime), 
               deploy_end = max(datetime)), by = .(id)]

# save 'clean' data 
# saveRDS(move.clean, paste0(derived, 'data_all_yt.RDS'))

# BC DATA ----
bc <- fread(file.path(raw, 'BC_NMC_Telemetry.csv'))

# convert from long/lat to NAD83/Canada Atlas Lambert (need units to be m)

sfbc <- st_as_sf(bc, coords = c('longitude', 'latitude'),
                  crs = crs)
outbc <- st_transform(sfbc, outcrs)
bc.trans <- setDT(sfheaders::sf_to_df(outbc, fill = T))
bc.trans[,`:=`(deploy_start = min(acquisition_date), 
                           deploy_end = max(acquisition_date)), by = .(wlh_id)]

bc.clean <- bc.trans[,.(prov = 'bc', area = population_unit, id=as.character(wlh_id), 
                        deployment_end_type = ifelse(isTRUE(mortality), dead, NA), 
                        deploy_start, deploy_end,
                        datetime = acquisition_date, x, y)]


# COMBO DATA ----
yt.clean <- readRDS(file.path(derived, 'data_all_yt.RDS'))
yt.clean[,prov := 'yt']

nmc.clean <- rbind(yt.clean, bc.clean, fill = T)

# save combo 'clean' data 
saveRDS(nmc.clean, file.path(derived, 'prepDat_nmc.RDS'))
