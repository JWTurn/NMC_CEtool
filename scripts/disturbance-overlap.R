### Find Caribou-disturbance overlap ====
# Julie Turner
# Started: February 2025

require('data.table')
require('amt')
require('dplyr')
require('tidyr')
require('sf')
require(ggplot2)
require(terra)

### Input data ----
raw <- file.path('data','raw')
derived <- file.path('data','derived')


# dat <- readRDS(file.path(derived, 'clean_NMC.RDS'))
# sArea <- file.path(derived, 'prepped', 'clean_NMC_20kmBuff.shp')

nmc.coords <- st_read(file.path(derived, "prepped", 'clean_NMC.shp'))
anthropolys <- st_read(file.path('data', 'raw', 'polygonal_features', 'anthropolys.shp'))
miningpolys <- st_read(file.path('data', 'raw', 'polygonal_features', 'miningpolys.shp'))
lf_paved<- st_read(file.path('data', 'raw', 'linear_features', 'roadpaved.shp'))
lf_unpaved <- st_read(file.path('data', 'raw', 'linear_features', 'roadunpaved.shp'))
lf_other <- st_read(file.path('data', 'raw', 'linear_features', 'lfother.shp'))


### 20 km buffer around points to get an idea of extent of study area
studyArea <- st_buffer(nmc.coords, dist = 20000)


mine.intersect <- st_filter(studyArea, miningpolys)
mine.union <- st_union(mine.intersect)

otherpolys.intersect <- st_filter(studyArea, anthropolys)
otherpolys.union <- st_union(otherpolys.intersect)

lf_paved.intersect <- st_filter(studyArea, lf_paved)
lf_paved.union <- st_union(lf_paved.intersect)

lf_unpaved.intersect <- st_filter(studyArea, lf_unpaved)
lf_unpaved.union <- st_union(lf_unpaved.intersect)

lf_other.intersect <- st_filter(studyArea, lf_other)
lf_other.union <- st_union(lf_other.intersect)

st_write(mine.union, file.path('data', 'disturbance_overlap', 'caribou_mines_overlap.shp'))
st_write(otherpolys.union, file.path('data', 'disturbance_overlap', 'caribou_otherpolys_overlap.shp'))
st_write(lf_paved.union, file.path('data', 'disturbance_overlap', 'caribou_lfpaved_overlap.shp'))
st_write(lf_unpaved.union, file.path('data', 'disturbance_overlap', 'caribou_lfunpaved_overlap.shp'))
st_write(lf_other.union, file.path('data', 'disturbance_overlap', 'caribou_lfother_overlap.shp'))
