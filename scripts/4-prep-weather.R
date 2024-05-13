### Prepare Climate/weather data ====
# Julie Turner
# Started: May 02 2024

require('data.table')
require('amt')
require('dplyr')
require('tidyr')
require('sf')
require(terra)
require(FedData)
require(climr)
require(ecmwfr)
require(ggplot2)
require(foreach)


lapply(dir('R', '*.R', full.names = TRUE), source)

### Input data ----
raw <- file.path('data','raw')
derived <- file.path('data','derived')

#canada <- file.path('C:','Users', 'julie', 'Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')
#canada <- file.path('~/Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')

dat <- readRDS(file.path(derived, 'clean_NMC.RDS'))
sArea <- file.path(derived, 'prepped', 'clean_NMC_20kmBuff.shp')
studyArea <- vect(sArea)

## PREP ----
crs <- st_crs(3154)$wkt
crs.lonlat <- st_crs(4326)$wkt
crs.daymet <- st_crs("+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")$wkt
crs.bc <- st_crs(3005)$wkt
maxyr <- lubridate::year(max(dat$t_, na.rm = T))
minyr <- lubridate::year(min(dat$t_, na.rm = T))
buff.diam <- 700


## daymetr ----
# convert study area to `sf` object and projection for daymet
studyArea.temp <- st_read(sArea) %>%
  st_transform(crs.lonlat)

# Get the DAYMET (North America only)
# Returns a list of raster bricks
# goes through 2022 as of May 2024
yrs <- minyr:2022
lapply(yrs, function(yy){
  get_daymet(
  template = studyArea.temp,
  label = yy,
  elements = c("tmin", "tmax", 'swe', 'prcp'),
  years = yy, 
  region = 'na',
  tempo = 'day', 
  extraction.dir = file.path(raw, 'FedData'),
  progress = T)
})

## ERA5 data ----
# see https://rpubs.com/Ajeep05/era5 for help setting up ERA5 in R

studyArea.bbox <- st_read(sArea) %>%
  st_transform(crs.lonlat) %>%
  st_bbox()
# Note for the sub-region extraction “area”, the order is (N, W, S, E)
area.ext <- "65.35436/-140.88175/53.74437/-119.93734"

era5.path <- file.path(raw, 'era5data')
vr <- c("2m_temperature")
vr.abbr <- 'temp_'
yr <- 2014:2023
mn <- sprintf("%02d",1:12)
dy <- sprintf("%02d", 1:31)
tm <- sprintf("%02d:00", 0:23)

for (i in 1:length(yr)) {
  for (j in 1:length(mn)) {
    request <- list(
      "dataset_short_name" = "reanalysis-era5-land",
      "product_type" = "reanalysis",
      "variable" = vr[1],
      "year" = yr[i],
      "month" = mn[j],
      "day" = dy,
      "time" = tm,
      "area" = area.ext,
      "format" = "netcdf",
      "target" = paste0("temp_",yr[i],"_",mn[j],".nc")
    )
    
    file <- wf_request(
      user     = "310121",   # user ID (for authentication)
      request  = request,  # the request
      transfer = TRUE,     # download the file
      path     = era5.path,  # store data in current working directory
      time_out = 3600
      )
    
  }
}

## ClimateNA ----
# convert study area to `sf` object and projection for DEM
canada <- rnaturalearth::ne_states(country = 'canada')
nmc <- filter(canada, name %in% c("Yukon", "British Columbia"))

#ee_Initialize(user = 'julwturner@gmail.com', drive = TRUE) 
ee_Initialize(drive = TRUE) 
dem <- ee$Image("USGS/SRTMGL1_003")

studyArea.temp <- st_read(sArea) %>%
  st_transform(crs.lonlat) %>%
  st_bbox() %>%
  st_as_sfc()
st_write(studyArea.temp, file.path(derived, 'studyArea_bbox.shp'))

ggplot()+
  geom_sf(data = nmc) +
  geom_sf(data = studyArea.temp)

dem <- elevatr::get_elev_raster(studyArea.temp, z =12, override_size_check = TRUE, neg_to_na = T)

dem <- aoi_geom_save_tif_matches(sf_or_file = studyArea.temp,
                                 dir_save_tifs = raw,
                                 resolution = 30,
                                 crs_value = crs.lonlat,
                                 threads = parallel::detectCores(),
                                 verbose = TRUE)
