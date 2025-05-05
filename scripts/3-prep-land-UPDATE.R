### Prepare Landcover data ====
# Julie Turner
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
# I like setting a folder shared across projects that share large files so I don't have to download many times
# could use raw if want to keep with this project
canada <- file.path('C:','Users', 'julie', 'Documents', 'git', 'reproducibleInputs')


## make study area ----
# area for extraction
sArea <- file.path(derived, 'prepped', 'clean_NMC_20kmBuff.shp')
# over extending to capture all features they may be responding to and account for edge effects
studyArea.buff <- buffer(vect(sArea), 100000)

# make one continuous study area
studyArea.sf <- st_as_sf(studyArea.buff)
studyArea.ch <- st_convex_hull(studyArea.sf)
#st_write(studyArea.ch, file.path(derived, 'NMC_studyArea_extended.shp'))

studyArea_extendedLandscape <- vect(file.path(derived, 'NMC_studyArea_extended.shp'))

## template rasters ----
# make template rasters at needed resolutions
rasterToMatch_extendedLandscape30m <- terra::rast(studyArea_extendedLandscape, res = c(30,30))
rasterToMatch_extendedLandscape30m [] <- 1
rasterToMatch_extendedLandscape30m  <- terra::mask(rasterToMatch_extendedLandscape30m, studyArea_extendedLandscape)

rasterToMatch_extendedLandscape250m <- terra::rast(studyArea_extendedLandscape, res = c(250,250))
rasterToMatch_extendedLandscape250m [] <- 1
rasterToMatch_extendedLandscape250m  <- terra::mask(rasterToMatch_extendedLandscape250m, studyArea_extendedLandscape)

rasterToMatch_extendedLandscape1000m <- terra::aggregate(rasterToMatch_extendedLandscape250m, 4)

# Do 5000m for once per week?

### years we need data for
historicLandYears <- 2012:2013 # update to last year of data
# historicHarvYears <- 2012:2020
## Harvest ----

harvURL <- "https://opendata.nfis.org/downloads/forest_change/CA_Forest_Harvest_1985-2020.zip"


# update to larger resolutions
harv30m <- reproducible::prepInputs(url = harvURL,
                                 destinationPath = canada,
                                 to = rasterToMatch_extendedLandscape30m, 
                                 fun = 'terra::rast', 
                                 method = 'near') |>
  Cache()
harv[harv==0]<-NA
writeRaster(harv, file.path(derived, 'nmc_harv30m_1985-2020.tif'))

harv250m <- reproducible::prepInputs(url = harvURL,
                                    destinationPath = canada,
                                    to = rasterToMatch_extendedLandscape250m, 
                                    fun = 'terra::rast', 
                                    method = 'average') |>
  Cache()
harv[harv==0]<-NA
writeRaster(harv, file.path(derived, 'nmc_harv250m_1985-2020.tif'))

# TODO need (average) year of harvest at the larger resolutions
# TODO need (average) distance to for each of harvest at the larger resolutions (1000 and 5000)
# TODO need presence/absence of harvest (active) in cell per year 1000 and 5000

## prop landcover ----

#landURL <- "https://opendata.nfis.org/downloads/forest_change/CA_forest_VLCE2_2010.zip"

buffer <- 750 
rtms <- list(rasterToMatch_extendedLandscape250m, rasterToMatch_extendedLandscape1000m)
names(rtms)  <- c("window250", 'agg1000') 
rtmsFuns <- list(paste0('make_landforest_prop(targetFile = targetFile, trast = rtm, buff = ',
                     buffer, ', where2save = saveName)'),
              paste('aggregate_landforest(targetFile = targetFile, trast = rtm, where2save = saveName)'))

names(historicLandYears) <- paste0('year_',historicLandYears)

# st<- Sys.time()

propLand <- Map(rtmname = names(rtms), rtm = rtms, rtmFun = rtmsFuns, 
                function(rtm, rtmname, rtmFun) {
                          
                          Map(nn = names(historicLandYears),ii=as.character(historicLandYears), function(nn,ii){
                            path <- file.path(derived, 'prop_land', rtmname) |> checkPath(create = TRUE)
                            saveName <- file.path(path, paste0('propLand_', ii, '.tif'))
                            reproducible::prepInputs(
                              url = paste0("https://opendata.nfis.org/downloads/forest_change/CA_forest_VLCE2_", ii, ".zip"),
                              targetFile = paste0('CA_forest_VLCE2_',ii, '.tif'),
                              destinationPath = canada, # end preprocess
                              fun = eval(parse(text = rtmFun)),
                             # make_landforest_prop = make_landforest_prop,
                              rtmFun = rtmFun#, # end process, doing postprocess within fun
                              #where2save = file.path(path, paste0('propLand_', ii, '.tif'))
                              ) |> Cache()
                          }) 
                }
)#|> Cache()


## human disturbances -----
# Features to disentangle
## LFs: paved road, unpaved road, other lf
## Polys: quartz, placer, other mines, all other polygons (not including harvest)
# TODO need distance to at larger resolutions per year for each feature
# TODO need presence/absence of each feature per year at large resolution
# TODO need each lf density per year at larger resolutions

# pseudo set up density (may be presence/absences?)
roadDensity250m <- reproducible::prepInputs(url = ROADS,
                                     destinationPath = canada, # maybe change to raw
                                     to = rasterToMatch_extendedLandscape250m, 
                                     fun = 'terra::vect', 
                                     method = 'average') |>
  Cache()
# if that doesn't give average density -- bring in at 30m, then average when aggregating up


## FIRES ----
# TODO this needs to be updated as above, but this is the old code

## FIRES #####
## this has been run already
# combine nbac (which seems to be more error checked) with older fires from NFDB
# TODO replace with URL
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

# NOTE: there are still some years that = -9999 that only have a decade associated with them
## current plan is to set them to NA in analysis since it's unknown and not too many

writeVector(nmc.fires.all, file.path(raw, 'fires', 'fires_1920_2023.shp'), overwrite = T)

# make rasters from polygonal fires by year
gc()

make_fire_rast(layer = file.path(raw, 'fires', 'fires_1920_2023.shp'),
               rast = land, minyr = minyr,
               where2save = file.path(raw, 'fires'))
