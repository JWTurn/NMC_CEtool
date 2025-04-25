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
### Input data ----
raw <- file.path('data','raw')
derived <- file.path('data','derived')
# I like setting a folder shared across projects that share large files so I don't have to download many times
# could use raw if want to keep with this project
canada <- file.path('C:','Users', 'julie', 'Documents', 'git', 'reproducibleInputs')



sArea <- file.path(derived, 'prepped', 'clean_NMC_20kmBuff.shp')
# over extending to capture all features they may be responding to and account for edge effects
studyArea.buff <- buffer(vect(sArea), 100000)

# make one continuous study area
studyArea.sf <- st_as_sf(studyArea.buff)
studyArea.ch <- st_convex_hull(studyArea.sf)
#st_write(studyArea.ch, file.path(derived, 'NMC_studyArea_extended.shp'))

studyArea_extendedLandscape <- vect(studyArea.ch)
# make template rasters at needed resolutions
rasterToMatch_extendedLandscape30m <- terra::rast(studyArea_extendedLandscape, res = c(30,30))
rasterToMatch_extendedLandscape30m [] <- 1
rasterToMatch_extendedLandscape30m  <- terra::mask(rasterToMatch_extendedLandscape30m, studyArea_extendedLandscape)

rasterToMatch_extendedLandscape250m <- terra::rast(studyArea_extendedLandscape, res = c(250,250))
rasterToMatch_extendedLandscape250m [] <- 1
rasterToMatch_extendedLandscape250m  <- terra::mask(rasterToMatch_extendedLandscape250m, studyArea_extendedLandscape)

rasterToMatch_extendedLandscape1000m <- terra::aggregate(rasterToMatch_extendedLandscape250m, 4)

harvURL <- "https://opendata.nfis.org/downloads/forest_change/CA_Forest_Harvest_1985-2020.zip"

#landURL <- "https://opendata.nfis.org/downloads/forest_change/CA_forest_VLCE2_2010.zip"


harv <- reproducible::prepInputs(url = harvURL,
                                 destinationPath = canada,
                                 to = rasterToMatch_extendedLandscape250m, 
                                 fun = 'terra::rast') 
harv[harv==0]<-NA
writeRaster(harv, file.path(derived, 'nmc_harv250m_1985-2020.tif'))


buffer = 700
  
rtms <- list(rasterToMatch_extendedLandscape250m, rasterToMatch_extendedLandscape1000m)
names(rtms)  <- c("window250", 'agg1000') 
rtmsFuns <- c(paste0('make_landforest_prop(targetFile = targetFile, trast = rtm, buff = ',
                     buffer,', where2save = NULL)'),
              paste('aggregate_landforest(targetFile = targetFile, trast = rtm', 
                    ', where2save = NULL)'))
historicLandYears <- 2012:2013
names(historicLandYears) <- historicLandYears

st<- Sys.time()

propLand <- Map(rtmname = names(rtms), rtm = rtms, rtmFun = rtmsFuns, 
                function(rtm, rtmname, rtmFun) {
                          path <- file.path(derived, 'prop_land', rtmname) |> checkPath(create = TRUE)
                          Map(ii=as.character(historicLandYears), function(ii){
                            reproducible::prepInputs(
                              url = paste0("https://opendata.nfis.org/downloads/forest_change/CA_forest_VLCE2_", ii, ".zip"),
                              targetFile = paste0('CA_forest_VLCE2_',ii, '.tif'),
                              destinationPath = canada, # end preprocess
                              fun = eval(parse(text = rtmFun)), # end process, doing postprocess within fun
                              writeTo = file.path(path, paste0('propLand_', ii, '.tif')) |> checkPath(create = TRUE)
                              ) 
                          }) |> Cache()
                }
)

