# === Targets workflow: iSSA with amt -------------------------------------
# Julie Turner
# adapted from code developed with Alec L. Robitaille


# Packages ----------------------------------------------------------------
library(targets)

library(tarchetypes)
library(amt)
library(data.table)
library(terra)
library(sf)
library(ggplot2)
library(distanceto)


# Functions ---------------------------------------------------------------
lapply(dir('R', '*.R', full.names = TRUE), source)
#source('R/functions.R')


# Options -----------------------------------------------------------------
tar_option_set(format = 'qs', 
               error = 'null',
               workspace_on_error = T)


# Variables ---------------------------------------------------------------
set.seed(37)
raw <- file.path('data','raw')
derived <- file.path('data','derived')

path <- file.path(derived, 'clean_NMC.RDS')
#canada <- file.path('C:','Users', 'peteb', 'OneDrive', 'Documents', 'BP', 'Yukoncfx', 'reproducibleInputs')
canada <- file.path('~/Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')
studyArea <- file.path(derived, 'prepped', 'clean_NMC_20kmBuff_crop.shp')
#canPoly <- st_read('lpr_000b16a_e.shp') For Pete, delete later

#prop700 <- file.path('data', 'raw', 'prop_land', '700')
#prop_water <- file.path('data', 'raw', 'prop_land', '700', 'prop_water')
#prop_snow <- file.path('data', 'raw', 'prop_land', '700', 'prop_snow')
#prop_rock <- file.path('data', 'raw', 'prop_land', '700', 'prop_rock')
#prop_barren <- file.path('data', 'raw', 'prop_land', '700', 'prop_barrenland')
#prop_bryoids <- file.path('data', 'raw', 'prop_land', '700', 'prop_bryoids')
#prop_shrub <- file.path('data', 'raw', 'prop_land', '700', 'prop_shrub')
#prop_wetland <- file.path('data', 'raw', 'prop_land', '700', 'prop_wetland')
#prop_wettreed <- file.path('data', 'raw', 'prop_land', '700', 'prop_wet_treed')
#prop_herbs <- file.path('data', 'raw', 'prop_land', '700', 'prop_herbs')
#prop_needleleaf <- file.path('data', 'raw','prop_land', '700', 'prop_needleleaf')
#prop_deciduous <- file.path('data', 'raw', 'prop_land', '700', 'prop_deciduous')
#prop_mixed <- file.path('data', 'raw', 'prop_land', '700', 'prop_mixed')


prop_land <- file.path('data', 'derived', 'prop_land')
landcov250 <- file.path('data', 'derived', 'prop_land', 'window250')
landcov1000 <- file.path('data', 'derived', 'prop_land', 'agg1000')
landcov5000 <- file.path('data', 'derived', 'prop_land', 'agg5000')

fires_250 <- file.path('data', 'derived', 'fireRasters250m')
fires_1000 <- file.path('data', 'derived', 'fireRasters1000m')
fires_5000 <- file.path('data', 'derived', 'fireRasters5000m')

harv_250 <- file.path('data', 'derived', 'harv250')
harv_1000 <- file.path('data', 'derived', 'harv1000')
hav_5000 <- file.path('data', 'derived', 'harv5000')

#Needs to be updated once done prepland ***
for.age <- file.path('data', 'raw', 'nmc_forestage_meanbuff700_2019.tif')
anthropolys <- file.path('data', 'raw', 'polygonal_features', 'anthropolys.shp')
miningpolys <- file.path('data', 'raw', 'polygonal_features', 'miningpolys.shp')
miningpolys.yt <- file.path('data', 'raw', 'polygonal_features', 'mining_nmc_yt.shp')
anthropolys.rast <- file.path('data', 'raw', 'anthroDisturbance', 'mining_polys_prop700.tif')
lf_paved<- file.path('data', 'raw', 'linear_features', 'roadpaved.shp')
lf_unpaved <- file.path('data', 'raw', 'linear_features', 'roadunpaved.shp')
lf_other <- file.path('data', 'raw', 'linear_features', 'lfother.shp')

id <- 'id'
datetime <- 't_'
longlat = FALSE
#not actually longitude and latitude, just don't want to change code
long <- 'x_'
lat <- 'y_'
crs <- st_crs(3154)$wkt
n_rand <- 30 # number of random steps to generate

# minimum year we want to pull data for
minyr <- 2012
maxyr <- 2024


# Split by: within which column or set of columns (eg. c(id, yr))
#  do we want to split our analysis?
splitBy <- id
interval <- 5 # to round by 5 year intervals
sl.interval <- 50 # to round by 50m intervals



# Resampling rate 
rate <- hours(13)

# Tolerance
tolerance <- minutes(90)


# Targets: prep -----------------------------------------------------------------
targets_prep <- c(
  # set up path to track
  tar_target(
    input.path,
    path,
    format = 'file'
  ),
  # Read input data
  tar_target(
    input,
    readRDS(input.path)
  ),
  
  # Remove duplicated and incomplete observations
  tar_target(
    mkunique,
    make_unique_complete(input, id, datetime, long, lat)
  ),
  
  
  # load paved linear features **UPDATE**
  tar_target(
    lf_paved.path,
    lf_paved,
    format = 'file'
  ),
  tar_target(
    paved.sf,
    load_sf(lf_paved.path, crs)
  ),

  # load unpaved linear features **UPDATE**
  tar_target(
    lf_unpaved.path,
    lf_unpaved,
    format = 'file'
  ),
  tar_target(
    unpaved.sf,
    load_sf(lf_unpaved.path, crs)
  ),

  # load other linear features **UPDATE**
  tar_target(
    lf_other.path,
    lf_other,
    format = 'file'
  ),
  tar_target(
    lf_other.sf,
    load_sf(lf_other.path, crs)
  ),
  
  # load anthropolys **UPDATE**
  tar_target(
    anthropolys.path,
    anthropolys,
    format = 'file'
  ),
  tar_target(
    anthropolys.sf,
    load_sf(anthropolys.path, crs)
  ),
  tar_target(
    anthropolys.rast.path,
    anthropolys.rast,
    format = 'file'
  ),
  
  # load miningpolys **UPDATE**
  tar_target(
    miningpolys.path,
    miningpolys,
    format = 'file'
  ),
  tar_target(
    miningpolys.sf,
    load_sf(miningpolys.path, crs)
  ),
  
  # load miningpolys for just Yukon **UPDATE**
  tar_target(
    miningpolys.yt.path,
    miningpolys.yt,
    format = 'file'
  ),
  tar_target(
    miningpolys.yt.sf,
    load_sf(miningpolys.yt.path, crs)
  ),

  # load harvest
  tar_target( #OLD ***
    harv.path,
    harv,
    format = 'file'
  ),
  tar_target(
    harv.sf,
    make_harv_sf(harv.path)
  ),
  
  tar_target(
    harv250.path,
    harv_250,
    format = "file"
  ),
  
  tar_target(
    harv250.sf,
    make_harv_rast(harv250.path)
  ),
  

  
  
  # load fires
  # tar_files(
  #   fires.path,
  #   list.files(path = fires_path, full.names = T)
  # ),
 
  # load forest age 
  tar_target(
    for.age.path,
    for.age,
    format = 'file'
  ),
  
  # track prop land
  # tar_files(
  #   prop700.path,
  #   list.files(path = prop700, full.names = T)
  # ),
  
  # filter out points outside study area
  tar_target(
    crop.pts,
    crop_pts(mkunique, studyArea, crs)
  ),
  
  # Set up split -- these are our iteration units
  tar_target(
    splits,
    crop.pts[, tar_group := .GRP, by = splitBy],
    iteration = 'group'
  ),
  
  tar_target(
    splitsnames,
    unique(mkunique[, .(path = path), by = splitBy])
  )
)


# Targets: tracks -----------------------------------------------------------------------
# Make tracks. Note from here on, when we want to iterate use pattern = map(x)
#  where x is the upstream target name
targets_tracks <- c(
  tar_target(
    tracks,
    make_track(splits, long, lat, datetime, crs = crs, all_cols = T),
    pattern = map(splits)
  ),
  
  # Resample sampling rate, filtering out extra long steps
  tar_target(
    resamples,
    resample_tracks(tracks, rate, tolerance, probsfilter = 0.99),
    pattern = map(tracks)
  ),
  
  
  
  # Check step distributions
  #  iteration = 'list' used for returning a list of ggplots,
  #  instead of the usual combination with vctrs::vec_c()
  tar_target(
    distributions,
    ggplot(resamples, aes(sl_)) + geom_density(alpha = 0.4)#,
    # pattern = map(resamples),
    # iteration = 'list'
  ),
  
  tar_target(
    sl_distr,
    fit_distr(resamples$sl_, 'gamma') 
  ),
  
  tar_target(
    ta_distr,
    fit_distr(resamples$ta_, 'vonmises') 
  ),
  
  # create random steps and extract covariates
  tar_target(
    randsteps,
    make_random_steps(resamples, n_rand = n_rand, sl_distr, ta_distr),
    pattern = map(resamples)
  ),
  
  
  # Distribution parameters
  tar_target(
    distparams,
    calc_distribution_parameters(randsteps),
    pattern = map(randsteps)
  ), 
  
  # make a data.table so easier to manipulate
  tar_target(
    dattab,
    make_data_table(randsteps)
  ),
  
  # add a year column
  tar_target(
    addyear,
    dattab[,`:=`(year=lubridate::year(t2_), 
                 int.year=plyr::round_any(lubridate::year(t2_), interval, floor))]
  )
)


# Targets: extract ------------------------------------------------------------------
## Proportion of land ----

targets_propland <- c(
  tar_target(
    buffer,
    plyr::round_any(median(addyear$sl_, na.rm = T), sl.interval, floor)
  ),

  tar_target(
    extr_propwater,
    extract_by_year(addyear, var = landcov250, startyr = minyr, endyr = 2022, maxyr, where = 'end')
  ),

  tar_target(
    extr_propsnow,
    extract_by_year(extr_propwater, var = prop_snow, startyr = minyr, endyr = 2019, maxyr, where = 'both')
  ),

  tar_target(
    extr_proprock,
    extract_by_year(extr_propsnow, var = prop_rock, startyr = minyr, endyr = 2019, maxyr, where = 'both')
  ),

  tar_target(
    extr_propbarren,
    extract_by_year(extr_proprock, var = prop_barren, startyr = minyr, endyr = 2019, maxyr, where = 'both')
  ),

  tar_target(
    extr_propbryoids,
    extract_by_year(extr_propbarren, var = prop_bryoids, startyr = minyr, endyr = 2019, maxyr, where = 'both')
  ),

  tar_target(
    extr_propshrub,
    extract_by_year(extr_propbryoids, var = prop_shrub, startyr = minyr, endyr = 2019, maxyr, where = 'both')
  ),

  tar_target(
    extr_propwetland,
    extract_by_year(extr_propshrub, var = prop_wetland, startyr = minyr, endyr = 2019, maxyr, where = 'both')
  ),

  tar_target(
    extr_propwettreed,
    extract_by_year(extr_propwetland, var = prop_wettreed, startyr = minyr, endyr = 2019, maxyr, where = 'both')
  ),

  tar_target(
    extr_propherbs,
    extract_by_year(extr_propwettreed, var = prop_herbs, startyr = minyr, endyr = 2019, maxyr, where = 'both')
  ),

  tar_target(
    extr_propneedle,
    extract_by_year(extr_propherbs, var = prop_needleleaf, startyr = minyr, endyr = 2019, maxyr, where = 'both')
  ),

  tar_target(
    extr_propdecid,
    extract_by_year(extr_propneedle, var = prop_deciduous, startyr = minyr, endyr = 2019, maxyr, where = 'both')
  ),

  tar_target(
    extr_propmixed,
    extract_by_year(extr_propdecid, var = prop_mixed, startyr = minyr, endyr = 2019, maxyr, where = 'both')
  )
)

## anthro disturbance ----
targets_disturbance <- c(     
  # Calculate distance to anthropogenic disturbances
  tar_target(
    dist_paved,
    extract_distto(extr_propmixed, paved.sf, 'paved', where = 'both', crs)
  ),
  
  tar_target(
    dist_unpaved,
    extract_distto(dist_paved, unpaved.sf, 'unpaved', where = 'both', crs)
  ),
  
  tar_target(
    dist_other,
    extract_distto(dist_unpaved, lf_other.sf, 'lf_other', where = 'both', crs)
  ),
  
  tar_target(
    dist_anthropolys,
    extract_distto(dist_other, anthropolys.sf, 'anthro', where = 'both', crs)
  ),
  
  tar_target(
    dist_miningpolys,
    extract_distto(dist_anthropolys, miningpolys.sf, 'mining', where = 'both', crs)
  ),
  
  tar_target(
    dist_placer,
    extract_distto(dist_miningpolys, dplyr::filter(miningpolys.yt.sf, RC_Disturb == 'Placer Mining'), 'placer', where = 'both', crs)
  ),
  
  tar_target(
    dist_quartz,
    extract_distto(dist_placer, dplyr::filter(miningpolys.yt.sf, RC_Disturb == 'Quartz Mining'), 'quartz', where = 'both', crs)
  ),
  
  # TODO fix to work for a range of years
  tar_target(
    dist_harv,
    extract_recent(dist_quartz, var = harv.sf, name = 'harv', method = 'distto', where = 'both') #, time_recent = 0
  ),
  
  # extract point in disturbances
  tar_target(
    extr_anthropolys.prop,
    extract_pt(dist_harv, anthropolys.rast.path, 'prop_disturbance', where = 'both')
  )
)


## time since ----
targets_timesince <- c(   
  # Extract forest age
  tar_target(
    extr_for.age,
    extract_pt(extr_anthropolys.prop, for.age.path, 'for.age', where = 'both')
  ),
  
  # TODO can't get tracked files to work for these two yet...it may have just been the fires path 
  # Extract harvest
  tar_target(
    extr_harv,
    extract_pt(extr_for.age, harv, 'harv', where = 'both')
  ),

  # calculate time since harvest
  tar_target(
    ts_harv,
    calc_ts(extr_harv, var = 'harv', where = 'both', no.data=100)
  ),
  
  # Extract fires
  tar_target(
    extr_fires,
    extract_by_year(ts_harv, fires, startyr = minyr, endyr = 2023, maxyr = maxyr, where = 'both')
  ),
  
  # calculate time since fire
  tar_target(
    ts_fire,
    calc_ts(extr_fires, var = 'fires', where = 'both', no.data=200) 
  )
)





## Step ID ----
targets_stepID <- c(
  # create step ID across individuals
  tar_target(
    stepID,
    setDT(ts_fire)[,indiv_step_id := paste(id, step_id_, sep = '_')]
  )
)


# Targets: all ------------------------------------------------------------------
# Automatically grab and combine all the 'targets_*' lists above
lapply(grep('targets', ls(), value = TRUE), get)


