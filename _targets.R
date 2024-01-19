# === Targets workflow: iSSA with amt -------------------------------------
# Julie Turner
# adapted from code by Alec L. Robitaille


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
               workspace_on_error = T)


# Variables ---------------------------------------------------------------
set.seed(37)
raw <- file.path('data','raw')
derived <- file.path('data','derived')

path <- file.path(derived, 'clean_NMC.RDS')
canada <- file.path('~/Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')

prop_water <- file.path('data', 'raw', 'prop_land', '700', 'prop_water')
prop_snow <- file.path('data', 'raw', 'prop_land', '700', 'prop_snow')
prop_rock <- file.path('data', 'raw', 'prop_land', '700', 'prop_rock')
prop_barren <- file.path('data', 'raw', 'prop_land', '700', 'prop_barrenland')
prop_bryoids <- file.path('data', 'raw', 'prop_land', '700', 'prop_bryoids')
prop_shrub <- file.path('data', 'raw', 'prop_land', '700', 'prop_shrub')
prop_wetland <- file.path('data', 'raw', 'prop_land', '700', 'prop_wetland')
prop_wettreed <- file.path('data', 'raw', 'prop_land', '700', 'prop_wet_treed')
prop_herbs <- file.path('data', 'raw', 'prop_land', '700', 'prop_herbs')
prop_needleleaf <- file.path('data', 'raw','prop_land', '700', 'prop_needleleaf')
prop_deciduous <- file.path('data', 'raw', 'prop_land', '700', 'prop_deciduous')
prop_mixed <- file.path('data', 'raw', 'prop_land', '700', 'prop_mixed')

fires <- file.path('data', 'raw', 'fire_nbac_1986_to_2020', 'fires')


id <- 'id'
datetime <- 't_'
longlat = FALSE
#not actually longitude and latitude, just don't want to change code
long <- 'x_'
lat <- 'y_'
crs <- st_crs(3154)$wkt

# minimum year we want to pull data for
minyr <- 2012
maxyr <- 2023


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
  # Read input data
  tar_target(
    input,
    readRDS(path)
  ),
  
  # Remove duplicated and incomplete observations
  tar_target(
    mkunique,
    make_unique_complete(input, id, datetime, long, lat)
  ),
  
  
  # # load linear features
  # tar_target(
  #   lf,
  #   load_sf(linfeat, crs)
  # ),
  # 
  # # load other linear features
  # tar_target(
  #   lf_other_2010,
  #   load_sf(linfeat_other_2010, crs)
  # ),
  # 
  # # load other linear features
  # tar_target(
  #   lf_other_2015,
  #   load_sf(linfeat_other_2015, crs)
  # ),

  
  # Set up split -- these are our iteration units
  tar_target(
    splits,
    mkunique[, tar_group := .GRP, by = splitBy],
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
    make_random_steps(resamples, sl_distr, ta_distr),
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
    extract_by_year(addyear, var = prop_water, startyr = minyr, endyr = 2019, maxyr, where = 'both')
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



## time since ----
targets_timesince <- c(    
  # # Extract harvest
  # tar_target(
  #   extrharv,
  #   extract_pt(extr_propmixed, harv, 'harv', where = 'both')
  # ),
  # 
  # # calculate time since harvest
  # tar_target(
  #   tsharv,
  #   calc_ts(extrharv, var = 'harv', where = 'both', no.data=100) 
  # ),
  
  # Extract fires
  tar_target(
    extrfires,
    extract_by_year(extr_propmixed, fires, startyr = minyr, endyr =2020, maxyr = maxyr, where = 'both')
  ),
  
  # calculate time since fire
  tar_target(
    tsfire,
    calc_ts(extrfires, var = 'fires', where = 'both', no.data=100) 
  )
)





## Step ID ----
targets_stepID <- c(
  # create step ID across individuals
  tar_target(
    stepID,
    setDT(tsfire)[,indiv_step_id := paste(id, step_id_, sep = '_')]
  )
)


# Targets: all ------------------------------------------------------------------
# Automatically grab and combine all the 'targets_*' lists above
lapply(grep('targets', ls(), value = TRUE), get)


