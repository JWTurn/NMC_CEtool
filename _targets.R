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


#Landcover for 250m resolution
prop_land_prefix <- file.path("data", "derived", "prop_land", "prop_land_split")
prop_land <- file.path('data', 'derived', 'prop_land')
landcov250 <- file.path('data', 'derived', 'prop_land', 'window250')
prop_water_250 <- file.path(prop_land_prefix, "prop_water_250")
prop_snow_250 <- file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_snow_250')
prop_rock_250 <- file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_rock_250')
prop_barren_250 <- file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_barrenland_250')
prop_bryoids_250 <- file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_bryoids_250')
prop_shrub_250 <- file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_shrub_250')
prop_wetland_250 <- file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_wetland_250')
prop_wettreed_250 <- file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_wetreed_250')
prop_herbs_250 <- file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_herbs_250')
prop_needleleaf_250 <- file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_needleleaf_250')
prop_deciduous_250 <- file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_deciduous_250')
prop_mixed_250 <- file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_mixed_250')


#Fires 250
fires_250 <- file.path('data', 'derived', 'fireRasters250m')

#Harvest 250
harv_dist_250 <- file.path('data', 'derived', 'harv_dist_250m')
harv_pa_250 <- file.path('data', 'derived', 'harv_pa_250m')
harv250 <- file.path('data', 'derived', 'harv250')

#Forest age 250
for_age_250 <- file.path('data', 'raw', 'age250')

#Anthro-disturbance 250
dens_lfother_250 <- file.path('data', 'derived', 'dens_disturb_250m', 'otherlf')
dens_paved_250 <- file.path('data', 'derived', 'dens_disturb_250m', 'paved')
dens_unpaved_250 <- file.path('data', 'derived', 'dens_disturb_250m', 'unpaved')
dist_paved_250 <- file.path('data', 'derived', 'dist_disturb_250m', 'paved')
dist_unpaved_250 <- file.path('data', 'derived', 'dist_disturb_250m', 'unpaved')
dist_lfother_250 <- file.path('data', 'derived', 'dist_disturb_250m', 'otherlf')
dist_placer_250 <- file.path('data', 'derived', 'dist_disturb_250m', 'placer')
dist_quartz_250 <- file.path('data', 'derived', 'dist_disturb_250m', 'quartz')
dist_othermines_250 <- file.path('data', 'derived', 'dist_disturb_250m', 'othermines')
dist_otherpoly_250 <- file.path('data', 'derived', 'dist_disturb_250m', 'otherpoly')
pa_paved_250 <- file.path('data', 'derived', 'pa_disturb_250m', 'paved')
pa_unpaved_250 <- file.path('data', 'derived', 'pa_disturb_250m', 'unpaved')
pa_lfother_250 <- file.path('data', 'derived', 'pa_disturb_250m', 'otherlf')
pa_placer_250 <- file.path('data', 'derived', 'pa_disturb_250m', 'placer')
pa_quartz_250 <- file.path('data', 'derived', 'pa_disturb_250m', 'quartz')
pa_othermines_250 <- file.path('data', 'derived', 'pa_disturb_250m', 'othermines')
pa_otherpoly_250 <- file.path('data', 'derived', 'pa_disturb_250m', 'otherpoly')


#Landcover for 1000m resolution
prop_land <- file.path('data', 'derived', 'prop_land')
landcov1000 <- file.path('data', 'derived', 'prop_land', 'agg1000')
prop_water_1000 <- file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_water_1000')
prop_snow_1000 <- file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_snow_1000')
prop_rock_1000 <- file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_rock_1000')
prop_barren_1000 <- file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_barrenland_1000')
prop_bryoids_1000 <- file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_bryoids_1000')
prop_shrub_1000 <- file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_shrub_1000')
prop_wetland_1000 <- file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_wetland_1000')
prop_wettreed_1000 <- file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_wetreed_1000')
prop_herbs_1000 <- file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_herbs_1000')
prop_needleleaf_1000 <- file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_needleleaf_1000')
prop_deciduous_1000 <- file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_deciduous_1000')
prop_mixed_1000 <- file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_mixed_1000')


#Fires 1000
fires_1000 <- file.path('data', 'derived', 'fireRasters1000m')

#Harvest 1000
harv_dist_1000 <- file.path('data', 'derived', 'harv_dist_1000m')
harv_pa_1000 <- file.path('data', 'derived', 'harv_pa_1000m')
harv1000 <- file.path('data', 'derived', 'harv1000')

#Forest age 1000
for_age_1000 <- file.path('data', 'raw', 'age1000')

#Anthro-disturbance 1000
dens_lfother_1000 <- file.path('data', 'derived', 'dens_disturb_1000m', 'otherlf')
dens_paved_1000 <- file.path('data', 'derived', 'dens_disturb_1000m', 'paved')
dens_unpaved_1000 <- file.path('data', 'derived', 'dens_disturb_1000m', 'unpaved')
dist_paved_1000 <- file.path('data', 'derived', 'dist_disturb_1000m', 'paved')
dist_unpaved_1000 <- file.path('data', 'derived', 'dist_disturb_1000m', 'unpaved')
dist_lfother_1000 <- file.path('data', 'derived', 'dist_disturb_1000m', 'otherlf')
dist_placer_1000 <- file.path('data', 'derived', 'dist_disturb_1000m', 'placer')
dist_quartz_1000 <- file.path('data', 'derived', 'dist_disturb_1000m', 'quartz')
dist_othermines_1000 <- file.path('data', 'derived', 'dist_disturb_1000m', 'othermines')
dist_otherpoly_1000 <- file.path('data', 'derived', 'dist_disturb_1000m', 'otherpoly')
pa_paved_1000 <- file.path('data', 'derived', 'pa_disturb_1000m', 'paved')
pa_unpaved_1000 <- file.path('data', 'derived', 'pa_disturb_1000m', 'unpaved')
pa_lfother_1000 <- file.path('data', 'derived', 'pa_disturb_1000m', 'otherlf')
pa_placer_1000 <- file.path('data', 'derived', 'pa_disturb_1000m', 'placer')
pa_quartz_1000 <- file.path('data', 'derived', 'pa_disturb_1000m', 'quartz')
pa_othermines_1000 <- file.path('data', 'derived', 'pa_disturb_1000m', 'othermines')
pa_otherpoly_1000 <- file.path('data', 'derived', 'pa_disturb_1000m', 'otherpoly')



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



# Resampling rate - explore fix rate median after new data
rate <- hours(13)

# Tolerance
tolerance <- minutes(90)


# Targets: prep ----------------------------------------------------------------- (skip loading rasters for disturbance - all have years)
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
  #tar_target(
  # lf_paved.path,
  # lf_paved,
  # format = 'file'
  #),
  #tar_target(
  # paved.sf,
  # load_sf(lf_paved.path, crs)
  #),

  # load unpaved linear features **UPDATE**
  #tar_target(
  #  lf_unpaved.path,
  #  lf_unpaved,
  #  format = 'file'
  #),
  ##tar_target(
  ##  unpaved.sf,
  #  load_sf(lf_unpaved.path, crs)
  #),

  # load other linear features **UPDATE**
  #tar_target(
  #  lf_other.path,
  #  lf_other,
  #  format = 'file'
  #),
  #tar_target(
  #  lf_other.sf,
  #  load_sf(lf_other.path, crs)
  #),
  
  # load anthropolys **UPDATE**
  #tar_target(
  #  anthropolys.path,
  #  anthropolys,
  #  format = 'file'
  #),
  #tar_target(
  #  anthropolys.sf,
  #  load_sf(anthropolys.path, crs)
  #),
  #tar_target(
  #  anthropolys.rast.path,
  #  anthropolys.rast,
  #  format = 'file'
  #),
  
  # load miningpolys **UPDATE**
  #tar_target(
  #  miningpolys.path,
  #  miningpolys,
  #  format = 'file'
  # ),
#tar_target(
# miningpolys.sf,
#    load_sf(miningpolys.path, crs)
#  ),
  
  # load miningpolys for just Yukon **UPDATE**
# tar_target(
#   miningpolys.yt.path,
#   miningpolys.yt,
#   format = 'file'
# ),
# tar_target(
#   miningpolys.yt.sf,
#   load_sf(miningpolys.yt.path, crs)
# ),

  # load harvest
# tar_target( #OLD ***
#   harv.path,
#   harv,
#   format = 'file'
# ),
# tar_target(
#   harv.sf,
#   make_harv_sf(harv.path)
# ),
  
# tar_target( #seperate target for each type (pa vs dist etc.)
#   harv250.path,
#   harv_250,
#   format = "file"
# ),
  
# tar_target(
#   harv250.sf,
#   make_harv_rast(harv250.path)
# ),
  
  
  # load fires
  # tar_files(
  #   fires.path,
  #   list.files(path = fires_path, full.names = T)
  # ),
 
  # load forest age 
# tar_target(
#   for.age.path,
#   for.age,
#   format = 'file'
# ),
  
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
    extract_by_year(addyear, var = prop_water_250, startyr = minyr, endyr = 2022, maxyr, where = 'both')
  ),

  tar_target(
    extr_propsnow,
    extract_by_year(extr_propwater, var = prop_snow_250, startyr = minyr, endyr = 2022, maxyr, where = 'both')
  ),

  tar_target(
    extr_proprock,
    extract_by_year(extr_propsnow, var = prop_rock_250, startyr = minyr, endyr = 2022, maxyr, where = 'both')
  ),

  tar_target(
    extr_propbarren,
    extract_by_year(extr_proprock, var = prop_barren_250, startyr = minyr, endyr = 2022, maxyr, where = 'both')
  ),

  tar_target(
    extr_propbryoids,
    extract_by_year(extr_propbarren, var = prop_bryoids_250, startyr = minyr, endyr = 2022, maxyr, where = 'both')
  ),

  tar_target(
    extr_propshrub,
    extract_by_year(extr_propbryoids, var = prop_shrub_250, startyr = minyr, endyr = 2022, maxyr, where = 'both')
  ),

  tar_target(
    extr_propwetland,
    extract_by_year(extr_propshrub, var = prop_wetland_250, startyr = minyr, endyr = 2022, maxyr, where = 'both')
  ),

  tar_target(
    extr_propwettreed,
    extract_by_year(extr_propwetland, var = prop_wettreed_250, startyr = minyr, endyr = 2022, maxyr, where = 'both')
  ),

  tar_target(
    extr_propherbs,
    extract_by_year(extr_propwettreed, var = prop_herbs_250, startyr = minyr, endyr = 2022, maxyr, where = 'both')
  ),

  tar_target(
    extr_propneedle,
    extract_by_year(extr_propherbs, var = prop_needleleaf_250, startyr = minyr, endyr = 2022, maxyr, where = 'both')
  ),

  tar_target(
    extr_propdecid,
    extract_by_year(extr_propneedle, var = prop_deciduous_250, startyr = minyr, endyr = 2022, maxyr, where = 'both')
  ),

  tar_target(
    extr_propmixed,
    extract_by_year(extr_propdecid, var = prop_mixed_250, startyr = minyr, endyr = 2022, maxyr, where = 'both')
  ),
  
  tar_target(
    extr_propwater1000,
    extract_by_year(extr_propmixed, var = prop_water_1000, startyr = minyr, endyr = 2022, maxyr, where = 'both')
  ),
  
  tar_target(
    extr_propsnow1000,
    extract_by_year(extr_propwater1000, var = prop_snow_1000, startyr = minyr, endyr = 2022, maxyr, where = 'both')
  ),
  
  tar_target(
    extr_proprock1000,
    extract_by_year(extr_propsnow1000, var = prop_rock_1000, startyr = minyr, endyr = 2022, maxyr, where = 'both')
  ),
  
  tar_target(
    extr_propbarren1000,
    extract_by_year(extr_proprock1000, var = prop_barren_1000, startyr = minyr, endyr = 2022, maxyr, where = 'both')
  ),
  
  tar_target(
    extr_propbryoids1000,
    extract_by_year(extr_propbarren1000, var = prop_bryoids_1000, startyr = minyr, endyr = 2022, maxyr, where = 'both')
  ),
  
  tar_target(
    extr_propshrub1000,
    extract_by_year(extr_propbryoids1000, var = prop_shrub_1000, startyr = minyr, endyr = 2022, maxyr, where = 'both')
  ),
  
  tar_target(
    extr_propwetland1000,
    extract_by_year(extr_propshrub1000, var = prop_wetland_1000, startyr = minyr, endyr = 2022, maxyr, where = 'both')
  ),
  
  tar_target(
    extr_propwettreed1000,
    extract_by_year(extr_propwetland1000, var = prop_wettreed_1000, startyr = minyr, endyr = 2022, maxyr, where = 'both')
  ),
  
  tar_target(
    extr_propherbs1000,
    extract_by_year(extr_propwettreed1000, var = prop_herbs_1000, startyr = minyr, endyr = 2022, maxyr, where = 'both')
  ),
  
  tar_target(
    extr_propneedle1000,
    extract_by_year(extr_propherbs1000, var = prop_needleleaf_1000, startyr = minyr, endyr = 2022, maxyr, where = 'both')
  ),
  
  tar_target(
    extr_propdecid1000,
    extract_by_year(extr_propneedle1000, var = prop_deciduous_1000, startyr = minyr, endyr = 2022, maxyr, where = 'both')
  ),
  
  tar_target(
    extr_propmixed1000,
    extract_by_year(extr_propdecid1000, var = prop_mixed_1000, startyr = minyr, endyr = 2022, maxyr, where = 'both')
  )
)

## anthro disturbance ----
#Distance-to
targets_disturbance <- c(     
  tar_target(
    dist_paved,
    extract_by_year(extr_propmixed1000, var = dist_paved_250, startyr = minyr, endyr = 2024, maxyr, where = 'both')
  ),
  
  tar_target(
    dist_unpaved,
    extract_by_year(dist_paved, var = dist_unpaved_250, startyr = minyr, endyr = 2024, maxyr, where = 'both')
  ),
  
  tar_target(
    dist_other,
    extract_by_year(dist_unpaved, var = dist_lfother_250, startyr = minyr, endyr = 2024, maxyr, where = 'both')
  ),
  
  tar_target(
    dist_otherpolys,
    extract_by_year(dist_other, var = dist_otherpoly_250, startyr = minyr, endyr = 2024, maxyr, where = 'both')
  ),
  
  tar_target(
    dist_miningpolys,
    extract_by_year(dist_otherpolys, var = dist_othermines_250, startyr = minyr, endyr = 2024, maxyr, where = 'both')
  ),
  
  tar_target(
    dist_placer,
    extract_by_year(dist_miningpolys, var = dist_placer_250, startyr = minyr, endyr = 2024, maxyr, where = 'both')
  ),
  
  tar_target(
    dist_quartz,
    extract_by_year(dist_placer, var = dist_quartz_250, startyr = minyr, endyr = 2024, maxyr, where = 'both')
  ),
  
  
#Presence/absence
  tar_target(
    pa_paved,
    extract_by_year(dist_quartz, var = pa_paved_250, startyr = minyr, endyr = 2024, maxyr, where = 'both')
  ),
  
  tar_target(
    pa_unpaved,
    extract_by_year(pa_paved, var = pa_unpaved_250, startyr = minyr, endyr = 2024, maxyr, where = 'both')
  ),
  
  tar_target(
    pa_other,
    extract_by_year(pa_unpaved, var = pa_lfother_250, startyr = minyr, endyr = 2024, maxyr, where = 'both')
  ),
  
  tar_target(
    pa_otherpolys,
    extract_by_year(pa_other, var = pa_otherpoly_250, startyr = minyr, endyr = 2024, maxyr, where = 'both')
  ),
  
  tar_target(
    pa_miningpolys,
    extract_by_year(pa_otherpolys, var = pa_othermines_250, startyr = minyr, endyr = 2024, maxyr, where = 'both')
  ),
  
  tar_target(
    pa_placer,
    extract_by_year(pa_miningpolys, var = pa_placer_250, startyr = minyr, endyr = 2024, maxyr, where = 'both')
  ),
  
  tar_target(
    pa_quartz,
    extract_by_year(pa_placer, var = pa_quartz_250, startyr = minyr, endyr = 2024, maxyr, where = 'both')
  ),
  
  
#Density
  tar_target(
    dens_paved,
    extract_by_year(pa_quartz, var = dens_paved_250, startyr = minyr, endyr = 2024, maxyr, where = 'both')
  ),
  
  tar_target(
    dens_unpaved,
    extract_by_year(dens_paved, var = dens_unpaved_250, startyr = minyr, endyr = 2024, maxyr, where = 'both')
  ),
  
  tar_target(
    dens_other,
    extract_by_year(dens_unpaved, var = dens_lfother_250, startyr = minyr, endyr = 2024, maxyr, where = 'both')
  ),
  
  
#Distance to harv
  tar_target(
    dist_harv,
    extract_by_year(dens_other, var = harv_dist_250, startyr = minyr, endyr = 2020, maxyr, where = 'both')
  ),
#Harv presence/absence
tar_target(
  pa_harv,
  extract_by_year(dist_harv, var = harv_pa_250, startyr = minyr, endyr = 2020, maxyr, where = 'both')
 ),


#1000
tar_target(
  dist_paved1000,
  extract_by_year(pa_harv, var = dist_paved_1000, startyr = minyr, endyr = 2024, maxyr, where = 'both')
),

tar_target(
  dist_unpaved1000,
  extract_by_year(dist_paved1000, var = dist_unpaved_1000, startyr = minyr, endyr = 2024, maxyr, where = 'both')
),

tar_target(
  dist_other1000,
  extract_by_year(dist_unpaved1000, var = dist_lfother_1000, startyr = minyr, endyr = 2024, maxyr, where = 'both')
),

tar_target(
  dist_otherpolys1000,
  extract_by_year(dist_other1000, var = dist_otherpoly_1000, startyr = minyr, endyr = 2024, maxyr, where = 'both')
),

tar_target(
  dist_miningpolys1000,
  extract_by_year(dist_otherpolys1000, var = dist_othermines_1000, startyr = minyr, endyr = 2024, maxyr, where = 'both')
),

tar_target(
  dist_placer1000,
  extract_by_year(dist_miningpolys1000, var = dist_placer_1000, startyr = minyr, endyr = 2024, maxyr, where = 'both')
),

tar_target(
  dist_quartz1000,
  extract_by_year(dist_placer1000, var = dist_quartz_1000, startyr = minyr, endyr = 2024, maxyr, where = 'both')
),


#Presence/absence
tar_target(
  pa_paved1000,
  extract_by_year(dist_quartz1000, var = pa_paved_1000, startyr = minyr, endyr = 2024, maxyr, where = 'both')
),

tar_target(
  pa_unpaved1000,
  extract_by_year(pa_paved1000, var = pa_unpaved_1000, startyr = minyr, endyr = 2024, maxyr, where = 'both')
),

tar_target(
  pa_other1000,
  extract_by_year(pa_unpaved1000, var = pa_lfother_1000, startyr = minyr, endyr = 2024, maxyr, where = 'both')
),

tar_target(
  pa_otherpolys1000,
  extract_by_year(pa_other1000, var = pa_otherpoly_1000, startyr = minyr, endyr = 2024, maxyr, where = 'both')
),

tar_target(
  pa_miningpolys1000,
  extract_by_year(pa_otherpolys1000, var = pa_othermines_1000, startyr = minyr, endyr = 2024, maxyr, where = 'both')
),

tar_target(
  pa_placer1000,
  extract_by_year(pa_miningpolys1000, var = pa_placer_1000, startyr = minyr, endyr = 2024, maxyr, where = 'both')
),

tar_target(
  pa_quartz1000,
  extract_by_year(pa_placer1000, var = pa_quartz_1000, startyr = minyr, endyr = 2024, maxyr, where = 'both')
),


#Density
tar_target(
  dens_paved1000,
  extract_by_year(pa_quartz1000, var = dens_paved_1000, startyr = minyr, endyr = 2024, maxyr, where = 'both')
),

tar_target(
  dens_unpaved1000,
  extract_by_year(dens_paved1000, var = dens_unpaved_1000, startyr = minyr, endyr = 2024, maxyr, where = 'both')
),

tar_target(
  dens_other1000,
  extract_by_year(dens_unpaved1000, var = dens_lfother_1000, startyr = minyr, endyr = 2024, maxyr, where = 'both')
),


#Distance to harv
tar_target(
  dist_harv1000,
  extract_by_year(dens_other1000, var = harv_dist_1000, startyr = minyr, endyr = 2020, maxyr, where = 'both')
),
#Harv presence/absence
tar_target(
  pa_harv1000,
  extract_by_year(dist_harv1000, var = harv_pa_1000, startyr = minyr, endyr = 2020, maxyr, where = 'both')
)
)


## time since ----
targets_timesince <- c(   
  # Extract forest age
  tar_target(
    extr_for.age,
    extract_by_year(pa_harv1000, var = for_age_250, startyr = minyr, endyr = 2024, maxyr, where = 'both')
  ),
  
  # Extract harvest
  tar_target(
    extr_harv,
    extract_by_year(extr_for.age, var = harv250, startyr = minyr, endyr = 2020, maxyr, where = 'both')
  ),

  # calculate time since harvest
  tar_target(
    ts_harv,
    calc_ts(extr_harv, var = 'harv250', where = 'both', no.data=100)
  ),
  
  # Extract fires
  tar_target(
    extr_fires,
    extract_by_year(ts_harv, var = fires_250, startyr = minyr, endyr = 2024, maxyr, where = 'both')
  ),
  
  # calculate time since fire
  tar_target(
    ts_fire,
    calc_ts(extr_fires, var = 'fires_250', where = 'both', no.data=200) 
  ),
  
  
  tar_target(
    extr_for.age1000,
    extract_by_year(ts_fire, var = for_age_1000, startyr = minyr, endyr = 2024, maxyr, where = 'both')
  ),
  
  # Extract harvest
  tar_target(
    extr_harv1000,
    extract_by_year(extr_for.age1000, var = harv1000, startyr = minyr, endyr = 2020, maxyr, where = 'both')
  ),
  
  # calculate time since harvest
  tar_target(
    ts_harv1000,
    calc_ts(extr_harv1000, var = 'harv1000', where = 'both', no.data=100)
  ),
  
  # Extract fires
  tar_target(
    extr_fires1000,
    extract_by_year(ts_harv1000, var = fires_1000, startyr = minyr, endyr = 2024, maxyr, where = 'both')
  ),
  
  # calculate time since fire
  tar_target(
    ts_fire1000,
    calc_ts(extr_fires1000, var = 'fires_1000', where = 'both', no.data=200) 
  )
)



## Step ID ----
targets_stepID <- c(
  # create step ID across individuals
  tar_target(
    stepID,
    setDT(ts_fire1000)[,indiv_step_id := paste(id, step_id_, sep = '_')]
  )
)


# Targets: all ------------------------------------------------------------------
# Automatically grab and combine all the 'targets_*' lists above
lapply(grep('targets', ls(), value = TRUE), get)

