### Explore Data ====
# Julie Turner
# Started: August 16 2023

require('data.table')
require('amt')
require('dplyr')
require('tidyr')
require('sf')


### Input data ----
raw <- file.path('data','raw')
derived <- file.path('data','derived')

dat <- readRDS(file.path(derived, 'prepDat_nmc.RDS'))

### check fix rates ####
# check for duplicated time stamps
dat[,any(duplicated(datetime)), by = .(id)]


# We have some duplicated time stamps, these need to be removed prior to creating a track.
dat <- unique(dat, by = c('id', 'datetime'))
dat[,npts := .N, by = .(id)]
range(dat$npts)
dat3 <- dat[npts>=3]

# limit to those with >3 pts
trk <- dat3 %>% make_track(x,y, datetime, crs = st_crs(3978), id = id) %>% 
  nest(data = -"id")
fixrate <- trk %>% mutate(sr = lapply(data, summarize_sampling_rate)) %>%
  dplyr::select(id, sr) %>% unnest(cols = c(sr))
range(fixrate$mean)
median(fixrate$mean)
range(fixrate$median)
median(fixrate$median)
# 9 hours
mean(fixrate$median)
# 11
