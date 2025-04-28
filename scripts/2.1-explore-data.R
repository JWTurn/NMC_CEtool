### Explore Data ====
# Julie Turner
# Started: November 2 2023
#Modified: April 28 2025 PB
require('data.table')
require('amt')
require('dplyr')
require('tidyr')
require('sf')
require('ggplot2')
require('readxl')


### Input data ----
raw <- file.path('data','raw')
derived <- file.path('data','derived')
canada <- file.path('~/Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')

dat.in <- readRDS(file.path(derived, 'prepDat_nmc.RDS'))
dat <- readRDS(file.path(derived, 'clean_NMC.RDS'))
mort <- setDT(read_excel(file.path(raw, "Mortality Tracker NEW_20240429.xlsx"), sheet = 'Caribou Mort Tracker'))

crs <- st_crs(3154)

# some exploration
# quick clean of raw data
dat.cc <- unique(dat.in[!is.na(id)&id!=""&complete.cases(latitude, longitude, datetime)], by = c('id', 'datetime'))


dat.cc[!is.na(id),uniqueN(id), by = prov]

dat.cc[!is.na(id),.(min = min(datetime, na.rm = T), max = max(datetime, na.rm = T)), by = prov]


dat[,uniqueN(id), by = prov]

dat[,.(min = min(t_, na.rm = T), max = max(t_, na.rm = T)), by = prov]

mort[,uniqueN(ANIM_ID), by = `ANIMAL FATE/ COLLAR STATUS`]

mort[ANIM_ID %in% unique(dat$id),uniqueN(ANIM_ID), by = `ANIMAL FATE/ COLLAR STATUS`]
mort[ANIM_ID %in% unique(dat.cc$id),uniqueN(ANIM_ID), by = `ANIMAL FATE/ COLLAR STATUS`]

# checking fix rate now clean
fixrate.clean <- dat %>% make_track(x_,y_, t_, crs = crs, all_cols = T) %>% 
  nest(data = -"id") %>% 
  mutate(sr = lapply(data, summarize_sampling_rate)) %>%
  dplyr::select(id, sr) %>% unnest(cols = c(sr))
range(fixrate.clean$mean)
median(fixrate.clean$mean)
range(fixrate.clean$median)
median(fixrate.clean$median)
# 8 hours
mean(fixrate.clean$median)
# 10.9


### steps ----
steps <- 
  dat %>% 
  nest(data = -id) %>% 
  mutate(track = map(data, ~ mk_track(., x_, y_, t_, crs= crs)))
step.lengths <- unlist(map(steps$track, step_lengths))
hist(step.lengths, ylim = c(0, 1000), main="Histogram of step lengths", xlab="Step lengths")



trk.fr <- dat |> 
  make_track(x_, y_, t_, all_cols = T, crs = crs) |> 
  nest(data = -"id") |> 
  mutate(resamp = map(data, function(x) 
    x |> track_resample(rate = hours(8), tolerance = minutes(90))))

# trk.fr %>% unnest(steps) %>%
#   inspect(cluster=FALSE)

trk.fr %>% unnest(resamp) %>%
  plot(main = "8 hour fix rate")

steps.fr <- trk.fr |> mutate(steps = map(resamp, function(x)
  x |> steps_by_burst()))
steps.fr |> select(id,  steps)|>
  unnest(cols = steps)|> 
  ggplot(aes(sl_)) + geom_density(alpha = 0.4)
steps.fr |> select(id,  steps)|>
  unnest(cols = steps)|> 
  ggplot(aes(ta_)) + geom_density(alpha = 0.4)
steps.id <- steps.fr |> select(id,  steps)|>
  unnest(cols = steps)

quantile(steps.id$sl_)
# 0%         25%         50%         75%        100% 
# 0.0000   173.8541   494.0337  1238.2478 32015.2891 
quantile(steps.id$sl_, probs = c(0.9, 0.95, 0.99))
# 90%      95%      99% 
# 2574.592 3913.722 8067.054  
mean(steps.id$sl_)
# [1] 1046.018
median(steps.id$sl_)
# [1] 494.0337



# trying a bigger fixrate
trk.fr13 <- dat |> 
  make_track(x_, y_, t_, all_cols = T, crs = crs) |> 
  nest(data = -"id") |> 
  mutate(resamp = map(data, function(x) 
    x |> track_resample(rate = hours(13), tolerance = minutes(90))))

# trk.fr %>% unnest(steps) %>%
#   inspect(cluster=FALSE)

trk.fr13 %>% unnest(resamp) %>%
  plot(main = "13 hour fix rate")

steps.fr13 <- trk.fr13 |> mutate(steps = map(resamp, function(x)
  x |> steps_by_burst()))

steps.fr13 |> select(id,  steps)|>
  unnest(cols = steps)|> 
  ggplot(aes(sl_)) + geom_density(alpha = 0.4)
steps.fr13 |> select(id,  steps)|>
  unnest(cols = steps)|> 
  ggplot(aes(ta_)) + geom_density(alpha = 0.4)
steps.id13 <- steps.fr13 |> select(id,  steps)|>
  unnest(cols = steps)
# this one has more points
quantile(steps.id13$sl_)
# 0%       25%       50%       75%      100% 
# 0.0000   274.0547   747.9967  1741.7375 82427.1468 
quantile(steps.id13$sl_, probs = c(0.9, 0.95, 0.99))
# 90%      95%      99% 
# 3475.857  5153.962 10157.446 
mean(steps.id13$sl_)
# [1] 1438.687

