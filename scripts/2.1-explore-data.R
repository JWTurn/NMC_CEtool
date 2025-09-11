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
#dat.new <- readRDS(file.path(derived, 'clean_NMC.RDS'))
#dat.old <- readRDS(file.path(derived, 'clean_NMC_old.RDS'))
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
# 10.5119


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
#  0%        25%        50%        75%       100% 
#0.0000   172.0587   486.6992  1220.7022 32015.2891 
quantile(steps.id$sl_, probs = c(0.9, 0.95, 0.99))
# 90%      95%      99% 
# 2541.657 3868.712 7978.692   
mean(steps.id$sl_)
# [1] 1033.364
median(steps.id$sl_)
# [1] 486.6992



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
# 0.0000   275.1718   748.6220  1743.0098 82427.1468 
quantile(steps.id13$sl_, probs = c(0.9, 0.95, 0.99))
# 90%      95%      99% 
# 3480.750  5165.166 10171.974 
mean(steps.id13$sl_)
# [1] 1440.936


#Summary stats

# katie updates
unique(dat$prov)
# combine nt and yt
dat[prov == "nt", prov := "yt"]
# combine prairie creeks
dat[area == "Prairie Creek Zinc", area := "PrairieCreek"]
# combine prairie creeks
dat[area == "Prairie Creek", area := "PrairieCreek"]
# combine little rancherias
dat[area == "LittleRancheriaYT", area := "LittleRancheria"]
# combine little rancherias
dat[area == "LittleRancheriaBC", area := "LittleRancheria"]
# combine little rancherias
dat[area == "Little Rancheria", area := "LittleRancheria"]
n_distinct(dat$area)
unique(dat$area)

dat
unique_ids_per_prov <- dat[, .(unique_ids = uniqueN(id)), by = prov]
print(unique_ids_per_prov)

ids <- fixrate.clean$id            # vector of ids
means <- fixrate.clean$mean        # vector of mean fix rates
medians <- fixrate.clean$median    # vector of median fix rates
id_prov_map <- setNames(as.character(dat$prov), as.character(dat$id))
provs <- id_prov_map[as.character(ids)]

aggregate_by_prov <- function(values, groups, FUN) {
  tapply(values, groups, FUN, na.rm = TRUE)
}

mean_fixrate_mean <- aggregate_by_prov(means, provs, mean)
median_fixrate_mean <- aggregate_by_prov(means, provs, median)
mean_fixrate_median <- aggregate_by_prov(medians, provs, mean)
median_fixrate_median <- aggregate_by_prov(medians, provs, median)

summary_df <- data.frame(
  prov = names(mean_fixrate_mean),
  mean_fixrate_mean = as.numeric(mean_fixrate_mean),
  median_fixrate_mean = as.numeric(median_fixrate_mean),
  mean_fixrate_median = as.numeric(mean_fixrate_median),
  median_fixrate_median = as.numeric(median_fixrate_median),
  stringsAsFactors = FALSE
)

print(summary_df)


#How many indivs in the filtered timing datasets?
library(lubridate)
dat[, ym := format(t_, "%Y-%m")]
months_per_id <- dat[, .(n_months = uniqueN(ym)), by = .(id, prov)]
valid_ids <- months_per_id[n_months > 9]
individuals_per_prov <- valid_ids[, .N, by = prov]
print(individuals_per_prov)



dat[, `:=`(
  year = year(t_),
  month = month(t_),
  day = day(t_)
)]

dat[, season := fifelse(
  (month %in% c(11, 12, 1, 2, 3)) | (month == 4 & day <= 15),
  "winter",
  fifelse(month %in% c(7, 8, 9), "summer", NA_character_)
)]

dat[, ym := format(t_, "%Y-%m")]

season_months <- dat[!is.na(season), .(n_months = uniqueN(ym)), by = .(id, prov, season)]

season_wide <- dcast(season_months, id + prov ~ season, value.var = "n_months", fill = 0)

summary <- season_wide[, .(
  n_summer_ids = sum(summer >= 1),
  n_winter_ids = sum(winter >= 3)
), by = prov]

print(summary)





