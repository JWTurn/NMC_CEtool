### Clean Data ====
# Julie Turner
# Started: August 16 2023

require('data.table')
require('amt')
require('dplyr')
require('tidyr')
require('sf')
require(ggplot2)
#require(terra)


### Input data ----
raw <- file.path('data','raw')
derived <- file.path('data','derived')
#canada <- file.path('~/Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')
canada <- file.path('C:','Users', 'julie', 'Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')

dat.in <- readRDS(file.path(derived, 'prepDat_nmc.RDS'))

# there are definitely outliers/wrong points
range(dat.in$longitude, na.rm = T)
# [1] -140.50307   13.53413
range(dat.in$latitude, na.rm = T)
# [1]  0.00000 65.17495


crs <- st_crs(4326)$wkt
outcrs <- st_crs(3154)
# TRY
# 3579 NAD83 (CSRS)/ Yukon Albers
# 3154 NAD83 (CSRS)/ UTM zone 7N ---- THIS ONE WORKING
# didn't work
# 3978, 3979

canPoly <- st_read(file.path(canada, 'CanadaPoly', 'lpr_000b16a_e.shp'))
# really using YT, BC, and NWT because of transboundary herds
ytbc <- subset(canPoly, canPoly$PREABBR %in% c('B.C.', 'Y.T.', 'N.W.T'))
ytbc.wgs <- st_transform(ytbc, crs)
ytbc.ext <- st_bbox(ytbc.wgs)


# make sure all have id, long, and lat
dat.cc <- dat.in[complete.cases(id, longitude, latitude)]
dat.sf <- st_as_sf(dat.cc, coords = c('longitude', 'latitude'),
                  crs = crs)

# filter data to be within the extent of YT/BC
#dat.within <- st_filter(dat.sf, ytbc.wgs, .pred = st_within)
#dat.within <- st_join(dat.sf, ytbc.wgs, join = st_within)
# dat.filter <- filter(dat.within, !is.na(PREABBR))
# plot(dat.filter$geometry)
dat.within <- st_crop(dat.sf, ytbc.ext)
plot(dat.within$geometry)


DT.filter <- setDT(sfheaders::sf_to_df(dat.within, fill = T))


# convert from long/lat to NAD83/Canada Atlas Lambert (need units to be m)
range(DT.filter$x, na.rm = T)
# [1] -140.5031 -117.3002
range(DT.filter$y, na.rm = T)
# [1] 49.44640 65.17495
quantile(DT.filter$y, na.rm = T)
# 0%      25%      50%      75%     100% 
# 49.44640 58.65384 60.10138 61.92756 65.17495 

# filter out those that are from the office further south
DT.filter2 <- DT.filter[y>50]

filter.sf <- st_as_sf(DT.filter2, coords = c('x', 'y'),
                  crs = crs)
plot(filter.sf$geometry)
out.filter.sf <- st_transform(filter.sf, outcrs)
dat.trans <- setDT(sfheaders::sf_to_df(out.filter.sf, fill = T))



### initial clean ####
# remove old collars with infrequent fixes
dat.gps <- dat.trans[!(duty_cycle %in% c("1 fix every 1-7 d (varied by season see notes)", 
                                  "1 fix every 5 d", '23-hourly fixes'))]
# check for duplicated time stamps
dat.gps[,any(duplicated(datetime)), by = .(id)]


# We have some duplicated time stamps, these need to be removed prior to creating a track.
dat <- unique(dat.gps[complete.cases(x,y, datetime)], by = c('id', 'datetime'))

# check out dates before deployment or after - Good QC for YT prior to Movebank, who knows BC
dat[,early := ifelse(datetime<deploy_on_timestamp, T, F)]
summary(dat$early)
dat[,late := ifelse(datetime>deploy_off_timestamp, T, F)]
summary(dat$late)
dat[,npts := .N, by = .(id)]
range(dat$npts)
# limit to those with >3 pts
dat3 <- dat[npts>=3]

sum.pts <- dat3 %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(prov = unique(prov),
                   deployment.date = as.Date(as.character(min(datetime))),
                   last.date = as.Date(as.character(max(datetime))),
                   number.fix = n_distinct(datetime))
# there are 11659 fixes from BC with no id... oy veh 
# also NAs
dat.ids <- dat3[!is.na(id)&id!=""]



coords<- dat.ids%>%st_as_sf(coords = c('x','y'))%>%
  st_set_crs(outcrs)
#st_write(coords, file.path(derived, 'prepped', 'NMCprepDat.shp'), append = F)

plot(coords$geometry)

# ok major weirdos gone but still some likely not real points

# Clean data for movement ----
trk <- dat.ids %>%
  mk_track(.x = x,
           .y= y,
           .t = datetime,
           crs=outcrs,
           all_cols = T) 
# Now we plot it
plot(trk, main = "All Data")
lines(trk)

# We can also take a look on a map.
trk %>%
 inspect(cluster=FALSE)

collar.summaries <- trk %>%
  dplyr::group_by(id) %>%
  dplyr::summarise(prov = unique(prov),
                   deployment.date = as.Date(as.character(min(t_))),
                   last.date = as.Date(as.character(max(t_))),
                   number.fix = n_distinct(t_))

# ... 1. pre-deployment ----
# Already filtered for data after deployment 

# ... 2. capture effect ----
# Recall that we said that previous research showed the capture effect can last
# up to 7 days in our study species.
trk3 <- trk %>%
 remove_capture_effect(start = days(7))


# ... 3. Unreachable habitat ----
# Skipping this for now


# ... 4. Low-quality fixes ----
# We want to remove any fix with very high DOP. A good cutoff for this is 10,
# but if you have a very low incidence of lower qualities, you can go lower.
hist(trk3$gps_dop,
     ylim=c(0,50000))

# Notice that most of our fixes are 0 - 6. 

trk4 <- trk3 %>% 
  filter(as.numeric(gps_dop) < 10 |is.na(gps_dop)) # many don't have dop info, so keeping them, otherwise no bc



# ... 5. Low-quality duplicates ----
# Let's check our fix rates for where there might be duplicates
trkid <- trk4 %>% 
  nest(data = -"id")
fixrate <- trkid %>% mutate(sr = lapply(data, summarize_sampling_rate)) %>%
  dplyr::select(id, sr) %>% unnest(cols = c(sr))
range(fixrate$mean)
median(fixrate$mean)
range(fixrate$median)
median(fixrate$median)
# 8 hours
mean(fixrate$median)
# 11.3

# quick check steps

step.lengths <- unlist(map(trkid$data, step_lengths))
hist(step.lengths, main="Histogram of step lengths", xlab="Step lengths", ylim = c(0,1000))

median(step.lengths, na.rm = T)


quantile(diff(trk4$t_))
# Time differences in secs
# 0%    25%    50%    75%   100% 
# 0      0      0      2 601200 

#  For hourly data, much larger values are still safe.

# making a toy dop col so can filter duplicates but keep unknown DOPs
trk4$dop2 <- ifelse(is.na(trk4$gps_dop), 11, trk4$gps_dop)
trk5 <- trk4 %>% 
  arrange(id, t_) %>% 
  nest(data = -id) %>%
  mutate(data = map(data, ~flag_duplicates(.x, gamma = minutes(15), DOP = "dop2"))) %>%
  unnest(data) 
head(trk5)

# Recall that this function just flags duplicates -- you may decide you didn't
# like your gamma parameter and want to try again. Let's look.
trk5 %>% 
  filter(duplicate_)


# Let's remove them.
trk6 <- trk5 %>% 
  filter(!duplicate_) # removed a lot of duplicates, particularly from BC...


# ... 6. Unreasonably fast steps ----
# This is where we need to decide what "unreasonably fast" means.
# Let's look at the data to help decide.
hist(sdr(trk6, time_unit = "mins"), breaks = 100000,
      xlim = range(0,600000),
      ylim = range(0,10000),
     col = "red")



# We see a big drop off by the time we get to SDR = 200000 Let's use that.
trk7 <- trk6 %>% 
  arrange(id, t_) %>% 
  nest(data = -id) %>%
  mutate(data = map(data, ~flag_fast_steps(.x, delta=200000))) %>%
  unnest(data) 
head(trk7)


# How did we do?
trk7 %>% 
  filter(fast_step_)

# We're going to remove a couple of "real" locations and a "fast_rt" location,
# but we mostly got it right. Here, we can afford to lose a few "real" locations
# for the sake of cleaning out bad ones.

# Get rid of them
trk8 <- trk7 %>% 
  filter(!fast_step_) #

plot(trk8,
     pch=16)


# ... 7. Fast roundtrips ----
# We can use the delta (=100000) from before, but now we also have to decide on
# epsilon. A good way to do this is to try a few different values and see
# how many steps you flag.
# ** as per discussion with Tal; Keep epsilon at 10 and change delta
# delta has to do with the speed, so a lower delta is slower movement... 
# "the threshold squared displacement rate (SDR) for flagging. Locations
# that imply both legs of a roundtrip with SDR > delta/epsilon are flagged.
# epsilon is a scaling factor to create the threshold for flagging



# delta = 100000:----
trk8a <- trk8 %>% 
  nest(data = -id) %>%
  mutate(data = map(data, ~flag_roundtrips(.x, delta = 100000, epsilon = 10))) %>%
  unnest(data)
# How many?
trk8a %>% 
  filter(fast_roundtrip_) #93

## Get the entries where fast roundtrip == TRUE
flagged.entries <- trk8a %>% 
  filter(fast_roundtrip_) %>% #68 observations
  dplyr::mutate(flag.number = 1:n())

## Select defining columns of interest (we only want id*date combinations that had a fast roundtrip)
flagged.entries.idonly <- flagged.entries %>%
  dplyr::select(c(id, t_, flag.number))

## Now get all the data points for the individuals*respective dates of interest
flagged.entries.data <- flagged.entries.idonly %>%
  left_join(., trk8a, by = c("id", "t_")) 

## Plot to see
colours <- c("black",
             "red")

flagged.entries.data %>%
  ggplot(aes(x=x_, y=y_)) +
  geom_path() +
  geom_point(aes(col=fast_roundtrip_)) +
  scale_color_manual(values = colours) +
  theme_bw() +
  facet_wrap(~id*t_, scales = "free") +
  theme(strip.background = element_rect(fill = NA)) +
  ggtitle("delta = 100000; epsilon = 10")
# they all look like they'd be a fast roundtrip/jump between collar uses


trk8.remove <- trk8a %>% 
  filter(!fast_roundtrip_) 


# ... 8. Defunct clusters ----

# By looking at the plot, we can see that the cluster at the end lasted for a
# while. Say we have some idea that it was there for at least two days (maybe
# from a mortality signal or a scheduled drop-off). What do the last 48
# points look like?
## Get mortality dates

lasts <- trk8.remove %>% 
  nest(data = -id) %>%
  mutate(data = map(data, ~tail(.x, 14))) %>%
  unnest(data)

#lasts <- tail(trk7, 48)
plot(lasts)
lines(lasts)

# there may be some clusters, hard to see at this scale
# if a cluster is about 50 m across, so maybe we should consider zeta to be
# 25. Again, we can check what really gets flagged and adjust if necessary.

# Try zeta = 25, 50, 100
trk9 <- trk8.remove %>% 
  flag_defunct_clusters(zeta = 200,
                        eta = 3,
                        theta = hours(48))

# not picking up any clusters
trk9 %>% 
  filter(defunct_cluster_) 


# final ----
final <- trk9 # down to 1263050 from 1505945

final.dt <- setDT(final)

saveRDS(final.dt, file.path(derived, 'clean_NMC.RDS'))

coords.final <- final.dt %>% st_as_sf(coords = c('x_','y_'))%>%
  st_set_crs(outcrs)
plot(coords.final$geometry)
st_write(coords.final, file.path(derived, "prepped", 'clean_NMC.shp'), append = F)


### 20 km buffer around points to get an idea of extent of study area
studyArea <- st_buffer(coords.final, dist = 20000)
sa.union <- st_union(studyArea)
sa.union
plot(sa.union)
#sa.vect <- vect(sa.union)
st_write(sa.union, file.path(derived, 'prepped', 'clean_NMC_20kmBuff.shp'), append = F)


# checking fix rate now clean
fixrate.clean <- trk9 %>% make_track(x_,y_, t_, crs = crs, all_cols = T) %>% 
  nest(data = -"id") %>% 
  mutate(sr = lapply(data, summarize_sampling_rate)) %>%
  dplyr::select(id, sr) %>% unnest(cols = c(sr))
range(fixrate$mean)
median(fixrate$mean)
range(fixrate$median)
median(fixrate$median)
# 8 hours
mean(fixrate$median)
# 11.3
### steps ----
steps <- 
  final %>% 
  nest(data = -id) %>% 
  mutate(track = map(data, ~ mk_track(., x_, y_, t_, crs= outcrs)))
step.lengths <- unlist(map(steps$track, step_lengths))
hist(step.lengths, ylim = c(0, 1000), main="Histogram of step lengths", xlab="Step lengths")

range(step.lengths, na.rm = T)
quantile(step.lengths, na.rm = T)
# 0%          25%          50%          75%         100% 
# 0.0000     134.7577     453.0335    1261.1896 1004079.8111 
quantile(step.lengths, na.rm = T, probs = c(0.9, 0.95, 0.99))
# 90%      95%      99% 
# 2777.656 4302.666 9245.579
mean(step.lengths, na.rm = T)
# 1210.56

# there are some REALLY big steps
