# === iSSA -------------------------------------
# Julie Turner & Peter Baker
# started 14 August 2024

#### Packages ####
libs <- c('data.table', 'dplyr', 'glmmTMB', 'broom.mixed', 'performance', 'tidyr', 'tibble')
lapply(libs, require, character.only = TRUE)

# my functions
lapply(dir('R', '*.R', full.names = TRUE), source)

### Input data ----
raw <-  file.path('data', 'raw')
derived <- file.path('data', 'derived')

dat <- targets::tar_read(stepID)

#dat[case_ == 1, .(mean_sl = mean(sl_), median_sl = median(sl_))]


### explore ----
dat[case_==TRUE,.(quantile(sl_)), by = .(prov)]

dat[, dt_num := as.numeric(dt_)]
dat[case_==TRUE,.(median(sl_/dt_num)), by = .(prov)]

summary(dat$ts_fires_1000_end) #Mostly 200 year old fires, looks good
hist(dat$ts_fires_1000_end)

#dat[ts_fires_end > 10000, ts_fires_end:= 25] # setting this to 20 for now because most were in 1990-2000, but I'll need to figure out something later
head(dat)

dat[,.(dist_to_paved = mean(dist_paved_1000_end, na.rm = T), dist_to_unpaved = mean(dist_unpaved_1000_end, na.rm = T),
       dist_to_lfother = mean(dist_lfother_1000_end, na.rm = T), dist_to_anthropolys = mean(dist_otherpoly_1000_end, na.rm =T),
       dist_to_harv = mean(harv_dist_1000_end, na.rm = T), dist_to_othermines = mean(dist_othermines_1000_end, na.rm = T))]

dat[prov == 'yt', .(dist_to_placer = mean(dist_placer_1000_end, na.rm = T), dist_to_quartz = mean(dist_quartz_1000_end, na.rm = T))]

dat[,.(dist_to_paved = min(dist_paved_1000_end, na.rm = T), dist_to_unpaved = min(dist_unpaved_1000_end, na.rm = T),
       dist_to_lfother = min(dist_lfother_1000_end, na.rm = T), dist_to_anthropolys = min(dist_otherpoly_1000_end, na.rm =T),
       dist_to_harv = min(harv_dist_1000_end, na.rm = T), dist_to_othermines = min(dist_othermines_1000_end, na.rm = T))]

dat[prov == 'yt', .(dist_to_placer = min(dist_placer_1000_end, na.rm = T), dist_to_quartz = min(dist_quartz_1000_end, na.rm = T))]


dat[,.(water = mean(prop_water_1000_end, na.rm = T), snow = mean(prop_snow_1000_end, na.rm = T),
       rock = mean(prop_rock_1000_end, na.rm = T), barren = mean(prop_barren_1000_end, na.rm = T),
       bryoids = mean(prop_bryoids_1000_end, na.rm = T), shrub = mean(prop_shrub_1000_end, na.rm = T),
       wetland = mean(prop_wetland_1000_end, na.rm = T), wettreed = mean(prop_wettreed_1000_end, na.rm = T), 
       herbs = mean(prop_herbs_1000_end, na.rm = T), needle =mean(prop_needleleaf_1000_end, na.rm = T),
       decid = mean(prop_deciduous_1000_end, na.rm = T),mixed = mean(prop_mixed_1000_end, na.rm = T),
       for.age = mean(for_age_1000_end, na.rm = T))]

quantile(dat$ts_fires_1000_end, probs = c(0.01, 0.05, 0.1, 0.15, 0.25, 0.5, 0.75, 1))

#combining landcover types
#250
dat$prop_veg_250_end = dat$prop_bryoids_250_end + dat$prop_herbs_250_end + dat$prop_shrub_250_end #+ dat$prop_wetland_end
mean(dat$prop_veg_250_end, na.rm = T)
dat$prop_veg_250_start = dat$prop_bryoids_250_start + dat$prop_herbs_250_start + dat$prop_shrub_250_start + dat$prop_wetland_250_start
mean(dat$prop_veg_250_start, na.rm = T)

dat$prop_forest_250_end = dat$prop_wettreed_250_end + dat$prop_needleleaf_250_end + dat$prop_deciduous_250_end + dat$prop_mixed_250_end
mean(dat$prop_forest_250_end, na.rm = T)
dat$prop_forest_250_start = dat$prop_wettreed_250_start + dat$prop_needleleaf_250_start + dat$prop_deciduous_250_start + dat$prop_mixed_250_start
mean(dat$prop_forest_250_start, na.rm = T)

dat$dist_mines_250_start = pmin(dat$dist_placer_250_start, dat$dist_quartz_250_start, dat$dist_othermines_250_start, na.rm = T)
dat$dist_mines_250_end = pmin(dat$dist_placer_250_end, dat$dist_quartz_250_end, dat$dist_othermines_250_end, na.rm = T)


#1000
dat$prop_veg_1000_end = dat$prop_bryoids_1000_end + dat$prop_herbs_1000_end + dat$prop_shrub_1000_end #+ dat$prop_wetland_end
mean(dat$prop_veg_1000_end, na.rm = T)
dat$prop_veg_1000_start = dat$prop_bryoids_1000_start + dat$prop_herbs_1000_start + dat$prop_shrub_1000_start + dat$prop_wetland_1000_start
mean(dat$prop_veg_1000_start, na.rm = T)

dat$prop_forest_1000_end = dat$prop_wettreed_1000_end + dat$prop_needleleaf_1000_end + dat$prop_deciduous_1000_end + dat$prop_mixed_1000_end
mean(dat$prop_forest_1000_end, na.rm = T)
dat$prop_forest_1000_start = dat$prop_wettreed_1000_start + dat$prop_needleleaf_1000_start + dat$prop_deciduous_1000_start + dat$prop_mixed_1000_start
mean(dat$prop_forest_1000_start, na.rm = T)

dat$dist_mines_1000_start = pmin(dat$dist_placer_1000_start, dat$dist_quartz_1000_start, dat$dist_othermines_1000_start, na.rm = T)
dat$dist_mines_1000_end = pmin(dat$dist_placer_1000_end, dat$dist_quartz_1000_end, dat$dist_othermines_1000_end, na.rm = T)

#5000
dat$prop_veg_5000_end = dat$prop_bryoids_5000_end + dat$prop_herbs_5000_end + dat$prop_shrub_5000_end #+ dat$prop_wetland_end
mean(dat$prop_veg_5000_end, na.rm = T)
dat$prop_veg_5000_start = dat$prop_bryoids_5000_start + dat$prop_herbs_5000_start + dat$prop_shrub_5000_start + dat$prop_wetland_5000_start
mean(dat$prop_veg_5000_start, na.rm = T)

dat$prop_forest_5000_end = dat$prop_wettreed_5000_end + dat$prop_needleleaf_5000_end + dat$prop_deciduous_5000_end + dat$prop_mixed_5000_end
mean(dat$prop_forest_5000_end, na.rm = T)
dat$prop_forest_5000_start = dat$prop_wettreed_5000_start + dat$prop_needleleaf_5000_start + dat$prop_deciduous_5000_start + dat$prop_mixed_5000_start
mean(dat$prop_forest_5000_start, na.rm = T)

dat$dist_mines_5000_start = pmin(dat$dist_placer_5000_start, dat$dist_quartz_5000_start, dat$dist_othermines_5000_start, na.rm = T)
dat$dist_mines_5000_end = pmin(dat$dist_placer_5000_end, dat$dist_quartz_5000_end, dat$dist_othermines_5000_end, na.rm = T)

gc()

dat.issa <- dat[, .(
  id, area, prov, x1_, y1_, x2_, y2_, sl_, ta_, burst_, t1_, t2_, dt_,
  case_, step_id_, indiv_step_id, year,
  
  # 5000m: Vegetation / Forest
  prop_veg_5000_start, prop_veg_5000_end,
  prop_forest_5000_start, prop_forest_5000_end,
  
  # 5000m: Fire Harv & Densities
  ts_fires_5000_start, ts_fires_5000_end, 
  elevation5000_start, elevation5000_end, northness5000_start, northness5000_end, ts_harv5000_end, ts_harv5000_start, 
  harv_dist_5000_start, harv_dist_5000_end,
  
  # 5000m: Distances
  dist_paved_5000_start, dist_paved_5000_end,
  dist_unpaved_5000_start, dist_unpaved_5000_end,
  dist_lfother_5000_start, dist_lfother_5000_end,
  dist_otherpoly_5000_start, dist_otherpoly_5000_end,
  dist_othermines_5000_start, dist_othermines_5000_end,
  dist_placer_5000_start, dist_placer_5000_end,
  dist_quartz_5000_start, dist_quartz_5000_end,
  
  # 1000m: Vegetation / Forest
  prop_veg_1000_start, prop_veg_1000_end,
  prop_forest_1000_start, prop_forest_1000_end,
  ts_fires_1000_start, ts_fires_1000_end, 
  elevation1000_start, elevation1000_end, northness1000_start, northness1000_end,ts_harv1000_end, ts_harv1000_start,
  harv_dist_1000_start, harv_dist_1000_end,
  
  # 1000m: Distances
  dist_paved_1000_start, dist_paved_1000_end,
  dist_unpaved_1000_start, dist_unpaved_1000_end,
  dist_lfother_1000_start, dist_lfother_1000_end,
  dist_otherpoly_1000_start, dist_otherpoly_1000_end,
  dist_othermines_1000_start, dist_othermines_1000_end,
  dist_placer_1000_start, dist_placer_1000_end,
  dist_quartz_1000_start, dist_quartz_1000_end,
  
  
  # 250m: Vegetation / Forest
  prop_veg_250_start, prop_veg_250_end,
  prop_forest_250_start, prop_forest_250_end,
  prop_needleleaf_250_start, prop_needleleaf_250_end,
  
  # 250m: Fire, Harv 
  ts_fires_250_start, ts_fires_250_end, 
  ts_harv250_end, ts_harv250_start,
  harv_dist_250_start, harv_dist_250_end,
  elevation250_end, northness250_end,
  
  # 250m: Distances
  dist_paved_250_start, dist_paved_250_end,
  dist_unpaved_250_start, dist_unpaved_250_end,
  dist_lfother_250_start, dist_lfother_250_end,
  dist_otherpoly_250_start, dist_otherpoly_250_end,
  dist_othermines_250_start, dist_othermines_250_end,
  dist_placer_250_start, dist_placer_250_end,
  dist_quartz_250_start, dist_quartz_250_end
)]

#dat.issa[dat, c("elevation250_end", "northness250_end") := .(i.elevation250_end, i.northness250_end), on = "indiv_step_id"]

gc()

dat.issa[case_ ==TRUE,mean(prop_veg_1000_end, na.rm = T), by = .(prov, year)]
dat.issa[case_ ==TRUE,mean(prop_forest_1000_end, na.rm = T), by = .(prov, year)]
dat.issa[case_ ==TRUE,mean(prop_needleleaf_1000_end, na.rm = T), by = .(prov, year)]
#saveRDS(dat.issa, file.path(derived, 'dat_iSSA_updated2.RDS'))

#---- Load-in iSSA data ----
dat.issa <- readRDS(file.path(derived, 'dat_iSSA_updated2.RDS'))

gc()


#Adding harv_dist to dist_otherpoly
dat.issa[, dist_otherpoly_250_end := pmin(harv_dist_250_end, dist_otherpoly_250_end, na.rm = TRUE)]
dat.issa[, dist_otherpoly_1000_start := pmin(harv_dist_1000_start, dist_otherpoly_1000_start, na.rm = TRUE)]
dat.issa[, dist_otherpoly_5000_start := pmin(harv_dist_5000_start, dist_otherpoly_5000_start, na.rm = TRUE)]


#Adjusted distances, 1000m
dat.issa[,dist_paved_1000_start_adj:= ifelse(.SD[,.(min(dist_paved_1000_start, na.rm = T))]>15000, 15001, dist_paved_1000_start), by = indiv_step_id]
dat.issa[,dist_unpaved_1000_start_adj:= ifelse(.SD[,.(min(dist_unpaved_1000_start, na.rm = T))]>15000, 15001, dist_unpaved_1000_start), by = indiv_step_id]
dat.issa[,dist_lfother_1000_start_adj:= ifelse(.SD[,.(min(dist_lfother_1000_start, na.rm = T))]>15000, 15001, dist_lfother_1000_start), by = indiv_step_id]
gc()
dat.issa[,dist_otherpoly_1000_start_adj:= ifelse(.SD[,.(min(dist_otherpoly_1000_start, na.rm = T))]>15000, 15001, dist_otherpoly_1000_start), by = indiv_step_id]
dat.issa[,dist_placer_1000_start_adj:= ifelse(.SD[,.(min(dist_placer_1000_start, na.rm = T))]>15000, 15001, dist_placer_1000_start), by = indiv_step_id]
gc()
dat.issa[,dist_quartz_1000_start_adj:= ifelse(.SD[,.(min(dist_quartz_1000_start, na.rm = T))]>15000, 15001, dist_quartz_1000_start), by = indiv_step_id]
dat.issa[,dist_othermines_1000_start_adj:= ifelse(.SD[,.(min(dist_othermines_1000_start, na.rm = T))]>15000, 15001, dist_othermines_1000_start), by = indiv_step_id]
dat.issa[,harv_dist_1000_start_adj:= ifelse(.SD[,.(min(harv_dist_1000_start, na.rm = T))]>15000, 15001, harv_dist_1000_start), by = indiv_step_id]
gc()

#Adjusted distances, 5000m
dat.issa[,dist_paved_5000_start_adj:= ifelse(.SD[,.(min(dist_paved_5000_start, na.rm = T))]>15000, 15001, dist_paved_5000_start), by = indiv_step_id]
dat.issa[,dist_unpaved_5000_start_adj:= ifelse(.SD[,.(min(dist_unpaved_5000_start, na.rm = T))]>15000, 15001, dist_unpaved_5000_start), by = indiv_step_id]
dat.issa[,dist_lfother_5000_start_adj:= ifelse(.SD[,.(min(dist_lfother_5000_start, na.rm = T))]>15000, 15001, dist_lfother_5000_start), by = indiv_step_id]
gc()
dat.issa[,dist_otherpoly_5000_start_adj:= ifelse(.SD[,.(min(dist_otherpoly_5000_start, na.rm = T))]>15000, 15001, dist_otherpoly_5000_start), by = indiv_step_id]
dat.issa[,dist_placer_5000_start_adj:= ifelse(.SD[,.(min(dist_placer_5000_start, na.rm = T))]>15000, 15001, dist_placer_5000_start), by = indiv_step_id]
gc()
dat.issa[,dist_quartz_5000_start_adj:= ifelse(.SD[,.(min(dist_quartz_5000_start, na.rm = T))]>15000, 15001, dist_quartz_5000_start), by = indiv_step_id]
dat.issa[,dist_othermines_5000_start_adj:= ifelse(.SD[,.(min(dist_othermines_5000_start, na.rm = T))]>15000, 15001, dist_othermines_5000_start), by = indiv_step_id]
gc()


#Add these in if needed
dat.issa[, dist_road_1000_start := pmin(dist_paved_1000_start, dist_unpaved_1000_start, na.rm = TRUE)]
dat.issa[,dist_road_1000_start_adj:= ifelse(.SD[,.(min(dist_road_1000_start, na.rm = T))]>15000, 15001, dist_road_1000_start), by = indiv_step_id]

dat.issa[, dist_road_5000_start := pmin(dist_paved_5000_start, dist_unpaved_5000_start, na.rm = TRUE)]
dat.issa[,dist_road_5000_start_adj:= ifelse(.SD[,.(min(dist_road_5000_start, na.rm = T))]>15000, 15001, dist_road_5000_start), by = indiv_step_id]

dat.issa[,dist_mines_5000_start := pmin(dist_placer_5000_start, dist_quartz_5000_start, dist_othermines_5000_start, na.rm = T)]
dat.issa[,dist_mines_5000_start_adj:= ifelse(.SD[,.(min(dist_mines_5000_start, na.rm = T))]>15000, 15001, dist_mines_5000_start), by = indiv_step_id]

dat.issa[,dist_mines_1000_start := pmin(dist_placer_1000_start, dist_quartz_1000_start, dist_othermines_1000_start, na.rm = T)]
dat.issa[,dist_mines_1000_start_adj:= ifelse(.SD[,.(min(dist_mines_1000_start, na.rm = T))]>15000, 15001, dist_mines_1000_start), by = indiv_step_id]


#250m
# Adjusted distances, 250m
dat.issa[,dist_paved_250_end_adj:= ifelse(.SD[,.(min(dist_paved_250_end, na.rm = T))]>15000, 15001, dist_paved_250_end), by = indiv_step_id]
dat.issa[,dist_unpaved_250_end_adj:= ifelse(.SD[,.(min(dist_unpaved_250_end, na.rm = T))]>15000, 15001, dist_unpaved_250_end), by = indiv_step_id]
dat.issa[,dist_lfother_250_end_adj:= ifelse(.SD[,.(min(dist_lfother_250_end, na.rm = T))]>15000, 15001, dist_lfother_250_end), by = indiv_step_id]
gc()
dat.issa[,dist_otherpoly_250_end_adj:= ifelse(.SD[,.(min(dist_otherpoly_250_end, na.rm = T))]>15000, 15001, dist_otherpoly_250_end), by = indiv_step_id]
dat.issa[,dist_placer_250_end_adj:= ifelse(.SD[,.(min(dist_placer_250_end, na.rm = T))]>15000, 15001, dist_placer_250_end), by = indiv_step_id]
gc()
dat.issa[,dist_quartz_250_end_adj:= ifelse(.SD[,.(min(dist_quartz_250_end, na.rm = T))]>15000, 15001, dist_quartz_250_end), by = indiv_step_id]
dat.issa[,dist_othermines_250_end_adj:= ifelse(.SD[,.(min(dist_othermines_250_end, na.rm = T))]>15000, 15001, dist_othermines_250_end), by = indiv_step_id]

gc()

dat.issa[, dist_road_250_end := pmin(dist_paved_250_end, dist_unpaved_250_end, na.rm = TRUE)]
dat.issa[,dist_road_250_end_adj:= ifelse(.SD[,.(min(dist_road_250_end, na.rm = T))]>15000, 15001, dist_road_250_end), by = indiv_step_id]

dat.issa[,dist_mines_250_end := pmin(dist_placer_250_end, dist_quartz_250_end, dist_othermines_250_end, na.rm = T)]
dat.issa[,dist_mines_250_end_adj:= ifelse(.SD[,.(min(dist_mines_250_end, na.rm = T))]>15000, 15001, dist_mines_250_end), by = indiv_step_id]

gc()

#Add for simple model
dat.issa[, dist_lf_250_end := pmin(dist_paved_250_end, dist_unpaved_250_end, dist_lfother_250_end, na.rm = TRUE)]
dat.issa[,dist_lf_250_end_adj:= ifelse(.SD[,.(min(dist_lf_250_end, na.rm = T))]>15000, 15001, dist_lf_250_end), by = indiv_step_id]
dat.issa[, dist_poly_250_end := pmin(dist_placer_250_end, dist_quartz_250_end, dist_othermines_250_end, dist_otherpoly_250_end, harv_dist_250_end, na.rm = TRUE)]
dat.issa[,dist_poly_250_end_adj:= ifelse(.SD[,.(min(dist_poly_250_end, na.rm = T))]>15000, 15001, dist_poly_250_end), by = indiv_step_id]

dat.issa[, dist_lf_1000_start := pmin(dist_paved_1000_start, dist_unpaved_1000_start, dist_lfother_1000_start, na.rm = TRUE)]
dat.issa[, dist_lf_1000_start_adj := ifelse(.SD[, .(min(dist_lf_1000_start, na.rm = TRUE))] > 15000, 15001, dist_lf_1000_start), by = indiv_step_id]
dat.issa[, dist_poly_1000_start := pmin(dist_placer_1000_start, dist_quartz_1000_start, dist_othermines_1000_start, dist_otherpoly_1000_start, harv_dist_1000_start, na.rm = TRUE)]
dat.issa[, dist_poly_1000_start_adj := ifelse(.SD[, .(min(dist_poly_1000_start, na.rm = TRUE))] > 15000, 15001, dist_poly_1000_start), by = indiv_step_id]

dat.issa[, dist_lf_5000_start := pmin(dist_paved_5000_start, dist_unpaved_5000_start, dist_lfother_5000_start, na.rm = TRUE)]
dat.issa[, dist_lf_5000_start_adj := ifelse(.SD[, .(min(dist_lf_5000_start, na.rm = TRUE))] > 15000, 15001, dist_lf_5000_start), by = indiv_step_id]
dat.issa[, dist_poly_5000_start := pmin(dist_placer_5000_start, dist_quartz_5000_start, dist_othermines_5000_start, dist_otherpoly_5000_start, harv_dist_5000_start, na.rm = TRUE)]
dat.issa[, dist_poly_5000_start_adj := ifelse(.SD[, .(min(dist_poly_5000_start, na.rm = TRUE))] > 15000, 15001, dist_poly_5000_start), by = indiv_step_id]


#saveRDS(dat.issa, file.path(derived, 'dat_iSSA_updated2.15.RDS')) #SAVING THE 10/15KM CUTOFF
#dat.issa <- readRDS(file.path(derived, 'dat_iSSA_updated2.RDS'))

gc()

#Sep into steps
dat.issa.20 <- dat.issa[,.SD[1:21], by = .(indiv_step_id)]
gc()
dat.issa.15 <- dat.issa[,.SD[1:16], by = .(indiv_step_id)]
gc()
dat.issa.10 <- dat.issa[,.SD[1:11], by = .(indiv_step_id)]

---------------------------------------------
  
  
# Add season
dat.issa.10$monthday <- format(as.Date(dat.issa.10$t1_, format="%Y/%m/%d"),"%m/%d") #Create new column that isolates the month and day out of t1_
dat.issa.10$monthday #Confirming that the new column was created correctly
names(dat.issa.10) #Confirming that the new column exists

dat.issa.10[monthday >= "11/01" | monthday <= "04/15", season := "winter"]
dat.issa.10[monthday >= "07/01" & monthday <= "09/30", season := "summer"]
dat.issa.10[monthday >= "04/16" & monthday <= "06/30", season := "spring"]
dat.issa.10[monthday >= "10/01" & monthday <= "10/31", season := "fall"]


#dat.issa.15
dat.issa.15$monthday <- format(as.Date(dat.issa.15$t1_, format="%Y/%m/%d"),"%m/%d") #Create new column that isolates the month and day out of t1_
dat.issa.15$monthday #Confirming that the new column was created correctly
names(dat.issa.15) #Confirming that the new column exists

dat.issa.15[monthday >= '11/01' | monthday <= '04/15', season:="winter"] #Create new season column, with Nov to mid-April being called Winter
dat.issa.15[monthday >= '07/01' & monthday <= '09/30', season:="summer"] #Create summer season from July through September

#dat.issa.20
dat.issa.20$monthday <- format(as.Date(dat.issa.20$t1_, format="%Y/%m/%d"),"%m/%d") #Create new column that isolates the month and day out of t1_
dat.issa.20$monthday #Confirming that the new column was created correctly
names(dat.issa.20) #Confirming that the new column exists

dat.issa.20[monthday >= '11/01' | monthday <= '04/15', season:="winter"] #Create new season column, with Nov to mid-April being called Winter
dat.issa.20[monthday >= '07/01' & monthday <= '09/30', season:="summer"] #Create summer season from July through September

dat.issa.10[, yearseason := paste0(year, season)]

# NEW MODELS ----------
## Simple model -----
#Trying only those close to disturbance
dat.issa.10.close <- dat.issa.10[
  dist_lf_250_end <= 20000 | dist_poly_250_end <= 20000
]


#Filtering dataset to only include individuals who have at least 10 steps within 20km of disturbance

# Step 1: Get the IDs that meet the 10-step criterion
ids_to_keep <- dat.issa.10.close %>%
  group_by(id) %>%
  summarise(n_steps = n_distinct(indiv_step_id)) %>%
  filter(n_steps >= 10) %>%
  pull(id)
# Step 2: Filter the main dataset to only those IDs
dat.issa.10.filtered <- dat.issa.10 %>%
  filter(id %in% ids_to_keep)
#dat.issa.10.filtered <- readRDS(file.path(derived, 'dat_iSSA_10_filtered.RDS'))


gc()
m.simple.filt <- glmmTMB(case_ ~ -1 +
                       I(log((sl_ + 1)/1000)) +
                       I(cos(ta_)) +
                       I(log((sl_ + 1)/1000)):I(cos(ta_)) +
                       prop_forest_250_end + #prop_forest_250_end:season + #prop_forest_250_end:prop_forest_5000_start + 
                       prop_veg_250_end + #prop_veg_250_end:season + #prop_veg_250_end:prop_veg_5000_start + 
                       I(log(ts_fires_250_end + 1)) + #I(log(ts_fires_250_end + 1)) : I(log(ts_fires_5000_start + 1)) +
                       I(log((dist_lf_250_end + 1)/1000)) + #I(log((dist_lf_250_end + 1)/1000)):season + #I(log((dist_lf_250_end_adj + 1)/1000)) : I(log((dist_lf_5000_start + 1)/1000)) + 
                       I(log((dist_poly_250_end + 1)/1000)) + #I(log((dist_poly_250_end + 1)/1000)):season + #I(log((dist_poly_250_end_adj + 1)/1000)) : I(log((dist_poly_5000_start + 1)/1000)) + 
                          
                       (1 | indiv_step_id) +
                       (0 + I(log((sl_ + 1)/1000)) | id) +
                       (0 + I(cos(ta_)) | id) +
                       (0 + I(log((sl_ + 1)/1000)) : I(cos(ta_)) | id) +
                       (0 + prop_forest_250_end | id) +
                       (0 + prop_veg_250_end | id) +
                       #(0 + I(log(ts_fires_250_end + 1)) | id) +
                       (0 + I(log((dist_lf_250_end + 1)/1000)) | id) +
                       (0 + I(log((dist_poly_250_end + 1)/1000)) | id) +
                         
                       (1 | year:season),
                     
                     family = poisson(), 
                     data = dat.issa.10.filtered[, `:=`(
                       year = as.factor(year), 
                       #season = as.factor(season),
                       indiv_step_id = as.factor(indiv_step_id),
                       id = as.factor(id)
                     )],
                     map = list(theta = factor(c(NA, 1:8))),
                     start = list(theta = c(log(1000), rep(0, 8))), 
                     verbose = TRUE, 
                     control = glmmTMBControl(rank_check = "adjust")
)

summary(m.simple.filt)
# save model output
saveRDS(m.simple.filt, "simple.filt.RDS")
#m.simple.filt <- readRDS("simple.filt.RDS")

m.simple.indiv <- coef(m.simple.filt)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(m.simple.indiv)[term!='(Intercept)']
pop <- setDT(tidy(m.simple.filt, effects = 'fixed'))


require(ggplot2)
ggplot() +
  geom_jitter(data = indivs[term %like% "dist"], aes(x = term, y = estimate),
              width = 0.15, height = 0, alpha = 0.2, color = "#9467bd") +
  geom_point(data = pop[term %like% "dist"], aes(x = term, y = estimate),
             color = "black", size = 2.5) +
  geom_errorbar(data = pop[term %like% "dist"],
                aes(x = term,
                    ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(x = "", y = "Estimate") 


ggplot() +
  geom_jitter(data = indivs[!(term %like% "dist")], aes(x = term, y = estimate),
              width = 0.15, height = 0, alpha = 0.2, color = "#9467bd") +
  geom_point(data = pop[!(term %like% "dist")], aes(x = term, y = estimate),
             color = "black", size = 2.5) +
  geom_errorbar(data = pop[!(term %like% "dist")],
                aes(x = term,
                    ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(x = "", y = "Estimate") 



## ---- Complex model ----
gc()
m.complex <- glmmTMB(case_ ~ -1 +
                       I(log((sl_ + 1)/1000)) +
                       I(cos(ta_)) +
                       I(log((sl_ + 1)/1000)):I(cos(ta_)) +
                       prop_forest_250_end + #prop_forest_250_end:prop_forest_5000_start +
                       prop_veg_250_end + #prop_veg_250_end:prop_veg_5000_start +
                       I(log(ts_fires_250_end + 1)) + #I(log(ts_fires_250_end + 1)) : I(log(ts_fires_5000_start + 1)) +
                       I(log((dist_road_250_end + 1)/1000)) + #I(log((dist_road_250_end_adj + 1)/1000)) : I(log((dist_road_5000_start_adj + 1)/1000)) +
                       #I(log((dist_paved_250_end_adj + 1)/1000)) + I(log((dist_paved_250_end_adj + 1)/1000)) : I(log((dist_paved_1000_start_adj + 1)/1000)) +
                       #I(log((dist_unpaved_250_end_adj + 1)/1000)) + I(log((dist_unpaved_250_end_adj + 1)/1000)) : I(log((dist_unpaved_1000_start_adj + 1)/1000)) +
                       I(log((dist_lfother_250_end + 1)/1000)) + #I(log((dist_lfother_250_end_adj + 1)/1000)) : I(log((dist_lfother_5000_start_adj + 1)/1000)) +
                       I(log((dist_otherpoly_250_end + 1)/1000)) + #I(log((dist_otherpoly_250_end_adj + 1)/1000)) : I(log((dist_otherpoly_5000_start_adj + 1)/1000)) +
                       #I(log((dist_mines_250_end + 1)/1000)) + #I(log((dist_mines_250_end_adj + 1)/1000)) : I(log((dist_mines_1000_start_adj + 1)/1000)) +
                       I(log((dist_placer_250_end + 1)/1000)) + #I(log((dist_placer_250_end_adj + 1)/1000)) : I(log((dist_placer_5000_start_adj + 1)/1000)) +
                       I(log((dist_quartz_250_end + 1)/1000)) + #I(log((dist_quartz_250_end_adj + 1)/1000)) : I(log((dist_quartz_5000_start_adj + 1)/1000)) +
                       I(log((dist_othermines_250_end + 1)/1000)) + #I(log((dist_othermines_250_end_adj + 1)/1000)) : I(log((dist_othermines_5000_start_adj + 1)/1000)) +
                       
                       (1 | indiv_step_id) +
                       (0 + I(log((sl_ + 1)/1000)) | id) +
                       (0 + I(cos(ta_)) | id) +
                       (0 + I(log((sl_ + 1)/1000)) : I(cos(ta_)) | id) +
                       (0 + prop_forest_250_end | id) +
                       (0 + prop_veg_250_end | id) +
                       #(0 + I(log(ts_fires_250_end + 1)) | id) +
                       (0 + I(log((dist_road_250_end + 1)/1000)) | id) +
                       #(0 + I(log((dist_paved_250_end_adj + 1)/1000)) | id) +
                       #(0 + I(log((dist_unpaved_250_end_adj + 1)/1000)) | id) +
                       (0 + I(log((dist_lfother_250_end + 1)/1000)) | id) +
                       (0 + I(log((dist_otherpoly_250_end + 1)/1000)) | id) +
                       #(0 + I(log((dist_mines_250_end + 1)/1000)) | id) +
                       #(0 + I(log((dist_placer_250_end + 1)/1000)) | id) +
                       #(0 + I(log((dist_quartz_250_end + 1)/1000)) | id) +
                       #(0 + I(log((dist_othermines_250_end + 1)/1000)) | id) +
                       
                       (1 | year:season),
                     
                     family = poisson(), 
                     data = dat.issa.10.filtered[, `:=`(
                       year = as.factor(year), 
                       season = as.factor(season),
                       indiv_step_id = as.factor(indiv_step_id),
                       id = as.factor(id)
                     )],
                     map = list(theta = factor(c(NA, 1:9))),
                     start = list(theta = c(log(1000), rep(0, 9))), 
                     verbose = TRUE, 
                     control = glmmTMBControl(rank_check = "adjust")
)


summary(m.complex)
# save model output
saveRDS(m.complex, "m.complex.RDS")
#m.complex <- readRDS("m.complex.RDS")

m.complex.indiv <- coef(m.complex)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(m.complex.indiv)[term!='(Intercept)']
pop <- setDT(tidy(m.complex, effects = 'fixed'))


require(ggplot2)
ggplot() +
  geom_jitter(data = indivs[term %like% "dist"], aes(x = term, y = estimate),
              width = 0.15, height = 0, alpha = 0.2, color = "#9467bd") +
  geom_point(data = pop[term %like% "dist"], aes(x = term, y = estimate),
             color = "black", size = 2.5) +
  geom_errorbar(data = pop[term %like% "dist"],
                aes(x = term,
                    ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(x = "", y = "Estimate") 


ggplot() +
  geom_jitter(data = indivs[!(term %like% "dist")], aes(x = term, y = estimate),
              width = 0.15, height = 0, alpha = 0.2, color = "#9467bd") +
  geom_point(data = pop[!(term %like% "dist")], aes(x = term, y = estimate),
             color = "black", size = 2.5) +
  geom_errorbar(data = pop[!(term %like% "dist")],
                aes(x = term,
                    ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(x = "", y = "Estimate") 


# Winter-only models -----
#create winter dataset
dat.issa.10.winter <- dat.issa.10.filtered[season == "winter"]

#explore elevation difference to create groups to differentiate winter use 
avg_elev_per_id <- aggregate(elevation250_end ~ id, data = dat.issa.10.winter, FUN = mean)
avg_elev_per_id$elev_behav <- ifelse(avg_elev_per_id$elevation250_end >= 1300, "high", "low")

dat.issa.10.winter <- merge(dat.issa.10.winter, avg_elev_per_id[, c("id", "elev_behav")],
                            by = "id", all.x = TRUE)

dat.issa.10.winter[, elev_bin := cut(elevation250_end, breaks = seq(300, 2600, by = 100))]

ggplot(dat.issa.10.winter, aes(x = elev_bin, y = prop_veg_250_end)) +
  geom_violin(fill = "lightblue", alpha = 0.5) +         # distribution shape per elevation bin
  geom_jitter(width = 0.1, alpha = 0.05, size = 0.5) +   # show individual points with jitter
  labs(x = "Elevation (binned, 100m intervals)",
       y = "Proportion vegetation",
       title = "Variation of vegetation proportion across elevation bins") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


plot(density(avg_elev_per_id$elevation250_end, na.rm = TRUE),
     main = "Density of Average Elevation per ID",
     xlab = "Average Elevation (m)",
     col = "darkblue",
     lwd = 2)

ggplot(dat.issa.10.winter, aes(x = x1_, y = y1_, color = elev_behav)) +
  geom_point(alpha = 0.6, size = 1.5) +
  labs(title = "Step Locations Colored by Elevation Behavior",
       x = "X Coordinate",
       y = "Y Coordinate",
       color = "Elevation Behavior") +
  theme_minimal()



#Plot with DEM
dem <- rast("C:/Users/peteb/OneDrive/Documents/BP/Yukoncfx/data/derived/elevation/elevation_250m.tif")
# Convert raster to data frame (with coordinates)
dem_df <- as.data.frame(dem, xy = TRUE, na.rm = TRUE)
colnames(dem_df) <- c("x", "y", "elevation")


ggplot() +
  geom_raster(data = dem_df, aes(x = x, y = y, fill = elevation)) +
  scale_fill_viridis(name = "Elevation (m)", na.value = "transparent") +
  
  geom_point(data = dat.issa.10.winter %>% filter(elev_behav == "high"),
             aes(x = x1_, y = y1_, color = elev_behav),
             size = 1.5, alpha = 0.6) +
  
  geom_point(data = dat.issa.10.winter %>% filter(elev_behav == "low"),
             aes(x = x1_, y = y1_, color = elev_behav),
             size = 1.5, alpha = 0.6) +
  
  scale_color_manual(values = c("low" = "blue", "high" = "red")) +
  
  labs(title = "Step Locations on DEM (Zoomed)",
       x = "Longitude", y = "Latitude", color = "Elevation Behavior") +
  
  theme_minimal() +
  
  # ⬇️ THIS LINE ZOOMS TO JUST THE POINTS
  coord_cartesian(
    xlim = range(dat.issa.10.winter$x1_, na.rm = TRUE),
    ylim = range(dat.issa.10.winter$y1_, na.rm = TRUE)
  )




#Trying to center
dat.issa.10.winter[, log_sl_c := scale(log(sl_ + 1) / 1000, center = TRUE, scale = FALSE)]
dat.issa.10.winter[, cos_ta_c := scale(cos(ta_), center = TRUE, scale = FALSE)]
dat.issa.10.winter[, prop_veg_c := scale(prop_veg_250_end, center = TRUE, scale = FALSE)]

dat.issa.10.winter[, elev_behav := factor(elev_behav, levels = c("low", "high"))]

dat.issa.10.winter[, elev01 := as.integer(elev_behav == "high")]


#Models with elevation behavior included
gc()
m.complex.winter <- glmmTMB(case_ ~ -1 +
                       I(log((sl_ + 1)/1000)) +
                       I(cos(ta_)) +
                       I(log((sl_ + 1)/1000)):I(cos(ta_)) +
                       I(log((sl_ + 1)/1000)):elev01 +
                       I(cos(ta_)):elev01 +
                       prop_forest_250_end + #prop_forest_250_end:elev_behav + #prop_forest_250_end:prop_forest_5000_start +
                       prop_veg_250_end + prop_veg_250_end:elev01 + #prop_veg_250_end:prop_veg_5000_start +
                       I(log(ts_fires_250_end + 1)) + #I(log(ts_fires_250_end + 1)) : I(log(ts_fires_5000_start + 1)) +
                       I(log((dist_road_250_end + 1)/1000)) + #I(log((dist_road_250_end + 1)/1000)):elev_behav + #I(log((dist_road_250_end_adj + 1)/1000)) : I(log((dist_road_5000_start_adj + 1)/1000)) +
                       #I(log((dist_paved_250_end_adj + 1)/1000)) + (log((dist_paved_250_end_adj + 1)/1000)):elev_behav + #I(log((dist_paved_250_end_adj + 1)/1000)) : I(log((dist_paved_1000_start_adj + 1)/1000)) +
                       #I(log((dist_unpaved_250_end_adj + 1)/1000)) + I(log((dist_unpaved_250_end_adj + 1)/1000)):elev_behav + #I(log((dist_unpaved_250_end_adj + 1)/1000)) : I(log((dist_unpaved_1000_start_adj + 1)/1000)) +
                       I(log((dist_lfother_250_end + 1)/1000)) + #I(log((dist_lfother_250_end + 1)/1000)):elev_behav + #I(log((dist_lfother_250_end_adj + 1)/1000)) : I(log((dist_lfother_5000_start_adj + 1)/1000)) +
                       I(log((dist_otherpoly_250_end + 1)/1000)) + #I(log((dist_otherpoly_250_end + 1)/1000)):elev_behav + #I(log((dist_otherpoly_250_end_adj + 1)/1000)) : I(log((dist_otherpoly_5000_start_adj + 1)/1000)) +
                       #I(log((dist_mines_250_end_adj + 1)/1000)) + #I(log((dist_mines_250_end_adj + 1)/1000)):elev_behav + I(log((dist_mines_250_end_adj + 1)/1000)) : I(log((dist_mines_1000_start_adj + 1)/1000)) +
                       I(log((dist_placer_250_end + 1)/1000)) + #I(log((dist_placer_250_end + 1)/1000)):elev_behav + #I(log((dist_placer_250_end_adj + 1)/1000)) : I(log((dist_placer_5000_start_adj + 1)/1000)) +
                       I(log((dist_quartz_250_end + 1)/1000)) + #I(log((dist_quartz_250_end + 1)/1000)):elev_behav + #I(log((dist_quartz_250_end_adj + 1)/1000)) : I(log((dist_quartz_5000_start_adj + 1)/1000)) +
                       I(log((dist_othermines_250_end + 1)/1000)) + #I(log((dist_othermines_250_end + 1)/1000)):elev_behav + #I(log((dist_othermines_250_end_adj + 1)/1000)) : I(log((dist_othermines_5000_start_adj + 1)/1000)) +
                       
                       (1 | indiv_step_id) +
                       (0 + I(log((sl_ + 1)/1000)) | id) +
                       (0 + I(cos(ta_)) | id) +
                       (0 + I(log((sl_ + 1)/1000)) : I(cos(ta_)) | id) +
                       (0 + prop_forest_250_end | id) +
                       (0 + prop_veg_250_end | id) +
                       #(0 + I(log(ts_fires_250_end + 1)) | id) +
                       (0 + I(log((dist_road_250_end + 1)/1000)) | id) +
                       #(0 + I(log((dist_paved_250_end_adj + 1)/1000)) | id) +
                       #(0 + I(log((dist_unpaved_250_end_adj + 1)/1000)) | id) +
                       (0 + I(log((dist_lfother_250_end + 1)/1000)) | id) +
                       (0 + I(log((dist_otherpoly_250_end + 1)/1000)) | id) +
                       #(0 + I(log((dist_mines_250_end_adj + 1)/1000)) | id) +
                       #(0 + I(log((dist_placer_250_end_adj + 1)/1000)) | id) +
                       #(0 + I(log((dist_quartz_250_end_adj + 1)/1000)) | id) +
                       #(0 + I(log((dist_othermines_250_end_adj + 1)/1000)) | id) +
                       
                       (1 | year),
                     
                     family = poisson(), 
                     data = dat.issa.10.winter[, `:=`(
                       year = as.factor(year), 
                       season = as.factor(season),
                       indiv_step_id = as.factor(indiv_step_id),
                       id = as.factor(id)
                     )],
                     map = list(theta = factor(c(NA, 1:9))),
                     start = list(theta = c(log(1000), rep(0, 9))), 
                     verbose = TRUE, 
                     control = glmmTMBControl(rank_check = "adjust")
)


summary(m.complex.winter)
# save model output
saveRDS(m.complex.winter, "m.complex.winter.RDS")
#m.complex.winter <- readRDS("m.complex.winter.RDS")

m.complex.winter.indiv <- coef(m.complex.winter)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(m.complex.winter.indiv)[term!='(Intercept)']
pop <- setDT(tidy(m.complex.winter, effects = 'fixed'))


require(ggplot2)
ggplot() +
  geom_jitter(data = indivs, aes(x = term, y = estimate),
              width = 0.15, height = 0, alpha = 0.2, color = "#9467bd") +
  geom_point(data = pop, aes(x = term, y = estimate),
             color = "black", size = 2.5) +
  geom_errorbar(data = pop,
                aes(x = term,
                    ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(x = "", y = "Estimate") 


ggplot() +
  geom_jitter(data = indivs[!(term %like% "dist")], aes(x = term, y = estimate),
              width = 0.15, height = 0, alpha = 0.2, color = "#9467bd") +
  geom_point(data = pop[!(term %like% "dist")], aes(x = term, y = estimate),
             color = "black", size = 2.5) +
  geom_errorbar(data = pop[!(term %like% "dist")],
                aes(x = term,
                    ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(x = "", y = "Estimate") 




#Simple winter
gc()
m.simple.winter <- glmmTMB(case_ ~ -1 +
                           I(log((sl_ + 1)/1000)) +
                           I(cos(ta_)) +
                           I(log((sl_ + 1)/1000)):I(cos(ta_)) +
                           I(log((sl_ + 1)/1000)):elev01 +
                           I(cos(ta_)):elev01 +
                           prop_forest_250_end + #prop_forest_250_end:elev_behav +
                           prop_veg_250_end + prop_veg_250_end:elev01 +
                           I(log(ts_fires_250_end + 1)) + 
                           I(log((dist_lf_250_end + 1)/1000)) + #I(log((dist_lf_250_end + 1)/1000)):elev_behav +
                           I(log((dist_poly_250_end + 1)/1000)) + #I(log((dist_poly_250_end + 1)/1000)):elev_behav +
                           
                           (1 | indiv_step_id) +
                           (0 + I(log((sl_ + 1)/1000)) | id) +
                           (0 + I(cos(ta_)) | id) +
                           (0 + I(log((sl_ + 1)/1000)) : I(cos(ta_)) | id) +
                           (0 + prop_forest_250_end | id) +
                           (0 + prop_veg_250_end | id) +
                           #(0 + I(log(ts_fires_250_end + 1)) | id) +
                           (0 + I(log((dist_lf_250_end + 1)/1000)) | id) +
                           (0 + I(log((dist_poly_250_end + 1)/1000)) | id) +
                           
                           (1 | year),
                         
                         family = poisson(), 
                         data = dat.issa.10.winter[, `:=`(
                           year = as.factor(year), 
                           #season = as.factor(season),
                           indiv_step_id = as.factor(indiv_step_id),
                           id = as.factor(id)
                         )],
                         map = list(theta = factor(c(NA, 1:8))),
                         start = list(theta = c(log(1000), rep(0, 8))), 
                         verbose = TRUE, 
                         control = glmmTMBControl(rank_check = "adjust")
)

summary(m.simple.winter)
# save model output
saveRDS(m.simple.winter, "simple.winter.RDS")
#m.simple.winter <- readRDS("simple.winter.RDS")



m.simple.winter.indiv <- coef(m.simple.winter)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(m.simple.winter.indiv)[term!='(Intercept)']
pop <- setDT(tidy(m.simple.winter, effects = 'fixed'))


require(ggplot2)
ggplot() +
  geom_jitter(data = indivs, aes(x = term, y = estimate),
              width = 0.15, height = 0, alpha = 0.2, color = "#9467bd") +
  geom_point(data = pop, aes(x = term, y = estimate),
             color = "black", size = 2.5) +
  geom_errorbar(data = pop,
                aes(x = term,
                    ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(x = "", y = "Estimate") 


ggplot() +
  geom_jitter(data = indivs[!(term %like% "dist")], aes(x = term, y = estimate),
              width = 0.15, height = 0, alpha = 0.2, color = "#9467bd") +
  geom_point(data = pop[!(term %like% "dist")], aes(x = term, y = estimate),
             color = "black", size = 2.5) +
  geom_errorbar(data = pop[!(term %like% "dist")],
                aes(x = term,
                    ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(x = "", y = "Estimate") 



-----------------------------------------------------------------------------------------------




# ------ OLD MODELS -------

# ---- FINE SCALE MODELS ----

### ---- Year-round ----
gc()
m.closedisturbancefull <- glmmTMB(case_ ~ -1 +
                                    I(log((sl_+1)/1000)) +
                                    I(cos(ta_)) +
                                    I(log((sl_+1)/1000)):I(cos(ta_)) +
                                    prop_forest_250_end + prop_forest_250_end:prop_forest_1000_end +
                                    prop_veg_250_end + prop_veg_250_end:prop_veg_1000_end +
                                    I(log(ts_fires_250_end+1)) + I(log(ts_fires_250_end+1)):I(log(ts_fires_1000_end+1)) +
                                    I(log(ts_harv250_end+1)) + I(log(ts_harv250_end+1)):I(log(ts_harv1000_end+1)) +
                                    I(log((dist_paved_250_end+1)/1000)) + I(log((dist_paved_250_end+1)/1000)):I(log((dist_paved_1000_end+1)/1000)) +
                                    I(log((dist_unpaved_250_end+1)/1000)) + I(log((dist_unpaved_250_end+1)/1000)):I(log((dist_unpaved_1000_end+1)/1000)) +
                                    #I(log(((dist_road_end+1)/1000))) + I(log((dist_road_250_end/1000)+1)):I(log((dist_road_1000_end/1000)+1)) +
                                    I(log((dist_lfother_250_end+1)/1000)) + I(log((dist_lfother_250_end+1)/1000)):I(log((dist_lfother_1000_end+1)/1000)) +
                                    I(log((dist_otherpoly_250_end+1)/1000)) + I(log((dist_otherpoly_250_end+1)/1000)):I(log((dist_otherpoly_1000_end+1)/1000)) +
                                    I(log((dist_placer_250_end+1)/1000)) + I(log((dist_placer_250_end+1)/1000)):I(log((dist_placer_1000_end+1)/1000)) +
                                    I(log((dist_quartz_250_end+1)/1000)) + I(log((dist_quartz_250_end+1)/1000)):I(log((dist_quartz_1000_end+1)/1000)) +
                                    I(log((dist_othermines_250_end+1)/1000)) + I(log((dist_othermines_250_end+1)/1000)):I(log((dist_othermines_1000_end+1)/1000)) +
                                    #I(log((dist_mines_250_end+1)/1000)) + I(log((dist_mines_250_end+1)/1000)):I(log((dist_mines_1000_end+1)/1000)) +
                                    #I(log((dist_otherdisturbance_250_end+1)/1000)) + I(log((dist_otherdisturbance_250_end+1)/1000)):I(log((dist_otherdisturbance_1000_end+1)/1000)) +
                                    
                                    (1|indiv_step_id) +
                                    (0 + I(log((sl_ +1)/1000))|id) +
                                    (0 + I(cos(ta_))|id) +
                                    (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                                    (0 + prop_forest_250_end|id) +
                                    (0 + prop_veg_250_end|id) +
                                    (0 + (I(log(ts_fires_250_end+1)))|id) +
                                    (0 + (I(log(ts_harv250_end+1)))|id) + 
                                    (0 + I(log((dist_paved_250_end+1)/1000))|id) + 
                                    (0 + I(log((dist_unpaved_250_end+1)/1000))|id) + 
                                    #(0 + I(log((dist_road_250_end+1)/1000))|id) + 
                                    (0 + I(log((dist_lfother_250_end+1)/1000))|id) +
                                    (0 + I(log((dist_otherpoly_250_end+1)/1000))|id) + 
                                    (0 + I(log((dist_placer_250_end+1)/1000))|id) + 
                                    (0 + I(log((dist_quartz_250_end+1)/1000))|id) + 
                                    (0 + I(log((dist_othermines_250_end+1)/1000))|id) + 
                                    #(0 + I(log((dist_mines_250_end+1)/1000))|id) + 
                                    #(0 + I(log((dist_otherdisturbance_250_end+1)/1000))|id) + 
                                    
                                    (1|year),
                                  family = poisson(), data = dat.issa.10.close[,`:=`(year = as.factor(year), 
                                                                                     indiv_step_id = as.factor(indiv_step_id),
                                                                                     id = as.factor(id))],
                                  map= list(theta = factor(c(NA,1:15))),
                                  start = list(theta =c(log(1000), seq(0,0, length.out = 15))), 
                                  verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(m.closedisturbancefull)
# save model output
saveRDS(m.closedisturbancefull, "m.closedisturbancefull.mod_lf20poly20.RDS")
#m.closedisturbancefull <- readRDS("m.closedisturbancefull.mod_lf20poly20.RDS")


m.closedisturbancefull.indiv <- coef(m.closedisturbancefull)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(m.closedisturbancefull.indiv)[term!='(Intercept)']
pop <- setDT(tidy(m.closedisturbancefull, effects = 'fixed'))


require(ggplot2)
ggplot(data = pop[term %like% 'dist'], aes(x =term, y = estimate)) +
  geom_point()+
  geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 
ggplot(data = pop[!(term %like% 'dist')], aes(x =term, y = estimate)) +
  geom_point()+
  geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 

ggplot(data = indivs[(term %like% 'dist')], aes(x =term, y = estimate)) + #This one
  geom_boxplot()+
  geom_jitter() +
  #geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 

ggplot(data = indivs[!(term %like% 'dist')], aes(x =term, y = estimate)) +
  geom_boxplot()+
  geom_jitter() +
  #geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip()  





### ---- SUMMER ----
dat.issa.10.summer.close <- dat.issa.10.summer[dist_anthrodisturbance_250_start <= 20000]

gc()
m.closedisturbancesummer <- glmmTMB(case_ ~ -1 +
                                      I(log((sl_+1)/1000)) +
                                      I(cos(ta_)) +
                                      I(log((sl_+1)/1000)):I(cos(ta_)) +
                                      prop_forest_250_end + prop_forest_250_end:prop_forest_1000_end +
                                      prop_veg_250_end + prop_veg_250_end:prop_veg_1000_end +
                                      I(log(ts_fires_250_end+1)) + I(log(ts_fires_250_end+1)):I(log(ts_fires_1000_end+1)) +
                                      I(log(ts_harv250_end+1)) + I(log(ts_harv250_end+1)):I(log(ts_harv1000_end+1)) +
                                      I(log((dist_paved_250_end+1)/1000)) + I(log((dist_paved_250_end+1)/1000)):I(log((dist_paved_1000_end+1)/1000)) +
                                      I(log((dist_unpaved_250_end+1)/1000)) + I(log((dist_unpaved_250_end+1)/1000)):I(log((dist_unpaved_1000_end+1)/1000)) +
                                      #I(log(((dist_road_end+1)/1000))) + I(log((dist_road_250_end/1000)+1)):I(log((dist_road_1000_end/1000)+1)) +
                                      I(log((dist_lfother_250_end+1)/1000)) + I(log((dist_lfother_250_end+1)/1000)):I(log((dist_lfother_1000_end+1)/1000)) +
                                      I(log((dist_otherpoly_250_end+1)/1000)) + I(log((dist_otherpoly_250_end+1)/1000)):I(log((dist_otherpoly_1000_end+1)/1000)) +
                                      I(log((dist_placer_250_end+1)/1000)) + I(log((dist_placer_250_end+1)/1000)):I(log((dist_placer_1000_end+1)/1000)) +
                                      I(log((dist_quartz_250_end+1)/1000)) + I(log((dist_quartz_250_end+1)/1000)):I(log((dist_quartz_1000_end+1)/1000)) +
                                      I(log((dist_othermines_250_end+1)/1000)) + I(log((dist_othermines_250_end+1)/1000)):I(log((dist_othermines_1000_end+1)/1000)) +
                                      #I(log((dist_mines_250_end+1)/1000)) + I(log((dist_mines_250_end+1)/1000)):I(log((dist_mines_1000_end+1)/1000)) +
                                      #I(log((dist_otherdisturbance_250_end+1)/1000)) + I(log((dist_otherdisturbance_250_end+1)/1000)):I(log((dist_otherdisturbance_1000_end+1)/1000)) +
                                      
                                      (1|indiv_step_id) +
                                      (0 + I(log((sl_ +1)/1000))|id) +
                                      (0 + I(cos(ta_))|id) +
                                      (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                                      (0 + prop_forest_250_end|id) +
                                      (0 + prop_veg_250_end|id) +
                                      (0 + (I(log(ts_fires_250_end+1)))|id) +
                                      (0 + (I(log(ts_harv250_end+1)))|id) + 
                                      (0 + I(log((dist_paved_250_end+1)/1000))|id) + 
                                      (0 + I(log((dist_unpaved_250_end+1)/1000))|id) + 
                                      #(0 + I(log((dist_road_250_end+1)/1000))|id) + 
                                      (0 + I(log((dist_lfother_250_end+1)/1000))|id) +
                                      (0 + I(log((dist_otherpoly_250_end+1)/1000))|id) + 
                                      (0 + I(log((dist_placer_250_end+1)/1000))|id) + 
                                      (0 + I(log((dist_quartz_250_end+1)/1000))|id) + 
                                      (0 + I(log((dist_othermines_250_end+1)/1000))|id) + 
                                      #(0 + I(log((dist_mines_250_end+1)/1000))|id) + 
                                      #(0 + I(log((dist_otherdisturbance_250_end+1)/1000))|id) + 
                                      
                                      (1|year),
                                    family = poisson(), data = dat.issa.10.summer.close[,`:=`(year = as.factor(year), 
                                                                                              indiv_step_id = as.factor(indiv_step_id),
                                                                                              id = as.factor(id))],
                                    map= list(theta = factor(c(NA,1:15))),
                                    start = list(theta =c(log(1000), seq(0,0, length.out = 15))), 
                                    verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(m.closedisturbancesummer)
# save model output
saveRDS(m.closedisturbancesummer, "m.closedisturbancesummer.mod_lf20poly20.RDS")

m.closedisturbancesummer.indiv <- coef(m.closedisturbancesummer)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(m.closedisturbancesummer.indiv)[term!='(Intercept)']
pop <- setDT(tidy(m.closedisturbancesummer, effects = 'fixed'))


require(ggplot2)
ggplot(data = pop[term %like% 'dist'], aes(x =term, y = estimate)) +
  geom_point()+
  geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 
ggplot(data = pop[!(term %like% 'dist')], aes(x =term, y = estimate)) +
  geom_point()+
  geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 

ggplot(data = indivs[(term %like% 'dist')], aes(x =term, y = estimate)) + #This one
  geom_boxplot()+
  geom_jitter() +
  #geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 

ggplot(data = indivs[!(term %like% 'dist')], aes(x =term, y = estimate)) +
  geom_boxplot()+
  geom_jitter() +
  #geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip()  



### ---- WINTER ----
dat.issa.10.winter.close <- dat.issa.10.winter[dist_anthrodisturbance_250_start <= 20000]

gc()
m.closedisturbancewinter <- glmmTMB(case_ ~ -1 +
                                      I(log((sl_+1)/1000)) +
                                      I(cos(ta_)) +
                                      I(log((sl_+1)/1000)):I(cos(ta_)) +
                                      prop_forest_250_end + prop_forest_250_end:prop_forest_1000_end +
                                      prop_veg_250_end + prop_veg_250_end:prop_veg_1000_end +
                                      I(log(ts_fires_250_end+1)) + I(log(ts_fires_250_end+1)):I(log(ts_fires_1000_end+1)) +
                                      I(log(ts_harv250_end+1)) + I(log(ts_harv250_end+1)):I(log(ts_harv1000_end+1)) +
                                      I(log((dist_paved_250_end+1)/1000)) + I(log((dist_paved_250_end+1)/1000)):I(log((dist_paved_1000_end+1)/1000)) +
                                      I(log((dist_unpaved_250_end+1)/1000)) + I(log((dist_unpaved_250_end+1)/1000)):I(log((dist_unpaved_1000_end+1)/1000)) +
                                      #I(log(((dist_road_end+1)/1000))) + I(log((dist_road_250_end/1000)+1)):I(log((dist_road_1000_end/1000)+1)) +
                                      I(log((dist_lfother_250_end+1)/1000)) + I(log((dist_lfother_250_end+1)/1000)):I(log((dist_lfother_1000_end+1)/1000)) +
                                      I(log((dist_otherpoly_250_end+1)/1000)) + I(log((dist_otherpoly_250_end+1)/1000)):I(log((dist_otherpoly_1000_end+1)/1000)) +
                                      I(log((dist_placer_250_end+1)/1000)) + I(log((dist_placer_250_end+1)/1000)):I(log((dist_placer_1000_end+1)/1000)) +
                                      I(log((dist_quartz_250_end+1)/1000)) + I(log((dist_quartz_250_end+1)/1000)):I(log((dist_quartz_1000_end+1)/1000)) +
                                      I(log((dist_othermines_250_end+1)/1000)) + I(log((dist_othermines_250_end+1)/1000)):I(log((dist_othermines_1000_end+1)/1000)) +
                                      #I(log((dist_mines_250_end+1)/1000)) + I(log((dist_mines_250_end+1)/1000)):I(log((dist_mines_1000_end+1)/1000)) +
                                      #I(log((dist_otherdisturbance_250_end+1)/1000)) + I(log((dist_otherdisturbance_250_end+1)/1000)):I(log((dist_otherdisturbance_1000_end+1)/1000)) +
                                      
                                      (1|indiv_step_id) +
                                      (0 + I(log((sl_ +1)/1000))|id) +
                                      (0 + I(cos(ta_))|id) +
                                      (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                                      (0 + prop_forest_250_end|id) +
                                      (0 + prop_veg_250_end|id) +
                                      (0 + (I(log(ts_fires_250_end+1)))|id) +
                                      (0 + (I(log(ts_harv250_end+1)))|id) + 
                                      (0 + I(log((dist_paved_250_end+1)/1000))|id) + 
                                      (0 + I(log((dist_unpaved_250_end+1)/1000))|id) + 
                                      #(0 + I(log((dist_road_250_end+1)/1000))|id) + 
                                      (0 + I(log((dist_lfother_250_end+1)/1000))|id) +
                                      (0 + I(log((dist_otherpoly_250_end+1)/1000))|id) + 
                                      (0 + I(log((dist_placer_250_end+1)/1000))|id) + 
                                      (0 + I(log((dist_quartz_250_end+1)/1000))|id) + 
                                      (0 + I(log((dist_othermines_250_end+1)/1000))|id) + 
                                      #(0 + I(log((dist_mines_250_end+1)/1000))|id) + 
                                      #(0 + I(log((dist_otherdisturbance_250_end+1)/1000))|id) + 
                                      
                                      (1|year),
                                    family = poisson(), data = dat.issa.10.winter.close[,`:=`(year = as.factor(year), 
                                                                                              indiv_step_id = as.factor(indiv_step_id),
                                                                                              id = as.factor(id))],
                                    map= list(theta = factor(c(NA,1:15))),
                                    start = list(theta =c(log(1000), seq(0,0, length.out = 15))), 
                                    verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(m.closedisturbancewinter)
# save model output
saveRDS(m.closedisturbancewinter, "m.closedisturbancewinter.mod_lf20poly20.RDS")

m.closedisturbancewinter.indiv <- coef(m.closedisturbancewinter)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(m.closedisturbancewinter.indiv)[term!='(Intercept)']
pop <- setDT(tidy(m.closedisturbancewinter, effects = 'fixed'))


require(ggplot2)
ggplot(data = pop[term %like% 'dist'], aes(x =term, y = estimate)) +
  geom_point()+
  geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 
ggplot(data = pop[!(term %like% 'dist')], aes(x =term, y = estimate)) +
  geom_point()+
  geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 

ggplot(data = indivs[(term %like% 'dist')], aes(x =term, y = estimate)) + #This one
  geom_boxplot()+
  geom_jitter() +
  #geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 

ggplot(data = indivs[!(term %like% 'dist')], aes(x =term, y = estimate)) +
  geom_boxplot()+
  geom_jitter() +
  #geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip()  



## ---- FAR FROM DISTURBANCE (SKIPPING) ----

### ---- Year-round ----
gc()
m.fardisturbancefull <- glmmTMB(case_ ~ -1 +
                                  I(log((sl_+1)/1000)) +
                                  I(cos(ta_)) +
                                  I(log((sl_+1)/1000)):I(cos(ta_)) +
                                  prop_forest_250_end + prop_forest_250_end:prop_forest_1000_end +
                                  prop_veg_250_end + prop_veg_250_end:prop_veg_1000_end +
                                  I(log(ts_fires_250_end+1)) + I(log(ts_fires_250_end+1)):I(log(ts_fires_1000_end+1)) +
                                  I(log(ts_harv250_end+1)) + I(log(ts_harv250_end+1)):I(log(ts_harv1000_end+1)) +
                                  I(log((dist_paved_250_end+1)/1000)) + I(log((dist_paved_250_end+1)/1000)):I(log((dist_paved_1000_end+1)/1000)) +
                                  I(log((dist_unpaved_250_end+1)/1000)) + I(log((dist_unpaved_250_end+1)/1000)):I(log((dist_unpaved_1000_end+1)/1000)) +
                                  #I(log(((dist_road_end+1)/1000))) + I(log((dist_road_250_end/1000)+1)):I(log((dist_road_1000_end/1000)+1)) +
                                  I(log((dist_lfother_250_end+1)/1000)) + I(log((dist_lfother_250_end+1)/1000)):I(log((dist_lfother_1000_end+1)/1000)) +
                                  I(log((dist_otherpoly_250_end+1)/1000)) + I(log((dist_otherpoly_250_end+1)/1000)):I(log((dist_otherpoly_1000_end+1)/1000)) +
                                  I(log((dist_placer_250_end+1)/1000)) + I(log((dist_placer_250_end+1)/1000)):I(log((dist_placer_1000_end+1)/1000)) +
                                  I(log((dist_quartz_250_end+1)/1000)) + I(log((dist_quartz_250_end+1)/1000)):I(log((dist_quartz_1000_end+1)/1000)) +
                                  I(log((dist_othermines_250_end+1)/1000)) + I(log((dist_othermines_250_end+1)/1000)):I(log((dist_othermines_1000_end+1)/1000)) +
                                  #I(log((dist_mines_250_end+1)/1000)) + I(log((dist_mines_250_end+1)/1000)):I(log((dist_mines_1000_end+1)/1000)) +
                                  #I(log((dist_otherdisturbance_250_end+1)/1000)) + I(log((dist_otherdisturbance_250_end+1)/1000)):I(log((dist_otherdisturbance_1000_end+1)/1000)) +
                                  
                                  (1|indiv_step_id) +
                                  (0 + I(log((sl_ +1)/1000))|id) +
                                  (0 + I(cos(ta_))|id) +
                                  (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                                  (0 + prop_forest_250_end|id) +
                                  (0 + prop_veg_250_end|id) +
                                  (0 + (I(log(ts_fires_250_end+1)))|id) +
                                  (0 + (I(log(ts_harv250_end+1)))|id) + 
                                  #(0 + I(log((dist_paved_250_end+1)/1000))|id) + 
                                  #(0 + I(log((dist_unpaved_250_end+1)/1000))|id) + 
                                  #(0 + I(log((dist_road_250_end+1)/1000))|id) + 
                                  #(0 + I(log((dist_lfother_250_end+1)/1000))|id) +
                                  #(0 + I(log((dist_otherpoly_250_end+1)/1000))|id) + 
                                  #(0 + I(log((dist_placer_250_end+1)/1000))|id) + 
                                  #(0 + I(log((dist_quartz_250_end+1)/1000))|id) + 
                                  #(0 + I(log((dist_othermines_250_end+1)/1000))|id) + 
                                  #(0 + I(log((dist_mines_250_end+1)/1000))|id) + 
                                  #(0 + I(log((dist_otherdisturbance_250_end+1)/1000))|id) + 
                                  
                                  (1|year),
                                family = poisson(), data = dat.issa.10[,`:=`(year = as.factor(year), 
                                                                             indiv_step_id = as.factor(indiv_step_id),
                                                                             id = as.factor(id))],
                                map= list(theta = factor(c(NA,1:8))),
                                start = list(theta =c(log(1000), seq(0,0, length.out = 8))), 
                                verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(m.fardisturbancefull)
# save model output
saveRDS(m.fardisturbancefull, "m.fardisturbancefull.mod_lf20poly20.RDS")

m.fardisturbancefull.indiv <- coef(m.fardisturbancefull)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(m.fardisturbancefull.indiv)[term!='(Intercept)']
pop <- setDT(tidy(m.fardisturbancefull, effects = 'fixed'))


require(ggplot2)
ggplot(data = pop[term %like% 'dist'], aes(x =term, y = estimate)) +
  geom_point()+
  geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 
ggplot(data = pop[!(term %like% 'dist')], aes(x =term, y = estimate)) +
  geom_point()+
  geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 

ggplot(data = indivs[(term %like% 'dist')], aes(x =term, y = estimate)) + #This one
  geom_boxplot()+
  geom_jitter() +
  #geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 

ggplot(data = indivs[!(term %like% 'dist')], aes(x =term, y = estimate)) +
  geom_boxplot()+
  geom_jitter() +
  #geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip()  




### ---- SUMMER ----
gc()
m.fardisturbancesummer <- glmmTMB(case_ ~ -1 +
                                    I(log((sl_+1)/1000)) +
                                    I(cos(ta_)) +
                                    I(log((sl_+1)/1000)):I(cos(ta_)) +
                                    prop_forest_250_end + prop_forest_250_end:prop_forest_1000_end +
                                    prop_veg_250_end + prop_veg_250_end:prop_veg_1000_end +
                                    I(log(ts_fires_250_end+1)) + I(log(ts_fires_250_end+1)):I(log(ts_fires_1000_end+1)) +
                                    I(log(ts_harv250_end+1)) + I(log(ts_harv250_end+1)):I(log(ts_harv1000_end+1)) +
                                    I(log((dist_paved_250_end+1)/1000)) + I(log((dist_paved_250_end+1)/1000)):I(log((dist_paved_1000_end+1)/1000)) +
                                    I(log((dist_unpaved_250_end+1)/1000)) + I(log((dist_unpaved_250_end+1)/1000)):I(log((dist_unpaved_1000_end+1)/1000)) +
                                    #I(log(((dist_road_end+1)/1000))) + I(log((dist_road_250_end/1000)+1)):I(log((dist_road_1000_end/1000)+1)) +
                                    I(log((dist_lfother_250_end+1)/1000)) + I(log((dist_lfother_250_end+1)/1000)):I(log((dist_lfother_1000_end+1)/1000)) +
                                    I(log((dist_otherpoly_250_end+1)/1000)) + I(log((dist_otherpoly_250_end+1)/1000)):I(log((dist_otherpoly_1000_end+1)/1000)) +
                                    I(log((dist_placer_250_end+1)/1000)) + I(log((dist_placer_250_end+1)/1000)):I(log((dist_placer_1000_end+1)/1000)) +
                                    I(log((dist_quartz_250_end+1)/1000)) + I(log((dist_quartz_250_end+1)/1000)):I(log((dist_quartz_1000_end+1)/1000)) +
                                    I(log((dist_othermines_250_end+1)/1000)) + I(log((dist_othermines_250_end+1)/1000)):I(log((dist_othermines_1000_end+1)/1000)) +
                                    #I(log((dist_mines_250_end+1)/1000)) + I(log((dist_mines_250_end+1)/1000)):I(log((dist_mines_1000_end+1)/1000)) +
                                    #I(log((dist_otherdisturbance_250_end+1)/1000)) + I(log((dist_otherdisturbance_250_end+1)/1000)):I(log((dist_otherdisturbance_1000_end+1)/1000)) +
                                    
                                    (1|indiv_step_id) +
                                    (0 + I(log((sl_ +1)/1000))|id) +
                                    (0 + I(cos(ta_))|id) +
                                    (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                                    (0 + prop_forest_250_end|id) +
                                    (0 + prop_veg_250_end|id) +
                                    (0 + (I(log(ts_fires_250_end+1)))|id) +
                                    (0 + (I(log(ts_harv250_end+1)))|id) + 
                                    #(0 + I(log((dist_paved_250_end+1)/1000))|id) + 
                                    #(0 + I(log((dist_unpaved_250_end+1)/1000))|id) + 
                                    #(0 + I(log((dist_road_250_end+1)/1000))|id) + 
                                    #(0 + I(log((dist_lfother_250_end+1)/1000))|id) +
                                    #(0 + I(log((dist_otherpoly_250_end+1)/1000))|id) + 
                                    #(0 + I(log((dist_placer_250_end+1)/1000))|id) + 
                                    #(0 + I(log((dist_quartz_250_end+1)/1000))|id) + 
                                    #(0 + I(log((dist_othermines_250_end+1)/1000))|id) + 
                                    #(0 + I(log((dist_mines_250_end+1)/1000))|id) + 
                                    #(0 + I(log((dist_otherdisturbance_250_end+1)/1000))|id) + 
                                    
                                    (1|year),
                                  family = poisson(), data = dat.issa.10.summer[,`:=`(year = as.factor(year), 
                                                                                      indiv_step_id = as.factor(indiv_step_id),
                                                                                      id = as.factor(id))],
                                  map= list(theta = factor(c(NA,1:8))),
                                  start = list(theta =c(log(1000), seq(0,0, length.out = 8))), 
                                  verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)



summary(m.fardisturbancesummer)
# save model output
saveRDS(m.fardisturbancesummer, "m.fardisturbancesummer.mod_lf20poly20.RDS")

m.fardisturbancesummer.indiv <- coef(m.fardisturbancesummer)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(m.fardisturbancesummer.indiv)[term!='(Intercept)']
pop <- setDT(tidy(m.fardisturbancesummer, effects = 'fixed'))


require(ggplot2)
ggplot(data = pop[term %like% 'dist'], aes(x =term, y = estimate)) +
  geom_point()+
  geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 
ggplot(data = pop[!(term %like% 'dist')], aes(x =term, y = estimate)) +
  geom_point()+
  geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 

ggplot(data = indivs[(term %like% 'dist')], aes(x =term, y = estimate)) + #This one
  geom_boxplot()+
  geom_jitter() +
  #geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 

ggplot(data = indivs[!(term %like% 'dist')], aes(x =term, y = estimate)) +
  geom_boxplot()+
  geom_jitter() +
  #geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip()  



### ---- WINTER ----
gc()
m.fardisturbancewinter <- glmmTMB(case_ ~ -1 +
                                    I(log((sl_+1)/1000)) +
                                    I(cos(ta_)) +
                                    I(log((sl_+1)/1000)):I(cos(ta_)) +
                                    prop_forest_250_end + prop_forest_250_end:prop_forest_1000_end +
                                    prop_veg_250_end + prop_veg_250_end:prop_veg_1000_end +
                                    I(log(ts_fires_250_end+1)) + I(log(ts_fires_250_end+1)):I(log(ts_fires_1000_end+1)) +
                                    I(log(ts_harv250_end+1)) + I(log(ts_harv250_end+1)):I(log(ts_harv1000_end+1)) +
                                    I(log((dist_paved_250_end+1)/1000)) + I(log((dist_paved_250_end+1)/1000)):I(log((dist_paved_1000_end+1)/1000)) +
                                    I(log((dist_unpaved_250_end+1)/1000)) + I(log((dist_unpaved_250_end+1)/1000)):I(log((dist_unpaved_1000_end+1)/1000)) +
                                    #I(log(((dist_road_end+1)/1000))) + I(log((dist_road_250_end/1000)+1)):I(log((dist_road_1000_end/1000)+1)) +
                                    I(log((dist_lfother_250_end+1)/1000)) + I(log((dist_lfother_250_end+1)/1000)):I(log((dist_lfother_1000_end+1)/1000)) +
                                    I(log((dist_otherpoly_250_end+1)/1000)) + I(log((dist_otherpoly_250_end+1)/1000)):I(log((dist_otherpoly_1000_end+1)/1000)) +
                                    I(log((dist_placer_250_end+1)/1000)) + I(log((dist_placer_250_end+1)/1000)):I(log((dist_placer_1000_end+1)/1000)) +
                                    I(log((dist_quartz_250_end+1)/1000)) + I(log((dist_quartz_250_end+1)/1000)):I(log((dist_quartz_1000_end+1)/1000)) +
                                    I(log((dist_othermines_250_end+1)/1000)) + I(log((dist_othermines_250_end+1)/1000)):I(log((dist_othermines_1000_end+1)/1000)) +
                                    #I(log((dist_mines_250_end+1)/1000)) + I(log((dist_mines_250_end+1)/1000)):I(log((dist_mines_1000_end+1)/1000)) +
                                    #I(log((dist_otherdisturbance_250_end+1)/1000)) + I(log((dist_otherdisturbance_250_end+1)/1000)):I(log((dist_otherdisturbance_1000_end+1)/1000)) +
                                    
                                    (1|indiv_step_id) +
                                    (0 + I(log((sl_ +1)/1000))|id) +
                                    (0 + I(cos(ta_))|id) +
                                    (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                                    (0 + prop_forest_250_end|id) +
                                    (0 + prop_veg_250_end|id) +
                                    (0 + (I(log(ts_fires_250_end+1)))|id) +
                                    (0 + (I(log(ts_harv250_end+1)))|id) + 
                                    #(0 + I(log((dist_paved_250_end+1)/1000))|id) + 
                                    #(0 + I(log((dist_unpaved_250_end+1)/1000))|id) + 
                                    #(0 + I(log((dist_road_250_end+1)/1000))|id) + 
                                    #(0 + I(log((dist_lfother_250_end+1)/1000))|id) +
                                    #(0 + I(log((dist_otherpoly_250_end+1)/1000))|id) + 
                                    #(0 + I(log((dist_placer_250_end+1)/1000))|id) + 
                                    #(0 + I(log((dist_quartz_250_end+1)/1000))|id) + 
                                    #(0 + I(log((dist_othermines_250_end+1)/1000))|id) + 
                                    #(0 + I(log((dist_mines_250_end+1)/1000))|id) + 
                                    #(0 + I(log((dist_otherdisturbance_250_end+1)/1000))|id) + 
                                    
                                    (1|year),
                                  family = poisson(), data = dat.issa.10.winter[,`:=`(year = as.factor(year), 
                                                                                      indiv_step_id = as.factor(indiv_step_id),
                                                                                      id = as.factor(id))],
                                  map= list(theta = factor(c(NA,1:8))),
                                  start = list(theta =c(log(1000), seq(0,0, length.out = 8))), 
                                  verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)




summary(m.fardisturbancewinter)
# save model output
saveRDS(m.fardisturbancewinter, "m.fardisturbancewinter.mod_lf20poly20.RDS")

m.fardisturbancewinter.indiv <- coef(m.fardisturbancewinter)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(m.fardisturbancewinter.indiv)[term!='(Intercept)']
pop <- setDT(tidy(m.fardisturbancewinter, effects = 'fixed'))


require(ggplot2)
ggplot(data = pop[term %like% 'dist'], aes(x =term, y = estimate)) +
  geom_point()+
  geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 
ggplot(data = pop[!(term %like% 'dist')], aes(x =term, y = estimate)) +
  geom_point()+
  geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 

ggplot(data = indivs[(term %like% 'dist')], aes(x =term, y = estimate)) + #This one
  geom_boxplot()+
  geom_jitter() +
  #geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 

ggplot(data = indivs[!(term %like% 'dist')], aes(x =term, y = estimate)) +
  geom_boxplot()+
  geom_jitter() +
  #geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip()  


#NEED TO ADD SEASNON TO RANDOM EFFECTS FOR FULL MODELS 
## --- Full Models (using) ----
#Re-trying full dataset, close and far combined, seasons combined - it worked! Sticking with these for now as the main ones

### --- Year-round ---- 
#Trying 20 steps for each model now. Re-saving models as x20step (didn't work), trying 15, 15 didnt work
gc()
m.fullmod10step <- glmmTMB(case_ ~ -1 +
                             I(log((sl_+1)/1000)) +
                             I(cos(ta_)) +
                             I(log((sl_+1)/1000)):I(cos(ta_)) +
                             prop_forest_250_end + prop_forest_250_end:prop_forest_1000_end +
                             prop_veg_250_end + prop_veg_250_end:prop_veg_1000_end +
                             I(log(ts_fires_250_end+1)) + I(log(ts_fires_250_end+1)):I(log(ts_fires_1000_end+1)) +
                             I(log(ts_harv250_end+1)) + I(log(ts_harv250_end+1)):I(log(ts_harv1000_end+1)) +
                             I(log((dist_paved_250_end+1)/1000)) + I(log((dist_paved_250_end+1)/1000)):I(log((dist_paved_1000_end+1)/1000)) +
                             I(log((dist_unpaved_250_end+1)/1000)) + I(log((dist_unpaved_250_end+1)/1000)):I(log((dist_unpaved_1000_end+1)/1000)) +
                             #I(log(((dist_road_end+1)/1000))) + I(log((dist_road_250_end/1000)+1)):I(log((dist_road_1000_end/1000)+1)) +
                             I(log((dist_lfother_250_end+1)/1000)) + I(log((dist_lfother_250_end+1)/1000)):I(log((dist_lfother_1000_end+1)/1000)) +
                             I(log((dist_otherpoly_250_end+1)/1000)) + I(log((dist_otherpoly_250_end+1)/1000)):I(log((dist_otherpoly_1000_end+1)/1000)) +
                             I(log((dist_placer_250_end+1)/1000)) + I(log((dist_placer_250_end+1)/1000)):I(log((dist_placer_1000_end+1)/1000)) +
                             I(log((dist_quartz_250_end+1)/1000)) + I(log((dist_quartz_250_end+1)/1000)):I(log((dist_quartz_1000_end+1)/1000)) +
                             I(log((dist_othermines_250_end+1)/1000)) + I(log((dist_othermines_250_end+1)/1000)):I(log((dist_othermines_1000_end+1)/1000)) +
                             #I(log((dist_mines_250_end+1)/1000)) + I(log((dist_mines_250_end+1)/1000)):I(log((dist_mines_1000_end+1)/1000)) +
                             #I(log((dist_otherdisturbance_250_end+1)/1000)) + I(log((dist_otherdisturbance_250_end+1)/1000)):I(log((dist_otherdisturbance_1000_end+1)/1000)) +
                             
                             (1|indiv_step_id) +
                             (0 + I(log((sl_ +1)/1000))|id) +
                             (0 + I(cos(ta_))|id) +
                             (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                             (0 + prop_forest_250_end|id) +
                             (0 + prop_veg_250_end|id) +
                             (0 + (I(log(ts_fires_250_end+1)))|id) +
                             (0 + (I(log(ts_harv250_end+1)))|id) + 
                             (0 + I(log((dist_paved_250_end+1)/1000))|id) + 
                             (0 + I(log((dist_unpaved_250_end+1)/1000))|id) + 
                             #(0 + I(log((dist_road_250_end+1)/1000))|id) + 
                             (0 + I(log((dist_lfother_250_end+1)/1000))|id) +
                             (0 + I(log((dist_otherpoly_250_end+1)/1000))|id) + 
                             (0 + I(log((dist_placer_250_end+1)/1000))|id) + 
                             (0 + I(log((dist_quartz_250_end+1)/1000))|id) + 
                             (0 + I(log((dist_othermines_250_end+1)/1000))|id) + 
                             #(0 + I(log((dist_mines_250_end+1)/1000))|id) + 
                             #(0 + I(log((dist_otherdisturbance_250_end+1)/1000))|id) + 
                             
                             (1|year),
                           family = poisson(), data = dat.issa.10[,`:=`(year = as.factor(year), 
                                                                        indiv_step_id = as.factor(indiv_step_id),
                                                                        id = as.factor(id))],
                           map= list(theta = factor(c(NA,1:15))),
                           start = list(theta =c(log(1000), seq(0,0, length.out = 15))), 
                           verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(m.fullmod10step)
# save model output
saveRDS(m.fullmod10step, "m.fullmod10step.RDS")
#m.fullmod10step <- readRDS("m.fullmod10step.rds")

m.fullmod.indiv <- coef(m.fullmod)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(m.fullmod.indiv)[term!='(Intercept)']
pop <- setDT(tidy(m.fullmod, effects = 'fixed'))


require(ggplot2)
ggplot(data = pop[term %like% 'dist'], aes(x =term, y = estimate)) +
  geom_point()+
  geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 
ggplot(data = pop[!(term %like% 'dist')], aes(x =term, y = estimate)) +
  geom_point()+
  geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 

ggplot(data = indivs[(term %like% 'dist')], aes(x =term, y = estimate)) + #This one
  geom_boxplot()+
  geom_jitter() +
  #geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 

ggplot(data = indivs[!(term %like% 'dist')], aes(x =term, y = estimate)) +
  geom_boxplot()+
  geom_jitter() +
  #geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip()  


###Functional response plots:
##Start with random slopes vs avg availability

dt <- as.data.table(dat.issa.10)

#Get avg availability by id
vars_1000 <- c(
  "prop_forest_1000_end",
  "prop_veg_1000_end",
  "ts_fires_1000_end",
  "ts_harv1000_end",
  "dist_paved_1000_end",
  "dist_unpaved_1000_end",
  "dist_lfother_1000_end",
  "dist_otherpoly_1000_end",
  "dist_placer_1000_end",
  "dist_quartz_1000_end",
  "dist_othermines_1000_end"
)

avg_data <- dt[, lapply(.SD, mean, na.rm = TRUE), by = id, .SDcols = vars_1000]

avg_data[, `:=`(
  ts_fires_1000_end       = log(ts_fires_1000_end + 1),
  ts_harv1000_end         = log(ts_harv1000_end + 1),
  dist_paved_1000_end     = log((dist_paved_1000_end + 1) / 1000),
  dist_unpaved_1000_end   = log((dist_unpaved_1000_end + 1) / 1000),
  dist_lfother_1000_end   = log((dist_lfother_1000_end + 1) / 1000),
  dist_otherpoly_1000_end = log((dist_otherpoly_1000_end + 1) / 1000),
  dist_placer_1000_end    = log((dist_placer_1000_end + 1) / 1000),
  dist_quartz_1000_end    = log((dist_quartz_1000_end + 1) / 1000),
  dist_othermines_1000_end= log((dist_othermines_1000_end + 1) / 1000)
)]

re_list <- ranef(m.fullmod)$cond

var_map <- list(
  "I(log((dist_paved_250_end + 1)/1000))" = "dist_paved_1000_end",
  "I(log((dist_unpaved_250_end + 1)/1000))" = "dist_unpaved_1000_end",
  "I(log((dist_lfother_250_end + 1)/1000))" = "dist_lfother_1000_end",
  "I(log((dist_otherpoly_250_end + 1)/1000))" = "dist_otherpoly_1000_end",
  "I(log((dist_placer_250_end + 1)/1000))" = "dist_placer_1000_end",
  "I(log((dist_quartz_250_end + 1)/1000))" = "dist_quartz_1000_end",
  "I(log((dist_othermines_250_end + 1)/1000))" = "dist_othermines_1000_end",
  "prop_forest_250_end" = "prop_forest_1000_end",
  "prop_veg_250_end" = "prop_veg_1000_end",
  "I(log(ts_fires_250_end + 1))" = "ts_fires_1000_end",
  "I(log(ts_harv250_end + 1))" = "ts_harv1000_end"
)

# Convert random slopes to data.table
random_slopes <- as.data.table(re_list$id, keep.rownames = "id")

axis_labels <- list(
  "I(log((dist_paved_250_end + 1)/1000))"      = list(x = "dist_paved avail", y = "dist_paved"),
  "I(log((dist_unpaved_250_end + 1)/1000))"    = list(x = "dist_unpaved avail", y = "dist_unpaved"),
  "I(log(ts_fires_250_end + 1))"               = list(x = "ts_fires avail", y = "ts_fires"),
  "I(log(ts_harv250_end + 1))"                 = list(x = "ts_harv avail", y = "ts_harv"),
  "prop_forest_250_end"                        = list(x = "prop_forest avail", y = "prop_forest"),
  "prop_veg_250_end"                           = list(x = "prop_veg avail", y = "prop_veg"),
  "I(log((dist_lfother_250_end + 1)/1000))"    = list(x = "dist_lfother avail", y = "dist_lfother"),
  "I(log((dist_otherpoly_250_end + 1)/1000))"  = list(x = "dist_otherpoly avail", y = "dist_otherpoly"),
  "I(log((dist_placer_250_end + 1)/1000))"     = list(x = "dist_placer avail", y = "dist_placer"),
  "I(log((dist_quartz_250_end + 1)/1000))"     = list(x = "dist_quartz avail", y = "dist_quartz"),
  "I(log((dist_othermines_250_end + 1)/1000))" = list(x = "dist_othermines avail", y = "dist_othermines")
)

plot_list <- list()

# Loop through each variable
for (model_var in names(var_map)) {
  avail_var <- var_map[[model_var]]
  
  plot_data <- merge(
    avg_data[, .(id, avail = get(avail_var))],
    random_slopes[, .(id, slope = get(model_var))],
    by = "id"
  )
  
  if (!is.null(axis_labels[[model_var]])) {
    x_lab <- axis_labels[[model_var]]$x
    y_lab <- axis_labels[[model_var]]$y
  } else {
    x_lab <- paste0("Average ", avail_var)
    y_lab <- paste0("Random slope for ", model_var)
  }
  
  if (avail_var == "ts_harv1000_end") {
    plot_data <- plot_data[avail >= 4.3]
  } else if (avail_var == "ts_fires_1000_end") {
    plot_data <- plot_data[avail >= 5.0]
  }
  
  print(paste("Plotting", model_var, "with x label:", x_lab, "and rows:", nrow(plot_data)))
  
  p <- ggplot(plot_data, aes(x = avail, y = slope)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "loess", color = "darkblue", se = TRUE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", size = 0.8) +
    labs(x = x_lab, y = "selection coeff") +
    theme_minimal(base_size = 10)
  
  plot_list[[model_var]] <- p
}

require(patchwork)
# Combine and print all plots
combined_plot <- wrap_plots(plot_list, ncol = 3)
print(combined_plot)




##Next plot fixed effect interaction vs average availability


# 1. ts_fires_250_end

# Extract random slopes
re_id <- as.data.table(ranef(m.fullmod)$cond$id)
re_id$id <- rownames(ranef(m.fullmod)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log(ts_fires_250_end + 1))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = ts_fires_250_end)],
  avg_data[, .(id, raw_Ma_x1 = ts_fires_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log(raw_x1 + 1)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmod))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log(ts_fires_250_end + 1)):I(log(ts_fires_1000_end + 1))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]

p1 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "ts_fires_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p1)


# 2. ts_harv250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmod)$cond$id)
re_id$id <- rownames(ranef(m.fullmod)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log(ts_harv250_end + 1))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = ts_harv250_end)],
  avg_data[, .(id, raw_Ma_x1 = ts_harv1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log(raw_x1 + 1)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmod))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log(ts_harv250_end + 1)):I(log(ts_harv1000_end + 1))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]

p2 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "ts_harv1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p2)



# 3. dist_paved_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmod)$cond$id)
re_id$id <- rownames(ranef(m.fullmod)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_paved_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_paved_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_paved_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmod))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_paved_250_end + 1)/1000)):I(log((dist_paved_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]

p3 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_paved_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p3)



# 4. dist_unpaved_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmod)$cond$id)
re_id$id <- rownames(ranef(m.fullmod)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_unpaved_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_unpaved_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_unpaved_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmod))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_unpaved_250_end + 1)/1000)):I(log((dist_unpaved_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p4 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_unpaved_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p4)


# 5. dist_lfother_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmod)$cond$id)
re_id$id <- rownames(ranef(m.fullmod)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_lfother_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_lfother_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_lfother_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmod))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_lfother_250_end + 1)/1000)):I(log((dist_lfother_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p5 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_lfother_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p5)


# 6. dist_otherpoly_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmod)$cond$id)
re_id$id <- rownames(ranef(m.fullmod)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_otherpoly_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_otherpoly_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_otherpoly_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmod))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_otherpoly_250_end + 1)/1000)):I(log((dist_otherpoly_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p6 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_otherpoly_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p6)


# 7. dist_placer_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmod)$cond$id)
re_id$id <- rownames(ranef(m.fullmod)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_placer_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_placer_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_placer_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmod))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_placer_250_end + 1)/1000)):I(log((dist_placer_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p7 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_placer_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p7)


# 8. dist_quartz_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmod)$cond$id)
re_id$id <- rownames(ranef(m.fullmod)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_quartz_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_quartz_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_quartz_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmod))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_quartz_250_end + 1)/1000)):I(log((dist_quartz_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p8 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_quartz_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p8)



# 9. dist_othermines_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmod)$cond$id)
re_id$id <- rownames(ranef(m.fullmod)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_othermines_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_othermines_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_othermines_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmod))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_othermines_250_end + 1)/1000)):I(log((dist_othermines_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p9 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_othermines_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p9)



# 10. prop_forest_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmod)$cond$id)
re_id$id <- rownames(ranef(m.fullmod)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `prop_forest_250_end`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = prop_forest_250_end)],
  avg_data[, .(id, raw_Ma_x1 = prop_forest_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmod))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "prop_forest_250_end:prop_forest_1000_end"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p10 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "prop_forest_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p10)



# 11. prop_veg_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmod)$cond$id)
re_id$id <- rownames(ranef(m.fullmod)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `prop_veg_250_end`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = prop_veg_250_end)],
  avg_data[, .(id, raw_Ma_x1 = prop_veg_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmod))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "prop_veg_250_end:prop_veg_1000_end"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p11 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "prop_veg_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p11)


plot_list <- list(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)
patchwork::wrap_plots(plot_list, ncol = 4)



### --- Summer ----
gc()
m.fullmodsummer20step <- glmmTMB(case_ ~ -1 +
                                   I(log((sl_+1)/1000)) +
                                   I(cos(ta_)) +
                                   I(log((sl_+1)/1000)):I(cos(ta_)) +
                                   prop_forest_250_end + prop_forest_250_end:prop_forest_1000_end +
                                   prop_veg_250_end + prop_veg_250_end:prop_veg_1000_end +
                                   I(log(ts_fires_250_end+1)) + I(log(ts_fires_250_end+1)):I(log(ts_fires_1000_end+1)) +
                                   I(log(ts_harv250_end+1)) + I(log(ts_harv250_end+1)):I(log(ts_harv1000_end+1)) +
                                   I(log((dist_paved_250_end+1)/1000)) + I(log((dist_paved_250_end+1)/1000)):I(log((dist_paved_1000_end+1)/1000)) +
                                   I(log((dist_unpaved_250_end+1)/1000)) + I(log((dist_unpaved_250_end+1)/1000)):I(log((dist_unpaved_1000_end+1)/1000)) +
                                   #I(log(((dist_road_end+1)/1000))) + I(log((dist_road_250_end/1000)+1)):I(log((dist_road_1000_end/1000)+1)) +
                                   I(log((dist_lfother_250_end+1)/1000)) + I(log((dist_lfother_250_end+1)/1000)):I(log((dist_lfother_1000_end+1)/1000)) +
                                   I(log((dist_otherpoly_250_end+1)/1000)) + I(log((dist_otherpoly_250_end+1)/1000)):I(log((dist_otherpoly_1000_end+1)/1000)) +
                                   I(log((dist_placer_250_end+1)/1000)) + I(log((dist_placer_250_end+1)/1000)):I(log((dist_placer_1000_end+1)/1000)) +
                                   I(log((dist_quartz_250_end+1)/1000)) + I(log((dist_quartz_250_end+1)/1000)):I(log((dist_quartz_1000_end+1)/1000)) +
                                   I(log((dist_othermines_250_end+1)/1000)) + I(log((dist_othermines_250_end+1)/1000)):I(log((dist_othermines_1000_end+1)/1000)) +
                                   #I(log((dist_mines_250_end+1)/1000)) + I(log((dist_mines_250_end+1)/1000)):I(log((dist_mines_1000_end+1)/1000)) +
                                   #I(log((dist_otherdisturbance_250_end+1)/1000)) + I(log((dist_otherdisturbance_250_end+1)/1000)):I(log((dist_otherdisturbance_1000_end+1)/1000)) +
                                   
                                   (1|indiv_step_id) +
                                   (0 + I(log((sl_ +1)/1000))|id) +
                                   (0 + I(cos(ta_))|id) +
                                   (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                                   (0 + prop_forest_250_end|id) +
                                   (0 + prop_veg_250_end|id) +
                                   (0 + (I(log(ts_fires_250_end+1)))|id) +
                                   (0 + (I(log(ts_harv250_end+1)))|id) + 
                                   (0 + I(log((dist_paved_250_end+1)/1000))|id) + 
                                   (0 + I(log((dist_unpaved_250_end+1)/1000))|id) + 
                                   #(0 + I(log((dist_road_250_end+1)/1000))|id) + 
                                   (0 + I(log((dist_lfother_250_end+1)/1000))|id) +
                                   (0 + I(log((dist_otherpoly_250_end+1)/1000))|id) + 
                                   (0 + I(log((dist_placer_250_end+1)/1000))|id) + 
                                   (0 + I(log((dist_quartz_250_end+1)/1000))|id) + 
                                   (0 + I(log((dist_othermines_250_end+1)/1000))|id) + 
                                   #(0 + I(log((dist_mines_250_end+1)/1000))|id) + 
                                   #(0 + I(log((dist_otherdisturbance_250_end+1)/1000))|id) + 
                                   
                                   (1|year),
                                 family = poisson(), data = dat.issa.20.summer[,`:=`(year = as.factor(year), 
                                                                                     indiv_step_id = as.factor(indiv_step_id),
                                                                                     id = as.factor(id))],
                                 map= list(theta = factor(c(NA,1:15))),
                                 start = list(theta =c(log(1000), seq(0,0, length.out = 15))), 
                                 verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(m.fullmodsummer20step)
# save model output
saveRDS(m.fullmodsummer20step, "m.fullmodsummer20step.RDS")
#m.fullmodsummer20step <- readRDS("m.fullmodsummer20step.RDS")


m.fullmodsummer20step.indiv <- coef(m.fullmodsummer20step)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(m.fullmodsummer20step.indiv)[term!='(Intercept)']
pop <- setDT(tidy(m.fullmodsummer20step, effects = 'fixed'))


require(ggplot2)
ggplot(data = pop[term %like% 'dist'], aes(x =term, y = estimate)) +
  geom_point()+
  geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 
ggplot(data = pop[!(term %like% 'dist')], aes(x =term, y = estimate)) +
  geom_point()+
  geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 

ggplot(data = indivs[(term %like% 'dist')], aes(x =term, y = estimate)) + #This one
  geom_boxplot()+
  geom_jitter() +
  #geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 

ggplot(data = indivs[!(term %like% 'dist')], aes(x =term, y = estimate)) +
  geom_boxplot()+
  geom_jitter() +
  #geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip()  




###Functional response plots:
##Start with random slopes vs avg availability

dt <- as.data.table(dat.issa.20.summer)

#Get avg availability by id
vars_1000 <- c(
  "prop_forest_1000_end",
  "prop_veg_1000_end",
  "ts_fires_1000_end",
  "ts_harv1000_end",
  "dist_paved_1000_end",
  "dist_unpaved_1000_end",
  "dist_lfother_1000_end",
  "dist_otherpoly_1000_end",
  "dist_placer_1000_end",
  "dist_quartz_1000_end",
  "dist_othermines_1000_end"
)

avg_data <- dt[, lapply(.SD, mean, na.rm = TRUE), by = id, .SDcols = vars_1000]

avg_data[, `:=`(
  ts_fires_1000_end       = log(ts_fires_1000_end + 1),
  ts_harv1000_end         = log(ts_harv1000_end + 1),
  dist_paved_1000_end     = log((dist_paved_1000_end + 1) / 1000),
  dist_unpaved_1000_end   = log((dist_unpaved_1000_end + 1) / 1000),
  dist_lfother_1000_end   = log((dist_lfother_1000_end + 1) / 1000),
  dist_otherpoly_1000_end = log((dist_otherpoly_1000_end + 1) / 1000),
  dist_placer_1000_end    = log((dist_placer_1000_end + 1) / 1000),
  dist_quartz_1000_end    = log((dist_quartz_1000_end + 1) / 1000),
  dist_othermines_1000_end= log((dist_othermines_1000_end + 1) / 1000)
)]

re_list <- ranef(m.fullmodsummer20step)$cond

var_map <- list(
  "I(log((dist_paved_250_end + 1)/1000))" = "dist_paved_1000_end",
  "I(log((dist_unpaved_250_end + 1)/1000))" = "dist_unpaved_1000_end",
  "I(log((dist_lfother_250_end + 1)/1000))" = "dist_lfother_1000_end",
  "I(log((dist_otherpoly_250_end + 1)/1000))" = "dist_otherpoly_1000_end",
  "I(log((dist_placer_250_end + 1)/1000))" = "dist_placer_1000_end",
  "I(log((dist_quartz_250_end + 1)/1000))" = "dist_quartz_1000_end",
  "I(log((dist_othermines_250_end + 1)/1000))" = "dist_othermines_1000_end",
  "prop_forest_250_end" = "prop_forest_1000_end",
  "prop_veg_250_end" = "prop_veg_1000_end",
  "I(log(ts_fires_250_end + 1))" = "ts_fires_1000_end",
  "I(log(ts_harv250_end + 1))" = "ts_harv1000_end"
)

# Convert random slopes to data.table
random_slopes <- as.data.table(re_list$id, keep.rownames = "id")

axis_labels <- list(
  "I(log((dist_paved_250_end + 1)/1000))"      = list(x = "dist_paved avail", y = "dist_paved"),
  "I(log((dist_unpaved_250_end + 1)/1000))"    = list(x = "dist_unpaved avail", y = "dist_unpaved"),
  "I(log(ts_fires_250_end + 1))"               = list(x = "ts_fires avail", y = "ts_fires"),
  "I(log(ts_harv250_end + 1))"                 = list(x = "ts_harv avail", y = "ts_harv"),
  "prop_forest_250_end"                        = list(x = "prop_forest avail", y = "prop_forest"),
  "prop_veg_250_end"                           = list(x = "prop_veg avail", y = "prop_veg"),
  "I(log((dist_lfother_250_end + 1)/1000))"    = list(x = "dist_lfother avail", y = "dist_lfother"),
  "I(log((dist_otherpoly_250_end + 1)/1000))"  = list(x = "dist_otherpoly avail", y = "dist_otherpoly"),
  "I(log((dist_placer_250_end + 1)/1000))"     = list(x = "dist_placer avail", y = "dist_placer"),
  "I(log((dist_quartz_250_end + 1)/1000))"     = list(x = "dist_quartz avail", y = "dist_quartz"),
  "I(log((dist_othermines_250_end + 1)/1000))" = list(x = "dist_othermines avail", y = "dist_othermines")
)

plot_list <- list()

# Loop through each variable
for (model_var in names(var_map)) {
  avail_var <- var_map[[model_var]]
  
  plot_data <- merge(
    avg_data[, .(id, avail = get(avail_var))],
    random_slopes[, .(id, slope = get(model_var))],
    by = "id"
  )
  
  if (!is.null(axis_labels[[model_var]])) {
    x_lab <- axis_labels[[model_var]]$x
    y_lab <- axis_labels[[model_var]]$y
  } else {
    x_lab <- paste0("Average ", avail_var)
    y_lab <- paste0("Random slope for ", model_var)
  }
  
  if (avail_var == "ts_harv1000_end") {
    plot_data <- plot_data[avail >= 4.0]
    #} else if (avail_var == "ts_fires_1000_end") {
    #plot_data <- plot_data[avail >= 5.0]
  }
  
  print(paste("Plotting", model_var, "with x label:", x_lab, "and rows:", nrow(plot_data)))
  
  p <- ggplot(plot_data, aes(x = avail, y = slope)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "loess", color = "darkblue", se = TRUE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", size = 0.8) +
    labs(x = x_lab, y = "selection coeff") +
    theme_minimal(base_size = 10)
  
  plot_list[[model_var]] <- p
}

require(patchwork)
# Combine and print all plots
combined_plot <- wrap_plots(plot_list, ncol = 3)
print(combined_plot)


##Next plot fixed effect interaction vs average availability

# 1. ts_fires_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodsummer20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodsummer20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log(ts_fires_250_end + 1))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = ts_fires_250_end)],
  avg_data[, .(id, raw_Ma_x1 = ts_fires_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log(raw_x1 + 1)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodsummer20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log(ts_fires_250_end + 1)):I(log(ts_fires_1000_end + 1))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]

p1 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "ts_fires_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p1)

# 2. ts_harv250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodsummer20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodsummer20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log(ts_harv250_end + 1))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = ts_harv250_end)],
  avg_data[, .(id, raw_Ma_x1 = ts_harv1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log(raw_x1 + 1)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodsummer20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log(ts_harv250_end + 1)):I(log(ts_harv1000_end + 1))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]

p2 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "ts_harv1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p2)


# 3. dist_paved_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodsummer20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodsummer20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_paved_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_paved_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_paved_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodsummer20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_paved_250_end + 1)/1000)):I(log((dist_paved_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]

p3 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_paved_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p3)


# 4. dist_unpaved_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodsummer20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodsummer20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_unpaved_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_unpaved_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_unpaved_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodsummer20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_unpaved_250_end + 1)/1000)):I(log((dist_unpaved_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p4 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_unpaved_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p4)


# 5. dist_lfother_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodsummer20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodsummer20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_lfother_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_lfother_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_lfother_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodsummer20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_lfother_250_end + 1)/1000)):I(log((dist_lfother_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p5 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_lfother_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p5)


# 6. dist_otherpoly_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodsummer20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodsummer20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_otherpoly_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_otherpoly_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_otherpoly_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodsummer20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_otherpoly_250_end + 1)/1000)):I(log((dist_otherpoly_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p6 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_otherpoly_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p6)

# 7. dist_placer_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodsummer20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodsummer20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_placer_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_placer_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_placer_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodsummer20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_placer_250_end + 1)/1000)):I(log((dist_placer_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p7 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_placer_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p7)


# 8. dist_quartz_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodsummer20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodsummer20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_quartz_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_quartz_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_quartz_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodsummer20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_quartz_250_end + 1)/1000)):I(log((dist_quartz_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p8 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_quartz_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p8)


# 9. dist_othermines_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodsummer20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodsummer20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_othermines_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_othermines_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_othermines_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodsummer20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_othermines_250_end + 1)/1000)):I(log((dist_othermines_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p9 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_othermines_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p9)


# 10. prop_forest_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodsummer20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodsummer20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `prop_forest_250_end`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = prop_forest_250_end)],
  avg_data[, .(id, raw_Ma_x1 = prop_forest_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodsummer20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "prop_forest_250_end:prop_forest_1000_end"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p10 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "prop_forest_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p10)


# 11. prop_veg_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodsummer20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodsummer20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `prop_veg_250_end`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = prop_veg_250_end)],
  avg_data[, .(id, raw_Ma_x1 = prop_veg_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodsummer20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "prop_veg_250_end:prop_veg_1000_end"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p11 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "prop_veg_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p11)


plot_list <- list(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)
patchwork::wrap_plots(plot_list, ncol = 4)




### --- Winter ----
gc()
m.fullmodwinter20step <- glmmTMB(case_ ~ -1 +
                                   I(log((sl_+1)/1000)) +
                                   I(cos(ta_)) +
                                   I(log((sl_+1)/1000)):I(cos(ta_)) +
                                   prop_forest_250_end + prop_forest_250_end:prop_forest_1000_end +
                                   prop_veg_250_end + prop_veg_250_end:prop_veg_1000_end +
                                   I(log(ts_fires_250_end+1)) + I(log(ts_fires_250_end+1)):I(log(ts_fires_1000_end+1)) +
                                   I(log(ts_harv250_end+1)) + I(log(ts_harv250_end+1)):I(log(ts_harv1000_end+1)) +
                                   I(log((dist_paved_250_end+1)/1000)) + I(log((dist_paved_250_end+1)/1000)):I(log((dist_paved_1000_end+1)/1000)) +
                                   I(log((dist_unpaved_250_end+1)/1000)) + I(log((dist_unpaved_250_end+1)/1000)):I(log((dist_unpaved_1000_end+1)/1000)) +
                                   #I(log(((dist_road_end+1)/1000))) + I(log((dist_road_250_end/1000)+1)):I(log((dist_road_1000_end/1000)+1)) +
                                   I(log((dist_lfother_250_end+1)/1000)) + I(log((dist_lfother_250_end+1)/1000)):I(log((dist_lfother_1000_end+1)/1000)) +
                                   I(log((dist_otherpoly_250_end+1)/1000)) + I(log((dist_otherpoly_250_end+1)/1000)):I(log((dist_otherpoly_1000_end+1)/1000)) +
                                   I(log((dist_placer_250_end+1)/1000)) + I(log((dist_placer_250_end+1)/1000)):I(log((dist_placer_1000_end+1)/1000)) +
                                   I(log((dist_quartz_250_end+1)/1000)) + I(log((dist_quartz_250_end+1)/1000)):I(log((dist_quartz_1000_end+1)/1000)) +
                                   I(log((dist_othermines_250_end+1)/1000)) + I(log((dist_othermines_250_end+1)/1000)):I(log((dist_othermines_1000_end+1)/1000)) +
                                   #I(log((dist_mines_250_end+1)/1000)) + I(log((dist_mines_250_end+1)/1000)):I(log((dist_mines_1000_end+1)/1000)) +
                                   #I(log((dist_otherdisturbance_250_end+1)/1000)) + I(log((dist_otherdisturbance_250_end+1)/1000)):I(log((dist_otherdisturbance_1000_end+1)/1000)) +
                                   
                                   (1|indiv_step_id) +
                                   (0 + I(log((sl_ +1)/1000))|id) +
                                   (0 + I(cos(ta_))|id) +
                                   (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                                   (0 + prop_forest_250_end|id) +
                                   (0 + prop_veg_250_end|id) +
                                   (0 + (I(log(ts_fires_250_end+1)))|id) +
                                   (0 + (I(log(ts_harv250_end+1)))|id) + 
                                   (0 + I(log((dist_paved_250_end+1)/1000))|id) + 
                                   (0 + I(log((dist_unpaved_250_end+1)/1000))|id) + 
                                   #(0 + I(log((dist_road_250_end+1)/1000))|id) + 
                                   (0 + I(log((dist_lfother_250_end+1)/1000))|id) +
                                   (0 + I(log((dist_otherpoly_250_end+1)/1000))|id) + 
                                   (0 + I(log((dist_placer_250_end+1)/1000))|id) + 
                                   (0 + I(log((dist_quartz_250_end+1)/1000))|id) + 
                                   (0 + I(log((dist_othermines_250_end+1)/1000))|id) + 
                                   #(0 + I(log((dist_mines_250_end+1)/1000))|id) + 
                                   #(0 + I(log((dist_otherdisturbance_250_end+1)/1000))|id) + 
                                   
                                   (1|year),
                                 family = poisson(), data = dat.issa.20.winter[,`:=`(year = as.factor(year), 
                                                                                     indiv_step_id = as.factor(indiv_step_id),
                                                                                     id = as.factor(id))],
                                 map= list(theta = factor(c(NA,1:15))),
                                 start = list(theta =c(log(1000), seq(0,0, length.out = 15))), 
                                 verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(m.fullmodwinter20step)
# save model output
saveRDS(m.fullmodwinter20step, "m.fullmodwinter20step.RDS")
#m.fullmodwinter20step <- readRDS("m.fullmodwinter20step.RDS")


m.fullmodwinter20step.indiv <- coef(m.fullmodwinter20step)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(m.fullmodwinter20step.indiv)[term!='(Intercept)']
pop <- setDT(tidy(m.fullmodwinter20step, effects = 'fixed'))


require(ggplot2)
ggplot(data = pop[term %like% 'dist'], aes(x =term, y = estimate)) +
  geom_point()+
  geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 
ggplot(data = pop[!(term %like% 'dist')], aes(x =term, y = estimate)) +
  geom_point()+
  geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 

ggplot(data = indivs[(term %like% 'dist')], aes(x =term, y = estimate)) + #This one
  geom_boxplot()+
  geom_jitter() +
  #geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 

ggplot(data = indivs[!(term %like% 'dist')], aes(x =term, y = estimate)) +
  geom_boxplot()+
  geom_jitter() +
  #geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip()  



###Functional response plots:
##Start with random slopes vs avg availability

dt <- as.data.table(dat.issa.20.winter)

#Get avg availability by id
vars_1000 <- c(
  "prop_forest_1000_end",
  "prop_veg_1000_end",
  "ts_fires_1000_end",
  "ts_harv1000_end",
  "dist_paved_1000_end",
  "dist_unpaved_1000_end",
  "dist_lfother_1000_end",
  "dist_otherpoly_1000_end",
  "dist_placer_1000_end",
  "dist_quartz_1000_end",
  "dist_othermines_1000_end"
)

avg_data <- dt[, lapply(.SD, mean, na.rm = TRUE), by = id, .SDcols = vars_1000]

avg_data[, `:=`(
  ts_fires_1000_end       = log(ts_fires_1000_end + 1),
  ts_harv1000_end         = log(ts_harv1000_end + 1),
  dist_paved_1000_end     = log((dist_paved_1000_end + 1) / 1000),
  dist_unpaved_1000_end   = log((dist_unpaved_1000_end + 1) / 1000),
  dist_lfother_1000_end   = log((dist_lfother_1000_end + 1) / 1000),
  dist_otherpoly_1000_end = log((dist_otherpoly_1000_end + 1) / 1000),
  dist_placer_1000_end    = log((dist_placer_1000_end + 1) / 1000),
  dist_quartz_1000_end    = log((dist_quartz_1000_end + 1) / 1000),
  dist_othermines_1000_end= log((dist_othermines_1000_end + 1) / 1000)
)]

re_list <- ranef(m.fullmodwinter20step)$cond

var_map <- list(
  "I(log((dist_paved_250_end + 1)/1000))" = "dist_paved_1000_end",
  "I(log((dist_unpaved_250_end + 1)/1000))" = "dist_unpaved_1000_end",
  "I(log((dist_lfother_250_end + 1)/1000))" = "dist_lfother_1000_end",
  "I(log((dist_otherpoly_250_end + 1)/1000))" = "dist_otherpoly_1000_end",
  "I(log((dist_placer_250_end + 1)/1000))" = "dist_placer_1000_end",
  "I(log((dist_quartz_250_end + 1)/1000))" = "dist_quartz_1000_end",
  "I(log((dist_othermines_250_end + 1)/1000))" = "dist_othermines_1000_end",
  "prop_forest_250_end" = "prop_forest_1000_end",
  "prop_veg_250_end" = "prop_veg_1000_end",
  "I(log(ts_fires_250_end + 1))" = "ts_fires_1000_end",
  "I(log(ts_harv250_end + 1))" = "ts_harv1000_end"
)

# Convert random slopes to data.table
random_slopes <- as.data.table(re_list$id, keep.rownames = "id")

axis_labels <- list(
  "I(log((dist_paved_250_end + 1)/1000))"      = list(x = "dist_paved avail", y = "dist_paved"),
  "I(log((dist_unpaved_250_end + 1)/1000))"    = list(x = "dist_unpaved avail", y = "dist_unpaved"),
  "I(log(ts_fires_250_end + 1))"               = list(x = "ts_fires avail", y = "ts_fires"),
  "I(log(ts_harv250_end + 1))"                 = list(x = "ts_harv avail", y = "ts_harv"),
  "prop_forest_250_end"                        = list(x = "prop_forest avail", y = "prop_forest"),
  "prop_veg_250_end"                           = list(x = "prop_veg avail", y = "prop_veg"),
  "I(log((dist_lfother_250_end + 1)/1000))"    = list(x = "dist_lfother avail", y = "dist_lfother"),
  "I(log((dist_otherpoly_250_end + 1)/1000))"  = list(x = "dist_otherpoly avail", y = "dist_otherpoly"),
  "I(log((dist_placer_250_end + 1)/1000))"     = list(x = "dist_placer avail", y = "dist_placer"),
  "I(log((dist_quartz_250_end + 1)/1000))"     = list(x = "dist_quartz avail", y = "dist_quartz"),
  "I(log((dist_othermines_250_end + 1)/1000))" = list(x = "dist_othermines avail", y = "dist_othermines")
)

plot_list <- list()

# Loop through each variable
for (model_var in names(var_map)) {
  avail_var <- var_map[[model_var]]
  
  plot_data <- merge(
    avg_data[, .(id, avail = get(avail_var))],
    random_slopes[, .(id, slope = get(model_var))],
    by = "id"
  )
  
  if (!is.null(axis_labels[[model_var]])) {
    x_lab <- axis_labels[[model_var]]$x
    y_lab <- axis_labels[[model_var]]$y
  } else {
    x_lab <- paste0("Average ", avail_var)
    y_lab <- paste0("Random slope for ", model_var)
  }
  
  if (avail_var == "ts_fires_1000_end") {
    plot_data <- plot_data[avail >= 4.9]
    #} else if (avail_var == "ts_fires_1000_end") {
    #plot_data <- plot_data[avail >= 5.0]
  }
  
  print(paste("Plotting", model_var, "with x label:", x_lab, "and rows:", nrow(plot_data)))
  
  p <- ggplot(plot_data, aes(x = avail, y = slope)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "loess", color = "darkblue", se = TRUE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", size = 0.8) +
    labs(x = x_lab, y = "selection coeff") +
    theme_minimal(base_size = 10)
  
  plot_list[[model_var]] <- p
}

require(patchwork)
# Combine and print all plots
combined_plot <- wrap_plots(plot_list, ncol = 3)
print(combined_plot)


##Next plot fixed effect interaction vs average availability

# 1. ts_fires_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodwinter20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodwinter20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log(ts_fires_250_end + 1))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = ts_fires_250_end)],
  avg_data[, .(id, raw_Ma_x1 = ts_fires_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log(raw_x1 + 1)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodwinter20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log(ts_fires_250_end + 1)):I(log(ts_fires_1000_end + 1))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]

p1 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "ts_fires_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p1)

# 2. ts_harv250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodwinter20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodwinter20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log(ts_harv250_end + 1))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = ts_harv250_end)],
  avg_data[, .(id, raw_Ma_x1 = ts_harv1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log(raw_x1 + 1)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodwinter20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log(ts_harv250_end + 1)):I(log(ts_harv1000_end + 1))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]

p2 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "ts_harv1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p2)


# 3. dist_paved_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodwinter20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodwinter20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_paved_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_paved_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_paved_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodwinter20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_paved_250_end + 1)/1000)):I(log((dist_paved_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]

p3 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_paved_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p3)



# 4. dist_unpaved_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodwinter20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodwinter20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_unpaved_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_unpaved_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_unpaved_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodwinter20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_unpaved_250_end + 1)/1000)):I(log((dist_unpaved_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p4 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_unpaved_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p4)


# 5. dist_lfother_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodwinter20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodwinter20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_lfother_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_lfother_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_lfother_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodwinter20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_lfother_250_end + 1)/1000)):I(log((dist_lfother_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p5 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_lfother_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p5)


# 6. dist_otherpoly_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodwinter20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodwinter20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_otherpoly_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_otherpoly_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_otherpoly_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodwinter20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_otherpoly_250_end + 1)/1000)):I(log((dist_otherpoly_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p6 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_otherpoly_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p6)


# 7. dist_placer_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodwinter20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodwinter20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_placer_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_placer_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_placer_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodwinter20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_placer_250_end + 1)/1000)):I(log((dist_placer_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p7 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_placer_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p7)



# 8. dist_quartz_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodwinter20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodwinter20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_quartz_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_quartz_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_quartz_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodwinter20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_quartz_250_end + 1)/1000)):I(log((dist_quartz_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p8 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_quartz_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p8)



# 9. dist_othermines_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodwinter20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodwinter20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_othermines_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_othermines_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_othermines_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodwinter20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_othermines_250_end + 1)/1000)):I(log((dist_othermines_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p9 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_othermines_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p9)


# 10. prop_forest_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodwinter20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodwinter20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `prop_forest_250_end`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = prop_forest_250_end)],
  avg_data[, .(id, raw_Ma_x1 = prop_forest_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodwinter20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "prop_forest_250_end:prop_forest_1000_end"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p10 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "prop_forest_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p10)



# 11. prop_veg_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodwinter20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodwinter20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `prop_veg_250_end`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = prop_veg_250_end)],
  avg_data[, .(id, raw_Ma_x1 = prop_veg_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodwinter20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "prop_veg_250_end:prop_veg_1000_end"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p11 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "prop_veg_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p11)


plot_list <- list(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)
patchwork::wrap_plots(plot_list, ncol = 4)




## --- Roads combined ----

### ---Year-round ----
gc()
m.fullmodroadscomb10step <- glmmTMB(case_ ~ -1 +
                                      I(log((sl_+1)/1000)) +
                                      I(cos(ta_)) +
                                      I(log((sl_+1)/1000)):I(cos(ta_)) +
                                      prop_forest_250_end + prop_forest_250_end:prop_forest_1000_end +
                                      prop_veg_250_end + prop_veg_250_end:prop_veg_1000_end +
                                      I(log(ts_fires_250_end+1)) + I(log(ts_fires_250_end+1)):I(log(ts_fires_1000_end+1)) +
                                      I(log(ts_harv250_end+1)) + I(log(ts_harv250_end+1)):I(log(ts_harv1000_end+1)) +
                                      #I(log((dist_paved_250_end+1)/1000)) + I(log((dist_paved_250_end+1)/1000)):I(log((dist_paved_1000_end+1)/1000)) +
                                      #I(log((dist_unpaved_250_end+1)/1000)) + I(log((dist_unpaved_250_end+1)/1000)):I(log((dist_unpaved_1000_end+1)/1000)) +
                                      I(log(((dist_road_250_end+1)/1000))) + I(log((dist_road_250_end+1)/1000)):I(log((dist_road_1000_end+1)/1000)) +
                                      I(log((dist_lfother_250_end+1)/1000)) + I(log((dist_lfother_250_end+1)/1000)):I(log((dist_lfother_1000_end+1)/1000)) +
                                      I(log((dist_otherpoly_250_end+1)/1000)) + I(log((dist_otherpoly_250_end+1)/1000)):I(log((dist_otherpoly_1000_end+1)/1000)) +
                                      I(log((dist_placer_250_end+1)/1000)) + I(log((dist_placer_250_end+1)/1000)):I(log((dist_placer_1000_end+1)/1000)) +
                                      I(log((dist_quartz_250_end+1)/1000)) + I(log((dist_quartz_250_end+1)/1000)):I(log((dist_quartz_1000_end+1)/1000)) +
                                      I(log((dist_othermines_250_end+1)/1000)) + I(log((dist_othermines_250_end+1)/1000)):I(log((dist_othermines_1000_end+1)/1000)) +
                                      #I(log((dist_mines_250_end+1)/1000)) + I(log((dist_mines_250_end+1)/1000)):I(log((dist_mines_1000_end+1)/1000)) +
                                      #I(log((dist_otherdisturbance_250_end+1)/1000)) + I(log((dist_otherdisturbance_250_end+1)/1000)):I(log((dist_otherdisturbance_1000_end+1)/1000)) +
                                      
                                      (1|indiv_step_id) +
                                      (0 + I(log((sl_ +1)/1000))|id) +
                                      (0 + I(cos(ta_))|id) +
                                      (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                                      (0 + prop_forest_250_end|id) +
                                      (0 + prop_veg_250_end|id) +
                                      (0 + (I(log(ts_fires_250_end+1)))|id) +
                                      (0 + (I(log(ts_harv250_end+1)))|id) + 
                                      #(0 + I(log((dist_paved_250_end+1)/1000))|id) + 
                                      #(0 + I(log((dist_unpaved_250_end+1)/1000))|id) + 
                                      (0 + I(log((dist_road_250_end+1)/1000))|id) + 
                                      (0 + I(log((dist_lfother_250_end+1)/1000))|id) +
                                      (0 + I(log((dist_otherpoly_250_end+1)/1000))|id) + 
                                      (0 + I(log((dist_placer_250_end+1)/1000))|id) + 
                                      (0 + I(log((dist_quartz_250_end+1)/1000))|id) + 
                                      (0 + I(log((dist_othermines_250_end+1)/1000))|id) + 
                                      #(0 + I(log((dist_mines_250_end+1)/1000))|id) + 
                                      #(0 + I(log((dist_otherdisturbance_250_end+1)/1000))|id) + 
                                      
                                      (1|year),
                                    family = poisson(), data = dat.issa.10[,`:=`(year = as.factor(year), 
                                                                                 indiv_step_id = as.factor(indiv_step_id),
                                                                                 id = as.factor(id))],
                                    map= list(theta = factor(c(NA,1:14))),
                                    start = list(theta =c(log(1000), seq(0,0, length.out = 14))), 
                                    verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(m.fullmodroadscomb10step)
# save model output
saveRDS(m.fullmodroadscomb10step, "m.fullmodroadscomb10step.RDS")
#m.fullmodroadscomb10step <- readRDS("m.fullmodroadscomb10step.RDS")


m.fullmodroadscomb15step.indiv <- coef(m.fullmodroadscomb15step)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(m.fullmodroadscomb15step.indiv)[term!='(Intercept)']
pop <- setDT(tidy(m.fullmodroadscomb15step, effects = 'fixed'))


require(ggplot2)
ggplot(data = pop[term %like% 'dist'], aes(x =term, y = estimate)) +
  geom_point()+
  geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 
ggplot(data = pop[!(term %like% 'dist')], aes(x =term, y = estimate)) +
  geom_point()+
  geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 

ggplot(data = indivs[(term %like% 'dist')], aes(x =term, y = estimate)) + #This one
  geom_boxplot()+
  geom_jitter() +
  #geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 

ggplot(data = indivs[!(term %like% 'dist')], aes(x =term, y = estimate)) +
  geom_boxplot()+
  geom_jitter() +
  #geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip()  





###Functional response plots:
##Start with random slopes vs avg availability

dt <- as.data.table(dat.issa.15)

#Get avg availability by id
vars_1000 <- c(
  "prop_forest_1000_end",
  "prop_veg_1000_end",
  "ts_fires_1000_end",
  "ts_harv1000_end",
  "dist_road_1000_end",
  "dist_lfother_1000_end",
  "dist_otherpoly_1000_end",
  "dist_placer_1000_end",
  "dist_quartz_1000_end",
  "dist_othermines_1000_end"
)

avg_data <- dt[, lapply(.SD, mean, na.rm = TRUE), by = id, .SDcols = vars_1000]

avg_data[, `:=`(
  ts_fires_1000_end       = log(ts_fires_1000_end + 1),
  ts_harv1000_end         = log(ts_harv1000_end + 1),
  dist_road_1000_end     = log((dist_road_1000_end + 1) / 1000),
  dist_lfother_1000_end   = log((dist_lfother_1000_end + 1) / 1000),
  dist_otherpoly_1000_end = log((dist_otherpoly_1000_end + 1) / 1000),
  dist_placer_1000_end    = log((dist_placer_1000_end + 1) / 1000),
  dist_quartz_1000_end    = log((dist_quartz_1000_end + 1) / 1000),
  dist_othermines_1000_end= log((dist_othermines_1000_end + 1) / 1000)
)]

re_list <- ranef(m.fullmodroadscomb15step)$cond

var_map <- list(
  "I(log((dist_road_250_end + 1)/1000))" = "dist_road_1000_end",
  "I(log((dist_lfother_250_end + 1)/1000))" = "dist_lfother_1000_end",
  "I(log((dist_otherpoly_250_end + 1)/1000))" = "dist_otherpoly_1000_end",
  "I(log((dist_placer_250_end + 1)/1000))" = "dist_placer_1000_end",
  "I(log((dist_quartz_250_end + 1)/1000))" = "dist_quartz_1000_end",
  "I(log((dist_othermines_250_end + 1)/1000))" = "dist_othermines_1000_end",
  "prop_forest_250_end" = "prop_forest_1000_end",
  "prop_veg_250_end" = "prop_veg_1000_end",
  "I(log(ts_fires_250_end + 1))" = "ts_fires_1000_end",
  "I(log(ts_harv250_end + 1))" = "ts_harv1000_end"
)

# Convert random slopes to data.table
random_slopes <- as.data.table(re_list$id, keep.rownames = "id")

axis_labels <- list(
  "I(log((dist_road_250_end + 1)/1000))"      = list(x = "dist_road avail", y = "dist_road"),
  "I(log(ts_fires_250_end + 1))"               = list(x = "ts_fires avail", y = "ts_fires"),
  "I(log(ts_harv250_end + 1))"                 = list(x = "ts_harv avail", y = "ts_harv"),
  "prop_forest_250_end"                        = list(x = "prop_forest avail", y = "prop_forest"),
  "prop_veg_250_end"                           = list(x = "prop_veg avail", y = "prop_veg"),
  "I(log((dist_lfother_250_end + 1)/1000))"    = list(x = "dist_lfother avail", y = "dist_lfother"),
  "I(log((dist_otherpoly_250_end + 1)/1000))"  = list(x = "dist_otherpoly avail", y = "dist_otherpoly"),
  "I(log((dist_placer_250_end + 1)/1000))"     = list(x = "dist_placer avail", y = "dist_placer"),
  "I(log((dist_quartz_250_end + 1)/1000))"     = list(x = "dist_quartz avail", y = "dist_quartz"),
  "I(log((dist_othermines_250_end + 1)/1000))" = list(x = "dist_othermines avail", y = "dist_othermines")
)

plot_list <- list()

# Loop through each variable
for (model_var in names(var_map)) {
  avail_var <- var_map[[model_var]]
  
  plot_data <- merge(
    avg_data[, .(id, avail = get(avail_var))],
    random_slopes[, .(id, slope = get(model_var))],
    by = "id"
  )
  
  if (!is.null(axis_labels[[model_var]])) {
    x_lab <- axis_labels[[model_var]]$x
    y_lab <- axis_labels[[model_var]]$y
  } else {
    x_lab <- paste0("Average ", avail_var)
    y_lab <- paste0("Random slope for ", model_var)
  }
  
  if (avail_var == "ts_harv1000_end") {
    plot_data <- plot_data[avail >= 4.4]
  } else if (avail_var == "ts_fires_1000_end") {
    plot_data <- plot_data[avail >= 4.9]
  }
  
  p <- ggplot(plot_data, aes(x = avail, y = slope)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "loess", color = "darkblue", se = TRUE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", size = 0.8) +
    labs(x = x_lab, y = "selection coeff") +
    theme_minimal(base_size = 10)
  
  plot_list[[model_var]] <- p
}

require(patchwork)
# Combine and print all plots
combined_plot <- wrap_plots(plot_list, ncol = 3)
print(combined_plot)


##Next plot fixed effect interaction vs average availability

# 1. ts_fires_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodroadscomb15step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodroadscomb15step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log(ts_fires_250_end + 1))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = ts_fires_250_end)],
  avg_data[, .(id, raw_Ma_x1 = ts_fires_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log(raw_x1 + 1)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodroadscomb15step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log(ts_fires_250_end + 1)):I(log(ts_fires_1000_end + 1))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]

p1 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "ts_fires_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p1)

# 2. ts_harv250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodroadscomb15step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodroadscomb15step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log(ts_harv250_end + 1))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = ts_harv250_end)],
  avg_data[, .(id, raw_Ma_x1 = ts_harv1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log(raw_x1 + 1)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodroadscomb15step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log(ts_harv250_end + 1)):I(log(ts_harv1000_end + 1))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]

p2 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "ts_harv1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p2)


# 3. dist_road_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodroadscomb15step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodroadscomb15step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_road_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_road_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_road_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodroadscomb15step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_road_250_end + 1)/1000)):I(log((dist_road_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]

p3 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_road_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p3)



# 4. dist_lfother_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodroadscomb15step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodroadscomb15step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_lfother_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_lfother_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_lfother_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodroadscomb15step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_lfother_250_end + 1)/1000)):I(log((dist_lfother_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p4 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_lfother_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p4)


# 5. dist_otherpoly_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodroadscomb15step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodroadscomb15step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_otherpoly_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_otherpoly_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_otherpoly_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodroadscomb15step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_otherpoly_250_end + 1)/1000)):I(log((dist_otherpoly_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p5 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_otherpoly_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p5)


# 6. dist_placer_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodroadscomb15step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodroadscomb15step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_placer_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_placer_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_placer_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodroadscomb15step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_placer_250_end + 1)/1000)):I(log((dist_placer_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p6 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_placer_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p6)



# 7. dist_quartz_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodroadscomb15step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodroadscomb15step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_quartz_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_quartz_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_quartz_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodroadscomb15step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_quartz_250_end + 1)/1000)):I(log((dist_quartz_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p7 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_quartz_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p7)


# 8. dist_othermines_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodroadscomb15step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodroadscomb15step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_othermines_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_othermines_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_othermines_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodroadscomb15step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_othermines_250_end + 1)/1000)):I(log((dist_othermines_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p8 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_othermines_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p8)



# 9. prop_forest_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodroadscomb15step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodroadscomb15step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `prop_forest_250_end`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = prop_forest_250_end)],
  avg_data[, .(id, raw_Ma_x1 = prop_forest_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodroadscomb15step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "prop_forest_250_end:prop_forest_1000_end"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p9 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "prop_forest_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p9)



# 10. prop_veg_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodroadscomb15step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodroadscomb15step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `prop_veg_250_end`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = prop_veg_250_end)],
  avg_data[, .(id, raw_Ma_x1 = prop_veg_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodroadscomb15step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "prop_veg_250_end:prop_veg_1000_end"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p10 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "prop_veg_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p10)


plot_list <- list(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)
patchwork::wrap_plots(plot_list, ncol = 4)




### ---Summer ----
gc()
m.fullmodsummerroadscomb20step <- glmmTMB(case_ ~ -1 +
                                            I(log((sl_+1)/1000)) +
                                            I(cos(ta_)) +
                                            I(log((sl_+1)/1000)):I(cos(ta_)) +
                                            prop_forest_250_end + prop_forest_250_end:prop_forest_1000_end +
                                            prop_veg_250_end + prop_veg_250_end:prop_veg_1000_end +
                                            I(log(ts_fires_250_end+1)) + I(log(ts_fires_250_end+1)):I(log(ts_fires_1000_end+1)) +
                                            I(log(ts_harv250_end+1)) + I(log(ts_harv250_end+1)):I(log(ts_harv1000_end+1)) +
                                            #I(log((dist_paved_250_end+1)/1000)) + I(log((dist_paved_250_end+1)/1000)):I(log((dist_paved_1000_end+1)/1000)) +
                                            #I(log((dist_unpaved_250_end+1)/1000)) + I(log((dist_unpaved_250_end+1)/1000)):I(log((dist_unpaved_1000_end+1)/1000)) +
                                            I(log(((dist_road_250_end+1)/1000))) + I(log((dist_road_250_end+1)/1000)):I(log((dist_road_1000_end+1)/1000)) +
                                            I(log((dist_lfother_250_end+1)/1000)) + I(log((dist_lfother_250_end+1)/1000)):I(log((dist_lfother_1000_end+1)/1000)) +
                                            I(log((dist_otherpoly_250_end+1)/1000)) + I(log((dist_otherpoly_250_end+1)/1000)):I(log((dist_otherpoly_1000_end+1)/1000)) +
                                            I(log((dist_placer_250_end+1)/1000)) + I(log((dist_placer_250_end+1)/1000)):I(log((dist_placer_1000_end+1)/1000)) +
                                            I(log((dist_quartz_250_end+1)/1000)) + I(log((dist_quartz_250_end+1)/1000)):I(log((dist_quartz_1000_end+1)/1000)) +
                                            I(log((dist_othermines_250_end+1)/1000)) + I(log((dist_othermines_250_end+1)/1000)):I(log((dist_othermines_1000_end+1)/1000)) +
                                            #I(log((dist_mines_250_end+1)/1000)) + I(log((dist_mines_250_end+1)/1000)):I(log((dist_mines_1000_end+1)/1000)) +
                                            #I(log((dist_otherdisturbance_250_end+1)/1000)) + I(log((dist_otherdisturbance_250_end+1)/1000)):I(log((dist_otherdisturbance_1000_end+1)/1000)) +
                                            
                                            (1|indiv_step_id) +
                                            (0 + I(log((sl_ +1)/1000))|id) +
                                            (0 + I(cos(ta_))|id) +
                                            (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                                            (0 + prop_forest_250_end|id) +
                                            (0 + prop_veg_250_end|id) +
                                            (0 + (I(log(ts_fires_250_end+1)))|id) +
                                            (0 + (I(log(ts_harv250_end+1)))|id) + 
                                            #(0 + I(log((dist_paved_250_end+1)/1000))|id) + 
                                            #(0 + I(log((dist_unpaved_250_end+1)/1000))|id) + 
                                            (0 + I(log((dist_road_250_end+1)/1000))|id) + 
                                            (0 + I(log((dist_lfother_250_end+1)/1000))|id) +
                                            (0 + I(log((dist_otherpoly_250_end+1)/1000))|id) + 
                                            (0 + I(log((dist_placer_250_end+1)/1000))|id) + 
                                            (0 + I(log((dist_quartz_250_end+1)/1000))|id) + 
                                            (0 + I(log((dist_othermines_250_end+1)/1000))|id) + 
                                            #(0 + I(log((dist_mines_250_end+1)/1000))|id) + 
                                            #(0 + I(log((dist_otherdisturbance_250_end+1)/1000))|id) + 
                                            
                                            (1|year),
                                          family = poisson(), data = dat.issa.20.summer[,`:=`(year = as.factor(year), 
                                                                                              indiv_step_id = as.factor(indiv_step_id),
                                                                                              id = as.factor(id))],
                                          map= list(theta = factor(c(NA,1:14))),
                                          start = list(theta =c(log(1000), seq(0,0, length.out = 14))), 
                                          verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(m.fullmodsummerroadscomb20step)
# save model output
saveRDS(m.fullmodsummerroadscomb20step, "m.fullmodsummerroadscomb20step.RDS")
#m.fullmodsummerroadscomb20step <- readRDS("m.fullmodsummerroadscomb20step.RDS")


m.fullmodsummerroadscomb20step.indiv <- coef(m.fullmodsummerroadscomb20step)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(m.fullmodsummerroadscomb20step.indiv)[term!='(Intercept)']
pop <- setDT(tidy(m.fullmodsummerroadscomb20step, effects = 'fixed'))


require(ggplot2)
ggplot(data = pop[term %like% 'dist'], aes(x =term, y = estimate)) +
  geom_point()+
  geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 
ggplot(data = pop[!(term %like% 'dist')], aes(x =term, y = estimate)) +
  geom_point()+
  geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 

ggplot(data = indivs[(term %like% 'dist')], aes(x =term, y = estimate)) + #This one
  geom_boxplot()+
  geom_jitter() +
  #geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 

ggplot(data = indivs[!(term %like% 'dist')], aes(x =term, y = estimate)) +
  geom_boxplot()+
  geom_jitter() +
  #geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip()  



###Functional response plots:
##Start with random slopes vs avg availability

dt <- as.data.table(dat.issa.20.summer)

#Get avg availability by id
vars_1000 <- c(
  "prop_forest_1000_end",
  "prop_veg_1000_end",
  "ts_fires_1000_end",
  "ts_harv1000_end",
  "dist_road_1000_end",
  "dist_lfother_1000_end",
  "dist_otherpoly_1000_end",
  "dist_placer_1000_end",
  "dist_quartz_1000_end",
  "dist_othermines_1000_end"
)

avg_data <- dt[, lapply(.SD, mean, na.rm = TRUE), by = id, .SDcols = vars_1000]

avg_data[, `:=`(
  ts_fires_1000_end       = log(ts_fires_1000_end + 1),
  ts_harv1000_end         = log(ts_harv1000_end + 1),
  dist_road_1000_end     = log((dist_road_1000_end + 1) / 1000),
  dist_lfother_1000_end   = log((dist_lfother_1000_end + 1) / 1000),
  dist_otherpoly_1000_end = log((dist_otherpoly_1000_end + 1) / 1000),
  dist_placer_1000_end    = log((dist_placer_1000_end + 1) / 1000),
  dist_quartz_1000_end    = log((dist_quartz_1000_end + 1) / 1000),
  dist_othermines_1000_end= log((dist_othermines_1000_end + 1) / 1000)
)]

re_list <- ranef(m.fullmodsummerroadscomb20step)$cond

var_map <- list(
  "I(log((dist_road_250_end + 1)/1000))" = "dist_road_1000_end",
  "I(log((dist_lfother_250_end + 1)/1000))" = "dist_lfother_1000_end",
  "I(log((dist_otherpoly_250_end + 1)/1000))" = "dist_otherpoly_1000_end",
  "I(log((dist_placer_250_end + 1)/1000))" = "dist_placer_1000_end",
  "I(log((dist_quartz_250_end + 1)/1000))" = "dist_quartz_1000_end",
  "I(log((dist_othermines_250_end + 1)/1000))" = "dist_othermines_1000_end",
  "prop_forest_250_end" = "prop_forest_1000_end",
  "prop_veg_250_end" = "prop_veg_1000_end",
  "I(log(ts_fires_250_end + 1))" = "ts_fires_1000_end",
  "I(log(ts_harv250_end + 1))" = "ts_harv1000_end"
)

# Convert random slopes to data.table
random_slopes <- as.data.table(re_list$id, keep.rownames = "id")

axis_labels <- list(
  "I(log((dist_road_250_end + 1)/1000))"      = list(x = "dist_road avail", y = "dist_road"),
  "I(log(ts_fires_250_end + 1))"               = list(x = "ts_fires avail", y = "ts_fires"),
  "I(log(ts_harv250_end + 1))"                 = list(x = "ts_harv avail", y = "ts_harv"),
  "prop_forest_250_end"                        = list(x = "prop_forest avail", y = "prop_forest"),
  "prop_veg_250_end"                           = list(x = "prop_veg avail", y = "prop_veg"),
  "I(log((dist_lfother_250_end + 1)/1000))"    = list(x = "dist_lfother avail", y = "dist_lfother"),
  "I(log((dist_otherpoly_250_end + 1)/1000))"  = list(x = "dist_otherpoly avail", y = "dist_otherpoly"),
  "I(log((dist_placer_250_end + 1)/1000))"     = list(x = "dist_placer avail", y = "dist_placer"),
  "I(log((dist_quartz_250_end + 1)/1000))"     = list(x = "dist_quartz avail", y = "dist_quartz"),
  "I(log((dist_othermines_250_end + 1)/1000))" = list(x = "dist_othermines avail", y = "dist_othermines")
)

plot_list <- list()

# Loop through each variable
for (model_var in names(var_map)) {
  avail_var <- var_map[[model_var]]
  
  plot_data <- merge(
    avg_data[, .(id, avail = get(avail_var))],
    random_slopes[, .(id, slope = get(model_var))],
    by = "id"
  )
  
  if (!is.null(axis_labels[[model_var]])) {
    x_lab <- axis_labels[[model_var]]$x
    y_lab <- axis_labels[[model_var]]$y
  } else {
    x_lab <- paste0("Average ", avail_var)
    y_lab <- paste0("Random slope for ", model_var)
  }
  
  if (avail_var == "ts_harv1000_end") {
    plot_data <- plot_data[avail >= 4.3]
    #} else if (avail_var == "ts_fires_1000_end") {
    # plot_data <- plot_data[avail >= 4.9]
  }
  
  p <- ggplot(plot_data, aes(x = avail, y = slope)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "loess", color = "darkblue", se = TRUE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", size = 0.8) +
    labs(x = x_lab, y = "selection coeff") +
    theme_minimal(base_size = 10)
  
  plot_list[[model_var]] <- p
}

require(patchwork)
# Combine and print all plots
combined_plot <- wrap_plots(plot_list, ncol = 3)
print(combined_plot)


##Next plot fixed effect interaction vs average availability

# 1. ts_fires_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodsummerroadscomb20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodsummerroadscomb20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log(ts_fires_250_end + 1))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = ts_fires_250_end)],
  avg_data[, .(id, raw_Ma_x1 = ts_fires_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log(raw_x1 + 1)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodsummerroadscomb20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log(ts_fires_250_end + 1)):I(log(ts_fires_1000_end + 1))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]

p1 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "ts_fires_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p1)

# 2. ts_harv250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodsummerroadscomb20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodsummerroadscomb20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log(ts_harv250_end + 1))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = ts_harv250_end)],
  avg_data[, .(id, raw_Ma_x1 = ts_harv1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log(raw_x1 + 1)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodsummerroadscomb20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log(ts_harv250_end + 1)):I(log(ts_harv1000_end + 1))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]

p2 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "ts_harv1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p2)


# 3. dist_road_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodsummerroadscomb20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodsummerroadscomb20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_road_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_road_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_road_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodsummerroadscomb20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_road_250_end + 1)/1000)):I(log((dist_road_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]

p3 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_road_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p3)


# 4. dist_lfother_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodsummerroadscomb20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodsummerroadscomb20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_lfother_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_lfother_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_lfother_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodsummerroadscomb20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_lfother_250_end + 1)/1000)):I(log((dist_lfother_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p4 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_lfother_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p4)


# 5. dist_otherpoly_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodsummerroadscomb20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodsummerroadscomb20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_otherpoly_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_otherpoly_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_otherpoly_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodsummerroadscomb20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_otherpoly_250_end + 1)/1000)):I(log((dist_otherpoly_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p5 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_otherpoly_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p5)


# 6. dist_placer_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodsummerroadscomb20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodsummerroadscomb20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_placer_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_placer_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_placer_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodsummerroadscomb20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_placer_250_end + 1)/1000)):I(log((dist_placer_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p6 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_placer_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p6)


# 7. dist_quartz_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodsummerroadscomb20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodsummerroadscomb20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_quartz_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_quartz_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_quartz_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodsummerroadscomb20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_quartz_250_end + 1)/1000)):I(log((dist_quartz_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p7 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_quartz_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p7)


# 8. dist_othermines_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodsummerroadscomb20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodsummerroadscomb20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_othermines_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_othermines_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_othermines_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodsummerroadscomb20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_othermines_250_end + 1)/1000)):I(log((dist_othermines_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p8 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_othermines_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p8)


# 9. prop_forest_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodsummerroadscomb20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodsummerroadscomb20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `prop_forest_250_end`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = prop_forest_250_end)],
  avg_data[, .(id, raw_Ma_x1 = prop_forest_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodsummerroadscomb20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "prop_forest_250_end:prop_forest_1000_end"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p9 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "prop_forest_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p9)



# 10. prop_veg_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodsummerroadscomb20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodsummerroadscomb20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `prop_veg_250_end`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = prop_veg_250_end)],
  avg_data[, .(id, raw_Ma_x1 = prop_veg_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodsummerroadscomb20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "prop_veg_250_end:prop_veg_1000_end"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p10 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "prop_veg_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p10)


plot_list <- list(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)
patchwork::wrap_plots(plot_list, ncol = 4)



### ---Winter ----
gc()
m.fullmodwinterroadscomb20step <- glmmTMB(case_ ~ -1 +
                                            I(log((sl_+1)/1000)) +
                                            I(cos(ta_)) +
                                            I(log((sl_+1)/1000)):I(cos(ta_)) +
                                            prop_forest_250_end + prop_forest_250_end:prop_forest_1000_end +
                                            prop_veg_250_end + prop_veg_250_end:prop_veg_1000_end +
                                            I(log(ts_fires_250_end+1)) + I(log(ts_fires_250_end+1)):I(log(ts_fires_1000_end+1)) +
                                            I(log(ts_harv250_end+1)) + I(log(ts_harv250_end+1)):I(log(ts_harv1000_end+1)) +
                                            #I(log((dist_paved_250_end+1)/1000)) + I(log((dist_paved_250_end+1)/1000)):I(log((dist_paved_1000_end+1)/1000)) +
                                            #I(log((dist_unpaved_250_end+1)/1000)) + I(log((dist_unpaved_250_end+1)/1000)):I(log((dist_unpaved_1000_end+1)/1000)) +
                                            I(log(((dist_road_250_end+1)/1000))) + I(log((dist_road_250_end+1)/1000)):I(log((dist_road_1000_end+1)/1000)) +
                                            I(log((dist_lfother_250_end+1)/1000)) + I(log((dist_lfother_250_end+1)/1000)):I(log((dist_lfother_1000_end+1)/1000)) +
                                            I(log((dist_otherpoly_250_end+1)/1000)) + I(log((dist_otherpoly_250_end+1)/1000)):I(log((dist_otherpoly_1000_end+1)/1000)) +
                                            I(log((dist_placer_250_end+1)/1000)) + I(log((dist_placer_250_end+1)/1000)):I(log((dist_placer_1000_end+1)/1000)) +
                                            I(log((dist_quartz_250_end+1)/1000)) + I(log((dist_quartz_250_end+1)/1000)):I(log((dist_quartz_1000_end+1)/1000)) +
                                            I(log((dist_othermines_250_end+1)/1000)) + I(log((dist_othermines_250_end+1)/1000)):I(log((dist_othermines_1000_end+1)/1000)) +
                                            #I(log((dist_mines_250_end+1)/1000)) + I(log((dist_mines_250_end+1)/1000)):I(log((dist_mines_1000_end+1)/1000)) +
                                            #I(log((dist_otherdisturbance_250_end+1)/1000)) + I(log((dist_otherdisturbance_250_end+1)/1000)):I(log((dist_otherdisturbance_1000_end+1)/1000)) +
                                            
                                            (1|indiv_step_id) +
                                            (0 + I(log((sl_ +1)/1000))|id) +
                                            (0 + I(cos(ta_))|id) +
                                            (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                                            (0 + prop_forest_250_end|id) +
                                            (0 + prop_veg_250_end|id) +
                                            (0 + (I(log(ts_fires_250_end+1)))|id) +
                                            (0 + (I(log(ts_harv250_end+1)))|id) + 
                                            #(0 + I(log((dist_paved_250_end+1)/1000))|id) + 
                                            #(0 + I(log((dist_unpaved_250_end+1)/1000))|id) + 
                                            (0 + I(log((dist_road_250_end+1)/1000))|id) + 
                                            (0 + I(log((dist_lfother_250_end+1)/1000))|id) +
                                            (0 + I(log((dist_otherpoly_250_end+1)/1000))|id) + 
                                            (0 + I(log((dist_placer_250_end+1)/1000))|id) + 
                                            (0 + I(log((dist_quartz_250_end+1)/1000))|id) + 
                                            (0 + I(log((dist_othermines_250_end+1)/1000))|id) + 
                                            #(0 + I(log((dist_mines_250_end+1)/1000))|id) + 
                                            #(0 + I(log((dist_otherdisturbance_250_end+1)/1000))|id) + 
                                            
                                            (1|year),
                                          family = poisson(), data = dat.issa.20.winter[,`:=`(year = as.factor(year), 
                                                                                              indiv_step_id = as.factor(indiv_step_id),
                                                                                              id = as.factor(id))],
                                          map= list(theta = factor(c(NA,1:14))),
                                          start = list(theta =c(log(1000), seq(0,0, length.out = 14))), 
                                          verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(m.fullmodwinterroadscomb20step)
# save model output
saveRDS(m.fullmodwinterroadscomb20step, "m.fullmodwinterroadscomb20step.RDS")
#m.fullmodwinterroadscomb20step <- readRDS("m.fullmodwinterroadscomb20step.RDS")


m.fullmodwinterroadscomb10step.indiv <- coef(m.fullmodwinterroadscomb10step)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(m.fullmodwinterroadscomb10step.indiv)[term!='(Intercept)']
pop <- setDT(tidy(m.fullmodwinterroadscomb10step, effects = 'fixed'))


require(ggplot2)
ggplot(data = pop[term %like% 'dist'], aes(x =term, y = estimate)) +
  geom_point()+
  geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 
ggplot(data = pop[!(term %like% 'dist')], aes(x =term, y = estimate)) +
  geom_point()+
  geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 

ggplot(data = indivs[(term %like% 'dist')], aes(x =term, y = estimate)) + #This one
  geom_boxplot()+
  geom_jitter() +
  #geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip() 

ggplot(data = indivs[!(term %like% 'dist')], aes(x =term, y = estimate)) +
  geom_boxplot()+
  geom_jitter() +
  #geom_errorbar(aes(ymin = estimate - (1.96*std.error), ymax = estimate + (1.96*std.error))) +
  geom_hline(aes(yintercept = 0), lty = 'dashed') +
  coord_flip()  



###Functional response plots:
##Start with random slopes vs avg availability

dt <- as.data.table(dat.issa.10.winter)

#Get avg availability by id
vars_1000 <- c(
  "prop_forest_1000_end",
  "prop_veg_1000_end",
  "ts_fires_1000_end",
  "ts_harv1000_end",
  "dist_road_1000_end",
  "dist_lfother_1000_end",
  "dist_otherpoly_1000_end",
  "dist_placer_1000_end",
  "dist_quartz_1000_end",
  "dist_othermines_1000_end"
)

avg_data <- dt[, lapply(.SD, mean, na.rm = TRUE), by = id, .SDcols = vars_1000]

avg_data[, `:=`(
  ts_fires_1000_end       = log(ts_fires_1000_end + 1),
  ts_harv1000_end         = log(ts_harv1000_end + 1),
  dist_road_1000_end     = log((dist_road_1000_end + 1) / 1000),
  dist_lfother_1000_end   = log((dist_lfother_1000_end + 1) / 1000),
  dist_otherpoly_1000_end = log((dist_otherpoly_1000_end + 1) / 1000),
  dist_placer_1000_end    = log((dist_placer_1000_end + 1) / 1000),
  dist_quartz_1000_end    = log((dist_quartz_1000_end + 1) / 1000),
  dist_othermines_1000_end= log((dist_othermines_1000_end + 1) / 1000)
)]

re_list <- ranef(m.fullmodwinterroadscomb10step)$cond

var_map <- list(
  "I(log((dist_road_250_end + 1)/1000))" = "dist_road_1000_end",
  "I(log((dist_lfother_250_end + 1)/1000))" = "dist_lfother_1000_end",
  "I(log((dist_otherpoly_250_end + 1)/1000))" = "dist_otherpoly_1000_end",
  "I(log((dist_placer_250_end + 1)/1000))" = "dist_placer_1000_end",
  "I(log((dist_quartz_250_end + 1)/1000))" = "dist_quartz_1000_end",
  "I(log((dist_othermines_250_end + 1)/1000))" = "dist_othermines_1000_end",
  "prop_forest_250_end" = "prop_forest_1000_end",
  "prop_veg_250_end" = "prop_veg_1000_end",
  "I(log(ts_fires_250_end + 1))" = "ts_fires_1000_end",
  "I(log(ts_harv250_end + 1))" = "ts_harv1000_end"
)

# Convert random slopes to data.table
random_slopes <- as.data.table(re_list$id, keep.rownames = "id")

axis_labels <- list(
  "I(log((dist_road_250_end + 1)/1000))"      = list(x = "dist_road avail", y = "dist_road"),
  "I(log(ts_fires_250_end + 1))"               = list(x = "ts_fires avail", y = "ts_fires"),
  "I(log(ts_harv250_end + 1))"                 = list(x = "ts_harv avail", y = "ts_harv"),
  "prop_forest_250_end"                        = list(x = "prop_forest avail", y = "prop_forest"),
  "prop_veg_250_end"                           = list(x = "prop_veg avail", y = "prop_veg"),
  "I(log((dist_lfother_250_end + 1)/1000))"    = list(x = "dist_lfother avail", y = "dist_lfother"),
  "I(log((dist_otherpoly_250_end + 1)/1000))"  = list(x = "dist_otherpoly avail", y = "dist_otherpoly"),
  "I(log((dist_placer_250_end + 1)/1000))"     = list(x = "dist_placer avail", y = "dist_placer"),
  "I(log((dist_quartz_250_end + 1)/1000))"     = list(x = "dist_quartz avail", y = "dist_quartz"),
  "I(log((dist_othermines_250_end + 1)/1000))" = list(x = "dist_othermines avail", y = "dist_othermines")
)

plot_list <- list()

# Loop through each variable
for (model_var in names(var_map)) {
  avail_var <- var_map[[model_var]]
  
  plot_data <- merge(
    avg_data[, .(id, avail = get(avail_var))],
    random_slopes[, .(id, slope = get(model_var))],
    by = "id"
  )
  
  if (!is.null(axis_labels[[model_var]])) {
    x_lab <- axis_labels[[model_var]]$x
    y_lab <- axis_labels[[model_var]]$y
  } else {
    x_lab <- paste0("Average ", avail_var)
    y_lab <- paste0("Random slope for ", model_var)
  }
  
  #if (avail_var == "ts_harv1000_end") {
  #plot_data <- plot_data[avail >= 4.3]
  #} else if (avail_var == "ts_fires_1000_end") {
  # plot_data <- plot_data[avail >= 4.9]
  #}
  require(ggplot2)
  p <- ggplot(plot_data, aes(x = avail, y = slope)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "loess", color = "darkblue", se = TRUE) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", size = 0.8) +
    labs(x = x_lab, y = "selection coeff") +
    theme_minimal(base_size = 10)
  
  plot_list[[model_var]] <- p
}

require(patchwork)
# Combine and print all plots
combined_plot <- wrap_plots(plot_list, ncol = 3)
print(combined_plot)


##Next plot fixed effect interaction vs average availability

# 1. ts_fires_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodwinterroadscomb20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodwinterroadscomb20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log(ts_fires_250_end + 1))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = ts_fires_250_end)],
  avg_data[, .(id, raw_Ma_x1 = ts_fires_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log(raw_x1 + 1)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodwinterroadscomb20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log(ts_fires_250_end + 1)):I(log(ts_fires_1000_end + 1))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]

p1 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "ts_fires_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p1)

# 2. ts_harv250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodwinterroadscomb20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodwinterroadscomb20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log(ts_harv250_end + 1))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = ts_harv250_end)],
  avg_data[, .(id, raw_Ma_x1 = ts_harv1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log(raw_x1 + 1)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodwinterroadscomb20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log(ts_harv250_end + 1)):I(log(ts_harv1000_end + 1))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]

p2 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "ts_harv1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p2)


# 3. dist_road_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodwinterroadscomb20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodwinterroadscomb20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_road_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_road_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_road_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodwinterroadscomb20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_road_250_end + 1)/1000)):I(log((dist_road_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]

p3 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_road_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p3)



# 4. dist_lfother_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodwinterroadscomb20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodwinterroadscomb20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_lfother_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_lfother_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_lfother_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodwinterroadscomb20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_lfother_250_end + 1)/1000)):I(log((dist_lfother_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p4 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_lfother_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p4)


# 5. dist_otherpoly_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodwinterroadscomb20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodwinterroadscomb20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_otherpoly_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_otherpoly_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_otherpoly_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodwinterroadscomb20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_otherpoly_250_end + 1)/1000)):I(log((dist_otherpoly_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p5 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_otherpoly_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p5)


# 6. dist_placer_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodwinterroadscomb20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodwinterroadscomb20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_placer_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_placer_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_placer_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodwinterroadscomb20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_placer_250_end + 1)/1000)):I(log((dist_placer_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p6 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_placer_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p6)



# 7. dist_quartz_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodwinterroadscomb20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodwinterroadscomb20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_quartz_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_quartz_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_quartz_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodwinterroadscomb20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_quartz_250_end + 1)/1000)):I(log((dist_quartz_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p7 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_quartz_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p7)



# 8. dist_othermines_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodwinterroadscomb20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodwinterroadscomb20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `I(log((dist_othermines_250_end + 1)/1000))`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = dist_othermines_250_end)],
  avg_data[, .(id, raw_Ma_x1 = dist_othermines_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  raw_x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

dt_full[, x1 := log((raw_x1 + 1)/1000)]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodwinterroadscomb20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "I(log((dist_othermines_250_end + 1)/1000)):I(log((dist_othermines_1000_end + 1)/1000))"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p8 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "dist_othermines_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p8)



# 9. prop_forest_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodwinterroadscomb20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodwinterroadscomb20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `prop_forest_250_end`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = prop_forest_250_end)],
  avg_data[, .(id, raw_Ma_x1 = prop_forest_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodwinterroadscomb20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "prop_forest_250_end:prop_forest_1000_end"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p9 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "prop_forest_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p9)




# 10. prop_veg_250_end
# Extract random slopes
re_id <- as.data.table(ranef(m.fullmodwinterroadscomb20step)$cond$id)
re_id$id <- rownames(ranef(m.fullmodwinterroadscomb20step)$cond$id)
re_slope <- re_id[, .(id, rand_beta1 = `prop_veg_250_end`)]

# Merge raw and avg data by id
dt_full <- merge(
  dt[, .(id, raw_x1 = prop_veg_250_end)],
  avg_data[, .(id, raw_Ma_x1 = prop_veg_1000_end)],
  by = "id"
)
dt_full <- merge(dt_full, re_slope, by = "id", all.x = TRUE)

# Aggregate to one row per id
dt_full <- dt_full[, .(
  x1 = mean(raw_x1, na.rm = TRUE),
  Ma_x1 = mean(raw_Ma_x1, na.rm = TRUE),
  beta1 = unique(rand_beta1)
), by = id]

# Get theta (interaction fixed effect)
coef_df <- as.data.frame(coef(summary(m.fullmodwinterroadscomb20step))$cond)
coef_df$term <- rownames(coef_df)
theta <- coef_df$Estimate[coef_df$term == "prop_veg_250_end:prop_veg_1000_end"]

# Calculate w_x using random slopes + fixed interaction term
dt_full[, w_x := exp((beta1 * x1) + (theta * x1 * Ma_x1))]
dt_full <- dt_full[w_x <= 100]

p10 <- ggplot(dt_full, aes(x = Ma_x1, y = w_x)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(
    method = "loess", 
    se = TRUE, 
    color = "darkblue", 
    linewidth = 1.25
  ) +
  labs(x = "prop_veg_1000_end avail", y = "w(x)") +
  theme_minimal(base_size = 10)

print(p10)

plot_list <- list(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)
patchwork::wrap_plots(plot_list, ncol = 4)






##---- Simplified model ----

### ---- Year-round ----
gc()
m.simple15step <- glmmTMB(case_ ~ -1 +
                            I(log((sl_+1)/1000)) +
                            I(cos(ta_)) +
                            I(log((sl_+1)/1000)):I(cos(ta_)) +
                            prop_forest_250_end + prop_forest_250_end:prop_forest_1000_end +
                            prop_veg_250_end + prop_veg_250_end:prop_veg_1000_end +
                            I(log(ts_fires_250_end+1)) + I(log(ts_fires_250_end+1)):I(log(ts_fires_1000_end+1)) +
                            I(log(ts_harv250_end+1)) + I(log(ts_harv250_end+1)):I(log(ts_harv1000_end+1)) +
                            I(log((dist_lf_250_end+1)/1000)) + I(log((dist_lf_250_end+1)/1000)):I(log((dist_lf_1000_end+1)/1000)) +
                            I(log((dist_poly_250_end+1)/1000)) + I(log((dist_poly_250_end+1)/1000)):I(log((dist_poly_1000_end+1)/1000)) +
                            
                            
                            (1|indiv_step_id) +
                            (0 + I(log((sl_ +1)/1000))|id) +
                            (0 + I(cos(ta_))|id) +
                            (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                            (0 + prop_forest_250_end|id) +
                            (0 + prop_veg_250_end|id) +
                            (0 + (I(log(ts_fires_250_end+1)))|id) +
                            (0 + (I(log(ts_harv250_end+1)))|id) + 
                            (0 + I(log((dist_lf_250_end+1)/1000))|id) + 
                            (0 + I(log((dist_poly_250_end+1)/1000))|id) + 
                            
                            
                            (1|year),
                          family = poisson(), data = dat.issa.15[,`:=`(year = as.factor(year), 
                                                                       indiv_step_id = as.factor(indiv_step_id),
                                                                       id = as.factor(id))],
                          map= list(theta = factor(c(NA,1:10))),
                          start = list(theta =c(log(1000), seq(0,0, length.out = 10))), 
                          verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(m.simple15step)
# save model output
saveRDS(m.simple15step, "m.simple15step.RDS")
#m.simple15step <- readRDS("m.simple15step.RDS")


m.simple.indiv <- coef(m.simple)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(m.simple.indiv)[term!='(Intercept)']
pop <- setDT(tidy(m.simple, effects = 'fixed'))
indivs <- indivs[!term %like% ":"]


ggplot() +
  geom_jitter(data = indivs[term %like% "dist"], aes(x = term, y = estimate),
              width = 0.15, height = 0, alpha = 0.2, color = "#9467bd") +
  geom_point(data = pop[term %like% "dist"], aes(x = term, y = estimate),
             color = "black", size = 2.5) +
  geom_errorbar(data = pop[term %like% "dist"],
                aes(x = term,
                    ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(x = "", y = "Estimate") 


ggplot() +
  geom_jitter(data = indivs[!(term %like% "dist")], aes(x = term, y = estimate),
              width = 0.15, height = 0, alpha = 0.2, color = "#9467bd") +
  geom_point(data = pop[!(term %like% "dist")], aes(x = term, y = estimate),
             color = "black", size = 2.5) +
  geom_errorbar(data = pop[!(term %like% "dist")],
                aes(x = term,
                    ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(x = "", y = "Estimate") 

### ---- Summer ----
gc()
m.simplesum20step <- glmmTMB(case_ ~ -1 +
                               I(log((sl_+1)/1000)) +
                               I(cos(ta_)) +
                               I(log((sl_+1)/1000)):I(cos(ta_)) +
                               prop_forest_250_end + prop_forest_250_end:prop_forest_1000_end +
                               prop_veg_250_end + prop_veg_250_end:prop_veg_1000_end +
                               I(log(ts_fires_250_end+1)) + I(log(ts_fires_250_end+1)):I(log(ts_fires_1000_end+1)) +
                               I(log(ts_harv250_end+1)) + I(log(ts_harv250_end+1)):I(log(ts_harv1000_end+1)) +
                               I(log((dist_lf_250_end+1)/1000)) + I(log((dist_lf_250_end+1)/1000)):I(log((dist_lf_1000_end+1)/1000)) +
                               I(log((dist_poly_250_end+1)/1000)) + I(log((dist_poly_250_end+1)/1000)):I(log((dist_poly_1000_end+1)/1000)) +
                               
                               
                               (1|indiv_step_id) +
                               (0 + I(log((sl_ +1)/1000))|id) +
                               (0 + I(cos(ta_))|id) +
                               (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                               (0 + prop_forest_250_end|id) +
                               (0 + prop_veg_250_end|id) +
                               (0 + (I(log(ts_fires_250_end+1)))|id) +
                               (0 + (I(log(ts_harv250_end+1)))|id) + 
                               (0 + I(log((dist_lf_250_end+1)/1000))|id) + 
                               (0 + I(log((dist_poly_250_end+1)/1000))|id) + 
                               
                               
                               (1|year),
                             family = poisson(), data = dat.issa.20.summer[,`:=`(year = as.factor(year), 
                                                                                 indiv_step_id = as.factor(indiv_step_id),
                                                                                 id = as.factor(id))],
                             map= list(theta = factor(c(NA,1:10))),
                             start = list(theta =c(log(1000), seq(0,0, length.out = 10))), 
                             verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(m.simplesum20step)
# save model output
saveRDS(m.simplesum20step, "m.simplesum20step.RDS")
#m.simplesum20step <- readRDS("m.simplesum20step.RDS")


m.simplesum.indiv <- coef(m.simplesum)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(m.simplesum.indiv)[term!='(Intercept)']
pop <- setDT(tidy(m.simplesum, effects = 'fixed'))
indivs <- indivs[!term %like% ":"]


ggplot() +
  geom_jitter(data = indivs[term %like% "dist"], aes(x = term, y = estimate),
              width = 0.15, height = 0, alpha = 0.2, color = "#9467bd") +
  geom_point(data = pop[term %like% "dist"], aes(x = term, y = estimate),
             color = "black", size = 2.5) +
  geom_errorbar(data = pop[term %like% "dist"],
                aes(x = term,
                    ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(x = "", y = "Estimate") 


ggplot() +
  geom_jitter(data = indivs[!(term %like% "dist")], aes(x = term, y = estimate),
              width = 0.15, height = 0, alpha = 0.2, color = "#9467bd") +
  geom_point(data = pop[!(term %like% "dist")], aes(x = term, y = estimate),
             color = "black", size = 2.5) +
  geom_errorbar(data = pop[!(term %like% "dist")],
                aes(x = term,
                    ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(x = "", y = "Estimate") 

### ---- Winter ----
gc()
m.simplewin15step <- glmmTMB(case_ ~ -1 +
                               I(log((sl_+1)/1000)) +
                               I(cos(ta_)) +
                               I(log((sl_+1)/1000)):I(cos(ta_)) +
                               prop_forest_250_end + prop_forest_250_end:prop_forest_1000_end +
                               prop_veg_250_end + prop_veg_250_end:prop_veg_1000_end +
                               I(log(ts_fires_250_end+1)) + I(log(ts_fires_250_end+1)):I(log(ts_fires_1000_end+1)) +
                               I(log(ts_harv250_end+1)) + I(log(ts_harv250_end+1)):I(log(ts_harv1000_end+1)) +
                               I(log((dist_lf_250_end+1)/1000)) + I(log((dist_lf_250_end+1)/1000)):I(log((dist_lf_1000_end+1)/1000)) +
                               I(log((dist_poly_250_end+1)/1000)) + I(log((dist_poly_250_end+1)/1000)):I(log((dist_poly_1000_end+1)/1000)) +
                               
                               
                               (1|indiv_step_id) +
                               (0 + I(log((sl_ +1)/1000))|id) +
                               (0 + I(cos(ta_))|id) +
                               (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                               (0 + prop_forest_250_end|id) +
                               (0 + prop_veg_250_end|id) +
                               (0 + (I(log(ts_fires_250_end+1)))|id) +
                               (0 + (I(log(ts_harv250_end+1)))|id) + 
                               (0 + I(log((dist_lf_250_end+1)/1000))|id) + 
                               (0 + I(log((dist_poly_250_end+1)/1000))|id) + 
                               
                               
                               (1|year),
                             family = poisson(), data = dat.issa.15.winter[,`:=`(year = as.factor(year), 
                                                                                 indiv_step_id = as.factor(indiv_step_id),
                                                                                 id = as.factor(id))],
                             map= list(theta = factor(c(NA,1:10))),
                             start = list(theta =c(log(1000), seq(0,0, length.out = 10))), 
                             verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(m.simplewin15step)
# save model output
saveRDS(m.simplewin15step, "m.simplewin15step.RDS")
#m.simplewin15step <- readRDS("m.simplewin15step.RDS")


m.simplewin.indiv <- coef(m.simplewin)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(m.simplewin.indiv)[term!='(Intercept)']
pop <- setDT(tidy(m.simplewin, effects = 'fixed'))
indivs <- indivs[!term %like% ":"]


ggplot() +
  geom_jitter(data = indivs[term %like% "dist"], aes(x = term, y = estimate),
              width = 0.15, height = 0, alpha = 0.2, color = "#9467bd") +
  geom_point(data = pop[term %like% "dist"], aes(x = term, y = estimate),
             color = "black", size = 2.5) +
  geom_errorbar(data = pop[term %like% "dist"],
                aes(x = term,
                    ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(x = "", y = "Estimate") 


ggplot() +
  geom_jitter(data = indivs[!(term %like% "dist")], aes(x = term, y = estimate),
              width = 0.15, height = 0, alpha = 0.2, color = "#9467bd") +
  geom_point(data = pop[!(term %like% "dist")], aes(x = term, y = estimate),
             color = "black", size = 2.5) +
  geom_errorbar(data = pop[!(term %like% "dist")],
                aes(x = term,
                    ymin = estimate - 1.96 * std.error,
                    ymax = estimate + 1.96 * std.error),
                width = 0.2, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(x = "", y = "Estimate") 
