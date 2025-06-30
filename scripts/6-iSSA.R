# === iSSA -------------------------------------
# Julie Turner
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

summary(dat$ts_fires_250_end) #Mostly 200 year old fires, looks good
hist(dat$ts_fires_250_end)

#dat[ts_fires_end > 10000, ts_fires_end:= 25] # setting this to 20 for now because most were in 1990-2000, but I'll need to figure out something later
head(dat)

dat[,.(dist_to_paved = mean(dist_paved_250_end, na.rm = T), dist_to_unpaved = mean(dist_unpaved_250_end, na.rm = T),
       dist_to_lfother = mean(dist_lfother_250_end, na.rm = T), dist_to_anthropolys = mean(dist_otherpoly_250_end, na.rm =T),
       dist_to_harv = mean(harv_dist_250_end, na.rm = T), dist_to_othermines = mean(dist_othermines_250_end, na.rm = T))]

dat[prov == 'yt', .(dist_to_placer = mean(dist_placer_250_end, na.rm = T), dist_to_quartz = mean(dist_quartz_250_end, na.rm = T))]

dat[,.(dist_to_paved = min(dist_paved_250_end, na.rm = T), dist_to_unpaved = min(dist_unpaved_250_end, na.rm = T),
       dist_to_lfother = min(dist_lfother_250_end, na.rm = T), dist_to_anthropolys = min(dist_otherpoly_250_end, na.rm =T),
       dist_to_harv = min(harv_dist_250_end, na.rm = T), dist_to_othermines = min(dist_othermines_250_end, na.rm = T))]

dat[prov == 'yt', .(dist_to_placer = min(dist_placer_250_end, na.rm = T), dist_to_quartz = min(dist_quartz_250_end, na.rm = T))]


dat[,.(water = mean(prop_water_250_end, na.rm = T), snow = mean(prop_snow_250_end, na.rm = T),
       rock = mean(prop_rock_250_end, na.rm = T), barren = mean(prop_barren_250_end, na.rm = T),
       bryoids = mean(prop_bryoids_250_end, na.rm = T), shrub = mean(prop_shrub_250_end, na.rm = T),
       wetland = mean(prop_wetland_250_end, na.rm = T), wettreed = mean(prop_wettreed_250_end, na.rm = T), 
       herbs = mean(prop_herbs_250_end, na.rm = T), needle =mean(prop_needleleaf_250_end, na.rm = T),
       decid = mean(prop_deciduous_250_end, na.rm = T),mixed = mean(prop_mixed_250_end, na.rm = T),
       for.age = mean(for_age_250_end, na.rm = T))]

quantile(dat$ts_fires_250_end, probs = c(0.01, 0.05, 0.1, 0.15, 0.25, 0.5, 0.75, 1))

#combining landcover types
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


dat.issa <- dat[,.(id, area, prov, x1_, y1_, x2_, y2_, sl_, ta_, burst_, t1_, t2_, dt_,
                   case_, step_id_, indiv_step_id, year, 
                   prop_veg_250_start, prop_veg_250_end,
                   prop_forest_250_start, prop_forest_250_end,
                   prop_needleleaf_250_start, prop_needleleaf_250_end,
                   for_age_250_start, for_age_250_end,
                   dist_paved_250_start, dist_paved_250_end, dist_unpaved_250_start, dist_unpaved_250_end, 
                   dist_road_250_end = pmin(dist_paved_250_end, dist_unpaved_250_end, na.rm = T),
                   dist_lfother_250_start, dist_lfother_250_end,
                   dist_lf_250_end = pmin(dist_paved_250_end, dist_unpaved_250_end, dist_lfother_250_end, na.rm = T),
                   dist_otherpoly_250_start, dist_otherpoly_250_end, dist_mines_250_start, dist_mines_250_end,
                   dist_placer_250_start, dist_placer_250_end, dist_quartz_250_start, dist_quartz_250_end,
                   dist_othermines_250_start, dist_othermines_250_end,
                   harv_dist_250_start, harv_dist_250_end, 
                   dist_otherdisturbance_250_start = pmin(dist_paved_250_start, dist_unpaved_250_start, dist_lfother_250_start, dist_otherpoly_250_start,
                                                          harv_dist_250_start, na.rm = T), 
                   dist_otherdisturbance_250_end = pmin(dist_paved_250_end, dist_unpaved_250_end, dist_lfother_250_end, dist_otherpoly_250_end,
                                                        harv_dist_250_end, na.rm = T), 
                   dist_anthrodisturbance_250_start = pmin(dist_paved_250_start, dist_unpaved_250_start, dist_lfother_250_start, dist_otherpoly_250_start,
                                                           harv_dist_250_start, dist_placer_250_start, dist_quartz_250_start, na.rm = T), 
                   dist_anthrodisturbance_250_end = pmin(dist_paved_250_end, dist_unpaved_250_end, dist_lfother_250_end, dist_otherpoly_250_end,
                                                         harv_dist_250_end, dist_placer_250_end, dist_quartz_250_end, na.rm = T),
                   for_age_250_start, for_age_250_end,
                   ts_harv250_start, ts_harv250_end, ts_fires_250_start, ts_fires_250_end,
                #1000
                   prop_veg_1000_start, prop_veg_1000_end,
                   prop_forest_1000_start, prop_forest_1000_end,
                   prop_needleleaf_1000_start, prop_needleleaf_1000_end,
                   for_age_1000_start, for_age_1000_end,
                   dist_paved_1000_start, dist_paved_1000_end, dist_unpaved_1000_start, dist_unpaved_1000_end, 
                   dist_road_1000_end = pmin(dist_paved_1000_end, dist_unpaved_1000_end, na.rm = T),
                   dist_lfother_1000_start, dist_lfother_1000_end,
                   dist_lf_1000_end = pmin(dist_paved_1000_end, dist_unpaved_1000_end, dist_lfother_1000_end, na.rm = T),
                   dist_otherpoly_1000_start, dist_otherpoly_1000_end, dist_mines_1000_start, dist_mines_1000_end,
                   dist_placer_1000_start, dist_placer_1000_end, dist_quartz_1000_start, dist_quartz_1000_end,
                   dist_othermines_1000_start, dist_othermines_1000_end,
                   harv_dist_1000_start, harv_dist_1000_end, 
                   dist_otherdisturbance_1000_start = pmin(dist_paved_1000_start, dist_unpaved_1000_start, dist_lfother_1000_start, dist_otherpoly_1000_start,
                                                           harv_dist_1000_start, na.rm = T), 
                   dist_otherdisturbance_1000_end = pmin(dist_paved_1000_end, dist_unpaved_1000_end, dist_lfother_1000_end, dist_otherpoly_1000_end,
                                                         harv_dist_1000_end, na.rm = T), 
                   dist_anthrodisturbance_1000_start = pmin(dist_paved_1000_start, dist_unpaved_1000_start, dist_lfother_1000_start, dist_otherpoly_1000_start,
                                                            harv_dist_1000_start, dist_placer_1000_start, dist_quartz_1000_start, na.rm = T), 
                   dist_anthrodisturbance_1000_end = pmin(dist_paved_1000_end, dist_unpaved_1000_end, dist_lfother_1000_end, dist_otherpoly_1000_end,
                                                          harv_dist_1000_end, dist_placer_1000_end, dist_quartz_1000_end, na.rm = T),
                   for_age_1000_start, for_age_1000_end,
                   ts_harv1000_start, ts_harv1000_end, ts_fires_1000_start, ts_fires_1000_end)]

sumdat <- dat.issa[,.(prov, year, prop_veg_250_start, prop_veg_250_end,
            for_age_250_start, for_age_250_end, prop_forest_250_start, prop_forest_250_end,
            dist_paved_250_start, dist_paved_250_end, dist_unpaved_250_start, dist_unpaved_250_end, 
            dist_road_250_end, dist_lfother_250_start, dist_lfother_250_end, dist_lf_250_end,
            dist_otherpoly_250_start, dist_otherpoly_250_end, dist_mines_250_start, dist_mines_250_end,
            dist_placer_250_start, dist_placer_250_end, dist_quartz_250_start, dist_quartz_250_end,
            harv_dist_250_start, harv_dist_250_end,
            dist_otherdisturbance_250_start, dist_otherdisturbance_250_end,
            dist_anthrodisturbance_250_start, dist_anthrodisturbance_250_end,
            ts_harv250_start, ts_harv250_end, ts_fires_250_start, ts_fires_250_end,
            
            for_age_1000_start, for_age_1000_end, prop_forest_1000_start, prop_forest_1000_end,
            dist_paved_1000_start, dist_paved_1000_end, dist_unpaved_1000_start, dist_unpaved_1000_end, 
            dist_road_1000_end, dist_lfother_1000_start, dist_lfother_1000_end, dist_lf_1000_end,
            dist_otherpoly_1000_start, dist_otherpoly_1000_end, dist_mines_1000_start, dist_mines_1000_end,
            dist_placer_1000_start, dist_placer_1000_end, dist_quartz_1000_start, dist_quartz_1000_end,
            harv_dist_1000_start, harv_dist_1000_end,
            dist_otherdisturbance_1000_start, dist_otherdisturbance_1000_end,
            dist_anthrodisturbance_1000_start, dist_anthrodisturbance_1000_end,
            ts_harv1000_start, ts_harv1000_end, ts_fires_1000_start, ts_fires_1000_end)] %>%
  group_by(prov) %>%
  summarise_all(mean, na.rm = T)

dat.issa[case_ ==TRUE,mean(prop_veg_250_end, na.rm = T), by = .(prov, year)]
dat.issa[case_ ==TRUE,mean(prop_forest_250_end, na.rm = T), by = .(prov, year)]
dat.issa[case_ ==TRUE,mean(prop_needleleaf_250_end, na.rm = T), by = .(prov, year)]
dat.issa[case_ ==TRUE,mean(for_age_250_end, na.rm = T), by = .(prov, year)]

# calc relative availability and and adjust distances so indicate that if so far from feature, it's not estimated as selection
# dat.issa[,for.age_avail:= mean(for.age_end, na.rm = T), by = indiv_step_id]
# dat.issa[,prop_veg_avail:= mean(prop_veg_end, na.rm = T), by = indiv_step_id]
# dat.issa[,prop_forest_avail:= mean(prop_forest_end, na.rm = T), by = indiv_step_id]
# dat.issa[,prop_needleleaf_avail:= mean(prop_needleleaf_end, na.rm = T), by = indiv_step_id]
# dat.issa[,prop_disturbance_avail:= mean(prop_disturbance_end, na.rm = T), by = indiv_step_id]
# dat.issa[,ts_harv_avail:= mean(ts_harv_end, na.rm = T), by = indiv_step_id]
# dat.issa[,ts_fires_avail:= mean(ts_fires_end, na.rm = T), by = indiv_step_id]


dat.issa[,dist_paved_250_start_adj:= ifelse(.SD[,.(min(dist_paved_250_start, na.rm = T))]>20000, 20001, dist_paved_250_start), by = indiv_step_id]
dat.issa[,dist_paved_250_end_adj:= ifelse(.SD[,.(min(dist_paved_250_end, na.rm = T))]>20000, 20001, dist_paved_250_end), by = indiv_step_id]
#dat.issa[,dist_paved_avail:= mean(dist_paved_end, na.rm = T), by = indiv_step_id]

dat.issa[,dist_unpaved_start_250_adj:= ifelse(.SD[,.(min(dist_unpaved_250_start, na.rm = T))]>20000, 20001, dist_unpaved_250_start), by = indiv_step_id]
dat.issa[,dist_unpaved_end_250_adj:= ifelse(.SD[,.(min(dist_unpaved_250_end, na.rm = T))]>20000, 20001, dist_unpaved_250_end), by = indiv_step_id]
#dat.issa[,dist_unpaved_avail:= mean(dist_unpaved_end, na.rm = T), by = indiv_step_id]

dat.issa[,dist_road_end_250_adj:= ifelse(.SD[,.(min(dist_road_250_end, na.rm = T))]>20000, 20001, dist_road_250_end), by = indiv_step_id]
#dat.issa[,dist_road_avail:= mean(dist_road_end, na.rm = T), by = indiv_step_id]

dat.issa[,dist_lfother_250_start_adj:= ifelse(.SD[,.(min(dist_lfother_250_start, na.rm = T))]>20000, 20001, dist_lfother_250_start), by = indiv_step_id]
dat.issa[,dist_lfother_250_end_adj:= ifelse(.SD[,.(min(dist_lfother_250_end, na.rm = T))]>20000, 20001, dist_lfother_250_end), by = indiv_step_id]
#dat.issa[,dist_lf_other_avail:= mean(dist_lf_other_end, na.rm = T), by = indiv_step_id]

dat.issa[,dist_lf_250_end_adj:= ifelse(.SD[,.(min(dist_lf_250_end, na.rm = T))]>20000, 20001, dist_lf_250_end), by = indiv_step_id]
#dat.issa[,dist_lf_avail:= mean(dist_lf_end, na.rm = T), by = indiv_step_id]

gc()
dat.issa[,dist_otherpoly_250_start_adj:= ifelse(.SD[,.(min(dist_otherpoly_250_start, na.rm = T))]>20000, 20001, dist_otherpoly_250_start), by = indiv_step_id]
dat.issa[,dist_otherpoly_250_end_adj:= ifelse(.SD[,.(min(dist_otherpoly_250_end, na.rm = T))]>20000, 20001, dist_otherpoly_250_end), by = indiv_step_id]
#dat.issa[,dist_anthro_avail:= mean(dist_anthro_end, na.rm = T), by = indiv_step_id]

dat.issa[,dist_mines_250_start_adj:= ifelse(.SD[,.(min(dist_mines_250_start, na.rm = T))]>20000, 20001, dist_mines_250_start), by = indiv_step_id]
dat.issa[,dist_mines_250_end_adj:= ifelse(.SD[,.(min(dist_mines_250_end, na.rm = T))]>20000, 20001, dist_mines_250_end), by = indiv_step_id]
#dat.issa[,dist_mines_250_avail:= mean(dist_mining_end, na.rm = T), by = indiv_step_id]

dat.issa[,dist_placer_250_start_adj:= ifelse(.SD[,.(min(dist_placer_250_start, na.rm = T))]>20000, 20001, dist_placer_250_start), by = indiv_step_id]
dat.issa[,dist_placer_250_end_adj:= ifelse(.SD[,.(min(dist_placer_250_end, na.rm = T))]>20000, 20001, dist_placer_250_end), by = indiv_step_id]
#dat.issa[,dist_placer_avail:= mean(dist_placer_end, na.rm = T), by = indiv_step_id]

dat.issa[,dist_quartz_250_start_adj:= ifelse(.SD[,.(min(dist_quartz_250_start, na.rm = T))]>20000, 20001, dist_quartz_250_start), by = indiv_step_id]
dat.issa[,dist_quartz_250_end_adj:= ifelse(.SD[,.(min(dist_quartz_250_end, na.rm = T))]>20000, 20001, dist_quartz_250_end), by = indiv_step_id]
#dat.issa[,dist_quartz_avail:= mean(dist_quartz_end, na.rm = T), by = indiv_step_id]

dat.issa[,dist_otherdisturbance_250_start_adj:= ifelse(.SD[,.(min(dist_otherdisturbance_250_start, na.rm = T))]>20000, 20001, dist_otherdisturbance_250_start), by = indiv_step_id]
dat.issa[,dist_otherdisturbance_250_end_adj:= ifelse(.SD[,.(min(dist_otherdisturbance_250_end, na.rm = T))]>20000, 20001, dist_otherdisturbance_250_end), by = indiv_step_id]
#dat.issa[,dist_otherdisturbance_avail:= mean(dist_otherdisturbance_end, na.rm = T), by = indiv_step_id]

dat.issa[,dist_anthrodisturbance_250_start_adj:= ifelse(.SD[,.(min(dist_anthrodisturbance_250_start, na.rm = T))]>20000, 20001, dist_anthrodisturbance_250_start), by = indiv_step_id]
dat.issa[,dist_anthrodisturbance_250_end_adj:= ifelse(.SD[,.(min(dist_anthrodisturbance_250_end, na.rm = T))]>20000, 20001, dist_anthrodisturbance_250_end), by = indiv_step_id]
#dat.issa[,dist_anthrodisturbance_avail:= mean(dist_anthrodisturbance_end, na.rm = T), by = indiv_step_id]
#saveRDS(dat.issa, file.path(derived, 'dat_iSSA_20lf20poly_adj_updated.RDS'))

dat.issa <- readRDS(file.path(derived, 'dat_iSSA_20lf20poly_adj_updated.RDS'))
dat.issa.20 <- dat.issa[,.SD[1:21], by = .(indiv_step_id)]
dat.issa.15 <- dat.issa[,.SD[1:16], by = .(indiv_step_id)]
dat.issa.10 <- dat.issa[,.SD[1:11], by = .(indiv_step_id)]

---------------------------------------------


# Separate seasons into diff datasets
dat.issa.10$monthday <- format(as.Date(dat.issa.10$t1_, format="%Y/%m/%d"),"%m/%d") #Create new column that isolates the month and day out of t1_
dat.issa.10$monthday #Confirming that the new column was created correctly
names(dat.issa.10) #Confirming that the new column exists

dat.issa.10[monthday >= '11/01' | monthday <= '04/15', season:="winter"] #Create new season column, with Nov to mid-April being called Winter
dat.issa.10.winter <- dat.issa.10[dat.issa.10$season == 'winter',] #Assign to new winter dataframe

dat.issa.10[monthday >= '07/01' & monthday <= '09/30', season:="summer"] #Create summer season from July through September
dat.issa.10.summer <- dat.issa.10[dat.issa.10$season == 'summer',] #Assign to new summer dataframe


###FINE SCALE MODELS: winter, summer, full, for both close to and far from disturbance (6 models), adjusted distances used for far from disturbance

##CLOSE TO DISTURBANCE
dat.issa.10.close <- dat.issa.10[dist_anthrodisturbance_250_start <= 20000]

#Year-round
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


###Functional response plots:
##Start with random slopes vs avg availability

#Get avg availability by id
dt <- as.data.table(dat.issa.10.close)

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

# Calculate average availability per id
avg_data <- dt[, lapply(.SD, mean, na.rm = TRUE), by = id, .SDcols = vars_1000]

re_list <- ranef(m.closedisturbancefull)$cond

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
  
  # Merge data for plotting
  plot_data <- merge(
    avg_data[, .(id, avail = get(avail_var))],
    random_slopes[, .(id, slope = get(model_var))],
    by = "id"
  )
  
  # Use manual axis labels
  if (!is.null(axis_labels[[model_var]])) {
    x_lab <- axis_labels[[model_var]]$x
    y_lab <- axis_labels[[model_var]]$y
  } else {
    x_lab <- paste0("Average ", avail_var)
    y_lab <- paste0("Random slope for ", model_var)
  }
  
  # Create plot
  p <- ggplot(plot_data, aes(x = avail, y = slope)) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "loess", color = "darkblue", se = TRUE) +
    labs(
      x = x_lab,
      y = "selection coeff",
    ) +
    theme_minimal(base_size = 10)
  
  # Add plot to list
  plot_list[[model_var]] <- p
}

# Combine and print all plots
combined_plot <- wrap_plots(plot_list, ncol = 3)
print(combined_plot)


##Next plot fixed effect interaction vs average availability

# Coefficient data frame
coef_df <- as.data.frame(coef(summary(m.closedisturbancefull))$cond)
coef_df$term <- rownames(coef_df)

##############################
# 1. ts_fires_250_end
##############################
raw_to_transformed <- list(
  "ts_fires_250_end" = "I(log(ts_fires_250_end + 1))",
  "ts_fires_1000_end" = "I(log(ts_fires_1000_end + 1))"
)

beta1 <- coef_df$Estimate[coef_df$term == raw_to_transformed[["ts_fires_250_end"]]]
theta <- coef_df$Estimate[coef_df$term == paste0(raw_to_transformed[["ts_fires_250_end"]], ":", raw_to_transformed[["ts_fires_1000_end"]])]

if(length(beta1) == 0 | length(theta) == 0) stop("Coefficients missing for ts_fires_250_end or interaction.")

dt_full <- merge(
  dt[, .(id, raw_x1 = get("ts_fires_250_end"))],
  avg_data[, .(id, raw_Ma_x1 = get("ts_fires_1000_end"))],
  by = "id"
)

dt_full[, x1 := log(raw_x1 + 1)]
dt_full[, Ma_x1 := log(raw_Ma_x1 + 1)]

dt_full[, w_x := exp(beta1 * x1 + theta * x1 * Ma_x1)]

plot_data <- dt_full[, .(
  mean_w = mean(w_x, na.rm = TRUE),
  sd_w = sd(w_x, na.rm = TRUE)
), by = Ma_x1]

p1 <- ggplot(plot_data, aes(x = Ma_x1, y = mean_w)) +
  geom_line(color = "steelblue", size = 1) +
  geom_ribbon(aes(ymin = mean_w - sd_w, ymax = mean_w + sd_w), alpha = 0.25, fill = "steelblue") +
  labs(x = "Transformed ts_fires_1000_end", y = "w(x)") +
  theme_minimal(base_size = 12)

print(p1)


##############################
# 2. ts_harv250_end
##############################
raw_to_transformed <- list(
  "ts_harv250_end" = "I(log(ts_harv250_end + 1))",
  "ts_harv1000_end" = "I(log(ts_harv1000_end + 1))"
)

beta1 <- coef_df$Estimate[coef_df$term == raw_to_transformed[["ts_harv250_end"]]]
theta <- coef_df$Estimate[coef_df$term == paste0(raw_to_transformed[["ts_harv250_end"]], ":", raw_to_transformed[["ts_harv1000_end"]])]

if(length(beta1) == 0 | length(theta) == 0) stop("Coefficients missing for ts_harv250_end or interaction.")

dt_full <- merge(
  dt[, .(id, raw_x1 = get("ts_harv250_end"))],
  avg_data[, .(id, raw_Ma_x1 = get("ts_harv1000_end"))],
  by = "id"
)

dt_full[, x1 := log(raw_x1 + 1)]
dt_full[, Ma_x1 := log(raw_Ma_x1 + 1)]

dt_full[, w_x := exp(beta1 * x1 + theta * x1 * Ma_x1)]

plot_data <- dt_full[, .(
  mean_w = mean(w_x, na.rm = TRUE),
  sd_w = sd(w_x, na.rm = TRUE)
), by = Ma_x1]

p2 <- ggplot(plot_data, aes(x = Ma_x1, y = mean_w)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_ribbon(aes(ymin = mean_w - sd_w, ymax = mean_w + sd_w), alpha = 0.25, fill = "darkgreen") +
  labs(x = "Transformed ts_harv1000_end", y = "w(x)") +
  theme_minimal(base_size = 12)

print(p2)


##############################
# 3. dist_paved_250_end
##############################
raw_to_transformed <- list(
  "dist_paved_250_end" = "I(log((dist_paved_250_end + 1)/1000))",
  "dist_paved_1000_end" = "I(log((dist_paved_1000_end + 1)/1000))"
)

beta1 <- coef_df$Estimate[coef_df$term == raw_to_transformed[["dist_paved_250_end"]]]
theta <- coef_df$Estimate[coef_df$term == paste0(raw_to_transformed[["dist_paved_250_end"]], ":", raw_to_transformed[["dist_paved_1000_end"]])]

if(length(beta1) == 0 | length(theta) == 0) stop("Coefficients missing for dist_paved_250_end or interaction.")

dt_full <- merge(
  dt[, .(id, raw_x1 = get("dist_paved_250_end"))],
  avg_data[, .(id, raw_Ma_x1 = get("dist_paved_1000_end"))],
  by = "id"
)

dt_full[, x1 := log((raw_x1 + 1) / 1000)]
dt_full[, Ma_x1 := log((raw_Ma_x1 + 1) / 1000)]

dt_full[, w_x := exp(beta1 * x1 + theta * x1 * Ma_x1)]

plot_data <- dt_full[, .(
  mean_w = mean(w_x, na.rm = TRUE),
  sd_w = sd(w_x, na.rm = TRUE)
), by = Ma_x1]

p3 <- ggplot(plot_data, aes(x = Ma_x1, y = mean_w)) +
  geom_line(color = "purple", size = 1) +
  geom_ribbon(aes(ymin = mean_w - sd_w, ymax = mean_w + sd_w), alpha = 0.25, fill = "purple") +
  labs(x = "Transformed dist_paved_1000_end", y = "w(x)") +
  theme_minimal(base_size = 12)

print(p3)


##############################
# 4. dist_unpaved_250_end
##############################
raw_to_transformed <- list(
  "dist_unpaved_250_end" = "I(log((dist_unpaved_250_end + 1)/1000))",
  "dist_unpaved_1000_end" = "I(log((dist_unpaved_1000_end + 1)/1000))"
)

beta1 <- coef_df$Estimate[coef_df$term == raw_to_transformed[["dist_unpaved_250_end"]]]
theta <- coef_df$Estimate[coef_df$term == paste0(raw_to_transformed[["dist_unpaved_250_end"]], ":", raw_to_transformed[["dist_unpaved_1000_end"]])]

if(length(beta1) == 0 | length(theta) == 0) stop("Coefficients missing for dist_unpaved_250_end or interaction.")

dt_full <- merge(
  dt[, .(id, raw_x1 = get("dist_unpaved_250_end"))],
  avg_data[, .(id, raw_Ma_x1 = get("dist_unpaved_1000_end"))],
  by = "id"
)

dt_full[, x1 := log((raw_x1 + 1) / 1000)]
dt_full[, Ma_x1 := log((raw_Ma_x1 + 1) / 1000)]

dt_full[, w_x := exp(beta1 * x1 + theta * x1 * Ma_x1)]

plot_data <- dt_full[, .(
  mean_w = mean(w_x, na.rm = TRUE),
  sd_w = sd(w_x, na.rm = TRUE)
), by = Ma_x1]

p4 <- ggplot(plot_data, aes(x = Ma_x1, y = mean_w)) +
  geom_line(color = "orange", size = 1) +
  geom_ribbon(aes(ymin = mean_w - sd_w, ymax = mean_w + sd_w), alpha = 0.25, fill = "orange") +
  labs(x = "Transformed dist_unpaved_1000_end", y = "w(x)") +
  theme_minimal(base_size = 12)

print(p4)


##############################
# 5. dist_lfother_250_end
##############################
raw_to_transformed <- list(
  "dist_lfother_250_end" = "I(log((dist_lfother_250_end + 1)/1000))",
  "dist_lfother_1000_end" = "I(log((dist_lfother_1000_end + 1)/1000))"
)

beta1 <- coef_df$Estimate[coef_df$term == raw_to_transformed[["dist_lfother_250_end"]]]
theta <- coef_df$Estimate[coef_df$term == paste0(raw_to_transformed[["dist_lfother_250_end"]], ":", raw_to_transformed[["dist_lfother_1000_end"]])]

if(length(beta1) == 0 | length(theta) == 0) stop("Coefficients missing for dist_lfother_250_end or interaction.")

dt_full <- merge(
  dt[, .(id, raw_x1 = get("dist_lfother_250_end"))],
  avg_data[, .(id, raw_Ma_x1 = get("dist_lfother_1000_end"))],
  by = "id"
)

dt_full[, x1 := log((raw_x1 + 1) / 1000)]
dt_full[, Ma_x1 := log((raw_Ma_x1 + 1) / 1000)]

dt_full[, w_x := exp(beta1 * x1 + theta * x1 * Ma_x1)]

plot_data <- dt_full[, .(
  mean_w = mean(w_x, na.rm = TRUE),
  sd_w = sd(w_x, na.rm = TRUE)
), by = Ma_x1]

p5 <- ggplot(plot_data, aes(x = Ma_x1, y = mean_w)) +
  geom_line(color = "brown", size = 1) +
  geom_ribbon(aes(ymin = mean_w - sd_w, ymax = mean_w + sd_w), alpha = 0.25, fill = "brown") +
  labs(x = "Transformed dist_lfother_1000_end", y = "w(x)") +
  theme_minimal(base_size = 12)

print(p5)


##############################
# 6. dist_otherpoly_250_end
##############################
raw_to_transformed <- list(
  "dist_otherpoly_250_end" = "I(log((dist_otherpoly_250_end + 1)/1000))",
  "dist_otherpoly_1000_end" = "I(log((dist_otherpoly_1000_end + 1)/1000))"
)

beta1 <- coef_df$Estimate[coef_df$term == raw_to_transformed[["dist_otherpoly_250_end"]]]
theta <- coef_df$Estimate[coef_df$term == paste0(raw_to_transformed[["dist_otherpoly_250_end"]], ":", raw_to_transformed[["dist_otherpoly_1000_end"]])]

if(length(beta1) == 0 | length(theta) == 0) stop("Coefficients missing for dist_otherpoly_250_end or interaction.")

dt_full <- merge(
  dt[, .(id, raw_x1 = get("dist_otherpoly_250_end"))],
  avg_data[, .(id, raw_Ma_x1 = get("dist_otherpoly_1000_end"))],
  by = "id"
)

dt_full[, x1 := log((raw_x1 + 1) / 1000)]
dt_full[, Ma_x1 := log((raw_Ma_x1 + 1) / 1000)]

dt_full[, w_x := exp(beta1 * x1 + theta * x1 * Ma_x1)]

plot_data <- dt_full[, .(
  mean_w = mean(w_x, na.rm = TRUE),
  sd_w = sd(w_x, na.rm = TRUE)
), by = Ma_x1]

p6 <- ggplot(plot_data, aes(x = Ma_x1, y = mean_w)) +
  geom_line(color = "darkred", size = 1) +
  geom_ribbon(aes(ymin = mean_w - sd_w, ymax = mean_w + sd_w), alpha = 0.25, fill = "darkred") +
  labs(x = "Transformed dist_otherpoly_1000_end", y = "w(x)") +
  theme_minimal(base_size = 12)

print(p6)


##############################
# 7. dist_placer_250_end
##############################
raw_to_transformed <- list(
  "dist_placer_250_end" = "I(log((dist_placer_250_end + 1)/1000))",
  "dist_placer_1000_end" = "I(log((dist_placer_1000_end + 1)/1000))"
)

beta1 <- coef_df$Estimate[coef_df$term == raw_to_transformed[["dist_placer_250_end"]]]
theta <- coef_df$Estimate[coef_df$term == paste0(raw_to_transformed[["dist_placer_250_end"]], ":", raw_to_transformed[["dist_placer_1000_end"]])]

if(length(beta1) == 0 | length(theta) == 0) stop("Coefficients missing for dist_placer_250_end or interaction.")

dt_full <- merge(
  dt[, .(id, raw_x1 = get("dist_placer_250_end"))],
  avg_data[, .(id, raw_Ma_x1 = get("dist_placer_1000_end"))],
  by = "id"
)

dt_full[, x1 := log((raw_x1 + 1) / 1000)]
dt_full[, Ma_x1 := log((raw_Ma_x1 + 1) / 1000)]

dt_full[, w_x := exp(beta1 * x1 + theta * x1 * Ma_x1)]

plot_data <- dt_full[, .(
  mean_w = mean(w_x, na.rm = TRUE),
  sd_w = sd(w_x, na.rm = TRUE)
), by = Ma_x1]

p7 <- ggplot(plot_data, aes(x = Ma_x1, y = mean_w)) +
  geom_line(color = "darkblue", size = 1) +
  geom_ribbon(aes(ymin = mean_w - sd_w, ymax = mean_w + sd_w), alpha = 0.25, fill = "darkblue") +
  labs(x = "Transformed dist_placer_1000_end", y = "w(x)") +
  theme_minimal(base_size = 12)

print(p7)


##############################
# 8. dist_quartz_250_end
##############################
raw_to_transformed <- list(
  "dist_quartz_250_end" = "I(log((dist_quartz_250_end + 1)/1000))",
  "dist_quartz_1000_end" = "I(log((dist_quartz_1000_end + 1)/1000))"
)

beta1 <- coef_df$Estimate[coef_df$term == raw_to_transformed[["dist_quartz_250_end"]]]
theta <- coef_df$Estimate[coef_df$term == paste0(raw_to_transformed[["dist_quartz_250_end"]], ":", raw_to_transformed[["dist_quartz_1000_end"]])]

if(length(beta1) == 0 | length(theta) == 0) stop("Coefficients missing for dist_quartz_250_end or interaction.")

dt_full <- merge(
  dt[, .(id, raw_x1 = get("dist_quartz_250_end"))],
  avg_data[, .(id, raw_Ma_x1 = get("dist_quartz_1000_end"))],
  by = "id"
)

dt_full[, x1 := log((raw_x1 + 1) / 1000)]
dt_full[, Ma_x1 := log((raw_Ma_x1 + 1) / 1000)]

dt_full[, w_x := exp(beta1 * x1 + theta * x1 * Ma_x1)]

plot_data <- dt_full[, .(
  mean_w = mean(w_x, na.rm = TRUE),
  sd_w = sd(w_x, na.rm = TRUE)
), by = Ma_x1]

p8 <- ggplot(plot_data, aes(x = Ma_x1, y = mean_w)) +
  geom_line(color = "steelblue", size = 1) +
  geom_ribbon(aes(ymin = mean_w - sd_w, ymax = mean_w + sd_w), alpha = 0.25, fill = "steelblue") +
  labs(x = "Transformed dist_quartz_1000_end", y = "w(x)") +
  theme_minimal(base_size = 12)

print(p8)


##############################
# 9. dist_othermines_250_end
##############################
raw_to_transformed <- list(
  "dist_othermines_250_end" = "I(log((dist_othermines_250_end + 1)/1000))",
  "dist_othermines_1000_end" = "I(log((dist_othermines_1000_end + 1)/1000))"
)

beta1 <- coef_df$Estimate[coef_df$term == raw_to_transformed[["dist_othermines_250_end"]]]
theta <- coef_df$Estimate[coef_df$term == paste0(raw_to_transformed[["dist_othermines_250_end"]], ":", raw_to_transformed[["dist_othermines_1000_end"]])]

if(length(beta1) == 0 | length(theta) == 0) stop("Coefficients missing for dist_othermines_250_end or interaction.")

dt_full <- merge(
  dt[, .(id, raw_x1 = get("dist_othermines_250_end"))],
  avg_data[, .(id, raw_Ma_x1 = get("dist_othermines_1000_end"))],
  by = "id"
)

dt_full[, x1 := log((raw_x1 + 1) / 1000)]
dt_full[, Ma_x1 := log((raw_Ma_x1 + 1) / 1000)]

dt_full[, w_x := exp(beta1 * x1 + theta * x1 * Ma_x1)]

plot_data <- dt_full[, .(
  mean_w = mean(w_x, na.rm = TRUE),
  sd_w = sd(w_x, na.rm = TRUE)
), by = Ma_x1]

p9 <- ggplot(plot_data, aes(x = Ma_x1, y = mean_w)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_ribbon(aes(ymin = mean_w - sd_w, ymax = mean_w + sd_w), alpha = 0.25, fill = "darkgreen") +
  labs(x = "Transformed dist_othermines_1000_end", y = "w(x)") +
  theme_minimal(base_size = 12)

print(p9)

                                
##############################
# 10. prop_forest_250_end
##############################
raw_to_transformed <- list(
  "prop_forest_250_end" = "prop_forest_250_end",
  "prop_forest_1000_end" = "prop_forest_1000_end"
)                                              
beta1 <- coef_df$Estimate[coef_df$term == "prop_forest_250_end"]
theta <- coef_df$Estimate[coef_df$term == "prop_forest_250_end:prop_forest_1000_end"]

if(length(beta1) == 0 || length(theta) == 0) stop("Missing coefficients for prop_forest_250_end.")

dt_full <- merge(
  dt[, .(id, raw_x1 = get("prop_forest_250_end"))],
  avg_data[, .(id, raw_Ma_x1 = get("prop_forest_1000_end"))],
  by = "id"
)

dt_full[, x1 := raw_x1]  # No transformation
dt_full[, Ma_x1 := raw_Ma_x1]  # No transformation
dt_full[, w_x := exp(beta1 * x1 + theta * x1 * Ma_x1)]

plot_data_10 <- dt_full[, .(
  mean_w = mean(w_x, na.rm = TRUE),
  sd_w = sd(w_x, na.rm = TRUE)
), by = Ma_x1]

p10 <- ggplot(plot_data_10, aes(x = Ma_x1, y = mean_w)) +
  geom_line(color = "darkolivegreen") +
  geom_ribbon(aes(ymin = mean_w - sd_w, ymax = mean_w + sd_w), alpha = 0.3, fill = "darkolivegreen") +
  labs(x = "prop_forest_1000_end", y = "w(x)") +
  theme_minimal()
print(p10)




##############################
# 11. prop_veg_250_end
##############################
raw_to_transformed <- list(
  "prop_veg_250_end" = "prop_veg_250_end",
  "prop_veg_1000_end" = "prop_veg_1000_end"
)

beta1 <- coef_df$Estimate[coef_df$term == raw_to_transformed[["prop_veg_250_end"]]]
theta <- coef_df$Estimate[coef_df$term == paste0(raw_to_transformed[["prop_veg_250_end"]], ":", raw_to_transformed[["prop_veg_1000_end"]])]

if(length(beta1) == 0 | length(theta) == 0) stop("Coefficients missing for prop_veg_250_end or interaction.")

dt_full <- merge(
  dt[, .(id, raw_x1 = get("prop_veg_250_end"))],
  avg_data[, .(id, raw_Ma_x1 = get("prop_veg_1000_end"))],
  by = "id"
)

dt_full[, x1 := raw_x1]
dt_full[, Ma_x1 := raw_Ma_x1]

dt_full[, w_x := exp(beta1 * x1 + theta * x1 * Ma_x1)]

plot_data <- dt_full[, .(
  mean_w = mean(w_x, na.rm = TRUE),
  sd_w = sd(w_x, na.rm = TRUE)
), by = Ma_x1]

p11 <- ggplot(plot_data, aes(x = Ma_x1, y = mean_w)) +
  geom_line(color = "forestgreen", size = 1) +
  geom_ribbon(aes(ymin = mean_w - sd_w, ymax = mean_w + sd_w),
              alpha = 0.25, fill = "forestgreen") +
  labs(
    x = "prop_veg_1000_end",
    y = "w(x)"
  ) +
  theme_minimal(base_size = 12)

print(p11)




plot_list <- list(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)
patchwork::wrap_plots(plot_list, ncol = 4)






#SUMMER
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



#INSERT FUNCTIONAL RESPONSE PLOTS



#WINTER
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

#INSERT FUNCTIONAL RESPONSE PLOTS

##FAR FROM DISTURBANCE

#Year-round
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


#Raincloud plots
indivs_filtered <- indivs[
  grepl("dist", term) & !grepl(":.*1000", term)
]

indivs_nondist_filtered <- indivs[
  !grepl("dist", term) & !grepl(":.*1000", term)
]


ggplot(data = indivs_filtered, aes(x = term, y = estimate)) +
  ggdist::stat_halfeye(
    adjust = 0.5,
    justification = -0.3,
    .width = 0,
    point_colour = NA,
    fill = "skyblue"
  ) +
  geom_boxplot(
    width = 0.15,
    outlier.shape = NA,
    position = position_nudge(x = 0.15)
  ) +
  geom_point(
    position = position_jitter(width = 0.05, height = 0),
    alpha = 0.4
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(
    x = "Term",
    y = "Estimate",
  ) +
  theme_minimal()


ggplot(data = indivs_nondist_filtered, aes(x = term, y = estimate)) +
  ggdist::stat_halfeye(
    adjust = 0.5,
    justification = -0.3,
    .width = 0,
    point_colour = NA,
    fill = "lightgreen"
  ) +
  geom_boxplot(
    width = 0.15,
    outlier.shape = NA,
    position = position_nudge(x = 0.15)
  ) +
  geom_point(
    position = position_jitter(width = 0.05, height = 0),
    alpha = 0.4
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(
    x = "Term",
    y = "Estimate",
  ) +
  theme_minimal()


indivs_forest <- indivs[term == "prop_forest_250_end"]

ggplot(indivs_forest, aes(x = term, y = estimate)) +
  ggdist::stat_halfeye(
    adjust = 0.5,
    justification = -0.3,
    .width = 0,
    point_colour = NA,
    fill = "forestgreen"
  ) +
  geom_boxplot(
    width = 0.15,
    outlier.shape = NA,
    position = position_nudge(x = 0.15)
  ) +
  geom_point(
    position = position_jitter(width = 0.05, height = 0),
    alpha = 0.4
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Estimate",
    title = "Raincloud Plot of Individual Slopes for prop_forest_250_end"
  ) +
  theme_minimal()




#SUMMER
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
                                    family = poisson(), data = dat.issa.10.summer[,`:=`(year = as.factor(year), 
                                                                                              indiv_step_id = as.factor(indiv_step_id),
                                                                                              id = as.factor(id))],
                                    map= list(theta = factor(c(NA,1:15))),
                                    start = list(theta =c(log(1000), seq(0,0, length.out = 15))), 
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



#WINTER
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
                                    family = poisson(), data = dat.issa.10.winter[,`:=`(year = as.factor(year), 
                                                                                              indiv_step_id = as.factor(indiv_step_id),
                                                                                              id = as.factor(id))],
                                    map= list(theta = factor(c(NA,1:15))),
                                    start = list(theta =c(log(1000), seq(0,0, length.out = 15))), 
                                    verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(m.fardisturbancewinter)
# save model output
saveRDS(m.faristurbancewinter, "m.fardisturbancewinter.mod_lf20poly20.RDS")

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























--------------#OLD CODE BELOW-------------------------------


#Fiddling further by filtering out data when the disturbance is far away
dat.issa.10$dist_anthrodisturbance_start <= 20000

gc()
m.closedisturbance <- glmmTMB(case_ ~ -1 +
                                I(log((sl_+1)/1000)) +
                                #I(cos(ta_)) +
                                I(log((sl_+1)/1000)):I(cos(ta_)) +
                                #scale(for.age_start):I(log(sl_+1)) + 
                                #  prop_forest_start:I(log(sl_+1)) +
                                # prop_veg_start:I(log(sl_+1)) + 
                                # scale(for.age_end) + scale(for.age_end):scale(for.age_avail) +
                                prop_forest_end + prop_forest_end:prop_forest_avail +
                                prop_veg_end + prop_veg_end:prop_veg_avail +
                                I(log(ts_fires_end+1)) + I(log(ts_fires_end+1)):I(log(ts_fires_avail+1)) +
                                # I(log(ts_harv_end+1)) + I(log(ts_harv_end+1)):I(log(ts_harv_avail+1)) +
                                # I(log((dist_paved_end_adj/1000)+1)) + I(log((dist_paved_end_adj/1000)+1)):I(log((dist_paved_avail/1000)+1)) +
                                #  I(log((dist_unpaved_end_adj/1000)+1)) + I(log((dist_unpaved_end_adj/1000)+1)):I(log((dist_unpaved_avail/1000)+1)) +
                                I(log(((dist_road_end+1)/1000))) + #I(log((dist_road_end_adj/1000)+1)):I(log((dist_road_avail/1000)+1)) +
                                I(log((dist_lf_other_end+1)/1000)) +# I(log((dist_lf_other_end_adj/1000)+1)):I(log((dist_lf_other_avail/1000)+1)) +
                                #I(log((dist_anthro_end_adj/1000)+1)) + I(log((dist_anthro_end_adj/1000)+1)):I(log((dist_anthro_avail/1000)+1)) +
                                I(log((dist_mining_end+1)/1000)) +# I(log((dist_mining_end_adj/1000)+1)):I(log((dist_mining_avail/1000)+1)) +
                                I(log((dist_otherdisturbance_end+1)/1000)) + #I(log((dist_otherdisturbance_end_adj/1000)+1)):I(log((dist_otherdisturbance_avail/1000)+1)) +
                                
                                (1|indiv_step_id) +
                                (0 + I(log((sl_ +1)/1000))|id) +
                                #(0 + I(cos(ta_))|id) +
                                (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                                #(0 + scale(for.age_start):I(log(sl_+1))|id) + 
                                #  (0 + prop_forest_start:I(log(sl_+1))|id) +
                                # (0 + prop_veg_start:I(log(sl_+1))|id) + 
                                # (0 + scale(for.age_end)|id) + 
                                (0 + prop_forest_end|id) +
                                (0 + prop_veg_end|id) +
                                (0 + (I(log(ts_fires_end+1)))|id) +
                                # (0 + (I(log(ts_harv_end+1)))|id) + 
                                # (0 + I(log((dist_paved_end_adj/1000)+1))|id) + 
                                # (0 + I(log((dist_unpaved_end_adj/1000)+1))|id) + 
                                (0 + I(log((dist_road_end+1)/1000))|id) + 
                                (0 + I(log((dist_lf_other_end+1)/1000))|id) +
                                #(0 + I(log((dist_anthro_end_adj/1000)+1))|id) + 
                                (0 + I(log((dist_mining_end+1)/1000))|id) + 
                                (0 + I(log((dist_otherdisturbance_end+1)/1000))|id) + 
                                
                                (1|year),
                              family = poisson(), data = dat.issa.10[,`:=`(year = as.factor(year), 
                                                                           indiv_step_id = as.factor(indiv_step_id),
                                                                           id = as.factor(id))],
                              map= list(theta = factor(c(NA,1:9))),
                              start = list(theta =c(log(1000), seq(0,0, length.out = 9))), 
                              verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)




##VARIANCE ISSUE HAS BEEN FIXED BY MODELLING NEAR-BY DISTURBANCES ONLY



---------------------------------------------


#Testing correlations between each unique distance-to variables for collinearity
cor(dat.issa.10$dist_paved_end, dat.issa.10$dist_unpaved_end, use = "complete.obs") #0.71
cor(dat.issa.10$dist_paved_end, dat.issa.10$dist_lf_other_end, use = "complete.obs") #-0.005
cor(dat.issa.10$dist_paved_end, dat.issa.10$dist_placer_end, use = "complete.obs") #-0.13
cor(dat.issa.10$dist_paved_end, dat.issa.10$dist_quartz_end, use = "complete.obs") #-0.30
cor(dat.issa.10$dist_paved_end, dat.issa.10$dist_harv_end, use = "complete.obs") #0.24
cor(dat.issa.10$dist_paved_end, dat.issa.10$dist_anthro_end, use = "complete.obs") #0.41
cor(dat.issa.10$dist_unpaved_end, dat.issa.10$dist_lf_other_end, use = "complete.obs") #0.29
cor(dat.issa.10$dist_unpaved_end, dat.issa.10$dist_placer_end, use = "complete.obs") #0.05
cor(dat.issa.10$dist_unpaved_end, dat.issa.10$dist_quartz_end, use = "complete.obs") #-0.12
cor(dat.issa.10$dist_unpaved_end, dat.issa.10$dist_harv_end, use = "complete.obs") #0.47
cor(dat.issa.10$dist_unpaved_end, dat.issa.10$dist_anthro_end, use = "complete.obs") #0.75
cor(dat.issa.10$dist_lf_other_end, dat.issa.10$dist_placer_end, use = "complete.obs") #0.29
cor(dat.issa.10$dist_lf_other_end, dat.issa.10$dist_quartz_end, use = "complete.obs") #0.32
cor(dat.issa.10$dist_lf_other_end, dat.issa.10$dist_harv_end, use = "complete.obs") #0.02
cor(dat.issa.10$dist_lf_other_end, dat.issa.10$dist_anthro_end, use = "complete.obs") #0.57
cor(dat.issa.10$dist_placer_end, dat.issa.10$dist_quartz_end, use = "complete.obs") #0.91
cor(dat.issa.10$dist_placer_end, dat.issa.10$dist_harv_end, use = "complete.obs") #0.04
cor(dat.issa.10$dist_placer_end, dat.issa.10$dist_anthro_end, use = "complete.obs") #0.22
cor(dat.issa.10$dist_quartz_end, dat.issa.10$dist_harv_end, use = "complete.obs") #-0.16
cor(dat.issa.10$dist_quartz_end, dat.issa.10$dist_anthro_end, use = "complete.obs") #0.09
cor(dat.issa.10$dist_harv_end, dat.issa.10$dist_anthro_end, use = "complete.obs") #0.33


cor_df <- dat.issa.10 %>% select('dist_paved_end', 'dist_unpaved_end', 'dist_lf_other_end', 'dist_placer_end', 'dist_quartz_end', 'dist_harv_end', 'dist_anthro_end') #create new df with the variables I want
library(corrplot)
M = cor(cor_df, use = "complete.obs") #produce plot of correlations
corrplot(M, method = 'number')

cor_df2 <- dat.issa.10 %>% select('dist_road_end', 'dist_mining_end', 'dist_anthro_end', 'dist_lf_other_end')
N = cor(cor_df2, use = "complete.obs") #produce plot of correlations
corrplot(N, method = 'number')


dat.issa.10[, dist_polydisturbance_end := pmin(dist_anthro_end, dist_harv_end, dist_placer_end, dist_quartz_end, na.rm = T)]
cor_df3 <- dat.issa.10 %>% select('dist_polydisturbance_end', 'dist_lf_end')
O = cor(cor_df3, use = "complete.obs") #produce plot of correlations
corrplot(O, method = 'number')


---------------------------------------------
#Dropping distance to anthrodisturbance out of the random effects
gc()
m.fardisturbance <- glmmTMB(case_ ~ -1 +
                            I(log((sl_+1)/1000)) +
                            #I(cos(ta_)) +
                            I(log((sl_+1)/1000)):I(cos(ta_)) +
                            #scale(for.age_start):I(log(sl_+1)) + 
                            # prop_forest_start:I(log(sl_+1)) +
                            #prop_veg_start:I(log(sl_+1)) + 
                            # scale(for.age_end) + scale(for.age_end):scale(for.age_avail) +
                            prop_forest_end + prop_forest_end:prop_forest_avail +
                            prop_veg_end + prop_veg_end:prop_veg_avail +
                            I(log(ts_fires_end+1)) + I(log(ts_fires_end+1)):I(log(ts_fires_avail+1)) +
                            # I(log(ts_harv_end+1)) + I(log(ts_harv_end+1)):I(log(ts_harv_avail+1)) +
                            # I(log((dist_paved_end_adj/1000)+1)) + I(log((dist_paved_end_adj/1000)+1)):I(log((dist_paved_avail/1000)+1)) +
                            #  I(log((dist_unpaved_end_adj/1000)+1)) + I(log((dist_unpaved_end_adj/1000)+1)):I(log((dist_unpaved_avail/1000)+1)) +
                            # I(log(((dist_road_end_adj+1)/1000))) + #I(log((dist_road_end_adj/1000)+1)):I(log((dist_road_avail/1000)+1)) +
                            # I(log((dist_lf_other_end_adj+1)/1000)) + I(log((dist_lf_other_end_adj/1000)+1)):I(log((dist_lf_other_avail/1000)+1)) +
                            #I(log((dist_anthro_end_adj/1000)+1)) + I(log((dist_anthro_end_adj/1000)+1)):I(log((dist_anthro_avail/1000)+1)) +
                            # I(log((dist_mining_end_adj+1)/1000)) +# I(log((dist_mining_end_adj/1000)+1)):I(log((dist_mining_avail/1000)+1)) +
                            I(log((dist_anthrodisturbance_end_adj+1)/1000)) + #I(log((dist_anthrodisturbance_end_adj+1)/1000)):I(log(((dist_anthrodisturbance_avail+1)/1000))) +
                             
                            (1|indiv_step_id) +
                            (0 + I(log((sl_ +1)/1000))|id) +
                            #(0 + I(cos(ta_))|id) +
                            (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                            #(0 + scale(for.age_start):I(log(sl_+1))|id) + 
                            #  (0 + prop_forest_start:I(log(sl_+1))|id) +
                            # (0 + prop_veg_start:I(log(sl_+1))|id) + 
                            # (0 + scale(for.age_end)|id) + 
                            (0 + prop_forest_end|id) +
                            (0 + prop_veg_end|id) +
                            (0 + (I(log(ts_fires_end+1)))|id) +
                            #(0 + (I(log(ts_harv_end+1)))|id) + 
                            #(0 + I(log((dist_paved_end_adj/1000)+1))|id) + 
                            # (0 + I(log((dist_unpaved_end_adj/1000)+1))|id) + 
                            # (0 + I(log((dist_road_end_adj+1)/1000))|id) + 
                            # (0 + I(log((dist_lf_other_end_adj+1)/1000))|id) +
                            #(0 + I(log((dist_anthro_end_adj/1000)+1))|id) + 
                            # (0 + I(log((dist_mining_end_adj+1)/1000))|id) + 
                            ##(0 + I(log((dist_anthrodisturbance_end_adj+1)/1000))|id) + 
                             
                           (1|year),
                         family = poisson(), data = dat.issa.10[,`:=`(year = as.factor(year), 
                                                                            indiv_step_id = as.factor(indiv_step_id),
                                                                            id = as.factor(id))],
                         map= list(theta = factor(c(NA,1:6))),
                         start = list(theta =c(log(1000), seq(0,0, length.out = 6))), 
                         verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
 )


summary(m.fardisturbance)
# save model output
saveRDS(m.fardisturbance, "m.fardisturbance.mod_lf20poly20.RDS")

m.fardis.indiv <- coef(m.fardisturbance)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(m.fardis.indiv)[term!='(Intercept)']
pop <- setDT(tidy(m.fardisturbance, effects = 'fixed'))


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
  
#worked and looks good
    
---------------------------------------------
  
###Now run seasonal datasets through the two models that have worked so far, then attempting to tease apart the mines (quartz and placer)
 

#PETE START HERE JUNE 19 - updates avails, keep other variables the same as before


#Starting off with Winter and close to disturbance
closedisturbwinter <- dat.issa.10.winter[dist_anthrodisturbance_start <= 20000]

gc()
winter.m.closedisturbance <- glmmTMB(case_ ~ -1 +
                                I(log((sl_+1)/1000)) +
                                #I(cos(ta_)) +
                                I(log((sl_+1)/1000)):I(cos(ta_)) +
                                #scale(for.age_start):I(log(sl_+1)) + 
                                #  prop_forest_start:I(log(sl_+1)) +
                                # prop_veg_start:I(log(sl_+1)) + 
                                # scale(for.age_end) + scale(for.age_end):scale(for.age_avail) +
                                prop_forest_end + prop_forest_end:prop_forest_avail +
                                prop_veg_end + prop_veg_end:prop_veg_avail +
                                I(log(ts_fires_end+1)) + I(log(ts_fires_end+1)):I(log(ts_fires_avail+1)) +
                                # I(log(ts_harv_end+1)) + I(log(ts_harv_end+1)):I(log(ts_harv_avail+1)) +
                                 I(log((dist_paved_end_adj/1000)+1)) + I(log((dist_paved_end_adj/1000)+1)):I(log((dist_paved_avail/1000)+1)) +
                                  I(log((dist_unpaved_end_adj/1000)+1)) + I(log((dist_unpaved_end_adj/1000)+1)):I(log((dist_unpaved_avail/1000)+1)) +
                                #I(log(((dist_road_end+1)/1000))) + #I(log((dist_road_end_adj/1000)+1)):I(log((dist_road_avail/1000)+1)) +
                                I(log((dist_lf_other_end+1)/1000)) +# I(log((dist_lf_other_end_adj/1000)+1)):I(log((dist_lf_other_avail/1000)+1)) +
                                #I(log((dist_anthro_end_adj/1000)+1)) + I(log((dist_anthro_end_adj/1000)+1)):I(log((dist_anthro_avail/1000)+1)) +
                                I(log((dist_placer_end+1)/1000)) + I(log((dist_quartz_end+1)/1000)) +
                                #I(log((dist_mining_end+1)/1000)) +# I(log((dist_mining_end_adj/1000)+1)):I(log((dist_mining_avail/1000)+1)) +
                                I(log((dist_otherdisturbance_end+1)/1000)) + #I(log((dist_otherdisturbance_end_adj/1000)+1)):I(log((dist_otherdisturbance_avail/1000)+1)) +
                                
                                (1|indiv_step_id) +
                                (0 + I(log((sl_ +1)/1000))|id) +
                                #(0 + I(cos(ta_))|id) +
                                (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                                #(0 + scale(for.age_start):I(log(sl_+1))|id) + 
                                #  (0 + prop_forest_start:I(log(sl_+1))|id) +
                                # (0 + prop_veg_start:I(log(sl_+1))|id) + 
                                # (0 + scale(for.age_end)|id) + 
                                (0 + prop_forest_end|id) +
                                (0 + prop_veg_end|id) +
                                (0 + (I(log(ts_fires_end+1)))|id) +
                                # (0 + (I(log(ts_harv_end+1)))|id) + 
                                 (0 + I(log((dist_paved_end_adj/1000)+1))|id) + 
                                 (0 + I(log((dist_unpaved_end_adj/1000)+1))|id) + 
                                #(0 + I(log((dist_road_end+1)/1000))|id) + 
                                (0 + I(log((dist_lf_other_end+1)/1000))|id) +
                                #(0 + I(log((dist_anthro_end_adj/1000)+1))|id) + 
                                (0 + I(log((dist_placer_end+1)/1000))|id) +
                                (0 + I(log((dist_quartz_end+1)/1000))|id) +
                                #(0 + I(log((dist_mining_end+1)/1000))|id) + 
                                (0 + I(log((dist_otherdisturbance_end+1)/1000))|id) + 
                                
                                (1|year),
                              family = poisson(), data = closedisturbwinter[,`:=`(year = as.factor(year), 
                                                                           indiv_step_id = as.factor(indiv_step_id),
                                                                           id = as.factor(id))],
                              map= list(theta = factor(c(NA,1:11))),
                              start = list(theta =c(log(1000), seq(0,0, length.out = 11))), 
                              verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(winter.m.closedisturbance)
# save model output
saveRDS(winter.m.closedisturbance, "winter.m.closedisturbance.mod_lf20poly20.RDS")
saveRDS(winter.m.closedisturbance, "winter.m.closedisturbanceplacerquartz.mod_lf20poly20.RDS")

winter.m.closedisturbance.indiv <- coef(winter.m.closedisturbance)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(winter.m.closedisturbance.indiv)[term!='(Intercept)']
pop <- setDT(tidy(winter.m.closedisturbance, effects = 'fixed'))


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

#Looks great, moving on to dropping dist_anthro from winterdataset - model worked with placer and quartz

gc()
winter.m.fardisturbancepq <- glmmTMB(case_ ~ -1 +
                              I(log((sl_+1)/1000)) +
                              #I(cos(ta_)) +
                              I(log((sl_+1)/1000)):I(cos(ta_)) +
                              #scale(for.age_start):I(log(sl_+1)) + 
                              # prop_forest_start:I(log(sl_+1)) +
                              #prop_veg_start:I(log(sl_+1)) + 
                              # scale(for.age_end) + scale(for.age_end):scale(for.age_avail) +
                              prop_forest_end + prop_forest_end:prop_forest_avail +
                              prop_veg_end + prop_veg_end:prop_veg_avail +
                              I(log(ts_fires_end+1)) + I(log(ts_fires_end+1)):I(log(ts_fires_avail+1)) +
                              # I(log(ts_harv_end+1)) + I(log(ts_harv_end+1)):I(log(ts_harv_avail+1)) +
                              # I(log((dist_paved_end_adj/1000)+1)) + I(log((dist_paved_end_adj/1000)+1)):I(log((dist_paved_avail/1000)+1)) +
                              #  I(log((dist_unpaved_end_adj/1000)+1)) + I(log((dist_unpaved_end_adj/1000)+1)):I(log((dist_unpaved_avail/1000)+1)) +
                              # I(log(((dist_road_end_adj+1)/1000))) + #I(log((dist_road_end_adj/1000)+1)):I(log((dist_road_avail/1000)+1)) +
                              # I(log((dist_lf_other_end_adj+1)/1000)) + I(log((dist_lf_other_end_adj/1000)+1)):I(log((dist_lf_other_avail/1000)+1)) +
                              #I(log((dist_anthro_end_adj/1000)+1)) + I(log((dist_anthro_end_adj/1000)+1)):I(log((dist_anthro_avail/1000)+1)) +
                              I(log((dist_placer_end+1)/1000)) + I(log((dist_quartz_end+1)/1000)) +
                              # I(log((dist_mining_end_adj+1)/1000)) +# I(log((dist_mining_end_adj/1000)+1)):I(log((dist_mining_avail/1000)+1)) +
                              I(log((dist_otherdisturbance_end_adj+1)/1000)) + #I(log((dist_anthrodisturbance_end_adj+1)/1000)):I(log(((dist_anthrodisturbance_avail+1)/1000))) +
                              
                              (1|indiv_step_id) +
                              (0 + I(log((sl_ +1)/1000))|id) +
                              #(0 + I(cos(ta_))|id) +
                              (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                              #(0 + scale(for.age_start):I(log(sl_+1))|id) + 
                              #  (0 + prop_forest_start:I(log(sl_+1))|id) +
                              # (0 + prop_veg_start:I(log(sl_+1))|id) + 
                              # (0 + scale(for.age_end)|id) + 
                              (0 + prop_forest_end|id) +
                              (0 + prop_veg_end|id) +
                              (0 + (I(log(ts_fires_end+1)))|id) +
                              #(0 + (I(log(ts_harv_end+1)))|id) + 
                              #(0 + I(log((dist_paved_end_adj/1000)+1))|id) + 
                              # (0 + I(log((dist_unpaved_end_adj/1000)+1))|id) + 
                              # (0 + I(log((dist_road_end_adj+1)/1000))|id) + 
                              # (0 + I(log((dist_lf_other_end_adj+1)/1000))|id) +
                              #(0 + I(log((dist_anthro_end_adj/1000)+1))|id) + 
                              # (0 + I(log((dist_mining_end_adj+1)/1000))|id) + 
                              (0 + I(log((dist_placer_end+1)/1000))|id) +
                              (0 + I(log((dist_quartz_end+1)/1000))|id) +
                              ##(0 + I(log((dist_anthrodisturbance_end_adj+1)/1000))|id) + 
                              
                              (1|year),
                            family = poisson(), data = dat.issa.10.winter[,`:=`(year = as.factor(year), 
                                                                         indiv_step_id = as.factor(indiv_step_id),
                                                                         id = as.factor(id))],
                            map= list(theta = factor(c(NA,1:8))),
                            start = list(theta =c(log(1000), seq(0,0, length.out = 8))), 
                            verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


  
summary(m.fardisturbance)
# save model output
saveRDS(m.fardisturbance, "winter.m.fardisturbance.mod_lf20poly20.RDS")

summary(winter.m.fardisturbancepq)
saveRDS(winter.m.fardisturbancepq, "winter.m.fardisturbancepq.mod_lf20poly20.RDS")

winter.m.fardisturbance.indiv <- coef(winter.m.fardisturbancepq)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(winter.m.fardisturbance.indiv)[term!='(Intercept)']
pop <- setDT(tidy(winter.m.fardisturbancepq, effects = 'fixed'))


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


# Looks good as well. Moving on to the summer dataset - model ran with placer and quartz

#Starting with the close to disturbance model
closedisturbsummer <- dat.issa.10.summer[dist_anthrodisturbance_start <= 20000]

gc()
summer.m.closedisturbancepq <- glmmTMB(case_ ~ -1 +
                                       I(log((sl_+1)/1000)) +
                                       #I(cos(ta_)) +
                                       I(log((sl_+1)/1000)):I(cos(ta_)) +
                                       #scale(for.age_start):I(log(sl_+1)) + 
                                       #  prop_forest_start:I(log(sl_+1)) +
                                       # prop_veg_start:I(log(sl_+1)) + 
                                       # scale(for.age_end) + scale(for.age_end):scale(for.age_avail) +
                                       prop_forest_end + prop_forest_end:prop_forest_avail +
                                       prop_veg_end + prop_veg_end:prop_veg_avail +
                                       I(log(ts_fires_end+1)) + I(log(ts_fires_end+1)):I(log(ts_fires_avail+1)) +
                                       # I(log(ts_harv_end+1)) + I(log(ts_harv_end+1)):I(log(ts_harv_avail+1)) +
                                       # I(log((dist_paved_end_adj/1000)+1)) + I(log((dist_paved_end_adj/1000)+1)):I(log((dist_paved_avail/1000)+1)) +
                                       #  I(log((dist_unpaved_end_adj/1000)+1)) + I(log((dist_unpaved_end_adj/1000)+1)):I(log((dist_unpaved_avail/1000)+1)) +
                                       I(log(((dist_road_end+1)/1000))) + #I(log((dist_road_end_adj/1000)+1)):I(log((dist_road_avail/1000)+1)) +
                                       I(log((dist_lf_other_end+1)/1000)) +# I(log((dist_lf_other_end_adj/1000)+1)):I(log((dist_lf_other_avail/1000)+1)) +
                                       #I(log((dist_anthro_end_adj/1000)+1)) + I(log((dist_anthro_end_adj/1000)+1)):I(log((dist_anthro_avail/1000)+1)) +
                                       I(log((dist_placer_end+1)/1000)) + I(log((dist_quartz_end+1)/1000)) +
                                       #I(log((dist_mining_end+1)/1000)) +# I(log((dist_mining_end_adj/1000)+1)):I(log((dist_mining_avail/1000)+1)) +
                                       I(log((dist_otherdisturbance_end+1)/1000)) + #I(log((dist_otherdisturbance_end_adj/1000)+1)):I(log((dist_otherdisturbance_avail/1000)+1)) +
                                       
                                       (1|indiv_step_id) +
                                       (0 + I(log((sl_ +1)/1000))|id) +
                                       #(0 + I(cos(ta_))|id) +
                                       (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                                       #(0 + scale(for.age_start):I(log(sl_+1))|id) + 
                                       #  (0 + prop_forest_start:I(log(sl_+1))|id) +
                                       # (0 + prop_veg_start:I(log(sl_+1))|id) + 
                                       # (0 + scale(for.age_end)|id) + 
                                       (0 + prop_forest_end|id) +
                                       (0 + prop_veg_end|id) +
                                       (0 + (I(log(ts_fires_end+1)))|id) +
                                       # (0 + (I(log(ts_harv_end+1)))|id) + 
                                       # (0 + I(log((dist_paved_end_adj/1000)+1))|id) + 
                                       # (0 + I(log((dist_unpaved_end_adj/1000)+1))|id) + 
                                       (0 + I(log((dist_road_end+1)/1000))|id) + 
                                       (0 + I(log((dist_lf_other_end+1)/1000))|id) +
                                       #(0 + I(log((dist_anthro_end_adj/1000)+1))|id) + 
                                       (0 + I(log((dist_placer_end+1)/1000))|id) +
                                       (0 + I(log((dist_quartz_end+1)/1000))|id) +
                                       #(0 + I(log((dist_mining_end+1)/1000))|id) + 
                                       (0 + I(log((dist_otherdisturbance_end+1)/1000))|id) + 
                                       
                                       (1|year),
                                     family = poisson(), data = closedisturbsummer[,`:=`(year = as.factor(year), 
                                                                                         indiv_step_id = as.factor(indiv_step_id),
                                                                                         id = as.factor(id))],
                                     map= list(theta = factor(c(NA,1:11))),
                                     start = list(theta =c(log(1000), seq(0,0, length.out = 11))), 
                                     verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(summer.m.closedisturbance)
# save model output
saveRDS(summer.m.closedisturbance, "summer.m.closedisturbance.mod_lf20poly20.RDS")

summary(summer.m.closedisturbancepq)
# save model output
saveRDS(summer.m.closedisturbancepq, "summer.m.closedisturbancepq.mod_lf20poly20.RDS")

summer.m.closedisturbance.indiv <- coef(summer.m.closedisturbancepq)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(summer.m.closedisturbance.indiv)[term!='(Intercept)']
pop <- setDT(tidy(summer.m.closedisturbancepq, effects = 'fixed'))


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

#Looks great, moving on to dropping dist_anthro from summer dataset - model ran with placer and quartz

gc()
summer.m.fardisturbancepq <- glmmTMB(case_ ~ -1 +
                                     I(log((sl_+1)/1000)) +
                                     #I(cos(ta_)) +
                                     I(log((sl_+1)/1000)):I(cos(ta_)) +
                                     #scale(for.age_start):I(log(sl_+1)) + 
                                     # prop_forest_start:I(log(sl_+1)) +
                                     #prop_veg_start:I(log(sl_+1)) + 
                                     # scale(for.age_end) + scale(for.age_end):scale(for.age_avail) +
                                     prop_forest_end + prop_forest_end:prop_forest_avail +
                                     prop_veg_end + prop_veg_end:prop_veg_avail +
                                     I(log(ts_fires_end+1)) + I(log(ts_fires_end+1)):I(log(ts_fires_avail+1)) +
                                     # I(log(ts_harv_end+1)) + I(log(ts_harv_end+1)):I(log(ts_harv_avail+1)) +
                                     # I(log((dist_paved_end_adj/1000)+1)) + I(log((dist_paved_end_adj/1000)+1)):I(log((dist_paved_avail/1000)+1)) +
                                     #  I(log((dist_unpaved_end_adj/1000)+1)) + I(log((dist_unpaved_end_adj/1000)+1)):I(log((dist_unpaved_avail/1000)+1)) +
                                     # I(log(((dist_road_end_adj+1)/1000))) + #I(log((dist_road_end_adj/1000)+1)):I(log((dist_road_avail/1000)+1)) +
                                     # I(log((dist_lf_other_end_adj+1)/1000)) + I(log((dist_lf_other_end_adj/1000)+1)):I(log((dist_lf_other_avail/1000)+1)) +
                                     #I(log((dist_anthro_end_adj/1000)+1)) + I(log((dist_anthro_end_adj/1000)+1)):I(log((dist_anthro_avail/1000)+1)) +
                                     I(log((dist_placer_end+1)/1000)) + I(log((dist_quartz_end+1)/1000)) +
                                     # I(log((dist_mining_end_adj+1)/1000)) +# I(log((dist_mining_end_adj/1000)+1)):I(log((dist_mining_avail/1000)+1)) +
                                     I(log((dist_otherdisturbance_end_adj+1)/1000)) + #I(log((dist_anthrodisturbance_end_adj+1)/1000)):I(log(((dist_anthrodisturbance_avail+1)/1000))) +
                                     
                                     (1|indiv_step_id) +
                                     (0 + I(log((sl_ +1)/1000))|id) +
                                     #(0 + I(cos(ta_))|id) +
                                     (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                                     #(0 + scale(for.age_start):I(log(sl_+1))|id) + 
                                     #  (0 + prop_forest_start:I(log(sl_+1))|id) +
                                     # (0 + prop_veg_start:I(log(sl_+1))|id) + 
                                     # (0 + scale(for.age_end)|id) + 
                                     (0 + prop_forest_end|id) +
                                     (0 + prop_veg_end|id) +
                                     (0 + (I(log(ts_fires_end+1)))|id) +
                                     #(0 + (I(log(ts_harv_end+1)))|id) + 
                                     #(0 + I(log((dist_paved_end_adj/1000)+1))|id) + 
                                     # (0 + I(log((dist_unpaved_end_adj/1000)+1))|id) + 
                                     # (0 + I(log((dist_road_end_adj+1)/1000))|id) + 
                                     # (0 + I(log((dist_lf_other_end_adj+1)/1000))|id) +
                                     #(0 + I(log((dist_anthro_end_adj/1000)+1))|id) + 
                                     (0 + I(log((dist_placer_end+1)/1000))|id) +
                                     (0 + I(log((dist_quartz_end+1)/1000))|id) +
                                     # (0 + I(log((dist_mining_end_adj+1)/1000))|id) + 
                                     ##(0 + I(log((dist_anthrodisturbance_end_adj+1)/1000))|id) + 
                                     
                                     (1|year),
                                   family = poisson(), data = dat.issa.10.summer[,`:=`(year = as.factor(year), 
                                                                                       indiv_step_id = as.factor(indiv_step_id),
                                                                                       id = as.factor(id))],
                                   map= list(theta = factor(c(NA,1:8))),
                                   start = list(theta =c(log(1000), seq(0,0, length.out = 8))), 
                                   verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)

summary(summer.m.fardisturbance)
# save model output
saveRDS(summer.m.fardisturbance, "summer.m.fardisturbance.mod_lf20poly20.RDS")

summary(summer.m.fardisturbancepq)
# save model output
saveRDS(summer.m.fardisturbancepq, "summer.m.fardisturbancepq.mod_lf20poly20.RDS")


summer.m.fardisturbance.indiv <- coef(summer.m.fardisturbancepq)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(summer.m.fardisturbance.indiv)[term!='(Intercept)']
pop <- setDT(tidy(summer.m.fardisturbancepq, effects = 'fixed'))


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
  
  
#Model looks good. Now to go back and try and parse apart mines into placer and quartz, then re-reun each of the four models  

---------------------------------------------

#Running models with lf and polys separate by season for the far model (where the RE is dropped)
  
  #Start with winter

setDT(dat.issa.10.winter) #Ensure the data is in data.table format
  
dat.issa.10.winter[, dist_polydisturbance_end := pmin(dist_anthro_end, dist_harv_end, dist_placer_end, dist_quartz_end, na.rm = T)]  
names(dat.issa.10.winter) #create new polydisturbance variable as minimum vector value of each of above

gc()
winter.m.fardisturbancepolylf <- glmmTMB(case_ ~ -1 +
                                       I(log((sl_+1)/1000)) +
                                       #I(cos(ta_)) +
                                       I(log((sl_+1)/1000)):I(cos(ta_)) +
                                       #scale(for.age_start):I(log(sl_+1)) + 
                                       # prop_forest_start:I(log(sl_+1)) +
                                       #prop_veg_start:I(log(sl_+1)) + 
                                       # scale(for.age_end) + scale(for.age_end):scale(for.age_avail) +
                                       prop_forest_end + prop_forest_end:prop_forest_avail +
                                       prop_veg_end + prop_veg_end:prop_veg_avail +
                                       I(log(ts_fires_end+1)) + I(log(ts_fires_end+1)):I(log(ts_fires_avail+1)) +
                                       # I(log(ts_harv_end+1)) + I(log(ts_harv_end+1)):I(log(ts_harv_avail+1)) +
                                       # I(log((dist_paved_end_adj/1000)+1)) + I(log((dist_paved_end_adj/1000)+1)):I(log((dist_paved_avail/1000)+1)) +
                                       # I(log((dist_unpaved_end_adj/1000)+1)) + I(log((dist_unpaved_end_adj/1000)+1)):I(log((dist_unpaved_avail/1000)+1)) +
                                       # I(log(((dist_road_end_adj+1)/1000))) + #I(log((dist_road_end_adj/1000)+1)):I(log((dist_road_avail/1000)+1)) +
                                       # I(log((dist_lf_other_end_adj+1)/1000)) + I(log((dist_lf_other_end_adj/1000)+1)):I(log((dist_lf_other_avail/1000)+1)) +
                                       #I(log((dist_anthro_end_adj/1000)+1)) + I(log((dist_anthro_end_adj/1000)+1)):I(log((dist_anthro_avail/1000)+1)) +
                                       #I(log((dist_placer_end+1)/1000)) + I(log((dist_quartz_end+1)/1000)) +
                                       # I(log((dist_mining_end_adj+1)/1000)) +# I(log((dist_mining_end_adj/1000)+1)):I(log((dist_mining_avail/1000)+1)) +
                                       #I(log((dist_otherdisturbance_end_adj+1)/1000)) + #I(log((dist_anthrodisturbance_end_adj+1)/1000)):I(log(((dist_anthrodisturbance_avail+1)/1000))) +
                                        I(log((dist_polydisturbance_end+1)/1000)) +
                                        I(log((dist_lf_end+1)/1000)) +
                                         
                                       (1|indiv_step_id) +
                                       (0 + I(log((sl_ +1)/1000))|id) +
                                       #(0 + I(cos(ta_))|id) +
                                       (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                                       #(0 + scale(for.age_start):I(log(sl_+1))|id) + 
                                       #  (0 + prop_forest_start:I(log(sl_+1))|id) +
                                       # (0 + prop_veg_start:I(log(sl_+1))|id) + 
                                       # (0 + scale(for.age_end)|id) + 
                                       (0 + prop_forest_end|id) +
                                       (0 + prop_veg_end|id) +
                                       (0 + (I(log(ts_fires_end+1)))|id) +
                                       #(0 + (I(log(ts_harv_end+1)))|id) + 
                                       #(0 + I(log((dist_paved_end_adj/1000)+1))|id) + 
                                       # (0 + I(log((dist_unpaved_end_adj/1000)+1))|id) + 
                                       # (0 + I(log((dist_road_end_adj+1)/1000))|id) + 
                                       # (0 + I(log((dist_lf_other_end_adj+1)/1000))|id) +
                                       #(0 + I(log((dist_anthro_end_adj/1000)+1))|id) + 
                                       # (0 + I(log((dist_mining_end_adj+1)/1000))|id) + 
                                       #(0 + I(log((dist_placer_end+1)/1000))|id) +
                                       #(0 + I(log((dist_quartz_end+1)/1000))|id) +
                                       (0 + I(log((dist_polydisturbance_end+1)/1000))|id) +
                                       (0 + I(log((dist_lf_end+1)/1000))|id) +
                                       ##(0 + I(log((dist_anthrodisturbance_end_adj+1)/1000))|id) + 
                                       
                                       (1|year),
                                     family = poisson(), data = dat.issa.10.winter[,`:=`(year = as.factor(year), 
                                                                                         indiv_step_id = as.factor(indiv_step_id),
                                                                                         id = as.factor(id))],
                                     map= list(theta = factor(c(NA,1:8))),
                                     start = list(theta =c(log(1000), seq(0,0, length.out = 8))), 
                                     verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)



summary(winter.m.fardisturbancepolylf)
# save model output
saveRDS(winter.m.fardisturbancepolylf, "winter.m.fardisturbancepolylf.mod_lf20poly20.RDS")

winter.m.fardisturbancepolylf.indiv <- coef(winter.m.fardisturbancepolylf)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(winter.m.fardisturbancepolylf.indiv)[term!='(Intercept)']
pop <- setDT(tidy(winter.m.fardisturbancepolylf, effects = 'fixed'))


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
  

ggplot(data = indivs[(term %like% 'dist')], aes(x = term, y = estimate)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) +
  geom_point(
    size = 1.5,
    alpha = .2,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  #coord_cartesian(xlim = c(1.2, 2.9), clip = "off") +
  coord_flip() 

ggplot(data = indivs[!(term %like% 'dist')], aes(x = term, y = estimate)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) +
  geom_point(
    size = 1.5,
    alpha = .2,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  #coord_cartesian(xlim = c(1.2, 2.9), clip = "off") +
  coord_flip() 





#Moving on to the summer dataset

setDT(dat.issa.10.summer) #Ensure the data is in data.table format

dat.issa.10.summer[, dist_polydisturbance_end := pmin(dist_anthro_end, dist_harv_end, dist_placer_end, dist_quartz_end, na.rm = T)]  
names(dat.issa.10.summer) #create new polydisturbance variable as minimum vector value of each of above

gc()
summer.m.fardisturbancepolylf <- glmmTMB(case_ ~ -1 +
                                           I(log((sl_+1)/1000)) +
                                           #I(cos(ta_)) +
                                           I(log((sl_+1)/1000)):I(cos(ta_)) +
                                           #scale(for.age_start):I(log(sl_+1)) + 
                                           # prop_forest_start:I(log(sl_+1)) +
                                           #prop_veg_start:I(log(sl_+1)) + 
                                           # scale(for.age_end) + scale(for.age_end):scale(for.age_avail) +
                                           prop_forest_end + prop_forest_end:prop_forest_avail +
                                           prop_veg_end + prop_veg_end:prop_veg_avail +
                                           I(log(ts_fires_end+1)) + I(log(ts_fires_end+1)):I(log(ts_fires_avail+1)) +
                                           # I(log(ts_harv_end+1)) + I(log(ts_harv_end+1)):I(log(ts_harv_avail+1)) +
                                           # I(log((dist_paved_end_adj/1000)+1)) + I(log((dist_paved_end_adj/1000)+1)):I(log((dist_paved_avail/1000)+1)) +
                                           # I(log((dist_unpaved_end_adj/1000)+1)) + I(log((dist_unpaved_end_adj/1000)+1)):I(log((dist_unpaved_avail/1000)+1)) +
                                           # I(log(((dist_road_end_adj+1)/1000))) + #I(log((dist_road_end_adj/1000)+1)):I(log((dist_road_avail/1000)+1)) +
                                           # I(log((dist_lf_other_end_adj+1)/1000)) + I(log((dist_lf_other_end_adj/1000)+1)):I(log((dist_lf_other_avail/1000)+1)) +
                                           #I(log((dist_anthro_end_adj/1000)+1)) + I(log((dist_anthro_end_adj/1000)+1)):I(log((dist_anthro_avail/1000)+1)) +
                                           #I(log((dist_placer_end+1)/1000)) + I(log((dist_quartz_end+1)/1000)) +
                                           # I(log((dist_mining_end_adj+1)/1000)) +# I(log((dist_mining_end_adj/1000)+1)):I(log((dist_mining_avail/1000)+1)) +
                                           #I(log((dist_otherdisturbance_end_adj+1)/1000)) + #I(log((dist_anthrodisturbance_end_adj+1)/1000)):I(log(((dist_anthrodisturbance_avail+1)/1000))) +
                                           I(log((dist_polydisturbance_end+1)/1000)) +
                                           I(log((dist_lf_end+1)/1000)) +
                                           
                                           (1|indiv_step_id) +
                                           (0 + I(log((sl_ +1)/1000))|id) +
                                           #(0 + I(cos(ta_))|id) +
                                           (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                                           #(0 + scale(for.age_start):I(log(sl_+1))|id) + 
                                           #  (0 + prop_forest_start:I(log(sl_+1))|id) +
                                           # (0 + prop_veg_start:I(log(sl_+1))|id) + 
                                           # (0 + scale(for.age_end)|id) + 
                                           (0 + prop_forest_end|id) +
                                           (0 + prop_veg_end|id) +
                                           (0 + (I(log(ts_fires_end+1)))|id) +
                                           #(0 + (I(log(ts_harv_end+1)))|id) + 
                                           #(0 + I(log((dist_paved_end_adj/1000)+1))|id) + 
                                           # (0 + I(log((dist_unpaved_end_adj/1000)+1))|id) + 
                                           # (0 + I(log((dist_road_end_adj+1)/1000))|id) + 
                                           # (0 + I(log((dist_lf_other_end_adj+1)/1000))|id) +
                                           #(0 + I(log((dist_anthro_end_adj/1000)+1))|id) + 
                                           # (0 + I(log((dist_mining_end_adj+1)/1000))|id) + 
                                           #(0 + I(log((dist_placer_end+1)/1000))|id) +
                                           #(0 + I(log((dist_quartz_end+1)/1000))|id) +
                                           (0 + I(log((dist_polydisturbance_end+1)/1000))|id) +
                                           (0 + I(log((dist_lf_end+1)/1000))|id) +
                                           ##(0 + I(log((dist_anthrodisturbance_end_adj+1)/1000))|id) + 
                                           
                                           (1|year),
                                         family = poisson(), data = dat.issa.10.summer[,`:=`(year = as.factor(year), 
                                                                                             indiv_step_id = as.factor(indiv_step_id),
                                                                                             id = as.factor(id))],
                                         map= list(theta = factor(c(NA,1:8))),
                                         start = list(theta =c(log(1000), seq(0,0, length.out = 8))), 
                                         verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)



summary(summer.m.fardisturbancepolylf)
# save model output
saveRDS(summer.m.fardisturbancepolylf, "summer.m.fardisturbancepolylf.mod_lf20poly20.RDS")

summer.m.fardisturbancepolylf.indiv <- coef(summer.m.fardisturbancepolylf)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(summer.m.fardisturbancepolylf.indiv)[term!='(Intercept)']
pop <- setDT(tidy(summer.m.fardisturbancepolylf, effects = 'fixed'))


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

ggplot(data = indivs[(term %like% 'dist')], aes(x = term, y = estimate)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) +
  geom_point(
    size = 1.5,
    alpha = .2,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  #coord_cartesian(xlim = c(1.2, 2.9), clip = "off") +
  coord_flip() 

ggplot(data = indivs[!(term %like% 'dist')], aes(x = term, y = estimate)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) +
  geom_point(
    size = 1.5,
    alpha = .2,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  #coord_cartesian(xlim = c(1.2, 2.9), clip = "off") +
  coord_flip() 



#Looking solid. Now checking to see if the raw data of dist_polydisturb and dist_lf looks good
setDT(dat.issa.10) #Ensure the data is in data.table format

dat.issa.10[, dist_polydisturbance_end := pmin(dist_anthro_end, dist_harv_end, dist_placer_end, dist_quartz_end, na.rm = T)]  
names(dat.issa.10) #create new polydisturbance variable as minimum vector value of each of above

dat.issa.10[,dist_polydisturbance_avail:= mean(dist_polydisturbance_end, na.rm = T), by = indiv_step_id]

new_dt <- dat.issa.10[, .(dist_polydisturbance_avail, dist_lf_avail)]

data_long <- reshape2::melt(new_dt)
setDT(data_long)

mean(new_dt$dist_polydisturbance_avail)
mean(new_dt$dist_lf_avail)

require(ggplot2)
require(ggdist)
ggplot(data = data_long, aes(x = variable, y = value)) + 
  ggdist::stat_halfeye(
    adjust = .5, 
    width = .6, 
    .width = 0, 
    justification = -.3, 
    point_colour = NA) + 
  geom_boxplot(
    width = .25, 
    outlier.shape = NA
  ) +
  geom_point(
    size = 1.5,
    alpha = .2,
    position = position_jitter(
      seed = 1, width = .1
    )
  ) + 
  #coord_cartesian(xlim = c(1.2, 2.9), clip = "off") +
  coord_flip() 

  



# Checking some additional correlations 
unique(dat.issa.10$prov)  
  
filtered_dt <- dat.issa.10[prov == 'yt', ]  
  
cor(filtered_dt$dist_placer_end, filtered_dt$dist_quartz_end, use = "complete.obs")  
cor(dat.issa.10$dist_placer_end, dat.issa.10$dist_quartz_end, use = "complete.obs")   

  
cor_df <- filtered_dt %>% select('dist_paved_end', 'dist_unpaved_end', 'dist_lf_other_end', 'dist_placer_end', 'dist_quartz_end', 'dist_harv_end', 'dist_anthro_end') #create new df with the variables I want
library(corrplot)
M = cor(cor_df, use = "complete.obs") #produce plot of correlations
corrplot(M, method = 'number')

cor_df2 <- dat.issa.10 %>% select('dist_road_end', 'dist_mining_end', 'dist_anthro_end', 'dist_lf_other_end')
N = cor(cor_df2, use = "complete.obs") #produce plot of correlations
corrplot(N, method = 'number')  
  