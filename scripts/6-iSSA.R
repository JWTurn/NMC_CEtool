# === iSSA -------------------------------------
# Julie Turner
# started 14 August 2024

#### Packages ####
libs <- c('data.table', 'dplyr', 'glmmTMB', 'broom.mixed', 'performance')
lapply(libs, require, character.only = TRUE)

# my functions
lapply(dir('R', '*.R', full.names = TRUE), source)

### Input data ----
raw <-  file.path('data', 'raw')
derived <- file.path('data', 'derived')

dat <- targets::tar_read(stepID)


### explore ----
dat[case_==TRUE,.(quantile(sl_)), by = .(prov)]

dat[, dt_num := as.numeric(dt_)]
dat[case_==TRUE,.(median(sl_/dt_num)), by = .(prov)]


dat[ts_fires_end > 10000, ts_fires_end:= 25] # setting this to 20 for now because most were in 1990-2000, but I'll need to figure out something later


dat[,.(distto_paved = mean(distpaved_end, na.rm = T), distto_unpaved = mean(distunpaved_end, na.rm = T),
       distto_lfother = mean(distlf_other_end, na.rm = T), distto_anthro = mean(distanthro_end, na.rm =T),
       distto_mining = mean(distmining_end, na.rm = T))]

dat[prov == 'yt', .(distto_placer = mean(distplacer_end, na.rm = T), distto_quartz = mean(distquartz_end, na.rm = T))]

dat[,.(distto_paved = min(distpaved_end, na.rm = T), distto_unpaved = min(distunpaved_end, na.rm = T),
       distto_lfother = min(distlf_other_end, na.rm = T), distto_anthro = min(distanthro_end, na.rm =T),
       distto_mining = min(distmining_end, na.rm = T))]

dat[prov == 'yt', .(distto_placer = min(distplacer_end, na.rm = T), distto_quartz = min(distquartz_end, na.rm = T))]


dat[,.(water = mean(prop_water_end, na.rm = T), snow = mean(prop_snow_end, na.rm = T),
       rock = mean(prop_rock_end, na.rm = T), barren = mean(prop_barren_end, na.rm = T),
       bryoids = mean(prop_bryoids_end, na.rm = T), shrub = mean(prop_shrub_end, na.rm = T),
       wetland = mean(prop_wetland_end, na.rm = T), wettreed = mean(prop_wettreed_end, na.rm = T), 
       herbs = mean(prop_herbs_end, na.rm = T), needle =mean(prop_needleleaf_end, na.rm = T),
       decid = mean(prop_deciduous_end, na.rm = T),mixed = mean(prop_mixed_end, na.rm = T),
       disturbance = mean(prop_disturbance_end, na.rm = T), for.age = mean(for.age_end, na.rm = T))]


quantile(dat$ts_fires_end, probs = c(0.01, 0.05, 0.1, 0.15, 0.25, 0.5, 0.75, 1))

quantile(dat$ts_harv_end, probs = c(0.01, 0.05, 0.1, 0.15, 0.25, 0.5, 0.75, 1))

dat$prop_veg_end = dat$prop_bryoids_end + dat$prop_herbs_end + dat$prop_shrub_end + dat$prop_wetland_end
mean(dat$prop_veg_end, na.rm = T)
dat$prop_veg_start = dat$prop_bryoids_start + dat$prop_herbs_start + dat$prop_shrub_start + dat$prop_wetland_start
mean(dat$prop_veg_start, na.rm = T)

dat$prop_forest_end = dat$prop_wettreed_end + dat$prop_needleleaf_end + dat$prop_deciduous_end + dat$prop_mixed_end
mean(dat$prop_forest_end, na.rm = T)
dat$prop_forest_start = dat$prop_wettreed_start + dat$prop_needleleaf_start + dat$prop_deciduous_start + dat$prop_mixed_start
mean(dat$prop_forest_start, na.rm = T)


dat.issa <- dat[,.(id, area, prov, x1_, y1_, x2_, y2_, sl_, ta_, burst_, t1_, t2_, dt_,
                   case_, step_id_, indiv_step_id, year, 
                   prop_veg_start, prop_veg_end,
                   prop_forest_start, prop_forest_end,
                   prop_needleleaf_start, prop_needleleaf_end,
                   for.age_start, for.age_end,
                   distpaved_start, distpaved_end, distunpaved_start, distunpaved_end, 
                   distroad_end = pmin(distpaved_end, distunpaved_end, na.rm = T),
                   distlf_other_start, distlf_other_end,
                   distlf_end = pmin(distpaved_end, distunpaved_end, distlf_other_end, na.rm = T),
                   distanthro_start, distanthro_end, distmining_start, distmining_end,
                   distplacer_start, distplacer_end, distquartz_start, distquartz_end,
                   distanthrodistubance_start = pmin(distpaved_start, distunpaved_start, distlf_other_start, distanthro_start,
                                                     distplacer_start, distquartz_start, na.rm = T), 
                   distanthrodistubance_end = pmin(distpaved_end, distunpaved_end, distlf_other_end, distanthro_end,
                                                     distplacer_end, distquartz_end, na.rm = T), 
                   prop_disturbance_start, prop_disturbance_end, for.age_start, for.age_end,
                   ts_harv_start, ts_harv_end, ts_fires_start, ts_fires_end)]

sumdat <- dat.issa[,.(prov, year, prop_veg_start, prop_veg_end,
            for.age_start, for.age_end, prop_forest_start, prop_forest_end,
            distpaved_start, distpaved_end, distunpaved_start, distunpaved_end, 
            distroad_end, distlf_other_start, distlf_other_end, distlf_end,
            distanthro_start, distanthro_end, distmining_start, distmining_end,
            distplacer_start, distplacer_end, distquartz_start, distquartz_end,
            distanthrodistubance_start, distanthrodistubance_end,
            prop_disturbance_start, prop_disturbance_end,
            ts_harv_start, ts_harv_end, ts_fires_start, ts_fires_end)] %>%
  group_by(prov) %>%
  summarise_all(mean, na.rm = T)



# full iSSA ----
m.full <- glmmTMB(case_ ~ -1 +
                   I(log(sl_+1)) +
                   I(cos(ta_)) +
                   I(log(sl_+1)):I(cos(ta_)) +
                   for.age_start:I(log(sl_+1)) + 
                   prop_veg_start:I(log(sl_+1)) + 
                   for.age_end +
                   prop_veg_end +
                   I(log(ts_fires_end+1)) + 
                   I(log(ts_harv_end+1)) + 
                   I(log((distpaved_end/1000)+1)) + 
                    I(log((distunpaved_end/1000)+1)) + 
                    I(log((distlf_other_end/1000)+1)) + 
                    I(log((distanthro_end/1000)+1)) + 
                  
                   (1|indiv_step_id) +
                   (0 + I(log(sl_ +1))|id) +
                   (0 + I(cos(ta_))|id) +
                   (0 + I(log(sl_+1)):I(cos(ta_))|id) +
                   (0 + for.age_start:I(log(sl_+1))|id) + 
                   (0 + prop_veg_start:I(log(sl_+1))|id) + 
                   (0 + for.age_end|id) + 
                   (0 + prop_veg_end|id) +
                   (0 + (I(log(ts_fires_end+1)))|id) +
                   (0 + (I(log(ts_harv_end+1)))|id) + 
                   (0 + I(log((distpaved_end/1000)+1))|id) + 
                   (0 + I(log((distunpaved_end/1000)+1))|id) + 
                   (0 + I(log((distlf_other_end/1000)+1))|id) +
                   (0 + I(log((distanthro_end/1000)+1))|id) + 
                  
                   (1|year),
                 family = poisson(), data = dat.issa[,`:=`(year = as.factor(year), 
                                                           indiv_step_id = as.factor(indiv_step_id),
                                                           id = as.factor(id))],
                 map= list(theta = factor(c(NA,1:14))),
                 start = list(theta =c(log(1000), seq(0,0, length.out = 14)))
)
## Doesn't converge


summary(m.full)
#saveRDS(m.nwt, file.path(derived, 'mod_selmove_nwt.RDS'))


# full RSF ----
m.rsf.full <- glmmTMB(case_ ~ -1 +
                    scale(for.age_end) +
                    prop_veg_end +
                    I(log(ts_fires_end+1)) + 
                    I(log(ts_harv_end+1)) + 
                    I(log((distpaved_end/1000)+1)) + 
                    I(log((distunpaved_end/1000)+1)) + 
                    I(log((distlf_other_end/1000)+1)) + 
                    I(log((distanthro_end/1000)+1)) + 
                    
                    (1|id) +
                    (0 + scale(for.age_end)|id) + 
                    (0 + prop_veg_end|id) +
                    (0 + (I(log(ts_fires_end+1)))|id) +
                    (0 + (I(log(ts_harv_end+1)))|id) + 
                    (0 + I(log((distpaved_end/1000)+1))|id) + 
                    (0 + I(log((distunpaved_end/1000)+1))|id) + 
                    (0 + I(log((distlf_other_end/1000)+1))|id) +
                    (0 + I(log((distanthro_end/1000)+1))|id) + 
                    
                    (1|year),
                  family = binomial(), data = dat.issa[,`:=`(year = as.factor(year),
                                                            id = as.factor(id))],
                  map= list(theta = factor(c(NA,1:9))),
                  start = list(theta =c(log(1000), seq(0,0, length.out = 9)))
)

summary(m.rsf.full)
check_model(m.rsf.full)
saveRDS(m.rsf.full, file.path(derived, 'mods', 'rsf_full.RDS'))

# combine roads iSSA ----
m.roads <- glmmTMB(case_ ~ -1 +
                    I(log(sl_+1)) +
                    I(cos(ta_)) +
                    I(log(sl_+1)):I(cos(ta_)) +
                    for.age_start:I(log(sl_+1)) + 
                    prop_veg_start:I(log(sl_+1)) + 
                    for.age_end +
                    prop_veg_end +
                    I(log(ts_fires_end+1)) + 
                    # I(log(ts_harv_end+1)) + 
                    # I(log((distpaved_end/1000)+1)) + 
                    # I(log((distunpaved_end/1000)+1)) + 
                     I(log((distroad_end/1000)+1)) + 
                    I(log((distlf_other_end/1000)+1)) + 
                    I(log((distanthro_end/1000)+1)) + 
                    
                    (1|indiv_step_id) +
                    (0 + I(log(sl_ +1))|id) +
                    (0 + I(cos(ta_))|id) +
                    (0 + I(log(sl_+1)):I(cos(ta_))|id) +
                    (0 + for.age_start:I(log(sl_+1))|id) + 
                    (0 + prop_veg_start:I(log(sl_+1))|id) + 
                    (0 + for.age_end|id) + 
                    (0 + prop_veg_end|id) +
                    (0 + (I(log(ts_fires_end+1)))|id) +
                    # (0 + (I(log(ts_harv_end+1)))|id) + 
                    # (0 + I(log((distpaved_end/1000)+1))|id) + 
                    # (0 + I(log((distunpaved_end/1000)+1))|id) + 
                     (0 + I(log((distroad_end/1000)+1))|id) + 
                     (0 + I(log((distlf_other_end/1000)+1))|id) +
                    (0 + I(log((distanthro_end/1000)+1))|id) + 
                    
                    (1|year),
                  family = poisson(), data = dat.issa[,`:=`(year = as.factor(year), 
                                                            indiv_step_id = as.factor(indiv_step_id),
                                                            id = as.factor(id))],
                  map= list(theta = factor(c(NA,1:12))),
                  start = list(theta =c(log(1000), seq(0,0, length.out = 12)))
)



summary(m.roads)


# iSSA only near disturbance ----
# filter to only those near a disturbance
dat.issa.disturbance <- dat.issa[indiv_step_id %in% (dat.issa[case_==T & (distanthro_start < 30000 | distanthro_end < 30000)]$indiv_step_id)]
m.disturbance <- glmmTMB(case_ ~ -1 +
                    I(log(sl_+1)) +
                    I(cos(ta_)) +
                    I(log(sl_+1)):I(cos(ta_)) +
                    for.age_start:I(log(sl_+1)) + 
                    prop_veg_start:I(log(sl_+1)) + 
                    for.age_end +
                    prop_veg_end +
                    I(log(ts_fires_end+1)) + 
                    I(log(ts_harv_end+1)) + 
                    I(log((distpaved_end/1000)+1)) + 
                    I(log((distunpaved_end/1000)+1)) + 
                    I(log((distlf_other_end/1000)+1)) + 
                    I(log((distanthro_end/1000)+1)) + 
                    
                    (1|indiv_step_id) +
                    (0 + I(log(sl_ +1))|id) +
                    (0 + I(cos(ta_))|id) +
                    (0 + I(log(sl_+1)):I(cos(ta_))|id) +
                    (0 + for.age_start:I(log(sl_+1))|id) + 
                    (0 + prop_veg_start:I(log(sl_+1))|id) + 
                    (0 + for.age_end|id) + 
                    (0 + prop_veg_end|id) +
                    (0 + (I(log(ts_fires_end+1)))|id) +
                    (0 + (I(log(ts_harv_end+1)))|id) + 
                    (0 + I(log((distpaved_end/1000)+1))|id) + 
                    (0 + I(log((distunpaved_end/1000)+1))|id) + 
                    (0 + I(log((distlf_other_end/1000)+1))|id) +
                    (0 + I(log((distanthro_end/1000)+1))|id) + 
                    
                    (1|year),
                  family = poisson(), data = dat.issa.disturbance[,`:=`(year = as.factor(year), 
                                                            indiv_step_id = as.factor(indiv_step_id),
                                                            id = as.factor(id))],
                  map= list(theta = factor(c(NA,1:14))),
                  start = list(theta =c(log(1000), seq(0,0, length.out = 14)))
)

summary(m.disturbance)
check_model(m.disturbance)

gc()
dat.issa.mines <- dat.issa[prov == 'yt' & indiv_step_id %in% (dat.issa[case_==T & (distmining_start < 30000 | distmining_end < 30000)]$indiv_step_id)]

m.disturbance2 <- glmmTMB(case_ ~ -1 +
                           I(log(sl_+1)) +
                           I(cos(ta_)) +
                           I(log(sl_+1)):I(cos(ta_)) +
                           prop_needleleaf_start:I(log(sl_+1)) + 
                           prop_veg_start:I(log(sl_+1)) + 
                           prop_needleleaf_end +
                           prop_veg_end +
                           I(log(ts_fires_end+1)) + 
                           #I(log(ts_harv_end+1)) + 
                           I(log((distpaved_end/1000)+1)) + 
                           I(log((distunpaved_end/1000)+1)) + 
                           I(log((distlf_other_end/1000)+1)) + 
                           #I(log((distanthro_end/1000)+1)) + 
                            I(log((distplacer_end/1000)+1)) +
                            I(log((distquartz_end/1000)+1)) +
                           
                           (1|indiv_step_id) +
                           (0 + I(log(sl_ +1))|id) +
                           (0 + I(cos(ta_))|id) +
                           (0 + I(log(sl_+1)):I(cos(ta_))|id) +
                           (0 + prop_needleleaf_start:I(log(sl_+1))|id) + 
                           (0 + prop_veg_start:I(log(sl_+1))|id) + 
                           (0 + prop_needleleaf_end|id) + 
                           (0 + prop_veg_end|id) +
                           (0 + (I(log(ts_fires_end+1)))|id) +
                           #(0 + (I(log(ts_harv_end+1)))|id) + 
                           (0 + I(log((distpaved_end/1000)+1))|id) + 
                           (0 + I(log((distunpaved_end/1000)+1))|id) + 
                           (0 + I(log((distlf_other_end/1000)+1))|id) +
                           #(0 + I(log((distanthro_end/1000)+1))|id) +
                            (0 + I(log((distplacer_end/1000)+1))|id) +
                            (0 + I(log((distquartz_end/1000)+1))|id) +
                            
                           (1|year),
                         family = poisson(), data = dat.issa.mines[,`:=`(year = as.factor(year), 
                                                                               indiv_step_id = as.factor(indiv_step_id),
                                                                               id = as.factor(id))],
                         map= list(theta = factor(c(NA,1:14))),
                         start = list(theta =c(log(1000), seq(0,0, length.out = 14)))
)

summary(m.disturbance2)
check_model(m.disturbance2)

saveRDS(m.disturbance2, file.path(derived, 'mods', 'iSSA_yt_mines.RDS'))

