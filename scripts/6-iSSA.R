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


### explore ----
dat[case_==TRUE,.(quantile(sl_)), by = .(prov)]

dat[, dt_num := as.numeric(dt_)]
dat[case_==TRUE,.(median(sl_/dt_num)), by = .(prov)]


dat[ts_fires_end > 10000, ts_fires_end:= 25] # setting this to 20 for now because most were in 1990-2000, but I'll need to figure out something later


dat[,.(dist_to_paved = mean(dist_paved_end, na.rm = T), dist_to_unpaved = mean(dist_unpaved_end, na.rm = T),
       dist_to_lfother = mean(dist_lf_other_end, na.rm = T), dist_to_anthro = mean(dist_anthro_end, na.rm =T),
       dist_to_harv = mean(dist_harv_end, na.rm = T), dist_to_mining = mean(dist_mining_end, na.rm = T))]

dat[prov == 'yt', .(dist_to_placer = mean(dist_placer_end, na.rm = T), dist_to_quartz = mean(dist_quartz_end, na.rm = T))]

dat[,.(dist_to_paved = min(dist_paved_end, na.rm = T), dist_to_unpaved = min(dist_unpaved_end, na.rm = T),
       dist_to_lfother = min(dist_lf_other_end, na.rm = T), dist_to_anthro = min(dist_anthro_end, na.rm =T),
       dist_to_harv = min(dist_harv_end, na.rm = T), dist_to_mining = min(dist_mining_end, na.rm = T))]

dat[prov == 'yt', .(dist_to_placer = min(dist_placer_end, na.rm = T), dist_to_quartz = min(dist_quartz_end, na.rm = T))]


dat[,.(water = mean(prop_water_end, na.rm = T), snow = mean(prop_snow_end, na.rm = T),
       rock = mean(prop_rock_end, na.rm = T), barren = mean(prop_barren_end, na.rm = T),
       bryoids = mean(prop_bryoids_end, na.rm = T), shrub = mean(prop_shrub_end, na.rm = T),
       wetland = mean(prop_wetland_end, na.rm = T), wettreed = mean(prop_wettreed_end, na.rm = T), 
       herbs = mean(prop_herbs_end, na.rm = T), needle =mean(prop_needleleaf_end, na.rm = T),
       decid = mean(prop_deciduous_end, na.rm = T),mixed = mean(prop_mixed_end, na.rm = T),
       disturbance = mean(prop_disturbance_end, na.rm = T), for.age = mean(for.age_end, na.rm = T))]


quantile(dat$ts_fires_end, probs = c(0.01, 0.05, 0.1, 0.15, 0.25, 0.5, 0.75, 1))

quantile(dat$ts_harv_end, probs = c(0.01, 0.05, 0.1, 0.15, 0.25, 0.5, 0.75, 1))

dat$prop_veg_end = dat$prop_bryoids_end + dat$prop_herbs_end + dat$prop_shrub_end #+ dat$prop_wetland_end
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
                   dist_paved_start, dist_paved_end, dist_unpaved_start, dist_unpaved_end, 
                   dist_road_end = pmin(dist_paved_end, dist_unpaved_end, na.rm = T),
                   dist_lf_other_start, dist_lf_other_end,
                   dist_lf_end = pmin(dist_paved_end, dist_unpaved_end, dist_lf_other_end, na.rm = T),
                   dist_anthro_start, dist_anthro_end, dist_mining_start, dist_mining_end,
                   dist_placer_start, dist_placer_end, dist_quartz_start, dist_quartz_end,
                   dist_harv_start, dist_harv_end, 
                   dist_otherdisturbance_start = pmin(dist_paved_start, dist_unpaved_start, dist_lf_other_start, dist_anthro_start,
                                                       dist_harv_start, na.rm = T), 
                   dist_otherdisturbance_end = pmin(dist_paved_end, dist_unpaved_end, dist_lf_other_end, dist_anthro_end,
                                                     dist_harv_end, na.rm = T), 
                   dist_anthrodisturbance_start = pmin(dist_paved_start, dist_unpaved_start, dist_lf_other_start, dist_anthro_start,
                                                     dist_harv_start, dist_placer_start, dist_quartz_start, na.rm = T), 
                   dist_anthrodisturbance_end = pmin(dist_paved_end, dist_unpaved_end, dist_lf_other_end, dist_anthro_end,
                                                     dist_harv_end, dist_placer_end, dist_quartz_end, na.rm = T), 
                   #prop_disturbance_start, prop_disturbance_end,
                   for.age_start, for.age_end,
                   ts_harv_start, ts_harv_end, ts_fires_start, ts_fires_end)]

sumdat <- dat.issa[,.(prov, year, prop_veg_start, prop_veg_end,
            for.age_start, for.age_end, prop_forest_start, prop_forest_end,
            dist_paved_start, dist_paved_end, dist_unpaved_start, dist_unpaved_end, 
            dist_road_end, dist_lf_other_start, dist_lf_other_end, dist_lf_end,
            dist_anthro_start, dist_anthro_end, dist_mining_start, dist_mining_end,
            dist_placer_start, dist_placer_end, dist_quartz_start, dist_quartz_end,
            dist_harv_start, dist_harv_end,
            dist_otherdisturbance_start, dist_otherdisturbance_end,
            dist_anthrodisturbance_start, dist_anthrodisturbance_end,
            #prop_disturbance_start, prop_disturbance_end,
            ts_harv_start, ts_harv_end, ts_fires_start, ts_fires_end)] %>%
  group_by(prov) %>%
  summarise_all(mean, na.rm = T)

dat.issa[case_ ==TRUE,mean(prop_veg_end, na.rm = T), by = .(prov, year)]
dat.issa[case_ ==TRUE,mean(prop_forest_end, na.rm = T), by = .(prov, year)]
dat.issa[case_ ==TRUE,mean(prop_needleleaf_end, na.rm = T), by = .(prov, year)]
dat.issa[case_ ==TRUE,mean(for.age_end, na.rm = T), by = .(prov, year)]

# calc relative availability and and adjust distances so indicate that if so far from feature, it's not estimated as selection
dat.issa[,for.age_avail:= mean(for.age_end, na.rm = T), by = indiv_step_id]
dat.issa[,prop_veg_avail:= mean(prop_veg_end, na.rm = T), by = indiv_step_id]
dat.issa[,prop_forest_avail:= mean(prop_forest_end, na.rm = T), by = indiv_step_id]
dat.issa[,prop_needleleaf_avail:= mean(prop_needleleaf_end, na.rm = T), by = indiv_step_id]
#dat.issa[,prop_disturbance_avail:= mean(prop_disturbance_end, na.rm = T), by = indiv_step_id]
dat.issa[,ts_harv_avail:= mean(ts_harv_end, na.rm = T), by = indiv_step_id]
dat.issa[,ts_fires_avail:= mean(ts_fires_end, na.rm = T), by = indiv_step_id]


dat.issa[,dist_paved_start_adj:= ifelse(.SD[,.(min(dist_paved_start, na.rm = T))]>20000, 20001, dist_paved_start), by = indiv_step_id]
dat.issa[,dist_paved_end_adj:= ifelse(.SD[,.(min(dist_paved_end, na.rm = T))]>20000, 20001, dist_paved_end), by = indiv_step_id]
dat.issa[,dist_paved_avail:= mean(dist_paved_end, na.rm = T), by = indiv_step_id]

dat.issa[,dist_unpaved_start_adj:= ifelse(.SD[,.(min(dist_unpaved_start, na.rm = T))]>20000, 20001, dist_unpaved_start), by = indiv_step_id]
dat.issa[,dist_unpaved_end_adj:= ifelse(.SD[,.(min(dist_unpaved_end, na.rm = T))]>20000, 20001, dist_unpaved_end), by = indiv_step_id]
dat.issa[,dist_unpaved_avail:= mean(dist_unpaved_end, na.rm = T), by = indiv_step_id]

dat.issa[,dist_road_end_adj:= ifelse(.SD[,.(min(dist_road_end, na.rm = T))]>20000, 20001, dist_road_end), by = indiv_step_id]
dat.issa[,dist_road_avail:= mean(dist_road_end, na.rm = T), by = indiv_step_id]

dat.issa[,dist_lf_other_start_adj:= ifelse(.SD[,.(min(dist_lf_other_start, na.rm = T))]>20000, 20001, dist_lf_other_start), by = indiv_step_id]
dat.issa[,dist_lf_other_end_adj:= ifelse(.SD[,.(min(dist_lf_other_end, na.rm = T))]>20000, 20001, dist_lf_other_end), by = indiv_step_id]
dat.issa[,dist_lf_other_avail:= mean(dist_lf_other_end, na.rm = T), by = indiv_step_id]

dat.issa[,dist_lf_end_adj:= ifelse(.SD[,.(min(dist_lf_end, na.rm = T))]>20000, 20001, dist_lf_end), by = indiv_step_id]
dat.issa[,dist_lf_avail:= mean(dist_lf_end, na.rm = T), by = indiv_step_id]

gc()
dat.issa[,dist_anthro_start_adj:= ifelse(.SD[,.(min(dist_anthro_start, na.rm = T))]>20000, 20001, dist_anthro_start), by = indiv_step_id]
dat.issa[,dist_anthro_end_adj:= ifelse(.SD[,.(min(dist_anthro_end, na.rm = T))]>20000, 20001, dist_anthro_end), by = indiv_step_id]
dat.issa[,dist_anthro_avail:= mean(dist_anthro_end, na.rm = T), by = indiv_step_id]

dat.issa[,dist_mining_start_adj:= ifelse(.SD[,.(min(dist_mining_start, na.rm = T))]>20000, 20001, dist_mining_start), by = indiv_step_id]
dat.issa[,dist_mining_end_adj:= ifelse(.SD[,.(min(dist_mining_end, na.rm = T))]>20000, 20001, dist_mining_end), by = indiv_step_id]
dat.issa[,dist_mining_avail:= mean(dist_mining_end, na.rm = T), by = indiv_step_id]

dat.issa[,dist_placer_start_adj:= ifelse(.SD[,.(min(dist_placer_start, na.rm = T))]>20000, 20001, dist_placer_start), by = indiv_step_id]
dat.issa[,dist_placer_end_adj:= ifelse(.SD[,.(min(dist_placer_end, na.rm = T))]>20000, 20001, dist_placer_end), by = indiv_step_id]
dat.issa[,dist_placer_avail:= mean(dist_placer_end, na.rm = T), by = indiv_step_id]

dat.issa[,dist_quartz_start_adj:= ifelse(.SD[,.(min(dist_quartz_start, na.rm = T))]>20000, 20001, dist_quartz_start), by = indiv_step_id]
dat.issa[,dist_quartz_end_adj:= ifelse(.SD[,.(min(dist_quartz_end, na.rm = T))]>20000, 20001, dist_quartz_end), by = indiv_step_id]
dat.issa[,dist_quartz_avail:= mean(dist_quartz_end, na.rm = T), by = indiv_step_id]

dat.issa[,dist_otherdisturbance_start_adj:= ifelse(.SD[,.(min(dist_otherdisturbance_start, na.rm = T))]>20000, 20001, dist_otherdisturbance_start), by = indiv_step_id]
dat.issa[,dist_otherdisturbance_end_adj:= ifelse(.SD[,.(min(dist_otherdisturbance_end, na.rm = T))]>20000, 20001, dist_otherdisturbance_end), by = indiv_step_id]
dat.issa[,dist_otherdisturbance_avail:= mean(dist_otherdisturbance_end, na.rm = T), by = indiv_step_id]

dat.issa[,dist_anthrodisturbance_start_adj:= ifelse(.SD[,.(min(dist_anthrodisturbance_start, na.rm = T))]>20000, 20001, dist_anthrodisturbance_start), by = indiv_step_id]
dat.issa[,dist_anthrodisturbance_end_adj:= ifelse(.SD[,.(min(dist_anthrodisturbance_end, na.rm = T))]>20000, 20001, dist_anthrodisturbance_end), by = indiv_step_id]
dat.issa[,dist_anthrodisturbance_avail:= mean(dist_anthrodisturbance_end, na.rm = T), by = indiv_step_id]
#saveRDS(dat.issa, file.path(derived, 'dat_iSSA_20lf20poly_adj.RDS'))

#### START ---- 
dat.issa <- readRDS(file.path(derived, 'dat_iSSA_20lf20poly_adj.RDS'))
dat.issa.20 <- dat.issa[,.SD[1:21], by = .(indiv_step_id)]
dat.issa.15 <- dat.issa[,.SD[1:16], by = .(indiv_step_id)]
dat.issa.10 <- dat.issa[,.SD[1:11], by = .(indiv_step_id)]

# Separate seasons into diff datasets
dat.issa.10$monthday <- format(as.Date(dat.issa.10$t1_, format="%Y/%m/%d"),"%m/%d") #Create new column that isolates the month and day out of t1_
dat.issa.10$monthday #Confirming that the new column was created correctly
names(dat.issa.10) #Confirming that the new column exists

dat.issa.10[monthday >= '11/01' | monthday <= '04/15', season:="winter"] #Create new season column, with Nov to mid-April being called Winter
dat.issa.10.winter <- dat.issa.10[dat.issa.10$season == 'winter',] #Assign to new winter dataframe


dat.issa.10[monthday >= '07/01' & monthday <= '09/30', season:="summer"] #Create summer season
dat.issa.10.summer <- dat.issa.10[dat.issa.10$season == 'summer',] #Assign to new summer dataframe


#Winter-only analysis
gc()
m.full.winter <- glmmTMB(case_ ~ -1 +
                    I(log((sl_+1)/1000)) +
                    #I(cos(ta_)) +
                    I(log((sl_+1)/1000)):I(cos(ta_)) + #turn angle
                    #scale(for.age_start):I(log(sl_+1)) + #forest age at start of step
                    #  prop_forest_start:I(log(sl_+1)) + #proportion of forest within buffer of start of step
                    # prop_veg_start:I(log(sl_+1)) + #prop
                    # scale(for.age_end) + scale(for.age_end):scale(for.age_avail) + #fine scale functional response - forest age available
                    prop_forest_end + prop_forest_end:prop_forest_avail +
                    prop_veg_end + prop_veg_end:prop_veg_avail + #this is modelling the selection
                    I(log(ts_fires_end+1)) + I(log(ts_fires_end+1)):I(log(ts_fires_avail+1)) + #ts is time since fires, ds is dintance to
                    # I(log(ts_harv_end+1)) + I(log(ts_harv_end+1)):I(log(ts_harv_avail+1)) +
                    # I(log((dist_paved_end_adj/1000)+1)) + I(log((dist_paved_end_adj/1000)+1)):I(log((dist_paved_avail/1000)+1)) +
                    #  I(log((dist_unpaved_end_adj/1000)+1)) + I(log((dist_unpaved_end_adj/1000)+1)):I(log((dist_unpaved_avail/1000)+1)) +
                    I(log(((dist_road_end_adj+1)/1000))) + #I(log((dist_road_end_adj/1000)+1)):I(log((dist_road_avail/1000)+1)) +
                    I(log((dist_lf_other_end_adj+1)/1000)) +# I(log((dist_lf_other_end_adj/1000)+1)):I(log((dist_lf_other_avail/1000)+1)) +
                    #I(log((dist_anthro_end_adj/1000)+1)) + I(log((dist_anthro_end_adj/1000)+1)):I(log((dist_anthro_avail/1000)+1)) +
                    I(log((dist_mining_end_adj+1)/1000)) +# I(log((dist_mining_end_adj/1000)+1)):I(log((dist_mining_avail/1000)+1)) +
                    I(log((dist_otherdisturbance_end_adj+1)/1000)) + #I(log((dist_otherdisturbance_end_adj/1000)+1)):I(log((dist_otherdisturbance_avail/1000)+1)) +
                    
                    (1|indiv_step_id) + #
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
                    (0 + I(log((dist_road_end_adj+1)/1000))|id) + 
                    (0 + I(log((dist_lf_other_end_adj+1)/1000))|id) +
                    #(0 + I(log((dist_anthro_end_adj/1000)+1))|id) + 
                    (0 + I(log((dist_mining_end_adj+1)/1000))|id) + 
                    (0 + I(log((dist_otherdisturbance_end_adj+1)/1000))|id) + 
                    
                    (1|year),
                  family = poisson(), data = dat.issa.10.winter[,`:=`(year = as.factor(year), 
                                                               indiv_step_id = as.factor(indiv_step_id),
                                                               id = as.factor(id))],
                  map= list(theta = factor(c(NA,1:10))),
                  start = list(theta =c(log(1000), seq(0,0, length.out = 10))), #set variance high for clusters
                  verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)
## 

summary(m.full.winter)
# save model output
saveRDS(m.full.winter, file.path(derived, 'mod_road_lfother_mining_anthroother_lf20poly20_winter.RDS'))

# read back in if needed
m.full.winter <- readRDS(file.path(derived, 'mod_road_lfother_mining_anthroother_lf10poly20_winter.RDS'))
m.full.indiv.winter <- coef(m.full.winter)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(m.full.indiv.winter)[term!='(Intercept)']
pop <- setDT(tidy(m.full.winter, effects = 'fixed'))


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


#Summer-only analysis
gc()
m.full.summer <- glmmTMB(case_ ~ -1 +
                           I(log((sl_+1)/1000)) +
                           #I(cos(ta_)) +
                           I(log((sl_+1)/1000)):I(cos(ta_)) + #turn angle
                           #scale(for.age_start):I(log(sl_+1)) + #forest age at start of step
                           #  prop_forest_start:I(log(sl_+1)) + #proportion of forest within buffer of start of step
                           # prop_veg_start:I(log(sl_+1)) + #prop
                           # scale(for.age_end) + scale(for.age_end):scale(for.age_avail) + #fine scale functional response - forest age available
                           prop_forest_end + prop_forest_end:prop_forest_avail +
                           prop_veg_end + prop_veg_end:prop_veg_avail + #this is modelling the selection
                           I(log(ts_fires_end+1)) + I(log(ts_fires_end+1)):I(log(ts_fires_avail+1)) + #ts is time since fires, ds is dintance to
                           # I(log(ts_harv_end+1)) + I(log(ts_harv_end+1)):I(log(ts_harv_avail+1)) +
                           # I(log((dist_paved_end_adj/1000)+1)) + I(log((dist_paved_end_adj/1000)+1)):I(log((dist_paved_avail/1000)+1)) +
                           #  I(log((dist_unpaved_end_adj/1000)+1)) + I(log((dist_unpaved_end_adj/1000)+1)):I(log((dist_unpaved_avail/1000)+1)) +
                           I(log(((dist_road_end_adj+1)/1000))) + #I(log((dist_road_end_adj/1000)+1)):I(log((dist_road_avail/1000)+1)) +
                           I(log((dist_lf_other_end_adj+1)/1000)) +# I(log((dist_lf_other_end_adj/1000)+1)):I(log((dist_lf_other_avail/1000)+1)) +
                           #I(log((dist_anthro_end_adj/1000)+1)) + I(log((dist_anthro_end_adj/1000)+1)):I(log((dist_anthro_avail/1000)+1)) +
                           I(log((dist_mining_end_adj+1)/1000)) +# I(log((dist_mining_end_adj/1000)+1)):I(log((dist_mining_avail/1000)+1)) +
                           I(log((dist_otherdisturbance_end_adj+1)/1000)) + #I(log((dist_otherdisturbance_end_adj/1000)+1)):I(log((dist_otherdisturbance_avail/1000)+1)) +
                           
                           (1|indiv_step_id) + #
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
                           (0 + I(log((dist_road_end_adj+1)/1000))|id) + 
                           (0 + I(log((dist_lf_other_end_adj+1)/1000))|id) +
                           #(0 + I(log((dist_anthro_end_adj/1000)+1))|id) + 
                           (0 + I(log((dist_mining_end_adj+1)/1000))|id) + 
                           (0 + I(log((dist_otherdisturbance_end_adj+1)/1000))|id) + 
                           
                           (1|year),
                         family = poisson(), data = dat.issa.10.summer[,`:=`(year = as.factor(year), 
                                                                             indiv_step_id = as.factor(indiv_step_id),
                                                                             id = as.factor(id))],
                         map= list(theta = factor(c(NA,1:10))),
                         start = list(theta =c(log(1000), seq(0,0, length.out = 10))), #set variance high for clusters
                         verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)
## 

summary(m.full.summer)
# save model output
saveRDS(m.full.summer, file.path(derived, 'mod_road_lfother_mining_anthroother_lf20poly20_summer.RDS'))

# read back in if needed
m.full.summer <- readRDS(file.path(derived, 'mod_road_lfother_mining_anthroother_lf10poly20_winter.RDS'))
m.full.indiv.summer <- coef(m.full.summer)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(m.full.indiv.summer)[term!='(Intercept)']
pop <- setDT(tidy(m.full.summer, effects = 'fixed'))


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


#FIDDLING WITH PARAMETERS
m.full <- glmmTMB(case_ ~ -1 +
                    I(log((sl_+1)/1000)) +
                    #I(cos(ta_)) +
                    I(log((sl_+1)/1000)):I(cos(ta_)) +
                    #scale(for.age_start):I(log(sl_+1)) + 
                    #  prop_forest_start:I(log(sl_+1)) +
                    # prop_veg_start:I(log(sl_+1)) + 
                    # scale(for.age_end) + scale(for.age_end):scale(for.age_avail) +
                    prop_forest_end + prop_forest_end:prop_forest_avail +
                    # prop_veg_end + prop_veg_end:prop_veg_avail +
                    I(log(ts_fires_end+1)) +# I(log(ts_fires_end+1)):I(log(ts_fires_avail+1)) +
                    # I(log(ts_harv_end+1)) + I(log(ts_harv_end+1)):I(log(ts_harv_avail+1)) +
                    # I(log((dist_paved_end_adj/1000)+1)) + I(log((dist_paved_end_adj/1000)+1)):I(log((dist_paved_avail/1000)+1)) +
                    #  I(log((dist_unpaved_end_adj/1000)+1)) + I(log((dist_unpaved_end_adj/1000)+1)):I(log((dist_unpaved_avail/1000)+1)) +
                    I(log(((dist_road_end_adj+1)/1000))) + #I(log((dist_road_end_adj/1000)+1)):I(log((dist_road_avail/1000)+1)) +
                    I(log((dist_lf_other_end_adj+1)/1000)) +# I(log((dist_lf_other_end_adj/1000)+1)):I(log((dist_lf_other_avail/1000)+1)) +
                    #I(log((dist_anthro_end_adj/1000)+1)) + I(log((dist_anthro_end_adj/1000)+1)):I(log((dist_anthro_avail/1000)+1)) +
                    I(log((dist_mining_end_adj+1)/1000)) +# I(log((dist_mining_end_adj/1000)+1)):I(log((dist_mining_avail/1000)+1)) +
                    I(log((dist_otherdisturbance_end_adj+1)/1000)) + #I(log((dist_otherdisturbance_end_adj/1000)+1)):I(log((dist_otherdisturbance_avail/1000)+1)) +
                    
                    (1|indiv_step_id) +
                    (0 + I(log((sl_ +1)/1000))|id) +
                    #(0 + I(cos(ta_))|id) +
                    (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                    #(0 + scale(for.age_start):I(log(sl_+1))|id) + 
                    #  (0 + prop_forest_start:I(log(sl_+1))|id) +
                    # (0 + prop_veg_start:I(log(sl_+1))|id) + 
                    # (0 + scale(for.age_end)|id) + 
                    (0 + prop_forest_end|id) +
                    # (0 + prop_veg_end|id) +
                    (0 + (I(log(ts_fires_end+1)))|id) +
                    # (0 + (I(log(ts_harv_end+1)))|id) + 
                    # (0 + I(log((dist_paved_end_adj/1000)+1))|id) + 
                    # (0 + I(log((dist_unpaved_end_adj/1000)+1))|id) + 
                    (0 + I(log((dist_road_end_adj+1)/1000))|id) + 
                    (0 + I(log((dist_lf_other_end_adj+1)/1000))|id) +
                    #(0 + I(log((dist_anthro_end_adj/1000)+1))|id) + 
                    (0 + I(log((dist_mining_end_adj+1)/1000))|id) + 
                    (0 + I(log((dist_otherdisturbance_end_adj+1)/1000))|id) + 
                    
                    (1|year),
                  family = poisson(), data = dat.issa.10[,`:=`(year = as.factor(year), 
                                                               indiv_step_id = as.factor(indiv_step_id),
                                                               id = as.factor(id))],
                  map= list(theta = factor(c(NA,1:9))),
                  start = list(theta =c(log(1000), seq(0,0, length.out = 9))), 
                  verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)




summary(m.full)
# save model output
saveRDS(m.full, file.path(derived, 'alt_mod_road_lfother_mining_anthroother_lf20poly20.RDS'))

# read back in if needed
m.full <- readRDS(file.path(derived, 'alt_mod_road_lfother_mining_anthroother_lf10poly20.RDS'))
m.full.indiv <- coef(m.full)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(m.full.indiv)[term!='(Intercept)']
pop <- setDT(tidy(m.full, effects = 'fixed'))


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


#Fiddling further 
dat.issa.10$dist_anthrodisturbance_start <= 20000

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
                              map= list(theta = factor(c(NA,1:10))),
                              start = list(theta =c(log(1000), seq(0,0, length.out = 10))), 
                              verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)

summary(m.closedisturbance)
# save model output
saveRDS(m.closedisturbance, file.path(derived, 'm.closed.disturbance.mod_road_lfother_mining_anthroother_lf20poly20.RDS'))


m.closedisturbance.indiv <- coef(m.closedisturbance)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(m.closedisturbance.indiv)[term!='(Intercept)']
pop <- setDT(tidy(m.closedisturbance, effects = 'fixed'))


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



# full iSSA ----
gc()
m.full <- glmmTMB(case_ ~ -1 +
                   I(log((sl_+1)/1000)) +
                   #I(cos(ta_)) +
                   I(log((sl_+1)/1000)):I(cos(ta_)) + #turn angle
                   #scale(for.age_start):I(log(sl_+1)) + #forest age at start of step
                   #  prop_forest_start:I(log(sl_+1)) + #proportion of forest within buffer of start of step
                   # prop_veg_start:I(log(sl_+1)) + #proportion of vegetation within buffer at start of step
                  # scale(for.age_end) + scale(for.age_end):scale(for.age_avail) + #fine scale functional response - forest age available
                   prop_forest_end + prop_forest_end:prop_forest_avail +
                    prop_veg_end + prop_veg_end:prop_veg_avail + #this is incorperating the 'selective' behaviour
                   I(log(ts_fires_end+1)) + I(log(ts_fires_end+1)):I(log(ts_fires_avail+1)) + #ts is time since fires, ds is dintance to
                  # I(log(ts_harv_end+1)) + I(log(ts_harv_end+1)):I(log(ts_harv_avail+1)) +
                   # I(log((dist_paved_end_adj/1000)+1)) + I(log((dist_paved_end_adj/1000)+1)):I(log((dist_paved_avail/1000)+1)) +
                   #  I(log((dist_unpaved_end_adj/1000)+1)) + I(log((dist_unpaved_end_adj/1000)+1)):I(log((dist_unpaved_avail/1000)+1)) +
                    I(log(((dist_road_end_adj+1)/1000))) + #I(log((dist_road_end_adj/1000)+1)):I(log((dist_road_avail/1000)+1)) +
                    I(log((dist_lf_other_end_adj+1)/1000)) +# I(log((dist_lf_other_end_adj/1000)+1)):I(log((dist_lf_other_avail/1000)+1)) +
                    #I(log((dist_anthro_end_adj/1000)+1)) + I(log((dist_anthro_end_adj/1000)+1)):I(log((dist_anthro_avail/1000)+1)) +
                    I(log((dist_mining_end_adj+1)/1000)) +# I(log((dist_mining_end_adj/1000)+1)):I(log((dist_mining_avail/1000)+1)) +
                    I(log((dist_otherdisturbance_end_adj+1)/1000)) + #I(log((dist_otherdisturbance_end_adj/1000)+1)):I(log((dist_otherdisturbance_avail/1000)+1)) +
                    
                   (1|indiv_step_id) + #each step is a random intercept to account for pseudorep (each step within an indiv is replication, all step options form a cluster)
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
                    (0 + I(log((dist_road_end_adj+1)/1000))|id) + 
                   (0 + I(log((dist_lf_other_end_adj+1)/1000))|id) +
                   #(0 + I(log((dist_anthro_end_adj/1000)+1))|id) + 
                    (0 + I(log((dist_mining_end_adj+1)/1000))|id) + 
                    (0 + I(log((dist_otherdisturbance_end_adj+1)/1000))|id) + 
                  
                   (1|year),
                 family = poisson(), data = dat.issa.10[,`:=`(year = as.factor(year), 
                                                           indiv_step_id = as.factor(indiv_step_id),
                                                           id = as.factor(id))],
                 map= list(theta = factor(c(NA,1:10))),
                 start = list(theta =c(log(1000), seq(0,0, length.out = 10))), #set variance high for clusters
                 verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)
## 


summary(m.full)
# save model output
saveRDS(m.full, file.path(derived, 'mod_road_lfother_mining_anthroother_lf20poly20.RDS'))

# read back in if needed
m.full <- readRDS(file.path(derived, 'mod_road_lfother_mining_anthroother_lf10poly20.RDS'))
m.full.indiv <- coef(m.full)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(m.full.indiv)[term!='(Intercept)']
pop <- setDT(tidy(m.full, effects = 'fixed'))


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



# full RSF ----
m.rsf.full <- glmmTMB(case_ ~ -1 +
                    scale(for.age_end) +
                    prop_veg_end +
                    I(log(ts_fires_end+1)) + 
                    I(log(ts_harv_end+1)) + 
                    I(log((dist_paved_end/1000)+1)) + 
                    I(log((dist_unpaved_end/1000)+1)) + 
                    I(log((dist_lf_other_end/1000)+1)) + 
                    I(log((dist_anthro_end/1000)+1)) + 
                    
                    (1|id) +
                    (0 + scale(for.age_end)|id) + 
                    (0 + prop_veg_end|id) +
                    (0 + (I(log(ts_fires_end+1)))|id) +
                    (0 + (I(log(ts_harv_end+1)))|id) + 
                    (0 + I(log((dist_paved_end/1000)+1))|id) + 
                    (0 + I(log((dist_unpaved_end/1000)+1))|id) + 
                    (0 + I(log((dist_lf_other_end/1000)+1))|id) +
                    (0 + I(log((dist_anthro_end/1000)+1))|id) + 
                    
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
                    # I(log((dist_paved_end/1000)+1)) + 
                    # I(log((dist_unpaved_end/1000)+1)) + 
                     I(log((dist_road_end/1000)+1)) + 
                    I(log((dist_lf_other_end/1000)+1)) + 
                    I(log((dist_anthro_end/1000)+1)) + 
                    
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
                    # (0 + I(log((dist_paved_end/1000)+1))|id) + 
                    # (0 + I(log((dist_unpaved_end/1000)+1))|id) + 
                     (0 + I(log((dist_road_end/1000)+1))|id) + 
                     (0 + I(log((dist_lf_other_end/1000)+1))|id) +
                    (0 + I(log((dist_anthro_end/1000)+1))|id) + 
                    
                    (1|year),
                  family = poisson(), data = dat.issa[,`:=`(year = as.factor(year), 
                                                            indiv_step_id = as.factor(indiv_step_id),
                                                            id = as.factor(id))],
                  map= list(theta = factor(c(NA,1:12))),
                  start = list(theta =c(log(1000), seq(0,0, length.out = 12)))
)



summary(m.roads)


# iSSA only near dist_urbance ----
# filter to only those near a dist_urbance
dat.issa.dist_urbance <- dat.issa[indiv_step_id %in% (dat.issa[case_==T & (dist_anthro_start < 30000 | dist_anthro_end < 30000)]$indiv_step_id)]
m.dist_urbance <- glmmTMB(case_ ~ -1 +
                    I(log(sl_+1)) +
                    I(cos(ta_)) +
                    I(log(sl_+1)):I(cos(ta_)) +
                    for.age_start:I(log(sl_+1)) + 
                    prop_veg_start:I(log(sl_+1)) + 
                    for.age_end +
                    prop_veg_end +
                    I(log(ts_fires_end+1)) + 
                    I(log(ts_harv_end+1)) + 
                    I(log((dist_paved_end/1000)+1)) + 
                    I(log((dist_unpaved_end/1000)+1)) + 
                    I(log((dist_lf_other_end/1000)+1)) + 
                    I(log((dist_anthro_end/1000)+1)) + 
                    
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
                    (0 + I(log((dist_paved_end/1000)+1))|id) + 
                    (0 + I(log((dist_unpaved_end/1000)+1))|id) + 
                    (0 + I(log((dist_lf_other_end/1000)+1))|id) +
                    (0 + I(log((dist_anthro_end/1000)+1))|id) + 
                    
                    (1|year),
                  family = poisson(), data = dat.issa.dist_urbance[,`:=`(year = as.factor(year), 
                                                            indiv_step_id = as.factor(indiv_step_id),
                                                            id = as.factor(id))],
                  map= list(theta = factor(c(NA,1:14))),
                  start = list(theta =c(log(1000), seq(0,0, length.out = 14)))
)

summary(m.dist_urbance)
check_model(m.dist_urbance)

gc()
dat.issa.mines <- dat.issa[prov == 'yt' & indiv_step_id %in% (dat.issa[case_==T & (dist_mining_start < 30000 | dist_mining_end < 30000)]$indiv_step_id)]

m.dist_urbance2 <- glmmTMB(case_ ~ -1 +
                           I(log(sl_+1)) +
                           I(cos(ta_)) +
                           I(log(sl_+1)):I(cos(ta_)) +
                           prop_needleleaf_start:I(log(sl_+1)) + 
                           prop_veg_start:I(log(sl_+1)) + 
                           prop_needleleaf_end +
                           prop_veg_end +
                           I(log(ts_fires_end+1)) + 
                           #I(log(ts_harv_end+1)) + 
                           I(log((dist_paved_end/1000)+1)) + 
                           I(log((dist_unpaved_end/1000)+1)) + 
                           I(log((dist_lf_other_end/1000)+1)) + 
                           #I(log((dist_anthro_end/1000)+1)) + 
                            I(log((dist_placer_end/1000)+1)) +
                            I(log((dist_quartz_end/1000)+1)) +
                           
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
                           (0 + I(log((dist_paved_end/1000)+1))|id) + 
                           (0 + I(log((dist_unpaved_end/1000)+1))|id) + 
                           (0 + I(log((dist_lf_other_end/1000)+1))|id) +
                           #(0 + I(log((dist_anthro_end/1000)+1))|id) +
                            (0 + I(log((dist_placer_end/1000)+1))|id) +
                            (0 + I(log((dist_quartz_end/1000)+1))|id) +
                            
                           (1|year),
                         family = poisson(), data = dat.issa.mines[,`:=`(year = as.factor(year), 
                                                                               indiv_step_id = as.factor(indiv_step_id),
                                                                               id = as.factor(id))],
                         map= list(theta = factor(c(NA,1:14))),
                         start = list(theta =c(log(1000), seq(0,0, length.out = 14)))
)

summary(m.dist_urbance2)
check_model(m.dist_urbance2)

saveRDS(m.dist_urbance2, file.path(derived, 'mods', 'iSSA_yt_mines.RDS'))
m.dist_urbance2 <- readRDS(file.path(derived, 'mods', 'iSSA_yt_mines.RDS'))
summary(m.dist_urbance2)
