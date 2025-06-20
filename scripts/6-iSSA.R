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
hist(dat$ts_fires_250_end) dat$

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

#What goes into dist_anthro?
dat$dist_anthro_250_start = pmin(dist_paved_250_end, dist_unpaved_250_end, na.rm = T)
dat$dist_anthro_250_end = pmin(dist_paved_250_end, dist_unpaved_250_end, na.rm = T)
dat$dist_mines_250_start = pmin(dat$dist_placer_250_start, dat$dist_quartz_250_start, dat$dist_othermines_250_start, na.rm = T)
dat$dist_mines_250_end = pmin(dat$dist_placer_250_end, dat$dist_quartz_250_end, dat$dist_othermines_250_end, na.rm = T)

#Need to include 1000m res stuff
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
# dat.issa[,for.age_avail:= mean(for.age_end, na.rm = T), by = indiv_step_id]
# dat.issa[,prop_veg_avail:= mean(prop_veg_end, na.rm = T), by = indiv_step_id]
# dat.issa[,prop_forest_avail:= mean(prop_forest_end, na.rm = T), by = indiv_step_id]
# dat.issa[,prop_needleleaf_avail:= mean(prop_needleleaf_end, na.rm = T), by = indiv_step_id]
# dat.issa[,prop_disturbance_avail:= mean(prop_disturbance_end, na.rm = T), by = indiv_step_id]
# dat.issa[,ts_harv_avail:= mean(ts_harv_end, na.rm = T), by = indiv_step_id]
# dat.issa[,ts_fires_avail:= mean(ts_fires_end, na.rm = T), by = indiv_step_id]


dat.issa[,dist_paved_start_adj:= ifelse(.SD[,.(min(dist_paved_start, na.rm = T))]>20000, 20001, dist_paved_start), by = indiv_step_id]
dat.issa[,dist_paved_end_adj:= ifelse(.SD[,.(min(dist_paved_end, na.rm = T))]>20000, 20001, dist_paved_end), by = indiv_step_id]
#dat.issa[,dist_paved_avail:= mean(dist_paved_end, na.rm = T), by = indiv_step_id]

dat.issa[,dist_unpaved_start_adj:= ifelse(.SD[,.(min(dist_unpaved_start, na.rm = T))]>20000, 20001, dist_unpaved_start), by = indiv_step_id]
dat.issa[,dist_unpaved_end_adj:= ifelse(.SD[,.(min(dist_unpaved_end, na.rm = T))]>20000, 20001, dist_unpaved_end), by = indiv_step_id]
#dat.issa[,dist_unpaved_avail:= mean(dist_unpaved_end, na.rm = T), by = indiv_step_id]

dat.issa[,dist_road_end_adj:= ifelse(.SD[,.(min(dist_road_end, na.rm = T))]>20000, 20001, dist_road_end), by = indiv_step_id]
#dat.issa[,dist_road_avail:= mean(dist_road_end, na.rm = T), by = indiv_step_id]

dat.issa[,dist_lf_other_start_adj:= ifelse(.SD[,.(min(dist_lf_other_start, na.rm = T))]>20000, 20001, dist_lf_other_start), by = indiv_step_id]
dat.issa[,dist_lf_other_end_adj:= ifelse(.SD[,.(min(dist_lf_other_end, na.rm = T))]>20000, 20001, dist_lf_other_end), by = indiv_step_id]
#dat.issa[,dist_lf_other_avail:= mean(dist_lf_other_end, na.rm = T), by = indiv_step_id]

dat.issa[,dist_lf_end_adj:= ifelse(.SD[,.(min(dist_lf_end, na.rm = T))]>20000, 20001, dist_lf_end), by = indiv_step_id]
#dat.issa[,dist_lf_avail:= mean(dist_lf_end, na.rm = T), by = indiv_step_id]

gc()
dat.issa[,dist_anthro_start_adj:= ifelse(.SD[,.(min(dist_anthro_start, na.rm = T))]>20000, 20001, dist_anthro_start), by = indiv_step_id]
dat.issa[,dist_anthro_end_adj:= ifelse(.SD[,.(min(dist_anthro_end, na.rm = T))]>20000, 20001, dist_anthro_end), by = indiv_step_id]
#dat.issa[,dist_anthro_avail:= mean(dist_anthro_end, na.rm = T), by = indiv_step_id]

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

#### PETE START ---- 
dat.issa <- readRDS(file.path(derived, 'dat_iSSA_20lf20poly_adj.RDS'))
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
                    I(log(ts_fires_end+1)) + I(log(ts_fires_end+1)):I(log(ts_fires_avail+1)) + #ts is time since fires, ds is distance to
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
list.files() 
m.full.winter <- readRDS("mod_road_lfother_mining_anthroother_lf20poly20_winter.RDS")
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
 

summary(m.full.summer)
# save model output
saveRDS(m.full.summer, file.path(derived, 'mod_road_lfother_mining_anthroother_lf20poly20_summer.RDS'))

# read back in if needed
list.files() 
m.full.summer <- readRDS("mod_road_lfother_mining_anthroother_lf20poly20_summer.RDS")
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


##BOTH SUMMER AND WINTER ANALYSES SHOWED THE SAME VARIANCE ISSUE, but also had difference coeff estimates


---------------------------------------------


#FIDDLING WITH PARAMETERS to simplify model
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


##SAME ISSUE

---------------------------------------------


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


#Not converging when quartz and placer are added, so re-vamping to look like an RSF
gc()
m.rsf.mines <- glmmTMB(case_ ~ -1 +
                        prop_forest_end +
                        prop_forest_end:prop_forest_avail +
                        prop_veg_end +
                        prop_veg_end:prop_veg_avail +
                        I(log(ts_fires_end+1)) + I(log(ts_fires_end+1)):I(log(ts_fires_avail+1)) +
                        I(log(((dist_road_end+1)/1000))) +
                        I(log((dist_lf_other_end+1)/1000)) + 
                        I(log((dist_otherdisturbance_end+1)/1000)) +
                        I(log((dist_quartz_end+1)/1000)) + I(log((dist_placer_end+1)/1000)) +
                        
                        (1|id) +
                        (0 + prop_forest_end|id) +
                        (0 + prop_veg_end|id) +
                        (0 + (I(log(ts_fires_end+1)))|id) +
                        (0 + I(log((dist_road_end+1)/1000))|id) + 
                        (0 + I(log((dist_lf_other_end+1)/1000))|id) +
                        (0 + I(log((dist_quartz_end+1)/1000))|id) +
                        (0 + I(log((dist_placer_end+1)/1000))|id) +
                        (0 + I(log((dist_otherdisturbance_end+1)/1000))|id) + 
                        
                        (1|year),
                      family = binomial(), data = dat.issa.10[,`:=`(year = as.factor(year),
                                                                 id = as.factor(id))],
                      map= list(theta = factor(c(NA,1:9))),
                      start = list(theta =c(log(1000), seq(0,0, length.out = 9))),
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


##VARIANCE ISSUE HAS BEEN FIXED BY MODELLING NEAR-BY DISTURBANCES ONLY



#RSF results
summary(m.rsf.mines)
# save model output
saveRDS(m.rsf.mines, "m.rsf.mines_lf20poly20.RDS")


m.rsf.mines.indiv <- coef(m.rsf.mines)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(m.rsf.mines.indiv)[term!='(Intercept)']
pop <- setDT(tidy(m.rsf.mines, effects = 'fixed'))


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


#No need to contonue with this RSF, waiting on Tal for more info

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


#LARGE MODEL testing after correlation (collapsing all polyginal and lf parameters), test on original, unfiltered dat.issa.10 dataset
# Filter the dat.issa.10 dataset to only include individuals with >9 months of data

dat.issa.10$month <- format(as.Date(dat.issa.10$t1_, format="%Y/%m/%d"),"%m") #Create new column that isolates the month
dat.issa.10$month #Confirming that the new column was created correctly
names(dat.issa.10) #Confirming that the new column exists


dat.issa.10 <- tibble::repair_names(dat.issa.10)  #Fix duplicated columns

dat.issa.10_filtered <- dat.issa.10[, .SD[n_distinct(month) >= 9], by = id] #To create filtered dataset of indivs with >9 months of data

length(unique(dat.issa.10_filtered$id)) #371 individuals in the filtered dataset
length(unique(dat.issa.10$id)) #466 individuals in the OG dataset


#Running model on filtered dataset
m.large <- glmmTMB(case_ ~ -1 +
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
                     # (0 + (I(log(ts_harv_end+1)))|id) + 
                     # (0 + I(log((dist_paved_end_adj/1000)+1))|id) + 
                     # (0 + I(log((dist_unpaved_end_adj/1000)+1))|id) + 
                     # (0 + I(log((dist_road_end_adj+1)/1000))|id) + 
                     # (0 + I(log((dist_lf_other_end_adj+1)/1000))|id) +
                     #(0 + I(log((dist_anthro_end_adj/1000)+1))|id) + 
                     # (0 + I(log((dist_mining_end_adj+1)/1000))|id) + 
                     (0 + I(log((dist_anthrodisturbance_end_adj+1)/1000))|id) + 
                     
                     (1|year),
                   family = poisson(), data = dat.issa.10_filtered[,`:=`(year = as.factor(year), 
                                                                indiv_step_id = as.factor(indiv_step_id),
                                                                id = as.factor(id))],
                   map= list(theta = factor(c(NA,1:7))),
                   start = list(theta =c(log(1000), seq(0,0, length.out = 7))), 
                   verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)

summary(m.large)
# save model output
saveRDS(m.large, "m.large_filtered.mod_lf20poly20.RDS")

m.large.indiv <- coef(m.large)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(m.large.indiv)[term!='(Intercept)']
pop <- setDT(tidy(m.large, effects = 'fixed'))


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

---------------------------------------------


#Further work with seasonal datasets - filtering timing
#Filter each seasonal dataset to at least 1 full month in summer and at least 3 months in winter (want full month of data, so upping it for the filter by 1)
#Winter
dat.issa.10.winter$month <- format(as.Date(dat.issa.10.winter$t1_, format="%Y/%m/%d"),"%m") #Create new column that isolates the month
dat.issa.10.winter$month #Confirming that the new column was created correctly
names(dat.issa.10.winter) #Confirming that the new column exists
dat.issa.10.winter <- tibble::repair_names(dat.issa.10.winter)  #Fix duplicated columns

dat.issa.10.winter_filtered <- dat.issa.10.winter[, .SD[n_distinct(month) >= 4], by = id]


#Summer
dat.issa.10.summer$month <- format(as.Date(dat.issa.10.summer$t1_, format="%Y/%m/%d"),"%m") #Create new column that isolates the month
dat.issa.10.summer$month #Confirming that the new column was created correctly
names(dat.issa.10.summer) #Confirming that the new column exists
dat.issa.10.summer <- tibble::repair_names(dat.issa.10.summer)  #Fix duplicated columns

dat.issa.10.summer_filtered <- dat.issa.10.summer[, .SD[n_distinct(month) >= 2], by = id]


m.winter.filt <- glmmTMB(case_ ~ -1 +
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
                     # (0 + (I(log(ts_harv_end+1)))|id) + 
                     # (0 + I(log((dist_paved_end_adj/1000)+1))|id) + 
                     # (0 + I(log((dist_unpaved_end_adj/1000)+1))|id) + 
                     # (0 + I(log((dist_road_end_adj+1)/1000))|id) + 
                     # (0 + I(log((dist_lf_other_end_adj+1)/1000))|id) +
                     #(0 + I(log((dist_anthro_end_adj/1000)+1))|id) + 
                     # (0 + I(log((dist_mining_end_adj+1)/1000))|id) + 
                     (0 + I(log((dist_anthrodisturbance_end_adj+1)/1000))|id) + 
                     
                     (1|year),
                   family = poisson(), data = dat.issa.10.winter_filtered[,`:=`(year = as.factor(year), 
                                                                         indiv_step_id = as.factor(indiv_step_id),
                                                                         id = as.factor(id))],
                   map= list(theta = factor(c(NA,1:7))),
                   start = list(theta =c(log(1000), seq(0,0, length.out = 7))), 
                   verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


m.summer.filt <- glmmTMB(case_ ~ -1 +
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
                           # (0 + (I(log(ts_harv_end+1)))|id) + 
                           # (0 + I(log((dist_paved_end_adj/1000)+1))|id) + 
                           # (0 + I(log((dist_unpaved_end_adj/1000)+1))|id) + 
                           # (0 + I(log((dist_road_end_adj+1)/1000))|id) + 
                           # (0 + I(log((dist_lf_other_end_adj+1)/1000))|id) +
                           #(0 + I(log((dist_anthro_end_adj/1000)+1))|id) + 
                           # (0 + I(log((dist_mining_end_adj+1)/1000))|id) + 
                           (0 + I(log((dist_anthrodisturbance_end_adj+1)/1000))|id) + 
                           
                           (1|year),
                         family = poisson(), data = dat.issa.10.summer_filtered[,`:=`(year = as.factor(year), 
                                                                                      indiv_step_id = as.factor(indiv_step_id),
                                                                                      id = as.factor(id))],
                         map= list(theta = factor(c(NA,1:7))),
                         start = list(theta =c(log(1000), seq(0,0, length.out = 7))), 
                         verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


saveRDS(m.winter.filt, "m.winter_filt.mod_lf20poly20.RDS")
saveRDS(m.summer.filt, "m.summer_filt.mod_lf20poly20.RDS")


m.winter.filt.indiv <- coef(m.winter.filt)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(m.winter.filt.indiv)[term!='(Intercept)']
pop <- setDT(tidy(m.winter.filt, effects = 'fixed'))


m.summer.filt.indiv <- coef(m.summer.filt)$cond$id %>% rownames_to_column("id") %>%
  pivot_longer(-id, names_to = "term", values_to = "estimate") %>%
  mutate(method = "ME")
indivs <- setDT(m.summer.filt.indiv)[term!='(Intercept)']
pop <- setDT(tidy(m.summer.filt, effects = 'fixed'))


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
 
# Separate seasons into diff datasets
dat.issa.10$monthday <- format(as.Date(dat.issa.10$t1_, format="%Y/%m/%d"),"%m/%d") #Create new column that isolates the month and day out of t1_
dat.issa.10$monthday #Confirming that the new column was created correctly
names(dat.issa.10) #Confirming that the new column exists

dat.issa.10[monthday >= '11/01' | monthday <= '04/15', season:="winter"] #Create new season column, with Nov to mid-April being called Winter
dat.issa.10.winter <- dat.issa.10[dat.issa.10$season == 'winter',] #Assign to new winter dataframe


dat.issa.10[monthday >= '07/01' & monthday <= '09/30', season:="summer"] #Create summer season from July through September
dat.issa.10.summer <- dat.issa.10[dat.issa.10$season == 'summer',] #Assign to new summer dataframe


dat.issa.10.winter <- tibble::repair_names(dat.issa.10.winter) #Rename duplicate columns in order to run filter functions
dat.issa.10.summer <- tibble::repair_names(dat.issa.10.summer)


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
  