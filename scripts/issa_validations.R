# iSSA Validations ----
# Peter Baker
# Started: Nov 19 2025

require(data.table)
require(glmmTMB)
require(amt)
library(terra)
library(sf)
library(dplyr)
library(ggplot2)
require(reproducible)

# my functions
lapply(dir('R', '*.R', full.names = TRUE), source)

### Input data ----
raw <- file.path('data', 'raw')
derived <- file.path('data', 'derived')

dat.issa <- readRDS(file.path(derived, 'dat_iSSA_updated2.RDS'))
gc()
dat.issa.10 <- dat.issa[,.SD[1:11], by = .(indiv_step_id)]

#Create season
dat.issa.10$monthday <- format(as.Date(dat.issa.10$t1_, format="%Y/%m/%d"),"%m/%d") #Create new column that isolates the month and day out of t1_
dat.issa.10$monthday #Confirming that the new column was created correctly
names(dat.issa.10) #Confirming that the new column exists

dat.issa.10[monthday >= "11/01" | monthday <= "04/15", season := "winter"]
dat.issa.10[monthday >= "07/01" & monthday <= "09/30", season := "summer"]
dat.issa.10[monthday >= "04/16" & monthday <= "06/30", season := "spring"]
dat.issa.10[monthday >= "10/01" & monthday <= "10/31", season := "fall"]


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

#saveRDS(dat.issa.10.filtered, file.path(derived, 'dat_iSSA_10_filtered.RDS'))
#dat.issa.10.filtered <- readRDS(file.path(derived, 'dat_iSSA_10_filtered.RDS'))

# Create training data -----
dat.trainyear <- dat.issa.10.filtered[!(year %in% c(2022, 2023, 2024, 2025))]
dat.trainBChigh <- dat.issa.10.filtered[dat.issa.10.filtered$area != "Pink Mountain", ]
dat.trainBClow <- dat.issa.10.filtered[dat.issa.10.filtered$area != "LittleRancheriaBC", ]
dat.trainYThigh <- dat.issa.10.filtered[dat.issa.10.filtered$area != "SouthernLakes", ]
dat.trainYThigh2 <- dat.issa.10.filtered[dat.issa.10.filtered$area != "Klaza", ]
dat.trainYTlow <- dat.issa.10.filtered[dat.issa.10.filtered$area != "BonnetPlume", ]
dat.trainYT <- dat.issa.10.filtered [dat.issa.10.filtered$prov != "yt", ]
dat.trainBC <- dat.issa.10.filtered [dat.issa.10.filtered$prov != "bc", ]

## Fit models ----
### 2022+ ----
#### Simple -----
gc()
m.simple.yeartrain <- glmmTMB(case_ ~ -1 +
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
                         data = dat.trainyear[, `:=`(
                           year = as.factor(year), 
                           season = as.factor(season),
                           indiv_step_id = as.factor(indiv_step_id),
                           id = as.factor(id)
                         )],
                         map = list(theta = factor(c(NA, 1:8))),
                         start = list(theta = c(log(1000), rep(0, 8))), 
                         verbose = TRUE, 
                         control = glmmTMBControl(rank_check = "adjust")
)

summary(m.simple.yeartrain)
# save model output
saveRDS(m.simple.yeartrain, "m.simple.yeartrain.RDS")
#m.simple.yeartrain <- readRDS("m.simple.yeartrain.RDS")

#### Complex -----
gc()
m.complex.yeartrain <- glmmTMB(case_ ~ -1 +
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
                     data = dat.trainyear[, `:=`(
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

summary(m.complex.yeartrain)
# save model output
saveRDS(m.complex.yeartrain, "m.complex.yeartrain.RDS")
#m.complex.yeartrain <- readRDS("m.complex.yeartrain.RDS")



### High disturbance ----
#### BC (Pink Mtn)-----
##### Simple -----
gc()
m.simple.bchigh_dist_train <- glmmTMB(case_ ~ -1 +
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
                              data = dat.trainBChigh[, `:=`(
                                year = as.factor(year), 
                                season = as.factor(season),
                                indiv_step_id = as.factor(indiv_step_id),
                                id = as.factor(id)
                              )],
                              map = list(theta = factor(c(NA, 1:8))),
                              start = list(theta = c(log(1000), rep(0, 8))), 
                              verbose = TRUE, 
                              control = glmmTMBControl(rank_check = "adjust")
)

summary(m.simple.bchigh_dist_train)
# save model output
saveRDS(m.simple.bchigh_dist_train, "m.simple.bchigh_dist_train.RDS")
#m.simple.bchigh_dist_train <- readRDS("m.simple.bchigh_dist_train.RDS")

##### Complex -----
gc()
m.complex.bchigh_dist_train <- glmmTMB(case_ ~ -1 +
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
                               data = dat.trainBChigh[, `:=`(
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

summary(m.complex.bchigh_dist_train)
# save model output
saveRDS(m.complex.bchigh_dist_train, "m.complex.bchigh_dist_train.RDS")
#m.complex.bchigh_dist_train <- readRDS("m.complex.bchigh_dist_train.RDS")


#### Yukon (Southern Lakes)-----
##### Simple -----
gc()
m.simple.ythigh_dist_train <- glmmTMB(case_ ~ -1 +
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
                                      data = dat.trainYThigh2[, `:=`(
                                        year = as.factor(year), 
                                        season = as.factor(season),
                                        indiv_step_id = as.factor(indiv_step_id),
                                        id = as.factor(id)
                                      )],
                                      map = list(theta = factor(c(NA, 1:8))),
                                      start = list(theta = c(log(1000), rep(0, 8))), 
                                      verbose = TRUE, 
                                      control = glmmTMBControl(rank_check = "adjust")
)

summary(m.simple.ythigh_dist_train)
# save model output
saveRDS(m.simple.ythigh_dist_train, "m.simple.ythigh_dist_train2.RDS")
#m.simple.ythigh_dist_train <- readRDS("m.simple.ythigh_dist_train2.RDS")

##### Complex -----
gc()
m.complex.ythigh_dist_train <- glmmTMB(case_ ~ -1 +
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
                                       data = dat.trainYThigh2[, `:=`(
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

summary(m.complex.ythigh_dist_train)
# save model output
saveRDS(m.complex.ythigh_dist_train, "m.complex.ythigh_dist_train2.RDS")
#m.complex.ythigh_dist_train <- readRDS("m.complex.ythigh_dist_train2.RDS")


### Low disturbance ----
#### BC (Little Rancheria)-----
##### Simple -----
gc()
m.simple.bclow_dist_train <- glmmTMB(case_ ~ -1 +
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
                                      data = dat.trainBClow[, `:=`(
                                        year = as.factor(year), 
                                        season = as.factor(season),
                                        indiv_step_id = as.factor(indiv_step_id),
                                        id = as.factor(id)
                                      )],
                                      map = list(theta = factor(c(NA, 1:8))),
                                      start = list(theta = c(log(1000), rep(0, 8))), 
                                      verbose = TRUE, 
                                      control = glmmTMBControl(rank_check = "adjust")
)

summary(m.simple.bclow_dist_train)
# save model output
saveRDS(m.simple.bclow_dist_train, "m.simple.bclow_dist_train.RDS")
#m.simple.bclow_dist_train <- readRDS("m.simple.bclow_dist_train.RDS")

##### Complex -----
gc()
m.complex.bclow_dist_train <- glmmTMB(case_ ~ -1 +
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
                                       data = dat.trainBClow[, `:=`(
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

summary(m.complex.bclow_dist_train)
# save model output
saveRDS(m.complex.bclow_dist_train, "m.complex.bclow_dist_train.RDS")
#m.complex.bclow_dist_train <- readRDS("m.complex.bclow_dist_train.RDS")


#### Yukon (Bonnet Plume)-----
##### Simple -----
gc()
m.simple.ytlow_dist_train <- glmmTMB(case_ ~ -1 +
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
                                      data = dat.trainYTlow[, `:=`(
                                        year = as.factor(year), 
                                        season = as.factor(season),
                                        indiv_step_id = as.factor(indiv_step_id),
                                        id = as.factor(id)
                                      )],
                                      map = list(theta = factor(c(NA, 1:8))),
                                      start = list(theta = c(log(1000), rep(0, 8))), 
                                      verbose = TRUE, 
                                      control = glmmTMBControl(rank_check = "adjust")
)

summary(m.simple.ytlow_dist_train)
# save model output
saveRDS(m.simple.ytlow_dist_train, "m.simple.ytlow_dist_train.RDS")
#m.simple.ytlow_dist_train <- readRDS("m.simple.ytlow_dist_train.RDS")

##### Complex -----
gc()
m.complex.ytlow_dist_train <- glmmTMB(case_ ~ -1 +
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
                                       data = dat.trainYTlow[, `:=`(
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

summary(m.complex.ytlow_dist_train)
# save model output
saveRDS(m.complex.ytlow_dist_train, "m.complex.ytlow_dist_train.RDS")
#m.complex.ytlow_dist_train <- readRDS("m.complex.ytlow_dist_train.RDS")


### Full province/territory -----
#### Yukon ------
##### Simple -----
gc()
m.simple.yt.train <- glmmTMB(case_ ~ -1 +
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
                                     data = dat.trainYT[, `:=`(
                                       year = as.factor(year), 
                                       season = as.factor(season),
                                       indiv_step_id = as.factor(indiv_step_id),
                                       id = as.factor(id)
                                     )],
                                     map = list(theta = factor(c(NA, 1:8))),
                                     start = list(theta = c(log(1000), rep(0, 8))), 
                                     verbose = TRUE, 
                                     control = glmmTMBControl(rank_check = "adjust")
)

summary(m.simple.yt.train)
# save model output
saveRDS(m.simple.yt.train, "m.simple.yt.train.RDS")
#m.simple.yt.train <- readRDS("m.simple.yt.train.RDS")

##### Complex -----
gc()
m.complex.yt.train <- glmmTMB(case_ ~ -1 +
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
                                      data = dat.trainYT[, `:=`(
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

summary(m.complex.yt.train)
# save model output
saveRDS(m.complex.yt.train, "m.complex.yt.train.RDS")
#m.complex.yt.train <- readRDS("m.complex.yt.train.RDS")



#### BC ------
##### Simple -----
gc()
m.simple.bc.train <- glmmTMB(case_ ~ -1 +
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
                             data = dat.trainBC[, `:=`(
                               year = as.factor(year), 
                               season = as.factor(season),
                               indiv_step_id = as.factor(indiv_step_id),
                               id = as.factor(id)
                             )],
                             map = list(theta = factor(c(NA, 1:8))),
                             start = list(theta = c(log(1000), rep(0, 8))), 
                             verbose = TRUE, 
                             control = glmmTMBControl(rank_check = "adjust")
)

summary(m.simple.bc.train)
# save model output
saveRDS(m.simple.bc.train, "m.simple.bc.train.RDS")
#m.simple.bc.train <- readRDS("m.simple.bc.train.RDS")

##### Complex -----
gc()
m.complex.bc.train <- glmmTMB(case_ ~ -1 +
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
                              data = dat.trainBC[, `:=`(
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

summary(m.complex.bc.train)
# save model output
saveRDS(m.complex.bc.train, "m.complex.bc.train.RDS")
#m.complex.bc.train <- readRDS("m.complex.bc.train.RDS")


## Create Test Data ---- 
dat.testyear <- dat.issa.10.filtered[year %in% c(2022, 2023, 2024, 2025) & case_ == TRUE]
dat.testBChigh <- dat.issa.10.filtered[
  dat.issa.10.filtered$area == "Pink Mountain" & dat.issa.10.filtered$case_ == TRUE, ]
dat.testBClow <- dat.issa.10.filtered[
  dat.issa.10.filtered$area == "LittleRancheriaBC" & dat.issa.10.filtered$case_ == TRUE, ]
dat.testYThigh2 <- dat.issa.10.filtered[
  dat.issa.10.filtered$area == "Klaza" & dat.issa.10.filtered$case_ == TRUE, ]
dat.testYTlow <- dat.issa.10.filtered[
  dat.issa.10.filtered$area == "BonnetPlume" & dat.issa.10.filtered$case_ == TRUE, ]
dat.testYT <- dat.issa.10.filtered[
  dat.issa.10.filtered$prov == "yt" & dat.issa.10.filtered$case_ == TRUE, ]
dat.testBC <- dat.issa.10.filtered[
  dat.issa.10.filtered$prov == "bc" & dat.issa.10.filtered$case_ == TRUE, ]


#Make study area
crs <- st_crs(3154)
buffer <- 10000
sArea.datfilt <- make_study_area(dat.testyear, crs, buff = buffer)
sArea.BChigh <- make_study_area(dat.testBChigh, crs, buff = buffer)
sArea.BClow <- make_study_area(dat.testBClow, crs, buff = buffer)
sArea.YThigh <- make_study_area(dat.testYThigh2, crs, buff = buffer)
sArea.YTlow <- make_study_area(dat.testYTlow, crs, buff = buffer)
sArea.BC <- make_study_area(dat.testBC, crs, buff = buffer)
sArea.YT <- make_study_area(dat.testYT, crs, buff = buffer)

## Validations ----
#Using code from SSUD script to create raster stack
### ---- Year validation ------
#### Simple model ------
crs <- "EPSG:3154"
gc()
testmyearssimple.ud <- mod2UD(mod = m.simple.yeartrain, envlayers = rastLand2020.fine, 
                                studyArea = vect(sArea.datfilt))
plot(testmyearssimple.ud)

#saveRDS(testmyearsfinefull.ud, "testmyearsfinefull.ud.rds")
#testmyearsfinefull.ud <- readRDS("testmyearsfinefull.ud.rds")

gc()
testmyearssimple.prop <- make_prop_use(dat.testyear, crs, testmyearssimple.ud)

gc()
used.bin.testmyearssimple <- make_map_bins(testmyearssimple.prop)

colnames(dat.testyear) <- make.unique(colnames(dat.testyear))

propptsbin.myearssimple.tot <- pts_in_bins_tot(dat.testyear, testmyearssimple.ud)

plot(propptsbin.myearssimple.tot$use.bin, propptsbin.myearssimple.tot$prop.pts)
cor(propptsbin.myearssimple.tot$use.bin, propptsbin.myearssimple.tot$prop.pts, method = 'spearman', use = "complete.obs")
#0.9878788


### Complex model ------
### ---- Year validation
gc()
testmyearscomplex.ud <- mod2UD(mod = m.complex.yeartrain, envlayers = rastLand2020.fine, 
                              studyArea = vect(sArea.datfilt))
plot(testmyearscomplex.ud)

#saveRDS(testmyearsfinefull.ud, "testmyearsfinefull.ud.rds")
#testmyearsfinefull.ud <- readRDS("testmyearsfinefull.ud.rds")

gc()
testmyearscomplex.prop <- make_prop_use(dat.testyear, crs, testmyearscomplex.ud)

gc()
used.bin.testmyearscomplex <- make_map_bins(testmyearscomplex.prop)

colnames(dat.testyear) <- make.unique(colnames(dat.testyear))

propptsbin.myearscomplex.tot <- pts_in_bins_tot(dat.testyear, testmyearscomplex.ud)

plot(propptsbin.myearscomplex.tot$use.bin, propptsbin.myearscomplex.tot$prop.pts)
cor(propptsbin.myearscomplex.tot$use.bin, propptsbin.myearscomplex.tot$prop.pts, method = 'spearman', use = "complete.obs")
#0.9515152



### ---- BC High Disturbance ------
#### Simple model ------
crs <- "EPSG:3154"
gc()
testBChighsimple.ud <- mod2UD(mod = m.simple.bchigh_dist_train, envlayers = rastLand2020.fine, 
                              studyArea = vect(sArea.BChigh))
plot(testBChighsimple.ud)

#saveRDS(testmyearsfinefull.ud, "testmyearsfinefull.ud.rds")
#testmyearsfinefull.ud <- readRDS("testmyearsfinefull.ud.rds")

gc()
testBChighsimple.prop <- make_prop_use(dat.testBChigh, crs, testBChighsimple.ud)

gc()
used.bin.testBChighsimple <- make_map_bins(testBChighsimple.prop)

colnames(dat.testBChigh) <- make.unique(colnames(dat.testBChigh))

propptsbin.BChighsimple.tot <- pts_in_bins_tot(dat.testBChigh, testBChighsimple.ud)

#saveRDS(propptsbin.BChighsimple.tot, file = "propptsbin.BChighsimple.tot.rds")
#propptsbin.BChighsimple.tot <- readRDS("propptsbin.BChighsimple.tot.rds")

plot(propptsbin.BChighsimple.tot$use.bin, propptsbin.BChighsimple.tot$prop.pts)
cor(propptsbin.BChighsimple.tot$use.bin, propptsbin.BChighsimple.tot$prop.pts, method = 'spearman', use = "complete.obs")
#1!!



### Complex model ------
gc()
testBChighcomplex.ud <- mod2UD(mod = m.complex.bchigh_dist_train, envlayers = rastLand2020.fine, 
                               studyArea = vect(sArea.BChigh))
plot(testBChighcomplex.ud)

gc()
testBChighcomplex.prop <- make_prop_use(dat.testBChigh, crs, testBChighcomplex.ud)

gc()
used.bin.testBChighcomplex <- make_map_bins(testBChighcomplex.prop)

colnames(dat.testBChigh) <- make.unique(colnames(dat.testBChigh))

propptsbin.BChighcomplex.tot <- pts_in_bins_tot(dat.testBChigh, testBChighcomplex.ud)

#saveRDS(propptsbin.BChighcomplex.tot, file = "propptsbin.BChighcomplex.tot.rds")
#propptsbin.BChighcomplex.tot <- readRDS("propptsbin.BChighcomplex.tot.rds")

plot(propptsbin.BChighcomplex.tot$use.bin, propptsbin.BChighcomplex.tot$prop.pts)
cor(propptsbin.BChighcomplex.tot$use.bin, propptsbin.BChighcomplex.tot$prop.pts, method = 'spearman', use = "complete.obs")
#0.92


### ---- YT High Disturbance ------
#### Simple model ------
crs <- "EPSG:3154"
gc()
testYThighsimple.ud <- mod2UD(mod = m.simple.ythigh_dist_train, envlayers = rastLand2020.fine, 
                              studyArea = vect(sArea.YThigh))
plot(testYThighsimple.ud)

gc()
testYThighsimple.prop <- make_prop_use(dat.testYThigh2, crs, testYThighsimple.ud)

gc()
used.bin.testYThighsimple <- make_map_bins(testYThighsimple.prop)

colnames(dat.testYThigh2) <- make.unique(colnames(dat.testYThigh2))

propptsbin.YThighsimple.tot <- pts_in_bins_tot(dat.testYThigh2, testYThighsimple.ud)

#saveRDS(propptsbin.YThighsimple.tot, file = "propptsbin.YThighsimple.tot2.rds")
#propptsbin.YThighsimple.tot2 <- readRDS("propptsbin.YThighsimple.tot2.rds")

plot(propptsbin.YThighsimple.tot$use.bin, propptsbin.YThighsimple.tot$prop.pts)
cor(propptsbin.YThighsimple.tot$use.bin, propptsbin.YThighsimple.tot$prop.pts, method = 'spearman', use = "complete.obs")
#0.99


### Complex model ------
### ---- Year validation
gc()
testYThighcomplex.ud <- mod2UD(mod = m.complex.ythigh_dist_train, envlayers = rastLand2020.fine, 
                               studyArea = vect(sArea.YThigh))
plot(testYThighcomplex.ud)

gc()
testYThighcomplex.prop <- make_prop_use(dat.testYThigh2, crs, testYThighcomplex.ud)

gc()
used.bin.testYThighcomplex <- make_map_bins(testYThighcomplex.prop)

colnames(dat.testYThigh2) <- make.unique(colnames(dat.testYThigh2))

propptsbin.YThighcomplex.tot <- pts_in_bins_tot(dat.testYThigh2, testYThighcomplex.ud)

#saveRDS(propptsbin.YThighcomplex.tot, file = "propptsbin.YThighcomplex.tot2.rds")
#propptsbin.YThighcomplex.tot2 <- readRDS("propptsbin.YThighcomplex.tot2.rds")

plot(propptsbin.YThighcomplex.tot$use.bin, propptsbin.YThighcomplex.tot$prop.pts)
cor(propptsbin.YThighcomplex.tot$use.bin, propptsbin.YThighcomplex.tot$prop.pts, method = 'spearman', use = "complete.obs")
#0.33

### ---- BC Low Disturbance ------
#### Simple model ------
gc()
testBClowsimple.ud <- mod2UD(mod = m.simple.bclow_dist_train, envlayers = rastLand2020.fine, 
                              studyArea = vect(sArea.BClow))
plot(testBClowsimple.ud)

gc()
testBClowsimple.prop <- make_prop_use(dat.testBClow, crs, testBClowsimple.ud)

gc()
used.bin.testBClowsimple <- make_map_bins(testBClowsimple.prop)

colnames(dat.testBClow) <- make.unique(colnames(dat.testBClow))

propptsbin.BClowsimple.tot <- pts_in_bins_tot(dat.testBClow, testBClowsimple.ud)

#saveRDS(propptsbin.BClowsimple.tot, file = "propptsbin.BClowsimple.tot.rds")
#propptsbin.BClowsimple.tot <- readRDS("propptsbin.BClowsimple.tot.rds")

plot(propptsbin.BClowsimple.tot$use.bin, propptsbin.BClowsimple.tot$prop.pts)
cor(propptsbin.BClowsimple.tot$use.bin, propptsbin.BClowsimple.tot$prop.pts, method = 'spearman', use = "complete.obs")
#0.95


### Complex model ------
gc()
testBClowcomplex.ud <- mod2UD(mod = m.complex.bclow_dist_train, envlayers = rastLand2020.fine, 
                               studyArea = vect(sArea.BClow))
plot(testBClowcomplex.ud)

gc()
testBClowcomplex.prop <- make_prop_use(dat.testBClow, crs, testBClowcomplex.ud)

gc()
used.bin.testBClowcomplex <- make_map_bins(testBClowcomplex.prop)

colnames(dat.testBClow) <- make.unique(colnames(dat.testBClow))

propptsbin.BClowcomplex.tot <- pts_in_bins_tot(dat.testBClow, testBClowcomplex.ud)

#saveRDS(propptsbin.BClowcomplex.tot, file = "propptsbin.BClowcomplex.tot.rds")
#propptsbin.BClowcomplex.tot <- readRDS("propptsbin.BClowcomplex.tot.rds")

plot(propptsbin.BClowcomplex.tot$use.bin, propptsbin.BClowcomplex.tot$prop.pts)
cor(propptsbin.BClowcomplex.tot$use.bin, propptsbin.BClowcomplex.tot$prop.pts, method = 'spearman', use = "complete.obs")
#0.96



### ---- YT Low Disturbance ------
#### Simple model ------
gc()
testYTlowsimple.ud <- mod2UD(mod = m.simple.ytlow_dist_train, envlayers = rastLand2020.fine, 
                              studyArea = vect(sArea.YTlow))
plot(testYTlowsimple.ud)

gc()
testYTlowsimple.prop <- make_prop_use(dat.testYTlow, crs, testYTlowsimple.ud)

gc()
used.bin.testYTlowsimple <- make_map_bins(testYTlowsimple.prop)

colnames(dat.testYTlow) <- make.unique(colnames(dat.testYTlow))

propptsbin.YTlowsimple.tot <- pts_in_bins_tot(dat.testYTlow, testYTlowsimple.ud)

#saveRDS(propptsbin.YTlowsimple.tot, file = "propptsbin.YTlowsimple.tot.rds")
#propptsbin.YTlowsimple.tot <- readRDS("propptsbin.YTlowsimple.tot.rds")

plot(propptsbin.YTlowsimple.tot$use.bin, propptsbin.YTlowsimple.tot$prop.pts)
cor(propptsbin.YTlowsimple.tot$use.bin, propptsbin.YTlowsimple.tot$prop.pts, method = 'spearman', use = "complete.obs")
#0.733


### Complex model ------
### ---- Year validation
gc()
testYTlowcomplex.ud <- mod2UD(mod = m.complex.ytlow_dist_train, envlayers = rastLand2020.fine, 
                               studyArea = vect(sArea.YTlow))
plot(testYTlowcomplex.ud)

gc()
testYTlowcomplex.prop <- make_prop_use(dat.testYTlow, crs, testYTlowcomplex.ud)

gc()
used.bin.testYTlowcomplex <- make_map_bins(testYTlowcomplex.prop)

colnames(dat.testYTlow) <- make.unique(colnames(dat.testYTlow))

propptsbin.YTlowcomplex.tot <- pts_in_bins_tot(dat.testYTlow, testYTlowcomplex.ud)

#saveRDS(propptsbin.YTlowcomplex.tot, file = "propptsbin.YTlowcomplex.tot.rds")
#propptsbin.YTlowcomplex.tot <- readRDS("propptsbin.YTlowcomplex.tot.rds")

plot(propptsbin.YTlowcomplex.tot$use.bin, propptsbin.YTlowcomplex.tot$prop.pts)
cor(propptsbin.YTlowcomplex.tot$use.bin, propptsbin.YTlowcomplex.tot$prop.pts, method = 'spearman', use = "complete.obs")
#0.89



### ---- BC  ------
#### Simple model ------
gc()
testBCsimple.ud <- mod2UD(mod = m.simple.bc.train, envlayers = rastLand2020.fine, 
                             studyArea = vect(sArea.BC))
plot(testBCsimple.ud)

gc()
testBCsimple.prop <- make_prop_use(dat.testBC, crs, testBCsimple.ud)

gc()
used.bin.testBCsimple <- make_map_bins(testBCsimple.prop)

colnames(dat.testBC) <- make.unique(colnames(dat.testBC))

propptsbin.BCsimple.tot <- pts_in_bins_tot(dat.testBC, testBCsimple.ud)

#saveRDS(propptsbin.BCsimple.tot, file = "propptsbin.BCsimple.tot.rds")
#propptsbin.BCsimple.tot <- readRDS("propptsbin.BCsimple.tot.rds")

plot(propptsbin.BCsimple.tot$use.bin, propptsbin.BCsimple.tot$prop.pts)
cor(propptsbin.BCsimple.tot$use.bin, propptsbin.BCsimple.tot$prop.pts, method = 'spearman', use = "complete.obs")
#0.78


### Complex model ------
gc()
testBCcomplex.ud <- mod2UD(mod = m.complex.bc.train, envlayers = rastLand2020.fine, 
                              studyArea = vect(sArea.BC))
plot(testBCcomplex.ud)

gc()
testBCcomplex.prop <- make_prop_use(dat.testBC, crs, testBCcomplex.ud)

gc()
used.bin.testBCcomplex <- make_map_bins(testBCcomplex.prop)

colnames(dat.testBC) <- make.unique(colnames(dat.testBC))

propptsbin.BCcomplex.tot <- pts_in_bins_tot(dat.testBC, testBCcomplex.ud)

#saveRDS(propptsbin.BCcomplex.tot, file = "propptsbin.BCcomplex.tot.rds")
#propptsbin.BCcomplex.tot <- readRDS("propptsbin.BCcomplex.tot.rds")

plot(propptsbin.BCcomplex.tot$use.bin, propptsbin.BCcomplex.tot$prop.pts)
cor(propptsbin.BCcomplex.tot$use.bin, propptsbin.BCcomplex.tot$prop.pts, method = 'spearman', use = "complete.obs")
#0.98


### ---- YT  ------
#### Simple model ------
gc()
testYTsimple.ud <- mod2UD(mod = m.simple.yt.train, envlayers = rastLand2020.fine, 
                             studyArea = vect(sArea.YT))
plot(testYTsimple.ud)

gc()
testYTsimple.prop <- make_prop_use(dat.testYT, crs, testYTsimple.ud)

gc()
used.bin.testYTsimple <- make_map_bins(testYTsimple.prop)

colnames(dat.testYT) <- make.unique(colnames(dat.testYT))

propptsbin.YTsimple.tot <- pts_in_bins_tot(dat.testYT, testYTsimple.ud)

#saveRDS(propptsbin.YTsimple.tot, file = "propptsbin.YTsimple.tot.rds")
#propptsbin.YTsimple.tot <- readRDS("propptsbin.YTsimple.tot.rds")

plot(propptsbin.YTsimple.tot$use.bin, propptsbin.YTsimple.tot$prop.pts)
cor(propptsbin.YTsimple.tot$use.bin, propptsbin.YTsimple.tot$prop.pts, method = 'spearman', use = "complete.obs")
#0.92

### Complex model ------
### ---- Year validation
gc()
testYTcomplex.ud <- mod2UD(mod = m.complex.yt.train, envlayers = rastLand2020.fine, 
                              studyArea = vect(sArea.YT))
plot(testYTcomplex.ud)

gc()
testYTcomplex.prop <- make_prop_use(dat.testYT, crs, testYTcomplex.ud)

gc()
used.bin.testYTcomplex <- make_map_bins(testYTcomplex.prop)

colnames(dat.testYT) <- make.unique(colnames(dat.testYT))

propptsbin.YTcomplex.tot <- pts_in_bins_tot(dat.testYT, testYTcomplex.ud)

#saveRDS(propptsbin.YTcomplex.tot, file = "propptsbin.YTcomplex.tot.rds")
#propptsbin.YTcomplex.tot <- readRDS("propptsbin.YTcomplex.tot.rds")

plot(propptsbin.YTcomplex.tot$use.bin, propptsbin.YTcomplex.tot$prop.pts)
cor(propptsbin.YTcomplex.tot$use.bin, propptsbin.YTcomplex.tot$prop.pts, method = 'spearman', use = "complete.obs")
#0.85

## PLOTTING VALIDATIONS -----------------
### ----- Full area
prop_list1 <- list(
  "YT Complex" = propptsbin.YTcomplex.tot,
  "YT Simple"  = propptsbin.YTsimple.tot,
  "BC Complex" = propptsbin.BCcomplex.tot,
  "BC Simple"  = propptsbin.BCsimple.tot
)

combined_df1 <- bind_rows(
  lapply(names(prop_list1), function(name) {
    df <- prop_list1[[name]]
    df$Variation <- name
    df
  })
)

soft_colors1 <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")

### ---- Plot 1 ----
ggplot(combined_df1, aes(x = use.bin, y = prop.pts, color = Variation)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_color_manual(values = soft_colors1) +
  scale_x_continuous(breaks = 0:9) +
  labs(
    x = "Use bin",
    y = "Area adjusted proportion of used points",
    color = "Model"
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )





### ---- Plot 2 Data List ----
prop_list2 <- list(
  "YT Low Complex" = propptsbin.YTlowcomplex.tot,
  "YT Low Simple"  = propptsbin.YTlowsimple.tot,
  "BC Low Complex" = propptsbin.BClowcomplex.tot,
  "BC Low Simple"  = propptsbin.BClowsimple.tot
)

combined_df2 <- bind_rows(
  lapply(names(prop_list2), function(name) {
    df <- prop_list2[[name]]
    df$Variation <- name
    df
  })
)

soft_colors2 <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")

### ---- Plot 2 ----
ggplot(combined_df2, aes(x = use.bin, y = prop.pts, color = Variation)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_color_manual(values = soft_colors2) +
  scale_x_continuous(breaks = 0:9) +
  labs(
    x = "Use bin",
    y = "Area adjusted proportion of used points",
    color = "Model"
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )



### ---- Plot 3 Data List ----
prop_list3 <- list(
  "YT High Complex (SL)" = propptsbin.YThighcomplex.tot,
  "YT High Simple (SL)"  = propptsbin.YThighsimple.tot,
  "YT High Complex (KZ)" = propptsbin.YThighcomplex.tot2,
  "YT High Simple (KZ)"  = propptsbin.YThighsimple.tot2,
  "BC High Complex" = propptsbin.BChighcomplex.tot,
  "BC High Simple"  = propptsbin.BChighsimple.tot
)

combined_df3 <- bind_rows(
  lapply(names(prop_list3), function(name) {
    df <- prop_list3[[name]]
    df$Variation <- name
    df
  })
)

soft_colors3 <- c(
  "#66c2a5",  
  "#fc8d62",  
  "#8da0cb",  
  "#e78ac3",  
  "#a6d854",  
  "#ffd92f"   
)

### ---- Plot 3 ----
ggplot(combined_df3, aes(x = use.bin, y = prop.pts, color = Variation)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_color_manual(values = soft_colors3) +
  scale_x_continuous(breaks = 0:9) +
  labs(
    x = "Use bin",
    y = "Area adjusted proportion of used points",
    color = "Model"
  ) +
  theme_classic(base_size = 13) +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

