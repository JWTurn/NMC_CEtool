# Validation ----
# Julie Turner & Peter Baker
# Started: August 13 2025

require(data.table)
require(glmmTMB)
require(amt)
# library(terra)
# library(sf)
library(dplyr)
library(ggplot2)

# my functions
lapply(dir('R', '*.R', full.names = TRUE), source)

### Input data ----
raw <- file.path('data', 'raw')
derived <- file.path('data', 'derived')

finedat <- readRDS(file.path(derived, 'dat_iSSA_updated.RDS'))
finedat.20 <- finedat[,.SD[1:21], by = .(indiv_step_id)]
finedat.15 <- finedat[,.SD[1:16], by = .(indiv_step_id)]
finedat.10 <- finedat[,.SD[1:11], by = .(indiv_step_id)]

int.yr <- 2015
set.seed(53)


#First prioritizing year-round full model, and year-round simple model for the year- validation
# Year- validation -----
## 2019/2020 ----
#Training dataset is all data before 2019/2020
finemod.train <- finedat.10[!(year %in% c(2019, 2020, 2021, 2022, 2023, 2024, 2025))]
finemod.train[,indiv_step_id := as.factor(indiv_step_id)]
finemod.train[,prov := as.factor(prov)]
finemod.train[,year:=as.factor(year)]
finemod.train[,area := as.factor(area)]
finemod.train[,id := as.factor(id)]

# fit models ----
gc()
finemod.train_full <- glmmTMB(case_ ~ -1 +
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
                           family = poisson(), data = finemod.train[,`:=`(year = as.factor(year), 
                                                                        indiv_step_id = as.factor(indiv_step_id),
                                                                        id = as.factor(id))],
                           map= list(theta = factor(c(NA,1:15))),
                           start = list(theta =c(log(1000), seq(0,0, length.out = 15))), 
                           verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)



saveRDS(m, file.path(derived, paste0('mod_nwt_train_selmove_', 
                                     int.yr, '-', int.yr+5,
                                     '_HPC.RDS')))
#######

m.indivs <- glmmTMB(form,
                    family = poisson(), data = dat.train,
                    map= list(theta = factor(c(NA,1:15))),
                    start = list(theta =c(log(1000), seq(0,0, length.out = 15))),
                    verbose = TRUE, control = glmmTMBControl(rank_check = "adjust")
)

summary(m.indivs)
m.indivs$frame <- m.indivs$frame[0, ]
saveRDS(m.indivs, file.path(derived, paste0('mod_train_selmove_', 
                                            int.yr, '-', int.yr+5,
                                            '_HPC.RDS')))

## bc -----
m.bc <- glmmTMB(form,
                family = poisson(), data = bc.train,
                map= list(theta = factor(c(NA,1:15))),
                start = list(theta =c(log(1000), seq(0,0, length.out = 15))),
                verbose = TRUE, control = glmmTMBControl(rank_check = "adjust")
)

summary(m.bc)
m.bc$frame <- m.bc$frame[0, ]
saveRDS(m.bc, file.path(derived, paste0('mod_bc_train_selmove_', 
                                        int.yr, '-', int.yr+5,
                                        '_HPC.RDS')))
