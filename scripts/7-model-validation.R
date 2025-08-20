# Validation ----
# Julie Turner & Peter Baker
# Started: August 13 2025

require(data.table)
require(glmmTMB)
require(amt)
library(terra)
library(sf)
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

coarsedat <- readRDS(file.path(derived, 'dat_iSSA_coarse.RDS'))
coarsedat.20 <- coarsedat[,.SD[1:21], by = .(indiv_step_id)]

int.yr <- 2015
set.seed(53)


#First prioritizing year-round full model, and year-round simple model for the year- validation
# Year- validation -----
## 2022+ ----
#Training dataset is all data before 2019/2020
finemod.train <- finedat.10[!(year %in% c(2022, 2023, 2024, 2025))]
finemod.train[,indiv_step_id := as.factor(indiv_step_id)]
finemod.train[,prov := as.factor(prov)]
finemod.train[,year:=as.factor(year)]
finemod.train[,area := as.factor(area)]
finemod.train[,id := as.factor(id)]

finemod.train.15 <- finedat.15[!(year %in% c(2022, 2023, 2024, 2025))]
finemod.train.15[,indiv_step_id := as.factor(indiv_step_id)]
finemod.train.15[,prov := as.factor(prov)]
finemod.train.15[,year:=as.factor(year)]
finemod.train.15[,area := as.factor(area)]
finemod.train.15[,id := as.factor(id)]

coarsemod.train <- coarsedat.20[!(year %in% c(2022, 2023, 2024, 2025))]
coarsemod.train[,indiv_step_id := as.factor(indiv_step_id)]
coarsemod.train[,prov := as.factor(prov)]
coarsemod.train[,year:=as.factor(year)]
coarsemod.train[,area := as.factor(area)]
coarsemod.train[,id := as.factor(id)]



### Functions -----
make_study_area <- function(dat, crs, buff){
  coords<- dat%>%st_as_sf(coords = c('x2_','y2_'))%>%
    st_set_crs(crs)
  ## buffer around points to get an idea of extent of study area 
  print('buffering')
  studyArea <- st_buffer(coords, dist = buff)
  print('union-ing')
  sa.union <- st_union(studyArea)
  return(sa.union)
}

make_prop_use <- function(dat, crs, trast){
  dat.sub <- dat[case_==TRUE]
  dat.coords<- vect(cbind(dat.sub$x1_, dat.sub$y1_), crs =crs, type = 'points')
  res <- res(trast)[[1]]
  rast<- rasterize(dat.coords, trast, fun = 'sum')/(res*res)
  return(rast)
}

make_map_bins <- function(map){
  breaks <- global(map, quantile, na.rm = T, probs = seq(0,1,.1))
  v.breaks <- unname(breaks)
  t.breaks <- as.vector(t(v.breaks))
  binned <- as.numeric(classify(map, t.breaks, include.lowest=TRUE, brackets=TRUE))
  return(binned)
}


pts_in_bins <- function(dat, bin.ud){
  DT <- dat[id!='' & case_ == TRUE]
  DT[, use.bin:= terra::extract(bin.ud, cbind(.SD))[,-1],
     .SDcols = c('x1_','y1_')]
  npts <- DT[,.(npts = .N), by= .(id)]
  mrg <- merge(DT, npts, by = 'id')
  propptsbin <- mrg[,.(prop.pts = .N/npts), by = .(use.bin, id)]
  return(propptsbin)
}


pts_in_bins_tot <- function(dat, bin.ud){
  DT <- dat[id!='' & case_ == TRUE]
  DT[, use.bin:= terra::extract(bin.ud, cbind(.SD))[,-1],
     .SDcols = c('x1_','y1_')]
  npts <- DT[,.(npts = .N)][[1]]
  propptsbin.tot <- DT[,.(prop.pts = .N/npts), by = .(use.bin)]
  return(propptsbin.tot)
}

mod2UD <- function(modpath, envlayers, studyArea, pde.saveName = NULL, map.saveName = NULL){
  mod <- readRDS(modpath)
  mod.tab <- make_betas_tab(mod)
  pde <- make_pde(mod.tab, envlayers, saveName = pde.saveName)
  map.pde <- as.numeric(make_pde_map(pde, studyArea, saveName = map.saveName))
  return(map.pde)}

make_betas_tab <- function(mod){
  sum <- broom.mixed::tidy(mod, effect = 'fixed')
  as.data.table(sum)[,.(term, estimate)]
}


crs <- st_crs(3154)
buffer <- 10000

gc()

## Fit models ----
### Full Model ----
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


summary(finemod.train_full)
# save model output
saveRDS(finemod.train_full, "finemod.train_full.RDS")
finemod.train_full <- file.path("finemod.train_full.RDS")

### Simple Model ----
gc()
finemod.train_simple <- glmmTMB(case_ ~ -1 +
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
                          family = poisson(), data = finemod.train.15[,`:=`(year = as.factor(year), 
                                                                       indiv_step_id = as.factor(indiv_step_id),
                                                                       id = as.factor(id))],
                          map= list(theta = factor(c(NA,1:10))),
                          start = list(theta =c(log(1000), seq(0,0, length.out = 10))), 
                          verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(finemod.train_simple)
# save model output
saveRDS(finemod.train_simple, "finemod.train_simple.RDS")
finemod.train_simple <- file.path("finemod.train_simple.RDS")

### Full mod roads comb ----
gc()
finemod.train_fullroadscomb <- glmmTMB(case_ ~ -1 +
                                I(log((sl_+1)/1000)) +
                                I(cos(ta_)) +
                                I(log((sl_+1)/1000)):I(cos(ta_)) +
                                prop_forest_250_end + prop_forest_250_end:prop_forest_1000_end +
                                prop_veg_250_end + prop_veg_250_end:prop_veg_1000_end +
                                I(log(ts_fires_250_end+1)) + I(log(ts_fires_250_end+1)):I(log(ts_fires_1000_end+1)) +
                                I(log(ts_harv250_end+1)) + I(log(ts_harv250_end+1)):I(log(ts_harv1000_end+1)) +
                                I(log((dist_road_250_end+1)/1000)) + I(log((dist_road_250_end+1)/1000)):I(log((dist_road_1000_end+1)/1000)) +
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
                                (0 + I(log((dist_road_250_end+1)/1000))|id) + 
                                (0 + I(log((dist_lfother_250_end+1)/1000))|id) +
                                (0 + I(log((dist_otherpoly_250_end+1)/1000))|id) + 
                                (0 + I(log((dist_placer_250_end+1)/1000))|id) + 
                                (0 + I(log((dist_quartz_250_end+1)/1000))|id) + 
                                (0 + I(log((dist_othermines_250_end+1)/1000))|id) + 
                                
                                (1|year),
                              family = poisson(), data = finemod.train[,`:=`(year = as.factor(year), 
                                                                             indiv_step_id = as.factor(indiv_step_id),
                                                                             id = as.factor(id))],
                              map= list(theta = factor(c(NA,1:14))),
                              start = list(theta =c(log(1000), seq(0,0, length.out = 14))), 
                              verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(finemod.train_fullroadscomb)
# save model output
saveRDS(finemod.train_fullroadscomb, "finemod.train_fullroadscomb.RDS")
finemod.train_fullroadscomb <- file.path("finemod.train_fullroadscomb.RDS")

### Coarse mod, pa, fire60 ----
gc()
coarsemod.train.pafire60 <- glmmTMB(case_ ~ -1 +
                              I(log((sl_+1)/1000)) +
                              I(cos(ta_)) +
                              I(log((sl_+1)/1000)):I(cos(ta_)) +
                              prop_forest_1000_end + 
                              prop_veg_1000_end + 
                              
                              fire_60perc1000_end + 
                              harv_pa_1000_end + 
                              pa_paved_1000_end + 
                              pa_unpaved_1000_end + 
                              pa_lfother_1000_end + 
                              pa_otherpoly_1000_end + 
                              pa_placer_1000_end + 
                              pa_quartz_1000_end + 
                              pa_othermines_1000_end +
                              
                              (1|indiv_step_id) +
                              (0 + I(log((sl_ +1)/1000))|id) +
                              (0 + I(cos(ta_))|id) +
                              (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                              (0 + prop_forest_1000_end|id) +
                              (0 + prop_veg_1000_end|id) +
                              
                              (0 + fire_60perc1000_end|id) +
                              (0 + harv_pa_1000_end|id) + 
                              (0 + pa_paved_1000_end|id) + 
                              (0 + pa_unpaved_1000_end|id) + 
                              (0 + pa_lfother_1000_end|id) +
                              (0 + pa_otherpoly_1000_end|id) + 
                              (0 + pa_placer_1000_end|id) + 
                              (0 + pa_quartz_1000_end|id) + 
                              (0 + pa_othermines_1000_end|id) + 
                              
                              (1|year),
                            family = poisson(), data = coarsemod.train[,`:=`(year = as.factor(year), 
                                                                         indiv_step_id = as.factor(indiv_step_id),
                                                                         id = as.factor(id))],
                            map= list(theta = factor(c(NA,1:15))),
                            start = list(theta =c(log(1000), seq(0,0, length.out = 15))), 
                            verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(coarsemod.train.pafire60)
# save model output
saveRDS(coarsemod.train.pafire60, "coarsemod.train.pafire60.RDS")
#coarsemod.train.pafire60 <- readRDS("coarsemod.train.pafire60.RDS")
coarsemod.train.pafire60 <- file.path("coarsemod.train.pafire60.RDS")

#make_betas_tab(coarsemod.train.pafire60)



### Coarse mod, pa, fire2 ----
gc()
coarsemod.train.pafire2 <- glmmTMB(case_ ~ -1 +
                                      I(log((sl_+1)/1000)) +
                                      I(cos(ta_)) +
                                      I(log((sl_+1)/1000)):I(cos(ta_)) +
                                      prop_forest_1000_end + 
                                      prop_veg_1000_end + 
                                      
                                      fire_2perc1000_end + 
                                      harv_pa_1000_end + 
                                      pa_paved_1000_end + 
                                      pa_unpaved_1000_end + 
                                      pa_lfother_1000_end + 
                                      pa_otherpoly_1000_end + 
                                      pa_placer_1000_end + 
                                      pa_quartz_1000_end + 
                                      pa_othermines_1000_end +
                                      
                                      (1|indiv_step_id) +
                                      (0 + I(log((sl_ +1)/1000))|id) +
                                      (0 + I(cos(ta_))|id) +
                                      (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                                      (0 + prop_forest_1000_end|id) +
                                      (0 + prop_veg_1000_end|id) +
                                      
                                      (0 + fire_2perc1000_end|id) +
                                      (0 + harv_pa_1000_end|id) + 
                                      (0 + pa_paved_1000_end|id) + 
                                      (0 + pa_unpaved_1000_end|id) + 
                                      (0 + pa_lfother_1000_end|id) +
                                      (0 + pa_otherpoly_1000_end|id) + 
                                      (0 + pa_placer_1000_end|id) + 
                                      (0 + pa_quartz_1000_end|id) + 
                                      (0 + pa_othermines_1000_end|id) + 
                                      
                                      (1|year),
                                    family = poisson(), data = coarsemod.train[,`:=`(year = as.factor(year), 
                                                                                     indiv_step_id = as.factor(indiv_step_id),
                                                                                     id = as.factor(id))],
                                    map= list(theta = factor(c(NA,1:15))),
                                    start = list(theta =c(log(1000), seq(0,0, length.out = 15))), 
                                    verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(coarsemod.train.pafire2)
# save model output
saveRDS(coarsemod.train.pafire2, "coarsemod.train.pafire2.RDS")
#coarsemod.train.pafire2 <- readRDS("coarsemod.train.pafire2.RDS")
coarsemod.train.pafire2 <- file.path("coarsemod.train.pafire2.RDS")


### Coarse mod, lf dens, fire60 ----
gc()
coarsemod.train.lfdensfire60 <- glmmTMB(case_ ~ -1 +
                                     I(log((sl_+1)/1000)) +
                                     I(cos(ta_)) +
                                     I(log((sl_+1)/1000)):I(cos(ta_)) +
                                     prop_forest_1000_end + 
                                     prop_veg_1000_end + 
                                     
                                     fire_60perc1000_end + 
                                     harv_pa_1000_end + 
                                     dens_paved_1000_end + 
                                     dens_unpaved_1000_end + 
                                     dens_lfother_1000_end + 
                                     pa_otherpoly_1000_end + 
                                     pa_placer_1000_end + 
                                     pa_quartz_1000_end + 
                                     pa_othermines_1000_end +
                                     
                                     (1|indiv_step_id) +
                                     (0 + I(log((sl_ +1)/1000))|id) +
                                     (0 + I(cos(ta_))|id) +
                                     (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                                     (0 + prop_forest_1000_end|id) +
                                     (0 + prop_veg_1000_end|id) +
                                     
                                     (0 + fire_60perc1000_end|id) +
                                     (0 + harv_pa_1000_end|id) + 
                                     (0 + dens_paved_1000_end|id) + 
                                     (0 + dens_unpaved_1000_end|id) + 
                                     (0 + dens_lfother_1000_end|id) +
                                     (0 + pa_otherpoly_1000_end|id) + 
                                     (0 + pa_placer_1000_end|id) + 
                                     (0 + pa_quartz_1000_end|id) + 
                                     (0 + pa_othermines_1000_end|id) + 
                                     
                                     (1|year),
                                   family = poisson(), data = coarsemod.train[,`:=`(year = as.factor(year), 
                                                                                    indiv_step_id = as.factor(indiv_step_id),
                                                                                    id = as.factor(id))],
                                   map= list(theta = factor(c(NA,1:15))),
                                   start = list(theta =c(log(1000), seq(0,0, length.out = 15))), 
                                   verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(coarsemod.train.lfdensfire60)
# save model output
saveRDS(coarsemod.train.lfdensfire60, "coarsemod.train.lfdensfire60.RDS")
#coarsemod.train.lfdensfire60 <- readRDS("coarsemod.train.lfdensfire60.RDS")
coarsemod.train.lfdensfire60 <- file.path("coarsemod.train.lfdensfire60.RDS")

#make_betas_tab(coarsemod.train.lfdensfire60)




### Coarse mod, lf dens, fire2 ----
gc()
coarsemod.train.lfdensfire2 <- glmmTMB(case_ ~ -1 +
                                          I(log((sl_+1)/1000)) +
                                          I(cos(ta_)) +
                                          I(log((sl_+1)/1000)):I(cos(ta_)) +
                                          prop_forest_1000_end + 
                                          prop_veg_1000_end + 
                                          
                                          fire_2perc1000_end + 
                                          harv_pa_1000_end + 
                                          dens_paved_1000_end + 
                                          dens_unpaved_1000_end + 
                                          dens_lfother_1000_end + 
                                          pa_otherpoly_1000_end + 
                                          pa_placer_1000_end + 
                                          pa_quartz_1000_end + 
                                          pa_othermines_1000_end +
                                          
                                          (1|indiv_step_id) +
                                          (0 + I(log((sl_ +1)/1000))|id) +
                                          (0 + I(cos(ta_))|id) +
                                          (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                                          (0 + prop_forest_1000_end|id) +
                                          (0 + prop_veg_1000_end|id) +
                                          
                                          (0 + fire_2perc1000_end|id) +
                                          (0 + harv_pa_1000_end|id) + 
                                          (0 + dens_paved_1000_end|id) + 
                                          (0 + dens_unpaved_1000_end|id) + 
                                          (0 + dens_lfother_1000_end|id) +
                                          (0 + pa_otherpoly_1000_end|id) + 
                                          (0 + pa_placer_1000_end|id) + 
                                          (0 + pa_quartz_1000_end|id) + 
                                          (0 + pa_othermines_1000_end|id) + 
                                          
                                          (1|year),
                                        family = poisson(), data = coarsemod.train[,`:=`(year = as.factor(year), 
                                                                                         indiv_step_id = as.factor(indiv_step_id),
                                                                                         id = as.factor(id))],
                                        map= list(theta = factor(c(NA,1:15))),
                                        start = list(theta =c(log(1000), seq(0,0, length.out = 15))), 
                                        verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(coarsemod.train.lfdensfire2)
# save model output
saveRDS(coarsemod.train.lfdensfire2, "coarsemod.train.lfdensfire2.RDS")
#coarsemod.train.lfdensfire2 <- readRDS("coarsemod.train.lfdensfire2.RDS")
coarsemod.train.lfdensfire2 <- file.path("coarsemod.train.lfdensfire2.RDS")




### Coarse mod, simple, fire60 ----
gc()
coarsemod.train.simplefire60 <- glmmTMB(case_ ~ -1 +
                            I(log((sl_+1)/1000)) +
                            I(cos(ta_)) +
                            I(log((sl_+1)/1000)):I(cos(ta_)) +
                            prop_forest_1000_end + 
                            prop_veg_1000_end + 
                            fire_60perc1000_end + 
                            dens_lf_1000_end + 
                            dens_poly_1000_end + 
                            
                            (1|indiv_step_id) +
                            (0 + I(log((sl_ +1)/1000))|id) +
                            (0 + I(cos(ta_))|id) +
                            (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                            (0 + prop_forest_1000_end|id) +
                            (0 + prop_veg_1000_end|id) +
                            (0 + fire_60perc1000_end|id) +
                            (0 + dens_lf_1000_end|id) + 
                            (0 + dens_poly_1000_end|id) + 
                            
                            (1|year),
                          family = poisson(), data = coarsemod.train[,`:=`(year = as.factor(year), 
                                                                       indiv_step_id = as.factor(indiv_step_id),
                                                                       id = as.factor(id))],
                          map= list(theta = factor(c(NA,1:9))),
                          start = list(theta =c(log(1000), seq(0,0, length.out = 9))), 
                          verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(coarsemod.train.simplefire60)
# save model output
saveRDS(coarsemod.train.simplefire60, "coarsemod.train.simplefire60.RDS")
#coarsemod.train.simplefire60 <- readRDS("coarsemod.train.simplefire60.RDS")
coarsemod.train.simplefire60 <- file.path("coarsemod.train.simplefire60.RDS")
#make_betas_tab(coarsemod.train.simplefire60)


## Create Test Data ---- 
finemod.test_full <- finedat.10[year %in% c(2022, 2023, 2024, 2025) & case_ == TRUE]
finemod.test_simple <- finedat.15[year %in% c(2022, 2023, 2024, 2025) & case_ == TRUE]
finemod.test_fullroadscomb <- finedat.10[year %in% c(2022, 2023, 2024, 2025) & case_ == TRUE]
coarsemod.test <- coarsedat.20[year %in% c(2022, 2023, 2024, 2025) & case_ == TRUE]

#Make study area
sArea.finemod <- make_study_area(finemod.test_full, crs, buff = buffer)
sArea.coarsemod <- make_study_area(coarsemod.test_pafire60, crs, buff = buffer)

## Validation -----
### Creating fine raster stacks ----
#Loading layers to stack, starting with prop_land
prop_wettreed <- rast(file.path(derived, 'prop_land', 'prop_land_split', 'prop_wet_treed_250_2020.tif'))
prop_needleleaf <- rast(file.path(derived, 'prop_land', 'prop_land_split', 'prop_needleleaf_250_2020.tif'))
prop_deciduous <- rast(file.path(derived, 'prop_land', 'prop_land_split', 'prop_deciduous_250_2020.tif'))
prop_mixed <- rast(file.path(derived, 'prop_land', 'prop_land_split', 'prop_mixed_250_2020.tif'))

prop_bryoids <- rast(file.path(derived, 'prop_land', 'prop_land_split', 'prop_bryoids_250_2020.tif'))
prop_herbs <- rast(file.path(derived, 'prop_land', 'prop_land_split', 'prop_herbs_250_2020.tif'))
prop_shrub  <- rast(file.path(derived, 'prop_land', 'prop_land_split', 'prop_shrub_250_2020.tif'))


# prop_forest = sum of 4 layers
prop_forest <- prop_wettreed + prop_needleleaf + prop_deciduous + prop_mixed

# prop_veg = sum of 3 layers
prop_veg <- prop_bryoids + prop_herbs + prop_shrub


#Harvest
harv <- rast(file.path(derived, 'harv250', 'harv250_2020.tif'))

#Fires
fire <- rast(file.path(derived, 'fireRasters250m', 'fires_2020.tif'))

# Load rasters
harv_year <- rast(file.path(derived, 'harv250', 'harv250_2020.tif'))
fire_year <- rast(file.path(derived, 'fireRasters250m', 'fires_2020.tif'))

reference_year <- 2020

harv_ts <- reference_year - harv_year
fire_ts <- reference_year - fire_year
harv_ts <- ifel(is.na(harv_ts), 100, harv_ts)
fire_ts <- ifel(is.na(fire_ts), 200, fire_ts)

distpaved <- rast(file.path(derived, 'dist_disturb_250m', 'paved_distance_250m_2020.tif'))
distunpaved <- rast(file.path(derived, 'dist_disturb_250m', 'unpaved_distance_250m_2020.tif'))
distlfother <- rast(file.path(derived, 'dist_disturb_250m', 'otherlf_distance_250m_2020.tif'))

distotherpoly <- rast(file.path(derived, 'dist_disturb_250m', 'otherpoly_distance_250m_2020.tif'))
distplacer <- rast(file.path(derived, 'dist_disturb_250m', 'placer_distance_250m_2020.tif'))
distquartz <- rast(file.path(derived, 'dist_disturb_250m', 'quartz_distance_250m_2020.tif'))
distothermines <- rast(file.path(derived, 'dist_disturb_250m', 'othermines_distance_250m_2020.tif'))

dist_lf <- distpaved + distunpaved + distlfother
dist_poly <- distotherpoly + distplacer + distquartz + distothermines
dist_road <- distpaved + distunpaved


# Load 1000m linear feature distance rasters
#Loading layers to stack, starting with prop_land
prop_wettreed_1000 <- rast(file.path(derived, 'prop_land', 'prop_land_split', 'prop_wet_treed_1000_2020.tif'))
prop_needleleaf_1000 <- rast(file.path(derived, 'prop_land', 'prop_land_split', 'prop_needleleaf_1000_2020.tif'))
prop_deciduous_1000 <- rast(file.path(derived, 'prop_land', 'prop_land_split', 'prop_deciduous_1000_2020.tif'))
prop_mixed_1000 <- rast(file.path(derived, 'prop_land', 'prop_land_split', 'prop_mixed_1000_2020.tif'))

prop_bryoids_1000 <- rast(file.path(derived, 'prop_land', 'prop_land_split', 'prop_bryoids_1000_2020.tif'))
prop_herbs_1000 <- rast(file.path(derived, 'prop_land', 'prop_land_split', 'prop_herbs_1000_2020.tif'))
prop_shrub_1000  <- rast(file.path(derived, 'prop_land', 'prop_land_split', 'prop_shrub_1000_2020.tif'))


# prop_forest = sum of 4 layers
prop_forest_1000 <- prop_wettreed_1000 + prop_needleleaf_1000 + prop_deciduous_1000 + prop_mixed_1000

# prop_veg = sum of 3 layers
prop_veg_1000 <- prop_bryoids_1000 + prop_herbs_1000 + prop_shrub_1000


distpaved_1000 <- rast(file.path(derived, 'dist_disturb_1000m', 'paved_distance_1000m_2020.tif'))
distunpaved_1000 <- rast(file.path(derived, 'dist_disturb_1000m', 'unpaved_distance_1000m_2020.tif'))
distlfother_1000 <- rast(file.path(derived, 'dist_disturb_1000m', 'otherlf_distance_1000m_2020.tif'))

# Load 1000m PA feature distance rasters
distotherpoly_1000 <- rast(file.path(derived, 'dist_disturb_1000m', 'otherpoly_distance_1000m_2020.tif'))
distplacer_1000 <- rast(file.path(derived, 'dist_disturb_1000m', 'placer_distance_1000m_2020.tif'))
distquartz_1000 <- rast(file.path(derived, 'dist_disturb_1000m', 'quartz_distance_1000m_2020.tif'))
distothermines_1000 <- rast(file.path(derived, 'dist_disturb_1000m', 'othermines_distance_1000m_2020.tif'))

papaved_1000 <- rast(file.path(derived, 'pa_disturb_1000m', 'paved_pa_1000m_2020.tif'))
paunpaved_1000 <- rast(file.path(derived, 'pa_disturb_1000m', 'unpaved_pa_1000m_2020.tif'))
palfother_1000 <- rast(file.path(derived, 'pa_disturb_1000m', 'otherlf_pa_1000m_2020.tif'))
denspaved_1000 <- rast(file.path(derived, 'dens_disturb_1000m', 'paved_dens_1000m_2020.tif'))
densunpaved_1000 <- rast(file.path(derived, 'dens_disturb_1000m', 'unpaved_dens_1000m_2020.tif'))
denslfother_1000 <- rast(file.path(derived, 'dens_disturb_1000m', 'otherlf_dens_1000m_2020.tif'))

paotherpoly_1000 <- rast(file.path(derived, 'pa_disturb_1000m', 'otherpoly_pa_1000m_2020.tif'))
paplacer_1000 <- rast(file.path(derived, 'pa_disturb_1000m', 'placer_pa_1000m_2020.tif'))
paquartz_1000 <- rast(file.path(derived, 'pa_disturb_1000m', 'quartz_pa_1000m_2020.tif'))
paothermines_1000 <- rast(file.path(derived, 'pa_disturb_1000m', 'othermines_pa_1000m_2020.tif'))
densotherpoly_1000 <- rast(file.path(derived, 'dens_disturb_1000m', 'otherpoly_dens_1000m_2020.tif'))
densplacer_1000 <- rast(file.path(derived, 'dens_disturb_1000m', 'placer_dens_1000m_2020.tif'))
densquartz_1000 <- rast(file.path(derived, 'dens_disturb_1000m', 'quartz_dens_1000m_2020.tif'))
densothermines_1000 <- rast(file.path(derived, 'dens_disturb_1000m', 'othermines_dens_1000m_2020.tif'))
densharv_1000 <- rast(file.path(derived, 'harv_dens_1000m', 'harvdens_1000m_2020.tif'))


dist_lf_1000  <- distpaved_1000  + distunpaved_1000  + distlfother_1000 
dist_poly_1000  <- distotherpoly_1000  + distplacer_1000  + distquartz_1000  + distothermines_1000 
dist_road_1000 <- distpaved_1000 + distunpaved_1000

dens_lf_1000  <- denspaved_1000  + densunpaved_1000  + denslfother_1000 
dens_poly_1000  <- densotherpoly_1000  + densplacer_1000  + densquartz_1000  + densothermines_1000 + densharv_1000


# Load 1000m harvest and fire year rasters
harv_year_1000 <- rast(file.path(derived, 'harv1000', 'harv1000_2020.tif'))
fire_year_1000 <- rast(file.path(derived, 'fireRasters1000m', 'fires_2020.tif'))

# Compute time-since rasters, replacing NAs with large default values
reference_year <- 2020
tsharv_1000 <- ifel(is.na(reference_year - harv_year_1000), 100, reference_year - harv_year_1000)
tsfire_1000 <- ifel(is.na(reference_year - fire_year_1000), 200, reference_year - fire_year_1000)

paharv_1000 <- rast(file.path(derived, 'harv_pa_1000m', 'harvpres_1000m_2020.tif'))
propfire60 <- rast(file.path(derived, 'perc_burned_1000', 'burned60_1000m_2020.tif'))
propfire2 <- rast(file.path(derived, 'perc_burned_1000', 'burned2_1000m_2020.tif'))

plot(tsfire_1000)
plot(issaArea, add = TRUE, border = "red", lwd = 2)




#Disaggrigating so I can stack w fine scale
distpaved_1000_disagg <- disagg(distpaved_1000, fact = 4, method = "near")
distunpaved_1000_disagg <- disagg(distunpaved_1000, fact = 4, method = "near")
distlfother_1000_disagg <- disagg(distlfother_1000, fact = 4, method = "near")

distotherpoly_1000_disagg <- disagg(distotherpoly_1000, fact = 4, method = "near")
distplacer_1000_disagg <- disagg(distplacer_1000, fact = 4, method = "near")
distquartz_1000_disagg <- disagg(distquartz_1000, fact = 4, method = "near")
distothermines_1000_disagg <- disagg(distothermines_1000, fact = 4, method = "near")

dist_lf_1000_disagg <- disagg(dist_lf_1000, fact = 4, method = "near")
dist_poly_1000_disagg <- disagg(dist_poly_1000, fact = 4, method = "near")

tsharv_1000_disagg <- disagg(tsharv_1000, fact = 4, method = "near")
tsfire_1000_disagg <- disagg(tsfire_1000, fact = 4, method = "near")

prop_forest_1000_disagg <- disagg(prop_forest_1000, fact = 4, method = "near")
prop_veg_1000_disagg <- disagg(prop_veg_1000, fact = 4, method = "near")

#Stacking
# Get the extent of a 250m raster to use for cropping 1000m rasters, they were slightly off
crop_extent <- ext(prop_forest) 

prop_forest_1000_disagg   <- crop(prop_forest_1000_disagg, crop_extent)
prop_veg_1000_disagg      <- crop(prop_veg_1000_disagg, crop_extent)
distpaved_1000_disagg     <- crop(distpaved_1000_disagg, crop_extent)
distunpaved_1000_disagg   <- crop(distunpaved_1000_disagg, crop_extent)
distlfother_1000_disagg   <- crop(distlfother_1000_disagg, crop_extent)
distotherpoly_1000_disagg <- crop(distotherpoly_1000_disagg, crop_extent)
distplacer_1000_disagg    <- crop(distplacer_1000_disagg, crop_extent)
distquartz_1000_disagg    <- crop(distquartz_1000_disagg, crop_extent)
distothermines_1000_disagg<- crop(distothermines_1000_disagg, crop_extent)
tsharv_1000_disagg        <- crop(tsharv_1000_disagg, crop_extent)
tsfire_1000_disagg        <- crop(tsfire_1000_disagg, crop_extent)

dist_lf_1000_disagg       <- crop(dist_lf_1000_disagg, crop_extent)
dist_poly_1000_disagg     <- crop(dist_poly_1000_disagg, crop_extent)
dist_road_1000_disagg     <- crop(dist_road_1000_disagg, crop_extent)

land2020 <- c(
  prop_forest, prop_veg, harv_ts, fire_ts,
  distpaved, distunpaved, distlfother, distotherpoly, distplacer, distquartz, distothermines,
  prop_forest_1000_disagg, prop_veg_1000_disagg,
  distpaved_1000_disagg, distunpaved_1000_disagg, distlfother_1000_disagg, distotherpoly_1000_disagg, distplacer_1000_disagg,
  distquartz_1000_disagg, distothermines_1000_disagg,
  tsharv_1000_disagg, tsfire_1000_disagg, dist_lf, dist_lf_1000_disagg, dist_poly, dist_poly_1000_disagg, dist_road, dist_road_1000_disagg
)


land2020_coarse <- c( 
  prop_forest_1000, prop_veg_1000,
  papaved_1000, paunpaved_1000, palfother_1000, paotherpoly_1000, paplacer_1000,
  paquartz_1000, paothermines_1000,
  paharv_1000, propfire60, propfire2, denspaved_1000, densunpaved_1000, denslfother_1000, dens_lf_1000, dens_poly_1000
)

gc()
# Rename layers
names(land2020) <- c(
  "prop_forest", "prop_veg", "harv_ts", "fire_ts",
  "distpaved", "distunpaved", "distlfother", "distotherpoly", "distplacer", "distquartz", "distothermines",
  "prop_forest_1000", "prop_veg_1000",
  "distpaved_1000", "distunpaved_1000", "distlfother_1000", "distotherpoly_1000", "distplacer_1000",
  "distquartz_1000", "distothermines_1000",
  "tsharv_1000", "tsfire_1000", "dist_lf", "dist_lf_1000", "dist_poly", "dist_poly_1000", "dist_road", "dist_road_1000"
)

names(land2020_coarse) <- c( 
  "prop_forest_1000", "prop_veg_1000",
  "papaved_1000", "paunpaved_1000", "palfother_1000", "paotherpoly_1000", "paplacer_1000",
  "paquartz_1000", "paothermines_1000",
  "paharv_1000", "propfire60", "propfire2", "denspaved_1000", "densunpaved_1000", "denslfother_1000", "dens_lf_1000", "dens_poly_1000"
)

# Apply transformations (log and log+1 as needed)
gc()
land2020$log_distpaved        <- log((land2020$distpaved + 1) / 1000)
land2020$log_distunpaved      <- log((land2020$distunpaved + 1) / 1000)
land2020$log_distlfother      <- log((land2020$distlfother + 1) / 1000)
land2020$log_distotherpoly    <- log((land2020$distotherpoly + 1) / 1000)
land2020$log_distplacer       <- log((land2020$distplacer + 1) / 1000)
land2020$log_distquartz       <- log((land2020$distquartz + 1) / 1000)
land2020$log_distothermines   <- log((land2020$distothermines + 1) / 1000)

gc()
land2020$log_distpaved_1000       <- log((land2020$distpaved_1000 + 1) / 1000)
land2020$log_distunpaved_1000     <- log((land2020$distunpaved_1000 + 1) / 1000)
land2020$log_distlfother_1000     <- log((land2020$distlfother_1000 + 1) / 1000)
land2020$log_distotherpoly_1000   <- log((land2020$distotherpoly_1000 + 1) / 1000)
land2020$log_distplacer_1000      <- log((land2020$distplacer_1000 + 1) / 1000)
land2020$log_distquartz_1000      <- log((land2020$distquartz_1000 + 1) / 1000)
land2020$log_distothermines_1000  <- log((land2020$distothermines_1000 + 1) / 1000)

gc()
land2020$log_tsharv         <- log(land2020$harv_ts + 1)
land2020$log_tsfire         <- log(land2020$fire_ts + 1)
land2020$log_tsharv_1000    <- log(land2020$tsharv_1000 + 1)
land2020$log_tsfire_1000    <- log(land2020$tsfire_1000 + 1)

gc()
land2020$log_dist_lf  <- log((land2020$dist_lf + 1) / 1000)
land2020$log_dist_poly  <- log((land2020$dist_poly + 1) / 1000)
land2020$log_dist_lf_1000  <- log((land2020$dist_lf_1000 + 1) / 1000)
land2020$log_dist_poly_1000  <- log((land2020$dist_poly_1000 + 1) / 1000)
land2020$log_dist_road  <- log((land2020$dist_road + 1) / 1000)
land2020$log_dist_road_1000  <- log((land2020$dist_road_1000 + 1) / 1000)

# Reproject final stack
gc()
crs_string <- st_crs(3154)$wkt
land2020 <- terra::project(land2020, crs_string)

land2020_coarse <- terra::project(land2020_coarse, crs_string)



#Full mod
gc()
testmyearsfinefull.ud <- mod2UD(modpath = finemod.train_full, envlayers = land2020, 
                        studyArea = vect(sArea.finemod))
plot(testmyearsfinefull.ud)

gc()
testmyearsfinefull.prop <- make_prop_use(finemod.test_full, crs, testmyearsfinefull.ud)

gc()
used.bin.myearsfinefull <- make_map_bins(testmyearsfinefull.prop)

colnames(finemod.test_full) <- make.unique(colnames(finemod.test_full))
gc()
propptsbin.myearsfinefull <- pts_in_bins(finemod.test_full, testmyearsfinefull.ud)
plot(propptsbin.myearsfinefull$use.bin, propptsbin.myearsfinefull$prop.pts)
cor(propptsbin.myearsfinefull$use.bin, propptsbin.myearsfinefull$prop.pts, method = 'spearman')
#0.3907727

layerCor(c(testmyearsfinefull.ud, used.bin.myearsfinefull), fun = 'pearson', na.rm = T)

propptsbin.myearsfinefull.tot <- pts_in_bins_tot(finemod.test_full, testmyearsfinefull.ud)

plot(propptsbin.myearsfinefull.tot$use.bin, propptsbin.myearsfinefull.tot$prop.pts)
cor(propptsbin.myearsfinefull.tot$use.bin, propptsbin.myearsfinefull.tot$prop.pts, method = 'spearman')
#0.7212121


#Simple model
#Updated PDE functions to match
make_pde <- function(mod.tab, land, saveName = NULL) {
  # Paved roads
  lf.main <- mod.tab[term == 'I(log((dist_lf_250_end + 1)/1000))' & !grepl(":", term), estimate]
  lf.int <- mod.tab[grepl('dist_paved_lf_end', term) & grepl(":", term), estimate]
  lf.cov <- 2 * paved.main * land$log_dist_lf
  lf.cov.int <- 2 * paved.int * land$log_dist_lf * land$log_dist_lf_1000
  
  # Unpaved roads
  poly.main <- mod.tab[term == 'I(log((dist_poly_250_end + 1)/1000))' & !grepl(":", term), estimate]
  poly.int <- mod.tab[grepl('dist_poly_250_end', term) & grepl(":", term), estimate]
  poly.cov <- 2 * poly.main * land$log_dist_poly
  poly.cov.int <- 2 * poly.int * land$log_dist_poly * land$log_dist_poly_1000
  
  # Prop forest
  prop_forest.main <- mod.tab[term == 'prop_forest_250_end' & !grepl(":", term), estimate]
  prop_forest.int <- mod.tab[grepl('prop_forest_250_end', term) & grepl(":", term), estimate]
  prop_forest.cov <- 2 * prop_forest.main * land$prop_forest
  prop_forest.cov.int <- 2 * prop_forest.int * land$prop_forest * land$prop_forest_1000
  
  # Prop veg
  prop_veg.main <- mod.tab[term == 'prop_veg_250_end' & !grepl(":", term), estimate]
  prop_veg.int <- mod.tab[grepl('prop_veg_250_end', term) & grepl(":", term), estimate]
  prop_veg.cov <- 2 * prop_veg.main * land$prop_veg
  prop_veg.cov.int <- 2 * prop_veg.int * land$prop_veg * land$prop_veg_1000
  
  # Harvest
  harv.main <- mod.tab[term == 'I(log(ts_harv250_end + 1))' & !grepl(":", term), estimate]
  harv.int <- mod.tab[grepl('ts_harv250_end', term) & grepl(":", term), estimate]
  harv.cov <- 2 * harv.main * land$log_tsharv
  harv.cov.int <- 2 * harv.int * land$log_tsharv * land$log_tsharv_1000
  
  # Fire
  fire.main <- mod.tab[term == 'I(log(ts_fires_250_end + 1))' & !grepl(":", term), estimate]
  fire.int <- mod.tab[grepl('ts_fires_250_end', term) & grepl(":", term), estimate]
  fire.cov <- 2 * fire.main * land$log_tsfire
  fire.cov.int <- 2 * fire.int * land$log_tsfire * land$log_tsfire_1000
  
  # Combine all terms in numerator
  numerator <- exp(
    lf.cov + lf.cov.int + poly.cov + poly.cov.int +
      prop_forest.cov + prop_forest.cov.int + prop_veg.cov + prop_veg.cov.int +
      harv.cov + harv.cov.int + fire.cov + fire.cov.int
  )
  
  # Normalize
  C <- global(numerator, sum, na.rm = TRUE)
  pde <- numerator / C[[1]]
  
  # Save if requested
  if (!is.null(saveName)) {
    writeRaster(pde, file.path(derived, saveName), overwrite = TRUE)
  }
  
  return(pde)
}


gc()
testmyearsfinesimple.ud <- mod2UD(modpath = finemod.train_simple, envlayers = land2020, 
                                studyArea = vect(sArea.finemod))
plot(testmyearsfinesimple.ud)

gc()
testmyearsfinesimple.prop <- make_prop_use(finemod.test_simple, crs, testmyearsfinesimple.ud)

gc()
used.bin.myearsfinesimple <- make_map_bins(testmyearsfinesimple.prop)

colnames(finemod.test_simple) <- make.unique(colnames(finemod.test_simple))
gc()
propptsbin.myearsfinesimple <- pts_in_bins(finemod.test_simple, testmyearsfinesimple.ud)
plot(propptsbin.myearsfinesimple$use.bin, propptsbin.myearsfinesimple$prop.pts)
cor(propptsbin.myearsfinesimple$use.bin, propptsbin.myearsfinesimple$prop.pts, method = 'spearman')
#0.5206863

layerCor(c(testmyearsfinesimple.ud, used.bin.myearsfinesimple), fun = 'pearson', na.rm = T)

propptsbin.myearsfinesimple.tot <- pts_in_bins_tot(finemod.test_simple, testmyearsfinesimple.ud)

plot(propptsbin.myearsfinesimple.tot$use.bin, propptsbin.myearsfinesimple.tot$prop.pts)
cor(propptsbin.myearsfinesimple.tot$use.bin, propptsbin.myearsfinesimple.tot$prop.pts, method = 'spearman')

#1?




###Full model roads combined ----

#Updated PDE functions to match
make_pde <- function(mod.tab, land, saveName = NULL) {
  # Paved roads
  road.main <- mod.tab[term == 'I(log((dist_road_250_end + 1)/1000))' & !grepl(":", term), estimate]
  road.int <- mod.tab[grepl('dist_road_250_end', term) & grepl(":", term), estimate]
  road.cov <- 2 * road.main * land$log_dist_road
  road.cov.int <- 2 * road.int * land$log_dist_road * land$log_dist_road_1000
  
  # Linear features - other
  lfother.main <- mod.tab[term == 'I(log((dist_lfother_250_end + 1)/1000))' & !grepl(":", term), estimate]
  lfother.int <- mod.tab[grepl('dist_lfother_250_end', term) & grepl(":", term), estimate]
  lfother.cov <- 2 * lfother.main * land$log_distlfother
  lfother.cov.int <- 2 * lfother.int * land$log_distlfother * land$log_distlfother_1000
  
  # Other polygon
  otherpoly.main <- mod.tab[term == 'I(log((dist_otherpoly_250_end + 1)/1000))' & !grepl(":", term), estimate]
  otherpoly.int <- mod.tab[grepl('dist_otherpoly_250_end', term) & grepl(":", term), estimate]
  otherpoly.cov <- 2 * otherpoly.main * land$log_distotherpoly
  otherpoly.cov.int <- 2 * otherpoly.int * land$log_distotherpoly * land$log_distotherpoly_1000
  
  # Placer
  placer.main <- mod.tab[term == 'I(log((dist_placer_250_end + 1)/1000))' & !grepl(":", term), estimate]
  placer.int <- mod.tab[grepl('dist_placer_250_end', term) & grepl(":", term), estimate]
  placer.cov <- 2 * placer.main * land$log_distplacer
  placer.cov.int <- 2 * placer.int * land$log_distplacer * land$log_distplacer_1000
  
  # Quartz
  quartz.main <- mod.tab[term == 'I(log((dist_quartz_250_end + 1)/1000))' & !grepl(":", term), estimate]
  quartz.int <- mod.tab[grepl('dist_quartz_250_end', term) & grepl(":", term), estimate]
  quartz.cov <- 2 * quartz.main * land$log_distquartz
  quartz.cov.int <- 2 * quartz.int * land$log_distquartz * land$log_distquartz_1000
  
  # Other mines
  othermines.main <- mod.tab[term == 'I(log((dist_othermines_250_end + 1)/1000))' & !grepl(":", term), estimate]
  othermines.int <- mod.tab[grepl('dist_othermines_250_end', term) & grepl(":", term), estimate]
  othermines.cov <- 2 * othermines.main * land$log_distothermines
  othermines.cov.int <- 2 * othermines.int * land$log_distothermines * land$log_distothermines_1000
  
  # Prop forest
  prop_forest.main <- mod.tab[term == 'prop_forest_250_end' & !grepl(":", term), estimate]
  prop_forest.int <- mod.tab[grepl('prop_forest_250_end', term) & grepl(":", term), estimate]
  prop_forest.cov <- 2 * prop_forest.main * land$prop_forest
  prop_forest.cov.int <- 2 * prop_forest.int * land$prop_forest * land$prop_forest_1000
  
  # Prop veg
  prop_veg.main <- mod.tab[term == 'prop_veg_250_end' & !grepl(":", term), estimate]
  prop_veg.int <- mod.tab[grepl('prop_veg_250_end', term) & grepl(":", term), estimate]
  prop_veg.cov <- 2 * prop_veg.main * land$prop_veg
  prop_veg.cov.int <- 2 * prop_veg.int * land$prop_veg * land$prop_veg_1000
  
  # Harvest
  harv.main <- mod.tab[term == 'I(log(ts_harv250_end + 1))' & !grepl(":", term), estimate]
  harv.int <- mod.tab[grepl('ts_harv250_end', term) & grepl(":", term), estimate]
  harv.cov <- 2 * harv.main * land$log_tsharv
  harv.cov.int <- 2 * harv.int * land$log_tsharv * land$log_tsharv_1000
  
  # Fire
  fire.main <- mod.tab[term == 'I(log(ts_fires_250_end + 1))' & !grepl(":", term), estimate]
  fire.int <- mod.tab[grepl('ts_fires_250_end', term) & grepl(":", term), estimate]
  fire.cov <- 2 * fire.main * land$log_tsfire
  fire.cov.int <- 2 * fire.int * land$log_tsfire * land$log_tsfire_1000
  
  # Combine all terms in numerator
  numerator <- exp(
    road.cov + road.cov.int +
      lfother.cov + lfother.cov.int + otherpoly.cov + otherpoly.cov.int +
      placer.cov + placer.cov.int + quartz.cov + quartz.cov.int +
      othermines.cov + othermines.cov.int +
      prop_forest.cov + prop_forest.cov.int + prop_veg.cov + prop_veg.cov.int +
      harv.cov + harv.cov.int + fire.cov + fire.cov.int
  )
  
  # Normalize
  C <- global(numerator, sum, na.rm = TRUE)
  pde <- numerator / C[[1]]
  
  # Save if requested
  if (!is.null(saveName)) {
    writeRaster(pde, file.path(derived, saveName), overwrite = TRUE)
  }
  
  return(pde)
}


gc()
testmyearsfinefullroads.ud <- mod2UD(modpath = finemod.train_fullroadscomb, envlayers = land2020, 
                                  studyArea = vect(sArea.finemod))
plot(testmyearsfinefullroads.ud)

gc()
testmyearsfinefullroads.prop <- make_prop_use(finemod.test_fullroadscomb, crs, testmyearsfinefullroads.ud)

gc()
used.bin.myearsfinefullroads <- make_map_bins(testmyearsfinefullroads.prop)

colnames(finemod.test_fullroadscomb) <- make.unique(colnames(finemod.test_fullroadscomb))
gc()
propptsbin.myearsfinefullroads <- pts_in_bins(finemod.test_fullroadscomb, testmyearsfinefullroads.ud)
plot(propptsbin.myearsfinefullroads$use.bin, propptsbin.myearsfinefullroads$prop.pts)
cor(propptsbin.myearsfinefullroads$use.bin, propptsbin.myearsfinefullroads$prop.pts, method = 'spearman')
#0.3726375

layerCor(c(testmyearsfinefullroads.ud, used.bin.myearsfinefullroads), fun = 'pearson', na.rm = T)

propptsbin.myearsfinefullroads.tot <- pts_in_bins_tot(finemod.test_fullroadscomb, testmyearsfinefullroads.ud)

plot(propptsbin.myearsfinefullroads.tot$use.bin, propptsbin.myearsfinefullroads.tot$prop.pts)
cor(propptsbin.myearsfinefullroads.tot$use.bin, propptsbin.myearsfinefullroads.tot$prop.pts, method = 'spearman')
#0.7333333


### Coarse mod pa, fire 60 ----
make_pde <- function(mod.tab, land, saveName = NULL) {
  # Paved roads
  paved.main <- mod.tab[term == 'pa_paved_1000_end' & !grepl(":", term), estimate]
  paved.cov <- 2 * paved.main * land$papaved_1000
  
  # Unpaved roads
  unpaved.main <- mod.tab[term == 'pa_unpaved_1000_end' & !grepl(":", term), estimate]
  unpaved.cov <- 2 * unpaved.main * land$paunpaved_1000
  
  # Linear features - other
  lfother.main <- mod.tab[term == 'pa_lfother_1000_end' & !grepl(":", term), estimate]
  lfother.cov <- 2 * lfother.main * land$palfother_1000
  
  # Other polygon
  otherpoly.main <- mod.tab[term == 'pa_otherpoly_1000_end' & !grepl(":", term), estimate]
  otherpoly.cov <- 2 * otherpoly.main * land$paotherpoly_1000
  
  # Placer
  placer.main <- mod.tab[term == 'pa_placer_1000_end' & !grepl(":", term), estimate]
  placer.cov <- 2 * placer.main * land$paplacer_1000
  
  # Quartz
  quartz.main <- mod.tab[term == 'pa_quartz_1000_end' & !grepl(":", term), estimate]
  quartz.cov <- 2 * quartz.main * land$paquartz_1000
  
  # Other mines
  othermines.main <- mod.tab[term == 'pa_othermines_1000_end' & !grepl(":", term), estimate]
  othermines.cov <- 2 * othermines.main * land$paothermines_1000
  
  # Prop forest
  prop_forest.main <- mod.tab[term == 'prop_forest_1000_end' & !grepl(":", term), estimate]
  prop_forest.cov <- 2 * prop_forest.main * land$prop_forest_1000
  
  # Prop veg
  prop_veg.main <- mod.tab[term == 'prop_veg_1000_end' & !grepl(":", term), estimate]
  prop_veg.cov <- 2 * prop_veg.main * land$prop_veg_1000
  
  # Harvest
  harv.main <- mod.tab[term == 'harv_pa_1000_end' & !grepl(":", term), estimate]
  harv.cov <- 2 * harv.main * land$paharv_1000
  
  # Fire
  fire.main <- mod.tab[term == 'fire_60perc1000_end' & !grepl(":", term), estimate]
  fire.cov <- 2 * fire.main * land$propfire60
  
  # Combine all terms in numerator
  numerator <- exp(
    paved.cov  + unpaved.cov +
      lfother.cov + otherpoly.cov +
      placer.cov +  quartz.cov + 
      othermines.cov + 
      prop_forest.cov + prop_veg.cov  +
      harv.cov + fire.cov 
  )
  
  # Normalize
  C <- global(numerator, sum, na.rm = TRUE)
  pde <- numerator / C[[1]]
  
  # Save if requested
  if (!is.null(saveName)) {
    writeRaster(pde, file.path(derived, saveName), overwrite = TRUE)
  }
  
  return(pde)
}


gc()
testmyearscoarsepafire60.ud <- mod2UD(modpath = coarsemod.train.pafire60, envlayers = land2020_coarse, 
                                studyArea = vect(sArea.coarsemod))
plot(testmyearscoarsepafire60.ud)

gc()
testmyearscoarsepafire60.prop <- make_prop_use(coarsemod.test_pafire60, "EPSG:3154", testmyearscoarsepafire60.ud)

gc()
used.bin.myearscoarsepafire60 <- make_map_bins(testmyearscoarsepafire60.prop)

colnames(coarsemod.test_pafire60) <- make.unique(colnames(coarsemod.test_pafire60))
gc()
propptsbin.myearscoarsepafire60 <- pts_in_bins(coarsemod.test_pafire60, testmyearscoarsepafire60.ud)
plot(propptsbin.myearscoarsepafire60$use.bin, propptsbin.myearscoarsepafire60$prop.pts)
cor(propptsbin.myearscoarsepafire60$use.bin, propptsbin.myearscoarsepafire60$prop.pts, method = 'spearman', use = 'complete.obs')
#0.522947

layerCor(c(testmyearscoarsepafire60.ud, used.bin.myearscoarsepafire60), fun = 'pearson', na.rm = T)

propptsbin.myearscoarsepafire60.tot <- pts_in_bins_tot(coarsemod.test_pafire60, testmyearscoarsepafire60.ud)

plot(propptsbin.myearscoarsepafire60.tot$use.bin, propptsbin.myearscoarsepafire60.tot$prop.pts)
cor(propptsbin.myearscoarsepafire60.tot$use.bin, propptsbin.myearscoarsepafire60.tot$prop.pts, method = 'spearman', use = 'complete.obs')
#0.8666667


### Coarse mod pa, fire 2 ----
make_pde <- function(mod.tab, land, saveName = NULL) {
  # Paved roads
  paved.main <- mod.tab[term == 'pa_paved_1000_end' & !grepl(":", term), estimate]
  paved.cov <- 2 * paved.main * land$papaved_1000
  
  # Unpaved roads
  unpaved.main <- mod.tab[term == 'pa_unpaved_1000_end' & !grepl(":", term), estimate]
  unpaved.cov <- 2 * unpaved.main * land$paunpaved_1000
  
  # Linear features - other
  lfother.main <- mod.tab[term == 'pa_lfother_1000_end' & !grepl(":", term), estimate]
  lfother.cov <- 2 * lfother.main * land$palfother_1000
  
  # Other polygon
  otherpoly.main <- mod.tab[term == 'pa_otherpoly_1000_end' & !grepl(":", term), estimate]
  otherpoly.cov <- 2 * otherpoly.main * land$paotherpoly_1000
  
  # Placer
  placer.main <- mod.tab[term == 'pa_placer_1000_end' & !grepl(":", term), estimate]
  placer.cov <- 2 * placer.main * land$paplacer_1000
  
  # Quartz
  quartz.main <- mod.tab[term == 'pa_quartz_1000_end' & !grepl(":", term), estimate]
  quartz.cov <- 2 * quartz.main * land$paquartz_1000
  
  # Other mines
  othermines.main <- mod.tab[term == 'pa_othermines_1000_end' & !grepl(":", term), estimate]
  othermines.cov <- 2 * othermines.main * land$paothermines_1000
  
  # Prop forest
  prop_forest.main <- mod.tab[term == 'prop_forest_1000_end' & !grepl(":", term), estimate]
  prop_forest.cov <- 2 * prop_forest.main * land$prop_forest_1000
  
  # Prop veg
  prop_veg.main <- mod.tab[term == 'prop_veg_1000_end' & !grepl(":", term), estimate]
  prop_veg.cov <- 2 * prop_veg.main * land$prop_veg_1000
  
  # Harvest
  harv.main <- mod.tab[term == 'harv_pa_1000_end' & !grepl(":", term), estimate]
  harv.cov <- 2 * harv.main * land$paharv_1000
  
  # Fire
  fire.main <- mod.tab[term == 'fire_2perc1000_end' & !grepl(":", term), estimate]
  fire.cov <- 2 * fire.main * land$propfire2
  
  # Combine all terms in numerator
  numerator <- exp(
    paved.cov  + unpaved.cov +
      lfother.cov + otherpoly.cov +
      placer.cov +  quartz.cov + 
      othermines.cov + 
      prop_forest.cov + prop_veg.cov  +
      harv.cov + fire.cov 
  )
  
  # Normalize
  C <- global(numerator, sum, na.rm = TRUE)
  pde <- numerator / C[[1]]
  
  # Save if requested
  if (!is.null(saveName)) {
    writeRaster(pde, file.path(derived, saveName), overwrite = TRUE)
  }
  
  return(pde)
}


gc()
testmyearscoarsepafire2.ud <- mod2UD(modpath = coarsemod.train.pafire2, envlayers = land2020_coarse, 
                                      studyArea = vect(sArea.coarsemod))
plot(testmyearscoarsepafire2.ud)

gc()
testmyearscoarsepafire2.prop <- make_prop_use(coarsemod.test, "EPSG:3154", testmyearscoarsepafire2.ud)

gc()
used.bin.myearscoarsepafire2 <- make_map_bins(testmyearscoarsepafire2.prop)

colnames(coarsemod.test) <- make.unique(colnames(coarsemod.test))
gc()
propptsbin.myearscoarsepafire2 <- pts_in_bins(coarsemod.test, testmyearscoarsepafire2.ud)
plot(propptsbin.myearscoarsepafire2$use.bin, propptsbin.myearscoarsepafire2$prop.pts)
cor(propptsbin.myearscoarsepafire2$use.bin, propptsbin.myearscoarsepafire2$prop.pts, method = 'spearman', use = 'complete.obs')
#0.4555739

layerCor(c(testmyearscoarsepafire2.ud, used.bin.myearscoarsepafire2), fun = 'pearson', na.rm = T)

propptsbin.myearscoarsepafire2.tot <- pts_in_bins_tot(coarsemod.test, testmyearscoarsepafire2.ud)

plot(propptsbin.myearscoarsepafire2.tot$use.bin, propptsbin.myearscoarsepafire2.tot$prop.pts)
cor(propptsbin.myearscoarsepafire2.tot$use.bin, propptsbin.myearscoarsepafire2.tot$prop.pts, method = 'spearman', use = 'complete.obs')
#0.7212121


### Coarse mod lf dens, fire 60 ----
make_pde <- function(mod.tab, land, saveName = NULL) {
  # Paved roads
  paved.main <- mod.tab[term == 'dens_paved_1000_end' & !grepl(":", term), estimate]
  paved.cov <- 2 * paved.main * land$denspaved_1000
  
  # Unpaved roads
  unpaved.main <- mod.tab[term == 'dens_unpaved_1000_end' & !grepl(":", term), estimate]
  unpaved.cov <- 2 * unpaved.main * land$densunpaved_1000
  
  # Linear features - other
  lfother.main <- mod.tab[term == 'dens_lfother_1000_end' & !grepl(":", term), estimate]
  lfother.cov <- 2 * lfother.main * land$denslfother_1000
  
  # Other polygon
  otherpoly.main <- mod.tab[term == 'pa_otherpoly_1000_end' & !grepl(":", term), estimate]
  otherpoly.cov <- 2 * otherpoly.main * land$paotherpoly_1000
  
  # Placer
  placer.main <- mod.tab[term == 'pa_placer_1000_end' & !grepl(":", term), estimate]
  placer.cov <- 2 * placer.main * land$paplacer_1000
  
  # Quartz
  quartz.main <- mod.tab[term == 'pa_quartz_1000_end' & !grepl(":", term), estimate]
  quartz.cov <- 2 * quartz.main * land$paquartz_1000
  
  # Other mines
  othermines.main <- mod.tab[term == 'pa_othermines_1000_end' & !grepl(":", term), estimate]
  othermines.cov <- 2 * othermines.main * land$paothermines_1000
  
  # Prop forest
  prop_forest.main <- mod.tab[term == 'prop_forest_1000_end' & !grepl(":", term), estimate]
  prop_forest.cov <- 2 * prop_forest.main * land$prop_forest_1000
  
  # Prop veg
  prop_veg.main <- mod.tab[term == 'prop_veg_1000_end' & !grepl(":", term), estimate]
  prop_veg.cov <- 2 * prop_veg.main * land$prop_veg_1000
  
  # Harvest
  harv.main <- mod.tab[term == 'harv_pa_1000_end' & !grepl(":", term), estimate]
  harv.cov <- 2 * harv.main * land$paharv_1000
  
  # Fire
  fire.main <- mod.tab[term == 'fire_60perc1000_end' & !grepl(":", term), estimate]
  fire.cov <- 2 * fire.main * land$propfire60
  
  # Combine all terms in numerator
  numerator <- exp(
    paved.cov  + unpaved.cov +
      lfother.cov + otherpoly.cov +
      placer.cov +  quartz.cov + 
      othermines.cov + 
      prop_forest.cov + prop_veg.cov  +
      harv.cov + fire.cov 
  )
  
  # Normalize
  C <- global(numerator, sum, na.rm = TRUE)
  pde <- numerator / C[[1]]
  
  # Save if requested
  if (!is.null(saveName)) {
    writeRaster(pde, file.path(derived, saveName), overwrite = TRUE)
  }
  
  return(pde)
}


gc()
testmyearscoarsedensfire60.ud <- mod2UD(modpath = coarsemod.train.lfdensfire60, envlayers = land2020_coarse, 
                                     studyArea = vect(sArea.coarsemod))
plot(testmyearscoarsedensfire60.ud)

gc()
testmyearscoarsedensfire60.prop <- make_prop_use(coarsemod.test, "EPSG:3154", testmyearscoarsedensfire60.ud)

gc()
used.bin.myearscoarsedensfire60 <- make_map_bins(testmyearscoarsedensfire60.prop)

colnames(coarsemod.test) <- make.unique(colnames(coarsemod.test))
gc()
propptsbin.myearscoarsedensfire60 <- pts_in_bins(coarsemod.test, testmyearscoarsedensfire60.ud)
plot(propptsbin.myearscoarsedensfire60$use.bin, propptsbin.myearscoarsedensfire60$prop.pts)
cor(propptsbin.myearscoarsedensfire60$use.bin, propptsbin.myearscoarsedensfire60$prop.pts, method = 'spearman', use = 'complete.obs')
#0.5176347

layerCor(c(testmyearscoarsedensfire60.ud, used.bin.myearscoarsedensfire60), fun = 'pearson', na.rm = T)

propptsbin.myearscoarsedensfire60.tot <- pts_in_bins_tot(coarsemod.test, testmyearscoarsedensfire60.ud)

plot(propptsbin.myearscoarsedensfire60.tot$use.bin, propptsbin.myearscoarsedensfire60.tot$prop.pts)
cor(propptsbin.myearscoarsedensfire60.tot$use.bin, propptsbin.myearscoarsedensfire60.tot$prop.pts, method = 'spearman', use = 'complete.obs')
#0.9030303


### Coarse mod lf dens, fire 2 ----
make_pde <- function(mod.tab, land, saveName = NULL) {
  # Paved roads
  paved.main <- mod.tab[term == 'dens_paved_1000_end' & !grepl(":", term), estimate]
  paved.cov <- 2 * paved.main * land$denspaved_1000
  
  # Unpaved roads
  unpaved.main <- mod.tab[term == 'dens_unpaved_1000_end' & !grepl(":", term), estimate]
  unpaved.cov <- 2 * unpaved.main * land$densunpaved_1000
  
  # Linear features - other
  lfother.main <- mod.tab[term == 'dens_lfother_1000_end' & !grepl(":", term), estimate]
  lfother.cov <- 2 * lfother.main * land$denslfother_1000
  
  # Other polygon
  otherpoly.main <- mod.tab[term == 'pa_otherpoly_1000_end' & !grepl(":", term), estimate]
  otherpoly.cov <- 2 * otherpoly.main * land$paotherpoly_1000
  
  # Placer
  placer.main <- mod.tab[term == 'pa_placer_1000_end' & !grepl(":", term), estimate]
  placer.cov <- 2 * placer.main * land$paplacer_1000
  
  # Quartz
  quartz.main <- mod.tab[term == 'pa_quartz_1000_end' & !grepl(":", term), estimate]
  quartz.cov <- 2 * quartz.main * land$paquartz_1000
  
  # Other mines
  othermines.main <- mod.tab[term == 'pa_othermines_1000_end' & !grepl(":", term), estimate]
  othermines.cov <- 2 * othermines.main * land$paothermines_1000
  
  # Prop forest
  prop_forest.main <- mod.tab[term == 'prop_forest_1000_end' & !grepl(":", term), estimate]
  prop_forest.cov <- 2 * prop_forest.main * land$prop_forest_1000
  
  # Prop veg
  prop_veg.main <- mod.tab[term == 'prop_veg_1000_end' & !grepl(":", term), estimate]
  prop_veg.cov <- 2 * prop_veg.main * land$prop_veg_1000
  
  # Harvest
  harv.main <- mod.tab[term == 'harv_pa_1000_end' & !grepl(":", term), estimate]
  harv.cov <- 2 * harv.main * land$paharv_1000
  
  # Fire
  fire.main <- mod.tab[term == 'fire_2perc1000_end' & !grepl(":", term), estimate]
  fire.cov <- 2 * fire.main * land$propfire2
  
  # Combine all terms in numerator
  numerator <- exp(
    paved.cov  + unpaved.cov +
      lfother.cov + otherpoly.cov +
      placer.cov +  quartz.cov + 
      othermines.cov + 
      prop_forest.cov + prop_veg.cov  +
      harv.cov + fire.cov 
  )
  
  # Normalize
  C <- global(numerator, sum, na.rm = TRUE)
  pde <- numerator / C[[1]]
  
  # Save if requested
  if (!is.null(saveName)) {
    writeRaster(pde, file.path(derived, saveName), overwrite = TRUE)
  }
  
  return(pde)
}


gc()
testmyearscoarsedensfire2.ud <- mod2UD(modpath = coarsemod.train.lfdensfire2, envlayers = land2020_coarse, 
                                        studyArea = vect(sArea.coarsemod))
plot(testmyearscoarsedensfire2.ud)

gc()
testmyearscoarsedensfire2.prop <- make_prop_use(coarsemod.test, "EPSG:3154", testmyearscoarsedensfire2.ud)

gc()
used.bin.myearscoarsedensfire2 <- make_map_bins(testmyearscoarsedensfire2.prop)

colnames(coarsemod.test) <- make.unique(colnames(coarsemod.test))
gc()
propptsbin.myearscoarsedensfire2 <- pts_in_bins(coarsemod.test, testmyearscoarsedensfire2.ud)
plot(propptsbin.myearscoarsedensfire2$use.bin, propptsbin.myearscoarsedensfire2$prop.pts)
cor(propptsbin.myearscoarsedensfire2$use.bin, propptsbin.myearscoarsedensfire2$prop.pts, method = 'spearman', use = 'complete.obs')
#0.4464689

layerCor(c(testmyearscoarsedensfire2.ud, used.bin.myearscoarsedensfire2), fun = 'pearson', na.rm = T)

propptsbin.myearscoarsedensfire2.tot <- pts_in_bins_tot(coarsemod.test, testmyearscoarsedensfire2.ud)

plot(propptsbin.myearscoarsedensfire2.tot$use.bin, propptsbin.myearscoarsedensfire2.tot$prop.pts)
cor(propptsbin.myearscoarsedensfire2.tot$use.bin, propptsbin.myearscoarsedensfire2.tot$prop.pts, method = 'spearman', use = 'complete.obs')
#0.6606061



### Coarse mod simple, fire 60 ----
make_pde <- function(mod.tab, land, saveName = NULL) {
  # Lf
  lf.main <- mod.tab[term == 'dens_lf_1000_end' & !grepl(":", term), estimate]
  lf.cov <- 2 * lf.main * land$dens_lf_1000
  
  # Poly
  poly.main <- mod.tab[term == 'dens_poly_1000_end' & !grepl(":", term), estimate]
  poly.cov <- 2 * poly.main * land$dens_poly_1000
  
  # Prop forest
  prop_forest.main <- mod.tab[term == 'prop_forest_1000_end' & !grepl(":", term), estimate]
  prop_forest.cov <- 2 * prop_forest.main * land$prop_forest_1000
  
  # Prop veg
  prop_veg.main <- mod.tab[term == 'prop_veg_1000_end' & !grepl(":", term), estimate]
  prop_veg.cov <- 2 * prop_veg.main * land$prop_veg_1000
  
  # Fire
  fire.main <- mod.tab[term == 'fire_60perc1000_end' & !grepl(":", term), estimate]
  fire.cov <- 2 * fire.main * land$propfire60
  
  # Combine all terms in numerator
  numerator <- exp(
    lf.cov  + poly.cov +
      prop_forest.cov + prop_veg.cov  + fire.cov 
  )
  
  # Normalize
  C <- global(numerator, sum, na.rm = TRUE)
  pde <- numerator / C[[1]]
  
  # Save if requested
  if (!is.null(saveName)) {
    writeRaster(pde, file.path(derived, saveName), overwrite = TRUE)
  }
  
  return(pde)
}


gc()
testmyearscoarsesimpfire60.ud <- mod2UD(modpath = coarsemod.train.simplefire60, envlayers = land2020_coarse, 
                                       studyArea = vect(sArea.coarsemod))
plot(testmyearscoarsedensfire2.ud)

gc()
testmyearscoarsedensfire2.prop <- make_prop_use(coarsemod.test, "EPSG:3154", testmyearscoarsedensfire2.ud)

gc()
used.bin.myearscoarsedensfire2 <- make_map_bins(testmyearscoarsedensfire2.prop)

colnames(coarsemod.test) <- make.unique(colnames(coarsemod.test))
gc()
propptsbin.myearscoarsedensfire2 <- pts_in_bins(coarsemod.test, testmyearscoarsedensfire2.ud)
plot(propptsbin.myearscoarsedensfire2$use.bin, propptsbin.myearscoarsedensfire2$prop.pts)
cor(propptsbin.myearscoarsedensfire2$use.bin, propptsbin.myearscoarsedensfire2$prop.pts, method = 'spearman', use = 'complete.obs')
#0.4464689

layerCor(c(testmyearscoarsedensfire2.ud, used.bin.myearscoarsedensfire2), fun = 'pearson', na.rm = T)

propptsbin.myearscoarsedensfire2.tot <- pts_in_bins_tot(coarsemod.test, testmyearscoarsedensfire2.ud)

plot(propptsbin.myearscoarsedensfire2.tot$use.bin, propptsbin.myearscoarsedensfire2.tot$prop.pts)
cor(propptsbin.myearscoarsedensfire2.tot$use.bin, propptsbin.myearscoarsedensfire2.tot$prop.pts, method = 'spearman', use = 'complete.obs')
#0.6606061


#######################################################################################

# Validation of missing herds in Yukon ----- 

## load data ----
#finedat already loaded above
dat.obs <- finedat[case_==TRUE]
issaArea <-  vect(file.path(derived, 'NMC_studyArea_extended.shp'))


# Test data ----
#Julie's 13h fixrate data of the missing herds from the Yukon
#And make study area for it 
dat.old <- readRDS(file.path(derived, 'dat_iSSA_20lf20poly_adj.RDS'))
unique(dat.old$area[dat.old$prov == "yt"])
unique(dat.issa$area[dat.old$prov == "yt"])
missing_herds <- c("Kluane", "HartRiver", "ClearCreek", "TayRiver", "WolfLake", "Aishihik")

#Make test data
dat.old.test <- dat.old[dat.old$area %in% missing_herds & dat.old$prov == "yt", ]
dat.old.test <- dat.old.test[!is.na(x2_) & !is.na(y2_)]
dat.old.test <- dat.old.test[case_==TRUE]

#Make study area
sArea.mherdsyt <- make_study_area(dat.old.test, crs, buff = buffer)

# predicted use plots ----
gc()

## Global model ----
#Updated PDE functions to match
make_pde <- function(mod.tab, land, saveName = NULL) {
  # Paved roads
  paved.main <- mod.tab[term == 'I(log((dist_paved_250_end + 1)/1000))' & !grepl(":", term), estimate]
  paved.int <- mod.tab[grepl('dist_paved_250_end', term) & grepl(":", term), estimate]
  paved.cov <- 2 * paved.main * land$log_distpaved
  paved.cov.int <- 2 * paved.int * land$log_distpaved * land$log_distpaved_1000
  
  # Unpaved roads
  unpaved.main <- mod.tab[term == 'I(log((dist_unpaved_250_end + 1)/1000))' & !grepl(":", term), estimate]
  unpaved.int <- mod.tab[grepl('dist_unpaved_250_end', term) & grepl(":", term), estimate]
  unpaved.cov <- 2 * unpaved.main * land$log_distunpaved
  unpaved.cov.int <- 2 * unpaved.int * land$log_distunpaved * land$log_distunpaved_1000
  
  # Linear features - other
  lfother.main <- mod.tab[term == 'I(log((dist_lfother_250_end + 1)/1000))' & !grepl(":", term), estimate]
  lfother.int <- mod.tab[grepl('dist_lfother_250_end', term) & grepl(":", term), estimate]
  lfother.cov <- 2 * lfother.main * land$log_distlfother
  lfother.cov.int <- 2 * lfother.int * land$log_distlfother * land$log_distlfother_1000
  
  # Other polygon
  otherpoly.main <- mod.tab[term == 'I(log((dist_otherpoly_250_end + 1)/1000))' & !grepl(":", term), estimate]
  otherpoly.int <- mod.tab[grepl('dist_otherpoly_250_end', term) & grepl(":", term), estimate]
  otherpoly.cov <- 2 * otherpoly.main * land$log_distotherpoly
  otherpoly.cov.int <- 2 * otherpoly.int * land$log_distotherpoly * land$log_distotherpoly_1000
  
  # Placer
  placer.main <- mod.tab[term == 'I(log((dist_placer_250_end + 1)/1000))' & !grepl(":", term), estimate]
  placer.int <- mod.tab[grepl('dist_placer_250_end', term) & grepl(":", term), estimate]
  placer.cov <- 2 * placer.main * land$log_distplacer
  placer.cov.int <- 2 * placer.int * land$log_distplacer * land$log_distplacer_1000
  
  # Quartz
  quartz.main <- mod.tab[term == 'I(log((dist_quartz_250_end + 1)/1000))' & !grepl(":", term), estimate]
  quartz.int <- mod.tab[grepl('dist_quartz_250_end', term) & grepl(":", term), estimate]
  quartz.cov <- 2 * quartz.main * land$log_distquartz
  quartz.cov.int <- 2 * quartz.int * land$log_distquartz * land$log_distquartz_1000
  
  # Other mines
  othermines.main <- mod.tab[term == 'I(log((dist_othermines_250_end + 1)/1000))' & !grepl(":", term), estimate]
  othermines.int <- mod.tab[grepl('dist_othermines_250_end', term) & grepl(":", term), estimate]
  othermines.cov <- 2 * othermines.main * land$log_distothermines
  othermines.cov.int <- 2 * othermines.int * land$log_distothermines * land$log_distothermines_1000
  
  # Prop forest
  prop_forest.main <- mod.tab[term == 'prop_forest_250_end' & !grepl(":", term), estimate]
  prop_forest.int <- mod.tab[grepl('prop_forest_250_end', term) & grepl(":", term), estimate]
  prop_forest.cov <- 2 * prop_forest.main * land$prop_forest
  prop_forest.cov.int <- 2 * prop_forest.int * land$prop_forest * land$prop_forest_1000
  
  # Prop veg
  prop_veg.main <- mod.tab[term == 'prop_veg_250_end' & !grepl(":", term), estimate]
  prop_veg.int <- mod.tab[grepl('prop_veg_250_end', term) & grepl(":", term), estimate]
  prop_veg.cov <- 2 * prop_veg.main * land$prop_veg
  prop_veg.cov.int <- 2 * prop_veg.int * land$prop_veg * land$prop_veg_1000
  
  # Harvest
  harv.main <- mod.tab[term == 'I(log(ts_harv250_end + 1))' & !grepl(":", term), estimate]
  harv.int <- mod.tab[grepl('ts_harv250_end', term) & grepl(":", term), estimate]
  harv.cov <- 2 * harv.main * land$log_tsharv
  harv.cov.int <- 2 * harv.int * land$log_tsharv * land$log_tsharv_1000
  
  # Fire
  fire.main <- mod.tab[term == 'I(log(ts_fires_250_end + 1))' & !grepl(":", term), estimate]
  fire.int <- mod.tab[grepl('ts_fires_250_end', term) & grepl(":", term), estimate]
  fire.cov <- 2 * fire.main * land$log_tsfire
  fire.cov.int <- 2 * fire.int * land$log_tsfire * land$log_tsfire_1000
  
  # Combine all terms in numerator
  numerator <- exp(
    paved.cov + paved.cov.int + unpaved.cov + unpaved.cov.int +
      lfother.cov + lfother.cov.int + otherpoly.cov + otherpoly.cov.int +
      placer.cov + placer.cov.int + quartz.cov + quartz.cov.int +
      othermines.cov + othermines.cov.int +
      prop_forest.cov + prop_forest.cov.int + prop_veg.cov + prop_veg.cov.int +
      harv.cov + harv.cov.int + fire.cov + fire.cov.int
  )
  
  # Normalize
  C <- global(numerator, sum, na.rm = TRUE)
  pde <- numerator / C[[1]]
  
  # Save if requested
  if (!is.null(saveName)) {
    writeRaster(pde, file.path(derived, saveName), overwrite = TRUE)
  }
  
  return(pde)
}


#' crops `make_pde()` to study area and descretizes to 10 bins
make_pde_map <- function(pde, sArea, saveName = NULL){
  pde.sa <- crop(pde, sArea, mask = T)
  plot(pde.sa)
  
  breaks <- global(pde.sa, quantile, na.rm = T, probs = seq(0,1,.1))
  v.breaks <- unname(breaks)
  t.breaks <- as.vector(t(v.breaks))
  pde.discrete <- classify(pde.sa, t.breaks, include.lowest=TRUE, brackets=TRUE)
  
  if(!is.null(saveName)){
    writeRaster(pde.discrete, 
                file.path(derived, saveName), overwrite = T)
  }
  
  return(pde.discrete)
}

### globalfine predicting missing herds from old data ----
finemod.train.mherdsyt <- file.path("m.fullmod10step.RDS")

gc()
testmherds.ud <- mod2UD(modpath = finemod.train.mherdsyt, envlayers = land2020, 
                      studyArea = vect(sArea.mherdsyt))
plot(testmherds.ud)


gc()
testmherds.prop <- make_prop_use(dat.old.test, crs, testmherds.ud)


gc()
used.bin.mherds <- make_map_bins(testmherds.prop)

colnames(dat.old.test) <- make.unique(colnames(dat.old.test))
gc()
propptsbin.mherds <- pts_in_bins(dat.old.test, testmherds.ud)
plot(propptsbin.mherds$use.bin, propptsbin.mherds$prop.pts)
cor(propptsbin.mherds$use.bin, propptsbin.mherds$prop.pts, method = 'spearman')

layerCor(c(testmherds.ud, used.bin.mherds), fun = 'pearson', na.rm = T)

propptsbin.mherds.tot <- pts_in_bins_tot(dat.old.test, testmherds.ud)

plot(propptsbin.mherds.tot$use.bin, propptsbin.mherds.tot$prop.pts)
cor(propptsbin.mherds.tot$use.bin, propptsbin.mherds.tot$prop.pts, method = 'spearman')

#0.8424242



### global2010 predicting 2015 ----
land2010 <- load_map_layers(landyr = 2015, disturbyr = 2010, ts_else = 40)

modtrain2010 <- file.path(derived, 'mods_hpc', 'mod_selmove_2010-2015_HPC_noTA.RDS')

gc()
test2010.ud <- mod2UD(modpath = modtrain2010, envlayers = land2015, 
                      studyArea = vect(sArea.2015))
plot(test2010.ud)


gc()
test2010.prop <- make_prop_use(dat.sub, crs, test2010.ud)


gc()
used.bin.2010 <- make_map_bins(test2010.prop)


gc()
propptsbin.2010 <- pts_in_bins(dat.sub, test2010.ud)
plot(propptsbin.2010$use.bin, propptsbin.2010$prop.pts)
cor(propptsbin.2010$use.bin, propptsbin.2010$prop.pts, method = 'spearman')

layerCor(c(test2010.ud, used.bin.2010), fun = 'pearson', na.rm = T)

propptsbin.2010.tot <- pts_in_bins_tot(dat.sub, test2010.ud)

plot(propptsbin.2010.tot$use.bin, propptsbin.2010.tot$prop.pts)
cor.test(propptsbin.2010.tot$use.bin, propptsbin.2010.tot$prop.pts, 
         method = 'spearman', na.rm = T)


### BC ----
modtrainbc <- file.path(derived, 'mods_train', 'mod_bc_train_selmove_2015-2020_HPC.RDS')

gc()
testbc.ud <- mod2UD(modpath = modtrainbc, envlayers = land2015, 
                    studyArea = vect(test.sArea.bc))
plot(testbc.ud)

gc()
testbc.prop <- make_prop_use(bc.test, crs, testbc.ud)

gc()
used.bin.bc <- make_map_bins(testbc.prop)

gc()
propptsbin.bc <- pts_in_bins(bc.test, testbc.ud)
plot(propptsbin.bc$use.bin, propptsbin.bc$prop.pts)
cor(propptsbin.bc$use.bin, propptsbin.bc$prop.pts, method = 'spearman')

layerCor(c(test2015.ud, used.bin.2015), fun = 'pearson', na.rm = T)

propptsbin.bc.tot <- pts_in_bins_tot(bc.test, testbc.ud)

plot(propptsbin.bc.tot$use.bin, propptsbin.bc.tot$prop.pts)
cor(propptsbin.bc.tot$use.bin, propptsbin.bc.tot$prop.pts, method = 'spearman')


### MB ----
modtrainmb <- file.path(derived, 'mods_train', 'mod_mb_train_selmove_2015-2020_HPC.RDS')

gc()
testmb.ud <- mod2UD(modpath = modtrainmb, envlayers = land2015, 
                    studyArea = vect(test.sArea.mb))
plot(testmb.ud)

gc()
testmb.prop <- make_prop_use(mb.test, crs, testmb.ud)

gc()
used.bin.mb <- make_map_bins(testmb.prop)

gc()
propptsbin.mb <- pts_in_bins(mb.test, testmb.ud)
plot(propptsbin.mb$use.bin, propptsbin.mb$prop.pts)
cor(propptsbin.mb$use.bin, propptsbin.mb$prop.pts, method = 'spearman')

layerCor(c(test2015.ud, used.bin.2015), fun = 'pearson', na.rm = T)

propptsbin.mb.tot <- pts_in_bins_tot(mb.test, testmb.ud)

plot(propptsbin.mb.tot$use.bin, propptsbin.mb.tot$prop.pts)
cor(propptsbin.mb.tot$use.bin, propptsbin.mb.tot$prop.pts, method = 'spearman')


### NWT ----
modtrainnwt <- file.path(derived, 'mods_train', 'mod_nwt_train_selmove_2015-2020_HPC.RDS')

gc()
testnwt.ud <- mod2UD(modpath = modtrainnwt, envlayers = land2015, 
                     studyArea = vect(test.sArea.nwt))
plot(testnwt.ud)

gc()
testnwt.prop <- make_prop_use(nwt.test, crs, testnwt.ud)

gc()
used.bin.nwt <- make_map_bins(testnwt.prop)

gc()
propptsbin.nwt <- pts_in_bins(nwt.test, testnwt.ud)
plot(propptsbin.nwt$use.bin, propptsbin.nwt$prop.pts)
cor(propptsbin.nwt$use.bin, propptsbin.nwt$prop.pts, method = 'spearman')

layerCor(c(test2015.ud, used.bin.2015), fun = 'pearson', na.rm = T)

propptsbin.nwt.tot <- pts_in_bins_tot(nwt.test, testnwt.ud)

plot(propptsbin.nwt.tot$use.bin, propptsbin.nwt.tot$prop.pts)
cor(propptsbin.nwt.tot$use.bin, propptsbin.nwt.tot$prop.pts, method = 'spearman')


### SK ----
modtrainsk <- file.path(derived, 'mods_train', 'mod_sk_train_selmove_2015-2020_HPC.RDS')

gc()
testsk.ud <- mod2UD(modpath = modtrainsk, envlayers = land2015, 
                    studyArea = vect(test.sArea.sk))
plot(testsk.ud)

gc()
testsk.prop <- make_prop_use(sk.test, crs, testsk.ud)

gc()
used.bin.sk <- make_map_bins(testsk.prop)

gc()
propptsbin.sk <- pts_in_bins(sk.test, testsk.ud)
plot(propptsbin.sk$use.bin, propptsbin.sk$prop.pts)
cor(propptsbin.sk$use.bin, propptsbin.sk$prop.pts, method = 'spearman')

layerCor(c(test2015.ud, used.bin.2015), fun = 'pearson', na.rm = T)

propptsbin.sk.tot <- pts_in_bins_tot(sk.test, testsk.ud)

plot(propptsbin.sk.tot$use.bin, propptsbin.sk.tot$prop.pts)
cor(propptsbin.sk.tot$use.bin, propptsbin.sk.tot$prop.pts, method = 'spearman')






### global consolidated ----
global.proppts <- rbind(propptsbin.2010.tot[,fold:= '2010-2015'], propptsbin.2015.tot[,fold:= '2015_internal'], 
                        propptsbin.bc.tot[,fold:= 'bc'], propptsbin.mb.tot[,fold:= 'mb'],
                        propptsbin.nwt.tot[,fold:= 'nwt'], propptsbin.sk.tot[,fold:= 'sk'])
saveRDS(global.proppts, file.path(derived, 'validations', 'global_ud_overlap.RDS'))

ggplot(global.proppts, aes(use.bin, prop.pts, color = fold)) +
  geom_point() +
  geom_line() + 
  theme_bw() +
  xlab('Use bin') + ylab('Area adjusted proportion of used points') +
  scale_x_continuous(breaks = seq(0,9, by = 1)) +
  scale_color_viridis_d()

cor.test(global.proppts$use.bin, global.proppts$prop.pts, 
         method = 'spearman', na.rm = T)

global.cor <- global.proppts[,.(rho = cor.test(use.bin, prop.pts, method = 'spearman', na.rm = T)$estimate[[1]]),
                             by = .(fold)]
global.cor.sum <- global.cor[,.(mean.rho = mean(rho), sd = sd(rho))]

## Jurisdictional models ----

dat.sub.juris <- dat[year>=2014 & year<=2019]

# models
modbc <- file.path(derived, 'mod_selmove_bc.RDS')
modmb <- file.path(derived, 'mod_selmove_mb_2015_50.RDS')
modnwt <- file.path(derived, 'mod_selmove_nwt.RDS')
modsk <- file.path(derived, 'mod_selmove_sk.RDS')

### nwt ----
nwt <- dat.sub.juris[jurisdiction %in% c('nwt', 'yt')]
nwt[,id:=as.factor(id)]
nwt[,indiv_step_id := as.factor(indiv_step_id)]
nwt[,jurisdiction := as.factor(jurisdiction)]
nwt[,year:=as.factor(year)]
nwt[,pop := as.factor(pop)]

gc()
test.sArea.nwt.juris <- make_study_area(nwt, crs, buff = buffer)

# predict NWT based on other juris mods
#### BC mod ----
gc()
nwt.modbc.ud <- mod2UD(modpath = modbc, envlayers = land2015, 
                       studyArea = vect(test.sArea.nwt.juris))
plot(nwt.modbc.ud)


gc()
nwt.modbc.prop <- make_prop_use(nwt, crs, nwt.modbc.ud)


gc()
used.bin.nwt.modbc <- make_map_bins(nwt.modbc.prop)


gc()
propptsbin.nwt.modbc <- pts_in_bins(nwt, nwt.modbc.ud)
plot(propptsbin.nwt.modbc$use.bin, propptsbin.nwt.modbc$prop.pts)
cor.test(propptsbin.nwt.modbc$use.bin, propptsbin.nwt.modbc$prop.pts, method = 'spearman', na.rm =T)

layerCor(c(nwt.modbc.ud, used.bin.nwt.modbc), fun = 'pearson', na.rm = T)

propptsbin.nwt.modbc.tot <- pts_in_bins_tot(nwt, nwt.modbc.ud)

plot(propptsbin.nwt.modbc.tot$use.bin, propptsbin.nwt.modbc.tot$prop.pts)
cor.test(propptsbin.nwt.modbc.tot$use.bin, propptsbin.nwt.modbc.tot$prop.pts, 
         method = 'spearman', na.rm = T)


#### MB mod ----
gc()
nwt.modmb.ud <- mod2UD(modpath = modmb, envlayers = land2015, 
                       studyArea = vect(test.sArea.nwt.juris))
plot(nwt.modmb.ud)


gc()
nwt.modmb.prop <- make_prop_use(nwt, crs, nwt.modmb.ud)


gc()
used.bin.nwt.modmb <- make_map_bins(nwt.modmb.prop)


gc()
propptsbin.nwt.modmb <- pts_in_bins(nwt, nwt.modmb.ud)
plot(propptsbin.nwt.modmb$use.bin, propptsbin.nwt.modmb$prop.pts)
cor.test(propptsbin.nwt.modmb$use.bin, propptsbin.nwt.modmb$prop.pts, method = 'spearman', na.rm =T)

layerCor(c(nwt.modmb.ud, used.bin.nwt.modmb), fun = 'pearson', na.rm = T)

propptsbin.nwt.modmb.tot <- pts_in_bins_tot(nwt, nwt.modmb.ud)

plot(propptsbin.nwt.modmb.tot$use.bin, propptsbin.nwt.modmb.tot$prop.pts)
cor.test(propptsbin.nwt.modmb.tot$use.bin, propptsbin.nwt.modmb.tot$prop.pts, 
         method = 'spearman', na.rm = T)


#### SK mod ----
gc()
nwt.modsk.ud <- mod2UD(modpath = modsk, envlayers = land2015, 
                       studyArea = vect(test.sArea.nwt.juris))
plot(nwt.modsk.ud)


gc()
nwt.modsk.prop <- make_prop_use(nwt, crs, nwt.modsk.ud)


gc()
used.bin.nwt.modsk <- make_map_bins(nwt.modsk.prop)


gc()
propptsbin.nwt.modsk <- pts_in_bins(nwt, nwt.modsk.ud)
plot(propptsbin.nwt.modsk$use.bin, propptsbin.nwt.modsk$prop.pts)
cor.test(propptsbin.nwt.modsk$use.bin, propptsbin.nwt.modsk$prop.pts, method = 'spearman', na.rm =T)

layerCor(c(nwt.modsk.ud, used.bin.nwt.modsk), fun = 'pearson', na.rm = T)

propptsbin.nwt.modsk.tot <- pts_in_bins_tot(nwt, nwt.modsk.ud)

plot(propptsbin.nwt.modsk.tot$use.bin, propptsbin.nwt.modsk.tot$prop.pts)
cor.test(propptsbin.nwt.modsk.tot$use.bin, propptsbin.nwt.modsk.tot$prop.pts, 
         method = 'spearman', na.rm = T)



### mb ----
mb.2015 <- dat[jurisdiction == 'mb' & int.year ==2015]
length(unique(mb.2015$id))*.5
# worked when sampled 150 indivs
mb.sub.id <- sample(unique(mb.2015$id), floor(length(unique(mb.2015$id))*.50))
mb <- mb.2015[id %in% mb.sub.id]
mb[,id:=as.factor(id)]
mb[,indiv_step_id := as.factor(indiv_step_id)]
mb[,jurisdiction := as.factor(jurisdiction)]
mb[,year:=as.factor(year)]
mb[,pop := as.factor(pop)]


gc()
test.sArea.mb.juris <- make_study_area(mb.2015.sub, crs, buff = buffer)

# predict MB based on other juris mods
#### BC mod ----
gc()
mb.modbc.ud <- mod2UD(modpath = modbc, envlayers = land2015, 
                      studyArea = vect(test.sArea.mb.juris))
plot(mb.modbc.ud)


gc()
mb.modbc.prop <- make_prop_use(mb, crs, mb.modbc.ud)


gc()
used.bin.mb.modbc <- make_map_bins(mb.modbc.prop)


gc()
propptsbin.mb.modbc <- pts_in_bins(mb, mb.modbc.ud)
plot(propptsbin.mb.modbc$use.bin, propptsbin.mb.modbc$prop.pts)
cor.test(propptsbin.mb.modbc$use.bin, propptsbin.mb.modbc$prop.pts, method = 'spearman', na.rm =T)

layerCor(c(mb.modbc.ud, used.bin.mb.modbc), fun = 'pearson', na.rm = T)

propptsbin.mb.modbc.tot <- pts_in_bins_tot(mb, mb.modbc.ud)

plot(propptsbin.mb.modbc.tot$use.bin, propptsbin.mb.modbc.tot$prop.pts)
cor.test(propptsbin.mb.modbc.tot$use.bin, propptsbin.mb.modbc.tot$prop.pts, 
         method = 'spearman', na.rm = T)


#### NWT mod ----
gc()
mb.modnwt.ud <- mod2UD(modpath = modnwt, envlayers = land2015, 
                       studyArea = vect(test.sArea.mb.juris))
plot(mb.modnwt.ud)


gc()
mb.modnwt.prop <- make_prop_use(mb, crs, mb.modnwt.ud)


gc()
used.bin.mb.modnwt <- make_map_bins(mb.modnwt.prop)


gc()
propptsbin.mb.modnwt <- pts_in_bins(mb, mb.modnwt.ud)
plot(propptsbin.mb.modnwt$use.bin, propptsbin.mb.modnwt$prop.pts)
cor.test(propptsbin.mb.modnwt$use.bin, propptsbin.mb.modnwt$prop.pts, method = 'spearman', na.rm =T)

layerCor(c(mb.modnwt.ud, used.bin.mb.modnwt), fun = 'pearson', na.rm = T)

propptsbin.mb.modnwt.tot <- pts_in_bins_tot(mb, mb.modnwt.ud)

plot(propptsbin.mb.modnwt.tot$use.bin, propptsbin.mb.modnwt.tot$prop.pts)
cor.test(propptsbin.mb.modnwt.tot$use.bin, propptsbin.mb.modnwt.tot$prop.pts, 
         method = 'spearman', na.rm = T)


#### SK mod ----
gc()
mb.modsk.ud <- mod2UD(modpath = modsk, envlayers = land2015, 
                      studyArea = vect(test.sArea.mb.juris))
plot(mb.modsk.ud)


gc()
mb.modsk.prop <- make_prop_use(mb, crs, mb.modsk.ud)


gc()
used.bin.mb.modsk <- make_map_bins(mb.modsk.prop)


gc()
propptsbin.mb.modsk <- pts_in_bins(mb, mb.modsk.ud)
plot(propptsbin.mb.modsk$use.bin, propptsbin.mb.modsk$prop.pts)
cor.test(propptsbin.mb.modsk$use.bin, propptsbin.mb.modsk$prop.pts, method = 'spearman', na.rm =T)

layerCor(c(mb.modsk.ud, used.bin.mb.modsk), fun = 'pearson', na.rm = T)

propptsbin.mb.modsk.tot <- pts_in_bins_tot(mb, mb.modsk.ud)

plot(propptsbin.mb.modsk.tot$use.bin, propptsbin.mb.modsk.tot$prop.pts)
cor.test(propptsbin.mb.modsk.tot$use.bin, propptsbin.mb.modsk.tot$prop.pts, 
         method = 'spearman', na.rm = T)



### sk ----
sk <- dat.sub[jurisdiction == 'sk']
sk[,id:=as.factor(id)]
sk[,indiv_step_id := as.factor(indiv_step_id)]
sk[,jurisdiction := as.factor(jurisdiction)]
sk[,year:=as.factor(year)]
sk[,pop := as.factor(pop)]

gc()
test.sArea.sk.juris <- make_study_area(sk, crs, buff = buffer)

# predict SK based on other juris mods
#### BC mod ----
gc()
sk.modbc.ud <- mod2UD(modpath = modbc, envlayers = land2015, 
                      studyArea = vect(test.sArea.sk.juris))
plot(sk.modbc.ud)


gc()
sk.modbc.prop <- make_prop_use(sk, crs, sk.modbc.ud)


gc()
used.bin.sk.modbc <- make_map_bins(sk.modbc.prop)


gc()
propptsbin.sk.modbc <- pts_in_bins(sk, sk.modbc.ud)
plot(propptsbin.sk.modbc$use.bin, propptsbin.sk.modbc$prop.pts)
cor.test(propptsbin.sk.modbc$use.bin, propptsbin.sk.modbc$prop.pts, method = 'spearman', na.rm =T)

layerCor(c(sk.modbc.ud, used.bin.sk.modbc), fun = 'pearson', na.rm = T)

propptsbin.sk.modbc.tot <- pts_in_bins_tot(sk, sk.modbc.ud)

plot(propptsbin.sk.modbc.tot$use.bin, propptsbin.sk.modbc.tot$prop.pts)
cor.test(propptsbin.sk.modbc.tot$use.bin, propptsbin.sk.modbc.tot$prop.pts, 
         method = 'spearman', na.rm = T)


#### MB mod ----
gc()
sk.modmb.ud <- mod2UD(modpath = modmb, envlayers = land2015, 
                      studyArea = vect(test.sArea.sk.juris))
plot(sk.modmb.ud)


gc()
sk.modmb.prop <- make_prop_use(sk, crs, sk.modmb.ud)


gc()
used.bin.sk.modmb <- make_map_bins(sk.modmb.prop)


gc()
propptsbin.sk.modmb <- pts_in_bins(sk, sk.modmb.ud)
plot(propptsbin.sk.modmb$use.bin, propptsbin.sk.modmb$prop.pts)
cor.test(propptsbin.sk.modmb$use.bin, propptsbin.sk.modmb$prop.pts, method = 'spearman', na.rm =T)

layerCor(c(sk.modmb.ud, used.bin.sk.modmb), fun = 'pearson', na.rm = T)

propptsbin.sk.modmb.tot <- pts_in_bins_tot(sk, sk.modmb.ud)

plot(propptsbin.sk.modmb.tot$use.bin, propptsbin.sk.modmb.tot$prop.pts)
cor.test(propptsbin.sk.modmb.tot$use.bin, propptsbin.sk.modmb.tot$prop.pts, 
         method = 'spearman', na.rm = T)


#### NWT mod ----
gc()
sk.modnwt.ud <- mod2UD(modpath = modnwt, envlayers = land2015, 
                       studyArea = vect(test.sArea.sk.juris))
plot(sk.modnwt.ud)


gc()
sk.modnwt.prop <- make_prop_use(sk, crs, sk.modnwt.ud)


gc()
used.bin.sk.modnwt <- make_map_bins(sk.modnwt.prop)


gc()
propptsbin.sk.modnwt <- pts_in_bins(sk, sk.modnwt.ud)
plot(propptsbin.sk.modnwt$use.bin, propptsbin.sk.modnwt$prop.pts)
cor.test(propptsbin.sk.modnwt$use.bin, propptsbin.sk.modnwt$prop.pts, method = 'spearman', na.rm =T)

layerCor(c(sk.modnwt.ud, used.bin.sk.modnwt), fun = 'pearson', na.rm = T)

propptsbin.sk.modnwt.tot <- pts_in_bins_tot(sk, sk.modnwt.ud)

plot(propptsbin.sk.modnwt.tot$use.bin, propptsbin.sk.modnwt.tot$prop.pts)
cor.test(propptsbin.sk.modnwt.tot$use.bin, propptsbin.sk.modnwt.tot$prop.pts, 
         method = 'spearman', na.rm = T)


### bc ----
bc <- dat.sub[jurisdiction == 'bc']
bc[,id:=as.factor(id)]
bc[,indiv_step_id := as.factor(indiv_step_id)]
bc[,jurisdiction := as.factor(jurisdiction)]
bc[,year:=as.factor(year)]
bc[,pop := as.factor(pop)]

gc()
test.sArea.bc.juris <- make_study_area(bc, crs, buff = buffer)

#### MB mod ----
gc()
bc.modmb.ud <- mod2UD(modpath = modmb, envlayers = land2015, 
                      studyArea = vect(test.sArea.bc.juris))
plot(bc.modmb.ud)


gc()
bc.modmb.prop <- make_prop_use(bc, crs, bc.modmb.ud)


gc()
used.bin.bc.modmb <- make_map_bins(bc.modmb.prop)


gc()
propptsbin.bc.modmb <- pts_in_bins(bc, bc.modmb.ud)
plot(propptsbin.bc.modmb$use.bin, propptsbin.bc.modmb$prop.pts)
cor.test(propptsbin.bc.modmb$use.bin, propptsbin.bc.modmb$prop.pts, method = 'spearman', na.rm =T)

layerCor(c(bc.modmb.ud, used.bin.bc.modmb), fun = 'pearson', na.rm = T)

propptsbin.bc.modmb.tot <- pts_in_bins_tot(bc, bc.modmb.ud)

plot(propptsbin.bc.modmb.tot$use.bin, propptsbin.bc.modmb.tot$prop.pts)
cor.test(propptsbin.bc.modmb.tot$use.bin, propptsbin.bc.modmb.tot$prop.pts, 
         method = 'spearman', na.rm = T)


#### NWT mod ----
gc()
bc.modnwt.ud <- mod2UD(modpath = modnwt, envlayers = land2015, 
                       studyArea = vect(test.sArea.bc.juris))
plot(bc.modnwt.ud)


gc()
bc.modnwt.prop <- make_prop_use(bc, crs, bc.modnwt.ud)


gc()
used.bin.bc.modnwt <- make_map_bins(bc.modnwt.prop)


gc()
propptsbin.bc.modnwt <- pts_in_bins(bc, bc.modnwt.ud)
plot(propptsbin.bc.modnwt$use.bin, propptsbin.bc.modnwt$prop.pts)
cor.test(propptsbin.bc.modnwt$use.bin, propptsbin.bc.modnwt$prop.pts, method = 'spearman', na.rm =T)

layerCor(c(bc.modnwt.ud, used.bin.bc.modnwt), fun = 'pearson', na.rm = T)

propptsbin.bc.modnwt.tot <- pts_in_bins_tot(bc, bc.modnwt.ud)

plot(propptsbin.bc.modnwt.tot$use.bin, propptsbin.bc.modnwt.tot$prop.pts)
cor.test(propptsbin.bc.modnwt.tot$use.bin, propptsbin.bc.modnwt.tot$prop.pts, 
         method = 'spearman', na.rm = T)


#### SK mod ----
gc()
bc.modsk.ud <- mod2UD(modpath = modsk, envlayers = land2015, 
                      studyArea = vect(test.sArea.bc.juris))
plot(bc.modsk.ud)


gc()
bc.modsk.prop <- make_prop_use(bc, crs, bc.modsk.ud)


gc()
used.bin.bc.modsk <- make_map_bins(bc.modsk.prop)


gc()
propptsbin.bc.modsk <- pts_in_bins(bc, bc.modsk.ud)
plot(propptsbin.bc.modsk$use.bin, propptsbin.bc.modsk$prop.pts)
cor.test(propptsbin.bc.modsk$use.bin, propptsbin.bc.modsk$prop.pts, method = 'spearman', na.rm =T)

layerCor(c(bc.modsk.ud, used.bin.bc.modsk), fun = 'pearson', na.rm = T)

propptsbin.bc.modsk.tot <- pts_in_bins_tot(bc, bc.modsk.ud)

plot(propptsbin.bc.modsk.tot$use.bin, propptsbin.bc.modsk.tot$prop.pts)
cor.test(propptsbin.bc.modsk.tot$use.bin, propptsbin.bc.modsk.tot$prop.pts, 
         method = 'spearman', na.rm = T)


## Juris consolidated ----
juris.proppts <- rbind(propptsbin.mb.modbc.tot[,`:=`(dat = 'mb', model = 'bc')], 
                       propptsbin.mb.modnwt.tot[,`:=`(dat = 'mb', model = 'nwt')],
                       propptsbin.mb.modsk.tot[,`:=`(dat = 'mb', model = 'sk')],
                       propptsbin.nwt.modbc.tot[,`:=`(dat = 'nwt', model = 'bc')], 
                       propptsbin.nwt.modmb.tot[,`:=`(dat = 'nwt', model = 'mb')],
                       propptsbin.nwt.modsk.tot[,`:=`(dat = 'nwt', model = 'sk')],
                       propptsbin.sk.modbc.tot[,`:=`(dat = 'sk', model = 'bc')], 
                       propptsbin.sk.modnwt.tot[,`:=`(dat = 'sk', model = 'nwt')],
                       propptsbin.sk.modmb.tot[,`:=`(dat = 'sk', model = 'mb')],
                       propptsbin.bc.modmb.tot[,`:=`(dat = 'bc', model = 'mb')], 
                       propptsbin.bc.modnwt.tot[,`:=`(dat = 'bc', model = 'nwt')],
                       propptsbin.bc.modsk.tot[,`:=`(dat = 'bc', model = 'sk')])

saveRDS(juris.proppts, file.path(derived, 'validations', 'juris_ud_overlap.RDS'))

juris.proppts <- readRDS(file.path(derived, 'validations', 'juris_ud_overlap.RDS'))
juris.proppts[,mod.labs:= paste0(toupper(model), ' model')]
ggplot(juris.proppts, aes(use.bin, prop.pts, color = toupper(dat))) +
  geom_point() +
  geom_line() + 
  theme_bw() +
  xlab('Use bin') + ylab('Area adjusted proportion of used points') +
  scale_x_continuous(breaks = seq(0,9, by = 1)) +
  scale_color_viridis_d(name='fold') +
  facet_wrap(~mod.labs) 

cor.test(juris.proppts$use.bin, juris.proppts$prop.pts, 
         method = 'spearman', na.rm = T)

juris.cor <- juris.proppts[,.(rho = cor.test(use.bin, prop.pts, method = 'spearman', na.rm = T)$estimate[[1]]),
                           by = .(model, dat)]
juris.cor.sum <- juris.cor[,.(mean.rho = mean(rho), sd = sd(rho)), by = .(model)]

######

pde.2010 <- rast(file.path(derived, 'pde2010_re.tif'))

pde.2015 <- as.numeric(rast(file.path(derived, 'pde2015_re.tif')))


crs <- st_crs(pde.2015)$wkt

temp <- pde.2015
temp[!is.na(temp)]<-0



# used.2015 <- rast.2015
# used.2015[is.na(used.2015)] <- temp

breaks <- global(rast.2015, quantile, na.rm = T, probs = seq(0,1,.1))
v.breaks <- unname(breaks)
t.breaks <- as.vector(t(v.breaks))
used.bin.2015 <- as.numeric(classify(rast.2015, t.breaks, include.lowest=TRUE, brackets=TRUE))
freq(used.bin.2015)
used.bin.0.2015 <- used.bin.2015
used.bin.0.2015[used.bin.0.2015==0]<-1
used.bin.0.2015[is.na(used.bin.0.2015)] <- temp

ptsbin.2015 <- dat.obs[int.year==2015]
ptsbin.2015[, use.bin:= terra::extract(pde.2015, cbind(.SD))[,-1],
            .SDcols = c('x1_','y1_')]
freq(pde.2015)
npts.2015 <- ptsbin.2015[,.(npts = .N), by= .(id)]
propptsbin.2015 <- merge(ptsbin.2015[id!=''], npts.2015, by = 'id')
propptsbin.2015 <- propptsbin.2015[,prop.pts := .N/npts, by = .(use.bin, id)]
plot(propptsbin.2015$use.bin, propptsbin.2015$prop.pts)
cor(propptsbin.2015$use.bin, propptsbin.2015$prop.pts, method = 'spearman')

global((sqrt(raster01(pde.2015))*sqrt(raster01(used.2015))), fun = 'sum', na.rm = T)

layerCor(c(as.numeric(pde.2015), used.bin.0.2015), fun = 'pearson', na.rm = T)
cor.2015 <- focalPairs(c(as.numeric(pde.2015), used.bin.0.2015), fun = 'pearson')

pde.use.2015 <- mask(as.numeric(pde.2015), rast.2015)
layerCor(c(pde.use.2015, rast.2015), fun = 'pearson', na.rm = T)

























