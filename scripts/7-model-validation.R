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

rsf.dat <- readRDS(file.path(derived, 'rsf_data_upper95.RDS'))

int.yr <- 2015
set.seed(53)



#Create seasonal data
# Separate seasons into diff datasets
finedat.20$monthday <- format(as.Date(finedat.20$t1_, format="%Y/%m/%d"),"%m/%d") #Create new column that isolates the month and day out of t1_
finedat.20$monthday #Confirming that the new column was created correctly
names(finedat.20) #Confirming that the new column exists

finedat.20[monthday >= '11/01' | monthday <= '04/15', season:="winter"] #Create new season column, with Nov to mid-April being called Winter
finedat.20.winter <- finedat.20[finedat.20$season == 'winter',] #Assign to new winter dataframe

finedat.20[monthday >= '07/01' & monthday <= '09/30', season:="summer"] #Create summer season from July through September
finedat.20.summer <- finedat.20[finedat.20$season == 'summer',] #Assign to new summer dataframe

finedat.15$monthday <- format(as.Date(finedat.15$t1_, format="%Y/%m/%d"),"%m/%d") #Create new column that isolates the month and day out of t1_
finedat.15$monthday #Confirming that the new column was created correctly
names(finedat.15) #Confirming that the new column exists

finedat.15[monthday >= '11/01' | monthday <= '04/15', season:="winter"] #Create new season column, with Nov to mid-April being called Winter
finedat.15.winter <- finedat.15[finedat.15$season == 'winter',] #Assign to new winter dataframe

finedat.15[monthday >= '07/01' & monthday <= '09/30', season:="summer"] #Create summer season from July through September
finedat.15.summer <- finedat.15[finedat.15$season == 'summer',] #Assign to new summer dataframe

#30 steps
finedat$monthday <- format(as.Date(finedat$t1_, format="%Y/%m/%d"),"%m/%d") #Create new column that isolates the month and day out of t1_
finedat$monthday #Confirming that the new column was created correctly
names(finedat) #Confirming that the new column exists

finedat[monthday >= '11/01' | monthday <= '04/15', season:="winter"] #Create new season column, with Nov to mid-April being called Winter
finedat.winter <- finedat[finedat$season == 'winter',] #Assign to new winter dataframe

finedat[monthday >= '07/01' & monthday <= '09/30', season:="summer"] #Create summer season from July through September
finedat.summer <- finedat[finedat$season == 'summer',] #Assign to new summer dataframe

coarsedat.20$monthday <- format(as.Date(coarsedat.20$t1_, format="%Y/%m/%d"),"%m/%d") #Create new column that isolates the month and day out of t1_
coarsedat.20$monthday #Confirming that the new column was created correctly
names(coarsedat.20) #Confirming that the new column exists

coarsedat.20[monthday >= '11/01' | monthday <= '04/15', season:="winter"] #Create new season column, with Nov to mid-April being called Winter
coarsedat.20.winter <- coarsedat.20[coarsedat.20$season == 'winter',] #Assign to new winter dataframe

coarsedat.20[monthday >= '07/01' & monthday <= '09/30', season:="summer"] #Create summer season from July through September
coarsedat.20.summer <- coarsedat.20[coarsedat.20$season == 'summer',] #Assign to new summer dataframe

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

finemod.train.summer <- finedat.20.summer[!(year %in% c(2022, 2023, 2024, 2025))]
finemod.train.summer[,indiv_step_id := as.factor(indiv_step_id)]
finemod.train.summer[,prov := as.factor(prov)]
finemod.train.summer[,year:=as.factor(year)]
finemod.train.summer[,area := as.factor(area)]
finemod.train.summer[,id := as.factor(id)]

finemod.train.winter <- finedat.20.winter[!(year %in% c(2022, 2023, 2024, 2025))]
finemod.train.winter[,indiv_step_id := as.factor(indiv_step_id)]
finemod.train.winter[,prov := as.factor(prov)]
finemod.train.winter[,year:=as.factor(year)]
finemod.train.winter[,area := as.factor(area)]
finemod.train.winter[,id := as.factor(id)]

finemod.train.winter15 <- finedat.15.winter[!(year %in% c(2022, 2023, 2024, 2025))]
finemod.train.winter15[,indiv_step_id := as.factor(indiv_step_id)]
finemod.train.winter15[,prov := as.factor(prov)]
finemod.train.winter15[,year:=as.factor(year)]
finemod.train.winter15[,area := as.factor(area)]
finemod.train.winter15[,id := as.factor(id)]

finemod.train.winter30 <- finedat.winter[!(year %in% c(2022, 2023, 2024, 2025))]
finemod.train.winter30[,indiv_step_id := as.factor(indiv_step_id)]
finemod.train.winter30[,prov := as.factor(prov)]
finemod.train.winter30[,year:=as.factor(year)]
finemod.train.winter30[,area := as.factor(area)]
finemod.train.winter30[,id := as.factor(id)]

coarsemod.train <- coarsedat.20[!(year %in% c(2022, 2023, 2024, 2025))]
coarsemod.train[,indiv_step_id := as.factor(indiv_step_id)]
coarsemod.train[,prov := as.factor(prov)]
coarsemod.train[,year:=as.factor(year)]
coarsemod.train[,area := as.factor(area)]
coarsemod.train[,id := as.factor(id)]

coarsemod.train.summer <- coarsedat.20.summer[!(year %in% c(2022, 2023, 2024, 2025))]
coarsemod.train.summer[,indiv_step_id := as.factor(indiv_step_id)]
coarsemod.train.summer[,prov := as.factor(prov)]
coarsemod.train.summer[,year:=as.factor(year)]
coarsemod.train.summer[,area := as.factor(area)]
coarsemod.train.summer[,id := as.factor(id)]

coarsemod.train.winter <- coarsedat.20.winter[!(year %in% c(2022, 2023, 2024, 2025))]
coarsemod.train.winter[,indiv_step_id := as.factor(indiv_step_id)]
coarsemod.train.winter[,prov := as.factor(prov)]
coarsemod.train.winter[,year:=as.factor(year)]
coarsemod.train.winter[,area := as.factor(area)]
coarsemod.train.winter[,id := as.factor(id)]

rsfmod.train <- rsf.dat[!(year %in% c(2022, 2023, 2024, 2025))]
rsfmod.train[,prov := as.factor(prov)]
rsfmod.train[,year:=as.factor(year)]
rsfmod.train[,area := as.factor(area)]
rsfmod.train[,id := as.factor(id)]
rsfmod.train[, case_ := detection == 1L]
data.table::setnames(rsfmod.train, old = c("x", "y"), new = c("x1_", "y1_"))

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
  npts_bins <- DT[,.(npts_bins = .N), by= .(id)]
  mrg <- merge(DT, npts_bins, by = 'id')
  propptsbin <- mrg[,.(prop.pts = .N/npts_bins), by = .(use.bin, id)]
  return(propptsbin)
}


pts_in_bins_tot <- function(dat, bin.ud){
  DT <- dat[id!='' & case_ == TRUE]
  DT[, use.bin:= terra::extract(bin.ud, cbind(.SD))[,-1],
     .SDcols = c('x1_','y1_')]
  npts_bins <- DT[,.(npts_bins = .N)][[1]]
  propptsbin.tot <- DT[,.(prop.pts = .N/npts_bins), by = .(use.bin)]
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


#### Full Model Summer ----
gc()
finemod.train_full.summer <- glmmTMB(case_ ~ -1 +
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
                              family = poisson(), data = finemod.train.summer[,`:=`(year = as.factor(year), 
                                                                             indiv_step_id = as.factor(indiv_step_id),
                                                                             id = as.factor(id))],
                              map= list(theta = factor(c(NA,1:15))),
                              start = list(theta =c(log(1000), seq(0,0, length.out = 15))), 
                              verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(finemod.train_full.summer)
# save model output
saveRDS(finemod.train_full.summer, "finemod.train_full.summer.RDS")
finemod.train_full.summer <- file.path("finemod.train_full.summer.RDS")


#### Full Model Winter ----
#Had to use 30 steps here, 20 steps wouldn't converge
gc()
finemod.train_full.winter <- glmmTMB(case_ ~ -1 +
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
                                     family = poisson(), data = finemod.train.winter30[,`:=`(year = as.factor(year), 
                                                                                           indiv_step_id = as.factor(indiv_step_id),
                                                                                           id = as.factor(id))],
                                     map= list(theta = factor(c(NA,1:15))),
                                     start = list(theta =c(log(1000), seq(0,0, length.out = 15))), 
                                     verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(finemod.train_full.winter)
# save model output
saveRDS(finemod.train_full.winter, "finemod.train_full.winter.RDS")
finemod.train_full.winter <- file.path("finemod.train_full.winter.RDS")



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



#### Simple Model Summer ----
gc()
finemod.train_simple.summer <- glmmTMB(case_ ~ -1 +
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
                                family = poisson(), data = finemod.train.summer[,`:=`(year = as.factor(year), 
                                                                                  indiv_step_id = as.factor(indiv_step_id),
                                                                                  id = as.factor(id))],
                                map= list(theta = factor(c(NA,1:10))),
                                start = list(theta =c(log(1000), seq(0,0, length.out = 10))), 
                                verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(finemod.train_simple.summer)
# save model output
saveRDS(finemod.train_simple.summer, "finemod.train_simple.summer.RDS")
finemod.train_simple.summer <- file.path("finemod.train_simple.summer.RDS")




#### Simple Model Winter ----
gc()
finemod.train_simple.winter <- glmmTMB(case_ ~ -1 +
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
                                       family = poisson(), data = finemod.train.winter[,`:=`(year = as.factor(year), 
                                                                                             indiv_step_id = as.factor(indiv_step_id),
                                                                                             id = as.factor(id))],
                                       map= list(theta = factor(c(NA,1:10))),
                                       start = list(theta =c(log(1000), seq(0,0, length.out = 10))), 
                                       verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(finemod.train_simple.winter)
# save model output
saveRDS(finemod.train_simple.winter, "finemod.train_simple.winter.RDS")
finemod.train_simple.winter <- file.path("finemod.train_simple.winter.RDS")


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


#### Full mod roads comb summer ----
gc()
finemod.train_fullroadscomb.summer <- glmmTMB(case_ ~ -1 +
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
                                       family = poisson(), data = finemod.train.summer[,`:=`(year = as.factor(year), 
                                                                                      indiv_step_id = as.factor(indiv_step_id),
                                                                                      id = as.factor(id))],
                                       map= list(theta = factor(c(NA,1:14))),
                                       start = list(theta =c(log(1000), seq(0,0, length.out = 14))), 
                                       verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(finemod.train_fullroadscomb.summer)
# save model output
saveRDS(finemod.train_fullroadscomb.summer, "finemod.train_fullroadscomb.summer.RDS")
finemod.train_fullroadscomb.summer <- file.path("finemod.train_fullroadscomb.summer.RDS")


#### Full mod roads comb winter ----
gc()
finemod.train_fullroadscomb.winter <- glmmTMB(case_ ~ -1 +
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
                                              family = poisson(), data = finemod.train.winter[,`:=`(year = as.factor(year), 
                                                                                                    indiv_step_id = as.factor(indiv_step_id),
                                                                                                    id = as.factor(id))],
                                              map= list(theta = factor(c(NA,1:14))),
                                              start = list(theta =c(log(1000), seq(0,0, length.out = 14))), 
                                              verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(finemod.train_fullroadscomb.winter)
# save model output
saveRDS(finemod.train_fullroadscomb.winter, "finemod.train_fullroadscomb.winter.RDS")
finemod.train_fullroadscomb.winter <- file.path("finemod.train_fullroadscomb.winter.RDS")


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


#### Coarse mod, pa, fire60 Summer ----
gc()
coarsemod.train.pafire60.summer <- glmmTMB(case_ ~ -1 +
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
                                    family = poisson(), data = coarsemod.train.summer[,`:=`(year = as.factor(year), 
                                                                                     indiv_step_id = as.factor(indiv_step_id),
                                                                                     id = as.factor(id))],
                                    map= list(theta = factor(c(NA,1:15))),
                                    start = list(theta =c(log(1000), seq(0,0, length.out = 15))), 
                                    verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(coarsemod.train.pafire60.summer)
# save model output
saveRDS(coarsemod.train.pafire60.summer, "coarsemod.train.pafire60.summer.RDS")
#coarsemod.train.pafire60.summer <- readRDS("coarsemod.train.pafire60.summer.RDS")
coarsemod.train.pafire60.summer <- file.path("coarsemod.train.pafire60.summer.RDS")


#### Coarse mod, pa, fire60 Winter ----
gc()
coarsemod.train.pafire60.winter <- glmmTMB(case_ ~ -1 +
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
                                           family = poisson(), data = coarsemod.train.winter[,`:=`(year = as.factor(year), 
                                                                                                   indiv_step_id = as.factor(indiv_step_id),
                                                                                                   id = as.factor(id))],
                                           map= list(theta = factor(c(NA,1:15))),
                                           start = list(theta =c(log(1000), seq(0,0, length.out = 15))), 
                                           verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(coarsemod.train.pafire60.winter)
# save model output
saveRDS(coarsemod.train.pafire60.winter, "coarsemod.train.pafire60.winter.RDS")
#coarsemod.train.pafire60.winter <- readRDS("coarsemod.train.pafire60.winter.RDS")
coarsemod.train.pafire60.winter <- file.path("coarsemod.train.pafire60.winter.RDS")


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


#### Coarse mod, pa, fire2 Summer ----
gc()
coarsemod.train.pafire2.summer <- glmmTMB(case_ ~ -1 +
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
                                   family = poisson(), data = coarsemod.train.summer[,`:=`(year = as.factor(year), 
                                                                                    indiv_step_id = as.factor(indiv_step_id),
                                                                                    id = as.factor(id))],
                                   map= list(theta = factor(c(NA,1:15))),
                                   start = list(theta =c(log(1000), seq(0,0, length.out = 15))), 
                                   verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(coarsemod.train.pafire2.summer)
# save model output
saveRDS(coarsemod.train.pafire2.summer, "coarsemod.train.pafire2.summer.RDS")
#coarsemod.train.pafire2.summer <- readRDS("coarsemod.train.pafire2.summer.RDS")
coarsemod.train.pafire2.summer <- file.path("coarsemod.train.pafire2.summer.RDS")



#### Coarse mod, pa, fire2 Winter ----
gc()
coarsemod.train.pafire2.winter <- glmmTMB(case_ ~ -1 +
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
                                          family = poisson(), data = coarsemod.train.winter[,`:=`(year = as.factor(year), 
                                                                                                  indiv_step_id = as.factor(indiv_step_id),
                                                                                                  id = as.factor(id))],
                                          map= list(theta = factor(c(NA,1:15))),
                                          start = list(theta =c(log(1000), seq(0,0, length.out = 15))), 
                                          verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(coarsemod.train.pafire2.winter)
# save model output
saveRDS(coarsemod.train.pafire2.winter, "coarsemod.train.pafire2.winter.RDS")
#coarsemod.train.pafire2.winter <- readRDS("coarsemod.train.pafire2.winter.RDS")
coarsemod.train.pafire2.winter <- file.path("coarsemod.train.pafire2.winter.RDS")


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

#### Coarse mod, lf dens, fire60 Summer ----
gc()
coarsemod.train.lfdensfire60.summer <- glmmTMB(case_ ~ -1 +
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
                                        family = poisson(), data = coarsemod.train.summer[,`:=`(year = as.factor(year), 
                                                                                         indiv_step_id = as.factor(indiv_step_id),
                                                                                         id = as.factor(id))],
                                        map= list(theta = factor(c(NA,1:15))),
                                        start = list(theta =c(log(1000), seq(0,0, length.out = 15))), 
                                        verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(coarsemod.train.lfdensfire60.summer)
# save model output
saveRDS(coarsemod.train.lfdensfire60.summer, "coarsemod.train.lfdensfire60.summer.RDS")
#coarsemod.train.lfdensfire60.summer <- readRDS("coarsemod.train.lfdensfire60.summer.RDS")
coarsemod.train.lfdensfire60.summer <- file.path("coarsemod.train.lfdensfire60.summer.RDS")



#### Coarse mod, lf dens, fire60 Winter ----
gc()
coarsemod.train.lfdensfire60.winter <- glmmTMB(case_ ~ -1 +
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
                                               family = poisson(), data = coarsemod.train.winter[,`:=`(year = as.factor(year), 
                                                                                                       indiv_step_id = as.factor(indiv_step_id),
                                                                                                       id = as.factor(id))],
                                               map= list(theta = factor(c(NA,1:15))),
                                               start = list(theta =c(log(1000), seq(0,0, length.out = 15))), 
                                               verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(coarsemod.train.lfdensfire60.winter)
# save model output
saveRDS(coarsemod.train.lfdensfire60.winter, "coarsemod.train.lfdensfire60.winter.RDS")
#coarsemod.train.lfdensfire60.winter <- readRDS("coarsemod.train.lfdensfire60.winter.RDS")
coarsemod.train.lfdensfire60.winter <- file.path("coarsemod.train.lfdensfire60.winter.RDS")


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




#### Coarse mod, lf dens, fire2 Summer ----
gc()
coarsemod.train.lfdensfire2.summer <- glmmTMB(case_ ~ -1 +
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
                                       family = poisson(), data = coarsemod.train.summer[,`:=`(year = as.factor(year), 
                                                                                        indiv_step_id = as.factor(indiv_step_id),
                                                                                        id = as.factor(id))],
                                       map= list(theta = factor(c(NA,1:15))),
                                       start = list(theta =c(log(1000), seq(0,0, length.out = 15))), 
                                       verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(coarsemod.train.lfdensfire2.summer)
# save model output
saveRDS(coarsemod.train.lfdensfire2.summer, "coarsemod.train.lfdensfire2.summer.RDS")
#coarsemod.train.lfdensfire2.summer <- readRDS("coarsemod.train.lfdensfire2.summer.RDS")
coarsemod.train.lfdensfire2.summer <- file.path("coarsemod.train.lfdensfire2.summer.RDS")



#### Coarse mod, lf dens, fire2 Winter ----
gc()
coarsemod.train.lfdensfire2.winter <- glmmTMB(case_ ~ -1 +
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
                                              family = poisson(), data = coarsemod.train.winter[,`:=`(year = as.factor(year), 
                                                                                                      indiv_step_id = as.factor(indiv_step_id),
                                                                                                      id = as.factor(id))],
                                              map= list(theta = factor(c(NA,1:15))),
                                              start = list(theta =c(log(1000), seq(0,0, length.out = 15))), 
                                              verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(coarsemod.train.lfdensfire2.winter)
# save model output
saveRDS(coarsemod.train.lfdensfire2.winter, "coarsemod.train.lfdensfire2.winter.RDS")
#coarsemod.train.lfdensfire2.winter <- readRDS("coarsemod.train.lfdensfire2.winter.RDS")
coarsemod.train.lfdensfire2.winter <- file.path("coarsemod.train.lfdensfire2.winter.RDS")


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


#### Coarse mod, simple, fire60 Summer ----
gc()
coarsemod.train.simplefire60.summer <- glmmTMB(case_ ~ -1 +
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
                                        family = poisson(), data = coarsemod.train.summer[,`:=`(year = as.factor(year), 
                                                                                         indiv_step_id = as.factor(indiv_step_id),
                                                                                         id = as.factor(id))],
                                        map= list(theta = factor(c(NA,1:9))),
                                        start = list(theta =c(log(1000), seq(0,0, length.out = 9))), 
                                        verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(coarsemod.train.simplefire60.summer)
# save model output
saveRDS(coarsemod.train.simplefire60.summer, "coarsemod.train.simplefire60.summer.RDS")
#coarsemod.train.simplefire60.summer <- readRDS("coarsemod.train.simplefire60.summer.RDS")
coarsemod.train.simplefire60.summer <- file.path("coarsemod.train.simplefire60.summer.RDS")
#make_betas_tab(coarsemod.train.simplefire60)



#### Coarse mod, simple, fire60 Winter ----
gc()
coarsemod.train.simplefire60.winter <- glmmTMB(case_ ~ -1 +
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
                                               family = poisson(), data = coarsemod.train.winter[,`:=`(year = as.factor(year), 
                                                                                                       indiv_step_id = as.factor(indiv_step_id),
                                                                                                       id = as.factor(id))],
                                               map= list(theta = factor(c(NA,1:9))),
                                               start = list(theta =c(log(1000), seq(0,0, length.out = 9))), 
                                               verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(coarsemod.train.simplefire60.winter)
# save model output
saveRDS(coarsemod.train.simplefire60.winter, "coarsemod.train.simplefire60.winter.RDS")
#coarsemod.train.simplefire60.winter <- readRDS("coarsemod.train.simplefire60.winter.RDS")
coarsemod.train.simplefire60.winter <- file.path("coarsemod.train.simplefire60.winter.RDS")



### Coarse mod, simple, fire2 ----
gc()
coarsemod.train.simplefire2 <- glmmTMB(case_ ~ -1 +
                                          I(log((sl_+1)/1000)) +
                                          I(cos(ta_)) +
                                          I(log((sl_+1)/1000)):I(cos(ta_)) +
                                          prop_forest_1000_end + 
                                          prop_veg_1000_end + 
                                          fire_2perc1000_end + 
                                          dens_lf_1000_end + 
                                          dens_poly_1000_end + 
                                          
                                          (1|indiv_step_id) +
                                          (0 + I(log((sl_ +1)/1000))|id) +
                                          (0 + I(cos(ta_))|id) +
                                          (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                                          (0 + prop_forest_1000_end|id) +
                                          (0 + prop_veg_1000_end|id) +
                                          (0 + fire_2perc1000_end|id) +
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


summary(coarsemod.train.simplefire2)
# save model output
saveRDS(coarsemod.train.simplefire2, "coarsemod.train.simplefire2.RDS")
#coarsemod.train.simplefire2 <- readRDS("coarsemod.train.simplefire2.RDS")
coarsemod.train.simplefire2 <- file.path("coarsemod.train.simplefire2.RDS")


#### Coarse mod, simple, fire2 Summer ----
gc()
coarsemod.train.simplefire2.summer <- glmmTMB(case_ ~ -1 +
                                         I(log((sl_+1)/1000)) +
                                         I(cos(ta_)) +
                                         I(log((sl_+1)/1000)):I(cos(ta_)) +
                                         prop_forest_1000_end + 
                                         prop_veg_1000_end + 
                                         fire_2perc1000_end + 
                                         dens_lf_1000_end + 
                                         dens_poly_1000_end + 
                                         
                                         (1|indiv_step_id) +
                                         (0 + I(log((sl_ +1)/1000))|id) +
                                         (0 + I(cos(ta_))|id) +
                                         (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                                         (0 + prop_forest_1000_end|id) +
                                         (0 + prop_veg_1000_end|id) +
                                         (0 + fire_2perc1000_end|id) +
                                         (0 + dens_lf_1000_end|id) + 
                                         (0 + dens_poly_1000_end|id) + 
                                         
                                         (1|year),
                                       family = poisson(), data = coarsemod.train.summer[,`:=`(year = as.factor(year), 
                                                                                        indiv_step_id = as.factor(indiv_step_id),
                                                                                        id = as.factor(id))],
                                       map= list(theta = factor(c(NA,1:9))),
                                       start = list(theta =c(log(1000), seq(0,0, length.out = 9))), 
                                       verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(coarsemod.train.simplefire2.summer)
# save model output
saveRDS(coarsemod.train.simplefire2.summer, "coarsemod.train.simplefire2.summer.RDS")
#coarsemod.train.simplefire2.summer <- readRDS("coarsemod.train.simplefire2.summer.RDS")
coarsemod.train.simplefire2.summer <- file.path("coarsemod.train.simplefire2.summer.RDS")


#### Coarse mod, simple, fire2 Winter ----
gc()
coarsemod.train.simplefire2.winter <- glmmTMB(case_ ~ -1 +
                                                I(log((sl_+1)/1000)) +
                                                I(cos(ta_)) +
                                                I(log((sl_+1)/1000)):I(cos(ta_)) +
                                                prop_forest_1000_end + 
                                                prop_veg_1000_end + 
                                                fire_2perc1000_end + 
                                                dens_lf_1000_end + 
                                                dens_poly_1000_end + 
                                                
                                                (1|indiv_step_id) +
                                                (0 + I(log((sl_ +1)/1000))|id) +
                                                (0 + I(cos(ta_))|id) +
                                                (0 + I(log((sl_+1)/1000)):I(cos(ta_))|id) +
                                                (0 + prop_forest_1000_end|id) +
                                                (0 + prop_veg_1000_end|id) +
                                                (0 + fire_2perc1000_end|id) +
                                                (0 + dens_lf_1000_end|id) + 
                                                (0 + dens_poly_1000_end|id) + 
                                                
                                                (1|year),
                                              family = poisson(), data = coarsemod.train.winter[,`:=`(year = as.factor(year), 
                                                                                                      indiv_step_id = as.factor(indiv_step_id),
                                                                                                      id = as.factor(id))],
                                              map= list(theta = factor(c(NA,1:9))),
                                              start = list(theta =c(log(1000), seq(0,0, length.out = 9))), 
                                              verbose =TRUE, control = glmmTMBControl(rank_check = "adjust")
)


summary(coarsemod.train.simplefire2.winter)
# save model output
saveRDS(coarsemod.train.simplefire2.winter, "coarsemod.train.simplefire2.winter.RDS")
#coarsemod.train.simplefire2.winter <- readRDS("coarsemod.train.simplefire2.winter.RDS")
coarsemod.train.simplefire2.winter <- file.path("coarsemod.train.simplefire2.winter.RDS")



#RSFs
### RSF mod, simple ----
RSFmod.train.simple <- glmmTMB(
  detection ~ -1 +
    prop_forest_z +
    prop_veg_z +
    prop_barren_z +
    dens_lf_z +
    dens_poly_z +
    
    (1 | indiv_clusterID) +
    (0 + prop_forest_z | id) +
    (0 + prop_veg_z | id) +
    (0 + prop_barren_z | id) +
    (0 + dens_lf_z | id) +
    (0 + dens_poly_z | id) +
    (1 | year),
  
  data = rsfmod.train,
  family = binomial(),
  weights = npts,
  map = list(theta = factor(c(NA, 1:6))), 
  start = list(theta = c(log(1000), rep(0, 6))),
  verbose = TRUE
)

summary(RSFmod.train.simple)
# save model output
saveRDS(RSFmod.train.simple, "RSFmod.train.simple.RDS")
#RSFmod.train.simple <- readRDS("RSFmod.train.simple.RDS")
RSFmod.train.simple <- file.path("RSFmod.train.simple.RDS")


### RSF mod, mines int ----
gc()
RSFmod.train.minesint <- glmmTMB(
  detection ~ -1 +
    prop_forest_z +
    prop_veg_z +
    prop_barren_z +
    dens_lf_z +
    dens_poly_z +
    pa_quartz_z:dens_poly_z +
    pa_placer_z:dens_poly_z +
    
    (1 | indiv_clusterID) +
    (0 + prop_forest_z | id) +
    (0 + prop_veg_z | id) +
    (0 + prop_barren_z | id) +
    (0 + dens_lf_z | id) +
    (0 + dens_poly_z | id) +
    (0 + pa_quartz_z:dens_poly_z | id) +
    (0 + pa_placer_z:dens_poly_z | id) +
    
    (1 | year),
  
  data = rsfmod.train,
  family = binomial(),
  weights = npts,
  map = list(theta = factor(c(NA, 1:8))), 
  start = list(theta = c(log(1000), rep(0, 8))),
  verbose = TRUE
)


summary(RSFmod.train.minesint)
# save model output
saveRDS(RSFmod.train.minesint, "RSFmod.train.minesint.RDS")
#RSFmod.train.minesint <- readRDS("RSFmod.train.minesint.RDS")
RSFmod.train.minesint <- file.path("RSFmod.train.minesint.RDS")

### RSF mod, mines pa ----
gc()
RSFmod.train.minespa <- glmmTMB(
  detection ~ -1 +
    prop_forest_z +
    prop_veg_z +
    prop_barren_z +
    dens_lf_z +
    pa_quartz_z +
    pa_placer_z +
    
    (1 | indiv_clusterID) +
    (0 + prop_forest_z | id) +
    (0 + prop_veg_z | id) +
    (0 + prop_barren_z | id) +
    (0 + dens_lf_z | id) +
    (0 + pa_quartz_z | id) +
    (0 + pa_placer_z | id) +
    
    (1 | year),
  
  data = rsfmod.train,
  family = binomial(),
  weights = npts,
  map = list(theta = factor(c(NA, 1:7))), 
  start = list(theta = c(log(1000), rep(0, 7))),
  verbose = TRUE
)


summary(RSFmod.train.minespa)
# save model output
saveRDS(RSFmod.train.minespa, "RSFmod.train.minespa.RDS")
#RSFmod.train.minespa <- readRDS("RSFmod.train.minespa.RDS")
RSFmod.train.minespa <- file.path("RSFmod.train.minespa.RDS")



## Create Test Data ---- 
finemod.test_full <- finedat.10[year %in% c(2022, 2023, 2024, 2025) & case_ == TRUE]
finemod.test_simple <- finedat.15[year %in% c(2022, 2023, 2024, 2025) & case_ == TRUE]
finemod.test_fullroadscomb <- finedat.10[year %in% c(2022, 2023, 2024, 2025) & case_ == TRUE]
coarsemod.test <- coarsedat.20[year %in% c(2022, 2023, 2024, 2025) & case_ == TRUE]

rsfmod.test <- rsf.dat[year %in% c(2022, 2023, 2024, 2025) & detection == 1]
setnames(rsfmod.test, old = c("x", "y"), new = c("x1_", "y1_"))
rsfmod.test[, case_ := TRUE] #Making case_ column to match the issa name

#Make study area
sArea.finemod <- make_study_area(finemod.test_full, crs, buff = buffer)
sArea.coarsemod <- make_study_area(coarsemod.test_pafire60, crs, buff = buffer)
sArea.rsfmod <- make_study_area(rsfmod.test %>% rename(x2_ = x, y2_ = y), crs = crs, buff = buffer)


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


# Landcover for 5000m resolution
prop_barren_5000      <- rast(file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_barrenland_5000_2020.tif'))
prop_bryoids_5000      <- rast(file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_bryoids_5000_2020.tif'))
prop_shrub_5000        <- rast(file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_shrub_5000_2020.tif'))
prop_wetland_5000      <- rast(file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_wetland_5000_2020.tif'))
prop_wettreed_5000     <- rast(file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_wet_treed_5000_2020.tif'))
prop_herbs_5000        <- rast(file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_herbs_5000_2020.tif'))
prop_needleleaf_5000   <- rast(file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_needleleaf_5000_2020.tif'))
prop_deciduous_5000    <- rast(file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_deciduous_5000_2020.tif'))
prop_mixed_5000        <- rast(file.path('data', 'derived', 'prop_land', 'prop_land_split', 'prop_mixed_5000_2020.tif'))

# Fires 5000
fire_60perc5000        <- rast(file.path('data', 'derived', 'perc_burned_5000', 'burned60_5000m_2020.tif'))
fire_2perc5000         <- rast(file.path('data', 'derived', 'perc_burned_5000', 'burned2_5000m_2020.tif'))

# Harvest 5000
harv_dist_5000         <- rast(file.path('data', 'derived', 'harv_dist_5000m', 'harvdist_5000m_2020.tif'))
harv_pa_5000           <- rast(file.path('data', 'derived', 'harv_pa_5000m', 'harvpres_5000m_2020.tif'))
harv_dens_5000         <- rast(file.path('data', 'derived', 'harv_dens_5000m', 'harvdens_5000m_2020.tif'))


# Anthropogenic disturbance 5000
denslfother_5000      <- rast(file.path('data', 'derived', 'dens_disturb_5000m', 'otherlf_dens_5000m_2020.tif'))
denspaved_5000        <- rast(file.path('data', 'derived', 'dens_disturb_5000m', 'paved_dens_5000m_2020.tif'))
densunpaved_5000      <- rast(file.path('data', 'derived', 'dens_disturb_5000m', 'unpaved_dens_5000m_2020.tif'))
densplacer_5000       <- rast(file.path('data', 'derived', 'dens_disturb_5000m', 'placer_dens_5000m_2020.tif'))
densquartz_5000       <- rast(file.path('data', 'derived', 'dens_disturb_5000m', 'quartz_dens_5000m_2020.tif'))
densothermines_5000   <- rast(file.path('data', 'derived', 'dens_disturb_5000m', 'othermines_dens_5000m_2020.tif'))
densotherpoly_5000    <- rast(file.path('data', 'derived', 'dens_disturb_5000m', 'otherpoly_dens_5000m_2020.tif'))

papaved_5000          <- rast(file.path('data', 'derived', 'pa_disturb_5000m', 'paved_pa_5000m_2020.tif'))
paunpaved_5000        <- rast(file.path('data', 'derived', 'pa_disturb_5000m', 'unpaved_pa_5000m_2020.tif'))
palfother_5000        <- rast(file.path('data', 'derived', 'pa_disturb_5000m', 'otherlf_pa_5000m_2020.tif'))
paplacer_5000         <- rast(file.path('data', 'derived', 'pa_disturb_5000m', 'placer_pa_5000m_2020.tif'))
paquartz_5000         <- rast(file.path('data', 'derived', 'pa_disturb_5000m', 'quartz_pa_5000m_2020.tif'))
paothermines_5000     <- rast(file.path('data', 'derived', 'pa_disturb_5000m', 'othermines_pa_5000m_2020.tif'))
paotherpoly_5000      <- rast(file.path('data', 'derived', 'pa_disturb_5000m', 'otherpoly_pa_5000m_2020.tif'))


# prop_forest = sum of 4 layers
prop_forest_5000 <- prop_wettreed_5000 + prop_needleleaf_5000 + prop_deciduous_5000 + prop_mixed_5000

# prop_veg = sum of 3 layers
prop_veg_5000 <- prop_bryoids_5000 + prop_herbs_5000 + prop_shrub_5000

dens_lf_5000  <- denspaved_5000  + densunpaved_5000  + denslfother_5000 
dens_poly_5000  <- densotherpoly_5000  + densplacer_5000  + densquartz_5000  + densothermines_5000 + harv_dens_5000



#Disaggrigating so I can stack w fine scale
distpaved_1000_disagg <- disagg(distpaved_1000, fact = 4, method = "near")
distunpaved_1000_disagg <- disagg(distunpaved_1000, fact = 4, method = "near")
distlfother_1000_disagg <- disagg(distlfother_1000, fact = 4, method = "near")

dist_road_1000_disagg <- disagg(dist_road_1000, fact = 4, method = "near")

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



#Scaling 5000m rasters for RSF to match the model
# dens_lf
dens_lf_mean <- global(dens_lf_5000, mean, na.rm = TRUE)[[1]]
dens_lf_sd   <- global(dens_lf_5000, sd,   na.rm = TRUE)[[1]]
dens_lf_z <- (dens_lf_5000 - dens_lf_mean) / dens_lf_sd

# dens_poly
dens_poly_mean <- global(dens_poly_5000, mean, na.rm = TRUE)[[1]]
dens_poly_sd   <- global(dens_poly_5000, sd,   na.rm = TRUE)[[1]]
dens_poly_z <- (dens_poly_5000 - dens_poly_mean) / dens_poly_sd

# prop_forest
prop_forest_mean <- global(prop_forest_5000, mean, na.rm = TRUE)[[1]]
prop_forest_sd   <- global(prop_forest_5000, sd,   na.rm = TRUE)[[1]]
prop_forest_z <- (prop_forest_5000 - prop_forest_mean) / prop_forest_sd

# prop_veg
prop_veg_mean <- global(prop_veg_5000, mean, na.rm = TRUE)[[1]]
prop_veg_sd   <- global(prop_veg_5000, sd,   na.rm = TRUE)[[1]]
prop_veg_z <- (prop_veg_5000 - prop_veg_mean) / prop_veg_sd

# pa_quartz
pa_quartz_mean <- global(pa_quartz_5000, mean, na.rm = TRUE)[[1]]
pa_quartz_sd   <- global(pa_quartz_5000, sd,   na.rm = TRUE)[[1]]
pa_quartz_z <- (pa_quartz_5000 - pa_quartz_mean) / pa_quartz_sd

# pa_placer
pa_placer_mean <- global(pa_placer_5000, mean, na.rm = TRUE)[[1]]
pa_placer_sd   <- global(pa_placer_5000, sd,   na.rm = TRUE)[[1]]
pa_placer_z <- (pa_placer_5000 - pa_placer_mean) / pa_placer_sd

# prop_barren
prop_barren_mean <- global(prop_barren_5000, mean, na.rm = TRUE)[[1]]
prop_barren_sd   <- global(prop_barren_5000, sd,   na.rm = TRUE)[[1]]
prop_barren_z <- (prop_barren_5000 - prop_barren_mean) / prop_barren_sd

land2020_RSF <- c(
  prop_forest_z, prop_veg_z, prop_barren_z,
  pa_placer_z,
  pa_quartz_z,
  dens_lf_z, dens_poly_z
  
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

names(land2020_RSF) <- c(
  "prop_forest_z", "prop_veg_z", "prop_barren_z",
  "pa_placer_z",
  "pa_quartz_z",
  "dens_lf_z", "dens_poly_z"
  
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

land2020_RSF <- terra::project(land2020_RSF, crs_string)

#Full mod
gc()
testmyearsfinefull.ud <- mod2UD(modpath = finemod.train_full, envlayers = land2020, 
                        studyArea = vect(sArea.finemod))
plot(testmyearsfinefull.ud)

#saveRDS(testmyearsfinefull.ud, "testmyearsfinefull.ud.rds")
#testmyearsfinefull.ud <- readRDS("testmyearsfinefull.ud.rds")

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
  lf.int <- mod.tab[grepl('dist_lf_250_end', term) & grepl(":", term), estimate]
  lf.cov <- 2 * lf.main * land$log_dist_lf
  lf.cov.int <- 2 * lf.int * land$log_dist_lf * land$log_dist_lf_1000
  
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

#saveRDS(testmyearsfinesimple.ud, "testmyearsfinesimple.ud.rds")
#testmyearsfinesimple.ud <- readRDS("testmyearsfinesimple.ud.rds")

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

#saveRDS(testmyearsfinefullroads.ud, "testmyearsfinefullroads.ud.rds")
#testmyearsfinefullroads.ud <- readRDS("testmyearsfinefullroads.ud.rds")

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

#saveRDS(testmyearscoarsepafire60.ud, "testmyearscoarsepafire60.ud.rds")
#testmyearscoarsepafire60.ud <- readRDS("testmyearscoarsepafire60.ud.rds")

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

#saveRDS(testmyearscoarsepafire2.ud, "testmyearscoarsepafire2.ud.rds")
#testmyearscoarsepafire2.ud <- readRDS("testmyearscoarsepafire2.ud.rds")

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


#saveRDS(testmyearscoarsedensfire60.ud, "testmyearscoarsedensfire60.ud.rds")
#testmyearscoarsedensfire60.ud.ud <- readRDS("testmyearscoarsedensfire60.ud.rds")

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

#saveRDS(testmyearscoarsedensfire2.ud, "testmyearscoarsedensfire2.ud.rds")
#testmyearscoarsedensfire2.ud <- readRDS("testmyearscoarsedensfire2.ud.rds")

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
plot(testmyearscoarsesimpfire60.ud)

#saveRDS(testmyearscoarsesimpfire60.ud, "testmyearscoarsesimpfire60.ud.rds")
#testmyearscoarsesimpfire60.ud <- readRDS("testmyearscoarsesimpfire60.ud.rds")

gc()
testmyearscoarsesimpfire60.prop <- make_prop_use(coarsemod.test, "EPSG:3154", testmyearscoarsesimpfire60.ud)

gc()
used.bin.myearscoarsesimpfire60 <- make_map_bins(testmyearscoarsesimpfire60.prop)

colnames(coarsemod.test) <- make.unique(colnames(coarsemod.test))
gc()
propptsbin.myearscoarsesimpfire60 <- pts_in_bins(coarsemod.test, testmyearscoarsesimpfire60.ud)
plot(propptsbin.myearscoarsesimpfire60$use.bin, propptsbin.myearscoarsesimpfire60$prop.pts)
cor(propptsbin.myearscoarsesimpfire60$use.bin, propptsbin.myearscoarsesimpfire60$prop.pts, method = 'spearman', use = 'complete.obs')
#0.5093211

layerCor(c(testmyearscoarsesimpfire60.ud, used.bin.myearscoarsesimpfire60), fun = 'pearson', na.rm = T)

propptsbin.myearscoarsesimpfire60.tot <- pts_in_bins_tot(coarsemod.test, testmyearscoarsesimpfire60.ud)

plot(propptsbin.myearscoarsesimpfire60.tot$use.bin, propptsbin.myearscoarsesimpfire60.tot$prop.pts)
cor(propptsbin.myearscoarsesimpfire60.tot$use.bin, propptsbin.myearscoarsesimpfire60.tot$prop.pts, method = 'spearman', use = 'complete.obs')
#0.8666667


### Coarse mod simple, fire 2 ----
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
  fire.main <- mod.tab[term == 'fire_2perc1000_end' & !grepl(":", term), estimate]
  fire.cov <- 2 * fire.main * land$propfire2
  
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
testmyearscoarsesimpfire2.ud <- mod2UD(modpath = coarsemod.train.simplefire2, envlayers = land2020_coarse, 
                                        studyArea = vect(sArea.coarsemod))
plot(testmyearscoarsesimpfire2.ud)

#saveRDS(testmyearscoarsesimpfire2.ud, "testmyearscoarsesimpfire2.ud.rds")
#testmyearscoarsesimpfire2.ud <- readRDS("testmyearscoarsesimpfire2.ud.rds")

gc()
testmyearscoarsesimpfire2.prop <- make_prop_use(coarsemod.test, "EPSG:3154", testmyearscoarsesimpfire2.ud)

gc()
used.bin.myearscoarsesimpfire2 <- make_map_bins(testmyearscoarsesimpfire2.prop)

colnames(coarsemod.test) <- make.unique(colnames(coarsemod.test))
gc()
propptsbin.myearscoarsesimpfire2 <- pts_in_bins(coarsemod.test, testmyearscoarsesimpfire2.ud)
plot(propptsbin.myearscoarsesimpfire2$use.bin, propptsbin.myearscoarsesimpfire2$prop.pts)
cor(propptsbin.myearscoarsesimpfire2$use.bin, propptsbin.myearscoarsesimpfire2$prop.pts, method = 'spearman', use = 'complete.obs')
#0.4559997

layerCor(c(testmyearscoarsesimpfire2.ud, used.bin.myearscoarsesimpfire2), fun = 'pearson', na.rm = T)

propptsbin.myearscoarsesimpfire2.tot <- pts_in_bins_tot(coarsemod.test, testmyearscoarsesimpfire2.ud)

plot(propptsbin.myearscoarsesimpfire2.tot$use.bin, propptsbin.myearscoarsesimpfire2.tot$prop.pts)
cor(propptsbin.myearscoarsesimpfire2.tot$use.bin, propptsbin.myearscoarsesimpfire2.tot$prop.pts, method = 'spearman', use = 'complete.obs')
#0.7212121



### RSF Mod, Simple ----
make_pde <- function(mod.tab, land, saveName = NULL) {
  # Lf
  lf.main <- mod.tab[term == 'dens_lf_z' & !grepl(":", term), estimate]
  lf.cov <- lf.main * land$dens_lf_z
  
  # Poly
  poly.main <- mod.tab[term == 'dens_poly_z' & !grepl(":", term), estimate]
  poly.cov <- poly.main * land$dens_poly_z
  
  # Prop forest
  prop_forest.main <- mod.tab[term == 'prop_forest_z' & !grepl(":", term), estimate]
  prop_forest.cov <- prop_forest.main * land$prop_forest_z
  
  # Prop veg
  prop_veg.main <- mod.tab[term == 'prop_veg_z' & !grepl(":", term), estimate]
  prop_veg.cov <- prop_veg.main * land$prop_veg_z
  
  # Prop veg
  prop_barren.main <- mod.tab[term == 'prop_barren_z' & !grepl(":", term), estimate]
  prop_barren.cov <- prop_barren.main * land$prop_barren_z
  
  # Combine all terms in numerator
  numerator <- exp(
    lf.cov  + poly.cov +
      prop_forest.cov + prop_veg.cov  + prop_barren.cov
  )
  
  pde <- numerator
  
  # Save if requested
  if (!is.null(saveName)) {
    writeRaster(pde, file.path(derived, saveName), overwrite = TRUE)
  }
  
  return(pde)
}



gc()
testmyearsrsfsimp.ud <- mod2UD(modpath = RSFmod.train.simple, envlayers = land2020_RSF, 
                                       studyArea = vect(sArea.rsfmod))
plot(testmyearsrsfsimp.ud)

#saveRDS(testmyearsrsfsimp.ud, "testmyearsrsfsimp.ud.rds")
#testmyearsrsfsimp.ud <- readRDS("testmyearsrsfsimp.ud.rds")

gc()
testmyearsrsfsimp.prop <- make_prop_use(rsfmod.test, "EPSG:3154", testmyearsrsfsimp.ud)


gc()
used.bin.myearsrsfsimp <- make_map_bins(testmyearsrsfsimp.prop)

colnames(rsfmod.test) <- make.unique(colnames(rsfmod.test))
gc()
propptsbin.myearsrsfsimp <- pts_in_bins(rsfmod.test, testmyearsrsfsimp.ud)
plot(propptsbin.myearsrsfsimp$use.bin, propptsbin.myearsrsfsimp$prop.pts)
cor(propptsbin.myearsrsfsimp$use.bin, propptsbin.myearsrsfsimp$prop.pts, method = 'spearman', use = 'complete.obs')
#-0.004992593

layerCor(c(testmyearsrsfsimp.ud, used.bin.myearsrsfsimp), fun = 'pearson', na.rm = T)

propptsbin.myearsrsfsimp.tot <- pts_in_bins_tot(rsfmod.test, testmyearsrsfsimp.ud)

plot(propptsbin.myearsrsfsimp.tot$use.bin, propptsbin.myearsrsfsimp.tot$prop.pts)
cor(propptsbin.myearsrsfsimp.tot$use.bin, propptsbin.myearsrsfsimp.tot$prop.pts, method = 'spearman', use = 'complete.obs')
#-0.2606061????

#Really bad model....



### RSF Mod, mines int ----
make_pde <- function(mod.tab, land, saveName = NULL) {
  # Lf
  lf.main <- mod.tab[term == 'dens_lf_z' & !grepl(":", term), estimate]
  lf.cov <- lf.main * land$dens_lf_z
  
  # Poly
  poly.main <- mod.tab[term == 'dens_poly_z' & !grepl(":", term), estimate]
  poly.cov <- poly.main * land$dens_poly_z
  
  # Prop forest
  prop_forest.main <- mod.tab[term == 'prop_forest_z' & !grepl(":", term), estimate]
  prop_forest.cov <- prop_forest.main * land$prop_forest_z
  
  # Prop veg
  prop_veg.main <- mod.tab[term == 'prop_veg_z' & !grepl(":", term), estimate]
  prop_veg.cov <- prop_veg.main * land$prop_veg_z
  
  # Prop veg
  prop_barren.main <- mod.tab[term == 'prop_barren_z' & !grepl(":", term), estimate]
  prop_barren.cov <- prop_barren.main * land$prop_barren_z
  
  #Quartz int
  quartz_int.main <- mod.tab[term == 'dens_poly_z:pa_quartz_z', estimate]
  quartz_int.cov <- quartz_int.main * land$dens_poly_z * land$pa_quartz_z
  
  #Placer int
  placer_int.main <- mod.tab[term == 'dens_poly_z:pa_placer_z', estimate]
  placer_int.cov <- placer_int.main * land$dens_poly_z * land$pa_placer_z
  
  # Combine all terms in numerator
  numerator <- exp(
    lf.cov  + poly.cov +
      prop_forest.cov + prop_veg.cov  + prop_barren.cov +
      placer_int.cov + quartz_int.cov
  )
  
  pde <- numerator
  
  # Save if requested
  if (!is.null(saveName)) {
    writeRaster(pde, file.path(derived, saveName), overwrite = TRUE)
  }
  
  return(pde)
}


gc()
testmyearsrsfminesint.ud <- mod2UD(modpath = RSFmod.train.minesint, envlayers = land2020_RSF, 
                               studyArea = vect(sArea.rsfmod))
plot(testmyearsrsfminesint.ud)

#saveRDS(testmyearsrsfminesint.ud, "testmyearsrsfminesint.ud.rds")
#testmyearsrsfminesint.ud <- readRDS("testmyearsrsfminesint.ud.rds")

gc()
testmyearsrsfminesint.prop <- make_prop_use(rsfmod.test, "EPSG:3154", testmyearsrsfminesint.ud)

gc()
used.bin.myearsrsfminesint <- make_map_bins(testmyearsrsfminesint.prop)

colnames(rsfmod.test) <- make.unique(colnames(rsfmod.test))
gc()
propptsbin.myearsrsfminesint <- pts_in_bins(rsfmod.test, testmyearsrsfminesint.ud)
plot(propptsbin.myearsrsfminesint$use.bin, propptsbin.myearsrsfsimp$prop.pts)
cor(propptsbin.myearsrsfminesint$use.bin, propptsbin.myearsrsfsimp$prop.pts, method = 'spearman', use = 'complete.obs')
#-0.02711126

layerCor(c(testmyearsrsfminesint.ud, used.bin.myearsrsfminesint), fun = 'pearson', na.rm = T)

propptsbin.myearsrsfminesint.tot <- pts_in_bins_tot(rsfmod.test, testmyearsrsfminesint.ud)

plot(propptsbin.myearsrsfminesint.tot$use.bin, propptsbin.myearsrsfminesint.tot$prop.pts)
cor(propptsbin.myearsrsfminesint.tot$use.bin, propptsbin.myearsrsfminesint.tot$prop.pts, method = 'spearman', use = 'complete.obs')
#-0.07878788

#Really bad model, again....




### RSF Mod, mines pa ----
make_pde <- function(mod.tab, land, saveName = NULL) {
  # Lf
  lf.main <- mod.tab[term == 'dens_lf_z' & !grepl(":", term), estimate]
  lf.cov <- lf.main * land$dens_lf_z
  
  # Prop forest
  prop_forest.main <- mod.tab[term == 'prop_forest_z' & !grepl(":", term), estimate]
  prop_forest.cov <- prop_forest.main * land$prop_forest_z
  
  # Prop veg
  prop_veg.main <- mod.tab[term == 'prop_veg_z' & !grepl(":", term), estimate]
  prop_veg.cov <- prop_veg.main * land$prop_veg_z
  
  # Prop veg
  prop_barren.main <- mod.tab[term == 'prop_barren_z' & !grepl(":", term), estimate]
  prop_barren.cov <- prop_barren.main * land$prop_barren_z
  
  #Quartz pa
  quartz.main <- mod.tab[term == 'pa_quartz_z', estimate]
  quartz.cov <- quartz.main * land$pa_quartz_z
  
  #Placer pa
  placer.main <- mod.tab[term == 'pa_placer_z', estimate]
  placer.cov <- placer.main * land$pa_placer_z
  
  # Combine all terms in numerator
  numerator <- exp(
    lf.cov +
      prop_forest.cov + prop_veg.cov  + prop_barren.cov +
      placer.cov + quartz.cov
  )
  
  pde <- numerator
  
  # Save if requested
  if (!is.null(saveName)) {
    writeRaster(pde, file.path(derived, saveName), overwrite = TRUE)
  }
  
  return(pde)
}


gc()
testmyearsrsfminespa.ud <- mod2UD(modpath = RSFmod.train.minespa, envlayers = land2020_RSF, 
                                   studyArea = vect(sArea.rsfmod))
plot(testmyearsrsfminespa.ud)

#saveRDS(testmyearsrsfminespa.ud, "testmyearsrsfminespa.ud.rds")
#testmyearsrsfminespa.ud <- readRDS("testmyearsrsfminespa.ud.rds")

gc()
testmyearsrsfminespa.prop <- make_prop_use(rsfmod.test, "EPSG:3154", testmyearsrsfminespa.ud)

gc()
used.bin.myearsrsfminespa <- make_map_bins(testmyearsrsfminespa.prop)

colnames(rsfmod.test) <- make.unique(colnames(rsfmod.test))
gc()
propptsbin.myearsrsfminespa <- pts_in_bins(rsfmod.test, testmyearsrsfminespa.ud)
plot(propptsbin.myearsrsfminespa$use.bin, propptsbin.myearsrsfminespa$prop.pts)
cor(propptsbin.myearsrsfminespa$use.bin, propptsbin.myearsrsfminespa$prop.pts, method = 'spearman', use = 'complete.obs')
#0.32

layerCor(c(testmyearsrsfminespa.ud, used.bin.myearsrsfminespa), fun = 'pearson', na.rm = T)

propptsbin.myearsrsfminespa.tot <- pts_in_bins_tot(rsfmod.test, testmyearsrsfminespa.ud)

plot(propptsbin.myearsrsfminespa.tot$use.bin, propptsbin.myearsrsfminespa.tot$prop.pts)
cor(propptsbin.myearsrsfminespa.tot$use.bin, propptsbin.myearsrsfminespa.tot$prop.pts, method = 'spearman', use = 'complete.obs')
#0.52

#Not bad!




### Plotting ----
prop_list <- list(
  "Coarse Simple Fire 2"  = propptsbin.myearscoarsesimpfire2.tot,
  "Coarse Simple Fire 60" = propptsbin.myearscoarsesimpfire60.tot,
  "Coarse Lf Dens Fire 2"   = propptsbin.myearscoarsedensfire2.tot,
  "Coarse Lf Dens Fire 60"  = propptsbin.myearscoarsedensfire60.tot,
  "Coarse PA Fire 2"      = propptsbin.myearscoarsepafire2.tot,
  "Coarse PA Fire 60"     = propptsbin.myearscoarsepafire60.tot,
  "Fine Full Roads Comb"       = propptsbin.myearsfinefullroads.tot,
  "Fine Simple"           = propptsbin.myearsfinesimple.tot,
  "Fine Full"             = propptsbin.myearsfinefull.tot
)

combined_df <- bind_rows(
  lapply(names(prop_list), function(name) {
    df <- prop_list[[name]]
    df$Variation <- name
    return(df)
  })
)

soft_colors <- c(
  "#66c2a5",  
  "#fc8d62",  
  "#8da0cb",  
  "#e78ac3",  
  "#a6d854",  
  "#ffd92f",  
  "#e5c494", 
  "#b3b3b3",  
  "#a6cee3"   
)

# Plot
ggplot(combined_df, aes(x = use.bin, y = prop.pts, color = Variation)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_color_manual(values = soft_colors) +
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

