# === Predicted use maps -------------------------------------
# Peter Baker, Julie Turner, Katie Florko
# started July 18, 2025

#### Packages ####
libs <- c('Require', 'reproducible', 'data.table', 'terra','sf',
          'glmmTMB', 'ggplot2', 'rasterVis', 'viridis', 'RColorBrewer',
          'tidyterra', 'patchwork')
lapply(libs, Require::Require, character.only = TRUE)

# my functions
lapply(dir('R', '*.R', full.names = TRUE), source)

### Input data ----
raw <-  file.path('data', 'raw')
derived <- file.path('data', 'derived')
canada <- file.path('C:','Users', 'julie', 'Dropbox', 'ActiveDocs', 'Git-local', 'Can_GIS_layers')
dataDrive <- file.path('G:', 'Shared drives', 'WSC_Yukon', 'CEtool_component3', 'data_models_new', 'Fine_Models', 'data')

canPoly <- st_read(file.path(canada, 'CanadaPoly', 'lpr_000b16a_e.shp'))
#canPoly <- st_read('lpr_000b16a_e.shp')
dat <- readRDS(file.path(derived, 'clean_NMC.RDS'))


#Map 1 is density points on top of herd boundaries, on top of BC and YT shapefile
mapCRS <- st_crs(4326)$wkt
ytbc <- subset(canPoly, canPoly$PREABBR %in% c('B.C.', 'Y.T.', 'N.W.T.'))
ytbc.wgs <- st_transform(ytbc, mapCRS)
ytbc.ext <- st_bbox(ytbc.wgs)
nmcherds <- st_read(file.path(raw, "nmcherds2.shp"))
# katie update: not including migratory herds
# nmcherds <- st_read("G:/Shared drives/WSC_Yukon/CEtool_component3/demogData/nmc_herds_updated/nmc_herds_updated_merged.shp")
nmcherds <- st_transform(nmcherds, crs = st_crs(ytbc.wgs))

dat.sf <- st_as_sf(dat, coords = c("x_", "y_"), crs = 3154)

dat.sf <- st_transform(dat.sf, mapCRS)

dat.within <- st_crop(dat.sf, st_bbox(ytbc.wgs))

towns <- st_read(file.path(raw, "towns.shp"))
herd_names <- st_read(file.path(raw, "herd_names.shp"))

towns <- st_transform(towns, st_crs(ytbc.wgs))
herd_names <- st_transform(herd_names, st_crs(ytbc.wgs))

library(dplyr)

herd_labels <- herd_names %>%
  mutate(
    x = st_coordinates(.)[,1],
    y = st_coordinates(.)[,2],
    x = case_when(
      `Herd.Name` == "Atlin" ~ x + 0,      
      `Herd.Name` == "Muskwa" ~ x + 1.4,     
      TRUE ~ x
    ),
    y = case_when(
      `Herd.Name` == "Atlin" ~ y - 0.2,      
      `Herd.Name` == "Muskwa" ~ y + 0.2,     
      TRUE ~ y
    )
  )

town_labels <- towns %>%
  mutate(
    x = st_coordinates(.)[,1],
    y = st_coordinates(.)[,2],
    x = case_when(
      `Town.Name` == "Fort Simpson" ~ x + 0,
      `Town.Name` == "Fort St John" ~ x + 0,
      `Town.Name` == "Fort Nelson"  ~ x + 1.8,
      `Town.Name` == "Whitehorse"  ~ x - 1.4,
      TRUE ~ x
    ),
    y = case_when(
      `Town.Name` == "Fort Simpson" ~ y - 0.2,
      `Town.Name` == "Fort St John" ~ y - 0.2,
      `Town.Name` == "Fort Nelson"  ~ y - 0.2,
      `Town.Name` == "Whitehorse"  ~ y - 0.2,
      TRUE ~ y
    )
  )



map1 <- ggplot() +
  geom_sf(data = ytbc.wgs, fill = "gray95", color = "black") +
  geom_sf(data = nmcherds, fill = "white", color = "grey50", alpha = 0.5) +
  geom_sf(data = dat.within, color = "#6A00A8FF", alpha = 0.01, size = 0.3) +
  geom_sf(data = towns, size = 1, color = "black") +
  geom_text(data = town_labels, aes(x = x, y = y, label = `Town.Name`), size = 3) +
  geom_text(data = herd_labels, aes(x = x, y = y, label = `Herd.Name`), 
            size = 3, color = "grey50") +
  
  coord_sf(xlim = c(-141.01807, -118), ylim = c(55, 68), expand = FALSE) +
  theme_minimal() +
  labs(x = NULL, y = NULL)
ggsave("map1.png", plot = map1, width = 6, height = 7.5, units = "in", dpi = 300, device = "png")

#Map 2 is PDE ----
# quick and dirty -- being updated
ftNelBuff <- towns |>
  filter(Town.Name == 'Fort Nelson') |>
  st_buffer(30000)


pdeFine <- rast(file.path(dataDrive, 'pde_simple.tif'))
pdeFine <- project(pdeFine, mapCRS)
ytbcVect <- vect(ytbc.wgs)
nmcherdsVect <- vect(nmcherds)

# remove Ft Nel points (this is fixed in next iter)
ftNelRast <- rasterize(vect(ftNelBuff), pdeFine)
pdeFineMask <- mask(pdeFine, ftNelRast, inverse = T)
plot(pdeFineMask)

maxPde <- global(pdeFine, max, na.rm = T)[[1]]
minPde <- global(pdeFine, min, na.rm = T)[[1]]

ggplot() +
  geom_spatvector(data = ytbcVect, fill = "gray95", color = "black") +
  geom_spatraster(data = as.numeric(pdeFineMask), show.legend = T) +
  scale_fill_viridis(option = 'plasma', na.value = NA) +
  # name = 'Intensity of selection', 
  # breaks = c(minPde, maxPde), 
   #labels = c('low', 'high')) +
  geom_spatvector(data = nmcherdsVect, fill = NA, color = "lightblue", alpha = 0.6) +
  geom_sf(data = towns, size = 1, color = "black") +
  geom_text(data = town_labels, aes(x = x, y = y, label = `Town.Name`), size = 3) +
  # geom_text(data = herd_labels, aes(x = x, y = y, label = `Herd.Name`), 
  #           size = 3, color = "blue") +
  
  coord_sf(xlim = c(-141.01807, -118), ylim = c(55, 68), expand = FALSE) +
  theme_minimal() +
  labs(x = NULL, y = NULL)

pdeCoarse<- rast(file.path(dataDrive, 'pde_coarse.tif'))
pdeCoarse <- project(pdeCoarse, mapCRS)

pdeCoarseMask <- mask(pdeCoarse, ftNelRast, inverse = T)
plot(pdeCoarseMask)

maxPdeCoarse <- global(pdeCoarse, max, na.rm = T)[[1]]
minPdeCoarse <- global(pdeCoarse, min, na.rm = T)[[1]]

ggplot() +
  geom_spatvector(data = ytbcVect, fill = "gray95", color = "black") +
  geom_spatraster(data = as.numeric(pdeCoarseMask), show.legend = T) +
  scale_fill_viridis(option = 'plasma', na.value = NA,
                     name = stringr::str_wrap('Utilization distribution', width = 13),
                     breaks = c(minPde, maxPdeCoarse),
                     labels = c('low', 'high')) +
  geom_spatvector(data = nmcherdsVect, fill = NA, color = "lightblue", alpha = 0.6) +
  geom_sf(data = towns, size = 1, color = "black") +
  geom_text(data = town_labels, aes(x = x, y = y, label = `Town.Name`), size = 3) +
  # geom_text(data = herd_labels, aes(x = x, y = y, label = `Herd.Name`), 
  #           size = 3, color = "blue") +
  
  coord_sf(xlim = c(-141.01807, -118), ylim = c(55, 68), expand = FALSE) +
  theme_minimal() +
  labs(x = NULL, y = NULL)


#### OLD 
# TODO update with new functions
#Map 2 is PDE
dat.sf <- st_transform(dat.sf, 3154)
studyArea <- st_buffer(dat.sf, dist = 10000)
sa.union <- st_union(studyArea)
sa.union
plot(sa.union)
#sa.vect <- vect(sa.union)
st_write(sa.union, file.path(derived, 'NMC_10kmBuff.shp'), append = F)


#Loading layers to stack, starting with prop_land
prop_wettreed <- rast(file.path(derived, 'prop_land', 'prop_land_split', 'prop_wet_treed_1000_2020.tif'))
prop_needleleaf <- rast(file.path(derived, 'prop_land', 'prop_land_split', 'prop_needleleaf_1000_2020.tif'))
prop_deciduous <- rast(file.path(derived, 'prop_land', 'prop_land_split', 'prop_deciduous_1000_2020.tif'))
prop_mixed <- rast(file.path(derived, 'prop_land', 'prop_land_split', 'prop_mixed_1000_2020.tif'))

prop_bryoids <- rast(file.path(derived, 'prop_land', 'prop_land_split', 'prop_bryoids_1000_2020.tif'))
prop_herbs <- rast(file.path(derived, 'prop_land', 'prop_land_split', 'prop_herbs_1000_2020.tif'))
prop_shrub  <- rast(file.path(derived, 'prop_land', 'prop_land_split', 'prop_shrub_1000_2020.tif'))


# prop_forest = sum of 4 layers
prop_forest <- prop_wettreed + prop_needleleaf + prop_deciduous + prop_mixed

# prop_veg = sum of 3 layers
prop_veg <- prop_bryoids + prop_herbs + prop_shrub


#Harvest p/a
harvpa <- rast(file.path(derived, 'harv_pa_1000m', 'harvpres_1000m_2020.tif'))

#Fires, percburned60
firesperc60 <- rast(file.path(derived, 'perc_burned_1000', 'burned60_1000m_2020.tif'))

#Density lf
denspaved <- rast(file.path(derived, 'dens_disturb_1000m', 'paved_dens_1000m_2020.tif'))
densunpaved <- rast(file.path(derived, 'dens_disturb_1000m', 'unpaved_dens_1000m_2020.tif'))
denslfother <- rast(file.path(derived, 'dens_disturb_1000m', 'otherlf_dens_1000m_2020.tif'))

#Pa other features
paotherpoly <- rast(file.path(derived, 'pa_disturb_1000m', 'otherpoly_pa_1000m_2020.tif'))
paplacer <- rast(file.path(derived, 'pa_disturb_1000m', 'placer_pa_1000m_2020.tif'))
paquartz <- rast(file.path(derived, 'pa_disturb_1000m', 'quartz_pa_1000m_2020.tif'))
paothermines <- rast(file.path(derived, 'pa_disturb_1000m', 'othermines_pa_1000m_2020.tif'))

template <- prop_forest

prop_veg <- resample(prop_veg, template, method = "bilinear")
harvpa <- resample(harvpa, template, method = "near")
firesperc60 <- resample(firesperc60, template, method = "near")
denspaved <- resample(denspaved, template, method = "bilinear")
densunpaved <- resample(densunpaved, template, method = "bilinear")
denslfother <- resample(denslfother, template, method = "bilinear")
paotherpoly <- resample(paotherpoly, template, method = "near")
paplacer <- resample(paplacer, template, method = "near")
paquartz <- resample(paquartz, template, method = "near")
paothermines <- resample(paothermines, template, method = "near")

#Stacking
r_stack <- c(prop_forest, prop_veg, harvpa, firesperc60, denspaved, densunpaved, denslfother,
             paotherpoly, paplacer, paquartz, paothermines)
sa_vect <- vect(sa.union)
stack_cropped <- crop(r_stack, sa_vect)
stack_cropped <- mask(stack_cropped, sa_vect)

names(stack_cropped) <- c("prop_forest", "prop_veg", "harvpa", "firesperc60", "denspaved", 
                    "densunpaved", "denslfother", "paotherpoly", "paplacer", "paquartz", "paothermines")

r <- stack_cropped

r$log_fire60 <- log(r$firesperc60 + 1)
r$log_harvpa <- log(r$harvpa + 1)
r$log_denspaved <- log(r$denspaved + 1)
r$log_densunpaved <- log(r$densunpaved + 1)
r$log_denslfother <- log(r$denslfother + 1)
r$log_paotherpoly <- log(r$paotherpoly + 1)
r$log_paplacer <- log(r$paplacer + 1)
r$log_paquartz <- log(r$paquartz + 1)
r$log_paothermines <- log(r$paothermines + 1)




#Model
gc()
m.densfires6020step <- readRDS("m.densfires6020step.RDS")
m.densfires6020stepsum <- readRDS("m.densfires6020stepsum.RDS")
m.densfires6020stepwin <- readRDS("m.densfires6020stepwin.RDS")



### PDE ----

make_betas_tab <- function(mod){
  sum <- broom.mixed::tidy(mod, effect = 'fixed')
  as.data.table(sum)[,.(term, estimate)]
}

gc()
m.densfires6020step.tab <- make_betas_tab(m.densfires6020step)
m.densfires6020stepsum.tab <- make_betas_tab(m.densfires6020stepsum)
m.densfires6020stepwin.tab <- make_betas_tab(m.densfires6020stepwin)

#### full model PDE functions ----
gc()

make_pde <- function(mod.tab, land, saveName = NULL){
  paved.cov<- (2*as.double(mod.tab[term %like% 'dens_paved_1000_end', 
                                .(estimate)])*r$log_denspaved)
  lfother.cov<- (2*as.double(mod.tab[term %like% 'dens_lfother_1000_end', 
                                     .(estimate)])*r$log_denslfother)
  unpaved.cov<- (2*as.double(mod.tab[term %like% 'dens_unpaved_1000_end', 
                                 .(estimate)])*r$log_densunpaved)
  harv.cov<- (2*as.double(mod.tab[term %like% 'harv_pa_1000_end', 
                                 .(estimate)])*r$log_harvpa)
  fireperc.cov <- (2*as.double(mod.tab[term %like% 'fire_60perc1000_end', 
                                         .(estimate)])*r$log_fire60)
  veg.cov <- (2*as.double(mod.tab[term %like% 'prop_veg_1000_end', 
                                  .(estimate)])*r$prop_veg)
  forest.cov <- (2*as.double(mod.tab[term %like% 'prop_forest_1000_end',
                                        .(estimate)])*r$prop_forest)
  quartz.cov <- (2*as.double(mod.tab[term %like% 'pa_quartz_1000_end', 
                                   .(estimate)])*r$log_paquartz)
  placer.cov <- (2*as.double(mod.tab[term %like% 'pa_placer_1000_end', 
                                      .(estimate)])*r$log_paplacer)
  otherpoly.cov <- (2*as.double(mod.tab[term %like% 'pa_otherpoly_1000_end', 
                                     .(estimate)])*r$log_paotherpoly)
  othermines.cov <- (2*as.double(mod.tab[term %like% 'pa_othermines_1000_end', 
                                     .(estimate)])*r$log_paothermines)
  
  numerator <- exp(paved.cov + lfother.cov + 
                     unpaved.cov + harv.cov + 
                     fireperc.cov + veg.cov + 
                     forest.cov + quartz.cov +
                     placer.cov + otherpoly.cov +
                     othermines.cov)
  
  
  # the normalizing constant.
  C <- global(numerator, sum, na.rm = T)
  pde <- numerator/C[[1]]
  
  if(!is.null(saveName)){
    writeRaster(pde, 
                file.path(derived, saveName), overwrite = T)
  }
  
  return(pde)
}



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



### 2020

pde.2020 <- make_pde(m.densfires6020step.tab, sa.union)
pde.2020sum <- make_pde(m.densfires6020stepsum.tab, sa.union)
pde.2020win <- make_pde(m.densfires6020stepwin.tab, sa.union)

#Year-round
pde.2020.sa <- make_pde_map(pde.2020, sa.union, saveName = 'pde2020_coarsedens.tif')
plot(pde.2020.sa, breaks = 0:10)

#Summer
pde.2020.sasum <- make_pde_map(pde.2020sum, sa.union, saveName = 'pde2020_coarsedenssum.tif')
plot(pde.2020.sasum, breaks = 0:10)

#Winter
pde.2020.sawin <- make_pde_map(pde.2020win, sa.union, saveName = 'pde2020_coarsedenswin.tif')
plot(pde.2020.sawin, breaks = 0:10)








####OLD CODE





## Extract values ----
canPoly <- vect(file.path(canada, 'CanadaPoly', 'lpr_000b16a_e.shp'))
# # make study area of available points
# datSteps <- readRDS(file.path(derived, 'dat_iSSA.RDS'))
# coords<- datSteps%>%st_as_sf(coords = c('x2_','y2_'))%>%
#   st_set_crs(st_crs(3978))
# ## 10 km buffer around points to get an idea of extent of study area ----
# gc()
# studyArea <- st_buffer(coords, dist = 10000)
# gc()
# sa.union <- st_union(studyArea)
# ## save buffered study area ----
# st_write(sa.union, file.path('data', 'derived-data', 'prepped-data', 'WBIiSSAdat_10kmBuff.shp'), append = F)
ext <- vect(file.path('data', 'derived-data', 'prepped-data', 'WBIprepDat_10kmBuff.shp'))
issaArea <- vect(file.path('data', 'derived-data', 'prepped-data', 'WBIiSSAdat_10kmBuff.shp'))
ab.range <- vect(file.path(canada, 'AB_CaribouSubregionalPlanBoundaries', 'CARIBOU_SUB_REGIONAL_PLAN_BOUNDARIES_2022_07_04.shp'))
dus <- vect(file.path(raw, 'juris_herds', 'wbi_herds.shp'))
wbi.dus.data <- subset(dus, !(dus$herd %in% c('Sahtu', 'Bear River', 'Cameron Hills')))
plot(wbi.dus.data)

wbi.sa <- union(wbi.dus.data, issaArea)
studyArea <- aggregate(wbi.sa, disolve = T)

## prep study areas for sims
studyArea.buff <- buffer(studyArea, 50000)
#writeVector(studyArea, file.path(derived, 'studyArea_4maps.shp'))
#studyArea <- issaArea

studyArea.sf <- st_as_sf(studyArea.buff)
studyArea.ch <- st_convex_hull(studyArea.sf)
st_write(studyArea.ch, file.path(derived, 'studyArea_4sims.shp'))
studyArea.split <- st_cast(studyArea.sf, 'POLYGON')
mbsk <- studyArea.split$geometry[1]
st_write(mbsk, file.path(derived, 'studyArea_mbsk_4sims.shp'))
bcnwt <- studyArea.split$geometry[2]
st_write(bcnwt, file.path(derived, 'studyArea_bcnwt_4sims.shp'))
nnwt <- studyArea.split$geometry[3]
st_write(nnwt, file.path(derived, 'studyArea_nnwt_4sims.shp'))

### Jurisidictional boundaries -----
gc()

canPoly <- project(canPoly, studyArea)

wbi.prov.full <- subset(canPoly, canPoly$PREABBR %in% c('Alta.', 'B.C.', 'Man.', 'N.W.T.', 'Sask.', 'Y.T.'))
wbi.prov <- crop(wbi.prov.full, ext(ext)) 

wbi.dus <- crop(wbi.dus.data, ext(ext))

bc <- subset(canPoly, canPoly$PREABBR %in% c('B.C.'))
nwt <- subset(canPoly, canPoly$PREABBR %in% c('N.W.T.', 'Y.T.'))
sk <- subset(canPoly, canPoly$PREABBR %in% c('Sask.'))
mb <- subset(canPoly, canPoly$PREABBR %in% c('Man.'))


# load layers
land <- load_map_layers(landyr = 2019, disturbyr = 2015, ts_else = 40)
gc()
land2010 <- load_map_layers(landyr = 2015, disturbyr = 2010, ts_else = 40)
gc()


ab.range <- project(ab.range, studyArea)
studyArea.ab <- union(studyArea, ab.range)


## Models ----
gc()
sel.2010 <- readRDS(file.path(derived, 'mods_hpc', 'mod_selmove_2010-2015_HPC_noTA.RDS'))

sel.2015 <- readRDS(file.path(derived, 'mods_hpc', 'mod_selmove_2015-2020_HPC_noTA.RDS'))

sel.bc <- readRDS(file.path(derived, 'mod_selmove_bc.RDS'))
sel.mb <- readRDS(file.path(derived, 'mod_selmove_mb_2015_50.RDS'))
sel.sk <- readRDS(file.path(derived, 'mod_selmove_sk.RDS'))
sel.nwt <- readRDS(file.path(derived, 'mod_selmove_nwt.RDS'))


### PDE ----
gc()
mod2010re.tab <- make_betas_tab(sel.2010)

#View(summary(sel.2015)$coef$cond)
mod2015re.tab <- make_betas_tab(sel.2015)

gc()
bc.tab <- make_betas_tab(sel.bc)
mb.tab <- make_betas_tab(sel.mb)
sk.tab <- make_betas_tab(sel.sk)
nwt.tab <- make_betas_tab(sel.nwt)





#### full model PDE functions ----
gc()

make_pde <- function(mod.tab, land, saveName = NULL){
  lf.cov<- (2*as.double(mod.tab[term %like% 'distlf_end', 
                                .(estimate)])*land$log_distlf)
  lfother.cov<- (2*as.double(mod.tab[term %like% 'distlf_other_end', 
                                     .(estimate)])*land$log_distlfother)
  tsf.cov<- (2*as.double(mod.tab[term %like% 'ts_fires_end', 
                                 .(estimate)])*land$log_tsf)
  tsh.cov<- (2*as.double(mod.tab[term %like% 'ts_harv_end', 
                                 .(estimate)])*land$log_tsh)
  needleleaf.cov <- (2*as.double(mod.tab[term %like% 'needleleaf_end', 
                                         .(estimate)])*land$prop_needleleaf)
  veg.cov <- (2*as.double(mod.tab[term %like% 'veg_end', 
                                  .(estimate)])*land$prop_veg)
  mixforest.cov <- (2*as.double(mod.tab[term %like% 'mixforest_end',
                                        .(estimate)])*land$prop_mixforest)
  wets.cov <- (2*as.double(mod.tab[term %like% 'wets_end', 
                                   .(estimate)])*land$prop_wets)
  disturb.cov <- (2*as.double(mod.tab[term %like% 'disturbance_end', 
                                      .(estimate)])*land$disturb)
  
  numerator <- exp(lf.cov + lfother.cov + 
                     tsf.cov + tsh.cov + 
                     needleleaf.cov + veg.cov + 
                     mixforest.cov + 
                     wets.cov +
                     disturb.cov)
  
  
  # the normalizing constant.
  C <- global(numerator, sum, na.rm = T)
  pde <- numerator/C[[1]]
  
  if(!is.null(saveName)){
    writeRaster(pde, 
                file.path(derived, saveName), overwrite = T)
  }
  
  return(pde)
}



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


### 2010 ----

pde.2010 <- make_pde(mod2010re.tab, land2010)

pde.2010.sa <- make_pde_map(pde.2010, studyArea, saveName = 'pde2010_re.tif')
plot(pde.2010.sa, breaks=0:10)


## with ab guestimate
pde.2010.sa.ab <- make_pde_map(pde.2010, studyArea.ab, saveName = 'pde2010_re_ab.tif')
plot(pde.2010.sa.ab, breaks=0:10)

### 2015 ----

pde.2015 <- make_pde(mod2015re.tab, land)

pde.2015.sa <- make_pde_map(pde.2015, studyArea, saveName = 'pde2015_re.tif')
plot(pde.2015.sa, breaks=0:10)


## with ab guestimate
pde.2015.sa.ab <- make_pde_map(pde.2015, studyArea.ab, saveName = 'pde2015_re_ab.tif')
plot(pde.2015.sa.ab, breaks=0:10)


## Jurisidiction model pdes ----
### BC ----
pde.bc <- make_pde(bc.tab, land)
bc.sa <- crop(studyArea, bc)
pde.bc.sa <- make_pde_map(pde.bc, bc.sa, saveName = 'pde_bc.tif')

### MB ----
pde.mb <- make_pde(mb.tab, land)
mb.sa <- crop(studyArea, mb)
pde.mb.sa <- make_pde_map(pde.mb, mb.sa, saveName = 'pde_mb.tif')

### SK ----
pde.sk <- make_pde(sk.tab, land)
sk.sa <- crop(studyArea, sk)
pde.sk.sa <- make_pde_map(pde.sk, sk.sa, saveName = 'pde_sk.tif')

### NWT + YT ----
pde.nwt <- make_pde(nwt.tab, land)
nwt.sa <- crop(studyArea, nwt)
pde.nwt.sa <- make_pde_map(pde.nwt, nwt.sa, saveName = 'pde_nwt.tif')

# plots ----
pde.re.discrete.2010 <- rast(file.path(derived, 'pde2010_re.tif'))
pde.re.discrete.2010.sa.ab <- rast(file.path(derived, 'pde2010_re_ab.tif'))

pde.re.discrete.2015 <- rast(file.path(derived, 'pde2015_re.tif'))
pde.re.discrete.2015.sa.ab <- rast(file.path(derived, 'pde2015_re_ab.tif'))

pde.bc.sa <- rast(file.path(derived, 'pde_bc.tif'))
pde.mb.sa <- rast(file.path(derived, 'pde_mb.tif'))
pde.nwt.sa <- rast(file.path(derived, 'pde_nwt.tif'))
pde.sk.sa <- rast(file.path(derived, 'pde_sk.tif'))

### 2010 plots ----
p.re.2010.wbi<- ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.re.discrete.2010), show.legend = T) +
  geom_spatvector(data = wbi.dus, linewidth = 0.5, color = 'maroon', fill = NA, show.legend = F) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('2010-2015 model') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.re.2010.wbi



### 2015 plots ----
p.re.2015.wbi<- ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.re.discrete.2015), show.legend = T) +
  geom_spatvector(data = wbi.dus, linewidth = 0.5, color = 'maroon', fill = NA, show.legend = F) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('2015-2020 model') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.re.2015.wbi

### Juris plot ----
p.juris <- ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.bc.sa), show.legend = T) +
  geom_spatraster(data = as.numeric(pde.nwt.sa), show.legend = F) +
  geom_spatraster(data = as.numeric(pde.mb.sa), show.legend = F) +
  geom_spatraster(data = as.numeric(pde.sk.sa), show.legend = F) +
  geom_spatvector(data = wbi.dus, linewidth = 0.5, color = 'maroon', fill = NA, show.legend = F) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('Jurisdictional models') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.juris


### Diff plot ----
pde.juris <- mosaic(pde.bc.sa, pde.mb.sa, pde.nwt.sa, pde.sk.sa)

pde.mod.diff <- pde.re.discrete.2015 - pde.juris
range(values(pde.mod.diff), na.rm = T)

p.mod.diff <- ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.mod.diff), show.legend = T) +
  geom_spatvector(data = wbi.dus, linewidth = 0.5, color = 'maroon', fill = NA, show.legend = F) +
  scale_fill_distiller(type = 'div', palette = 'PRGn', na.value = NA) +
  ggtitle('Global 2015 - Jurisdiction models') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Difference in selection') +
  coord_sf(crs = 3978)
p.mod.diff

p.re.2015.wbi + p.juris + p.mod.diff
#################
### BC plot ----
p.bc <- ggplot(bc) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.bc.sa), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('BC') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.bc

pde.bc.global <- crop(crop(pde.re.discrete.2015, pde.bc.sa), bc)
p.bc.global <- ggplot(bc) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.bc.global), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('BC') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.bc.global

### MB plot ----
p.mb <- ggplot(mb) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.mb.sa), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('mb') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.mb

### SK plot ----
p.sk <- ggplot(sk) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.sk.sa), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('sk') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.sk

### NWT plot ----
p.nwt <- ggplot(nwt) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.nwt.sa), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('nwt') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.nwt


#######
##### extrapolate ab ----
p.re.2010.wbi.ab <- ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatvector(data = wbi.dus, linewidth = 0.5, color = 'maroon', fill = NA, show.legend = F) +
  geom_spatraster(data = as.numeric(pde.re.discrete.2010.sa.ab), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('2010-2015 model extrapolating AB') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.re.2010.wbi.ab

p.re.2010.wbi + p.re.2010.wbi.ab

p.re.2015.wbi.ab <- ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.re.discrete.2015.sa.ab), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('2015-2020 model extrapolating AB') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.re.2015.wbi.ab

p.re.2015.wbi + p.re.2015.wbi.ab
###### nwt ----
nwt.sf <- st_as_sf(nwt)
nwt.2010.rast <- raster::raster(nwt.2010)
nwt.2010_df <- as.data.frame(nwt.2010.rast, xy = TRUE) %>%
  mutate(layer = factor(log_distlf))

# ggplot(nwt) +
#   geom_spatvector(fill = NA) +
#   geom_spatraster(data = nwt.2010, show.legend = T) +
ggplot() +
  geom_tile(data = nwt.2010_df, aes(x = x, y=y, fill = layer), show.legend = F) +
  scale_fill_manual(values = mako(10),na.value = NA, breaks = 0:10) +
  geom_spatvector(data = nwt, colour = "black", fill = NA) + 
  ggtitle('2010-2015 model') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)

p.re.2010.nwt<- ggplot(nwt) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(nwt.2010), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('2010-2015 model') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.re.2010.nwt

p.re.2015.nwt<- ggplot(nwt) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(nwt.2015), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  ggtitle('2010-2015 model') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.re.2015.nwt

##### attempt 2 ----
nwt.sf <- st_as_sf(nwt)
nwt.2010.rast <- raster::raster(nwt.2010)
nwt.2010_df <- as.data.frame(nwt.2010.rast, xy = TRUE) %>%
  mutate(layer = factor(log_distlf))
nwt.2015_df <- as.data.frame(raster::raster(nwt.2015), xy = TRUE) %>%
  mutate(layer = factor(log_distlf))
nwt.diff_df <- as.data.frame(raster::raster(nwt.2015-nwt.2010), xy = TRUE) %>%
  mutate(layer = factor(log_distlf))


p.nwt.2010 <- ggplot() +
  geom_tile(data = nwt.2010_df, aes(x = x, y=y, fill = layer), show.legend = F) +
  scale_fill_manual(values = mako(10),na.value = NA, breaks = 0:10) +
  geom_spatvector(data = nwt, colour = "black", fill = NA) + 
  ggtitle('2010-2015 model') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)

p.nwt.2015 <- ggplot() +
  geom_tile(data = nwt.2015_df, aes(x = x, y=y, fill = layer), show.legend = T) +
  scale_fill_manual(values = mako(10),na.value = NA, breaks = 0:10) +
  geom_spatvector(data = nwt, colour = "black", fill = NA) + 
  ggtitle('2015-2020 model') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)

p.nwt.diff <- ggplot() +
  geom_tile(data = nwt.diff_df, aes(x = x, y=y, fill = layer), show.legend = T) +
  scale_fill_manual(values = magma(20),na.value = NA, breaks= -10:10) +
  geom_spatvector(data = nwt, colour = "black", fill = NA) + 
  ggtitle('Change over time') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Change in selection') +
  coord_sf(crs = 3978)
p.nwt.diff

p.nwt.2010 + p.nwt.2015 + p.nwt.diff

wbi.sf <- st_as_sf(wbi.prov)
wbi.2010_df <- as.data.frame(raster::raster(pde.re.discrete.2010*1), xy = TRUE) %>%
  mutate(layer = factor(log_distlf))
wbi.2015_df <- as.data.frame(raster::raster(pde.re.discrete.2015*1), xy = TRUE) %>%
  mutate(layer = factor(log_distlf))
wbi.diff_df <- as.data.frame(raster::raster(pde.re.discrete.2015-pde.re.discrete.2010), xy = TRUE) %>%
  mutate(layer = factor(log_distlf))


p.wbi.2010 <- ggplot() +
  geom_tile(data = wbi.2010_df, aes(x = x, y=y, fill = layer), show.legend = F) +
  scale_fill_manual(values = mako(10),na.value = NA, breaks = 0:10) +
  geom_spatvector(data = wbi.prov, colour = "black", fill = NA) + 
  ggtitle('2010-2015 model') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)

p.wbi.2015 <- ggplot() +
  geom_tile(data = wbi.2015_df, aes(x = x, y=y, fill = layer), show.legend = T) +
  scale_fill_manual(values = mako(10),na.value = NA, breaks = 0:10) +
  geom_spatvector(data = wbi.prov, colour = "black", fill = NA) + 
  ggtitle('2015-2020 model') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)

p.wbi.diff <- ggplot() +
  geom_tile(data = wbi.diff_df, aes(x = x, y=y, fill = layer), show.legend = T) +
  scale_fill_manual(values = magma(20),na.value = NA, breaks= -10:10) +
  geom_spatvector(data = wbi.prov, colour = "black", fill = NA) + 
  ggtitle('Change over time') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Change in selection') +
  coord_sf(crs = 3978)
p.wbi.diff

p.wbi.2010 + p.wbi.2015 + p.wbi.diff

#######

p.2015 <- gplot(pde.discrete.2015) +
  geom_tile(aes(fill = value), show.legend = T) +
  #geom_sf(data = st_as_sf(wbi.prov), fill = NA) +
  ggtitle('2015-2020 Jurisdictional model') +
  scale_fill_gradientn(colours = mako(10),na.value = "white", limits = c(0,10)) +
  theme_bw() +
  labs(fill = 'Intensity of use') +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  coord_equal()
p.2015

p.2015.ab <- gplot(pde.discrete.2015.sa.ab) +
  geom_tile(aes(fill = value), show.legend = T) +
  ggtitle('2015-2020  Jurisdictional model extrapolating AB') +
  scale_fill_gradientn(colours = mako(10),na.value = "white", limits = c(0,10)) +
  theme_bw() +
  labs(fill = 'Intensity of use') +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  coord_equal()
p.2015.ab


p.simp.2015 <- gplot(pde.simp.discrete.2015) +
  geom_tile(aes(fill = value), show.legend = T) +
  ggtitle('2015-2020 nested jurisdictional model') +
  scale_fill_gradientn(colours = mako(10),na.value = "white", limits = c(0,10)) +
  theme_bw() +
  labs(fill = 'Intensity of use') +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  coord_equal()
p.simp.2015


p.simp.2015.ab <- gplot(pde.simp.discrete.2015.sa.ab) +
  #geom_spatvector(data = wbi.prov, fill = NA) + 
  geom_tile(aes(fill = value), show.legend = T) +
  ggtitle('2015-2020 nested jurisdictional model') +
  scale_fill_gradientn(colours = mako(10),na.value = "white", limits = c(0,10)) +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of use') +
  # coord_equal() +
  coord_sf(crs = 3978)

p.simp.2015.ab
plot(pde.simp.discrete.2015.sa.ab, breaks=0:10)


p.re.2010 <- gplot(pde.re.discrete.2010) +
  geom_tile(aes(fill = value), show.legend = F) +
  ggtitle('2010-2015  model') +
  scale_fill_gradientn(colours = mako(10),na.value = "white", limits = c(0,10)) +
  theme_bw() +
  labs(fill = 'Intensity of use') +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  coord_equal()
p.re.2010

p.re.2015 <- gplot(pde.re.discrete.2015) +
  geom_tile(aes(fill = value), show.legend = T) +
  ggtitle('2015-2020  model') +
  scale_fill_gradientn(colours = mako(10),na.value = "white", limits = c(0,10)) +
  theme_bw() +
  labs(fill = 'Intensity of use') +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  coord_equal()
p.re.2015

p.re.diff <- gplot(pde.re.discrete.2015 - pde.re.discrete.2010) +
  geom_tile(aes(fill = value), show.legend = T) +
  ggtitle('Change over time') +
  scale_fill_gradientn(colours = magma(10),na.value = "white") +
  theme_bw() +
  labs(fill = 'Change in intensity') +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  coord_equal()
p.re.diff

p.re.2010 + p.re.2015 + p.re.diff

p.nwt.2010 <- gplot(nwt.2010) +
  geom_tile(aes(fill = value), show.legend = F) +
  ggtitle('2010-2015  model') +
  scale_fill_gradientn(colours = mako(10),na.value = "white", limits = c(0,10)) +
  theme_bw() +
  labs(fill = 'Intensity of use') +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  coord_equal()
p.nwt.2010

p.nwt.2015 <- gplot(nwt.2015) +
  geom_tile(aes(fill = value), show.legend = T) +
  ggtitle('2015-2020  model') +
  scale_fill_gradientn(colours = mako(10),na.value = "white", limits = c(0,10)) +
  theme_bw() +
  labs(fill = 'Intensity of use') +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  coord_equal()
p.nwt.2015

p.nwt.diff <- gplot(nwt.2015 - nwt.2010) +
  geom_tile(aes(fill = value), show.legend = T) +
  ggtitle('Change over time') +
  scale_fill_gradientn(colours = magma(10),na.value = "white") +
  theme_bw() +
  labs(fill = 'Change in intensity') +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  coord_equal()
p.nwt.diff

p.nwt.2010 + p.nwt.2015 + p.nwt.diff

p.re.2015.ab <- gplot(pde.re.discrete.2015.sa.ab) +
  #geom_spatvector(data = wbi.prov, fill = NA) + 
  geom_tile(aes(fill = value), show.legend = T) +
  ggtitle('2015-2020  model') +
  scale_fill_gradientn(colours = mako(10),na.value = "white", limits = c(0,10)) +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of use') +
  # coord_equal() +
  coord_sf(crs = 3978)

p.re.2015.ab


p.2015.ab + p.simp.2015.ab #+ p.re.2015.ab

####
# p.2015.wbi<- ggplot(wbi.prov) +
#   geom_spatvector(fill = NA) +
#   geom_spatraster(data = as.numeric(pde.discrete.2015.sa), show.legend = T) +
#   scale_fill_viridis(na.value = NA) +
#   ggtitle('2015-2020 model') +
#   theme_bw() +
#   theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
#   theme_void() +
#   labs(fill = 'Intensity of use') +
#   coord_sf(crs = 3978)
# p.2015.wbi

p.2015.wbi.ab <- ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.discrete.2015.sa.ab), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  #scale_fill_viridis(na.value = NA) +
  ggtitle('2015-2020 fixed model extrapolating AB') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.2015.wbi.ab


p.simp.2015.wbi<- ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.simp.discrete.2015), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  #scale_fill_viridis(na.value = NA) +
  ggtitle('2015-2020 jurisdictional model') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.simp.2015.wbi

p.simp.2015.wbi.ab <- ggplot(wbi.prov) +
  geom_spatvector(fill = NA) +
  geom_spatraster(data = as.numeric(pde.simp.discrete.2015.sa.ab), show.legend = T) +
  scale_fill_gradientn(colours = mako(10),na.value = NA, limits = c(0,10)) +
  #scale_fill_viridis(na.value = NA) +
  ggtitle('2015-2020 jurisdictional model extrapolating AB') +
  theme_bw() +
  theme(plot.title=element_text(size=12,hjust = 0.05),axis.title = element_blank()) +
  theme_void() +
  labs(fill = 'Intensity of selection') +
  coord_sf(crs = 3978)
p.simp.2015.wbi.ab


p.simp.2015.wbi + p.simp.2015.wbi.ab