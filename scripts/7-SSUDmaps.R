# making pde SSUD maps

require(reproducible)
require(terra)
require(glmmTMB)
require(data.table)

# using a folder where I already saved the NTEMS files so not eating up space
canada <- file.path('C:','Users', 'julie', 'Documents', 'git', 'reproducibleInputs')

dataDrive <- file.path('G:', 'Shared drives', 'WSC_Yukon', 'CEtool_component3', 'data_models_new', 'Fine_Models', 'data')

# pick year of layers need, can make a loop/Map to go over the years
yr =2020

### 250m ----
# make list of all paths Landcover
# the names need to match the names of variables in the model 
pathLand250 <- list(prop_water_250 = file.path(dataDrive, 'derived', 'prop_land', 'prop_land_split', "prop_water_250"),
                    prop_snow_250 = file.path(dataDrive, 'derived', 'prop_land', 'prop_land_split', "prop_snow_250"),
                    prop_rock_250 = file.path(dataDrive, 'derived', 'prop_land', 'prop_land_split', 'prop_rock_250'),
                    prop_barren_250 = file.path(dataDrive, 'derived', 'prop_land', 'prop_land_split', 'prop_barrenland_250'),
                    prop_bryoids_250 = file.path(dataDrive, 'derived', 'prop_land', 'prop_land_split', 'prop_bryoids_250'),
                    prop_shrub_250 = file.path(dataDrive, 'derived', 'prop_land', 'prop_land_split', 'prop_shrub_250'),
                    prop_wetland_250 = file.path(dataDrive, 'derived', 'prop_land', 'prop_land_split', 'prop_wetland_250'),
                    prop_wettreed_250 = file.path(dataDrive, 'derived', 'prop_land', 'prop_land_split', 'prop_wet_treed_250'),
                    prop_herbs_250 = file.path(dataDrive, 'derived', 'prop_land', 'prop_land_split', 'prop_herbs_250'),
                    prop_needleleaf_250 = file.path(dataDrive, 'derived', 'prop_land', 'prop_land_split', 'prop_needleleaf_250'),
                    prop_deciduous_250 = file.path(dataDrive, 'derived', 'prop_land', 'prop_land_split', 'prop_deciduous_250'),
                    prop_mixed_250 = file.path(dataDrive, 'derived', 'prop_land', 'prop_land_split', 'prop_mixed_250'),
                    
                    
                    #Fires 250
                    fires_250 = file.path(dataDrive, 'derived', 'fireRasters250m', 'fires'),
                    
                    #Harvest 250
                    harv250 = file.path(dataDrive, 'derived', 'harv250', 'harv250'),
                    harv_dist_250 = file.path(dataDrive, 'derived', 'harv_dist_250m', 'harvdist_250m'),
                    harv_pa_250 = file.path(dataDrive, 'derived', 'harv_pa_250m', 'harvpres_250m'),
                    harv_dens_250 = file.path(dataDrive, 'derived', 'harv_dens_250m', 'harvdens_250m'),
                    
                    #Forest age 250
                    for_age_250 = file.path(dataDrive, 'raw', 'age250', 'forest_age'),
                    
                    #Anthro-disturbance 250
                    dens_lfother_250 = file.path(dataDrive, 'derived', 'dens_disturb_250m', 'otherlf_dens_250m'),
                    dens_paved_250 = file.path(dataDrive, 'derived', 'dens_disturb_250m', 'paved_dens_250m'),
                    dens_unpaved_250 = file.path(dataDrive, 'derived', 'dens_disturb_250m', 'unpaved_dens_250m'),
                    dens_placer_250 = file.path(dataDrive, 'derived', 'dens_disturb_250m', 'placer_dens_250m'),
                    dens_quartz_250 = file.path(dataDrive, 'derived', 'dens_disturb_250m', 'quartz_dens_250m'),
                    dens_otherpoly_250 = file.path(dataDrive, 'derived', 'dens_disturb_250m', 'otherpoly_dens_250m'),
                    dens_othermines_250 = file.path(dataDrive, 'derived', 'dens_disturb_250m', 'othermines_dens_250m'),
                    dist_paved_250 = file.path(dataDrive, 'derived', 'dist_disturb_250m', 'paved_distance_250m'),
                    dist_unpaved_250 = file.path(dataDrive, 'derived', 'dist_disturb_250m', 'unpaved_distance_250m'),
                    dist_lfother_250 = file.path(dataDrive, 'derived', 'dist_disturb_250m', 'otherlf_distance_250m'),
                    dist_placer_250 = file.path(dataDrive, 'derived', 'dist_disturb_250m', 'placer_distance_250m'),
                    dist_quartz_250 = file.path(dataDrive, 'derived', 'dist_disturb_250m', 'quartz_distance_250m'),
                    dist_othermines_250 = file.path(dataDrive, 'derived', 'dist_disturb_250m', 'othermines_distance_250m'),
                    dist_otherpoly_250 = file.path(dataDrive, 'derived', 'dist_disturb_250m', 'otherpoly_distance_250m'),
                    pa_paved_250 = file.path(dataDrive, 'derived', 'pa_disturb_250m', 'paved_pa_250m'),
                    pa_unpaved_250 = file.path(dataDrive, 'derived', 'pa_disturb_250m', 'unpaved_pa_250m'),
                    pa_lfother_250 = file.path(dataDrive, 'derived', 'pa_disturb_250m', 'otherlf_pa_250m'),
                    pa_placer_250 = file.path(dataDrive, 'derived', 'pa_disturb_250m', 'placer_pa_250m'),
                    pa_quartz_250 = file.path(dataDrive, 'derived', 'pa_disturb_250m', 'quartz_pa_250m'),
                    pa_othermines_250 = file.path(dataDrive, 'derived', 'pa_disturb_250m', 'othermines_pa_250m'),
                    pa_otherpoly_250 = file.path(dataDrive, 'derived', 'pa_disturb_250m', 'otherpoly_pa_250m'))

# reat in all the paths to make a list of rasters
rastLand250 <- list()
for(i in names(pathLand250)){
  rastLand250 <- c(rastLand250, rast(paste0(pathLand250[[i]], '_', yr, '.tif')))
}
names(rastLand250) <- names(pathLand250)

# add rasters we created to improve availability and test models
rastLand250 <- append(rastLand250, list(ts_harv250 = (yr - rastLand250$harv250)))
rastLand250[['ts_harv250']][is.na(rastLand250[['ts_harv250']])] <- 100
rastLand250[['ts_harv250']] <- mask(rastLand250[['ts_harv250']], rastLand250[[1]])

rastLand250 <- append(rastLand250, list(ts_fires_250 = (yr - rastLand250$fires_250)))
rastLand250[['ts_fires_250']][is.na(rastLand250[['ts_fires_250']])] <- 200
rastLand250[['ts_fires_250']] <- mask(rastLand250[['ts_fires_250']], rastLand250[[1]])

rastLand250 <- append(rastLand250, list(prop_forest_250 = rastLand250$prop_wettreed_250 + rastLand250$prop_needleleaf_250 + 
                                          rastLand250$prop_deciduous_250 + rastLand250$prop_mixed_250))

rastLand250 <- append(rastLand250, list(prop_veg_250 = rastLand250$prop_bryoids_250 + rastLand250$prop_herbs_250 + rastLand250$prop_shrub_250))

rastLand250 <- append(rastLand250, list(dist_lf_250 = min(c(rastLand250$dist_lfother_250, rastLand250$dist_paved_250, rastLand250$dist_unpaved_250), na.rm = T)))

rastLand250 <- append(rastLand250, list(dist_poly_250 = min(c(rastLand250$dist_placer_250, rastLand250$dist_quartz_250, rastLand250$harv_dist_250, rastLand250$dist_othermines_250, rastLand250$dist_otherpoly), na.rm = T)))

### 1000 m ----
# do same as above, but there are slightly different rasters at this resolution
pathLand1000 <- list(prop_water_1000 = file.path(dataDrive, 'derived', 'prop_land', 'prop_land_split', "prop_water_1000"),
                     prop_snow_1000 = file.path(dataDrive, 'derived', 'prop_land', 'prop_land_split', "prop_snow_1000"),
                     prop_rock_1000 = file.path(dataDrive, 'derived', 'prop_land', 'prop_land_split', 'prop_rock_1000'),
                     prop_barren_1000 = file.path(dataDrive, 'derived', 'prop_land', 'prop_land_split', 'prop_barrenland_1000'),
                     prop_bryoids_1000 = file.path(dataDrive, 'derived', 'prop_land', 'prop_land_split', 'prop_bryoids_1000'),
                     prop_shrub_1000 = file.path(dataDrive, 'derived', 'prop_land', 'prop_land_split', 'prop_shrub_1000'),
                     prop_wetland_1000 = file.path(dataDrive, 'derived', 'prop_land', 'prop_land_split', 'prop_wetland_1000'),
                     prop_wettreed_1000 = file.path(dataDrive, 'derived', 'prop_land', 'prop_land_split', 'prop_wet_treed_1000'),
                     prop_herbs_1000 = file.path(dataDrive, 'derived', 'prop_land', 'prop_land_split', 'prop_herbs_1000'),
                     prop_needleleaf_1000 = file.path(dataDrive, 'derived', 'prop_land', 'prop_land_split', 'prop_needleleaf_1000'),
                     prop_deciduous_1000 = file.path(dataDrive, 'derived', 'prop_land', 'prop_land_split', 'prop_deciduous_1000'),
                     prop_mixed_1000 = file.path(dataDrive, 'derived', 'prop_land', 'prop_land_split', 'prop_mixed_1000'),
                     
                     
                     #Fires 1000
                     fires_1000 = file.path(dataDrive, 'derived', 'fireRasters1000m', 'fires'),
                     fire_60perc1000 = file.path(dataDrive, 'derived', 'perc_burned_1000', 'burned60_1000m'),
                     fire_2perc1000 = file.path(dataDrive, 'derived', 'perc_burned_1000', 'burned2_1000m'),
                     
                     #Harvest 1000
                     harv1000 = file.path(dataDrive, 'derived', 'harv1000', 'harv1000'),
                     harv_dist_1000 = file.path(dataDrive, 'derived', 'harv_dist_1000m', 'harvdist_1000m'),
                     harv_pa_1000 = file.path(dataDrive, 'derived', 'harv_pa_1000m', 'harvpres_1000m'),
                     harv_dens_1000 = file.path(dataDrive, 'derived', 'harv_dens_1000m', 'harvdens_1000m'),
                     
                     #Forest age 1000
                     for_age_1000 = file.path(dataDrive, 'raw', 'age1000', 'forest_age'),
                     
                     #Anthro-disturbance 1000
                     dens_lfother_1000 = file.path(dataDrive, 'derived', 'dens_disturb_1000m', 'otherlf_dens_1000m'),
                     dens_paved_1000 = file.path(dataDrive, 'derived', 'dens_disturb_1000m', 'paved_dens_1000m'),
                     dens_unpaved_1000 = file.path(dataDrive, 'derived', 'dens_disturb_1000m', 'unpaved_dens_1000m'),
                     dens_placer_1000 = file.path(dataDrive, 'derived', 'dens_disturb_1000m', 'placer_dens_1000m'),
                     dens_quartz_1000 = file.path(dataDrive, 'derived', 'dens_disturb_1000m', 'quartz_dens_1000m'),
                     dens_otherpoly_1000 = file.path(dataDrive, 'derived', 'dens_disturb_1000m', 'otherpoly_dens_1000m'),
                     dens_othermines_1000 = file.path(dataDrive, 'derived', 'dens_disturb_1000m', 'othermines_dens_1000m'),
                     dist_paved_1000 = file.path(dataDrive, 'derived', 'dist_disturb_1000m', 'paved_distance_1000m'),
                     dist_unpaved_1000 = file.path(dataDrive, 'derived', 'dist_disturb_1000m', 'unpaved_distance_1000m'),
                     dist_lfother_1000 = file.path(dataDrive, 'derived', 'dist_disturb_1000m', 'otherlf_distance_1000m'),
                     dist_placer_1000 = file.path(dataDrive, 'derived', 'dist_disturb_1000m', 'placer_distance_1000m'),
                     dist_quartz_1000 = file.path(dataDrive, 'derived', 'dist_disturb_1000m', 'quartz_distance_1000m'),
                     dist_othermines_1000 = file.path(dataDrive, 'derived', 'dist_disturb_1000m', 'othermines_distance_1000m'),
                     dist_otherpoly_1000 = file.path(dataDrive, 'derived', 'dist_disturb_1000m', 'otherpoly_distance_1000m'),
                     pa_paved_1000 = file.path(dataDrive, 'derived', 'pa_disturb_1000m', 'paved_pa_1000m'),
                     pa_unpaved_1000 = file.path(dataDrive, 'derived', 'pa_disturb_1000m', 'unpaved_pa_1000m'),
                     pa_lfother_1000 = file.path(dataDrive, 'derived', 'pa_disturb_1000m', 'otherlf_pa_1000m'),
                     pa_placer_1000 = file.path(dataDrive, 'derived', 'pa_disturb_1000m', 'placer_pa_1000m'),
                     pa_quartz_1000 = file.path(dataDrive, 'derived', 'pa_disturb_1000m', 'quartz_pa_1000m'),
                     pa_othermines_1000 = file.path(dataDrive, 'derived', 'pa_disturb_1000m', 'othermines_pa_1000m'),
                     pa_otherpoly_1000 = file.path(dataDrive, 'derived', 'pa_disturb_1000m', 'otherpoly_pa_1000m'))

# make a list of rasters
rastLand1000 <- list()
for(i in names(pathLand1000)){
  rastLand1000 <- c(rastLand1000, rast(paste0(pathLand1000[[i]], '_', yr, '.tif')))
}
names(rastLand1000) <- names(pathLand1000)

# calc the other rasters
rastLand1000 <- append(rastLand1000, list(ts_harv1000 = (yr - rastLand1000$harv1000)))
rastLand1000[['ts_harv1000']][is.na(rastLand1000[['ts_harv1000']])] <- 100
rastLand1000[['ts_harv1000']] <- mask(rastLand1000[['ts_harv1000']], rastLand1000[[1]])

rastLand1000 <- append(rastLand1000, list(ts_fires_1000 = (yr - rastLand1000$fires_1000)))
rastLand1000[['ts_fires_1000']][is.na(rastLand1000[['ts_fires_1000']])] <- 200
rastLand1000[['ts_fires_1000']] <- mask(rastLand1000[['ts_fires_1000']], rastLand1000[[1]])

rastLand1000 <- append(rastLand1000, list(prop_forest_1000 = rastLand1000$prop_wettreed_1000 + rastLand1000$prop_needleleaf_1000 + 
                                            rastLand1000$prop_deciduous_1000 + rastLand1000$prop_mixed_1000))

rastLand1000 <- append(rastLand1000, list(prop_veg_1000 = rastLand1000$prop_bryoids_1000 + rastLand1000$prop_herbs_1000 + rastLand1000$prop_shrub_1000))

rastLand1000 <- append(rastLand1000, list(dist_lf_1000 = min(c(rastLand1000$dist_lfother_1000, rastLand1000$dist_paved_1000, rastLand1000$dist_unpaved_1000), na.rm = T)))

rastLand1000 <- append(rastLand1000, list(dist_poly_1000 = min(c(rastLand1000$dist_placer_1000, rastLand1000$dist_quartz_1000, rastLand1000$harv_dist_1000, rastLand1000$dist_othermines_1000, rastLand1000$dist_otherpoly), na.rm = T)))


# rasterize coarse rasters to matching resolution as fine for math
rastLand1000fine <- lapply(rastLand1000, function(x){
  postProcess(x, to = rastLand250[[1]])
})

# combine coarse and fine lists
rastLand2020.fine <- append(rastLand250, rastLand1000fine)

### model ----
## prepping a study area for mapping, filling some holes within polygon
# TODO need to remove Fort St. John erroneous blob
# cleanDat <- prepInputs(url = "https://drive.google.com/file/d/1s4iXZgu1_Ih5W3PQ0mgG4G8TaPmScC1E/view?usp=share_link",
#                   targetFile = "clean_NMC.RDS",
#                   destinationPath = tempdir(),
#                   fun = "readRDS")
# 
# require(sf)
# 
# outcrs <- st_crs(3154)
# coords.final <- cleanDat %>% st_as_sf(coords = c('x_','y_'))%>%
#   st_set_crs(outcrs)
# plot(coords.final$geometry)
# #st_write(coords.final, file.path(derived, "prepped", 'clean_NMC.shp'), append = F)
# 
# ### 20 km buffer around points to get an idea of extent of study area
# studyArea <- st_buffer(coords.final, dist = 20000)
# sa.union <- st_union(studyArea)
# sa.union
# plot(sa.union)
# studyArea.ch <- st_convex_hull(studyArea)
# plot(studyArea.ch$geometry)
# studyArea.ch.u <- st_union(studyArea.ch)
# plot(studyArea.ch.u)
# 
# studyArea.fill <- nngeo::st_remove_holes(studyArea.ch.u)
# st_write(studyArea.fill, file.path(dataDrive, 'studyArea_map.shp'))

# study area for maps

studyArea.fill <- vect(file.path(dataDrive, 'studyArea_map.shp'))

# load mod
mod.fine <- prepInputs(url = "https://drive.google.com/file/d/1mOyoqH6Gj8SpMMNguTF-SxqI9GW_qEnc/view?usp=share_link",
                       targetFile = "m.simple15step.RDS",
                       destinationPath = tempdir(),
                       fun = "readRDS")


pde.fine <- make_pde(mod = mod.fine, lsRasters = rastLand2020.fine, studyArea = studyArea.fill)
plot(pde.fine)

writeRaster(pde.fine, file.path(dataDrive, 'pde_fine.tif'))

gc()

mod.coarse <- prepInputs(url = "https://drive.google.com/file/d/1DhxS_EhcmsrH77HFJ_1CreApRzCn7BWd/view?usp=share_link",
                         targetFile = "m.densfires6020step.RDS",
                         destinationPath = tempdir(),
                         fun = "readRDS")
#fixef(mod.coarse)
pde.coarse <- make_pde(mod = mod.coarse, lsRasters = rastLand2020.fine, studyArea = studyArea.fill)
plot(pde.coarse)

writeRaster(pde.coarse, file.path(dataDrive, 'pde_coarse.tif'))

# TODO need to rescale to 1
pde.2scale <- pde.fine + pde.coarse
plot(pde.2scale)

