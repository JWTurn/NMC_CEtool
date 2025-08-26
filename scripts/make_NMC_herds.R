require(sf)
require(data.table)
require(terra)


### Input data ----
raw <- file.path('data', 'raw')
derived <- file.path('data', 'derived')


bcHerds <- st_read(file.path(raw, 'NMC_herds', '2023_NorthMtnCaribouBC_comb.shp'))
ytHerds <- st_read(file.path(raw, 'NMC_herds', 'YukonCaribouHerds_7May2024', 'YukonCaribouHerds_7May2024.shp'))

plot(ytHerds$geometry)
summary(as.factor(ytHerds$Type))
ytHerdsVect <- vect(ytHerds)
ytNMCherds <- subset(ytHerdsVect, ytHerdsVect$Type == 'Woodland')

bc.proj <-project(vect(bcHerds), ytNMCherds)
nmc <- rbind(ytNMCherds, bc.proj)
# ytNMCherds <- ytHerds |>
#   filter(Type == 'Woodland')

writeVector(nmc, file.path(raw, 'NMC_herds', 'nmc_herds_updated.shp'))


