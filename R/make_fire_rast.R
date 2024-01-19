#' @title make rasters of most recent fire by year
#' @export
#' @author Julie W. Turner
#' 
make_fire_rast <- function(layer, rast, minyr = NULL, where2save){
  lyr <- vect(layer)
  raster <- rast(rast)
  yearsWithData <- unique(lyr[["YEAR"]])$YEAR
  if(!is.null(minyr)){
    yearsWithData <- minyr:max(yearsWithData, na.rm = T)
  }
  
  lapply(yearsWithData, function(yr) {
    subst <- subset(lyr, lyr$YEAR<= yr)
    print(paste0('start ', yr))
    #fires <- 
      suppressWarnings({
        historicFireRasters <- rasterize(subst, raster, field="YEAR", fun=max)
      })
      fnames <- paste0("fires_", yr, ".tif")
      writeRaster(historicFireRasters, file.path(where2save, fnames), overwrite = T)
      print(paste0('saved ', yr))
  })
}
