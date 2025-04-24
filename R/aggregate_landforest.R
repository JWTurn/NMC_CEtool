#' @title make grid proportion layers from yearly CA_forest
#' @export
#' @author Julie W. Turner


aggregate_landforest <- function(targetFile, trast, where2save = NULL){
  
    
    landRaw <- terra::rast(targetFile)
    landCrop <- postProcess(landRaw, cropTo=terra::buffer(trast, width = 10000))
   
    
    land.seg <- terra::segregate(landCrop) #creates 1 file with diff layers, then focal on whole thing
    
    land.agg <- postProcess(landRaw, to = trast, method = 'average')
    
    
    names(propLand) <- c('prop_water', 'prop_snow', 'prop_rock', 'prop_barrenland', 'prop_bryoids', 
                         'prop_shrub', 'prop_wetland', 'prop_wet_treed', 'prop_herbs', 
                         'prop_needleleaf', 'prop_deciduous', 'prop_mixed')
    if(!is.null(where2save)){
      writeRaster(land.agg, where2save)
    }
    return(land.agg)
  
  
  }
