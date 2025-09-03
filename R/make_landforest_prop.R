#' @title make moving window proportion layers from yearly CA_forest
#' @export
#' @author Julie W. Turner
#' 
make_landforest_prop <- function(targetFile, trast, buff, where2save = NULL){

  # TODO update this to not need aggregate_landforest.R function also
  landRaw <- terra::rast(targetFile)
  landCrop <- postProcess(landRaw, cropTo=terra::buffer(trast, width = 10000))
  
  # TODO make an argument/table so can use with diff datasets
  lccClasses <- c(20, 31, 32, 33, 40, 50, 80, 81, 100, 210, 220, 230)
  
  land.seg <- terra::segregate(landCrop, classes = lccClasses) #creates 1 file with diff layers, then focal on whole thing
  
  message('landcover segregated')
  
  land.agg <- postProcess(land.seg, to = trast, method = 'average')
   # What to buffer for proportion of landclasses
  message('landcover aggregated to trast')

    ## This creates an object which can be used to make a layer of specified diameter
    # The d value is what determines the buffer size if you want to change it.
    ## If you're doing multiple landcover classes, you only need to run this line once, as long as each of the habitat variables has the same resolution
    Buff <- focalMat(land.agg, d=buff, type = 'circle')
    ## This generates a new raster where each cell corresponds to the mean wetland within the buffer.
    # Since it's all 1s and 0s, this is the same as the proportion of wetland surrounding the focal variable
    
    # set classes to all those that aren't no data (0)
    
    propLand <- focal(land.agg, Buff, na.rm = TRUE, pad = TRUE, padValue = 0)
    
    
    names(propLand) <- c('prop_water', 'prop_snow', 'prop_rock', 'prop_barrenland', 'prop_bryoids', 
                         'prop_shrub', 'prop_wetland', 'prop_wet_treed', 'prop_herbs', 
                         'prop_needleleaf', 'prop_deciduous', 'prop_mixed')
    message('moving window calculated')
    if(!is.null(where2save)){
      writeRaster(propLand, where2save, overwrite = T)
      message('raster saved')
    }
    
    return(propLand)
    

}