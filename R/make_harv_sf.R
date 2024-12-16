#' @title make harv raster into vector
#' @export
#' @author Julie W. Turner
#' 
make_harv_sf <- function(path){
  poly <- as.polygons(rast(path))
  poly.sf <- st_as_sf(poly)
  names(poly.sf)[1] <- "year"
  return(poly.sf)
}
  