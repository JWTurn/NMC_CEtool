#' crop points within study areal
#' @author Julie W. Turner
#' 
#' @param DT 
#' @param studyArea 
#' @param crs 
#'
#' @return
#' @export
#'

crop_pts <- function(DT, studyArea, crs) {
  sa.ext <- sf::st_read(studyArea)
  
  dat.sf <- st_as_sf(DT, coords = c('x_', 'y_'),
                     crs = crs)
  
  dat.crop <- st_crop(dat.sf, sa.ext)
  
  DT.filter <- setDT(sfheaders::sf_to_df(dat.crop, fill = T))
  setnames(DT.filter, old = c('x', 'y'), new = c('x_', 'y_'))
  return(DT.filter)
}