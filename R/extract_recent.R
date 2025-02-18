#' @title extract recent 
#' @export
#' @author Julie W. Turner
#' 
#' time_recent is what equates to recent
#' assumes filtering on a variable called year
#' 
extract_recent <- function(DT, var, name, method = 'distto', where){ #time_recent,
  object_name <- name
  
  
  coords_start  <-  c('x1_', 'y1_')
  coords_end  <-  c('x2_', 'y2_')
  
  if(method=='distto'){
    if(where== 'start'){
      DT[!is.na(x1_)&!is.na(y1_), paste0('dist_', object_name, '_start') := distance_to(st_as_sf(.SD, coords = coords_start, crs = crs),
                                                                                        filter(var, abs(year - .BY[[1]]) == min(abs(year - .BY[[1]])))),
         by = year]
    }
    
    if(where== 'end'){
      DT[!is.na(x2_)&!is.na(y2_), paste0('dist_', object_name, '_end') := distance_to(st_as_sf(.SD, coords = coords_end, crs = crs),
                                                                                      filter(var, abs(year - .BY[[1]]) == min(abs(year - .BY[[1]])))),
         by = year]
    }
    
    if(where== 'both'){
      DT[!is.na(x1_)&!is.na(y1_), paste0('dist_', object_name, '_start') := distance_to(st_as_sf(.SD, coords = coords_start, crs = crs),
                                                                                        filter(var, abs(year - .BY[[1]]) == min(abs(year - .BY[[1]])))),
         by = year]
      DT[!is.na(x2_)&!is.na(y2_), paste0('dist_', object_name, '_end') := distance_to(st_as_sf(.SD, coords = coords_end, crs = crs),
                                                                                      filter(var, abs(year - .BY[[1]]) == min(abs(year - .BY[[1]])))),
         by = year]
    }
    # if(where== 'start'){
    #   DT[!is.na(x1_)&!is.na(y1_), paste0('dist_', object_name, '_start') := distance_to(st_as_sf(.SD, coords = coords_start, crs = crs),
    #                                                           filter(var, year %in% .BY[[1]]:(.BY[[1]]+time_recent))),
    #      by = year]
    # }
    # 
    # if(where== 'end'){
    #   DT[!is.na(x2_)&!is.na(y2_), paste0('dist_', object_name, '_end') := distance_to(st_as_sf(.SD, coords = coords_end, crs = crs),
    #                                                           filter(var, year %in% .BY[[1]]:(.BY[[1]]+time_recent))),
    #      by = year]
    # }
    # 
    # if(where== 'both'){
    #   DT[!is.na(x1_)&!is.na(y1_), paste0('dist_', object_name, '_start') := distance_to(st_as_sf(.SD, coords = coords_start, crs = crs),
    #                                                             filter(var, year %in% .BY[[1]]:(.BY[[1]]+time_recent))),
    #      by = year]
    #   DT[!is.na(x2_)&!is.na(y2_), paste0('dist_', object_name, '_end') := distance_to(st_as_sf(.SD, coords = coords_end, crs = crs),
    #                                                           filter(var, year %in% .BY[[1]]:(.BY[[1]]+time_recent))),
    #      by = year]
    # }

  }
 return(DT) 
}