#' @title make random steps
#' @export
#' @author Julie W. Turner
#' 
make_random_steps <- function(DT, sl.distr = NULL, ta.distr = NULL) {
  if (is.null(DT)) return()
  if (nrow(DT) == 0) return()

  if (!is.null(sl.distr)) {
    rands <- random_steps(DT, n = 10, sl_distr = sl.distr, ta_distr = ta.distr)  
  } 
  if (is.null(sl.distr)){
    rands <- random_steps(DT, n = 10) 
  }
  
  return(rands)
  # %>%
  #   time_of_day(where = 'start', include.crepuscule = F)
}