#' @title calculate distribution parameters
#' @export
#' @author Julie W. Turner
#' 
calc_distribution_parameters <- function(steps) {
  if (is.null(steps)) return()
  tas <- ta_distr_params(steps)
  sls <- sl_distr_params(steps)
  data.table(cbind(setDT(tas), setDT(sls), id = unique(steps$id)))
}
