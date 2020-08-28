#' Get R effective for stochastic model
#'
#' @template dqc
#' @param n_inf Number of infections (or effective population size of infected 
#'   individuals). Default 1000.
#' @param ... Optional parameters to pass to [`get_intermediate_inf()`].
#'
#' @return Numeric value indicating the R effective given the
#'   Detected-Quarantine-Community proportions supplied.
#' @export
get_r_effective_stoch <- function(dqc, n_inf=1000, ...) {
  dqc <- check_dqc(dqc)
  int_inf <- get_intermediate_inf(dqc, n_inf, ...)
  
  sum(int_inf)/n_inf
}

