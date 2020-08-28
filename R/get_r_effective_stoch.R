#' Get R effective for stochastic model
#'
#' @template dqc
#' @param n_inf Number of infections (or effective population size of infected individuals)
#' @param ... Optional parameters to pass to [`get_infect_mat()`].
#'
#' @return Numeric value indicating the R effective given the
#'   Detected-Quarantine-Community proportions supplied.
#' @export
get_r_effective_stoch <- function(dqc, n_inf, ...) {
  dqc <- check_dqc(dqc)
  int_inf <- get_intermediate_inf(dqc, n_inf, ...)
  
  sum(int_inf)/n_inf
}

