#' Get R effective
#'
#' @template pqc
#' @param ... Optional parameters to pass to [`get_infect_mat()`].
#'
#' @return Numeric value indicating the R effective given the
#'   Passive-Quarantine-Community proportions supplied.
#' @export
get_r_effective <- function(pqc, ...) {
  pqc <- check_pqc(pqc)

  infect_mat <- get_infect_mat(...)
  sum(pqc %*% infect_mat)
}
