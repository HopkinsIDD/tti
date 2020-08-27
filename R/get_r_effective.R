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
  get_r_effective_base(pqc, infect_mat)
}

#' Get R Effective
#'
#' This low-level function calculates R effective from a PQC vector and
#' infection matrix
#'
#' @param pqc Values for PQC compartments. All elements of this vector
#'   should sum to 1.
#' @param infect Matrix. The infection matrix.
#' @export
get_r_effective_base <- function(pqc, infect) {
  sum(pqc %*% infect)
}
