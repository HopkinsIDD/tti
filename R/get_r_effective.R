#' Get R effective
#'
#' @template dqc
#' @param ... Optional parameters to pass to [`get_infect_mat()`].
#'
#' @return Numeric value indicating the R effective given the
#'   Detected-Quarantine-Community proportions supplied.
#' @export
get_r_effective <- function(dqc, ...) {
  dqc <- check_dqc(dqc)

  infect_mat <- get_infect_mat(...)
  get_r_effective_base(dqc, infect_mat)
}

#' Get R Effective
#'
#' This low-level function calculates R effective from a DQC vector and
#' infection matrix
#'
#' @param dqc Values for DQC compartments. All elements of this vector
#'   should sum to 1.
#' @param infect Matrix. The infection matrix.
#' @export
get_r_effective_base <- function(dqc, infect) {
  sum(dqc %*% infect)
}
