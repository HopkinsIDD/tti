#' Calculate the proportion quarantined
#'
#' Calculates the proportion quarantined from a Detected-Quarantine-Community
#' vector
#'
#' @template dqc
#'
#' @return Numeric value. Proportion quarantined.
#' @export
get_prop_quarantined <- function(dqc) {
  dqc <- check_dqc(dqc)

  dqc[["Qcds"]] + dqc[["Qcda"]] + dqc[["Qhds"]] + dqc[["Qhda"]] + dqc[["Qq"]]
}
