#' Calculate the proportion quarantined
#'
#' Calculates the proportion quarantined from a Passive-Quarantine-Community
#' vector
#'
#' @template pqc
#'
#' @return Numeric value. Proportion quarantined.
#' @export
get_prop_quarantined <- function(pqc) {
  pqc <- check_pqc(pqc)

  pqc[["Qcps"]] + pqc[["Qcpa"]] + pqc[["Qhps"]] + pqc[["Qhpa"]] + pqc[["Qq"]]
}
