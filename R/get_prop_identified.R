#' Calculate the proportion isolated and quarantined
#'
#' Calculates the proportion isolated and quarantined from a Passive-Quarantine-Community
#' vector
#'
#' @template pqc
#'
#' @return Numeric value. Proportion identified.
#' @export
get_prop_identified <- function(pqc) {
  pqc <- check_pqc(pqc)

  pqc[["Ps"]] + pqc[["Pa"]] + pqc[["Qcps"]] + pqc[["Qcpa"]] + pqc[["Qhps"]] +
    pqc[["Qhpa"]] + pqc[["Qq"]]
}
