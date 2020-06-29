#' Calculate the proportion quarantined
#'
#' Calculates the proportion passively detected from a
#' Passive-Quarantine-Community vector
#'
#' @template pqc
#'
#' @return Numeric value. Proportion passively detected.
#' @export
get_prop_passive <- function(pqc) {
  pqc <- check_pqc(pqc)

  pqc[["Ps"]] + pqc[["Pa"]]
}
