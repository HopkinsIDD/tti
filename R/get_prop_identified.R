#' Calculate the proportion isolated and quarantined
#'
#' Calculates the proportion isolated and quarantined from a 
#' Detected-Quarantine-Community vector
#'
#' @template dqc
#'
#' @return Numeric value. Proportion identified.
#' @export
get_prop_identified <- function(dqc) {
  dqc <- check_dqc(dqc)

  dqc[["Ds"]] + dqc[["Da"]] + dqc[["Qcds"]] + dqc[["Qcda"]] + dqc[["Qhds"]] +
    dqc[["Qhda"]] + dqc[["Qq"]]
}
