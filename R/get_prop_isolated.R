#' Calculate the proportion detected and isolated through testing
#'
#' Calculates the proportion detected and isolated through testing
#' (i.e. outside of contact tracing) from a Detected-Quarantine-
#' Community vector
#'
#' @template dqc
#'
#' @return Numeric value. Proportion detected and isolated.
#' @export
get_prop_passive <- function(dqc) {
  dqc <- check_dqc(dqc)

  dqc[["Ds"]] + dqc[["Da"]]
}
