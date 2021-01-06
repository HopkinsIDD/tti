#' Calculate proportion of the infectious period reduced
#'
#' Calculates the proportion of the infections period reduced given the
#' distribution of infectiousness and time from symptom onset to isolation.
#' Infectious period is assumed to be gamma distributed.
#'
#' @param delay Numeric greater than 0. Time from symptom onset to isolation
#' @param offset Numeric. Offset of infectiousness compared to symptom onset.
#'     Default is -2.31.
#' @param shape Numeric. Shape of the gamma distribution of infectious period.
#'    Default is 1.65.
#' @param rate Numeric. Rate of the gamma distribution of infectious period.
#'    Default is 0.5.
#'
#' @return Numeric. Proportion of infectious period reduced given the
#'   distribution of infectiousness and time from symptom onset to isolation.
#' @export
get_prop_infect_time <- function(delay, offset = -12.27,
                                 shape = 21.13, rate = 1.592) {
  if (delay < offset) {
    return(0)
  }
  stats::pgamma(delay - offset, shape = shape, rate = rate)
}
