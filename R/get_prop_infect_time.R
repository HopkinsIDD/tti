#' Calculate proportion of the infectious period reduced
#'
#' Calculates the proportion of the infections period (reduced given the
#' distribution of infectiousness and time from symptom onset to isolation).
#' Infectious period is assumed to be gamma distributed.
#'
#' @param delay Numeric greater than 0. Time from symptom onset to isolation
#' @param offset Numeric. Offset of infectiousness compared to symptom onset.
#'     Default is -2.31.
#' @param shape Numeric. Shape of the gamma distribution of infectious period.
#'    Default is 1.65.
#' @param rate Numeric. Rate of the gamma distribution of infectious period.
#'    Default is 0.5.
#' @param isolation_days Numeric greater than 0. Number of days from symptom
#'    onset to end of isolation. Default is `Inf` implying isolation until no
#'    longer infectious.
#'
#' @return Numeric. Proportion of infectious period (reduced given the
#'   distribution of infectiousness and time from symptom onset to isolation).
#' @export
get_prop_infect_time <- function(delay, offset = -2.31,
                                 shape = 1.65, rate = 0.5,
                                 isolation_days = Inf) {
  if (delay < offset) {
    return(0)
  }
  if (isolation_days < Inf) {
    pre_isolation <- stats::pgamma(delay - offset,
                                   shape = shape,
                                   rate = rate)
    post_isolation <- stats::pgamma(isolation_days - offset,
                                    shape = shape, rate = rate,
                                    lower.tail = FALSE)
    return(min(pre_isolation + post_isolation, 1))

  }
  stats::pgamma(delay - offset, shape = shape, rate = rate)
}
