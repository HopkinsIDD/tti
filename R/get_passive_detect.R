##' Calculate passive detection rates for infections
##'
##' Function to calculate proportion of infections
##'    detected through testing + surveillance
##'
##' @param n_tests Numeric positive value. Number of tests devoted to
##'    detection of infection type of interest
##' @param prop_pos Numeric value between 0 and 1. Proportion of tests that
##'    are returned positive among infection type of interest
##' @param n_infect Numeric positive value. Number of infections in the
##'    population
##' @param alpha Numeric value between 0 and 1. Proportion of infections
##'    that of infection type of interest
##' @return Numeric.  Proportion of infections of type of interested that
##'    are detected
get_passive_detect <- function(n_tests, prop_pos, n_infect, alpha) {
  is_positive(n_infect)
  is_positive(n_tests)
  is_probability(prop_pos)
  is_probability(alpha)

  n_detected <- get_n_infections(n_tests, prop_pos)

  ## If testing capacity is high, make sure we can't detect more than 100%
  ## of infections

  prop_detected <- pmin(n_detected / (alpha * n_infect), 1)
  return(prop_detected)
}
