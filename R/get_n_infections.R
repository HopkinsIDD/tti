##' Calculate Number of Infections
##'
##' Calculates number of potential infections detected from surveillance
##'
##' @param n_tests Numeric positive value. Number of tests.
##' @param prop_pos Proportion of tests that are returned positive
##'
##' @return Numeric. Number of potential infections that can be detected with
##'  testing
##' @export
get_n_infections <- function(n_tests, prop_pos) {
  is_probability(prop_pos)
  is_positive(n_tests)
  return(round(n_tests * prop_pos))
}
