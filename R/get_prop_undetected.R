get_prop_undetected <- function(quarantine_days) {
  stats::plnorm(quarantine_days, meanlog = 1.63, sdlog = 0.5,
                lower.tail = FALSE)
  ## based on BMJ Open meta analysis: http://dx.doi.org/10.1136/bmjopen-2020-039652
}
