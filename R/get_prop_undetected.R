get_prop_undetected <- function(quarantine_days) {
  d <- boot_lnorm_params_covid
  d$d <- quarantine_days
  d$p <- stats::plnorm(d$d, meanlog = d$meanlog, sdlog = d$sdlog,
                lower.tail = FALSE)
  stats::median(d$p)
}
