get_prop_undetected <- function(quarantine_days) {
  d <- boot_lnorm_params_covid
  d$u <- stats::runif(1000, 1, 5)
  d <- d[rep(seq_len(nrow(d)), each = quarantine_days), ]
  d$d <- rep(1:quarantine_days, length.out = nrow(d))
  d$p <- stats::plnorm(d$d + d$u, meanlog = d$meanlog, sdlog = d$sdlog,
                lower.tail = FALSE)
  median(d$p)
}
