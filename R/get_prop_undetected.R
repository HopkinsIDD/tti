get_prop_undetected <- function(quarantine_days) {
  d <- boot_lnorm_params_covid
  d <- d[rep(seq_len(nrow(d)), each = quarantine_days), ]
  d$d <- rep(1:quarantine_days, length.out = nrow(d))
  d$p <- stats::plnorm(d$d, meanlog = d$meanlog, sdlog = d$sdlog,
                lower.tail = FALSE)
  median(d$p[d$d == quarantine_days])
}
