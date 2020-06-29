#' Create a data frame with the proportion infected over time
#'
#' @param duration Numeric value greater than 0. The number of iterations you
#'    would like to examine.
#' @template init
#' @param ... Optional parameters to pass to [`get_infect_mat()`] and
#'   [`get_detect_mat()`]. See [`get_infect_mat()`] and [`get_detect_mat()`]
#'   for defaults.
#'
#' @return data frame with three columns:
#'   * `t`: the time
#'   * `prop_infected`: the proportion infected
#'   * `r_effective`: the effective reproduction number at the given time point
#'   * `category`: the category
#' @export
#'
get_proportions_df <- function(duration, init = c(
                                 Ps = 0, Pa = 0, Qcps = 0, Qhps = 0, Qcpa = 0,
                                 Qhpa = 0, Qq = 0, Cs = 0.8, Ca = 0.2
                               ), ...) {
  init <- check_pqc(init)
  dots <- list(...)

  detect_args <- dots[names(dots) %in%
    c("alpha", "omega_c", "omega_h", "omega_q", "rho_s", "rho_a")]

  infect_args <- dots[names(dots) %in%
    c(
      "alpha", "R", "kappa", "eta", "nu", "t_ps", "t_pa", "t_qcs", "t_qca",
      "t_qhs", "t_qha", "t_incubation", "offset", "shape", "rate"
    )]

  categories <- c("Ps", "Pa", "Qcps", "Qhps", "Qcpa", "Qhpa", "Qq", "Cs", "Ca")

  detect <- do.call(get_detect_mat, detect_args)
  infect <- do.call(get_infect_mat, infect_args)
  infect_detect <- infect %*% detect

  d <- tibble::tibble(
    t = 1,
    prop_infected = as.numeric(init),
    r_effective = sum(as.numeric(init) %*% infect),
    category = categories
  )

  time <- 2
  while (time <= duration) {
    pqc <-
      as.numeric(d$prop_infected[d$t == time - 1] %*% infect_detect)
    d <- tibble::add_row(
      d,
      t = time,
      prop_infected =  pqc / sum(pqc),
      r_effective = sum((pqc / sum(pqc)) %*% infect),
      category = categories
    )
    time <- time + 1
  }
  d
}
