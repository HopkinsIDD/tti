#' Calculate the Passive-Quarantine-Community vector equilibrium
#'
#' This function iterates through the Passive-Quarantine-Community (PQR) vector
#'   until an equilibrium is reached at a given tolerance.
#'
#' @template init
#' @param tolerance Numeric value between 0 and 1. Tolerance to determine whether an
#'    equilibrium has been met. Default: 1e-4
#' @param burn_in Numeric positive value. Minimum number of iterations to run.
#'    Default: 10.
#' @param ... Optional parameters to pass to [`get_infect_mat()`] and
#'   [`get_detect_mat()`]. See [`get_infect_mat()`] and [`get_detect_mat()`]
#'   for defaults.
#'
#' @return A named vector, the Passive-Quarantine-Community proportions at
#'   equilibrium.
#' @export
#'
get_pqc_equilibrium <- function(init = c(
                                  Ps = 0,
                                  Pa = 0,
                                  Qcps = 0,
                                  Qhps = 0,
                                  Qcpa = 0,
                                  Qhpa = 0,
                                  Qq = 0,
                                  Cs = 0.8,
                                  Ca = 0.2
                                ),
                                tolerance = 1e-4,
                                burn_in = 10,
                                ...) {
  init <- check_pqc(init)
  is_probability(tolerance)
  is_positive(burn_in)

  dots <- list(...)

  detect_args <- dots[names(dots) %in%
    c("alpha", "omega_c", "omega_h", "omega_q", "rho_s", "rho_a")]
  infect_args <- dots[names(dots) %in%
    c(
      "alpha", "R", "kappa", "eta", "nu", "t_ps", "t_pa", "t_qcs", "t_qca",
      "t_qhs", "t_qha", "t_incubation", "offset", "shape", "rate"
    )]

  detect <- do.call(get_detect_mat, detect_args)
  infect <- do.call(get_infect_mat, infect_args)
  infect_detect <- infect %*% detect

  pqc <- as.numeric(init)
  pqc_next <- (pqc %*% infect_detect) / sum(pqc %*% infect_detect)
  iter <- 1
  while (iter < burn_in || mean(abs(pqc - pqc_next)) > tolerance) {
    pqc <- pqc_next
    pqc_next <- (pqc %*% infect_detect) / sum(pqc %*% infect_detect)
    iter <- iter + 1

    if (is.nan(sum(pqc_next))) {
      ## Reassign this so it doesn't break the condition evaluation in the next loop
      pqc_next <- pqc 
      ## Break out of the loop on the next iteration
      iter <- burn_in + 1
    }
  }
  categories <- c("Ps", "Pa", "Qcps", "Qhps", "Qcpa", "Qhpa", "Qq", "Cs", "Ca")
  stats::setNames(as.vector(pqc_next), categories)
}
