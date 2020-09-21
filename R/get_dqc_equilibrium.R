#' Calculate the Detected-Quarantine-Community vector equilibrium
#'
#' This function iterates through the Detected-Quarantine-Community (DQC) vector
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
#' @return A named vector, the Detected-Quarantine-Community proportions at
#'   equilibrium.
#' @export
#'
get_dqc_equilibrium <- function(init = c(
                                  Ds = 0,
                                  Da = 0,
                                  Qcds = 0,
                                  Qhds = 0,
                                  Qcda = 0,
                                  Qhda = 0,
                                  Qq = 0,
                                  Cs = 0.8,
                                  Ca = 0.2
                                ),
                                tolerance = 1e-4,
                                burn_in = 10,
                                ...) {
  init <- check_dqc(init)
  is_probability(tolerance)
  is_positive(burn_in)

  dots <- list(...)

  detect_args <- dots[names(dots) %in%
    c("alpha", "omega_c", "omega_h", "omega_q", "rho_s", "rho_a")]
  infect_args <- dots[names(dots) %in%
    c(
      "alpha", "R", "kappa", "eta", "nu", "t_ds", "t_da", "t_qcs", "t_qca",
      "t_qhs", "t_qha", "t_incubation", "offset", "shape", "rate"
    )]

  detect <- do.call(get_detect_mat, detect_args)
  infect <- do.call(get_infect_mat, infect_args)

  dqc_next <- get_dqc_equilibrium_base(init, infect, detect, tolerance, burn_in)

  categories <- c("Ds", "Da", "Qcds", "Qhds", "Qcda", "Qhda", "Qq", "Cs", "Ca")
  stats::setNames(as.vector(dqc_next), categories)
}

#' Calculate the vector equilibrium
#'
#' This low-level function iterates through the initial DQC vector
#'   until an equilibrium is reached at a given tolerance.
#'
#' @param init Initial values for DQC compartments. All elements of this vector
#'   should sum to 1.
#' @param infect Matrix. The infection matrix.
#' @param detect Matrix. The detection matrix.
#' @param tolerance Numeric value between 0 and 1. Tolerance to determine whether an
#'    equilibrium has been met. Default: 1e-4
#' @param burn_in Numeric positive value. Minimum number of iterations to run.
#'    Default: 10.
#'
#' @return The proportions at equilibrium.
#' @export
#'
get_dqc_equilibrium_base <- function(init, infect, detect, tolerance = 1e-4, burn_in = 10) {
  dqc <- as.numeric(init)
  infect_detect <- infect %*% detect
  dqc_next <- (dqc %*% infect_detect) / sum(dqc %*% infect_detect)
  iter <- 1
  while (iter < burn_in || mean(abs(dqc - dqc_next)) > tolerance) {
    dqc <- dqc_next
    dqc_next <- (dqc %*% infect_detect) / sum(dqc %*% infect_detect)
    iter <- iter + 1

    if (is.nan(sum(dqc_next))) {
      ## Reassign this so it doesn't break the condition evaluation in the next loop
      dqc_next <- dqc
      ## Break out of the loop on the next iteration
      iter <- burn_in + 1
    }
  }
  dqc_next
}
