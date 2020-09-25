#' Create detect matrix
#'
#' This function will create the DETECT matrix
#'
#' @param alpha Numeric value between 0 and 1. The probability of an asymptomatic
#'    infection. Default: 0.2.
#' @param omega_c Numeric value between 0 and 1. The probability of being traced and
#'    quarantined given infected community contact of isolated case. Default: 0.5.
#' @param omega_h Numeric value between 0 and 1. The probability of being traced and
#'    quarantined given infected household contact of isolated case. Default: 0.5.
#' @param omega_q Numeric value between 0 and 1. The probability of being traced and
#'    quarantined given infected contact of quarantined person. Default: 0.5.
#' @param quarantine_days Positive numeric value. The number of days contacts are told
#'    to quarantine. Default: 14.
#' @param rho_s Numeric value between 0 and 1. The probability of detection
#'    and isolation given symptomatic. Default: 0.1.
#' @param rho_a Numeric value between 0 and 1. The probability of detection
#'    and isolation given asymptomatic. Default: 0.05.
#'
#' @return Matrix
#' @export

get_detect_mat <-
  function(alpha = 0.2,
           omega_c = 0.5,
           omega_h = 0.5,
           omega_q = 0.5,
           quarantine_days = 14,
           rho_s = 0.1,
           rho_a = 0.05) {
    is_positive(quarantine_days)
    is_probability(alpha)
    is_probability(omega_c)
    is_probability(omega_h)
    is_probability(omega_q)
    is_probability(rho_s)
    is_probability(rho_a)

    beta <- 1 - get_prop_undetected(quarantine_days)

    matrix(
      c(
        (1 - alpha) * (1 - (omega_c * beta)) * rho_s,
        alpha * (1 - (omega_c * beta)) * rho_a,
        (omega_c * beta),
        0,
        0,
        0,
        0,
        (1 - alpha) * (1 - (omega_c * beta)) * (1 - rho_s),
        alpha * (1 - (omega_c * beta)) * (1 - rho_a),
        (1 - alpha) * (1 - (omega_h * beta)) * rho_s,
        alpha * (1 - (omega_h * beta)) * rho_a,
        0,
        (omega_h * beta),
        0,
        0,
        0,
        (1 - alpha) * (1 - (omega_h * beta)) * (1 - rho_s),
        alpha * (1 - (omega_h * beta)) * (1 - rho_a),
        (1 - alpha) * (1 - (omega_c * beta)) * rho_s,
        alpha * (1 - (omega_c * beta)) * rho_a,
        0,
        0,
        (omega_c * beta),
        0,
        0,
        (1 - alpha) * (1 - (omega_c * beta)) * (1 - rho_s),
        alpha * (1 - (omega_c * beta)) * (1 - rho_a),
        (1 - alpha) * (1 - (omega_h * beta)) * rho_s,
        alpha * (1 - (omega_h * beta)) * rho_a,
        0,
        0,
        0,
        (omega_h * beta),
        0,
        (1 - alpha) * (1 - (omega_h * beta)) * (1 - rho_s),
        alpha * (1 - (omega_h * beta)) * (1 - rho_a),
        (1 - alpha) * (1 - (omega_q * beta)) * rho_s,
        alpha * (1 - (omega_q * beta)) * rho_a,
        0,
        0,
        0,
        0,
        (omega_q * beta),
        (1 - alpha) * (1 - (omega_q * beta)) * (1 - rho_s),
        alpha * (1 - (omega_q * beta)) * (1 - rho_a),
        (1 - alpha) * rho_s,
        alpha * rho_a,
        0,
        0,
        0,
        0,
        0,
        (1 - alpha) * (1 - rho_s),
        alpha * (1 - rho_a)
      ),
      nrow = 6,
      byrow = TRUE
    )
  }
