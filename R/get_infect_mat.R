#' Create infect matrix
#'
#' This function will create the INFECT matrix
#'
#' @param alpha Numeric value between 0 and 1. The probability of an asymptomatic
#'    infection. Default 0.2.
#' @param R Positive numeric value. Reproduction number. Default: 2.5.
#' @param kappa Positive numeric value. Relative transmissibility of an
#'    asymptomatic individual compared to a symptomatic individual.
#'    Default: 0.5.
#' @param eta Numeric value between 0 and 1. Probability contact is a household
#'    contact. Default: 0.5
#' @param nu Positive numeric value. Relative risk of infection for a household
#'     contact compared to a community contact. Default: 4.
#' @param t_ds Non-negative numeric value. Time delay from symptom onset to
#'     isolation in detected symptomatic person. Default: 3.
#' @param t_da Non-negative numeric value. Time delay from symptom onset to
#'     isolation in detected asymptomatic person. Default: 3.
#' @param t_qcs Non-negative numeric value. Time delay from symptomatic index
#'     cases's symptom onset to quarantine of community contacts.
#'     Default: 3.
#' @param t_qca Non-negative numeric value. Time delay from asymptomatic index
#'     cases's symptom onset to quarantine of community contacts.
#'     Default: 3.
#' @param t_qhs Non-negative numeric value. Time delay from symptomatic index
#'     cases's symptom onset to quarantine of household contacts.
#'     Default: 3.
#' @param t_qha Non-negative numeric value. Time delay from asymptomatic index
#'     cases's symptom onset to quarantine of community contacts.
#'     Default: 3.
#' @param t_q Non-negative numeric value. Time delay from quarantined index
#'     cases's symptom onset to quarantine of contacts.
#'     Default: 3.
#' @param t_incubation Non-negative numeric value. The estimated average
#'     incubation time. Default: 5.5.
#' @param offset Numeric. Offset of infectiousness compared to symptoms onset.
#'     Default is -2.31.
#' @param shape Numeric. Shape of the gamma distribution of infectious period.
#'    Default is 1.65.
#' @param rate Numeric. Rate of the gamma distribution of infectious period.
#'    Default is 0.5.
#' @param isolation_days Numeric greater than 0. Number of days from symptom
#'    onset to end of isolation. Default is `Inf` implying isolation until no
#'    longer infectious.
#' @return Matrix
#' @export
get_infect_mat <- function(alpha = 0.2, R = 2.5, kappa = 0.5, eta = 0.5, nu = 4,
                           t_ds = 3, t_da = 3, t_qcs = 3, t_qca = 3, t_qhs = 3,
                           t_qha = 3, t_q = 3, t_incubation = 5.5, offset = -2.31,
                           shape = 1.65, rate = 0.5, isolation_days = Inf) {
  is_probability(alpha)
  is_probability(eta)
  is_positive(R)
  is_positive(nu)
  is_positive(t_ds)
  is_positive(t_da)
  is_positive(t_qcs)
  is_positive(t_qca)
  is_positive(t_qhs)
  is_positive(t_qha)
  is_positive(t_q)

  gamma_ds <- get_prop_infect_time(
    t_ds,
    offset = offset, shape = shape, rate = rate, isolation_days = isolation_days
  )
  gamma_da <- get_prop_infect_time(
    t_da,
    offset = offset, shape = shape, rate = rate, isolation_days = isolation_days
  )

  second_gen_shape <- shape + (shape + (t_incubation + offset) * rate)

  gamma_qcs <- get_prop_infect_time(
    t_qcs,
    shape = second_gen_shape, rate = rate, offset = 0,
    isolation_days = isolation_days
  )
  gamma_qca <- get_prop_infect_time(
    t_qca,
    shape = second_gen_shape, rate = rate, offset = 0,
    isolation_days = isolation_days
  )
  gamma_qhs <- get_prop_infect_time(
    t_qhs,
    shape = second_gen_shape, rate = rate, offset = 0,
    isolation_days = isolation_days
  )
  gamma_qha <- get_prop_infect_time(
    t_qha,
    shape = second_gen_shape, rate = rate, offset = 0,
    isolation_days = isolation_days
  )
  gamma_q <- get_prop_infect_time(
    t_q,
    shape = second_gen_shape, rate = rate, offset = 0,
    isolation_days = isolation_days
  )

  R_s <- R / ((alpha * kappa) - alpha + 1)
  R_a <- (kappa * R) / ((alpha * kappa) - alpha + 1)
  R_c <- R / ((eta * nu) - eta + 1)
  R_h <- (nu * R) / ((eta * nu) - eta + 1)
  R_ah <- (kappa * nu * R) / (((alpha * kappa) - alpha + 1) *
    ((eta * nu) - eta + 1))
  R_ac <- (kappa * R) / (((alpha * kappa) - alpha + 1) *
    ((eta * nu) - eta + 1))
  R_sh <- (nu * R) / (((alpha * kappa) - alpha + 1) * ((eta * nu) - eta + 1))
  R_sc <- R / (((alpha * kappa) - alpha + 1) * ((eta * nu) - eta + 1))

  matrix(
    c(
      (1 - eta) * gamma_ds * R_sc, eta * gamma_ds * R_sh, 0, 0, 0, 0,
      0, 0, (1 - eta) * gamma_da * R_ac, eta * gamma_da * R_ah, 0, 0,
      0, 0, 0, 0, gamma_qcs * R, 0,
      0, 0, 0, 0, gamma_qhs * R, 0,
      0, 0, 0, 0, gamma_qca * R, 0,
      0, 0, 0, 0, gamma_qha * R, 0,
      0, 0, 0, 0, gamma_q * R, 0,
      0, 0, 0, 0, 0, R_s,
      0, 0, 0, 0, 0, R_a
    ),
    nrow = 9,
    byrow = TRUE
  )
}
