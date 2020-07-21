#' Create a data frame with R effective, varying parameters
#'
#' This function allows you to input numeric scalars or vectors for each
#'    parameter to examine how the effective R will vary across configurations.
#' @param alpha Numeric value or vector of numeric values between 0 and 1.
#'    The probability of an asymptomatic infection. Default 0.2.
#' @param R Positive numeric value or vector of positive numeric values.
#'     Reproduction number. Default: 2.5.
#' @param kappa Positive numeric value or vector of positive numeric values. Relative transmissibility of an
#'    asymptomatic individual compared to a symptomatic individual.
#'    Default: 0.5.
#' @param eta Numeric value or vector of numeric values between 0 and 1. Probability contact is a household
#'    contact. Default: 0.5
#' @param nu Positive numeric value or vector of positive numeric values. Relative risk of infection for a household
#'     contact compared to a community contact. Default: 4.
#' @param t_ps Positive numeric value. Time delay from symptom onset to
#'     isolation in passively detected symptomatic person. Default: 3.
#' @param t_pa Positive numeric value. Time delay from symptom onset to
#'     isolation in passively detected asymptomatic person. Default: 3.
#' @param t_qcs Positive numeric value. Time delay from symptomatic index
#'     cases's symptom onset to quarantine of community contacts.
#'     Default: 3.
#' @param t_qca Positive numeric value. Time delay from asymptomatic index
#'     cases's symptom onset to quarantine of community contacts.
#'     Default: 3.
#' @param t_qhs Positive numeric value. Time delay from symptomatic index
#'     cases's symptom onset to quarantine of household contacts.
#'     Default: 3.
#' @param t_qha Positive numeric value. Time delay from asymptomatic index
#'     cases's symptom onset to quarantine of community contacts.
#'     Default: 3.
#' @param t_q Positive numeric value. Time delay from quarantined index
#'     cases's symptom onset to quarantine of contacts.
#'     Default: 3.
#' @param omega_c Numeric value or vector of numeric values between 0 and 1. The probability of being traced and
#'    quarantined given community contact of a person. Default: 0.5.
#' @param omega_h Numeric value or vector of numeric values between 0 and 1. The probability of being traced and
#'    quarantined given household contact of a person. Default: 0.5.
#' @param omega_q Numeric value or vector of numeric values between 0 and 1. The probability of being traced and
#'    quarantined given quarantine contact of a person. Default: 0.5.
#' @param rho_s Numeric value or vector of numeric values between 0 and 1. The probability of passive
#'    detection given symptomatic. Default: 0.1.
#' @param rho_a Numeric value or vector of numeric values between 0 and 1. The probability of passive
#'    detection given asymptomatic. Default: 0.05.
#' @param offset Numeric. Offset of infectiousness compared to symptoms onset.
#'     Default is -2.31.
#' @param shape Numeric. Shape of the gamma distribution of infectious period.
#'    Default is 1.65.
#' @param rate Numeric. Rate of the gamma distribution of infectious period.
#'    Default is 0.5.
#'
#' @return Data frame with 18 columns:
#'   * `r_effective`: The R effective value
#'   * `alpha`
#'   * `R`
#'   * `kappa`
#'   * `eta`
#'   * `nu`
#'   * `t_ps`
#'   * `t_pa`
#'   * `t_qcs`
#'   * `t_qca`
#'   * `t_qhs`
#'   * `t_qha`
#'   * `t_q`
#'   * `omega_c`
#'   * `omega_h`
#'   * `omega_q`
#'   * `rho_s`
#'   * `rho_a`
#' @export
#'
#' @examples
#' get_r_effective_df(alpha = c(0.1, 0.2), kappa = c(0.5, 0.6))
get_r_effective_df <- function(alpha = 0.2, R = 2.5, kappa = 0.5, eta = 0.5, nu = 4,
                               t_ps = 3, t_pa = 3, t_qcs = 3, t_qca = 3, t_qhs = 3,
                               t_qha = 3, t_q = 3, omega_c = 0.5,
                               omega_h = 0.5,
                               omega_q = 0.5,
                               rho_s = 0.1, rho_a = 0.05, offset = -2.31,
                               shape = 1.65, rate = 0.5) {
  lst <- expand.grid(
    alpha = alpha,
    R = R,
    kappa = kappa,
    eta = eta,
    nu = nu,
    t_ps = t_ps,
    t_pa = t_pa,
    t_qcs = t_qcs,
    t_qca = t_qca,
    t_qhs = t_qhs,
    t_qha = t_qha,
    t_q = t_q,
    omega_c = omega_c,
    omega_h = omega_h,
    omega_q = omega_q,
    rho_s = rho_s,
    rho_a = rho_a,
    offset = offset,
    shape = shape,
    rate = rate
  )

  purrr::pmap_df(lst, get_r_effective_df_one)
}
get_r_effective_df_one <- function(alpha, R, kappa, eta, nu, t_ps, t_pa, t_qcs, t_qca,
                                   t_qhs, t_qha, t_q, omega_c, omega_h, omega_q, rho_s,
                                   rho_a, offset, shape, rate) {
  pqc <- get_pqc_equilibrium(
    alpha = alpha,
    R = R,
    kappa = kappa,
    eta = eta,
    nu = nu,
    t_ps = t_ps,
    t_pa = t_pa,
    t_qcs = t_qcs,
    t_qca = t_qca,
    t_qhs = t_qhs,
    t_qha = t_qha,
    t_q = t_q,
    omega_c = omega_c,
    omega_h = omega_h,
    omega_q = omega_q,
    rho_s = rho_s,
    rho_a = rho_a,
    offset = offset,
    shape = shape,
    rate = rate
  )
  r_effective <- get_r_effective(
    pqc,
    alpha = alpha,
    R = R,
    kappa = kappa,
    eta = eta,
    nu = nu,
    t_ps = t_ps,
    t_pa = t_pa,
    t_qcs = t_qcs,
    t_qca = t_qca,
    t_qhs = t_qhs,
    t_qha = t_qha,
    t_q = t_q,
    offset = offset,
    shape = shape,
    rate = rate
  )
  tibble::tibble(
    r_effective = r_effective,
    alpha = alpha,
    R = R,
    kappa = kappa,
    eta = eta,
    nu = nu,
    t_ps = t_ps,
    t_pa = t_pa,
    t_qcs = t_qcs,
    t_qca = t_qca,
    t_qhs = t_qhs,
    t_qha = t_qha,
    t_q = t_q,
    omega_c = omega_c,
    omega_h = omega_h,
    omega_q = omega_q,
    rho_s = rho_s,
    rho_a = rho_a
  )
}
