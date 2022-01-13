#' Create a data frame with R effective, varying parameters
#'
#' This function allows you to input numeric scalars or vectors for each
#'    parameter to examine how the effective R will vary across configurations.
#'    Two values (`stoch`, `n_iter`) require single values.
#'
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
#' @param omega_c Numeric value or vector of numeric values between 0 and 1. The probability of being traced and
#'    quarantined given community contact of a person. Default: 0.5.
#' @param omega_h Numeric value or vector of numeric values between 0 and 1. The probability of being traced and
#'    quarantined given household contact of a person. Default: 0.5.
#' @param omega_q Numeric value or vector of numeric values between 0 and 1. The probability of being traced and
#'    quarantined given quarantine contact of a person. Default: 0.5.
#' @param quarantine_days Positive numeric value. The number of days contacts are told
#'    to quarantine. Default: 14.
#' @param rho_s Numeric value or vector of numeric values between 0 and 1. The probability of
#'    detection and isolation given symptomatic. Default: 0.1.
#' @param rho_a Numeric value or vector of numeric values between 0 and 1. The probability of
#'    detection and isolation given asymptomatic. Default: 0.05.
#' @param t_incubation Non-negative numeric value. The estimated average
#'     incubation time. Default: 5.5.
#' @param offset Numeric. Offset of infectiousness compared to symptoms onset.
#'     Default is -2.31.
#' @param shape Numeric. Shape of the gamma distribution of infectious period.
#'    Default is 1.65.
#' @param rate Numeric. Rate of the gamma distribution of infectious period.
#'    Default is 0.5.
#' @param stoch Logical. Whether to run stochastic model with overdispersion.
#'    Default is FALSE.
#' @param theta Non-negative numeric. Required only if `stoch`=TRUE. Overdispersion
#'    parameter for negative binomial distribution. Default NULL.
#' @param n_inf Non-negative numeric. Required only if `stoch`=TRUE. Number of
#'    infections (or effective population size of infected individuals).
#'    Default NULL.
#' @param n_iter Non-negative numeric. Required only if `stoch`=TRUE. Number of
#'    iterations of stochastic model to run for each unique parameter value.
#'    Default NULL.
#'
#' @return Data frame with columns:
#'   * `r_effective`: The R effective value
#'   * `prop_identified`: The proportion of infections identified
#'   * `alpha`
#'   * `R`
#'   * `kappa`
#'   * `eta`
#'   * `nu`
#'   * `t_ds`
#'   * `t_da`
#'   * `t_qcs`
#'   * `t_qca`
#'   * `t_qhs`
#'   * `t_qha`
#'   * `t_q`
#'   * `omega_c`
#'   * `omega_h`
#'   * `omega_q`
#'   * `quarantine_days`
#'   * `rho_s`
#'   * `rho_a`
#'   * `theta` if `stoch`=TRUE
#'   * `n_inf` if `stoch`=TRUE
#'
#' @export
#'
#' @examples
#' get_r_effective_df(alpha = c(0.1, 0.2), kappa = c(0.5, 0.6))
#' get_r_effective_df(stoch = TRUE, theta = c(0.1, 0.3), n_inf = 100, n_iter = 100)
get_r_effective_df <- function(alpha = 0.2, R = 2.5, kappa = 0.5, eta = 0.5, nu = 4,
                               t_ds = 3, t_da = 3, t_qcs = 3, t_qca = 3, t_qhs = 3,
                               t_qha = 3, t_q = 3, omega_c = 0.5,
                               omega_h = 0.5,
                               omega_q = 0.5,
                               quarantine_days = Inf,
                               rho_s = 0.1, rho_a = 0.05,
                               t_incubation = 5.5,
                               offset = -2.31,
                               shape = 1.65, rate = 0.5,
                               stoch = FALSE, theta = NULL, n_inf = NULL, n_iter = NULL) {
  if (length(n_iter) > 1 | length(stoch) > 1) {
    stop_glue(
      "A single value must be provided for `stoch` and `n_iter`"
    )
  }

  if (stoch & (is.null(theta) | is.null(n_inf) | is.null(n_iter))) {
    stop_glue(
      "Values must be provided for `theta`, `n_inf`, and `n_iter`",
      "to run a stochastic model."
    )
  }

  if (!stoch & (length(theta) > 0 | length(n_inf) > 0 | length(n_iter) > 0)) {
    warning("Parameters `theta`, `n_inf`, and `n_iter` are ignored in deterministic model.
  Please use `stoch`=TRUE for stochastic model")
    theta <- NULL
    n_inf <- NULL
    n_iter <- NULL
  }

  if (stoch) {
    iterations <- 1:n_iter
  } else {
    iterations <- NULL
  }

  lst <- tidyr::expand_grid(
    alpha = alpha,
    R = R,
    kappa = kappa,
    eta = eta,
    nu = nu,
    t_ds = t_ds,
    t_da = t_da,
    t_qcs = t_qcs,
    t_qca = t_qca,
    t_qhs = t_qhs,
    t_qha = t_qha,
    t_q = t_q,
    omega_c = omega_c,
    omega_h = omega_h,
    omega_q = omega_q,
    quarantine_days = quarantine_days,
    rho_s = rho_s,
    rho_a = rho_a,
    t_incubation = t_incubation,
    offset = offset,
    shape = shape,
    rate = rate,
    theta = theta,
    n_inf = n_inf,
    n_iter = iterations
  )

  if (stoch) {
    purrr::pmap_df(lst, get_r_effective_df_one_stoch)
  } else {
    purrr::pmap_df(lst, get_r_effective_df_one)
  }
}

get_r_effective_df_one <- function(alpha, R, kappa, eta, nu, t_ds, t_da, t_qcs, t_qca,
                                   t_qhs, t_qha, t_q, omega_c, omega_h, omega_q,
                                   quarantine_days, rho_s,
                                   rho_a, t_incubation, offset, shape, rate) {
  dqc <- get_dqc_equilibrium(
    alpha = alpha,
    R = R,
    kappa = kappa,
    eta = eta,
    nu = nu,
    t_ds = t_ds,
    t_da = t_da,
    t_qcs = t_qcs,
    t_qca = t_qca,
    t_qhs = t_qhs,
    t_qha = t_qha,
    t_q = t_q,
    omega_c = omega_c,
    omega_h = omega_h,
    omega_q = omega_q,
    quarantine_days = quarantine_days,
    rho_s = rho_s,
    rho_a = rho_a,
    t_incubation = t_incubation,
    offset = offset,
    shape = shape,
    rate = rate
  )
  r_effective <- get_r_effective(
    dqc,
    alpha = alpha,
    R = R,
    kappa = kappa,
    eta = eta,
    nu = nu,
    t_ds = t_ds,
    t_da = t_da,
    t_qcs = t_qcs,
    t_qca = t_qca,
    t_qhs = t_qhs,
    t_qha = t_qha,
    t_q = t_q,
    t_incubation = t_incubation,
    offset = offset,
    shape = shape,
    rate = rate
  )
  tibble::tibble(
    r_effective = r_effective,
    prop_identified = get_prop_identified(dqc),
    alpha = alpha,
    R = R,
    kappa = kappa,
    eta = eta,
    nu = nu,
    t_ds = t_ds,
    t_da = t_da,
    t_qcs = t_qcs,
    t_qca = t_qca,
    t_qhs = t_qhs,
    t_qha = t_qha,
    t_q = t_q,
    omega_c = omega_c,
    omega_h = omega_h,
    omega_q = omega_q,
    quarantine_days = quarantine_days,
    rho_s = rho_s,
    rho_a = rho_a
  )
}


get_r_effective_df_one_stoch <- function(alpha, R, kappa, eta, nu, t_ds, t_da, t_qcs, t_qca,
                                         t_qhs, t_qha, t_q, omega_c, omega_h, omega_q, quarantine_days,
                                         rho_s, rho_a, t_incubation, offset, shape, rate, n_inf, theta, n_iter) {
  dqc <- get_dqc_stoch(
    alpha = alpha,
    R = R,
    kappa = kappa,
    eta = eta,
    nu = nu,
    t_ds = t_ds,
    t_da = t_da,
    t_qcs = t_qcs,
    t_qca = t_qca,
    t_qhs = t_qhs,
    t_qha = t_qha,
    t_q = t_q,
    omega_c = omega_c,
    omega_h = omega_h,
    omega_q = omega_q,
    quarantine_days = quarantine_days,
    rho_s = rho_s,
    rho_a = rho_a,
    t_incubation = t_incubation,
    offset = offset,
    shape = shape,
    rate = rate,
    theta = theta,
    n_inf = n_inf
  )
  r_effective <- get_r_effective(
    dqc,
    alpha = alpha,
    R = R,
    kappa = kappa,
    eta = eta,
    nu = nu,
    t_ds = t_ds,
    t_da = t_da,
    t_qcs = t_qcs,
    t_qca = t_qca,
    t_qhs = t_qhs,
    t_qha = t_qha,
    t_q = t_q,
    t_incubation = t_incubation,
    offset = offset,
    shape = shape,
    rate = rate
  )
  tibble::tibble(
    r_effective = r_effective,
    prop_identified = get_prop_identified(dqc),
    n_iter = n_iter,
    alpha = alpha,
    R = R,
    kappa = kappa,
    eta = eta,
    nu = nu,
    t_ds = t_ds,
    t_da = t_da,
    t_qcs = t_qcs,
    t_qca = t_qca,
    t_qhs = t_qhs,
    t_qha = t_qha,
    t_q = t_q,
    omega_c = omega_c,
    omega_h = omega_h,
    omega_q = omega_q,
    quarantine_days = quarantine_days,
    rho_s = rho_s,
    rho_a = rho_a,
    n_inf = n_inf,
    theta = theta
  )
}
