#' Create an input dataframe that serve as parameter input for stochastic simulations
#'
#'
#' @param nsimulations number of stochastic simulations
#' @param input_list list of input parameters with named elements: alpha, omega_c, omega_h, omega_q, rho_s, rho_a, R, kappa, eta, nu, t_ps, t_pa, t_qcs, t_qca, t_qhs, t_qha, t_q
#' @param input_vartype list of input parameter variable types with named elements: alpha, omega_c, omega_h, omega_q, rho_s, rho_a, R, kappa, eta, nu, t_ps, t_pa, t_qcs, t_qca, t_qhs, t_qha, t_q. Currently supported character options are "proportion", "R_overdispersed", "uniform", "poisson", "fixed"
#'
#' @return dataframe
#' @export
#'
make_input_stochastic <- function(nsimulations, input_list, input_vartype) {
  if (length(input_list) != 17) {
    stop("You have not specified the correct number of variables. Please include: alpha, omega_c, omega_h, omega_q, rho_s, rho_a, R, kappa, eta, nu, t_ps, t_pa, t_qcs, t_qca, t_qhs, t_qha, t_q.")
  }
  if (length(input_list) != length(input_vartype)) {
    stop("You have not provided the same number of variables in input_list and input_vartype")
  }

  param_list <- lapply(1:length(input_list), function(i) {
    varname <- names(input_list)[i]
    varvalue <- input_list[[varname]]
    vartype <- input_vartype[[varname]]

    if (vartype == "proportion") {
      warning(glue::glue("Assuming that {varname} is the probability parameter for a binomial distribution, we examined the number of successes in 1000 events and divided the random draw by 1000 to get a stochastic parameter value for {varname} centered around {varvalue}."))
      rc <- stats::rbinom(nsimulations, 1000, prob = varvalue) / 1000
    } else if (vartype == "R_overdispersed") {

      ## set k to a value between .04 and .2 (https://cmmid.github.io/topics/covid19/overdispersion-from-outbreaksize.html)
      sz <- varvalue / ((1 / .12) - 1)
      warning(glue::glue("Assuming that {varname} is drawn from a negative binomial distribution with mu {varvalue} and size {sz}. Minimum value is set to 0.05."))
      rc <- pmax(stats::rnbinom(nsimulations, mu = varvalue, size = sz), .05)
    } else if (vartype == "uniform") {
      minvar <- pmax(varvalue - 1, 0)
      maxvar <- varvalue + 1
      warning(glue::glue("Assuming that {varname} is drawn from a uniform distribution from {minvar} to {maxvar}."))
      rc <- stats::runif(nsimulations, min = minvar, max = maxvar)
    } else if (vartype == "poisson") {
      warning(glue::glue("Assuming that {varname} is drawn from a Poisson distribution with rate {varvalue}."))
      rc <- stats::rpois(nsimulations, lambda = varvalue)
    } else if (vartype == "fixed") {
      warning(glue::glue("Assuming that {varname} is fixed at {varvalue}."))
      rc <- rep(varvalue, nsimulations)
    } else {
      warning(glue::glue("You have specified an invalid vartype for {varname}. Assuming a fixed value at {varvalue}."))
      rc <- rep(varvalue, nsimulations)
    }

    return(rc)
  })

  rc <- dplyr::bind_cols(param_list)
  names(rc) <- names(input_list)
  rc$sim_id <- 1:nrow(rc)

  return(rc)
}
