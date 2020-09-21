#' Creates an data frame that serves as parameter inputs for stochastic simulations
#'
#'
#' @param nsimulations number of stochastic simulations
#' @param input_list list of input parameters with named elements: alpha, omega_c, omega_h, omega_q, rho_s, rho_a, R, kappa, eta, nu, t_ds, t_da, t_qcs, t_qca, t_qhs, t_qha, t_q
#' @param input_vartype list of input parameter variable types with named elements: alpha, omega_c, omega_h, omega_q, rho_s, rho_a, R, kappa, eta, nu, t_ds, t_da, t_qcs, t_qca, t_qhs, t_qha, t_q. Currently supported character options are "proportion", "R_overdispersed", "uniform", "poisson", "fixed"
#'
#' @return data frame
#' @export
#'
get_stochastic_input <- function(nsimulations, input_list, input_vartype) {
  allvars <- c("alpha", "omega_c", "omega_h", "omega_q", "rho_s", "rho_a", "R", "kappa", "eta", "nu", "t_ds", "t_da", "t_qcs", "t_qca", "t_qhs", "t_qha", "t_q")
  varnames <- names(input_list)
  if (!all(allvars %in% varnames)) {
    stop("You have not specified all of the necessary parameters. Please provide explicit values for: alpha, omega_c, omega_h, omega_q, rho_s, rho_a, R, kappa, eta, nu, t_ds, t_da, t_qcs, t_qca, t_qhs, t_qha, t_q.")
  }
  if (length(input_vartype) < length(allvars)) {
    warning("You have not provided an input_vartype for all parameters. Unspecified input_vartypes will default to a fixed value.")
  }

  param_list <- lapply(1:length(input_list), function(i) {
    varname <- names(input_list)[i]
    varvalue <- input_list[[varname]]
    vartype <- ifelse(!is.null(input_vartype[[varname]]), input_vartype[[varname]], "fixed")

    if (vartype == "proportion") {
      warning(glue::glue("Assuming that {varname} is the probability parameter for a binomial distribution, we examined the number of successes in 1000 events and divided the random draw by 1000 to get a stochastic parameter value for {varname} centered around {varvalue}."))
      rc <- stats::rbinom(nsimulations, 1000, prob = varvalue) / 1000
    } else if (vartype == "R_overdispersed") {

      ## set k to a value between .04 and .2 (https://cmmid.github.io/topics/covid19/overdispersion-from-outbreaksize.html) ## May want to make this tunable in the future (instead of fixed at .12)
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

  ## check that user did not mis-specify proportions
  proportions_vars <- c("alpha", "omega_c", "omega_h", "omega_q", "rho_s", "rho_a", "eta")
  check_prop <- dplyr::select(rc, !!proportions_vars)
  if (any(check_prop > 1) | any(check_prop < 0) | any(is.na(check_prop))) {
    props <- paste(proportions_vars, collapse = ", ")
    stop(glue::glue("You have mis-specified one of the parameters that is supposed to be a proportion: {props}. Please check input_list and input_vartype."))
  }

  ## check that user did not mis-specify non-negative values
  nonneg_vars <- c("R", "kappa", "nu", "t_ds", "t_da", "t_qcs", "t_qhs", "t_qha", "t_q")
  check_nonneg <- dplyr::select(rc, !!nonneg_vars)
  if (any(check_nonneg < 0)) {
    nonnegs <- paste(check_nonneg, collaps = ", ")
    stop(glue::glue("You have mis-specified one of the parameters that is supposed to be non-negative: {nonnegs}. Please check input_list and input_vartype."))
  }

  return(rc)
}
