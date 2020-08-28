#' Calculate the Detected-Quarantine-Community vector stochastically
#'
#' This function iterates through the Detected-Quarantine-Community (DQC) vector
#'   stochastically for a set number of generations.
#'
#' @template init
#' @param n_generation Numeric positive value. Number of iterations to run.
#'    Default: 10.
#' @param ... Optional parameters to pass to [`get_intermediate_inf`] and
#'   [`get_detect_mat()`]. See [`get_intermediate_inf`] and [`get_detect_mat()`]
#'   for defaults.
#'
#' @return A named vector, the Detected-Quarantine-Community proportions after
#'   some number of generations
#' @export
#'
get_dqc_stoch <- function(init = c(
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
                          n_generation = 10,
                          ...) {
  init <- check_dqc(init)
  is_positive(n_generation)

  dots <- list(...)

  detect_args <- dots[names(dots) %in%
    c("alpha", "omega_c", "omega_h", "omega_q", "rho_s", "rho_a")]
  
  infect_args <- dots[names(dots) %in%
    c(
      "alpha", "R", "kappa", "eta", "nu", "t_ds", "t_da", "t_qcs", "t_qca",
      "t_qhs", "t_qha", "t_incubation", "offset", "shape", "rate", "theta",
      "n_inf"
    )]
  
  detect <- do.call(get_detect_mat, detect_args)
  
  dqc <- init
  categories <- c("Ds", "Da", "Qcds", "Qhds", "Qcda", "Qhda", "Qq", "Cs", "Ca")
  
  for(i in 1:n_generation){
    
    int <- do.call(get_intermediate_inf, c(quote(dqc), infect_args))
                   
    dqc <- rowSums(sapply(1:length(int), function(x, det=detect) stats::rmultinom(1, int[x], det[x,])))
    
    dqc <- stats::setNames(dqc / sum(dqc), categories)
  }

  dqc
  
}


#' Calculate the intermediate infection states
#'
#' This low-level function stochastically derives the intermediate infections derived
#' from a DQC matrix in one generation
#'
#' @param dqc Current values for DQC compartments. All elements of this vector
#'   should sum to 1.
#' @param n_inf Number of infections (or effective population size of infected 
#'   individuals). Default 1000.
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
#' @param theta Non-negative numeric value. Shape parameter for overdispersion 
#'    in R. Default 0.2.
#' @return The intermediate infection states
#' @export
#'
get_intermediate_inf <- function(dqc, n_inf = 1000, alpha = 0.2, R = 2.5, kappa = 0.5, eta = 0.5, nu = 4,
                                 t_ds = 3, t_da = 3, t_qcs = 3, t_qca = 3, t_qhs = 3,
                                 t_qha = 3, t_q = 3, t_incubation = 5.5, offset = -2.31,
                                 shape = 1.65, rate = 0.5, theta=0.2) {
  
  dqc <- check_dqc(dqc)
  dqc <- round(dqc * n_inf)
  
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
  is_positive(theta)
  
  gamma_ds <- get_prop_infect_time(
    t_ds,
    offset = offset, shape = shape, rate = rate
  )
  gamma_da <- get_prop_infect_time(
    t_da,
    offset = offset, shape = shape, rate = rate
  )
  
  second_gen_shape <- shape + (shape + (t_incubation + offset) * rate)
  
  gamma_qcs <- get_prop_infect_time(
    t_qcs,
    shape = second_gen_shape, rate = rate, offset = 0
  )
  gamma_qca <- get_prop_infect_time(
    t_qca,
    shape = second_gen_shape, rate = rate, offset = 0
  )
  gamma_qhs <- get_prop_infect_time(
    t_qhs,
    shape = second_gen_shape, rate = rate, offset = 0
  )
  gamma_qha <- get_prop_infect_time(
    t_qha,
    shape = second_gen_shape, rate = rate, offset = 0
  )
  gamma_q <- get_prop_infect_time(
    t_q,
    shape = second_gen_shape, rate = rate, offset = 0
  )
  
  R_s <- R / ((alpha * kappa) - alpha + 1)
  R_a <- (kappa * R) / ((alpha * kappa) - alpha + 1)
  pr_h <- (eta * nu) / ((eta * nu) - eta + 1)

  # infections derived from Ps
  Ids <- stats::rnbinom(dqc["Ds"], mu=gamma_ds*R_s, size=theta)
  Idsh <- sum(stats::rbinom(length(Ids), Ids, pr_h))
  Idsc <- sum(Ids) - Idsh
  
  # infections derive from Pa
  Ida <- stats::rnbinom(dqc["Da"], mu=gamma_da*R_a, size=theta)
  Idah <- sum(stats::rbinom(length(Ida), Ida, pr_h))
  Idac <- sum(Ida) - Idah
  
  # infections derived from Qhs, Qha, Qcs, Qca, Qq
  Iq <- sum(stats::rnbinom(dqc["Qcds"], mu=gamma_qcs*R, size=theta)) +
          sum(stats::rnbinom(dqc["Qhds"], mu=gamma_qhs*R, size=theta)) +
          sum(stats::rnbinom(dqc["Qcda"], mu=gamma_qca*R, size=theta)) +
          sum(stats::rnbinom(dqc["Qhda"], mu=gamma_qha*R, size=theta)) +
          sum(stats::rnbinom(dqc["Qq"], mu=gamma_q*R, size=theta)) 

  # infections derived from Cs
  Ic <- sum(stats::rnbinom(dqc["Cs"], mu=R_s, size=theta)) +
          sum(stats::rnbinom(dqc["Ca"], mu=R_a, size=theta))
  
  return(int = c(Idsc=Idsc, Idsh=Idsh, Idac=Idac, Idah=Idah, Iq=Iq, Ic=Ic))
}


