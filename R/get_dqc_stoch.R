#' Calculate the Detected-Quarantine-Community vector stochastically
#'
#' This function iterates through the Detected-Quarantine-Community (DQC) vector
#'   stochastically for a set number of generations.
#'
#' @template init
#' @param n_generation Numeric positive value. Number of iterations to run.
#'    Default: 10.
#' @param ... Optional parameters to pass to [`get_infect_mat`], 
#'   [`get_detect_mat()`], and [`get_intermediate_inf`]. See [`get_infect_mat`],
#'   [`get_detect_mat()`], and [`get_intermediate_inf`] for defaults.
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
      "t_qhs", "t_qha", "t_incubation", "offset", "shape", "rate"
    )]
  
  stoch_args <- dots[names(dots) %in% c("theta", "n_inf")]
  
  detect <- do.call(get_detect_mat, detect_args)
  infect <- do.call(get_infect_mat, infect_args)
  stoch_args$infect <- infect # there has to be a better way to do this?
  
  dqc <- init
  categories <- c("Ds", "Da", "Qcds", "Qhds", "Qcda", "Qhda", "Qq", "Cs", "Ca")
  
  for(i in 1:n_generation){
    
    stoch_args$dqc <- dqc
    
    int <- do.call(get_intermediate_inf, stoch_args)
                   
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
#' @param infect INFECT matrix returned by [`get_infect_mat`]
#' @param n_inf Number of infections (or effective population size of infected 
#'   individuals). Default 1000.
#' @param theta Non-negative numeric value. Shape parameter for overdispersion 
#'    in R. Default 0.2.
#' @return Named vector of intermediate infection states
#' @export
#'
get_intermediate_inf <- function(dqc, infect, n_inf = 1000, theta = 0.2) {
  
  dqc <- check_dqc(dqc)
  dqc <- round(dqc * n_inf)
 
  # infections derived from Ps
  Idsc <- sum(stats::rnbinom(dqc["Ds"], mu=infect[1,1], size=theta))
  Idsh <- sum(stats::rnbinom(dqc["Ds"], mu=infect[1,2], size=theta))
  
  # infections derive from Pa
  Idac <- sum(stats::rnbinom(dqc["Da"], mu=infect[2,3], size=theta))
  Idah <- sum(stats::rnbinom(dqc["Da"], mu=infect[2,4], size=theta))
  
  # infections derived from Qhs, Qha, Qcs, Qca, Qq
  Iq <- sum(stats::rnbinom(dqc["Qcds"], mu=infect[3,5], size=theta)) +
          sum(stats::rnbinom(dqc["Qhds"], mu=infect[4,5], size=theta)) +
          sum(stats::rnbinom(dqc["Qcda"], mu=infect[5,5], size=theta)) +
          sum(stats::rnbinom(dqc["Qhda"], mu=infect[6,5], size=theta)) +
          sum(stats::rnbinom(dqc["Qq"], mu=infect[7,5], size=theta)) 

  # infections derived from Cs
  Ic <- sum(stats::rnbinom(dqc["Cs"], mu=infect[8,6], size=theta)) +
          sum(stats::rnbinom(dqc["Ca"], mu=infect[9,6], size=theta))
  
  return(int = c(Idsc=Idsc, Idsh=Idsh, Idac=Idac, Idah=Idah, Iq=Iq, Ic=Ic))
}


