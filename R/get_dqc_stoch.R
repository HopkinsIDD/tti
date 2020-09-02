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
  
  categories <- c("Ds", "Da", "Qcds", "Qhds", "Qcda", "Qhda", "Qq", "Cs", "Ca")
  stoch_args$dqc <- init
  
  int <- do.call(get_intermediate_inf, stoch_args)
  while(sum(int)==0){
    int <- do.call(get_intermediate_inf, stoch_args)
  }
  
  dqc <- rowSums(sapply(1:length(int), function(x, det=detect) stats::rmultinom(1, int[x], det[x,])))
  dqc <- stats::setNames(dqc / sum(dqc), categories)
  
  i=1
  
  while(i<n_generation){
    
    stoch_args$dqc <- dqc
    
    int_new <- do.call(get_intermediate_inf, stoch_args)
    if(sum(int_new>0)){
      int <- int_new
    }
    
    dqc <- rowSums(sapply(1:length(int), function(x, det=detect) stats::rmultinom(1, int[x], det[x,])))
    
    dqc <- stats::setNames(dqc / sum(dqc), categories)
    
    i=i+1
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
#' @return Vector of intermediate infection states
#' @export
#'
get_intermediate_inf <- function(dqc, infect, n_inf = 1000, theta = 0.2) {
  
  dqc <- check_dqc(dqc)
  dqc <- round(dqc * n_inf)
 
  tmp <- rep(0, ncol(infect))
  
  for(i in 1:length(dqc)){
    tmp <- tmp +
            rowSums(matrix(stats::rnbinom(dqc[i]*ncol(infect), mu=infect[i,], size=theta), nrow=ncol(infect)))
  }
  
  return(tmp)
}


