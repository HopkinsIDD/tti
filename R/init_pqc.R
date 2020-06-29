#' Create a named vector to use as a Passive-Quarantine-Community input
#'
#' Creates a named vector to use as the `pqc` or `init` parameter in tti
#' functions.
#'
#' @param Ps Proportion of population that are symptomatic and passively
#'   detected
#' @param Pa Proportion of population that are asymptomatic and passively
#'   detected
#' @param Qcps Proportion of population that were infected by symptomatic
#'   community contacts and are quarantined
#' @param Qhps Proportion of population that were infected by symptomatic
#'   household contacts and are quarantined
#' @param Qcpa Proportion of population that were infected by asymptomatic
#'   community contacts and are quarantined
#' @param Qhpa Proportion of population that were infected by asymptomatic
#'   household contacts and are quarantined
#' @param Qq Proportion of population that were infected by quarantined
#'   contacts and are quarantined
#' @param Cs Proportion of population that are symptomatic and undetected
#'   in the community
#' @param Ca Proportion of population that are asymptomatic and undetected
#'   in the community
#'
#' @return Named vector.
#' @export

init_pqc <- function(Ps = 0, Pa = 0, Qcps = 0, Qhps = 0, Qcpa = 0, Qhpa = 0,
                     Qq = 0, Cs = 0.8, Ca = 0.2) {
  c(
    Ps = Ps, Pa = Pa, Qcps = Qcps, Qhps = Qhps, Qcpa = Qcpa, Qhpa = Qhpa,
    Qq = Qq, Cs = Cs, Ca = Ca
  )
}
