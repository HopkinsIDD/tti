#' Create a string to use as a Detected-Quarantine-Community vector input
#'
#' Creates a string formatted as a named vector to use as the `dqc` or `init`
#' parameter in tti functions.
#'
#' @param Ds Proportion of population that are symptomatic, detected and
#'   isolated
#' @param Da Proportion of population that are asymptomatic, detected and
#'   isolated
#' @param Qcds Proportion of population that are infected community contacts
#'   of symptomatic, isolated infections and are quarantined
#' @param Qhds Proportion of population that are infected household contacts
#'   of symptomatic, isolated infections and are quarantined
#' @param Qcda Proportion of population that are infected community contacts
#'   of asymptomatic, isolated infections and are quarantined
#' @param Qhda Proportion of population that are infected household contacts
#'   of symptomatic, isolated infections and are quarantined
#' @param Qq Proportion of population that were infected by quarantined
#'   contacts and are quarantined
#' @param Cs Proportion of population that are symptomatic and undetected
#'   in the community
#' @param Ca Proportion of population that are asymptomatic and undetected
#'   in the community
#'
#' @return Character string.

cat_dqc <- function(Ds = 0, Da = 0, Qcds = 0, Qhds = 0, Qcda = 0, Qhda = 0,
                    Qq = 0, Cs = 0.8, Ca = 0.2) {
  glue::glue(
    "c(Ds = {Ds}, Da = {Da}, Qcds = {Qcds}, Qhds = {Qhds}, ",
    "Qcda = {Qcda}, Qhda = {Qhda}, Qq = {Qq}, Cs = {Cs}, Ca = {Ca})"
  )
}
