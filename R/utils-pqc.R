check_pqc <- function(pqc) {
  categories <- c("Ps", "Pa", "Qcps", "Qhps", "Qcpa", "Qhpa", "Qq", "Cs", "Ca")

  if (missing(pqc)) {
    stop_glue(
      "Parameter `{deparse(substitute(pqc))}` is missing with no default. ",
      "It must be a named vector with the following names:
  * {glue::glue_collapse({categories}, sep = ', ')}"
    )
  }
  if (!all(utils::hasName(pqc, categories))) {
    stop_glue(
      "Parameter `{deparse(substitute(pqc))}` must be a named vector with ",
      "the following names:
  * {glue::glue_collapse({categories}, sep = ', ')}"
    )
  }

  if (not_equal(sum(pqc), 1)) {
    stop_glue(
      "Parameter `{deparse(substitute(pqc))}` must be a named vector that ",
      "sums to 1.
      * Your `{deparse(substitute(pqc))}` summed to: {sum(pqc)}"
    )
  }

  pqc <- pqc[categories]
}
