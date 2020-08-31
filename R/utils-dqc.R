check_dqc <- function(dqc) {
  categories <- c("Ds", "Da", "Qcds", "Qhds", "Qcda", "Qhda", "Qq", "Cs", "Ca")

  if (missing(dqc)) {
    stop_glue(
      "Parameter `{deparse(substitute(dqc))}` is missing with no default. ",
      "It must be a named vector with the following names:
  * {glue::glue_collapse({categories}, sep = ', ')}"
    )
  }
  if (!all(utils::hasName(dqc, categories))) {
    stop_glue(
      "Parameter `{deparse(substitute(dqc))}` must be a named vector with ",
      "the following names:
  * {glue::glue_collapse({categories}, sep = ', ')}"
    )
  }

  if (not_equal(sum(dqc), 1)) {
    stop_glue(
      "Parameter `{deparse(substitute(dqc))}` must be a named vector that ",
      "sums to 1.
      * Your `{deparse(substitute(dqc))}` summed to: {sum(dqc)}"
    )
  }

  if (!all(dqc >= 0)) {
    stop_glue(
      "All elements of `{deparse(substitute(dqc))}` must be positive"
    )
  }

  dqc <- dqc[categories]
}
