#' @param init Named vector of initial values indicating proportion infected in
#'    each detected-community-quarantine category. All elements of the vector
#'    must sum to 1. Must include the names:
#'    `Ds`, `Da`, `Qcds`, `Qhds`, `Qcda`, `Qhda`, `Qq`, `Cs`, `Ca`.
#'    Default: `c(Ds = 0, Da = 0, Qcds = 0, Qhds = 0, Qcda = 0, Qhda = 0, Qq = 0, Cs = 0.8, Ca = 0.2)`
#'    See [`init_dqc()`] to generate this vector.
