#' @param init Named vector of initial values indicating proportion infected in
#'    each passive-community-quarantine category. All elements of the vector
#'    must sum to 1. Must include the names:
#'    `Ps`, `Pa`, `Qcps`, `Qhps`, `Qcpa`, `Qhpa`, `Qq`, `Cs`, `Ca`.
#'    Default: `c(Ps = 0, Pa = 0, Qcps = 0, Qhps = 0, Qcpa = 0, Qhpa = 0, Qq = 0, Cs = 0.8, Ca = 0.2)`
#'    See [`init_pqc()`] to generate this vector.
