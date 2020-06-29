test_that("PQC only takes a vector that sums to 1", {
  pqc_bad <- c(
    Ps = 1, Pa = 0, Qcps = 0, Qhps = 0, Qcpa = 0, Qhpa = 0, Qq = 0,
    Cs = 0.8, Ca = 0.2
  )
  expect_error(check_pqc(pqc_bad))
})

test_that("PQC must be named vector", {
  pqc_bad <- c(1, 0, 0)
  expect_error(check_pqc(pqc_bad))
})

test_that("PQC can be misordered, and will be reordered", {
  pqc_misordered <- c(
    Pa = 0, Ps = 0, Qcps = 0, Qhps = 0, Qcpa = 0, Qhpa = 0, Qq = 0, Cs = 0.8,
    Ca = 0.2
  )
  pqc_reordered <- check_pqc(pqc_misordered)
  expect_identical(names(pqc_misordered), c(
    "Pa", "Ps", "Qcps", "Qhps", "Qcpa",
    "Qhpa", "Qq", "Cs", "Ca"
  ))
  expect_identical(names(pqc_reordered), c(
    "Ps", "Pa", "Qcps", "Qhps", "Qcpa",
    "Qhpa", "Qq", "Cs", "Ca"
  ))
})