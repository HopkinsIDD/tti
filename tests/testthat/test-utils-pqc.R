test_that("DQC only takes a vector that sums to 1", {
  dqc_bad <- c(
    Ds = 1, Da = 0, Qcds = 0, Qhds = 0, Qcda = 0, Qhda = 0, Qq = 0,
    Cs = 0.8, Ca = 0.2
  )
  expect_error(
    check_dqc(dqc_bad),
    "Parameter `dqc_bad` must be a named vector that sums to 1."
  )
})

test_that("DQC must be named vector", {
  dqc_bad <- c(1, 0, 0)
  expect_error(check_dqc(dqc_bad), "Parameter `dqc_bad` must be a named vector")
})

test_that("DQC can be misordered, and will be reordered", {
  dqc_misordered <- c(
    Da = 0, Ds = 0, Qcds = 0, Qhds = 0, Qcda = 0, Qhda = 0, Qq = 0, Cs = 0.8,
    Ca = 0.2
  )
  dqc_reordered <- check_dqc(dqc_misordered)
  expect_identical(names(dqc_misordered), c(
    "Da", "Ds", "Qcds", "Qhds", "Qcda",
    "Qhda", "Qq", "Cs", "Ca"
  ))
  expect_identical(names(dqc_reordered), c(
    "Ds", "Da", "Qcds", "Qhds", "Qcda",
    "Qhda", "Qq", "Cs", "Ca"
  ))
})

test_that("DQC elements must be positive", {
  dqc <- c(
    Ds = -1, Da = 2, Qcds = 0, Qhds = 0, Qcda = 0,
    Qhda = 0, Qq = 0, Cs = 0, Ca = 0
  )
  expect_error(check_dqc(dqc), "All elements of `dqc` must be positive")
})

test_that("DQC can't be missing", {
  expect_error(check_dqc(), "Parameter `` is missing with no default")
})
