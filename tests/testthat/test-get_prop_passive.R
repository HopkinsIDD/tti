test_that("get_prop_isolated works", {
  ds <- 0.1
  da <- 0.9
  dqc <- c(
    Ds = ds, Da = da, Qcds = 0, Qhds = 0, Qcda = 0, Qhda = 0, Qq = 0,
    Cs = 0, Ca = 0
  )
  pass <- get_prop_isolated(dqc)
  expect_equal(ds + da, pass)

  ds <- 0.4
  da <- 0.2
  dqc <- c(
    Ds = ds, Da = da, Qcds = 0, Qhds = 0, Qcda = 0, Qhda = 0, Qq = 0,
    Cs = 0.4, Ca = 0
  )
  pass <- get_prop_isolated(dqc)
  expect_equal(ds + da, pass)

  ds <- 0
  da <- 0
  dqc <- c(
    Ds = ds, Da = da, Qcds = 0, Qhds = 0, Qcda = 0, Qhda = 0, Qq = 0,
    Cs = 0.9, Ca = 0.1
  )
  pass <- get_prop_isolated(dqc)
  expect_equal(ds + da, pass)
})
