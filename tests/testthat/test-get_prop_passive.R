test_that("get_prop_passive works", {
  ps <- 0.1
  pa <- 0.9
  pqc <- c(Ps = ps, Pa = pa, Qcps = 0, Qhps = 0, Qcpa = 0, Qhpa = 0, Qq = 0,
           Cs = 0, Ca = 0)
  pass <- get_prop_passive(pqc)
  expect_equal(ps + pa, pass)

  ps <- 0.4
  pa <- 0.2
  pqc <- c(Ps = ps, Pa = pa, Qcps = 0, Qhps = 0, Qcpa = 0, Qhpa = 0, Qq = 0,
           Cs = 0.4, Ca = 0)
  pass <- get_prop_passive(pqc)
  expect_equal(ps + pa, pass)

  ps <- 0
  pa <- 0
  pqc <- c(Ps = ps, Pa = pa, Qcps = 0, Qhps = 0, Qcpa = 0, Qhpa = 0, Qq = 0,
           Cs = 0.9, Ca = 0.1)
  pass <- get_prop_passive(pqc)
  expect_equal(ps + pa, pass)
})
