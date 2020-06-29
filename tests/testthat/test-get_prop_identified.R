test_that("get_prop_identified works", {
  ps <- 0.1
  pa <- 0.2
  qcps <- 0.15
  qhps <- 0.25
  qcpa <- 0.15
  qhpa <- 0.05
  qq <- 0.1
  pqc <- c(Ps = ps, Pa = pa, Qcps = qcps, Qhps = qhps, Qcpa = qcpa,
           Qhpa = qhpa, Qq = qq, Cs = 0, Ca = 0)
  iden <- get_prop_identified(pqc)
  expect_equal(sum(ps, pa, qcps, qhps, qcpa, qhpa, qq), iden)
})

