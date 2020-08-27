test_that("get_prop_identified works", {
  ds <- 0.1
  da <- 0.2
  qcds <- 0.15
  qhds <- 0.25
  qcda <- 0.15
  qhda <- 0.05
  qq <- 0.1
  dqc <- c(
    Ds = ds, Da = da, Qcds = qcds, Qhds = qhds, Qcda = qcda,
    Qhda = qhda, Qq = qq, Cs = 0, Ca = 0
  )
  iden <- get_prop_identified(dqc)
  expect_equal(sum(ds, da, qcds, qhds, qcda, qhda, qq), iden)
})
