test_that("cat_dqc works", {
  expect_equal(
    cat_dqc(),
    "c(Ds = 0, Da = 0, Qcds = 0, Qhds = 0, Qcda = 0, Qhda = 0, Qq = 0, Cs = 0.8, Ca = 0.2)"
  )
})
