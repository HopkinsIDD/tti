test_that("cat_pqc works", {
  expect_equal(
    cat_pqc(),
    "c(Ps = 0, Pa = 0, Qcps = 0, Qhps = 0, Qcpa = 0, Qhpa = 0, Qq = 0, Cs = 0.8, Ca = 0.2)"
  )
})
