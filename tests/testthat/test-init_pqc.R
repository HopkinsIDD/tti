test_that("init_dqc works", {
  expect_equal(
    init_dqc(1, 2, 3, 4, 5, 6, 7, 8, 9),
    c(
      Ds = 1, Da = 2, Qcds = 3, Qhds = 4, Qcda = 5, Qhda = 6, Qq = 7,
      Cs = 8, Ca = 9
    )
  )
})
