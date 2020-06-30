test_that("init_pqc works", {
  expect_equal(init_pqc(1, 2, 3, 4, 5, 6, 7, 8, 9),
               c(Ps = 1, Pa = 2, Qcps = 3, Qhps = 4, Qcpa = 5, Qhpa = 6, Qq = 7,
                 Cs = 8, Ca = 9)
  )
})
