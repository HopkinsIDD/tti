test_that("get_r_effective_df works", {
  r <- get_r_effective_df()
  expect_equal(nrow(r), 1)
  expect_equal(ncol(r), 18)
  expect_equal(round(r$r_effective, 3), 2.362)
})
