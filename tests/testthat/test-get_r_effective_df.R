test_that("get_r_effective_df works", {
  r <- get_r_effective_df()
  expect_equal(nrow(r), 1)
  expect_equal(ncol(r), 19)
  expect_equal(round(r$r_effective, 3), 2.362)
})

test_that("get_r_effective_df works with stochastic arguments", {
  r <- get_r_effective_df(stoch = TRUE, n_iter = 5, theta = 0.1, n_inf = 100)
  expect_equal(nrow(r), 5)
  expect_equal(ncol(r), 22)
})

test_that("get_r_effective_df works for deterministic models with incorrectly specified arguments", {
  r <- get_r_effective_df(stoch = FALSE, n_iter = 5, theta = 0.1, n_inf = 100)
  expect_equal(nrow(r), 1)
  expect_equal(ncol(r), 19)
})
