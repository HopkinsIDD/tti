test_that("Recursive function gives proper time points", {
  d <- get_proportions_df(3)
  expect_equal(unique(d$t), c(1, 2, 3))
})

test_that("Proportions sum to 1", {
  d <- get_proportions_df(3)
  sum(d$prop_infected[d$t == 1])
  expect_equal(sum(d$prop_infected[d$t == 1]), 1)
  expect_equal(sum(d$prop_infected[d$t == 2]), 1)
  expect_equal(sum(d$prop_infected[d$t == 3]), 1)
})
