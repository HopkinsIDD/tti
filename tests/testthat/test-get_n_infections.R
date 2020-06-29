test_that("get_n_infections works", {
  expect_equal(get_n_infections(100, 0.2), 20)
})

test_that("get_n_infections errors if not proportion", {
  expect_error(get_n_infections(100, 1.2))
})

test_that("get_n_infections errors if negative number of rests", {
  expect_error(get_n_infections(-1, 0.2))
})

