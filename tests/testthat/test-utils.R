test_that("is_probability works", {
  expect_true(is_probability(0))
  expect_true(is_probability(0.5))
  expect_true(is_probability(1))
  expect_error(is_probability(-0.1))
  expect_error(is_probability(1.1))
  expect_error(is_probability("a"))
})

test_that("is_positive works", {
  expect_true(is_positive(0))
  expect_true(is_positive(100))
  expect_error(is_positive(-1))
  expect_error(is_positive("a"))
})
