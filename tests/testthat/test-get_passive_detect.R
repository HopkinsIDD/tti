test_that("get_passive_detect works", {
  expect_equal(get_passive_detect(10, 0.1, 100, 0.2), 0.05)
})
