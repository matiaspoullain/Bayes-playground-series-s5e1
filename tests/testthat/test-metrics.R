source(file.path("..", "..", "src", "metrics.R"))

test_that("mape computes mean absolute percentage error", {
  actual <- c(100, 200, 300)
  predicted <- c(110, 180, 300)

  expect_equal(mape(actual, predicted), 6.666667, tolerance = 1e-6)
})

test_that("mape is zero for perfect predictions", {
  actual <- c(50, 75, 90)
  expect_equal(mape(actual, actual), 0)
})
