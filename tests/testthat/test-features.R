library(data.table)
source(file.path("..", "..", "src", "features.R"))

test_that("add_calendar_features extracts year, day_of_year, weekday", {
  dt <- data.table(date = c("2024-01-01", "2024-07-04", "2024-12-31"))
  result <- add_calendar_features(dt)

  expect_equal(result$year, c(2024, 2024, 2024))
  expect_equal(result$year_centered, c(11, 11, 11))
  expect_equal(result$day_of_year, c(1, 186, 366))
  expect_equal(
    as.character(result$weekday),
    c("Monday", "Thursday", "Tuesday")
  )
})

test_that("add_calendar_features weekday is a factor with fixed level order", {
  dt <- data.table(date = c("2024-01-01"))
  result <- add_calendar_features(dt)
  expect_equal(
    levels(result$weekday),
    c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  )
})

test_that("add_holiday_feature flags matching date/country pairs", {
  dt <- data.table(
    date = c("2024-01-01", "2024-01-02", "2024-01-01"),
    country = c("Canada", "Canada", "Finland")
  )
  holidays <- data.table(date = c("2024-01-01"), country = c("Canada"))

  result <- add_holiday_feature(dt, holidays)

  expect_equal(result$is_holiday, c(1, 0, 0))
})
