library(data.table)

YEAR_REFERENCE <- 2013

add_calendar_features <- function(dt) {
  dt <- copy(as.data.table(dt))
  dates <- as.Date(dt$date)

  weekday_labels <- c(
    "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"
  )

  dt[, year := as.numeric(format(dates, "%Y"))]
  dt[, year_centered := year - YEAR_REFERENCE]
  dt[, day_of_year := as.numeric(format(dates, "%j"))]
  dt[, weekday := factor(
    weekday_labels[as.integer(format(dates, "%u"))],
    levels = weekday_labels
  )]

  dt
}

add_holiday_feature <- function(dt, holidays) {
  dt <- copy(as.data.table(dt))
  holidays <- as.data.table(holidays)

  dt[, is_holiday := 0L]
  dt[holidays, on = .(date, country), is_holiday := 1L]

  dt
}
