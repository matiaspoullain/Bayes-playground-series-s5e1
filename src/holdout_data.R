source("src/features.R")
library(data.table)

build_holdout_split <- function(fit_year_max, holdout_years, countries = NULL) {
  holidays <- fread("data/processed/holidays.csv")

  train_raw <- fread("data/raw/train.csv")
  train_raw <- train_raw[!is.na(num_sold)]
  train_raw <- add_calendar_features(train_raw)
  train_raw <- add_holiday_feature(train_raw, holidays)

  if (!is.null(countries)) {
    train_raw <- train_raw[country %in% countries]
  }

  list(
    fit_data = train_raw[year <= fit_year_max],
    holdout_data = train_raw[year %in% holdout_years]
  )
}
