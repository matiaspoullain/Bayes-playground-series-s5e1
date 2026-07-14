source("src/features.R")
source("src/model_spec.R")
source("src/metrics.R")
library(brms)
library(data.table)

holidays <- fread("data/processed/holidays.csv")

train_raw <- fread("data/raw/train.csv")
train_raw <- train_raw[!is.na(num_sold)]
train_raw <- add_calendar_features(train_raw)
train_raw <- add_holiday_feature(train_raw, holidays)

fit_data <- train_raw[country %in% c("Canada", "Kenya") & year <= 2012]
holdout_data <- train_raw[country %in% c("Canada", "Kenya") & year == 2013]

fit <- brm(
  formula = sticker_sales_formula(),
  data = fit_data,
  family = sticker_sales_family(),
  prior = sticker_sales_priors(),
  backend = "cmdstanr",
  chains = 4, cores = 4, iter = 5000, warmup = 100,
  seed = 1,
  silent = 0
)

print(summary(fit))

pred_draws <- posterior_predict(fit, newdata = holdout_data, allow_new_levels = TRUE)
pred_median <- apply(pred_draws, 2, median)

holdout_mape <- mape(holdout_data$num_sold, pred_median)
cat("1-year holdout MAPE:", holdout_mape, "\n")

dir.create("models", showWarnings = FALSE)
saveRDS(fit, "models/fit_holdout_1yr.rds")