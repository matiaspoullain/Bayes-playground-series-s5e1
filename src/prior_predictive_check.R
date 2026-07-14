source("src/features.R")
source("src/model_spec.R")
library(brms)
library(data.table)

train <- fread("data/raw/train.csv")
train <- train[!is.na(num_sold)]
train <- add_calendar_features(train)

holidays <- fread("data/processed/holidays.csv")
train <- add_holiday_feature(train, holidays)

fit_prior <- brm(
  formula = sticker_sales_formula(),
  data = train,
  family = sticker_sales_family(),
  prior = sticker_sales_priors(),
  sample_prior = "only",
  backend = "cmdstanr",
  chains = 2, iter = 500, warmup = 250,
  seed = 1
)

preds <- posterior_predict(fit_prior, ndraws = 100)
cat("Prior predictive range: [", min(preds), ",", max(preds), "]\n")
cat("Prior predictive median:", median(preds), "\n")
