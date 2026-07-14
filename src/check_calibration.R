library(brms)
source("src/current_fit.R")

fit <- readRDS(CURRENT_FIT_PATH)
holdout_data <- current_split$holdout_data

pred_draws <- posterior_predict(fit, newdata = holdout_data, allow_new_levels = TRUE)

lower_50 <- apply(pred_draws, 2, quantile, probs = 0.25)
upper_50 <- apply(pred_draws, 2, quantile, probs = 0.75)
lower_90 <- apply(pred_draws, 2, quantile, probs = 0.05)
upper_90 <- apply(pred_draws, 2, quantile, probs = 0.95)

coverage_50 <- mean(holdout_data$num_sold >= lower_50 & holdout_data$num_sold <= upper_50)
coverage_90 <- mean(holdout_data$num_sold >= lower_90 & holdout_data$num_sold <= upper_90)

cat("Empirical 50% interval coverage:", coverage_50, "(nominal: 0.50)\n")
cat("Empirical 90% interval coverage:", coverage_90, "(nominal: 0.90)\n")
