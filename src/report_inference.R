library(brms)
library(tidybayes)
library(ggdist)
library(dplyr)
library(ggplot2)
source("src/current_fit.R")

fit <- readRDS(CURRENT_FIT_PATH)
weekday_levels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# --- Table 1 + Figure 1: fixed effects (weekday/holiday), full posterior + 50%/90% HPDI ---

fixed_effect_draws <- fit %>%
  tidybayes::gather_draws(`b_.*`, regex = TRUE) %>%
  dplyr::filter(!.variable %in% c("b_Intercept", "b_year_centered"))

fixed_effects_table <- fixed_effect_draws %>%
  ggdist::median_hdi(.value, .width = 0.9) %>%
  dplyr::rename(effect = .variable, estimate = .value, hdi_lower = .lower, hdi_upper = .upper)
write.csv(fixed_effects_table, file.path(OUTPUT_DIR, "fixed_effects_summary.csv"), row.names = FALSE)
print(fixed_effects_table)

ggplot(fixed_effect_draws, aes(y = .variable, x = .value)) +
  ggdist::stat_halfeye(point_interval = "median_hdi", .width = c(0.5, 0.9)) +
  labs(
    x = "Effect (log scale)", y = NULL,
    title = "Posterior distributions of weekday/holiday fixed effects (median + 50%/90% HPDI)"
  ) +
  theme_minimal()
ggsave(file.path(OUTPUT_DIR, "fixed_effects_hpdi.png"), width = 9, height = 6)

# --- Figure 2: weekday x is_holiday conditional effect ---
# is_holiday is stored as numeric 0/1, so conditional_effects would otherwise treat it as a
# continuous moderator (plotting mean +/- 1 SD instead of the actual holiday/non-holiday levels).

ce_weekday <- conditional_effects(
  fit,
  effects = "weekday:is_holiday",
  int_conditions = list(is_holiday = c(0, 1))
)
ggsave(file.path(OUTPUT_DIR, "conditional_effects_weekday_holiday.png"), plot(ce_weekday)[[1]], width = 9, height = 6)

# --- Figure 3: annual seasonality conditional effect (response scale) ---

ce_season <- conditional_effects(fit, effects = "day_of_year")
ggsave(file.path(OUTPUT_DIR, "conditional_effects_seasonality.png"), plot(ce_season)[[1]], width = 9, height = 6)

# --- Figure 3b: annual seasonality smooth term (link scale, classic GAM partial-effect plot) ---
# s(day_of_year, bs = "cc") is the model's only smooth term; weekday is a categorical
# fixed effect (Figure 2), not a spline, so there is no second smooth curve to plot here.

smooth_season <- conditional_smooths(fit)
ggsave(file.path(OUTPUT_DIR, "smooth_seasonality.png"), plot(smooth_season)[[1]], width = 9, height = 6)

# --- Table 2 + Figure 4: group-level intercepts (country/store/product), ranked forest plot ---
# Note: this fit was trained on Canada + Kenya only, so the "country" facet has 2 levels here.

ranef_country <- as.data.frame(ranef(fit)$country[, , "Intercept"])
ranef_country$group <- rownames(ranef_country)
ranef_country$level <- "country"

ranef_store <- as.data.frame(ranef(fit)$store[, , "Intercept"])
ranef_store$group <- rownames(ranef_store)
ranef_store$level <- "store"

ranef_product <- as.data.frame(ranef(fit)$product[, , "Intercept"])
ranef_product$group <- rownames(ranef_product)
ranef_product$level <- "product"

group_effects <- rbind(ranef_country, ranef_store, ranef_product)
write.csv(group_effects, file.path(OUTPUT_DIR, "group_intercepts_summary.csv"), row.names = FALSE)

ggplot(group_effects, aes(y = reorder(group, Estimate), x = Estimate)) +
  geom_pointrange(aes(xmin = Q2.5, xmax = Q97.5)) +
  facet_wrap(~level, scales = "free_y") +
  labs(
    x = "Intercept (log scale)", y = NULL,
    title = "Group-level intercepts: country / store / product (95% CrI)"
  ) +
  theme_minimal()
ggsave(file.path(OUTPUT_DIR, "group_intercepts_forest_plot.png"), width = 10, height = 6)

# --- Table 3: variance decomposition across hierarchy levels ---

variance_summary <- as.data.frame(posterior_summary(fit, variable = "^sd_|^shape$", regex = TRUE))
write.csv(variance_summary, file.path(OUTPUT_DIR, "variance_decomposition.csv"), row.names = TRUE)
print(variance_summary)

# --- Table/check: cold-start vs. well-observed combo, predictive interval width ---
# year_centered = 0 corresponds to 2013 (YEAR_REFERENCE in features.R), the holdout year for this fit.

comparison_data <- data.frame(
  country = c("Kenya", "Canada"),
  store = c("Discount Stickers", "Premium Sticker Mart"),
  product = c("Holographic Goose", "Kaggle"),
  year_centered = c(0, 0),
  day_of_year = c(180, 180),
  weekday = factor(c("Wednesday", "Wednesday"), levels = weekday_levels),
  is_holiday = c(0, 0),
  case = c(
    "cold-start (Kenya/Discount/Holographic Goose)",
    "well-observed (Canada/Premium/Kaggle)"
  )
)

comparison_draws <- posterior_predict(fit, newdata = comparison_data, allow_new_levels = TRUE)
comparison_summary <- data.frame(
  case = comparison_data$case,
  median = apply(comparison_draws, 2, median),
  lower_90 = apply(comparison_draws, 2, quantile, probs = 0.05),
  upper_90 = apply(comparison_draws, 2, quantile, probs = 0.95)
)
comparison_summary$interval_width <- comparison_summary$upper_90 - comparison_summary$lower_90
comparison_summary$relative_width <- comparison_summary$interval_width / comparison_summary$median
print(comparison_summary)
write.csv(comparison_summary, file.path(OUTPUT_DIR, "cold_start_comparison.csv"), row.names = FALSE)
