library(brms)
library(data.table)
library(ggplot2)
source("src/current_fit.R")

fit <- readRDS(CURRENT_FIT_PATH)
holdout_data <- copy(current_split$holdout_data)
holdout_data[, date := as.Date(date)]

pred_draws <- posterior_predict(fit, newdata = holdout_data, allow_new_levels = TRUE)

plot_group_fit <- function(group_var) {
  group_label <- paste(holdout_data$date, holdout_data[[group_var]])

  actual <- holdout_data[, .(actual = sum(num_sold)), by = c("date", group_var)]

  agg_draws <- t(rowsum(t(pred_draws), group = group_label))
  key_split <- unique(holdout_data[, c("date", group_var), with = FALSE])
  key_split[, group_label := paste(date, get(group_var))]
  key_split <- key_split[match(colnames(agg_draws), group_label)]

  predicted <- data.table(
    date = key_split$date,
    group = key_split[[group_var]],
    pred_median = apply(agg_draws, 2, median),
    pred_lower_95 = apply(agg_draws, 2, quantile, probs = 0.025),
    pred_upper_95 = apply(agg_draws, 2, quantile, probs = 0.975)
  )
  setnames(predicted, "group", group_var)

  plot_data <- merge(actual, predicted, by = c("date", group_var))

  ggplot(plot_data, aes(x = date)) +
    geom_ribbon(aes(ymin = pred_lower_95, ymax = pred_upper_95), fill = "steelblue", alpha = 0.3) +
    geom_line(aes(y = pred_median, color = "predicted")) +
    geom_line(aes(y = actual, color = "actual")) +
    facet_wrap(vars(.data[[group_var]]), scales = "free_y") +
    scale_color_manual(values = c(actual = "black", predicted = "steelblue")) +
    labs(
      x = NULL, y = "num_sold (summed over other groups)", color = NULL,
      title = paste0("Actual vs. predicted holdout sales by ", group_var, " (median + 95% predictive interval)")
    ) +
    theme_minimal()
  ggsave(file.path(OUTPUT_DIR, paste0(group_var, "_fit.png")), width = 10, height = 6)
}

plot_group_fit("country")
plot_group_fit("store")
plot_group_fit("product")
