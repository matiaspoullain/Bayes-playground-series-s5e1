source("src/holdout_data.R")

CURRENT_FIT_PATH <- "models/fit_holdout_1yr.rds"
OUTPUT_DIR <- "tables_figures/1yr"

current_split <- build_holdout_split(
  fit_year_max = 2012,
  holdout_years = 2013,
  countries = c("Canada", "Kenya")
)

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
