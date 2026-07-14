library(brms)
library(ggplot2)
source("src/current_fit.R")

fit <- readRDS(CURRENT_FIT_PATH)

pp_check(fit, type = "stat_grouped", group = "country", stat = "mean", ndraws = 100)
ggsave(file.path(OUTPUT_DIR, "ppc_by_country.png"), width = 10, height = 6)

pp_check(fit, type = "stat_grouped", group = "store", stat = "mean", ndraws = 100)
ggsave(file.path(OUTPUT_DIR, "ppc_by_store.png"), width = 10, height = 6)
