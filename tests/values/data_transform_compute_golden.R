#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Independent base-R expectations. This script never sources NLSS code.
args <- commandArgs(TRUE)
arg <- function(flag, fallback) { i <- which(args == flag); if (length(i)) args[i + 1L] else fallback }
d <- read.csv(arg("--data", "tests/data/golden_dataset.csv"), stringsAsFactors = FALSE)
out <- arg("--out", "tests/values/data_transform_golden.csv")
reference <- data.frame(
  change = d$post_score - d$pre_score,
  constant = rep(7, nrow(d)),
  log_age = log(d$age), log10_income = log10(d$income), sqrt_education = sqrt(d$education),
  exp_x1 = exp(d$x1), abs_x2 = abs(d$x2),
  center_outcome_reg = d$outcome_reg - mean(d$outcome_reg, na.rm = TRUE),
  outcome_anova_z = as.numeric(scale(d$outcome_anova)), pre_score_z = as.numeric(scale(d$pre_score)),
  ordinal_var_rec = ifelse(is.na(d$ordinal_var), 0, ifelse(d$ordinal_var == 1, 2, ifelse(d$ordinal_var == 2, 3, d$ordinal_var))),
  satisfaction_pct = cut(d$satisfaction, unique(as.numeric(quantile(d$satisfaction,
    seq(0, 1, length.out = 5), type = 7, na.rm = TRUE))), include.lowest = TRUE, right = TRUE, labels = FALSE),
  age_bin = cut(d$age, c(18, 30, 45, 80), include.lowest = TRUE, right = TRUE, labels = FALSE)
)
rows <- do.call(rbind, lapply(names(reference), function(variable) data.frame(
  case_id = "all_operations", row = seq_len(nrow(d)), variable = variable,
  value = reference[[variable]], stringsAsFactors = FALSE)))
options(digits = 17)
write.csv(rows, out, row.names = FALSE, na = "NA")
cat("Wrote", nrow(rows), "independent base-R values to", out, "\n")
