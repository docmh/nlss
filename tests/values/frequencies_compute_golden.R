#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Independent base-R oracle; never source NLSS computation code.
args <- commandArgs(TRUE)
value <- function(flag, default) {
  i <- match(flag, args)
  if (is.na(i)) default else args[i + 1L]
}
config <- yaml::read_yaml(value("--tests-config", Sys.getenv("NLSS_TESTS_CONFIG", "tests/tests.yml")), eval.expr = FALSE)$tests
data <- read.csv(value("--data", config$golden_dataset), stringsAsFactors = FALSE)
output <- value("--out", "tests/values/frequencies_golden.csv")
rows <- list()
for (case in c("ungrouped", "grouped")) {
  groups <- if (case == "grouped") unique(data$group3) else ""
  for (g in groups) for (variable in c("cat_var", "cat_var2", "gender", "ordinal_var", "all_missing_var")) {
    x <- if (case == "grouped") data[[variable]][!is.na(data$group3) & data$group3 == g] else data[[variable]]
    valid <- x[!is.na(x)]
    counts <- table(factor(valid, levels = sort(unique(valid))))
    levels <- names(counts)
    if (!length(levels)) { levels <- "(no valid data)"; counts <- 0L }
    rows[[length(rows) + 1L]] <- data.frame(case_id = case, variable = variable, group = g,
      level = levels, n = as.numeric(counts), pct_total = as.numeric(counts) / length(x) * 100,
      pct_valid = if (length(valid)) as.numeric(counts) / length(valid) * 100 else NA_real_,
      total_n = length(x), missing_n = sum(is.na(x)), missing_pct = mean(is.na(x)) * 100)
  }
}
options(digits = 17)
write.csv(do.call(rbind, rows), output, row.names = FALSE, na = "")
cat("Independent frequency golden rows:", sum(vapply(rows, nrow, integer(1))), "\n")
