#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Independent base-R completion references: no NLSS functions are loaded.
args <- commandArgs(TRUE)
arg <- function(flag, fallback) { i <- which(args == flag); if (length(i)) args[i + 1L] else fallback }
d <- read.csv(arg("--data", "tests/data/golden_dataset.csv"), stringsAsFactors = FALSE)
out <- arg("--out", "tests/values/impute_golden.csv")
vars <- c("age", "income", "pre_score", "group2", "satisfaction", "high_missing_var")
rows <- list()
add <- function(case, table, row, key, value) {
  missing <- is.na(value)
  rows[[length(rows) + 1L]] <<- data.frame(case_id = case, table = table, row = row, key = key,
    kind = if (missing) "missing" else if (is.numeric(value)) "number" else "text",
    value = if (missing) "" else if (is.numeric(value)) sprintf("%.17g", value) else as.character(value), stringsAsFactors = FALSE)
}
for (method in c("auto", "mean", "median", "mode", "random", "constant")) {
  set.seed(811L)
  for (i in seq_along(vars)) {
    variable <- vars[i]; x <- d[[variable]]; missing <- is.na(x); observed <- x[!missing]
    effective <- if (is.numeric(x)) method else if (method %in% c("random", "constant")) method else "mode"
    if (effective == "auto") {
      skew <- if (length(observed) < 3L) NA_real_ else if (all(observed == observed[1])) 0 else mean(((observed - mean(observed)) / sd(observed))^3)
      effective <- if (!is.na(skew) && abs(skew) > 1) "median" else "mean"
    }
    donors <- unique(observed)
    replacement <- switch(effective, mean = mean(observed), median = median(observed),
      mode = donors[which.max(tabulate(match(observed, donors)))],
      random = observed[sample.int(length(observed), sum(missing), replace = TRUE)],
      constant = if (is.numeric(x)) 7 else "Missing")
    completed <- x; completed[missing] <- replacement
    values <- list(variable = variable, missing_n = sum(missing), missing_pct = mean(missing) * 100,
      engine = "simple", method = effective, imputed_n = sum(missing), target = paste0(variable, "_imp"), indicator = paste0(variable, "_miss"))
    for (key in names(values)) add(method, "summary_df", i, key, values[[key]])
    for (j in seq_along(completed)) {
      add(method, "transformed_df", j, paste0(variable, "_imp"), completed[j])
      add(method, "transformed_df", j, paste0(variable, "_miss"), as.integer(missing[j]))
    }
  }
  add(method, "metrics", 1L, "completion_mode", "single_completion")
  add(method, "metrics", 1L, "completion_aggregation", "none")
}
result <- do.call(rbind, rows)
write.csv(result, out, row.names = FALSE, na = "NA")
cat("Wrote", nrow(result), "independent imputation cells to", out, "\n")
