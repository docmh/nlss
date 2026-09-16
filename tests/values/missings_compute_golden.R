#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Independent base-R reference; never sources or executes NLSS functions.
args <- commandArgs(TRUE)
arg <- function(flag, fallback) { i <- which(args == flag); if (length(i)) args[i + 1L] else fallback }
d <- read.csv(arg("--data", "tests/data/golden_dataset.csv"), stringsAsFactors = FALSE)
out <- arg("--out", "tests/values/missings_golden.csv")
vars <- c("age", "income", "pre_score", "group2", "satisfaction", "high_missing_var")
rows <- list()
add <- function(case, table, row, key, value) {
  kind <- if (is.na(value)) "missing" else if (is.numeric(value)) "number" else "text"
  rows[[length(rows) + 1L]] <<- data.frame(case_id = case, table = table, row = row, key = key,
    kind = kind, value = if (is.na(value)) "" else if (is.numeric(value)) sprintf("%.17g", value) else as.character(value), stringsAsFactors = FALSE)
}
for (method in c("auto", "listwise", "impute", "indicator", "drop")) {
  selected <- if (method == "auto") "indicator" else method # Maximum missingness 73/240 lies in (.20,.40].
  miss <- is.na(d[vars]); prop <- colMeans(miss); after <- d
  dropped <- if (selected == "drop") vars[prop >= .3] else character()
  indicated <- if (selected %in% c("indicator", "drop")) setdiff(vars[prop >= .1], dropped) else character()
  methods <- decisions <- setNames(rep("", length(vars)), vars)
  if (selected == "listwise") {
    after <- d[complete.cases(d[vars]), , drop = FALSE]
    decisions[] <- ifelse(colSums(miss) > 0, "listwise deletion", "no missing")
  } else {
    after[dropped] <- NULL
    for (variable in vars) {
      if (variable %in% dropped) { decisions[variable] <- "drop"; next }
      if (!any(miss[, variable])) { decisions[variable] <- "keep"; next }
      observed <- d[[variable]][!miss[, variable]]
      if (is.numeric(observed)) {
        skew <- if (length(observed) < 3L) NA_real_ else if (sd(observed) == 0) 0 else mean((observed - mean(observed))^3) / sd(observed)^3
        use_median <- !is.na(skew) && abs(skew) > 1
        replacement <- if (use_median) median(observed) else mean(observed)
        methods[variable] <- if (use_median) "median" else "mean"
      } else {
        frequencies <- table(observed)
        replacement <- names(frequencies)[which.max(frequencies)]
        methods[variable] <- "mode"
      }
      after[[variable]][miss[, variable]] <- replacement
      decisions[variable] <- if (variable %in% indicated) "impute + indicator" else "impute"
    }
    for (variable in indicated) after[[paste0(variable, "_miss")]] <- as.integer(miss[, variable])
  }
  for (i in seq_along(vars)) {
    variable <- vars[i]
    values <- list(variable = variable, total_n = nrow(d), missing_n = sum(miss[, variable]), missing_prop = prop[variable],
      missing_pct = 100 * prop[variable], decision = decisions[variable], impute_method = methods[variable],
      indicator = if (variable %in% indicated) paste0(variable, "_miss") else "")
    for (key in names(values)) add(method, "summary_df", i, key, values[[key]])
  }
  pattern <- apply(miss, 1L, function(mask) paste(ifelse(mask, "M", "O"), collapse = ""))
  frequencies <- sort(table(pattern), decreasing = TRUE)
  shown <- head(frequencies, 3L)
  for (i in seq_along(shown)) {
    values <- list(pattern = names(shown)[i], missing_count = sum(strsplit(names(shown)[i], "", fixed = TRUE)[[1]] == "M"),
      n = as.integer(shown[i]), pct_total = as.numeric(shown[i]) / nrow(d) * 100)
    for (key in names(values)) add(method, "patterns_df", i, key, values[[key]])
  }
  if (length(frequencies) > 3L) {
    values <- list(pattern = "Other patterns", missing_count = NA_real_, n = sum(frequencies[-seq_len(3L)]), pct_total = sum(frequencies[-seq_len(3L)]) / nrow(d) * 100)
    for (key in names(values)) add(method, "patterns_df", 4L, key, values[[key]])
  }
  for (variable in intersect(c("id", vars, paste0(vars, "_miss")), names(after))) {
    for (i in seq_len(nrow(after))) add(method, "transformed_df", i, variable, after[[variable]][i])
  }
  add(method, "metrics", 1L, "rows_removed", nrow(d) - nrow(after))
  add(method, "metrics", 1L, "method_selected", selected)
}
result <- do.call(rbind, rows)
write.csv(result, out, row.names = FALSE, na = "NA")
cat("Wrote", nrow(result), "independent cells to", out, "\n")
