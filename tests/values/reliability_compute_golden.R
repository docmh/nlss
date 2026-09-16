#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0

args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(flag, default = NULL) {
  idx <- which(args == flag)
  if (length(idx) == 0) return(default)
  if (idx[length(idx)] + 1 > length(args)) return(default)
  args[idx[length(idx)] + 1]
}

trim_arg <- function(x) {
  if (is.null(x)) return(NULL)
  x <- trimws(x)
  if (x == "") return(NULL)
  x
}

data_path <- trim_arg(get_arg("--data", file.path("tests", "data", "golden_dataset.csv")))
out_path <- trim_arg(get_arg("--out", file.path("tests", "values", "reliability_golden.csv")))

if (is.null(data_path) || !file.exists(data_path)) {
  stop("Missing --data path.")
}
if (is.null(out_path)) {
  stop("Missing --out path.")
}

options(scipen = 999, digits = 15)

df <- read.csv(data_path, stringsAsFactors = FALSE)

coerce_numeric <- function(df_use, vars) {
  for (var in vars) {
    if (!var %in% names(df_use)) next
    df_use[[var]] <- suppressWarnings(as.numeric(as.character(df_use[[var]])))
  }
  df_use
}

compute_missing_summary <- function(values) {
  total_n <- nrow(values)
  complete_n <- sum(complete.cases(values))
  missing_n <- total_n - complete_n
  missing_pct <- ifelse(total_n > 0, missing_n / total_n * 100, NA_real_)
  list(missing_n = missing_n, missing_pct = missing_pct, total_n = total_n, complete_n = complete_n)
}

# Test-only authoritative oracles. Do not source NLSS calculation code.
if (!requireNamespace("psych", quietly = TRUE)) stop("Golden regeneration requires test-only package psych.")
compute_icc <- function(values, model, type, unit, conf_level) {
  result <- suppressWarnings(psych::ICC(as.data.frame(values), lmer = FALSE, alpha = 1 - conf_level))$results
  selected <- paste0(if (model == "oneway") "ICC1" else if (type == "agreement") "ICC2" else "ICC3", if (unit == "average") "k" else "")
  row <- result[result$type == selected, , drop = FALSE]
  list(estimate = row$ICC, ci_low = row[["lower bound"]], ci_high = row[["upper bound"]],
    f_stat = row$F, df1 = row$df1, df2 = row$df2, p_value = row$p,
    n_subjects = nrow(values), n_raters = ncol(values))
}

compute_kappa <- function(x, y, weight = "none") {
  valid <- complete.cases(x, y)
  levels <- if (is.numeric(x) && is.numeric(y)) sort(unique(c(x[valid], y[valid]))) else sort(unique(c(as.character(x[valid]), as.character(y[valid]))))
  tab <- table(factor(x[valid], levels = levels), factor(y[valid], levels = levels))
  result <- suppressWarnings(psych::cohen.kappa(tab, n.obs = sum(valid), w.exp = if (weight == "linear") 1 else 2))
  list(estimate = if (weight == "none") result$kappa else result$weighted.kappa, n = sum(valid), n_categories = length(levels))
}

compute_test_retest <- function(x, y, method = "pearson", conf_level = 0.95) {
  valid <- complete.cases(x, y)
  result <- suppressWarnings(stats::cor.test(x[valid], y[valid], method = method, exact = FALSE, conf.level = conf_level))
  estimate <- unname(result$estimate)
  # Spearman retains the documented Fisher-z approximation, not an exact CI.
  bounds <- if (method == "pearson") result$conf.int[1:2] else tanh(atanh(estimate) + c(-1, 1) * stats::qnorm((1 + conf_level) / 2) / sqrt(sum(valid) - 3))
  list(estimate = estimate, ci_low = bounds[1], ci_high = bounds[2], p_value = result$p.value, n = sum(valid))
}

long_to_wide <- function(df_long, id_var, rater_var, score_var) {
  df_long <- df_long[, c(id_var, rater_var, score_var), drop = FALSE]
  df_long <- df_long[!is.na(df_long[[id_var]]) & !is.na(df_long[[rater_var]]), , drop = FALSE]
  if (nrow(df_long) == 0) {
    stop("No rows available after removing missing id/rater values.")
  }
  combos <- df_long[, c(id_var, rater_var), drop = FALSE]
  if (any(duplicated(combos))) {
    stop("Duplicate id/rater combinations found in long data.")
  }
  ids <- unique(df_long[[id_var]])
  raters <- unique(df_long[[rater_var]])
  mat <- matrix(NA, nrow = length(ids), ncol = length(raters))
  rownames(mat) <- as.character(ids)
  colnames(mat) <- as.character(raters)
  id_idx <- match(df_long[[id_var]], ids)
  rater_idx <- match(df_long[[rater_var]], raters)
  mat[cbind(id_idx, rater_idx)] <- df_long[[score_var]]
  list(matrix = mat, ids = ids, raters = raters)
}

build_icc_row <- function(case_id, wide_data, model, type, unit, conf_level, group_label = "None", format = "wide") {
  missing <- compute_missing_summary(wide_data)
  complete_data <- wide_data[complete.cases(wide_data), , drop = FALSE]
  stats <- compute_icc(complete_data, model, type, unit, conf_level)
  data.frame(
    case_id = case_id,
    analysis = "icc",
    format = format,
    group = group_label,
    model = model,
    type = if (model == "oneway") "agreement" else type,
    unit = unit,
    weight = "",
    method = "",
    var1 = "",
    var2 = "",
    conf_level = conf_level,
    estimate = stats$estimate,
    ci_low = stats$ci_low,
    ci_high = stats$ci_high,
    p_value = stats$p_value,
    f_stat = stats$f_stat,
    df1 = stats$df1,
    df2 = stats$df2,
    n = stats$n_subjects,
    n_raters = stats$n_raters,
    missing_n = missing$missing_n,
    missing_pct = missing$missing_pct,
    stringsAsFactors = FALSE
  )
}

build_kappa_row <- function(case_id, wide_data, weight, group_label = "None", format = "wide", var_names = NULL) {
  if (is.null(var_names)) var_names <- colnames(wide_data)
  missing <- compute_missing_summary(wide_data)
  stats <- compute_kappa(wide_data[[1]], wide_data[[2]], weight)
  data.frame(
    case_id = case_id,
    analysis = "kappa",
    format = format,
    group = group_label,
    model = "",
    type = "",
    unit = "",
    weight = weight,
    method = "",
    var1 = var_names[1],
    var2 = var_names[2],
    conf_level = NA_real_,
    estimate = stats$estimate,
    ci_low = NA_real_,
    ci_high = NA_real_,
    p_value = NA_real_,
    f_stat = NA_real_,
    df1 = NA_real_,
    df2 = NA_real_,
    n = stats$n,
    n_raters = 2,
    missing_n = missing$missing_n,
    missing_pct = missing$missing_pct,
    stringsAsFactors = FALSE
  )
}

build_retest_row <- function(case_id, wide_data, method, conf_level, group_label = "None", format = "wide", var_names = NULL) {
  if (is.null(var_names)) var_names <- colnames(wide_data)
  missing <- compute_missing_summary(wide_data)
  stats <- compute_test_retest(wide_data[[1]], wide_data[[2]], method, conf_level)
  data.frame(
    case_id = case_id,
    analysis = "test_retest",
    format = format,
    group = group_label,
    model = "",
    type = "",
    unit = "",
    weight = "",
    method = method,
    var1 = var_names[1],
    var2 = var_names[2],
    conf_level = conf_level,
    estimate = stats$estimate,
    ci_low = stats$ci_low,
    ci_high = stats$ci_high,
    p_value = stats$p_value,
    f_stat = NA_real_,
    df1 = NA_real_,
    df2 = NA_real_,
    n = stats$n,
    n_raters = 2,
    missing_n = missing$missing_n,
    missing_pct = missing$missing_pct,
    stringsAsFactors = FALSE
  )
}

prepare_icc_long <- function(df_use) {
  long_df <- reshape(
    df_use,
    varying = c("pre_score", "mid_score", "post_score"),
    v.names = "score",
    timevar = "time",
    times = c("pre", "mid", "post"),
    idvar = "id",
    direction = "long"
  )
  long_df$time <- factor(long_df$time, levels = c("pre", "mid", "post"))
  long_df$group3 <- factor(long_df$group3)
  long_df <- long_df[!is.na(long_df$score), ]
  long_df[, c("id", "time", "score", "group3")]
}

prepare_retest_long <- function(df_use) {
  long_df <- reshape(
    df_use,
    varying = c("pre_score", "post_score"),
    v.names = "score",
    timevar = "time",
    times = c("pre", "post"),
    idvar = "id",
    direction = "long"
  )
  long_df$time <- factor(long_df$time, levels = c("pre", "post"))
  long_df$group3 <- factor(long_df$group3)
  long_df <- long_df[!is.na(long_df$score), ]
  long_df[, c("id", "time", "score", "group3")]
}

prepare_ordinal_df <- function(df_use) {
  df_use$ordinal_var2 <- df_use$ordinal_var
  max_val <- max(df_use$ordinal_var2, na.rm = TRUE)
  df_use$ordinal_var2[df_use$ordinal_var2 == max_val] <- df_use$ordinal_var2[df_use$ordinal_var2 == max_val] - 1
  df_use
}

prepare_coerce_df <- function(df_use) {
  df_use$pre_score <- as.character(df_use$pre_score)
  df_use$post_score <- as.character(df_use$post_score)
  df_use$pre_score[df_use$id %% 17 == 0] <- "bad"
  df_use$post_score[df_use$id %% 19 == 0] <- "bad"
  df_use$pre_score[df_use$id %% 10 == 0] <- NA
  df_use$post_score[df_use$id %% 15 == 0] <- NA
  df_use
}

rows <- list()

base_numeric <- coerce_numeric(df, c("pre_score", "mid_score", "post_score"))

wide_icc <- base_numeric[, c("pre_score", "mid_score", "post_score")]
rows[[length(rows) + 1]] <- build_icc_row(
  "icc_twoway_random_agreement_single",
  wide_icc,
  model = "twoway-random",
  type = "agreement",
  unit = "single",
  conf_level = 0.95
)
rows[[length(rows) + 1]] <- build_icc_row(
  "icc_oneway_consistency_average_0.9",
  wide_icc,
  model = "oneway",
  type = "consistency",
  unit = "average",
  conf_level = 0.9
)
rows[[length(rows) + 1]] <- build_icc_row(
  "icc_twoway_random_consistency_single",
  wide_icc,
  model = "twoway-random",
  type = "consistency",
  unit = "single",
  conf_level = 0.95
)
rows[[length(rows) + 1]] <- build_icc_row(
  "icc_twoway_mixed_agreement_average",
  wide_icc,
  model = "twoway-mixed",
  type = "agreement",
  unit = "average",
  conf_level = 0.95
)

long_icc <- prepare_icc_long(base_numeric)
long_icc_A <- long_icc[long_icc$group3 == "A", ]
wide_icc_long <- long_to_wide(long_icc_A, "id", "time", "score")$matrix
rows[[length(rows) + 1]] <- build_icc_row(
  "icc_long_twoway_random_agreement_single_group_A",
  as.data.frame(wide_icc_long),
  model = "twoway-random",
  type = "agreement",
  unit = "single",
  conf_level = 0.95,
  group_label = "A",
  format = "long"
)

wide_retest <- base_numeric[, c("pre_score", "post_score")]
rows[[length(rows) + 1]] <- build_retest_row(
  "test_retest_spearman_0.9",
  wide_retest,
  method = "spearman",
  conf_level = 0.9
)

coerce_df <- prepare_coerce_df(df)
coerce_df <- coerce_numeric(coerce_df, c("pre_score", "post_score"))
wide_coerce <- coerce_df[, c("pre_score", "post_score")]
rows[[length(rows) + 1]] <- build_retest_row(
  "test_retest_pearson_coerce",
  wide_coerce,
  method = "pearson",
  conf_level = 0.95
)

long_retest <- prepare_retest_long(base_numeric)
long_retest_A <- long_retest[long_retest$group3 == "A", ]
wide_retest_long <- long_to_wide(long_retest_A, "id", "time", "score")$matrix
rows[[length(rows) + 1]] <- build_retest_row(
  "test_retest_long_pearson_group_A",
  as.data.frame(wide_retest_long),
  method = "pearson",
  conf_level = 0.95,
  group_label = "A",
  format = "long",
  var_names = colnames(wide_retest_long)
)

wide_kappa <- df[, c("cat_var", "cat_var2")]
rows[[length(rows) + 1]] <- build_kappa_row(
  "kappa_none_cat_var",
  wide_kappa,
  weight = "none"
)

kappa_group <- df[!is.na(df$group3) & df$group3 == "A", c("cat_var", "cat_var2")]
rows[[length(rows) + 1]] <- build_kappa_row(
  "kappa_none_cat_var_group_A",
  kappa_group,
  weight = "none",
  group_label = "A"
)

ordinal_df <- prepare_ordinal_df(df)
wide_ordinal <- ordinal_df[, c("ordinal_var", "ordinal_var2")]
rows[[length(rows) + 1]] <- build_kappa_row(
  "kappa_quadratic_ordinal",
  wide_ordinal,
  weight = "quadratic"
)
rows[[length(rows) + 1]] <- build_kappa_row(
  "kappa_linear_ordinal",
  wide_ordinal,
  weight = "linear"
)

out_df <- do.call(rbind, rows)
write.csv(out_df, out_path, row.names = FALSE, na = "NA")
cat("Wrote:", out_path, "\n")
