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
out_summary <- trim_arg(get_arg("--out", file.path("tests", "values", "nonparametric_golden.csv")))
out_posthoc <- trim_arg(get_arg("--posthoc-out", file.path("tests", "values", "nonparametric_posthoc_golden.csv")))
out_diagnostics <- trim_arg(get_arg("--diagnostics-out", file.path("tests", "values", "nonparametric_diagnostics_golden.csv")))

if (is.null(data_path) || !file.exists(data_path)) {
  stop("Missing --data path.")
}
if (is.null(out_summary) || is.null(out_posthoc) || is.null(out_diagnostics)) {
  stop("Missing output path(s).")
}

options(scipen = 999, digits = 15)

df <- read.csv(data_path, stringsAsFactors = FALSE)

get_levels <- function(vec) {
  if (is.factor(vec)) {
    return(as.character(levels(vec)))
  }
  values <- unique(vec[!is.na(vec)])
  if (length(values) == 0) return(character(0))
  if (is.numeric(values)) {
    return(as.character(sort(values)))
  }
  as.character(sort(values))
}

safe_wilcox_test <- function(...) {
  tryCatch(suppressWarnings(wilcox.test(...)), error = function(e) NULL)
}

extract_flags <- function(test) {
  method <- if (!is.null(test) && !is.null(test$method)) tolower(test$method) else ""
  list(
    exact_used = grepl("exact", method),
    continuity_used = grepl("continuity", method)
  )
}

calc_rank_biserial_signed <- function(diffs) {
  diffs <- diffs[!is.na(diffs)]
  diffs <- diffs[diffs != 0]
  n <- length(diffs)
  if (n == 0) {
    return(list(value = NA_real_, w_pos = NA_real_, w_neg = NA_real_, n = 0))
  }
  ranks <- rank(abs(diffs))
  w_pos <- sum(ranks[diffs > 0])
  w_neg <- sum(ranks[diffs < 0])
  denom <- n * (n + 1) / 2
  value <- if (denom > 0) (w_pos - w_neg) / denom else NA_real_
  list(value = value, w_pos = w_pos, w_neg = w_neg, n = n)
}

calc_z_signed <- function(w_pos, n) {
  if (is.na(w_pos) || is.na(n) || n <= 0) return(NA_real_)
  mu <- n * (n + 1) / 4
  sigma <- sqrt(n * (n + 1) * (2 * n + 1) / 24)
  if (sigma == 0) return(NA_real_)
  (w_pos - mu) / sigma
}

calc_r_from_z <- function(z, n) {
  if (is.na(z) || is.na(n) || n <= 0) return(NA_real_)
  z / sqrt(n)
}

calc_u_from_w <- function(w_stat, n1) {
  if (is.na(w_stat) || is.na(n1)) return(NA_real_)
  w_stat - n1 * (n1 + 1) / 2
}

calc_z_mann_whitney <- function(u_stat, n1, n2) {
  if (is.na(u_stat) || n1 <= 0 || n2 <= 0) return(NA_real_)
  mu <- n1 * n2 / 2
  sigma <- sqrt(n1 * n2 * (n1 + n2 + 1) / 12)
  if (sigma == 0) return(NA_real_)
  (u_stat - mu) / sigma
}

calc_rank_biserial_mann_whitney <- function(u_stat, n1, n2) {
  if (is.na(u_stat) || n1 <= 0 || n2 <= 0) return(NA_real_)
  (2 * u_stat) / (n1 * n2) - 1
}

calc_epsilon_sq <- function(h_stat, n, k) {
  if (is.na(h_stat) || n <= k || k < 2) return(NA_real_)
  (h_stat - k + 1) / (n - k)
}

calc_kendall_w <- function(q_stat, n, k) {
  if (is.na(q_stat) || n <= 0 || k < 2) return(NA_real_)
  q_stat / (n * (k - 1))
}

summary_cols <- c(
  "case_id", "test_type", "variable", "measure_1", "measure_2",
  "group", "group_1", "group_2",
  "n_1", "n_2", "n_total",
  "median_1", "median_2", "iqr_1", "iqr_2",
  "median", "iqr", "median_diff", "iqr_diff",
  "statistic", "df", "p", "effect_size_value", "ci_low", "ci_high", "mu",
  "opt_test", "opt_mode", "opt_vars", "opt_x", "opt_y", "opt_group",
  "opt_within", "opt_subject_id",
  "opt_mu", "opt_alternative", "opt_exact", "opt_continuity", "opt_conf_level",
  "opt_posthoc", "opt_p_adjust", "opt_effect_size", "opt_digits"
)

posthoc_cols <- c(
  "case_id", "test_type", "variable", "group", "group_1", "group_2",
  "n_1", "n_2", "statistic", "p", "p_adj", "effect_size_value", "ci_low", "ci_high"
)

diag_cols <- c(
  "case_id", "test_type", "variable", "group",
  "n_total", "n_nonzero", "zero_diff_n", "n", "k",
  "ties", "exact_used", "continuity_used", "group_levels", "group_sizes"
)

make_row <- function(columns, values) {
  row <- as.list(setNames(rep(NA, length(columns)), columns))
  for (name in names(values)) {
    row[[name]] <- values[[name]]
  }
  as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
}

summary_rows <- list()
diag_rows <- list()
posthoc_rows <- list()

## Case 1: Wilcoxon one-sample (rb)
case_id <- "wilcoxon_one_sample_rb"
var <- "skewed_var"
mu <- 0
alternative <- "greater"
conf_level <- 0.9
exact <- FALSE
continuity <- TRUE
effect_size <- "rb"
digits <- 3

vec <- df[[var]]
clean <- vec[!is.na(vec)]
n <- length(clean)
median_val <- ifelse(n > 0, median(clean), NA_real_)
iqr_val <- ifelse(n > 0, IQR(clean), NA_real_)
diffs <- clean - mu
median_diff <- ifelse(n > 0, median(diffs), NA_real_)
iqr_diff <- ifelse(n > 0, IQR(diffs), NA_real_)
rank_info <- calc_rank_biserial_signed(diffs)
zero_diffs <- sum(diffs == 0, na.rm = TRUE)
ties <- length(unique(abs(diffs[diffs != 0]))) < length(diffs[diffs != 0])
test <- if (n > 0) safe_wilcox_test(
  clean,
  mu = mu,
  alternative = alternative,
  conf.int = TRUE,
  conf.level = conf_level,
  exact = exact,
  correct = continuity
) else NULL

stat_val <- if (!is.null(test)) unname(test$statistic) else rank_info$w_pos
p_val <- if (!is.null(test)) test$p.value else NA_real_
ci_low <- if (!is.null(test) && !is.null(test$conf.int)) test$conf.int[1] else NA_real_
ci_high <- if (!is.null(test) && !is.null(test$conf.int)) test$conf.int[2] else NA_real_
z_val <- calc_z_signed(rank_info$w_pos, rank_info$n)
r_val <- calc_r_from_z(z_val, rank_info$n)
rb_val <- rank_info$value
effect_val <- if (effect_size == "rb") rb_val else r_val
flags <- extract_flags(test)

summary_rows[[length(summary_rows) + 1]] <- make_row(summary_cols, list(
  case_id = case_id,
  test_type = "Wilcoxon signed-rank",
  variable = var,
  measure_1 = "",
  measure_2 = "",
  group = "",
  group_1 = "",
  group_2 = "",
  n_1 = n,
  n_2 = NA_real_,
  n_total = n,
  median_1 = NA_real_,
  median_2 = NA_real_,
  iqr_1 = NA_real_,
  iqr_2 = NA_real_,
  median = median_val,
  iqr = iqr_val,
  median_diff = median_diff,
  iqr_diff = iqr_diff,
  statistic = stat_val,
  df = NA_real_,
  p = p_val,
  effect_size_value = effect_val,
  ci_low = ci_low,
  ci_high = ci_high,
  mu = mu,
  opt_test = "wilcoxon",
  opt_mode = "wilcoxon_one_sample",
  opt_vars = var,
  opt_mu = mu,
  opt_alternative = alternative,
  opt_exact = exact,
  opt_continuity = continuity,
  opt_conf_level = conf_level,
  opt_effect_size = effect_size,
  opt_digits = digits
))

diag_rows[[length(diag_rows) + 1]] <- make_row(diag_cols, list(
  case_id = "diag_wilcoxon_one_sample",
  test_type = "wilcoxon_one_sample",
  variable = var,
  group = "",
  n_total = n,
  n_nonzero = rank_info$n,
  zero_diff_n = zero_diffs,
  ties = ties,
  exact_used = flags$exact_used,
  continuity_used = flags$continuity_used
))

## Case 2: Wilcoxon paired (r)
case_id <- "wilcoxon_paired_r"
x_var <- "pre_score"
y_var <- "post_score"
alternative <- "two.sided"
conf_level <- 0.95
exact <- FALSE
continuity <- FALSE
effect_size <- "r"
digits <- 2

x <- df[[x_var]]
y <- df[[y_var]]
complete <- !is.na(x) & !is.na(y)
x <- x[complete]
y <- y[complete]
n <- length(x)

median_1 <- ifelse(n > 0, median(x), NA_real_)
median_2 <- ifelse(n > 0, median(y), NA_real_)
iqr_1 <- ifelse(n > 0, IQR(x), NA_real_)
iqr_2 <- ifelse(n > 0, IQR(y), NA_real_)
diffs <- x - y
median_diff <- ifelse(n > 0, median(diffs), NA_real_)
iqr_diff <- ifelse(n > 0, IQR(diffs), NA_real_)

rank_info <- calc_rank_biserial_signed(diffs)
zero_diffs <- sum(diffs == 0, na.rm = TRUE)
ties <- length(unique(abs(diffs[diffs != 0]))) < length(diffs[diffs != 0])
test <- if (n > 0) safe_wilcox_test(
  x,
  y,
  paired = TRUE,
  alternative = alternative,
  conf.int = TRUE,
  conf.level = conf_level,
  exact = exact,
  correct = continuity
) else NULL

stat_val <- if (!is.null(test)) unname(test$statistic) else rank_info$w_pos
p_val <- if (!is.null(test)) test$p.value else NA_real_
ci_low <- if (!is.null(test) && !is.null(test$conf.int)) test$conf.int[1] else NA_real_
ci_high <- if (!is.null(test) && !is.null(test$conf.int)) test$conf.int[2] else NA_real_
z_val <- calc_z_signed(rank_info$w_pos, rank_info$n)
r_val <- calc_r_from_z(z_val, rank_info$n)
rb_val <- rank_info$value
effect_val <- if (effect_size == "rb") rb_val else r_val
flags <- extract_flags(test)

summary_rows[[length(summary_rows) + 1]] <- make_row(summary_cols, list(
  case_id = case_id,
  test_type = "Wilcoxon signed-rank",
  variable = paste0(x_var, " vs ", y_var),
  measure_1 = x_var,
  measure_2 = y_var,
  group = "",
  group_1 = "",
  group_2 = "",
  n_1 = n,
  n_2 = NA_real_,
  n_total = n,
  median_1 = median_1,
  median_2 = median_2,
  iqr_1 = iqr_1,
  iqr_2 = iqr_2,
  median = NA_real_,
  iqr = NA_real_,
  median_diff = median_diff,
  iqr_diff = iqr_diff,
  statistic = stat_val,
  df = NA_real_,
  p = p_val,
  effect_size_value = effect_val,
  ci_low = ci_low,
  ci_high = ci_high,
  mu = NA_real_,
  opt_test = "wilcoxon",
  opt_mode = "wilcoxon_paired",
  opt_x = x_var,
  opt_y = y_var,
  opt_alternative = alternative,
  opt_exact = exact,
  opt_continuity = continuity,
  opt_conf_level = conf_level,
  opt_effect_size = effect_size,
  opt_digits = digits
))

diag_rows[[length(diag_rows) + 1]] <- make_row(diag_cols, list(
  case_id = "diag_wilcoxon_paired",
  test_type = "wilcoxon_paired",
  variable = paste0(x_var, " vs ", y_var),
  group = "",
  n_total = n,
  n_nonzero = rank_info$n,
  zero_diff_n = zero_diffs,
  ties = ties,
  exact_used = flags$exact_used,
  continuity_used = flags$continuity_used
))

## Case 3: Mann-Whitney (rb)
case_id <- "mann_whitney_rb"
var <- "outcome_anova"
group_var <- "group2"
alternative <- "two.sided"
conf_level <- 0.95
exact <- FALSE
continuity <- TRUE
effect_size <- "rb"
digits <- 2

vec <- df[[var]]
group_vals <- df[[group_var]]
complete <- !is.na(vec) & !is.na(group_vals)
vec <- vec[complete]
groups <- as.character(group_vals[complete])
levels <- get_levels(groups)
group1 <- as.character(levels[1])
group2 <- as.character(levels[2])
x1 <- vec[groups == group1]
x2 <- vec[groups == group2]
n1 <- length(x1)
n2 <- length(x2)

median_1 <- ifelse(n1 > 0, median(x1), NA_real_)
median_2 <- ifelse(n2 > 0, median(x2), NA_real_)
iqr_1 <- ifelse(n1 > 0, IQR(x1), NA_real_)
iqr_2 <- ifelse(n2 > 0, IQR(x2), NA_real_)

test <- if (n1 > 0 && n2 > 0) safe_wilcox_test(
  x1,
  x2,
  alternative = alternative,
  conf.int = TRUE,
  conf.level = conf_level,
  exact = exact,
  correct = continuity
) else NULL

w_stat <- if (!is.null(test)) unname(test$statistic) else NA_real_
u_stat <- calc_u_from_w(w_stat, n1)
p_val <- if (!is.null(test)) test$p.value else NA_real_
ci_low <- if (!is.null(test) && !is.null(test$conf.int)) test$conf.int[1] else NA_real_
ci_high <- if (!is.null(test) && !is.null(test$conf.int)) test$conf.int[2] else NA_real_

z_val <- calc_z_mann_whitney(u_stat, n1, n2)
r_val <- calc_r_from_z(z_val, n1 + n2)
rb_val <- calc_rank_biserial_mann_whitney(u_stat, n1, n2)
effect_val <- if (effect_size == "rb") rb_val else r_val

ties <- length(unique(c(x1, x2))) < length(c(x1, x2))
flags <- extract_flags(test)

summary_rows[[length(summary_rows) + 1]] <- make_row(summary_cols, list(
  case_id = case_id,
  test_type = "Mann-Whitney U",
  variable = var,
  measure_1 = "",
  measure_2 = "",
  group = group_var,
  group_1 = group1,
  group_2 = group2,
  n_1 = n1,
  n_2 = n2,
  n_total = n1 + n2,
  median_1 = median_1,
  median_2 = median_2,
  iqr_1 = iqr_1,
  iqr_2 = iqr_2,
  median = NA_real_,
  iqr = NA_real_,
  median_diff = median_1 - median_2,
  iqr_diff = NA_real_,
  statistic = u_stat,
  df = NA_real_,
  p = p_val,
  effect_size_value = effect_val,
  ci_low = ci_low,
  ci_high = ci_high,
  mu = NA_real_,
  opt_test = "mann_whitney",
  opt_mode = "mann_whitney",
  opt_vars = var,
  opt_group = group_var,
  opt_alternative = alternative,
  opt_exact = exact,
  opt_continuity = continuity,
  opt_conf_level = conf_level,
  opt_effect_size = effect_size,
  opt_digits = digits
))

diag_rows[[length(diag_rows) + 1]] <- make_row(diag_cols, list(
  case_id = "diag_mann_whitney_control",
  test_type = "mann_whitney",
  variable = var,
  group = group1,
  n = n1,
  ties = ties,
  exact_used = flags$exact_used,
  continuity_used = flags$continuity_used
))

diag_rows[[length(diag_rows) + 1]] <- make_row(diag_cols, list(
  case_id = "diag_mann_whitney_treatment",
  test_type = "mann_whitney",
  variable = var,
  group = group2,
  n = n2,
  ties = ties,
  exact_used = flags$exact_used,
  continuity_used = flags$continuity_used
))

## Case 4: Kruskal-Wallis + posthoc
case_id <- "kruskal_outcome_anova"
var <- "outcome_anova"
group_var <- "group3"
effect_size <- "epsilon_sq"
digits <- 3
posthoc_adjust <- "holm"

vec <- df[[var]]
group_vals <- df[[group_var]]
complete <- !is.na(vec) & !is.na(group_vals)
vec <- vec[complete]
groups <- as.character(group_vals[complete])
levels <- get_levels(groups)
k <- length(levels)
n_total <- length(vec)

test <- if (n_total > 0) tryCatch(kruskal.test(vec ~ groups), error = function(e) NULL) else NULL
h_stat <- if (!is.null(test)) unname(test$statistic) else NA_real_
df_val <- if (!is.null(test)) unname(test$parameter) else NA_real_
p_val <- if (!is.null(test)) test$p.value else NA_real_
epsilon_sq <- calc_epsilon_sq(h_stat, n_total, k)

summary_rows[[length(summary_rows) + 1]] <- make_row(summary_cols, list(
  case_id = case_id,
  test_type = "Kruskal-Wallis",
  variable = var,
  measure_1 = "",
  measure_2 = "",
  group = group_var,
  group_1 = "",
  group_2 = "",
  n_1 = NA_real_,
  n_2 = NA_real_,
  n_total = n_total,
  median_1 = NA_real_,
  median_2 = NA_real_,
  iqr_1 = NA_real_,
  iqr_2 = NA_real_,
  median = NA_real_,
  iqr = NA_real_,
  median_diff = NA_real_,
  iqr_diff = NA_real_,
  statistic = h_stat,
  df = df_val,
  p = p_val,
  effect_size_value = epsilon_sq,
  ci_low = NA_real_,
  ci_high = NA_real_,
  mu = NA_real_,
  opt_test = "kruskal",
  opt_mode = "kruskal",
  opt_vars = var,
  opt_group = group_var,
  opt_posthoc = "pairwise",
  opt_p_adjust = posthoc_adjust,
  opt_effect_size = effect_size,
  opt_digits = digits
))

size_table <- table(groups)
diag_rows[[length(diag_rows) + 1]] <- make_row(diag_cols, list(
  case_id = "diag_kruskal_outcome_anova",
  test_type = "kruskal",
  variable = var,
  group = group_var,
  n_total = n_total,
  group_levels = paste(names(size_table), collapse = ","),
  group_sizes = paste(as.integer(size_table), collapse = ",")
))

## Kruskal posthoc (pairwise Wilcoxon)
pairs <- combn(levels, 2, simplify = FALSE)
p_values <- numeric(0)
posthoc_rows_local <- list()
for (pair in pairs) {
  g1 <- pair[1]
  g2 <- pair[2]
  x1 <- vec[groups == g1]
  x2 <- vec[groups == g2]
  n1 <- length(x1)
  n2 <- length(x2)
  test <- if (n1 > 0 && n2 > 0) safe_wilcox_test(
    x1,
    x2,
    alternative = "two.sided",
    conf.int = TRUE,
    conf.level = 0.95,
    exact = FALSE,
    correct = TRUE
  ) else NULL
  w_stat <- if (!is.null(test)) unname(test$statistic) else NA_real_
  u_stat <- calc_u_from_w(w_stat, n1)
  p_val <- if (!is.null(test)) test$p.value else NA_real_
  z_val <- calc_z_mann_whitney(u_stat, n1, n2)
  r_val <- calc_r_from_z(z_val, n1 + n2)
  posthoc_rows_local[[length(posthoc_rows_local) + 1]] <- list(
    test_type = "kruskal_posthoc",
    variable = var,
    group = group_var,
    group_1 = g1,
    group_2 = g2,
    n_1 = n1,
    n_2 = n2,
    statistic = u_stat,
    p = p_val,
    effect_size_value = r_val,
    ci_low = if (!is.null(test) && !is.null(test$conf.int)) test$conf.int[1] else NA_real_,
    ci_high = if (!is.null(test) && !is.null(test$conf.int)) test$conf.int[2] else NA_real_
  )
  p_values <- c(p_values, p_val)
}

if (length(posthoc_rows_local) > 0 && length(p_values) == length(posthoc_rows_local)) {
  p_adj <- p.adjust(p_values, method = posthoc_adjust)
  for (i in seq_along(posthoc_rows_local)) {
    posthoc_rows_local[[i]]$p_adj <- p_adj[i]
  }
}

for (row in posthoc_rows_local) {
  if (row$group_1 == "A" && row$group_2 == "B") {
    posthoc_rows[[length(posthoc_rows) + 1]] <- make_row(posthoc_cols, c(
      list(case_id = "kruskal_posthoc_A_B"),
      row
    ))
  }
}

## Case 5: Friedman + posthoc
case_id <- "friedman_pre_mid_post"
within_vars <- c("pre_score", "mid_score", "post_score")
subject_id <- "id"
effect_size <- "kendall_w"
digits <- 3
posthoc_adjust <- "BH"

data_subset <- df[, c(subject_id, within_vars), drop = FALSE]
complete <- complete.cases(data_subset)
data_subset <- data_subset[complete, , drop = FALSE]
n_total <- nrow(data_subset)
k <- length(within_vars)

test <- if (n_total > 0) tryCatch(friedman.test(as.matrix(data_subset[, within_vars, drop = FALSE])), error = function(e) NULL) else NULL
q_stat <- if (!is.null(test)) unname(test$statistic) else NA_real_
df_val <- if (!is.null(test)) unname(test$parameter) else NA_real_
p_val <- if (!is.null(test)) test$p.value else NA_real_
w_val <- calc_kendall_w(q_stat, n_total, k)

summary_rows[[length(summary_rows) + 1]] <- make_row(summary_cols, list(
  case_id = case_id,
  test_type = "Friedman",
  variable = paste(within_vars, collapse = ","),
  measure_1 = "",
  measure_2 = "",
  group = "",
  group_1 = "",
  group_2 = "",
  n_1 = NA_real_,
  n_2 = NA_real_,
  n_total = n_total,
  median_1 = NA_real_,
  median_2 = NA_real_,
  iqr_1 = NA_real_,
  iqr_2 = NA_real_,
  median = NA_real_,
  iqr = NA_real_,
  median_diff = NA_real_,
  iqr_diff = NA_real_,
  statistic = q_stat,
  df = df_val,
  p = p_val,
  effect_size_value = w_val,
  ci_low = NA_real_,
  ci_high = NA_real_,
  mu = NA_real_,
  opt_test = "friedman",
  opt_mode = "friedman",
  opt_within = paste(within_vars, collapse = ","),
  opt_subject_id = subject_id,
  opt_posthoc = "pairwise",
  opt_p_adjust = posthoc_adjust,
  opt_effect_size = effect_size,
  opt_digits = digits
))

diag_rows[[length(diag_rows) + 1]] <- make_row(diag_cols, list(
  case_id = "diag_friedman_pre_mid_post",
  test_type = "friedman",
  variable = paste(within_vars, collapse = ","),
  group = "",
  n_total = n_total,
  k = k
))

pairs <- combn(within_vars, 2, simplify = FALSE)
p_values <- numeric(0)
posthoc_rows_local <- list()

for (pair in pairs) {
  v1 <- pair[1]
  v2 <- pair[2]
  x1 <- data_subset[[v1]]
  x2 <- data_subset[[v2]]
  n <- length(x1)
  test <- if (n > 0) safe_wilcox_test(
    x1,
    x2,
    paired = TRUE,
    alternative = "two.sided",
    conf.int = TRUE,
    conf.level = 0.95,
    exact = FALSE,
    correct = TRUE
  ) else NULL
  stat_val <- if (!is.null(test)) unname(test$statistic) else NA_real_
  p_val <- if (!is.null(test)) test$p.value else NA_real_
  diffs <- x1 - x2
  rank_info <- calc_rank_biserial_signed(diffs)
  z_val <- calc_z_signed(rank_info$w_pos, rank_info$n)
  r_val <- calc_r_from_z(z_val, rank_info$n)
  posthoc_rows_local[[length(posthoc_rows_local) + 1]] <- list(
    test_type = "friedman_posthoc",
    variable = paste(within_vars, collapse = ","),
    group = "",
    group_1 = v1,
    group_2 = v2,
    n_1 = n,
    n_2 = NA_real_,
    statistic = stat_val,
    p = p_val,
    effect_size_value = r_val,
    ci_low = if (!is.null(test) && !is.null(test$conf.int)) test$conf.int[1] else NA_real_,
    ci_high = if (!is.null(test) && !is.null(test$conf.int)) test$conf.int[2] else NA_real_
  )
  p_values <- c(p_values, p_val)
}

if (length(posthoc_rows_local) > 0 && length(p_values) == length(posthoc_rows_local)) {
  p_adj <- p.adjust(p_values, method = posthoc_adjust)
  for (i in seq_along(posthoc_rows_local)) {
    posthoc_rows_local[[i]]$p_adj <- p_adj[i]
  }
}

for (row in posthoc_rows_local) {
  if (row$group_1 == "pre_score" && row$group_2 == "mid_score") {
    posthoc_rows[[length(posthoc_rows) + 1]] <- make_row(posthoc_cols, c(
      list(case_id = "friedman_posthoc_pre_mid"),
      row
    ))
  }
}

summary_df <- if (length(summary_rows) > 0) do.call(rbind, summary_rows) else data.frame()
posthoc_df <- if (length(posthoc_rows) > 0) do.call(rbind, posthoc_rows) else data.frame()
diagnostics_df <- if (length(diag_rows) > 0) do.call(rbind, diag_rows) else data.frame()

if (nrow(summary_df) > 0) {
  summary_df <- summary_df[, summary_cols]
}
if (nrow(posthoc_df) > 0) {
  posthoc_df <- posthoc_df[, posthoc_cols]
}
if (nrow(diagnostics_df) > 0) {
  diagnostics_df <- diagnostics_df[, diag_cols]
}

sanitize_numeric_cols <- function(df_in) {
  if (nrow(df_in) == 0) return(df_in)
  for (col in names(df_in)) {
    if (is.numeric(df_in[[col]])) {
      df_in[[col]] <- ifelse(is.finite(df_in[[col]]), df_in[[col]], NA_real_)
    }
  }
  df_in
}

summary_df <- sanitize_numeric_cols(summary_df)
posthoc_df <- sanitize_numeric_cols(posthoc_df)
diagnostics_df <- sanitize_numeric_cols(diagnostics_df)

write.csv(summary_df, out_summary, row.names = FALSE)
write.csv(posthoc_df, out_posthoc, row.names = FALSE)
write.csv(diagnostics_df, out_diagnostics, row.names = FALSE)
