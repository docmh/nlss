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
out_summary <- trim_arg(get_arg("--out", file.path("tests", "values", "correlations_golden.csv")))
out_diagnostics <- trim_arg(get_arg("--diagnostics-out", file.path("tests", "values", "correlations_diagnostics_golden.csv")))
out_comparison <- trim_arg(get_arg("--comparison-out", file.path("tests", "values", "correlations_comparison_golden.csv")))

if (is.null(data_path) || !file.exists(data_path)) {
  stop("Missing --data path.")
}
if (is.null(out_summary) || is.null(out_diagnostics) || is.null(out_comparison)) {
  stop("Missing output path(s).")
}

options(scipen = 999, digits = 15)

df <- read.csv(data_path, stringsAsFactors = FALSE)

get_complete_rows <- function(df_use) {
  if (nrow(df_use) == 0) return(logical(0))
  idx <- complete.cases(df_use)
  if (any(idx)) {
    finite_idx <- rep(TRUE, nrow(df_use))
    for (col in names(df_use)) {
      finite_idx <- finite_idx & is.finite(df_use[[col]])
    }
    idx <- idx & finite_idx
  }
  idx
}

build_pairs <- function(vars) {
  if (length(vars) < 2) {
    return(data.frame(var1 = character(0), var2 = character(0), stringsAsFactors = FALSE))
  }
  combo <- combn(vars, 2)
  data.frame(var1 = combo[1, ], var2 = combo[2, ], stringsAsFactors = FALSE)
}

build_cross_pairs <- function(x_vars, y_vars) {
  if (length(x_vars) == 0 || length(y_vars) == 0) {
    return(build_pairs(unique(c(x_vars, y_vars))))
  }
  grid <- expand.grid(var1 = x_vars, var2 = y_vars, stringsAsFactors = FALSE)
  grid <- grid[grid$var1 != grid$var2, , drop = FALSE]
  if (nrow(grid) == 0) return(grid)
  key <- paste(pmin(grid$var1, grid$var2), pmax(grid$var1, grid$var2), sep = "||")
  grid <- grid[!duplicated(key), , drop = FALSE]
  grid
}

calc_ci <- function(r, n, conf_level) {
  if (is.na(r) || n < 4 || abs(r) >= 1) return(c(NA_real_, NA_real_))
  z <- atanh(r)
  se <- 1 / sqrt(n - 3)
  z_crit <- qnorm((1 + conf_level) / 2)
  ci <- tanh(z + c(-1, 1) * z_crit * se)
  c(ci[1], ci[2])
}

prepare_partial <- function(df_use, var1, var2, controls, method) {
  data <- df_use[, c(var1, var2, controls), drop = FALSE]
  if (method != "pearson") {
    data <- as.data.frame(lapply(data, function(col) rank(col, ties.method = "average")))
  }
  x <- data[[var1]]
  y <- data[[var2]]
  controls_df <- data[, controls, drop = FALSE]
  if (ncol(controls_df) == 0) return(list(x = x, y = y))

  x_res <- tryCatch(
    resid(lm(x ~ ., data = data.frame(x = x, controls_df))),
    error = function(e) NULL
  )
  y_res <- tryCatch(
    resid(lm(y ~ ., data = data.frame(y = y, controls_df))),
    error = function(e) NULL
  )
  if (is.null(x_res) || is.null(y_res)) {
    return(list(x = rep(NA_real_, length(x)), y = rep(NA_real_, length(y))))
  }
  list(x = x_res, y = y_res)
}

safe_cor_test <- function(x, y, method, alternative, conf_level) {
  test <- tryCatch(
    suppressWarnings(cor.test(x, y, method = method, alternative = alternative, conf.level = conf_level)),
    error = function(e) NULL
  )
  if (is.null(test)) return(list(r = NA_real_, p = NA_real_))
  list(r = unname(test$estimate), p = test$p.value)
}

bootstrap_ci_cor <- function(df_use, var1, var2, controls, method, conf_level, n_boot) {
  n <- nrow(df_use)
  if (n < 3 || n_boot <= 0) return(c(NA_real_, NA_real_))
  stats <- numeric(n_boot)
  for (i in seq_len(n_boot)) {
    idx <- sample.int(n, size = n, replace = TRUE)
    boot_df <- df_use[idx, , drop = FALSE]
    if (length(controls) > 0) {
      partial_data <- prepare_partial(boot_df, var1, var2, controls, method)
      stats[i] <- suppressWarnings(cor(partial_data$x, partial_data$y, method = "pearson", use = "complete.obs"))
    } else {
      stats[i] <- suppressWarnings(cor(boot_df[[var1]], boot_df[[var2]], method = method, use = "complete.obs"))
    }
  }
  alpha <- (1 - conf_level) / 2
  quantile(stats, probs = c(alpha, 1 - alpha), na.rm = TRUE)
}

is_valid_fisher_r <- function(value) {
  !is.na(value) && is.finite(value) && abs(value) < 1
}

calc_z_p_value <- function(z_value, alternative) {
  if (is.na(z_value)) return(NA_real_)
  if (alternative == "greater") return(1 - pnorm(z_value))
  if (alternative == "less") return(pnorm(z_value))
  2 * pnorm(-abs(z_value))
}

fisher_z_test_r0 <- function(r, n, r0, alternative, controls_n = 0) {
  if (!is_valid_fisher_r(r) || !is_valid_fisher_r(r0) || is.na(n) || n <= (controls_n + 3)) {
    return(list(z = NA_real_, p = NA_real_))
  }
  z_r <- atanh(r)
  z_0 <- atanh(r0)
  se <- 1 / sqrt(n - controls_n - 3)
  z_val <- (z_r - z_0) / se
  list(z = z_val, p = calc_z_p_value(z_val, alternative))
}

fisher_z_test_independent <- function(r1, n1, r2, n2, alternative, controls_n = 0) {
  if (!is_valid_fisher_r(r1) || !is_valid_fisher_r(r2) || is.na(n1) || is.na(n2) ||
      n1 <= (controls_n + 3) || n2 <= (controls_n + 3)) {
    return(list(z = NA_real_, p = NA_real_))
  }
  z1 <- atanh(r1)
  z2 <- atanh(r2)
  se <- sqrt(1 / (n1 - controls_n - 3) + 1 / (n2 - controls_n - 3))
  z_val <- (z1 - z2) / se
  list(z = z_val, p = calc_z_p_value(z_val, alternative))
}

calc_skewness <- function(x) {
  n <- length(x)
  if (n < 3) return(NA_real_)
  mean_x <- mean(x)
  sd_x <- sd(x)
  if (is.na(sd_x) || sd_x == 0) return(NA_real_)
  sum(((x - mean_x) / sd_x)^3) * (n / ((n - 1) * (n - 2)))
}

calc_kurtosis <- function(x) {
  n <- length(x)
  if (n < 4) return(NA_real_)
  mean_x <- mean(x)
  sd_x <- sd(x)
  if (is.na(sd_x) || sd_x == 0) return(NA_real_)
  term1 <- (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3))
  term2 <- (3 * (n - 1)^2) / ((n - 2) * (n - 3))
  term1 * sum(((x - mean_x) / sd_x)^4) - term2
}

build_diagnostics <- function(df_sub, vars, group_label) {
  rows <- list()
  total_n <- nrow(df_sub)
  for (var in vars) {
    vec <- df_sub[[var]]
    missing_n <- sum(is.na(vec))
    missing_pct <- ifelse(total_n > 0, missing_n / total_n * 100, NA_real_)
    valid <- vec[!is.na(vec)]
    n <- length(valid)

    shapiro_w <- NA_real_
    shapiro_p <- NA_real_
    if (n >= 3 && n <= 5000) {
      test <- tryCatch(shapiro.test(valid), error = function(e) NULL)
      if (!is.null(test)) {
        shapiro_w <- unname(test$statistic)
        shapiro_p <- test$p.value
      }
    }

    rows[[length(rows) + 1]] <- data.frame(
      variable = var,
      group = group_label,
      n = n,
      total_n = total_n,
      missing_n = missing_n,
      missing_pct = missing_pct,
      skewness = calc_skewness(valid),
      kurtosis = calc_kurtosis(valid),
      shapiro_w = shapiro_w,
      shapiro_p = shapiro_p,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

compute_pair <- function(df_sub, var1, var2, group_label, method, alternative,
                         conf_level, missing_method, controls, complete_idx,
                         bootstrap, bootstrap_samples) {
  total_n <- nrow(df_sub)
  pair_vars <- unique(c(var1, var2, controls))
  if (missing_method == "complete" && !is.null(complete_idx)) {
    df_use <- df_sub[complete_idx, pair_vars, drop = FALSE]
  } else {
    df_use <- df_sub[, pair_vars, drop = FALSE]
    row_idx <- get_complete_rows(df_use)
    df_use <- df_use[row_idx, , drop = FALSE]
  }

  n_used <- nrow(df_use)
  missing_n <- total_n - n_used
  missing_pct <- ifelse(total_n > 0, missing_n / total_n * 100, NA_real_)

  r <- NA_real_
  p_val <- NA_real_
  ci_low <- NA_real_
  ci_high <- NA_real_
  boot_ci_low <- NA_real_
  boot_ci_high <- NA_real_

  if (n_used >= 3) {
    if (length(controls) > 0) {
      partial_data <- prepare_partial(df_use, var1, var2, controls, method)
      test <- safe_cor_test(partial_data$x, partial_data$y, "pearson", alternative, conf_level)
      r <- test$r
      p_val <- test$p
    } else {
      test <- safe_cor_test(df_use[[var1]], df_use[[var2]], method, alternative, conf_level)
      r <- test$r
      p_val <- test$p
    }

    if (!is.na(r) && (method == "pearson" || length(controls) > 0)) {
      ci_vals <- calc_ci(r, n_used, conf_level)
      ci_low <- ci_vals[1]
      ci_high <- ci_vals[2]
    }
  }

  if (bootstrap && n_used >= 3 && bootstrap_samples > 0) {
    boot_ci <- bootstrap_ci_cor(df_use, var1, var2, controls, method, conf_level, bootstrap_samples)
    boot_ci_low <- boot_ci[1]
    boot_ci_high <- boot_ci[2]
  }

  data.frame(
    var1 = var1,
    var2 = var2,
    group = group_label,
    method = method,
    alternative = alternative,
    controls = ifelse(length(controls) > 0, paste(controls, collapse = ","), ""),
    partial = length(controls) > 0,
    n = n_used,
    total_n = total_n,
    missing_n = missing_n,
    missing_pct = missing_pct,
    r = r,
    p_value = p_val,
    ci_low = ci_low,
    ci_high = ci_high,
    boot_ci_low = boot_ci_low,
    boot_ci_high = boot_ci_high,
    stringsAsFactors = FALSE
  )
}

compute_analysis <- function(df, vars = character(0), x_vars = character(0), y_vars = character(0),
                             group_var = NULL, method = "pearson", missing_method = "pairwise",
                             alternative = "two.sided", controls = character(0), conf_level = 0.95,
                             p_adjust = "none", r0 = NULL, bootstrap = FALSE, bootstrap_samples = 1000,
                             compare_groups = FALSE) {
  pairs <- if (length(x_vars) > 0 || length(y_vars) > 0) build_cross_pairs(x_vars, y_vars) else build_pairs(vars)
  if (nrow(pairs) == 0) stop("No variable pairs available.")

  summary_list <- list()
  diagnostics_list <- list()

  if (!is.null(group_var) && nzchar(group_var)) {
    group_vec <- df[[group_var]]
    group_levels <- unique(group_vec)
    for (g in group_levels) {
      idx <- if (is.na(g)) is.na(group_vec) else group_vec == g
      df_sub <- df[idx, , drop = FALSE]
      group_label <- ifelse(is.na(g), "NA", as.character(g))

      complete_idx <- NULL
      if (missing_method == "complete") {
        complete_df <- df_sub[, unique(c(pairs$var1, pairs$var2, controls)), drop = FALSE]
        complete_idx <- get_complete_rows(complete_df)
      }

      for (i in seq_len(nrow(pairs))) {
        row <- pairs[i, ]
        summary_list[[length(summary_list) + 1]] <- compute_pair(
          df_sub, row$var1, row$var2, group_label,
          method, alternative, conf_level, missing_method, controls, complete_idx,
          bootstrap, bootstrap_samples
        )
      }

      diagnostics_vars <- unique(c(pairs$var1, pairs$var2, controls))
      diagnostics_list[[length(diagnostics_list) + 1]] <- build_diagnostics(df_sub, diagnostics_vars, group_label)
    }
  } else {
    complete_idx <- NULL
    if (missing_method == "complete") {
      complete_df <- df[, unique(c(pairs$var1, pairs$var2, controls)), drop = FALSE]
      complete_idx <- get_complete_rows(complete_df)
    }
    for (i in seq_len(nrow(pairs))) {
      row <- pairs[i, ]
      summary_list[[length(summary_list) + 1]] <- compute_pair(
        df, row$var1, row$var2, "",
        method, alternative, conf_level, missing_method, controls, complete_idx,
        bootstrap, bootstrap_samples
      )
    }

    diagnostics_vars <- unique(c(pairs$var1, pairs$var2, controls))
    diagnostics_list[[length(diagnostics_list) + 1]] <- build_diagnostics(df, diagnostics_vars, "")
  }

  summary_df <- do.call(rbind, summary_list)
  diagnostics_df <- do.call(rbind, diagnostics_list)

  summary_df$missing_method <- missing_method
  summary_df$conf_level <- conf_level

  summary_df$p_adjusted <- NA_real_
  summary_df$p_adjust_method <- p_adjust
  if (p_adjust != "none") {
    groups <- unique(summary_df$group)
    for (g in groups) {
      idx <- summary_df$group == g & !is.na(summary_df$p_value)
      if (any(idx)) {
        summary_df$p_adjusted[idx] <- p.adjust(summary_df$p_value[idx], method = p_adjust)
      }
    }
  }

  summary_df$r0 <- if (!is.null(r0)) r0 else NA_real_
  summary_df$z_r0 <- NA_real_
  summary_df$p_r0 <- NA_real_
  if (!is.null(r0) && nrow(summary_df) > 0) {
    for (i in seq_len(nrow(summary_df))) {
      test <- fisher_z_test_r0(summary_df$r[i], summary_df$n[i], r0, alternative, length(controls))
      summary_df$z_r0[i] <- test$z
      summary_df$p_r0[i] <- test$p
    }
  }

  comparison_df <- NULL
  if (compare_groups) {
    if (is.null(group_var) || !nzchar(group_var)) stop("compare_groups requires group_var")
    group_levels <- unique(df[[group_var]])
    group_levels <- group_levels[!is.na(group_levels)]
    if (length(group_levels) != 2) stop("Group comparisons require exactly two non-missing group levels.")
    group1 <- as.character(group_levels[1])
    group2 <- as.character(group_levels[2])
    compare_rows <- list()
    for (i in seq_len(nrow(pairs))) {
      row <- pairs[i, ]
      row1 <- summary_df[summary_df$group == group1 & summary_df$var1 == row$var1 & summary_df$var2 == row$var2, , drop = FALSE]
      row2 <- summary_df[summary_df$group == group2 & summary_df$var1 == row$var1 & summary_df$var2 == row$var2, , drop = FALSE]
      r1 <- ifelse(nrow(row1) > 0, row1$r[1], NA_real_)
      r2 <- ifelse(nrow(row2) > 0, row2$r[1], NA_real_)
      n1 <- ifelse(nrow(row1) > 0, row1$n[1], NA_real_)
      n2 <- ifelse(nrow(row2) > 0, row2$n[1], NA_real_)
      test <- fisher_z_test_independent(r1, n1, r2, n2, alternative, length(controls))
      compare_rows[[length(compare_rows) + 1]] <- data.frame(
        var1 = row$var1,
        var2 = row$var2,
        group1 = group1,
        group2 = group2,
        r1 = r1,
        r2 = r2,
        n1 = n1,
        n2 = n2,
        z = test$z,
        p_value = test$p,
        stringsAsFactors = FALSE
      )
    }
    comparison_df <- do.call(rbind, compare_rows)
  }

  list(summary_df = summary_df, diagnostics_df = diagnostics_df, comparison_df = comparison_df)
}

find_summary_row <- function(summary_df, var1, var2, group = "", method = NULL, controls = NULL,
                             missing_method = NULL, alternative = NULL) {
  rows <- summary_df[summary_df$var1 == var1 & summary_df$var2 == var2 & summary_df$group == group, , drop = FALSE]
  if (!is.null(method)) rows <- rows[rows$method == method, , drop = FALSE]
  if (!is.null(controls)) rows <- rows[rows$controls == controls, , drop = FALSE]
  if (!is.null(missing_method)) rows <- rows[rows$missing_method == missing_method, , drop = FALSE]
  if (!is.null(alternative)) rows <- rows[rows$alternative == alternative, , drop = FALSE]
  if (nrow(rows) == 0) stop("Summary row not found for ", var1, ", ", var2)
  rows[1, , drop = FALSE]
}

find_diagnostic_row <- function(diagnostics_df, variable, group = "") {
  rows <- diagnostics_df[diagnostics_df$variable == variable & diagnostics_df$group == group, , drop = FALSE]
  if (nrow(rows) == 0) stop("Diagnostics row not found for ", variable)
  rows[1, , drop = FALSE]
}

find_comparison_row <- function(comparison_df, var1, var2, group1, group2) {
  rows <- comparison_df[comparison_df$var1 == var1 & comparison_df$var2 == var2 &
                          comparison_df$group1 == group1 & comparison_df$group2 == group2, , drop = FALSE]
  if (nrow(rows) == 0) stop("Comparison row not found for ", var1, ", ", var2)
  rows[1, , drop = FALSE]
}

summary_rows <- list()

# Baseline Pearson (pairwise)
res_baseline <- compute_analysis(
  df,
  vars = c("x1", "x2", "x3"),
  method = "pearson",
  missing_method = "pairwise",
  alternative = "two.sided",
  conf_level = 0.95,
  p_adjust = "none"
)
row_baseline <- find_summary_row(res_baseline$summary_df, "x1", "x2", "", method = "pearson", controls = "")
summary_rows[[length(summary_rows) + 1]] <- data.frame(
  case_id = "pearson_pairwise_x1_x2",
  var1 = row_baseline$var1,
  var2 = row_baseline$var2,
  group = row_baseline$group,
  method = row_baseline$method,
  alternative = row_baseline$alternative,
  controls = row_baseline$controls,
  partial = row_baseline$partial,
  missing_method = row_baseline$missing_method,
  conf_level = row_baseline$conf_level,
  p_adjust_method = row_baseline$p_adjust_method,
  r0 = row_baseline$r0,
  n = row_baseline$n,
  total_n = row_baseline$total_n,
  missing_n = row_baseline$missing_n,
  missing_pct = row_baseline$missing_pct,
  r = row_baseline$r,
  p_value = row_baseline$p_value,
  ci_low = row_baseline$ci_low,
  ci_high = row_baseline$ci_high,
  boot_ci_low = row_baseline$boot_ci_low,
  boot_ci_high = row_baseline$boot_ci_high,
  p_adjusted = row_baseline$p_adjusted,
  z_r0 = row_baseline$z_r0,
  p_r0 = row_baseline$p_r0,
  stringsAsFactors = FALSE
)

# Spearman cross (complete, Holm p-adjust)
res_cross <- compute_analysis(
  df,
  x_vars = c("x1", "x2"),
  y_vars = c("mediator", "outcome_anova"),
  method = "spearman",
  missing_method = "complete",
  alternative = "two.sided",
  conf_level = 0.95,
  p_adjust = "holm"
)
row_cross <- find_summary_row(res_cross$summary_df, "x1", "mediator", "", method = "spearman", controls = "", missing_method = "complete")
summary_rows[[length(summary_rows) + 1]] <- data.frame(
  case_id = "spearman_complete_x1_mediator",
  var1 = row_cross$var1,
  var2 = row_cross$var2,
  group = row_cross$group,
  method = row_cross$method,
  alternative = row_cross$alternative,
  controls = row_cross$controls,
  partial = row_cross$partial,
  missing_method = row_cross$missing_method,
  conf_level = row_cross$conf_level,
  p_adjust_method = row_cross$p_adjust_method,
  r0 = row_cross$r0,
  n = row_cross$n,
  total_n = row_cross$total_n,
  missing_n = row_cross$missing_n,
  missing_pct = row_cross$missing_pct,
  r = row_cross$r,
  p_value = row_cross$p_value,
  ci_low = row_cross$ci_low,
  ci_high = row_cross$ci_high,
  boot_ci_low = row_cross$boot_ci_low,
  boot_ci_high = row_cross$boot_ci_high,
  p_adjusted = row_cross$p_adjusted,
  z_r0 = row_cross$z_r0,
  p_r0 = row_cross$p_r0,
  stringsAsFactors = FALSE
)

# Partial Spearman with controls
res_partial <- compute_analysis(
  df,
  vars = c("x1", "x2", "x3"),
  method = "spearman",
  missing_method = "pairwise",
  alternative = "two.sided",
  controls = c("age", "income"),
  conf_level = 0.95,
  p_adjust = "none"
)
row_partial <- find_summary_row(res_partial$summary_df, "x1", "x2", "", method = "spearman", controls = "age,income")
summary_rows[[length(summary_rows) + 1]] <- data.frame(
  case_id = "partial_spearman_x1_x2",
  var1 = row_partial$var1,
  var2 = row_partial$var2,
  group = row_partial$group,
  method = row_partial$method,
  alternative = row_partial$alternative,
  controls = row_partial$controls,
  partial = row_partial$partial,
  missing_method = row_partial$missing_method,
  conf_level = row_partial$conf_level,
  p_adjust_method = row_partial$p_adjust_method,
  r0 = row_partial$r0,
  n = row_partial$n,
  total_n = row_partial$total_n,
  missing_n = row_partial$missing_n,
  missing_pct = row_partial$missing_pct,
  r = row_partial$r,
  p_value = row_partial$p_value,
  ci_low = row_partial$ci_low,
  ci_high = row_partial$ci_high,
  boot_ci_low = row_partial$boot_ci_low,
  boot_ci_high = row_partial$boot_ci_high,
  p_adjusted = row_partial$p_adjusted,
  z_r0 = row_partial$z_r0,
  p_r0 = row_partial$p_r0,
  stringsAsFactors = FALSE
)

# Kendall (pairwise)
res_kendall <- compute_analysis(
  df,
  vars = c("x1", "x2", "x3"),
  method = "kendall",
  missing_method = "pairwise",
  alternative = "two.sided",
  conf_level = 0.95,
  p_adjust = "none"
)
row_kendall <- find_summary_row(res_kendall$summary_df, "x1", "x2", "", method = "kendall", controls = "")
summary_rows[[length(summary_rows) + 1]] <- data.frame(
  case_id = "kendall_pairwise_x1_x2",
  var1 = row_kendall$var1,
  var2 = row_kendall$var2,
  group = row_kendall$group,
  method = row_kendall$method,
  alternative = row_kendall$alternative,
  controls = row_kendall$controls,
  partial = row_kendall$partial,
  missing_method = row_kendall$missing_method,
  conf_level = row_kendall$conf_level,
  p_adjust_method = row_kendall$p_adjust_method,
  r0 = row_kendall$r0,
  n = row_kendall$n,
  total_n = row_kendall$total_n,
  missing_n = row_kendall$missing_n,
  missing_pct = row_kendall$missing_pct,
  r = row_kendall$r,
  p_value = row_kendall$p_value,
  ci_low = row_kendall$ci_low,
  ci_high = row_kendall$ci_high,
  boot_ci_low = row_kendall$boot_ci_low,
  boot_ci_high = row_kendall$boot_ci_high,
  p_adjusted = row_kendall$p_adjusted,
  z_r0 = row_kendall$z_r0,
  p_r0 = row_kendall$p_r0,
  stringsAsFactors = FALSE
)

# Fisher r0 test
res_r0 <- compute_analysis(
  df,
  vars = c("x1", "x2", "x3"),
  method = "pearson",
  missing_method = "pairwise",
  alternative = "two.sided",
  conf_level = 0.95,
  p_adjust = "none",
  r0 = 0.2
)
row_r0 <- find_summary_row(res_r0$summary_df, "x1", "x2", "", method = "pearson", controls = "")
summary_rows[[length(summary_rows) + 1]] <- data.frame(
  case_id = "r0_pearson_x1_x2",
  var1 = row_r0$var1,
  var2 = row_r0$var2,
  group = row_r0$group,
  method = row_r0$method,
  alternative = row_r0$alternative,
  controls = row_r0$controls,
  partial = row_r0$partial,
  missing_method = row_r0$missing_method,
  conf_level = row_r0$conf_level,
  p_adjust_method = row_r0$p_adjust_method,
  r0 = row_r0$r0,
  n = row_r0$n,
  total_n = row_r0$total_n,
  missing_n = row_r0$missing_n,
  missing_pct = row_r0$missing_pct,
  r = row_r0$r,
  p_value = row_r0$p_value,
  ci_low = row_r0$ci_low,
  ci_high = row_r0$ci_high,
  boot_ci_low = row_r0$boot_ci_low,
  boot_ci_high = row_r0$boot_ci_high,
  p_adjusted = row_r0$p_adjusted,
  z_r0 = row_r0$z_r0,
  p_r0 = row_r0$p_r0,
  stringsAsFactors = FALSE
)

# Bootstrap Pearson
set.seed(42)
res_boot <- compute_analysis(
  df,
  vars = c("x1", "x2", "x3"),
  method = "pearson",
  missing_method = "pairwise",
  alternative = "two.sided",
  conf_level = 0.95,
  p_adjust = "none",
  bootstrap = TRUE,
  bootstrap_samples = 200
)
row_boot <- find_summary_row(res_boot$summary_df, "x1", "x2", "", method = "pearson", controls = "")
summary_rows[[length(summary_rows) + 1]] <- data.frame(
  case_id = "bootstrap_pearson_x1_x2",
  var1 = row_boot$var1,
  var2 = row_boot$var2,
  group = row_boot$group,
  method = row_boot$method,
  alternative = row_boot$alternative,
  controls = row_boot$controls,
  partial = row_boot$partial,
  missing_method = row_boot$missing_method,
  conf_level = row_boot$conf_level,
  p_adjust_method = row_boot$p_adjust_method,
  r0 = row_boot$r0,
  n = row_boot$n,
  total_n = row_boot$total_n,
  missing_n = row_boot$missing_n,
  missing_pct = row_boot$missing_pct,
  r = row_boot$r,
  p_value = row_boot$p_value,
  ci_low = row_boot$ci_low,
  ci_high = row_boot$ci_high,
  boot_ci_low = row_boot$boot_ci_low,
  boot_ci_high = row_boot$boot_ci_high,
  p_adjusted = row_boot$p_adjusted,
  z_r0 = row_boot$z_r0,
  p_r0 = row_boot$p_r0,
  stringsAsFactors = FALSE
)

# Grouped Pearson (group2)
res_group <- compute_analysis(
  df,
  vars = c("x1", "x2", "x3"),
  group_var = "group2",
  method = "pearson",
  missing_method = "pairwise",
  alternative = "two.sided",
  conf_level = 0.95,
  p_adjust = "none"
)
row_group <- find_summary_row(res_group$summary_df, "x1", "x2", "control", method = "pearson", controls = "")
summary_rows[[length(summary_rows) + 1]] <- data.frame(
  case_id = "grouped_pearson_x1_x2_control",
  var1 = row_group$var1,
  var2 = row_group$var2,
  group = row_group$group,
  method = row_group$method,
  alternative = row_group$alternative,
  controls = row_group$controls,
  partial = row_group$partial,
  missing_method = row_group$missing_method,
  conf_level = row_group$conf_level,
  p_adjust_method = row_group$p_adjust_method,
  r0 = row_group$r0,
  n = row_group$n,
  total_n = row_group$total_n,
  missing_n = row_group$missing_n,
  missing_pct = row_group$missing_pct,
  r = row_group$r,
  p_value = row_group$p_value,
  ci_low = row_group$ci_low,
  ci_high = row_group$ci_high,
  boot_ci_low = row_group$boot_ci_low,
  boot_ci_high = row_group$boot_ci_high,
  p_adjusted = row_group$p_adjusted,
  z_r0 = row_group$z_r0,
  p_r0 = row_group$p_r0,
  stringsAsFactors = FALSE
)

summary_golden <- do.call(rbind, summary_rows)
write.csv(summary_golden, out_summary, row.names = FALSE)

# Diagnostics golden (from baseline)
row_diag <- find_diagnostic_row(res_baseline$diagnostics_df, "x1", "")
diagnostics_golden <- data.frame(
  case_id = "diagnostic_x1",
  variable = row_diag$variable,
  group = row_diag$group,
  n = row_diag$n,
  total_n = row_diag$total_n,
  missing_n = row_diag$missing_n,
  missing_pct = row_diag$missing_pct,
  skewness = row_diag$skewness,
  kurtosis = row_diag$kurtosis,
  shapiro_w = row_diag$shapiro_w,
  shapiro_p = row_diag$shapiro_p,
  stringsAsFactors = FALSE
)
write.csv(diagnostics_golden, out_diagnostics, row.names = FALSE)

# Comparison golden (group2 compare)
res_compare <- compute_analysis(
  df,
  vars = c("x1", "x2", "x3"),
  group_var = "group2",
  method = "pearson",
  missing_method = "pairwise",
  alternative = "two.sided",
  conf_level = 0.95,
  p_adjust = "none",
  compare_groups = TRUE
)
if (is.null(res_compare$comparison_df)) {
  stop("comparison_df missing for compare_groups")
}
comp_row <- find_comparison_row(res_compare$comparison_df, "x1", "x2", "treatment", "control")
comparison_golden <- data.frame(
  case_id = "compare_groups_x1_x2",
  var1 = comp_row$var1,
  var2 = comp_row$var2,
  group1 = comp_row$group1,
  group2 = comp_row$group2,
  r1 = comp_row$r1,
  r2 = comp_row$r2,
  n1 = comp_row$n1,
  n2 = comp_row$n2,
  z = comp_row$z,
  p_value = comp_row$p_value,
  stringsAsFactors = FALSE
)
write.csv(comparison_golden, out_comparison, row.names = FALSE)

cat("Wrote correlations golden values to", out_summary, "\n")
cat("Wrote correlations diagnostics goldens to", out_diagnostics, "\n")
cat("Wrote correlations comparison goldens to", out_comparison, "\n")
