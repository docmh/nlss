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
out_summary <- trim_arg(get_arg("--out", file.path("tests", "values", "t_test_golden.csv")))
out_diagnostics <- trim_arg(get_arg("--diagnostics-out", file.path("tests", "values", "t_test_diagnostics_golden.csv")))

if (is.null(data_path) || !file.exists(data_path)) {
  stop("Missing --data path.")
}
if (is.null(out_summary) || is.null(out_diagnostics)) {
  stop("Missing output path(s).")
}

options(scipen = 999, digits = 15)

safe_shapiro <- function(values) {
  n <- length(values)
  if (n < 3 || n > 5000) return(list(w = NA_real_, p = NA_real_))
  test <- tryCatch(shapiro.test(values), error = function(e) NULL)
  if (is.null(test)) return(list(w = NA_real_, p = NA_real_))
  list(w = unname(test$statistic), p = test$p.value)
}

calc_d_one_sample <- function(mean_diff, sd_val) {
  if (is.na(sd_val) || sd_val == 0) return(NA_real_)
  mean_diff / sd_val
}

calc_d_paired <- function(mean_diff, sd_diff) {
  if (is.na(sd_diff) || sd_diff == 0) return(NA_real_)
  mean_diff / sd_diff
}

calc_d_independent <- function(mean_diff, sd1, sd2, n1, n2) {
  if (n1 < 2 || n2 < 2 || is.na(sd1) || is.na(sd2)) return(NA_real_)
  pooled <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  if (is.na(pooled) || pooled == 0) return(NA_real_)
  mean_diff / pooled
}

bootstrap_ci <- function(values, stat_fn, n_boot, conf_level) {
  stats <- numeric(0)
  n <- length(values)
  for (i in seq_len(n_boot)) {
    sample_idx <- sample.int(n, size = n, replace = TRUE)
    stats[i] <- stat_fn(values[sample_idx])
  }
  alpha <- (1 - conf_level) / 2
  quantile(stats, probs = c(alpha, 1 - alpha), na.rm = TRUE)
}

bootstrap_ci_independent <- function(x1, x2, stat_fn, n_boot, conf_level) {
  n1 <- length(x1)
  n2 <- length(x2)
  stats <- numeric(0)
  for (i in seq_len(n_boot)) {
    s1 <- sample(x1, size = n1, replace = TRUE)
    s2 <- sample(x2, size = n2, replace = TRUE)
    stats[i] <- stat_fn(s1, s2)
  }
  alpha <- (1 - conf_level) / 2
  quantile(stats, probs = c(alpha, 1 - alpha), na.rm = TRUE)
}

build_summary_row <- function(
  case_id,
  mode,
  vars,
  x,
  y,
  group,
  alternative,
  var_equal,
  conf_level,
  bootstrap,
  bootstrap_samples,
  test_type,
  variable,
  measure_1,
  measure_2,
  group_1,
  group_2,
  n_1,
  n_2,
  mean_1,
  mean_2,
  sd_1,
  sd_2,
  mean_diff,
  t_stat,
  df_val,
  p_val,
  d_val,
  ci_low,
  ci_high,
  boot_ci_low,
  boot_ci_high,
  boot_d_low,
  boot_d_high,
  mu_val
) {
  data.frame(
    case_id = case_id,
    mode = mode,
    vars = vars,
    x = x,
    y = y,
    group = group,
    alternative = alternative,
    var_equal = var_equal,
    conf_level = conf_level,
    bootstrap = bootstrap,
    bootstrap_samples = bootstrap_samples,
    test_type = test_type,
    variable = variable,
    measure_1 = measure_1,
    measure_2 = measure_2,
    group_1 = group_1,
    group_2 = group_2,
    n_1 = n_1,
    n_2 = n_2,
    mean_1 = mean_1,
    mean_2 = mean_2,
    sd_1 = sd_1,
    sd_2 = sd_2,
    mean_diff = mean_diff,
    t = t_stat,
    df = df_val,
    p = p_val,
    d = d_val,
    ci_low = ci_low,
    ci_high = ci_high,
    boot_ci_low = boot_ci_low,
    boot_ci_high = boot_ci_high,
    boot_d_ci_low = boot_d_low,
    boot_d_ci_high = boot_d_high,
    mu = mu_val,
    stringsAsFactors = FALSE
  )
}

compute_one_sample <- function(df, case_id, var, mu, alternative, conf_level, bootstrap, bootstrap_samples, seed) {
  vec <- df[[var]]
  if (!is.numeric(vec)) stop(paste("Variable is not numeric:", var))
  clean <- vec[!is.na(vec)]
  n <- length(clean)
  mean_val <- ifelse(n > 0, mean(clean), NA_real_)
  sd_val <- ifelse(n > 1, sd(clean), NA_real_)
  mean_diff <- ifelse(n > 0, mean_val - mu, NA_real_)
  test <- if (n >= 2) {
    tryCatch(t.test(clean, mu = mu, alternative = alternative, conf.level = conf_level), error = function(e) NULL)
  } else {
    NULL
  }
  t_stat <- if (!is.null(test)) unname(test$statistic) else NA_real_
  df_val <- if (!is.null(test)) unname(test$parameter) else NA_real_
  p_val <- if (!is.null(test)) test$p.value else NA_real_
  ci_low <- if (!is.null(test)) test$conf.int[1] else NA_real_
  ci_high <- if (!is.null(test)) test$conf.int[2] else NA_real_
  d_val <- calc_d_one_sample(mean_diff, sd_val)

  boot_ci_low <- NA_real_
  boot_ci_high <- NA_real_
  boot_d_low <- NA_real_
  boot_d_high <- NA_real_
  if (bootstrap && n >= 2) {
    if (!is.null(seed) && !is.na(seed)) {
      set.seed(as.numeric(seed))
    }
    boot_ci <- bootstrap_ci(
      clean,
      function(x) mean(x) - mu,
      n_boot = bootstrap_samples,
      conf_level = conf_level
    )
    boot_ci_low <- boot_ci[1]
    boot_ci_high <- boot_ci[2]
    boot_d <- bootstrap_ci(
      clean,
      function(x) calc_d_one_sample(mean(x) - mu, sd(x)),
      n_boot = bootstrap_samples,
      conf_level = conf_level
    )
    boot_d_low <- boot_d[1]
    boot_d_high <- boot_d[2]
  }

  shapiro <- safe_shapiro(clean)
  diagnostics <- data.frame(
    case_id = paste0("diag_", case_id),
    test_type = "one_sample",
    variable = var,
    group = "",
    n = n,
    shapiro_w = shapiro$w,
    shapiro_p = shapiro$p,
    var_test_f = NA_real_,
    var_test_p = NA_real_,
    stringsAsFactors = FALSE
  )

  summary_row <- build_summary_row(
    case_id = case_id,
    mode = "one_sample",
    vars = var,
    x = "",
    y = "",
    group = "",
    alternative = alternative,
    var_equal = NA,
    conf_level = conf_level,
    bootstrap = bootstrap,
    bootstrap_samples = ifelse(bootstrap, bootstrap_samples, NA_real_),
    test_type = "one_sample",
    variable = var,
    measure_1 = "",
    measure_2 = "",
    group_1 = "",
    group_2 = "",
    n_1 = n,
    n_2 = NA_real_,
    mean_1 = mean_val,
    mean_2 = NA_real_,
    sd_1 = sd_val,
    sd_2 = NA_real_,
    mean_diff = mean_diff,
    t_stat = t_stat,
    df_val = df_val,
    p_val = p_val,
    d_val = d_val,
    ci_low = ci_low,
    ci_high = ci_high,
    boot_ci_low = boot_ci_low,
    boot_ci_high = boot_ci_high,
    boot_d_low = boot_d_low,
    boot_d_high = boot_d_high,
    mu_val = mu
  )
  list(summary = summary_row, diagnostics = diagnostics)
}

compute_independent <- function(df, case_id, var, group_var, alternative, var_equal, conf_level, bootstrap, bootstrap_samples, seed) {
  vec <- df[[var]]
  if (!is.numeric(vec)) stop(paste("Variable is not numeric:", var))
  group_vals <- df[[group_var]]
  levels <- unique(group_vals[!is.na(group_vals)])
  if (length(levels) != 2) stop("Grouping variable must have exactly two levels.")
  group1 <- as.character(levels[1])
  group2 <- as.character(levels[2])

  complete <- !is.na(vec) & !is.na(group_vals)
  vec <- vec[complete]
  groups <- as.character(group_vals[complete])

  x1 <- vec[groups == group1]
  x2 <- vec[groups == group2]
  n1 <- length(x1)
  n2 <- length(x2)
  mean1 <- ifelse(n1 > 0, mean(x1), NA_real_)
  mean2 <- ifelse(n2 > 0, mean(x2), NA_real_)
  sd1 <- ifelse(n1 > 1, sd(x1), NA_real_)
  sd2 <- ifelse(n2 > 1, sd(x2), NA_real_)
  mean_diff <- ifelse(n1 > 0 && n2 > 0, mean1 - mean2, NA_real_)

  test <- if (n1 >= 2 && n2 >= 2) {
    tryCatch(t.test(x1, x2, alternative = alternative, var.equal = var_equal, conf.level = conf_level),
             error = function(e) NULL)
  } else {
    NULL
  }
  t_stat <- if (!is.null(test)) unname(test$statistic) else NA_real_
  df_val <- if (!is.null(test)) unname(test$parameter) else NA_real_
  p_val <- if (!is.null(test)) test$p.value else NA_real_
  ci_low <- if (!is.null(test)) test$conf.int[1] else NA_real_
  ci_high <- if (!is.null(test)) test$conf.int[2] else NA_real_
  d_val <- calc_d_independent(mean_diff, sd1, sd2, n1, n2)

  boot_ci_low <- NA_real_
  boot_ci_high <- NA_real_
  boot_d_low <- NA_real_
  boot_d_high <- NA_real_
  if (bootstrap && n1 >= 2 && n2 >= 2) {
    if (!is.null(seed) && !is.na(seed)) {
      set.seed(as.numeric(seed))
    }
    boot_ci <- bootstrap_ci_independent(
      x1,
      x2,
      function(a, b) mean(a) - mean(b),
      n_boot = bootstrap_samples,
      conf_level = conf_level
    )
    boot_ci_low <- boot_ci[1]
    boot_ci_high <- boot_ci[2]
    boot_d <- bootstrap_ci_independent(
      x1,
      x2,
      function(a, b) {
        mean_diff_boot <- mean(a) - mean(b)
        calc_d_independent(mean_diff_boot, sd(a), sd(b), length(a), length(b))
      },
      n_boot = bootstrap_samples,
      conf_level = conf_level
    )
    boot_d_low <- boot_d[1]
    boot_d_high <- boot_d[2]
  }

  shapiro1 <- safe_shapiro(x1)
  shapiro2 <- safe_shapiro(x2)
  var_test <- if (n1 >= 2 && n2 >= 2) {
    tryCatch(var.test(x1, x2), error = function(e) NULL)
  } else {
    NULL
  }

  diagnostics <- list(
    data.frame(
      case_id = paste0("diag_", case_id, "_", group1),
      test_type = "independent",
      variable = var,
      group = group1,
      n = n1,
      shapiro_w = shapiro1$w,
      shapiro_p = shapiro1$p,
      var_test_f = NA_real_,
      var_test_p = NA_real_,
      stringsAsFactors = FALSE
    ),
    data.frame(
      case_id = paste0("diag_", case_id, "_", group2),
      test_type = "independent",
      variable = var,
      group = group2,
      n = n2,
      shapiro_w = shapiro2$w,
      shapiro_p = shapiro2$p,
      var_test_f = if (!is.null(var_test)) unname(var_test$statistic) else NA_real_,
      var_test_p = if (!is.null(var_test)) var_test$p.value else NA_real_,
      stringsAsFactors = FALSE
    )
  )

  summary_row <- build_summary_row(
    case_id = case_id,
    mode = "independent",
    vars = var,
    x = "",
    y = "",
    group = group_var,
    alternative = alternative,
    var_equal = var_equal,
    conf_level = conf_level,
    bootstrap = bootstrap,
    bootstrap_samples = ifelse(bootstrap, bootstrap_samples, NA_real_),
    test_type = "independent",
    variable = var,
    measure_1 = "",
    measure_2 = "",
    group_1 = group1,
    group_2 = group2,
    n_1 = n1,
    n_2 = n2,
    mean_1 = mean1,
    mean_2 = mean2,
    sd_1 = sd1,
    sd_2 = sd2,
    mean_diff = mean_diff,
    t_stat = t_stat,
    df_val = df_val,
    p_val = p_val,
    d_val = d_val,
    ci_low = ci_low,
    ci_high = ci_high,
    boot_ci_low = boot_ci_low,
    boot_ci_high = boot_ci_high,
    boot_d_low = boot_d_low,
    boot_d_high = boot_d_high,
    mu_val = NA_real_
  )
  list(summary = summary_row, diagnostics = diagnostics)
}

compute_paired <- function(df, case_id, x_var, y_var, alternative, conf_level, bootstrap, bootstrap_samples, seed) {
  x <- df[[x_var]]
  y <- df[[y_var]]
  if (!is.numeric(x) || !is.numeric(y)) stop("Paired variables must be numeric.")
  complete <- !is.na(x) & !is.na(y)
  x <- x[complete]
  y <- y[complete]
  n <- length(x)
  mean1 <- ifelse(n > 0, mean(x), NA_real_)
  mean2 <- ifelse(n > 0, mean(y), NA_real_)
  sd1 <- ifelse(n > 1, sd(x), NA_real_)
  sd2 <- ifelse(n > 1, sd(y), NA_real_)
  diff_vals <- x - y
  mean_diff <- ifelse(n > 0, mean(diff_vals), NA_real_)
  sd_diff <- ifelse(n > 1, sd(diff_vals), NA_real_)

  test <- if (n >= 2) {
    tryCatch(t.test(x, y, paired = TRUE, alternative = alternative, conf.level = conf_level), error = function(e) NULL)
  } else {
    NULL
  }
  t_stat <- if (!is.null(test)) unname(test$statistic) else NA_real_
  df_val <- if (!is.null(test)) unname(test$parameter) else NA_real_
  p_val <- if (!is.null(test)) test$p.value else NA_real_
  ci_low <- if (!is.null(test)) test$conf.int[1] else NA_real_
  ci_high <- if (!is.null(test)) test$conf.int[2] else NA_real_
  d_val <- calc_d_paired(mean_diff, sd_diff)

  boot_ci_low <- NA_real_
  boot_ci_high <- NA_real_
  boot_d_low <- NA_real_
  boot_d_high <- NA_real_
  if (bootstrap && n >= 2) {
    if (!is.null(seed) && !is.na(seed)) {
      set.seed(as.numeric(seed))
    }
    boot_ci <- bootstrap_ci(
      diff_vals,
      function(x) mean(x),
      n_boot = bootstrap_samples,
      conf_level = conf_level
    )
    boot_ci_low <- boot_ci[1]
    boot_ci_high <- boot_ci[2]
    boot_d <- bootstrap_ci(
      diff_vals,
      function(x) calc_d_paired(mean(x), sd(x)),
      n_boot = bootstrap_samples,
      conf_level = conf_level
    )
    boot_d_low <- boot_d[1]
    boot_d_high <- boot_d[2]
  }

  shapiro <- safe_shapiro(diff_vals)
  diagnostics <- data.frame(
    case_id = paste0("diag_", case_id),
    test_type = "paired",
    variable = paste0(x_var, " - ", y_var),
    group = "",
    n = n,
    shapiro_w = shapiro$w,
    shapiro_p = shapiro$p,
    var_test_f = NA_real_,
    var_test_p = NA_real_,
    stringsAsFactors = FALSE
  )

  summary_row <- build_summary_row(
    case_id = case_id,
    mode = "paired",
    vars = "",
    x = x_var,
    y = y_var,
    group = "",
    alternative = alternative,
    var_equal = NA,
    conf_level = conf_level,
    bootstrap = bootstrap,
    bootstrap_samples = ifelse(bootstrap, bootstrap_samples, NA_real_),
    test_type = "paired",
    variable = paste0(x_var, " - ", y_var),
    measure_1 = x_var,
    measure_2 = y_var,
    group_1 = "",
    group_2 = "",
    n_1 = n,
    n_2 = NA_real_,
    mean_1 = mean1,
    mean_2 = mean2,
    sd_1 = sd1,
    sd_2 = sd2,
    mean_diff = mean_diff,
    t_stat = t_stat,
    df_val = df_val,
    p_val = p_val,
    d_val = d_val,
    ci_low = ci_low,
    ci_high = ci_high,
    boot_ci_low = boot_ci_low,
    boot_ci_high = boot_ci_high,
    boot_d_low = boot_d_low,
    boot_d_high = boot_d_high,
    mu_val = NA_real_
  )
  list(summary = summary_row, diagnostics = diagnostics)
}

df <- read.csv(data_path, stringsAsFactors = FALSE)

summary_rows <- list()
diagnostic_rows <- list()

case_one_sample <- compute_one_sample(
  df,
  case_id = "one_sample_x1_bootstrap_greater",
  var = "x1",
  mu = 0.1,
  alternative = "greater",
  conf_level = 0.9,
  bootstrap = TRUE,
  bootstrap_samples = 200,
  seed = 123
)
summary_rows[[length(summary_rows) + 1]] <- case_one_sample$summary
diagnostic_rows[[length(diagnostic_rows) + 1]] <- case_one_sample$diagnostics

case_independent_welch <- compute_independent(
  df,
  case_id = "independent_outcome_anova_welch",
  var = "outcome_anova",
  group_var = "group2",
  alternative = "two.sided",
  var_equal = FALSE,
  conf_level = 0.95,
  bootstrap = FALSE,
  bootstrap_samples = 0,
  seed = NA
)
summary_rows[[length(summary_rows) + 1]] <- case_independent_welch$summary
diagnostic_rows <- c(diagnostic_rows, case_independent_welch$diagnostics)

case_independent_equal <- compute_independent(
  df,
  case_id = "independent_x2_equal_var_bootstrap",
  var = "x2",
  group_var = "group2",
  alternative = "two.sided",
  var_equal = TRUE,
  conf_level = 0.95,
  bootstrap = TRUE,
  bootstrap_samples = 150,
  seed = 7
)
summary_rows[[length(summary_rows) + 1]] <- case_independent_equal$summary

case_paired <- compute_paired(
  df,
  case_id = "paired_pre_post",
  x_var = "pre_score",
  y_var = "post_score",
  alternative = "two.sided",
  conf_level = 0.95,
  bootstrap = FALSE,
  bootstrap_samples = 0,
  seed = NA
)
summary_rows[[length(summary_rows) + 1]] <- case_paired$summary
diagnostic_rows[[length(diagnostic_rows) + 1]] <- case_paired$diagnostics

summary_df <- do.call(rbind, summary_rows)
diagnostics_df <- do.call(rbind, diagnostic_rows)

sanitize_inf <- function(df) {
  for (col in names(df)) {
    if (is.numeric(df[[col]])) {
      df[[col]][is.infinite(df[[col]])] <- NA_real_
    }
  }
  df
}

summary_df <- sanitize_inf(summary_df)
diagnostics_df <- sanitize_inf(diagnostics_df)

write.csv(summary_df, out_summary, row.names = FALSE, na = "NA")
write.csv(diagnostics_df, out_diagnostics, row.names = FALSE, na = "NA")

cat("- ", out_summary, "\n", sep = "")
cat("- ", out_diagnostics, "\n", sep = "")
