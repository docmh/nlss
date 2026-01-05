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

options(scipen = 999, digits = 15)

data_path <- trim_arg(get_arg("--data", file.path("tests", "data", "golden_dataset.csv")))
out_path <- trim_arg(get_arg("--out", file.path("tests", "values", "descriptive_stats_golden.csv")))

if (is.null(data_path) || !file.exists(data_path)) {
  stop("Missing --data path.")
}
if (is.null(out_path)) {
  stop("Missing --out path.")
}

df <- read.csv(data_path, stringsAsFactors = FALSE)

calc_skewness <- function(x, mean_x, sd_x) {
  n <- length(x)
  if (n < 3 || is.na(sd_x) || sd_x == 0) return(NA_real_)
  sum(((x - mean_x) / sd_x)^3) * (n / ((n - 1) * (n - 2)))
}

calc_kurtosis <- function(x, mean_x, sd_x) {
  n <- length(x)
  if (n < 4 || is.na(sd_x) || sd_x == 0) return(NA_real_)
  term1 <- (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3))
  term2 <- (3 * (n - 1)^2) / ((n - 2) * (n - 3))
  term1 * sum(((x - mean_x) / sd_x)^4) - term2
}

calc_mode <- function(x, min_count = 2) {
  if (length(x) == 0) return(NA_real_)
  tab <- table(x, useNA = "no")
  if (length(tab) == 0) return(NA_real_)
  max_count <- max(tab)
  if (max_count < min_count) return(NA_real_)
  modes <- suppressWarnings(as.numeric(names(tab)[tab == max_count]))
  if (length(modes) != 1) return(NA_real_)
  modes[1]
}

calc_stats <- function(vec, total_n, trim, iqr_multiplier, outlier_z) {
  missing_n <- sum(is.na(vec))
  x <- vec[!is.na(vec)]
  n <- length(x)

  if (n == 0) {
    return(list(
      total_n = total_n,
      n = 0,
      missing_n = missing_n,
      missing_pct = ifelse(total_n > 0, missing_n / total_n * 100, NA_real_),
      mean = NA_real_,
      sd = NA_real_,
      median = NA_real_,
      min = NA_real_,
      max = NA_real_,
      variance = NA_real_,
      range = NA_real_,
      q1 = NA_real_,
      q3 = NA_real_,
      iqr = NA_real_,
      mad = NA_real_,
      cv = NA_real_,
      trimmed_mean = NA_real_,
      p5 = NA_real_,
      p10 = NA_real_,
      p90 = NA_real_,
      p95 = NA_real_,
      outliers_tukey = NA_real_,
      outliers_z = NA_real_,
      mode = NA_real_,
      n_unique = NA_real_,
      se = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      skewness = NA_real_,
      kurtosis = NA_real_
    ))
  }

  mean_x <- mean(x)
  sd_x <- if (n > 1) sd(x) else NA_real_
  variance_x <- if (n > 1) var(x) else NA_real_
  median_x <- median(x)
  min_x <- min(x)
  max_x <- max(x)
  range_x <- max_x - min_x
  q <- quantile(x, probs = c(0.05, 0.1, 0.25, 0.75, 0.9, 0.95), names = FALSE, type = 7)
  p5_x <- q[1]
  p10_x <- q[2]
  q1_x <- q[3]
  q3_x <- q[4]
  p90_x <- q[5]
  p95_x <- q[6]
  iqr_x <- q3_x - q1_x
  mad_x <- mad(x, constant = 1.4826, na.rm = TRUE)
  trim_x <- mean(x, trim = trim)
  cv_x <- if (!is.na(sd_x) && !is.na(mean_x) && mean_x != 0) sd_x / abs(mean_x) else NA_real_
  outlier_tukey_count <- if (!is.na(iqr_x)) {
    lower <- q1_x - iqr_multiplier * iqr_x
    upper <- q3_x + iqr_multiplier * iqr_x
    sum(x < lower | x > upper)
  } else {
    NA_real_
  }
  outlier_z_count <- if (!is.na(sd_x) && sd_x > 0) {
    sum(abs((x - mean_x) / sd_x) > outlier_z)
  } else {
    NA_real_
  }
  mode_x <- calc_mode(x)
  n_unique <- length(unique(x))
  se_x <- if (!is.na(sd_x)) sd_x / sqrt(n) else NA_real_
  if (!is.na(se_x) && n > 1) {
    tcrit <- qt(0.975, df = n - 1)
    ci_low <- mean_x - tcrit * se_x
    ci_high <- mean_x + tcrit * se_x
  } else {
    ci_low <- NA_real_
    ci_high <- NA_real_
  }

  skew_x <- calc_skewness(x, mean_x, sd_x)
  kurt_x <- calc_kurtosis(x, mean_x, sd_x)

  list(
    total_n = total_n,
    n = n,
    missing_n = missing_n,
    missing_pct = ifelse(total_n > 0, missing_n / total_n * 100, NA_real_),
    mean = mean_x,
    sd = sd_x,
    median = median_x,
    min = min_x,
    max = max_x,
    variance = variance_x,
    range = range_x,
    q1 = q1_x,
    q3 = q3_x,
    iqr = iqr_x,
    mad = mad_x,
    cv = cv_x,
    trimmed_mean = trim_x,
    p5 = p5_x,
    p10 = p10_x,
    p90 = p90_x,
    p95 = p95_x,
    outliers_tukey = outlier_tukey_count,
    outliers_z = outlier_z_count,
    mode = mode_x,
    n_unique = n_unique,
    se = se_x,
    ci_low = ci_low,
    ci_high = ci_high,
    skewness = skew_x,
    kurtosis = kurt_x
  )
}

cases <- list(
  list(case_id = "default_outcome_anova", var = "outcome_anova", group_var = "", group_value = "", trim = 0.1, iqr_multiplier = 1.5, outlier_z = 3),
  list(case_id = "default_skewed_var", var = "skewed_var", group_var = "", group_value = "", trim = 0.1, iqr_multiplier = 1.5, outlier_z = 3),
  list(case_id = "default_outlier_var", var = "outlier_var", group_var = "", group_value = "", trim = 0.1, iqr_multiplier = 1.5, outlier_z = 3),
  list(case_id = "default_ordinal_var", var = "ordinal_var", group_var = "", group_value = "", trim = 0.1, iqr_multiplier = 1.5, outlier_z = 3),
  list(case_id = "default_high_missing_var", var = "high_missing_var", group_var = "", group_value = "", trim = 0.1, iqr_multiplier = 1.5, outlier_z = 3),
  list(case_id = "default_all_missing_var", var = "all_missing_var", group_var = "", group_value = "", trim = 0.1, iqr_multiplier = 1.5, outlier_z = 3),
  list(case_id = "default_zero_var", var = "zero_var", group_var = "", group_value = "", trim = 0.1, iqr_multiplier = 1.5, outlier_z = 3),
  list(case_id = "grouped_outlier_var_control", var = "outlier_var", group_var = "group2", group_value = "control", trim = 0.2, iqr_multiplier = 2, outlier_z = 2.5)
)

rows <- list()

for (case in cases) {
  var <- case$var
  if (!var %in% names(df)) {
    stop(paste("Missing variable in dataset:", var))
  }

  group_var <- case$group_var
  group_value <- case$group_value
  df_sub <- df
  group_label <- ""
  if (!is.null(group_var) && nzchar(group_var)) {
    if (!group_var %in% names(df)) {
      stop(paste("Missing group variable in dataset:", group_var))
    }
    group_vec <- df[[group_var]]
    if (is.na(group_value) || group_value == "NA") {
      idx <- is.na(group_vec)
      group_label <- "NA"
    } else {
      idx <- group_vec == group_value
      # Preserve NA indices to mirror descriptive_stats group subsetting behavior.
      group_label <- as.character(group_value)
    }
    df_sub <- df[idx, , drop = FALSE]
  }

  vec <- df_sub[[var]]
  total_n <- length(vec)
  stats <- calc_stats(vec, total_n, case$trim, case$iqr_multiplier, case$outlier_z)

  row <- data.frame(
    case_id = case$case_id,
    variable = var,
    group = group_label,
    group_var = ifelse(is.null(group_var) || !nzchar(group_var), "", group_var),
    trim = case$trim,
    iqr_multiplier = case$iqr_multiplier,
    outlier_z = case$outlier_z,
    total_n = stats$total_n,
    n = stats$n,
    missing_n = stats$missing_n,
    missing_pct = stats$missing_pct,
    mean = stats$mean,
    sd = stats$sd,
    median = stats$median,
    min = stats$min,
    max = stats$max,
    variance = stats$variance,
    range = stats$range,
    q1 = stats$q1,
    q3 = stats$q3,
    iqr = stats$iqr,
    mad = stats$mad,
    cv = stats$cv,
    trimmed_mean = stats$trimmed_mean,
    p5 = stats$p5,
    p10 = stats$p10,
    p90 = stats$p90,
    p95 = stats$p95,
    outliers_tukey = stats$outliers_tukey,
    outliers_z = stats$outliers_z,
    mode = stats$mode,
    n_unique = stats$n_unique,
    se = stats$se,
    ci_low = stats$ci_low,
    ci_high = stats$ci_high,
    skewness = stats$skewness,
    kurtosis = stats$kurtosis,
    stringsAsFactors = FALSE
  )
  rows[[length(rows) + 1]] <- row
}

golden <- do.call(rbind, rows)
write.csv(golden, out_path, row.names = FALSE)
cat("Wrote golden values to", out_path, "\n")
