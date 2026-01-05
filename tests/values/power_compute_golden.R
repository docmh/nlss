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
out_path <- trim_arg(get_arg("--out", file.path("tests", "values", "power_golden.csv")))

if (is.null(data_path) || !file.exists(data_path)) {
  stop("Missing --data path.")
}
if (is.null(out_path)) {
  stop("Missing --out path.")
}

if (!requireNamespace("pwr", quietly = TRUE)) {
  stop("Package 'pwr' is required to compute power goldens.")
}

has_sem <- requireNamespace("semPower", quietly = TRUE)

options(scipen = 999, digits = 15)

alpha_default <- 0.05
power_default <- 0.8

clean_complete <- function(df, cols) {
  data <- df[, cols, drop = FALSE]
  data <- data[complete.cases(data), , drop = FALSE]
  data
}

auto_abs <- function(x) {
  if (is.na(x)) return(x)
  abs(x)
}

estimate_ttest_two_sample <- function(df, var, group_var) {
  if (!var %in% names(df)) stop("Variable not found.")
  if (!group_var %in% names(df)) stop("Grouping variable not found.")
  vals <- df[[var]]
  groups <- df[[group_var]]
  idx <- is.finite(vals) & !is.na(groups)
  vals <- vals[idx]
  groups <- as.factor(groups[idx])
  levels <- levels(groups)
  if (length(levels) != 2) stop("Grouping variable must have exactly two levels.")
  g1 <- levels[1]
  g2 <- levels[2]
  v1 <- vals[groups == g1]
  v2 <- vals[groups == g2]
  if (length(v1) < 2 || length(v2) < 2) stop("Not enough data per group.")
  m1 <- mean(v1)
  m2 <- mean(v2)
  sd1 <- sd(v1)
  sd2 <- sd(v2)
  n1 <- length(v1)
  n2 <- length(v2)
  pooled <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  if (is.na(pooled) || pooled == 0) stop("Pooled SD is zero.")
  d <- (m1 - m2) / pooled
  list(d = d, n1 = n1, n2 = n2)
}

estimate_correlation <- function(df, x_var, y_var) {
  if (!x_var %in% names(df) || !y_var %in% names(df)) stop("Correlation variables not found.")
  x <- df[[x_var]]
  y <- df[[y_var]]
  idx <- is.finite(x) & is.finite(y)
  if (sum(idx) < 3) stop("Not enough data for correlation.")
  r <- suppressWarnings(cor(x[idx], y[idx]))
  list(r = r, n = sum(idx))
}

estimate_regression <- function(df, dv, ivs) {
  cols <- c(dv, ivs)
  data <- clean_complete(df, cols)
  if (nrow(data) < length(ivs) + 2) stop("Not enough data for regression.")
  formula <- as.formula(paste(dv, "~", paste(ivs, collapse = " + ")))
  model <- lm(formula, data = data)
  r2 <- summary(model)$r.squared
  if (is.na(r2) || r2 < 0) stop("Could not compute r2.")
  f2 <- if (r2 >= 1) NA_real_ else r2 / (1 - r2)
  list(r2 = r2, f2 = f2, n = nrow(data))
}

estimate_anova <- function(df, dv, group_var) {
  if (!dv %in% names(df)) stop("DV not found.")
  if (!group_var %in% names(df)) stop("Group variable not found.")
  data <- clean_complete(df, c(dv, group_var))
  data[[group_var]] <- as.factor(data[[group_var]])
  if (nrow(data) < 3) stop("Not enough data for ANOVA.")
  if (length(levels(data[[group_var]])) < 2) stop("Group variable needs >= 2 levels.")
  model <- aov(stats::as.formula(paste(dv, "~", group_var)), data = data)
  table <- summary(model)[[1]]
  if (nrow(table) < 2) stop("ANOVA table incomplete.")
  ss_between <- table[1, "Sum Sq"]
  ss_within <- table[2, "Sum Sq"]
  eta2 <- ss_between / (ss_between + ss_within)
  if (is.na(eta2) || eta2 <= 0 || eta2 >= 1) stop("Invalid eta2.")
  f <- sqrt(eta2 / (1 - eta2))
  list(eta2 = eta2, f = f, n = nrow(data), groups = length(levels(data[[group_var]])))
}

solve_rmsea_sensitivity <- function(df_sem, rmsea0, alpha, power_target, n_total) {
  if (!has_sem) return(NA_real_)
  power_at <- function(rmsea_candidate) {
    res <- semPower::semPower.postHoc(
      effect = rmsea_candidate,
      effect.measure = "RMSEA",
      df = df_sem,
      alpha = alpha,
      N = n_total,
      n = n_total
    )
    val <- res$power
    if (is.null(val) && !is.null(res$Power)) val <- res$Power
    as.numeric(val)
  }
  lower <- max(rmsea0 + 1e-4, 1e-4)
  upper <- 0.3
  p_low <- power_at(lower)
  p_high <- power_at(upper)
  if (!is.finite(p_low) || !is.finite(p_high)) stop("SEM sensitivity could not bracket.")
  if ((p_low - power_target) * (p_high - power_target) > 0) {
    stop("SEM sensitivity could not bracket.")
  }
  uniroot(function(x) power_at(x) - power_target, interval = c(lower, upper))$root
}

extract_sem_value <- function(obj, keys) {
  if (is.null(obj)) return(NA_real_)
  if (!is.list(obj)) return(NA_real_)
  for (key in keys) {
    if (!is.null(obj[[key]])) {
      val <- obj[[key]]
      if (is.numeric(val)) return(as.numeric(val[1]))
    }
  }
  NA_real_
}

add_row <- function(rows, values) {
  rows[[length(rows) + 1]] <- values
  rows
}

init_row <- function(case_id) {
  list(
    case_id = case_id,
    analysis = "",
    mode = "",
    effect_metric = "",
    t_type = "",
    alternative = "",
    effect_source = "user",
    alpha = NA_real_,
    power = NA_real_,
    effect_size = NA_real_,
    n_total = NA_real_,
    n_per_group = NA_real_,
    n1 = NA_real_,
    n2 = NA_real_,
    groups = NA_real_,
    ratio = NA_real_,
    u = NA_real_,
    df = NA_real_,
    r2 = NA_real_,
    rmsea0 = NA_real_,
    rmsea1 = NA_real_
  )
}

rows <- list()

df <- read.csv(data_path, stringsAsFactors = FALSE)

# t-test apriori (two-sample)
{
  res <- pwr::pwr.t.test(d = 0.5, sig.level = alpha_default, power = power_default, type = "two.sample", alternative = "two.sided")
  n1 <- ceiling(res$n)
  n2 <- n1
  row <- init_row("ttest_apriori_two_sample")
  row$analysis <- "ttest"
  row$mode <- "apriori"
  row$effect_metric <- "d"
  row$t_type <- "two-sample"
  row$alternative <- "two.sided"
  row$alpha <- alpha_default
  row$power <- power_default
  row$effect_size <- 0.5
  row$n_total <- n1 + n2
  row$n1 <- n1
  row$n2 <- n2
  row$ratio <- 1
  rows <- add_row(rows, row)
}

# t-test apriori with estimated effect
{
  est <- estimate_ttest_two_sample(df, "outcome_anova", "group2")
  d_abs <- auto_abs(est$d)
  res <- pwr::pwr.t.test(d = d_abs, sig.level = alpha_default, power = power_default, type = "two.sample", alternative = "two.sided")
  n1 <- ceiling(res$n)
  n2 <- n1
  row <- init_row("ttest_apriori_estimated")
  row$analysis <- "ttest"
  row$mode <- "apriori"
  row$effect_metric <- "d"
  row$t_type <- "two-sample"
  row$alternative <- "two.sided"
  row$effect_source <- "estimated"
  row$alpha <- alpha_default
  row$power <- power_default
  row$effect_size <- d_abs
  row$n_total <- n1 + n2
  row$n1 <- n1
  row$n2 <- n2
  row$ratio <- 1
  rows <- add_row(rows, row)
}

# t-test posthoc (two-sample, ratio)
{
  n1 <- 30
  ratio <- 1.5
  n2 <- n1 * ratio
  res <- pwr::pwr.t2n.test(n1 = n1, n2 = n2, d = 0.3, sig.level = alpha_default, alternative = "two.sided")
  row <- init_row("ttest_posthoc_ratio")
  row$analysis <- "ttest"
  row$mode <- "posthoc"
  row$effect_metric <- "d"
  row$t_type <- "two-sample"
  row$alternative <- "two.sided"
  row$alpha <- alpha_default
  row$power <- res$power
  row$effect_size <- 0.3
  row$n_total <- n1 + n2
  row$n1 <- n1
  row$n2 <- n2
  row$ratio <- ratio
  rows <- add_row(rows, row)
}

# t-test posthoc (one-sample, less)
{
  res <- pwr::pwr.t.test(n = 60, d = 0.3, sig.level = alpha_default, type = "one.sample", alternative = "less")
  row <- init_row("ttest_posthoc_one_sample_less")
  row$analysis <- "ttest"
  row$mode <- "posthoc"
  row$effect_metric <- "d"
  row$t_type <- "one-sample"
  row$alternative <- "less"
  row$alpha <- alpha_default
  row$power <- res$power
  row$effect_size <- 0.3
  row$n_total <- 60
  rows <- add_row(rows, row)
}

# t-test sensitivity (two-sample, ratio)
{
  n_total <- 90
  ratio <- 2
  n1 <- ceiling(n_total / (1 + ratio))
  n2 <- n_total - n1
  res <- pwr::pwr.t2n.test(n1 = n1, n2 = n2, sig.level = alpha_default, power = power_default, alternative = "two.sided")
  row <- init_row("ttest_sensitivity_two_sample_ratio")
  row$analysis <- "ttest"
  row$mode <- "sensitivity"
  row$effect_metric <- "d"
  row$t_type <- "two-sample"
  row$alternative <- "two.sided"
  row$alpha <- alpha_default
  row$power <- power_default
  row$effect_size <- res$d
  row$n_total <- n_total
  row$n1 <- n1
  row$n2 <- n2
  row$ratio <- ratio
  rows <- add_row(rows, row)
}

# ANOVA posthoc
{
  res <- pwr::pwr.anova.test(k = 3, n = 25, f = 0.25, sig.level = alpha_default)
  row <- init_row("anova_posthoc")
  row$analysis <- "anova"
  row$mode <- "posthoc"
  row$effect_metric <- "f"
  row$alpha <- alpha_default
  row$power <- res$power
  row$effect_size <- 0.25
  row$n_per_group <- 25
  row$n_total <- 75
  row$groups <- 3
  rows <- add_row(rows, row)
}

# ANOVA apriori with eta2 conversion
{
  eta2 <- 0.06
  f <- sqrt(eta2 / (1 - eta2))
  res <- pwr::pwr.anova.test(k = 3, f = f, sig.level = alpha_default, power = power_default)
  n_per_group <- ceiling(res$n)
  row <- init_row("anova_apriori_eta2")
  row$analysis <- "anova"
  row$mode <- "apriori"
  row$effect_metric <- "eta2"
  row$alpha <- alpha_default
  row$power <- power_default
  row$effect_size <- eta2
  row$n_per_group <- n_per_group
  row$n_total <- n_per_group * 3
  row$groups <- 3
  rows <- add_row(rows, row)
}

# ANOVA sensitivity
{
  res <- pwr::pwr.anova.test(k = 4, n = 20, sig.level = alpha_default, power = power_default)
  row <- init_row("anova_sensitivity")
  row$analysis <- "anova"
  row$mode <- "sensitivity"
  row$effect_metric <- "f"
  row$alpha <- alpha_default
  row$power <- power_default
  row$effect_size <- res$f
  row$n_per_group <- 20
  row$n_total <- 80
  row$groups <- 4
  rows <- add_row(rows, row)
}

# ANOVA estimated effect (groups default to 2 when --groups is not supplied)
{
  est <- estimate_anova(df, "outcome_anova", "group3")
  groups_default <- 2
  res <- pwr::pwr.anova.test(k = groups_default, f = est$f, sig.level = alpha_default, power = power_default)
  n_per_group <- ceiling(res$n)
  row <- init_row("anova_estimated")
  row$analysis <- "anova"
  row$mode <- "apriori"
  row$effect_metric <- "eta2"
  row$effect_source <- "estimated"
  row$alpha <- alpha_default
  row$power <- power_default
  row$effect_size <- est$eta2
  row$n_per_group <- n_per_group
  row$n_total <- n_per_group * groups_default
  row$groups <- groups_default
  rows <- add_row(rows, row)
}

# Correlation posthoc (greater)
{
  res <- pwr::pwr.r.test(n = 150, r = 0.25, sig.level = alpha_default, alternative = "greater")
  row <- init_row("corr_posthoc_greater")
  row$analysis <- "correlation"
  row$mode <- "posthoc"
  row$effect_metric <- "r"
  row$alternative <- "greater"
  row$alpha <- alpha_default
  row$power <- res$power
  row$effect_size <- 0.25
  row$n_total <- 150
  rows <- add_row(rows, row)
}

# Correlation sensitivity
{
  res <- pwr::pwr.r.test(n = 100, sig.level = alpha_default, power = power_default, alternative = "two.sided")
  row <- init_row("corr_sensitivity")
  row$analysis <- "correlation"
  row$mode <- "sensitivity"
  row$effect_metric <- "r"
  row$alternative <- "two.sided"
  row$alpha <- alpha_default
  row$power <- power_default
  row$effect_size <- res$r
  row$n_total <- 100
  rows <- add_row(rows, row)
}

# Correlation estimated effect
{
  est <- estimate_correlation(df, "x1", "x2")
  r_abs <- auto_abs(est$r)
  res <- pwr::pwr.r.test(r = r_abs, sig.level = alpha_default, power = power_default, alternative = "two.sided")
  row <- init_row("corr_estimated")
  row$analysis <- "correlation"
  row$mode <- "apriori"
  row$effect_metric <- "r"
  row$alternative <- "two.sided"
  row$effect_source <- "estimated"
  row$alpha <- alpha_default
  row$power <- power_default
  row$effect_size <- r_abs
  row$n_total <- ceiling(res$n)
  rows <- add_row(rows, row)
}

# Regression apriori (f2)
{
  res <- pwr::pwr.f2.test(u = 3, f2 = 0.15, sig.level = alpha_default, power = power_default)
  row <- init_row("reg_apriori_f2")
  row$analysis <- "regression"
  row$mode <- "apriori"
  row$effect_metric <- "f2"
  row$alpha <- alpha_default
  row$power <- power_default
  row$effect_size <- 0.15
  row$n_total <- ceiling(res$v + 3 + 1)
  row$u <- 3
  rows <- add_row(rows, row)
}

# Regression posthoc (r2)
{
  r2 <- 0.13
  f2 <- r2 / (1 - r2)
  v <- 120 - 3 - 1
  res <- pwr::pwr.f2.test(u = 3, v = v, f2 = f2, sig.level = alpha_default)
  row <- init_row("reg_posthoc_r2")
  row$analysis <- "regression"
  row$mode <- "posthoc"
  row$effect_metric <- "r2"
  row$alpha <- alpha_default
  row$power <- res$power
  row$effect_size <- r2
  row$n_total <- 120
  row$u <- 3
  row$r2 <- r2
  rows <- add_row(rows, row)
}

# Regression sensitivity
{
  v <- 100 - 3 - 1
  res <- pwr::pwr.f2.test(u = 3, v = v, sig.level = alpha_default, power = power_default)
  row <- init_row("reg_sensitivity")
  row$analysis <- "regression"
  row$mode <- "sensitivity"
  row$effect_metric <- "f2"
  row$alpha <- alpha_default
  row$power <- power_default
  row$effect_size <- res$f2
  row$n_total <- 100
  row$u <- 3
  rows <- add_row(rows, row)
}

# Regression estimated effect
{
  est <- estimate_regression(df, "outcome_reg", c("x1", "x2", "x3"))
  res <- pwr::pwr.f2.test(u = 3, f2 = est$f2, sig.level = alpha_default, power = power_default)
  row <- init_row("reg_estimated")
  row$analysis <- "regression"
  row$mode <- "apriori"
  row$effect_metric <- "r2"
  row$effect_source <- "estimated"
  row$alpha <- alpha_default
  row$power <- power_default
  row$effect_size <- est$r2
  row$n_total <- ceiling(res$v + 3 + 1)
  row$u <- 3
  row$r2 <- est$r2
  rows <- add_row(rows, row)
}

# SEM cases (if semPower installed)
if (has_sem) {
  df_sem <- 120
  rmsea0 <- 0.05
  rmsea1 <- 0.08

  res_ap <- semPower::semPower.aPriori(
    effect = rmsea1,
    effect.measure = "RMSEA",
    df = df_sem,
    alpha = alpha_default,
    power = power_default
  )
  n_ap <- extract_sem_value(res_ap, c("N", "n", "sample.size", "sample_size", "requiredN"))
  n_ap <- ceiling(n_ap)
  row <- init_row("sem_apriori")
  row$analysis <- "sem"
  row$mode <- "apriori"
  row$effect_metric <- "rmsea"
  row$alpha <- alpha_default
  row$power <- power_default
  row$effect_size <- rmsea1
  row$n_total <- n_ap
  row$df <- df_sem
  row$rmsea0 <- rmsea0
  row$rmsea1 <- rmsea1
  rows <- add_row(rows, row)

  res_ph <- semPower::semPower.postHoc(
    effect = rmsea1,
    effect.measure = "RMSEA",
    df = df_sem,
    alpha = alpha_default,
    N = 200,
    n = 200
  )
  p_ph <- extract_sem_value(res_ph, c("power", "Power"))
  row <- init_row("sem_posthoc")
  row$analysis <- "sem"
  row$mode <- "posthoc"
  row$effect_metric <- "rmsea"
  row$alpha <- alpha_default
  row$power <- p_ph
  row$effect_size <- rmsea1
  row$n_total <- 200
  row$df <- df_sem
  row$rmsea0 <- rmsea0
  row$rmsea1 <- rmsea1
  rows <- add_row(rows, row)

  rmsea_sens <- solve_rmsea_sensitivity(df_sem, rmsea0, alpha_default, power_default, 120)
  row <- init_row("sem_sensitivity")
  row$analysis <- "sem"
  row$mode <- "sensitivity"
  row$effect_metric <- "rmsea"
  row$alpha <- alpha_default
  row$power <- power_default
  row$effect_size <- rmsea_sens
  row$n_total <- 120
  row$df <- df_sem
  row$rmsea0 <- rmsea0
  row$rmsea1 <- rmsea_sens
  rows <- add_row(rows, row)
}

out_df <- do.call(rbind, lapply(rows, function(row) as.data.frame(row, stringsAsFactors = FALSE)))
write.csv(out_df, out_path, row.names = FALSE, na = "")

cat("Wrote:", out_path, "\n")
