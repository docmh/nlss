#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Independent reference fixtures, not a copy of the NLSS analysis engine.
args <- commandArgs(trailingOnly = TRUE)
arg <- function(flag, default) {
  i <- which(args == flag)
  if (length(i) > 1L || (length(i) && i == length(args))) stop("Invalid argument: ", flag)
  if (length(i)) args[i + 1L] else default
}
data_path <- arg("--data", "tests/data/golden_dataset.csv")
out_summary <- arg("--out", "tests/values/correlations_golden.csv")
out_diagnostics <- arg("--diagnostics-out", "tests/values/correlations_diagnostics_golden.csv")
out_comparison <- arg("--comparison-out", "tests/values/correlations_comparison_golden.csv")
options(scipen = 999, digits = 15)
data <- read.csv(data_path, stringsAsFactors = FALSE)
complete_data <- function(frame, variables) {
  frame[complete.cases(frame[, variables, drop = FALSE]), variables, drop = FALSE]
}

# Nonpartial inference delegates to cor.test. Partial inference is independently
# obtained from a multiple-regression coefficient test, not cor.test(residuals):
# its residual df includes the effective control rank. Rank all columns first
# for partial Spearman (an approximation, not an exact rank test).
reference <- function(frame, pair, method, controls = character()) {
  if (!length(controls)) {
    fit <- suppressWarnings(stats::cor.test(frame[[pair[1]]], frame[[pair[2]]],
      method = method, alternative = "two.sided", conf.level = .95))
    return(list(r = unname(fit$estimate), p = fit$p.value,
      ci = if (is.null(fit$conf.int)) c(NA_real_, NA_real_) else unname(fit$conf.int),
      control_rank = 0L))
  }
  frame <- frame[, c(pair, controls), drop = FALSE]
  names(frame) <- c("response_x", "response_y", paste0("control_", seq_along(controls)))
  if (method == "spearman") frame[] <- lapply(frame, rank, ties.method = "average")
  fit <- stats::lm(response_y ~ ., frame)
  coefficient <- coef(summary(fit))["response_x", ]
  t <- unname(coefficient["t value"])
  df <- df.residual(fit)
  r <- sign(t) * sqrt(t^2 / (t^2 + df))
  # Effective controls = full model rank minus intercept and focal predictor.
  control_rank <- fit$rank - 2L
  ci <- tanh(atanh(r) + c(-1, 1) * qnorm(.975) / sqrt(nrow(frame) - control_rank - 3L))
  list(r = r, p = unname(coefficient["Pr(>|t|)"]), ci = ci, control_rank = control_rank)
}

matrix_pairs <- function(variables) {
  t(combn(variables, 2L))
}
base_pairs <- matrix_pairs(c("x1", "x2", "x3"))
cross_pairs <- as.matrix(expand.grid(var1 = c("x1", "x2"),
  var2 = c("mediator", "outcome_anova"), stringsAsFactors = FALSE))
cases <- list(
  list(id = "pearson_pairwise_x1_x2", pairs = base_pairs),
  list(id = "spearman_complete_x1_mediator", pairs = cross_pairs,
    method = "spearman", missing = "complete", adjust = "holm"),
  list(id = "partial_spearman_x1_x2", pairs = base_pairs,
    method = "spearman", controls = c("age", "income")),
  list(id = "kendall_pairwise_x1_x2", pairs = base_pairs, method = "kendall"),
  list(id = "r0_pearson_x1_x2", pairs = base_pairs, r0 = .2),
  list(id = "bootstrap_pearson_x1_x2", pairs = base_pairs, bootstrap = 200L, seed = 42L),
  list(id = "grouped_pearson_x1_x2_control", pairs = base_pairs, group = "control")
)

summary_rows <- lapply(cases, function(spec) {
  method <- if (is.null(spec$method)) "pearson" else spec$method
  missing <- if (is.null(spec$missing)) "pairwise" else spec$missing
  controls <- if (is.null(spec$controls)) character() else spec$controls
  adjustment <- if (is.null(spec$adjust)) "none" else spec$adjust
  group <- if (is.null(spec$group)) "" else spec$group
  subset <- if (nzchar(group)) data[!is.na(data$group2) & data$group2 == group, , drop = FALSE] else data
  variables <- unique(c(as.vector(spec$pairs), controls))
  population <- if (missing == "complete") complete_data(subset, variables) else subset
  pair <- unname(spec$pairs[1, ])
  selected <- complete_data(population, unique(c(pair, controls)))
  expected <- reference(selected, pair, method, controls)
  n <- nrow(selected)
  adjusted <- NA_real_
  if (adjustment != "none") {
    family_p <- apply(spec$pairs, 1L, function(pairing) {
      sample <- complete_data(population, unique(c(pairing, controls)))
      reference(sample, unname(pairing), method, controls)$p
    })
    adjusted <- stats::p.adjust(family_p, method = adjustment)[1]
  }
  bootstrap <- c(NA_real_, NA_real_)
  if (!is.null(spec$bootstrap)) {
    stopifnot(!length(controls), method == "pearson")
    set.seed(spec$seed)
    # The selected first pair consumes the first resampling stream; no other
    # pair's resamples are needed to obtain this fixture's two bounds.
    draws <- replicate(spec$bootstrap, {
      indices <- sample.int(n, n, replace = TRUE)
      stats::cor(selected[[pair[1]]][indices], selected[[pair[2]]][indices])
    })
    stopifnot(all(is.finite(draws)))
    bootstrap <- unname(stats::quantile(draws, c(.025, .975), type = 7))
  }
  r0 <- if (is.null(spec$r0)) NA_real_ else spec$r0
  z <- if (is.na(r0)) NA_real_ else
    (atanh(expected$r) - atanh(r0)) * sqrt(n - expected$control_rank - 3)
  data.frame(case_id = spec$id, var1 = pair[1], var2 = pair[2], group = group,
    method = method, alternative = "two.sided", controls = paste(controls, collapse = ","),
    partial = length(controls) > 0L, missing_method = missing, conf_level = .95,
    p_adjust_method = adjustment, r0 = r0, n = n, total_n = nrow(subset),
    missing_n = nrow(subset) - n, missing_pct = (nrow(subset) - n) / nrow(subset) * 100,
    r = expected$r, p_value = expected$p, ci_low = expected$ci[1], ci_high = expected$ci[2],
    boot_ci_low = bootstrap[1], boot_ci_high = bootstrap[2], p_adjusted = adjusted,
    z_r0 = z, p_r0 = if (is.na(z)) NA_real_ else 2 * stats::pnorm(-abs(z)),
    stringsAsFactors = FALSE)
})
write.csv(do.call(rbind, summary_rows), out_summary, row.names = FALSE)

# Variablewise diagnostics, using bias-corrected standardized central moments.
values <- data$x1[!is.na(data$x1)]
n <- length(values)
centered <- values - mean(values)
m2 <- mean(centered^2)
g1 <- mean(centered^3) / m2^(3/2)
g2 <- mean(centered^4) / m2^2 - 3
shapiro <- stats::shapiro.test(values)
diagnostics <- data.frame(case_id = "diagnostic_x1", variable = "x1", group = "",
  n = n, total_n = nrow(data), missing_n = nrow(data) - n,
  missing_pct = (nrow(data) - n) / nrow(data) * 100,
  skewness = sqrt(n * (n - 1)) / (n - 2) * g1,
  kurtosis = (n - 1) / ((n - 2) * (n - 3)) * ((n + 1) * g2 + 6),
  shapiro_w = unname(shapiro$statistic), shapiro_p = shapiro$p.value)
write.csv(diagnostics, out_diagnostics, row.names = FALSE)

# Independent-group Fisher comparison: groups never include phantom NA rows.
groups <- lapply(c("treatment", "control"), function(group) {
  frame <- data[!is.na(data$group2) & data$group2 == group, , drop = FALSE]
  frame <- complete_data(frame, c("x1", "x2"))
  list(r = stats::cor(frame$x1, frame$x2), n = nrow(frame))
})
z <- (atanh(groups[[1]]$r) - atanh(groups[[2]]$r)) /
  sqrt(1 / (groups[[1]]$n - 3) + 1 / (groups[[2]]$n - 3))
comparison <- data.frame(case_id = "compare_groups_x1_x2", var1 = "x1", var2 = "x2",
  group1 = "treatment", group2 = "control", r1 = groups[[1]]$r, r2 = groups[[2]]$r,
  n1 = groups[[1]]$n, n2 = groups[[2]]$n, z = z, p_value = 2 * stats::pnorm(-abs(z)))
write.csv(comparison, out_comparison, row.names = FALSE)
cat("Wrote independent correlation references to", out_summary, out_diagnostics, out_comparison, "\n")
