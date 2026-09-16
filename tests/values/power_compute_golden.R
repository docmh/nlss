#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Independent package/base-R references: no NLSS source or numerical helpers.
args <- commandArgs(TRUE)
arg <- function(flag, default) { i <- which(args == flag); if (length(i)) args[max(i) + 1L] else default }
data_path <- arg("--data", "tests/data/golden_dataset.csv")
out_path <- arg("--out", "tests/values/power_golden.csv")
if (!requireNamespace("pwr", quietly = TRUE)) stop("Package 'pwr' is required.")
d <- read.csv(data_path, stringsAsFactors = FALSE)
options(scipen = 999, digits = 15)
rows <- list()
add <- function(case_id, analysis, mode, metric, effect, ..., source = if (mode == "sensitivity") "solved" else "user") {
  row <- list(case_id = case_id, analysis = analysis, mode = mode, effect_metric = metric,
    t_type = "", alternative = "", effect_source = source, alpha = .05, power = .8,
    effect_size = effect, n_total = NA_real_, n_per_group = NA_real_, n1 = NA_real_, n2 = NA_real_,
    groups = NA_real_, ratio = NA_real_, u = NA_real_, df = NA_real_, r2 = NA_real_, rmsea0 = NA_real_, rmsea1 = NA_real_)
  changes <- list(...); row[names(changes)] <- changes
  rows[[length(rows) + 1L]] <<- row
}
# The direct two-sample t statistic identifies d independently of NLSS's estimator.
t_data <- d[is.finite(d$outcome_anova) & !is.na(d$group2), ]
t_fit <- t.test(outcome_anova ~ group2, data = t_data, var.equal = TRUE)
t_n <- as.numeric(table(t_data$group2))
t_d <- unname(t_fit$statistic) * sqrt(sum(1 / t_n))
for (estimated in c(FALSE, TRUE)) {
  effect <- if (estimated) t_d else .5
  fit <- pwr::pwr.t.test(d = abs(effect), sig.level = .05, power = .8, type = "two.sample")
  n <- ceiling(fit$n)
  add(if (estimated) "ttest_apriori_estimated" else "ttest_apriori_two_sample", "ttest", "apriori", "d", effect,
    t_type = "two-sample", alternative = "two.sided", n_total = 2 * n, n1 = n, n2 = n, ratio = 1,
    source = if (estimated) "estimated" else "user")
}
fit <- pwr::pwr.t2n.test(n1 = 30, n2 = 45, d = .3, sig.level = .05)
add("ttest_posthoc_ratio", "ttest", "posthoc", "d", .3, t_type = "two-sample", alternative = "two.sided", power = fit$power, n_total = 75, n1 = 30, n2 = 45, ratio = 1.5)
fit <- pwr::pwr.t.test(n = 60, d = .3, sig.level = .05, type = "one.sample", alternative = "less")
add("ttest_posthoc_one_sample_less", "ttest", "posthoc", "d", .3, t_type = "one-sample", alternative = "less", power = fit$power, n_total = 60)
fit <- pwr::pwr.t2n.test(n1 = 30, n2 = 60, sig.level = .05, power = .8)
add("ttest_sensitivity_two_sample_ratio", "ttest", "sensitivity", "d", fit$d, t_type = "two-sample", alternative = "two.sided", n_total = 90, n1 = 30, n2 = 60, ratio = 2)

fit <- pwr::pwr.anova.test(k = 3, n = 25, f = .25, sig.level = .05)
add("anova_posthoc", "anova", "posthoc", "f", .25, power = fit$power, n_per_group = 25, n_total = 75, groups = 3)
# The model's reduction in residual sums of squares supplies eta-squared.
a_data <- d[complete.cases(d[c("outcome_anova", "group3")]), ]
a_eta <- summary(lm(outcome_anova ~ factor(group3), data = a_data))$r.squared
a_groups <- nlevels(factor(a_data$group3))
for (estimated in c(FALSE, TRUE)) {
  eta <- if (estimated) a_eta else .06
  k <- if (estimated) a_groups else 3
  fit <- pwr::pwr.anova.test(k = k, f = sqrt(eta / (1 - eta)), sig.level = .05, power = .8)
  n <- ceiling(fit$n)
  add(if (estimated) "anova_estimated" else "anova_apriori_eta2", "anova", "apriori", "eta2", eta,
    n_per_group = n, n_total = n * k, groups = k, source = if (estimated) "estimated" else "user")
}
fit <- pwr::pwr.anova.test(k = 4, n = 20, sig.level = .05, power = .8)
add("anova_sensitivity", "anova", "sensitivity", "f", fit$f, n_per_group = 20, n_total = 80, groups = 4)

fit <- pwr::pwr.r.test(n = 150, r = .25, sig.level = .05, alternative = "greater")
add("corr_posthoc_greater", "correlation", "posthoc", "r", .25, alternative = "greater", power = fit$power, n_total = 150)
fit <- pwr::pwr.r.test(n = 100, sig.level = .05, power = .8)
add("corr_sensitivity", "correlation", "sensitivity", "r", fit$r, alternative = "two.sided", n_total = 100)
c_data <- d[is.finite(d$x1) & is.finite(d$x2), ]
r <- cor(c_data$x1, c_data$x2)
fit <- pwr::pwr.r.test(r = abs(r), sig.level = .05, power = .8)
add("corr_estimated", "correlation", "apriori", "r", r, alternative = "two.sided", n_total = ceiling(fit$n), source = "estimated")

fit <- pwr::pwr.f2.test(u = 3, f2 = .15, sig.level = .05, power = .8)
add("reg_apriori_f2", "regression", "apriori", "f2", .15, n_total = ceiling(fit$v + 4), u = 3)
fit <- pwr::pwr.f2.test(u = 3, v = 116, f2 = .13 / .87, sig.level = .05)
add("reg_posthoc_r2", "regression", "posthoc", "r2", .13, power = fit$power, n_total = 120, u = 3, r2 = .13)
fit <- pwr::pwr.f2.test(u = 3, v = 96, sig.level = .05, power = .8)
add("reg_sensitivity", "regression", "sensitivity", "f2", fit$f2, n_total = 100, u = 3)
r_fit <- lm(outcome_reg ~ x1 + x2 + x3, data = d)
r2 <- summary(r_fit)$r.squared; u <- r_fit$rank - 1L
fit <- pwr::pwr.f2.test(u = u, f2 = r2 / (1 - r2), sig.level = .05, power = .8)
add("reg_estimated", "regression", "apriori", "r2", r2, n_total = ceiling(fit$v + u + 1), u = u, r2 = r2, source = "estimated")

# Include both noncentralities: a single-effect semPower call tests exact fit,
# not the close-fit null RMSEA0=.05 that these CLI cases request.
rmsea_power <- function(n, e1, e0 = .05, df = 120, alpha = .05) {
  critical <- qchisq(1 - alpha, df = df, ncp = (n - 1) * df * e0^2)
  pchisq(critical, df = df, ncp = (n - 1) * df * e1^2, lower.tail = FALSE)
}
n <- ceiling(uniroot(function(n) rmsea_power(n, .08) - .8, c(2, 1e4), tol = 1e-10)$root)
add("sem_apriori", "sem", "apriori", "rmsea", .08, n_total = n, df = 120, rmsea0 = .05, rmsea1 = .08)
add("sem_posthoc", "sem", "posthoc", "rmsea", .08, power = rmsea_power(200, .08), n_total = 200, df = 120, rmsea0 = .05, rmsea1 = .08)
e <- uniroot(function(e) rmsea_power(120, e) - .8, c(.05, .5), tol = 1e-10)$root
add("sem_sensitivity", "sem", "sensitivity", "rmsea", e, n_total = 120, df = 120, rmsea0 = .05, rmsea1 = e)
write.csv(do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE)), out_path, row.names = FALSE, na = "")
cat("Wrote:", out_path, "\n")
