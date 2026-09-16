#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Independent public lavaan calls: no NLSS modules, builders or statistic helpers.
args <- commandArgs(TRUE)
arg <- function(flag, default) {
  i <- which(args == flag)
  if (length(i)) args[[tail(i, 1) + 1L]] else default
}
if (!requireNamespace("lavaan", quietly = TRUE)) stop("lavaan is required.")
options(scipen = 999, digits = 15)
df <- read.csv(arg("--data", "tests/data/golden_dataset.csv"), stringsAsFactors = FALSE)
indices <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr")
path <- lavaan::sem("outcome_reg ~ x1 + x2", df, estimator = "ML", missing = "listwise", se = "standard")
cfa_syntax <- "F1 =~ f1_1 + f1_2 + f1_3_rev + f1_4
F2 =~ f2_1 + f2_2 + f2_3 + f2_4_rev"
cfa <- lavaan::cfa(cfa_syntax, df, estimator = "ML", missing = "listwise", se = "standard")
grouped <- lavaan::cfa(cfa_syntax, df, group = "group2", estimator = "ML", missing = "listwise", se = "standard")
mediation <- lavaan::sem("mediator ~ a1*x1
outcome_reg ~ c_prime*x1 + b1*mediator
indirect_mediator := a1*b1
total_indirect := indirect_mediator
total := c_prime + total_indirect", df, estimator = "ML", missing = "listwise", se = "standard")

fit_row <- function(fit, case_id, analysis) {
  measures <- lavaan::fitMeasures(fit, indices)
  data.frame(case_id = case_id, analysis = analysis, as.list(measures), check.names = FALSE)
}
fit_out <- rbind(fit_row(path, "path_basic_fit", "path"),
  fit_row(cfa, "cfa_basic_fit", "cfa"), fit_row(mediation, "mediation_basic_fit", "mediation"))

parameter_row <- function(fit, case_id, analysis, lhs, op, rhs, group = "") {
  estimates <- lavaan::parameterEstimates(fit, standardized = TRUE, level = .95)
  group_labels <- lavaan::lavInspect(fit, "group.label")
  group_id <- if (nzchar(group)) match(group, group_labels) else NULL
  selected <- estimates$lhs == lhs & estimates$op == op & estimates$rhs == rhs
  if (!is.null(group_id)) selected <- selected & estimates$group == group_id
  row <- estimates[selected, , drop = FALSE]
  stopifnot(nrow(row) == 1L)
  data.frame(case_id = case_id, analysis = analysis, group = group,
    path = paste(lhs, op, rhs), label = if ("label" %in% names(row)) row$label else "", op = op,
    est = row$est, se = row$se, z = row$z, p = row$pvalue,
    ci_low = row$ci.lower, ci_high = row$ci.upper, std = row$std.all)
}
param_out <- rbind(
  parameter_row(path, "path_basic_x1", "path", "outcome_reg", "~", "x1"),
  parameter_row(path, "path_basic_x2", "path", "outcome_reg", "~", "x2"),
  parameter_row(cfa, "cfa_basic_loading_f1_1", "cfa", "F1", "=~", "f1_1"),
  parameter_row(cfa, "cfa_basic_cov_f1_f2", "cfa", "F1", "~~", "F2"),
  parameter_row(grouped, "cfa_group_control_loading_f1_1", "cfa", "F1", "=~", "f1_1", "control"),
  parameter_row(mediation, "mediation_indirect_mediator", "mediation", "indirect_mediator", ":=", "a1*b1"),
  parameter_row(grouped, "cfa_group_control_loading_f1_2", "cfa", "F1", "=~", "f1_2", "control"),
  parameter_row(grouped, "cfa_group_treatment_loading_f1_2", "cfa", "F1", "=~", "f1_2", "treatment"))

r2_out <- data.frame(case_id = c("path_basic_r2_outcome_reg", "mediation_r2_mediator"),
  analysis = c("path", "mediation"), label = c("outcome_reg", "mediator"),
  r2 = c(unname(lavaan::lavInspect(path, "r2")[["outcome_reg"]]),
    unname(lavaan::lavInspect(mediation, "r2")[["mediator"]])))
mi <- lavaan::modindices(cfa, sort. = TRUE, minimum.value = 1)[1, ]
mod_out <- data.frame(case_id = "cfa_modindices_top1", analysis = "cfa",
  lhs = mi$lhs, op = mi$op, rhs = mi$rhs, mi = mi$mi, epc = mi$epc, sepc.all = mi$sepc.all)

invariance_model <- "F1 =~ f1_1 + f1_2 + f1_3_rev + f1_4"
configural <- lavaan::cfa(invariance_model, df, group = "group2", group.equal = character(),
  estimator = "ML", missing = "listwise", se = "standard")
metric <- lavaan::cfa(invariance_model, df, group = "group2", group.equal = "loadings",
  estimator = "ML", missing = "listwise", se = "standard")
measures <- rbind(lavaan::fitMeasures(configural, indices), lavaan::fitMeasures(metric, indices))
invariance_out <- data.frame(case_id = c("invariance_configural", "invariance_metric"),
  analysis = "invariance", step = c("configural", "metric"), group_equal = c("", "loadings"),
  chisq = measures[, "chisq"], df = measures[, "df"], p = measures[, "pvalue"],
  cfi = measures[, "cfi"], tli = measures[, "tli"], rmsea = measures[, "rmsea"], srmr = measures[, "srmr"],
  delta_cfi = c(NA, diff(measures[, "cfi"])), delta_rmsea = c(NA, diff(measures[, "rmsea"])))
outputs <- list(fit = fit_out, params = param_out, r2 = r2_out, invariance = invariance_out, modindices = mod_out)
for (key in names(outputs)) write.csv(outputs[[key]],
  arg(paste0("--out-", key), paste0("tests/values/sem_", key, "_golden.csv")), row.names = FALSE)
