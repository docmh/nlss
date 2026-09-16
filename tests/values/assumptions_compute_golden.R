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
out_path <- trim_arg(get_arg("--out", file.path("tests", "values", "assumptions_golden.csv")))

if (is.null(data_path) || !file.exists(data_path)) {
  stop("Missing --data path.")
}
if (is.null(out_path)) {
  stop("Missing --out path.")
}

df <- read.csv(data_path, stringsAsFactors = FALSE)

# Only reference adapters: no NLSS helper or duplicated scientific engine.
for (pkg in c("car", "lmtest", "lme4", "lavaan", "performance", "influence.ME")) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Golden generation requires ", pkg)
}
# influence.ME declares lme4 in Depends and calls its exported functions unqualified.
suppressPackageStartupMessages(library(lme4))
safe_shapiro <- function(values, max_n = 5000) {
  values <- values[!is.na(values)]
  stopifnot(length(values) >= 3L, length(values) <= max_n)
  ref <- stats::shapiro.test(values)
  list(w = unname(ref$statistic), p = ref$p.value, n = length(values))
}
calc_levene <- function(values, group) {
  ref <- car::leveneTest(values, factor(group), center = median)
  list(stat = ref$`F value`[1], df1 = ref$Df[1], df2 = ref$Df[2], p = ref$`Pr(>F)`[1])
}
homogeneity_reference <- function(test) {
  list(stat = unname(test$statistic), df1 = unname(test$parameter[1]),
       df2 = if (length(test$parameter) > 1L) unname(test$parameter[2]) else NA_real_,
       p = test$p.value)
}
calc_bartlett <- function(values, group) homogeneity_reference(stats::bartlett.test(values, group))
calc_fligner <- function(values, group) homogeneity_reference(stats::fligner.test(values, group))
calc_var_test <- function(values, group) homogeneity_reference(stats::var.test(values ~ factor(group)))
calc_bp <- function(model) homogeneity_reference(lmtest::bptest(model, studentize = TRUE))
calc_dw <- function(resid) sum(diff(resid)^2) / sum(resid^2)
calc_vif <- function(model) {
  value <- car::vif(model)
  data.frame(term = names(value), vif = unname(value))
}
calc_linearity <- function(model) {
  data <- model.frame(model)
  augmented <- lm(outcome_reg ~ x1 + x2 + x3 + I((x1 - mean(x1))^2), data = data)
  ref <- stats::anova(model, augmented)
  list(stat = ref$F[2], df1 = ref$Df[2], df2 = df.residual(augmented), p = ref$`Pr(>F)`[2])
}
calc_abs_resid_cor <- function(resid, fitted_vals) {
  ref <- stats::cor.test(abs(resid), fitted_vals)
  list(stat = unname(ref$estimate), p = ref$p.value, df1 = unname(ref$parameter))
}

make_row <- function(case_id, analysis_type, model, assumption, test, target, group,
                     statistic, df1, df2, p, value, n) {
  data.frame(
    case_id = case_id,
    analysis_type = analysis_type,
    model = model,
    assumption = assumption,
    test = test,
    target = target,
    group = group,
    statistic = as.numeric(statistic),
    df1 = as.numeric(df1),
    df2 = as.numeric(df2),
    p = as.numeric(p),
    value = as.numeric(value),
    n = as.numeric(n),
    stringsAsFactors = FALSE
  )
}

rows <- list()

# t-test one-sample
vals <- df[["x1"]]
sh <- safe_shapiro(vals)
rows[[length(rows) + 1]] <- make_row(
  "ttest_one_sample_shapiro_x1", "ttest", "One-sample", "Normality", "Shapiro-Wilk", "x1", "",
  sh$w, NA_real_, NA_real_, sh$p, NA_real_, sh$n
)

# t-test independent (group2)
group <- df[["group2"]]
complete <- !is.na(vals) & !is.na(group)
vals_ind <- vals[complete]
group_ind <- as.character(group[complete])
control_vals <- vals_ind[group_ind == "control"]
sh <- safe_shapiro(control_vals)
rows[[length(rows) + 1]] <- make_row(
  "ttest_independent_shapiro_control_x1", "ttest", "Independent", "Normality", "Shapiro-Wilk", "x1", "control",
  sh$w, NA_real_, NA_real_, sh$p, NA_real_, sh$n
)
lev <- calc_levene(vals_ind, group_ind)
rows[[length(rows) + 1]] <- make_row(
  "ttest_independent_levene_x1", "ttest", "Independent", "Homogeneity", "Levene (median)", "x1", "",
  lev$stat, lev$df1, lev$df2, lev$p, NA_real_, length(vals_ind)
)
bart <- calc_bartlett(vals_ind, group_ind)
rows[[length(rows) + 1]] <- make_row(
  "ttest_independent_bartlett_x1", "ttest", "Independent", "Homogeneity", "Bartlett", "x1", "",
  bart$stat, bart$df1, bart$df2, bart$p, NA_real_, length(vals_ind)
)
flig <- calc_fligner(vals_ind, group_ind)
rows[[length(rows) + 1]] <- make_row(
  "ttest_independent_fligner_x1", "ttest", "Independent", "Homogeneity", "Fligner-Killeen", "x1", "",
  flig$stat, flig$df1, flig$df2, flig$p, NA_real_, length(vals_ind)
)
ftest <- calc_var_test(vals_ind, group_ind)
rows[[length(rows) + 1]] <- make_row(
  "ttest_independent_f_x1", "ttest", "Independent", "Homogeneity", "F-test", "x1", "",
  ftest$stat, ftest$df1, ftest$df2, ftest$p, NA_real_, length(vals_ind)
)

# t-test paired
x <- df[["pre_score"]]
y <- df[["post_score"]]
complete <- !is.na(x) & !is.na(y)
diff_vals <- x[complete] - y[complete]
sh <- safe_shapiro(diff_vals)
rows[[length(rows) + 1]] <- make_row(
  "ttest_paired_shapiro_pre_post", "ttest", "Paired", "Normality", "Shapiro-Wilk", "pre_score - post_score", "",
  sh$w, NA_real_, NA_real_, sh$p, NA_real_, sh$n
)

# ANOVA between
dv <- df[["outcome_anova"]]
group3 <- df[["group3"]]
complete <- !is.na(dv) & !is.na(group3)
dv_between <- dv[complete]
group_between <- as.character(group3[complete])
sh <- safe_shapiro(dv_between[group_between == "A"])
rows[[length(rows) + 1]] <- make_row(
  "anova_between_shapiro_A_outcome_anova", "anova", "Between", "Normality", "Shapiro-Wilk", "outcome_anova", "A",
  sh$w, NA_real_, NA_real_, sh$p, NA_real_, sh$n
)
lev <- calc_levene(dv_between, group_between)
rows[[length(rows) + 1]] <- make_row(
  "anova_between_levene_outcome_anova", "anova", "Between", "Homogeneity", "Levene (median)", "outcome_anova", "",
  lev$stat, lev$df1, lev$df2, lev$p, NA_real_, length(dv_between)
)

# ANOVA within
within_vars <- c("pre_score", "mid_score", "post_score")
within_df <- df[, within_vars, drop = FALSE]
within_df <- within_df[complete.cases(within_df), , drop = FALSE]
sh <- safe_shapiro(within_df[["pre_score"]])
rows[[length(rows) + 1]] <- make_row(
  "anova_within_shapiro_pre_score", "anova", "Within", "Normality", "Shapiro-Wilk", "pre_score", "",
  sh$w, NA_real_, NA_real_, sh$p, NA_real_, sh$n
)
fit <- lm(cbind(pre_score, mid_score, post_score) ~ 1, data = within_df)
mauchly <- stats::mauchly.test(fit, X = ~1)
contrast_dimension <- length(within_vars) - 1L
mauchly_df <- contrast_dimension * (contrast_dimension + 1L) / 2 - 1L
rows[[length(rows) + 1]] <- make_row(
  "anova_within_mauchly_within", "anova", "Within", "Sphericity", "Mauchly", "Within", "",
  unname(mauchly$statistic), mauchly_df, NA_real_, mauchly$p.value, NA_real_, nrow(within_df)
)

# ANOVA mixed (within + between group3)
mixed_df <- df[, c(within_vars, "group3"), drop = FALSE]
mixed_df <- mixed_df[complete.cases(mixed_df), , drop = FALSE]
group_factor <- interaction(mixed_df["group3"], drop = TRUE, sep = ":")
sh <- safe_shapiro(mixed_df[["pre_score"]][group_factor == "A"])
rows[[length(rows) + 1]] <- make_row(
  "anova_mixed_shapiro_A_pre_score", "anova", "Mixed", "Normality", "Shapiro-Wilk", "pre_score", "A",
  sh$w, NA_real_, NA_real_, sh$p, NA_real_, sh$n
)
lev <- calc_levene(mixed_df[["pre_score"]], group_factor)
rows[[length(rows) + 1]] <- make_row(
  "anova_mixed_levene_pre_score", "anova", "Mixed", "Homogeneity", "Levene (median)", "pre_score", "",
  lev$stat, lev$df1, lev$df2, lev$p, NA_real_, nrow(mixed_df)
)
fit <- lm(cbind(pre_score, mid_score, post_score) ~ group3, data = mixed_df)
mauchly <- stats::mauchly.test(fit, X = ~1)
rows[[length(rows) + 1]] <- make_row(
  "anova_mixed_mauchly_within", "anova", "Mixed", "Sphericity", "Mauchly", "Within", "",
  unname(mauchly$statistic), mauchly_df, NA_real_, mauchly$p.value, NA_real_, nrow(mixed_df)
)

# Regression diagnostics
reg_model <- lm(outcome_reg ~ x1 + x2 + x3, data = df, na.action = na.omit)
resid_vals <- resid(reg_model)
n_reg <- length(resid_vals)
sh <- safe_shapiro(resid_vals)
rows[[length(rows) + 1]] <- make_row(
  "regression_shapiro_residuals_block1", "regression", "Block 1", "Normality", "Shapiro-Wilk", "Residuals", "",
  sh$w, NA_real_, NA_real_, sh$p, NA_real_, sh$n
)
mf <- model.frame(reg_model)
lin <- calc_linearity(reg_model)
rows[[length(rows) + 1]] <- make_row(
  "regression_linearity_x1_block1", "regression", "Block 1", "Linearity", "Quadratic added-term F", "x1", "",
  lin$stat, lin$df1, lin$df2, lin$p, NA_real_, n_reg
)
bp <- calc_bp(reg_model)
rows[[length(rows) + 1]] <- make_row(
  "regression_breusch_pagan_block1", "regression", "Block 1", "Homoscedasticity", "Breusch-Pagan", "Residuals", "",
  bp$stat, bp$df1, bp$df2, bp$p, NA_real_, n_reg
)
dw <- calc_dw(resid_vals)
rows[[length(rows) + 1]] <- make_row(
  "regression_durbin_watson_block1", "regression", "Block 1", "Independence", "Durbin-Watson", "Residuals", "",
  dw, NA_real_, NA_real_, NA_real_, NA_real_, n_reg
)
std_res <- tryCatch(rstandard(reg_model), error = function(e) NULL)
if (!is.null(std_res)) {
  max_abs <- max(abs(std_res), na.rm = TRUE)
  rows[[length(rows) + 1]] <- make_row(
    "regression_outliers_block1", "regression", "Block 1", "Outliers", "Std. residuals", "Residuals", "",
    NA_real_, NA_real_, NA_real_, NA_real_, max_abs, n_reg
  )
}
cooks <- tryCatch(cooks.distance(reg_model), error = function(e) NULL)
if (!is.null(cooks)) {
  max_cook <- max(cooks, na.rm = TRUE)
  rows[[length(rows) + 1]] <- make_row(
    "regression_influence_block1", "regression", "Block 1", "Influence", "Cook's distance", "Residuals", "",
    NA_real_, NA_real_, NA_real_, NA_real_, max_cook, n_reg
  )
}
vifs <- calc_vif(reg_model)
if (nrow(vifs) > 0) {
  row <- vifs[vifs$term == "x1", , drop = FALSE]
  if (nrow(row) > 0) {
    rows[[length(rows) + 1]] <- make_row(
      "regression_vif_x1_block1", "regression", "Block 1", "Multicollinearity", "VIF", "x1", "",
      NA_real_, NA_real_, NA_real_, NA_real_, row$vif[1], n_reg
    )
  }
}

# Mixed models diagnostics
if (!requireNamespace("lme4", quietly = TRUE)) {
  stop("Mixed models golden values require lme4.")
}
long_df <- reshape(
  df,
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
mm_fit <- lme4::lmer(score ~ time + group3 + x1 + (1 | id), data = long_df, REML = TRUE,
  control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000L)))
mm_resid <- residuals(mm_fit)
mm_fitted <- fitted(mm_fit)
n_mm <- length(mm_resid)
sh <- safe_shapiro(mm_resid)
rows[[length(rows) + 1]] <- make_row(
  "mixed_models_shapiro_residuals", "mixed_models", "Mixed", "Normality", "Shapiro-Wilk", "Residuals", "",
  sh$w, NA_real_, NA_real_, sh$p, NA_real_, sh$n
)
ranefs <- lme4::ranef(mm_fit, condVar = FALSE)
if (length(ranefs) > 0 && "id" %in% names(ranefs)) {
  re_df <- ranefs[["id"]]
  if (is.data.frame(re_df) && "(Intercept)" %in% names(re_df)) {
    sh <- safe_shapiro(re_df[["(Intercept)"]])
    rows[[length(rows) + 1]] <- make_row(
      "mixed_models_random_effects_shapiro_intercept", "mixed_models", "Mixed", "Random-effects normality", "Shapiro-Wilk", "(Intercept)", "id",
      sh$w, NA_real_, NA_real_, sh$p, NA_real_, sh$n
    )
  }
}
cor_res <- calc_abs_resid_cor(mm_resid, mm_fitted)
if (!is.null(cor_res)) {
  rows[[length(rows) + 1]] <- make_row(
    "mixed_models_homoscedasticity_abs_resid", "mixed_models", "Mixed", "Homoscedasticity", "Abs resid vs fitted", "Residuals", "",
    cor_res$stat, cor_res$df1, NA_real_, cor_res$p, NA_real_, n_mm
  )
}
if (requireNamespace("performance", quietly = TRUE)) {
  perf_test <- tryCatch(performance::check_heteroscedasticity(mm_fit), error = function(e) NULL)
  if (is.null(perf_test)) stop("Direct performance reference failed.")
  perf_vals <- list(stat = NA_real_, df1 = NA_real_, df2 = NA_real_, p = as.numeric(perf_test)[1])
  if (!is.na(perf_vals$stat) || !is.na(perf_vals$p)) {
    rows[[length(rows) + 1]] <- make_row(
      "mixed_models_performance_heteroscedasticity", "mixed_models", "Mixed", "Homoscedasticity",
      "performance::check_heteroscedasticity", "Residuals", "",
      perf_vals$stat, perf_vals$df1, perf_vals$df2, perf_vals$p, NA_real_, n_mm
    )
  }
}
resid_sd <- sd(mm_resid, na.rm = TRUE)
if (is.finite(resid_sd) && resid_sd > 0) {
  std_res <- mm_resid / resid_sd
  max_abs <- max(abs(std_res), na.rm = TRUE)
  rows[[length(rows) + 1]] <- make_row(
    "mixed_models_outliers", "mixed_models", "Mixed", "Outliers", "Std. residuals", "Residuals", "",
    NA_real_, NA_real_, NA_real_, NA_real_, max_abs, n_mm
  )
}
if (requireNamespace("influence.ME", quietly = TRUE)) {
  group_list <- tryCatch(lme4::getME(mm_fit, "flist"), error = function(e) NULL)
  if (!is.null(group_list) && "id" %in% names(group_list)) {
    infl <- tryCatch(influence.ME::influence(mm_fit, group = "id"), error = function(e) NULL)
    if (!is.null(infl)) {
      cooks <- tryCatch(stats::cooks.distance(infl), error = function(e) NULL)
      if (!is.null(cooks)) {
        max_cook <- max(cooks, na.rm = TRUE)
        rows[[length(rows) + 1]] <- make_row(
          "mixed_models_influence_id", "mixed_models", "Mixed", "Influence", "Cook's distance (cluster)", "id", "",
          NA_real_, NA_real_, NA_real_, NA_real_, max_cook, length(cooks)
        )
      }
    }
  }
}

# SEM (CFA) diagnostics
if (!requireNamespace("lavaan", quietly = TRUE)) {
  stop("SEM golden values require lavaan.")
}
model_vars <- c(
  "f1_1", "f1_2", "f1_3_rev", "f1_4",
  "f2_1", "f2_2", "f2_3", "f2_4_rev"
)
ordered_vars <- c("f1_1", "f1_2")
cont_vars <- setdiff(model_vars, ordered_vars)
cont_vars <- cont_vars[sapply(df[cont_vars], is.numeric)]
if (length(cont_vars) > 0) {
  target_var <- if ("f1_3_rev" %in% cont_vars) "f1_3_rev" else cont_vars[1]
  sh <- safe_shapiro(df[[target_var]])
  rows[[length(rows) + 1]] <- make_row(
    "sem_cfa_shapiro_f1_3_rev", "sem", "CFA", "Normality", "Shapiro-Wilk", target_var, "",
    sh$w, NA_real_, NA_real_, sh$p, NA_real_, sh$n
  )
}
cont_complete <- if (length(cont_vars) > 0) {
  cont_data <- df[, cont_vars, drop = FALSE]
  cont_data[complete.cases(cont_data), , drop = FALSE]
} else {
  NULL
}
if (!is.null(cont_complete) && nrow(cont_complete) > 2 && ncol(cont_complete) > 1) {
  cov_mat <- cov(cont_complete)
  center <- colMeans(cont_complete)
  d2 <- mahalanobis(cont_complete, center, cov_mat)
  max_dist <- max(d2, na.rm = TRUE)
  rows[[length(rows) + 1]] <- make_row(
    "sem_cfa_mahalanobis", "sem", "CFA", "Outliers", "Mahalanobis distance", "Indicators", "",
    NA_real_, NA_real_, NA_real_, NA_real_, max_dist, nrow(cont_complete)
  )
  cor_mat <- cor(cont_complete, use = "pairwise.complete.obs")
  max_cor <- max(abs(cor_mat[upper.tri(cor_mat)]), na.rm = TRUE)
  rows[[length(rows) + 1]] <- make_row(
    "sem_cfa_max_cor", "sem", "CFA", "Multicollinearity", "Max |r|", "Indicators", "",
    NA_real_, NA_real_, NA_real_, NA_real_, max_cor, nrow(cont_complete)
  )
  kappa_val <- kappa(cor_mat)
  rows[[length(rows) + 1]] <- make_row(
    "sem_cfa_kappa", "sem", "CFA", "Multicollinearity", "Condition number", "Indicators", "",
    NA_real_, NA_real_, NA_real_, NA_real_, kappa_val, nrow(cont_complete)
  )
}

model_text <- paste(
  "F1 =~ f1_1 + f1_2 + f1_3_rev + f1_4",
  "F2 =~ f2_1 + f2_2 + f2_3 + f2_4_rev",
  sep = "\n"
)
fit <- lavaan::cfa(
  model_text,
  data = df,
  estimator = "WLSMV",
  missing = "listwise",
  se = "standard",
  ordered = ordered_vars
)
pe <- lavaan::parameterEstimates(fit, standardized = TRUE)
if (!is.null(pe) && nrow(pe) > 0) {
  var_rows <- pe[pe$op == "~~" & pe$lhs == pe$rhs, , drop = FALSE]
  neg_count <- sum(var_rows$est < 0, na.rm = TRUE)
  rows[[length(rows) + 1]] <- make_row(
    "sem_cfa_heywood_negative_variances", "sem", "CFA", "Heywood", "Negative variances", "Model", "",
    NA_real_, NA_real_, NA_real_, NA_real_, neg_count, lavaan::lavInspect(fit, "nobs")
  )
  if ("std.all" %in% names(pe)) {
    load_rows <- pe[pe$op == "=~", , drop = FALSE]
    count <- sum(abs(load_rows$std.all) > 1, na.rm = TRUE)
    rows[[length(rows) + 1]] <- make_row(
      "sem_cfa_heywood_std_loading_gt1", "sem", "CFA", "Heywood", "Std. loading > 1", "Model", "",
      NA_real_, NA_real_, NA_real_, NA_real_, count, lavaan::lavInspect(fit, "nobs")
    )
  }
}

assumptions_rows <- do.call(rbind, rows)
stopifnot(nrow(assumptions_rows) == 33L, !anyDuplicated(assumptions_rows$case_id))
write.csv(assumptions_rows, out_path, row.names = FALSE)
cat("Wrote assumptions golden values to", out_path, "\n")
