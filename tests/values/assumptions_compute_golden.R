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

safe_shapiro <- function(values, max_n = 5000) {
  values <- values[!is.na(values)]
  n <- length(values)
  if (n < 3 || n > max_n) {
    return(list(w = NA_real_, p = NA_real_, n = n))
  }
  test <- tryCatch(shapiro.test(values), error = function(e) NULL)
  if (is.null(test)) return(list(w = NA_real_, p = NA_real_, n = n))
  list(w = unname(test$statistic), p = test$p.value, n = n)
}

calc_levene <- function(values, group) {
  group <- as.factor(group)
  if (nlevels(group) < 2) {
    return(list(stat = NA_real_, df1 = NA_real_, df2 = NA_real_, p = NA_real_))
  }
  fit <- tryCatch(
    lm(abs(values - tapply(values, group, median, na.rm = TRUE)[as.character(group)]) ~ group),
    error = function(e) NULL
  )
  if (is.null(fit)) return(list(stat = NA_real_, df1 = NA_real_, df2 = NA_real_, p = NA_real_))
  res <- anova(fit)
  list(
    stat = res$`F value`[1],
    df1 = res$Df[1],
    df2 = res$Df[2],
    p = res$`Pr(>F)`[1]
  )
}

calc_bartlett <- function(values, group) {
  test <- tryCatch(bartlett.test(values, group), error = function(e) NULL)
  if (is.null(test)) return(list(stat = NA_real_, df1 = NA_real_, df2 = NA_real_, p = NA_real_))
  list(
    stat = unname(test$statistic),
    df1 = unname(test$parameter),
    df2 = NA_real_,
    p = test$p.value
  )
}

calc_fligner <- function(values, group) {
  test <- tryCatch(fligner.test(values, group), error = function(e) NULL)
  if (is.null(test)) return(list(stat = NA_real_, df1 = NA_real_, df2 = NA_real_, p = NA_real_))
  list(
    stat = unname(test$statistic),
    df1 = unname(test$parameter),
    df2 = NA_real_,
    p = test$p.value
  )
}

calc_var_test <- function(values, group) {
  group <- as.factor(group)
  levels <- levels(group)
  if (length(levels) != 2) return(list(stat = NA_real_, df1 = NA_real_, df2 = NA_real_, p = NA_real_))
  x <- values[group == levels[1]]
  y <- values[group == levels[2]]
  test <- tryCatch(var.test(x, y), error = function(e) NULL)
  if (is.null(test)) return(list(stat = NA_real_, df1 = NA_real_, df2 = NA_real_, p = NA_real_))
  df_vals <- unname(test$parameter)
  list(
    stat = unname(test$statistic),
    df1 = df_vals[1],
    df2 = df_vals[2],
    p = test$p.value
  )
}

calc_bp <- function(model) {
  res <- resid(model)
  model_frame <- model.frame(model)
  if (ncol(model_frame) <= 1) return(NULL)
  predictors <- model_frame[, -1, drop = FALSE]
  aux <- tryCatch(lm(res^2 ~ ., data = predictors), error = function(e) NULL)
  if (is.null(aux)) return(NULL)
  r2 <- summary(aux)$r.squared
  n <- length(res)
  df <- length(coef(aux)) - 1
  if (df <= 0) return(NULL)
  stat <- n * r2
  p <- pchisq(stat, df, lower.tail = FALSE)
  list(stat = stat, df1 = df, df2 = NA_real_, p = p)
}

calc_dw <- function(resid) {
  if (length(resid) < 2) return(NA_real_)
  sum(diff(resid)^2) / sum(resid^2)
}

calc_vif <- function(model) {
  mm <- model.matrix(model)
  if ("(Intercept)" %in% colnames(mm)) {
    mm <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]
  }
  if (ncol(mm) == 0) return(data.frame())
  if (ncol(mm) == 1) {
    return(data.frame(term = colnames(mm), vif = 1, stringsAsFactors = FALSE))
  }
  vifs <- numeric(ncol(mm))
  for (j in seq_len(ncol(mm))) {
    fit <- tryCatch(lm(mm[, j] ~ mm[, -j, drop = FALSE]), error = function(e) NULL)
    if (is.null(fit)) {
      vifs[j] <- NA_real_
    } else {
      r2 <- summary(fit)$r.squared
      vifs[j] <- ifelse(is.na(r2) || r2 >= 1, Inf, 1 / (1 - r2))
    }
  }
  data.frame(term = colnames(mm), vif = vifs, stringsAsFactors = FALSE)
}

calc_linearity <- function(x, residuals) {
  if (!is.numeric(x) || length(x) < 3) return(list(stat = NA_real_, p = NA_real_))
  test <- tryCatch(cor.test(x, residuals), error = function(e) NULL)
  if (is.null(test)) return(list(stat = NA_real_, p = NA_real_))
  list(stat = unname(test$estimate), p = test$p.value)
}

calc_abs_resid_cor <- function(resid, fitted_vals) {
  if (length(resid) < 3 || length(fitted_vals) < 3) return(NULL)
  test <- tryCatch(cor.test(abs(resid), fitted_vals), error = function(e) NULL)
  if (is.null(test)) return(NULL)
  list(stat = unname(test$estimate), p = test$p.value, df1 = unname(test$parameter))
}

extract_first_numeric <- function(value) {
  if (is.null(value)) return(NA_real_)
  if (is.numeric(value)) return(as.numeric(value[1]))
  if (is.character(value)) return(suppressWarnings(as.numeric(value[1])))
  if (is.list(value) && length(value) > 0) return(extract_first_numeric(value[[1]]))
  NA_real_
}

extract_named_numeric <- function(obj, keys) {
  for (key in keys) {
    if (!is.null(obj[[key]])) {
      val <- extract_first_numeric(obj[[key]])
      if (!is.na(val)) return(val)
    }
  }
  NA_real_
}

extract_test_values <- function(test) {
  if (is.null(test)) {
    return(list(stat = NA_real_, p = NA_real_, df1 = NA_real_, df2 = NA_real_))
  }
  if (inherits(test, "htest")) {
    stat <- extract_first_numeric(test$statistic)
    p <- extract_first_numeric(test$p.value)
    df <- test$parameter
    df1 <- NA_real_
    df2 <- NA_real_
    if (!is.null(df)) {
      df_vals <- as.numeric(df)
      if (length(df_vals) > 0) df1 <- df_vals[1]
      if (length(df_vals) > 1) df2 <- df_vals[2]
    }
    return(list(stat = stat, p = p, df1 = df1, df2 = df2))
  }
  if (is.data.frame(test) && nrow(test) > 0) {
    row <- test[1, , drop = FALSE]
    stat <- extract_named_numeric(row, c("statistic", "stat", "chisq", "chi.square", "t", "z"))
    p <- extract_named_numeric(row, c("p", "p.value", "p_value", "pval"))
    df1 <- extract_named_numeric(row, c("df", "df1", "df_1"))
    df2 <- extract_named_numeric(row, c("df2", "df_2"))
    return(list(stat = stat, p = p, df1 = df1, df2 = df2))
  }
  if (is.list(test)) {
    stat <- extract_named_numeric(test, c("statistic", "stat", "chisq", "chi.square", "t", "z"))
    p <- extract_named_numeric(test, c("p.value", "p", "p_value", "pval"))
    df1 <- extract_named_numeric(test, c("df", "df1", "df_1"))
    df2 <- extract_named_numeric(test, c("df2", "df_2"))
    return(list(stat = stat, p = p, df1 = df1, df2 = df2))
  }
  list(stat = NA_real_, p = NA_real_, df1 = NA_real_, df2 = NA_real_)
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
mauchly <- mauchly.test(fit)
rows[[length(rows) + 1]] <- make_row(
  "anova_within_mauchly_within", "anova", "Within", "Sphericity", "Mauchly", "Within", "",
  unname(mauchly$statistic), NA_real_, NA_real_, mauchly$p.value, NA_real_, nrow(within_df)
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
mauchly <- mauchly.test(fit)
rows[[length(rows) + 1]] <- make_row(
  "anova_mixed_mauchly_within", "anova", "Mixed", "Sphericity", "Mauchly", "Within", "",
  unname(mauchly$statistic), NA_real_, NA_real_, mauchly$p.value, NA_real_, nrow(mixed_df)
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
lin <- calc_linearity(mf[["x1"]], resid_vals)
rows[[length(rows) + 1]] <- make_row(
  "regression_linearity_x1_block1", "regression", "Block 1", "Linearity", "Residual correlation", "x1", "",
  lin$stat, NA_real_, NA_real_, lin$p, NA_real_, n_reg
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
mm_fit <- lme4::lmer(score ~ time + group3 + x1 + (1 | id), data = long_df, REML = TRUE)
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
  perf_vals <- extract_test_values(perf_test)
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
      cooks <- tryCatch(influence.ME::cooks.distance(infl), error = function(e) NULL)
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
    NA_real_, NA_real_, NA_real_, NA_real_, neg_count, NA_real_
  )
  if ("std.all" %in% names(pe)) {
    load_rows <- pe[pe$op == "=~", , drop = FALSE]
    count <- sum(abs(load_rows$std.all) > 1, na.rm = TRUE)
    rows[[length(rows) + 1]] <- make_row(
      "sem_cfa_heywood_std_loading_gt1", "sem", "CFA", "Heywood", "Std. loading > 1", "Model", "",
      NA_real_, NA_real_, NA_real_, NA_real_, count, NA_real_
    )
  }
}

assumptions_rows <- do.call(rbind, rows)
write.csv(assumptions_rows, out_path, row.names = FALSE)
cat("Wrote assumptions golden values to", out_path, "\n")
