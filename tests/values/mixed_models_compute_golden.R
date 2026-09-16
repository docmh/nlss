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

require_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Required package '%s' is not installed.", pkg))
  }
}

data_path <- trim_arg(get_arg("--data", file.path("tests", "data", "golden_dataset.csv")))

out_fixed <- trim_arg(get_arg("--out-fixed", file.path("tests", "values", "mixed_models_fixed_golden.csv")))
out_random <- trim_arg(get_arg("--out-random", file.path("tests", "values", "mixed_models_random_golden.csv")))
out_fit <- trim_arg(get_arg("--out-fit", file.path("tests", "values", "mixed_models_fit_golden.csv")))
out_r2_icc <- trim_arg(get_arg("--out-r2-icc", file.path("tests", "values", "mixed_models_r2_icc_golden.csv")))
out_anova <- trim_arg(get_arg("--out-anova", file.path("tests", "values", "mixed_models_anova_golden.csv")))
out_emmeans <- trim_arg(get_arg("--out-emmeans", file.path("tests", "values", "mixed_models_emmeans_golden.csv")))
out_contrasts <- trim_arg(get_arg("--out-contrasts", file.path("tests", "values", "mixed_models_contrasts_golden.csv")))
out_diagnostics <- trim_arg(get_arg("--out-diagnostics", file.path("tests", "values", "mixed_models_diagnostics_golden.csv")))

if (is.null(data_path) || !file.exists(data_path)) {
  stop("Missing --data path.")
}

if (is.null(out_fixed) || is.null(out_random) || is.null(out_fit) || is.null(out_r2_icc) ||
    is.null(out_anova) || is.null(out_emmeans) || is.null(out_contrasts) || is.null(out_diagnostics)) {
  stop("Missing output path(s).")
}

options(scipen = 999, digits = 15)

require_pkg("lme4")
require_pkg("lmerTest")
require_pkg("performance")
require_pkg("emmeans")

# Independent package oracles: no NLSS source or adapter helpers are loaded.
raw <- read.csv(data_path, stringsAsFactors = FALSE)
long_df <- reshape(raw, varying = c("pre_score", "mid_score", "post_score"),
  v.names = "score", timevar = "time", times = c("pre", "mid", "post"), idvar = "id", direction = "long")
long_df <- long_df[complete.cases(long_df[, c("score", "id", "time", "group3", "x1")]), c("id", "time", "score", "group3", "x1")]
long_df$time <- factor(long_df$time)
long_df$group3 <- factor(long_df$group3)
base_formula <- score ~ time + group3 + x1 + (1 | id)
fit_base <- lmerTest::lmer(base_formula, long_df, REML = TRUE,
  control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
fit_std <- lmerTest::lmer(base_formula, long_df, REML = FALSE,
  control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 20000)))
coefficient_table <- function(fit, standardize = FALSE) {
  coef <- coef(summary(fit, ddf = "Satterthwaite"))
  critical <- qt(.975, coef[, "df"])
  beta <- rep(NA_real_, nrow(coef))
  if (standardize) beta[rownames(coef) == "x1"] <- coef["x1", "Estimate"] * sd(long_df$x1) / sd(long_df$score)
  data.frame(term = rownames(coef), estimate = coef[, "Estimate"], se = coef[, "Std. Error"],
    df = coef[, "df"], t = coef[, "t value"], p = coef[, "Pr(>|t|)"],
    ci_low = coef[, "Estimate"] - critical * coef[, "Std. Error"],
    ci_high = coef[, "Estimate"] + critical * coef[, "Std. Error"], std_beta = beta, row.names = NULL)
}
fixed_base <- coefficient_table(fit_base)
fixed_std <- coefficient_table(fit_std, TRUE)
vc <- as.data.frame(lme4::VarCorr(fit_base))
random_base <- data.frame(group = vc$grp, term = ifelse(is.na(vc$var1), "Residual", vc$var1),
  variance = vc$vcov, stddev = vc$sdcor, corr = NA_real_)
fit_base_stats <- data.frame(n = nobs(fit_base), aic = AIC(fit_base), bic = BIC(fit_base),
  logLik = as.numeric(logLik(fit_base)), deviance = suppressWarnings(deviance(fit_base)))
r2 <- performance::r2(fit_base)
r2_base <- data.frame(r2_marginal = r2$R2_marginal, r2_conditional = r2$R2_conditional)
icc_base <- data.frame(icc = performance::icc(fit_base)$ICC_adjusted)
anova_base <- as.data.frame(anova(fit_base, type = 3, ddf = "Satterthwaite"))
anova_base$term <- rownames(anova_base)
rownames(anova_base) <- NULL
shapiro <- shapiro.test(residuals(fit_base))
diagnostics_base <- data.frame(metric = c("singular_fit", "convergence", "shapiro_wilk"),
  value = c(as.character(lme4::isSingular(fit_base)), if (fit_base@optinfo$conv$opt == 0) "ok" else "warning", "available"),
  statistic = c(NA_real_, NA_real_, unname(shapiro$statistic)), p = c(NA_real_, NA_real_, shapiro$p.value), note = "")

fit_emm <- lmerTest::lmer(score ~ time * group3 + x1 + (1 | id), long_df, REML = TRUE,
  control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
emm <- emmeans::emmeans(fit_emm, specs = ~ time * group3, lmer.df = "satterthwaite",
  lmerTest.limit = Inf, pbkrtest.limit = Inf, disable.lmerTest = FALSE, disable.pbkrtest = FALSE)
es <- summary(emm, infer = c(TRUE, TRUE), level = .9, adjust = "none")
emmeans_rows <- data.frame(term = "time*group3", level = paste0("time=", es$time, ", group3=", es$group3),
  contrast = "", emmean = es$emmean, estimate = NA_real_, se = es$SE, df = es$df, t = es$t.ratio,
  p = es$p.value, p_adj = NA_real_, ci_low = es$lower.CL, ci_high = es$upper.CL, method = "emmeans")
cs <- summary(emmeans::contrast(emm, method = "pairwise"), infer = c(TRUE, TRUE), adjust = "holm", level = .9)
contrast_rows <- data.frame(term = "time*group3", level = "", contrast = cs$contrast, emmean = NA_real_,
  estimate = cs$estimate, se = cs$SE, df = cs$df, t = cs$t.ratio, p = NA_real_, p_adj = cs$p.value,
  ci_low = cs$lower.CL, ci_high = cs$upper.CL, method = "pairwise")

require_row <- function(df, label) {
  if (is.null(df) || nrow(df) == 0) stop(paste0("Missing expected row: ", label))
  df
}

pick_row <- function(df, term) {
  require_row(df[df$term == term, , drop = FALSE], paste0("term=", term))
}

pick_row_level <- function(df, term, level) {
  require_row(df[df$term == term & df$level == level, , drop = FALSE], paste0("term=", term, ", level=", level))
}

pick_row_contrast <- function(df, term, contrast) {
  require_row(df[df$term == term & df$contrast == contrast, , drop = FALSE], paste0("term=", term, ", contrast=", contrast))
}

# Fixed effects goldens
fixed_rows <- rbind(
  cbind(
    data.frame(
      case_id = "base_timepost",
      formula = "score ~ time + group3 + x1 + (1|id)",
      dv = "",
      fixed = "",
      random = "(1|id)",
      standardize = "none",
      stringsAsFactors = FALSE
    ),
    pick_row(fixed_base, "timepost")
  ),
  cbind(
    data.frame(
      case_id = "base_timepre",
      formula = "score ~ time + group3 + x1 + (1|id)",
      dv = "",
      fixed = "",
      random = "(1|id)",
      standardize = "none",
      stringsAsFactors = FALSE
    ),
    pick_row(fixed_base, "timepre")
  ),
  cbind(
    data.frame(
      case_id = "base_x1",
      formula = "score ~ time + group3 + x1 + (1|id)",
      dv = "",
      fixed = "",
      random = "(1|id)",
      standardize = "none",
      stringsAsFactors = FALSE
    ),
    pick_row(fixed_base, "x1")
  ),
  cbind(
    data.frame(
      case_id = "standardize_x1",
      formula = "",
      dv = "score",
      fixed = "time,group3,x1",
      random = "(1|id)",
      standardize = "predictors",
      stringsAsFactors = FALSE
    ),
    pick_row(fixed_std, "x1")
  )
)

# Random effects goldens
random_rows <- rbind(
  cbind(
    data.frame(
      case_id = "base_intercept",
      formula = "score ~ time + group3 + x1 + (1|id)",
      dv = "",
      fixed = "",
      random = "(1|id)",
      stringsAsFactors = FALSE
    ),
    pick_row(random_base, "(Intercept)")
  )
)

# Fit stats golden
fit_rows <- cbind(
  data.frame(
    case_id = "base_fit",
    formula = "score ~ time + group3 + x1 + (1|id)",
    stringsAsFactors = FALSE
  ),
  fit_base_stats[1, , drop = FALSE]
)

# R2/ICC golden
r2_val <- if (nrow(r2_base) > 0) r2_base[1, , drop = FALSE] else data.frame(r2_marginal = NA_real_, r2_conditional = NA_real_)
icc_val <- if (nrow(icc_base) > 0) icc_base[1, , drop = FALSE] else data.frame(icc = NA_real_)
r2_icc_rows <- data.frame(
  case_id = "base_r2_icc",
  formula = "score ~ time + group3 + x1 + (1|id)",
  r2_marginal = r2_val$r2_marginal,
  r2_conditional = r2_val$r2_conditional,
  icc = icc_val$icc,
  stringsAsFactors = FALSE
)

# ANOVA golden
anova_time <- anova_base[anova_base$term == "time", , drop = FALSE]
anova_x1 <- anova_base[anova_base$term == "x1", , drop = FALSE]
anova_rows <- rbind(
  cbind(
    data.frame(
      case_id = "anova_time",
      formula = "score ~ time + group3 + x1 + (1|id)",
      type = "III",
      df_method = "satterthwaite",
      stringsAsFactors = FALSE
    ),
    anova_time
  ),
  cbind(
    data.frame(
      case_id = "anova_x1",
      formula = "score ~ time + group3 + x1 + (1|id)",
      type = "III",
      df_method = "satterthwaite",
      stringsAsFactors = FALSE
    ),
    anova_x1
  )
)

# emmeans golden
emm_row <- pick_row_level(emmeans_rows, "time*group3", "time=mid, group3=A")
emmeans_out <- cbind(
  data.frame(
    case_id = "emmeans_mid_A",
    formula = "score ~ time * group3 + x1 + (1|id)",
    emmeans = "time*group3",
    stringsAsFactors = FALSE
  ),
  emm_row
)

# contrasts golden
contrast_row <- pick_row_contrast(contrast_rows, "time*group3", "mid A - post A")
contrasts_out <- cbind(
  data.frame(
    case_id = "contrast_midA_postA",
    formula = "score ~ time * group3 + x1 + (1|id)",
    emmeans = "time*group3",
    contrasts = "pairwise",
    stringsAsFactors = FALSE
  ),
  contrast_row
)
# Include every other family member: testing only the first row cannot detect
# accidental scalar recycling of adjusted p-values.
other_contrasts <- contrast_rows[contrast_rows$contrast != "mid A - post A", , drop = FALSE]
contrasts_out <- rbind(contrasts_out, cbind(data.frame(
  case_id = paste0("contrast_family_", seq_len(nrow(other_contrasts))),
  formula = "score ~ time * group3 + x1 + (1|id)", emmeans = "time*group3", contrasts = "pairwise"), other_contrasts))

# diagnostics golden
diag_singular <- diagnostics_base[diagnostics_base$metric == "singular_fit", , drop = FALSE]
diag_conv <- diagnostics_base[diagnostics_base$metric == "convergence", , drop = FALSE]
diag_shapiro <- diagnostics_base[diagnostics_base$metric == "shapiro_wilk", , drop = FALSE]

diagnostics_rows <- rbind(
  cbind(
    data.frame(
      case_id = "diag_singular",
      formula = "score ~ time + group3 + x1 + (1|id)",
      stringsAsFactors = FALSE
    ),
    diag_singular
  ),
  cbind(
    data.frame(
      case_id = "diag_convergence",
      formula = "score ~ time + group3 + x1 + (1|id)",
      stringsAsFactors = FALSE
    ),
    diag_conv
  ),
  cbind(
    data.frame(
      case_id = "diag_shapiro",
      formula = "score ~ time + group3 + x1 + (1|id)",
      stringsAsFactors = FALSE
    ),
    diag_shapiro
  )
)

write.csv(fixed_rows, out_fixed, row.names = FALSE)
write.csv(random_rows, out_random, row.names = FALSE)
write.csv(fit_rows, out_fit, row.names = FALSE)
write.csv(r2_icc_rows, out_r2_icc, row.names = FALSE)
write.csv(anova_rows, out_anova, row.names = FALSE)
write.csv(emmeans_out, out_emmeans, row.names = FALSE)
write.csv(contrasts_out, out_contrasts, row.names = FALSE)
write.csv(diagnostics_rows, out_diagnostics, row.names = FALSE)

cat("Wrote:\n")
cat("-", out_fixed, "\n")
cat("-", out_random, "\n")
cat("-", out_fit, "\n")
cat("-", out_r2_icc, "\n")
cat("-", out_anova, "\n")
cat("-", out_emmeans, "\n")
cat("-", out_contrasts, "\n")
cat("-", out_diagnostics, "\n")
