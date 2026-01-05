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

coerce_model_factors <- function(df, vars, dv) {
  for (var in vars) {
    if (!var %in% names(df)) next
    if (identical(var, dv)) next
    if (is.numeric(df[[var]])) next
    df[[var]] <- as.factor(df[[var]])
  }
  df
}

get_complete_rows <- function(df) {
  if (nrow(df) == 0) return(logical(0))
  idx <- complete.cases(df)
  if (any(idx)) {
    num_cols <- names(df)[sapply(df, is.numeric)]
    if (length(num_cols) > 0) {
      finite_idx <- rep(TRUE, nrow(df))
      for (col in num_cols) {
        finite_idx <- finite_idx & is.finite(df[[col]])
      }
      idx <- idx & finite_idx
    }
  }
  idx
}

build_lmer_control <- function(optimizer, maxfun) {
  if (is.null(optimizer) || !nzchar(optimizer)) {
    if (is.null(maxfun) || is.na(maxfun)) return(lme4::lmerControl())
    return(lme4::lmerControl(optCtrl = list(maxfun = maxfun)))
  }
  if (is.null(maxfun) || is.na(maxfun)) return(lme4::lmerControl(optimizer = optimizer))
  lme4::lmerControl(optimizer = optimizer, optCtrl = list(maxfun = maxfun))
}

compute_standardized_betas <- function(data, dv, term_names, estimates, standardize) {
  if (standardize == "none") return(rep(NA_real_, length(term_names)))
  if (!dv %in% names(data)) return(rep(NA_real_, length(term_names)))
  y <- data[[dv]]
  if (!is.numeric(y)) return(rep(NA_real_, length(term_names)))
  sd_y <- sd(y)
  if (is.na(sd_y) || sd_y == 0) return(rep(NA_real_, length(term_names)))
  betas <- rep(NA_real_, length(term_names))
  for (i in seq_along(term_names)) {
    term <- term_names[i]
    if (term == "(Intercept)") next
    if (!term %in% names(data)) next
    if (!is.numeric(data[[term]])) next
    sd_x <- sd(data[[term]])
    if (is.na(sd_x) || sd_x == 0) next
    betas[i] <- estimates[i] * sd_x / sd_y
  }
  betas
}

get_coef_column <- function(mat, names) {
  for (name in names) {
    if (name %in% colnames(mat)) return(mat[, name])
  }
  NULL
}

extract_fixed_effects <- function(summary_obj, data, dv, conf_level, standardize) {
  coef_mat <- as.matrix(summary_obj$coefficients)
  if (is.null(coef_mat) || nrow(coef_mat) == 0) return(data.frame())
  term_names <- rownames(coef_mat)
  estimate <- coef_mat[, 1]
  se <- coef_mat[, 2]
  df_vals <- get_coef_column(coef_mat, c("df"))
  if (is.null(df_vals)) df_vals <- rep(NA_real_, length(estimate))
  t_vals <- get_coef_column(coef_mat, c("t value", "t", "t.value"))
  if (is.null(t_vals)) t_vals <- estimate / se
  p_vals <- get_coef_column(coef_mat, c("Pr(>|t|)", "Pr(>|z|)", "p.value", "p-value"))
  if (is.null(p_vals)) p_vals <- rep(NA_real_, length(estimate))

  ci_low <- rep(NA_real_, length(estimate))
  ci_high <- rep(NA_real_, length(estimate))
  for (i in seq_along(estimate)) {
    df_val <- df_vals[i]
    crit <- if (!is.na(df_val)) {
      qt(1 - (1 - conf_level) / 2, df_val)
    } else {
      qnorm(1 - (1 - conf_level) / 2)
    }
    ci_low[i] <- estimate[i] - crit * se[i]
    ci_high[i] <- estimate[i] + crit * se[i]
  }

  std_beta <- compute_standardized_betas(data, dv, term_names, estimate, standardize)

  data.frame(
    term = term_names,
    estimate = estimate,
    se = se,
    df = df_vals,
    t = t_vals,
    p = p_vals,
    ci_low = ci_low,
    ci_high = ci_high,
    std_beta = std_beta,
    stringsAsFactors = FALSE
  )
}

extract_random_effects <- function(fit) {
  vc <- lme4::VarCorr(fit)
  if (length(vc) == 0) return(data.frame())
  rows <- list()
  for (grp in names(vc)) {
    mat <- as.matrix(vc[[grp]])
    sd_vals <- attr(vc[[grp]], "stddev")
    terms <- rownames(mat)
    if (length(terms) == 0) next
    for (i in seq_along(terms)) {
      rows[[length(rows) + 1]] <- data.frame(
        group = grp,
        term = terms[i],
        variance = sd_vals[i]^2,
        stddev = sd_vals[i],
        corr = NA_real_,
        stringsAsFactors = FALSE
      )
    }
    corr <- attr(vc[[grp]], "correlation")
    if (!is.null(corr) && nrow(corr) > 1) {
      for (i in seq_len(nrow(corr) - 1)) {
        for (j in (i + 1):nrow(corr)) {
          rows[[length(rows) + 1]] <- data.frame(
            group = grp,
            term = paste0("corr(", terms[i], ",", terms[j], ")"),
            variance = NA_real_,
            stddev = NA_real_,
            corr = corr[i, j],
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

extract_fit_stats <- function(fit) {
  data.frame(
    n = tryCatch(nobs(fit), error = function(e) NA_real_),
    aic = suppressWarnings(AIC(fit)),
    bic = suppressWarnings(BIC(fit)),
    logLik = suppressWarnings(as.numeric(logLik(fit))),
    deviance = suppressWarnings(deviance(fit)),
    stringsAsFactors = FALSE
  )
}

extract_r2_df <- function(fit) {
  res <- tryCatch(performance::r2(fit), error = function(e) NULL)
  if (is.null(res)) return(data.frame())
  data.frame(
    r2_marginal = res$R2_marginal,
    r2_conditional = res$R2_conditional,
    stringsAsFactors = FALSE
  )
}

resolve_icc_value <- function(res) {
  if (!(is.data.frame(res) || is.list(res))) return(NA_real_)
  keys <- names(res)
  if (is.null(keys) || length(keys) == 0) return(NA_real_)
  preferred <- c("ICC", "ICC_adjusted", "ICC_unadjusted", "ICC_conditional", "ICC_marginal")
  for (key in preferred) {
    if (key %in% keys) {
      val <- res[[key]]
      if (length(val) > 0) return(as.numeric(val[1]))
    }
  }
  for (key in keys) {
    if (grepl("^ICC", key, ignore.case = TRUE)) {
      val <- res[[key]]
      if (length(val) > 0) return(as.numeric(val[1]))
    }
  }
  for (key in keys) {
    val <- suppressWarnings(as.numeric(res[[key]]))
    if (length(val) > 0 && !all(is.na(val))) return(val[1])
  }
  NA_real_
}

extract_icc_df <- function(fit) {
  res <- tryCatch(performance::icc(fit), error = function(e) NULL)
  if (is.null(res)) return(data.frame())
  icc_val <- resolve_icc_value(res)
  if (is.na(icc_val)) return(data.frame())
  data.frame(
    icc = icc_val,
    stringsAsFactors = FALSE
  )
}

build_anova_df <- function(fit, type, df_method) {
  used_type <- type
  fallback_used <- FALSE
  out <- NULL
  if (type == "I") {
    out <- tryCatch(stats::anova(fit), error = function(e) NULL)
  } else {
    type_val <- ifelse(type == "III", 3, 2)
    ddf_label <- if (df_method == "kenward-roger") "Kenward-Roger" else "Satterthwaite"
    out <- tryCatch(stats::anova(fit, type = type_val, ddf = ddf_label), error = function(e) NULL)
  }
  if (is.null(out)) {
    out <- tryCatch(stats::anova(fit), error = function(e) NULL)
    if (!is.null(out) && type != "I") {
      fallback_used <- TRUE
      used_type <- "I"
    }
  }
  if (is.null(out)) return(data.frame())
  df <- as.data.frame(out)
  df$term <- rownames(df)
  rownames(df) <- NULL
  attr(df, "type_used") <- used_type
  attr(df, "fallback_used") <- fallback_used
  df
}

build_diagnostics <- function(fit, max_shapiro_n) {
  rows <- list()
  is_singular <- tryCatch(lme4::isSingular(fit), error = function(e) NA)
  rows[[length(rows) + 1]] <- data.frame(
    metric = "singular_fit",
    value = ifelse(is.na(is_singular), "", ifelse(isTRUE(is_singular), "TRUE", "FALSE")),
    statistic = NA_real_,
    p = NA_real_,
    note = "",
    stringsAsFactors = FALSE
  )

  conv_note <- ""
  conv_msgs <- tryCatch(fit@optinfo$conv$lme4$messages, error = function(e) NULL)
  if (!is.null(conv_msgs) && length(conv_msgs) > 0) {
    conv_note <- paste(conv_msgs, collapse = "; ")
  }
  rows[[length(rows) + 1]] <- data.frame(
    metric = "convergence",
    value = ifelse(nzchar(conv_note), "warning", "ok"),
    statistic = NA_real_,
    p = NA_real_,
    note = conv_note,
    stringsAsFactors = FALSE
  )

  resid_vals <- tryCatch(residuals(fit), error = function(e) NULL)
  if (!is.null(resid_vals)) {
    n <- length(resid_vals)
    if (n > 2 && n <= max_shapiro_n) {
      shap <- tryCatch(shapiro.test(resid_vals), error = function(e) NULL)
      if (!is.null(shap)) {
        rows[[length(rows) + 1]] <- data.frame(
          metric = "shapiro_wilk",
          value = "",
          statistic = unname(shap$statistic),
          p = shap$p.value,
          note = "",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  do.call(rbind, rows)
}

build_emmeans_rows <- function(emm_summary, term_label) {
  base_cols <- c("emmean", "SE", "df", "lower.CL", "upper.CL", "t.ratio", "p.value")
  factor_cols <- setdiff(names(emm_summary), base_cols)
  level <- ""
  if (length(factor_cols) > 0) {
    level <- apply(emm_summary[, factor_cols, drop = FALSE], 1, function(row) {
      paste(paste0(factor_cols, "=", row), collapse = ", ")
    })
  }
  data.frame(
    term = term_label,
    level = level,
    contrast = "",
    emmean = emm_summary$emmean,
    estimate = NA_real_,
    se = emm_summary$SE,
    df = emm_summary$df,
    t = emm_summary$t.ratio,
    p = emm_summary$p.value,
    p_adj = NA_real_,
    ci_low = emm_summary$lower.CL,
    ci_high = emm_summary$upper.CL,
    method = "emmeans",
    stringsAsFactors = FALSE
  )
}

build_contrasts_rows <- function(contrast_summary, term_label, p_adjust, method_label) {
  p_adj_vals <- ifelse(p_adjust != "none", contrast_summary$p.value, NA_real_)
  p_vals <- ifelse(p_adjust == "none", contrast_summary$p.value, NA_real_)
  method <- if (!is.null(method_label) && nzchar(method_label)) method_label else p_adjust
  data.frame(
    term = term_label,
    level = "",
    contrast = contrast_summary$contrast,
    emmean = NA_real_,
    estimate = contrast_summary$estimate,
    se = contrast_summary$SE,
    df = contrast_summary$df,
    t = contrast_summary$t.ratio,
    p = p_vals,
    p_adj = p_adj_vals,
    ci_low = contrast_summary$lower.CL,
    ci_high = contrast_summary$upper.CL,
    method = method,
    stringsAsFactors = FALSE
  )
}

prepare_long_data <- function(path) {
  raw <- read.csv(path, stringsAsFactors = FALSE)
  long_df <- reshape(
    raw,
    varying = c("pre_score", "mid_score", "post_score"),
    v.names = "score",
    timevar = "time",
    times = c("pre", "mid", "post"),
    idvar = "id",
    direction = "long"
  )
  long_df <- long_df[!is.na(long_df$score), ]
  long_df <- long_df[, c("id", "time", "score", "group3", "x1"), drop = FALSE]
  long_df
}

build_model_data <- function(df, formula, dv) {
  vars <- all.vars(formula)
  df <- coerce_model_factors(df, vars, dv)
  complete_idx <- get_complete_rows(df[, vars, drop = FALSE])
  data_model <- df[complete_idx, , drop = FALSE]
  droplevels(data_model)
}

long_df <- prepare_long_data(data_path)
control <- build_lmer_control("bobyqa", 100000)
control_std <- build_lmer_control("bobyqa", 20000)

# Base model
base_formula <- as.formula("score ~ time + group3 + x1 + (1|id)")
base_data <- build_model_data(long_df, base_formula, "score")
fit_base <- lmerTest::lmer(base_formula, data = base_data, REML = TRUE, control = control)
summary_base <- summary(fit_base, ddf = "Satterthwaite")
fixed_base <- extract_fixed_effects(summary_base, base_data, "score", 0.95, "none")
random_base <- extract_random_effects(fit_base)
fit_base_stats <- extract_fit_stats(fit_base)
r2_base <- extract_r2_df(fit_base)
icc_base <- extract_icc_df(fit_base)
anova_base <- build_anova_df(fit_base, "III", "satterthwaite")
diagnostics_base <- build_diagnostics(fit_base, 100000)

# Standardized predictors model
std_formula <- base_formula
std_data <- base_data
fit_std <- lmerTest::lmer(std_formula, data = std_data, REML = FALSE, control = control_std)
summary_std <- summary(fit_std, ddf = "Satterthwaite")
fixed_std <- extract_fixed_effects(summary_std, std_data, "score", 0.95, "predictors")

# emmeans/contrasts model
emm_formula <- as.formula("score ~ time * group3 + x1 + (1|id)")
emm_data <- build_model_data(long_df, emm_formula, "score")
fit_emm <- lmerTest::lmer(emm_formula, data = emm_data, REML = TRUE, control = control)
emm <- emmeans::emmeans(fit_emm, specs = ~ time * group3)
emm_summary <- summary(emm, infer = c(TRUE, TRUE), level = 0.9)
emmeans_rows <- build_emmeans_rows(emm_summary, "time*group3")
cont <- emmeans::contrast(emm, method = "pairwise")
cont_summary <- summary(cont, infer = c(TRUE, TRUE), adjust = "holm", level = 0.9)
contrast_rows <- build_contrasts_rows(cont_summary, "time*group3", "holm", "pairwise")

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
