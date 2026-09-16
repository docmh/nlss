# SPDX-License-Identifier: Apache-2.0
#!/usr/bin/env Rscript

bootstrap_dir <- {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", cmd_args[grep("^--file=", cmd_args)])
  if (length(file_arg) > 0 && nzchar(file_arg[1])) {
    dirname(normalizePath(file_arg[1], winslash = "/", mustWork = FALSE))
  } else {
    getwd()
  }
}
source(file.path(bootstrap_dir, "lib", "bootstrap.R"))
nlss_bootstrap()
source_lib("contrast_utils.R")

print_usage <- function() {
  cat("Mixed Models (lme4)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript mixed_models.R --csv data.csv --formula \"score ~ time + (1|id)\"\n")
  cat("  Rscript mixed_models.R --csv data.csv --dv score --fixed time,group --random \"1|id,time|id\"\n")
  cat("  Rscript mixed_models.R --csv data.csv --formula \"score ~ time*group + (1|id)\" --emmeans time*group --contrasts pairwise\n")
  cat("  Rscript mixed_models.R --parquet data.parquet --formula \"score ~ time + (1|id)\"\n")
  cat("  Rscript mixed_models.R --interactive\n")
  cat("\n")
  cat("Options:\n")
  cat("  --csv PATH             CSV input file\n")
  cat("  --sav PATH             SPSS .sav input file\n")
  cat("  --sep VALUE            CSV separator (default: ,)\n")
  cat("  --header TRUE/FALSE    CSV header (default: TRUE)\n")
  cat("  --rds PATH             RDS input file (data frame)\n")
  cat("  --rdata PATH           RData input file\n")
  cat("  --parquet PATH         Parquet input file\n")
  cat("  --df NAME              Data frame object name in RData\n")
  cat("  --formula TEXT         Full lme4 formula (overrides --dv/--fixed/--random)\n")
  cat("  --dv NAME              Dependent variable (required without --formula)\n")
  cat("  --fixed LIST           Comma-separated fixed effects\n")
  cat("  --random LIST          Comma-separated random terms (e.g., 1|id,time|id)\n")
  cat("  --reml TRUE/FALSE      REML estimation (default: TRUE)\n")
  cat("  --type TYPE            Type I/II/III tests (default: III)\n")
  cat("  --df-method METHOD     satterthwaite/kenward-roger/none (default: satterthwaite)\n")
  cat("  --standardize TYPE     none/predictors (default: none)\n")
  cat("  --emmeans TERM         Term for marginal means (default: none)\n")
  cat("  --contrasts TYPE       none/pairwise/custom/<method> (default: none)\n")
  cat("  --contrast-file PATH   JSON contrast spec for custom or method args\n")
  cat("  --p-adjust METHOD      P-value adjustment (default: holm)\n")
  cat("  --conf-level VALUE     Confidence level (default: 0.95)\n")
  cat("  --optimizer NAME       Optimizer (default: bobyqa)\n")
  cat("  --seed N               Seed for stochastic emmeans adjustments (canonical default)\n")
  cat("  --maxfun N             Optimizer maxfun (default: 100000)\n")
  cat("  --diagnostics TRUE/FALSE Diagnostics (default: TRUE)\n")
  cat("  --max-shapiro-n N      Max n for Shapiro test (default: 5000)\n")
  cat("  --digits N             Rounding digits (default: 2)\n")
  cat("  --template REF         Template path or template key (optional)\n")
  cat("  --user-prompt TEXT     Original AI user prompt for logging (optional)\n")
  cat("  --log TRUE/FALSE       Write analysis_log.jsonl (default: TRUE)\n")
  cat("  --interactive          Prompt for inputs\n")
  cat("  --help                 Show this help\n")
}

interactive_options <- function() {
  cat("Interactive input selected.\n")
  input_type <- prompt("Input type (csv/sav/rds/rdata/parquet)", "csv")
  input_type <- tolower(input_type)
  opts <- list()

  if (input_type == "csv") {
    opts$csv <- prompt("CSV path")
    sep_default <- get_config_value("defaults.csv.sep")
    header_default <- get_config_value("defaults.csv.header")
    opts$sep <- prompt("Separator", sep_default)
    opts$header <- prompt("Header TRUE/FALSE", ifelse(isTRUE(header_default), "TRUE", "FALSE"))
  } else if (input_type == "sav") {
    opts$sav <- prompt("SAV path")
  } else if (input_type == "rds") {
    opts$rds <- prompt("RDS path")
  } else if (input_type == "rdata") {
    opts$rdata <- prompt("RData path")
    opts$df <- prompt("Data frame object name")
  } else if (input_type == "parquet") {
    opts$parquet <- prompt("Parquet path")
  } else {
    stop("Unsupported input type.")
  }

  use_formula <- prompt("Use full formula? (yes/no)", "yes")
  if (tolower(use_formula) %in% c("yes", "y")) {
    opts$formula <- prompt("Model formula (e.g., score ~ time + (1|id))", "")
  } else {
    opts$dv <- prompt("Dependent variable", "")
    opts$fixed <- prompt("Fixed effects (comma-separated)", "")
    opts$random <- prompt("Random terms (comma-separated; e.g., 1|id,time|id)", "")
  }

  reml_default <- get_config_value("modules.mixed_models.reml")
  type_default <- get_config_value("modules.mixed_models.type")
  df_method_default <- get_config_value("modules.mixed_models.df_method")
  standardize_default <- get_config_value("modules.mixed_models.standardize")
  emmeans_default <- get_config_value("modules.mixed_models.emmeans")
  contrasts_default <- get_config_value("modules.mixed_models.contrasts")
  p_adjust_default <- get_config_value("modules.mixed_models.p_adjust")
  conf_default <- get_config_value("modules.mixed_models.conf_level")
  optimizer_default <- get_config_value("modules.mixed_models.optimizer")
  maxfun_default <- get_config_value("modules.mixed_models.maxfun")
  diagnostics_default <- get_config_value("modules.mixed_models.diagnostics")
  max_shapiro_n_default <- get_config_value("modules.mixed_models.max_shapiro_n")
  digits_default <- get_config_value("defaults.digits")

  opts$reml <- prompt("REML TRUE/FALSE", ifelse(isTRUE(reml_default), "TRUE", "FALSE"))
  opts$type <- prompt("Type (I/II/III)", type_default)
  opts$`df-method` <- prompt("DF method (satterthwaite/kenward-roger/none)", df_method_default)
  opts$standardize <- prompt("Standardize (none/predictors)", standardize_default)
  opts$emmeans <- prompt("Marginal means term (none or term)", emmeans_default)
  opts$contrasts <- prompt("Contrasts (none/pairwise/custom/<method>)", contrasts_default)
  contrast_mode <- normalize_contrasts(opts$contrasts, contrasts_default)
  if (contrast_mode == "custom") {
    opts$`contrast-file` <- prompt("Contrast JSON file", "")
  }
  opts$`p-adjust` <- prompt("P-value adjustment", p_adjust_default)
  opts$`conf-level` <- prompt("Confidence level", as.character(conf_default))
  opts$optimizer <- prompt("Optimizer", optimizer_default)
  opts$maxfun <- prompt("Optimizer maxfun", as.character(maxfun_default))
  opts$diagnostics <- prompt("Diagnostics TRUE/FALSE", ifelse(isTRUE(diagnostics_default), "TRUE", "FALSE"))
  opts$`max-shapiro-n` <- prompt("Max Shapiro n", as.character(max_shapiro_n_default))
  opts$digits <- prompt("Rounding digits", as.character(digits_default))
  opts$template <- prompt("Template (path or key; blank for default)", "")
  opts$`user-prompt` <- prompt("User prompt (optional)", "")
  log_default <- get_config_value("defaults.log")
  opts$log <- prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))
  opts
}

format_num <- function(value, digits) {
  if (is.na(value)) return("")
  format(round(value, digits), nsmall = digits, trim = TRUE)
}

format_stat <- function(value, digits) {
  if (is.na(value)) return("")
  txt <- format(round(value, digits), nsmall = digits, trim = TRUE)
  sub("^(-?)0", "\\1", txt)
}

format_p <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value < 0.001) return("< .001")
  p_txt <- format(round(p_value, 3), nsmall = 3, trim = TRUE)
  p_txt <- sub("^0", "", p_txt)
  paste("=", p_txt)
}

format_ci <- function(low, high, digits) {
  if (is.na(low) || is.na(high)) return("")
  paste0("[", format_stat(low, digits), ", ", format_stat(high, digits), "]")
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

normalize_reml <- function(value, default = TRUE) {
  parse_bool(value, default = default)
}

normalize_standardize <- function(value, default = "none") {
  val <- tolower(if (is.null(value) || value == "") default else value)
  if (val %in% c("none", "no", "false")) return("none")
  if (val %in% c("predictors", "pred", "x")) return("predictors")
  stop("standardize must be none or predictors.")
}

normalize_type <- function(value, default = "III") {
  val <- if (!is.null(value) && value != "") value else default
  val <- toupper(gsub("[^A-Za-z0-9]", "", val))
  if (val %in% c("1", "I")) return("I")
  if (val %in% c("2", "II")) return("II")
  if (val %in% c("3", "III")) return("III")
  stop("type must be I, II or III.")
}

normalize_df_method <- function(value, default = "satterthwaite") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(gsub("[^a-z-]", "", val))
  if (val %in% c("satterthwaite", "satter")) return("satterthwaite")
  if (val %in% c("kenward-roger", "kenwardroger", "kr")) return("kenward-roger")
  if (val %in% c("none", "no", "false")) return("none")
  stop("df-method must be satterthwaite, kenward-roger or none.")
}

normalize_contrasts <- function(value, default = "none") normalize_contrast_mode(value, default)

normalize_emmeans <- function(value, default = "none") {
  val <- if (!is.null(value) && value != "") value else default
  val <- trimws(as.character(val))
  if (!nzchar(val) || tolower(val) %in% c("none", "no", "false")) return("")
  val
}

normalize_conf_level <- function(value, default = 0.95) {
  if (is.null(value) || value == "") value <- default
  val <- suppressWarnings(as.numeric(value))
  if (length(val) != 1L || !is.finite(val) || val <= 0 || val >= 1) stop("conf-level must be between 0 and 1.")
  val
}

normalize_maxfun <- function(value, default = 100000) {
  if (is.null(value) || value == "") value <- default
  val <- suppressWarnings(as.numeric(value))
  if (length(val) != 1L || !is.finite(val) || val <= 0 || val != floor(val) || val > .Machine$integer.max) stop("maxfun must be a positive R integer.")
  as.integer(val)
}

normalize_random_terms <- function(terms) {
  out <- character(0)
  for (term in terms) {
    term <- trimws(term)
    if (!nzchar(term)) next
    if (grepl("\\|", term)) {
      if (!grepl("^\\(.*\\)$", term)) {
        term <- paste0("(", term, ")")
      }
      out <- c(out, term)
    } else {
      out <- c(out, paste0("(1|", term, ")"))
    }
  }
  out
}

extract_random_terms_from_formula <- function(formula_text) {
  vapply(reformulas::findbars(as.formula(formula_text)), function(x)
    paste0("(", gsub(" *\\| *", "|", paste(deparse(x), collapse = " ")), ")"), character(1))
}

build_model_formula <- function(dv, fixed_terms, random_terms) {
  fixed_term <- if (length(fixed_terms) > 0) paste(fixed_terms, collapse = " + ") else "1"
  random_term <- if (length(random_terms) > 0) paste(random_terms, collapse = " + ") else ""
  rhs <- if (nzchar(random_term)) paste(fixed_term, "+", random_term) else fixed_term
  as.formula(paste(dv, "~", rhs))
}

coerce_model_factors <- function(df, vars, dv) {
  for (var in vars) {
    if (!var %in% names(df)) next
    if (identical(var, dv)) next
    if (is.numeric(df[[var]])) next
    df[[var]] <- as.factor(df[[var]])
  }
  df
}

build_lmer_control <- function(optimizer, maxfun) {
  option <- if (optimizer == "nloptwrap") "maxeval" else if (optimizer %in% c("optim", "optimx")) "maxit" else "maxfun"
  lme4::lmerControl(optimizer = optimizer, optCtrl = setNames(list(maxfun), option))
}

get_coef_column <- function(mat, names) {
  for (name in names) {
    if (name %in% colnames(mat)) return(mat[, name])
  }
  NULL
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

extract_fixed_effects <- function(summary_obj, data, dv, conf_level, standardize) {
  coef_mat <- as.matrix(summary_obj$coefficients)
  if (is.null(coef_mat) || nrow(coef_mat) == 0) {
    return(data.frame())
  }
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
  rows[[length(rows) + 1L]] <- data.frame(group = "Residual", term = "Residual",
    variance = sigma(fit)^2, stddev = sigma(fit), corr = NA_real_)
  do.call(rbind, rows)
}

extract_fit_stats <- function(fit) {
  data.frame(
    n = tryCatch(nobs(fit), error = function(e) NA_real_),
    aic = suppressWarnings(AIC(fit)),
    bic = suppressWarnings(BIC(fit)),
    logLik = suppressWarnings(as.numeric(logLik(fit))),
    deviance = suppressWarnings(deviance(fit)),
    criterion = if (lme4::isREML(fit)) "REML criterion (legacy deviance field)" else "ML deviance",
    reml = lme4::isREML(fit),
    stringsAsFactors = FALSE
  )
}

extract_r2_df <- function(fit) {
  res <- tryCatch(performance::r2(fit), error = function(e) { warning("R-squared unavailable: ", conditionMessage(e)); NULL })
  val <- function(key) if (is.list(res) && length(res[[key]])) as.numeric(res[[key]][1]) else NA_real_
  out <- data.frame(r2_marginal = val("R2_marginal"), r2_conditional = val("R2_conditional"))
  out$status <- ifelse(is.finite(out$r2_marginal) & is.finite(out$r2_conditional), "available", "unavailable_or_partial")
  out$reason <- if (out$status == "available") "" else "performance did not return both estimable R-squared components (for example, singular random effects)."
  out
}

extract_icc_df <- function(fit) {
  res <- tryCatch(performance::icc(fit), error = function(e) { warning("ICC unavailable: ", conditionMessage(e)); NULL })
  val <- function(key) if (is.list(res) && length(res[[key]])) as.numeric(res[[key]][1]) else NA_real_
  adjusted <- val("ICC_adjusted")
  data.frame(icc = adjusted, icc_adjusted = adjusted, icc_unadjusted = val("ICC_unadjusted"),
    status = if (is.finite(adjusted)) "available" else "unavailable",
    reason = if (is.finite(adjusted)) "" else "performance did not return an estimable adjusted ICC (for example, singular random effects).")
}

build_anova_df <- function(fit, type, df_method, has_lmerTest) {
  if (df_method != "none") {
    ddf <- if (df_method == "kenward-roger") "Kenward-Roger" else "Satterthwaite"
    out <- stats::anova(fit, type = match(type, c("I", "II", "III")), ddf = ddf)
    method <- paste("lmerTest F", ddf)
  } else if (type == "I") {
    out <- stats::anova(fit)
    method <- "lme4 sequential F without denominator df or p-values"
  } else {
    out <- car::Anova(fit, type = type, test.statistic = "Chisq")
    method <- "car Wald chi-squared"
  }
  df <- as.data.frame(out)
  df$term <- rownames(df)
  rownames(df) <- NULL
  attr(df, "type_used") <- type
  attr(df, "method") <- method
  df
}

build_diagnostics <- function(fit, max_shapiro_n, residual_checks = TRUE) {
  singular <- lme4::isSingular(fit)
  opt_code <- suppressWarnings(as.numeric(unlist(fit@optinfo$conv$opt)))
  code_available <- length(opt_code) > 0L && all(is.finite(opt_code))
  messages <- unique(c(unlist(fit@optinfo$conv$lme4$messages), unlist(fit@optinfo$warnings)))
  # lme4 also sends singular-boundary messages through its convergence channel:
  # retain them, but distinguish that diagnosis from optimizer convergence.
  convergence_messages <- messages[!grepl("boundary .*singular", messages, ignore.case = TRUE)]
  conv_note <- paste(c(if (!code_available) "Optimizer convergence code unavailable." else
      if (any(opt_code != 0)) paste("Optimizer convergence code:", paste(opt_code, collapse = ", ")),
    convergence_messages), collapse = "; ")
  rows <- data.frame(metric = c("singular_fit", "convergence"), value = c(as.character(singular),
    if (!code_available) "unavailable" else if (nzchar(conv_note)) "warning" else "ok"), statistic = NA_real_, p = NA_real_,
    note = c(if (singular) "Random-effects covariance is on or near its boundary; inferential reliability needs review." else "", conv_note))
  if (residual_checks) {
    residual <- residuals(fit)
    eligible <- length(residual) >= 3L && length(residual) <= min(max_shapiro_n, 5000L) && length(unique(residual)) > 1L
    test <- if (eligible) tryCatch(shapiro.test(residual), error = function(e) NULL) else NULL
    rows <- rbind(rows, data.frame(metric = "shapiro_wilk", value = if (is.null(test)) "unavailable" else "available",
      statistic = if (is.null(test)) NA_real_ else unname(test$statistic),
      p = if (is.null(test)) NA_real_ else test$p.value,
      note = if (is.null(test)) "Shapiro-Wilk requires 3 to min(max-shapiro-n, 5000) nonconstant residuals." else
        "Conditional residual normality screen; does not test random-effect normality or independent residuals."))
  }
  rows
}

build_fixed_effects_table_body <- function(fixed_df, digits, table_meta) {
  display <- fixed_df
  display$term_display <- if ("term_label" %in% names(display)) display$term_label else display$term
  default_specs <- list(
    list(key = "model", label = "Model", drop_if_empty = TRUE),
    list(key = "term", label = "Effect"),
    list(key = "b", label = "b"),
    list(key = "se", label = "SE"),
    list(key = "df", label = "df", drop_if_empty = TRUE),
    list(key = "t", label = "t", drop_if_empty = TRUE),
    list(key = "p", label = "p", drop_if_empty = TRUE),
    list(key = "ci_low", label = "CI low", drop_if_empty = TRUE),
    list(key = "ci_high", label = "CI high", drop_if_empty = TRUE),
    list(key = "std_beta", label = "beta", drop_if_empty = TRUE)
  )
  columns <- normalize_table_columns(table_meta$columns, default_specs)
  show_model <- length(unique(display$model)) > 1
  rows <- list()
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    row_map <- list(
      model = if (show_model) row$model else "",
      term = row$term_display,
      b = format_stat(row$estimate, digits),
      se = format_stat(row$se, digits),
      df = format_stat(row$df, digits),
      t = format_stat(row$t, digits),
      p = format_p(row$p),
      ci_low = format_stat(row$ci_low, digits),
      ci_high = format_stat(row$ci_high, digits),
      std_beta = format_stat(row$std_beta, digits)
    )
    row_vals <- vapply(columns, function(col) {
      as_cell_text(row_map[[col$key]])
    }, character(1))
    rows[[length(rows) + 1]] <- row_vals
  }
  drop_result <- drop_empty_columns(columns, rows)
  columns <- drop_result$columns
  rows <- drop_result$rows
  headers <- vapply(columns, function(col) col$label, character(1))
  body <- render_markdown_table(headers, rows)
  list(body = body, columns = columns)
}

resolve_anova_column <- function(df, keys) {
  for (key in keys) {
    if (key %in% names(df)) return(key)
  }
  NULL
}

resolve_anova_columns <- function(df) {
  list(
    num_df = resolve_anova_column(df, c("NumDF", "numDF")),
    den_df = resolve_anova_column(df, c("DenDF", "denDF")),
    df = resolve_anova_column(df, c("Df", "df")),
    f = resolve_anova_column(df, c("F value", "F.value", "F", "F-value", "Fvalue")),
    chisq = resolve_anova_column(df, c("Chisq", "ChiSq", "Chisq value", "Chi.sq", "chisq")),
    p = resolve_anova_column(df, c("Pr(>F)", "Pr(>Chisq)", "Pr(>Chi)", "p.value", "p"))
  )
}

build_mixed_anova_table_body <- function(anova_df, digits, table_meta) {
  display <- anova_df
  if (!"term" %in% names(display)) return(list(body = "", columns = list()))
  display <- display[display$term != "(Intercept)", , drop = FALSE]
  if (nrow(display) == 0) return(list(body = "", columns = list()))

  cols <- resolve_anova_columns(display)
  default_specs <- list(
    list(key = "term", label = "Effect"),
    list(key = "num_df", label = "Num df", drop_if_empty = TRUE),
    list(key = "den_df", label = "Den df", drop_if_empty = TRUE),
    list(key = "df", label = "df", drop_if_empty = TRUE),
    list(key = "f", label = "F", drop_if_empty = TRUE),
    list(key = "chisq", label = "Chi²", drop_if_empty = TRUE),
    list(key = "p", label = "p", drop_if_empty = TRUE)
  )
  columns <- normalize_table_columns(table_meta$columns, default_specs)
  rows <- list()
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    num_df_val <- if (!is.null(cols$num_df)) row[[cols$num_df]] else NA_real_
    den_df_val <- if (!is.null(cols$den_df)) row[[cols$den_df]] else NA_real_
    df_val <- if (is.null(cols$num_df) && is.null(cols$den_df) && !is.null(cols$df)) row[[cols$df]] else NA_real_
    row_map <- list(
      term = row$term,
      num_df = format_num(num_df_val, digits),
      den_df = format_num(den_df_val, digits),
      df = format_num(df_val, digits),
      f = format_stat(if (!is.null(cols$f)) row[[cols$f]] else NA_real_, digits),
      chisq = format_stat(if (!is.null(cols$chisq)) row[[cols$chisq]] else NA_real_, digits),
      p = format_p(if (!is.null(cols$p)) row[[cols$p]] else NA_real_)
    )
    row_vals <- vapply(columns, function(col) {
      as_cell_text(row_map[[col$key]])
    }, character(1))
    rows[[length(rows) + 1]] <- row_vals
  }
  drop_result <- drop_empty_columns(columns, rows)
  columns <- drop_result$columns
  rows <- drop_result$rows
  headers <- vapply(columns, function(col) col$label, character(1))
  body <- render_markdown_table(headers, rows)
  list(body = body, columns = columns)
}

build_mixed_anova_narrative_rows <- function(anova_df, digits) {
  display <- anova_df
  if (!"term" %in% names(display)) return(list())
  display <- display[display$term != "(Intercept)", , drop = FALSE]
  if (nrow(display) == 0) return(list())

  cols <- resolve_anova_columns(display)
  rows <- list()
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    term <- row$term
    num_df_val <- if (!is.null(cols$num_df)) row[[cols$num_df]] else NA_real_
    den_df_val <- if (!is.null(cols$den_df)) row[[cols$den_df]] else NA_real_
    df_val <- if (!is.null(cols$df)) row[[cols$df]] else NA_real_
    f_val <- if (!is.null(cols$f)) row[[cols$f]] else NA_real_
    chisq_val <- if (!is.null(cols$chisq)) row[[cols$chisq]] else NA_real_
    p_val <- if (!is.null(cols$p)) row[[cols$p]] else NA_real_

    sentence <- ""
    if (!is.na(f_val)) {
      if (!is.na(num_df_val) && !is.na(den_df_val)) {
        sentence <- sprintf(
          "%s: F(%s, %s) = %s, p %s.",
          term,
          format_num(num_df_val, digits),
          format_num(den_df_val, digits),
          format_stat(f_val, digits),
          format_p(p_val)
        )
      } else {
        sentence <- sprintf(
          "%s: F = %s, p %s.",
          term,
          format_stat(f_val, digits),
          format_p(p_val)
        )
      }
    } else if (!is.na(chisq_val)) {
      df_text <- if (!is.na(df_val)) paste0("(", format_num(df_val, digits), ")") else ""
      sentence <- sprintf(
        "%s: Chi²%s = %s, p %s.",
        term,
        df_text,
        format_stat(chisq_val, digits),
        format_p(p_val)
      )
    }
    rows[[length(rows) + 1]] <- list(
      full_sentence = sentence,
      term = term,
      num_df = format_num(num_df_val, digits),
      den_df = format_num(den_df_val, digits),
      df = format_num(df_val, digits),
      f = format_stat(f_val, digits),
      chisq = format_stat(chisq_val, digits),
      p = format_p(p_val)
    )
  }
  rows
}

build_mixed_models_anova_note_tokens <- function(type, df_method_used, method) {
  list(note_default = paste0("Type ", type, " tests of fixed effects. Method: ", method,
    ". Factor coding is preserved in the request; Type III hypotheses must be interpreted with that coding.",
    if (df_method_used == "none" && type == "I") " No denominator df or p-values are supplied by this sequential lme4 table." else ""))
}

build_emmeans_table_body <- function(emmeans_df, digits, table_meta) {
  display <- emmeans_df
  display$term_display <- if ("term_label" %in% names(display)) display$term_label else display$term
  display$level_display <- if ("level_label" %in% names(display)) display$level_label else display$level
  default_specs <- list(
    list(key = "term", label = "Term"),
    list(key = "level", label = "Level", drop_if_empty = TRUE),
    list(key = "contrast", label = "Contrast", drop_if_empty = TRUE),
    list(key = "emmean", label = "EMM", drop_if_empty = TRUE),
    list(key = "estimate", label = "Estimate", drop_if_empty = TRUE),
    list(key = "se", label = "SE"),
    list(key = "df", label = "df", drop_if_empty = TRUE),
    list(key = "t", label = "t", drop_if_empty = TRUE),
    list(key = "p", label = "p", drop_if_empty = TRUE),
    list(key = "p_adj", label = "p_adj", drop_if_empty = TRUE),
    list(key = "ci_low", label = "CI low", drop_if_empty = TRUE),
    list(key = "ci_high", label = "CI high", drop_if_empty = TRUE),
    list(key = "method", label = "Method", drop_if_empty = TRUE),
    list(key = "status", label = "Status", drop_if_empty = TRUE)
  )
  columns <- normalize_table_columns(table_meta$columns, default_specs)
  rows <- list()
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    row_map <- list(
      term = row$term_display,
      level = row$level_display,
      contrast = row$contrast,
      emmean = format_stat(row$emmean, digits),
      estimate = format_stat(row$estimate, digits),
      se = format_stat(row$se, digits),
      df = format_stat(row$df, digits),
      t = format_stat(row$t, digits),
      p = format_p(row$p),
      p_adj = format_p(row$p_adj),
      ci_low = format_stat(row$ci_low, digits),
      ci_high = format_stat(row$ci_high, digits),
      method = row$method,
      status = row$status
    )
    row_vals <- vapply(columns, function(col) {
      as_cell_text(row_map[[col$key]])
    }, character(1))
    rows[[length(rows) + 1]] <- row_vals
  }
  drop_result <- drop_empty_columns(columns, rows)
  columns <- drop_result$columns
  rows <- drop_result$rows
  headers <- vapply(columns, function(col) col$label, character(1))
  body <- render_markdown_table(headers, rows)
  list(body = body, columns = columns)
}

build_fixed_effects_narrative_rows <- function(fixed_df, digits) {
  display <- fixed_df
  display$term_display <- if ("term_label" %in% names(display)) display$term_label else display$term
  rows <- list()
  if (nrow(display) == 0) return(rows)
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    if (row$term == "(Intercept)" || !is.finite(row$estimate)) next
    term_label <- row$term_display
    b_text <- format_stat(row$estimate, digits)
    se_text <- format_stat(row$se, digits)
    t_text <- format_stat(row$t, digits)
    df_text <- format_stat(row$df, digits)
    p_text <- format_p(row$p)
    ci_text <- format_ci(row$ci_low, row$ci_high, digits)
    sentence <- sprintf("%s: b = %s, SE = %s", term_label, b_text, se_text)
    if (nzchar(t_text)) {
      if (nzchar(df_text)) {
        sentence <- paste0(sentence, ", t(", df_text, ") = ", t_text)
      } else {
        sentence <- paste0(sentence, ", t = ", t_text)
      }
    }
    if (nzchar(p_text)) {
      sentence <- paste0(sentence, ", p ", p_text)
    }
    if (nzchar(ci_text)) {
      sentence <- paste0(sentence, ", CI ", ci_text)
    }
    sentence <- paste0(sentence, ".")
    rows[[length(rows) + 1]] <- list(
      full_sentence = sentence,
      term = term_label,
      b = b_text,
      se = se_text,
      df = df_text,
      t = t_text,
      p = p_text,
      ci = ci_text,
      ci_low = format_stat(row$ci_low, digits),
      ci_high = format_stat(row$ci_high, digits),
      std_beta = format_stat(row$std_beta, digits)
    )
  }
  rows
}

build_emmeans_narrative_rows <- function(emmeans_df, digits) {
  display <- emmeans_df
  display$term_display <- if ("term_label" %in% names(display)) display$term_label else display$term
  display$level_display <- if ("level_label" %in% names(display)) display$level_label else display$level
  rows <- list()
  if (nrow(display) == 0) return(rows)
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    label <- if (nzchar(row$contrast)) row$contrast else row$level_display
    if (!nzchar(label)) label <- row$term_display
    estimate <- if (!is.na(row$emmean)) row$emmean else row$estimate
    est_text <- format_stat(estimate, digits)
    se_text <- format_stat(row$se, digits)
    t_text <- format_stat(row$t, digits)
    df_text <- format_stat(row$df, digits)
    p_text <- if (!is.na(row$p_adj)) format_p(row$p_adj) else format_p(row$p)
    ci_text <- format_ci(row$ci_low, row$ci_high, digits)
    sentence <- sprintf("%s: estimate = %s, SE = %s", label, est_text, se_text)
    if (nzchar(t_text)) {
      if (nzchar(df_text)) {
        sentence <- paste0(sentence, ", t(", df_text, ") = ", t_text)
      } else {
        sentence <- paste0(sentence, ", t = ", t_text)
      }
    }
    if (nzchar(p_text)) {
      sentence <- paste0(sentence, ", p ", p_text)
    }
    if (nzchar(ci_text)) {
      sentence <- paste0(sentence, ", CI ", ci_text)
    }
    sentence <- if (is.finite(estimate)) paste0(sentence, ".") else
      paste0(label, ": unavailable (non-estimable marginal mean or contrast); the planned row is retained.")
    rows[[length(rows) + 1]] <- list(
      full_sentence = sentence,
      term = row$term,
      level = row$level,
      contrast = row$contrast,
      emmean = format_stat(row$emmean, digits),
      estimate = format_stat(row$estimate, digits),
      se = se_text,
      df = df_text,
      t = t_text,
      p = format_p(row$p),
      p_adj = format_p(row$p_adj),
      ci = ci_text,
      ci_low = format_stat(row$ci_low, digits),
      ci_high = format_stat(row$ci_high, digits)
    )
  }
  rows
}

build_mixed_models_note_tokens <- function(random_terms, conf_level, standardize, fit_df, r2_df, icc_df, diagnostics_df, df_method_used, optimizer, reml, emmeans_note) {
  notes <- character(0)
  notes <- c(notes, "Fixed effects are unstandardized (b) with standard errors.")
  if (standardize != "none") {
    notes <- c(notes, "Standardized coefficients (beta) are reported for numeric predictors.")
  }
  notes <- c(notes, sprintf("CI uses %s%% confidence.", round(conf_level * 100)))
  if (df_method_used != "none") {
    notes <- c(notes, paste0("df method: ", df_method_used, "."))
  }
  if (nzchar(emmeans_note)) {
    notes <- c(notes, emmeans_note)
  }
  random_note <- ""
  if (length(random_terms) > 0) {
    random_note <- paste0("Random effects: ", paste(random_terms, collapse = ", "), ".")
    notes <- c(notes, random_note)
  }
  fit_note <- ""
  if (!is.null(fit_df) && nrow(fit_df) > 0) {
    row <- fit_df[1, ]
    fit_note <- paste0(
      "Model fit: AIC = ", format_stat(row$aic, 2),
      ", BIC = ", format_stat(row$bic, 2),
      ", logLik = ", format_stat(row$logLik, 2),
      "."
    )
    notes <- c(notes, fit_note)
  }
  r2_m <- ""
  r2_c <- ""
  if (!is.null(r2_df) && nrow(r2_df) > 0) {
    r2_m <- format_stat(r2_df$r2_marginal[1], 2)
    r2_c <- format_stat(r2_df$r2_conditional[1], 2)
    if (nzchar(r2_m) && nzchar(r2_c)) {
      notes <- c(notes, paste0("R²m = ", r2_m, ", R²c = ", r2_c, "."))
    }
  }
  icc_val <- ""
  if (!is.null(icc_df) && nrow(icc_df) > 0) {
    icc_val <- format_stat(icc_df$icc[1], 2)
    if (nzchar(icc_val)) {
      notes <- c(notes, paste0("ICC = ", icc_val, "."))
    }
  }
  convergence_note <- ""
  if (!is.null(diagnostics_df) && nrow(diagnostics_df) > 0) {
    conv_row <- diagnostics_df[diagnostics_df$metric == "convergence", , drop = FALSE]
    if (nrow(conv_row) > 0 && nzchar(conv_row$note[1])) {
      convergence_note <- paste0("Convergence warning: ", conv_row$note[1], ".")
      notes <- c(notes, convergence_note)
    }
    sing_row <- diagnostics_df[diagnostics_df$metric == "singular_fit", , drop = FALSE]
    if (nrow(sing_row) > 0 && sing_row$value[1] == "TRUE") {
      convergence_note <- paste0(convergence_note, ifelse(nzchar(convergence_note), " ", ""), "Singular fit detected.")
      notes <- c(notes, "Singular fit detected.")
    }
  }

  list(
    note_default = paste(notes, collapse = " "),
    random_effects_note = random_note,
    fit_note = fit_note,
    icc = icc_val,
    r2_marginal = r2_m,
    r2_conditional = r2_c,
    convergence_note = convergence_note,
    optimizer = optimizer,
    reml = ifelse(isTRUE(reml), "REML", "ML")
  )
}

build_emmeans_note_tokens <- function(conf_level, contrast_label, p_adjust, contrast_file = "") {
  notes <- character(0)
  notes <- c(notes, sprintf("CI uses %s%% confidence.", round(conf_level * 100)))
  if (contrast_label != "none") {
    note <- paste0("Contrasts: ", contrast_label, ".")
    if (nzchar(contrast_file)) {
      note <- paste0(note, " File: ", basename(contrast_file), ".")
    }
    note <- paste0(note, " P-value adjustment: ", p_adjust, ".")
    notes <- c(notes, note)
  }
  list(note_default = paste(notes, collapse = " "))
}

mixed_summary_value <- function(x, keys) {
  for (key in keys) if (key %in% names(x)) return(as.numeric(x[[key]]))
  rep(NA_real_, nrow(x))
}

build_emmeans_rows <- function(emm_summary, term_label, grid_names) {
  columns <- intersect(grid_names, names(emm_summary))
  level <- if (length(columns)) apply(emm_summary[, columns, drop = FALSE], 1, function(row)
    paste(paste0(columns, "=", row), collapse = ", ")) else rep("", nrow(emm_summary))
  data.frame(term = term_label, level = level, contrast = "", emmean = emm_summary$emmean, estimate = NA_real_,
    se = emm_summary$SE, df = emm_summary$df, t = mixed_summary_value(emm_summary, c("t.ratio", "z.ratio")),
    p = mixed_summary_value(emm_summary, "p.value"), p_adj = NA_real_,
    ci_low = mixed_summary_value(emm_summary, c("lower.CL", "asymp.LCL")),
    ci_high = mixed_summary_value(emm_summary, c("upper.CL", "asymp.UCL")), method = "emmeans",
    p_adjust_requested = "none", p_adjust_effective = attr(emm_summary, "adjust"),
    status = ifelse(is.finite(emm_summary$emmean), "available", "unavailable"), stringsAsFactors = FALSE)
}

build_contrasts_rows <- function(contrast_summary, term_label, p_adjust, method_label) {
  effective <- attr(contrast_summary, "adjust")
  data.frame(term = term_label, level = "", contrast = contrast_summary$contrast, emmean = NA_real_,
    estimate = contrast_summary$estimate, se = contrast_summary$SE, df = contrast_summary$df,
    t = mixed_summary_value(contrast_summary, c("t.ratio", "z.ratio")),
    p = if (effective == "none") contrast_summary$p.value else rep(NA_real_, nrow(contrast_summary)),
    p_adj = if (effective != "none") contrast_summary$p.value else rep(NA_real_, nrow(contrast_summary)),
    ci_low = mixed_summary_value(contrast_summary, c("lower.CL", "asymp.LCL")),
    ci_high = mixed_summary_value(contrast_summary, c("upper.CL", "asymp.UCL")), method = method_label,
    p_adjust_requested = p_adjust, p_adjust_effective = effective,
    status = ifelse(is.finite(contrast_summary$estimate), "available", "unavailable"), stringsAsFactors = FALSE)
}

mixed_value_hex <- function(value) paste(sprintf("%02x", as.integer(charToRaw(enc2utf8(as.character(value))))), collapse = "")

mixed_frame_contract <- function(frame) lapply(frame, function(x) list(
  class = class(x), values = if (is.factor(x)) as.character(x) else unname(x),
  levels = if (is.factor(x)) levels(x) else NULL,
  columns = if (is.matrix(x)) colnames(x) else NULL))


mixed_group_identity <- function(formula, data) {
  group_vars <- unique(unlist(lapply(reformulas::findbars(formula), function(bar) all.vars(bar[[3]]))))
  aliases <- list()
  for (name in group_vars) {
    raw <- data[[name]]
    if (!is.numeric(raw)) next
    values <- sort(unique(raw[!is.na(raw)]))
    if (length(levels(factor(raw))) == length(values)) next
    alias <- ".nlss_group_identity"
    while (alias %in% names(data)) alias <- paste0(alias, "_")
    labels <- sprintf("%.17g", values)
    if (anyDuplicated(labels)) labels <- paste0(labels, " [", seq_along(labels), "]")
    data[[alias]] <- factor(match(raw, values), levels = seq_along(values), labels = labels)
    aliases[[name]] <- alias
  }
  substitute_group <- function(node) {
    if (is.symbol(node) && as.character(node) %in% names(aliases)) return(as.name(aliases[[as.character(node)]]))
    if (is.call(node)) for (i in seq_along(node)[-1L]) node[[i]] <- substitute_group(node[[i]])
    node
  }
  walk <- function(node) {
    if (!is.call(node)) return(node)
    if (is.symbol(node[[1]]) && as.character(node[[1]]) %in% c("|", "||")) node[[3]] <- substitute_group(node[[3]]) else
      for (i in seq_along(node)[-1L]) node[[i]] <- walk(node[[i]])
    node
  }
  formula[[3]] <- walk(formula[[3]])
  list(formula = formula, data = data, aliases = aliases)
}

main <- function() {
  opts <- nlss_run_options(commandArgs(trailingOnly = TRUE), "mixed_models")

  if (!is.null(opts$help)) {
    print_usage()
    return(invisible(NULL))
  }

  if (parse_bool(opts$interactive, FALSE)) {
    opts <- modifyList(opts, interactive_options())
  }

  digits_default <- get_config_value("defaults.digits")
  log_default <- get_config_value("defaults.log")
  reml_default <- get_config_value("modules.mixed_models.reml")
  type_default <- get_config_value("modules.mixed_models.type")
  df_method_default <- get_config_value("modules.mixed_models.df_method")
  standardize_default <- get_config_value("modules.mixed_models.standardize")
  emmeans_default <- get_config_value("modules.mixed_models.emmeans")
  contrasts_default <- get_config_value("modules.mixed_models.contrasts")
  p_adjust_default <- get_config_value("modules.mixed_models.p_adjust")
  conf_default <- get_config_value("modules.mixed_models.conf_level")
  optimizer_default <- get_config_value("modules.mixed_models.optimizer")
  maxfun_default <- get_config_value("modules.mixed_models.maxfun")
  diagnostics_default <- get_config_value("modules.mixed_models.diagnostics")
  max_shapiro_n_default <- get_config_value("modules.mixed_models.max_shapiro_n")

  digits <- if (!is.null(opts$digits)) as.numeric(opts$digits) else digits_default
  df <- nlss_load_input(opts)
  out_dir <- get_workspace_out_dir(df)
  nlss_begin_run("mixed_models", df, opts, out_dir)
  emit_input_issue <- function(out_dir, opts, message, details = list(), status = "invalid_input") {
    nlss_run_context$request$validation_issue <- list(message = message, details = details, status = status)
    stop(message)
  }

  if (!requireNamespace("lme4", quietly = TRUE)) {
    emit_input_issue(out_dir, opts, "Mixed models require the 'lme4' package.", details = list(package = "lme4"), status = "missing_dependency")
  }
  if (!requireNamespace("performance", quietly = TRUE)) {
    emit_input_issue(out_dir, opts, "Mixed models require the 'performance' package.", details = list(package = "performance"), status = "missing_dependency")
  }

  reml <- normalize_reml(opts$reml, reml_default)
  type <- normalize_type(opts$type, type_default)
  df_method <- normalize_df_method(opts$`df-method`, df_method_default)
  standardize <- normalize_standardize(opts$standardize, standardize_default)
  emmeans_term <- normalize_emmeans(opts$emmeans, emmeans_default)
  has_emmeans <- requireNamespace("emmeans", quietly = TRUE)
  emmeans_note <- ""
  contrast_file <- if (!is.null(opts$`contrast-file`)) as.character(opts$`contrast-file`) else ""
  contrasts_input <- normalize_contrasts(opts$contrasts, contrasts_default)
  contrast_spec <- if (!is.null(nlss_run_context$replay)) nlss_run_context$replay$request$design$contrast_spec else
    tryCatch(resolve_contrast_spec(contrasts_input, contrast_file), error = function(e) e)
  if (!is.null(contrast_spec$source)) contrast_spec$source <- basename(contrast_spec$source)
  if (inherits(contrast_spec, "error")) {
    emit_input_issue(out_dir, opts, contrast_spec$message, details = list(contrasts = contrasts_input, contrast_file = contrast_file))
  }
  contrast_label <- if (!is.null(contrast_spec$label)) contrast_spec$label else "none"
  if (!is.null(contrast_spec$term) && nzchar(contrast_spec$term)) {
    if (nzchar(emmeans_term) && emmeans_term != contrast_spec$term) {
      emit_input_issue(
        out_dir,
        opts,
        "Contrast term does not match --emmeans.",
        details = list(emmeans = emmeans_term, contrast_term = contrast_spec$term)
      )
    }
    if (!nzchar(emmeans_term)) {
      emmeans_term <- contrast_spec$term
    }
  }
  contrasts_active <- !is.null(contrast_spec) && contrast_spec$mode != "none"
  if (contrasts_active && !nzchar(emmeans_term)) stop("Contrasts require --emmeans or a term in the contrast JSON.")
  if (nzchar(emmeans_term) && !has_emmeans) stop("Requested marginal means require the 'emmeans' package.")
  p_adjust <- if (!is.null(opts$`p-adjust`) && nzchar(opts$`p-adjust`)) as.character(opts$`p-adjust`) else p_adjust_default
  conf_level <- normalize_conf_level(opts$`conf-level`, conf_default)
  optimizer <- if (!is.null(opts$optimizer) && nzchar(opts$optimizer)) as.character(opts$optimizer) else optimizer_default
  maxfun <- normalize_maxfun(opts$maxfun, maxfun_default)
  diagnostics <- parse_bool(opts$diagnostics, default = diagnostics_default)
  max_shapiro_n <- if (!is.null(opts$`max-shapiro-n`)) as.numeric(opts$`max-shapiro-n`) else max_shapiro_n_default

  if (length(digits) != 1L || !is.finite(digits) || digits < 0 || digits > 15 || digits != floor(digits)) stop("digits must be an integer from 0 to 15.")
  if (length(max_shapiro_n) != 1L || !is.finite(max_shapiro_n) || max_shapiro_n < 3 || max_shapiro_n != floor(max_shapiro_n)) stop("max-shapiro-n must be an integer of at least 3.")
  if (!p_adjust %in% c(p.adjust.methods, "tukey", "sidak", "mvt", "dunnettx", "scheffe")) stop("Unsupported p-adjust method.")
  seed <- nlss_run_seed(opts$seed, stochastic = nzchar(emmeans_term))
  formula_text <- if (!is.null(opts$formula) && nzchar(opts$formula)) as.character(opts$formula) else ""
  dv <- if (!is.null(opts$dv) && nzchar(opts$dv)) as.character(opts$dv) else ""
  fixed_terms <- parse_list(opts$fixed)
  random_terms_raw <- parse_list(opts$random)
  random_terms <- normalize_random_terms(random_terms_raw)

  model_formula <- NULL
  if (nzchar(formula_text)) {
    model_formula <- tryCatch(as.formula(formula_text), error = function(e) {
      emit_input_issue(out_dir, opts, paste0("Invalid formula: ", e$message), details = list(formula = formula_text))
    })
    if (!is.symbol(model_formula[[2]])) stop("LMM response must name one numeric column; prepare transformed responses explicitly.")
    dv <- as.character(model_formula[[2]])
    random_terms <- extract_random_terms_from_formula(formula_text)
  } else {
    if (!nzchar(dv)) {
      emit_input_issue(out_dir, opts, "Mixed models require --formula or --dv.", details = list(dv = opts$dv))
    }
    if (length(random_terms) == 0) {
      emit_input_issue(out_dir, opts, "Mixed models require --random or random effects in --formula.", details = list(random = opts$random))
    }
    model_formula <- build_model_formula(dv, fixed_terms, random_terms)
  }

  if (length(random_terms) == 0) {
    emit_input_issue(out_dir, opts, "Mixed models require at least one random effect term.", details = list(formula = formula_text))
  }

  model_vars <- all.vars(model_formula)
  missing_vars <- setdiff(model_vars, names(df))
  if (length(missing_vars) > 0) {
    emit_input_issue(out_dir, opts, paste0("Missing variables: ", paste(missing_vars, collapse = ", ")), details = list(missing = missing_vars))
  }

  if (!dv %in% names(df)) {
    emit_input_issue(out_dir, opts, sprintf("Dependent variable '%s' not found.", dv), details = list(dv = dv))
  }
  if (!is.numeric(df[[dv]])) {
    emit_input_issue(out_dir, opts, "Dependent variable must be numeric for LMM.", details = list(dv = dv))
  }
  infinite_vars <- model_vars[vapply(df[model_vars], function(x) is.numeric(x) && any(is.infinite(x)), logical(1))]
  if (length(infinite_vars)) {
    emit_input_issue(out_dir, opts, paste0("Selected model variables contain infinite values: ", paste(infinite_vars, collapse = ", ")),
      details = list(variables = infinite_vars, source_rows = lapply(df[infinite_vars], function(x) which(is.infinite(x)))))
  }

  source_classes <- lapply(df[model_vars], class)
  df <- coerce_model_factors(df, model_vars, dv)
  requested_formula <- model_formula
  identities <- mixed_group_identity(model_formula, df)
  model_formula <- identities$formula
  df <- identities$data
  rownames(df) <- as.character(seq_len(nrow(df)))
  complete_idx <- get_complete_rows(df[, model_vars, drop = FALSE])
  data_model <- df[complete_idx, , drop = FALSE]
  if (nrow(data_model) == 0) {
    emit_input_issue(out_dir, opts, "No complete cases available after listwise deletion.")
  }
  data_model <- droplevels(data_model)

  has_lmerTest <- requireNamespace("lmerTest", quietly = TRUE)
  df_method_used <- df_method
  if (df_method != "none" && !has_lmerTest) stop("Requested df-method requires the 'lmerTest' package.")
  if (df_method == "kenward-roger" && !requireNamespace("pbkrtest", quietly = TRUE)) stop("Kenward-Roger inference requires the 'pbkrtest' package.")
  if (df_method == "kenward-roger" && !reml) stop("Kenward-Roger inference requires REML; request --reml TRUE or another df-method.")
  if (df_method == "none" && type != "I" && !requireNamespace("car", quietly = TRUE)) stop("Type II/III Wald tests with df-method none require 'car'.")
  if (length(unique(data_model[[dv]])) < 2L) stop("LMM response must vary across retained observations.")

  control <- build_lmer_control(optimizer, maxfun)
  fit <- tryCatch({
    if (df_method_used != "none" && has_lmerTest) {
      lmerTest::lmer(model_formula, data = data_model, REML = reml, control = control, na.action = na.omit)
    } else {
      lme4::lmer(model_formula, data = data_model, REML = reml, control = control, na.action = na.omit)
    }
  }, error = function(e) e)

  if (inherits(fit, "error")) {
    emit_input_issue(out_dir, opts, paste0("Model fit failed: ", fit$message), status = "fit_failed")
  }

  frame <- model.frame(fit)
  if (any(!get_complete_rows(frame))) stop("Fitted model frame contains non-finite transformed values.")
  included_rows <- as.integer(rownames(frame))
  data_model <- droplevels(df[included_rows, , drop = FALSE])
  if (length(unique(model.response(frame))) < 2L || !is.finite(sigma(fit)) || sigma(fit) <= 0)
    stop("LMM requires a varying response and positive finite residual standard deviation.")
  if (any(!is.finite(lme4::fixef(fit)))) stop("LMM fixed effects are not finite.")
  summary_obj <- if (df_method_used != "none" && has_lmerTest) {
    ddf_label <- if (df_method_used == "kenward-roger") "Kenward-Roger" else "Satterthwaite"
    summary(fit, ddf = ddf_label)
  } else {
    summary(fit)
  }

  fixed_df <- extract_fixed_effects(summary_obj, data_model, dv, conf_level, standardize)
  if (nrow(fixed_df) > 0) {
    fixed_df$model <- "Model 1"
  }
  label_meta <- resolve_label_metadata(data_model)
  fixed_df <- add_term_label_column(fixed_df, label_meta, term_col = "term")
  random_df <- extract_random_effects(fit)
  fit_df <- extract_fit_stats(fit)
  r2_df <- extract_r2_df(fit)
  icc_df <- extract_icc_df(fit)
  diagnostics_df <- build_diagnostics(fit, max_shapiro_n, residual_checks = diagnostics)
  anova_df <- build_anova_df(fit, type, df_method_used, has_lmerTest)
  anova_type_used <- attr(anova_df, "type_used")
  if (is.null(anova_type_used) || !nzchar(anova_type_used)) {
    anova_type_used <- type
  }
  anova_method <- attr(anova_df, "method")

  emmeans_df <- data.frame()
  contrasts_df <- data.frame()
  contrast_adjustment <- NULL
  emmeans_messages <- character()
  emmeans_grid <- NULL
  contrast_method <- NULL
  emmeans_df_method <- if (df_method == "none") "asymptotic" else df_method
  if (nzchar(emmeans_term)) {
    # Do not let emmeans' default KR method or size limit silently replace the
    # requested inference. Its own selected method is checked and recorded.
    emm <- emmeans::emmeans(fit, specs = as.formula(paste("~", emmeans_term)),
      lmer.df = emmeans_df_method, lmerTest.limit = Inf, pbkrtest.limit = Inf,
      disable.lmerTest = FALSE, disable.pbkrtest = FALSE)
    actual_df_method <- attr(emm@dffun, "mesg")
    if (!identical(actual_df_method, emmeans_df_method))
      stop("emmeans could not honor requested df method: ", emmeans_df_method, " (effective: ", actual_df_method, ").")
    emm_summary <- summary(emm, infer = c(TRUE, TRUE), level = conf_level, adjust = "none")
    emmeans_grid <- as.data.frame(emm@grid)
    emmeans_df <- build_emmeans_rows(emm_summary, emmeans_term, setdiff(names(emm@grid), ".wgt."))
    emmeans_messages <- unique(attr(emm_summary, "mesg"))
    if (contrasts_active) {
      contrast_method <- build_contrast_method(contrast_spec, emm, emmeans_term)
      if (is.list(contrast_method$method) && any(!is.finite(unlist(contrast_method$method))))
        stop("Custom contrast weights must all be finite.")
      cont <- do.call(emmeans::contrast, c(list(emm, method = contrast_method$method), contrast_method$args))
      cont_summary <- summary(cont, infer = c(TRUE, TRUE), adjust = p_adjust, level = conf_level)
      contrasts_df <- build_contrasts_rows(cont_summary, emmeans_term, p_adjust, contrast_label)
      contrast_adjustment <- list(requested = p_adjust, effective = attr(cont_summary, "adjust"),
        messages = attr(cont_summary, "mesg"), family_size = nrow(cont_summary))
      emmeans_messages <- unique(c(emmeans_messages, attr(cont_summary, "mesg")))
    }
  }

  emmeans_df <- add_term_label_column(emmeans_df, label_meta, term_col = "term")
  emmeans_df <- add_value_label_column(emmeans_df, label_meta, var_col = "term", value_col = "level")
  contrasts_df <- add_term_label_column(contrasts_df, label_meta, term_col = "term")
  contrasts_df <- add_value_label_column(contrasts_df, label_meta, var_col = "term", value_col = "level")

  x <- lme4::getME(fit, "X")
  dropped <- attr(x, "col.dropped")
  if (length(dropped)) {
    absent <- fixed_df[rep(1L, length(dropped)), , drop = FALSE]
    absent[] <- lapply(absent, function(col) if (is.numeric(col)) rep(NA_real_, length(dropped)) else rep("", length(dropped)))
    absent$term <- names(dropped)
    absent$term_label <- names(dropped)
    absent$model <- "Model 1"
    fixed_df <- rbind(fixed_df, absent)
  }
  fixed_df$status <- ifelse(is.finite(fixed_df$estimate), "available", "unavailable")
  fixed_df$reason <- ifelse(fixed_df$status == "available", "", "Dropped non-estimable fixed-effect column.")
  fixed_df$ci_method <- if (df_method == "none") "normal Wald" else paste("t Wald", df_method)
  groups <- lme4::getME(fit, "flist")
  design <- list(formula = paste(deparse(requested_formula), collapse = " "),
    effective_formula = paste(deparse(model_formula), collapse = " "),
    grouping_aliases = identities$aliases,
    fixed_formula = paste(deparse(reformulas::nobars(model_formula)), collapse = " "),
    random_terms = random_terms, model_variables = model_vars, response = dv,
    included_rows = included_rows, excluded_rows = setdiff(seq_len(nrow(df)), included_rows),
    raw_complete_rows = which(complete_idx), transformed_excluded_rows = setdiff(which(complete_idx), included_rows),
    missing = "joint complete finite source cases, then model-frame NA omission after transformations",
    source_classes = source_classes, analysis_classes = lapply(data_model[model_vars], class),
    coercion = "Numeric codes retain numeric roles; nonnumeric model predictors become factors; grouping factors use lme4's actual fitted assignments.",
    model_frame = mixed_frame_contract(frame),
    grouping = lapply(names(groups), function(name) {
      group <- groups[[name]]
      list(name = name, row_group_ids = as.integer(group), levels = lapply(seq_along(levels(group)), function(i)
        list(level_id = i, label = levels(group)[i], value_hex = mixed_value_hex(levels(group)[i]),
          source_rows = included_rows[as.integer(group) == i])))
    }),
    factor_levels = lapply(data_model[model_vars[vapply(data_model[model_vars], is.factor, logical(1))]], levels),
    factor_contrasts = attr(x, "contrasts"),
    model_matrix = list(columns = colnames(x), rank = qr(x)$rank, dropped = as.list(dropped)),
    inference = list(type_requested = type, type_effective = anova_type_used, omnibus_method = anova_method,
      df_requested = df_method, df_effective = df_method_used,
      fixed_ci = if (df_method == "none") "normal Wald; fixed-effect p and denominator df unavailable" else paste("t Wald", df_method),
      emmeans_df = if (nzchar(emmeans_term)) emmeans_df_method else NULL),
    fit_status = list(reml_requested = reml, reml_effective = lme4::isREML(fit),
      singular = lme4::isSingular(fit), singular_tolerance = 1e-4,
      optimizer = fit@optinfo$optimizer, convergence_code = fit@optinfo$conv$opt,
      optimizer_control = fit@optinfo$control, function_evaluations = fit@optinfo$feval,
      messages = unique(c(unlist(fit@optinfo$conv$lme4$messages), unlist(fit@optinfo$warnings)))),
    standardization = list(method = standardize, definition = "descriptive b * SD(x) / SD(y), direct numeric main effects only; no model refit",
    response_sd = sd(data_model[[dv]]), predictor_sd = lapply(data_model[model_vars], function(x) if (is.numeric(x)) sd(x) else NULL)),
    contrast_spec = contrast_spec, contrast_method = contrast_method, contrast_adjustment = contrast_adjustment,
    emmeans_grid = emmeans_grid, emmeans_messages = emmeans_messages)
  nlss_resolve_request(list(formula = design$formula, reml = reml, type_requested = type,
    type = anova_type_used, df_method = df_method_used, standardize = standardize,
    emmeans = emmeans_term, contrasts = contrast_label, p_adjust = p_adjust, conf_level = conf_level,
    optimizer = optimizer, maxfun = maxfun, diagnostics = diagnostics, max_shapiro_n = max_shapiro_n,
    seed = seed, digits = digits), design)
  availability_notes <- c(if (length(dropped)) paste("Non-estimable fixed effects:", paste(names(dropped), collapse = ", ")),
    if (any(fixed_df$status != "available")) "Unavailable coefficient rows are retained explicitly.",
    if (nrow(emmeans_df) && any(emmeans_df$status != "available")) "Some requested marginal means are unavailable (non-estimable).",
    if (nrow(contrasts_df) && any(contrasts_df$status != "available")) "Some requested contrasts are unavailable (non-estimable); planned rows are retained.",
    if (r2_df$status != "available") r2_df$reason, if (icc_df$status != "available") icc_df$reason,
    if (df_method == "none") "Fixed-effect intervals use normal Wald critical values; denominator df and fixed-effect p-values are unavailable.")
  scientific_warnings <- if (length(nlss_run_context$warnings)) unique(vapply(nlss_run_context$warnings, function(x) x$message, character(1))) else character()
  emmeans_note <- paste(c(paste0("Response: ", resolve_variable_label(label_meta, dv), " [", dv, "]."),
    paste0("Fit criterion: ", fit_df$criterion, ". REML criteria must not be used to compare different fixed-effect designs."),
    availability_notes, emmeans_messages,
    render_paths_for_log(scientific_warnings, workspace_root = nlss_run_context$root)), collapse = " ")
  analysis_flags <- list(
    formula = if (nzchar(formula_text)) formula_text else NULL,
    dv = if (!nzchar(formula_text)) dv else NULL,
    fixed = if (!nzchar(formula_text) && length(fixed_terms) > 0) fixed_terms else NULL,
    random = if (length(random_terms) > 0) random_terms else NULL,
    reml = reml,
    "df-method" = if (df_method_used != "none") df_method_used else NULL,
    type = type,
    standardize = if (standardize != "none") standardize else NULL,
    emmeans = if (nzchar(emmeans_term)) emmeans_term else NULL,
    contrasts = if (contrasts_active) contrast_label else NULL,
    "contrast-file" = if (nzchar(contrast_file)) basename(contrast_file) else NULL,
    "p-adjust" = if (contrasts_active) p_adjust else NULL,
    "conf-level" = conf_level,
    optimizer = optimizer,
    maxfun = maxfun,
    diagnostics = diagnostics,
    digits = digits
  )

  note_tokens <- build_mixed_models_note_tokens(
    random_terms,
    conf_level,
    standardize,
    fit_df,
    r2_df,
    icc_df,
    diagnostics_df,
    df_method_used,
    optimizer,
    reml,
    emmeans_note
  )
  narrative_rows <- build_fixed_effects_narrative_rows(fixed_df, digits)
  nlss_text <- ""
  if (length(narrative_rows) > 0) {
    nlss_text <- paste(vapply(narrative_rows, function(row) row$full_sentence, character(1)), collapse = "\n")
  }

  template_override <- resolve_template_override(opts$template, module = "mixed_models")
  nlss_report_path <- file.path(out_dir, "report_canonical.md")

  anova_template_path <- if (!is.null(template_override)) {
    template_override
  } else {
    resolve_template_path("mixed_models.tests", "mixed-models/tests-of-fixed-effects-template.md")
  }
  anova_template_path <- nlss_freeze_template(anova_template_path, "tests")
  anova_meta <- get_template_meta(anova_template_path)
  anova_table <- build_mixed_anova_table_body(anova_df, digits, anova_meta$table)
  if (nzchar(anova_table$body)) {
    anova_note_tokens <- build_mixed_models_anova_note_tokens(anova_type_used, df_method_used, anova_method)
    anova_narrative_rows <- build_mixed_anova_narrative_rows(anova_df, digits)
    anova_text <- ""
    if (length(anova_narrative_rows) > 0) {
      anova_text <- paste(vapply(anova_narrative_rows, function(row) row$full_sentence, character(1)), collapse = "\n")
    }
    anova_nlss_table <- paste0("Table 1\n\n", anova_table$body, "\n", anova_note_tokens$note_default)
    anova_context <- list(
      tokens = c(
        list(
          table_body = anova_table$body,
          narrative_default = anova_text
        ),
        anova_note_tokens
      ),
      narrative_rows = anova_narrative_rows
    )
    nlss_stage_report(
      nlss_report_path,
      "Mixed Models: Tests of Fixed Effects",
      anova_nlss_table,
      anova_text,
      analysis_flags = analysis_flags,
      template_path = anova_template_path,
      template_context = anova_context
    )
  }

  coef_template_path <- if (!is.null(template_override)) {
    template_override
  } else {
    resolve_template_path("mixed_models.default", "mixed-models/default-template.md")
  }
  coef_template_path <- nlss_freeze_template(coef_template_path, "fixed")
  coef_template_meta <- get_template_meta(coef_template_path)
  table_result <- build_fixed_effects_table_body(fixed_df, digits, coef_template_meta$table)
  nlss_table <- paste0("Table 1\n\n", table_result$body, "\n", note_tokens$note_default)

  template_context <- list(
    tokens = c(
      list(
        table_body = table_result$body,
        narrative_default = nlss_text
      ),
      note_tokens
    ),
    narrative_rows = narrative_rows
  )

  nlss_stage_report(
    nlss_report_path,
    "Mixed Models: Estimates of Fixed Effects",
    nlss_table,
    nlss_text,
    analysis_flags = analysis_flags,
    template_path = coef_template_path,
    template_context = template_context
  )


  variance_rows <- lapply(seq_len(nrow(random_df)), function(i) {
    row <- random_df[i, ]
    c(row$group, row$term, format_num(row$variance, digits), format_num(row$stddev, digits), format_stat(row$corr, digits))
  })
  variance_table <- render_markdown_table(c("Group", "Term", "Variance", "SD", "Correlation"), variance_rows)
  nlss_stage_report(nlss_report_path, "Mixed Models: Variance Components",
    paste0("Table 1\n\n", variance_table),
    paste0("Estimation: ", if (reml) "REML" else "ML", "; ", nobs(fit), " observations. ",
      "Adjusted ICC = ", format_stat(icc_df$icc_adjusted, digits),
      "; unadjusted ICC = ", format_stat(icc_df$icc_unadjusted, digits), ". ",
      "R-squared and ICC use performance's model-based variance definitions, including random-slope contributions when present."),
    analysis_flags = analysis_flags)
  diagnostic_rows <- lapply(seq_len(nrow(diagnostics_df)), function(i) {
    row <- diagnostics_df[i, ]
    c(row$metric, row$value, format_stat(row$statistic, digits), format_p(row$p), row$note)
  })
  diagnostic_table <- render_markdown_table(c("Diagnostic", "Status", "Statistic", "p", "Note"), diagnostic_rows)
  nlss_stage_report(nlss_report_path, "Mixed Models: Diagnostics",
    paste0("Table 1\n\n", diagnostic_table),
    paste("Singularity and optimizer convergence are always reported; residual checks are",
      if (diagnostics) "enabled." else "disabled.", emmeans_note), analysis_flags = analysis_flags)

  if (nrow(emmeans_df) > 0 && nrow(contrasts_df) > 0) {
    emmeans_rows <- rbind(emmeans_df, contrasts_df)
  } else if (nrow(emmeans_df) > 0) {
    emmeans_rows <- emmeans_df
  } else if (nrow(contrasts_df) > 0) {
    emmeans_rows <- contrasts_df
  } else {
    emmeans_rows <- data.frame()
  }
  if (nrow(emmeans_rows) > 0) {
    emmeans_template_path <- if (!is.null(template_override)) {
      template_override
    } else {
      resolve_template_path("mixed_models.emmeans", "mixed-models/emmeans-template.md")
    }
    emmeans_template_path <- nlss_freeze_template(emmeans_template_path, "emmeans")
    emmeans_meta <- get_template_meta(emmeans_template_path)
    emmeans_table <- build_emmeans_table_body(emmeans_rows, digits, emmeans_meta$table)
    emmeans_note_tokens <- build_emmeans_note_tokens(conf_level, format_contrast_label(contrast_spec), if (is.null(contrast_adjustment)) "none" else contrast_adjustment$effective, contrast_file)
    emmeans_narrative_rows <- build_emmeans_narrative_rows(emmeans_rows, digits)
    emmeans_text <- ""
    if (length(emmeans_narrative_rows) > 0) {
      emmeans_text <- paste(vapply(emmeans_narrative_rows, function(row) row$full_sentence, character(1)), collapse = "\n")
    }
    emmeans_nlss_table <- paste0("Table 1\n\n", emmeans_table$body, "\n", emmeans_note_tokens$note_default)
    emmeans_context <- list(
      tokens = c(
        list(
          table_body = emmeans_table$body,
          narrative_default = emmeans_text
        ),
        emmeans_note_tokens
      ),
      narrative_rows = emmeans_narrative_rows
    )
    nlss_stage_report(
      nlss_report_path,
      "Mixed Models emmeans",
      emmeans_nlss_table,
      emmeans_text,
      analysis_flags = analysis_flags,
      template_path = emmeans_template_path,
      template_context = emmeans_context
    )
  }

  cat("Wrote:\n")
  cat("- ", render_output_path(nlss_report_path, out_dir), "\n", sep = "")

  results <- list(fixed_effects_df = fixed_df, random_effects_df = random_df, fit_df = fit_df,
    r2_df = r2_df, icc_df = icc_df, anova_df = anova_df, emmeans_df = emmeans_df,
    contrasts_df = contrasts_df, diagnostics_df = diagnostics_df, contrast_adjustment = contrast_adjustment,
    inference = design$inference, fit_status = design$fit_status)
  nlss_set_result(results)
  if (parse_bool(opts$log, log_default)) {
    ctx <- get_run_context()
    nlss_stage_log(
      out_dir,
      module = "mixed_models",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = results,
      options = analysis_flags,
      user_prompt = get_user_prompt(opts)
    )
  }
}

nlss_run_main("mixed_models", main)
