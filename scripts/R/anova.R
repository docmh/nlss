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
  cat("ANOVA (base R)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript anova.R --csv data.csv --dv outcome --between group\n")
  cat("  Rscript anova.R --csv data.csv --dv outcome --between group,gender --covariates age\n")
  cat("  Rscript anova.R --csv data.csv --within pre,post --subject-id id\n")
  cat("  Rscript anova.R --csv data.csv --within pre,mid,post --between group --subject-id id\n")
  cat("  Rscript anova.R --parquet data.parquet --dv outcome --between group\n")
  cat("  Rscript anova.R --interactive\n")
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
  cat("  --dv NAME              Dependent variable (between-subjects)\n")
  cat("  --between LIST         Comma-separated between-subjects factors\n")
  cat("  --within LIST           Comma-separated within-subjects variables (wide format)\n")
  cat("  --subject-id NAME      Subject identifier (required for within/mixed)\n")
  cat("  --covariates LIST       Comma-separated covariates (numeric)\n")
  cat("  --type TYPE            Sum of squares type (I/II/III; default: II)\n")
  cat("  --effect-size TYPE     eta_sq, partial_eta, omega_sq, or partial_omega (default: partial_eta)\n")
  cat("  --posthoc TYPE          none/tukey/pairwise (default: tukey)\n")
  cat("  --emmeans TERM         Term for planned contrasts (default: none)\n")
  cat("  --contrasts TYPE       none/pairwise/custom/<method> (default: none)\n")
  cat("  --contrast-file PATH   JSON contrast spec for custom or method args\n")
  cat("  --p-adjust METHOD      P-value adjustment (default: holm)\n")
  cat("  --conf-level VALUE     Confidence level (default: 0.95)\n")
  cat("  --sphericity MODE      auto/none (default: auto)\n")
  cat("  --bootstrap TRUE/FALSE Bootstrap confidence intervals (default: FALSE)\n")
  cat("  --bootstrap-samples N  Bootstrap resamples (default: 1000)\n")
  cat("  --seed N               Resampling/emmeans seed (default: modules.anova.seed)\n")
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

  mode <- prompt("Design (between/within/mixed)", "between")
  mode <- tolower(mode)

  if (mode %in% c("between", "mixed")) {
    if (mode == "between") opts$dv <- prompt("Dependent variable", "")
    opts$between <- prompt("Between-subjects factors (comma-separated)", "")
    opts$covariates <- prompt("Covariates (comma-separated, optional)", "")
  }

  if (mode %in% c("within", "mixed")) {
    opts$within <- prompt("Within-subjects variables (comma-separated, wide format)", "")
    opts$`subject-id` <- prompt("Subject ID", "")
    if (mode == "mixed" && (is.null(opts$between) || opts$between == "")) {
      opts$between <- prompt("Between-subjects factors (comma-separated)", "")
    }
  }

  type_default <- get_config_value("modules.anova.type")
  if (mode %in% c("within", "mixed")) {
    type_default <- "I"
    cat("Repeated/mixed ANOVA uses sequential Type I sums of squares.\n")
  }
  effect_default <- get_config_value("modules.anova.effect_size")
  posthoc_default <- get_config_value("modules.anova.posthoc")
  emmeans_default <- get_config_value("modules.anova.emmeans")
  contrasts_default <- get_config_value("modules.anova.contrasts")
  p_adjust_default <- get_config_value("modules.anova.p_adjust")
  conf_default <- get_config_value("modules.anova.conf_level")
  sphericity_default <- get_config_value("modules.anova.sphericity")
  bootstrap_default <- get_config_value("modules.anova.bootstrap")
  bootstrap_samples_default <- get_config_value("modules.anova.bootstrap_samples")
  digits_default <- get_config_value("defaults.digits")

  opts$type <- prompt("Sum of squares type (I/II/III)", type_default)
  opts$`effect-size` <- prompt("Effect size (eta_sq/partial_eta/omega_sq/partial_omega)", effect_default)
  opts$posthoc <- prompt("Post-hoc method (none/tukey/pairwise)", posthoc_default)
  opts$emmeans <- prompt("Planned contrasts term (none or term)", emmeans_default)
  opts$contrasts <- prompt("Planned contrasts (none/pairwise/custom/<method>)", contrasts_default)
  contrast_mode <- normalize_contrasts(opts$contrasts, contrasts_default)
  if (contrast_mode == "custom") {
    opts$`contrast-file` <- prompt("Contrast JSON file", "")
  }
  opts$`p-adjust` <- prompt("P-value adjustment", p_adjust_default)
  opts$`conf-level` <- prompt("Confidence level", as.character(conf_default))
  opts$sphericity <- prompt("Sphericity (auto/none)", sphericity_default)
  opts$bootstrap <- prompt("Bootstrap TRUE/FALSE", ifelse(isTRUE(bootstrap_default), "TRUE", "FALSE"))
  opts$`bootstrap-samples` <- prompt("Bootstrap samples", as.character(bootstrap_samples_default))
  opts$digits <- prompt("Rounding digits", as.character(digits_default))
  opts$template <- prompt("Template (path or key; blank for default)", "")
  opts$`user-prompt` <- prompt("User prompt (optional)", "")
  log_default <- get_config_value("defaults.log")
  opts$log <- prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))

  opts
}

normalize_type <- function(value, default = "II") {
  val <- if (!is.null(value) && value != "") value else default
  val <- toupper(gsub("[^A-Za-z0-9]", "", val))
  if (val %in% c("1", "I")) return("I")
  if (val %in% c("2", "II")) return("II")
  if (val %in% c("3", "III")) return("III")
  stop("Invalid --type; use I, II or III.")
}

normalize_effect_size <- function(value, default = "partial_eta") {
  val <- if (!is.null(value) && value != "") value else default
  val <- gsub("[^a-z0-9]", "", tolower(val))
  if (val %in% c("eta", "etasq", "eta2", "etasquared")) return("eta_sq")
  if (val %in% c("partialeta", "partialetasq", "partialeta2", "partialetasquared", "peta", "petasq")) {
    return("partial_eta_sq")
  }
  if (val %in% c("omega", "omegasq", "omega2", "omegasquared")) return("omega_sq")
  if (val %in% c("partialomega", "partialomegasq", "partialomega2", "partialomegasquared", "pomega", "pomegasq")) {
    return("partial_omega_sq")
  }
  if (val == "partial_eta") return("partial_eta_sq")
  if (val == "eta_sq") return("eta_sq")
  if (val == "omega_sq") return("omega_sq")
  if (val == "partial_omega") return("partial_omega_sq")
  stop("Invalid --effect-size.")
}

normalize_posthoc <- function(value, default = "tukey") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (val %in% c("none", "no")) return("none")
  if (val %in% c("pairwise", "pairs")) return("pairwise")
  if (val %in% c("tukey", "tukeyhsd")) return("tukey")
  stop("Invalid --posthoc; use none, tukey or pairwise.")
}

normalize_emmeans <- function(value, default = "none") {
  val <- if (!is.null(value) && value != "") value else default
  val <- trimws(as.character(val))
  if (!nzchar(val) || tolower(val) %in% c("none", "no", "false")) return("")
  val
}

normalize_contrasts <- normalize_contrast_mode

normalize_sphericity <- function(value, default = "auto") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (val %in% c("none", "no")) return("none")
  if (val == "auto") return("auto")
  stop("Invalid --sphericity; use auto or none.")
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

format_effect_label <- function(effect_size) {
  if (effect_size == "eta_sq") return("eta²")
  if (effect_size == "partial_eta_sq") return("Partial eta²")
  if (effect_size == "omega_sq") return("omega²")
  if (effect_size == "partial_omega_sq") return("Partial omega²")
  effect_size
}

get_effect_value <- function(row, effect_size) {
  if (effect_size == "eta_sq") return(row$eta_sq)
  if (effect_size == "partial_eta_sq") return(row$partial_eta_sq)
  if (effect_size == "omega_sq") return(row$omega_sq)
  if (effect_size == "partial_omega_sq") return(row$partial_omega_sq)
  NA_real_
}

calc_boot_ci <- function(values, conf_level) {
  values <- values[is.finite(values)]
  if (length(values) < 2L) return(c(NA_real_, NA_real_))
  alpha <- (1 - conf_level) / 2
  low <- as.numeric(stats::quantile(values, probs = alpha, names = FALSE, na.rm = TRUE))
  high <- as.numeric(stats::quantile(values, probs = 1 - alpha, names = FALSE, na.rm = TRUE))
  c(low, high)
}

build_term_ids <- function(summary_df) {
  paste(summary_df$model, summary_df$term, sep = "|")
}

bootstrap_effect_sizes_between <- function(data_subset, dv, between_vars, covariates, type, effect_size, bootstrap_samples, term_ids, expected_df) {
  boot_vals <- vector("list", length(term_ids))
  names(boot_vals) <- term_ids
  n <- nrow(data_subset)
  if (n == 0 || bootstrap_samples <= 0) return(boot_vals)
  for (i in seq_len(bootstrap_samples)) {
    idx <- sample(seq_len(n), size = n, replace = TRUE)
    sample_df <- data_subset[idx, , drop = FALSE]
    fits <- tryCatch(build_between_model(sample_df, dv, between_vars, covariates), error = function(e) NULL)
    if (is.null(fits)) next
    summary_result <- tryCatch(extract_between_summary(fits$lm, type), error = function(e) NULL)
    if (is.null(summary_result)) next
    boot_summary <- summary_result$summary
    if (is.null(boot_summary) || nrow(boot_summary) == 0) next
    boot_summary$term_id <- paste(boot_summary$model, boot_summary$term, sep = "|")
    for (j in seq_len(nrow(boot_summary))) {
      term_id <- boot_summary$term_id[j]
      idx_match <- match(term_id, term_ids)
      if (is.na(idx_match) || boot_summary$df1[j] != expected_df[idx_match]) next
      val <- get_effect_value(boot_summary[j, ], effect_size)
      boot_vals[[idx_match]] <- c(boot_vals[[idx_match]], val)
    }
  }
  boot_vals
}

bootstrap_effect_sizes_within <- function(data_within, within_vars, between_vars, covariates, subject_id, effect_size, bootstrap_samples, term_ids, expected_df) {
  boot_vals <- vector("list", length(term_ids))
  names(boot_vals) <- term_ids
  n <- nrow(data_within$wide)
  if (n == 0 || bootstrap_samples <= 0) return(boot_vals)
  within_name <- data_within$within_name
  for (i in seq_len(bootstrap_samples)) {
    idx <- sample(seq_len(n), size = n, replace = TRUE)
    sample_wide <- data_within$wide[idx, , drop = FALSE]
    sample_wide[[subject_id]] <- factor(seq_len(nrow(sample_wide)))
    sample_long <- reshape(
      sample_wide,
      varying = within_vars,
      v.names = data_within$response_name,
      timevar = within_name,
      times = within_vars,
      idvar = subject_id,
      direction = "long"
    )
    sample_long[[within_name]] <- factor(sample_long[[within_name]], levels = within_vars)
    fit <- tryCatch(build_within_model(sample_long, subject_id, within_name, between_vars, covariates, data_within$response_name), error = function(e) NULL)
    if (is.null(fit)) next
    summary_result <- tryCatch(extract_within_summary(fit$aov, subject_id, within_name), error = function(e) NULL)
    if (is.null(summary_result)) next
    boot_summary <- summary_result$summary
    if (is.null(boot_summary) || nrow(boot_summary) == 0) next
    boot_summary$term_id <- paste(boot_summary$model, boot_summary$term, sep = "|")
    for (j in seq_len(nrow(boot_summary))) {
      term_id <- boot_summary$term_id[j]
      idx_match <- match(term_id, term_ids)
      if (is.na(idx_match) || boot_summary$df1[j] != expected_df[idx_match]) next
      val <- get_effect_value(boot_summary[j, ], effect_size)
      boot_vals[[idx_match]] <- c(boot_vals[[idx_match]], val)
    }
  }
  boot_vals
}

apply_bootstrap_ci <- function(summary_df, boot_vals, conf_level, bootstrap_samples) {
  summary_df$boot_ci_low <- NA_real_
  summary_df$boot_ci_high <- NA_real_
  summary_df$boot_valid <- vapply(boot_vals, function(x) sum(is.finite(x)), integer(1))
  summary_df$boot_discarded <- bootstrap_samples - summary_df$boot_valid
  summary_df$boot_ci_status <- ifelse(summary_df$boot_valid < 2L, "unavailable", ifelse(summary_df$boot_discarded > 0L, "conditional_on_estimable_resamples", "available"))
  if (any(summary_df$boot_discarded > 0L)) warning("Bootstrap omitted unavailable effect estimates; inspect per-term boot_valid/boot_discarded and interval status.")
  if (length(boot_vals) == 0) return(summary_df)
  for (i in seq_len(nrow(summary_df))) {
    vals <- boot_vals[[i]]
    if (is.null(vals) || length(vals) == 0) next
    ci <- calc_boot_ci(vals, conf_level)
    summary_df$boot_ci_low[i] <- ci[1]
    summary_df$boot_ci_high[i] <- ci[2]
  }
  summary_df
}

safe_shapiro <- function(values, max_n = 5000) {
  values <- values[!is.na(values)]
  n <- length(values)
  if (n < 3 || n > max_n) {
    return(list(w = NA_real_, p = NA_real_, n = n, note = "Shapiro-Wilk requires 3-5000 observations."))
  }
  test <- tryCatch(shapiro.test(values), error = function(e) NULL)
  if (is.null(test)) return(list(w = NA_real_, p = NA_real_, n = n, note = "Shapiro-Wilk failed."))
  list(w = unname(test$statistic), p = test$p.value, n = n, note = "")
}

calc_levene <- function(values, group) {
  group <- as.factor(group)
  if (nlevels(group) < 2) {
    return(list(stat = NA_real_, df1 = NA_real_, df2 = NA_real_, p = NA_real_, note = "Need at least two groups."))
  }
  fit <- tryCatch(
    lm(abs(values - tapply(values, group, median, na.rm = TRUE)[as.character(group)]) ~ group),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    return(list(stat = NA_real_, df1 = NA_real_, df2 = NA_real_, p = NA_real_, note = "Levene test failed."))
  }
  res <- anova(fit)
  list(
    stat = res$`F value`[1],
    df1 = res$Df[1],
    df2 = res$Df[2],
    p = res$`Pr(>F)`[1],
    note = ""
  )
}

calc_bartlett <- function(values, group) {
  test <- tryCatch(bartlett.test(values, group), error = function(e) NULL)
  if (is.null(test)) {
    return(list(stat = NA_real_, df1 = NA_real_, df2 = NA_real_, p = NA_real_, note = "Bartlett test failed."))
  }
  list(
    stat = unname(test$statistic),
    df1 = unname(test$parameter),
    df2 = NA_real_,
    p = test$p.value,
    note = ""
  )
}

calc_fligner <- function(values, group) {
  test <- tryCatch(fligner.test(values, group), error = function(e) NULL)
  if (is.null(test)) {
    return(list(stat = NA_real_, df1 = NA_real_, df2 = NA_real_, p = NA_real_, note = "Fligner-Killeen test failed."))
  }
  list(
    stat = unname(test$statistic),
    df1 = unname(test$parameter),
    df2 = NA_real_,
    p = test$p.value,
    note = ""
  )
}

coerce_numeric <- function(vec, name) {
  out <- if (is.numeric(vec)) vec else suppressWarnings(as.numeric(as.character(vec)))
  lost <- which(!is.na(vec) & is.na(out))
  if (length(lost)) warning("Numeric conversion of ", name, " introduced missing values at source rows: ", paste(lost, collapse = ", "))
  if (any(!is.na(out) & !is.finite(out))) stop("Non-finite numeric observations in ", name, ".")
  out
}

anova_factor <- function(x) {
  if (is.factor(x)) return(droplevels(x))
  raw <- sort(unique(x[!is.na(x)]))
  labels <- as.character(raw)
  if (anyDuplicated(labels)) labels <- paste0(labels, " [level ", seq_along(labels), "]")
  factor(match(x, raw), levels = seq_along(raw), labels = labels)
}

anova_names <- function(x) if (length(x)) paste0("`", gsub("`", "\\`", x, fixed = TRUE), "`") else character(0)

anova_value_hex <- function(x) {
  if (is.numeric(x) || inherits(x, c("Date", "POSIXt", "difftime"))) sprintf("%a", as.double(x)) else NULL
}

anova_constant_in_cells <- function(response, data, between_vars) {
  response <- as.matrix(response)
  cells <- if (length(between_vars)) do.call(paste, c(lapply(data[between_vars], as.integer), sep = ":")) else rep("overall", nrow(data))
  all(vapply(split(seq_len(nrow(data)), cells), function(rows)
    all(vapply(seq_len(ncol(response)), function(j) length(unique(response[rows, j])) < 2L, logical(1))), logical(1)))
}

prepare_anova_cases <- function(df, numeric_vars, factor_vars, required) {
  missing <- setdiff(required, names(df))
  if (length(missing)) stop("Unknown variables: ", paste(missing, collapse = ", "))
  data <- df[, required, drop = FALSE]
  for (var in numeric_vars) data[[var]] <- coerce_numeric(data[[var]], var)
  rows <- which(complete.cases(data))
  data <- data[rows, , drop = FALSE]
  if (!nrow(data)) stop("No complete cases available for analysis.")
  for (var in factor_vars) {
    data[[var]] <- anova_factor(data[[var]])
    if (nlevels(data[[var]]) < 2L) stop("Factor ", var, " must have at least two observed levels.")
  }
  attr(data, "source_rows") <- rows
  data
}

prepare_between_data <- function(df, dv, between_vars, covariates) {
  prepare_anova_cases(df, c(dv, covariates), between_vars, c(dv, between_vars, covariates))
}

prepare_within_data <- function(df, within_vars, subject_id, between_vars, covariates) {
  required <- c(subject_id, within_vars, between_vars, covariates)
  data_subset <- prepare_anova_cases(df, c(within_vars, covariates), between_vars, required)
  if (anyDuplicated(data_subset[[subject_id]])) stop("Wide repeated-measures data require one unique row per subject ID.")
  data_subset[[subject_id]] <- anova_factor(data_subset[[subject_id]])
  within_name <- "within"
  while (within_name %in% required) within_name <- paste0(within_name, "_")
  response_name <- "dv"
  while (response_name %in% required) response_name <- paste0(response_name, "_")
  long_data <- reshape(
    data_subset,
    varying = within_vars,
    v.names = response_name,
    timevar = within_name,
    times = within_vars,
    idvar = subject_id,
    direction = "long"
  )
  long_data[[within_name]] <- factor(long_data[[within_name]], levels = within_vars)
  list(wide = data_subset, long = long_data, within_name = within_name, response_name = response_name)
}

build_between_model <- function(data_subset, dv, between_vars, covariates) {
  if (length(unique(data_subset[[dv]])) < 2L) stop("ANOVA requires variation in the observed dependent variable.")
  if (anova_constant_in_cells(data_subset[[dv]], data_subset, between_vars))
    stop("ANOVA has exactly zero residual variation: responses are identical within every observed between-factor cell.")
  between_term <- if (length(between_vars) > 0) paste(anova_names(between_vars), collapse = " * ") else "1"
  cov_term <- if (length(covariates) > 0) paste(anova_names(covariates), collapse = " + ") else ""
  rhs <- between_term
  if (nzchar(cov_term)) rhs <- paste(rhs, "+", cov_term)
  formula <- as.formula(paste(anova_names(dv), "~", rhs))
  fit <- lm(formula, data = data_subset, na.action = na.fail)
  if (fit$rank < ncol(model.matrix(fit))) stop("ANOVA design is rank deficient; remove redundant covariates or resolve empty/confounded cells.")
  if (df.residual(fit) <= 0 || sum(resid(fit)^2) <= 0) stop("ANOVA requires positive residual degrees of freedom and residual variance.")
  list(lm = fit, aov = aov(formula, data = data_subset, na.action = na.fail), formula = formula)
}

build_within_model <- function(long_data, subject_id, within_name, between_vars, covariates, response_name = "dv") {
  if (length(unique(long_data[[response_name]])) < 2L) stop("Repeated-measures ANOVA requires variation in the observed responses.")
  long_data[[subject_id]] <- anova_factor(long_data[[subject_id]])
  ordered <- long_data[order(long_data[[subject_id]], long_data[[within_name]]), , drop = FALSE]
  k <- nlevels(ordered[[within_name]])
  responses <- matrix(ordered[[response_name]], ncol = k, byrow = TRUE)
  profiles <- responses - responses[, 1L]
  subjects <- ordered[seq.int(1L, nrow(ordered), by = k), , drop = FALSE]
  if (anova_constant_in_cells(profiles, subjects, between_vars))
    stop("Repeated-measures ANOVA has exactly zero within-error variation: within-difference profiles are identical in every between-factor cell.")
  between_term <- if (length(between_vars) > 0) paste(anova_names(between_vars), collapse = " * ") else ""
  fixed_term <- if (nzchar(between_term)) paste(between_term, "*", anova_names(within_name)) else anova_names(within_name)
  if (length(covariates) > 0) {
    fixed_term <- paste(fixed_term, "+", paste(anova_names(covariates), collapse = " + "))
  }
  formula <- as.formula(paste(anova_names(response_name), "~", fixed_term, "+ Error(", anova_names(subject_id), "/", anova_names(within_name), ")"))
  fixed <- as.formula(paste(anova_names(response_name), "~", fixed_term))
  matrix <- model.matrix(fixed, long_data)
  if (qr(matrix)$rank < ncol(matrix)) stop("Repeated-measures fixed design is rank deficient.")
  fit <- aov(formula, data = long_data, na.action = na.fail)
  # emmeans may refit aovlist with orthogonal contrasts outside this function.
  # Its saved call must contain the actual formula/data, not local symbols.
  fit_call <- attr(fit, "call")
  fit_call$formula <- formula
  fit_call$data <- long_data
  attr(fit, "call") <- fit_call
  list(aov = fit, formula = formula, model_matrix = matrix)
}

extract_between_summary <- function(lm_fit, type) {
  use_type <- normalize_type(type, "II")
  used_type <- use_type
  if (use_type != "I" && !requireNamespace("car", quietly = TRUE)) stop("Type II/III ANOVA requires the 'car' package; no fallback to Type I is performed.")
  anova_tbl <- if (use_type == "I") anova(lm_fit) else car::Anova(lm_fit, type = if (use_type == "III") 3 else 2, singular.ok = FALSE)
  table <- as.data.frame(anova_tbl)
  table$term <- rownames(table)

  table <- table[!(table$term %in% c("(Intercept)", "Residuals")), , drop = FALSE]
  residual_df <- df.residual(lm_fit)
  residual_ss <- sum(resid(lm_fit)^2, na.rm = TRUE)
  response <- model.response(model.frame(lm_fit))
  ss_total <- sum((response - mean(response))^2)
  ms_error <- if (!is.na(residual_df) && residual_df > 0) residual_ss / residual_df else NA_real_

  f_col <- if ("F value" %in% names(table)) "F value" else if ("F" %in% names(table)) "F" else NULL
  p_col <- if ("Pr(>F)" %in% names(table)) "Pr(>F)" else if ("Pr(>Chisq)" %in% names(table)) "Pr(>Chisq)" else NULL
  ms_col <- if ("Mean Sq" %in% names(table)) "Mean Sq" else NULL

  rows <- list()
  for (i in seq_len(nrow(table))) {
    row <- table[i, ]
    ss <- row$`Sum Sq`
    df1 <- row$Df
    ms <- if (!is.null(ms_col)) row[[ms_col]] else ss / df1
    f_val <- if (!is.null(f_col)) row[[f_col]] else if (!is.na(residual_df) && residual_df > 0) {
      ms / (residual_ss / residual_df)
    } else {
      NA_real_
    }
    p_val <- if (!is.null(p_col)) row[[p_col]] else NA_real_
    eta_sq <- if (!is.na(ss_total) && ss_total > 0) ss / ss_total else NA_real_
    partial_eta <- if (!is.na(residual_ss) && (ss + residual_ss) > 0) ss / (ss + residual_ss) else NA_real_
    omega_sq <- if (!is.na(ms_error) && !is.na(ss_total) && (ss_total + ms_error) > 0) {
      (ss - df1 * ms_error) / (ss_total + ms_error)
    } else {
      NA_real_
    }
    partial_omega <- if (!is.na(ms_error) && !is.na(residual_ss) && (ss + residual_ss + ms_error) > 0) {
      (ss - df1 * ms_error) / (ss + residual_ss + ms_error)
    } else {
      NA_real_
    }
    rows[[length(rows) + 1]] <- data.frame(
      model = "Between",
      term = row$term,
      df1 = df1,
      df2 = residual_df,
      ss = ss,
      ms = ms,
      f = f_val,
      p = p_val,
      eta_sq = eta_sq,
      partial_eta_sq = partial_eta,
      omega_sq = omega_sq,
      partial_omega_sq = partial_omega,
      df1_gg = NA_real_,
      df2_gg = NA_real_,
      p_gg = NA_real_,
      df1_hf = NA_real_,
      df2_hf = NA_real_,
      p_hf = NA_real_,
      stringsAsFactors = FALSE
    )
  }
  list(summary = do.call(rbind, rows), ss_total = ss_total, used_type = used_type)
}

extract_summary_table <- function(summary_obj) {
  if (is.data.frame(summary_obj)) return(summary_obj)
  if (is.list(summary_obj) && length(summary_obj) > 0) {
    if (is.data.frame(summary_obj[[1]])) return(summary_obj[[1]])
  }
  NULL
}

label_stratum <- function(stratum_name, subject_id, within_name) {
  if (is.null(stratum_name) || !nzchar(stratum_name)) return("")
  label <- sub("^Error:\\s*", "", stratum_name)
  if (!is.null(subject_id) && nzchar(subject_id)) {
    if (identical(label, subject_id)) return("Between")
    quoted_subject <- anova_names(subject_id)
    if (identical(label, substring(quoted_subject, 2L, nchar(quoted_subject) - 1L))) return("Between")
    if (identical(label, paste0(subject_id, ":", within_name))) return("Within")
    # Parse names only (never evaluate): escaped backticks are part of an ID,
    # whereas formula quotes are not. R sometimes omits quotes for bare strata.
    parsed <- tryCatch(str2lang(label), error = function(e) NULL)
    if (is.symbol(parsed) && identical(as.character(parsed), subject_id)) return("Between")
    if (is.call(parsed) && length(parsed) == 3L && identical(parsed[[1]], as.name(":")) &&
        identical(parsed[[2]], as.name(subject_id)) && identical(parsed[[3]], as.name(within_name))) return("Within")
  }
  label
}

extract_within_summary <- function(aov_fit, subject_id, within_name) {
  summaries <- summary(aov_fit)
  ss_total <- 0
  for (name in names(summaries)) {
    table <- extract_summary_table(summaries[[name]])
    if (is.null(table)) next
    if ("Sum Sq" %in% names(table)) {
      ss_total <- ss_total + sum(table$`Sum Sq`, na.rm = TRUE)
    }
  }

  rows <- list()
  for (name in names(summaries)) {
    table <- extract_summary_table(summaries[[name]])
    if (is.null(table)) next
    if (!"Sum Sq" %in% names(table)) next
    row_names <- trimws(rownames(table))
    resid_idx <- which(row_names == "Residuals")
    error_ss <- if (length(resid_idx) > 0) table$`Sum Sq`[resid_idx[1]] else NA_real_
    error_df <- if (length(resid_idx) > 0) table$Df[resid_idx[1]] else NA_real_
    ms_error <- if (!is.na(error_df) && error_df > 0) error_ss / error_df else NA_real_
    model_label <- label_stratum(name, subject_id, within_name)

    for (i in seq_len(nrow(table))) {
      term <- row_names[i]
      if (term == "Residuals" || term == "(Intercept)") next
      row <- table[i, ]
      ss <- row$`Sum Sq`
      df1 <- row$Df
      ms <- if ("Mean Sq" %in% names(row)) row$`Mean Sq` else ss / df1
      f_val <- if ("F value" %in% names(row)) row$`F value` else NA_real_
      p_val <- if ("Pr(>F)" %in% names(row)) row$`Pr(>F)` else NA_real_
      eta_sq <- if (!is.na(ss_total) && ss_total > 0) ss / ss_total else NA_real_
      partial_eta <- if (!is.na(error_ss) && (ss + error_ss) > 0) ss / (ss + error_ss) else NA_real_
      omega_sq <- if (!is.na(ms_error) && !is.na(ss_total) && (ss_total + ms_error) > 0) {
        (ss - df1 * ms_error) / (ss_total + ms_error)
      } else {
        NA_real_
      }
      partial_omega <- if (!is.na(ms_error) && !is.na(error_ss) && (ss + error_ss + ms_error) > 0) {
        (ss - df1 * ms_error) / (ss + error_ss + ms_error)
      } else {
        NA_real_
      }
      rows[[length(rows) + 1]] <- data.frame(
        model = model_label,
        term = term,
        df1 = df1,
        df2 = error_df,
        ss = ss,
        ms = ms,
        f = f_val,
        p = p_val,
        eta_sq = eta_sq,
        partial_eta_sq = partial_eta,
        omega_sq = omega_sq,
        partial_omega_sq = partial_omega,
        df1_gg = NA_real_,
        df2_gg = NA_real_,
        p_gg = NA_real_,
        df1_hf = NA_real_,
        df2_hf = NA_real_,
        p_hf = NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }
  list(summary = do.call(rbind, rows), ss_total = ss_total)
}

build_between_posthoc_tukey <- function(aov_fit, conf_level = 0.95) {
  tukey <- TukeyHSD(aov_fit, conf.level = conf_level)
  means <- model.tables(aov_fit, "means")$tables
  rows <- list()
  for (term in names(tukey)) {
    table <- as.data.frame(tukey[[term]])
    table$contrast <- rownames(table)
    for (i in seq_len(nrow(table))) {
      row <- table[i, ]
      dims <- dimnames(means[[term]])
      labels <- if (length(dims) == 1L) dims[[1]] else apply(expand.grid(dims, stringsAsFactors = FALSE), 1, paste, collapse = ":")
      pair_index <- which(lower.tri(matrix(0, length(labels), length(labels))), arr.ind = TRUE)
      group_1 <- labels[pair_index[i, 1]]
      group_2 <- labels[pair_index[i, 2]]
      rows[[length(rows) + 1]] <- data.frame(
        term = term,
        group = "",
        group_1 = group_1,
        group_2 = group_2,
        contrast = row$contrast,
        mean_diff = row$diff,
        se = NA_real_,
        t = NA_real_,
        df = NA_real_,
        p = NA_real_,
        p_adj = row$`p adj`,
        ci_low = row$lwr,
        ci_high = row$upr,
        method = "tukey",
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

anova_pairwise_row <- function(x, y, term, group, first, second, paired, conf_level) {
  test <- tryCatch(t.test(x, y, paired = paired, conf.level = conf_level), error = identity)
  unavailable <- inherits(test, "error") || !is.finite(test$p.value)
  reason <- if (inherits(test, "error")) conditionMessage(test) else if (unavailable) "Non-finite test inference." else ""
  if (unavailable) warning("Unavailable comparison ", term, " ", group, ": ", first, " versus ", second, ": ", reason)
  data.frame(term = term, group = group, group_1 = first, group_2 = second,
    contrast = paste(first, second, sep = "-"),
    mean_diff = mean(x) - mean(y), se = if (unavailable) NA_real_ else unname(test$stderr),
    t = if (unavailable) NA_real_ else unname(test$statistic),
    df = if (unavailable) NA_real_ else unname(test$parameter),
    p = if (unavailable) NA_real_ else test$p.value, p_adj = NA_real_,
    ci_low = if (unavailable) NA_real_ else test$conf.int[1],
    ci_high = if (unavailable) NA_real_ else test$conf.int[2],
    method = if (paired) "paired" else "pairwise",
    status = if (unavailable) "unavailable" else "available", reason = reason, stringsAsFactors = FALSE)
}

anova_adjust_family <- function(rows, p_adjust) {
  if (!length(rows)) return(data.frame())
  result <- do.call(rbind, rows)
  result$family_size <- nrow(result)
  result$p_adj <- p.adjust(result$p, method = p_adjust, n = nrow(result))
  result
}

build_between_posthoc_pairwise <- function(data_subset, dv, between_vars, p_adjust, conf_level) {
  families <- lapply(between_vars, function(var) {
    values <- unique(as.character(data_subset[[var]]))
    pairs <- combn(values, 2L, simplify = FALSE)
    rows <- lapply(pairs, function(pair) anova_pairwise_row(
      data_subset[[dv]][data_subset[[var]] == pair[1]],
      data_subset[[dv]][data_subset[[var]] == pair[2]],
      var, "", pair[1], pair[2], FALSE, conf_level))
    anova_adjust_family(rows, p_adjust)
  })
  if (!length(families)) return(data.frame())
  do.call(rbind, families)
}

build_within_posthoc_pairwise <- function(data_wide, within_vars, between_vars, conf_level, p_adjust) {
  pairs <- combn(within_vars, 2L, simplify = FALSE)
  combos <- if (length(between_vars)) unique(data_wide[, between_vars, drop = FALSE]) else data.frame(overall = 1L)
  families <- lapply(seq_len(nrow(combos)), function(i) {
    selected <- rep(TRUE, nrow(data_wide))
    for (var in between_vars) selected <- selected & data_wide[[var]] == combos[[var]][i]
    data <- data_wide[selected, , drop = FALSE]
    label <- if (length(between_vars)) paste(paste0(between_vars, "=", vapply(combos[i, between_vars, drop = FALSE], as.character, character(1))), collapse = ", ") else "Overall"
    rows <- lapply(pairs, function(pair) anova_pairwise_row(data[[pair[1]]], data[[pair[2]]],
      "within", label, pair[1], pair[2], TRUE, conf_level))
    family <- anova_adjust_family(rows, p_adjust)
    family$group_id <- i
    family
  })
  do.call(rbind, families)
}

compute_sphericity <- function(data, within_vars, between_vars, covariates) {
  between <- if (length(between_vars)) paste(anova_names(between_vars), collapse = " * ") else "1"
  rhs <- paste(c(between, anova_names(covariates)), collapse = " + ")
  response <- paste0("cbind(", paste(anova_names(within_vars), collapse = ", "), ")")
  fit <- lm(as.formula(paste(response, "~", rhs)), data = data, na.action = na.fail)
  test <- tryCatch(mauchly.test(fit, X = ~1), error = function(e) NULL)
  k <- length(within_vars) - 1L
  q <- qr.Q(qr(contr.helmert(k + 1L)))
  covariance <- crossprod(resid(fit) %*% q)
  eigenvalues <- eigen(covariance, symmetric = TRUE, only.values = TRUE)$values
  estimable <- df.residual(fit) >= k && min(eigenvalues) > max(eigenvalues) * .Machine$double.eps * (k + 1L)
  gg <- if (estimable) sum(eigenvalues)^2 / (k * sum(eigenvalues^2)) else NA_real_
  hf <- if (estimable) min(1, ((df.residual(fit) + 1) * k * gg - 2) / (k * (df.residual(fit) - k * gg))) else NA_real_
  note <- if (!estimable || is.null(test)) "Sphericity test/corrections unavailable: singular residual contrast covariance or insufficient subjects." else ""
  if (nzchar(note)) warning(note)
  list(w = if (!is.null(test) && estimable) unname(test$statistic) else NA_real_,
    p = if (!is.null(test) && estimable) test$p.value else NA_real_,
    epsilon_gg = gg, epsilon_hf = hf, error_df = df.residual(fit), model_rank = fit$rank, note = note)
}

build_assumptions <- function(mode, data_between, data_within, dv, between_vars, within_vars, covariates, subject_id, alpha, max_shapiro_n, sphericity) {
  rows <- list()

  if (mode == "between") {
    residuals <- resid(data_between$lm)
    shapiro <- safe_shapiro(residuals, max_n = max_shapiro_n)
    rows[[length(rows) + 1]] <- data.frame(
      assumption = "Normality",
      test = "Shapiro-Wilk",
      target = "Residuals",
      group = "",
      statistic = shapiro$w,
      df1 = NA_real_,
      df2 = NA_real_,
      p = shapiro$p,
      note = shapiro$note,
      stringsAsFactors = FALSE
    )

    if (length(between_vars) > 0) {
      group_factor <- interaction(data_between$data[, between_vars, drop = FALSE], drop = TRUE)
      levene <- calc_levene(data_between$data[[dv]], group_factor)
      rows[[length(rows) + 1]] <- data.frame(
        assumption = "Homogeneity",
        test = "Levene (median)",
        target = dv,
        group = "",
        statistic = levene$stat,
        df1 = levene$df1,
        df2 = levene$df2,
        p = levene$p,
        note = levene$note,
        stringsAsFactors = FALSE
      )
      bartlett <- calc_bartlett(data_between$data[[dv]], group_factor)
      rows[[length(rows) + 1]] <- data.frame(
        assumption = "Homogeneity",
        test = "Bartlett",
        target = dv,
        group = "",
        statistic = bartlett$stat,
        df1 = bartlett$df1,
        df2 = bartlett$df2,
        p = bartlett$p,
        note = bartlett$note,
        stringsAsFactors = FALSE
      )
      fligner <- calc_fligner(data_between$data[[dv]], group_factor)
      rows[[length(rows) + 1]] <- data.frame(
        assumption = "Homogeneity",
        test = "Fligner-Killeen",
        target = dv,
        group = "",
        statistic = fligner$stat,
        df1 = fligner$df1,
        df2 = fligner$df2,
        p = fligner$p,
        note = fligner$note,
        stringsAsFactors = FALSE
      )
    }
  }

  if (mode %in% c("within", "mixed")) {
    within_name <- data_within$within_name
    fixed <- if (length(between_vars)) paste(paste(anova_names(between_vars), collapse = " * "), "*", anova_names(within_name)) else anova_names(within_name)
    rhs <- paste(c(anova_names(subject_id), fixed, anova_names(covariates)), collapse = " + ")
    form <- as.formula(paste(anova_names(data_within$response_name), "~", rhs))
    lm_fit <- tryCatch(lm(form, data = data_within$long), error = function(e) NULL)
    if (!is.null(lm_fit)) {
      residuals <- resid(lm_fit)
      shapiro <- safe_shapiro(residuals, max_n = max_shapiro_n)
      rows[[length(rows) + 1]] <- data.frame(
        assumption = "Normality",
        test = "Shapiro-Wilk",
        target = "Residuals",
        group = "",
        statistic = shapiro$w,
        df1 = NA_real_,
        df2 = NA_real_,
        p = shapiro$p,
        note = shapiro$note,
        stringsAsFactors = FALSE
      )
    }

    if (length(between_vars) > 0) {
      group_factor <- interaction(data_within$wide[, between_vars, drop = FALSE], drop = TRUE)
      for (var in within_vars) {
        levene <- calc_levene(data_within$wide[[var]], group_factor)
        rows[[length(rows) + 1]] <- data.frame(
          assumption = "Homogeneity",
          test = "Levene (median)",
          target = var,
          group = "",
          statistic = levene$stat,
          df1 = levene$df1,
          df2 = levene$df2,
          p = levene$p,
          note = levene$note,
          stringsAsFactors = FALSE
        )
      }
    }

    if (sphericity == "auto" && length(within_vars) >= 3) {
      sph <- compute_sphericity(data_within$wide, within_vars, between_vars, covariates)
      rows[[length(rows) + 1]] <- data.frame(assumption = "Sphericity", test = "Mauchly",
        target = "Within", group = "", statistic = sph$w, df1 = NA_real_, df2 = NA_real_,
        p = sph$p, note = sph$note, stringsAsFactors = FALSE)
    }
  }

  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

summarize_assumptions <- function(assumptions_df, alpha) {
  if (is.null(assumptions_df) || nrow(assumptions_df) == 0) return("")
  violations <- assumptions_df[!is.na(assumptions_df$p) & assumptions_df$p < alpha, , drop = FALSE]
  missing <- assumptions_df[!is.finite(assumptions_df$p), , drop = FALSE]
  unavailable <- if (nrow(missing)) paste0(" Unavailable diagnostics: ", paste(paste0(missing$test, " (", missing$target, "): ", missing$note), collapse = "; "), ".") else ""
  if (nrow(violations) == 0) return(paste0("No violations detected among available diagnostics at alpha = ", alpha, ".", unavailable))
  labels <- unique(paste(violations$assumption, "(", violations$test, ")", sep = ""))
  paste0("Potential violations: ", paste(labels, collapse = "; "), ".", unavailable)
}

format_nlss_table <- function(summary_df, digits, note_text, effect_size, effect_size_label) {
  display <- summary_df
  display$term_display <- if ("term_label" %in% names(display)) display$term_label else display$term
  headers <- c("Model", "Effect", "df1", "df2", "F", "p", effect_size_label)
  rows <- list()
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    es <- get_effect_value(row, effect_size)
    rows[[length(rows) + 1]] <- c(
      row$model,
      row$term_display,
      format_num(row$df1, digits),
      format_num(row$df2, digits),
      format_stat(row$f, digits),
      format_p(row$p),
      format_stat(es, digits)
    )
  }
  table_md <- paste0("| ", paste(headers, collapse = " | "), " |\n")
  table_md <- paste0(table_md, "| ", paste(rep("---", length(headers)), collapse = " | "), " |\n")
  for (row in rows) {
    table_md <- paste0(table_md, "| ", paste(row, collapse = " | "), " |\n")
  }
  paste0("Table 1\n\n", table_md, "\n", note_text)
}

format_posthoc_table <- function(posthoc_df, digits, note_text) {
  display <- posthoc_df
  display$term_display <- if ("term_label" %in% names(display)) display$term_label else display$term
  display$group_1_display <- if ("group_1_label" %in% names(display)) display$group_1_label else display$group_1
  display$group_2_display <- if ("group_2_label" %in% names(display)) display$group_2_label else display$group_2
  headers <- c("Effect", "Group", "Group 1", "Group 2", "Mean diff", "t", "df", "p", "p_adj", "CI")
  rows <- list()
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    rows[[length(rows) + 1]] <- c(
      row$term_display,
      row$group,
      row$group_1_display,
      row$group_2_display,
      format_num(row$mean_diff, digits),
      format_stat(row$t, digits),
      format_num(row$df, digits),
      format_p(row$p),
      format_p(row$p_adj),
      format_ci(row$ci_low, row$ci_high, digits)
    )
  }
  table_md <- paste0("| ", paste(headers, collapse = " | "), " |\n")
  table_md <- paste0(table_md, "| ", paste(rep("---", length(headers)), collapse = " | "), " |\n")
  for (row in rows) {
    table_md <- paste0(table_md, "| ", paste(row, collapse = " | "), " |\n")
  }
  paste0("Table 1\n\n", table_md, "\n", note_text)
}

format_nlss_text <- function(summary_df, digits, effect_size, effect_size_label) {
  display <- summary_df
  display$term_display <- if ("term_label" %in% names(display)) display$term_label else display$term
  lines <- character(0)
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    if (is.na(row$f) || is.na(row$df1) || is.na(row$df2)) {
      lines <- c(lines, paste0(row$term_display, ": effect could not be computed."))
      next
    }
    es <- get_effect_value(row, effect_size)
    line <- sprintf(
      "%s: F(%s, %s) = %s, p %s, %s = %s.",
      row$term_display,
      format_num(row$df1, digits),
      format_num(row$df2, digits),
      format_stat(row$f, digits),
      format_p(row$p),
      effect_size_label,
      format_stat(es, digits)
    )
    if ("boot_valid" %in% names(row)) line <- paste0(line,
      " Bootstrap: ", row$boot_valid, " valid and ", row$boot_discarded,
      " discarded resamples; interval status: ", row$boot_ci_status, ".")
    lines <- c(lines, line)
  }
  paste(lines, collapse = "\n")
}

build_anova_table_body <- function(summary_df, digits, table_meta, effect_size) {
  display <- summary_df
  display$term_display <- if ("term_label" %in% names(display)) display$term_label else display$term
  default_specs <- list(
    list(key = "term", label = "Source"),
    list(key = "df", label = "df"),
    list(key = "ss", label = "SS", drop_if_empty = TRUE),
    list(key = "ms", label = "MS", drop_if_empty = TRUE),
    list(key = "f", label = "F", drop_if_empty = TRUE),
    list(key = "p", label = "p", drop_if_empty = TRUE),
    list(key = "partial_eta_sq", label = "Partial eta²", drop_if_empty = TRUE),
    list(key = "eta_sq", label = "eta²", drop_if_empty = TRUE),
    list(key = "partial_omega_sq", label = "Partial omega²", drop_if_empty = TRUE),
    list(key = "omega_sq", label = "omega²", drop_if_empty = TRUE),
    list(key = "boot_ci_low", label = "Boot CI low", drop_if_empty = TRUE),
    list(key = "boot_ci_high", label = "Boot CI high", drop_if_empty = TRUE)
  )
  columns <- normalize_table_columns(table_meta$columns, default_specs)
  rows <- list()
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    eta_val <- if (effect_size == "eta_sq") row$eta_sq else NA_real_
    partial_val <- if (effect_size == "partial_eta_sq") row$partial_eta_sq else NA_real_
    omega_val <- if (effect_size == "omega_sq") row$omega_sq else NA_real_
    partial_omega_val <- if (effect_size == "partial_omega_sq") row$partial_omega_sq else NA_real_
    row_map <- list(
      model = row$model,
      term = row$term_display,
      df = format_num(row$df1, digits),
      df1 = format_num(row$df1, digits),
      df2 = format_num(row$df2, digits),
      f = format_stat(row$f, digits),
      p = format_p(row$p),
      partial_eta_sq = format_stat(partial_val, digits),
      eta_sq = format_stat(eta_val, digits),
      partial_omega_sq = format_stat(partial_omega_val, digits),
      omega_sq = format_stat(omega_val, digits),
      boot_ci_low = format_stat(row$boot_ci_low, digits),
      boot_ci_high = format_stat(row$boot_ci_high, digits),
      ss = format_num(row$ss, digits),
      ms = format_num(row$ms, digits),
      df1_gg = format_num(row$df1_gg, digits),
      df2_gg = format_num(row$df2_gg, digits),
      p_gg = format_p(row$p_gg),
      df1_hf = format_num(row$df1_hf, digits),
      df2_hf = format_num(row$df2_hf, digits),
      p_hf = format_p(row$p_hf)
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

build_posthoc_table_body <- function(posthoc_df, digits, table_meta) {
  display <- posthoc_df
  display$term_display <- if ("term_label" %in% names(display)) display$term_label else display$term
  display$group_1_display <- if ("group_1_label" %in% names(display)) display$group_1_label else display$group_1
  display$group_2_display <- if ("group_2_label" %in% names(display)) display$group_2_label else display$group_2
  default_specs <- list(
    list(key = "term", label = "Effect", drop_if_empty = TRUE),
    list(key = "group", label = "Group", drop_if_empty = TRUE),
    list(key = "group_1", label = "Group 1", drop_if_empty = TRUE),
    list(key = "group_2", label = "Group 2", drop_if_empty = TRUE),
    list(key = "contrast", label = "Contrast", drop_if_empty = TRUE),
    list(key = "mean_diff", label = "Mean diff", drop_if_empty = TRUE),
    list(key = "se", label = "SE", drop_if_empty = TRUE),
    list(key = "t", label = "t", drop_if_empty = TRUE),
    list(key = "df", label = "df", drop_if_empty = TRUE),
    list(key = "p", label = "p", drop_if_empty = TRUE),
    list(key = "p_adj", label = "p_adj", drop_if_empty = TRUE),
    list(key = "ci_low", label = "CI low", drop_if_empty = TRUE),
    list(key = "ci_high", label = "CI high", drop_if_empty = TRUE)
  )
  columns <- normalize_table_columns(table_meta$columns, default_specs)
  rows <- list()
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    row_map <- list(
      term = row$term_display,
      group = row$group,
      group_1 = row$group_1_display,
      group_2 = row$group_2_display,
      contrast = row$contrast,
      mean_diff = format_num(row$mean_diff, digits),
      se = format_num(row$se, digits),
      t = format_stat(row$t, digits),
      df = format_num(row$df, digits),
      p = format_p(row$p),
      p_adj = format_p(row$p_adj),
      ci_low = format_stat(row$ci_low, digits),
      ci_high = format_stat(row$ci_high, digits)
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

build_anova_note_tokens <- function(type, effect_size_label, conf_level, posthoc, p_adjust, bootstrap, bootstrap_samples, assumption_note, contrast_note = "") {
  parts <- c(
    paste0("Sum of squares type ", type, "."),
    paste0("Effect size: ", effect_size_label, "."),
    paste0("Confidence level: ", round(conf_level * 100), "%."),
    ifelse(posthoc != "none", paste0("Post-hoc: ", posthoc, " (p adjust: ", if (posthoc == "tukey") "Tukey simultaneous" else p_adjust, ")."), ""),
    ifelse(bootstrap, paste0("Bootstrap CIs use ", bootstrap_samples, " resamples."), "")
  )
  parts <- parts[nzchar(parts)]
  note_default <- paste(parts, collapse = " ")
  if (nzchar(assumption_note)) {
    note_default <- paste(note_default, assumption_note)
  }
  if (nzchar(contrast_note)) {
    note_default <- paste(note_default, contrast_note)
  }
  list(note_default = note_default, assumption_note = assumption_note)
}

build_posthoc_note_tokens <- function(posthoc, p_adjust) {
  note_default <- if (posthoc == "none") "" else paste0("Post-hoc method: ", posthoc, ". P-value adjustment: ", if (posthoc == "tukey") "Tukey simultaneous" else p_adjust, ". Pairwise adjustment retains the full planned family including unavailable tests; paired/Welch CIs are pointwise.")
  list(note_default = note_default)
}

build_contrast_note_tokens <- function(contrast_label, p_adjust, conf_level, contrast_file = "", contrast_note = "") {
  if (contrast_label == "none") return(list(note_default = contrast_note))
  note <- paste0("Contrasts: ", contrast_label, ".")
  if (nzchar(contrast_file)) {
    note <- paste0(note, " File: ", basename(contrast_file), ".")
  }
  note <- paste0(note, " P-value adjustment: ", p_adjust, ".")
  note <- paste0(note, " Confidence level: ", round(conf_level * 100), "%.")
  if (nzchar(contrast_note)) {
    note <- paste(note, contrast_note)
  }
  list(note_default = note)
}

build_anova_narrative_rows <- function(summary_df, digits, effect_size, effect_size_label) {
  display <- summary_df
  display$term_display <- if ("term_label" %in% names(display)) display$term_label else display$term
  rows <- list()
  nlss_text <- format_nlss_text(display, digits, effect_size, effect_size_label)
  lines <- strsplit(nlss_text, "\n", fixed = TRUE)[[1]]
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    full_sentence <- if (i <= length(lines)) lines[i] else ""
    es <- get_effect_value(row, effect_size)
    rows[[length(rows) + 1]] <- list(
      full_sentence = full_sentence,
      model = row$model,
      term = row$term_display,
      df1 = format_num(row$df1, digits),
      df2 = format_num(row$df2, digits),
      f = format_stat(row$f, digits),
      p = format_p(row$p),
      effect_size_label = effect_size_label,
      effect_size_value = format_stat(es, digits),
      boot_ci = format_ci(row$boot_ci_low, row$boot_ci_high, digits),
      boot_ci_low = format_stat(row$boot_ci_low, digits),
      boot_ci_high = format_stat(row$boot_ci_high, digits)
    )
  }
  rows
}

build_anova_table_groups <- function(summary_df) {
  if (is.null(summary_df) || nrow(summary_df) == 0) return(list())
  labels <- unique(as.character(summary_df$model))
  labels <- labels[!is.na(labels)]
  preferred <- c("Between", "Within")
  ordered <- c(preferred[preferred %in% labels], labels[!labels %in% preferred])
  groups <- list()
  for (label in ordered) {
    group_df <- summary_df[summary_df$model == label, , drop = FALSE]
    if (nrow(group_df) == 0) next
    groups[[length(groups) + 1]] <- list(label = label, data = group_df)
  }
  groups
}

format_anova_section_label <- function(label) {
  if (is.null(label) || !nzchar(label)) return("ANOVA: Tests of Effects")
  if (label == "Between") return("ANOVA: Tests of Between-Subjects Effects")
  if (label == "Within") return("ANOVA: Tests of Within-Subjects Effects")
  paste0("ANOVA: Tests of Effects (", label, ")")
}

build_posthoc_narrative_rows <- function(posthoc_df, digits) {
  display <- posthoc_df
  display$term_display <- if ("term_label" %in% names(display)) display$term_label else display$term
  display$group_1_display <- if ("group_1_label" %in% names(display)) display$group_1_label else display$group_1
  display$group_2_display <- if ("group_2_label" %in% names(display)) display$group_2_label else display$group_2
  rows <- list()
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    line <- sprintf(
      "%s: %s vs %s, mean diff = %s, p %s.",
      row$term_display,
      row$group_1_display,
      row$group_2_display,
      format_num(row$mean_diff, digits),
      if (!is.na(row$p_adj)) format_p(row$p_adj) else format_p(row$p)
    )
    if ("status" %in% names(row) && row$status == "unavailable") line <- paste0(
      row$term_display, " ", row$group, ": ", row$group_1_display, " versus ",
      row$group_2_display, " unavailable: ", row$reason)
    rows[[length(rows) + 1]] <- list(
      full_sentence = line,
      term = row$term_display,
      group = row$group,
      group_1 = row$group_1_display,
      group_2 = row$group_2_display,
      mean_diff = format_num(row$mean_diff, digits),
      t = format_stat(row$t, digits),
      df = format_num(row$df, digits),
      p = format_p(row$p),
      p_adj = format_p(row$p_adj)
    )
  }
  rows
}

build_contrast_rows <- function(contrast_summary, term_label, p_adjust, method_label) {
  p_adj_vals <- if (p_adjust != "none") contrast_summary$p.value else NA_real_
  p_vals <- if (p_adjust == "none") contrast_summary$p.value else NA_real_
  method <- if (!is.null(method_label) && nzchar(method_label)) method_label else p_adjust
  data.frame(
    term = term_label,
    contrast = contrast_summary$contrast,
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

build_contrast_table_body <- function(contrast_df, digits, table_meta) {
  display <- contrast_df
  display$term_display <- if ("term_label" %in% names(display)) display$term_label else display$term
  default_specs <- list(
    list(key = "term", label = "Effect", drop_if_empty = TRUE),
    list(key = "contrast", label = "Contrast"),
    list(key = "estimate", label = "Estimate", drop_if_empty = TRUE),
    list(key = "se", label = "SE", drop_if_empty = TRUE),
    list(key = "df", label = "df", drop_if_empty = TRUE),
    list(key = "t", label = "t", drop_if_empty = TRUE),
    list(key = "p", label = "p", drop_if_empty = TRUE),
    list(key = "p_adj", label = "p_adj", drop_if_empty = TRUE),
    list(key = "ci_low", label = "CI low", drop_if_empty = TRUE),
    list(key = "ci_high", label = "CI high", drop_if_empty = TRUE),
    list(key = "method", label = "Method", drop_if_empty = TRUE)
  )
  columns <- normalize_table_columns(table_meta$columns, default_specs)
  rows <- list()
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    row_map <- list(
      term = row$term_display,
      contrast = row$contrast,
      estimate = format_num(row$estimate, digits),
      se = format_num(row$se, digits),
      df = format_num(row$df, digits),
      t = format_stat(row$t, digits),
      p = format_p(row$p),
      p_adj = format_p(row$p_adj),
      ci_low = format_stat(row$ci_low, digits),
      ci_high = format_stat(row$ci_high, digits),
      method = row$method
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

build_contrast_narrative_rows <- function(contrast_df, digits) {
  display <- contrast_df
  display$term_display <- if ("term_label" %in% names(display)) display$term_label else display$term
  rows <- list()
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    p_text <- if (!is.na(row$p_adj)) format_p(row$p_adj) else format_p(row$p)
    line <- sprintf(
      "%s: %s, estimate = %s, t(%s) = %s, p %s.",
      row$term_display,
      row$contrast,
      format_num(row$estimate, digits),
      format_num(row$df, digits),
      format_stat(row$t, digits),
      p_text
    )
    rows[[length(rows) + 1]] <- list(
      full_sentence = line,
      term = row$term_display,
      contrast = row$contrast,
      estimate = format_num(row$estimate, digits),
      se = format_num(row$se, digits),
      df = format_num(row$df, digits),
      t = format_stat(row$t, digits),
      p = format_p(row$p),
      p_adj = format_p(row$p_adj),
      ci = format_ci(row$ci_low, row$ci_high, digits)
    )
  }
  rows
}

main <- function() {
  opts <- nlss_run_options(commandArgs(trailingOnly = TRUE), "anova")

  if (!is.null(opts$help)) {
    print_usage()
    return(invisible(NULL))
  }

  if (parse_bool(opts$interactive, FALSE)) {
    opts <- modifyList(opts, interactive_options())
  }

  digits_default <- get_config_value("defaults.digits")
  log_default <- get_config_value("defaults.log")
  type_default <- get_config_value("modules.anova.type")
  effect_default <- get_config_value("modules.anova.effect_size")
  posthoc_default <- get_config_value("modules.anova.posthoc")
  emmeans_default <- get_config_value("modules.anova.emmeans")
  contrasts_default <- get_config_value("modules.anova.contrasts")
  p_adjust_default <- get_config_value("modules.anova.p_adjust")
  conf_default <- get_config_value("modules.anova.conf_level")
  sphericity_default <- get_config_value("modules.anova.sphericity")
  bootstrap_default <- get_config_value("modules.anova.bootstrap")
  bootstrap_samples_default <- get_config_value("modules.anova.bootstrap_samples")
  alpha_default <- get_config_value("modules.assumptions.alpha")
  max_shapiro_n <- get_config_value("modules.assumptions.max_shapiro_n")

  digits <- if (!is.null(opts$digits)) as.numeric(opts$digits) else digits_default
  df <- nlss_load_input(opts)
  out_dir <- get_workspace_out_dir(df)
  nlss_begin_run("anova", df, opts, out_dir)
  emit_input_issue <- function(out_dir, opts, message, details = list()) {
    nlss_run_context$request$validation_issue <- list(message = message, details = details)
    stop(message)
  }

  dv <- if (!is.null(opts$dv)) as.character(opts$dv) else ""
  between_vars <- parse_list(opts$between)
  within_vars <- parse_list(opts$within)
  subject_id <- if (!is.null(opts$`subject-id`)) as.character(opts$`subject-id`) else ""
  covariates <- parse_list(opts$covariates)

  has_between <- length(between_vars) > 0
  has_within <- length(within_vars) > 0
  if (has_within && nzchar(dv)) stop("Repeated/mixed designs use --within, not a separate --dv.")
  if (!has_within && nzchar(subject_id)) stop("--subject-id is only used by repeated/mixed designs.")

  if (!has_between && !has_within) {
    emit_input_issue(out_dir, opts, "Specify --between or --within for ANOVA.")
  }
  if (!has_within && (!nzchar(dv))) {
    emit_input_issue(out_dir, opts, "Between-subjects ANOVA requires --dv.")
  }
  if (has_within && !nzchar(subject_id)) {
    emit_input_issue(out_dir, opts, "Within-subjects ANOVA requires --subject-id.")
  }
  if (has_within && length(within_vars) < 2) {
    emit_input_issue(out_dir, opts, "Within-subjects ANOVA requires at least two variables in --within.")
  }

  overlap <- intersect(within_vars, c(between_vars, covariates, dv))
  if (length(overlap) > 0) {
    emit_input_issue(out_dir, opts, "Within variables cannot overlap with dv/between/covariates.", details = list(overlap = overlap))
  }

  emmeans_term <- normalize_emmeans(opts$emmeans, emmeans_default)
  contrast_file <- if (!is.null(opts$`contrast-file`)) as.character(opts$`contrast-file`) else ""
  contrasts_input <- normalize_contrasts(opts$contrasts, contrasts_default)
  contrast_spec <- if (!is.null(nlss_run_context$replay)) nlss_run_context$replay$request$design$contrast_spec else
    tryCatch(resolve_contrast_spec(contrasts_input, contrast_file), error = function(e) e)
  if (!is.null(contrast_spec$source)) contrast_spec$source <- basename(contrast_spec$source)
  contrast_file <- if (nzchar(contrast_file)) basename(contrast_file) else ""
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
  has_emmeans <- requireNamespace("emmeans", quietly = TRUE)
  contrast_note <- ""
  contrast_adjustment <- NULL
  emmeans_messages <- character(0)

  type <- normalize_type(opts$type, type_default)
  effect_size <- normalize_effect_size(opts$`effect-size`, effect_default)
  posthoc <- normalize_posthoc(opts$posthoc, posthoc_default)
  p_adjust <- if (!is.null(opts$`p-adjust`) && opts$`p-adjust` != "") opts$`p-adjust` else p_adjust_default
  conf_level <- if (!is.null(opts$`conf-level`)) as.numeric(opts$`conf-level`) else conf_default
  sphericity <- normalize_sphericity(opts$sphericity, sphericity_default)
  bootstrap <- parse_bool(opts$bootstrap, default = bootstrap_default)
  bootstrap_samples <- if (!is.null(opts$`bootstrap-samples`)) as.numeric(opts$`bootstrap-samples`) else bootstrap_samples_default
  check_number <- function(value, name, min, max, integer = FALSE, open = FALSE) {
    if (length(value) != 1L || !is.finite(value) || value < min || value > max ||
        (integer && value != floor(value)) || (open && (value == min || value == max)))
      stop("Invalid numeric option --", name, ".")
    value
  }
  check_number(digits, "digits", 0, 15, TRUE)
  check_number(conf_level, "conf-level", 0, 1, open = TRUE)
  check_number(bootstrap_samples, "bootstrap-samples", 2, .Machine$integer.max, TRUE)
  if (!p_adjust %in% c(p.adjust.methods, "tukey", "scheffe", "sidak", "mvt", "dunnettx")) stop("Invalid --p-adjust method.")
  roles <- c(if (!has_within) dv, between_vars, within_vars, covariates, if (has_within) subject_id)
  if (anyDuplicated(roles)) stop("Analysis variable roles must be distinct and must not contain duplicates.")
  seed <- nlss_run_seed(opts$seed, stochastic = bootstrap || contrasts_active || nzchar(emmeans_term))
  nlss_run_context$request$configuration$modules$assumptions <- list(alpha = alpha_default, max_shapiro_n = max_shapiro_n)

  mode <- if (has_within && has_between) "mixed" else if (has_within) "within" else "between"
  posthoc_used <- if (mode == "between") posthoc else if (posthoc == "none") "none" else "pairwise"
  if (posthoc_used == "pairwise" && !p_adjust %in% p.adjust.methods)
    stop("Pairwise post-hoc tests require a stats::p.adjust method; use --posthoc none for emmeans-only adjustments.")

  if (contrasts_active && !nzchar(emmeans_term)) {
    if (mode == "between" && length(between_vars) == 1) {
      emmeans_term <- between_vars[1]
    } else if (mode == "within" && length(within_vars) >= 1) {
      emmeans_term <- "within"
    } else {
      emit_input_issue(
        out_dir,
        opts,
        "Planned contrasts require --emmeans or a contrast term in the JSON spec.",
        details = list(mode = mode, between = between_vars, within = within_vars)
      )
    }
  }

  if (!contrasts_active) {
    contrast_label <- "none"
  }
  if ((contrasts_active || nzchar(emmeans_term)) && !has_emmeans) {
    stop("Planned contrasts require the 'emmeans' package.")
  }

  summary_df <- data.frame()
  posthoc_df <- data.frame()
  contrasts_df <- data.frame()
  emmeans_df <- data.frame()
  assumptions_df <- data.frame()
  used_type <- type
  data_between <- NULL
  data_within <- NULL
  contrast_fit <- NULL

  if (mode == "between") {
    data_between <- prepare_between_data(df, dv, between_vars, covariates)
    if (type == "III") for (var in between_vars) contrasts(data_between[[var]]) <- contr.sum(nlevels(data_between[[var]]))
    fits <- build_between_model(data_between, dv, between_vars, covariates)
    contrast_fit <- fits$lm
    summary_result <- extract_between_summary(fits$lm, type)
    summary_df <- summary_result$summary
    used_type <- summary_result$used_type

    if (posthoc_used != "none" && length(between_vars) > 0) {
      if (posthoc_used == "tukey") {
        posthoc_df <- build_between_posthoc_tukey(fits$aov, conf_level)
      } else {
        posthoc_df <- build_between_posthoc_pairwise(data_between, dv, between_vars, p_adjust, conf_level)
      }
    }

    assumptions_df <- build_assumptions(
      mode,
      data_between = list(data = data_between, lm = fits$lm),
      data_within = NULL,
      dv = dv,
      between_vars = between_vars,
      within_vars = within_vars,
      covariates = covariates,
      subject_id = subject_id,
      alpha = alpha_default,
      max_shapiro_n = max_shapiro_n,
      sphericity = sphericity
    )
  } else {
    data_within <- prepare_within_data(df, within_vars, subject_id, between_vars, covariates)
    fits <- build_within_model(data_within$long, subject_id, data_within$within_name, between_vars, covariates, data_within$response_name)
    contrast_fit <- fits$aov
    summary_result <- extract_within_summary(fits$aov, subject_id, data_within$within_name)
    summary_df <- summary_result$summary
    used_type <- "I"
    if (!is.null(opts$type) && type != "I") stop("Type II/III sums of squares are supported for between-subjects models only; use --type I for repeated/mixed ANOVA.")
    if (type != "I") warning("Repeated/mixed ANOVA uses sequential Type I sums of squares; the configured Type II/III default applies only to between-subjects analyses.")
    if (sphericity == "auto" && length(within_vars) >= 3) {
      sph <- compute_sphericity(data_within$wide, within_vars, between_vars, covariates)
      rows <- summary_df$model == "Within"
      summary_df$df1_gg[rows] <- summary_df$df1[rows] * sph$epsilon_gg
      summary_df$df2_gg[rows] <- summary_df$df2[rows] * sph$epsilon_gg
      summary_df$p_gg[rows] <- pf(summary_df$f[rows], summary_df$df1_gg[rows], summary_df$df2_gg[rows], lower.tail = FALSE)
      summary_df$df1_hf[rows] <- summary_df$df1[rows] * sph$epsilon_hf
      summary_df$df2_hf[rows] <- summary_df$df2[rows] * sph$epsilon_hf
      summary_df$p_hf[rows] <- pf(summary_df$f[rows], summary_df$df1_hf[rows], summary_df$df2_hf[rows], lower.tail = FALSE)
    }

    if (posthoc_used != "none") {
      posthoc_df <- build_within_posthoc_pairwise(data_within$wide, within_vars, between_vars, conf_level, p_adjust)
    }

    assumptions_df <- build_assumptions(
      mode,
      data_between = NULL,
      data_within = data_within,
      dv = "dv",
      between_vars = between_vars,
      within_vars = within_vars,
      covariates = covariates,
      subject_id = subject_id,
      alpha = alpha_default,
      max_shapiro_n = max_shapiro_n,
      sphericity = sphericity
    )
  }

  if (nzchar(emmeans_term) && has_emmeans && !is.null(contrast_fit)) {
    specs <- as.formula(paste("~", emmeans_term))
    emm <- emmeans::emmeans(contrast_fit, specs = specs)
    means_summary <- summary(emm, infer = c(TRUE, FALSE), level = conf_level)
    emmeans_messages <- attr(means_summary, "mesg")
    emmeans_df <- as.data.frame(means_summary)
    if (contrasts_active) {
      contrast_method <- tryCatch(build_contrast_method(contrast_spec, emm, emmeans_term), error = function(e) e)
      if (inherits(contrast_method, "error")) {
        emit_input_issue(
          out_dir,
          opts,
          contrast_method$message,
          details = list(contrasts = contrast_label, contrast_file = contrast_file)
        )
      }
      if (is.list(contrast_method$method) && any(!is.finite(unlist(contrast_method$method)))) stop("Custom contrast weights must be finite.")
      cont <- do.call(emmeans::contrast, c(list(emm, method = contrast_method$method), contrast_method$args))
      if (!is.null(cont)) {
        cont_summary <- summary(cont, infer = c(TRUE, TRUE), adjust = p_adjust, level = conf_level)
        contrast_adjustment <- list(requested = p_adjust, effective = attr(cont_summary, "adjust"), messages = attr(cont_summary, "mesg"))
        if (is.null(contrast_adjustment$effective)) contrast_adjustment$effective <- p_adjust
        contrast_note <- paste0("Requested contrast adjustment: ", p_adjust, "; effective: ", contrast_adjustment$effective, ". ",
          paste(contrast_adjustment$messages, collapse = "; "))
        contrasts_df <- build_contrast_rows(cont_summary, emmeans_term, contrast_adjustment$effective, contrast_label)
        contrasts_df$p_adjust_requested <- p_adjust
        contrasts_df$p_adjust_effective <- contrast_adjustment$effective
      } else {
        contrast_note <- "Planned contrasts could not be computed."
      }
    }
  }

  if (nrow(summary_df) == 0) stop("No ANOVA results could be computed.")
  if (any(!is.finite(summary_df$f) | !is.finite(summary_df$p) | summary_df$df2 <= 0))
    stop("ANOVA primary effects require estimable F tests with positive error degrees of freedom.")
  if (contrasts_active && (!nrow(contrasts_df) || any(!is.finite(contrasts_df$estimate) | !is.finite(contrasts_df$t))))
    stop("Requested contrasts are not estimable.")
  prepared <- if (mode == "between") data_between else data_within$wide
  source_rows <- attr(prepared, "source_rows")
  factors <- c(between_vars, if (has_within) subject_id)
  design <- list(mode = mode, included_rows = source_rows, excluded_rows = setdiff(seq_len(nrow(df)), source_rows),
    missing = "joint complete cases after explicit numeric conversion", contrast_spec = contrast_spec,
    factor_levels = lapply(prepared[factors], levels),
    factor_contrasts = lapply(prepared[between_vars], contrasts),
    factor_mapping = lapply(factors, function(var) {
      raw <- df[[var]][source_rows]
      ids <- as.integer(prepared[[var]])
      lapply(seq_along(levels(prepared[[var]])), function(i) list(level_id = i,
        label = levels(prepared[[var]])[i], raw_value = raw[which(ids == i)[1]], value_hex = anova_value_hex(raw[which(ids == i)[1]]),
        source_rows = source_rows[ids == i]))
    }),
    source_classes = lapply(df[roles], class),
    model_formula = paste(deparse(fits$formula), collapse = " "),
    model_matrix_columns = if (mode == "between") colnames(model.matrix(fits$lm)) else colnames(fits$model_matrix),
    model_rank = if (mode == "between") fits$lm$rank else qr(fits$model_matrix)$rank,
    effect_definitions = list(eta_sq = "SS_effect / centered total response SS",
      omega_sq = "(SS_effect - df_effect * MSE) / (centered total response SS + MSE)",
      partial_eta_sq = "SS_effect / (SS_effect + SS_error_stratum)",
      partial_omega_sq = "(SS_effect - df_effect * MSE) / (SS_effect + SS_error_stratum + MSE)"),
    bootstrap_unit = if (has_within) "complete subject rows, jointly across repeated measures; new factor ID per draw" else "complete rows jointly across all variables",
    posthoc_family = "all planned comparisons per between factor or within each between-cell, including unavailable tests in adjustment n; pairwise CIs unadjusted; Tukey simultaneous",
    within_levels = if (has_within) within_vars else NULL,
    subject_mapping = if (has_within) lapply(seq_along(source_rows), function(i) list(subject_index = i,
      source_row = source_rows[i], raw_id = df[[subject_id]][source_rows[i]], value_hex = anova_value_hex(df[[subject_id]][source_rows[i]]),
      analysis_id = as.character(prepared[[subject_id]][i]))) else NULL,
    sphericity = if (exists("sph", inherits = FALSE)) sph else NULL,
    error_strata = if (has_within) lapply(fits$aov, function(x) list(rank = x$rank, residual_df = x$df.residual)) else NULL)
  names(design$factor_mapping) <- factors
  if (nrow(emmeans_df)) {
    design$emmeans_grid <- as.data.frame(emm)
    design$emmeans_messages <- emmeans_messages
  }
  if (contrasts_active) design$contrast_method <- contrast_method
  design$contrast_adjustment <- contrast_adjustment
  resolved <- list(mode = mode, dv = if (!has_within) dv else NULL, between = between_vars,
    within = within_vars, subject_id = if (has_within) subject_id else NULL, covariates = covariates,
    type_requested = type, type = used_type, effect_size = effect_size, posthoc = posthoc_used,
    emmeans = emmeans_term, contrasts = contrast_label, p_adjust = p_adjust, conf_level = conf_level,
    sphericity = sphericity, bootstrap = bootstrap, bootstrap_samples = bootstrap_samples, seed = seed, digits = digits)
  nlss_resolve_request(resolved, design)

  effect_size_label <- format_effect_label(effect_size)
  summary_df$boot_ci_low <- NA_real_
  summary_df$boot_ci_high <- NA_real_
  if (isTRUE(bootstrap)) {
    term_ids <- build_term_ids(summary_df)
    if (mode == "between") {
      boot_vals <- bootstrap_effect_sizes_between(
        data_between,
        dv,
        between_vars,
        covariates,
        type,
        effect_size,
        bootstrap_samples,
        term_ids, summary_df$df1
      )
    } else {
      boot_vals <- bootstrap_effect_sizes_within(
        data_within,
        within_vars,
        between_vars,
        covariates,
        subject_id,
        effect_size,
        bootstrap_samples,
        term_ids, summary_df$df1
      )
    }
    summary_df <- apply_bootstrap_ci(summary_df, boot_vals, conf_level, bootstrap_samples)
  }
  summary_df$status <- "available"
  if (nrow(posthoc_df) && !"status" %in% names(posthoc_df)) posthoc_df$status <- "available"
  if (nrow(contrasts_df)) contrasts_df$status <- "available"
  label_meta <- resolve_label_metadata(df)
  summary_df <- add_term_label_column(summary_df, label_meta, term_col = "term")
  posthoc_df <- add_term_label_column(posthoc_df, label_meta, term_col = "term")
  posthoc_df <- add_value_label_column(posthoc_df, label_meta, var_col = "term", value_col = "group_1")
  posthoc_df <- add_value_label_column(posthoc_df, label_meta, var_col = "term", value_col = "group_2")
  contrasts_df <- add_term_label_column(contrasts_df, label_meta, term_col = "term")
  assumption_note <- summarize_assumptions(assumptions_df, alpha_default)
  note_tokens <- build_anova_note_tokens(
    used_type,
    effect_size_label,
    conf_level,
    posthoc_used,
    p_adjust,
    bootstrap,
    bootstrap_samples,
    assumption_note,
    contrast_note
  )
  if (length(nlss_run_context$warnings)) {
    warning_messages <- unique(vapply(nlss_run_context$warnings, function(x) x$message, character(1)))
    warning_messages <- render_paths_for_log(warning_messages, workspace_root = nlss_run_context$root)
    note_tokens$note_default <- paste(note_tokens$note_default, "Warnings:", paste(warning_messages, collapse = "; "))
  }

  nlss_report_path <- file.path(out_dir, "report_canonical.md")
  template_override <- resolve_template_override(opts$template, module = "anova")
  template_path <- if (!is.null(template_override)) {
    template_override
  } else {
    resolve_template_path("anova.default", "anova/default-template.md")
  }
  template_path <- nlss_freeze_template(template_path, "default")
  template_meta <- get_template_meta(template_path)

  analysis_flags <- list(
    mode = mode,
    dv = if (mode == "between") dv else NULL,
    between = if (has_between) between_vars else NULL,
    within = if (has_within) within_vars else NULL,
    "subject-id" = if (has_within) subject_id else NULL,
    covariates = if (length(covariates) > 0) covariates else NULL,
    type = used_type,
    "effect-size" = effect_size,
    posthoc = posthoc_used,
    emmeans = if (contrasts_active) emmeans_term else NULL,
    contrasts = if (contrasts_active) contrast_label else NULL,
    "contrast-file" = if (nzchar(contrast_file)) basename(contrast_file) else NULL,
    "p-adjust" = p_adjust,
    "conf-level" = conf_level,
    sphericity = if (has_within) sphericity else NULL,
    bootstrap = bootstrap,
    "bootstrap-samples" = if (bootstrap) bootstrap_samples else NULL,
    digits = digits
  )

  table_groups <- build_anova_table_groups(summary_df)
  if (length(table_groups) == 0) {
    table_groups <- list(list(label = "", data = summary_df))
  }
  for (group in table_groups) {
    group_df <- group$data
    analysis_label <- format_anova_section_label(group$label)
    nlss_text <- format_nlss_text(group_df, digits, effect_size, effect_size_label)
    nlss_table <- format_nlss_table(group_df, digits, note_tokens$note_default, effect_size, effect_size_label)
    table_result <- build_anova_table_body(group_df, digits, template_meta$table, effect_size)
    narrative_rows <- build_anova_narrative_rows(group_df, digits, effect_size, effect_size_label)
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
      analysis_label,
      nlss_table,
      nlss_text,
      analysis_flags = analysis_flags,
      template_path = template_path,
      template_context = template_context
    )
  }

  if (nrow(posthoc_df) > 0) {
    posthoc_note_tokens <- build_posthoc_note_tokens(posthoc_used, p_adjust)
    posthoc_nlss_table <- format_posthoc_table(posthoc_df, digits, posthoc_note_tokens$note_default)
    posthoc_template_path <- if (!is.null(template_override)) {
      template_override
    } else {
      resolve_template_path("anova.posthoc", "anova/posthoc-template.md")
    }
    posthoc_template_path <- nlss_freeze_template(posthoc_template_path, "posthoc")
    posthoc_meta <- get_template_meta(posthoc_template_path)
    posthoc_table <- build_posthoc_table_body(posthoc_df, digits, posthoc_meta$table)
    posthoc_narrative_rows <- build_posthoc_narrative_rows(posthoc_df, digits)
    posthoc_text <- paste(vapply(posthoc_narrative_rows, function(row) row$full_sentence, character(1)), collapse = "\n")
    posthoc_context <- list(
      tokens = c(
        list(
          table_body = posthoc_table$body,
          narrative_default = posthoc_text
        ),
        posthoc_note_tokens
      ),
      narrative_rows = posthoc_narrative_rows
    )
    nlss_stage_report(
      nlss_report_path,
      "ANOVA post-hoc",
      posthoc_nlss_table,
      posthoc_text,
      analysis_flags = analysis_flags,
      template_path = posthoc_template_path,
      template_context = posthoc_context
    )
  }

  if (nrow(emmeans_df)) {
    mean_rows <- lapply(seq_len(nrow(emmeans_df)), function(i)
      vapply(emmeans_df[i, , drop = FALSE], function(x) if (is.numeric(x)) format_num(x, digits) else as.character(x), character(1)))
    mean_body <- render_markdown_table(names(emmeans_df), mean_rows)
    mean_note <- paste0("Estimated marginal means; equal weighting over factor levels, covariates at reference-grid values. ",
      round(conf_level * 100), "% pointwise confidence intervals; grid and weights are saved in the request.")
    mean_note <- paste(mean_note, paste(emmeans_messages, collapse = "; "))
    nlss_stage_report(nlss_report_path, "ANOVA estimated marginal means",
      paste0("Table 1\n\n", mean_body, "\n", mean_note), mean_note, analysis_flags = analysis_flags)
  }

  if (nrow(contrasts_df) > 0) {
    contrast_note_tokens <- build_contrast_note_tokens(
      format_contrast_label(contrast_spec),
      contrast_adjustment$effective,
      conf_level,
      contrast_file,
      contrast_note
    )
    contrast_template_path <- if (!is.null(template_override)) {
      template_override
    } else {
      resolve_template_path("anova.contrasts", "anova/contrasts-template.md")
    }
    contrast_template_path <- nlss_freeze_template(contrast_template_path, "contrasts")
    contrast_meta <- get_template_meta(contrast_template_path)
    contrast_table <- build_contrast_table_body(contrasts_df, digits, contrast_meta$table)
    contrast_narrative_rows <- build_contrast_narrative_rows(contrasts_df, digits)
    contrast_text <- paste(vapply(contrast_narrative_rows, function(row) row$full_sentence, character(1)), collapse = "\n")
    contrast_nlss_table <- paste0("Table 1\n\n", contrast_table$body, "\n", contrast_note_tokens$note_default)
    contrast_context <- list(
      tokens = c(
        list(
          table_body = contrast_table$body,
          narrative_default = contrast_text
        ),
        contrast_note_tokens
      ),
      narrative_rows = contrast_narrative_rows
    )
    nlss_stage_report(
      nlss_report_path,
      "ANOVA contrasts",
      contrast_nlss_table,
      contrast_text,
      analysis_flags = analysis_flags,
      template_path = contrast_template_path,
      template_context = contrast_context
    )
  }

  cat("Wrote:\n")
  cat("- ", render_output_path(nlss_report_path, out_dir), "\n", sep = "")

  nlss_set_result(list(summary_df = summary_df, posthoc_df = posthoc_df, contrasts_df = contrasts_df, assumptions_df = assumptions_df, emmeans_df = emmeans_df, contrast_adjustment = contrast_adjustment))
  if (parse_bool(opts$log, default = log_default)) {
    ctx <- get_run_context()
    nlss_stage_log(
      out_dir,
      module = "anova",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(
        summary_df = summary_df,
        posthoc_df = posthoc_df,
        contrasts_df = contrasts_df,
        emmeans_df = emmeans_df,
        contrast_adjustment = contrast_adjustment,
        assumptions_df = assumptions_df
      ),
      options = list(
        mode = mode,
        dv = if (mode == "between") dv else NULL,
        between = if (has_between) between_vars else NULL,
        within = if (has_within) within_vars else NULL,
        subject_id = if (has_within) subject_id else NULL,
        covariates = if (length(covariates) > 0) covariates else NULL,
        type = used_type,
        effect_size = effect_size,
        posthoc = posthoc_used,
        emmeans = if (contrasts_active) emmeans_term else NULL,
        contrasts = if (contrasts_active) contrast_label else NULL,
        contrast_file = if (nzchar(contrast_file)) contrast_file else NULL,
        p_adjust = p_adjust,
        conf_level = conf_level,
        sphericity = if (has_within) sphericity else NULL,
        bootstrap = bootstrap,
        bootstrap_samples = if (bootstrap) bootstrap_samples else NULL,
        digits = digits
      ),
      user_prompt = get_user_prompt(opts)
    )
  }
}

nlss_run_main("anova", main)
