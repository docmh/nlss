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

print_usage <- function() {
  cat("Correlations (base R)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript correlations.R --csv data.csv [--vars var1,var2] [--group group_var]\n")
  cat("  Rscript correlations.R --csv data.csv --x var1,var2 --y var3,var4 [--group group_var]\n")
  cat("  Rscript correlations.R --sav data.sav [--vars var1,var2] [--group group_var]\n")
  cat("  Rscript correlations.R --rds data.rds [--vars var1,var2]\n")
  cat("  Rscript correlations.R --rdata data.RData --df data_frame_name [--vars var1,var2]\n")
  cat("  Rscript correlations.R --parquet data.parquet [--vars var1,var2]\n")
  cat("  Rscript correlations.R --interactive\n")
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
  cat("  --vars LIST            Comma-separated variables for a full matrix (default: all numeric)\n")
  cat("  --x LIST               Comma-separated X variables (for cross-correlation)\n")
  cat("  --y LIST               Comma-separated Y variables (for cross-correlation)\n")
  cat("  --group NAME           Grouping variable name (optional)\n")
  cat("  --method TYPE          pearson/spearman/kendall (default: pearson)\n")
  cat("  --missing TYPE         pairwise/complete (default: pairwise)\n")
  cat("  --alternative TYPE     two.sided/greater/less (default: two.sided)\n")
  cat("  --controls LIST        Comma-separated control variables for partial correlations\n")
  cat("  --p-adjust TYPE        none/bonferroni/holm/hochberg/hommel/BH/BY/fdr (default: none)\n")
  cat("  --conf-level VALUE     Confidence level for Fisher CI (default: 0.95)\n")
  cat("  --bootstrap TRUE/FALSE  Bootstrap confidence intervals (default: FALSE)\n")
  cat("  --bootstrap-samples N   Bootstrap resamples (default: 1000)\n")
  cat("  --seed N                Random seed for bootstrap (config default: 1)\n")
  cat("  --r0 VALUE             Fisher r-to-z test value (optional)\n")
  cat("  --compare-groups TRUE/FALSE Compare correlations between two groups (default: FALSE)\n")
  cat("  --coerce TRUE/FALSE    Coerce non-numeric vars to numeric (default: FALSE)\n")
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

  opts$vars <- prompt("Variables (comma-separated, blank for all numeric)", "")
  opts$x <- prompt("X variables (comma-separated, blank for none)", "")
  opts$y <- prompt("Y variables (comma-separated, blank for none)", "")
  opts$group <- prompt("Grouping variable (blank for none)", "")
  method_default <- get_config_value("modules.correlations.method")
  missing_default <- get_config_value("modules.correlations.missing")
  alternative_default <- get_config_value("modules.correlations.alternative")
  opts$method <- prompt("Method (pearson/spearman/kendall)", method_default)
  opts$missing <- prompt("Missing handling (pairwise/complete)", missing_default)
  opts$alternative <- prompt("Alternative (two.sided/greater/less)", alternative_default)
  opts$controls <- prompt("Control variables (comma-separated, blank for none)", "")
  adjust_default <- get_config_value("modules.correlations.p_adjust")
  conf_default <- get_config_value("modules.correlations.conf_level")
  bootstrap_default <- get_config_value("modules.correlations.bootstrap")
  bootstrap_samples_default <- get_config_value("modules.correlations.bootstrap_samples")
  compare_groups_default <- get_config_value("modules.correlations.compare_groups")
  coerce_default <- get_config_value("modules.correlations.coerce")
  digits_default <- get_config_value("defaults.digits")
  opts$`p-adjust` <- prompt(
    "P-value adjustment (none/bonferroni/holm/hochberg/hommel/BH/BY/fdr)",
    adjust_default
  )
  opts$`conf-level` <- prompt("Confidence level", as.character(conf_default))
  opts$bootstrap <- prompt("Bootstrap TRUE/FALSE", ifelse(isTRUE(bootstrap_default), "TRUE", "FALSE"))
  opts$`bootstrap-samples` <- prompt("Bootstrap samples", as.character(bootstrap_samples_default))
  opts$seed <- prompt("Bootstrap seed", as.character(get_config_value("modules.correlations.seed")))
  opts$r0 <- prompt("Fisher r-to-z test value (r0; optional)", "")
  opts$`compare-groups` <- prompt("Compare groups TRUE/FALSE", ifelse(isTRUE(compare_groups_default), "TRUE", "FALSE"))
  opts$coerce <- prompt("Coerce non-numeric TRUE/FALSE", ifelse(isTRUE(coerce_default), "TRUE", "FALSE"))
  opts$digits <- prompt("Rounding digits", as.character(digits_default))
  opts$template <- prompt("Template (path or key; blank for default)", "")
  opts$`user-prompt` <- prompt("User prompt (optional)", "")
  log_default <- get_config_value("defaults.log")
  opts$log <- prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))
  opts
}

normalize_method <- function(value) {
  val <- tolower(value)
  if (length(val) != 1L || is.na(val) || !nzchar(val)) stop("Supply one correlation method.")
  if (val %in% c("pearson", "r", "pearsonr")) return("pearson")
  if (val %in% c("spearman", "rho")) return("spearman")
  if (val %in% c("kendall", "tau")) return("kendall")
  stop("Unknown correlation method: ", val)
}

normalize_missing <- function(value) {
  val <- tolower(value)
  if (length(val) != 1L || is.na(val) || !nzchar(val)) stop("Supply one missing-data method.")
  if (val %in% c("pairwise", "pair")) return("pairwise")
  if (val %in% c("complete", "listwise")) return("complete")
  stop("Unknown missing-data method: ", val)
}

normalize_alternative <- function(value) {
  val <- tolower(value)
  if (length(val) != 1L || is.na(val) || !nzchar(val)) stop("Supply one alternative.")
  if (val %in% c("two.sided", "two-sided", "two")) return("two.sided")
  if (val %in% c("greater", "less")) return(val)
  stop("Unknown alternative: ", val)
}

normalize_adjust <- function(value) {
  val <- tolower(value)
  if (length(val) != 1L || is.na(val) || !nzchar(val)) stop("Supply one p-value adjustment.")
  if (val %in% c("none", "no", "false")) return("none")
  if (val %in% c("bonferroni", "holm", "hochberg", "hommel")) return(val)
  if (val %in% c("bh", "fdr")) return("BH")
  if (val %in% c("by")) return("BY")
  stop("Unknown p-value adjustment: ", val)
}

adjust_label <- function(method) {
  if (method == "BH") return("FDR (BH)")
  if (method == "BY") return("FDR (BY)")
  method
}

coerce_numeric <- function(vec) {
  if (is.numeric(vec)) return(vec)
  if (is.logical(vec)) return(as.numeric(vec))
  if (inherits(vec, "Date") || inherits(vec, "POSIXct") || inherits(vec, "POSIXlt")) {
    return(as.numeric(vec))
  }
  if (is.factor(vec)) return(as.numeric(as.character(vec)))
  if (is.character(vec)) return(as.numeric(vec))
  as.numeric(vec)
}

coerce_dataframe <- function(df, vars, coerce) {
  if (!coerce || length(vars) == 0) return(df)
  for (var in vars) {
    if (!is.numeric(df[[var]])) {
      original <- df[[var]]
      converted <- suppressWarnings(coerce_numeric(original))
      introduced_nas <- sum(is.na(converted) & !is.na(original))
      if (introduced_nas > 0) {
        warning(sprintf("Coercion introduced %s NA values for %s.", introduced_nas, var), call. = FALSE)
      }
      df[[var]] <- converted
    }
  }
  df
}

calc_skewness <- function(x) {
  n <- length(x)
  if (n < 3) return(NA_real_)
  mean_x <- mean(x)
  sd_x <- sd(x)
  if (is.na(sd_x) || sd_x == 0) return(NA_real_)
  sum(((x - mean_x) / sd_x)^3) * (n / ((n - 1) * (n - 2)))
}

calc_kurtosis <- function(x) {
  n <- length(x)
  if (n < 4) return(NA_real_)
  mean_x <- mean(x)
  sd_x <- sd(x)
  if (is.na(sd_x) || sd_x == 0) return(NA_real_)
  term1 <- (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3))
  term2 <- (3 * (n - 1)^2) / ((n - 2) * (n - 3))
  term1 * sum(((x - mean_x) / sd_x)^4) - term2
}

calc_ci <- function(r, n, conf_level, alternative = "two.sided", controls_n = 0L) {
  if (!is.finite(r) || n <= controls_n + 3L) return(c(NA_real_, NA_real_))
  se <- 1 / sqrt(n - controls_n - 3L)
  z <- atanh(r)
  if (alternative == "greater") return(c(tanh(z - qnorm(conf_level) * se), 1))
  if (alternative == "less") return(c(-1, tanh(z + qnorm(conf_level) * se)))
  tanh(z + c(-1, 1) * qnorm((1 + conf_level) / 2) * se)
}

bootstrap_ci_cor <- function(df_use, var1, var2, controls, method, conf_level,
                             n_boot, alternative = "two.sided", control_rank = 0L) {
  draws <- rep(NA_real_, n_boot)
  reasons <- character(n_boot)
  n <- nrow(df_use)
  for (i in seq_len(n_boot)) {
    sample_df <- df_use[sample.int(n, size = n, replace = TRUE), , drop = FALSE]
    values <- prepare_partial(sample_df, var1, var2, controls, method)
    if (values$control_rank != control_rank) {
      reasons[i] <- "changed_control_rank"
    } else if (!values$estimable) {
      reasons[i] <- "no_residual_variation"
    } else {
      draws[i] <- cor(values$x, values$y, method = if (length(controls)) "pearson" else method)
      if (!is.finite(draws[i])) reasons[i] <- "nonfinite_coefficient"
    }
  }
  valid <- is.finite(draws)
  interval <- c(NA_real_, NA_real_)
  if (sum(valid) >= 2L) {
    alpha <- 1 - conf_level
    interval <- switch(alternative,
      greater = c(unname(quantile(draws[valid], alpha, type = 7)), 1),
      less = c(-1, unname(quantile(draws[valid], 1 - alpha, type = 7))),
      unname(quantile(draws[valid], c(alpha / 2, 1 - alpha / 2), type = 7)))
  }
  list(ci = interval, valid = sum(valid), failed = sum(!valid),
    failure_reasons = as.list(table(reasons[nzchar(reasons)])),
    status = if (sum(valid) < 2L) "insufficient_valid_resamples" else if (all(valid)) "available" else "conditional_on_valid_resamples")
}

is_valid_fisher_r <- function(value) {
  !is.na(value) && is.finite(value) && abs(value) < 1
}

calc_z_p_value <- function(z_value, alternative) {
  if (is.na(z_value)) return(NA_real_)
  if (alternative == "greater") return(pnorm(z_value, lower.tail = FALSE))
  if (alternative == "less") return(pnorm(z_value))
  2 * pnorm(-abs(z_value))
}

fisher_z_test_r0 <- function(r, n, r0, alternative, controls_n = 0) {
  if (!is_valid_fisher_r(r) || !is_valid_fisher_r(r0) || is.na(n) || n <= (controls_n + 3)) {
    return(list(z = NA_real_, p = NA_real_))
  }
  z_r <- atanh(r)
  z_0 <- atanh(r0)
  se <- 1 / sqrt(n - controls_n - 3)
  z_val <- (z_r - z_0) / se
  list(z = z_val, p = calc_z_p_value(z_val, alternative))
}

fisher_z_test_independent <- function(r1, n1, r2, n2, alternative, controls_n = 0, controls_n2 = controls_n) {
  if (!is_valid_fisher_r(r1) || !is_valid_fisher_r(r2) || is.na(n1) || is.na(n2) ||
      n1 <= (controls_n + 3) || n2 <= (controls_n2 + 3)) {
    return(list(z = NA_real_, p = NA_real_))
  }
  z1 <- atanh(r1)
  z2 <- atanh(r2)
  se <- sqrt(1 / (n1 - controls_n - 3) + 1 / (n2 - controls_n2 - 3))
  z_val <- (z1 - z2) / se
  list(z = z_val, p = calc_z_p_value(z_val, alternative))
}

get_complete_rows <- function(df) {
  if (nrow(df) == 0) return(logical(0))
  idx <- complete.cases(df)
  if (any(idx)) {
    finite_idx <- rep(TRUE, nrow(df))
    for (col in names(df)) {
      finite_idx <- finite_idx & is.finite(df[[col]])
    }
    idx <- idx & finite_idx
  }
  idx
}

build_pairs <- function(vars) {
  if (length(vars) < 2) {
    return(data.frame(var1 = character(0), var2 = character(0), stringsAsFactors = FALSE))
  }
  combo <- combn(vars, 2)
  data.frame(var1 = combo[1, ], var2 = combo[2, ], stringsAsFactors = FALSE)
}

make_pair_key <- function(var1, var2) {
  paste0(nchar(pmin(var1, var2)), ":", pmin(var1, var2), pmax(var1, var2))
}

build_cross_pairs <- function(x_vars, y_vars) {
  if (length(x_vars) == 0 || length(y_vars) == 0) {
    return(build_pairs(unique(c(x_vars, y_vars))))
  }
  grid <- expand.grid(var1 = x_vars, var2 = y_vars, stringsAsFactors = FALSE)
  grid <- grid[grid$var1 != grid$var2, , drop = FALSE]
  if (nrow(grid) == 0) return(grid)
  key <- make_pair_key(grid$var1, grid$var2)
  grid <- grid[!duplicated(key), , drop = FALSE]
  grid
}

prepare_partial <- function(df_use, var1, var2, controls, method) {
  data <- df_use[, unique(c(var1, var2, controls)), drop = FALSE]
  if (length(controls) && method == "spearman") {
    data[] <- lapply(data, rank, ties.method = "average")
  }
  x <- data[[var1]]
  y <- data[[var2]]
  design <- cbind("(Intercept)" = rep(1, nrow(data)), as.matrix(data[, controls, drop = FALSE]))
  if (!nrow(data)) return(list(x = x, y = y, control_rank = 0L, design_rank = 0L,
    design_columns = colnames(design), aliased_controls = character(), estimable = FALSE))
  fit <- lm.fit(design, cbind(x, y))
  residuals <- if (length(controls)) fit$residuals else cbind(x, y)
  original_norm <- vapply(list(x, y), function(v) sqrt(sum((v - mean(v))^2)), numeric(1))
  residual_norm <- apply(residuals, 2, function(v) sqrt(sum((v - mean(v))^2)))
  estimable <- all(is.finite(residual_norm)) && all(original_norm > 0) &&
    all(residual_norm > sqrt(.Machine$double.eps) * original_norm)
  dropped <- if (fit$rank < ncol(design)) fit$qr$pivot[seq.int(fit$rank + 1L, ncol(design))] else integer()
  list(x = residuals[, 1], y = residuals[, 2], control_rank = max(0L, fit$rank - 1L),
    design_rank = fit$rank, design_columns = colnames(design),
    aliased_controls = colnames(design)[dropped], estimable = estimable)
}

correlation_test <- function(x, y, method, alternative, conf_level) {
  ties <- anyDuplicated(x) > 0L || anyDuplicated(y) > 0L
  n <- length(x)
  exact <- if (method == "kendall") !ties && n < 50L else !ties
  test <- stats::cor.test(x, y, method = method, alternative = alternative,
    conf.level = conf_level, exact = exact)
  inference <- if (method == "pearson") "Pearson_t" else if (method == "kendall") {
    if (exact) "Kendall_exact" else "Kendall_normal_approximation"
  } else if (exact && n < 10L) "Spearman_exact_AS89" else if (exact && n <= 1290L) {
    "Spearman_AS89_Edgeworth_approximation"
  } else "Spearman_t_approximation"
  list(r = unname(test$estimate), p = test$p.value,
    statistic = unname(test$statistic), df = if (is.null(test$parameter)) NA_real_ else unname(test$parameter),
    ci = if (is.null(test$conf.int)) c(NA_real_, NA_real_) else unname(test$conf.int),
    inference = inference, ties = ties,
    exact = (method == "kendall" && exact) || (method == "spearman" && exact && n < 10L))
}

compute_pair <- function(df_sub, var1, var2, group_label, method, alternative,
                         conf_level, missing_method, controls, complete_idx,
                         bootstrap, bootstrap_samples) {
  pair_vars <- unique(c(var1, var2, controls))
  selected <- if (missing_method == "complete") complete_idx else get_complete_rows(df_sub[, pair_vars, drop = FALSE])
  included_rows <- which(selected)
  df_use <- df_sub[included_rows, pair_vars, drop = FALSE]
  n <- nrow(df_use)
  values <- prepare_partial(df_use, var1, var2, controls, method)
  partial <- length(controls) > 0L
  residual_df <- n - values$control_rank - 2L
  status <- if (n < 3L) "insufficient_cases" else if (residual_df <= 0L) {
    "insufficient_residual_degrees_of_freedom"
  } else if (!values$estimable) "no_residual_variation" else "available"
  test <- list(r = NA_real_, p = NA_real_, statistic = NA_real_, df = residual_df,
    ci = c(NA_real_, NA_real_), inference = "unavailable", ties = FALSE, exact = FALSE)
  if (status == "available") {
    if (partial) {
      test$r <- cor(values$x, values$y)
      test$statistic <- test$r * sqrt(residual_df / ((1 - test$r) * (1 + test$r)))
      test$p <- switch(alternative, greater = pt(test$statistic, residual_df, lower.tail = FALSE),
        less = pt(test$statistic, residual_df), 2 * pt(-abs(test$statistic), residual_df))
      test$ci <- calc_ci(test$r, n, conf_level, alternative, values$control_rank)
      test$inference <- if (method == "pearson") "partial_Pearson_t" else "partial_Spearman_t_approximation"
      test$ties <- anyDuplicated(df_use[[var1]]) > 0L || anyDuplicated(df_use[[var2]]) > 0L
    } else {
      test <- correlation_test(values$x, values$y, method, alternative, conf_level)
    }
  }
  if (length(values$aliased_controls)) warning("Pair ", var1, " / ", var2,
    ": redundant controls; inference uses effective design rank ", values$design_rank, ".", call. = FALSE)
  if (status != "available") warning("Pair ", var1, " / ", var2, ": ", status, ".", call. = FALSE)
  boot <- list(ci = c(NA_real_, NA_real_), valid = 0L, failed = 0L,
    failure_reasons = list(), status = if (bootstrap) "estimate_unavailable" else "not_requested")
  if (bootstrap && status == "available") {
    boot <- bootstrap_ci_cor(df_use, var1, var2, controls, method, conf_level,
      bootstrap_samples, alternative, values$control_rank)
    if (boot$failed) warning("Pair ", var1, " / ", var2, ": ", boot$failed, " of ",
      bootstrap_samples, " bootstrap resamples were not estimable; ", boot$status, ".", call. = FALSE)
  }
  result <- data.frame(var1 = var1, var2 = var2, group = group_label, method = method,
    alternative = alternative, controls = paste(controls, collapse = ","), partial = partial,
    n = n, total_n = nrow(df_sub), missing_n = nrow(df_sub) - n,
    missing_pct = if (nrow(df_sub)) (nrow(df_sub) - n) / nrow(df_sub) * 100 else NA_real_,
    r = test$r, p_value = test$p, ci_low = test$ci[1], ci_high = test$ci[2],
    boot_ci_low = boot$ci[1], boot_ci_high = boot$ci[2], estimate_status = status,
    statistic = test$statistic, statistic_status = if (is.na(test$statistic)) "unavailable" else if (is.infinite(test$statistic)) {
      if (test$statistic > 0) "positive_infinite" else "negative_infinite"
    } else "finite",
    df = test$df, control_rank = values$control_rank, control_design_rank = values$design_rank,
    inference = test$inference, ties = test$ties, exact = test$exact,
    ci_status = if (all(is.finite(test$ci))) "available" else if (method != "pearson" && !partial) "not_implemented_for_rank_method" else "insufficient_cases_or_unavailable_estimate",
    bootstrap_valid = boot$valid, bootstrap_failed = boot$failed, bootstrap_status = boot$status,
    stringsAsFactors = FALSE)
  attr(result, "case_design") <- list(var1 = var1, var2 = var2, included_rows = included_rows,
    control_design = list(columns = values$design_columns, rank = values$design_rank,
      effective_controls = values$control_rank, aliased_columns = values$aliased_controls,
      rank_transform = partial && method == "spearman", rank_ties = "average", qr_tolerance = 1e-7),
    bootstrap_failure_reasons = boot$failure_reasons)
  result
}

build_diagnostics <- function(df_sub, vars, group_label) {
  rows <- list()
  total_n <- nrow(df_sub)
  for (var in vars) {
    vec <- df_sub[[var]]
    missing_n <- sum(is.na(vec))
    missing_pct <- ifelse(total_n > 0, missing_n / total_n * 100, NA_real_)
    valid <- vec[!is.na(vec)]
    n <- length(valid)

    shapiro_w <- NA_real_
    shapiro_p <- NA_real_
    if (n >= 3 && n <= 5000) {
      test <- tryCatch(shapiro.test(valid), error = function(e) NULL)
      if (!is.null(test)) {
        shapiro_w <- unname(test$statistic)
        shapiro_p <- test$p.value
      }
    }

    rows[[length(rows) + 1]] <- data.frame(
      variable = var,
      group = group_label,
      n = n,
      total_n = total_n,
      missing_n = missing_n,
      missing_pct = missing_pct,
      skewness = calc_skewness(valid),
      kurtosis = calc_kurtosis(valid),
      shapiro_w = shapiro_w,
      shapiro_p = shapiro_p,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

disambiguate_group_labels <- function(labels) {
  # Labels are for reading, never identity: distinct numeric/timestamp values
  # (or distinct codes with the same value label) can render identically.
  collisions <- duplicated(labels) | duplicated(labels, fromLast = TRUE)
  used <- labels
  for (i in which(collisions)) {
    suffix <- paste0(" [group ", i, "]")
    candidate <- paste0(labels[i], suffix)
    index <- 1L
    while (candidate %in% used) {
      index <- index + 1L
      candidate <- paste0(labels[i], " [group ", i, "-", index, "]")
    }
    labels[i] <- candidate
    used <- c(used, candidate)
  }
  labels
}


format_num <- function(value, digits) {
  if (is.na(value)) return("NA")
  format(round(value, digits), nsmall = digits, trim = TRUE)
}

format_r <- function(value, digits) {
  if (is.na(value)) return("NA")
  r_txt <- format(round(value, digits), nsmall = digits, trim = TRUE)
  sub("^(-?)0", "\\1", r_txt)
}

format_ci <- function(low, high, digits) {
  if (is.na(low) || is.na(high)) return("NA")
  paste0("[", format_r(low, digits), ", ", format_r(high, digits), "]")
}

format_p <- function(p_value) {
  if (is.na(p_value)) return("NA")
  if (p_value < 0.001) return("< .001")
  p_txt <- format(round(p_value, 3), nsmall = 3, trim = TRUE)
  p_txt <- sub("^0", "", p_txt)
  paste("=", p_txt)
}

format_p_matrix <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value < 0.001) return("< .001")
  p_txt <- format(round(p_value, 3), nsmall = 3, trim = TRUE)
  sub("^0", "", p_txt)
}

method_title <- function(method, partial) {
  base <- switch(method,
                 pearson = "Pearson",
                 spearman = "Spearman",
                 kendall = "Kendall",
                 method)
  if (partial) paste("Partial", base) else base
}

method_text <- function(method, partial) {
  base <- switch(method,
                 pearson = "Pearson's r",
                 spearman = "Spearman's rho",
                 kendall = "Kendall's tau",
                 method)
  if (partial) paste("partial", base) else base
}

format_ci_cell <- function(low, high, digits) {
  if (is.na(low) || is.na(high)) return("")
  format_ci(low, high, digits)
}

format_p_cell <- function(value) {
  if (is.na(value)) return("")
  format_p(value)
}

format_r_cell <- function(value, digits) {
  if (is.na(value)) return("")
  format_r(value, digits)
}

format_z_cell <- function(value, digits) {
  if (is.na(value)) return("")
  format_num(value, digits)
}

build_correlations_table_body <- function(summary_df, digits, conf_level, adjust_method, table_spec = NULL) {
  display <- summary_df
  display$group <- as.character(display$group)
  display$group[is.na(display$group)] <- "NA"
  display$var1_display <- if ("var1_label" %in% names(display)) display$var1_label else display$var1
  display$var2_display <- if ("var2_label" %in% names(display)) display$var2_label else display$var2
  display$group_display <- if ("group_label" %in% names(display)) display$group_label else display$group

  ci_label <- paste0(round(conf_level * 100), "% CI")
  default_columns <- list(
    list(key = "group", label = "Group", drop_if_empty = TRUE),
    list(key = "var1", label = "Variable 1"),
    list(key = "var2", label = "Variable 2"),
    list(key = "r", label = "r"),
    list(key = "r0", label = "r0", drop_if_empty = TRUE),
    list(key = "z_r0", label = "z (r0)", drop_if_empty = TRUE),
    list(key = "p_r0", label = "p (r0)", drop_if_empty = TRUE),
    list(key = "ci", label = ci_label, drop_if_empty = TRUE),
    list(key = "boot_ci", label = "Boot CI", drop_if_empty = TRUE),
    list(key = "p", label = "p"),
    list(key = "p_adj", label = "p_adj", drop_if_empty = TRUE),
    list(key = "n", label = "n")
  )
  columns <- normalize_table_columns(
    if (!is.null(table_spec$columns)) table_spec$columns else NULL,
    default_columns
  )

  rows <- list()
  for (i in seq_len(nrow(display))) {
    row <- display[i, , drop = FALSE]
    row_vals <- character(0)
    for (col in columns) {
      key <- col$key
      val <- ""
      if (key == "group") {
        val <- as_cell_text(row$group_display)
      } else if (key == "var1") {
        val <- as_cell_text(row$var1_display)
      } else if (key == "var2") {
        val <- as_cell_text(row$var2_display)
      } else if (key == "r") {
        val <- format_r_cell(row$r, digits)
      } else if (key == "r0") {
        val <- format_r_cell(row$r0, digits)
      } else if (key == "z_r0") {
        val <- format_z_cell(row$z_r0, digits)
      } else if (key == "p_r0") {
        val <- format_p_cell(row$p_r0)
      } else if (key == "ci") {
        val <- format_ci_cell(row$ci_low, row$ci_high, digits)
      } else if (key == "boot_ci") {
        val <- format_ci_cell(row$boot_ci_low, row$boot_ci_high, digits)
      } else if (key == "p") {
        val <- format_p_cell(row$p_value)
      } else if (key == "p_adj") {
        val <- format_p_cell(row$p_adjusted)
      } else if (key == "n") {
        val <- ifelse(is.na(row$n), "", as.character(row$n))
      } else if (key == "boot_ci_low") {
        val <- format_r_cell(row$boot_ci_low, digits)
      } else if (key == "boot_ci_high") {
        val <- format_r_cell(row$boot_ci_high, digits)
      } else if (key %in% names(row)) {
        cell <- row[[key]][1]
        if (is.numeric(cell)) {
          val <- format_num(cell, digits)
        } else {
          val <- as_cell_text(cell)
        }
      }
      row_vals <- c(row_vals, val)
    }
    rows[[length(rows) + 1]] <- row_vals
  }
  filtered <- drop_empty_columns(columns, rows)
  columns <- filtered$columns
  rows <- filtered$rows
  label_tokens <- list(ci_label = ci_label)
  headers <- vapply(columns, function(col) {
    label <- if (!is.null(col$label) && nzchar(col$label)) col$label else col$key
    render_template_tokens(label, label_tokens)
  }, character(1))
  render_markdown_table(headers, rows)
}

is_matrix_layout <- function(table_spec) {
  if (is.null(table_spec)) return(FALSE)
  if (!is.null(table_spec$layout)) {
    return(tolower(as.character(table_spec$layout)) == "matrix")
  }
  if (!is.null(table_spec$matrix)) return(isTRUE(table_spec$matrix))
  FALSE
}

resolve_matrix_diagonal <- function(table_spec, digits) {
  if (!is.null(table_spec$diagonal)) {
    diag_val <- as.character(table_spec$diagonal)
    if (!is.na(diag_val)) return(diag_val)
  }
  if (!is.null(table_spec$diag)) {
    diag_val <- as.character(table_spec$diag)
    if (!is.na(diag_val)) return(diag_val)
  }
  format_r(1, digits)
}

build_correlations_matrix_table_body <- function(summary_df, vars, digits, adjust_method, table_spec = NULL) {
  display <- summary_df
  display$group <- as.character(display$group)
  display$group[is.na(display$group)] <- "NA"
  display$var1_display <- if ("var1_label" %in% names(display)) display$var1_label else display$var1
  display$var2_display <- if ("var2_label" %in% names(display)) display$var2_label else display$var2
  display$group_display <- if ("group_label" %in% names(display)) display$group_label else display$group

  if (length(vars) == 0) {
    vars <- unique(c(display$var1, display$var2))
  }
  vars <- vars[vars != ""]
  vars <- vars[!is.na(vars)]
  if (length(vars) == 0) return("")

  label_map <- list()
  for (i in seq_len(nrow(display))) {
    v1 <- display$var1[i]
    v2 <- display$var2[i]
    l1 <- display$var1_display[i]
    l2 <- display$var2_display[i]
    if (!is.null(v1) && nzchar(v1) && !is.null(l1) && nzchar(l1)) label_map[[v1]] <- l1
    if (!is.null(v2) && nzchar(v2) && !is.null(l2) && nzchar(l2)) label_map[[v2]] <- l2
  }
  vars_display <- vapply(vars, function(v) {
    lbl <- label_map[[v]]
    if (is.null(lbl) || !nzchar(lbl)) v else lbl
  }, character(1))

  diag_value <- resolve_matrix_diagonal(table_spec, digits)
  groups <- unique(display$group_id)
  use_group_label <- length(groups) > 1 || any(nzchar(display$group))

  sections <- character(0)
  for (g in groups) {
    subset <- display[display$group_id == g, , drop = FALSE]
    key <- make_pair_key(subset$var1, subset$var2)
    header <- c("", vars_display)
    rows <- list()
    for (i in seq_along(vars)) {
      row_vals <- c(vars_display[i])
      for (j in seq_along(vars)) {
        cell <- ""
        if (i == j) {
          cell <- diag_value
        } else {
          row_idx <- match(make_pair_key(vars[i], vars[j]), key)
          if (!is.na(row_idx)) {
            row <- subset[row_idx, ]
            if (i > j) {
              cell <- format_r_cell(row$r, digits)
            } else {
              p_val <- if (adjust_method != "none" && !is.na(row$p_adjusted)) row$p_adjusted else row$p_value
              cell <- format_p_matrix(p_val)
            }
          }
        }
        row_vals <- c(row_vals, cell)
      }
      rows[[length(rows) + 1]] <- row_vals
    }
    table_md <- render_markdown_table(header, rows)
    if (use_group_label) {
      group_label <- if (nrow(subset) > 0) subset$group_display[1] else g
      table_md <- paste0("Group: ", group_label, "\n\n", table_md)
    }
    sections <- c(sections, table_md)
  }
  paste(sections, collapse = "\n\n")
}

build_correlations_note_tokens <- function(summary_df, conf_level, adjust_method, missing_method, alternative, bootstrap, bootstrap_samples) {
  display <- summary_df
  partial <- nrow(display) > 0 && any(display$partial)
  controls <- unique(display$controls[display$controls != ""])
  ci_label <- paste0(round(conf_level * 100), "% CI")
  tail_note <- if (alternative != "two.sided") "One-tailed tests." else "Two-tailed tests."
  missing_note <- paste0("Missing values handled ", missing_method, ".")
  partial_note <- ""
  if (partial && length(controls) > 0) {
    partial_note <- paste0("Partial correlations control for ", paste(controls, collapse = "; "),
      "; inference accounts for effective control rank. Partial Spearman inference is approximate.")
  }
  p_adjust_note <- ""
  if (adjust_method != "none") {
    p_adjust_note <- paste0("p-values adjusted using ", adjust_label(adjust_method),
      " across estimable requested pairs within each group; unavailable pairs are excluded from the family.")
  }
  ci_note <- ""
  if (nrow(display) > 0 && any(!is.na(display$ci_low))) {
    ci_note <- paste(ci_label, "computed via Fisher's z.")
  }
  r0_note <- ""
  method <- if (nrow(display) > 0) display$method[1] else ""
  r0_values <- unique(display$r0[!is.na(display$r0)])
  if (length(r0_values) > 0) {
    r0_text <- format(r0_values[1], trim = TRUE)
    r0_note <- paste0("Fisher r-to-z tests compare correlations against r0 = ", r0_text, ".")
    if (nzchar(method) && method != "pearson") {
      r0_note <- paste0(r0_note, " Approximation used for ", method, " correlations.")
    }
  }
  boot_note <- if (bootstrap) paste0("Paired-row percentile bootstrap CIs request ", bootstrap_samples,
    " resamples per pair (quantile type 7); one-sided intervals use the corresponding boundary at -1 or 1.") else ""
  if (bootstrap && any(display$bootstrap_failed > 0)) {
    boot_note <- paste0(boot_note, " Non-estimable resamples were excluded (",
      sum(display$bootstrap_failed), " across the requested pairs); affected intervals are conditional on valid draws, not a coverage guarantee.")
  }
  unavailable <- sum(display$estimate_status != "available")
  if (unavailable > 0) missing_note <- paste0(missing_note, " ", unavailable,
    " requested pair(s) have an explicitly unavailable coefficient; consult per-pair status.")
  parts <- c(tail_note, missing_note, partial_note, p_adjust_note, ci_note, boot_note, r0_note)
  note_default <- paste(parts[nzchar(parts)], collapse = " ")
  list(
    ci_label = ci_label,
    tail_note = tail_note,
    missing_note = missing_note,
    partial_note = partial_note,
    p_adjust_note = p_adjust_note,
    ci_note = ci_note,
    boot_note = boot_note,
    r0_note = r0_note,
    note_default = note_default
  )
}

build_correlations_comparison_table_body <- function(compare_df, digits, table_spec = NULL) {
  if (is.null(compare_df) || nrow(compare_df) == 0) return("")
  default_columns <- list(
    list(key = "group1", label = "Group 1"),
    list(key = "group2", label = "Group 2"),
    list(key = "var1", label = "Variable 1"),
    list(key = "var2", label = "Variable 2"),
    list(key = "r1", label = "r1"),
    list(key = "r2", label = "r2"),
    list(key = "n1", label = "n1"),
    list(key = "n2", label = "n2"),
    list(key = "z", label = "z"),
    list(key = "p", label = "p")
  )
  columns <- normalize_table_columns(
    if (!is.null(table_spec$columns)) table_spec$columns else NULL,
    default_columns
  )

  rows <- list()
  for (i in seq_len(nrow(compare_df))) {
    row <- compare_df[i, , drop = FALSE]
    row_vals <- character(0)
    for (col in columns) {
      key <- col$key
      val <- ""
      if (key == "group1") {
        val <- as_cell_text(row$group1)
      } else if (key == "group2") {
        val <- as_cell_text(row$group2)
      } else if (key == "var1") {
        val <- as_cell_text(row$var1)
      } else if (key == "var2") {
        val <- as_cell_text(row$var2)
      } else if (key == "r1") {
        val <- format_r_cell(row$r1, digits)
      } else if (key == "r2") {
        val <- format_r_cell(row$r2, digits)
      } else if (key == "n1") {
        val <- ifelse(is.na(row$n1), "", as.character(row$n1))
      } else if (key == "n2") {
        val <- ifelse(is.na(row$n2), "", as.character(row$n2))
      } else if (key == "z") {
        val <- format_z_cell(row$z, digits)
      } else if (key == "p") {
        val <- format_p_cell(row$p_value)
      } else if (key %in% names(row)) {
        cell <- row[[key]][1]
        if (is.numeric(cell)) {
          val <- format_num(cell, digits)
        } else {
          val <- as_cell_text(cell)
        }
      }
      row_vals <- c(row_vals, val)
    }
    rows[[length(rows) + 1]] <- row_vals
  }

  filtered <- drop_empty_columns(columns, rows)
  columns <- filtered$columns
  rows <- filtered$rows
  headers <- vapply(columns, function(col) {
    if (!is.null(col$label) && nzchar(col$label)) col$label else col$key
  }, character(1))
  render_markdown_table(headers, rows)
}

build_correlations_comparison_note_tokens <- function(method, alternative, missing_method) {
  tail_note <- if (alternative != "two.sided") "One-tailed tests." else "Two-tailed tests."
  missing_note <- paste0("Missing values handled ", missing_method, ".")
  method_note <- ""
  if (method != "pearson") {
    method_note <- paste0("Fisher r-to-z tests use an approximation for ", method, " correlations.")
  }
  parts <- c("Fisher r-to-z tests compare correlations between independent groups.", tail_note, missing_note, method_note)
  note_default <- paste(parts[nzchar(parts)], collapse = " ")
  list(
    tail_note = tail_note,
    missing_note = missing_note,
    method_note = method_note,
    note_default = note_default
  )
}

build_correlations_comparison_narrative_rows <- function(compare_df, digits) {
  if (is.null(compare_df) || nrow(compare_df) == 0) return(list())
  rows <- list()
  for (i in seq_len(nrow(compare_df))) {
    row <- compare_df[i, , drop = FALSE]
    var1_label <- if ("var1_label" %in% names(row)) row$var1_label else row$var1
    var2_label <- if ("var2_label" %in% names(row)) row$var2_label else row$var2
    group1_label <- if ("group1_label" %in% names(row)) row$group1_label else row$group1
    group2_label <- if ("group2_label" %in% names(row)) row$group2_label else row$group2
    label <- paste(var1_label, "with", var2_label)
    z_text <- format_z_cell(row$z, digits)
    p_text <- format_p_cell(row$p_value)
    n1_text <- ifelse(is.na(row$n1), "NA", as.character(row$n1))
    n2_text <- ifelse(is.na(row$n2), "NA", as.character(row$n2))
    if (is.na(row$z) || is.na(row$p_value)) {
      line <- paste0(
        label, ": comparison could not be computed (n1 = ", n1_text,
        ", n2 = ", n2_text, ")."
      )
    } else {
      line <- paste0(
        "Correlation between ", var1_label, " and ", var2_label,
        ": comparison of ", group1_label, " and ", group2_label,
        ", z = ", z_text, ", p ", p_text, "."
      )
    }
    rows[[length(rows) + 1]] <- list(
      label = label,
      group1 = as_cell_text(group1_label),
      group2 = as_cell_text(group2_label),
      var1 = as_cell_text(var1_label),
      var2 = as_cell_text(var2_label),
      r1 = format_r_cell(row$r1, digits),
      r2 = format_r_cell(row$r2, digits),
      n1 = n1_text,
      n2 = n2_text,
      z = z_text,
      p = p_text,
      full_sentence = line
    )
  }
  rows
}

build_correlations_narrative_rows <- function(summary_df, digits, conf_level, adjust_method) {
  display <- summary_df
  display$group <- as.character(display$group)
  display$group[is.na(display$group)] <- "NA"
  display$var1_display <- if ("var1_label" %in% names(display)) display$var1_label else display$var1
  display$var2_display <- if ("var2_label" %in% names(display)) display$var2_label else display$var2
  display$group_display <- if ("group_label" %in% names(display)) display$group_label else display$group
  rows <- list()
  for (i in seq_len(nrow(display))) {
    row <- display[i, , drop = FALSE]
    label <- if (row$group == "") {
      paste(row$var1_display, "with", row$var2_display)
    } else {
      paste("Group", row$group_display, ",", row$var1_display, "with", row$var2_display)
    }

    missing_pct_str <- ifelse(is.na(row$missing_pct), "NA", format_num(row$missing_pct, 1))
    missing_n_str <- ifelse(is.na(row$missing_n), "NA", as.character(row$missing_n))
    missing_text <- paste("Missing =", missing_n_str, "(", missing_pct_str, "%)", sep = " ")

    n_str <- ifelse(is.na(row$n), "NA", as.character(row$n))
    stat_text <- method_text(row$method, row$partial)
    r_text <- format_r_cell(row$r, digits)
    ci_text <- ""
    ci_only <- ""
    if (!is.na(row$ci_low) && !is.na(row$ci_high)) {
      ci_only <- format_ci(row$ci_low, row$ci_high, digits)
      ci_text <- paste0(", ", round(conf_level * 100), "% CI ", ci_only)
    }
    boot_ci_only <- ""
    boot_ci_text <- ""
    if (!is.na(row$boot_ci_low) && !is.na(row$boot_ci_high)) {
      boot_ci_only <- format_ci(row$boot_ci_low, row$boot_ci_high, digits)
      boot_ci_text <- paste0(", bootstrap CI ", boot_ci_only)
    }

    p_val <- if (adjust_method != "none" && !is.na(row$p_adjusted)) row$p_adjusted else row$p_value
    p_text <- format_p_cell(p_val)

    if (is.na(row$r) || is.na(row$n) || row$n < 3) {
      line <- sprintf(
        "%s: correlation could not be computed (n = %s). %s.",
        label,
        n_str,
        missing_text
      )
    } else {
      line <- paste0(
        label, ": ",
        stat_text, " = ", r_text,
        ci_text,
        boot_ci_text,
        ", p ", p_text,
        ", n = ", n_str,
        ". ", missing_text, "."
      )
    }
    availability_text <- if (row$estimate_status != "available") {
      paste0(" Estimate status: ", row$estimate_status, ".")
    } else if (row$bootstrap_status %in% c("conditional_on_valid_resamples", "insufficient_valid_resamples")) {
      paste0(" Bootstrap status: ", row$bootstrap_status, " (", row$bootstrap_valid,
        " valid, ", row$bootstrap_failed, " failed resamples).")
    } else ""
    line <- paste0(line, availability_text)

    rows[[length(rows) + 1]] <- list(
      label = label,
      group = as_cell_text(row$group_display),
      var1 = as_cell_text(row$var1_display),
      var2 = as_cell_text(row$var2_display),
      stat_text = stat_text,
      r = r_text,
      r0 = format_r_cell(row$r0, digits),
      z_r0 = format_z_cell(row$z_r0, digits),
      p_r0 = format_p_cell(row$p_r0),
      ci = ci_only,
      ci_text = ci_text,
      boot_ci = boot_ci_only,
      boot_ci_text = boot_ci_text,
      estimate_status = row$estimate_status,
      inference = row$inference,
      ci_status = row$ci_status,
      bootstrap_status = row$bootstrap_status,
      bootstrap_valid = row$bootstrap_valid,
      bootstrap_failed = row$bootstrap_failed,
      availability_text = availability_text,
      p = p_text,
      n = n_str,
      missing_n = missing_n_str,
      missing_pct = missing_pct_str,
      missing_text = missing_text,
      full_sentence = line
    )
  }
  rows
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  opts <- nlss_run_options(args, "correlations")

  if (!is.null(opts$help)) {
    print_usage()
    quit(status = 0)
  }

  if (parse_bool(opts$interactive, default = FALSE)) {
    opts <- modifyList(opts, interactive_options())
  }

  df <- nlss_load_input(opts)
  out_dir <- get_workspace_out_dir(df)
  nlss_begin_run("correlations", df, opts, out_dir)
  option <- function(key, config_key = key) {
    if (!is.null(opts[[key]])) opts[[key]] else get_config_value(paste0("modules.correlations.", config_key))
  }
  number <- function(value, label, min, max, integer = FALSE) {
    parsed <- suppressWarnings(as.numeric(value))
    if (is.logical(value) || length(parsed) != 1L || !is.finite(parsed) ||
        parsed < min || parsed > max || (integer && parsed != floor(parsed))) {
      stop(label, " is outside its supported numeric domain.")
    }
    parsed
  }
  digits <- number(if (!is.null(opts$digits)) opts$digits else get_config_value("defaults.digits"),
    "Digits", 0, 15, TRUE)
  conf_level <- number(option("conf-level", "conf_level"), "Confidence level", 0, 1)
  if (conf_level <= 0 || conf_level >= 1) stop("Confidence level must be strictly between 0 and 1.")
  bootstrap <- parse_bool(option("bootstrap"))
  bootstrap_samples <- number(option("bootstrap-samples", "bootstrap_samples"),
    "Bootstrap samples", 2, .Machine$integer.max, TRUE)
  seed <- nlss_run_seed(opts$seed, stochastic = bootstrap)
  compare_groups <- parse_bool(option("compare-groups", "compare_groups"))
  r0_input <- NULL
  if (isTRUE(opts$r0)) {
    stop("r0 requires a numeric value.")
  }
  if (!is.null(opts$r0) && !is.logical(opts$r0) && opts$r0 != "") r0_input <- opts$r0
  r0_value <- NULL
  if (!is.null(r0_input) && nzchar(r0_input)) {
    r0_value <- as.numeric(r0_input)
    if (length(r0_value) != 1L || !is.finite(r0_value)) stop("Invalid r0 value.")
    if (abs(r0_value) >= 1) stop("r0 must be between -1 and 1 (exclusive).")
  }
  method <- normalize_method(option("method"))
  missing_method <- normalize_missing(option("missing"))
  alternative <- normalize_alternative(option("alternative"))
  adjust_method <- normalize_adjust(option("p-adjust", "p_adjust"))
  coerce_flag <- parse_bool(option("coerce"))

  group_var <- if (!is.null(opts$group) && opts$group != "") opts$group else NULL
  if (compare_groups && is.null(group_var)) {
    stop("Group comparisons require --group.")
  }

  vars <- parse_list(opts$vars)
  x_vars <- parse_list(opts$x)
  y_vars <- parse_list(opts$y)
  controls <- parse_list(paste(unlist(option("controls"), use.names = FALSE), collapse = ","))
  if (length(vars) && (length(x_vars) || length(y_vars))) {
    stop("Choose either --vars or the --x/--y variable sets, not both.")
  }
  if (!is.null(group_var) && group_var %in% controls) stop("Grouping variable cannot also be a control.")
  for (selection in list(vars, x_vars, y_vars, controls)) {
    if (anyDuplicated(selection)) stop("Variable selections must not contain duplicates.")
  }

  if (length(x_vars) == 0 && length(y_vars) == 0 && length(vars) == 0) {
    vars <- setdiff(select_variables(df, NULL, group_var,
      default = get_config_value("modules.correlations.vars_default")), controls)
  }

  pairs <- NULL
  if (length(x_vars) > 0 || length(y_vars) > 0) {
    if (length(x_vars) == 0 || length(y_vars) == 0) {
      vars <- unique(c(x_vars, y_vars))
      pairs <- build_pairs(vars)
    } else {
      pairs <- build_cross_pairs(x_vars, y_vars)
    }
  } else {
    pairs <- build_pairs(vars)
  }

  if (nrow(pairs) == 0) stop("No variable pairs available for correlation analysis.")
  if (length(intersect(controls, c(pairs$var1, pairs$var2)))) stop("Controls must be distinct from correlated variables.")

  all_vars <- unique(c(pairs$var1, pairs$var2, controls, if (!is.null(group_var)) group_var))
  missing_vars <- setdiff(all_vars, names(df))
  if (length(missing_vars) > 0) {
    stop(paste("Unknown variables:", paste(missing_vars, collapse = ", ")))
  }

  if (length(controls) > 0 && method == "kendall") {
    stop("Partial correlations are not supported for Kendall's tau.")
  }
  if ((compare_groups || !is.null(r0_value)) && method == "kendall") {
    stop("Fisher r-to-z comparisons are not supported for Kendall's tau.")
  }

  analysis_vars <- unique(c(pairs$var1, pairs$var2, controls))
  original <- df[analysis_vars]
  variable_types <- lapply(df[all_vars], class)
  variable_levels <- lapply(df[all_vars], function(x) if (is.factor(x)) levels(x) else NULL)
  label_meta <- resolve_label_metadata(df)
  df <- coerce_dataframe(df, analysis_vars, coerce_flag)
  for (var in unique(c(pairs$var1, pairs$var2, controls))) {
    if (!is.numeric(df[[var]])) {
      stop(paste("Variable is not numeric:", var, "(use --coerce to convert)."))
    }
    if (any(!is.finite(df[[var]]) & !is.na(df[[var]]))) stop("Non-finite analysis values in ", var, ".")
  }

  group_vec <- if (is.null(group_var)) rep("", nrow(df)) else df[[group_var]]
  group_values <- if (is.null(group_var)) "" else unique(group_vec)
  missing_label <- nlss_missing_group_label(group_vec, label_meta, group_var)
  groups <- lapply(seq_along(group_values), function(i) {
    value <- group_values[i]
    missing <- is.na(value)
    rows <- if (missing) which(is.na(group_vec)) else which(!is.na(group_vec) & group_vec == value)
    list(group_id = i, group = if (missing) missing_label else as.character(value), is_missing = missing,
      value = if (is.null(group_var) || missing) NULL else if (is.factor(value)) as.character(value) else unname(value),
      value_hex = if (missing || is.null(group_var)) NULL else if (is.numeric(value) || inherits(value, c("Date", "POSIXt"))) {
        sprintf("%a", as.numeric(value))
      } else NULL,
      row_indices = rows,
      complete_case_rows = rows[get_complete_rows(df[rows, analysis_vars, drop = FALSE])])
  })
  if (!length(groups)) stop("No observed groups available for correlation analysis.")
  raw_groups <- vapply(groups, function(g) g$group, character(1))
  group_keys <- disambiguate_group_labels(raw_groups)
  group_labels <- disambiguate_group_labels(vapply(groups, function(g) {
    if (g$is_missing || is.null(group_var)) g$group else resolve_value_label(label_meta, group_var, g$group)
  }, character(1)))
  for (i in seq_along(groups)) {
    groups[[i]]$raw_group <- raw_groups[i]
    groups[[i]]$group <- group_keys[i]
    groups[[i]]$group_label <- group_labels[i]
  }
  if (compare_groups && sum(!vapply(groups, function(g) g$is_missing, logical(1))) != 2L) {
    stop("Group comparisons require exactly two non-missing group levels.")
  }
  summary_list <- diagnostics_list <- list()
  for (g in seq_along(groups)) {
    group <- groups[[g]]
    df_sub <- df[group$row_indices, , drop = FALSE]
    complete_idx <- get_complete_rows(df_sub[, analysis_vars, drop = FALSE])
    pair_cases <- list()
    for (i in seq_len(nrow(pairs))) {
      row <- pairs[i, ]
      result <- compute_pair(df_sub, row$var1, row$var2, group$group,
        method, alternative, conf_level, missing_method, controls, complete_idx,
        bootstrap, bootstrap_samples)
      case <- attr(result, "case_design")
      case$included_rows <- group$row_indices[case$included_rows]
      case$excluded_rows <- setdiff(group$row_indices, case$included_rows)
      pair_cases[[i]] <- case
      attr(result, "case_design") <- NULL
      result$group_id <- group$group_id
      result$group_missing <- group$is_missing
      summary_list[[length(summary_list) + 1L]] <- result
    }
    groups[[g]]$pairs <- pair_cases
    diagnostics <- build_diagnostics(df_sub, analysis_vars, group$group)
    diagnostics$group_id <- group$group_id
    diagnostics$group_missing <- group$is_missing
    diagnostics_list[[g]] <- diagnostics
  }
  summary_df <- do.call(rbind, summary_list)
  diagnostics_df <- do.call(rbind, diagnostics_list)
  resolved_options <- list(digits = digits, conf_level = conf_level, bootstrap = bootstrap,
    bootstrap_samples = bootstrap_samples, seed = seed, method = method, missing = missing_method,
    alternative = alternative, p_adjust = adjust_method, coerce = coerce_flag, r0 = r0_value,
    compare_groups = compare_groups, vars = vars, x = x_vars, y = y_vars, controls = controls, group = group_var)
  nlss_resolve_request(resolved_options, design = list(rows = nrow(df), variables = analysis_vars,
    pairs = pairs, groups = groups, variable_types = variable_types, variable_levels = variable_levels,
    analysis_types = lapply(df[analysis_vars], class),
    coercion = setNames(lapply(analysis_vars, function(v) list(
      applied = coerce_flag && !is.numeric(original[[v]]),
      introduced_missing_rows = which(!is.na(original[[v]]) & is.na(df[[v]])))), analysis_vars),
    partial = list(method = "OLS residual correlation; rank-transform all variables first for Spearman",
      df = "n - effective_control_rank - 2", fisher_se = "1 / sqrt(n - effective_control_rank - 3)"),
    bootstrap = list(method = "paired-row percentile", quantile_type = 7L,
      alternative = alternative, unavailable_draws = "exclude and disclose; interval conditional on estimable draws",
      changed_control_rank = "failed resample", minimum_valid_resamples = 2L),
    missing = list(correlations = missing_method, diagnostics = "variablewise",
      grouping = "missing grouping values form a separate group"),
    multiplicity = list(family = "estimable requested correlation tests within each group; unavailable pairs excluded",
      fisher_comparisons = "unadjusted")))
  if (!any(summary_df$estimate_status == "available")) stop("No requested correlation is estimable.")
  summary_df$missing_method <- missing_method
  summary_df$conf_level <- conf_level

  summary_df$p_adjusted <- NA_real_
  summary_df$p_adjust_method <- adjust_method
  if (adjust_method != "none") {
    for (g in unique(summary_df$group_id)) {
      idx <- summary_df$group_id == g & !is.na(summary_df$p_value)
      if (any(idx)) {
        summary_df$p_adjusted[idx] <- p.adjust(summary_df$p_value[idx], method = adjust_method)
      }
    }
  }

  summary_df$r0 <- if (!is.null(r0_value)) r0_value else NA_real_
  summary_df$z_r0 <- NA_real_
  summary_df$p_r0 <- NA_real_
  summary_df$r0_status <- if (is.null(r0_value)) "not_requested" else "unavailable"
  summary_df$fisher_inference <- if (method == "spearman") "Fisher_z_Spearman_approximation" else "Fisher_z_asymptotic"
  if (!is.null(r0_value) && nrow(summary_df) > 0) {
    for (i in seq_len(nrow(summary_df))) {
      test <- fisher_z_test_r0(summary_df$r[i], summary_df$n[i], r0_value, alternative, summary_df$control_rank[i])
      summary_df$z_r0[i] <- test$z
      summary_df$p_r0[i] <- test$p
      summary_df$r0_status[i] <- if (is.finite(test$z)) "available" else if (abs(summary_df$r[i]) == 1 && !is.na(summary_df$r[i])) "boundary_correlation" else "insufficient_cases_or_unavailable_estimate"
    }
  }

  comparison_df <- NULL
  if (compare_groups) {
    comparison_groups <- groups[!vapply(groups, function(g) g$is_missing, logical(1))]
    group1 <- comparison_groups[[1]]
    group2 <- comparison_groups[[2]]
    compare_rows <- list()
    for (i in seq_len(nrow(pairs))) {
      row <- pairs[i, ]
      row1 <- summary_df[summary_df$group_id == group1$group_id & summary_df$var1 == row$var1 & summary_df$var2 == row$var2, , drop = FALSE]
      row2 <- summary_df[summary_df$group_id == group2$group_id & summary_df$var1 == row$var1 & summary_df$var2 == row$var2, , drop = FALSE]
      r1 <- ifelse(nrow(row1) > 0, row1$r[1], NA_real_)
      r2 <- ifelse(nrow(row2) > 0, row2$r[1], NA_real_)
      n1 <- ifelse(nrow(row1) > 0, row1$n[1], NA_real_)
      n2 <- ifelse(nrow(row2) > 0, row2$n[1], NA_real_)
      test <- fisher_z_test_independent(r1, n1, r2, n2, alternative, row1$control_rank[1], row2$control_rank[1])
      compare_rows[[length(compare_rows) + 1]] <- data.frame(
        var1 = row$var1,
        var2 = row$var2,
        group1 = group1$group,
        group2 = group2$group,
        group1_id = group1$group_id, group2_id = group2$group_id,
        r1 = r1,
        r2 = r2,
        n1 = n1,
        n2 = n2,
        z = test$z,
        p_value = test$p,
        control_rank1 = row1$control_rank[1], control_rank2 = row2$control_rank[1],
        status = if (is.finite(test$z)) "available" else "boundary_or_insufficient_cases_or_unavailable_estimate",
        inference = if (method == "spearman") "Fisher_z_Spearman_approximation" else "Fisher_z_asymptotic",
        stringsAsFactors = FALSE
      )
    }
    comparison_df <- do.call(rbind, compare_rows)
  }
  summary_df <- add_variable_label_column(summary_df, label_meta, var_col = "var1")
  summary_df <- add_variable_label_column(summary_df, label_meta, var_col = "var2")
  diagnostics_df <- add_variable_label_column(diagnostics_df, label_meta, var_col = "variable")
  if (!is.null(group_var)) {
    summary_df$group_label <- group_labels[summary_df$group_id]
    diagnostics_df$group_label <- group_labels[diagnostics_df$group_id]
  }
  if (!is.null(comparison_df) && nrow(comparison_df) > 0) {
    comparison_df <- add_variable_label_column(comparison_df, label_meta, var_col = "var1")
    comparison_df <- add_variable_label_column(comparison_df, label_meta, var_col = "var2")
    comparison_df$group1_label <- group_labels[comparison_df$group1_id]
    comparison_df$group2_label <- group_labels[comparison_df$group2_id]
  }

  use_cross_template <- length(x_vars) > 0 && length(y_vars) > 0
  template_override <- resolve_template_override(opts$template, module = "correlations")
  template_path <- if (!is.null(template_override)) {
    template_override
  } else if (use_cross_template) {
    resolve_template_path("correlations.cross", "correlations/cross-correlation-template.md")
  } else {
    resolve_template_path("correlations.default", "correlations/default-template.md")
  }

  analysis_flags <- list(
    vars = if (length(x_vars) == 0 || length(y_vars) == 0) vars else NULL,
    x = if (length(x_vars) > 0) x_vars else NULL,
    y = if (length(y_vars) > 0) y_vars else NULL,
    group = if (!is.null(group_var) && group_var != "") group_var else "None",
    method = method,
    missing = missing_method,
    alternative = alternative,
    controls = if (length(controls) > 0) controls else "None",
    "p-adjust" = adjust_method,
    "conf-level" = conf_level,
    bootstrap = bootstrap,
    "bootstrap-samples" = if (bootstrap) bootstrap_samples else NULL,
    r0 = if (!is.null(r0_value)) r0_value else NULL,
    "compare-groups" = if (compare_groups) compare_groups else NULL,
    coerce = coerce_flag,
    digits = digits
  )

  nlss_report_path <- file.path(out_dir, "report_canonical.md")
  # Resolve the matrix/r0 incompatibility before copying one final template.
  # During replay only the saved template is authoritative.
  if (is.null(nlss_run_context$replay) && !is.null(r0_value) &&
      is_matrix_layout(get_template_meta(template_path)$table)) {
    template_path <- resolve_template_path("correlations.default", "correlations/default-template.md")
  }
  template_path <- nlss_freeze_template(template_path, "correlations.main")
  template_meta <- get_template_meta(template_path)
  table_spec <- if (!is.null(template_meta$table)) template_meta$table else NULL
  use_matrix <- is_matrix_layout(table_spec)
  vars_for_matrix <- if (length(vars) > 0) vars else unique(c(summary_df$var1, summary_df$var2))
  if (use_cross_template || !is.null(r0_value)) {
    use_matrix <- FALSE
  }
  table_body <- if (use_matrix && length(vars_for_matrix) >= 2) {
    build_correlations_matrix_table_body(summary_df, vars_for_matrix, digits, adjust_method, table_spec)
  } else {
    build_correlations_table_body(summary_df, digits, conf_level, adjust_method, table_spec)
  }
  note_tokens <- build_correlations_note_tokens(summary_df, conf_level, adjust_method, missing_method, alternative, bootstrap, bootstrap_samples)
  if (use_matrix) note_tokens$note_default <- paste(note_tokens$note_default,
    "The diagonal is a layout convention; pairwise deletion does not guarantee a positive-semidefinite joint matrix.")
  narrative_rows <- build_correlations_narrative_rows(summary_df, digits, conf_level, adjust_method)
  nlss_text <- paste(vapply(narrative_rows, function(row) row$full_sentence, character(1)), collapse = "\n")
  nlss_table <- paste0("Table 1\nCorrelations\n\n", table_body, "\n\nNote. ", note_tokens$note_default, "\n")
  template_context <- list(
    tokens = c(
      list(
        table_body = table_body,
        narrative_default = nlss_text
      ),
      note_tokens
    ),
    narrative_rows = narrative_rows
  )
  nlss_stage_report(
    nlss_report_path,
    "Correlations",
    nlss_table,
    nlss_text,
    analysis_flags = analysis_flags,
    template_path = template_path,
    template_context = template_context
  )

  if (!is.null(comparison_df) && nrow(comparison_df) > 0) {
    comparison_template_path <- resolve_template_path(
      "correlations.comparison",
      "correlations/comparison-template.md"
    )
    comparison_template_path <- nlss_freeze_template(comparison_template_path, "correlations.comparison")
    comparison_meta <- get_template_meta(comparison_template_path)
    comparison_table_body <- build_correlations_comparison_table_body(
      comparison_df,
      digits,
      comparison_meta$table
    )
    comparison_note_tokens <- build_correlations_comparison_note_tokens(method, alternative, missing_method)
    comparison_rows <- build_correlations_comparison_narrative_rows(comparison_df, digits)
    comparison_lines <- vapply(comparison_rows, function(row) row$full_sentence, character(1))
    comparison_text <- paste(comparison_lines, collapse = "\n")
    comparison_table <- paste0(
      "Table 1\nCorrelation comparisons\n\n",
      comparison_table_body,
      "\n\nNote. ",
      comparison_note_tokens$note_default,
      "\n"
    )
    comparison_context <- list(
      tokens = c(
        list(
          table_body = comparison_table_body,
          narrative_default = comparison_text
        ),
        comparison_note_tokens
      ),
      narrative_rows = comparison_rows
    )
    comparison_flags <- list(
      group = if (!is.null(group_var)) group_var else "None",
      method = method,
      missing = missing_method,
      alternative = alternative,
      controls = if (length(controls) > 0) controls else "None",
      "compare-groups" = compare_groups
    )
    nlss_stage_report(
      nlss_report_path,
      "Correlation comparisons",
      comparison_table,
      comparison_text,
      analysis_flags = comparison_flags,
      template_path = comparison_template_path,
      template_context = comparison_context
    )
  }

  results <- list(summary_df = summary_df, diagnostics_df = diagnostics_df, comparison_df = comparison_df)
  nlss_set_result(results)
  if (parse_bool(opts$log, default = get_config_value("defaults.log"))) {
    ctx <- get_run_context()
    nlss_stage_log(out_dir, module = "correlations", prompt = ctx$prompt, commands = ctx$commands,
      results = results, options = resolved_options, user_prompt = get_user_prompt(opts))
  }
}

nlss_run_main("correlations", main)
