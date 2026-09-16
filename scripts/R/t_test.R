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
  cat("t-tests (base R)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript t_test.R --csv data.csv --vars var1,var2 [--mu 0]\n")
  cat("  Rscript t_test.R --csv data.csv --vars var1,var2 --group group_var\n")
  cat("  Rscript t_test.R --csv data.csv --x var1,var2 --y var3,var4\n")
  cat("  Rscript t_test.R --sav data.sav --vars var1,var2 [--mu 0]\n")
  cat("  Rscript t_test.R --rds data.rds --vars var1,var2\n")
  cat("  Rscript t_test.R --rdata data.RData --df data_frame_name --vars var1,var2\n")
  cat("  Rscript t_test.R --parquet data.parquet --vars var1,var2\n")
  cat("  Rscript t_test.R --interactive\n")
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
  cat("  --vars LIST            Comma-separated variables (one-sample/independent)\n")
  cat("  --group NAME           Grouping variable (independent samples)\n")
  cat("  --x LIST               Comma-separated measure 1 variables (paired)\n")
  cat("  --y LIST               Comma-separated measure 2 variables (paired)\n")
  cat("  --mu VALUE             One-sample test value (default: 0.0)\n")
  cat("  --alternative TYPE     two.sided/greater/less (default: two.sided)\n")
  cat("  --var-equal TRUE/FALSE Assume equal variances (independent; default: FALSE)\n")
  cat("  --conf-level VALUE     Confidence level (default: 0.95)\n")
  cat("  --bootstrap TRUE/FALSE Bootstrap confidence intervals (default: FALSE)\n")
  cat("  --bootstrap-samples N  Bootstrap resamples (default: 1000)\n")
  cat("  --seed N               Bootstrap seed (default: modules.t_test.seed)\n")
  cat("  --digits N             Rounding digits (default: 2)\n")
  cat("  --expect-two-groups TRUE/FALSE  Informational output when group levels != 2 (default: FALSE)\n")
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

  test_type <- prompt("Test type (one-sample/independent/paired)", "one-sample")
  test_type <- tolower(test_type)
  if (!test_type %in% c("one-sample", "independent", "paired")) stop("Unsupported test type.")
  if (test_type == "paired") {
    opts$x <- prompt("Measure 1 variables (comma-separated)", "")
    opts$y <- prompt("Measure 2 variables (comma-separated)", "")
  } else {
    opts$vars <- prompt("Variables (comma-separated)", "")
    if (test_type == "independent") {
      opts$group <- prompt("Grouping variable", "")
    } else {
      mu_default <- get_config_value("modules.t_test.mu")
      opts$mu <- prompt("Test value (mu)", as.character(mu_default))
    }
  }

  alternative_default <- get_config_value("modules.t_test.alternative")
  conf_default <- get_config_value("modules.t_test.conf_level")
  var_equal_default <- get_config_value("modules.t_test.var_equal")
  bootstrap_default <- get_config_value("modules.t_test.bootstrap")
  bootstrap_samples_default <- get_config_value("modules.t_test.bootstrap_samples")
  digits_default <- get_config_value("defaults.digits")

  opts$alternative <- prompt("Alternative (two.sided/greater/less)", alternative_default)
  opts$`conf-level` <- prompt("Confidence level", as.character(conf_default))
  opts$`var-equal` <- prompt("Equal variances TRUE/FALSE", ifelse(isTRUE(var_equal_default), "TRUE", "FALSE"))
  opts$bootstrap <- prompt("Bootstrap TRUE/FALSE", ifelse(isTRUE(bootstrap_default), "TRUE", "FALSE"))
  opts$`bootstrap-samples` <- prompt("Bootstrap samples", as.character(bootstrap_samples_default))
  opts$seed <- prompt("Bootstrap seed", as.character(get_config_value("modules.t_test.seed")))
  opts$digits <- prompt("Rounding digits", as.character(digits_default))
  opts$template <- prompt("Template (path or key; blank for default)", "")
  opts$`user-prompt` <- prompt("User prompt (optional)", "")
  log_default <- get_config_value("defaults.log")
  opts$log <- prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))
  opts
}

normalize_alternative <- function(value, default) {
  val <- tolower(if (is.null(value) || identical(value, "")) default else value)
  if (val %in% c("two.sided", "two-sided", "two")) return("two.sided")
  if (val %in% c("greater", "less")) return(val)
  stop("Alternative must be two.sided, greater, or less.")
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

format_test_type <- function(value) {
  if (is.null(value) || !nzchar(value)) return("")
  if (value == "one_sample") return("One-sample")
  if (value == "independent") return("Independent")
  if (value == "paired") return("Paired")
  value
}

safe_shapiro <- function(values) {
  n <- length(values)
  reason <- if (n < 3 || n > 5000) "requires 3 to 5000 observations" else
    if (length(unique(values)) < 2) "constant sample" else ""
  if (nzchar(reason)) return(list(w = NA_real_, p = NA_real_, status = "unavailable", reason = reason))
  test <- stats::shapiro.test(values)
  list(w = unname(test$statistic), p = test$p.value, status = "available", reason = "")
}

calc_d_one_sample <- function(mean_diff, sd_val) {
  if (!is.finite(sd_val) || sd_val <= 0) return(NA_real_)
  mean_diff / sd_val
}

calc_d_paired <- calc_d_one_sample

calc_d_independent <- function(mean_diff, sd1, sd2, n1, n2) {
  if (n1 < 2 || n2 < 2 || !all(is.finite(c(sd1, sd2)))) return(NA_real_)
  pooled <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  calc_d_one_sample(mean_diff, pooled)
}

# Separate mean-difference and d resampling streams preserve the existing algorithm.
# Invalid standardized resamples (zero SD) are counted, never silently hidden.
bootstrap_interval <- function(statistics, conf_level, alternative) {
  valid <- is.finite(statistics)
  n_valid <- sum(valid)
  if (n_valid < 2) {
    warning("Bootstrap interval unavailable: fewer than two finite resample statistics.")
    return(list(low = NA_real_, high = NA_real_, valid = n_valid,
      discarded = sum(!valid), status = "unavailable"))
  }
  if (any(!valid)) warning(sum(!valid), " non-finite bootstrap statistics were excluded; valid/discarded counts are recorded.")
  values <- statistics[valid]
  bounds <- switch(alternative,
    two.sided = stats::quantile(values, c((1 - conf_level) / 2, (1 + conf_level) / 2), names = FALSE, type = 7),
    greater = c(stats::quantile(values, 1 - conf_level, names = FALSE, type = 7), Inf),
    less = c(-Inf, stats::quantile(values, conf_level, names = FALSE, type = 7)))
  list(low = bounds[1], high = bounds[2], valid = n_valid,
    discarded = sum(!valid), status = if (any(!valid)) "available_finite_resamples" else "available")
}

bootstrap_ci <- function(values, stat_fn, n_boot, conf_level, alternative = "two.sided") {
  statistics <- vapply(seq_len(n_boot), function(i) {
    stat_fn(values[sample.int(length(values), length(values), replace = TRUE)])
  }, numeric(1))
  bootstrap_interval(statistics, conf_level, alternative)
}

bootstrap_ci_independent <- function(x1, x2, stat_fn, n_boot, conf_level, alternative = "two.sided") {
  statistics <- vapply(seq_len(n_boot), function(i) {
    stat_fn(x1[sample.int(length(x1), length(x1), replace = TRUE)],
      x2[sample.int(length(x2), length(x2), replace = TRUE)])
  }, numeric(1))
  bootstrap_interval(statistics, conf_level, alternative)
}

ttest_bound_status <- function(value) {
  if (is.na(value)) "unavailable" else if (is.infinite(value))
    if (value > 0) "positive_infinity" else "negative_infinity" else "finite"
}

compute_ttest <- function(entry, mode, mu, alternative, var_equal, conf_level, bootstrap, bootstrap_samples) {
  x <- entry$x
  y <- entry$y
  n1 <- length(x)
  n2 <- length(y)
  if (n1 < 2 || (mode == "independent" && n2 < 2)) {
    stop("At least two complete observations per tested sample are required: ", entry$variable)
  }
  if (mode == "paired" && n1 != n2) stop("Internal paired-row alignment error.")
  test <- tryCatch(stats::t.test(x, y = if (mode == "one_sample") NULL else y,
    mu = if (mode == "one_sample") mu else 0, paired = mode == "paired",
    alternative = alternative, var.equal = var_equal, conf.level = conf_level),
    error = function(e) stop("t-test is not estimable for ", entry$variable, ": ", conditionMessage(e)))
  if (!all(is.finite(c(test$statistic, test$parameter, test$p.value))) ||
      anyNA(test$conf.int)) stop("t-test has non-finite primary statistics: ", entry$variable)
  mean1 <- mean(x)
  sd1 <- stats::sd(x)
  mean2 <- if (mode == "one_sample") NA_real_ else mean(y)
  sd2 <- if (mode == "one_sample") NA_real_ else stats::sd(y)
  differences <- if (mode == "paired") x - y else x
  mean_diff <- if (mode == "one_sample") mean1 - mu else if (mode == "paired") mean(differences) else mean1 - mean2
  d <- if (mode == "independent") calc_d_independent(mean_diff, sd1, sd2, n1, n2) else
    calc_d_one_sample(mean_diff, stats::sd(differences))
  if (!is.finite(d)) stop("Standardized mean difference is not estimable: ", entry$variable)
  empty_boot <- list(low = NA_real_, high = NA_real_, valid = 0L, discarded = 0L, status = "not_requested")
  boot_mean <- boot_d <- empty_boot
  if (bootstrap) {
    if (mode == "independent") {
      boot_mean <- bootstrap_ci_independent(x, y, function(a, b) mean(a) - mean(b),
        bootstrap_samples, conf_level, alternative)
      boot_d <- bootstrap_ci_independent(x, y, function(a, b)
        calc_d_independent(mean(a) - mean(b), stats::sd(a), stats::sd(b), length(a), length(b)),
        bootstrap_samples, conf_level, alternative)
    } else {
      null <- if (mode == "one_sample") mu else 0
      boot_mean <- bootstrap_ci(differences, function(a) mean(a) - null,
        bootstrap_samples, conf_level, alternative)
      boot_d <- bootstrap_ci(differences, function(a) calc_d_one_sample(mean(a) - null, stats::sd(a)),
        bootstrap_samples, conf_level, alternative)
    }
  }
  diagnostic_samples <- if (mode == "independent") list(x, y) else list(differences)
  diagnostics <- lapply(seq_along(diagnostic_samples), function(i) {
    shapiro <- safe_shapiro(diagnostic_samples[[i]])
    data.frame(test_type = mode, variable = entry$variable,
      group = if (mode == "independent") entry$groups[i] else "",
      group_id = if (mode == "independent") i else NA_integer_,
      n = length(diagnostic_samples[[i]]), shapiro_w = shapiro$w, shapiro_p = shapiro$p,
      shapiro_status = shapiro$status, shapiro_reason = shapiro$reason,
      var_test_f = NA_real_, var_test_p = NA_real_, var_test_status = "not_applicable",
      var_test_reason = "", stringsAsFactors = FALSE)
  })
  if (mode == "independent") {
    variance_test <- tryCatch(stats::var.test(x, y), error = identity)
    if (inherits(variance_test, "error")) {
      diagnostics[[2]]$var_test_status <- "unavailable"
      diagnostics[[2]]$var_test_reason <- conditionMessage(variance_test)
    } else {
      diagnostics[[2]]$var_test_f <- unname(variance_test$statistic)
      diagnostics[[2]]$var_test_p <- variance_test$p.value
      diagnostics[[2]]$var_test_status <- ttest_bound_status(unname(variance_test$statistic))
    }
  }
  ci <- unname(test$conf.int)
  diff_ci <- ci - if (mode == "one_sample") mu else 0
  row <- data.frame(test_type = mode, variable = entry$variable,
    measure_1 = if (mode == "paired") entry$variables[1] else "",
    measure_2 = if (mode == "paired") entry$variables[2] else "",
    group_1 = if (mode == "independent") entry$groups[1] else "",
    group_2 = if (mode == "independent") entry$groups[2] else "",
    group_1_id = if (mode == "independent") 1L else NA_integer_,
    group_2_id = if (mode == "independent") 2L else NA_integer_,
    n_1 = n1, n_2 = if (mode == "independent") n2 else NA_real_,
    mean_1 = mean1, mean_2 = mean2, sd_1 = sd1, sd_2 = sd2,
    mean_diff = mean_diff, t = unname(test$statistic), df = unname(test$parameter),
    p = test$p.value, d = d, ci_low = ci[1], ci_high = ci[2],
    diff_ci_low = diff_ci[1], diff_ci_high = diff_ci[2],
    ci_estimand = if (mode == "one_sample") "population_mean" else "population_mean_difference",
    diff_ci_estimand = if (mode == "one_sample") "population_mean_minus_mu" else "population_mean_difference",
    ci_low_status = ttest_bound_status(ci[1]), ci_high_status = ttest_bound_status(ci[2]),
    d_definition = if (mode == "paired") "dz_difference_sd" else if (mode == "independent") "pooled_sample_sd" else "sample_sd",
    boot_ci_low = boot_mean$low, boot_ci_high = boot_mean$high,
    boot_d_ci_low = boot_d$low, boot_d_ci_high = boot_d$high,
    boot_ci_status = boot_mean$status, boot_d_ci_status = boot_d$status,
    boot_ci_low_status = ttest_bound_status(boot_mean$low),
    boot_ci_high_status = ttest_bound_status(boot_mean$high),
    boot_d_ci_low_status = ttest_bound_status(boot_d$low),
    boot_d_ci_high_status = ttest_bound_status(boot_d$high),
    boot_valid = boot_mean$valid, boot_discarded = boot_mean$discarded,
    boot_d_valid = boot_d$valid, boot_d_discarded = boot_d$discarded,
    mu = if (mode == "one_sample") mu else NA_real_, null_value = if (mode == "one_sample") mu else 0,
    alternative = alternative, conf_level = conf_level,
    stderr = test$stderr, method = test$method, status = "estimated", stringsAsFactors = FALSE)
  list(summary = row, diagnostics = do.call(rbind, diagnostics))
}

format_nlss_table <- function(summary_df, digits, note_text) {
  body <- build_ttest_table_body(summary_df, digits, table_meta = list())$body
  paste0("Table 1\n\n", body, "\nNote. ", note_text)
}

format_nlss_text <- function(summary_df, digits, conf_level, alternative, var_equal) {
  display <- summary_df
  display$variable_display <- if ("variable_label" %in% names(display)) display$variable_label else display$variable
  display$measure_1_display <- if ("measure_1_label" %in% names(display)) display$measure_1_label else display$measure_1
  display$measure_2_display <- if ("measure_2_label" %in% names(display)) display$measure_2_label else display$measure_2
  display$group_1_display <- if ("group_1_label" %in% names(display)) display$group_1_label else display$group_1
  display$group_2_display <- if ("group_2_label" %in% names(display)) display$group_2_label else display$group_2
  lines <- character(0)
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    test_label <- switch(
      row$test_type,
      one_sample = "one-sample t-test",
      independent = "independent-samples t-test",
      paired = "paired-samples t-test",
      "t-test"
    )
    if (is.na(row$t) || is.na(row$df)) {
      line <- sprintf("%s: %s could not be computed (n = %s).",
                      row$variable_display,
                      test_label,
                      ifelse(is.na(row$n_1), "NA", as.character(row$n_1)))
      lines <- c(lines, line)
      next
    }

    ci_text <- format_ci(row$ci_low, row$ci_high, digits)
    p_text <- format_p(row$p)

    if (row$test_type == "one_sample") {
      line <- sprintf(
        "%s: %s against mu = %s, M = %s, SD = %s, t(%s) = %s, p %s, d = %s, %s%% CI %s.",
        row$variable_display,
        test_label,
        format_stat(row$mu, digits),
        format_num(row$mean_1, digits),
        format_num(row$sd_1, digits),
        format_num(row$df, digits),
        format_stat(row$t, digits),
        p_text,
        format_stat(row$d, digits),
        round(conf_level * 100),
        ci_text
      )
    } else if (row$test_type == "independent") {
      variance_text <- ifelse(var_equal, "equal variances assumed", "Welch correction")
      line <- sprintf(
        "%s: %s (%s; %s: M = %s, SD = %s, n = %s; %s: M = %s, SD = %s, n = %s), t(%s) = %s, p %s, d = %s, %s%% CI %s.",
        row$variable_display,
        test_label,
        variance_text,
        row$group_1_display,
        format_num(row$mean_1, digits),
        format_num(row$sd_1, digits),
        ifelse(is.na(row$n_1), "NA", as.character(row$n_1)),
        row$group_2_display,
        format_num(row$mean_2, digits),
        format_num(row$sd_2, digits),
        ifelse(is.na(row$n_2), "NA", as.character(row$n_2)),
        format_num(row$df, digits),
        format_stat(row$t, digits),
        p_text,
        format_stat(row$d, digits),
        round(conf_level * 100),
        ci_text
      )
    } else {
      line <- sprintf(
        "%s: %s, %s (M = %s, SD = %s) vs %s (M = %s, SD = %s), t(%s) = %s, p %s, d = %s, %s%% CI %s.",
        row$variable_display,
        test_label,
        row$measure_1_display,
        format_num(row$mean_1, digits),
        format_num(row$sd_1, digits),
        row$measure_2_display,
        format_num(row$mean_2, digits),
        format_num(row$sd_2, digits),
        format_num(row$df, digits),
        format_stat(row$t, digits),
        p_text,
        format_stat(row$d, digits),
        round(conf_level * 100),
        ci_text
      )
    }
    lines <- c(lines, line)
  }
  paste(lines, collapse = "\n")
}

build_ttest_table_body <- function(summary_df, digits, table_meta) {
  display <- summary_df
  display$variable_display <- if ("variable_label" %in% names(display)) display$variable_label else display$variable
  display$measure_1_display <- if ("measure_1_label" %in% names(display)) display$measure_1_label else display$measure_1
  display$measure_2_display <- if ("measure_2_label" %in% names(display)) display$measure_2_label else display$measure_2
  display$group_1_display <- if ("group_1_label" %in% names(display)) display$group_1_label else display$group_1
  display$group_2_display <- if ("group_2_label" %in% names(display)) display$group_2_label else display$group_2
  default_specs <- list(
    list(key = "test_type", label = "Test"),
    list(key = "variable", label = "Variable"),
    list(key = "measure_1", label = "Measure 1", drop_if_empty = TRUE),
    list(key = "measure_2", label = "Measure 2", drop_if_empty = TRUE),
    list(key = "group_1", label = "Group 1", drop_if_empty = TRUE),
    list(key = "group_2", label = "Group 2", drop_if_empty = TRUE),
    list(key = "n_1", label = "n1"),
    list(key = "n_2", label = "n2", drop_if_empty = TRUE),
    list(key = "mean_1", label = "M1"),
    list(key = "mean_2", label = "M2", drop_if_empty = TRUE),
    list(key = "sd_1", label = "SD1"),
    list(key = "sd_2", label = "SD2", drop_if_empty = TRUE),
    list(key = "mean_diff", label = "Mean diff"),
    list(key = "t", label = "t"),
    list(key = "df", label = "df"),
    list(key = "p", label = "p"),
    list(key = "d", label = "d"),
    list(key = "ci_low", label = "CI low", drop_if_empty = TRUE),
    list(key = "ci_high", label = "CI high", drop_if_empty = TRUE),
    list(key = "boot_ci_low", label = "Boot CI low", drop_if_empty = TRUE),
    list(key = "boot_ci_high", label = "Boot CI high", drop_if_empty = TRUE),
    list(key = "boot_d_ci_low", label = "Boot d CI low", drop_if_empty = TRUE),
    list(key = "boot_d_ci_high", label = "Boot d CI high", drop_if_empty = TRUE)
  )
  columns <- normalize_table_columns(table_meta$columns, default_specs)
  rows <- list()
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    row_map <- list(
      test_type = format_test_type(row$test_type),
      variable = row$variable_display,
      measure_1 = row$measure_1_display,
      measure_2 = row$measure_2_display,
      group_1 = row$group_1_display,
      group_2 = row$group_2_display,
      n_1 = ifelse(is.na(row$n_1), "", as.character(row$n_1)),
      n_2 = ifelse(is.na(row$n_2), "", as.character(row$n_2)),
      mean_1 = format_num(row$mean_1, digits),
      mean_2 = format_num(row$mean_2, digits),
      sd_1 = format_num(row$sd_1, digits),
      sd_2 = format_num(row$sd_2, digits),
      mean_diff = format_num(row$mean_diff, digits),
      t = format_stat(row$t, digits),
      df = format_num(row$df, digits),
      p = format_p(row$p),
      d = format_stat(row$d, digits),
      ci_low = format_stat(row$ci_low, digits),
      ci_high = format_stat(row$ci_high, digits),
      diff_ci_low = format_stat(row$diff_ci_low, digits),
      diff_ci_high = format_stat(row$diff_ci_high, digits),
      ci_estimand = row$ci_estimand,
      diff_ci_estimand = row$diff_ci_estimand,
      d_definition = row$d_definition,
      boot_ci_low = format_stat(row$boot_ci_low, digits),
      boot_ci_high = format_stat(row$boot_ci_high, digits),
      boot_d_ci_low = format_stat(row$boot_d_ci_low, digits),
      boot_d_ci_high = format_stat(row$boot_d_ci_high, digits)
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

build_ttest_note_tokens <- function(alternative, conf_level, var_equal, bootstrap, bootstrap_samples, mu_value, test_type) {
  alt_note <- ifelse(alternative == "two.sided", "Two-tailed tests.", "One-tailed tests.")
  ci_note <- paste0(round(conf_level * 100), "% confidence intervals.")
  var_note <- ifelse(test_type == "independent",
                     ifelse(var_equal, "Equal variances assumed.", "Welch correction used."),
                     "")
  boot_note <- ifelse(bootstrap, paste0("Percentile bootstrap CIs use ", bootstrap_samples,
    " resamples per statistic and the specified alternative; unavailable resamples are counted in the results."), "")
  mu_note <- ifelse(test_type == "one_sample", paste0("Test value mu = ", mu_value,
    ". Analytic CI bounds describe the population mean; mean diff and its bootstrap CI describe mean minus mu. Null-relative analytic bounds are also saved as diff_ci_low/high."),
    "Analytic and bootstrap mean-difference intervals describe the first measure/group minus the second; null difference is zero.")
  d_note <- if (test_type == "paired") "d denotes dz, standardized by the SD of paired differences." else
    if (test_type == "independent") "d uses the pooled sample SD, including for Welch tests." else "d uses the sample SD."
  parts <- c(alt_note, ci_note, var_note, mu_note, d_note, boot_note)
  parts <- parts[nzchar(parts)]
  note_default <- paste(parts, collapse = " ")
  list(note_default = note_default)
}

build_ttest_narrative_rows <- function(summary_df, digits, conf_level, alternative, var_equal) {
  display <- summary_df
  display$variable_display <- if ("variable_label" %in% names(display)) display$variable_label else display$variable
  display$measure_1_display <- if ("measure_1_label" %in% names(display)) display$measure_1_label else display$measure_1
  display$measure_2_display <- if ("measure_2_label" %in% names(display)) display$measure_2_label else display$measure_2
  display$group_1_display <- if ("group_1_label" %in% names(display)) display$group_1_label else display$group_1
  display$group_2_display <- if ("group_2_label" %in% names(display)) display$group_2_label else display$group_2
  rows <- list()
  nlss_text <- format_nlss_text(display, digits, conf_level, alternative, var_equal)
  lines <- strsplit(nlss_text, "\n", fixed = TRUE)[[1]]
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    full_sentence <- if (i <= length(lines)) lines[i] else ""
    rows[[length(rows) + 1]] <- list(
      full_sentence = full_sentence,
      test_type = format_test_type(row$test_type),
      variable = row$variable_display,
      measure_1 = row$measure_1_display,
      measure_2 = row$measure_2_display,
      group_1 = row$group_1_display,
      group_2 = row$group_2_display,
      n_1 = row$n_1,
      n_2 = row$n_2,
      mean_1 = format_num(row$mean_1, digits),
      mean_2 = format_num(row$mean_2, digits),
      sd_1 = format_num(row$sd_1, digits),
      sd_2 = format_num(row$sd_2, digits),
      mean_diff = format_num(row$mean_diff, digits),
      t = format_stat(row$t, digits),
      df = format_num(row$df, digits),
      p = format_p(row$p),
      d = format_stat(row$d, digits),
      ci = format_ci(row$ci_low, row$ci_high, digits),
      conf_level = round(conf_level * 100),
      mu = format_stat(row$mu, digits)
    )
  }
  rows
}

emit_input_issue <- function(out_dir, opts, message, details = list(), expected = FALSE) {
  nlss_run_context$request$validation_issue <- list(
    status = if (expected) "expected_invalid_input" else "invalid_input",
    message = message, details = details)
  if (expected) stop(structure(list(message = message, call = NULL),
    class = c("nlss_ttest_expected_groups", "error", "condition")))
  stop(message)
}

main <- function() {
  opts <- nlss_run_options(commandArgs(trailingOnly = TRUE), "t_test")
  if (!is.null(opts$help)) { print_usage(); return(invisible(NULL)) }
  if (parse_bool(opts$interactive, FALSE)) opts <- modifyList(opts, interactive_options())
  df <- nlss_load_input(opts)
  out_dir <- get_workspace_out_dir(df)
  nlss_begin_run("t_test", df, opts, out_dir)
  label_meta <- resolve_label_metadata(df)
  input_issue <- function(message, details = list(), expected = FALSE)
    emit_input_issue(out_dir, opts, message, details, expected)
  present <- function(value) !is.null(value) && !identical(value, "")
  has_group <- present(opts$group)
  has_x <- present(opts$x)
  has_y <- present(opts$y)
  if (has_group && (has_x || has_y)) input_issue("Paired tests do not use --group.")
  mode <- if (has_x || has_y) "paired" else if (has_group) "independent" else "one_sample"
  if (mode != "one_sample" && present(opts$mu)) input_issue("--mu is supported only for one-sample t-tests; paired and independent tests use a zero mean-difference null.")
  if (mode == "paired" && present(opts$vars)) input_issue("Paired tests use --x and --y, not --vars.")
  numeric_option <- function(flag, config, minimum = -Inf, maximum = Inf, integer = FALSE, open = FALSE) {
    value <- if (is.null(opts[[flag]])) get_config_value(config) else suppressWarnings(as.numeric(opts[[flag]]))
    if (length(value) != 1L || !is.finite(value) || value < minimum || value > maximum ||
        (open && (value == minimum || value == maximum)) || (integer && value != floor(value)))
      input_issue(paste("Invalid numeric option --", flag, ".", sep = ""))
    value
  }
  digits <- numeric_option("digits", "defaults.digits", 0, 15, TRUE)
  mu <- numeric_option("mu", "modules.t_test.mu")
  alternative <- tryCatch(normalize_alternative(opts$alternative, get_config_value("modules.t_test.alternative")),
    error = function(e) input_issue(conditionMessage(e)))
  var_equal <- parse_bool(opts$`var-equal`, get_config_value("modules.t_test.var_equal"))
  conf_level <- numeric_option("conf-level", "modules.t_test.conf_level", 0, 1, open = TRUE)
  bootstrap <- parse_bool(opts$bootstrap, get_config_value("modules.t_test.bootstrap"))
  bootstrap_samples <- numeric_option("bootstrap-samples", "modules.t_test.bootstrap_samples", 2, .Machine$integer.max, TRUE)
  expect_two_groups <- parse_bool(opts$`expect-two-groups`, FALSE)
  group_var <- if (has_group) opts$group else NULL
  group_values <- NULL
  group_order <- NULL
  if (mode == "independent") {
    if (!group_var %in% names(df)) input_issue("Grouping variable not found in data frame.")
    group_values <- unique(df[[group_var]][!is.na(df[[group_var]])])
    if (length(group_values) != 2) {
      input_issue(sprintf("Grouping variable '%s' has %d non-missing level(s); expected exactly two. Levels: %s.",
        group_var, length(group_values), paste(as.character(group_values), collapse = ", ")),
        list(group_var = group_var, level_count = length(group_values), levels = group_values),
        expected = expect_two_groups)
    }
    group_order <- lapply(seq_along(group_values), function(i)
      list(group_id = i, value = group_values[i],
        value_hex = if (is.numeric(group_values) || inherits(group_values, c("Date", "POSIXt"))) {
          sprintf("%a", as.numeric(group_values[i]))
        } else NULL,
        label = resolve_value_label(label_meta, group_var, as.character(group_values[i])),
        row_indices = which(!is.na(df[[group_var]]) & df[[group_var]] == group_values[i])))
    # Labels are presentation, never group identity. Distinct raw numeric or
    # sub-second time values, or duplicate user labels, can print identically.
    if (identical(group_order[[1]]$label, group_order[[2]]$label)) {
      for (i in seq_along(group_order)) group_order[[i]]$label <-
        paste0(group_order[[i]]$label, " [group ", i, "]")
    }
  }
  vars <- x_vars <- y_vars <- character(0)
  if (mode == "paired") {
    if (!(has_x && has_y)) input_issue("Paired t-tests require both --x and --y.")
    x_vars <- parse_list(opts$x)
    y_vars <- parse_list(opts$y)
    if (!length(x_vars) || !length(y_vars)) input_issue("Paired t-tests require --x and --y variables.")
    if (length(x_vars) != length(y_vars)) input_issue("--x and --y must have the same number of variables.")
    if (anyDuplicated(data.frame(x = x_vars, y = y_vars))) input_issue("Paired tests must not repeat the same variable pair.")
    if (any(x_vars == y_vars)) input_issue("Paired tests require distinct variables within each pair.")
    selected <- unique(c(x_vars, y_vars))
  } else {
    if (anyDuplicated(parse_list(opts$vars))) input_issue("Test variables must be distinct.")
    vars <- tryCatch(select_variables(df, opts$vars, group_var,
      default = get_config_value("modules.t_test.vars_default"), include_numeric = FALSE),
      error = function(e) input_issue(conditionMessage(e)))
    if (!length(vars)) input_issue("No numeric variables available for analysis.")
    selected <- vars
  }
  unknown <- setdiff(selected, names(df))
  if (length(unknown)) input_issue(paste("Unknown variables:", paste(unknown, collapse = ", ")))
  for (variable in selected) {
    if (!is.numeric(df[[variable]])) input_issue(paste("Variable is not numeric:", variable))
    if (any(!is.finite(df[[variable]]) & !is.na(df[[variable]])))
      input_issue(paste("Non-finite numeric observations are not supported:", variable))
  }
  entries <- tests <- list()
  count <- if (mode == "paired") length(x_vars) else length(vars)
  for (i in seq_len(count)) {
    variables <- if (mode == "paired") c(x_vars[i], y_vars[i]) else vars[i]
    complete <- stats::complete.cases(df[, variables, drop = FALSE])
    if (mode == "independent") complete <- complete & !is.na(df[[group_var]])
    rows <- which(complete)
    group_rows <- if (mode == "independent") lapply(group_order, function(g) intersect(rows, g$row_indices)) else NULL
    x <- if (mode == "independent") df[[variables[1]]][group_rows[[1]]] else df[[variables[1]]][rows]
    y <- if (mode == "independent") df[[variables[1]]][group_rows[[2]]] else
      if (mode == "paired") df[[variables[2]]][rows] else NULL
    name <- paste(variables, collapse = " - ")
    entries[[i]] <- list(x = x, y = y, variable = name, variables = variables,
      groups = as.character(group_values))
    tests[[i]] <- list(variable = name, variables = variables, row_indices = rows,
      excluded_row_indices = which(!complete), n_total = nrow(df), n_complete = length(rows),
      missing_by_variable = lapply(df[, variables, drop = FALSE], function(v) which(is.na(v))),
      group_row_indices = group_rows, paired_row_indices = if (mode == "paired") rows else NULL)
  }
  seed <- nlss_run_seed(opts$seed, stochastic = bootstrap)
  resolved_options <- list(mode = mode, vars = vars, x = x_vars, y = y_vars, group = group_var,
    mu = if (mode == "one_sample") mu else NULL, alternative = alternative,
    var_equal = if (mode == "independent") var_equal else NULL, conf_level = conf_level,
    bootstrap = bootstrap, bootstrap_samples = bootstrap_samples, seed = seed,
    digits = digits, expect_two_groups = expect_two_groups)
  nlss_resolve_request(resolved_options, design = list(rows = nrow(df), tests = tests,
    variables = selected, variable_classes = lapply(df[, unique(c(selected, group_var)), drop = FALSE], class),
    factor_levels = lapply(df[, unique(c(selected, group_var)), drop = FALSE], function(v) if (is.factor(v)) levels(v) else NULL),
    group_order = group_order, group_order_rule = if (mode == "independent") "first observed non-missing raw value" else NULL,
    missing_group_row_indices = if (mode == "independent") which(is.na(df[[group_var]])) else NULL,
    missing = "Complete observations per test; paired values are retained or excluded together.",
    null_value = if (mode == "one_sample") mu else 0,
    mean_diff_estimand = if (mode == "one_sample") "population_mean_minus_mu" else "population_mean_difference",
    ci_estimand = if (mode == "one_sample") "population_mean" else "population_mean_difference",
    diff_ci_estimand = if (mode == "one_sample") "population_mean_minus_mu" else "population_mean_difference",
    effect_size = if (mode == "paired") "dz: mean(x-y)/sd(x-y)" else if (mode == "independent")
      "d: (mean(group1)-mean(group2))/pooled sample SD, also for Welch tests" else "d: (mean(x)-mu)/sd(x)",
    bootstrap = if (bootstrap) list(method = "percentile", quantile_type = 7L, alternative = alternative,
      estimand = if (mode == "one_sample") "population_mean_minus_mu" else "population_mean_difference",
      resampling = if (mode == "independent") "within each group" else if (mode == "paired") "paired row differences" else "within sample",
      streams = "sequential mean-difference then d resamples for each test in the recorded order",
      non_finite = "exclude, warn, and report counts; fewer than two finite statistics leaves interval unavailable") else NULL))
  calculated <- lapply(entries, compute_ttest, mode = mode, mu = mu, alternative = alternative,
    var_equal = var_equal, conf_level = conf_level, bootstrap = bootstrap, bootstrap_samples = bootstrap_samples)
  summary_df <- do.call(rbind, lapply(calculated, `[[`, "summary"))
  diagnostics_df <- do.call(rbind, lapply(calculated, `[[`, "diagnostics"))
  for (column in c("variable", "measure_1", "measure_2")) summary_df <- add_variable_label_column(summary_df, label_meta, var_col = column)
  for (column in c("group_1", "group_2")) summary_df <- add_group_label_column(summary_df, label_meta, group_var, group_col = column)
  if (mode == "independent") {
    summary_df$group_1_label <- group_order[[1]]$label
    summary_df$group_2_label <- group_order[[2]]$label
    diagnostics_df$group_label <- vapply(diagnostics_df$group_id,
      function(i) group_order[[i]]$label, character(1))
  }
  note_tokens <- build_ttest_note_tokens(alternative, conf_level, var_equal, bootstrap, bootstrap_samples, mu, mode)
  template_path <- resolve_template_override(opts$template, module = "t_test")
  if (is.null(template_path)) template_path <- resolve_template_path("t_test.default", "t-test/default-template.md")
  template_path <- nlss_freeze_template(template_path, "t_test.main")
  template_meta <- get_template_meta(template_path)
  nlss_text <- format_nlss_text(summary_df, digits, conf_level, alternative, var_equal)
  table_result <- build_ttest_table_body(summary_df, digits, template_meta$table)
  template_context <- list(tokens = c(list(table_body = table_result$body, narrative_default = nlss_text), note_tokens),
    narrative_rows = build_ttest_narrative_rows(summary_df, digits, conf_level, alternative, var_equal))
  analysis_flags <- list(vars = vars, x = x_vars, y = y_vars, group = group_var,
    mu = if (mode == "one_sample") mu else NULL, alternative = alternative,
    "var-equal" = if (mode == "independent") var_equal else NULL, "conf-level" = conf_level,
    bootstrap = bootstrap, "bootstrap-samples" = if (bootstrap) bootstrap_samples else NULL,
    seed = seed, digits = digits)
  nlss_stage_report(file.path(out_dir, "report_canonical.md"), "t-tests",
    format_nlss_table(summary_df, digits, note_tokens$note_default), nlss_text,
    analysis_flags = analysis_flags, template_path = template_path, template_context = template_context)
  nlss_set_result(list(summary_df = summary_df, diagnostics_df = diagnostics_df))
  if (parse_bool(opts$log, get_config_value("defaults.log"))) {
    ctx <- get_run_context()
    nlss_stage_log(out_dir, module = "t_test", prompt = ctx$prompt, commands = ctx$commands,
      results = list(summary_df = summary_df, diagnostics_df = diagnostics_df), options = resolved_options,
      user_prompt = get_user_prompt(opts))
  }
}

tryCatch(nlss_run_main("t_test", main),
  nlss_ttest_expected_groups = function(e) cat("EXPECTED_NEGATIVE: t_test group levels: ", conditionMessage(e), "\n", sep = ""))
