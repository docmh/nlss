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
  cat("Reliability analysis (base R)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript reliability.R --csv data.csv --analysis icc --vars r1,r2,r3\n")
  cat("  Rscript reliability.R --csv data.csv --analysis kappa --vars r1,r2\n")
  cat("  Rscript reliability.R --csv data.csv --analysis test_retest --vars t1,t2\n")
  cat("  Rscript reliability.R --csv data.csv --analysis icc --format long --id id --rater rater --score score\n")
  cat("  Rscript reliability.R --interactive\n")
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
  cat("  --analysis TYPE         icc/kappa/test_retest (default: icc)\n")
  cat("  --format TYPE           wide/long (default: wide)\n")
  cat("  --vars LIST             Comma-separated variables for wide format\n")
  cat("  --id NAME               Subject ID variable (long format)\n")
  cat("  --rater NAME            Rater/time variable (long format)\n")
  cat("  --score NAME            Score variable (long format)\n")
  cat("  --group NAME            Grouping variable name (optional)\n")
  cat("  --missing TYPE           complete/pairwise (default: complete)\n")
  cat("  --icc-model TYPE         oneway/twoway-random/twoway-mixed (default: twoway-random)\n")
  cat("  --icc-type TYPE          agreement/consistency (default: agreement)\n")
  cat("  --icc-unit TYPE          single/average (default: single)\n")
  cat("  --kappa-weight TYPE      none/linear/quadratic (default: none)\n")
  cat("  --method TYPE            pearson/spearman (default: pearson)\n")
  cat("  --conf-level VALUE       Confidence level (default: 0.95)\n")
  cat("  --coerce TRUE/FALSE      Coerce numeric inputs when needed (default: FALSE)\n")
  cat("  --digits N               Rounding digits (default: 2)\n")
  cat("  --template REF           Template path or template key (optional)\n")
  cat("  --user-prompt TEXT       Original AI user prompt for logging (optional)\n")
  cat("  --log TRUE/FALSE         Write analysis_log.jsonl (default: TRUE)\n")
  cat("  --expect-invalid TRUE/FALSE  Treat invalid input as expected (default: FALSE)\n")
  cat("  --interactive            Prompt for inputs\n")
  cat("  --help                   Show this help\n")
}

interactive_options <- function() {
  cat("Interactive input selected.\n")
  input_type <- prompt("Input type (csv/sav/rds/rdata/parquet)", "csv")
  input_type <- tolower(input_type)
  opts <- list()

  if (input_type == "csv") {
    opts$csv <- prompt("CSV path")
    sep_default <- get_config_value("defaults.csv.sep", ",")
    header_default <- get_config_value("defaults.csv.header", TRUE)
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

  analysis_default <- get_config_value("modules.reliability.analysis", "icc")
  format_default <- get_config_value("modules.reliability.format", "wide")

  opts$analysis <- prompt("Analysis (icc/kappa/test_retest)", analysis_default)
  opts$format <- prompt("Format (wide/long)", format_default)
  opts$vars <- prompt("Variables (comma-separated for wide)", "")
  opts$id <- prompt("ID variable (long)", "")
  opts$rater <- prompt("Rater/time variable (long)", "")
  opts$score <- prompt("Score variable (long)", "")
  opts$group <- prompt("Grouping variable (blank for none)", "")

  missing_default <- get_config_value("modules.reliability.missing", "complete")
  conf_default <- get_config_value("modules.reliability.conf_level", 0.95)
  coerce_default <- get_config_value("modules.reliability.coerce", FALSE)
  digits_default <- get_config_value("defaults.digits", 2)
  icc_model_default <- get_config_value("modules.reliability.icc_model", "twoway-random")
  icc_type_default <- get_config_value("modules.reliability.icc_type", "agreement")
  icc_unit_default <- get_config_value("modules.reliability.icc_unit", "single")
  kappa_weight_default <- get_config_value("modules.reliability.kappa_weight", "none")
  method_default <- get_config_value("modules.reliability.method", "pearson")

  opts$missing <- prompt("Missing handling (complete/pairwise)", missing_default)
  opts$`icc-model` <- prompt("ICC model (oneway/twoway-random/twoway-mixed)", icc_model_default)
  opts$`icc-type` <- prompt("ICC type (agreement/consistency)", icc_type_default)
  opts$`icc-unit` <- prompt("ICC unit (single/average)", icc_unit_default)
  opts$`kappa-weight` <- prompt("Kappa weights (none/linear/quadratic)", kappa_weight_default)
  opts$method <- prompt("Test-retest method (pearson/spearman)", method_default)
  opts$`conf-level` <- prompt("Confidence level", as.character(conf_default))
  opts$coerce <- prompt("Coerce numeric TRUE/FALSE", ifelse(isTRUE(coerce_default), "TRUE", "FALSE"))
  opts$digits <- prompt("Rounding digits", as.character(digits_default))
  opts$template <- prompt("Template (path or key; blank for default)", "")
  opts$`user-prompt` <- prompt("User prompt (optional)", "")
  log_default <- get_config_value("defaults.log", TRUE)
  opts$log <- prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))
  opts
}

normalize_analysis <- function(value, default = "icc") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(as.character(val))
  val <- gsub("-", "_", val)
  if (val %in% c("icc", "intra_class", "intraclass")) return("icc")
  if (val %in% c("kappa", "cohen")) return("kappa")
  if (val %in% c("test_retest", "testretest", "retest", "stability")) return("test_retest")
  stop("Invalid analysis option: ", val)
}

normalize_format <- function(value, default = "wide") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(as.character(val))
  if (val %in% c("wide", "long")) return(val)
  stop("Invalid format option: ", val)
}

normalize_missing <- function(value, default = "complete") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(as.character(val))
  if (val %in% c("complete", "listwise")) return("complete")
  if (val %in% c("pairwise", "pair")) return("pairwise")
  stop("Invalid missing option: ", val)
}

normalize_icc_model <- function(value, default = "twoway-random") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(as.character(val))
  val <- gsub("_", "-", val)
  if (val %in% c("oneway", "one-way")) return("oneway")
  if (val %in% c("twoway", "two-way", "twoway-random", "two-way-random", "random")) return("twoway-random")
  if (val %in% c("twoway-mixed", "two-way-mixed", "mixed")) return("twoway-mixed")
  stop("Invalid icc-model option: ", val)
}

normalize_icc_type <- function(value, default = "agreement") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(as.character(val))
  if (val %in% c("agreement", "absolute")) return("agreement")
  if (val %in% c("consistency", "consist")) return("consistency")
  stop("Invalid icc-type option: ", val)
}

normalize_icc_unit <- function(value, default = "single") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(as.character(val))
  if (val %in% c("single", "individual")) return("single")
  if (val %in% c("average", "mean", "avg")) return("average")
  stop("Invalid icc-unit option: ", val)
}

normalize_kappa_weight <- function(value, default = "none") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(as.character(val))
  if (val %in% c("none", "unweighted")) return("none")
  if (val %in% c("linear", "lin")) return("linear")
  if (val %in% c("quadratic", "quad")) return("quadratic")
  stop("Invalid kappa-weight option: ", val)
}

normalize_method <- function(value, default = "pearson") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(as.character(val))
  if (val %in% c("pearson", "pear")) return("pearson")
  if (val %in% c("spearman", "rho")) return("spearman")
  stop("Invalid method option: ", val)
}

coerce_dataframe <- function(df, vars, coerce) {
  if (!isTRUE(coerce) || length(vars) == 0) return(df)
  df[vars] <- lapply(df[vars], function(x) {
    if (is.numeric(x)) return(x)
    suppressWarnings(as.numeric(as.character(x)))
  })
  df
}

compute_missing_summary <- function(values) {
  total_n <- nrow(values)
  complete_n <- sum(complete.cases(values))
  missing_n <- total_n - complete_n
  missing_pct <- ifelse(total_n > 0, missing_n / total_n * 100, NA_real_)
  list(total = total_n, complete = complete_n, missing_n = missing_n, missing_pct = missing_pct)
}

# Balanced ANOVA ICCs and intervals follow Shrout/Fleiss as independently
# implemented in psych::ICC(lmer = FALSE); psych is a test oracle, not a runtime
# dependency. Model (random/mixed) changes interpretation, not the chosen
# agreement/consistency formula.
compute_icc <- function(values, model, type, unit, conf_level) {
  values <- as.matrix(values)
  n <- nrow(values)
  k <- ncol(values)
  result <- list(estimate = NA_real_, ci_low = NA_real_, ci_high = NA_real_,
    f_stat = NA_real_, df1 = NA_real_, df2 = NA_real_, p_value = NA_real_,
    n_subjects = n, n_raters = k, status_reason = "")
  if (n < 2 || k < 2) {
    result$status_reason <- "ICC requires at least two complete subjects and two raters."
    return(result)
  }
  grand <- mean(values)
  ms_rows <- k * sum((rowMeans(values) - grand)^2) / (n - 1)
  df1 <- n - 1
  if (model == "oneway") {
    ms_error <- sum((values - rowMeans(values))^2) / (n * (k - 1))
    ms_cols <- NA_real_
    df2 <- n * (k - 1)
  } else {
    # Computing residuals directly avoids cancellation of almost equal SS terms.
    residuals <- sweep(sweep(values, 1, rowMeans(values)), 2, colMeans(values)) + grand
    ms_error <- sum(residuals^2) / ((n - 1) * (k - 1))
    ms_cols <- n * sum((colMeans(values) - grand)^2) / (k - 1)
    df2 <- (n - 1) * (k - 1)
  }
  agreement <- model != "oneway" && type == "agreement"
  single_denom <- ms_rows + (k - 1) * ms_error +
    if (agreement) k * (ms_cols - ms_error) / n else 0
  average_denom <- ms_rows + if (agreement) (ms_cols - ms_error) / n else 0
  result$estimate <- (ms_rows - ms_error) / if (unit == "average") average_denom else single_denom
  result$df1 <- df1
  result$df2 <- df2
  if (!is.finite(result$estimate)) {
    result$status_reason <- "ICC is not estimable because the variance denominator is zero."
    result$estimate <- NA_real_
    return(result)
  }
  if (ms_error == 0) {
    # A valid point estimate (including perfect agreement) is not an inferential
    # F test with a positive error mean square.
    result$status_reason <- "ICC point estimate only: zero error variance; F inference and confidence interval unavailable."
    return(result)
  }
  result$f_stat <- ms_rows / ms_error
  result$p_value <- stats::pf(result$f_stat, df1, df2, lower.tail = FALSE)
  alpha <- 1 - conf_level
  if (agreement) {
    single <- (ms_rows - ms_error) / single_denom
    fj <- ms_cols / ms_error
    a <- k * single * fj
    b <- n * (1 + (k - 1) * single) - k * single
    v <- (k - 1) * (n - 1) * (a + b)^2 / ((n - 1) * a^2 + b^2)
    if (!is.finite(v) || v <= 0) {
      result$status_reason <- "ICC point estimate and F test available; agreement CI degrees of freedom are not estimable."
      return(result)
    }
    f_upper <- stats::qf(1 - alpha / 2, n - 1, v)
    f_lower <- stats::qf(1 - alpha / 2, v, n - 1)
    common <- k * ms_cols + (k * n - k - n) * ms_error
    low <- n * (ms_rows - f_upper * ms_error) / (f_upper * common + n * ms_rows)
    high <- n * (f_lower * ms_rows - ms_error) / (common + n * f_lower * ms_rows)
    if (unit == "average") {
      low <- k * low / (1 + (k - 1) * low)
      high <- k * high / (1 + (k - 1) * high)
    }
  } else {
    f_low <- result$f_stat / stats::qf(1 - alpha / 2, df1, df2)
    f_high <- result$f_stat / stats::qf(alpha / 2, df1, df2)
    if (unit == "average") {
      low <- 1 - 1 / f_low
      high <- 1 - 1 / f_high
    } else {
      low <- (f_low - 1) / (f_low + k - 1)
      high <- (f_high - 1) / (f_high + k - 1)
    }
  }
  if (is.na(low) || is.na(high) || low > high) {
    result$status_reason <- "ICC point estimate and F test available; the confidence-interval transformation is singular."
  } else {
    result$ci_low <- low
    result$ci_high <- high
  }
  result
}

kappa_category_order <- function(x, y, weight = "none") {
  declared <- lapply(list(x, y), function(z) if (is.factor(z)) levels(z) else NULL)
  declared <- Filter(Negate(is.null), declared)
  if (length(declared)) {
    if (weight != "none" && length(declared) == 2 &&
        !identical(declared[[1]], declared[[2]])) {
      stop("Weighted kappa requires the same declared category order for both raters.")
    }
    categories <- unique(unlist(declared, use.names = FALSE))
    observed <- unique(c(as.character(x[!is.na(x)]), as.character(y[!is.na(y)])))
    if (weight != "none" && any(!observed %in% categories)) {
      stop("Weighted kappa has observed values outside the declared category order.")
    }
    return(list(levels = unique(c(categories, sort(setdiff(observed, categories)))),
      source = "declared factor levels, including unused categories"))
  }
  if (is.numeric(x) && is.numeric(y)) {
    return(list(levels = as.character(sort(unique(c(x[!is.na(x)], y[!is.na(y)])))),
      source = "numeric code order"))
  }
  list(levels = sort(unique(c(as.character(x[!is.na(x)]), as.character(y[!is.na(y)])))),
    source = "lexical character order under recorded locale")
}

compute_kappa <- function(x, y, weight = "none", categories = kappa_category_order(x, y, weight)$levels) {
  idx <- complete.cases(x, y)
  x <- x[idx]
  y <- y[idx]
  n <- length(x)
  k <- length(categories)
  result <- list(estimate = NA_real_, n = n, n_categories = k, status_reason = "")
  if (n == 0 || k < 2) {
    result$status_reason <- "Kappa is not estimable without complete pairs in at least two categories."
    return(result)
  }
  tab <- table(factor(as.character(x), levels = categories), factor(as.character(y), levels = categories))
  distances <- abs(outer(seq_len(k), seq_len(k), "-"))
  weights <- switch(weight, none = (distances == 0) * 1,
    linear = 1 - distances / (k - 1), quadratic = 1 - (distances / (k - 1))^2)
  observed <- tab / n
  expected <- outer(rowSums(observed), colSums(observed))
  po <- sum(weights * observed)
  pe <- sum(weights * expected)
  if (1 - pe <= .Machine$double.eps) {
    result$status_reason <- "Kappa is not estimable because expected agreement equals one."
  } else {
    result$estimate <- (po - pe) / (1 - pe)
  }
  result
}

compute_test_retest <- function(x, y, method = "pearson", conf_level = 0.95) {
  idx <- complete.cases(x, y)
  x <- x[idx]
  y <- y[idx]
  n <- length(x)
  result <- list(estimate = NA_real_, ci_low = NA_real_, ci_high = NA_real_,
    p_value = NA_real_, n = n, status_reason = "")
  if (n < 3 || stats::sd(x) == 0 || stats::sd(y) == 0) {
    result$status_reason <- "Test-retest inference requires at least three complete pairs and variation in both variables."
    return(result)
  }
  test_args <- list(x = x, y = y, method = method, conf.level = conf_level)
  if (method == "spearman") test_args$exact <- FALSE
  test <- do.call(stats::cor.test, test_args)
  result$estimate <- as.numeric(test$estimate)
  result$p_value <- test$p.value
  if (method == "pearson") {
    if (!is.null(test$conf.int)) {
      result$ci_low <- test$conf.int[1]
      result$ci_high <- test$conf.int[2]
    }
  } else if (n > 3) {
    # Compatibility estimate: Fisher-z interval for Spearman is an approximation,
    # not an exact rank interval. Do not perturb a perfect point correlation.
    z <- atanh(result$estimate)
    half_width <- stats::qnorm(1 - (1 - conf_level) / 2) / sqrt(n - 3)
    result$ci_low <- tanh(z - half_width)
    result$ci_high <- tanh(z + half_width)
  }
  if (n == 3) result$status_reason <- "Correlation and p value available; Fisher-z CI requires at least four complete pairs."
  result
}

long_to_wide <- function(df, id_var, rater_var, score_var) {
  valid <- !is.na(df[[id_var]]) & !is.na(df[[rater_var]])
  filtered <- df[valid, c(id_var, rater_var, score_var), drop = FALSE]
  if (!nrow(filtered)) stop("No rows available after removing missing id/rater values.")
  if (anyDuplicated(filtered[, c(id_var, rater_var), drop = FALSE])) {
    stop("Duplicate id/rater combinations found in long data. Aggregate before running reliability.")
  }
  ids <- unique(filtered[[id_var]])
  raters <- unique(filtered[[rater_var]])
  # Index the original score vector to preserve factors/ordered levels and values;
  # assigning a factor into a generic matrix would silently use integer codes.
  values <- lapply(raters, function(r) {
    r_rows <- which(filtered[[rater_var]] == r)
    filtered[[score_var]][r_rows[match(ids, filtered[[id_var]][r_rows])]]
  })
  names(values) <- as.character(raters)
  wide <- as.data.frame(values, check.names = FALSE)
  source_rows_by_rater <- lapply(raters, function(r) {
    r_rows <- which(filtered[[rater_var]] == r)
    which(valid)[r_rows[match(ids, filtered[[id_var]][r_rows])]]
  })
  names(source_rows_by_rater) <- names(values)
  list(matrix = wide, ids = ids, raters = raters,
    included_rows = which(valid), dropped_id_rater_rows = which(!valid),
    source_rows_by_rater = source_rows_by_rater,
    source_rows_by_subject = lapply(ids, function(id) which(valid)[which(filtered[[id_var]] == id)]))
}

icc_label <- function(model, type, unit) {
  size <- if (unit == "average") "k" else "1"
  family <- if (model == "oneway") "1" else if (type == "agreement") "A" else "C"
  paste0("ICC(", family, ",", size, ")")
}

analysis_label <- function(analysis) {
  switch(analysis,
    icc = "ICC",
    kappa = "Kappa",
    test_retest = "Test-retest",
    analysis
  )
}

build_method_label <- function(row) {
  if (row$analysis == "icc") {
    return(paste0(row$icc_label, " (", row$type, ", ", row$unit, ")"))
  }
  if (row$analysis == "kappa") {
    if (row$weight == "none") return("Cohen's kappa")
    return(paste("Weighted kappa (", row$weight, ")", sep = ""))
  }
  if (row$analysis == "test_retest") {
    return(paste("Test-retest", row$method))
  }
  row$analysis
}

format_num <- function(value, digits) {
  if (is.na(value)) return("")
  format(round(value, digits), nsmall = digits, trim = TRUE)
}

format_num_text <- function(value, digits) {
  if (is.na(value)) return("NA")
  format(round(value, digits), nsmall = digits, trim = TRUE)
}

format_ci <- function(low, high, digits) {
  if (is.na(low) || is.na(high)) return("")
  paste0("[", format_num_text(low, digits), ", ", format_num_text(high, digits), "]")
}

format_p <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value < 0.001) return("< .001")
  p_txt <- format(round(p_value, 3), nsmall = 3, trim = TRUE)
  p_txt <- sub("^0", "", p_txt)
  paste("=", p_txt)
}

format_p_text <- function(p_value) {
  if (is.na(p_value)) return("NA")
  if (p_value < 0.001) return("< .001")
  p_txt <- format(round(p_value, 3), nsmall = 3, trim = TRUE)
  p_txt <- sub("^0", "", p_txt)
  paste("=", p_txt)
}

build_reliability_table_body <- function(summary_df, digits, conf_level, table_spec = NULL) {
  display <- summary_df
  display$group <- as.character(display$group)
  display$group[is.na(display$group)] <- "NA"
  display$group_display <- if ("group_label" %in% names(display)) display$group_label else display$group
  display$var1_display <- if ("var1_label" %in% names(display)) display$var1_label else display$var1
  display$var2_display <- if ("var2_label" %in% names(display)) display$var2_label else display$var2

  ci_label <- paste0(format(conf_level * 100, trim = TRUE, scientific = FALSE), "% CI")
  default_columns <- list(
    list(key = "analysis", label = "Analysis"),
    list(key = "group", label = "Group", drop_if_empty = TRUE),
    list(key = "method_label", label = "Measure"),
    list(key = "estimate", label = "Estimate"),
    list(key = "ci", label = ci_label, drop_if_empty = TRUE),
    list(key = "p", label = "p", drop_if_empty = TRUE),
    list(key = "n", label = "n"),
    list(key = "n_raters", label = "Raters", drop_if_empty = TRUE),
    list(key = "model", label = "Model", drop_if_empty = TRUE),
    list(key = "type", label = "Type", drop_if_empty = TRUE),
    list(key = "unit", label = "Unit", drop_if_empty = TRUE),
    list(key = "weight", label = "Weights", drop_if_empty = TRUE),
    list(key = "var1", label = "Var 1", drop_if_empty = TRUE),
    list(key = "var2", label = "Var 2", drop_if_empty = TRUE),
    list(key = "f", label = "F", drop_if_empty = TRUE),
    list(key = "df1", label = "df1", drop_if_empty = TRUE),
    list(key = "df2", label = "df2", drop_if_empty = TRUE)
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
      if (key == "analysis") {
        val <- as_cell_text(row$analysis_label)
      } else if (key == "group") {
        val <- as_cell_text(row$group_display)
      } else if (key == "method_label") {
        val <- as_cell_text(row$method_label)
      } else if (key == "estimate") {
        val <- format_num(row$estimate, digits)
      } else if (key == "ci") {
        val <- format_ci(row$ci_low, row$ci_high, digits)
      } else if (key == "p") {
        val <- format_p(row$p_value)
      } else if (key == "n") {
        val <- ifelse(is.na(row$n), "", as.character(row$n))
      } else if (key == "n_raters") {
        val <- ifelse(is.na(row$n_raters), "", as.character(row$n_raters))
      } else if (key == "f") {
        val <- format_num(row$f_stat, digits)
      } else if (key %in% c("df1", "df2")) {
        val <- ifelse(is.na(row[[key]][1]), "", as.character(row[[key]][1]))
      } else if (key %in% names(row)) {
        cell <- row[[key]][1]
        if (is.numeric(cell)) {
          val <- format_num(cell, digits)
        } else {
          if (key == "var1") {
            val <- as_cell_text(row$var1_display[1])
          } else if (key == "var2") {
            val <- as_cell_text(row$var2_display[1])
          } else {
            val <- as_cell_text(cell)
          }
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

build_reliability_note_tokens <- function(summary_df, analysis, conf_level, missing_method, icc_model, icc_type, icc_unit, kappa_weight, retest_method) {
  ci_label <- paste0(format(conf_level * 100, trim = TRUE, scientific = FALSE), "% CI")
  missing_note <- paste0("Missing values handled ", missing_method, ".")
  icc_note <- ""
  kappa_note <- ""
  retest_note <- ""
  if (analysis == "icc") {
    icc_note <- paste0("ICC model = ", icc_model, ", type = ", icc_type, ", unit = ", icc_unit, ".")
  }
  if (analysis == "kappa") {
    kappa_note <- paste(if (kappa_weight == "none") "Unweighted kappa." else paste0("Kappa weights: ", kappa_weight, "; equally spaced ranks in the recorded category order."), "Kappa confidence intervals and p values are not implemented.")
  }
  if (analysis == "test_retest") {
    retest_note <- paste0("Test-retest method: ", retest_method, ".")
  }

  ci_note <- ""
  if (nrow(summary_df) > 0 && any(!is.na(summary_df$ci_low))) {
    if (analysis == "test_retest") {
      ci_note <- paste(ci_label, if (retest_method == "spearman") "is a Fisher-z approximation for Spearman, not an exact rank interval." else "uses the Fisher-z approximation from stats::cor.test.")
    } else if (analysis == "icc") {
      ci_note <- paste(ci_label, "computed from F distributions.")
    }
  }

  limitations <- unique(summary_df$status_reason[nzchar(summary_df$status_reason)])
  note_parts <- c(missing_note, icc_note, kappa_note, retest_note, ci_note, limitations)
  note_default <- paste(note_parts[nzchar(note_parts)], collapse = " ")

  list(
    ci_label = ci_label,
    missing_note = missing_note,
    icc_note = icc_note,
    kappa_note = kappa_note,
    retest_note = retest_note,
    ci_note = ci_note,
    note_default = note_default
  )
}

build_reliability_narrative_rows <- function(summary_df, digits, conf_level) {
  display <- summary_df
  display$group <- as.character(display$group)
  display$group[is.na(display$group)] <- "NA"
  display$group_display <- if ("group_label" %in% names(display)) display$group_label else display$group
  display$var1_display <- if ("var1_label" %in% names(display)) display$var1_label else display$var1
  display$var2_display <- if ("var2_label" %in% names(display)) display$var2_label else display$var2
  rows <- list()

  for (i in seq_len(nrow(display))) {
    row <- display[i, , drop = FALSE]
    group_label <- if (row$group == "") "Overall" else paste("Group", row$group_display)

    missing_pct <- ifelse(is.na(row$missing_pct), "NA", format_num_text(row$missing_pct, 1))
    missing_text <- paste0(
      "Missing = ",
      ifelse(is.na(row$missing_n), "NA", as.character(row$missing_n)),
      " (",
      missing_pct,
      "%)"
    )

    estimate_text <- format_num_text(row$estimate, digits)
    ci_text <- ""
    if (!is.na(row$ci_low) && !is.na(row$ci_high)) {
      ci_text <- paste0(", ", format(conf_level * 100, trim = TRUE, scientific = FALSE), "% CI ", format_ci(row$ci_low, row$ci_high, digits))
    }

    p_text <- format_p_text(row$p_value)
    p_clause <- ifelse(is.na(row$p_value), "", paste0(", p ", p_text))

    if (is.na(row$estimate)) {
      line <- paste0(
        group_label,
        ": reliability could not be computed (n = ",
        ifelse(is.na(row$n), "NA", as.character(row$n)),
        "). ",
        missing_text,
        "."
      )
    } else if (row$analysis == "icc") {
      line <- paste0(
        group_label,
        ": ",
        row$method_label,
        " = ",
        estimate_text,
        ci_text,
        ifelse(is.na(row$f_stat), "", paste0(", F(", row$df1, ", ", row$df2, ") = ", format_num_text(row$f_stat, digits))),
        p_clause,
        ", n = ",
        as.character(row$n),
        ", k = ",
        as.character(row$n_raters),
        ". ",
        missing_text,
        "."
      )
    } else if (row$analysis == "kappa") {
      line <- paste0(
        group_label,
        ": ",
        row$method_label,
        " = ",
        estimate_text,
        ", n = ",
        as.character(row$n),
        ". ",
        missing_text,
        "."
      )
    } else {
      line <- paste0(
        group_label,
        ": ",
        row$method_label,
        " = ",
        estimate_text,
        ci_text,
        p_clause,
        ", n = ",
        as.character(row$n),
        ". ",
        missing_text,
        "."
      )
    }

    rows[[length(rows) + 1]] <- list(
      analysis = as_cell_text(row$analysis),
      analysis_label = as_cell_text(row$analysis_label),
      group = as_cell_text(row$group_display),
      group_label = group_label,
      method_label = as_cell_text(row$method_label),
      icc_label = as_cell_text(row$icc_label),
      estimate = estimate_text,
      ci = format_ci(row$ci_low, row$ci_high, digits),
      ci_text = ci_text,
      p = format_p_text(row$p_value),
      n = ifelse(is.na(row$n), "NA", as.character(row$n)),
      n_raters = ifelse(is.na(row$n_raters), "NA", as.character(row$n_raters)),
      var1 = as_cell_text(row$var1_display),
      var2 = as_cell_text(row$var2_display),
      missing_n = ifelse(is.na(row$missing_n), "NA", as.character(row$missing_n)),
      missing_pct = missing_pct,
      missing_text = missing_text,
      full_sentence = line
    )
  }

  rows
}

format_nlss_text <- function(summary_df, digits, conf_level) {
  rows <- build_reliability_narrative_rows(summary_df, digits, conf_level)
  lines <- vapply(rows, function(row) row$full_sentence, character(1))
  paste(lines, collapse = "\n")
}

format_nlss_table <- function(summary_df, digits, note_text, conf_level) {
  table_body <- build_reliability_table_body(summary_df, digits, conf_level, table_spec = NULL)
  header <- "Table 1\nReliability analysis\n\n"
  note_line <- if (nzchar(note_text)) paste0("Note. ", note_text) else "Note."
  paste0(header, table_body, "\n", note_line, "\n")
}

emit_input_issue <- function(out_dir, opts, message, details = list(), status = "invalid_input", expected = FALSE) {
  log_default <- get_config_value("defaults.log", TRUE)
  if (parse_bool(opts$log, default = log_default)) {
    ctx <- get_run_context()
    nlss_stage_log(
      out_dir,
      module = "reliability",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(
        status = ifelse(expected, "expected_invalid_input", status),
        message = message,
        details = details
      ),
      options = list(
        analysis = opts$analysis,
        format = opts$format,
        vars = opts$vars,
        id = opts$id,
        rater = opts$rater,
        score = opts$score
      ),
      user_prompt = get_user_prompt(opts)
    )
  }
  if (expected) {
    stop(structure(list(message = message, call = NULL),
      class = c("nlss_reliability_expected_invalid", "error", "condition")))
  }
  stop(message)
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  opts <- nlss_run_options(args, "reliability")
  if (!is.null(opts$help)) {
    print_usage()
    quit(status = 0)
  }
  if (parse_bool(opts$interactive, default = FALSE)) {
    opts <- modifyList(opts, interactive_options())
  }
  df <- nlss_load_input(opts)
  out_dir <- get_workspace_out_dir(df)
  nlss_begin_run("reliability", df, opts, out_dir)
  expect_invalid <- parse_bool(opts$`expect-invalid`, default = FALSE)
  input_issue <- function(message, details = list()) {
    emit_input_issue(out_dir, opts, message, details, expected = expect_invalid)
  }
  scientific_options <- tryCatch({
    list(
      analysis = normalize_analysis(opts$analysis, get_config_value("modules.reliability.analysis")),
      format = normalize_format(opts$format, get_config_value("modules.reliability.format")),
      missing = normalize_missing(opts$missing, get_config_value("modules.reliability.missing")),
      icc_model = normalize_icc_model(opts$`icc-model`, get_config_value("modules.reliability.icc_model")),
      icc_type = normalize_icc_type(opts$`icc-type`, get_config_value("modules.reliability.icc_type")),
      icc_unit = normalize_icc_unit(opts$`icc-unit`, get_config_value("modules.reliability.icc_unit")),
      kappa_weight = normalize_kappa_weight(opts$`kappa-weight`, get_config_value("modules.reliability.kappa_weight")),
      method = normalize_method(opts$method, get_config_value("modules.reliability.method"))
    )
  }, error = function(e) input_issue(conditionMessage(e)))
  analysis <- scientific_options$analysis
  format <- scientific_options$format
  missing_requested <- scientific_options$missing
  missing_method <- if (analysis == "icc") "complete" else missing_requested
  icc_model <- scientific_options$icc_model
  icc_type <- scientific_options$icc_type
  effective_type <- if (icc_model == "oneway") "agreement" else icc_type
  icc_unit <- scientific_options$icc_unit
  kappa_weight <- scientific_options$kappa_weight
  retest_method <- scientific_options$method
  conf_level <- if (!is.null(opts$`conf-level`)) as.numeric(opts$`conf-level`) else get_config_value("modules.reliability.conf_level")
  digits <- if (!is.null(opts$digits)) as.numeric(opts$digits) else get_config_value("defaults.digits")
  coerce_flag <- parse_bool(opts$coerce, get_config_value("modules.reliability.coerce"))
  log_default <- get_config_value("defaults.log")
  if (!is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) input_issue("Confidence level must be strictly between zero and one.")
  if (!is.finite(digits) || digits != floor(digits) || digits < 0 || digits > 15) input_issue("Digits must be an integer from 0 to 15.")
  if (analysis == "icc" && icc_model == "oneway" && icc_type != "agreement") {
    warning("One-way ICC has no separate consistency estimand; computing one-way agreement and recording the requested type.")
  }
  if (analysis == "icc" && missing_requested != "complete") {
    warning("ICC requires complete subjects across all raters; requested pairwise handling is resolved to complete cases.")
  }
  group_var <- if (!is.null(opts$group) && opts$group != "") opts$group else NULL
  if (!is.null(group_var) && !group_var %in% names(df)) input_issue("Grouping variable not found in data frame.")
  id_var <- if (format == "long") opts$id else NULL
  rater_var <- if (format == "long") opts$rater else NULL
  score_var <- if (format == "long") opts$score else NULL
  vars <- character(0)
  if (format == "long") {
    roles <- c(id_var, rater_var, score_var)
    if (length(roles) != 3 || any(!nzchar(roles))) input_issue("Long format requires --id, --rater, and --score.")
    if (any(!roles %in% names(df))) input_issue(paste("Long-format variables not found:", paste(setdiff(roles, names(df)), collapse = ", ")))
    if (anyDuplicated(roles)) input_issue("Long-format ID, rater, and score must be distinct variables.")
    if (!is.null(group_var) && group_var %in% c(rater_var, score_var)) input_issue("The grouping variable must differ from the rater and score variables.")
  } else {
    if (anyDuplicated(parse_list(opts$vars))) input_issue("Rating variables must be distinct.")
    vars <- tryCatch(select_variables(df, opts$vars, group_var,
      default = if (analysis == "kappa") "non-numeric" else "numeric"),
      error = function(e) input_issue(conditionMessage(e)))
    if (anyDuplicated(vars)) input_issue("Rating variables must be distinct.")
    if (analysis == "icc" && length(vars) < 2) input_issue("ICC requires at least two variables in wide format.")
    if (analysis != "icc" && length(vars) != 2) input_issue("Kappa and test-retest require exactly two variables in wide format.")
  }
  label_meta <- resolve_label_metadata(df)
  group_values <- if (is.null(group_var)) "" else unique(df[[group_var]])
  if (!length(group_values)) input_issue("No observed groups available for reliability analysis.")
  missing_group <- if (is.null(group_var)) "NA" else nlss_missing_group_label(df[[group_var]], label_meta, group_var)
  prepared <- list()
  groups <- list()
  for (group_index in seq_along(group_values)) {
    g <- group_values[group_index]
    is_missing <- !is.null(group_var) && is.na(g)
    indices <- if (is.null(group_var)) seq_len(nrow(df)) else
      if (is_missing) which(is.na(df[[group_var]])) else which(!is.na(df[[group_var]]) & df[[group_var]] == g)
    group_label <- if (is_missing) missing_group else as.character(g)
    subset <- df[indices, , drop = FALSE]
    long_result <- NULL
    if (format == "long") {
      long_result <- tryCatch(long_to_wide(subset, id_var, rater_var, score_var),
        error = function(e) input_issue(conditionMessage(e), list(group = group_label)))
      wide <- long_result$matrix
    } else {
      wide <- subset[, vars, drop = FALSE]
    }
    var_names <- names(wide)
    if (analysis == "icc" && ncol(wide) < 2) input_issue("ICC requires at least two raters.")
    if (analysis != "icc" && ncol(wide) != 2) input_issue("Kappa and test-retest require exactly two raters/variables.")
    original_classes <- lapply(wide, class)
    introduced_missing <- lapply(wide, function(x) integer(0))
    if (analysis %in% c("icc", "test_retest")) {
      before <- wide
      wide <- coerce_dataframe(wide, var_names, coerce_flag)
      for (v in var_names) {
        if (!is.numeric(wide[[v]])) input_issue(paste("Variable is not numeric:", v))
        if (any(!is.finite(wide[[v]]) & !is.na(wide[[v]]))) input_issue(paste("Non-finite numeric ratings are not supported:", v))
        introduced_missing[[v]] <- which(!is.na(before[[v]]) & is.na(wide[[v]]))
        if (length(introduced_missing[[v]])) warning("Numeric coercion introduced missing ratings in ", v, "; exact subject rows are recorded in the request.")
      }
    }
    category_order <- if (analysis == "kappa")
      tryCatch(kappa_category_order(wide[[1]], wide[[2]], kappa_weight),
        error = function(e) input_issue(conditionMessage(e))) else NULL
    complete_indices <- which(complete.cases(wide))
    source_rows <- if (format == "long")
      lapply(long_result$source_rows_by_subject, function(i) indices[i]) else lapply(indices, identity)
    source_rows_by_rater <- if (format == "long")
      lapply(long_result$source_rows_by_rater, function(i) indices[i]) else
      setNames(rep(list(indices), ncol(wide)), var_names)
    introduced_source_rows <- lapply(var_names, function(v) source_rows_by_rater[[v]][introduced_missing[[v]]])
    names(introduced_source_rows) <- var_names
    group_design <- list(value = if (is_missing) NULL else g, label = group_label,
      group_class = if (is.null(group_var)) NULL else class(df[[group_var]]),
      is_missing = is_missing, row_indices = indices, rating_variables = var_names,
      input_classes = original_classes, analysis_classes = lapply(wide, class),
      factor_levels = lapply(wide, function(x) if (is.factor(x)) levels(x) else NULL),
      complete_subject_indices = complete_indices,
      complete_source_row_indices = sort(unlist(source_rows[complete_indices], use.names = FALSE)),
      source_rows_by_subject = source_rows,
      source_rows_by_rater = source_rows_by_rater,
      coercion_introduced_missing_subject_indices = introduced_missing,
      coercion_introduced_missing_source_rows = introduced_source_rows,
      category_order = category_order)
    if (format == "long") {
      group_design$long <- list(ids = long_result$ids, raters = long_result$raters,
        id_variable = id_var, rater_variable = rater_var, score_variable = score_var,
        included_row_indices = indices[long_result$included_rows],
        dropped_id_rater_row_indices = indices[long_result$dropped_id_rater_rows])
    }
    groups[[length(groups) + 1]] <- group_design
    prepared[[length(prepared) + 1]] <- list(data = wide, group = group_label, group_missing = is_missing,
      var_names = var_names, category_order = category_order)
  }
  resolved_options <- list(analysis = analysis, format = format, vars = vars,
    id = id_var, rater = rater_var, score = score_var, group = group_var,
    missing = missing_method, missing_requested = missing_requested,
    icc_model = icc_model, icc_type = icc_type, icc_effective_type = effective_type,
    icc_unit = icc_unit, kappa_weight = kappa_weight, method = retest_method,
    conf_level = conf_level, coerce = coerce_flag, digits = digits)
  nlss_resolve_request(resolved_options, design = list(rows = nrow(df), groups = groups,
    missing = "Complete rating vectors within each group; pairwise and complete coincide for two raters.",
    icc_effective_type = if (analysis == "icc") effective_type else NULL,
    kappa_weights = if (analysis == "kappa") "Equally spaced category ranks in the recorded order." else NULL,
    ci_method = if (analysis == "kappa") "not implemented" else if (analysis == "icc")
      "Shrout-Fleiss balanced-ANOVA F intervals; agreement uses estimated denominator degrees of freedom" else
      if (retest_method == "pearson") "stats::cor.test Fisher-z approximation" else
      "Fisher-z approximation for Spearman; not an exact rank interval"))
  summary_rows <- list()
  for (entry in prepared) {
    wide <- entry$data
    var_names <- entry$var_names
    missing_summary <- compute_missing_summary(wide)
    complete_data <- wide[complete.cases(wide), , drop = FALSE]
    calculated <- switch(analysis,
      icc = compute_icc(complete_data, icc_model, effective_type, icc_unit, conf_level),
      kappa = compute_kappa(wide[[1]], wide[[2]], kappa_weight, entry$category_order$levels),
      test_retest = compute_test_retest(wide[[1]], wide[[2]], retest_method, conf_level))
    if (!is.finite(calculated$estimate)) {
      input_issue(paste("Reliability is not estimable for", if (entry$group == "") "the dataset:" else paste0("group ", entry$group, ":"), calculated$status_reason),
        list(group = entry$group, group_missing = entry$group_missing, reason = calculated$status_reason))
    }
    if (nzchar(calculated$status_reason)) warning(calculated$status_reason)
    value <- function(key) if (is.null(calculated[[key]])) NA_real_ else calculated[[key]]
    row <- data.frame(
      analysis = analysis, analysis_label = analysis_label(analysis), group = entry$group,
      group_missing = entry$group_missing, method_label = "",
      icc_label = if (analysis == "icc") icc_label(icc_model, effective_type, icc_unit) else "",
      model = if (analysis == "icc") icc_model else "",
      type = if (analysis == "icc") effective_type else "",
      unit = if (analysis == "icc") icc_unit else "",
      weight = if (analysis == "kappa") kappa_weight else "",
      method = if (analysis == "test_retest") retest_method else "",
      var1 = if (analysis == "icc") "" else var_names[1],
      var2 = if (analysis == "icc") "" else var_names[2],
      estimate = calculated$estimate, ci_low = value("ci_low"), ci_high = value("ci_high"),
      p_value = value("p_value"), f_stat = value("f_stat"), df1 = value("df1"), df2 = value("df2"),
      n = if (analysis == "icc") calculated$n_subjects else calculated$n,
      n_raters = ncol(wide), n_categories = value("n_categories"),
      missing_n = missing_summary$missing_n, missing_pct = missing_summary$missing_pct,
      estimate_status = "finite",
      ci_status = if (analysis == "kappa") "not_implemented" else if (anyNA(c(value("ci_low"), value("ci_high")))) "not_available" else "available",
      inference_status = if (analysis == "kappa") "not_implemented" else if (is.na(value("p_value"))) "not_available" else "available",
      status_reason = calculated$status_reason, stringsAsFactors = FALSE)
    for (key in c("ci_low", "ci_high", "f_stat")) {
      row[[paste0(key, "_status")]] <- if (is.na(row[[key]])) "not_available" else
        if (is.infinite(row[[key]])) if (row[[key]] > 0) "positive_infinity" else "negative_infinity" else "finite"
    }
    row$method_label <- build_method_label(row)
    summary_rows[[length(summary_rows) + 1]] <- row
  }

  summary_df <- do.call(rbind, summary_rows)
  label_meta <- resolve_label_metadata(df)
  summary_df <- add_variable_label_column(summary_df, label_meta, var_col = "var1")
  summary_df <- add_variable_label_column(summary_df, label_meta, var_col = "var2")
  summary_df <- add_group_label_column(summary_df, label_meta, group_var, group_col = "group")
  if (!is.null(group_var)) summary_df$group_label[summary_df$group_missing] <- summary_df$group[summary_df$group_missing]

  template_override <- resolve_template_override(opts$template, module = "reliability")
  template_path <- if (!is.null(template_override)) {
    template_override
  } else {
    resolve_template_path("reliability.default", "reliability/default-template.md")
  }
  template_path <- nlss_freeze_template(template_path, "reliability.main")
  template_meta <- get_template_meta(template_path)
  nlss_report_path <- file.path(out_dir, "report_canonical.md")

  note_tokens <- build_reliability_note_tokens(
    summary_df,
    analysis,
    conf_level,
    missing_method,
    icc_model,
    effective_type,
    icc_unit,
    kappa_weight,
    retest_method
  )

  nlss_text <- format_nlss_text(summary_df, digits, conf_level)
  nlss_table <- format_nlss_table(summary_df, digits, note_tokens$note_default, conf_level)
  table_body <- build_reliability_table_body(summary_df, digits, conf_level, template_meta$table)
  narrative_rows <- build_reliability_narrative_rows(summary_df, digits, conf_level)

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

  analysis_flags <- list(
    analysis = analysis,
    format = format,
    vars = if (format == "wide") var_names else NULL,
    id = if (format == "long") opts$id else NULL,
    rater = if (format == "long") opts$rater else NULL,
    score = if (format == "long") opts$score else NULL,
    group = if (!is.null(group_var) && group_var != "") group_var else "None",
    missing = missing_method,
    "icc-model" = if (analysis == "icc") icc_model else NULL,
    "icc-type" = if (analysis == "icc") effective_type else NULL,
    "icc-type-requested" = if (analysis == "icc" && icc_type != effective_type) icc_type else NULL,
    "icc-unit" = if (analysis == "icc") icc_unit else NULL,
    "kappa-weight" = if (analysis == "kappa") kappa_weight else NULL,
    method = if (analysis == "test_retest") retest_method else NULL,
    "conf-level" = conf_level,
    coerce = if (analysis %in% c("icc", "test_retest")) coerce_flag else NULL,
    digits = digits
  )

  nlss_stage_report(
    nlss_report_path,
    "Reliability analysis",
    nlss_table,
    nlss_text,
    analysis_flags = analysis_flags,
    template_path = template_path,
    template_context = template_context
  )

  nlss_set_result(list(summary_df = summary_df))

  if (parse_bool(opts$log, default = log_default)) {
    ctx <- get_run_context()
    nlss_stage_log(
      out_dir,
      module = "reliability",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(summary_df = summary_df),
      options = resolved_options,
      user_prompt = get_user_prompt(opts)
    )
  }
}

tryCatch(nlss_run_main("reliability", main),
  nlss_reliability_expected_invalid = function(e) cat("EXPECTED_NEGATIVE: reliability invalid input: ", conditionMessage(e), "\n", sep = ""))
