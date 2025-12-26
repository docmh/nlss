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
source(file.path(bootstrap_dir, "lib", "paths.R"))
source_lib("cli.R")
source_lib("config.R")
source_lib("io.R")
source_lib("data_utils.R")
source_lib("formatting.R")

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
  input_type <- resolve_prompt("Input type (csv/sav/rds/rdata/parquet)", "csv")
  input_type <- tolower(input_type)
  opts <- list()

  if (input_type == "csv") {
    opts$csv <- resolve_prompt("CSV path")
    sep_default <- resolve_config_value("defaults.csv.sep", ",")
    header_default <- resolve_config_value("defaults.csv.header", TRUE)
    opts$sep <- resolve_prompt("Separator", sep_default)
    opts$header <- resolve_prompt("Header TRUE/FALSE", ifelse(isTRUE(header_default), "TRUE", "FALSE"))
  } else if (input_type == "sav") {
    opts$sav <- resolve_prompt("SAV path")
  } else if (input_type == "rds") {
    opts$rds <- resolve_prompt("RDS path")
  } else if (input_type == "rdata") {
    opts$rdata <- resolve_prompt("RData path")
    opts$df <- resolve_prompt("Data frame object name")
  } else if (input_type == "parquet") {
    opts$parquet <- resolve_prompt("Parquet path")
  } else {
    stop("Unsupported input type.")
  }

  analysis_default <- resolve_config_value("modules.reliability.analysis", "icc")
  format_default <- resolve_config_value("modules.reliability.format", "wide")

  opts$analysis <- resolve_prompt("Analysis (icc/kappa/test_retest)", analysis_default)
  opts$format <- resolve_prompt("Format (wide/long)", format_default)
  opts$vars <- resolve_prompt("Variables (comma-separated for wide)", "")
  opts$id <- resolve_prompt("ID variable (long)", "")
  opts$rater <- resolve_prompt("Rater/time variable (long)", "")
  opts$score <- resolve_prompt("Score variable (long)", "")
  opts$group <- resolve_prompt("Grouping variable (blank for none)", "")

  missing_default <- resolve_config_value("modules.reliability.missing", "complete")
  conf_default <- resolve_config_value("modules.reliability.conf_level", 0.95)
  coerce_default <- resolve_config_value("modules.reliability.coerce", FALSE)
  digits_default <- resolve_config_value("defaults.digits", 2)
  icc_model_default <- resolve_config_value("modules.reliability.icc_model", "twoway-random")
  icc_type_default <- resolve_config_value("modules.reliability.icc_type", "agreement")
  icc_unit_default <- resolve_config_value("modules.reliability.icc_unit", "single")
  kappa_weight_default <- resolve_config_value("modules.reliability.kappa_weight", "none")
  method_default <- resolve_config_value("modules.reliability.method", "pearson")

  opts$missing <- resolve_prompt("Missing handling (complete/pairwise)", missing_default)
  opts$`icc-model` <- resolve_prompt("ICC model (oneway/twoway-random/twoway-mixed)", icc_model_default)
  opts$`icc-type` <- resolve_prompt("ICC type (agreement/consistency)", icc_type_default)
  opts$`icc-unit` <- resolve_prompt("ICC unit (single/average)", icc_unit_default)
  opts$`kappa-weight` <- resolve_prompt("Kappa weights (none/linear/quadratic)", kappa_weight_default)
  opts$method <- resolve_prompt("Test-retest method (pearson/spearman)", method_default)
  opts$`conf-level` <- resolve_prompt("Confidence level", as.character(conf_default))
  opts$coerce <- resolve_prompt("Coerce numeric TRUE/FALSE", ifelse(isTRUE(coerce_default), "TRUE", "FALSE"))
  opts$digits <- resolve_prompt("Rounding digits", as.character(digits_default))
  opts$template <- resolve_prompt("Template (path or key; blank for default)", "")
  opts$`user-prompt` <- resolve_prompt("User prompt (optional)", "")
  log_default <- resolve_config_value("defaults.log", TRUE)
  opts$log <- resolve_prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))
  opts
}

resolve_prompt <- function(label, default = NULL) {
  if (exists("prompt", mode = "function")) {
    return(get("prompt", mode = "function")(label, default = default))
  }
  if (is.null(default)) {
    answer <- readline(paste0(label, ": "))
  } else {
    answer <- readline(paste0(label, " [", default, "]: "))
    if (answer == "") answer <- default
  }
  answer
}

resolve_default_out <- function() {
  if (exists("get_default_out", mode = "function")) {
    return(get("get_default_out", mode = "function")())
  }
  "./outputs/tmp"
}

resolve_config_value <- function(path, default = NULL) {
  if (exists("get_config_value", mode = "function")) {
    return(get("get_config_value", mode = "function")(path, default = default))
  }
  default
}

resolve_parse_args <- function(args) {
  if (exists("parse_args", mode = "function")) {
    return(get("parse_args", mode = "function")(args))
  }
  opts <- list()
  i <- 1
  while (i <= length(args)) {
    arg <- args[i]
    if (grepl("^--", arg)) {
      key <- sub("^--", "", arg)
      if (grepl("=", key)) {
        parts <- strsplit(key, "=", fixed = TRUE)[[1]]
        opts[[parts[1]]] <- parts[2]
      } else if (i < length(args) && !grepl("^--", args[i + 1])) {
        opts[[key]] <- args[i + 1]
        i <- i + 1
      } else {
        opts[[key]] <- TRUE
      }
    }
    i <- i + 1
  }
  opts
}

resolve_parse_bool <- function(value, default = FALSE) {
  if (exists("parse_bool", mode = "function")) {
    return(get("parse_bool", mode = "function")(value, default = default))
  }
  if (is.null(value)) return(default)
  if (is.logical(value)) return(value)
  val <- tolower(as.character(value))
  val %in% c("true", "t", "1", "yes", "y")
}

resolve_parse_list <- function(value, sep = ",") {
  if (exists("parse_list", mode = "function")) {
    return(get("parse_list", mode = "function")(value, sep = sep))
  }
  if (is.null(value) || is.logical(value)) return(character(0))
  value <- as.character(value)
  if (value == "") return(character(0))
  trimws(strsplit(value, sep, fixed = TRUE)[[1]])
}

resolve_ensure_out_dir <- function(path) {
  if (exists("ensure_out_dir", mode = "function")) {
    return(get("ensure_out_dir", mode = "function")(path))
  }
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  path
}

resolve_load_dataframe <- function(opts) {
  if (exists("load_dataframe", mode = "function")) {
    return(get("load_dataframe", mode = "function")(opts))
  }
  stop("Missing load_dataframe. Ensure lib/io.R is sourced.")
}

resolve_get_workspace_out_dir <- function(df) {
  if (exists("get_workspace_out_dir", mode = "function")) {
    return(get("get_workspace_out_dir", mode = "function")(df))
  }
  stop("Missing get_workspace_out_dir. Ensure lib/io.R is sourced.")
}

resolve_select_variables <- function(df, vars, group_var = NULL, default = "numeric") {
  if (exists("select_variables", mode = "function")) {
    return(get("select_variables", mode = "function")(df, vars, group_var = group_var, default = default))
  }
  available <- names(df)
  if (is.null(vars) || vars == "") {
    selected <- if (default == "numeric") available[sapply(df, is.numeric)] else available
    if (!is.null(group_var)) selected <- setdiff(selected, group_var)
    return(selected)
  }
  requested <- trimws(strsplit(vars, ",", fixed = TRUE)[[1]])
  missing <- setdiff(requested, available)
  if (length(missing) > 0) {
    stop(paste("Unknown variables:", paste(missing, collapse = ", ")))
  }
  if (!is.null(group_var)) requested <- setdiff(requested, group_var)
  requested
}

resolve_get_template_path <- function(key, default_relative = NULL) {
  if (exists("resolve_template_path", mode = "function")) {
    return(get("resolve_template_path", mode = "function")(key, default_relative))
  }
  NULL
}

resolve_get_template_meta <- function(path) {
  if (exists("get_template_meta", mode = "function")) {
    return(get("get_template_meta", mode = "function")(path))
  }
  list()
}

resolve_template_override <- local({
  override_impl <- NULL
  if (exists("resolve_template_override", mode = "function")) {
    override_impl <- get("resolve_template_override", mode = "function")
  }
  function(template_ref, module = NULL) {
    if (!is.null(override_impl)) {
      return(override_impl(template_ref, module = module))
    }
    NULL
  }
})

resolve_normalize_table_columns <- function(columns, default_specs) {
  if (exists("normalize_table_columns", mode = "function")) {
    return(get("normalize_table_columns", mode = "function")(columns, default_specs))
  }
  default_specs
}

resolve_drop_empty_columns <- function(columns, rows) {
  if (exists("drop_empty_columns", mode = "function")) {
    return(get("drop_empty_columns", mode = "function")(columns, rows))
  }
  list(columns = columns, rows = rows)
}

resolve_render_markdown_table <- function(headers, rows) {
  if (exists("render_markdown_table", mode = "function")) {
    return(get("render_markdown_table", mode = "function")(headers, rows))
  }
  ""
}

resolve_render_template_tokens <- function(text, tokens) {
  if (exists("render_template_tokens", mode = "function")) {
    return(get("render_template_tokens", mode = "function")(text, tokens))
  }
  text
}

resolve_as_cell_text <- function(value) {
  if (exists("as_cell_text", mode = "function")) {
    return(get("as_cell_text", mode = "function")(value))
  }
  if (length(value) == 0 || is.null(value) || is.na(value)) return("")
  as.character(value)
}

resolve_round_numeric <- function(df, digits) {
  if (exists("round_numeric", mode = "function")) {
    return(get("round_numeric", mode = "function")(df, digits))
  }
  out <- df
  numeric_cols <- sapply(out, is.numeric)
  out[numeric_cols] <- lapply(out[numeric_cols], function(x) round(x, digits))
  out
}

resolve_append_apa_report <- function(path, analysis_label, apa_table, apa_text, analysis_flags = NULL, template_path = NULL, template_context = NULL) {
  if (exists("append_apa_report", mode = "function")) {
    return(get("append_apa_report", mode = "function")(
      path,
      analysis_label,
      apa_table,
      apa_text,
      analysis_flags = analysis_flags,
      template_path = template_path,
      template_context = template_context
    ))
  }
  stop("Missing append_apa_report. Ensure lib/formatting.R is sourced.")
}

resolve_get_run_context <- function() {
  if (exists("get_run_context", mode = "function")) {
    return(get("get_run_context", mode = "function")())
  }
  trailing <- commandArgs(trailingOnly = TRUE)
  commands <- c("Rscript", trailing)
  commands <- commands[nzchar(commands)]
  prompt <- paste(commands, collapse = " ")
  list(prompt = prompt, commands = commands)
}

resolve_append_analysis_log <- function(out_dir, module, prompt, commands, results, options = list(), user_prompt = NULL) {
  if (exists("append_analysis_log", mode = "function")) {
    return(get("append_analysis_log", mode = "function")(
      out_dir,
      module,
      prompt,
      commands,
      results,
      options = options,
      user_prompt = user_prompt
    ))
  }
  cat("Note: append_analysis_log not available; skipping analysis_log.jsonl output.\n")
  invisible(FALSE)
}

resolve_get_user_prompt <- function(opts) {
  if (exists("get_user_prompt", mode = "function")) {
    return(get("get_user_prompt", mode = "function")(opts))
  }
  NULL
}

normalize_analysis <- function(value, default = "icc") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(as.character(val))
  val <- gsub("-", "_", val)
  if (val %in% c("icc", "intra_class", "intraclass")) return("icc")
  if (val %in% c("kappa", "cohen")) return("kappa")
  if (val %in% c("test_retest", "testretest", "retest", "stability")) return("test_retest")
  default
}

normalize_format <- function(value, default = "wide") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(as.character(val))
  if (val %in% c("wide", "long")) return(val)
  default
}

normalize_missing <- function(value, default = "complete") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(as.character(val))
  if (val %in% c("complete", "listwise")) return("complete")
  if (val %in% c("pairwise", "pair")) return("pairwise")
  default
}

normalize_icc_model <- function(value, default = "twoway-random") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(as.character(val))
  val <- gsub("_", "-", val)
  if (val %in% c("oneway", "one-way")) return("oneway")
  if (val %in% c("twoway", "two-way", "twoway-random", "two-way-random", "random")) return("twoway-random")
  if (val %in% c("twoway-mixed", "two-way-mixed", "mixed")) return("twoway-mixed")
  default
}

normalize_icc_type <- function(value, default = "agreement") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(as.character(val))
  if (val %in% c("agreement", "absolute")) return("agreement")
  if (val %in% c("consistency", "consist")) return("consistency")
  default
}

normalize_icc_unit <- function(value, default = "single") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(as.character(val))
  if (val %in% c("single", "individual")) return("single")
  if (val %in% c("average", "mean", "avg")) return("average")
  default
}

normalize_kappa_weight <- function(value, default = "none") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(as.character(val))
  if (val %in% c("none", "unweighted")) return("none")
  if (val %in% c("linear", "lin")) return("linear")
  if (val %in% c("quadratic", "quad")) return("quadratic")
  default
}

normalize_method <- function(value, default = "pearson") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(as.character(val))
  if (val %in% c("pearson", "pear")) return("pearson")
  if (val %in% c("spearman", "rho")) return("spearman")
  default
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

compute_f_bounds <- function(f_stat, df1, df2, conf_level) {
  if (is.na(f_stat) || f_stat <= 0 || is.na(df1) || is.na(df2)) {
    return(list(lower = NA_real_, upper = NA_real_))
  }
  alpha <- 1 - conf_level
  lower <- f_stat / stats::qf(1 - alpha / 2, df1, df2)
  upper <- f_stat / stats::qf(alpha / 2, df1, df2)
  list(lower = lower, upper = upper)
}

compute_icc <- function(values, model, type, unit, conf_level) {
  values <- as.matrix(values)
  n <- nrow(values)
  k <- ncol(values)
  if (n < 2 || k < 2) {
    return(list(
      estimate = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      f_stat = NA_real_,
      df1 = NA_real_,
      df2 = NA_real_,
      p_value = NA_real_,
      n_subjects = n,
      n_raters = k
    ))
  }

  grand_mean <- mean(values)
  row_means <- rowMeans(values)
  col_means <- colMeans(values)

  if (model == "oneway") {
    ss_between <- k * sum((row_means - grand_mean)^2)
    ss_within <- sum((values - row_means)^2)
    df_between <- n - 1
    df_within <- n * (k - 1)
    ms_between <- ss_between / df_between
    ms_within <- ss_within / df_within
    if (is.na(ms_within) || ms_within <= 0) {
      return(list(
        estimate = NA_real_,
        ci_low = NA_real_,
        ci_high = NA_real_,
        f_stat = NA_real_,
        df1 = df_between,
        df2 = df_within,
        p_value = NA_real_,
        n_subjects = n,
        n_raters = k
      ))
    }
    f_stat <- ms_between / ms_within
    df1 <- df_between
    df2 <- df_within

    if (unit == "average") {
      estimate <- (ms_between - ms_within) / ms_between
      denom_adjust <- 0
    } else {
      estimate <- (ms_between - ms_within) / (ms_between + (k - 1) * ms_within)
      denom_adjust <- (k - 1)
    }

    bounds <- compute_f_bounds(f_stat, df1, df2, conf_level)
    if (!is.na(bounds$lower) && !is.na(bounds$upper)) {
      ci_low <- (bounds$lower - 1) / (bounds$lower + denom_adjust)
      ci_high <- (bounds$upper - 1) / (bounds$upper + denom_adjust)
    } else {
      ci_low <- NA_real_
      ci_high <- NA_real_
    }

    p_value <- stats::pf(f_stat, df1, df2, lower.tail = FALSE)

    return(list(
      estimate = estimate,
      ci_low = ci_low,
      ci_high = ci_high,
      f_stat = f_stat,
      df1 = df1,
      df2 = df2,
      p_value = p_value,
      n_subjects = n,
      n_raters = k
    ))
  }

  ss_total <- sum((values - grand_mean)^2)
  ss_rows <- k * sum((row_means - grand_mean)^2)
  ss_cols <- n * sum((col_means - grand_mean)^2)
  ss_error <- ss_total - ss_rows - ss_cols
  if (ss_error < 0) ss_error <- 0

  df_rows <- n - 1
  df_cols <- k - 1
  df_error <- df_rows * df_cols

  ms_rows <- ss_rows / df_rows
  ms_cols <- ss_cols / df_cols
  ms_error <- ss_error / df_error
  if (is.na(ms_error) || ms_error <= 0) {
    return(list(
      estimate = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      f_stat = NA_real_,
      df1 = df_rows,
      df2 = df_error,
      p_value = NA_real_,
      n_subjects = n,
      n_raters = k
    ))
  }

  f_stat <- ms_rows / ms_error
  df1 <- df_rows
  df2 <- df_error

  denom_adjust <- 0
  if (type == "agreement") {
    if (unit == "average") {
      denom_adjust <- (ms_cols - ms_error) / (n * ms_error)
      estimate <- (ms_rows - ms_error) / (ms_rows + (ms_cols - ms_error) / n)
    } else {
      denom_adjust <- (k - 1) + k * (ms_cols - ms_error) / (n * ms_error)
      estimate <- (ms_rows - ms_error) / (ms_rows + (k - 1) * ms_error + k * (ms_cols - ms_error) / n)
    }
  } else {
    if (unit == "average") {
      estimate <- (ms_rows - ms_error) / ms_rows
      denom_adjust <- 0
    } else {
      estimate <- (ms_rows - ms_error) / (ms_rows + (k - 1) * ms_error)
      denom_adjust <- (k - 1)
    }
  }

  bounds <- compute_f_bounds(f_stat, df1, df2, conf_level)
  if (!is.na(bounds$lower) && !is.na(bounds$upper)) {
    ci_low <- (bounds$lower - 1) / (bounds$lower + denom_adjust)
    ci_high <- (bounds$upper - 1) / (bounds$upper + denom_adjust)
  } else {
    ci_low <- NA_real_
    ci_high <- NA_real_
  }

  p_value <- stats::pf(f_stat, df1, df2, lower.tail = FALSE)

  list(
    estimate = estimate,
    ci_low = ci_low,
    ci_high = ci_high,
    f_stat = f_stat,
    df1 = df1,
    df2 = df2,
    p_value = p_value,
    n_subjects = n,
    n_raters = k
  )
}

compute_kappa <- function(x, y, weight = "none") {
  idx <- complete.cases(x, y)
  x <- x[idx]
  y <- y[idx]
  n <- length(x)
  if (n == 0) {
    return(list(
      estimate = NA_real_,
      n = 0,
      n_categories = 0
    ))
  }

  levels <- sort(unique(c(as.character(x), as.character(y))))
  fx <- factor(as.character(x), levels = levels)
  fy <- factor(as.character(y), levels = levels)
  tab <- table(fx, fy)
  k <- length(levels)
  if (k < 2) {
    return(list(
      estimate = NA_real_,
      n = n,
      n_categories = k
    ))
  }

  weights <- matrix(0, nrow = k, ncol = k)
  for (i in seq_len(k)) {
    for (j in seq_len(k)) {
      if (weight == "none") {
        weights[i, j] <- ifelse(i == j, 1, 0)
      } else if (weight == "linear") {
        weights[i, j] <- 1 - abs(i - j) / (k - 1)
      } else {
        weights[i, j] <- 1 - ((i - j) / (k - 1))^2
      }
    }
  }

  observed <- tab / n
  expected <- outer(rowSums(observed), colSums(observed))
  po <- sum(weights * observed)
  pe <- sum(weights * expected)

  estimate <- ifelse(1 - pe == 0, NA_real_, (po - pe) / (1 - pe))

  list(
    estimate = estimate,
    n = n,
    n_categories = k
  )
}

compute_test_retest <- function(x, y, method = "pearson", conf_level = 0.95) {
  idx <- complete.cases(x, y)
  x <- x[idx]
  y <- y[idx]
  n <- length(x)
  if (n < 3) {
    return(list(
      estimate = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      p_value = NA_real_,
      n = n
    ))
  }

  test_args <- list(x = x, y = y, method = method)
  if (method == "spearman") test_args$exact <- FALSE
  test <- suppressWarnings(do.call(stats::cor.test, test_args))
  estimate <- as.numeric(test$estimate)
  p_value <- test$p.value

  estimate <- max(min(estimate, 0.999999), -0.999999)
  z <- atanh(estimate)
  se <- 1 / sqrt(n - 3)
  z_crit <- stats::qnorm(1 - (1 - conf_level) / 2)
  ci_low <- tanh(z - z_crit * se)
  ci_high <- tanh(z + z_crit * se)

  list(
    estimate = estimate,
    ci_low = ci_low,
    ci_high = ci_high,
    p_value = p_value,
    n = n
  )
}

long_to_wide <- function(df, id_var, rater_var, score_var) {
  df <- df[, c(id_var, rater_var, score_var), drop = FALSE]
  df <- df[!is.na(df[[id_var]]) & !is.na(df[[rater_var]]), , drop = FALSE]
  if (nrow(df) == 0) {
    stop("No rows available after removing missing id/rater values.")
  }
  combos <- df[, c(id_var, rater_var), drop = FALSE]
  if (any(duplicated(combos))) {
    stop("Duplicate id/rater combinations found in long data. Aggregate before running reliability.")
  }
  ids <- unique(df[[id_var]])
  raters <- unique(df[[rater_var]])
  mat <- matrix(NA, nrow = length(ids), ncol = length(raters))
  rownames(mat) <- as.character(ids)
  colnames(mat) <- as.character(raters)
  id_idx <- match(df[[id_var]], ids)
  rater_idx <- match(df[[rater_var]], raters)
  mat[cbind(id_idx, rater_idx)] <- df[[score_var]]
  list(matrix = mat, ids = ids, raters = raters)
}

icc_label <- function(model, unit) {
  if (model == "oneway") return(ifelse(unit == "average", "ICC(1,k)", "ICC(1,1)"))
  if (model == "twoway-random") return(ifelse(unit == "average", "ICC(2,k)", "ICC(2,1)"))
  if (model == "twoway-mixed") return(ifelse(unit == "average", "ICC(3,k)", "ICC(3,1)"))
  "ICC"
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

  ci_label <- paste0(round(conf_level * 100), "% CI")
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

  columns <- resolve_normalize_table_columns(
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
        val <- resolve_as_cell_text(row$analysis_label)
      } else if (key == "group") {
        val <- resolve_as_cell_text(row$group)
      } else if (key == "method_label") {
        val <- resolve_as_cell_text(row$method_label)
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
          val <- resolve_as_cell_text(cell)
        }
      }
      row_vals <- c(row_vals, val)
    }
    rows[[length(rows) + 1]] <- row_vals
  }

  filtered <- resolve_drop_empty_columns(columns, rows)
  columns <- filtered$columns
  rows <- filtered$rows

  label_tokens <- list(ci_label = ci_label)
  headers <- vapply(columns, function(col) {
    label <- if (!is.null(col$label) && nzchar(col$label)) col$label else col$key
    resolve_render_template_tokens(label, label_tokens)
  }, character(1))

  resolve_render_markdown_table(headers, rows)
}

build_reliability_note_tokens <- function(summary_df, analysis, conf_level, missing_method, icc_model, icc_type, icc_unit, kappa_weight, retest_method) {
  ci_label <- paste0(round(conf_level * 100), "% CI")
  missing_note <- paste0("Missing values handled ", missing_method, ".")
  icc_note <- ""
  kappa_note <- ""
  retest_note <- ""
  if (analysis == "icc") {
    icc_note <- paste0("ICC model = ", icc_model, ", type = ", icc_type, ", unit = ", icc_unit, ".")
  }
  if (analysis == "kappa") {
    kappa_note <- if (kappa_weight == "none") "Unweighted kappa." else paste0("Kappa weights: ", kappa_weight, ".")
  }
  if (analysis == "test_retest") {
    retest_note <- paste0("Test-retest method: ", retest_method, ".")
  }

  ci_note <- ""
  if (nrow(summary_df) > 0 && any(!is.na(summary_df$ci_low))) {
    if (analysis == "test_retest") {
      ci_note <- paste(ci_label, "computed via Fisher's z.")
    } else if (analysis == "icc") {
      ci_note <- paste(ci_label, "computed from F distributions.")
    }
  }

  note_parts <- c(missing_note, icc_note, kappa_note, retest_note, ci_note)
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
  rows <- list()

  for (i in seq_len(nrow(display))) {
    row <- display[i, , drop = FALSE]
    group_label <- if (row$group == "") "Overall" else paste("Group", row$group)

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
      ci_text <- paste0(", ", round(conf_level * 100), "% CI ", format_ci(row$ci_low, row$ci_high, digits))
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
      analysis = resolve_as_cell_text(row$analysis),
      analysis_label = resolve_as_cell_text(row$analysis_label),
      group = resolve_as_cell_text(row$group),
      group_label = group_label,
      method_label = resolve_as_cell_text(row$method_label),
      icc_label = resolve_as_cell_text(row$icc_label),
      estimate = estimate_text,
      ci = format_ci(row$ci_low, row$ci_high, digits),
      ci_text = ci_text,
      p = format_p_text(row$p_value),
      n = ifelse(is.na(row$n), "NA", as.character(row$n)),
      n_raters = ifelse(is.na(row$n_raters), "NA", as.character(row$n_raters)),
      var1 = resolve_as_cell_text(row$var1),
      var2 = resolve_as_cell_text(row$var2),
      missing_n = ifelse(is.na(row$missing_n), "NA", as.character(row$missing_n)),
      missing_pct = missing_pct,
      missing_text = missing_text,
      full_sentence = line
    )
  }

  rows
}

format_apa_text <- function(summary_df, digits, conf_level) {
  rows <- build_reliability_narrative_rows(summary_df, digits, conf_level)
  lines <- vapply(rows, function(row) row$full_sentence, character(1))
  paste(lines, collapse = "\n")
}

format_apa_table <- function(summary_df, digits, note_text, conf_level) {
  table_body <- build_reliability_table_body(summary_df, digits, conf_level, table_spec = NULL)
  header <- "Table 1\nReliability analysis\n\n"
  note_line <- if (nzchar(note_text)) paste0("Note. ", note_text) else "Note."
  paste0(header, table_body, "\n", note_line, "\n")
}

emit_input_issue <- function(out_dir, opts, message, details = list(), status = "invalid_input", expected = FALSE) {
  log_default <- resolve_config_value("defaults.log", TRUE)
  if (resolve_parse_bool(opts$log, default = log_default)) {
    ctx <- resolve_get_run_context()
    resolve_append_analysis_log(
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
      user_prompt = resolve_get_user_prompt(opts)
    )
  }
  if (expected) {
    cat("EXPECTED_NEGATIVE: reliability invalid input\n")
    quit(status = 0)
  }
  stop(message)
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  opts <- resolve_parse_args(args)

  if (!is.null(opts$help)) {
    print_usage()
    quit(status = 0)
  }

  if (!is.null(opts$interactive)) {
    opts <- modifyList(opts, interactive_options())
  }

  digits_default <- resolve_config_value("defaults.digits", 2)
  log_default <- resolve_config_value("defaults.log", TRUE)
  analysis_default <- resolve_config_value("modules.reliability.analysis", "icc")
  format_default <- resolve_config_value("modules.reliability.format", "wide")
  missing_default <- resolve_config_value("modules.reliability.missing", "complete")
  conf_default <- resolve_config_value("modules.reliability.conf_level", 0.95)
  coerce_default <- resolve_config_value("modules.reliability.coerce", FALSE)
  icc_model_default <- resolve_config_value("modules.reliability.icc_model", "twoway-random")
  icc_type_default <- resolve_config_value("modules.reliability.icc_type", "agreement")
  icc_unit_default <- resolve_config_value("modules.reliability.icc_unit", "single")
  kappa_weight_default <- resolve_config_value("modules.reliability.kappa_weight", "none")
  method_default <- resolve_config_value("modules.reliability.method", "pearson")

  digits <- if (!is.null(opts$digits)) as.numeric(opts$digits) else digits_default
  analysis <- normalize_analysis(opts$analysis, default = analysis_default)
  format <- normalize_format(opts$format, default = format_default)
  missing_method <- normalize_missing(opts$missing, default = missing_default)
  conf_level <- if (!is.null(opts$`conf-level`)) as.numeric(opts$`conf-level`) else conf_default
  coerce_flag <- resolve_parse_bool(opts$coerce, default = coerce_default)
  expect_invalid <- resolve_parse_bool(opts$`expect-invalid`, default = FALSE)

  if (analysis == "icc" && missing_method != "complete") {
    missing_method <- "complete"
  }

  icc_model <- normalize_icc_model(opts$`icc-model`, default = icc_model_default)
  icc_type <- normalize_icc_type(opts$`icc-type`, default = icc_type_default)
  icc_unit <- normalize_icc_unit(opts$`icc-unit`, default = icc_unit_default)
  kappa_weight <- normalize_kappa_weight(opts$`kappa-weight`, default = kappa_weight_default)
  retest_method <- normalize_method(opts$method, default = method_default)

  df <- resolve_load_dataframe(opts)
  out_dir <- resolve_get_workspace_out_dir(df)

  group_var <- if (!is.null(opts$group) && opts$group != "") opts$group else NULL
  if (!is.null(group_var) && !(group_var %in% names(df))) {
    stop("Grouping variable not found in data frame.")
  }

  if (analysis == "icc" && icc_model == "twoway-mixed" && icc_type == "agreement") {
    icc_type <- "consistency"
  }

  if (analysis %in% c("kappa", "test_retest") && format == "wide") {
    vars_default <- if (analysis == "kappa") "non-numeric" else "numeric"
  } else {
    vars_default <- "numeric"
  }

  summary_rows <- list()

  group_levels <- if (!is.null(group_var)) unique(df[[group_var]]) else ""
  for (g in group_levels) {
    if (!is.null(group_var)) {
      idx <- if (is.na(g)) is.na(df[[group_var]]) else df[[group_var]] == g
      df_sub <- df[idx, , drop = FALSE]
      group_label <- ifelse(is.na(g), "NA", as.character(g))
    } else {
      df_sub <- df
      group_label <- ""
    }

    wide_data <- NULL
    var_names <- character(0)

    if (format == "long") {
      id_var <- if (!is.null(opts$id) && opts$id != "") opts$id else NULL
      rater_var <- if (!is.null(opts$rater) && opts$rater != "") opts$rater else NULL
      score_var <- if (!is.null(opts$score) && opts$score != "") opts$score else NULL

      if (is.null(id_var) || is.null(rater_var) || is.null(score_var)) {
        emit_input_issue(out_dir, opts, "Long format requires --id, --rater, and --score.", expected = expect_invalid)
      }

      if (!(id_var %in% names(df_sub))) {
        emit_input_issue(out_dir, opts, paste("ID variable not found:", id_var), expected = expect_invalid)
      }
      if (!(rater_var %in% names(df_sub))) {
        emit_input_issue(out_dir, opts, paste("Rater variable not found:", rater_var), expected = expect_invalid)
      }
      if (!(score_var %in% names(df_sub))) {
        emit_input_issue(out_dir, opts, paste("Score variable not found:", score_var), expected = expect_invalid)
      }

      long_result <- long_to_wide(df_sub, id_var, rater_var, score_var)
      wide_data <- long_result$matrix
      var_names <- colnames(wide_data)
    } else {
      vars <- resolve_select_variables(df_sub, opts$vars, group_var, default = vars_default)
      if (analysis == "icc" && length(vars) < 2) {
        emit_input_issue(out_dir, opts, "ICC requires at least two variables in wide format.", expected = expect_invalid)
      }
      if (analysis %in% c("kappa", "test_retest") && length(vars) != 2) {
        emit_input_issue(out_dir, opts, "Kappa and test-retest require exactly two variables in wide format.", expected = expect_invalid)
      }
      wide_data <- df_sub[, vars, drop = FALSE]
      var_names <- vars
    }

    if (analysis %in% c("icc", "test_retest")) {
      wide_data <- coerce_dataframe(as.data.frame(wide_data), var_names, coerce_flag)
      for (var in var_names) {
        if (!is.numeric(wide_data[[var]])) {
          emit_input_issue(out_dir, opts, paste("Variable is not numeric:", var), expected = expect_invalid)
        }
      }
    }

    missing_summary <- compute_missing_summary(as.data.frame(wide_data))
    complete_data <- wide_data[complete.cases(wide_data), , drop = FALSE]

    if (analysis == "icc") {
      icc_stats <- compute_icc(complete_data, icc_model, icc_type, icc_unit, conf_level)
      icc_code <- icc_label(icc_model, icc_unit)
      row <- data.frame(
        analysis = analysis,
        analysis_label = analysis_label(analysis),
        group = group_label,
        method_label = "",
        icc_label = icc_code,
        model = icc_model,
        type = icc_type,
        unit = icc_unit,
        weight = "",
        method = "",
        var1 = "",
        var2 = "",
        estimate = icc_stats$estimate,
        ci_low = icc_stats$ci_low,
        ci_high = icc_stats$ci_high,
        p_value = icc_stats$p_value,
        f_stat = icc_stats$f_stat,
        df1 = icc_stats$df1,
        df2 = icc_stats$df2,
        n = icc_stats$n_subjects,
        n_raters = icc_stats$n_raters,
        missing_n = missing_summary$missing_n,
        missing_pct = missing_summary$missing_pct,
        stringsAsFactors = FALSE
      )
      row$method_label <- build_method_label(row)
      summary_rows[[length(summary_rows) + 1]] <- row
    } else if (analysis == "kappa") {
      if (ncol(wide_data) != 2) {
        emit_input_issue(out_dir, opts, "Kappa requires exactly two raters/variables.", expected = expect_invalid)
      }
      kappa_stats <- compute_kappa(wide_data[[1]], wide_data[[2]], kappa_weight)
      row <- data.frame(
        analysis = analysis,
        analysis_label = analysis_label(analysis),
        group = group_label,
        method_label = "",
        icc_label = "",
        model = "",
        type = "",
        unit = "",
        weight = kappa_weight,
        method = "",
        var1 = var_names[1],
        var2 = var_names[2],
        estimate = kappa_stats$estimate,
        ci_low = NA_real_,
        ci_high = NA_real_,
        p_value = NA_real_,
        f_stat = NA_real_,
        df1 = NA_real_,
        df2 = NA_real_,
        n = kappa_stats$n,
        n_raters = 2,
        missing_n = missing_summary$missing_n,
        missing_pct = missing_summary$missing_pct,
        stringsAsFactors = FALSE
      )
      row$method_label <- build_method_label(row)
      summary_rows[[length(summary_rows) + 1]] <- row
    } else {
      if (ncol(wide_data) != 2) {
        emit_input_issue(out_dir, opts, "Test-retest requires exactly two raters/variables.", expected = expect_invalid)
      }
      retest_stats <- compute_test_retest(wide_data[[1]], wide_data[[2]], retest_method, conf_level)
      row <- data.frame(
        analysis = analysis,
        analysis_label = analysis_label(analysis),
        group = group_label,
        method_label = "",
        icc_label = "",
        model = "",
        type = "",
        unit = "",
        weight = "",
        method = retest_method,
        var1 = var_names[1],
        var2 = var_names[2],
        estimate = retest_stats$estimate,
        ci_low = retest_stats$ci_low,
        ci_high = retest_stats$ci_high,
        p_value = retest_stats$p_value,
        f_stat = NA_real_,
        df1 = NA_real_,
        df2 = NA_real_,
        n = retest_stats$n,
        n_raters = 2,
        missing_n = missing_summary$missing_n,
        missing_pct = missing_summary$missing_pct,
        stringsAsFactors = FALSE
      )
      row$method_label <- build_method_label(row)
      summary_rows[[length(summary_rows) + 1]] <- row
    }
  }

  summary_df <- do.call(rbind, summary_rows)

  template_override <- resolve_template_override(opts$template, module = "reliability")
  template_path <- if (!is.null(template_override)) {
    template_override
  } else {
    resolve_get_template_path("reliability.default", "reliability/default-template.md")
  }
  template_meta <- resolve_get_template_meta(template_path)
  apa_report_path <- file.path(out_dir, "apa_report.md")

  note_tokens <- build_reliability_note_tokens(
    summary_df,
    analysis,
    conf_level,
    missing_method,
    icc_model,
    icc_type,
    icc_unit,
    kappa_weight,
    retest_method
  )

  apa_text <- format_apa_text(summary_df, digits, conf_level)
  apa_table <- format_apa_table(summary_df, digits, note_tokens$note_default, conf_level)
  table_body <- build_reliability_table_body(summary_df, digits, conf_level, template_meta$table)
  narrative_rows <- build_reliability_narrative_rows(summary_df, digits, conf_level)

  template_context <- list(
    tokens = c(
      list(
        table_body = table_body,
        narrative_default = apa_text
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
    "icc-type" = if (analysis == "icc") icc_type else NULL,
    "icc-unit" = if (analysis == "icc") icc_unit else NULL,
    "kappa-weight" = if (analysis == "kappa") kappa_weight else NULL,
    method = if (analysis == "test_retest") retest_method else NULL,
    "conf-level" = conf_level,
    coerce = if (analysis %in% c("icc", "test_retest")) coerce_flag else NULL,
    digits = digits
  )

  resolve_append_apa_report(
    apa_report_path,
    "Reliability analysis",
    apa_table,
    apa_text,
    analysis_flags = analysis_flags,
    template_path = template_path,
    template_context = template_context
  )

  cat("Wrote:\n")
  cat("- ", apa_report_path, "\n", sep = "")

  if (resolve_parse_bool(opts$log, default = log_default)) {
    ctx <- resolve_get_run_context()
    resolve_append_analysis_log(
      out_dir,
      module = "reliability",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(summary_df = summary_df),
      options = list(
        analysis = analysis,
        format = format,
        vars = if (format == "wide") var_names else NULL,
        id = if (format == "long") opts$id else NULL,
        rater = if (format == "long") opts$rater else NULL,
        score = if (format == "long") opts$score else NULL,
        group = group_var,
        missing = missing_method,
        icc_model = icc_model,
        icc_type = icc_type,
        icc_unit = icc_unit,
        kappa_weight = kappa_weight,
        method = retest_method,
        conf_level = conf_level,
        coerce = coerce_flag,
        digits = digits
      ),
      user_prompt = resolve_get_user_prompt(opts)
    )
  }
}

main()
