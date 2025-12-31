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
source(file.path(bootstrap_dir, "lib", "paths.R"))
source_lib("cli.R")
source_lib("config.R")
source_lib("io.R")
source_lib("data_utils.R")
source_lib("formatting.R")


# Static analysis aliases for source_lib-defined functions.
render_output_path <- get("render_output_path", mode = "function")
add_variable_label_column <- get("add_variable_label_column", mode = "function")
resolve_label_metadata <- get("resolve_label_metadata", mode = "function")
resolve_row_display <- get("resolve_row_display", mode = "function")
resolve_variable_label <- get("resolve_variable_label", mode = "function")
source_lib <- get("source_lib", mode = "function")

print_usage <- function() {
  cat("Missing data assessment and handling (base R)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript missings.R --csv data.csv [--vars var1,var2]\n")
  cat("  Rscript missings.R --sav data.sav [--vars var1,var2]\n")
  cat("  Rscript missings.R --rds data.rds [--vars var1,var2]\n")
  cat("  Rscript missings.R --rdata data.RData --df data_frame_name [--vars var1,var2]\n")
  cat("  Rscript missings.R --parquet data.parquet [--vars var1,var2]\n")
  cat("  Rscript missings.R --interactive\n")
  cat("\n")
  cat("Options:\n")
  cat("  --csv PATH               CSV input file\n")
  cat("  --sav PATH               SPSS .sav input file\n")
  cat("  --sep VALUE              CSV separator (default: ,)\n")
  cat("  --header TRUE/FALSE      CSV header (default: TRUE)\n")
  cat("  --rds PATH               RDS input file (data frame)\n")
  cat("  --rdata PATH             RData input file\n")
  cat("  --parquet PATH           Parquet input file\n")
  cat("  --df NAME                Data frame object name in RData\n")
  cat("  --vars LIST              Comma-separated variable names (default: all columns)\n")
  cat("  --method VALUE           auto|listwise|impute|indicator|drop (default: auto)\n")
  cat("  --low-threshold VALUE    Low missingness threshold (default: 0.05)\n")
  cat("  --moderate-threshold VALUE Moderate missingness threshold (default: 0.20)\n")
  cat("  --high-threshold VALUE   High missingness threshold (default: 0.40)\n")
  cat("  --drop-threshold VALUE   Drop-variable threshold (default: 0.60)\n")
  cat("  --indicator-threshold VALUE Add missingness indicators above threshold (default: 0.30)\n")
  cat("  --indicator-suffix TEXT  Suffix for missingness indicators (default: _miss)\n")
  cat("  --skew-threshold VALUE   Skewness cutoff for numeric imputation (default: 1.00)\n")
  cat("  --max-patterns N         Max missingness patterns to list (default: 10)\n")
  cat("  --digits N               Rounding digits (default: 2)\n")
  cat("  --template REF           Template path or template key (optional)\n")
  cat("  --user-prompt TEXT       Original AI user prompt for logging (optional)\n")
  cat("  --log TRUE/FALSE         Write analysis_log.jsonl (default: TRUE)\n")
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

  opts$vars <- resolve_prompt("Variables (comma-separated, blank for all)", "")
  method_default <- resolve_config_value("modules.missings.method", "auto")
  opts$method <- resolve_prompt("Method (auto/listwise/impute/indicator/drop)", method_default)
  low_default <- resolve_config_value("modules.missings.low_threshold", 0.05)
  moderate_default <- resolve_config_value("modules.missings.moderate_threshold", 0.2)
  high_default <- resolve_config_value("modules.missings.high_threshold", 0.4)
  drop_default <- resolve_config_value("modules.missings.drop_threshold", 0.6)
  indicator_default <- resolve_config_value("modules.missings.indicator_threshold", 0.3)
  indicator_suffix_default <- resolve_config_value("modules.missings.indicator_suffix", "_miss")
  skew_default <- resolve_config_value("modules.missings.skew_threshold", 1.0)
  max_patterns_default <- resolve_config_value("modules.missings.max_patterns", 10)
  digits_default <- resolve_config_value("defaults.digits", 2)

  opts$`low-threshold` <- resolve_prompt("Low threshold (0-1)", as.character(low_default))
  opts$`moderate-threshold` <- resolve_prompt("Moderate threshold (0-1)", as.character(moderate_default))
  opts$`high-threshold` <- resolve_prompt("High threshold (0-1)", as.character(high_default))
  opts$`drop-threshold` <- resolve_prompt("Drop threshold (0-1)", as.character(drop_default))
  opts$`indicator-threshold` <- resolve_prompt("Indicator threshold (0-1)", as.character(indicator_default))
  opts$`indicator-suffix` <- resolve_prompt("Indicator suffix", indicator_suffix_default)
  opts$`skew-threshold` <- resolve_prompt("Skew threshold", as.character(skew_default))
  opts$`max-patterns` <- resolve_prompt("Max patterns", as.character(max_patterns_default))
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

resolve_ensure_out_dir <- function(path) {
  if (exists("ensure_out_dir", mode = "function")) {
    return(get("ensure_out_dir", mode = "function")(path))
  }
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  path
}

resolve_load_dataframe <- function(opts) {
  if (exists("load_dataframe", mode = "function")) {
    return(get("load_dataframe", mode = "function")(opts, lock_safe = TRUE))
  }
  stop("Missing load_dataframe. Ensure lib/io.R is sourced.")
}


resolve_get_workspace_out_dir <- function(df) {
  if (exists("get_workspace_out_dir", mode = "function")) {
    return(get("get_workspace_out_dir", mode = "function")(df))
  }
  stop("Missing get_workspace_out_dir. Ensure lib/io.R is sourced.")
}

resolve_write_parquet_data <- function(df, path) {
  if (exists("write_parquet_data", mode = "function")) {
    return(get("write_parquet_data", mode = "function")(df, path))
  }
  stop("Missing write_parquet_data. Ensure lib/io.R is sourced.")
}

resolve_backup_workspace_parquet <- function(path) {
  if (exists("backup_workspace_parquet", mode = "function")) {
    return(get("backup_workspace_parquet", mode = "function")(path))
  }
  ""
}

resolve_select_variables <- function(df, vars, group_var = NULL, default = "all", include_numeric = FALSE) {
  if (exists("select_variables", mode = "function")) {
    return(get("select_variables", mode = "function")(
      df,
      vars,
      group_var = group_var,
      default = default,
      include_numeric = include_numeric
    ))
  }
  available <- names(df)
  if (is.null(vars) || vars == "") {
    selected <- available
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

resolve_append_nlss_report <- function(path, analysis_label, nlss_table, nlss_text, analysis_flags = NULL, template_path = NULL, template_context = NULL) {
  if (exists("append_nlss_report", mode = "function")) {
    return(get("append_nlss_report", mode = "function")(
      path,
      analysis_label,
      nlss_table,
      nlss_text,
      analysis_flags = analysis_flags,
      template_path = template_path,
      template_context = template_context
    ))
  }
  stop("Missing report formatter. Ensure lib/formatting.R is sourced.")
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

resolve_round_numeric <- function(df, digits) {
  if (exists("round_numeric", mode = "function")) {
    return(get("round_numeric", mode = "function")(df, digits))
  }
  out <- df
  numeric_cols <- sapply(out, is.numeric)
  out[numeric_cols] <- lapply(out[numeric_cols], function(x) round(x, digits))
  out
}

resolve_format_percent <- function(value, digits) {
  if (exists("format_percent", mode = "function")) {
    return(get("format_percent", mode = "function")(value, digits))
  }
  if (is.na(value)) return("")
  format(round(value, digits), nsmall = digits, trim = TRUE)
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


resolve_get_template_path <- function(key, default_relative = NULL) {
  if (exists("resolve_template_path", mode = "function")) {
    return(get("resolve_template_path", mode = "function")(key, default_relative))
  }
  NULL
}

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

resolve_as_cell_text <- function(value) {
  if (exists("as_cell_text", mode = "function")) {
    return(get("as_cell_text", mode = "function")(value))
  }
  if (is.null(value) || length(value) == 0 || is.na(value)) return("")
  as.character(value)
}

resolve_get_next_table_number <- function(path) {
  if (exists("get_next_table_number", mode = "function")) {
    return(get("get_next_table_number", mode = "function")(path))
  }
  1
}

parse_numeric <- function(value, default = NULL) {
  if (is.null(value) || is.logical(value) || value == "") return(default)
  num <- suppressWarnings(as.numeric(value))
  if (is.na(num)) stop("Invalid numeric value: ", value)
  num
}

parse_method <- function(value, default = "auto") {
  if (is.null(value) || is.logical(value) || value == "") return(default)
  method <- tolower(as.character(value))
  valid <- c("auto", "listwise", "impute", "indicator", "drop")
  if (!method %in% valid) {
    stop("Invalid --method. Use one of: ", paste(valid, collapse = ", "))
  }
  method
}

validate_threshold <- function(value, label) {
  if (is.na(value)) return(invisible(TRUE))
  if (value < 0 || value > 1) {
    stop(label, " must be between 0 and 1.")
  }
  invisible(TRUE)
}

detect_type <- function(vec) {
  if (inherits(vec, "Date") || inherits(vec, "POSIXct") || inherits(vec, "POSIXlt")) {
    return("datetime")
  }
  if (is.numeric(vec)) return("numeric")
  if (is.logical(vec)) return("logical")
  if (is.factor(vec)) return("factor")
  if (is.character(vec)) return("character")
  class(vec)[1]
}

compute_skewness <- function(values) {
  vals <- values[!is.na(values)]
  if (length(vals) < 3) return(NA_real_)
  s <- sd(vals)
  if (is.na(s) || s == 0) return(0)
  m <- mean(vals)
  mean((vals - m)^3) / (s^3)
}

format_num <- function(value, digits) {
  if (is.na(value)) return("")
  format(round(value, digits), nsmall = digits, trim = TRUE)
}

format_impute_value <- function(value, digits) {
  if (is.null(value) || length(value) == 0 || is.na(value)) return("")
  if (inherits(value, "Date") || inherits(value, "POSIXct") || inherits(value, "POSIXlt")) {
    return(format(value))
  }
  if (is.numeric(value)) return(format_num(value, digits))
  as.character(value)
}

impute_numeric <- function(vec, skew_threshold) {
  values <- vec[!is.na(vec)]
  if (length(values) == 0) {
    return(list(vec = vec, method = "", value = NA, note = "all missing"))
  }
  is_date <- inherits(vec, "Date")
  is_time <- inherits(vec, "POSIXct") || inherits(vec, "POSIXlt")
  numeric_vals <- if (is_date || is_time) as.numeric(values) else values
  skew <- compute_skewness(numeric_vals)
  use_median <- !is.na(skew) && abs(skew) > skew_threshold
  method <- if (use_median) "median" else "mean"
  impute_val <- if (use_median) median(numeric_vals) else mean(numeric_vals)
  if (is_date) impute_val <- as.Date(impute_val, origin = "1970-01-01")
  if (is_time) impute_val <- as.POSIXct(impute_val, origin = "1970-01-01", tz = attr(vec, "tzone"))
  out <- vec
  out[is.na(out)] <- impute_val
  list(vec = out, method = method, value = impute_val, note = "")
}

impute_mode <- function(vec) {
  values <- vec[!is.na(vec)]
  if (length(values) == 0) {
    return(list(vec = vec, method = "", value = NA, note = "all missing"))
  }
  counts <- sort(table(values), decreasing = TRUE)
  mode_val <- names(counts)[1]
  out <- vec
  if (is.logical(out)) {
    mode_val <- tolower(mode_val) %in% c("true", "t", "1", "yes", "y")
  }
  if (is.factor(out)) out <- as.character(out)
  out[is.na(out)] <- mode_val
  if (is.factor(vec)) {
    out <- factor(out, levels = levels(vec), ordered = is.ordered(vec))
  }
  list(vec = out, method = "mode", value = mode_val, note = "")
}

ensure_unique_name <- function(name, existing) {
  if (!(name %in% existing)) return(name)
  idx <- 1
  candidate <- paste0(name, "_", idx)
  while (candidate %in% existing) {
    idx <- idx + 1
    candidate <- paste0(name, "_", idx)
  }
  candidate
}

map_variable_labels <- function(vars, labels) {
  if (length(vars) == 0) return(vars)
  vapply(vars, function(name) resolve_variable_label(labels, name), character(1))
}

map_variable_list_label <- function(text, labels) {
  if (is.null(text) || length(text) == 0 || is.na(text[1])) return("")
  value <- as.character(text[1])
  if (!nzchar(value)) return(value)
  if (value %in% c("None", "Multiple")) return(value)
  parts <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
  if (length(parts) == 0) return(value)
  mapped <- vapply(parts, function(part) resolve_variable_label(labels, part), character(1))
  paste(mapped, collapse = ", ")
}

build_patterns_df <- function(df, vars, max_patterns) {
  if (length(vars) == 0 || nrow(df) == 0) {
    return(list(
      patterns_df = data.frame(pattern = character(0), missing_vars = character(0), missing_count = integer(0), n = integer(0), pct_total = numeric(0), stringsAsFactors = FALSE),
      truncated = FALSE
    ))
  }
  miss_mat <- is.na(df[, vars, drop = FALSE])
  pattern_strings <- apply(miss_mat, 1, function(row) paste(ifelse(row, "M", "O"), collapse = ""))
  counts <- sort(table(pattern_strings), decreasing = TRUE)
  patterns <- names(counts)
  total_n <- nrow(df)
  rows <- list()
  for (pat in patterns) {
    idx <- which(pat == patterns)
    missing_idx <- unlist(strsplit(pat, "", fixed = TRUE)) == "M"
    missing_vars <- vars[missing_idx]
    rows[[length(rows) + 1]] <- data.frame(
      pattern = pat,
      missing_vars = ifelse(length(missing_vars) == 0, "None", paste(missing_vars, collapse = ", ")),
      missing_count = sum(missing_idx),
      n = as.integer(counts[[idx]]),
      pct_total = ifelse(total_n > 0, as.integer(counts[[idx]]) / total_n * 100, NA_real_),
      stringsAsFactors = FALSE
    )
  }
  patterns_df <- do.call(rbind, rows)
  truncated <- FALSE
  if (nrow(patterns_df) > max_patterns) {
    truncated <- TRUE
    keep <- patterns_df[1:max_patterns, , drop = FALSE]
    other <- patterns_df[(max_patterns + 1):nrow(patterns_df), , drop = FALSE]
    other_n <- sum(other$n)
    other_pct <- ifelse(total_n > 0, other_n / total_n * 100, NA_real_)
    other_row <- data.frame(
      pattern = "Other patterns",
      missing_vars = "Multiple",
      missing_count = NA_integer_,
      n = as.integer(other_n),
      pct_total = other_pct,
      stringsAsFactors = FALSE
    )
    patterns_df <- rbind(keep, other_row)
  }
  list(patterns_df = patterns_df, truncated = truncated)
}

build_missing_table_body <- function(summary_df, digits, table_spec = NULL) {
  display <- resolve_round_numeric(summary_df, digits)
  default_columns <- list(
    list(key = "variable", label = "Variable"),
    list(key = "type", label = "Type"),
    list(key = "missing_n", label = "Missing n"),
    list(key = "missing_pct", label = "Missing %"),
    list(key = "decision", label = "Decision"),
    list(key = "impute_method", label = "Impute method", drop_if_empty = TRUE),
    list(key = "impute_value", label = "Impute value", drop_if_empty = TRUE),
    list(key = "indicator", label = "Indicator", drop_if_empty = TRUE)
  )
  columns <- resolve_normalize_table_columns(
    if (!is.null(table_spec$columns)) table_spec$columns else NULL,
    default_columns
  )

  rows <- list()
  if (nrow(display) == 0) {
    rows <- list(list("No variables", "", "", "", "", "", "", ""))
  } else {
    for (i in seq_len(nrow(display))) {
      row_vals <- character(0)
      row <- display[i, , drop = FALSE]
      for (col in columns) {
        key <- col$key
        val <- ""
        if (key %in% c("variable", "indicator")) {
          val <- resolve_row_display(row, key)
        } else if (key %in% c("type", "decision", "impute_method", "impute_value")) {
          val <- resolve_as_cell_text(row[[key]][1])
        } else if (key %in% c("missing_n", "total_n")) {
          val <- ifelse(is.na(row[[key]][1]), "", as.character(row[[key]][1]))
        } else if (key %in% c("missing_pct")) {
          val <- resolve_format_percent(row[[key]][1], digits)
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
  }

  filtered <- resolve_drop_empty_columns(columns, rows)
  columns <- filtered$columns
  rows <- filtered$rows
  headers <- vapply(columns, function(col) {
    if (!is.null(col$label) && nzchar(col$label)) col$label else col$key
  }, character(1))
  list(
    body = resolve_render_markdown_table(headers, rows),
    columns = vapply(columns, function(col) col$key, character(1))
  )
}

build_patterns_table_body <- function(patterns_df, digits, table_spec = NULL) {
  display <- resolve_round_numeric(patterns_df, digits)
  default_columns <- list(
    list(key = "pattern", label = "Pattern"),
    list(key = "missing_vars", label = "Missing variables"),
    list(key = "n", label = "n"),
    list(key = "pct_total", label = "%"),
    list(key = "missing_count", label = "Missing count", drop_if_empty = TRUE)
  )
  columns <- resolve_normalize_table_columns(
    if (!is.null(table_spec$columns)) table_spec$columns else NULL,
    default_columns
  )

  rows <- list()
  if (nrow(display) == 0) {
    rows <- list(list("No patterns", "", "", "", ""))
  } else {
    for (i in seq_len(nrow(display))) {
      row <- display[i, , drop = FALSE]
      row_vals <- character(0)
      for (col in columns) {
        key <- col$key
        val <- ""
        if (key == "pattern") {
          val <- resolve_as_cell_text(row[[key]][1])
        } else if (key == "missing_vars") {
          val <- resolve_row_display(row, "missing_vars")
        } else if (key == "n" || key == "missing_count") {
          val <- ifelse(is.na(row[[key]][1]), "", as.character(row[[key]][1]))
        } else if (key == "pct_total") {
          val <- resolve_format_percent(row[[key]][1], digits)
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
  }

  filtered <- resolve_drop_empty_columns(columns, rows)
  columns <- filtered$columns
  rows <- filtered$rows
  headers <- vapply(columns, function(col) {
    if (!is.null(col$label) && nzchar(col$label)) col$label else col$key
  }, character(1))
  list(
    body = resolve_render_markdown_table(headers, rows),
    columns = vapply(columns, function(col) col$key, character(1))
  )
}

build_narrative_rows <- function(summary_df, digits) {
  rows <- list()
  if (nrow(summary_df) == 0) return(rows)
  for (i in seq_len(nrow(summary_df))) {
    row <- summary_df[i, , drop = FALSE]
    var_display <- resolve_row_display(row, "variable")
    indicator_display <- resolve_row_display(row, "indicator")
    missing_text <- ifelse(is.na(row$missing_pct), "NA", resolve_format_percent(row$missing_pct, digits))
    line <- paste0(
      var_display,
      ": missing = ",
      row$missing_n,
      " (", missing_text, "%)."
    )
    if (nzchar(row$decision)) {
      line <- paste0(line, " Decision: ", row$decision, ".")
    }
    if (nzchar(row$impute_method) && nzchar(row$impute_value)) {
      line <- paste0(line, " Impute ", row$impute_method, " = ", row$impute_value, ".")
    } else if (nzchar(row$impute_method)) {
      line <- paste0(line, " Impute ", row$impute_method, ".")
    }
    if (nzchar(indicator_display)) {
      line <- paste0(line, " Indicator: ", indicator_display, ".")
    }
    rows[[length(rows) + 1]] <- list(
      variable = var_display,
      missing_n = row$missing_n,
      missing_pct = row$missing_pct,
      decision = row$decision,
      impute_method = row$impute_method,
      impute_value = row$impute_value,
      indicator = indicator_display,
      full_sentence = line
    )
  }
  rows
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
  vars_default <- resolve_config_value("modules.missings.vars_default", "all")
  method_default <- resolve_config_value("modules.missings.method", "auto")
  low_default <- resolve_config_value("modules.missings.low_threshold", 0.05)
  moderate_default <- resolve_config_value("modules.missings.moderate_threshold", 0.2)
  high_default <- resolve_config_value("modules.missings.high_threshold", 0.4)
  drop_default <- resolve_config_value("modules.missings.drop_threshold", 0.6)
  indicator_default <- resolve_config_value("modules.missings.indicator_threshold", 0.3)
  indicator_suffix_default <- resolve_config_value("modules.missings.indicator_suffix", "_miss")
  skew_default <- resolve_config_value("modules.missings.skew_threshold", 1.0)
  max_patterns_default <- resolve_config_value("modules.missings.max_patterns", 10)

  digits <- if (!is.null(opts$digits)) as.numeric(opts$digits) else digits_default
  method <- parse_method(opts$method, method_default)
  low_threshold <- parse_numeric(opts$`low-threshold`, low_default)
  moderate_threshold <- parse_numeric(opts$`moderate-threshold`, moderate_default)
  high_threshold <- parse_numeric(opts$`high-threshold`, high_default)
  drop_threshold <- parse_numeric(opts$`drop-threshold`, drop_default)
  indicator_threshold <- parse_numeric(opts$`indicator-threshold`, indicator_default)
  indicator_suffix <- if (!is.null(opts$`indicator-suffix`)) as.character(opts$`indicator-suffix`) else indicator_suffix_default
  skew_threshold <- parse_numeric(opts$`skew-threshold`, skew_default)
  max_patterns <- if (!is.null(opts$`max-patterns`)) as.integer(opts$`max-patterns`) else max_patterns_default

  validate_threshold(low_threshold, "low-threshold")
  validate_threshold(moderate_threshold, "moderate-threshold")
  validate_threshold(high_threshold, "high-threshold")
  validate_threshold(drop_threshold, "drop-threshold")
  validate_threshold(indicator_threshold, "indicator-threshold")

  if (is.na(max_patterns) || max_patterns < 1) {
    stop("--max-patterns must be a positive integer.")
  }

  df <- resolve_load_dataframe(opts)
  out_dir <- resolve_get_workspace_out_dir(df)
  workspace_parquet_path <- attr(df, "workspace_parquet_path")
  vars <- resolve_select_variables(df, opts$vars, default = vars_default)
  if (length(vars) == 0) stop("No variables available for missingness analysis.")

  summary_rows <- list()
  for (var in vars) {
    vec <- df[[var]]
    total_n <- length(vec)
    missing_n <- sum(is.na(vec))
    missing_prop <- ifelse(total_n > 0, missing_n / total_n, NA_real_)
    summary_rows[[length(summary_rows) + 1]] <- data.frame(
      variable = var,
      type = detect_type(vec),
      total_n = total_n,
      missing_n = missing_n,
      missing_prop = missing_prop,
      missing_pct = ifelse(is.na(missing_prop), NA_real_, missing_prop * 100),
      stringsAsFactors = FALSE
    )
  }

  summary_df <- do.call(rbind, summary_rows)

  method_selected <- method
  if (method == "auto") {
    missing_props <- summary_df$missing_prop
    missing_props <- missing_props[!is.na(missing_props)]
    max_missing <- if (length(missing_props) == 0) 0 else max(missing_props)
    if (max_missing <= low_threshold) {
      method_selected <- "listwise"
    } else if (max_missing <= moderate_threshold) {
      method_selected <- "impute"
    } else if (max_missing <= high_threshold) {
      method_selected <- "indicator"
    } else {
      method_selected <- "drop"
    }
  }

  drop_vars <- character(0)
  if (method_selected == "drop") {
    drop_vars <- summary_df$variable[summary_df$missing_prop >= drop_threshold & !is.na(summary_df$missing_prop)]
  }

  indicator_vars <- character(0)
  if (method_selected %in% c("indicator", "drop")) {
    indicator_vars <- summary_df$variable[
      summary_df$missing_prop >= indicator_threshold &
        !summary_df$variable %in% drop_vars &
        !is.na(summary_df$missing_prop)
    ]
  }

  output_df <- df
  rows_removed <- 0
  indicator_map <- list()
  summary_df$decision <- ""
  summary_df$impute_method <- ""
  summary_df$impute_value <- ""
  summary_df$indicator <- ""

  if (method_selected == "listwise") {
    keep <- if (length(vars) > 0) complete.cases(df[, vars, drop = FALSE]) else rep(TRUE, nrow(df))
    rows_removed <- sum(!keep)
    output_df <- df[keep, , drop = FALSE]
    summary_df$decision <- ifelse(summary_df$missing_n > 0, "listwise deletion", "no missing")
  } else {
    if (length(drop_vars) > 0) {
      output_df[drop_vars] <- NULL
    }
    for (i in seq_len(nrow(summary_df))) {
      var <- summary_df$variable[i]
      missing_n <- summary_df$missing_n[i]
      if (var %in% drop_vars) {
        summary_df$decision[i] <- "drop"
        next
      }
      if (!(var %in% names(output_df))) {
        next
      }
      if (missing_n == 0) {
        summary_df$decision[i] <- "keep"
        next
      }
      vec <- output_df[[var]]
      imputed <- NULL
      if (summary_df$type[i] %in% c("numeric", "datetime")) {
        imputed <- impute_numeric(vec, skew_threshold)
      } else {
        imputed <- impute_mode(vec)
      }
      output_df[[var]] <- imputed$vec
      summary_df$impute_method[i] <- imputed$method
      summary_df$impute_value[i] <- format_impute_value(imputed$value, digits)
      if (imputed$note == "all missing") {
        summary_df$decision[i] <- "all missing"
      } else if (var %in% indicator_vars) {
        summary_df$decision[i] <- "impute + indicator"
      } else {
        summary_df$decision[i] <- "impute"
      }
    }
    if (length(indicator_vars) > 0) {
      for (var in indicator_vars) {
        if (!(var %in% names(df))) next
        indicator_name <- ensure_unique_name(paste0(var, indicator_suffix), names(output_df))
        output_df[[indicator_name]] <- as.integer(is.na(df[[var]]))
        indicator_map[[var]] <- indicator_name
      }
    }
  }

  if (length(indicator_map) > 0) {
    for (i in seq_len(nrow(summary_df))) {
      var <- summary_df$variable[i]
      if (var %in% names(indicator_map)) {
        summary_df$indicator[i] <- indicator_map[[var]]
      }
    }
  }

  label_meta <- resolve_label_metadata(df)
  summary_df <- add_variable_label_column(summary_df, label_meta, var_col = "variable")
  summary_df <- add_variable_label_column(summary_df, label_meta, var_col = "indicator")

  patterns_info <- build_patterns_df(df, vars, max_patterns)
  patterns_df <- patterns_info$patterns_df
  if (nrow(patterns_df) > 0) {
    patterns_df$missing_vars_label <- vapply(
      patterns_df$missing_vars,
      map_variable_list_label,
      character(1),
      labels = label_meta
    )
  }

  total_n <- nrow(df)
  complete_cases_n <- if (length(vars) > 0) sum(complete.cases(df[, vars, drop = FALSE])) else total_n
  complete_cases_pct <- ifelse(total_n > 0, complete_cases_n / total_n * 100, NA_real_)
  missing_range <- summary_df$missing_pct
  missing_range <- missing_range[!is.na(missing_range)]
  min_missing <- if (length(missing_range) == 0) NA_real_ else min(missing_range)
  max_missing <- if (length(missing_range) == 0) NA_real_ else max(missing_range)

  method_label <- switch(
    method_selected,
    listwise = "listwise deletion",
    impute = "single imputation",
    indicator = "single imputation with missingness indicators",
    drop = "drop high-missing variables with imputation",
    method_selected
  )

  vars_display <- map_variable_labels(vars, label_meta)
  drop_vars_display <- map_variable_labels(drop_vars, label_meta)
  indicator_vars_display <- map_variable_labels(indicator_vars, label_meta)

  sentences <- c()
  if (!is.na(min_missing) && !is.na(max_missing)) {
    sentences <- c(sentences, sprintf("Missingness ranged from %s%% to %s%%.", resolve_format_percent(min_missing, digits), resolve_format_percent(max_missing, digits)))
  }
  if (total_n > 0) {
    sentences <- c(sentences, sprintf("Complete cases: %d of %d (%s%%).", complete_cases_n, total_n, resolve_format_percent(complete_cases_pct, digits)))
  }
  sentences <- c(sentences, sprintf("Selected handling method: %s.", method_label))
  if (method_selected == "listwise" && total_n > 0) {
    sentences <- c(sentences, sprintf("Listwise deletion removed %d rows (%s%%).", rows_removed, resolve_format_percent(rows_removed / total_n * 100, digits)))
  }
  if (length(drop_vars) > 0) {
    sentences <- c(sentences, paste0("Dropped variables (missing >= ", resolve_format_percent(drop_threshold * 100, digits), "%): ", paste(drop_vars_display, collapse = ", "), "."))
  }
  if (length(indicator_vars) > 0) {
    sentences <- c(sentences, paste0("Missingness indicators added for: ", paste(indicator_vars_display, collapse = ", "), "."))
  }
  nlss_text <- paste(sentences, collapse = " ")

  summary_table_note <- paste0(
    "Missing % uses total N. ",
    "Numeric imputation uses mean or median (skew threshold = ",
    format_num(skew_threshold, digits),
    "); categorical uses mode."
  )
  pattern_note <- paste0(
    "Pattern order: ",
    paste(vars_display, collapse = ", "),
    ". M = missing, O = observed.",
    ifelse(patterns_info$truncated, " Other patterns grouped.", "")
  )

  template_override <- resolve_template_override(opts$template, module = "missings")
  template_path <- if (!is.null(template_override)) {
    template_override
  } else {
    resolve_get_template_path("missings.default", "missings/default-template.md")
  }
  template_meta <- resolve_get_template_meta(template_path)
  summary_table <- build_missing_table_body(summary_df, digits, template_meta$table)
  pattern_spec <- template_meta$patterns_table
  if (is.null(pattern_spec) && !is.null(template_meta$tables$patterns)) {
    pattern_spec <- template_meta$tables$patterns
  }
  patterns_table <- build_patterns_table_body(patterns_df, digits, pattern_spec)

  nlss_report_path <- file.path(out_dir, "report_canonical.md")
  table_start <- as.integer(resolve_get_next_table_number(nlss_report_path))
  narrative_rows <- build_narrative_rows(summary_df, digits)
  template_context <- list(
    tokens = list(
      summary_table_body = summary_table$body,
      patterns_table_body = patterns_table$body,
      summary_note_body = summary_table_note,
      patterns_note_body = pattern_note,
      table_number_next = as.character(table_start + 1),
      pattern_limit = as.character(max_patterns),
      narrative_default = nlss_text
    ),
    narrative_rows = narrative_rows
  )

  nlss_table <- paste(
    "Table 1",
    "Missingness summary.",
    summary_table$body,
    "",
    "Table 2",
    "Missingness patterns.",
    patterns_table$body,
    sep = "\n"
  )

  analysis_flags <- list(
    vars = vars,
    method = method_selected,
    "low-threshold" = low_threshold,
    "moderate-threshold" = moderate_threshold,
    "high-threshold" = high_threshold,
    "drop-threshold" = drop_threshold,
    "indicator-threshold" = indicator_threshold,
    "indicator-suffix" = indicator_suffix,
    "skew-threshold" = skew_threshold,
    "max-patterns" = max_patterns,
    digits = digits
  )

  resolve_append_nlss_report(
    nlss_report_path,
    "Missing data assessment",
    nlss_table,
    nlss_text,
    analysis_flags = analysis_flags,
    template_path = template_path,
    template_context = template_context
  )

  backup_path <- ""
  output_path <- file.path(out_dir, "missing_handled_data.rds")
  if (!is.null(workspace_parquet_path) && nzchar(workspace_parquet_path)) {
    backup_path <- resolve_backup_workspace_parquet(workspace_parquet_path)
    resolve_write_parquet_data(output_df, workspace_parquet_path)
    output_path <- workspace_parquet_path
  } else {
    saveRDS(output_df, output_path)
  }

  cat("Wrote:\n")
  cat("- ", render_output_path(nlss_report_path, out_dir), "\n", sep = "")
  cat("- ", render_output_path(output_path, out_dir), "\n", sep = "")

  if (resolve_parse_bool(opts$log, default = log_default)) {
    ctx <- resolve_get_run_context()
    resolve_append_analysis_log(
      out_dir,
      module = "missings",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(
        summary_df = summary_df,
        patterns_df = patterns_df,
        transformed_df = output_df,
        method_selected = method_selected,
        drop_vars = drop_vars,
        indicator_vars = indicator_vars,
        rows_removed = rows_removed,
        output_path = render_output_path(output_path, out_dir),
        backup_path = backup_path
      ),
      options = list(
        vars = vars,
        method = method_selected,
        low_threshold = low_threshold,
        moderate_threshold = moderate_threshold,
        high_threshold = high_threshold,
        drop_threshold = drop_threshold,
        indicator_threshold = indicator_threshold,
        indicator_suffix = indicator_suffix,
        skew_threshold = skew_threshold,
        max_patterns = max_patterns
      ),
      user_prompt = resolve_get_user_prompt(opts)
    )
  }
}

main()
