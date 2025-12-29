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
  cat("Imputation (base R; optional mice/VIM)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript impute.R --csv data.csv --vars age,gender\n")
  cat("  Rscript impute.R --sav data.sav --vars age,gender\n")
  cat("  Rscript impute.R --rds data.rds --vars age,gender\n")
  cat("  Rscript impute.R --rdata data.RData --df data_frame_name --vars age,gender\n")
  cat("  Rscript impute.R --parquet data.parquet --vars age,gender\n")
  cat("  Rscript impute.R --interactive\n")
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
  cat("  --vars LIST              Comma-separated variable names (default: config)\n")
  cat("  --engine VALUE           auto|simple|mice|knn (default: auto)\n")
  cat("  --numeric-method VALUE   auto|mean|median|mode|random|constant (default: median)\n")
  cat("  --categorical-method VALUE mode|random|constant (default: mode)\n")
  cat("  --method-map MAP         Per-variable method map: \"var=median|var2=mode\"\n")
  cat("  --value-map MAP          Per-variable constant map: \"var=0|var2=missing\"\n")
  cat("  --constant VALUE         Global constant value (for constant method)\n")
  cat("  --suffix TEXT            Suffix for imputed columns (default: _imp)\n")
  cat("  --indicator TRUE/FALSE   Add missingness indicator columns (default: FALSE)\n")
  cat("  --indicator-suffix TEXT  Suffix for indicators (default: _miss)\n")
  cat("  --skew-threshold VALUE   Skew threshold for auto numeric method (default: 1)\n")
  cat("  --m N                    mice imputations (default: 5)\n")
  cat("  --maxit N                mice max iterations (default: 5)\n")
  cat("  --k N                    VIM kNN neighbors (default: 5)\n")
  cat("  --seed N                 Random seed (optional)\n")
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

  opts$vars <- resolve_prompt("Variables (comma-separated, blank for default)", "")
  engine_default <- resolve_config_value("modules.impute.engine", "auto")
  opts$engine <- resolve_prompt("Engine (auto/simple/mice/knn)", engine_default)
  numeric_default <- resolve_config_value("modules.impute.numeric_method", "median")
  opts$`numeric-method` <- resolve_prompt("Numeric method (auto/mean/median/mode/random/constant)", numeric_default)
  categorical_default <- resolve_config_value("modules.impute.categorical_method", "mode")
  opts$`categorical-method` <- resolve_prompt("Categorical method (mode/random/constant)", categorical_default)
  opts$`method-map` <- resolve_prompt("Per-variable method map (var=method|var2=method)", "")
  opts$`value-map` <- resolve_prompt("Per-variable constant map (var=value|var2=value)", "")
  opts$constant <- resolve_prompt("Global constant value (for constant method)", "")
  suffix_default <- resolve_config_value("modules.impute.suffix", "_imp")
  opts$suffix <- resolve_prompt("Imputed column suffix", suffix_default)
  indicator_default <- resolve_config_value("modules.impute.indicator", FALSE)
  opts$indicator <- resolve_prompt("Create indicator columns TRUE/FALSE", ifelse(isTRUE(indicator_default), "TRUE", "FALSE"))
  indicator_suffix_default <- resolve_config_value("modules.impute.indicator_suffix", "_miss")
  opts$`indicator-suffix` <- resolve_prompt("Indicator suffix", indicator_suffix_default)
  skew_default <- resolve_config_value("modules.impute.skew_threshold", 1)
  opts$`skew-threshold` <- resolve_prompt("Skew threshold", as.character(skew_default))
  m_default <- resolve_config_value("modules.impute.m", 5)
  opts$m <- resolve_prompt("mice imputations (m)", as.character(m_default))
  maxit_default <- resolve_config_value("modules.impute.maxit", 5)
  opts$maxit <- resolve_prompt("mice max iterations", as.character(maxit_default))
  k_default <- resolve_config_value("modules.impute.k", 5)
  opts$k <- resolve_prompt("VIM kNN neighbors (k)", as.character(k_default))
  opts$seed <- resolve_prompt("Random seed (optional)", "")
  digits_default <- resolve_config_value("defaults.digits", 2)
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

parse_int <- function(value, default = NULL) {
  if (is.null(value) || is.logical(value) || value == "") return(default)
  num <- suppressWarnings(as.integer(value))
  if (is.na(num)) stop("Invalid integer value: ", value)
  num
}

parse_numeric <- function(value, default = NULL) {
  if (is.null(value) || is.logical(value) || value == "") return(default)
  num <- suppressWarnings(as.numeric(value))
  if (is.na(num)) stop("Invalid numeric value: ", value)
  num
}

parse_list <- function(value) {
  if (is.null(value) || is.logical(value) || value == "") return(character(0))
  items <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
  items <- items[nzchar(items)]
  unique(items)
}

parse_map <- function(value) {
  if (is.null(value) || is.logical(value) || value == "") return(list())
  entries <- strsplit(value, "\\|")[[1]]
  out <- list()
  for (entry in entries) {
    entry <- trimws(entry)
    if (!nzchar(entry)) next
    parts <- strsplit(entry, "=", fixed = TRUE)[[1]]
    if (length(parts) < 2) stop("Invalid map entry: ", entry)
    key <- trimws(parts[1])
    val <- trimws(paste(parts[-1], collapse = "="))
    if (!nzchar(key)) stop("Invalid map entry: ", entry)
    out[[key]] <- val
  }
  out
}

collapse_map <- function(map) {
  if (length(map) == 0) return("")
  pairs <- vapply(names(map), function(name) {
    paste0(name, "=", map[[name]])
  }, character(1))
  paste(pairs, collapse = "; ")
}

normalize_engine <- function(value, default = "auto") {
  val <- if (!is.null(value) && !is.logical(value) && value != "") value else default
  val <- tolower(trimws(as.character(val)))
  if (val %in% c("auto", "simple", "mice", "knn")) return(val)
  if (val %in% c("k-nn", "k_nn")) return("knn")
  stop("Invalid --engine. Use auto, simple, mice, or knn.")
}

resolve_engine <- function(engine) {
  if (engine != "auto") return(engine)
  if (requireNamespace("mice", quietly = TRUE)) return("mice")
  if (requireNamespace("VIM", quietly = TRUE)) return("knn")
  "simple"
}

normalize_method <- function(value, default = "median") {
  val <- if (!is.null(value) && !is.logical(value) && value != "") value else default
  val <- tolower(trimws(as.character(val)))
  if (val %in% c("auto", "mean", "median", "mode", "random", "constant")) return(val)
  if (val %in% c("avg")) return("mean")
  if (val %in% c("med")) return("median")
  if (val %in% c("sample")) return("random")
  if (val %in% c("value", "const")) return("constant")
  stop("Invalid method: ", value)
}

is_numeric_type <- function(vec) {
  is.numeric(vec) || inherits(vec, "Date") || inherits(vec, "POSIXct") || inherits(vec, "POSIXlt")
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

validate_method_for_type <- function(method, type_label) {
  allowed_numeric <- c("auto", "mean", "median", "mode", "random", "constant")
  allowed_categorical <- c("mode", "random", "constant")
  allowed <- if (type_label == "numeric") allowed_numeric else allowed_categorical
  if (!method %in% allowed) {
    stop("Invalid method for ", type_label, " variables: ", method)
  }
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

parse_bool_text <- function(value) {
  if (is.null(value) || value == "") return(NA)
  val <- tolower(trimws(as.character(value)))
  if (val %in% c("true", "t", "1", "yes", "y")) return(TRUE)
  if (val %in% c("false", "f", "0", "no", "n")) return(FALSE)
  NA
}

coerce_logical_values <- function(values) {
  if (is.logical(values)) return(values)
  if (is.numeric(values)) return(as.logical(values))
  vals <- as.character(values)
  vapply(vals, parse_bool_text, logical(1))
}

coerce_constant_value <- function(vec, constant_value) {
  if (is.null(constant_value) || constant_value == "") {
    stop("Constant value is required for constant imputation.")
  }
  if (is.numeric(vec)) {
    val <- suppressWarnings(as.numeric(constant_value))
    if (is.na(val)) stop("Invalid numeric constant: ", constant_value)
    return(list(value = val, note = ""))
  }
  if (inherits(vec, "Date")) {
    val_num <- suppressWarnings(as.numeric(constant_value))
    if (!is.na(val_num)) {
      return(list(value = as.Date(val_num, origin = "1970-01-01"), note = ""))
    }
    val_date <- suppressWarnings(as.Date(constant_value))
    if (!is.na(val_date)) return(list(value = val_date, note = ""))
    stop("Invalid Date constant: ", constant_value)
  }
  if (inherits(vec, "POSIXct") || inherits(vec, "POSIXlt")) {
    val_num <- suppressWarnings(as.numeric(constant_value))
    if (!is.na(val_num)) {
      return(list(value = as.POSIXct(val_num, origin = "1970-01-01", tz = attr(vec, "tzone")), note = ""))
    }
    val_time <- suppressWarnings(as.POSIXct(constant_value))
    if (!is.na(val_time)) return(list(value = val_time, note = ""))
    stop("Invalid datetime constant: ", constant_value)
  }
  if (is.logical(vec)) {
    val <- parse_bool_text(constant_value)
    if (is.na(val)) stop("Invalid logical constant: ", constant_value)
    return(list(value = val, note = ""))
  }
  list(value = as.character(constant_value), note = "")
}

apply_imputed_values <- function(orig, missing_idx, replacement) {
  if (!any(missing_idx)) return(orig)
  if (length(replacement) == 0) return(orig)
  if (is.factor(orig)) {
    vals <- as.character(orig)
    vals[missing_idx] <- as.character(replacement)
    levels_new <- unique(c(levels(orig), as.character(replacement)))
    return(factor(vals, levels = levels_new, ordered = is.ordered(orig)))
  }
  out <- orig
  out[missing_idx] <- replacement
  out
}

impute_numeric <- function(vec, method, skew_threshold) {
  values <- vec[!is.na(vec)]
  if (length(values) == 0) {
    return(list(vec = vec, method = method, value = NA, note = "all missing"))
  }
  is_date <- inherits(vec, "Date")
  is_time <- inherits(vec, "POSIXct") || inherits(vec, "POSIXlt")
  numeric_vals <- if (is_date || is_time) as.numeric(values) else as.numeric(values)
  method_used <- method
  if (method == "auto") {
    skew <- compute_skewness(numeric_vals)
    use_median <- !is.na(skew) && abs(skew) > skew_threshold
    method_used <- if (use_median) "median" else "mean"
  }
  if (method_used == "mean") {
    impute_val <- mean(numeric_vals)
  } else if (method_used == "median") {
    impute_val <- median(numeric_vals)
  } else {
    impute_val <- mean(numeric_vals)
    method_used <- "mean"
  }
  if (is_date) impute_val <- as.Date(impute_val, origin = "1970-01-01")
  if (is_time) impute_val <- as.POSIXct(impute_val, origin = "1970-01-01", tz = attr(vec, "tzone"))
  out <- vec
  out[is.na(out)] <- impute_val
  list(vec = out, method = method_used, value = impute_val, note = "")
}

impute_mode <- function(vec) {
  values <- vec[!is.na(vec)]
  if (length(values) == 0) {
    return(list(vec = vec, method = "", value = NA, note = "all missing"))
  }
  mode_val <- compute_mode_value(values)
  out <- vec
  if (is.factor(out)) {
    out_vals <- as.character(out)
    out_vals[is.na(out)] <- as.character(mode_val)
    levels_new <- unique(c(levels(out), as.character(mode_val)))
    out <- factor(out_vals, levels = levels_new, ordered = is.ordered(out))
  } else {
    out[is.na(out)] <- mode_val
  }
  list(vec = out, method = "mode", value = mode_val, note = "")
}

impute_random <- function(vec) {
  values <- vec[!is.na(vec)]
  if (length(values) == 0) {
    return(list(vec = vec, method = "random", value = NA, note = "all missing"))
  }
  missing_idx <- is.na(vec)
  samples <- sample(values, sum(missing_idx), replace = TRUE)
  out <- vec
  if (is.factor(out)) {
    out_vals <- as.character(out)
    out_vals[missing_idx] <- as.character(samples)
    levels_new <- unique(c(levels(out), as.character(samples)))
    out <- factor(out_vals, levels = levels_new, ordered = is.ordered(vec))
  } else {
    out[missing_idx] <- samples
  }
  list(vec = out, method = "random", value = NA, note = "")
}

impute_constant <- function(vec, constant_value) {
  res <- coerce_constant_value(vec, constant_value)
  missing_idx <- is.na(vec)
  out <- apply_imputed_values(vec, missing_idx, res$value)
  list(vec = out, method = "constant", value = res$value, note = res$note)
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

resolve_method_for_var <- function(var, vec, method_map, numeric_method, categorical_method) {
  method <- if (var %in% names(method_map)) method_map[[var]] else if (is_numeric_type(vec)) numeric_method else categorical_method
  method <- normalize_method(method, default = method)
  type_label <- if (is_numeric_type(vec)) "numeric" else "categorical"
  if (type_label == "categorical" && method == "auto") method <- "mode"
  validate_method_for_type(method, type_label)
  list(method = method, type = type_label, raw_type = detect_type(vec))
}

prepare_engine_frame <- function(df) {
  out <- df
  for (name in names(out)) {
    if (is.character(out[[name]])) {
      out[[name]] <- as.factor(out[[name]])
    }
  }
  out
}

compute_mode_value <- function(values) {
  vals <- values[!is.na(values)]
  if (length(vals) == 0) return(NA)
  uniq_vals <- unique(vals)
  if (length(uniq_vals) == 1) return(uniq_vals[1])
  counts <- tabulate(match(vals, uniq_vals))
  uniq_vals[which.max(counts)]
}

pool_mice_values <- function(imputed_list, var, missing_idx, orig_vec, type_label) {
  if (length(imputed_list) == 0 || !any(missing_idx)) return(numeric(0))
  cols <- lapply(imputed_list, function(df) df[[var]][missing_idx])
  if (length(cols) == 1) return(cols[[1]])
  if (type_label == "numeric") {
    mat <- do.call(cbind, lapply(cols, as.numeric))
    pooled <- rowMeans(mat, na.rm = TRUE)
    if (inherits(orig_vec, "Date")) {
      pooled <- as.Date(pooled, origin = "1970-01-01")
    } else if (inherits(orig_vec, "POSIXct") || inherits(orig_vec, "POSIXlt")) {
      pooled <- as.POSIXct(pooled, origin = "1970-01-01", tz = attr(orig_vec, "tzone"))
    }
    return(pooled)
  }
  if (is.logical(orig_vec)) {
    mat <- do.call(cbind, lapply(cols, coerce_logical_values))
    return(apply(mat, 1, compute_mode_value))
  }
  mat <- do.call(cbind, lapply(cols, function(x) as.character(x)))
  apply(mat, 1, compute_mode_value)
}

build_impute_table_body <- function(summary_df, digits, table_spec = NULL) {
  default_columns <- list(
    list(key = "variable", label = "Variable"),
    list(key = "type", label = "Type"),
    list(key = "missing_n", label = "Missing n"),
    list(key = "missing_pct", label = "Missing %"),
    list(key = "engine", label = "Engine"),
    list(key = "method", label = "Method"),
    list(key = "impute_value", label = "Impute value", drop_if_empty = TRUE),
    list(key = "imputed_n", label = "Imputed n"),
    list(key = "target", label = "Imputed column"),
    list(key = "indicator", label = "Indicator", drop_if_empty = TRUE),
    list(key = "note", label = "Note", drop_if_empty = TRUE)
  )
  columns <- resolve_normalize_table_columns(
    if (!is.null(table_spec$columns)) table_spec$columns else NULL,
    default_columns
  )

  rows <- list()
  if (nrow(summary_df) == 0) {
    rows[[1]] <- list(
      variable = "",
      type = "",
      missing_n = "",
      missing_pct = "",
      engine = "",
      method = "",
      impute_value = "",
      imputed_n = "",
      target = "",
      indicator = "",
      note = "No variables selected."
    )
  } else {
    for (i in seq_len(nrow(summary_df))) {
      row <- summary_df[i, , drop = FALSE]
      rows[[length(rows) + 1]] <- list(
        variable = resolve_row_display(row, "variable"),
        type = row$type,
        missing_n = row$missing_n,
        missing_pct = resolve_format_percent(row$missing_pct, digits),
        engine = row$engine,
        method = row$method,
        impute_value = row$impute_value,
        imputed_n = row$imputed_n,
        target = resolve_row_display(row, "target"),
        indicator = resolve_row_display(row, "indicator"),
        note = row$note
      )
    }
  }

  table_rows <- list()
  for (row in rows) {
    row_vals <- character(0)
    for (col in columns) {
      key <- col$key
      val <- if (key %in% names(row)) row[[key]] else ""
      row_vals <- c(row_vals, resolve_as_cell_text(val))
    }
    table_rows[[length(table_rows) + 1]] <- row_vals
  }

  filtered <- resolve_drop_empty_columns(columns, table_rows)
  columns <- filtered$columns
  table_rows <- filtered$rows
  headers <- vapply(columns, function(col) {
    if (!is.null(col$label) && nzchar(col$label)) col$label else col$key
  }, character(1))
  list(
    body = resolve_render_markdown_table(headers, table_rows),
    columns = vapply(columns, function(col) col$key, character(1))
  )
}

build_impute_note_tokens <- function(summary_df, engine_requested, engine_used, indicator, indicator_suffix, m, maxit, k, seed, ignored_map, skipped_vars_display) {
  note_parts <- character(0)
  if (engine_requested == "auto" && engine_used != "auto") {
    note_parts <- c(note_parts, paste0("Engine auto-selected: ", engine_used, "."))
  } else {
    note_parts <- c(note_parts, paste0("Engine: ", engine_used, "."))
  }
  if (engine_used == "mice") {
    note_parts <- c(note_parts, paste0("mice m = ", m, ", maxit = ", maxit, "; pooled to single imputed columns."))
  }
  if (engine_used == "knn") {
    note_parts <- c(note_parts, paste0("kNN used with k = ", k, "."))
  }
  if (indicator) {
    note_parts <- c(note_parts, paste0("Indicators use suffix '", indicator_suffix, "'."))
  }
  if (!is.na(seed)) {
    note_parts <- c(note_parts, paste0("Random seed: ", seed, "."))
  }
  if (length(skipped_vars_display) > 0) {
    note_parts <- c(note_parts, paste0("All-missing variables left unimputed: ", paste(skipped_vars_display, collapse = ", "), "."))
  }
  if (ignored_map) {
    note_parts <- c(note_parts, "Method/value maps apply only to engine 'simple'.")
  }
  list(
    note_default = paste(note_parts, collapse = " "),
    engine_note = if (length(note_parts) > 0) note_parts[1] else "",
    indicator_note = if (indicator) paste0("Indicators use suffix '", indicator_suffix, "'.") else "",
    pool_note = if (engine_used == "mice") paste0("mice m = ", m, ", maxit = ", maxit, "; pooled to single imputed columns.") else "",
    seed_note = if (!is.na(seed)) paste0("Random seed: ", seed, ".") else "",
    skipped_note = if (length(skipped_vars_display) > 0) paste0("All-missing variables left unimputed: ", paste(skipped_vars_display, collapse = ", "), ".") else "",
    map_note = if (ignored_map) "Method/value maps apply only to engine 'simple'." else ""
  )
}

build_impute_narrative_rows <- function(summary_df, digits) {
  rows <- list()
  if (nrow(summary_df) == 0) {
    rows[[1]] <- list(full_sentence = "No variables selected.")
    return(rows)
  }
  for (i in seq_len(nrow(summary_df))) {
    row <- summary_df[i, , drop = FALSE]
    var_display <- resolve_row_display(row, "variable")
    target_display <- resolve_row_display(row, "target")
    indicator_display <- resolve_row_display(row, "indicator")
    missing_n <- row$missing_n
    missing_pct <- resolve_format_percent(row$missing_pct, digits)
    base <- ""
    if (missing_n == 0) {
      base <- paste0(var_display, " had no missing values; ", target_display, " mirrors the original.")
    } else if (row$imputed_n == 0 && row$note == "all missing") {
      base <- paste0(var_display, " had all values missing; no imputation was possible for ", target_display, ".")
    } else {
      base <- paste0(var_display, ": ", missing_n, " missing (", missing_pct, "%), imputed via ",
                     row$method, " (", row$engine, ") into ", target_display, ".")
    }
    if (nzchar(indicator_display)) {
      base <- paste0(base, " Indicator ", indicator_display, " added.")
    }
    if (nzchar(row$note) && row$note != "all missing") {
      base <- paste0(base, " Note: ", row$note, ".")
    }
    rows[[length(rows) + 1]] <- list(
      variable = var_display,
      type = row$type,
      missing_n = row$missing_n,
      missing_pct = missing_pct,
      engine = row$engine,
      method = row$method,
      impute_value = row$impute_value,
      imputed_n = row$imputed_n,
      target = target_display,
      indicator = indicator_display,
      note = row$note,
      full_sentence = trimws(base)
    )
  }
  rows
}

emit_input_issue <- function(out_dir, opts, message, details = list(), status = "invalid_input") {
  log_default <- resolve_config_value("defaults.log", TRUE)
  if (resolve_parse_bool(opts$log, default = log_default)) {
    ctx <- resolve_get_run_context()
    resolve_append_analysis_log(
      out_dir,
      module = "impute",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(
        status = status,
        message = message,
        details = details
      ),
      options = details,
      user_prompt = resolve_get_user_prompt(opts)
    )
  }
  stop(message)
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  opts <- resolve_parse_args(args)

  if (length(opts) == 0 || !is.null(opts$help)) {
    print_usage()
    return(invisible(NULL))
  }

  if (!is.null(opts$interactive)) {
    opts <- interactive_options()
  }

  df <- resolve_load_dataframe(opts)
  out_dir <- resolve_get_workspace_out_dir(df)
  workspace_parquet_path <- attr(df, "workspace_parquet_path")

  vars_default <- resolve_config_value("modules.impute.vars_default", "all")
  vars <- resolve_select_variables(df, opts$vars, default = vars_default)
  if (length(vars) == 0) stop("No variables selected for imputation.")

  digits_default <- resolve_config_value("defaults.digits", 2)
  digits <- parse_int(opts$digits, default = digits_default)

  engine_default <- resolve_config_value("modules.impute.engine", "auto")
  engine_requested <- normalize_engine(opts$engine, engine_default)
  engine_used <- resolve_engine(engine_requested)

  numeric_default <- resolve_config_value("modules.impute.numeric_method", "median")
  numeric_method <- normalize_method(opts$`numeric-method`, numeric_default)
  categorical_default <- resolve_config_value("modules.impute.categorical_method", "mode")
  categorical_method <- normalize_method(opts$`categorical-method`, categorical_default)

  skew_default <- resolve_config_value("modules.impute.skew_threshold", 1)
  skew_threshold <- parse_numeric(opts$`skew-threshold`, default = skew_default)

  suffix_default <- resolve_config_value("modules.impute.suffix", "_imp")
  suffix <- if (!is.null(opts$suffix) && nzchar(opts$suffix)) as.character(opts$suffix) else suffix_default

  indicator_default <- resolve_config_value("modules.impute.indicator", FALSE)
  indicator <- resolve_parse_bool(opts$indicator, default = indicator_default)
  indicator_suffix_default <- resolve_config_value("modules.impute.indicator_suffix", "_miss")
  indicator_suffix <- if (!is.null(opts$`indicator-suffix`) && nzchar(opts$`indicator-suffix`)) {
    as.character(opts$`indicator-suffix`)
  } else {
    indicator_suffix_default
  }

  m_default <- resolve_config_value("modules.impute.m", 5)
  m <- parse_int(opts$m, default = m_default)
  if (is.null(m) || is.na(m) || m < 1) m <- 1
  if (engine_used == "mice" && m < 2) m <- 2
  maxit_default <- resolve_config_value("modules.impute.maxit", 5)
  maxit <- parse_int(opts$maxit, default = maxit_default)
  if (is.null(maxit) || is.na(maxit) || maxit < 1) maxit <- 1
  k_default <- resolve_config_value("modules.impute.k", 5)
  k <- parse_int(opts$k, default = k_default)
  if (is.null(k) || is.na(k) || k < 1) k <- 1

  seed <- parse_int(opts$seed, default = NA)

  method_map_raw <- parse_map(opts$`method-map`)
  method_map <- list()
  if (length(method_map_raw) > 0) {
    for (name in names(method_map_raw)) {
      method_map[[name]] <- normalize_method(method_map_raw[[name]], default = method_map_raw[[name]])
    }
  }
  value_map <- parse_map(opts$`value-map`)
  constant_value <- if (!is.null(opts$constant)) as.character(opts$constant) else ""

  if (length(method_map) > 0) {
    missing_methods <- setdiff(names(method_map), names(df))
    if (length(missing_methods) > 0) {
      stop("Unknown variables in --method-map: ", paste(missing_methods, collapse = ", "))
    }
  }
  if (length(value_map) > 0) {
    missing_values <- setdiff(names(value_map), names(df))
    if (length(missing_values) > 0) {
      stop("Unknown variables in --value-map: ", paste(missing_values, collapse = ", "))
    }
  }

  if (engine_used == "mice" && !requireNamespace("mice", quietly = TRUE)) {
    emit_input_issue(out_dir, opts, "Imputation engine 'mice' requires the 'mice' package.", details = list(package = "mice"), status = "missing_dependency")
  }
  if (engine_used == "knn" && !requireNamespace("VIM", quietly = TRUE)) {
    emit_input_issue(out_dir, opts, "Imputation engine 'knn' requires the 'VIM' package.", details = list(package = "VIM"), status = "missing_dependency")
  }

  output_df <- df
  existing_names <- names(output_df)
  summary_rows <- list()
  indicator_vars <- character(0)
  imputed_vars <- character(0)
  target_map <- list()
  skipped_vars <- character(0)

  ignored_map <- engine_used != "simple" && (length(method_map) > 0 || length(value_map) > 0)

  if (!is.na(seed)) set.seed(seed)

  if (engine_used == "simple") {
    for (var in vars) {
      vec <- df[[var]]
      missing_idx <- is.na(vec)
      missing_n <- sum(missing_idx)
      total_n <- length(vec)
      missing_pct <- ifelse(total_n > 0, missing_n / total_n * 100, NA_real_)

      method_info <- resolve_method_for_var(var, vec, method_map, numeric_method, categorical_method)
      method_choice <- method_info$method
      type_label <- method_info$raw_type

      impute_result <- list(vec = vec, method = method_choice, value = NA, note = "")
      if (missing_n > 0) {
        if (method_choice %in% c("mean", "median", "auto")) {
          impute_result <- impute_numeric(vec, method_choice, skew_threshold)
        } else if (method_choice == "mode") {
          impute_result <- impute_mode(vec)
        } else if (method_choice == "random") {
          impute_result <- impute_random(vec)
        } else if (method_choice == "constant") {
          constant_for_var <- if (var %in% names(value_map)) value_map[[var]] else constant_value
          if (constant_for_var == "") {
            stop("Constant value required for variable: ", var)
          }
          impute_result <- impute_constant(vec, constant_for_var)
        }
      }
      if (missing_n == total_n && total_n > 0) {
        impute_result$note <- "all missing"
        skipped_vars <- c(skipped_vars, var)
      }
      imputed_n <- if (missing_n > 0 && impute_result$note != "all missing") missing_n else 0

      target <- ensure_unique_name(paste0(var, suffix), existing_names)
      existing_names <- c(existing_names, target)
      output_df[[target]] <- impute_result$vec
      imputed_vars <- c(imputed_vars, target)
      target_map[[var]] <- target

      indicator_name <- ""
      if (indicator && missing_n > 0) {
        indicator_name <- ensure_unique_name(paste0(var, indicator_suffix), existing_names)
        existing_names <- c(existing_names, indicator_name)
        output_df[[indicator_name]] <- ifelse(missing_idx, 1L, 0L)
        indicator_vars <- c(indicator_vars, indicator_name)
      }

      summary_rows[[length(summary_rows) + 1]] <- data.frame(
        variable = var,
        type = type_label,
        missing_n = missing_n,
        missing_pct = missing_pct,
        engine = engine_used,
        method = impute_result$method,
        impute_value = format_impute_value(impute_result$value, digits),
        imputed_n = imputed_n,
        target = target,
        indicator = indicator_name,
        note = impute_result$note,
        stringsAsFactors = FALSE
      )
    }
  } else if (engine_used == "mice") {
    all_missing_map <- vapply(vars, function(var) all(is.na(df[[var]])), logical(1))
    if (any(all_missing_map)) skipped_vars <- c(skipped_vars, vars[all_missing_map])
    vars_engine <- vars[!all_missing_map]
    imputed_list <- list()
    mice_methods <- list()

    if (length(vars_engine) > 0) {
      engine_df <- prepare_engine_frame(df[, vars_engine, drop = FALSE])
      mice_args <- list(data = engine_df, m = m, maxit = maxit, printFlag = FALSE)
      if (!is.na(seed)) mice_args$seed <- seed
      mice_run <- tryCatch(do.call(mice::mice, mice_args), error = function(e) e)
      if (inherits(mice_run, "error")) {
        emit_input_issue(out_dir, opts, paste0("mice failed: ", mice_run$message), status = "fit_failed")
      }
      imputed_list <- mice::complete(mice_run, action = "all")
      mice_methods <- mice_run$method
    }

    for (var in vars) {
      vec <- df[[var]]
      missing_idx <- is.na(vec)
      missing_n <- sum(missing_idx)
      total_n <- length(vec)
      missing_pct <- ifelse(total_n > 0, missing_n / total_n * 100, NA_real_)
      type_label <- detect_type(vec)
      method_used <- if (!is.null(mice_methods[[var]]) && nzchar(mice_methods[[var]])) mice_methods[[var]] else "mice"

      imputed_vec <- vec
      imputed_n <- 0
      note <- ""

      if (missing_n == total_n && total_n > 0) {
        note <- "all missing"
      } else if (missing_n > 0 && length(imputed_list) > 0 && var %in% vars_engine) {
        pool_vals <- pool_mice_values(imputed_list, var, missing_idx, vec, if (is_numeric_type(vec)) "numeric" else "categorical")
        imputed_vec <- apply_imputed_values(vec, missing_idx, pool_vals)
        imputed_n <- missing_n
      }

      target <- ensure_unique_name(paste0(var, suffix), existing_names)
      existing_names <- c(existing_names, target)
      output_df[[target]] <- imputed_vec
      imputed_vars <- c(imputed_vars, target)
      target_map[[var]] <- target

      indicator_name <- ""
      if (indicator && missing_n > 0) {
        indicator_name <- ensure_unique_name(paste0(var, indicator_suffix), existing_names)
        existing_names <- c(existing_names, indicator_name)
        output_df[[indicator_name]] <- ifelse(missing_idx, 1L, 0L)
        indicator_vars <- c(indicator_vars, indicator_name)
      }

      summary_rows[[length(summary_rows) + 1]] <- data.frame(
        variable = var,
        type = type_label,
        missing_n = missing_n,
        missing_pct = missing_pct,
        engine = engine_used,
        method = method_used,
        impute_value = "",
        imputed_n = imputed_n,
        target = target,
        indicator = indicator_name,
        note = note,
        stringsAsFactors = FALSE
      )
    }
  } else if (engine_used == "knn") {
    all_missing_map <- vapply(vars, function(var) all(is.na(df[[var]])), logical(1))
    if (any(all_missing_map)) skipped_vars <- c(skipped_vars, vars[all_missing_map])
    vars_engine <- vars[!all_missing_map]
    knn_result <- NULL

    if (length(vars_engine) > 0) {
      engine_df <- prepare_engine_frame(df[, vars_engine, drop = FALSE])
      knn_args <- list(data = engine_df, variable = vars_engine, k = k, imp_var = FALSE)
      knn_result <- tryCatch(do.call(VIM::kNN, knn_args), error = function(e) e)
      if (inherits(knn_result, "error")) {
        emit_input_issue(out_dir, opts, paste0("kNN failed: ", knn_result$message), status = "fit_failed")
      }
    }

    for (var in vars) {
      vec <- df[[var]]
      missing_idx <- is.na(vec)
      missing_n <- sum(missing_idx)
      total_n <- length(vec)
      missing_pct <- ifelse(total_n > 0, missing_n / total_n * 100, NA_real_)
      type_label <- detect_type(vec)
      method_used <- "knn"

      imputed_vec <- vec
      imputed_n <- 0
      note <- ""

      if (missing_n == total_n && total_n > 0) {
        note <- "all missing"
      } else if (missing_n > 0 && !is.null(knn_result) && var %in% vars_engine) {
        replacement <- knn_result[[var]][missing_idx]
        imputed_vec <- apply_imputed_values(vec, missing_idx, replacement)
        imputed_n <- missing_n
      }

      target <- ensure_unique_name(paste0(var, suffix), existing_names)
      existing_names <- c(existing_names, target)
      output_df[[target]] <- imputed_vec
      imputed_vars <- c(imputed_vars, target)
      target_map[[var]] <- target

      indicator_name <- ""
      if (indicator && missing_n > 0) {
        indicator_name <- ensure_unique_name(paste0(var, indicator_suffix), existing_names)
        existing_names <- c(existing_names, indicator_name)
        output_df[[indicator_name]] <- ifelse(missing_idx, 1L, 0L)
        indicator_vars <- c(indicator_vars, indicator_name)
      }

      summary_rows[[length(summary_rows) + 1]] <- data.frame(
        variable = var,
        type = type_label,
        missing_n = missing_n,
        missing_pct = missing_pct,
        engine = engine_used,
        method = method_used,
        impute_value = "",
        imputed_n = imputed_n,
        target = target,
        indicator = indicator_name,
        note = note,
        stringsAsFactors = FALSE
      )
    }
  }

  summary_df <- if (length(summary_rows) > 0) do.call(rbind, summary_rows) else data.frame()
  label_meta <- resolve_label_metadata(df)
  summary_df <- add_variable_label_column(summary_df, label_meta, var_col = "variable")
  summary_df <- add_variable_label_column(summary_df, label_meta, var_col = "target")
  summary_df <- add_variable_label_column(summary_df, label_meta, var_col = "indicator")
  skipped_vars_display <- map_variable_labels(skipped_vars, label_meta)

  missing_range <- summary_df$missing_pct
  missing_range <- missing_range[!is.na(missing_range)]
  min_missing <- if (length(missing_range) == 0) NA_real_ else min(missing_range)
  max_missing <- if (length(missing_range) == 0) NA_real_ else max(missing_range)

  sentences <- character(0)
  if (!is.na(min_missing) && !is.na(max_missing)) {
    sentences <- c(sentences, sprintf("Missingness ranged from %s%% to %s%%.", resolve_format_percent(min_missing, digits), resolve_format_percent(max_missing, digits)))
  }
  sentences <- c(sentences, paste0("Imputation engine: ", engine_used, "."))
  if (engine_used == "mice") {
    sentences <- c(sentences, paste0("m = ", m, ", maxit = ", maxit, "; pooled to single imputed columns."))
  }
  if (engine_used == "knn") {
    sentences <- c(sentences, paste0("kNN used with k = ", k, "."))
  }
  if (indicator) {
    sentences <- c(sentences, "Missingness indicators were added for variables with missing values.")
  }
  if (length(skipped_vars_display) > 0) {
    sentences <- c(sentences, paste0("All-missing variables left unimputed: ", paste(skipped_vars_display, collapse = ", "), "."))
  }
  if (ignored_map) {
    sentences <- c(sentences, "Method/value maps apply only to engine 'simple'.")
  }
  apa_text <- paste(sentences, collapse = " ")

  template_override <- resolve_template_override(opts$template, module = "impute")
  template_path <- if (!is.null(template_override)) {
    template_override
  } else {
    resolve_get_template_path("impute.default", "impute/default-template.md")
  }
  template_meta <- resolve_get_template_meta(template_path)
  table_result <- build_impute_table_body(summary_df, digits, template_meta$table)
  narrative_rows <- build_impute_narrative_rows(summary_df, digits)
  note_tokens <- build_impute_note_tokens(summary_df, engine_requested, engine_used, indicator, indicator_suffix, m, maxit, k, seed, ignored_map, skipped_vars_display)

  apa_report_path <- file.path(out_dir, "report_canonical.md")

  template_context <- list(
    tokens = c(
      list(
        table_body = table_result$body,
        narrative_default = apa_text
      ),
      note_tokens
    ),
    narrative_rows = narrative_rows
  )

  apa_table <- paste(
    "Table 1",
    "Imputation summary.",
    table_result$body,
    "",
    paste0("Note. ", note_tokens$note_default),
    sep = "\n"
  )

  analysis_flags <- list(
    vars = vars,
    "engine-requested" = engine_requested,
    engine = engine_used,
    "numeric-method" = numeric_method,
    "categorical-method" = categorical_method,
    "method-map" = collapse_map(method_map),
    "value-map" = collapse_map(value_map),
    constant = if (nzchar(constant_value)) constant_value else "",
    suffix = suffix,
    indicator = indicator,
    "indicator-suffix" = indicator_suffix,
    "skew-threshold" = skew_threshold,
    m = if (engine_used == "mice") m else NULL,
    maxit = if (engine_used == "mice") maxit else NULL,
    k = if (engine_used == "knn") k else NULL,
    seed = if (!is.na(seed)) seed else NULL,
    digits = digits
  )

  resolve_append_apa_report(
    apa_report_path,
    "Imputation",
    apa_table,
    apa_text,
    analysis_flags = analysis_flags,
    template_path = template_path,
    template_context = template_context
  )

  backup_path <- ""
  output_path <- file.path(out_dir, "imputed_data.rds")
  if (!is.null(workspace_parquet_path) && nzchar(workspace_parquet_path)) {
    backup_path <- resolve_backup_workspace_parquet(workspace_parquet_path)
    resolve_write_parquet_data(output_df, workspace_parquet_path)
    output_path <- workspace_parquet_path
  } else {
    saveRDS(output_df, output_path)
  }

  cat("Wrote:\n")
  cat("- ", render_output_path(apa_report_path, out_dir), "\n", sep = "")
  cat("- ", render_output_path(output_path, out_dir), "\n", sep = "")

  log_default <- resolve_config_value("defaults.log", TRUE)
  if (resolve_parse_bool(opts$log, default = log_default)) {
    ctx <- resolve_get_run_context()
    resolve_append_analysis_log(
      out_dir,
      module = "impute",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(
        summary_df = summary_df,
        imputed_vars = imputed_vars,
        indicator_vars = indicator_vars,
        target_map = target_map,
        engine = engine_used,
        output_path = render_output_path(output_path, out_dir),
        backup_path = backup_path,
        skipped_vars = skipped_vars
      ),
      options = list(
        vars = vars,
        engine_requested = engine_requested,
        engine = engine_used,
        numeric_method = numeric_method,
        categorical_method = categorical_method,
        method_map = method_map,
        value_map = value_map,
        constant = constant_value,
        suffix = suffix,
        indicator = indicator,
        indicator_suffix = indicator_suffix,
        skew_threshold = skew_threshold,
        m = m,
        maxit = maxit,
        k = k,
        seed = seed
      ),
      user_prompt = resolve_get_user_prompt(opts)
    )
  }
}

main()
