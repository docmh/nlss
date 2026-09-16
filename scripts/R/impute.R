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
source_lib("data_change.R")
source_lib("imputation_artifact.R")

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
  cat("  --seed N                 Random seed (default: modules.impute.seed)\n")
  cat("  --digits N               Rounding digits (default: 2)\n")
  cat("  --template REF           Template path or template key (optional)\n")
  cat("  --user-prompt TEXT       Original AI user prompt for logging (optional)\n")
  cat("  --log TRUE/FALSE         Write analysis_log.jsonl (default: TRUE)\n")
  cat("  --interactive            Prompt for inputs\n")
  cat("  --help                   Show this help\n")
  cat("\nThe mice engine writes single-completion columns and preserves its mids object.\n")
  cat("It does not perform Rubin-pooled multiple-imputation inference.\n")
  cat("Mandatory run bundles remain enabled with --log FALSE.\n")
  print_import_usage()
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

  opts$vars <- prompt("Variables (comma-separated, blank for default)", "")
  engine_default <- get_config_value("modules.impute.engine")
  opts$engine <- prompt("Engine (auto/simple/mice/knn)", engine_default)
  numeric_default <- get_config_value("modules.impute.numeric_method")
  opts$`numeric-method` <- prompt("Numeric method (auto/mean/median/mode/random/constant)", numeric_default)
  categorical_default <- get_config_value("modules.impute.categorical_method")
  opts$`categorical-method` <- prompt("Categorical method (mode/random/constant)", categorical_default)
  opts$`method-map` <- prompt("Per-variable method map (var=method|var2=method)", "")
  opts$`value-map` <- prompt("Per-variable constant map (var=value|var2=value)", "")
  opts$constant <- prompt("Global constant value (for constant method)", "")
  suffix_default <- get_config_value("modules.impute.suffix")
  opts$suffix <- prompt("Imputed column suffix", suffix_default)
  indicator_default <- get_config_value("modules.impute.indicator")
  opts$indicator <- prompt("Create indicator columns TRUE/FALSE", ifelse(isTRUE(indicator_default), "TRUE", "FALSE"))
  indicator_suffix_default <- get_config_value("modules.impute.indicator_suffix")
  opts$`indicator-suffix` <- prompt("Indicator suffix", indicator_suffix_default)
  skew_default <- get_config_value("modules.impute.skew_threshold")
  opts$`skew-threshold` <- prompt("Skew threshold", as.character(skew_default))
  m_default <- get_config_value("modules.impute.m")
  opts$m <- prompt("mice imputations (m)", as.character(m_default))
  maxit_default <- get_config_value("modules.impute.maxit")
  opts$maxit <- prompt("mice max iterations", as.character(maxit_default))
  k_default <- get_config_value("modules.impute.k")
  opts$k <- prompt("VIM kNN neighbors (k)", as.character(k_default))
  opts$seed <- prompt("Random seed", as.character(get_config_value("modules.impute.seed")))
  digits_default <- get_config_value("defaults.digits")
  opts$digits <- prompt("Rounding digits", as.character(digits_default))
  opts$template <- prompt("Template (path or key; blank for default)", "")
  opts$`user-prompt` <- prompt("User prompt (optional)", "")
  log_default <- get_config_value("defaults.log")
  opts$log <- prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))
  opts
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
    if (key %in% names(out)) stop("Duplicate variable in map: ", key, ". Supply one explicit value per variable.")
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

normalize_engine <- function(value) {
  val <- value
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

normalize_method <- function(value) {
  val <- value
  val <- tolower(trimws(as.character(val)))
  if (val %in% c("auto", "mean", "median", "mode", "random", "constant")) return(val)
  if (val %in% c("avg")) return("mean")
  if (val %in% c("med")) return("median")
  if (val %in% c("sample")) return("random")
  if (val %in% c("value", "const")) return("constant")
  stop("Invalid method: ", value)
}

is_numeric_type <- function(vec) {
  is.numeric(vec) || inherits(vec, c("Date", "POSIXct", "difftime"))
}

detect_type <- function(vec) {
  if (inherits(vec, c("Date", "POSIXct", "difftime"))) {
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
  if (length(vals) < 3L) return(NA_real_)
  if (all(vals == vals[1L])) return(0)
  s <- sd(vals)
  centered <- vals - mean(vals)
  skew <- if (is.finite(s) && s > 0) mean((centered / s)^3) else NA_real_
  # Standardize before cubing: the algebraic statistic is unchanged, while raw
  # cubes can underflow even when their ratio is finite but numerically wrong.
  # A nonconstant column is not constant merely because its squared deviations
  # underflow to zero. Rescale before SD calculation when squared deviations
  # approach underflow, or whenever the initial calculation is non-finite.
  small_deviations <- max(abs(centered)) < sqrt(.Machine$double.xmin)
  if (!is.finite(skew) || small_deviations) {
    scale <- max(abs(vals))
    scaled <- vals / scale
    s <- sd(scaled)
    skew <- if (is.finite(s) && s > 0) mean(((scaled - mean(scaled)) / s)^3) else NA_real_
  }
  if (!is.finite(skew)) stop("Numeric skewness is not finite; rescale the selected variable explicitly.")
  skew
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

restore_impute_numeric <- function(values, original) {
  if (inherits(original, "Date")) return(as.Date(values, origin = "1970-01-01"))
  if (inherits(original, "POSIXct")) return(as.POSIXct(values, origin = "1970-01-01", tz = attr(original, "tzone")))
  if (inherits(original, "difftime")) {
    out <- as.difftime(values, units = attr(original, "units"))
    class(out) <- class(original)
    return(out)
  }
  values
}

coerce_constant_value <- function(vec, constant_value) {
  if (is.null(constant_value) || !nzchar(constant_value)) stop("Constant value is required for constant imputation.")
  if (inherits(vec, c("Date", "POSIXct"))) {
    number <- suppressWarnings(as.numeric(constant_value))
    if (is.finite(number)) return(list(value = restore_impute_numeric(number, vec), note = ""))
    value <- tryCatch(if (inherits(vec, "Date")) as.Date(constant_value) else
      as.POSIXct(constant_value, tz = attr(vec, "tzone")), error = function(e) NA)
    if (length(value) != 1L || is.na(value) || !is.finite(as.numeric(value))) stop("Invalid temporal constant: ", constant_value)
    return(list(value = value, note = ""))
  }
  if (is_numeric_type(vec)) {
    value <- suppressWarnings(as.numeric(constant_value))
    if (!is.finite(value)) stop("Invalid finite numeric constant: ", constant_value)
    return(list(value = restore_impute_numeric(value, vec), note = ""))
  }
  if (is.logical(vec)) {
    value <- parse_bool_text(constant_value)
    if (is.na(value)) stop("Invalid logical constant: ", constant_value)
    return(list(value = value, note = ""))
  }
  list(value = as.character(constant_value), note = "")
}

apply_imputed_values <- function(orig, missing_idx, replacement) {
  if (!any(missing_idx)) return(orig)
  if (length(replacement) == 0) return(orig)
  if (is.factor(orig)) {
    vals <- as.character(orig)
    vals[missing_idx] <- as.character(replacement)
    levels_new <- unique(c(levels(orig), as.character(replacement[!is.na(replacement)])))
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
  numeric_vals <- as.numeric(values)
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
  if (!is.finite(impute_val)) stop("Imputation produced a non-finite value; rescale the selected variable.")
  impute_val <- restore_impute_numeric(impute_val, vec)
  out <- vec
  out[is.na(out)] <- impute_val
  list(vec = out, method = method_used, value = impute_val, note = "")
}

impute_mode <- function(vec) {
  values <- vec[!is.na(vec)]
  if (length(values) == 0) {
    return(list(vec = vec, method = "mode", value = NA, note = "all missing"))
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
  samples <- values[sample.int(length(values), sum(missing_idx), replace = TRUE)]
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
  method <- normalize_method(method)
  type_label <- if (is_numeric_type(vec)) "numeric" else "categorical"
  if (type_label == "categorical" && method == "auto") method <- "mode"
  validate_method_for_type(method, type_label)
  list(method = method, type = type_label, raw_type = detect_type(vec))
}

prepare_engine_frame <- function(df) {
  out <- df
  attributes(out) <- attributes(out)[c("names", "row.names", "class")]
  rownames(out) <- NULL
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

collapse_mice_values <- function(imputed_list, var, missing_idx, orig_vec, type_label) {
  if (length(imputed_list) == 0 || !any(missing_idx)) return(numeric(0))
  cols <- lapply(imputed_list, function(df) df[[var]][missing_idx])
  if (length(cols) == 1) return(cols[[1]])
  if (type_label == "numeric") {
    mat <- do.call(cbind, lapply(cols, as.numeric))
    completed <- rowMeans(mat, na.rm = TRUE)
    completed[is.nan(completed)] <- NA_real_
    return(restore_impute_numeric(completed, orig_vec))
  }
  if (is.logical(orig_vec)) {
    mat <- do.call(cbind, lapply(cols, coerce_logical_values))
    return(apply(mat, 1, compute_mode_value))
  }
  mat <- do.call(cbind, lapply(cols, function(x) as.character(x)))
  apply(mat, 1, compute_mode_value)
}

imputation_inference_warning <- function() {
  paste("SINGLE COMPLETION ONLY: mice draws were reduced to one set of imputed columns",
        "using means for numeric values and modes for categorical values.",
        "This is not Rubin-pooled multiple-imputation inference and does not propagate",
        "between-imputation uncertainty into subsequent standard errors, confidence intervals or p-values.",
        "For multiple-imputation inference, fit each model to all preserved imputations and pool the model estimates.",
        "That analysis-and-pooling workflow is not implemented by this command.")
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
  columns <- normalize_table_columns(
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
        missing_pct = format_percent(row$missing_pct, digits),
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
      row_vals <- c(row_vals, as_cell_text(val))
    }
    table_rows[[length(table_rows) + 1]] <- row_vals
  }

  filtered <- drop_empty_columns(columns, table_rows)
  columns <- filtered$columns
  table_rows <- filtered$rows
  headers <- vapply(columns, function(col) {
    if (!is.null(col$label) && nzchar(col$label)) col$label else col$key
  }, character(1))
  list(
    body = render_markdown_table(headers, table_rows),
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
    note_parts <- c(note_parts, paste0("mice m = ", m, ", maxit = ", maxit,
                                     "; single-completion columns, not Rubin-pooled inference."))
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
    # Keep the legacy template token as an alias, with scientifically explicit wording.
    pool_note = if (engine_used == "mice") paste0("mice m = ", m, ", maxit = ", maxit, "; single completion only; inference_pooled = FALSE.") else "",
    completion_note = if (engine_used == "mice") "Numeric draws are averaged and categorical draws use their mode to form single-completion columns." else "Single-completion columns were generated.",
    inference_warning = if (engine_used == "mice") imputation_inference_warning() else "",
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
    missing_pct <- format_percent(row$missing_pct, digits)
    base <- ""
    if (missing_n == 0) {
      base <- paste0(var_display, " had no missing values; ", target_display, " mirrors the original.")
    } else if (row$imputed_n == 0 && row$note == "all missing") {
      base <- paste0(var_display, " had all values missing; no imputation was possible for ", target_display, ".")
    } else {
      base <- paste0(var_display, ": ", missing_n, " missing (", missing_pct, "%), ", row$imputed_n, " filled via ",
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

impute_number <- function(opts, key, config, lower = -Inf, upper = Inf, integer = FALSE) {
  value <- opts[[key]]
  if (is.null(value)) value <- get_config_value(config)
  number <- suppressWarnings(as.numeric(value))
  if (length(value) != 1L || is.logical(value) || length(number) != 1L ||
      !is.finite(number) || number < lower || number > upper || (integer && number != floor(number))) {
    stop("--", key, " must be a finite ", if (integer) "integer" else "number",
      " between ", lower, " and ", upper, ".")
  }
  if (integer) as.integer(number) else number
}

impute_text <- function(opts, key, config = NULL, blank = FALSE) {
  value <- opts[[key]]
  if (is.null(value)) value <- if (is.null(config)) "" else get_config_value(config)
  if (!is.character(value) || length(value) != 1L || is.na(value) || (!blank && !nzchar(value))) {
    stop("--", key, " must be one ", if (!blank) "nonempty ", "text value.")
  }
  value
}

validate_impute_column <- function(vec, name) {
  allowed <- is.null(dim(vec)) && (
    (is.numeric(vec) && !is.object(vec)) || (is.logical(vec) && !is.object(vec)) ||
    (is.character(vec) && !is.object(vec)) || identical(class(vec), "factor") ||
    identical(class(vec), c("ordered", "factor")) || identical(class(vec), "Date") ||
    identical(class(vec), c("POSIXct", "POSIXt")) || identical(class(vec), "difftime") ||
    identical(class(vec), c("hms", "difftime")))
  if (!allowed) stop("Unsupported imputation column '", name, "' (", paste(class(vec), collapse = "/"),
    "). Select ordinary numeric, logical, categorical or temporal vectors; matrix/list/custom classes require an explicit prior transformation.")
  if (is_numeric_type(vec) && any(is.infinite(as.numeric(vec)))) {
    stop("Selected imputation column '", name, "' contains infinite values; clean these explicitly first.")
  }
}

clean_impute_metadata <- function(output, before, target_map, indicator_map, audit, engine, seed) {
  old <- attr(before, "nlss_import_contract", exact = TRUE)
  labels <- resolve_label_metadata(before)
  conflicts <- list()
  for (name in names(target_map)) {
    target <- target_map[[name]]
    labels$variables[[target]] <- paste0("Single completion of ", resolve_variable_label(labels, name))
    source_column <- old$columns[[name]]
    values <- labels$values[[name]]
    if (length(values)) {
      codes <- names(values)
      if (is_numeric_type(before[[name]])) codes <- suppressWarnings(as.numeric(codes))
      missing <- source_column$missing
      remove <- import_missing_mask(codes, unlist(missing$na_values, use.names = FALSE),
        unlist(missing$na_range, use.names = FALSE))
      labels$values[[target]] <- if (any(!remove)) values[!remove] else NULL
      if (any(remove)) conflicts[[target]] <- list(source_variable = name,
        removed_source_missing_labels = values[remove],
        reason = "Original missing-code labels are not active category labels for derived completed values.")
    }
  }
  for (name in names(indicator_map)) {
    indicator <- indicator_map[[name]]
    labels$variables[[indicator]] <- paste0("Missingness indicator for ", resolve_variable_label(labels, name))
    labels$values[[indicator]] <- list("0" = "Observed", "1" = "Missing")
  }
  attributes(output) <- attributes(output)[c("names", "row.names", "class")]
  rownames(output) <- NULL
  attr(output, "nlss_labels") <- normalize_label_metadata(labels)
  dictionary <- import_capture_dictionary(output)
  dictionary$source_rows <- old$source_rows
  for (name in names(before)) dictionary$columns[[name]] <- old$columns[[name]]
  provenance <- list(input_version_id = attr(before, "nlss_dataset_ref")$version_id,
    observation_basis = "input_version_rows", source_rows = seq_len(nrow(before)),
    output_n = nrow(output), engine = engine, seed = seed, target_map = target_map,
    indicator_map = indicator_map, imputed_input_rows = lapply(audit, function(item) item$imputed_input_rows),
    removed_source_missing_labels = conflicts, completion_mode = "single_completion", inference_pooled = FALSE,
    original_missing_provenance = "Original columns retain original_source_rows missing metadata; it is never reapplied to derived columns or current row positions.")
  dictionary$imputation <- provenance
  for (name in names(target_map)) {
    target <- target_map[[name]]
    dictionary$columns[[target]]$derived_from <- list(variable = name, input_version_id = provenance$input_version_id,
      operation = "impute_single_completion", method = audit[[name]]$method, engine = engine)
  }
  for (name in names(indicator_map)) {
    indicator <- indicator_map[[name]]
    dictionary$columns[[indicator]]$derived_from <- list(variable = name, input_version_id = provenance$input_version_id,
      operation = "missingness_indicator")
  }
  attr(output, "nlss_import_contract") <- dictionary
  attr(output, "nlss_import_contract") <- attr(import_prepare_storage(output), "nlss_import_contract")
  list(data = output, label_conflicts = conflicts)
}

main <- function() {
  opts <- nlss_run_options(commandArgs(trailingOnly = TRUE), "impute")
  if (!is.null(opts[["help"]])) { print_usage(); return(invisible(NULL)) }
  if (parse_bool(opts[["interactive"]], FALSE)) {
    opts <- modifyList(opts, interactive_options())
    opts[["interactive"]] <- TRUE
  }
  df <- nlss_load_input(opts)
  out_dir <- get_workspace_out_dir(df)
  nlss_begin_run("impute", df, opts, out_dir)
  vars <- unique(select_variables(df, opts[["vars"]], default = get_config_value("modules.impute.vars_default")))
  if (!length(vars)) stop("No variables selected for imputation.")
  for (name in vars) validate_impute_column(df[[name]], name)
  digits <- impute_number(opts, "digits", "defaults.digits", 0, 15, TRUE)
  engine_requested <- normalize_engine(impute_text(opts, "engine", "modules.impute.engine"))
  replay <- nlss_run_context$replay
  engine_used <- if (is.null(replay)) resolve_engine(engine_requested) else replay$request$options$engine
  if (!engine_used %in% c("simple", "mice", "knn")) stop("Saved effective imputation engine is invalid.")
  numeric_method <- normalize_method(impute_text(opts, "numeric-method", "modules.impute.numeric_method"))
  categorical_method <- normalize_method(impute_text(opts, "categorical-method", "modules.impute.categorical_method"))
  if (categorical_method == "auto") categorical_method <- "mode"
  validate_method_for_type(categorical_method, "categorical")
  skew_threshold <- impute_number(opts, "skew-threshold", "modules.impute.skew_threshold", 0)
  suffix <- impute_text(opts, "suffix", "modules.impute.suffix", TRUE)
  indicator <- parse_bool(opts[["indicator"]], get_config_value("modules.impute.indicator"))
  indicator_suffix <- impute_text(opts, "indicator-suffix", "modules.impute.indicator_suffix", TRUE)
  m_requested <- impute_number(opts, "m", "modules.impute.m", 1, .Machine$integer.max, TRUE)
  m <- if (engine_used == "mice") max(2L, m_requested) else m_requested
  maxit <- impute_number(opts, "maxit", "modules.impute.maxit", 1, .Machine$integer.max, TRUE)
  k <- impute_number(opts, "k", "modules.impute.k", 1, .Machine$integer.max, TRUE)
  seed <- impute_number(opts, "seed", "modules.impute.seed", 0, .Machine$integer.max, TRUE)
  method_map <- lapply(parse_map(impute_text(opts, "method-map", blank = TRUE)), normalize_method)
  value_map <- parse_map(impute_text(opts, "value-map", blank = TRUE))
  constant_value <- impute_text(opts, "constant", blank = TRUE)
  for (map_name in c("method_map", "value_map")) {
    unknown <- setdiff(names(get(map_name)), names(df))
    if (length(unknown)) stop("Unknown variables in --", gsub("_", "-", map_name), ": ", paste(unknown, collapse = ", "))
  }
  package <- if (engine_used == "mice") "mice" else if (engine_used == "knn") "VIM" else NULL
  if (!is.null(package) && !requireNamespace(package, quietly = TRUE)) {
    stop("Imputation engine '", engine_used, "' requires the '", package, "' package.")
  }
  all_missing <- vapply(df[vars], function(vec) all(is.na(vec)), logical(1))
  vars_engine <- if (engine_used == "simple") vars else vars[!all_missing]
  settings <- list(vars = vars, engine_requested = engine_requested, engine = engine_used,
    numeric_method = numeric_method, categorical_method = categorical_method,
    method_map = method_map, value_map = value_map, constant = constant_value, suffix = suffix,
    indicator = indicator, indicator_suffix = indicator_suffix, skew_threshold = skew_threshold,
    m_requested = m_requested, m = m, maxit = maxit, k = k, seed = seed, digits = digits)
  design <- list(source_n = nrow(df), source_classes = lapply(df[vars], class),
    source_rows = seq_len(nrow(df)), observation_basis = "input_version_rows",
    engine_variables = vars_engine, excluded_all_missing = if (engine_used == "simple") character() else vars[all_missing],
    engine_preparation = "Original selected column classes; character columns become factors for mice/VIM. Runtime frame attributes are removed. Mice input carries deterministic nlss_input_version_id frame provenance before generation; the returned mids is never modified.",
    mode_tie_break = "First observed value among tied categories, including first imputation draw for mice aggregation.",
    skewness_definition = "mean(((x-mean(x))/sd(x))^3); sd uses n-1; fewer than three observations gives unavailable skewness and mean fill.",
    replay = list(eligible = TRUE, reason = "Validated ordinary vectors and explicit seed with frozen engine and software environment."),
    completion_mode = "single_completion", inference_pooled = FALSE)
  nlss_run_seed(seed, stochastic = TRUE)
  nlss_resolve_request(settings, design)
  output_df <- df
  summary_rows <- list()
  target_map <- indicator_map <- handling_audit <- list()
  skipped_vars <- character()
  imputation_artifact <- NULL
  engine_diagnostics <- list()
  imputed_list <- list()
  mice_methods <- character()
  knn_result <- NULL
  if (engine_used != "simple" && length(vars_engine)) {
    engine_df <- prepare_engine_frame(df[, vars_engine, drop = FALSE])
    if (engine_used == "mice") {
      # Equal selected values can come from different full input versions (for
      # example after an earlier completion appended columns). Bind provenance
      # before mice generates its unchanged object and content-addressed bytes.
      attr(engine_df, "nlss_input_version_id") <- attr(df, "nlss_dataset_ref")$version_id
      mice_run <- mice::mice(data = engine_df, m = m, maxit = maxit, printFlag = FALSE, seed = seed)
      imputed_list <- mice::complete(mice_run, action = "all")
      mice_methods <- mice_run$method
      imputation_artifact <- nlss_preserve_mice_artifact(mice_run, out_dir, attr(df, "nlss_dataset_ref"), seed)
      engine_diagnostics <- list(methods = as.list(mice_run$method), predictor_matrix = unname(mice_run$predictorMatrix),
        predictor_variables = colnames(mice_run$predictorMatrix), logged_events = mice_run$loggedEvents,
        iterations = mice_run$iteration, m = mice_run$m,
        chain_mean = mice_run$chainMean, chain_variance = mice_run$chainVar,
        completion_missing_counts = lapply(imputed_list, function(frame) as.list(vapply(frame, function(vec) sum(is.na(vec)), integer(1)))),
        adequacy_assessed = FALSE)
    } else {
      knn_result <- VIM::kNN(data = engine_df, variable = vars_engine, k = k, imp_var = FALSE)
      engine_diagnostics <- list(k = k, variables = vars_engine, variable_order = vars_engine,
        use_imputed_distances = TRUE, adequacy_assessed = FALSE)
    }
  }
  for (var in vars) {
    vec <- df[[var]]
    missing_idx <- is.na(vec)
    missing_n <- sum(missing_idx)
    method <- if (engine_used == "simple") resolve_method_for_var(var, vec, method_map, numeric_method, categorical_method)$method else engine_used
    method_requested <- method
    value <- NA
    imputed_vec <- vec
    note <- ""
    if (engine_used == "mice" && var %in% names(mice_methods) && nzchar(mice_methods[[var]])) method <- mice_methods[[var]]
    if (missing_n > 0L) {
      if (engine_used == "simple") {
        completed <- switch(method,
          auto = impute_numeric(vec, method, skew_threshold), mean = impute_numeric(vec, method, skew_threshold),
          median = impute_numeric(vec, method, skew_threshold), mode = impute_mode(vec), random = impute_random(vec),
          constant = impute_constant(vec, if (var %in% names(value_map)) value_map[[var]] else constant_value))
        imputed_vec <- completed$vec
        method <- completed$method
        value <- completed$value
        note <- completed$note
      } else if (var %in% vars_engine) {
        replacement <- if (engine_used == "mice") collapse_mice_values(imputed_list, var, missing_idx, vec,
          if (is_numeric_type(vec)) "numeric" else "categorical") else knn_result[[var]][missing_idx]
        imputed_vec <- apply_imputed_values(vec, missing_idx, replacement)
      }
    }
    imputed_rows <- which(missing_idx & !is.na(imputed_vec))
    remaining_rows <- which(is.na(imputed_vec))
    if (missing_n > 0L && length(remaining_rows) == length(vec)) {
      note <- "all missing"
      skipped_vars <- c(skipped_vars, var)
    } else if (length(remaining_rows)) note <- paste(length(remaining_rows), "values remain missing")
    if (is_numeric_type(imputed_vec) && any(is.infinite(as.numeric(imputed_vec)))) stop("Imputation produced non-finite values for ", var, ".")
    target <- ensure_unique_name(paste0(var, suffix), names(output_df))
    output_df[[target]] <- imputed_vec
    target_map[[var]] <- target
    indicator_name <- ""
    if (indicator && missing_n > 0L) {
      indicator_name <- ensure_unique_name(paste0(var, indicator_suffix), names(output_df))
      output_df[[indicator_name]] <- as.integer(missing_idx)
      indicator_map[[var]] <- indicator_name
    }
    temporal <- inherits(value, c("Date", "POSIXct", "difftime"))
    handling_audit[[var]] <- list(target = target, type = detect_type(vec), method = method, method_requested = method_requested,
      missing_input_rows = which(missing_idx), imputed_input_rows = imputed_rows,
      remaining_missing_rows = remaining_rows, missing_n = missing_n, imputed_n = length(imputed_rows),
      remaining_missing_n = length(remaining_rows), value_raw = if (temporal) as.numeric(value) else value,
      value_storage = if (temporal) attr(import_prepare_storage(data.frame(value = value)), "nlss_import_contract")$storage$value else NULL,
      skewness = if (engine_used == "simple" && is_numeric_type(vec) && method_requested == "auto") compute_skewness(as.numeric(vec)) else NULL)
    summary_rows[[length(summary_rows) + 1L]] <- data.frame(variable = var, type = detect_type(vec),
      missing_n = missing_n, missing_pct = if (length(vec)) missing_n / length(vec) * 100 else NA_real_,
      engine = engine_used, method = method, impute_value = format_impute_value(value, digits),
      imputed_n = length(imputed_rows), target = target, indicator = indicator_name, note = note,
      stringsAsFactors = FALSE)
  }
  cleaned <- clean_impute_metadata(output_df, df, target_map, indicator_map, handling_audit, engine_used, seed)
  output_df <- cleaned$data
  summary_df <- do.call(rbind, summary_rows)
  labels <- resolve_label_metadata(output_df)
  for (key in c("variable", "target", "indicator")) summary_df <- add_variable_label_column(summary_df, labels, var_col = key)
  ignored_map <- engine_used != "simple" && (length(method_map) > 0L || length(value_map) > 0L || nzchar(constant_value))
  notices <- character()
  if (m_requested != m) notices <- c(notices, paste0("Requested m = ", m_requested, "; effective mice m = ", m, " (at least two imputations)."))
  if (!nrow(df)) notices <- c(notices, "The input contains zero observations; empty completion columns are retained without an imputation estimate.")
  if (length(skipped_vars)) notices <- c(notices, paste0("All-missing variables left unimputed: ", paste(skipped_vars, collapse = ", "), "."))
  if (engine_used == "mice" && any(unlist(engine_diagnostics$completion_missing_counts) > 0L)) notices <- c(notices,
    "Some preserved mice completions remain incomplete. Numeric means use available draws and categorical modes use available categories; this does not establish complete multiple-imputation data for model fitting.")
  if (any(vapply(handling_audit, function(item) item$remaining_missing_n > 0L, logical(1)))) notices <- c(notices, "Some target values remain missing; actual filled and remaining counts are recorded per variable.")
  if (ignored_map) notices <- c(notices, "Method/value maps and constant values apply only to engine 'simple'; the selected engine did not use them.")
  numeric_labels <- vars[vapply(vars, function(name) is_numeric_type(df[[name]]) &&
    length(attr(df, "nlss_import_contract")$columns[[name]]$value_labels) > 0L, logical(1))]
  if (length(numeric_labels)) notices <- c(notices, paste0("Numeric value labels do not establish a categorical measurement level. Numeric imputation retains stored quantitative coding for: ", paste(numeric_labels, collapse = ", "), "."))
  inference_warning <- if (engine_used == "mice") imputation_inference_warning() else
    "SINGLE COMPLETION ONLY: completed columns do not propagate imputation uncertainty into subsequent inference. Choosing a method does not establish the missing-data mechanism or justify the analysis."
  caveats <- c(inference_warning, "Engine diagnostics and preserved artifacts do not certify convergence, adequate imputation models, a plausible missing-data mechanism or substantive validity.")
  for (notice in c(caveats, notices)) warning(notice, call. = FALSE)
  data_change <- nlss_prepare_data_change(df, output_df)
  json_df <- import_prepare_storage(output_df)
  results <- list(summary_df = summary_df, transformed_df = json_df,
    transformed_df_storage = attr(json_df, "nlss_import_contract")$storage,
    imputed_vars = unname(vapply(target_map, identity, character(1))), indicator_vars = unname(vapply(indicator_map, identity, character(1))),
    target_map = target_map, indicator_map = indicator_map, engine = engine_used,
    engine_requested = engine_requested, m_requested = m_requested, m = m,
    completion_mode = "single_completion", completion_aggregation = if (engine_used == "mice") "mean_numeric_mode_categorical" else "none",
    inference_pooled = FALSE, inference_warning = inference_warning, imputation_artifact = imputation_artifact,
    engine_diagnostics = engine_diagnostics, handling_audit = handling_audit, label_conflicts = cleaned$label_conflicts,
    source_rows = seq_len(nrow(df)), source_n = nrow(df), output_n = nrow(output_df),
    caveats = caveats, notices = notices, data_change = data_change,
    output_path = data_change$output_path, backup_path = data_change$backup_path, skipped_vars = skipped_vars,
    labels = labels)
  nlss_set_result(results)
  redact <- function(value) {
    if (is.factor(value)) value <- as.character(value)
    if (is.character(value)) return(nlss_mask_prose_paths(value, nlss_run_context$root))
    if (is.data.frame(value)) { for (name in names(value)) value[[name]] <- redact(value[[name]]); return(value) }
    if (is.list(value)) return(lapply(value, redact))
    value
  }
  display <- redact(summary_df)
  skipped_display <- map_variable_labels(skipped_vars, labels)
  template_path <- resolve_template_override(opts[["template"]], module = "impute")
  if (is.null(template_path)) template_path <- resolve_template_path("impute.default")
  template_path <- nlss_freeze_template(template_path, "impute.main")
  template_meta <- get_template_meta(template_path)
  table <- build_impute_table_body(display, digits, template_meta$table)
  note_tokens <- build_impute_note_tokens(display, engine_requested, engine_used, indicator,
    indicator_suffix, m, maxit, k, seed, ignored_map, skipped_display)
  nlss_text <- paste0("Imputation engine: ", engine_used, ". Original columns were preserved; ",
    length(target_map), " single-completion columns were appended. ", note_tokens$note_default)
  template_context <- list(tokens = c(list(table_body = table$body, narrative_default = nlss_text), note_tokens),
    narrative_rows = build_impute_narrative_rows(display, digits))
  flags <- list(vars = vars, "engine-requested" = engine_requested, engine = engine_used,
    "numeric-method" = numeric_method, "categorical-method" = categorical_method,
    "method-map" = collapse_map(method_map), "value-map" = collapse_map(value_map), constant = constant_value,
    suffix = suffix, indicator = indicator, "indicator-suffix" = indicator_suffix,
    "skew-threshold" = skew_threshold, m = if (engine_used == "mice") m else NULL,
    maxit = if (engine_used == "mice") maxit else NULL, k = if (engine_used == "knn") k else NULL,
    "completion-mode" = "single_completion", "inference-pooled" = FALSE, seed = seed, digits = digits)
  report_path <- file.path(out_dir, "report_canonical.md")
  nlss_table <- paste("Table 1", "Imputation summary.", table$body, "", paste0("Note. ", note_tokens$note_default), sep = "\n")
  nlss_stage_report(report_path, "Imputation", nlss_table, redact(nlss_text),
    analysis_flags = redact(flags), template_path = template_path, template_context = redact(template_context))
  artifact_text <- if (engine_used != "mice") "" else if (is.null(imputation_artifact))
    "No mids artifact was generated because no variables could be passed to mice." else
    paste0("Preserved mids object (", imputation_artifact$m, " imputations): `", imputation_artifact$path,
      "`. SHA-256: `", imputation_artifact$sha256, "`.")
  mandatory <- paste(c("## Imputation inference limitation", caveats, artifact_text, notices), collapse = "\n\n")
  nlss_stage_report(report_path, "Imputation safeguards", "", redact(mandatory))
  if (parse_bool(opts[["log"]], get_config_value("defaults.log"))) {
    ctx <- get_run_context()
    legacy <- redact(results)
    legacy$display_note <- "External textual paths are masked in this legacy projection; private run results and immutable data preserve exact values."
    nlss_stage_log(out_dir, "impute", ctx$prompt, ctx$commands, legacy, redact(settings), get_user_prompt(opts))
  }
}

nlss_run_main("impute", main)
