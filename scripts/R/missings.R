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
  cat("  Mandatory run bundles remain enabled with --log FALSE.\n")
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

  opts$vars <- prompt("Variables (comma-separated, blank for all)", "")
  method_default <- get_config_value("modules.missings.method")
  opts$method <- prompt("Method (auto/listwise/impute/indicator/drop)", method_default)
  low_default <- get_config_value("modules.missings.low_threshold")
  moderate_default <- get_config_value("modules.missings.moderate_threshold")
  high_default <- get_config_value("modules.missings.high_threshold")
  drop_default <- get_config_value("modules.missings.drop_threshold")
  indicator_default <- get_config_value("modules.missings.indicator_threshold")
  indicator_suffix_default <- get_config_value("modules.missings.indicator_suffix")
  skew_default <- get_config_value("modules.missings.skew_threshold")
  max_patterns_default <- get_config_value("modules.missings.max_patterns")
  digits_default <- get_config_value("defaults.digits")

  opts$`low-threshold` <- prompt("Low threshold (0-1)", as.character(low_default))
  opts$`moderate-threshold` <- prompt("Moderate threshold (0-1)", as.character(moderate_default))
  opts$`high-threshold` <- prompt("High threshold (0-1)", as.character(high_default))
  opts$`drop-threshold` <- prompt("Drop threshold (0-1)", as.character(drop_default))
  opts$`indicator-threshold` <- prompt("Indicator threshold (0-1)", as.character(indicator_default))
  opts$`indicator-suffix` <- prompt("Indicator suffix", indicator_suffix_default)
  opts$`skew-threshold` <- prompt("Skew threshold", as.character(skew_default))
  opts$`max-patterns` <- prompt("Max patterns", as.character(max_patterns_default))
  opts$digits <- prompt("Rounding digits", as.character(digits_default))
  opts$template <- prompt("Template (path or key; blank for default)", "")
  opts$`user-prompt` <- prompt("User prompt (optional)", "")
  log_default <- get_config_value("defaults.log")
  opts$log <- prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))
  opts
}

missing_number <- function(opts, key, config, lower = -Inf, upper = Inf, integer = FALSE) {
  value <- opts[[key]]
  if (is.null(value)) value <- get_config_value(config)
  number <- suppressWarnings(as.numeric(value))
  if (length(value) != 1L || is.logical(value) || length(number) != 1L ||
      !is.finite(number) || number < lower || number > upper ||
      (integer && number != floor(number))) {
    stop("--", key, " must be a finite ", if (integer) "integer" else "number",
      " between ", lower, " and ", upper, ".")
  }
  if (integer) as.integer(number) else number
}

detect_type <- function(vec) {
  if (inherits(vec, c("Date", "POSIXct", "POSIXlt", "difftime"))) return("datetime")
  if (is.numeric(vec)) return("numeric")
  if (is.logical(vec)) return("logical")
  if (is.factor(vec)) return("factor")
  if (is.character(vec)) return("character")
  class(vec)[1]
}

validate_missing_column <- function(vec, name) {
  allowed <- is.null(dim(vec)) && (
    (is.numeric(vec) && !is.object(vec)) ||
    (is.logical(vec) && !is.object(vec)) ||
    (is.character(vec) && !is.object(vec)) ||
    identical(class(vec), "factor") || identical(class(vec), c("ordered", "factor")) ||
    identical(class(vec), "Date") || identical(class(vec), c("POSIXct", "POSIXt")) ||
    identical(class(vec), "difftime") || identical(class(vec), c("hms", "difftime")))
  if (!allowed) stop("Unsupported missing-data column '", name, "' (",
    paste(class(vec), collapse = "/"), "). Select ordinary numeric, logical, categorical or temporal vectors; matrix/list/custom classes require an explicit prior transformation.")
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
  if (is.null(value) || length(value) == 0L || is.na(value)) return("")
  if (inherits(value, c("Date", "POSIXt", "difftime"))) return(format(value))
  if (is.numeric(value)) return(format_num(value, digits))
  as.character(value)
}

impute_numeric <- function(vec, skew_threshold) {
  values <- vec[!is.na(vec)]
  if (!length(values)) return(list(vec = vec, method = "", value = NULL,
    skewness = NA_real_, note = "all missing"))
  numbers <- as.numeric(values)
  if (any(!is.finite(numbers))) stop("Cannot impute from non-finite observed values; infinities are not treated as missing. Resolve them explicitly first.")
  skew <- compute_skewness(numbers)
  method <- if (!is.na(skew) && abs(skew) > skew_threshold) "median" else "mean"
  fill <- if (method == "median") median(numbers) else mean(numbers)
  if (!is.finite(fill)) stop("The numeric imputation value is not finite; no data were published.")
  impute_val <- fill
  if (inherits(vec, c("Date", "POSIXt", "difftime"))) {
    impute_val <- structure(fill, class = class(vec))
    for (key in c("tzone", "units")) attr(impute_val, key) <- attr(vec, key, exact = TRUE)
  }
  out <- vec
  out[is.na(out)] <- impute_val
  list(vec = out, method = method, value = impute_val, skewness = skew, note = "")
}

impute_mode <- function(vec) {
  values <- vec[!is.na(vec)]
  if (!length(values)) return(list(vec = vec, method = "", value = NULL,
    skewness = NA_real_, note = "all missing"))
  # Stable table order breaks ties: factor levels for factors, sorted values
  # under the captured locale otherwise. Ordered factors retain their order.
  counts <- sort(table(values), decreasing = TRUE)
  mode_val <- names(counts)[1]
  out <- vec
  if (is.logical(out)) mode_val <- identical(mode_val, "TRUE")
  if (is.factor(out)) out <- as.character(out)
  out[is.na(out)] <- mode_val
  if (is.factor(vec)) out <- factor(out, levels = levels(vec), ordered = is.ordered(vec))
  list(vec = out, method = "mode", value = mode_val, skewness = NA_real_, note = "")
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
  display <- round_numeric(summary_df, digits)
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
  columns <- normalize_table_columns(
    if (!is.null(table_spec$columns)) table_spec$columns else NULL,
    default_columns
  )

  rows <- list()
  if (nrow(display) == 0) {
    row <- rep("", length(columns)); row[1] <- "No variables"
    rows <- list(row)
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
          val <- as_cell_text(row[[key]][1])
        } else if (key %in% c("missing_n", "total_n")) {
          val <- ifelse(is.na(row[[key]][1]), "", as.character(row[[key]][1]))
        } else if (key %in% c("missing_pct")) {
          val <- format_percent(row[[key]][1], digits)
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
  }

  filtered <- drop_empty_columns(columns, rows)
  columns <- filtered$columns
  rows <- filtered$rows
  headers <- vapply(columns, function(col) {
    if (!is.null(col$label) && nzchar(col$label)) col$label else col$key
  }, character(1))
  list(
    body = render_markdown_table(headers, rows),
    columns = vapply(columns, function(col) col$key, character(1))
  )
}

build_patterns_table_body <- function(patterns_df, digits, table_spec = NULL) {
  display <- round_numeric(patterns_df, digits)
  default_columns <- list(
    list(key = "pattern", label = "Pattern"),
    list(key = "missing_vars", label = "Missing variables"),
    list(key = "n", label = "n"),
    list(key = "pct_total", label = "%"),
    list(key = "missing_count", label = "Missing count", drop_if_empty = TRUE)
  )
  columns <- normalize_table_columns(
    if (!is.null(table_spec$columns)) table_spec$columns else NULL,
    default_columns
  )

  rows <- list()
  if (nrow(display) == 0) {
    row <- rep("", length(columns)); row[1] <- "No patterns"
    rows <- list(row)
  } else {
    for (i in seq_len(nrow(display))) {
      row <- display[i, , drop = FALSE]
      row_vals <- character(0)
      for (col in columns) {
        key <- col$key
        val <- ""
        if (key == "pattern") {
          val <- as_cell_text(row[[key]][1])
        } else if (key == "missing_vars") {
          val <- resolve_row_display(row, "missing_vars")
        } else if (key == "n" || key == "missing_count") {
          val <- ifelse(is.na(row[[key]][1]), "", as.character(row[[key]][1]))
        } else if (key == "pct_total") {
          val <- format_percent(row[[key]][1], digits)
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
  }

  filtered <- drop_empty_columns(columns, rows)
  columns <- filtered$columns
  rows <- filtered$rows
  headers <- vapply(columns, function(col) {
    if (!is.null(col$label) && nzchar(col$label)) col$label else col$key
  }, character(1))
  list(
    body = render_markdown_table(headers, rows),
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
    missing_text <- ifelse(is.na(row$missing_pct), "NA", format_percent(row$missing_pct, digits))
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

missing_nonfinite_rows <- function(vec) {
  if (is.numeric(vec) || inherits(vec, c("Date", "POSIXt", "difftime"))) {
    return(which(is.infinite(as.numeric(vec))))
  }
  integer()
}

missing_label_conflicts <- function(before, audit) {
  dictionary <- attr(before, "nlss_import_contract", exact = TRUE)
  conflicts <- list()
  for (name in names(audit)) {
    item <- audit[[name]]
    value <- item$value_raw
    column <- dictionary$columns[[name]]
    if (!length(item$imputed_input_rows) || length(value) != 1L || is.na(value) ||
        !length(column$value_labels)) next
    definitions <- column$missing
    explicit <- unlist(definitions$na_values, use.names = FALSE)
    range <- unlist(definitions$na_range, use.names = FALSE)
    if (!import_missing_mask(value, explicit, range)) next
    for (entry in column$value_labels) {
      if (length(entry$value) == 1L && !is.na(entry$value) && entry$value == value) {
        conflicts[[length(conflicts) + 1L]] <- list(variable = name, code = value,
          label = entry$label, original_label_entry = entry,
          missing_definition = if (value %in% explicit) "na_values" else "na_range",
          reason = "imputed_value_collides_with_original_user_missing_code",
          imputed_input_rows = item$imputed_input_rows)
      }
    }
  }
  conflicts
}

clean_missing_metadata <- function(df, before, source_rows, indicator_map, audit, label_conflicts) {
  old <- attr(before, "nlss_import_contract", exact = TRUE)
  labels <- resolve_label_metadata(before)
  labels$variables <- labels$variables[intersect(names(labels$variables), names(df))]
  labels$values <- labels$values[intersect(names(labels$values), names(df))]
  for (name in names(indicator_map)) {
    indicator <- indicator_map[[name]]
    labels$variables[[indicator]] <- paste0("Missingness indicator for ", resolve_variable_label(labels, name))
    labels$values[[indicator]] <- list("0" = "Observed", "1" = "Missing")
  }
  for (conflict in label_conflicts) {
    name <- conflict$variable
    map <- labels$values[[name]]
    codes <- names(map)
    if (is.numeric(conflict$code)) codes <- suppressWarnings(as.numeric(codes))
    keep <- is.na(codes) | codes != conflict$code
    labels$values[[name]] <- if (any(keep)) map[keep] else NULL
  }
  # Arrow serializes frame attributes. Runtime paths and load-route attributes
  # are not data semantics; retain one canonical set for ordinary runs/replay.
  attributes(df) <- attributes(df)[c("names", "row.names", "class")]
  rownames(df) <- NULL
  attr(df, "nlss_labels") <- normalize_label_metadata(labels)
  dictionary <- import_capture_dictionary(df)
  dictionary$source_rows <- old$source_rows
  for (name in intersect(names(df), names(old$columns))) dictionary$columns[[name]] <- old$columns[[name]]
  for (conflict in label_conflicts) {
    name <- conflict$variable
    dictionary$columns[[name]]$value_labels <- Filter(function(entry) {
      length(entry$value) != 1L || is.na(entry$value) || entry$value != conflict$code
    }, dictionary$columns[[name]]$value_labels)
  }
  # Existing definitions and observations describe original-source values,
  # including user-missing codes now filled. Never reinterpret their row IDs
  # as positions in this output or reapply their masks after imputation.
  dictionary$missing_handling <- list(
    input_version_id = attr(before, "nlss_dataset_ref")$version_id,
    observation_basis = "input_version_rows", source_rows = source_rows,
    output_n = nrow(df), indicator_map = indicator_map, label_conflicts = label_conflicts,
    original_missing_provenance = "Retained column missing observations refer only to original_source_rows; they are not output-row masks.",
    imputed_input_rows = lapply(audit, function(item) item$imputed_input_rows))
  attr(df, "nlss_import_contract") <- dictionary
  attr(df, "nlss_import_contract") <- attr(import_prepare_storage(df), "nlss_import_contract")
  df
}

main <- function() {
  opts <- nlss_run_options(commandArgs(trailingOnly = TRUE), "missings")
  if (!is.null(opts[["help"]])) { print_usage(); return(invisible(NULL)) }
  if (parse_bool(opts[["interactive"]], FALSE)) {
    opts <- modifyList(opts, interactive_options())
    opts[["interactive"]] <- TRUE
  }
  df <- nlss_load_input(opts)
  out_dir <- get_workspace_out_dir(df)
  nlss_begin_run("missings", df, opts, out_dir)
  digits <- missing_number(opts, "digits", "defaults.digits", 0, 15, integer = TRUE)
  method <- opts[["method"]]
  if (is.null(method)) method <- get_config_value("modules.missings.method")
  if (length(method) != 1L || is.logical(method) || is.na(method) ||
      !tolower(method) %in% c("auto", "listwise", "impute", "indicator", "drop")) {
    stop("Invalid --method. Use auto, listwise, impute, indicator or drop.")
  }
  method <- tolower(method)
  thresholds <- lapply(c("low", "moderate", "high", "drop", "indicator"), function(key) {
    missing_number(opts, paste0(key, "-threshold"), paste0("modules.missings.", key, "_threshold"), 0, 1)
  })
  names(thresholds) <- c("low", "moderate", "high", "drop", "indicator")
  low_threshold <- thresholds$low
  moderate_threshold <- thresholds$moderate
  high_threshold <- thresholds$high
  drop_threshold <- thresholds$drop
  indicator_threshold <- thresholds$indicator
  if (low_threshold > moderate_threshold || moderate_threshold > high_threshold) {
    stop("Thresholds must satisfy low-threshold <= moderate-threshold <= high-threshold.")
  }
  indicator_suffix <- opts[["indicator-suffix"]]
  if (is.null(indicator_suffix)) indicator_suffix <- get_config_value("modules.missings.indicator_suffix")
  if (!is.character(indicator_suffix) || length(indicator_suffix) != 1L || is.na(indicator_suffix)) {
    stop("--indicator-suffix must be one text value.")
  }
  skew_threshold <- missing_number(opts, "skew-threshold", "modules.missings.skew_threshold", 0)
  max_patterns <- missing_number(opts, "max-patterns", "modules.missings.max_patterns", 1, .Machine$integer.max, integer = TRUE)
  vars <- unique(select_variables(df, opts[["vars"]], default = get_config_value("modules.missings.vars_default")))
  if (!length(vars)) stop("No variables available for missingness analysis.")
  for (name in vars) validate_missing_column(df[[name]], name)
  settings <- list(vars = vars, method = method, low_threshold = low_threshold,
    moderate_threshold = moderate_threshold, high_threshold = high_threshold,
    drop_threshold = drop_threshold, indicator_threshold = indicator_threshold,
    indicator_suffix = indicator_suffix, skew_threshold = skew_threshold,
    max_patterns = max_patterns, digits = digits)
  design <- list(source_n = nrow(df), source_classes = lapply(df[vars], class),
    source_rows = seq_len(nrow(df)), observation_basis = "input_version_rows",
    replay = list(eligible = TRUE, reason = "Deterministic missingness rules on validated ordinary vectors."),
    skewness_definition = "mean((x-mean(x))^3)/sd(x)^3; sd uses n-1; fewer than three observations gives unavailable skewness and mean fill.",
    mode_tie_break = "First category in decreasing-count table order; factor levels or locale-sorted values.")
  nlss_resolve_request(settings, design)

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
  source_rows <- seq_len(nrow(df))
  handling_audit <- setNames(vector("list", length(vars)), vars)
  fills <- list()
  notices <- character()
  rows_removed <- 0
  indicator_map <- list()
  summary_df$decision <- ""
  summary_df$impute_method <- ""
  summary_df$impute_value <- ""
  summary_df$indicator <- ""

  if (method_selected == "listwise") {
    keep <- if (length(vars) > 0) complete.cases(df[, vars, drop = FALSE]) else rep(TRUE, nrow(df))
    rows_removed <- sum(!keep)
    source_rows <- which(keep)
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
      fills[[var]] <- imputed
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

  if (!ncol(output_df)) stop("A dataset must retain at least one variable; no data were published.")
  if (!nrow(output_df)) notices <- c(notices, "No rows remain in the handled dataset; percentages with zero denominators are unavailable and no inference is supported.")
  for (var in vars) {
    fill <- fills[[var]]
    after <- output_df[[var]]
    present <- var %in% names(output_df)
    missing_after <- if (present) which(is.na(after)) else integer()
    filled <- if (is.null(fill)) integer() else which(is.na(df[[var]]) & !is.na(fill$vec))
    value <- if (is.null(fill)) NULL else fill$value
    temporal <- !is.null(value) && inherits(value, c("Date", "POSIXt", "difftime"))
    value_storage <- if (temporal) attr(import_prepare_storage(data.frame(value = value)), "nlss_import_contract")$storage$value else NULL
    handling_audit[[var]] <- list(variable = var, source_type = detect_type(df[[var]]),
      observation_basis = "input_version_rows; missing_after_rows indexes the output",
      missing_before_n = sum(is.na(df[[var]])), missing_before_rows = which(is.na(df[[var]])),
      missing_after_n = if (present) length(missing_after) else NULL,
      missing_after_rows = if (present) missing_after else NULL,
      missing_after_input_rows = if (present) source_rows[missing_after] else NULL,
      observed_before_n = sum(!is.na(df[[var]])), imputed_n = length(filled),
      imputed_input_rows = filled, method = if (is.null(fill)) NULL else fill$method,
      value_raw = if (temporal) as.numeric(value) else value, value_storage = value_storage,
      skewness = if (is.null(fill)) NULL else fill$skewness,
      nonfinite_before_rows = missing_nonfinite_rows(df[[var]]),
      nonfinite_after_rows = if (present) missing_nonfinite_rows(after) else NULL,
      status = if (!present) "dropped" else if (!is.null(fill) && identical(fill$note, "all missing")) "unavailable_all_missing"
        else if (length(filled)) "imputed" else if (method_selected == "listwise") "listwise_selected" else "unchanged",
      indicator = indicator_map[[var]])
    if (!is.null(fill) && identical(fill$note, "all missing")) {
      notices <- c(notices, paste0(var, ": all values are missing; no imputation value is available and the missing values remain."))
    }
    if (length(filled) && identical(detect_type(df[[var]]), "numeric") &&
        length(attr(df, "nlss_import_contract")$columns[[var]]$value_labels)) {
      notices <- c(notices, paste0(var, ": numeric-coded value labels do not establish a measurement level. Arithmetic imputation may create an unlabeled value; declare categorical variables explicitly before handling if category-mode imputation is intended."))
    }
    if (length(missing_nonfinite_rows(df[[var]]))) {
      notices <- c(notices, paste0(var, ": infinite observed values are retained, not classified as missing."))
    }
  }
  label_conflicts <- missing_label_conflicts(df, handling_audit)
  for (conflict in label_conflicts) {
    notices <- c(notices, paste0(conflict$variable, ": imputed value ", conflict$code,
      " equals an original user-missing code; removed conflicting active label '", conflict$label,
      "'. The original label and missing definitions remain in versioned provenance."))
  }
  unchanged <- identical(names(df), names(output_df)) && identical(nrow(df), nrow(output_df)) &&
    all(vapply(names(df), function(name) nlss_data_column_equal(df[[name]], output_df[[name]]), logical(1)))
  if (!unchanged) output_df <- clean_missing_metadata(output_df, df, source_rows, indicator_map, handling_audit, label_conflicts)
  design$source_rows <- source_rows
  design$method_selected <- method_selected
  design$indicator_map <- indicator_map
  design$label_conflicts <- label_conflicts
  nlss_resolve_request(settings, design)

  label_meta <- resolve_label_metadata(output_df)
  summary_df <- add_variable_label_column(summary_df, resolve_label_metadata(df), var_col = "variable")
  summary_df <- add_variable_label_column(summary_df, label_meta, var_col = "indicator")

  patterns_info <- build_patterns_df(df, vars, max_patterns)
  patterns_df <- patterns_info$patterns_df
  if (nrow(patterns_df) > 0) {
    patterns_df$missing_vars_label <- vapply(
      patterns_df$missing_vars,
      map_variable_list_label,
      character(1),
      labels = resolve_label_metadata(df)
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

  vars_display <- map_variable_labels(vars, resolve_label_metadata(df))
  drop_vars_display <- map_variable_labels(drop_vars, resolve_label_metadata(df))
  indicator_vars_display <- map_variable_labels(indicator_vars, label_meta)

  sentences <- c()
  if (!is.na(min_missing) && !is.na(max_missing)) {
    sentences <- c(sentences, sprintf("Missingness ranged from %s%% to %s%%.", format_percent(min_missing, digits), format_percent(max_missing, digits)))
  }
  if (total_n > 0) {
    sentences <- c(sentences, sprintf("Complete cases: %d of %d (%s%%).", complete_cases_n, total_n, format_percent(complete_cases_pct, digits)))
  }
  sentences <- c(sentences, sprintf("Selected handling method: %s.", method_label))
  if (method_selected == "listwise" && total_n > 0) {
    sentences <- c(sentences, sprintf("Listwise deletion removed %d rows (%s%%).", rows_removed, format_percent(rows_removed / total_n * 100, digits)))
  }
  if (length(drop_vars) > 0) {
    sentences <- c(sentences, paste0("Dropped variables (missing >= ", format_percent(drop_threshold * 100, digits), "%): ", paste(drop_vars_display, collapse = ", "), "."))
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

  template_override <- resolve_template_override(opts[["template"]], module = "missings")
  template_path <- if (!is.null(template_override)) {
    template_override
  } else {
    resolve_template_path("missings.default", "missings/default-template.md")
  }
  template_path <- nlss_freeze_template(template_path, "missings.main")
  template_meta <- get_template_meta(template_path)
  report_summary <- summary_df
  for (name in names(report_summary)) if (is.character(report_summary[[name]])) {
    report_summary[[name]] <- nlss_mask_prose_paths(report_summary[[name]], nlss_run_context$root)
  }
  summary_table <- build_missing_table_body(report_summary, digits, template_meta$table)
  pattern_spec <- template_meta$patterns_table
  if (is.null(pattern_spec) && !is.null(template_meta$tables$patterns)) {
    pattern_spec <- template_meta$tables$patterns
  }
  patterns_table <- build_patterns_table_body(patterns_df, digits, pattern_spec)
  patterns_table$body <- nlss_mask_prose_paths(patterns_table$body, nlss_run_context$root)
  nlss_text <- nlss_mask_prose_paths(nlss_text, nlss_run_context$root)
  pattern_note <- nlss_mask_prose_paths(pattern_note, nlss_run_context$root)

  nlss_report_path <- file.path(out_dir, "report_canonical.md")
  table_start <- as.integer(get_next_table_number(nlss_report_path))
  narrative_rows <- build_narrative_rows(report_summary, digits)
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

  caveats <- c(
    "Missingness proportions and the automatic threshold rule do not diagnose MCAR, MAR or MNAR or justify an inferential missing-data strategy.",
    "Single mean/median/mode imputation and missingness indicators do not propagate imputation uncertainty and do not guarantee unbiased estimates or valid standard errors.",
    "Listwise deletion and dropping variables can change the analysis population and estimand. Research context and sensitivity analyses remain necessary.")
  if (method == "auto") warning(caveats[1], call. = FALSE)
  if (method_selected %in% c("impute", "indicator", "drop")) warning(caveats[2], call. = FALSE)
  for (notice in notices) warning(notice, call. = FALSE)
  data_change <- nlss_prepare_data_change(df, output_df, source_rows = source_rows)
  json_df <- import_prepare_storage(output_df)
  results <- list(summary_df = summary_df, patterns_df = patterns_df,
    transformed_df = json_df, transformed_df_storage = attr(json_df, "nlss_import_contract")$storage,
    method_requested = method, method_selected = method_selected, drop_vars = drop_vars,
    indicator_vars = indicator_vars, indicator_map = indicator_map, rows_removed = rows_removed,
    source_rows = source_rows, source_n = nrow(df), output_n = nrow(output_df),
    complete_cases_n = complete_cases_n, complete_cases_pct = complete_cases_pct,
    handling_audit = handling_audit, label_conflicts = label_conflicts, caveats = caveats, notices = notices,
    labels = resolve_label_metadata(output_df), data_change = data_change,
    output_path = data_change$output_path, backup_path = data_change$backup_path)
  nlss_set_result(results)
  for (name in names(analysis_flags)) if (is.character(analysis_flags[[name]])) {
    analysis_flags[[name]] <- nlss_mask_prose_paths(analysis_flags[[name]], nlss_run_context$root)
  }
  nlss_stage_report(nlss_report_path, "Missing data assessment", nlss_table, nlss_text,
    analysis_flags = analysis_flags, template_path = template_path, template_context = template_context)
  # Keep scientific cautions visible even when an authored template omits notes
  # or replaces the numerical narrative. This is output evidence, not a final
  # research report or a claim to have justified a strategy semantically.
  nlss_stage_report(nlss_report_path, "Missing-data handling safeguards", "",
    nlss_mask_prose_paths(paste(c(caveats, notices), collapse = "\n\n"), nlss_run_context$root))
  if (parse_bool(opts[["log"]], get_config_value("defaults.log"))) {
    ctx <- get_run_context()
    redact <- function(value) {
      if (is.factor(value)) value <- as.character(value)
      if (is.character(value)) return(nlss_mask_prose_paths(value, nlss_run_context$root))
      if (is.data.frame(value)) {
        for (name in names(value)) value[[name]] <- redact(value[[name]])
        return(value)
      }
      if (is.list(value)) return(lapply(value, redact))
      value
    }
    legacy <- redact(results)
    legacy$display_note <- "External textual paths are masked in this legacy projection; private run results and immutable data preserve exact values."
    legacy_options <- redact(settings)
    legacy_options$method <- method_selected
    nlss_stage_log(out_dir, "missings", ctx$prompt, ctx$commands, legacy,
      legacy_options, get_user_prompt(opts))
  }
}

nlss_run_main("missings", main)
