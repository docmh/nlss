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
  cat("Data explorer (base R)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript data_explorer.R --csv data.csv [--vars var1,var2]\n")
  cat("  Rscript data_explorer.R --sav data.sav [--vars var1,var2]\n")
  cat("  Rscript data_explorer.R --rds data.rds [--vars var1,var2]\n")
  cat("  Rscript data_explorer.R --rdata data.RData --df data_frame_name [--vars var1,var2]\n")
  cat("  Rscript data_explorer.R --parquet data.parquet [--vars var1,var2]\n")
  cat("  Rscript data_explorer.R --interactive\n")
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
  cat("  --vars LIST            Comma-separated variable names (default: all columns)\n")
  cat("  --digits N             Rounding digits (default: 2)\n")
  cat("  --max-levels N         Max unique levels to list before truncating (default: 20)\n")
  cat("  --top-n N              When truncating, show top N levels (default: 10)\n")
  cat("  --template REF         Template path or template key (optional)\n")
  cat("  --user-prompt TEXT     Original AI user prompt for logging (optional)\n")
  cat("  --log TRUE/FALSE       Write analysis_log.jsonl (default: TRUE)\n")
  cat("  --interactive          Prompt for inputs\n")
  cat("  --help                 Show this help\n")
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
  digits_default <- resolve_config_value("defaults.digits", 2)
  max_levels_default <- resolve_config_value("modules.data_explorer.max_levels", 20)
  top_n_default <- resolve_config_value("modules.data_explorer.top_n", 10)
  opts$digits <- resolve_prompt("Rounding digits", as.character(digits_default))
  opts$`max-levels` <- resolve_prompt("Max levels before truncating", as.character(max_levels_default))
  opts$`top-n` <- resolve_prompt("Top N levels when truncating", as.character(top_n_default))
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

resolve_select_variables <- function(df, vars, group_var = NULL, default = "numeric", include_numeric = FALSE) {
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
    if (default == "all") {
      selected <- available
    } else if (default == "non-numeric") {
      if (include_numeric) {
        selected <- available
      } else {
        selected <- available[!sapply(df, is.numeric)]
        if (length(selected) == 0) selected <- available
      }
    } else {
      selected <- available[sapply(df, is.numeric)]
    }
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

resolve_get_levels <- function(vec) {
  if (exists("get_levels", mode = "function")) {
    return(get("get_levels", mode = "function")(vec))
  }
  if (is.factor(vec)) {
    return(as.character(levels(vec)))
  }
  values <- unique(vec[!is.na(vec)])
  if (length(values) == 0) return(character(0))
  if (is.numeric(values)) {
    return(as.character(sort(values)))
  }
  as.character(sort(values))
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


is_integer_like <- function(x) {
  if (!is.numeric(x)) return(FALSE)
  values <- x[!is.na(x)]
  if (length(values) == 0) return(FALSE)
  all(abs(values - round(values)) < 1e-8)
}

guess_measurement_level <- function(vec, unique_n, max_levels) {
  if (inherits(vec, "Date") || inherits(vec, "POSIXct") || inherits(vec, "POSIXlt")) {
    return(list(level = "interval", note = "date/time"))
  }
  if (is.ordered(vec)) return(list(level = "ordinal", note = "ordered factor"))
  if (is.factor(vec) || is.character(vec) || is.logical(vec)) {
    return(list(level = "nominal", note = "categorical"))
  }
  if (is.numeric(vec)) {
    if (is_integer_like(vec) && unique_n > 0 && unique_n <= max_levels) {
      return(list(level = "ordinal", note = "integer-like with few levels"))
    }
    return(list(level = "interval/ratio", note = "numeric"))
  }
  list(level = "unknown", note = class(vec)[1])
}

format_value <- function(x, digits) {
  if (inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    return(format(x))
  }
  if (is.numeric(x)) {
    return(format(round(x, digits), trim = TRUE, scientific = FALSE))
  }
  as.character(x)
}

format_example_values <- function(values, digits) {
  if (length(values) == 0) return("")
  ex <- head(values, 3)
  paste(vapply(ex, format_value, character(1), digits = digits), collapse = "; ")
}

get_numeric_summary <- function(vec) {
  if (!is.numeric(vec)) {
    return(list(mean = NA_real_, sd = NA_real_, min = NA_real_, max = NA_real_, median = NA_real_, q1 = NA_real_, q3 = NA_real_))
  }
  values <- vec[!is.na(vec)]
  if (length(values) == 0) {
    return(list(mean = NA_real_, sd = NA_real_, min = NA_real_, max = NA_real_, median = NA_real_, q1 = NA_real_, q3 = NA_real_))
  }
  qs <- as.numeric(quantile(values, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 7))
  list(
    mean = mean(values),
    sd = if (length(values) > 1) sd(values) else NA_real_,
    min = min(values),
    max = max(values),
    median = qs[2],
    q1 = qs[1],
    q3 = qs[3]
  )
}


build_levels_table <- function(vec, variable, max_levels, top_n) {
  total_n <- length(vec)
  missing_n <- sum(is.na(vec))
  valid_n <- total_n - missing_n
  values <- vec[!is.na(vec)]
  unique_n <- length(unique(values))

  if (valid_n == 0) {
    df <- data.frame(
      variable = variable,
      level = "(no valid data)",
      n = 0,
      pct_total = 0,
      pct_valid = NA_real_,
      total_n = total_n,
      missing_n = missing_n,
      missing_pct = ifelse(total_n > 0, missing_n / total_n * 100, NA_real_),
      stringsAsFactors = FALSE
    )
    return(list(levels_df = df, levels_truncated = FALSE, levels_note = "no valid data"))
  }

  is_cat <- is.factor(vec) || is.character(vec) || is.logical(vec) || is.ordered(vec)
  integer_like <- is_integer_like(vec)
  allow_numeric_levels <- integer_like && unique_n > 0 && unique_n <= max_levels

  if (!is_cat && !allow_numeric_levels) {
    note <- if (is.numeric(vec)) {
      paste0("levels not listed (unique_n = ", unique_n, ")")
    } else {
      "levels not listed"
    }
    return(list(levels_df = NULL, levels_truncated = FALSE, levels_note = note))
  }

  level_values <- resolve_get_levels(vec)
  counts <- table(factor(values, levels = level_values), useNA = "no")

  levels_truncated <- FALSE
  if (is_cat && length(level_values) > max_levels) {
    counts_sorted <- sort(counts, decreasing = TRUE)
    top_levels <- head(names(counts_sorted), min(top_n, length(counts_sorted)))
    top_counts <- counts_sorted[top_levels]
    other_levels <- setdiff(names(counts_sorted), top_levels)
    other_count <- sum(counts_sorted[other_levels])
    df <- data.frame(
      variable = variable,
      level = names(top_counts),
      n = as.integer(top_counts),
      pct_total = ifelse(total_n > 0, as.numeric(top_counts) / total_n * 100, NA_real_),
      pct_valid = ifelse(valid_n > 0, as.numeric(top_counts) / valid_n * 100, NA_real_),
      total_n = total_n,
      missing_n = missing_n,
      missing_pct = ifelse(total_n > 0, missing_n / total_n * 100, NA_real_),
      stringsAsFactors = FALSE
    )
    if (other_count > 0) {
      df <- rbind(
        df,
        data.frame(
          variable = variable,
          level = "Other (remaining)",
          n = as.integer(other_count),
          pct_total = ifelse(total_n > 0, as.numeric(other_count) / total_n * 100, NA_real_),
          pct_valid = ifelse(valid_n > 0, as.numeric(other_count) / valid_n * 100, NA_real_),
          total_n = total_n,
          missing_n = missing_n,
          missing_pct = ifelse(total_n > 0, missing_n / total_n * 100, NA_real_),
          stringsAsFactors = FALSE
        )
      )
    }
    levels_truncated <- TRUE
  } else {
    df <- data.frame(
      variable = variable,
      level = names(counts),
      n = as.integer(counts),
      pct_total = ifelse(total_n > 0, as.numeric(counts) / total_n * 100, NA_real_),
      pct_valid = ifelse(valid_n > 0, as.numeric(counts) / valid_n * 100, NA_real_),
      total_n = total_n,
      missing_n = missing_n,
      missing_pct = ifelse(total_n > 0, missing_n / total_n * 100, NA_real_),
      stringsAsFactors = FALSE
    )
  }

  note <- if (levels_truncated) {
    paste0("levels truncated to top ", top_n)
  } else {
    ""
  }

  list(levels_df = df, levels_truncated = levels_truncated, levels_note = note)
}


format_num <- function(value, digits) {
  if (is.na(value)) return("NA")
  format(round(value, digits), nsmall = digits, trim = TRUE)
}

format_table_num <- function(value, digits) {
  if (is.na(value)) return("")
  format(round(value, digits), nsmall = digits, trim = TRUE)
}

format_logical_cell <- function(value) {
  if (is.na(value)) return("")
  ifelse(isTRUE(value), "Yes", "No")
}

build_overview_table_body <- function(df, digits, table_spec = NULL) {
  display <- resolve_round_numeric(df, digits)
  default_columns <- list(
    list(key = "variable", label = "Variable"),
    list(key = "class", label = "Class"),
    list(key = "measurement_level", label = "Scale"),
    list(key = "valid_n", label = "n"),
    list(key = "missing_pct", label = "Missing %"),
    list(key = "unique_n", label = "Unique"),
    list(key = "mean", label = "M"),
    list(key = "sd", label = "SD"),
    list(key = "min", label = "Min"),
    list(key = "max", label = "Max")
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
      if (key %in% names(row)) {
        cell <- row[[key]][1]
        if (key %in% c("variable", "class", "storage", "measurement_level", "measurement_note", "example_values", "levels_note")) {
          val <- resolve_as_cell_text(cell)
        } else if (key %in% c("total_n", "valid_n", "missing_n", "unique_n")) {
          val <- ifelse(is.na(cell), "", as.character(cell))
        } else if (key == "missing_pct") {
          val <- resolve_format_percent(cell, digits)
        } else if (key %in% c("mean", "sd", "min", "max", "median", "q1", "q3")) {
          val <- format_table_num(cell, digits)
        } else if (key %in% c("levels_included", "levels_truncated")) {
          if (is.logical(cell)) {
            val <- format_logical_cell(cell)
          } else {
            val <- resolve_as_cell_text(cell)
          }
        } else if (is.numeric(cell)) {
          val <- format_table_num(cell, digits)
        } else if (is.logical(cell)) {
          val <- format_logical_cell(cell)
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
  headers <- vapply(columns, function(col) {
    if (!is.null(col$label) && nzchar(col$label)) col$label else col$key
  }, character(1))
  list(
    body = resolve_render_markdown_table(headers, rows),
    columns = vapply(columns, function(col) col$key, character(1))
  )
}

build_levels_table_body <- function(df, digits, table_spec = NULL) {
  if (nrow(df) == 0) {
    return(list(
      body = "(No level tables produced; see variable overview for unique counts.)",
      columns = character(0)
    ))
  }
  display <- resolve_round_numeric(df, digits)
  default_columns <- list(
    list(key = "variable", label = "Variable"),
    list(key = "level", label = "Level"),
    list(key = "n", label = "n"),
    list(key = "pct_total", label = "%"),
    list(key = "pct_valid", label = "Valid %")
  )
  columns <- resolve_normalize_table_columns(
    if (!is.null(table_spec$columns)) table_spec$columns else NULL,
    default_columns
  )
  rows <- list()
  combo_df <- unique(display[, "variable", drop = FALSE])
  for (idx in seq_len(nrow(combo_df))) {
    var <- combo_df$variable[idx]
    subset <- display[display$variable == var, , drop = FALSE]
    total_n <- subset$total_n[1]
    missing_n <- subset$missing_n[1]
    missing_pct <- subset$missing_pct[1]
    for (i in seq_len(nrow(subset))) {
      row <- subset[i, , drop = FALSE]
      row_vals <- character(0)
      for (col in columns) {
        key <- col$key
        val <- ""
        if (key %in% c("variable", "level")) {
          val <- resolve_as_cell_text(row[[key]][1])
        } else if (key %in% c("n", "total_n", "missing_n")) {
          val <- ifelse(is.na(row[[key]][1]), "", as.character(row[[key]][1]))
        } else if (key %in% c("pct_total", "pct_valid", "missing_pct")) {
          val <- resolve_format_percent(row[[key]][1], digits)
        } else if (key %in% names(row)) {
          cell <- row[[key]][1]
          if (is.numeric(cell)) {
            val <- format_table_num(cell, digits)
          } else if (is.logical(cell)) {
            val <- format_logical_cell(cell)
          } else {
            val <- resolve_as_cell_text(cell)
          }
        }
        row_vals <- c(row_vals, val)
      }
      rows[[length(rows) + 1]] <- row_vals
    }
    if (!is.na(missing_n) && missing_n > 0) {
      row_vals <- character(0)
      for (col in columns) {
        key <- col$key
        val <- ""
        if (key == "variable") {
          val <- resolve_as_cell_text(var)
        } else if (key == "level") {
          val <- "Missing"
        } else if (key == "n") {
          val <- as.character(missing_n)
        } else if (key == "pct_total") {
          val <- resolve_format_percent(missing_pct, digits)
        } else if (key == "pct_valid") {
          val <- ""
        } else if (key == "total_n") {
          val <- ifelse(is.na(total_n), "", as.character(total_n))
        } else if (key == "missing_n") {
          val <- ifelse(is.na(missing_n), "", as.character(missing_n))
        } else if (key == "missing_pct") {
          val <- resolve_format_percent(missing_pct, digits)
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

build_overview_note_tokens <- function() {
  note_default <- "Scale levels are heuristic; interval/ratio cannot be distinguished automatically."
  list(
    overview_note_default = note_default,
    overview_note_body = note_default
  )
}

build_levels_note_tokens <- function(levels_df, overview_df, column_keys, top_n) {
  if (nrow(levels_df) == 0) {
    note_default <- "None."
    return(list(
      levels_note_default = note_default,
      levels_note_body = note_default,
      pct_total_note = "",
      pct_valid_note = "",
      missing_note = "",
      truncation_note = ""
    ))
  }
  pct_total_note <- ""
  pct_valid_note <- ""
  if ("pct_total" %in% column_keys) {
    pct_total_note <- "% = percent of total."
  }
  if ("pct_valid" %in% column_keys) {
    pct_valid_note <- "Valid % excludes missing values."
  }
  missing_note <- "Missing values are listed separately."
  truncation_note <- ""
  if (any(overview_df$levels_truncated, na.rm = TRUE)) {
    truncation_note <- paste0(
      "Levels truncated to top ",
      top_n,
      "; remaining levels combined as Other (remaining)."
    )
  }
  note_parts <- c(pct_total_note, pct_valid_note, missing_note, truncation_note)
  note_default <- paste(note_parts[nzchar(note_parts)], collapse = " ")
  list(
    levels_note_default = note_default,
    levels_note_body = note_default,
    pct_total_note = pct_total_note,
    pct_valid_note = pct_valid_note,
    missing_note = missing_note,
    truncation_note = truncation_note
  )
}

build_explorer_narrative_rows <- function(overview_df, levels_df, digits) {
  rows <- list()
  for (i in seq_len(nrow(overview_df))) {
    row <- overview_df[i, , drop = FALSE]
    label <- resolve_as_cell_text(row$variable)
    total_n <- row$total_n
    valid_n <- row$valid_n
    missing_n <- row$missing_n
    missing_pct <- row$missing_pct
    unique_n <- row$unique_n

    total_n_str <- ifelse(is.na(total_n), "NA", as.character(total_n))
    valid_n_str <- ifelse(is.na(valid_n), "NA", as.character(valid_n))
    missing_n_str <- ifelse(is.na(missing_n), "NA", as.character(missing_n))
    missing_pct_str <- ifelse(is.na(missing_pct), "NA", resolve_format_percent(missing_pct, digits))
    unique_n_str <- ifelse(is.na(unique_n), "NA", as.character(unique_n))

    mean_str <- ifelse(is.na(row$mean), "NA", format_num(row$mean, digits))
    sd_str <- ifelse(is.na(row$sd), "NA", format_num(row$sd, digits))
    min_str <- ifelse(is.na(row$min), "NA", format_num(row$min, digits))
    max_str <- ifelse(is.na(row$max), "NA", format_num(row$max, digits))
    median_str <- ifelse(is.na(row$median), "NA", format_num(row$median, digits))
    q1_str <- ifelse(is.na(row$q1), "NA", format_num(row$q1, digits))
    q3_str <- ifelse(is.na(row$q3), "NA", format_num(row$q3, digits))

    missing_text <- paste0("Missing = ", missing_n_str, " (", missing_pct_str, "%)")
    levels_text <- ""
    if (nrow(levels_df) > 0) {
      subset <- levels_df[levels_df$variable == row$variable[1], , drop = FALSE]
      if (nrow(subset) > 0) {
        level_parts <- character(0)
        for (j in seq_len(nrow(subset))) {
          lv <- subset[j, , drop = FALSE]
          level_parts <- c(
            level_parts,
            sprintf(
              "%s (n = %s, valid %% = %s)",
              lv$level[1],
              ifelse(is.na(lv$n), "NA", as.character(lv$n)),
              ifelse(is.na(lv$pct_valid), "NA", resolve_format_percent(lv$pct_valid, digits))
            )
          )
        }
        levels_text <- paste(level_parts, collapse = "; ")
      }
    }

    if (is.na(total_n) || total_n == 0) {
      line <- sprintf("%s: no observations available.", label)
    } else {
      line <- sprintf(
        "%s (%s, scale: %s): n = %s, missing = %s (%s%%), unique values = %s.",
        label,
        resolve_as_cell_text(row$class),
        resolve_as_cell_text(row$measurement_level),
        valid_n_str,
        missing_n_str,
        missing_pct_str,
        unique_n_str
      )
      if (!is.na(row$mean)) {
        line <- paste0(
          line,
          " M = ", mean_str,
          ", SD = ", sd_str,
          ", Min = ", min_str,
          ", Max = ", max_str,
          "."
        )
      }
      if (nzchar(levels_text)) {
        line <- paste0(line, " Levels: ", levels_text, ".")
        if (isTRUE(row$levels_truncated[1])) {
          line <- paste0(line, " Remaining levels combined as Other.")
        }
      }
    }

    rows[[length(rows) + 1]] <- list(
      label = label,
      variable = resolve_as_cell_text(row$variable),
      class = resolve_as_cell_text(row$class),
      storage = resolve_as_cell_text(row$storage),
      measurement_level = resolve_as_cell_text(row$measurement_level),
      measurement_note = resolve_as_cell_text(row$measurement_note),
      total_n = total_n_str,
      valid_n = valid_n_str,
      missing_n = missing_n_str,
      missing_pct = missing_pct_str,
      unique_n = unique_n_str,
      example_values = resolve_as_cell_text(row$example_values),
      mean = mean_str,
      sd = sd_str,
      min = min_str,
      max = max_str,
      median = median_str,
      q1 = q1_str,
      q3 = q3_str,
      levels_text = levels_text,
      missing_text = missing_text,
      levels_truncated = format_logical_cell(row$levels_truncated[1]),
      full_sentence = line
    )
  }
  rows
}

format_apa_overview_table <- function(df, digits) {
  display <- resolve_round_numeric(df, digits)
  headers <- c("Variable", "Class", "Scale", "n", "Missing %", "Unique", "M", "SD", "Min", "Max")
  md <- paste0("Table 1\nVariable overview\n\n| ", paste(headers, collapse = " | "), " |\n")
  md <- paste0(md, "| ", paste(rep("---", length(headers)), collapse = " | "), " |\n")

  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    row_vals <- c(
      row$variable,
      row$class,
      row$measurement_level,
      ifelse(is.na(row$valid_n), "", as.character(row$valid_n)),
      resolve_format_percent(row$missing_pct, digits),
      ifelse(is.na(row$unique_n), "", as.character(row$unique_n)),
      ifelse(is.na(row$mean), "", format_num(row$mean, digits)),
      ifelse(is.na(row$sd), "", format_num(row$sd, digits)),
      ifelse(is.na(row$min), "", format_num(row$min, digits)),
      ifelse(is.na(row$max), "", format_num(row$max, digits))
    )
    md <- paste0(md, "| ", paste(row_vals, collapse = " | "), " |\n")
  }

  md <- paste0(md, "\nNote. Scale levels are heuristic; interval/ratio cannot be distinguished automatically.\n")
  md
}

format_apa_levels_table <- function(df, digits) {
  if (nrow(df) == 0) {
    return("Table 2\nValue levels\n\n(No level tables produced; see variable overview for unique counts.)\n")
  }

  display <- resolve_round_numeric(df, digits)
  headers <- c("Variable", "Level", "n", "%", "Valid %")
  md <- paste0("Table 2\nValue levels\n\n| ", paste(headers, collapse = " | "), " |\n")
  md <- paste0(md, "| ", paste(rep("---", length(headers)), collapse = " | "), " |\n")

  combo_df <- unique(display[, "variable", drop = FALSE])
  for (idx in seq_len(nrow(combo_df))) {
    var <- combo_df$variable[idx]
    subset <- display[display$variable == var, , drop = FALSE]
    for (i in seq_len(nrow(subset))) {
      row <- subset[i, ]
      row_vals <- c(
        var,
        row$level,
        ifelse(is.na(row$n), "", as.character(row$n)),
        resolve_format_percent(row$pct_total, digits),
        resolve_format_percent(row$pct_valid, digits)
      )
      md <- paste0(md, "| ", paste(row_vals, collapse = " | "), " |\n")
    }

    missing_n <- subset$missing_n[1]
    missing_pct <- subset$missing_pct[1]
    if (!is.na(missing_n) && missing_n > 0) {
      row_vals <- c(
        var,
        "Missing",
        as.character(missing_n),
        resolve_format_percent(missing_pct, digits),
        ""
      )
      md <- paste0(md, "| ", paste(row_vals, collapse = " | "), " |\n")
    }
  }

  md <- paste0(md, "\nNote. % = percent of total; Valid % excludes missing values.\n")
  md
}

format_apa_text <- function(overview_df, levels_df, digits) {
  lines <- character(0)
  for (i in seq_len(nrow(overview_df))) {
    row <- overview_df[i, ]
    label <- row$variable
    total_n <- row$total_n
    valid_n <- row$valid_n
    missing_n <- row$missing_n
    missing_pct <- row$missing_pct
    unique_n <- row$unique_n

    if (is.na(total_n) || total_n == 0) {
      lines <- c(lines, sprintf("%s: no observations available.", label))
      next
    }

    line <- sprintf(
      "%s (%s, scale: %s): n = %s, missing = %s (%s%%), unique values = %s.",
      label,
      row$class,
      row$measurement_level,
      ifelse(is.na(valid_n), "NA", as.character(valid_n)),
      ifelse(is.na(missing_n), "NA", as.character(missing_n)),
      ifelse(is.na(missing_pct), "NA", resolve_format_percent(missing_pct, digits)),
      ifelse(is.na(unique_n), "NA", as.character(unique_n))
    )

    if (!is.na(row$mean)) {
      line <- paste0(
        line,
        " M = ", format_num(row$mean, digits),
        ", SD = ", format_num(row$sd, digits),
        ", Min = ", format_num(row$min, digits),
        ", Max = ", format_num(row$max, digits), "."
      )
    }

    if (nrow(levels_df) > 0) {
      subset <- levels_df[levels_df$variable == label, , drop = FALSE]
      if (nrow(subset) > 0) {
        level_parts <- character(0)
        for (j in seq_len(nrow(subset))) {
          lv <- subset[j, ]
          level_parts <- c(
            level_parts,
            sprintf(
              "%s (n = %s, valid %% = %s)",
              lv$level,
              ifelse(is.na(lv$n), "NA", as.character(lv$n)),
              ifelse(is.na(lv$pct_valid), "NA", resolve_format_percent(lv$pct_valid, digits))
            )
          )
        }
        line <- paste0(line, " Levels: ", paste(level_parts, collapse = "; "), ".")
        if (isTRUE(row$levels_truncated)) {
          line <- paste0(line, " Remaining levels combined as Other.")
        }
      }
    }

    lines <- c(lines, line)
  }
  paste(lines, collapse = "\n")
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
  vars_default <- resolve_config_value("modules.data_explorer.vars_default", "all")
  max_levels_default <- resolve_config_value("modules.data_explorer.max_levels", 20)
  top_n_default <- resolve_config_value("modules.data_explorer.top_n", 10)
  digits <- if (!is.null(opts$digits)) as.numeric(opts$digits) else digits_default
  max_levels <- if (!is.null(opts$`max-levels`)) as.integer(opts$`max-levels`) else max_levels_default
  top_n <- if (!is.null(opts$`top-n`)) as.integer(opts$`top-n`) else top_n_default
  df <- resolve_load_dataframe(opts)
  out_dir <- resolve_get_workspace_out_dir(df)
  vars <- resolve_select_variables(df, opts$vars, default = vars_default)
  if (length(vars) == 0) stop("No variables available for exploration.")

  overview_rows <- list()
  levels_rows <- list()

  for (var in vars) {
    vec <- df[[var]]
    total_n <- length(vec)
    missing_n <- sum(is.na(vec))
    valid_n <- total_n - missing_n
    values <- vec[!is.na(vec)]
    unique_vals <- unique(values)
    unique_n <- length(unique_vals)

    measurement <- guess_measurement_level(vec, unique_n, max_levels)
    example_values <- format_example_values(unique_vals, digits)
    numeric_summary <- get_numeric_summary(vec)

    levels_info <- build_levels_table(vec, var, max_levels, top_n)
    levels_included <- !is.null(levels_info$levels_df)
    levels_truncated <- isTRUE(levels_info$levels_truncated)
    levels_note <- levels_info$levels_note

    if (levels_included) {
      levels_rows[[length(levels_rows) + 1]] <- levels_info$levels_df
    }

    overview_rows[[length(overview_rows) + 1]] <- data.frame(
      variable = var,
      class = class(vec)[1],
      storage = typeof(vec),
      measurement_level = measurement$level,
      measurement_note = measurement$note,
      total_n = total_n,
      valid_n = valid_n,
      missing_n = missing_n,
      missing_pct = ifelse(total_n > 0, missing_n / total_n * 100, NA_real_),
      unique_n = unique_n,
      example_values = example_values,
      mean = numeric_summary$mean,
      sd = numeric_summary$sd,
      min = numeric_summary$min,
      max = numeric_summary$max,
      median = numeric_summary$median,
      q1 = numeric_summary$q1,
      q3 = numeric_summary$q3,
      levels_included = levels_included,
      levels_truncated = levels_truncated,
      levels_note = levels_note,
      stringsAsFactors = FALSE
    )
  }

  overview_df <- do.call(rbind, overview_rows)
  if (length(levels_rows) > 0) {
    levels_df <- do.call(rbind, levels_rows)
  } else {
    levels_df <- data.frame(
      variable = character(0),
      level = character(0),
      n = integer(0),
      pct_total = numeric(0),
      pct_valid = numeric(0),
      total_n = integer(0),
      missing_n = integer(0),
      missing_pct = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  apa_report_path <- file.path(out_dir, "apa_report.md")
  apa_tables <- paste(
    format_apa_overview_table(overview_df, digits),
    "\n",
    format_apa_levels_table(levels_df, digits)
  )
  apa_text <- format_apa_text(overview_df, levels_df, digits)
  template_override <- resolve_template_override(opts$template, module = "data_explorer")
  template_path <- if (!is.null(template_override)) {
    template_override
  } else {
    resolve_get_template_path("data_explorer.default", "data-explorer/default-template.md")
  }
  template_meta <- resolve_get_template_meta(template_path)
  overview_spec <- if (!is.null(template_meta) && !is.null(template_meta[["table"]])) template_meta[["table"]] else NULL
  if (is.null(overview_spec) &&
      !is.null(template_meta) &&
      !is.null(template_meta[["tables"]]) &&
      !is.null(template_meta[["tables"]][["overview"]])) {
    overview_spec <- template_meta[["tables"]][["overview"]]
  }
  levels_spec <- if (!is.null(template_meta) && !is.null(template_meta[["levels_table"]])) template_meta[["levels_table"]] else NULL
  if (is.null(levels_spec) &&
      !is.null(template_meta) &&
      !is.null(template_meta[["tables"]]) &&
      !is.null(template_meta[["tables"]][["levels"]])) {
    levels_spec <- template_meta[["tables"]][["levels"]]
  }
  overview_table <- build_overview_table_body(overview_df, digits, overview_spec)
  levels_table <- build_levels_table_body(levels_df, digits, levels_spec)
  overview_note_tokens <- build_overview_note_tokens()
  levels_note_tokens <- build_levels_note_tokens(levels_df, overview_df, levels_table$columns, top_n)
  narrative_rows <- build_explorer_narrative_rows(overview_df, levels_df, digits)
  table_start <- as.integer(resolve_get_next_table_number(apa_report_path))
  template_context <- list(
    tokens = c(
      list(
        overview_table_body = overview_table$body,
        levels_table_body = levels_table$body,
        table_number_next = as.character(table_start + 1),
        narrative_default = apa_text
      ),
      overview_note_tokens,
      levels_note_tokens
    ),
    narrative_rows = narrative_rows
  )
  analysis_flags <- list(
    vars = vars,
    `max-levels` = max_levels,
    `top-n` = top_n,
    digits = digits
  )
  resolve_append_apa_report(
    apa_report_path,
    "Data exploration",
    apa_tables,
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
      module = "data_explorer",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(overview_df = overview_df, levels_df = levels_df),
      options = list(digits = digits, vars = vars, max_levels = max_levels, top_n = top_n),
      user_prompt = resolve_get_user_prompt(opts)
    )
  }
}

main()
