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
  cat("Frequencies (base R)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript frequencies.R --csv data.csv [--vars var1,var2] [--group group_var]\n")
  cat("  Rscript frequencies.R --sav data.sav [--vars var1,var2] [--group group_var]\n")
  cat("  Rscript frequencies.R --rds data.rds [--vars var1,var2] [--group group_var]\n")
  cat("  Rscript frequencies.R --rdata data.RData --df data_frame_name [--vars var1,var2] [--group group_var]\n")
  cat("  Rscript frequencies.R --parquet data.parquet [--vars var1,var2] [--group group_var]\n")
  cat("  Rscript frequencies.R --interactive\n")
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
  cat("  --vars LIST            Comma-separated variable names (default: all non-numeric)\n")
  cat("  --group NAME           Grouping variable name (optional)\n")
  cat("  --include-numeric      Include numeric columns when --vars is omitted\n")
  cat("  --digits N             Rounding digits for percentages (default: 2)\n")
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

  opts$vars <- resolve_prompt("Variables (comma-separated, blank for all non-numeric)", "")
  opts$group <- resolve_prompt("Grouping variable (blank for none)", "")
  include_numeric_default <- resolve_config_value("modules.frequencies.include_numeric", FALSE)
  opts$`include-numeric` <- resolve_prompt(
    "Include numeric columns when vars omitted TRUE/FALSE",
    ifelse(isTRUE(include_numeric_default), "TRUE", "FALSE")
  )
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
  if (length(value) == 0 || is.null(value) || is.na(value)) return("")
  as.character(value)
}


build_freq_rows <- function(vec, variable, group_label) {
  total_n <- length(vec)
  missing_n <- sum(is.na(vec))
  valid_n <- total_n - missing_n
  missing_pct <- ifelse(total_n > 0, missing_n / total_n * 100, NA_real_)

  levels <- resolve_get_levels(vec)
  if (valid_n == 0 || length(levels) == 0) {
    return(data.frame(
      variable = variable,
      group = group_label,
      level = "(no valid data)",
      n = 0,
      pct_total = 0,
      pct_valid = NA_real_,
      total_n = total_n,
      missing_n = missing_n,
      missing_pct = missing_pct,
      stringsAsFactors = FALSE
    ))
  }

  counts <- table(factor(vec, levels = levels), useNA = "no")
  pct_total <- if (total_n > 0) {
    as.numeric(counts) / total_n * 100
  } else {
    rep(NA_real_, length(counts))
  }
  pct_valid <- if (valid_n > 0) {
    as.numeric(counts) / valid_n * 100
  } else {
    rep(NA_real_, length(counts))
  }
  data.frame(
    variable = variable,
    group = group_label,
    level = names(counts),
    n = as.integer(counts),
    pct_total = pct_total,
    pct_valid = pct_valid,
    total_n = total_n,
    missing_n = missing_n,
    missing_pct = missing_pct,
    stringsAsFactors = FALSE
  )
}

build_summary <- function(df, vars, group_var = NULL) {
  rows <- list()
  if (!is.null(group_var) && group_var != "") {
    group_vec <- df[[group_var]]
    group_levels <- unique(group_vec)
    for (g in group_levels) {
      idx <- if (is.na(g)) is.na(group_vec) else group_vec == g
      sub_df <- df[idx, , drop = FALSE]
      group_label <- ifelse(is.na(g), "NA", as.character(g))
      for (var in vars) {
        rows[[length(rows) + 1]] <- build_freq_rows(sub_df[[var]], var, group_label)
      }
    }
  } else {
    for (var in vars) {
      rows[[length(rows) + 1]] <- build_freq_rows(df[[var]], var, "")
    }
  }

  summary_df <- do.call(rbind, rows)
  numeric_cols <- setdiff(names(summary_df), c("variable", "group", "level"))
  for (col in numeric_cols) {
    summary_df[[col]] <- as.numeric(summary_df[[col]])
  }
  summary_df
}


format_apa_table <- function(df, digits) {
  display <- resolve_round_numeric(df, digits)
  display$group <- as.character(display$group)
  display$group[is.na(display$group)] <- "NA"
  display$variable_display <- if ("variable_label" %in% names(display)) display$variable_label else display$variable
  display$level_display <- if ("level_label" %in% names(display)) display$level_label else display$level
  display$group_display <- if ("group_label" %in% names(display)) display$group_label else display$group
  display$group_display <- as.character(display$group_display)
  display$group_display[is.na(display$group_display)] <- "NA"
  use_group <- !all(display$group == "")

  headers <- c("Variable", if (use_group) "Group", "Level", "n", "%", "Valid %")
  md <- paste0("Table 1\nFrequencies\n\n| ", paste(headers, collapse = " | "), " |\n")
  md <- paste0(md, "| ", paste(rep("---", length(headers)), collapse = " | "), " |\n")

  combo_df <- unique(display[, c("variable", "group")])
  for (idx in seq_len(nrow(combo_df))) {
    var <- combo_df$variable[idx]
    grp <- combo_df$group[idx]
    subset <- display[display$variable == var & display$group == grp, , drop = FALSE]
    var_label <- subset$variable_display[1]
    grp_label <- subset$group_display[1]
    for (i in seq_len(nrow(subset))) {
      row <- subset[i, ]
      row_vals <- c(
        var_label,
        if (use_group) grp_label,
        row$level_display,
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
        var_label,
        if (use_group) grp_label,
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

format_apa_text <- function(df, digits) {
  display <- resolve_round_numeric(df, digits)
  display$group <- as.character(display$group)
  display$group[is.na(display$group)] <- "NA"
  display$variable_display <- if ("variable_label" %in% names(display)) display$variable_label else display$variable
  display$level_display <- if ("level_label" %in% names(display)) display$level_label else display$level
  display$group_display <- if ("group_label" %in% names(display)) display$group_label else display$group
  display$group_display <- as.character(display$group_display)
  display$group_display[is.na(display$group_display)] <- "NA"
  lines <- character(0)
  combo_df <- unique(display[, c("variable", "group")])
  for (idx in seq_len(nrow(combo_df))) {
    var <- combo_df$variable[idx]
    grp <- combo_df$group[idx]
    subset <- display[display$variable == var & display$group == grp, , drop = FALSE]
    total_n <- subset$total_n[1]
    missing_n <- subset$missing_n[1]
    missing_pct <- subset$missing_pct[1]
    var_label <- subset$variable_display[1]
    grp_label <- subset$group_display[1]
    label <- if (is.na(grp) || grp == "") {
      var_label
    } else {
      paste0("Group ", grp_label, ", ", var_label)
    }
    if (is.na(total_n) || total_n == 0) {
      line <- sprintf("%s: no observations available.", label)
      lines <- c(lines, line)
      next
    }

    if (nrow(subset) == 1 && subset$level[1] == "(no valid data)") {
      line <- sprintf(
        "%s (n = %s): no valid observations. Missing = %s (%s%%).",
        label,
        as.character(total_n),
        ifelse(is.na(missing_n), "NA", as.character(missing_n)),
        ifelse(is.na(missing_pct), "NA", resolve_format_percent(missing_pct, digits))
      )
      lines <- c(lines, line)
      next
    }

    level_parts <- character(0)
    for (i in seq_len(nrow(subset))) {
      row <- subset[i, ]
      level_parts <- c(
        level_parts,
        sprintf(
          "%s (n = %s, valid %% = %s)",
          row$level_display,
          ifelse(is.na(row$n), "NA", as.character(row$n)),
          ifelse(is.na(row$pct_valid), "NA", resolve_format_percent(row$pct_valid, digits))
        )
      )
    }

    line <- sprintf(
      "%s (n = %s): %s. Missing = %s (%s%%).",
      label,
      as.character(total_n),
      paste(level_parts, collapse = "; "),
      ifelse(is.na(missing_n), "NA", as.character(missing_n)),
      ifelse(is.na(missing_pct), "NA", resolve_format_percent(missing_pct, digits))
    )
    lines <- c(lines, line)
  }
  paste(lines, collapse = "\n")
}

format_num <- function(value, digits) {
  if (is.na(value)) return("")
  format(round(value, digits), nsmall = digits, trim = TRUE)
}

build_frequencies_table_body <- function(summary_df, digits, table_spec = NULL) {
  display <- resolve_round_numeric(summary_df, digits)
  display$group <- as.character(display$group)
  display$group[is.na(display$group)] <- "NA"
  display$variable_display <- if ("variable_label" %in% names(display)) display$variable_label else display$variable
  display$level_display <- if ("level_label" %in% names(display)) display$level_label else display$level
  display$group_display <- if ("group_label" %in% names(display)) display$group_label else display$group
  display$group_display <- as.character(display$group_display)
  display$group_display[is.na(display$group_display)] <- "NA"

  default_columns <- list(
    list(key = "variable", label = "Variable"),
    list(key = "group", label = "Group", drop_if_empty = TRUE),
    list(key = "level", label = "Level"),
    list(key = "n", label = "n"),
    list(key = "pct_total", label = "%"),
    list(key = "pct_valid", label = "Valid %", drop_if_empty = TRUE)
  )
  columns <- resolve_normalize_table_columns(
    if (!is.null(table_spec$columns)) table_spec$columns else NULL,
    default_columns
  )

  rows <- list()
  combo_df <- unique(display[, c("variable", "group")])
  for (idx in seq_len(nrow(combo_df))) {
    var <- combo_df$variable[idx]
    grp <- combo_df$group[idx]
    subset <- display[display$variable == var & display$group == grp, , drop = FALSE]
    total_n <- subset$total_n[1]
    missing_n <- subset$missing_n[1]
    missing_pct <- subset$missing_pct[1]
    var_label <- subset$variable_display[1]
    grp_label <- subset$group_display[1]

    for (i in seq_len(nrow(subset))) {
      row <- subset[i, , drop = FALSE]
      row_vals <- character(0)
      for (col in columns) {
        key <- col$key
        val <- ""
        if (key %in% c("variable", "group", "level")) {
          if (key == "variable") {
            val <- resolve_as_cell_text(row$variable_display[1])
          } else if (key == "group") {
            val <- resolve_as_cell_text(row$group_display[1])
          } else {
            val <- resolve_as_cell_text(row$level_display[1])
          }
        } else if (key %in% c("n", "total_n", "missing_n")) {
          val <- ifelse(is.na(row[[key]][1]), "", as.character(row[[key]][1]))
        } else if (key %in% c("pct_total", "pct_valid", "missing_pct")) {
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

    if (!is.na(missing_n) && missing_n > 0) {
      row_vals <- character(0)
      for (col in columns) {
        key <- col$key
        val <- ""
        if (key == "variable") {
          val <- resolve_as_cell_text(var_label)
        } else if (key == "group") {
          val <- resolve_as_cell_text(grp_label)
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

build_frequencies_note_tokens <- function(column_keys) {
  pct_total_note <- ""
  pct_valid_note <- ""
  if ("pct_total" %in% column_keys) {
    pct_total_note <- "% = percent of total."
  }
  if ("pct_valid" %in% column_keys) {
    pct_valid_note <- "Valid % excludes missing values."
  }
  missing_note <- "Missing values are listed separately."
  note_parts <- c(pct_total_note, pct_valid_note, missing_note)
  list(
    pct_total_note = pct_total_note,
    pct_valid_note = pct_valid_note,
    missing_note = missing_note,
    note_default = paste(note_parts[nzchar(note_parts)], collapse = " ")
  )
}

build_frequencies_narrative_rows <- function(summary_df, digits) {
  display <- resolve_round_numeric(summary_df, digits)
  display$group <- as.character(display$group)
  display$group[is.na(display$group)] <- "NA"
  display$variable_display <- if ("variable_label" %in% names(display)) display$variable_label else display$variable
  display$level_display <- if ("level_label" %in% names(display)) display$level_label else display$level
  display$group_display <- if ("group_label" %in% names(display)) display$group_label else display$group
  display$group_display <- as.character(display$group_display)
  display$group_display[is.na(display$group_display)] <- "NA"
  rows <- list()
  combo_df <- unique(display[, c("variable", "group")])
  for (idx in seq_len(nrow(combo_df))) {
    var <- combo_df$variable[idx]
    grp <- combo_df$group[idx]
    subset <- display[display$variable == var & display$group == grp, , drop = FALSE]
    total_n <- subset$total_n[1]
    missing_n <- subset$missing_n[1]
    missing_pct <- subset$missing_pct[1]
    valid_n <- ifelse(is.na(total_n) || is.na(missing_n), NA, total_n - missing_n)
    var_label <- subset$variable_display[1]
    grp_label <- subset$group_display[1]
    label <- if (grp == "") var_label else paste0("Group ", grp_label, ", ", var_label)

    total_n_str <- ifelse(is.na(total_n), "NA", as.character(total_n))
    missing_n_str <- ifelse(is.na(missing_n), "NA", as.character(missing_n))
    missing_pct_str <- ifelse(is.na(missing_pct), "NA", resolve_format_percent(missing_pct, digits))
    valid_n_str <- ifelse(is.na(valid_n), "NA", as.character(valid_n))
    missing_text <- paste0("Missing = ", missing_n_str, " (", missing_pct_str, "%)")

    levels_text <- ""
    if (is.na(total_n) || total_n == 0) {
      line <- sprintf("%s: no observations available.", label)
    } else if (nrow(subset) == 1 && subset$level[1] == "(no valid data)") {
      levels_text <- "no valid observations"
      line <- sprintf(
        "%s (n = %s): no valid observations. Missing = %s (%s%%).",
        label,
        total_n_str,
        missing_n_str,
        missing_pct_str
      )
    } else {
      level_parts <- character(0)
      for (i in seq_len(nrow(subset))) {
        row <- subset[i, , drop = FALSE]
        level_parts <- c(
          level_parts,
          sprintf(
            "%s (n = %s, valid %% = %s)",
            row$level_display[1],
            ifelse(is.na(row$n), "NA", as.character(row$n)),
            ifelse(is.na(row$pct_valid), "NA", resolve_format_percent(row$pct_valid, digits))
          )
        )
      }
      levels_text <- paste(level_parts, collapse = "; ")
      line <- sprintf(
        "%s (n = %s): %s. %s.",
        label,
        total_n_str,
        levels_text,
        missing_text
      )
    }

    rows[[length(rows) + 1]] <- list(
      label = label,
      variable = var,
      group = grp,
      total_n = total_n_str,
      valid_n = valid_n_str,
      missing_n = missing_n_str,
      missing_pct = missing_pct_str,
      missing_text = missing_text,
      levels_text = levels_text,
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
  vars_default <- resolve_config_value("modules.frequencies.vars_default", "non-numeric")
  include_numeric_default <- resolve_config_value("modules.frequencies.include_numeric", FALSE)
  digits <- if (!is.null(opts$digits)) as.numeric(opts$digits) else digits_default
  df <- resolve_load_dataframe(opts)
  out_dir <- resolve_get_workspace_out_dir(df)
  group_var <- if (!is.null(opts$group) && opts$group != "") opts$group else NULL
  if (!is.null(group_var) && !(group_var %in% names(df))) {
    stop("Grouping variable not found in data frame.")
  }

  include_numeric <- resolve_parse_bool(opts$`include-numeric`, default = include_numeric_default)
  vars <- resolve_select_variables(
    df,
    opts$vars,
    group_var,
    default = vars_default,
    include_numeric = include_numeric
  )
  if (length(vars) == 0) stop("No variables available for frequency analysis.")

  summary_df <- build_summary(df, vars, group_var)
  label_meta <- resolve_label_metadata(df)
  summary_df <- add_variable_label_column(summary_df, label_meta, var_col = "variable")
  summary_df <- add_value_label_column(summary_df, label_meta, var_col = "variable", value_col = "level")
  summary_df <- add_group_label_column(summary_df, label_meta, group_var, group_col = "group")
  apa_report_path <- file.path(out_dir, "report_canonical.md")
  apa_table <- format_apa_table(summary_df, digits)
  apa_text <- format_apa_text(summary_df, digits)
  use_group_template <- !is.null(group_var)
  template_override <- resolve_template_override(opts$template, module = "frequencies")
  template_path <- if (!is.null(template_override)) {
    template_override
  } else if (use_group_template) {
    resolve_get_template_path("frequencies.grouped", "frequencies/grouped-template.md")
  } else {
    resolve_get_template_path("frequencies.default", "frequencies/default-template.md")
  }
  template_meta <- resolve_get_template_meta(template_path)
  table_result <- build_frequencies_table_body(summary_df, digits, template_meta$table)
  note_tokens <- build_frequencies_note_tokens(table_result$columns)
  narrative_rows <- build_frequencies_narrative_rows(summary_df, digits)
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
  analysis_flags <- list(
    vars = vars,
    group = if (!is.null(group_var) && group_var != "") group_var else "None",
    "include-numeric" = include_numeric,
    digits = digits
  )
  resolve_append_apa_report(
    apa_report_path,
    "Frequencies",
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
      module = "frequencies",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(summary_df = summary_df),
      options = list(digits = digits, vars = vars, group = group_var, include_numeric = include_numeric),
      user_prompt = resolve_get_user_prompt(opts)
    )
  }
}

main()
