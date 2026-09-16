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

  opts$vars <- prompt("Variables (comma-separated, blank for all non-numeric)", "")
  opts$group <- prompt("Grouping variable (blank for none)", "")
  include_numeric_default <- get_config_value("modules.frequencies.include_numeric", FALSE)
  opts$`include-numeric` <- prompt(
    "Include numeric columns when vars omitted TRUE/FALSE",
    ifelse(isTRUE(include_numeric_default), "TRUE", "FALSE")
  )
  digits_default <- get_config_value("defaults.digits", 2)
  opts$digits <- prompt("Rounding digits", as.character(digits_default))
  opts$template <- prompt("Template (path or key; blank for default)", "")
  opts$`user-prompt` <- prompt("User prompt (optional)", "")
  log_default <- get_config_value("defaults.log", TRUE)
  opts$log <- prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))
  opts
}

build_freq_rows <- function(vec, variable, group_label) {
  total_n <- length(vec)
  missing_n <- sum(is.na(vec))
  valid_n <- total_n - missing_n
  missing_pct <- ifelse(total_n > 0, missing_n / total_n * 100, NA_real_)

  levels <- get_levels(vec)
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

frequency_groups <- function(df, group_var, labels) {
  if (is.null(group_var)) {
    return(list(list(group = "", value = NULL, is_missing = FALSE,
      row_indices = seq_len(nrow(df)))))
  }
  group_vec <- df[[group_var]]
  values <- unique(group_vec)
  missing_label <- nlss_missing_group_label(group_vec, labels, group_var)
  lapply(seq_along(values), function(i) {
    value <- values[i]
    missing <- is.na(value)
    rows <- if (missing) which(is.na(group_vec)) else which(!is.na(group_vec) & group_vec == value)
    list(group = if (missing) missing_label else as.character(value),
      value = if (missing) NULL else as.character(value), is_missing = missing,
      row_indices = rows)
  })
}

build_summary <- function(df, vars, groups) {
  rows <- list()
  for (group in groups) {
    sub_df <- df[group$row_indices, , drop = FALSE]
    for (var in vars) {
      row <- build_freq_rows(sub_df[[var]], var, group$group)
      row$group_missing <- group$is_missing
      rows[[length(rows) + 1L]] <- row
    }
  }

  summary_df <- do.call(rbind, rows)
  numeric_cols <- setdiff(names(summary_df), c("variable", "group", "level", "group_missing"))
  for (col in numeric_cols) {
    summary_df[[col]] <- as.numeric(summary_df[[col]])
  }
  summary_df
}


format_nlss_table <- function(df, digits) {
  display <- round_numeric(df, digits)
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
        format_percent(row$pct_total, digits),
        format_percent(row$pct_valid, digits)
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
        format_percent(missing_pct, digits),
        ""
      )
      md <- paste0(md, "| ", paste(row_vals, collapse = " | "), " |\n")
    }
  }

  md <- paste0(md, "\nNote. % = percent of total; Valid % excludes missing values.\n")
  md
}

format_nlss_text <- function(df, digits) {
  display <- round_numeric(df, digits)
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
        ifelse(is.na(missing_pct), "NA", format_percent(missing_pct, digits))
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
          ifelse(is.na(row$pct_valid), "NA", format_percent(row$pct_valid, digits))
        )
      )
    }

    line <- sprintf(
      "%s (n = %s): %s. Missing = %s (%s%%).",
      label,
      as.character(total_n),
      paste(level_parts, collapse = "; "),
      ifelse(is.na(missing_n), "NA", as.character(missing_n)),
      ifelse(is.na(missing_pct), "NA", format_percent(missing_pct, digits))
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
  display <- round_numeric(summary_df, digits)
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
  columns <- normalize_table_columns(
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
            val <- as_cell_text(row$variable_display[1])
          } else if (key == "group") {
            val <- as_cell_text(row$group_display[1])
          } else {
            val <- as_cell_text(row$level_display[1])
          }
        } else if (key %in% c("n", "total_n", "missing_n")) {
          val <- ifelse(is.na(row[[key]][1]), "", as.character(row[[key]][1]))
        } else if (key %in% c("pct_total", "pct_valid", "missing_pct")) {
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

    if (!is.na(missing_n) && missing_n > 0) {
      row_vals <- character(0)
      for (col in columns) {
        key <- col$key
        val <- ""
        if (key == "variable") {
          val <- as_cell_text(var_label)
        } else if (key == "group") {
          val <- as_cell_text(grp_label)
        } else if (key == "level") {
          val <- "Missing"
        } else if (key == "n") {
          val <- as.character(missing_n)
        } else if (key == "pct_total") {
          val <- format_percent(missing_pct, digits)
        } else if (key == "pct_valid") {
          val <- ""
        } else if (key == "total_n") {
          val <- ifelse(is.na(total_n), "", as.character(total_n))
        } else if (key == "missing_n") {
          val <- ifelse(is.na(missing_n), "", as.character(missing_n))
        } else if (key == "missing_pct") {
          val <- format_percent(missing_pct, digits)
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
  display <- round_numeric(summary_df, digits)
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
    missing_pct_str <- ifelse(is.na(missing_pct), "NA", format_percent(missing_pct, digits))
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
            ifelse(is.na(row$pct_valid), "NA", format_percent(row$pct_valid, digits))
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
  opts <- nlss_run_options(args, "frequencies")

  if (!is.null(opts$help)) {
    print_usage()
    quit(status = 0)
  }

  if (parse_bool(opts$interactive, default = FALSE)) {
    opts <- modifyList(opts, interactive_options())
  }

  digits_default <- get_config_value("defaults.digits", 2)
  log_default <- get_config_value("defaults.log", TRUE)
  vars_default <- get_config_value("modules.frequencies.vars_default", "non-numeric")
  include_numeric_default <- get_config_value("modules.frequencies.include_numeric", FALSE)
  digits <- if (!is.null(opts$digits)) as.numeric(opts$digits) else digits_default
  df <- nlss_load_input(opts)
  out_dir <- get_workspace_out_dir(df)
  nlss_begin_run("frequencies", df, opts, out_dir)
  if (!is.finite(digits) || digits < 0 || digits > 15 || digits != floor(digits)) {
    stop("Digits must be an integer from 0 to 15.")
  }
  group_var <- if (!is.null(opts$group) && opts$group != "") opts$group else NULL
  if (!is.null(group_var) && !(group_var %in% names(df))) {
    stop("Grouping variable not found in data frame.")
  }

  include_numeric <- parse_bool(opts$`include-numeric`, default = include_numeric_default)
  vars <- select_variables(
    df,
    opts$vars,
    group_var,
    default = vars_default,
    include_numeric = include_numeric
  )
  if (length(vars) == 0) stop("No variables available for frequency analysis.")

  label_meta <- resolve_label_metadata(df)
  groups <- frequency_groups(df, group_var, label_meta)
  if (!length(groups)) stop("No observed groups available for frequency analysis.")
  nlss_resolve_request(list(digits = digits, vars = vars, group = group_var,
    include_numeric = include_numeric), design = list(
      missing = "variablewise; missing grouping values form a separate group",
      rows = nrow(df), groups = groups,
      variable_types = lapply(df[unique(c(vars, group_var))], class),
      variable_levels = lapply(df[unique(c(vars, group_var))], get_levels),
      missing_counts = lapply(df[unique(c(vars, group_var))], function(x) sum(is.na(x)))))

  summary_df <- build_summary(df, vars, groups)
  summary_df <- add_variable_label_column(summary_df, label_meta, var_col = "variable")
  summary_df <- add_value_label_column(summary_df, label_meta, var_col = "variable", value_col = "level")
  summary_df <- add_group_label_column(summary_df, label_meta, group_var, group_col = "group")
  if (!is.null(group_var)) {
    summary_df$group_label[summary_df$group_missing] <- summary_df$group[summary_df$group_missing]
  }
  nlss_report_path <- file.path(out_dir, "report_canonical.md")
  nlss_table <- format_nlss_table(summary_df, digits)
  nlss_text <- format_nlss_text(summary_df, digits)
  use_group_template <- !is.null(group_var)
  template_override <- resolve_template_override(opts$template, module = "frequencies")
  template_path <- if (!is.null(template_override)) {
    template_override
  } else if (use_group_template) {
    resolve_template_path("frequencies.grouped", "frequencies/grouped-template.md")
  } else {
    resolve_template_path("frequencies.default", "frequencies/default-template.md")
  }
  template_path <- nlss_freeze_template(template_path, "frequencies.main")
  template_meta <- get_template_meta(template_path)
  table_result <- build_frequencies_table_body(summary_df, digits, template_meta$table)
  note_tokens <- build_frequencies_note_tokens(table_result$columns)
  narrative_rows <- build_frequencies_narrative_rows(summary_df, digits)
  template_context <- list(
    tokens = c(
      list(
        table_body = table_result$body,
        narrative_default = nlss_text
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
  nlss_stage_report(
    nlss_report_path,
    "Frequencies",
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
      module = "frequencies",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(summary_df = summary_df),
      options = list(digits = digits, vars = vars, group = group_var, include_numeric = include_numeric),
      user_prompt = get_user_prompt(opts)
    )
  }
}

nlss_run_main("frequencies", main)
