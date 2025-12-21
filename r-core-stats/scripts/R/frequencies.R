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
  cat("  Rscript frequencies.R --interactive\n")
  cat("\n")
  cat("Options:\n")
  cat("  --csv PATH             CSV input file\n")
  cat("  --sav PATH             SPSS .sav input file\n")
  cat("  --sep VALUE            CSV separator (default: ,)\n")
  cat("  --header TRUE/FALSE    CSV header (default: TRUE)\n")
  cat("  --rds PATH             RDS input file (data frame)\n")
  cat("  --rdata PATH           RData input file\n")
  cat("  --df NAME              Data frame object name in RData\n")
  cat("  --vars LIST            Comma-separated variable names (default: all non-numeric)\n")
  cat("  --group NAME           Grouping variable name (optional)\n")
  cat("  --include-numeric      Include numeric columns when --vars is omitted\n")
  cat("  --digits N             Rounding digits for percentages (default: 2)\n")
  cat("  --user-prompt TEXT     Original AI user prompt for logging (optional)\n")
  cat("  --log TRUE/FALSE       Write analysis_log.jsonl (default: TRUE)\n")
  cat("  --out DIR              Output directory (default: ./outputs/tmp)\n")
  cat("  --interactive          Prompt for inputs\n")
  cat("  --help                 Show this help\n")
}


interactive_options <- function() {
  cat("Interactive input selected.\n")
  input_type <- resolve_prompt("Input type (csv/sav/rds/rdata)", "csv")
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
  opts$`user-prompt` <- resolve_prompt("User prompt (optional)", "")
  log_default <- resolve_config_value("defaults.log", TRUE)
  opts$log <- resolve_prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))
  opts$out <- resolve_prompt("Output directory", resolve_default_out())
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

resolve_append_apa_report <- function(path, analysis_label, apa_table, apa_text, analysis_flags = NULL, template_path = NULL) {
  if (exists("append_apa_report", mode = "function")) {
    return(get("append_apa_report", mode = "function")(
      path,
      analysis_label,
      apa_table,
      apa_text,
      analysis_flags = analysis_flags,
      template_path = template_path
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
  data.frame(
    variable = variable,
    group = group_label,
    level = names(counts),
    n = as.integer(counts),
    pct_total = ifelse(total_n > 0, as.numeric(counts) / total_n * 100, NA_real_),
    pct_valid = ifelse(valid_n > 0, as.numeric(counts) / valid_n * 100, NA_real_),
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
  use_group <- !all(display$group == "")

  headers <- c("Variable", if (use_group) "Group", "Level", "n", "%", "Valid %")
  md <- paste0("Table 1\nFrequencies\n\n| ", paste(headers, collapse = " | "), " |\n")
  md <- paste0(md, "| ", paste(rep("---", length(headers)), collapse = " | "), " |\n")

  combo_df <- unique(display[, c("variable", "group")])
  for (idx in seq_len(nrow(combo_df))) {
    var <- combo_df$variable[idx]
    grp <- combo_df$group[idx]
    subset <- display[display$variable == var & display$group == grp, , drop = FALSE]
    for (i in seq_len(nrow(subset))) {
      row <- subset[i, ]
      row_vals <- c(
        var,
        if (use_group) grp,
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
        if (use_group) grp,
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
  lines <- character(0)
  combo_df <- unique(display[, c("variable", "group")])
  for (idx in seq_len(nrow(combo_df))) {
    var <- combo_df$variable[idx]
    grp <- combo_df$group[idx]
    subset <- display[display$variable == var & display$group == grp, , drop = FALSE]
    total_n <- subset$total_n[1]
    missing_n <- subset$missing_n[1]
    missing_pct <- subset$missing_pct[1]
    label <- if (is.na(grp) || grp == "") {
      var
    } else {
      paste0("Group ", grp, ", ", var)
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
          row$level,
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
  out_dir <- resolve_ensure_out_dir(if (!is.null(opts$out)) opts$out else resolve_default_out())

  df <- resolve_load_dataframe(opts)
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
  apa_report_path <- file.path(out_dir, "apa_report.md")
  apa_table <- format_apa_table(summary_df, digits)
  apa_text <- format_apa_text(summary_df, digits)
  resolve_append_apa_report(apa_report_path, "Frequencies", apa_table, apa_text)

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
