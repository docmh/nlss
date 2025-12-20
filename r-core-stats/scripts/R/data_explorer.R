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
  cat("  Rscript data_explorer.R --interactive\n")
  cat("\n")
  cat("Options:\n")
  cat("  --csv PATH             CSV input file\n")
  cat("  --sav PATH             SPSS .sav input file\n")
  cat("  --sep VALUE            CSV separator (default: ,)\n")
  cat("  --header TRUE/FALSE    CSV header (default: TRUE)\n")
  cat("  --rds PATH             RDS input file (data frame)\n")
  cat("  --rdata PATH           RData input file\n")
  cat("  --df NAME              Data frame object name in RData\n")
  cat("  --vars LIST            Comma-separated variable names (default: all columns)\n")
  cat("  --digits N             Rounding digits (default: 2)\n")
  cat("  --max-levels N         Max unique levels to list before truncating (default: 20)\n")
  cat("  --top-n N              When truncating, show top N levels (default: 10)\n")
  cat("  --user-prompt TEXT     Original AI user prompt for logging (optional)\n")
  cat("  --log TRUE/FALSE       Write analysis_log.jsonl (default: TRUE)\n")
  cat("  --out DIR              Output directory (default: ./outputs/tmp)\n")
  cat("  --interactive          Prompt for inputs\n")
  cat("  --help                 Show this help\n")
}


interactive_options <- function() {
  cat("Interactive input selected.\n")
  input_type <- prompt("Input type (csv/sav/rds/rdata)", "csv")
  input_type <- tolower(input_type)
  opts <- list()

  if (input_type == "csv") {
    opts$csv <- prompt("CSV path")
    opts$sep <- prompt("Separator", ",")
    opts$header <- prompt("Header TRUE/FALSE", "TRUE")
  } else if (input_type == "sav") {
    opts$sav <- prompt("SAV path")
  } else if (input_type == "rds") {
    opts$rds <- prompt("RDS path")
  } else if (input_type == "rdata") {
    opts$rdata <- prompt("RData path")
    opts$df <- prompt("Data frame object name")
  } else {
    stop("Unsupported input type.")
  }

  opts$vars <- prompt("Variables (comma-separated, blank for all)", "")
  opts$digits <- prompt("Rounding digits", "2")
  opts$`max-levels` <- prompt("Max levels before truncating", "20")
  opts$`top-n` <- prompt("Top N levels when truncating", "10")
  opts$`user-prompt` <- prompt("User prompt (optional)", "")
  opts$log <- prompt("Write JSONL log TRUE/FALSE", "TRUE")
  opts$out <- prompt("Output directory", get_default_out())
  opts
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

  level_values <- get_levels(vec)
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

format_apa_overview_table <- function(df, digits) {
  display <- round_numeric(df, digits)
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
      format_percent(row$missing_pct, digits),
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

  display <- round_numeric(df, digits)
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
        format_percent(row$pct_total, digits),
        format_percent(row$pct_valid, digits)
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
        format_percent(missing_pct, digits),
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
      ifelse(is.na(missing_pct), "NA", format_percent(missing_pct, digits)),
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
              ifelse(is.na(lv$pct_valid), "NA", format_percent(lv$pct_valid, digits))
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
  opts <- parse_args(args)

  if (!is.null(opts$help)) {
    print_usage()
    quit(status = 0)
  }

  if (!is.null(opts$interactive)) {
    opts <- modifyList(opts, interactive_options())
  }

  digits <- if (!is.null(opts$digits)) as.numeric(opts$digits) else 2
  max_levels <- if (!is.null(opts$`max-levels`)) as.integer(opts$`max-levels`) else 20
  top_n <- if (!is.null(opts$`top-n`)) as.integer(opts$`top-n`) else 10
  out_dir <- ensure_out_dir(if (!is.null(opts$out)) opts$out else get_default_out())

  df <- load_dataframe(opts)
  vars <- select_variables(df, opts$vars, default = "all")
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
  append_apa_report(apa_report_path, "Data exploration", apa_tables, apa_text)

  cat("Wrote:\n")
  cat("- ", apa_report_path, "\n", sep = "")

  if (parse_bool(opts$log, default = TRUE)) {
    ctx <- get_run_context()
    append_analysis_log(
      out_dir,
      module = "data_explorer",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(overview_df = overview_df, levels_df = levels_df),
      options = list(digits = digits, vars = vars, max_levels = max_levels, top_n = top_n),
      user_prompt = get_user_prompt(opts)
    )
  }
}

main()
