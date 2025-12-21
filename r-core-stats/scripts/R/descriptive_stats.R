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
  cat("Descriptive statistics (base R)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript descriptive_stats.R --csv data.csv [--vars var1,var2] [--group group_var]\n")
  cat("  Rscript descriptive_stats.R --sav data.sav [--vars var1,var2] [--group group_var]\n")
  cat("  Rscript descriptive_stats.R --rds data.rds [--vars var1,var2] [--group group_var]\n")
  cat("  Rscript descriptive_stats.R --rdata data.RData --df data_frame_name [--vars var1,var2] [--group group_var]\n")
  cat("  Rscript descriptive_stats.R --interactive\n")
  cat("\n")
  cat("Options:\n")
  cat("  --csv PATH           CSV input file\n")
  cat("  --sav PATH           SPSS .sav input file\n")
  cat("  --sep VALUE          CSV separator (default: ,)\n")
  cat("  --header TRUE/FALSE  CSV header (default: TRUE)\n")
  cat("  --rds PATH           RDS input file (data frame)\n")
  cat("  --rdata PATH         RData input file\n")
  cat("  --df NAME            Data frame object name in RData\n")
  cat("  --vars LIST          Comma-separated variable names (default: all numeric)\n")
  cat("  --group NAME         Grouping variable name (optional)\n")
  cat("  --digits N           Rounding digits (default: 2)\n")
  cat("  --user-prompt TEXT   Original AI user prompt for logging (optional)\n")
  cat("  --log TRUE/FALSE     Write analysis_log.jsonl (default: TRUE)\n")
  cat("  --out DIR            Output directory (default: ./outputs/tmp)\n")
  cat("  --interactive        Prompt for inputs\n")
  cat("  --help               Show this help\n")
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

  opts$vars <- resolve_prompt("Variables (comma-separated, blank for all numeric)", "")
  opts$group <- resolve_prompt("Grouping variable (blank for none)", "")
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


calc_skewness <- function(x, mean_x, sd_x) {
  n <- length(x)
  if (n < 3 || is.na(sd_x) || sd_x == 0) return(NA_real_)
  sum(((x - mean_x) / sd_x)^3) * (n / ((n - 1) * (n - 2)))
}

calc_kurtosis <- function(x, mean_x, sd_x) {
  n <- length(x)
  if (n < 4 || is.na(sd_x) || sd_x == 0) return(NA_real_)
  term1 <- (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3))
  term2 <- (3 * (n - 1)^2) / ((n - 2) * (n - 3))
  term1 * sum(((x - mean_x) / sd_x)^4) - term2
}

summarize_vector <- function(vec, total_n, digits) {
  missing_n <- sum(is.na(vec))
  x <- vec[!is.na(vec)]
  n <- length(x)

  if (n == 0) {
    return(list(
      n = 0,
      missing_n = missing_n,
      missing_pct = ifelse(total_n > 0, missing_n / total_n * 100, NA_real_),
      mean = NA_real_,
      sd = NA_real_,
      median = NA_real_,
      min = NA_real_,
      max = NA_real_,
      se = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      skewness = NA_real_,
      kurtosis = NA_real_
    ))
  }

  mean_x <- mean(x)
  sd_x <- if (n > 1) sd(x) else NA_real_
  median_x <- median(x)
  min_x <- min(x)
  max_x <- max(x)
  se_x <- if (!is.na(sd_x)) sd_x / sqrt(n) else NA_real_
  if (!is.na(se_x) && n > 1) {
    tcrit <- qt(0.975, df = n - 1)
    ci_low <- mean_x - tcrit * se_x
    ci_high <- mean_x + tcrit * se_x
  } else {
    ci_low <- NA_real_
    ci_high <- NA_real_
  }

  skew_x <- calc_skewness(x, mean_x, sd_x)
  kurt_x <- calc_kurtosis(x, mean_x, sd_x)

  list(
    n = n,
    missing_n = missing_n,
    missing_pct = ifelse(total_n > 0, missing_n / total_n * 100, NA_real_),
    mean = mean_x,
    sd = sd_x,
    median = median_x,
    min = min_x,
    max = max_x,
    se = se_x,
    ci_low = ci_low,
    ci_high = ci_high,
    skewness = skew_x,
    kurtosis = kurt_x
  )
}

build_summary <- function(df, vars, group_var = NULL, digits = 2) {
  rows <- list()
  if (!is.null(group_var) && group_var != "") {
    group_vec <- df[[group_var]]
    group_levels <- unique(group_vec)
    for (g in group_levels) {
      idx <- if (is.na(g)) is.na(group_vec) else group_vec == g
      sub_df <- df[idx, , drop = FALSE]
      for (var in vars) {
        stats <- summarize_vector(sub_df[[var]], length(sub_df[[var]]), digits)
        row <- c(
          list(
            variable = var,
            group = ifelse(is.na(g), "NA", as.character(g)),
            total_n = length(sub_df[[var]])
          ),
          stats
        )
        rows[[length(rows) + 1]] <- row
      }
    }
  } else {
    for (var in vars) {
      stats <- summarize_vector(df[[var]], length(df[[var]]), digits)
      row <- c(
        list(
          variable = var,
          group = "",
          total_n = length(df[[var]])
        ),
        stats
      )
      rows[[length(rows) + 1]] <- row
    }
  }

  summary_df <- do.call(rbind, lapply(rows, function(row) as.data.frame(row, stringsAsFactors = FALSE)))
  numeric_cols <- setdiff(names(summary_df), c("variable", "group"))
  for (col in numeric_cols) {
    summary_df[[col]] <- as.numeric(summary_df[[col]])
  }
  summary_df
}


format_apa_table <- function(df, digits) {
  display <- df
  display <- resolve_round_numeric(display, digits)
  if (all(display$group == "")) {
    display$group <- NULL
  }
  table_cols <- c("variable", "group", "n", "mean", "sd", "min", "max")
  table_cols <- table_cols[table_cols %in% names(display)]
  display <- display[, table_cols, drop = FALSE]
  names(display) <- c("Variable", if ("group" %in% names(display)) "Group" else NULL,
                      "n", "M", "SD", "Min", "Max")
  header <- "Table 1\nDescriptive statistics\n"
  md <- paste0(header, "\n| ", paste(names(display), collapse = " | "), " |\n")
  md <- paste0(md, "| ", paste(rep("---", ncol(display)), collapse = " | "), " |\n")
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    row_vals <- vapply(row, function(x) ifelse(is.na(x), "", as.character(x)), character(1))
    md <- paste0(md, "| ", paste(row_vals, collapse = " | "), " |\n")
  }
  md <- paste0(md, "\nNote. M = mean; SD = standard deviation. Missing values excluded per variable.\n")
  md
}

format_apa_text <- function(df, digits) {
  display <- resolve_round_numeric(df, digits)
  lines <- character(0)
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    label <- row$variable
    if (row$group != "") {
      label <- paste0("Group ", row$group, ", ", label)
    }
    missing_pct_str <- ifelse(
      is.na(row$missing_pct),
      "NA",
      format(row$missing_pct, nsmall = 1, trim = TRUE)
    )
    line <- sprintf(
      "%s: M = %s, SD = %s, 95%% CI [%s, %s], n = %s, missing = %s (%s%%).",
      label,
      ifelse(is.na(row$mean), "NA", format(row$mean, nsmall = digits, trim = TRUE)),
      ifelse(is.na(row$sd), "NA", format(row$sd, nsmall = digits, trim = TRUE)),
      ifelse(is.na(row$ci_low), "NA", format(row$ci_low, nsmall = digits, trim = TRUE)),
      ifelse(is.na(row$ci_high), "NA", format(row$ci_high, nsmall = digits, trim = TRUE)),
      ifelse(is.na(row$n), "NA", as.character(row$n)),
      ifelse(is.na(row$missing_n), "NA", as.character(row$missing_n)),
      missing_pct_str
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
  vars_default <- resolve_config_value("modules.descriptive_stats.vars_default", "numeric")
  digits <- if (!is.null(opts$digits)) as.numeric(opts$digits) else digits_default
  out_dir <- resolve_ensure_out_dir(if (!is.null(opts$out)) opts$out else resolve_default_out())

  df <- resolve_load_dataframe(opts)
  group_var <- if (!is.null(opts$group) && opts$group != "") opts$group else NULL
  if (!is.null(group_var) && !(group_var %in% names(df))) {
    stop("Grouping variable not found in data frame.")
  }

  vars <- resolve_select_variables(df, opts$vars, group_var, default = vars_default)
  if (length(vars) == 0) stop("No numeric variables available for analysis.")

  summary_df <- build_summary(df, vars, group_var, digits)
  apa_report_path <- file.path(out_dir, "apa_report.md")
  apa_table <- format_apa_table(summary_df, digits)
  apa_text <- format_apa_text(summary_df, digits)
  analysis_flags <- list(
    vars = vars,
    group = if (!is.null(group_var) && group_var != "") group_var else "None",
    digits = digits
  )
  resolve_append_apa_report(apa_report_path, "Descriptive statistics", apa_table, apa_text, analysis_flags = analysis_flags)

  cat("Wrote:\n")
  cat("- ", apa_report_path, "\n", sep = "")

  if (resolve_parse_bool(opts$log, default = log_default)) {
    ctx <- resolve_get_run_context()
    resolve_append_analysis_log(
      out_dir,
      module = "descriptive_stats",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(summary_df = summary_df),
      options = list(digits = digits, vars = vars, group = group_var),
      user_prompt = resolve_get_user_prompt(opts)
    )
  }
}

main()
