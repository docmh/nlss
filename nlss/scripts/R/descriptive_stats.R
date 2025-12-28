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
add_group_label_column <- get("add_group_label_column", mode = "function")
add_variable_label_column <- get("add_variable_label_column", mode = "function")
resolve_label_metadata <- get("resolve_label_metadata", mode = "function")
source_lib <- get("source_lib", mode = "function")

print_usage <- function() {
  cat("Descriptive statistics (base R)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript descriptive_stats.R --csv data.csv [--vars var1,var2] [--group group_var]\n")
  cat("  Rscript descriptive_stats.R --sav data.sav [--vars var1,var2] [--group group_var]\n")
  cat("  Rscript descriptive_stats.R --rds data.rds [--vars var1,var2] [--group group_var]\n")
  cat("  Rscript descriptive_stats.R --rdata data.RData --df data_frame_name [--vars var1,var2] [--group group_var]\n")
  cat("  Rscript descriptive_stats.R --parquet data.parquet [--vars var1,var2] [--group group_var]\n")
  cat("  Rscript descriptive_stats.R --interactive\n")
  cat("\n")
  cat("Options:\n")
  cat("  --csv PATH           CSV input file\n")
  cat("  --sav PATH           SPSS .sav input file\n")
  cat("  --sep VALUE          CSV separator (default: ,)\n")
  cat("  --header TRUE/FALSE  CSV header (default: TRUE)\n")
  cat("  --rds PATH           RDS input file (data frame)\n")
  cat("  --rdata PATH         RData input file\n")
  cat("  --parquet PATH       Parquet input file\n")
  cat("  --df NAME            Data frame object name in RData\n")
  cat("  --vars LIST          Comma-separated variable names (default: all numeric)\n")
  cat("  --group NAME         Grouping variable name (optional)\n")
  cat("  --digits N           Rounding digits (default: 2)\n")
  cat("  --trim VALUE         Trim proportion for trimmed mean (default: 0.1)\n")
  cat("  --iqr-multiplier N   IQR multiplier for Tukey outliers (default: 1.5)\n")
  cat("  --outlier-z N        Z threshold for outlier counts (default: 3)\n")
  cat("  --template REF       Template path or template key (optional)\n")
  cat("  --user-prompt TEXT   Original AI user prompt for logging (optional)\n")
  cat("  --log TRUE/FALSE     Write analysis_log.jsonl (default: TRUE)\n")
  cat("  --interactive        Prompt for inputs\n")
  cat("  --help               Show this help\n")
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

  opts$vars <- resolve_prompt("Variables (comma-separated, blank for all numeric)", "")
  opts$group <- resolve_prompt("Grouping variable (blank for none)", "")
  digits_default <- resolve_config_value("defaults.digits", 2)
  opts$digits <- resolve_prompt("Rounding digits", as.character(digits_default))
  trim_default <- resolve_config_value("modules.descriptive_stats.trim", 0.1)
  opts$trim <- resolve_prompt("Trim proportion (0-0.5)", as.character(trim_default))
  iqr_default <- resolve_config_value("modules.descriptive_stats.iqr_multiplier", 1.5)
  opts$`iqr-multiplier` <- resolve_prompt("IQR multiplier", as.character(iqr_default))
  outlier_default <- resolve_config_value("modules.descriptive_stats.outlier_z", 3)
  opts$`outlier-z` <- resolve_prompt("Outlier z threshold", as.character(outlier_default))
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

calc_mode <- function(x, min_count = 2) {
  if (length(x) == 0) return(NA_real_)
  tab <- table(x, useNA = "no")
  if (length(tab) == 0) return(NA_real_)
  max_count <- max(tab)
  if (max_count < min_count) return(NA_real_)
  modes <- as.numeric(names(tab)[tab == max_count])
  if (length(modes) != 1) return(NA_real_)
  modes[1]
}

summarize_vector <- function(vec, total_n, digits, trim = 0.1, iqr_multiplier = 1.5, outlier_z = 3) {
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
      variance = NA_real_,
      range = NA_real_,
      q1 = NA_real_,
      q3 = NA_real_,
      iqr = NA_real_,
      mad = NA_real_,
      cv = NA_real_,
      trimmed_mean = NA_real_,
      p5 = NA_real_,
      p10 = NA_real_,
      p90 = NA_real_,
      p95 = NA_real_,
      outliers_tukey = NA_real_,
      outliers_z = NA_real_,
      mode = NA_real_,
      n_unique = NA_real_,
      se = NA_real_,
      ci_low = NA_real_,
      ci_high = NA_real_,
      skewness = NA_real_,
      kurtosis = NA_real_
    ))
  }

  mean_x <- mean(x)
  sd_x <- if (n > 1) sd(x) else NA_real_
  variance_x <- if (n > 1) var(x) else NA_real_
  median_x <- median(x)
  min_x <- min(x)
  max_x <- max(x)
  range_x <- max_x - min_x
  q <- quantile(x, probs = c(0.05, 0.1, 0.25, 0.75, 0.9, 0.95), names = FALSE, type = 7)
  p5_x <- q[1]
  p10_x <- q[2]
  q1_x <- q[3]
  q3_x <- q[4]
  p90_x <- q[5]
  p95_x <- q[6]
  iqr_x <- q3_x - q1_x
  mad_x <- mad(x, constant = 1.4826, na.rm = TRUE)
  trim_x <- mean(x, trim = trim)
  cv_x <- if (!is.na(sd_x) && !is.na(mean_x) && mean_x != 0) sd_x / abs(mean_x) else NA_real_
  outlier_tukey_count <- if (!is.na(iqr_x)) {
    lower <- q1_x - iqr_multiplier * iqr_x
    upper <- q3_x + iqr_multiplier * iqr_x
    sum(x < lower | x > upper)
  } else {
    NA_real_
  }
  outlier_z_count <- if (!is.na(sd_x) && sd_x > 0) {
    sum(abs((x - mean_x) / sd_x) > outlier_z)
  } else {
    NA_real_
  }
  mode_x <- calc_mode(x)
  n_unique <- length(unique(x))
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
    variance = variance_x,
    range = range_x,
    q1 = q1_x,
    q3 = q3_x,
    iqr = iqr_x,
    mad = mad_x,
    cv = cv_x,
    trimmed_mean = trim_x,
    p5 = p5_x,
    p10 = p10_x,
    p90 = p90_x,
    p95 = p95_x,
    outliers_tukey = outlier_tukey_count,
    outliers_z = outlier_z_count,
    mode = mode_x,
    n_unique = n_unique,
    se = se_x,
    ci_low = ci_low,
    ci_high = ci_high,
    skewness = skew_x,
    kurtosis = kurt_x
  )
}

build_summary <- function(df, vars, group_var = NULL, digits = 2, trim = 0.1, iqr_multiplier = 1.5, outlier_z = 3) {
  rows <- list()
  if (!is.null(group_var) && group_var != "") {
    group_vec <- df[[group_var]]
    group_levels <- unique(group_vec)
    for (g in group_levels) {
      idx <- if (is.na(g)) is.na(group_vec) else group_vec == g
      sub_df <- df[idx, , drop = FALSE]
      for (var in vars) {
        stats <- summarize_vector(
          sub_df[[var]],
          length(sub_df[[var]]),
          digits,
          trim = trim,
          iqr_multiplier = iqr_multiplier,
          outlier_z = outlier_z
        )
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
      stats <- summarize_vector(
        df[[var]],
        length(df[[var]]),
        digits,
        trim = trim,
        iqr_multiplier = iqr_multiplier,
        outlier_z = outlier_z
      )
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
  display$variable_display <- if ("variable_label" %in% names(display)) display$variable_label else display$variable
  display$group_display <- if ("group_label" %in% names(display)) display$group_label else display$group
  if (all(display$group == "")) {
    display$group <- NULL
    display$group_display <- NULL
  }
  table_cols <- c("variable_display", "group_display", "n", "mean", "sd", "min", "max")
  table_cols <- table_cols[table_cols %in% names(display)]
  display <- display[, table_cols, drop = FALSE]
  names(display) <- c("Variable", if ("group_display" %in% names(display)) "Group" else NULL,
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
  display$variable_display <- if ("variable_label" %in% names(display)) display$variable_label else display$variable
  display$group_display <- if ("group_label" %in% names(display)) display$group_label else display$group
  lines <- character(0)
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    label <- row$variable_display
    if (row$group != "") {
      label <- paste0("Group ", row$group_display, ", ", label)
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

format_num <- function(value, digits) {
  if (is.na(value)) return("")
  format(round(value, digits), nsmall = digits, trim = TRUE)
}

build_descriptive_table_body <- function(df, digits, table_spec = NULL) {
  display <- resolve_round_numeric(df, digits)
  display$group <- as.character(display$group)
  display$group[is.na(display$group)] <- "NA"
  display$variable_display <- if ("variable_label" %in% names(display)) display$variable_label else display$variable
  display$group_display <- if ("group_label" %in% names(display)) display$group_label else display$group

  default_columns <- list(
    list(key = "variable", label = "Variable"),
    list(key = "group", label = "Group", drop_if_empty = TRUE),
    list(key = "n", label = "n"),
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
        if (key %in% c("variable", "group")) {
          if (key == "variable") {
            val <- resolve_as_cell_text(row$variable_display[1])
          } else {
            val <- resolve_as_cell_text(row$group_display[1])
          }
        } else if (key %in% c("n", "missing_n", "total_n", "outliers_tukey", "outliers_z", "n_unique")) {
          val <- ifelse(is.na(cell), "", as.character(cell))
        } else if (key == "missing_pct") {
          val <- format_num(cell, digits)
        } else if (is.numeric(cell)) {
          val <- format_num(cell, digits)
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
  resolve_render_markdown_table(headers, rows)
}

build_descriptive_narrative_rows <- function(df, digits) {
  display <- resolve_round_numeric(df, digits)
  display$group <- as.character(display$group)
  display$group[is.na(display$group)] <- "NA"
  display$variable_display <- if ("variable_label" %in% names(display)) display$variable_label else display$variable
  display$group_display <- if ("group_label" %in% names(display)) display$group_label else display$group
  rows <- list()
  for (i in seq_len(nrow(display))) {
    row <- display[i, , drop = FALSE]
    label <- row$variable_display
    if (row$group != "") {
      label <- paste0("Group ", row$group_display, ", ", label)
    }
    missing_pct_str <- ifelse(
      is.na(row$missing_pct),
      "NA",
      format(row$missing_pct, nsmall = 1, trim = TRUE)
    )
    mean_str <- ifelse(is.na(row$mean), "NA", format(row$mean, nsmall = digits, trim = TRUE))
    sd_str <- ifelse(is.na(row$sd), "NA", format(row$sd, nsmall = digits, trim = TRUE))
    ci_low_str <- ifelse(is.na(row$ci_low), "NA", format(row$ci_low, nsmall = digits, trim = TRUE))
    ci_high_str <- ifelse(is.na(row$ci_high), "NA", format(row$ci_high, nsmall = digits, trim = TRUE))
    n_str <- ifelse(is.na(row$n), "NA", as.character(row$n))
    missing_n_str <- ifelse(is.na(row$missing_n), "NA", as.character(row$missing_n))

    line <- sprintf(
      "%s: M = %s, SD = %s, 95%% CI [%s, %s], n = %s, missing = %s (%s%%).",
      label,
      mean_str,
      sd_str,
      ci_low_str,
      ci_high_str,
      n_str,
      missing_n_str,
      missing_pct_str
    )

    rows[[length(rows) + 1]] <- list(
      label = label,
      variable = resolve_as_cell_text(row$variable_display),
      group = resolve_as_cell_text(row$group_display),
      n = n_str,
      missing_n = missing_n_str,
      missing_pct = missing_pct_str,
      mean = mean_str,
      sd = sd_str,
      se = ifelse(is.na(row$se), "NA", format(row$se, nsmall = digits, trim = TRUE)),
      median = ifelse(is.na(row$median), "NA", format(row$median, nsmall = digits, trim = TRUE)),
      min = ifelse(is.na(row$min), "NA", format(row$min, nsmall = digits, trim = TRUE)),
      max = ifelse(is.na(row$max), "NA", format(row$max, nsmall = digits, trim = TRUE)),
      variance = ifelse(is.na(row$variance), "NA", format(row$variance, nsmall = digits, trim = TRUE)),
      range = ifelse(is.na(row$range), "NA", format(row$range, nsmall = digits, trim = TRUE)),
      q1 = ifelse(is.na(row$q1), "NA", format(row$q1, nsmall = digits, trim = TRUE)),
      q3 = ifelse(is.na(row$q3), "NA", format(row$q3, nsmall = digits, trim = TRUE)),
      iqr = ifelse(is.na(row$iqr), "NA", format(row$iqr, nsmall = digits, trim = TRUE)),
      mad = ifelse(is.na(row$mad), "NA", format(row$mad, nsmall = digits, trim = TRUE)),
      cv = ifelse(is.na(row$cv), "NA", format(row$cv, nsmall = digits, trim = TRUE)),
      trimmed_mean = ifelse(is.na(row$trimmed_mean), "NA", format(row$trimmed_mean, nsmall = digits, trim = TRUE)),
      p5 = ifelse(is.na(row$p5), "NA", format(row$p5, nsmall = digits, trim = TRUE)),
      p10 = ifelse(is.na(row$p10), "NA", format(row$p10, nsmall = digits, trim = TRUE)),
      p90 = ifelse(is.na(row$p90), "NA", format(row$p90, nsmall = digits, trim = TRUE)),
      p95 = ifelse(is.na(row$p95), "NA", format(row$p95, nsmall = digits, trim = TRUE)),
      outliers_tukey = ifelse(is.na(row$outliers_tukey), "NA", as.character(row$outliers_tukey)),
      outliers_z = ifelse(is.na(row$outliers_z), "NA", as.character(row$outliers_z)),
      mode = ifelse(is.na(row$mode), "NA", format(row$mode, nsmall = digits, trim = TRUE)),
      n_unique = ifelse(is.na(row$n_unique), "NA", as.character(row$n_unique)),
      ci_low = ci_low_str,
      ci_high = ci_high_str,
      full_sentence = line
    )
  }
  rows
}

build_descriptive_note_tokens <- function(trim = 0.1, iqr_multiplier = 1.5, outlier_z = 3) {
  abbrev_note <- "M = mean; SD = standard deviation."
  missing_note <- "Missing values excluded per variable."
  trim_pct <- format(round(trim * 100, 1), nsmall = 1, trim = TRUE)
  trim_note <- paste0("Trimmed mean uses ", trim_pct, "% trimming.")
  robust_note <- "Median (Mdn), IQR, and MAD summarize robust location and spread."
  percentile_note <- "Percentiles reported: 5th, 10th, 90th, 95th."
  outlier_note <- paste0("Outliers flagged by Tukey fences (", format(iqr_multiplier, trim = TRUE), "xIQR) and |z| > ", format(outlier_z, trim = TRUE), ".")
  cv_note <- "CV = SD / |M|."
  mode_note <- "Mode reported only when a single most frequent value exists."
  list(
    note_abbrev = abbrev_note,
    missing_note = missing_note,
    trim_note = trim_note,
    robust_note = robust_note,
    percentile_note = percentile_note,
    outlier_note = outlier_note,
    cv_note = cv_note,
    mode_note = mode_note,
    note_default = paste(abbrev_note, missing_note)
  )
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
  trim_default <- resolve_config_value("modules.descriptive_stats.trim", 0.1)
  iqr_default <- resolve_config_value("modules.descriptive_stats.iqr_multiplier", 1.5)
  outlier_default <- resolve_config_value("modules.descriptive_stats.outlier_z", 3)
  digits <- if (!is.null(opts$digits)) as.numeric(opts$digits) else digits_default
  trim <- if (!is.null(opts$trim)) as.numeric(opts$trim) else trim_default
  iqr_multiplier <- if (!is.null(opts$`iqr-multiplier`)) as.numeric(opts$`iqr-multiplier`) else iqr_default
  outlier_z <- if (!is.null(opts$`outlier-z`)) as.numeric(opts$`outlier-z`) else outlier_default
  if (is.na(trim) || trim < 0 || trim >= 0.5) {
    stop("Trim must be between 0 (inclusive) and 0.5 (exclusive).")
  }
  if (is.na(iqr_multiplier) || iqr_multiplier <= 0) {
    stop("IQR multiplier must be greater than 0.")
  }
  if (is.na(outlier_z) || outlier_z <= 0) {
    stop("Outlier z threshold must be greater than 0.")
  }
  df <- resolve_load_dataframe(opts)
  out_dir <- resolve_get_workspace_out_dir(df)
  group_var <- if (!is.null(opts$group) && opts$group != "") opts$group else NULL
  if (!is.null(group_var) && !(group_var %in% names(df))) {
    stop("Grouping variable not found in data frame.")
  }

  vars <- resolve_select_variables(df, opts$vars, group_var, default = vars_default)
  if (length(vars) == 0) stop("No numeric variables available for analysis.")

  summary_df <- build_summary(df, vars, group_var, digits, trim = trim, iqr_multiplier = iqr_multiplier, outlier_z = outlier_z)
  label_meta <- resolve_label_metadata(df)
  summary_df <- add_variable_label_column(summary_df, label_meta, var_col = "variable")
  summary_df <- add_group_label_column(summary_df, label_meta, group_var, group_col = "group")
  apa_report_path <- file.path(out_dir, "report_canonical.md")
  apa_table <- format_apa_table(summary_df, digits)
  apa_text <- format_apa_text(summary_df, digits)
  template_override <- resolve_template_override(opts$template, module = "descriptive_stats")
  template_path <- if (!is.null(template_override)) {
    template_override
  } else {
    resolve_get_template_path("descriptive_stats.default", "descriptive-stats/default-template.md")
  }
  template_meta <- resolve_get_template_meta(template_path)
  table_body <- build_descriptive_table_body(summary_df, digits, template_meta$table)
  note_tokens <- build_descriptive_note_tokens(trim = trim, iqr_multiplier = iqr_multiplier, outlier_z = outlier_z)
  narrative_rows <- build_descriptive_narrative_rows(summary_df, digits)
  template_context <- list(
    tokens = c(
      list(
        table_body = table_body,
        narrative_default = apa_text,
        trim = trim,
        trim_pct = format(round(trim * 100, 1), nsmall = 1, trim = TRUE),
        iqr_multiplier = iqr_multiplier,
        outlier_z = outlier_z
      ),
      note_tokens
    ),
    narrative_rows = narrative_rows
  )
  analysis_flags <- list(
    vars = vars,
    group = if (!is.null(group_var) && group_var != "") group_var else "None",
    digits = digits,
    trim = trim,
    `iqr-multiplier` = iqr_multiplier,
    `outlier-z` = outlier_z
  )
  resolve_append_apa_report(
    apa_report_path,
    "Descriptive statistics",
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
      module = "descriptive_stats",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(summary_df = summary_df),
      options = list(digits = digits, vars = vars, group = group_var, trim = trim, iqr_multiplier = iqr_multiplier, outlier_z = outlier_z),
      user_prompt = resolve_get_user_prompt(opts)
    )
  }
}

main()
