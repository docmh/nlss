#!/usr/bin/env Rscript

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
  cat("  --out DIR            Output directory (default: ./outputs/tmp)\n")
  cat("  --interactive        Prompt for inputs\n")
  cat("  --help               Show this help\n")
}

parse_args <- function(args) {
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

parse_bool <- function(value, default = FALSE) {
  if (is.null(value)) return(default)
  if (is.logical(value)) return(value)
  val <- tolower(as.character(value))
  val %in% c("true", "t", "1", "yes", "y")
}

prompt <- function(label, default = NULL) {
  if (is.null(default)) {
    answer <- readline(paste0(label, ": "))
  } else {
    answer <- readline(paste0(label, " [", default, "]: "))
    if (answer == "") answer <- default
  }
  answer
}

get_default_out <- function() {
  "./outputs/tmp"
}

read_sav_data <- function(path) {
  if (requireNamespace("haven", quietly = TRUE)) {
    df <- haven::read_sav(path)
    return(as.data.frame(df, stringsAsFactors = FALSE))
  }
  if (requireNamespace("foreign", quietly = TRUE)) {
    df <- suppressWarnings(foreign::read.spss(path, to.data.frame = TRUE, use.value.labels = FALSE))
    if (!is.data.frame(df)) df <- as.data.frame(df, stringsAsFactors = FALSE)
    return(df)
  }
  stop("SPSS .sav support requires the 'haven' or 'foreign' package. Install one: install.packages('haven').")
}

load_dataframe <- function(opts) {
  if (!is.null(opts$csv)) {
    sep <- if (!is.null(opts$sep)) opts$sep else ","
    header <- parse_bool(opts$header, default = TRUE)
    df <- read.csv(opts$csv, sep = sep, header = header, stringsAsFactors = FALSE)
    return(df)
  }

  if (!is.null(opts$sav)) {
    df <- read_sav_data(opts$sav)
    if (!is.data.frame(df)) stop("SAV does not contain a data frame.")
    return(df)
  }

  if (!is.null(opts$rds)) {
    df <- readRDS(opts$rds)
    if (!is.data.frame(df)) stop("RDS does not contain a data frame.")
    return(df)
  }

  if (!is.null(opts$rdata)) {
    env <- new.env()
    load(opts$rdata, envir = env)
    if (is.null(opts$df)) stop("--df is required when using --rdata")
    if (!exists(opts$df, envir = env)) stop("Data frame not found in RData.")
    df <- get(opts$df, envir = env)
    if (!is.data.frame(df)) stop("RData object is not a data frame.")
    return(df)
  }

  stop("No input provided. Use --csv, --sav, --rds, --rdata, or --interactive.")
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

  opts$vars <- prompt("Variables (comma-separated, blank for all numeric)", "")
  opts$group <- prompt("Grouping variable (blank for none)", "")
  opts$digits <- prompt("Rounding digits", "2")
  opts$out <- prompt("Output directory", get_default_out())
  opts
}

select_variables <- function(df, vars, group_var = NULL) {
  available <- names(df)
  if (is.null(vars) || vars == "") {
    numeric_cols <- available[sapply(df, is.numeric)]
    if (!is.null(group_var)) numeric_cols <- setdiff(numeric_cols, group_var)
    return(numeric_cols)
  }
  requested <- trimws(strsplit(vars, ",", fixed = TRUE)[[1]])
  missing <- setdiff(requested, available)
  if (length(missing) > 0) {
    stop(paste("Unknown variables:", paste(missing, collapse = ", ")))
  }
  if (!is.null(group_var)) requested <- setdiff(requested, group_var)
  requested
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

round_numeric <- function(df, digits) {
  out <- df
  numeric_cols <- sapply(out, is.numeric)
  out[numeric_cols] <- lapply(out[numeric_cols], function(x) round(x, digits))
  out
}

format_apa_table <- function(df, digits) {
  display <- df
  display <- round_numeric(display, digits)
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
  display <- round_numeric(df, digits)
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
  opts <- parse_args(args)

  if (!is.null(opts$help)) {
    print_usage()
    quit(status = 0)
  }

  if (!is.null(opts$interactive)) {
    opts <- modifyList(opts, interactive_options())
  }

  digits <- if (!is.null(opts$digits)) as.numeric(opts$digits) else 2
  out_dir <- if (!is.null(opts$out)) opts$out else get_default_out()
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  df <- load_dataframe(opts)
  group_var <- if (!is.null(opts$group) && opts$group != "") opts$group else NULL
  if (!is.null(group_var) && !(group_var %in% names(df))) {
    stop("Grouping variable not found in data frame.")
  }

  vars <- select_variables(df, opts$vars, group_var)
  if (length(vars) == 0) stop("No numeric variables available for analysis.")

  summary_df <- build_summary(df, vars, group_var, digits)
  rounded_df <- round_numeric(summary_df, digits)

  summary_path <- file.path(out_dir, "descriptive_summary.csv")
  apa_table_path <- file.path(out_dir, "apa_table.md")
  apa_text_path <- file.path(out_dir, "apa_text.txt")

  write.csv(rounded_df, summary_path, row.names = FALSE, na = "")
  writeLines(format_apa_table(summary_df, digits), apa_table_path)
  writeLines(format_apa_text(summary_df, digits), apa_text_path)

  cat("Wrote:\n")
  cat("- ", summary_path, "\n", sep = "")
  cat("- ", apa_table_path, "\n", sep = "")
  cat("- ", apa_text_path, "\n", sep = "")
}

main()
