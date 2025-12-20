#!/usr/bin/env Rscript

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
  cat("  --out DIR              Output directory (default: ./outputs/tmp)\n")
  cat("  --interactive          Prompt for inputs\n")
  cat("  --help                 Show this help\n")
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

  opts$vars <- prompt("Variables (comma-separated, blank for all non-numeric)", "")
  opts$group <- prompt("Grouping variable (blank for none)", "")
  opts$`include-numeric` <- prompt("Include numeric columns when vars omitted TRUE/FALSE", "FALSE")
  opts$digits <- prompt("Rounding digits", "2")
  opts$out <- prompt("Output directory", get_default_out())
  opts
}

select_variables <- function(df, vars, group_var = NULL, include_numeric = FALSE) {
  available <- names(df)
  if (is.null(vars) || vars == "") {
    if (include_numeric) {
      selected <- available
    } else {
      selected <- available[!sapply(df, is.numeric)]
      if (length(selected) == 0) {
        selected <- available
      }
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

get_levels <- function(vec) {
  if (is.factor(vec)) {
    return(as.character(levels(vec)))
  }
  values <- unique(vec[!is.na(vec)])
  if (length(values) == 0) return(character(0))
  if (is.numeric(values)) {
    return(as.character(sort(values)))
  }
  return(as.character(sort(values)))
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

round_numeric <- function(df, digits) {
  out <- df
  numeric_cols <- sapply(out, is.numeric)
  out[numeric_cols] <- lapply(out[numeric_cols], function(x) round(x, digits))
  out
}

format_percent <- function(value, digits) {
  if (is.na(value)) return("")
  format(round(value, digits), nsmall = digits, trim = TRUE)
}

format_apa_table <- function(df, digits) {
  display <- round_numeric(df, digits)
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
        if (use_group) grp,
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

format_apa_text <- function(df, digits) {
  display <- round_numeric(df, digits)
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
          row$level,
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

  include_numeric <- parse_bool(opts$`include-numeric`, default = FALSE)
  vars <- select_variables(df, opts$vars, group_var, include_numeric)
  if (length(vars) == 0) stop("No variables available for frequency analysis.")

  summary_df <- build_summary(df, vars, group_var)
  rounded_df <- round_numeric(summary_df, digits)

  summary_path <- file.path(out_dir, "frequencies_summary.csv")
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
