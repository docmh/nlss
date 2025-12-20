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
  cat("Cross-tabulations (base R)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript crosstabs.R --csv data.csv --row row_var --col col_var [--group group_var]\n")
  cat("  Rscript crosstabs.R --sav data.sav --row row_var --col col_var [--group group_var]\n")
  cat("  Rscript crosstabs.R --rds data.rds --row row_var --col col_var [--group group_var]\n")
  cat("  Rscript crosstabs.R --rdata data.RData --df data_frame_name --row row_var --col col_var\n")
  cat("  Rscript crosstabs.R --interactive\n")
  cat("\n")
  cat("Options:\n")
  cat("  --csv PATH               CSV input file\n")
  cat("  --sav PATH               SPSS .sav input file\n")
  cat("  --sep VALUE              CSV separator (default: ,)\n")
  cat("  --header TRUE/FALSE      CSV header (default: TRUE)\n")
  cat("  --rds PATH               RDS input file (data frame)\n")
  cat("  --rdata PATH             RData input file\n")
  cat("  --df NAME                Data frame object name in RData\n")
  cat("  --row NAME               Row variable name\n")
  cat("  --rows LIST              Comma-separated row variables\n")
  cat("  --col NAME               Column variable name\n")
  cat("  --cols LIST              Comma-separated column variables\n")
  cat("  --group NAME             Grouping variable name (optional)\n")
  cat("  --percent TYPE           row/col/total/all (default: all)\n")
  cat("  --apa-percent TYPE       row/col/total/all/none (default: row)\n")
  cat("  --chisq TRUE/FALSE        Run chi-square test (default: TRUE)\n")
  cat("  --yates TRUE/FALSE        Yates correction for 2x2 (default: FALSE)\n")
  cat("  --fisher TRUE/FALSE       Run Fisher's exact test (default: FALSE)\n")
  cat("  --fisher-simulate TRUE/FALSE  Monte Carlo Fisher (default: FALSE)\n")
  cat("  --fisher-b N              Fisher Monte Carlo replications (default: 2000)\n")
  cat("  --fisher-conf-level N     Fisher confidence level (default: 0.95)\n")
  cat("  --expected TRUE/FALSE     Include expected counts (default: TRUE)\n")
  cat("  --residuals TRUE/FALSE    Include residuals (default: TRUE)\n")
  cat("  --digits N                Rounding digits (default: 2)\n")
  cat("  --user-prompt TEXT        Original AI user prompt for logging (optional)\n")
  cat("  --log TRUE/FALSE          Write analysis_log.jsonl (default: TRUE)\n")
  cat("  --out DIR                 Output directory (default: ./outputs/tmp)\n")
  cat("  --interactive             Prompt for inputs\n")
  cat("  --help                    Show this help\n")
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

  opts$row <- prompt("Row variable(s) (comma-separated)", "")
  opts$col <- prompt("Column variable(s) (comma-separated)", "")
  opts$group <- prompt("Grouping variable (blank for none)", "")
  opts$percent <- prompt("Percentages to include (row/col/total/all)", "all")
  opts$`apa-percent` <- prompt("APA percent (row/col/total/all/none)", "row")
  opts$chisq <- prompt("Run chi-square TRUE/FALSE", "TRUE")
  opts$yates <- prompt("Use Yates correction for 2x2 TRUE/FALSE", "FALSE")
  opts$fisher <- prompt("Run Fisher's exact test TRUE/FALSE", "FALSE")
  opts$`fisher-simulate` <- prompt("Fisher simulate TRUE/FALSE", "FALSE")
  opts$`fisher-b` <- prompt("Fisher Monte Carlo replications", "2000")
  opts$`fisher-conf-level` <- prompt("Fisher confidence level", "0.95")
  opts$expected <- prompt("Include expected counts TRUE/FALSE", "TRUE")
  opts$residuals <- prompt("Include residuals TRUE/FALSE", "TRUE")
  opts$digits <- prompt("Rounding digits", "2")
  opts$`user-prompt` <- prompt("User prompt (optional)", "")
  opts$log <- prompt("Write JSONL log TRUE/FALSE", "TRUE")
  opts$out <- prompt("Output directory", get_default_out())
  opts
}


parse_percent_flags <- function(value, default = "all") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  parts <- trimws(strsplit(val, ",", fixed = TRUE)[[1]])
  if (length(parts) == 0 || all(parts == "")) parts <- default
  if ("all" %in% parts) {
    return(list(row = TRUE, col = TRUE, total = TRUE))
  }
  list(
    row = "row" %in% parts,
    col = "col" %in% parts,
    total = "total" %in% parts
  )
}

normalize_apa_percent <- function(value, default = "row") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (!(val %in% c("row", "col", "total", "all", "none"))) {
    val <- default
  }
  val
}


apply_cell_filters <- function(cells, percent_flags, include_expected, include_residuals) {
  out <- cells
  if (!percent_flags$row) out$pct_row <- NA_real_
  if (!percent_flags$col) out$pct_col <- NA_real_
  if (!percent_flags$total) out$pct_total <- NA_real_
  if (!include_expected) out$expected <- NA_real_
  if (!include_residuals) {
    out$std_resid <- NA_real_
    out$adj_resid <- NA_real_
  }
  out
}

build_table <- function(df, row_var, col_var, group_label, options) {
  row_vec <- df[[row_var]]
  col_vec <- df[[col_var]]
  total_n <- length(row_vec)

  row_missing_n <- sum(is.na(row_vec))
  col_missing_n <- sum(is.na(col_vec))
  both_missing_n <- sum(is.na(row_vec) & is.na(col_vec))

  valid_idx <- !is.na(row_vec) & !is.na(col_vec)
  valid_n <- sum(valid_idx)
  missing_n <- total_n - valid_n
  missing_pct <- ifelse(total_n > 0, missing_n / total_n * 100, NA_real_)

  row_levels <- get_levels(row_vec)
  col_levels <- get_levels(col_vec)

  if (valid_n == 0 || length(row_levels) == 0 || length(col_levels) == 0) {
    cells <- data.frame(
      row_var = row_var,
      col_var = col_var,
      group = group_label,
      row_level = "(no valid data)",
      col_level = "(no valid data)",
      n = 0,
      pct_row = NA_real_,
      pct_col = NA_real_,
      pct_total = NA_real_,
      expected = NA_real_,
      std_resid = NA_real_,
      adj_resid = NA_real_,
      stringsAsFactors = FALSE
    )

    tests <- data.frame(
      row_var = row_var,
      col_var = col_var,
      group = group_label,
      valid_n = valid_n,
      total_n = total_n,
      missing_n = missing_n,
      missing_pct = missing_pct,
      row_missing_n = row_missing_n,
      col_missing_n = col_missing_n,
      both_missing_n = both_missing_n,
      n_rows = length(row_levels),
      n_cols = length(col_levels),
      chi_square = NA_real_,
      chi_df = NA_real_,
      chi_p = NA_real_,
      fisher_p = NA_real_,
      fisher_odds_ratio = NA_real_,
      fisher_ci_low = NA_real_,
      fisher_ci_high = NA_real_,
      phi = NA_real_,
      cramers_v = NA_real_,
      contingency_c = NA_real_,
      yates_applied = FALSE,
      fisher_simulated = options$fisher_simulate,
      chisq_method = "",
      fisher_method = "",
      stringsAsFactors = FALSE
    )

    diagnostics <- data.frame(
      row_var = row_var,
      col_var = col_var,
      group = group_label,
      valid_n = valid_n,
      n_cells = length(row_levels) * length(col_levels),
      min_expected = NA_real_,
      n_expected_lt_1 = NA_real_,
      n_expected_lt_5 = NA_real_,
      pct_expected_lt_5 = NA_real_,
      stringsAsFactors = FALSE
    )

    return(list(cells = cells, tests = tests, diagnostics = diagnostics))
  }

  row_factor <- factor(row_vec, levels = row_levels)
  col_factor <- factor(col_vec, levels = col_levels)
  tab <- table(row_factor[valid_idx], col_factor[valid_idx], useNA = "no")

  row_sums <- rowSums(tab)
  col_sums <- colSums(tab)
  expected <- outer(row_sums, col_sums) / valid_n
  dimnames(expected) <- dimnames(tab)

  std_resid <- (tab - expected) / sqrt(expected)
  std_resid[expected == 0] <- NA_real_

  row_prop <- row_sums / valid_n
  col_prop <- col_sums / valid_n
  denom <- sqrt(expected * (1 - row_prop) * (1 - col_prop))
  adj_resid <- (tab - expected) / denom
  adj_resid[denom == 0] <- NA_real_

  row_pct <- sweep(tab, 1, row_sums, FUN = "/") * 100
  col_pct <- sweep(tab, 2, col_sums, FUN = "/") * 100
  total_pct <- tab / valid_n * 100
  row_pct[is.nan(row_pct)] <- NA_real_
  col_pct[is.nan(col_pct)] <- NA_real_
  total_pct[is.nan(total_pct)] <- NA_real_

  cells <- as.data.frame(as.table(tab), stringsAsFactors = FALSE)
  names(cells) <- c("row_level", "col_level", "n")
  cells$pct_row <- as.data.frame(as.table(row_pct), stringsAsFactors = FALSE)$Freq
  cells$pct_col <- as.data.frame(as.table(col_pct), stringsAsFactors = FALSE)$Freq
  cells$pct_total <- as.data.frame(as.table(total_pct), stringsAsFactors = FALSE)$Freq
  cells$expected <- as.data.frame(as.table(expected), stringsAsFactors = FALSE)$Freq
  cells$std_resid <- as.data.frame(as.table(std_resid), stringsAsFactors = FALSE)$Freq
  cells$adj_resid <- as.data.frame(as.table(adj_resid), stringsAsFactors = FALSE)$Freq
  cells$row_var <- row_var
  cells$col_var <- col_var
  cells$group <- group_label
  cells <- cells[, c("row_var", "col_var", "group", "row_level", "col_level", "n",
                     "pct_row", "pct_col", "pct_total", "expected", "std_resid", "adj_resid")]

  tab_test <- tab[row_sums > 0, col_sums > 0, drop = FALSE]

  chi_square <- NA_real_
  chi_df <- NA_real_
  chi_p <- NA_real_
  chisq_method <- ""
  yates_applied <- FALSE

  if (options$chisq && valid_n > 0 && nrow(tab_test) >= 2 && ncol(tab_test) >= 2) {
    correct_flag <- options$yates && nrow(tab_test) == 2 && ncol(tab_test) == 2
    chisq_res <- suppressWarnings(chisq.test(tab_test, correct = correct_flag))
    chi_square <- unname(chisq_res$statistic)
    chi_df <- unname(chisq_res$parameter)
    chi_p <- chisq_res$p.value
    chisq_method <- chisq_res$method
    yates_applied <- correct_flag
  }

  phi <- if (!is.na(chi_square) && nrow(tab_test) == 2 && ncol(tab_test) == 2 && valid_n > 0) {
    sqrt(chi_square / valid_n)
  } else {
    NA_real_
  }

  min_dim <- min(nrow(tab_test), ncol(tab_test))
  cramers_v <- if (!is.na(chi_square) && valid_n > 0 && min_dim > 1) {
    sqrt(chi_square / (valid_n * (min_dim - 1)))
  } else {
    NA_real_
  }

  contingency_c <- if (!is.na(chi_square) && valid_n > 0) {
    sqrt(chi_square / (chi_square + valid_n))
  } else {
    NA_real_
  }

  fisher_p <- NA_real_
  fisher_or <- NA_real_
  fisher_ci_low <- NA_real_
  fisher_ci_high <- NA_real_
  fisher_method <- ""

  if (options$fisher && valid_n > 0 && nrow(tab_test) >= 2 && ncol(tab_test) >= 2) {
    fisher_res <- tryCatch(
      fisher.test(tab_test,
                  simulate.p.value = options$fisher_simulate,
                  B = options$fisher_b,
                  conf.level = options$fisher_conf_level),
      error = function(e) NULL
    )
    if (!is.null(fisher_res)) {
      fisher_p <- fisher_res$p.value
      fisher_method <- fisher_res$method
      if (!is.null(fisher_res$estimate) && length(fisher_res$estimate) == 1) {
        fisher_or <- unname(fisher_res$estimate)
      }
      if (!is.null(fisher_res$conf.int) && length(fisher_res$conf.int) == 2) {
        fisher_ci_low <- fisher_res$conf.int[1]
        fisher_ci_high <- fisher_res$conf.int[2]
      }
    }
  }

  tests <- data.frame(
    row_var = row_var,
    col_var = col_var,
    group = group_label,
    valid_n = valid_n,
    total_n = total_n,
    missing_n = missing_n,
    missing_pct = missing_pct,
    row_missing_n = row_missing_n,
    col_missing_n = col_missing_n,
    both_missing_n = both_missing_n,
    n_rows = nrow(tab_test),
    n_cols = ncol(tab_test),
    chi_square = chi_square,
    chi_df = chi_df,
    chi_p = chi_p,
    fisher_p = fisher_p,
    fisher_odds_ratio = fisher_or,
    fisher_ci_low = fisher_ci_low,
    fisher_ci_high = fisher_ci_high,
    phi = phi,
    cramers_v = cramers_v,
    contingency_c = contingency_c,
    yates_applied = yates_applied,
    fisher_simulated = options$fisher_simulate,
    chisq_method = chisq_method,
    fisher_method = fisher_method,
    stringsAsFactors = FALSE
  )

  expected_test <- outer(rowSums(tab_test), colSums(tab_test)) / sum(tab_test)
  min_expected <- min(expected_test)
  n_cells <- length(expected_test)
  n_expected_lt_1 <- sum(expected_test < 1)
  n_expected_lt_5 <- sum(expected_test < 5)
  pct_expected_lt_5 <- ifelse(n_cells > 0, n_expected_lt_5 / n_cells * 100, NA_real_)

  diagnostics <- data.frame(
    row_var = row_var,
    col_var = col_var,
    group = group_label,
    valid_n = valid_n,
    n_cells = n_cells,
    min_expected = min_expected,
    n_expected_lt_1 = n_expected_lt_1,
    n_expected_lt_5 = n_expected_lt_5,
    pct_expected_lt_5 = pct_expected_lt_5,
    stringsAsFactors = FALSE
  )

  list(cells = cells, tests = tests, diagnostics = diagnostics)
}


format_num <- function(value, digits) {
  if (is.na(value)) return("NA")
  format(round(value, digits), nsmall = digits, trim = TRUE)
}

format_p <- function(p_value) {
  if (is.na(p_value)) return("NA")
  if (p_value < 0.001) return("< .001")
  p_txt <- format(round(p_value, 3), nsmall = 3, trim = TRUE)
  p_txt <- sub("^0", "", p_txt)
  paste("=", p_txt)
}

get_percent_columns <- function(apa_percent) {
  if (apa_percent == "none") return(character(0))
  if (apa_percent == "all") return(c("pct_row", "pct_col", "pct_total"))
  if (apa_percent == "row") return("pct_row")
  if (apa_percent == "col") return("pct_col")
  if (apa_percent == "total") return("pct_total")
  character(0)
}

format_apa_table <- function(cells_df, digits, apa_percent) {
  display <- round_numeric(cells_df, digits)
  display$group <- as.character(display$group)
  display$group[is.na(display$group)] <- "NA"
  use_group <- !all(display$group == "")

  combos <- unique(display[, c("row_var", "col_var", "group")])
  percent_cols <- get_percent_columns(apa_percent)
  percent_labels <- c(pct_row = "Row %", pct_col = "Column %", pct_total = "Total %")

  sections <- character(0)
  for (idx in seq_len(nrow(combos))) {
    row_var <- combos$row_var[idx]
    col_var <- combos$col_var[idx]
    group <- combos$group[idx]
    subset <- display[display$row_var == row_var & display$col_var == col_var & display$group == group, , drop = FALSE]

    header <- sprintf("Table 1\nCross-tabulation of %s by %s", row_var, col_var)
    if (use_group) {
      header <- paste0(header, "\nGroup: ", group)
    }

    headers <- c("Row", "Column", "n", percent_labels[percent_cols])
    md <- paste0(header, "\n\n| ", paste(headers, collapse = " | "), " |\n")
    md <- paste0(md, "| ", paste(rep("---", length(headers)), collapse = " | "), " |\n")

    for (i in seq_len(nrow(subset))) {
      row <- subset[i, ]
      row_vals <- c(
        row$row_level,
        row$col_level,
        ifelse(is.na(row$n), "", as.character(row$n))
      )
      if (length(percent_cols) > 0) {
        for (col in percent_cols) {
          row_vals <- c(row_vals, format_percent(row[[col]], digits))
        }
      }
      md <- paste0(md, "| ", paste(row_vals, collapse = " | "), " |\n")
    }

    note_parts <- c("Note. Percentages are based on complete cases; missing values excluded.")
    if (length(percent_cols) > 0) {
      note_parts <- c(note_parts, paste("Reported:", paste(percent_labels[percent_cols], collapse = ", "), sep = " "))
    }
    md <- paste0(md, "\n", paste(note_parts, collapse = " "), "\n")

    sections <- c(sections, md)
  }

  paste(sections, collapse = "\n\n")
}

format_apa_text <- function(tests_df, diagnostics_df, digits) {
  tests <- round_numeric(tests_df, digits)
  diagnostics <- round_numeric(diagnostics_df, digits)

  tests$group <- as.character(tests$group)
  diagnostics$group <- as.character(diagnostics$group)
  tests$group[is.na(tests$group)] <- "NA"
  diagnostics$group[is.na(diagnostics$group)] <- "NA"

  combos <- unique(tests[, c("row_var", "col_var", "group")])
  lines <- character(0)

  for (idx in seq_len(nrow(combos))) {
    row_var <- combos$row_var[idx]
    col_var <- combos$col_var[idx]
    group <- combos$group[idx]
    row <- tests[tests$row_var == row_var & tests$col_var == col_var & tests$group == group, , drop = FALSE]
    diag_row <- diagnostics[diagnostics$row_var == row_var & diagnostics$col_var == col_var & diagnostics$group == group, , drop = FALSE]

    label <- if (group == "") {
      sprintf("%s by %s", row_var, col_var)
    } else {
      sprintf("Group %s, %s by %s", group, row_var, col_var)
    }

    valid_n <- row$valid_n[1]
    missing_n <- row$missing_n[1]
    missing_pct <- row$missing_pct[1]

    if (is.na(valid_n) || valid_n == 0) {
      line <- sprintf(
        "%s: no valid observations. Missing = %s (%s%%).",
        label,
        ifelse(is.na(missing_n), "NA", as.character(missing_n)),
        ifelse(is.na(missing_pct), "NA", format_num(missing_pct, 1))
      )
      lines <- c(lines, line)
      next
    }

    parts <- character(0)
    if (!is.na(row$chi_square[1])) {
      chi_part <- sprintf(
        "chi-square(%s, N = %s) = %s, p %s",
        ifelse(is.na(row$chi_df[1]), "NA", as.character(row$chi_df[1])),
        as.character(valid_n),
        format_num(row$chi_square[1], digits),
        format_p(row$chi_p[1])
      )
      parts <- c(parts, chi_part)

      effect_text <- if (!is.na(row$phi[1])) {
        sprintf("phi = %s", format_num(row$phi[1], digits))
      } else if (!is.na(row$cramers_v[1])) {
        sprintf("Cramer's V = %s", format_num(row$cramers_v[1], digits))
      } else {
        ""
      }

      if (effect_text != "") {
        parts <- c(parts, effect_text)
      }
    }

    if (!is.na(row$fisher_p[1])) {
      fisher_part <- sprintf("Fisher's exact test p %s", format_p(row$fisher_p[1]))
      if (!is.na(row$fisher_odds_ratio[1])) {
        fisher_part <- paste0(
          fisher_part,
          ", odds ratio = ",
          format_num(row$fisher_odds_ratio[1], digits)
        )
        if (!is.na(row$fisher_ci_low[1]) && !is.na(row$fisher_ci_high[1])) {
          fisher_part <- paste0(
            fisher_part,
            ", 95% CI [",
            format_num(row$fisher_ci_low[1], digits),
            ", ",
            format_num(row$fisher_ci_high[1], digits),
            "]"
          )
        }
      }
      parts <- c(parts, fisher_part)
    }

    diag_part <- ""
    if (nrow(diag_row) > 0 && !is.na(diag_row$min_expected[1])) {
      diag_part <- sprintf(
        "Expected counts: min = %s; %s%% of cells < 5",
        format_num(diag_row$min_expected[1], digits),
        format_num(diag_row$pct_expected_lt_5[1], 1)
      )
    }

    missing_part <- sprintf(
      "Missing = %s (%s%%)",
      ifelse(is.na(missing_n), "NA", as.character(missing_n)),
      ifelse(is.na(missing_pct), "NA", format_num(missing_pct, 1))
    )

    if (length(parts) == 0) {
      line <- paste0(label, ": no association test computed")
      if (diag_part != "") {
        line <- paste0(line, ". ", diag_part)
      }
      line <- paste0(line, ". ", missing_part, ".")
      lines <- c(lines, line)
      next
    }

    line <- paste0(label, ": ", paste(parts, collapse = "; "))
    if (diag_part != "") {
      line <- paste0(line, ". ", diag_part)
    }
    line <- paste0(line, ". ", missing_part, ".")

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
  out_dir <- ensure_out_dir(if (!is.null(opts$out)) opts$out else get_default_out())

  df <- load_dataframe(opts)
  group_var <- if (!is.null(opts$group) && opts$group != "") opts$group else NULL

  rows <- if (!is.null(opts$rows)) parse_list(opts$rows) else parse_list(opts$row)
  cols <- if (!is.null(opts$cols)) parse_list(opts$cols) else parse_list(opts$col)

  if (length(rows) == 0 || length(cols) == 0) {
    stop("Provide --row/--rows and --col/--cols.")
  }

  vars_needed <- unique(c(rows, cols, if (!is.null(group_var)) group_var))
  missing_vars <- setdiff(vars_needed, names(df))
  if (length(missing_vars) > 0) {
    stop(paste("Unknown variables:", paste(missing_vars, collapse = ", ")))
  }

  percent_flags <- parse_percent_flags(opts$percent, default = "all")
  apa_percent <- normalize_apa_percent(opts$`apa-percent`, default = "row")

  options <- list(
    chisq = parse_bool(opts$chisq, default = TRUE),
    yates = parse_bool(opts$yates, default = FALSE),
    fisher = parse_bool(opts$fisher, default = FALSE),
    fisher_simulate = parse_bool(opts$`fisher-simulate`, default = FALSE),
    fisher_b = if (!is.null(opts$`fisher-b`)) as.integer(opts$`fisher-b`) else 2000,
    fisher_conf_level = if (!is.null(opts$`fisher-conf-level`)) as.numeric(opts$`fisher-conf-level`) else 0.95,
    include_expected = parse_bool(opts$expected, default = TRUE),
    include_residuals = parse_bool(opts$residuals, default = TRUE)
  )

  cells_list <- list()
  tests_list <- list()
  diag_list <- list()

  if (!is.null(group_var)) {
    group_vec <- df[[group_var]]
    group_levels <- unique(group_vec)
    for (g in group_levels) {
      idx <- if (is.na(g)) is.na(group_vec) else group_vec == g
      sub_df <- df[idx, , drop = FALSE]
      group_label <- ifelse(is.na(g), "NA", as.character(g))
      for (row_var in rows) {
        for (col_var in cols) {
          result <- build_table(sub_df, row_var, col_var, group_label, options)
          cells_list[[length(cells_list) + 1]] <- result$cells
          tests_list[[length(tests_list) + 1]] <- result$tests
          diag_list[[length(diag_list) + 1]] <- result$diagnostics
        }
      }
    }
  } else {
    for (row_var in rows) {
      for (col_var in cols) {
        result <- build_table(df, row_var, col_var, "", options)
        cells_list[[length(cells_list) + 1]] <- result$cells
        tests_list[[length(tests_list) + 1]] <- result$tests
        diag_list[[length(diag_list) + 1]] <- result$diagnostics
      }
    }
  }

  cells_df <- do.call(rbind, cells_list)
  tests_df <- do.call(rbind, tests_list)
  diagnostics_df <- do.call(rbind, diag_list)

  apa_report_path <- file.path(out_dir, "apa_report.md")
  apa_table <- format_apa_table(cells_df, digits, apa_percent)
  apa_text <- format_apa_text(tests_df, diagnostics_df, digits)
  append_apa_report(apa_report_path, "Cross-tabulations", apa_table, apa_text)

  cat("Wrote:\n")
  cat("- ", apa_report_path, "\n", sep = "")

  if (parse_bool(opts$log, default = TRUE)) {
    ctx <- get_run_context()
    append_analysis_log(
      out_dir,
      module = "crosstabs",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(cells_df = cells_df, tests_df = tests_df, diagnostics_df = diagnostics_df),
      options = list(
        digits = digits,
        rows = rows,
        cols = cols,
        group = group_var,
        percent = percent_flags,
        apa_percent = apa_percent,
        chisq = options$chisq,
        yates = options$yates,
        fisher = options$fisher,
        fisher_simulate = options$fisher_simulate,
        fisher_b = options$fisher_b,
        fisher_conf_level = options$fisher_conf_level,
        include_expected = options$include_expected,
        include_residuals = options$include_residuals
      ),
      user_prompt = get_user_prompt(opts)
    )
  }
}

main()
