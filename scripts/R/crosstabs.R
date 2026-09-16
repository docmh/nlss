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


# Static analysis aliases for source_lib-defined functions.
render_output_path <- get("render_output_path", mode = "function")
add_group_label_column <- get("add_group_label_column", mode = "function")
add_value_label_column <- get("add_value_label_column", mode = "function")
add_variable_label_column <- get("add_variable_label_column", mode = "function")
resolve_label_metadata <- get("resolve_label_metadata", mode = "function")
source_lib <- get("source_lib", mode = "function")

print_usage <- function() {
  cat("Cross-tabulations (base R)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript crosstabs.R --csv data.csv --row row_var --col col_var [--group group_var]\n")
  cat("  Rscript crosstabs.R --sav data.sav --row row_var --col col_var [--group group_var]\n")
  cat("  Rscript crosstabs.R --rds data.rds --row row_var --col col_var [--group group_var]\n")
  cat("  Rscript crosstabs.R --rdata data.RData --df data_frame_name --row row_var --col col_var\n")
  cat("  Rscript crosstabs.R --parquet data.parquet --row row_var --col col_var\n")
  cat("  Rscript crosstabs.R --interactive\n")
  cat("\n")
  cat("Options:\n")
  cat("  --csv PATH               CSV input file\n")
  cat("  --sav PATH               SPSS .sav input file\n")
  cat("  --sep VALUE              CSV separator (default: ,)\n")
  cat("  --header TRUE/FALSE      CSV header (default: TRUE)\n")
  cat("  --rds PATH               RDS input file (data frame)\n")
  cat("  --rdata PATH             RData input file\n")
  cat("  --parquet PATH           Parquet input file\n")
  cat("  --df NAME                Data frame object name in RData\n")
  cat("  --row NAME               Row variable name\n")
  cat("  --rows LIST              Comma-separated row variables\n")
  cat("  --col NAME               Column variable name\n")
  cat("  --cols LIST              Comma-separated column variables\n")
  cat("  --group NAME             Grouping variable name (optional)\n")
  cat("  --percent TYPE           row/col/total/all (default: all)\n")
  cat("  --nlss-percent TYPE      row/col/total/all/none (default: row)\n")
  cat("  --chisq TRUE/FALSE        Run chi² test (default: TRUE)\n")
  cat("  --yates TRUE/FALSE        Yates correction for 2x2 (default: FALSE)\n")
  cat("  --fisher TRUE/FALSE       Run Fisher's exact test (default: FALSE)\n")
  cat("  --fisher-simulate TRUE/FALSE  Monte Carlo Fisher (default: FALSE)\n")
  cat("  --fisher-b N              Fisher Monte Carlo replications (default: 2000)\n")
  cat("  --seed N                  Seed for simulated Fisher (default: modules.crosstabs.seed)\n")
  cat("  --fisher-conf-level N     Fisher confidence level (default: 0.95)\n")
  cat("  --expected TRUE/FALSE     Include expected counts (default: TRUE)\n")
  cat("  --residuals TRUE/FALSE    Include residuals (default: TRUE)\n")
  cat("  --digits N                Rounding digits (default: 2)\n")
  cat("  --template REF            Template path or template key (optional)\n")
  cat("  --user-prompt TEXT        Original AI user prompt for logging (optional)\n")
  cat("  --log TRUE/FALSE          Write analysis_log.jsonl (default: TRUE)\n")
  cat("  --interactive             Prompt for inputs\n")
  cat("  --help                    Show this help\n")
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

  opts$row <- prompt("Row variable(s) (comma-separated)", "")
  opts$col <- prompt("Column variable(s) (comma-separated)", "")
  opts$group <- prompt("Grouping variable (blank for none)", "")
  percent_default <- get_config_value("modules.crosstabs.percent", "all")
  nlss_percent_default <- get_config_value("modules.crosstabs.nlss_percent", "row")
  chisq_default <- get_config_value("modules.crosstabs.chisq", TRUE)
  yates_default <- get_config_value("modules.crosstabs.yates", FALSE)
  fisher_default <- get_config_value("modules.crosstabs.fisher", FALSE)
  fisher_sim_default <- get_config_value("modules.crosstabs.fisher_simulate", FALSE)
  fisher_b_default <- get_config_value("modules.crosstabs.fisher_b", 2000)
  fisher_conf_default <- get_config_value("modules.crosstabs.fisher_conf_level", 0.95)
  expected_default <- get_config_value("modules.crosstabs.expected", TRUE)
  residuals_default <- get_config_value("modules.crosstabs.residuals", TRUE)
  digits_default <- get_config_value("defaults.digits", 2)
  opts$percent <- prompt("Percentages to include (row/col/total/all)", percent_default)
  opts$`nlss-percent` <- prompt("NLSS format percent (row/col/total/all/none)", nlss_percent_default)
  opts$chisq <- prompt("Run chi² TRUE/FALSE", ifelse(isTRUE(chisq_default), "TRUE", "FALSE"))
  opts$yates <- prompt("Use Yates correction for 2x2 TRUE/FALSE", ifelse(isTRUE(yates_default), "TRUE", "FALSE"))
  opts$fisher <- prompt("Run Fisher's exact test TRUE/FALSE", ifelse(isTRUE(fisher_default), "TRUE", "FALSE"))
  opts$`fisher-simulate` <- prompt(
    "Fisher simulate TRUE/FALSE",
    ifelse(isTRUE(fisher_sim_default), "TRUE", "FALSE")
  )
  opts$`fisher-b` <- prompt("Fisher Monte Carlo replications", as.character(fisher_b_default))
  opts$seed <- prompt("Simulation seed", as.character(get_config_value("modules.crosstabs.seed", 1L)))
  opts$`fisher-conf-level` <- prompt("Fisher confidence level", as.character(fisher_conf_default))
  opts$expected <- prompt("Include expected counts TRUE/FALSE", ifelse(isTRUE(expected_default), "TRUE", "FALSE"))
  opts$residuals <- prompt("Include residuals TRUE/FALSE", ifelse(isTRUE(residuals_default), "TRUE", "FALSE"))
  opts$digits <- prompt("Rounding digits", as.character(digits_default))
  opts$template <- prompt("Template (path or key; blank for default)", "")
  opts$`user-prompt` <- prompt("User prompt (optional)", "")
  log_default <- get_config_value("defaults.log", TRUE)
  opts$log <- prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))
  opts
}

parse_percent_flags <- function(value, default = "all") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  parts <- trimws(strsplit(val, ",", fixed = TRUE)[[1]])
  if (!length(parts) || any(!parts %in% c("row", "col", "total", "all"))) {
    stop("Percent must be row, col, total, all, or a comma-separated combination.")
  }
  if ("all" %in% parts) {
    return(list(row = TRUE, col = TRUE, total = TRUE))
  }
  list(
    row = "row" %in% parts,
    col = "col" %in% parts,
    total = "total" %in% parts
  )
}

normalize_nlss_percent <- function(value, default = "row") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (!(val %in% c("row", "col", "total", "all", "none"))) {
    stop("NLSS percent must be row, col, total, all, or none.")
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

fisher_value_status <- function(value) {
  if (is.na(value)) return("not_available")
  if (is.infinite(value)) return(if (value > 0) "positive_infinity" else "negative_infinity")
  "finite"
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
    if (options$chisq || options$fisher) {
      stop("Association test not estimable for ", row_var, " by ", col_var,
           if (nzchar(group_label)) paste0(" (group ", group_label, ")") else "",
           ": no complete observations. For descriptive counts only, use --chisq FALSE --fisher FALSE.")
    }
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
      fisher_odds_ratio_status = "not_available",
      fisher_ci_low_status = "not_available",
      fisher_ci_high_status = "not_available",
      phi = NA_real_,
      cramers_v = NA_real_,
      contingency_c = NA_real_,
      yates_applied = FALSE,
      fisher_simulated = FALSE,
      fisher_conf_level = options$fisher_conf_level,
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
  denom <- sqrt(expected * outer(1 - row_prop, 1 - col_prop))
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
  if ((options$chisq || options$fisher) && (nrow(tab_test) < 2L || ncol(tab_test) < 2L)) {
    stop("Association test not estimable for ", row_var, " by ", col_var,
         if (nzchar(group_label)) paste0(" (group ", group_label, ")") else "",
         ": at least two observed row and column levels are required. ",
         "For descriptive counts only, use --chisq FALSE --fisher FALSE.")
  }

  chi_square <- NA_real_
  chi_df <- NA_real_
  chi_p <- NA_real_
  chisq_method <- ""
  yates_applied <- FALSE

  if (options$chisq && valid_n > 0 && nrow(tab_test) >= 2 && ncol(tab_test) >= 2) {
    correct_flag <- options$yates && nrow(tab_test) == 2 && ncol(tab_test) == 2
    chisq_res <- stats::chisq.test(tab_test, correct = correct_flag)
    if (any(!is.finite(c(chisq_res$statistic, chisq_res$parameter, chisq_res$p.value)))) {
      stop("Requested chi-square test returned non-finite results for ", row_var, " by ", col_var, ".")
    }
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
  fisher_simulated <- FALSE

  if (options$fisher && valid_n > 0 && nrow(tab_test) >= 2 && ncol(tab_test) >= 2) {
    fisher_res <- tryCatch(
      stats::fisher.test(tab_test,
                  simulate.p.value = options$fisher_simulate,
                  B = options$fisher_b,
                  conf.level = options$fisher_conf_level),
      error = function(e) stop("Requested Fisher test failed for ", row_var, " by ", col_var,
                               ": ", conditionMessage(e), call. = FALSE)
    )
    if (!is.finite(fisher_res$p.value)) stop("Requested Fisher test returned a non-finite p-value.")
    {
      fisher_p <- fisher_res$p.value
      fisher_method <- fisher_res$method
      fisher_simulated <- grepl("simulated p-value", fisher_method, fixed = TRUE)
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
    fisher_odds_ratio_status = fisher_value_status(fisher_or),
    fisher_ci_low_status = fisher_value_status(fisher_ci_low),
    fisher_ci_high_status = fisher_value_status(fisher_ci_high),
    phi = phi,
    cramers_v = cramers_v,
    contingency_c = contingency_c,
    yates_applied = yates_applied,
    fisher_simulated = fisher_simulated,
    fisher_conf_level = options$fisher_conf_level,
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

attach_effect_sizes_to_cells <- function(cells_df, tests_df) {
  if (is.null(cells_df) || nrow(cells_df) == 0) return(cells_df)
  if (is.null(tests_df) || nrow(tests_df) == 0) return(cells_df)
  if (!all(c("row_var", "col_var", "group") %in% names(cells_df))) return(cells_df)
  if (!all(c("row_var", "col_var", "group") %in% names(tests_df))) return(cells_df)

  cells_df$group <- as.character(cells_df$group)
  tests_df$group <- as.character(tests_df$group)
  cells_df$group[is.na(cells_df$group)] <- "NA"
  tests_df$group[is.na(tests_df$group)] <- "NA"

  # Compare the tuple itself: variable or group names may contain separators.
  cell_key <- cells_df[, c("row_var", "col_var", "group"), drop = FALSE]
  match_idx <- vapply(seq_len(nrow(cells_df)), function(i) {
    which(tests_df$row_var == cells_df$row_var[i] &
          tests_df$col_var == cells_df$col_var[i] &
          tests_df$group == cells_df$group[i])[1L]
  }, integer(1))

  cells_df$phi <- ifelse(!is.na(match_idx), tests_df$phi[match_idx], NA_real_)
  cells_df$cramers_v <- ifelse(!is.na(match_idx), tests_df$cramers_v[match_idx], NA_real_)
  cells_df$contingency_c <- ifelse(!is.na(match_idx), tests_df$contingency_c[match_idx], NA_real_)

  first_row <- !duplicated(cell_key)
  cells_df$phi[!first_row] <- NA_real_
  cells_df$cramers_v[!first_row] <- NA_real_
  cells_df$contingency_c[!first_row] <- NA_real_

  cells_df
}

get_percent_columns <- function(nlss_percent) {
  if (nlss_percent == "none") return(character(0))
  if (nlss_percent == "all") return(c("pct_row", "pct_col", "pct_total"))
  if (nlss_percent == "row") return("pct_row")
  if (nlss_percent == "col") return("pct_col")
  if (nlss_percent == "total") return("pct_total")
  character(0)
}

format_nlss_table <- function(cells_df, digits, nlss_percent, layout = "sectioned", include_group = TRUE) {
  display <- round_numeric(cells_df, digits)
  display$group <- as.character(display$group)
  display$group[is.na(display$group)] <- "NA"
  display$row_var_display <- if ("row_var_label" %in% names(display)) display$row_var_label else display$row_var
  display$col_var_display <- if ("col_var_label" %in% names(display)) display$col_var_label else display$col_var
  display$row_level_display <- if ("row_level_label" %in% names(display)) display$row_level_label else display$row_level
  display$col_level_display <- if ("col_level_label" %in% names(display)) display$col_level_label else display$col_level
  display$group_display <- if ("group_label" %in% names(display)) display$group_label else display$group
  display$group_display <- as.character(display$group_display)
  display$group_display[is.na(display$group_display)] <- "NA"

  layout <- tolower(layout)
  if (!(layout %in% c("sectioned", "long"))) {
    layout <- "sectioned"
  }

  percent_cols <- get_percent_columns(nlss_percent)
  percent_labels <- c(pct_row = "Row %", pct_col = "Column %", pct_total = "Total %")

  if (layout == "long") {
    include_group <- isTRUE(include_group) && !all(display$group == "")
    combos <- unique(display[, c("row_var", "col_var")])
    if (nrow(combos) == 1) {
      row_label <- display$row_var_display[1]
      col_label <- display$col_var_display[1]
      header <- sprintf("Table 1\nCross-tabulation of %s by %s", row_label, col_label)
    } else {
      header <- "Table 1\nCross-tabulation results"
    }

    headers <- c("Row Variable", "Column Variable")
    if (include_group) {
      headers <- c(headers, "Group")
    }
    headers <- c(headers, "Row Level", "Column Level", "n", percent_labels[percent_cols])

    md <- paste0(header, "\n\n| ", paste(headers, collapse = " | "), " |\n")
    md <- paste0(md, "| ", paste(rep("---", length(headers)), collapse = " | "), " |\n")

    for (i in seq_len(nrow(display))) {
      row <- display[i, ]
      row_vals <- c(row$row_var_display, row$col_var_display)
      if (include_group) {
        row_vals <- c(row_vals, row$group_display)
      }
      row_vals <- c(
        row_vals,
        row$row_level_display,
        row$col_level_display,
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

    return(md)
  }

  use_group <- !all(display$group == "")
  combos <- unique(display[, c("row_var", "col_var", "group")])

  sections <- character(0)
  for (idx in seq_len(nrow(combos))) {
    row_var <- combos$row_var[idx]
    col_var <- combos$col_var[idx]
    group <- combos$group[idx]
    subset <- display[display$row_var == row_var & display$col_var == col_var & display$group == group, , drop = FALSE]
    row_label <- subset$row_var_display[1]
    col_label <- subset$col_var_display[1]
    group_label <- subset$group_display[1]
    header <- sprintf("Table 1\nCross-tabulation of %s by %s", row_label, col_label)
    if (use_group) {
      header <- paste0(header, "\nGroup: ", group_label)
    }

    headers <- c("Row", "Column", "n", percent_labels[percent_cols])
    md <- paste0(header, "\n\n| ", paste(headers, collapse = " | "), " |\n")
    md <- paste0(md, "| ", paste(rep("---", length(headers)), collapse = " | "), " |\n")

    for (i in seq_len(nrow(subset))) {
      row <- subset[i, ]
      row_vals <- c(
        row$row_level_display,
        row$col_level_display,
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

format_nlss_text <- function(tests_df, diagnostics_df, digits) {
  tests <- tests_df
  diagnostics <- diagnostics_df

  tests$group <- as.character(tests$group)
  diagnostics$group <- as.character(diagnostics$group)
  tests$group[is.na(tests$group)] <- "NA"
  diagnostics$group[is.na(diagnostics$group)] <- "NA"
  tests$row_var_display <- if ("row_var_label" %in% names(tests)) tests$row_var_label else tests$row_var
  tests$col_var_display <- if ("col_var_label" %in% names(tests)) tests$col_var_label else tests$col_var
  tests$group_display <- if ("group_label" %in% names(tests)) tests$group_label else tests$group
  diagnostics$row_var_display <- if ("row_var_label" %in% names(diagnostics)) diagnostics$row_var_label else diagnostics$row_var
  diagnostics$col_var_display <- if ("col_var_label" %in% names(diagnostics)) diagnostics$col_var_label else diagnostics$col_var
  diagnostics$group_display <- if ("group_label" %in% names(diagnostics)) diagnostics$group_label else diagnostics$group

  combos <- unique(tests[, c("row_var", "col_var", "group")])
  lines <- character(0)

  for (idx in seq_len(nrow(combos))) {
    row_var <- combos$row_var[idx]
    col_var <- combos$col_var[idx]
    group <- combos$group[idx]
    row <- tests[tests$row_var == row_var & tests$col_var == col_var & tests$group == group, , drop = FALSE]
    diag_row <- diagnostics[diagnostics$row_var == row_var & diagnostics$col_var == col_var & diagnostics$group == group, , drop = FALSE]

    row_label <- row$row_var_display[1]
    col_label <- row$col_var_display[1]
    group_label <- row$group_display[1]
    label <- if (group == "") {
      sprintf("%s by %s", row_label, col_label)
    } else {
      sprintf("Group %s, %s by %s", group_label, row_label, col_label)
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
        "chi²(%s, N = %s) = %s, p %s",
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
            ", ", format(100 * row$fisher_conf_level[1], trim = TRUE), "% CI [",
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

format_num_cell <- function(value, digits) {
  if (is.na(value)) return("")
  format_num(value, digits)
}

build_crosstabs_table_body <- function(cells_df, digits, table_spec = NULL) {
  display <- round_numeric(cells_df, digits)
  display$group <- as.character(display$group)
  display$group[is.na(display$group)] <- "NA"
  display$row_var_display <- if ("row_var_label" %in% names(display)) display$row_var_label else display$row_var
  display$col_var_display <- if ("col_var_label" %in% names(display)) display$col_var_label else display$col_var
  display$row_level_display <- if ("row_level_label" %in% names(display)) display$row_level_label else display$row_level
  display$col_level_display <- if ("col_level_label" %in% names(display)) display$col_level_label else display$col_level
  display$group_display <- if ("group_label" %in% names(display)) display$group_label else display$group
  display$group_display <- as.character(display$group_display)
  display$group_display[is.na(display$group_display)] <- "NA"

  default_columns <- list(
    list(key = "row_var", label = "Row Variable"),
    list(key = "col_var", label = "Column Variable"),
    list(key = "group", label = "Group", drop_if_empty = TRUE),
    list(key = "row_level", label = "Row Level"),
    list(key = "col_level", label = "Column Level"),
    list(key = "n", label = "n"),
    list(key = "pct_row", label = "Row %", drop_if_empty = TRUE),
    list(key = "pct_col", label = "Column %", drop_if_empty = TRUE),
    list(key = "pct_total", label = "Total %", drop_if_empty = TRUE),
    list(key = "phi", label = "phi", drop_if_empty = TRUE),
    list(key = "cramers_v", label = "Cramer's V", drop_if_empty = TRUE),
    list(key = "contingency_c", label = "C", drop_if_empty = TRUE)
  )
  columns <- normalize_table_columns(
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
      if (key %in% c("row_var", "col_var", "group", "row_level", "col_level")) {
        if (key == "row_var") {
          val <- as_cell_text(row$row_var_display[1])
        } else if (key == "col_var") {
          val <- as_cell_text(row$col_var_display[1])
        } else if (key == "group") {
          val <- as_cell_text(row$group_display[1])
        } else if (key == "row_level") {
          val <- as_cell_text(row$row_level_display[1])
        } else if (key == "col_level") {
          val <- as_cell_text(row$col_level_display[1])
        } else {
          val <- as_cell_text(row[[key]][1])
        }
      } else if (key == "n") {
        val <- ifelse(is.na(row$n), "", as.character(row$n))
      } else if (key %in% c("pct_row", "pct_col", "pct_total")) {
        val <- format_percent(row[[key]][1], digits)
      } else if (key %in% c("expected", "std_resid", "adj_resid")) {
        val <- format_num_cell(row[[key]][1], digits)
      } else if (key %in% names(row)) {
        cell <- row[[key]][1]
        if (is.numeric(cell)) {
          val <- format_num_cell(cell, digits)
        } else {
          val <- as_cell_text(cell)
        }
      }
      row_vals <- c(row_vals, val)
    }
    rows[[length(rows) + 1]] <- row_vals
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

build_crosstabs_note_tokens <- function(column_keys) {
  percent_label_map <- c(pct_row = "Row %", pct_col = "Column %", pct_total = "Total %")
  percent_labels <- percent_label_map[names(percent_label_map) %in% column_keys]
  percent_labels_text <- paste(percent_labels, collapse = ", ")
  missing_note <- "Percentages are based on complete cases; missing values excluded."
  note_parts <- c(missing_note)
  if (nzchar(percent_labels_text)) {
    note_parts <- c(note_parts, paste("Reported:", percent_labels_text))
  }
  list(
    percent_labels = percent_labels_text,
    missing_note = missing_note,
    note_default = paste(note_parts, collapse = " ")
  )
}

build_crosstabs_narrative_rows <- function(tests_df, diagnostics_df, digits) {
  tests <- tests_df
  diagnostics <- diagnostics_df
  tests$group <- as.character(tests$group)
  diagnostics$group <- as.character(diagnostics$group)
  tests$group[is.na(tests$group)] <- "NA"
  diagnostics$group[is.na(diagnostics$group)] <- "NA"
  tests$row_var_display <- if ("row_var_label" %in% names(tests)) tests$row_var_label else tests$row_var
  tests$col_var_display <- if ("col_var_label" %in% names(tests)) tests$col_var_label else tests$col_var
  tests$group_display <- if ("group_label" %in% names(tests)) tests$group_label else tests$group
  diagnostics$row_var_display <- if ("row_var_label" %in% names(diagnostics)) diagnostics$row_var_label else diagnostics$row_var
  diagnostics$col_var_display <- if ("col_var_label" %in% names(diagnostics)) diagnostics$col_var_label else diagnostics$col_var
  diagnostics$group_display <- if ("group_label" %in% names(diagnostics)) diagnostics$group_label else diagnostics$group

  combos <- unique(tests[, c("row_var", "col_var", "group")])
  rows <- list()

  for (idx in seq_len(nrow(combos))) {
    row_var <- combos$row_var[idx]
    col_var <- combos$col_var[idx]
    group <- combos$group[idx]
    row <- tests[tests$row_var == row_var & tests$col_var == col_var & tests$group == group, , drop = FALSE]
    diag_row <- diagnostics[diagnostics$row_var == row_var & diagnostics$col_var == col_var & diagnostics$group == group, , drop = FALSE]

    row_label <- row$row_var_display[1]
    col_label <- row$col_var_display[1]
    group_label <- row$group_display[1]
    label <- if (group == "") {
      sprintf("%s by %s", row_label, col_label)
    } else {
      sprintf("Group %s, %s by %s", group_label, row_label, col_label)
    }

    valid_n <- row$valid_n[1]
    missing_n <- row$missing_n[1]
    missing_pct <- row$missing_pct[1]
    missing_text <- sprintf(
      "Missing = %s (%s%%)",
      ifelse(is.na(missing_n), "NA", as.character(missing_n)),
      ifelse(is.na(missing_pct), "NA", format_num(missing_pct, 1))
    )

    if (is.na(valid_n) || valid_n == 0) {
      line <- sprintf(
        "%s: no valid observations. Missing = %s (%s%%).",
        label,
        ifelse(is.na(missing_n), "NA", as.character(missing_n)),
        ifelse(is.na(missing_pct), "NA", format_num(missing_pct, 1))
      )
      rows[[length(rows) + 1]] <- list(
        label = label,
        row_var = row_label,
        col_var = col_label,
        group = group_label,
        valid_n = ifelse(is.na(valid_n), "NA", as.character(valid_n)),
        missing_n = ifelse(is.na(missing_n), "NA", as.character(missing_n)),
        missing_pct = ifelse(is.na(missing_pct), "NA", format_num(missing_pct, 1)),
        missing_text = missing_text,
        full_sentence = line
      )
      next
    }

    chisq_text <- ""
    effect_text <- ""
    if (!is.na(row$chi_square[1])) {
      chisq_text <- sprintf(
        "chi²(%s, N = %s) = %s, p %s",
        ifelse(is.na(row$chi_df[1]), "NA", as.character(row$chi_df[1])),
        as.character(valid_n),
        format_num(row$chi_square[1], digits),
        format_p(row$chi_p[1])
      )
      if (!is.na(row$phi[1])) {
        effect_text <- sprintf("phi = %s", format_num(row$phi[1], digits))
      } else if (!is.na(row$cramers_v[1])) {
        effect_text <- sprintf("Cramer's V = %s", format_num(row$cramers_v[1], digits))
      }
    }

    fisher_text <- ""
    if (!is.na(row$fisher_p[1])) {
      fisher_text <- sprintf("Fisher's exact test p %s", format_p(row$fisher_p[1]))
      if (!is.na(row$fisher_odds_ratio[1])) {
        fisher_text <- paste0(
          fisher_text,
          ", odds ratio = ",
          format_num(row$fisher_odds_ratio[1], digits)
        )
        if (!is.na(row$fisher_ci_low[1]) && !is.na(row$fisher_ci_high[1])) {
          fisher_text <- paste0(
            fisher_text,
            ", ", format(100 * row$fisher_conf_level[1], trim = TRUE), "% CI [",
            format_num(row$fisher_ci_low[1], digits),
            ", ",
            format_num(row$fisher_ci_high[1], digits),
            "]"
          )
        }
      }
    }

    tests_parts <- c(chisq_text, effect_text, fisher_text)
    tests_parts <- tests_parts[nzchar(tests_parts)]
    tests_text <- paste(tests_parts, collapse = "; ")

    expected_text <- ""
    if (nrow(diag_row) > 0 && !is.na(diag_row$min_expected[1])) {
      expected_text <- sprintf(
        "Expected counts: min = %s; %s%% of cells < 5",
        format_num(diag_row$min_expected[1], digits),
        format_num(diag_row$pct_expected_lt_5[1], 1)
      )
    }

    if (length(tests_parts) == 0) {
      line <- paste0(label, ": no association test computed")
      if (expected_text != "") {
        line <- paste0(line, ". ", expected_text)
      }
      line <- paste0(line, ". ", missing_text, ".")
    } else {
      line <- paste0(label, ": ", tests_text)
      if (expected_text != "") {
        line <- paste0(line, ". ", expected_text)
      }
      line <- paste0(line, ". ", missing_text, ".")
    }

    rows[[length(rows) + 1]] <- list(
      label = label,
      row_var = row_var,
      col_var = col_var,
      group = group,
      valid_n = ifelse(is.na(valid_n), "NA", as.character(valid_n)),
      missing_n = ifelse(is.na(missing_n), "NA", as.character(missing_n)),
      missing_pct = ifelse(is.na(missing_pct), "NA", format_num(missing_pct, 1)),
      chisq_text = chisq_text,
      effect_text = effect_text,
      fisher_text = fisher_text,
      tests_text = tests_text,
      expected_text = expected_text,
      missing_text = missing_text,
      full_sentence = line
    )
  }

  rows
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  opts <- nlss_run_options(args, "crosstabs")

  if (!is.null(opts$help)) {
    print_usage()
    quit(status = 0)
  }

  if (parse_bool(opts$interactive, FALSE)) {
    opts <- modifyList(opts, interactive_options())
  }

  digits_default <- get_config_value("defaults.digits", 2)
  log_default <- get_config_value("defaults.log", TRUE)
  percent_default <- get_config_value("modules.crosstabs.percent", "all")
  nlss_percent_default <- get_config_value("modules.crosstabs.nlss_percent", "row")
  chisq_default <- get_config_value("modules.crosstabs.chisq", TRUE)
  yates_default <- get_config_value("modules.crosstabs.yates", FALSE)
  fisher_default <- get_config_value("modules.crosstabs.fisher", FALSE)
  fisher_sim_default <- get_config_value("modules.crosstabs.fisher_simulate", FALSE)
  fisher_b_default <- get_config_value("modules.crosstabs.fisher_b", 2000)
  fisher_conf_default <- get_config_value("modules.crosstabs.fisher_conf_level", 0.95)
  expected_default <- get_config_value("modules.crosstabs.expected", TRUE)
  residuals_default <- get_config_value("modules.crosstabs.residuals", TRUE)
  digits <- if (!is.null(opts$digits)) as.numeric(opts$digits) else digits_default
  df <- nlss_load_input(opts)
  out_dir <- get_workspace_out_dir(df)
  nlss_begin_run("crosstabs", df, opts, out_dir)
  if (length(digits) != 1L || !is.finite(digits) || digits < 0 || digits > 15 || digits != floor(digits)) {
    stop("Digits must be an integer from 0 to 15.")
  }
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

  percent_flags <- parse_percent_flags(opts$percent, default = percent_default)
  nlss_percent <- normalize_nlss_percent(opts$`nlss-percent`, default = nlss_percent_default)

  options <- list(
    chisq = parse_bool(opts$chisq, default = chisq_default),
    yates = parse_bool(opts$yates, default = yates_default),
    fisher = parse_bool(opts$fisher, default = fisher_default),
    fisher_simulate = parse_bool(opts$`fisher-simulate`, default = fisher_sim_default),
    fisher_b = if (!is.null(opts$`fisher-b`)) as.numeric(opts$`fisher-b`) else fisher_b_default,
    fisher_conf_level = if (!is.null(opts$`fisher-conf-level`)) as.numeric(opts$`fisher-conf-level`) else fisher_conf_default,
    include_expected = parse_bool(opts$expected, default = expected_default),
    include_residuals = parse_bool(opts$residuals, default = residuals_default)
  )

  if (length(options$fisher_b) != 1L || !is.finite(options$fisher_b) ||
      options$fisher_b < 1 || options$fisher_b > .Machine$integer.max ||
      options$fisher_b != floor(options$fisher_b)) {
    stop("Fisher Monte Carlo replications must be a positive R integer.")
  }
  options$fisher_b <- as.integer(options$fisher_b)
  if (length(options$fisher_conf_level) != 1L || !is.finite(options$fisher_conf_level) ||
      options$fisher_conf_level <= 0 || options$fisher_conf_level >= 1) {
    stop("Fisher confidence level must be strictly between 0 and 1.")
  }
  options$seed <- nlss_run_seed(opts$seed, stochastic = options$fisher && options$fisher_simulate)
  if (!is.null(options$seed)) nlss_run_context$request$cli$seed <- options$seed
  label_meta <- resolve_label_metadata(df)
  groups <- if (is.null(group_var)) {
    list(list(group = "", value = NULL, is_missing = FALSE, row_indices = seq_len(nrow(df))))
  } else {
    group_vec <- df[[group_var]]
    missing_label <- nlss_missing_group_label(group_vec, label_meta, group_var)
    lapply(unique(group_vec), function(g) {
      is_missing <- is.na(g)
      list(group = if (is_missing) missing_label else as.character(g),
           value = if (is_missing) NULL else as.character(g), is_missing = is_missing,
           row_indices = which(if (is_missing) is.na(group_vec) else !is.na(group_vec) & group_vec == g))
    })
  }
  if (!length(groups)) stop("No observations available for grouped cross-tabulation.")
  table_design <- list()
  for (group in groups) {
    sub_df <- df[group$row_indices, , drop = FALSE]
    for (row_var in rows) for (col_var in cols) {
      complete <- !is.na(sub_df[[row_var]]) & !is.na(sub_df[[col_var]])
      table_design[[length(table_design) + 1L]] <- list(
        group = group$group, group_missing = group$is_missing, row_var = row_var, col_var = col_var,
        row_indices = group$row_indices[complete],
        row_levels = get_levels(sub_df[[row_var]]), col_levels = get_levels(sub_df[[col_var]]))
    }
  }
  resolved_options <- c(list(digits = digits, rows = rows, cols = cols, group = group_var,
                            percent = percent_flags, nlss_percent = nlss_percent), options)
  nlss_resolve_request(resolved_options, design = list(
    missing = "pairwise_complete_within_group", group_missing = "separate_group",
    rows = nrow(df), variable_types = lapply(df[vars_needed], class),
    groups = groups, tables = table_design))

  cells_list <- list()
  tests_list <- list()
  diag_list <- list()
  for (group in groups) {
    sub_df <- df[group$row_indices, , drop = FALSE]
    for (row_var in rows) for (col_var in cols) {
      result <- build_table(sub_df, row_var, col_var, group$group, options)
      result$cells$group_missing <- group$is_missing
      result$tests$group_missing <- group$is_missing
      result$diagnostics$group_missing <- group$is_missing
      cells_list[[length(cells_list) + 1L]] <- result$cells
      tests_list[[length(tests_list) + 1L]] <- result$tests
      diag_list[[length(diag_list) + 1L]] <- result$diagnostics
    }
  }

  cells_df <- do.call(rbind, cells_list)
  tests_df <- do.call(rbind, tests_list)
  diagnostics_df <- do.call(rbind, diag_list)
  cells_df <- attach_effect_sizes_to_cells(cells_df, tests_df)
  cells_df <- add_variable_label_column(cells_df, label_meta, var_col = "row_var")
  cells_df <- add_variable_label_column(cells_df, label_meta, var_col = "col_var")
  cells_df <- add_value_label_column(cells_df, label_meta, var_col = "row_var", value_col = "row_level")
  cells_df <- add_value_label_column(cells_df, label_meta, var_col = "col_var", value_col = "col_level")
  cells_df <- add_group_label_column(cells_df, label_meta, group_var, group_col = "group")
  tests_df <- add_variable_label_column(tests_df, label_meta, var_col = "row_var")
  tests_df <- add_variable_label_column(tests_df, label_meta, var_col = "col_var")
  tests_df <- add_group_label_column(tests_df, label_meta, group_var, group_col = "group")
  diagnostics_df <- add_variable_label_column(diagnostics_df, label_meta, var_col = "row_var")
  diagnostics_df <- add_variable_label_column(diagnostics_df, label_meta, var_col = "col_var")
  diagnostics_df <- add_group_label_column(diagnostics_df, label_meta, group_var, group_col = "group")
  if (!is.null(group_var)) {
    cells_df$group_label[cells_df$group_missing] <- cells_df$group[cells_df$group_missing]
    tests_df$group_label[tests_df$group_missing] <- tests_df$group[tests_df$group_missing]
    diagnostics_df$group_label[diagnostics_df$group_missing] <- diagnostics_df$group[diagnostics_df$group_missing]
  }

  percent_label <- if (!is.null(opts$percent) && opts$percent != "") opts$percent else percent_default
  use_group_template <- !is.null(group_var)
  template_override <- resolve_template_override(opts$template, module = "crosstabs")
  template_path <- if (!is.null(template_override)) {
    template_override
  } else if (use_group_template) {
    resolve_template_path("crosstabs.grouped", "crosstabs/grouped-template.md")
  } else {
    resolve_template_path("crosstabs.default", "crosstabs/default-template.md")
  }
  template_path <- nlss_freeze_template(template_path, "crosstabs.table")
  analysis_flags <- list(
    rows = rows,
    cols = cols,
    group = if (!is.null(group_var) && group_var != "") group_var else "None",
    percent = percent_label,
    "nlss-percent" = nlss_percent,
    chisq = options$chisq,
    yates = if (options$chisq) options$yates else NULL,
    fisher = options$fisher,
    "fisher-simulate" = if (options$fisher) options$fisher_simulate else NULL,
    "fisher-b" = if (options$fisher && options$fisher_simulate) options$fisher_b else NULL,
    seed = options$seed,
    "fisher-conf-level" = if (options$fisher) options$fisher_conf_level else NULL,
    "include-expected" = options$include_expected,
    "include-residuals" = options$include_residuals,
    digits = digits
  )

  report_percent <- list(row = nlss_percent %in% c("row", "all"),
                         col = nlss_percent %in% c("col", "all"),
                         total = nlss_percent %in% c("total", "all"))
  report_cells_df <- apply_cell_filters(cells_df, report_percent, options$include_expected, options$include_residuals)
  cells_df <- apply_cell_filters(cells_df, percent_flags, options$include_expected, options$include_residuals)
  results <- list(cells_df = cells_df, tests_df = tests_df, diagnostics_df = diagnostics_df,
                  report_cells_df = report_cells_df)
  nlss_set_result(results)
  nlss_report_path <- file.path(out_dir, "report_canonical.md")
  nlss_table <- format_nlss_table(
    report_cells_df,
    digits,
    nlss_percent,
    layout = "long",
    include_group = use_group_template
  )
  nlss_text <- format_nlss_text(tests_df, diagnostics_df, digits)
  template_meta <- get_template_meta(template_path)
  table_result <- build_crosstabs_table_body(report_cells_df, digits, template_meta$table)
  note_tokens <- build_crosstabs_note_tokens(table_result$columns)
  narrative_rows <- build_crosstabs_narrative_rows(tests_df, diagnostics_df, digits)
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
  nlss_stage_report(
    nlss_report_path,
    "Cross-tabulations",
    nlss_table,
    nlss_text,
    analysis_flags = analysis_flags,
    template_path = template_path,
    template_context = template_context
  )

  if (parse_bool(opts$log, default = log_default)) {
    ctx <- get_run_context()
    nlss_stage_log(
      out_dir,
      module = "crosstabs",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = results,
      options = resolved_options,
      user_prompt = get_user_prompt(opts)
    )
  }
}

nlss_run_main("crosstabs", main)
