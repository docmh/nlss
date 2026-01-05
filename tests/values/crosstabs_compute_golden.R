#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0

args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(flag, default = NULL) {
  idx <- which(args == flag)
  if (length(idx) == 0) return(default)
  if (idx[length(idx)] + 1 > length(args)) return(default)
  args[idx[length(idx)] + 1]
}

trim_arg <- function(x) {
  if (is.null(x)) return(NULL)
  x <- trimws(x)
  if (x == "") return(NULL)
  x
}

options(scipen = 999, digits = 15)

data_path <- trim_arg(get_arg("--data", file.path("tests", "data", "golden_dataset.csv")))
out_tests <- trim_arg(get_arg("--out", file.path("tests", "values", "crosstabs_tests_golden.csv")))
out_cells <- trim_arg(get_arg("--cells-out", file.path("tests", "values", "crosstabs_cells_golden.csv")))
out_diag <- trim_arg(get_arg("--diagnostics-out", file.path("tests", "values", "crosstabs_diagnostics_golden.csv")))

if (is.null(data_path) || !file.exists(data_path)) {
  stop("Missing --data path.")
}
if (is.null(out_tests) || is.null(out_cells) || is.null(out_diag)) {
  stop("Missing output path(s).")
}

df <- read.csv(data_path, stringsAsFactors = FALSE)

get_levels <- function(vec) {
  if (is.factor(vec)) return(as.character(levels(vec)))
  values <- unique(vec[!is.na(vec)])
  if (length(values) == 0) return(character(0))
  if (is.numeric(values)) return(as.character(sort(values)))
  as.character(sort(values))
}

compute_table <- function(df_sub, row_var, col_var, group_label, opts) {
  row_vec <- df_sub[[row_var]]
  col_vec <- df_sub[[col_var]]
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
    stop("No valid data for crosstabs golden computation.")
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
  yates_applied <- FALSE

  if (isTRUE(opts$chisq) && valid_n > 0 && nrow(tab_test) >= 2 && ncol(tab_test) >= 2) {
    correct_flag <- isTRUE(opts$yates) && nrow(tab_test) == 2 && ncol(tab_test) == 2
    chisq_res <- suppressWarnings(chisq.test(tab_test, correct = correct_flag))
    chi_square <- unname(chisq_res$statistic)
    chi_df <- unname(chisq_res$parameter)
    chi_p <- chisq_res$p.value
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

  if (isTRUE(opts$fisher) && valid_n > 0 && nrow(tab_test) >= 2 && ncol(tab_test) >= 2) {
    fisher_res <- tryCatch(
      fisher.test(
        tab_test,
        simulate.p.value = isTRUE(opts$fisher_simulate),
        B = opts$fisher_b,
        conf.level = opts$fisher_conf_level
      ),
      error = function(e) NULL
    )
    if (!is.null(fisher_res)) {
      fisher_p <- fisher_res$p.value
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
    fisher_simulated = isTRUE(opts$fisher_simulate),
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

cases <- list(
  list(
    id = "gender_group3",
    row_var = "gender",
    col_var = "group3",
    group_label = "",
    data = df,
    options = list(
      chisq = TRUE,
      yates = FALSE,
      fisher = FALSE,
      fisher_simulate = FALSE,
      fisher_b = 2000,
      fisher_conf_level = 0.95
    )
  ),
  list(
    id = "binary_group2_yates_fisher",
    row_var = "binary_outcome",
    col_var = "group2",
    group_label = "",
    data = df,
    options = list(
      chisq = TRUE,
      yates = TRUE,
      fisher = TRUE,
      fisher_simulate = FALSE,
      fisher_b = 2000,
      fisher_conf_level = 0.95
    )
  ),
  list(
    id = "catvar_catvar2_group_control",
    row_var = "cat_var",
    col_var = "cat_var2",
    group_label = "control",
    data = df[df$group2 == "control", , drop = FALSE],
    options = list(
      chisq = TRUE,
      yates = FALSE,
      fisher = FALSE,
      fisher_simulate = FALSE,
      fisher_b = 2000,
      fisher_conf_level = 0.95
    )
  )
)

case_lookup <- setNames(cases, vapply(cases, function(x) x$id, character(1)))

tests_rows <- list()
diag_rows <- list()

for (case in cases) {
  res <- compute_table(case$data, case$row_var, case$col_var, case$group_label, case$options)
  tests <- res$tests
  tests$case_id <- case$id
  tests$opt_chisq <- case$options$chisq
  tests$opt_yates <- case$options$yates
  tests$opt_fisher <- case$options$fisher
  tests$opt_fisher_simulate <- case$options$fisher_simulate
  tests_rows[[length(tests_rows) + 1]] <- tests

  diag <- res$diagnostics
  diag$case_id <- case$id
  diag_rows[[length(diag_rows) + 1]] <- diag
}

tests_df <- do.call(rbind, tests_rows)
diag_df <- do.call(rbind, diag_rows)

cell_cases <- list(
  list(case_id = "gender_group3_F_A", case_ref = "gender_group3", row_level = "F", col_level = "A"),
  list(case_id = "binary_group2_1_treatment", case_ref = "binary_group2_yates_fisher", row_level = "1", col_level = "treatment"),
  list(case_id = "catvar_catvar2_control_cat1_high", case_ref = "catvar_catvar2_group_control", row_level = "cat1", col_level = "high")
)

cells_rows <- list()
for (cell_case in cell_cases) {
  case <- case_lookup[[cell_case$case_ref]]
  res <- compute_table(case$data, case$row_var, case$col_var, case$group_label, case$options)
  cells <- res$cells
  match_idx <- cells$row_level == cell_case$row_level & cells$col_level == cell_case$col_level
  if (!any(match_idx)) {
    stop(paste("Missing cell for", cell_case$case_id))
  }
  cell_row <- cells[match_idx, , drop = FALSE]
  cell_row$case_id <- cell_case$case_id
  cells_rows[[length(cells_rows) + 1]] <- cell_row
}

cells_df <- do.call(rbind, cells_rows)

cells_df <- cells_df[, c(
  "case_id", "row_var", "col_var", "group", "row_level", "col_level",
  "n", "pct_row", "pct_col", "pct_total", "expected", "std_resid", "adj_resid"
)]

order_tests <- c(
  "case_id", "row_var", "col_var", "group", "valid_n", "total_n", "missing_n",
  "missing_pct", "row_missing_n", "col_missing_n", "both_missing_n", "n_rows",
  "n_cols", "chi_square", "chi_df", "chi_p", "fisher_p", "fisher_odds_ratio",
  "fisher_ci_low", "fisher_ci_high", "phi", "cramers_v", "contingency_c",
  "yates_applied", "fisher_simulated", "opt_chisq", "opt_yates", "opt_fisher",
  "opt_fisher_simulate"
)

tests_df <- tests_df[, order_tests]

diag_df <- diag_df[, c(
  "case_id", "row_var", "col_var", "group", "valid_n", "n_cells",
  "min_expected", "n_expected_lt_1", "n_expected_lt_5", "pct_expected_lt_5"
)]

write.csv(tests_df, out_tests, row.names = FALSE, na = "NA")
write.csv(cells_df, out_cells, row.names = FALSE, na = "NA")
write.csv(diag_df, out_diag, row.names = FALSE, na = "NA")

cat("Wrote:\n")
cat("- ", out_tests, "\n", sep = "")
cat("- ", out_cells, "\n", sep = "")
cat("- ", out_diag, "\n", sep = "")
