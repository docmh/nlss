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

data_path <- trim_arg(get_arg("--data", file.path("tests", "data", "golden_dataset.csv")))
out_summary <- trim_arg(get_arg("--summary-out", file.path("tests", "values", "efa_summary_golden.csv")))
out_loadings <- trim_arg(get_arg("--loadings-out", file.path("tests", "values", "efa_loadings_golden.csv")))
out_eigen <- trim_arg(get_arg("--eigen-out", file.path("tests", "values", "efa_eigen_golden.csv")))

if (is.null(data_path) || !file.exists(data_path)) {
  stop("Missing --data path.")
}
if (is.null(out_summary) || is.null(out_loadings) || is.null(out_eigen)) {
  stop("Missing output path(s).")
}
if (!requireNamespace("psych", quietly = TRUE)) {
  stop("Missing psych package.")
}

options(scipen = 999, digits = 15)

main_df <- read.csv(data_path, stringsAsFactors = FALSE)

poly_text <- "p1,p2,p3,p4\n1,2,3,4\n2,3,4,1\n3,4,2,2\n4,1,1,3\n2,2,3,4\n3,3,4,1\n4,4,1,2\n1,1,2,3\n2,3,3,4\n3,2,4,1\n4,3,1,2\n1,4,2,3\n"
poly_df <- read.csv(text = poly_text, stringsAsFactors = FALSE)

tetra_text <- "b1,b2,b3\n0,1,0\n1,1,0\n0,0,1\n1,0,1\n0,1,1\n1,0,0\n0,0,0\n1,1,1\n0,1,0\n1,0,1\n"
tetra_df <- read.csv(text = tetra_text, stringsAsFactors = FALSE)

calc_cor_matrix <- function(df, vars, cor_type, missing_method) {
  data <- df[, vars, drop = FALSE]
  if (cor_type %in% c("pearson", "spearman")) {
    if (missing_method == "complete") {
      data <- data[complete.cases(data), , drop = FALSE]
      if (nrow(data) < 2) stop("Not enough rows for correlation matrix.")
      cor_mat <- suppressWarnings(cor(data, use = "complete.obs", method = cor_type))
      n_obs <- nrow(data)
    } else {
      if (nrow(data) < 2) stop("Not enough rows for correlation matrix.")
      cor_mat <- suppressWarnings(cor(data, use = "pairwise.complete.obs", method = cor_type))
      n_obs <- nrow(data)
    }
    return(list(cor_mat = cor_mat, n_obs = n_obs))
  }

  if (missing_method == "complete") {
    data <- data[complete.cases(data), , drop = FALSE]
  }
  if (nrow(data) < 2) stop("Not enough rows for correlation matrix.")

  if (cor_type == "polychoric") {
    res <- psych::polychoric(data)
    return(list(cor_mat = res$rho, n_obs = nrow(data)))
  }
  if (cor_type == "tetrachoric") {
    res <- psych::tetrachoric(data)
    return(list(cor_mat = res$rho, n_obs = nrow(data)))
  }
  stop("Unsupported correlation type.")
}

determine_n_factors <- function(eigenvalues, rule, fixed_n, threshold, max_n) {
  if (length(eigenvalues) == 0) return(NA_real_)
  if (rule == "fixed" && !is.na(fixed_n)) {
    n_val <- as.numeric(fixed_n)
  } else {
    n_val <- sum(eigenvalues > threshold)
  }
  if (is.na(n_val) || n_val < 1) n_val <- 1
  if (!is.null(max_n) && n_val > max_n) n_val <- max_n
  n_val
}

build_loadings_df <- function(loadings, h2, u2, complexity, group_label, loading_cutoff, sort_loadings) {
  item_names <- rownames(loadings)
  if (is.null(item_names) || length(item_names) == 0) {
    item_names <- paste0("Item", seq_len(nrow(loadings)))
  }
  factor_names <- colnames(loadings)
  abs_loadings <- abs(loadings)
  primary_idx <- apply(abs_loadings, 1, function(x) {
    if (all(is.na(x))) return(NA_integer_)
    which.max(x)
  })
  primary_factor <- vapply(primary_idx, function(i) ifelse(is.na(i), NA_character_, factor_names[i]), character(1))
  primary_loading <- vapply(seq_len(nrow(loadings)), function(i) {
    idx <- primary_idx[i]
    if (is.na(idx)) return(NA_real_)
    loadings[i, idx]
  }, numeric(1))

  df <- data.frame(
    item = item_names,
    factor = primary_factor,
    loading = primary_loading,
    h2 = h2,
    u2 = u2,
    complexity = complexity,
    group = group_label,
    stringsAsFactors = FALSE
  )

  if (sort_loadings) {
    order_idx <- order(df$factor, -abs(df$loading), df$item, na.last = TRUE)
    df <- df[order_idx, , drop = FALSE]
  }

  df
}

build_eigen_df <- function(eigenvalues, n_items, group_label) {
  if (length(eigenvalues) == 0) return(data.frame())
  proportion <- eigenvalues / n_items
  cumulative <- cumsum(proportion)
  data.frame(
    group = group_label,
    component = seq_along(eigenvalues),
    eigenvalue = eigenvalues,
    proportion = proportion,
    cumulative = cumulative,
    stringsAsFactors = FALSE
  )
}

build_summary_row <- function(group_label, n_obs, n_items, n_factors, method, rotation, cor_type, missing_method, kmo_val, bartlett, variance_explained, eigen_threshold) {
  data.frame(
    group = group_label,
    n_obs = n_obs,
    n_items = n_items,
    n_factors = n_factors,
    method = method,
    rotation = rotation,
    cor = cor_type,
    missing = missing_method,
    eigen_threshold = eigen_threshold,
    kmo = kmo_val,
    bartlett_chi2 = ifelse(is.null(bartlett$chisq), NA_real_, bartlett$chisq),
    bartlett_df = ifelse(is.null(bartlett$df), NA_real_, bartlett$df),
    bartlett_p = ifelse(is.null(bartlett$p.value), NA_real_, bartlett$p.value),
    variance_explained = variance_explained,
    stringsAsFactors = FALSE
  )
}

run_case <- function(df, vars, group_var, method, rotation, n_factors_rule, n_factors_fixed, eigen_threshold, cor_type, missing_method, loading_cutoff, sort_loadings) {
  loadings_list <- list()
  eigen_list <- list()
  summary_list <- list()

  group_values <- if (!is.null(group_var) && nzchar(group_var)) unique(df[[group_var]]) else NA

  for (group_value in group_values) {
    if (!is.null(group_var) && nzchar(group_var)) {
      df_group <- df[df[[group_var]] == group_value, , drop = FALSE]
      group_label <- ifelse(is.na(group_value), "NA", as.character(group_value))
    } else {
      df_group <- df
      group_label <- ""
    }

    cor_res <- calc_cor_matrix(df_group, vars, cor_type, missing_method)
    cor_mat <- cor_res$cor_mat
    n_obs <- cor_res$n_obs

    if (!is.matrix(cor_mat) || nrow(cor_mat) < 2) {
      stop("Correlation matrix could not be computed.")
    }
    if (any(!is.finite(cor_mat))) {
      stop("Correlation matrix contains non-finite values.")
    }

    eigenvalues <- suppressWarnings(eigen(cor_mat, symmetric = TRUE, only.values = TRUE)$values)
    n_items <- length(vars)
    n_factors <- determine_n_factors(eigenvalues, n_factors_rule, n_factors_fixed, eigen_threshold, n_items)

    efa_res <- if (method == "pca") {
      psych::principal(r = cor_mat, nfactors = n_factors, rotate = rotation, scores = FALSE)
    } else {
      psych::fa(r = cor_mat, nfactors = n_factors, rotate = rotation, fm = method, n.obs = n_obs)
    }

    loadings <- as.matrix(efa_res$loadings)
    if (nrow(loadings) == 0) {
      stop("Loadings could not be computed.")
    }

    if (!is.null(rownames(loadings))) {
      row_order <- match(vars, rownames(loadings))
      if (all(!is.na(row_order))) {
        loadings <- loadings[row_order, , drop = FALSE]
      }
    }

    factor_names <- paste0("F", seq_len(ncol(loadings)))
    colnames(loadings) <- factor_names

    h2 <- rowSums(loadings^2, na.rm = TRUE)
    u2 <- 1 - h2
    complexity <- if (!is.null(efa_res$complexity)) {
      as.numeric(efa_res$complexity)
    } else {
      rep(NA_real_, length(h2))
    }

    loadings_df <- build_loadings_df(loadings, h2, u2, complexity, group_label, loading_cutoff, sort_loadings)
    eigen_df <- build_eigen_df(eigenvalues, n_items, group_label)

    kmo_val <- NA_real_
    bartlett <- list()
    kmo_res <- tryCatch(psych::KMO(cor_mat), error = function(e) NULL)
    if (!is.null(kmo_res) && !is.null(kmo_res$MSA)) {
      kmo_val <- as.numeric(kmo_res$MSA)
    }
    bartlett_res <- tryCatch(psych::cortest.bartlett(cor_mat, n = n_obs), error = function(e) NULL)
    if (!is.null(bartlett_res)) {
      bartlett <- bartlett_res
    }

    variance_explained <- sum(eigenvalues[seq_len(n_factors)]) / n_items

    summary_row <- build_summary_row(
      group_label = group_label,
      n_obs = n_obs,
      n_items = n_items,
      n_factors = n_factors,
      method = method,
      rotation = rotation,
      cor_type = cor_type,
      missing_method = missing_method,
      kmo_val = kmo_val,
      bartlett = bartlett,
      variance_explained = variance_explained,
      eigen_threshold = eigen_threshold
    )

    loadings_list[[length(loadings_list) + 1]] <- loadings_df
    eigen_list[[length(eigen_list) + 1]] <- eigen_df
    summary_list[[length(summary_list) + 1]] <- summary_row
  }

  list(
    loadings_df = do.call(rbind, loadings_list),
    eigen_df = do.call(rbind, eigen_list),
    summary_df = do.call(rbind, summary_list)
  )
}

base_vars <- c("f1_1", "f1_2", "f1_3_rev", "f1_4", "f2_1", "f2_2", "f2_3", "f2_4_rev")

cases <- list(
  list(
    case_id = "efa_default_pca_eigen",
    dataset = "golden",
    vars = base_vars,
    group = "",
    method = "pca",
    rotation = "varimax",
    n_factors = "eigen",
    eigen_threshold = 1,
    cor = "pearson",
    missing = "complete",
    loading_cutoff = 0.3,
    sort_loadings = TRUE
  ),
  list(
    case_id = "efa_pa_eigen_threshold",
    dataset = "golden",
    vars = base_vars,
    group = "",
    method = "pa",
    rotation = "none",
    n_factors = "eigen",
    eigen_threshold = 1.2,
    cor = "pearson",
    missing = "complete",
    loading_cutoff = 0.3,
    sort_loadings = TRUE
  ),
  list(
    case_id = "efa_minres_fixed_spearman_pairwise",
    dataset = "golden",
    vars = base_vars,
    group = "",
    method = "minres",
    rotation = "varimax",
    n_factors = 2,
    eigen_threshold = 1,
    cor = "spearman",
    missing = "pairwise",
    loading_cutoff = 0.4,
    sort_loadings = FALSE
  ),
  list(
    case_id = "efa_grouped_fixed_group3",
    dataset = "golden",
    vars = c("f1_1", "f1_2", "f1_3_rev", "f1_4"),
    group = "group3",
    method = "pca",
    rotation = "varimax",
    n_factors = 2,
    eigen_threshold = 1,
    cor = "pearson",
    missing = "complete",
    loading_cutoff = 0.3,
    sort_loadings = TRUE
  ),
  list(
    case_id = "efa_polychoric_fixed",
    dataset = "poly",
    vars = c("p1", "p2", "p3", "p4"),
    group = "",
    method = "pca",
    rotation = "varimax",
    n_factors = 2,
    eigen_threshold = 1,
    cor = "polychoric",
    missing = "complete",
    loading_cutoff = 0.3,
    sort_loadings = TRUE
  ),
  list(
    case_id = "efa_tetrachoric_fixed",
    dataset = "tetra",
    vars = c("b1", "b2", "b3"),
    group = "",
    method = "pca",
    rotation = "none",
    n_factors = 1,
    eigen_threshold = 1,
    cor = "tetrachoric",
    missing = "complete",
    loading_cutoff = 0.3,
    sort_loadings = TRUE
  )
)

datasets <- list(
  golden = main_df,
  poly = poly_df,
  tetra = tetra_df
)

summary_rows <- list()
loadings_rows <- list()
eigen_rows <- list()

for (case in cases) {
  df <- datasets[[case$dataset]]
  if (is.null(df)) stop(paste("Unknown dataset", case$dataset))

  n_rule <- if (is.character(case$n_factors) && tolower(case$n_factors) == "eigen") "eigen" else "fixed"
  n_fixed <- if (n_rule == "fixed") as.numeric(case$n_factors) else NA_real_

  result <- run_case(
    df,
    case$vars,
    case$group,
    case$method,
    case$rotation,
    n_rule,
    n_fixed,
    case$eigen_threshold,
    case$cor,
    case$missing,
    case$loading_cutoff,
    case$sort_loadings
  )

  summary_df <- result$summary_df
  summary_df$case_id <- case$case_id
  summary_df$group_var <- case$group
  summary_df$n_factors_rule <- n_rule
  summary_df$n_factors_option <- n_fixed
  summary_rows[[length(summary_rows) + 1]] <- summary_df

  loadings_df <- result$loadings_df
  loadings_df$case_id <- case$case_id
  loadings_rows[[length(loadings_rows) + 1]] <- loadings_df

  eigen_df <- result$eigen_df
  eigen_df$case_id <- case$case_id
  eigen_rows[[length(eigen_rows) + 1]] <- eigen_df
}

summary_out <- do.call(rbind, summary_rows)
loadings_out <- do.call(rbind, loadings_rows)
eigen_out <- do.call(rbind, eigen_rows)

summary_out <- summary_out[, c(
  "case_id",
  "group_var",
  "group",
  "n_obs",
  "n_items",
  "n_factors",
  "method",
  "rotation",
  "cor",
  "missing",
  "n_factors_rule",
  "n_factors_option",
  "eigen_threshold",
  "kmo",
  "bartlett_chi2",
  "bartlett_df",
  "bartlett_p",
  "variance_explained"
)]

loadings_out <- loadings_out[, c(
  "case_id",
  "group",
  "item",
  "factor",
  "loading",
  "h2",
  "u2",
  "complexity"
)]

eigen_out <- eigen_out[, c(
  "case_id",
  "group",
  "component",
  "eigenvalue",
  "proportion",
  "cumulative"
)]

write.csv(summary_out, out_summary, row.names = FALSE)
write.csv(loadings_out, out_loadings, row.names = FALSE)
write.csv(eigen_out, out_eigen, row.names = FALSE)
