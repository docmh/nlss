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

# Independent reference calculations: no NLSS code is sourced. Extract package
# results directly rather than reproducing adapter formulas for communalities.
run_case <- function(df, vars, group_var, method, rotation, n_factors_rule, n_factors_fixed,
                     eigen_threshold, cor_type, missing_method, loading_cutoff, sort_loadings) {
  set.seed(1)
  groups <- if (nzchar(group_var)) unique(df[[group_var]]) else ""
  summaries <- loadings <- eigen_rows <- list()
  for (g in groups) {
    rows <- if (!nzchar(group_var)) seq_len(nrow(df)) else
      if (is.na(g)) which(is.na(df[[group_var]])) else which(!is.na(df[[group_var]]) & df[[group_var]] == g)
    x <- df[rows, vars, drop = FALSE]
    x <- x[if (missing_method == "complete") complete.cases(x) else rowSums(!is.na(x)) > 0, , drop = FALSE]
    r <- switch(cor_type,
      pearson = cor(x, use = "pairwise.complete.obs"),
      spearman = cor(x, use = "pairwise.complete.obs", method = "spearman"),
      polychoric = psych::polychoric(x)$rho,
      tetrachoric = psych::tetrachoric(x)$rho)
    eig <- eigen(r, symmetric = TRUE, only.values = TRUE)$values
    k <- if (n_factors_rule == "fixed") n_factors_fixed else max(1, sum(eig > eigen_threshold))
    fit <- if (method == "pca") psych::principal(r, nfactors = k, rotate = rotation, scores = FALSE, n.obs = nrow(x)) else
      psych::fa(r, nfactors = k, rotate = rotation, fm = method, n.obs = nrow(x))
    L <- unclass(fit$loadings)[vars, , drop = FALSE]
    primary <- max.col(abs(L), ties.method = "first")
    h2 <- fit$communality[vars]
    label <- if (is.na(g)) "NA" else as.character(g)
    ld <- data.frame(item = vars, factor = paste0("F", primary),
      loading = L[cbind(seq_along(vars), primary)], h2 = unname(h2),
      u2 = unname(fit$uniquenesses[vars]), complexity = unname(fit$complexity[vars]),
      group = label)
    if (sort_loadings) ld <- ld[order(ld$factor, -abs(ld$loading), ld$item), ]
    kmo <- tryCatch(psych::KMO(r)$MSA, error = function(e) NA_real_)
    bart <- tryCatch(psych::cortest.bartlett(r, n = nrow(x)), error = function(e) list())
    summaries[[length(summaries) + 1L]] <- data.frame(group = label, n_obs = nrow(x), n_items = length(vars),
      n_factors = k, method = method, rotation = rotation, cor = cor_type, missing = missing_method,
      eigen_threshold = eigen_threshold, kmo = kmo,
      bartlett_chi2 = if (is.null(bart$chisq)) NA_real_ else bart$chisq,
      bartlett_df = if (is.null(bart$df)) NA_real_ else bart$df,
      bartlett_p = if (is.null(bart$p.value)) NA_real_ else bart$p.value,
      variance_explained = sum(h2) / length(vars))
    loadings[[length(loadings) + 1L]] <- ld
    eigen_rows[[length(eigen_rows) + 1L]] <- data.frame(group = label, component = seq_along(eig),
      eigenvalue = eig, proportion = eig / length(vars), cumulative = cumsum(eig / length(vars)))
  }
  list(summary_df = do.call(rbind, summaries), loadings_df = do.call(rbind, loadings),
    eigen_df = do.call(rbind, eigen_rows))
}

base_vars <- c("f1_1", "f1_2", "f1_3_rev", "f1_4", "f2_1", "f2_2", "f2_3", "f2_4_rev")

cases <- list(
  list(case_id = "efa_uls_oblimin", dataset = "golden", vars = base_vars, group = "",
    method = "uls", rotation = "oblimin", n_factors = 2, eigen_threshold = 1,
    cor = "pearson", missing = "complete", loading_cutoff = .3, sort_loadings = TRUE),
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
