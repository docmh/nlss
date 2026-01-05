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
item_out <- trim_arg(get_arg("--item-out", file.path("tests", "values", "scale_item_golden.csv")))
rel_out <- trim_arg(get_arg("--reliability-out", file.path("tests", "values", "scale_reliability_golden.csv")))

if (is.null(data_path) || !file.exists(data_path)) {
  stop("Missing --data path.")
}
if (is.null(item_out) || is.null(rel_out)) {
  stop("Missing output path(s).")
}

options(scipen = 999, digits = 15)

df <- read.csv(data_path, stringsAsFactors = FALSE)

coerce_numeric_df <- function(df_use, vars) {
  for (var in vars) {
    if (!var %in% names(df_use)) next
    df_use[[var]] <- suppressWarnings(as.numeric(as.character(df_use[[var]])))
  }
  df_use
}

reverse_items <- function(df_use, items, reverse_items, reverse_min = NULL, reverse_max = NULL) {
  if (length(reverse_items) == 0) {
    return(list(df = df_use, info = list(items = character(0), method = "none")))
  }
  reverse_items <- intersect(reverse_items, items)
  method <- "fixed"
  if (is.null(reverse_min) || is.null(reverse_max) || is.na(reverse_min) || is.na(reverse_max)) {
    method <- "observed"
  }
  per_item <- data.frame(item = character(0), min = numeric(0), max = numeric(0), stringsAsFactors = FALSE)
  for (item in reverse_items) {
    vec <- df_use[[item]]
    if (method == "observed") {
      min_val <- suppressWarnings(min(vec, na.rm = TRUE))
      max_val <- suppressWarnings(max(vec, na.rm = TRUE))
      if (!is.finite(min_val) || !is.finite(max_val)) {
        min_val <- NA_real_
        max_val <- NA_real_
      }
    } else {
      min_val <- reverse_min
      max_val <- reverse_max
    }
    if (!is.na(min_val) && !is.na(max_val) && is.finite(min_val) && is.finite(max_val)) {
      df_use[[item]] <- max_val + min_val - vec
    }
    per_item <- rbind(
      per_item,
      data.frame(item = item, min = min_val, max = max_val, stringsAsFactors = FALSE)
    )
  }
  list(
    df = df_use,
    info = list(
      items = reverse_items,
      method = method,
      reverse_min = reverse_min,
      reverse_max = reverse_max,
      per_item = per_item
    )
  )
}

compute_alpha <- function(cov_mat) {
  if (is.null(cov_mat) || !is.matrix(cov_mat)) return(NA_real_)
  k <- ncol(cov_mat)
  if (k < 2) return(NA_real_)
  if (any(is.na(cov_mat))) return(NA_real_)
  total_var <- sum(cov_mat)
  sum_item_var <- sum(diag(cov_mat))
  if (is.na(total_var) || total_var <= 0) return(NA_real_)
  if (is.na(sum_item_var) || sum_item_var <= 0) return(NA_real_)
  (k / (k - 1)) * (1 - sum_item_var / total_var)
}

compute_alpha_std <- function(k, r_bar) {
  if (is.na(r_bar) || k < 2) return(NA_real_)
  denom <- 1 + (k - 1) * r_bar
  if (is.na(denom) || denom == 0) return(NA_real_)
  (k * r_bar) / denom
}

compute_item_total_r <- function(cov_mat, item_name, corrected = FALSE) {
  if (is.null(cov_mat) || !is.matrix(cov_mat)) return(NA_real_)
  idx <- match(item_name, colnames(cov_mat))
  if (is.na(idx)) return(NA_real_)
  if (any(is.na(cov_mat[idx, ]))) return(NA_real_)
  var_i <- cov_mat[idx, idx]
  if (is.na(var_i) || var_i <= 0) return(NA_real_)
  cov_i_total <- sum(cov_mat[idx, ])
  total_var <- sum(cov_mat)
  if (is.na(total_var) || total_var <= 0) return(NA_real_)
  if (corrected) {
    cov_i_rest <- cov_i_total - var_i
    var_rest <- total_var - 2 * cov_i_total + var_i
    if (is.na(var_rest) || var_rest <= 0) return(NA_real_)
    return(cov_i_rest / sqrt(var_i * var_rest))
  }
  cov_i_total / sqrt(var_i * total_var)
}

compute_alpha_if_deleted <- function(cov_mat, item_name) {
  if (is.null(cov_mat) || !is.matrix(cov_mat)) return(NA_real_)
  idx <- match(item_name, colnames(cov_mat))
  if (is.na(idx)) return(NA_real_)
  if (ncol(cov_mat) <= 2) return(NA_real_)
  sub_mat <- cov_mat[-idx, -idx, drop = FALSE]
  compute_alpha(sub_mat)
}

compute_r_bar <- function(cor_mat) {
  if (is.null(cor_mat) || !is.matrix(cor_mat)) {
    return(list(r_bar = NA_real_, r_min = NA_real_, r_max = NA_real_))
  }
  vals <- cor_mat[lower.tri(cor_mat)]
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) {
    return(list(r_bar = NA_real_, r_min = NA_real_, r_max = NA_real_))
  }
  list(r_bar = mean(vals), r_min = min(vals), r_max = max(vals))
}

compute_omega_total <- function(cor_mat, n_obs) {
  if (is.null(cor_mat) || !is.matrix(cor_mat)) {
    return(list(value = NA_real_, status = "correlation_missing"))
  }
  k <- ncol(cor_mat)
  if (k < 3) return(list(value = NA_real_, status = "insufficient_items"))
  if (any(is.na(cor_mat))) return(list(value = NA_real_, status = "correlation_missing"))
  if (is.null(n_obs) || is.na(n_obs) || n_obs < 3) {
    return(list(value = NA_real_, status = "insufficient_n"))
  }
  res <- tryCatch(
    factanal(covmat = list(cov = cor_mat, n.obs = n_obs), factors = 1, rotation = "none"),
    error = function(e) NULL
  )
  if (is.null(res)) return(list(value = NA_real_, status = "factanal_failed"))
  loadings <- as.numeric(res$loadings[, 1])
  uniq <- res$uniquenesses
  if (any(is.na(loadings)) || any(is.na(uniq))) {
    return(list(value = NA_real_, status = "loadings_missing"))
  }
  numerator <- (sum(loadings))^2
  denom <- numerator + sum(uniq)
  if (is.na(denom) || denom <= 0) return(list(value = NA_real_, status = "invalid_denominator"))
  list(value = numerator / denom, status = "ok")
}

build_item_stats <- function(items_df, cov_mat, group_label, total_n) {
  items <- names(items_df)
  rows <- list()
  for (item in items) {
    vec <- items_df[[item]]
    missing_n <- sum(is.na(vec))
    missing_pct <- ifelse(total_n > 0, missing_n / total_n * 100, NA_real_)
    valid <- vec[!is.na(vec)]
    n <- length(valid)
    mean_val <- if (n > 0) mean(valid) else NA_real_
    sd_val <- if (n > 1) sd(valid) else NA_real_
    min_val <- if (n > 0) min(valid) else NA_real_
    max_val <- if (n > 0) max(valid) else NA_real_
    item_total_r <- compute_item_total_r(cov_mat, item, corrected = FALSE)
    item_rest_r <- compute_item_total_r(cov_mat, item, corrected = TRUE)
    alpha_if_deleted <- compute_alpha_if_deleted(cov_mat, item)
    rows[[length(rows) + 1]] <- data.frame(
      item = item,
      group = group_label,
      n = n,
      missing_n = missing_n,
      missing_pct = missing_pct,
      mean = mean_val,
      sd = sd_val,
      min = min_val,
      max = max_val,
      item_total_r = item_total_r,
      item_rest_r = item_rest_r,
      alpha_if_deleted = alpha_if_deleted,
      stringsAsFactors = FALSE
    )
  }
  df_out <- do.call(rbind, rows)
  numeric_cols <- setdiff(names(df_out), c("item", "group"))
  for (col in numeric_cols) {
    df_out[[col]] <- as.numeric(df_out[[col]])
  }
  df_out
}

compute_reliability_summary <- function(items_df, cov_mat, cor_mat, group_label, score_method, omega_flag) {
  items <- names(items_df)
  k <- length(items)
  total_n <- nrow(items_df)
  complete_idx <- complete.cases(items_df)
  n_complete <- sum(complete_idx)
  missing_n <- total_n - n_complete
  missing_pct <- ifelse(total_n > 0, missing_n / total_n * 100, NA_real_)

  scores <- numeric(0)
  if (n_complete > 0) {
    sub_df <- items_df[complete_idx, , drop = FALSE]
    if (score_method == "mean") {
      scores <- rowMeans(sub_df)
    } else {
      scores <- rowSums(sub_df)
    }
  }

  score_mean <- if (length(scores) > 0) mean(scores) else NA_real_
  score_sd <- if (length(scores) > 1) sd(scores) else NA_real_
  score_min <- if (length(scores) > 0) min(scores) else NA_real_
  score_max <- if (length(scores) > 0) max(scores) else NA_real_

  alpha <- compute_alpha(cov_mat)
  r_stats <- compute_r_bar(cor_mat)
  alpha_std <- compute_alpha_std(k, r_stats$r_bar)

  omega_info <- list(value = NA_real_, status = "disabled")
  if (omega_flag) {
    omega_info <- compute_omega_total(cor_mat, n_complete)
  }

  data.frame(
    group = group_label,
    n_items = k,
    n_total = total_n,
    n_complete = n_complete,
    missing_n = missing_n,
    missing_pct = missing_pct,
    alpha = alpha,
    alpha_std = alpha_std,
    omega_total = omega_info$value,
    omega_status = omega_info$status,
    r_bar = r_stats$r_bar,
    r_min = r_stats$r_min,
    r_max = r_stats$r_max,
    score_method = score_method,
    score_mean = score_mean,
    score_sd = score_sd,
    score_min = score_min,
    score_max = score_max,
    stringsAsFactors = FALSE
  )
}

run_case <- function(df_use, case_name, vars, group_var = NULL, reverse_items = character(0),
                     reverse_min = NULL, reverse_max = NULL, missing = "pairwise",
                     score = "sum", omega = TRUE) {
  df_case <- df_use
  df_case <- coerce_numeric_df(df_case, vars)
  rev_result <- reverse_items(df_case, vars, reverse_items, reverse_min, reverse_max)
  df_case <- rev_result$df

  item_rows <- list()
  rel_rows <- list()

  append_rows <- function(items_df, group_label) {
    use_method <- if (missing == "pairwise") "pairwise.complete.obs" else "complete.obs"
    cov_mat <- tryCatch(cov(items_df, use = use_method), error = function(e) NULL)
    cor_mat <- tryCatch(cor(items_df, use = use_method), error = function(e) NULL)
    if (!is.null(cov_mat) && length(vars) == 1) {
      cov_mat <- matrix(cov_mat, nrow = 1, ncol = 1, dimnames = list(vars, vars))
    }
    if (!is.null(cor_mat) && length(vars) == 1) {
      cor_mat <- matrix(cor_mat, nrow = 1, ncol = 1, dimnames = list(vars, vars))
    }
    item_df <- build_item_stats(items_df, cov_mat, group_label, nrow(items_df))
    rel_df <- compute_reliability_summary(items_df, cov_mat, cor_mat, group_label, score, omega)
    list(item_df = item_df, reliability_df = rel_df)
  }

  if (!is.null(group_var) && nzchar(group_var)) {
    group_vec <- df_case[[group_var]]
    group_levels <- unique(group_vec)
    for (g in group_levels) {
      idx <- if (is.na(g)) is.na(group_vec) else group_vec == g
      sub_df <- df_case[idx, , drop = FALSE]
      group_label <- ifelse(is.na(g), "NA", as.character(g))
      items_df <- sub_df[, vars, drop = FALSE]
      res <- append_rows(items_df, group_label)
      item_rows[[length(item_rows) + 1]] <- res$item_df
      rel_rows[[length(rel_rows) + 1]] <- res$reliability_df
    }
  } else {
    items_df <- df_case[, vars, drop = FALSE]
    res <- append_rows(items_df, "")
    item_rows[[length(item_rows) + 1]] <- res$item_df
    rel_rows[[length(rel_rows) + 1]] <- res$reliability_df
  }

  item_df <- do.call(rbind, item_rows)
  rel_df <- do.call(rbind, rel_rows)

  reverse_text <- if (length(reverse_items) > 0) paste(reverse_items, collapse = ",") else ""
  rev_min <- if (is.null(reverse_min)) NA_real_ else reverse_min
  rev_max <- if (is.null(reverse_max)) NA_real_ else reverse_max
  group_tag <- ifelse(rel_df$group == "", "all", rel_df$group)
  rel_df$case_id <- paste(case_name, group_tag, "summary", sep = "|")
  rel_df$case_name <- case_name
  rel_df$missing_method <- missing
  rel_df$score_method <- score
  rel_df$omega <- omega
  rel_df$reverse_items <- reverse_text
  rel_df$reverse_min <- rev_min
  rel_df$reverse_max <- rev_max
  rel_df$group_var <- if (!is.null(group_var)) group_var else ""

  item_group_tag <- ifelse(item_df$group == "", "all", item_df$group)
  item_df$case_id <- paste(case_name, item_group_tag, item_df$item, sep = "|")
  item_df$case_name <- case_name
  item_df$missing_method <- missing
  item_df$score_method <- score
  item_df$omega <- omega
  item_df$reverse_items <- reverse_text
  item_df$reverse_min <- rev_min
  item_df$reverse_max <- rev_max
  item_df$group_var <- if (!is.null(group_var)) group_var else ""

  list(item_df = item_df, reliability_df = rel_df)
}

cases <- list(
  list(
    name = "f1_pairwise_mean_omega_rev",
    vars = c("f1_1", "f1_2", "f1_3_rev", "f1_4"),
    group = NULL,
    reverse = c("f1_3_rev"),
    reverse_min = 1,
    reverse_max = 5,
    missing = "pairwise",
    score = "mean",
    omega = TRUE
  ),
  list(
    name = "f2_complete_mean_omega_rev",
    vars = c("f2_1", "f2_2", "f2_3", "f2_4_rev"),
    group = NULL,
    reverse = c("f2_4_rev"),
    reverse_min = 1,
    reverse_max = 5,
    missing = "complete",
    score = "mean",
    omega = TRUE
  ),
  list(
    name = "f1_pairwise_sum_noomega_group2",
    vars = c("f1_1", "f1_2", "f1_3_rev", "f1_4"),
    group = "group2",
    reverse = character(0),
    reverse_min = NULL,
    reverse_max = NULL,
    missing = "pairwise",
    score = "sum",
    omega = FALSE
  )
)

item_all <- list()
rel_all <- list()
for (case in cases) {
  res <- run_case(
    df,
    case$name,
    case$vars,
    group_var = case$group,
    reverse_items = case$reverse,
    reverse_min = case$reverse_min,
    reverse_max = case$reverse_max,
    missing = case$missing,
    score = case$score,
    omega = case$omega
  )
  item_all[[length(item_all) + 1]] <- res$item_df
  rel_all[[length(rel_all) + 1]] <- res$reliability_df
}

item_df <- do.call(rbind, item_all)
rel_df <- do.call(rbind, rel_all)

item_df <- item_df[, c(
  "case_id",
  "case_name",
  "item",
  "group",
  "missing_method",
  "score_method",
  "omega",
  "reverse_items",
  "reverse_min",
  "reverse_max",
  "group_var",
  "n",
  "missing_n",
  "missing_pct",
  "mean",
  "sd",
  "min",
  "max",
  "item_total_r",
  "item_rest_r",
  "alpha_if_deleted"
)]

rel_df <- rel_df[, c(
  "case_id",
  "case_name",
  "group",
  "missing_method",
  "score_method",
  "omega",
  "omega_status",
  "reverse_items",
  "reverse_min",
  "reverse_max",
  "group_var",
  "n_items",
  "n_total",
  "n_complete",
  "missing_n",
  "missing_pct",
  "alpha",
  "alpha_std",
  "omega_total",
  "r_bar",
  "r_min",
  "r_max",
  "score_mean",
  "score_sd",
  "score_min",
  "score_max"
)]

write.csv(item_df, item_out, row.names = FALSE, na = "NA")
write.csv(rel_df, rel_out, row.names = FALSE, na = "NA")

cat("Wrote:\n")
cat("- ", item_out, "\n", sep = "")
cat("- ", rel_out, "\n", sep = "")
