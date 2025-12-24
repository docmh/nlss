#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

resolve_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", cmd_args[grep("^--file=", cmd_args)])
  if (length(file_arg) > 0 && nzchar(file_arg[1])) {
    return(dirname(normalizePath(file_arg[1], winslash = "/", mustWork = FALSE)))
  }
  frame_file <- tryCatch(sys.frames()[[1]]$ofile, error = function(e) NULL)
  if (!is.null(frame_file) && nzchar(frame_file)) {
    return(dirname(normalizePath(frame_file, winslash = "/", mustWork = FALSE)))
  }
  getwd()
}

script_dir <- resolve_script_dir()
root <- normalizePath(file.path(script_dir, "..", ".."), winslash = "/", mustWork = FALSE)
scale_script <- file.path(root, "core-stats", "scripts", "R", "scale.R")

if (!file.exists(scale_script)) {
  stop(paste("Missing script:", scale_script))
}

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("jsonlite package is required to run scale statistical tests.")
}

config_path <- file.path(root, "core-stats", "scripts", "config.yml")
if (!file.exists(config_path)) {
  stop(paste("Missing config:", config_path))
}
config_backup <- tempfile("config-", fileext = ".yml")
file.copy(config_path, config_backup, overwrite = TRUE)
on.exit({
  file.copy(config_backup, config_path, overwrite = TRUE)
  unlink(config_backup)
}, add = TRUE)

set_default_output_dir <- function(path) {
  lines <- readLines(config_path, warn = FALSE)
  updated <- FALSE
  for (idx in seq_along(lines)) {
    if (grepl("^\\s*output_dir:", lines[[idx]])) {
      indent <- sub("(\\s*).*$", "\\1", lines[[idx]])
      lines[[idx]] <- paste0(indent, "output_dir: \"", path, "\"")
      updated <- TRUE
      break
    }
  }
  if (!updated) {
    stop("output_dir not found in config.yml")
  }
  writeLines(lines, config_path, useBytes = TRUE)
}

run_root_env <- Sys.getenv("CORE_STATS_TEST_ROOT", unset = "")
runs_base <- file.path(root, "outputs", "test-runs")
if (nzchar(run_root_env)) {
  run_root <- normalizePath(run_root_env, winslash = "/", mustWork = FALSE)
} else {
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  run_root <- file.path(runs_base, timestamp)
}
dir.create(run_root, recursive = TRUE, showWarnings = FALSE)

workdir <- file.path(run_root, "scale-statistical-tests")
dir.create(workdir, recursive = TRUE, showWarnings = FALSE)

workspace_root <- file.path(workdir, "workspace")
dir.create(workspace_root, recursive = TRUE, showWarnings = FALSE)
set_default_output_dir(workspace_root)

tmpdir <- file.path(workdir, "tmp")
dir.create(tmpdir, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(TMPDIR = tmpdir)

results <- character(0)
failures <- character(0)

assert_true <- function(cond, msg) {
  if (!isTRUE(cond)) stop(msg, call. = FALSE)
}

assert_contains <- function(text, expected, msg) {
  if (!grepl(expected, text, fixed = TRUE)) {
    stop(msg, call. = FALSE)
  }
}

assert_close <- function(actual, expected, tol, msg) {
  if (is.na(actual) && is.na(expected)) return(invisible(TRUE))
  if (is.na(actual) || is.na(expected)) stop(msg, call. = FALSE)
  if (abs(actual - expected) > tol) {
    stop(msg, call. = FALSE)
  }
  invisible(TRUE)
}

assert_vector_close <- function(actual, expected, tol, msg) {
  assert_true(length(actual) == length(expected), paste(msg, "(length mismatch)"))
  for (i in seq_along(actual)) {
    a <- actual[[i]]
    e <- expected[[i]]
    if (is.na(a) && is.na(e)) next
    if (is.na(a) || is.na(e)) stop(msg, call. = FALSE)
    if (abs(a - e) > tol) stop(msg, call. = FALSE)
  }
}

assert_df_close <- function(actual, expected, numeric_cols, character_cols, tol, label, order_cols = NULL) {
  if (!is.null(order_cols) && length(order_cols) > 0) {
    actual <- actual[do.call(order, actual[order_cols]), , drop = FALSE]
    expected <- expected[do.call(order, expected[order_cols]), , drop = FALSE]
    rownames(actual) <- NULL
    rownames(expected) <- NULL
  }

  assert_true(nrow(actual) == nrow(expected), paste(label, "row count mismatch"))

  for (col in character_cols) {
    assert_true(col %in% names(actual), paste(label, "missing actual column:", col))
    assert_true(col %in% names(expected), paste(label, "missing expected column:", col))
    actual_vals <- as.character(actual[[col]])
    expected_vals <- as.character(expected[[col]])
    assert_true(identical(actual_vals, expected_vals), paste(label, "column mismatch:", col))
  }

  for (col in numeric_cols) {
    assert_true(col %in% names(actual), paste(label, "missing actual column:", col))
    assert_true(col %in% names(expected), paste(label, "missing expected column:", col))
    actual_vals <- as.numeric(actual[[col]])
    expected_vals <- as.numeric(expected[[col]])
    assert_vector_close(actual_vals, expected_vals, tol, paste(label, "numeric mismatch:", col))
  }
}

read_latest_log <- function(out_dir) {
  log_path <- file.path(out_dir, "analysis_log.jsonl")
  assert_true(file.exists(log_path), paste("Missing analysis log:", log_path))
  lines <- readLines(log_path, warn = FALSE)
  assert_true(length(lines) > 0, paste("Empty analysis log:", log_path))
  jsonlite::fromJSON(lines[length(lines)])
}

run_scale <- function(name, df, items, args = list(), expect_failure = FALSE, input_type = "csv") {
  case_dir <- file.path(workdir, name)
  dir.create(case_dir, recursive = TRUE, showWarnings = FALSE)

  csv_path <- file.path(case_dir, paste0(name, ".csv"))
  rds_path <- file.path(case_dir, paste0(name, ".rds"))
  if (input_type == "rds") {
    saveRDS(df, rds_path)
  } else {
    write.csv(df, csv_path, row.names = FALSE)
  }

  case_workspace_dir <- file.path(workspace_root, name)
  dir.create(case_workspace_dir, recursive = TRUE, showWarnings = FALSE)

  cmd_args <- c(scale_script)
  if (input_type == "rds") {
    cmd_args <- c(cmd_args, "--rds", rds_path)
  } else {
    cmd_args <- c(cmd_args, "--csv", csv_path)
  }
  cmd_args <- c(
    cmd_args,
    "--vars", paste(items, collapse = ","),
    "--log", "TRUE",
    "--digits", "6"
  )

  if (!is.null(args$group)) {
    cmd_args <- c(cmd_args, "--group", args$group)
  }
  if (!is.null(args$missing)) {
    cmd_args <- c(cmd_args, "--missing", args$missing)
  }
  if (!is.null(args$score)) {
    cmd_args <- c(cmd_args, "--score", args$score)
  }
  if (!is.null(args$reverse)) {
    cmd_args <- c(cmd_args, "--reverse", args$reverse)
  }
  if (!is.null(args$reverse_min)) {
    cmd_args <- c(cmd_args, "--reverse-min", as.character(args$reverse_min))
  }
  if (!is.null(args$reverse_max)) {
    cmd_args <- c(cmd_args, "--reverse-max", as.character(args$reverse_max))
  }
  if (!is.null(args$omega)) {
    cmd_args <- c(cmd_args, "--omega", ifelse(isTRUE(args$omega), "TRUE", "FALSE"))
  }
  if (!is.null(args$coerce)) {
    cmd_args <- c(cmd_args, "--coerce", ifelse(isTRUE(args$coerce), "TRUE", "FALSE"))
  }

  output <- system2("Rscript", cmd_args, stdout = TRUE, stderr = TRUE)
  status <- attr(output, "status")
  if (is.null(status)) status <- 0

  if (expect_failure) {
    assert_true(status != 0, paste("Expected failure for", name))
    return(list(out_dir = case_workspace_dir, status = status, output = output))
  }

  if (status != 0) {
    stop(paste("Scale run failed:", name, "\n", paste(output, collapse = "\n")), call. = FALSE)
  }

  entry <- read_latest_log(case_workspace_dir)
  list(
    out_dir = case_workspace_dir,
    item_df = entry$results$item_df,
    reliability_df = entry$results$reliability_df,
    report_path = file.path(case_workspace_dir, "apa_report.md")
  )
}

apply_reverse <- function(df, items, reverse_items, reverse_min, reverse_max) {
  df_out <- df
  if (length(reverse_items) == 0) return(df_out)
  reverse_items <- intersect(reverse_items, items)
  method <- "fixed"
  if (is.null(reverse_min) || is.null(reverse_max) || is.na(reverse_min) || is.na(reverse_max)) {
    method <- "observed"
  }
  for (item in reverse_items) {
    vec <- df_out[[item]]
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
      df_out[[item]] <- max_val + min_val - vec
    }
  }
  df_out
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

build_item_stats_expected <- function(items_df, cov_mat, group_label, total_n) {
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
  df <- do.call(rbind, rows)
  numeric_cols <- setdiff(names(df), c("item", "group"))
  for (col in numeric_cols) {
    df[[col]] <- as.numeric(df[[col]])
  }
  df
}

compute_reliability_expected <- function(items_df, cov_mat, cor_mat, group_label, score_method, omega_flag) {
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

compute_expected <- function(df, items, group_var = NULL, missing_method = "pairwise",
                             score_method = "sum", omega_flag = TRUE,
                             reverse_items = character(0), reverse_min = NULL, reverse_max = NULL) {
  df_use <- apply_reverse(df, items, reverse_items, reverse_min, reverse_max)
  use_method <- if (missing_method == "pairwise") "pairwise.complete.obs" else "complete.obs"

  item_list <- list()
  rel_list <- list()

  if (!is.null(group_var)) {
    group_vec <- df_use[[group_var]]
    group_levels <- unique(group_vec)
    for (g in group_levels) {
      idx <- if (is.na(g)) is.na(group_vec) else group_vec == g
      sub_df <- df_use[idx, , drop = FALSE]
      group_label <- ifelse(is.na(g), "NA", as.character(g))
      items_df <- sub_df[, items, drop = FALSE]

      cov_mat <- tryCatch(cov(items_df, use = use_method), error = function(e) NULL)
      cor_mat <- tryCatch(cor(items_df, use = use_method), error = function(e) NULL)
      if (!is.null(cov_mat) && length(items) == 1) {
        cov_mat <- matrix(cov_mat, nrow = 1, ncol = 1, dimnames = list(items, items))
      }
      if (!is.null(cor_mat) && length(items) == 1) {
        cor_mat <- matrix(cor_mat, nrow = 1, ncol = 1, dimnames = list(items, items))
      }

      item_list[[length(item_list) + 1]] <- build_item_stats_expected(items_df, cov_mat, group_label, nrow(items_df))
      rel_list[[length(rel_list) + 1]] <- compute_reliability_expected(items_df, cov_mat, cor_mat, group_label,
                                                                        score_method, omega_flag)
    }
  } else {
    items_df <- df_use[, items, drop = FALSE]
    cov_mat <- tryCatch(cov(items_df, use = use_method), error = function(e) NULL)
    cor_mat <- tryCatch(cor(items_df, use = use_method), error = function(e) NULL)
    if (!is.null(cov_mat) && length(items) == 1) {
      cov_mat <- matrix(cov_mat, nrow = 1, ncol = 1, dimnames = list(items, items))
    }
    if (!is.null(cor_mat) && length(items) == 1) {
      cor_mat <- matrix(cor_mat, nrow = 1, ncol = 1, dimnames = list(items, items))
    }

    item_list[[length(item_list) + 1]] <- build_item_stats_expected(items_df, cov_mat, "", nrow(items_df))
    rel_list[[length(rel_list) + 1]] <- compute_reliability_expected(items_df, cov_mat, cor_mat, "",
                                                                      score_method, omega_flag)
  }

  list(
    item_df = do.call(rbind, item_list),
    reliability_df = do.call(rbind, rel_list)
  )
}

run_test <- function(name, fn) {
  ok <- TRUE
  err <- NULL
  tryCatch(fn(), error = function(e) {
    ok <<- FALSE
    err <<- e$message
  })
  if (ok) {
    results <<- c(results, paste(name, ": OK"))
  } else {
    results <<- c(results, paste(name, ": FAIL -", err))
    failures <<- c(failures, name)
  }
}

numeric_item_cols <- c(
  "n", "missing_n", "missing_pct", "mean", "sd", "min", "max",
  "item_total_r", "item_rest_r", "alpha_if_deleted"
)
character_item_cols <- c("item", "group")

numeric_rel_cols <- c(
  "n_items", "n_total", "n_complete", "missing_n", "missing_pct", "alpha",
  "alpha_std", "omega_total", "r_bar", "r_min", "r_max",
  "score_mean", "score_sd", "score_min", "score_max"
)
character_rel_cols <- c("group", "score_method", "omega_status")

run_test("basic_no_missing", function() {
  df <- data.frame(
    item1 = c(1, 2, 3, 4, 5),
    item2 = c(2, 2, 3, 4, 5),
    item3 = c(1, 3, 4, 6, 7)
  )
  items <- c("item1", "item2", "item3")
  res <- run_scale("basic_no_missing", df, items, args = list(missing = "pairwise", score = "sum", omega = TRUE))
  expected <- compute_expected(df, items, missing_method = "pairwise", score_method = "sum", omega_flag = TRUE)

  assert_df_close(res$item_df, expected$item_df, numeric_item_cols, character_item_cols, 1e-6,
                  "basic_no_missing item", order_cols = c("group", "item"))
  assert_df_close(res$reliability_df, expected$reliability_df, numeric_rel_cols, character_rel_cols, 1e-6,
                  "basic_no_missing reliability", order_cols = c("group"))
  assert_true(all(res$reliability_df$omega_status == "ok"), "Omega status should be ok")
})

run_test("score_mean", function() {
  df <- data.frame(
    item1 = c(1, 2, 3, 4, 5),
    item2 = c(2, 2, 3, 4, 5),
    item3 = c(1, 3, 4, 6, 7)
  )
  items <- c("item1", "item2", "item3")
  res <- run_scale("score_mean", df, items, args = list(score = "mean", omega = FALSE))
  expected <- compute_expected(df, items, score_method = "mean", omega_flag = FALSE)

  assert_df_close(res$reliability_df, expected$reliability_df, numeric_rel_cols, character_rel_cols, 1e-6,
                  "score_mean reliability", order_cols = c("group"))
  assert_true(all(res$reliability_df$omega_status == "disabled"), "Omega should be disabled")
})

run_test("missing_pairwise", function() {
  df <- data.frame(
    item1 = c(1, 2, 3, 4, 5),
    item2 = c(2, NA, 3, 4, 5),
    item3 = c(1, 2, NA, 4, 5)
  )
  items <- c("item1", "item2", "item3")
  res <- run_scale("missing_pairwise", df, items, args = list(missing = "pairwise", omega = FALSE))
  expected <- compute_expected(df, items, missing_method = "pairwise", omega_flag = FALSE)

  assert_df_close(res$item_df, expected$item_df, numeric_item_cols, character_item_cols, 1e-6,
                  "missing_pairwise item", order_cols = c("group", "item"))
  assert_df_close(res$reliability_df, expected$reliability_df, numeric_rel_cols, character_rel_cols, 1e-6,
                  "missing_pairwise reliability", order_cols = c("group"))
})

run_test("missing_complete", function() {
  df <- data.frame(
    item1 = c(1, 2, 3, 4, 5),
    item2 = c(2, NA, 3, 4, 5),
    item3 = c(1, 2, NA, 4, 5)
  )
  items <- c("item1", "item2", "item3")
  res_complete <- run_scale("missing_complete", df, items, args = list(missing = "complete", omega = FALSE))
  expected_complete <- compute_expected(df, items, missing_method = "complete", omega_flag = FALSE)

  assert_df_close(res_complete$reliability_df, expected_complete$reliability_df, numeric_rel_cols,
                  character_rel_cols, 1e-6, "missing_complete reliability", order_cols = c("group"))

  res_pair <- run_scale("missing_complete_compare", df, items, args = list(missing = "pairwise", omega = FALSE))
  alpha_pair <- res_pair$reliability_df$alpha[1]
  alpha_complete <- res_complete$reliability_df$alpha[1]
  assert_true(is.na(alpha_pair) || is.na(alpha_complete) || abs(alpha_pair - alpha_complete) > 1e-6,
              "Expected alpha to differ between pairwise and complete")
})

run_test("reverse_fixed", function() {
  df <- data.frame(
    item1 = c(1, 2, 3, 4, 5),
    item2 = c(1, 1, 2, 2, 3),
    item3 = c(5, 4, 4, 3, 2)
  )
  items <- c("item1", "item2", "item3")
  res <- run_scale(
    "reverse_fixed",
    df,
    items,
    args = list(reverse = "item2", reverse_min = 1, reverse_max = 5, omega = FALSE)
  )
  expected <- compute_expected(df, items, omega_flag = FALSE, reverse_items = "item2",
                               reverse_min = 1, reverse_max = 5)

  assert_df_close(res$item_df, expected$item_df, numeric_item_cols, character_item_cols, 1e-6,
                  "reverse_fixed item", order_cols = c("group", "item"))
  report_text <- paste(readLines(res$report_path, warn = FALSE), collapse = "\n")
  assert_contains(report_text, "Reverse-scored items: item2 (min = 1, max = 5).",
                  "Reverse fixed note missing")
})

run_test("reverse_observed", function() {
  df <- data.frame(
    item1 = c(1, 2, 3, 4, 5),
    item2 = c(1, 1, 2, 2, 3),
    item3 = c(5, 4, 4, 3, 2)
  )
  items <- c("item1", "item2", "item3")
  res <- run_scale("reverse_observed", df, items, args = list(reverse = "item2", omega = FALSE))
  expected <- compute_expected(df, items, omega_flag = FALSE, reverse_items = "item2")

  assert_df_close(res$item_df, expected$item_df, numeric_item_cols, character_item_cols, 1e-6,
                  "reverse_observed item", order_cols = c("group", "item"))
  report_text <- paste(readLines(res$report_path, warn = FALSE), collapse = "\n")
  assert_contains(report_text, "Reverse-scored items: item2 (using observed min/max).",
                  "Reverse observed note missing")
})

run_test("grouped_with_na", function() {
  df <- data.frame(
    group = c("A", "A", "B", "B", NA, "A", "B", NA),
    item1 = c(1, 2, 3, 4, 2, 3, 5, 4),
    item2 = c(2, 3, 2, 5, 4, 3, 5, 4),
    item3 = c(1, 2, 4, 6, 3, 4, 6, 5)
  )
  items <- c("item1", "item2", "item3")
  res <- run_scale("grouped_with_na", df, items, args = list(group = "group", omega = FALSE))
  expected <- compute_expected(df, items, group_var = "group", omega_flag = FALSE)

  assert_df_close(res$reliability_df, expected$reliability_df, numeric_rel_cols, character_rel_cols, 1e-6,
                  "grouped_with_na reliability", order_cols = c("group"))
  assert_true(any(res$reliability_df$group == "NA"), "Expected NA group label")
})

run_test("single_item", function() {
  df <- data.frame(
    item1 = c(1, 2, 3, 4, 5)
  )
  items <- c("item1")
  res <- run_scale("single_item", df, items, args = list(omega = TRUE))
  expected <- compute_expected(df, items, omega_flag = TRUE)

  assert_df_close(res$item_df, expected$item_df, numeric_item_cols, character_item_cols, 1e-6,
                  "single_item item", order_cols = c("group", "item"))
  assert_true(is.na(res$reliability_df$alpha[1]), "Alpha should be NA for single item")
  assert_true(res$reliability_df$omega_status[1] == "insufficient_items",
              "Omega status should be insufficient_items")
})

run_test("constant_item", function() {
  df <- data.frame(
    item1 = c(3, 3, 3, 3),
    item2 = c(1, 2, 3, 4)
  )
  items <- c("item1", "item2")
  res <- run_scale("constant_item", df, items, args = list(omega = FALSE))
  expected <- compute_expected(df, items, omega_flag = FALSE)

  assert_df_close(res$item_df, expected$item_df, numeric_item_cols, character_item_cols, 1e-6,
                  "constant_item item", order_cols = c("group", "item"))
  assert_df_close(res$reliability_df, expected$reliability_df, numeric_rel_cols, character_rel_cols, 1e-6,
                  "constant_item reliability", order_cols = c("group"))
})

run_test("coerce_behavior", function() {
  df <- data.frame(
    item1 = c("1", "2", "3", "4"),
    item2 = c("2", "3", "4", "5")
  )
  items <- c("item1", "item2")
  res <- run_scale("coerce_true", df, items, args = list(coerce = TRUE, omega = FALSE), input_type = "rds")
  expected_df <- data.frame(
    item1 = as.numeric(df$item1),
    item2 = as.numeric(df$item2)
  )
  expected <- compute_expected(expected_df, items, omega_flag = FALSE)

  assert_df_close(res$item_df, expected$item_df, numeric_item_cols, character_item_cols, 1e-6,
                  "coerce_true item", order_cols = c("group", "item"))

  run_scale("coerce_false", df, items, args = list(coerce = FALSE, omega = FALSE),
            expect_failure = TRUE, input_type = "rds")
})

run_test("all_missing_item", function() {
  df <- data.frame(
    item1 = c(NA_real_, NA_real_, NA_real_),
    item2 = c(1, 2, 3)
  )
  items <- c("item1", "item2")
  res <- run_scale("all_missing_item", df, items, args = list(omega = FALSE), input_type = "rds")
  expected <- compute_expected(df, items, omega_flag = FALSE)

  assert_df_close(res$item_df, expected$item_df, numeric_item_cols, character_item_cols, 1e-6,
                  "all_missing_item item", order_cols = c("group", "item"))
  assert_df_close(res$reliability_df, expected$reliability_df, numeric_rel_cols, character_rel_cols, 1e-6,
                  "all_missing_item reliability", order_cols = c("group"))
})

run_results_file <- file.path(workdir, "results.txt")
writeLines(results, run_results_file)

summary_file <- file.path(run_root, "scale-tests-results.txt")
writeLines(results, summary_file)

cat(paste(results, collapse = "\n"), "\n")
cat("Results written to:", run_results_file, "\n")
cat("Latest summary:", summary_file, "\n")

if (length(failures) > 0) {
  stop(paste("Scale statistical tests failed:", paste(failures, collapse = ", ")))
}
