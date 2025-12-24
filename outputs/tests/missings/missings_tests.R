#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

assert_true <- function(cond, message) {
  if (!isTRUE(cond)) stop(message, call. = FALSE)
}

assert_equal <- function(actual, expected, message) {
  if (length(actual) == 1 && length(expected) == 1 && is.numeric(actual) && is.numeric(expected)) {
    if (isTRUE(all.equal(as.numeric(actual), as.numeric(expected)))) return(invisible(TRUE))
  }
  if (!identical(actual, expected)) {
    stop(paste0(message, " (expected: ", expected, ", got: ", actual, ")"), call. = FALSE)
  }
}

assert_close <- function(actual, expected, tol, message) {
  if (is.na(actual) || is.na(expected) || abs(actual - expected) > tol) {
    stop(paste0(message, " (expected: ", expected, ", got: ", actual, ")"), call. = FALSE)
  }
}

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
repo_root <- normalizePath(file.path(script_dir, "..", ".."))
setwd(repo_root)

out_root <- file.path(repo_root, "outputs", "tests", "missings")
data_dir <- file.path(out_root, "data")
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

config_path <- file.path(repo_root, "core-stats", "scripts", "config.yml")
config_lines <- readLines(config_path)
on.exit(writeLines(config_lines, config_path), add = TRUE)

output_idx <- grep("^\\s*output_dir:", config_lines)
assert_true(length(output_idx) > 0, "config.yml missing defaults.output_dir")
config_lines[output_idx[1]] <- sub("output_dir:.*", "output_dir: \"./outputs/tests\"", config_lines[output_idx[1]])
writeLines(config_lines, config_path)

missings_script <- file.path(repo_root, "core-stats", "scripts", "R", "missings.R")
out_dir <- file.path(repo_root, "outputs", "tests")
base_outputs <- c("missing_handled_data.rds", "apa_report.md", "analysis_log.jsonl")

resolve_dataset_dir <- function(name) {
  file.path(out_dir, name)
}

clean_outputs <- function(dataset_dir) {
  for (file in base_outputs) {
    path <- file.path(dataset_dir, file)
    if (file.exists(path)) file.remove(path)
  }
}

copy_outputs <- function(case_dir, dataset_dir, stdout_lines) {
  dir.create(case_dir, recursive = TRUE, showWarnings = FALSE)
  for (file in base_outputs) {
    src <- file.path(dataset_dir, file)
    if (file.exists(src)) {
      file.copy(src, file.path(case_dir, file), overwrite = TRUE)
    }
  }
  if (length(stdout_lines) > 0) {
    writeLines(stdout_lines, file.path(case_dir, "run.log"))
  }
}

run_case <- function(name, input_type, df, vars, extra_args = character(0), post_check = NULL) {
  case_dir <- file.path(out_root, name)
  dataset_dir <- resolve_dataset_dir(name)
  dir.create(dataset_dir, recursive = TRUE, showWarnings = FALSE)
  clean_outputs(dataset_dir)

  args <- c(missings_script)
  if (input_type == "csv") {
    data_path <- file.path(data_dir, paste0(name, ".csv"))
    write.csv(df, data_path, row.names = FALSE)
    args <- c(args, "--csv", data_path)
  } else if (input_type == "rds") {
    data_path <- file.path(data_dir, paste0(name, ".rds"))
    saveRDS(df, data_path)
    args <- c(args, "--rds", data_path)
  } else if (input_type == "rdata") {
    data_path <- file.path(data_dir, paste0(name, ".RData"))
    df_name <- "test_df"
    assign(df_name, df)
    save(list = df_name, file = data_path)
    args <- c(args, "--rdata", data_path, "--df", df_name)
  } else {
    stop("Unsupported input type: ", input_type)
  }

  if (length(vars) > 0) {
    args <- c(args, "--vars", paste(vars, collapse = ","))
  }
  if (length(extra_args) > 0) {
    args <- c(args, extra_args)
  }

  stdout <- system2("Rscript", args, stdout = TRUE, stderr = TRUE)
  status <- attr(stdout, "status")
  if (!is.null(status) && status != 0) {
    copy_outputs(case_dir, dataset_dir, stdout)
    stop("Test case failed: ", name, "\n", paste(stdout, collapse = "\n"), call. = FALSE)
  }

  out_rds <- file.path(dataset_dir, "missing_handled_data.rds")
  assert_true(file.exists(out_rds), paste0("Missing output dataset for case: ", name))
  out_df <- readRDS(out_rds)

  if (!is.null(post_check)) {
    post_check(out_df, df, dataset_dir)
  }

  copy_outputs(case_dir, dataset_dir, stdout)
  clean_outputs(dataset_dir)
}

tests_run <- 0

set.seed(100)
df1 <- data.frame(a = rnorm(50), b = rnorm(50))
df1$a[5] <- NA
run_case(
  "listwise_auto_csv",
  "csv",
  df1,
  c("a", "b"),
  post_check = function(out_df, in_df, out_dir) {
    tests_run <<- tests_run + 1
    expected_rows <- sum(complete.cases(in_df[, c("a", "b"), drop = FALSE]))
    assert_equal(nrow(out_df), expected_rows, "Listwise deletion row count mismatch")
    assert_true(sum(is.na(out_df$a)) == 0, "Listwise output still has missing values")
  }
)

set.seed(101)
skew <- rexp(50, rate = 0.6)
missing_idx <- sample(1:50, 5)
skew[missing_idx] <- NA
df2 <- data.frame(skew = skew)
run_case(
  "impute_auto_skew_rds",
  "rds",
  df2,
  "skew",
  post_check = function(out_df, in_df, out_dir) {
    tests_run <<- tests_run + 1
    median_val <- median(in_df$skew, na.rm = TRUE)
    assert_true(sum(is.na(out_df$skew)) == 0, "Imputation left missing values")
    imputed_vals <- out_df$skew[missing_idx]
    assert_close(imputed_vals[1], median_val, 1e-8, "Skewed imputation did not use median")
  }
)

set.seed(102)
cvals <- rnorm(50)
c_miss <- sample(1:50, 15)
cvals[c_miss] <- NA
catvals <- factor(sample(c("A", "B", "C"), 50, replace = TRUE))
cat_miss <- sample(setdiff(1:50, c_miss), 15)
catvals[cat_miss] <- NA
df3 <- data.frame(c = cvals, cat = catvals)
run_case(
  "indicator_auto_rdata",
  "rdata",
  df3,
  c("c", "cat"),
  post_check = function(out_df, in_df, out_dir) {
    tests_run <<- tests_run + 1
    assert_true("c_miss" %in% names(out_df), "Missingness indicator for c not created")
    assert_true("cat_miss" %in% names(out_df), "Missingness indicator for cat not created")
    assert_true(all(out_df$c_miss == as.integer(is.na(in_df$c))), "c_miss indicator mismatch")
    assert_true(all(out_df$cat_miss == as.integer(is.na(in_df$cat))), "cat_miss indicator mismatch")
    assert_true(sum(is.na(out_df$c)) == 0, "Indicator method did not impute numeric")
    assert_true(sum(is.na(out_df$cat)) == 0, "Indicator method did not impute categorical")
    assert_true(is.factor(out_df$cat), "Categorical variable lost factor type after imputation")
  }
)

set.seed(103)
dvals <- rnorm(50)
dvals[sample(1:50, 40)] <- NA
evals <- rnorm(50)
evals[sample(1:50, 17)] <- NA
df4 <- data.frame(d = dvals, e = evals)
run_case(
  "drop_auto_csv",
  "csv",
  df4,
  c("d", "e"),
  post_check = function(out_df, in_df, out_dir) {
    tests_run <<- tests_run + 1
    assert_true(!("d" %in% names(out_df)), "Drop method did not remove high-missing variable")
    assert_true("e" %in% names(out_df), "Variable e missing after drop method")
    assert_true("e_miss" %in% names(out_df), "Missingness indicator for e not created")
    assert_true(sum(is.na(out_df$e)) == 0, "Drop method did not impute remaining variable")
  }
)

set.seed(104)
xvals <- rnorm(50)
xvals[sample(1:50, 10)] <- NA
yvals <- rnorm(50)
df5 <- data.frame(x = xvals, y = yvals)
run_case(
  "listwise_explicit_csv",
  "csv",
  df5,
  c("x", "y"),
  extra_args = c("--method", "listwise"),
  post_check = function(out_df, in_df, out_dir) {
    tests_run <<- tests_run + 1
    expected_rows <- sum(complete.cases(in_df[, c("x", "y"), drop = FALSE]))
    assert_equal(nrow(out_df), expected_rows, "Explicit listwise row count mismatch")
  }
)

set.seed(105)
df6 <- data.frame(all_miss = rep(NA_real_, 50), y = rnorm(50))
run_case(
  "all_missing_impute_rds",
  "rds",
  df6,
  c("all_miss", "y"),
  extra_args = c("--method", "impute"),
  post_check = function(out_df, in_df, out_dir) {
    tests_run <<- tests_run + 1
    assert_true(all(is.na(out_df$all_miss)), "All-missing variable should remain NA")
    assert_equal(nrow(out_df), nrow(in_df), "Row count changed for all-missing case")
  }
)

df7 <- data.frame(x = numeric(0), y = character(0), stringsAsFactors = FALSE)
run_case(
  "zero_rows_csv",
  "csv",
  df7,
  c("x", "y"),
  extra_args = c("--method", "impute"),
  post_check = function(out_df, in_df, out_dir) {
    tests_run <<- tests_run + 1
    assert_equal(nrow(out_df), 0, "Zero-row dataset should remain empty")
    assert_equal(ncol(out_df), 2, "Zero-row dataset column count mismatch")
  }
)

set.seed(106)
df8 <- data.frame(
  p1 = rnorm(30),
  p2 = rnorm(30),
  p3 = rnorm(30),
  p4 = rnorm(30)
)
df8$p1[sample(1:30, 8)] <- NA
df8$p2[sample(1:30, 6)] <- NA
df8$p3[sample(1:30, 7)] <- NA
df8$p4[sample(1:30, 5)] <- NA
run_case(
  "pattern_truncation_csv",
  "csv",
  df8,
  c("p1", "p2", "p3", "p4"),
  extra_args = c("--max-patterns", "2"),
  post_check = function(out_df, in_df, out_dir) {
    tests_run <<- tests_run + 1
    report_path <- file.path(out_dir, "apa_report.md")
    assert_true(file.exists(report_path), "APA report missing for pattern truncation case")
    report_text <- paste(readLines(report_path, warn = FALSE), collapse = "\n")
    assert_true(grepl("Other patterns", report_text), "Pattern truncation not reflected in report")
  }
)

cat("Missings tests completed:", tests_run, "cases passed.\n")
