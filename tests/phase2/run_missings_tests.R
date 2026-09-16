#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Public CLI acceptance: independent base-R values, immutable before/after data,
# metadata, publication refusal and non-activating replay. No NLSS source oracle.
args <- commandArgs(TRUE)
script <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])[1]
repo <- normalizePath(file.path(dirname(script), "../.."), winslash = "/")
script <- normalizePath(script, winslash = "/")
if ("--help" %in% args) { cat("Usage: run_missings_tests.R [--root PATH] [--keep N] [--match REGEX]\n"); quit(status = 0) }
if (length(args) %% 2L || (length(args) && any(!args[seq(1L, length(args), 2L)] %in% c("--root", "--keep", "--match")))) stop("Unknown/incomplete runner option")
arg <- function(name, fallback) { i <- which(args == name); if (length(i) > 1L) stop("Repeated runner option"); if (length(i)) args[i + 1L] else fallback }
pattern <- arg("--match", ".*"); invisible(grepl(pattern, "validate regex"))
required <- c("yaml", "jsonlite", "digest", "arrow", "haven")
for (package in required) if (!requireNamespace(package, quietly = TRUE)) stop("Missing test package: ", package)
absolute <- function(path) normalizePath(if (grepl("^(/|[A-Za-z]:|\\\\)", path)) path else file.path(repo, path), winslash = "/", mustWork = FALSE)
cfg <- yaml::read_yaml(absolute(Sys.getenv("NLSS_TESTS_CONFIG", "tests/tests.yml")), eval.expr = FALSE)$tests
forced <- arg("--root", Sys.getenv("NLSS_TEST_ROOT", ""))
keep <- suppressWarnings(as.integer(arg("--keep", Sys.getenv("NLSS_KEEP_RUNS", as.character(cfg$keep_runs_default)))))
if (length(keep) != 1L || is.na(keep) || keep < 0L) stop("Invalid --keep")
collection <- file.path(if (nzchar(forced)) absolute(forced) else absolute(cfg$output_dir), "phase2-missings")
work <- file.path(collection, paste0("run-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid()))
dir.create(work, recursive = TRUE)
config <- file.path(work, "config.yml")
reset_config <- function() yaml::write_yaml(list(defaults = list(digits = 12L)), config)
reset_config(); Sys.setenv(NLSS_CONFIG_PATH = config, OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1")
Sys.unsetenv("NLSS_REPLAY_REQUEST")
check <- function(ok, message) if (!isTRUE(ok)) stop(message, call. = FALSE)
read_text <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
read_json <- function(path) jsonlite::fromJSON(path, simplifyVector = TRUE)
write_json <- function(value, path) jsonlite::write_json(value, path, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null", na = "null")
sha <- function(path) digest::digest(file = path, algo = "sha256")
utc <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
started_utc <- utc()
production_paths <- sort(list.files(file.path(repo, "scripts/R"), "[.]R$", recursive = TRUE, full.names = TRUE))
source_at_start <- setNames(vapply(production_paths, sha, character(1)), substring(production_paths, nchar(repo) + 2L))
write_json(list(started_utc = started_utc, runner_sha256 = sha(script), selected_pattern = pattern, production_files = as.list(source_at_start)), file.path(work, "started.json"))
numeric_checks <- 0L
near <- function(actual, expected, label, tolerance = 1e-10) {
  actual <- as.numeric(actual); expected <- as.numeric(expected)
  check(length(actual) == length(expected), paste(label, "length differs"))
  check(identical(is.na(actual), is.na(expected)), paste(label, "missing mask differs"))
  check(identical(is.nan(actual), is.nan(expected)), paste(label, "NaN mask differs"))
  finite <- is.finite(expected)
  check(identical(is.infinite(actual), is.infinite(expected)), paste(label, "infinite mask differs"))
  check(identical(sign(actual[is.infinite(actual)]), sign(expected[is.infinite(expected)])), paste(label, "infinite signs differ"))
  check(all(abs(actual[finite] - expected[finite]) <= tolerance * pmax(1, abs(expected[finite]))), paste(label, "differs from independent reference"))
  numeric_checks <<- numeric_checks + length(expected)
}
same <- function(actual, expected, label) {
  if (is.numeric(expected)) near(actual, expected, label)
  else check(identical(unname(as.character(actual)), unname(as.character(expected))), paste(label, "values differ"))
}
snapshot <- function(paths) { paths <- paths[file.exists(paths) & !dir.exists(paths)]; setNames(vapply(paths, sha, character(1)), paths) }
tree <- function(path) snapshot(sort(list.files(path, recursive = TRUE, full.names = TRUE, all.files = TRUE)))
results <- list(); registered_tests <- character()
test <- function(name, code) {
  registered_tests <<- c(registered_tests, name)
  if (!grepl(pattern, name)) return(invisible(NULL))
  previous <- getwd(); on.exit(setwd(previous)); reset_config(); before <- numeric_checks; start <- proc.time()[["elapsed"]]
  error <- tryCatch({ force(code); NULL }, error = conditionMessage)
  results[[length(results) + 1L]] <<- list(test = name, passed = is.null(error), numeric_checks = numeric_checks - before,
    seconds = unname(proc.time()[["elapsed"]] - start), message = if (is.null(error)) "OK" else error)
  cat(if (is.null(error)) "[PASS] " else "[FAIL] ", name, if (is.null(error)) "" else paste0(": ", error), "\n", sep = "")
}
sample <- data.frame(id = 1:8, x = c(1, 2, 3, 4, 8, NA, 0, -1), y = c(4, 8, 2, 6, 1, 9, 3, 5),
  code = c(1, 2, 3, NA, 1, 2, 3, 4), text = c("10", "2", "30", NA, "bad", "4", "7", "8"),
  category = factor(c("low", "high", "low", NA, "middle", "high", "middle", "low"), levels = c("middle", "high", "low")))
new_case <- function(name, data = sample, format = "rds") {
  base <- file.path(work, "cases", name); project <- file.path(base, "project"); dir.create(project, recursive = TRUE)
  yaml::write_yaml(list(version = 1L, datasets = list()), file.path(project, "nlss-workspace.yml"))
  input <- file.path(base, paste0("sample.", format))
  switch(format, rds = saveRDS(data, input), csv = write.csv(data, input, row.names = FALSE),
    sav = haven::write_sav(data, input), parquet = arrow::write_parquet(data, input), RData = { survey <- data; save(survey, file = input) })
  list(base = base, project = project, input = input, data = data, format = format,
    directory = file.path(project, if (format == "RData") "survey" else "sample"),
    source = c(paste0("--", tolower(format)), input, if (format == "RData") c("--df", "survey")))
}
tokens <- function(options) unlist(Map(function(key, value) c(paste0("--", key), as.character(value)), names(options), options), use.names = FALSE)
run_cli <- function(context, argv, module = "missings", failure = FALSE) {
  previous <- getwd(); on.exit(setwd(previous)); setwd(context$project)
  log <- tempfile(paste0(module, "-"), context$base, fileext = ".log")
  command <- list(module = module, argv = argv, started_utc = utc(), expected_failure = failure,
    entrypoint_sha256 = sha(file.path(repo, "scripts/R", paste0(module, ".R"))))
  record_path <- paste0(log, ".process.json"); write_json(command, record_path)
  status <- system2(file.path(R.home("bin"), "Rscript"), c(shQuote(file.path(repo, "scripts/R", paste0(module, ".R"))), shQuote(argv)), stdout = log, stderr = log)
  command$finished_utc <- utc(); command$exit_code <- status; write_json(command, record_path)
  check(if (failure) status != 0L else status == 0L, paste(module, "unexpected exit", status, read_text(log)))
  log
}
runs <- function(context) {
  paths <- list.files(context$directory, "^request[.]json$", recursive = TRUE, full.names = TRUE, all.files = TRUE)
  dirname(paths[grepl("/runs/[^.][^/]+/request[.]json$", paths)])
}
working <- function(context) file.path(context$directory, paste0(basename(context$directory), ".parquet"))
protected <- function(context) c(working(context), file.path(context$directory, c("import.json", "dictionary.json", "codebook.md", "report_canonical.md", "analysis_log.jsonl")), file.path(context$project, "nlss-workspace.yml"))
read_ref <- function(context, ref) {
  check(is.list(ref) && nzchar(ref$version_id), "Missing dataset version reference")
  for (kind in c("snapshot", "dictionary")) {
    path <- ref[[paste0(kind, "_path")]]; hash <- ref[[if (kind == "snapshot") "data_sha256" else "dictionary_sha256"]]
    check(!grepl("^/|(^|/)[.][.](/|$)", path), "Version reference escapes project")
    check(identical(sha(file.path(context$project, path)), hash), paste("Version", kind, "hash mismatch"))
  }
  as.data.frame(arrow::read_parquet(file.path(context$project, ref$snapshot_path)))
}
run <- function(context, options = list(), source = TRUE, module = "missings") {
  old <- runs(context); input_hash <- sha(context$input)
  log <- run_cli(context, c(if (source) context$source, if (is.list(options)) tokens(options) else options), module)
  added <- setdiff(runs(context), old); check(length(added) == 1L, "Expected exactly one published run")
  request_path <- file.path(added, "request.json"); req <- read_json(request_path); res <- read_json(file.path(added, "result.json"))
  check(res$status == "completed" && isTRUE(req$resolved), "Unresolved or failed run")
  check(req$module == "missings" && res$module == "missings", "Wrong module identity")
  check(identical(req$run_id, basename(added)) && identical(req$run_id, res$run_id), "Run identity mismatch")
  check(identical(res$artifacts$request$sha256, sha(request_path)), "Request/result hash mismatch")
  for (artifact in res$artifacts) check(identical(sha(file.path(added, artifact$path)), artifact$sha256), "Artifact hash mismatch")
  change <- res$results$data_change
  check(is.list(change) && identical(change$input$version_id, req$dataset$version_id), "Before reference not bound to request")
  before <- read_ref(context, change$input); after <- read_ref(context, change$output)
  source_rows <- as.integer(unlist(req$design$source_rows, use.names = FALSE))
  check(identical(change$input$dataset_id, change$output$dataset_id), "Missing-data handling invented another dataset")
  check(identical(change$input$source_sha256, change$output$source_sha256), "Missing-data handling replaced original source identity")
  check(identical(change$input$import_version_id, change$output$import_version_id), "Missing-data handling replaced import binding")
  check(all(req$design$source_rows %in% seq_len(nrow(before))) && !anyDuplicated(req$design$source_rows), "Invalid frozen source-row identities")
  near(res$results$source_rows, req$design$source_rows, "Request/result row maps agree", 0)
  near(nrow(after), length(req$design$source_rows), "Output row count matches map", 0)
  near(res$results$source_n, nrow(before), "Reported original N", 0)
  near(res$results$output_n, nrow(after), "Reported output N", 0)
  complete_n <- sum(complete.cases(before[res$results$summary_df$variable]))
  near(res$results$complete_cases_n, complete_n, "Original selected complete cases", 0)
  near(if (is.null(res$results$complete_cases_pct)) NA_real_ else res$results$complete_cases_pct,
    if (nrow(before)) 100 * complete_n / nrow(before) else NA_real_, "Original complete-case percentage")
  for (variable in res$results$summary_df$variable) {
    audit <- res$results$handling_audit[[variable]]
    check(is.list(audit), paste("Missing per-variable handling audit", variable))
    before_missing <- which(is.na(before[[variable]]))
    near(audit$missing_before_n, length(before_missing), paste(variable, "before missing count"), 0)
    near(audit$missing_before_rows, before_missing, paste(variable, "before missing positions"), 0)
    near(audit$observed_before_n, nrow(before) - length(before_missing), paste(variable, "observed donor count"), 0)
    if (variable %in% names(after)) {
      after_missing <- which(is.na(after[[variable]]))
      near(audit$missing_after_n, length(after_missing), paste(variable, "after missing count"), 0)
      near(audit$missing_after_rows, after_missing, paste(variable, "after missing positions"), 0)
      near(audit$missing_after_input_rows, source_rows[after_missing], paste(variable, "after missing input-row positions"), 0)
      filled <- source_rows[which(is.na(before[[variable]][source_rows]) & !is.na(after[[variable]]))]
      near(audit$imputed_input_rows, filled, paste(variable, "actual filled input rows"), 0)
      near(audit$imputed_n, length(filled), paste(variable, "actual filled count"), 0)
      if (length(filled) && is.numeric(before[[variable]])) {
        expected <- if (audit$method == "median") median(before[[variable]], na.rm = TRUE) else mean(before[[variable]], na.rm = TRUE)
        near(audit$value_raw, expected, paste(variable, "lossless fill audit"))
      }
    } else {
      check(audit$status == "dropped" && is.null(audit$missing_after_n), "Dropped column falsely reports observed output missingness")
      near(audit$imputed_n, 0, "Dropped column cannot claim imputed values", 0)
    }
  }
  check(identical(sha(context$input), input_hash), "External source changed")
  check(file.exists(file.path(added, "output.md")), "Normal output missing")
  if (isTRUE(change$applied)) {
    check(identical(sha(working(context)), change$output$data_sha256), "Current working data differ from output version")
    backup <- file.path(context$directory, change$backup_path)
    check(file.exists(backup) && identical(sha(backup), change$input$data_sha256), "Verified before-data backup missing")
    check(identical(sha(file.path(context$directory, "dictionary.json")), change$output$dictionary_sha256), "Current dictionary differs from output version")
  }
  check(!dir.exists(file.path(context$directory, ".analysis-lock")) && !dir.exists(file.path(context$project, ".publication-lock")) &&
    !dir.exists(file.path(context$directory, ".import-lock")), "Completed run retained owned lock")
  list(path = added, request_path = request_path, request = req, result = res, change = change, before = before, after = after,
    values = res$results, dictionary = read_json(file.path(context$project, change$output$dictionary_path)), markdown = read_text(file.path(added, "output.md")), log = log)
}
failed <- function(context, options, source = TRUE, module = "missings") {
  initialized <- file.exists(working(context)); before <- snapshot(protected(context)); versions <- tree(file.path(context$directory, "versions")); old <- runs(context)
  log <- run_cli(context, c(if (source) context$source, if (is.list(options)) tokens(options) else options), module, failure = TRUE)
  if (initialized) {
    check(identical(before, snapshot(protected(context))), "Failed request changed protected data/dictionary/report/log/manifest")
    # A failed prepared change may retain its own immutable diagnostic snapshot;
    # previously published versions must nevertheless remain byte-for-byte intact.
    check(identical(versions, snapshot(names(versions))), "Failed request changed earlier versions")
  }
  for (path in setdiff(runs(context), old)) {
    res <- read_json(file.path(path, "result.json"))
    check(res$status == "failed" && !file.exists(file.path(path, "output.md")), "Failed request published successful output")
  }
  invisible(log)
}
replay <- function(context, original) {
  old <- tree(original$path)
  paths <- c(working(context), file.path(context$directory, c("import.json", "dictionary.json", "codebook.md")),
    list.files(file.path(context$directory, "backup"), full.names = TRUE))
  current <- snapshot(paths)
  again <- run(context, c("--request", original$request_path), source = FALSE, module = "replay_run")
  check(isFALSE(again$change$applied), "Replay activated output data")
  check(is.null(again$change$backup_path) || !nzchar(again$change$backup_path), "Replay created backup")
  check(identical(again$request$replay_of, original$request$run_id), "Replay origin absent")
  check(identical(again$change$output$data_sha256, original$change$output$data_sha256), "Replay output bytes differ")
  check(identical(again$change$output$dictionary_sha256, original$change$output$dictionary_sha256), "Replay dictionary bytes differ")
  check(identical(again$markdown, original$markdown), "Deterministic replay Markdown differs")
  check(identical(current, snapshot(paths)) && identical(old, tree(original$path)), "Replay changed working data/metadata/backup/original run")
  again
}

# Direct computations below are deliberately independent of production helpers.
golden_vars <- c("age", "income", "pre_score", "group2", "satisfaction", "high_missing_var")
golden <- read.csv(file.path(repo, "tests/values/missings_golden.csv"), stringsAsFactors = FALSE, na.strings = NULL)
golden_check <- function(context, actual, case_id) {
  expected <- golden[golden$case_id == case_id, ]
  for (table in unique(expected$table)) {
    selected <- expected[expected$table == table, ]
    owner <- if (table == "metrics") actual$values else actual$values[[table]]
    if (table != "metrics") near(nrow(owner), max(selected$row), paste("Golden row count", table), 0)
    for (i in seq_len(nrow(selected))) {
      row <- selected[i, ]; observed <- if (table == "metrics") owner[[row$key]] else owner[[row$key]][row$row]
      if (row$kind == "number") near(observed, as.numeric(row$value), paste(case_id, table, row$row, row$key))
      else if (row$kind == "missing") check(is.na(observed), "Golden expected missing cell")
      else same(observed, row$value, paste(case_id, table, row$row, row$key))
    }
  }
  # Immutable after-version is a second projection of the same reference data.
  for (variable in unique(expected$key[expected$table == "transformed_df"])) {
    rows <- expected[expected$table == "transformed_df" & expected$key == variable, ]
    if (all(rows$kind == "number")) near(actual$after[[variable]], as.numeric(rows$value), paste("Immutable golden", variable))
    else same(actual$after[[variable]], rows$value, paste("Immutable golden", variable))
  }
  log <- file.path(context$base, "golden-check.log")
  status <- system2(Sys.getenv("PYTHON_BIN", "python3"), shQuote(c(file.path(repo, "tests/values/check_missings_values.py"),
    file.path(context$directory, "analysis_log.jsonl"), "0", file.path(repo, "tests/values/missings_golden.csv"), case_id)), stdout = log, stderr = log)
  check(status == 0L, paste("Independent JSONL golden checker:", read_text(log)))
}
for (method in c("auto", "listwise", "impute", "indicator", "drop")) test(paste0("missings_", method, "_independent_goldens_smoke"), {
  data <- read.csv(absolute(cfg$golden_dataset), stringsAsFactors = FALSE)
  context <- new_case(paste0("golden-", method), data)
  actual <- run(context, list(vars = paste(golden_vars, collapse = ","), method = method,
    `indicator-threshold` = .1, `drop-threshold` = .3, `max-patterns` = 3))
  golden_check(context, actual, method)
})

for (item in list(c(0, "listwise"), c(.05, "listwise"), c(.10, "impute"), c(.20, "impute"),
  c(.25, "indicator"), c(.40, "indicator"), c(.45, "drop"))) test(paste0("missings_auto_boundary_", item[1]), {
  proportion <- as.numeric(item[1]); x <- seq_len(20); if (proportion > 0) x[seq_len(20 * proportion)] <- NA
  actual <- run(new_case(paste0("boundary-", proportion), data.frame(id = 1:20, x = x)), list(vars = "x", method = "auto"))
  same(actual$values$method_selected, item[2], "Auto threshold uses inclusive upper boundary")
  near(actual$values$summary_df$missing_prop, proportion, "Missing proportion")
  if (proportion > 0) check(grepl("MCAR|MAR|MNAR|mechanism", actual$markdown), "Auto output lacks missingness-mechanism limitation")
})
test("missings_auto_custom_ordered_thresholds", {
  data <- data.frame(id = 1:10, x = c(NA, NA, 3:10))
  actual <- run(new_case("custom-auto", data), list(vars = "x", method = "auto", `low-threshold` = .2,
    `moderate-threshold` = .5, `high-threshold` = .7))
  same(actual$values$method_selected, "listwise", "Custom low threshold honored")
  near(actual$after$id, 3:10, "Exact retained row IDs", 0)
})
test("missings_listwise_selected_rows_and_unselected_missing", {
  data <- data.frame(id = 1:6, x = c(1, NA, 3, 4, NA, 6), other = c(NA, 2, NA, 4, 5, 6))
  actual <- run(new_case("listwise-rows", data), list(vars = "x", method = "listwise"))
  keep <- which(!is.na(data$x))
  near(actual$after$id, data$id[keep], "Selected-variable listwise deletion", 0)
  near(actual$request$design$source_rows, keep, "Request retained input-row indices", 0)
  near(actual$values$source_rows, keep, "Result retained input-row indices", 0)
  near(actual$after$other, data$other[keep], "Unselected missing values remain")
  near(actual$values$rows_removed, 2, "Rows removed", 0)
})
test("missings_listwise_complete_case_percentage_and_pattern_order", {
  data <- data.frame(id = 1:5, a = c(1, NA, 3, NA, 5), b = c(NA, 2, 3, NA, 5))
  actual <- run(new_case("pattern-order", data), list(vars = "b,a", method = "impute", `max-patterns` = 2))
  patterns <- actual$values$patterns_df
  same(patterns$pattern, c("OO", "MM", "Other patterns"), "Frequency ranking and lexical tie order")
  near(patterns$n, c(2, 1, 2), "Pattern count including Other", 0)
  near(patterns$pct_total, c(40, 20, 40), "Patterns use original full N")
  same(actual$values$summary_df$variable, c("b", "a"), "User variable order preserved")
  check(grepl("2 of 5|40", actual$markdown), "Complete-case denominator absent")
})
for (n in c(1L, 2L, 4L)) test(paste0("missings_pattern_limit_", n), {
  data <- data.frame(id = 1:4, a = c(1, NA, 3, NA), b = c(1, 2, NA, NA))
  actual <- run(new_case(paste0("patterns-", n), data), list(vars = "a,b", method = "impute", `max-patterns` = n))
  near(sum(actual$values$patterns_df$n), 4, "Pattern aggregation conserves observations", 0)
  near(sum(actual$values$patterns_df$pct_total), 100, "Pattern aggregation conserves percentages")
  near(nrow(actual$values$patterns_df), min(n, 4L) + as.integer(n < 4L), "Top-N plus Other row", 0)
})
for (item in list(list(name = "symmetric", x = c(-2, -1, 1, 2, NA), method = "mean"),
  list(name = "skewed", x = c(1, 1, 1, 1, 100, NA), method = "median"),
  list(name = "constant", x = c(3, 3, 3, NA), method = "mean"),
  list(name = "one_donor", x = c(7, NA, NA), method = "mean"),
  list(name = "two_donors", x = c(1, 2, NA), method = "mean"))) test(paste0("missings_numeric_", item$name), {
  data <- data.frame(id = seq_along(item$x), x = item$x)
  actual <- run(new_case(item$name, data), list(vars = "x", method = "impute"))
  expected <- item$x; expected[is.na(expected)] <- if (item$method == "median") median(item$x, na.rm = TRUE) else mean(item$x, na.rm = TRUE)
  near(actual$after$x, expected, "Independent numeric imputation")
  same(actual$values$summary_df$impute_method, item$method, "Selected imputation method")
})
for (threshold in c(0, 100)) test(paste0("missings_skew_threshold_", threshold), {
  x <- c(1, 1, 1, 1, 100, NA)
  actual <- run(new_case(paste0("skew-", threshold), data.frame(id = seq_along(x), x = x)),
    list(vars = "x", method = "impute", `skew-threshold` = threshold))
  selected <- if (threshold == 0) "median" else "mean"
  near(tail(actual$after$x, 1), if (selected == "median") median(x, na.rm = TRUE) else mean(x, na.rm = TRUE), "Threshold changes mean/median")
  same(actual$values$summary_df$impute_method, selected, "Skew threshold selected method")
})
test("missings_sample_sd_skew_not_population_sd", {
  x <- c(0, 0, 1, NA); observed <- x[!is.na(x)]
  sample_skew <- mean((observed - mean(observed))^3) / sd(observed)^3
  population_skew <- mean((observed - mean(observed))^3) / mean((observed - mean(observed))^2)^1.5
  threshold <- (sample_skew + population_skew) / 2
  actual <- run(new_case("sample-sd", data.frame(id = seq_along(x), x = x)), list(vars = "x", method = "impute", `skew-threshold` = threshold))
  same(actual$values$summary_df$impute_method, "mean", "Legacy sample-SD skew definition retained")
  near(tail(actual$after$x, 1), 1 / 3, "Raw mean is not rounded to report digits")
})
test("missings_skew_positive_rescaling_underflow_overflow", {
  original <- c(rep(0, 9), 1, NA)
  for (scale in c(1, 1e-300, 1e300)) {
    x <- original * scale
    actual <- run(new_case(paste0("skew-rescale-", scale), data.frame(id = seq_along(x), x = x)), list(vars = "x", method = "impute"))
    same(actual$values$summary_df$impute_method, "median", "Positive scaling cannot change skew-selected method")
    check(identical(as.numeric(tail(actual$after$x, 1)), 0), "Skewed median must be exact zero even for tiny nonconstant donors")
    near(actual$after$x / scale, c(rep(0, 9), 1, 0), "Scaled-normalized raw imputation")
  }
  scale <- 1e-107; x <- original * scale
  actual <- run(new_case("skew-finite-underflow", data.frame(id = seq_along(x), x = x)),
    list(vars = "x", method = "impute", `skew-threshold` = 2.4))
  same(actual$values$summary_df$impute_method, "mean", "Finite underflow must not inflate skew above threshold 2.4")
  near(actual$after$x / scale, c(rep(0, 9), 1, .1), "Mean fill retains normalized tiny-value precision")
})
test("missings_raw_values_not_rounded_by_digits", {
  data <- data.frame(id = 1:4, x = c(0, 0, 1, NA))
  actual <- run(new_case("digits", data), list(vars = "x", method = "impute", digits = 0))
  near(tail(actual$after$x, 1), 1 / 3, "Immutable output retains full precision")
  near(tail(actual$values$transformed_df$x, 1), 1 / 3, "Private JSON retains full precision")
})
test("missings_modes_ties_factor_order_logical", {
  data <- data.frame(id = 1:5, text = c("z", "a", "z", "a", NA),
    category = factor(c("z", "a", "z", "a", NA), levels = c("z", "unused", "a")),
    ordered = ordered(c("high", "low", "high", "low", NA), levels = c("low", "high")),
    flag = c(TRUE, FALSE, TRUE, FALSE, NA))
  actual <- run(new_case("mode-ties", data), list(vars = "text,category,ordered,flag", method = "impute"))
  same(actual$after$text, c("z", "a", "z", "a", "a"), "Character tie uses sorted table order")
  same(as.character(actual$after$category), c("z", "a", "z", "a", "z"), "Factor tie uses level order")
  same(as.character(actual$after$ordered), c("high", "low", "high", "low", "low"), "Ordered factor mode")
  check(is.factor(actual$after$category) && identical(levels(actual$after$category), levels(data$category)), "Factor levels lost")
  check(is.ordered(actual$after$ordered), "Ordered factor class lost")
  check(is.logical(actual$after$flag), "Logical imputation changed storage type")
  same(actual$after$flag, c(TRUE, FALSE, TRUE, FALSE, FALSE), "Logical tied mode is FALSE")
})
test("missings_indicator_threshold_collision_and_original_masks", {
  data <- data.frame(id = 1:5, x = c(1, NA, NA, 4, 5), x_flag = 11:15, x_flag_1 = 21:25, y = c(NA, 2:5))
  actual <- run(new_case("indicator-collision", data), list(vars = "x,y", method = "indicator", `indicator-threshold` = .4, `indicator-suffix` = "_flag"))
  near(actual$after$x_flag_2, as.integer(is.na(data$x)), "Indicator uses original missing mask", 0)
  near(actual$after$x_flag, data$x_flag, "Existing similarly named variable untouched", 0)
  check(!"y_flag" %in% names(actual$after), "Below-threshold indicator was added")
  same(actual$values$summary_df$indicator, c("x_flag_2", ""), "Collision suffix recorded")
  check(!anyNA(actual$after[c("x", "y")]), "Indicator branch did not impute non-indicated variable")
})
test("missings_duplicate_variable_selection_processed_once", {
  actual <- run(new_case("duplicate-vars", data.frame(id = 1:4, x = c(1, NA, 3, 5))),
    list(vars = "x,x", method = "indicator", `indicator-threshold` = .25))
  same(actual$values$summary_df$variable, "x", "Duplicate selection has one summary row")
  check(identical(names(actual$values$handling_audit), "x"), "Duplicate selection has duplicate handling audit")
  check("x_miss" %in% names(actual$after) && !"x_miss_1" %in% names(actual$after), "Duplicate selection created repeated indicator")
  near(actual$after$x_miss, c(0, 1, 0, 0), "Unique selected variable uses original mask", 0)
})
test("missings_drop_threshold_inclusive_and_survivor_imputation", {
  data <- data.frame(id = 1:5, x = c(1, NA, NA, 4, 5), y = c(NA, 2:5), unselected = rep(NA_real_, 5))
  actual <- run(new_case("drop-inclusive", data), list(vars = "x,y", method = "drop", `drop-threshold` = .4, `indicator-threshold` = .2))
  check(!"x" %in% names(actual$after) && "unselected" %in% names(actual$after), "Drop selected wrong columns")
  near(actual$after$y, c(3.5, 2:5), "Surviving variable imputed")
  near(actual$after$y_miss, c(1, 0, 0, 0, 0), "Surviving variable indicator", 0)
  check(!"x_miss" %in% names(actual$after), "Dropped column left an indicator")
})
test("missings_all_missing_impute_is_unresolved_not_fabricated", {
  data <- data.frame(id = 1:3, x = rep(NA_real_, 3), text = rep(NA_character_, 3))
  actual <- run(new_case("all-missing", data), list(vars = "x,text", method = "impute"))
  check(all(is.na(actual$after$x)) && all(is.na(actual$after$text)), "All-missing variable received a fabricated fill")
  same(actual$values$summary_df$decision, rep("all missing", 2), "All-missing status")
  same(actual$values$summary_df$impute_method, rep("", 2), "All-missing variable falsely claims a method")
})
test("missings_all_missing_indicator_retains_unresolved_values", {
  actual <- run(new_case("all-missing-indicator", data.frame(id = 1:3, x = rep(NA_real_, 3))),
    list(vars = "x", method = "indicator", `indicator-threshold` = 1))
  check(all(is.na(actual$after$x)), "All-missing values fabricated")
  near(actual$after$x_miss, rep(1, 3), "All-missing original indicator", 0)
})
test("missings_default_all_variables_and_noop", {
  context <- new_case("no-missing", data.frame(id = 1:3, x = c(1.25, 2.5, 3.75), category = c("a", "b", "c")))
  actual <- run(context, list(method = "auto"))
  same(actual$values$summary_df$variable, names(context$data), "Default selects all variables")
  check(isTRUE(actual$change$unchanged) && isFALSE(actual$change$applied), "No-op claims a data change")
  check(identical(actual$change$input$version_id, actual$change$output$version_id), "No-op invented version")
  check(!length(list.files(file.path(context$directory, "backup"))), "No-op made a backup")
  replay(context, actual)
})

for (format in c("csv", "RData", "parquet", "sav")) test(paste0("missings_import_", format), {
  data <- data.frame(id = 1:4, x = c(1, NA, 3, 5))
  actual <- run(new_case(paste0("format-", format), data, format), list(vars = "x", method = "impute"))
  near(actual$after$x, c(1, 3, 3, 5), paste("Independent imported values", format))
})
test("missings_temporal_date_timestamp_duration", {
  data <- data.frame(id = 1:4, date = as.Date(c("2026-01-01", "2026-01-03", NA, "2026-01-05")),
    stamp = as.POSIXct(c("2026-01-01 12:00:00", "2026-01-01 12:00:01", NA, "2026-01-01 12:00:03"), tz = "Europe/Berlin"),
    elapsed = as.difftime(c(1, 2, NA, 4), units = "hours"))
  context <- new_case("temporal", data)
  actual <- run(context, list(vars = "date,stamp,elapsed", method = "impute"))
  for (variable in c("date", "stamp", "elapsed")) {
    expected <- as.numeric(data[[variable]]); expected[is.na(expected)] <- mean(expected, na.rm = TRUE)
    near(actual$after[[variable]], expected, paste("Temporal underlying units", variable), 1e-12)
  }
  storage <- actual$dictionary$storage
  check(storage$date$kind == "Date" && storage$stamp$kind == "POSIXct", "Temporal type metadata lost")
  check("Europe/Berlin" %in% storage$stamp$timezone && storage$elapsed$units == "hours", "Timezone/duration unit lost")
  replay(context, actual)
})
test("missings_labelled_sav_lineage_smoke", {
  data <- data.frame(id = 1:6,
    score = haven::labelled_spss(c(1, 2, 99, 1, 2, 99), labels = c(Low = 1, High = 2, Refused = 99), na_values = 99, label = "Survey response"),
    untouched = haven::labelled(c(1, 2, 1, 2, 1, 2), labels = c(No = 1, Yes = 2), label = "Unchanged group"))
  context <- new_case("labelled-sav", data, "sav")
  actual <- run(context, list(vars = "score", method = "impute"))
  near(actual$before$score, c(1, 2, NA, 1, 2, NA), "User-missings normalized before handling")
  near(actual$after$score, c(1, 2, 1.5, 1, 2, 1.5), "Labelled numeric fills use codes, not label indices")
  near(actual$values$summary_df$missing_n, 2, "Imported missing count", 0)
  before <- read_json(file.path(context$project, actual$change$input$dictionary_path))
  check(identical(actual$dictionary$columns$untouched, before$columns$untouched), "Unselected labelled metadata changed")
  check(identical(actual$dictionary$columns$score$value_labels, before$columns$score$value_labels), "Valid source value-label dictionary lost")
  check(identical(actual$dictionary$columns$score$variable_label, before$columns$score$variable_label), "Variable label lost")
  check(grepl("Survey response", actual$markdown, fixed = TRUE), "Human-facing variable label missing")
  run_cli(context, c("--parquet", working(context), "--vars", "score"), "descriptive_stats")
  near(arrow::read_parquet(working(context))$score, c(1, 2, 1.5, 1, 2, 1.5), "Followup must not reapply original missing observations")
})
for (kind in c("value", "range")) test(paste0("missings_filled_former_missing_code_label_conflict_", kind), {
  value <- if (kind == "value") haven::labelled_spss(c(98, 100, 99), labels = c(Legitimate = 98, `No response` = 99), na_values = 99, label = "Response")
    else haven::labelled_spss(c(98, 100, 99), labels = c(Legitimate = 98, `No response` = 99), na_range = c(98.5, 99.5), label = "Response")
  context <- new_case(paste0("label-conflict-", kind), data.frame(id = 1:3, score = value), "sav")
  actual <- run(context, list(vars = "score", method = "impute"))
  near(actual$after$score, c(98, 100, 99), "Former user-missing code becomes observed fill")
  original <- read_json(file.path(context$project, actual$change$input$dictionary_path))
  check(grepl("No response", jsonlite::toJSON(original$columns$score$value_labels), fixed = TRUE), "Original conflicting label lost from before-version")
  check(!grepl("No response", jsonlite::toJSON(actual$dictionary$columns$score$value_labels), fixed = TRUE), "Observed fill still has a user-missing display label")
  check(grepl("Legitimate", jsonlite::toJSON(actual$dictionary$columns$score$value_labels), fixed = TRUE), "Nonconflicting valid label lost")
  check(grepl("No response", jsonlite::toJSON(actual$dictionary$missing_handling$label_conflicts), fixed = TRUE), "Removed conflict label absent from historical audit")
  replay(context, actual)
  old <- runs(context)
  run_cli(context, c("--parquet", working(context), "--vars", "score", "--include-numeric", "TRUE"), "frequencies")
  added <- setdiff(runs(context), old); check(length(added) == 1L, "Following frequencies run missing")
  check(!grepl("No response", read_text(file.path(added, "output.md")), fixed = TRUE), "Following frequencies mislabels an observed fill as no response")
  near(arrow::read_parquet(working(context))$score, c(98, 100, 99), "Later loading must not reapply original missing-code rule")
})
test("missings_tagged_missing_listwise_row_provenance", {
  data <- data.frame(id = 1:6, chosen = c(1, NA, 3, 4, NA, 6),
    tagged = haven::labelled(c(1, haven::tagged_na("a"), haven::tagged_na("b"), 4, 5, 6), label = "Tagged response"))
  context <- new_case("tagged-row-map", data)
  actual <- run(context, list(vars = "chosen", method = "listwise"))
  near(actual$after$id, c(1, 3, 4, 6), "Listwise source positions", 0)
  near(actual$after$tagged, c(1, NA, 4, 6), "Unselected tagged missing follows retained row")
  near(actual$dictionary$missing_handling$source_rows, c(1, 3, 4, 6), "Dictionary explicit input-row lineage", 0)
  run_cli(context, c("--parquet", working(context), "--vars", "tagged"), "descriptive_stats")
  near(arrow::read_parquet(working(context))$tagged, c(1, NA, 4, 6), "Reload cannot interpret source-row tags as output-row positions")
})
test("missings_listwise_then_transform_retains_original_source_dictionary", {
  data <- data.frame(id = 1:6, chosen = c(1, NA, 3, 4, NA, 6),
    tagged = haven::labelled(c(1, haven::tagged_na("a"), haven::tagged_na("b"), 4, 5, 6), label = "Tagged response"))
  context <- new_case("listwise-transform-chain", data)
  run_cli(context, c(context$source, "--agent", "Codex"), "init_workspace")
  first <- run(context, list(vars = "chosen", method = "listwise"), source = FALSE)
  old <- runs(context)
  run_cli(context, c("--rename", "tagged:renamed", "--calc", "doubled=chosen*2"), "data_transform")
  paths <- setdiff(runs(context), old); check(length(paths) == 1L, "Transformation chain run missing")
  change <- read_json(file.path(paths, "result.json"))$results$data_change
  dictionary <- read_json(file.path(context$project, change$output$dictionary_path))
  near(dictionary$source_rows, 6, "Dictionary original source N must not reset to retained N", 0)
  before <- read_json(file.path(context$project, first$change$input$dictionary_path))
  check(identical(dictionary$columns$renamed$missing, before$columns$tagged$missing), "Original tagged-missing observations changed by row filtering/rename")
  check(identical(dictionary$columns$renamed$variable_label, before$columns$tagged$variable_label), "Original renamed variable label lost")
  request <- read_json(file.path(paths, "request.json"))
  near(request$design$source_rows, 1:4, "Transform source map addresses its immediate input version", 0)
  run_cli(context, c("--vars", "renamed,doubled"), "descriptive_stats")
  current <- as.data.frame(arrow::read_parquet(working(context)))
  near(current$renamed, c(1, NA, 4, 6), "Transform/followup preserve actual retained missing mask")
  near(current$doubled, c(2, 6, 8, 12), "Transform chain numeric reference")
})
test("missings_followup_active_project_uses_after_version", {
  context <- new_case("active-followup", data.frame(id = 1:4, x = c(1, NA, 3, 5)))
  run_cli(context, c(context$source, "--agent", "Codex"), "init_workspace")
  actual <- run(context, list(vars = "x", method = "impute"), source = FALSE)
  old <- runs(context)
  run_cli(context, c("--vars", "x"), "descriptive_stats")
  added <- setdiff(runs(context), old)
  check(length(added) == 1L, "Followup did not publish exactly one run")
  request <- read_json(file.path(added, "request.json"))
  check(identical(request$dataset$version_id, actual$change$output$version_id), "Followup selected stale before-data")
})
test("missings_replay_frozen_before_version_after_later_change_smoke", {
  context <- new_case("replay-frozen", data.frame(id = 1:5, x = c(1, NA, 3, 4, 5), y = c(NA, 2, 3, 4, 5)))
  original <- run(context, list(vars = "x", method = "listwise"))
  later <- run(context, c("--parquet", working(context), "--vars", "y", "--method", "impute"), source = FALSE)
  check(!identical(later$change$output$data_sha256, original$change$output$data_sha256), "Replay fixture did not change working data")
  again <- replay(context, original)
  near(again$after$id, c(1, 3, 4, 5), "Replay original before-version rows", 0)
})
for (kind in c("request", "input_data", "output_data", "output_dictionary", "lineage")) test(paste0("missings_replay_tampered_", kind, "_refused"), {
  context <- new_case(paste0("tamper-", kind), data.frame(id = 1:4, x = c(1, NA, 3, 5)))
  actual <- run(context, list(vars = "x", method = "impute"))
  path <- switch(kind, request = actual$request_path,
    input_data = file.path(context$project, actual$change$input$snapshot_path),
    output_data = file.path(context$project, actual$change$output$snapshot_path),
    output_dictionary = file.path(context$project, actual$change$output$dictionary_path),
    lineage = file.path(actual$path, "data-change.json"))
  connection <- file(path, open = "ab"); writeBin(charToRaw(" "), connection); close(connection)
  failed(context, c("--request", actual$request_path), source = FALSE, module = "replay_run")
})
test("missings_private_config_and_cli_override", {
  data <- data.frame(id = 1:4, x = c(1, NA, 3, 5), category = c("a", "b", "c", "d"))
  yaml::write_yaml(list(defaults = list(digits = 7L), modules = list(missings = list(method = "listwise", vars_default = "numeric", indicator_suffix = "_absent"))), config)
  configured <- run(new_case("configured", data))
  same(configured$values$method_selected, "listwise", "Private method default honored")
  same(configured$values$summary_df$variable, c("id", "x"), "Private variable selector default honored")
  explicit <- run(new_case("cli-override", data), list(vars = "x", method = "indicator", `indicator-threshold` = .25))
  same(explicit$values$method_selected, "indicator", "CLI overrides private method default")
  same(explicit$values$summary_df$variable, "x", "CLI overrides private variable selector default")
  near(explicit$after$x_absent, c(0, 1, 0, 0), "Private suffix used with CLI method", 0)
})
test("missings_log_off_preserves_mandatory_bundle", {
  context <- new_case("log-off", data.frame(id = 1:4, x = c(1, NA, 3, 5)))
  actual <- run(context, list(vars = "x", method = "impute", log = FALSE, `user-prompt` = "Handle selected missings"))
  check(!file.exists(file.path(context$directory, "analysis_log.jsonl")), "Log FALSE emitted optional legacy log")
  check(file.exists(file.path(actual$path, "data-change.json")) && file.exists(file.path(actual$path, "codebook.md")), "Log FALSE removed mandatory audit artifacts")
})
test("missings_template_override_both_tables_and_semantic_tokens", {
  context <- new_case("template", data.frame(id = 1:4, x = c(1, NA, 3, 5)))
  template <- file.path(context$base, "custom.md")
  writeLines(c("---", "table:", "  columns:", "    - key: variable", "      label: Named variable", "    - key: missing_n", "      label: Absent count",
    "patterns_table:", "  columns:", "    - key: pattern", "      label: Pattern code", "    - key: 'n'", "      label: Pattern frequency", "---",
    cfg$template_marker, "{{summary_table_body}}", "{{patterns_table_body}}", "{{summary_note_body}}", "{{patterns_note_body}}", "{{narrative_default}}"), template)
  actual <- run(context, list(vars = "x", method = "impute", template = template))
  for (text in c(cfg$template_marker, "Named variable", "Absent count", "Pattern code", "Pattern frequency"))
    check(grepl(text, actual$markdown, fixed = TRUE), paste("Custom template missing", text))
  check(!grepl("{{", actual$markdown, fixed = TRUE), "Unresolved template token")
})
for (method in c("listwise", "impute", "indicator", "drop", "auto")) test(paste0("missings_empty_input_", method), {
  context <- new_case(paste0("empty-", method), data.frame(id = integer(), x = numeric()))
  actual <- run(context, list(vars = "x", method = method))
  near(nrow(actual$after), 0, "Empty input retains zero rows", 0)
  check(is.na(actual$values$summary_df$missing_prop) && is.na(actual$values$summary_df$missing_pct), "Empty denominator fabricated a percentage")
  near(if (is.data.frame(actual$values$patterns_df)) nrow(actual$values$patterns_df) else length(actual$values$patterns_df), 0, "Empty input has no observed patterns", 0)
  check(grepl("empty|zero|no rows|no observations", actual$markdown, ignore.case = TRUE), "Empty-input interpretation limitation missing")
})
test("missings_listwise_zero_remaining_rows", {
  actual <- run(new_case("zero-remaining", data.frame(id = 1:3, x = rep(NA_real_, 3))), list(vars = "x", method = "listwise"))
  near(nrow(actual$after), 0, "Listwise all-missing removes every row", 0)
  near(actual$values$rows_removed, 3, "All deleted rows counted", 0)
  near(actual$request$design$source_rows, integer(), "Empty retained-row map", 0)
})
for (item in list(list(name = "NaN", x = c(1, NaN, 3, 5), failure = FALSE),
  list(name = "positive_infinity", x = c(1, Inf, NA, 5), failure = TRUE),
  list(name = "negative_infinity", x = c(1, -Inf, NA, 5), failure = TRUE))) test(paste0("missings_nonfinite_", item$name), {
  context <- new_case(paste0("nonfinite-", item$name), data.frame(id = seq_along(item$x), x = item$x))
  if (item$failure) failed(context, list(vars = "x", method = "impute"))
  else near(run(context, list(vars = "x", method = "impute"))$after$x, c(1, 3, 3, 5), "NaN counts as missing")
})
test("missings_nonfinite_unselected_values_preserved", {
  data <- data.frame(id = 1:4, x = c(1, NA, 3, 5), other = c(1, Inf, -Inf, NA))
  actual <- run(new_case("unselected-infinity", data), list(vars = "x", method = "impute"))
  near(actual$after$other, data$other, "Unselected nonfinite values remain unchanged")
})
for (item in list(
  list(name = "unknown_flag", argv = c("--unknown-missings-option", "TRUE")),
  list(name = "bare_method", argv = c("--method")),
  list(name = "bad_method", argv = c("--method", "multiple")),
  list(name = "bare_threshold", argv = c("--low-threshold")),
  list(name = "nonfinite_threshold", argv = c("--low-threshold", "Inf")),
  list(name = "negative_threshold", argv = c("--drop-threshold", "-0.1")),
  list(name = "large_threshold", argv = c("--indicator-threshold", "1.1")),
  list(name = "unordered_thresholds", argv = c("--low-threshold", ".3", "--moderate-threshold", ".2")),
  list(name = "fractional_patterns", argv = c("--max-patterns", "1.5")),
  list(name = "zero_patterns", argv = c("--max-patterns", "0")),
  list(name = "nonfinite_patterns", argv = c("--max-patterns", "Inf")),
  list(name = "negative_skew", argv = c("--skew-threshold", "-1")),
  list(name = "fractional_digits", argv = c("--digits", "1.2")),
  list(name = "bad_log", argv = c("--log", "perhaps")),
  list(name = "missing_variable", argv = c("--vars", "not_present")),
  list(name = "multiple_sources", argv = c("--csv", "not-present.csv"))
)) test(paste0("missings_invalid_", item$name, "_refused"), {
  context <- new_case(paste0("invalid-", item$name), data.frame(id = 1:4, x = c(1, NA, 3, 5)))
  # Prime a valid no-op so refusal must preserve all already published state.
  run(context, list(vars = "id", method = "impute"))
  failed(context, item$argv)
})
test("missings_drop_all_columns_refused", {
  context <- new_case("drop-all", data.frame(x = rep(NA_real_, 3)))
  failed(context, list(vars = "x", method = "drop", `drop-threshold` = .5))
})
for (kind in c("analysis", "import", "publication")) test(paste0("missings_lock_conflict_", kind), {
  context <- new_case(paste0("lock-", kind), data.frame(id = 1:4, x = c(1, NA, 3, 5)))
  run(context, list(vars = "id", method = "impute"))
  lock <- file.path(if (kind == "publication") context$project else context$directory, paste0(".", kind, "-lock"))
  dir.create(lock); writeLines("fixture lock", file.path(lock, "owner"))
  failed(context, list(vars = "x", method = "impute"))
  check(dir.exists(lock) && identical(read_text(file.path(lock, "owner")), "fixture lock"), "Foreign lock removed")
})

if (!length(results)) stop("No tests selected")
check(!anyDuplicated(registered_tests), "Duplicate acceptance case names")
summary_path <- file.path(work, "results.json")
write_json(list(schema_version = 1L, suite = "phase2-missings", execution_contract = "resolved-request-v1", test_pattern = pattern,
  available_cases = length(registered_tests), available_test_names = registered_tests,
  started_utc = started_utc, finished_utc = utc(), source_changed_during_run = !identical(source_at_start, setNames(vapply(production_paths, sha, character(1)), names(source_at_start))),
  source_sha256 = list(missings = sha(file.path(repo, "scripts/R/missings.R")), runner = sha(script),
    goldens = sha(file.path(repo, "tests/values/missings_golden.csv"))), numeric_checks = numeric_checks, tests = results,
  environment = list(r = R.version.string, packages = as.list(vapply(required, function(p) as.character(packageVersion(p)), character(1))))), summary_path)
passed <- sum(vapply(results, function(x) x$passed, logical(1)))
cat(sprintf("Phase 2 missings: %d/%d cases passed; %d numeric comparisons. Results: %s\n", passed, length(results), numeric_checks, summary_path))
if (!nzchar(forced) && keep > 0L) {
  dirs <- sort(list.dirs(collection, recursive = FALSE, full.names = TRUE), decreasing = TRUE)
  dirs <- dirs[grepl("^run-[0-9]{14}-[0-9]+$", basename(dirs))]
  if (length(dirs) > keep) for (path in setdiff(tail(dirs, -keep), work)) unlink(path, recursive = TRUE)
}
if (passed != length(results)) quit(status = 1L)
