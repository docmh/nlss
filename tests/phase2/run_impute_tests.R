#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Public CLI acceptance: independent base-R values, immutable before/after data,
# metadata, publication refusal and non-activating replay. No NLSS source oracle.
args <- commandArgs(TRUE)
script <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])[1]
repo <- normalizePath(file.path(dirname(script), "../.."), winslash = "/")
script <- normalizePath(script, winslash = "/")
if ("--help" %in% args) { cat("Usage: run_impute_tests.R [--root PATH] [--keep N] [--match REGEX]\n"); quit(status = 0) }
if (length(args) %% 2L || (length(args) && any(!args[seq(1L, length(args), 2L)] %in% c("--root", "--keep", "--match")))) stop("Unknown/incomplete runner option")
arg <- function(name, fallback) { i <- which(args == name); if (length(i) > 1L) stop("Repeated runner option"); if (length(i)) args[i + 1L] else fallback }
pattern <- arg("--match", ".*"); invisible(grepl(pattern, "validate regex"))
required <- c("yaml", "jsonlite", "digest", "arrow")
for (package in required) if (!requireNamespace(package, quietly = TRUE)) stop("Missing test package: ", package)
absolute <- function(path) normalizePath(if (grepl("^(/|[A-Za-z]:|\\\\)", path)) path else file.path(repo, path), winslash = "/", mustWork = FALSE)
cfg <- yaml::read_yaml(absolute(Sys.getenv("NLSS_TESTS_CONFIG", "tests/tests.yml")), eval.expr = FALSE)$tests
forced <- arg("--root", Sys.getenv("NLSS_TEST_ROOT", ""))
keep <- suppressWarnings(as.integer(arg("--keep", Sys.getenv("NLSS_KEEP_RUNS", as.character(cfg$keep_runs_default)))))
if (length(keep) != 1L || is.na(keep) || keep < 0L) stop("Invalid --keep")
collection <- file.path(if (nzchar(forced)) absolute(forced) else absolute(cfg$output_dir), "phase2-impute")
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
  optional <- c(if (grepl("mice|auto_engine", name)) "mice", if (grepl("knn", name)) "VIM",
    if (grepl("sav|tagged", name)) "haven", if (grepl("temporal", name)) "hms")
  absent <- optional[!vapply(optional, requireNamespace, logical(1), quietly = TRUE)]
  if (length(absent)) {
    message <- paste("Optional test package unavailable:", paste(absent, collapse = ", "))
    results[[length(results) + 1L]] <<- list(test = name, passed = NA, skipped = TRUE, numeric_checks = 0L, seconds = 0, message = message)
    cat("[SKIP] ", name, ": ", message, "\n", sep = ""); return(invisible(NULL))
  }
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
run_cli <- function(context, argv, module = "impute", failure = FALSE) {
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
run <- function(context, options = list(), source = TRUE, module = "impute") {
  old <- runs(context); input_hash <- sha(context$input)
  if (module == "impute" && is.list(options) && is.null(options$engine)) options$engine <- "simple"
  log <- run_cli(context, c(if (source) context$source, if (is.list(options)) tokens(options) else options), module)
  added <- setdiff(runs(context), old); check(length(added) == 1L, "Expected exactly one published run")
  request_path <- file.path(added, "request.json"); req <- read_json(request_path); res <- read_json(file.path(added, "result.json"))
  check(res$status == "completed" && isTRUE(req$resolved), "Unresolved or failed run")
  check(req$module == "impute" && res$module == "impute", "Wrong module identity")
  check(identical(req$run_id, basename(added)) && identical(req$run_id, res$run_id), "Run identity mismatch")
  check(identical(res$artifacts$request$sha256, sha(request_path)), "Request/result hash mismatch")
  for (artifact in res$artifacts) check(identical(sha(file.path(added, artifact$path)), artifact$sha256), "Artifact hash mismatch")
  change <- res$results$data_change
  check(is.list(change) && identical(change$input$version_id, req$dataset$version_id), "Before reference not bound to request")
  before <- read_ref(context, change$input); after <- read_ref(context, change$output)
  near(nrow(after), nrow(before), "Imputation must preserve every input row", 0)
  for (key in c("dataset_id", "source_sha256", "import_version_id")) check(identical(change$input[[key]], change$output[[key]]), paste("Changed original source identity", key))
  for (variable in names(before)) same(after[[variable]], before[[variable]], paste("Original column preserved", variable))
  for (i in seq_len(nrow(res$results$summary_df))) {
    summary <- res$results$summary_df[i, ]; variable <- summary$variable; target <- summary$target
    check(target %in% names(after) && !target %in% names(before), "Target overwrote an original column or is missing")
    missing <- is.na(before[[variable]]); filled <- which(missing & !is.na(after[[target]])); audit <- res$results$handling_audit[[variable]]
    same(after[[target]][!missing], before[[variable]][!missing], paste("Observed target values preserved", target))
    near(summary$missing_n, sum(missing), paste(variable, "original missing count"), 0)
    near(summary$missing_pct, if (length(missing)) 100 * mean(missing) else NA_real_, paste(variable, "original missing denominator"))
    near(summary$imputed_n, length(filled), paste(variable, "actual filled count"), 0)
    check(is.list(audit), paste("Missing per-variable audit", variable))
    near(audit$missing_input_rows, which(missing), paste(variable, "original missing rows"), 0)
    near(audit$imputed_input_rows, filled, paste(variable, "filled row identities"), 0)
    near(audit$remaining_missing_rows, which(is.na(after[[target]])), paste(variable, "remaining missing rows"), 0)
    near(audit$imputed_n, length(filled), paste(variable, "audit fill count"), 0)
    near(audit$remaining_missing_n, sum(is.na(after[[target]])), paste(variable, "audit remaining missing count"), 0)
    if (nzchar(summary$indicator)) near(after[[summary$indicator]], as.integer(missing), paste(variable, "original missing indicator"), 0)
  }
  check(isFALSE(res$results$inference_pooled) && res$results$completion_mode == "single_completion", "Completion falsely claims pooled inference")
  check(identical(sha(context$input), input_hash), "External source changed")
  check(file.exists(file.path(added, "output.md")), "Normal output missing")
  if (isTRUE(change$applied)) {
    check(identical(sha(working(context)), change$output$data_sha256), "Working data differ from output version")
    check(identical(sha(file.path(context$directory, change$backup_path)), change$input$data_sha256), "Verified input backup missing")
    check(identical(sha(file.path(context$directory, "dictionary.json")), change$output$dictionary_sha256), "Current dictionary differs from output version")
  }
  for (path in c(file.path(context$directory, c(".analysis-lock", ".import-lock")), file.path(context$project, ".publication-lock")))
    check(!dir.exists(path), "Completed run retained owned lock")
  list(path = added, request_path = request_path, request = req, result = res, change = change, before = before, after = after,
    values = res$results, dictionary = read_json(file.path(context$project, change$output$dictionary_path)), markdown = read_text(file.path(added, "output.md")), log = log)
}
failed <- function(context, options, source = TRUE, module = "impute") {
  initialized <- file.exists(working(context)); before <- snapshot(protected(context)); versions <- tree(file.path(context$directory, "versions")); old <- runs(context)
  if (module == "impute" && is.list(options) && is.null(options$engine)) options$engine <- "simple"
  log <- run_cli(context, c(if (source) context$source, if (is.list(options)) tokens(options) else options), module, failure = TRUE)
  if (initialized) {
    check(identical(before, snapshot(protected(context))), "Failed request changed protected data/metadata/report/log/manifest")
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
  current <- snapshot(paths); artifacts <- tree(file.path(context$directory, "imputations"))
  again <- run(context, c("--request", original$request_path), source = FALSE, module = "replay_run")
  check(isFALSE(again$change$applied), "Replay activated output data")
  check(is.null(again$change$backup_path) || !nzchar(again$change$backup_path), "Replay created backup")
  check(identical(again$request$replay_of, original$request$run_id), "Replay origin absent")
  check(identical(again$change$output$data_sha256, original$change$output$data_sha256), "Replay output bytes differ")
  check(identical(again$change$output$dictionary_sha256, original$change$output$dictionary_sha256), "Replay dictionary bytes differ")
  check(identical(again$markdown, original$markdown), "Deterministic replay Markdown differs")
  check(identical(current, snapshot(paths)) && identical(old, tree(original$path)), "Replay changed current data/metadata/backup/original run")
  check(identical(artifacts, tree(file.path(context$directory, "imputations"))), "Replay created or changed producer artifacts")
  again
}
mode_first <- function(x) { x <- x[!is.na(x)]; first <- unique(x); first[which.max(tabulate(match(x, first)))] }
golden_vars <- c("age", "income", "pre_score", "group2", "satisfaction", "high_missing_var")
golden <- read.csv(file.path(repo, "tests/values/impute_golden.csv"), stringsAsFactors = FALSE, na.strings = NULL)
golden_check <- function(context, actual, case_id) {
  expected <- golden[golden$case_id == case_id, ]
  for (i in seq_len(nrow(expected))) {
    row <- expected[i, ]; owner <- if (row$table == "metrics") actual$values else actual$values[[row$table]]
    observed <- if (row$table == "metrics") owner[[row$key]] else owner[[row$key]][row$row]
    if (row$kind == "number") near(observed, as.numeric(row$value), paste(case_id, row$table, row$row, row$key))
    else if (row$kind == "missing") check(is.na(observed), "Golden expected missing cell")
    else same(observed, row$value, paste(case_id, row$table, row$row, row$key))
    if (row$table == "transformed_df") same(actual$after[[row$key]][row$row], if (row$kind == "number") as.numeric(row$value) else row$value, "Immutable after-version golden")
  }
  log <- file.path(context$base, "golden-check.log")
  status <- system2(Sys.getenv("PYTHON_BIN", "python3"), shQuote(c(file.path(repo, "tests/values/check_impute_values.py"),
    file.path(context$directory, "analysis_log.jsonl"), "0", file.path(repo, "tests/values/impute_golden.csv"), case_id)), stdout = log, stderr = log)
  check(status == 0L, paste("Independent JSONL golden checker:", read_text(log)))
}
for (method in c("auto", "mean", "median", "mode", "random", "constant")) test(paste0("impute_", method, "_independent_goldens_smoke"), {
  data <- read.csv(absolute(cfg$golden_dataset), stringsAsFactors = FALSE)
  context <- new_case(paste0("golden-", method), data)
  actual <- run(context, list(vars = paste(golden_vars, collapse = ","), engine = "simple", `numeric-method` = method,
    `categorical-method` = if (method %in% c("random", "constant")) method else "mode", indicator = TRUE, seed = 811,
    `value-map` = "age=7|income=7|pre_score=7|group2=Missing|satisfaction=7|high_missing_var=7"))
  golden_check(context, actual, method)
  template <- actual$request$templates[["impute.main"]]
  check(is.list(template) && is.character(template$path) && nzchar(template$path), "Default impute.main template was not preserved")
  canonical_hash <- sha(file.path(repo, "assets/impute/default-template.md"))
  check(identical(template$sha256, canonical_hash) && identical(sha(file.path(actual$path, template$path)), canonical_hash),
    "Saved default template differs from the canonical impute asset")
  check(grepl("(?m)^#+[ \\t]+Imputation Summary[ \\t]*$", actual$markdown, perl = TRUE), "Default Imputation Summary heading missing")
})
test("impute_modes_first_observation_ties_and_factor_order", {
  data <- data.frame(x = c(8, 2, 8, 2, NA), text = c("z", "a", "z", "a", NA),
    category = factor(c("z", "a", "z", "a", NA), levels = c("a", "unused", "z")),
    ordered = ordered(c("high", "low", "high", "low", NA), levels = c("low", "high")), flag = c(TRUE, FALSE, TRUE, FALSE, NA))
  actual <- run(new_case("mode-ties", data), list(`numeric-method` = "mode"))
  for (variable in names(data)) same(tail(actual$after[[paste0(variable, "_imp")]], 1), data[[variable]][1], paste(variable, "first-observed tie"))
  check(identical(levels(actual$after$category_imp), levels(data$category)) && is.ordered(actual$after$ordered_imp), "Factor levels or ordered class changed")
  check(is.logical(actual$after$flag_imp), "Logical mode changed storage")
})
test("impute_random_single_numeric_donor_never_samples_one_to_value", {
  data <- data.frame(x = c(17, rep(NA, 20)), y = c(-4.5, rep(NA, 20)))
  actual <- run(new_case("one-donor", data), list(`numeric-method` = "random", seed = 47))
  near(actual$after$x_imp, rep(17, 21), "One integer-valued donor", 0)
  near(actual$after$y_imp, rep(-4.5, 21), "One negative fractional donor", 0)
})
test("impute_random_default_seed_and_explicit_seed", {
  data <- data.frame(x = c(3, 8, 12, rep(NA, 8)), text = c("z", "a", "q", rep(NA, 8)))
  left <- run(new_case("default-seed", data), list(`numeric-method` = "random", `categorical-method` = "random"))
  right <- run(new_case("explicit-seed", data), list(`numeric-method` = "random", `categorical-method` = "random", seed = 1))
  same(left$after$x_imp, right$after$x_imp, "Default seed equals explicit 1")
  same(left$after$text_imp, right$after$text_imp, "Categorical default draws")
  set.seed(1); expected <- data
  for (name in names(data)) { missing <- is.na(data[[name]]); observed <- data[[name]][!missing]; expected[[name]][missing] <- observed[sample.int(length(observed), sum(missing), TRUE)] }
  near(left$after$x_imp, expected$x, "Independent default numeric draws")
  same(left$after$text_imp, expected$text, "Independent default categorical draws")
})
test("impute_allmissing_constant_counts_fills_and_preserves_sources", {
  data <- data.frame(x = rep(NA_real_, 4), text = rep(NA_character_, 4), flag = rep(NA, 4), category = factor(rep(NA_character_, 4), levels = c("a", "b")))
  actual <- run(new_case("allmissing-constant", data), list(`numeric-method` = "constant", `categorical-method` = "constant", `value-map` = "x=4.25|text=Missing|flag=true|category=new", indicator = TRUE))
  near(actual$after$x_imp, rep(4.25, 4), "All-missing numeric constant")
  same(actual$after$text_imp, rep("Missing", 4), "All-missing text constant")
  check(all(actual$after$flag_imp) && "new" %in% levels(actual$after$category_imp), "Logical/factor constants lost")
  near(actual$values$summary_df$imputed_n, rep(4, 4), "Actual all-missing constant count", 0)
  check(length(actual$values$skipped_vars) == 0L, "Successfully filled all-missing variable was falsely skipped")
})
for (engine in c("simple", "mice", "knn")) test(paste0("impute_allmissing_unresolved_", engine), {
  data <- data.frame(x = rep(NA_real_, 3), text = rep(NA_character_, 3))
  actual <- run(new_case(paste0("allmissing-", engine), data), list(engine = engine, indicator = TRUE))
  check(all(is.na(actual$after$x_imp)) && all(is.na(actual$after$text_imp)), "Absent donors fabricated data")
  near(actual$values$summary_df$imputed_n, c(0, 0), "No donor means zero fills", 0)
  check(is.null(actual$values$imputation_artifact), "No engine run fabricated a mids artifact")
})
test("impute_names_collisions_duplicates_indicators_and_maps", {
  data <- data.frame(x = c(1, NA, 3, 5), x_imp = 11:14, x_imp_1 = 21:24, x_miss = 31:34, text = c("a", NA, "b", "a"))
  actual <- run(new_case("names-maps", data), list(vars = "x,x,text", `numeric-method` = "mean", `categorical-method` = "random",
    `method-map` = "x=median|text=constant", `value-map` = "text=a=b", suffix = "_imp", indicator = TRUE))
  same(actual$values$summary_df$variable, c("x", "text"), "Repeated variable processed once")
  same(actual$values$summary_df$target, c("x_imp_2", "text_imp"), "Target collisions resolved")
  near(actual$after$x_imp_2, c(1, 3, 3, 5), "Method-map median")
  same(actual$after$text_imp, c("a", "a=b", "b", "a"), "Value-map preserves literal equals")
  near(actual$after$x_miss_1, c(0, 1, 0, 0), "Indicator collisions resolved", 0)
})
test("impute_custom_suffix_shared_with_indicator_never_overwrites", {
  data <- data.frame(x = c(1, NA, 3))
  actual <- run(new_case("same-suffix", data), list(vars = "x", suffix = "_new", indicator = TRUE, `indicator-suffix` = "_new"))
  near(actual$after$x_new, c(1, 2, 3), "Custom target suffix")
  near(actual$after$x_new_1, c(0, 1, 0), "Indicator cannot overwrite completion", 0)
})
test("impute_no_missing_still_creates_original_preserving_completion", {
  actual <- run(new_case("no-missing", data.frame(x = 1:3, text = c("a", "b", "c"))), list(indicator = TRUE))
  same(actual$values$summary_df$variable, c("x", "text"), "Default all selection")
  near(actual$values$summary_df$imputed_n, c(0, 0), "No missing values filled", 0)
  check(all(c("x_imp", "text_imp") %in% names(actual$after)) && !any(grepl("_miss", names(actual$after))), "No-missing target/indicator convention changed")
})
test("impute_empty_input_has_no_fabricated_missing_percentage", {
  actual <- run(new_case("empty", data.frame(x = numeric(), text = character())), list(indicator = TRUE))
  near(nrow(actual$after), 0, "Zero source rows remain zero", 0)
  check(all(is.na(actual$values$summary_df$missing_pct)), "Empty denominator became percentage")
})
test("impute_auto_skew_scaling_and_small_sample", {
  for (scale in c(1, 1e-300, 1e300)) {
    data <- data.frame(x = c(rep(0, 9), 1, NA) * scale)
    actual <- run(new_case(paste0("skew-scale-", scale), data), list(`numeric-method` = "auto"))
    same(actual$values$summary_df$method, "median", "Positive scale cannot change skew decision")
    near(actual$after$x_imp / scale, c(rep(0, 9), 1, 0), "Scale normalized median")
  }
  actual <- run(new_case("small-sample", data.frame(x = c(2, 5, NA))), list(`numeric-method` = "auto"))
  same(actual$values$summary_df$method, "mean", "Undefined small-sample skew selects mean")
  near(actual$after$x_imp, c(2, 5, 3.5), "Small-sample raw mean")
})
test("impute_skew_threshold_and_digits_do_not_round_data", {
  data <- data.frame(x = c(0, 0, 1, NA))
  expected_skew <- mean(((data$x[1:3] - mean(data$x[1:3])) / sd(data$x[1:3]))^3)
  actual <- run(new_case("skew-threshold", data), list(`numeric-method` = "auto", `skew-threshold` = expected_skew + .001, digits = 0))
  same(actual$values$summary_df$method, "mean", "Sample-SD skew definition retained")
  near(actual$after$x_imp, c(0, 0, 1, 1 / 3), "Report digits never round output")
  near(actual$values$handling_audit$x$value_raw, 1 / 3, "Audit retains unrounded value")
})
test("impute_method_map_auto_freezes_effective_skew_audit", {
  data <- data.frame(x = c(0, 0, 1, NA))
  actual <- run(new_case("map-auto", data), list(`method-map` = "x=auto"))
  same(actual$values$summary_df$method, "mean", "Per-variable auto overrides median default")
  near(actual$values$handling_audit$x$skewness, mean(((c(0, 0, 1) - 1 / 3) / sd(c(0, 0, 1)))^3), "Per-variable auto exposes effective skew")
})
for (format in c("csv", "rds", "RData", "parquet", "sav")) test(paste0("impute_import_", format), {
  data <- data.frame(id = 1:4, x = c(1, NA, 3, 5))
  actual <- run(new_case(paste0("format-", format), data, format), list(vars = "x", `numeric-method` = "mean"))
  near(actual$after$x_imp, c(1, 3, 3, 5), paste("Independent imported completion", format))
})
test("impute_csv_decimal_comma_explicit_types_and_missing_codes", {
  context <- new_case("csv-options", data.frame(x = 1), "csv")
  writeLines(c("id;score;category", "001;1,5;A", "002;99;B", "003;4,5;A"), context$input)
  actual <- run(context, list(vars = "score", sep = ";", `csv-decimal` = ",", `csv-col-types` = "id=character,score=numeric", `csv-na-values` = "99"))
  same(actual$after$id, c("001", "002", "003"), "Leading-zero IDs remain text")
  near(actual$after$score_imp, c(1.5, 3, 4.5), "Explicit CSV missing code normalized before fill")
})
for (method in c("mean", "constant")) test(paste0("impute_temporal_date_timestamp_duration_", method), {
  data <- data.frame(id = 1:4, date = as.Date(c("2026-01-01", "2026-01-03", NA, "2026-01-05")),
    stamp = as.POSIXct(c("2026-01-01 12:00:00", "2026-01-01 12:00:01", NA, "2026-01-01 12:00:03"), tz = "Europe/Berlin"),
    elapsed = as.difftime(c(1, 2, NA, 4), units = "hours"), clock = hms::as_hms(c(1.25, 2.5, NA, 4.75)))
  context <- new_case(paste0("temporal-", method), data)
  constants <- c(date = 20454, stamp = 1767265202.25, elapsed = 2.25, clock = 3.75)
  actual <- run(context, list(vars = "date,stamp,elapsed,clock", `numeric-method` = method,
    `value-map` = paste(paste(names(constants), constants, sep = "="), collapse = "|")))
  for (variable in names(constants)) {
    expected <- as.numeric(data[[variable]]); expected[is.na(expected)] <- if (method == "mean") mean(expected, na.rm = TRUE) else constants[[variable]]
    near(actual$after[[paste0(variable, "_imp")]], expected, paste("Temporal storage unit", variable), 1e-12)
  }
  storage <- actual$dictionary$storage
  check(storage$date_imp$kind == "Date" && storage$stamp_imp$kind == "POSIXct", "Temporal logical type lost")
  check("Europe/Berlin" %in% storage$stamp_imp$timezone && storage$elapsed_imp$units == "hours", "Timezone/duration unit lost")
  if (method == "mean") replay(context, actual)
})
test("impute_temporal_text_constants", {
  data <- data.frame(date = as.Date(c("2026-01-01", NA)), stamp = as.POSIXct(c("2026-01-01 12:00:00", NA), tz = "Europe/Berlin"))
  actual <- run(new_case("temporal-text", data), list(`numeric-method` = "constant", `value-map` = "date=2026-01-03|stamp=2026-01-01 12:00:02"))
  near(actual$after$date_imp, as.numeric(as.Date(c("2026-01-01", "2026-01-03"))), "ISO date constant")
  near(actual$after$stamp_imp, as.numeric(as.POSIXct(c("2026-01-01 12:00:00", "2026-01-01 12:00:02"), tz = "Europe/Berlin")), "Timestamp constant uses source timezone")
})
for (kind in c("value", "range")) test(paste0("impute_sav_missing_labels_original_and_derived_", kind), {
  value <- if (kind == "value") haven::labelled_spss(c(98, 100, 99), labels = c(Legitimate = 98, `No response` = 99), na_values = 99, label = "Survey response")
    else haven::labelled_spss(c(98, 100, 99), labels = c(Legitimate = 98, `No response` = 99), na_range = c(98.5, 99.5), label = "Survey response")
  context <- new_case(paste0("labels-", kind), data.frame(id = 1:3, score = value), "sav")
  actual <- run(context, list(vars = "score", `numeric-method` = "mean", indicator = TRUE))
  near(actual$before$score, c(98, 100, NA), "Source missing normalized")
  near(actual$after$score_imp, c(98, 100, 99), "Numeric labels do not imply category indices")
  original <- read_json(file.path(context$project, actual$change$input$dictionary_path))
  check(identical(actual$dictionary$columns$score, original$columns$score), "Original source dictionary metadata changed")
  check(!grepl("No response", jsonlite::toJSON(actual$dictionary$columns$score_imp$value_labels), fixed = TRUE), "Derived observed fill has source missing display label")
  check(grepl("Survey response", actual$markdown, fixed = TRUE), "Human-facing source variable label lost")
  run_cli(context, c("--parquet", working(context), "--vars", "score_imp"), "descriptive_stats")
  near(arrow::read_parquet(working(context))$score_imp, c(98, 100, 99), "Reload must not reapply original missing codes to derived values")
})
test("impute_tagged_source_rows_retained_after_listwise_chain", {
  data <- data.frame(id = 1:6, chosen = c(1, NA, 3, 4, NA, 6),
    tagged = haven::labelled(c(1, haven::tagged_na("a"), haven::tagged_na("b"), 4, 5, 6), label = "Tagged response"))
  context <- new_case("tagged-chain", data)
  run_cli(context, c(context$source, "--agent", "Codex"), "init_workspace")
  run_cli(context, c("--vars", "chosen", "--method", "listwise"), "missings")
  actual <- run(context, list(vars = "tagged", `numeric-method` = "mean"), source = FALSE)
  near(actual$after$id, c(1, 3, 4, 6), "Prior listwise row identities")
  near(actual$after$tagged_imp, c(1, 11 / 3, 4, 6), "Current-row missingness used, not historical tagged row")
  near(actual$dictionary$source_rows, 6, "Original source row count remains historical", 0)
  original <- read_json(file.path(context$project, actual$change$input$dictionary_path))
  check(identical(actual$dictionary$columns$tagged, original$columns$tagged), "Historical tagged source dictionary changed")
})

engine_fixture <- function() {
  set.seed(7601)
  d <- data.frame(x = rnorm(90), y = rnorm(90), group = factor(sample(c("A", "B", "C"), 90, TRUE)))
  d$y <- .45 * d$x + d$y
  d$x[c(1, 5, 9, 15, 21, 32)] <- NA
  d$y[c(2, 6, 10, 16, 22, 33)] <- NA
  d$group[c(3, 7, 11, 17, 23, 34)] <- NA
  d
}
check_mice <- function(context, actual, expected_mids) {
  artifact <- actual$values$imputation_artifact
  check(is.list(artifact) && artifact$object_class == "mids", "Preserved mids object missing")
  path <- file.path(context$project, artifact$path)
  check(identical(sha(path), artifact$sha256) && identical(basename(dirname(path)), paste0("mice-", artifact$sha256)), "Mids content-addressed hash mismatch")
  metadata <- read_json(file.path(dirname(path), "metadata.json")); stored <- readRDS(path)
  check(identical(metadata$dataset$version_id, actual$request$dataset$version_id), "Mids source reference not bound to pre-imputation input")
  near(stored$m, expected_mids$m, "All imputations preserved", 0)
  near(stored$iteration, expected_mids$iteration, "Actual iteration count", 0)
  check(identical(stored$method, expected_mids$method), "Public mice imputation methods differ")
  near(stored$predictorMatrix, expected_mids$predictorMatrix, "Public mice predictor matrix", 0)
  expected_completed <- mice::complete(expected_mids, action = "all")
  stored_completed <- mice::complete(stored, action = "all")
  for (variable in names(expected_mids$data)) {
    same(stored$data[[variable]], expected_mids$data[[variable]], paste(variable, "exact engine input"))
    for (i in seq_along(expected_completed)) same(stored_completed[[i]][[variable]], expected_completed[[i]][[variable]], paste(variable, "public mice completion", i))
    same(as.matrix(stored$imp[[variable]]), as.matrix(expected_mids$imp[[variable]]), paste(variable, "every original mice draw"))
    original <- expected_mids$data[[variable]]; missing <- is.na(original); completed <- original
    if (is.numeric(original)) completed[missing] <- rowMeans(vapply(expected_completed, function(d) as.numeric(d[[variable]][missing]), numeric(sum(missing))))
    else if (is.logical(original)) {
      # Public mice may expose binary draws as 0/1; completion returns to the
      # original logical measurement type after the first-draw mode decision.
      completed[missing] <- vapply(which(missing), function(row) mode_first(vapply(expected_completed,
        function(d) as.logical(d[[variable]][row]), logical(1))), logical(1))
    } else {
      replacements <- vapply(which(missing), function(row) as.character(mode_first(vapply(expected_completed, function(d) as.character(d[[variable]][row]), character(1)))), character(1))
      completed <- as.character(original); completed[missing] <- replacements
    }
    same(actual$after[[actual$values$target_map[[variable]]]], completed, paste(variable, "single completion across all draws"))
  }
  check(actual$values$completion_aggregation == "mean_numeric_mode_categorical", "Mice completion aggregation not disclosed")
  check(grepl("SINGLE COMPLETION ONLY", actual$markdown, fixed = TRUE) && grepl(artifact$sha256, actual$markdown, fixed = TRUE), "Mandatory inference notice/hash missing")
  check(grepl("SINGLE COMPLETION ONLY", read_text(actual$log), fixed = TRUE), "Console inference notice missing")
  invisible(stored)
}
for (m in c(1L, 3L)) test(paste0("impute_mice_public_draws_m", m), {
  data <- engine_fixture(); context <- new_case(paste0("mice-m", m), data)
  expected <- mice::mice(data, m = max(m, 2L), maxit = 2, printFlag = FALSE, seed = 811)
  actual <- run(context, list(engine = "mice", m = m, maxit = 2, seed = 811, indicator = TRUE))
  check_mice(context, actual, expected)
  if (m == 3L) replay(context, actual)
})
test("impute_mice_character_preparation_and_ignored_maps", {
  data <- engine_fixture(); data$group <- as.character(data$group)
  prepared <- data; prepared$group <- factor(prepared$group)
  context <- new_case("mice-character", data)
  expected <- mice::mice(prepared, m = 2, maxit = 1, printFlag = FALSE, seed = 1)
  actual <- run(context, list(engine = "mice", m = 2, maxit = 1, `method-map` = "x=constant", `value-map` = "x=777"))
  check_mice(context, actual, expected)
  check(grepl("ignored|did not use them|maps apply only", actual$markdown, ignore.case = TRUE), "Engine ignored simple maps without disclosure")
})
test("impute_mice_ordered_and_logical_public_methods", {
  data <- engine_fixture(); set.seed(44)
  data$ordered <- ordered(sample(c("low", "middle", "high"), nrow(data), TRUE), levels = c("low", "middle", "high"))
  data$flag <- sample(c(TRUE, FALSE), nrow(data), TRUE)
  data$ordered[c(4, 8, 12, 18, 24, 35)] <- NA; data$flag[c(5, 9, 13, 19, 25, 36)] <- NA
  context <- new_case("mice-logical-ordered", data)
  expected <- mice::mice(data, m = 2, maxit = 1, printFlag = FALSE, seed = 1)
  actual <- run(context, list(engine = "mice", m = 2, maxit = 1))
  check_mice(context, actual, expected)
  check(is.ordered(actual$after$ordered_imp) && is.logical(actual$after$flag_imp), "Mice completion lost ordered/logical source type")
})
test("impute_auto_engine_requested_and_effective_mice", {
  data <- engine_fixture(); context <- new_case("auto-engine", data)
  expected <- mice::mice(data, m = 2, maxit = 1, printFlag = FALSE, seed = 1)
  actual <- run(context, list(engine = "auto", m = 2, maxit = 1))
  check(actual$values$engine == "mice", "Installed mice must retain auto-engine priority")
  check(actual$request$options$engine_requested == "auto" && actual$request$options$engine == "mice", "Requested/effective engine not frozen")
  check_mice(context, actual, expected)
})
for (k in c(1L, 5L)) test(paste0("impute_knn_public_completion_k", k), {
  data <- engine_fixture(); context <- new_case(paste0("knn-k", k), data)
  set.seed(811); expected <- VIM::kNN(data, variable = names(data), k = k, imp_var = FALSE)
  actual <- run(context, list(engine = "knn", k = k, seed = 811, indicator = TRUE))
  for (variable in names(data)) same(actual$after[[paste0(variable, "_imp")]], expected[[variable]], paste(variable, "public VIM completion"))
  check(is.null(actual$values$imputation_artifact), "kNN incorrectly claims a mids artifact")
  if (k == 5L) replay(context, actual)
})
test("impute_knn_temporal_public_partial_completion", {
  data <- data.frame(x = c(1, 5, 2, 8, 4, 6), date = as.Date("2026-01-01") + c(1, 3, NA, 7, 2, 5),
    stamp = as.POSIXct("2026-01-01", tz = "Europe/Berlin") + c(1, 3, NA, 7, 2, 5), elapsed = as.difftime(c(1, 3, NA, 7, 2, 5), units = "hours"))
  context <- new_case("knn-temporal", data)
  set.seed(1); expected <- suppressWarnings(VIM::kNN(data, variable = names(data), k = 2, imp_var = FALSE))
  actual <- run(context, list(engine = "k-nn", k = 2))
  for (variable in names(data)) near(actual$after[[paste0(variable, "_imp")]], as.numeric(expected[[variable]]), paste(variable, "public VIM temporal result/missingness"))
  near(actual$values$summary_df$imputed_n, vapply(names(data), function(variable) sum(is.na(data[[variable]]) & !is.na(expected[[variable]])), integer(1)), "Partial package completion only counts actual fills", 0)
})
test("impute_mice_log_false_custom_template_keeps_mandatory_artifact", {
  context <- new_case("mice-no-log", engine_fixture()); template <- file.path(context$base, "minimal.md")
  writeLines(c("# Custom presentation", "No table requested."), template)
  actual <- run(context, list(engine = "mice", m = 2, maxit = 1, log = FALSE, template = template))
  check(!file.exists(file.path(context$directory, "analysis_log.jsonl")), "Log FALSE created JSONL projection")
  check(grepl("Custom presentation", actual$markdown, fixed = TRUE) && grepl("SINGLE COMPLETION ONLY", actual$markdown, fixed = TRUE), "Custom template suppressed mandatory scientific notice")
  check(file.exists(file.path(context$project, actual$values$imputation_artifact$path)), "Log FALSE discarded mice artifact")
  replay(context, actual)
})
test("impute_mice_same_seed_new_input_version_keeps_distinct_binding", {
  data <- engine_fixture(); context <- new_case("mice-version-binding", data)
  options <- list(vars = "x,y,group", engine = "mice", m = 2, maxit = 1, seed = 811)
  first <- run(context, options)
  preserved <- tree(file.path(context$directory, "imputations"))
  expected <- mice::mice(data, m = 2, maxit = 1, printFlag = FALSE, seed = 811)
  # Reuse the existing working dataset containing completion columns, but select
  # exactly the same original columns. Draws may match; input versions may not.
  second <- run(context, c("--parquet", working(context), tokens(options)), source = FALSE)
  check(!identical(first$request$dataset$version_id, second$request$dataset$version_id), "Followup should bind a distinct current input version")
  check(!identical(first$values$imputation_artifact$path, second$values$imputation_artifact$path), "Distinct source versions reused a falsely bound artifact")
  check(identical(preserved, snapshot(names(preserved))), "Second producer overwrote the earlier immutable artifact")
  for (actual in list(first, second)) {
    stored <- check_mice(context, actual, expected)
    check(identical(attr(stored$data, "nlss_input_version_id"), actual$request$dataset$version_id), "Engine frame provenance disagrees with producer source version")
  }
  replay(context, first); replay(context, second)
})
test("impute_simple_frozen_config_template_source_and_replay", {
  context <- new_case("frozen-replay", data.frame(x = c(0, 0, 1, NA)))
  template <- file.path(context$base, "custom.md")
  writeLines(c("---", "table:", "  columns:", "    - key: variable", "      label: Named variable", "    - key: imputed_n", "      label: Filled count", "---",
    cfg$template_marker, "{{table_body}}", "{{note_body}}", "{{narrative_default}}"), template)
  yaml::write_yaml(list(defaults = list(digits = 0L), modules = list(impute = list(numeric_method = "mean", suffix = "_completed", seed = 31L))), config)
  actual <- run(context, list(vars = "x", template = template))
  near(actual$after$x_completed, c(0, 0, 1, 1 / 3), "Private configuration supplies mean")
  check(grepl(cfg$template_marker, actual$markdown, fixed = TRUE) && grepl("Filled count", actual$markdown, fixed = TRUE), "Custom table columns not honored")
  saveRDS(data.frame(x = c(99, NA)), context$input); writeLines("Changed original template", template); reset_config()
  replay(context, actual)
})
test("impute_cli_overrides_private_defaults", {
  context <- new_case("cli-wins", data.frame(x = c(0, 0, 9, NA)))
  yaml::write_yaml(list(modules = list(impute = list(numeric_method = "mean", suffix = "_configured", indicator = TRUE))), config)
  actual <- run(context, list(`numeric-method` = "median", suffix = "_cli", indicator = FALSE))
  near(actual$after$x_cli, c(0, 0, 9, 0), "CLI method overrides config")
  check(!"x_configured" %in% names(actual$after) && !"x_miss" %in% names(actual$after), "CLI suffix/indicator lost precedence")
})
test("impute_simple_random_replay_does_not_activate_previous_output", {
  context <- new_case("random-replay", data.frame(x = c(2, 7, 12, NA, NA)))
  actual <- run(context, list(`numeric-method` = "random", seed = 19))
  run(context, list(`numeric-method` = "constant", constant = 77), source = TRUE)
  replay(context, actual)
})
test("impute_private_literal_path_constants_preserved_for_replay", {
  data <- data.frame(text = c("a", NA, "b"))
  context <- new_case("literal-path", data)
  literal <- "/external/private/person:record=value"
  actual <- run(context, list(`categorical-method` = "constant", `value-map` = paste0("text=", literal), `user-prompt` = "Fill the selected variable."))
  same(actual$after$text_imp, c("a", literal, "b"), "Literal constant is scientific data, not a rewritten path")
  check(grepl(literal, read_text(actual$request_path), fixed = TRUE), "Private request masks required constant")
  replay(context, actual)
})
test("impute_template_cannot_suppress_single_completion_limitation", {
  context <- new_case("simple-notice", data.frame(x = c(1, NA, 3)))
  template <- file.path(context$base, "minimal.md"); writeLines("# Minimal simple completion", template)
  actual <- run(context, list(template = template, log = FALSE))
  check(grepl("single|uncertainty", actual$markdown, ignore.case = TRUE), "Minimal template hides imputation uncertainty boundary")
  check(!file.exists(file.path(context$directory, "analysis_log.jsonl")), "Log FALSE created legacy log")
})
test("impute_selected_nonfinite_values_refused_unselected_preserved", {
  data <- data.frame(x = c(1, NA, 3, 5), other = c(1, Inf, -Inf, NA))
  context <- new_case("nonfinite", data)
  actual <- run(context, list(vars = "x"))
  near(actual$after$other, data$other, "Unselected infinities retained")
  failed(context, list(vars = "other"))
})
test("impute_nan_counts_as_missing", {
  actual <- run(new_case("nan", data.frame(x = c(1, NaN, 3, 5))), list(`numeric-method` = "mean"))
  near(actual$after$x_imp, c(1, 3, 3, 5), "NaN is missing, not a donor")
})
for (kind in c("matrix", "list", "custom")) test(paste0("impute_unsupported_selected_class_", kind), {
  data <- data.frame(id = 1:4)
  data$x <- switch(kind, matrix = I(matrix(1:8, nrow = 4)), list = I(list(1, 2, NULL, 4)), custom = structure(c(1, NA, 3, 4), class = "opaque_survey_number"))
  context <- new_case(paste0("unsupported-", kind), data)
  failed(context, list(vars = "x"))
})
invalid_groups <- list(
  cli = list(unknown_flag = c("--unknown-impute-option", "TRUE"), bare_engine = "--engine", bare_seed = "--seed",
    bad_engine = c("--engine", "bogus"), bad_log = c("--log", "perhaps"), bad_indicator = c("--indicator", "perhaps"),
    missing_variable = c("--vars", "absent"), multiple_sources = c("--csv", "missing.csv")),
  integer = list(fractional_m = c("--m", "1.5"), zero_m = c("--m", "0"), nonfinite_m = c("--m", "Inf"),
    fractional_maxit = c("--maxit", "1.2"), zero_maxit = c("--maxit", "0"), negative_k = c("--k", "-1"),
    fractional_k = c("--k", "2.1"), fractional_seed = c("--seed", "1.5"), huge_seed = c("--seed", "2147483648"),
    fractional_digits = c("--digits", "1.2")),
  method = list(unknown_method = c("--numeric-method", "interpolate"), categorical_mean = c("--vars", "text", "--categorical-method", "mean"),
    unknown_map_variable = c("--method-map", "absent=mean"), malformed_method_map = c("--method-map", "x"),
    unknown_value_variable = c("--value-map", "absent=3"), duplicate_method_map = c("--method-map", "x=mean|x=median")),
  constant = list(missing_constant = c("--numeric-method", "constant"), bad_numeric_constant = c("--numeric-method", "constant", "--constant", "not_a_number"),
    infinite_constant = c("--numeric-method", "constant", "--constant", "Inf"), bad_logical_constant = c("--vars", "flag", "--categorical-method", "constant", "--constant", "maybe")),
  threshold = list(negative_skew = c("--skew-threshold", "-1"), nonfinite_skew = c("--skew-threshold", "Inf")))
for (family in names(invalid_groups)) test(paste0("impute_invalid_", family, "_domains_refused_without_publication"), {
  context <- new_case(paste0("invalid-", family), data.frame(x = c(1, NA, 3, 5), text = c("a", NA, "b", "a"), flag = c(TRUE, NA, FALSE, TRUE)))
  run(context, list(vars = "x"))
  for (name in names(invalid_groups[[family]])) {
    argv <- invalid_groups[[family]][[name]]
    # Explicit simple method isolates option validation from automatic package selection.
    if (!"--engine" %in% argv) argv <- c("--engine", "simple", argv)
    tryCatch(failed(context, argv), error = function(error) stop(name, ": ", conditionMessage(error), call. = FALSE))
  }
})
for (kind in c("analysis", "import", "publication")) test(paste0("impute_foreign_lock_", kind, "_refused"), {
  context <- new_case(paste0("lock-", kind), data.frame(x = c(1, NA, 3)))
  run(context)
  lock <- file.path(if (kind == "publication") context$project else context$directory, paste0(".", kind, "-lock"))
  dir.create(lock); writeLines("foreign fixture lock", file.path(lock, "owner"))
  failed(context, list(vars = "x"))
  check(dir.exists(lock) && identical(read_text(file.path(lock, "owner")), "foreign fixture lock"), "Foreign lock removed")
})

if (!length(results)) stop("No tests selected")
check(!anyDuplicated(registered_tests), "Duplicate acceptance case names")
summary_path <- file.path(work, "results.json")
environment_packages <- c(required, c("mice", "VIM", "haven", "hms")[vapply(c("mice", "VIM", "haven", "hms"), requireNamespace, logical(1), quietly = TRUE)])
write_json(list(schema_version = 1L, suite = "phase2-impute", execution_contract = "resolved-request-v1", test_pattern = pattern,
  available_cases = length(registered_tests), available_test_names = registered_tests,
  started_utc = started_utc, finished_utc = utc(), source_changed_during_run = !identical(source_at_start, setNames(vapply(production_paths, sha, character(1)), names(source_at_start))),
  source_sha256 = list(impute = sha(file.path(repo, "scripts/R/impute.R")), runner = sha(script),
    goldens = sha(file.path(repo, "tests/values/impute_golden.csv"))), numeric_checks = numeric_checks, tests = results,
  environment = list(r = R.version.string, packages = as.list(vapply(environment_packages, function(p) as.character(packageVersion(p)), character(1))))), summary_path)
passed <- sum(vapply(results, function(x) x$passed, logical(1)), na.rm = TRUE)
skipped <- sum(vapply(results, function(x) isTRUE(x$skipped), logical(1)))
cat(sprintf("Phase 2 impute: %d/%d cases passed; %d skipped; %d numeric comparisons. Results: %s\n", passed, length(results), skipped, numeric_checks, summary_path))
if (!nzchar(forced) && keep > 0L) {
  dirs <- sort(list.dirs(collection, recursive = FALSE, full.names = TRUE), decreasing = TRUE)
  dirs <- dirs[grepl("^run-[0-9]{14}-[0-9]+$", basename(dirs))]
  if (length(dirs) > keep) for (path in setdiff(tail(dirs, -keep), work)) unlink(path, recursive = TRUE)
}
if (passed + skipped != length(results)) quit(status = 1L)
