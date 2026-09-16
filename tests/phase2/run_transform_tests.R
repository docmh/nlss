#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Public CLI acceptance: independent base-R values, immutable before/after data,
# metadata, publication refusal and non-activating replay. No NLSS source oracle.
args <- commandArgs(TRUE)
script <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])[1]
repo <- normalizePath(file.path(dirname(script), "../.."), winslash = "/")
script <- normalizePath(script, winslash = "/")
if ("--help" %in% args) { cat("Usage: run_transform_tests.R [--root PATH] [--keep N] [--match REGEX]\n"); quit(status = 0) }
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
collection <- file.path(if (nzchar(forced)) absolute(forced) else absolute(cfg$output_dir), "phase2-transform")
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
run_cli <- function(context, argv, module = "data_transform", failure = FALSE) {
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
run <- function(context, options = list(), source = TRUE, module = "data_transform") {
  old <- runs(context); input_hash <- sha(context$input)
  log <- run_cli(context, c(if (source) context$source, if (is.list(options)) tokens(options) else options), module)
  added <- setdiff(runs(context), old); check(length(added) == 1L, "Expected exactly one published run")
  request_path <- file.path(added, "request.json"); req <- read_json(request_path); res <- read_json(file.path(added, "result.json"))
  check(res$status == "completed" && isTRUE(req$resolved), "Unresolved or failed run")
  check(req$module == "data_transform" && res$module == "data_transform", "Wrong module identity")
  check(identical(req$run_id, basename(added)) && identical(req$run_id, res$run_id), "Run identity mismatch")
  check(identical(res$artifacts$request$sha256, sha(request_path)), "Request/result hash mismatch")
  for (artifact in res$artifacts) check(identical(sha(file.path(added, artifact$path)), artifact$sha256), "Artifact hash mismatch")
  change <- res$results$data_change
  check(is.list(change) && identical(change$input$version_id, req$dataset$version_id), "Before reference not bound to request")
  before <- read_ref(context, change$input); after <- read_ref(context, change$output)
  near(nrow(after), nrow(before), "Transformation preserves row count", 0)
  check(identical(change$input$dataset_id, change$output$dataset_id), "Transformation invented another dataset")
  check(identical(change$input$source_sha256, change$output$source_sha256), "Transformation replaced original source identity")
  check(identical(change$input$import_version_id, change$output$import_version_id), "Transformation replaced import binding")
  near(req$design$source_rows, seq_len(nrow(before)), "Frozen source-row identities", 0)
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
    dictionary = read_json(file.path(context$project, change$output$dictionary_path)), markdown = read_text(file.path(added, "output.md")), log = log)
}
failed <- function(context, options, source = TRUE, module = "data_transform") {
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

for (fn in c("log", "log10", "sqrt", "exp", "abs", "center", "scale", "z", "zscore", "standardize")) test(paste0("transform_function_", fn), {
  context <- new_case(fn); options <- list(transform = paste0("y=", fn), `transform-into` = "y=answer")
  actual <- run(context, options)
  reference <- switch(fn, log = log(sample$y), log10 = log10(sample$y), sqrt = sqrt(sample$y), exp = exp(sample$y),
    abs = abs(sample$y), center = sample$y - mean(sample$y), as.numeric(scale(sample$y)))
  near(actual$after$answer, reference, paste("Independent", fn))
  same(actual$after$id, sample$id, "Original case IDs")
  check(isTRUE(actual$request$design$replay$eligible), "Built-in transform not replay eligible")
})
test("transform_all_operations_independent_goldens_smoke", {
  data <- read.csv(absolute(cfg$golden_dataset), stringsAsFactors = FALSE)
  context <- new_case("all-operations", data)
  actual <- run(context, list(calc = "gain=post_score-pre_score|constant=7", transform = "age=log|income=log10|education=sqrt|x1=exp|x2=abs|outcome_reg=center|outcome_anova=scale",
    standardize = "pre_score", recode = "ordinal_var=1:2,2:3,NA:0", `percentile-bins` = "satisfaction=4", bins = "age=18,30,45,80", rename = "gain:change", drop = "x3", `confirm-drop` = TRUE))
  expected <- read.csv(file.path(repo, "tests/values/data_transform_golden.csv"), stringsAsFactors = FALSE)
  for (variable in unique(expected$variable)) {
    rows <- expected[expected$variable == variable, ]; near(actual$after[[variable]][rows$row], rows$value, paste("Golden", variable))
  }
  check(!"x3" %in% names(actual$after) && !"gain" %in% names(actual$after), "Drop/rename not applied")
  log <- actual$result$results$transform_log_df
  check(identical(unique(log$action), c("calc", "transform", "standardize", "recode", "percentile_bin", "bin", "rename", "drop")), "Actual operation order changed")
  python <- Sys.getenv("PYTHON_BIN", "python3")
  checker_log <- file.path(context$base, "golden-check.log")
  status <- system2(python, shQuote(c(file.path(repo, "tests/values/check_data_transform_golden.py"),
    file.path(context$directory, "analysis_log.jsonl"), "0", file.path(repo, "tests/values/data_transform_golden.csv"), "all_operations")), stdout = checker_log, stderr = checker_log)
  check(status == 0L, paste("Independent JSONL golden checker:", read_text(checker_log)))
})
test("transform_dependency_order_and_custom_names", {
  actual <- run(new_case("chain"), list(drop = "id", `confirm-drop` = TRUE, rename = "band:final band",
    bins = "ranked=0,1,2", `bins-into` = "ranked=band", `percentile-bins` = "coded=2", `percentile-into` = "coded=ranked",
    recode = "standard=NA:0", `recode-into` = "standard=coded", standardize = "rooted", `standardize-into` = "rooted=standard",
    transform = "doubled=sqrt", `transform-into` = "doubled=rooted", calc = "doubled=y*2"))
  z <- as.numeric(scale(sqrt(sample$y * 2))); ref <- cut(z, quantile(z, c(0, .5, 1), type = 7), include.lowest = TRUE, labels = FALSE)
  near(actual$after[["final band"]], cut(ref, c(0, 1, 2), include.lowest = TRUE, labels = FALSE), "Dependency chain")
  check(!"id" %in% names(actual$after), "Final drop missing")
})
test("transform_suffixes_and_multiple_calc", {
  actual <- run(new_case("suffixes"), list(calc = "a=x+y|b=a*2", standardize = "y", `standardize-suffix` = "_std",
    recode = "code=1:7", `recode-suffix` = "_new", `percentile-bins` = "y=2", `percentile-suffix` = "_half", bins = "y=0,4,10", `bins-suffix` = "_class"))
  near(actual$after$b, (sample$x + sample$y) * 2, "Sequential calc dependency")
  near(actual$after$y_std, as.numeric(scale(sample$y)), "Custom z suffix")
  near(actual$after$code_new, ifelse(sample$code == 1, 7, sample$code), "Custom recode suffix")
  near(actual$after$y_half, cut(sample$y, quantile(sample$y, c(0, .5, 1)), include.lowest = TRUE, labels = FALSE), "Custom percentile suffix")
  near(actual$after$y_class, cut(sample$y, c(0, 4, 10), include.lowest = TRUE, labels = FALSE), "Custom bin suffix")
})
test("transform_recode_simultaneous_and_missing", {
  actual <- run(new_case("simultaneous"), list(recode = "code=1:2,2:3,NA:0"))
  near(actual$after$code_rec, c(2, 3, 3, 0, 2, 3, 3, 4), "Mappings compare original values, not preceding replacements")
})
for (quoted in c(FALSE, TRUE)) test(paste0("transform_recode_close_numeric_codes_", if (quoted) "quoted" else "numeric"), {
  data <- data.frame(code = c(1, 1 + .Machine$double.eps, 3, NA))
  mapping <- if (quoted) "code='1':10,'1.0000000000000002':20" else "code=1:10,1.0000000000000002:20"
  actual <- run(new_case(paste0("close-numeric-codes-", quoted), data), list(recode = mapping))
  near(actual$after$code_rec, c(10, 20, 3, NA), "Close numeric recode keys retain full double identity", 0)
})
test("transform_recode_factor_new_levels", {
  actual <- run(new_case("factor-recode"), list(recode = "category=low:small,high:large,NA:unknown"))
  same(actual$after$category_rec, c("small", "large", "small", "unknown", "middle", "large", "middle", "small"), "Factor recode retains new categories")
})
test("transform_recode_numeric_to_text_and_arrow_syntax", {
  actual <- run(new_case("recode-text"), list(recode = "code=1->one,2->two,NA->missing"))
  same(actual$after$code_rec, c("one", "two", "3", "missing", "one", "two", "3", "4"), "Mixed recode promotes values without loss")
})
test("transform_numeric_factor_coercion_uses_values", {
  data <- data.frame(x = factor(c("10", "2", NA, "30"), levels = c("30", "10", "2")))
  actual <- run(new_case("factor-numeric", data), list(transform = "x=sqrt", coerce = TRUE))
  near(actual$after$sqrt_x, sqrt(c(10, 2, NA, 30)), "Factor coercion uses labels, not internal codes")
})
test("transform_character_coercion_loss_is_disclosed", {
  actual <- run(new_case("character-numeric"), list(transform = "text=sqrt", coerce = TRUE))
  near(actual$after$sqrt_text, sqrt(c(10, 2, 30, NA, NA, 4, 7, 8)), "Character coercion")
  check(grepl("NA|missing|coerc", paste(actual$markdown, actual$result$results$transform_log_df$note), ignore.case = TRUE), "Coercion losses undisclosed")
})
test("transform_percentile_ties_and_sorted_boundaries", {
  data <- data.frame(x = c(0, 0, 0, 0, 1, 1, 2, NA), y = c(-1, 0, 1, 2, 3, 4, 5, NA))
  actual <- run(new_case("ties-boundaries", data), list(`percentile-bins` = "x=4", bins = "y=4,0,2"))
  near(actual$after$x_pct, cut(data$x, unique(quantile(data$x, seq(0, 1, .25), na.rm = TRUE)), include.lowest = TRUE, labels = FALSE), "Reduced tied quantile bins")
  near(actual$after$y_bin, c(NA, 1, 1, 1, 2, 2, NA, NA), "Sorted right-closed cuts, endpoints and outside values")
  check(grepl("reduced", actual$markdown, ignore.case = TRUE) && grepl("sorted", actual$markdown, ignore.case = TRUE), "Bin adjustments undisclosed")
})
test("transform_nonfinite_results_are_preserved_and_disclosed", {
  actual <- run(new_case("nonfinite"), list(transform = "x=log"))
  near(actual$after$log_x, suppressWarnings(log(sample$x)), "Log domain, NA and -Inf")
  check(length(actual$result$warnings) > 0L || grepl("non.?finite|NaN|missing", actual$markdown, ignore.case = TRUE), "Nonfinite output not disclosed")
  step <- jsonlite::fromJSON(file.path(actual$path, "result.json"), simplifyVector = FALSE)$results$step_details[[1]]
  near(unlist(step$newly_missing_rows), 8, "New log-domain missing source row", 0)
  near(unlist(step$newly_infinite_rows), 7, "New -Inf source row", 0)
})
test("transform_constant_and_all_missing_standardization", {
  data <- data.frame(a = c(3, 3, NA, 3), b = rep(NA_real_, 4))
  actual <- run(new_case("constant", data), list(standardize = "a,b"))
  near(actual$after$a_z, as.numeric(scale(data$a)), "Constant z undefined")
  near(actual$after$b_z, as.numeric(scale(data$b)), "All-missing z undefined")
})
test("transform_noop_preserves_values_and_truthful_report", {
  actual <- run(new_case("noop"))
  for (column in names(sample)) same(actual$after[[column]], sample[[column]], paste("No-op", column))
  check(grepl("No transformations applied", actual$markdown, fixed = TRUE), "No-op presented as a transformation")
})
test("transform_confirmed_overwrite_and_drop", {
  actual <- run(new_case("confirmed"), list(calc = "x=y+1", `overwrite-vars` = TRUE, `confirm-overwrite` = TRUE,
    rename = "x:y", drop = "code", `confirm-drop` = TRUE))
  near(actual$after$y, sample$y + 1, "Confirmed rename replacement")
  check(!"x" %in% names(actual$after) && !"code" %in% names(actual$after), "Confirmed drop/rename incomplete")
})
for (operation in c("transform", "standardize", "recode", "percentile", "bins")) test(paste0("transform_inplace_confirmed_", operation), {
  options <- switch(operation,
    transform = list(transform = "y=sqrt", `transform-into` = "y=y"),
    standardize = list(standardize = "y", `standardize-into` = "y=y"),
    recode = list(recode = "y=1:2,2:3", `recode-into` = "y=y"),
    percentile = list(`percentile-bins` = "y=2", `percentile-into` = "y=y"),
    bins = list(bins = "y=0,4,10", `bins-into` = "y=y"))
  options[["overwrite-vars"]] <- TRUE; options[["confirm-overwrite"]] <- TRUE
  actual <- run(new_case(paste0("inplace-", operation)), options)
  reference <- switch(operation, transform = sqrt(sample$y), standardize = as.numeric(scale(sample$y)),
    recode = ifelse(sample$y == 1, 2, ifelse(sample$y == 2, 3, sample$y)),
    percentile = cut(sample$y, quantile(sample$y, c(0, .5, 1)), include.lowest = TRUE, labels = FALSE),
    bins = cut(sample$y, c(0, 4, 10), include.lowest = TRUE, labels = FALSE))
  near(actual$after$y, reference, paste("In-place", operation))
  check("y" %in% actual$result$results$column_changes$modified, "In-place change omitted from column audit")
})
test("transform_config_defaults_and_cli_override", {
  yaml::write_yaml(list(modules = list(data_transform = list(standardize_suffix = "_configured", recode_suffix = "_configured"))), config)
  actual <- run(new_case("config"), list(standardize = "y", recode = "code=1:9", `recode-suffix` = "_cli"))
  near(actual$after$y_configured, as.numeric(scale(sample$y)), "Configured suffix")
  near(actual$after$code_cli, ifelse(sample$code == 1, 9, sample$code), "CLI wins over config suffix")
})

for (format in c("csv", "rds", "RData", "parquet", "sav")) test(paste0("transform_import_", tolower(format)), {
  data <- sample[c("id", "x", "y", "code")]
  actual <- run(new_case(paste0("format-", format), data, format), list(calc = "sum=x+y", rename = "code:renamed"))
  near(actual$after$sum, data$x + data$y, paste(format, "computed values"))
  near(actual$after$renamed, data$code, paste(format, "rename values"))
})
test("transform_csv_semicolon_decimal_and_nonsyntactic_names", {
  data <- data.frame("score one" = c(1.5, 2.25, NA), "participant id" = c("001", "002", "010"), check.names = FALSE)
  context <- new_case("csv-semantics", data, "csv")
  write.table(data, context$input, sep = ";", dec = ",", row.names = FALSE, na = "MISSING")
  actual <- run(context, list(sep = ";", `csv-decimal` = ",", `csv-na-values` = "MISSING", `csv-col-types` = "participant id=character,score one=numeric",
    calc = "double score=`score one`*2", rename = "participant id:subject id"))
  near(actual$after[["double score"]], data[["score one"]] * 2, "Decimal comma and quoted variable calc")
  same(actual$after[["subject id"]], data[["participant id"]], "Leading-zero identifier")
})
for (format in c("rds", "sav")) test(paste0("transform_label_lineage_", format, if (format == "sav") "_smoke" else ""), {
  data <- data.frame(id = 1:5, score = haven::labelled_spss(c(1, 2, 99, 3, NA), c(Agree = 1, Agree = 2, Other = 3, Missing = 99), na_values = 99, label = "Response label"),
    untouched = haven::labelled(c(2, 1, 2, NA, 1), c(No = 1, Yes = 2), label = "Keep this label"))
  context <- new_case(paste0("labels-", format), data, format)
  actual <- run(context, list(recode = "score=1:7,2:8", rename = "score:original response"))
  near(actual$after$score_rec, c(7, 8, NA, 3, NA), "User missing masked before recode")
  columns <- actual$dictionary$columns
  check(!"score" %in% names(columns) && "original response" %in% names(columns), "Rename did not update dictionary keys")
  check(identical(columns[["original response"]]$variable_label, "Response label"), "Renamed variable lost label")
  check(length(columns[["original response"]]$value_labels) > 0L, "Renamed variable lost value labels")
  check(identical(columns$untouched$variable_label, "Keep this label"), "Unchanged variable label lost")
  check(is.null(columns$score_rec$value_labels) && is.null(columns$score_rec$missing), "Derived values inherited false value labels/missing definitions")
  prior <- read_json(file.path(context$project, actual$change$input$dictionary_path))
  check(identical(prior$columns$score, columns[["original response"]]), "Pure rename altered original metadata definitions")
  check(identical(prior$columns$untouched, columns$untouched), "Unchanged column metadata was rewritten")
  check(!is.null(prior$columns$score$missing), "Original missing definitions absent from preserved before dictionary")
})
test("transform_dates_times_and_rename_dictionary", {
  data <- data.frame(id = 1:4, date = as.Date("2020-01-01") + c(0, 1, NA, 3),
    stamp = as.POSIXct("2020-01-01 12:00:00", tz = "Europe/Berlin") + c(.125, 1.75, NA, 3.5),
    elapsed = as.difftime(c(.125, 1.5, NA, 8.25), units = "hours"))
  actual <- run(new_case("temporal", data), list(rename = "date:visit date,stamp:recorded at", calc = "next date=date+1"))
  near(actual$after[["visit date"]], as.numeric(data$date), "Renamed Date storage")
  near(actual$after[["recorded at"]], as.numeric(data$stamp), "Fractional timestamp storage", 0)
  near(actual$after$elapsed, as.numeric(data$elapsed), "Difftime units and fractional storage", 0)
  near(actual$after[["next date"]], as.numeric(data$date + 1), "Calculated Date storage")
  storage <- actual$dictionary$storage
  check(identical(storage[["visit date"]]$kind, "Date") && identical(storage[["next date"]]$kind, "Date"), "Date logical type lost")
  check(identical(storage[["recorded at"]]$kind, "POSIXct") && "Europe/Berlin" %in% storage[["recorded at"]]$timezone, "Timestamp timezone lost")
  check(identical(storage$elapsed$kind, "difftime") && identical(storage$elapsed$units, "hours"), "Difftime unit lost")
  check(!"date" %in% names(storage) && !"stamp" %in% names(storage), "Renamed temporal storage retained stale keys")
})
test("transform_overwritten_missing_code_becomes_valid_data", {
  data <- data.frame(x = haven::labelled_spss(c(1, 2, 99, 3), c(Yes = 1, No = 2, Missing = 99), na_values = 99))
  context <- new_case("missing-code-valid", data)
  actual <- run(context, list(recode = "x=NA:99", `recode-into` = "x=x", `overwrite-vars` = TRUE, `confirm-overwrite` = TRUE))
  near(actual$after$x, c(1, 2, 99, 3), "Explicit recode may create valid formerly-missing code")
  check(is.null(actual$dictionary$columns$x$missing) && is.null(actual$dictionary$columns$x$value_labels), "Overwritten missing rules/labels wrongly retained")
  next_run <- run(context, list(calc = "y=x+1"))
  near(next_run$after$y, c(2, 3, 100, 4), "Following import must not remask a transformed valid value")
})
test("transform_zero_rows_scalar_calculation_preserves_empty_dataset", {
  actual <- run(new_case("empty", data.frame(x = numeric(), id = integer())), list(calc = "answer=7"))
  check(nrow(actual$after) == 0L && "answer" %in% names(actual$after), "Scalar recycling created/dropped empty dataset rows")
})

invalid <- list(
  overwrite_permission = list(calc = "x=y*2"), overwrite_confirmation = list(calc = "x=y*2", `overwrite-vars` = TRUE),
  drop_confirmation = list(drop = "x"), rename_collision = list(rename = "x:y"),
  invalid_calc = list(calc = "answer=(x+"), calc_length = list(calc = "answer=c(1,2,3)"),
  calc_matrix = list(calc = "answer=matrix(1,4,2)"),
  calc_null = list(calc = "answer=NULL"),
  unknown_variable = list(transform = "absent=log"), unknown_function = list(transform = "x=imaginary"),
  text_without_coercion = list(transform = "text=sqrt"), fractional_percentiles = list(`percentile-bins` = "x=2.5"),
  too_few_percentiles = list(`percentile-bins` = "x=1"), duplicate_breaks = list(bins = "x=0,2,2,4"),
  invalid_breaks = list(bins = "x=0,no,4"), duplicate_recode_keys = list(recode = "code=1:2,1:3"),
  unknown_drop = list(drop = "absent", `confirm-drop` = TRUE), unknown_rename = list(rename = "absent:x"))
for (name in names(invalid)) test(paste0("transform_invalid_", name), {
  context <- new_case(paste0("invalid-", name)); run(context, list(calc = "baseline=y+1"))
  failed(context, invalid[[name]])
})
for (name in c("constant", "missing")) test(paste0("transform_invalid_percentiles_", name), {
  data <- data.frame(x = if (name == "constant") rep(2, 5) else rep(NA_real_, 5))
  failed(new_case(paste0("bad-percentile-", name), data), list(`percentile-bins` = "x=4"))
})
test("transform_invalid_drop_all_columns", {
  context <- new_case("drop-all", data.frame(x = 1:4)); run(context)
  failed(context, list(drop = "x", `confirm-drop` = TRUE))
})
test("transform_invalid_nonfinite_percentiles", {
  failed(new_case("infinite-percentiles", data.frame(x = c(1, 2, Inf, 4))), list(`percentile-bins` = "x=2"))
})

test("transform_replay_frozen_before_version_after_later_change_smoke", {
  context <- new_case("replay")
  original <- run(context, list(calc = "added=x+y"))
  later <- run(context, list(calc = "y=y*100", `overwrite-vars` = TRUE, `confirm-overwrite` = TRUE))
  check(identical(later$change$input$version_id, original$change$output$version_id), "Successive transformation lineage broken")
  again <- replay(context, original)
  near(again$after$added, sample$x + sample$y, "Replay uses original before snapshot")
  near(as.data.frame(arrow::read_parquet(working(context)))$y, sample$y * 100, "Replay preserved newer working values")
})
test("transform_replay_general_expression_refused_before_execution", {
  context <- new_case("nonreplayable")
  sentinel <- file.path(context$base, "side-effect.txt")
  expression <- paste0("value={writeLines('called', ", encodeString(sentinel, quote = '"'), "); seq_along(x)}")
  original <- run(context, list(calc = expression))
  check(file.exists(sentinel), "General legacy expression was not executed")
  check(isFALSE(original$request$design$replay$eligible), "Side-effect expression falsely replayable")
  writeLines("must stay unchanged", sentinel)
  failed(context, c("--request", original$request_path), source = FALSE, module = "replay_run")
  check(identical(read_text(sentinel), "must stay unchanged"), "Refused replay evaluated general expression")
})
test("transform_replay_random_expression_explicitly_ineligible", {
  original <- run(new_case("random"), list(calc = "value=runif(length(x))"))
  check(isFALSE(original$request$design$replay$eligible) && nzchar(original$request$design$replay$reason), "Random expression silently promises replay")
  check(all(original$after$value >= 0 & original$after$value <= 1), "Legacy random expression was removed")
})
test("transform_replay_bounded_functions_and_shadowed_names", {
  data <- sample; data$mean <- 101:108; data$pi <- 11:18
  context <- new_case("bounded-calc", data)
  original <- run(context, list(calc = "centered=y-mean(y)|p=pi*2|flag=ifelse(is.na(x),0,ifelse(x>2,1,-1))|spread=sd(y)"))
  near(original$after$centered, data$y - mean(data$y), "Explicit mean function binding despite same-named data column")
  near(original$after$p, data$pi * 2, "Data column pi is not silently replaced by mathematical constant")
  near(original$after$flag, ifelse(is.na(data$x), 0, ifelse(data$x > 2, 1, -1)), "Whitelisted nested conditional and NA handling")
  near(original$after$spread, rep(sd(data$y), nrow(data)), "Whitelisted sample SD and scalar recycling")
  check(isTRUE(original$request$design$replay$eligible), "Bounded plain-vector functions not replayable")
  replay(context, original)
})
test("transform_replay_unverified_dependency_stays_ineligible", {
  original <- run(new_case("unknown-dependency"), list(calc = "random=runif(length(x))|dependent=random+1"))
  near(original$after$dependent - original$after$random, rep(1, nrow(sample)), "Legacy general calculation dependency")
  check(isFALSE(original$request$design$replay$eligible), "Dependent random calculation falsely replayable")
  policy <- jsonlite::fromJSON(original$request_path, simplifyVector = FALSE)$design$replay$calc_classification
  check(length(policy) == 2L && isFALSE(policy[[1]]$eligible) && isFALSE(policy[[2]]$eligible), "Unverified dependency promoted into bounded replay")
})
test("transform_calc_single_column_matrix_retains_legacy_values", {
  actual <- run(new_case("calc-single-matrix"), list(calc = "standard=scale(y)"))
  near(actual$after$standard, as.numeric(scale(sample$y)), "Legacy one-column matrix calculation")
  check(isFALSE(actual$request$design$replay$eligible), "General scale dispatch falsely whitelisted")
})
test("transform_calc_list_column_retains_legacy_values", {
  context <- new_case("calc-list-column")
  actual <- run(context, list(calc = "nested=as.list(x)"))
  check(is.list(actual$after$nested) && length(actual$after$nested) == nrow(sample), "Legacy list-column output was removed")
  near(vapply(actual$after$nested, function(x) if (length(x)) as.numeric(x) else NA_real_, numeric(1)), sample$x, "Legacy list-column values")
  check(isFALSE(actual$request$design$replay$eligible), "General list construction falsely whitelisted")
  next_run <- run(context, list(calc = "double=y*2"))
  near(next_run$after$double, sample$y * 2, "Following calculation on a dataset retaining a list column")
  near(vapply(next_run$after$nested, function(x) if (length(x)) as.numeric(x) else NA_real_, numeric(1)), sample$x, "Following calculation preserved list values")
  failed(context, c("--request", actual$request_path), source = FALSE, module = "replay_run")
})
test("transform_replay_tampered_request_refused", {
  context <- new_case("tampered"); original <- run(context, list(calc = "added=x+1"))
  request <- read_json(original$request_path); request$options$calc <- "added=x+100"; write_json(request, original$request_path)
  failed(context, c("--request", original$request_path), source = FALSE, module = "replay_run")
})
for (kind in c("data", "dictionary")) test(paste0("transform_replay_tampered_output_", kind, "_refused"), {
  context <- new_case(paste0("tampered-output-", kind)); original <- run(context, list(calc = "added=x+1"))
  ref <- original$change$output
  path <- file.path(context$project, ref[[if (kind == "data") "snapshot_path" else "dictionary_path"]])
  writeBin(c(readBin(path, "raw", file.info(path)$size), charToRaw("deliberately-tampered-fixture")), path)
  failed(context, c("--request", original$request_path), source = FALSE, module = "replay_run")
})
test("transform_followup_analysis_uses_new_version", {
  context <- new_case("followup")
  run_cli(context, c(context$source, "--agent", "Codex"), "init_workspace")
  actual <- run(context, list(calc = "newvalue=y*2"))
  previous <- runs(context)
  run_cli(context, c("--vars", "newvalue"), "descriptive_stats")
  added <- setdiff(runs(context), previous); check(length(added) == 1L, "Follow-up descriptive run missing")
  request <- read_json(file.path(added, "request.json"))
  check(identical(request$dataset$version_id, actual$change$output$version_id), "Following analysis did not use transformed version")
})
test("transform_log_false_keeps_mandatory_audit", {
  context <- new_case("no-log"); actual <- run(context, list(calc = "added=y+1", log = FALSE))
  check(!file.exists(file.path(context$directory, "analysis_log.jsonl")), "--log FALSE ignored")
  check(is.list(actual$result$results$data_change) && is.data.frame(actual$result$results$transformed_df), "--log FALSE removed mandatory data audit")
})
test("transform_custom_template_is_preserved_for_replay", {
  context <- new_case("template"); template <- file.path(context$base, "template.md")
  writeLines(c("---", "title: Transformation", "---", "TRANSFORM_CUSTOM_MARKER", "{{table_body}}", "{{narrative_default}}"), template)
  original <- run(context, list(calc = "added=y+1", template = template))
  check(grepl("TRANSFORM_CUSTOM_MARKER", original$markdown, fixed = TRUE), "Template override absent")
  writeLines("changed outside the saved run", template)
  replay(context, original)
})
test("transform_calc_path_masking_preserves_division_and_private_values", {
  context <- new_case("calc-path-masking")
  arithmetic <- c("(x+y)/3", "x /3", "((x+y)/(2+y))/3")
  literal_values <- c("/tmp/private/sensitive/unix.csv", "C:\\private\\sensitive\\windows.csv", "/tmp/private/sensitive/raw.csv")
  expressions <- c(arithmetic, encodeString(literal_values[1], quote = '"'), encodeString(literal_values[2], quote = '"'),
    'r"(/tmp/private/sensitive/raw.csv)"')
  variables <- c("a", "b", "nested", "unix_path", "win_path", "raw_path")
  actual <- run(context, list(calc = paste(paste0(variables, "=", expressions), collapse = "|")))
  near(actual$after$a, (sample$x + sample$y) / 3, "Division calculation")
  near(actual$after$b, sample$x / 3, "Spaced division calculation")
  near(actual$after$nested, ((sample$x + sample$y) / (2 + sample$y)) / 3, "Nested division calculation")
  entry <- jsonlite::fromJSON(tail(readLines(file.path(context$directory, "analysis_log.jsonl"), warn = FALSE), 1L))
  legacy_details <- entry$results$transform_log_df$details
  for (i in seq_along(arithmetic)) {
    check(identical(legacy_details[i], arithmetic[i]), "Legacy calc details changed arithmetic into a path")
    for (surface in list(actual$markdown, read_text(file.path(context$directory, "report_canonical.md")),
      entry$options$calc, paste(unlist(entry$commands), collapse = " "))) {
      check(grepl(arithmetic[i], surface, fixed = TRUE), "Markdown/options/commands lost exact arithmetic expression")
    }
  }
  for (i in seq_along(literal_values)) {
    column <- variables[i + 3L]
    same(actual$after[[column]], rep(literal_values[i], nrow(sample)), "Immutable data retains literal path exactly")
    same(actual$result$results$transformed_df[[column]], rep(literal_values[i], nrow(sample)), "Private JSON retains literal path exactly")
    check(!grepl("sensitive", legacy_details[i + 3L], fixed = TRUE) && grepl("<external>", legacy_details[i + 3L], fixed = TRUE), "Literal path not masked in legacy calc details")
    check(all(!grepl("sensitive", entry$results$transformed_df[[column]], fixed = TRUE)), "Legacy textual values leaked external path")
  }
  for (surface in list(actual$markdown, read_text(file.path(context$directory, "report_canonical.md")),
    entry$options$calc, paste(unlist(entry$commands), collapse = " "))) {
    check(!grepl("sensitive", surface, fixed = TRUE) && grepl("<external>", surface, fixed = TRUE), "Human-facing projection leaked external path literals")
  }
})
for (kind in c("analysis", "import", "publication")) test(paste0("transform_lock_conflict_", kind), {
  context <- new_case(paste0("lock-", kind)); run(context, list(calc = "baseline=y+1"))
  lock <- file.path(if (kind == "publication") context$project else context$directory, paste0(".", kind, "-lock"))
  dir.create(lock); writeLines("fixture lock", file.path(lock, "owner"))
  failed(context, list(calc = "blocked=y+2"))
  check(dir.exists(lock) && identical(read_text(file.path(lock, "owner")), "fixture lock"), "Foreign lock removed")
})
test("transform_data_change_publication_contract", {
  path <- absolute(cfg$phase2$data_change_contract)
  check(file.exists(path), "Registered data-change publication contract missing")
  log <- file.path(work, "data-change-publication.log")
  previous <- Sys.getenv("NLSS_TEST_ROOT", unset = NA_character_)
  Sys.setenv(NLSS_TEST_ROOT = file.path(work, "data-change-publication"))
  status <- tryCatch(system2(file.path(R.home("bin"), "Rscript"), shQuote(path), stdout = log, stderr = log),
    finally = if (is.na(previous)) Sys.unsetenv("NLSS_TEST_ROOT") else Sys.setenv(NLSS_TEST_ROOT = previous))
  check(status == 0L && !grepl("[FAIL]", read_text(log), fixed = TRUE), paste("Publication contract failed:", read_text(log)))
})

if (!length(results)) stop("No tests selected")
check(!anyDuplicated(registered_tests), "Duplicate acceptance case names")
summary_path <- file.path(work, "results.json")
write_json(list(schema_version = 1L, suite = "phase2-transform", execution_contract = "resolved-request-v1", test_pattern = pattern,
  available_cases = length(registered_tests), available_test_names = registered_tests,
  started_utc = started_utc, finished_utc = utc(), source_changed_during_run = !identical(source_at_start, setNames(vapply(production_paths, sha, character(1)), names(source_at_start))),
  source_sha256 = list(data_transform = sha(file.path(repo, "scripts/R/data_transform.R")), runner = sha(script),
    goldens = sha(file.path(repo, "tests/values/data_transform_golden.csv"))), numeric_checks = numeric_checks, tests = results,
  environment = list(r = R.version.string, packages = as.list(vapply(required, function(p) as.character(packageVersion(p)), character(1))))), summary_path)
passed <- sum(vapply(results, function(x) x$passed, logical(1)))
cat(sprintf("Phase 2 transform: %d/%d cases passed; %d numeric comparisons. Results: %s\n", passed, length(results), numeric_checks, summary_path))
if (!nzchar(forced) && keep > 0L) {
  dirs <- sort(list.dirs(collection, recursive = FALSE, full.names = TRUE), decreasing = TRUE)
  dirs <- dirs[grepl("^run-[0-9]{14}-[0-9]+$", basename(dirs))]
  if (length(dirs) > keep) for (path in setdiff(tail(dirs, -keep), work)) unlink(path, recursive = TRUE)
}
if (passed != length(results)) quit(status = 1L)
