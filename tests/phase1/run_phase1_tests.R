#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Independent numerical expectations come from base R/stats, never NLSS modules.

args <- commandArgs(trailingOnly = TRUE)
script <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])
repo <- normalizePath(file.path(dirname(script[1]), "../.."), winslash = "/")
arg_value <- function(name, default = NULL) {
  index <- which(args == name)
  if (!length(index)) return(default)
  index <- tail(index, 1)
  if (index == length(args) || startsWith(args[index + 1], "--")) stop("Missing value for ", name)
  args[index + 1]
}
if ("--help" %in% args) {
  cat("Usage: Rscript tests/phase1/run_phase1_tests.R [--root PATH] [--keep N]\n",
      "Honors NLSS_TEST_ROOT, NLSS_KEEP_RUNS and NLSS_TESTS_CONFIG. No network required.\n", sep = "")
  quit(status = 0)
}
if (length(args) && any(!args[seq(1, length(args), by = 2)] %in% c("--root", "--keep"))) {
  stop("Unknown runner option. Use --help.")
}
required <- c("yaml", "jsonlite", "haven", "arrow", "digest", "hms", "mice")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Phase 1 tests require installed packages: ", paste(missing, collapse = ", "))
absolute <- function(path) {
  if (!grepl("^(/|[A-Za-z]:|\\\\)", path)) path <- file.path(repo, path)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}
tests_config <- absolute(Sys.getenv("NLSS_TESTS_CONFIG", file.path(repo, "tests/tests.yml")))
test_config <- yaml::read_yaml(tests_config)$tests
configured <- function(value, fallback) if (is.null(value) || !nzchar(as.character(value))) fallback else value
output_base <- absolute(configured(test_config$output_dir, "outputs/test-runs"))
forced_root <- arg_value("--root", Sys.getenv("NLSS_TEST_ROOT", ""))
run_root <- if (nzchar(forced_root)) absolute(forced_root) else file.path(output_base, format(Sys.time(), "%Y%m%d%H%M%S"))
keep <- suppressWarnings(as.integer(arg_value("--keep", Sys.getenv("NLSS_KEEP_RUNS", as.character(test_config$keep_runs_default)))))
if (length(keep) != 1 || is.na(keep) || keep < 0) stop("--keep/NLSS_KEEP_RUNS must be a non-negative integer.")
work <- file.path(run_root, "phase1", paste0("run-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid()))
dir.create(work, recursive = TRUE, showWarnings = FALSE)
work <- normalizePath(work, winslash = "/")
fixtures <- file.path(work, "fixtures")
dir.create(fixtures)
fixture_path <- absolute(configured(test_config$phase1$fixtures, file.path(test_config$data_dir, "import/haven-fixtures.json")))
fixture_manifest <- jsonlite::fromJSON(fixture_path, simplifyVector = FALSE)
sha256 <- function(path) digest::digest(file = path, algo = "sha256")
for (fixture in fixture_manifest$fixtures) {
  destination <- file.path(fixtures, fixture$name)
  writeBin(jsonlite::base64_dec(fixture$base64), destination)
  if (!identical(sha256(destination), fixture$sha256)) stop("Public fixture hash mismatch: ", fixture$name)
}

# The helpers below inspect the public storage contract; they do not generate goldens.
source(file.path(repo, "scripts/R/lib/cli.R"))
source(file.path(repo, "scripts/R/lib/config.R"))
# A private default configuration makes these tests independent of concurrent
# legacy smoke tests, which temporarily edit the repository configuration.
private_config <- file.path(work, "config.yml")
yaml::write_yaml(get_builtin_config(), private_config)
Sys.setenv(NLSS_CONFIG_PATH = private_config)
source(file.path(repo, "scripts/R/lib/data_utils.R"))
source(file.path(repo, "scripts/R/lib/io.R"))
results <- list()
check <- function(condition, message) if (!isTRUE(condition)) stop(message, call. = FALSE)
near <- function(actual, expected, label, tolerance = 1e-6) {
  check(length(actual) == length(expected) && length(actual) > 0,
        paste(label, "has a missing or unexpected number of values"))
  check(identical(is.na(actual), is.na(expected)), paste(label, "has different missing values"))
  valid <- !is.na(expected)
  check(all(is.finite(actual[valid])) && all(abs(actual[valid] - expected[valid]) <= tolerance),
        paste(label, "differs from the independent expected values"))
}
test <- function(name, code) {
  previous <- getwd()
  on.exit(setwd(previous), add = TRUE)
  start <- proc.time()[["elapsed"]]
  error <- tryCatch({force(code); NULL}, error = function(e) conditionMessage(e))
  results[[length(results) + 1L]] <<- list(test = name, passed = is.null(error),
                                        seconds = unname(proc.time()[["elapsed"]] - start),
                                        message = if (is.null(error)) "OK" else error)
  cat(if (is.null(error)) "[PASS] " else "[FAIL] ", name,
      if (is.null(error)) "" else paste0(": ", error), "\n", sep = "")
}
new_case <- function(name) {
  base <- file.path(work, "cases", name)
  project <- file.path(base, "project")
  dir.create(project, recursive = TRUE)
  yaml::write_yaml(list(version = 1, datasets = list()), file.path(project, "nlss-workspace.yml"))
  setwd(project)
  list(base = base, project = project)
}
run_module <- function(module, options, expect_failure = FALSE, error_pattern = NULL) {
  destination <- tempfile(pattern = paste0(module, "-"), tmpdir = dirname(getwd()), fileext = ".log")
  status <- system2(file.path(R.home("bin"), "Rscript"),
                    c(shQuote(file.path(repo, "scripts/R", paste0(module, ".R"))), shQuote(options)),
                    stdout = destination, stderr = destination)
  output <- paste(readLines(destination, warn = FALSE), collapse = "\n")
  if (expect_failure) {
    check(status != 0, paste("Expected a nonzero failure for", module, "but the command succeeded"))
    check(nzchar(output), "An expected failure supplied no feedback")
    if (!is.null(error_pattern)) check(grepl(error_pattern, output, fixed = TRUE),
                                      paste("Expected failure feedback:", error_pattern, "but received:", output))
  } else check(status == 0, paste(module, "failed:", output))
  invisible(list(status = status, output = output))
}
read_entries <- function(dataset) {
  path <- file.path(dataset, "analysis_log.jsonl")
  check(file.exists(path), paste("Missing analysis log:", path))
  lapply(readLines(path, warn = FALSE), jsonlite::fromJSON)
}
entry_for <- function(dataset, module) {
  entries <- Filter(function(entry) identical(entry$module, module), read_entries(dataset))
  check(length(entries) > 0, paste("No logged result for", module))
  tail(entries, 1)[[1]]
}
check_provenance <- function(entry, project) {
  provenance <- entry$dataset
  for (field in c("dataset_id", "version_id", "data_sha256", "dictionary_sha256", "snapshot_path", "source_sha256")) {
    check(is.character(provenance[[field]]) && length(provenance[[field]]) == 1 && nzchar(provenance[[field]]),
          paste("Missing dataset provenance field:", field))
  }
  snapshot <- file.path(project, provenance$snapshot_path)
  check(file.exists(snapshot), "Logged dataset snapshot does not exist")
  check(identical(sha256(snapshot), provenance$data_sha256), "Logged data hash does not match immutable snapshot")
  dictionary <- file.path(dirname(snapshot), "dictionary.json")
  check(file.exists(dictionary) && identical(sha256(dictionary), provenance$dictionary_sha256),
        "Logged dictionary hash does not match immutable dictionary")
  check(file.exists(file.path(dirname(snapshot), "provenance.json")), "Version provenance is missing")
  invisible(provenance)
}
check_descriptive <- function(entry, expected) {
  summary <- entry$results$summary_df
  for (name in names(expected)) {
    values <- expected[[name]]
    row <- summary[summary$variable == name, , drop = FALSE]
    check(nrow(row) == 1, paste("Expected one descriptive row for", name))
    valid <- values[!is.na(values)]
    ci <- unname(stats::t.test(valid)$conf.int)
    wanted <- list(n = length(valid), missing_n = sum(is.na(values)), mean = mean(valid),
                   sd = stats::sd(valid), variance = stats::var(valid), median = stats::median(valid),
                   min = min(valid), max = max(valid), ci_low = ci[1], ci_high = ci[2])
    for (field in names(wanted)) near(row[[field]], wanted[[field]], paste(name, field))
  }
}
check_regression <- function(entry, expected) {
  fitted <- stats::lm(y ~ x, data = expected, na.action = stats::na.omit)
  independent <- summary(fitted)
  intervals <- stats::confint(fitted)
  coefficients <- entry$results$coefficients_df
  check(nrow(coefficients) == nrow(independent$coefficients), "Unexpected regression coefficient count")
  coefficients <- coefficients[match(rownames(independent$coefficients), coefficients$term), , drop = FALSE]
  for (field in c("estimate", "se", "stat", "p")) {
    column <- match(field, c("estimate", "se", "stat", "p"))
    near(coefficients[[field]], unname(independent$coefficients[, column]), paste("regression", field))
  }
  near(coefficients$ci_low, unname(intervals[, 1]), "regression lower CI")
  near(coefficients$ci_high, unname(intervals[, 2]), "regression upper CI")
  near(entry$results$summary_df$n, stats::nobs(fitted), "regression N")
  near(entry$results$summary_df$r2, independent$r.squared, "regression R squared")
  near(entry$results$summary_df$adj_r2, independent$adj.r.squared, "regression adjusted R squared")
}
check_analysis_pair <- function(input, name, expected, project) {
  run_module("descriptive_stats", c(input, "--vars", "x,y"))
  run_module("regression", c(input, "--dv", "y", "--ivs", "x"))
  descriptive <- entry_for(name, "descriptive_stats")
  regression <- entry_for(name, "regression")
  check_descriptive(descriptive, expected)
  check_regression(regression, expected)
  dp <- check_provenance(descriptive, project)
  rp <- check_provenance(regression, project)
  check(identical(dp$version_id, rp$version_id), "Unchanged input changed dataset version between modules")
  check(file.exists(file.path(name, "codebook.md")), "Human-readable import codebook is missing")
  check(file.exists(file.path(name, "dictionary.json")), "Machine-readable import dictionary is missing")
  check(file.exists(file.path(name, "import.json")), "Import provenance is missing")
  check(file.exists(file.path(name, "report_canonical.md")), "Canonical Markdown analysis output is missing")
  invisible(rp)
}

run_contract_script <- function(path, label) {
  unit_script <- absolute(path)
  output <- file.path(work, paste0(label, ".log"))
  status <- system2(file.path(R.home("bin"), "Rscript"), shQuote(unit_script),
                    env = paste0("NLSS_TEST_ROOT=", shQuote(work)), stdout = output, stderr = output)
  check(status == 0, paste(label, "failed:", paste(readLines(output, warn = FALSE), collapse = "\n")))
}
test("import_contract_units", {
  run_contract_script(configured(test_config$phase1$contract_unit, "tests/phase1/import_contract_unit.R"), "import-contract-unit")
})
test("cli_and_config_contract", {
  run_contract_script(configured(test_config$phase1$cli_config, "tests/phase1/cli_config.R"), "cli-config")
})
test("imputation_contract", {
  run_contract_script(configured(test_config$phase1$imputation_contract, "tests/phase1/imputation_contract.R"), "imputation-contract")
})

test("clean_numeric_matches_base_R", {
  context <- new_case("clean-numeric")
  golden <- read.csv(absolute(test_config$golden_dataset), stringsAsFactors = FALSE)
  expected <- data.frame(x = golden$x1, y = golden$outcome_reg)
  path <- file.path(context$base, "numeric.csv")
  write.csv(expected, path, row.names = FALSE)
  check_analysis_pair(c("--csv", path), "numeric", expected, context$project)
})

raw <- data.frame(x = as.numeric(1:40), y = 0.3 * (1:40) + sin(1:40) + ((1:40) %% 3) / 7)
expected <- raw
expected$x[37:40] <- NA_real_
raw$x[37:40] <- c(97, 98, 99, NA)
raw$x <- haven::labelled_spss(raw$x, labels = c("First value" = 1, "Refused" = 99),
                            na_values = 99, na_range = c(97, 98), label = "Predictor label")
for (format in c("sav", "rds", "rdata", "parquet")) {
  test(paste0("user_missing_equivalence_", format), {
    context <- new_case(paste0("equivalence-", format))
    name <- paste0("equivalent_", format)
    path <- file.path(context$base, paste0(name, ".", format))
    input <- c(paste0("--", format), path)
    if (format == "sav") haven::write_sav(raw, path)
    if (format == "rds") saveRDS(raw, path)
    if (format == "rdata") {
      holder <- new.env(parent = emptyenv())
      holder[[name]] <- raw
      save(list = name, file = path, envir = holder)
      input <- c(input, "--df", name)
    }
    if (format == "parquet") arrow::write_parquet(raw, path)
    check_analysis_pair(input, name, expected, context$project)
    normalized <- read_parquet_data(file.path(name, paste0(name, ".parquet")))
    near(normalized$x, expected$x, paste(format, "normalized values"))
    check(!inherits(normalized$x, "haven_labelled"), "Interchange class leaked into the analysis frame")
    dictionary <- jsonlite::fromJSON(file.path(name, "dictionary.json"), simplifyVector = FALSE)
    column <- dictionary$columns$x
    check(identical(column$variable_label, "Predictor label"), "Variable label was lost")
    near(unlist(column$missing$na_values), 99, "SPSS discrete missing definition")
    near(unlist(column$missing$na_range), c(97, 98), "SPSS missing range")
    observations <- column$missing$observations
    user_missing <- Filter(function(value) identical(value$kind, "user_defined"), observations)
    near(sort(vapply(user_missing, function(value) as.numeric(value$row), numeric(1))), c(37, 38, 39),
         "Source-row missing-code provenance")
    codebook <- paste(readLines(file.path(name, "codebook.md"), warn = FALSE), collapse = "\n")
    check(grepl("Predictor label", codebook, fixed = TRUE) && grepl("Refused", codebook, fixed = TRUE),
          "Codebook does not expose variable/value labels")
  })
}

test("rdata_missing_named_object_never_falls_back", {
  context <- new_case("rdata-wrong-name")
  actual_frame <- data.frame(x = 1:5)
  path <- file.path(context$base, "objects.RData")
  save(actual_frame, file = path)
  run_module("descriptive_stats", c("--rdata", path, "--df", "misspelled_frame", "--vars", "x"),
             expect_failure = TRUE, error_pattern = "RData object not found")
  check(!file.exists("misspelled_frame/analysis_log.jsonl"), "Missing RData object produced an analysis result")
  run_module("descriptive_stats", c("--rdata", path, "--df=", "--vars", "x"),
             expect_failure = TRUE, error_pattern = "--df must name a non-empty RData object")
})

test("public_sav_string_labels", {
  new_case("public-string-labels")
  run_module("frequencies", c("--sav", file.path(fixtures, "official-str.sav"), "--vars", "gender"))
  summary <- entry_for("official-str", "frequencies")$results$summary_df
  check(setequal(summary$level_label, c("Female", "Male")), "String value labels were lost or changed")
  near(summary$n, c(1, 1), "String label frequencies")
})
test("public_sav_missing_definition", {
  new_case("public-numeric-missing")
  run_module("descriptive_stats", c("--sav", file.path(fixtures, "official-num-na.sav"), "--vars", "VAR00002"))
  summary <- entry_for("official-num-na", "descriptive_stats")$results$summary_df
  near(summary$n, 1, "Public SAV valid N")
  near(summary$missing_n, 1, "Public SAV missing N")
  source <- haven::read_sav(file.path(fixtures, "official-num-na.sav"), user_na = TRUE)
  dictionary <- jsonlite::fromJSON("official-num-na/dictionary.json", simplifyVector = FALSE)
  near(unlist(dictionary$columns$VAR00002$missing$na_values), attr(source$VAR00002, "na_values"),
       "Public SAV missing definition")
})
test("public_sav_dates_and_fractional_times", {
  new_case("public-datetime")
  path <- file.path(fixtures, "official-datetime.sav")
  source <- haven::read_sav(path, user_na = TRUE)
  run_module("data_explorer", c("--sav", path))
  normalized <- read_parquet_data("official-datetime/official-datetime.parquet")
  for (name in names(source)) near(as.numeric(normalized[[name]]), as.numeric(source[[name]]), paste("datetime", name))
  check(inherits(normalized$date, "Date"), "Date analysis type was lost")
  check(inherits(normalized$date.posix, "POSIXct"), "Datetime analysis type was lost")
  check(inherits(normalized$time, "difftime"), "Time duration analysis type was lost")
})
test("public_stata_tags_preserved_as_provenance_via_rds", {
  context <- new_case("public-tagged-missing")
  source <- as.data.frame(haven::read_dta(file.path(fixtures, "official-tagged-na.dta")))
  path <- file.path(context$base, "tagged.rds")
  saveRDS(source, path)
  run_module("descriptive_stats", c("--rds", path, "--vars", "x"))
  normalized <- read_parquet_data("tagged/tagged.parquet")
  near(normalized$x, as.numeric(source$x), "Tagged missing analysis values")
  dictionary <- jsonlite::fromJSON("tagged/dictionary.json", simplifyVector = FALSE)
  tagged <- Filter(function(value) identical(value$kind, "tagged"), dictionary$columns$x$missing$observations)
  check(length(tagged) == sum(haven::is_tagged_na(source$x)), "Missing-tag observations were lost")
  for (item in tagged) check(identical(item$tag, haven::na_tag(source$x[as.integer(item$row)])), "Missing tag changed")
})
test("explicit_csv_locale_types_and_na_values", {
  context <- new_case("csv-locale")
  path <- file.path(context$base, "german.csv")
  writeLines(c("id;score;gruppe", "001;1,5;Gruppe ä", "002;2,5;Gruppe ö", "003;99;Gruppe ü"), path, useBytes = TRUE)
  input <- c("--csv", path, "--sep", ";", "--csv-decimal", ",", "--csv-encoding", "UTF-8",
             "--csv-col-types", "id=character,score=numeric", "--csv-na-values", "NA,99")
  run_module("descriptive_stats", c(input, "--vars", "score"))
  normalized <- read_parquet_data("german/german.parquet")
  check(identical(normalized$id, c("001", "002", "003")), "Explicit character IDs lost leading zeroes")
  near(normalized$score, c(1.5, 2.5, NA_real_), "Explicit decimal comma and NA values")
  check(identical(normalized$gruppe, c("Gruppe ä", "Gruppe ö", "Gruppe ü")), "UTF-8 strings changed")
  changed <- input
  changed[which(changed == "NA,99")] <- "NA"
  run_module("descriptive_stats", c(changed, "--vars", "score"), expect_failure = TRUE,
             error_pattern = "import options changed")
  run_module("descriptive_stats", c(changed, "--vars", "score", "--import-action", "new-version"))
  near(read_parquet_data("german/german.parquet")$score, c(1.5, 2.5, 99), "Explicit import-options new version")
})
test("csv_invalid_declared_type_fails_with_feedback", {
  context <- new_case("csv-invalid-type")
  path <- file.path(context$base, "invalid.csv")
  writeLines(c("id,score", "001,not-a-number", "002,2.5"), path)
  run_module("descriptive_stats", c("--csv", path, "--csv-col-types", "id=character,score=numeric", "--vars", "score"),
             expect_failure = TRUE, error_pattern = "Invalid numeric value")
})
test("same_basename_never_silently_reuses_other_source", {
  context <- new_case("source-collision")
  a <- file.path(context$base, "a")
  b <- file.path(context$base, "b")
  dir.create(a)
  dir.create(b)
  first <- file.path(a, "survey.csv")
  second <- file.path(b, "survey.csv")
  write.csv(data.frame(x = 1:5), first, row.names = FALSE)
  write.csv(data.frame(x = 11:15), second, row.names = FALSE)
  run_module("descriptive_stats", c("--csv", first, "--vars", "x"))
  before <- entry_for("survey", "descriptive_stats")
  count <- length(read_entries("survey"))
  run_module("descriptive_stats", c("--csv", second, "--vars", "x"), expect_failure = TRUE,
             error_pattern = "different source")
  check(length(read_entries("survey")) == count, "A rejected import appended a successful analysis result")
  run_module("descriptive_stats", c("--csv", second, "--vars", "x", "--dataset-name", "other-survey"))
  near(entry_for("other-survey", "descriptive_stats")$results$summary_df$mean, 13, "Explicit separate dataset")
  near(read_parquet_data("survey/survey.parquet")$x, 1:5, "Original collision dataset")
  check(identical(entry_for("survey", "descriptive_stats")$dataset, before$dataset), "Rejected import rewrote old provenance")
})
test("changed_source_requires_explicit_version_and_preserves_old_result", {
  context <- new_case("source-version")
  path <- file.path(context$base, "survey.csv")
  write.csv(data.frame(x = 1:5), path, row.names = FALSE)
  run_module("descriptive_stats", c("--csv", path, "--vars", "x"))
  first_entry <- entry_for("survey", "descriptive_stats")
  first <- check_provenance(first_entry, context$project)
  first_binding <- jsonlite::fromJSON("survey/import.json")
  archived_source <- file.path(context$project, first_binding$source_copy)
  check(file.exists(archived_source) && identical(sha256(archived_source), first$source_sha256),
        "Original source bytes were not archived with their recorded hash")
  run_module("descriptive_stats", c("--csv", path, "--vars", "x"))
  check(identical(entry_for("survey", "descriptive_stats")$dataset$version_id, first$version_id), "Identical source created a new version")
  write.csv(data.frame(x = 11:15), path, row.names = FALSE)
  run_module("descriptive_stats", c("--csv", path, "--vars", "x"), expect_failure = TRUE,
             error_pattern = "Source contents or import options changed")
  near(read_parquet_data("survey/survey.parquet")$x, 1:5, "Rejected source change retained prior working data")
  run_module("descriptive_stats", c("--csv", path, "--vars", "x", "--import-action", "new-version"))
  latest <- entry_for("survey", "descriptive_stats")
  current <- check_provenance(latest, context$project)
  check(!identical(current$version_id, first$version_id), "Changed source reused old version")
  check(identical(current$dataset_id, first$dataset_id), "New version changed stable dataset identity")
  check(identical(current$source_sha256, sha256(path)), "New version did not record actual source content hash")
  near(latest$results$summary_df$mean, 13, "New-version mean")
  check(identical(sha256(file.path(context$project, first$snapshot_path)), first$data_sha256), "Prior immutable data was overwritten")
  check(identical(read_entries("survey")[[1]]$dataset, first_entry$dataset), "Old result provenance was rewritten")
  near(read_parquet_data(file.path(context$project, first$snapshot_path))$x, 1:5, "Historical snapshot values")
  check(identical(sha256(archived_source), first$source_sha256), "New import overwrote the archived original source")
})
test("working_copy_edits_get_a_new_analysis_snapshot", {
  context <- new_case("working-copy-version")
  path <- file.path(context$base, "survey.csv")
  write.csv(data.frame(x = 1:5), path, row.names = FALSE)
  run_module("descriptive_stats", c("--csv", path, "--vars", "x"))
  before <- check_provenance(entry_for("survey", "descriptive_stats"), context$project)
  workspace_path <- file.path(context$project, "survey/survey.parquet")
  changed <- read_parquet_data(workspace_path)
  changed$x <- 11:15
  write_parquet_data(changed, workspace_path)
  run_module("descriptive_stats", c("--csv", path, "--vars", "x"))
  after_entry <- entry_for("survey", "descriptive_stats")
  after <- check_provenance(after_entry, context$project)
  near(after_entry$results$summary_df$mean, 13, "Working copy was not replaced by unchanged original source")
  check(!identical(after$version_id, before$version_id), "Working-copy modification reused prior analysis version")
  check(identical(after$dataset_id, before$dataset_id), "Working-copy edit changed stable dataset identity")
  check(identical(after$source_sha256, before$source_sha256), "Working-copy edit invented a new source hash")
  near(read_parquet_data(file.path(context$project, before$snapshot_path))$x, 1:5, "Old working-copy snapshot")
  run_module("descriptive_stats", c("--parquet", workspace_path, "--vars", "x"))
  check(identical(entry_for("survey", "descriptive_stats")$dataset$version_id, after$version_id),
        "Explicit workspace Parquet changed version without changing data")
  run_module("descriptive_stats", c("--csv", path, "--vars", "x", "--import-action", "new-version"))
  restored <- entry_for("survey", "descriptive_stats")
  check_provenance(restored, context$project)
  near(restored$results$summary_df$mean, 3, "Explicit source reimport did not restore original values")
  near(read_parquet_data(workspace_path)$x, 1:5, "Explicit source reimport working values")
  near(read_parquet_data(file.path(context$project, after$snapshot_path))$x, 11:15,
       "Edited snapshot was not retained after explicit original-source reimport")
})
test("corrupted_immutable_dictionary_fails_closed", {
  context <- new_case("dictionary-integrity")
  path <- file.path(context$base, "survey.csv")
  write.csv(data.frame(x = 1:5), path, row.names = FALSE)
  run_module("descriptive_stats", c("--csv", path, "--vars", "x"))
  before <- check_provenance(entry_for("survey", "descriptive_stats"), context$project)
  dictionary <- file.path(dirname(file.path(context$project, before$snapshot_path)), "dictionary.json")
  # Deliberately alter only this isolated test artifact; original source data
  # and the working copy are unaffected. The rejection must identify integrity.
  content <- readBin(dictionary, "raw", n = file.info(dictionary)$size)
  writeBin(c(content, charToRaw(" ")), dictionary)
  run_module("descriptive_stats", c("--csv", path, "--vars", "x"), expect_failure = TRUE,
             error_pattern = "dictionary failed integrity validation")
  check(length(read_entries("survey")) == 1, "Corrupt version produced an additional analysis result")
})
test("changed_source_identity_versions_identical_numeric_data", {
  context <- new_case("source-identity-version")
  path <- file.path(context$base, "survey.csv")
  writeLines(c("x", "1", "2", "3", "4", "5"), path)
  run_module("descriptive_stats", c("--csv", path, "--vars", "x"))
  before <- check_provenance(entry_for("survey", "descriptive_stats"), context$project)
  writeLines(c("x", " 1", " 2", " 3", " 4", " 5"), path)
  run_module("descriptive_stats", c("--csv", path, "--vars", "x", "--import-action", "new-version"))
  after_entry <- entry_for("survey", "descriptive_stats")
  after <- check_provenance(after_entry, context$project)
  near(after_entry$results$summary_df$mean, 3, "Whitespace-only reimport changed numeric values")
  check(identical(after$data_sha256, before$data_sha256), "Whitespace fixture did not produce identical normalized data")
  check(!identical(after$source_sha256, before$source_sha256), "Whitespace fixture did not change source identity")
  check(!identical(after$version_id, before$version_id), "Distinct source identity reused an old provenance version")
  check(!identical(after$snapshot_path, before$snapshot_path), "Distinct source identity reused old snapshot provenance")
})
test("corrupted_preserved_source_fails_closed", {
  context <- new_case("source-archive-integrity")
  path <- file.path(context$base, "survey.csv")
  write.csv(data.frame(x = 1:5), path, row.names = FALSE)
  run_module("descriptive_stats", c("--csv", path, "--vars", "x"))
  binding <- jsonlite::fromJSON("survey/import.json")
  archive <- file.path(context$project, binding$source_copy)
  content <- readBin(archive, "raw", n = file.info(archive)$size)
  writeBin(c(content, charToRaw(" ")), archive)
  run_module("descriptive_stats", c("--parquet", file.path(context$project, "survey/survey.parquet"), "--vars", "x"),
             expect_failure = TRUE, error_pattern = "Preserved source failed integrity validation")
  check(length(read_entries("survey")) == 1, "Corrupt preserved source produced an additional analysis result")
})

summary_path <- file.path(work, "results.json")
jsonlite::write_json(list(schema_version = 1, suite = "phase1", tests = results,
                         environment = list(r = R.version.string,
                                            packages = as.list(vapply(required, function(p) as.character(utils::packageVersion(p)), character(1))))),
                    summary_path, pretty = TRUE, auto_unbox = TRUE, digits = NA)
passed <- sum(vapply(results, function(result) result$passed, logical(1)))
cat(sprintf("Phase 1: %d/%d cases passed. Results: %s\n", passed, length(results), summary_path))
# Explicit run roots belong to callers and are never pruned by this runner.
if (!nzchar(forced_root) && keep > 0 && dir.exists(output_base)) {
  candidates <- list.dirs(output_base, full.names = TRUE, recursive = FALSE)
  candidates <- sort(candidates[grepl("^[0-9]{14}$", basename(candidates))], decreasing = TRUE)
  obsolete <- if (length(candidates) > keep) candidates[seq.int(keep + 1L, length(candidates))] else character(0)
  obsolete <- setdiff(obsolete, run_root)
  for (path in obsolete) {
    if (identical(dirname(path), output_base) && grepl("^[0-9]{14}$", basename(path))) unlink(path, recursive = TRUE)
  }
}
quit(status = if (passed == length(results)) 0 else 1)
