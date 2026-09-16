#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))[[1]]
repo <- normalizePath(file.path(dirname(script), "../.."), winslash = "/")
args <- commandArgs(TRUE)
if ("--help" %in% args) { cat("Usage: utility_contract.R [--root PATH] [--keep N] [--match REGEX]\n"); quit(status = 0) }
if (length(args) %% 2L || (length(args) && any(!args[seq(1L, length(args), 2L)] %in% c("--root", "--keep", "--match")))) stop("Unknown/incomplete option")
arg <- function(key, default) { i <- which(args == key); if (length(i) > 1L) stop("Repeated option"); if (length(i)) args[i + 1L] else default }
cfg <- yaml::read_yaml(Sys.getenv("NLSS_TESTS_CONFIG", file.path(repo, "tests/tests.yml")), eval.expr = FALSE)$tests
collection <- file.path(arg("--root", Sys.getenv("NLSS_TEST_ROOT", file.path(repo, cfg$output_dir))), "phase2-utility-contract")
dir.create(collection, recursive = TRUE, showWarnings = FALSE)
root <- tempfile("run-", normalizePath(collection, winslash = "/")); dir.create(root)
keep <- as.numeric(arg("--keep", Sys.getenv("NLSS_KEEP_RUNS", as.character(cfg$keep_runs_default))))
if (!is.finite(keep) || keep < 0 || keep != floor(keep)) stop("Invalid --keep")
pattern <- arg("--match", ".*")
sha <- function(path) digest::digest(file = path, algo = "sha256")
bytes <- function(path) if (file.exists(path)) readBin(path, "raw", file.info(path)$size) else NULL
results <- list(); assertions <- 0L
check <- function(ok, label) { if (!isTRUE(ok)) stop(label); assertions <<- assertions + 1L }
expect_error <- function(code, text) {
  error <- tryCatch({ force(code); "" }, error = conditionMessage)
  check(nzchar(error) && grepl(text, error, ignore.case = TRUE), paste("Expected error", text, "got", error))
}
test <- function(name, code) {
  if (!grepl(pattern, name)) return(invisible(NULL))
  before <- assertions; start <- proc.time()[["elapsed"]]; cwd <- getwd(); on.exit(setwd(cwd))
  error <- tryCatch({ force(code); NULL }, error = conditionMessage)
  results[[length(results) + 1L]] <<- list(test = name, passed = is.null(error), assertions = assertions - before,
    seconds = proc.time()[["elapsed"]] - start, message = if (is.null(error)) "OK" else error)
  cat(if (is.null(error)) "[PASS] " else "[FAIL] ", name, if (is.null(error)) "" else paste0(": ", error), "\n", sep = "")
}
new_case <- function() {
  out <- tempfile("project-", root); dir.create(out); setwd(out)
  config <- file.path(out, "config.yml")
  yaml::write_yaml(list(defaults = list(output_dir = out)), config)
  Sys.setenv(NLSS_CONFIG_PATH = config)
  Sys.unsetenv("NLSS_REPLAY_REQUEST")
  e <- new.env(parent = globalenv())
  source(file.path(repo, "scripts/R/lib/bootstrap.R"), local = e); e$nlss_bootstrap(e)
  e$source_lib("utility_contract.R")
  report <- file.path(out, "report_canonical.md"); log <- file.path(out, "analysis_log.jsonl")
  writeBin(charToRaw("old\r\nreport"), report)
  list(e = e, out = out, report = report, log = log)
}
run <- function(case, publish = function(id, stage) { writeLines("new", case$report); writeLines(id, case$log) }, ...) {
  case$e$nlss_publish_utility("calc", case$out, list(expression = "1+1"), list(value = 2), "# Calculation\n2",
    publish, artifacts = list("raw-result.rds" = serialize(c(NA_real_, NaN, Inf, -Inf, 2), NULL, version = 3L)), ...)
}
for (module in c("calc", "research_academia", "metaskill_runner", "init_workspace")) test(paste0("utility_", module), {
  case <- new_case()
  artifact <- charToRaw("# Human-written report\r\nA freely structured synthesis.\r\n")
  saved <- case$e$nlss_publish_utility(module, case$out, list(purpose = "fixture"), list(value = 2), "# Output\n2",
    function(id, stage) { writeLines(id, case$log) }, artifacts = list("report.md" = artifact))
  request <- jsonlite::read_json(file.path(saved$path, "request.json")); result <- jsonlite::read_json(file.path(saved$path, "result.json"))
  check(identical(request$kind, "utility") && identical(request$module, module), "Utility identity")
  check(identical(request$replay$eligible, FALSE) && is.null(request$dataset), "No fictitious dataset or replay")
  check(identical(result$status, "completed") && length(request$environment$packages) > 0L, "Recorded success/environment")
  check(identical(bytes(file.path(saved$path, "report.md")), artifact), "Exact authored bytes")
  for (item in result$artifacts) check(identical(sha(file.path(saved$path, item$path)), item$sha256), "Artifact hash association")
  check(!dir.exists(file.path(case$out, ".publication-lock")), "Lock released")
  check(!length(list.files(saved$path, "projection-backup", all.files = TRUE)), "No duplicated projection backups in completed audit")
})
test("raw_nonfinite_values_preserved", {
  case <- new_case(); saved <- run(case)
  check(identical(readRDS(file.path(saved$path, "raw-result.rds")), c(NA_real_, NaN, Inf, -Inf, 2)), "Nonfinite value types retained")
})
for (status in c("partial", "failed")) test(paste0("truthful_", status), {
  case <- new_case(); saved <- run(case, status = status)
  check(identical(saved$result$status, status), "Outer retrieval status")
  check(file.exists(file.path(saved$path, if (status == "failed") "diagnostic-output.md" else "output.md")), "Appropriate output kind")
  if (status == "failed") check(!file.exists(file.path(saved$path, "output.md")), "Failed retrieval not normal output")
})
for (fault in c("callback", "artifact", "rename")) test(paste0("rollback_", fault), {
  case <- new_case(); before <- bytes(case$report)
  if (fault == "rename") case$e$file.rename <- function(from, to) {
    if (startsWith(basename(from), ".pending-")) FALSE else base::file.rename(from, to)
  }
  expect_error(run(case, function(id, stage) {
    writeLines("new report", case$report); writeLines("new log", case$log)
    if (fault == "callback") stop("injected callback failure")
    if (fault == "artifact") writeLines("tampered", file.path(stage, "raw-result.rds"))
  }), if (fault == "callback") "injected" else if (fault == "artifact") "artifact" else "publish")
  check(identical(bytes(case$report), before) && !file.exists(case$log), "Exact old/new projection rollback")
  check(!dir.exists(file.path(case$out, ".publication-lock")), "Failure releases lock")
  records <- list.files(file.path(case$out, "utility-runs"), "result.json", recursive = TRUE, full.names = TRUE, all.files = TRUE)
  check(length(records) == 1L, "Diagnostic bundle retained")
  failed <- jsonlite::read_json(records[[1]])
  check(identical(failed$status, "failed") && is.null(failed$results) && isTRUE(failed$error$projections_restored), "No successful partial result")
})
test("foreign_publication_lock_preserved", {
  case <- new_case(); lock <- file.path(case$out, ".publication-lock"); dir.create(lock)
  before <- bytes(case$report)
  expect_error(run(case), "locked")
  check(dir.exists(lock) && identical(bytes(case$report), before) && !file.exists(case$log), "Foreign lock and projections unchanged")
  pending <- list.files(file.path(case$out, "utility-runs"), "result.json", recursive = TRUE, full.names = TRUE, all.files = TRUE)
  check(length(pending) == 1L && identical(jsonlite::read_json(pending[[1]])$status, "pending"), "Lock failure cannot claim completed status")
})
test("incomplete_rollback_retains_original_bytes", {
  case <- new_case(); before <- bytes(case$report)
  expect_error(suppressWarnings(run(case, function(id, stage) {
    unlink(case$report); dir.create(case$report); stop("injected blocked recovery")
  })), "injected")
  record <- list.files(file.path(case$out, "utility-runs"), "result.json", recursive = TRUE, full.names = TRUE, all.files = TRUE)[[1]]
  saved <- jsonlite::read_json(record)
  check(identical(saved$error$projections_restored, FALSE), "Incomplete recovery not claimed successful")
  backup <- file.path(dirname(record), saved$error$recovery_backup)
  check(dir.exists(backup) && identical(bytes(file.path(backup, "1")), before), "Original recovery bytes retained")
  check(file.exists(file.path(backup, "targets.json")), "Recovery target mapping retained")
})
test("immutable_runs_not_overwritten", {
  case <- new_case(); first <- run(case); first_hash <- sha(file.path(first$path, "result.json")); second <- run(case)
  check(!identical(first$path, second$path) && identical(first_hash, sha(file.path(first$path, "result.json"))), "Earlier utility evidence intact")
})
test("symlink_projection_refused", {
  case <- new_case(); target <- file.path(case$out, "external.txt"); writeLines("untouched", target)
  check(file.symlink(target, case$log), "Create test symlink")
  expect_error(run(case), "Unsafe")
  check(identical(readLines(target), "untouched"), "No symlink-target change")
})
test("symlink_audit_directory_refused", {
  case <- new_case(); other <- tempfile("outside-", root); dir.create(other)
  check(file.symlink(other, file.path(case$out, "utility-runs")), "Create test directory link")
  expect_error(run(case), "Unsafe")
  check(!length(list.files(other, all.files = TRUE, no.. = TRUE)), "No escaped audit files")
})
test("symlink_output_root_refused", {
  case <- new_case(); alias <- file.path(root, paste0(basename(case$out), "-alias"))
  check(file.symlink(case$out, alias), "Create output-root alias")
  expect_error(case$e$nlss_publish_utility("calc", alias, list(), list(), "x", function(id, stage) NULL), "symlink")
  check(!dir.exists(file.path(case$out, "utility-runs")), "Root alias caused no publication")
})
test("unsafe_artifact_and_projection_paths_refused", {
  case <- new_case()
  expect_error(case$e$nlss_publish_utility("calc", case$out, list(), list(), "x", function(id, stage) NULL,
    artifacts = list("../escape" = "no")), "artifact")
  expect_error(run(case, targets = file.path(root, "outside.md")), "Unsafe")
})
test("manifest_rollback_under_project_lock", {
  case <- new_case(); manifest <- file.path(case$out, "nlss-workspace.yml")
  yaml::write_yaml(list(version = 1L, datasets = list()), manifest); before <- bytes(manifest)
  dataset <- file.path(case$out, "sample"); dir.create(dataset)
  expect_error(case$e$nlss_publish_utility("metaskill_runner", dataset, list(), list(), "semantic event",
    function(id, stage) { writeLines("broken", manifest); stop("injected") }), "injected")
  check(identical(bytes(manifest), before), "Manifest restored exactly")
})
test("unregistered_dataset_uses_explicit_project_lock", {
  case <- new_case(); dataset <- file.path(case$out, "sample"); dir.create(dataset)
  lock <- file.path(case$out, ".publication-lock"); dir.create(lock)
  expect_error(case$e$nlss_publish_utility("metaskill_runner", dataset, list(), list(), "event",
    function(id, stage) stop("must not execute"), workspace_root = case$out), "locked")
  check(dir.exists(lock) && !dir.exists(file.path(dataset, ".publication-lock")), "Project lock respected without a manifest")
})
if (!length(results)) stop("No cases selected")
jsonlite::write_json(list(results = results, assertions = assertions, passed = all(vapply(results, `[[`, logical(1), "passed"))),
  file.path(root, "summary.json"), pretty = TRUE, auto_unbox = TRUE)
cat("Utility contract: ", sum(vapply(results, `[[`, logical(1), "passed")), "/", length(results), "; ", assertions, " assertions\n", sep = "")
if (keep > 0) {
  old <- sort(list.dirs(dirname(root), recursive = FALSE, full.names = TRUE), decreasing = TRUE)
  if (length(old) > keep) unlink(setdiff(old[(keep + 1L):length(old)], root), recursive = TRUE)
}
if (!all(vapply(results, `[[`, logical(1), "passed"))) quit(status = 1L)
