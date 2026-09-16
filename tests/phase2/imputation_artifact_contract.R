#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Isolated artifact/metadata publication and replay-payload boundary checks.
script <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])[1]
repo <- normalizePath(file.path(dirname(script), "../.."), winslash = "/")
args <- commandArgs(TRUE)
if ("--help" %in% args) { cat("Usage: imputation_artifact_contract.R [--root PATH] [--keep N] [--match REGEX]\n"); quit(status = 0) }
if (length(args) %% 2L || (length(args) && any(!args[seq(1L, length(args), 2L)] %in% c("--root", "--keep", "--match")))) stop("Unknown/incomplete test option")
arg <- function(key, default) { i <- which(args == key); if (length(i) > 1L) stop("Repeated option"); if (length(i)) args[i + 1L] else default }
cfg <- yaml::read_yaml(file.path(repo, "tests/tests.yml"), eval.expr = FALSE)$tests
forced <- arg("--root", Sys.getenv("NLSS_TEST_ROOT", ""))
if (!nzchar(forced)) forced <- file.path(repo, cfg$output_dir)
collection <- file.path(forced, "phase2-imputation-artifact")
dir.create(collection, recursive = TRUE, showWarnings = FALSE)
collection <- normalizePath(collection, winslash = "/")
keep <- suppressWarnings(as.numeric(arg("--keep", Sys.getenv("NLSS_KEEP_RUNS", as.character(cfg$keep_runs_default)))))
if (length(keep) != 1L || !is.finite(keep) || keep < 0L || keep != floor(keep)) stop("Invalid --keep")
pattern <- arg("--match", ".*"); invisible(grepl(pattern, "validate regex"))
root <- tempfile("run-", collection); dir.create(root)
Sys.unsetenv("NLSS_REPLAY_REQUEST")
config <- file.path(root, "config.yml"); yaml::write_yaml(list(defaults = list(digits = 12L)), config)
Sys.setenv(NLSS_CONFIG_PATH = config, OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1")
sha <- function(path) digest::digest(file = path, algo = "sha256")
write_json <- function(value, path) jsonlite::write_json(value, path, pretty = TRUE, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
utc <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
files <- sort(list.files(file.path(repo, "scripts/R"), "[.]R$", recursive = TRUE, full.names = TRUE))
source_identity <- setNames(vapply(files, sha, character(1)), substring(files, nchar(repo) + 2L))
started <- utc()
write_json(list(started_utc = started, selected_pattern = pattern, runner_sha256 = sha(script),
  production_files = as.list(source_identity)), file.path(root, "started.json"))
checks <- 0L; results <- list(); registered <- character()
check <- function(ok, label) { if (!isTRUE(ok)) stop(label, call. = FALSE); checks <<- checks + 1L }
test <- function(name, code) {
  registered <<- c(registered, name)
  if (!grepl(pattern, name)) return(invisible(NULL))
  prior <- getwd(); on.exit(setwd(prior)); before <- checks; start <- proc.time()[["elapsed"]]
  error <- tryCatch({ force(code); NULL }, error = conditionMessage)
  results[[length(results) + 1L]] <<- list(test = name, passed = is.null(error), checks = checks - before,
    seconds = unname(proc.time()[["elapsed"]] - start), message = if (is.null(error)) "OK" else error)
  cat(if (is.null(error)) "[PASS] " else "[FAIL] ", name, if (is.null(error)) "" else paste0(": ", error), "\n", sep = "")
}
expect_error <- function(code, pattern, label) {
  message <- tryCatch({ force(code); "" }, error = conditionMessage)
  check(nzchar(message) && grepl(pattern, message, ignore.case = TRUE), paste(label, message))
}
bytes <- function(path) if (file.exists(path) && !dir.exists(path)) readBin(path, "raw", file.info(path)$size) else NULL
clone <- function(object) unserialize(serialize(object, NULL, version = 3L))
data <- data.frame(id = 1:20, x = sqrt(1:20) + rep(c(0.3, -0.2), 10), y = sin(1:20) + (1:20) / 10)
data$x[c(3, 8, 16)] <- NA_real_; data$y[c(5, 12)] <- NA_real_
fixture <- mice::mice(data, m = 2L, maxit = 2L, printFlag = FALSE, seed = 11L)
new_case <- function(name) {
  base <- tempfile(paste0(name, "-"), root); dir.create(base); setwd(base)
  e <- new.env(parent = globalenv())
  source(file.path(repo, "scripts/R/lib/bootstrap.R"), local = e); e$nlss_bootstrap(e)
  e$source_lib("imputation_artifact.R")
  conf <- e$get_builtin_config(); conf$defaults$output_dir <- file.path(base, "project"); e$config_env$config <- conf
  input <- file.path(base, "sample.rds"); saveRDS(data, input)
  before <- e$load_dataframe(list(rds = input)); out <- e$get_workspace_out_dir(before)
  paths <- c(working = file.path(out, "sample.parquet"), dictionary = file.path(out, "dictionary.json"),
    codebook = file.path(out, "codebook.md"), binding = file.path(out, "import.json"),
    report = file.path(out, "report_canonical.md"), log = file.path(out, "analysis_log.jsonl"))
  writeLines("Existing report.", paths[["report"]]); writeLines("{\"module\":\"earlier\"}", paths[["log"]])
  list(base = base, e = e, before = before, out = out, project = dirname(out),
    ref = attr(before, "nlss_dataset_ref"), paths = paths, prior = lapply(paths, bytes), state = new.env())
}
run <- function(case, object = fixture, before = function(e) NULL, after = function(e, artifact) NULL, replay = NULL) {
  e <- case$e
  e$nlss_run_main("impute", function() {
    e$nlss_begin_run("impute", case$before, list(log = FALSE))
    e$nlss_resolve_request(list(engine = "mice", seed = 11L), list(replay = list(eligible = TRUE)))
    if (!is.null(replay)) e$nlss_run_context$replay <- list(result = list(results = list(imputation_artifact = replay)))
    before(e)
    artifact <- e$nlss_preserve_mice_artifact(object, case$out, case$ref, 11L)
    case$state$artifact <- artifact
    e$nlss_set_result(list(imputation_artifact = artifact, inference_pooled = FALSE))
    e$nlss_stage_report(case$paths[["report"]], "Imputation artifact fixture", "", "No pooled inference is claimed.")
    after(e, artifact)
  })
}
artifact_paths <- function(case) list.files(file.path(case$out, "imputations"), "mids[.]rds$", recursive = TRUE, full.names = TRUE)
assert_failed <- function(case, label) {
  terminal <- list.files(file.path(case$out, "runs"), "result[.]json$", recursive = TRUE, full.names = TRUE)
  check(length(terminal) >= 1L, paste(label, "terminal bundle retained"))
  saved <- jsonlite::read_json(tail(terminal, 1L), simplifyVector = FALSE)
  check(identical(saved$status, "failed"), paste(label, "failed status"))
  check(!file.exists(file.path(dirname(tail(terminal, 1L)), "output.md")), paste(label, "no normal output"))
  for (name in names(case$paths)) check(identical(bytes(case$paths[[name]]), case$prior[[name]]), paste(label, name, "unchanged"))
  for (lock in c(".analysis-lock", ".import-lock")) check(!dir.exists(file.path(case$out, lock)), paste(label, lock, "released"))
}

test("consumer_compatible_exact_preservation", {
  case <- new_case("normal"); run(case); ref <- case$state$artifact
  check(length(artifact_paths(case)) == 1L, "Exactly one immutable mids")
  verified <- case$e$nlss_verify_imputation_artifact(ref, case$project, case$ref)
  check(case$e$nlss_mids_equal(fixture, verified$mids, FALSE), "Original complete mids preserved")
  check(identical(verified$metadata_sha256, ref$metadata_sha256), "Metadata byte hash bound")
  check(identical(ref$inference_pooled, FALSE), "Artifact not mistaken for pooled inference")
  check(identical(ref$payload_comparison$excluded_runtime_fields, c("call", "date")), "Only proved runtime fields excluded")
  check(identical(ref$payload_comparison$byte_reproducibility, FALSE), "No byte reproducibility claim")
  records <- list.files(file.path(case$out, "runs"), "imputation-artifact.json", recursive = TRUE, full.names = TRUE)
  check(length(records) == 1L, "Mandatory run-local reference with log FALSE")
  check(identical(case$e$import_json(case$e$read_import_json(records)), case$e$import_json(ref)), "Run-local reference exactly matches result")
  check(identical(bytes(case$paths[["working"]]), case$prior[["working"]]), "Artifact preservation itself does not mutate working data")
})

test("scientific_payload_all_fields_and_formula_context", {
  case <- new_case("payload"); e <- case$e
  another <- mice::mice(data, m = 2L, maxit = 2L, printFlag = FALSE, seed = 11L)
  check(e$nlss_mids_equal(fixture, another), "Independent same-seed scientific payload matches")
  runtime <- clone(fixture); runtime$call <- quote(mice::mice(replay_input)); runtime$date <- runtime$date + 1
  check(e$nlss_mids_equal(fixture, runtime), "Call/date changes do not defeat scientific replay")
  check(!e$nlss_mids_equal(fixture, runtime, FALSE), "Exact roundtrip still checks call/date")
  for (field in setdiff(names(fixture), c("call", "date"))) {
    changed <- clone(fixture); changed[[field]] <- list(tampered = TRUE, original = changed[[field]])
    check(!e$nlss_mids_equal(fixture, changed), paste("Scientific field protected:", field))
  }
  changed <- clone(fixture); changed$imp$x[1, 1] <- changed$imp$x[1, 1] + 1e-12
  check(!e$nlss_mids_equal(fixture, changed), "No numerical tolerance hides changed draw")
  changed <- clone(fixture); attr(changed$data$x, "new_attribute") <- "changed"
  check(!e$nlss_mids_equal(fixture, changed), "Scientific data attributes compared")
  changed <- clone(fixture); assign("i", 999L, envir = environment(changed$formulas[[1]]))
  check(!e$nlss_mids_equal(fixture, changed), "Generated formula captured bindings protected")
  changed <- clone(fixture); environment(changed$formulas[[1]]) <- new.env(parent = globalenv())
  check(!e$nlss_mids_equal(fixture, changed), "Exotic formula environment refused")
})

test("generation_input_version_marker_preserved", {
  case <- new_case("version-marker")
  marked <- data; attr(marked, "nlss_input_version_id") <- case$ref$version_id
  object <- mice::mice(marked, m = 2L, maxit = 2L, printFlag = FALSE, seed = 11L)
  run(case, object)
  verified <- case$e$nlss_verify_imputation_artifact(case$state$artifact, case$project, case$ref)
  check(identical(attr(verified$mids$data, "nlss_input_version_id"), case$ref$version_id), "Generation input-version marker preserved")
  check(case$e$nlss_mids_equal(object, verified$mids, FALSE), "Marked returned mids remains exact")
})

test("replay_reuses_original_bytes_with_runtime_differences", {
  case <- new_case("replay"); run(case); ref <- case$state$artifact
  path <- file.path(case$project, ref$path); metadata <- file.path(case$project, ref$metadata_path)
  old_bytes <- bytes(path); old_meta <- bytes(metadata)
  regenerated <- clone(fixture); regenerated$date <- regenerated$date + 1; regenerated$call <- quote(mice::mice(replayed_data))
  run(case, regenerated, replay = ref)
  check(length(artifact_paths(case)) == 1L, "Replay does not publish new serialized mids")
  check(identical(bytes(path), old_bytes) && identical(bytes(metadata), old_meta), "Replay never alters original artifact/metadata")
  check(identical(case$e$import_json(case$state$artifact), case$e$import_json(ref)), "Replay returns original authenticated reference")
})

test("identical_normal_artifact_reuse_never_overwrites", {
  case <- new_case("reuse"); run(case); ref <- case$state$artifact
  old <- bytes(file.path(case$project, ref$metadata_path)); run(case)
  check(length(artifact_paths(case)) == 1L, "Byte-identical normal result reuses one immutable artifact")
  check(identical(bytes(file.path(case$project, ref$metadata_path)), old), "Existing metadata unchanged")
})

for (kind in c("roundtrip", "metadata_write", "artifact_rename", "source_mismatch", "seed_mismatch", "version_marker", "second_call",
               "artifact_tamper", "metadata_tamper", "reference_tamper", "reference_omitted")) local({
  selected_kind <- kind
  test(paste0("failure_", selected_kind), {
    case <- new_case(selected_kind); object <- clone(fixture); e <- case$e
    if (selected_kind == "roundtrip") e$saveRDS <- function(object, file, ...) {
      object$imp$x[1, 1] <- object$imp$x[1, 1] + 1
      base::saveRDS(object, file, ...)
    }
    if (selected_kind == "metadata_write") {
      original_writer <- e$write_import_json
      e$write_import_json <- function(value, path) {
        if (basename(path) == "metadata.json") stop("injected metadata write failure")
        original_writer(value, path)
      }
    }
    if (selected_kind == "artifact_rename") e$file.rename <- function(from, to) {
      if (startsWith(basename(from), ".mice-")) return(FALSE)
      base::file.rename(from, to)
    }
    if (selected_kind == "source_mismatch") object$data$x[1] <- object$data$x[1] + 1
    if (selected_kind == "seed_mismatch") object$seed <- 12L
    if (selected_kind == "version_marker") attr(object$data, "nlss_input_version_id") <- "wrong-input-version"
    after <- function(e, ref) {
      if (selected_kind == "second_call") e$nlss_preserve_mice_artifact(object, case$out, case$ref, 11L)
      if (selected_kind == "artifact_tamper") writeBin(charToRaw("changed artifact"), file.path(case$project, ref$path))
      if (selected_kind == "metadata_tamper") writeLines("{}", file.path(case$project, ref$metadata_path))
      if (selected_kind == "reference_tamper") writeLines("{}", file.path(e$nlss_run_context$staging, "imputation-artifact.json"))
      if (selected_kind == "reference_omitted") e$nlss_set_result(list(inference_pooled = FALSE))
    }
    expect_error(run(case, object, after = after), ".", selected_kind)
    assert_failed(case, selected_kind)
    leftovers <- list.files(file.path(case$out, "imputations"), "^[.]mice-", all.files = TRUE)
    check(!length(leftovers), "Owned unpublished artifact staging cleaned")
  })
})

for (kind in c("parent_symlink", "artifact_symlink", "rds_symlink", "metadata_symlink", "reference_escape", "metadata_binding")) local({
  selected_kind <- kind
  test(paste0("integrity_", selected_kind), {
    case <- new_case(selected_kind); e <- case$e
    outside <- file.path(case$base, "outside"); dir.create(outside)
    sentinel <- file.path(outside, "sentinel"); writeLines("Do not change", sentinel); prior <- bytes(sentinel)
    if (selected_kind == "parent_symlink") {
      check(file.symlink(outside, file.path(case$out, "imputations")), "Created isolated parent symlink fixture")
      expect_error(run(case), "symlink", "Symlink parent refused")
      assert_failed(case, selected_kind)
      check(identical(list.files(outside), "sentinel"), "No artifact written outside dataset")
    } else {
      run(case); ref <- case$state$artifact
      rds <- file.path(case$project, ref$path); meta <- file.path(case$project, ref$metadata_path)
      if (selected_kind == "artifact_symlink") {
        moved <- file.path(outside, basename(dirname(rds))); check(file.rename(dirname(rds), moved), "Moved owned artifact fixture")
        check(file.symlink(moved, dirname(rds)), "Created isolated artifact symlink")
      }
      if (selected_kind %in% c("rds_symlink", "metadata_symlink")) {
        target <- if (selected_kind == "rds_symlink") rds else meta
        moved <- file.path(outside, basename(target)); check(file.rename(target, moved), "Moved owned file fixture")
        check(file.symlink(moved, target), "Created isolated file symlink")
      }
      if (selected_kind == "reference_escape") ref$path <- "../outside/sentinel"
      if (selected_kind == "metadata_binding") {
        altered <- e$read_import_json(meta); altered$m <- altered$m + 1L; e$write_import_json(altered, meta)
        ref$m <- altered$m; ref$metadata_sha256 <- sha(meta)
      }
      expect_error(e$nlss_verify_imputation_artifact(ref, case$project, case$ref), ".", selected_kind)
    }
    check(identical(bytes(sentinel), prior), "Outside sentinel unchanged")
  })
})

unchanged <- identical(unname(vapply(files, sha, character(1))), unname(source_identity))
summary <- list(started_utc = started, finished_utc = utc(), total = length(results), checks = checks,
  passed = sum(vapply(results, function(x) x$passed, logical(1))), registered_total = length(registered),
  selected_pattern = pattern, source_unchanged = unchanged, results = results)
write_json(summary, file.path(root, "summary.json"))
cat("Artifacts: ", root, "\n", sep = "")
if (keep > 0L) {
  older <- list.dirs(collection, recursive = FALSE, full.names = TRUE)
  older <- older[startsWith(basename(older), "run-") & older != root]
  older <- older[order(file.info(older)$mtime, decreasing = TRUE)]
  if (length(older) >= keep) for (path in older[seq.int(keep, length(older))]) unlink(path, recursive = TRUE)
}
quit(status = if (length(results) && summary$passed == length(results) && unchanged) 0L else 1L)
