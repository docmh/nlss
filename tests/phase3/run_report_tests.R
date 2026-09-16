#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Preserved semantic text and referenced evidence: no report generation or replay.
args <- commandArgs(TRUE)
script <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))[1], winslash = "/", mustWork = TRUE)
repo <- normalizePath(file.path(dirname(script), "../.."), winslash = "/", mustWork = TRUE)
if (identical(args, "--help")) {
  cat("Usage: run_report_tests.R [--root PATH] [--keep N] [--match REGEX]\n",
      "Honors NLSS_TEST_ROOT, NLSS_KEEP_RUNS and NLSS_TESTS_CONFIG. Forced roots are never pruned.\n", sep = "")
  quit(status = 0L)
}
if (length(args) %% 2L || (length(args) && any(!args[seq.int(1L, length(args), 2L)] %in% c("--root", "--keep", "--match")))) stop("Unknown or incomplete runner option.")
option <- function(name, fallback) {
  i <- which(args == name)
  if (length(i) > 1L) stop("Repeated runner option: ", name)
  if (length(i)) args[i + 1L] else fallback
}
required <- c("yaml", "jsonlite", "digest", "arrow")
for (p in required) if (!requireNamespace(p, quietly = TRUE)) stop("Missing test package: ", p)
absolute <- function(path) normalizePath(if (grepl("^(/|[A-Za-z]:|\\\\)", path)) path else file.path(repo, path), winslash = "/", mustWork = FALSE)
config_path <- absolute(Sys.getenv("NLSS_TESTS_CONFIG", "tests/tests.yml"))
settings <- yaml::read_yaml(config_path, eval.expr = FALSE)$tests
golden <- absolute(settings$golden_dataset); note_fixture <- absolute(settings$phase3$study_document)
if (!file.exists(golden) || !file.exists(note_fixture)) stop("Missing registered Phase 3 fixture.")
forced <- option("--root", Sys.getenv("NLSS_TEST_ROOT", ""))
keep_text <- option("--keep", Sys.getenv("NLSS_KEEP_RUNS", as.character(settings$keep_runs_default)))
if (!grepl("^[0-9]+$", keep_text)) stop("Invalid --keep/NLSS_KEEP_RUNS.")
keep <- suppressWarnings(as.integer(keep_text)); if (is.na(keep)) stop("Out-of-range retention count.")
pattern <- option("--match", ".*"); invisible(grepl(pattern, "validate regex"))
collection <- file.path(if (nzchar(forced)) absolute(forced) else absolute(settings$output_dir), "phase3-reports")
work <- file.path(collection, paste0("run-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid()))
if (file.exists(work) || !dir.create(work, recursive = TRUE)) stop("Cannot create fresh test directory.")
work <- normalizePath(work, winslash = "/", mustWork = TRUE)
sha <- function(path) digest::digest(file = path, algo = "sha256")
utc <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
json <- function(path) jsonlite::read_json(path, simplifyVector = FALSE)
write_json <- function(value, path) jsonlite::write_json(value, path, auto_unbox = TRUE, null = "null", digits = NA, pretty = TRUE)
text <- function(path) if (file.exists(path)) paste(readLines(path, warn = FALSE), collapse = "\n") else ""
write_text <- function(path, value) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(value, path, useBytes = TRUE)
}
source_paths <- sort(unique(c(script, config_path, golden, note_fixture, file.path(repo, "scripts/config.yml"),
  list.files(file.path(repo, "scripts/R"), "[.]R$", recursive = TRUE, full.names = TRUE))))
hash_sources <- function() setNames(lapply(source_paths, sha), vapply(source_paths, function(path) {
  if (startsWith(path, paste0(repo, "/"))) substring(path, nchar(repo) + 2L) else path
}, character(1)))
initial_sources <- hash_sources(); started <- utc()
results <- commands <- assertions <- list(); current <- "setup"
save_results <- function(status = NULL, finished = NULL) {
  write_json(list(schema_version = 1L, suite = "phase3-reports", owner = "nlss-report-test-runner",
    command = c(file.path(R.home("bin"), "Rscript"), script, args), match = pattern,
    started_at = started, finished_at = finished, exit_status = status,
    source_sha256 = initial_sources, source_unchanged = identical(initial_sources, hash_sources()),
    environment = list(r = R.version.string, platform = R.version$platform,
      packages = as.list(vapply(required, function(p) as.character(utils::packageVersion(p)), character(1)))),
    tests = results, commands = commands), file.path(work, "results.json"))
}
check <- function(ok, label) {
  passed <- isTRUE(ok); assertions[[length(assertions) + 1L]] <<- list(assertion = label, passed = passed)
  if (!passed) stop(label, call. = FALSE)
  invisible(TRUE)
}
skip <- function(label) assertions[[length(assertions) + 1L]] <<- list(assertion = label, skipped = TRUE)
expect_error <- function(code, pattern = NULL) {
  problem <- tryCatch({ force(code); NULL }, error = conditionMessage)
  check(!is.null(problem), "Invalid operation reports an error")
  if (!is.null(pattern)) check(grepl(pattern, problem, ignore.case = TRUE), paste("Error identifies", pattern))
  invisible(problem)
}
file_state <- function(path) {
  link <- Sys.readlink(path)
  if (!is.na(link) && nzchar(link)) return(list(kind = "symlink", link = link))
  if (!file.exists(path)) return(list(kind = "missing"))
  info <- file.info(path)
  if (isTRUE(info$isdir)) return(list(kind = "directory"))
  regular <- if (.Platform$OS.type == "unix") {
    executable <- Sys.which("test"); if (!nzchar(executable)) stop("POSIX inventory requires test -f.")
    system2(executable, c("-f", shQuote(path)), stdout = FALSE, stderr = FALSE) == 0L
  } else isTRUE(file_test("-f", path))
  if (!regular) return(list(kind = "special"))
  list(kind = "file", sha256 = sha(path), mtime = as.numeric(info$mtime), mode = as.character(info$mode))
}
tree <- function(root) {
  values <- list()
  visit <- function(path, relative) {
    state <- file_state(path); values[[relative]] <<- state
    if (identical(state$kind, "directory")) for (name in sort(list.files(path, all.files = TRUE, no.. = TRUE))) {
      visit(file.path(path, name), if (relative == ".") name else paste(relative, name, sep = "/"))
    }
  }
  visit(root, "."); values
}
readonly <- function(root, code) {
  before <- tree(root); result <- tryCatch(force(code), error = identity)
  check(identical(before, tree(root)), "Inspection preserves every path, file hash, mtime, mode and symlink")
  if (inherits(result, "error")) stop(result)
  result
}
config <- file.path(work, "config.yml")
yaml::write_yaml(list(defaults = list(output_dir = file.path(work, "unused-output"), digits = 10L)), config)
Sys.setenv(NLSS_CONFIG_PATH = config, OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1")
Sys.unsetenv("NLSS_REPLAY_REQUEST")
runtime <- function() {
  e <- new.env(parent = globalenv()); source(file.path(repo, "scripts/R/lib/bootstrap.R"), local = e)
  e$nlss_bootstrap(e); e$source_lib("project_store.R"); e$source_lib("project_report.R"); e
}
cli <- function(module, argv, cwd = work, failure = FALSE, timeout = 90L) {
  old <- getwd(); on.exit(setwd(old), add = TRUE); setwd(cwd)
  out <- tempfile("stdout-", work); err <- tempfile("stderr-", work); on.exit(unlink(c(out, err)), add = TRUE)
  command <- c(file.path(repo, "scripts/R", paste0(module, ".R")), argv); began <- utc()
  status <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), shQuote(command), stdout = out, stderr = err, timeout = timeout))
  output <- text(out); error <- text(err)
  record <- list(test = current, command = c(file.path(R.home("bin"), "Rscript"), command), cwd = cwd,
    started_at = began, finished_at = utc(), exit_status = as.integer(status),
    stdout = substr(output, 1L, 16000L), stderr = substr(error, 1L, 16000L),
    stdout_truncated = nchar(output) > 16000L, stderr_truncated = nchar(error) > 16000L)
  commands[[length(commands) + 1L]] <<- record; save_results()
  check(if (failure) status != 0L && status != 124L else status == 0L, paste(module, "expected exit:", status, record$stderr))
  if (identical(module, "project_report")) {
    if (failure) check(!nzchar(output) && nzchar(error), "Rejected report CLI writes feedback to stderr, no misleading JSON stdout")
    else {
      check(jsonlite::validate(output), "Successful report CLI stdout is exactly one JSON document")
      return(jsonlite::fromJSON(output, simplifyVector = FALSE))
    }
  }
  record
}
test <- function(name, code) {
  if (!grepl(pattern, name)) return(invisible(NULL))
  current <<- name; assertions <<- list(); began <- utc(); elapsed <- proc.time()[["elapsed"]]
  old <- getwd(); on.exit(setwd(old), add = TRUE)
  problem <- tryCatch({ force(code); NULL }, error = conditionMessage)
  results[[length(results) + 1L]] <<- list(test = name, passed = is.null(problem), started_at = began,
    finished_at = utc(), seconds = unname(proc.time()[["elapsed"]] - elapsed),
    message = if (is.null(problem)) "OK" else problem, assertions = assertions)
  save_results(); cat(if (is.null(problem)) "[PASS] " else "[FAIL] ", name,
    if (is.null(problem)) "" else paste0(": ", problem), "\n", sep = "")
}
seed <- NULL
seed_project <- function() {
  if (!is.null(seed)) return(seed)
  root <- file.path(work, "seed-project"); dir.create(root)
  e <- runtime(); e$write_parquet_data(utils::read.csv(golden, stringsAsFactors = FALSE), file.path(root, "Original.parquet"))
  check(file.copy(note_fixture, file.path(root, "research_note.md")), "Registered synthetic research note copied")
  cli("project_create", c("--project", root, "--source", "Original.parquet", "--working", "data/current.parquet", "--name", "study"))
  cli("descriptive_stats", c("--project", root, "--dataset", "study", "--vars", "x1"))
  cli("data_transform", c("--project", root, "--dataset", "study", "--calc", "x1_plus=x1+10"))
  paths <- list.dirs(file.path(root, ".nlss/runs"), recursive = FALSE, full.names = TRUE)
  paths <- paths[!startsWith(basename(paths), ".")]
  check(length(paths) == 2L && all(vapply(paths, function(p) identical(json(file.path(p, "result.json"))$status, "completed"), logical(1))), "Seed contains two real completed managed runs")
  seed <<- list(root = root, ids = basename(paths)); seed
}
case <- function(suffix = "project") {
  original <- seed_project(); root <- file.path(work, "cases", current, suffix)
  dir.create(root, recursive = TRUE)
  for (name in list.files(original$root, all.files = TRUE, no.. = TRUE)) check(file.copy(file.path(original$root, name), root,
    recursive = TRUE, copy.mode = TRUE, copy.date = TRUE), "Private copy retains seed evidence")
  report <- file.path(root, "Manuskript.md")
  writeBin(charToRaw(paste0("# Eine frei verfasste Synthese – ä\r\n\r\n",
    "This synthetic draft weighs design limitations; it asserts no real-world finding.\r\n",
    "Its author chooses the structure, not a statistical table template.\r\n\r\n",
    "```r\r\nstop(\"quoted code must never execute\")\r\n```\r\n")), report)
  list(root = root, ids = original$ids, report = report, note = file.path(root, "research_note.md"), env = runtime())
}
finalize <- function(context, report_id = "manuskript", runs = context$ids) {
  context$env$nlss_save_report(context$root, "Manuskript.md", paste(runs, collapse = ","), report_id)
}
inspect <- function(context, revision, report = NULL, id = "manuskript") {
  readonly(context$root, context$env$nlss_inspect_report(context$root, id, revision, report = report, verify = TRUE))
}
records <- function(context) {
  paths <- list.files(file.path(context$root, ".nlss/reports"), "^rev-[a-f0-9]{64}[.]json$", recursive = TRUE, full.names = TRUE)
  setNames(lapply(paths, file_state), substring(paths, nchar(context$root) + 2L))
}
protected <- function(context) list(report = file_state(context$report), note = file_state(context$note),
  source = file_state(file.path(context$root, "Original.parquet")), working = tree(file.path(context$root, "data")),
  datasets = tree(file.path(context$root, ".nlss/datasets")), runs = tree(file.path(context$root, ".nlss/runs")))
record <- function(context, response) json(file.path(context$root, response$record_path))
verify_record <- function(context, response) {
  check(grepl("^rev-[a-f0-9]{64}$", response$revision_id), "Revision uses a stable content identity")
  value <- record(context, response)
  check(identical(value$kind, "semantic_report_revision") && identical(as.integer(value$schema_version), 1L), "Descriptor identifies semantic text evidence, not a statistical run")
  check(identical(value$revision_id, response$revision_id), "Response and descriptor share revision identity")
  check(identical(value$report$path, paste0(".nlss/objects/", value$report$sha256)) &&
      identical(sha(file.path(context$root, value$report$path)), value$report$sha256), "Preserved report object has exact byte hash")
  check(identical(value$verification$scope, "evidence_integrity_only") && identical(value$verification$computation, "not_reexecuted") &&
      identical(value$verification$semantic_review, "not_certified"), "Verification scope does not certify calculation or semantic interpretation")
  check(length(value$runs) == length(context$ids), "Revision references the selected real analyses")
  for (run in value$runs) for (kind in c("request", "result")) {
    check(identical(sha(file.path(context$root, run[[kind]]$path)), run[[kind]]$sha256), paste("Revision authenticates run", kind))
  }
  value
}
save_results()

test("free_authored_report_exact_bytes_and_idempotent_ordered_selection", {
  context <- case(); before <- protected(context)
  first <- cli("project_report", c("--action", "save", "--project", context$root, "--report-id", "manuskript",
    "--report", "Manuskript.md", "--runs", paste(rev(context$ids), collapse = ",")))
  check(identical(first$status, "saved"), "First explicit finalization publishes a revision")
  revision <- verify_record(context, first)
  check(identical(revision$report$sha256, sha(context$report)), "Finalization preserves arbitrary UTF-8/CRLF Markdown bytes without formatting")
  check(identical(protected(context), before), "Finalization does not overwrite authored text, notes, source, working data or run evidence")
  check(is.null(revision$scientific_context), "No unselected finalization note is automatically captured")
  after <- tree(context$root)
  second <- finalize(context, runs = sort(context$ids))
  check(identical(second$status, "already_saved") && identical(second$revision_id, first$revision_id), "Reordered identical run selection is idempotent")
  check(identical(after, tree(context$root)), "Idempotency preserves object/descriptor counts, bytes and file mtimes")
  context$env$nlss_code_hash <- function() paste(rep("f", 64), collapse = "")
  third <- finalize(context)
  check(identical(third$revision_id, first$revision_id) && identical(after, tree(context$root)), "Finalizer implementation fingerprint does not create duplicate content revisions")
  check(length(records(context)) == 1L, "There is one revision descriptor, not a second journal or statistical run")
})

test("changed_report_revisions_ignore_ordinary_document_edits", {
  context <- case(); first <- finalize(context); original <- verify_record(context, first)
  run_evidence <- tree(file.path(context$root, ".nlss/runs")); original_object <- file_state(file.path(context$root, original$report$path))
  write_text(context$note, "An ordinary document edit, not an NLSS capture event.")
  second <- finalize(context)
  check(identical(first$revision_id, second$revision_id), "Unselected document edits do not create report revisions")
  write_text(context$report, c(text(context$report), "An author-added methodological qualification."))
  third <- finalize(context)
  check(!identical(second$revision_id, third$revision_id) && length(records(context)) == 2L, "Only changed authored text creates a further revision")
  check(identical(run_evidence, tree(file.path(context$root, ".nlss/runs"))) && identical(original_object, file_state(file.path(context$root, original$report$path))), "New revisions preserve old prose and run bundles unchanged")
})

test("inspection_is_readonly_and_distinguishes_visible_text_drift", {
  context <- case(); finalized <- finalize(context)
  without_visible <- inspect(context, finalized$revision_id)
  check(identical(without_visible$status, "verified") && is.null(without_visible$visible_report), "Unselected visible manuscript is not implicitly inspected")
  initial <- inspect(context, finalized$revision_id, "Manuskript.md")
  check(identical(initial$visible_report$comparison, "matches_recorded"), "Identical visible manuscript matches its preserved revision")
  write_text(context$report, "The user's later draft; its changed claims have not been revalidated.")
  changed <- readonly(context$root, cli("project_report", c("--action", "inspect", "--project", context$root,
    "--report-id", "manuskript", "--revision", finalized$revision_id, "--report", "Manuskript.md", "--verify", "TRUE")))
  check(identical(changed$status, "verified") && identical(changed$visible_report$comparison, "differs_from_recorded"), "Historical evidence can verify while current visible text differs")
  missing <- inspect(context, finalized$revision_id, "absent.md")
  check(identical(missing$visible_report$status, "missing"), "Missing current text does not erase a preserved revision")
})

test("old_engine_replay_ineligible_evidence_is_verified_without_execution", {
  context <- case()
  for (id in context$ids) {
    directory <- file.path(context$root, ".nlss/runs", id)
    request <- json(file.path(directory, "request.json")); result <- json(file.path(directory, "result.json"))
    request$code_sha256 <- paste(rep("0", 64), collapse = "")
    request$environment$r_version <- "Historical engine fixture, deliberately not the current environment"
    if (identical(request$module, "data_transform")) request$design$replay <- list(eligible = FALSE, reason = "Preserved historical analysis is intentionally non-replayable")
    write_json(request, file.path(directory, "request.json")); result$artifacts$request$sha256 <- sha(file.path(directory, "request.json"))
    write_json(result, file.path(directory, "result.json"))
  }
  for (name in c("nlss_read_replay", "nlss_load_input", "nlss_managed_load", "load_dataframe", "read_parquet_data", "readRDS", "append_nlss_report", "format_nlss_report", "nlss_run_main")) {
    assign(name, local({ label <- name; function(...) stop("Forbidden execution or template path: ", label) }), envir = context$env)
  }
  before <- protected(context)
  finalized <- finalize(context); inspected <- inspect(context, finalized$revision_id)
  check(identical(inspected$status, "verified"), "Historical internally verified evidence does not require a matching replay engine")
  check(identical(before, protected(context)), "Old-code/non-replayable verification does not execute commands or rewrite evidence")
  check(identical(inspected$revision$verification$semantic_review, "not_certified"), "Preserved authored prose is not automatically certified")
})

test("root_move_and_mutable_dataset_locators_do_not_rewrite_history", {
  context <- case(); finalized <- finalize(context)
  old_records <- records(context); moved <- paste0(context$root, " renamed ä")
  check(file.rename(context$root, moved), "Whole private project moved")
  context$root <- moved; context$report <- file.path(moved, "Manuskript.md"); context$note <- file.path(moved, "research_note.md")
  marker <- file.path(moved, "nlss-workspace.yml"); manifest <- yaml::read_yaml(marker, eval.expr = FALSE)
  manifest$active_dataset <- "renamed-study"; manifest$datasets[[1]]$name <- "renamed-study"
  yaml::write_yaml(manifest, marker)
  dataset_path <- list.files(file.path(moved, ".nlss/datasets"), "^dataset[.]json$", recursive = TRUE, full.names = TRUE)
  dataset <- json(dataset_path); dataset$working <- "user-selected/missing-current.parquet"; write_json(dataset, dataset_path)
  check(file.rename(file.path(moved, "data/current.parquet"), file.path(moved, "moved-current.parquet")), "Visible working data moved without historical relinking")
  verified <- inspect(context, finalized$revision_id, "Manuskript.md")
  check(identical(verified$status, "verified") && identical(old_records, records(context)), "Run-linked report evidence survives root move and mutable dataset name/location changes")
  again <- finalize(context)
  check(identical(again$status, "already_saved") && identical(again$revision_id, finalized$revision_id), "Historical selection does not bind to today's active dataset name")
})

test("missing_failed_pending_duplicate_and_foreign_identity_runs_refuse", {
  for (kind in c("missing", "failed", "pending", "duplicate", "foreign_identity")) {
    context <- case(kind); selection <- context$ids
    if (kind == "missing") selection[1] <- "missing-run"
    if (kind == "pending") selection[1] <- paste0(".pending-", selection[1])
    if (kind == "duplicate") selection <- c(selection, selection[1])
    if (kind == "failed") {
      path <- file.path(context$root, ".nlss/runs", selection[1], "result.json")
      result <- json(path); result$status <- "failed"; write_json(result, path)
    }
    if (kind == "foreign_identity") {
      marker <- file.path(context$root, "nlss-workspace.yml"); value <- yaml::read_yaml(marker, eval.expr = FALSE)
      value$workspace_id <- "another-workspace"; yaml::write_yaml(value, marker)
    }
    before <- protected(context)
    cli("project_report", c("--action", "save", "--project", context$root, "--report-id", "manuskript", "--report", "Manuskript.md",
      "--runs", paste(selection, collapse = ",")), failure = TRUE)
    check(identical(before, protected(context)) && !length(records(context)), paste(kind, "cannot publish a report revision or overwrite existing inputs"))
  }
})

test("tampered_referenced_evidence_refuses_finalization", {
  for (kind in c("request", "output", "dataset")) {
    context <- case(kind); directory <- file.path(context$root, ".nlss/runs", context$ids[1])
    request <- json(file.path(directory, "request.json"))
    target <- switch(kind, request = file.path(directory, "request.json"), output = file.path(directory, "output.md"),
      dataset = file.path(context$root, request$dataset$snapshot_path))
    Sys.chmod(target, "0600"); write_text(target, "Deliberately damaged test evidence")
    before <- protected(context)
    expect_error(finalize(context))
    check(identical(before, protected(context)) && !length(records(context)), paste("Damaged", kind, "is not silently certified or repaired"))
  }
})

test("damaged_report_object_descriptor_or_linked_run_refuses_readonly_inspection", {
  for (kind in c("report_object", "descriptor", "linked_result")) {
    context <- case(kind); finalized <- finalize(context); revision <- record(context, finalized)
    target <- switch(kind, report_object = file.path(context$root, revision$report$path), descriptor = file.path(context$root, finalized$record_path),
      linked_result = file.path(context$root, revision$runs[[1]]$result$path))
    Sys.chmod(target, "0600")
    if (kind == "descriptor") { revision$report$selected_path <- "invented-location.md"; write_json(revision, target) }
    else write_text(target, "Damaged preserved evidence")
    expect_error(inspect(context, finalized$revision_id))
  }
})

test("unsafe_ids_report_paths_and_nested_project_boundaries_refuse", {
  for (kind in c("id", "traversal", "outside", "managed_report", "nested")) {
    context <- case(kind); id <- "manuskript"; report <- "Manuskript.md"
    if (kind == "id") id <- "../escape"
    if (kind == "traversal") report <- "../outside.md"
    if (kind == "outside") { report <- file.path(dirname(context$root), "outside.md"); write_text(report, "User-owned outside manuscript") }
    if (kind == "managed_report") report <- paste0(".nlss/runs/", context$ids[1], "/output.md")
    if (kind == "nested") {
      write_text(file.path(context$root, "nested/nlss-workspace.yml"), "schema_version: 1")
      write_text(file.path(context$root, "nested/manuscript.md"), "Another project's manuscript"); report <- "nested/manuscript.md"
    }
    before <- protected(context)
    cli("project_report", c("--action", "save", "--project", context$root, "--report-id", id, "--report", report,
      "--runs", paste(context$ids, collapse = ",")), failure = TRUE)
    check(identical(before, protected(context)) && !length(records(context)), paste(kind, "is refused without overwriting user files or publishing a revision"))
  }
})

test("fifo_symlink_and_wrong_revision_preflights_are_bounded", {
  for (kind in c("report_fifo", "report_symlink", "descriptor_fifo", "descriptor_symlink", "wrong_revision")) {
    context <- case(kind); finalized <- finalize(context)
    descriptor_path <- file.path(context$root, finalized$record_path)
    selected <- if (startsWith(kind, "report_")) file.path(context$root, "selected.md") else descriptor_path
    if (kind != "wrong_revision") {
      if (endsWith(kind, "fifo")) {
        if (.Platform$OS.type == "windows" || !nzchar(Sys.which("mkfifo"))) { skip(paste(kind, "requires POSIX mkfifo")); next }
        if (file.exists(selected)) unlink(selected)
        check(system2(Sys.which("mkfifo"), shQuote(selected)) == 0L, paste("Private", kind, "created"))
      } else {
        if (file.exists(selected)) unlink(selected)
        if (!isTRUE(suppressWarnings(file.symlink(context$report, selected)))) { skip(paste(kind, "requires OS symlink permission")); next }
      }
    }
    before <- tree(context$root)
    if (startsWith(kind, "report_")) cli("project_report", c("--action", "save", "--project", context$root, "--report-id", "other-report",
      "--report", "selected.md", "--runs", paste(context$ids, collapse = ",")), failure = TRUE, timeout = 8L)
    else cli("project_report", c("--action", "inspect", "--project", context$root, "--report-id", "manuskript", "--revision",
      if (kind == "wrong_revision") "../../outside" else finalized$revision_id), failure = TRUE, timeout = 8L)
    check(identical(before, tree(context$root)), paste(kind, "is rejected without special-file read or project mutation"))
  }
})

test("capture_time_growth_empty_and_postcapture_edits_refuse_publication", {
  for (kind in c("report_growth", "empty_report", "postcapture_edit")) {
    context <- case(kind); e <- context$env; hit <- FALSE
    selected <- "Manuskript.md"
    selected_path <- file.path(context$root, selected)
    limit <- file.info(context$report)$size + 32
    original_limit <- e$nlss_report_limit; original_store <- e$nlss_store_file; original_run <- e$nlss_report_run
    e$nlss_report_limit <- function(which) {
      if (identical(which, "report")) limit else original_limit(which)
    }
    capture_store <- function(root, relative, max_bytes = Inf) {
      if (!hit && identical(relative, selected)) {
        hit <<- TRUE
        check(identical(as.numeric(max_bytes), as.numeric(limit)), "Actual capture receives the configured document byte bound")
        if (kind == "postcapture_edit") {
          value <- original_store(root, relative, max_bytes)
          write_text(selected_path, "An external editor changed the manuscript after capture.")
          return(value)
        }
        if (kind == "empty_report") writeBin(raw(), selected_path)
        else writeBin(charToRaw(paste(rep("x", limit + 1L), collapse = "")), selected_path)
      }
      original_store(root, relative, max_bytes)
    }
    # Dataset verification reloads project_store.R. Install the local capture
    # hook only after the real evidence reader has finished; never skip checks.
    e$nlss_report_run <- function(...) {
      value <- original_run(...)
      e$nlss_store_file <- capture_store
      value
    }
    before <- protected(context)
    expect_error(finalize(context))
    check(hit && !length(records(context)), paste(kind, "is detected at its intended capture/publication boundary"))
    after <- protected(context)
    before[["report"]] <- after[["report"]]
    check(identical(before, after), "Capture failure preserves the external edit and every other protected input")
    check(!dir.exists(file.path(context$root, ".nlss/.analysis-lock")) && !dir.exists(file.path(context$root, ".nlss/.publication-lock")), "Capture failure releases only owned locks")
  }
})

test("postserialization_document_run_and_marker_drift_refuses_publication", {
  for (kind in c("report", "run", "marker")) {
    context <- case(kind); e <- context$env; hit <- FALSE; expected <- NULL
    marker <- file.path(context$root, "nlss-workspace.yml")
    original_write <- e$write_import_json
    e$write_import_json <- function(value, path) {
      result <- original_write(value, path)
      if (!hit && startsWith(basename(path), ".pending-report-")) {
        hit <<- TRUE
        if (kind == "report") write_text(context$report, "An external edit made after revision serialization.")
        if (kind == "run") {
          target <- file.path(context$root, ".nlss/runs", context$ids[1], "result.json")
          changed <- json(target); changed$external_test_annotation <- "Changed during serialization"
          write_json(changed, target)
        }
        if (kind == "marker") {
          changed <- yaml::read_yaml(marker, eval.expr = FALSE)
          changed$external_test_annotation <- "Changed during serialization"
          yaml::write_yaml(changed, marker)
        }
        expected <<- list(inputs = protected(context), marker = file_state(marker))
      }
      result
    }
    expect_error(finalize(context), "changed during report save")
    check(hit && !length(records(context)), paste("Post-serialization", kind, "drift prevents a success revision"))
    check(identical(expected, list(inputs = protected(context), marker = file_state(marker))),
      "Prepublication recheck preserves the exact external edit and every other protected input")
    check(!dir.exists(file.path(context$root, ".nlss/.analysis-lock")) && !dir.exists(file.path(context$root, ".nlss/.publication-lock")),
      "Late drift refusal releases owned locks without overwriting the external editor's changes")
  }
})

test("oversized_serialized_revision_remains_pending_not_published", {
  context <- case(); e <- context$env; staged <- FALSE
  original_write <- e$write_import_json; original_limit <- e$nlss_report_limit
  e$write_import_json <- function(value, path) {
    result <- original_write(value, path)
    if (startsWith(basename(path), ".pending-report-")) staged <<- TRUE
    result
  }
  e$nlss_report_limit <- function(kind) if (staged && identical(kind, "evidence")) 1 else original_limit(kind)
  before <- protected(context)
  expect_error(finalize(context), "revision exceeds")
  check(staged && !length(records(context)), "The serialized final record is size-checked before successful publication")
  pending <- list.files(file.path(context$root, ".nlss/staging"), "^[.]pending-report-", all.files = TRUE, full.names = TRUE)
  check(length(pending) == 1L && file.info(pending)$size > 1, "Oversized pending evidence remains identifiable for inspection")
  check(identical(before, protected(context)), "Record-size refusal cannot overwrite visible files or referenced analyses")
})

test("rehashed_revision_still_requires_valid_metadata_locators_and_claims", {
  for (kind in c("timestamp", "code_hash", "verification_claim", "historical_locator")) {
    context <- case(kind); finalized <- finalize(context)
    value <- record(context, finalized)
    if (kind == "timestamp") value$finalized_at <- "not-a-timestamp"
    if (kind == "code_hash") value$finalizer_code_sha256 <- "not-a-sha256"
    if (kind == "verification_claim") value$verification$semantic_review <- "certified"
    if (kind == "historical_locator") value$report$selected_path <- "../outside.md"
    value$revision_id <- context$env$nlss_report_revision(context$env$nlss_report_payload(value))
    path <- file.path(context$root, ".nlss/reports/manuskript", paste0(value$revision_id, ".json"))
    if (file.exists(path)) Sys.chmod(path, "0600")
    write_json(value, path)
    expect_error(inspect(context, value$revision_id))
  }
})

test("publication_lock_and_descriptor_rename_failure_preserve_visible_text", {
  context <- case()
  for (name in c(".analysis-lock", ".publication-lock")) {
    lock <- file.path(context$root, ".nlss", name)
    dir.create(lock); write_text(file.path(lock, "owner.txt"), "Pre-existing owner; never remove my lock")
    before <- protected(context); prior_lock <- tree(lock); prior_records <- records(context)
    expect_error(finalize(context), "lock")
    check(identical(before, protected(context)) && identical(prior_lock, tree(lock)) && identical(prior_records, records(context)),
      paste("Foreign", name, "remains untouched and prevents revision publication"))
    unlink(lock, recursive = TRUE)
  }
  lock <- file.path(context$root, ".nlss/.publication-lock")
  e <- context$env; hit <- FALSE; protected_before <- protected(context)
  e$file.rename <- function(from, to) {
    if (grepl("^rev-[a-f0-9]{64}[.]json$", basename(to))) { hit <<- TRUE; return(FALSE) }
    base::file.rename(from, to)
  }
  expect_error(finalize(context))
  check(hit && !length(records(context)), "Injected descriptor-publication failure does not leave a falsely published revision")
  check(identical(protected_before, protected(context)), "Failed finalization never overwrites visible prose, note, data or referenced analyses")
  check(!dir.exists(lock), "Failed finalization releases only its own publication lock")
  rm("file.rename", envir = e)
  retry <- finalize(context)
  check(identical(retry$status, "saved") && length(records(context)) == 1L, "Retry can reuse safely preserved objects and publish one revision")
})

test("task_d_ordinary_delivery_browse_and_general_evidence", {
  context <- case(); e <- context$env
  cli("regression", c("--project", context$root, "--dv", "outcome_reg", "--ivs", "x1,x2"))
  cli("regression", c("--project", context$root, "--csv", golden, "--dv", "outcome_reg", "--ivs", "x1"))
  cli("power", c("--project", context$root, "--analysis", "ttest", "--effect-size", "0.5", "--power", "0.8"))
  cli("calc", c("--project", context$root, "--expr", "1+2"))
  ids <- unlist(lapply(c("runs", "utility-runs"), function(kind) basename(list.dirs(file.path(context$root, ".nlss", kind), recursive = FALSE))))
  before <- protected(context); protocol <- file_state(file.path(context$root, "report_canonical.md"))
  saved <- cli("project_report", c("--project", context$root, "--report", "Manuskript.md", "--runs", paste(ids, collapse = ",")))
  check(saved$status == "saved" && startsWith(saved$report_id, "report-"), "Ordinary delivery needs neither action nor researcher-managed report/revision ID")
  check(identical(before, protected(context)) && identical(protocol, file_state(file.path(context$root, "report_canonical.md"))), "Delivery preserves visible editable report, root protocol and statistical bytes")
  value <- record(context, saved)
  check(length(value$runs) == 6L && all(c("regression", "power", "calc") %in% vapply(value$runs, `[[`, "", "module")), "One report preserves managed/explicit-file/parameter/utility evidence without module opt-ins")
  power <- Filter(function(run) run$module == "power", value$runs)[[1]]
  check(is.null(power$dataset), "A parameter-only report dependency has no fabricated dataset")
  history <- readonly(context$root, cli("project_report", c("--action", "inspect", "--project", context$root, "--report", "Manuskript.md")))
  check(history$evidence_verification == "not_checked" && length(history$revisions) == 1L, "Report-path browsing lists saved descriptors without a verification claim")
  checked <- readonly(context$root, e$nlss_inspect_report(context$root, report = "Manuskript.md", revision = saved$revision_id, verify = TRUE))
  check(checked$status == "verified", "Explicit verification supports the same common evidence selection")
  expect_error(e$nlss_save_report(context$root, "report_canonical.md", paste(ids, collapse = ",")), "generated project protocol")
})

test("task_d_lightweight_cross_session_browse_with_missing_evidence", {
  context <- case()
  saved <- context$env$nlss_save_report(context$root, "Manuskript.md", paste(context$ids, collapse = ","))
  path <- file.path(context$root, ".nlss/runs", context$ids[1], "output.md")
  check(file.rename(path, paste0(path, ".user-moved")), "User moves saved output in a private test project")
  moved <- paste0(context$root, " moved")
  check(file.rename(context$root, moved), "Whole project relocated for a fresh-session read")
  context$root <- moved; e <- runtime()
  e$nlss_report_run <- function(...) stop("Full evidence read requested")
  history <- readonly(moved, e$nlss_inspect_report(moved, report = "Manuskript.md"))
  read <- readonly(moved, e$nlss_inspect_report(moved, report = "Manuskript.md", revision = saved$revision_id))
  check(history$status == "listed" && read$status == "read" && read$evidence_verification == "not_checked", "Browsing does not require intact artifacts or a previous in-memory session")
  check(identical(read$revision$report$sha256, sha(file.path(moved, read$revision$report$path))), "Exact saved Markdown is accessible through its existing object reference")
  e <- runtime()
  expect_error(readonly(moved, e$nlss_inspect_report(moved, report = "Manuskript.md", revision = saved$revision_id, verify = TRUE)))
})

test("task_d_removed_note_options_and_no_document_discovery", {
  context <- case(); before <- tree(context$root)
  for (module in c("descriptive_stats", "data_transform", "project_report")) {
    rejected <- cli(module, c("--project", context$root, "--context-note", "research_note.md"), failure = TRUE)
    check(grepl("Unknown option", rejected$stderr, ignore.case = TRUE), "Retired capture switch uses ordinary unknown-option validation")
  }
  rejected <- cli("project_inspect", c("--project", context$root, "--note", "research_note.md"), failure = TRUE)
  check(grepl("Unknown option", rejected$stderr, ignore.case = TRUE), "Retired inspector switch has no compatibility alias")
  check(identical(before, tree(context$root)), "Rejected note options do not mutate user or archived bytes")
  original_hash <- context$env$import_hash
  context$env$import_hash <- function(x, file = FALSE) {
    if (file && endsWith(x, ".md")) stop("Unexpected user document hash")
    original_hash(x, file = file)
  }
  inspected <- readonly(context$root, context$env$nlss_inspect_project(context$root))
  check(!"research_note" %in% names(inspected), "Project inspection neither discovers nor fingerprints an ordinary document")
  for (id in context$ids) {
    path <- file.path(context$root, ".nlss/runs", id)
    check(!"scientific_context" %in% names(json(file.path(path, "request.json"))) &&
      !"scientific_context" %in% names(json(file.path(path, "result.json"))), "New request/result records do not generate note roles or references")
  }
  check(!file.exists(file.path(context$root, ".nlss/objects", sha(context$note))), "Ordinary document bytes are not automatically captured")
})

test("task_e_current_project_view_links_records_and_reports_after_move", {
  context <- case(); e <- context$env
  cli("regression", c("--project", context$root, "--dv", "outcome_reg", "--ivs", "x1,x2"))
  cli("power", c("--project", context$root, "--analysis", "ttest", "--effect-size", "0.5", "--power", "0.8"))
  cli("calc", c("--project", context$root, "--expr", "1+2"))
  cli("metaskill_runner", c("--project", context$root, "--dataset", "study", "--meta", "evidence-navigation", "--phase", "activation"))
  ids <- basename(list.dirs(file.path(context$root, ".nlss/runs"), recursive = FALSE))
  report <- "Reports/Stüdy (final) #1.md"
  write_text(file.path(context$root, report), text(context$report))
  first <- e$nlss_save_report(context$root, report, paste(ids, collapse = ","))
  write_text(file.path(context$root, report), c(text(context$report), "An edited interpretation."))
  second <- e$nlss_save_report(context$root, report, paste(ids, collapse = ","))
  moved <- paste0(context$root, " moved (2) #")
  check(file.rename(context$root, moved), "Current project moved with all evidence and visible files")
  deep <- file.path(moved, "working/deep"); dir.create(deep, recursive = TRUE)
  command <- readonly(moved, cli("project_inspect", c("--format", "json"), cwd = deep))
  view <- jsonlite::fromJSON(command$stdout, simplifyVector = FALSE)
  check(identical(view$project_root, moved) && view$protocol$status == "present", "Nearest-ancestor view exposes the automatic root protocol after relocation")
  runs <- view$project_runs$entries
  check(length(runs) == 4L && all(c("descriptive_stats", "data_transform", "regression", "power") %in% vapply(runs, `[[`, "", "module")), "Common run inventory exposes multiple procedures without storage opt-ins")
  check(all(vapply(runs, function(x) identical(x$recorded_status, "completed") && identical(x$status, "listed") && identical(x$evidence_verification, "not_checked"), logical(1))), "Recorded completion is not presented as integrity verification")
  check(all(vapply(Filter(function(x) x$input_kind == "dataset", runs), function(x) nzchar(x$dataset$dataset_id) && nzchar(x$dataset$version_id), logical(1))), "Each data analysis exposes its recorded input version")
  check(is.null(Filter(function(x) x$module == "power", runs)[[1]]$dataset), "Parameter-only record has no fabricated dataset")
  utilities <- setNames(view$project_utilities$entries, vapply(view$project_utilities$entries, `[[`, "", "module"))
  check(length(utilities) == 2L && utilities$calc$input_kind == "utility", "Utility records retain their distinct inventory")
  check(!is.null(utilities$metaskill_runner$dataset$version_id), "Real dataset-bound utility exposes the version in its common request payload")
  check(length(view$reports$entries) == 2L && all(vapply(view$reports$entries, function(x) x$status == "listed" && x$visible_report$path == report && x$saved_report$status == "present", logical(1))), "Both report revisions expose visible and preserved Markdown paths")
  rendered <- readonly(moved, cli("project_inspect", character(), cwd = deep))$stdout
  matches <- regmatches(rendered, gregexpr("\\]\\(<[^>]+>\\)", rendered, perl = TRUE))[[1]]
  paths <- vapply(matches, function(x) utils::URLdecode(sub("^\\]\\(<|>\\)$", "", sub(">\\)$", "", x))), "")
  check(length(paths) >= 20L && all(file.exists(paths)), "Rendered protocol/data/run/report links resolve despite spaces, Unicode, # and parentheses")
  check(all(startsWith(paths, paste0(moved, "/"))), "Stdout links use the moved project root, not the deep invocation directory")
  check(regexpr("Open analysis protocol", rendered, fixed = TRUE) < regexpr("## Analyses", rendered, fixed = TRUE), "Default evidence protocol is prominent before run details")
  check(grepl("| study / ", rendered, fixed = TRUE), "Input table uses the registered dataset name alongside the recorded version")
})

test("task_e_incomplete_and_damaged_records_do_not_hide_other_results", {
  context <- case(); e <- context$env
  saved <- e$nlss_save_report(context$root, "Manuskript.md", paste(context$ids, collapse = ","))
  output <- file.path(context$root, ".nlss/runs", context$ids[1], "output.md")
  check(file.rename(output, paste0(output, ".moved")), "One output deliberately moved in private fixture")
  write_text(file.path(context$root, ".nlss/runs/broken/result.json"), "{not JSON}")
  write_text(file.path(context$root, ".nlss/runs/.pending-interrupted/request.json"), "{}")
  write_text(file.path(context$root, ".nlss/reports/broken/rev-broken.json"), "not JSON")
  captured <- file.path(context$root, saved$report$path)
  check(file.rename(captured, paste0(captured, ".moved")), "One preserved report deliberately moved in private fixture")
  write_text(file.path(context$root, ".nlss/runs/malformed/request.json"), "{\"run_id\":\"malformed\",\"module\":[],\"schema_version\":1}")
  write_text(file.path(context$root, ".nlss/runs/malformed/result.json"), "{\"run_id\":\"malformed\",\"module\":[],\"schema_version\":1}")
  before <- tree(context$root)
  view <- e$nlss_inspect_project(context$root)
  by_id <- setNames(view$project_runs$entries, vapply(view$project_runs$entries, `[[`, "", "run_id"))
  check(by_id[[context$ids[1]]]$status == "missing_output" && by_id[[context$ids[2]]]$status == "listed", "One missing output does not hide the other completed run")
  check(by_id$broken$status == "incomplete" && by_id$broken$request$status == "missing" && by_id$broken$result$status == "invalid_metadata", "Missing/malformed records retain explicit component statuses")
  check(by_id[[".pending-interrupted"]]$status == "pending" && by_id$malformed$status == "invalid_metadata", "Pending and structurally invalid records cannot become completed evidence")
  states <- vapply(view$reports$entries, `[[`, "", "status")
  check(all(c("missing_report", "invalid_metadata") %in% states), "Unavailable and malformed report records are reported individually")
  invisible(e$nlss_inspection_markdown(view))
  check(identical(before, tree(context$root)), "Inspecting/rendering problems creates no repairs, logs, snapshots or overwritten drafts")
})

test("task_e_metadata_only_browse_and_current_layout_rejection", {
  context <- case(); e <- context$env
  e$nlss_save_report(context$root, "Manuskript.md", paste(context$ids, collapse = ","))
  write_text(file.path(context$root, ".nlss/reports/nested/nlss-workspace.yml"), "not read")
  write_text(file.path(context$root, ".nlss/reports/nested/deep/secret.json"), "not read")
  for (name in c("import_hash", "readRDS", "read_parquet_data", "nlss_report_verify", "nlss_report_run", "nlss_load_input", "nlss_store_file", "nlss_publish_run")) {
    assign(name, function(...) stop("Forbidden data/hash/write dependency during browsing"), envir = e)
  }
  original_read <- e$nlss_project_metadata; reads <- character()
  e$nlss_project_metadata <- function(root, path, ...) {
    reads <<- c(reads, path)
    if (grepl("objects/|staging/|deep/|research_note", path)) stop("Forbidden unrelated content read")
    original_read(root, path, ...)
  }
  view <- readonly(context$root, e$nlss_inspect_project(context$root))
  check(any(vapply(view$reports$entries, function(x) identical(x$status, "other_project_not_inspected"), logical(1))), "Nested project boundary is visible without entering it")
  check(length(reads) == 7L, "Ordinary browsing reads only marker, dataset descriptor, two request/result pairs and one revision descriptor")
  marker <- file.path(context$root, "nlss-workspace.yml")
  value <- yaml::read_yaml(marker); value$schema_version <- 1L; yaml::write_yaml(value, marker)
  expect_error(readonly(context$root, e$nlss_inspect_project(context$root)), "Unsupported project marker")
})

test("task_e_unreadable_candidates_and_absent_protocol_are_honest", {
  context <- case(); e <- context$env
  write_text(file.path(context$root, ".nlss/runs/oversized/request.json"), strrep("x", 2 * 1024^2 + 1L))
  write_text(file.path(context$root, ".nlss/runs/nested/nlss-workspace.yml"), "not read")
  write_text(file.path(context$root, ".nlss/runs/nested/request.json"), "not read")
  for (name in c("request", "result")) {
    value <- json(file.path(context$root, ".nlss/runs", context$ids[1], paste0(name, ".json")))
    value$run_id <- "fractional-schema"; value$schema_version <- 1.5
    path <- file.path(context$root, ".nlss/runs/fractional-schema", paste0(name, ".json"))
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE); write_json(value, path)
  }
  protocol <- file.path(context$root, "report_canonical.md")
  check(file.rename(protocol, paste0(protocol, ".moved")), "Private protocol moved without regenerating it")
  if (.Platform$OS.type == "unix") {
    check(file.symlink(file.path(context$root, ".nlss/runs", context$ids[1]), file.path(context$root, ".nlss/runs/linked")), "Private linked run candidate prepared")
    fifo <- file.path(context$root, ".nlss/runs/fifo/request.json"); dir.create(dirname(fifo))
    check(system2("mkfifo", shQuote(fifo)) == 0L, "Private FIFO prepared")
  } else skip("POSIX symlink/FIFO candidates are Unix-only")
  view <- readonly(context$root, e$nlss_inspect_project(context$root))
  by_id <- setNames(view$project_runs$entries, vapply(view$project_runs$entries, `[[`, "", "run_id"))
  check(by_id$oversized$request$status == "metadata_too_large", "Oversized metadata is labelled, not parsed or truncated")
  check(by_id$nested$request$status == "other_project_not_inspected", "Run inventory does not traverse a nested project")
  check(by_id[["fractional-schema"]]$status == "invalid_metadata", "Fractional schemas are not coerced to supported versions")
  if (.Platform$OS.type == "unix") check(by_id$linked$request$status == "symlink_not_inspected" && by_id$fifo$request$status == "not_a_file", "Symlink and FIFO evidence is not opened")
  markdown <- readonly(context$root, e$nlss_inspection_markdown(view))
  check(view$protocol$status == "missing" && any(grepl("Open analysis protocol (missing)", markdown, fixed = TRUE)), "Absent root protocol is visible without a misleading working link or regeneration")
})

test("metaskill_flags_current_project_and_dataset_selection", {
  context <- case("selected"); peer <- case("unselected")
  before <- protected(context); peer_before <- tree(peer$root)
  cli("metaskill_runner", c("--project", context$root, "--dataset", "study", "--meta", "outside", "--phase", "activation"), cwd = peer$root)
  check(identical(peer_before, tree(peer$root)), "Explicit --project wins over the invocation project's marker without writing to that other project")
  deep <- file.path(context$root, "sub/deep"); dir.create(deep, recursive = TRUE)
  cli("metaskill_runner", c("--meta", "inside", "--phase", "activation", "--log", "FALSE"), cwd = deep)
  marker <- file.path(context$root, "nlss-workspace.yml")
  value <- yaml::read_yaml(marker); value$active_dataset <- "unregistered-default"; yaml::write_yaml(value, marker)
  cli("metaskill_runner", c("--project", context$root, "--dataset", "study", "--meta", "explicit-dataset"))
  check(yaml::read_yaml(marker)$active_dataset == "unregistered-default", "Explicit --dataset overrides, but does not rewrite, the active dataset choice")
  runs <- list.dirs(file.path(context$root, ".nlss/utility-runs"), recursive = FALSE)
  check(length(runs) == 3L, "Each real lifecycle invocation publishes exactly one utility bundle")
  for (run in runs) {
    request <- json(file.path(run, "request.json")); result <- json(file.path(run, "result.json"))
    check(result$status == "completed" && request$module == "metaskill_runner" && result$results$dataset == "study", "Utility uses the selected registered data and completes")
    check(nzchar(request$request$dataset$version_id) && sha(file.path(run, "request.json")) == result$artifacts$request.json$sha256, "Real utility request binds the observed dataset version and matches its saved hash")
  }
  protocol <- text(file.path(context$root, "report_canonical.md"))
  check(all(vapply(c("outside", "inside", "explicit-dataset"), grepl, logical(1), x = protocol, fixed = TRUE)), "Root protocol contains all lifecycle entries, including --log FALSE")
  check(!file.exists(file.path(context$root, ".nlss/report_canonical.md")) && !any(basename(list.files(context$root, recursive = TRUE, all.files = TRUE)) == "analysis_log.jsonl"), "No hidden protocol copy or parallel JSONL journal")
  check(!dir.exists(file.path(context$root, ".nlss/.analysis-lock")), "Successful utility invocations release their acquired input lock")
  check(identical(before, protected(context)), "Selection repair preserves original/working data, report, ordinary note and statistical runs")
})

test("metaskill_flags_rejections_leave_projects_unchanged", {
  context <- case()
  for (selection in list(c("--project", file.path(work, "absent-project")),
                         c("--project", context$root, "--dataset", "unknown"),
                         c("--project", context$root, "--dataset", "study", "--csv", golden))) {
    readonly(context$root, cli("metaskill_runner", c(selection, "--meta", "invalid"), cwd = context$root, failure = TRUE))
  }
  readonly(context$root, cli("metaskill_runner", c("--project", context$root, "--dataset", "study", "--meta", "missing-report", "--phase", "finalization"), failure = TRUE))
  help <- cli("metaskill_runner", "--help")$stdout
  check(grepl("--project", help, fixed = TRUE) && grepl("--dataset", help, fixed = TRUE), "Help documents both selection flags")
  lock <- file.path(context$root, ".nlss/.analysis-lock"); dir.create(lock)
  readonly(context$root, cli("metaskill_runner", c("--project", context$root, "--dataset", "study", "--meta", "foreign-lock"), failure = TRUE))
  check(dir.exists(lock), "Rejected invocation never removes a foreign input lock")
  broken <- case("missing-working")
  working <- file.path(broken$root, "data/current.parquet")
  check(file.rename(working, paste0(working, ".moved")), "Working file moved in a private negative fixture")
  readonly(broken$root, cli("metaskill_runner", c("--project", broken$root, "--dataset", "study", "--meta", "missing-working"), failure = TRUE))
  check(!dir.exists(file.path(broken$root, ".nlss/.analysis-lock")), "Data-loading failure also releases the lock acquired by this invocation")
})

test("metaskill_flags_explicit_finalization_keeps_visible_report", {
  context <- case()
  report <- file.path(context$root, paste0("report_", format(Sys.Date(), "%Y%m%d"), "_flagtest_result.md"))
  check(file.copy(context$report, report), "Authored report prepared at the visible project root")
  before <- file_state(report); data_before <- protected(context)
  cli("metaskill_runner", c("--project", context$root, "--dataset", "study", "--meta", "flagtest", "--intent", "result", "--phase", "finalization", "--synopsis", "Explicit lifecycle test", "--log", "FALSE"))
  run <- list.dirs(file.path(context$root, ".nlss/utility-runs"), recursive = FALSE)
  check(length(run) == 1L && json(file.path(run, "result.json"))$status == "completed", "Explicit finalization uses the common utility destination")
  check(identical(before, file_state(report)) && sha(report) == sha(file.path(run, "semantic-report.md")), "Finalization preserves exact authored bytes without moving or rewriting the visible document")
  check(identical(data_before, protected(context)), "Finalization does not modify statistical evidence or working/original data")
  check(grepl("Explicit lifecycle test", text(file.path(context$root, "report_canonical.md")), fixed = TRUE), "Requested synopsis reaches the root protocol")
  check(!dir.exists(file.path(context$root, ".nlss/.analysis-lock")), "Finalization releases its acquired input lock")
})

test("metaskill_flags_standalone_source_is_not_project_adoption", {
  original <- sha(golden)
  cli("metaskill_runner", c("--csv", golden, "--meta", "standalone-source"))
  output <- file.path(work, "unused-output")
  check(!file.exists(file.path(output, "nlss-workspace.yml")), "Ordinary standalone source invocation does not create a project marker")
  records <- list.files(output, "result.json", recursive = TRUE, full.names = TRUE)
  check(length(records) == 1L && json(records)$status == "completed" && json(records)$kind == "utility", "Existing standalone source mode still publishes utility evidence")
  check(identical(original, sha(golden)), "Standalone import leaves the bundled source unchanged")
})

test("metaskill_flags_project_does_not_replace_explicit_source", {
  context <- case(); before <- protected(context); original <- sha(golden)
  cli("metaskill_runner", c("--project", context$root, "--csv", golden, "--meta", "explicit-source", "--log", "FALSE"))
  runs <- list.dirs(file.path(context$root, ".nlss/utility-runs"), recursive = FALSE)
  check(length(runs) == 1L && json(file.path(runs, "result.json"))$status == "completed", "Explicit source plus project routes a completed utility to .nlss")
  request <- json(file.path(runs, "request.json"))
  id <- yaml::read_yaml(file.path(context$root, "nlss-workspace.yml"))$datasets[[1]]$id
  check(request$request$dataset$dataset_id != id, "Explicit CSV is not silently replaced by the active registered working dataset")
  check(identical(before, protected(context)) && identical(original, sha(golden)), "Explicit source mode preserves the registered data and original CSV")
})

passed <- sum(vapply(results, function(result) result$passed, logical(1)))
stable <- identical(initial_sources, hash_sources())
status <- if (length(results) && passed == length(results) && stable) 0L else 1L
save_results(status, utc())
cat(sprintf("Phase 3 reports: %d/%d grouped cases passed. Results: %s\n", passed, length(results), file.path(work, "results.json")))
if (!length(results)) cat("No cases matched --match; this is not a passing run.\n")
if (!stable) cat("Source identity changed during execution; this run does not certify a stable revision.\n")
if (!nzchar(forced) && keep > 0L) {
  candidates <- sort(list.dirs(collection, recursive = FALSE, full.names = TRUE), decreasing = TRUE)
  candidates <- candidates[grepl("^run-[0-9]{14}-[0-9]+$", basename(candidates))]
  if (length(candidates) > keep) for (candidate in candidates[seq.int(keep + 1L, length(candidates))]) {
    if (identical(candidate, work) || nzchar(Sys.readlink(candidate))) next
    previous <- tryCatch(json(file.path(candidate, "results.json")), error = function(e) NULL)
    if (identical(previous$owner, "nlss-report-test-runner") && identical(previous$suite, "phase3-reports") &&
        !is.null(previous$finished_at) && !is.null(previous$exit_status)) unlink(candidate, recursive = TRUE)
  }
}
quit(status = status)
