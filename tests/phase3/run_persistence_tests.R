#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# One bounded Parquet -> managed project -> run/change/replay acceptance path.
# Public CLIs exercise normal use; local environments inject publication faults.

args <- commandArgs(TRUE)
script <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))[1], winslash = "/", mustWork = TRUE)
repo <- normalizePath(file.path(dirname(script), "../.."), winslash = "/", mustWork = TRUE)
if (identical(args, "--help")) {
  cat("Usage: run_persistence_tests.R [--root PATH] [--keep N] [--match REGEX]\n",
      "Honors NLSS_TEST_ROOT, NLSS_KEEP_RUNS and NLSS_TESTS_CONFIG. Forced roots are never pruned.\n", sep = "")
  quit(status = 0L)
}
if (length(args) %% 2L || (length(args) && any(!args[seq.int(1L, length(args), 2L)] %in% c("--root", "--keep", "--match")))) stop("Unknown or incomplete runner option. Use --help.")
option <- function(name, fallback) {
  index <- which(args == name)
  if (length(index) > 1L) stop("Repeated runner option: ", name)
  if (length(index)) args[index + 1L] else fallback
}
required <- c("yaml", "jsonlite", "digest", "arrow", "haven")
for (package in required) if (!requireNamespace(package, quietly = TRUE)) stop("Missing test package: ", package)
absolute <- function(path) normalizePath(if (grepl("^(/|[A-Za-z]:|\\\\)", path)) path else file.path(repo, path), winslash = "/", mustWork = FALSE)
config_path <- absolute(Sys.getenv("NLSS_TESTS_CONFIG", "tests/tests.yml"))
settings <- yaml::read_yaml(config_path, eval.expr = FALSE)$tests
golden <- absolute(settings$golden_dataset)
note_fixture <- absolute(settings$phase3$study_document)
if (!file.exists(golden) || !file.exists(note_fixture)) stop("Missing registered Phase 3 fixture.")
forced <- option("--root", Sys.getenv("NLSS_TEST_ROOT", ""))
keep_text <- option("--keep", Sys.getenv("NLSS_KEEP_RUNS", as.character(settings$keep_runs_default)))
if (!grepl("^[0-9]+$", keep_text)) stop("Invalid --keep/NLSS_KEEP_RUNS.")
keep <- suppressWarnings(as.integer(keep_text))
if (is.na(keep)) stop("Out-of-range --keep/NLSS_KEEP_RUNS.")
pattern <- option("--match", ".*")
invisible(grepl(pattern, "validate regex"))
collection <- file.path(if (nzchar(forced)) absolute(forced) else absolute(settings$output_dir), "phase3-persistence")
work <- file.path(collection, paste0("run-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid()))
if (file.exists(work) || !dir.create(work, recursive = TRUE)) stop("Cannot create fresh test directory.")
work <- normalizePath(work, winslash = "/", mustWork = TRUE)
sha <- function(path) digest::digest(file = path, algo = "sha256")
utc <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
text <- function(path) if (file.exists(path)) paste(readLines(path, warn = FALSE), collapse = "\n") else ""
json <- function(path) jsonlite::read_json(path, simplifyVector = FALSE)
write_json <- function(value, path) jsonlite::write_json(value, path, auto_unbox = TRUE, null = "null", digits = NA, pretty = TRUE)
write_text <- function(path, value) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(value, path, useBytes = TRUE)
}
source_paths <- sort(unique(c(script, config_path, golden, note_fixture, file.path(repo, "tests/values/descriptive_stats_golden.csv"),
  file.path(repo, "tests/values/descriptive_stats_compute_golden.R"), file.path(repo, "scripts/config.yml"),
  list.files(file.path(repo, "scripts/R"), "[.]R$", recursive = TRUE, full.names = TRUE))))
hash_sources <- function() setNames(lapply(source_paths, sha), vapply(source_paths, function(path) {
  if (startsWith(path, paste0(repo, "/"))) substring(path, nchar(repo) + 2L) else path
}, character(1)))
source_initial <- hash_sources()
started <- utc()
results <- commands <- assertions <- list()
case_name <- "setup"
save_results <- function(status = NULL, finished = NULL) {
  write_json(list(schema_version = 1L, suite = "phase3-persistence", owner = "nlss-persistence-test-runner",
    command = c(file.path(R.home("bin"), "Rscript"), script, args), match = pattern,
    started_at = started, finished_at = finished, exit_status = status,
    source_sha256 = source_initial, source_unchanged = identical(source_initial, hash_sources()),
    environment = list(r = R.version.string, platform = R.version$platform,
      packages = as.list(vapply(required, function(p) as.character(utils::packageVersion(p)), character(1)))),
    tests = results, commands = commands), file.path(work, "results.json"))
}
check <- function(ok, label) {
  passed <- isTRUE(ok)
  assertions[[length(assertions) + 1L]] <<- list(assertion = label, passed = passed)
  if (!passed) stop(label, call. = FALSE)
  invisible(TRUE)
}
skip <- function(label) assertions[[length(assertions) + 1L]] <<- list(assertion = label, skipped = TRUE)
near <- function(actual, expected, label) {
  check(isTRUE(all.equal(as.numeric(actual), as.numeric(expected), tolerance = 1e-10)), label)
}
expect_error <- function(code, pattern = NULL) {
  problem <- tryCatch({ force(code); NULL }, error = conditionMessage)
  check(!is.null(problem), "Rejected operation reports an error")
  if (!is.null(pattern)) check(grepl(pattern, problem, ignore.case = TRUE), paste("Error identifies", pattern))
  invisible(problem)
}
private_config <- file.path(work, "config.yml")
reset_config <- function() yaml::write_yaml(list(defaults = list(output_dir = file.path(work, "standalone-output"), digits = 12L)), private_config)
reset_config()
Sys.setenv(NLSS_CONFIG_PATH = private_config, OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1")
Sys.unsetenv("NLSS_REPLAY_REQUEST")
runtime <- function() {
  e <- new.env(parent = globalenv())
  source(file.path(repo, "scripts/R/lib/bootstrap.R"), local = e)
  e$nlss_bootstrap(e)
  e$source_lib("project_store.R")
  e$source_lib("data_change.R")
  e
}
file_state <- function(path) {
  link <- Sys.readlink(path)
  if (!is.na(link) && nzchar(link)) return(list(kind = "symlink", link = link))
  if (!file.exists(path)) return(list(kind = "missing"))
  info <- file.info(path)
  if (isTRUE(info$isdir)) return(list(kind = "directory"))
  regular <- if (.Platform$OS.type == "unix") {
    executable <- Sys.which("test")
    if (!nzchar(executable)) stop("POSIX file inventory requires the standard test executable.")
    system2(executable, c("-f", shQuote(path)), stdout = FALSE, stderr = FALSE) == 0L
  } else isTRUE(file_test("-f", path))
  if (!regular) return(list(kind = "special"))
  list(kind = "file", sha256 = sha(path), mtime = as.numeric(info$mtime), mode = as.character(info$mode))
}
tree <- function(root) {
  entries <- list()
  visit <- function(path, name) {
    state <- file_state(path)
    entries[[name]] <<- state
    if (identical(state$kind, "directory")) for (child in sort(list.files(path, all.files = TRUE, no.. = TRUE))) {
      visit(file.path(path, child), if (name == ".") child else paste(name, child, sep = "/"))
    }
  }
  visit(root, ".")
  entries
}
unchanged <- function(before, root, label) check(identical(before, tree(root)), label)
run_cli <- function(module, argv, cwd = work, failure = FALSE, timeout = 90L) {
  old <- getwd(); on.exit(setwd(old), add = TRUE); setwd(cwd)
  stdout <- tempfile("stdout-", work); stderr <- tempfile("stderr-", work)
  on.exit(unlink(c(stdout, stderr)), add = TRUE)
  command <- c(file.path(repo, "scripts/R", paste0(module, ".R")), argv)
  began <- utc()
  status <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), shQuote(command), stdout = stdout, stderr = stderr, timeout = timeout))
  output <- text(stdout); errors <- text(stderr)
  record <- list(test = case_name, command = c(file.path(R.home("bin"), "Rscript"), command), cwd = cwd,
    started_at = began, finished_at = utc(), exit_status = as.integer(status),
    stdout = substr(output, 1L, 16000L), stderr = substr(errors, 1L, 16000L),
    stdout_truncated = nchar(output) > 16000L, stderr_truncated = nchar(errors) > 16000L)
  commands[[length(commands) + 1L]] <<- record
  save_results()
  check(if (failure) status != 0L && status != 124L else status == 0L,
    paste(module, "expected exit outcome:", status, record$stderr, record$stdout))
  record
}
test <- function(name, code) {
  if (!grepl(pattern, name)) return(invisible(NULL))
  case_name <<- name; assertions <<- list(); reset_config()
  old <- getwd(); on.exit(setwd(old), add = TRUE)
  began <- utc(); elapsed <- proc.time()[["elapsed"]]
  problem <- tryCatch({ force(code); NULL }, error = conditionMessage)
  results[[length(results) + 1L]] <<- list(test = name, passed = is.null(problem),
    started_at = began, finished_at = utc(), seconds = unname(proc.time()[["elapsed"]] - elapsed),
    message = if (is.null(problem)) "OK" else problem, assertions = assertions)
  save_results()
  cat(if (is.null(problem)) "[PASS] " else "[FAIL] ", name,
    if (is.null(problem)) "" else paste0(": ", problem), "\n", sep = "")
}
new_case <- function(suffix = "project", create = TRUE) {
  root <- file.path(work, "cases", case_name, suffix)
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  data <- utils::read.csv(golden, stringsAsFactors = FALSE)
  n <- nrow(data)
  data$leading_id <- sprintf("%05d", seq_len(n))
  data$labelled_scale <- haven::labelled_spss(rep(c(1, 2, 99, 4), length.out = n),
    labels = c("Niedrig" = 1, "Hoch" = 4), na_values = 99, label = "Skala – ä")
  data$visit_date <- as.Date("2024-02-01") + seq_len(n)
  data$visit_time <- as.difftime(seq_len(n) + 0.09, units = "secs")
  data$visit_timestamp <- as.POSIXct("2024-02-01 12:00:00", tz = "UTC") + seq_len(n) + 0.125
  e <- runtime()
  source <- file.path(root, "Original.parquet")
  e$write_parquet_data(data, source)
  check(file.copy(note_fixture, file.path(root, "research_note.md")), "Private research note fixture copied")
  context <- list(root = root, source = source, working = file.path(root, "Working data/current.parquet"),
    note = file.path(root, "research_note.md"), data = data, source_hash = sha(source), env = e)
  if (create) create_project(context)
  context
}
create_project <- function(context, argv = character()) {
  run_cli("project_create", c("--project", context$root, "--source", "Original.parquet", "--working", "Working data/current.parquet", "--name", "study", argv), context$root)
  check(identical(sha(context$source), context$source_hash), "Creation preserves original bytes")
  check(file.exists(context$working), "Creation supplies the selected visible working file")
  invisible(context)
}
descriptors <- function(context) list.files(file.path(context$root, ".nlss/datasets"), "^dataset[.]json$", recursive = TRUE, full.names = TRUE)
descriptor <- function(context) {
  paths <- descriptors(context)
  check(length(paths) == 1L, "Project has one logical dataset descriptor")
  json(paths)
}
objects <- function(context) tree(file.path(context$root, ".nlss/objects"))
versions <- function(context) {
  paths <- list.files(file.path(context$root, ".nlss/datasets"), "[.]json$", recursive = TRUE, full.names = TRUE)
  paths <- paths[grepl("/versions/", paths, fixed = TRUE)]
  setNames(lapply(paths, sha), substring(paths, nchar(context$root) + 2L))
}
runs <- function(context) {
  parent <- file.path(context$root, ".nlss/runs")
  paths <- list.dirs(parent, recursive = FALSE, full.names = TRUE)
  paths[!startsWith(basename(paths), ".") & file.exists(file.path(paths, "request.json"))]
}
no_projections <- function(context) {
  paths <- list.files(context$root, all.files = TRUE, recursive = TRUE)
  check(!any(basename(paths) %in% c("analysis_log.jsonl", "scratchpad.md", "import.json")), "Managed path creates no parallel log/scratchpad/import projections")
  check(all(paths[basename(paths) == "report_canonical.md"] == "report_canonical.md"), "Only the default root protocol is maintained")
  check(!any(grepl("(^|/)backup(/|$)", paths)), "Managed path creates no duplicate legacy backups")
}
verify_ref <- function(context, reference) {
  check(is.list(reference) && nzchar(reference$dataset_id) && nzchar(reference$version_id), "Version reference has stable identities")
  for (kind in c("snapshot", "dictionary")) {
    path <- reference[[paste0(kind, "_path")]]
    hash <- reference[[if (kind == "snapshot") "data_sha256" else "dictionary_sha256"]]
    check(identical(path, paste0(".nlss/objects/", hash)), paste(kind, "uses its content-addressed object"))
    check(identical(sha(file.path(context$root, path)), hash), paste(kind, "object bytes match hash"))
  }
  context$env$read_parquet_data(file.path(context$root, reference$snapshot_path))
}
analysis <- function(context, module = "descriptive_stats", argv = c("--vars", "x1,labelled_scale"), select = TRUE) {
  previous <- runs(context)
  run_cli(module, c(if (module != "replay_run") c("--project", context$root, if (select) c("--dataset", "study")),
    argv), context$root)
  added <- setdiff(runs(context), previous)
  check(length(added) == 1L, "One execution publishes exactly one authoritative run")
  request <- json(file.path(added, "request.json")); result <- json(file.path(added, "result.json"))
  check(isTRUE(request$resolved) && identical(result$status, "completed"), "Run is resolved and completed")
  check(identical(request$run_id, basename(added)) && identical(request$run_id, result$run_id), "Request/result/directory share run identity")
  check(identical(request$dataset, result$dataset), "Request and result bind the same input version")
  check(identical(result$artifacts$request$sha256, sha(file.path(added, "request.json"))), "Result authenticates its request")
  for (artifact in result$artifacts) check(identical(sha(file.path(added, artifact$path)), artifact$sha256), paste("Artifact hash:", artifact$path))
  verify_ref(context, request$dataset)
  check(file.exists(file.path(added, "output.md")), "Per-run statistical Markdown exists")
  no_projections(context)
  list(path = added, request = request, result = result)
}
save_results()

for (source_format in c("csv", "sav", "rds", "rdata", "parquet")) local({
  format <- source_format
  test(paste0("startup_raw_folder_", format), {
    root <- file.path(work, "cases", case_name, "Study folder ä")
    dir.create(root, recursive = TRUE)
    e <- runtime()
    raw <- data.frame(id = c("001", "002", "003", "004"), score = c(1.5, 2.5, 99, 4.5))
    if (format != "csv") raw$score <- haven::labelled_spss(raw$score,
      labels = c(Low = 1.5, High = 4.5), na_values = 99, label = "Score – ä")
    original <- file.path(root, paste0("Survey.", format))
    argv <- character()
    if (format == "csv") {
      write_text(original, c("id;score", "001;1,5", "002;2,5", "003;99", "004;4,5"))
      # Effective site defaults must be recorded just like explicit flags.
      yaml::write_yaml(list(defaults = list(output_dir = file.path(work, "standalone-output"),
        csv = list(sep = ";", decimal = ",", na_values = "NA,99"))), private_config)
    }
    if (format == "sav") haven::write_sav(raw, original)
    if (format == "rds") saveRDS(raw, original)
    if (format == "rdata") { survey <- raw; distractor <- data.frame(score = 999); save(survey, distractor, file = original); argv <- c("--df", "survey") }
    if (format == "parquet") e$write_parquet_data(raw, original)
    received <- file_state(original)
    setup <- run_cli("project_create", c("--project", root, "--source", basename(original), argv), work)
    reply <- jsonlite::fromJSON(setup$stdout, simplifyVector = FALSE)
    check(identical(reply$status, "created"), "One ordinary raw-source command initializes the project and returns JSON")
    check(identical(reply$source$sha256, received$sha256) && identical(reply$source$import$format, format), "Registration binds actual received bytes and reader format")
    check(identical(reply$source$selected_path, basename(original)), "Source path is portable, not a prepared intermediate")
    check(!reply$protocol$available, "Setup does not pretend a statistical protocol exists before analysis")
    working <- file.path(root, reply$dataset$working)
    df <- e$read_parquet_data(working)
    near(df$score, c(1.5, 2.5, NA, 4.5), "User-missing codes/CSV tokens normalize to ordinary NA")
    check(identical(df$id, raw$id), "Leading-zero identifiers remain text")
    if (format != "csv") {
      contract <- attr(df, "nlss_import_contract")$columns$score
      check(identical(contract$variable_label, "Score – ä") && length(contract$value_labels) == 2L, "Variable and value labels survive direct initiation")
      check(length(contract$missing$observations) == 1L, "Source-row missing provenance survives initiation")
    } else {
      options <- reply$source$import$options
      check(identical(options$sep, ";") && identical(options$`csv-decimal`, ",") && identical(options$`csv-na-values`, "NA,99"), "Resolved CSV defaults are in the source binding")
    }
    check(identical(file_state(original), received), "Received data bytes, permissions and modification time are untouched")
    context <- list(root = root, working = working, env = e)
    no_projections(context)
    check(length(list.files(file.path(root, ".nlss/staging"), all.files = TRUE, no.. = TRUE)) == 0L, "Successful setup leaves no intermediate files")
    run_cli("descriptive_stats", c("--project", root, "--dataset", reply$dataset$name, "--vars", "score"), work)
    result <- json(file.path(runs(context)[1], "result.json"))
    near(result$results$summary_df[[1]]$mean, mean(c(1.5, 2.5, 4.5)), "Unchanged statistical module consumes the registered raw import correctly")
    check(identical(result$dataset$source_sha256, received$sha256), "Analysis evidence refers back to actual raw source bytes")
    check(file.exists(file.path(root, "report_canonical.md")), "First analysis automatically supplies the root protocol")
    df$score[1] <- 11.5; e$write_parquet_data(df, working)
    before <- tree(root)
    reused <- run_cli("project_create", c("--project", root, "--source", basename(original), argv), work)
    check(identical(jsonlite::fromJSON(reused$stdout)$status, "reused"), "Repeated matching setup returns reused")
    run_cli("project_create", c("--project", root), work)
    unchanged(before, root, "Both reuse modes preserve working edits and add no files, versions or logs")
    # No exact-replay exemption: the ordinary existing consumer must accept raw-source evidence.
    run_cli("replay_run", c("--request", file.path(runs(context)[1], "request.json")), work)
    near(e$read_parquet_data(working)$score[1], 11.5, "Replay of the imported input never activates over later edits")
  })
})

test("startup_add_dataset_reuse_and_conflicts", {
  context <- new_case()
  original <- tree(context$root)
  write_text(file.path(context$root, "Wave2.csv"), c("x1", "10", "20", "30"))
  setup <- run_cli("project_create", c("--project", context$root, "--source", "Wave2.csv"), work)
  reply <- jsonlite::fromJSON(setup$stdout, simplifyVector = FALSE)
  check(identical(reply$status, "dataset_added") && identical(reply$dataset$name, "Wave2"), "Same entrypoint registers a new dataset")
  manifest <- yaml::read_yaml(file.path(context$root, "nlss-workspace.yml"))
  check(length(manifest$datasets) == 2L && identical(manifest$active_dataset, "study"), "Adding data does not silently change the active dataset")
  check(all(vapply(names(original)[vapply(original, function(x) identical(x$kind, "file"), logical(1)) &
      names(original) != "nlss-workspace.yml"], function(p) identical(original[[p]], file_state(file.path(context$root, p))), logical(1))), "Existing data, evidence and user documents are unchanged")
  run_cli("descriptive_stats", c("--project", context$root, "--dataset", "Wave2", "--vars", "x1"), work)
  added <- tail(runs(context), 1L)
  near(json(file.path(added, "result.json"))$results$summary_df[[1]]$mean, 20, "Returned dataset selection loads the added data")
  before <- tree(context$root)
  run_cli("project_create", c("--project", context$root, "--source", "Wave2.csv", "--name", "study"), failure = TRUE)
  run_cli("project_create", c("--project", context$root, "--source", "Wave2.csv", "--sep", ";"), failure = TRUE)
  run_cli("project_create", c("--project", context$root, "--name", "Wave2", "--working", "elsewhere.parquet"), failure = TRUE)
  unchanged(before, context$root, "Name/options/location conflicts do not mutate the project")
  direct <- context$env$nlss_create_project(context$root, name = "Wave2")
  check(identical(direct$status, "reused") && identical(direct$dataset$id, reply$dataset$id), "Direct R adapter boundary returns the same structured registration as the CLI")
  unchanged(before, context$root, "Direct adapter reuse is also write-free")
  write_text(file.path(context$root, "Wave2.csv"), c("x1", "100", "200"))
  before <- tree(context$root)
  failed <- run_cli("project_create", c("--project", context$root, "--source", "Wave2.csv"), failure = TRUE)
  check(identical(jsonlite::fromJSON(failed$stdout)$status, "error"), "Failed CLI emits structured error with nonzero exit")
  unchanged(before, context$root, "Changed source does not reset earlier working data")
  # A distinct deliberate registration can preserve both interpretations/versions.
  run_cli("project_create", c("--project", context$root, "--source", "Wave2.csv", "--name", "Wave2-revised"), work)
  before <- tree(context$root)
  run_cli("project_create", c("--project", context$root, "--source", "Wave2.csv"), failure = TRUE)
  unchanged(before, context$root, "Multiple registrations require an explicit name, not a guessed latest version")
  moved <- paste0(context$root, " moved")
  check(file.rename(context$root, moved), "Whole project relocated")
  before <- tree(moved)
  run_cli("project_create", c("--project", moved, "--source", "Wave2.csv", "--name", "Wave2-revised"), work)
  unchanged(before, moved, "Source identity and repeatability survive whole-project relocation")
})

test("startup_invalid_import_and_locks", {
  for (kind in c("rdata_selection", "not_dataframe", "csv_conversion", "wrong_options")) {
    root <- file.path(work, "cases", case_name, kind); dir.create(root, recursive = TRUE)
    if (kind == "rdata_selection") { a <- b <- data.frame(x = 1); save(a, b, file = file.path(root, "input.RData")); argv <- c("--source", "input.RData") }
    if (kind == "not_dataframe") { saveRDS(1:3, file.path(root, "input.rds")); argv <- c("--source", "input.rds") }
    if (kind == "csv_conversion") { write_text(file.path(root, "input.csv"), c("x", "not_numeric")); argv <- c("--source", "input.csv", "--csv-col-types", "x=numeric") }
    if (kind == "wrong_options") { saveRDS(data.frame(x = 1), file.path(root, "input.rds")); argv <- c("--source", "input.rds", "--sep", ";") }
    before <- tree(root)
    run_cli("project_create", c("--project", root, argv), failure = TRUE)
    unchanged(before, root, "Invalid import fails before allocating project infrastructure")
  }
  context <- new_case()
  write_text(file.path(context$root, "extra.csv"), c("x", "1"))
  for (lock in c(".analysis-lock", ".publication-lock")) {
    path <- file.path(context$root, ".nlss", lock); dir.create(path)
    before <- tree(context$root)
    run_cli("project_create", c("--project", context$root, "--source", "extra.csv"), failure = TRUE)
    check(dir.exists(path), "Contending writer lock remains owned by its original operation")
    check(!dir.exists(file.path(context$root, "data/extra_working.parquet")) && length(descriptors(context)) == 1L, "Contention does not register or replace working data")
    # Lock acquisition/removal may change directory timestamps, but no persistent files.
    files <- names(before)[vapply(before, function(x) identical(x$kind, "file"), logical(1))]
    check(all(vapply(files, function(p) identical(before[[p]], file_state(file.path(context$root, p))), logical(1))), "Contention preserves all existing file bytes and mtimes")
    unlink(path, recursive = TRUE)
  }
  run_cli("project_create", c("--project", context$root, "--source", "extra.csv"))
  check(!dir.exists(file.path(context$root, ".nlss/.analysis-lock")) && !dir.exists(file.path(context$root, ".nlss/.publication-lock")), "Successful registration releases only its acquired locks")
})

test("task_ab_regression_planning_utility_and_default_protocol", {
  context <- new_case()
  before_source <- sha(context$source); before_working <- sha(context$working)
  before_marker <- sha(file.path(context$root, "nlss-workspace.yml"))
  deep <- file.path(context$root, "notes/deep/analysis")
  dir.create(deep, recursive = TRUE)
  setwd(deep)
  before_resolution <- tree(context$root)
  check(identical(context$env$get_default_out(), context$root), "Existing root getter resolves the nearest project")
  check(identical(context$env$find_workspace_manifest(), file.path(context$root, "nlss-workspace.yml")) &&
    identical(context$env$resolve_workspace_root(), context$root), "Existing manifest/root APIs consume current discovery")
  check(identical(context$env$get_workspace_out_dir(), file.path(context$root, ".nlss")), "Existing output getter returns the independent evidence root")
  unchanged(before_resolution, context$root, "Activated location APIs do not write files or load/copy working data")
  run_cli("regression", c("--dv", "outcome_reg", "--ivs", "x1,x2"), deep)
  regression <- json(file.path(runs(context)[1], "result.json"))
  check(identical(regression$status, "completed"), "Regression runs without project or dataset opt-in from a deep cwd")
  coefficients <- regression$results$coefficients_df
  expected <- stats::coef(stats::lm(outcome_reg ~ x1 + x2, data = context$data))
  near(vapply(coefficients, function(row) row$estimate, numeric(1)), unname(expected), "Regression coefficients equal independent lm")
  run_cli("power", c("--project", context$root, "--analysis", "ttest", "--effect-size", "0.5", "--power", "0.8"), work)
  all_results <- lapply(runs(context), function(p) json(file.path(p, "result.json")))
  power <- Filter(function(x) identical(x$module, "power"), all_results)[[1]]
  check(identical(power$input$kind, "parameters") && is.null(power$dataset), "Power retains parameter inputs without selecting a dataset")
  expected_n <- ceiling(pwr::pwr.t.test(d = .5, power = .8, sig.level = .05, type = "two.sample", alternative = "two.sided")$n)
  near(power$results$summary_df[[1]]$n_total, 2 * expected_n, "Parameter-only Power sample size matches independent pwr")
  near(power$results$summary_df[[1]]$attained_power,
    pwr::pwr.t.test(n = expected_n, d = .5, sig.level = .05, type = "two.sample", alternative = "two.sided")$power,
    "Attained power agrees with independent calculation at integer sample sizes")
  check(!dir.exists(file.path(context$root, "planning")) && !dir.exists(file.path(context$root, ".nlss/planning")), "No planning folder is invented for current-layout runs")
  run_cli("calc", c("--project", context$root, "--expr", "1+2"), work)
  utility <- list.dirs(file.path(context$root, ".nlss/utility-runs"), recursive = FALSE, full.names = TRUE)
  check(length(utility) == 1L, "Calc publishes one utility bundle in .nlss")
  calc <- json(file.path(utility, "result.json"))
  check(identical(calc$kind, "utility") && identical(calc$status, "completed"), "Utility keeps its own payload kind/status")
  protocol <- text(file.path(context$root, "report_canonical.md"))
  check(length(gregexpr("<!-- nlss-run:", protocol, fixed = TRUE)[[1]]) == 3L, "Three executions automatically produce three protocol entries")
  check(all(vapply(c(".nlss/runs/", ".nlss/utility-runs/", "regression", "power", "calc"), grepl, logical(1), x = protocol, fixed = TRUE)), "Root protocol links both kinds of evidence")
  check(identical(sha(context$source), before_source) && identical(sha(context$working), before_working) &&
    identical(sha(file.path(context$root, "nlss-workspace.yml")), before_marker), "Analyses, planning and utilities leave source/working data and marker unchanged")
  transformation <- analysis(context, "data_transform", c("--calc", "x1_plus=x1+10"))
  near(context$env$read_parquet_data(context$working)$x1_plus, context$data$x1 + 10, "Successive data change prepares the visible target")
  all_results <- lapply(runs(context), function(p) json(file.path(p, "result.json")))
  protocol <- text(file.path(context$root, "report_canonical.md"))
  for (result in c(all_results, list(calc))) {
    check(lengths(regmatches(protocol, gregexpr(paste0("Run: `", result$run_id, "`"), protocol, fixed = TRUE))) == 1L,
      "Every successive analysis/utility/data change occurs exactly once")
    check(grepl(paste0("## ", result$module, " — ", result$status), protocol, fixed = TRUE), "Protocol status agrees with the saved result")
  }
  paths <- list.files(context$root, recursive = TRUE, all.files = TRUE)
  check(sum(basename(paths) == "report_canonical.md") == 1L && !any(grepl("protocol.*[.]md$", paths)),
    "No automatic protocol snapshot family exists")
  no_projections(context)
  original_runs <- runs(context)
  context$env$nlss_rebuild_project_protocol(context$root, "protocol-rebuilt.md")
  rebuilt <- text(file.path(context$root, "protocol-rebuilt.md"))
  check(length(gregexpr("<!-- nlss-run:", rebuilt, fixed = TRUE)[[1]]) == 4L && identical(original_runs, runs(context)), "Protocol reconstruction reads saved outputs without new statistical runs")
  for (result in c(all_results, list(calc))) check(grepl(result$run_id, rebuilt, fixed = TRUE), "Rebuilt protocol includes the recorded run")
  table_lines <- function(value) grep("^\\|", strsplit(value, "\n", fixed = TRUE)[[1]], value = TRUE)
  check(identical(table_lines(protocol), table_lines(rebuilt)), "Reconstructed numerical tables match the automatically maintained protocol")
})

test("task_ab_explicit_csv_and_dataset_selection", {
  context <- new_case()
  source_before <- sha(context$source); working_before <- sha(context$working)
  csv <- file.path(context$root, "additional.csv")
  write.csv(context$data[c("x1", "x2", "outcome_reg")], csv, row.names = FALSE)
  csv_before <- sha(csv)
  run_cli("regression", c("--project", context$root, "--csv", csv, "--dv", "outcome_reg", "--ivs", "x1,x2"), work)
  result <- json(file.path(runs(context)[1], "result.json"))
  check(identical(result$status, "completed") && !identical(result$dataset$origin, "managed_parquet"), "Explicit source import uses the same output route without changing storage identity")
  check(file.exists(file.path(context$root, "additional/additional.parquet")) && !dir.exists(file.path(context$root, ".nlss/additional")), "Explicit import retains visible working data outside .nlss")
  run_cli("frequencies", c("--project", context$root, "--dataset", "study", "--vars", "x1", "--include-numeric", "TRUE"), work)
  check(length(runs(context)) == 2L, "Another statistical module uses the general shared project selector")
  check(identical(sha(csv), csv_before) && identical(sha(context$source), source_before) && identical(sha(context$working), working_before), "All original and registered working files remain untouched")
  check(!any(basename(list.files(context$root, recursive = TRUE, all.files = TRUE)) == "analysis_log.jsonl"), "Explicit imports do not add a parallel analysis journal")
})

test("task_ab_protocol_collision_preserves_authored_file", {
  context <- new_case()
  protocol <- file.path(context$root, "report_canonical.md")
  writeLines("User-authored document, not an NLSS protocol.", protocol)
  before <- sha(protocol); working_before <- sha(context$working)
  result <- run_cli("regression", c("--project", context$root, "--dv", "outcome_reg", "--ivs", "x1,x2"), work, failure = TRUE)
  check(grepl("unrecognized/user-authored", result$stderr, fixed = TRUE), "Protocol collision is reported explicitly")
  check(identical(sha(protocol), before) && identical(sha(context$working), working_before), "Failed publication does not replace authored prose or working data")
  check(!length(runs(context)), "Failed protocol publication does not publish a success bundle")
  utility <- run_cli("calc", c("--project", context$root, "--expr", "1+2"), work, failure = TRUE)
  check(grepl("unrecognized/user-authored", utility$stderr, fixed = TRUE), "Utility protocol collision is also reported explicitly")
  check(identical(sha(protocol), before) && identical(sha(context$working), working_before), "Utility failure preserves authored prose and working data")
  pending <- list.dirs(file.path(context$root, ".nlss/utility-runs"), recursive = FALSE, full.names = TRUE)
  check(length(pending) == 1L && startsWith(basename(pending), ".pending-") &&
    identical(json(file.path(pending, "result.json"))$status, "failed"), "Utility failed publication remains diagnostic, not a completed execution")
})

for (selection in c("standalone", "project")) local({
  selected <- selection
  test(paste0("task_f_source_dataset_name_", selected), {
    context <- new_case(create = selected == "project")
    before <- sha(context$source)
    working_before <- if (selected == "project") sha(context$working) else NULL
    run_cli("descriptive_stats", c(if (selected == "project") c("--project", context$root),
      "--parquet", context$source, "--dataset-name", "distinct_import", "--vars", "x1"), work)
    directory <- if (selected == "project") file.path(context$root, ".nlss/runs") else
      file.path(work, "standalone-output/distinct_import/runs")
    bundles <- list.files(directory, "^request[.]json$", recursive = TRUE, full.names = TRUE)
    check(length(bundles) == 1L, "Explicit source name publishes one run without selecting a registered dataset")
    result <- json(file.path(dirname(bundles), "result.json"))
    near(result$results$summary_df[[1]]$mean, mean(context$data$x1, na.rm = TRUE), "Named import uses the explicit source values")
    check(identical(sha(context$source), before), "Named import preserves original source bytes")
    visible_root <- if (selected == "project") context$root else file.path(work, "standalone-output")
    check(file.exists(file.path(visible_root, "distinct_import/distinct_import.parquet")), "Named working data remain visible at the requested name")
    if (selected == "project") {
      check(identical(sha(context$working), working_before), "Named source does not replace the registered working data")
      check(!any(grepl("[.]jsonl$", list.files(context$root, recursive = TRUE, all.files = TRUE))), "Named project import adds no parallel journal")
    }
  })
})

for (format in c("csv", "rds", "sav")) local({
  selected <- format
  test(paste0("task_f_grouped_missing_counts_", selected), {
    context <- new_case()
    expected <- context$data[c("group2", "pre_score", "post_score")]
    expected$change <- expected$post_score - expected$pre_score
    input <- expected
    if (selected == "rds") input$group2 <- factor(input$group2, levels = c("control", "treatment", "unused"))
    if (selected == "sav") {
      input$group2 <- haven::labelled_spss(ifelse(is.na(input$group2), 99,
        ifelse(input$group2 == "treatment", 1, 2)),
        labels = c(treatment = 1, control = 2), na_values = 99, label = "Study group")
    }
    path <- file.path(context$root, paste0("grouped.", selected))
    switch(selected, csv = write.csv(input, path, row.names = FALSE),
      rds = saveRDS(input, path), sav = haven::write_sav(input, path))
    before <- sha(path)
    run_cli("descriptive_stats", c("--project", context$root, paste0("--", selected), path,
      "--vars", "pre_score,post_score,change", "--group", "group2"), work)
    result <- json(file.path(runs(context)[[1]], "result.json"))
    rows <- result$results$summary_df
    check(length(rows) == 9L, "Three observed groups including missing, without an unused factor level")
    for (group in c("treatment", "control", NA_character_)) {
      indices <- if (is.na(group)) which(is.na(expected$group2)) else which(expected$group2 == group)
      key <- if (is.na(group)) "NA" else if (selected == "sav") if (group == "treatment") "1" else "2" else group
      for (variable in c("pre_score", "post_score", "change")) {
        row <- Filter(function(x) identical(x$group, key) && identical(x$variable, variable), rows)
        check(length(row) == 1L, paste("Exactly one output row for", key, variable))
        row <- row[[1]]; values <- expected[[variable]][indices]
        reference <- list(total_n = length(indices), n = sum(!is.na(values)), missing_n = sum(is.na(values)),
          missing_pct = 100 * mean(is.na(values)), mean = mean(values, na.rm = TRUE), sd = sd(values, na.rm = TRUE))
        for (field in names(reference)) near(row[[field]], reference[[field]], paste(selected, key, variable, field, "matches independent observed rows"))
      }
    }
    for (variable in c("pre_score", "post_score", "change")) {
      total <- sum(vapply(Filter(function(x) identical(x$variable, variable), rows), function(x) x$total_n, numeric(1)))
      near(total, nrow(expected), "Every original observation belongs to exactly one group")
    }
    if (selected == "sav") {
      check(all(vapply(Filter(function(x) identical(x$group, "1"), rows), function(x) identical(x$group_label, "treatment"), logical(1))), "SPSS group value labels survive normalization")
    }
    check(identical(sha(path), before) && identical(sha(context$source), context$source_hash), "Original inputs remain untouched")
  })
})

test("task_f_descriptive_existing_numeric_goldens", {
  context <- new_case()
  references <- read.csv(file.path(repo, "tests/values/descriptive_stats_golden.csv"), stringsAsFactors = FALSE)
  for (grouped in c(FALSE, TRUE)) {
    cases <- references[nzchar(references$group_var) == grouped, , drop = FALSE]
    previous <- runs(context)
    run_cli("descriptive_stats", c("--project", context$root, "--dataset", "study",
      "--vars", paste(unique(cases$variable), collapse = ","),
      if (grouped) c("--group", "group2", "--trim", "0.2", "--iqr-multiplier", "2", "--outlier-z", "2.5")), work)
    rows <- json(file.path(setdiff(runs(context), previous), "result.json"))$results$summary_df
    for (i in seq_len(nrow(cases))) {
      reference <- cases[i, ]; key <- if (is.na(reference$group)) "" else reference$group
      row <- Filter(function(x) identical(x$variable, reference$variable) && identical(x$group, key), rows)
      check(length(row) == 1L, paste("Existing golden row", reference$case_id))
      for (field in names(reference)[vapply(reference, is.numeric, logical(1))]) {
        if (field %in% c("trim", "iqr_multiplier", "outlier_z")) next
        actual <- row[[1]][[field]]
        near(if (is.null(actual)) NA_real_ else actual, reference[[field]], paste(reference$case_id, field, "matches independently computed golden"))
      }
    }
  }
})

test("task_ab_unsupported_marker_and_parameter_selector_no_writes", {
  context <- new_case()
  before <- tree(context$root)
  run_cli("power", c("--project", context$root, "--dataset", "study", "--planning", "TRUE", "--effect-size", ".5"), work, failure = TRUE)
  unchanged(before, context$root, "Conflicting parameter/dataset selection fails before writes")
  writeLines("schema_version: 1", file.path(context$root, "nlss-workspace.yml"))
  before <- tree(context$root)
  result <- run_cli("regression", c("--project", context$root, "--dv", "outcome_reg", "--ivs", "x1,x2"), work, failure = TRUE)
  check(grepl("Unsupported project marker", result$stderr, fixed = TRUE), "Old project format is refused without conversion")
  unchanged(before, context$root, "Unsupported project is not modified or adopted")
  expect_error(context$env$write_workspace_manifest(list(schema_version = 1L), file.path(context$root, "new-marker.yml")), "old project formats")
  unchanged(before, context$root, "Old initializer cannot publish an unusable marker or report false success")
})

test("task_ab_all_existing_source_formats", {
  context <- new_case()
  measurement <- data.frame(score = c(1, 2, 3, NA_real_))
  sources <- file.path(context$root, paste0("sample_", c("csv", "sav", "rds", "rdata", "parquet"),
    c(".csv", ".sav", ".rds", ".RData", ".parquet")))
  names(sources) <- c("csv", "sav", "rds", "rdata", "parquet")
  write.csv(measurement, sources[["csv"]], row.names = FALSE)
  haven::write_sav(measurement, sources[["sav"]])
  saveRDS(measurement, sources[["rds"]])
  save(measurement, file = sources[["rdata"]])
  context$env$write_parquet_data(measurement, sources[["parquet"]])
  for (format in names(sources)) {
    original_hash <- sha(sources[[format]]); previous <- runs(context)
    run_cli("descriptive_stats", c("--project", context$root, paste0("--", format), sources[[format]],
      if (format == "rdata") c("--df", "measurement"), "--vars", "score"), work)
    added <- setdiff(runs(context), previous)
    check(length(added) == 1L, paste(format, "publishes through common project output"))
    result <- json(file.path(added, "result.json"))
    near(result$results$summary_df[[1]]$mean, 2, paste(format, "retains independently expected mean"))
    check(identical(sha(sources[[format]]), original_hash), paste(format, "source remains untouched"))
  }
  check(!any(basename(list.files(context$root, recursive = TRUE, all.files = TRUE)) == "analysis_log.jsonl"), "All data formats share one protocol, not parallel journals")
})

current_run <- function(context, module, argv, cwd = work) {
  previous <- runs(context)
  run_cli(module, c(if (module != "replay_run") c("--project", context$root), argv), cwd)
  added <- setdiff(runs(context), previous)
  check(length(added) == 1L, "One current-layout run is published")
  result <- json(file.path(added, "result.json"))
  check(identical(result$status, "completed"), "Published run completed")
  for (artifact in result$artifacts) check(identical(sha(file.path(added, artifact$path)), artifact$sha256), "Preserved run artifact hash matches")
  list(path = added, request = json(file.path(added, "request.json")), result = result)
}

test("task_c_plot_artifacts_links_and_replay_after_move", {
  context <- new_case()
  before <- file_state(context$working)
  argv <- c("--type", "scatter", "--x", "x1", "--y", "outcome_reg", "--smooth", "lm", "--se", "TRUE")
  original <- current_run(context, "plot", argv)
  plot_data <- readRDS(file.path(original$path, "plot-data.rds"))
  d <- context$data[complete.cases(context$data[c("x1", "outcome_reg")]), ]
  expected <- ggplot2::ggplot(d, ggplot2::aes(x = x1, y = outcome_reg)) + ggplot2::geom_point() + ggplot2::geom_smooth(method = "lm", se = TRUE)
  layers <- suppressMessages(ggplot2::ggplot_build(expected)$data)
  for (i in seq_along(layers)) for (key in intersect(c("x", "y", "ymin", "ymax", "se"), names(layers[[i]]))) {
    near(plot_data[[1]]$layers[[i]][[key]], layers[[i]][[key]], paste("Plot layer matches ggplot2:", i, key))
  }
  check(!dir.exists(file.path(context$root, ".nlss/plots")), "No duplicate project-level figure copies")
  check(identical(file_state(context$working), before), "Plot leaves the visible working dataset unchanged")
  images <- list.files(original$path, "[.]png$", recursive = TRUE)
  check(length(images) > 0L, "Plot preserves a run-local image")
  relative <- substring(original$path, nchar(context$root) + 2L)
  for (path in images) {
    check(grepl(paste0("](", path, ")"), text(file.path(original$path, "output.md")), fixed = TRUE), "Run output links its own image")
    check(grepl(paste0("](", relative, "/", path, ")"), text(file.path(context$root, "report_canonical.md")), fixed = TRUE), "Root protocol links the same preserved image")
  }
  old_run <- tree(original$path)
  destination <- paste0(context$root, " moved ä")
  check(file.rename(context$root, destination), "Whole project moved")
  context$root <- destination; context$working <- file.path(destination, "Working data/current.parquet")
  original$path <- file.path(destination, relative)
  replay <- current_run(context, "replay_run", c("--request", file.path(original$path, "request.json")))
  check(identical(replay$request$replay_of, original$request$run_id), "Plot replay retains original identity after relocation")
  check(isTRUE(all.equal(readRDS(file.path(replay$path, "plot-data.rds")), plot_data)), "Plot replay reproduces numerical evidence")
  unchanged(old_run, original$path, "Replay does not replace earlier image/data artifacts")
  check(identical(file_state(context$working), before), "Replay does not activate working data")
  context$env$nlss_rebuild_project_protocol(context$root, "rebuilt.md")
  for (path in images) check(grepl(paste0("](", relative, "/", path, ")"), text(file.path(context$root, "rebuilt.md")), fixed = TRUE), "Rebuilt relocated protocol retains figure links")
})

test("task_c_regression_and_parameter_replay_from_other_cwd", {
  context <- new_case()
  regression <- current_run(context, "regression", c("--dv", "outcome_reg", "--ivs", "x1,x2"))
  power <- current_run(context, "power", c("--analysis", "ttest", "--effect-size", ".5", "--power", ".8"))
  other <- new_case("other-project")
  before_other <- tree(other$root); working <- file_state(context$working)
  for (original in list(regression, power)) {
    replay <- current_run(context, "replay_run", c("--request", file.path(original$path, "request.json")), cwd = other$root)
    check(identical(replay$request$replay_of, original$request$run_id), "Replay uses selected evidence, not invocation project's dataset")
    check(identical(replay$result$results, original$result$results), "Replayed regression/Power results match saved values")
    check(identical(text(file.path(replay$path, "output.md")), text(file.path(original$path, "output.md"))), "Replayed deterministic Markdown matches")
  }
  unchanged(before_other, other$root, "Unrelated current directory project is untouched")
  check(identical(file_state(context$working), working), "Replay leaves working data untouched")
})

for (input_kind in c("registered", "explicit_rds")) local({
  kind <- input_kind
  test(paste0("task_c_imputation_pooling_replay_", kind), {
    context <- new_case()
    source <- context$source; flags <- character()
    if (kind == "explicit_rds") {
      source <- file.path(context$root, "additional.rds")
      saveRDS(context$data[c("x1", "x2", "outcome_reg")], source)
      flags <- c("--rds", source)
    }
    before_source <- sha(source)
    original <- current_run(context, "impute", c(flags, "--vars", "x1,x2,outcome_reg", "--engine", "mice", "--m", "3", "--maxit", "2", "--seed", "714"))
    artifact <- original$result$results$imputation_artifact
    check(startsWith(artifact$path, ".nlss/imputations/mice-"), "One project-level mids artifact is preserved outside run bundles")
    artifact_file <- file.path(context$root, artifact$path)
    mids <- readRDS(artifact_file)
    near(mids$m, 3, "All requested imputations retained")
    working <- if (kind == "registered") context$working else file.path(context$root, "additional/additional.parquet")
    prepared <- context$env$read_parquet_data(working)
    for (name in c("x1", "x2", "outcome_reg")) near(prepared[[name]], context$data[[name]], paste("Original column unchanged:", name))
    completed <- mice::complete(mids, "all")
    reference_fits <- lapply(completed, function(d) stats::lm(outcome_reg ~ x1 + x2, data = d))
    reference <- as.data.frame(summary(mice::pool(reference_fits, rule = "rubin1987"), type = "all", conf.int = TRUE))
    pooled <- current_run(context, "mi_regression", c("--mids", artifact_file, "--formula", "outcome_reg ~ x1 + x2"))
    coefficients <- pooled$result$results$coefficients_df
    for (field in c("estimate", "std.error", "statistic", "df", "p.value", "conf.low", "conf.high", "ubar", "b", "t", "fmi")) {
      near(vapply(coefficients, function(row) row[[field]], numeric(1)), reference[[field]], paste("Pooled coefficient matches direct mice:", field))
    }
    check(file.exists(file.path(pooled$path, "fits.rds")) && file.exists(file.path(pooled$path, "pooled.rds")), "Fitted and pooled objects remain in owning run")
    check(!any(basename(list.files(file.path(context$root, ".nlss/runs"), recursive = TRUE)) == "mids.rds"), "Consumers do not recopy the mids artifact per run")
    check(!any(grepl("(^|/)backup(/|$)", list.files(context$root, recursive = TRUE))), "Current-project transforms do not accumulate permanent backup copies")
    check(identical(sha(source), before_source), "Imputation and pooling preserve original source")
    before_artifacts <- tree(file.path(context$root, ".nlss/imputations")); before_working <- file_state(working)
    relative_working <- substring(working, nchar(context$root) + 2L)
    relative_runs <- vapply(list(original, pooled), function(x) substring(x$path, nchar(context$root) + 2L), "")
    destination <- paste0(context$root, " moved ä")
    check(file.rename(context$root, destination), "Imputation project relocated as one folder")
    context$root <- destination
    for (i in seq_along(relative_runs)) {
      replay <- current_run(context, "replay_run", c("--request", file.path(destination, relative_runs[i], "request.json")))
      if (i == 1L) check(identical(replay$result$results$imputation_artifact, artifact) && !isTRUE(replay$result$results$data_change$applied), "Imputation replay reuses exact artifact without applying completion")
      else check(identical(replay$result$results$coefficients_df, pooled$result$results$coefficients_df), "Pooled replay retains numerical coefficients")
    }
    unchanged(before_artifacts, file.path(destination, ".nlss/imputations"), "Replay/consumers do not rewrite or duplicate mids artifacts")
    check(identical(file_state(file.path(destination, relative_working)), before_working), "Neither replay replaces visible prepared data")
  })
})

test("task_c_new_unmarked_imputation_does_not_adopt_a_project", {
  base <- file.path(work, "cases", case_name)
  dir.create(base, recursive = TRUE)
  output <- file.path(base, "output")
  yaml::write_yaml(list(defaults = list(output_dir = output, digits = 12L)), private_config)
  source <- file.path(base, "dataset.rds")
  data <- read.csv(golden)[c("x1", "x2", "outcome_reg")]
  saveRDS(data, source); before <- file_state(source)
  run_cli("impute", c("--rds", source, "--vars", "x1,x2,outcome_reg", "--engine", "mice", "--m", "2", "--maxit", "1", "--seed", "715"), base)
  directories <- list.dirs(file.path(output, "dataset/runs"), recursive = FALSE, full.names = TRUE)
  check(length(directories) == 1L, "New unmarked analysis still publishes its ordinary run")
  artifact <- json(file.path(directories, "result.json"))$results$imputation_artifact
  check(startsWith(artifact$path, ".nlss/imputations/"), "New standalone artifacts use the same single layout")
  path <- file.path(output, artifact$path)
  artifact_before <- file_state(path)
  run_cli("mi_regression", c("--mids", path, "--formula", "outcome_reg ~ x1 + x2"), base)
  pooled <- list.dirs(file.path(output, ".nlss/runs"), recursive = FALSE, full.names = TRUE)
  check(length(pooled) == 1L && isTRUE(json(file.path(pooled, "result.json"))$results$inference_pooled), "New standalone artifact remains usable for pooled inference")
  check(!any(basename(list.files(base, recursive = TRUE, all.files = TRUE)) == "nlss-workspace.yml"), "No project marker is silently created")
  check(identical(file_state(path), artifact_before) && identical(file_state(source), before), "Standalone pooling preserves source and mids bytes")
})

test("task_c_row_filter_retains_labels_and_replays_without_activation", {
  context <- new_case()
  before <- context$env$read_parquet_data(context$working)
  selected <- which(!is.na(before$labelled_scale))
  original <- current_run(context, "missings", c("--vars", "labelled_scale", "--method", "listwise"))
  after <- context$env$read_parquet_data(context$working)
  check(identical(after$leading_id, before$leading_id[selected]), "Row filtering retains the expected source identities")
  near(unlist(original$result$results$data_change$source_rows), selected, "Lineage preserves immediate input row map")
  check(identical(attr(before$labelled_scale, "labels"), attr(after$labelled_scale, "labels")), "Value labels survive filtered working-file publication")
  check(identical(sha(context$source), context$source_hash), "Filtering does not modify the original")
  working <- file_state(context$working)
  relative <- substring(original$path, nchar(context$root) + 2L)
  replay <- current_run(context, "replay_run", c("--request", paste0("./", relative, "/request.json")), cwd = context$root)
  check(identical(replay$result$results$data_change$output, original$result$results$data_change$output), "Row-filter replay verifies preserved output identity")
  check(identical(file_state(context$working), working), "Row-filter replay does not replace current working data")
  no_projections(context)
})

test("create_preserves_source_contract_and_independent_working_file", {
  context <- new_case()
  manifest <- yaml::read_yaml(file.path(context$root, "nlss-workspace.yml"), eval.expr = FALSE)
  check(identical(as.integer(manifest$schema_version), 2L), "New opt-in project has schema 2 marker")
  ds <- descriptor(context)
  check(identical(ds$source$sha256, context$source_hash), "Descriptor preserves exact original hash")
  check(file.exists(file.path(context$root, ".nlss/objects", context$source_hash)), "Original bytes are preserved in managed storage")
  check(!length(runs(context)), "Project creation does not invent a statistical run")
  check(!file.exists(file.path(context$root, ".nlss/objects", sha(context$note))), "Creation does not automatically capture a research note")
  before_objects <- objects(context)
  working_data <- context$env$read_parquet_data(context$working)
  near(working_data$visit_time, as.numeric(context$data$visit_time), "Subsecond times survive persistence")
  check(inherits(working_data$visit_date, "Date") && inherits(working_data$visit_timestamp, "POSIXct"), "Temporal classes survive persistence")
  check(identical(working_data$leading_id, context$data$leading_id), "Leading-zero IDs stay strings")
  check(identical(is.na(working_data$labelled_scale), rep(c(FALSE, FALSE, TRUE, FALSE), length.out = nrow(context$data))), "User-missing analysis mask is preserved")
  contract <- attr(working_data, "nlss_import_contract")
  check(identical(contract$columns$labelled_scale$variable_label, "Skala – ä") && length(contract$columns$labelled_scale$value_labels) == 2L,
    "Variable and value labels survive the working contract")
  check(length(contract$columns$labelled_scale$missing$observations) > 0L, "Original missing provenance remains in the dictionary")
  working_data$x1 <- working_data$x1 + 100
  context$env$write_parquet_data(working_data, context$working)
  check(identical(sha(context$source), context$source_hash), "Editing working data cannot change original bytes")
  check(identical(before_objects, objects(context)), "Editing working data cannot change immutable objects or alias them")
  connection <- file(context$working, open = "r+b")
  writeBin(charToRaw("EDIT"), connection); close(connection)
  check(identical(sha(context$source), context$source_hash) && identical(before_objects, objects(context)),
    "In-place working-byte edit proves the file is not a hardlink to original or historical evidence")
  no_projections(context)
})

test("unchanged_analyses_ignore_user_document_edits", {
  context <- new_case()
  first <- analysis(context)
  before_objects <- objects(context); before_versions <- versions(context)
  second <- analysis(context, select = FALSE)
  check(identical(first$request$dataset, second$request$dataset), "Unchanged analysis reuses the exact version")
  check(identical(before_objects, objects(context)) && identical(before_versions, versions(context)), "Repeated data use creates no duplicate objects or versions")
  check(!"scientific_context" %in% names(first$request), "Requests do not generate dedicated context references")
  check(!file.exists(file.path(context$root, ".nlss/objects", sha(context$note))), "An ordinary user document is not captured")
  write_text(context$note, c(text(context$note), "A later user-authored clarification."))
  third <- analysis(context)
  check(identical(first$request$dataset, third$request$dataset) && identical(before_objects, objects(context)), "Editing a user document creates neither a dataset version nor a captured note")
})

test("noop_and_real_transform_preserve_versions_without_backups", {
  context <- new_case()
  before <- file_state(context$working); before_objects <- objects(context); before_versions <- versions(context)
  noop <- analysis(context, "data_transform", character())
  change <- noop$result$results$data_change
  check(isTRUE(change$unchanged) && !isTRUE(change$applied), "No-op truthfully reports no working update")
  check(identical(change$input, change$output), "No-op reuses its input reference")
  check(identical(before, file_state(context$working)) && identical(before_objects, objects(context)) && identical(before_versions, versions(context)),
    "No-op preserves working bytes/mtime and adds no version or object")
  actual <- analysis(context, "data_transform", c("--calc", "x1_plus=x1+10"))
  change <- actual$result$results$data_change
  check(isTRUE(change$applied) && !isTRUE(change$unchanged), "Real transformation records applied change")
  check(identical(change$input, actual$request$dataset) && !identical(change$input$version_id, change$output$version_id), "Lineage binds exact before and distinct after versions")
  output <- verify_ref(context, change$output)
  near(output$x1_plus, context$data$x1 + 10, "Transformed values match independent base-R expectation")
  check(identical(sha(context$working), change$output$data_sha256) && identical(sha(context$source), context$source_hash), "Working file activates output while original stays unchanged")
  check(is.null(change$backup_path), "Existing immutable input replaces an extra permanent backup")
  check(is.null(actual$result$results$transformed_df) && !is.null(actual$result$results$data_representation),
    "Managed change references its output instead of duplicating every data row in JSON")
  check(identical(json(file.path(actual$path, "data-change.json")), change), "Authenticated change artifact matches result lineage")
  for (name in setdiff(names(before_objects), ".")) check(identical(objects(context)[[name]], before_objects[[name]]), "Transformation leaves previously preserved objects unchanged")
})

test("external_edit_is_consumed_once_with_honest_new_version", {
  context <- new_case()
  first <- analysis(context)
  data <- context$env$read_parquet_data(context$working)
  data <- data[rev(seq_len(nrow(data))), , drop = FALSE]
  data$x1 <- data$x1 + 25
  context$env$write_parquet_data(data, context$working)
  edited_hash <- sha(context$working)
  actual <- analysis(context)
  check(identical(actual$request$dataset$data_sha256, edited_hash), "Next analysis consumes the actual edited working bytes")
  check(!identical(first$request$dataset$version_id, actual$request$dataset$version_id), "External edit receives a new immutable version")
  summary <- Filter(function(row) identical(row$variable, "x1"), actual$result$results$summary_df)
  check(length(summary) == 1L, "Computed output identifies the selected x1 variable")
  near(summary[[1]]$mean, mean(data$x1, na.rm = TRUE), "Actual analysis mean matches independently calculated external working values")
  restored <- verify_ref(context, actual$request$dataset)
  near(restored$x1, data$x1, "External values and reordered rows are preserved")
  check(identical(restored$leading_id, data$leading_id), "External row order retains its actual IDs")
  check(identical(is.na(restored$labelled_scale), is.na(data$labelled_scale)), "Reordered missing values are not reapplied by old row positions")
  preserved <- objects(context); old_versions <- versions(context)
  again <- analysis(context)
  check(identical(actual$request$dataset, again$request$dataset) && identical(preserved, objects(context)) && identical(old_versions, versions(context)), "Already observed external edit is not resnapshotted on every use")
  check(identical(sha(context$source), context$source_hash), "Adoption never rewrites the received original")
})

# Fault injection is deliberately after candidate preparation, before publication.
# Nothing is patched on disk and no timing race is needed to reach the boundary.
fault_run <- function(context, kind, explicit_source = NULL) {
  e <- context$env
  state <- new.env(parent = emptyenv()); state$hit <- FALSE
  if (kind == "protocol_write") {
    original_append <- e$nlss_append_project_protocol
    e$nlss_append_project_protocol <- function(...) {
      original_append(...)
      if (!state$hit) { state$hit <- TRUE; stop("Injected failure after protocol write.") }
    }
  }
  if (kind %in% c("bundle_rename", "bundle_rename_external_edit")) e$file.rename <- function(from, to) {
    if (!state$hit && startsWith(basename(from), ".pending-") && dir.exists(from) && identical(dirname(from), dirname(to))) {
      state$hit <- TRUE
      if (kind == "bundle_rename_external_edit") {
        edited <- e$read_parquet_data(context$working); edited$x1 <- edited$x1 + 777
        e$write_parquet_data(edited, context$working); state$external_hash <- sha(context$working)
      }
      return(FALSE)
    }
    base::file.rename(from, to)
  }
  problem <- tryCatch({
    e$nlss_run_main("data_transform", function() {
      opts <- list(project = context$root, log = TRUE)
      if (is.null(explicit_source)) opts$dataset <- "study" else opts$rds <- explicit_source
      e$nlss_run_context$project_selection <- context$root
      before <- e$nlss_load_input(opts)
      e$nlss_begin_run("data_transform", before, opts)
      e$nlss_resolve_request(list(calc = "x1_plus=x1+10"), design = list(replay = list(eligible = TRUE, reason = "Deterministic test boundary")))
      after <- before; after$x1_plus <- after$x1 + 10
      change <- e$nlss_prepare_data_change(before, after); state$change <- change
      e$nlss_set_result(list(data_change = change))
      warning("Task F publication fixture warning: candidate is not yet committed.", call. = FALSE)
      e$nlss_stage_report(file.path(e$nlss_run_context$out_dir, "report_canonical.md"), "Data transformation",
        "| Variable | Changed |\n| --- | ---: |\n| x1_plus | 1 |", "Deterministic persistence fixture.")
      if (kind == "prepublication_conflict") {
        edited <- before; edited$x1 <- edited$x1 + 555
        e$write_parquet_data(edited, context$working)
        state$external_hash <- sha(context$working); state$hit <- TRUE
      }
    })
    NULL
  }, error = conditionMessage)
  check(state$hit && !is.null(problem), paste(kind, "reaches intended fault and reports failure"))
  state
}
for (kind in c("prepublication_conflict", "bundle_rename", "bundle_rename_external_edit", "protocol_write")) local({
  fault <- kind
  test(paste0("publication_", fault, "_preserves_user_bytes"), {
    context <- new_case()
    if (fault == "protocol_write") analysis(context)
    previous_runs <- runs(context)
    previous_protocol <- text(file.path(context$root, "report_canonical.md"))
    previous <- sha(context$working); source_state <- file_state(context$source); before_objects <- objects(context)
    state <- fault_run(context, fault)
    check(identical(sha(context$working), if (fault %in% c("bundle_rename", "protocol_write")) previous else state$external_hash), "Rollback restores only owned bytes and never overwrites detected external edits")
    check(identical(source_state, file_state(context$source)), "Failed publication leaves original untouched")
    for (name in setdiff(names(before_objects), ".")) check(identical(objects(context)[[name]], before_objects[[name]]), "Failure preserves prior immutable evidence")
    verify_ref(context, state$change$output)
    published <- setdiff(runs(context), previous_runs)
    check(length(published) == 1L, "One failed execution has one terminal evidence bundle")
    terminal <- json(file.path(published, "result.json"))
    check(identical(terminal$status, "failed") && identical(terminal$calculation_status, "completed"),
      "Failed publication retains the completed calculation without advertising a completed run")
    check(is.list(terminal$results$data_change) && !isTRUE(terminal$results$data_change$applied),
      "Retained numerical result does not falsely claim the working update was applied")
    failed_change <- terminal$results$data_change
    if (identical(failed_change$recovery_status, "manual_recovery_required")) {
      check(is.null(failed_change$applied), "Uncertain manual recovery does not claim the prior working update was rolled back")
    } else {
      check(identical(failed_change$applied, FALSE), "Detected conflict or verified rollback records an unapplied working update")
    }
    check(identical(json(file.path(published, "data-change.json")), terminal$results$data_change),
      "Failed result and authenticated data-change artifact agree")
    check(identical(terminal$results$data_change$output, state$change$output), "Failure retains the computed candidate reference")
    if (fault == "prepublication_conflict") check(identical(terminal$publication_status, "conflict_not_applied"),
      "Detected external conflict is explicitly marked not applied")
    check(!file.exists(file.path(published, "output.md")) && file.exists(file.path(published, "partial-output.md")),
      "Calculated output is retained only as partial output for failed publication")
    for (artifact in terminal$artifacts) check(identical(sha(file.path(published, artifact$path)), artifact$sha256),
      paste("Failed-run artifact hash:", artifact$path))
    check(any(vapply(terminal$warnings, function(w) grepl("Task F publication fixture warning", w$message, fixed = TRUE), logical(1))),
      "Execution preserves the warning in machine-readable evidence")
    no_projections(context)
    protocol <- text(file.path(context$root, "report_canonical.md"))
    check(grepl("Warning: Task F publication fixture warning", protocol, fixed = TRUE), "Default protocol exposes the recorded warning")
    if (fault == "protocol_write") {
      before_rebuild <- tree(file.path(context$root, ".nlss"))
      context$env$nlss_rebuild_project_protocol(context$root, "rebuilt-failure.md")
      rebuilt <- text(file.path(context$root, "rebuilt-failure.md"))
      check(grepl("data_transform — failed", rebuilt, fixed = TRUE) &&
        grepl("Warning: Task F publication fixture warning", rebuilt, fixed = TRUE), "Rebuilt protocol retains failed status and warning")
      unchanged(before_rebuild, file.path(context$root, ".nlss"), "Reconstruction does not execute statistics or modify stored evidence")
    }
    check(startsWith(protocol, previous_protocol) && grepl("data_transform — failed", protocol, fixed = TRUE) &&
      !grepl("data_transform — completed", protocol, fixed = TRUE), "Failure preserves earlier protocol entries and never leaves a success-shaped transformation entry")
  })
})

for (kind in c("bundle_rename", "bundle_rename_external_edit", "protocol_write")) local({
  fault <- kind
  test(paste0("task_c_explicit_import_", fault), {
    context <- new_case()
    source <- file.path(context$root, "additional.rds")
    saveRDS(context$data[c("x1", "x2", "outcome_reg")], source)
    original_source <- file_state(source)
    current_run(context, "descriptive_stats", c("--rds", source, "--vars", "x1"))
    context$working <- file.path(context$root, "additional/additional.parquet")
    before <- sha(context$working); original_protocol <- text(file.path(context$root, "report_canonical.md"))
    previous <- runs(context)
    state <- fault_run(context, fault, explicit_source = source)
    check(identical(sha(context$working), if (fault == "bundle_rename_external_edit") state$external_hash else before), "Explicit import recovery preserves owned bytes or a later external edit")
    check(identical(file_state(source), original_source), "Failed publication never changes the original RDS")
    check(!dir.exists(file.path(context$root, "additional/backup")), "Protected publication needs no permanent backup family")
    context$env$nlss_verify_dataset(state$change$input, context$root)
    check(grepl(original_protocol, text(file.path(context$root, "report_canonical.md")), fixed = TRUE), "Earlier protocol evidence survives")
    added <- setdiff(runs(context), previous)
    check(length(added) == 1L && identical(json(file.path(added, "result.json"))$status, "failed"), "Explicit input failure is not published as success")
    change <- json(file.path(added, "data-change.json"))
    check(!isTRUE(change$applied), "Failed lineage does not claim committed working data")
    if (fault == "bundle_rename_external_edit") check(identical(change$recovery_status, "manual_recovery_required"), "External edit is retained and recovery uncertainty is explicit")
  })
})

test("creation_rejects_collisions_unsafe_paths_and_existing_markers", {
  variants <- c("existing_working", "same_file", "managed_collision", "parent_traversal", "absolute_external", "managed_working", "nested_project", "existing_marker", "unsupported_format")
  for (variant in variants) {
    context <- new_case(variant, create = FALSE)
    source <- "Original.parquet"; target <- "Working data/current.parquet"
    if (variant == "existing_working") write_text(context$working, "User-owned existing content")
    if (variant == "same_file") target <- source
    if (variant == "managed_collision") write_text(file.path(context$root, ".nlss/user.txt"), "Unowned managed-name collision")
    if (variant == "parent_traversal") target <- "../outside.parquet"
    if (variant == "absolute_external") target <- file.path(dirname(context$root), "outside.parquet")
    if (variant == "managed_working") target <- ".nlss/current.parquet"
    if (variant == "nested_project") write_text(file.path(context$root, "Working data/nlss-workspace.yml"), "schema_version: 1")
    if (variant == "existing_marker") write_text(file.path(context$root, "nlss-workspace.yml"), c("schema_version: 1", "workspace_id: legacy", "datasets: []"))
    if (variant == "unsupported_format") { source <- "source.unsupported"; write_text(file.path(context$root, source), c("x", "1")) }
    before <- tree(context$root)
    run_cli("project_create", c("--project", context$root, "--source", source, "--working", target, "--name", "study"), failure = TRUE)
    unchanged(before, context$root, paste(variant, "refusal preserves all existing project paths and bytes"))
  }
  context <- new_case("absolute_internal", create = FALSE)
  run_cli("project_create", c("--project", context$root, "--source", context$source, "--working", context$working, "--name", "study"))
  ds <- descriptor(context)
  check(identical(ds$working, "Working data/current.parquet") && identical(ds$source$selected_path, "Original.parquet"),
    "Contained absolute source/working selections are normalized to portable project-relative descriptors")
})

test("symlink_and_special_file_boundaries_fail_without_traversal", {
  for (variant in c("source_symlink", "working_parent_symlink", "managed_symlink", "source_fifo")) {
    context <- new_case(variant, create = FALSE)
    outside <- file.path(dirname(context$root), paste0(variant, "-outside")); dir.create(outside)
    write_text(file.path(outside, "sentinel"), "Unrelated external content")
    source <- "Original.parquet"
    if (variant == "source_fifo") {
      if (.Platform$OS.type == "windows" || !nzchar(Sys.which("mkfifo"))) { skip("Source FIFO case requires POSIX mkfifo"); next }
      source <- "pipe.parquet"
      check(system2(Sys.which("mkfifo"), shQuote(file.path(context$root, source))) == 0L, "Private source FIFO created")
    } else {
      target <- switch(variant, source_symlink = file.path(context$root, "alias.parquet"), working_parent_symlink = dirname(context$working), managed_symlink = file.path(context$root, ".nlss"))
      origin <- if (variant == "source_symlink") context$source else outside
      if (!isTRUE(suppressWarnings(file.symlink(origin, target)))) { skip(paste(variant, "requires OS symlink permission")); next }
      if (variant == "source_symlink") source <- "alias.parquet"
    }
    before <- tree(context$root); external_before <- tree(outside)
    run_cli("project_create", c("--project", context$root, "--source", source, "--working", "Working data/current.parquet", "--name", "study"), failure = TRUE, timeout = 8L)
    unchanged(before, context$root, paste(variant, "refusal preserves project"))
    unchanged(external_before, outside, paste(variant, "does not touch external target"))
  }
})

test("malformed_descriptor_and_conflicting_selectors_are_rejected", {
  for (variant in c("descriptor", "mixed_source", "unknown_dataset")) {
    context <- new_case(variant)
    argv <- c("--project", context$root, "--vars", "x1")
    if (variant == "descriptor") write_text(descriptors(context), "{malformed")
    if (variant == "mixed_source") argv <- c(argv, "--dataset", "study", "--parquet", context$source)
    if (variant == "unknown_dataset") argv <- c(argv, "--dataset", "absent")
    before <- tree(context$root)
    run_cli("descriptive_stats", argv, failure = TRUE)
    unchanged(before, context$root, paste(variant, "fails before publication without changing project"))
  }
})

test("managed_inspection_is_readonly_and_never_calls_initial_version_latest", {
  context <- new_case()
  initial <- descriptor(context)$initial_version
  actual <- analysis(context, "data_transform", c("--calc", "x1_plus=x1+10"))
  before <- tree(context$root)
  response <- run_cli("project_inspect", c("--project", context$root, "--verify", "TRUE", "--format", "json"))
  inspected <- jsonlite::fromJSON(response$stdout, simplifyVector = FALSE)
  unchanged(before, context$root, "Managed inspection changes no data, note, object, descriptor, run, projection or lock")
  check(isTRUE(inspected$read_only) && identical(inspected$layout, "managed_parquet_v1") && length(inspected$datasets) == 1L,
    "Inspector recognizes the managed project with one dataset")
  ds <- inspected$datasets[[1]]
  check(identical(ds$reference_basis, "initial_registered_version_not_latest") && identical(ds$recorded_version_id, initial$version_id) &&
      !identical(ds$recorded_version_id, actual$result$results$data_change$output$version_id),
    "Inspector explicitly identifies the initial reference and never mislabels it as latest")
  check(identical(ds$working$comparison, "differs_from_recorded") && identical(ds$snapshot$comparison, "matches_recorded") &&
      identical(ds$preserved_source$comparison, "matches_recorded"),
    "Legitimate changed working data remain distinct from intact initial snapshot and preserved source")
  check(inspected$project_runs$published_candidates == 1L && identical(inspected$project_runs$verification, "not_verified"),
    "Managed run inventory counts candidates without claiming run or replay verification")
})

test("replay_rejects_nonrequest_specialfiles_and_wrong_parent_before_read", {
  context <- new_case()
  original <- analysis(context)
  sentinel <- file.path(dirname(context$root), "external-request.json")
  write_text(sentinel, "Unrelated external content must not be read as a request")
  for (kind in c("nonrequest_symlink", "nonrequest_fifo", "wrong_parent_fifo")) {
    selected <- file.path(original$path, paste0(kind, ".json"))
    if (kind == "nonrequest_symlink") {
      if (!isTRUE(suppressWarnings(file.symlink(sentinel, selected)))) { skip("Non-request symlink requires OS symlink permission"); next }
    } else {
      if (.Platform$OS.type == "windows" || !nzchar(Sys.which("mkfifo"))) { skip(paste(kind, "requires POSIX mkfifo")); next }
      if (kind == "wrong_parent_fifo") {
        selected <- file.path(context$root, ".nlss/not-runs", basename(original$path), "request.json")
        dir.create(dirname(selected), recursive = TRUE)
        check(file.copy(file.path(original$path, "result.json"), file.path(dirname(selected), "result.json")), "Private wrong-layout companion result copied")
      }
      check(system2(Sys.which("mkfifo"), shQuote(selected)) == 0L, paste("Private", kind, "created"))
    }
    before <- tree(context$root); external_before <- file_state(sentinel)
    guard <- runtime(); original_reader <- guard$read_import_json; selected_read <- FALSE
    guard$read_import_json <- function(path) {
      if (identical(normalizePath(path, winslash = "/", mustWork = FALSE), normalizePath(selected, winslash = "/", mustWork = FALSE))) {
        selected_read <<- TRUE
        stop("Forbidden request JSON read reached before path preflight.")
      }
      original_reader(path)
    }
    expect_error(guard$nlss_read_replay(selected))
    check(!selected_read, paste(kind, "is rejected before its JSON reader is reached"))
    run_cli("replay_run", c("--request", selected), failure = TRUE, timeout = 8L)
    unchanged(before, context$root, paste(kind, "is rejected without reading a FIFO or modifying evidence"))
    check(identical(external_before, file_state(sentinel)), "Replay rejection preserves external symlink target")
  }
})

test("unmarked_source_does_not_search_child_markers", {
  for (kind in c("fifo", "symlink")) {
    context <- new_case(kind, create = FALSE)
    child <- file.path(context$root, "child-project")
    dir.create(child)
    marker <- file.path(child, "nlss-workspace.yml")
    external <- file.path(dirname(context$root), paste0(kind, "-external-marker.yml"))
    write_text(external, c("schema_version: 2", "storage: managed_parquet_v1", "workspace_id: external", "datasets: []"))
    if (kind == "fifo") {
      if (.Platform$OS.type == "windows" || !nzchar(Sys.which("mkfifo"))) { skip("Child-marker FIFO requires POSIX mkfifo"); next }
      check(system2(Sys.which("mkfifo"), shQuote(marker)) == 0L, "Private child-marker FIFO created")
    } else if (!isTRUE(suppressWarnings(file.symlink(external, marker)))) {
      skip("Child-marker symlink requires OS symlink permission"); next
    }
    before <- tree(context$root); external_before <- file_state(external)
    yaml::write_yaml(list(defaults = list(output_dir = file.path(work, "standalone-output", kind), digits = 12L)), private_config)
    output_before <- list.files(file.path(work, "standalone-output"), "^request[.]json$", recursive = TRUE, full.names = TRUE)
    run_cli("descriptive_stats", c("--parquet", context$source, "--vars", "x1"), cwd = context$root, timeout = 15L)
    unchanged(before, context$root, paste(kind, "child marker is not searched or adopted"))
    check(identical(external_before, file_state(external)), "Standalone execution never modifies the child symlink target")
    output_after <- list.files(file.path(work, "standalone-output"), "^request[.]json$", recursive = TRUE, full.names = TRUE)
    check(length(setdiff(output_after, output_before)) == 1L, "Explicit standalone source publishes once to its configured output")
  }
})

test("root_rename_replay_uses_frozen_input_without_activation", {
  context <- new_case()
  original <- analysis(context, "data_transform", c("--calc", "x1_plus=x1+10"))
  old_bundle <- tree(original$path)
  relative_run <- substring(original$path, nchar(context$root) + 2L)
  destination <- paste0(context$root, " renamed ä with spaces")
  check(file.rename(context$root, destination), "Whole project renamed")
  context$root <- destination
  context$source <- file.path(destination, "Original.parquet"); context$working <- file.path(destination, "Working data/current.parquet"); context$note <- file.path(destination, "research_note.md")
  old_path <- file.path(destination, relative_run)
  working_data <- context$env$read_parquet_data(context$working); working_data$x1 <- working_data$x1 + 200
  context$env$write_parquet_data(working_data, context$working)
  write_text(context$note, "Later user context, not the replay context.")
  before <- file_state(context$working); before_objects <- objects(context)
  replay <- analysis(context, "replay_run", c("--request", file.path(old_path, "request.json")))
  check(identical(replay$request$replay_of, original$request$run_id), "Replay binds original run identity after relocation")
  check(identical(replay$request$dataset, original$request$dataset), "Replay uses original immutable input rather than externally edited working data")
  check(!isTRUE(replay$result$results$data_change$applied), "Replay explicitly does not activate output")
  check(identical(replay$result$results$data_change$output, original$result$results$data_change$output), "Replay verifies original output reference")
  check(identical(before, file_state(context$working)) && identical(before_objects, objects(context)), "Replay neither changes working bytes/mtime nor recopies data/context objects")
  unchanged(old_bundle, old_path, "Replay leaves the original run byte-for-byte intact")
})

test("replay_rejects_damaged_objects_and_code_environment_drift", {
  context <- new_case()
  original <- analysis(context)
  path <- file.path(original$path, "request.json")
  e <- runtime(); code_hash <- e$nlss_code_hash
  e$nlss_code_hash <- function() paste(rep("0", 64), collapse = "")
  before <- tree(context$root)
  expect_error(e$nlss_read_replay(path), "code differs")
  unchanged(before, context$root, "Code gate remains strict and read-only")
  e$nlss_code_hash <- code_hash
  current_environment <- e$nlss_execution_environment
  e$nlss_execution_environment <- function() { value <- current_environment(); value$r_version <- "deliberate mismatch"; value }
  expect_error(e$nlss_read_replay(path), "environment differs")
  unchanged(before, context$root, "Environment gate remains strict and read-only")
  damaged_path <- file.path(context$root, original$request$dataset$snapshot_path)
  Sys.chmod(damaged_path, mode = "0644")
  write_text(damaged_path, "Damaged historical object bytes")
  damaged <- tree(context$root)
  run_cli("replay_run", c("--request", path), failure = TRUE)
  unchanged(damaged, context$root, "Damaged historical input cannot trigger replay, repair or activation")
})


passed <- sum(vapply(results, function(result) result$passed, logical(1)))
stable <- identical(source_initial, hash_sources())
status <- if (length(results) && passed == length(results) && stable) 0L else 1L
save_results(status, utc())
cat(sprintf("Phase 3 persistence: %d/%d grouped cases passed. Results: %s\n", passed, length(results), file.path(work, "results.json")))
if (!length(results)) cat("No cases matched --match; this is not a passing run.\n")
if (!stable) cat("Source identity changed during execution; this run does not certify a stable revision.\n")
if (!nzchar(forced) && keep > 0L) {
  candidates <- sort(list.dirs(collection, recursive = FALSE, full.names = TRUE), decreasing = TRUE)
  candidates <- candidates[grepl("^run-[0-9]{14}-[0-9]+$", basename(candidates))]
  if (length(candidates) > keep) for (candidate in candidates[seq.int(keep + 1L, length(candidates))]) {
    if (identical(candidate, work) || nzchar(Sys.readlink(candidate))) next
    previous <- tryCatch(json(file.path(candidate, "results.json")), error = function(e) NULL)
    if (identical(previous$owner, "nlss-persistence-test-runner") && identical(previous$suite, "phase3-persistence") &&
        !is.null(previous$finished_at) && !is.null(previous$exit_status)) unlink(candidate, recursive = TRUE)
  }
}
quit(status = status)
