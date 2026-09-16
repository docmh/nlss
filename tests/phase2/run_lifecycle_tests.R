#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Public CLI lifecycle fixtures; no production code generates expected results.
script <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])[1]
repo <- normalizePath(file.path(dirname(script), "../.."), winslash = "/")
args <- commandArgs(TRUE)
if ("--help" %in% args) { cat("Usage: run_lifecycle_tests.R [--root PATH] [--keep N] [--match REGEX]\n"); quit(status = 0) }
if (length(args) %% 2L || (length(args) && any(!args[seq(1L, length(args), 2L)] %in% c("--root", "--keep", "--match")))) stop("Unknown/incomplete test option")
arg <- function(key, default) { i <- which(args == key); if (length(i) > 1L) stop("Repeated option"); if (length(i)) args[i + 1L] else default }
cfg <- yaml::read_yaml(file.path(repo, "tests/tests.yml"), eval.expr = FALSE)$tests
forced <- arg("--root", Sys.getenv("NLSS_TEST_ROOT", ""))
if (!nzchar(forced)) forced <- file.path(repo, cfg$output_dir)
collection <- file.path(forced, "phase2-lifecycle")
dir.create(collection, recursive = TRUE, showWarnings = FALSE)
collection <- normalizePath(collection, winslash = "/")
keep <- suppressWarnings(as.numeric(arg("--keep", Sys.getenv("NLSS_KEEP_RUNS", as.character(cfg$keep_runs_default)))))
if (length(keep) != 1L || !is.finite(keep) || keep < 0L || keep != floor(keep)) stop("Invalid --keep")
pattern <- arg("--match", ".*"); invisible(grepl(pattern, "validate regex"))
root <- tempfile("run-", collection); dir.create(root)
sha <- function(path) digest::digest(file = path, algo = "sha256")
bytes <- function(path) if (file.exists(path) && !dir.exists(path)) readBin(path, "raw", file.info(path)$size) else NULL
json <- function(path) jsonlite::read_json(path, simplifyVector = FALSE)
write_json <- function(value, path) jsonlite::write_json(value, path, pretty = TRUE, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
utc <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
files <- sort(c(list.files(file.path(repo, "scripts"), recursive = TRUE, full.names = TRUE),
  list.files(file.path(repo, "assets"), recursive = TRUE, full.names = TRUE)))
source_identity <- setNames(vapply(files, sha, character(1)), substring(files, nchar(repo) + 2L))
started <- utc()
write_json(list(started_utc = started, selected_pattern = pattern, runner_sha256 = sha(script),
  production_files = as.list(source_identity)), file.path(root, "started.json"))
frozen <- file.path(root, "repo"); dir.create(frozen)
stopifnot(all(file.copy(file.path(repo, c("scripts", "assets", "SKILL.md")), frozen, recursive = TRUE)))
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
data <- data.frame(id = 1:6, score = c(1, NA, 3, 4, NA, 6), group = c("a", "a", "b", "b", "a", "b"))
new_case <- function(name, overrides = list()) {
  base <- tempfile(paste0(name, "-"), root); dir.create(base)
  project <- file.path(base, "project")
  config <- file.path(base, "config.yml")
  yaml::write_yaml(modifyList(list(defaults = list(output_dir = project, interactive = FALSE),
    logging = list(include_checksum = FALSE)), overrides), config)
  input <- file.path(base, "sample.rds"); saveRDS(data, input)
  list(base = base, project = project, config = config, input = input, out = file.path(project, "sample"), calls = new.env())
}
run <- function(case, module, args = character(), ok = TRUE, cwd = case$base, source_repo = frozen, extra_env = list()) {
  setwd(cwd)
  n <- length(ls(case$calls)) + 1L; assign(as.character(n), TRUE, case$calls)
  output <- file.path(case$base, paste0("cli-", n, ".log"))
  old <- Sys.getenv(unique(c("NLSS_CONFIG_PATH", "NLSS_REPLAY_REQUEST", names(extra_env))), unset = NA_character_)
  on.exit({ for (key in names(old)) if (is.na(old[[key]])) Sys.unsetenv(key) else do.call(Sys.setenv, setNames(list(old[[key]]), key)) }, add = TRUE)
  Sys.setenv(NLSS_CONFIG_PATH = case$config, OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1")
  Sys.unsetenv("NLSS_REPLAY_REQUEST")
  if (length(extra_env)) do.call(Sys.setenv, extra_env)
  start <- utc()
  exit <- system2(file.path(R.home("bin"), "Rscript"), shQuote(c(file.path(source_repo, "scripts/R", paste0(module, ".R")), args)), stdout = output, stderr = output)
  write_json(list(started_utc = start, finished_utc = utc(), module = module, args = args, exit = exit), paste0(output, ".json"))
  check(if (ok) exit == 0L else exit != 0L, paste(module, "exit", exit, paste(readLines(output, warn = FALSE), collapse = " | ")))
  invisible(readLines(output, warn = FALSE))
}
init <- function(case, extra = character()) run(case, "init_workspace", c("--rds", case$input, extra))
audits <- function(out, module) {
  directories <- list.dirs(file.path(out, "utility-runs"), recursive = FALSE, full.names = TRUE)
  directories[startsWith(basename(directories), paste0(module, "-"))]
}
audit <- function(out, module) {
  dirs <- audits(out, module); check(length(dirs) > 0L, "Utility event preserved")
  saved <- dirs[which.max(file.info(dirs)$mtime)]
  request <- json(file.path(saved, "request.json")); result <- json(file.path(saved, "result.json"))
  check(identical(request$kind, "utility") && identical(request$replay$eligible, FALSE), "Truthful non-statistical audit")
  check(identical(result$status, "completed"), "Completed event")
  for (item in result$artifacts) check(identical(sha(file.path(saved, item$path)), item$sha256), paste("Artifact hash", item$path))
  list(path = saved, request = request, result = result)
}
projections <- function(case) c(file.path(case$project, "nlss-workspace.yml"), file.path(case$out, c("scratchpad.md", "report_canonical.md", "analysis_log.jsonl", "sample.parquet", "import.json")))
no_imports <- function(case) check(!length(list.files(case$project, "[.]parquet$", recursive = TRUE)), "No dataset imported during failing preflight")
semantic_path <- function(case, meta = "sample-description", intent = "interpret") file.path(case$out, paste0("report_", format(Sys.Date(), "%Y%m%d"), "_", meta, "_", intent, ".md"))

test("init_multiple_formats_and_exact_bindings", {
  case <- new_case("multi")
  csv <- file.path(case$base, "other.csv"); write.csv(data, csv, row.names = FALSE)
  sav <- file.path(case$base, "labelled.sav"); labelled <- data
  labelled$score <- haven::labelled_spss(c(1, 99, 3, 4, 99, 6), labels = c(Low = 1, Missing = 99), na_values = 99, label = "Score label")
  haven::write_sav(labelled, sav)
  parquet <- file.path(case$base, "third.parquet"); arrow::write_parquet(data, parquet)
  run(case, "init_workspace", c("--rds", case$input, "--csv", csv, "--sav", sav, "--parquet", parquet))
  manifest <- yaml::read_yaml(file.path(case$project, "nlss-workspace.yml"))
  check(identical(vapply(manifest$datasets, `[[`, "", "name"), c("other", "labelled", "sample", "third")), "Format ordering retained")
  check(identical(manifest$active_dataset, "other"), "First dataset active")
  event <- audit(case$project, "init_workspace")
  check(length(event$request$request$datasets) == 4L, "All exact versions recorded")
  for (entry in manifest$datasets) {
    out <- file.path(case$project, entry$name)
    working <- file.path(case$project, entry$parquet)
    check(nrow(as.data.frame(arrow::read_parquet(working))) == 6L, "All rows retained")
    logs <- lapply(readLines(file.path(out, "analysis_log.jsonl")), jsonlite::fromJSON)
    check(identical(tail(logs, 1L)[[1]]$dataset$dataset_id, entry$dataset$dataset_id), "Per-dataset log identity, not last loaded dataset")
    check(!grepl(case$base, paste(readLines(file.path(out, "analysis_log.jsonl")), collapse = "\n"), fixed = TRUE), "First initialization log masks external input paths")
    check(identical(sha(file.path(case$project, entry$dataset$snapshot_path)), entry$dataset$data_sha256), "Exact working-version bytes")
  }
  dictionary <- json(file.path(case$project, "labelled", "dictionary.json"))
  check(identical(dictionary$columns$score$variable_label, "Score label"), "SPSS variable label retained")
})

test("init_rdata_multiple_files_and_object_selection", {
  case <- new_case("rdata"); source_one <- file.path(case$base, "one.RData"); source_two <- file.path(case$base, "two.RData")
  first <- data; second <- data[1:3, ]; save(first, file = source_one); save(second, file = source_two)
  run(case, "init_workspace", c("--rdata", paste(source_one, source_two, sep = ","), "--df", "first,second"))
  check(nrow(as.data.frame(arrow::read_parquet(file.path(case$project, "first/first.parquet")))) == 6L, "First RData object")
  check(nrow(as.data.frame(arrow::read_parquet(file.path(case$project, "second/second.parquet")))) == 3L, "Second RData object")
  audit(case$project, "init_workspace")
})

test("init_csv_options_and_agent_template_override", {
  case <- new_case("csv-options", list(defaults = list(csv = list(sep = ";", decimal = ",", na_values = "MISSING", col_types = "id=integer,score=double"))))
  source <- file.path(case$base, "regional data.csv")
  writeLines(c("id;score", "1;1,5", "2;MISSING"), source)
  template <- file.path(case$base, "custom.md")
  writeLines(c("# Custom initialization", "{{agent}}", "{{table_body}}", "{{narrative_default}}"), template)
  run(case, "init_workspace", c("--csv", source, "--dataset-name", "regional", "--agent", "Research Agent", "--template", template))
  result <- as.data.frame(arrow::read_parquet(file.path(case$project, "regional/regional.parquet")))
  check(identical(result$id, 1:2) && identical(result$score, c(1.5, NA_real_)), "Configured CSV declarations used exactly")
  event <- audit(case$project, "init_workspace")
  check(identical(event$request$request$agent, "Research Agent"), "CLI agent overrides config in audit")
  check(identical(bytes(file.path(event$path, "report-template.md")), bytes(template)), "Template captured exactly")
  report <- paste(readLines(file.path(case$project, "regional/report_canonical.md"), warn = FALSE), collapse = "\n")
  check(grepl("Custom initialization", report, fixed = TRUE) && grepl("Research Agent", report, fixed = TRUE), "Selected template controls report")
  run(case, "init_workspace", c("--csv", source, "--dataset-name", "regional"))
})

test("init_empty_placeholder_and_log_false", {
  case <- new_case("empty")
  run(case, "init_workspace", c("--log", "FALSE"))
  check(file.exists(file.path(case$project, "workspace/scratchpad.md")), "Planning placeholder")
  check(!file.exists(file.path(case$project, "workspace/analysis_log.jsonl")), "Optional JSONL disabled")
  no_imports(case); event <- audit(case$project, "init_workspace")
  check(event$result$results$dataset_count == 0L, "Zero dataset lifecycle record")
})

test("init_repeated_preserves_scratchpad_and_edited_working_copy", {
  case <- new_case("reinit"); init(case)
  scratch <- file.path(case$out, "scratchpad.md"); writeBin(charToRaw("# Existing researcher notes\r\nDo not rewrite.\r\n"), scratch); before <- bytes(scratch)
  working <- file.path(case$out, "sample.parquet"); changed <- as.data.frame(arrow::read_parquet(working)); changed$score[1] <- 42; arrow::write_parquet(changed, working)
  working_sha <- sha(working); init(case)
  check(identical(bytes(scratch), before), "Research notes preserved byte-exact")
  check(identical(sha(working), working_sha), "Unchanged source reuses edited working data")
  manifest <- yaml::read_yaml(file.path(case$project, "nlss-workspace.yml"))
  check(identical(manifest$datasets[[1]]$dataset$data_sha256, working_sha), "Manifest references edited input version")
  event <- audit(case$project, "init_workspace")
  check(identical(event$result$results$scratchpads[[1]]$action, "preserved"), "Scratchpad action audited")
})

test("init_changed_source_requires_explicit_new_version", {
  case <- new_case("sourcechange"); init(case)
  paths <- projections(case); before <- lapply(paths, bytes)
  newer <- data; newer$score[1] <- 17; saveRDS(newer, case$input)
  run(case, "init_workspace", c("--rds", case$input), ok = FALSE)
  check(identical(lapply(paths, bytes), before), "Source-change refusal leaves projections exact")
  init(case, c("--import-action", "new-version"))
  check(as.data.frame(arrow::read_parquet(file.path(case$out, "sample.parquet")))$score[1] == 17, "Explicit source version activated")
  check(length(list.dirs(file.path(case$out, "versions"), recursive = FALSE)) == 2L, "Old immutable version retained")
})

for (kind in c("missing", "invalid_rds", "collision", "template")) local({ selected <- kind
  test(paste0("init_preflight_", selected), {
    case <- new_case(selected); extra <- character()
    if (selected == "missing") extra <- c("--csv", file.path(case$base, "missing.csv"))
    if (selected == "invalid_rds") { bad <- file.path(case$base, "bad.rds"); saveRDS(1:5, bad); extra <- c("--rds", paste(case$input, bad, sep = ",")) }
    if (selected == "collision") { bad <- file.path(case$base, "sample.csv"); write.csv(data, bad, row.names = FALSE); extra <- c("--csv", bad) }
    if (selected == "template") extra <- c("--template", file.path(case$base, "absent.md"))
    args <- if (selected == "invalid_rds") extra else c("--rds", case$input, extra)
    run(case, "init_workspace", args, ok = FALSE); no_imports(case)
    check(!file.exists(file.path(case$project, "nlss-workspace.yml")), "No false initialized manifest")
  })
})

test("metaskill_missing_report_precedes_import", {
  case <- new_case("missingreport")
  run(case, "metaskill_runner", c("--rds", case$input, "--meta", "sample-description", "--phase", "finalization", "--intent", "interpret"), ok = FALSE)
  no_imports(case)
  check(!file.exists(file.path(case$out, "analysis_log.jsonl")), "No false successful finalization log")
})

test("metaskill_semantic_report_exact_archive_and_reconstruction", {
  case <- new_case("semantic"); init(case)
  report <- semantic_path(case)
  content <- charToRaw(enc2utf8("# Research interpretation\r\n\r\n## Context matters\r\nA bespoke synthesis beyond any template: München, uncertainty, and alternative explanations.\r\n"))
  writeBin(content, report)
  run(case, "metaskill_runner", c("--meta", "sample-description", "--phase", "finalization", "--intent", "interpret", "--synopsis", "Context-sensitive synthesis."), cwd = case$project, extra_env = list(LC_ALL = "C"))
  check(identical(bytes(report), content), "Authored bytes never rewritten")
  event <- audit(case$out, "metaskill_runner")
  check(identical(bytes(file.path(event$path, "semantic-report.md")), content), "Semantic artifact byte-exact")
  check(identical(event$result$results$semantic_report$sha256, sha(report)), "Authored report hash")
  canonical <- paste(readLines(file.path(case$out, "report_canonical.md"), warn = FALSE), collapse = "\n")
  check(grepl('"sample-description" finalized', canonical, fixed = TRUE), "Finalized narrative is truthful")
  check(grepl("Context-sensitive synthesis.", canonical, fixed = TRUE), "Synopsis retained")
  logs <- lapply(readLines(file.path(case$out, "analysis_log.jsonl")), jsonlite::fromJSON)
  log <- tail(logs, 1L)[[1]]
  check(nzchar(log$metaskill_report_block_b64), "Legacy reconstruction block retained")
  payload <- jsonlite::base64_dec(log$metaskill_report_block_b64)
  if (grepl("gzip", log$metaskill_report_block_encoding)) payload <- memDecompress(payload, "gzip")
  check(identical(payload, content), "Legacy semantic block preserves CRLF and trailing newline byte-exact")
})

test("lifecycle_help_false_and_semantic_no_trailing_newline", {
  case <- new_case("help-false"); init(case, c("--help", "FALSE"))
  content <- charToRaw("# Authored report\nNo final newline.")
  writeBin(content, semantic_path(case))
  run(case, "metaskill_runner", c("--meta", "sample-description", "--phase", "finalization", "--intent", "interpret", "--help", "FALSE"), cwd = case$project)
  logs <- lapply(readLines(file.path(case$out, "analysis_log.jsonl")), jsonlite::fromJSON)
  entry <- tail(logs, 1L)[[1]]
  payload <- memDecompress(jsonlite::base64_dec(entry$metaskill_report_block_b64), "gzip")
  check(identical(payload, content), "No-trailing-newline semantic block preserved exactly")
  check(identical(bytes(semantic_path(case)), content), "Original report unchanged")
})

test("metaskill_activation_log_false_and_custom_phase", {
  case <- new_case("customphase"); init(case)
  log <- file.path(case$out, "analysis_log.jsonl"); before <- bytes(log)
  run(case, "metaskill_runner", c("--meta", "bespoke", "--phase", "review", "--log", "FALSE"), cwd = case$out)
  check(identical(bytes(log), before), "Optional legacy log unchanged")
  event <- audit(case$out, "metaskill_runner")
  check(identical(event$request$request$phase, "review"), "Custom lifecycle phase preserved")
  check(grepl("phase 'review' recorded", paste(readLines(file.path(event$path, "output.md"), warn = FALSE), collapse = "\n"), fixed = TRUE), "Custom phase not mislabeled as activation")
})

test("metaskill_direct_source_without_manifest", {
  case <- new_case("direct-meta")
  run(case, "metaskill_runner", c("--rds", case$input, "--meta", "bespoke", "--notes", "Source: /private/study.sav"))
  check(!file.exists(file.path(case$project, "nlss-workspace.yml")), "Legacy direct-source route does not invent initialization")
  event <- audit(case$out, "metaskill_runner")
  ref <- event$request$request$dataset
  check(identical(sha(file.path(case$project, ref$snapshot_path)), ref$data_sha256), "Exact reference uses resolved project root")
  log <- paste(readLines(file.path(case$out, "analysis_log.jsonl")), collapse = "\n")
  output <- paste(readLines(file.path(case$out, "report_canonical.md"), warn = FALSE), collapse = "\n")
  check(!grepl(case$base, log, fixed = TRUE) && !grepl("/private/study.sav", log, fixed = TRUE), "Source and embedded prose paths masked without manifest")
  check(grepl("<external>/study.sav", output, fixed = TRUE), "Canonical prose path masked")
  check(!dir.exists(file.path(case$project, ".publication-lock")), "Project-wide publication lock released")
})

test("lifecycle_locked_and_symlink_projection_preflight", {
  case <- new_case("safety"); init(case); paths <- projections(case); before <- lapply(paths, bytes)
  lock <- file.path(case$project, ".publication-lock"); dir.create(lock)
  run(case, "init_workspace", c("--rds", case$input), ok = FALSE)
  run(case, "metaskill_runner", c("--meta", "sample-description"), ok = FALSE, cwd = case$project)
  check(identical(lapply(paths, bytes), before), "Locked projections unchanged")
  unlink(lock, recursive = TRUE)
  report <- file.path(case$out, "report_canonical.md"); outside <- file.path(case$base, "outside.md")
  check(file.rename(report, outside), "Move owned fixture report")
  check(file.symlink(outside, report), "Create owned symlink fixture")
  prior <- bytes(outside)
  run(case, "init_workspace", c("--rds", case$input), ok = FALSE)
  run(case, "metaskill_runner", c("--meta", "sample-description"), ok = FALSE, cwd = case$project)
  check(identical(bytes(outside), prior), "Outside report untouched")
})

test("lifecycle_symlink_workspace_root_precedes_import", {
  case <- new_case("root-alias")
  outside <- file.path(case$base, "outside"); dir.create(outside)
  writeLines("Preserve outside root.", file.path(outside, "sentinel"))
  check(file.symlink(outside, case$project), "Create isolated workspace alias")
  before <- bytes(file.path(outside, "sentinel"))
  run(case, "init_workspace", c("--rds", case$input), ok = FALSE)
  run(case, "metaskill_runner", c("--rds", case$input, "--meta", "sample-description"), ok = FALSE)
  check(identical(list.files(outside), "sentinel") && identical(bytes(file.path(outside, "sentinel")), before), "No import or lifecycle write through configured alias")
})

for (module in c("init_workspace", "metaskill_runner")) for (mode in c("throw", "false")) local({ selected <- module; fault_mode <- mode
  test(paste0("lifecycle_", selected, "_late_log_failure_rollback_", fault_mode), {
    case <- new_case(paste0("fault-", selected), list(logging = list(include_checksum = TRUE))); init(case)
    report <- semantic_path(case); writeLines("# Authored report\nA considered interpretation.", report)
    paths <- c(projections(case), report); before <- lapply(paths, bytes)
    fault_repo <- file.path(case$base, "fault-repo"); dir.create(fault_repo)
    check(all(file.copy(file.path(frozen, c("scripts", "assets", "SKILL.md")), fault_repo, recursive = TRUE)), "Copy isolated fault installation")
    bootstrap <- file.path(fault_repo, "scripts/R/lib/bootstrap.R")
    writeLines(c(readLines(bootstrap), ".nlss_fixture_bootstrap <- nlss_bootstrap",
      "nlss_bootstrap <- function(envir = parent.frame(), ...) {",
      "  .nlss_fixture_bootstrap(envir, ...)",
      "  original_append <- get('append_analysis_log', envir = envir)",
      paste0("  assign('append_analysis_log', function(...) { original_append(...); ",
        if (fault_mode == "throw") "stop('Injected lifecycle log failure')" else "invisible(FALSE)", " }, envir = envir)"),
      "}"), bootstrap)
    args <- if (selected == "init_workspace") c("--rds", case$input) else c("--meta", "sample-description", "--phase", "finalization", "--intent", "interpret")
    run(case, selected, args, ok = FALSE, cwd = case$project, source_repo = fault_repo)
    check(identical(lapply(paths, bytes), before), "Every live projection and authored report restored byte-exact")
    out <- if (selected == "init_workspace") case$project else case$out
    pending <- list.dirs(file.path(out, "utility-runs"), recursive = FALSE, full.names = TRUE)
    pending <- pending[startsWith(basename(pending), ".pending-")]
    check(length(pending) == 1L, "Failed event preserved once")
    failed <- json(file.path(pending, "result.json"))
    check(identical(failed$status, "failed") && identical(failed$error$projections_restored, TRUE), "Truthful failed publication status")
    check(!file.exists(file.path(pending, "output.md")), "Failed attempt has no successful output")
    check(!dir.exists(file.path(case$project, ".publication-lock")), "Owned project lock released")
  })
})

test("lifecycle_representative_sav_mi_semantic_end_to_end", {
  case <- new_case("end-to-end", list(logging = list(include_checksum = TRUE)))
  index <- 1:36
  frame <- data.frame(x = sin(index) + index / 8, z = cos(index / 3), y = 2 + index / 4 + sin(index * 2))
  frame$x[c(3, 11, 24)] <- 99; frame$y[c(7, 15, 30)] <- NA_real_
  frame$x <- haven::labelled_spss(frame$x, labels = c("Not answered" = 99), na_values = 99, label = "Predictor label")
  source <- file.path(case$base, "study.sav"); haven::write_sav(frame, source); source_hash <- sha(source)
  run(case, "init_workspace", c("--sav", source, "--dataset-name", "sample"))
  manifest <- yaml::read_yaml(file.path(case$project, "nlss-workspace.yml"))
  original <- manifest$datasets[[1]]$dataset
  check(identical(original$source_sha256, source_hash), "Original SAV source identity recorded")
  binding <- json(file.path(case$out, "import.json"))
  check(identical(sha(file.path(case$project, binding$source_copy)), source_hash), "Preserved SAV is byte-exact")
  dictionary <- json(file.path(case$out, "dictionary.json"))
  check(identical(dictionary$columns$x$variable_label, "Predictor label"), "SAV label survived initialization")
  dirs <- function() list.dirs(file.path(case$out, "runs"), recursive = FALSE, full.names = TRUE)
  statistical <- function(module, arguments, expected_module = module) {
    before <- dirs(); run(case, module, arguments, cwd = case$project)
    added <- setdiff(dirs(), before); check(length(added) == 1L, "Exactly one statistical run published")
    request <- json(file.path(added, "request.json")); result <- json(file.path(added, "result.json"))
    check(identical(result$status, "completed") && identical(result$module, expected_module), "Statistical stage completed under correct module")
    for (item in result$artifacts) check(identical(sha(file.path(added, item$path)), item$sha256), "Statistical artifact hash verified")
    list(path = added, request = request, result = result)
  }
  descriptives <- statistical("descriptive_stats", c("--vars", "x,y"))
  check(identical(descriptives$request$dataset$version_id, original$version_id), "Descriptives use exact imported version")
  transformed <- statistical("data_transform", c("--calc", "aux=z+1"))
  transformed_ref <- transformed$result$results$data_change$output
  check(!identical(transformed_ref$version_id, original$version_id), "Transformation records a distinct output version")
  imputed <- statistical("impute", c("--engine", "mice", "--vars", "x,y,z", "--m", "2", "--maxit", "2", "--seed", "717"))
  check(identical(imputed$request$dataset$version_id, transformed_ref$version_id), "MICE input is transformed working version")
  artifact <- imputed$result$results$imputation_artifact
  mids_path <- file.path(case$project, artifact$path)
  check(inherits(readRDS(mids_path), "mids") && readRDS(mids_path)$m == 2L, "Multiple stochastic completions retained")
  check(identical(sha(mids_path), artifact$sha256), "Exact mids artifact hash")
  pooled <- statistical("mi_regression", c("--mids", mids_path, "--formula", "y ~ x + z"))
  check(isTRUE(pooled$result$results$inference_pooled) && pooled$result$results$m == 2L, "Inference uses MI pooling, not single completion")
  check(length(pooled$result$results$coefficients_df) > 0L && file.exists(file.path(pooled$path, "pooled.rds")), "Machine-readable coefficients and raw pooled object preserved")
  current_hash <- sha(file.path(case$out, "sample.parquet"))
  for (stage in list(descriptives, pooled)) {
    replayed <- statistical("replay_run", c("--request", file.path(stage$path, "request.json")), stage$request$module)
    check(identical(replayed$request$replay_of, stage$request$run_id), "Statistical replay bound to original request")
    check(identical(sha(file.path(case$out, "sample.parquet")), current_hash), "Replay does not activate old working data")
  }
  semantic <- charToRaw(enc2utf8("# Research synthesis\r\n\r\nAn authored integration of design, missing-data assumptions, pooled uncertainty, and alternative explanations. München.\r\n"))
  authored <- semantic_path(case); writeBin(semantic, authored)
  run(case, "metaskill_runner", c("--meta", "sample-description", "--phase", "finalization", "--intent", "interpret", "--synopsis", "Researcher-authored integration, not a template validity claim."), cwd = case$project)
  event <- audit(case$out, "metaskill_runner")
  check(identical(bytes(authored), semantic), "Finalization leaves authored report exact")
  log <- file.path(case$out, "analysis_log.jsonl"); log_hash <- sha(log)
  reconstructed <- file.path(case$base, "reconstructed")
  run(case, "reconstruct_reports", c(log, "--out-dir", reconstructed), cwd = case$project, extra_env = list(LC_ALL = "C"))
  reports <- list.files(reconstructed, "^report_[0-9].*_reconstructed[.]md$", full.names = TRUE)
  check(length(reports) == 1L && identical(bytes(reports[1]), semantic), "Reconstructed semantic report preserves exact UTF-8/CRLF bytes")
  run(case, "check_integrity", log, cwd = case$project)
  check(identical(sha(log), log_hash) && identical(bytes(authored), semantic), "Read-only reconstruction/integrity leave source reports/log unchanged")
  check(identical(sha(source), source_hash) && identical(sha(file.path(case$project, binding$source_copy)), source_hash), "Original SAV and preserved source remain unchanged end-to-end")
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
