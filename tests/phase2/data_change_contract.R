#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Isolated mutation/publication fault injection. All overrides stay in memory;
# each case owns its dataset and never rewrites repository config or templates.
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
repo <- normalizePath(file.path(dirname(script), "../.."), winslash = "/")
root <- Sys.getenv("NLSS_TEST_ROOT", tempfile("nlss-data-change-contract-"))
dir.create(root, recursive = TRUE, showWarnings = FALSE)
root <- normalizePath(root, winslash = "/")
Sys.unsetenv("NLSS_REPLAY_REQUEST")
args <- commandArgs(TRUE)
match_at <- match("--match", args)
case_match <- if (!is.na(match_at)) {
  if (match_at == length(args)) stop("--match requires a regular expression.")
  args[[match_at + 1L]]
} else Sys.getenv("NLSS_TEST_MATCH", "")
selected <- function(name) !nzchar(case_match) || grepl(case_match, name)
checks <- 0L
check <- function(ok, label) {
  if (!isTRUE(ok)) stop(label, call. = FALSE)
  checks <<- checks + 1L
  cat("[PASS] ", label, "\n", sep = "")
}
bytes <- function(path) {
  if (!file.exists(path) || dir.exists(path)) return(NULL)
  readBin(path, "raw", file.info(path)$size)
}
new_case <- function(label) {
  base <- tempfile(paste0(label, "-"), tmpdir = root)
  dir.create(base); setwd(base)
  env <- new.env(parent = globalenv())
  source(file.path(repo, "scripts/R/lib/bootstrap.R"), local = env)
  env$nlss_bootstrap(env)
  env$source_lib("data_change.R")
  config <- env$get_builtin_config()
  config$defaults$output_dir <- file.path(base, "project")
  env$config_env$config <- config
  input <- file.path(base, "sample.csv")
  write.csv(data.frame(id = 1:4, x = c(1, 3, NA, 7)), input, row.names = FALSE)
  before <- env$load_dataframe(list(csv = input))
  out <- env$get_workspace_out_dir(before)
  targets <- c(working = file.path(out, "sample.parquet"),
    dictionary = file.path(out, "dictionary.json"), codebook = file.path(out, "codebook.md"),
    report = file.path(out, "report_canonical.md"), log = file.path(out, "analysis_log.jsonl"),
    manifest = file.path(dirname(out), "nlss-workspace.yml"), binding = file.path(out, "import.json"))
  writeLines("Existing transformation report: äöü", targets[["report"]], useBytes = TRUE)
  writeLines('{"module":"existing","results":{"value":1}}', targets[["log"]])
  reference <- attr(before, "nlss_dataset_ref")
  input_paths <- file.path(dirname(out), c(reference$snapshot_path, reference$dictionary_path))
  list(env = env, before = before, out = out, base = base, targets = targets,
    prior = lapply(targets, bytes), reference = reference,
    input_paths = input_paths, input_bytes = lapply(input_paths, bytes), state = new.env())
}
assert_released <- function(case, label, foreign_import = FALSE, foreign_analysis = FALSE, foreign_publication = FALSE) {
  check(identical(dir.exists(file.path(case$out, ".analysis-lock")), foreign_analysis),
    paste(label, if (foreign_analysis) "foreign analysis lock retained" else "analysis lock released"))
  check(identical(dir.exists(file.path(case$out, ".import-lock")), foreign_import),
    paste(label, if (foreign_import) "foreign import lock retained" else "import lock released"))
  check(identical(dir.exists(file.path(dirname(case$out), ".publication-lock")), foreign_publication),
    paste(label, if (foreign_publication) "foreign publication lock retained" else "publication lock released"))
}
terminal_result <- function(case, label) {
  terminal <- list.files(file.path(case$out, "runs"), "^result[.]json$", full.names = TRUE, recursive = TRUE)
  # list.files intentionally does not include .pending-* runs: only published
  # directories can establish the completed/failed terminal outcome.
  check(length(terminal) == 1L, paste(label, "one published terminal bundle"))
  check(!startsWith(basename(dirname(terminal)), "."), paste(label, "terminal bundle is not pending"))
  list(result = jsonlite::read_json(terminal, simplifyVector = FALSE), directory = dirname(terminal))
}
run <- function(case, mutate = function(...) NULL, noop = FALSE, metadata_only = FALSE) {
  e <- case$env
  e$nlss_run_main("data_transform", function() {
    e$nlss_begin_run("data_transform", case$before, list(log = TRUE))
    check(dir.exists(file.path(case$out, ".analysis-lock")) &&
      dir.exists(file.path(case$out, ".import-lock")), "data change owns analysis and import locks during computation")
    after <- case$before
    if (!noop && !metadata_only) after$x <- after$x + 10
    if (metadata_only) {
      attr(after, "nlss_import_contract")$columns$x$variable_label <- "Updated label without changed values"
      attr(after, "nlss_labels")$variables$x <- "Updated label without changed values"
    }
    e$nlss_resolve_request(list(transform = if (noop) "identity" else "x + 10"),
      design = list(replay = list(eligible = TRUE, reason = "deterministic boundary fixture")))
    change <- e$nlss_prepare_data_change(case$before, after)
    case$state$change <- change
    case$state$output_paths <- file.path(dirname(case$out),
      c(change$output$snapshot_path, change$output$dictionary_path))
    case$state$output_bytes <- lapply(case$state$output_paths, bytes)
    e$nlss_set_result(list(data_change = change))
    e$nlss_stage_report(case$targets[["report"]], "Data transformation",
      "Table 1. Changes\n\n| Variable | Changed |\n| --- | ---: |\n| x | 3 |", "Deterministic publication fixture.")
    e$nlss_stage_log(case$out, "data_transform", "boundary fixture", "boundary fixture",
      list(data_change = change))
    mutate(e, change)
  })
}

for (kind in c("success", "noop", "metadata_only", "report", "log_manifest", "copy_working", "copy_dictionary",
               "copy_codebook", "copy_corrupt", "bundle_rename", "backup_copy", "backup_corrupt",
               "backup_collision", "current_changed", "binding_changed", "output_tamper", "lineage_tamper")) {
  if (!selected(paste0("transform_", kind))) next
  case <- new_case(kind); e <- case$env
  fault_hit <- FALSE
  if (kind == "report") e$append_nlss_report <- function(...) {
    fault_hit <<- TRUE
    writeLines("partial-report", case$targets[["report"]]); stop("injected report failure")
  }
  if (kind == "log_manifest") e$append_analysis_log <- function(...) {
    fault_hit <<- TRUE
    writeLines("partial-log", case$targets[["log"]])
    writeLines("partial-manifest", case$targets[["manifest"]])
    stop("injected log/manifest failure")
  }
  if (kind %in% c("copy_working", "copy_dictionary", "copy_codebook", "copy_corrupt")) {
    target <- case$targets[[if (kind == "copy_corrupt") "working" else sub("^copy_", "", kind)]]
    e$file.copy <- function(from, to, ...) {
      if (!fault_hit && identical(to, target)) {
        fault_hit <<- TRUE; writeBin(charToRaw("partial-copy"), to)
        return(kind == "copy_corrupt")
      }
      base::file.copy(from, to, ...)
    }
  }
  if (kind == "bundle_rename") e$file.rename <- function(from, to) {
    if (!fault_hit && startsWith(basename(from), ".pending-") && dir.exists(from) &&
        identical(dirname(from), dirname(to))) {
      fault_hit <<- TRUE; return(FALSE)
    }
    base::file.rename(from, to)
  }
  mutation <- function(e, change) {
    if (kind %in% c("backup_copy", "backup_corrupt")) {
      backup <- file.path(case$out, change$backup_path)
      e$file.copy <- function(from, to, ...) {
        if (!fault_hit && identical(to, backup)) {
          fault_hit <<- TRUE; writeBin(charToRaw("incomplete-backup"), to)
          return(kind == "backup_corrupt")
        }
        base::file.copy(from, to, ...)
      }
    }
    if (kind == "backup_collision") {
      backup <- file.path(case$out, change$backup_path)
      dir.create(dirname(backup), recursive = TRUE, showWarnings = FALSE)
      writeBin(charToRaw("pre-existing-backup"), backup)
      case$state$collision <- backup
    }
    if (kind == "current_changed") {
      changed <- case$before; changed$x <- c(100, 200, 300, 400)
      e$write_parquet_data(changed, case$targets[["working"]])
      case$state$external_bytes <- bytes(case$targets[["working"]])
    }
    if (kind == "binding_changed") {
      binding <- e$read_import_json(case$targets[["binding"]])
      binding$external_change <- TRUE
      e$write_import_json(binding, case$targets[["binding"]])
      case$state$external_bytes <- bytes(case$targets[["binding"]])
    }
    if (kind == "output_tamper") {
      writeBin(charToRaw("tampered-output"), case$state$output_paths[1])
      case$state$output_bytes <- lapply(case$state$output_paths, bytes)
    }
    if (kind == "lineage_tamper") writeLines('{"applied":true,"output":"unverified"}',
      file.path(e$nlss_run_context$staging, "data-change.json"))
  }
  error <- tryCatch({ run(case, mutation, noop = kind == "noop", metadata_only = kind == "metadata_only"); NULL }, error = conditionMessage)
  success <- kind %in% c("success", "noop", "metadata_only")
  check(if (success) is.null(error) else !is.null(error), paste(kind, "exit outcome", if (is.null(error)) "" else error))
  if (kind %in% c("report", "log_manifest", "copy_working", "copy_dictionary", "copy_codebook", "copy_corrupt", "bundle_rename", "backup_copy", "backup_corrupt")) {
    check(fault_hit, paste(kind, "intended fault point reached"))
  }
  terminal <- terminal_result(case, kind); result <- terminal$result
  check(identical(result$status, if (success) "completed" else "failed"), paste(kind, "truthful terminal status"))
  for (artifact in result$artifacts) check(identical(artifact$sha256,
    digest::digest(file = file.path(terminal$directory, artifact$path), algo = "sha256")),
    paste(kind, "retained artifact hash", artifact$path))
  assert_released(case, kind)
  check(identical(lapply(case$input_paths, bytes), case$input_bytes), paste(kind, "immutable input bytes preserved"))
  check(identical(lapply(case$state$output_paths, bytes), case$state$output_bytes), paste(kind, "prepared immutable output retained"))
  check(identical(bytes(case$targets[["binding"]]), if (kind == "binding_changed") case$state$external_bytes else case$prior$binding),
    paste(kind, "import binding not rewritten by transformation"))
  if (success) {
    change <- result$results$data_change
    check(identical(change$input, result$dataset), paste(kind, "result identifies exact before version"))
    check(identical(jsonlite::read_json(file.path(terminal$directory, "data-change.json")), change),
      paste(kind, "authenticated lineage matches successful result"))
    check(isTRUE(e$nlss_verify_dataset(change$output, dirname(case$out))), paste(kind, "output reference verifies"))
    check(identical(bytes(case$targets[["working"]]), bytes(case$state$output_paths[1])), paste(kind, "working bytes match recorded output"))
    check(identical(bytes(case$targets[["dictionary"]]), bytes(case$state$output_paths[2])), paste(kind, "current dictionary matches output version"))
    values <- e$read_parquet_data(case$targets[["working"]])$x
    check(identical(values, if (kind %in% c("noop", "metadata_only")) case$before$x else case$before$x + 10), paste(kind, "published values correct"))
    if (kind == "noop") {
      check(!isTRUE(change$applied), "no-op does not claim an applied mutation")
      check(identical(change$input, change$output), "no-op reuses exact input version")
      check(!length(list.files(file.path(case$out, "backup"))), "no-op creates no backup")
      check(identical(lapply(case$targets[c("working", "dictionary", "codebook")], bytes),
        case$prior[c("working", "dictionary", "codebook")]), "no-op preserves working previews byte for byte")
    } else {
      check(isTRUE(change$applied), "completed change records applied state")
      check(identical(bytes(file.path(case$out, change$backup_path)), case$prior$working), "verified backup preserves previous working bytes")
      check(!identical(change$input$version_id, change$output$version_id), "changed data has distinct immutable version")
      if (kind == "metadata_only") {
        dictionary <- jsonlite::read_json(case$targets[["dictionary"]])
        check(identical(dictionary$columns$x$variable_label, "Updated label without changed values"),
          "metadata-only change publishes actual updated dictionary")
        check(!isTRUE(change$unchanged), "metadata-only change is not mislabeled as a no-op")
      }
    }
  } else {
    expected <- case$prior
    if (kind == "current_changed") expected$working <- case$state$external_bytes
    if (kind == "binding_changed") expected$binding <- case$state$external_bytes
    check(identical(lapply(case$targets, bytes), expected), paste(kind, "exact protected target restoration; concurrent bytes not overwritten"))
    check(is.null(result$results), paste(kind, "failed run claims no successful data change"))
    if (kind != "lineage_tamper") {
      lineage <- jsonlite::read_json(file.path(terminal$directory, "data-change.json"))
      check(is.null(lineage$applied) && identical(lineage$publication_status, "failed_not_committed"),
        paste(kind, "failed lineage makes no successful application claim"))
    }
    check(!file.exists(file.path(terminal$directory, "output.md")), paste(kind, "no successful Markdown"))
    if (kind == "backup_collision") check(identical(bytes(case$state$collision), charToRaw("pre-existing-backup")), "existing backup collision preserved")
    if (kind %in% c("backup_copy", "backup_corrupt")) check(!file.exists(file.path(case$out, case$state$change$backup_path)),
      paste(kind, "incomplete backup is not left as a valid named Parquet"))
    if (kind %in% c("report", "log_manifest", "copy_working", "copy_dictionary", "copy_codebook", "copy_corrupt", "bundle_rename")) {
      check(identical(bytes(file.path(case$out, case$state$change$backup_path)), case$prior$working),
        paste(kind, "verified before-data backup retained after rollback"))
    }
    if (kind == "lineage_tamper") check(identical(result$artifacts[["data-change.json"]]$status, "changed_after_registration") &&
      !identical(result$artifacts[["data-change.json"]]$sha256, result$artifacts[["data-change.json"]]$expected_sha256),
      "tampered lineage is explicitly distinguished from registered bytes")
  }
}

for (kind in c("import_lock", "analysis_lock", "publication_lock", "loaded_input_changed",
               "symlink_working", "symlink_dictionary", "symlink_codebook", "symlink_backup",
               "symlink_report", "symlink_log", "symlink_manifest", "symlink_binding")) {
  if (!selected(paste0("transform_", kind))) next
  case <- new_case(kind); e <- case$env
  outside <- file.path(case$base, "outside")
  dir.create(outside)
  if (kind == "import_lock") dir.create(file.path(case$out, ".import-lock"))
  if (kind == "analysis_lock") dir.create(file.path(case$out, ".analysis-lock"))
  if (kind == "publication_lock") dir.create(file.path(dirname(case$out), ".publication-lock"))
  if (kind == "loaded_input_changed") {
    changed <- case$before; changed$x <- rep(99, 4)
    e$write_parquet_data(changed, case$targets[["working"]])
  }
  if (startsWith(kind, "symlink_")) {
    name <- sub("^symlink_", "", kind)
    target <- if (name == "backup") file.path(case$out, "backup") else case$targets[[name]]
    destination <- if (name == "backup") outside else file.path(outside, basename(target))
    if (file.exists(target)) stopifnot(file.rename(target, destination))
    stopifnot(file.symlink(destination, target))
  }
  before_attempt <- lapply(case$targets, bytes)
  outside_files <- list.files(outside, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  outside_bytes <- lapply(outside_files, bytes)
  error <- tryCatch({ run(case); NULL }, error = conditionMessage)
  check(!is.null(error), paste(kind, "refused"))
  check(identical(lapply(case$targets, bytes), before_attempt), paste(kind, "working data and projections untouched"))
  check(identical(list.files(outside, full.names = TRUE, all.files = TRUE, no.. = TRUE), outside_files) &&
    identical(lapply(outside_files, bytes), outside_bytes), paste(kind, "no external writes"))
  check(identical(lapply(case$input_paths, bytes), case$input_bytes), paste(kind, "immutable input untouched"))
  terminal <- list.files(file.path(case$out, "runs"), "^result[.]json$", recursive = TRUE, full.names = TRUE)
  check(all(vapply(terminal, function(path) identical(jsonlite::read_json(path)$status, "failed"), logical(1))),
    paste(kind, "no successful run published"))
  lineage_paths <- list.files(file.path(case$out, "runs"), "^data-change[.]json$", recursive = TRUE,
    full.names = TRUE, all.files = TRUE)
  for (path in lineage_paths) {
    lineage <- jsonlite::read_json(path)
    check(is.null(lineage$applied) && identical(lineage$publication_status, "failed_not_committed"),
      paste(kind, "failed or pending lineage makes no successful application claim"))
  }
  assert_released(case, kind, foreign_import = kind == "import_lock",
    foreign_analysis = kind == "analysis_lock", foreign_publication = kind == "publication_lock")
}

for (mode in c("parquet", "active_dataset", "dataset_directory")) {
  if (!selected(paste0("transform_direct_load_", mode))) next
  case <- new_case(paste0("direct_load_", mode)); e <- case$env
  e$update_workspace_manifest(dirname(case$out), data.frame(dataset = "sample",
    copy_path = case$targets[["working"]], source_path = file.path(case$base, "sample.csv"), type = "csv"))
  setwd(if (mode == "dataset_directory") case$out else dirname(case$out))
  opts <- if (mode == "parquet") list(parquet = case$targets[["working"]]) else list()
  error <- tryCatch({ run(case, function(e, change) {
    # A direct reader previously refreshed dictionary/codebook before checking
    # the transformation's import lock. Prove the public load paths honor it.
    protected <- lapply(case$targets, bytes)
    refused <- tryCatch({ e$load_dataframe(opts); NULL }, error = conditionMessage)
    check(!is.null(refused) && grepl("locked", refused, ignore.case = TRUE), paste(mode, "direct reader rejects held import lock"))
    check(dir.exists(file.path(case$out, ".import-lock")), paste(mode, "reader does not remove transformation import lock"))
    check(identical(lapply(case$targets, bytes), protected), paste(mode, "blocked reader leaves working data and previews untouched"))
  }); NULL }, error = conditionMessage)
  check(is.null(error), paste(mode, "transformation still commits after rejected reader", if (is.null(error)) "" else error))
  assert_released(case, mode)
  current <- bytes(case$targets[["working"]])
  # Mark only the owned disposable previews stale, then verify refresh reads
  # the newly published Parquet/dictionary, not the previous input version.
  writeLines("stale dictionary preview", case$targets[["dictionary"]])
  writeLines("stale codebook preview", case$targets[["codebook"]])
  loaded <- e$load_dataframe(opts)
  check(identical(loaded$x, case$before$x + 10), paste(mode, "direct reader succeeds after release with transformed values"))
  check(identical(attr(loaded, "nlss_dataset_ref"), case$state$change$output), paste(mode, "direct reader identifies committed output version"))
  check(identical(bytes(case$targets[["working"]]), current), paste(mode, "preview refresh does not rewrite working data"))
  check(identical(bytes(case$targets[["dictionary"]]), bytes(case$state$output_paths[2])), paste(mode, "dictionary refreshed from committed version"))
  check(grepl(case$state$change$output$version_id,
    paste(readLines(case$targets[["codebook"]], warn = FALSE), collapse = "\n"), fixed = TRUE),
    paste(mode, "codebook refreshed with committed version"))
  assert_released(case, paste(mode, "after direct read"))
  actual_reader <- e$read_parquet_data
  e$read_parquet_data <- function(...) stop("injected direct read failure")
  protected <- lapply(case$targets, bytes)
  failed <- tryCatch({ e$load_dataframe(opts); NULL }, error = conditionMessage)
  e$read_parquet_data <- actual_reader
  check(identical(failed, "injected direct read failure"), paste(mode, "reader failure reached while owning import lock"))
  check(identical(lapply(case$targets, bytes), protected), paste(mode, "failed direct read leaves working data and previews unchanged"))
  assert_released(case, paste(mode, "after reader failure"))
}
# Wave 12 exercises the same publication machinery with fewer output rows.
# The fixture deliberately uses only base-R row selection: numerical method
# acceptance belongs to run_missings_tests.R, not to this boundary test.
run_missing <- function(case, mutate = function(...) NULL, rows = c(1L, 2L, 4L),
                        source_rows = rows, module = "missings", design_rows = rows) {
  e <- case$env
  e$nlss_run_main(module, function() {
    replay <- !is.null(e$nlss_run_context$replay)
    before <- if (replay) e$nlss_load_input(list()) else case$before
    e$nlss_begin_run(module, before, list(log = TRUE, method = "listwise"))
    check(dir.exists(file.path(case$out, ".analysis-lock")), "missing-data change owns analysis lock during computation")
    check(identical(dir.exists(file.path(case$out, ".import-lock")), !replay),
      "missing-data replay does not acquire an import mutation lock")
    after <- before[rows, , drop = FALSE]
    # Match a data-changing module's canonical output boundary: runtime load
    # paths/hash bookkeeping are not data attributes or reproducible output.
    attributes(after) <- attributes(after)[intersect(c("names", "row.names", "class",
      "nlss_labels", "nlss_import_contract"), names(attributes(after)))]
    e$nlss_resolve_request(list(method = "listwise"), design = list(source_rows = design_rows,
      replay = list(eligible = TRUE, reason = "Deterministic base-R row selection boundary fixture")))
    change <- e$nlss_prepare_data_change(before, after, source_rows = source_rows)
    case$state$change <- change
    case$state$output_paths <- file.path(dirname(case$out),
      c(change$output$snapshot_path, change$output$dictionary_path))
    case$state$output_bytes <- lapply(case$state$output_paths, bytes)
    e$nlss_set_result(list(source_rows = rows, data_change = change))
    e$nlss_stage_report(case$targets[["report"]], "Missing data handling",
      "Table 1. Retained rows\n\n| Rows |\n| ---: |\n| 3 |", "Deterministic row-selection fixture.")
    e$nlss_stage_log(case$out, module, "row-selection boundary fixture", "row-selection boundary fixture",
      list(source_rows = rows, data_change = change))
    mutate(e, change)
  })
}

for (kind in c("success", "zero_rows", "all_rows", "report", "log_manifest", "copy_working",
               "copy_dictionary", "copy_codebook", "bundle_rename", "current_changed", "lineage_tamper")) {
  label <- paste0("missings_", kind)
  if (!selected(label)) next
  case <- new_case(label); e <- case$env
  rows <- if (kind == "zero_rows") integer() else if (kind == "all_rows") 1:4 else c(1L, 2L, 4L)
  fault_hit <- FALSE
  if (kind == "report") e$append_nlss_report <- function(...) {
    fault_hit <<- TRUE
    writeLines("partial-missing-data-report", case$targets[["report"]]); stop("injected missing-data report failure")
  }
  if (kind == "log_manifest") e$append_analysis_log <- function(...) {
    fault_hit <<- TRUE
    writeLines("partial-missing-data-log", case$targets[["log"]])
    writeLines("partial-missing-data-manifest", case$targets[["manifest"]])
    stop("injected missing-data log/manifest failure")
  }
  if (kind %in% c("copy_working", "copy_dictionary", "copy_codebook")) {
    target <- case$targets[[sub("^copy_", "", kind)]]
    e$file.copy <- function(from, to, ...) {
      if (!fault_hit && identical(to, target)) {
        fault_hit <<- TRUE; writeBin(charToRaw("partial-missing-data-copy"), to); return(FALSE)
      }
      base::file.copy(from, to, ...)
    }
  }
  if (kind == "bundle_rename") e$file.rename <- function(from, to) {
    if (!fault_hit && startsWith(basename(from), ".pending-") && dir.exists(from) &&
        identical(dirname(from), dirname(to))) {
      fault_hit <<- TRUE; return(FALSE)
    }
    base::file.rename(from, to)
  }
  error <- tryCatch({ run_missing(case, rows = rows, mutate = function(e, change) {
    if (kind == "current_changed") {
      changed <- case$before; changed$x <- rep(99, 4)
      e$write_parquet_data(changed, case$targets[["working"]])
      case$state$external_bytes <- bytes(case$targets[["working"]])
    }
    if (kind == "lineage_tamper") writeLines('{"source_rows":[4,2,1],"applied":true}',
      file.path(e$nlss_run_context$staging, "data-change.json"))
  }); NULL }, error = conditionMessage)
  success <- kind %in% c("success", "zero_rows", "all_rows")
  check(if (success) is.null(error) else !is.null(error), paste(label, "exit outcome", if (is.null(error)) "" else error))
  if (kind %in% c("report", "log_manifest", "copy_working", "copy_dictionary", "copy_codebook", "bundle_rename"))
    check(fault_hit, paste(label, "intended fault point reached"))
  terminal <- terminal_result(case, label); result <- terminal$result
  check(identical(result$status, if (success) "completed" else "failed"), paste(label, "truthful terminal status"))
  for (artifact in result$artifacts) check(identical(artifact$sha256,
    digest::digest(file = file.path(terminal$directory, artifact$path), algo = "sha256")),
    paste(label, "retained artifact hash", artifact$path))
  assert_released(case, label)
  check(identical(lapply(case$input_paths, bytes), case$input_bytes), paste(label, "immutable input bytes preserved"))
  check(identical(lapply(case$state$output_paths, bytes), case$state$output_bytes), paste(label, "prepared immutable output retained"))
  check(identical(bytes(case$targets[["binding"]]), case$prior$binding), paste(label, "import binding unchanged"))
  if (success) {
    change <- result$results$data_change
    request <- jsonlite::read_json(file.path(terminal$directory, "request.json"))
    check(identical(change$input, result$dataset), paste(label, "exact input version retained"))
    check(identical(change$observation_basis, "input_version_rows"), paste(label, "row mapping explicitly refers to input version"))
    check(identical(as.integer(unlist(change$source_rows)), rows) &&
      identical(change$source_rows, request$design$source_rows), paste(label, "authenticated lineage matches resolved source rows"))
    check(identical(jsonlite::read_json(file.path(terminal$directory, "data-change.json")), change),
      paste(label, "authenticated lineage matches successful result"))
    check(isTRUE(e$nlss_verify_dataset(change$output, dirname(case$out))), paste(label, "output reference verifies"))
    actual <- e$read_parquet_data(case$targets[["working"]])
    check(identical(actual$id, case$before$id[rows]) && identical(actual$x, case$before$x[rows]),
      paste(label, "published rows retain their actual original identities and values"))
    check(identical(bytes(case$targets[["working"]]), bytes(case$state$output_paths[1])) &&
      identical(bytes(case$targets[["dictionary"]]), bytes(case$state$output_paths[2])),
      paste(label, "published data and dictionary match immutable output"))
    if (kind == "all_rows") {
      check(isTRUE(change$unchanged) && !isTRUE(change$applied) && identical(change$input, change$output),
        "missings all-row identity preserves existing no-op contract")
      check(!length(list.files(file.path(case$out, "backup"))), "missings all-row identity creates no backup")
    } else check(identical(bytes(file.path(case$out, change$backup_path)), case$prior$working),
      paste(label, "verified backup preserves all input rows"))
  } else {
    expected <- case$prior
    if (kind == "current_changed") expected$working <- case$state$external_bytes
    check(identical(lapply(case$targets, bytes), expected), paste(label, "exact protected target restoration without overwriting concurrent bytes"))
    check(is.null(result$results) && !file.exists(file.path(terminal$directory, "output.md")),
      paste(label, "no successful result or Markdown claimed"))
    if (kind == "lineage_tamper") check(identical(result$artifacts[["data-change.json"]]$status, "changed_after_registration"),
      paste(label, "tampered lineage remains diagnostic evidence"))
    else {
      lineage <- jsonlite::read_json(file.path(terminal$directory, "data-change.json"))
      check(is.null(lineage$applied) && identical(lineage$publication_status, "failed_not_committed"),
        paste(label, "failed lineage does not claim applied deletion"))
    }
    if (kind %in% c("report", "log_manifest", "copy_working", "copy_dictionary", "copy_codebook", "bundle_rename"))
      check(identical(bytes(file.path(case$out, case$state$change$backup_path)), case$prior$working),
        paste(label, "verified full-row backup retained after rollback"))
  }
}

invalid_rows <- list(missing = NULL, duplicate = c(1L, 1L, 4L), reordered = c(4L, 2L, 1L),
  fractional = c(1, 2.5, 4), zero = c(0L, 2L, 4L), negative = c(-1L, 2L, 4L),
  outside_input = c(1L, 2L, 5L), absent = c(1L, NA_integer_, 4L),
  infinite = c(1, Inf, 4), character = c("1", "2", "4"), logical = c(TRUE, TRUE, FALSE),
  wrong_length = c(1L, 2L))
for (kind in c(names(invalid_rows), "different_request", "fractional_request", "character_request", "missing_request",
               "transform_row_change", "transform_explicit_identity")) {
  label <- paste0("missings_invalid_map_", kind)
  if (!selected(label)) next
  case <- new_case(label); e <- case$env
  rows <- if (kind == "transform_explicit_identity") 1:4 else c(1L, 2L, 4L)
  map <- if (kind %in% names(invalid_rows)) invalid_rows[[kind]] else rows
  if (kind == "transform_row_change") map <- NULL
  module <- if (startsWith(kind, "transform_")) "data_transform" else "missings"
  design_rows <- switch(kind, different_request = c(1L, 3L, 4L), fractional_request = c(1.5, 2, 4),
    character_request = c("1", "2", "4"), missing_request = NULL, rows)
  error <- tryCatch({ run_missing(case, rows = rows, source_rows = map, module = module, design_rows = design_rows); NULL }, error = conditionMessage)
  check(!is.null(error) && grepl("row|mapping", error, ignore.case = TRUE), paste(label, "rejected at source-row contract", if (is.null(error)) "" else error))
  check(identical(lapply(case$targets, bytes), case$prior), paste(label, "working data and projections unchanged"))
  check(identical(lapply(case$input_paths, bytes), case$input_bytes), paste(label, "immutable input unchanged"))
  check(!length(list.files(file.path(case$out, "backup"))), paste(label, "no deletion backup created for invalid operation"))
  terminal <- terminal_result(case, label)
  check(identical(terminal$result$status, "failed") && is.null(terminal$result$results), paste(label, "failed bundle makes no applied-change claim"))
  assert_released(case, label)
}

for (kind in c("import_lock", "analysis_lock", "publication_lock", "loaded_input_changed")) {
  label <- paste0("missings_", kind)
  if (!selected(label)) next
  case <- new_case(label); e <- case$env
  if (kind == "import_lock") dir.create(file.path(case$out, ".import-lock"))
  if (kind == "analysis_lock") dir.create(file.path(case$out, ".analysis-lock"))
  if (kind == "publication_lock") dir.create(file.path(dirname(case$out), ".publication-lock"))
  if (kind == "loaded_input_changed") {
    changed <- case$before; changed$x <- rep(99, 4)
    e$write_parquet_data(changed, case$targets[["working"]])
  }
  prior <- lapply(case$targets, bytes)
  error <- tryCatch({ run_missing(case); NULL }, error = conditionMessage)
  check(!is.null(error), paste(label, "refused"))
  check(identical(lapply(case$targets, bytes), prior), paste(label, "working data and projections untouched"))
  check(identical(lapply(case$input_paths, bytes), case$input_bytes), paste(label, "immutable input untouched"))
  terminal <- list.files(file.path(case$out, "runs"), "^result[.]json$", recursive = TRUE, full.names = TRUE)
  check(all(vapply(terminal, function(path) identical(jsonlite::read_json(path)$status, "failed"), logical(1))),
    paste(label, "no successful run published"))
  assert_released(case, label, foreign_import = kind == "import_lock", foreign_analysis = kind == "analysis_lock",
    foreign_publication = kind == "publication_lock")
}

for (kind in c("frozen_rows", "lineage_tamper", "output_tamper")) {
  label <- paste0("missings_replay_", kind)
  if (!selected(label)) next
  case <- new_case(label); e <- case$env
  run_missing(case)
  original <- terminal_result(case, label)
  request_path <- file.path(original$directory, "request.json")
  original_change <- original$result$results$data_change
  original_output <- lapply(case$state$output_paths, bytes)
  if (kind == "lineage_tamper") {
    lineage_path <- file.path(original$directory, "data-change.json")
    altered <- jsonlite::read_json(lineage_path)
    altered$source_rows <- list(4L, 2L, 1L)
    e$write_import_json(altered, lineage_path)
    altered_result <- original$result
    altered_result$artifacts[["data-change.json"]]$sha256 <- e$import_hash(lineage_path, file = TRUE)
    e$write_import_json(altered_result, file.path(original$directory, "result.json"))
  }
  if (kind == "output_tamper") writeBin(charToRaw("tampered-missing-data-output"), case$state$output_paths[1])
  # Activate a different, larger working version before replay. Frozen-input
  # replay must neither read this table nor activate the earlier three rows.
  later <- case$before; later$x <- c(101, 103, 105, 107)
  e$write_parquet_data(later, case$targets[["working"]])
  later <- e$snapshot_working_dataframe(case$targets[["working"]])
  protected <- lapply(case$targets[c("working", "dictionary", "codebook", "binding")], bytes)
  backup_paths <- list.files(file.path(case$out, "backup"), full.names = TRUE)
  backup_bytes <- lapply(backup_paths, bytes)
  before_runs <- list.files(file.path(case$out, "runs"), "^result[.]json$", recursive = TRUE, full.names = TRUE)
  Sys.setenv(NLSS_REPLAY_REQUEST = request_path)
  error <- tryCatch({ run_missing(case); NULL }, error = conditionMessage)
  Sys.unsetenv("NLSS_REPLAY_REQUEST")
  check(if (kind == "frozen_rows") is.null(error) else !is.null(error), paste(label, "replay outcome", if (is.null(error)) "" else error))
  check(identical(lapply(case$targets[c("working", "dictionary", "codebook", "binding")], bytes), protected),
    paste(label, "later working data and previews never reactivated or overwritten"))
  check(identical(list.files(file.path(case$out, "backup"), full.names = TRUE), backup_paths) &&
    identical(lapply(backup_paths, bytes), backup_bytes), paste(label, "replay creates or changes no working-data backup"))
  check(identical(lapply(case$input_paths, bytes), case$input_bytes), paste(label, "original full-row immutable input retained"))
  assert_released(case, label)
  after_runs <- list.files(file.path(case$out, "runs"), "^result[.]json$", recursive = TRUE, full.names = TRUE)
  if (kind == "frozen_rows") {
    added <- setdiff(after_runs, before_runs)
    check(length(added) == 1L, paste(label, "one new replay bundle"))
    replay <- jsonlite::read_json(added)
    change <- replay$results$data_change
    check(identical(replay$status, "completed") && !isTRUE(change$applied) && is.null(change$backup_path),
      paste(label, "completed replay explicitly nonapplying"))
    check(identical(change$input, original_change$input) && identical(change$output, original_change$output) &&
      identical(change$source_rows, original_change$source_rows), paste(label, "exact before-after lineage and retained row identities reproduced"))
    check(identical(lapply(case$state$output_paths, bytes), original_output), paste(label, "original immutable output bytes retained"))
    check(identical(bytes(file.path(dirname(added), "output.md")), bytes(file.path(original$directory, "output.md"))),
      paste(label, "deterministic per-run Markdown reproduced"))
  } else check(identical(after_runs, before_runs), paste(label, "tampered saved evidence rejected before a new run"))
}
if (selected("literal_project_path_prefix")) {
  # Project names are literal filesystem components, never regular expressions.
  # The combined name also exercises actual import/version/run publication, not
  # merely string helpers on paths that the rest of the workflow cannot use.
  case <- new_case("literal-prefix-+[](). with spaces-ü日本語"); e <- case$env
  components <- c(plus = "project+wave", brackets = "project[12]", parentheses = "project(12)",
    dots = "project.v1", spaces = "project wave 12", unicode = "Projekt ü Ä 日本語")
  for (kind in names(components)) {
    directory <- file.path(case$base, components[[kind]])
    dir.create(directory)
    internal <- file.path(directory, "nested", "value.json")
    sibling <- file.path(paste0(directory, "-sibling"), "value.json")
    check(identical(e$make_relative_path(internal, directory), "nested/value.json"),
      paste("literal prefix", kind, "internal path strips exact directory bytes"))
    check(identical(e$make_relative_path(internal, paste0(directory, "/")), "nested/value.json"),
      paste("literal prefix", kind, "trailing separator retains same internal path"))
    check(identical(e$make_relative_path(sibling, directory), e$normalize_path(sibling)),
      paste("literal prefix", kind, "similarly prefixed sibling remains external"))
  }
  check(startsWith(case$reference$snapshot_path, "sample/versions/") &&
    startsWith(case$reference$dictionary_path, "sample/versions/"),
    "literal project path imports with actual project-relative immutable references")
  check(isTRUE(e$nlss_verify_dataset(case$reference, dirname(case$out))),
    "literal project path imported immutable input verifies")
  run_missing(case)
  terminal <- terminal_result(case, "literal project path")
  change <- terminal$result$results$data_change
  check(identical(terminal$result$status, "completed") && identical(change$output_path, "sample/sample.parquet"),
    "literal project path publishes completed run with relative working-data output")
  check(isTRUE(e$nlss_verify_dataset(change$output, dirname(case$out))),
    "literal project path published immutable output verifies")
  check(identical(e$read_parquet_data(case$targets[["working"]])$id, c(1L, 2L, 4L)),
    "literal project path publishes correct retained rows")
  check(identical(lapply(case$input_paths, bytes), case$input_bytes),
    "literal project path preserves immutable input bytes")
  assert_released(case, "literal project path")
}
for (kind in c("success", "row_count", "row_order", "source_changed", "source_removed", "source_reordered",
               "explicit_rows", "report_failure", "working_copy_failure", "bundle_failure")) {
  label <- paste0("impute_", kind)
  if (!selected(label)) next
  case <- new_case(label); e <- case$env
  fault_hit <- FALSE
  if (kind == "report_failure") e$append_nlss_report <- function(...) {
    fault_hit <<- TRUE; writeLines("partial-impute-report", case$targets[["report"]]); stop("injected impute report failure")
  }
  if (kind == "working_copy_failure") e$file.copy <- function(from, to, ...) {
    if (!fault_hit && identical(to, case$targets[["working"]])) {
      fault_hit <<- TRUE; writeBin(charToRaw("partial-imputed-data"), to); return(FALSE)
    }
    base::file.copy(from, to, ...)
  }
  if (kind == "bundle_failure") e$file.rename <- function(from, to) {
    if (!fault_hit && startsWith(basename(from), ".pending-") && identical(dirname(from), dirname(to))) {
      fault_hit <<- TRUE; return(FALSE)
    }
    base::file.rename(from, to)
  }
  error <- tryCatch({
    e$nlss_run_main("impute", function() {
      e$nlss_begin_run("impute", case$before, list(engine = "simple", log = TRUE))
      check(dir.exists(file.path(case$out, ".import-lock")), paste(label, "imputation owns import lock"))
      after <- case$before
      after$x_imp <- replace(after$x, is.na(after$x), 3)
      if (kind == "row_count") after <- after[-1, , drop = FALSE]
      if (kind == "row_order") after <- after[4:1, , drop = FALSE]
      if (kind == "source_changed") after$x[1] <- 999
      if (kind == "source_removed") after$x <- NULL
      if (kind == "source_reordered") after <- after[c("x", "id", "x_imp")]
      e$nlss_resolve_request(list(engine = "simple"), list(replay = list(eligible = TRUE)))
      change <- e$nlss_prepare_data_change(case$before, after,
        source_rows = if (kind == "explicit_rows") 1:4 else NULL)
      case$state$change <- change
      e$nlss_set_result(list(data_change = change, imputation_artifact = NULL))
      e$nlss_stage_report(case$targets[["report"]], "Imputation", "Table 1\n\nOne completed value.", "Single completion.")
      e$nlss_stage_log(case$out, "impute", "boundary fixture", "boundary fixture", list(data_change = change))
    }); NULL
  }, error = conditionMessage)
  success <- identical(kind, "success")
  check(if (success) is.null(error) else !is.null(error), paste(label, "expected outcome", error))
  if (endsWith(kind, "failure")) check(fault_hit, paste(label, "intended publication fault reached"))
  terminal <- terminal_result(case, label)
  check(identical(terminal$result$status, if (success) "completed" else "failed"), paste(label, "truthful status"))
  check(identical(lapply(case$input_paths, bytes), case$input_bytes), paste(label, "immutable source retained"))
  assert_released(case, label)
  if (success) {
    output <- e$read_parquet_data(case$targets[["working"]])
    check(identical(output$id, case$before$id) && identical(output$x, case$before$x) &&
      identical(output$x_imp, c(1, 3, 3, 7)), paste(label, "only new completion column changes"))
    change <- terminal$result$results$data_change
    check(identical(bytes(file.path(case$out, change$backup_path)), case$prior$working), paste(label, "verified original backup"))
  } else {
    check(identical(lapply(case$targets, bytes), case$prior), paste(label, "all protected files restored exactly"))
    check(is.null(terminal$result$results), paste(label, "no successful result claimed"))
  }
}
if (!checks) stop("No data-change contract cases matched: ", case_match)
cat("Data change publication contract:", checks, "checks passed.\n")
