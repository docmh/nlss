# SPDX-License-Identifier: Apache-2.0
#!/usr/bin/env Rscript
# Isolated failure injection: no repository source/configuration is modified.
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]]
repo <- normalizePath(file.path(dirname(sub("^--file=", "", script_arg)), "../.."), winslash = "/")
for (package in c("yaml", "jsonlite", "arrow", "digest")) {
  if (!requireNamespace(package, quietly = TRUE)) stop("Publication tests require ", package, ".")
}
settings <- yaml::read_yaml(file.path(repo, "tests/tests.yml"))$tests
root <- Sys.getenv("NLSS_TEST_ROOT", "")
if (!nzchar(root)) root <- file.path(repo, settings$output_dir, paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-phase2-publication"))
root <- normalizePath(root, winslash = "/", mustWork = FALSE)
test_dir <- file.path(root, "tmp", "phase2-publication")
dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)
checks <- character()
check <- function(value, label) {
  if (!isTRUE(value)) stop("FAIL: ", label, call. = FALSE)
  checks <<- c(checks, paste("PASS", label))
  cat(tail(checks, 1L), "\n")
}
bytes <- function(path) {
  if (!file.exists(path)) return(NULL)
  readBin(path, "raw", n = file.info(path)$size)
}
original_wd <- getwd()
prior_replay <- Sys.getenv("NLSS_REPLAY_REQUEST", unset = NA_character_)
Sys.unsetenv("NLSS_REPLAY_REQUEST")

new_case <- function(label) {
  base <- tempfile(paste0(label, "-"), tmpdir = test_dir)
  dir.create(base)
  setwd(base)
  target <- new.env(parent = globalenv())
  source(file.path(repo, "scripts/R/lib/bootstrap.R"), local = target)
  target$nlss_bootstrap(target)
  config <- target$get_builtin_config()
  config$defaults$output_dir <- file.path(base, "project")
  config$logging$enabled <- TRUE
  target$config_env$config <- config
  input <- file.path(base, "sample.csv")
  write.csv(data.frame(x = 1:5, y = c(2, 3, 7, 8, 12)), input, row.names = FALSE)
  df <- target$load_dataframe(list(csv = input))
  out <- target$get_workspace_out_dir(df)
  project <- dirname(out)
  targets <- c(report = file.path(out, "report_canonical.md"), log = file.path(out, "analysis_log.jsonl"),
               manifest = file.path(project, config$defaults$workspace_manifest))
  writeLines(c("# Existing completed report", "UTF-8: äöü", "Do not replace this content."), targets[["report"]], useBytes = TRUE)
  writeLines('{"module":"existing","results":{"value":123.456}}', targets[["log"]])
  target$update_workspace_manifest(project, data.frame(dataset = "sample",
    copy_path = file.path(out, "sample.parquet"), source_path = input, type = "csv"))
  check(file.exists(targets[["manifest"]]), paste(label, "starts with a valid project manifest"))
  list(label = label, target = target, df = df, out = out, project = project, targets = targets,
       prior = lapply(targets, bytes), input = input)
}

run_analysis <- function(case, with_log = TRUE, supply_result = TRUE) {
  target <- case$target
  target$nlss_run_main("descriptive_stats", function() {
    target$nlss_begin_run("descriptive_stats", case$df, list(csv = case$input, log = with_log))
    target$nlss_resolve_request(list(vars = "x", digits = 2))
    target$nlss_stage_report(case$targets[["report"]], "Publication test", "Table 1\n\n| x | 3 |", "Staged deterministic output.")
    if (with_log) target$nlss_stage_log(case$out, "descriptive_stats", "probe", "probe",
                                      list(summary = list(mean = 3)), list(vars = "x"))
    if (supply_result) target$nlss_set_result(list(summary = list(mean = 3)))
  })
}

read_bundle <- function(case) {
  paths <- list.files(file.path(case$out, "runs"), pattern = "^result[.]json$", recursive = TRUE, full.names = TRUE, all.files = TRUE)
  check(length(paths) == 1L, paste(case$label, "publishes exactly one terminal bundle"))
  list(path = dirname(paths[[1]]), result = jsonlite::read_json(paths[[1]], simplifyVector = FALSE))
}

check_failure <- function(case, failure, expected_message) {
  check(is.character(failure) && grepl(expected_message, failure, fixed = TRUE), paste(case$label, "returns the original publication error"))
  for (name in names(case$targets)) {
    check(identical(bytes(case$targets[[name]]), case$prior[[name]]), paste(case$label, "restores exact", name, "bytes"))
  }
  bundle <- read_bundle(case)
  check(identical(bundle$result$status, "failed"), paste(case$label, "cannot be marked completed"))
  check(is.null(bundle$result$results), paste(case$label, "does not expose successful statistical results"))
  check(grepl(expected_message, bundle$result$error$message, fixed = TRUE), paste(case$label, "records the failure reason"))
  check(!file.exists(file.path(bundle$path, "output.md")), paste(case$label, "does not publish completed Markdown"))
  check(file.exists(file.path(bundle$path, "partial-output.md")), paste(case$label, "marks staged output as partial"))
  check(!startsWith(basename(bundle$path), ".pending-"), paste(case$label, "leaves a terminal failed directory"))
  check(!dir.exists(file.path(case$out, ".analysis-lock")), paste(case$label, "releases its owned analysis lock"))
  check(!dir.exists(file.path(case$project, ".publication-lock")), paste(case$label, "releases its owned publication lock"))
  check(identical(case$target$import_hash(file.path(bundle$path, "request.json"), file = TRUE),
                  bundle$result$artifacts$request$sha256), paste(case$label, "retains a valid request hash"))
}

for (kind in c("report", "log_and_manifest", "report_without_optional_log")) {
  case <- new_case(kind)
  if (kind == "log_and_manifest") {
    case$target$append_analysis_log <- function(...) {
      writeLines("PARTIAL BAD LOG", case$targets[["log"]])
      writeLines("PARTIAL BAD MANIFEST", case$targets[["manifest"]])
      stop("injected log and manifest publication failure")
    }
    message <- "injected log and manifest publication failure"
  } else {
    case$target$append_nlss_report <- function(...) {
      writeLines("PARTIAL BAD REPORT", case$targets[["report"]])
      stop("injected report publication failure")
    }
    message <- "injected report publication failure"
  }
  failure <- tryCatch({ run_analysis(case, with_log = kind != "report_without_optional_log"); NULL }, error = conditionMessage)
  check_failure(case, failure, message)
}

# A numerical result is mandatory even when the optional legacy JSONL is off.
case <- new_case("required_logger_returns_false")
case$target$append_analysis_log <- function(...) invisible(FALSE)
failure <- tryCatch({ run_analysis(case, with_log = TRUE); NULL }, error = conditionMessage)
check_failure(case, failure, "Required legacy analysis log was not written.")

case <- new_case("missing_required_result")
failure <- tryCatch({ run_analysis(case, with_log = FALSE, supply_result = FALSE); NULL }, error = conditionMessage)
check_failure(case, failure, "resolved request and result")

case <- new_case("success_without_optional_log")
run_analysis(case, with_log = FALSE)
bundle <- read_bundle(case)
check(identical(bundle$result$status, "completed"), "successful --log FALSE retains mandatory completed audit")
check(file.exists(file.path(bundle$path, "request.json")) && file.exists(file.path(bundle$path, "output.md")),
      "successful --log FALSE retains request and deterministic Markdown")
check(identical(bytes(case$targets[["log"]]), case$prior$log), "successful --log FALSE leaves existing optional log unchanged")
check(!identical(bytes(case$targets[["report"]]), case$prior$report), "successful publication appends the legacy report")
check(!dir.exists(file.path(case$out, ".analysis-lock")), "successful publication releases owned analysis lock")
check(!dir.exists(file.path(case$project, ".publication-lock")), "successful publication releases owned publication lock")

case <- new_case("globally_disabled_optional_log")
case$target$config_env$config$logging$enabled <- FALSE
run_analysis(case, with_log = TRUE)
bundle <- read_bundle(case)
check(identical(bundle$result$status, "completed"), "global optional-log opt-out does not masquerade as a write failure")
check(file.exists(file.path(bundle$path, "request.json")) && file.exists(file.path(bundle$path, "output.md")),
      "global optional-log opt-out retains mandatory audit")
check(identical(bytes(case$targets[["log"]]), case$prior$log), "global optional-log opt-out leaves existing log unchanged")
check(!dir.exists(file.path(case$project, ".publication-lock")) && !dir.exists(file.path(case$out, ".analysis-lock")),
      "global optional-log opt-out releases owned locks")

# Only migrated writers participate in this lock. Deliberately interleave two
# dataset publications in one project; no timing assumptions or subprocess races.
for (outer_fails in c(FALSE, TRUE)) {
  case <- new_case(if (outer_fails) "project_lock_rollback" else "project_lock_success")
  peer_target <- new.env(parent = globalenv())
  source(file.path(repo, "scripts/R/lib/bootstrap.R"), local = peer_target)
  peer_target$nlss_bootstrap(peer_target)
  peer_target$config_env$config <- case$target$get_config()
  peer_input <- file.path(dirname(case$input), "peer.csv")
  write.csv(data.frame(x = 1:5, y = c(2, 3, 7, 8, 12)), peer_input, row.names = FALSE)
  peer_df <- peer_target$load_dataframe(list(csv = peer_input))
  peer_out <- peer_target$get_workspace_out_dir(peer_df)
  peer_target$update_workspace_manifest(case$project, data.frame(dataset = "peer",
    copy_path = file.path(peer_out, "peer.parquet"), source_path = peer_input, type = "csv"))
  peer <- list(label = "peer", target = peer_target, df = peer_df, out = peer_out,
    project = case$project, input = peer_input,
    targets = c(report = file.path(peer_out, "report_canonical.md"),
      log = file.path(peer_out, "analysis_log.jsonl"), manifest = case$targets[["manifest"]]))
  run_analysis(peer)
  peer_sequence <- function() {
    vapply(readLines(peer$targets[["log"]], warn = FALSE), function(line) {
      as.integer(jsonlite::fromJSON(line)$log_seq)
    }, integer(1))
  }
  check(identical(unname(peer_sequence()), 1L), paste(case$label, "peer baseline sequence is one"))
  peer_prior <- lapply(peer$targets, bytes)
  original_append <- case$target$append_nlss_report
  blocked_error <- NULL
  case$target$append_nlss_report <- function(...) {
    check(dir.exists(file.path(case$project, ".publication-lock")), paste(case$label, "owns project lock before projection changes"))
    blocked_error <<- tryCatch({ run_analysis(peer); NULL }, error = conditionMessage)
    check(is.character(blocked_error) && grepl("publication|locked", blocked_error, ignore.case = TRUE),
          paste(case$label, "other dataset publication fails fast with lock context"))
    check(dir.exists(file.path(case$project, ".publication-lock")), paste(case$label, "blocked peer cannot remove another writer's lock"))
    for (name in names(peer$targets)) {
      check(identical(bytes(peer$targets[[name]]), peer_prior[[name]]), paste(case$label, "blocked peer leaves exact", name, "bytes unchanged"))
    }
    check(!dir.exists(file.path(peer_out, ".analysis-lock")), paste(case$label, "blocked peer releases only its dataset lock"))
    if (outer_fails) {
      writeLines("PARTIAL OUTER REPORT", case$targets[["report"]])
      stop("outer publication fails after peer is blocked")
    }
    original_append(...)
  }
  failure <- tryCatch({ run_analysis(case); NULL }, error = conditionMessage)
  if (outer_fails) {
    check(is.character(failure) && grepl("outer publication fails", failure, fixed = TRUE), paste(case$label, "outer failure is preserved"))
  } else {
    check(is.null(failure), paste(case$label, "outer publication completes successfully"))
  }
  check(!dir.exists(file.path(case$project, ".publication-lock")), paste(case$label, "outer completion or rollback releases project lock"))
  check(!dir.exists(file.path(case$out, ".analysis-lock")), paste(case$label, "outer completion or rollback releases dataset lock"))
  check(identical(unname(peer_sequence()), 1L), paste(case$label, "blocked peer did not append a legacy log entry"))
  peer_results <- list.files(file.path(peer_out, "runs"), pattern = "^result[.]json$",
    recursive = TRUE, full.names = TRUE, all.files = TRUE)
  completed <- vapply(peer_results, function(path) identical(jsonlite::read_json(path)$status, "completed"), logical(1))
  check(sum(completed) == 1L, paste(case$label, "blocked peer cannot publish a false completed bundle"))
  run_analysis(peer)
  check(identical(unname(peer_sequence()), c(1L, 2L)), paste(case$label, "peer retry has consecutive unique log sequences"))
  manifest <- peer_target$read_workspace_manifest(peer$targets[["manifest"]])
  check(identical(as.integer(peer_target$resolve_manifest_log_seq(manifest, peer_out, case$project)), 2L),
        paste(case$label, "peer manifest sequence agrees with its completed log"))
  check(!dir.exists(file.path(case$project, ".publication-lock")) && !dir.exists(file.path(peer_out, ".analysis-lock")),
        paste(case$label, "successful peer retry leaves no owned locks"))
}

setwd(original_wd)
if (is.na(prior_replay)) Sys.unsetenv("NLSS_REPLAY_REQUEST") else Sys.setenv(NLSS_REPLAY_REQUEST = prior_replay)
writeLines(checks, file.path(root, "phase2-publication-contract.log"))
jsonlite::write_json(list(passed = length(checks), failed = 0L, checks = checks), file.path(root, "phase2-publication-contract.json"), auto_unbox = TRUE, pretty = TRUE)
cat("Publication contract checks:", length(checks), "passed.\n")
