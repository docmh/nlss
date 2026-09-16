#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Isolated shared-boundary fault injection. No source/config overrides on disk.
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
repo <- normalizePath(file.path(dirname(script), "../.."), winslash = "/")
root <- Sys.getenv("NLSS_TEST_ROOT", tempfile("nlss-plot-publication-"))
dir.create(root, recursive = TRUE, showWarnings = FALSE)
root <- normalizePath(root, winslash = "/")
Sys.unsetenv("NLSS_REPLAY_REQUEST")
checks <- 0L
check <- function(ok, label) {
  if (!isTRUE(ok)) stop(label, call. = FALSE)
  checks <<- checks + 1L
  cat("[PASS] ", label, "\n", sep = "")
}
bytes <- function(path) if (!file.exists(path) || dir.exists(path)) NULL else readBin(path, "raw", file.info(path)$size)
new_case <- function(label) {
  base <- tempfile(paste0(label, "-"), tmpdir = root)
  dir.create(base); setwd(base)
  env <- new.env(parent = globalenv())
  source(file.path(repo, "scripts/R/lib/bootstrap.R"), local = env); env$nlss_bootstrap(env)
  config <- env$get_builtin_config(); config$defaults$output_dir <- file.path(base, "project")
  env$config_env$config <- config
  input <- file.path(base, "sample.csv")
  write.csv(data.frame(x = 1:4), input, row.names = FALSE)
  df <- env$load_dataframe(list(csv = input)); out <- env$get_workspace_out_dir(df)
  dir.create(file.path(out, "plots"))
  targets <- file.path(out, c("report_canonical.md", "analysis_log.jsonl", "plots/figure-001.png", "plots/figure-002.png"))
  targets <- c(targets, file.path(dirname(out), "nlss-workspace.yml"))
  writeLines("Existing figure report: äöü", targets[1], useBytes = TRUE)
  writeLines('{"module":"existing","results":{"value":1}}', targets[2])
  writeBin(charToRaw("old-image-bytes"), targets[3])
  list(env = env, df = df, out = out, targets = targets, prior = lapply(targets, bytes))
}
run <- function(case, mutate = function(...) NULL) {
  e <- case$env
  e$nlss_run_main("plot", function() {
    e$nlss_begin_run("plot", case$df, list(log = TRUE))
    e$nlss_resolve_request(list(type = "histogram", vars = "x"))
    for (i in 1:2) {
      name <- sprintf("plots/figure-%03d.png", i)
      path <- file.path(e$nlss_run_context$staging, name)
      dir.create(dirname(path), showWarnings = FALSE)
      writeBin(charToRaw(paste0("new-image-", i)), path)
      e$nlss_save_run_file(name, legacy_path = name, overwrite = i == 1)
    }
    e$nlss_stage_figure_report(case$targets[1], "Plots", "Figure 1. Run-local\n![plot](plots/figure-001.png)",
      legacy = list(figure_body = "Figure 7. Canonical\n![plot](plots/figure-001.png)", figure_start = 7L))
    e$nlss_set_result(list(figures = list(list(figure_number = 1, n = 4))))
    e$nlss_stage_log(case$out, "plot", "probe", "probe", list(figures = list(list(n = 4))))
    mutate(e)
  })
}
for (kind in c("success", "report", "log_manifest", "copy_second", "bundle_rename", "tamper")) {
  case <- new_case(kind); e <- case$env
  if (kind == "report") e$append_nlss_figure_report <- function(...) {
    writeLines("partial-report", case$targets[1]); stop("injected report failure")
  }
  if (kind == "log_manifest") e$append_analysis_log <- function(...) {
    writeLines("partial-log", case$targets[2]); writeLines("partial-manifest", case$targets[5])
    stop("injected log failure")
  }
  if (kind == "copy_second") e$file.copy <- function(from, to, ...) {
    if (identical(to, case$targets[4])) {
      writeBin(charToRaw("partial-image"), to); return(FALSE)
    }
    base::file.copy(from, to, ...)
  }
  if (kind == "bundle_rename") {
    attempts <- 0L
    e$file.rename <- function(from, to) {
      if (startsWith(basename(from), ".pending-") && dir.exists(from) && identical(dirname(from), dirname(to))) {
        attempts <<- attempts + 1L
        if (attempts == 1L) return(FALSE)
      }
      base::file.rename(from, to)
    }
  }
  mutation <- if (kind == "tamper") function(e) writeBin(charToRaw("changed-after-registration"),
    file.path(e$nlss_run_context$staging, "plots/figure-001.png")) else function(...) NULL
  error <- tryCatch({ run(case, mutation); NULL }, error = conditionMessage)
  check(if (kind == "success") is.null(error) else !is.null(error), paste(kind, "exit outcome"))
  terminal <- list.files(file.path(case$out, "runs"), "^result[.]json$", full.names = TRUE, recursive = TRUE)
  check(length(terminal) == 1L, paste(kind, "one terminal bundle"))
  result <- jsonlite::read_json(terminal[1], simplifyVector = FALSE)
  check(identical(result$status, if (kind == "success") "completed" else "failed"), paste(kind, "truthful status"))
  for (artifact in result$artifacts) check(identical(artifact$sha256,
    digest::digest(file = file.path(dirname(terminal), artifact$path), algo = "sha256")), paste(kind, "retained artifact hash", artifact$path))
  if (kind == "tamper") check(identical(result$artifacts[["plots/figure-001.png"]]$status, "changed_after_registration") &&
    !identical(result$artifacts[["plots/figure-001.png"]]$sha256, result$artifacts[["plots/figure-001.png"]]$expected_sha256), "failed run distinguishes registered and retained image bytes")
  check(!dir.exists(file.path(case$out, ".analysis-lock")) &&
    !dir.exists(file.path(dirname(case$out), ".publication-lock")), paste(kind, "owned locks released"))
  if (kind == "success") {
    for (i in 1:2) check(identical(bytes(case$targets[i + 2]), bytes(file.path(dirname(terminal), sprintf("plots/figure-%03d.png", i)))),
      paste(kind, "image projection", i))
    check(grepl("Figure 1. Run-local", paste(readLines(file.path(dirname(terminal), "output.md"), warn = FALSE), collapse = "\n"), fixed = TRUE), "run-local figure output")
    check(grepl("Figure 7. Canonical", paste(readLines(case$targets[1], warn = FALSE), collapse = "\n"), fixed = TRUE), "canonical figure output")
  } else {
    check(identical(lapply(case$targets, bytes), case$prior), paste(kind, "exact old report/log/manifest/image restoration; no orphan new image"))
    check(!file.exists(file.path(dirname(terminal), "output.md")), paste(kind, "no successful Markdown"))
  }
}

for (kind in c("traversal", "absolute", "duplicate", "existing", "empty", "symlink_directory", "symlink_file", "late_collision")) {
  case <- new_case(kind); e <- case$env
  outside <- file.path(dirname(case$out), "outside")
  dir.create(outside)
  if (kind == "symlink_directory") {
    # Move only the known, private fixture directory; preserve it for verification.
    stopifnot(file.rename(file.path(case$out, "plots"), file.path(case$out, "old-plots")))
    stopifnot(file.symlink(outside, file.path(case$out, "plots")))
  }
  if (kind == "symlink_file") stopifnot(file.symlink(file.path(outside, "sentinel.png"), case$targets[4]))
  error <- tryCatch({ e$nlss_run_main("plot", function() {
    e$nlss_begin_run("plot", case$df, list())
    e$nlss_resolve_request(list(type = "histogram"))
    source <- file.path(e$nlss_run_context$staging, "source.png")
    writeBin(if (kind == "empty") raw() else charToRaw("image"), source)
    target <- if (kind == "traversal") "plots/../../outside.png" else if (kind == "absolute") "/tmp/outside.png" else
      if (kind == "existing") "plots/figure-001.png" else "plots/figure-002.png"
    e$nlss_save_run_file("plots/new.png", source, target)
    if (kind == "duplicate") e$nlss_save_run_file("plots/another.png", source, target)
    if (kind == "late_collision") writeBin(charToRaw("appeared-after-staging"), case$targets[4])
    e$nlss_stage_figure_report(case$targets[1], "Plots", "Figure 1. test")
    e$nlss_set_result(list(figures = list(list(n = 4))))
  }); NULL }, error = conditionMessage)
  check(!is.null(error), paste(kind, "refused"))
  check(!length(list.files(outside, all.files = TRUE, no.. = TRUE)), paste(kind, "no external writes"))
  check(identical(bytes(case$targets[1]), case$prior[[1]]) && identical(bytes(case$targets[2]), case$prior[[2]]), paste(kind, "report/log unchanged"))
  if (kind == "late_collision") check(identical(bytes(case$targets[4]), charToRaw("appeared-after-staging")), "late collision not overwritten")
}
cat("Plot publication contract:", checks, "checks passed.\n")
