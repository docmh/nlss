#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))[1]
source(file.path(dirname(normalizePath(script, winslash = "/", mustWork = TRUE)), "lib", "bootstrap.R"))
nlss_bootstrap()
source_lib("project_report.R")
tryCatch({
  opts <- parse_args(commandArgs(TRUE), allowed = c("help", "action", "project", "report-id", "report", "runs", "revision", "verify"), boolean = c("help", "verify"))
  if (!is.null(opts$help)) {
    cat("Save authored report evidence as part of delivery, or browse saved revisions.\n",
        "Usage: project_report.R --report report.md --runs ID1,ID2 [--project DIR]\n",
        "       project_report.R --action inspect --report report.md [--revision rev-HASH] [--verify TRUE]\n",
        "Paths are project-relative; --project may be omitted for nearest-ancestor discovery. Output is JSON.\n",
        "Default action: save. The agent supplies the actual selected evidence; no report/revision ID is needed to save.\n",
        "Optional --report-id SLUG overrides path-based identity. Inspection does not verify evidence unless requested.\n",
        "Visible text remains untouched. Saving or verification does not certify calculations or semantic correctness.\n", sep = "")
  } else {
    action <- if (is.null(opts$action)) "save" else opts$action
    if (!action %in% c("save", "inspect")) stop("Choose --action save or --action inspect.")
    if (action == "save") {
      if (any(c("revision", "verify") %in% names(opts))) stop("--revision and --verify are inspect-only.")
      result <- nlss_save_report(opts$project, opts$report, opts$runs, opts$`report-id`)
    } else {
      if (!is.null(opts$runs)) stop("--runs is save-only.")
      result <- nlss_inspect_report(opts$project, opts$`report-id`, opts$revision, opts$report, verify = parse_bool(opts$verify))
    }
    cat(import_json(result), "\n", sep = "")
  }
}, error = function(e) { message(conditionMessage(e)); quit(status = 2L) })
