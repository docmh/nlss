#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))[1]
source(file.path(dirname(normalizePath(script, winslash = "/", mustWork = TRUE)), "lib", "bootstrap.R"))
nlss_bootstrap()
source_lib("project_store.R")
tryCatch({
  import_flags <- c("df", "sep", "header", "csv-decimal", "csv-encoding", "csv-col-types", "csv-na-values")
  opts <- parse_args(commandArgs(TRUE), allowed = c("help", "project", "source", "working", "name", import_flags), boolean = c("help", "header"))
  if (!is.null(opts$help)) {
    cat("Initialize/reuse a current NLSS project and import a selected dataset. Returns JSON.\n",
        "Usage: project_create.R --project DIR [--source FILE] [--name NAME] [--working PATH]\n",
        "Formats: CSV, SAV, RDS, RData (--df OBJECT required), Parquet. No manual conversion.\n",
        "Defaults: source-derived name, data/<name>_working.parquet; source paths are project-relative.\n",
        "Existing project without --source: return selected --name or active dataset, without writes.\n",
        "Matching source/options: reuse edited working data. New name/source: add registration.\n",
        "Additional datasets do not change the active dataset; use the returned name in --dataset.\n",
        "CSV: --sep, --header, --csv-decimal, --csv-encoding, --csv-col-types, --csv-na-values.\n",
        "Changed source/options, conflicting names or working paths: stop without replacement.\n", sep = "")
  } else {
    result <- nlss_create_project(opts$project, opts$source, opts$working, opts$name,
      import_options = opts[intersect(names(opts), import_flags)])
    cat(import_json(result), "\n", sep = "")
  }
}, error = function(e) {
  cat(import_json(list(schema_version = 1L, status = "error", message = conditionMessage(e))), "\n", sep = "")
  message(conditionMessage(e)); quit(status = 2L)
})
