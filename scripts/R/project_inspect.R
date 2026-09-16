#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
bootstrap_dir <- {
  file_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(file_arg)) dirname(normalizePath(file_arg[1], winslash = "/", mustWork = TRUE)) else getwd()
}
source(file.path(bootstrap_dir, "lib", "bootstrap.R"))
nlss_bootstrap()
source_lib("project_inspect.R")

main <- function() {
  opts <- parse_args(commandArgs(TRUE), allowed = c("help", "project", "verify", "format"),
                     boolean = c("help", "verify"))
  if (parse_bool(opts$help)) {
    cat("Usage: project_inspect.R [--project DIR|MARKER] [--verify TRUE|FALSE] [--format markdown|json]\n",
        "Read-only project inspection. Default: nearest ancestor marker; no child/sibling search.\n",
        "No data loading, writes, logs, repair or migration.\n", sep = "")
    return(invisible(NULL))
  }
  format <- if (is.null(opts$format)) get_config_value("modules.project_inspect.format") else opts$format
  if (!format %in% c("markdown", "json")) stop("--format must be markdown or json.", call. = FALSE)
  verify <- parse_bool(opts$verify, get_config_value("modules.project_inspect.verify"))
  result <- nlss_inspect_project(opts$project, verify = verify)
  if (format == "json") {
    cat(jsonlite::toJSON(result, auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE, digits = NA), "\n", sep = "")
  } else cat(paste(nlss_inspection_markdown(result), collapse = "\n"), "\n", sep = "")
}
tryCatch(main(), error = function(e) {
  cat("Project inspection failed: ", conditionMessage(e), "\n", sep = "", file = stderr())
  quit(status = 2L)
})
