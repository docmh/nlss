# SPDX-License-Identifier: Apache-2.0
# Small shared loader for migrated entrypoints. Required libraries and functions
# are never optional; a broken installation must not silently skip safeguards.
.nlss_bootstrap_lib_dir <- local({
  frame_files <- vapply(sys.frames(), function(frame) {
    value <- frame$ofile
    if (is.null(value) || length(value) != 1L) "" else as.character(value)
  }, character(1))
  candidates <- frame_files[nzchar(frame_files) & basename(frame_files) == "bootstrap.R"]
  if (!length(candidates)) stop("bootstrap.R must be loaded with source().", call. = FALSE)
  dirname(normalizePath(tail(candidates, 1L), winslash = "/", mustWork = TRUE))
})

nlss_bootstrap <- function(envir = parent.frame(), lib_dir = .nlss_bootstrap_lib_dir) {
  if (!is.environment(envir)) stop("Bootstrap target must be an environment.", call. = FALSE)
  lib_dir <- normalizePath(lib_dir, winslash = "/", mustWork = TRUE)
  libraries <- c("dependency_resolver.R", "paths.R", "cli.R", "config.R", "io.R", "data_utils.R", "formatting.R", "run_contract.R")
  paths <- file.path(lib_dir, libraries)
  missing <- libraries[!file.exists(paths)]
  if (length(missing)) {
    stop("Missing mandatory NLSS libraries: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  load_library <- function(file_name) {
    if (length(file_name) != 1L || is.na(file_name) ||
        !identical(basename(file_name), file_name)) {
      stop("source_lib expects one library filename.", call. = FALSE)
    }
    path <- file.path(lib_dir, file_name)
    if (!file.exists(path)) stop("Missing mandatory NLSS library: ", file_name, call. = FALSE)
    source(path, local = envir)
    invisible(path)
  }
  for (library in libraries) load_library(library)
  get("nlss_dependency_activate_library", envir = envir)()
  # paths.R retains the legacy implementation for non-migrated modules. This
  # loader binds subsequent source_lib calls to this installation and target.
  assign("source_lib", load_library, envir = envir)
  assign("get_script_dir", function() dirname(lib_dir), envir = envir)
  # The two standalone history readers share a positional CLI adapter rather
  # than parse_args(). Reuse it here; no per-utility dependency hooks or parser.
  entry <- get("nlss_dependency_entrypoint", envir = envir)()
  if (!is.null(entry) && entry %in% c("check_integrity", "reconstruct_reports")) {
    load_library("log_utilities.R")
    opts <- get("nlss_log_arguments", envir = envir)(commandArgs(TRUE), entry,
      if (entry == "check_integrity") "NLSS_INTEGRITY_LOG" else "NLSS_RECONSTRUCT_LOG")
    get("nlss_dependency_preflight", envir = envir)(entry, opts)
  }
  required <- c("parse_args", "parse_bool", "get_config_value", "load_dataframe",
                "append_analysis_log", "append_nlss_report", "select_variables",
                "round_numeric", "resolve_label_metadata", "nlss_run_main", "nlss_begin_run",
                "nlss_resolve_request", "nlss_publish_run")
  missing_functions <- required[!vapply(required, exists, logical(1),
                                       envir = envir, mode = "function", inherits = FALSE)]
  if (length(missing_functions)) {
    stop("Missing mandatory NLSS functions: ", paste(missing_functions, collapse = ", "), call. = FALSE)
  }
  invisible(list(lib_dir = lib_dir, libraries = libraries))
}
