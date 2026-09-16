#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Base-R setup exception: unavailable yaml/jsonlite must not prevent recovery.
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))[1]
lib <- file.path(dirname(normalizePath(script, winslash = "/", mustWork = TRUE)), "lib")
for (name in c("dependency_resolver.R", "cli.R", "config.R")) source(file.path(lib, name))
main <- function() {
  args <- commandArgs(TRUE)
  separator <- match("--", args, nomatch = length(args) + 1L)
  target <- if (separator < length(args)) args[(separator + 1L):length(args)] else character()
  own <- if (separator > 1L) args[seq_len(separator - 1L)] else character()
  opts <- parse_args(own, module = NULL,
    allowed = c("help", "action", "operation", "repo", "library", "type", "approve"), boolean = "help")
  if (!is.null(opts$help)) {
    cat("Check NLSS requirements without running an analysis; install only after approval.\n",
      "dependency_resolver.R --operation NAME [--action check|plan|install] [--library DIR]\n",
      "  [--repo HTTPS_OR_FILE_URL] [--type source|PLATFORM_BINARY] [--approve PACKAGE@VERSION,...]\n",
      "  -- ORIGINAL_OPERATION_OPTIONS\n",
      "check is offline; plan reads repository metadata; install requires the exact plan's approval string.\n",
      "Custom library: set NLSS_R_LIBRARY to the same path for subsequent NLSS commands.\n",
      "Missing foundational packages are resolved first; retry check after installing them.\n", sep = "")
    return(0L)
  }
  if (is.null(opts$operation)) stop("Supply --operation (for example sem, power, project-create).")
  operation <- gsub("-", "_", opts$operation, fixed = TRUE)
  if (!operation %in% nlss_dependency_operations() || operation %in% c("dependency_resolver", "install_nlss", "run_nlss")) stop("Choose an analysis/utility target; setup helpers themselves need only base R.")
  if (operation %in% c("check_integrity", "reconstruct_reports")) {
    source(file.path(lib, "paths.R"))
    source(file.path(lib, "io.R"))
    source(file.path(lib, "log_utilities.R"))
    target <- if (!length(target)) list() else nlss_log_arguments(target, operation,
      if (operation == "check_integrity") "NLSS_INTEGRITY_LOG" else "NLSS_RECONSTRUCT_LOG")
  } else {
    schema <- cli_option_schema(operation)
    target <- parse_args(target, module = operation, allowed = schema$allowed, boolean = schema$boolean)
  }
  if (!is.null(opts$library)) Sys.setenv(NLSS_R_LIBRARY = opts$library)
  nlss_dependency_activate_library()
  check <- nlss_dependency_request(operation, target)
  action <- if (is.null(opts$action)) "check" else opts$action
  if (!action %in% c("check", "plan", "install")) stop("Choose --action check, plan or install.")
  if (operation == "replay_run" && check$status == "ready") {
    if (action != "check") stop("Restore the recorded replay environment explicitly; no historical package installation is inferred.")
    if (is.null(target$request)) stop("Replay readiness requires -- --request PATH; foundation availability alone is not replay readiness.")
    # Only after foundations are available, reuse the existing read-only replay
    # verifier. Never claim ready merely because today's basic packages load.
    source(file.path(lib, "bootstrap.R"), local = TRUE)
    nlss_bootstrap(envir = environment())
    replay <- nlss_read_replay(target$request)
    requirements <- lapply(names(replay$request$environment$packages), function(package)
      list(package = package, exact = replay$request$environment$packages[[package]],
           reason = "recorded replay environment", exports = character()))
    check <- nlss_dependency_check(operation, requirements = requirements)
    check$scope <- "Selected request passed the unchanged exact replay verifier; no analysis executed."
  }
  if (action == "check" || check$status == "ready") {
    cat(nlss_dependency_json(check), "\n", sep = "")
    return(if (check$status == "ready") 0L else 42L)
  }
  if (is.null(opts$repo)) stop("Planning/installing requires an explicit --repo; ask for installation approval after reviewing the plan.")
  library <- nlss_dependency_install_library(nlss_dependency_library(), dirname(dirname(dirname(lib))), target$project)
  type <- if (is.null(opts$type)) .Platform$pkgType else opts$type
  plan <- nlss_dependency_install_plan(check, opts$repo, library, type)
  result <- if (action == "install") {
    requirements <- nlss_dependency_requirements(operation, target,
      foundation = nlss_dependency_check(operation, target, foundation = TRUE)$status != "ready")
    nlss_dependency_install(plan, opts$approve, requirements, file.path(lib, "dependency_resolver.R"))
  } else plan
  result$available <- NULL
  cat(nlss_dependency_json(result), "\n", sep = "")
  if (result$status %in% c("ready", "installed") || action == "plan") 0L else 43L
}
status <- tryCatch(main(), error = function(e) {
  cat(nlss_dependency_json(list(status = "dependency_error", message = conditionMessage(e))), "\n", sep = "")
  44L
})
quit(save = "no", status = status)
