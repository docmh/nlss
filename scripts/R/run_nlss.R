#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# A process-local entrypoint adapter, not a second option parser or runner.
local({
  actual <- base::commandArgs(FALSE)
  args <- base::commandArgs(TRUE)
  if (!length(args) || identical(args, "--help")) {
    cat("Usage: Rscript run_nlss.R OPERATION [unchanged operation arguments]\n",
        "Example: Rscript run_nlss.R project-create --project DIR --source FILE\n",
        "Hyphen/underscore operation names are accepted. OPERATION --help shows its own help.\n",
        "Standalone installation uses install_nlss.R directly. No working-directory change.\n", sep = "")
    quit(save = "no", status = 0L)
  }
  file_arg <- grep("^--file=", actual)[[1]]
  own <- sub("^--file=", "", actual[file_arg])
  # R encodes spaces in --file before exposing commandArgs, unlike trailing args.
  if (!file.exists(own)) own <- gsub("~+~", " ", own, fixed = TRUE)
  root <- dirname(normalizePath(own, winslash = "/", mustWork = TRUE))
  source(file.path(root, "lib/dependency_resolver.R"), local = TRUE)
  operation <- gsub("-", "_", args[[1]], fixed = TRUE)
  if (!operation %in% setdiff(nlss_dependency_operations(), c("run_nlss", "install_nlss")))
    stop("Choose a known NLSS operation; use install_nlss.R directly for standalone maintenance.", call. = FALSE)
  target <- file.path(root, paste0(operation, ".R"))
  trailing <- args[-1L]
  separator <- match("--args", actual, nomatch = length(actual) + 1L)
  forwarded <- actual[seq_len(separator - 1L)]
  forwarded[file_arg] <- paste0("--file=", target)
  forwarded <- c(forwarded, "--args", trailing)
  # Existing entrypoint/bootstrap/audit code sees the selected module and exact
  # original arguments. Do not modify base R or introduce a persistent hook.
  assign("commandArgs", function(trailingOnly = FALSE) if (trailingOnly) trailing else forwarded,
         envir = .GlobalEnv)
  sys.source(target, envir = .GlobalEnv)
})
