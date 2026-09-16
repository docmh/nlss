# SPDX-License-Identifier: Apache-2.0
#!/usr/bin/env Rscript
bootstrap_dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]), winslash = "/", mustWork = TRUE))
source(file.path(bootstrap_dir, "lib", "bootstrap.R"))
nlss_bootstrap()

main <- function() {
  opts <- parse_args(commandArgs(TRUE), module = NULL, allowed = c("request", "help"), boolean = "help")
  if (!is.null(opts$help)) {
    cat("Replay a completed, verified NLSS request without an AI model.\n",
        "Usage: Rscript replay_run.R --request <dataset-or-planning>/runs/<run-id>/request.json\n",
        "Requires unchanged NLSS R code, recorded R/packages/system libraries, input hashes and saved templates.\n",
        "Creates a new run in the same project; never overwrites the selected run.\n", sep = "")
    return(0L)
  }
  if (is.null(opts$request)) stop("Replay requires --request.")
  replay <- nlss_read_replay(opts$request)
  script <- file.path(bootstrap_dir, "run_nlss.R")
  Sys.setenv(NLSS_REPLAY_REQUEST = replay$path)
  system2(file.path(R.home("bin"), "Rscript"), c(shQuote(script), shQuote(replay$request$module)))
}
quit(status = main())
