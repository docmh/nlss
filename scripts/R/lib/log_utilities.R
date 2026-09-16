# SPDX-License-Identifier: Apache-2.0
# Positional compatibility adapter shared only by the two legacy log readers.
nlss_log_arguments <- function(args, module, env_name) {
  reconstruct <- identical(module, "reconstruct_reports")
  allowed <- if (reconstruct) c("help", "out-dir", "overwrite") else c("help", "diagnose")
  values <- list()
  positional <- character()
  i <- 1L
  while (i <= length(args)) {
    token <- args[[i]]
    if (token == "-h") token <- "--help"
    if (token == "--diag") token <- "--diagnose"
    if (token %in% c("--no-diagnose", "--no-diagnostic")) token <- "--diagnose=FALSE"
    if (startsWith(token, "--")) {
      key <- sub("=.*$", "", substring(token, 3L))
      if (!key %in% allowed) stop("Unknown option --", key, ". See --help.")
      if (key %in% names(values)) stop("Duplicate option --", key, ".")
      if (grepl("=", token, fixed = TRUE)) {
        value <- substring(token, regexpr("=", token, fixed = TRUE)[[1]] + 1L)
      } else if (key %in% c("help", "diagnose", "overwrite")) {
        value <- TRUE
        if (i < length(args) && tolower(args[[i + 1L]]) %in% c("true", "false", "t", "f", "yes", "no", "y", "n", "1", "0")) {
          i <- i + 1L
          value <- args[[i]]
        }
      } else {
        if (i == length(args) || startsWith(args[[i + 1L]], "--")) stop("Missing value for --", key, ".")
        i <- i + 1L
        value <- args[[i]]
      }
      if (key %in% c("help", "diagnose", "overwrite")) value <- parse_bool(value)
      else if (!nzchar(value)) stop("Empty value for --", key, ".")
      values[[key]] <- value
    } else positional <- c(positional, token)
    i <- i + 1L
  }
  if (isTRUE(values$help)) return(values)
  if (length(positional) > 1L && grepl("^[A-Za-z]:?$", positional[[1]]) && grepl("^[\\\\/]", positional[[2]])) {
    positional <- paste0(sub(":$", "", positional[[1]]), ":", paste(positional[-1], collapse = " "))
  }
  if (length(positional) > 1L) stop("Expected one log path; quote paths containing spaces.")
  path <- if (length(positional)) positional[[1]] else Sys.getenv(env_name, "")
  if (!nzchar(path)) stop("Missing log path. See --help.")
  path <- normalize_input_path(path)
  if (!file.exists(path) || dir.exists(path)) stop("Missing log: ", path)
  values$path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  values
}
