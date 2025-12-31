# Copyright (c) 2026 Mike Hammes
# SPDX-License-Identifier: Apache-2.0
parse_args <- function(args) {
  opts <- list()
  i <- 1
  while (i <= length(args)) {
    arg <- args[i]
    if (grepl("^--", arg)) {
      key <- sub("^--", "", arg)
      if (grepl("=", key)) {
        parts <- strsplit(key, "=", fixed = TRUE)[[1]]
        opts[[parts[1]]] <- parts[2]
      } else if (i < length(args) && !grepl("^--", args[i + 1])) {
        opts[[key]] <- args[i + 1]
        i <- i + 1
      } else {
        opts[[key]] <- TRUE
      }
    }
    i <- i + 1
  }
  opts
}

parse_bool <- function(value, default = FALSE) {
  if (is.null(value)) return(default)
  if (is.logical(value)) return(value)
  val <- tolower(as.character(value))
  val %in% c("true", "t", "1", "yes", "y")
}

parse_list <- function(value, sep = ",") {
  if (is.null(value) || is.logical(value)) return(character(0))
  value <- as.character(value)
  if (value == "") return(character(0))
  trimws(strsplit(value, sep, fixed = TRUE)[[1]])
}

prompt <- local({
  cache <- new.env(parent = emptyenv())
  cache$lines <- NULL
  cache$index <- 1
  cache$path <- NULL

  function(label, default = NULL) {
    required_path <- is.null(default) && grepl("path", label, ignore.case = TRUE)

    repeat {
      prompt_file <- Sys.getenv("NLSS_PROMPT_FILE", "")
      if (nzchar(prompt_file)) {
        if (is.null(cache$lines) || is.null(cache$path) || !identical(cache$path, prompt_file)) {
          cache$lines <- readLines(prompt_file, warn = FALSE)
          cache$index <- 1
          cache$path <- prompt_file
        }
        answer <- ""
        if (!is.null(cache$lines) && cache$index <= length(cache$lines)) {
          answer <- cache$lines[[cache$index]]
          cache$index <- cache$index + 1
        }
        if (!is.null(default) && answer == "") answer <- default
        if (required_path && (is.na(answer) || !nzchar(answer))) {
          stop("Prompt file is missing a required path for: ", label)
        }
        return(answer)
      }

      if (is.null(default)) {
        answer <- readline(paste0(label, ": "))
      } else {
        answer <- readline(paste0(label, " [", default, "]: "))
        if (answer == "") answer <- default
      }

      if (required_path && (is.na(answer) || !nzchar(answer))) {
        cat("Path is required.\n")
        next
      }
      return(answer)
    }
  }
})
