# Copyright (c) 2026 Mike Hammes
# SPDX-License-Identifier: Apache-2.0
#!/usr/bin/env Rscript

bootstrap_dir <- {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", cmd_args[grep("^--file=", cmd_args)])
  if (length(file_arg) > 0 && nzchar(file_arg[1])) {
    dirname(normalizePath(file_arg[1], winslash = "/", mustWork = FALSE))
  } else {
    getwd()
  }
}
source(file.path(bootstrap_dir, "lib", "paths.R"))
source_lib("config.R")
source_lib("io.R")


# Static analysis aliases for source_lib-defined functions.
normalize_input_path <- get("normalize_input_path", mode = "function")
source_lib <- get("source_lib", mode = "function")

print_usage <- function() {
  cat("Usage: check_integrity.R <analysis_log.jsonl> [--diagnose TRUE|FALSE]\n", file = stderr())
}

ensure_jsonlite <- function() {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    cat("Missing dependency: jsonlite. Install it: install.packages('jsonlite').\n", file = stderr())
    quit(status = 2)
  }
}

parse_bool <- function(value, default = FALSE) {
  if (is.null(value)) return(default)
  text <- tolower(trimws(as.character(value)))
  if (!nzchar(text)) return(default)
  if (text %in% c("true", "t", "1", "yes", "y")) return(TRUE)
  if (text %in% c("false", "f", "0", "no", "n")) return(FALSE)
  default
}

safe_seq_value <- function(value) {
  if (is.null(value)) return(NA_integer_)
  parsed <- suppressWarnings(as.integer(value))
  if (length(parsed) == 0) return(NA_integer_)
  parsed <- parsed[1]
  if (is.na(parsed) || parsed < 0) return(NA_integer_)
  parsed
}

decode_bytes <- function(raw_vec) {
  if (length(raw_vec) == 0) return("")
  text <- rawToChar(raw_vec)
  decoded <- suppressWarnings(iconv(text, from = "UTF-8", to = "UTF-8", sub = NA))
  if (is.na(decoded)) {
    decoded <- suppressWarnings(iconv(text, from = "latin1", to = "UTF-8"))
  }
  if (is.na(decoded)) text else decoded
}

hex_to_raw <- function(hex) {
  if (is.null(hex)) return(raw(0))
  hex <- trimws(tolower(as.character(hex)))
  if (!nzchar(hex) || nchar(hex) %% 2 != 0) return(raw(0))
  bytes <- substring(hex, seq(1, nchar(hex), 2), seq(2, nchar(hex), 2))
  vals <- suppressWarnings(strtoi(bytes, 16L))
  if (any(is.na(vals))) return(raw(0))
  as.raw(vals)
}

xor_hex <- function(left, right) {
  if (is.null(left) || is.null(right)) return("")
  left_raw <- hex_to_raw(left)
  right_raw <- hex_to_raw(right)
  if (length(left_raw) == 0 || length(right_raw) == 0) return("")
  if (length(left_raw) != length(right_raw)) return("")
  out <- as.raw(bitwXor(as.integer(left_raw), as.integer(right_raw)))
  paste(sprintf("%02x", as.integer(out)), collapse = "")
}

hashlib_md5_bytes <- function(raw_vec) {
  tmp_path <- tempfile(pattern = "nlss-checksum-", fileext = ".bin")
  on.exit(unlink(tmp_path), add = TRUE)
  writeBin(raw_vec, tmp_path)
  digest <- tools::md5sum(tmp_path)
  unname(digest[[1]])
}

hashlib_md5_text <- function(text) {
  if (is.null(text) || !nzchar(text)) return("")
  tmp_path <- tempfile(pattern = "nlss-seq-", fileext = ".txt")
  on.exit(unlink(tmp_path), add = TRUE)
  writeBin(charToRaw(text), tmp_path)
  digest <- tools::md5sum(tmp_path)
  unname(digest[[1]])
}

find_last_raw_pattern <- function(haystack, needle) {
  if (length(needle) == 0 || length(haystack) < length(needle)) return(-1L)
  last <- -1L
  max_start <- length(haystack) - length(needle) + 1
  if (max_start < 1) return(-1L)
  for (i in seq_len(max_start)) {
    if (all(haystack[i:(i + length(needle) - 1)] == needle)) last <- i
  }
  last
}

args <- commandArgs(trailingOnly = TRUE)
diagnose <- TRUE
positional <- character(0)
bool_tokens <- c("true", "t", "1", "yes", "y", "false", "f", "0", "no", "n")
i <- 1L
while (i <= length(args)) {
  arg <- args[i]
  if (arg %in% c("-h", "--help")) {
    print_usage()
    quit(status = 2)
  }
  if (grepl("^--diagnose=", arg)) {
    value <- sub("^--diagnose=", "", arg)
    diagnose <- parse_bool(value, default = TRUE)
    i <- i + 1L
    next
  }
  if (arg %in% c("--diagnose", "--diag")) {
    next_value <- if (i + 1L <= length(args)) args[i + 1L] else ""
    if (nzchar(next_value) && tolower(next_value) %in% bool_tokens) {
      diagnose <- parse_bool(next_value, default = TRUE)
      i <- i + 2L
      next
    }
    diagnose <- TRUE
    i <- i + 1L
    next
  }
  if (arg %in% c("--no-diagnose", "--no-diagnostic")) {
    diagnose <- FALSE
    i <- i + 1L
    next
  }
  positional <- c(positional, arg)
  i <- i + 1L
}

ensure_jsonlite()

env_log <- Sys.getenv("NLSS_INTEGRITY_LOG", unset = "")
if (length(positional) < 1 && !nzchar(env_log)) {
  print_usage()
  quit(status = 2)
}

arg_log <- ""
if (length(positional) >= 1 && nzchar(positional[1])) {
  arg_log <- normalize_input_path(positional[1])
}
if ((is.null(arg_log) || !nzchar(arg_log) || !file.exists(arg_log)) && length(positional) >= 2) {
  first <- positional[1]
  second <- positional[2]
  reconstructed <- ""
  if (grepl("^[A-Za-z]$", first) && grepl("^[\\\\/]", second)) {
    rest <- paste(positional[2:length(positional)], collapse = " ")
    reconstructed <- paste0(first, ":", rest)
  } else if (grepl("^[A-Za-z]:$", first) && grepl("^[\\\\/]", second)) {
    rest <- paste(positional[2:length(positional)], collapse = " ")
    reconstructed <- paste0(first, rest)
  }
  if (nzchar(reconstructed)) {
    reconstructed <- normalize_input_path(reconstructed)
    if (file.exists(reconstructed)) {
      arg_log <- reconstructed
    }
  }
}
env_log_norm <- ""
if (nzchar(env_log)) {
  env_log_norm <- normalize_input_path(env_log)
}

log_path <- ""
if (nzchar(arg_log) && file.exists(arg_log)) {
  log_path <- arg_log
} else if (nzchar(env_log_norm) && file.exists(env_log_norm)) {
  log_path <- env_log_norm
}

if (!nzchar(log_path)) {
  fallback <- if (nzchar(arg_log)) arg_log else env_log_norm
  cat("Missing log: ", fallback, "\n", sep = "", file = stderr())
  quit(status = 2)
}

file_size <- file.info(log_path)$size
if (is.na(file_size) || file_size <= 0) {
  cat("No checksum entries found.\n")
  quit(status = 0)
}

raw_data <- readBin(log_path, "raw", n = file_size)
pattern <- charToRaw(",\"checksum\":\"")
counts <- list()
records <- list()
line_index <- 0L
prev_seq <- NA_integer_
prev_line_raw <- NULL
prev_line_ending <- raw(0)

# Keep original line endings because they affect the checksum.
pos <- 1L
total_len <- length(raw_data)
while (pos <= total_len) {
  rel_newline <- which(raw_data[pos:total_len] == as.raw(0x0A))
  if (length(rel_newline) == 0) {
    line_raw <- raw_data[pos:total_len]
    line_ending <- raw(0)
    pos <- total_len + 1L
  } else {
    newline_pos <- pos + rel_newline[1] - 1L
    if (newline_pos > pos && raw_data[newline_pos - 1L] == as.raw(0x0D)) {
      line_raw <- if (newline_pos - 2L >= pos) raw_data[pos:(newline_pos - 2L)] else raw(0)
      line_ending <- raw_data[(newline_pos - 1L):newline_pos]
    } else {
      line_raw <- if (newline_pos - 1L >= pos) raw_data[pos:(newline_pos - 1L)] else raw(0)
      line_ending <- raw_data[newline_pos]
    }
    pos <- newline_pos + 1L
  }

  if (length(line_ending) == 0) line_ending <- charToRaw("\n")

  line_text <- decode_bytes(line_raw)
  if (!nzchar(trimws(line_text))) next
  line_index <- line_index + 1L
  record <- list(
    line_index = line_index,
    log_seq = NA_integer_,
    seq_status = "missing",
    gap_range = "",
    reverted = ""
  )

  entry <- tryCatch(jsonlite::fromJSON(line_text, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.null(entry)) {
    seq_value <- safe_seq_value(entry$log_seq)
    if (!is.na(seq_value)) {
      record$log_seq <- seq_value
      if (is.na(prev_seq)) {
        record$seq_status <- "start"
      } else if (seq_value <= prev_seq) {
        record$seq_status <- "non_monotonic"
      } else if (seq_value > prev_seq + 1L) {
        record$seq_status <- "gap"
        record$gap_range <- paste0(prev_seq + 1L, "-", seq_value - 1L)
      } else {
        record$seq_status <- "ok"
      }
      prev_seq <- seq_value
    }

    combined <- entry$checksum
    if (is.character(combined) && length(combined) > 0 && nzchar(combined)) {
      if (length(line_raw) >= 2 &&
          line_raw[length(line_raw)] == as.raw(0x7D) &&
          line_raw[length(line_raw) - 1L] == as.raw(0x22)) {
        idx <- find_last_raw_pattern(line_raw, pattern)
        if (idx > 0L) {
          if (idx > 1L) {
            base_raw <- c(line_raw[1:(idx - 1L)], as.raw(0x7D))
          } else {
            base_raw <- as.raw(0x7D)
          }
          entry_checksum <- hashlib_md5_bytes(c(base_raw, line_ending))
          reverted <- xor_hex(combined, entry_checksum)
          version <- suppressWarnings(as.integer(entry$checksum_version))
          if (is.na(version)) version <- 1L
          if (version >= 2L && !is.null(prev_line_raw)) {
            prev_checksum <- hashlib_md5_bytes(c(prev_line_raw, prev_line_ending))
            reverted <- xor_hex(reverted, prev_checksum)
          }
          if (version >= 3L) {
            if (!is.na(seq_value)) {
              seq_checksum <- hashlib_md5_text(as.character(seq_value))
              if (nzchar(seq_checksum)) {
                reverted <- xor_hex(reverted, seq_checksum)
              }
            }
          }
          if (nzchar(reverted)) {
            record$reverted <- reverted
            if (is.null(counts[[reverted]])) {
              counts[[reverted]] <- 1L
            } else {
              counts[[reverted]] <- counts[[reverted]] + 1L
            }
          }
        }
      }
    }
  }
  records[[length(records) + 1L]] <- record
  prev_line_raw <- line_raw
  prev_line_ending <- line_ending
}

if (length(counts) == 0) {
  cat("No checksum entries found.\n")
  quit(status = 0)
}

counts_vec <- unlist(counts, use.names = TRUE)
max_count <- max(counts_vec)
dominant <- names(counts_vec)[counts_vec == max_count]

for (i in seq_along(records)) {
  rec <- records[[i]]
  has_checksum <- nzchar(rec$reverted)
  if (!has_checksum) {
    rec$checksum_status <- "no_checksum"
  } else if (rec$reverted %in% dominant) {
    rec$checksum_status <- "ok"
  } else {
    rec$checksum_status <- "mismatch"
  }
  if (rec$checksum_status == "no_checksum") {
    rec$inferred <- "no_checksum"
  } else if (rec$seq_status == "missing") {
    rec$inferred <- "seq_missing"
  } else if (rec$seq_status == "non_monotonic") {
    rec$inferred <- "seq_out_of_order"
  } else if (rec$seq_status == "gap") {
    rec$inferred <- "post_delete_gap"
  } else if (rec$checksum_status == "mismatch") {
    rec$inferred <- "mismatch"
  } else {
    rec$inferred <- "ok"
  }
  records[[i]] <- rec
}

if (length(records) >= 2) {
  for (i in seq_len(length(records) - 1L)) {
    rec <- records[[i]]
    if (rec$inferred == "mismatch" && rec$seq_status %in% c("ok", "start")) {
      next_rec <- records[[i + 1L]]
      if (next_rec$inferred == "mismatch" &&
          next_rec$seq_status == "ok" &&
          !is.na(rec$log_seq) &&
          !is.na(next_rec$log_seq) &&
          next_rec$log_seq == rec$log_seq + 1L) {
        rec$inferred <- "edited_candidate"
        if (next_rec$inferred == "mismatch") {
          next_rec$inferred <- "post_edit_chain"
        }
        records[[i]] <- rec
        records[[i + 1L]] <- next_rec
      }
    }
  }
}

if (length(records) >= 1L) {
  last_idx <- length(records)
  rec <- records[[last_idx]]
  if (rec$inferred == "mismatch" && rec$seq_status %in% c("ok", "start")) {
    rec$inferred <- "edited_candidate"
    records[[last_idx]] <- rec
  }
}

keys <- sort(names(counts))
for (checksum in keys) {
  cat(checksum, counts[[checksum]], "\n", sep = " ")
}

if (length(keys) > 1) {
  cat("WARNING: multiple reverted checksums found.\n", file = stderr())
}

if (diagnose) {
  for (rec in records) {
    log_seq_text <- if (is.na(rec$log_seq)) "NA" else as.character(rec$log_seq)
    checksum_text <- if (nzchar(rec$reverted)) rec$reverted else "NA"
    gap_info <- if (nzchar(rec$gap_range)) paste0(" missing_seq=", rec$gap_range) else ""
    cat(
      sprintf(
        "DIAG line=%d log_seq=%s status=%s seq=%s inferred=%s checksum=%s%s\n",
        rec$line_index,
        log_seq_text,
        rec$checksum_status,
        rec$seq_status,
        rec$inferred,
        checksum_text,
        gap_info
      ),
      file = stderr()
    )
  }
}
