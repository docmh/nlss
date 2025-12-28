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

print_usage <- function() {
  cat("Usage: check_integrity.R <analysis_log.jsonl>\n", file = stderr())
}

ensure_jsonlite <- function() {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    cat("Missing dependency: jsonlite. Install it: install.packages('jsonlite').\n", file = stderr())
    quit(status = 2)
  }
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
if (length(args) < 1 || args[1] %in% c("-h", "--help")) {
  print_usage()
  quit(status = 2)
}

ensure_jsonlite()

log_path <- normalize_input_path(args[1])
if (!nzchar(log_path) || !file.exists(log_path)) {
  cat("Missing log: ", log_path, "\n", sep = "", file = stderr())
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

  entry <- tryCatch(jsonlite::fromJSON(line_text, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.null(entry)) {
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
          if (nzchar(reverted)) {
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
  prev_line_raw <- line_raw
  prev_line_ending <- line_ending
}

if (length(counts) == 0) {
  cat("No checksum entries found.\n")
  quit(status = 0)
}

keys <- sort(names(counts))
for (checksum in keys) {
  cat(checksum, counts[[checksum]], "\n", sep = " ")
}

if (length(keys) > 1) {
  cat("WARNING: multiple reverted checksums found.\n", file = stderr())
}
