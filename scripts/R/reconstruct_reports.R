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
render_output_path <- get("render_output_path", mode = "function")
normalize_input_path <- get("normalize_input_path", mode = "function")
ensure_out_dir <- get("ensure_out_dir", mode = "function")
sanitize_file_component <- get("sanitize_file_component", mode = "function")
source_lib <- get("source_lib", mode = "function")

print_usage <- function() {
  cat("Usage: reconstruct_reports.R <analysis_log.jsonl> [--out-dir PATH]\n", file = stderr())
}

ensure_jsonlite <- function() {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    cat("Missing dependency: jsonlite. Install it: install.packages('jsonlite').\n", file = stderr())
    quit(status = 2)
  }
}

warn_line <- function(message) {
  cat(message, "\n", file = stderr())
}

parse_phase <- function(entry) {
  phase <- ""
  if (!is.null(entry$results) && !is.null(entry$results$phase)) {
    phase <- as.character(entry$results$phase)
  } else if (!is.null(entry$options) && !is.null(entry$options$phase)) {
    phase <- as.character(entry$options$phase)
  }
  trimws(phase)
}

get_report_block_source <- function(entry) {
  source <- ""
  if (!is.null(entry$results) && !is.null(entry$results$report_block_source)) {
    source <- as.character(entry$results$report_block_source)
  } else if (!is.null(entry$options) && !is.null(entry$options$report_block_source)) {
    source <- as.character(entry$options$report_block_source)
  }
  trimws(source)
}

is_finalization_phase <- function(phase) {
  if (is.null(phase) || !nzchar(phase)) return(FALSE)
  phase_lower <- tolower(trimws(as.character(phase)))
  phase_lower %in% c("finalization", "finalise", "finalize", "completion", "complete", "completed", "finish", "finished")
}

slugify_component <- function(value, fallback) {
  if (is.null(value) || !nzchar(value)) return(fallback)
  clean <- sanitize_file_component(value)
  clean <- gsub("_+", "_", clean)
  if (!nzchar(clean)) clean <- fallback
  clean
}

extract_date_stamp <- function(entry) {
  stamp <- ""
  if (!is.null(entry$timestamp_utc)) {
    stamp <- as.character(entry$timestamp_utc)
  } else if (!is.null(entry$results) && !is.null(entry$results$timestamp)) {
    stamp <- as.character(entry$results$timestamp)
  }
  if (!nzchar(stamp)) return(format(Sys.Date(), "%Y%m%d"))
  if (grepl("^\\d{4}-\\d{2}-\\d{2}", stamp)) {
    return(gsub("-", "", substr(stamp, 1, 10)))
  }
  format(Sys.Date(), "%Y%m%d")
}

decode_report_block <- function(data, encoding, line_index, label) {
  if (is.null(data) || !nzchar(data)) return("")
  if (is.null(encoding) || !nzchar(encoding)) {
    warn_line(sprintf("Skipping line %d (%s): missing encoding.", line_index, label))
    return("")
  }
  enc <- tolower(trimws(as.character(encoding)))
  if (enc != "gzip+base64") {
    warn_line(sprintf("Skipping line %d (%s): unsupported encoding '%s'.", line_index, label, enc))
    return("")
  }
  decoded <- tryCatch(jsonlite::base64_dec(data), error = function(e) NULL)
  if (is.null(decoded) || length(decoded) == 0) {
    warn_line(sprintf("Skipping line %d (%s): base64 decode failed.", line_index, label))
    return("")
  }
  raw_text <- tryCatch(memDecompress(decoded, type = "gzip"), error = function(e) raw(0))
  if (length(raw_text) == 0) {
    warn_line(sprintf("Skipping line %d (%s): gzip decompress failed.", line_index, label))
    return("")
  }
  text <- tryCatch(rawToChar(raw_text), error = function(e) "")
  enc2utf8(text)
}

args <- commandArgs(trailingOnly = TRUE)
out_dir <- ""
positional <- character(0)
i <- 1L
while (i <= length(args)) {
  arg <- args[i]
  if (arg %in% c("-h", "--help")) {
    print_usage()
    quit(status = 0)
  }
  if (grepl("^--out-dir=", arg)) {
    out_dir <- sub("^--out-dir=", "", arg)
    i <- i + 1L
    next
  }
  if (arg == "--out-dir") {
    out_dir <- if (i + 1L <= length(args)) args[i + 1L] else ""
    i <- i + 2L
    next
  }
  positional <- c(positional, arg)
  i <- i + 1L
}

ensure_jsonlite()

env_log <- Sys.getenv("NLSS_RECONSTRUCT_LOG", unset = "")
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

out_dir <- if (nzchar(out_dir)) normalize_input_path(out_dir) else dirname(log_path)
if (!nzchar(out_dir)) out_dir <- "."
ensure_out_dir(out_dir)

out_report <- file.path(out_dir, "report_canonical_reconstructed.md")
report_con <- file(out_report, open = "w", encoding = "UTF-8")
on.exit({
  if (isOpen(report_con)) close(report_con)
}, add = TRUE)

lines <- readLines(log_path, warn = FALSE)
if (length(lines) == 0) {
  warn_line("Empty log file.")
}

block_count <- 0L
skipped_entries <- 0L
metaskill_reports <- 0L
for (idx in seq_along(lines)) {
  line <- lines[[idx]]
  if (!nzchar(trimws(line))) next
  entry <- tryCatch(jsonlite::fromJSON(line, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(entry)) {
    warn_line(sprintf("Skipping line %d: invalid JSON.", idx))
    skipped_entries <- skipped_entries + 1L
    next
  }

  phase <- parse_phase(entry)
  is_meta <- !is.null(entry$module) && entry$module == "metaskill_runner"
  is_final <- is_meta && is_finalization_phase(phase)
  block_source <- if (is_meta) get_report_block_source(entry) else ""

  if (is_final && identical(block_source, "metaskill_report")) {
    canonical_block <- decode_report_block(entry$report_block_full_b64, entry$report_block_full_encoding, idx, "report_block_full")
    if (nzchar(canonical_block)) {
      cat(canonical_block, file = report_con, sep = "")
      block_count <- block_count + 1L
    } else {
      warn_line(sprintf("Skipping canonical block on line %d: missing report_block_full.", idx))
    }

    metaskill_block <- decode_report_block(entry$report_block_b64, entry$report_block_encoding, idx, "report_block")
    if (!nzchar(metaskill_block)) {
      warn_line(sprintf("Skipping metaskill report on line %d: missing report_block.", idx))
      next
    }
    meta_name <- ""
    intent <- ""
    if (!is.null(entry$results) && !is.null(entry$results$metaskill)) {
      meta_name <- as.character(entry$results$metaskill)
    } else if (!is.null(entry$options) && !is.null(entry$options$meta)) {
      meta_name <- as.character(entry$options$meta)
    }
    if (!is.null(entry$results) && !is.null(entry$results$intent)) {
      intent <- as.character(entry$results$intent)
    } else if (!is.null(entry$options) && !is.null(entry$options$intent)) {
      intent <- as.character(entry$options$intent)
    }
    date_stamp <- extract_date_stamp(entry)
    meta_slug <- slugify_component(meta_name, "metaskill")
    intent_slug <- slugify_component(intent, "no-intent")
    report_name <- paste0("report_", date_stamp, "_", meta_slug, "_", intent_slug, "_reconstructed.md")
    report_path <- file.path(out_dir, report_name)
    con <- file(report_path, open = "w", encoding = "UTF-8")
    cat(metaskill_block, file = con, sep = "")
    close(con)
    if (exists("ensure_output_front_matter", mode = "function")) {
      get("ensure_output_front_matter", mode = "function")(report_path)
    }
    metaskill_reports <- metaskill_reports + 1L
    next
  }

  block <- decode_report_block(entry$report_block_b64, entry$report_block_encoding, idx, "report_block")
  if (nzchar(block)) {
    cat(block, file = report_con, sep = "")
    block_count <- block_count + 1L
  }

  if (is_final) {
    metaskill_block <- decode_report_block(entry$metaskill_report_block_b64, entry$metaskill_report_block_encoding, idx, "metaskill_report_block")
    if (!nzchar(metaskill_block)) {
      metaskill_block <- decode_report_block(entry$report_block_full_b64, entry$report_block_full_encoding, idx, "report_block_full")
    }
    if (!nzchar(metaskill_block)) {
      warn_line(sprintf("Skipping metaskill report on line %d: missing metaskill report block.", idx))
      next
    }
    meta_name <- ""
    intent <- ""
    if (!is.null(entry$results) && !is.null(entry$results$metaskill)) {
      meta_name <- as.character(entry$results$metaskill)
    } else if (!is.null(entry$options) && !is.null(entry$options$meta)) {
      meta_name <- as.character(entry$options$meta)
    }
    if (!is.null(entry$results) && !is.null(entry$results$intent)) {
      intent <- as.character(entry$results$intent)
    } else if (!is.null(entry$options) && !is.null(entry$options$intent)) {
      intent <- as.character(entry$options$intent)
    }
    date_stamp <- extract_date_stamp(entry)
    meta_slug <- slugify_component(meta_name, "metaskill")
    intent_slug <- slugify_component(intent, "no-intent")
    report_name <- paste0("report_", date_stamp, "_", meta_slug, "_", intent_slug, "_reconstructed.md")
    report_path <- file.path(out_dir, report_name)
    con <- file(report_path, open = "w", encoding = "UTF-8")
    cat(metaskill_block, file = con, sep = "")
    close(con)
    if (exists("ensure_output_front_matter", mode = "function")) {
      get("ensure_output_front_matter", mode = "function")(report_path)
    }
    metaskill_reports <- metaskill_reports + 1L
  }
}

if (block_count == 0L) {
  unlink(out_report)
  cat("No report_block entries found; older logs are not supported.\n", file = stderr())
  quit(status = 2)
}

if (isOpen(report_con)) close(report_con)
if (exists("ensure_output_front_matter", mode = "function")) {
  get("ensure_output_front_matter", mode = "function")(out_report)
}

cat("Wrote:\n")
cat("- ", render_output_path(out_report, out_dir), "\n", sep = "")
if (metaskill_reports > 0L) {
  cat("Metaskill reports: ", metaskill_reports, "\n", sep = "")
}
if (skipped_entries > 0L) {
  cat("Skipped entries: ", skipped_entries, "\n", sep = "")
}
