# SPDX-License-Identifier: Apache-2.0
#!/usr/bin/env Rscript
bootstrap_dir <- {
  file_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (length(file_arg)) dirname(normalizePath(file_arg[[1]], winslash = "/")) else getwd()
}
source(file.path(bootstrap_dir, "lib", "bootstrap.R"))
nlss_bootstrap()
source_lib("log_utilities.R")

print_usage <- function() cat("Usage: reconstruct_reports.R <analysis_log.jsonl> [--out-dir PATH] [--overwrite TRUE|FALSE]\n")
scalar <- function(x, default = "") if (is.character(x) && length(x) == 1L && !is.na(x)) x else default
field <- function(entry, name, fallback = "") {
  value <- if (is.list(entry$results)) entry$results[[name]] else NULL
  if (is.null(value) && is.list(entry$options)) value <- entry$options[[name]]
  scalar(value, fallback)
}
warn_line <- function(message) cat(message, "\n", file = stderr())
decode_report_block <- function(data, encoding, line, label) {
  if (!nzchar(scalar(data))) return("")
  fail <- function(reason) {
    warn_line(sprintf("Skipping line %d (%s): %s.", line, label, reason))
    ""
  }
  if (!identical(tolower(trimws(scalar(encoding))), "gzip+base64")) return(fail("missing or unsupported encoding"))
  bytes <- tryCatch(jsonlite::base64_dec(data), error = function(e) NULL)
  if (is.null(bytes) || !length(bytes)) return(fail("base64 decode failed"))
  decoded <- tryCatch(withCallingHandlers(memDecompress(bytes, type = "gzip"),
    warning = function(w) stop(conditionMessage(w))), error = function(e) NULL)
  if (is.null(decoded) || !length(decoded)) return(fail("gzip decompress failed"))
  text <- tryCatch(rawToChar(decoded), error = function(e) "")
  if (!nzchar(text) || is.na(iconv(text, "UTF-8", "UTF-8", sub = NA))) return(fail("invalid UTF-8 report"))
  Encoding(text) <- "UTF-8"
  text
}

main <- function() {
  opts <- nlss_log_arguments(commandArgs(TRUE), "reconstruct_reports", "NLSS_RECONSTRUCT_LOG")
  if (isTRUE(opts$help)) { print_usage(); return(invisible(NULL)) }
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Missing dependency: jsonlite.")
  log_path <- opts$path
  source_hash <- import_hash(log_path, file = TRUE)
  lines <- readLines(log_path, warn = FALSE)
  canonical <- character()
  outputs <- list()
  skipped <- 0L
  reports <- 0L
  decode <- function(entry, stem, index) decode_report_block(entry[[paste0(stem, "_b64")]],
    entry[[paste0(stem, "_encoding")]], index, stem)
  for (index in seq_along(lines)) {
    if (!nzchar(trimws(lines[[index]]))) next
    entry <- tryCatch(jsonlite::fromJSON(lines[[index]], simplifyVector = FALSE), error = function(e) NULL)
    if (!is.list(entry) || is.null(names(entry))) {
      warn_line(sprintf("Skipping line %d: invalid JSON.", index))
      skipped <- skipped + 1L
      next
    }
    final <- identical(entry$module, "metaskill_runner") && tolower(trimws(field(entry, "phase"))) %in%
      c("finalization", "finalise", "finalize", "completion", "complete", "completed", "finish", "finished")
    legacy_meta <- final && identical(trimws(field(entry, "report_block_source")), "metaskill_report")
    block <- decode(entry, if (legacy_meta) "report_block_full" else "report_block", index)
    if (nzchar(block)) canonical <- c(canonical, block)
    if (!final) next
    meta_block <- decode(entry, if (legacy_meta) "report_block" else "metaskill_report_block", index)
    if (!nzchar(meta_block) && !legacy_meta) meta_block <- decode(entry, "report_block_full", index)
    if (!nzchar(meta_block)) {
      warn_line(sprintf("Skipping metaskill report on line %d: missing report block.", index))
      next
    }
    stamp <- scalar(entry$timestamp_utc, field(entry, "timestamp"))
    date <- if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", stamp)) gsub("-", "", substr(stamp, 1L, 10L)) else "undated"
    slug <- function(value, fallback) {
      value <- gsub("_+", "_", sanitize_file_component(value))
      if (nzchar(value)) value else fallback
    }
    meta <- field(entry, "metaskill", field(entry, "meta"))
    stem <- paste0("report_", date, "_", slug(meta, "metaskill"), "_", slug(field(entry, "intent"), "no-intent"))
    name <- paste0(stem, "_reconstructed.md")
    suffix <- 0L
    while (name %in% names(outputs)) {
      suffix <- suffix + 1L
      name <- paste0(stem, "_line", index, if (suffix > 1L) paste0("_", suffix) else "", "_reconstructed.md")
    }
    outputs[[name]] <- meta_block
    reports <- reports + 1L
  }
  if (!length(canonical)) stop("No report_block entries found; older logs are not supported.")
  outputs <- c(list(report_canonical_reconstructed.md = paste(canonical, collapse = "")), outputs)
  out_dir <- if (is.null(opts[["out-dir"]])) dirname(log_path) else normalize_input_path(opts[["out-dir"]])
  out_dir <- normalizePath(ensure_out_dir(out_dir), winslash = "/", mustWork = TRUE)
  paths <- file.path(out_dir, names(outputs))
  safe <- function(path) {
    link <- Sys.readlink(path)
    if ((!is.na(link) && nzchar(link)) || dir.exists(path) ||
        !identical(normalizePath(path, winslash = "/", mustWork = FALSE), path) ||
        identical(path, log_path)) stop("Unsafe reconstruction target: ", basename(path))
  }
  for (path in paths) safe(path)
  lock <- file.path(out_dir, ".reconstruction-lock")
  if (!dir.create(lock, showWarnings = FALSE)) stop("Another reconstruction is active; inspect stale locks before recovery.")
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  same <- vapply(seq_along(paths), function(i) file.exists(paths[[i]]) &&
    identical(readBin(paths[[i]], "raw", file.info(paths[[i]])$size), charToRaw(outputs[[i]])), logical(1))
  if (any(file.exists(paths) & !same) && !isTRUE(opts$overwrite)) {
    stop("Reconstructed output already exists with different content. Choose another --out-dir or explicitly use --overwrite TRUE.")
  }
  stage <- tempfile(".reconstruction-", tmpdir = out_dir)
  if (!dir.create(stage)) stop("Could not stage reconstructed reports.")
  remove_stage <- TRUE
  on.exit(if (remove_stage) unlink(stage, recursive = TRUE), add = TRUE)
  existed <- file.exists(paths)
  old_hashes <- vapply(seq_along(paths), function(i) if (existed[[i]]) import_hash(paths[[i]], file = TRUE) else "", character(1))
  for (i in seq_along(paths)) {
    if (existed[[i]] && !file.copy(paths[[i]], file.path(stage, paste0("backup-", i)))) stop("Could not protect previous reconstruction.")
    writeBin(charToRaw(outputs[[i]]), file.path(stage, names(outputs)[[i]]))
  }
  changed <- integer()
  done <- FALSE
  on.exit({
    if (!done) for (i in changed) {
      if (existed[[i]]) {
        restored <- !dir.exists(paths[[i]]) && isTRUE(tryCatch({
          safe(paths[[i]])
          file.copy(file.path(stage, paste0("backup-", i)), paths[[i]], overwrite = TRUE) &&
            identical(import_hash(paths[[i]], file = TRUE), old_hashes[[i]])
        }, error = function(e) FALSE))
        if (!restored) remove_stage <- FALSE
      } else if (file.exists(paths[[i]])) {
        if (dir.exists(paths[[i]]) || unlink(paths[[i]]) != 0L) remove_stage <- FALSE
      }
    }
    if (!remove_stage) {
      write_import_json(lapply(changed, function(i) list(target = basename(paths[[i]]),
        existed = existed[[i]], backup_file = if (existed[[i]]) paste0("backup-", i) else NULL)), file.path(stage, "recovery.json"))
      warning("Could not restore all reconstruction outputs; recovery bytes retained in ", basename(stage), ".")
    }
  }, add = TRUE, after = FALSE)
  if (!identical(source_hash, import_hash(log_path, file = TRUE))) stop("Source log changed during reconstruction.")
  for (i in seq_along(paths)) {
    safe(paths[[i]])
    current <- if (file.exists(paths[[i]])) import_hash(paths[[i]], file = TRUE) else ""
    if (!identical(current, old_hashes[[i]])) stop("Reconstruction target changed during publication.")
    if (same[[i]]) next
    changed <- c(changed, i)
    source <- file.path(stage, names(outputs)[[i]])
    if (!file.copy(source, paths[[i]], overwrite = isTRUE(opts$overwrite)) ||
        !identical(import_hash(source, file = TRUE), import_hash(paths[[i]], file = TRUE))) stop("Could not publish reconstructed report.")
  }
  done <- TRUE
  cat("Wrote:\n", paste0("- ", basename(paths), collapse = "\n"), "\n", sep = "")
  if (reports) cat("Metaskill reports: ", reports, "\n", sep = "")
  if (skipped) cat("Skipped entries: ", skipped, "\n", sep = "")
  invisible(paths)
}
main()
