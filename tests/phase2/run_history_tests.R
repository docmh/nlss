#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Independent binary log fixtures; these tests do not use NLSS log producers.
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))[[1]]
repo <- normalizePath(file.path(dirname(script), "../.."), winslash = "/")
args <- commandArgs(TRUE)
if ("--help" %in% args) { cat("Usage: run_history_tests.R [--root PATH] [--keep N] [--match REGEX]\n"); quit(status = 0) }
if (length(args) %% 2L || (length(args) && any(!args[seq(1L, length(args), 2L)] %in% c("--root", "--keep", "--match")))) stop("Unknown/incomplete option")
arg <- function(key, default) { i <- which(args == key); if (length(i) > 1L) stop("Repeated option"); if (length(i)) args[i + 1L] else default }
cfg <- yaml::read_yaml(Sys.getenv("NLSS_TESTS_CONFIG", file.path(repo, "tests/tests.yml")), eval.expr = FALSE)$tests
collection <- file.path(arg("--root", Sys.getenv("NLSS_TEST_ROOT", file.path(repo, cfg$output_dir))), "phase2-history")
dir.create(collection, recursive = TRUE, showWarnings = FALSE)
root <- tempfile("run-", normalizePath(collection, winslash = "/")); dir.create(root)
keep <- as.numeric(arg("--keep", Sys.getenv("NLSS_KEEP_RUNS", as.character(cfg$keep_runs_default))))
if (!is.finite(keep) || keep < 0 || keep != floor(keep)) stop("Invalid --keep")
pattern <- arg("--match", ".*")
sha <- function(path) digest::digest(file = path, algo = "sha256")
bytes <- function(path) if (file.exists(path)) readBin(path, "raw", file.info(path)$size) else NULL
json <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", digits = NA)
save_json <- function(x, path) writeLines(json(x), path)
results <- list(); assertions <- 0L; processes <- list()
check <- function(ok, label) { if (!isTRUE(ok)) stop(label); assertions <<- assertions + 1L }
test <- function(name, code) {
  if (!grepl(pattern, name)) return(invisible(NULL))
  before <- assertions; start <- proc.time()[["elapsed"]]; cwd <- getwd(); on.exit(setwd(cwd))
  Sys.unsetenv(c("NLSS_INTEGRITY_LOG", "NLSS_RECONSTRUCT_LOG"))
  error <- tryCatch({ force(code); NULL }, error = conditionMessage)
  results[[length(results) + 1L]] <<- list(test = name, passed = is.null(error), assertions = assertions - before,
    seconds = proc.time()[["elapsed"]] - start, message = if (is.null(error)) "OK" else error)
  cat(if (is.null(error)) "[PASS] " else "[FAIL] ", name, if (is.null(error)) "" else paste0(": ", error), "\n", sep = "")
}
new_case <- function() {
  out <- tempfile("case-", root); dir.create(out); setwd(out)
  config <- file.path(out, "config.yml")
  yaml::write_yaml(list(defaults = list(output_dir = file.path(out, "must-not-exist"))), config)
  Sys.setenv(NLSS_CONFIG_PATH = config)
  out
}
run <- function(module, options, failure = FALSE) {
  log <- tempfile(module, root, ".log")
  started <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
  command <- c(file.path(repo, "scripts/R", paste0(module, ".R")), options)
  status <- system2(file.path(R.home("bin"), "Rscript"), shQuote(command), stdout = log, stderr = log)
  processes[[length(processes) + 1L]] <<- list(command = as.list(command), started_utc = started,
    finished_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"), exit_code = status, log = basename(log))
  text <- paste(readLines(log, warn = FALSE), collapse = "\n")
  check(if (failure) status != 0L else status == 0L, paste("Unexpected exit", status, text))
  check(!dir.exists(file.path(getwd(), "must-not-exist")), "Standalone reader created a workspace")
  text
}
encode <- function(text) jsonlite::base64_enc(memCompress(charToRaw(text), type = "gzip"))
block <- function(text) list(report_block_b64 = encode(text), report_block_encoding = "gzip+base64")
write_log <- function(records, path = file.path(getwd(), "input.jsonl")) {
  writeLines(vapply(records, json, character(1)), path, useBytes = TRUE); path
}
md5 <- function(text) digest::digest(charToRaw(text), algo = "md5", serialize = FALSE)
xor <- function(x, y) {
  pairs <- seq(1L, 31L, 2L)
  paste(sprintf("%02x", bitwXor(strtoi(substring(x, pairs, pairs + 1L), 16L), strtoi(substring(y, pairs, pairs + 1L), 16L))), collapse = "")
}
for (version in 1:3) for (ending in c("\n", "\r\n")) test(paste0("checksum_v", version, if (ending == "\n") "_LF" else "_CRLF"), {
  out <- new_case(); code <- paste(rep("ab", 16L), collapse = ""); lines <- character()
  for (i in 1:3) {
    original <- json(list(module = "fixture", checksum_version = version, log_seq = i, results = list(value = i)))
    sum <- xor(code, md5(paste0(original, ending)))
    if (version >= 2L && i > 1L) sum <- xor(sum, md5(lines[[i - 1L]]))
    if (version == 3L) sum <- xor(sum, md5(as.character(i)))
    lines[[i]] <- paste0(sub("}$", "", original), ',"checksum":"', sum, '"}', ending)
  }
  path <- file.path(out, "input.jsonl"); writeBin(charToRaw(paste(lines, collapse = "")), path); before <- sha(path)
  output <- run("check_integrity", c(path, "--diagnose", "TRUE"))
  check(grepl(paste0(code, " 3"), output, fixed = TRUE), "Independent checksum reference")
  check(!grepl("WARNING: multiple", output, fixed = TRUE), "Clean log reported inconsistent")
  check(identical(before, sha(path)), "Source log changed")
})
for (module in c("check_integrity", "reconstruct_reports")) test(paste0(module, "_strict_cli"), {
  out <- new_case(); path <- write_log(list(block("# Saved\n")))
  run(module, "--help")
  run(module, c(path, "--unknown"), TRUE)
  run(module, c(path, path), TRUE)
  run(module, c(path, if (module == "check_integrity") "--diagnose=flase" else "--overwrite=flase"), TRUE)
  run(module, c(path, if (module == "check_integrity") c("--diagnose", "TRUE", "--diag") else c("--out-dir", out, "--out-dir", out)), TRUE)
})
test("environment_fallback_only_without_explicit_path", {
  out <- new_case(); path <- write_log(list(block("# Saved\n")))
  Sys.setenv(NLSS_INTEGRITY_LOG = path, NLSS_RECONSTRUCT_LOG = path)
  run("check_integrity", character()); run("reconstruct_reports", character())
  run("check_integrity", "missing-file.jsonl", TRUE); run("reconstruct_reports", "missing-file.jsonl", TRUE)
})
test("invalid_json_shapes_do_not_crash_readers", {
  out <- new_case(); path <- file.path(out, "input.jsonl")
  writeLines(c("5", "null", "[]", '{"checksum":["a","b"],"checksum_version":[]}', json(block("# Good\n"))), path)
  run("check_integrity", path); run("reconstruct_reports", path)
  check(identical(bytes(file.path(out, "report_canonical_reconstructed.md")), charToRaw("# Good\n")), "Valid blocks retained")
})
test("byte_exact_reconstruction_and_idempotence", {
  out <- new_case(); expected <- "# Überprüfung\r\n\r\nA semantic interpretation.\nNo trailing newline"
  path <- write_log(list(block(expected))); before <- sha(path)
  run("reconstruct_reports", path); target <- file.path(out, "report_canonical_reconstructed.md")
  check(identical(bytes(target), charToRaw(expected)), "Stored report bytes reproduced without new frontmatter")
  hash <- sha(target); run("reconstruct_reports", path)
  check(identical(hash, sha(target)) && identical(before, sha(path)), "Idempotence/source unchanged")
})
test("no_blocks_preserves_existing_report", {
  out <- new_case(); target <- file.path(out, "report_canonical_reconstructed.md"); writeLines("old research", target)
  before <- sha(target); path <- write_log(list(list(module = "legacy")))
  run("reconstruct_reports", path, TRUE)
  check(identical(before, sha(target)), "Invalid log destroyed existing output")
})
test("utf8_reconstruction_under_C_locale", {
  out <- new_case(); expected <- "# Überprüfung\r\nSemantic text: größer.\n"
  path <- write_log(list(block(expected)))
  prior <- Sys.getenv("LC_ALL", unset = NA_character_)
  Sys.setenv(LC_ALL = "C")
  tryCatch(run("reconstruct_reports", path), finally = {
    if (is.na(prior)) Sys.unsetenv("LC_ALL") else Sys.setenv(LC_ALL = prior)
  })
  check(identical(bytes(file.path(out, "report_canonical_reconstructed.md")), charToRaw(expected)), "UTF-8 bytes changed under C locale")
})
test("explicit_overwrite_and_missing_option_value", {
  out <- new_case(); target <- file.path(out, "report_canonical_reconstructed.md"); writeLines("old research", target)
  before <- sha(target); path <- write_log(list(block("new research")))
  run("reconstruct_reports", c(path, "--out-dir"), TRUE)
  run("reconstruct_reports", path, TRUE); check(identical(before, sha(target)), "Collision overwrote output")
  run("reconstruct_reports", c(path, "--overwrite", "TRUE"))
  check(identical(bytes(target), charToRaw("new research")), "Explicit overwrite failed")
})
test("multiple_finalizations_preserved_and_undated_explicit", {
  out <- new_case()
  meta <- c(list(module = "metaskill_runner", results = list(phase = "finalization", metaskill = "report", intent = "study")),
    block("# Canonical\n"), list(metaskill_report_block_b64 = encode("first report"), metaskill_report_block_encoding = "gzip+base64"))
  second <- meta; second$metaskill_report_block_b64 <- encode("revised report")
  path <- write_log(list(meta, second)); run("reconstruct_reports", path)
  files <- list.files(out, "undated.*reconstructed[.]md$", full.names = TRUE)
  check(length(files) == 2L, "Repeated finalizations collapsed")
  check(setequal(vapply(files, function(p) rawToChar(bytes(p)), character(1)), c("first report", "revised report")), "Semantic revisions changed")
})
test("legacy_metaskill_block_routing", {
  out <- new_case()
  meta <- c(list(module = "metaskill_runner", timestamp_utc = "2026-01-02T03:04:05Z",
    results = list(phase = " finalization ", metaskill = "report", intent = "study", report_block_source = " metaskill_report ")),
    block("semantic report"), list(report_block_full_b64 = encode("canonical report"), report_block_full_encoding = "gzip+base64"))
  path <- write_log(list(meta)); run("reconstruct_reports", path)
  check(identical(bytes(file.path(out, "report_canonical_reconstructed.md")), charToRaw("canonical report")), "Legacy canonical routing")
  check(identical(bytes(file.path(out, "report_20260102_report_study_reconstructed.md")), charToRaw("semantic report")), "Legacy semantic routing")
})
test("generated_suffix_does_not_replace_natural_report_name", {
  out <- new_case()
  records <- lapply(seq_along(c("study", "study_line3", "study")), function(i) c(
    list(module = "metaskill_runner", results = list(phase = "finalization", metaskill = "report", intent = c("study", "study_line3", "study")[[i]])),
    block("canonical\n"), list(metaskill_report_block_b64 = encode(paste0("revision", i)), metaskill_report_block_encoding = " gzip+base64 ")))
  path <- write_log(records); run("reconstruct_reports", path)
  files <- list.files(out, "^report_undated_.*reconstructed[.]md$", full.names = TRUE)
  check(length(files) == 3L && setequal(vapply(files, function(p) rawToChar(bytes(p)), character(1)), paste0("revision", 1:3)), "Generated suffix overwrote a distinct natural report name")
})
test("source_output_collision_refused", {
  out <- new_case(); path <- write_log(list(block("# Saved")), file.path(out, "report_canonical_reconstructed.md")); before <- sha(path)
  run("reconstruct_reports", c(path, "--overwrite", "TRUE"), TRUE)
  check(identical(before, sha(path)), "Source log overwritten")
})
test("symlink_output_refused", {
  out <- new_case(); path <- write_log(list(block("# Saved"))); other <- file.path(out, "untouched.txt"); writeLines("old", other)
  check(file.symlink(other, file.path(out, "report_canonical_reconstructed.md")), "Create test link")
  run("reconstruct_reports", c(path, "--overwrite", "TRUE"), TRUE)
  check(identical(readLines(other), "old"), "Symlink target overwritten")
})
test("invalid_blocks_skip_with_warning", {
  out <- new_case(); path <- write_log(list(block("# Good"), list(report_block_b64 = "not-base64", report_block_encoding = "gzip+base64")))
  output <- run("reconstruct_reports", path)
  check(grepl("base64 decode failed", output, fixed = TRUE), "Missing decode diagnostic")
  check(identical(bytes(file.path(out, "report_canonical_reconstructed.md")), charToRaw("# Good")), "Good block missing")
})
if (!length(results)) stop("No cases selected")
jsonlite::write_json(list(results = results, assertions = assertions, processes = processes,
  passed = all(vapply(results, `[[`, logical(1), "passed"))), file.path(root, "summary.json"), pretty = TRUE, auto_unbox = TRUE)
cat("History: ", sum(vapply(results, `[[`, logical(1), "passed")), "/", length(results), "; ", assertions, " assertions\n", sep = "")
if (keep > 0) {
  old <- sort(list.dirs(dirname(root), recursive = FALSE, full.names = TRUE), decreasing = TRUE)
  if (length(old) > keep) unlink(setdiff(old[(keep + 1L):length(old)], root), recursive = TRUE)
}
if (!all(vapply(results, `[[`, logical(1), "passed"))) quit(status = 1L)
