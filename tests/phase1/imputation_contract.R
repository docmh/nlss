# SPDX-License-Identifier: Apache-2.0
#!/usr/bin/env Rscript
# Offline seeded mice contract regression; no repository configuration mutations.
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[[1]]
script_path <- normalizePath(sub("^--file=", "", script_arg), winslash = "/", mustWork = TRUE)
repo <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
required <- c("yaml", "jsonlite", "arrow", "digest", "mice")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Imputation contract tests require: ", paste(missing, collapse = ", "))
source(file.path(repo, "scripts", "R", "lib", "config.R"))
tests_cfg <- yaml::read_yaml(file.path(repo, "tests", "tests.yml"))$tests
run_root <- Sys.getenv("NLSS_TEST_ROOT", "")
if (!nzchar(run_root)) run_root <- file.path(repo, tests_cfg$output_dir, paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-phase1-impute"))
run_root <- normalizePath(run_root, winslash = "/", mustWork = FALSE)
dir.create(file.path(run_root, "tmp"), recursive = TRUE, showWarnings = FALSE)
work <- tempfile("imputation-contract-", tmpdir = file.path(run_root, "tmp"))
dir.create(work, recursive = TRUE, showWarnings = FALSE)
frozen_config <- get_builtin_config()
frozen_config$logging$include_checksum <- FALSE
frozen_config$logging$include_user_prompt <- TRUE
frozen_path <- file.path(work, "config.yml")
yaml::write_yaml(frozen_config, frozen_path)
Sys.setenv(NLSS_CONFIG_PATH = frozen_path)

checks <- character()
check <- function(value, message) {
  if (!isTRUE(value)) stop("FAIL: ", message, call. = FALSE)
  checks <<- c(checks, paste("PASS", message))
  cat(tail(checks, 1), "\n")
}
near <- function(actual, expected, message) {
  check(length(actual) == length(expected) && identical(is.na(actual), is.na(expected)) &&
        all(abs(actual[!is.na(expected)] - expected[!is.na(expected)]) < 1e-12), message)
}
run <- function(project, options) {
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(project)
  log <- tempfile("command-", tmpdir = work, fileext = ".log")
  status <- system2(file.path(R.home("bin"), "Rscript"),
    c(shQuote(file.path(repo, "scripts", "R", "impute.R")), shQuote(options)), stdout = log, stderr = log)
  output <- paste(readLines(log, warn = FALSE), collapse = "\n")
  if (status != 0L) stop("impute.R failed:\n", output)
  invisible(output)
}
new_project <- function(name) {
  project <- file.path(work, name, "project")
  dir.create(project, recursive = TRUE)
  yaml::write_yaml(list(version = 1, datasets = list()), file.path(project, "nlss-workspace.yml"))
  project
}
latest_result <- function(dataset) {
  rows <- lapply(readLines(file.path(dataset, "analysis_log.jsonl"), warn = FALSE), jsonlite::fromJSON)
  tail(Filter(function(x) identical(x$module, "impute"), rows), 1)[[1]]
}

set.seed(7601)
n <- 90L
original <- data.frame(x = rnorm(n), y = rnorm(n), group = factor(sample(c("A", "B", "C"), n, TRUE)))
original$y <- 0.45 * original$x + original$y
original$x[c(1, 5, 9, 15, 21, 32)] <- NA_real_
original$y[c(2, 6, 10, 16, 22, 33)] <- NA_real_
original$group[c(3, 7, 11, 17, 23, 34)] <- NA
source_path <- file.path(work, "sample.rds")
saveRDS(original, source_path)
seed <- 811L
expected_mids <- mice::mice(original, m = 3, maxit = 2, printFlag = FALSE, seed = seed)
expected_completed <- mice::complete(expected_mids, action = "all")
project <- new_project("seeded")
output <- run(project, c("--rds", source_path, "--vars", "x,y,group", "--engine", "mice", "--m", "3", "--maxit", "2", "--seed", as.character(seed)))
dataset <- file.path(project, "sample")
entry <- latest_result(dataset)
result <- entry$results
artifact <- result$imputation_artifact
check(identical(result$completion_mode, "single_completion"), "single-completion status is explicit")
check(identical(result$completion_aggregation, "mean_numeric_mode_categorical"), "aggregation method is explicit")
check(identical(result$inference_pooled, FALSE), "no inference pooling is claimed")
check(grepl("SINGLE COMPLETION ONLY", output, fixed = TRUE), "console warns about inference limitation")
check(grepl("not Rubin-pooled", result$inference_warning, fixed = TRUE), "machine-readable warning is explicit")
check(!is.null(artifact) && artifact$m == 3, "all three imputations are recorded")
check(!grepl("^/|^[A-Za-z]:", artifact$path), "artifact path is workspace-relative")
artifact_path <- file.path(project, artifact$path)
check(file.exists(artifact_path), "mids artifact exists")
check(identical(digest::digest(file = artifact_path, algo = "sha256"), artifact$sha256), "artifact SHA-256 matches its content")
check(identical(basename(dirname(artifact_path)), paste0("mice-", artifact$sha256)), "artifact has immutable content-addressed location")
stored <- readRDS(artifact_path)
check(inherits(stored, "mids") && stored$m == 3 && stored$iteration == 2, "stored object retains mids class, m, and iterations")
check(identical(stored$method, expected_mids$method), "mice methods remain unchanged")
check(identical(stored$predictorMatrix, expected_mids$predictorMatrix), "mice predictor matrix remains unchanged")
for (name in names(original)) {
  check(identical(stored$imp[[name]], expected_mids$imp[[name]]), paste("independent seeded imputation draws match for", name))
}
actual <- as.data.frame(arrow::read_parquet(file.path(dataset, "sample.parquet")))
for (name in c("x", "y")) {
  expected <- original[[name]]
  missing_rows <- which(is.na(expected))
  expected[missing_rows] <- Reduce(`+`, lapply(expected_completed, function(frame) frame[[name]][missing_rows])) / length(expected_completed)
  near(actual[[paste0(name, "_imp")]], expected, paste("legacy single numeric completion unchanged for", name))
  near(actual[[name]], original[[name]], paste("original numeric column unchanged for", name))
}
expected_group <- as.character(original$group)
for (row in which(is.na(expected_group))) {
  draws <- vapply(expected_completed, function(frame) as.character(frame$group[row]), character(1))
  first_order <- unique(draws)
  expected_group[row] <- first_order[which.max(vapply(first_order, function(value) sum(draws == value), integer(1)))]
}
check(identical(as.character(actual$group_imp), expected_group), "legacy categorical modal completion unchanged")
check(identical(as.character(actual$group), as.character(original$group)), "original categorical column unchanged")
metadata <- jsonlite::read_json(file.path(dirname(artifact_path), "metadata.json"), simplifyVector = TRUE)
check(identical(metadata$sha256, artifact$sha256) && metadata$m == 3 && !metadata$inference_pooled,
      "artifact sidecar records content and inference contract")
check(identical(metadata$dataset$version_id, entry$dataset$version_id), "artifact is linked to the exact pre-imputation dataset version")
markdown <- paste(readLines(file.path(dataset, "report_canonical.md"), warn = FALSE), collapse = "\n")
check(grepl("## Imputation inference limitation", markdown, fixed = TRUE) && grepl(artifact$sha256, markdown, fixed = TRUE),
      "Markdown includes mandatory warning and artifact hash")
block <- rawToChar(memDecompress(jsonlite::base64_dec(entry$report_block_b64), type = "gzip"))
check(grepl("SINGLE COMPLETION ONLY", block, fixed = TRUE), "reconstructible report block retains inference warning")

# Different seeds must publish a new artifact; the previous object's bytes and
# metadata must be retained even when the working parquet gains more columns.
original_hash <- digest::digest(file = artifact_path, algo = "sha256")
original_metadata_hash <- digest::digest(file = file.path(dirname(artifact_path), "metadata.json"), algo = "sha256")
run(project, c("--parquet", file.path(dataset, "sample.parquet"), "--vars", "x,y,group", "--engine", "mice",
               "--m", "3", "--maxit", "2", "--seed", "812"))
second <- latest_result(dataset)$results$imputation_artifact
check(!identical(second$path, artifact$path), "different imputation draws get a distinct artifact")
check(identical(digest::digest(file = artifact_path, algo = "sha256"), original_hash) &&
      identical(digest::digest(file = file.path(dirname(artifact_path), "metadata.json"), algo = "sha256"), original_metadata_hash),
      "previous imputation artifact and metadata are never overwritten")

# A user template may omit notes/narrative, but the scientific limitation remains
# mandatory and the raw imputations remain preserved even with JSONL disabled.
custom_project <- new_project("custom-template")
custom_template <- file.path(work, "minimal-template.md")
writeLines(c("# Custom imputation presentation", "", "Only a custom heading."), custom_template)
run(custom_project, c("--rds", source_path, "--vars", "x,y,group", "--engine", "mice", "--m", "3", "--maxit", "2",
                      "--seed", as.character(seed), "--template", custom_template, "--log", "FALSE"))
custom_dataset <- file.path(custom_project, "sample")
custom_report <- paste(readLines(file.path(custom_dataset, "report_canonical.md"), warn = FALSE), collapse = "\n")
check(grepl("Custom imputation presentation", custom_report, fixed = TRUE) && grepl("SINGLE COMPLETION ONLY", custom_report, fixed = TRUE),
      "custom template cannot suppress inference warning")
check(length(list.files(file.path(custom_dataset, "imputations"), pattern = "mids[.]rds$", recursive = TRUE)) == 1L,
      "mids artifact preservation does not require JSONL logging")

simple_project <- new_project("simple")
run(simple_project, c("--rds", source_path, "--vars", "x,y,group", "--engine", "simple", "--numeric-method", "median"))
simple_result <- latest_result(file.path(simple_project, "sample"))$results
check(identical(simple_result$completion_mode, "single_completion") && !simple_result$inference_pooled &&
      is.null(simple_result$imputation_artifact), "simple engine retains its completion mode without claiming a mids artifact")
writeLines(checks, file.path(run_root, "phase1-imputation-contract.log"))
cat("Imputation contract checks:", length(checks), "passed.\n")
