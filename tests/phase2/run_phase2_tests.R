#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# End-to-end checks of public CLI/storage contracts. Numerical expectations use
# base R/stats and direct mice workflows, never NLSS computation helpers.

args <- commandArgs(trailingOnly = TRUE)
mi_only <- "--mi-only" %in% args
if (sum(args == "--mi-only") > 1L) stop("Repeated runner option: --mi-only")
args <- args[args != "--mi-only"]
suite_name <- if (mi_only) "phase2-mi" else "phase2"
script <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])
repo <- normalizePath(file.path(dirname(script[1]), "../.."), winslash = "/")
arg_value <- function(name, default = NULL) {
  index <- which(args == name)
  if (!length(index)) return(default)
  if (length(index) > 1L) stop("Repeated runner option: ", name)
  if (index == length(args) || startsWith(args[index + 1L], "--")) stop("Missing value for ", name)
  args[index + 1L]
}
if ("--help" %in% args) {
  cat("Usage: Rscript tests/phase2/run_phase2_tests.R [--root PATH] [--keep N] [--mi-only] [--match REGEX]\n",
      "--mi-only runs the MI core and end-to-end model acceptance cases only.\n",
      "Honors NLSS_TEST_ROOT, NLSS_KEEP_RUNS and NLSS_TESTS_CONFIG. No network required.\n", sep = "")
  quit(status = 0)
}
if (length(args) && (length(args) %% 2L || any(!args[seq(1L, length(args), by = 2L)] %in% c("--root", "--keep", "--match")))) {
  stop("Unknown or incomplete runner option. Use --help.")
}
selected_pattern <- arg_value("--match", ".*")
invisible(grepl(selected_pattern, "validate regex"))
required <- c("yaml", "jsonlite", "arrow", "digest", "mice")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Phase 2 tests require installed packages: ", paste(missing, collapse = ", "))
absolute <- function(path) {
  if (!grepl("^(/|[A-Za-z]:|\\\\)", path)) path <- file.path(repo, path)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}
test_config <- yaml::read_yaml(absolute(Sys.getenv("NLSS_TESTS_CONFIG", "tests/tests.yml")))$tests
configured <- function(value, fallback) if (is.null(value) || !nzchar(as.character(value))) fallback else value
output_base <- absolute(configured(test_config$output_dir, "outputs/test-runs"))
forced_root <- arg_value("--root", Sys.getenv("NLSS_TEST_ROOT", ""))
run_root <- if (nzchar(forced_root)) absolute(forced_root) else file.path(output_base, format(Sys.time(), "%Y%m%d%H%M%S"))
keep <- suppressWarnings(as.integer(arg_value("--keep", Sys.getenv("NLSS_KEEP_RUNS", as.character(test_config$keep_runs_default)))))
if (length(keep) != 1L || is.na(keep) || keep < 0L) stop("--keep/NLSS_KEEP_RUNS must be a non-negative integer.")
work <- file.path(run_root, suite_name, paste0("run-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid()))
dir.create(work, recursive = TRUE, showWarnings = FALSE)
work <- normalizePath(work, winslash = "/")

# Tests change only this private YAML, never the shipped configuration. Each
# scenario resets it so a failed replay assertion cannot poison later cases.
baseline_config <- yaml::read_yaml(file.path(repo, "scripts/config.yml"))
baseline_config$defaults$digits <- 5L
private_config <- file.path(work, "config.yml")
reset_config <- function() yaml::write_yaml(baseline_config, private_config)
reset_config()
Sys.setenv(NLSS_CONFIG_PATH = private_config)
golden <- read.csv(absolute(test_config$golden_dataset), stringsAsFactors = FALSE)
numeric_data <- data.frame(x = golden$x1, z = golden$x2, y = golden$outcome_reg)
numeric_data <- numeric_data[stats::complete.cases(numeric_data), , drop = FALSE]
sha256 <- function(path) digest::digest(file = path, algo = "sha256")
read_json <- function(path) jsonlite::fromJSON(path, simplifyVector = TRUE)
text_file <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
results <- list()
check <- function(condition, message) if (!isTRUE(condition)) stop(message, call. = FALSE)
near <- function(actual, expected, label, tolerance = 1e-8) {
  actual <- as.numeric(actual)
  expected <- as.numeric(expected)
  check(length(actual) == length(expected) && length(actual) > 0L, paste(label, "has unexpected length"))
  check(identical(is.na(actual), is.na(expected)), paste(label, "has different missing values"))
  valid <- !is.na(expected)
  check(all(is.finite(actual[valid])) && all(abs(actual[valid] - expected[valid]) <= tolerance),
        paste(label, "differs from the independent expected values"))
}
test <- function(name, code) {
  if (!grepl(selected_pattern, name)) return(invisible(NULL))
  selected_mi_case <- name %in% c("independent_MI_pooling_core_contract", "seeded_mice_artifact_prepares_independent_MI_reference") ||
    startsWith(name, "MI_CLI_")
  if (mi_only && !selected_mi_case) return(invisible(NULL))
  previous <- getwd()
  on.exit(setwd(previous), add = TRUE)
  reset_config()
  start <- proc.time()[["elapsed"]]
  error <- tryCatch({force(code); NULL}, error = function(e) conditionMessage(e))
  results[[length(results) + 1L]] <<- list(test = name, passed = is.null(error),
    seconds = unname(proc.time()[["elapsed"]] - start), message = if (is.null(error)) "OK" else error)
  cat(if (is.null(error)) "[PASS] " else "[FAIL] ", name,
      if (is.null(error)) "" else paste0(": ", error), "\n", sep = "")
}
with_environment <- function(values, code) {
  prior <- Sys.getenv(names(values), unset = NA_character_, names = TRUE)
  on.exit({
    for (name in names(prior)) {
      if (is.na(prior[[name]])) Sys.unsetenv(name) else do.call(Sys.setenv, setNames(list(prior[[name]]), name))
    }
  }, add = TRUE)
  do.call(Sys.setenv, as.list(values))
  force(code)
}
new_case <- function(name, data = numeric_data) {
  base <- file.path(work, "cases", name)
  project <- file.path(base, "project")
  dir.create(project, recursive = TRUE)
  yaml::write_yaml(list(version = 1L, datasets = list()), file.path(project, "nlss-workspace.yml"))
  input <- file.path(base, "sample.csv")
  write.csv(data, input, row.names = FALSE)
  setwd(project)
  list(base = base, project = project, input = input, dataset = file.path(project, "sample"), data = data)
}
run_module <- function(module, options, expect_failure = FALSE, error_pattern = NULL, installation = repo) {
  destination <- tempfile(pattern = paste0(module, "-"), tmpdir = dirname(getwd()), fileext = ".log")
  status <- system2(file.path(R.home("bin"), "Rscript"),
    c(shQuote(file.path(installation, "scripts/R", paste0(module, ".R"))), shQuote(options)),
    stdout = destination, stderr = destination)
  output <- text_file(destination)
  if (expect_failure) {
    check(status != 0L, paste("Expected", module, "to fail, but it succeeded"))
    check(nzchar(output), "An expected failure supplied no feedback")
    if (!is.null(error_pattern)) check(grepl(error_pattern, output, fixed = TRUE),
      paste("Expected failure feedback:", error_pattern, "but received:", output))
  } else check(status == 0L, paste(module, "failed:", output))
  invisible(list(status = status, output = output, log = destination))
}
check_descriptive <- function(result, expected) {
  summary <- result$summary_df
  for (variable in c("x", "y")) {
    row <- summary[summary$variable == variable, , drop = FALSE]
    check(nrow(row) == 1L, paste("Expected one descriptive row for", variable))
    values <- expected[[variable]]
    valid <- values[!is.na(values)]
    ci <- stats::t.test(valid)$conf.int
    wanted <- list(n = length(valid), missing_n = sum(is.na(values)), mean = mean(valid), sd = stats::sd(valid),
                   variance = stats::var(valid), median = stats::median(valid), min = min(valid), max = max(valid),
                   ci_low = ci[1], ci_high = ci[2])
    for (field in names(wanted)) near(row[[field]], wanted[[field]], paste(variable, field))
  }
}
check_lm <- function(result, expected, confidence = 0.95) {
  fitted <- stats::lm(y ~ x + z, data = expected, na.action = stats::na.omit)
  independent <- summary(fitted)
  coefficients <- result$coefficients_df
  check(nrow(coefficients) == nrow(independent$coefficients), "Unexpected regression coefficient count")
  coefficients <- coefficients[match(rownames(independent$coefficients), coefficients$term), , drop = FALSE]
  for (field in c("estimate", "se", "stat", "p")) {
    near(coefficients[[field]], independent$coefficients[, match(field, c("estimate", "se", "stat", "p"))],
         paste("Regression", field))
  }
  intervals <- stats::confint(fitted, level = confidence)
  near(coefficients$ci_low, intervals[, 1], "Regression lower CI")
  near(coefficients$ci_high, intervals[, 2], "Regression upper CI")
  near(result$summary_df$n, stats::nobs(fitted), "Regression N")
  near(result$summary_df$r2, independent$r.squared, "Regression R squared")
  near(result$summary_df$adj_r2, independent$adj.r.squared, "Regression adjusted R squared")
}
markdown_table <- function(markdown, header_key) {
  lines <- strsplit(markdown, "\n", fixed = TRUE)[[1]]
  cells <- function(line) trimws(strsplit(sub("[|][[:space:]]*$", "", sub("^[[:space:]]*[|]", "", line)), "|", fixed = TRUE)[[1]])
  starts <- which(vapply(lines, function(line) grepl("^[[:space:]]*[|]", line) && header_key %in% cells(line), logical(1)))
  check(length(starts) > 0L, paste("Markdown table has no", header_key, "header"))
  start <- starts[1]
  rows <- list()
  index <- start + 2L
  while (index <= length(lines) && grepl("^[[:space:]]*[|]", lines[index])) {
    rows[[length(rows) + 1L]] <- cells(lines[index])
    index <- index + 1L
  }
  check(length(rows) > 0L, "Markdown table has no data rows")
  frame <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(frame) <- cells(lines[start])
  frame
}
check_descriptive_markdown <- function(result, markdown, digits) {
  table <- markdown_table(markdown, "Variable")
  expected <- result$summary_df
  table <- table[match(expected$variable, table$Variable), , drop = FALSE]
  for (mapping in list(c("n", "n"), c("M", "mean"), c("SD", "sd"), c("Min", "min"), c("Max", "max"))) {
    near(table[[mapping[1]]], round(expected[[mapping[2]]], digits), paste("Markdown", mapping[1]), tolerance = 1e-10)
  }
}

run_directories <- function(project) {
  requests <- list.files(project, pattern = "^request[.]json$", full.names = TRUE, recursive = TRUE)
  dirname(requests[grepl("/runs/[^/]+/request[.]json$", requests)])
}
new_run <- function(context, module, options, expect_failure = FALSE, installation = repo) {
  before <- run_directories(context$project)
  command <- run_module(module, options, expect_failure = expect_failure, installation = installation)
  added <- setdiff(run_directories(context$project), before)
  check(length(added) == 1L, paste("Expected exactly one new recorded run for", module, "but found", length(added)))
  bundle <- list(path = added, request_path = file.path(added, "request.json"),
                 result_path = file.path(added, "result.json"), output_path = file.path(added, "output.md"), command = command)
  check(file.exists(bundle$result_path), "A recorded run has no terminal result.json")
  bundle$request <- read_json(bundle$request_path)
  bundle$result <- read_json(bundle$result_path)
  if (expect_failure) {
    check(identical(bundle$result$status, "failed"), "Failed analysis is not marked failed")
    check(!is.null(bundle$result$error) && nzchar(paste(unlist(bundle$result$error), collapse = "")),
          "Failed analysis has no machine-readable error context")
    check(!file.exists(bundle$output_path), "Failed analysis published a normal completed output.md")
  } else {
    check(identical(bundle$result$status, "completed"), "Successful analysis is not marked completed")
    check(file.exists(bundle$output_path) && file.info(bundle$output_path)$size > 0, "Completed run has no Markdown output")
    check(is.null(bundle$result$error), "Completed result unexpectedly carries an error")
    bundle$markdown <- text_file(bundle$output_path)
  }
  bundle
}
check_bundle <- function(bundle, context, module) {
  request <- bundle$request
  result <- bundle$result
  check(request$schema_version == 1L && result$schema_version == 1L, "Unsupported run contract schema")
  check(identical(request$module, module) && identical(result$module, module), "Recorded module differs from CLI")
  check(identical(result$run_id, basename(bundle$path)), "Result run ID differs from its directory")
  check(is.list(request$options) && length(request$options) > 0L, "Resolved scientific options were not saved")
  check(is.list(request$configuration) && identical(as.integer(request$configuration$defaults$digits), 5L),
        "Effective private configuration was not saved")
  check(is.list(request$environment) && length(request$environment) > 0L, "R environment was not saved")
  check(any(grepl("R version|4[.]|5[.]", as.character(unlist(request$environment)))), "Recorded environment has no R version")
  check(!is.null(request$environment$packages) && length(request$environment$packages) > 0L,
        "Package versions were not saved")
  check(is.list(request$rng) && length(request$rng$kind) > 0L, "RNG kind was not saved")
  check(is.character(request$code_sha256) && grepl("^[a-f0-9]{64}$", request$code_sha256), "Code SHA-256 was not saved")
  check(!is.null(request$templates), "Resolved templates were not saved")
  for (template in request$templates) {
    path <- file.path(bundle$path, template$path)
    check(file.exists(path) && identical(sha256(path), template$sha256), "Preserved template differs from its request hash")
  }
  check(!is.null(result$artifacts) && length(result$artifacts) > 0L, "Result artifact references were not saved")
  for (artifact in result$artifacts) {
    check(is.list(artifact) && is.character(artifact$path) && is.character(artifact$sha256), "Result artifact has no path/hash pair")
    path <- file.path(bundle$path, artifact$path)
    check(file.exists(path) && identical(sha256(path), artifact$sha256), paste("Recorded artifact failed byte verification:", artifact$path))
  }
  for (field in c("data_sha256", "dictionary_sha256", "snapshot_path", "dictionary_path", "version_id")) {
    check(is.character(request$dataset[[field]]) && nzchar(request$dataset[[field]]), paste("Request has no dataset", field))
    check(identical(request$dataset[[field]], result$dataset[[field]]), paste("Result and request differ for", field))
  }
  check(identical(sha256(file.path(context$project, request$dataset$snapshot_path)), request$dataset$data_sha256),
        "Request data hash differs from preserved input bytes")
  check(identical(sha256(file.path(context$project, request$dataset$dictionary_path)), request$dataset$dictionary_sha256),
        "Request dictionary hash differs from preserved dictionary bytes")
  invisible(bundle)
}
replay <- function(context, bundle) new_run(context, "replay_run", c("--request", bundle$request_path))
successful_runs <- function(context) {
  dirs <- run_directories(context$project)
  Filter(function(path) file.exists(file.path(path, "result.json")) &&
           identical(read_json(file.path(path, "result.json"))$status, "completed"), dirs)
}

run_contract_script <- function(path, label) {
  script_path <- absolute(path)
  check(file.exists(script_path), paste("Missing registered contract script:", path))
  destination <- file.path(work, paste0(label, ".log"))
  status <- system2(file.path(R.home("bin"), "Rscript"), shQuote(script_path),
    env = paste0("NLSS_TEST_ROOT=", shQuote(work)), stdout = destination, stderr = destination)
  check(status == 0L, paste(label, "failed:\n", text_file(destination)))
}
test("bootstrap_and_canonical_defaults_contract", {
  run_contract_script(configured(test_config$phase2$bootstrap_config, "tests/phase2/bootstrap_config.R"), "bootstrap-config")
})
test("independent_MI_pooling_core_contract", {
  run_contract_script(configured(test_config$phase2$mi_pool, "tests/phase2/mi_pool.R"), "mi-pool")
})
test("publication_failure_rolls_back_legacy_projections", {
  run_contract_script(configured(test_config$phase2$publication_contract, "tests/phase2/publication_contract.R"), "publication-contract")
})

test("old_descriptive_CLI_records_complete_resolved_bundle", {
  context <- new_case("descriptive-contract")
  bundle <- new_run(context, "descriptive_stats", c("--csv", context$input, "--vars", "x,y"))
  check_bundle(bundle, context, "descriptive_stats")
  check_descriptive(bundle$result$results, context$data)
  check_descriptive_markdown(bundle$result$results, bundle$markdown, 5L)
  check(file.exists(file.path(context$dataset, "analysis_log.jsonl")), "Legacy JSONL output disappeared")
  check(file.exists(file.path(context$dataset, "report_canonical.md")), "Legacy Markdown output disappeared")
  check(!is.null(bundle$request$options$trim) && !is.null(bundle$request$options$digits),
        "Implicit descriptive defaults are absent from resolved options")
})

test("old_regression_CLI_matches_independent_lm", {
  context <- new_case("regression-contract")
  bundle <- new_run(context, "regression", c("--csv", context$input, "--dv", "y", "--ivs", "x,z", "--conf-level", "0.90"))
  check_bundle(bundle, context, "regression")
  check_lm(bundle$result$results, context$data, 0.90)
  near(bundle$request$options$conf_level, 0.90, "Resolved confidence level")
  check(!is.null(bundle$request$options$family) && !is.null(bundle$request$options$center),
        "Implicit regression defaults are absent from resolved options")
  table <- markdown_table(bundle$markdown, "Predictor")
  coefficients <- bundle$result$results$coefficients_df
  table <- table[match(coefficients$term, table$Predictor), , drop = FALSE]
  for (mapping in list(c("b", "estimate"), c("SE", "se"), c("CI low", "ci_low"), c("CI high", "ci_high"))) {
    near(table[[mapping[1]]], round(coefficients[[mapping[2]]], 5), paste("Markdown regression", mapping[1]), tolerance = 1e-10)
  }
})

test("seeded_bootstrap_matches_direct_stats_and_repeats_exactly", {
  context <- new_case("bootstrap")
  options <- c("--csv", context$input, "--dv", "y", "--ivs", "x,z", "--bootstrap", "TRUE",
               "--bootstrap-samples", "80", "--seed", "9631", "--conf-level", "0.90")
  first <- new_run(context, "regression", options)
  second <- new_run(context, "regression", options)
  check(identical(first$result$results, second$result$results), "Seeded identical commands produced different numeric results")
  check(identical(first$markdown, second$markdown), "Seeded identical commands produced different deterministic Markdown")
  set.seed(9631L)
  draws <- replicate(80L, {
    rows <- sample.int(nrow(context$data), replace = TRUE)
    stats::coef(stats::lm(y ~ x + z, data = context$data[rows, , drop = FALSE]))
  })
  intervals <- t(apply(draws, 1L, stats::quantile, probs = c(0.05, 0.95)))
  coefficients <- first$result$results$coefficients_df
  intervals <- intervals[match(coefficients$term, rownames(intervals)), , drop = FALSE]
  near(coefficients$boot_ci_low, intervals[, 1], "Independent bootstrap lower CI")
  near(coefficients$boot_ci_high, intervals[, 2], "Independent bootstrap upper CI")
  near(first$request$rng$seed, 9631, "Saved bootstrap seed")
  check(length(first$request$rng$state) > 0L, "Bootstrap RNG state was not recorded")
  again <- replay(context, first)
  check(identical(first$result$results, again$result$results), "Bootstrap replay changed numeric results")
  check(identical(first$markdown, again$markdown), "Bootstrap replay changed deterministic Markdown")
})

test("bootstrap_without_explicit_seed_still_saves_replayable_rng", {
  context <- new_case("bootstrap-implicit-seed")
  original <- new_run(context, "regression", c("--csv", context$input, "--dv", "y", "--ivs", "x,z",
                     "--bootstrap", "TRUE", "--bootstrap-samples", "40"))
  check(length(original$request$rng$state) > 0L, "An unseeded stochastic request did not preserve its RNG state")
  rerun <- replay(context, original)
  check(identical(original$result$results, rerun$result$results), "Replay failed to reproduce implicitly seeded bootstrap")
  check(identical(original$markdown, rerun$markdown), "Implicitly seeded bootstrap Markdown changed on replay")
})

test("replay_ignores_changed_source_working_copy_and_live_configuration", {
  context <- new_case("replay-frozen-inputs")
  original <- new_run(context, "descriptive_stats", c("--csv", context$input, "--vars", "x,y"))
  request_hash <- sha256(original$request_path)
  changed <- context$data
  changed$x <- changed$x + 1000
  changed$y <- changed$y - 500
  write.csv(changed, context$input, row.names = FALSE)
  arrow::write_parquet(changed, file.path(context$dataset, "sample.parquet"))
  working_hash <- sha256(file.path(context$dataset, "sample.parquet"))
  edited_config <- baseline_config
  edited_config$defaults$digits <- 1L
  edited_config$modules$descriptive_stats$trim <- 0.25
  yaml::write_yaml(edited_config, private_config)
  rerun <- replay(context, original)
  check(identical(original$result$results, rerun$result$results), "Replay adopted changed live input or configuration")
  check(identical(original$markdown, rerun$markdown), "Replay Markdown differs after unrelated live changes")
  check(identical(original$request$dataset, rerun$request$dataset), "Replay changed frozen dataset identity")
  check(identical(original$request$configuration, rerun$request$configuration), "Replay did not retain saved effective configuration")
  check(identical(sha256(original$request_path), request_hash), "Replay overwrote its original request")
  check(identical(sha256(file.path(context$dataset, "sample.parquet")), working_hash), "Replay overwrote the current working data")
})

test("replay_uses_preserved_template_after_original_template_changes", {
  context <- new_case("replay-template")
  template <- file.path(context$base, "custom-template.md")
  writeLines(c("# ORIGINAL_PHASE2_TEMPLATE", "", "{{table_body}}", "", "{{narrative}}"), template)
  original <- new_run(context, "descriptive_stats", c("--csv", context$input, "--vars", "x,y", "--template", template))
  check(grepl("ORIGINAL_PHASE2_TEMPLATE", original$markdown, fixed = TRUE), "Custom template was not used initially")
  writeLines(c("# CHANGED_PHASE2_TEMPLATE", "", "Current live template must not change a replay."), template)
  rerun <- replay(context, original)
  check(identical(original$markdown, rerun$markdown), "Replay reread changed live template instead of its saved template")
  check(!grepl("CHANGED_PHASE2_TEMPLATE", rerun$markdown, fixed = TRUE), "Live template change leaked into replay")
})

test("replay_preserves_recorded_R_numeric_formatting_options", {
  context <- new_case("replay-formatting-options")
  original <- new_run(context, "descriptive_stats", c("--csv", context$input, "--vars", "x,y"))
  profile <- file.path(context$base, "changed-profile.R")
  writeLines("options(OutDec = ',', scipen = -10, digits = 3, width = 30)", profile)
  with_environment(list(R_PROFILE_USER = profile), {
    rerun <- replay(context, original)
    check(identical(original$result$results, rerun$result$results), "Changing the caller's R profile changed replayed results")
    check(identical(original$markdown, rerun$markdown), "Replay inherited changed R formatting options")
    check(identical(original$request$environment$options, rerun$request$environment$options),
          "Replay did not preserve recorded R calculation/formatting options")
  })
})

for (target in c("snapshot_path", "dictionary_path")) {
  test(paste0("corrupted_", target, "_refuses_replay_without_success"), {
    context <- new_case(paste0("corrupt-", target))
    original <- new_run(context, "descriptive_stats", c("--csv", context$input, "--vars", "x,y"))
    successful_before <- successful_runs(context)
    path <- file.path(context$project, original$request$dataset[[target]])
    content <- readBin(path, "raw", n = file.info(path)$size)
    writeBin(c(content, charToRaw("corrupted")), path)
    command <- run_module("replay_run", c("--request", original$request_path), expect_failure = TRUE)
    check(grepl("hash|integrity|checksum", command$output, ignore.case = TRUE), "Corrupt-input failure has no integrity explanation")
    check(identical(successful_runs(context), successful_before), "Corrupt input was published as a completed analysis")
  })
}

test("modified_saved_request_is_not_silently_replayed_as_original", {
  context <- new_case("request-integrity")
  original <- new_run(context, "descriptive_stats", c("--csv", context$input, "--vars", "x,y"))
  before <- successful_runs(context)
  modified <- original$request
  modified$options$trim <- 0.25
  modified$cli$trim <- "0.25"
  jsonlite::write_json(modified, original$request_path, pretty = TRUE, auto_unbox = TRUE, digits = NA)
  command <- run_module("replay_run", c("--request", original$request_path), expect_failure = TRUE)
  check(grepl("integrity", command$output, ignore.case = TRUE), "Modified request has no integrity-refusal explanation")
  check(identical(successful_runs(context), before), "Modified saved request produced a new completed replay")
})

test("invalid_analysis_has_terminal_failed_status_without_normal_output", {
  context <- new_case("failed-analysis")
  original <- new_run(context, "regression", c("--csv", context$input, "--dv", "y", "--ivs", "x,z"))
  successful_before <- successful_runs(context)
  report_hash <- sha256(file.path(context$dataset, "report_canonical.md"))
  failed <- new_run(context, "regression", c("--csv", context$input, "--dv", "y", "--ivs", "misspelled_predictor"), expect_failure = TRUE)
  check(grepl("misspelled_predictor", paste(unlist(failed$result$error), collapse = " "), fixed = TRUE),
        "Recorded failure omitted the invalid analysis variable")
  check(identical(successful_runs(context), successful_before), "Failed analysis added a completed run")
  check(identical(sha256(file.path(context$dataset, "report_canonical.md")), report_hash), "Failed analysis altered prior completed Markdown")
  check(identical(read_json(original$result_path)$status, "completed"), "Failed analysis invalidated an existing completed run")
})

test("invalid_scientific_option_domains_never_publish_completed_runs", {
  context <- new_case("invalid-option-domains")
  new_run(context, "regression", c("--csv", context$input, "--dv", "y", "--ivs", "x,z"))
  before <- successful_runs(context)
  bad <- list(
    list(module = "regression", options = c("--dv", "y", "--ivs", "x,z", "--conf-level", "1.5")),
    list(module = "regression", options = c("--dv", "y", "--ivs", "x,z", "--bootstrap", "TRUE", "--bootstrap-samples", "3.5")),
    list(module = "descriptive_stats", options = c("--vars", "x,y", "--outlier-z", "Inf")),
    list(module = "descriptive_stats", options = c("--vars", "x,y", "--digits", "many"))
  )
  for (request in bad) {
    run_module(request$module, c("--csv", context$input, request$options), expect_failure = TRUE)
    check(identical(successful_runs(context), before), paste(request$module, "published invalid scientific options"))
  }
})

test("analysis_warnings_are_retained_in_machine_readable_result", {
  context <- new_case("warning-capture", data.frame(x = seq_len(30L), z = sin(seq_len(30L)), y = 1 + 2 * seq_len(30L)))
  bundle <- new_run(context, "regression", c("--csv", context$input, "--dv", "y", "--ivs", "x,z"))
  warnings <- paste(unlist(bundle$result$warnings), collapse = " ")
  check(grepl("essentially perfect fit", warnings, fixed = TRUE), "A standard lm warning was not recorded")
})

test("legacy_log_FALSE_does_not_disable_mandatory_run_provenance", {
  context <- new_case("mandatory-audit")
  bundle <- new_run(context, "descriptive_stats", c("--csv", context$input, "--vars", "x,y", "--log", "FALSE"))
  check_bundle(bundle, context, "descriptive_stats")
  check_descriptive(bundle$result$results, context$data)
  check(!file.exists(file.path(context$dataset, "analysis_log.jsonl")), "--log FALSE unexpectedly wrote legacy JSONL")
  check(file.exists(file.path(context$dataset, "report_canonical.md")), "--log FALSE unexpectedly suppressed legacy Markdown")
})

test("user_prompt_privacy_optout_applies_to_mandatory_run_bundle", {
  context <- new_case("prompt-privacy")
  confidential_prompt <- "PRIVATE_PHASE2_PROMPT_MUST_NOT_BE_STORED"
  config <- baseline_config
  config$logging$include_user_prompt <- FALSE
  yaml::write_yaml(config, private_config)
  bundle <- new_run(context, "descriptive_stats", c("--csv", context$input, "--vars", "x,y", "--log", "FALSE",
                    "--user-prompt", confidential_prompt))
  check_bundle(bundle, context, "descriptive_stats")
  for (path in c(bundle$request_path, bundle$result_path, bundle$output_path)) {
    check(!grepl(confidential_prompt, text_file(path), fixed = TRUE), paste("Prompt privacy optout leaked into", basename(path)))
  }
  check(is.null(bundle$request$cli$`user-prompt`), "Replayable CLI retained a user prompt despite privacy optout")
  check(is.null(bundle$request$user_prompt), "Request explicitly retained a private user prompt")
  logged <- new_run(context, "descriptive_stats", c("--csv", context$input, "--vars", "x,y", "--log", "TRUE",
                    "--user-prompt", confidential_prompt))
  for (path in c(logged$request_path, logged$result_path, logged$output_path,
                 file.path(context$dataset, "analysis_log.jsonl"), file.path(context$dataset, "report_canonical.md"))) {
    check(file.exists(path), paste("Missing logged-privacy output:", basename(path)))
    check(!grepl(confidential_prompt, text_file(path), fixed = TRUE), paste("Prompt privacy optout leaked into", basename(path)))
  }
})

test("quoted_canonical_logging_booleans_keep_enabled_and_privacy_semantics", {
  context <- new_case("quoted-canonical-booleans")
  installation <- file.path(context$base, "installation")
  dir.create(installation)
  check(all(file.copy(file.path(repo, c("scripts", "assets")), installation, recursive = TRUE)),
        "Could not prepare isolated canonical-configuration installation")
  canonical <- baseline_config
  canonical$logging$enabled <- "true"
  canonical$logging$include_user_prompt <- "true"
  canonical_path <- file.path(installation, "scripts/config.yml")
  yaml::write_yaml(canonical, canonical_path)
  with_environment(list(NLSS_CONFIG_PATH = ""), {
    allowed <- "PHASE2_ALLOWED_QUOTED_TRUE_PROMPT"
    first <- new_run(context, "descriptive_stats", c("--csv", context$input, "--vars", "x,y", "--user-prompt", allowed),
                     installation = installation)
    check(identical(first$request$user_prompt, allowed), "Canonical quoted true unexpectedly suppressed the allowed user prompt")
    log_path <- file.path(context$dataset, "analysis_log.jsonl")
    check(file.exists(log_path) && grepl(allowed, text_file(log_path), fixed = TRUE),
          "Canonical quoted true unexpectedly disabled legacy prompt logging")
    before <- sha256(log_path)
    canonical$logging$enabled <- "false"
    canonical$logging$include_user_prompt <- "false"
    yaml::write_yaml(canonical, canonical_path)
    private <- "PHASE2_PRIVATE_QUOTED_FALSE_PROMPT"
    second <- new_run(context, "descriptive_stats", c("--csv", context$input, "--vars", "x,y", "--user-prompt", private),
                      installation = installation)
    check(identical(sha256(log_path), before), "Canonical quoted false unexpectedly appended a legacy log")
    for (path in c(second$request_path, second$result_path, second$output_path, log_path)) {
      check(!grepl(private, text_file(path), fixed = TRUE), paste("Canonical quoted-false privacy leaked into", basename(path)))
    }
  })
})

mi_context <- NULL
mi_expected <- NULL
mi_artifact <- NULL
test("seeded_mice_artifact_prepares_independent_MI_reference", {
  values <- data.frame(x = golden$x1, z = golden$x2, y = golden$outcome_reg,
                       binary = golden$binary_outcome, count = golden$count_outcome)
  values <- head(values[stats::complete.cases(values), , drop = FALSE], 100L)
  values$x[c(2, 8, 16, 24, 41, 56)] <- NA_real_
  values$z[c(3, 9, 17, 25, 42, 57)] <- NA_real_
  values$y[c(4, 10, 18, 26, 43, 58)] <- NA_real_
  mi_context <- new_case("mi-end-to-end", values)
  # Read the public CSV bytes as mice's independent input, avoiding any NLSS
  # dataframe or import helper when constructing the reference imputations.
  reference_data <- read.csv(mi_context$input, stringsAsFactors = FALSE)
  mi_expected <- mice::mice(reference_data, m = 3L, maxit = 2L, seed = 6141L, printFlag = FALSE)
  run_module("impute", c("--csv", mi_context$input, "--vars", "x,z,y,binary,count", "--engine", "mice",
                        "--m", "3", "--maxit", "2", "--seed", "6141"))
  entries <- lapply(readLines(file.path(mi_context$dataset, "analysis_log.jsonl"), warn = FALSE), jsonlite::fromJSON)
  entry <- tail(Filter(function(value) identical(value$module, "impute"), entries), 1L)[[1]]
  mi_artifact <- file.path(mi_context$project, entry$results$imputation_artifact$path)
  check(file.exists(mi_artifact), "Seeded imputation did not preserve its mids artifact")
  stored <- readRDS(mi_artifact)
  for (variable in names(reference_data)) {
    check(identical(stored$imp[[variable]], mi_expected$imp[[variable]]), paste("Independent mice draws differ for", variable))
  }
})

mi_models <- list(
  gaussian = list(formula = "y ~ x + z", family = "gaussian", link = "identity"),
  binomial_logit = list(formula = "binary ~ x + z", family = "binomial", link = "logit"),
  binomial_probit = list(formula = "binary ~ x + z", family = "binomial", link = "probit"),
  poisson = list(formula = "count ~ x + z", family = "poisson", link = "log")
)
for (name in names(mi_models)) {
  test(paste0("MI_CLI_", name, "_matches_direct_mice_pooling"), {
    check(!is.null(mi_context) && !is.null(mi_expected) && !is.null(mi_artifact), "MI reference preparation failed")
    setwd(mi_context$project)
    specification <- mi_models[[name]]
    bundle <- new_run(mi_context, "mi_regression", c("--mids", mi_artifact, "--formula", specification$formula,
      "--family", specification$family, "--link", specification$link, "--conf-level", "0.90"))
    check_bundle(bundle, mi_context, "mi_regression")
    check(isTRUE(bundle$result$results$inference_pooled), "MI model does not explicitly report pooled inference")
    near(bundle$result$results$m, 3, "Number of independently analysed imputations")
    fits <- lapply(mice::complete(mi_expected, action = "all"), function(data) {
      if (specification$family == "gaussian") stats::lm(stats::as.formula(specification$formula), data = data) else {
        family <- if (specification$family == "binomial") stats::binomial(specification$link) else stats::poisson(specification$link)
        stats::glm(stats::as.formula(specification$formula), data = data, family = family)
      }
    })
    pooled <- mice::pool(mice::as.mira(fits), dfcom = stats::df.residual(fits[[1]]), rule = "rubin1987")
    expected <- as.data.frame(summary(pooled, type = "all", conf.int = TRUE, conf.level = 0.90))
    coefficients <- bundle$result$results$coefficients_df
    check(nrow(coefficients) == nrow(expected), "MI pooled coefficient count differs from direct mice")
    coefficients <- coefficients[match(expected$term, coefficients$term), , drop = FALSE]
    for (field in c("estimate", "std.error", "statistic", "df", "p.value", "conf.low", "conf.high",
                    "m", "riv", "lambda", "fmi", "ubar", "b", "t", "dfcom")) {
      near(coefficients[[field]], expected[[field]], paste(name, "pooled", field))
    }
    table <- markdown_table(bundle$markdown, "Term")
    table <- table[match(coefficients$term, table$Term), , drop = FALSE]
    for (mapping in list(c("Estimate", "estimate"), c("SE", "std.error"), c("Statistic", "statistic"),
                         c("df", "df"), c("p", "p.value"), c("CI low", "conf.low"), c("CI high", "conf.high"))) {
      near(table[[mapping[1]]], round(coefficients[[mapping[2]]], 5), paste(name, "Markdown", mapping[1]), tolerance = 1e-10)
    }
    rerun <- replay(mi_context, bundle)
    check(identical(bundle$result$results, rerun$result$results), "MI replay changed its pooled statistical results")
    check(identical(bundle$markdown, rerun$markdown), "MI replay changed deterministic Markdown")
  })
}

test("MI_CLI_rejects_unsupported_formula_without_completed_run", {
  check(!is.null(mi_context) && !is.null(mi_artifact), "MI reference preparation failed")
  setwd(mi_context$project)
  before <- successful_runs(mi_context)
  new_run(mi_context, "mi_regression", c("--mids", mi_artifact, "--formula", "y ~ log(x)"), expect_failure = TRUE)
  check(identical(successful_runs(mi_context), before), "Unsupported MI formula produced a completed run")
})

summary_path <- file.path(work, "results.json")
jsonlite::write_json(list(schema_version = 1L, suite = suite_name, selected_pattern = selected_pattern, tests = results,
  environment = list(r = R.version.string, packages = as.list(vapply(required, function(p) as.character(utils::packageVersion(p)), character(1))))),
  summary_path, pretty = TRUE, auto_unbox = TRUE, digits = NA)
passed <- sum(vapply(results, function(result) result$passed, logical(1)))
cat(sprintf("%s: %d/%d cases passed. Results: %s\n", if (mi_only) "Phase 2 MI" else "Phase 2", passed, length(results), summary_path))
# Explicit roots belong to callers and are never pruned. Retention is confined
# to timestamped, default-root test directories, as in the Phase 1 runner.
if (!nzchar(forced_root) && keep > 0L && dir.exists(output_base)) {
  candidates <- list.dirs(output_base, full.names = TRUE, recursive = FALSE)
  candidates <- sort(candidates[grepl("^[0-9]{14}$", basename(candidates))], decreasing = TRUE)
  obsolete <- if (length(candidates) > keep) candidates[seq.int(keep + 1L, length(candidates))] else character(0)
  for (path in setdiff(obsolete, run_root)) {
    if (identical(dirname(path), output_base) && grepl("^[0-9]{14}$", basename(path))) unlink(path, recursive = TRUE)
  }
}
quit(status = if (length(results) > 0L && passed == length(results)) 0 else 1)
