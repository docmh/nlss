#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Public legacy-assumptions CLI checks; independent installed-package references.
args <- commandArgs(TRUE)
script <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])
repo <- normalizePath(file.path(dirname(script[1]), "../.."), winslash = "/")
if ("--help" %in% args) {
  cat("Usage: Rscript tests/phase2/run_mixed_diagnostic_tests.R [--root PATH] [--keep N] [--match REGEX]\n",
      "Honors NLSS_TEST_ROOT, NLSS_KEEP_RUNS and NLSS_TESTS_CONFIG. This tests legacy assumptions, not run replay.\n", sep = "")
  quit(status = 0)
}
if (length(args) %% 2L || (length(args) && any(!args[seq(1L, length(args), 2L)] %in% c("--root", "--keep", "--match")))) stop("Unknown/incomplete runner option")
arg <- function(name, fallback) {
  i <- which(args == name)
  if (length(i) > 1L) stop("Repeated runner option: ", name)
  if (length(i)) args[i + 1L] else fallback
}
pattern <- arg("--match", ".*")
invisible(grepl(pattern, "validate regular expression"))
required <- c("yaml", "jsonlite", "arrow", "digest", "lme4", "performance", "influence.ME",
  if (grepl(pattern, "mixed_diagnostic_dharma_seeded_settings")) "DHARMa")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing test packages: ", paste(missing, collapse = ", "))
# influence.ME declares lme4 in Depends and calls its attached generics.
# Only this independent reference process is affected; every CLI is a fresh R process.
suppressPackageStartupMessages(library(lme4))
absolute <- function(path) normalizePath(if (grepl("^(/|[A-Za-z]:|\\\\)", path)) path else file.path(repo, path), winslash = "/", mustWork = FALSE)
cfg <- yaml::read_yaml(absolute(Sys.getenv("NLSS_TESTS_CONFIG", "tests/tests.yml")), eval.expr = FALSE)$tests
forced_root <- arg("--root", Sys.getenv("NLSS_TEST_ROOT", ""))
keep <- suppressWarnings(as.integer(arg("--keep", Sys.getenv("NLSS_KEEP_RUNS", as.character(cfg$keep_runs_default)))))
if (length(keep) != 1L || is.na(keep) || keep < 0L) stop("Invalid --keep")
output_base <- absolute(cfg$output_dir)
collection <- file.path(if (nzchar(forced_root)) absolute(forced_root) else output_base, "phase2-mixed-diagnostic")
work <- file.path(collection, paste0("run-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid()))
dir.create(work, recursive = TRUE)
baseline <- yaml::read_yaml(file.path(repo, "scripts/config.yml"), eval.expr = FALSE)
baseline$defaults$digits <- 8L
baseline$logging$enabled <- TRUE
private_config <- file.path(work, "config.yml")
yaml::write_yaml(baseline, private_config)
Sys.setenv(NLSS_CONFIG_PATH = private_config)
text_file <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
check <- function(ok, message) if (!isTRUE(ok)) stop(message, call. = FALSE)
numeric_checks <- 0L
near <- function(actual, expected, label, tolerance = 2e-7, probability = FALSE) {
  actual <- as.numeric(actual); expected <- as.numeric(expected)
  check(length(actual) == length(expected) && length(expected) > 0L, paste(label, "length differs"))
  check(identical(is.na(actual), is.na(expected)), paste(label, "missing values differ"))
  select <- !is.na(expected)
  scale <- if (probability) pmax(1e-300, abs(expected[select])) else pmax(1, abs(expected[select]))
  check(all(is.finite(actual[select])) && all(abs(actual[select] - expected[select]) <= tolerance * scale), paste(label, "differs from independent reference"))
  numeric_checks <<- numeric_checks + length(expected)
}
reference <- function(code) tryCatch(force(code), error = function(e) stop("Reference/fixture failure: ", conditionMessage(e), call. = FALSE))
results <- list()
test <- function(name, code) {
  if (!grepl(pattern, name)) return(invisible(NULL))
  previous <- getwd(); on.exit(setwd(previous), add = TRUE)
  start <- proc.time()[["elapsed"]]; before <- numeric_checks
  error <- tryCatch({ force(code); NULL }, error = function(e) conditionMessage(e))
  results[[length(results) + 1L]] <<- list(test = name, passed = is.null(error),
    numeric_checks = numeric_checks - before, seconds = unname(proc.time()[["elapsed"]] - start), message = if (is.null(error)) "OK" else error)
  cat(if (is.null(error)) "[PASS] " else "[FAIL] ", name, if (is.null(error)) "" else paste0(": ", error), "\n", sep = "")
}
new_case <- function(name, data) {
  base <- file.path(work, "cases", name); project <- file.path(base, "project")
  dir.create(project, recursive = TRUE)
  yaml::write_yaml(list(version = 1L, datasets = list()), file.path(project, "nlss-workspace.yml"))
  input <- file.path(base, "sample.rds"); saveRDS(data, input)
  list(base = base, project = project, input = input, data = data)
}
run_case <- function(context, options = list()) {
  # Explicit options prevent unrelated future default changes from broadening the probes.
  resolved <- modifyList(list(analysis = "mixed_models", formula = "Reaction ~ Days + (Days | Subject)",
    reml = TRUE, optimizer = "bobyqa", maxfun = 100000L, influence = FALSE, performance = FALSE,
    dharma = FALSE, normality = "shapiro", `random-effects` = FALSE, homoscedasticity = FALSE,
    outliers = FALSE, singular = TRUE, convergence = TRUE, log = TRUE), options)
  arguments <- c("--rds", context$input, unlist(Map(function(key, value) c(paste0("--", key), as.character(value)), names(resolved), resolved), use.names = FALSE))
  writeLines(paste(shQuote(c(file.path(R.home("bin"), "Rscript"), file.path(repo, "scripts/R/assumptions.R"), arguments)), collapse = " "), file.path(context$base, "command.txt"))
  old <- getwd(); on.exit(setwd(old), add = TRUE); setwd(context$project)
  command_log <- file.path(context$base, "cli.log")
  status <- system2(file.path(R.home("bin"), "Rscript"), c(shQuote(file.path(repo, "scripts/R/assumptions.R")), shQuote(arguments)), stdout = command_log, stderr = command_log)
  check(identical(status, 0L), paste("assumptions unexpected exit", status, text_file(command_log)))
  log <- file.path(context$project, "sample/analysis_log.jsonl")
  check(file.exists(log), "Legacy analysis_log.jsonl missing")
  entries <- lapply(readLines(log, warn = FALSE), jsonlite::fromJSON, simplifyVector = TRUE)
  entries <- Filter(function(entry) identical(entry$module, "assumptions"), entries)
  check(length(entries) == 1L, "Expected exactly one assumptions legacy entry")
  entry <- entries[[1L]]
  rows <- entry$results$checks_df; diagnostics <- entry$results$diagnostics
  check(is.data.frame(rows) && nrow(rows) > 0L && "status" %in% names(rows), "Mixed diagnostic rows/status missing")
  check(all(rows$status %in% c("available", "skipped", "unavailable")), "Unexpected mixed diagnostic status")
  check(is.list(diagnostics) && is.list(diagnostics$optimizer) && is.list(diagnostics$requested), "Resolved mixed diagnostic metadata missing")
  report <- file.path(context$project, "sample/report_canonical.md")
  check(file.exists(report), "Legacy Markdown missing")
  list(rows = rows, diagnostics = diagnostics, entry = entry, markdown = text_file(report), command = text_file(command_log))
}
row_for <- function(result, test, target = NULL) {
  rows <- result$rows[result$rows$test == test, , drop = FALSE]
  if (!is.null(target)) rows <- rows[rows$target == target, , drop = FALSE]
  check(nrow(rows) == 1L, paste("Expected one diagnostic row:", test, target))
  rows
}
unavailable <- function(row, status, label) {
  check(identical(row$status, status), paste(label, "wrong status"))
  check(!row$decision %in% c("ok", "passed", "pass"), paste(label, "falsely passes"))
  check(nzchar(row$note), paste(label, "reason missing"))
  check(is.na(row$p) && is.na(row$statistic), paste(label, "has a fabricated statistic/p"))
}
optimizer_reference <- function(result, fit, optimizer, key, limit) {
  meta <- result$diagnostics$optimizer
  check(identical(meta$requested, optimizer) && identical(meta$control_key, key), "Optimizer name/control mapping differs")
  near(meta$control_value, limit, "Effective optimizer limit", tolerance = 0)
  near(meta$code, fit@optinfo$conv$opt, "Optimizer return code", tolerance = 0)
  if (length(fit@optinfo$feval) && all(is.finite(fit@optinfo$feval))) near(meta$evaluations, fit@optinfo$feval, "Optimizer evaluation count", tolerance = 0)
  else check(is.null(meta$evaluations) || all(is.na(meta$evaluations)), "Unavailable optimizer evaluation count was fabricated")
  check(identical(meta$message, fit@optinfo$message), "Optimizer message differs")
  check(isTRUE(result$diagnostics$reml_requested) && isTRUE(result$diagnostics$reml_effective), "Requested/effective REML missing")
}
sleepstudy_data <- lme4::sleepstudy
normal_fit <- function() {
  reference(lme4::lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy_data,
    control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000L))))
}

test("mixed_diagnostic_optimizer_code_smoke", {
  fit <- reference(suppressWarnings(lme4::lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy_data,
    control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 80L)))))
  check(fit@optinfo$conv$opt != 0 && !length(fit@optinfo$conv$lme4$messages), "Reference/fixture failure: bobyqa80 no longer isolates optimizer-only failure")
  context <- new_case("optimizer-code", sleepstudy_data)
  result <- run_case(context, list(maxfun = 80L))
  optimizer_reference(result, fit, "bobyqa", "maxfun", 80L)
  row <- row_for(result, "lme4")
  check(identical(row$status, "available") && identical(row$decision, "flag"), "Nonzero optimizer code presented as success")
  check(grepl("maximum|evaluation|code", row$note, ignore.case = TRUE), "Optimizer failure reason absent")
  check(grepl("maximum number of function evaluations|convergence code 1|optimizer.*code.*1", result$markdown, ignore.case = TRUE), "Markdown hides optimizer failure")
})

test("mixed_diagnostic_package_diagnostics_smoke", {
  fit <- normal_fit()
  perf <- reference(performance::check_heteroscedasticity(fit))
  check(inherits(perf, "check_heteroscedasticity") && length(perf) == 1L && is.finite(as.numeric(perf)), "Reference/fixture failure: performance return type changed")
  # Unlike the CLI's function-local fit, this reference's data symbol exists
  # in the public evaluation environment expected by influence.ME.
  infl <- reference(influence.ME::influence(fit, group = "Subject"))
  cooks <- as.numeric(reference(stats::cooks.distance(infl)))
  check(length(cooks) == nlevels(sleepstudy_data$Subject) && all(is.finite(cooks)), "Reference/fixture failure: invalid Cook distances")
  context <- new_case("package-diagnostics", sleepstudy_data)
  result <- run_case(context, list(performance = TRUE, influence = TRUE, homoscedasticity = TRUE, `cook-multiplier` = 1))
  perf_row <- row_for(result, "performance::check_heteroscedasticity")
  near(perf_row$p, as.numeric(perf), "Typed performance p-value", probability = TRUE)
  check(identical(perf_row$status, "available") && identical(perf_row$decision, if (as.numeric(perf) < .05) "violated" else "ok"), "Performance decision/status wrong")
  cook_row <- row_for(result, "Cook's distance (cluster)")
  near(cook_row$value, max(cooks), "Maximum cluster Cook distance")
  near(cook_row$n, length(cooks), "Influence cluster count", tolerance = 0)
  flagged <- sum(cooks > 1 / length(cooks))
  check(identical(cook_row$status, "available") && identical(cook_row$decision, if (flagged > 0L) "flag" else "ok"), "Cook flag/status differs")
  check(grepl(paste0(":\\s*", flagged, "(?:\\D|$)"), cook_row$note, perl = TRUE), "Flagged cluster count absent or incorrect")
  check(grepl("performance::check_heteroscedasticity.*Residuals", result$markdown) && grepl("Cook's distance \\(cluster\\)", result$markdown), "Markdown merely claims requested checks without result rows")
})

test("mixed_diagnostic_singular_boundary_smoke", {
  data <- data.frame(subject = factor(rep(1:12, each = 8)), x = rep(1:8, 12), y = rep(c(2, 3, 4, 6, 5, 8, 9, 7), 12))
  fit <- reference(suppressMessages(lme4::lmer(y ~ x + (1 | subject), data = data,
    control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000L)))))
  check(fit@optinfo$conv$opt == 0 && isTRUE(lme4::isSingular(fit)), "Reference/fixture failure: expected singular but converged fit")
  random_values <- lme4::ranef(fit)[[1]][[1]]
  reference_error <- tryCatch({ stats::shapiro.test(random_values); NULL }, error = conditionMessage)
  check(is.character(reference_error), "Reference/fixture failure: expected zero-BLUP Shapiro failure")
  context <- new_case("singular-boundary", data)
  result <- run_case(context, list(formula = "y ~ x + (1 | subject)", `random-effects` = TRUE))
  optimizer_reference(result, fit, "bobyqa", "maxfun", 100000L)
  convergence <- row_for(result, "lme4"); singular <- row_for(result, "lme4::isSingular")
  check(identical(convergence$decision, "ok") && identical(singular$decision, "flag"), "Singularity conflated with optimizer convergence")
  unavailable(row_for(result, "Shapiro-Wilk", "(Intercept)"), "unavailable", "Zero-BLUP Shapiro error")
  check(grepl("unavailable|failed|identical", result$markdown, ignore.case = TRUE), "Markdown hides requested diagnostic failure")
})

test("mixed_diagnostic_nloptwrap_limit", {
  fit <- reference(suppressWarnings(lme4::lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy_data,
    control = lme4::lmerControl(optimizer = "nloptwrap", optCtrl = list(maxeval = 1L)))))
  context <- new_case("nloptwrap-limit", sleepstudy_data)
  result <- run_case(context, list(optimizer = "nloptwrap", maxfun = 1L))
  optimizer_reference(result, fit, "nloptwrap", "maxeval", 1L)
  check(grepl("maxeval", result$markdown, fixed = TRUE), "Markdown hides optimizer-specific control key")
  row <- row_for(result, "lme4")
  if (fit@optinfo$conv$opt != 0) check(identical(row$decision, "flag"), "Nonzero nloptwrap code presented as success")
})

test("mixed_diagnostic_skipped_normality", {
  context <- new_case("skipped-normality", sleepstudy_data)
  result <- run_case(context, list(`max-shapiro-n` = 10L))
  row <- row_for(result, "Shapiro-Wilk", "Residuals")
  unavailable(row, "skipped", "Configured Shapiro sample limit")
  near(row$n, nrow(sleepstudy_data), "Skipped diagnostic sample size", tolerance = 0)
  check(grepl("10", row$note, fixed = TRUE), "Skip reason hides configured sample limit")
  check(grepl("skipped", result$markdown, ignore.case = TRUE), "Markdown hides skipped requested diagnostic")
})

test("mixed_diagnostic_nlminbwrap_evaluation_limit", {
  fit <- reference(suppressWarnings(lme4::lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy_data,
    control = lme4::lmerControl(optimizer = "nlminbwrap", optCtrl = list(maxfun = 1L)))))
  check(grepl("function evaluation limit", fit@optinfo$message, fixed = TRUE), "Reference/fixture failure: nlminbwrap evaluation-budget semantics changed")
  context <- new_case("nlminbwrap-evaluation-limit", sleepstudy_data)
  result <- run_case(context, list(optimizer = "nlminbwrap", maxfun = 1L))
  optimizer_reference(result, fit, "nlminbwrap", "maxfun", 1L)
  check(identical(row_for(result, "lme4")$decision, "flag"), "Evaluation-limited nlminbwrap fit falsely passes")
})

test("mixed_diagnostic_disabled_diagnostics", {
  context <- new_case("disabled-diagnostics", sleepstudy_data)
  result <- run_case(context, list(normality = "none", singular = FALSE))
  check(nrow(result$rows) == 1L && identical(result$rows$assumption, "Convergence"), "Disabled diagnostic produced a result/success row")
  requested <- result$diagnostics$requested
  check(identical(requested$normality, "none") && isTRUE(requested$convergence), "Normality/convergence requests missing")
  for (key in c("random_effects", "homoscedasticity", "performance", "influence", "dharma", "outliers", "singular"))
    check(identical(requested[[key]], FALSE), paste("Disabled request not retained:", key))
  check(!grepl("Additional heteroscedasticity check via|Cook's D threshold|Residual normality assessed", result$markdown), "Disabled check falsely described as performed")
})

test("mixed_diagnostic_dharma_seeded_settings", {
  fit <- normal_fit()
  # The migrated run initializes RNG before any simulation. DHARMa otherwise
  # consumes one extra runif(1) when .Random.seed does not yet exist, despite
  # its explicit seed argument. Match the documented initialized RNG protocol.
  set.seed(123L)
  sim <- reference(DHARMa::simulateResiduals(fit, plot = FALSE, seed = 123, n = 250, refit = FALSE))
  uniformity <- reference(DHARMa::testUniformity(sim, plot = FALSE))
  dispersion <- reference(DHARMa::testDispersion(sim, plot = FALSE))
  context <- new_case("dharma-seeded-settings", sleepstudy_data)
  result <- run_case(context, list(dharma = TRUE))
  meta <- result$diagnostics$dharma
  check(isTRUE(meta$requested) && identical(meta$refit, FALSE), "DHARMa requested/refit metadata missing")
  near(meta$seed, 123, "DHARMa seed", tolerance = 0); near(meta$n, 250, "DHARMa simulation count", tolerance = 0)
  for (pair in list(list("DHARMa uniformity", uniformity), list("DHARMa dispersion", dispersion))) {
    row <- row_for(result, pair[[1]])
    check(identical(row$status, "available"), "Available DHARMa check omitted/failed")
    near(row$statistic, unname(pair[[2]]$statistic), paste(pair[[1]], "statistic"))
    near(row$p, pair[[2]]$p.value, paste(pair[[1]], "p"), probability = TRUE)
  }
  check(grepl("123", result$markdown, fixed = TRUE) && grepl("250", result$markdown, fixed = TRUE), "Markdown hides effective DHARMa seed/simulation settings")
})

test("mixed_diagnostic_model_case_group_identity", {
  data <- sleepstudy_data[c(seq(180, 2, -2), seq(1, 179, 2)), ]
  data$Reaction[c(4, 21)] <- NA_real_; data$Days[8] <- NA_real_; data$Subject[39] <- NA
  included <- which(stats::complete.cases(data)); excluded <- setdiff(seq_len(nrow(data)), included)
  context <- new_case("model-case-group-identity", data)
  result <- run_case(context)
  near(result$diagnostics$included_rows, included, "Included original source rows", tolerance = 0)
  near(result$diagnostics$excluded_rows, excluded, "Excluded original source rows", tolerance = 0)
  near(row_for(result, "Shapiro-Wilk", "Residuals")$n, length(included), "Actual model frame n", tolerance = 0)
  check(is.list(result$diagnostics$grouping), "Grouping identity metadata missing")
  records <- function(value) if (is.data.frame(value)) lapply(seq_len(nrow(value)), function(i)
    lapply(value, function(column) if (is.list(column)) column[[i]] else column[i])) else value
  groups <- records(result$diagnostics$grouping)
  selected <- Filter(function(group) identical(group$name, "Subject"), groups)
  check(length(selected) == 1L, "Expected exactly one Subject grouping record")
  group_levels <- records(selected[[1L]]$levels)
  expected <- split(included, droplevels(data$Subject[included]))
  check(length(group_levels) == length(expected) && setequal(vapply(group_levels, function(level) level$level, character(1)), names(expected)), "Grouping level identities differ")
  for (level in group_levels) near(level$source_rows, expected[[level$level]], paste("Grouping source rows", level$level), tolerance = 0)
})

summary_path <- file.path(work, "results.json")
jsonlite::write_json(list(schema_version = 1L, suite = "phase2-mixed-diagnostic", execution_contract = "legacy-assumptions", test_pattern = pattern,
  source_sha256 = list(assumptions = digest::digest(file = file.path(repo, "scripts/R/assumptions.R"), algo = "sha256"),
    runner = digest::digest(file = script[1L], algo = "sha256")),
  numeric_checks = numeric_checks, tests = results, environment = list(r = R.version.string,
  packages = as.list(vapply(required, function(p) as.character(utils::packageVersion(p)), character(1))))), summary_path, auto_unbox = TRUE, pretty = TRUE, digits = NA)
passed <- sum(vapply(results, function(result) result$passed, logical(1)))
cat(sprintf("Phase 2 mixed diagnostics: %d/%d cases passed; %d numeric comparisons. Results: %s\n", passed, length(results), numeric_checks, summary_path))
if (!nzchar(forced_root) && keep > 0L && dir.exists(collection)) {
  candidates <- list.dirs(collection, full.names = TRUE, recursive = FALSE)
  candidates <- sort(candidates[grepl("^run-[0-9]{14}-[0-9]+$", basename(candidates))], decreasing = TRUE)
  if (length(candidates) > keep) for (path in setdiff(candidates[seq.int(keep + 1L, length(candidates))], work)) {
    if (identical(dirname(path), collection) && file.exists(file.path(path, "results.json"))) unlink(path, recursive = TRUE)
  }
}
quit(status = if (length(results) > 0L && passed == length(results)) 0 else 1)
