#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Independent public-CLI acceptance: no NLSS scientific helper or historical golden.
args <- commandArgs(TRUE)
script <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])
repo <- normalizePath(file.path(dirname(script[1]), "../.."), winslash = "/")
if ("--help" %in% args) {
  cat("Usage: Rscript tests/phase2/run_sem_tests.R [--root PATH] [--keep N] [--match REGEX]\n",
      "Honors NLSS_TEST_ROOT, NLSS_KEEP_RUNS and NLSS_TESTS_CONFIG; private offline fixtures/configuration.\n", sep = "")
  quit(status = 0)
}
if (length(args) %% 2L || (length(args) && any(!args[seq(1L, length(args), 2L)] %in% c("--root", "--keep", "--match")))) stop("Unknown/incomplete runner option")
arg <- function(name, fallback) {
  i <- which(args == name)
  if (length(i) > 1L) stop("Repeated runner option: ", name)
  if (length(i)) args[i + 1L] else fallback
}
required <- c("yaml", "jsonlite", "arrow", "digest", "haven", "lavaan")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing test packages: ", paste(missing, collapse = ", "))
absolute <- function(path) normalizePath(if (grepl("^(/|[A-Za-z]:|\\\\)", path)) path else file.path(repo, path), winslash = "/", mustWork = FALSE)
cfg <- yaml::read_yaml(absolute(Sys.getenv("NLSS_TESTS_CONFIG", "tests/tests.yml")), eval.expr = FALSE)$tests
test_pattern <- arg("--match", ".*")
invisible(grepl(test_pattern, "validate regular expression"))
forced_root <- arg("--root", Sys.getenv("NLSS_TEST_ROOT", ""))
output_base <- absolute(cfg$output_dir)
run_root <- if (nzchar(forced_root)) absolute(forced_root) else file.path(output_base, format(Sys.time(), "%Y%m%d%H%M%S"))
keep <- suppressWarnings(as.integer(arg("--keep", Sys.getenv("NLSS_KEEP_RUNS", as.character(cfg$keep_runs_default)))))
if (length(keep) != 1L || is.na(keep) || keep < 0L) stop("Invalid --keep")
work <- file.path(run_root, "phase2-sem", paste0("run-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid()))
dir.create(work, recursive = TRUE)
baseline <- yaml::read_yaml(file.path(repo, "scripts/config.yml"), eval.expr = FALSE)
baseline$defaults$digits <- 5L
options(mc.cores = 1L)
Sys.setenv(OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1")
private_config <- file.path(work, "config.yml")
reset_config <- function() yaml::write_yaml(baseline, private_config)
reset_config()
Sys.setenv(NLSS_CONFIG_PATH = private_config)
text_file <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
read_json <- function(path) jsonlite::fromJSON(path, simplifyVector = TRUE)
sha <- function(path) digest::digest(file = path, algo = "sha256")
check <- function(ok, message) if (!isTRUE(ok)) stop(message, call. = FALSE)
numeric_checks <- 0L
near <- function(actual, expected, label, tolerance = 3e-6) {
  if (is.null(actual) && length(expected) == 1L && !is.finite(expected)) actual <- NA_real_
  actual <- as.numeric(actual); expected <- as.numeric(expected)
  check(length(actual) == length(expected) && length(expected) > 0L, paste(label, "length differs"))
  expected[is.infinite(expected)] <- NA_real_
  check(identical(is.na(actual), is.na(expected)), paste(label, "missing values differ"))
  valid <- !is.na(expected)
  check(all(is.finite(actual[valid])) && all(abs(actual[valid] - expected[valid]) <= tolerance * pmax(1, abs(expected[valid]))), paste(label, "differs from independent reference"))
  numeric_checks <<- numeric_checks + length(expected)
}
near_probability <- function(actual, expected, label) {
  if (is.null(actual) && length(expected) == 1L && is.na(expected)) actual <- NA_real_
  actual <- as.numeric(actual); expected <- as.numeric(expected)
  check(length(actual) == length(expected) && length(expected) > 0L, paste(label, "length differs"))
  check(identical(is.na(actual), is.na(expected)), paste(label, "missing values differ"))
  valid <- !is.na(expected)
  check(all(is.finite(actual[valid])) && all(abs(actual[valid] - expected[valid]) <= pmax(1e-300, abs(expected[valid]) * 3e-6)), paste(label, "differs from independent probability reference"))
  numeric_checks <<- numeric_checks + length(expected)
}
check_row <- function(row, reference, label) for (field in names(reference)) {
  if (grepl("(^p$|^p_|_p$|Pr\\()", field)) near_probability(row[[field]], reference[[field]], paste(label, field))
  else near(row[[field]], reference[[field]], paste(label, field))
}
results <- list()
test <- function(name, code) {
  if (!grepl(test_pattern, name)) return(invisible(NULL))
  previous <- getwd(); on.exit(setwd(previous), add = TRUE)
  reset_config(); start <- proc.time()[["elapsed"]]; before <- numeric_checks
  error <- tryCatch({ force(code); NULL }, error = function(e) conditionMessage(e))
  results[[length(results) + 1L]] <<- list(module = "sem", test = name, passed = is.null(error),
    numeric_checks = numeric_checks - before, seconds = unname(proc.time()[["elapsed"]] - start), message = if (is.null(error)) "OK" else error)
  cat(if (is.null(error)) "[PASS] " else "[FAIL] ", name, if (is.null(error)) "" else paste0(": ", error), "\n", sep = "")
}
new_case <- function(name, data, format = "rds") {
  base <- file.path(work, "cases", name); project <- file.path(base, "project")
  dir.create(project, recursive = TRUE)
  yaml::write_yaml(list(version = 1L, datasets = list()), file.path(project, "nlss-workspace.yml"))
  input <- file.path(base, paste0("sample.", format))
  if (format == "rds") saveRDS(data, input) else if (format == "sav") haven::write_sav(data, input) else if (format == "parquet") arrow::write_parquet(data, input) else write.csv(data, input, row.names = FALSE)
  setwd(project)
  list(project = project, base = base, input = input, flag = paste0("--", format), dataset = file.path(project, "sample"), data = data)
}
run_module <- function(module, options, failure = FALSE) {
  log <- tempfile(module, tmpdir = dirname(getwd()), fileext = ".log")
  status <- system2(file.path(R.home("bin"), "Rscript"), c(shQuote(file.path(repo, "scripts/R", paste0(module, ".R"))), shQuote(options)), stdout = log, stderr = log)
  check(if (failure) status != 0L else status == 0L, paste(module, "unexpected exit", status, text_file(log)))
  invisible(log)
}
runs <- function(context) {
  paths <- list.files(context$project, "^request[.]json$", recursive = TRUE, full.names = TRUE)
  dirname(paths[grepl("/runs/[^.][^/]+/request[.]json$", paths)])
}
new_run <- function(context, options, failure = FALSE, module = "sem", source = TRUE) {
  before <- runs(context)
  command <- run_module(module, c(if (source) c(context$flag, context$input), options), failure)
  added <- setdiff(runs(context), before)
  check(length(added) == 1L, paste(module, "did not record exactly one terminal run"))
  request_path <- file.path(added, "request.json"); result_path <- file.path(added, "result.json")
  request <- read_json(request_path); result <- read_json(result_path)
  check(identical(result$status, if (failure) "failed" else "completed"), "Wrong terminal state")
  check(identical(result$artifacts$request$sha256, sha(request_path)), "Request/result hash association differs")
  output <- file.path(added, "output.md")
  if (failure) check(!file.exists(output) && !is.null(result$error), "Failed analysis has normal output or no error") else {
    check(file.exists(output), "Completed analysis has no Markdown")
    for (artifact in result$artifacts) check(identical(sha(file.path(added, artifact$path)), artifact$sha256), "Artifact hash mismatch")
    for (template in request$templates) check(identical(sha(file.path(added, template$path)), template$sha256), "Template hash mismatch")
    for (pair in list(c("snapshot_path", "data_sha256"), c("dictionary_path", "dictionary_sha256"))) check(
      identical(sha(file.path(context$project, request$dataset[[pair[1]]])), request$dataset[[pair[2]]]), "Immutable input/dictionary hash mismatch")
    check(length(request$options) > 0L && length(request$design) > 0L && length(request$environment$packages) > 0L, "Resolved request lacks scientific/environment context")
  }
  list(path = added, request_path = request_path, request = request, result = result, markdown = if (failure) NULL else text_file(output), command = command)
}
replay <- function(context, bundle) {
  out <- new_run(context, c("--request", bundle$request_path), module = "replay_run", source = FALSE)
  check(identical(out$result$results, bundle$result$results), "Replay changed raw statistical results")
  check(identical(out$markdown, bundle$markdown), "Replay changed Markdown bytes")
  out
}
failed <- function(context, options) {
  paths <- file.path(context$dataset, c("report_canonical.md", "analysis_log.jsonl"))
  existing <- paths[file.exists(paths)]; before <- vapply(existing, sha, character(1))
  out <- new_run(context, options, failure = TRUE)
  check(identical(before, vapply(existing, sha, character(1))), "Failed analysis changed published report/log")
  check(!dir.exists(file.path(context$dataset, ".analysis-lock")), "Failed analysis retained its lock")
  run_module("replay_run", c("--request", out$request_path), failure = TRUE)
  out
}
replace_options <- function(base, override) {
  keys <- base[seq(1L, length(base), 2L)]; wanted <- !keys %in% override[seq(1L, length(override), 2L)]
  c(as.vector(rbind(keys[wanted], base[seq(2L, length(base), 2L)][wanted])), override)
}

set.seed(73013)
n <- 300L
data <- data.frame(x = rnorm(n), z = rnorm(n))
data$m1 <- .6 * data$x + .2 * data$z + rnorm(n, sd = .8)
data$m2 <- .3 * data$x + .45 * data$m1 + .15 * data$z + rnorm(n, sd = .85)
data$y <- .2 * data$x + .5 * data$m1 - .25 * data$m2 + .35 * data$z + rnorm(n, sd = .9)
f1 <- rnorm(n); f2 <- .4 * f1 + sqrt(.84) * rnorm(n)
items <- paste0("i", 1:6)
for (i in seq_along(items)) data[[items[i]]] <- c(.85, .7, .8, .75, .8, .65)[i] * if (i <= 3) f1 else f2
for (name in items) data[[name]] <- data[[name]] + rnorm(n, sd = .65)
data$group <- factor(rep(c("B", "A"), length.out = n), levels = c("A", "B", "unused"))
data$i1[c(3, 19)] <- NA_real_; data$i4[c(7, 21)] <- NA_real_; data$y[14] <- NaN; data$x[11] <- NA_real_
data$unused <- Inf
cfa_model <- "F1 =~ i1 + i2 + i3\nF2 =~ i4 + i5 + i6"
fit_names <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr", "aic", "bic", "logl")
base_options <- c("--analysis", "cfa", "--model", cfa_model, "--estimator", "ML", "--missing", "listwise", "--se", "standard", "--ci", "standard", "--std", "std.all", "--fit", paste(fit_names, collapse = ","), "--seed", "1")
fit_reference <- function(sample = data, model = cfa_model, analysis = "cfa", estimator = "ML", missing = "listwise", se = "standard", ordered = character(), group = "", equal = character(), bootstrap = 0L, seed = 1L) {
  arguments <- list(model = model, data = sample, estimator = estimator, missing = missing, se = se)
  if (length(ordered)) {
    for (name in ordered) sample[[name]] <- as.ordered(sample[[name]])
    arguments$data <- sample; arguments$ordered <- ordered
  }
  if (nzchar(group)) arguments$group <- group
  if (length(equal)) arguments$group.equal <- equal
  if (bootstrap > 0L) arguments$bootstrap <- bootstrap
  set.seed(seed)
  suppressWarnings(do.call(if (analysis == "cfa") lavaan::cfa else lavaan::sem, arguments))
}
check_recursive_numeric <- function(actual, expected, label) {
  if (is.numeric(expected)) { near(actual, expected, label); return(invisible(NULL)) }
  if (is.matrix(expected)) { near(actual, expected, label); return(invisible(NULL)) }
  if (is.list(expected)) {
    check(is.list(actual) && length(actual) == length(expected), paste(label, "structure differs"))
    for (i in seq_along(expected)) check_recursive_numeric(actual[[i]], expected[[i]], paste(label, if (is.null(names(expected))) i else names(expected)[i]))
  }
}
check_fit <- function(bundle, fit, std = "std.all", confidence = .95, ci = "standard", check_r2 = TRUE, modindices = 0, residuals = FALSE, expected_model = NULL) {
  values <- bundle$result$results
  check(isTRUE(lavaan::lavInspect(fit, "converged")), "Independent model did not converge")
  expected <- suppressWarnings(lavaan::parameterEstimates(fit, standardized = std != "none", ci = TRUE, level = confidence,
    boot.ci.type = switch(ci, bootstrap = "perc", bca = "bca.simple", "norm")))
  full <- values$parameter_table
  check(is.data.frame(full) && nrow(full) == nrow(expected), "Full parameter table missing variance/intercept/threshold/defined rows")
  for (field in c("lhs", "op", "rhs")) check(identical(as.character(full[[field]]), as.character(expected[[field]])), paste("Full parameter identity differs", field))
  specification <- lavaan::parTable(fit)
  actual_specification <- values$parameter_specification
  check(is.data.frame(actual_specification) && nrow(actual_specification) == nrow(specification), "Free/fixed/constrained parameter specification missing")
  for (field in intersect(c("lhs", "op", "rhs", "label", "plabel"), names(specification))) check(identical(as.character(actual_specification[[field]]), as.character(specification[[field]])), paste("Parameter specification differs", field))
  for (field in intersect(c("id", "user", "block", "group", "free", "ustart", "exo", "est", "se"), names(specification))) near(actual_specification[[field]], specification[[field]], paste("Parameter specification", field))
  for (field in intersect(c("est", "se", "z", "pvalue", "ci.lower", "ci.upper", "std.lv", "std.all", "std.nox"), names(expected))) {
    if (field == "pvalue") near_probability(full[[field]], expected[[field]], paste("Full parameter", field)) else near(full[[field]], expected[[field]], paste("Full parameter", field))
  }
  selected <- expected[expected$op %in% c("=~", "~", "~~", ":=") & !(expected$op == "~~" & expected$lhs == expected$rhs), , drop = FALSE]
  actual <- values$params_df
  check(identical(actual$path, paste(selected$lhs, selected$op, selected$rhs)), "Legacy parameter paths differ")
  check_row(actual, list(est = selected$est, se = selected$se, z = selected$z, p = selected$pvalue,
    ci_low = selected$ci.lower, ci_high = selected$ci.upper, std = if (std == "none") rep(NA_real_, nrow(selected)) else selected[[std]]), "Parameter projection")
  expected_fit <- suppressWarnings(lavaan::fitMeasures(fit, names(values$fit)))
  check(length(expected_fit) > 0L, "Fit measures absent")
  for (field in names(expected_fit)) near(unlist(values$fit[[field]]), expected_fit[[field]], paste("Fit", field))
  near(values$n, lavaan::lavInspect(fit, "nobs"), "Actual fitted N by group", tolerance = 0)
  if (check_r2) {
    expected_r2 <- unlist(lavaan::lavInspect(fit, "r2"))
    near(values$r2_df$r2, expected_r2, "R-squared")
  } else check(length(values$r2_df) == 0L || isTRUE(nrow(values$r2_df) == 0L), "Unrequested R-squared emitted")
  if (modindices > 0) {
    expected_mi <- suppressWarnings(lavaan::modindices(fit, sort. = TRUE, minimum.value = modindices))
    actual_mi <- values$modindices_df
    check(is.data.frame(actual_mi) && nrow(actual_mi) == nrow(expected_mi), "Full modification index rows missing")
    for (field in c("lhs", "op", "rhs")) check(identical(actual_mi[[field]], expected_mi[[field]]), paste("Modification identity", field))
    for (field in intersect(c("mi", "epc", "sepc.lv", "sepc.all", "sepc.nox"), names(expected_mi))) near(actual_mi[[field]], expected_mi[[field]], paste("Modification index", field))
  }
  if (residuals) {
    expected_res <- suppressWarnings(lavaan::residuals(fit, type = "standardized"))
    check(!is.null(values$residuals_output), "Requested standardized residual matrices missing")
    expected_res <- jsonlite::fromJSON(jsonlite::toJSON(expected_res, auto_unbox = TRUE, digits = NA, na = "null", null = "null"), simplifyVector = TRUE)
    check_recursive_numeric(values$residuals_output, expected_res, "Standardized residuals")
  }
  if (!is.null(expected_model)) check(identical(gsub("[[:space:]]+", "", bundle$request$design$model_syntax), gsub("[[:space:]]+", "", expected_model)), "Frozen model syntax differs from independently specified model")
  effective <- lavaan::lavInspect(fit, "options")
  check(length(values$inference) > 0L && length(values$fit_status) > 0L, "Effective inference or mandatory fit status missing")
  for (field in intersect(c("estimator", "estimator.orig", "missing", "se", "test", "information", "meanstructure", "fixed.x", "parameterization", "group.equal"), names(effective))) {
    check(identical(as.character(unlist(values$inference$effective[[field]])), as.character(unlist(effective[[field]]))), paste("Effective lavaan option differs", field))
  }
  near(values$fit_status$n_parameters, lavaan::lavInspect(fit, "npar"), "Free parameter count", tolerance = 0)
  check(identical(values$fit_status$admissible, isTRUE(suppressWarnings(lavaan::lavInspect(fit, "post.check")))) && isTRUE(values$fit_status$converged), "Convergence/admissibility differs")
  source_rows <- lavaan::lavInspect(fit, "case.idx")
  if (!is.list(source_rows)) source_rows <- list(source_rows)
  raw <- jsonlite::fromJSON(bundle$request_path, simplifyVector = FALSE)$design$case_selection
  check(length(raw$groups) == length(source_rows), "Case-selection group inventory differs")
  for (i in seq_along(source_rows)) near(unlist(raw$groups[[i]]$source_rows), source_rows[[i]], "Fitted source rows by group", tolerance = 0)
  near(unlist(raw$included_source_rows), sort(unique(unlist(source_rows, use.names = FALSE))), "Included source rows", tolerance = 0)
  check(identical(as.integer(unlist(raw$excluded_source_rows)), setdiff(seq_len(raw$source_n), as.integer(unlist(source_rows, use.names = FALSE)))), "Excluded source rows differ")
  groups <- lavaan::lavInspect(fit, "group.label")
  if (length(groups)) {
    check(identical(unlist(raw$group_order, use.names = FALSE), groups), "Fitted group order differs")
    expected_groups <- ifelse(selected$group > 0L, groups[pmax(1L, selected$group)], "")
    check(identical(actual$group, expected_groups), "Parameter group labels attached to wrong estimates")
  }
  if (identical(effective$se, "bootstrap")) {
    draws <- lavaan::lavInspect(fit, "boot")
    errors <- union(as.integer(attr(draws, "error.idx")), which(!apply(is.finite(draws), 1L, all)))
    inadmissible <- unique(as.integer(attr(draws, "nonadmissible")))
    check_row(values$bootstrap, list(attempted = nrow(draws), successful = nrow(draws) - length(errors), failed = length(errors),
      inadmissible = length(inadmissible), admissible_successful = nrow(draws) - length(union(errors, inadmissible))), "Bootstrap draw accounting")
  }
  invisible(fit)
}
run_compare <- function(name, sample = data, options = character(), model = cfa_model, analysis = "cfa", estimator = "ML", missing = "listwise", se = "standard", std = "std.all", confidence = .95, ci = "standard", ordered = character(), group = "", equal = character(), bootstrap = 0L, seed = 1L, check_r2 = TRUE, modindices = 0, residuals = FALSE, format = "rds", builder = NULL) {
  context <- new_case(name, sample, format)
  command <- replace_options(base_options, c("--analysis", analysis, "--model", model, "--estimator", estimator, "--missing", missing, "--se", se, "--std", std, "--conf-level", as.character(confidence), "--ci", ci, "--seed", as.character(seed),
    "--r2", if (check_r2) "TRUE" else "FALSE", "--modindices", as.character(modindices), "--residuals", if (residuals) "TRUE" else "FALSE",
    if (length(ordered)) c("--ordered", paste(ordered, collapse = ",")), if (nzchar(group)) c("--group", group), if (length(equal)) c("--group-equal", paste(equal, collapse = ",")),
    if (bootstrap > 0L) c("--bootstrap", "TRUE", "--bootstrap-samples", as.character(bootstrap)), options))
  if (!is.null(builder)) {
    command <- replace_options(command, c("--model", ""))
    command <- command[-c(which(command == "--model"), which(command == "--model") + 1L)]
    command <- c(command, builder)
  }
  bundle <- new_run(context, command)
  fit <- fit_reference(sample, model, analysis, estimator, missing, se, ordered, group, equal, bootstrap, seed)
  check_fit(bundle, fit, std, confidence, ci, check_r2, modindices, residuals, expected_model = model)
  list(context = context, bundle = bundle, fit = fit)
}

test("sem_cfa_full_parameter_fit_r2_modindices_residuals_smoke", {
  run_compare("cfa-full", modindices = .01, residuals = TRUE)
})
test("sem_cfa_builder", {
  run_compare("cfa-builder", builder = c("--factors", "F1=i1,i2,i3;F2=i4,i5,i6"))
})
test("sem_path_builder_ols_reference_smoke", {
  model <- "y ~ x + m1 + z"
  result <- run_compare("path-builder", model = model, analysis = "path", builder = c("--dv", "y", "--ivs", "x,m1", "--covariates", "z"))
  fit <- lm(y ~ x + m1 + z, data = data)
  params <- result$bundle$result$results$params_df
  near(params$est[params$op == "~"], coef(fit)[-1], "Path slopes vs independent OLS")
  check(grepl("chi²(0)", result$bundle$markdown, fixed = TRUE), "Saturated-model zero degrees of freedom disappeared from Markdown")
})
test("sem_explicit_model_constraints_defined_effects_and_paths_alias", {
  model <- "m1 ~ a*x\ny ~ b*m1 + c*x\nindirect := a*b\ntotal := c + a*b"
  run_compare("defined", model = model, analysis = "sem", builder = c("--paths", model))
})
mediation_models <- list(
  simple = "m1 ~ a1*x\ny ~ c_prime*x + b1*m1\nindirect_m1 := a1*b1\ntotal_indirect := indirect_m1\ntotal := c_prime + total_indirect",
  parallel = "m1 ~ a1*x + z\nm2 ~ a2*x + z\ny ~ c_prime*x + b1*m1 + b2*m2 + z\nindirect_m1 := a1*b1\nindirect_m2 := a2*b2\ntotal_indirect := indirect_m1 + indirect_m2\ntotal := c_prime + total_indirect",
  serial = "m1 ~ a1*x + z\nm2 ~ a2*x + d21*m1 + z\ny ~ c_prime*x + b1*m1 + b2*m2 + z\nindirect_m1 := a1*b1\nindirect_m2 := a2*b2\nindirect_serial := a1*d21*b2\ntotal_indirect := indirect_m1 + indirect_m2 + indirect_serial\ntotal := c_prime + total_indirect")
for (mode in names(mediation_models)) test(paste0("sem_mediation_", mode), {
  run_compare(paste0("mediation-", mode), model = mediation_models[[mode]], analysis = "mediation",
    builder = c("--x", "x", "--m", if (mode == "simple") "m1" else "m1,m2", "--y", "y", if (mode != "simple") c("--covariates", "z"), if (mode == "serial") c("--serial", "TRUE")))
})
for (estimator in c("MLR", "MLM", "MLMV", "MLMVS")) test(paste0("sem_estimator_", estimator), {
  run_compare(paste0("estimator-", estimator), estimator = estimator, se = "robust")
})
for (std in c("none", "std.lv", "std.all")) test(paste0("sem_standardization_", std, "_confidence90"), {
  run_compare(paste0("std-", std), std = std, confidence = .9)
})
test("sem_fiml_distinct_missing_patterns", {
  run_compare("fiml", missing = "fiml", estimator = "MLR", se = "robust")
})
test("sem_multigroup_level_order_and_constraints", {
  run_compare("multigroup", group = "group", equal = c("loadings", "intercepts"))
})
test("sem_multigroup_missing_group_not_literal_NA", {
  sample <- data; sample$group <- factor(rep(c("NA", "A", NA), length.out = nrow(sample)), levels = c("A", "NA", "unused"))
  run_compare("missing-group", sample, group = "group")
})
test("sem_invariance_four_steps_independent_lavaan_smoke", {
  context <- new_case("invariance", data)
  bundle <- new_run(context, replace_options(base_options, c("--analysis", "invariance", "--group", "group", "--invariance", "configural,metric,scalar,strict")))
  expected <- list(); constraints <- list(character(), "loadings", c("loadings", "intercepts"), c("loadings", "intercepts", "residuals"))
  for (i in seq_along(constraints)) {
    fit <- fit_reference(analysis = "invariance", group = "group", equal = constraints[[i]])
    expected[[i]] <- lavaan::fitMeasures(fit, c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr"))
  }
  actual <- bundle$result$results$fit
  check(identical(actual$step, c("configural", "metric", "scalar", "strict")), "Invariance steps missing or reordered")
  for (field in names(expected[[1]])) near(actual[[if (field == "pvalue") "p" else field]], vapply(expected, `[[`, numeric(1), field), paste("Invariance", field))
  for (field in c("cfi", "rmsea")) near(actual[[paste0("delta_", field)]], c(NA, diff(vapply(expected, `[[`, numeric(1), field))), paste("Invariance delta", field))
  replay(context, bundle)
})
for (estimator in c("WLSMV", "ULSMV", "DWLS")) for (missing in c("listwise", "pairwise")) test(paste0("sem_ordinal_", estimator, "_", missing, if (estimator == "WLSMV" && missing == "listwise") "_smoke" else ""), {
  sample <- data
  for (name in items) sample[[name]] <- as.integer(cut(sample[[name]], c(-Inf, -.6, 0, .7, Inf)))
  run_compare(paste0("ordinal-", estimator, "-", missing), sample, estimator = estimator, missing = missing, se = if (estimator == "DWLS") "standard" else "robust", ordered = items,
    options = c("--fit", "chisq,df,pvalue,cfi,tli,rmsea,srmr"))
})
for (format in c("rds", "sav", "parquet")) test(paste0("sem_close_numeric_ordinal_codes_", format), {
  clean <- data
  sample <- data
  codes <- 1 + c(0, 1, 2) * 1e-15
  for (name in items) {
    category <- cut(data[[name]], c(-Inf, -.4, .5, Inf), labels = FALSE)
    clean[[name]] <- ordered(category, levels = 1:3)
    sample[[name]] <- codes[category]
  }
  context <- new_case(paste0("close-ordinal-", format), sample, format)
  bundle <- new_run(context, replace_options(base_options, c("--ordered", paste(items, collapse = ","),
    "--estimator", "WLSMV", "--se", "robust", "--fit", "chisq,df,pvalue,cfi,tli,rmsea,srmr")))
  check_fit(bundle, fit_reference(clean, estimator = "WLSMV", se = "robust", ordered = items))
  for (name in items) check(length(bundle$request$design$category_levels[[name]]) == 3L,
    "Distinct numerical ordinal categories were merged")
  replay(context, bundle)
})
test("sem_ordered_nonlexical_levels_preserved", {
  sample <- data
  for (name in items) sample[[name]] <- ordered(c("low", "middle", "high")[cut(sample[[name]], c(-Inf, -.4, .5, Inf), labels = FALSE)], levels = c("middle", "low", "high"))
  run_compare("ordered-levels", sample, estimator = "WLSMV", se = "robust", ordered = items, options = c("--fit", "chisq,df,pvalue,cfi,tli,rmsea,srmr"))
})
test("sem_intrinsic_ordered_factor_without_ordered_flag", {
  sample <- data
  for (name in items) sample[[name]] <- ordered(cut(sample[[name]], c(-Inf, -.4, .5, Inf), labels = c("low", "middle", "high")), levels = c("low", "middle", "high"))
  run_compare("intrinsic-ordered", sample, estimator = "WLSMV", se = "robust", options = c("--fit", "chisq,df,pvalue,cfi,tli,rmsea,srmr"))
})
test("sem_ordinal_multigroup_threshold_constraints", {
  sample <- data
  for (name in items) sample[[name]] <- as.integer(cut(sample[[name]], c(-Inf, -.6, 0, .7, Inf)))
  run_compare("ordinal-thresholds", sample, estimator = "WLSMV", se = "robust", ordered = items,
    group = "group", equal = c("loadings", "thresholds"), options = c("--fit", "chisq,df,pvalue,cfi,tli,rmsea,srmr"))
})
test("sem_nearby_numeric_group_IDs_remain_distinct", {
  sample <- data; sample$group <- rep(c(1, 1 + 1e-14), length.out = nrow(sample))
  context <- new_case("nearby-numeric-groups", sample)
  bundle <- new_run(context, c(base_options, "--group", "group"))
  codes <- sprintf("%.17g", sample$group)
  sample$group <- factor(codes, levels = unique(codes))
  fit <- fit_reference(sample, group = "group")
  check_fit(bundle, fit)
  check(length(bundle$request$design$case_selection$group_order) == 2L, "Close numeric grouping IDs collapsed")
})
test("sem_sav_labelled_continuous_and_user_missing_smoke", {
  sample <- data; sample$unused <- NULL; sample$i1[1] <- 99
  for (name in items) sample[[name]] <- haven::labelled_spss(sample[[name]], labels = c(Low = -1, High = 1, Missing = 99), na_values = 99, label = paste("Research item", name))
  context <- new_case("sav-continuous", sample, "sav")
  bundle <- new_run(context, base_options)
  clean <- data; clean$i1[1] <- NA_real_
  check_fit(bundle, fit_reference(clean))
  check(grepl("Research item", text_file(file.path(context$project, bundle$request$dataset$dictionary_path)), fixed = TRUE), "SAV labels missing from dictionary")
})
test("sem_sav_ordinal_values_are_codes_not_labels", {
  sample <- data; sample$unused <- NULL
  for (name in items) sample[[name]] <- haven::labelled_spss(as.integer(cut(sample[[name]], c(-Inf, -.6, 0, .7, Inf))), labels = c(Z = 1, B = 2, A = 3, Z = 4, Missing = 99), na_values = 99)
  sample$i1[1] <- 99
  context <- new_case("sav-ordinal", sample, "sav")
  bundle <- new_run(context, replace_options(base_options, c("--ordered", paste(items, collapse = ","), "--estimator", "WLSMV", "--se", "robust", "--fit", "chisq,df,pvalue,cfi,tli,rmsea,srmr")))
  clean <- sample; for (name in items) clean[[name]] <- as.numeric(haven::zap_missing(clean[[name]]))
  check_fit(bundle, fit_reference(clean, estimator = "WLSMV", se = "robust", ordered = items))
})
for (ci in c("standard", "bootstrap", "bca")) test(paste0("sem_bootstrap_seeded_", ci), {
  result <- run_compare(paste0("bootstrap-", ci), model = mediation_models$simple, analysis = "mediation", se = "bootstrap", bootstrap = 40L, ci = ci, seed = 729L)
  check(length(result$bundle$result$results$bootstrap) > 0L, "Bootstrap success/failure audit missing")
  replay(result$context, result$bundle)
})
test("sem_private_model_input_config_template_mutation_replay_smoke", {
  context <- new_case("private-replay", data)
  model_with_comment <- paste0("# Research model; semicolon in a comment is not model syntax\n", cfa_model)
  model_file <- file.path(context$base, "private-model.lav"); writeLines(model_with_comment, model_file)
  template <- file.path(context$base, "private-template.md"); writeLines(c(paste0("# ", cfg$template_marker), "{{table_body}}", "{{narrative}}", "{{note_body}}"), template)
  yaml::write_yaml(list(logging = list(include_user_prompt = FALSE, include_outputs = FALSE)), private_config)
  command <- replace_options(base_options, c("--model", "")); command <- command[-c(which(command == "--model"), which(command == "--model") + 1L)]
  bundle <- new_run(context, c(command, "--model-file", "../private-model.lav", "--template", template, "--log", "FALSE", "--user-prompt", "PRIVATE_SEM_INTENT"))
  check_fit(bundle, fit_reference(), expected_model = model_with_comment)
  check(grepl(cfg$template_marker, bundle$markdown, fixed = TRUE), "Custom template ignored")
  check(!grepl(context$base, text_file(bundle$request_path), fixed = TRUE), "External data/model/template path leaked")
  check(!grepl("../private-model.lav", text_file(bundle$request_path), fixed = TRUE), "Relative external model path leaked")
  check(!grepl("PRIVATE_SEM_INTENT", text_file(bundle$request_path), fixed = TRUE), "Prompt privacy ignored")
  writeLines("y ~ x", model_file); writeLines("CHANGED TEMPLATE", template)
  changed <- data; changed$i1[1] <- 999; saveRDS(changed, context$input)
  working <- file.path(context$dataset, "sample.parquet"); arrow::write_parquet(changed, working)
  yaml::write_yaml(list(defaults = list(digits = 1L), modules = list(sem = list(seed = 773L, std = "none", estimator = "MLR"))), private_config)
  replay(context, bundle)
  near(arrow::read_parquet(working)$i1[1], 999, "Replay preserved working copy", tolerance = 0)
})
test("sem_optional_output_disabled", {
  run_compare("optional-off", check_r2 = FALSE)
})
test("sem_converged_Heywood_solution_explicitly_inadmissible", {
  set.seed(1730L)
  residual <- scale(matrix(rnorm(600L), ncol = 3L), center = TRUE, scale = FALSE)
  standardized <- residual %*% solve(chol(cov(residual)))
  covariance <- matrix(c(1,.3,.75,.3,1,.75,.75,.75,1), nrow = 3L)
  sample <- as.data.frame(standardized %*% chol(covariance)); names(sample) <- c("i1", "i2", "i3")
  result <- run_compare("heywood", sample, model = "F =~ i1 + i2 + i3")
  check(!isTRUE(lavaan::lavInspect(result$fit, "post.check")), "Independent Heywood fixture was admissible")
  check(result$bundle$result$results$fit_status$status == "inadmissible", "Converged improper solution presented as valid")
  check(grepl("inadmissible|negative variance", result$bundle$markdown, ignore.case = TRUE), "Improper solution hidden from Markdown")
})
test("sem_config_defaults_cli_precedence", {
  context <- new_case("defaults", data)
  yaml::write_yaml(list(modules = list(sem = list(seed = 729L, estimator = "ML", missing = "listwise", se = "standard", std = "none"))), private_config)
  bundle <- new_run(context, c("--analysis", "cfa", "--model", cfa_model, "--std", "std.lv"))
  check(bundle$request$config$modules$sem$seed == 729L && is.null(bundle$request$options$seed) && bundle$request$options$std == "std.lv", "Config / CLI precedence differs")
  check_fit(bundle, fit_reference(), std = "std.lv")
})
test("sem_bootstrap_configured_seed_and_sample_count", {
  context <- new_case("bootstrap-defaults", data)
  yaml::write_yaml(list(modules = list(sem = list(seed = 729L, estimator = "ML", missing = "listwise", se = "bootstrap", bootstrap = TRUE, bootstrap_samples = 40L))), private_config)
  bundle <- new_run(context, c("--analysis", "mediation", "--model", mediation_models$simple))
  check(bundle$request$options$seed == 729L && bundle$request$options$bootstrap_samples == 40L, "Bootstrap configuration seed/count not resolved")
  check_fit(bundle, fit_reference(model = mediation_models$simple, analysis = "mediation", se = "bootstrap", bootstrap = 40L, seed = 729L))
  replay(context, bundle)
})
test("sem_failed_analysis_preserves_prior_publication", {
  context <- new_case("protected", data); new_run(context, base_options)
  failed(context, replace_options(base_options, c("--model", "F =~ does_not_exist + i1 + i2")))
})
test("sem_failed_external_model_path_is_masked", {
  context <- new_case("failed-private-model", data); new_run(context, base_options)
  for (missing_model in c(file.path(context$base, "private-missing-model.lav"), "../private-missing-model.lav")) {
    bundle <- failed(context, c(base_options, "--model-file", missing_model))
    for (path in c(bundle$request_path, file.path(bundle$path, "result.json"))) {
      text <- text_file(path)
      check(!grepl(context$base, text, fixed = TRUE) && !grepl("../private-missing-model.lav", text, fixed = TRUE), "Failed-run external model path leaked")
      check(grepl("<external>/private-missing-model.lav", text, fixed = TRUE), "Masked missing-model identity was lost")
    }
  }
})
invalid <- list(analysis = c("--analysis", "invented"), estimator = c("--estimator", "invented"), missing = c("--missing", "invented"), se = c("--se", "invented"), ci = c("--ci", "invented"),
  std = c("--std", "invented"), confidence_one = c("--conf-level", "1"), confidence_nan = c("--conf-level", "NaN"), seed = c("--seed", "-1"), seed_fractional = c("--seed", "1.5"),
  boot_zero = c("--bootstrap-samples", "0"), boot_fractional = c("--bootstrap-samples", "2.5"), boolean = c("--r2", "perhaps"), modindices = c("--modindices", "-1"),
  unknown_fit = c("--fit", "chisq,invented"), unknown_ordered = c("--ordered", "absent"), unknown_group = c("--group", "absent"),
  ordinal_fiml = c("--estimator", "WLSMV", "--ordered", paste(items, collapse = ","), "--missing", "fiml"),
  invalid_model = c("--model", "this is not lavaan syntax"), invariance_without_group = c("--analysis", "invariance"), invalid_invariance = c("--analysis", "invariance", "--group", "group", "--invariance", "configural,invented"))
invalid$percentile_without_bootstrap <- c("--ci", "bootstrap")
invalid$bca_without_bootstrap <- c("--ci", "bca")
invalid$group_also_observed <- c("--model", "i1 ~ group", "--group", "group")
invalid$group_equal_without_group <- c("--group-equal", "loadings")
for (name in names(invalid)) test(paste0("sem_invalid_option_", name), {
  context <- new_case(paste0("invalid-", name), data)
  run_module("sem", c(context$flag, context$input, replace_options(base_options, invalid[[name]])), failure = TRUE)
  check(!any(vapply(runs(context), function(path) identical(read_json(file.path(path, "result.json"))$status, "completed"), logical(1))), "Invalid request published completed analysis")
})
for (name in c("infinity", "negative_infinity", "all_missing", "constant", "non_numeric")) test(paste0("sem_invalid_data_", name), {
  sample <- data
  if (name == "infinity") sample$i1[1] <- Inf
  if (name == "negative_infinity") sample$i1[1] <- -Inf
  if (name == "all_missing") sample$i1 <- NA_real_
  if (name == "constant") sample$i1 <- 1
  if (name == "non_numeric") sample$i1 <- rep("free text", nrow(sample))
  context <- new_case(paste0("invalid-data-", name), sample)
  run_module("sem", c(context$flag, context$input, base_options), failure = TRUE)
})

summary_path <- file.path(work, "results.json")
jsonlite::write_json(list(schema_version = 1L, suite = "phase2-sem", modules = "sem", test_pattern = test_pattern,
  numeric_checks = numeric_checks, tests = results, source_sha256 = sha(file.path(repo, "scripts/R/sem.R")),
  test_sha256 = sha(normalizePath(script[1], winslash = "/")),
  r_source_sha256 = as.list(vapply(sort(list.files(file.path(repo, "scripts/R"), "[.]R$", recursive = TRUE, full.names = TRUE)), sha, character(1))),
  environment = list(r = R.version.string, packages = as.list(vapply(required, function(p) as.character(utils::packageVersion(p)), character(1))))),
  summary_path, auto_unbox = TRUE, pretty = TRUE, digits = NA)
passed <- sum(vapply(results, function(result) result$passed, logical(1)))
cat(sprintf("Phase 2 SEM: %d/%d cases passed; %d numeric comparisons. Results: %s\n", passed, length(results), numeric_checks, summary_path))
if (!nzchar(forced_root) && keep > 0L && dir.exists(output_base)) {
  candidates <- sort(list.dirs(output_base, full.names = TRUE, recursive = FALSE), decreasing = TRUE)
  candidates <- candidates[grepl("^[0-9]{14}$", basename(candidates))]
  if (length(candidates) > keep) for (path in setdiff(candidates[seq.int(keep + 1L, length(candidates))], run_root)) {
    if (identical(dirname(path), output_base) && grepl("^[0-9]{14}$", basename(path))) unlink(path, recursive = TRUE)
  }
}
quit(status = if (length(results) > 0L && passed == length(results)) 0 else 1)
