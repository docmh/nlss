#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Independent public-CLI acceptance: no NLSS scientific helper or historical golden.
args <- commandArgs(TRUE)
script <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])
repo <- normalizePath(file.path(dirname(script[1]), "../.."), winslash = "/")
if ("--help" %in% args) {
  cat("Usage: Rscript tests/phase2/run_power_tests.R [--root PATH] [--keep N] [--match REGEX]\n",
      "Honors NLSS_TEST_ROOT, NLSS_KEEP_RUNS and NLSS_TESTS_CONFIG; private offline fixtures/configuration.\n", sep = "")
  quit(status = 0)
}
if (length(args) %% 2L || (length(args) && any(!args[seq(1L, length(args), 2L)] %in% c("--root", "--keep", "--match")))) stop("Unknown/incomplete runner option")
arg <- function(name, fallback) {
  i <- which(args == name)
  if (length(i) > 1L) stop("Repeated runner option: ", name)
  if (length(i)) args[i + 1L] else fallback
}
required <- c("yaml", "jsonlite", "arrow", "digest", "haven", "pwr", "semPower")
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
work <- file.path(run_root, "phase2-power", paste0("run-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid()))
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
near <- function(actual, expected, label, tolerance = 3e-6, probability = grepl("power", label, ignore.case = TRUE)) {
  actual <- as.numeric(actual); expected <- as.numeric(expected)
  check(length(actual) == length(expected) && length(expected) > 0L, paste(label, "length differs"))
  check(identical(is.na(actual), is.na(expected)), paste(label, "missing values differ"))
  valid <- !is.na(expected)
  scale <- if (probability) pmax(1e-300, abs(expected[valid])) else pmax(1, abs(expected[valid]))
  check(all(is.finite(actual[valid])) && all(abs(actual[valid] - expected[valid]) <= tolerance * scale),
    paste(label, "differs: actual", paste(actual, collapse = ","), "expected", paste(expected, collapse = ",")))
  numeric_checks <<- numeric_checks + length(expected)
}
results <- list()
test <- function(name, code) {
  if (!grepl(test_pattern, name)) return(invisible(NULL))
  previous <- getwd(); on.exit(setwd(previous), add = TRUE)
  reset_config(); start <- proc.time()[["elapsed"]]; before <- numeric_checks
  error <- tryCatch({ force(code); NULL }, error = function(e) paste(conditionMessage(e),
    if (!is.null(conditionCall(e))) paste("Call:", paste(deparse(conditionCall(e)), collapse = " ")) else ""))
  results[[length(results) + 1L]] <<- list(module = "power", test = name, passed = is.null(error),
    numeric_checks = numeric_checks - before, seconds = unname(proc.time()[["elapsed"]] - start),
    message = if (is.null(error)) "OK" else error)
  cat(if (is.null(error)) "[PASS] " else "[FAIL] ", name, if (is.null(error)) "" else paste0(": ", error), "\n", sep = "")
}
new_case <- function(name, data = NULL, format = "rds") {
  base <- file.path(work, "cases", name); project <- file.path(base, "project")
  dir.create(project, recursive = TRUE)
  yaml::write_yaml(list(version = 1L, datasets = list()), file.path(project, "nlss-workspace.yml"))
  input <- NULL
  if (!is.null(data)) {
    input <- file.path(base, paste0("sample.", format))
    if (format == "rds") saveRDS(data, input)
    else if (format == "sav") haven::write_sav(data, input)
    else if (format == "parquet") arrow::write_parquet(data, input)
    else write.csv(data, input, row.names = FALSE)
  }
  setwd(project)
  list(project = project, base = base, input = input, flag = paste0("--", format), data = data)
}
run_power <- function(context, options, failure = FALSE) {
  log <- tempfile("command-", tmpdir = context$base, fileext = ".log")
  before <- list.files(context$project, "^result[.]json$", recursive = TRUE, full.names = TRUE)
  options <- c(if (!is.null(context$input)) c(context$flag, context$input), options)
  status <- system2(file.path(R.home("bin"), "Rscript"), c(shQuote(file.path(repo, "scripts/R/power.R")), shQuote(options)), stdout = log, stderr = log)
  check(if (failure) status != 0L else status == 0L, paste("Unexpected exit", status, text_file(log)))
  after <- setdiff(list.files(context$project, "^result[.]json$", recursive = TRUE, full.names = TRUE), before)
  if (failure) {
    for (path in after) {
      result <- read_json(path)
      check(result$status == "failed", "Input error published completed result")
      check(!file.exists(file.path(dirname(path), "output.md")), "Input error published normal Markdown")
    }
    return(invisible(log))
  }
  check(length(after) == 1L, "Expected exactly one terminal run")
  path <- dirname(after); result <- read_json(after); request <- read_json(file.path(path, "request.json"))
  check(result$status == "completed", "Wrong terminal state")
  check(identical(result$artifacts$request$sha256, sha(file.path(path, "request.json"))), "Request hash association differs")
  check(file.exists(file.path(path, "output.md")), "Deterministic Markdown absent")
  for (artifact in result$artifacts) check(identical(sha(file.path(path, artifact$path)), artifact$sha256), "Artifact hash differs")
  check(nrow(result$results$summary_df) == 1L, "Summary must have one calculation")
  list(path = path, request = request, result = result, row = result$results$summary_df[1, ],
    markdown = text_file(file.path(path, "output.md")))
}
check_row <- function(bundle, expected) {
  for (field in names(expected)) {
    if (is.numeric(expected[[field]])) near(bundle$row[[field]], expected[[field]], field)
    else check(identical(as.character(bundle$row[[field]]), expected[[field]]), paste("Unexpected", field))
  }
  invisible(bundle)
}
calc <- function(name, options, expected) check_row(run_power(new_case(name), options), expected)
# The oracles below call published package functions or direct distributions.
# No production source is sourced, evaluated, or used as a statistical oracle.
for (kind in c("one-sample", "paired", "two-sample")) for (mode in c("apriori", "posthoc", "sensitivity")) for (alternative in c("two.sided", "greater", "less")) {
  name <- paste("power_t", kind, mode, alternative, sep = "_")
  if (kind == "two-sample" && mode == "apriori" && alternative == "two.sided") name <- paste0(name, "_smoke")
  test(name, {
    effect <- if (alternative == "less") -.45 else .45
    type <- switch(kind, "one-sample" = "one.sample", "two-sample" = "two.sample", paired = "paired")
    opts <- c("--analysis", "ttest", "--mode", mode, "--t-type", kind, "--alternative", alternative, "--alpha", ".025", "--power", ".85")
    if (mode != "sensitivity") opts <- c(opts, "--effect-size", as.character(effect))
    if (mode != "apriori") opts <- c(opts, if (kind == "two-sample") "--n-per-group" else "--n", "47")
    ref_args <- list(sig.level = .025, alternative = alternative, type = type)
    if (mode != "sensitivity") ref_args$d <- effect
    if (mode != "posthoc") ref_args$power <- .85
    if (mode != "apriori") ref_args$n <- 47
    ref <- do.call(pwr::pwr.t.test, ref_args)
    n <- if (mode == "apriori") ceiling(ref$n) else 47
    expected <- list(effect_size = if (mode == "sensitivity") ref$d else effect, alpha = .025,
      power = if (mode == "posthoc") ref$power else .85, n_total = n * if (kind == "two-sample") 2 else 1)
    if (kind == "two-sample") expected <- c(expected, list(n1 = n, n2 = n))
    bundle <- calc(name, opts, expected)
    attained <- pwr::pwr.t.test(n = n, d = if (mode == "sensitivity") ref$d else effect, sig.level = .025, alternative = alternative, type = type)$power
    near(bundle$row$attained_power, attained, "Power at actual integer N")
  })
}
for (ratio in c(.5, 1.5, 2.3)) test(paste0("power_t_unequal_apriori_ratio_", ratio), {
  target <- .83; effect <- .37
  power_at <- function(n1) pwr::pwr.t2n.test(n1 = n1, n2 = ratio * n1, d = effect, sig.level = .05)$power
  continuous <- uniroot(function(n) power_at(n) - target, c(max(2, 2 / ratio), 1e5), tol = 1e-9)$root
  n1 <- ceiling(continuous); n2 <- ceiling(n1 * ratio)
  bundle <- calc(paste0("unequal-", ratio), c("--analysis", "ttest", "--mode", "apriori", "--effect-size", ".37", "--ratio", as.character(ratio), "--power", ".83"),
    list(n1 = n1, n2 = n2, n_total = n1 + n2, power = target, effect_size = effect))
  near(bundle$row$attained_power, pwr::pwr.t2n.test(n1 = n1, n2 = n2, d = effect, sig.level = .05)$power, "Unequal integer attained power")
})
for (mode in c("posthoc", "sensitivity")) for (alternative in c("two.sided", "greater", "less")) test(paste("power_t_unequal", mode, alternative, sep = "_"), {
  opts <- c("--analysis", "ttest", "--mode", mode, "--n1", "31", "--n2", "67", "--alternative", alternative)
  d <- if (alternative == "less") -.35 else .35
  ref <- if (mode == "posthoc") pwr::pwr.t2n.test(n1 = 31, n2 = 67, d = d, alternative = alternative)
    else pwr::pwr.t2n.test(n1 = 31, n2 = 67, power = .8, alternative = alternative)
  if (mode == "posthoc") opts <- c(opts, "--effect-size", as.character(d))
  calc(paste("unequal", mode, alternative, sep = "-"), opts,
    list(n1 = 31, n2 = 67, n_total = 98, power = ref$power, effect_size = ref$d))
})
for (alternative in c("two.sided", "greater", "less")) for (mode in c("apriori", "posthoc", "sensitivity")) test(paste("power_correlation", mode, alternative, sep = "_"), {
  effect <- if (alternative == "less") -.27 else .27
  opts <- c("--analysis", "correlation", "--mode", mode, "--alternative", alternative, "--alpha", ".01", "--power", ".9")
  if (mode != "sensitivity") opts <- c(opts, "--effect-size", as.character(effect))
  if (mode != "apriori") opts <- c(opts, "--n", "117")
  ref_args <- list(sig.level = .01, alternative = alternative)
  if (mode != "posthoc") ref_args$power <- .9
  if (mode != "sensitivity") ref_args$r <- effect
  if (mode != "apriori") ref_args$n <- 117
  ref <- do.call(pwr::pwr.r.test, ref_args)
  calculated_effect <- if (mode == "sensitivity") ref$r else effect
  n <- if (mode == "apriori") ceiling(ref$n) else 117
  bundle <- calc(paste("cor", mode, alternative, sep = "-"), opts, list(effect_size = calculated_effect, n_total = n, power = if (mode == "posthoc") ref$power else .9))
  near(bundle$row$attained_power, pwr::pwr.r.test(n = n, r = calculated_effect, sig.level = .01, alternative = alternative)$power, "Correlation attained power")
})
for (family in c("ttest", "correlation")) for (alternative in c("greater", "less")) test(paste("power_opposite_signed_effect", family, alternative, sep = "_"), {
  effect <- if (alternative == "greater") -.3 else .3
  ref <- if (family == "ttest") pwr::pwr.t.test(n = 61, d = effect, type = "one.sample", alternative = alternative)$power
    else pwr::pwr.r.test(n = 61, r = effect, alternative = alternative)$power
  calc(paste(family, alternative), c("--analysis", family, "--mode", "posthoc", "--effect-size", as.character(effect),
    "--n", "61", "--alternative", alternative, if (family == "ttest") c("--t-type", "one-sample")),
    list(effect_size = effect, power = ref, n_total = 61))
})
for (mode in c("apriori", "posthoc", "sensitivity")) for (metric in c("f", "eta2")) test(paste("power_anova", mode, metric, sep = "_"), {
  f <- .29; effect <- if (metric == "f") f else f^2 / (1 + f^2)
  opts <- c("--analysis", "anova", "--mode", mode, "--groups", "4", "--effect-metric", metric, "--alpha", ".025")
  if (mode != "sensitivity") opts <- c(opts, "--effect-size", as.character(effect))
  if (mode != "apriori") opts <- c(opts, "--n-per-group", "39")
  ref_args <- list(k = 4, sig.level = .025)
  if (mode != "posthoc") ref_args$power <- .8
  if (mode != "sensitivity") ref_args$f <- f
  if (mode != "apriori") ref_args$n <- 39
  ref <- do.call(pwr::pwr.anova.test, ref_args); n <- if (mode == "apriori") ceiling(ref$n) else 39
  bundle <- calc(paste("anova", mode, metric), opts, list(groups = 4, n_per_group = n, n_total = 4 * n,
    effect_size = if (mode == "sensitivity") ref$f else effect, power = if (mode == "posthoc") ref$power else .8))
  near(bundle$row$attained_power, pwr::pwr.anova.test(k = 4, n = n, f = if (mode == "sensitivity") ref$f else f, sig.level = .025)$power, "ANOVA attained power")
})
for (mode in c("apriori", "posthoc", "sensitivity")) for (metric in c("f2", "r2")) test(paste("power_regression", mode, metric, sep = "_"), {
  f2 <- .13; effect <- if (metric == "f2") f2 else f2 / (1 + f2)
  opts <- c("--analysis", "regression", "--mode", mode, "--u", "4", "--effect-metric", metric, "--alpha", ".025")
  if (mode != "sensitivity") opts <- c(opts, "--effect-size", as.character(effect))
  if (mode != "apriori") opts <- c(opts, "--n", "101")
  ref_args <- list(u = 4, sig.level = .025)
  if (mode != "posthoc") ref_args$power <- .8
  if (mode != "sensitivity") ref_args$f2 <- f2
  if (mode != "apriori") ref_args$v <- 96
  ref <- do.call(pwr::pwr.f2.test, ref_args); n <- if (mode == "apriori") ceiling(ref$v + 5) else 101
  bundle <- calc(paste("reg", mode, metric), opts, list(u = 4, n_total = n,
    effect_size = if (mode == "sensitivity") ref$f2 else effect, power = if (mode == "posthoc") ref$power else .8))
  near(bundle$row$attained_power, pwr::pwr.f2.test(u = 4, v = n - 5, f2 = if (mode == "sensitivity") ref$f2 else f2, sig.level = .025)$power, "Regression attained power")
})
rmsea_power <- function(n, null, alternative, df = 73, alpha = .025) {
  upper <- alternative > null
  critical <- qchisq(if (upper) 1 - alpha else alpha, df, ncp = (n - 1) * df * null^2)
  pchisq(critical, df, ncp = (n - 1) * df * alternative^2, lower.tail = !upper)
}
for (pair in list(c(0, .08), c(.05, .08), c(.08, .05), c(.08, 0))) for (mode in c("apriori", "posthoc")) test(paste("power_sem", mode, paste(pair, collapse = "_"), sep = "_"), {
  null <- pair[1]; effect <- pair[2]
  n <- if (mode == "apriori") ceiling(uniroot(function(n) rmsea_power(n, null, effect) - .85, c(2, 1e4), tol = 1e-9)$root) else 233
  bundle <- calc(paste("sem", mode, null, effect), c("--analysis", "sem", "--mode", mode, "--df", "73", "--rmsea0", as.character(null),
    "--rmsea1", as.character(effect), "--alpha", ".025", "--power", ".85", if (mode == "posthoc") c("--n", "233")),
    list(n_total = n, df = 73, rmsea0 = null, rmsea1 = effect, power = if (mode == "apriori") .85 else rmsea_power(n, null, effect)))
  near(bundle$row$attained_power, rmsea_power(n, null, effect), "SEM attained noncentral power")
  if (null == 0) near(bundle$row$attained_power, semPower::semPower.postHoc(effect = effect, effect.measure = "RMSEA", alpha = .025, N = n, df = 73)$power, "Direct semPower exact-fit result")
})
for (null in c(0, .05, .15)) test(paste0("power_sem_sensitivity_null_", null, if (null == .05) "_smoke" else ""), {
  effect <- uniroot(function(e) rmsea_power(173, null, e) - .8, c(null + 1e-10, .8), tol = 1e-9)$root
  calc(paste0("sem-sens-", null), c("--analysis", "sem", "--mode", "sensitivity", "--df", "73", "--rmsea0", as.character(null), "--n", "173", "--alpha", ".025"),
    list(n_total = 173, rmsea0 = null, rmsea1 = effect, effect_size = effect, attained_power = .8))
})
# Deterministic, deliberately nontrivial source cases; factor rank != predictor count.
set.seed(72914)
data <- data.frame(x = rnorm(96), z = rnorm(96), group = factor(rep(c("B", "A", "C"), 32), levels = c("B", "A", "C", "unused")))
data$y <- .4 * data$x - .2 * data$z + c(B = -.3, A = .2, C = .6)[as.character(data$group)] + rnorm(96)
data$paired <- data$x + .4 + rnorm(96, sd = .8)
data$x[c(2, 7)] <- NA; data$y[c(4, 11)] <- NA; data$paired[8] <- NA; data$group[5] <- NA
check_estimation_rows <- function(bundle, rows) {
  estimation <- bundle$result$results$effect_estimation
  actual <- estimation$included_rows
  if (is.null(actual)) actual <- estimation$row_indices
  near(unlist(actual), rows, "Estimated-effect source row identities", tolerance = 0)
  check(!is.null(bundle$request$dataset$version_id), "Data-estimated effect lacks immutable dataset binding")
}
for (kind in c("one-sample", "paired", "two-sample")) test(paste0("power_estimated_t_", kind), {
  sample <- data
  if (kind == "two-sample") sample <- sample[is.na(sample$group) | sample$group != "C", ]
  options <- c("--analysis", "ttest", "--mode", "posthoc", "--t-type", kind, "--estimate-effect", "TRUE", "--alternative", "less")
  if (kind == "one-sample") {
    rows <- which(is.finite(sample$x)); values <- sample$x[rows]
    fit <- t.test(values, mu = .3); effect <- unname(fit$statistic) / sqrt(length(values))
    options <- c(options, "--vars", "x", "--mu", ".3"); n <- length(values)
    ref <- pwr::pwr.t.test(n = n, d = effect, type = "one.sample", alternative = "less")
  } else if (kind == "paired") {
    rows <- which(is.finite(sample$x) & is.finite(sample$paired))
    fit <- t.test(sample$x[rows], sample$paired[rows], paired = TRUE)
    effect <- unname(fit$statistic) / sqrt(length(rows)); n <- length(rows)
    options <- c(options, "--x", "x", "--y", "paired")
    ref <- pwr::pwr.t.test(n = n, d = effect, type = "paired", alternative = "less")
  } else {
    rows <- which(is.finite(sample$y) & !is.na(sample$group)); used <- droplevels(sample[rows, ])
    fit <- t.test(y ~ group, data = used, var.equal = TRUE); counts <- as.numeric(table(used$group))
    effect <- unname(fit$statistic) * sqrt(sum(1 / counts)); n <- sum(counts)
    options <- c(options, "--vars", "y", "--group", "group")
    ref <- pwr::pwr.t2n.test(n1 = counts[1], n2 = counts[2], d = effect, alternative = "less")
  }
  bundle <- check_row(run_power(new_case(paste0("estimated-t-", kind), sample), options), list(effect_size = effect, power = ref$power, n_total = n))
  check_estimation_rows(bundle, rows)
})
for (format in c("rds", "sav", "parquet", "csv")) test(paste0("power_estimated_correlation_", format, if (format == "sav") "_smoke" else ""), {
  sample <- data[c("x", "y")]
  sample$x <- haven::labelled_spss(sample$x, labels = c("Refused" = 99), na_values = 99, label = "Predictor score")
  sample$x[19] <- 99
  sample$y <- haven::labelled(sample$y, label = "Outcome score")
  expected_data <- data[c("x", "y")]; expected_data$x[19] <- NA_real_
  if (format %in% c("csv", "parquet")) sample <- expected_data
  rows <- which(complete.cases(expected_data)); effect <- cor(expected_data$x[rows], expected_data$y[rows])
  bundle <- check_row(run_power(new_case(paste0("estimated-corr-", format), sample, format),
    c("--analysis", "correlation", "--mode", "posthoc", "--estimate-effect", "TRUE", "--x", "x", "--y", "y")),
    list(effect_size = effect, n_total = length(rows), power = pwr::pwr.r.test(n = length(rows), r = effect)$power))
  check_estimation_rows(bundle, rows)
})
test("power_estimated_t_sav_value_labels_and_user_missings", {
  sample <- data[c("y")]
  codes <- rep(c(20, 10), length.out = nrow(sample)); codes[5] <- NA_real_
  rows <- which(is.finite(sample$y) & !is.na(codes))
  reference_data <- data.frame(y = sample$y[rows], group = factor(codes[rows]))
  counts <- as.numeric(table(reference_data$group))
  effect <- unname(t.test(y ~ group, data = reference_data, var.equal = TRUE)$statistic) * sqrt(sum(1 / counts))
  codes[5] <- 99
  sample$group <- haven::labelled_spss(codes, labels = c("Control" = 10, "Treatment" = 20, "Refused" = 99), na_values = 99, label = "Experimental condition")
  bundle <- check_row(run_power(new_case("estimated-t-sav-labels", sample, "sav"),
    c("--analysis", "ttest", "--mode", "posthoc", "--estimate-effect", "TRUE", "--vars", "y", "--group", "group", "--alternative", "greater")),
    list(effect_size = effect, n1 = counts[1], n2 = counts[2], n_total = sum(counts),
      power = pwr::pwr.t2n.test(n1 = counts[1], n2 = counts[2], d = effect, alternative = "greater")$power))
  check_estimation_rows(bundle, rows)
  check(identical(as.character(bundle$result$results$effect_estimation$groups), c("10", "20")), "SPSS group codes changed direction or became user-missing categories")
  check(grepl("Experimental condition", jsonlite::toJSON(bundle$result$results$effect_estimation$labels, auto_unbox = TRUE)), "SPSS variable label missing from estimation audit")
})
test("power_estimated_anova_observed_groups_smoke", {
  rows <- which(complete.cases(data[c("y", "group")]))
  sample <- droplevels(data[rows, ]); fit <- lm(y ~ group, data = sample)
  eta <- summary(fit)$r.squared; f <- sqrt(eta / (1 - eta)); k <- nlevels(sample$group)
  ref <- pwr::pwr.anova.test(k = k, f = f, power = .8)
  bundle <- check_row(run_power(new_case("estimated-anova", data), c("--analysis", "anova", "--mode", "apriori", "--estimate-effect", "TRUE", "--dv", "y", "--group", "group")),
    list(effect_size = eta, groups = k, n_per_group = ceiling(ref$n), n_total = k * ceiling(ref$n)))
  check_estimation_rows(bundle, rows)
})
for (aliased in c(FALSE, TRUE)) test(paste0("power_estimated_regression_actual_rank_", aliased), {
  sample <- data; sample$duplicate <- 2 * sample$x
  vars <- c("y", "x", "group", if (aliased) "duplicate")
  rows <- which(complete.cases(sample[vars])); fit <- lm(reformulate(vars[-1], response = "y"), data = sample[rows, ])
  r2 <- summary(fit)$r.squared; u <- fit$rank - 1L; n <- nobs(fit)
  bundle <- check_row(run_power(new_case(paste0("estimated-reg-", aliased), sample), c("--analysis", "regression", "--mode", "posthoc",
    "--estimate-effect", "TRUE", "--dv", "y", "--ivs", paste(vars[-1], collapse = ","))),
    list(effect_size = r2, r2 = r2, u = u, n_total = n, power = pwr::pwr.f2.test(u = u, v = n - u - 1, f2 = r2 / (1 - r2))$power))
  check_estimation_rows(bundle, rows)
})
for (case in list(
  list("alpha_zero", c("--alpha", "0")), list("alpha_one", c("--alpha", "1")),
  list("alpha_text", c("--alpha", "invalid")), list("power_zero", c("--power", "0")),
  list("power_one", c("--power", "1")), list("fractional_n", c("--n", "20.5")),
  list("ratio_zero", c("--ratio", "0")), list("negative_ratio", c("--ratio", "-1")),
  list("nonfinite_effect", c("--effect-size", "Inf")), list("unknown_flag", c("--alhpa", ".1")))) test(paste0("power_invalid_", case[[1]]), {
  options <- c("--analysis", "ttest", "--mode", "posthoc", "--t-type", "one-sample", "--effect-size", ".3", "--n", "30")
  keys <- options[seq(1, length(options), 2)]; override <- case[[2]]
  keep <- !keys %in% override[seq(1, length(override), 2)]
  options <- c(as.vector(rbind(keys[keep], options[seq(2, length(options), 2)][keep])), override)
  run_power(new_case(paste0("invalid-", case[[1]])), options, failure = TRUE)
})
for (family in c("ttest", "anova", "regression")) test(paste0("power_zero_effect_posthoc_", family), {
  extra <- switch(family, ttest = c("--t-type", "one-sample", "--alternative", "greater", "--n", "40"),
    anova = c("--groups", "3", "--n-per-group", "40"), regression = c("--u", "3", "--n", "120"))
  bundle <- calc(paste0("zero-effect-", family), c("--analysis", family, "--mode", "posthoc", "--effect-size", "0", extra),
    list(effect_size = 0, power = .05, attained_power = .05))
})
test("power_sem_effect_size_alias", {
  bundle <- calc("sem-effect-alias", c("--analysis", "sem", "--mode", "posthoc", "--sem-df", "73", "--rmsea0", ".05", "--effect-size", ".09", "--n", "173", "--alpha", ".025"),
    list(effect_size = .09, rmsea1 = .09, power = rmsea_power(173, .05, .09)))
})
test("power_sem_effect_size_alias_conflict_rejected", {
  run_power(new_case("sem-effect-conflict"), c("--analysis", "sem", "--mode", "posthoc", "--sem-df", "73", "--rmsea0", ".05", "--rmsea1", ".08", "--effect-size", ".09", "--n", "173"), failure = TRUE)
})
test("power_total_allocation_disclosed_smoke", {
  bundle <- calc("total-allocation", c("--analysis", "ttest", "--mode", "posthoc", "--effect-size", ".3", "--n", "91", "--ratio", "2"),
    list(n1 = 31, n2 = 60, n_total = 91, ratio = 60 / 31, power = pwr::pwr.t2n.test(n1 = 31, n2 = 60, d = .3)$power))
  near(bundle$result$results$calculation$requested_ratio, 2, "Requested allocation ratio")
  near(bundle$result$results$calculation$requested_sample_sizes$n_total, 91, "Requested total N")
  check(any(grepl("integer allocation", bundle$result$results$notes)), "Integer allocation difference not disclosed")
})
test("power_per_group_allocation_disclosed", {
  bundle <- calc("group-allocation", c("--analysis", "ttest", "--mode", "posthoc", "--effect-size", ".3", "--n-per-group", "31", "--ratio", "1.5"),
    list(n1 = 31, n2 = 47, n_total = 78, ratio = 47 / 31, power = pwr::pwr.t2n.test(n1 = 31, n2 = 47, d = .3)$power))
  near(bundle$result$results$calculation$requested_ratio, 1.5, "Requested allocation ratio")
  check(any(grepl("integer allocation", bundle$result$results$notes)), "Integer allocation difference not disclosed")
})
test("power_anova_total_rounding_disclosed", {
  bundle <- calc("anova-total-allocation", c("--analysis", "anova", "--mode", "posthoc", "--effect-size", ".3", "--groups", "3", "--n", "91"),
    list(n_total = 90, n_per_group = 30, groups = 3, power = pwr::pwr.anova.test(k = 3, n = 30, f = .3)$power))
  near(bundle$result$results$calculation$requested_sample_sizes$n_total, 91, "Requested total N")
  check(any(grepl("rounds total N down", bundle$result$results$notes)), "Discarded balanced-design allocation remainder not disclosed")
})
for (case in list(
  list("ttest_zero_apriori", c("--analysis", "ttest", "--effect-size", "0")),
  list("ttest_opposite_apriori", c("--analysis", "ttest", "--effect-size", "-.3", "--alternative", "greater")),
  list("ttest_n_conflict", c("--analysis", "ttest", "--mode", "posthoc", "--effect-size", ".3", "--n1", "30", "--n2", "40", "--n", "71")),
  list("ttest_incomplete_n", c("--analysis", "ttest", "--mode", "posthoc", "--effect-size", ".3", "--n1", "30")),
  list("anova_one_group", c("--analysis", "anova", "--effect-size", ".3", "--groups", "1")),
  list("anova_fractional_group", c("--analysis", "anova", "--effect-size", ".3", "--groups", "2.5")),
  list("anova_eta_one", c("--analysis", "anova", "--effect-metric", "eta2", "--effect-size", "1")),
  list("correlation_perfect", c("--analysis", "correlation", "--effect-size", "1")),
  list("correlation_n_small", c("--analysis", "correlation", "--mode", "posthoc", "--effect-size", ".3", "--n", "3")),
  list("regression_fractional_u", c("--analysis", "regression", "--effect-size", ".3", "--u", "1.5")),
  list("regression_r2_negative", c("--analysis", "regression", "--effect-metric", "r2", "--effect-size", "-.1")),
  list("regression_n_small", c("--analysis", "regression", "--mode", "posthoc", "--effect-size", ".3", "--u", "3", "--n", "4")),
  list("sem_null_equals_alt", c("--analysis", "sem", "--df", "40", "--rmsea0", ".05", "--rmsea1", ".05")),
  list("sem_negative_null", c("--analysis", "sem", "--df", "40", "--rmsea0", "-.05")),
  list("sem_zero_df", c("--analysis", "sem", "--df", "0")),
  list("sem_fractional_df", c("--analysis", "sem", "--df", "3.5")))) test(paste0("power_domain_", case[[1]]), {
  run_power(new_case(paste0("domain-", case[[1]])), case[[2]], failure = TRUE)
})
for (problem in c("infinite", "constant", "text")) test(paste0("power_invalid_effect_data_", problem), {
  sample <- data
  if (problem == "infinite") sample$x[20] <- Inf
  if (problem == "constant") sample$x <- 1
  if (problem == "text") sample$x <- rep("not a numeric score", nrow(sample))
  run_power(new_case(paste0("invalid-effect-data-", problem), sample), c("--analysis", "ttest", "--mode", "posthoc", "--t-type", "one-sample", "--estimate-effect", "TRUE", "--vars", "x"), failure = TRUE)
})
test("power_estimated_group_conflict_rejected", {
  run_power(new_case("anova-group-conflict", data), c("--analysis", "anova", "--mode", "apriori", "--estimate-effect", "TRUE", "--dv", "y", "--group", "group", "--groups", "2"), failure = TRUE)
})
test("power_estimated_rank_conflict_rejected", {
  run_power(new_case("reg-u-conflict", data), c("--analysis", "regression", "--mode", "posthoc", "--estimate-effect", "TRUE", "--dv", "y", "--ivs", "x,group", "--u", "2"), failure = TRUE)
})
summary_path <- file.path(work, "results.json")
jsonlite::write_json(list(schema_version = 1L, suite = "phase2-power", modules = "power", test_pattern = test_pattern,
  numeric_checks = numeric_checks, tests = results, source_sha256 = sha(file.path(repo, "scripts/R/power.R")),
  test_sha256 = sha(normalizePath(script[1], winslash = "/")),
  r_source_sha256 = as.list(vapply(sort(list.files(file.path(repo, "scripts/R"), "[.]R$", recursive = TRUE, full.names = TRUE)), sha, character(1))),
  environment = list(r = R.version.string, packages = as.list(vapply(required, function(p) as.character(utils::packageVersion(p)), character(1))))),
  summary_path, auto_unbox = TRUE, pretty = TRUE, digits = NA)
passed <- sum(vapply(results, function(result) result$passed, logical(1)))
cat(sprintf("Phase 2 Power: %d/%d cases passed; %d numeric comparisons. Results: %s\n", passed, length(results), numeric_checks, summary_path))
if (!nzchar(forced_root) && keep > 0L && dir.exists(output_base)) {
  candidates <- sort(list.dirs(output_base, full.names = TRUE, recursive = FALSE), decreasing = TRUE)
  candidates <- candidates[grepl("^[0-9]{14}$", basename(candidates))]
  if (length(candidates) > keep) for (path in setdiff(candidates[seq.int(keep + 1L, length(candidates))], run_root)) {
    if (identical(dirname(path), output_base) && grepl("^[0-9]{14}$", basename(path))) unlink(path, recursive = TRUE)
  }
}
quit(status = if (length(results) > 0L && passed == length(results)) 0 else 1)
