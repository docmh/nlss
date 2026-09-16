#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Public-CLI acceptance. Numeric oracles use base R/stats, never NLSS helpers.
args <- commandArgs(TRUE)
script <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])
repo <- normalizePath(file.path(dirname(script[1]), "../.."), winslash = "/")
if ("--help" %in% args) {
  cat("Usage: Rscript tests/phase2/run_inference_tests.R [--root PATH] [--keep N] [--modules t_test,correlations] [--match REGEX]\n",
      "Honors NLSS_TEST_ROOT, NLSS_KEEP_RUNS and NLSS_TESTS_CONFIG; uses private configuration and no network.\n", sep = "")
  quit(status = 0)
}
if (length(args) %% 2L || (length(args) && any(!args[seq(1L, length(args), 2L)] %in% c("--root", "--keep", "--modules", "--match")))) stop("Unknown/incomplete runner option")
arg <- function(name, fallback) {
  i <- which(args == name)
  if (length(i) > 1L) stop("Repeated runner option: ", name)
  if (length(i)) args[i + 1L] else fallback
}
required <- c("yaml", "jsonlite", "arrow", "digest", "haven")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing test packages: ", paste(missing, collapse = ", "))
absolute <- function(path) normalizePath(if (grepl("^(/|[A-Za-z]:|\\\\)", path)) path else file.path(repo, path), winslash = "/", mustWork = FALSE)
cfg <- yaml::read_yaml(absolute(Sys.getenv("NLSS_TESTS_CONFIG", "tests/tests.yml")), eval.expr = FALSE)$tests
modules <- strsplit(arg("--modules", "t_test,correlations"), ",", fixed = TRUE)[[1]]
if (!length(modules) || anyDuplicated(modules) || any(!modules %in% c("t_test", "correlations"))) stop("Unsupported --modules selection")
test_pattern <- arg("--match", ".*")
if (!nzchar(test_pattern)) stop("Empty --match regular expression")
invisible(grepl(test_pattern, "validate regular expression"))
forced_root <- arg("--root", Sys.getenv("NLSS_TEST_ROOT", ""))
output_base <- absolute(cfg$output_dir)
run_root <- if (nzchar(forced_root)) absolute(forced_root) else file.path(output_base, format(Sys.time(), "%Y%m%d%H%M%S"))
keep <- suppressWarnings(as.integer(arg("--keep", Sys.getenv("NLSS_KEEP_RUNS", as.character(cfg$keep_runs_default)))))
if (length(keep) != 1L || is.na(keep) || keep < 0L) stop("Invalid --keep")
work <- file.path(run_root, "phase2-inference", paste0("run-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid()))
dir.create(work, recursive = TRUE)
baseline <- yaml::read_yaml(file.path(repo, "scripts/config.yml"), eval.expr = FALSE)
baseline$defaults$digits <- 5L
private_config <- file.path(work, "config.yml")
reset_config <- function() yaml::write_yaml(baseline, private_config)
reset_config()
Sys.setenv(NLSS_CONFIG_PATH = private_config)
text_file <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
read_json <- function(path) jsonlite::fromJSON(path, simplifyVector = TRUE)
sha <- function(path) digest::digest(file = path, algo = "sha256")
check <- function(ok, message) if (!isTRUE(ok)) stop(message, call. = FALSE)
numeric_checks <- 0L
near <- function(actual, expected, label, tolerance = 1e-8) {
  actual <- as.numeric(actual); expected <- as.numeric(expected)
  check(length(actual) == length(expected) && length(expected) > 0L, paste(label, "length differs"))
  expected[is.infinite(expected)] <- NA_real_
  check(identical(is.na(actual), is.na(expected)), paste(label, "missing values differ"))
  keep <- !is.na(expected)
  check(all(is.finite(actual[keep])) && all(abs(actual[keep] - expected[keep]) <= tolerance * pmax(1, abs(expected[keep]))), paste(label, "differs from independent reference"))
  numeric_checks <<- numeric_checks + length(expected)
}
results <- list()
test <- function(module, name, code) {
  if (!module %in% modules || !grepl(test_pattern, name)) return(invisible(NULL))
  previous <- getwd(); on.exit(setwd(previous), add = TRUE)
  reset_config(); start <- proc.time()[["elapsed"]]; before <- numeric_checks
  error <- tryCatch({ force(code); NULL }, error = function(e) conditionMessage(e))
  results[[length(results) + 1L]] <<- list(module = module, test = name, passed = is.null(error),
    numeric_checks = numeric_checks - before, seconds = unname(proc.time()[["elapsed"]] - start), message = if (is.null(error)) "OK" else error)
  cat(if (is.null(error)) "[PASS] " else "[FAIL] ", name, if (is.null(error)) "" else paste0(": ", error), "\n", sep = "")
}
new_case <- function(name, data, format = "rds") {
  base <- file.path(work, "cases", name); project <- file.path(base, "project")
  dir.create(project, recursive = TRUE)
  yaml::write_yaml(list(version = 1L, datasets = list()), file.path(project, "nlss-workspace.yml"))
  input <- file.path(base, paste0("sample.", format))
  if (format == "rds") saveRDS(data, input) else if (format == "sav") haven::write_sav(data, input) else write.csv(data, input, row.names = FALSE)
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
new_run <- function(context, module, options, failure = FALSE, source = TRUE, expected_invalid = FALSE) {
  before <- runs(context)
  command <- run_module(module, c(if (source) c(context$flag, context$input), options), failure && !expected_invalid)
  added <- setdiff(runs(context), before)
  check(length(added) == 1L, paste(module, "did not record exactly one terminal run"))
  request_path <- file.path(added, "request.json"); result_path <- file.path(added, "result.json")
  request <- read_json(request_path); result <- read_json(result_path)
  check(identical(result$status, if (failure) "failed" else "completed"), "Wrong terminal state")
  check(identical(result$artifacts$request$sha256, sha(request_path)), "Request/result hash association differs")
  output <- file.path(added, "output.md")
  if (failure) {
    check(!file.exists(output) && !is.null(result$error), "Failed analysis has normal output or no error")
  } else {
    check(file.exists(output), "Completed analysis has no Markdown")
    for (artifact in result$artifacts) check(identical(sha(file.path(added, artifact$path)), artifact$sha256), "Artifact hash mismatch")
    for (template in request$templates) check(identical(sha(file.path(added, template$path)), template$sha256), "Template hash mismatch")
    for (pair in list(c("snapshot_path", "data_sha256"), c("dictionary_path", "dictionary_sha256"))) {
      check(identical(sha(file.path(context$project, request$dataset[[pair[1]]])), request$dataset[[pair[2]]]), "Immutable input/dictionary hash mismatch")
    }
    check(length(request$options) > 0L && length(request$design) > 0L && length(request$environment$packages) > 0L, "Resolved request lacks scientific/environment context")
  }
  list(path = added, request_path = request_path, request = request, result = result, markdown = if (failure) NULL else text_file(output), command = command)
}
replay <- function(context, bundle) {
  out <- new_run(context, "replay_run", c("--request", bundle$request_path), source = FALSE)
  check(identical(out$result$results, bundle$result$results), "Replay changed raw statistical results")
  check(identical(out$markdown, bundle$markdown), "Replay changed Markdown bytes")
  out
}
markdown_table <- function(markdown, key, occurrence = 1L) {
  lines <- strsplit(markdown, "\n", fixed = TRUE)[[1]]
  cells <- function(line) trimws(strsplit(sub("[|][[:space:]]*$", "", sub("^[[:space:]]*[|]", "", line)), "|", fixed = TRUE)[[1]])
  positions <- which(vapply(seq_along(lines), function(i) grepl("^[[:space:]]*[|]", lines[i]) && key %in% cells(lines[i]) && i < length(lines) && grepl("^[| :|-]+$", trimws(lines[i + 1L])) && grepl("---", lines[i + 1L], fixed = TRUE), logical(1)))
  check(length(positions) >= occurrence, paste("No Markdown table header", key))
  start <- positions[occurrence]; i <- start + 2L; rows <- list()
  while (i <= length(lines) && grepl("^[[:space:]]*[|]", lines[i])) { rows[[length(rows) + 1L]] <- cells(lines[i]); i <- i + 1L }
  check(length(rows) > 0L, "Empty Markdown table")
  out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE); names(out) <- cells(lines[start]); out
}
numeric_cells <- function(x) suppressWarnings(as.numeric(gsub("%", "", x, fixed = TRUE)))

# Oracles are independent public stats functions, regression fits, and central
# moments. Bootstrap draws below implement the documented sampling protocol,
# not an NLSS function; no analysis implementation is ever sourced.
set.seed(981723)
sample_data <- data.frame(a = stats::rnorm(64, 4, 1.4), b = stats::rnorm(64, 2, 2),
  c = stats::rnorm(64), z = stats::rnorm(64))
sample_data$b <- sample_data$b + .6 * sample_data$a
sample_data$c <- sample_data$c + .35 * sample_data$a - .4 * sample_data$z
sample_data$a[c(2, 23)] <- NA; sample_data$b[c(4, 39)] <- NA
sample_data$c[c(8, 51)] <- NA; sample_data$z[c(9, 53)] <- NA
sample_data$group <- factor(rep(c("B", "A"), each = 32), levels = c("A", "B", "unused"))
sample_data$group[c(7, 45)] <- NA
near_probability <- function(actual, expected, label) {
  actual <- as.numeric(actual); expected <- as.numeric(expected)
  check(length(actual) == length(expected) && length(expected) > 0L, paste(label, "length differs"))
  check(identical(is.na(actual), is.na(expected)), paste(label, "missing values differ"))
  valid <- !is.na(expected)
  check(all(is.finite(actual[valid])) && all(abs(actual[valid] - expected[valid]) <= pmax(1e-300, abs(expected[valid]) * 2e-7)), paste(label, "differs from independent probability reference"))
  numeric_checks <<- numeric_checks + length(expected)
}
check_row <- function(row, reference, label) for (field in names(reference)) {
  if (grepl("(^p$|_p$|^p_|^shapiro_p$)", field)) near_probability(row[[field]], reference[[field]], paste(label, field))
  else near(row[[field]], reference[[field]], paste(label, field), tolerance = 3e-8)
}
tail_probability <- function(value, alternative, distribution = stats::pnorm) {
  switch(alternative, two.sided = 2 * distribution(-abs(value)), greater = distribution(value, lower.tail = FALSE), less = distribution(value))
}
percentile <- function(values, alternative, confidence = .9, bounds = c(-Inf, Inf)) {
  values <- values[is.finite(values)]
  switch(alternative, two.sided = as.numeric(stats::quantile(values, c((1 - confidence) / 2, (1 + confidence) / 2))),
    greater = c(as.numeric(stats::quantile(values, 1 - confidence)), bounds[2]),
    less = c(bounds[1], as.numeric(stats::quantile(values, confidence))))
}
effect_d <- function(x, y = NULL) {
  if (is.null(y)) return(mean(x) / stats::sd(x))
  fit <- stats::lm(c(x, y) ~ factor(rep(c(0, 1), c(length(x), length(y)))))
  (mean(x) - mean(y)) / summary(fit)$sigma
}
ttest_reference <- function(data, row, type, alternative, equal = FALSE, mu = 0, bootstrap = FALSE, draws = 79L) {
  if (type == "one_sample") {
    x <- data[[row$variable]]; x <- x[!is.na(x)]; y <- NULL
    test_value <- stats::t.test(x, mu = mu, alternative = alternative, conf.level = .9)
    contrast <- x - mu
    expected <- list(n_1 = length(x), n_2 = NA_real_, mean_1 = mean(x), mean_2 = NA_real_, sd_1 = stats::sd(x), sd_2 = NA_real_,
      mean_diff = mean(x) - mu, d = effect_d(contrast), mu = mu)
  } else if (type == "paired") {
    pair <- data[c(row$measure_1, row$measure_2)]; pair <- pair[stats::complete.cases(pair), , drop = FALSE]
    x <- pair[[1]]; y <- pair[[2]]; contrast <- x - y
    test_value <- stats::t.test(x, y, paired = TRUE, alternative = alternative, conf.level = .9)
    expected <- list(n_1 = length(x), n_2 = NA_real_, mean_1 = mean(x), mean_2 = mean(y), sd_1 = stats::sd(x), sd_2 = stats::sd(y),
      mean_diff = mean(contrast), d = effect_d(contrast))
  } else {
    good <- !is.na(data[[row$variable]]) & !is.na(data$group)
    raw_groups <- unique(data$group[!is.na(data$group)])
    x <- data[[row$variable]][good & data$group == raw_groups[row$group_1_id]]
    y <- data[[row$variable]][good & data$group == raw_groups[row$group_2_id]]
    test_value <- stats::t.test(x, y, alternative = alternative, var.equal = equal, conf.level = .9)
    expected <- list(n_1 = length(x), n_2 = length(y), mean_1 = mean(x), mean_2 = mean(y), sd_1 = stats::sd(x), sd_2 = stats::sd(y),
      mean_diff = mean(x) - mean(y), d = effect_d(x, y))
  }
  expected <- c(expected, list(t = unname(test_value$statistic), df = unname(test_value$parameter), p = test_value$p.value,
    ci_low = test_value$conf.int[1], ci_high = test_value$conf.int[2], stderr = test_value$stderr, conf_level = .9,
    null_value = if (type == "one_sample") mu else 0))
  if (type == "one_sample") expected <- c(expected, list(diff_ci_low = test_value$conf.int[1] - mu, diff_ci_high = test_value$conf.int[2] - mu))
  if (bootstrap) {
    draw <- function(statistic) {
      if (type == "independent") replicate(draws, {
        first_sample <- x[sample.int(length(x), length(x), replace = TRUE)]
        second_sample <- y[sample.int(length(y), length(y), replace = TRUE)]
        statistic(first_sample, second_sample)
      })
      else replicate(draws, statistic(contrast[sample.int(length(contrast), length(contrast), replace = TRUE)]))
    }
    boot_mean <- if (type == "independent") draw(function(x, y) mean(x) - mean(y)) else draw(mean)
    boot_d <- if (type == "independent") draw(effect_d) else draw(effect_d)
    mean_ci <- percentile(boot_mean, alternative); d_ci <- percentile(boot_d, alternative)
    expected <- c(expected, list(boot_ci_low = mean_ci[1], boot_ci_high = mean_ci[2], boot_d_ci_low = d_ci[1], boot_d_ci_high = d_ci[2]))
  }
  list(expected = expected, samples = if (type == "independent") list(x, y) else list(if (type == "paired") contrast else x))
}
check_ttest <- function(bundle, data, alternative = "two.sided", equal = FALSE, mu = 0, bootstrap = FALSE, seed = 1L) {
  rows <- bundle$result$results$summary_df; diagnostics <- bundle$result$results$diagnostics_df
  designs <- jsonlite::fromJSON(bundle$request_path, simplifyVector = FALSE)$design$tests
  check(length(designs) == nrow(rows), "T-test case-selection design count differs")
  set.seed(seed)
  for (i in seq_len(nrow(rows))) {
    row <- rows[i, , drop = FALSE]
    needed <- if (row$test_type == "paired") c(row$measure_1, row$measure_2) else c(row$variable, if (row$test_type == "independent") "group")
    expected_rows <- which(stats::complete.cases(data[needed]))
    check(identical(as.integer(unlist(designs[[i]]$row_indices)), expected_rows), "T-test selected source row identities differ")
    reference <- ttest_reference(data, row, row$test_type, alternative, equal, mu, bootstrap)
    check_row(row, reference$expected, paste("t-test", row$test_type, row$variable))
    for (field in intersect(names(reference$expected), c("ci_low", "ci_high", "boot_ci_low", "boot_ci_high", "boot_d_ci_low", "boot_d_ci_high"))) {
      value <- reference$expected[[field]]
      if (is.infinite(value)) check(identical(row[[paste0(field, "_status")]], if (value > 0) "positive_infinity" else "negative_infinity"), "Infinite t-test interval bound lost its JSON status")
    }
    actual_diag <- diagnostics[diagnostics$variable == row$variable, , drop = FALSE]
    check(nrow(actual_diag) == length(reference$samples), "T-test diagnostic sample count differs")
    for (j in seq_along(reference$samples)) {
      values <- reference$samples[[j]]; sw <- stats::shapiro.test(values)
      check_row(actual_diag[j, ], list(n = length(values), shapiro_w = unname(sw$statistic), shapiro_p = sw$p.value), "t-test diagnostics")
    }
    if (row$test_type == "independent") {
      variance <- stats::var.test(reference$samples[[1]], reference$samples[[2]])
      check_row(actual_diag[2, ], list(var_test_f = unname(variance$statistic), var_test_p = variance$p.value), "variance diagnostic")
    }
  }
  table <- markdown_table(bundle$markdown, "Variable")
  near(numeric_cells(table$M1), round(rows$mean_1, bundle$request$options$digits), "T-test rounded Markdown M1")
  near(numeric_cells(table$t), round(rows$t, bundle$request$options$digits), "T-test rounded Markdown t")
}
ttest_options <- function(type) switch(type, one_sample = c("--vars", "a,b", "--mu", "2.5"),
  independent = c("--vars", "a,b", "--group", "group"), paired = c("--x", "a,c", "--y", "b,a"))
for (type in c("one_sample", "independent", "paired")) for (alternative in c("two.sided", "greater", "less")) {
  test("t_test", paste("ttest_all_values", type, alternative, sep = "_"), {
    context <- new_case(paste("ttest", type, alternative, sep = "-"), sample_data)
    bundle <- new_run(context, "t_test", c(ttest_options(type), "--alternative", alternative, "--conf-level", ".9", "--digits", "4"))
    check_ttest(bundle, sample_data, alternative, mu = if (type == "one_sample") 2.5 else 0)
    if (type == "independent") check(all(bundle$result$results$summary_df$group_1 == "B"), "First-observed group order changed to factor-level order")
    if (alternative == "two.sided") replay(context, bundle)
  })
}
test("t_test", "ttest_pooled_variance_all_values", {
  context <- new_case("ttest-pooled", sample_data)
  bundle <- new_run(context, "t_test", c(ttest_options("independent"), "--var-equal", "TRUE", "--conf-level", ".9"))
  check_ttest(bundle, sample_data, equal = TRUE)
})
for (type in c("one_sample", "independent", "paired")) for (alternative in c("two.sided", "greater", "less")) {
  test("t_test", paste("ttest_bootstrap_numeric", type, alternative, sep = "_"), {
    context <- new_case(paste("ttest-boot", type, alternative, sep = "-"), sample_data)
    bundle <- new_run(context, "t_test", c(ttest_options(type), "--alternative", alternative, "--conf-level", ".9", "--bootstrap", "TRUE", "--bootstrap-samples", "79", "--seed", "341"))
    check_ttest(bundle, sample_data, alternative, mu = if (type == "one_sample") 2.5 else 0, bootstrap = TRUE, seed = 341L)
    if (alternative == "two.sided") replay(context, bundle)
  })
}
test("t_test", "ttest_SPSS_labels_user_missing_group_order", {
  data <- sample_data; data$a[2] <- 99; data$group <- ifelse(is.na(data$group), 9, ifelse(data$group == "B", 2, 1))
  data$a <- haven::labelled_spss(data$a, labels = c("Nicht beantwortet" = 99), na_values = 99, label = "Belastung")
  data$group <- haven::labelled_spss(data$group, labels = c("Kontrolle" = 1, "Intervention" = 2, "Fehlend" = 9), na_values = 9, label = "Bedingung")
  context <- new_case("ttest-spss", data, "sav")
  bundle <- new_run(context, "t_test", c("--vars", "a,b", "--group", "group", "--conf-level", ".9"))
  clean <- sample_data; clean$group <- ifelse(is.na(sample_data$group), NA_real_, ifelse(sample_data$group == "B", 2, 1))
  check_ttest(bundle, clean)
  check(grepl("Belastung", bundle$markdown, fixed = TRUE) && grepl("Intervention", bundle$markdown, fixed = TRUE), "SPSS t-test labels missing")
  replay(context, bundle)
})

partial_reference <- function(data, var1, var2, controls, method, alternative, confidence = .9) {
  d <- data[c(var1, var2, controls)]
  if (method == "spearman") d[] <- lapply(d, rank, ties.method = "average")
  design <- cbind(1, as.matrix(d[controls])); decomposition <- qr(design)
  x <- qr.resid(decomposition, d[[var1]]); y <- qr.resid(decomposition, d[[var2]])
  r <- stats::cor(x, y); k <- decomposition$rank - 1L
  # p from the slope test in a separate regression, not a reimplementation of
  # the module's r-to-t expression. Its df accounts for effective control rank.
  fit <- stats::lm(d[[var1]] ~ d[[var2]] + as.matrix(d[controls]))
  statistic <- summary(fit)$coefficients[2, "t value"]
  p <- tail_probability(statistic, alternative, function(q, ...) stats::pt(q, df = fit$df.residual, ...))
  n <- nrow(d); ci <- c(NA_real_, NA_real_)
  if (n > k + 3L) {
    z <- atanh(r); se <- 1 / sqrt(n - k - 3)
    ci <- switch(alternative, two.sided = tanh(z + c(-1, 1) * stats::qnorm((1 + confidence) / 2) * se),
      greater = c(tanh(z - stats::qnorm(confidence) * se), 1), less = c(-1, tanh(z + stats::qnorm(confidence) * se)))
  }
  list(r = r, p_value = p, ci_low = ci[1], ci_high = ci[2], df = fit$df.residual, control_rank = k, statistic = statistic)
}
check_correlations <- function(bundle, data, variables = c("a", "b", "c"), controls = character(), method = "pearson", alternative = "two.sided", missing = "pairwise", adjust = "none", grouped = FALSE, bootstrap = FALSE, seed = 1L, r0 = NULL) {
  rows <- bundle$result$results$summary_df
  designs <- jsonlite::fromJSON(bundle$request_path, simplifyVector = FALSE)$design$groups
  raw_groups <- if (grouped) unique(data$group) else list(NULL)
  expected_groups <- lapply(seq_along(raw_groups), function(i) {
    if (!grouped) seq_len(nrow(data)) else which(if (is.na(raw_groups[i])) is.na(data$group) else !is.na(data$group) & data$group == raw_groups[i])
  })
  check(identical(vapply(designs, function(group) as.integer(group$group_id), integer(1)), seq_along(expected_groups)), "Stable first-observed group IDs differ")
  for (group in designs) {
    expected_group <- expected_groups[[group$group_id]]
    check(identical(as.integer(unlist(group$row_indices)), expected_group), "Correlation group source row identities differ")
    for (pair in group$pairs) {
      needed <- unique(c(if (missing == "complete") variables else c(pair$var1, pair$var2), controls))
      expected_pair <- expected_group[stats::complete.cases(data[expected_group, needed, drop = FALSE])]
      check(identical(as.integer(unlist(pair$included_rows)), expected_pair), "Correlation pair source row identities differ")
      check(identical(as.integer(unlist(pair$excluded_rows)), setdiff(expected_group, expected_pair)), "Correlation excluded source row identities differ")
    }
  }
  set.seed(seed)
  for (i in seq_len(nrow(rows))) {
    row <- rows[i, , drop = FALSE]
    d <- data[expected_groups[[row$group_id]], , drop = FALSE]
    joint <- unique(c(if (missing == "complete") variables else c(row$var1, row$var2), controls))
    selected <- stats::complete.cases(d[joint]); good <- d[selected, , drop = FALSE]
    if (length(controls)) reference <- partial_reference(good, row$var1, row$var2, controls, method, alternative) else {
      value <- suppressWarnings(stats::cor.test(good[[row$var1]], good[[row$var2]], method = method, alternative = alternative, conf.level = .9))
      ci <- if (length(value$conf.int)) value$conf.int else c(NA_real_, NA_real_)
      reference <- list(r = unname(value$estimate), p_value = value$p.value, ci_low = ci[1], ci_high = ci[2],
        statistic = unname(value$statistic), df = if (length(value$parameter)) unname(value$parameter) else NA_real_)
    }
    reference <- c(reference, list(n = sum(selected), total_n = nrow(d), missing_n = sum(!selected), missing_pct = mean(!selected) * 100))
    if (!is.null(r0)) {
      k <- if (length(controls)) reference$control_rank else 0
      z <- (atanh(reference$r) - atanh(r0)) * sqrt(nrow(good) - k - 3)
      reference <- c(reference, list(r0 = r0, z_r0 = z, p_r0 = tail_probability(z, alternative)))
    }
    if (bootstrap) {
      coefficients <- replicate(79L, {
        draw <- good[sample.int(nrow(good), nrow(good), replace = TRUE), , drop = FALSE]
        if (length(controls)) partial_reference(draw, row$var1, row$var2, controls, method, alternative)$r
        else stats::cor(draw[[row$var1]], draw[[row$var2]], method = method)
      })
      interval <- percentile(coefficients, alternative, bounds = c(-1, 1))
      reference <- c(reference, list(boot_ci_low = interval[1], boot_ci_high = interval[2], bootstrap_valid = 79, bootstrap_failed = 0))
    }
    check_row(row, reference, paste("correlation", row$group, row$var1, row$var2))
  }
  if (adjust != "none") {
    partitions <- split(seq_len(nrow(rows)), rows$group_id)
    for (indices in partitions) near_probability(rows$p_adjusted[indices], stats::p.adjust(rows$p_value[indices], method = adjust), paste("p.adjust", adjust))
  }
  diagnostics <- bundle$result$results$diagnostics_df
  for (i in seq_len(nrow(diagnostics))) {
    row <- diagnostics[i, , drop = FALSE]
    d <- data[expected_groups[[row$group_id]], , drop = FALSE]
    x <- d[[row$variable]]; x <- x[!is.na(x)]; n <- length(x); centered <- x - mean(x)
    moments <- vapply(2:4, function(order) mean(centered ^ order), numeric(1))
    skew <- sqrt(n * (n - 1)) / (n - 2) * moments[2] / moments[1] ^ 1.5
    kurt <- (n - 1) / ((n - 2) * (n - 3)) * ((n + 1) * (moments[3] / moments[1] ^ 2 - 3) + 6)
    sw <- if (length(x) >= 3 && length(x) <= 5000 && stats::sd(x) > 0) stats::shapiro.test(x) else list(statistic = NA_real_, p.value = NA_real_)
    check_row(row, list(n = n, total_n = nrow(d), missing_n = nrow(d) - n, missing_pct = (nrow(d) - n) / nrow(d) * 100,
      skewness = skew, kurtosis = kurt, shapiro_w = unname(sw$statistic), shapiro_p = sw$p.value), "correlation diagnostic")
  }
}
for (method in c("pearson", "spearman", "kendall")) for (alternative in c("two.sided", "greater", "less")) test("correlations", paste("correlation_all_values", method, alternative, sep = "_"), {
  context <- new_case(paste("corr", method, alternative, sep = "-"), sample_data)
  bundle <- new_run(context, "correlations", c("--vars", "a,b,c", "--method", method, "--alternative", alternative, "--conf-level", ".9"))
  check_correlations(bundle, sample_data, method = method, alternative = alternative)
  if (method == "spearman") check(all(!bundle$result$results$summary_df$exact) && all(grepl("Edgeworth", bundle$result$results$summary_df$inference)), "AS89 approximation mislabeled as an exact Spearman test")
  if (alternative == "two.sided") replay(context, bundle)
})
test("correlations", "correlation_complete_case_cross_sets_and_duplicate_pair_removal", {
  context <- new_case("corr-complete-cross", sample_data)
  bundle <- new_run(context, "correlations", c("--x", "a,b", "--y", "b,c", "--missing", "complete", "--conf-level", ".9"))
  check(nrow(bundle$result$results$summary_df) == 3L, "Cross-set self/duplicate pairs are not removed")
  check_correlations(bundle, sample_data, missing = "complete")
  replay(context, bundle)
})
test("correlations", "configured_controls_are_effective_and_frozen_in_replay", {
  context <- new_case("corr-configured-controls", sample_data)
  yaml::write_yaml(list(modules = list(correlations = list(controls = "z", conf_level = .9))), private_config)
  bundle <- new_run(context, "correlations", c("--vars", "a,b,c"))
  check_correlations(bundle, sample_data, controls = "z")
  check(all(bundle$result$results$summary_df$partial), "Configured controls silently ignored")
  yaml::write_yaml(list(modules = list(correlations = list(controls = ""))), private_config)
  replay(context, bundle)
})
test("correlations", "matrix_template_contains_rounded_r_and_adjusted_p", {
  context <- new_case("corr-matrix-layout", sample_data)
  bundle <- new_run(context, "correlations", c("--vars", "a,b,c", "--p-adjust", "holm", "--template", "matrix", "--conf-level", ".9", "--digits", "4"))
  check_correlations(bundle, sample_data, adjust = "holm")
  table <- markdown_table(bundle$markdown, "a")
  check(nrow(table) == 3L && ncol(table) == 4L, "Matrix template did not render a full three-variable matrix")
  rows <- bundle$result$results$summary_df
  for (i in seq_len(nrow(rows))) {
    row <- rows[i, ]; low <- match(row$var2, c("a", "b", "c")); high <- match(row$var1, c("a", "b", "c"))
    coefficient <- gsub("[*]", "", table[low, high + 1L])
    near(as.numeric(coefficient), round(row$r, 4), "Rounded matrix coefficient")
    expected_p <- if (row$p_adjusted < .001) "< .001" else sub("^0", "", formatC(row$p_adjusted, format = "f", digits = 3))
    check(gsub("[[:space:]]", "", table[high, low + 1L]) == gsub("[[:space:]]", "", expected_p), "Matrix upper triangle does not show adjusted p")
  }
  replay(context, bundle)
})
for (method in c("pearson", "spearman")) for (alternative in c("two.sided", "greater", "less")) test("correlations", paste("partial_effective_rank", method, alternative, sep = "_"), {
  data <- sample_data; data$zcopy <- 2 * data$z; data$constant_control <- 4
  context <- new_case(paste("partial", method, alternative, sep = "-"), data)
  bundle <- new_run(context, "correlations", c("--vars", "a,b,c", "--controls", "z,zcopy,constant_control", "--method", method, "--alternative", alternative, "--conf-level", ".9", "--r0", ".15"))
  check_correlations(bundle, data, controls = c("z", "zcopy", "constant_control"), method = method, alternative = alternative, r0 = .15)
  check(length(bundle$result$warnings) > 0L, "Redundant control columns have no warning")
  if (alternative == "two.sided") replay(context, bundle)
})
for (adjust in stats::p.adjust.methods) test("correlations", paste0("correlation_grouped_p_adjust_", adjust), {
  data <- sample_data; data$group <- rep(c("NA", NA_character_, "other"), length.out = nrow(data))
  context <- new_case(paste0("corr-adjust-", adjust), data)
  bundle <- new_run(context, "correlations", c("--vars", "a,b,c", "--group", "group", "--p-adjust", adjust, "--conf-level", ".9"))
  check(nrow(bundle$result$results$summary_df) == 9L, "Missing and literal NA groups were merged")
  check_correlations(bundle, data, adjust = adjust, grouped = TRUE)
})
for (method in c("pearson", "spearman")) for (alternative in c("two.sided", "greater", "less")) test("correlations", paste("correlation_group_Fisher", method, alternative, sep = "_"), {
  context <- new_case(paste("corr-compare", method, alternative, sep = "-"), sample_data)
  bundle <- new_run(context, "correlations", c("--vars", "a,b,c", "--group", "group", "--compare-groups", "TRUE", "--method", method, "--alternative", alternative, "--conf-level", ".9"))
  comparisons <- bundle$result$results$comparison_df
  for (i in seq_len(nrow(comparisons))) {
    row <- comparisons[i, ]; coefficients <- counts <- numeric(2)
    for (g in 1:2) {
      d <- sample_data[!is.na(sample_data$group) & sample_data$group == row[[paste0("group", g)]], c(row$var1, row$var2)]
      d <- d[stats::complete.cases(d), , drop = FALSE]
      coefficients[g] <- stats::cor(d[[1]], d[[2]], method = method); counts[g] <- nrow(d)
    }
    z <- diff(rev(atanh(coefficients))) / sqrt(sum(1 / (counts - 3)))
    check_row(row, list(r1 = coefficients[1], r2 = coefficients[2], n1 = counts[1], n2 = counts[2], z = z, p_value = tail_probability(z, alternative)), "Fisher group comparison")
  }
})
for (method in c("pearson", "spearman", "kendall")) for (alternative in c("two.sided", "greater", "less")) test("correlations", paste("correlation_bootstrap_numeric", method, alternative, sep = "_"), {
  context <- new_case(paste("corr-boot", method, alternative, sep = "-"), sample_data)
  bundle <- new_run(context, "correlations", c("--vars", "a,b,c", "--method", method, "--alternative", alternative, "--conf-level", ".9", "--bootstrap", "TRUE", "--bootstrap-samples", "79", "--seed", "341"))
  check_correlations(bundle, sample_data, method = method, alternative = alternative, bootstrap = TRUE, seed = 341L)
  if (alternative == "two.sided") replay(context, bundle)
})
test("correlations", "partial_rank_bootstrap_and_complete_case_selection", {
  context <- new_case("partial-boot", sample_data)
  bundle <- new_run(context, "correlations", c("--vars", "a,b,c", "--controls", "z", "--method", "spearman", "--missing", "complete", "--conf-level", ".9", "--bootstrap", "TRUE", "--bootstrap-samples", "79", "--seed", "341"))
  check_correlations(bundle, sample_data, controls = "z", method = "spearman", missing = "complete", bootstrap = TRUE, seed = 341L)
  replay(context, bundle)
})
test("correlations", "rank_tests_ties_and_small_sample_exact_policy", {
  for (ties in c(FALSE, TRUE)) for (method in c("spearman", "kendall")) {
    data <- data.frame(a = 1:8, b = c(8, 3, 4, 1, 7, 2, 6, 5))
    if (ties) data$b[2] <- data$b[3]
    context <- new_case(paste("rank-small", method, ties, sep = "-"), data)
    bundle <- new_run(context, "correlations", c("--vars", "a,b", "--method", method, "--conf-level", ".9"))
    check_correlations(bundle, data, variables = c("a", "b"), method = method)
    check(identical(bundle$result$results$summary_df$exact, !ties), "Small rank-test exact/tie metadata differs from actual policy")
  }
})
test("correlations", "correlation_SPSS_labels_user_missing", {
  data <- sample_data; data$a[2] <- 99
  data$a <- haven::labelled_spss(data$a, labels = c("Nicht beantwortet" = 99), na_values = 99, label = "Belastung")
  data$b <- haven::labelled(data$b, label = "Lebensqualitaet")
  codes <- rep(c(2, 1, 9), length.out = nrow(data))
  data$group <- haven::labelled_spss(codes, labels = c("Kontrolle" = 1, "Intervention" = 2, "Fehlend" = 9), na_values = 9, label = "Bedingung")
  context <- new_case("corr-spss", data, "sav")
  bundle <- new_run(context, "correlations", c("--vars", "a,b,c", "--group", "group", "--conf-level", ".9"))
  clean <- sample_data; clean$group <- replace(codes, codes == 9, NA_real_)
  check_correlations(bundle, clean, grouped = TRUE)
  check(all(vapply(c("Belastung", "Lebensqualitaet", "Intervention", "Kontrolle"), function(label) grepl(label, bundle$markdown, fixed = TRUE), logical(1))), "SPSS correlation variable/value labels missing")
})
test("correlations", "correlation_explicit_factor_text_coercion", {
  data <- sample_data; data$a <- factor(as.character(data$a)); data$b <- as.character(data$b); data$b[10] <- "not numeric"
  context <- new_case("corr-coerce", data)
  bundle <- new_run(context, "correlations", c("--vars", "a,b,c", "--coerce", "TRUE", "--conf-level", ".9"))
  clean <- sample_data; clean$b[10] <- NA
  check_correlations(bundle, clean)
  check(length(bundle$result$warnings) > 0L, "Introduced coercion missing has no warning")
  request <- jsonlite::fromJSON(bundle$request_path, simplifyVector = FALSE)
  check(identical(as.integer(unlist(request$design$coercion$b$introduced_missing_rows)), 10L), "Coercion loss source row differs")
})

for (module in c("t_test", "correlations")) {
  options <- if (module == "t_test") c("--vars", "a,b", "--mu", "2.5") else c("--vars", "a,b,c")
  test(module, paste0(module, "_default_seed_privacy_frozen_inputs_templates_config_replay"), {
    context <- new_case(paste0(module, "-privacy"), sample_data)
    yaml::write_yaml(list(defaults = list(digits = 5L), logging = list(include_user_prompt = FALSE, include_outputs = FALSE)), private_config)
    template <- file.path(context$base, "custom.md")
    writeLines(c(paste0("# ", cfg$template_marker), "{{table_body}}", "{{narrative}}"), template)
    bundle <- new_run(context, module, c(options, "--template", template, "--log", "FALSE", "--user-prompt", "PRIVATE_INFERENCE_PROMPT", "--conf-level", ".9", "--bootstrap", "TRUE", "--bootstrap-samples", "79"))
    check(grepl(cfg$template_marker, bundle$markdown, fixed = TRUE), "Custom template not used")
    check(!grepl("PRIVATE_INFERENCE_PROMPT", text_file(bundle$request_path), fixed = TRUE), "Prompt privacy ignored")
    check(!grepl(context$base, text_file(bundle$request_path), fixed = TRUE), "External fixture path leaked")
    check(length(bundle$result$results) > 0L, "Legacy output optout removed mandatory results")
    if (module == "t_test") check_ttest(bundle, sample_data, mu = 2.5, bootstrap = TRUE) else check_correlations(bundle, sample_data, bootstrap = TRUE)
    log <- file.path(context$dataset, "analysis_log.jsonl")
    if (file.exists(log)) check(!grepl(paste0('"module":"', module, '"'), gsub(" ", "", text_file(log)), fixed = TRUE), "Legacy log optout ignored")
    writeLines("CHANGED ORIGINAL TEMPLATE", template)
    working <- file.path(context$dataset, "sample.parquet"); current <- arrow::read_parquet(working, as_data_frame = TRUE)
    current$a[1] <- 123456; arrow::write_parquet(current, working)
    source_data <- sample_data; source_data$a[1] <- -999; saveRDS(source_data, context$input)
    yaml::write_yaml(list(defaults = list(digits = 1L), modules = setNames(list(list(seed = 777L)), module)), private_config)
    replay(context, bundle)
    near(arrow::read_parquet(working, as_data_frame = TRUE)$a[1], 123456, "Replay preserved current working input")
  })
  test(module, paste0(module, "_invalid_option_domains_duplicates_and_incompatible_roles"), {
    context <- new_case(paste0(module, "-invalid"), sample_data)
    invalid <- list(c("--digits", "1.5"), c("--conf-level", "1"), c("--conf-level", "NaN"), c("--seed", "-1"),
      c("--bootstrap-samples", "1.5"), c("--bootstrap-samples", "0"), c("--alternative", "unknown"), c("--vars", "a,a,b"))
    if (module == "t_test") invalid <- c(invalid, list(c("--group", "group", "--mu", "2"), c("--x", "a", "--y", "b", "--mu", "2"), c("--x", "a,b", "--y", "c")))
    else invalid <- c(invalid, list(c("--method", "unknown"), c("--missing", "unknown"), c("--p-adjust", "unknown"),
      c("--r0", "1"), c("--controls", "z,z"), c("--vars", "a,b", "--controls", "a"), c("--method", "kendall", "--controls", "z"), c("--method", "kendall", "--r0", "0"), c("--vars", "a,b", "--x", "a", "--y", "c")))
    for (invalid_options in invalid) run_module(module, c(context$flag, context$input, invalid_options), failure = TRUE)
    check(!any(vapply(runs(context), function(path) identical(read_json(file.path(path, "result.json"))$status, "completed"), logical(1))), "Invalid options published completed output")
  })
  test(module, paste0(module, "_nonestimable_failure_prior_output_preserved_and_not_replayable"), {
    data <- sample_data; data$constant <- 4
    context <- new_case(paste0(module, "-failure"), data)
    first <- new_run(context, module, options)
    canonical <- file.path(context$dataset, "report_canonical.md"); before <- sha(canonical)
    failed <- new_run(context, module, c("--vars", if (module == "t_test") "a,constant" else "a,constant"), failure = TRUE)
    check(identical(before, sha(canonical)), "Failed analysis changed prior canonical output")
    run_module("replay_run", c("--request", failed$request_path), failure = TRUE)
    check(!dir.exists(file.path(context$dataset, ".analysis-lock")), "Failed run left dataset lock")
    check(!dir.exists(file.path(context$project, ".publication-lock")), "Failed run left publication lock")
  })
}
test("t_test", "ttest_expect_two_groups_is_failed_not_completed", {
  data <- sample_data; data$group <- rep(c("a", "b", "c"), length.out = nrow(data))
  context <- new_case("ttest-expected-invalid", data)
  failed <- new_run(context, "t_test", c("--vars", "a", "--group", "group", "--expect-two-groups", "TRUE"), failure = TRUE, expected_invalid = TRUE)
  run_module("replay_run", c("--request", failed$request_path), failure = TRUE)
})
test("correlations", "perfect_correlations_and_short_samples_have_honest_intervals", {
  for (n in c(3L, 8L)) {
    data <- data.frame(a = seq_len(n), b = -seq_len(n))
    context <- new_case(paste0("corr-perfect-", n), data)
    bundle <- new_run(context, "correlations", c("--vars", "a,b", "--conf-level", ".9"))
    reference <- stats::cor.test(data$a, data$b, conf.level = .9)
    check_row(bundle$result$results$summary_df, list(r = -1, p_value = reference$p.value, n = n), "perfect correlation")
    expected_ci <- if (n == 3) c(NA_real_, NA_real_) else reference$conf.int
    check_row(bundle$result$results$summary_df, list(ci_low = expected_ci[1], ci_high = expected_ci[2]), "perfect CI")
  }
})
test("correlations", "one_unavailable_pair_is_explicit_and_does_not_hide_valid_pair", {
  data <- sample_data; data$constant <- 4
  context <- new_case("corr-partial-availability", data)
  bundle <- new_run(context, "correlations", c("--vars", "a,b,c,constant", "--p-adjust", "bonferroni"))
  rows <- bundle$result$results$summary_df
  check(sum(is.finite(rows$r)) == 3L && sum(is.na(rows$r)) == 3L, "Unavailable correlations are fabricated or valid pair dropped")
  check(all(rows$estimate_status[is.na(rows$r)] != "ok"), "Unavailable pair lacks explicit estimate status")
  check(length(bundle$result$warnings) > 0L, "Unavailable pair has no warning")
  near_probability(rows$p_adjusted, stats::p.adjust(rows$p_value, method = "bonferroni"), "Bonferroni uses estimable requested tests")
})
test("correlations", "partial_group_Fisher_uses_each_groups_effective_rank", {
  data <- sample_data; data$z[which(data$group == "B")] <- 4
  context <- new_case("partial-group-rank", data)
  bundle <- new_run(context, "correlations", c("--vars", "a,b", "--controls", "z", "--group", "group", "--compare-groups", "TRUE", "--conf-level", ".9"))
  row <- bundle$result$results$comparison_df[1, ]; references <- list()
  for (g in 1:2) {
    selected <- !is.na(data$group) & data$group == row[[paste0("group", g)]] & stats::complete.cases(data[c("a", "b", "z")])
    d <- data[selected, ]; ref <- partial_reference(d, "a", "b", "z", "pearson", "two.sided")
    references[[g]] <- c(ref, list(n = nrow(d)))
  }
  check(references[[1]]$control_rank != references[[2]]$control_rank, "Fixture does not have group-specific control ranks")
  z <- (atanh(references[[1]]$r) - atanh(references[[2]]$r)) / sqrt(sum(vapply(references, function(ref) 1 / (ref$n - ref$control_rank - 3), numeric(1))))
  check_row(row, list(r1 = references[[1]]$r, r2 = references[[2]]$r, n1 = references[[1]]$n, n2 = references[[2]]$n,
    z = z, p_value = 2 * stats::pnorm(-abs(z))), "Group-specific partial Fisher rank")
})
for (group_type in c("numeric", "adjacent_double", "fractional_timestamp")) test("correlations", paste0("correlation_stable_group_identity_", group_type), {
  data <- sample_data
  values <- switch(group_type, numeric = c(1, 1 + 1e-15), adjacent_double = c(1, 1 + .Machine$double.eps), fractional_timestamp = as.POSIXct("2025-01-01", tz = "UTC") + c(.0000002, .0000004))
  check(length(unique(values)) == 2L && length(unique(as.character(values))) == 1L, "Fixture does not collide only in display formatting")
  data$group <- rep(values, each = 32); data$b[33:64] <- -data$b[33:64]
  context <- new_case(paste0("corr-colliding-group-", group_type), data)
  bundle <- new_run(context, "correlations", c("--vars", "a,b,c", "--group", "group", "--compare-groups", "TRUE", "--p-adjust", "bonferroni", "--template", "matrix", "--conf-level", ".9", "--digits", "4"))
  check_correlations(bundle, data, grouped = TRUE, adjust = "bonferroni")
  saved_groups <- jsonlite::fromJSON(bundle$request_path, simplifyVector = FALSE)$design$groups
  check(identical(vapply(saved_groups, function(group) group$value_hex, character(1)), sprintf("%a", as.numeric(values))), "JSON design lost exact group numeric/time value identity")
  rows <- bundle$result$results$summary_df; comparisons <- bundle$result$results$comparison_df
  check(length(unique(rows$group)) == 2L && length(unique(rows$group_label)) == 2L, "Distinct groups still share indistinguishable display labels")
  for (i in seq_len(nrow(comparisons))) {
    row <- comparisons[i, ]; check(identical(as.integer(c(row$group1_id, row$group2_id)), 1:2), "Comparison lost stable group IDs")
    estimates <- sizes <- numeric(2)
    for (g in 1:2) {
      d <- data[data$group == values[g], c(row$var1, row$var2)]; d <- d[stats::complete.cases(d), , drop = FALSE]
      estimates[g] <- stats::cor(d[[1]], d[[2]]); sizes[g] <- nrow(d)
    }
    z <- (atanh(estimates[1]) - atanh(estimates[2])) / sqrt(sum(1 / (sizes - 3)))
    check_row(row, list(r1 = estimates[1], r2 = estimates[2], n1 = sizes[1], n2 = sizes[2], z = z, p_value = 2 * stats::pnorm(-abs(z))), "Collision-free Fisher comparison")
  }
  for (g in 1:2) {
    table <- markdown_table(bundle$markdown, "a", occurrence = g)
    group_rows <- rows[rows$group_id == g, , drop = FALSE]
    for (i in seq_len(nrow(group_rows))) {
      row <- group_rows[i, ]; low <- match(row$var2, c("a", "b", "c")); high <- match(row$var1, c("a", "b", "c"))
      near(as.numeric(gsub("[*]", "", table[low, high + 1L])), round(row$r, 4), "Collision-free grouped matrix coefficient")
      expected_p <- if (row$p_adjusted < .001) "<.001" else sub("^0", "", formatC(row$p_adjusted, format = "f", digits = 3))
      check(gsub("[[:space:]]", "", table[high, low + 1L]) == expected_p, "Colliding groups were mixed in matrix adjusted p")
    }
  }
  replay(context, bundle)
})
test("t_test", "ttest_stable_raw_group_identity_and_disambiguated_displays", {
  for (group_type in c("numeric", "fractional_timestamp")) {
    data <- sample_data
    values <- if (group_type == "numeric") c(1, 1 + 1e-15) else as.POSIXct("2025-01-01", tz = "UTC") + c(.0000002, .0000004)
    check(length(unique(values)) == 2L && length(unique(as.character(values))) == 1L, "Fixture lacks a display-only group collision")
    data$group <- rep(values, each = 32)
    context <- new_case(paste0("ttest-colliding-group-", group_type), data)
    bundle <- new_run(context, "t_test", c("--vars", "a,b", "--group", "group", "--conf-level", ".9", "--digits", "4"))
    check_ttest(bundle, data)
    rows <- bundle$result$results$summary_df
    check(all(rows$group_1_id == 1L & rows$group_2_id == 2L), "T-test stable group IDs differ")
    table <- markdown_table(bundle$markdown, "Variable")
    check(all(table$`Group 1` != table$`Group 2`), "Distinct t-test groups have identical displayed labels")
    replay(context, bundle)
  }
})
test("t_test", "ttest_bootstrap_degenerate_resamples_are_counted", {
  data <- data.frame(a = c(1, 4)); context <- new_case("ttest-small-bootstrap", data)
  bundle <- new_run(context, "t_test", c("--vars", "a", "--mu", "2.5", "--bootstrap", "TRUE", "--bootstrap-samples", "79", "--seed", "341", "--conf-level", ".9"))
  set.seed(341)
  means <- replicate(79, mean(data$a[sample.int(2, 2, replace = TRUE)]) - 2.5)
  effects <- replicate(79, {
    draw <- data$a[sample.int(2, 2, replace = TRUE)]; (mean(draw) - 2.5) / stats::sd(draw)
  })
  valid <- sum(is.finite(effects)); check(valid < 79 && valid > 1, "Fixture has no degenerate bootstrap effects")
  mean_ci <- percentile(means, "two.sided"); d_ci <- percentile(effects, "two.sided")
  check_row(bundle$result$results$summary_df, list(boot_valid = 79, boot_discarded = 0, boot_d_valid = valid, boot_d_discarded = 79 - valid,
    boot_ci_low = mean_ci[1], boot_ci_high = mean_ci[2], boot_d_ci_low = d_ci[1], boot_d_ci_high = d_ci[2]), "Degenerate t-test resamples")
  check(length(bundle$result$warnings) > 0L, "Discarded t-test bootstrap draws have no warning")
  replay(context, bundle)
})
test("correlations", "correlation_bootstrap_degenerate_resamples_are_counted", {
  data <- data.frame(a = 1:3, b = c(3, 1, 2)); context <- new_case("corr-small-bootstrap", data)
  bundle <- new_run(context, "correlations", c("--vars", "a,b", "--bootstrap", "TRUE", "--bootstrap-samples", "79", "--seed", "341", "--conf-level", ".9"))
  set.seed(341)
  values <- suppressWarnings(replicate(79, {
    rows <- sample.int(3, 3, replace = TRUE); stats::cor(data$a[rows], data$b[rows])
  }))
  valid <- sum(is.finite(values)); check(valid < 79 && valid > 1, "Fixture has no degenerate bootstrap correlations")
  interval <- percentile(values, "two.sided")
  check_row(bundle$result$results$summary_df, list(bootstrap_valid = valid, bootstrap_failed = 79 - valid, boot_ci_low = interval[1], boot_ci_high = interval[2]), "Degenerate correlation resamples")
  check(length(bundle$result$warnings) > 0L, "Discarded correlation draws have no warning")
  replay(context, bundle)
})
for (module in c("t_test", "correlations")) test(module, paste0(module, "_nonfinite_selected_values_fail"), {
  data <- sample_data; data$a[1] <- Inf
  context <- new_case(paste0(module, "-nonfinite"), data)
  run_module(module, c(context$flag, context$input, "--vars", "a,b"), failure = TRUE)
  check(!any(vapply(runs(context), function(path) identical(read_json(file.path(path, "result.json"))$status, "completed"), logical(1))), "Non-finite values were silently dropped or published")
})

summary_path <- file.path(work, "results.json")
jsonlite::write_json(list(schema_version = 1L, suite = "phase2-inference", modules = modules, test_pattern = test_pattern, numeric_checks = numeric_checks, tests = results,
  environment = list(r = R.version.string, packages = as.list(vapply(required, function(p) as.character(utils::packageVersion(p)), character(1))))), summary_path, auto_unbox = TRUE, pretty = TRUE, digits = NA)
passed <- sum(vapply(results, function(result) result$passed, logical(1)))
cat(sprintf("Phase 2 inference: %d/%d cases passed; %d numeric comparisons. Results: %s\n", passed, length(results), numeric_checks, summary_path))
if (!nzchar(forced_root) && keep > 0L && dir.exists(output_base)) {
  candidates <- list.dirs(output_base, full.names = TRUE, recursive = FALSE)
  candidates <- sort(candidates[grepl("^[0-9]{14}$", basename(candidates))], decreasing = TRUE)
  if (length(candidates) > keep) for (path in setdiff(candidates[seq.int(keep + 1L, length(candidates))], run_root)) {
    if (identical(dirname(path), output_base) && grepl("^[0-9]{14}$", basename(path))) unlink(path, recursive = TRUE)
  }
}
quit(status = if (length(results) > 0L && passed == length(results)) 0 else 1)
