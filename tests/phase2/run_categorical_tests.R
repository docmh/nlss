#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Public-CLI acceptance. Numeric oracles use base R/stats, never NLSS helpers.
args <- commandArgs(TRUE)
script <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])
repo <- normalizePath(file.path(dirname(script[1]), "../.."), winslash = "/")
if ("--help" %in% args) {
  cat("Usage: Rscript tests/phase2/run_categorical_tests.R [--root PATH] [--keep N] [--modules frequencies,crosstabs,data_explorer]\n",
      "Honors NLSS_TEST_ROOT, NLSS_KEEP_RUNS and NLSS_TESTS_CONFIG; uses private configuration and no network.\n", sep = "")
  quit(status = 0)
}
if (length(args) %% 2L || (length(args) && any(!args[seq(1L, length(args), 2L)] %in% c("--root", "--keep", "--modules")))) stop("Unknown/incomplete runner option")
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
modules <- strsplit(arg("--modules", "frequencies,crosstabs,data_explorer"), ",", fixed = TRUE)[[1]]
if (!length(modules) || anyDuplicated(modules) || any(!modules %in% c("frequencies", "crosstabs", "data_explorer"))) stop("Unsupported --modules selection")
forced_root <- arg("--root", Sys.getenv("NLSS_TEST_ROOT", ""))
output_base <- absolute(cfg$output_dir)
run_root <- if (nzchar(forced_root)) absolute(forced_root) else file.path(output_base, format(Sys.time(), "%Y%m%d%H%M%S"))
keep <- suppressWarnings(as.integer(arg("--keep", Sys.getenv("NLSS_KEEP_RUNS", as.character(cfg$keep_runs_default)))))
if (length(keep) != 1L || is.na(keep) || keep < 0L) stop("Invalid --keep")
work <- file.path(run_root, "phase2-categorical", paste0("run-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid()))
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
  check(identical(is.na(actual), is.na(expected)), paste(label, "missing values differ"))
  keep <- !is.na(expected)
  check(all(is.finite(actual[keep])) && all(abs(actual[keep] - expected[keep]) <= tolerance), paste(label, "differs from independent reference"))
  numeric_checks <<- numeric_checks + length(expected)
}
results <- list()
test <- function(module, name, code) {
  if (!module %in% modules) return(invisible(NULL))
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
new_run <- function(context, module, options, failure = FALSE, source = TRUE) {
  before <- runs(context)
  command <- run_module(module, c(if (source) c(context$flag, context$input), options), failure)
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
markdown_table <- function(markdown, key) {
  lines <- strsplit(markdown, "\n", fixed = TRUE)[[1]]
  cells <- function(line) trimws(strsplit(sub("[|][[:space:]]*$", "", sub("^[[:space:]]*[|]", "", line)), "|", fixed = TRUE)[[1]])
  positions <- which(vapply(lines, function(line) grepl("^[[:space:]]*[|]", line) && key %in% cells(line), logical(1)))
  check(length(positions) > 0L, paste("No Markdown table header", key))
  start <- positions[1]; i <- start + 2L; rows <- list()
  while (i <= length(lines) && grepl("^[[:space:]]*[|]", lines[i])) { rows[[length(rows) + 1L]] <- cells(lines[i]); i <- i + 1L }
  check(length(rows) > 0L, "Empty Markdown table")
  out <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE); names(out) <- cells(lines[start]); out
}
numeric_cells <- function(x) suppressWarnings(as.numeric(gsub("%", "", x, fixed = TRUE)))
freq_reference <- function(x) {
  valid <- x[!is.na(x)]
  levels <- if (is.factor(x)) levels(x) else as.character(sort(unique(valid)))
  if (!length(valid) || !length(levels)) return(data.frame(level = "(no valid data)", n = 0, pct_total = 0, pct_valid = NA_real_, total_n = length(x), missing_n = sum(is.na(x)), missing_pct = mean(is.na(x)) * 100))
  counts <- as.numeric(table(factor(x, levels = levels)))
  data.frame(level = levels, n = counts, pct_total = counts / length(x) * 100,
    pct_valid = counts / length(valid) * 100, total_n = length(x), missing_n = sum(is.na(x)), missing_pct = mean(is.na(x)) * 100)
}
check_frequencies <- function(actual, data, vars, group = NULL) {
  groups <- if (is.null(group)) "" else unique(data[[group]])
  for (g in groups) for (variable in vars) {
    selected <- if (is.null(group)) rep(TRUE, nrow(data)) else if (is.na(g)) is.na(data[[group]]) else !is.na(data[[group]]) & data[[group]] == g
    rows <- actual[actual$variable == variable, , drop = FALSE]
    if (!is.null(group)) {
      check("group_missing" %in% names(rows), "Grouped frequencies do not disambiguate missing groups")
      rows <- rows[if (is.na(g)) rows$group_missing else !rows$group_missing & rows$group == as.character(g), , drop = FALSE]
    }
    wanted <- freq_reference(data[[variable]][selected])
    check(nrow(rows) == nrow(wanted), paste("Frequency row count differs", variable, g))
    rows <- rows[match(wanted$level, rows$level), , drop = FALSE]
    for (field in setdiff(names(wanted), "level")) near(rows[[field]], wanted[[field]], paste("frequency", variable, g, field))
  }
}
asymmetric <- data.frame(category = factor(c("A", "B", "A", "C", NA, "A", "B", "A", "C", "A", "A", NA), levels = c("C", "A", "B", "unused")),
  number = c(1, 1, 2, 1, 3, NA, 2, 1, 2, 1, 1, 3), empty = rep(NA_real_, 12),
  group = c("NA", "NA", "NA", "NA", "NA", NA, NA, NA, "(Missing group)", "(Missing group)", "(Missing group)", "(Missing group)"))
for (grouped in c(FALSE, TRUE)) test("frequencies", paste0("frequency_asymmetric_factor_missing_", grouped), {
  context <- new_case(paste0("frequency-asymmetric-", grouped), asymmetric)
  bundle <- new_run(context, "frequencies", c("--vars", "category,number,empty", if (grouped) c("--group", "group"), "--digits", "3"))
  check_frequencies(bundle$result$results$summary_df, asymmetric, c("category", "number", "empty"), if (grouped) "group" else NULL)
  if (grouped) {
    groups <- bundle$request$design$groups
    for (i in seq_len(nrow(groups))) {
      wanted <- which(if (groups$is_missing[i]) is.na(asymmetric$group) else !is.na(asymmetric$group) & asymmetric$group == groups$value[i])
      check(identical(as.integer(groups$row_indices[[i]]), wanted), "Frequency request records incorrect source row identities")
    }
  }
  table <- markdown_table(bundle$markdown, "Level")
  rows <- table[table$Variable == "category" & table$Level != "Missing", , drop = FALSE]
  actual <- bundle$result$results$summary_df
  expected <- actual[actual$variable == "category", , drop = FALSE]
  check(nrow(rows) == nrow(expected), "Markdown category rows were merged or lost")
  near(numeric_cells(rows$n), expected$n, "frequency Markdown count")
  near(numeric_cells(rows$`%`), round(expected$pct_total, 3), "frequency Markdown percent")
  near(numeric_cells(rows$`Valid %`), round(expected$pct_valid, 3), "frequency Markdown valid percent")
  if (grouped) check(length(unique(rows$Group)) == 3L, "Literal NA/missing/literal display marker are not visually distinct")
  replay(context, bundle)
})
for (include in c(FALSE, TRUE)) test("frequencies", paste0("frequency_implicit_selection_numeric_", include), {
  context <- new_case(paste0("frequency-select-", include), asymmetric)
  bundle <- new_run(context, "frequencies", c("--include-numeric", as.character(include)))
  wanted <- if (include) names(asymmetric) else c("category", "group")
  check(setequal(bundle$request$options$vars, wanted), "Implicit frequency variable selection differs")
  check_frequencies(bundle$result$results$summary_df, asymmetric, wanted)
})
for (grouped in c(FALSE, TRUE)) test("frequencies", paste0("frequency_standard_dataset_golden_", grouped), {
  data <- read.csv(absolute(cfg$golden_dataset), stringsAsFactors = FALSE)
  context <- new_case(paste0("frequency-golden-", grouped), data, "csv")
  new_run(context, "frequencies", c("--vars", "cat_var,cat_var2,gender,ordinal_var,all_missing_var", if (grouped) c("--group", "group3")))
  python <- Sys.getenv("PYTHON_BIN", "")
  if (!nzchar(python)) python <- if (nzchar(Sys.which("python3"))) Sys.which("python3") else Sys.which("python")
  check(nzchar(python), "Python is required for independent frequency golden checker")
  log <- file.path(context$base, "golden-check.log")
  status <- system2(python, shQuote(c(file.path(repo, "tests/values/check_frequencies_golden.py"), file.path(context$dataset, "analysis_log.jsonl"),
    file.path(repo, "tests/values/frequencies_golden.csv"), if (grouped) "grouped" else "ungrouped")), stdout = log, stderr = log)
  check(status == 0L, text_file(log))
  cat(text_file(log), "\n")
})
labelled <- data.frame(response = haven::labelled_spss(c(1, 1, 1, 2, 2, 9, 1, 2, 1, 1, 2, 9),
  labels = c("Ablehnung" = 1, "Zustimmung" = 2, "Keine Angabe" = 9), na_values = 9, label = "Antwort"),
  condition = haven::labelled(rep(c(1, 2), 6), labels = c("Kontrolle" = 1, "Intervention" = 2), label = "Bedingung"))
for (module in c("frequencies", "crosstabs", "data_explorer")) test(module, paste0(module, "_SPSS_labels_missing_codes_and_replay"), {
  context <- new_case(paste0(module, "-labels"), labelled, "sav")
  options <- switch(module, frequencies = c("--vars", "response", "--group", "condition"), crosstabs = c("--row", "response", "--col", "condition", "--fisher", "TRUE"), data_explorer = c("--vars", "response,condition"))
  bundle <- new_run(context, module, options)
  check(grepl("Antwort", bundle$markdown, fixed = TRUE) && grepl("Ablehnung", bundle$markdown, fixed = TRUE) && grepl("Zustimmung", bundle$markdown, fixed = TRUE), "SPSS labels missing from Markdown")
  clean <- data.frame(response = c(1, 1, 1, 2, 2, NA, 1, 2, 1, 1, 2, NA), condition = rep(c(1, 2), 6))
  if (module == "frequencies") check_frequencies(bundle$result$results$summary_df, clean, "response", "condition")
  if (module == "crosstabs") near(bundle$result$results$tests_df$valid_n, 10, "SPSS crosstabs valid N")
  if (module == "data_explorer") near(bundle$result$results$overview_df$missing_n, c(2, 0), "SPSS explorer missing N")
  replay(context, bundle)
})
expand_table <- function(tab) {
  rows <- as.data.frame(as.table(tab), stringsAsFactors = FALSE)
  names(rows) <- c("row", "col", "n")
  rows[rep(seq_len(nrow(rows)), rows$n), c("row", "col"), drop = FALSE]
}
two <- matrix(c(9, 4, 3, 11), 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
rxc <- matrix(c(8, 2, 4, 3, 11, 6, 5, 7, 9, 4, 2, 8), 3, dimnames = list(c("a", "b", "c"), c("w", "x", "y", "z")))
check_crosstabs <- function(actual, data, rows = "row", cols = "col", group = NULL, percent = "all", expected = TRUE, residuals = TRUE, yates = FALSE, fisher = FALSE, simulate = FALSE, seed = 1L, B = 250L, confidence = .95) {
  groups <- if (is.null(group)) "" else unique(data[[group]])
  if (simulate) set.seed(seed)
  for (g in groups) for (row in rows) for (col in cols) {
    selected <- if (is.null(group)) rep(TRUE, nrow(data)) else if (is.na(g)) is.na(data[[group]]) else !is.na(data[[group]]) & data[[group]] == g
    d <- data[selected, , drop = FALSE]
    valid <- stats::complete.cases(d[c(row, col)])
    tab <- table(d[[row]][valid], d[[col]][valid])
    select_rows <- function(frame) {
      frame <- frame[frame$row_var == row & frame$col_var == col, , drop = FALSE]
      if (!is.null(group)) frame <- frame[if (is.na(g)) frame$group_missing else !frame$group_missing & frame$group == as.character(g), , drop = FALSE]
      frame
    }
    cells <- select_rows(actual$cells_df); tests <- select_rows(actual$tests_df); diagnostics <- select_rows(actual$diagnostics_df)
    check(nrow(tests) == 1L && nrow(diagnostics) == 1L && nrow(cells) == length(tab), "Crosstabs pair/group row counts differ")
    wanted <- as.data.frame(as.table(tab), stringsAsFactors = FALSE)
    cells <- cells[match(paste(wanted[[1]], wanted[[2]], sep = "\r"), paste(cells$row_level, cells$col_level, sep = "\r")), , drop = FALSE]
    near(cells$n, as.vector(tab), "crosstabs observed counts")
    for (kind in c("row", "col", "total")) {
      values <- if (kind == "total") tab / sum(tab) * 100 else prop.table(tab, if (kind == "row") 1 else 2) * 100
      near(cells[[paste0("pct_", kind)]], if (percent %in% c("all", kind)) as.vector(values) else rep(NA_real_, length(tab)), paste("crosstabs", kind, "percent"))
    }
    reference <- suppressWarnings(stats::chisq.test(tab, correct = yates))
    residual_reference <- suppressWarnings(stats::chisq.test(tab, correct = FALSE))
    near(cells$expected, if (expected) as.vector(reference$expected) else rep(NA_real_, length(tab)), "crosstabs expected counts")
    near(cells$std_resid, if (residuals) as.vector(residual_reference$residuals) else rep(NA_real_, length(tab)), "crosstabs Pearson residuals")
    near(cells$adj_resid, if (residuals) as.vector(residual_reference$stdres) else rep(NA_real_, length(tab)), "crosstabs adjusted residuals")
    scalar <- list(valid_n = sum(valid), total_n = nrow(d), missing_n = sum(!valid), missing_pct = mean(!valid) * 100,
      row_missing_n = sum(is.na(d[[row]])), col_missing_n = sum(is.na(d[[col]])), both_missing_n = sum(is.na(d[[row]]) & is.na(d[[col]])),
      chi_square = unname(reference$statistic), chi_df = unname(reference$parameter), chi_p = reference$p.value,
      phi = if (all(dim(tab) == 2L)) sqrt(unname(reference$statistic) / sum(tab)) else NA_real_,
      cramers_v = sqrt(unname(reference$statistic) / (sum(tab) * min(dim(tab) - 1))),
      contingency_c = sqrt(unname(reference$statistic) / (unname(reference$statistic) + sum(tab))))
    for (field in names(scalar)) near(tests[[field]], scalar[[field]], paste("crosstabs", field))
    if (fisher) {
      exact <- stats::fisher.test(tab, simulate.p.value = simulate, B = B, conf.level = confidence)
      near(tests$fisher_p, exact$p.value, "Fisher p")
      if (!is.null(exact$estimate)) {
        near(tests$fisher_odds_ratio, unname(exact$estimate), "Fisher conditional OR")
        near(tests$fisher_ci_low, exact$conf.int[1], "Fisher lower CI")
        near(tests$fisher_ci_high, exact$conf.int[2], "Fisher upper CI")
      }
    }
    values <- list(n_cells = length(tab), min_expected = min(reference$expected), n_expected_lt_1 = sum(reference$expected < 1),
      n_expected_lt_5 = sum(reference$expected < 5), pct_expected_lt_5 = mean(reference$expected < 5) * 100)
    for (field in names(values)) near(diagnostics[[field]], values[[field]], paste("expected-count diagnostic", field))
  }
}
for (yates in c(FALSE, TRUE)) test("crosstabs", paste0("crosstabs_exact_2x2_yates_", yates), {
  context <- new_case(paste0("crosstabs-exact-", yates), expand_table(two))
  bundle <- new_run(context, "crosstabs", c("--row", "row", "--col", "col", "--percent", "all", "--nlss-percent", "all", "--expected", "TRUE", "--residuals", "TRUE", "--yates", as.character(yates), "--fisher", "TRUE", "--fisher-conf-level", "0.9"))
  check_crosstabs(bundle$result$results, context$data, yates = yates, fisher = TRUE, confidence = .9)
  table <- markdown_table(bundle$markdown, "Row Level")
  cells <- bundle$result$results$cells_df
  for (mapping in list(c("n", "n"), c("Row %", "pct_row"), c("Column %", "pct_col"), c("Total %", "pct_total"))) near(numeric_cells(table[[mapping[1]]]), round(cells[[mapping[2]]], 5), paste("crosstabs Markdown", mapping[1]))
  replay(context, bundle)
})
for (percent in c("row", "col", "total", "all")) test("crosstabs", paste0("crosstabs_raw_percent_diagnostic_filters_", percent), {
  context <- new_case(paste0("crosstabs-filter-", percent), expand_table(rxc))
  bundle <- new_run(context, "crosstabs", c("--row", "row", "--col", "col", "--percent", percent, "--nlss-percent", "none", "--expected", "FALSE", "--residuals", "FALSE", "--yates", "FALSE"))
  check_crosstabs(bundle$result$results, context$data, percent = percent, expected = FALSE, residuals = FALSE)
  table <- markdown_table(bundle$markdown, "Row Level")
  check(!any(c("Row %", "Column %", "Total %") %in% names(table)), "--nlss-percent none did not hide percentages")
})
for (percent in c("row", "col", "total")) test("crosstabs", paste0("crosstabs_independent_Markdown_percent_", percent), {
  context <- new_case(paste0("crosstabs-display-", percent), expand_table(rxc))
  bundle <- new_run(context, "crosstabs", c("--row", "row", "--col", "col", "--percent", "all", "--nlss-percent", percent))
  table <- markdown_table(bundle$markdown, "Row Level")
  headers <- c(row = "Row %", col = "Column %", total = "Total %")
  check(identical(intersect(unname(headers), names(table)), unname(headers[percent])), "Markdown percentage selection differs")
  near(numeric_cells(table[[headers[percent]]]), round(bundle$result$results$cells_df[[paste0("pct_", percent)]], 5), "Independent Markdown percentage")
  check(all(vapply(bundle$result$results$cells_df[c("pct_row", "pct_col", "pct_total")], function(x) all(is.finite(x)), logical(1))), "Markdown selection changed raw percent all")
})
for (positive in c(FALSE, TRUE)) test("crosstabs", paste0("crosstabs_exact_Fisher_boundary_", positive), {
  tab <- if (positive) matrix(c(8, 0, 0, 6), 2) else matrix(c(0, 6, 8, 0), 2)
  dimnames(tab) <- list(c("r1", "r2"), c("c1", "c2"))
  context <- new_case(paste0("crosstabs-boundary-", positive), expand_table(tab))
  bundle <- new_run(context, "crosstabs", c("--row", "row", "--col", "col", "--fisher", "TRUE", "--fisher-simulate", "TRUE"))
  reference <- stats::fisher.test(tab)
  row <- bundle$result$results$tests_df
  near(row$fisher_p, reference$p.value, "Boundary Fisher p")
  check(identical(row$fisher_simulated, FALSE), "A 2x2 exact Fisher result was labelled simulated")
  for (field in c("fisher_odds_ratio", "fisher_ci_low", "fisher_ci_high")) {
    wanted <- switch(field, fisher_odds_ratio = unname(reference$estimate), fisher_ci_low = reference$conf.int[1], fisher_ci_high = reference$conf.int[2])
    if (is.infinite(wanted)) {
      check(is.na(row[[field]]) && identical(row[[paste0(field, "_status")]], "positive_infinity"), paste("Unbounded", field, "was confused with unavailable inference"))
    } else {
      near(row[[field]], wanted, paste("Boundary", field))
      check(identical(row[[paste0(field, "_status")]], "finite"), paste("Finite", field, "status differs"))
    }
  }
  replay(context, bundle)
})
test("crosstabs", "crosstabs_nonestimable_requested_test_fails_counts_only_succeeds", {
  context <- new_case("crosstabs-degenerate", data.frame(row = c("a", "a", "a", NA), col = c("x", "y", "x", "y")))
  new_run(context, "crosstabs", c("--row", "row", "--col", "col"), failure = TRUE)
  bundle <- new_run(context, "crosstabs", c("--row", "row", "--col", "col", "--chisq", "FALSE", "--fisher", "FALSE"))
  near(bundle$result$results$cells_df$n, c(2, 1), "Counts-only degenerate cells")
  check(all(is.na(bundle$result$results$tests_df$chi_p)) && all(is.na(bundle$result$results$tests_df$fisher_p)), "Counts-only invented inference")
})
test("crosstabs", "crosstabs_seeded_rxc_Fisher_direct_stats_and_replay", {
  context <- new_case("crosstabs-simulated", expand_table(rxc))
  bundle <- new_run(context, "crosstabs", c("--row", "row", "--col", "col", "--percent", "all", "--expected", "TRUE", "--residuals", "TRUE", "--yates", "FALSE", "--fisher", "TRUE", "--fisher-simulate", "TRUE", "--fisher-b", "250", "--seed", "4831"))
  check_crosstabs(bundle$result$results, context$data, fisher = TRUE, simulate = TRUE, seed = 4831)
  near(bundle$request$rng$seed, 4831, "Saved Fisher seed")
  replay(context, bundle)
})
test("crosstabs", "crosstabs_implicit_seed_uses_own_module_not_regression", {
  context <- new_case("crosstabs-module-default-seed", expand_table(rxc))
  yaml::write_yaml(list(defaults = list(digits = 5L), modules = list(crosstabs = list(seed = 37L), regression = list(seed = 99L))), private_config)
  bundle <- new_run(context, "crosstabs", c("--row", "row", "--col", "col", "--percent", "all", "--expected", "TRUE", "--residuals", "TRUE", "--yates", "FALSE", "--fisher", "TRUE", "--fisher-simulate", "TRUE", "--fisher-b", "250"))
  check_crosstabs(bundle$result$results, context$data, fisher = TRUE, simulate = TRUE, seed = 37L)
  near(bundle$request$rng$seed, 37L, "Implicit Fisher seed from correct module")
  near(bundle$request$options$seed, 37L, "Resolved implicit Fisher seed")
  replay(context, bundle)
})
test("crosstabs", "crosstabs_grouped_multi_pair_missing_disambiguation", {
  data <- do.call(rbind, lapply(c("NA", NA, "(Missing group)"), function(g) { d <- expand_table(two); d$group <- g; d }))
  data$row2 <- ifelse(data$row == "r1", "X", "Y"); data$col2 <- ifelse(data$col == "c1", "V", "W")
  data$row[c(1, 29)] <- NA; data$col[c(2, 30)] <- NA
  context <- new_case("crosstabs-grouped-multiple", data)
  bundle <- new_run(context, "crosstabs", c("--rows", "row,row2", "--cols", "col,col2", "--group", "group", "--percent", "all", "--expected", "TRUE", "--residuals", "TRUE", "--yates", "FALSE"))
  check_crosstabs(bundle$result$results, data, rows = c("row", "row2"), cols = c("col", "col2"), group = "group")
  groups <- bundle$request$design$groups
  design <- bundle$request$design$tables
  for (i in seq_len(nrow(design))) {
    g <- groups[groups$group == design$group[i], , drop = FALSE]
    selected <- if (g$is_missing) is.na(data$group) else !is.na(data$group) & data$group == g$value
    wanted <- which(selected & stats::complete.cases(data[c(design$row_var[i], design$col_var[i])]))
    check(identical(as.integer(design$row_indices[[i]]), wanted), "Crosstabs request records incorrect complete-case row identities")
  }
  check(length(unique(markdown_table(bundle$markdown, "Row Level")$Group)) == 3L, "Crosstabs displayed missing group collides with literal group")
  replay(context, bundle)
})
test("crosstabs", "crosstabs_requested_Fisher_error_has_no_completed_bundle", {
  tab <- matrix(10L + ((seq_len(81) * 17L) %% 43L), 9, dimnames = list(paste0("r", 1:9), paste0("c", 1:9)))
  reference_error <- tryCatch({ stats::fisher.test(tab); NULL }, error = function(e) conditionMessage(e))
  check(!is.null(reference_error), "Requested-test-failure fixture no longer triggers stats::fisher.test error")
  context <- new_case("crosstabs-failed-Fisher", expand_table(tab))
  bundle <- new_run(context, "crosstabs", c("--row", "row", "--col", "col", "--fisher", "TRUE", "--fisher-simulate", "FALSE"), failure = TRUE)
  check(grepl("Fisher|FEXACT|fisher", paste(unlist(bundle$result$error), collapse = " ")), "Failed Fisher context was lost")
})
explorer_data <- data.frame(category = c(rep("A", 7), rep("B", 4), rep("C", 3), "D", "E", "F", NA),
  ordered = ordered(rep(c("low", "medium", "high"), 6), levels = c("low", "medium", "high")),
  integer = rep(c(1L, 1L, 2L), 6), continuous = c(NA, seq(.25, 8.25, .5)),
  logical = rep(c(TRUE, FALSE, NA), 6), date = as.Date("2020-01-01") + seq_len(18), empty = rep(NA_real_, 18))
test("data_explorer", "explorer_full_precision_overview_levels_and_heuristic_types", {
  context <- new_case("explorer-overview", explorer_data)
  bundle <- new_run(context, "data_explorer", c("--max-levels", "8", "--top-n", "2", "--digits", "4"))
  overview <- bundle$result$results$overview_df
  check(identical(as.character(overview$variable), names(explorer_data)), "Explorer default selection/order differs")
  expected_types <- c("nominal", "ordinal", "ordinal", "interval/ratio", "nominal", "interval", "interval/ratio")
  check(identical(overview$measurement_level, expected_types), "Heuristic measurement types differ")
  for (variable in names(explorer_data)) {
    x <- explorer_data[[variable]]; valid <- x[!is.na(x)]; row <- overview[overview$variable == variable, , drop = FALSE]
    for (pair in list(c("total_n", length(x)), c("valid_n", length(valid)), c("missing_n", sum(is.na(x))), c("missing_pct", mean(is.na(x)) * 100), c("unique_n", length(unique(valid))))) near(row[[pair[1]]], as.numeric(pair[2]), paste("explorer", variable, pair[1]))
    if (is.numeric(x) && length(valid)) for (field in c("mean", "sd", "min", "max", "median", "q1", "q3")) {
      expected <- switch(field, mean = mean(valid), sd = stats::sd(valid), min = min(valid), max = max(valid), median = stats::median(valid), q1 = unname(stats::quantile(valid, .25)), q3 = unname(stats::quantile(valid, .75)))
      near(row[[field]], expected, paste("explorer numeric", variable, field))
    }
  }
  for (variable in c("category", "ordered", "integer", "logical", "empty")) {
    actual <- bundle$result$results$levels_df; actual <- actual[actual$variable == variable, , drop = FALSE]
    wanted <- freq_reference(explorer_data[[variable]])
    actual <- actual[match(wanted$level, actual$level), , drop = FALSE]
    for (field in setdiff(names(wanted), "level")) near(actual[[field]], wanted[[field]], paste("explorer levels", variable, field))
  }
  table <- markdown_table(bundle$markdown, "Scale")
  for (mapping in list(c("n", "valid_n"), c("Missing %", "missing_pct"), c("Unique", "unique_n"), c("M", "mean"), c("SD", "sd"), c("Min", "min"), c("Max", "max"))) near(numeric_cells(table[[mapping[1]]]), round(overview[[mapping[2]]], 4), paste("explorer Markdown", mapping[1]))
  replay(context, bundle)
})
test("data_explorer", "explorer_topN_asymmetric_Other_percentages_and_run_numbering", {
  context <- new_case("explorer-topN", explorer_data)
  first <- new_run(context, "data_explorer", c("--vars", "category", "--max-levels", "3", "--top-n", "2"))
  second <- new_run(context, "data_explorer", c("--vars", "category", "--max-levels", "3", "--top-n", "2"))
  levels <- second$result$results$levels_df
  check(identical(levels$level, c("A", "B", "Other (remaining)")), "Explorer top-N ordering/Other differs")
  near(levels$n, c(7, 4, 6), "Explorer top-N counts")
  near(levels$pct_total, c(7, 4, 6) / 18 * 100, "Explorer top-N total percentages")
  near(levels$pct_valid, c(7, 4, 6) / 17 * 100, "Explorer top-N valid percentages")
  check(isTRUE(second$result$results$overview_df$levels_truncated), "Explorer truncation not disclosed")
  check(identical(first$markdown, second$markdown), "Repeating explorer changed standalone table numbering")
  check(grepl("**Table 1**", second$markdown, fixed = TRUE) && grepl("**Table 2**", second$markdown, fixed = TRUE) && !grepl("**Table 3**", second$markdown, fixed = TRUE), "Per-run explorer numbering is not 1/2")
  check(grepl("**Table 4**", text_file(file.path(context$dataset, "report_canonical.md")), fixed = TRUE), "Legacy explorer table numbering stopped advancing")
  replay(context, second)
})
test("data_explorer", "explorer_literal_Other_category_is_distinct_from_remainder", {
  data <- data.frame(category = c(rep("Other (remaining)", 7), rep("A", 5), rep("B", 3), rep("C", 2), NA))
  context <- new_case("explorer-literal-Other", data)
  bundle <- new_run(context, "data_explorer", c("--max-levels", "2", "--top-n", "2"))
  levels <- bundle$result$results$levels_df
  check(identical(levels$level_kind, c("observed", "observed", "remainder")), "Explorer lost observed versus aggregated level identity")
  check(!anyDuplicated(levels$level), "Literal Other category collides with aggregate name")
  check(identical(levels$level[1], "Other (remaining)"), "Literal Other category was renamed")
  near(levels$n, c(7, 5, 5), "Literal Other/remainder counts")
  near(levels$pct_total, c(7, 5, 5) / 18 * 100, "Literal Other total percentages")
  near(levels$pct_valid, c(7, 5, 5) / 17 * 100, "Literal Other valid percentages")
  table <- markdown_table(bundle$markdown, "Level")
  table <- table[table$Level != "Missing", , drop = FALSE]
  check(!anyDuplicated(table$Level) && nrow(table) == 3L, "Markdown merges literal Other with remainder")
  replay(context, bundle)
})
test("data_explorer", "explorer_topN_exceeding_all_levels_is_not_truncated", {
  context <- new_case("explorer-no-effective-truncation", explorer_data)
  bundle <- new_run(context, "data_explorer", c("--vars", "category", "--max-levels", "3", "--top-n", "20"))
  overview <- bundle$result$results$overview_df
  levels <- bundle$result$results$levels_df
  check(identical(overview$levels_truncated, FALSE), "Explorer falsely reports effective truncation")
  check(all(levels$level_kind == "observed"), "Untruncated levels include an aggregate")
  wanted <- freq_reference(explorer_data$category)
  actual <- levels[match(wanted$level, levels$level), , drop = FALSE]
  for (field in setdiff(names(wanted), "level")) near(actual[[field]], wanted[[field]], paste("Explorer untruncated", field))
})
for (module in c("frequencies", "crosstabs", "data_explorer")) {
  module_options <- switch(module, frequencies = c("--vars", "row,col"), crosstabs = c("--row", "row", "--col", "col"), data_explorer = c("--vars", "row,col"))
  test(module, paste0(module, "_log_optout_privacy_templates_and_frozen_replay"), {
    context <- new_case(paste0(module, "-privacy-template"), expand_table(two))
    yaml::write_yaml(list(defaults = list(digits = 5L), logging = list(include_user_prompt = FALSE)), private_config)
    marker <- cfg$template_marker
    template <- file.path(context$base, "custom.md")
    writeLines(c(paste0("# ", marker), "{{table_body}}", "{{overview_table_body}}", "{{levels_table_body}}", "{{narrative}}"), template)
    bundle <- new_run(context, module, c(module_options, "--interactive", "FALSE", "--template", template, "--log", "FALSE", "--user-prompt", "PRIVATE_CATEGORICAL_PROMPT"))
    check(grepl(marker, bundle$markdown, fixed = TRUE), "Custom template not applied")
    check(!grepl("PRIVATE_CATEGORICAL_PROMPT", text_file(bundle$request_path), fixed = TRUE), "Prompt optout leaked into request")
    log <- file.path(context$dataset, "analysis_log.jsonl")
    if (file.exists(log)) check(!grepl(paste0('"module":"', module, '"'), gsub(" ", "", text_file(log)), fixed = TRUE), "--log FALSE wrote module legacy log")
    writeLines("CHANGED ORIGINAL TEMPLATE", template)
    current <- arrow::read_parquet(file.path(context$dataset, "sample.parquet"), as_data_frame = TRUE)
    current$row[1] <- "CHANGED WORKING VALUE"
    arrow::write_parquet(current, file.path(context$dataset, "sample.parquet"))
    yaml::write_yaml(list(defaults = list(digits = 1L), logging = list(include_user_prompt = TRUE)), private_config)
    replay(context, bundle)
    check(identical(arrow::read_parquet(file.path(context$dataset, "sample.parquet"), as_data_frame = TRUE)$row[1], "CHANGED WORKING VALUE"), "Replay overwrote working data")
  })
  test(module, paste0(module, "_invalid_numeric_options_fail_closed"), {
    context <- new_case(paste0(module, "-invalid-options"), expand_table(two))
    run_module(module, c(context$flag, context$input, module_options, "--digits", "1.5"), failure = TRUE)
    directories <- runs(context)
    check(!any(vapply(directories, function(path) identical(read_json(file.path(path, "result.json"))$status, "completed"), logical(1))), "Invalid options created a completed run")
  })
  test(module, paste0(module, "_unpublished_or_misnamed_bundle_cannot_replay"), {
    context <- new_case(paste0(module, "-pending-replay"), expand_table(two))
    bundle <- new_run(context, module, module_options)
    for (name in c(".pending-success-shaped-copy", "incorrect-run-identity")) {
      destination <- file.path(dirname(bundle$path), name)
      dir.create(destination)
      check(all(file.copy(list.files(bundle$path, all.files = TRUE, full.names = TRUE, no.. = TRUE), destination, recursive = TRUE)), "Could not prepare copied replay fixture")
      before <- runs(context)
      run_module("replay_run", c("--request", file.path(destination, "request.json")), failure = TRUE)
      check(identical(before, runs(context)), "Unpublished/misnamed fixture produced a new run")
    }
  })
}
summary_path <- file.path(work, "results.json")
jsonlite::write_json(list(schema_version = 1L, suite = "phase2-categorical", modules = modules, numeric_checks = numeric_checks, tests = results,
  environment = list(r = R.version.string, packages = as.list(vapply(required, function(p) as.character(utils::packageVersion(p)), character(1))))), summary_path, auto_unbox = TRUE, pretty = TRUE, digits = NA)
passed <- sum(vapply(results, function(result) result$passed, logical(1)))
cat(sprintf("Phase 2 categorical: %d/%d cases passed; %d numeric comparisons. Results: %s\n", passed, length(results), numeric_checks, summary_path))
if (!nzchar(forced_root) && keep > 0L && dir.exists(output_base)) {
  candidates <- list.dirs(output_base, full.names = TRUE, recursive = FALSE)
  candidates <- sort(candidates[grepl("^[0-9]{14}$", basename(candidates))], decreasing = TRUE)
  if (length(candidates) > keep) for (path in setdiff(candidates[seq.int(keep + 1L, length(candidates))], run_root)) {
    if (identical(dirname(path), output_base) && grepl("^[0-9]{14}$", basename(path))) unlink(path, recursive = TRUE)
  }
}
quit(status = if (passed == length(results)) 0 else 1)
