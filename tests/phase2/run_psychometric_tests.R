#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Public-CLI acceptance. Numeric oracles use base R/stats, never NLSS helpers.
args <- commandArgs(TRUE)
script <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])
repo <- normalizePath(file.path(dirname(script[1]), "../.."), winslash = "/")
if ("--help" %in% args) {
  cat("Usage: Rscript tests/phase2/run_psychometric_tests.R [--root PATH] [--keep N] [--modules scale,reliability] [--match REGEX]\n",
      "Honors NLSS_TEST_ROOT, NLSS_KEEP_RUNS and NLSS_TESTS_CONFIG; uses private configuration and no network.\n", sep = "")
  quit(status = 0)
}
if (length(args) %% 2L || (length(args) && any(!args[seq(1L, length(args), 2L)] %in% c("--root", "--keep", "--modules", "--match")))) stop("Unknown/incomplete runner option")
arg <- function(name, fallback) {
  i <- which(args == name)
  if (length(i) > 1L) stop("Repeated runner option: ", name)
  if (length(i)) args[i + 1L] else fallback
}
required <- c("yaml", "jsonlite", "arrow", "digest", "haven", "psych")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing test packages: ", paste(missing, collapse = ", "))
absolute <- function(path) normalizePath(if (grepl("^(/|[A-Za-z]:|\\\\)", path)) path else file.path(repo, path), winslash = "/", mustWork = FALSE)
cfg <- yaml::read_yaml(absolute(Sys.getenv("NLSS_TESTS_CONFIG", "tests/tests.yml")), eval.expr = FALSE)$tests
modules <- strsplit(arg("--modules", "scale,reliability"), ",", fixed = TRUE)[[1]]
if (!length(modules) || anyDuplicated(modules) || any(!modules %in% c("scale", "reliability"))) stop("Unsupported --modules selection")
test_pattern <- arg("--match", ".*")
if (!nzchar(test_pattern)) stop("Empty --match regular expression")
invisible(grepl(test_pattern, "validate regular expression"))
forced_root <- arg("--root", Sys.getenv("NLSS_TEST_ROOT", ""))
output_base <- absolute(cfg$output_dir)
run_root <- if (nzchar(forced_root)) absolute(forced_root) else file.path(output_base, format(Sys.time(), "%Y%m%d%H%M%S"))
keep <- suppressWarnings(as.integer(arg("--keep", Sys.getenv("NLSS_KEEP_RUNS", as.character(cfg$keep_runs_default)))))
if (length(keep) != 1L || is.na(keep) || keep < 0L) stop("Invalid --keep")
work <- file.path(run_root, "phase2-psychometric", paste0("run-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid()))
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

# Fixed synthetic data are generated with a seed only inside this test process.
# No CLI implementation or NLSS statistical function is sourced by this suite.
set.seed(270319)
latent <- stats::rnorm(72)
items <- as.data.frame(sapply(c(.7, .8, .6, .9), function(loading) 3 + loading * latent + stats::rnorm(72, sd = .55)))
names(items) <- paste0("item", 1:4)
items$item2 <- 6 - items$item2
items$item1[c(2, 31)] <- NA_real_
items$item3[c(5, 29, 66)] <- NA_real_
items$group <- rep(c("NA", NA_character_, "treatment"), each = 24)
item_names <- paste0("item", 1:4)
select_group <- function(frame, group) {
  if (is.null(group)) return(frame)
  check("group_missing" %in% names(frame), "Missing group identity is not explicit")
  frame[if (is.na(group)) frame$group_missing else !frame$group_missing & frame$group == as.character(group), , drop = FALSE]
}
group_data <- function(data, group) if (is.null(group)) data else data[if (is.na(group)) is.na(data$group) else !is.na(data$group) & data$group == group, , drop = FALSE]
check_group_rows <- function(bundle, data) {
  groups <- bundle$request$design$groups
  check(is.data.frame(groups), "Resolved groups are not tabular")
  for (i in seq_len(nrow(groups))) {
    expected <- which(if (isTRUE(groups$is_missing[i])) is.na(data$group) else !is.na(data$group) & data$group == groups$value[i])
    check(identical(as.integer(groups$row_indices[[i]]), expected), "Resolved original group row identities differ")
  }
}
scale_reference <- function(data, missing = "pairwise", score = "sum", omega = TRUE) {
  complete <- stats::complete.cases(data)
  analysis <- if (missing == "complete") data[complete, , drop = FALSE] else data
  covmat <- stats::cov(analysis, use = "pairwise.complete.obs")
  cormat <- stats::cor(analysis, use = "pairwise.complete.obs")
  # psych raw alpha/deleted alpha use covariance, standardized alpha uses the
  # separately estimated correlation matrix (not cov2cor(pairwise covariance)).
  alpha <- suppressWarnings(psych::alpha(covmat, n.obs = sum(complete), check.keys = FALSE, warnings = FALSE))
  standardized <- suppressWarnings(psych::alpha(cormat, n.obs = sum(complete), check.keys = FALSE, warnings = FALSE))
  # Obtain item-to-total correlation from a synthetic matrix with exactly the
  # target covariance, avoiding duplication of the module covariance formula.
  k <- ncol(data)
  orthogonal <- qr.Q(qr(cbind(rep(1, k + 1L), stats::contr.helmert(k + 1L))))[, -1, drop = FALSE]
  equivalent <- sqrt(k) * orthogonal %*% chol(covmat)
  item_total <- vapply(seq_len(k), function(i) stats::cor(equivalent[, i], rowSums(equivalent)), numeric(1))
  item_rest <- vapply(seq_len(k), function(i) stats::cor(equivalent[, i], rowSums(equivalent[, -i, drop = FALSE])), numeric(1))
  scores <- if (score == "sum") rowSums(data[complete, , drop = FALSE]) else rowMeans(data[complete, , drop = FALSE])
  omega_value <- NA_real_
  if (omega) {
    fit <- stats::factanal(covmat = list(cov = cormat, n.obs = sum(complete)), factors = 1, rotation = "none", control = list(nstart = 1))
    common_variance <- sum(as.matrix(fit$loadings) %*% t(as.matrix(fit$loadings)))
    omega_value <- common_variance / (common_variance + sum(fit$uniquenesses))
  }
  item <- data.frame(n = colSums(!is.na(data)), missing_n = colSums(is.na(data)), missing_pct = colMeans(is.na(data)) * 100,
    mean = vapply(data, mean, numeric(1), na.rm = TRUE), sd = vapply(data, stats::sd, numeric(1), na.rm = TRUE),
    min = vapply(data, min, numeric(1), na.rm = TRUE), max = vapply(data, max, numeric(1), na.rm = TRUE),
    item_total_r = item_total, item_rest_r = item_rest, alpha_if_deleted = alpha$alpha.drop$raw_alpha)
  correlations <- cormat[lower.tri(cormat)]
  summary <- list(n_items = k, n_total = nrow(data), n_complete = sum(complete), missing_n = sum(!complete), missing_pct = mean(!complete) * 100,
    alpha = alpha$total$raw_alpha, alpha_std = standardized$total$std.alpha, omega_total = omega_value,
    r_bar = mean(correlations), r_min = min(correlations), r_max = max(correlations),
    score_mean = mean(scores), score_sd = stats::sd(scores), score_min = min(scores), score_max = max(scores))
  list(item = item, summary = summary)
}
check_scale <- function(bundle, data, missing = "pairwise", score = "sum", omega = TRUE, grouped = FALSE) {
  for (group in if (grouped) unique(data$group) else list(NULL)) {
    wanted <- scale_reference(group_data(data, group)[item_names], missing, score, omega)
    actual_items <- select_group(bundle$result$results$item_df, group)
    actual_items <- actual_items[match(item_names, actual_items$item), , drop = FALSE]
    for (field in names(wanted$item)) near(actual_items[[field]], wanted$item[[field]], paste("scale item", group, field), tolerance = 2e-7)
    actual_summary <- select_group(bundle$result$results$reliability_df, group)
    for (field in names(wanted$summary)) near(actual_summary[[field]], wanted$summary[[field]], paste("scale summary", group, field), tolerance = 2e-7)
  }
}
for (missing_method in c("pairwise", "complete")) for (score_method in c("sum", "mean")) test("scale", paste("scale_all_values", missing_method, score_method, sep = "_"), {
  context <- new_case(paste("scale-values", missing_method, score_method, sep = "-"), items)
  bundle <- new_run(context, "scale", c("--vars", paste(item_names, collapse = ","), "--reverse", "item2", "--reverse-min", "0", "--reverse-max", "6", "--missing", missing_method, "--score", score_method, "--omega", "TRUE", "--digits", "4"))
  transformed <- items; transformed$item2 <- 6 - transformed$item2
  check_scale(bundle, transformed, missing_method, score_method)
  check(identical(bundle$request$options$missing, missing_method) && identical(bundle$request$options$score, score_method), "Scale resolved methods differ")
  table <- markdown_table(bundle$markdown, "Item")
  near(numeric_cells(table$M), round(bundle$result$results$item_df$mean, 4), "scale Markdown item M")
  near(numeric_cells(table$SD), round(bundle$result$results$item_df$sd, 4), "scale Markdown item SD")
  if (score_method == "sum") replay(context, bundle)
})
test("scale", "scale_observed_reverse_bounds_are_global_group_NA_distinct", {
  context <- new_case("scale-group-observed", items)
  bundle <- new_run(context, "scale", c("--vars", paste(item_names, collapse = ","), "--reverse", "item2", "--group", "group", "--omega", "FALSE"))
  transformed <- items; transformed$item2 <- min(items$item2) + max(items$item2) - transformed$item2
  check_scale(bundle, transformed, omega = FALSE, grouped = TRUE)
  check_group_rows(bundle, items)
  check(length(unique(markdown_table(bundle$markdown, "Item")$Group)) == 3L, "Scale group display names collide")
  replay(context, bundle)
})
test("scale", "scale_explicit_coercion_factor_codes_and_missing", {
  data <- items; data$item1 <- factor(as.character(data$item1)); data$item3 <- as.character(data$item3); data$item3[8] <- "not_numeric"
  context <- new_case("scale-coerce", data)
  bundle <- new_run(context, "scale", c("--vars", paste(item_names, collapse = ","), "--coerce", "TRUE", "--omega", "FALSE"))
  data$item1 <- as.numeric(as.character(data$item1)); data$item3 <- suppressWarnings(as.numeric(data$item3))
  check_scale(bundle, data, omega = FALSE)
})
test("scale", "scale_SPSS_variable_value_labels_and_user_missing", {
  data <- items; data$group <- rep(c(1, 2, 9), each = 24)
  data$item1[2] <- 99
  data$item1 <- haven::labelled_spss(data$item1, labels = c("Keine Angabe" = 99), na_values = 99, label = "Belastung")
  data$group <- haven::labelled_spss(data$group, labels = c("Kontrolle" = 1, "Intervention" = 2, "Keine Gruppe" = 9), na_values = 9, label = "Bedingung")
  context <- new_case("scale-spss", data, "sav")
  bundle <- new_run(context, "scale", c("--vars", paste(item_names, collapse = ","), "--group", "group", "--omega", "FALSE"))
  clean <- items; clean$group <- rep(c(1, 2, NA_real_), each = 24)
  check_scale(bundle, clean, omega = FALSE, grouped = TRUE)
  check(grepl("Belastung", bundle$markdown, fixed = TRUE) && grepl("Kontrolle", bundle$markdown, fixed = TRUE), "Scale SPSS labels missing")
  replay(context, bundle)
})
test("scale", "unavailable_item_and_indefinite_pairwise_matrix_are_not_partial_reliability", {
  data <- items; data$item4 <- rep(3, nrow(data))
  context <- new_case("scale-constant-item", data)
  bundle <- new_run(context, "scale", c("--vars", paste(item_names, collapse = ","), "--omega", "TRUE"))
  row <- bundle$result$results$reliability_df
  check(is.na(row$alpha_std) && row$alpha_std_status != "ok", "Standardized alpha uses only remaining computable correlations")
  check(is.na(row$omega_total) && row$omega_status != "ok", "Incomplete correlation matrix yields omega")
  data <- data.frame(a = c(-1, 1, -1, 1, NA, NA), b = c(-1, 1, NA, NA, -1, 1), c = c(NA, NA, -1, 1, 1, -1))
  check(min(eigen(stats::cov(data, use = "pairwise.complete.obs"), symmetric = TRUE)$values) < 0, "Indefinite fixture is not indefinite")
  context <- new_case("scale-indefinite", data)
  bundle <- new_run(context, "scale", c("--vars", "a,b,c", "--missing", "pairwise", "--omega", "TRUE"))
  row <- bundle$result$results$reliability_df
  check(is.na(row$alpha) && row$alpha_status != "ok", "Indefinite covariance produced raw alpha")
  check(all(is.na(bundle$result$results$item_df$item_total_r)), "Indefinite covariance produced item-total correlations")
  near(bundle$result$results$item_df$mean, c(0, 0, 0), "Indefinite-data item descriptions retained")
  check(length(bundle$result$warnings) > 0L, "Unavailable matrix reliability has no warning")
})

ratings <- data.frame(r1 = c(8, 2, 5, 4, 7, 3, 6, 9, 2, 8, 4, 5, 1, 6, 7, 4, 9, 3, 8, 5),
  r2 = c(9, 4, 4, 6, 8, 3, 7, 8, 4, 9, 5, 6, 2, 7, 9, 5, 8, 3, 7, 7),
  r3 = c(7, 3, 6, 3, 6, 4, 5, 7, 1, 6, 4, 4, 2, 5, 8, 3, 7, 2, 9, 4))
ratings$r1[4] <- NA; ratings$r3[8] <- NA
icc_reference <- function(data, model, type, unit, confidence = .9) {
  valid <- stats::complete.cases(data)
  reference <- suppressWarnings(psych::ICC(data[valid, , drop = FALSE], lmer = FALSE, alpha = 1 - confidence))$results
  target <- paste0(if (model == "oneway") "ICC1" else if (type == "agreement") "ICC2" else "ICC3", if (unit == "average") "k" else "")
  row <- reference[reference$type == target, , drop = FALSE]
  list(estimate = row$ICC, ci_low = row$`lower bound`, ci_high = row$`upper bound`, p_value = row$p, f_stat = row$F,
    df1 = row$df1, df2 = row$df2, n = sum(valid), n_raters = ncol(data), missing_n = sum(!valid), missing_pct = mean(!valid) * 100)
}
check_row <- function(actual, expected, label) for (field in names(expected)) near(actual[[field]], expected[[field]], paste(label, field), tolerance = 2e-7)
for (model in c("oneway", "twoway-random", "twoway-mixed")) test("reliability", paste0("ICC_all_types_units_", model), {
  context <- new_case(paste0("icc-", model), ratings)
  for (type in c("agreement", "consistency")) for (unit in c("single", "average")) {
    bundle <- new_run(context, "reliability", c("--analysis", "icc", "--vars", "r1,r2,r3", "--icc-model", model, "--icc-type", type, "--icc-unit", unit, "--missing", "pairwise", "--conf-level", "0.9"))
    check_row(bundle$result$results$summary_df, icc_reference(ratings, model, type, unit), paste("ICC", model, type, unit))
    check(identical(bundle$request$options$icc_model, model), "ICC model lost")
  }
  replay(context, bundle)
})
long_ratings <- function(data) {
  do.call(rbind, lapply(seq_along(data), function(j) data.frame(id = seq_len(nrow(data)), rater = names(data)[j], score = data[[j]], stringsAsFactors = FALSE)))
}
test("reliability", "ICC_long_unbalanced_subject_ids_and_missing_identity_rows", {
  data <- long_ratings(ratings)
  data <- data[!is.na(data$score), ]; data <- rbind(data, data.frame(id = NA_integer_, rater = "r2", score = 5), data.frame(id = 3L, rater = NA_character_, score = 4))
  context <- new_case("icc-long-unbalanced", data)
  bundle <- new_run(context, "reliability", c("--analysis", "icc", "--format", "long", "--id", "id", "--rater", "rater", "--score", "score", "--conf-level", "0.9"))
  check_row(bundle$result$results$summary_df, icc_reference(ratings, "twoway-random", "agreement", "single"), "ICC long")
  replay(context, bundle)
})
test("reliability", "ICC_groups_literal_NA_and_missing_group_do_not_merge", {
  data <- do.call(rbind, replicate(3, ratings, simplify = FALSE)); data$group <- rep(c("NA", NA_character_, "control"), each = nrow(ratings))
  data$r2 <- data$r2 + rep(c(0, 2, -1), each = nrow(ratings))
  context <- new_case("icc-groups", data)
  bundle <- new_run(context, "reliability", c("--vars", "r1,r2,r3", "--group", "group", "--conf-level", "0.9"))
  for (group in unique(data$group)) check_row(select_group(bundle$result$results$summary_df, group), icc_reference(group_data(data, group)[c("r1", "r2", "r3")], "twoway-random", "agreement", "single"), "ICC grouped")
  check_group_rows(bundle, data)
  check(length(unique(markdown_table(bundle$markdown, "Group")$Group)) == 3L, "ICC group display names collide")
})
kappa_data <- data.frame(a = c(1, 1, 1, 1, 2, 2, 2, 2, 10, 10, 10, 10, 1, 2, 10, 1, 2, 10, NA, 10),
  b = c(1, 1, 2, 10, 1, 2, 2, 10, 1, 2, 10, 10, 1, 2, 10, 2, 10, 1, 2, NA))
kappa_reference <- function(data, weight, levels) {
  valid <- stats::complete.cases(data)
  tab <- table(factor(data[[1]][valid], levels = levels), factor(data[[2]][valid], levels = levels))
  ref <- suppressWarnings(psych::cohen.kappa(tab, n.obs = sum(valid), w.exp = if (weight == "linear") 1 else 2))
  list(estimate = if (weight == "none") ref$kappa else ref$weighted.kappa, n = sum(valid), n_raters = 2,
    missing_n = sum(!valid), missing_pct = mean(!valid) * 100, n_categories = length(levels))
}
for (weight in c("none", "linear", "quadratic")) test("reliability", paste0("kappa_numeric_order_unbalanced_", weight), {
  context <- new_case(paste0("kappa-numeric-", weight), kappa_data)
  bundle <- new_run(context, "reliability", c("--analysis", "kappa", "--vars", "a,b", "--kappa-weight", weight))
  check_row(bundle$result$results$summary_df, kappa_reference(kappa_data, weight, c(1, 2, 10)), "numeric kappa")
  if (weight == "quadratic") replay(context, bundle)
})
test("reliability", "kappa_declared_factor_order_unused_levels_long_parity", {
  levels <- c("medium", "low", "unused", "high")
  data <- kappa_data
  for (name in names(data)) data[[name]] <- factor(c("low", "medium", "high")[match(data[[name]], c(1, 2, 10))], levels = levels, ordered = TRUE)
  context <- new_case("kappa-factor-order", data)
  bundle <- new_run(context, "reliability", c("--analysis", "kappa", "--vars", "a,b", "--kappa-weight", "linear"))
  check_row(bundle$result$results$summary_df, kappa_reference(data, "linear", levels), "ordered factor kappa")
  context_long <- new_case("kappa-factor-long", long_ratings(data))
  long <- new_run(context_long, "reliability", c("--analysis", "kappa", "--format", "long", "--id", "id", "--rater", "rater", "--score", "score", "--kappa-weight", "linear"))
  check_row(long$result$results$summary_df, kappa_reference(data, "linear", levels), "long ordered factor kappa")
  replay(context_long, long)
})
retest_reference <- function(data, method, confidence = .9) {
  valid <- stats::complete.cases(data); clean <- data[valid, , drop = FALSE]
  fit <- suppressWarnings(stats::cor.test(clean[[1]], clean[[2]], method = method, exact = FALSE, conf.level = confidence))
  estimate <- unname(fit$estimate)
  ci <- if (method == "pearson") fit$conf.int[1:2] else tanh(atanh(estimate) + c(-1, 1) * stats::qnorm((1 + confidence) / 2) / sqrt(nrow(clean) - 3))
  list(estimate = estimate, ci_low = ci[1], ci_high = ci[2], p_value = fit$p.value, n = sum(valid), n_raters = 2, missing_n = sum(!valid), missing_pct = mean(!valid) * 100)
}
for (method in c("pearson", "spearman")) for (format in c("wide", "long")) test("reliability", paste("test_retest", method, format, sep = "_"), {
  data <- ratings[c("r1", "r2")]
  context <- new_case(paste("retest", method, format, sep = "-"), if (format == "wide") data else long_ratings(data))
  opts <- if (format == "wide") c("--vars", "r1,r2") else c("--format", "long", "--id", "id", "--rater", "rater", "--score", "score")
  bundle <- new_run(context, "reliability", c("--analysis", "test_retest", "--method", method, "--conf-level", "0.9", opts))
  check_row(bundle$result$results$summary_df, retest_reference(data, method), paste("retest", method, format))
  check(!is.null(bundle$request$design$ci_method), "Test-retest CI method is not explicit")
  if (format == "wide") replay(context, bundle)
})
test("reliability", "test_retest_perfect_correlation_is_not_clipped", {
  context <- new_case("retest-perfect", data.frame(a = 1:12, b = -(1:12)))
  bundle <- new_run(context, "reliability", c("--analysis", "test_retest", "--vars", "a,b"))
  near(bundle$result$results$summary_df$estimate, -1, "Perfect Pearson point estimate", tolerance = 1e-14)
  near(bundle$result$results$summary_df$p_value, 0, "Perfect Pearson p", tolerance = 1e-14)
})
test("reliability", "point_estimable_ICC_boundary_and_three_case_retest_CI_status", {
  context <- new_case("icc-perfect", data.frame(r1 = 1:12, r2 = 1:12, r3 = 1:12))
  bundle <- new_run(context, "reliability", c("--vars", "r1,r2,r3"))
  near(bundle$result$results$summary_df$estimate, 1, "Perfect ICC point estimate", tolerance = 1e-14)
  check(!is.null(bundle$result$results$summary_df$f_stat_status), "Infinite/boundary ICC F lacks machine-readable status")
  replay(context, bundle)
  data <- data.frame(a = 1:3, b = c(2, 1, 4))
  context <- new_case("retest-three-subjects", data)
  bundle <- new_run(context, "reliability", c("--analysis", "test_retest", "--vars", "a,b"))
  row <- bundle$result$results$summary_df
  fit <- stats::cor.test(data$a, data$b)
  near(row$estimate, unname(fit$estimate), "Three-case Pearson point")
  near(row$p_value, fit$p.value, "Three-case Pearson p")
  check(is.na(row$ci_low) && is.na(row$ci_high) && identical(row$ci_status, "not_available"), "Three-case correlation invents confidence interval")
})
test("reliability", "expected_invalid_exit_zero_still_records_nonreplayable_failed_bundle", {
  context <- new_case("reliability-expected-invalid", data.frame(a = rep("constant", 8), b = rep("constant", 8)))
  bundle <- new_run(context, "reliability", c("--analysis", "kappa", "--vars", "a,b", "--expect-invalid", "TRUE"), failure = TRUE, expected_invalid = TRUE)
  run_module("replay_run", c("--request", bundle$request_path), failure = TRUE)
  check(length(runs(context)) == 1L, "Expected-invalid failed run was replayed")
})
test("reliability", "long_coercion_provenance_identifies_only_the_bad_rating_row", {
  data <- data.frame(id = rep(1:4, each = 2), rater = rep(c("A", "B"), 4), score = c("bad", "1", "2", "3", "4", "5", "6", "7"))
  context <- new_case("long-coercion-provenance", data)
  bundle <- new_run(context, "reliability", c("--analysis", "test_retest", "--format", "long", "--id", "id", "--rater", "rater", "--score", "score", "--coerce", "TRUE"))
  design <- jsonlite::fromJSON(bundle$request_path, simplifyVector = FALSE)$design$groups[[1]]
  check(identical(as.integer(unlist(design$coercion_introduced_missing_source_rows$A)), 1L), "Bad rating provenance includes other raters")
  check(length(design$coercion_introduced_missing_source_rows$B) == 0L, "Valid rater gets false coercion loss")
  check(identical(as.integer(unlist(design$source_rows_by_rater$A)), c(1L, 3L, 5L, 7L)), "First-rater source rows differ")
  check(identical(as.integer(unlist(design$source_rows_by_rater$B)), c(2L, 4L, 6L, 8L)), "Second-rater source rows differ")
  check(identical(as.integer(unlist(design$complete_subject_indices)), 2:4), "Complete-case subjects differ")
  near(bundle$result$results$summary_df$n, 3, "Coercion retains three paired subjects")
})
test("reliability", "retest_SPSS_labels_user_missing_and_explicit_numeric_coercion", {
  data <- ratings[c("r1", "r2")]; data$r1[4] <- 99
  data$r1 <- haven::labelled_spss(data$r1, labels = c("Nicht beantwortet" = 99), na_values = 99, label = "Messung Eins")
  data$r2 <- haven::labelled(data$r2, label = "Messung Zwei")
  context <- new_case("retest-spss", data, "sav")
  bundle <- new_run(context, "reliability", c("--analysis", "test_retest", "--vars", "r1,r2", "--conf-level", "0.9"))
  check_row(bundle$result$results$summary_df, retest_reference(ratings[c("r1", "r2")], "pearson"), "SPSS retest")
  check(grepl("Messung Eins", bundle$markdown, fixed = TRUE) && grepl("Messung Zwei", bundle$markdown, fixed = TRUE), "Retest SPSS variable labels missing")
  data <- ratings[c("r1", "r2")]; data$r1 <- factor(as.character(data$r1)); data$r2 <- as.character(data$r2); data$r2[6] <- "bad"
  context <- new_case("retest-coerce", data)
  bundle <- new_run(context, "reliability", c("--analysis", "test_retest", "--vars", "r1,r2", "--coerce", "TRUE", "--conf-level", "0.9"))
  data$r1 <- as.numeric(as.character(data$r1)); data$r2 <- suppressWarnings(as.numeric(data$r2))
  check_row(bundle$result$results$summary_df, retest_reference(data, "pearson"), "coerced retest")
})
test("reliability", "structural_long_duplicates_fail_and_preserve_prior_publication", {
  data <- long_ratings(ratings)
  context <- new_case("reliability-duplicate", rbind(data, data[1, ]))
  bundle <- new_run(context, "reliability", c("--format", "long", "--id", "id", "--rater", "rater", "--score", "score"), failure = TRUE)
  check(grepl("[Dd]uplicate", bundle$result$error$message), "Duplicate id/rater failure is not explained")
})
test("reliability", "contradictory_weighted_factor_orders_fail_closed", {
  data <- data.frame(a = factor(c("low", "medium", "high", "low", "high", "medium"), levels = c("low", "medium", "high")),
    b = factor(c("low", "high", "high", "medium", "high", "low"), levels = c("high", "medium", "low")))
  context <- new_case("kappa-conflicting-orders", data)
  new_run(context, "reliability", c("--analysis", "kappa", "--vars", "a,b", "--kappa-weight", "linear"), failure = TRUE)
})
for (module in c("scale", "reliability")) {
  module_data <- if (module == "scale") items else ratings
  module_options <- if (module == "scale") c("--vars", paste(item_names, collapse = ","), "--omega", "FALSE") else c("--vars", "r1,r2,r3")
  test(module, paste0(module, "_privacy_log_optout_frozen_config_template_and_input_replay"), {
    context <- new_case(paste0(module, "-privacy"), module_data)
    yaml::write_yaml(list(defaults = list(digits = 5L), logging = list(include_user_prompt = FALSE, include_outputs = FALSE)), private_config)
    template <- file.path(context$base, "custom.md")
    writeLines(c(paste0("# ", cfg$template_marker), "{{table_body}}", "{{narrative}}"), template)
    bundle <- new_run(context, module, c(module_options, "--interactive", "FALSE", "--template", template, "--log", "FALSE", "--user-prompt", "PRIVATE_PSYCHOMETRIC_PROMPT"))
    check(grepl(cfg$template_marker, bundle$markdown, fixed = TRUE), "Custom template absent")
    check(!grepl("PRIVATE_PSYCHOMETRIC_PROMPT", text_file(bundle$request_path), fixed = TRUE), "Private prompt leaked")
    check(length(bundle$result$results) > 0L, "Legacy output optout removed mandatory raw results")
    log <- file.path(context$dataset, "analysis_log.jsonl")
    if (file.exists(log)) check(!grepl(paste0('"module":"', module, '"'), gsub(" ", "", text_file(log)), fixed = TRUE), "Legacy log optout ignored")
    writeLines("CHANGED ORIGINAL TEMPLATE", template)
    current_path <- file.path(context$dataset, "sample.parquet")
    current <- arrow::read_parquet(current_path, as_data_frame = TRUE); current[[1]][1] <- 123456
    arrow::write_parquet(current, current_path)
    source_data <- module_data; source_data[[1]][1] <- -999; saveRDS(source_data, context$input)
    yaml::write_yaml(list(defaults = list(digits = 1L), logging = list(include_user_prompt = TRUE)), private_config)
    replay(context, bundle)
    near(arrow::read_parquet(current_path, as_data_frame = TRUE)[[1]][1], 123456, "Replay preserved working input")
  })
  test(module, paste0(module, "_invalid_option_domains_fail_closed"), {
    context <- new_case(paste0(module, "-invalid-options"), module_data)
    invalid <- list(c("--digits", "1.5"), c("--missing", "arbitrary"), c("--vars", if (module == "scale") "item1,item1" else "r1,r1"))
    invalid <- c(invalid, if (module == "scale") list(c("--reverse", "unknown"), c("--reverse", "item2", "--reverse-min", "5", "--reverse-max", "1"), c("--vars", "item1,item1,item2", "--group", "group")) else list(c("--conf-level", "1"), c("--icc-model", "unknown")))
    for (options in invalid) {
      base_options <- module_options
      duplicate <- which(base_options == options[1])
      if (length(duplicate)) base_options <- base_options[-c(duplicate, duplicate + 1L)]
      run_module(module, c(context$flag, context$input, base_options, options), failure = TRUE)
    }
    check(!any(vapply(runs(context), function(path) identical(read_json(file.path(path, "result.json"))$status, "completed"), logical(1))), "Invalid options published success")
  })
  test(module, paste0(module, "_constant_data_has_explicit_nonestimable_status"), {
    data <- module_data
    numeric_vars <- if (module == "scale") item_names else c("r1", "r2", "r3")
    data[numeric_vars] <- lapply(data[numeric_vars], function(x) rep(4, length(x)))
    context <- new_case(paste0(module, "-constant"), data)
    bundle <- new_run(context, module, module_options, failure = module == "reliability")
    if (module == "scale") {
      row <- bundle$result$results$reliability_df
      check(length(row$alpha) == 1L && is.na(row$alpha), "Constant-data alpha is presented as estimable")
      check(length(row$alpha_status) == 1L && !is.na(row$alpha_status) && !row$alpha_status %in% c("ok", "finite", "estimated"), "Constant-data unavailable status missing")
      replay(context, bundle)
    }
  })
}
summary_path <- file.path(work, "results.json")
jsonlite::write_json(list(schema_version = 1L, suite = "phase2-psychometric", modules = modules, test_pattern = test_pattern, numeric_checks = numeric_checks, tests = results,
  environment = list(r = R.version.string, packages = as.list(vapply(required, function(p) as.character(utils::packageVersion(p)), character(1))))), summary_path, auto_unbox = TRUE, pretty = TRUE, digits = NA)
passed <- sum(vapply(results, function(result) result$passed, logical(1)))
cat(sprintf("Phase 2 psychometric: %d/%d cases passed; %d numeric comparisons. Results: %s\n", passed, length(results), numeric_checks, summary_path))
if (!nzchar(forced_root) && keep > 0L && dir.exists(output_base)) {
  candidates <- list.dirs(output_base, full.names = TRUE, recursive = FALSE)
  candidates <- sort(candidates[grepl("^[0-9]{14}$", basename(candidates))], decreasing = TRUE)
  if (length(candidates) > keep) for (path in setdiff(candidates[seq.int(keep + 1L, length(candidates))], run_root)) {
    if (identical(dirname(path), output_base) && grepl("^[0-9]{14}$", basename(path))) unlink(path, recursive = TRUE)
  }
}
quit(status = if (length(results) > 0L && passed == length(results)) 0 else 1)
