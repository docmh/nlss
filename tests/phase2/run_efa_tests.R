#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Independent public-CLI acceptance: no NLSS scientific helper or historical golden.
args <- commandArgs(TRUE)
script <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])
repo <- normalizePath(file.path(dirname(script[1]), "../.."), winslash = "/")
if ("--help" %in% args) {
  cat("Usage: Rscript tests/phase2/run_efa_tests.R [--root PATH] [--keep N] [--match REGEX]\n",
      "Honors NLSS_TEST_ROOT, NLSS_KEEP_RUNS and NLSS_TESTS_CONFIG; private offline fixtures/configuration.\n", sep = "")
  quit(status = 0)
}
if (length(args) %% 2L || (length(args) && any(!args[seq(1L, length(args), 2L)] %in% c("--root", "--keep", "--match")))) stop("Unknown/incomplete runner option")
arg <- function(name, fallback) {
  i <- which(args == name)
  if (length(i) > 1L) stop("Repeated runner option: ", name)
  if (length(i)) args[i + 1L] else fallback
}
required <- c("yaml", "jsonlite", "arrow", "digest", "haven", "psych", "GPArotation")
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
work <- file.path(run_root, "phase2-efa", paste0("run-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid()))
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
  actual <- as.numeric(actual); expected <- as.numeric(expected)
  check(length(actual) == length(expected) && length(expected) > 0L, paste(label, "length differs"))
  expected[is.infinite(expected)] <- NA_real_
  check(identical(is.na(actual), is.na(expected)), paste(label, "missing values differ"))
  valid <- !is.na(expected)
  check(all(is.finite(actual[valid])) && all(abs(actual[valid] - expected[valid]) <= tolerance * pmax(1, abs(expected[valid]))), paste(label, "differs from independent reference"))
  numeric_checks <<- numeric_checks + length(expected)
}
near_probability <- function(actual, expected, label) {
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
  results[[length(results) + 1L]] <<- list(module = "efa", test = name, passed = is.null(error),
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
new_run <- function(context, options, failure = FALSE, module = "efa", source = TRUE) {
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

# Independent latent data, deliberately uneven correlations and missing patterns.
set.seed(72913)
n <- 360L
latent <- matrix(rnorm(n * 2L), ncol = 2L); latent[, 2] <- .43 * latent[, 1] + sqrt(1 - .43^2) * latent[, 2]
loadings <- matrix(c(.84,.16,.73,.08,.81,.12,.66,.21,.17,.84,.11,.72,.22,.77,.08,.68), ncol = 2L, byrow = TRUE)
items <- paste0("i", seq_len(nrow(loadings)))
data <- as.data.frame(latent %*% t(loadings) + matrix(rnorm(n * length(items), sd = .64), nrow = n))
names(data) <- items
data$group <- factor(rep(c("B", "A"), length.out = n), levels = c("A", "B", "unused"))
data$i1[c(2, 14)] <- NA_real_; data$i3[c(6, 31)] <- NA_real_; data$i7[29] <- NaN
data$unused <- Inf
base_options <- c("--vars", paste(items, collapse = ","), "--n-factors", "2", "--method", "pca", "--rotation", "varimax", "--cor", "pearson", "--missing", "complete", "--seed", "1")
raw_groups <- function(bundle, where = "results") {
  object <- jsonlite::fromJSON(if (where == "results") file.path(bundle$path, "result.json") else bundle$request_path, simplifyVector = FALSE)
  object[[if (where == "results") "results" else "design"]]$groups
}
matrix_values <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.matrix(x)) return(x)
  if (is.list(x) && !is.null(x$values)) {
    values <- x$values
    if (is.list(values)) values <- do.call(rbind, lapply(values, unlist))
    return(as.matrix(values))
  }
  if (is.list(x) && !is.null(x$data)) return(matrix_values(list(values = x$data)))
  if (is.list(x)) return(do.call(rbind, lapply(x, unlist)))
  as.matrix(x)
}
reference <- function(sample, method = "pca", rotation = "varimax", correlation = "pearson", missing = "complete", factors = 2L, threshold = 1, selected = items) {
  x <- sample[selected]
  if (correlation %in% c("polychoric", "tetrachoric")) for (name in names(x)) {
    x[[name]] <- if (is.factor(x[[name]])) as.integer(x[[name]]) else match(x[[name]], sort(unique(x[[name]][!is.na(x[[name]])])))
  }
  complete <- complete.cases(x)
  used <- if (missing == "complete") x[complete, , drop = FALSE] else x[rowSums(!is.na(x)) > 0L, , drop = FALSE]
  r <- if (correlation %in% c("pearson", "spearman")) stats::cor(used, use = if (missing == "complete") "complete.obs" else "pairwise.complete.obs", method = correlation) else if (correlation == "polychoric") suppressWarnings(psych::polychoric(used)$rho) else suppressWarnings(psych::tetrachoric(used)$rho)
  eigenvalues <- eigen(r, symmetric = TRUE, only.values = TRUE)$values
  if (is.character(factors)) factors <- max(1L, sum(eigenvalues > threshold))
  fit <- if (method == "pca") suppressWarnings(psych::principal(r, nfactors = factors, rotate = rotation, scores = FALSE)) else suppressWarnings(psych::fa(r, nfactors = factors, rotate = rotation, fm = method, n.obs = nrow(used)))
  pattern <- as.matrix(fit$loadings)
  phi <- if (is.null(fit$Phi)) diag(ncol(pattern)) else fit$Phi
  h2 <- diag(pattern %*% phi %*% t(pattern))
  list(correlation = r, eigen = eigenvalues, fit = fit, pattern = pattern, phi = phi, structure = pattern %*% phi,
    h2 = h2, u2 = 1 - h2, complete = which(complete), n = nrow(used), factors = factors,
    pairwise_n = crossprod(!is.na(as.matrix(x))), kmo = psych::KMO(r), bartlett = suppressWarnings(psych::cortest.bartlett(r, n = nrow(used))))
}
check_solution <- function(bundle, ref, index = 1L, source_rows = seq_len(nrow(data)), selected = items) {
  groups <- raw_groups(bundle); check(length(groups) >= index, "Raw per-group EFA solution missing")
  group <- groups[[index]]
  matrices <- group$matrices
  near(matrix_values(matrices$correlation), ref$correlation, "Correlation matrix")
  near(matrix_values(matrices$pattern_loadings), ref$pattern, "Unsuppressed pattern loadings")
  near(matrix_values(matrices$structure_loadings), ref$structure, "Structure loadings")
  near(matrix_values(matrices$Phi), ref$phi, "Factor correlations")
  near(matrix_values(matrices$reproduced), ref$structure %*% t(ref$pattern), "Reproduced covariance")
  near(matrix_values(matrices$residual), ref$correlation - ref$structure %*% t(ref$pattern), "Residual correlation")
  near(matrix_values(matrices$variance_accounted), ref$fit$Vaccounted, "Variance accounted")
  near(unlist(group$diagnostics$kmo$item), ref$kmo$MSAi, "Item-level KMO")
  actual <- bundle$result$results$loadings_df
  if ("group_index" %in% names(actual)) actual <- actual[actual$group_index == index, , drop = FALSE] else if (length(groups) > 1L) actual <- actual[actual$group == group$group, , drop = FALSE]
  actual <- actual[match(selected, actual$item), , drop = FALSE]
  check(identical(actual$item, selected), "Item identity/order differs")
  primary <- max.col(abs(ref$pattern), ties.method = "first")
  near(actual$loading, ref$pattern[cbind(seq_along(selected), primary)], "Primary loading")
  near(actual$h2, ref$h2, "Communality including oblique factor covariance")
  near(actual$u2, ref$u2, "Uniqueness")
  near(actual$complexity, ref$fit$complexity, "Item complexity")
  summary <- bundle$result$results$summary_df[index, , drop = FALSE]
  check_row(summary, list(n_obs = ref$n, n_items = length(selected), n_factors = ref$factors, kmo = ref$kmo$MSA,
    bartlett_chi2 = ref$bartlett$chisq, bartlett_df = ref$bartlett$df, bartlett_p = ref$bartlett$p.value,
    variance_explained = sum(ref$h2) / length(selected)), "EFA summary")
  eigen <- bundle$result$results$eigen_df
  if ("group_index" %in% names(eigen)) eigen <- eigen[eigen$group_index == index, , drop = FALSE] else if (length(groups) > 1L) eigen <- eigen[eigen$group == group$group, , drop = FALSE]
  check_row(eigen, list(eigenvalue = ref$eigen, proportion = ref$eigen / length(selected), cumulative = cumsum(ref$eigen / length(selected))), "Correlation eigenvalues")
  design <- raw_groups(bundle, "design")[[index]]
  near(unlist(design$row_indices), source_rows, "Group source rows", tolerance = 0)
  near(unlist(design$complete_case_rows), source_rows[ref$complete], "Complete source rows", tolerance = 0)
  near(matrix_values(design$pairwise_n), ref$pairwise_n, "Pairwise sample counts", tolerance = 0)
  invisible(ref)
}
run_compare <- function(name, sample = data, options = character(), method = "pca", rotation = "varimax", correlation = "pearson", missing = "complete", factors = 2L, threshold = 1, selected = items, format = "rds") {
  context <- new_case(name, sample, format)
  bundle <- new_run(context, replace_options(base_options, c("--method", method, "--rotation", rotation, "--cor", correlation, "--missing", missing, "--n-factors", as.character(factors), "--eigen-threshold", as.character(threshold), "--vars", paste(selected, collapse = ","), options)))
  set.seed(1L); ref <- reference(sample, method, rotation, correlation, missing, factors, threshold, selected)
  check_solution(bundle, ref, source_rows = seq_len(nrow(sample)), selected = selected)
  list(context = context, bundle = bundle, ref = ref)
}

for (method in c("pca", "pa", "minres", "ml", "uls", "gls", "wls", "alpha")) test(paste0("efa_method_", method, if (method == "pca") "_smoke" else ""), {
  run_compare(paste0("method-", method), method = method)
})
for (rotation in c("none", "promax", "oblimin", "quartimax", "equamax", "bentlerQ", "geominQ")) test(paste0("efa_rotation_", rotation, if (rotation == "oblimin") "_smoke" else ""), {
  run_compare(paste0("rotation-", rotation), method = "minres", rotation = rotation)
})
for (rotation in c("Varimax", "Promax")) test(paste0("efa_legacy_rotation_alias_", rotation), {
  context <- new_case(paste0("rotation-alias-", rotation), data)
  bundle <- new_run(context, replace_options(base_options, c("--rotation", rotation)))
  set.seed(1L); check_solution(bundle, reference(data, rotation = tolower(rotation)))
  check(bundle$request$options$rotation == tolower(rotation), "Legacy case-insensitive rotation alias changed its meaning")
})
for (missing in c("complete", "pairwise")) for (correlation in c("pearson", "spearman")) test(paste0("efa_correlation_", correlation, "_", missing), {
  run_compare(paste0(correlation, "-", missing), correlation = correlation, missing = missing)
})
for (correlation in c("polychoric", "tetrachoric")) for (missing in c("complete", "pairwise")) test(paste0("efa_correlation_", correlation, "_", missing, if (correlation == "polychoric" && missing == "complete") "_smoke" else ""), {
  sample <- data
  for (name in items) sample[[name]] <- if (correlation == "polychoric") as.integer(cut(sample[[name]], breaks = c(-Inf, -.8, -.15, .5, Inf))) else as.integer(sample[[name]] > 0)
  run_compare(paste0(correlation, "-", missing), sample, correlation = correlation, missing = missing)
})
test("efa_eigen_retention_and_threshold", {
  run_compare("eigen", factors = "eigen", threshold = .85)
  run_compare("eigen-none", factors = "eigen", threshold = 100)
})
test("efa_fixed_one_factor_and_reordered_items", {
  run_compare("one-factor", factors = 1L, rotation = "none", selected = rev(items))
})
test("efa_raw_loadings_unchanged_by_display_cutoff_sort", {
  first <- run_compare("cutoff-high", options = c("--loading-cutoff", ".9", "--sort-loadings", "FALSE"))
  second <- new_run(first$context, replace_options(base_options, c("--loading-cutoff", "0", "--sort-loadings", "TRUE")))
  check(identical(raw_groups(first$bundle), raw_groups(second)), "Display cutoff or sorting changed raw scientific solution")
  check(!identical(first$bundle$markdown, second$markdown), "Display cutoff/sort did not affect Markdown")
})
test("efa_grouped_factor_levels_missing_and_literal_NA", {
  sample <- data
  sample$group <- factor(rep(c("B", "NA", NA, "A"), length.out = nrow(sample)), levels = c("A", "NA", "B", "unused"))
  context <- new_case("grouped", sample)
  bundle <- new_run(context, c(base_options, "--group", "group"))
  designs <- raw_groups(bundle, "design")
  check(length(designs) == 4L, "Missing group merged with literal NA or unused group fitted")
  seen <- integer()
  set.seed(1L)
  for (i in seq_along(designs)) {
    rows <- as.integer(unlist(designs[[i]]$row_indices)); seen <- c(seen, rows)
    same <- sample$group[rows]
    check(length(unique(same)) == 1L, "Group split mixed distinct identities")
    check_solution(bundle, reference(sample[rows, , drop = FALSE]), i, rows)
  }
  check(identical(sort(seen), seq_len(nrow(sample))), "Group partition omitted or duplicated source rows")
})
test("efa_sav_value_labels_user_missing_numeric_roles_smoke", {
  sample <- data; sample$unused <- NULL; sample$i1[1] <- 99
  for (name in items) sample[[name]] <- haven::labelled_spss(sample[[name]], labels = c(Low = -1, High = 1, Missing = 99), na_values = 99, label = paste("Research item", name))
  context <- new_case("sav", sample, "sav")
  bundle <- new_run(context, base_options)
  clean <- data; clean$i1[1] <- NA_real_
  set.seed(1); check_solution(bundle, reference(clean))
  check(grepl("Research item", bundle$markdown, fixed = TRUE), "SAV variable labels absent from Markdown")
  dictionary <- read_json(file.path(context$project, bundle$request$dataset$dictionary_path))
  check(grepl("Missing", jsonlite::toJSON(dictionary), fixed = TRUE), "SAV value/missing labels lost from preserved dictionary")
})
test("efa_ordered_nonlexical_levels_and_numeric_coercion", {
  sample <- data
  for (name in items) sample[[name]] <- ordered(c("low", "middle", "high")[cut(sample[[name]], c(-Inf, -.3, .5, Inf), labels = FALSE)], levels = c("middle", "low", "high"))
  run_compare("ordered", sample, correlation = "polychoric", options = c("--coerce", "FALSE"))
  sample <- data; sample$i1 <- as.character(sample$i1)
  context <- new_case("coercion", sample)
  bundle <- new_run(context, c(base_options, "--coerce", "TRUE"))
  set.seed(1); check_solution(bundle, reference(data))
})
test("efa_gapped_ordinal_codes_and_duplicate_SAV_labels", {
  sample <- data; sample$unused <- NULL
  for (name in items) {
    codes <- 10L * as.integer(cut(sample[[name]], c(-Inf, -.6, 0, .7, Inf)))
    sample[[name]] <- haven::labelled_spss(codes, labels = c(Z = 10, A = 20, A = 30, B = 40, Missing = 99), na_values = 99)
  }
  sample$i1[1] <- 99
  context <- new_case("gapped-sav", sample, "sav")
  bundle <- new_run(context, replace_options(base_options, c("--cor", "polychoric")))
  clean <- sample; for (name in items) clean[[name]] <- as.numeric(haven::zap_missing(clean[[name]]))
  set.seed(1); check_solution(bundle, reference(clean, correlation = "polychoric"))
})
test("efa_nearby_numeric_groups_keep_distinct_partitions", {
  sample <- data; sample$group <- rep(c(1, 1 + 1e-14), length.out = nrow(sample))
  context <- new_case("nearby-groups", sample)
  bundle <- new_run(context, c(base_options, "--group", "group"))
  groups <- raw_groups(bundle, "design")
  check(length(groups) == 2L, "Close numeric group IDs collapsed")
  set.seed(1)
  for (i in seq_along(groups)) {
    rows <- as.integer(unlist(groups[[i]]$row_indices))
    check(length(unique(sample$group[rows])) == 1L, "Close numeric groups mixed")
    check_solution(bundle, reference(sample[rows, , drop = FALSE]), i, rows)
  }
})
test("efa_pairwise_all_missing_rows_excluded_from_nominal_N", {
  sample <- data; sample[1, items] <- NA_real_
  result <- run_compare("all-missing-row", sample, missing = "pairwise")
  check(result$bundle$result$results$summary_df$bartlett_status == "approximate", "Pairwise Bartlett nominal-N approximation not disclosed")
})
test("efa_nonselected_infinity_and_nan_as_missing", {
  run_compare("unused-inf", data)
})
test("efa_private_source_configuration_template_and_rng_replay_smoke", {
  context <- new_case("replay", data)
  yaml::write_yaml(list(logging = list(include_user_prompt = FALSE, include_outputs = FALSE)), private_config)
  template <- file.path(context$base, "private-template.md")
  writeLines(c(paste0("# ", cfg$template_marker), "{{table_body}}", "{{narrative}}", "{{note_body}}"), template)
  bundle <- new_run(context, c(base_options, "--template", template, "--log", "FALSE", "--user-prompt", "PRIVATE_EFA_INTENT"))
  check(grepl(cfg$template_marker, bundle$markdown, fixed = TRUE), "Custom template ignored")
  check(!grepl("PRIVATE_EFA_INTENT", text_file(bundle$request_path), fixed = TRUE), "Prompt privacy ignored")
  check(!grepl(context$base, text_file(bundle$request_path), fixed = TRUE), "External source/template path leaked")
  check(length(bundle$result$results) > 0L, "Mandatory results suppressed by legacy include_outputs")
  logfile <- file.path(context$dataset, "analysis_log.jsonl")
  if (file.exists(logfile)) check(!grepl('"module":"efa"', gsub(" ", "", text_file(logfile)), fixed = TRUE), "Legacy log optout ignored")
  writeLines("CHANGED TEMPLATE", template)
  changed <- data; changed$i1[1] <- 999; saveRDS(changed, context$input)
  working <- file.path(context$dataset, "sample.parquet"); arrow::write_parquet(changed, working)
  yaml::write_yaml(list(defaults = list(digits = 1L), modules = list(efa = list(seed = 771L, method = "ml", rotation = "none"))), private_config)
  replay(context, bundle)
  near(arrow::read_parquet(working)$i1[1], 999, "Replay preserved working copy", tolerance = 0)
})
test("efa_config_defaults_cli_precedence", {
  context <- new_case("defaults", data)
  yaml::write_yaml(list(modules = list(efa = list(seed = 728L, method = "ml", rotation = "none", n_factors = "2"))), private_config)
  bundle <- new_run(context, c("--vars", paste(items, collapse = ","), "--method", "pca"))
  check(bundle$request$options$seed == 728L && bundle$request$options$method == "pca" && bundle$request$options$rotation == "none", "Resolved config / CLI precedence differs")
  set.seed(728L); check_solution(bundle, reference(data, rotation = "none"))
})
test("efa_failed_analysis_preserves_prior_publication", {
  context <- new_case("protected", data)
  new_run(context, base_options)
  failed(context, replace_options(base_options, c("--vars", "i1,does_not_exist")))
})
invalid <- list(method = c("--method", "invented"), rotation = c("--rotation", "invented"), correlation = c("--cor", "invented"), missing = c("--missing", "invented"),
  fractional_factors = c("--n-factors", "1.5"), excessive_factors = c("--n-factors", "99"), zero_factors = c("--n-factors", "0"), cutoff = c("--loading-cutoff", "-1"),
  seed = c("--seed", "-1"), fractional_seed = c("--seed", "1.5"), threshold_nan = c("--eigen-threshold", "NaN"), sort_boolean = c("--sort-loadings", "perhaps"),
  duplicate_vars = c("--vars", "i1,i1,i2"), missing_item = c("--vars", "i1,absent"), one_item = c("--vars", "i1"))
for (name in names(invalid)) test(paste0("efa_invalid_option_", name), {
  context <- new_case(paste0("invalid-", name), data)
  run_module("efa", c(context$flag, context$input, replace_options(base_options, invalid[[name]])), failure = TRUE)
  check(!any(vapply(runs(context), function(path) identical(read_json(file.path(path, "result.json"))$status, "completed"), logical(1))), "Invalid request published completed analysis")
})
test("efa_singular_matrix_PCA_retained_with_diagnostic_status", {
  sample <- data; sample$i2 <- sample$i1
  context <- new_case("singular-pca", sample)
  bundle <- new_run(context, base_options)
  set.seed(1L); correlation <- cor(sample[complete.cases(sample[items]), items])
  expected <- suppressWarnings(psych::principal(correlation, nfactors = 2, rotate = "varimax", scores = FALSE))
  matrices <- raw_groups(bundle)[[1]]$matrices
  near(matrix_values(matrices$pattern_loadings), as.matrix(expected$loadings), "Singular PCA pattern loadings")
  check(raw_groups(bundle)[[1]]$diagnostics$matrix_status == "singular", "Singular correlation matrix not disclosed")
  check(bundle$result$results$summary_df$bartlett_status == "unavailable", "Singular Bartlett statistic represented as available")
})
test("efa_inconsistent_pairwise_matrix_rejected", {
  values <- seq(-1, 1, length.out = 8L)
  sample <- data.frame(i1 = c(values, values, rep(NA_real_, 8)), i2 = c(values, rep(NA_real_, 8), values), i3 = c(rep(NA_real_, 8), values, -values))
  check(min(eigen(cor(sample, use = "pairwise.complete.obs"), symmetric = TRUE)$values) < 0, "Non-PSD fixture lost its independent precondition")
  context <- new_case("non-psd", sample)
  run_module("efa", c(context$flag, context$input, replace_options(base_options, c("--vars", "i1,i2,i3", "--missing", "pairwise"))), failure = TRUE)
})
for (name in c("infinity", "negative_infinity", "constant", "all_missing")) test(paste0("efa_invalid_data_", name), {
  sample <- data
  if (name == "infinity") sample$i1[1] <- Inf
  if (name == "negative_infinity") sample$i1[1] <- -Inf
  if (name == "constant") sample$i1 <- 1
  if (name == "all_missing") sample$i1 <- NA_real_
  context <- new_case(paste0("invalid-data-", name), sample)
  run_module("efa", c(context$flag, context$input, base_options), failure = TRUE)
})

summary_path <- file.path(work, "results.json")
jsonlite::write_json(list(schema_version = 1L, suite = "phase2-efa", modules = "efa", test_pattern = test_pattern,
  numeric_checks = numeric_checks, tests = results, source_sha256 = sha(file.path(repo, "scripts/R/efa.R")),
  test_sha256 = sha(normalizePath(script[1], winslash = "/")),
  r_source_sha256 = as.list(vapply(sort(list.files(file.path(repo, "scripts/R"), "[.]R$", recursive = TRUE, full.names = TRUE)), sha, character(1))),
  environment = list(r = R.version.string, packages = as.list(vapply(required, function(p) as.character(utils::packageVersion(p)), character(1))))),
  summary_path, auto_unbox = TRUE, pretty = TRUE, digits = NA)
passed <- sum(vapply(results, function(result) result$passed, logical(1)))
cat(sprintf("Phase 2 EFA: %d/%d cases passed; %d numeric comparisons. Results: %s\n", passed, length(results), numeric_checks, summary_path))
if (!nzchar(forced_root) && keep > 0L && dir.exists(output_base)) {
  candidates <- sort(list.dirs(output_base, full.names = TRUE, recursive = FALSE), decreasing = TRUE)
  candidates <- candidates[grepl("^[0-9]{14}$", basename(candidates))]
  if (length(candidates) > keep) for (path in setdiff(candidates[seq.int(keep + 1L, length(candidates))], run_root)) {
    if (identical(dirname(path), output_base) && grepl("^[0-9]{14}$", basename(path))) unlink(path, recursive = TRUE)
  }
}
quit(status = if (length(results) > 0L && passed == length(results)) 0 else 1)
