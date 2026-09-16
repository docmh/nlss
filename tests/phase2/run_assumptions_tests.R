#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Public CLI acceptance; numerical oracles call base R / installed packages,
# never source NLSS scientific helpers or use its old outputs as truth.
args <- commandArgs(TRUE)
script <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])[1]
repo <- normalizePath(file.path(dirname(script), "../.."), winslash = "/")
script <- normalizePath(script, winslash = "/")
if ("--help" %in% args) { cat("Usage: run_assumptions_tests.R [--root PATH] [--keep N] [--match REGEX]\n"); quit(status = 0) }
if (length(args) %% 2L || (length(args) && any(!args[seq(1L, length(args), 2L)] %in% c("--root", "--keep", "--match")))) stop("Unknown/incomplete runner option")
arg <- function(name, fallback) { i <- which(args == name); if (length(i) > 1L) stop("Repeated runner option"); if (length(i)) args[i + 1L] else fallback }
pattern <- arg("--match", ".*"); invisible(grepl(pattern, "validate regex"))
required <- c("yaml", "jsonlite", "digest", "arrow", "haven", "car", "lme4", "lavaan",
  if (grepl(pattern, "assumptions_mixed_dharma_custom_seed_replay")) "DHARMa")
for (package in required) if (!requireNamespace(package, quietly = TRUE)) stop("Missing test package: ", package)
absolute <- function(path) normalizePath(if (grepl("^(/|[A-Za-z]:|\\\\)", path)) path else file.path(repo, path), winslash = "/", mustWork = FALSE)
cfg <- yaml::read_yaml(absolute(Sys.getenv("NLSS_TESTS_CONFIG", "tests/tests.yml")), eval.expr = FALSE)$tests
forced <- arg("--root", Sys.getenv("NLSS_TEST_ROOT", ""))
keep <- suppressWarnings(as.integer(arg("--keep", Sys.getenv("NLSS_KEEP_RUNS", as.character(cfg$keep_runs_default)))))
if (length(keep) != 1L || is.na(keep) || keep < 0L) stop("Invalid --keep")
collection <- file.path(if (nzchar(forced)) absolute(forced) else absolute(cfg$output_dir), "phase2-assumptions")
work <- file.path(collection, paste0("run-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid()))
dir.create(work, recursive = TRUE)
baseline <- yaml::read_yaml(file.path(repo, "scripts/config.yml"), eval.expr = FALSE)
config <- file.path(work, "config.yml")
reset_config <- function() yaml::write_yaml(list(defaults = list(digits = 9L)), config)
reset_config(); Sys.setenv(NLSS_CONFIG_PATH = config, OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1")
check <- function(ok, message) if (!isTRUE(ok)) stop(message, call. = FALSE)
read_text <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
read_json <- function(path) jsonlite::fromJSON(path, simplifyVector = TRUE)
raw_json <- function(path) jsonlite::fromJSON(path, simplifyVector = FALSE)
write_json <- function(value, path) jsonlite::write_json(value, path, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null", na = "null")
sha <- function(path) digest::digest(file = path, algo = "sha256")
numeric_checks <- 0L
near <- function(actual, expected, label, tolerance = 2e-7, probability = FALSE) {
  actual <- as.numeric(unlist(actual)); expected <- as.numeric(unlist(expected))
  check(length(actual) == length(expected) && length(expected) > 0L, paste(label, "length differs"))
  check(identical(is.na(actual), is.na(expected)), paste(label, "missing mask differs"))
  use <- !is.na(expected)
  check(identical(is.infinite(actual[use]), is.infinite(expected[use])), paste(label, "infinite mask differs"))
  finite <- use & is.finite(expected)
  scale <- if (probability) pmax(1e-300, abs(expected[finite])) else pmax(1, abs(expected[finite]))
  check(all(abs(actual[finite] - expected[finite]) <= tolerance * scale), paste(label, "differs from independent reference"))
  numeric_checks <<- numeric_checks + length(expected)
}
results <- list()
test <- function(name, code) {
  if (!grepl(pattern, name)) return(invisible(NULL))
  old <- getwd(); on.exit(setwd(old)); reset_config(); before <- numeric_checks; start <- proc.time()[["elapsed"]]
  error <- tryCatch({ force(code); NULL }, error = conditionMessage)
  results[[length(results) + 1L]] <<- list(test = name, passed = is.null(error), numeric_checks = numeric_checks - before,
    seconds = unname(proc.time()[["elapsed"]] - start), message = if (is.null(error)) "OK" else error)
  cat(if (is.null(error)) "[PASS] " else "[FAIL] ", name, if (is.null(error)) "" else paste0(": ", error), "\n", sep = "")
}
set.seed(9301L)
n <- 84L
sample <- data.frame(id = seq_len(n), g = factor(rep(c("control", "treatment"), each = n / 2), levels = c("treatment", "control")),
  a = factor(rep(c("A", "B"), length.out = n)), b = factor(rep(c("low", "mid", "high"), each = 14, length.out = n)),
  x = rnorm(n), z = rnorm(n), y = rnorm(n), pre = rnorm(n), mid = rnorm(n), post = rnorm(n))
sample$y <- 1 + .7 * sample$x - .3 * sample$z + rnorm(n, sd = .7 + abs(sample$x))
sample$mid <- .7 * sample$pre + sample$mid; sample$post <- .5 * sample$pre + sample$post
sample$x[c(3, 23)] <- NA; sample$z[7] <- NA; sample$y[12] <- NA; sample$pre[8] <- NA; sample$post[19] <- NA
sample$g[31] <- NA; sample$b[36] <- NA
new_case <- function(name, data = sample, format = "rds") {
  base <- file.path(work, "cases", name); project <- file.path(base, "project"); dir.create(project, recursive = TRUE)
  yaml::write_yaml(list(version = 1L, datasets = list()), file.path(project, "nlss-workspace.yml"))
  input <- file.path(base, paste0("sample.", format))
  switch(format, rds = saveRDS(data, input), csv = write.csv(data, input, row.names = FALSE),
    sav = haven::write_sav(data, input), parquet = arrow::write_parquet(data, input), RData = { survey <- data; save(survey, file = input) })
  list(base = base, project = project, input = input, data = data, format = format,
    directory = file.path(project, if (format == "RData") "survey" else "sample"),
    source = c(paste0("--", tolower(format)), input, if (format == "RData") c("--df", "survey")))
}
tokens <- function(options) unlist(Map(function(key, value) c(paste0("--", key), as.character(value)), names(options), options), use.names = FALSE)
run_cli <- function(context, argv, module = "assumptions", failure = FALSE) {
  old <- getwd(); on.exit(setwd(old)); setwd(context$project)
  log <- tempfile(paste0(module, "-"), context$base, fileext = ".log")
  status <- system2(file.path(R.home("bin"), "Rscript"), c(shQuote(file.path(repo, "scripts/R", paste0(module, ".R"))), shQuote(argv)), stdout = log, stderr = log)
  check(if (failure) status != 0L else status == 0L, paste(module, "unexpected exit", status, read_text(log)))
  log
}
runs <- function(context) {
  files <- list.files(context$directory, "^request[.]json$", recursive = TRUE, full.names = TRUE, all.files = TRUE)
  dirname(files[grepl("/runs/[^.][^/]+/request[.]json$", files)])
}
run <- function(context, options, source = TRUE, module = "assumptions") {
  before <- runs(context)
  log <- run_cli(context, c(if (source) context$source, if (is.list(options)) tokens(options) else options), module)
  added <- setdiff(runs(context), before); check(length(added) == 1L, "Expected exactly one published run")
  request_path <- file.path(added, "request.json"); result_path <- file.path(added, "result.json")
  req <- read_json(request_path); res <- read_json(result_path); raw <- raw_json(result_path)
  check(res$status == "completed" && isTRUE(req$resolved), "Unresolved or failed run")
  check(req$module == "assumptions" && res$module == "assumptions", "Wrong module identity")
  check(req$schema_version == 1L && res$schema_version == 1L, "Dataset schema changed")
  check(req$run_id == basename(added) && identical(req$run_id, res$run_id), "Run identity mismatch")
  check(identical(res$artifacts$request$sha256, sha(request_path)), "Request/result hash mismatch")
  for (artifact in res$artifacts) check(identical(sha(file.path(added, artifact$path)), artifact$sha256), "Published artifact hash mismatch")
  for (template in req$templates) check(identical(sha(file.path(added, template$path)), template$sha256), "Preserved template hash mismatch")
  check(length(req$templates) > 0L && length(req$environment$packages) > 0L && length(req$options) > 0L, "Execution context missing")
  check(identical(sha(file.path(context$project, req$dataset$snapshot_path)), req$dataset$data_sha256), "Snapshot hash mismatch")
  check(identical(sha(file.path(context$project, req$dataset$dictionary_path)), req$dataset$dictionary_sha256), "Dictionary hash mismatch")
  rows <- res$results$checks_df
  check(is.data.frame(rows) && nrow(rows) > 0L, "Raw checks_df compatibility key missing")
  check(all(rows$status %in% c("available", "unavailable", "skipped")), "Missing diagnostic availability status")
  bad <- rows$status != "available"
  check(all(rows$decision[bad] == ""), "Unavailable or skipped check was given a decision")
  check(is.list(res$results$diagnostics), "Missing diagnostic context")
  check(!dir.exists(file.path(context$directory, ".analysis-lock")) && !dir.exists(file.path(context$project, ".publication-lock")), "Completed run retained lock")
  list(path = added, request_path = request_path, request = req, result = res, rows = rows, meta = res$results$diagnostics,
    raw_meta = raw$results$diagnostics, markdown = read_text(file.path(added, "output.md")), log = log)
}
row <- function(bundle, test, target = NULL, model = NULL, group = NULL) {
  x <- bundle$rows[bundle$rows$test == test, , drop = FALSE]
  if (!is.null(target)) x <- x[x$target == target, , drop = FALSE]
  if (!is.null(model)) x <- x[x$model == model, , drop = FALSE]
  if (!is.null(group)) x <- x[x$group == group, , drop = FALSE]
  check(nrow(x) == 1L, paste("Expected one diagnostic row:", test, target, model, group)); x
}
shapiro <- function(actual, values, label = "Shapiro") {
  expected <- stats::shapiro.test(values)
  near(actual$statistic, expected$statistic, paste(label, "W")); near(actual$p, expected$p.value, paste(label, "p"), probability = TRUE)
  near(actual$n, sum(!is.na(values)), paste(label, "N"), tolerance = 0)
  check(actual$status == "available", paste(label, "not available"))
}
selection <- function(actual, included, target = NULL, model = NULL) {
  choices <- actual$raw_meta$case_selection
  if (!is.null(target)) choices <- Filter(function(x) identical(x$target, target), choices)
  if (!is.null(model)) choices <- Filter(function(x) identical(x$model, model), choices)
  check(length(choices) == 1L, "Expected one case-selection identity")
  value <- choices[[1L]]
  near(value$included_rows, included, "Included source identities", tolerance = 0)
  excluded <- setdiff(seq_len(actual$meta$source_n), included)
  if (length(excluded)) near(value$excluded_rows, excluded, "Excluded source identities", tolerance = 0)
  else check(!length(value$excluded_rows), "Invented excluded source rows")
  value
}
homogeneity <- function(actual, values, groups, method) {
  groups <- droplevels(factor(groups)); labels <- c(levene = "Levene (median)", bartlett = "Bartlett", fligner = "Fligner-Killeen", f = "F-test")
  if (method == "levene") {
    expected <- car::leveneTest(values, groups, center = median)
    stat <- expected$`F value`[1]; p <- expected$`Pr(>F)`[1]; dfs <- expected$Df
  } else {
    expected <- switch(method, bartlett = stats::bartlett.test(values, groups), fligner = stats::fligner.test(values, groups),
      f = stats::var.test(values[groups == levels(groups)[1]], values[groups == levels(groups)[2]]))
    stat <- expected$statistic; p <- expected$p.value; dfs <- unname(expected$parameter)
  }
  near(actual$statistic, stat, paste(labels[method], "statistic")); near(actual$p, p, paste(labels[method], "p"), probability = TRUE)
  near(actual$df1, dfs[1], paste(labels[method], "df1")); if (length(dfs) > 1L) near(actual$df2, dfs[2], paste(labels[method], "df2"))
  near(actual$n, length(values), "Homogeneity N", tolerance = 0)
}
snapshot <- function(paths) { paths <- paths[file.exists(paths) & !dir.exists(paths)]; setNames(vapply(paths, sha, character(1)), paths) }
protected <- function(context) c(file.path(context$project, "nlss-workspace.yml"), file.path(context$directory, c("report_canonical.md", "analysis_log.jsonl", paste0(basename(context$directory), ".parquet"))))
tree <- function(path) snapshot(sort(list.files(path, recursive = TRUE, full.names = TRUE, all.files = TRUE)))
replay <- function(context, original) {
  before <- tree(original$path); working <- snapshot(tail(protected(context), 1L))
  again <- run(context, c("--request", original$request_path), source = FALSE, module = "replay_run")
  check(identical(again$result$results, original$result$results), "Replay raw results differ")
  check(identical(again$markdown, original$markdown), "Replay Markdown differs")
  check(identical(again$request$replay_of, original$request$run_id), "Missing replay origin")
  check(identical(before, tree(original$path)) && identical(working, snapshot(tail(protected(context), 1L))), "Replay modified original run or working data")
  again
}
failed <- function(context, options, module = "assumptions", source = TRUE) {
  before <- snapshot(protected(context)); previous <- runs(context)
  log <- run_cli(context, c(if (source) context$source, if (is.list(options)) tokens(options) else options), module, failure = TRUE)
  check(identical(before, snapshot(protected(context))), "Failed request changed protected data/report/log/manifest")
  for (path in setdiff(runs(context), previous)) {
    res <- read_json(file.path(path, "result.json"))
    check(res$status == "failed" && !file.exists(file.path(path, "output.md")), "Failed request published ordinary output")
    run_cli(context, c("--request", file.path(path, "request.json")), "replay_run", failure = TRUE)
  }
  invisible(log)
}
tt <- list(analysis = "ttest", vars = "y", homogeneity = "none")
mm <- list(analysis = "mixed_models", formula = "Reaction ~ Days + (Days | Subject)", reml = TRUE,
  optimizer = "bobyqa", maxfun = 100000L, normality = "shapiro", `random-effects` = FALSE,
  influence = FALSE, homoscedasticity = FALSE, performance = FALSE, dharma = FALSE, outliers = FALSE,
  singular = TRUE, convergence = TRUE)
sem_model <- "visual =~ x1+x2+x3; textual =~ x4+x5+x6; speed =~ x7+x8+x9"
sem_opts <- list(analysis = "cfa", model = sem_model, estimator = "ML", missing = "listwise", se = "standard", seed = 401L,
  normality = "none", mardia = FALSE, mahalanobis = FALSE, collinearity = FALSE, convergence = TRUE, heywood = TRUE)

for (format in c("rds", "csv", "RData", "sav", "parquet")) test(paste0("assumptions_ttest_source_", format, if (format == "rds") "_smoke"), {
  context <- new_case(paste0("source-", format), format = format)
  actual <- run(context, tt); shapiro(row(actual, "Shapiro-Wilk", "y"), sample$y)
  selection(actual, which(!is.na(sample$y)), target = "y")
  check(identical(actual$request$dataset$source_sha256, sha(context$input)), "Source hash absent or wrong")
  replay(context, actual)
})
for (method in c("levene", "bartlett", "fligner", "f")) test(paste0("assumptions_ttest_homogeneity_", method), {
  # Ordered factor levels deliberately differ from first occurrence and lexical order.
  context <- new_case(paste0("homogeneity-", method)); actual <- run(context, list(analysis = "ttest", vars = "y", group = "g", homogeneity = method))
  use <- complete.cases(sample[c("y", "g")]); y <- sample$y[use]; g <- droplevels(sample$g[use])
  for (level in levels(g)) shapiro(row(actual, "Shapiro-Wilk", "y", group = level), y[g == level], paste("Group", level))
  label <- c(levene = "Levene (median)", bartlett = "Bartlett", fligner = "Fligner-Killeen", f = "F-test")[[method]]
  homogeneity(row(actual, label), y, g, method)
})
test("assumptions_ttest_paired_joint_cases", {
  context <- new_case("paired"); actual <- run(context, list(analysis = "ttest", x = "pre,x", y = "post,z", homogeneity = "none"))
  for (pair in list(c("pre", "post"), c("x", "z"))) {
    use <- complete.cases(sample[pair]); shapiro(row(actual, "Shapiro-Wilk", paste(pair, collapse = " - ")), sample[[pair[1]]][use] - sample[[pair[2]]][use])
    selection(actual, which(use), target = paste(pair, collapse = " - "))
  }
  check(length(actual$raw_meta$case_selection) == 2L, "Paired source selections absent")
})
test("assumptions_ttest_labels_and_user_missings", {
  data <- sample; data$y[4] <- 99; data$g <- ifelse(is.na(data$g), NA_real_, ifelse(data$g == "treatment", 2, 1))
  data$y <- haven::labelled_spss(data$y, labels = c(Missing = 99), na_values = 99, label = "Outcome label")
  data$g <- haven::labelled_spss(data$g, labels = c(Control = 1, Treatment = 2), label = "Condition label")
  actual <- run(new_case("labelled", data, "sav"), list(analysis = "ttest", vars = "y", group = "g", homogeneity = "all"))
  expected <- as.numeric(data$y); expected[expected == 99] <- NA; groups <- as.numeric(data$g)
  for (group in c(1, 2)) shapiro(row(actual, "Shapiro-Wilk", "y", group = as.character(group)), expected[which(groups == group)])
  check(grepl("Outcome label", jsonlite::toJSON(actual$meta)), "Variable labels omitted from diagnostic metadata")
  check(!grepl("<external>|/cases/", actual$markdown) || !grepl(normalizePath(dirname(dirname(actual$path))), actual$markdown, fixed = TRUE), "Internal source path leaked")
})
for (kind in c("one", "paired", "independent", "regression", "anova", "sem", "mixed")) test(paste0("assumptions_auto_routes_", kind), {
  options <- switch(kind, one = tt, paired = list(analysis = "ttest", x = "pre", y = "post"),
    independent = list(analysis = "ttest", vars = "y", group = "g"), regression = list(analysis = "regression", dv = "y", ivs = "x,z"),
    anova = list(analysis = "anova", dv = "y", between = "a,b"), sem = sem_opts, mixed = mm)
  options$analysis <- "auto"
  context <- new_case(paste0("auto-", kind), if (kind == "sem") lavaan::HolzingerSwineford1939 else if (kind == "mixed") lme4::sleepstudy else sample)
  actual <- run(context, options)
  expected <- switch(kind, one = "ttest", paired = "ttest", independent = "ttest", mixed = "mixed_models", kind)
  check(all(actual$rows$analysis_type == expected), "Automatic family routing differs")
})
for (kind in c("constant", "too-small", "over-limit")) test(paste0("assumptions_shapiro_", kind, "_availability"), {
  data <- data.frame(y = switch(kind, constant = rep(4, 12), `too-small` = c(1, 3), `over-limit` = seq_len(12)))
  actual <- run(new_case(paste0("shapiro-", kind), data), modifyList(tt, list(`max-shapiro-n` = if (kind == "constant") 50L else 10L)))
  x <- row(actual, "Shapiro-Wilk"); check(x$status == if (kind == "constant") "unavailable" else "skipped", "Invalid Shapiro status")
  check(is.na(x$p) && is.na(x$statistic) && nzchar(x$note), "Unavailable Shapiro fabricated values or hid reason")
})

test("assumptions_anova_between_crossed_groups", {
  actual <- run(new_case("between-crossed"), list(analysis = "anova", dv = "y", between = "a,b", homogeneity = "all"))
  use <- complete.cases(sample[c("y", "a", "b")]); data <- sample[use, ]; g <- interaction(data$a, data$b, drop = TRUE, sep = ":")
  for (group in levels(g)) shapiro(row(actual, "Shapiro-Wilk", "y", group = group), data$y[g == group])
  for (method in c("levene", "bartlett", "fligner")) {
    label <- c(levene = "Levene (median)", bartlett = "Bartlett", fligner = "Fligner-Killeen")[[method]]
    homogeneity(row(actual, label), data$y, g, method)
  }
  check(row(actual, "F-test")$status == "unavailable", "Two-group F-test fabricated for six cells")
})
for (mixed in c(FALSE, TRUE)) test(paste0("assumptions_anova_", if (mixed) "mixed" else "within", if (!mixed) "_smoke"), {
  vars <- c("pre", "mid", "post"); between <- if (mixed) c("a", "b") else character()
  options <- list(analysis = "anova", within = paste(vars, collapse = ","), homogeneity = if (mixed) "all" else "none")
  if (mixed) options$between <- paste(between, collapse = ",")
  context <- new_case(if (mixed) "mixed-wide" else "within-wide"); actual <- run(context, options)
  use <- complete.cases(sample[c(vars, between)]); data <- sample[use, ]
  fit <- if (mixed) lm(cbind(pre, mid, post) ~ a * b, data) else lm(cbind(pre, mid, post) ~ 1, data)
  expected <- stats::mauchly.test(fit, X = ~1)
  near(row(actual, "Mauchly")$statistic, expected$statistic, "Projected Mauchly W")
  near(row(actual, "Mauchly")$p, expected$p.value, "Projected Mauchly p", probability = TRUE)
  near(row(actual, "Mauchly")$n, sum(use), "Repeated complete N", tolerance = 0)
  selection(actual, which(use))
  if (!mixed) for (var in vars) shapiro(row(actual, "Shapiro-Wilk", var), data[[var]])
  replay(context, actual)
})
test("assumptions_anova_two_measurements_no_sphericity_claim", {
  actual <- run(new_case("two-measures"), list(analysis = "anova", within = "pre,post", homogeneity = "none"))
  mauchly <- actual$rows[actual$rows$test == "Mauchly", , drop = FALSE]
  check(!nrow(mauchly) || all(mauchly$status != "available"), "Two repeated measurements falsely tested for sphericity")
})
test("assumptions_anova_subject_identity", {
  data <- sample; data$id[5] <- NA
  actual <- run(new_case("subject-identity", data), list(analysis = "anova", within = "pre,mid,post", `subject-id` = "id", homogeneity = "none"))
  use <- complete.cases(data[c("pre", "mid", "post", "id")]); fit <- lm(cbind(pre, mid, post) ~ 1, data[use, ])
  near(row(actual, "Mauchly")$statistic, stats::mauchly.test(fit, X = ~1)$statistic, "Subject-ID complete Mauchly")
  near(row(actual, "Mauchly")$n, sum(use), "Subject ID excludes missing ID", tolerance = 0)
})
test("assumptions_anova_tuple_labels_do_not_merge_cells", {
  data <- sample[!is.na(sample$y), ]; count <- nrow(data)
  data$a <- factor(rep(c("A:B", "A"), length.out = count)); data$b <- factor(rep(c("C", "B:C"), length.out = count))
  actual <- run(new_case("tuple-collision", data), list(analysis = "anova", dv = "y", between = "a,b", homogeneity = "levene"))
  groups <- actual$raw_meta$case_selection[[1]]$grouping
  check(length(groups) == 2L && length(unique(vapply(groups, function(x) x$level, character(1)))) == 2L, "Distinct factor tuples collapsed")
  for (group in groups) {
    indices <- as.integer(unlist(group$source_rows)); shapiro(row(actual, "Shapiro-Wilk", "y", group = group$level), data$y[indices])
    check(length(unique(paste(as.integer(data$a[indices]), as.integer(data$b[indices]), sep = ":"))) == 1L, "Cell mixes source tuples")
  }
})
test("assumptions_ttest_nearby_numeric_groups_remain_distinct", {
  data <- sample; data$g <- rep(c(1, 1 + 1e-15), length.out = nrow(data))
  actual <- run(new_case("nearby-groups", data), list(analysis = "ttest", vars = "y", group = "g", homogeneity = "levene"))
  groups <- actual$raw_meta$case_selection[[1]]$grouping
  check(length(groups) == 2L, "Nearby numeric category identities collapsed")
  for (group in groups) {
    indices <- as.integer(unlist(group$source_rows)); check(length(unique(data$g[indices])) == 1L, "Group contains different numeric identities")
    shapiro(row(actual, "Shapiro-Wilk", "y", group = group$level), data$y[indices])
  }
})
test("assumptions_anova_singular_covariance_unavailable", {
  data <- sample; data$mid <- data$pre; data$post <- data$pre
  actual <- run(new_case("singular-sphericity", data), list(analysis = "anova", within = "pre,mid,post", homogeneity = "none"))
  x <- row(actual, "Mauchly"); check(x$status == "unavailable" && is.na(x$p) && x$decision == "", "Singular covariance produced sphericity success")
})

regression_reference <- function(actual, data, predictors, block = "Block 1", alpha = .05, z = 3, multiplier = 4) {
  form <- reformulate(predictors, "y"); fit <- lm(form, data, na.action = na.fail); residual <- residuals(fit); design <- model.matrix(fit)
  model_meta <- Filter(function(x) identical(x$model, block), actual$raw_meta$models)
  check(length(model_meta) == 1L, "Missing independently fitted model metadata")
  model_meta <- model_meta[[1L]]
  near(model_meta$residuals, residual, "Model residual vector")
  near(model_meta$fitted, fitted(fit), "Model fitted vector")
  near(model_meta$rank, fit$rank, "Actual model rank", tolerance = 0)
  near(model_meta$residual_df, df.residual(fit), "Actual residual df", tolerance = 0)
  for (kind in c("outliers", "influence")) {
    reference <- if (kind == "outliers") rstandard(fit) else cooks.distance(fit)
    threshold <- if (kind == "outliers") z else multiplier / nobs(fit)
    flagged <- if (kind == "outliers") abs(reference) > threshold else reference > threshold
    record <- model_meta$residual_diagnostics[[kind]]
    near(record$source_rows, as.integer(rownames(data)), paste(kind, "source identities"), tolerance = 0)
    near(record$values, reference, paste(kind, "all case values"))
    near(record$threshold, threshold, paste(kind, "threshold")); near(record$count, sum(flagged), paste(kind, "count"), tolerance = 0)
    if (any(flagged)) near(record$flagged_source_rows, as.integer(rownames(data))[flagged], paste(kind, "flagged source identities"), tolerance = 0)
    else check(!length(record$flagged_source_rows), "Invented flagged source rows")
  }
  shapiro(row(actual, "Shapiro-Wilk", model = block), residual, block)
  bp <- lm(I(residual^2) ~ design[, -1, drop = FALSE]); r2 <- summary(bp)$r.squared
  aux_rank <- qr(model.matrix(bp))$rank - 1L; statistic <- nrow(data) * r2
  bprow <- row(actual, "Breusch-Pagan", model = block)
  near(bprow$statistic, statistic, "Studentized BP nR2"); near(bprow$df1, aux_rank, "BP auxiliary rank")
  near(bprow$p, pchisq(statistic, aux_rank, lower.tail = FALSE), "BP p", probability = TRUE)
  dw <- row(actual, "Durbin-Watson", model = block)
  near(dw$statistic, sum(diff(residual)^2) / sum(residual^2), "DW source order")
  check(dw$status == "available" && is.na(dw$p), "DW statistic without p marked unavailable or fabricated p")
  outliers <- row(actual, "Std. residuals", model = block); influence <- row(actual, "Cook's distance", model = block)
  near(outliers$value, max(abs(rstandard(fit))), "Maximum standardized residual")
  near(influence$value, max(cooks.distance(fit)), "Maximum Cook distance")
  check(outliers$decision == if (any(abs(rstandard(fit)) > z)) "flag" else "ok", "Outlier threshold decision differs")
  check(influence$decision == if (any(cooks.distance(fit) > multiplier / nrow(data))) "flag" else "ok", "Cook threshold decision differs")
  X <- design[, colnames(design) != "(Intercept)", drop = FALSE]
  expected_vif <- if (ncol(X) == 1) setNames(1, colnames(X)) else diag(solve(cor(X)))
  for (term in names(expected_vif)) near(row(actual, "VIF", term, block)$value, expected_vif[[term]], paste("Design-column VIF", term))
  for (pred in predictors[vapply(data[predictors], is.numeric, logical(1))]) {
    added <- update(form, paste0(". ~ . + I(`", pred, "`^2)")); enlarged <- lm(added, data)
    expected <- anova(fit, enlarged)
    x <- actual$rows[actual$rows$assumption == "Linearity" & actual$rows$model == block & actual$rows$target == pred, , drop = FALSE]
    check(nrow(x) == 1L && x$status == "available", "Quadratic departure diagnostic missing")
    near(x$statistic, expected$F[2], "Quadratic nested F"); near(x$p, expected$`Pr(>F)`[2], "Quadratic nested p", probability = TRUE)
  }
}
test("assumptions_regression_joint_hierarchical_smoke", {
  context <- new_case("joint-blocks"); actual <- run(context, list(analysis = "regression", dv = "y", blocks = "x;z", `outlier-z` = 2, `cook-multiplier` = 2))
  use <- complete.cases(sample[c("y", "x", "z")]); data <- sample[use, ]
  regression_reference(actual, data, "x", "Block 1", z = 2, multiplier = 2)
  regression_reference(actual, data, c("x", "z"), "Block 2", z = 2, multiplier = 2)
  check(all(actual$rows$n == sum(use)), "Hierarchical models used different complete cases")
  for (block in c("Block 1", "Block 2")) selection(actual, which(use), model = block)
  replay(context, actual)
})
test("assumptions_regression_factor_design_vif", {
  actual <- run(new_case("factor-design"), list(analysis = "regression", dv = "y", ivs = "x,b"))
  use <- complete.cases(sample[c("y", "x", "b")]); regression_reference(actual, sample[use, ], c("x", "b"))
  linear <- actual$rows[actual$rows$assumption == "Linearity" & actual$rows$target == "b", , drop = FALSE]
  check(nrow(linear) == 1L && linear$status == "unavailable", "Requested categorical linearity silently absent or passed")
})
test("assumptions_regression_sav_labels_remain_numeric", {
  data <- sample; data$y[4] <- 99
  data$y <- haven::labelled_spss(data$y, labels = c(Missing = 99), na_values = 99, label = "Outcome score")
  data$x <- haven::labelled(as.numeric(data$x), labels = c(Reference = 0), label = "Continuous predictor")
  actual <- run(new_case("regression-labels", data, "sav"), list(analysis = "regression", dv = "y", ivs = "x,z"))
  expected <- sample; expected$y[4] <- NA; expected <- expected[complete.cases(expected[c("y", "x", "z")]), ]
  regression_reference(actual, expected, c("x", "z"))
  check(grepl("Continuous predictor", jsonlite::toJSON(actual$meta)), "Regression input labels absent")
})
test("assumptions_regression_nonlinearity_detected", {
  x <- seq(-2, 2, length.out = 71); data <- data.frame(x = x, y = 2 + .4 * x + 3 * x^2 + sin(seq_along(x)) * .15)
  actual <- run(new_case("nonlinearity", data), list(analysis = "regression", dv = "y", ivs = "x"))
  regression_reference(actual, data, "x")
  check(actual$rows$p[actual$rows$assumption == "Linearity"] < 1e-20, "Strong quadratic departure escaped diagnostic")
})
test("assumptions_regression_rank_deficient_bp", {
  data <- sample; data$alias <- 2 * data$x
  actual <- run(new_case("rank-deficient", data), list(analysis = "regression", dv = "y", ivs = "x,alias", linearity = FALSE))
  use <- complete.cases(data[c("y", "x", "alias")]); fit <- lm(y ~ x + alias, data[use, ])
  aux <- lm(I(residuals(fit)^2) ~ x, data[use, ]); stat <- nobs(fit) * summary(aux)$r.squared
  near(row(actual, "Breusch-Pagan")$df1, 1, "Alias BP rank", tolerance = 0)
  near(row(actual, "Breusch-Pagan")$p, pchisq(stat, 1, lower.tail = FALSE), "Alias BP p", probability = TRUE)
  vifs <- actual$rows[actual$rows$test == "VIF", , drop = FALSE]
  check(all(vifs$status == "unavailable" | vifs$decision %in% c("high", "flag")), "Aliased design falsely passed collinearity")
})
test("assumptions_regression_constant_predictor_unavailable", {
  data <- sample; data$constant <- 4
  actual <- run(new_case("constant-predictor", data), list(analysis = "regression", dv = "y", ivs = "x,constant"))
  curvature <- row(actual, "Quadratic added-term F", "constant"); vif <- row(actual, "VIF", "constant")
  check(curvature$status == "unavailable" && is.na(curvature$p), "Constant curvature falsely estimated")
  check(vif$status == "unavailable" && is.na(vif$value), "Constant predictor assigned finite or infinite VIF")
})
test("assumptions_regression_saturated_diagnostics_not_false_success", {
  data <- data.frame(y = c(1, 4, 2), x = 1:3, z = c(1, 0, 1))
  actual <- run(new_case("saturated", data), list(analysis = "regression", dv = "y", ivs = "x,z"))
  for (name in c("Std. residuals", "Cook's distance", "Breusch-Pagan")) {
    x <- row(actual, name); check(x$status == "unavailable" && x$decision == "", paste("Saturated model invented", name))
  }
})
test("assumptions_regression_perfect_fit_residual_checks_unavailable", {
  data <- data.frame(x = seq_len(24), y = 2 + 3 * seq_len(24))
  actual <- run(new_case("perfect-regression", data), list(analysis = "regression", dv = "y", ivs = "x"))
  for (name in c("Shapiro-Wilk", "Quadratic added-term F", "Breusch-Pagan", "Durbin-Watson", "Std. residuals", "Cook's distance")) {
    x <- row(actual, name); check(x$status == "unavailable" && x$decision == "", paste("Numerical-roundoff residuals used for", name))
  }
  near(row(actual, "VIF", "x")$value, 1, "Perfect fit retains independent design VIF", tolerance = 0)
})
for (flag in c("linearity", "homoscedasticity", "vif", "durbin-watson", "outliers", "influence")) test(paste0("assumptions_regression_disable_", flag), {
  options <- list(analysis = "regression", dv = "y", ivs = "x,z"); options[[flag]] <- FALSE
  actual <- run(new_case(paste0("disable-", flag)), options)
  assumption <- c(linearity = "Linearity", homoscedasticity = "Homoscedasticity", vif = "Multicollinearity", `durbin-watson` = "Independence", outliers = "Outliers", influence = "Influence")[[flag]]
  check(!any(actual$rows$assumption == assumption), "Disabled check remains in table")
  check(identical(actual$meta$requested[[gsub("-", "_", flag)]], FALSE), "Disabled request not retained")
})
test("assumptions_regression_thresholds_change_flags_not_estimates", {
  context <- new_case("thresholds"); base <- list(analysis = "regression", dv = "y", ivs = "x,z")
  ordinary <- run(context, base); adjusted <- run(context, modifyList(base, list(alpha = .9, `vif-warn` = .9, `vif-high` = 1, `outlier-z` = .2, `cook-multiplier` = .1)))
  for (name in c("statistic", "p", "value", "n")) near(adjusted$rows[[name]], ordinary$rows[[name]], paste("Threshold invariant", name))
  check(all(adjusted$rows$decision[adjusted$rows$test == "VIF"] == "high"), "VIF high threshold ignored")
  near(adjusted$request$options$alpha, .9, "Resolved alpha")
})

test("assumptions_mixed_bundle_smoke", {
  context <- new_case("mixed-bundle", lme4::sleepstudy); actual <- run(context, mm)
  fit <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy,
    control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000L)))
  shapiro(row(actual, "Shapiro-Wilk", "Residuals"), residuals(fit))
  near(actual$meta$included_rows, seq_len(nrow(lme4::sleepstudy)), "Mixed actual model cases", tolerance = 0)
  replay(context, actual)
})
test("assumptions_mixed_dharma_custom_seed_replay", {
  context <- new_case("dharma-seed", lme4::sleepstudy); actual <- run(context, modifyList(mm, list(dharma = TRUE, seed = 947L)))
  fit <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy,
    control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000L)))
  set.seed(947L)
  sim <- DHARMa::simulateResiduals(fit, plot = FALSE, seed = 947L, n = 250L, refit = FALSE)
  uniformity <- DHARMa::testUniformity(sim, plot = FALSE); dispersion <- DHARMa::testDispersion(sim, plot = FALSE)
  near(row(actual, "DHARMa uniformity")$p, uniformity$p.value, "Seeded DHARMa uniformity", probability = TRUE)
  near(row(actual, "DHARMa dispersion")$p, dispersion$p.value, "Seeded DHARMa dispersion", probability = TRUE)
  near(actual$meta$dharma$seed, 947, "Requested DHARMa seed", tolerance = 0)
  replay(context, actual)
})
test("assumptions_mixed_nested_random_term_matches_lme4", {
  context <- new_case("nested-random-term", lme4::sleepstudy)
  formula <- "Reaction ~ Days + I(Days^2) + (I(Days^2) | Subject)"
  actual <- run(context, modifyList(mm, list(formula = formula)))
  fit <- lme4::lmer(as.formula(formula), lme4::sleepstudy,
    control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000L)))
  shapiro(row(actual, "Shapiro-Wilk", "Residuals"), residuals(fit))
  near(actual$meta$included_rows, as.integer(rownames(model.frame(fit))), "Nested-term fitted source cases", tolerance = 0)
  replay(context, actual)
})
test("assumptions_sem_bundle_smoke", {
  context <- new_case("sem-bundle", lavaan::HolzingerSwineford1939); actual <- run(context, sem_opts)
  fit <- lavaan::cfa(sem_model, lavaan::HolzingerSwineford1939, estimator = "ML", missing = "listwise", se = "standard")
  near(actual$meta$fit$fit_status$n_parameters, lavaan::lavInspect(fit, "npar"), "SEM diagnostic parameter count", tolerance = 0)
  near(actual$meta$fit$case_selection$nobs, lavaan::lavInspect(fit, "nobs"), "SEM actual N", tolerance = 0)
  replay(context, actual)
})
test("assumptions_sem_model_file_frozen_replay", {
  context <- new_case("sem-file", lavaan::HolzingerSwineford1939); model_file <- file.path(context$base, "external-model.txt"); writeLines(sem_model, model_file)
  options <- sem_opts; options$model <- NULL; options$`model-file` <- model_file
  actual <- run(context, options); writeLines("THIS IS NO LONGER LAVAAN SYNTAX", model_file)
  check(grepl("visual =~", actual$request$design$model_syntax, fixed = TRUE), "Resolved model syntax not frozen")
  replay(context, actual)
})
for (pair in list(c("model", "paths"), c("model", "model-file"), c("paths", "model-file"))) test(paste0("assumptions_sem_rejects_conflicting_", paste(pair, collapse = "_")), {
  context <- new_case(paste0("conflicting-", paste(pair, collapse = "-")), lavaan::HolzingerSwineford1939)
  run(context, sem_opts)
  file <- file.path(context$base, "model.txt"); writeLines(sem_model, file)
  options <- sem_opts; options$model <- NULL
  for (key in pair) options[[key]] <- if (key == "model-file") file else sem_model
  failed(context, options)
})
test("assumptions_sem_explicit_syntax_overrides_builder_with_audit", {
  context <- new_case("sem-builder-precedence", lavaan::HolzingerSwineford1939)
  actual <- run(context, c(tokens(modifyList(sem_opts, list(factors = "IGNORED=not_a_variable"))), "--serial"))
  specification <- actual$meta$model_specification
  check(specification$model_source == "model", "Actual syntax source not recorded")
  check(identical(specification$unused_builder_inputs$factors, "IGNORED=not_a_variable"), "Unused builder inputs hidden")
  check(identical(specification$unused_builder_inputs$serial, "TRUE"), "Implicit Boolean unused-input audit not normalized")
  check(length(actual$result$warnings) > 0L && grepl("builder|unused|overrid", actual$markdown, ignore.case = TRUE), "Syntax precedence not disclosed in warnings and report")
  replay(context, actual)
})
for (std in c("none", "std.lv")) test(paste0("assumptions_sem_effective_loading_screen_", std), {
  context <- new_case(paste0("sem-reporting-", std), lavaan::HolzingerSwineford1939)
  actual <- run(context, modifyList(sem_opts, list(std = std, ci = "bca.simple")))
  fit <- lavaan::cfa(sem_model, lavaan::HolzingerSwineford1939, estimator = "ML", missing = "listwise", se = "standard")
  solution <- lavaan::standardizedSolution(fit, type = "std.all")
  count <- sum(abs(solution$est.std[solution$op == "=~"]) > 1, na.rm = TRUE)
  near(row(actual, "Std. loading > 1")$value, count, "Fully standardized loading screen count", tolerance = 0)
  reporting <- actual$meta$reporting
  check(reporting$standardization_requested == std && reporting$loading_screen_effective == "std.all", "Requested and diagnostic-effective standardization conflated")
  check(reporting$confidence_intervals == "not_computed" && nzchar(reporting$ci_requested), "Diagnostic CLI falsely implies confidence intervals were computed")
  check(grepl("std.all", actual$markdown, fixed = TRUE) && grepl("confidence|interval", actual$markdown, ignore.case = TRUE), "Reporting limitation absent from Markdown")
  replay(context, actual)
})
test("assumptions_sem_rejects_single_observed_explicit_group", {
  data <- lavaan::HolzingerSwineford1939; data$school <- factor(rep("Only", nrow(data)))
  context <- new_case("sem-single-group", data); run(context, sem_opts)
  failed(context, modifyList(sem_opts, list(group = "school")))
})
test("assumptions_sem_adjacent_numeric_ordinal_codes_preserved", {
  data <- lavaan::HolzingerSwineford1939[paste0("x", 1:9)]
  for (name in names(data)) data[[name]] <- c(1, 1 + 1e-15, 1 + 2e-15)[as.integer(cut(data[[name]], unique(quantile(data[[name]], c(0, .34, .67, 1))), include.lowest = TRUE))]
  context <- new_case("sem-adjacent-ordinal", data)
  actual <- run(context, modifyList(sem_opts, list(ordered = paste(names(data), collapse = ","), estimator = "WLSMV", se = "robust")))
  expected <- data
  for (name in names(data)) expected[[name]] <- ordered(sprintf("%.17g", data[[name]]), levels = sprintf("%.17g", sort(unique(data[[name]]))))
  fit <- lavaan::cfa(sem_model, expected, ordered = names(expected), estimator = "WLSMV", se = "robust", missing = "listwise")
  for (name in names(data)) {
    record <- Filter(function(x) identical(x$variable, name), actual$raw_meta$ordered)
    check(length(record) == 1L && identical(unlist(record[[1L]]$levels, use.names = FALSE), levels(expected[[name]])), "Adjacent ordinal codes collapsed or changed order")
  }
  near(actual$meta$fit$case_selection$nobs, lavaan::lavInspect(fit, "nobs"), "Adjacent ordinal fit N", tolerance = 0)
  near(actual$meta$fit$case_selection$included_source_rows, lavaan::lavInspect(fit, "case.idx"), "Adjacent ordinal source cases", tolerance = 0)
  replay(context, actual)
})
test("assumptions_sem_bootstrap_seed_replay", {
  context <- new_case("sem-bootstrap", lavaan::HolzingerSwineford1939)
  actual <- run(context, modifyList(sem_opts, list(bootstrap = TRUE, `bootstrap-samples` = 23L, se = "bootstrap", seed = 883L)))
  check(actual$meta$fit$bootstrap$enabled, "Bootstrap diagnostic refit not enabled")
  near(actual$meta$fit$bootstrap$requested, 23, "Requested bootstrap draws", tolerance = 0)
  replay(context, actual)
})
for (family in c("mixed_models", "sem")) test(paste0("assumptions_inherited_", family, "_configuration_replay"), {
  context <- new_case(paste0("inherited-config-", family), if (family == "sem") lavaan::HolzingerSwineford1939 else lme4::sleepstudy)
  if (family == "mixed_models") {
    yaml::write_yaml(list(modules = list(mixed_models = list(reml = FALSE, optimizer = "nloptwrap", maxfun = 11777L))), config)
    options <- mm; options$reml <- options$optimizer <- options$maxfun <- NULL
  } else {
    yaml::write_yaml(list(modules = list(sem = list(estimator = "MLR", missing = "fiml", se = "robust", seed = 1771L))), config)
    options <- sem_opts; options$estimator <- options$missing <- options$se <- options$seed <- NULL
  }
  actual <- run(context, options)
  check(is.list(actual$request$configuration$modules[[family]]), "Inherited model-family defaults absent from request")
  check(!"research_academia" %in% names(actual$request$configuration$modules), "Unrelated module configuration copied")
  if (family == "mixed_models") {
    check(identical(actual$meta$reml_effective, FALSE), "Inherited ML setting ignored")
    yaml::write_yaml(list(modules = list(mixed_models = list(reml = TRUE, optimizer = "bobyqa", maxfun = 3L))), config)
  } else {
    check(actual$meta$fit$inference$effective$estimator.orig == "MLR", "Inherited SEM estimator ignored")
    yaml::write_yaml(list(modules = list(sem = list(estimator = "ML", missing = "listwise", se = "standard", seed = 2L))), config)
  }
  replay(context, actual)
})

test("assumptions_log_false_keeps_mandatory_bundle", {
  context <- new_case("log-false"); actual <- run(context, modifyList(tt, list(log = FALSE)))
  log <- file.path(context$directory, "analysis_log.jsonl")
  check(!file.exists(log) || !length(readLines(log)), "--log FALSE wrote legacy entries")
  check(file.exists(file.path(context$directory, "report_canonical.md")), "Canonical Markdown disappeared with log FALSE")
  replay(context, actual)
})
test("assumptions_legacy_outputs_disabled_retains_raw_results", {
  yaml::write_yaml(list(logging = list(include_outputs = FALSE)), config)
  actual <- run(new_case("outputs-disabled"), tt); shapiro(row(actual, "Shapiro-Wilk", "y"), sample$y)
})
test("assumptions_prompt_file_interaction_replays_noninteractively", {
  context <- new_case("interactive"); prompt_file <- file.path(context$base, "answers.txt")
  writeLines(c("rds", context$input, "ttest", "one-sample", "y", "shapiro", "none", ".05", "7", "", "Inspect this outcome.", "TRUE"), prompt_file)
  previous <- Sys.getenv("NLSS_PROMPT_FILE", unset = NA_character_)
  actual <- tryCatch({ Sys.setenv(NLSS_PROMPT_FILE = prompt_file); run(context, c("--interactive"), source = FALSE) },
    finally = if (is.na(previous)) Sys.unsetenv("NLSS_PROMPT_FILE") else Sys.setenv(NLSS_PROMPT_FILE = previous))
  shapiro(row(actual, "Shapiro-Wilk", "y"), sample$y)
  replay(context, actual)
})
test("assumptions_prompt_privacy_and_urls", {
  context <- new_case("privacy"); prompt <- "Check /private/research/person.csv and C:\\private\\trial.sav; use https://example.org/study/path."
  actual <- run(context, modifyList(tt, list(`user-prompt` = prompt)))
  files <- c(actual$request_path, file.path(actual$path, "result.json"), file.path(context$directory, c("analysis_log.jsonl", "report_canonical.md")))
  text <- paste(vapply(files, read_text, character(1)), collapse = "\n")
  check(!grepl("/private/research|private.{1,4}trial|/cases/privacy/", text), "External paths leaked from prompt/source")
  check(grepl("<external>/person.csv", text, fixed = TRUE) && grepl("<external>/trial.sav", text, fixed = TRUE), "Masked prompt context missing")
  check(grepl("https://example.org/study/path", text, fixed = TRUE), "URL was corrupted as a filesystem path")
})
test("assumptions_frozen_configuration_template_and_input", {
  context <- new_case("frozen-all"); template <- file.path(context$base, "template.md")
  writeLines(c("# FROZEN_ASSUMPTIONS_TEMPLATE", "{{table_body}}", "{{note_body}}", "{{narrative}}"), template)
  yaml::write_yaml(list(defaults = list(digits = 7L), modules = list(assumptions = list(alpha = .13))), config)
  actual <- run(context, modifyList(tt, list(template = template)))
  check(grepl("FROZEN_ASSUMPTIONS_TEMPLATE", actual$markdown), "Custom template ignored")
  saveRDS(data.frame(y = rep(55, 20)), context$input)
  arrow::write_parquet(data.frame(y = c(1, 3, 9)), file.path(context$directory, "sample.parquet"))
  writeLines("CHANGED_TEMPLATE", template); yaml::write_yaml(list(modules = list(assumptions = list(alpha = .9, normality = "none"))), config)
  replay(context, actual)
})
test("assumptions_publication_failure_transaction", {
  context <- new_case("publication-lock"); run(context, tt)
  lock <- file.path(context$project, ".publication-lock"); dir.create(lock)
  failed(context, tt)
  check(dir.exists(lock), "Failed publication removed someone else's lock")
})
test("assumptions_analysis_lock_contention", {
  context <- new_case("analysis-lock"); run(context, tt)
  lock <- file.path(context$directory, ".analysis-lock"); dir.create(lock)
  failed(context, tt); check(dir.exists(lock), "Contending analysis removed another lock")
})
test("assumptions_runtime_failure_is_not_completed", {
  context <- new_case("fit-failure", data.frame(y = 1:5, x = factor(rep("only", 5))))
  run(context, tt)
  failed(context, list(analysis = "regression", dv = "y", ivs = "x"))
})
for (kind in c("request", "output", "template", "snapshot", "dictionary")) test(paste0("assumptions_replay_rejects_tampered_", kind), {
  context <- new_case(paste0("tamper-", kind)); actual <- run(context, tt)
  target <- switch(kind, request = actual$request_path, output = file.path(actual$path, "output.md"),
    template = file.path(actual$path, actual$request$templates[[1]]$path), snapshot = file.path(context$project, actual$request$dataset$snapshot_path),
    dictionary = file.path(context$project, actual$request$dataset$dictionary_path))
  con <- file(target, "ab"); writeBin(charToRaw("\nTAMPER"), con); close(con)
  failed(context, c("--request", actual$request_path), module = "replay_run", source = FALSE)
})
for (kind in c("code", "environment", "result-identity")) test(paste0("assumptions_replay_rejects_", kind), {
  context <- new_case(paste0("association-", kind)); actual <- run(context, tt)
  request <- raw_json(actual$request_path); result_file <- file.path(actual$path, "result.json"); result <- raw_json(result_file)
  if (kind == "code") request$code_sha256 <- paste(rep("0", 64), collapse = "")
  if (kind == "environment") request$environment$packages$stats <- "0.0.0"
  if (kind == "result-identity") result$module <- "power"
  write_json(request, actual$request_path); result$artifacts$request$sha256 <- sha(actual$request_path); write_json(result, result_file)
  failed(context, c("--request", actual$request_path), module = "replay_run", source = FALSE)
})

invalid <- list(
  unknown = c("--typo", "TRUE"), duplicate = c("--alpha", ".1", "--alpha", ".2"), boolean = c("--outliers", "perhaps"),
  normality = c("--normality", "invented"), homogeneity = c("--homogeneity", "invented"), analysis = c("--analysis", "invented"),
  alpha_zero = c("--alpha", "0"), alpha_one = c("--alpha", "1"), alpha_nan = c("--alpha", "NaN"),
  digits_fraction = c("--digits", "2.5"), digits_negative = c("--digits", "-1"),
  shapiro_fraction = c("--max-shapiro-n", "3.2"), shapiro_zero = c("--max-shapiro-n", "0"),
  vif_order = c("--vif-warn", "10", "--vif-high", "5"), outlier_zero = c("--outlier-z", "0"),
  cook_negative = c("--cook-multiplier", "-1"), seed_negative = c("--seed", "-1"), seed_fraction = c("--seed", "1.5"),
  wrong_dv = c("--analysis", "regression", "--dv", "unknown", "--ivs", "x"),
  wrong_variable = c("--vars", "unknown"), paired_lengths = c("--x", "x,z", "--y", "y"),
  paired_missing_y = c("--x", "x"), group_many_levels = c("--group", "b"),
  wrong_family_model = c("--model", "y ~ x"), wrong_family_formula = c("--formula", "y ~ x + (1|id)"),
  wrong_family_between = c("--between", "a"), wrong_family_sem_toggle = c("--mardia", "TRUE"),
  multiple_sources = c("--csv", "not-a-file.csv"), dataset_selector_only = c("--dataset-name", "phantom"),
  wrong_reader_flag = c("--csv-decimal", ","), rdata_df_without_rdata = c("--df", "unused"))
for (name in names(invalid)) test(paste0("assumptions_rejects_", name), {
  context <- new_case(paste0("invalid-", name)); run(context, tt)
  extra <- invalid[[name]]
  base <- c("--analysis", "ttest", "--vars", "y")
  # Invalid requests replace rather than accidentally duplicate a valid selector,
  # except the case explicitly testing duplicate-option rejection.
  if ("--analysis" %in% extra) base <- character()
  if ("--vars" %in% extra) base <- base[-which(base %in% c("--vars", "y"))]
  if ("--x" %in% extra) base <- base[!base %in% c("--vars", "y")]
  failed(context, c(base, extra), source = name != "dataset_selector_only")
})
for (family in c("mixed_models", "sem")) for (bad_seed in c(-1, 1.5)) test(paste0("assumptions_rejects_", family, "_seed_", bad_seed), {
  context <- new_case(paste0("invalid-seed-", family, "-", bad_seed), if (family == "sem") lavaan::HolzingerSwineford1939 else lme4::sleepstudy)
  options <- if (family == "sem") sem_opts else mm
  run(context, options); options$seed <- bad_seed
  failed(context, options)
})
for (family in c("ttest", "anova", "regression", "mixed_models", "sem")) test(paste0("assumptions_rejects_selected_infinity_", family), {
  data <- if (family == "mixed_models") lme4::sleepstudy else if (family == "sem") lavaan::HolzingerSwineford1939 else sample
  key <- switch(family, mixed_models = "Reaction", sem = "x1", "y"); data[[key]][1] <- Inf
  options <- switch(family, ttest = tt, anova = list(analysis = "anova", dv = "y", between = "a"),
    regression = list(analysis = "regression", dv = "y", ivs = "x,z"), mixed_models = mm, sem = sem_opts)
  context <- new_case(paste0("infinity-", family), data)
  run_cli(context, c(context$source, tokens(options)), failure = TRUE)
})
test("assumptions_unused_infinity_does_not_poison_selected_data", {
  data <- sample; data$unused <- Inf; actual <- run(new_case("unused-infinity", data), tt)
  shapiro(row(actual, "Shapiro-Wilk", "y"), data$y)
})
test("assumptions_duplicate_repeated_subjects_rejected", {
  data <- sample; data$id[2] <- data$id[1]; context <- new_case("duplicate-subjects", data)
  run(context, tt); failed(context, list(analysis = "anova", within = "pre,mid,post", `subject-id` = "id"))
})

if (!length(results)) stop("No tests selected")
summary <- file.path(work, "results.json")
write_json(list(schema_version = 1L, suite = "phase2-assumptions", execution_contract = "resolved-request-v1", test_pattern = pattern,
  source_sha256 = list(assumptions = sha(file.path(repo, "scripts/R/assumptions.R")), runner = sha(script)),
  numeric_checks = numeric_checks, tests = results,
  environment = list(r = R.version.string, packages = as.list(vapply(required, function(p) as.character(packageVersion(p)), character(1))))), summary)
passed <- sum(vapply(results, function(x) x$passed, logical(1)))
cat(sprintf("Phase 2 assumptions: %d/%d cases passed; %d numeric comparisons. Results: %s\n", passed, length(results), numeric_checks, summary))
# Prune only the runner's own timestamped output directories for default roots.
if (!nzchar(forced) && keep > 0L) {
  dirs <- sort(list.dirs(collection, recursive = FALSE, full.names = TRUE), decreasing = TRUE)
  dirs <- dirs[grepl("^run-[0-9]{14}-[0-9]+$", basename(dirs))]
  if (length(dirs) > keep) for (path in setdiff(tail(dirs, -keep), work)) unlink(path, recursive = TRUE)
}
if (passed != length(results)) quit(status = 1L)
