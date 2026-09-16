#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Public-CLI acceptance. Numeric oracles use stats/car/emmeans, never NLSS scientific helpers.
args <- commandArgs(TRUE)
script <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])
repo <- normalizePath(file.path(dirname(script[1]), "../.."), winslash = "/")
if ("--help" %in% args) {
  cat("Usage: Rscript tests/phase2/run_design_tests.R [--root PATH] [--keep N] [--modules anova,nonparametric] [--match REGEX]\n",
      "Honors NLSS_TEST_ROOT, NLSS_KEEP_RUNS and NLSS_TESTS_CONFIG; uses private configuration and no network.\n", sep = "")
  quit(status = 0)
}
if (length(args) %% 2L || (length(args) && any(!args[seq(1L, length(args), 2L)] %in% c("--root", "--keep", "--modules", "--match")))) stop("Unknown/incomplete runner option")
arg <- function(name, fallback) {
  i <- which(args == name)
  if (length(i) > 1L) stop("Repeated runner option: ", name)
  if (length(i)) args[i + 1L] else fallback
}
modules <- strsplit(arg("--modules", "anova,nonparametric"), ",", fixed = TRUE)[[1]]
if (!length(modules) || anyDuplicated(modules) || any(!modules %in% c("anova", "nonparametric"))) stop("Unsupported --modules selection")
required <- c("yaml", "jsonlite", "arrow", "digest", "haven", if ("anova" %in% modules) c("car", "emmeans"))
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
work <- file.path(run_root, "phase2-design", paste0("run-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid()))
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
    if (identical(request$module, "nonparametric")) check_rank_design(request_path, context$data)
  }
  list(path = added, request_path = request_path, request = request, result = result, markdown = if (failure) NULL else text_file(output), command = command)
}
replay <- function(context, bundle) {
  out <- new_run(context, "replay_run", c("--request", bundle$request_path), source = FALSE)
  check(identical(out$result$results, bundle$result$results), "Replay changed raw statistical results")
  check(identical(out$markdown, bundle$markdown), "Replay changed Markdown bytes")
  out
}
near_probability <- function(actual, expected, label) {
  actual <- as.numeric(actual); expected <- as.numeric(expected)
  check(length(actual) == length(expected) && length(expected) > 0L, paste(label, "length differs"))
  check(identical(is.na(actual), is.na(expected)), paste(label, "missing values differ"))
  valid <- !is.na(expected)
  check(all(is.finite(actual[valid])) && all(abs(actual[valid] - expected[valid]) <= pmax(1e-300, abs(expected[valid]) * 2e-7)), paste(label, "differs from independent probability reference"))
  numeric_checks <<- numeric_checks + length(expected)
}
check_row <- function(row, reference, label) for (field in names(reference)) {
  if (grepl("(^p$|^p_|_p$)", field)) near_probability(row[[field]], reference[[field]], paste(label, field))
  else near(row[[field]], reference[[field]], paste(label, field), tolerance = 3e-8)
}
interval <- function(row, reference, label) {
  check_row(row, list(ci_low = reference[1], ci_high = reference[2]), label)
  for (i in 1:2) if (is.infinite(reference[i])) {
    field <- paste0(c("ci_low", "ci_high")[i], "_status")
    check(identical(row[[field]], if (reference[i] < 0) "negative_infinity" else "positive_infinity"), paste(label, "lost unbounded interval status"))
  }
}
failed <- function(context, module, options) {
  paths <- file.path(context$dataset, c("report_canonical.md", "analysis_log.jsonl"))
  existing <- paths[file.exists(paths)]; before <- vapply(existing, sha, character(1))
  out <- new_run(context, module, options, failure = TRUE)
  check(identical(before, vapply(existing, sha, character(1))), "Failed analysis changed published report/log")
  check(!dir.exists(file.path(context$dataset, ".analysis-lock")), "Failed analysis retained its lock")
  run_module("replay_run", c("--request", out$request_path), failure = TRUE)
  invisible(out)
}
override_options <- function(base, override) {
  names <- base[seq(1L, length(base), 2L)]
  keep <- !names %in% override[seq(1L, length(override), 2L)]
  c(as.vector(rbind(names[keep], base[seq(2L, length(base), 2L)][keep])), override)
}

# Independent fixtures deliberately differ from the historical golden dataset:
# nonalphabetic factor order, nonmonotonic IDs, unequal cells and distinct missings.
set.seed(624180)
design_data <- data.frame(group = factor(rep(c("C", "A", "B"), c(21, 18, 24)), levels = c("B", "C", "A", "unused")),
  sex = factor(rep(c("Y", "X", "X"), 21), levels = c("Y", "X")), cov = stats::rnorm(63))
design_data$score <- 3 + c(C = .1, A = .7, B = 1.1)[as.character(design_data$group)] + .6 * design_data$cov +
  .5 * (design_data$sex == "Y") + .8 * (design_data$sex == "Y" & design_data$group == "A") + stats::rnorm(63, sd = .85)
design_data$id <- sample(seq(101, 287, by = 3))
subject <- stats::rnorm(63)
design_data$pre <- 3 + subject + stats::rnorm(63, sd = .6)
design_data$mid <- 3.4 + .6 * subject + stats::rnorm(63, sd = 1.3)
design_data$post <- 4 + .2 * subject + .6 * (design_data$group == "A") + stats::rnorm(63, sd = .9)
design_data$score[c(4, 41)] <- NA; design_data$cov[c(9, 52)] <- NA
design_data$mid[c(3, 46)] <- NA; design_data$post[c(11, 59)] <- NA
rank_data <- data.frame(x = c(-3.1, -1.7, -.4, .8, 1.5, 2.2, 3.9, 4.1, 5.8, 6.7, 7.6, 8.4, NA),
  y = c(-2, -.2, .1, .5, 2.5, 2, 3.4, 4.3, 5.1, 6.1, 7.8, 8.6, 9))
rank_groups <- design_data
rank_groups$score <- round(rank_groups$score, 1)
clean_between <- function(data, covariates = FALSE, factorial = FALSE) {
  vars <- c("score", "group", if (factorial) "sex", if (covariates) "cov")
  out <- data[stats::complete.cases(data[vars]), , drop = FALSE]
  out$group <- droplevels(as.factor(out$group)); out$sex <- droplevels(as.factor(out$sex)); out
}
between_fit <- function(data, type = "I", covariates = FALSE, factorial = FALSE) {
  data <- clean_between(data, covariates, factorial)
  contrast <- if (type == "III") setNames(rep(list("contr.sum"), 1L + factorial), c("group", if (factorial) "sex")) else NULL
  form <- if (factorial && covariates) score ~ group * sex + cov else if (factorial) score ~ group * sex else if (covariates) score ~ group + cov else score ~ group
  stats::lm(form, data = data, contrasts = contrast)
}
anova_expected <- function(fit, type = "I") {
  table <- if (type == "I") stats::anova(fit) else car::Anova(fit, type = if (type == "II") 2 else 3)
  wanted <- !trimws(rownames(table)) %in% c("(Intercept)", "Residuals")
  table <- table[wanted, , drop = FALSE]
  response <- stats::model.response(stats::model.frame(fit))
  total <- sum((response - mean(response))^2); error <- sum(stats::residuals(fit)^2); mse <- error / stats::df.residual(fit)
  ss <- table$`Sum Sq`; d <- table$Df
  data.frame(term = trimws(rownames(table)), df1 = d, df2 = stats::df.residual(fit), ss = ss, ms = ss / d,
    f = table$`F value`, p = table$`Pr(>F)`, eta_sq = ss / total, partial_eta_sq = ss / (ss + error),
    omega_sq = (ss - d * mse) / (total + mse), partial_omega_sq = (ss - d * mse) / (ss + error + mse))
}
check_anova <- function(bundle, fit, type = "I") {
  expected <- anova_expected(fit, type); actual <- bundle$result$results$summary_df
  check(nrow(actual) == nrow(expected) && setequal(actual$term, expected$term), "ANOVA effect inventory differs")
  for (i in seq_len(nrow(expected))) check_row(actual[actual$term == expected$term[i], ], as.list(expected[i, setdiff(names(expected), "term")]), paste("ANOVA", type, expected$term[i]))
  near(bundle$request$design$included_rows, as.integer(rownames(stats::model.frame(fit))), "ANOVA included source rows", tolerance = 0)
  near(bundle$request$design$model_rank, fit$rank, "ANOVA design rank", tolerance = 0)
  check(identical(bundle$request$design$model_matrix_columns, colnames(stats::model.matrix(fit))), "ANOVA design matrix columns differ")
}
check_between_diagnostics <- function(bundle, fit, data, factorial = FALSE) {
  table <- bundle$result$results$assumptions_df
  sw <- stats::shapiro.test(stats::residuals(fit))
  check_row(table[table$test == "Shapiro-Wilk", ], list(statistic = unname(sw$statistic), p = sw$p.value), "Residual Shapiro")
  group <- if (factorial) interaction(data$group, data$sex, drop = TRUE) else data$group
  levene <- car::leveneTest(data$score, group, center = stats::median)
  check_row(table[table$test == "Levene (median)", ], list(statistic = levene$`F value`[1], df1 = levene$Df[1], df2 = levene$Df[2], p = levene$`Pr(>F)`[1]), "Levene")
  for (name in c("Bartlett", "Fligner-Killeen")) {
    value <- if (name == "Bartlett") stats::bartlett.test(data$score, group) else stats::fligner.test(data$score, group)
    check_row(table[table$test == name, ], list(statistic = unname(value$statistic), df1 = unname(value$parameter), p = value$p.value), name)
  }
}
repeated_reference <- function(data, mixed = FALSE, covariate = FALSE, measures = c("post", "pre", "mid")) {
  vars <- c("id", measures, if (mixed) "group", if (covariate) "cov")
  kept <- which(stats::complete.cases(data[vars])); wide <- data[kept, , drop = FALSE]
  wide$group <- droplevels(as.factor(wide$group))
  long <- data.frame(value = unlist(wide[measures], use.names = FALSE), id = factor(rep(wide$id, times = length(measures))),
    within = factor(rep(measures, each = nrow(wide)), levels = measures), group = rep(wide$group, times = length(measures)), cov = rep(wide$cov, times = length(measures)))
  form <- if (mixed && covariate) value ~ group * within + cov + Error(id / within) else if (mixed) value ~ group * within + Error(id / within) else if (covariate) value ~ within + cov + Error(id / within) else value ~ within + Error(id / within)
  fit <- stats::aov(form, data = long)
  list(wide = wide, long = long, rows = kept, fit = fit, tables = summary(fit))
}
check_repeated <- function(bundle, reference) {
  actual <- bundle$result$results$summary_df; count <- 0L
  total <- sum((reference$long$value - mean(reference$long$value))^2)
  for (stratum in names(reference$tables)) {
    table <- reference$tables[[stratum]][[1]]; names <- trimws(rownames(table))
    residual <- which(names == "Residuals")
    if (!length(residual)) next
    error <- table$`Sum Sq`[residual]; d2 <- table$Df[residual]; mse <- error / d2
    for (i in which(!names %in% c("Residuals", "(Intercept)"))) {
      model <- if (grepl("within", stratum, fixed = TRUE)) "Within" else "Between"
      row <- actual[actual$term == names[i] & actual$model == model, , drop = FALSE]
      check(nrow(row) == 1L, paste("Repeated effect/stratum inventory differs", names[i], model))
      ss <- table$`Sum Sq`[i]; d1 <- table$Df[i]
      check_row(row, list(df1 = d1, df2 = d2, ss = ss, ms = ss / d1, f = table$`F value`[i], p = table$`Pr(>F)`[i],
        eta_sq = ss / total, partial_eta_sq = ss / (ss + error), omega_sq = (ss - d1 * mse) / (total + mse),
        partial_omega_sq = (ss - d1 * mse) / (ss + error + mse)), paste("Repeated", names[i]))
      count <- count + 1L
    }
  }
  check(nrow(actual) == count, "Repeated model has unexpected effects")
  near(bundle$request$design$included_rows, reference$rows, "Repeated included source rows", tolerance = 0)
  mapping <- jsonlite::fromJSON(bundle$request_path, simplifyVector = FALSE)$design$subject_mapping
  check(identical(as.integer(vapply(mapping, "[[", numeric(1), "source_row")), reference$rows), "Repeated subject-to-source-row map differs")
  near(vapply(mapping, "[[", numeric(1), "raw_id"), reference$wide$id, "Repeated raw subject ID order", tolerance = 0)
}

check_rank_design <- function(path, data) {
  request <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  options <- request$options; mode <- options$mode
  for (entry in request$design$tests) {
    variables <- unlist(entry$variables)
    fields <- c(variables, options$group, if (mode == "friedman") options$subject_id)
    # SPSS raw fixtures still contain declared user-missing values. Their exact
    # normalized row mapping is checked by the separate SPSS numeric oracle.
    if (any(vapply(data[fields], inherits, logical(1), "haven_labelled"))) next
    rows <- which(stats::complete.cases(data[fields]))
    check(identical(as.integer(unlist(entry$row_indices)), rows), "Rank selected source rows differ")
    check(identical(as.integer(unlist(entry$excluded_row_indices)), setdiff(seq_len(nrow(data)), rows)), "Rank excluded source rows differ")
    if (mode == "friedman") near(unlist(entry$subject_ids), data[[options$subject_id]][rows], "Friedman original subject order", tolerance = 0)
    if (mode %in% c("wilcoxon_one_sample", "wilcoxon_paired")) {
      difference <- if (mode == "wilcoxon_one_sample") data[[variables[1]]][rows] - options$mu else data[[variables[1]]][rows] - data[[variables[2]]][rows]
      check(identical(as.integer(unlist(entry$zero_difference_row_indices)), rows[difference == 0]), "Signed zero-difference row mapping differs")
    }
    if (!is.null(options$group)) {
      levels <- rank_order(data[[options$group]])
      ids <- as.integer(unlist(entry$group_ids))
      for (i in seq_along(ids)) {
        expected <- rows[data[[options$group]][rows] == levels[ids[i]]]
        check(identical(as.integer(unlist(entry$group_row_indices[[i]])), expected), "Rank raw group row partition differs")
      }
    }
  }
}

wilcox_reference <- function(x, y = NULL, paired = FALSE, mu = 0, alternative = "two.sided", exact = NULL, continuity = TRUE, effect = "r", confidence = .9) {
  if (paired) { good <- stats::complete.cases(x, y); x <- x[good]; y <- y[good] } else { x <- x[!is.na(x)]; if (!is.null(y)) y <- y[!is.na(y)] }
  primary <- suppressWarnings(stats::wilcox.test(x, y, paired = paired, mu = mu, alternative = alternative, exact = exact, correct = continuity))
  ci <- suppressWarnings(tryCatch(stats::wilcox.test(x, y, paired = paired, mu = mu, alternative = alternative, exact = exact,
    correct = continuity, conf.int = TRUE, conf.level = confidence), error = function(e) NULL))
  independent <- !is.null(y) && !paired
  if (independent) {
    n <- length(x) + length(y)
    superiority <- mean(outer(x, y, ">")) + .5 * mean(outer(x, y, "=="))
    rb <- 2 * superiority - 1
    variance <- length(x) * length(y) / n * stats::var(rank(c(x, y)))
    z <- (length(x) * length(y) * (superiority - .5)) / sqrt(variance)
    effective_n <- n
  } else {
    difference <- if (paired) x - y - mu else x - mu
    nonzero <- difference[difference != 0]
    ranks <- rank(abs(nonzero)); signed <- sum(sign(nonzero) * ranks)
    rb <- signed / sum(ranks); z <- signed / sqrt(sum(ranks^2)); effective_n <- length(nonzero)
  }
  expected <- list(statistic = unname(primary$statistic), p = primary$p.value, z = z,
    effect_size_value = if (effect == "rb") rb else z / sqrt(effective_n))
  if (!is.null(ci) && !is.null(ci$estimate)) expected$location_estimate <- unname(ci$estimate)
  list(expected = expected, primary = primary, ci = ci, x = x, y = y, effective_n = effective_n)
}
check_wilcox <- function(row, reference, label) {
  check_row(row, reference$expected, label)
  if (!is.null(reference$ci) && !is.null(reference$ci$conf.int)) {
    interval(row, reference$ci$conf.int, label)
    check_row(row, list(actual_ci_level = attr(reference$ci$conf.int, "conf.level")), paste(label, "coverage"))
  }
  check(identical(row$exact_used, grepl("exact", reference$primary$method, ignore.case = TRUE)), paste(label, "actual exact method differs"))
  check(identical(row$continuity_used, grepl("continuity", reference$primary$method, ignore.case = TRUE)), paste(label, "actual continuity method differs"))
}
rank_order <- function(group) if (is.factor(group)) levels(droplevels(group)) else sort(unique(group[!is.na(group)]))
check_rank_posthoc <- function(bundle, data, mode, adjust, alternative = "two.sided", exact = FALSE, continuity = FALSE, effect = "r") {
  table <- bundle$result$results$posthoc_df
  if (mode == "friedman") data <- data[stats::complete.cases(data[c("id", "post", "pre", "mid")]), , drop = FALSE]
  check(nrow(table) > 0L, "Requested rank posthoc is empty")
  p <- numeric(nrow(table))
  for (i in seq_len(nrow(table))) {
    row <- table[i, ]
    if (mode == "friedman") {
      x <- data[[row$group_1]]; y <- data[[row$group_2]]
    } else {
      levels <- rank_order(data$group)
      first <- if (!is.null(row$group_1_id)) levels[row$group_1_id] else row$group_1
      second <- if (!is.null(row$group_2_id)) levels[row$group_2_id] else row$group_2
      x <- data$score[!is.na(data$group) & data$group == first]; y <- data$score[!is.na(data$group) & data$group == second]
    }
    reference <- wilcox_reference(x, y, paired = mode == "friedman", alternative = alternative, exact = exact, continuity = continuity, effect = effect)
    check_wilcox(row, reference, paste(mode, "posthoc", i))
    check_row(row, list(n_1 = length(reference$x), n_2 = if (mode == "friedman") NA_real_ else length(reference$y),
      median_1 = stats::median(reference$x), median_2 = stats::median(reference$y), iqr_1 = stats::IQR(reference$x), iqr_2 = stats::IQR(reference$y)), "Rank posthoc descriptive")
    p[i] <- reference$primary$p.value
  }
  near_probability(table$p_adj, stats::p.adjust(p, method = adjust, n = length(p)), "Rank posthoc correction")
}

# ANOVA: types and nonorthogonal designs; every effect size is checked on every
# row, irrespective of which effect is selected for the Markdown projection.
for (type in c("I", "II", "III")) for (shape in c("oneway", "factorial_ancova")) test("anova", paste0("anova_", type, "_", shape, if (type == "II" && shape == "factorial_ancova") "_smoke"), {
  complex <- shape == "factorial_ancova"
  context <- new_case(paste0("anova-", type, "-", shape), design_data)
  bundle <- new_run(context, "anova", c("--dv", "score", "--between", if (complex) "group,sex" else "group",
    if (complex) c("--covariates", "cov"), "--type", type, "--posthoc", "none"))
  fit <- between_fit(design_data, type, complex, complex)
  check_anova(bundle, fit, type)
  check_between_diagnostics(bundle, fit, clean_between(design_data, complex, complex), complex)
})
for (effect in c("eta_sq", "partial_eta", "omega_sq", "partial_omega")) test("anova", paste0("anova_selected_effect_", effect), {
  context <- new_case(paste0("anova-effect-", effect), design_data)
  bundle <- new_run(context, "anova", c("--dv", "score", "--between", "group", "--type", "I", "--effect-size", effect, "--posthoc", "none"))
  check_anova(bundle, between_fit(design_data))
  check(grepl(switch(effect, eta_sq = "eta", partial_eta = "eta", omega_sq = "omega", partial_omega = "omega"), bundle$markdown), "Selected effect absent from output")
})
for (posthoc in c("tukey", "pairwise")) test("anova", paste0("anova_between_", posthoc, "_posthoc"), {
  source <- design_data
  if (posthoc == "tukey") levels(source$group) <- paste0(levels(source$group), "-arm")
  context <- new_case(paste0("anova-posthoc-", posthoc), source)
  bundle <- new_run(context, "anova", c("--dv", "score", "--between", "group", "--type", "I", "--posthoc", posthoc, "--conf-level", ".9", "--p-adjust", "holm"))
  data <- clean_between(source); table <- bundle$result$results$posthoc_df
  check(nrow(table) == 3L, "Posthoc comparison count differs")
  if (posthoc == "tukey") {
    expected <- stats::TukeyHSD(stats::aov(score ~ group, data = data), conf.level = .9)$group
    for (i in seq_len(nrow(table))) {
      row <- table[i, ]; ref <- expected[row$contrast, ]
      check_row(row, list(mean_diff = ref["diff"], ci_low = ref["lwr"], ci_high = ref["upr"], p_adj = ref["p adj"]), "Tukey confidence and adjustment")
      check(row$group_1 %in% levels(data$group) && row$group_2 %in% levels(data$group), "Tukey split a hyphenated group label into false identities")
      near(row$mean_diff, mean(data$score[data$group == row$group_1]) - mean(data$score[data$group == row$group_2]), "Tukey raw group direction")
    }
  } else {
    p <- numeric(nrow(table))
    for (i in seq_len(nrow(table))) {
      row <- table[i, ]; x <- data$score[data$group == row$group_1]; y <- data$score[data$group == row$group_2]
      ref <- stats::t.test(x, y, conf.level = .9)
      check_row(row, list(mean_diff = mean(x) - mean(y), t = unname(ref$statistic), df = unname(ref$parameter), p = ref$p.value,
        ci_low = ref$conf.int[1], ci_high = ref$conf.int[2]), "Unadjusted Welch posthoc")
      p[i] <- ref$p.value
    }
    near_probability(table$p_adj, stats::p.adjust(p, "holm"), "Welch posthoc Holm")
  }
})
for (mixed in c(FALSE, TRUE)) for (covariate in c(FALSE, TRUE)) test("anova", paste0("anova_", if (mixed) "mixed" else "within", if (covariate) "_covariate" else "_smoke"), {
  context <- new_case(paste0("anova-repeated-", mixed, "-", covariate), design_data)
  bundle <- new_run(context, "anova", c("--within", "post,pre,mid", "--subject-id", "id", if (mixed) c("--between", "group"),
    if (covariate) c("--covariates", "cov"), "--type", "I", "--posthoc", "pairwise", "--sphericity", "none", "--p-adjust", "bonferroni", "--conf-level", ".9"))
  reference <- repeated_reference(design_data, mixed, covariate)
  check_repeated(bundle, reference)
  normality_formula <- if (mixed && covariate) value ~ id + group * within + cov else if (mixed) value ~ id + group * within else if (covariate) value ~ id + within + cov else value ~ id + within
  normality <- stats::shapiro.test(stats::residuals(stats::lm(normality_formula, data = reference$long)))
  assumptions <- bundle$result$results$assumptions_df
  check_row(assumptions[assumptions$test == "Shapiro-Wilk", ], list(statistic = unname(normality$statistic), p = normality$p.value), "Repeated full-model residual diagnostic")
  if (mixed) for (measure in c("post", "pre", "mid")) {
    ref <- car::leveneTest(reference$wide[[measure]], reference$wide$group, center = stats::median)
    check_row(assumptions[assumptions$test == "Levene (median)" & assumptions$target == measure, ],
      list(statistic = ref$`F value`[1], df1 = ref$Df[1], df2 = ref$Df[2], p = ref$`Pr(>F)`[1]), "Repeated between-group homogeneity")
  }
  table <- bundle$result$results$posthoc_df
  check(nrow(table) == if (mixed) 9L else 3L, "Repeated posthoc comparison count differs")
  for (group in unique(table$group)) {
    selected <- table[table$group == group, , drop = FALSE]
    data <- reference$wide
    if (mixed) data <- data[as.character(data$group) == sub("^group=", "", group), , drop = FALSE]
    p <- numeric(nrow(selected))
    for (i in seq_len(nrow(selected))) {
      row <- selected[i, ]; ref <- stats::t.test(data[[row$group_1]], data[[row$group_2]], paired = TRUE, conf.level = .9)
      check_row(row, list(mean_diff = mean(data[[row$group_1]] - data[[row$group_2]]), t = unname(ref$statistic), df = unname(ref$parameter), p = ref$p.value,
        ci_low = ref$conf.int[1], ci_high = ref$conf.int[2]), "Repeated posthoc")
      p[i] <- ref$p.value
    }
    near_probability(selected$p_adj, stats::p.adjust(p, "bonferroni"), "Repeated posthoc family correction")
  }
})
for (mixed in c(FALSE, TRUE)) test("anova", paste0("anova_sphericity_", if (mixed) "mixed" else "within"), {
  context <- new_case(paste0("anova-sphericity-", mixed), design_data)
  bundle <- new_run(context, "anova", c("--within", "post,pre,mid", "--subject-id", "id", if (mixed) c("--between", "group"), "--type", "I", "--posthoc", "none", "--sphericity", "auto"))
  reference <- repeated_reference(design_data, mixed); check_repeated(bundle, reference)
  data <- reference$wide
  model <- if (mixed) stats::lm(cbind(post, pre, mid) ~ group, data = data) else stats::lm(cbind(post, pre, mid) ~ 1, data = data)
  mauchly <- stats::mauchly.test(model, X = ~1)
  row <- bundle$result$results$assumptions_df
  row <- row[row$test == "Mauchly", , drop = FALSE]
  check_row(row, list(statistic = unname(mauchly$statistic), p = mauchly$p.value), "Sphericity on within contrasts")
  sphere <- stats::anova(model, X = ~1, test = "Spherical")
  eps <- attr(sphere, "heading")
  # Corrections are separately checked against car's public repeated-measures
  # summary; the test does not derive epsilons from NLSS functions.
  idata <- data.frame(within = factor(c("post", "pre", "mid"), levels = c("post", "pre", "mid")))
  cv <- summary(car::Anova(model, idata = idata, idesign = ~within, type = 3), multivariate = FALSE)
  correction <- cv$pval.adjustments
  actual <- bundle$result$results$summary_df
  for (i in which(grepl("within", actual$term, fixed = TRUE))) {
    term <- actual$term[i]; expected <- correction[term, ]
    check_row(actual[i, ], list(df1_gg = actual$df1[i] * expected["GG eps"], df2_gg = actual$df2[i] * expected["GG eps"],
      p_gg = stats::pf(actual$f[i], actual$df1[i] * expected["GG eps"], actual$df2[i] * expected["GG eps"], lower.tail = FALSE),
      df1_hf = actual$df1[i] * min(1, expected["HF eps"]), df2_hf = actual$df2[i] * min(1, expected["HF eps"]),
      p_hf = stats::pf(actual$f[i], actual$df1[i] * min(1, expected["HF eps"]), actual$df2[i] * min(1, expected["HF eps"]), lower.tail = FALSE)), "Sphericity-corrected degrees of freedom")
  }
})
for (method in c("pairwise", "trt.vs.ctrl", "custom")) test("anova", paste0("anova_planned_contrasts_", method), {
  context <- new_case(paste0("anova-contrasts-", method), design_data)
  extra <- character()
  weights <- list(B_vs_C = c(1, -1, 0), A_vs_others = c(-.5, -.5, 1))
  if (method == "custom") {
    spec <- file.path(context$base, "contrasts.json")
    jsonlite::write_json(list(term = "group", contrasts = list(B_vs_C = list(B = 1, C = -1, A = 0), A_vs_others = c(-.5, -.5, 1))), spec, auto_unbox = TRUE)
    extra <- c("--contrast-file", spec)
  }
  bundle <- new_run(context, "anova", c("--dv", "score", "--between", "group", "--covariates", "cov", "--type", "II", "--posthoc", "none",
    "--emmeans", "group", "--contrasts", method, "--p-adjust", "bonferroni", "--conf-level", ".9", extra))
  grid <- emmeans::emmeans(between_fit(design_data, "II", covariates = TRUE), ~group)
  expected <- as.data.frame(summary(emmeans::contrast(grid, method = if (method == "custom") weights else method), infer = c(TRUE, TRUE), adjust = "bonferroni", level = .9))
  actual <- bundle$result$results$contrasts_df
  check(nrow(actual) == nrow(expected) && setequal(actual$contrast, expected$contrast), "Planned contrast inventory differs")
  for (i in seq_len(nrow(expected))) {
    row <- expected[i, ]; saved <- actual[actual$contrast == row$contrast, , drop = FALSE]
    check_row(saved, list(estimate = row$estimate, se = row$SE, df = row$df, t = row$t.ratio,
      p_adj = row$p.value, ci_low = row$lower.CL, ci_high = row$upper.CL), "Model-adjusted planned contrast")
  }
  if (method == "custom") {
    jsonlite::write_json(list(term = "group", contrasts = list(CHANGED = c(0, 1, -1))), spec, auto_unbox = TRUE)
    replay(context, bundle)
  }
})
test("anova", "anova_builtin_contrast_JSON_reference_argument", {
  context <- new_case("anova-JSON-control", design_data)
  spec <- file.path(context$base, "contrasts.json")
  jsonlite::write_json(list(term = "group", method = "trt.vs.ctrl", args = list(ref = "A")), spec, auto_unbox = TRUE)
  bundle <- new_run(context, "anova", c("--dv", "score", "--between", "group", "--type", "I", "--posthoc", "none", "--contrast-file", spec, "--p-adjust", "none", "--conf-level", ".9"))
  expected <- as.data.frame(summary(emmeans::contrast(emmeans::emmeans(between_fit(design_data), ~group), "trt.vs.ctrl", ref = "A"), infer = c(TRUE, TRUE), adjust = "none", level = .9))
  actual <- bundle$result$results$contrasts_df
  check(setequal(actual$contrast, expected$contrast), "JSON control argument ignored")
  for (i in seq_len(nrow(expected))) {
    row <- expected[i, ]; saved <- actual[actual$contrast == row$contrast, , drop = FALSE]
    check_row(saved, list(estimate = row$estimate, se = row$SE, df = row$df, t = row$t.ratio, p = row$p.value, ci_low = row$lower.CL, ci_high = row$upper.CL), "JSON specified reference")
  }
  jsonlite::write_json(list(term = "group", method = "trt.vs.ctrl", args = list(ref = "C")), spec, auto_unbox = TRUE)
  replay(context, bundle)
})
for (mode in c("between", "mixed")) test("anova", paste0("anova_emmeans_grid_without_contrasts_", mode), {
  context <- new_case(paste0("anova-grid-", mode), design_data)
  if (mode == "between") {
    opts <- c("--dv", "score", "--between", "group", "--covariates", "cov", "--emmeans", "group")
    grid <- emmeans::emmeans(between_fit(design_data, "I", covariates = TRUE), ~group)
  } else {
    opts <- c("--within", "post,pre,mid", "--subject-id", "id", "--between", "group", "--emmeans", "within*group")
    long <- repeated_reference(design_data, mixed = TRUE)$long
    fit <- stats::aov(value ~ group * within + Error(id / within), data = long)
    grid <- emmeans::emmeans(fit, ~within * group)
  }
  bundle <- new_run(context, "anova", c(opts, "--type", "I", "--posthoc", "none", "--sphericity", "none", "--conf-level", ".9"))
  expected <- as.data.frame(summary(grid, infer = c(TRUE, FALSE), level = .9))
  actual <- bundle$result$results$emmeans_df
  check(nrow(actual) == nrow(expected), "Estimated marginal mean grid row count differs")
  for (field in names(expected)) {
    if (is.numeric(expected[[field]])) near(actual[[field]], expected[[field]], paste("Marginal mean", mode, field), 3e-8)
    else check(identical(as.character(actual[[field]]), as.character(expected[[field]])), "Marginal mean grid level ordering differs")
  }
  check(grepl("Marginal|marginal", bundle$markdown), "Requested marginal means absent from Markdown")
  replay(context, bundle)
})
test("anova", "anova_mixed_planned_contrasts_and_bootstrap_replay", {
  context <- new_case("anova-mixed-contrasts", design_data)
  bundle <- new_run(context, "anova", c("--within", "post,pre,mid", "--subject-id", "id", "--between", "group", "--type", "I", "--posthoc", "none", "--sphericity", "none",
    "--emmeans", "within", "--contrasts", "pairwise", "--p-adjust", "bonferroni", "--conf-level", ".9", "--bootstrap", "TRUE", "--bootstrap-samples", "19", "--seed", "781"))
  long <- repeated_reference(design_data, mixed = TRUE)$long
  fit <- stats::aov(value ~ group * within + Error(id / within), data = long)
  expected <- as.data.frame(summary(emmeans::contrast(emmeans::emmeans(fit, ~within), "pairwise"), infer = c(TRUE, TRUE), adjust = "bonferroni", level = .9))
  actual <- bundle$result$results$contrasts_df
  for (i in seq_len(nrow(expected))) {
    row <- expected[i, ]; saved <- actual[actual$contrast == row$contrast, , drop = FALSE]
    check_row(saved, list(estimate = row$estimate, se = row$SE, df = row$df, t = row$t.ratio, p_adj = row$p.value,
      ci_low = row$lower.CL, ci_high = row$upper.CL), "Mixed adjusted planned contrast")
  }
  replay(context, bundle)
})
test("anova", "anova_stochastic_mvt_contrasts_seed_and_replay", {
  context <- new_case("anova-mvt-contrasts", design_data)
  bundle <- new_run(context, "anova", c("--dv", "score", "--between", "group", "--type", "I", "--posthoc", "none", "--emmeans", "group",
    "--contrasts", "pairwise", "--p-adjust", "mvt", "--conf-level", ".9", "--seed", "918"))
  grid <- emmeans::emmeans(between_fit(design_data), ~group)
  set.seed(918)
  expected <- as.data.frame(summary(emmeans::contrast(grid, "pairwise"), infer = c(TRUE, TRUE), adjust = "mvt", level = .9))
  actual <- bundle$result$results$contrasts_df
  for (i in seq_len(nrow(expected))) {
    row <- expected[i, ]; saved <- actual[actual$contrast == row$contrast, , drop = FALSE]
    check_row(saved, list(estimate = row$estimate, se = row$SE, df = row$df, t = row$t.ratio, p_adj = row$p.value,
      ci_low = row$lower.CL, ci_high = row$upper.CL), "Seeded multivariate-t adjustment")
  }
  replay(context, bundle)
})
test("anova", "anova_requested_Tukey_actual_Sidak_contrast_adjustment", {
  context <- new_case("anova-effective-contrast-adjustment", design_data)
  bundle <- new_run(context, "anova", c("--dv", "score", "--between", "group", "--type", "I", "--posthoc", "none", "--emmeans", "group",
    "--contrasts", "poly", "--p-adjust", "tukey", "--conf-level", ".9"))
  summary <- summary(emmeans::contrast(emmeans::emmeans(between_fit(design_data), ~group), "poly"), infer = c(TRUE, TRUE), adjust = "tukey", level = .9)
  expected <- as.data.frame(summary); actual <- bundle$result$results$contrasts_df
  check(identical(attr(summary, "adjust"), "sidak"), "Fixture no longer triggers the package's effective Sidak correction")
  for (i in seq_len(nrow(expected))) {
    row <- expected[i, ]; saved <- actual[actual$contrast == row$contrast, , drop = FALSE]
    check_row(saved, list(estimate = row$estimate, se = row$SE, df = row$df, t = row$t.ratio, p_adj = row$p.value, ci_low = row$lower.CL, ci_high = row$upper.CL), "Effective Sidak correction")
  }
  check(grepl("sidak", bundle$markdown, ignore.case = TRUE) && grepl("tukey", bundle$markdown, ignore.case = TRUE), "Markdown misidentifies effective contrast correction as requested Tukey")
  check(grepl("sidak", text_file(bundle$request_path), ignore.case = TRUE), "Effective correction absent from machine-readable design")
  replay(context, bundle)
})
test("anova", "anova_interaction_named_contrast_order", {
  context <- new_case("anova-interaction-contrast", design_data)
  fit <- between_fit(design_data, "III", covariates = TRUE, factorial = TRUE)
  grid <- emmeans::emmeans(fit, ~group * sex)
  order <- as.data.frame(grid)
  keys <- paste0("group=", order$group, ", sex=", order$sex)
  weights <- setNames(c(1, -1, 0, -1, 1, 0), keys)
  spec <- file.path(context$base, "contrasts.json")
  jsonlite::write_json(list(term = "group*sex", contrasts = list(interaction = as.list(weights[rev(seq_along(weights))]))), spec, auto_unbox = TRUE)
  bundle <- new_run(context, "anova", c("--dv", "score", "--between", "group,sex", "--covariates", "cov", "--type", "III", "--posthoc", "none",
    "--contrasts", "custom", "--contrast-file", spec, "--p-adjust", "none", "--conf-level", ".9"))
  expected <- as.data.frame(summary(emmeans::contrast(grid, list(interaction = unname(weights))), infer = c(TRUE, TRUE), adjust = "none", level = .9))
  check_row(bundle$result$results$contrasts_df, list(estimate = expected$estimate, se = expected$SE, df = expected$df, t = expected$t.ratio,
    p = expected$p.value, p_adj = NA_real_, ci_low = expected$lower.CL, ci_high = expected$upper.CL), "Named interaction weights")
})
for (mode in c("between", "within", "mixed")) test("anova", paste0("anova_bootstrap_joint_cases_", mode), {
  context <- new_case(paste0("anova-bootstrap-", mode), design_data)
  options <- if (mode == "between") c("--dv", "score", "--between", "group") else c("--within", "post,pre,mid", "--subject-id", "id", if (mode == "mixed") c("--between", "group"))
  bundle <- new_run(context, "anova", c(options, "--type", "I", "--posthoc", "none", "--sphericity", "none", "--effect-size", "partial_eta",
    "--bootstrap", "TRUE", "--bootstrap-samples", "39", "--seed", "8137", "--conf-level", ".9"))
  data <- if (mode == "between") clean_between(design_data) else repeated_reference(design_data, mixed = mode == "mixed")$wide
  keys <- paste(bundle$result$results$summary_df$model, bundle$result$results$summary_df$term, sep = ":")
  samples <- matrix(NA_real_, nrow = 39, ncol = length(keys), dimnames = list(NULL, keys))
  set.seed(8137)
  for (draw in seq_len(39)) {
    rows <- sample.int(nrow(data), nrow(data), replace = TRUE)
    selected <- data[rows, , drop = FALSE]
    if (mode == "between") {
      expected <- anova_expected(between_fit(selected), "I")
      samples[draw, paste("Between", expected$term, sep = ":")] <- expected$partial_eta_sq
    } else {
      selected$id <- seq_len(nrow(selected))
      reference <- repeated_reference(selected, mixed = mode == "mixed")
      for (stratum in names(reference$tables)) {
        table <- reference$tables[[stratum]][[1]]; terms <- trimws(rownames(table))
        residual <- which(terms == "Residuals")
        if (!length(residual)) next
        for (i in which(!terms %in% c("Residuals", "(Intercept)"))) {
          key <- paste(if (grepl("within", stratum, fixed = TRUE)) "Within" else "Between", terms[i], sep = ":")
          samples[draw, key] <- table$`Sum Sq`[i] / (table$`Sum Sq`[i] + table$`Sum Sq`[residual])
        }
      }
    }
  }
  actual <- bundle$result$results$summary_df
  for (i in seq_len(nrow(actual))) {
    values <- samples[, keys[i]]; valid <- sum(is.finite(values)); ci <- stats::quantile(values[is.finite(values)], c(.05, .95))
    check_row(actual[i, ], list(boot_ci_low = ci[1], boot_ci_high = ci[2], boot_valid = valid, boot_discarded = 39 - valid), "Joint bootstrap effect interval")
  }
  replay(context, bundle)
})

# Rank tests: sample/paired/independent hypotheses, directional CIs, both effects.
for (mode in c("one_sample", "paired", "mann_whitney")) for (alternative in c("two.sided", "less", "greater")) for (effect in c("r", "rb"))
test("nonparametric", paste0("rank_", mode, "_", alternative, "_", effect, if (alternative == "two.sided" && effect == "r") "_smoke"), {
  data <- if (mode == "mann_whitney") subset(rank_groups, group != "A") else rank_data
  context <- new_case(paste0("rank-", mode, "-", alternative, "-", effect), data)
  options <- switch(mode, one_sample = c("--vars", "x", "--mu", "1.3"), paired = c("--x", "x", "--y", "y"), mann_whitney = c("--vars", "score", "--group", "group"))
  bundle <- new_run(context, "nonparametric", c(options, "--test", "auto", "--alternative", alternative, "--effect-size", effect,
    "--conf-level", ".9", "--exact", "auto", "--continuity", "TRUE"))
  if (mode == "mann_whitney") {
    levels <- rank_order(data$group)
    reference <- wilcox_reference(data$score[data$group == levels[1]], data$score[data$group == levels[2]], alternative = alternative, effect = effect)
  } else reference <- wilcox_reference(data$x, if (mode == "paired") data$y, paired = mode == "paired", mu = if (mode == "one_sample") 1.3 else 0, alternative = alternative, effect = effect)
  row <- bundle$result$results$summary_df
  check_wilcox(row, reference, paste(mode, alternative, effect))
  check_row(row, list(n_1 = length(reference$x), n_2 = if (mode == "mann_whitney") length(reference$y) else NA_real_,
    n_total = length(reference$x) + if (mode == "mann_whitney") length(reference$y) else 0L), "Rank sample sizes")
  if (mode == "one_sample") check_row(row, list(median = stats::median(reference$x), iqr = stats::IQR(reference$x),
    median_diff = stats::median(reference$x - 1.3), iqr_diff = stats::IQR(reference$x - 1.3), mu = 1.3), "Signed descriptive")
  else check_row(row, list(median_1 = stats::median(reference$x), median_2 = stats::median(reference$y), iqr_1 = stats::IQR(reference$x),
    iqr_2 = stats::IQR(reference$y)), "Two-sample descriptive")
})
for (mode in c("one_sample", "paired", "mann_whitney")) for (exact in c(TRUE, FALSE)) test("nonparametric", paste0("rank_explicit_exact_", mode, "_", exact), {
  data <- rank_data
  if (mode == "mann_whitney") data <- data.frame(score = c(rank_data$x[1:8], rank_data$y[1:8] + .034), group = rep(c("B", "A"), each = 8))
  context <- new_case(paste0("rank-exact-", mode, "-", exact), data)
  options <- switch(mode, one_sample = c("--vars", "x"), paired = c("--x", "x", "--y", "y"), mann_whitney = c("--vars", "score", "--group", "group"))
  bundle <- new_run(context, "nonparametric", c(options, "--exact", as.character(exact), "--continuity", "FALSE", "--conf-level", ".9"))
  reference <- if (mode == "mann_whitney") wilcox_reference(data$score[data$group == "A"], data$score[data$group == "B"], exact = exact, continuity = FALSE)
    else wilcox_reference(data$x, if (mode == "paired") data$y, paired = mode == "paired", exact = exact, continuity = FALSE)
  check_wilcox(bundle$result$results$summary_df, reference, "Explicit exact and continuity")
})
test("nonparametric", "rank_signed_ties_zeros_and_exact_fallback", {
  data <- data.frame(x = c(-3, -2, -2, 0, 0, 1, 1, 2, 4, 4, 5, NA))
  context <- new_case("rank-ties-zero", data)
  bundle <- new_run(context, "nonparametric", c("--vars", "x", "--exact", "TRUE", "--continuity", "FALSE", "--conf-level", ".9"))
  reference <- wilcox_reference(data$x, exact = TRUE, continuity = FALSE)
  check_wilcox(bundle$result$results$summary_df, reference, "Signed tied variance")
  check_row(bundle$result$results$diagnostics_df, list(n_total = 11, n_nonzero = 9, zero_diff_n = 2), "Signed zero exclusion")
  check(isTRUE(bundle$result$results$diagnostics_df$ties), "Tied ranks missing from diagnostics")
  if (!grepl("exact", reference$primary$method, ignore.case = TRUE)) check(length(bundle$result$warnings) > 0L, "Tied exact fallback lacks disclosed warnings")
})
for (mode in c("kruskal", "friedman")) for (adjust in stats::p.adjust.methods) test("nonparametric", paste0("rank_", mode, "_posthoc_", adjust, if (adjust == "holm") "_smoke"), {
  data <- rank_groups
  context <- new_case(paste0("rank-", mode, "-", adjust), data)
  options <- if (mode == "kruskal") c("--vars", "score", "--group", "group") else c("--within", "post,pre,mid", "--subject-id", "id")
  bundle <- new_run(context, "nonparametric", c(options, "--test", "auto", "--posthoc", "pairwise", "--p-adjust", adjust,
    "--exact", "FALSE", "--continuity", "FALSE", "--conf-level", ".9"))
  if (mode == "kruskal") {
    clean <- data[stats::complete.cases(data[c("score", "group")]), , drop = FALSE]
    reference <- stats::kruskal.test(clean$score, clean$group)
    expected <- list(statistic = unname(reference$statistic), df = unname(reference$parameter), p = reference$p.value,
      n_total = nrow(clean), effect_size_value = (unname(reference$statistic) - 2) / (nrow(clean) - 3))
  } else {
    clean <- data[stats::complete.cases(data[c("id", "post", "pre", "mid")]), , drop = FALSE]
    reference <- stats::friedman.test(as.matrix(clean[c("post", "pre", "mid")]))
    expected <- list(statistic = unname(reference$statistic), df = unname(reference$parameter), p = reference$p.value,
      n_total = nrow(clean), effect_size_value = unname(reference$statistic) / (nrow(clean) * 2))
  }
  check_row(bundle$result$results$summary_df, expected, paste(mode, "omnibus and effect"))
  check_rank_posthoc(bundle, data, mode, adjust)
})
test("nonparametric", "rank_negative_rank_eta_is_not_clamped_or_mislabelled", {
  data <- data.frame(score = rep(c(1, 2, 3, 4), 3), group = rep(c("A", "B", "C"), each = 4))
  context <- new_case("rank-negative-eta", data)
  bundle <- new_run(context, "nonparametric", c("--test", "kruskal", "--vars", "score", "--group", "group", "--effect-size", "epsilon_sq"))
  reference <- stats::kruskal.test(data$score, data$group)
  check_row(bundle$result$results$summary_df, list(statistic = unname(reference$statistic), p = reference$p.value,
    effect_size_value = (unname(reference$statistic) - 2) / 9), "Unclamped rank eta")
  check(!grepl("epsilon", bundle$result$results$summary_df$effect_size_label, ignore.case = TRUE), "Legacy alias still falsely labels eta as epsilon")
  check(length(bundle$result$warnings) > 0L, "Legacy effect alias lacks compatibility disclosure")
})
for (mode in c("kruskal", "friedman")) test("nonparametric", paste0("rank_directional_rank_biserial_posthoc_", mode), {
  context <- new_case(paste0("rank-rb-posthoc-", mode), rank_groups)
  options <- if (mode == "kruskal") c("--vars", "score", "--group", "group") else c("--within", "post,pre,mid", "--subject-id", "id")
  bundle <- new_run(context, "nonparametric", c(options, "--test", mode, "--posthoc", "pairwise", "--p-adjust", "holm", "--effect-size", "rb",
    "--alternative", "greater", "--exact", "TRUE", "--continuity", "TRUE", "--conf-level", ".9"))
  check_rank_posthoc(bundle, rank_groups, mode, "holm", alternative = "greater", exact = TRUE, continuity = TRUE, effect = "rb")
})
test("nonparametric", "rank_Friedman_tie_corrected_statistic_and_W", {
  values <- matrix(c(1,1,2, 2,3,3, 3,3,3, 2,1,2, 8,7,8, 9,10,11, 0,0,3, 4,5,4), ncol = 3, byrow = TRUE)
  data <- data.frame(id = c(20, 3, 11, 2, 50, 40, 19, 7), post = values[,1], pre = values[,2], mid = values[,3])
  context <- new_case("rank-friedman-ties", data)
  bundle <- new_run(context, "nonparametric", c("--within", "post,pre,mid", "--subject-id", "id", "--test", "friedman", "--posthoc", "pairwise", "--conf-level", ".9", "--exact", "FALSE", "--continuity", "FALSE"))
  reference <- stats::friedman.test(values)
  check_row(bundle$result$results$summary_df, list(statistic = unname(reference$statistic), df = 2, p = reference$p.value,
    effect_size_value = unname(reference$statistic) / 16), "Tie-corrected Friedman and Kendall W")
  check_rank_posthoc(bundle, data, "friedman", "holm")
})
for (kind in c("numeric", "fractional_timestamp")) test("nonparametric", paste0("rank_distinct_raw_group_identity_", kind), {
  values <- if (kind == "numeric") c(1, 1 + 1e-15) else as.POSIXct("2025-01-01", tz = "UTC") + c(.0000002, .0000004)
  data <- data.frame(score = c(rank_data$x[1:10], rank_data$y[1:10]), group = rep(rev(values), each = 10))
  context <- new_case(paste0("rank-group-collision-", kind), data)
  bundle <- new_run(context, "nonparametric", c("--test", "mann_whitney", "--vars", "score", "--group", "group", "--conf-level", ".9"))
  reference <- wilcox_reference(data$score[data$group == values[1]], data$score[data$group == values[2]])
  check_wilcox(bundle$result$results$summary_df, reference, "Collision-free Mann-Whitney")
  row <- bundle$result$results$summary_df
  check(!identical(row$group_1_label, row$group_2_label), "Different groups have indistinguishable display labels")
  check(identical(as.integer(c(row$group_1_id, row$group_2_id)), 1:2), "Mann-Whitney lost stable raw group IDs")
  replay(context, bundle)
})
test("nonparametric", "rank_literal_NA_group_distinct_from_missing_group", {
  data <- data.frame(score = c(1, 3, 2, 7, 4, 5, 6, 9, 11), group = c("NA", "A", "NA", "A", "NA", "A", "NA", "A", NA))
  context <- new_case("rank-literal-NA", data)
  bundle <- new_run(context, "nonparametric", c("--vars", "score", "--group", "group", "--conf-level", ".9"))
  check_wilcox(bundle$result$results$summary_df, wilcox_reference(data$score[!is.na(data$group) & data$group == "A"], data$score[!is.na(data$group) & data$group == "NA"]), "Literal NA group")
})
for (module in c("anova", "nonparametric")) {
  options <- if (module == "anova") c("--dv", "score", "--between", "group", "--type", "II", "--posthoc", "none") else c("--vars", "score", "--group", "group", "--test", "kruskal")
  test(module, paste0(module, "_SPSS_labels_user_missing_values"), {
    data <- design_data
    data$sex <- as.character(data$sex)
    raw <- match(as.character(data$group), c("B", "C", "A"))
    data$group <- haven::labelled_spss(raw, labels = c("Treatment-B" = 1, "Treatment-C" = 2, "Control-A" = 3, "Not answered" = 99), na_values = 99, label = "Study condition")
    data$group[8] <- 99
    data$score <- haven::labelled_spss(data$score, labels = c("Unanswered" = 999), na_values = 999, label = "Outcome score")
    data$score[12] <- 999
    context <- new_case(paste0(module, "-spss"), data, "sav")
    bundle <- new_run(context, module, override_options(options, c("--posthoc", "pairwise")))
    expected <- design_data; expected$group <- factor(raw); expected$group[8] <- NA; expected$score[12] <- NA
    if (module == "anova") check_anova(bundle, between_fit(expected, "II"), "II") else {
      kept <- stats::complete.cases(expected[c("score", "group")]); value <- stats::kruskal.test(expected$score[kept], expected$group[kept])
      check_row(bundle$result$results$summary_df, list(statistic = unname(value$statistic), p = value$p.value, n_total = sum(kept)), "SPSS Kruskal")
    }
    dictionary <- text_file(file.path(context$project, bundle$request$dataset$dictionary_path))
    check(grepl("Treatment-B", dictionary, fixed = TRUE) && grepl("Outcome score", dictionary, fixed = TRUE), "SPSS value/variable labels lost from dictionary")
    check(grepl(if (module == "anova") "Study condition" else "Outcome score", bundle$markdown, fixed = TRUE), "SPSS variable label absent from output")
    check(grepl("Treatment-B", bundle$markdown, fixed = TRUE) && grepl("Control-A", bundle$markdown, fixed = TRUE), "SPSS group value labels absent from posthoc output")
    replay(context, bundle)
  })
  test(module, paste0(module, "_privacy_frozen_input_template_config_replay_smoke"), {
    context <- new_case(paste0(module, "-privacy"), design_data)
    yaml::write_yaml(list(defaults = list(digits = 5L), logging = list(include_user_prompt = FALSE, include_outputs = FALSE)), private_config)
    template <- file.path(context$base, "custom.md")
    writeLines(c(paste0("# ", cfg$template_marker), "{{table_body}}", "{{narrative}}"), template)
    extra <- if (module == "anova") c("--bootstrap", "TRUE", "--bootstrap-samples", "29") else character()
    bundle <- new_run(context, module, c(options, extra, "--template", template, "--log", "FALSE", "--user-prompt", "PRIVATE_DESIGN_PROMPT"))
    check(grepl(cfg$template_marker, bundle$markdown, fixed = TRUE), "Private custom template not used")
    check(!grepl("PRIVATE_DESIGN_PROMPT", text_file(bundle$request_path), fixed = TRUE), "Prompt privacy ignored")
    check(!grepl(context$base, text_file(bundle$request_path), fixed = TRUE), "External source/template path leaked")
    check(length(bundle$result$results) > 0L, "Legacy include_outputs removed mandatory result")
    if (module == "anova") {
      check_anova(bundle, between_fit(design_data, "II"), "II")
      check(identical(bundle$request$options$seed, 1L), "Canonical default seed not resolved")
    } else {
      data <- clean_between(design_data); ref <- stats::kruskal.test(data$score, data$group)
      check_row(bundle$result$results$summary_df, list(statistic = unname(ref$statistic), p = ref$p.value), "Privacy run primary numbers")
    }
    logfile <- file.path(context$dataset, "analysis_log.jsonl")
    if (file.exists(logfile)) check(!grepl(paste0('"module":"', module, '"'), gsub(" ", "", text_file(logfile)), fixed = TRUE), "Legacy log optout ignored")
    writeLines("CHANGED ORIGINAL TEMPLATE", template)
    working <- file.path(context$dataset, "sample.parquet"); current <- arrow::read_parquet(working, as_data_frame = TRUE)
    current$score[1] <- 123456; arrow::write_parquet(current, working)
    source_data <- design_data; source_data$score[1] <- -999; saveRDS(source_data, context$input)
    yaml::write_yaml(list(defaults = list(digits = 1L), modules = setNames(list(if (module == "anova") list(seed = 981L, type = "I") else list(p_adjust = "none")), module)), private_config)
    replay(context, bundle)
    near(arrow::read_parquet(working, as_data_frame = TRUE)$score[1], 123456, "Replay preserved changed working data")
  })
  test(module, paste0(module, "_nonestimable_failed_run_preserves_previous_publication"), {
    data <- design_data; data$constant <- if (module == "anova") 5 else 0
    if (module == "anova") data$constant_by_group <- 5 + 2 * as.numeric(data$group)
    context <- new_case(paste0(module, "-failed"), data)
    new_run(context, module, options)
    bad <- if (module == "anova") c("--dv", "constant", "--between", "group", "--posthoc", "none") else c("--vars", "constant", "--test", "wilcoxon")
    failed(context, module, bad)
    if (module == "anova") failed(context, module, c("--dv", "constant_by_group", "--between", "group", "--posthoc", "none"))
  })
  test(module, paste0(module, "_invalid_option_domains_and_incompatible_roles"), {
    context <- new_case(paste0(module, "-domains"), design_data)
    invalid <- list(c("--digits", "1.5"), c("--conf-level", "1"), c("--conf-level", "NaN"), c("--posthoc", "unknown"), c("--p-adjust", "unknown"), c("--effect-size", "unknown"))
    if (module == "anova") invalid <- c(invalid, list(c("--type", "IV"), c("--seed", "-1"), c("--bootstrap-samples", "1.5"), c("--bootstrap-samples", "0"),
      c("--sphericity", "unknown"), c("--between", "group,group"), c("--covariates", "score")))
    else invalid <- c(invalid, list(c("--alternative", "unknown"), c("--exact", "notboolean"), c("--test", "unknown"), c("--vars", "score,score"),
      c("--x", "pre,mid", "--y", "post"), c("--vars", "score", "--test", "mann_whitney"), c("--within", "pre,pre", "--subject-id", "id")))
    for (bad in invalid) run_module(module, c(context$flag, context$input, override_options(options, bad), "--interactive", "FALSE"), failure = TRUE)
    check(!any(vapply(runs(context), function(path) identical(read_json(file.path(path, "result.json"))$status, "completed"), logical(1))), "Invalid option request completed")
  })
  test(module, paste0(module, "_nonfinite_selected_data_fails"), {
    data <- design_data; data$score[1] <- Inf
    context <- new_case(paste0(module, "-nonfinite"), data)
    run_module(module, c(context$flag, context$input, options), failure = TRUE)
    check(!any(vapply(runs(context), function(path) identical(read_json(file.path(path, "result.json"))$status, "completed"), logical(1))), "Infinity silently removed or published")
  })
  test(module, paste0(module, "_duplicate_subject_ids_fail"), {
    data <- design_data; data$id[2] <- data$id[1]
    context <- new_case(paste0(module, "-duplicate-id"), data)
    repeated <- c("--within", "post,pre,mid", "--subject-id", "id", if (module == "anova") c("--type", "I", "--posthoc", "none") else c("--test", "friedman"))
    failed(context, module, repeated)
  })
}
test("anova", "anova_numeric_coercion_missing_rows_are_explicit", {
  data <- design_data; data$cov <- as.character(data$cov); data$cov[5] <- "not_numeric"
  context <- new_case("anova-coercion", data)
  bundle <- new_run(context, "anova", c("--dv", "score", "--between", "group", "--covariates", "cov", "--type", "II", "--posthoc", "none"))
  expected <- design_data; expected$cov[5] <- NA
  check_anova(bundle, between_fit(expected, "II", covariates = TRUE), "II")
  check(length(bundle$result$warnings) > 0L, "Covariate coercion loss is silent")
  check(any(grepl("cov.*source rows: 5", unlist(bundle$result$warnings))), "Coercion warning does not identify variable and source row")
  check(identical(bundle$request$design$source_classes$cov, "character") && 5L %in% bundle$request$design$excluded_rows, "Coercion source type/excluded row audit missing")
})
test("anova", "anova_nonsyntactic_subject_and_internal_name_collision", {
  data <- design_data
  names(data)[match(c("id", "group", "cov", "post", "pre", "mid"), names(data))] <- c("person id", "study group", "age cov", "within", "dv", "visit two")
  context <- new_case("anova-spaced-and-colliding-names", data)
  bundle <- new_run(context, "anova", c("--within", "within,dv,visit two", "--subject-id", "person id", "--between", "study group", "--covariates", "age cov",
    "--type", "I", "--posthoc", "none", "--sphericity", "auto"))
  # Compare the same independently fitted values after translating only the
  # human/source names. The repeated-factor and response storage names are not
  # source columns and must not overwrite a measured variable.
  normalized <- bundle
  normalized$result$results$summary_df$term <- gsub("`", "", normalized$result$results$summary_df$term, fixed = TRUE)
  normalized$result$results$summary_df$term <- gsub("study group", "group", normalized$result$results$summary_df$term, fixed = TRUE)
  normalized$result$results$summary_df$term <- gsub("age cov", "cov", normalized$result$results$summary_df$term, fixed = TRUE)
  normalized$result$results$summary_df$term <- gsub("within_", "within", normalized$result$results$summary_df$term, fixed = TRUE)
  check_repeated(normalized, repeated_reference(design_data, mixed = TRUE, covariate = TRUE))
  actual <- normalized$result$results$summary_df
  check(sum(actual$model == "Within") == 2L && all(is.finite(actual$p_gg[actual$model == "Within"])), "Non-syntactic subject ID damaged Within stratum or corrections")
  replay(context, bundle)
})
test("nonparametric", "rank_nonnumeric_factor_not_silently_used_as_internal_codes", {
  data <- data.frame(score = factor(c("1", "10", "2", "7", "3", "4"), levels = c("7", "3", "1", "2", "4", "10")))
  context <- new_case("rank-factor-codes", data)
  failed(context, "nonparametric", c("--vars", "score", "--test", "wilcoxon"))
})
test("nonparametric", "rank_reduced_CI_coverage_is_honest_in_Markdown", {
  data <- data.frame(score = c(1, 3, 5)); context <- new_case("rank-reduced-coverage", data)
  bundle <- new_run(context, "nonparametric", c("--vars", "score", "--test", "wilcoxon", "--exact", "TRUE", "--conf-level", ".95"))
  reference <- wilcox_reference(data$score, exact = TRUE, confidence = .95)
  check_wilcox(bundle$result$results$summary_df, reference, "Reduced achievable confidence level")
  check(grepl("75[[:space:]]*%", bundle$markdown), "Markdown hides the actually achieved 75% interval coverage")
  check(length(bundle$result$warnings) > 0L, "Reduced coverage is not warned")
})
test("nonparametric", "rank_exact_request_fallback_is_honest_in_Markdown", {
  data <- data.frame(score = c(-3, -2, -2, 0, 1, 1, 2, 4, 4, 5)); context <- new_case("rank-output-exact-fallback", data)
  bundle <- new_run(context, "nonparametric", c("--vars", "score", "--test", "wilcoxon", "--exact", "TRUE", "--conf-level", ".9"))
  reference <- wilcox_reference(data$score, exact = TRUE)
  check_wilcox(bundle$result$results$summary_df, reference, "Actual method under ties and zeros")
  if (!grepl("exact", reference$primary$method, ignore.case = TRUE))
    check(grepl("approxim|continuity|asymptotic", bundle$markdown, ignore.case = TRUE), "Markdown only shows requested exact option, not effective approximation")
  else check(grepl("exact", bundle$markdown, ignore.case = TRUE), "Markdown hides the actual exact inference method")
})
test("anova", "anova_repeated_nonzero_constant_response_fails", {
  data <- design_data; data$flat_a <- data$flat_b <- data$flat_c <- 5
  data$parallel_a <- seq_len(nrow(data)); data$parallel_b <- data$parallel_a + 2; data$parallel_c <- data$parallel_a + 5
  context <- new_case("anova-repeated-all-flat", data)
  new_run(context, "anova", c("--within", "post,pre,mid", "--subject-id", "id", "--type", "I", "--posthoc", "none", "--sphericity", "none"))
  failed(context, "anova", c("--within", "flat_a,flat_b,flat_c", "--subject-id", "id", "--type", "I", "--posthoc", "none", "--sphericity", "none"))
  failed(context, "anova", c("--within", "parallel_a,parallel_b,parallel_c", "--subject-id", "id", "--type", "I", "--posthoc", "none", "--sphericity", "none"))
})
for (mode in c("between", "within")) test("anova", paste0("anova_unavailable_posthoc_", mode, "_visible_and_planned_family"), {
  data <- if (mode == "between") data.frame(score = c(rep(1, 8), rep(1, 8), c(1, 2, 3, 3, 4, 5, 7, 9)), group = rep(c("A", "B", "C"), each = 8)) else
    data.frame(id = c(20, 3, 11, 2, 50, 40, 19, 7), pre = 1:8, mid = 1:8, post = c(2, 5, 1, 7, 3, 8, 5, 10))
  context <- new_case(paste0("anova-unavailable-posthoc-", mode), data)
  options <- if (mode == "between") c("--dv", "score", "--between", "group") else c("--within", "pre,mid,post", "--subject-id", "id")
  bundle <- new_run(context, "anova", c(options, "--type", "I", "--posthoc", "pairwise", "--p-adjust", "bonferroni", "--sphericity", "none"))
  rows <- bundle$result$results$posthoc_df
  check(nrow(rows) == 3L && sum(is.na(rows$p)) == 1L, "ANOVA unestimable planned comparison was dropped or supplied a false p")
  expected <- vapply(seq_len(nrow(rows)), function(i) {
    row <- rows[i, ]; x <- if (mode == "within") data[[row$group_1]] else data$score[data$group == row$group_1]
    y <- if (mode == "within") data[[row$group_2]] else data$score[data$group == row$group_2]
    tryCatch(stats::t.test(x, y, paired = mode == "within")$p.value, error = function(e) NA_real_)
  }, numeric(1))
  near_probability(rows$p, expected, "ANOVA planned comparison p values")
  near_probability(rows$p_adj, stats::p.adjust(expected, "bonferroni", n = 3), "ANOVA complete planned family")
  check(grepl("unavailable|not estimable|could not", bundle$markdown, ignore.case = TRUE), "Markdown hides unavailable ANOVA posthoc")
  check(length(bundle$result$warnings) > 0L, "Unavailable ANOVA posthoc lacks warning")
})
test("anova", "anova_conditional_bootstrap_counts_are_visible", {
  data <- data.frame(score = c(1, 3, 2, 4, 5, 7), group = factor(rep(c("A", "B", "C"), each = 2)), sex = "X", cov = 0)
  context <- new_case("anova-conditional-bootstrap", data)
  bundle <- new_run(context, "anova", c("--dv", "score", "--between", "group", "--type", "I", "--posthoc", "none",
    "--bootstrap", "TRUE", "--bootstrap-samples", "79", "--seed", "398", "--conf-level", ".9"))
  set.seed(398)
  draws <- suppressWarnings(replicate(79, {
    sample <- data[sample.int(6, 6, replace = TRUE), , drop = FALSE]
    cells <- split(sample$score, droplevels(sample$group))
    total <- sum((sample$score - mean(sample$score))^2)
    within <- sum(vapply(cells, function(x) sum((x - mean(x))^2), numeric(1)))
    # One-way ANOVA variance decomposition independently identifies truly
    # constant cells; no arbitrary residual epsilon or NLSS function is used.
    if (length(cells) != 3L || total == 0 || within == 0) NA_real_ else (total - within) / total
  }))
  valid <- sum(is.finite(draws)); ci <- stats::quantile(draws[is.finite(draws)], c(.05, .95))
  check(valid > 1L && valid < 79L, "Bootstrap fixture lacks degenerate resamples")
  row <- bundle$result$results$summary_df
  check_row(row, list(boot_valid = valid, boot_discarded = 79 - valid, boot_ci_low = ci[1], boot_ci_high = ci[2]), "Conditional bootstrap interval and counts")
  check(grepl("discard|failed|unsuccessful|invalid", bundle$markdown, ignore.case = TRUE) &&
    grepl(as.character(valid), bundle$markdown, fixed = TRUE) && grepl(as.character(79 - valid), bundle$markdown, fixed = TRUE), "Markdown hides bootstrap retained/discarded counts")
  check(length(bundle$result$warnings) > 0L, "Conditional bootstrap lacks warning")
  replay(context, bundle)
})
for (mode in c("kruskal", "friedman")) test("nonparametric", paste0("rank_unavailable_posthoc_", mode, "_visible_and_planned_family"), {
  data <- if (mode == "kruskal") data.frame(score = c(rep(1, 8), rep(1, 8), c(1, 2, 3, 3, 4, 5, 7, 9)), group = rep(c("A", "B", "C"), each = 8)) else
    data.frame(id = c(20, 3, 11, 2, 50, 40, 19, 7), pre = 1:8, mid = 1:8, post = c(2, 5, 1, 7, 3, 8, 5, 10))
  context <- new_case(paste0("rank-unavailable-posthoc-", mode), data)
  options <- if (mode == "kruskal") c("--vars", "score", "--group", "group") else c("--within", "pre,mid,post", "--subject-id", "id")
  bundle <- new_run(context, "nonparametric", c(options, "--test", mode, "--posthoc", "pairwise", "--p-adjust", "bonferroni", "--exact", "FALSE"))
  rows <- bundle$result$results$posthoc_df
  check(nrow(rows) == 3L && sum(is.na(rows$p)) == 1L, "Unestimable planned comparison was dropped or supplied a false p")
  near_probability(rows$p_adj, stats::p.adjust(rows$p, "bonferroni", n = 3), "Planned-family correction with unavailable test")
  check(grepl("unavailable|not estimable|could not", bundle$markdown, ignore.case = TRUE), "Markdown hides unavailable posthoc status")
  check(length(bundle$result$warnings) > 0L, "Unavailable posthoc lacks warning")
})

summary_path <- file.path(work, "results.json")
jsonlite::write_json(list(schema_version = 1L, suite = "phase2-design", modules = modules, test_pattern = test_pattern, numeric_checks = numeric_checks, tests = results,
  environment = list(r = R.version.string, packages = as.list(vapply(required, function(p) as.character(utils::packageVersion(p)), character(1))))), summary_path, auto_unbox = TRUE, pretty = TRUE, digits = NA)
passed <- sum(vapply(results, function(result) result$passed, logical(1)))
cat(sprintf("Phase 2 design: %d/%d cases passed; %d numeric comparisons. Results: %s\n", passed, length(results), numeric_checks, summary_path))
if (!nzchar(forced_root) && keep > 0L && dir.exists(output_base)) {
  candidates <- list.dirs(output_base, full.names = TRUE, recursive = FALSE)
  candidates <- sort(candidates[grepl("^[0-9]{14}$", basename(candidates))], decreasing = TRUE)
  if (length(candidates) > keep) for (path in setdiff(candidates[seq.int(keep + 1L, length(candidates))], run_root)) {
    if (identical(dirname(path), output_base) && grepl("^[0-9]{14}$", basename(path))) unlink(path, recursive = TRUE)
  }
}
quit(status = if (length(results) > 0L && passed == length(results)) 0 else 1)
