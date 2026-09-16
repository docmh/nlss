#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Independent public-CLI acceptance: no NLSS scientific helper or historical golden.
args <- commandArgs(TRUE)
script <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])
repo <- normalizePath(file.path(dirname(script[1]), "../.."), winslash = "/")
if ("--help" %in% args) {
  cat("Usage: Rscript tests/phase2/run_mixed_model_tests.R [--root PATH] [--keep N] [--match REGEX]\n",
      "Honors NLSS_TEST_ROOT, NLSS_KEEP_RUNS and NLSS_TESTS_CONFIG; private offline fixtures/configuration.\n", sep = "")
  quit(status = 0)
}
if (length(args) %% 2L || (length(args) && any(!args[seq(1L, length(args), 2L)] %in% c("--root", "--keep", "--match")))) stop("Unknown/incomplete runner option")
arg <- function(name, fallback) {
  i <- which(args == name)
  if (length(i) > 1L) stop("Repeated runner option: ", name)
  if (length(i)) args[i + 1L] else fallback
}
required <- c("yaml", "jsonlite", "arrow", "digest", "haven", "lme4", "lmerTest", "pbkrtest", "car", "emmeans", "performance")
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
work <- file.path(run_root, "phase2-mixed-models", paste0("run-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid()))
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
  results[[length(results) + 1L]] <<- list(module = "mixed_models", test = name, passed = is.null(error),
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
new_run <- function(context, options, failure = FALSE, module = "mixed_models", source = TRUE) {
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

# Unequal clusters, nonmonotonic numeric identifiers, reordered factor levels,
# crossed item and nested school effects, with missings in distinct source rows.
set.seed(601296)
sizes <- rep(c(5L, 8L, 6L, 9L, 7L), 6L)
subject_index <- rep(seq_along(sizes), sizes)
data <- data.frame(id = rep(sample(seq(101, 971, by = 30)), sizes),
  visit = unlist(lapply(sizes, seq_len)),
  group = factor(rep(rep(c("C", "B", "A"), 10), sizes), levels = c("B", "C", "A", "unused")),
  school = factor(rep(rep(LETTERS[1:6], each = 5), sizes)),
  item = factor(rep(paste0("item-", c(4, 1, 6, 2, 7, 3, 5)), length.out = sum(sizes))))
data$x <- (data$visit - 4) / 2 + stats::rnorm(nrow(data), sd = .12)
data$z <- stats::rnorm(nrow(data))
data$sex <- factor(rep(c("F", "M", "M", "F", "F"), length.out = nrow(data)), levels = c("M", "F"))
intercepts <- stats::rnorm(length(sizes), sd = 1.2)
slopes <- stats::rnorm(length(sizes), sd = .35)
data$score <- 4 + .65 * data$x - .3 * data$z + c(A = .8, B = -.3, C = .2)[as.character(data$group)] +
  .4 * (data$group == "A") * data$x + .2 * (data$sex == "F") + intercepts[subject_index] + slopes[subject_index] * data$x +
  rep(stats::rnorm(6, sd = .6), times = as.numeric(tapply(sizes, rep(1:6, each = 5), sum))) +
  stats::rnorm(7, sd = .4)[data$item] + stats::rnorm(nrow(data), sd = .65)
data$score[c(3, 47)] <- NA_real_; data$x[c(8, 112)] <- NA_real_; data$group[82] <- NA; data$id[169] <- NA_real_
base_formula <- "score ~ x * group + z + (1 | id)"
base_options <- c("--formula", base_formula, "--type", "III", "--df-method", "satterthwaite", "--diagnostics", "TRUE")
ddf_label <- function(method) switch(method, satterthwaite = "Satterthwaite", `kenward-roger` = "Kenward-Roger", none = "lme4")
reference_fit <- function(data, formula = base_formula, method = "satterthwaite", reml = TRUE, type = "III", optimizer = "bobyqa", maxfun = 100000L) {
  form <- stats::as.formula(formula)
  needed <- all.vars(form)
  for (name in needed) if (!is.numeric(data[[name]])) data[[name]] <- droplevels(as.factor(data[[name]]))
  data <- data[stats::complete.cases(data[needed]), , drop = FALSE]
  data <- droplevels(data)
  # Keep package-native grouping conversion and declared fixed-factor coding.
  fit <- if (method == "none") lme4::lmer(form, data = data, REML = reml, control = lme4::lmerControl(optimizer = optimizer, optCtrl = list(maxfun = maxfun))) else
    lmerTest::lmer(form, data = data, REML = reml, control = lme4::lmerControl(optimizer = optimizer, optCtrl = list(maxfun = maxfun)))
  fit
}
check_model <- function(bundle, fit, method = "satterthwaite", conf_level = .95, standardize = "none", original = data) {
  summary <- if (method == "none") summary(fit) else summary(fit, ddf = ddf_label(method))
  coefficients <- coef(summary); all_fixed <- bundle$result$results$fixed_effects_df
  dropped <- attr(lme4::getME(fit, "X"), "col.dropped")
  check(identical(all_fixed$term, c(rownames(coefficients), names(dropped))), "Fixed-effect term order differs")
  if (length(dropped)) {
    absent <- all_fixed[all_fixed$term %in% names(dropped), , drop = FALSE]
    check(all(is.na(absent$estimate)) && all(absent$status == "unavailable") && all(nzchar(absent$reason)), "Dropped terms lack explicit unavailable result rows")
  }
  actual <- all_fixed[all_fixed$term %in% rownames(coefficients), , drop = FALSE]
  df <- if ("df" %in% colnames(coefficients)) coefficients[, "df"] else rep(NA_real_, nrow(coefficients))
  p <- if ("Pr(>|t|)" %in% colnames(coefficients)) coefficients[, "Pr(>|t|)"] else rep(NA_real_, nrow(coefficients))
  critical <- if (method == "none") stats::qnorm((1 + conf_level) / 2) else stats::qt((1 + conf_level) / 2, df)
  check_row(actual, list(estimate = coefficients[, 1], se = coefficients[, 2], df = df, t = coefficients[, "t value"], p = p,
    ci_low = coefficients[, 1] - critical * coefficients[, 2], ci_high = coefficients[, 1] + critical * coefficients[, 2]), "Fixed effects")
  rows <- as.integer(rownames(stats::model.frame(fit)))
  near(bundle$request$design$included_rows, rows, "Included source rows", tolerance = 0)
  check(identical(as.integer(bundle$request$design$excluded_rows), setdiff(seq_len(nrow(original)), rows)), "Excluded source rows differ")
  expected_matrix <- lme4::getME(fit, "X")
  check(identical(bundle$request$design$model_matrix$columns, colnames(expected_matrix)), "Fitted fixed-design column order differs")
  near(bundle$request$design$model_matrix$rank, qr(expected_matrix)$rank, "Fitted fixed-design rank", tolerance = 0)
  raw_design <- jsonlite::fromJSON(bundle$request_path, simplifyVector = FALSE)$design
  grouping <- raw_design$grouping
  expected_groups <- lme4::getME(fit, "flist")
  original_group_name <- function(name) {
    for (source in names(raw_design$grouping_aliases)) name <- gsub(raw_design$grouping_aliases[[source]], source, name, fixed = TRUE)
    name
  }
  group_names <- vapply(grouping, function(item) original_group_name(item$name), character(1))
  check(setequal(group_names, names(expected_groups)), "Fitted grouping-term inventory differs")
  for (name in names(expected_groups)) {
    item <- grouping[[which(group_names == name)]]
    actual_ids <- as.integer(unlist(item$row_group_ids)); expected_ids <- as.integer(expected_groups[[name]])
    check(length(actual_ids) == length(expected_ids) && identical(outer(actual_ids, actual_ids, "=="), outer(expected_ids, expected_ids, "==")), paste("Actual fitted cluster partition differs", name))
    for (level in item$levels) {
      check(identical(as.integer(unlist(level$source_rows)), rows[actual_ids == level$level_id]), paste("Cluster source-row map differs", name))
      check(nzchar(level$value_hex), paste("Cluster identity encoding missing", name))
    }
  }
  # Direct variance components are checked, including random-slope correlations.
  vc <- lme4::VarCorr(fit); random <- bundle$result$results$random_effects_df
  random$group <- vapply(random$group, original_group_name, character(1))
  for (group in names(vc)) {
    block <- vc[[group]]
    for (term in rownames(block)) {
      row <- random[random$group == group & random$term == term, , drop = FALSE]
      check(nrow(row) == 1L, paste("Missing random variance", group, term))
      check_row(row, list(variance = block[term, term], stddev = sqrt(block[term, term])), paste("Random variance", group, term))
    }
    if (nrow(block) > 1L) for (i in seq_len(nrow(block) - 1L)) for (j in (i + 1L):nrow(block)) {
      term <- paste0("corr(", rownames(block)[i], ",", rownames(block)[j], ")")
      row <- random[random$group == group & random$term == term, , drop = FALSE]
      check(nrow(row) == 1L, paste("Missing random correlation", group, term))
      near(row$corr, stats::cov2cor(block)[i, j], paste("Random correlation", group, term))
    }
  }
  residual <- random[random$group == "Residual" & random$term == "Residual", , drop = FALSE]
  check(nrow(residual) == 1L, "Residual variance component missing")
  check_row(residual, list(variance = stats::sigma(fit)^2, stddev = stats::sigma(fit)), "Residual variance")
  fit_row <- bundle$result$results$fit_df
  check_row(fit_row, list(n = stats::nobs(fit), aic = stats::AIC(fit), bic = stats::BIC(fit), logLik = as.numeric(stats::logLik(fit)),
    deviance = suppressWarnings(stats::deviance(fit))), "Model fit")
  check(identical(fit_row$reml, lme4::isREML(fit)), "Fit criterion ML/REML status differs")
  check(grepl(if (lme4::isREML(fit)) "REML criterion" else "ML deviance", fit_row$criterion, fixed = TRUE), "Legacy deviance field lacks its actual criterion label")
  if (standardize == "predictors") {
    model_data <- original[rows, , drop = FALSE]; response <- stats::model.response(stats::model.frame(fit))
    beta <- vapply(seq_len(nrow(coefficients)), function(i) {
      term <- rownames(coefficients)[i]
      if (term %in% names(model_data) && is.numeric(model_data[[term]])) coefficients[i, 1] * stats::sd(model_data[[term]]) / stats::sd(response) else NA_real_
    }, numeric(1))
    near(actual$std_beta, beta, "Descriptive standardized numeric main-effect coefficients")
  } else check(all(is.na(actual$std_beta)), "Unrequested standardization produced estimates")
  invisible(fit)
}
check_omnibus <- function(bundle, fit, type = "III", method = "satterthwaite") {
  expected <- if (method != "none") stats::anova(fit, type = type, ddf = ddf_label(method)) else if (type == "I") stats::anova(fit) else car::Anova(fit, type = type)
  expected <- as.data.frame(expected); actual <- bundle$result$results$anova_df
  check(identical(as.character(actual$term), as.character(rownames(expected))), paste("Omnibus term inventory differs", type, method))
  if (nrow(expected) == 0L) return(invisible(NULL))
  for (name in names(expected)) if (is.numeric(expected[[name]])) check_row(actual, setNames(list(expected[[name]]), name), paste("Omnibus", type, method))
}
check_r2_icc <- function(bundle, fit) {
  r2 <- suppressWarnings(performance::r2_nakagawa(fit)); icc <- suppressWarnings(performance::icc(fit))
  check_row(bundle$result$results$r2_df, list(r2_marginal = unname(r2$R2_marginal), r2_conditional = unname(r2$R2_conditional)), "Nakagawa R squared")
  check_row(bundle$result$results$icc_df, list(icc = unname(icc$ICC_adjusted), icc_adjusted = unname(icc$ICC_adjusted), icc_unadjusted = unname(icc$ICC_unadjusted)), "Adjusted and unadjusted ICC")
}
check_diagnostics <- function(bundle, fit, shapiro = TRUE) {
  actual <- bundle$result$results$diagnostics_df
  singular <- actual[actual$metric == "singular_fit", , drop = FALSE]
  check(nrow(singular) == 1L && identical(singular$value, as.character(lme4::isSingular(fit))), "Singularity diagnostic differs")
  if (shapiro) {
    sw <- stats::shapiro.test(stats::residuals(fit)); row <- actual[actual$metric == "shapiro_wilk", , drop = FALSE]
    check(nrow(row) == 1L, "Residual normality result missing")
    check_row(row, list(statistic = unname(sw$statistic), p = sw$p.value), "Residual Shapiro-Wilk")
  }
}
check_emmeans <- function(bundle, fit, term, method = "satterthwaite", contrasts = "none", adjust = "holm", conf_level = .95, seed = 1L, contrast_args = list()) {
  set.seed(seed)
  emm <- emmeans::emmeans(fit, stats::as.formula(paste("~", term)), lmer.df = if (method == "none") "asymptotic" else method)
  expected <- as.data.frame(summary(emm, infer = c(TRUE, TRUE), level = conf_level))
  actual <- bundle$result$results$emmeans_df
  check(nrow(actual) == nrow(expected), "Marginal-means grid size differs")
  columns <- setdiff(names(emm@grid), ".wgt.")
  level <- apply(expected[, columns, drop = FALSE], 1, function(row) paste(paste0(columns, "=", row), collapse = ", "))
  check(identical(actual$level, unname(level)), "Marginal-means factor-grid row identities differ")
  check(identical(bundle$request$design$inference$emmeans_df, if (method == "none") "asymptotic" else method), "Marginal-means denominator-df method not audited")
  interval_names <- if ("lower.CL" %in% names(expected)) c("lower.CL", "upper.CL") else c("asymp.LCL", "asymp.UCL")
  statistic <- if ("t.ratio" %in% names(expected)) "t.ratio" else "z.ratio"
  check_row(actual, list(emmean = expected$emmean, se = expected$SE, df = expected$df, t = expected[[statistic]], p = expected$p.value,
    ci_low = expected[[interval_names[1]]], ci_high = expected[[interval_names[2]]]), "Estimated marginal means")
  if (!identical(contrasts, "none")) {
    object <- do.call(emmeans::contrast, c(list(object = emm, method = contrasts), contrast_args))
    expected <- summary(object, infer = c(TRUE, TRUE), level = conf_level, adjust = adjust)
    actual <- bundle$result$results$contrasts_df
    check(identical(actual$contrast, as.character(expected$contrast)), "Contrast inventory/order differs")
    interval_names <- if ("lower.CL" %in% names(expected)) c("lower.CL", "upper.CL") else c("asymp.LCL", "asymp.UCL")
    statistic <- if ("t.ratio" %in% names(expected)) "t.ratio" else "z.ratio"
    check_row(actual, list(estimate = expected$estimate, se = expected$SE, df = expected$df, t = expected[[statistic]],
      ci_low = expected[[interval_names[1]]], ci_high = expected[[interval_names[2]]]), "Estimated contrasts")
    near_probability(if (adjust == "none") actual$p else actual$p_adj, expected$p.value, "Contrast probabilities")
    check(all(actual$p_adjust_requested == adjust) && all(actual$p_adjust_effective == attr(expected, "adjust")), "Requested/effective contrast adjustment differs")
    near(bundle$request$design$contrast_adjustment$family_size, nrow(expected), "Declared contrast family size", tolerance = 0)
  }
  invisible(emm)
}

for (method in c("satterthwaite", "kenward-roger")) for (type in c("I", "II", "III")) test(
  paste0("lmm_factorial_", type, "_", method, if (method == "satterthwaite" && type == "I") "_smoke" else ""), {
    context <- new_case(paste0("factorial-", type, "-", method), data)
    bundle <- new_run(context, replace_options(base_options, c("--type", type, "--df-method", method)))
    fit <- reference_fit(data, method = method, type = type)
    check_model(bundle, fit, method); check_omnibus(bundle, fit, type, method)
    check_r2_icc(bundle, fit); check_diagnostics(bundle, fit)
    check(identical(bundle$request$options$type, type), "Requested omnibus type lost")
    check(identical(bundle$request$options$df_method, method), "Requested coefficient DF method lost")
    if (type == "I" && method == "satterthwaite") {
      type3 <- as.data.frame(stats::anova(fit, type = "III", ddf = "Satterthwaite"))
      type1 <- as.data.frame(stats::anova(fit, type = "I", ddf = "Satterthwaite"))
      check(any(abs(type1$`F value` - type3$`F value`) > .01), "Fixture does not distinguish sequential Type I from Type III")
      replay(context, bundle)
    }
  })
for (type in c("I", "II", "III")) test(paste0("lmm_no_denominator_df_", type), {
  context <- new_case(paste0("none-", type), data)
  bundle <- new_run(context, replace_options(base_options, c("--type", type, "--df-method", "none")))
  fit <- reference_fit(data, method = "none", type = type)
  check_model(bundle, fit, "none"); check_omnibus(bundle, fit, type, "none")
  check(all(is.na(bundle$result$results$fixed_effects_df$p)), "df-method none fabricates fixed-effect p values")
  check(grepl("normal|Wald|asymptotic", bundle$markdown, ignore.case = TRUE), "No-df normal-Wald interval method hidden")
})
for (method in c("satterthwaite", "none")) test(paste0("lmm_ml_", method), {
  context <- new_case(paste0("ml-", method), data)
  bundle <- new_run(context, replace_options(base_options, c("--reml", "FALSE", "--df-method", method, "--conf-level", ".9")))
  fit <- reference_fit(data, method = method, reml = FALSE)
  check_model(bundle, fit, method, conf_level = .9); check_omnibus(bundle, fit, "III", method)
  check(identical(bundle$request$options$reml, FALSE), "ML request recorded as REML")
})
structures <- list(
  random_slope = "score ~ x * group + z + (1 + x | id)",
  independent_random_slope = "score ~ x * group + z + (1 + x || id)",
  crossed_intercepts = "score ~ x * group + z + (1 | id) + (1 | item)",
  nested_intercepts = "score ~ x + z + group + (1 | school/id)",
  random_intercept_only = "score ~ 1 + (1 | id)",
  factor_interaction = "score ~ x + group * sex + (1 | id)")
for (name in names(structures)) test(paste0("lmm_", name, if (name == "random_slope") "_smoke" else ""), {
  formula <- structures[[name]]; context <- new_case(name, data)
  bundle <- new_run(context, replace_options(base_options, c("--formula", formula)))
  fit <- reference_fit(data, formula)
  check_model(bundle, fit); check_omnibus(bundle, fit); check_r2_icc(bundle, fit); check_diagnostics(bundle, fit)
  if (name == "random_slope") replay(context, bundle)
})
test("lmm_builder_fixed_random_matches_full_formula", {
  context <- new_case("builder", data)
  options <- c("--dv", "score", "--fixed", "x,z,group", "--random", "1|id,1|item", "--type", "II")
  bundle <- new_run(context, options)
  fit <- reference_fit(data, "score ~ x + z + group + (1 | id) + (1 | item)", type = "II")
  check_model(bundle, fit); check_omnibus(bundle, fit, "II")
})
test("lmm_builder_intercept_only", {
  context <- new_case("builder-intercept", data)
  bundle <- new_run(context, c("--dv", "score", "--random", "1|id"))
  fit <- reference_fit(data, "score ~ 1 + (1 | id)")
  check_model(bundle, fit)
})
test("lmm_formula_overrides_builder_and_unselected_bad_values", {
  sample <- data; sample$unused <- Inf
  context <- new_case("formula-priority", sample)
  bundle <- new_run(context, c(base_options, "--dv", "unused", "--fixed", "does_not_exist", "--random", "bad|missing"))
  check_model(bundle, reference_fit(data), original = sample)
})
test("lmm_standardized_numeric_main_effects_are_descriptive", {
  context <- new_case("standardization", data)
  bundle <- new_run(context, c(base_options, "--standardize", "predictors"))
  check_model(bundle, reference_fit(data), standardize = "predictors")
  check(grepl("standard|SD", bundle$markdown, ignore.case = TRUE), "Standardization method not reported")
})
test("lmm_transformed_predictor_uses_actual_model_frame_rows", {
  sample <- data; sample$positive <- exp(sample$z); sample$positive[c(19, 29)] <- NA_real_
  formula <- "score ~ log(positive) + I(x^2) + group + (1 | id)"
  context <- new_case("transform-predictor", sample)
  bundle <- new_run(context, replace_options(base_options, c("--formula", formula)))
  fit <- reference_fit(sample, formula)
  check_model(bundle, fit, original = sample); check_omnibus(bundle, fit)
  check(grepl("log\\(positive\\)", jsonlite::toJSON(bundle$request$design$model_frame, auto_unbox = TRUE)), "Transformed model-frame column not preserved")
})
test("lmm_unsupported_transformed_response_rejected_explicitly", {
  sample <- data; sample$positive_score <- exp(sample$score / 5)
  formula <- "log(positive_score) ~ x + z + group + (1 | id)"
  context <- new_case("transform-response", sample)
  log <- run_module("mixed_models", c(context$flag, context$input, replace_options(base_options, c("--formula", formula))), failure = TRUE)
  check(grepl("response|dependent|simple|transform", text_file(log), ignore.case = TRUE), "Unsupported transformed response lacks an informative error")
})
for (format in c("csv", "parquet")) test(paste0("lmm_", format, "_source"), {
  sample <- data; sample$group <- as.character(sample$group); sample$sex <- as.character(sample$sex)
  context <- new_case(paste0("source-", format), sample, format)
  bundle <- new_run(context, base_options)
  check_model(bundle, reference_fit(sample), original = sample)
})
test("lmm_sav_variable_value_labels_user_missing_and_numeric_covariates_smoke", {
  sample <- data
  sample$group <- haven::labelled_spss(match(as.character(sample$group), c("B", "C", "A")),
    labels = c("Baseline B" = 1, "Study C" = 2, "Treatment A" = 3, "Missing treatment" = 99), na_values = 99, label = "Study allocation")
  sample$group[15] <- 99
  sample$x <- haven::labelled_spss(sample$x, labels = c("Reference dose" = 0), na_values = 999, label = "Observed dose")
  sample$x[25] <- 999
  sample$score <- haven::labelled_spss(sample$score, na_values = 999, label = "Outcome score")
  sample$score[35] <- 999
  sample$id <- haven::labelled_spss(sample$id, label = "Participant identifier")
  context <- new_case("spss-labelled", sample, "sav")
  formula <- "score ~ x * factor(group) + z + (1 | id)"
  bundle <- new_run(context, replace_options(base_options, c("--formula", formula)))
  plain <- data
  plain$group <- match(as.character(data$group), c("B", "C", "A")); plain$group[15] <- NA
  plain$x[25] <- NA; plain$score[35] <- NA
  fit <- reference_fit(plain, formula)
  check_model(bundle, fit, original = plain); check_omnibus(bundle, fit)
  dictionary <- text_file(file.path(context$project, bundle$request$dataset$dictionary_path))
  check(all(vapply(c("Baseline B", "Outcome score", "Observed dose", "Study allocation"), grepl, logical(1), x = dictionary, fixed = TRUE)), "SAV labels lost from immutable dictionary")
  check(grepl("Outcome score", bundle$markdown, fixed = TRUE), "Outcome variable label absent from LMM Markdown")
  replay(context, bundle)
})
test("lmm_numeric_labelled_predictor_remains_numeric_without_role_change", {
  sample <- data; sample$x <- haven::labelled(sample$x, c("Zero" = 0), label = "Numeric dose")
  context <- new_case("labelled-numeric", sample)
  bundle <- new_run(context, base_options)
  check_model(bundle, reference_fit(data), original = data)
  check(sum(bundle$result$results$fixed_effects_df$term == "x") == 1L, "Numeric labelled predictor silently became categorical")
})
test("lmm_nearby_numeric_cluster_ids_remain_distinct", {
  sample <- data; identities <- sort(unique(stats::na.omit(sample$id)))
  index <- match(sample$id, identities); sample$id <- 1 + index * 1e-15
  context <- new_case("nearby-clusters", sample)
  bundle <- new_run(context, base_options)
  expected <- data; expected$id <- factor(index)
  fit <- reference_fit(expected)
  check_model(bundle, fit, original = sample)
  check(length(unique(stats::na.omit(sample$id))) == length(identities), "Near-ID fixture collapsed before fitting")
  replay(context, bundle)
})
test("lmm_literal_na_cluster_id_distinct_from_missing", {
  sample <- data; sample$id <- as.character(sample$id)
  first <- sample$id[which(!is.na(sample$id))[1]]; sample$id[sample$id == first & !is.na(sample$id)] <- "NA"
  context <- new_case("literal-na-cluster", sample)
  bundle <- new_run(context, base_options)
  check_model(bundle, reference_fit(sample), original = sample)
})
test("lmm_space_containing_response_predictor_and_group_names", {
  sample <- data
  names(sample)[match(c("score", "x", "id"), names(sample))] <- c("observed score", "dose value", "participant code")
  formula <- "`observed score` ~ `dose value` * group + z + (1 | `participant code`)"
  context <- new_case("spaces", sample)
  bundle <- new_run(context, replace_options(base_options, c("--formula", formula)))
  check_model(bundle, reference_fit(sample, formula), original = sample)
})
for (method in c("satterthwaite", "kenward-roger", "none")) test(paste0("lmm_emmeans_pairwise_", method, if (method == "kenward-roger") "_smoke" else ""), {
  context <- new_case(paste0("emmeans-", method), data)
  bundle <- new_run(context, replace_options(base_options, c("--df-method", method, "--emmeans", "group", "--contrasts", "pairwise", "--p-adjust", "holm", "--conf-level", ".9")))
  fit <- reference_fit(data, method = method)
  check_model(bundle, fit, method, conf_level = .9)
  check_emmeans(bundle, fit, "group", method, "pairwise", "holm", .9)
  if (method == "kenward-roger") replay(context, bundle)
})
test("lmm_emmeans_grid_without_contrasts", {
  context <- new_case("emmeans-grid", data)
  bundle <- new_run(context, replace_options(base_options, c("--emmeans", "group*sex", "--formula", "score ~ x + group * sex + (1 | id)")))
  fit <- reference_fit(data, "score ~ x + group * sex + (1 | id)")
  check_emmeans(bundle, fit, "group*sex")
  check(is.null(bundle$result$results$contrasts_df) || NROW(bundle$result$results$contrasts_df) == 0L, "Unrequested contrast results appeared")
})
for (adjust in c("none", "bonferroni", "tukey")) test(paste0("lmm_emmeans_pairwise_adjustment_", adjust), {
  context <- new_case(paste0("adjust-", adjust), data)
  bundle <- new_run(context, c(base_options, "--emmeans", "group", "--contrasts", "pairwise", "--p-adjust", adjust))
  check_emmeans(bundle, reference_fit(data), "group", contrasts = "pairwise", adjust = adjust)
})
test("lmm_builtin_nonpairwise_effective_adjustment_visible", {
  context <- new_case("poly-tukey", data)
  bundle <- new_run(context, c(base_options, "--emmeans", "group", "--contrasts", "poly", "--p-adjust", "tukey"))
  check_emmeans(bundle, reference_fit(data), "group", contrasts = "poly", adjust = "tukey")
  check(grepl("sidak", bundle$markdown, ignore.case = TRUE), "Non-pairwise Tukey-to-Sidak replacement hidden in Markdown")
  check(grepl("sidak", jsonlite::toJSON(bundle$request$design, auto_unbox = TRUE), ignore.case = TRUE), "Effective Sidak adjustment missing from design")
})
test("lmm_custom_named_ordered_contrasts_frozen_replay", {
  context <- new_case("custom-frozen", data)
  file <- file.path(context$base, "contrasts.json")
  spec <- list(term = "group", contrasts = list(B_minus_C = list(B = 1, C = -1, A = 0), A_minus_B = c(-1, 0, 1)))
  jsonlite::write_json(spec, file, auto_unbox = TRUE)
  bundle <- new_run(context, c(base_options, "--contrasts", "custom", "--contrast-file", file, "--p-adjust", "holm"))
  check_emmeans(bundle, reference_fit(data), "group", contrasts = list(B_minus_C = c(1, -1, 0), A_minus_B = c(-1, 0, 1)))
  check(grepl("B_minus_C", jsonlite::toJSON(bundle$request$design$contrast_spec, auto_unbox = TRUE), fixed = TRUE), "Custom contrast weights not frozen in design")
  jsonlite::write_json(list(term = "does_not_exist", contrasts = list(changed = c(90, -80, -10))), file, auto_unbox = TRUE)
  replay(context, bundle)
})
test("lmm_builtin_contrast_json_reference_argument", {
  context <- new_case("builtin-json", data)
  file <- file.path(context$base, "contrasts.json")
  jsonlite::write_json(list(term = "group", method = "trt.vs.ctrl", args = list(ref = "C")), file, auto_unbox = TRUE)
  bundle <- new_run(context, c(base_options, "--contrast-file", file, "--p-adjust", "bonferroni"))
  check_emmeans(bundle, reference_fit(data), "group", contrasts = "trt.vs.ctrl", adjust = "bonferroni", contrast_args = list(ref = "C"))
})
test("lmm_nonestimable_marginal_means_and_contrasts_remain_visible", {
  sample <- data[is.na(data$group) | data$group != "A" | data$sex != "F", , drop = FALSE]
  rownames(sample) <- NULL
  formula <- "score ~ x + group * sex + (1 | id)"
  context <- new_case("nonestimable-emmeans", sample)
  bundle <- new_run(context, c("--formula", formula, "--type", "I", "--df-method", "none", "--emmeans", "group*sex", "--contrasts", "pairwise", "--p-adjust", "holm"))
  fit <- suppressWarnings(reference_fit(sample, formula, "none", type = "I"))
  check_model(bundle, fit, "none", original = sample)
  check_emmeans(bundle, fit, "group*sex", "none", "pairwise")
  means <- bundle$result$results$emmeans_df; contrasts <- bundle$result$results$contrasts_df
  check(any(is.na(means$emmean)) && all(means$status[is.na(means$emmean)] == "unavailable"), "Unestimable grid rows disappeared or lack status")
  check(any(is.na(contrasts$estimate)) && all(contrasts$status[is.na(contrasts$estimate)] == "unavailable"), "Unestimable planned contrasts disappeared or lack status")
  section <- strsplit(bundle$markdown, "# Mixed Models: Marginal Means", fixed = TRUE)[[1]]
  check(length(section) == 2L && grepl("unavailable|not estimable|non.estim|nonEst", section[2], ignore.case = TRUE), "Unavailable marginal means/contrasts hidden from their Markdown section")
})
test("lmm_invalid_custom_contrasts_do_not_replace_prior_results", {
  context <- new_case("invalid-custom-contrasts", data)
  new_run(context, base_options)
  file <- file.path(context$base, "contrasts.json")
  for (spec in list(list(term = "group", contrasts = list(short = c(1, -1))),
    list(term = "group", contrasts = list(unknown = list(B = 1, C = -1, absent = 0))),
    list(term = "group", method = "invented.method"))) {
    jsonlite::write_json(spec, file, auto_unbox = TRUE)
    failed(context, c(base_options, "--contrast-file", file))
  }
})
test("lmm_seeded_multivariate_t_adjustment_replays", {
  context <- new_case("seeded-mvt", data)
  bundle <- new_run(context, c(base_options, "--emmeans", "group", "--contrasts", "pairwise", "--p-adjust", "mvt", "--seed", "367"))
  check_emmeans(bundle, reference_fit(data), "group", contrasts = "pairwise", adjust = "mvt", seed = 367)
  check(identical(bundle$request$options$seed, 367L), "Explicit RNG seed not resolved")
  replay(context, bundle)
})
test("lmm_diagnostics_disabled_keeps_fit_warning_audit", {
  context <- new_case("diagnostics-off", data)
  bundle <- new_run(context, replace_options(base_options, c("--diagnostics", "FALSE")))
  check_model(bundle, reference_fit(data)); check_omnibus(bundle, reference_fit(data))
  diagnostics <- bundle$result$results$diagnostics_df
  check(is.null(diagnostics) || !"shapiro_wilk" %in% diagnostics$metric, "Disabled residual diagnostics still ran")
  check(length(bundle$request$design$fit_status) > 0L, "Fit status audit disappeared with optional diagnostics")
})
test("lmm_shapiro_sample_limit_disclosed", {
  context <- new_case("shapiro-limit", data)
  bundle <- new_run(context, c(base_options, "--max-shapiro-n", "10"))
  actual <- bundle$result$results$diagnostics_df
  if ("shapiro_wilk" %in% actual$metric) check(all(is.na(actual$p[actual$metric == "shapiro_wilk"])), "Shapiro limit ignored")
  check(grepl("Shapiro", bundle$markdown, ignore.case = TRUE) && grepl("skip|limit|not.*computed|not.*performed|unavailable", bundle$markdown, ignore.case = TRUE), "Skipped residual test hidden from Markdown")
})
for (optimizer in c("bobyqa", "nloptwrap")) test(paste0("lmm_optimizer_budget_", optimizer, "_is_effective_and_audited"), {
  sample <- lme4::sleepstudy
  budget <- if (optimizer == "bobyqa") 80L else 1L
  context <- new_case(paste0("optimizer-budget-", optimizer), sample)
  formula <- "Reaction ~ Days + (Days | Subject)"
  bundle <- new_run(context, c("--formula", formula, "--optimizer", optimizer, "--maxfun", as.character(budget), "--df-method", "none", "--type", "I", "--diagnostics", "FALSE"))
  control <- lme4::lmerControl(optimizer = optimizer, optCtrl = if (optimizer == "bobyqa") list(maxfun = budget) else list(maxeval = budget))
  fit <- suppressWarnings(lme4::lmer(stats::as.formula(formula), sample, control = control))
  check_model(bundle, fit, "none", original = sample)
  status <- bundle$request$design$fit_status
  near(status$convergence_code, fit@optinfo$conv$opt, "Actual optimizer exit code", tolerance = 0)
  near(status$function_evaluations, fit@optinfo$feval, "Actual optimizer evaluation count", tolerance = 0)
  key <- if (optimizer == "bobyqa") "maxfun" else "maxeval"
  near(status$optimizer_control[[key]], budget, "Optimizer-specific evaluation budget", tolerance = 0)
  check(grepl("converg|optim|budget|maximum|limit", bundle$markdown, ignore.case = TRUE) && length(bundle$result$warnings) > 0L, "Optimizer stop hidden from Markdown/warnings")
  check(bundle$result$results$diagnostics_df$value[bundle$result$results$diagnostics_df$metric == "convergence"] != "ok", "Nonzero optimizer code misclassified as convergence ok")
})
test("lmm_singular_fit_visible_even_without_optional_diagnostics", {
  sample <- expand.grid(visit = 1:6, id = 1:24)
  sample$x <- sample$visit
  sample$score <- 2 + .7 * sample$x + rep(c(-.5, .2, .6, -.2, -.4, .3), 24)
  context <- new_case("singular", sample)
  formula <- "score ~ x + (1 | id)"
  bundle <- new_run(context, c("--formula", formula, "--df-method", "none", "--type", "I", "--diagnostics", "FALSE"))
  fit <- suppressWarnings(reference_fit(sample, formula, "none", type = "I"))
  check(lme4::isSingular(fit), "Fixture is not singular")
  check_model(bundle, fit, "none", original = sample)
  check(grepl("singular|boundary", bundle$markdown, ignore.case = TRUE), "Singular fit hidden in Markdown")
  check(length(bundle$result$warnings) > 0L && grepl("singular|boundary", jsonlite::toJSON(bundle$request$design$fit_status, auto_unbox = TRUE), ignore.case = TRUE), "Singular fit lacks mandatory machine-readable audit")
})
test("lmm_rank_deficient_fixed_effects_are_not_silently_dropped", {
  sample <- data; sample$x_duplicate <- 2 * sample$x
  context <- new_case("rank-deficient", sample)
  formula <- "score ~ x + x_duplicate + z + (1 | id)"
  bundle <- new_run(context, c("--formula", formula, "--type", "I"))
  fit <- suppressWarnings(reference_fit(sample, formula, type = "I"))
  check_model(bundle, fit, original = sample)
  check(grepl("rank.deficien|dropped|non.estim", bundle$markdown, ignore.case = TRUE), "Dropped fixed-effect column hidden from Markdown")
  check(length(bundle$request$design$model_matrix$dropped) > 0L, "Dropped design columns not preserved")
})
test("lmm_config_defaults_cli_precedence", {
  context <- new_case("config-override", data)
  yaml::write_yaml(list(modules = list(mixed_models = list(type = "II", reml = FALSE, df_method = "none", seed = 729L))), private_config)
  bundle <- new_run(context, c("--formula", base_formula, "--type", "I", "--df-method", "satterthwaite", "--emmeans", "group"))
  fit <- reference_fit(data, type = "I", reml = FALSE)
  check_model(bundle, fit); check_omnibus(bundle, fit, "I")
  check(identical(bundle$request$options$reml, FALSE) && identical(bundle$request$options$seed, 729L), "Configuration defaults ignored")
})
test("lmm_privacy_frozen_input_template_configuration_replay_smoke", {
  context <- new_case("privacy-replay", data)
  yaml::write_yaml(list(defaults = list(digits = 5L), logging = list(include_user_prompt = FALSE, include_outputs = FALSE)), private_config)
  template <- file.path(context$base, "custom.md")
  writeLines(c(paste0("# ", cfg$template_marker), "{{table_body}}", "{{narrative}}", "{{note_body}}"), template)
  bundle <- new_run(context, c(base_options, "--emmeans", "group", "--template", template, "--log", "FALSE", "--user-prompt", "PRIVATE_MIXED_MODEL_PROMPT"))
  check(grepl(cfg$template_marker, bundle$markdown, fixed = TRUE), "Private custom template not used")
  check(!grepl("PRIVATE_MIXED_MODEL_PROMPT", text_file(bundle$request_path), fixed = TRUE), "Prompt privacy ignored")
  check(!grepl(context$base, text_file(bundle$request_path), fixed = TRUE), "External input/template path leaked")
  check(length(bundle$result$results) > 0L, "Legacy include_outputs removed mandatory result")
  check_model(bundle, reference_fit(data))
  check(identical(bundle$request$options$seed, 1L), "Canonical seed not resolved")
  logfile <- file.path(context$dataset, "analysis_log.jsonl")
  if (file.exists(logfile)) check(!grepl('"module":"mixed_models"', gsub(" ", "", text_file(logfile)), fixed = TRUE), "Legacy log optout ignored")
  writeLines("CHANGED ORIGINAL TEMPLATE", template)
  working <- file.path(context$dataset, "sample.parquet"); current <- arrow::read_parquet(working, as_data_frame = TRUE)
  current$score[1] <- 123456; arrow::write_parquet(current, working)
  source_data <- data; source_data$score[1] <- -999; saveRDS(source_data, context$input)
  yaml::write_yaml(list(defaults = list(digits = 1L), modules = list(mixed_models = list(seed = 981L, type = "I", reml = FALSE))), private_config)
  replay(context, bundle)
  near(arrow::read_parquet(working, as_data_frame = TRUE)$score[1], 123456, "Replay preserved changed working data", tolerance = 0)
})
test("lmm_failed_model_preserves_prior_publication", {
  sample <- data; sample$constant <- 4
  context <- new_case("failed-protection", sample)
  new_run(context, base_options)
  failed(context, c("--formula", "constant ~ x + (1 | id)"))
  failed(context, c("--formula", "score ~ x + (1 | group)", "--contrasts", "custom", "--emmeans", "group", "--contrast-file", file.path(context$base, "missing.json")))
})
invalid <- list(type = c("--type", "IV"), df_method = c("--df-method", "invented"), confidence_one = c("--conf-level", "1"),
  confidence_nan = c("--conf-level", "NaN"), seed_negative = c("--seed", "-1"), seed_fractional = c("--seed", "1.5"),
  maxfun_zero = c("--maxfun", "0"), maxfun_fractional = c("--maxfun", "10.5"), shapiro_zero = c("--max-shapiro-n", "0"),
  reml_boolean = c("--reml", "nonsense"), standardization = c("--standardize", "all"), adjustment = c("--p-adjust", "invented"),
  digits_fractional = c("--digits", "1.5"), missing_random = c("--formula", "score ~ x + group"),
  kr_requires_reml = c("--df-method", "kenward-roger", "--reml", "FALSE"),
  contrasts_without_grid = c("--contrasts", "pairwise"))
for (name in names(invalid)) test(paste0("lmm_invalid_", name), {
  context <- new_case(paste0("invalid-", name), data)
  run_module("mixed_models", c(context$flag, context$input, replace_options(base_options, invalid[[name]])), failure = TRUE)
  check(!any(vapply(runs(context), function(path) identical(read_json(file.path(path, "result.json"))$status, "completed"), logical(1))), "Invalid request published a completed run")
})
for (name in c("response_infinity", "predictor_infinity", "group_single_level", "all_missing_response", "transformed_nonfinite")) test(paste0("lmm_invalid_data_", name), {
  sample <- data; formula <- base_formula
  if (name == "response_infinity") sample$score[1] <- Inf
  if (name == "predictor_infinity") sample$x[1] <- Inf
  if (name == "group_single_level") sample$id <- 1
  if (name == "all_missing_response") sample$score <- NA_real_
  if (name == "transformed_nonfinite") { sample$positive <- exp(sample$z); sample$positive[1] <- 0; formula <- "score ~ log(positive) + (1 | id)" }
  context <- new_case(paste0("invalid-data-", name), sample)
  run_module("mixed_models", c(context$flag, context$input, replace_options(base_options, c("--formula", formula))), failure = TRUE)
  check(!any(vapply(runs(context), function(path) identical(read_json(file.path(path, "result.json"))$status, "completed"), logical(1))), "Invalid data were silently dropped or published")
})

summary_path <- file.path(work, "results.json")
jsonlite::write_json(list(schema_version = 1L, suite = "phase2-mixed-models", modules = "mixed_models", test_pattern = test_pattern,
  numeric_checks = numeric_checks, tests = results,
  source_sha256 = sha(file.path(repo, "scripts/R/mixed_models.R")),
  test_sha256 = sha(normalizePath(script[1], winslash = "/")),
  environment = list(r = R.version.string, packages = as.list(vapply(required, function(p) as.character(utils::packageVersion(p)), character(1))))),
  summary_path, auto_unbox = TRUE, pretty = TRUE, digits = NA)
passed <- sum(vapply(results, function(result) result$passed, logical(1)))
cat(sprintf("Phase 2 mixed models: %d/%d cases passed; %d numeric comparisons. Results: %s\n", passed, length(results), numeric_checks, summary_path))
if (!nzchar(forced_root) && keep > 0L && dir.exists(output_base)) {
  candidates <- list.dirs(output_base, full.names = TRUE, recursive = FALSE)
  candidates <- sort(candidates[grepl("^[0-9]{14}$", basename(candidates))], decreasing = TRUE)
  if (length(candidates) > keep) for (path in setdiff(candidates[seq.int(keep + 1L, length(candidates))], run_root)) {
    if (identical(dirname(path), output_base) && grepl("^[0-9]{14}$", basename(path))) unlink(path, recursive = TRUE)
  }
}
quit(status = if (length(results) > 0L && passed == length(results)) 0 else 1)
