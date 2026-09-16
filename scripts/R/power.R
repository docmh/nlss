# SPDX-License-Identifier: Apache-2.0
#!/usr/bin/env Rscript

bootstrap_dir <- {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", cmd_args[grep("^--file=", cmd_args)])
  if (length(file_arg) && nzchar(file_arg[1])) dirname(normalizePath(file_arg[1], winslash = "/", mustWork = FALSE)) else getwd()
}
source(file.path(bootstrap_dir, "lib", "bootstrap.R"))
nlss_bootstrap()

print_usage <- function() {
  cat(paste(c(
    "Power analysis and auditable study planning (pwr + semPower)", "",
    "Rscript power.R --analysis ttest --effect-size 0.5 --power 0.8",
    "Rscript power.R --analysis anova --mode sensitivity --groups 3 --n-per-group 30",
    "Rscript power.R --analysis sem --sem-df 120 --rmsea0 0.05 --rmsea1 0.08",
    "Rscript power.R --sav survey.sav --analysis correlation --estimate-effect TRUE --x x --y y", "",
    "--analysis ttest/anova/correlation/regression/sem; --mode apriori/posthoc/sensitivity",
    "--planning TRUE/FALSE: force parameter-only planning / dataset-backed execution.",
    "Without a source or --dataset-name, parameter-only calculations use project/planning.",
    "--csv/--sav/--rds/--rdata/--parquet PATH; --df NAME selects an RData object.",
    "--sem-df NUMBER: SEM degrees of freedom; numeric --df remains an alias without --rdata.",
    "--effect-size NUMBER; --effect-metric auto/d/f/f2/r/eta2/r2/rmsea; --effect-basis TEXT",
    "--alpha NUMBER; --power NUMBER; --alternative two.sided/greater/less",
    "--t-type one-sample/two-sample/paired; --ratio n2/n1; --mu NUMBER",
    "--n/--n-total NUMBER; --n-per-group NUMBER; --n1 NUMBER; --n2 NUMBER",
    "--groups NUMBER (ANOVA); --u NUMBER (regression numerator df)",
    "--rmsea0 NUMBER; --rmsea1 NUMBER (SEM null/alternative RMSEA)",
    "--estimate-effect TRUE/FALSE; --vars LIST; --group/--between NAME; --x NAME; --y NAME; --dv NAME; --ivs LIST",
    "--sep VALUE; --header TRUE/FALSE; --csv-decimal/--csv-encoding/--csv-col-types/--csv-na-values VALUE",
    "--dataset-name NAME; --import-action verify/new-version; --digits NUMBER; --template REF",
    "--log TRUE/FALSE (legacy JSONL only); --user-prompt TEXT; --interactive; --help",
    "Defaults: scripts/config.yml; validated overrides: NLSS_CONFIG_PATH."
  ), collapse = "\n"), "\n")
}

interactive_options <- function() {
  input <- tolower(prompt("Input type (planning/csv/sav/rds/rdata/parquet)", "planning"))
  opts <- list()
  if (input == "planning") opts$planning <- TRUE else {
    if (!input %in% c("csv", "sav", "rds", "rdata", "parquet")) stop("Unsupported input type.")
    opts[[input]] <- prompt("Input path")
    if (input == "rdata") opts$df <- prompt("Data frame object name")
    if (input == "csv") {
      opts$sep <- prompt("Separator", get_config_value("defaults.csv.sep"))
      opts$header <- prompt("Header TRUE/FALSE", get_config_value("defaults.csv.header"))
    }
  }
  ask_default <- function(key) prompt(key, get_config_value(paste0("modules.power.", gsub("-", "_", key))))
  for (key in c("analysis", "mode", "effect-metric", "alpha")) opts[[key]] <- ask_default(key)
  analysis <- tolower(opts$analysis)
  if (!analysis %in% c("ttest", "anova", "correlation", "regression", "sem")) stop("Interactive analysis must be ttest/anova/correlation/regression/sem.")
  mode <- tolower(opts$mode)
  if (!mode %in% c("apriori", "posthoc", "sensitivity")) stop("Interactive mode must be apriori/posthoc/sensitivity.")
  if (mode != "posthoc") opts$power <- ask_default("power")
  if (analysis %in% c("ttest", "correlation")) opts$alternative <- ask_default("alternative")
  if (analysis == "ttest") {
    opts[["t-type"]] <- ask_default("t-type")
    if (!opts[["t-type"]] %in% c("one-sample", "two-sample", "paired")) stop("Interactive t-type must be one-sample/two-sample/paired.")
    if (opts[["t-type"]] == "two-sample") opts$ratio <- ask_default("ratio")
    if (opts[["t-type"]] == "one-sample") opts$mu <- ask_default("mu")
  }
  opts[["estimate-effect"]] <- if (input != "planning" && analysis != "sem") ask_default("estimate-effect") else FALSE
  estimate <- parse_bool(opts[["estimate-effect"]])
  if (analysis == "anova" && !estimate) opts$groups <- ask_default("groups")
  if (analysis == "regression" && !estimate) opts$u <- ask_default("u")
  if (analysis == "sem") {
    opts[["sem-df"]] <- prompt("sem-df")
    opts$rmsea0 <- ask_default("rmsea0")
    if (mode != "sensitivity") opts$rmsea1 <- ask_default("rmsea1")
  }
  if (!estimate && analysis != "sem" && mode != "sensitivity") opts[["effect-size"]] <- prompt("effect-size")
  opts[["effect-basis"]] <- prompt("effect-basis (optional)", "")
  if (mode != "apriori") {
    sizes <- c("n", if (analysis %in% c("ttest", "anova")) "n-per-group",
      if (analysis == "ttest" && opts[["t-type"]] == "two-sample") c("n1", "n2"))
    for (key in sizes) opts[[key]] <- prompt(paste0(key, " (optional)"), "")
  }
  if (estimate) {
    roles <- switch(analysis, ttest = switch(opts[["t-type"]], `one-sample` = "vars", `two-sample` = c("vars", "group"), paired = c("x", "y")),
      anova = c("dv", "group"), correlation = c("x", "y"), regression = c("dv", "ivs"))
    for (key in roles) opts[[key]] <- prompt(key)
  }
  for (key in c("template", "user-prompt")) opts[[key]] <- prompt(paste0(key, " (optional)"), "")
  opts$digits <- prompt("digits", get_config_value("defaults.digits"))
  opts$log <- prompt("log TRUE/FALSE", get_config_value("defaults.log"))
  opts[vapply(opts, function(x) length(x) == 1L && nzchar(as.character(x)), logical(1))]
}

power_number <- function(value, name, default = NA_real_, integer = FALSE, minimum = -Inf, maximum = Inf) {
  if (is.null(value)) return(default)
  number <- suppressWarnings(as.numeric(value))
  if (is.logical(value) || length(number) != 1L || !is.finite(number) ||
      number < minimum || number > maximum || (integer && number != floor(number))) {
    stop("--", name, " must be a finite ", if (integer) "integer" else "number", " in [", minimum, ", ", maximum, "].")
  }
  number
}

power_choice <- function(value, name, aliases) {
  value <- tolower(as.character(value))
  match <- names(aliases)[vapply(aliases, function(x) length(value) == 1L && value %in% x, logical(1))]
  if (!length(match)) stop("Unsupported --", name, ": ", paste(value, collapse = ", "))
  match[1]
}

power_effect_metric <- function(value, analysis) {
  if (tolower(value) == "auto") return(c(ttest = "d", anova = "f", correlation = "r", regression = "f2", sem = "rmsea")[[analysis]])
  power_choice(value, "effect-metric", list(d = c("d", "cohen_d", "cohen-d"), f = c("f", "cohen_f"),
    f2 = c("f2", "f^2", "cohen_f2", "cohen-f2"), r = c("r", "rho"), eta2 = c("eta2", "eta^2", "eta", "etasq"),
    r2 = c("r2", "r^2"), rmsea = "rmsea"))
}

power_effect_estimate <- function(df, analysis, t_type, vars, group, x, y, dv, ivs, mu) {
  selected <- switch(analysis, ttest = if (t_type == "paired") c(x, y) else c(vars, if (t_type == "two-sample") group),
    correlation = c(x, y), anova = c(dv, group), regression = c(dv, ivs))
  expected <- switch(analysis, ttest = if (t_type == "one-sample") 1L else 2L, correlation = 2L, anova = 2L, regression = length(ivs) + 1L)
  if (length(selected) != expected || any(!nzchar(selected)) || anyDuplicated(selected) ||
      (analysis == "regression" && !length(ivs))) stop("Provide distinct, complete variable roles for effect estimation.")
  absent <- setdiff(selected, names(df))
  if (length(absent)) stop("Effect-estimation variables not found: ", paste(absent, collapse = ", "))
  numeric_vars <- switch(analysis, ttest = if (t_type == "paired") c(x, y) else vars, correlation = c(x, y), anova = dv, regression = dv)
  for (v in numeric_vars) if (!is.numeric(df[[v]])) stop("Effect estimation requires a numeric variable: ", v, ". Labels do not define its statistical role.")
  data <- df[, selected, drop = FALSE]
  for (v in selected) {
    if (is.numeric(data[[v]]) && any(!is.finite(data[[v]]) & !is.na(data[[v]]))) stop("Non-finite values in effect-estimation variable: ", v)
  }
  keep <- complete.cases(data)
  data <- droplevels(data[keep, , drop = FALSE])
  if (nrow(data) < 2L) stop("Not enough complete cases for effect estimation.")
  audit <- list(included_rows = which(keep), excluded_rows = which(!keep), source_n = nrow(df), n = nrow(data),
    variables = selected, variable_types = lapply(df[selected], class), labels = resolve_label_metadata(df),
    missing = "joint complete cases over all selected roles; non-finite nonmissing values rejected")
  if (analysis == "ttest") {
    if (t_type == "two-sample") {
      g <- droplevels(as.factor(data[[group]]))
      if (nlevels(g) != 2L) stop("Grouping variable must have exactly two observed levels.")
      values <- split(data[[vars]], g)
      sizes <- lengths(values)
      if (any(sizes < 2L)) stop("At least two complete cases are required per group.")
      means <- vapply(values, mean, numeric(1)); sds <- vapply(values, sd, numeric(1))
      denominator <- sqrt(sum((sizes - 1) * sds^2) / (sum(sizes) - 2))
      effect <- (means[1] - means[2]) / denominator
      audit <- c(audit, list(n1 = unname(sizes[1]), n2 = unname(sizes[2]), groups = levels(g),
        group_rows = lapply(levels(g), function(level) which(keep)[g == level]), means = means, sds = sds,
        denominator = denominator, definition = "(mean(group1) - mean(group2)) / pooled within-group SD"))
    } else {
      values <- if (t_type == "paired") data[[x]] - data[[y]] else data[[vars]] - mu
      denominator <- sd(values)
      effect <- mean(values) / denominator
      audit <- c(audit, list(mean_difference = mean(values), denominator = denominator,
        definition = if (t_type == "paired") "d_z = mean(x - y) / SD(x - y); n counts pairs" else "d = (mean(x) - mu) / SD(x)"))
    }
    if (!is.finite(effect)) stop("Effect size unavailable: the relevant standard deviation is zero or non-finite.")
    return(c(audit, list(metric = "d", value = unname(effect), d = unname(effect))))
  }
  if (analysis == "correlation") {
    if (nrow(data) < 4L) stop("At least four complete pairs are required for pwr correlation power.")
    effect <- cor(data[[x]], data[[y]], method = "pearson")
    if (!is.finite(effect) || abs(effect) >= 1) stop("Correlation effect must be finite and strictly between -1 and 1.")
    return(c(audit, list(metric = "r", value = effect, r = effect, definition = "Pearson product-moment correlation")))
  }
  # Safe temporary names avoid interpreting user variable names as formula syntax.
  model_data <- data.frame(response = data[[dv]])
  if (analysis == "anova") {
    model_data$group <- droplevels(as.factor(data[[group]]))
    if (nlevels(model_data$group) < 2L) stop("ANOVA effect estimation requires at least two observed groups.")
    model <- lm(response ~ group, data = model_data)
    audit$groups <- nlevels(model_data$group)
    audit$group_levels <- levels(model_data$group)
    audit$group_counts <- as.list(table(model_data$group))
    audit$group_rows <- lapply(levels(model_data$group), function(level) which(keep)[model_data$group == level])
  } else {
    for (i in seq_along(ivs)) model_data[[paste0("predictor", i)]] <- data[[ivs[i]]]
    model <- lm(response ~ ., data = model_data)
    audit$predictor_mapping <- setNames(ivs, paste0("predictor", seq_along(ivs)))
    audit$factor_levels <- model$xlevels
    audit$contrasts <- model$contrasts
  }
  if (df.residual(model) <= 0 || model$rank < 2L) stop("Effect-estimation model needs nonzero model and residual degrees of freedom.")
  r2 <- summary(model)$r.squared
  if (!is.finite(r2) || r2 < 0 || r2 >= 1) stop("Effect-estimation R-squared must be in [0, 1).")
  audit$model_rank <- model$rank
  audit$u <- model$rank - 1L
  audit$residual_df <- df.residual(model)
  audit$aliased_coefficients <- names(coef(model))[is.na(coef(model))]
  audit$model_matrix <- model.matrix(model)
  audit$coefficients <- coef(model)
  if (analysis == "anova") return(c(audit, list(metric = "eta2", value = r2, eta2 = r2, f = sqrt(r2 / (1 - r2)),
    definition = "one-way eta-squared = between-group SS / total SS; converted to Cohen f for balanced-design planning")))
  c(audit, list(metric = "r2", value = r2, r2 = r2, f2 = r2 / (1 - r2),
    definition = "omnibus intercept-only versus full-model R-squared; f-squared = R-squared / (1 - R-squared), u = fitted rank minus one"))
}

power_n_root <- function(fun, target, lower = 2, upper_limit = 1e9) {
  low_power <- fun(lower)
  if (!is.finite(low_power)) stop("Power is non-finite at the minimum admissible sample size.")
  if (low_power >= target) return(lower)
  upper <- max(4, 2 * lower)
  repeat {
    upper_power <- fun(upper)
    if (!is.finite(upper_power)) stop("Power is non-finite while bracketing sample size.")
    if (upper_power >= target) break
    if (upper >= upper_limit) stop("Target power cannot be attained within the numerical sample-size bound (1e9). Check effect direction and assumptions.")
    upper <- min(upper_limit, 2 * upper)
  }
  uniroot(function(n) fun(n) - target, c(lower, upper), tol = 1e-8)$root
}

power_sem_result <- function(n, df, alpha, rmsea0, rmsea1, lower_tail = rmsea1 < rmsea0) {
  # semPower's public RMSEA interface tests exact fit. Retain that package result,
  # but use the requested noncentral null to calculate a close/not-close-fit test.
  # MacCallum et al. (1996); CRAN semTools::findRMSEApower documents both tails.
  reference <- if (rmsea1 > 0) semPower::semPower.postHoc(effect = rmsea1, effect.measure = "RMSEA", alpha = alpha, N = n, df = df) else NULL
  ncp1 <- if (is.null(reference)) 0 else reference$ncp
  ncp0 <- (n - 1) * df * rmsea0^2
  critical <- qchisq(alpha, df = df, ncp = ncp0, lower.tail = lower_tail)
  list(power = pchisq(critical, df = df, ncp = ncp1, lower.tail = lower_tail), N = n, df = df,
    rmsea0 = rmsea0, rmsea1 = rmsea1, null_ncp = ncp0, alternative_ncp = ncp1, critical_value = critical,
    tail = if (lower_tail) "lower" else "upper", exact_fit_reference = if (is.null(reference)) NULL else unclass(reference),
    exact_fit_reference_status = if (is.null(reference)) "RMSEA=0 boundary; noncentrality is exactly zero" else "semPower exact-fit reference; not the requested noncentral-null power")
}

format_power_number <- function(value, digits) {
  if (length(value) != 1L || is.na(value)) return("")
  format(round(value, digits), nsmall = digits, trim = TRUE)
}

build_power_table_body <- function(summary_df, digits, table_spec = NULL) {
  keys <- c("analysis", "mode", "effect_metric", "effect_size", "alpha", "power", "attained_power", "n_total", "n_per_group", "n1", "n2", "groups", "ratio", "u", "df", "r2", "rmsea0", "rmsea1", "t_type", "alternative", "effect_source")
  labels <- c("Analysis", "Mode", "Effect", "Effect size", "alpha", "Power", "Attained power", "N", "n/group", "n1", "n2", "k", "Ratio", "u", "df", "R²", "RMSEA0", "RMSEA1", "t type", "Alternative", "Effect source")
  defaults <- lapply(seq_along(keys), function(i) list(key = keys[i], label = labels[i], drop_if_empty = i > 6L))
  columns <- normalize_table_columns(table_spec$columns, defaults)
  rows <- lapply(seq_len(nrow(summary_df)), function(i) vapply(columns, function(col) {
    value <- summary_df[[col$key]][i]
    if (!length(value) || is.na(value)) return("")
    if (col$key %in% c("n_total", "n_per_group", "n1", "n2", "groups", "u", "df")) return(format(value, scientific = FALSE, trim = TRUE))
    if (is.numeric(value)) return(format_power_number(value, digits))
    as_cell_text(value)
  }, character(1)))
  filtered <- drop_empty_columns(columns, rows)
  render_markdown_table(vapply(filtered$columns, function(x) x$label, character(1)), filtered$rows)
}

build_power_narrative <- function(row, digits) {
  number <- function(x) format_power_number(x, digits)
  sample <- if (is.finite(row$n1)) paste0("N = ", row$n_total, " (n1 = ", row$n1, ", n2 = ", row$n2, ")") else
    if (is.finite(row$n_per_group)) paste0("N = ", row$n_total, " (", row$n_per_group, " per group; k = ", row$groups, ")") else
      paste0(if (row$t_type == "paired") "complete pairs = " else "N = ", row$n_total)
  effect <- paste0(row$effect_metric, " = ", number(row$effect_size))
  analysis <- switch(row$analysis, ttest = paste(row$t_type, "t-test"), anova = "balanced one-way ANOVA", correlation = "Pearson correlation", regression = "omnibus multiple regression", sem = "SEM RMSEA test")
  if (row$mode == "apriori") return(paste0("A priori power analysis for ", analysis, " (", effect, ", alpha = ", number(row$alpha),
    ", target power = ", number(row$power_target), ") yielded ", sample, "; attained power after rounding = ", number(row$attained_power), "."))
  if (row$mode == "posthoc") return(paste0("Post hoc power analysis for ", analysis, " with ", sample, " (", effect, ", alpha = ", number(row$alpha), ") yielded power = ", number(row$power), "."))
  paste0("Sensitivity analysis for ", analysis, " with ", sample, " (alpha = ", number(row$alpha), ", target power = ", number(row$power_target), ") yielded a detectable effect of ", effect, ".")
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (!length(args) && is.null(nlss_run_context$replay)) { print_usage(); stop("Supply power-analysis options or --interactive.") }
  if ("--help" %in% args) { print_usage(); return(invisible(NULL)) }
  opts <- nlss_run_options(args, "power")
  if (isTRUE(opts[["interactive"]])) opts <- interactive_options()
  value <- function(key, config = gsub("-", "_", key)) {
    if (!is.null(opts[[key]])) opts[[key]] else get_config_value(paste0("modules.power.", config))
  }
  number <- function(key, config = TRUE, ...) power_number(if (config) value(key) else opts[[key]], key, ...)
  analysis <- power_choice(value("analysis"), "analysis", list(ttest = c("t", "ttest", "t-test", "t_test"), anova = c("anova", "aov"), correlation = c("correlation", "cor", "corr"), regression = c("regression", "regress", "lm"), sem = c("sem", "cfa")))
  mode <- power_choice(value("mode"), "mode", list(apriori = c("apriori", "a-priori", "a_priori", "a priori"), posthoc = c("posthoc", "post-hoc", "post_hoc", "achieved"), sensitivity = c("sensitivity", "detectable")))
  alternative <- power_choice(value("alternative"), "alternative", list(two.sided = c("two.sided", "two-sided", "two"), greater = "greater", less = "less"))
  t_type <- power_choice(value("t-type"), "t-type", list(`one-sample` = c("one-sample", "one_sample", "onesample"), `two-sample` = c("two-sample", "two_sample", "independent", "between"), paired = c("paired", "pair")))
  applicability <- list(groups = analysis == "anova", u = analysis == "regression", `sem-df` = analysis == "sem",
    rmsea0 = analysis == "sem", rmsea1 = analysis == "sem", alternative = analysis %in% c("ttest", "correlation"),
    `t-type` = analysis == "ttest", ratio = analysis == "ttest" && t_type == "two-sample", mu = analysis == "ttest" && t_type == "one-sample")
  inactive <- names(applicability)[!unlist(applicability)]
  supplied_inactive <- intersect(names(opts), inactive)
  if (length(supplied_inactive)) stop("Options not applicable to this analysis/design: ", paste0("--", supplied_inactive, collapse = ", "))
  effect_metric <- power_effect_metric(value("effect-metric"), analysis)
  allowed <- list(ttest = "d", anova = c("f", "eta2"), correlation = "r", regression = c("f2", "r2"), sem = "rmsea")
  if (!effect_metric %in% allowed[[analysis]]) stop("Unsupported effect metric for ", analysis, ": ", effect_metric)
  alpha <- number("alpha", minimum = 0, maximum = 1)
  target <- number("power", minimum = 0, maximum = 1)
  if (alpha <= 0 || alpha >= 1 || target <= 0 || target >= 1) stop("Alpha and target power must lie strictly between 0 and 1.")
  if (mode != "posthoc" && target <= alpha) stop("Planning/sensitivity target power must exceed alpha.")
  ratio <- number("ratio", minimum = .Machine$double.eps)
  mu <- number("mu")
  groups <- number("groups", integer = TRUE, minimum = 2)
  u <- number("u", integer = TRUE, minimum = 1)
  rmsea0 <- number("rmsea0", minimum = 0)
  rmsea1 <- number("rmsea1", minimum = 0)
  estimate <- parse_bool(value("estimate-effect"))
  digits <- power_number(if (is.null(opts[["digits"]])) get_config_value("defaults.digits") else opts[["digits"]], "digits", integer = TRUE, minimum = 0, maximum = 15)
  effect_input <- number("effect-size", FALSE)
  notes <- character(0)

  if (analysis == "sem" && !is.null(opts[["df"]]) && is.null(opts[["rdata"]])) {
    legacy_df <- power_number(opts[["df"]], "df", integer = TRUE, minimum = 1)
    if (!is.null(opts[["sem-df"]]) && legacy_df != power_number(opts[["sem-df"]], "sem-df")) stop("Conflicting --df and --sem-df.")
    opts[["sem-df"]] <- legacy_df
    opts[["df"]] <- NULL
  }
  if (!is.null(opts[["df"]]) && is.null(opts[["rdata"]])) stop("--df selects an object only with --rdata; use --sem-df for SEM degrees of freedom.")
  sem_df <- power_number(opts[["sem-df"]], "sem-df", integer = TRUE, minimum = 1)
  if (analysis == "sem") {
    if (estimate) stop("SEM RMSEA power does not support --estimate-effect.")
    if (is.na(sem_df)) stop("SEM power requires --sem-df (legacy numeric --df without --rdata is accepted).")
    if (!is.na(effect_input)) {
      if (!is.null(opts[["rmsea1"]]) && effect_input != rmsea1) stop("Conflicting --effect-size and --rmsea1.")
      rmsea1 <- effect_input
    }
    if (rmsea1 < 0 || (mode != "sensitivity" && rmsea1 == rmsea0)) stop("SEM RMSEA alternatives must be nonnegative and differ from the null.")
  }

  has_source <- any(vapply(c("csv", "sav", "rds", "rdata", "parquet"), function(key) !is.null(opts[[key]]), logical(1)))
  if (!has_source && !is.null(opts[["dataset-name"]])) stop("--dataset-name names an explicit source import; it does not select an existing dataset. Supply a source or run from the intended dataset folder with --planning FALSE.")
  csv_options <- intersect(names(opts), c("sep", "header", "csv-decimal", "csv-encoding", "csv-col-types", "csv-na-values"))
  if (length(csv_options) && is.null(opts[["csv"]])) stop("CSV reader options require an explicit --csv input: ", paste0("--", csv_options, collapse = ", "))
  if (!has_source && !is.null(opts[["import-action"]])) stop("--import-action requires an explicit source input.")
  role_options <- intersect(names(opts), c("vars", "group", "between", "x", "y", "dv", "ivs"))
  if (estimate) {
    allowed_roles <- switch(analysis, ttest = switch(t_type, `one-sample` = "vars", `two-sample` = c("vars", "group", "between"), paired = c("x", "y")),
      anova = c("dv", "group", "between"), correlation = c("x", "y"), regression = c("dv", "ivs"))
    surplus_roles <- setdiff(role_options, allowed_roles)
    if (length(surplus_roles)) stop("Effect-estimation roles not applicable to this analysis/design: ", paste0("--", surplus_roles, collapse = ", "))
  } else if (length(role_options)) notes <- c(notes, paste0("Variable roles supplied as context (", paste0("--", role_options, collapse = ", "),
    ") are not used: --estimate-effect is FALSE; the supplied effect or solved sensitivity threshold drives this calculation."))
  planning <- if (!is.null(opts[["planning"]])) parse_bool(opts[["planning"]]) else !has_source && !estimate
  replay <- nlss_run_context$replay
  if (!is.null(replay) && is.null(replay$request$input)) planning <- FALSE
  if (planning && (has_source || estimate)) stop("--planning TRUE cannot be combined with a dataset source/name or --estimate-effect TRUE.")
  # Persist the effective boundary: replay must never auto-select another active dataset.
  opts[["planning"]] <- planning
  if (planning) {
    nlss_begin_planning_run("power", opts)
    df <- NULL
  } else {
    df <- nlss_load_input(opts)
    nlss_begin_run("power", df, opts)
  }
  out_dir <- nlss_run_context$out_dir
  effect_basis <- nlss_mask_prose_paths(if (is.null(opts[["effect-basis"]])) "" else as.character(opts[["effect-basis"]]), nlss_run_context$root)
  package <- if (analysis == "sem") "semPower" else "pwr"
  if (!requireNamespace(package, quietly = TRUE)) stop("Power analysis requires the '", package, "' package.")

  n_inputs <- list(n_total = number("n", FALSE, integer = TRUE, minimum = 2),
    n_per_group = number("n-per-group", FALSE, integer = TRUE, minimum = 2),
    n1 = number("n1", FALSE, integer = TRUE, minimum = 2), n2 = number("n2", FALSE, integer = TRUE, minimum = 2))
  n_alias <- number("n-total", FALSE, integer = TRUE, minimum = 2)
  if (!is.na(n_inputs$n_total) && !is.na(n_alias) && n_inputs$n_total != n_alias) stop("Conflicting --n and --n-total.")
  if (is.na(n_inputs$n_total)) n_inputs$n_total <- n_alias
  n_total <- n_inputs$n_total; n_per_group <- n_inputs$n_per_group; n1 <- n_inputs$n1; n2 <- n_inputs$n2
  if ((analysis != "ttest" || t_type != "two-sample") && (!is.na(n1) || !is.na(n2))) stop("--n1 and --n2 apply only to two-sample t-tests.")
  if (!analysis %in% c("ttest", "anova") && !is.na(n_per_group)) stop("--n-per-group applies only to t-tests and ANOVA.")
  vars <- parse_list(opts[["vars"]]); ivs <- parse_list(opts[["ivs"]])
  text <- function(key) if (is.null(opts[[key]])) "" else as.character(opts[[key]])
  group <- text("group")
  if (nzchar(text("between"))) {
    if (nzchar(group) && group != text("between")) stop("Conflicting --group and --between.")
    group <- text("between")
  }
  estimation <- NULL
  effect_source <- if (estimate) "estimated" else "user"
  if (estimate) {
    if (!is.na(effect_input)) stop("Supply an effect size OR --estimate-effect TRUE, not both.")
    estimation <- power_effect_estimate(df, analysis, t_type, vars, group, text("x"), text("y"), text("dv"), ivs, mu)
    effect_input <- estimation$value
    effect_metric <- estimation$metric
    if (analysis == "anova") {
      if (!is.null(opts[["groups"]]) && groups != estimation$groups) stop("--groups conflicts with the observed ANOVA effect-estimation groups.")
      groups <- estimation$groups
      if (length(unique(unlist(estimation$group_counts))) > 1L) notes <- c(notes, "Observed groups are unequal; ANOVA power uses a balanced-design approximation, not an exact unequal-group power analysis.")
    }
    if (analysis == "regression") {
      if (!is.null(opts[["u"]]) && u != estimation$u) stop("--u conflicts with the omnibus effect-estimation model rank minus one.")
      u <- estimation$u
      if (length(estimation$aliased_coefficients)) notes <- c(notes, "Rank-deficient predictors: the estimable model rank determines numerator degrees of freedom; aliases are recorded.")
    }
    if (mode != "apriori" && all(is.na(unlist(n_inputs)))) {
      if (analysis == "ttest" && t_type == "two-sample") { n1 <- estimation$n1; n2 <- estimation$n2 } else n_total <- estimation$n
      notes <- c(notes, "Sample size derived from complete effect-estimation cases.")
    }
    notes <- c(notes, "Effect size is sample-estimated, not an independently justified population effect; sampling uncertainty is not incorporated in the power calculation.")
    notes <- c(notes, paste0("Effect-estimation sample: ", estimation$n, " complete cases from ", estimation$source_n,
      " source rows; ", length(estimation$excluded_rows), " excluded. Source-row identities, variable/value labels and the effect definition are retained in JSON."))
    if (analysis == "ttest" && t_type == "two-sample") notes <- c(notes,
      paste0("Signed d compares group 1 (", estimation$groups[1], ") minus group 2 (", estimation$groups[2], ")."))
  }
  if (analysis == "sem") effect_input <- rmsea1
  if (mode != "sensitivity" && is.na(effect_input)) stop("Effect size is required unless --estimate-effect TRUE.")
  if (mode == "sensitivity") {
    if (!is.na(effect_input) && analysis != "sem") notes <- c(notes, "Sensitivity solves the effect; any supplied/estimated effect is retained as context, not used as the detectable effect.")
    effect_source <- "solved"
  }
  effect_calc <- effect_input
  if (!is.na(effect_input)) {
    if (effect_metric %in% c("eta2", "r2")) {
      if (effect_input < 0 || effect_input >= 1) stop("eta-squared/R-squared must lie in [0, 1).")
      effect_calc <- effect_input / (1 - effect_input)
      if (effect_metric == "eta2") effect_calc <- sqrt(effect_calc)
      notes <- c(notes, paste0("Effect converted from ", effect_metric, " to ", if (effect_metric == "eta2") "f" else "f2", "."))
    }
    if (analysis == "correlation" && abs(effect_input) >= 1) stop("Correlation effect must lie strictly between -1 and 1.")
    if (analysis %in% c("anova", "regression") && effect_calc < 0) stop("ANOVA/regression effect size must be nonnegative.")
    if (analysis %in% c("ttest", "correlation") && alternative == "two.sided") effect_calc <- abs(effect_input)
    if (mode == "apriori" && analysis != "sem") {
      if (effect_calc == 0 || (analysis %in% c("ttest", "correlation") && ((alternative == "less" && effect_calc > 0) || (alternative == "greater" && effect_calc < 0)))) stop("A priori target above alpha is unattainable for a zero or opposite-direction effect.")
    }
  }

  if (mode == "apriori" && any(!is.na(unlist(n_inputs)))) notes <- c(notes, "A priori mode solves sample size; supplied sample sizes are recorded but do not constrain the solution.")
  if (mode != "apriori") {
    if (analysis == "ttest" && t_type == "two-sample") {
      if (xor(is.na(n1), is.na(n2))) stop("Supply both --n1 and --n2, or a total/per-group sample size.")
      if (!is.na(n1)) {
        if (!is.na(n_total) && n_total != n1 + n2) stop("Total sample size conflicts with n1 + n2.")
        if (!is.na(n_per_group) && n_per_group != n1) stop("--n-per-group conflicts with --n1.")
        if (!is.na(n_per_group) && n2 != ceiling(n_per_group * ratio)) stop("--n-per-group and --ratio conflict with --n2.")
        if (!is.null(opts[["ratio"]]) && abs(n2 / n1 - ratio) > 1e-8) stop("--ratio conflicts with n2 / n1.")
      } else if (!is.na(n_per_group)) {
        n1 <- n_per_group; n2 <- ceiling(n1 * ratio)
        if (!is.na(n_total) && n_total != n1 + n2) stop("Total sample size conflicts with per-group allocation.")
      } else if (!is.na(n_total)) {
        n1 <- ceiling(n_total / (1 + ratio)); n2 <- n_total - n1
      } else stop("Two-sample t-test requires --n1/--n2, --n-per-group or --n.")
      if (min(n1, n2) < 2) stop("Each t-test group requires at least two cases.")
      n_total <- n1 + n2
      if (abs(n2 / n1 - ratio) > 1e-8) notes <- c(notes, "The actual integer allocation differs from the requested/default ratio; requested and effective ratios are retained.")
    } else if (analysis == "anova") {
      if (is.na(n_per_group)) {
        if (is.na(n_total)) stop("ANOVA requires --n-per-group or --n.")
        n_per_group <- floor(n_total / groups)
        if (n_per_group * groups != n_total) notes <- c(notes, "ANOVA balanced-design approximation rounds total N down to a multiple of the number of groups; requested and effective N are retained.")
      } else if (!is.na(n_total) && n_total != n_per_group * groups) stop("Total N conflicts with groups times n-per-group.")
      if (n_per_group < 2) stop("ANOVA requires at least two observations per group.")
      n_total <- n_per_group * groups
    } else {
      if (!is.na(n1) || !is.na(n2)) stop("--n1 and --n2 apply only to two-sample t-tests.")
      if (!is.na(n_per_group)) {
        if (analysis != "ttest") stop("--n-per-group applies only to t-tests and ANOVA.")
        if (!is.na(n_total) && n_total != n_per_group) stop("Conflicting --n and --n-per-group.")
        n_total <- n_per_group
      }
      if (is.na(n_total)) stop("This calculation requires --n/--n-total or sample-based effect estimation.")
      if (analysis == "correlation" && n_total < 4) stop("Correlation power requires N >= 4.")
      if (analysis == "regression" && n_total <= u + 1) stop("Regression requires positive denominator degrees of freedom N - u - 1.")
    }
  }

  options <- list(analysis = analysis, mode = mode, planning = planning, effect_metric = effect_metric, effect_size = effect_input,
    effect_size_calc = effect_calc, effect_basis = effect_basis, alpha = alpha, power = target, t_type = if (analysis == "ttest") t_type else NULL,
    alternative = if (analysis %in% c("ttest", "correlation")) alternative else NULL, ratio = if (analysis == "ttest" && t_type == "two-sample") ratio else NULL,
    mu = if (analysis == "ttest" && t_type == "one-sample") mu else NULL, n_total = n_total, n_per_group = n_per_group, n1 = n1, n2 = n2,
    requested_sample_sizes = n_inputs, groups = if (analysis == "anova") groups else NULL, u = if (analysis == "regression") u else NULL,
    df = if (analysis == "sem") sem_df else NULL, rmsea0 = if (analysis == "sem") rmsea0 else NULL, rmsea1 = if (analysis == "sem") rmsea1 else NULL,
    estimate_effect = estimate, effect_source = effect_source, vars = vars, group = group, x = text("x"), y = text("y"), dv = text("dv"), ivs = ivs, digits = digits)
  design <- list(input_kind = if (planning) "parameters" else "dataset", effect_estimation = estimation,
    sample_unit = if (analysis == "ttest" && t_type == "paired") "complete pairs" else "observations",
    hypothesis = if (analysis == "sem") list(null_rmsea = rmsea0, alternative_rmsea = if (mode == "sensitivity") NULL else rmsea1,
      tail = if (mode != "sensitivity" && rmsea1 < rmsea0) "lower" else "upper") else NULL,
    notes = notes, scientific_scope = switch(analysis, ttest = "pooled-variance standardized t-test; paired uses difference-score SD", anova = "balanced one-way fixed-effects ANOVA", correlation = "Pearson correlation; pwr Fisher-transform approximation", regression = "omnibus fixed-model F-test, intercept plus u estimable predictors", sem = "single-sample asymptotic RMSEA close/not-close-fit test, noncentrality (N - 1) df RMSEA^2"))
  nlss_resolve_request(options, design)

  row <- list(analysis = analysis, mode = mode, effect_metric = effect_metric, effect_size = effect_input, alpha = alpha,
    power = NA_real_, power_target = if (mode == "posthoc") NA_real_ else target, attained_power = NA_real_,
    n_total = NA_real_, n_per_group = NA_real_, n1 = NA_real_, n2 = NA_real_, groups = NA_real_, ratio = NA_real_,
    u = NA_real_, df = NA_real_, r2 = if (effect_metric == "r2") effect_input else NA_real_,
    rmsea0 = if (analysis == "sem") rmsea0 else NA_real_, rmsea1 = if (analysis == "sem") rmsea1 else NA_real_,
    t_type = if (analysis == "ttest") t_type else "", alternative = if (analysis %in% c("ttest", "correlation")) alternative else "", effect_source = effect_source)
  continuous <- list(); raw <- NULL; attained <- NULL
  if (analysis == "ttest") {
    two <- t_type == "two-sample"
    engine <- if (two) "pwr::pwr.t2n.test" else "pwr::pwr.t.test"
    call <- function(n, effect = effect_calc, desired = NULL, second = NULL) {
      if (two) pwr::pwr.t2n.test(n1 = n, n2 = second, d = effect, sig.level = alpha, power = desired, alternative = alternative) else
        pwr::pwr.t.test(n = n, d = effect, sig.level = alpha, power = desired, type = if (t_type == "paired") "paired" else "one.sample", alternative = alternative)
    }
    if (mode == "apriori") {
      solved <- power_n_root(function(n) call(n, second = if (two) ratio * n else NULL)$power, target, lower = if (two) max(2, 2 / ratio) else 2)
      continuous <- if (two) list(n1 = solved, n2 = solved * ratio, n_total = solved * (1 + ratio)) else list(n_total = solved)
      if (two) { n1 <- ceiling(solved); n2 <- ceiling(n1 * ratio); n_total <- n1 + n2 } else n_total <- ceiling(solved)
      raw <- call(solved, second = if (two) ratio * solved else NULL)
    } else raw <- call(if (two) n1 else n_total, effect = if (mode == "sensitivity") NULL else effect_calc, desired = if (mode == "sensitivity") target else NULL, second = if (two) n2 else NULL)
    if (mode == "sensitivity") { row$effect_metric <- "d"; row$effect_size <- raw$d }
    attained <- call(if (two) n1 else n_total, effect = if (mode == "sensitivity") raw$d else effect_calc, second = if (two) n2 else NULL)
    if (two) { row$n1 <- n1; row$n2 <- n2; row$ratio <- n2 / n1 }
    row$n_total <- n_total
  } else if (analysis == "anova") {
    engine <- "pwr::pwr.anova.test"
    if (mode == "apriori") {
      solved <- power_n_root(function(n) pwr::pwr.anova.test(k = groups, n = n, f = effect_calc, sig.level = alpha)$power, target)
      continuous <- list(n_per_group = solved, n_total = solved * groups)
      n_per_group <- ceiling(solved)
      raw <- pwr::pwr.anova.test(k = groups, n = solved, f = effect_calc, sig.level = alpha)
    } else raw <- pwr::pwr.anova.test(k = groups, n = n_per_group, f = if (mode == "sensitivity") NULL else effect_calc, sig.level = alpha, power = if (mode == "sensitivity") target else NULL)
    if (mode == "sensitivity") { row$effect_metric <- "f"; row$effect_size <- raw$f }
    attained <- pwr::pwr.anova.test(k = groups, n = n_per_group, f = if (mode == "sensitivity") raw$f else effect_calc, sig.level = alpha)
    row$groups <- groups; row$n_per_group <- n_per_group; row$n_total <- groups * n_per_group
  } else if (analysis == "correlation") {
    engine <- "pwr::pwr.r.test"
    if (mode == "apriori") {
      solved <- power_n_root(function(n) pwr::pwr.r.test(n = n, r = effect_calc, sig.level = alpha, alternative = alternative)$power, target, lower = 4)
      continuous <- list(n_total = solved); n_total <- ceiling(solved)
      raw <- pwr::pwr.r.test(n = solved, r = effect_calc, sig.level = alpha, alternative = alternative)
    } else raw <- pwr::pwr.r.test(n = n_total, r = if (mode == "sensitivity") NULL else effect_calc, sig.level = alpha, power = if (mode == "sensitivity") target else NULL, alternative = alternative)
    if (mode == "sensitivity") { row$effect_metric <- "r"; row$effect_size <- raw$r }
    attained <- pwr::pwr.r.test(n = n_total, r = if (mode == "sensitivity") raw$r else effect_calc, sig.level = alpha, alternative = alternative)
    row$n_total <- n_total
  } else if (analysis == "regression") {
    engine <- "pwr::pwr.f2.test"
    if (mode == "apriori") {
      solved <- power_n_root(function(n) pwr::pwr.f2.test(u = u, v = n - u - 1, f2 = effect_calc, sig.level = alpha)$power, target, lower = u + 2)
      continuous <- list(n_total = solved, denominator_df = solved - u - 1); n_total <- ceiling(solved)
      raw <- pwr::pwr.f2.test(u = u, v = solved - u - 1, f2 = effect_calc, sig.level = alpha)
    } else raw <- pwr::pwr.f2.test(u = u, v = n_total - u - 1, f2 = if (mode == "sensitivity") NULL else effect_calc, sig.level = alpha, power = if (mode == "sensitivity") target else NULL)
    if (mode == "sensitivity") { row$effect_metric <- "f2"; row$effect_size <- raw$f2; row$r2 <- NA_real_ }
    attained <- pwr::pwr.f2.test(u = u, v = n_total - u - 1, f2 = if (mode == "sensitivity") raw$f2 else effect_calc, sig.level = alpha)
    row$u <- u; row$n_total <- n_total
  } else {
    engine <- "semPower::semPower.postHoc + stats noncentral-null chi-square"
    if (mode == "apriori") {
      solved <- power_n_root(function(n) power_sem_result(n, sem_df, alpha, rmsea0, rmsea1)$power, target)
      continuous <- list(n_total = solved); n_total <- ceiling(solved)
      raw <- power_sem_result(solved, sem_df, alpha, rmsea0, rmsea1)
    } else if (mode == "sensitivity") {
      upper <- max(.1, rmsea0 * 2)
      while (power_sem_result(n_total, sem_df, alpha, rmsea0, upper, FALSE)$power < target && upper < 1e4) upper <- upper * 2
      if (upper >= 1e4) stop("SEM sensitivity could not bracket the alternative RMSEA.")
      rmsea1 <- uniroot(function(effect) power_sem_result(n_total, sem_df, alpha, rmsea0, effect, FALSE)$power - target, c(rmsea0, upper), tol = 1e-10)$root
      row$effect_size <- rmsea1; row$rmsea1 <- rmsea1
    }
    attained <- power_sem_result(n_total, sem_df, alpha, rmsea0, rmsea1, if (mode == "sensitivity") FALSE else rmsea1 < rmsea0)
    if (is.null(raw)) raw <- attained
    row$n_total <- n_total; row$df <- sem_df
    notes <- c(notes, paste0("RMSEA test uses the ", attained$tail, " tail and the specified noncentral null; semPower's saved exact-fit reference is not the close-fit result."))
    if (mode == "sensitivity") notes <- c(notes, "SEM sensitivity solves RMSEA1 above RMSEA0 (upper-tail close-fit test).")
  }
  row$attained_power <- attained$power
  row$power <- if (mode == "posthoc") attained$power else target
  if (!is.finite(row$attained_power) || row$attained_power < 0 || row$attained_power > 1) stop("The requested calculation did not produce finite power in [0, 1].")
  if (mode == "apriori" && row$attained_power + 1e-7 < target) stop("Integer sample allocation did not attain the requested target power.")
  if (mode == "apriori") notes <- c(notes, "Power denotes the requested target; attained power is recomputed at the reported integer sample sizes. Continuous solutions are preserved in JSON.")
  if (mode == "posthoc") notes <- c(notes, "Post hoc power is descriptive and does not establish study adequacy or change a hypothesis-test conclusion.")
  if (nzchar(effect_basis)) notes <- c(notes, paste0("Effect basis (user supplied, not independently verified): ", effect_basis))
  notes <- c(notes, "These deterministic outputs support, but do not replace, a context-sensitive research report and design justification.")
  summary_df <- as.data.frame(row, stringsAsFactors = FALSE)
  calculation <- list(engine = engine, raw = unclass(raw), attained = unclass(attained), continuous_sample_size = continuous,
    requested_sample_sizes = n_inputs, requested_ratio = if (analysis == "ttest" && t_type == "two-sample") ratio else NULL,
    attained_power = row$attained_power, rounding = if (mode == "apriori") "ceil continuous sample size; two-sample n2 = ceil(ceil(n1) * ratio)" else "sample-size allocation documented in design notes")
  results <- list(summary_df = summary_df, calculation = calculation, effect_estimation = estimation, notes = notes)
  nlss_set_result(results)
  template <- resolve_template_override(opts[["template"]], module = "power")
  if (is.null(template)) template <- resolve_template_path("power.default", "power/default-template.md")
  template <- nlss_freeze_template(template, "power.main")
  table <- build_power_table_body(summary_df, digits, get_template_meta(template)$table)
  narrative <- build_power_narrative(row, digits)
  note <- paste(notes, collapse = " ")
  flags <- list(analysis = analysis, mode = mode, planning = planning, "effect-metric" = row$effect_metric, "effect-size" = row$effect_size,
    alpha = alpha, power = row$power, "attained-power" = row$attained_power, n = row$n_total,
    "t-type" = if (analysis == "ttest") t_type else NULL, alternative = options$alternative, groups = options$groups, u = options$u,
    "sem-df" = options$df, rmsea0 = options$rmsea0, rmsea1 = if (analysis == "sem") row$rmsea1 else NULL, "estimate-effect" = estimate,
    vars = if (estimate && length(vars)) vars else NULL, group = if (estimate && nzchar(group)) group else NULL,
    x = if (estimate && nzchar(text("x"))) text("x") else NULL, y = if (estimate && nzchar(text("y"))) text("y") else NULL,
    dv = if (estimate && nzchar(text("dv"))) text("dv") else NULL, ivs = if (estimate && length(ivs)) ivs else NULL,
    mu = options$mu, digits = digits)
  nlss_stage_report(file.path(out_dir, "report_canonical.md"), "Power analysis", paste0("Table 1\n\n", table, "\n", note), narrative,
    analysis_flags = flags, template_path = template, template_context = list(tokens = list(table_body = table, narrative_default = narrative, note_default = note), narrative_rows = list(list(full_sentence = narrative))))
  if (parse_bool(opts[["log"]], default = get_config_value("defaults.log"))) {
    ctx <- get_run_context()
    nlss_stage_log(out_dir, module = "power", prompt = ctx$prompt, commands = ctx$commands, results = results, options = options, user_prompt = get_user_prompt(opts))
  }
}

nlss_run_main("power", main)
