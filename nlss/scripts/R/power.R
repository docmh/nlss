#!/usr/bin/env Rscript

bootstrap_dir <- {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", cmd_args[grep("^--file=", cmd_args)])
  if (length(file_arg) > 0 && nzchar(file_arg[1])) {
    dirname(normalizePath(file_arg[1], winslash = "/", mustWork = FALSE))
  } else {
    getwd()
  }
}
source(file.path(bootstrap_dir, "lib", "paths.R"))
source_lib("cli.R")
source_lib("config.R")
source_lib("io.R")
source_lib("data_utils.R")
source_lib("formatting.R")


# Static analysis aliases for source_lib-defined functions.
render_output_path <- get("render_output_path", mode = "function")
source_lib <- get("source_lib", mode = "function")

print_usage <- function() {
  cat("Power analysis (pwr + semPower)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript power.R --csv data.csv --analysis ttest --mode apriori --t-type two-sample --effect-size 0.5 --power 0.8\n")
  cat("  Rscript power.R --parquet data.parquet --analysis anova --mode sensitivity --groups 3 --n-per-group 30 --power 0.8\n")
  cat("  Rscript power.R --parquet data.parquet --analysis correlation --mode posthoc --effect-size 0.3 --n 120\n")
  cat("  Rscript power.R --parquet data.parquet --analysis regression --mode apriori --effect-size 0.15 --effect-metric f2 --u 3\n")
  cat("  Rscript power.R --parquet data.parquet --analysis sem --mode apriori --df 120 --rmsea0 0.05 --rmsea1 0.08 --power 0.8\n")
  cat("  Rscript power.R --interactive\n")
  cat("\n")
  cat("Options:\n")
  cat("  --csv PATH             CSV input file\n")
  cat("  --sav PATH             SPSS .sav input file\n")
  cat("  --sep VALUE            CSV separator (default: ,)\n")
  cat("  --header TRUE/FALSE    CSV header (default: TRUE)\n")
  cat("  --rds PATH             RDS input file (data frame)\n")
  cat("  --rdata PATH           RData input file\n")
  cat("  --parquet PATH         Parquet input file\n")
  cat("  --df NAME              Data frame object name in RData\n")
  cat("  --analysis TYPE         ttest/anova/correlation/regression/sem (default from config)\n")
  cat("  --mode TYPE            apriori/posthoc/sensitivity (default from config)\n")
  cat("  --effect-size VALUE    Effect size (optional if --estimate-effect TRUE)\n")
  cat("  --effect-metric TYPE   d/f/f2/r/eta2/r2/rmsea (default from config)\n")
  cat("  --alpha VALUE          Alpha level (default from config)\n")
  cat("  --power VALUE          Target power (default from config)\n")
  cat("  --alternative TYPE     two.sided/greater/less (default from config)\n")
  cat("  --t-type TYPE          one-sample/two-sample/paired (ttest only)\n")
  cat("  --ratio VALUE          Group size ratio n2/n1 (ttest two-sample; default from config)\n")
  cat("  --mu VALUE             One-sample mean under H0 (default from config)\n")
  cat("  --n VALUE              Total sample size (posthoc/sensitivity)\n")
  cat("  --n-total VALUE        Alias for --n\n")
  cat("  --n-per-group VALUE    Per-group sample size (anova/ttest)\n")
  cat("  --n1 VALUE             Sample size for group 1 (ttest two-sample)\n")
  cat("  --n2 VALUE             Sample size for group 2 (ttest two-sample)\n")
  cat("  --groups VALUE         Number of groups (anova)\n")
  cat("  --u VALUE              Number of predictors (regression)\n")
  cat("  --rmsea0 VALUE          RMSEA under H0 (sem)\n")
  cat("  --rmsea1 VALUE          RMSEA under H1 (sem)\n")
  cat("  --estimate-effect TRUE/FALSE  Estimate effect from data (default from config)\n")
  cat("  --vars LIST            Variables for ttest effect estimation\n")
  cat("  --group NAME           Grouping variable (ttest/anova estimation)\n")
  cat("  --between NAME         Alias for --group (anova estimation)\n")
  cat("  --x NAME               X variable (paired/correlation estimation)\n")
  cat("  --y NAME               Y variable (paired/correlation estimation)\n")
  cat("  --dv NAME              Dependent variable (regression/anova estimation)\n")
  cat("  --ivs LIST             Predictors (regression estimation)\n")
  cat("  --digits N             Rounding digits (default: 2)\n")
  cat("  --template REF         Template path or template key (optional)\n")
  cat("  --user-prompt TEXT     Original AI user prompt for logging (optional)\n")
  cat("  --log TRUE/FALSE       Write analysis_log.jsonl (default: TRUE)\n")
  cat("  --interactive          Prompt for inputs\n")
  cat("  --help                 Show this help\n")
}

interactive_options <- function() {
  cat("Interactive input selected.\n")
  input_type <- resolve_prompt("Input type (csv/sav/rds/rdata/parquet)", "csv")
  input_type <- tolower(input_type)
  opts <- list()

  if (input_type == "csv") {
    opts$csv <- resolve_prompt("CSV path")
    sep_default <- resolve_config_value("defaults.csv.sep", ",")
    header_default <- resolve_config_value("defaults.csv.header", TRUE)
    opts$sep <- resolve_prompt("Separator", sep_default)
    opts$header <- resolve_prompt("Header TRUE/FALSE", ifelse(isTRUE(header_default), "TRUE", "FALSE"))
  } else if (input_type == "sav") {
    opts$sav <- resolve_prompt("SAV path")
  } else if (input_type == "rds") {
    opts$rds <- resolve_prompt("RDS path")
  } else if (input_type == "rdata") {
    opts$rdata <- resolve_prompt("RData path")
    opts$df <- resolve_prompt("Data frame object name")
  } else if (input_type == "parquet") {
    opts$parquet <- resolve_prompt("Parquet path")
  } else {
    stop("Unsupported input type.")
  }

  analysis_default <- resolve_config_value("modules.power.analysis", "ttest")
  mode_default <- resolve_config_value("modules.power.mode", "apriori")
  effect_metric_default <- resolve_config_value("modules.power.effect_metric", "auto")
  alpha_default <- resolve_config_value("modules.power.alpha", 0.05)
  power_default <- resolve_config_value("modules.power.power", 0.8)
  alt_default <- resolve_config_value("modules.power.alternative", "two.sided")
  t_type_default <- resolve_config_value("modules.power.t_type", "two-sample")
  ratio_default <- resolve_config_value("modules.power.ratio", 1)
  mu_default <- resolve_config_value("modules.power.mu", 0)
  groups_default <- resolve_config_value("modules.power.groups", 2)
  u_default <- resolve_config_value("modules.power.u", 1)
  rmsea0_default <- resolve_config_value("modules.power.rmsea0", 0.05)
  rmsea1_default <- resolve_config_value("modules.power.rmsea1", 0.08)
  estimate_default <- resolve_config_value("modules.power.estimate_effect", FALSE)
  digits_default <- resolve_config_value("defaults.digits", 2)

  opts$analysis <- resolve_prompt("Analysis (ttest/anova/correlation/regression/sem)", analysis_default)
  opts$mode <- resolve_prompt("Mode (apriori/posthoc/sensitivity)", mode_default)
  opts$`effect-metric` <- resolve_prompt("Effect metric (d/f/f2/r/eta2/r2/rmsea)", effect_metric_default)
  opts$`effect-size` <- resolve_prompt("Effect size (optional)", "")
  opts$alpha <- resolve_prompt("Alpha", as.character(alpha_default))
  opts$power <- resolve_prompt("Power", as.character(power_default))
  opts$alternative <- resolve_prompt("Alternative (two.sided/greater/less)", alt_default)
  opts$`t-type` <- resolve_prompt("t-test type (one-sample/two-sample/paired)", t_type_default)
  opts$ratio <- resolve_prompt("Group size ratio n2/n1", as.character(ratio_default))
  opts$mu <- resolve_prompt("One-sample mu", as.character(mu_default))
  opts$n <- resolve_prompt("Total sample size (optional)", "")
  opts$`n-per-group` <- resolve_prompt("Sample size per group (optional)", "")
  opts$n1 <- resolve_prompt("n1 (optional)", "")
  opts$n2 <- resolve_prompt("n2 (optional)", "")
  opts$groups <- resolve_prompt("Groups (k; optional)", as.character(groups_default))
  opts$u <- resolve_prompt("Predictors (u; optional)", as.character(u_default))
  opts$df <- resolve_prompt("SEM df (optional)", "")
  opts$rmsea0 <- resolve_prompt("RMSEA0 (optional)", as.character(rmsea0_default))
  opts$rmsea1 <- resolve_prompt("RMSEA1 (optional)", as.character(rmsea1_default))
  opts$`estimate-effect` <- resolve_prompt("Estimate effect TRUE/FALSE", ifelse(isTRUE(estimate_default), "TRUE", "FALSE"))
  opts$vars <- resolve_prompt("Variables for t-test estimation (comma-separated)", "")
  opts$group <- resolve_prompt("Grouping variable (optional)", "")
  opts$between <- resolve_prompt("Between-group variable (optional)", "")
  opts$x <- resolve_prompt("X variable (optional)", "")
  opts$y <- resolve_prompt("Y variable (optional)", "")
  opts$dv <- resolve_prompt("Dependent variable (optional)", "")
  opts$ivs <- resolve_prompt("Predictors (comma-separated; optional)", "")
  opts$digits <- resolve_prompt("Rounding digits", as.character(digits_default))
  opts$template <- resolve_prompt("Template (path or key; blank for default)", "")
  opts$`user-prompt` <- resolve_prompt("User prompt (optional)", "")
  log_default <- resolve_config_value("defaults.log", TRUE)
  opts$log <- resolve_prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))
  opts
}

resolve_prompt <- function(label, default = NULL) {
  if (exists("prompt", mode = "function")) {
    return(get("prompt", mode = "function")(label, default = default))
  }
  if (is.null(default)) {
    answer <- readline(paste0(label, ": "))
  } else {
    answer <- readline(paste0(label, " [", default, "]: "))
    if (answer == "") answer <- default
  }
  answer
}

resolve_config_value <- function(path, default = NULL) {
  if (exists("get_config_value", mode = "function")) {
    return(get("get_config_value", mode = "function")(path, default = default))
  }
  default
}

resolve_parse_args <- function(args) {
  if (exists("parse_args", mode = "function")) {
    return(get("parse_args", mode = "function")(args))
  }
  opts <- list()
  i <- 1
  while (i <= length(args)) {
    arg <- args[i]
    if (grepl("^--", arg)) {
      key <- sub("^--", "", arg)
      if (grepl("=", key)) {
        parts <- strsplit(key, "=", fixed = TRUE)[[1]]
        opts[[parts[1]]] <- parts[2]
      } else if (i < length(args) && !grepl("^--", args[i + 1])) {
        opts[[key]] <- args[i + 1]
        i <- i + 1
      } else {
        opts[[key]] <- TRUE
      }
    }
    i <- i + 1
  }
  opts
}

resolve_parse_bool <- function(value, default = FALSE) {
  if (exists("parse_bool", mode = "function")) {
    return(get("parse_bool", mode = "function")(value, default = default))
  }
  if (is.null(value)) return(default)
  if (is.logical(value)) return(value)
  val <- tolower(as.character(value))
  val %in% c("true", "t", "1", "yes", "y")
}

resolve_parse_list <- function(value, sep = ",") {
  if (exists("parse_list", mode = "function")) {
    return(get("parse_list", mode = "function")(value, sep = sep))
  }
  if (is.null(value) || is.logical(value)) return(character(0))
  value <- as.character(value)
  if (value == "") return(character(0))
  trimws(strsplit(value, sep, fixed = TRUE)[[1]])
}

is_scalar_number <- function(value) {
  is.numeric(value) && length(value) == 1 && !is.na(value) && is.finite(value)
}

resolve_load_dataframe <- function(opts) {
  if (exists("load_dataframe", mode = "function")) {
    return(get("load_dataframe", mode = "function")(opts))
  }
  stop("Missing load_dataframe. Ensure lib/io.R is sourced.")
}

resolve_get_workspace_out_dir <- function(df = NULL, label = NULL) {
  if (exists("get_workspace_out_dir", mode = "function")) {
    return(get("get_workspace_out_dir", mode = "function")(df, label = label))
  }
  "./outputs/tmp"
}

resolve_get_run_context <- function() {
  if (exists("get_run_context", mode = "function")) {
    return(get("get_run_context", mode = "function")())
  }
  list(prompt = "", commands = character(0))
}

resolve_append_analysis_log <- function(out_dir, module, prompt, commands, results, options = list(), user_prompt = NULL) {
  if (exists("append_analysis_log", mode = "function")) {
    return(get("append_analysis_log", mode = "function")(
      out_dir,
      module,
      prompt,
      commands,
      results,
      options = options,
      user_prompt = user_prompt
    ))
  }
  cat("Note: append_analysis_log not available; skipping analysis_log.jsonl output.\n")
  invisible(FALSE)
}

resolve_get_user_prompt <- function(opts) {
  if (exists("get_user_prompt", mode = "function")) {
    return(get("get_user_prompt", mode = "function")(opts))
  }
  NULL
}

resolve_get_template_path <- function(key, default_relative = NULL) {
  if (exists("resolve_template_path", mode = "function")) {
    return(get("resolve_template_path", mode = "function")(key, default_relative))
  }
  if (is.null(default_relative) || !nzchar(default_relative)) return(NULL)
  if (exists("get_assets_dir", mode = "function")) {
    return(file.path(get("get_assets_dir", mode = "function")(), default_relative))
  }
  file.path(getwd(), "nlss", "assets", default_relative)
}

resolve_get_template_meta <- function(path) {
  if (exists("get_template_meta", mode = "function")) {
    return(get("get_template_meta", mode = "function")(path))
  }
  list()
}

resolve_template_override <- local({
  override_impl <- NULL
  if (exists("resolve_template_override", mode = "function")) {
    override_impl <- get("resolve_template_override", mode = "function")
  }
  function(template_ref, module = NULL) {
    if (!is.null(override_impl)) {
      return(override_impl(template_ref, module = module))
    }
    NULL
  }
})

resolve_normalize_table_columns <- function(columns, default_specs) {
  if (exists("normalize_table_columns", mode = "function")) {
    return(get("normalize_table_columns", mode = "function")(columns, default_specs))
  }
  default_specs
}

resolve_drop_empty_columns <- function(columns, rows) {
  if (exists("drop_empty_columns", mode = "function")) {
    return(get("drop_empty_columns", mode = "function")(columns, rows))
  }
  list(columns = columns, rows = rows)
}

resolve_render_markdown_table <- function(headers, rows) {
  if (exists("render_markdown_table", mode = "function")) {
    return(get("render_markdown_table", mode = "function")(headers, rows))
  }
  ""
}

resolve_as_cell_text <- function(value) {
  if (exists("as_cell_text", mode = "function")) {
    return(get("as_cell_text", mode = "function")(value))
  }
  if (length(value) == 0 || is.null(value) || is.na(value)) return("")
  as.character(value)
}

resolve_append_nlss_report <- function(path, analysis_label, nlss_table, nlss_text, analysis_flags = NULL, template_path = NULL, template_context = NULL) {
  if (exists("append_nlss_report", mode = "function")) {
    return(get("append_nlss_report", mode = "function")(
      path,
      analysis_label,
      nlss_table,
      nlss_text,
      analysis_flags = analysis_flags,
      template_path = template_path,
      template_context = template_context
    ))
  }
  stop("Missing report formatter. Ensure lib/formatting.R is sourced.")
}

parse_numeric <- function(value, default = NA_real_) {
  if (is.null(value)) return(default)
  if (is.logical(value)) return(default)
  if (!nzchar(as.character(value))) return(default)
  num <- suppressWarnings(as.numeric(value))
  if (is.na(num)) return(default)
  num
}

format_num <- function(value, digits) {
  if (is.na(value)) return("")
  format(round(value, digits), nsmall = digits, trim = TRUE)
}

format_stat <- function(value, digits) {
  if (is.na(value)) return("")
  txt <- format(round(value, digits), nsmall = digits, trim = TRUE)
  sub("^(-?)0", "\\1", txt)
}

format_int <- function(value) {
  if (is.na(value)) return("")
  as.character(as.integer(round(value)))
}

normalize_analysis <- function(value, default = "ttest") {
  val <- if (!is.null(value) && nzchar(value)) value else default
  val <- tolower(val)
  if (val %in% c("t", "ttest", "t-test", "t_test")) return("ttest")
  if (val %in% c("anova", "aov")) return("anova")
  if (val %in% c("correlation", "cor", "corr")) return("correlation")
  if (val %in% c("regression", "regress", "lm")) return("regression")
  if (val %in% c("sem", "cfa")) return("sem")
  default
}

normalize_mode <- function(value, default = "apriori") {
  val <- if (!is.null(value) && nzchar(value)) value else default
  val <- tolower(val)
  if (val %in% c("apriori", "a-priori", "a_priori", "a priori")) return("apriori")
  if (val %in% c("posthoc", "post-hoc", "post_hoc", "achieved")) return("posthoc")
  if (val %in% c("sensitivity", "detectable")) return("sensitivity")
  default
}

normalize_alternative <- function(value, default = "two.sided") {
  val <- if (!is.null(value) && nzchar(value)) value else default
  val <- tolower(val)
  if (val %in% c("two.sided", "two-sided", "two")) return("two.sided")
  if (val %in% c("greater", "less")) return(val)
  default
}

normalize_t_type <- function(value, default = "two-sample") {
  val <- if (!is.null(value) && nzchar(value)) value else default
  val <- tolower(val)
  if (val %in% c("one-sample", "one_sample", "onesample")) return("one-sample")
  if (val %in% c("paired", "pair")) return("paired")
  if (val %in% c("two-sample", "two_sample", "independent", "between")) return("two-sample")
  default
}

normalize_effect_metric <- function(value, analysis, default = "auto") {
  val <- if (!is.null(value) && nzchar(value)) value else default
  val <- tolower(val)
  if (val %in% c("auto", "")) {
    if (analysis == "ttest") return("d")
    if (analysis == "anova") return("f")
    if (analysis == "correlation") return("r")
    if (analysis == "regression") return("f2")
    if (analysis == "sem") return("rmsea")
  }
  if (val %in% c("d", "cohen_d", "cohen-d")) return("d")
  if (val %in% c("f", "cohen_f")) return("f")
  if (val %in% c("f2", "f^2", "cohen_f2", "cohen-f2")) return("f2")
  if (val %in% c("r", "rho")) return("r")
  if (val %in% c("eta2", "eta^2", "eta", "etasq")) return("eta2")
  if (val %in% c("r2", "r^2")) return("r2")
  if (val %in% c("rmsea")) return("rmsea")
  val
}

emit_input_issue <- function(out_dir, opts, message, details = list(), status = "invalid_input") {
  log_default <- resolve_config_value("defaults.log", TRUE)
  if (resolve_parse_bool(opts$log, default = log_default)) {
    ctx <- resolve_get_run_context()
    resolve_append_analysis_log(
      out_dir,
      module = "power",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(
        status = status,
        message = message,
        details = details
      ),
      options = details,
      user_prompt = resolve_get_user_prompt(opts)
    )
  }
  stop(message)
}

estimate_ttest_effect <- function(df, t_type, var, group_var, x_var, y_var, mu) {
  if (t_type == "one-sample") {
    if (!nzchar(var) || !var %in% names(df)) stop("Variable not found for one-sample t-test.")
    vals <- df[[var]]
    vals <- vals[is.finite(vals)]
    if (length(vals) < 2) stop("Not enough data for one-sample effect size.")
    m <- mean(vals)
    s <- sd(vals)
    if (is.na(s) || s == 0) stop("Standard deviation is zero; cannot estimate effect size.")
    d <- (m - mu) / s
    list(d = d, n = length(vals), mean = m, sd = s)
  } else if (t_type == "paired") {
    if (!nzchar(x_var) || !nzchar(y_var)) stop("Paired t-test requires --x and --y.")
    if (!x_var %in% names(df) || !y_var %in% names(df)) stop("Paired variables not found.")
    x <- df[[x_var]]
    y <- df[[y_var]]
    idx <- is.finite(x) & is.finite(y)
    if (sum(idx) < 2) stop("Not enough paired data for effect size.")
    diffs <- x[idx] - y[idx]
    m <- mean(diffs)
    s <- sd(diffs)
    if (is.na(s) || s == 0) stop("Standard deviation is zero; cannot estimate effect size.")
    d <- m / s
    list(d = d, n = sum(idx), mean = m, sd = s)
  } else {
    if (!nzchar(var) || !var %in% names(df)) stop("Variable not found for independent t-test.")
    if (!nzchar(group_var) || !group_var %in% names(df)) stop("Grouping variable not found for independent t-test.")
    vals <- df[[var]]
    groups <- df[[group_var]]
    idx <- is.finite(vals) & !is.na(groups)
    vals <- vals[idx]
    groups <- as.factor(groups[idx])
    levels <- levels(groups)
    if (length(levels) != 2) stop("Grouping variable must have exactly two levels.")
    g1 <- levels[1]
    g2 <- levels[2]
    v1 <- vals[groups == g1]
    v2 <- vals[groups == g2]
    if (length(v1) < 2 || length(v2) < 2) stop("Not enough data per group for effect size.")
    m1 <- mean(v1)
    m2 <- mean(v2)
    sd1 <- sd(v1)
    sd2 <- sd(v2)
    n1 <- length(v1)
    n2 <- length(v2)
    pooled <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
    if (is.na(pooled) || pooled == 0) stop("Pooled SD is zero; cannot estimate effect size.")
    d <- (m1 - m2) / pooled
    list(d = d, n1 = n1, n2 = n2, mean1 = m1, mean2 = m2, sd1 = sd1, sd2 = sd2)
  }
}

estimate_correlation_effect <- function(df, x_var, y_var) {
  if (!nzchar(x_var) || !nzchar(y_var)) stop("Correlation estimation requires --x and --y.")
  if (!x_var %in% names(df) || !y_var %in% names(df)) stop("Correlation variables not found.")
  x <- df[[x_var]]
  y <- df[[y_var]]
  idx <- is.finite(x) & is.finite(y)
  if (sum(idx) < 3) stop("Not enough data for correlation effect size.")
  r <- suppressWarnings(cor(x[idx], y[idx]))
  list(r = r, n = sum(idx))
}

estimate_regression_effect <- function(df, dv, ivs) {
  if (!nzchar(dv) || !dv %in% names(df)) stop("Dependent variable not found for regression.")
  if (length(ivs) == 0) stop("Predictors required for regression effect estimation.")
  missing <- setdiff(ivs, names(df))
  if (length(missing) > 0) stop(paste("Predictors not found:", paste(missing, collapse = ", ")))
  cols <- c(dv, ivs)
  data <- df[, cols, drop = FALSE]
  data <- data[complete.cases(data), , drop = FALSE]
  if (nrow(data) < length(ivs) + 2) stop("Not enough data for regression effect size.")
  formula <- as.formula(paste(dv, "~", paste(ivs, collapse = " + ")))
  model <- lm(formula, data = data)
  r2 <- summary(model)$r.squared
  if (is.na(r2) || r2 < 0) stop("Could not compute R2.")
  f2 <- if (r2 >= 1) NA_real_ else r2 / (1 - r2)
  list(r2 = r2, f2 = f2, n = nrow(data))
}

estimate_anova_effect <- function(df, dv, group_var) {
  if (!nzchar(dv) || !dv %in% names(df)) stop("Dependent variable not found for ANOVA.")
  if (!nzchar(group_var) || !group_var %in% names(df)) stop("Grouping variable not found for ANOVA.")
  data <- df[, c(dv, group_var), drop = FALSE]
  data <- data[complete.cases(data), , drop = FALSE]
  if (nrow(data) < 3) stop("Not enough data for ANOVA effect size.")
  data[[group_var]] <- as.factor(data[[group_var]])
  if (length(levels(data[[group_var]])) < 2) stop("Grouping variable needs at least two levels.")
  model <- aov(stats::as.formula(paste(dv, "~", group_var)), data = data)
  table <- summary(model)[[1]]
  if (nrow(table) < 2) stop("ANOVA table incomplete.")
  ss_between <- table[1, "Sum Sq"]
  ss_within <- table[2, "Sum Sq"]
  eta2 <- ss_between / (ss_between + ss_within)
  if (is.na(eta2) || eta2 < 0 || eta2 >= 1) stop("Could not compute eta2.")
  f <- sqrt(eta2 / (1 - eta2))
  list(eta2 = eta2, f = f, n = nrow(data), groups = length(levels(data[[group_var]])))
}

coerce_effect_size <- function(metric, value) {
  if (metric == "eta2") {
    if (value <= 0 || value >= 1) return(list(metric = "f", value = NA_real_, note = "eta2 must be between 0 and 1."))
    f <- sqrt(value / (1 - value))
    return(list(metric = "f", value = f, note = "Effect size converted from eta2 to f."))
  }
  if (metric == "r2") {
    if (value < 0 || value >= 1) return(list(metric = "f2", value = NA_real_, note = "r2 must be between 0 and 1."))
    f2 <- if (value >= 1) NA_real_ else value / (1 - value)
    return(list(metric = "f2", value = f2, note = "Effect size converted from r2 to f2."))
  }
  list(metric = metric, value = value, note = "")
}

solve_t2n_apriori <- function(target_power, d, alpha, alternative, ratio) {
  if (ratio <= 0) stop("Ratio must be positive.")
  power_at <- function(n1) {
    n2 <- ratio * n1
    res <- pwr::pwr.t2n.test(n1 = n1, n2 = n2, d = d, sig.level = alpha, alternative = alternative)
    res$power
  }
  n1 <- 2
  p1 <- power_at(n1)
  if (!is.finite(p1)) stop("Unable to compute power for initial sample size.")
  if (p1 >= target_power) return(n1)
  while (p1 < target_power && n1 < 1e7) {
    n1 <- n1 * 2
    p1 <- power_at(n1)
  }
  if (p1 < target_power) stop("Required sample size too large to bracket.")
  uniroot(function(x) power_at(x) - target_power, interval = c(n1 / 2, n1))$root
}

build_power_table_body <- function(summary_df, digits, table_spec = NULL) {
  default_columns <- list(
    list(key = "analysis", label = "Analysis"),
    list(key = "mode", label = "Mode"),
    list(key = "effect_metric", label = "Effect"),
    list(key = "effect_size", label = "Effect size"),
    list(key = "alpha", label = "alpha"),
    list(key = "power", label = "Power"),
    list(key = "n_total", label = "N", drop_if_empty = TRUE),
    list(key = "n_per_group", label = "n/group", drop_if_empty = TRUE),
    list(key = "n1", label = "n1", drop_if_empty = TRUE),
    list(key = "n2", label = "n2", drop_if_empty = TRUE),
    list(key = "groups", label = "k", drop_if_empty = TRUE),
    list(key = "ratio", label = "Ratio", drop_if_empty = TRUE),
    list(key = "u", label = "u", drop_if_empty = TRUE),
    list(key = "df", label = "df", drop_if_empty = TRUE),
    list(key = "r2", label = "R2", drop_if_empty = TRUE),
    list(key = "rmsea0", label = "RMSEA0", drop_if_empty = TRUE),
    list(key = "rmsea1", label = "RMSEA1", drop_if_empty = TRUE),
    list(key = "t_type", label = "t type", drop_if_empty = TRUE),
    list(key = "alternative", label = "Alternative", drop_if_empty = TRUE),
    list(key = "effect_source", label = "Effect source", drop_if_empty = TRUE)
  )
  columns <- resolve_normalize_table_columns(
    if (!is.null(table_spec$columns)) table_spec$columns else NULL,
    default_columns
  )

  rows <- list()
  for (i in seq_len(nrow(summary_df))) {
    row <- summary_df[i, , drop = FALSE]
    row_vals <- character(0)
    for (col in columns) {
      key <- col$key
      val <- ""
      if (key == "analysis") {
        label <- tolower(as.character(row[[key]]))
        if (label == "ttest") label <- "t-test"
        if (label == "anova") label <- "ANOVA"
        if (label == "correlation") label <- "Correlation"
        if (label == "regression") label <- "Regression"
        if (label == "sem") label <- "SEM"
        val <- resolve_as_cell_text(label)
      } else if (key == "mode") {
        label <- tolower(as.character(row[[key]]))
        if (label == "apriori") label <- "a priori"
        if (label == "posthoc") label <- "post hoc"
        if (label == "sensitivity") label <- "sensitivity"
        val <- resolve_as_cell_text(label)
      } else if (key %in% c("effect_metric", "t_type", "alternative", "effect_source")) {
        val <- resolve_as_cell_text(row[[key]])
      } else if (key %in% c("n_total", "n_per_group", "n1", "n2", "groups", "u", "df")) {
        val <- ifelse(is.na(row[[key]]), "", format_int(row[[key]]))
      } else if (key %in% c("alpha", "power", "effect_size", "ratio", "r2", "rmsea0", "rmsea1")) {
        val <- ifelse(is.na(row[[key]]), "", format_num(row[[key]], digits))
      } else if (key %in% names(row)) {
        cell <- row[[key]][1]
        if (is.numeric(cell)) {
          val <- format_num(cell, digits)
        } else {
          val <- resolve_as_cell_text(cell)
        }
      }
      row_vals <- c(row_vals, val)
    }
    rows[[length(rows) + 1]] <- row_vals
  }

  filtered <- resolve_drop_empty_columns(columns, rows)
  columns <- filtered$columns
  rows <- filtered$rows
  headers <- vapply(columns, function(col) col$label, character(1))
  body <- resolve_render_markdown_table(headers, rows)
  list(body = body, columns = columns)
}

build_power_note_tokens <- function(mode, effect_metric, conversion_note, effect_source, n_source_note) {
  notes <- character(0)
  if (nzchar(conversion_note)) notes <- c(notes, conversion_note)
  if (effect_source == "estimated") notes <- c(notes, "Effect size estimated from data.")
  if (nzchar(n_source_note)) notes <- c(notes, n_source_note)
  if (mode == "posthoc") notes <- c(notes, "Post hoc power is descriptive and should not be used for hypothesis decisions.")
  note_default <- paste(notes, collapse = " ")
  if (!nzchar(note_default)) note_default <- "None."
  list(note_default = note_default)
}

build_power_narrative <- function(row, digits) {
  analysis <- as.character(row$analysis)
  mode <- as.character(row$mode)
  effect_metric <- as.character(row$effect_metric)
  effect_size <- row$effect_size
  alpha <- row$alpha
  power <- row$power
  n_total <- row$n_total
  n1 <- row$n1
  n2 <- row$n2
  n_per_group <- row$n_per_group
  groups <- row$groups
  u <- row$u
  df <- row$df
  rmsea0 <- row$rmsea0
  rmsea1 <- row$rmsea1
  t_type <- row$t_type

  analysis_label <- analysis
  if (analysis == "ttest") {
    if (t_type == "paired") analysis_label <- "paired t-test"
    if (t_type == "one-sample") analysis_label <- "one-sample t-test"
    if (t_type == "two-sample") analysis_label <- "two-sample t-test"
  } else if (analysis == "anova") {
    analysis_label <- "one-way ANOVA"
  } else if (analysis == "correlation") {
    analysis_label <- "correlation"
  } else if (analysis == "regression") {
    analysis_label <- "multiple regression"
  } else if (analysis == "sem") {
    analysis_label <- "SEM (RMSEA)"
  }

  effect_text <- paste0(effect_metric, " = ", format_stat(effect_size, digits))
  alpha_text <- format_stat(alpha, digits)
  power_text <- format_stat(power, digits)

  sample_text <- ""
  if (!is.na(n1) && !is.na(n2)) {
    sample_text <- paste0("N = ", format_int(n1 + n2), " (n1 = ", format_int(n1), ", n2 = ", format_int(n2), ")")
  } else if (!is.na(n_per_group) && !is.na(groups)) {
    sample_text <- paste0("n/group = ", format_int(n_per_group), ", k = ", format_int(groups))
  } else if (!is.na(n_total)) {
    sample_text <- paste0("N = ", format_int(n_total))
  }

  extra_parts <- character(0)
  if (!is.na(u)) extra_parts <- c(extra_parts, paste0("u = ", format_int(u)))
  if (!is.na(df)) extra_parts <- c(extra_parts, paste0("df = ", format_int(df)))
  if (!is.na(rmsea0) && !is.na(rmsea1)) {
    extra_parts <- c(extra_parts, paste0("RMSEA0 = ", format_stat(rmsea0, digits), ", RMSEA1 = ", format_stat(rmsea1, digits)))
  }
  extra_text <- ""
  if (length(extra_parts) > 0) extra_text <- paste0(" (", paste(extra_parts, collapse = "; "), ")")

  if (mode == "apriori") {
    return(paste0(
      "A priori power analysis for ", analysis_label, " (", effect_text,
      ", alpha = ", alpha_text, ", power = ", power_text,
      ") indicated a required ", sample_text, extra_text, "."
    ))
  }
  if (mode == "posthoc") {
    return(paste0(
      "Post hoc power analysis for ", analysis_label, " with ", sample_text,
      " (", effect_text, ", alpha = ", alpha_text, ") yielded achieved power = ", power_text, extra_text, "."
    ))
  }
  paste0(
    "Sensitivity analysis for ", analysis_label, " with ", sample_text,
    " (alpha = ", alpha_text, ", power = ", power_text,
    ") indicated a minimum detectable effect of ", effect_text, extra_text, "."
  )
}

resolve_n_inputs <- function(opts) {
  n_total <- parse_numeric(opts$n, NA_real_)
  n_total_alt <- parse_numeric(opts$`n-total`, NA_real_)
  if (is.na(n_total)) n_total <- n_total_alt
  n_per_group <- parse_numeric(opts$`n-per-group`, NA_real_)
  n1 <- parse_numeric(opts$n1, NA_real_)
  n2 <- parse_numeric(opts$n2, NA_real_)
  list(n_total = n_total, n_per_group = n_per_group, n1 = n1, n2 = n2)
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) == 0) {
    print_usage()
    quit(status = 1)
  }
  if ("--help" %in% args) {
    print_usage()
    quit(status = 0)
  }

  opts <- resolve_parse_args(args)
  if (isTRUE(opts$interactive)) {
    opts <- interactive_options()
  }

  analysis_default <- resolve_config_value("modules.power.analysis", "ttest")
  mode_default <- resolve_config_value("modules.power.mode", "apriori")
  effect_metric_default <- resolve_config_value("modules.power.effect_metric", "auto")
  alpha_default <- resolve_config_value("modules.power.alpha", 0.05)
  power_default <- resolve_config_value("modules.power.power", 0.8)
  t_type_default <- resolve_config_value("modules.power.t_type", "two-sample")
  ratio_default <- resolve_config_value("modules.power.ratio", 1)
  mu_default <- resolve_config_value("modules.power.mu", 0)
  groups_default <- resolve_config_value("modules.power.groups", 2)
  u_default <- resolve_config_value("modules.power.u", 1)
  rmsea0_default <- resolve_config_value("modules.power.rmsea0", 0.05)
  rmsea1_default <- resolve_config_value("modules.power.rmsea1", 0.08)
  estimate_default <- resolve_config_value("modules.power.estimate_effect", FALSE)
  alt_default <- resolve_config_value("modules.power.alternative", "two.sided")
  digits_default <- resolve_config_value("defaults.digits", 2)
  log_default <- resolve_config_value("defaults.log", TRUE)

  analysis <- normalize_analysis(opts$analysis, analysis_default)
  mode <- normalize_mode(opts$mode, mode_default)
  effect_metric_input <- normalize_effect_metric(opts$`effect-metric`, analysis, effect_metric_default)
  alpha <- parse_numeric(opts$alpha, alpha_default)
  power_target <- parse_numeric(opts$power, power_default)
  t_type <- normalize_t_type(opts$`t-type`, t_type_default)
  ratio <- parse_numeric(opts$ratio, ratio_default)
  mu <- parse_numeric(opts$mu, mu_default)
  groups <- parse_numeric(opts$groups, groups_default)
  u <- parse_numeric(opts$u, u_default)
  df_sem <- parse_numeric(opts$df, NA_real_)
  rmsea0 <- parse_numeric(opts$rmsea0, rmsea0_default)
  rmsea1 <- parse_numeric(opts$rmsea1, rmsea1_default)
  alternative <- normalize_alternative(opts$alternative, alt_default)
  digits <- parse_numeric(opts$digits, digits_default)
  estimate_effect <- resolve_parse_bool(opts$`estimate-effect`, default = estimate_default)

  df <- resolve_load_dataframe(opts)
  out_dir <- resolve_get_workspace_out_dir(df)

  if (analysis != "sem") {
    if (!requireNamespace("pwr", quietly = TRUE)) {
      emit_input_issue(out_dir, opts, "Power analysis requires the 'pwr' package.", details = list(package = "pwr"), status = "missing_dependency")
    }
  } else {
    if (!requireNamespace("semPower", quietly = TRUE)) {
      emit_input_issue(out_dir, opts, "SEM power analysis requires the 'semPower' package.", details = list(package = "semPower"), status = "missing_dependency")
    }
  }

  if (analysis == "sem" && estimate_effect) {
    emit_input_issue(out_dir, opts, "SEM power analysis does not support --estimate-effect.")
  }

  allowed_metrics <- list(
    ttest = c("d"),
    anova = c("f", "eta2"),
    correlation = c("r"),
    regression = c("f2", "r2"),
    sem = c("rmsea")
  )
  allowed_metric <- allowed_metrics[[analysis]]
  if (is.null(allowed_metric) || !effect_metric_input %in% allowed_metric) {
    emit_input_issue(out_dir, opts, paste0("Unsupported effect metric for ", analysis, ": ", effect_metric_input))
  }

  effect_size_input <- parse_numeric(opts$`effect-size`, NA_real_)
  effect_size_calc <- effect_size_input
  conversion_note <- ""
  effect_source <- if (estimate_effect) "estimated" else "user"
  n_source_note <- ""

  if (analysis == "sem") {
    effect_metric_input <- "rmsea"
    if (is.na(effect_size_input)) effect_size_input <- rmsea1
    effect_size_calc <- effect_size_input
  }
  if (analysis != "anova") groups <- NA_real_
  if (analysis != "regression") u <- NA_real_
  if (analysis != "ttest") ratio <- NA_real_

  n_inputs <- resolve_n_inputs(opts)
  n_total <- n_inputs$n_total
  n_per_group <- n_inputs$n_per_group
  n1 <- n_inputs$n1
  n2 <- n_inputs$n2
  r2_value <- NA_real_

  between_var <- ""
  if (!is.null(opts$between) && nzchar(opts$between)) {
    between_var <- as.character(opts$between)
  }
  group_var <- ""
  if (!is.null(opts$group) && nzchar(opts$group)) {
    group_var <- as.character(opts$group)
  }
  if (!nzchar(group_var) && nzchar(between_var)) group_var <- between_var

  if (estimate_effect) {
    if (analysis == "ttest") {
      var_list <- resolve_parse_list(opts$vars)
      var_name <- if (length(var_list) > 0) var_list[1] else ""
      x_var <- if (!is.null(opts$x)) as.character(opts$x) else ""
      y_var <- if (!is.null(opts$y)) as.character(opts$y) else ""
      est <- tryCatch(estimate_ttest_effect(df, t_type, var_name, group_var, x_var, y_var, mu), error = function(e) e)
      if (inherits(est, "error")) {
        emit_input_issue(out_dir, opts, est$message)
      }
      effect_metric_input <- "d"
      effect_size_input <- est$d
      effect_size_calc <- abs(est$d)
      if (is_scalar_number(est$n)) {
        n_total <- if (is.na(n_total)) est$n else n_total
        n_source_note <- if (is.na(n_inputs$n_total)) "Sample size derived from data." else ""
      }
      if (is_scalar_number(est$n1) && is_scalar_number(est$n2)) {
        n1 <- if (is.na(n1)) est$n1 else n1
        n2 <- if (is.na(n2)) est$n2 else n2
        if (is.na(n_inputs$n1) && is.na(n_inputs$n2)) n_source_note <- "Sample sizes derived from data."
      }
    } else if (analysis == "correlation") {
      x_var <- if (!is.null(opts$x)) as.character(opts$x) else ""
      y_var <- if (!is.null(opts$y)) as.character(opts$y) else ""
      est <- tryCatch(estimate_correlation_effect(df, x_var, y_var), error = function(e) e)
      if (inherits(est, "error")) {
        emit_input_issue(out_dir, opts, est$message)
      }
      effect_metric_input <- "r"
      effect_size_input <- est$r
      effect_size_calc <- abs(est$r)
      if (is.na(n_total) && is_scalar_number(est$n)) {
        n_total <- est$n
        n_source_note <- "Sample size derived from data."
      }
    } else if (analysis == "regression") {
      dv <- if (!is.null(opts$dv)) as.character(opts$dv) else ""
      ivs <- resolve_parse_list(opts$ivs)
      est <- tryCatch(estimate_regression_effect(df, dv, ivs), error = function(e) e)
      if (inherits(est, "error")) {
        emit_input_issue(out_dir, opts, est$message)
      }
      effect_metric_input <- "r2"
      effect_size_input <- est$r2
      r2_value <- est$r2
      converted <- coerce_effect_size("r2", est$r2)
      effect_size_calc <- converted$value
      conversion_note <- converted$note
      if (is.na(n_total) && is_scalar_number(est$n)) {
        n_total <- est$n
        n_source_note <- "Sample size derived from data."
      }
    } else if (analysis == "anova") {
      dv <- if (!is.null(opts$dv)) as.character(opts$dv) else ""
      if (!nzchar(group_var)) group_var <- if (!is.null(opts$between)) as.character(opts$between) else ""
      est <- tryCatch(estimate_anova_effect(df, dv, group_var), error = function(e) e)
      if (inherits(est, "error")) {
        emit_input_issue(out_dir, opts, est$message)
      }
      effect_metric_input <- "eta2"
      effect_size_input <- est$eta2
      converted <- coerce_effect_size("eta2", est$eta2)
      effect_size_calc <- converted$value
      conversion_note <- converted$note
      if (is.na(n_total) && is_scalar_number(est$n)) {
        n_total <- est$n
        n_source_note <- "Sample size derived from data (approximate)."
      }
      if (is.na(groups) && is_scalar_number(est$groups)) groups <- est$groups
    }
  } else {
    if (is.na(effect_size_input) && analysis != "sem" && mode != "sensitivity") {
      emit_input_issue(out_dir, opts, "Effect size is required unless --estimate-effect is TRUE.")
    }
    if (analysis %in% c("anova", "regression")) {
      converted <- coerce_effect_size(effect_metric_input, effect_size_input)
      effect_size_calc <- converted$value
      if (nzchar(converted$note)) conversion_note <- converted$note
      if (analysis == "regression" && effect_metric_input == "r2") r2_value <- effect_size_input
    }
  }

  if (analysis %in% c("ttest", "correlation") && !is.na(effect_size_input)) {
    effect_size_input <- abs(effect_size_input)
    effect_size_calc <- abs(effect_size_calc)
  }

  if (analysis == "sem") {
    if (is.na(df_sem)) {
      emit_input_issue(out_dir, opts, "SEM power analysis requires --df.")
    }
    if (is.na(rmsea0) || is.na(rmsea1)) {
      emit_input_issue(out_dir, opts, "SEM power analysis requires --rmsea0 and --rmsea1.")
    }
  }

  if (analysis != "sem" && mode != "sensitivity" && (is.na(effect_size_calc) || effect_size_calc <= 0)) {
    emit_input_issue(out_dir, opts, "Effect size must be positive.")
  }

  if (is.na(alpha) || alpha <= 0 || alpha >= 1) alpha <- alpha_default
  if (is.na(power_target) || power_target <= 0 || power_target >= 1) power_target <- power_default

  if (analysis == "ttest") {
    if (t_type == "two-sample") {
      if (!is.na(n_per_group)) {
        if (is.na(n1)) n1 <- n_per_group
        if (is.na(n2)) n2 <- n_per_group * ratio
      } else if (!is.na(n_total) && (is.na(n1) || is.na(n2))) {
        if (ratio <= 0) ratio <- 1
        n1 <- ceiling(n_total / (1 + ratio))
        n2 <- n_total - n1
      }
    } else {
      if (!is.na(n_per_group) && is.na(n_total)) n_total <- n_per_group
    }
  }

  if (analysis == "anova") {
    if (!is.na(n_total) && is.na(n_per_group)) {
      if (is.na(groups) || groups < 2) groups <- groups_default
      n_per_group <- floor(n_total / groups)
      if (n_per_group > 0) n_total <- n_per_group * groups
    }
  }

  summary_row <- list(
    analysis = analysis,
    mode = mode,
    effect_metric = effect_metric_input,
    effect_size = effect_size_input,
    alpha = alpha,
    power = NA_real_,
    n_total = NA_real_,
    n_per_group = NA_real_,
    n1 = NA_real_,
    n2 = NA_real_,
    groups = NA_real_,
    ratio = if (analysis == "ttest" && t_type == "two-sample") ratio else NA_real_,
    u = NA_real_,
    df = NA_real_,
    r2 = r2_value,
    rmsea0 = if (analysis == "sem") rmsea0 else NA_real_,
    rmsea1 = if (analysis == "sem") rmsea1 else NA_real_,
    t_type = if (analysis == "ttest") t_type else "",
    alternative = if (analysis == "ttest" || analysis == "correlation") alternative else "",
    effect_source = effect_source
  )

  if (analysis == "ttest") {
    if (mode == "apriori") {
      if (t_type == "two-sample") {
        if (abs(ratio - 1) < 1e-6) {
          res <- pwr::pwr.t.test(d = effect_size_calc, sig.level = alpha, power = power_target, type = "two.sample", alternative = alternative)
          n1 <- ceiling(res$n)
          n2 <- n1
        } else {
          n1 <- ceiling(solve_t2n_apriori(power_target, effect_size_calc, alpha, alternative, ratio))
          n2 <- ceiling(n1 * ratio)
        }
        summary_row$n1 <- n1
        summary_row$n2 <- n2
        summary_row$n_total <- n1 + n2
        summary_row$power <- power_target
      } else {
        res <- pwr::pwr.t.test(d = effect_size_calc, sig.level = alpha, power = power_target, type = ifelse(t_type == "paired", "paired", "one.sample"), alternative = alternative)
        summary_row$n_total <- ceiling(res$n)
        summary_row$power <- power_target
      }
    } else if (mode == "posthoc") {
      if (t_type == "two-sample") {
        if (is.na(n1) || is.na(n2)) {
          emit_input_issue(out_dir, opts, "Post hoc two-sample t-test requires sample sizes (n1/n2 or n-total).")
        }
        res <- pwr::pwr.t2n.test(n1 = n1, n2 = n2, d = effect_size_calc, sig.level = alpha, alternative = alternative)
        summary_row$n1 <- n1
        summary_row$n2 <- n2
        summary_row$n_total <- n1 + n2
        summary_row$power <- res$power
      } else {
        if (is.na(n_total)) emit_input_issue(out_dir, opts, "Post hoc t-test requires --n or --n-per-group.")
        res <- pwr::pwr.t.test(n = n_total, d = effect_size_calc, sig.level = alpha, type = ifelse(t_type == "paired", "paired", "one.sample"), alternative = alternative)
        summary_row$n_total <- n_total
        summary_row$power <- res$power
      }
    } else {
      if (t_type == "two-sample") {
        if (is.na(n1) || is.na(n2)) {
          emit_input_issue(out_dir, opts, "Sensitivity two-sample t-test requires sample sizes (n1/n2 or n-total).")
        }
        res <- pwr::pwr.t2n.test(n1 = n1, n2 = n2, sig.level = alpha, power = power_target, alternative = alternative)
        summary_row$n1 <- n1
        summary_row$n2 <- n2
        summary_row$n_total <- n1 + n2
        summary_row$power <- power_target
        summary_row$effect_metric <- "d"
        summary_row$effect_size <- res$d
      } else {
        if (is.na(n_total)) emit_input_issue(out_dir, opts, "Sensitivity t-test requires --n or --n-per-group.")
        res <- pwr::pwr.t.test(n = n_total, sig.level = alpha, power = power_target, type = ifelse(t_type == "paired", "paired", "one.sample"), alternative = alternative)
        summary_row$n_total <- n_total
        summary_row$power <- power_target
        summary_row$effect_metric <- "d"
        summary_row$effect_size <- res$d
      }
    }
  } else if (analysis == "anova") {
    if (is.na(groups) || groups < 2) emit_input_issue(out_dir, opts, "ANOVA requires --groups >= 2.")
    summary_row$groups <- groups
    if (mode == "apriori") {
      res <- pwr::pwr.anova.test(k = groups, f = effect_size_calc, sig.level = alpha, power = power_target)
      summary_row$n_per_group <- ceiling(res$n)
      summary_row$n_total <- summary_row$n_per_group * groups
      summary_row$power <- power_target
    } else if (mode == "posthoc") {
      if (is.na(n_per_group)) emit_input_issue(out_dir, opts, "Post hoc ANOVA requires --n-per-group or --n.")
      res <- pwr::pwr.anova.test(k = groups, n = n_per_group, f = effect_size_calc, sig.level = alpha)
      summary_row$n_per_group <- n_per_group
      summary_row$n_total <- n_per_group * groups
      summary_row$power <- res$power
    } else {
      if (is.na(n_per_group)) emit_input_issue(out_dir, opts, "Sensitivity ANOVA requires --n-per-group or --n.")
      res <- pwr::pwr.anova.test(k = groups, n = n_per_group, sig.level = alpha, power = power_target)
      summary_row$n_per_group <- n_per_group
      summary_row$n_total <- n_per_group * groups
      summary_row$power <- power_target
      summary_row$effect_metric <- "f"
      summary_row$effect_size <- res$f
    }
  } else if (analysis == "correlation") {
    if (mode == "apriori") {
      res <- pwr::pwr.r.test(r = effect_size_calc, sig.level = alpha, power = power_target, alternative = alternative)
      summary_row$n_total <- ceiling(res$n)
      summary_row$power <- power_target
    } else if (mode == "posthoc") {
      if (is.na(n_total)) emit_input_issue(out_dir, opts, "Post hoc correlation requires --n.")
      res <- pwr::pwr.r.test(n = n_total, r = effect_size_calc, sig.level = alpha, alternative = alternative)
      summary_row$n_total <- n_total
      summary_row$power <- res$power
    } else {
      if (is.na(n_total)) emit_input_issue(out_dir, opts, "Sensitivity correlation requires --n.")
      res <- pwr::pwr.r.test(n = n_total, sig.level = alpha, power = power_target, alternative = alternative)
      summary_row$n_total <- n_total
      summary_row$power <- power_target
      summary_row$effect_metric <- "r"
      summary_row$effect_size <- res$r
    }
  } else if (analysis == "regression") {
    if (is.na(u) || u < 1) emit_input_issue(out_dir, opts, "Regression requires --u (predictor count).")
    summary_row$u <- u
    if (mode == "apriori") {
      res <- pwr::pwr.f2.test(u = u, f2 = effect_size_calc, sig.level = alpha, power = power_target)
      summary_row$n_total <- ceiling(res$v + u + 1)
      summary_row$power <- power_target
    } else if (mode == "posthoc") {
      if (is.na(n_total)) emit_input_issue(out_dir, opts, "Post hoc regression requires --n.")
      v <- n_total - u - 1
      if (v <= 0) emit_input_issue(out_dir, opts, "Sample size too small for regression.")
      res <- pwr::pwr.f2.test(u = u, v = v, f2 = effect_size_calc, sig.level = alpha)
      summary_row$n_total <- n_total
      summary_row$power <- res$power
    } else {
      if (is.na(n_total)) emit_input_issue(out_dir, opts, "Sensitivity regression requires --n.")
      v <- n_total - u - 1
      if (v <= 0) emit_input_issue(out_dir, opts, "Sample size too small for regression.")
      res <- pwr::pwr.f2.test(u = u, v = v, sig.level = alpha, power = power_target)
      summary_row$n_total <- n_total
      summary_row$power <- power_target
      summary_row$effect_metric <- "f2"
      summary_row$effect_size <- res$f2
    }
  } else if (analysis == "sem") {
    summary_row$df <- df_sem
    sem_args_base <- list(
      effect.measure = "RMSEA",
      df = df_sem,
      alpha = alpha
    )

    call_sem <- function(fn_name, args) {
      fn <- get(fn_name, envir = asNamespace("semPower"))
      formals <- names(formals(fn))
      args_filtered <- args[names(args) %in% formals]
      do.call(fn, args_filtered)
    }

    extract_value <- function(obj, keys) {
      if (is.null(obj)) return(NA_real_)
      if (is.list(obj)) {
        for (key in keys) {
          if (!is.null(obj[[key]])) {
            val <- obj[[key]]
            if (is.numeric(val)) return(as.numeric(val[1]))
          }
        }
      }
      NA_real_
    }

    if (mode == "apriori") {
      res <- tryCatch(call_sem("semPower.aPriori", c(sem_args_base, list(effect = rmsea1, power = power_target))), error = function(e) e)
      if (inherits(res, "error")) emit_input_issue(out_dir, opts, res$message)
      n_total <- extract_value(res, c("N", "n", "sample.size", "sample_size", "requiredN"))
      if (is.na(n_total)) emit_input_issue(out_dir, opts, "SEM power result missing sample size.")
      summary_row$n_total <- ceiling(n_total)
      summary_row$power <- power_target
    } else if (mode == "posthoc") {
      if (is.na(n_total)) emit_input_issue(out_dir, opts, "Post hoc SEM requires --n.")
      res <- tryCatch(call_sem("semPower.postHoc", c(sem_args_base, list(effect = rmsea1, N = n_total, n = n_total))), error = function(e) e)
      if (inherits(res, "error")) emit_input_issue(out_dir, opts, res$message)
      power_val <- extract_value(res, c("power", "Power"))
      if (is.na(power_val)) emit_input_issue(out_dir, opts, "SEM power result missing power.")
      summary_row$n_total <- n_total
      summary_row$power <- power_val
    } else {
      if (is.na(n_total)) emit_input_issue(out_dir, opts, "Sensitivity SEM requires --n.")
      power_at <- function(rmsea_candidate) {
        res <- call_sem("semPower.postHoc", c(sem_args_base, list(effect = rmsea_candidate, N = n_total, n = n_total)))
        extract_value(res, c("power", "Power"))
      }
      lower <- max(rmsea0 + 1e-4, 1e-4)
      upper <- 0.3
      p_low <- power_at(lower)
      p_high <- power_at(upper)
      if (!is.finite(p_low) || !is.finite(p_high)) emit_input_issue(out_dir, opts, "SEM sensitivity could not bracket RMSEA1.")
      if ((p_low - power_target) * (p_high - power_target) > 0) {
        emit_input_issue(out_dir, opts, "SEM sensitivity could not bracket RMSEA1.")
      }
      rmsea_solution <- uniroot(function(x) power_at(x) - power_target, interval = c(lower, upper))$root
      summary_row$n_total <- n_total
      summary_row$power <- power_target
      summary_row$effect_metric <- "rmsea"
      summary_row$effect_size <- rmsea_solution
      summary_row$rmsea1 <- rmsea_solution
    }
  }

  summary_df <- as.data.frame(summary_row, stringsAsFactors = FALSE)

  narrative <- build_power_narrative(summary_row, digits)
  note_tokens <- build_power_note_tokens(mode, effect_metric_input, conversion_note, effect_source, n_source_note)

  analysis_flags <- list(
    analysis = analysis,
    mode = mode,
    "effect-metric" = summary_row$effect_metric,
    "effect-size" = summary_row$effect_size,
    alpha = alpha,
    power = summary_row$power,
    "t-type" = if (analysis == "ttest") t_type else NULL,
    alternative = if (analysis %in% c("ttest", "correlation")) alternative else NULL,
    ratio = if (analysis == "ttest" && t_type == "two-sample") summary_row$ratio else NULL,
    "n" = if (!is.na(summary_row$n_total)) summary_row$n_total else NULL,
    "n-per-group" = if (!is.na(summary_row$n_per_group)) summary_row$n_per_group else NULL,
    n1 = if (!is.na(summary_row$n1)) summary_row$n1 else NULL,
    n2 = if (!is.na(summary_row$n2)) summary_row$n2 else NULL,
    groups = if (!is.na(summary_row$groups)) summary_row$groups else NULL,
    u = if (!is.na(summary_row$u)) summary_row$u else NULL,
    df = if (!is.na(summary_row$df)) summary_row$df else NULL,
    rmsea0 = if (analysis == "sem") summary_row$rmsea0 else NULL,
    rmsea1 = if (analysis == "sem") summary_row$rmsea1 else NULL,
    "estimate-effect" = if (estimate_effect) TRUE else NULL,
    digits = digits
  )

  template_override <- resolve_template_override(opts$template, module = "power")
  template_path <- if (!is.null(template_override)) {
    template_override
  } else {
    resolve_get_template_path("power.default", "power/default-template.md")
  }

  template_meta <- resolve_get_template_meta(template_path)
  table_result <- build_power_table_body(summary_df, digits, template_meta$table)
  nlss_table <- paste0("Table 1\n\n", table_result$body, "\n", note_tokens$note_default)

  template_context <- list(
    tokens = c(
      list(
        table_body = table_result$body,
        narrative_default = narrative
      ),
      note_tokens
    ),
    narrative_rows = list(list(full_sentence = narrative))
  )

  nlss_report_path <- file.path(out_dir, "report_canonical.md")
  resolve_append_nlss_report(
    nlss_report_path,
    "Power analysis",
    nlss_table,
    narrative,
    analysis_flags = analysis_flags,
    template_path = template_path,
    template_context = template_context
  )

  cat("Wrote:\n")
  cat("- ", render_output_path(nlss_report_path, out_dir), "\n", sep = "")

  if (resolve_parse_bool(opts$log, default = log_default)) {
    ctx <- resolve_get_run_context()
    resolve_append_analysis_log(
      out_dir,
      module = "power",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(
        summary_df = summary_df
      ),
      options = list(
        analysis = analysis,
        mode = mode,
        effect_metric = effect_metric_input,
        effect_size = effect_size_input,
        effect_size_calc = effect_size_calc,
        alpha = alpha,
        power = power_target,
        t_type = if (analysis == "ttest") t_type else NULL,
        alternative = if (analysis %in% c("ttest", "correlation")) alternative else NULL,
        ratio = ratio,
        n_total = n_total,
        n_per_group = n_per_group,
        n1 = n1,
        n2 = n2,
        groups = groups,
        u = u,
        df = df_sem,
        rmsea0 = rmsea0,
        rmsea1 = rmsea1,
        estimate_effect = estimate_effect,
        effect_source = effect_source
      ),
      user_prompt = resolve_get_user_prompt(opts)
    )
  }
}

main()
