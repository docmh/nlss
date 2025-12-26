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

print_usage <- function() {
  cat("ANOVA (base R)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript anova.R --csv data.csv --dv outcome --between group\n")
  cat("  Rscript anova.R --csv data.csv --dv outcome --between group,gender --covariates age\n")
  cat("  Rscript anova.R --csv data.csv --within pre,post --subject-id id\n")
  cat("  Rscript anova.R --csv data.csv --within pre,mid,post --between group --subject-id id\n")
  cat("  Rscript anova.R --parquet data.parquet --dv outcome --between group\n")
  cat("  Rscript anova.R --interactive\n")
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
  cat("  --dv NAME              Dependent variable (between-subjects)\n")
  cat("  --between LIST         Comma-separated between-subjects factors\n")
  cat("  --within LIST           Comma-separated within-subjects variables (wide format)\n")
  cat("  --subject-id NAME      Subject identifier (required for within/mixed)\n")
  cat("  --covariates LIST       Comma-separated covariates (numeric)\n")
  cat("  --type TYPE            Sum of squares type (I/II/III; default: II)\n")
  cat("  --effect-size TYPE     eta_sq or partial_eta (default: partial_eta)\n")
  cat("  --posthoc TYPE          none/tukey/pairwise (default: tukey)\n")
  cat("  --p-adjust METHOD      P-value adjustment (default: holm)\n")
  cat("  --conf-level VALUE     Confidence level (default: 0.95)\n")
  cat("  --sphericity MODE      auto/none (default: auto)\n")
  cat("  --bootstrap TRUE/FALSE Bootstrap confidence intervals (default: FALSE)\n")
  cat("  --bootstrap-samples N  Bootstrap resamples (default: 1000)\n")
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

  mode <- resolve_prompt("Design (between/within/mixed)", "between")
  mode <- tolower(mode)

  if (mode %in% c("between", "mixed")) {
    opts$dv <- resolve_prompt("Dependent variable", "")
    opts$between <- resolve_prompt("Between-subjects factors (comma-separated)", "")
    opts$covariates <- resolve_prompt("Covariates (comma-separated, optional)", "")
  }

  if (mode %in% c("within", "mixed")) {
    opts$within <- resolve_prompt("Within-subjects variables (comma-separated, wide format)", "")
    opts$`subject-id` <- resolve_prompt("Subject ID", "")
    if (mode == "mixed" && (is.null(opts$between) || opts$between == "")) {
      opts$between <- resolve_prompt("Between-subjects factors (comma-separated)", "")
    }
  }

  type_default <- resolve_config_value("modules.anova.type", "II")
  effect_default <- resolve_config_value("modules.anova.effect_size", "partial_eta")
  posthoc_default <- resolve_config_value("modules.anova.posthoc", "tukey")
  p_adjust_default <- resolve_config_value("modules.anova.p_adjust", "holm")
  conf_default <- resolve_config_value("modules.anova.conf_level", 0.95)
  sphericity_default <- resolve_config_value("modules.anova.sphericity", "auto")
  bootstrap_default <- resolve_config_value("modules.anova.bootstrap", FALSE)
  bootstrap_samples_default <- resolve_config_value("modules.anova.bootstrap_samples", 1000)
  digits_default <- resolve_config_value("defaults.digits", 2)

  opts$type <- resolve_prompt("Sum of squares type (I/II/III)", type_default)
  opts$`effect-size` <- resolve_prompt("Effect size (eta_sq/partial_eta)", effect_default)
  opts$posthoc <- resolve_prompt("Post-hoc method (none/tukey/pairwise)", posthoc_default)
  opts$`p-adjust` <- resolve_prompt("P-value adjustment", p_adjust_default)
  opts$`conf-level` <- resolve_prompt("Confidence level", as.character(conf_default))
  opts$sphericity <- resolve_prompt("Sphericity (auto/none)", sphericity_default)
  opts$bootstrap <- resolve_prompt("Bootstrap TRUE/FALSE", ifelse(isTRUE(bootstrap_default), "TRUE", "FALSE"))
  opts$`bootstrap-samples` <- resolve_prompt("Bootstrap samples", as.character(bootstrap_samples_default))
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

resolve_default_out <- function() {
  if (exists("get_default_out", mode = "function")) {
    return(get("get_default_out", mode = "function")())
  }
  "./outputs/tmp"
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

resolve_ensure_out_dir <- function(path) {
  if (exists("ensure_out_dir", mode = "function")) {
    return(get("ensure_out_dir", mode = "function")(path))
  }
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  path
}

resolve_load_dataframe <- function(opts) {
  if (exists("load_dataframe", mode = "function")) {
    return(get("load_dataframe", mode = "function")(opts))
  }
  stop("Missing load_dataframe. Ensure lib/io.R is sourced.")
}


resolve_get_workspace_out_dir <- function(df) {
  if (exists("get_workspace_out_dir", mode = "function")) {
    return(get("get_workspace_out_dir", mode = "function")(df))
  }
  stop("Missing get_workspace_out_dir. Ensure lib/io.R is sourced.")
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

resolve_append_apa_report <- function(path, analysis_label, apa_table, apa_text, analysis_flags = NULL, template_path = NULL, template_context = NULL) {
  if (exists("append_apa_report", mode = "function")) {
    return(get("append_apa_report", mode = "function")(
      path,
      analysis_label,
      apa_table,
      apa_text,
      analysis_flags = analysis_flags,
      template_path = template_path,
      template_context = template_context
    ))
  }
  stop("Missing append_apa_report. Ensure lib/formatting.R is sourced.")
}

resolve_get_run_context <- function() {
  if (exists("get_run_context", mode = "function")) {
    return(get("get_run_context", mode = "function")())
  }
  trailing <- commandArgs(trailingOnly = TRUE)
  commands <- c("Rscript", trailing)
  commands <- commands[nzchar(commands)]
  prompt <- paste(commands, collapse = " ")
  list(prompt = prompt, commands = commands)
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

emit_input_issue <- function(out_dir, opts, message, details = list(), status = "invalid_input") {
  log_default <- resolve_config_value("defaults.log", TRUE)
  if (resolve_parse_bool(opts$log, default = log_default)) {
    ctx <- resolve_get_run_context()
    resolve_append_analysis_log(
      out_dir,
      module = "anova",
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

normalize_type <- function(value, default = "II") {
  val <- if (!is.null(value) && value != "") value else default
  val <- toupper(gsub("[^A-Za-z0-9]", "", val))
  if (val %in% c("1", "I")) return("I")
  if (val %in% c("2", "II")) return("II")
  if (val %in% c("3", "III")) return("III")
  default
}

normalize_effect_size <- function(value, default = "partial_eta") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(gsub("[^a-z0-9]", "", val))
  if (val %in% c("eta", "etasq", "eta2", "etasquared")) return("eta_sq")
  if (val %in% c("partialeta", "partialetasq", "partialeta2", "partialetasquared", "peta", "petasq")) {
    return("partial_eta_sq")
  }
  if (val == "partial_eta") return("partial_eta_sq")
  if (val == "eta_sq") return("eta_sq")
  default
}

normalize_posthoc <- function(value, default = "tukey") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (val %in% c("none", "no")) return("none")
  if (val %in% c("pairwise", "pairs")) return("pairwise")
  if (val %in% c("tukey", "tukeyhsd")) return("tukey")
  default
}

normalize_sphericity <- function(value, default = "auto") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (val %in% c("none", "no")) return("none")
  "auto"
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

format_p <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value < 0.001) return("< .001")
  p_txt <- format(round(p_value, 3), nsmall = 3, trim = TRUE)
  p_txt <- sub("^0", "", p_txt)
  paste("=", p_txt)
}

format_ci <- function(low, high, digits) {
  if (is.na(low) || is.na(high)) return("")
  paste0("[", format_stat(low, digits), ", ", format_stat(high, digits), "]")
}

format_effect_label <- function(effect_size) {
  if (effect_size == "eta_sq") return("eta_sq")
  "eta_p2"
}

calc_boot_ci <- function(values, conf_level) {
  values <- values[!is.na(values)]
  if (length(values) == 0) return(c(NA_real_, NA_real_))
  alpha <- (1 - conf_level) / 2
  low <- as.numeric(stats::quantile(values, probs = alpha, names = FALSE, na.rm = TRUE))
  high <- as.numeric(stats::quantile(values, probs = 1 - alpha, names = FALSE, na.rm = TRUE))
  c(low, high)
}

build_term_ids <- function(summary_df) {
  paste(summary_df$model, summary_df$term, sep = "|")
}

bootstrap_effect_sizes_between <- function(data_subset, dv, between_vars, covariates, type, effect_size_label, bootstrap_samples, term_ids) {
  boot_vals <- vector("list", length(term_ids))
  names(boot_vals) <- term_ids
  n <- nrow(data_subset)
  if (n == 0 || bootstrap_samples <= 0) return(boot_vals)
  for (i in seq_len(bootstrap_samples)) {
    idx <- sample(seq_len(n), size = n, replace = TRUE)
    sample_df <- data_subset[idx, , drop = FALSE]
    fits <- tryCatch(build_between_model(sample_df, dv, between_vars, covariates), error = function(e) NULL)
    if (is.null(fits)) next
    summary_result <- tryCatch(extract_between_summary(fits$lm, type), error = function(e) NULL)
    if (is.null(summary_result)) next
    boot_summary <- summary_result$summary
    if (is.null(boot_summary) || nrow(boot_summary) == 0) next
    boot_summary$term_id <- paste(boot_summary$model, boot_summary$term, sep = "|")
    for (j in seq_len(nrow(boot_summary))) {
      term_id <- boot_summary$term_id[j]
      idx_match <- match(term_id, term_ids)
      if (is.na(idx_match)) next
      val <- if (effect_size_label == "eta_sq") boot_summary$eta_sq[j] else boot_summary$partial_eta_sq[j]
      boot_vals[[idx_match]] <- c(boot_vals[[idx_match]], val)
    }
  }
  boot_vals
}

bootstrap_effect_sizes_within <- function(data_within, within_vars, between_vars, covariates, subject_id, effect_size_label, bootstrap_samples, term_ids) {
  boot_vals <- vector("list", length(term_ids))
  names(boot_vals) <- term_ids
  n <- nrow(data_within$wide)
  if (n == 0 || bootstrap_samples <= 0) return(boot_vals)
  within_name <- data_within$within_name
  for (i in seq_len(bootstrap_samples)) {
    idx <- sample(seq_len(n), size = n, replace = TRUE)
    sample_wide <- data_within$wide[idx, , drop = FALSE]
    sample_wide[[subject_id]] <- seq_len(nrow(sample_wide))
    sample_long <- reshape(
      sample_wide,
      varying = within_vars,
      v.names = "dv",
      timevar = within_name,
      times = within_vars,
      idvar = subject_id,
      direction = "long"
    )
    sample_long[[within_name]] <- factor(sample_long[[within_name]], levels = within_vars)
    fit <- tryCatch(build_within_model(sample_long, subject_id, within_name, between_vars, covariates), error = function(e) NULL)
    if (is.null(fit)) next
    summary_result <- tryCatch(extract_within_summary(fit$aov, subject_id, within_name), error = function(e) NULL)
    if (is.null(summary_result)) next
    boot_summary <- summary_result$summary
    if (is.null(boot_summary) || nrow(boot_summary) == 0) next
    boot_summary$term_id <- paste(boot_summary$model, boot_summary$term, sep = "|")
    for (j in seq_len(nrow(boot_summary))) {
      term_id <- boot_summary$term_id[j]
      idx_match <- match(term_id, term_ids)
      if (is.na(idx_match)) next
      val <- if (effect_size_label == "eta_sq") boot_summary$eta_sq[j] else boot_summary$partial_eta_sq[j]
      boot_vals[[idx_match]] <- c(boot_vals[[idx_match]], val)
    }
  }
  boot_vals
}

apply_bootstrap_ci <- function(summary_df, boot_vals, conf_level) {
  summary_df$boot_ci_low <- NA_real_
  summary_df$boot_ci_high <- NA_real_
  if (length(boot_vals) == 0) return(summary_df)
  for (i in seq_len(nrow(summary_df))) {
    vals <- boot_vals[[i]]
    if (is.null(vals) || length(vals) == 0) next
    ci <- calc_boot_ci(vals, conf_level)
    summary_df$boot_ci_low[i] <- ci[1]
    summary_df$boot_ci_high[i] <- ci[2]
  }
  summary_df
}

safe_shapiro <- function(values, max_n = 5000) {
  values <- values[!is.na(values)]
  n <- length(values)
  if (n < 3 || n > max_n) {
    return(list(w = NA_real_, p = NA_real_, n = n, note = "Shapiro-Wilk requires 3-5000 observations."))
  }
  test <- tryCatch(shapiro.test(values), error = function(e) NULL)
  if (is.null(test)) return(list(w = NA_real_, p = NA_real_, n = n, note = "Shapiro-Wilk failed."))
  list(w = unname(test$statistic), p = test$p.value, n = n, note = "")
}

calc_levene <- function(values, group) {
  group <- as.factor(group)
  if (nlevels(group) < 2) {
    return(list(stat = NA_real_, df1 = NA_real_, df2 = NA_real_, p = NA_real_, note = "Need at least two groups."))
  }
  fit <- tryCatch(
    lm(abs(values - tapply(values, group, median, na.rm = TRUE)[as.character(group)]) ~ group),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    return(list(stat = NA_real_, df1 = NA_real_, df2 = NA_real_, p = NA_real_, note = "Levene test failed."))
  }
  res <- anova(fit)
  list(
    stat = res$`F value`[1],
    df1 = res$Df[1],
    df2 = res$Df[2],
    p = res$`Pr(>F)`[1],
    note = ""
  )
}

calc_bartlett <- function(values, group) {
  test <- tryCatch(bartlett.test(values, group), error = function(e) NULL)
  if (is.null(test)) {
    return(list(stat = NA_real_, df1 = NA_real_, df2 = NA_real_, p = NA_real_, note = "Bartlett test failed."))
  }
  list(
    stat = unname(test$statistic),
    df1 = unname(test$parameter),
    df2 = NA_real_,
    p = test$p.value,
    note = ""
  )
}

calc_fligner <- function(values, group) {
  test <- tryCatch(fligner.test(values, group), error = function(e) NULL)
  if (is.null(test)) {
    return(list(stat = NA_real_, df1 = NA_real_, df2 = NA_real_, p = NA_real_, note = "Fligner-Killeen test failed."))
  }
  list(
    stat = unname(test$statistic),
    df1 = unname(test$parameter),
    df2 = NA_real_,
    p = test$p.value,
    note = ""
  )
}

coerce_numeric <- function(vec, name) {
  if (is.numeric(vec)) return(vec)
  out <- suppressWarnings(as.numeric(as.character(vec)))
  if (all(is.na(out))) {
    stop(paste("Covariate", name, "could not be coerced to numeric."))
  }
  out
}

prepare_between_data <- function(df, dv, between_vars, covariates) {
  if (!(dv %in% names(df))) {
    stop(paste("Dependent variable not found:", dv))
  }
  required <- c(dv, between_vars, covariates)
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(paste("Unknown variables:", paste(missing, collapse = ", ")))
  }
  data_subset <- df[, required, drop = FALSE]
  data_subset <- data_subset[complete.cases(data_subset), , drop = FALSE]
  if (nrow(data_subset) == 0) stop("No complete cases available for analysis.")
  if (!is.numeric(data_subset[[dv]])) {
    data_subset[[dv]] <- suppressWarnings(as.numeric(as.character(data_subset[[dv]])))
  }
  if (all(is.na(data_subset[[dv]]))) {
    stop("Dependent variable must be numeric.")
  }
  for (var in between_vars) {
    data_subset[[var]] <- as.factor(data_subset[[var]])
  }
  for (var in covariates) {
    data_subset[[var]] <- coerce_numeric(data_subset[[var]], var)
  }
  data_subset
}

prepare_within_data <- function(df, within_vars, subject_id, between_vars, covariates) {
  required <- c(subject_id, within_vars, between_vars, covariates)
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(paste("Unknown variables:", paste(missing, collapse = ", ")))
  }
  data_subset <- df[, required, drop = FALSE]
  data_subset <- data_subset[complete.cases(data_subset), , drop = FALSE]
  if (nrow(data_subset) == 0) stop("No complete cases available for analysis.")
  for (var in between_vars) {
    data_subset[[var]] <- as.factor(data_subset[[var]])
  }
  for (var in covariates) {
    data_subset[[var]] <- coerce_numeric(data_subset[[var]], var)
  }
  for (var in within_vars) {
    if (!is.numeric(data_subset[[var]])) {
      data_subset[[var]] <- suppressWarnings(as.numeric(as.character(data_subset[[var]])))
    }
    if (all(is.na(data_subset[[var]]))) {
      stop(paste("Within variable must be numeric:", var))
    }
  }
  within_name <- "within"
  long_data <- reshape(
    data_subset,
    varying = within_vars,
    v.names = "dv",
    timevar = within_name,
    times = within_vars,
    idvar = subject_id,
    direction = "long"
  )
  long_data[[within_name]] <- factor(long_data[[within_name]], levels = within_vars)
  list(wide = data_subset, long = long_data, within_name = within_name)
}

build_between_model <- function(data_subset, dv, between_vars, covariates) {
  between_term <- if (length(between_vars) > 0) paste(between_vars, collapse = " * ") else "1"
  cov_term <- if (length(covariates) > 0) paste(covariates, collapse = " + ") else ""
  rhs <- between_term
  if (nzchar(cov_term)) rhs <- paste(rhs, "+", cov_term)
  formula <- as.formula(paste(dv, "~", rhs))
  list(lm = lm(formula, data = data_subset), aov = aov(formula, data = data_subset), formula = formula)
}

build_within_model <- function(long_data, subject_id, within_name, between_vars, covariates) {
  between_term <- if (length(between_vars) > 0) paste(between_vars, collapse = " * ") else ""
  fixed_term <- if (nzchar(between_term)) paste(between_term, "*", within_name) else within_name
  if (length(covariates) > 0) {
    fixed_term <- paste(fixed_term, "+", paste(covariates, collapse = " + "))
  }
  formula <- as.formula(paste("dv ~", fixed_term, "+ Error(", subject_id, "/", within_name, ")"))
  list(aov = aov(formula, data = long_data), formula = formula)
}

extract_between_summary <- function(lm_fit, type) {
  use_type <- normalize_type(type, "II")
  use_car <- use_type != "I" && requireNamespace("car", quietly = TRUE)
  table <- NULL
  used_type <- use_type
  if (use_car) {
    anova_tbl <- tryCatch(car::Anova(lm_fit, type = ifelse(use_type == "III", 3, 2)), error = function(e) NULL)
    if (!is.null(anova_tbl)) {
      table <- as.data.frame(anova_tbl)
      table$term <- rownames(table)
    }
  }
  if (is.null(table)) {
    used_type <- "I"
    anova_tbl <- anova(lm_fit)
    table <- as.data.frame(anova_tbl)
    table$term <- rownames(table)
  }

  table <- table[!(table$term %in% c("(Intercept)", "Residuals")), , drop = FALSE]
  residual_df <- df.residual(lm_fit)
  residual_ss <- sum(resid(lm_fit)^2, na.rm = TRUE)
  ss_total <- sum(table$`Sum Sq`, na.rm = TRUE) + residual_ss

  f_col <- if ("F value" %in% names(table)) "F value" else if ("F" %in% names(table)) "F" else NULL
  p_col <- if ("Pr(>F)" %in% names(table)) "Pr(>F)" else if ("Pr(>Chisq)" %in% names(table)) "Pr(>Chisq)" else NULL
  ms_col <- if ("Mean Sq" %in% names(table)) "Mean Sq" else NULL

  rows <- list()
  for (i in seq_len(nrow(table))) {
    row <- table[i, ]
    ss <- row$`Sum Sq`
    df1 <- row$Df
    ms <- if (!is.null(ms_col)) row[[ms_col]] else ss / df1
    f_val <- if (!is.null(f_col)) row[[f_col]] else if (!is.na(residual_df) && residual_df > 0) {
      ms / (residual_ss / residual_df)
    } else {
      NA_real_
    }
    p_val <- if (!is.null(p_col)) row[[p_col]] else NA_real_
    eta_sq <- if (!is.na(ss_total) && ss_total > 0) ss / ss_total else NA_real_
    partial_eta <- if (!is.na(residual_ss) && (ss + residual_ss) > 0) ss / (ss + residual_ss) else NA_real_
    rows[[length(rows) + 1]] <- data.frame(
      model = "Between",
      term = row$term,
      df1 = df1,
      df2 = residual_df,
      ss = ss,
      ms = ms,
      f = f_val,
      p = p_val,
      eta_sq = eta_sq,
      partial_eta_sq = partial_eta,
      df1_gg = NA_real_,
      df2_gg = NA_real_,
      p_gg = NA_real_,
      df1_hf = NA_real_,
      df2_hf = NA_real_,
      p_hf = NA_real_,
      stringsAsFactors = FALSE
    )
  }
  list(summary = do.call(rbind, rows), ss_total = ss_total, used_type = used_type)
}

extract_summary_table <- function(summary_obj) {
  if (is.data.frame(summary_obj)) return(summary_obj)
  if (is.list(summary_obj) && length(summary_obj) > 0) {
    if (is.data.frame(summary_obj[[1]])) return(summary_obj[[1]])
  }
  NULL
}

label_stratum <- function(stratum_name, subject_id, within_name) {
  if (is.null(stratum_name) || !nzchar(stratum_name)) return("")
  label <- gsub("^Error:\\s*", "", stratum_name)
  if (!is.null(subject_id) && nzchar(subject_id)) {
    if (grepl(paste0(subject_id, ":", within_name), label, fixed = TRUE)) return("Within")
    if (grepl(subject_id, label, fixed = TRUE)) return("Between")
  }
  label
}

extract_within_summary <- function(aov_fit, subject_id, within_name) {
  summaries <- summary(aov_fit)
  ss_total <- 0
  for (name in names(summaries)) {
    table <- extract_summary_table(summaries[[name]])
    if (is.null(table)) next
    if ("Sum Sq" %in% names(table)) {
      ss_total <- ss_total + sum(table$`Sum Sq`, na.rm = TRUE)
    }
  }

  rows <- list()
  for (name in names(summaries)) {
    table <- extract_summary_table(summaries[[name]])
    if (is.null(table)) next
    if (!"Sum Sq" %in% names(table)) next
    resid_idx <- which(rownames(table) == "Residuals")
    error_ss <- if (length(resid_idx) > 0) table$`Sum Sq`[resid_idx[1]] else NA_real_
    error_df <- if (length(resid_idx) > 0) table$Df[resid_idx[1]] else NA_real_
    model_label <- label_stratum(name, subject_id, within_name)

    for (i in seq_len(nrow(table))) {
      term <- rownames(table)[i]
      if (term == "Residuals" || term == "(Intercept)") next
      row <- table[i, ]
      ss <- row$`Sum Sq`
      df1 <- row$Df
      ms <- if ("Mean Sq" %in% names(row)) row$`Mean Sq` else ss / df1
      f_val <- if ("F value" %in% names(row)) row$`F value` else NA_real_
      p_val <- if ("Pr(>F)" %in% names(row)) row$`Pr(>F)` else NA_real_
      eta_sq <- if (!is.na(ss_total) && ss_total > 0) ss / ss_total else NA_real_
      partial_eta <- if (!is.na(error_ss) && (ss + error_ss) > 0) ss / (ss + error_ss) else NA_real_
      rows[[length(rows) + 1]] <- data.frame(
        model = model_label,
        term = term,
        df1 = df1,
        df2 = error_df,
        ss = ss,
        ms = ms,
        f = f_val,
        p = p_val,
        eta_sq = eta_sq,
        partial_eta_sq = partial_eta,
        df1_gg = NA_real_,
        df2_gg = NA_real_,
        p_gg = NA_real_,
        df1_hf = NA_real_,
        df2_hf = NA_real_,
        p_hf = NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }
  list(summary = do.call(rbind, rows), ss_total = ss_total)
}

build_between_posthoc_tukey <- function(aov_fit) {
  tukey <- tryCatch(TukeyHSD(aov_fit), error = function(e) NULL)
  if (is.null(tukey)) return(data.frame())
  rows <- list()
  for (term in names(tukey)) {
    table <- as.data.frame(tukey[[term]])
    table$contrast <- rownames(table)
    for (i in seq_len(nrow(table))) {
      row <- table[i, ]
      parts <- strsplit(row$contrast, "-", fixed = TRUE)[[1]]
      group_1 <- if (length(parts) >= 1) parts[1] else ""
      group_2 <- if (length(parts) >= 2) parts[2] else ""
      rows[[length(rows) + 1]] <- data.frame(
        term = term,
        group = "",
        group_1 = group_1,
        group_2 = group_2,
        contrast = row$contrast,
        mean_diff = row$diff,
        se = NA_real_,
        t = NA_real_,
        df = NA_real_,
        p = NA_real_,
        p_adj = row$`p adj`,
        ci_low = row$lwr,
        ci_high = row$upr,
        method = "tukey",
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

build_between_posthoc_pairwise <- function(data_subset, dv, between_vars, p_adjust, conf_level) {
  rows <- list()
  if (length(between_vars) == 0) return(data.frame())
  for (factor_name in between_vars) {
    levels <- unique(data_subset[[factor_name]])
    levels <- levels[!is.na(levels)]
    if (length(levels) < 2) next
    pairs <- combn(as.character(levels), 2, simplify = FALSE)
    p_vals <- numeric(0)
    row_buffer <- list()
    for (pair in pairs) {
      g1 <- pair[1]
      g2 <- pair[2]
      x <- data_subset[[dv]][data_subset[[factor_name]] == g1]
      y <- data_subset[[dv]][data_subset[[factor_name]] == g2]
      test <- tryCatch(t.test(x, y, paired = FALSE, conf.level = conf_level), error = function(e) NULL)
      if (is.null(test)) next
      mean_diff <- mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)
      row_buffer[[length(row_buffer) + 1]] <- list(
        term = factor_name,
        group = "",
        group_1 = g1,
        group_2 = g2,
        contrast = paste(g1, g2, sep = "-") ,
        mean_diff = mean_diff,
        se = NA_real_,
        t = unname(test$statistic),
        df = unname(test$parameter),
        p = test$p.value,
        p_adj = NA_real_,
        ci_low = test$conf.int[1],
        ci_high = test$conf.int[2],
        method = "pairwise",
        stringsAsFactors = FALSE
      )
      p_vals <- c(p_vals, test$p.value)
    }
    if (length(row_buffer) == 0) next
    p_adj_vals <- p.adjust(p_vals, method = p_adjust)
    for (i in seq_along(row_buffer)) {
      row_buffer[[i]]$p_adj <- p_adj_vals[i]
      rows[[length(rows) + 1]] <- as.data.frame(row_buffer[[i]], stringsAsFactors = FALSE)
    }
  }
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

build_within_posthoc_pairwise <- function(data_wide, within_vars, between_vars, conf_level, p_adjust) {
  rows <- list()
  pairs <- combn(within_vars, 2, simplify = FALSE)
  if (length(pairs) == 0) return(data.frame())

  if (length(between_vars) == 0) {
    p_vals <- numeric(0)
    row_buffer <- list()
    for (pair in pairs) {
      v1 <- pair[1]
      v2 <- pair[2]
      test <- tryCatch(t.test(data_wide[[v1]], data_wide[[v2]], paired = TRUE, conf.level = conf_level), error = function(e) NULL)
      if (is.null(test)) next
      mean_diff <- mean(data_wide[[v1]] - data_wide[[v2]], na.rm = TRUE)
      row_buffer[[length(row_buffer) + 1]] <- list(
        term = "within",
        group = "Overall",
        group_1 = v1,
        group_2 = v2,
        contrast = paste(v1, v2, sep = "-") ,
        mean_diff = mean_diff,
        se = NA_real_,
        t = unname(test$statistic),
        df = unname(test$parameter),
        p = test$p.value,
        p_adj = NA_real_,
        ci_low = test$conf.int[1],
        ci_high = test$conf.int[2],
        method = "paired",
        stringsAsFactors = FALSE
      )
      p_vals <- c(p_vals, test$p.value)
    }
    if (length(row_buffer) > 0) {
      p_adj_vals <- p.adjust(p_vals, method = p_adjust)
      for (i in seq_along(row_buffer)) {
        row_buffer[[i]]$p_adj <- p_adj_vals[i]
        rows[[length(rows) + 1]] <- as.data.frame(row_buffer[[i]], stringsAsFactors = FALSE)
      }
    }
  } else {
    combos <- unique(data_wide[, between_vars, drop = FALSE])
    for (i in seq_len(nrow(combos))) {
      combo <- combos[i, , drop = FALSE]
      subset_idx <- rep(TRUE, nrow(data_wide))
      for (var in between_vars) {
        subset_idx <- subset_idx & data_wide[[var]] == combo[[var]]
      }
      subset_data <- data_wide[subset_idx, , drop = FALSE]
      if (nrow(subset_data) == 0) next
      group_label <- paste(paste0(between_vars, "=", combo[1, ]), collapse = ", ")
      p_vals <- numeric(0)
      row_buffer <- list()
      for (pair in pairs) {
        v1 <- pair[1]
        v2 <- pair[2]
        test <- tryCatch(t.test(subset_data[[v1]], subset_data[[v2]], paired = TRUE, conf.level = conf_level), error = function(e) NULL)
        if (is.null(test)) next
        mean_diff <- mean(subset_data[[v1]] - subset_data[[v2]], na.rm = TRUE)
        row_buffer[[length(row_buffer) + 1]] <- list(
          term = "within",
          group = group_label,
          group_1 = v1,
          group_2 = v2,
          contrast = paste(v1, v2, sep = "-") ,
          mean_diff = mean_diff,
          se = NA_real_,
          t = unname(test$statistic),
          df = unname(test$parameter),
          p = test$p.value,
          p_adj = NA_real_,
          ci_low = test$conf.int[1],
          ci_high = test$conf.int[2],
          method = "paired",
          stringsAsFactors = FALSE
        )
        p_vals <- c(p_vals, test$p.value)
      }
      if (length(row_buffer) > 0) {
        p_adj_vals <- p.adjust(p_vals, method = p_adjust)
        for (j in seq_along(row_buffer)) {
          row_buffer[[j]]$p_adj <- p_adj_vals[j]
          rows[[length(rows) + 1]] <- as.data.frame(row_buffer[[j]], stringsAsFactors = FALSE)
        }
      }
    }
  }
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

build_assumptions <- function(mode, data_between, data_within, dv, between_vars, within_vars, covariates, subject_id, alpha, max_shapiro_n, sphericity) {
  rows <- list()

  if (mode == "between") {
    residuals <- resid(data_between$lm)
    shapiro <- safe_shapiro(residuals, max_n = max_shapiro_n)
    rows[[length(rows) + 1]] <- data.frame(
      assumption = "Normality",
      test = "Shapiro-Wilk",
      target = "Residuals",
      group = "",
      statistic = shapiro$w,
      df1 = NA_real_,
      df2 = NA_real_,
      p = shapiro$p,
      note = shapiro$note,
      stringsAsFactors = FALSE
    )

    if (length(between_vars) > 0) {
      group_factor <- interaction(data_between$data[, between_vars, drop = FALSE], drop = TRUE)
      levene <- calc_levene(data_between$data[[dv]], group_factor)
      rows[[length(rows) + 1]] <- data.frame(
        assumption = "Homogeneity",
        test = "Levene (median)",
        target = dv,
        group = "",
        statistic = levene$stat,
        df1 = levene$df1,
        df2 = levene$df2,
        p = levene$p,
        note = levene$note,
        stringsAsFactors = FALSE
      )
      bartlett <- calc_bartlett(data_between$data[[dv]], group_factor)
      rows[[length(rows) + 1]] <- data.frame(
        assumption = "Homogeneity",
        test = "Bartlett",
        target = dv,
        group = "",
        statistic = bartlett$stat,
        df1 = bartlett$df1,
        df2 = bartlett$df2,
        p = bartlett$p,
        note = bartlett$note,
        stringsAsFactors = FALSE
      )
      fligner <- calc_fligner(data_between$data[[dv]], group_factor)
      rows[[length(rows) + 1]] <- data.frame(
        assumption = "Homogeneity",
        test = "Fligner-Killeen",
        target = dv,
        group = "",
        statistic = fligner$stat,
        df1 = fligner$df1,
        df2 = fligner$df2,
        p = fligner$p,
        note = fligner$note,
        stringsAsFactors = FALSE
      )
    }
  }

  if (mode %in% c("within", "mixed")) {
    within_name <- data_within$within_name
    terms <- c(within_name, between_vars, covariates)
    rhs <- if (length(terms) > 0) paste(terms, collapse = " + ") else "1"
    form <- as.formula(paste("dv ~", rhs))
    lm_fit <- tryCatch(lm(form, data = data_within$long), error = function(e) NULL)
    if (!is.null(lm_fit)) {
      residuals <- resid(lm_fit)
      shapiro <- safe_shapiro(residuals, max_n = max_shapiro_n)
      rows[[length(rows) + 1]] <- data.frame(
        assumption = "Normality",
        test = "Shapiro-Wilk",
        target = "Residuals",
        group = "",
        statistic = shapiro$w,
        df1 = NA_real_,
        df2 = NA_real_,
        p = shapiro$p,
        note = shapiro$note,
        stringsAsFactors = FALSE
      )
    }

    if (length(between_vars) > 0) {
      group_factor <- interaction(data_within$wide[, between_vars, drop = FALSE], drop = TRUE)
      for (var in within_vars) {
        levene <- calc_levene(data_within$wide[[var]], group_factor)
        rows[[length(rows) + 1]] <- data.frame(
          assumption = "Homogeneity",
          test = "Levene (median)",
          target = var,
          group = "",
          statistic = levene$stat,
          df1 = levene$df1,
          df2 = levene$df2,
          p = levene$p,
          note = levene$note,
          stringsAsFactors = FALSE
        )
      }
    }

    if (sphericity == "auto" && length(within_vars) >= 3) {
      response_formula <- paste0("cbind(", paste(within_vars, collapse = ", "), ")")
      rhs <- if (length(between_vars) > 0) {
        paste(between_vars, collapse = " + ")
      } else {
        "1"
      }
      form <- as.formula(paste(response_formula, "~", rhs))
      fit <- tryCatch(lm(form, data = data_within$wide), error = function(e) NULL)
      if (!is.null(fit)) {
        test <- tryCatch(mauchly.test(fit), error = function(e) NULL)
        if (!is.null(test)) {
          rows[[length(rows) + 1]] <- data.frame(
            assumption = "Sphericity",
            test = "Mauchly",
            target = "Within",
            group = "",
            statistic = unname(test$statistic),
            df1 = NA_real_,
            df2 = NA_real_,
            p = test$p.value,
            note = "",
            stringsAsFactors = FALSE
          )
        } else {
          rows[[length(rows) + 1]] <- data.frame(
            assumption = "Sphericity",
            test = "Mauchly",
            target = "Within",
            group = "",
            statistic = NA_real_,
            df1 = NA_real_,
            df2 = NA_real_,
            p = NA_real_,
            note = "Mauchly test failed.",
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

summarize_assumptions <- function(assumptions_df, alpha) {
  if (is.null(assumptions_df) || nrow(assumptions_df) == 0) return("")
  violations <- assumptions_df[!is.na(assumptions_df$p) & assumptions_df$p < alpha, , drop = FALSE]
  if (nrow(violations) == 0) return(paste0("No assumption violations flagged at alpha = ", alpha, "."))
  labels <- unique(paste(violations$assumption, "(", violations$test, ")", sep = ""))
  paste0("Potential violations: ", paste(labels, collapse = "; "), ".")
}

format_apa_table <- function(summary_df, digits, note_text, effect_size_label) {
  headers <- c("Model", "Effect", "df1", "df2", "F", "p", effect_size_label)
  rows <- list()
  for (i in seq_len(nrow(summary_df))) {
    row <- summary_df[i, ]
    es <- if (effect_size_label == "eta_sq") row$eta_sq else row$partial_eta_sq
    rows[[length(rows) + 1]] <- c(
      row$model,
      row$term,
      format_num(row$df1, digits),
      format_num(row$df2, digits),
      format_stat(row$f, digits),
      format_p(row$p),
      format_stat(es, digits)
    )
  }
  table_md <- paste0("| ", paste(headers, collapse = " | "), " |\n")
  table_md <- paste0(table_md, "| ", paste(rep("---", length(headers)), collapse = " | "), " |\n")
  for (row in rows) {
    table_md <- paste0(table_md, "| ", paste(row, collapse = " | "), " |\n")
  }
  paste0("Table 1\n\n", table_md, "\n", note_text)
}

format_posthoc_table <- function(posthoc_df, digits, note_text) {
  headers <- c("Effect", "Group", "Group 1", "Group 2", "Mean diff", "t", "df", "p", "p_adj", "CI")
  rows <- list()
  for (i in seq_len(nrow(posthoc_df))) {
    row <- posthoc_df[i, ]
    rows[[length(rows) + 1]] <- c(
      row$term,
      row$group,
      row$group_1,
      row$group_2,
      format_num(row$mean_diff, digits),
      format_stat(row$t, digits),
      format_num(row$df, digits),
      format_p(row$p),
      format_p(row$p_adj),
      format_ci(row$ci_low, row$ci_high, digits)
    )
  }
  table_md <- paste0("| ", paste(headers, collapse = " | "), " |\n")
  table_md <- paste0(table_md, "| ", paste(rep("---", length(headers)), collapse = " | "), " |\n")
  for (row in rows) {
    table_md <- paste0(table_md, "| ", paste(row, collapse = " | "), " |\n")
  }
  paste0("Table 1\n\n", table_md, "\n", note_text)
}

format_apa_text <- function(summary_df, digits, effect_size_label) {
  lines <- character(0)
  for (i in seq_len(nrow(summary_df))) {
    row <- summary_df[i, ]
    if (is.na(row$f) || is.na(row$df1) || is.na(row$df2)) {
      lines <- c(lines, paste0(row$term, ": effect could not be computed."))
      next
    }
    es <- if (effect_size_label == "eta_sq") row$eta_sq else row$partial_eta_sq
    line <- sprintf(
      "%s: F(%s, %s) = %s, p %s, %s = %s.",
      row$term,
      format_num(row$df1, digits),
      format_num(row$df2, digits),
      format_stat(row$f, digits),
      format_p(row$p),
      effect_size_label,
      format_stat(es, digits)
    )
    lines <- c(lines, line)
  }
  paste(lines, collapse = "\n")
}

build_anova_table_body <- function(summary_df, digits, table_meta, effect_size_label) {
  default_specs <- list(
    list(key = "model", label = "Model", drop_if_empty = TRUE),
    list(key = "term", label = "Effect"),
    list(key = "df1", label = "df1"),
    list(key = "df2", label = "df2"),
    list(key = "f", label = "F"),
    list(key = "p", label = "p"),
    list(key = "partial_eta_sq", label = "eta_p2"),
    list(key = "eta_sq", label = "eta_sq", drop_if_empty = TRUE),
    list(key = "boot_ci_low", label = "Boot CI low", drop_if_empty = TRUE),
    list(key = "boot_ci_high", label = "Boot CI high", drop_if_empty = TRUE),
    list(key = "ss", label = "SS", drop_if_empty = TRUE),
    list(key = "ms", label = "MS", drop_if_empty = TRUE),
    list(key = "df1_gg", label = "df1_GG", drop_if_empty = TRUE),
    list(key = "df2_gg", label = "df2_GG", drop_if_empty = TRUE),
    list(key = "p_gg", label = "p_GG", drop_if_empty = TRUE),
    list(key = "df1_hf", label = "df1_HF", drop_if_empty = TRUE),
    list(key = "df2_hf", label = "df2_HF", drop_if_empty = TRUE),
    list(key = "p_hf", label = "p_HF", drop_if_empty = TRUE)
  )
  columns <- resolve_normalize_table_columns(table_meta$columns, default_specs)
  rows <- list()
  for (i in seq_len(nrow(summary_df))) {
    row <- summary_df[i, ]
    eta_val <- if (effect_size_label == "eta_sq") row$eta_sq else NA_real_
    partial_val <- if (effect_size_label == "eta_p2") row$partial_eta_sq else NA_real_
    row_map <- list(
      model = row$model,
      term = row$term,
      df1 = format_num(row$df1, digits),
      df2 = format_num(row$df2, digits),
      f = format_stat(row$f, digits),
      p = format_p(row$p),
      partial_eta_sq = format_stat(partial_val, digits),
      eta_sq = format_stat(eta_val, digits),
      boot_ci_low = format_stat(row$boot_ci_low, digits),
      boot_ci_high = format_stat(row$boot_ci_high, digits),
      ss = format_num(row$ss, digits),
      ms = format_num(row$ms, digits),
      df1_gg = format_num(row$df1_gg, digits),
      df2_gg = format_num(row$df2_gg, digits),
      p_gg = format_p(row$p_gg),
      df1_hf = format_num(row$df1_hf, digits),
      df2_hf = format_num(row$df2_hf, digits),
      p_hf = format_p(row$p_hf)
    )
    row_vals <- vapply(columns, function(col) {
      resolve_as_cell_text(row_map[[col$key]])
    }, character(1))
    rows[[length(rows) + 1]] <- row_vals
  }
  drop_result <- resolve_drop_empty_columns(columns, rows)
  columns <- drop_result$columns
  rows <- drop_result$rows
  headers <- vapply(columns, function(col) col$label, character(1))
  body <- resolve_render_markdown_table(headers, rows)
  list(body = body, columns = columns)
}

build_posthoc_table_body <- function(posthoc_df, digits, table_meta) {
  default_specs <- list(
    list(key = "term", label = "Effect", drop_if_empty = TRUE),
    list(key = "group", label = "Group", drop_if_empty = TRUE),
    list(key = "group_1", label = "Group 1", drop_if_empty = TRUE),
    list(key = "group_2", label = "Group 2", drop_if_empty = TRUE),
    list(key = "contrast", label = "Contrast", drop_if_empty = TRUE),
    list(key = "mean_diff", label = "Mean diff", drop_if_empty = TRUE),
    list(key = "se", label = "SE", drop_if_empty = TRUE),
    list(key = "t", label = "t", drop_if_empty = TRUE),
    list(key = "df", label = "df", drop_if_empty = TRUE),
    list(key = "p", label = "p", drop_if_empty = TRUE),
    list(key = "p_adj", label = "p_adj", drop_if_empty = TRUE),
    list(key = "ci_low", label = "CI low", drop_if_empty = TRUE),
    list(key = "ci_high", label = "CI high", drop_if_empty = TRUE)
  )
  columns <- resolve_normalize_table_columns(table_meta$columns, default_specs)
  rows <- list()
  for (i in seq_len(nrow(posthoc_df))) {
    row <- posthoc_df[i, ]
    row_map <- list(
      term = row$term,
      group = row$group,
      group_1 = row$group_1,
      group_2 = row$group_2,
      contrast = row$contrast,
      mean_diff = format_num(row$mean_diff, digits),
      se = format_num(row$se, digits),
      t = format_stat(row$t, digits),
      df = format_num(row$df, digits),
      p = format_p(row$p),
      p_adj = format_p(row$p_adj),
      ci_low = format_stat(row$ci_low, digits),
      ci_high = format_stat(row$ci_high, digits)
    )
    row_vals <- vapply(columns, function(col) {
      resolve_as_cell_text(row_map[[col$key]])
    }, character(1))
    rows[[length(rows) + 1]] <- row_vals
  }
  drop_result <- resolve_drop_empty_columns(columns, rows)
  columns <- drop_result$columns
  rows <- drop_result$rows
  headers <- vapply(columns, function(col) col$label, character(1))
  body <- resolve_render_markdown_table(headers, rows)
  list(body = body, columns = columns)
}

build_anova_note_tokens <- function(type, effect_size_label, conf_level, posthoc, p_adjust, bootstrap, bootstrap_samples, assumption_note) {
  parts <- c(
    paste0("Sum of squares type ", type, "."),
    paste0("Effect size: ", effect_size_label, "."),
    paste0("Confidence level: ", round(conf_level * 100), "%."),
    ifelse(posthoc != "none", paste0("Post-hoc: ", posthoc, " (p adjust: ", p_adjust, ")."), ""),
    ifelse(bootstrap, paste0("Bootstrap CIs use ", bootstrap_samples, " resamples."), "")
  )
  parts <- parts[nzchar(parts)]
  note_default <- paste(parts, collapse = " ")
  if (nzchar(assumption_note)) {
    note_default <- paste(note_default, assumption_note)
  }
  list(note_default = note_default, assumption_note = assumption_note)
}

build_posthoc_note_tokens <- function(posthoc, p_adjust) {
  note_default <- if (posthoc == "none") "" else paste0("Post-hoc method: ", posthoc, ". P-value adjustment: ", p_adjust, ".")
  list(note_default = note_default)
}

build_anova_narrative_rows <- function(summary_df, digits, effect_size_label) {
  rows <- list()
  apa_text <- format_apa_text(summary_df, digits, effect_size_label)
  lines <- strsplit(apa_text, "\n", fixed = TRUE)[[1]]
  for (i in seq_len(nrow(summary_df))) {
    row <- summary_df[i, ]
    full_sentence <- if (i <= length(lines)) lines[i] else ""
    es <- if (effect_size_label == "eta_sq") row$eta_sq else row$partial_eta_sq
    rows[[length(rows) + 1]] <- list(
      full_sentence = full_sentence,
      model = row$model,
      term = row$term,
      df1 = format_num(row$df1, digits),
      df2 = format_num(row$df2, digits),
      f = format_stat(row$f, digits),
      p = format_p(row$p),
      effect_size_label = effect_size_label,
      effect_size_value = format_stat(es, digits),
      boot_ci = format_ci(row$boot_ci_low, row$boot_ci_high, digits),
      boot_ci_low = format_stat(row$boot_ci_low, digits),
      boot_ci_high = format_stat(row$boot_ci_high, digits)
    )
  }
  rows
}

build_posthoc_narrative_rows <- function(posthoc_df, digits) {
  rows <- list()
  for (i in seq_len(nrow(posthoc_df))) {
    row <- posthoc_df[i, ]
    line <- sprintf(
      "%s: %s vs %s, mean diff = %s, p %s.",
      row$term,
      row$group_1,
      row$group_2,
      format_num(row$mean_diff, digits),
      if (!is.na(row$p_adj)) format_p(row$p_adj) else format_p(row$p)
    )
    rows[[length(rows) + 1]] <- list(
      full_sentence = line,
      term = row$term,
      group = row$group,
      group_1 = row$group_1,
      group_2 = row$group_2,
      mean_diff = format_num(row$mean_diff, digits),
      t = format_stat(row$t, digits),
      df = format_num(row$df, digits),
      p = format_p(row$p),
      p_adj = format_p(row$p_adj)
    )
  }
  rows
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  opts <- resolve_parse_args(args)

  if (!is.null(opts$help)) {
    print_usage()
    quit(status = 0)
  }

  if (!is.null(opts$interactive)) {
    opts <- modifyList(opts, interactive_options())
  }

  digits_default <- resolve_config_value("defaults.digits", 2)
  log_default <- resolve_config_value("defaults.log", TRUE)
  type_default <- resolve_config_value("modules.anova.type", "II")
  effect_default <- resolve_config_value("modules.anova.effect_size", "partial_eta")
  posthoc_default <- resolve_config_value("modules.anova.posthoc", "tukey")
  p_adjust_default <- resolve_config_value("modules.anova.p_adjust", "holm")
  conf_default <- resolve_config_value("modules.anova.conf_level", 0.95)
  sphericity_default <- resolve_config_value("modules.anova.sphericity", "auto")
  bootstrap_default <- resolve_config_value("modules.anova.bootstrap", FALSE)
  bootstrap_samples_default <- resolve_config_value("modules.anova.bootstrap_samples", 1000)
  alpha_default <- resolve_config_value("modules.assumptions.alpha", 0.05)
  max_shapiro_n <- resolve_config_value("modules.assumptions.max_shapiro_n", 5000)

  digits <- if (!is.null(opts$digits)) as.numeric(opts$digits) else digits_default
  df <- resolve_load_dataframe(opts)
  out_dir <- resolve_get_workspace_out_dir(df)

  dv <- if (!is.null(opts$dv)) as.character(opts$dv) else ""
  between_vars <- resolve_parse_list(opts$between)
  within_vars <- resolve_parse_list(opts$within)
  subject_id <- if (!is.null(opts$`subject-id`)) as.character(opts$`subject-id`) else ""
  covariates <- resolve_parse_list(opts$covariates)

  has_between <- length(between_vars) > 0
  has_within <- length(within_vars) > 0

  if (!has_between && !has_within) {
    emit_input_issue(out_dir, opts, "Specify --between or --within for ANOVA.")
  }
  if (!has_within && (!nzchar(dv))) {
    emit_input_issue(out_dir, opts, "Between-subjects ANOVA requires --dv.")
  }
  if (has_within && !nzchar(subject_id)) {
    emit_input_issue(out_dir, opts, "Within-subjects ANOVA requires --subject-id.")
  }
  if (has_within && length(within_vars) < 2) {
    emit_input_issue(out_dir, opts, "Within-subjects ANOVA requires at least two variables in --within.")
  }

  overlap <- intersect(within_vars, c(between_vars, covariates, dv))
  if (length(overlap) > 0) {
    emit_input_issue(out_dir, opts, "Within variables cannot overlap with dv/between/covariates.", details = list(overlap = overlap))
  }

  type <- normalize_type(opts$type, type_default)
  effect_size <- normalize_effect_size(opts$`effect-size`, effect_default)
  posthoc <- normalize_posthoc(opts$posthoc, posthoc_default)
  p_adjust <- if (!is.null(opts$`p-adjust`) && opts$`p-adjust` != "") opts$`p-adjust` else p_adjust_default
  conf_level <- if (!is.null(opts$`conf-level`)) as.numeric(opts$`conf-level`) else conf_default
  sphericity <- normalize_sphericity(opts$sphericity, sphericity_default)
  bootstrap <- resolve_parse_bool(opts$bootstrap, default = bootstrap_default)
  bootstrap_samples <- if (!is.null(opts$`bootstrap-samples`)) as.numeric(opts$`bootstrap-samples`) else bootstrap_samples_default
  if (is.na(bootstrap_samples) || bootstrap_samples <= 0) bootstrap_samples <- bootstrap_samples_default

  mode <- if (has_within && has_between) "mixed" else if (has_within) "within" else "between"
  posthoc_used <- if (mode == "between") posthoc else if (posthoc == "none") "none" else "pairwise"

  summary_df <- data.frame()
  posthoc_df <- data.frame()
  assumptions_df <- data.frame()
  used_type <- type
  data_between <- NULL
  data_within <- NULL

  if (mode == "between") {
    data_between <- prepare_between_data(df, dv, between_vars, covariates)
    fits <- build_between_model(data_between, dv, between_vars, covariates)
    summary_result <- extract_between_summary(fits$lm, type)
    summary_df <- summary_result$summary
    used_type <- summary_result$used_type

    if (posthoc_used != "none" && length(between_vars) > 0) {
      if (posthoc_used == "tukey") {
        posthoc_df <- build_between_posthoc_tukey(fits$aov)
      } else {
        posthoc_df <- build_between_posthoc_pairwise(data_between, dv, between_vars, p_adjust, conf_level)
      }
    }

    assumptions_df <- build_assumptions(
      mode,
      data_between = list(data = data_between, lm = fits$lm),
      data_within = NULL,
      dv = dv,
      between_vars = between_vars,
      within_vars = within_vars,
      covariates = covariates,
      subject_id = subject_id,
      alpha = alpha_default,
      max_shapiro_n = max_shapiro_n,
      sphericity = sphericity
    )
  } else {
    data_within <- prepare_within_data(df, within_vars, subject_id, between_vars, covariates)
    fits <- build_within_model(data_within$long, subject_id, data_within$within_name, between_vars, covariates)
    summary_result <- extract_within_summary(fits$aov, subject_id, data_within$within_name)
    summary_df <- summary_result$summary
    used_type <- "I"

    if (posthoc_used != "none") {
      posthoc_df <- build_within_posthoc_pairwise(data_within$wide, within_vars, between_vars, conf_level, p_adjust)
    }

    assumptions_df <- build_assumptions(
      mode,
      data_between = NULL,
      data_within = data_within,
      dv = "dv",
      between_vars = between_vars,
      within_vars = within_vars,
      covariates = covariates,
      subject_id = subject_id,
      alpha = alpha_default,
      max_shapiro_n = max_shapiro_n,
      sphericity = sphericity
    )
  }

  if (nrow(summary_df) == 0) stop("No ANOVA results could be computed.")

  effect_size_label <- format_effect_label(effect_size)
  summary_df$boot_ci_low <- NA_real_
  summary_df$boot_ci_high <- NA_real_
  if (isTRUE(bootstrap)) {
    term_ids <- build_term_ids(summary_df)
    if (mode == "between") {
      boot_vals <- bootstrap_effect_sizes_between(
        data_between,
        dv,
        between_vars,
        covariates,
        type,
        effect_size_label,
        bootstrap_samples,
        term_ids
      )
    } else {
      boot_vals <- bootstrap_effect_sizes_within(
        data_within,
        within_vars,
        between_vars,
        covariates,
        subject_id,
        effect_size_label,
        bootstrap_samples,
        term_ids
      )
    }
    summary_df <- apply_bootstrap_ci(summary_df, boot_vals, conf_level)
  }
  assumption_note <- summarize_assumptions(assumptions_df, alpha_default)
  note_tokens <- build_anova_note_tokens(
    used_type,
    effect_size_label,
    conf_level,
    posthoc_used,
    p_adjust,
    bootstrap,
    bootstrap_samples,
    assumption_note
  )

  apa_report_path <- file.path(out_dir, "report_canonical.md")
  apa_text <- format_apa_text(summary_df, digits, effect_size_label)
  apa_table <- format_apa_table(summary_df, digits, note_tokens$note_default, effect_size_label)
  template_override <- resolve_template_override(opts$template, module = "anova")
  template_path <- if (!is.null(template_override)) {
    template_override
  } else {
    resolve_get_template_path("anova.default", "anova/default-template.md")
  }
  template_meta <- resolve_get_template_meta(template_path)
  table_result <- build_anova_table_body(summary_df, digits, template_meta$table, effect_size_label)
  narrative_rows <- build_anova_narrative_rows(summary_df, digits, effect_size_label)
  template_context <- list(
    tokens = c(
      list(
        table_body = table_result$body,
        narrative_default = apa_text
      ),
      note_tokens
    ),
    narrative_rows = narrative_rows
  )

  analysis_flags <- list(
    mode = mode,
    dv = if (mode == "between") dv else NULL,
    between = if (has_between) between_vars else NULL,
    within = if (has_within) within_vars else NULL,
    "subject-id" = if (has_within) subject_id else NULL,
    covariates = if (length(covariates) > 0) covariates else NULL,
    type = used_type,
    "effect-size" = effect_size,
    posthoc = posthoc_used,
    "p-adjust" = p_adjust,
    "conf-level" = conf_level,
    sphericity = if (has_within) sphericity else NULL,
    bootstrap = bootstrap,
    "bootstrap-samples" = if (bootstrap) bootstrap_samples else NULL,
    digits = digits
  )

  resolve_append_apa_report(
    apa_report_path,
    "ANOVA",
    apa_table,
    apa_text,
    analysis_flags = analysis_flags,
    template_path = template_path,
    template_context = template_context
  )

  if (nrow(posthoc_df) > 0) {
    posthoc_note_tokens <- build_posthoc_note_tokens(posthoc_used, p_adjust)
    posthoc_apa_table <- format_posthoc_table(posthoc_df, digits, posthoc_note_tokens$note_default)
    posthoc_template_path <- if (!is.null(template_override)) {
      template_override
    } else {
      resolve_get_template_path("anova.posthoc", "anova/posthoc-template.md")
    }
    posthoc_meta <- resolve_get_template_meta(posthoc_template_path)
    posthoc_table <- build_posthoc_table_body(posthoc_df, digits, posthoc_meta$table)
    posthoc_narrative_rows <- build_posthoc_narrative_rows(posthoc_df, digits)
    posthoc_text <- paste(vapply(posthoc_narrative_rows, function(row) row$full_sentence, character(1)), collapse = "\n")
    posthoc_context <- list(
      tokens = c(
        list(
          table_body = posthoc_table$body,
          narrative_default = posthoc_text
        ),
        posthoc_note_tokens
      ),
      narrative_rows = posthoc_narrative_rows
    )
    resolve_append_apa_report(
      apa_report_path,
      "ANOVA post-hoc",
      posthoc_apa_table,
      posthoc_text,
      analysis_flags = analysis_flags,
      template_path = posthoc_template_path,
      template_context = posthoc_context
    )
  }

  cat("Wrote:\n")
  cat("- ", apa_report_path, "\n", sep = "")

  if (resolve_parse_bool(opts$log, default = log_default)) {
    ctx <- resolve_get_run_context()
    resolve_append_analysis_log(
      out_dir,
      module = "anova",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(summary_df = summary_df, posthoc_df = posthoc_df, assumptions_df = assumptions_df),
      options = list(
        mode = mode,
        dv = if (mode == "between") dv else NULL,
        between = if (has_between) between_vars else NULL,
        within = if (has_within) within_vars else NULL,
        subject_id = if (has_within) subject_id else NULL,
        covariates = if (length(covariates) > 0) covariates else NULL,
        type = used_type,
        effect_size = effect_size,
        posthoc = posthoc_used,
        p_adjust = p_adjust,
        conf_level = conf_level,
        sphericity = if (has_within) sphericity else NULL,
        bootstrap = bootstrap,
        bootstrap_samples = if (bootstrap) bootstrap_samples else NULL,
        digits = digits
      ),
      user_prompt = resolve_get_user_prompt(opts)
    )
  }
}

main()
