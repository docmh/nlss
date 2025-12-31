# SPDX-License-Identifier: Apache-2.0
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
source_lib("contrast_utils.R")


# Static analysis aliases for source_lib-defined functions.
render_output_path <- get("render_output_path", mode = "function")
add_term_label_column <- get("add_term_label_column", mode = "function")
add_value_label_column <- get("add_value_label_column", mode = "function")
build_contrast_method <- get("build_contrast_method", mode = "function")
format_contrast_label <- get("format_contrast_label", mode = "function")
resolve_contrast_spec <- get("resolve_contrast_spec", mode = "function")
resolve_label_metadata <- get("resolve_label_metadata", mode = "function")
source_lib <- get("source_lib", mode = "function")

print_usage <- function() {
  cat("Mixed Models (lme4)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript mixed_models.R --csv data.csv --formula \"score ~ time + (1|id)\"\n")
  cat("  Rscript mixed_models.R --csv data.csv --dv score --fixed time,group --random \"1|id,time|id\"\n")
  cat("  Rscript mixed_models.R --csv data.csv --formula \"score ~ time*group + (1|id)\" --emmeans time*group --contrasts pairwise\n")
  cat("  Rscript mixed_models.R --parquet data.parquet --formula \"score ~ time + (1|id)\"\n")
  cat("  Rscript mixed_models.R --interactive\n")
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
  cat("  --formula TEXT         Full lme4 formula (overrides --dv/--fixed/--random)\n")
  cat("  --dv NAME              Dependent variable (required without --formula)\n")
  cat("  --fixed LIST           Comma-separated fixed effects\n")
  cat("  --random LIST          Comma-separated random terms (e.g., 1|id,time|id)\n")
  cat("  --reml TRUE/FALSE      REML estimation (default: TRUE)\n")
  cat("  --type TYPE            Type I/II/III tests (default: III)\n")
  cat("  --df-method METHOD     satterthwaite/kenward-roger/none (default: satterthwaite)\n")
  cat("  --standardize TYPE     none/predictors (default: none)\n")
  cat("  --emmeans TERM         Term for marginal means (default: none)\n")
  cat("  --contrasts TYPE       none/pairwise/custom/<method> (default: none)\n")
  cat("  --contrast-file PATH   JSON contrast spec for custom or method args\n")
  cat("  --p-adjust METHOD      P-value adjustment (default: holm)\n")
  cat("  --conf-level VALUE     Confidence level (default: 0.95)\n")
  cat("  --optimizer NAME       Optimizer (default: bobyqa)\n")
  cat("  --maxfun N             Optimizer maxfun (default: 100000)\n")
  cat("  --diagnostics TRUE/FALSE Diagnostics (default: TRUE)\n")
  cat("  --max-shapiro-n N      Max n for Shapiro test (default: 5000)\n")
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

  use_formula <- resolve_prompt("Use full formula? (yes/no)", "yes")
  if (tolower(use_formula) %in% c("yes", "y")) {
    opts$formula <- resolve_prompt("Model formula (e.g., score ~ time + (1|id))", "")
  } else {
    opts$dv <- resolve_prompt("Dependent variable", "")
    opts$fixed <- resolve_prompt("Fixed effects (comma-separated)", "")
    opts$random <- resolve_prompt("Random terms (comma-separated; e.g., 1|id,time|id)", "")
  }

  reml_default <- resolve_config_value("modules.mixed_models.reml", TRUE)
  type_default <- resolve_config_value("modules.mixed_models.type", "III")
  df_method_default <- resolve_config_value("modules.mixed_models.df_method", "satterthwaite")
  standardize_default <- resolve_config_value("modules.mixed_models.standardize", "none")
  emmeans_default <- resolve_config_value("modules.mixed_models.emmeans", "none")
  contrasts_default <- resolve_config_value("modules.mixed_models.contrasts", "none")
  p_adjust_default <- resolve_config_value("modules.mixed_models.p_adjust", "holm")
  conf_default <- resolve_config_value("modules.mixed_models.conf_level", 0.95)
  optimizer_default <- resolve_config_value("modules.mixed_models.optimizer", "bobyqa")
  maxfun_default <- resolve_config_value("modules.mixed_models.maxfun", 100000)
  diagnostics_default <- resolve_config_value("modules.mixed_models.diagnostics", TRUE)
  max_shapiro_n_default <- resolve_config_value("modules.mixed_models.max_shapiro_n", 5000)
  digits_default <- resolve_config_value("defaults.digits", 2)

  opts$reml <- resolve_prompt("REML TRUE/FALSE", ifelse(isTRUE(reml_default), "TRUE", "FALSE"))
  opts$type <- resolve_prompt("Type (I/II/III)", type_default)
  opts$`df-method` <- resolve_prompt("DF method (satterthwaite/kenward-roger/none)", df_method_default)
  opts$standardize <- resolve_prompt("Standardize (none/predictors)", standardize_default)
  opts$emmeans <- resolve_prompt("Marginal means term (none or term)", emmeans_default)
  opts$contrasts <- resolve_prompt("Contrasts (none/pairwise/custom/<method>)", contrasts_default)
  contrast_mode <- normalize_contrasts(opts$contrasts, contrasts_default)
  if (contrast_mode == "custom") {
    opts$`contrast-file` <- resolve_prompt("Contrast JSON file", "")
  }
  opts$`p-adjust` <- resolve_prompt("P-value adjustment", p_adjust_default)
  opts$`conf-level` <- resolve_prompt("Confidence level", as.character(conf_default))
  opts$optimizer <- resolve_prompt("Optimizer", optimizer_default)
  opts$maxfun <- resolve_prompt("Optimizer maxfun", as.character(maxfun_default))
  opts$diagnostics <- resolve_prompt("Diagnostics TRUE/FALSE", ifelse(isTRUE(diagnostics_default), "TRUE", "FALSE"))
  opts$`max-shapiro-n` <- resolve_prompt("Max Shapiro n", as.character(max_shapiro_n_default))
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
      module = "mixed_models",
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

get_complete_rows <- function(df) {
  if (nrow(df) == 0) return(logical(0))
  idx <- complete.cases(df)
  if (any(idx)) {
    num_cols <- names(df)[sapply(df, is.numeric)]
    if (length(num_cols) > 0) {
      finite_idx <- rep(TRUE, nrow(df))
      for (col in num_cols) {
        finite_idx <- finite_idx & is.finite(df[[col]])
      }
      idx <- idx & finite_idx
    }
  }
  idx
}

normalize_reml <- function(value, default = TRUE) {
  resolve_parse_bool(value, default = default)
}

normalize_standardize <- function(value, default = "none") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (val %in% c("none", "no", "false")) return("none")
  if (val %in% c("predictors", "pred", "x")) return("predictors")
  default
}

normalize_type <- function(value, default = "III") {
  val <- if (!is.null(value) && value != "") value else default
  val <- toupper(gsub("[^A-Za-z0-9]", "", val))
  if (val %in% c("1", "I")) return("I")
  if (val %in% c("2", "II")) return("II")
  if (val %in% c("3", "III")) return("III")
  default
}

normalize_df_method <- function(value, default = "satterthwaite") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(gsub("[^a-z-]", "", val))
  if (val %in% c("satterthwaite", "satter")) return("satterthwaite")
  if (val %in% c("kenward-roger", "kenwardroger", "kr")) return("kenward-roger")
  if (val %in% c("none", "no", "false")) return("none")
  default
}

normalize_contrasts <- function(value, default = "none") {
  if (exists("normalize_contrast_mode", mode = "function")) {
    return(get("normalize_contrast_mode", mode = "function")(value, default))
  }
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (val %in% c("none", "no", "false")) return("none")
  if (val %in% c("pairwise", "pairs")) return("pairwise")
  if (val %in% c("custom", "json")) return("custom")
  val
}

normalize_emmeans <- function(value, default = "none") {
  val <- if (!is.null(value) && value != "") value else default
  val <- trimws(as.character(val))
  if (!nzchar(val) || tolower(val) %in% c("none", "no", "false")) return("")
  val
}

normalize_conf_level <- function(value, default = 0.95) {
  if (is.null(value) || value == "") return(default)
  val <- suppressWarnings(as.numeric(value))
  if (is.na(val) || val <= 0 || val >= 1) return(default)
  val
}

normalize_maxfun <- function(value, default = 100000) {
  if (is.null(value) || value == "") return(default)
  val <- suppressWarnings(as.numeric(value))
  if (is.na(val) || val <= 0) return(default)
  as.integer(val)
}

normalize_random_terms <- function(terms) {
  out <- character(0)
  for (term in terms) {
    term <- trimws(term)
    if (!nzchar(term)) next
    if (grepl("\\|", term)) {
      if (!grepl("^\\(.*\\)$", term)) {
        term <- paste0("(", term, ")")
      }
      out <- c(out, term)
    } else {
      out <- c(out, paste0("(1|", term, ")"))
    }
  }
  out
}

extract_random_terms_from_formula <- function(formula_text) {
  if (is.null(formula_text) || !nzchar(formula_text)) return(character(0))
  matches <- gregexpr("\\([^\\)]+\\|[^\\)]+\\)", formula_text, perl = TRUE)
  if (matches[[1]][1] == -1) return(character(0))
  terms <- regmatches(formula_text, matches)[[1]]
  trimws(terms)
}

build_model_formula <- function(dv, fixed_terms, random_terms) {
  fixed_term <- if (length(fixed_terms) > 0) paste(fixed_terms, collapse = " + ") else "1"
  random_term <- if (length(random_terms) > 0) paste(random_terms, collapse = " + ") else ""
  rhs <- if (nzchar(random_term)) paste(fixed_term, "+", random_term) else fixed_term
  as.formula(paste(dv, "~", rhs))
}

coerce_model_factors <- function(df, vars, dv) {
  for (var in vars) {
    if (!var %in% names(df)) next
    if (identical(var, dv)) next
    if (is.numeric(df[[var]])) next
    df[[var]] <- as.factor(df[[var]])
  }
  df
}

build_lmer_control <- function(optimizer, maxfun) {
  if (!requireNamespace("lme4", quietly = TRUE)) return(NULL)
  if (is.null(optimizer) || !nzchar(optimizer)) {
    if (is.null(maxfun) || is.na(maxfun)) return(lme4::lmerControl())
    return(lme4::lmerControl(optCtrl = list(maxfun = maxfun)))
  }
  if (is.null(maxfun) || is.na(maxfun)) {
    return(lme4::lmerControl(optimizer = optimizer))
  }
  lme4::lmerControl(optimizer = optimizer, optCtrl = list(maxfun = maxfun))
}

get_coef_column <- function(mat, names) {
  for (name in names) {
    if (name %in% colnames(mat)) return(mat[, name])
  }
  NULL
}

compute_standardized_betas <- function(data, dv, term_names, estimates, standardize) {
  if (standardize == "none") return(rep(NA_real_, length(term_names)))
  if (!dv %in% names(data)) return(rep(NA_real_, length(term_names)))
  y <- data[[dv]]
  if (!is.numeric(y)) return(rep(NA_real_, length(term_names)))
  sd_y <- sd(y)
  if (is.na(sd_y) || sd_y == 0) return(rep(NA_real_, length(term_names)))
  betas <- rep(NA_real_, length(term_names))
  for (i in seq_along(term_names)) {
    term <- term_names[i]
    if (term == "(Intercept)") next
    if (!term %in% names(data)) next
    if (!is.numeric(data[[term]])) next
    sd_x <- sd(data[[term]])
    if (is.na(sd_x) || sd_x == 0) next
    betas[i] <- estimates[i] * sd_x / sd_y
  }
  betas
}

extract_fixed_effects <- function(summary_obj, data, dv, conf_level, standardize) {
  coef_mat <- as.matrix(summary_obj$coefficients)
  if (is.null(coef_mat) || nrow(coef_mat) == 0) {
    return(data.frame())
  }
  term_names <- rownames(coef_mat)
  estimate <- coef_mat[, 1]
  se <- coef_mat[, 2]
  df_vals <- get_coef_column(coef_mat, c("df"))
  if (is.null(df_vals)) df_vals <- rep(NA_real_, length(estimate))
  t_vals <- get_coef_column(coef_mat, c("t value", "t", "t.value"))
  if (is.null(t_vals)) t_vals <- estimate / se
  p_vals <- get_coef_column(coef_mat, c("Pr(>|t|)", "Pr(>|z|)", "p.value", "p-value"))
  if (is.null(p_vals)) p_vals <- rep(NA_real_, length(estimate))

  ci_low <- rep(NA_real_, length(estimate))
  ci_high <- rep(NA_real_, length(estimate))
  for (i in seq_along(estimate)) {
    df_val <- df_vals[i]
    crit <- if (!is.na(df_val)) {
      qt(1 - (1 - conf_level) / 2, df_val)
    } else {
      qnorm(1 - (1 - conf_level) / 2)
    }
    ci_low[i] <- estimate[i] - crit * se[i]
    ci_high[i] <- estimate[i] + crit * se[i]
  }

  std_beta <- compute_standardized_betas(data, dv, term_names, estimate, standardize)

  data.frame(
    term = term_names,
    estimate = estimate,
    se = se,
    df = df_vals,
    t = t_vals,
    p = p_vals,
    ci_low = ci_low,
    ci_high = ci_high,
    std_beta = std_beta,
    stringsAsFactors = FALSE
  )
}

extract_random_effects <- function(fit) {
  vc <- lme4::VarCorr(fit)
  if (length(vc) == 0) return(data.frame())
  rows <- list()
  for (grp in names(vc)) {
    mat <- as.matrix(vc[[grp]])
    sd_vals <- attr(vc[[grp]], "stddev")
    terms <- rownames(mat)
    if (length(terms) == 0) next
    for (i in seq_along(terms)) {
      rows[[length(rows) + 1]] <- data.frame(
        group = grp,
        term = terms[i],
        variance = sd_vals[i]^2,
        stddev = sd_vals[i],
        corr = NA_real_,
        stringsAsFactors = FALSE
      )
    }
    corr <- attr(vc[[grp]], "correlation")
    if (!is.null(corr) && nrow(corr) > 1) {
      for (i in seq_len(nrow(corr) - 1)) {
        for (j in (i + 1):nrow(corr)) {
          rows[[length(rows) + 1]] <- data.frame(
            group = grp,
            term = paste0("corr(", terms[i], ",", terms[j], ")"),
            variance = NA_real_,
            stddev = NA_real_,
            corr = corr[i, j],
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, rows)
}

extract_fit_stats <- function(fit) {
  data.frame(
    n = tryCatch(nobs(fit), error = function(e) NA_real_),
    aic = suppressWarnings(AIC(fit)),
    bic = suppressWarnings(BIC(fit)),
    logLik = suppressWarnings(as.numeric(logLik(fit))),
    deviance = suppressWarnings(deviance(fit)),
    stringsAsFactors = FALSE
  )
}

extract_r2_df <- function(fit) {
  if (!requireNamespace("performance", quietly = TRUE)) return(data.frame())
  res <- tryCatch(performance::r2(fit), error = function(e) NULL)
  if (is.null(res)) return(data.frame())
  data.frame(
    r2_marginal = res$R2_marginal,
    r2_conditional = res$R2_conditional,
    stringsAsFactors = FALSE
  )
}

extract_icc_df <- function(fit) {
  if (!requireNamespace("performance", quietly = TRUE)) return(data.frame())
  res <- tryCatch(performance::icc(fit), error = function(e) NULL)
  if (is.null(res)) return(data.frame())
  icc_val <- resolve_icc_value(res)
  if (is.na(icc_val)) return(data.frame())
  data.frame(
    icc = icc_val,
    stringsAsFactors = FALSE
  )
}

resolve_icc_value <- function(res) {
  if (!(is.data.frame(res) || is.list(res))) return(NA_real_)
  keys <- names(res)
  if (is.null(keys) || length(keys) == 0) return(NA_real_)
  preferred <- c("ICC", "ICC_adjusted", "ICC_unadjusted", "ICC_conditional", "ICC_marginal")
  for (key in preferred) {
    if (key %in% keys) {
      val <- res[[key]]
      if (length(val) > 0) return(as.numeric(val[1]))
    }
  }
  for (key in keys) {
    if (grepl("^ICC", key, ignore.case = TRUE)) {
      val <- res[[key]]
      if (length(val) > 0) return(as.numeric(val[1]))
    }
  }
  for (key in keys) {
    val <- suppressWarnings(as.numeric(res[[key]]))
    if (length(val) > 0 && !all(is.na(val))) return(val[1])
  }
  NA_real_
}

build_anova_df <- function(fit, type, df_method, has_lmerTest) {
  out <- NULL
  used_type <- type
  fallback_used <- FALSE
  if (type == "I") {
    out <- tryCatch(stats::anova(fit), error = function(e) NULL)
  } else if (has_lmerTest && df_method != "none") {
    type_val <- ifelse(type == "III", 3, 2)
    ddf_label <- if (df_method == "kenward-roger") "Kenward-Roger" else "Satterthwaite"
    out <- tryCatch(stats::anova(fit, type = type_val, ddf = ddf_label), error = function(e) NULL)
  }
  if (is.null(out) && requireNamespace("car", quietly = TRUE)) {
    out <- tryCatch(car::Anova(fit, type = type), error = function(e) NULL)
  }
  if (is.null(out)) {
    out <- tryCatch(stats::anova(fit), error = function(e) NULL)
    if (!is.null(out) && type != "I") {
      fallback_used <- TRUE
      used_type <- "I"
    }
  }
  if (is.null(out)) return(data.frame())
  df <- as.data.frame(out)
  df$term <- rownames(df)
  rownames(df) <- NULL
  attr(df, "type_used") <- used_type
  attr(df, "fallback_used") <- fallback_used
  df
}

build_diagnostics <- function(fit, max_shapiro_n) {
  rows <- list()
  is_singular <- tryCatch(lme4::isSingular(fit), error = function(e) NA)
  rows[[length(rows) + 1]] <- data.frame(
    metric = "singular_fit",
    value = ifelse(is.na(is_singular), "", ifelse(isTRUE(is_singular), "TRUE", "FALSE")),
    statistic = NA_real_,
    p = NA_real_,
    note = "",
    stringsAsFactors = FALSE
  )

  conv_note <- ""
  conv_msgs <- tryCatch(fit@optinfo$conv$lme4$messages, error = function(e) NULL)
  if (!is.null(conv_msgs) && length(conv_msgs) > 0) {
    conv_note <- paste(conv_msgs, collapse = "; ")
  }
  rows[[length(rows) + 1]] <- data.frame(
    metric = "convergence",
    value = ifelse(nzchar(conv_note), "warning", "ok"),
    statistic = NA_real_,
    p = NA_real_,
    note = conv_note,
    stringsAsFactors = FALSE
  )

  resid_vals <- tryCatch(residuals(fit), error = function(e) NULL)
  if (!is.null(resid_vals)) {
    n <- length(resid_vals)
    if (n > 2 && n <= max_shapiro_n) {
      shap <- tryCatch(shapiro.test(resid_vals), error = function(e) NULL)
      if (!is.null(shap)) {
        rows[[length(rows) + 1]] <- data.frame(
          metric = "shapiro_wilk",
          value = "",
          statistic = unname(shap$statistic),
          p = shap$p.value,
          note = "",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  do.call(rbind, rows)
}

build_fixed_effects_table_body <- function(fixed_df, digits, table_meta) {
  display <- fixed_df
  display$term_display <- if ("term_label" %in% names(display)) display$term_label else display$term
  default_specs <- list(
    list(key = "model", label = "Model", drop_if_empty = TRUE),
    list(key = "term", label = "Effect"),
    list(key = "b", label = "b"),
    list(key = "se", label = "SE"),
    list(key = "df", label = "df", drop_if_empty = TRUE),
    list(key = "t", label = "t", drop_if_empty = TRUE),
    list(key = "p", label = "p", drop_if_empty = TRUE),
    list(key = "ci_low", label = "CI low", drop_if_empty = TRUE),
    list(key = "ci_high", label = "CI high", drop_if_empty = TRUE),
    list(key = "std_beta", label = "beta", drop_if_empty = TRUE)
  )
  columns <- resolve_normalize_table_columns(table_meta$columns, default_specs)
  show_model <- length(unique(display$model)) > 1
  rows <- list()
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    row_map <- list(
      model = if (show_model) row$model else "",
      term = row$term_display,
      b = format_stat(row$estimate, digits),
      se = format_stat(row$se, digits),
      df = format_stat(row$df, digits),
      t = format_stat(row$t, digits),
      p = format_p(row$p),
      ci_low = format_stat(row$ci_low, digits),
      ci_high = format_stat(row$ci_high, digits),
      std_beta = format_stat(row$std_beta, digits)
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

resolve_anova_column <- function(df, keys) {
  for (key in keys) {
    if (key %in% names(df)) return(key)
  }
  NULL
}

resolve_anova_columns <- function(df) {
  list(
    num_df = resolve_anova_column(df, c("NumDF", "numDF")),
    den_df = resolve_anova_column(df, c("DenDF", "denDF")),
    df = resolve_anova_column(df, c("Df", "df")),
    f = resolve_anova_column(df, c("F value", "F.value", "F", "F-value", "Fvalue")),
    chisq = resolve_anova_column(df, c("Chisq", "ChiSq", "Chisq value", "Chi.sq", "chisq")),
    p = resolve_anova_column(df, c("Pr(>F)", "Pr(>Chisq)", "Pr(>Chi)", "p.value", "p"))
  )
}

build_mixed_anova_table_body <- function(anova_df, digits, table_meta) {
  display <- anova_df
  if (!"term" %in% names(display)) return(list(body = "", columns = list()))
  display <- display[display$term != "(Intercept)", , drop = FALSE]
  if (nrow(display) == 0) return(list(body = "", columns = list()))

  cols <- resolve_anova_columns(display)
  default_specs <- list(
    list(key = "term", label = "Effect"),
    list(key = "num_df", label = "Num df", drop_if_empty = TRUE),
    list(key = "den_df", label = "Den df", drop_if_empty = TRUE),
    list(key = "df", label = "df", drop_if_empty = TRUE),
    list(key = "f", label = "F", drop_if_empty = TRUE),
    list(key = "chisq", label = "Chi-square", drop_if_empty = TRUE),
    list(key = "p", label = "Sig.", drop_if_empty = TRUE)
  )
  columns <- resolve_normalize_table_columns(table_meta$columns, default_specs)
  rows <- list()
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    num_df_val <- if (!is.null(cols$num_df)) row[[cols$num_df]] else NA_real_
    den_df_val <- if (!is.null(cols$den_df)) row[[cols$den_df]] else NA_real_
    df_val <- if (is.null(cols$num_df) && is.null(cols$den_df) && !is.null(cols$df)) row[[cols$df]] else NA_real_
    row_map <- list(
      term = row$term,
      num_df = format_num(num_df_val, digits),
      den_df = format_num(den_df_val, digits),
      df = format_num(df_val, digits),
      f = format_stat(if (!is.null(cols$f)) row[[cols$f]] else NA_real_, digits),
      chisq = format_stat(if (!is.null(cols$chisq)) row[[cols$chisq]] else NA_real_, digits),
      p = format_p(if (!is.null(cols$p)) row[[cols$p]] else NA_real_)
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

build_mixed_anova_narrative_rows <- function(anova_df, digits) {
  display <- anova_df
  if (!"term" %in% names(display)) return(list())
  display <- display[display$term != "(Intercept)", , drop = FALSE]
  if (nrow(display) == 0) return(list())

  cols <- resolve_anova_columns(display)
  rows <- list()
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    term <- row$term
    num_df_val <- if (!is.null(cols$num_df)) row[[cols$num_df]] else NA_real_
    den_df_val <- if (!is.null(cols$den_df)) row[[cols$den_df]] else NA_real_
    df_val <- if (!is.null(cols$df)) row[[cols$df]] else NA_real_
    f_val <- if (!is.null(cols$f)) row[[cols$f]] else NA_real_
    chisq_val <- if (!is.null(cols$chisq)) row[[cols$chisq]] else NA_real_
    p_val <- if (!is.null(cols$p)) row[[cols$p]] else NA_real_

    sentence <- ""
    if (!is.na(f_val)) {
      if (!is.na(num_df_val) && !is.na(den_df_val)) {
        sentence <- sprintf(
          "%s: F(%s, %s) = %s, p %s.",
          term,
          format_num(num_df_val, digits),
          format_num(den_df_val, digits),
          format_stat(f_val, digits),
          format_p(p_val)
        )
      } else {
        sentence <- sprintf(
          "%s: F = %s, p %s.",
          term,
          format_stat(f_val, digits),
          format_p(p_val)
        )
      }
    } else if (!is.na(chisq_val)) {
      df_text <- if (!is.na(df_val)) paste0("(", format_num(df_val, digits), ")") else ""
      sentence <- sprintf(
        "%s: Chi-square%s = %s, p %s.",
        term,
        df_text,
        format_stat(chisq_val, digits),
        format_p(p_val)
      )
    }
    rows[[length(rows) + 1]] <- list(
      full_sentence = sentence,
      term = term,
      num_df = format_num(num_df_val, digits),
      den_df = format_num(den_df_val, digits),
      df = format_num(df_val, digits),
      f = format_stat(f_val, digits),
      chisq = format_stat(chisq_val, digits),
      p = format_p(p_val)
    )
  }
  rows
}

build_mixed_models_anova_note_tokens <- function(type, df_method_used, fallback_used = FALSE) {
  notes <- character(0)
  type_label <- type
  if (fallback_used) {
    type_label <- "I (fallback)"
  }
  if (nzchar(type_label)) {
    notes <- c(notes, paste0("Type ", type_label, " tests of fixed effects."))
  }
  if (!is.null(df_method_used) && df_method_used != "none") {
    notes <- c(notes, paste0("df method: ", df_method_used, "."))
  }
  if (fallback_used) {
    notes <- c(notes, "Fallback used because Type II/III tests were unavailable.")
  }
  list(note_default = paste(notes, collapse = " "))
}

build_emmeans_table_body <- function(emmeans_df, digits, table_meta) {
  display <- emmeans_df
  display$term_display <- if ("term_label" %in% names(display)) display$term_label else display$term
  display$level_display <- if ("level_label" %in% names(display)) display$level_label else display$level
  default_specs <- list(
    list(key = "term", label = "Term"),
    list(key = "level", label = "Level", drop_if_empty = TRUE),
    list(key = "contrast", label = "Contrast", drop_if_empty = TRUE),
    list(key = "emmean", label = "EMM", drop_if_empty = TRUE),
    list(key = "estimate", label = "Estimate", drop_if_empty = TRUE),
    list(key = "se", label = "SE"),
    list(key = "df", label = "df", drop_if_empty = TRUE),
    list(key = "t", label = "t", drop_if_empty = TRUE),
    list(key = "p", label = "p", drop_if_empty = TRUE),
    list(key = "p_adj", label = "p_adj", drop_if_empty = TRUE),
    list(key = "ci_low", label = "CI low", drop_if_empty = TRUE),
    list(key = "ci_high", label = "CI high", drop_if_empty = TRUE),
    list(key = "method", label = "Method", drop_if_empty = TRUE)
  )
  columns <- resolve_normalize_table_columns(table_meta$columns, default_specs)
  rows <- list()
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    row_map <- list(
      term = row$term_display,
      level = row$level_display,
      contrast = row$contrast,
      emmean = format_stat(row$emmean, digits),
      estimate = format_stat(row$estimate, digits),
      se = format_stat(row$se, digits),
      df = format_stat(row$df, digits),
      t = format_stat(row$t, digits),
      p = format_p(row$p),
      p_adj = format_p(row$p_adj),
      ci_low = format_stat(row$ci_low, digits),
      ci_high = format_stat(row$ci_high, digits),
      method = row$method
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

build_fixed_effects_narrative_rows <- function(fixed_df, digits) {
  display <- fixed_df
  display$term_display <- if ("term_label" %in% names(display)) display$term_label else display$term
  rows <- list()
  if (nrow(display) == 0) return(rows)
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    if (row$term == "(Intercept)") next
    term_label <- row$term_display
    b_text <- format_stat(row$estimate, digits)
    se_text <- format_stat(row$se, digits)
    t_text <- format_stat(row$t, digits)
    df_text <- format_stat(row$df, digits)
    p_text <- format_p(row$p)
    ci_text <- format_ci(row$ci_low, row$ci_high, digits)
    sentence <- sprintf("%s: b = %s, SE = %s", term_label, b_text, se_text)
    if (nzchar(t_text)) {
      if (nzchar(df_text)) {
        sentence <- paste0(sentence, ", t(", df_text, ") = ", t_text)
      } else {
        sentence <- paste0(sentence, ", t = ", t_text)
      }
    }
    if (nzchar(p_text)) {
      sentence <- paste0(sentence, ", p ", p_text)
    }
    if (nzchar(ci_text)) {
      sentence <- paste0(sentence, ", CI ", ci_text)
    }
    sentence <- paste0(sentence, ".")
    rows[[length(rows) + 1]] <- list(
      full_sentence = sentence,
      term = term_label,
      b = b_text,
      se = se_text,
      df = df_text,
      t = t_text,
      p = p_text,
      ci = ci_text,
      ci_low = format_stat(row$ci_low, digits),
      ci_high = format_stat(row$ci_high, digits),
      std_beta = format_stat(row$std_beta, digits)
    )
  }
  rows
}

build_emmeans_narrative_rows <- function(emmeans_df, digits) {
  display <- emmeans_df
  display$term_display <- if ("term_label" %in% names(display)) display$term_label else display$term
  display$level_display <- if ("level_label" %in% names(display)) display$level_label else display$level
  rows <- list()
  if (nrow(display) == 0) return(rows)
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    label <- if (nzchar(row$contrast)) row$contrast else row$level_display
    if (!nzchar(label)) label <- row$term_display
    estimate <- if (!is.na(row$emmean)) row$emmean else row$estimate
    est_text <- format_stat(estimate, digits)
    se_text <- format_stat(row$se, digits)
    t_text <- format_stat(row$t, digits)
    df_text <- format_stat(row$df, digits)
    p_text <- if (!is.na(row$p_adj)) format_p(row$p_adj) else format_p(row$p)
    ci_text <- format_ci(row$ci_low, row$ci_high, digits)
    sentence <- sprintf("%s: estimate = %s, SE = %s", label, est_text, se_text)
    if (nzchar(t_text)) {
      if (nzchar(df_text)) {
        sentence <- paste0(sentence, ", t(", df_text, ") = ", t_text)
      } else {
        sentence <- paste0(sentence, ", t = ", t_text)
      }
    }
    if (nzchar(p_text)) {
      sentence <- paste0(sentence, ", p ", p_text)
    }
    if (nzchar(ci_text)) {
      sentence <- paste0(sentence, ", CI ", ci_text)
    }
    sentence <- paste0(sentence, ".")
    rows[[length(rows) + 1]] <- list(
      full_sentence = sentence,
      term = row$term,
      level = row$level,
      contrast = row$contrast,
      emmean = format_stat(row$emmean, digits),
      estimate = format_stat(row$estimate, digits),
      se = se_text,
      df = df_text,
      t = t_text,
      p = format_p(row$p),
      p_adj = format_p(row$p_adj),
      ci = ci_text,
      ci_low = format_stat(row$ci_low, digits),
      ci_high = format_stat(row$ci_high, digits)
    )
  }
  rows
}

build_mixed_models_note_tokens <- function(random_terms, conf_level, standardize, fit_df, r2_df, icc_df, diagnostics_df, df_method_used, optimizer, reml, emmeans_note) {
  notes <- character(0)
  notes <- c(notes, "Fixed effects are unstandardized (b) with standard errors.")
  if (standardize != "none") {
    notes <- c(notes, "Standardized coefficients (beta) are reported for numeric predictors.")
  }
  notes <- c(notes, sprintf("CI uses %s%% confidence.", round(conf_level * 100)))
  if (df_method_used != "none") {
    notes <- c(notes, paste0("df method: ", df_method_used, "."))
  }
  if (nzchar(emmeans_note)) {
    notes <- c(notes, emmeans_note)
  }
  random_note <- ""
  if (length(random_terms) > 0) {
    random_note <- paste0("Random effects: ", paste(random_terms, collapse = ", "), ".")
    notes <- c(notes, random_note)
  }
  fit_note <- ""
  if (!is.null(fit_df) && nrow(fit_df) > 0) {
    row <- fit_df[1, ]
    fit_note <- paste0(
      "Model fit: AIC = ", format_stat(row$aic, 2),
      ", BIC = ", format_stat(row$bic, 2),
      ", logLik = ", format_stat(row$logLik, 2),
      "."
    )
    notes <- c(notes, fit_note)
  }
  r2_m <- ""
  r2_c <- ""
  if (!is.null(r2_df) && nrow(r2_df) > 0) {
    r2_m <- format_stat(r2_df$r2_marginal[1], 2)
    r2_c <- format_stat(r2_df$r2_conditional[1], 2)
    if (nzchar(r2_m) && nzchar(r2_c)) {
      notes <- c(notes, paste0("R2m = ", r2_m, ", R2c = ", r2_c, "."))
    }
  }
  icc_val <- ""
  if (!is.null(icc_df) && nrow(icc_df) > 0) {
    icc_val <- format_stat(icc_df$icc[1], 2)
    if (nzchar(icc_val)) {
      notes <- c(notes, paste0("ICC = ", icc_val, "."))
    }
  }
  convergence_note <- ""
  if (!is.null(diagnostics_df) && nrow(diagnostics_df) > 0) {
    conv_row <- diagnostics_df[diagnostics_df$metric == "convergence", , drop = FALSE]
    if (nrow(conv_row) > 0 && nzchar(conv_row$note[1])) {
      convergence_note <- paste0("Convergence warning: ", conv_row$note[1], ".")
      notes <- c(notes, convergence_note)
    }
    sing_row <- diagnostics_df[diagnostics_df$metric == "singular_fit", , drop = FALSE]
    if (nrow(sing_row) > 0 && sing_row$value[1] == "TRUE") {
      convergence_note <- paste0(convergence_note, ifelse(nzchar(convergence_note), " ", ""), "Singular fit detected.")
      notes <- c(notes, "Singular fit detected.")
    }
  }

  list(
    note_default = paste(notes, collapse = " "),
    random_effects_note = random_note,
    fit_note = fit_note,
    icc = icc_val,
    r2_marginal = r2_m,
    r2_conditional = r2_c,
    convergence_note = convergence_note,
    optimizer = optimizer,
    reml = ifelse(isTRUE(reml), "REML", "ML")
  )
}

build_emmeans_note_tokens <- function(conf_level, contrast_label, p_adjust, contrast_file = "") {
  notes <- character(0)
  notes <- c(notes, sprintf("CI uses %s%% confidence.", round(conf_level * 100)))
  if (contrast_label != "none") {
    note <- paste0("Contrasts: ", contrast_label, ".")
    if (nzchar(contrast_file)) {
      note <- paste0(note, " File: ", basename(contrast_file), ".")
    }
    note <- paste0(note, " P-value adjustment: ", p_adjust, ".")
    notes <- c(notes, note)
  }
  list(note_default = paste(notes, collapse = " "))
}

summarize_emmeans <- function(emm, conf_level) {
  summary(emm, infer = c(TRUE, TRUE), level = conf_level)
}

build_emmeans_rows <- function(emm_summary, term_label) {
  base_cols <- c("emmean", "SE", "df", "lower.CL", "upper.CL", "t.ratio", "p.value")
  factor_cols <- setdiff(names(emm_summary), base_cols)
  level <- ""
  if (length(factor_cols) > 0) {
    level <- apply(emm_summary[, factor_cols, drop = FALSE], 1, function(row) {
      paste(paste0(factor_cols, "=", row), collapse = ", ")
    })
  }
  data.frame(
    term = term_label,
    level = level,
    contrast = "",
    emmean = emm_summary$emmean,
    estimate = NA_real_,
    se = emm_summary$SE,
    df = emm_summary$df,
    t = emm_summary$t.ratio,
    p = emm_summary$p.value,
    p_adj = NA_real_,
    ci_low = emm_summary$lower.CL,
    ci_high = emm_summary$upper.CL,
    method = "emmeans",
    stringsAsFactors = FALSE
  )
}

build_contrasts_rows <- function(contrast_summary, term_label, p_adjust, method_label) {
  p_adj_vals <- ifelse(p_adjust != "none", contrast_summary$p.value, NA_real_)
  p_vals <- ifelse(p_adjust == "none", contrast_summary$p.value, NA_real_)
  method <- if (!is.null(method_label) && nzchar(method_label)) method_label else p_adjust
  data.frame(
    term = term_label,
    level = "",
    contrast = contrast_summary$contrast,
    emmean = NA_real_,
    estimate = contrast_summary$estimate,
    se = contrast_summary$SE,
    df = contrast_summary$df,
    t = contrast_summary$t.ratio,
    p = p_vals,
    p_adj = p_adj_vals,
    ci_low = contrast_summary$lower.CL,
    ci_high = contrast_summary$upper.CL,
    method = method,
    stringsAsFactors = FALSE
  )
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
  reml_default <- resolve_config_value("modules.mixed_models.reml", TRUE)
  type_default <- resolve_config_value("modules.mixed_models.type", "III")
  df_method_default <- resolve_config_value("modules.mixed_models.df_method", "satterthwaite")
  standardize_default <- resolve_config_value("modules.mixed_models.standardize", "none")
  emmeans_default <- resolve_config_value("modules.mixed_models.emmeans", "none")
  contrasts_default <- resolve_config_value("modules.mixed_models.contrasts", "none")
  p_adjust_default <- resolve_config_value("modules.mixed_models.p_adjust", "holm")
  conf_default <- resolve_config_value("modules.mixed_models.conf_level", 0.95)
  optimizer_default <- resolve_config_value("modules.mixed_models.optimizer", "bobyqa")
  maxfun_default <- resolve_config_value("modules.mixed_models.maxfun", 100000)
  diagnostics_default <- resolve_config_value("modules.mixed_models.diagnostics", TRUE)
  max_shapiro_n_default <- resolve_config_value("modules.mixed_models.max_shapiro_n", 5000)

  digits <- if (!is.null(opts$digits)) as.numeric(opts$digits) else digits_default
  df <- resolve_load_dataframe(opts)
  out_dir <- resolve_get_workspace_out_dir(df)

  if (!requireNamespace("lme4", quietly = TRUE)) {
    emit_input_issue(out_dir, opts, "Mixed models require the 'lme4' package.", details = list(package = "lme4"), status = "missing_dependency")
  }
  if (!requireNamespace("performance", quietly = TRUE)) {
    emit_input_issue(out_dir, opts, "Mixed models require the 'performance' package.", details = list(package = "performance"), status = "missing_dependency")
  }

  reml <- normalize_reml(opts$reml, reml_default)
  type <- normalize_type(opts$type, type_default)
  df_method <- normalize_df_method(opts$`df-method`, df_method_default)
  standardize <- normalize_standardize(opts$standardize, standardize_default)
  emmeans_term <- normalize_emmeans(opts$emmeans, emmeans_default)
  has_emmeans <- requireNamespace("emmeans", quietly = TRUE)
  emmeans_note <- ""
  contrast_file <- if (!is.null(opts$`contrast-file`)) as.character(opts$`contrast-file`) else ""
  contrasts_input <- normalize_contrasts(opts$contrasts, contrasts_default)
  contrast_spec <- tryCatch(resolve_contrast_spec(contrasts_input, contrast_file), error = function(e) e)
  if (inherits(contrast_spec, "error")) {
    emit_input_issue(out_dir, opts, contrast_spec$message, details = list(contrasts = contrasts_input, contrast_file = contrast_file))
  }
  contrast_label <- if (!is.null(contrast_spec$label)) contrast_spec$label else "none"
  if (!is.null(contrast_spec$term) && nzchar(contrast_spec$term)) {
    if (nzchar(emmeans_term) && emmeans_term != contrast_spec$term) {
      emit_input_issue(
        out_dir,
        opts,
        "Contrast term does not match --emmeans.",
        details = list(emmeans = emmeans_term, contrast_term = contrast_spec$term)
      )
    }
    if (!nzchar(emmeans_term)) {
      emmeans_term <- contrast_spec$term
    }
  }
  contrasts_active <- !is.null(contrast_spec) && contrast_spec$mode != "none"
  if (!nzchar(emmeans_term)) {
    contrasts_active <- FALSE
    contrast_label <- "none"
  }
  if (nzchar(emmeans_term) && !has_emmeans) {
    emmeans_note <- "emmeans requested but the 'emmeans' package is not installed."
  }
  p_adjust <- if (!is.null(opts$`p-adjust`) && nzchar(opts$`p-adjust`)) as.character(opts$`p-adjust`) else p_adjust_default
  conf_level <- normalize_conf_level(opts$`conf-level`, conf_default)
  optimizer <- if (!is.null(opts$optimizer) && nzchar(opts$optimizer)) as.character(opts$optimizer) else optimizer_default
  maxfun <- normalize_maxfun(opts$maxfun, maxfun_default)
  diagnostics <- resolve_parse_bool(opts$diagnostics, default = diagnostics_default)
  max_shapiro_n <- if (!is.null(opts$`max-shapiro-n`)) as.numeric(opts$`max-shapiro-n`) else max_shapiro_n_default

  formula_text <- if (!is.null(opts$formula) && nzchar(opts$formula)) as.character(opts$formula) else ""
  dv <- if (!is.null(opts$dv) && nzchar(opts$dv)) as.character(opts$dv) else ""
  fixed_terms <- resolve_parse_list(opts$fixed)
  random_terms_raw <- resolve_parse_list(opts$random)
  random_terms <- normalize_random_terms(random_terms_raw)

  model_formula <- NULL
  if (nzchar(formula_text)) {
    model_formula <- tryCatch(as.formula(formula_text), error = function(e) {
      emit_input_issue(out_dir, opts, paste0("Invalid formula: ", e$message), details = list(formula = formula_text))
    })
    dv <- as.character(model_formula[[2]])
    if (length(random_terms) == 0) {
      random_terms <- extract_random_terms_from_formula(formula_text)
    }
  } else {
    if (!nzchar(dv)) {
      emit_input_issue(out_dir, opts, "Mixed models require --formula or --dv.", details = list(dv = opts$dv))
    }
    if (length(random_terms) == 0) {
      emit_input_issue(out_dir, opts, "Mixed models require --random or random effects in --formula.", details = list(random = opts$random))
    }
    model_formula <- build_model_formula(dv, fixed_terms, random_terms)
  }

  if (length(random_terms) == 0) {
    emit_input_issue(out_dir, opts, "Mixed models require at least one random effect term.", details = list(formula = formula_text))
  }

  model_vars <- all.vars(model_formula)
  missing_vars <- setdiff(model_vars, names(df))
  if (length(missing_vars) > 0) {
    emit_input_issue(out_dir, opts, paste0("Missing variables: ", paste(missing_vars, collapse = ", ")), details = list(missing = missing_vars))
  }

  if (!dv %in% names(df)) {
    emit_input_issue(out_dir, opts, sprintf("Dependent variable '%s' not found.", dv), details = list(dv = dv))
  }
  if (!is.numeric(df[[dv]])) {
    emit_input_issue(out_dir, opts, "Dependent variable must be numeric for LMM.", details = list(dv = dv))
  }

  df <- coerce_model_factors(df, model_vars, dv)
  complete_idx <- get_complete_rows(df[, model_vars, drop = FALSE])
  data_model <- df[complete_idx, , drop = FALSE]
  if (nrow(data_model) == 0) {
    emit_input_issue(out_dir, opts, "No complete cases available after listwise deletion.")
  }
  data_model <- droplevels(data_model)

  has_lmerTest <- requireNamespace("lmerTest", quietly = TRUE)
  df_method_used <- df_method
  if (df_method != "none" && !has_lmerTest) {
    df_method_used <- "none"
  }

  control <- build_lmer_control(optimizer, maxfun)
  fit <- tryCatch({
    if (df_method_used != "none" && has_lmerTest) {
      lmerTest::lmer(model_formula, data = data_model, REML = reml, control = control)
    } else {
      lme4::lmer(model_formula, data = data_model, REML = reml, control = control)
    }
  }, error = function(e) e)

  if (inherits(fit, "error")) {
    emit_input_issue(out_dir, opts, paste0("Model fit failed: ", fit$message), status = "fit_failed")
  }

  summary_obj <- if (df_method_used != "none" && has_lmerTest) {
    ddf_label <- if (df_method_used == "kenward-roger") "Kenward-Roger" else "Satterthwaite"
    summary(fit, ddf = ddf_label)
  } else {
    summary(fit)
  }

  fixed_df <- extract_fixed_effects(summary_obj, data_model, dv, conf_level, standardize)
  if (nrow(fixed_df) > 0) {
    fixed_df$model <- "Model 1"
  }
  label_meta <- resolve_label_metadata(data_model)
  fixed_df <- add_term_label_column(fixed_df, label_meta, term_col = "term")
  random_df <- extract_random_effects(fit)
  fit_df <- extract_fit_stats(fit)
  r2_df <- extract_r2_df(fit)
  icc_df <- extract_icc_df(fit)
  diagnostics_df <- if (diagnostics) build_diagnostics(fit, max_shapiro_n) else data.frame()
  anova_df <- build_anova_df(fit, type, df_method_used, has_lmerTest)
  anova_type_used <- attr(anova_df, "type_used")
  if (is.null(anova_type_used) || !nzchar(anova_type_used)) {
    anova_type_used <- type
  }
  anova_fallback_used <- isTRUE(attr(anova_df, "fallback_used"))

  analysis_flags <- list(
    formula = if (nzchar(formula_text)) formula_text else NULL,
    dv = if (!nzchar(formula_text)) dv else NULL,
    fixed = if (!nzchar(formula_text) && length(fixed_terms) > 0) fixed_terms else NULL,
    random = if (length(random_terms) > 0) random_terms else NULL,
    reml = reml,
    "df-method" = if (df_method_used != "none") df_method_used else NULL,
    type = type,
    standardize = if (standardize != "none") standardize else NULL,
    emmeans = if (nzchar(emmeans_term)) emmeans_term else NULL,
    contrasts = if (contrasts_active) contrast_label else NULL,
    "contrast-file" = if (nzchar(contrast_file)) basename(contrast_file) else NULL,
    "p-adjust" = if (contrasts_active) p_adjust else NULL,
    "conf-level" = conf_level,
    optimizer = optimizer,
    maxfun = maxfun,
    diagnostics = diagnostics,
    digits = digits
  )

  note_tokens <- build_mixed_models_note_tokens(
    random_terms,
    conf_level,
    standardize,
    fit_df,
    r2_df,
    icc_df,
    diagnostics_df,
    df_method_used,
    optimizer,
    reml,
    emmeans_note
  )
  narrative_rows <- build_fixed_effects_narrative_rows(fixed_df, digits)
  nlss_text <- ""
  if (length(narrative_rows) > 0) {
    nlss_text <- paste(vapply(narrative_rows, function(row) row$full_sentence, character(1)), collapse = "\n")
  }

  template_override <- resolve_template_override(opts$template, module = "mixed_models")
  nlss_report_path <- file.path(out_dir, "report_canonical.md")

  anova_template_path <- if (!is.null(template_override)) {
    template_override
  } else {
    resolve_get_template_path("mixed_models.tests", "mixed-models/tests-of-fixed-effects-template.md")
  }
  anova_meta <- resolve_get_template_meta(anova_template_path)
  anova_table <- build_mixed_anova_table_body(anova_df, digits, anova_meta$table)
  if (nzchar(anova_table$body)) {
    anova_note_tokens <- build_mixed_models_anova_note_tokens(anova_type_used, df_method_used, anova_fallback_used)
    anova_narrative_rows <- build_mixed_anova_narrative_rows(anova_df, digits)
    anova_text <- ""
    if (length(anova_narrative_rows) > 0) {
      anova_text <- paste(vapply(anova_narrative_rows, function(row) row$full_sentence, character(1)), collapse = "\n")
    }
    anova_nlss_table <- paste0("Table 1\n\n", anova_table$body, "\n", anova_note_tokens$note_default)
    anova_context <- list(
      tokens = c(
        list(
          table_body = anova_table$body,
          narrative_default = anova_text
        ),
        anova_note_tokens
      ),
      narrative_rows = anova_narrative_rows
    )
    resolve_append_nlss_report(
      nlss_report_path,
      "Mixed Models: Tests of Fixed Effects",
      anova_nlss_table,
      anova_text,
      analysis_flags = analysis_flags,
      template_path = anova_template_path,
      template_context = anova_context
    )
  }

  coef_template_path <- if (!is.null(template_override)) {
    template_override
  } else {
    resolve_get_template_path("mixed_models.default", "mixed-models/default-template.md")
  }
  coef_template_meta <- resolve_get_template_meta(coef_template_path)
  table_result <- build_fixed_effects_table_body(fixed_df, digits, coef_template_meta$table)
  nlss_table <- paste0("Table 1\n\n", table_result$body, "\n", note_tokens$note_default)

  template_context <- list(
    tokens = c(
      list(
        table_body = table_result$body,
        narrative_default = nlss_text
      ),
      note_tokens
    ),
    narrative_rows = narrative_rows
  )

  resolve_append_nlss_report(
    nlss_report_path,
    "Mixed Models: Estimates of Fixed Effects",
    nlss_table,
    nlss_text,
    analysis_flags = analysis_flags,
    template_path = coef_template_path,
    template_context = template_context
  )

  emmeans_df <- data.frame()
  contrasts_df <- data.frame()
  emmeans_rows <- data.frame()

  if (nzchar(emmeans_term)) {
    if (has_emmeans) {
      specs <- as.formula(paste("~", emmeans_term))
      emm <- tryCatch(emmeans::emmeans(fit, specs = specs), error = function(e) NULL)
      if (!is.null(emm)) {
        emm_summary <- summarize_emmeans(emm, conf_level)
        emmeans_df <- build_emmeans_rows(emm_summary, emmeans_term)
        if (contrasts_active) {
          contrast_method <- tryCatch(build_contrast_method(contrast_spec, emm, emmeans_term), error = function(e) e)
          if (inherits(contrast_method, "error")) {
            emit_input_issue(
              out_dir,
              opts,
              contrast_method$message,
              details = list(contrasts = contrast_label, contrast_file = contrast_file)
            )
          }
          cont <- tryCatch(do.call(emmeans::contrast, c(list(emm, method = contrast_method$method), contrast_method$args)), error = function(e) NULL)
          if (!is.null(cont)) {
            cont_summary <- summary(cont, infer = c(TRUE, TRUE), adjust = p_adjust, level = conf_level)
            contrasts_df <- build_contrasts_rows(cont_summary, emmeans_term, p_adjust, contrast_label)
          }
        }
      }
    }
  }

  emmeans_df <- add_term_label_column(emmeans_df, label_meta, term_col = "term")
  emmeans_df <- add_value_label_column(emmeans_df, label_meta, var_col = "term", value_col = "level")
  contrasts_df <- add_term_label_column(contrasts_df, label_meta, term_col = "term")
  contrasts_df <- add_value_label_column(contrasts_df, label_meta, var_col = "term", value_col = "level")

  if (nrow(emmeans_df) > 0 && nrow(contrasts_df) > 0) {
    emmeans_rows <- rbind(emmeans_df, contrasts_df)
  } else if (nrow(emmeans_df) > 0) {
    emmeans_rows <- emmeans_df
  } else if (nrow(contrasts_df) > 0) {
    emmeans_rows <- contrasts_df
  } else {
    emmeans_rows <- data.frame()
  }
  if (nrow(emmeans_rows) > 0) {
    emmeans_template_path <- if (!is.null(template_override)) {
      template_override
    } else {
      resolve_get_template_path("mixed_models.emmeans", "mixed-models/emmeans-template.md")
    }
    emmeans_meta <- resolve_get_template_meta(emmeans_template_path)
    emmeans_table <- build_emmeans_table_body(emmeans_rows, digits, emmeans_meta$table)
    emmeans_note_tokens <- build_emmeans_note_tokens(conf_level, format_contrast_label(contrast_spec), p_adjust, contrast_file)
    emmeans_narrative_rows <- build_emmeans_narrative_rows(emmeans_rows, digits)
    emmeans_text <- ""
    if (length(emmeans_narrative_rows) > 0) {
      emmeans_text <- paste(vapply(emmeans_narrative_rows, function(row) row$full_sentence, character(1)), collapse = "\n")
    }
    emmeans_nlss_table <- paste0("Table 1\n\n", emmeans_table$body, "\n", emmeans_note_tokens$note_default)
    emmeans_context <- list(
      tokens = c(
        list(
          table_body = emmeans_table$body,
          narrative_default = emmeans_text
        ),
        emmeans_note_tokens
      ),
      narrative_rows = emmeans_narrative_rows
    )
    resolve_append_nlss_report(
      nlss_report_path,
      "Mixed Models emmeans",
      emmeans_nlss_table,
      emmeans_text,
      analysis_flags = analysis_flags,
      template_path = emmeans_template_path,
      template_context = emmeans_context
    )
  }

  cat("Wrote:\n")
  cat("- ", render_output_path(nlss_report_path, out_dir), "\n", sep = "")

  if (resolve_parse_bool(opts$log, default = log_default)) {
    ctx <- resolve_get_run_context()
    resolve_append_analysis_log(
      out_dir,
      module = "mixed_models",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(
        fixed_effects_df = fixed_df,
        random_effects_df = random_df,
        fit_df = fit_df,
        r2_df = r2_df,
        icc_df = icc_df,
        anova_df = anova_df,
        emmeans_df = emmeans_df,
        contrasts_df = contrasts_df,
        diagnostics_df = diagnostics_df
      ),
      options = analysis_flags,
      user_prompt = resolve_get_user_prompt(opts)
    )
  }
}

main()
