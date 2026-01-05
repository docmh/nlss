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


# Static analysis aliases for source_lib-defined functions.
render_output_path <- get("render_output_path", mode = "function")
source_lib <- get("source_lib", mode = "function")

print_usage <- function() {
  cat("Structural equation modeling (lavaan)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript sem.R --csv data.csv --analysis sem --model \"<lavaan syntax>\"\n")
  cat("  Rscript sem.R --parquet data.parquet --analysis cfa --factors \"F1=item1,item2;F2=item3,item4\"\n")
  cat("  Rscript sem.R --parquet data.parquet --analysis mediation --x x1 --m m1,m2 --y y1\n")
  cat("  Rscript sem.R --parquet data.parquet --analysis path --dv outcome --ivs x1,x2\n")
  cat("  Rscript sem.R --interactive\n")
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
  cat("  --analysis TYPE         sem/cfa/path/mediation/invariance\n")
  cat("  --model TEXT            lavaan model syntax (quotes recommended)\n")
  cat("  --model-file PATH       lavaan model syntax file\n")
  cat("  --paths TEXT            alias for --model (path syntax)\n")
  cat("  --factors TEXT          CFA builder: F1=item1,item2;F2=item3,item4\n")
  cat("  --dv NAME               Path builder dependent variable\n")
  cat("  --ivs LIST              Path builder predictors\n")
  cat("  --x NAME                Mediation predictor\n")
  cat("  --m LIST                Mediation mediators\n")
  cat("  --y NAME                Mediation outcome\n")
  cat("  --covariates LIST       Optional covariates\n")
  cat("  --serial TRUE/FALSE     Serial mediation (supports two mediators)\n")
  cat("  --group NAME            Multi-group analysis variable\n")
  cat("  --group-equal LIST      lavaan group.equal constraints\n")
  cat("  --invariance LIST       Invariance steps (configural,metric,scalar,strict)\n")
  cat("  --ordered LIST          Ordered categorical variables\n")
  cat("  --estimator NAME        ML, MLR, MLM, MLMV, MLMVS, WLSMV, ULSMV, DWLS\n")
  cat("  --missing TYPE          fiml/listwise/pairwise\n")
  cat("  --se TYPE               standard/robust/bootstrap\n")
  cat("  --ci TYPE               standard/bootstrap/bca\n")
  cat("  --conf-level VALUE      Confidence level (default: 0.95)\n")
  cat("  --bootstrap TRUE/FALSE  Bootstrap standard errors\n")
  cat("  --bootstrap-samples N   Bootstrap resamples (default: 5000)\n")
  cat("  --std TYPE              none/std.lv/std.all\n")
  cat("  --fit LIST              Fit indices to report\n")
  cat("  --r2 TRUE/FALSE         Report R² (default: TRUE)\n")
  cat("  --modindices N          Modification index cutoff (0 to skip)\n")
  cat("  --residuals TRUE/FALSE  Store standardized residuals in log\n")
  cat("  --digits N              Rounding digits (default: 2)\n")
  cat("  --template REF          Template path or template key (optional)\n")
  cat("  --user-prompt TEXT      Original AI user prompt for logging (optional)\n")
  cat("  --log TRUE/FALSE        Write analysis_log.jsonl (default: TRUE)\n")
  cat("  --interactive           Prompt for inputs\n")
  cat("  --help                  Show this help\n")
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

  analysis_default <- resolve_config_value("modules.sem.analysis", "sem")
  opts$analysis <- resolve_prompt("Analysis (sem/cfa/path/mediation/invariance)", analysis_default)

  if (tolower(opts$analysis) == "cfa") {
    opts$factors <- resolve_prompt("Factors (F1=item1,item2;F2=item3,item4)", "")
    opts$model <- resolve_prompt("Model syntax (blank to use factors)", "")
  } else if (tolower(opts$analysis) == "mediation") {
    opts$x <- resolve_prompt("Predictor (x)")
    opts$m <- resolve_prompt("Mediators (comma-separated)")
    opts$y <- resolve_prompt("Outcome (y)")
    opts$covariates <- resolve_prompt("Covariates (comma-separated, optional)", "")
    opts$serial <- resolve_prompt("Serial mediation TRUE/FALSE", "FALSE")
  } else if (tolower(opts$analysis) == "path") {
    opts$dv <- resolve_prompt("Dependent variable", "")
    opts$ivs <- resolve_prompt("Predictors (comma-separated)", "")
    opts$model <- resolve_prompt("Model syntax (blank to use dv/ivs)", "")
  } else {
    opts$model <- resolve_prompt("Model syntax", "")
  }

  estimator_default <- resolve_config_value("modules.sem.estimator", "MLR")
  missing_default <- resolve_config_value("modules.sem.missing", "fiml")
  se_default <- resolve_config_value("modules.sem.se", "robust")
  ci_default <- resolve_config_value("modules.sem.ci", "standard")
  conf_default <- resolve_config_value("modules.sem.conf_level", 0.95)
  bootstrap_default <- resolve_config_value("modules.sem.bootstrap", FALSE)
  bootstrap_samples_default <- resolve_config_value("modules.sem.bootstrap_samples", 5000)
  std_default <- resolve_config_value("modules.sem.std", "std.all")
  fit_default <- resolve_config_value("modules.sem.fit", "chisq,df,cfi,tli,rmsea,srmr")
  r2_default <- resolve_config_value("modules.sem.r2", TRUE)
  modindices_default <- resolve_config_value("modules.sem.modindices", 0)
  residuals_default <- resolve_config_value("modules.sem.residuals", FALSE)
  digits_default <- resolve_config_value("defaults.digits", 2)

  opts$estimator <- resolve_prompt("Estimator", estimator_default)
  opts$missing <- resolve_prompt("Missing handling", missing_default)
  opts$se <- resolve_prompt("SE type", se_default)
  opts$ci <- resolve_prompt("CI type", ci_default)
  opts$`conf-level` <- resolve_prompt("Confidence level", as.character(conf_default))
  opts$bootstrap <- resolve_prompt("Bootstrap TRUE/FALSE", ifelse(isTRUE(bootstrap_default), "TRUE", "FALSE"))
  opts$`bootstrap-samples` <- resolve_prompt("Bootstrap samples", as.character(bootstrap_samples_default))
  opts$std <- resolve_prompt("Standardization (none/std.lv/std.all)", std_default)
  opts$fit <- resolve_prompt("Fit indices", fit_default)
  opts$r2 <- resolve_prompt("Report R² TRUE/FALSE", ifelse(isTRUE(r2_default), "TRUE", "FALSE"))
  opts$modindices <- resolve_prompt("Modindices cutoff", as.character(modindices_default))
  opts$residuals <- resolve_prompt("Include residuals TRUE/FALSE", ifelse(isTRUE(residuals_default), "TRUE", "FALSE"))
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
  resolve_default_out()
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

resolve_get_run_context <- function() {
  if (exists("get_run_context", mode = "function")) {
    return(get("get_run_context", mode = "function")())
  }
  list(prompt = "", commands = character(0))
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

normalize_analysis <- function(value, default = "sem") {
  val <- if (!is.null(value) && nzchar(value)) tolower(as.character(value)) else tolower(default)
  if (val %in% c("sem", "structural")) return("sem")
  if (val %in% c("cfa", "confirmatory")) return("cfa")
  if (val %in% c("path", "path-analysis", "path_analysis")) return("path")
  if (val %in% c("mediation", "med", "indirect")) return("mediation")
  if (val %in% c("invariance", "measurement-invariance", "mi")) return("invariance")
  default
}

normalize_estimator <- function(value, default = "MLR") {
  val <- if (!is.null(value) && nzchar(value)) as.character(value) else as.character(default)
  val <- toupper(val)
  allowed <- c("ML", "MLR", "MLM", "MLMV", "MLMVS", "WLSMV", "ULSMV", "DWLS", "ULS", "GLS")
  if (val %in% allowed) return(val)
  toupper(default)
}

normalize_missing <- function(value, default = "fiml") {
  val <- if (!is.null(value) && nzchar(value)) tolower(as.character(value)) else tolower(default)
  if (val %in% c("fiml", "ml")) return("fiml")
  if (val %in% c("listwise", "list")) return("listwise")
  if (val %in% c("pairwise", "pair")) return("pairwise")
  val
}

normalize_se <- function(value, default = "robust") {
  val <- if (!is.null(value) && nzchar(value)) tolower(as.character(value)) else tolower(default)
  if (val %in% c("standard", "none", "default")) return("standard")
  if (val %in% c("robust", "sandwich")) return("robust")
  if (val %in% c("bootstrap", "boot")) return("bootstrap")
  "standard"
}

normalize_ci <- function(value, default = "standard") {
  val <- if (!is.null(value) && nzchar(value)) tolower(as.character(value)) else tolower(default)
  if (val %in% c("standard", "normal", "none")) return("standard")
  if (val %in% c("bootstrap", "boot", "perc", "percentile")) return("bootstrap")
  if (val %in% c("bca", "bca.simple", "bca_simple")) return("bca")
  "standard"
}

normalize_std <- function(value, default = "std.all") {
  val <- if (!is.null(value) && nzchar(value)) tolower(as.character(value)) else tolower(default)
  if (val %in% c("none", "no", "false")) return("none")
  if (val %in% c("std.lv", "std_lv", "latent")) return("std.lv")
  if (val %in% c("std.all", "std_all", "all")) return("std.all")
  default
}

normalize_model_syntax <- function(text) {
  if (is.null(text)) return("")
  if (length(text) > 1) text <- paste(text, collapse = "\n")
  text <- as.character(text)
  if (!nzchar(text)) return("")
  out <- gsub("\r\n?", "\n", text)
  out <- gsub(";", "\n", out)
  trimws(out)
}

parse_factor_spec <- function(text) {
  if (is.null(text) || !nzchar(text)) return(list())
  parts <- strsplit(text, ";", fixed = TRUE)[[1]]
  factors <- list()
  for (chunk in parts) {
    chunk <- trimws(chunk)
    if (!nzchar(chunk)) next
    pair <- strsplit(chunk, "=", fixed = TRUE)[[1]]
    if (length(pair) != 2) {
      stop("Invalid factor specification: ", chunk)
    }
    name <- trimws(pair[1])
    items <- trimws(strsplit(pair[2], ",", fixed = TRUE)[[1]])
    items <- items[nzchar(items)]
    if (!nzchar(name) || length(items) == 0) {
      stop("Invalid factor specification: ", chunk)
    }
    factors[[name]] <- items
  }
  factors
}

build_cfa_model <- function(factors) {
  if (length(factors) == 0) return("")
  lines <- character(0)
  for (name in names(factors)) {
    items <- factors[[name]]
    if (length(items) == 0) next
    lines <- c(lines, paste0(name, " =~ ", paste(items, collapse = " + ")))
  }
  paste(lines, collapse = "\n")
}

build_path_model <- function(dv, ivs, covariates) {
  if (!nzchar(dv) || length(ivs) == 0) return("")
  rhs <- paste(ivs, collapse = " + ")
  if (length(covariates) > 0) rhs <- paste(rhs, paste(covariates, collapse = " + "), sep = " + ")
  paste0(dv, " ~ ", rhs)
}

build_mediation_model <- function(x, mediators, y, covariates, serial = FALSE) {
  if (!nzchar(x) || !nzchar(y) || length(mediators) == 0) return("")
  cov_text <- ""
  if (length(covariates) > 0) cov_text <- paste0(" + ", paste(covariates, collapse = " + "))

  lines <- character(0)
  if (isTRUE(serial)) {
    if (length(mediators) != 2) {
      stop("Serial mediation currently supports exactly two mediators.")
    }
    m1 <- mediators[1]
    m2 <- mediators[2]
    lines <- c(lines, paste0(m1, " ~ a1*", x, cov_text))
    lines <- c(lines, paste0(m2, " ~ a2*", x, " + d21*", m1, cov_text))
    lines <- c(lines, paste0(y, " ~ c_prime*", x, " + b1*", m1, " + b2*", m2, cov_text))
    lines <- c(lines, paste0("indirect_", m1, " := a1*b1"))
    lines <- c(lines, paste0("indirect_", m2, " := a2*b2"))
    lines <- c(lines, "indirect_serial := a1*d21*b2")
    lines <- c(lines, paste0("total_indirect := indirect_", m1, " + indirect_", m2, " + indirect_serial"))
    lines <- c(lines, "total := c_prime + total_indirect")
  } else {
    b_terms <- character(0)
    indirect_terms <- character(0)
    for (i in seq_along(mediators)) {
      m <- mediators[i]
      lines <- c(lines, paste0(m, " ~ a", i, "*", x, cov_text))
      b_terms <- c(b_terms, paste0("b", i, "*", m))
      indirect_terms <- c(indirect_terms, paste0("indirect_", m, " := a", i, "*b", i))
    }
    rhs <- paste(c(paste0("c_prime*", x), b_terms), collapse = " + ")
    if (length(covariates) > 0) rhs <- paste(rhs, paste(covariates, collapse = " + "), sep = " + ")
    lines <- c(lines, paste0(y, " ~ ", rhs))
    lines <- c(lines, indirect_terms)
    if (length(indirect_terms) > 0) {
      total_indirect <- paste(sub(" :=.*$", "", indirect_terms), collapse = " + ")
      lines <- c(lines, paste0("total_indirect := ", total_indirect))
      lines <- c(lines, "total := c_prime + total_indirect")
    }
  }
  paste(lines, collapse = "\n")
}

extract_model_vars <- function(model_syntax) {
  if (is.null(model_syntax)) return(character(0))
  if (length(model_syntax) > 1) model_syntax <- paste(model_syntax, collapse = "\n")
  if (!nzchar(model_syntax)) return(character(0))

  vars <- character(0)
  latent_vars <- character(0)
  if (requireNamespace("lavaan", quietly = TRUE)) {
    table <- tryCatch(lavaan::lavaanify(model_syntax, auto = FALSE), error = function(e) NULL)
    if (is.null(table) || nrow(table) == 0) {
      table <- tryCatch(lavaan::lavaanify(model_syntax, auto = TRUE), error = function(e) NULL)
    }
    if (!is.null(table) && nrow(table) > 0) {
      relevant_ops <- table$op %in% c("=~", "~", "~~")
      latent_vars <- unique(table$lhs[table$op == "=~"])
      vars <- unique(c(table$lhs[relevant_ops], table$rhs[relevant_ops]))
      vars <- setdiff(vars, latent_vars)
      vars <- vars[nzchar(vars)]
      vars <- setdiff(vars, "1")
    }
  }
  if (length(vars) > 0) return(vars)

  text <- gsub("\r\n?", "\n", model_syntax)
  lines <- unlist(strsplit(text, "\n", fixed = TRUE))
  lines <- trimws(lines)
  lines <- sub("#.*$", "", lines)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]

  extract_terms <- function(expr) {
    expr <- trimws(expr)
    if (!nzchar(expr)) return(character(0))
    expr <- gsub("-", "+-", expr, fixed = TRUE)
    parts <- unlist(strsplit(expr, "+", fixed = TRUE))
    parts <- trimws(parts)
    parts <- parts[nzchar(parts)]
    out <- character(0)
    for (part in parts) {
      part <- trimws(part)
      if (!nzchar(part)) next
      part <- sub("^[^\\*]+\\*", "", part)
      part <- gsub("[()]", "", part)
      if (!nzchar(part)) next
      tokens <- regmatches(part, gregexpr("[A-Za-z\\.][A-Za-z0-9_\\.]*", part, perl = TRUE))[[1]]
      if (length(tokens) > 0) out <- c(out, tokens)
    }
    out
  }

  for (line in lines) {
    if (!nzchar(line)) next
    if (grepl(":=", line, fixed = TRUE)) next
    op <- NULL
    if (grepl("=~", line, fixed = TRUE)) {
      op <- "=~"
    } else if (grepl("~~", line, fixed = TRUE)) {
      op <- "~~"
    } else if (grepl("~", line, fixed = TRUE)) {
      op <- "~"
    }
    if (is.null(op)) next
    parts <- strsplit(line, op, fixed = TRUE)[[1]]
    if (length(parts) < 2) next
    lhs <- trimws(parts[1])
    rhs <- trimws(paste(parts[-1], collapse = op))
    if (op == "=~") {
      latent_vars <- c(latent_vars, extract_terms(lhs))
      vars <- c(vars, extract_terms(rhs))
    } else {
      vars <- c(vars, extract_terms(lhs), extract_terms(rhs))
    }
  }

  vars <- unique(vars)
  vars <- vars[nzchar(vars)]
  vars <- setdiff(vars, c("1", latent_vars))
  vars
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

build_fit_sentence <- function(fit_values, digits) {
  if (length(fit_values) == 0) return("")
  parts <- character(0)
  if (!is.null(fit_values$chisq) && !is.null(fit_values$df)) {
    chisq <- format_stat(fit_values$chisq, digits)
    df <- format_stat(fit_values$df, 0)
    p_val <- if (!is.null(fit_values$pvalue)) format_p(fit_values$pvalue) else ""
    if (nzchar(p_val)) {
      parts <- c(parts, paste0("chi²(", df, ") = ", chisq, ", p ", p_val))
    } else {
      parts <- c(parts, paste0("chi²(", df, ") = ", chisq))
    }
  }
  metrics <- c("cfi", "tli", "rmsea", "srmr", "aic", "bic")
  labels <- c(cfi = "CFI", tli = "TLI", rmsea = "RMSEA", srmr = "SRMR", aic = "AIC", bic = "BIC")
  for (metric in metrics) {
    if (!is.null(fit_values[[metric]])) {
      parts <- c(parts, paste0(labels[[metric]], " = ", format_stat(fit_values[[metric]], digits)))
    }
  }
  if (length(parts) == 0) return("")
  paste0("Model fit: ", paste(parts, collapse = ", "), ".")
}

flatten_r2_values <- function(r2_values) {
  if (is.null(r2_values) || length(r2_values) == 0) return(list())
  if (is.numeric(r2_values)) {
    out <- as.list(r2_values)
    if (is.null(names(out))) names(out) <- rep("", length(out))
    return(out)
  }
  if (is.list(r2_values)) {
    out <- list()
    group_names <- names(r2_values)
    for (i in seq_along(r2_values)) {
      values <- r2_values[[i]]
      if (is.null(values)) next
      group_label <- if (!is.null(group_names) && nzchar(group_names[i])) group_names[i] else paste0("Group ", i)
      if (!is.numeric(values)) next
      val_names <- names(values)
      if (is.null(val_names) || all(!nzchar(val_names))) {
        for (j in seq_along(values)) {
          label <- paste0(group_label, ": var", j)
          out[[label]] <- values[[j]]
        }
      } else {
        for (j in seq_along(values)) {
          label <- paste0(group_label, ": ", val_names[j])
          out[[label]] <- values[[j]]
        }
      }
    }
    return(out)
  }
  list()
}

build_r2_sentence <- function(r2_values, digits) {
  flat <- flatten_r2_values(r2_values)
  if (length(flat) == 0) return("")
  labels <- names(flat)
  parts <- character(0)
  for (i in seq_along(flat)) {
    label <- labels[i]
    value <- flat[[i]]
    if (is.na(value)) next
    if (is.null(label) || !nzchar(label)) {
      parts <- c(parts, format_stat(value, digits))
    } else {
      parts <- c(parts, paste0(label, " = ", format_stat(value, digits)))
    }
  }
  if (length(parts) == 0) return("")
  paste0("R²: ", paste(parts, collapse = "; "), ".")
}

build_r2_df <- function(r2_values) {
  flat <- flatten_r2_values(r2_values)
  if (length(flat) == 0) return(data.frame())
  labels <- names(flat)
  if (is.null(labels)) labels <- rep("", length(flat))
  data.frame(
    label = labels,
    r2 = as.numeric(flat),
    stringsAsFactors = FALSE
  )
}

build_sem_table_body <- function(param_df, digits, table_meta) {
  default_specs <- list(
    list(key = "group", label = "Group", drop_if_empty = TRUE),
    list(key = "path", label = "Path"),
    list(key = "label", label = "Label", drop_if_empty = TRUE),
    list(key = "est", label = "b"),
    list(key = "se", label = "SE"),
    list(key = "z", label = "z", drop_if_empty = TRUE),
    list(key = "p", label = "p"),
    list(key = "ci_low", label = "CI low", drop_if_empty = TRUE),
    list(key = "ci_high", label = "CI high", drop_if_empty = TRUE),
    list(key = "std", label = "Std", drop_if_empty = TRUE)
  )
  columns <- resolve_normalize_table_columns(table_meta$columns, default_specs)
  show_group <- any(nzchar(param_df$group), na.rm = TRUE)

  rows <- list()
  for (i in seq_len(nrow(param_df))) {
    row <- param_df[i, ]
    row_map <- list(
      group = if (show_group) row$group else "",
      path = row$path,
      label = row$label,
      est = format_stat(row$est, digits),
      se = format_stat(row$se, digits),
      z = format_stat(row$z, digits),
      p = format_p(row$p),
      ci_low = format_stat(row$ci_low, digits),
      ci_high = format_stat(row$ci_high, digits),
      std = format_stat(row$std, digits)
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

build_invariance_table_body <- function(summary_df, digits, table_meta) {
  default_specs <- list(
    list(key = "step", label = "Step"),
    list(key = "group_equal", label = "Constraints", drop_if_empty = TRUE),
    list(key = "chisq", label = "Chi²"),
    list(key = "df", label = "df"),
    list(key = "p", label = "p"),
    list(key = "cfi", label = "CFI"),
    list(key = "tli", label = "TLI"),
    list(key = "rmsea", label = "RMSEA"),
    list(key = "srmr", label = "SRMR"),
    list(key = "delta_cfi", label = "Delta CFI", drop_if_empty = TRUE),
    list(key = "delta_rmsea", label = "Delta RMSEA", drop_if_empty = TRUE)
  )
  columns <- resolve_normalize_table_columns(table_meta$columns, default_specs)
  rows <- list()
  for (i in seq_len(nrow(summary_df))) {
    row <- summary_df[i, ]
    row_map <- list(
      step = row$step,
      group_equal = row$group_equal,
      chisq = format_stat(row$chisq, digits),
      df = format_stat(row$df, 0),
      p = format_p(row$p),
      cfi = format_stat(row$cfi, digits),
      tli = format_stat(row$tli, digits),
      rmsea = format_stat(row$rmsea, digits),
      srmr = format_stat(row$srmr, digits),
      delta_cfi = format_stat(row$delta_cfi, digits),
      delta_rmsea = format_stat(row$delta_rmsea, digits)
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

build_sem_note_tokens <- function(estimator, missing, se, ci, conf_level, std, n_obs, group_labels) {
  notes <- character(0)
  notes <- c(notes, paste0("Estimator = ", estimator, "."))
  if (nzchar(missing)) notes <- c(notes, paste0("Missing = ", toupper(missing), "."))
  if (nzchar(se)) notes <- c(notes, paste0("SE = ", se, "."))
  if (nzchar(ci)) notes <- c(notes, paste0("CI = ", ci, ", ", round(conf_level * 100), "%."))
  if (nzchar(std) && std != "none") notes <- c(notes, paste0("Standardization = ", std, "."))
  if (!is.null(n_obs) && !all(is.na(n_obs))) {
    n_text <- if (length(n_obs) > 1) paste(n_obs, collapse = ", ") else as.character(n_obs)
    if (nzchar(n_text)) notes <- c(notes, paste0("N = ", n_text, "."))
  }
  if (!is.null(group_labels) && length(group_labels) > 0) {
    notes <- c(notes, paste0("Groups: ", paste(group_labels, collapse = ", "), "."))
  }
  list(note_default = paste(notes, collapse = " "))
}

emit_input_issue <- function(out_dir, opts, message, details = list(), status = "invalid_input") {
  log_default <- resolve_config_value("defaults.log", TRUE)
  if (resolve_parse_bool(opts$log, default = log_default)) {
    ctx <- resolve_get_run_context()
    resolve_append_analysis_log(
      out_dir,
      module = "sem",
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

fit_sem_model <- function(analysis, model_syntax, df, estimator, missing, se, bootstrap_samples, ordered_vars, group_var, group_equal) {
  args <- list(
    model = model_syntax,
    data = df,
    estimator = estimator,
    missing = missing,
    se = se
  )
  if (length(ordered_vars) > 0) args$ordered <- ordered_vars
  if (nzchar(group_var)) args$group <- group_var
  if (!is.null(group_equal) && length(group_equal) > 0) args$group.equal <- group_equal
  if (se == "bootstrap") args$bootstrap <- bootstrap_samples

  if (analysis == "cfa") {
    do.call(lavaan::cfa, args)
  } else {
    do.call(lavaan::sem, args)
  }
}

collect_fit_values <- function(fit, fit_indices) {
  values <- list()
  if (length(fit_indices) == 0) return(values)
  fit_vals <- tryCatch(lavaan::fitMeasures(fit, fit_indices), error = function(e) NULL)
  if (is.null(fit_vals)) return(values)
  if (is.numeric(fit_vals)) {
    values <- as.list(fit_vals)
  } else if (is.list(fit_vals)) {
    values <- fit_vals
  }
  values
}

build_param_df <- function(fit, std, conf_level, ci_type, group_labels) {
  boot_ci_type <- NULL
  if (ci_type == "bootstrap") boot_ci_type <- "perc"
  if (ci_type == "bca") boot_ci_type <- "bca.simple"
  pe <- lavaan::parameterEstimates(
    fit,
    standardized = (std != "none"),
    ci = TRUE,
    level = conf_level,
    boot.ci.type = boot_ci_type
  )

  keep_ops <- c("=~", "~", "~~", ":=")
  pe <- pe[pe$op %in% keep_ops, , drop = FALSE]
  if (nrow(pe) == 0) return(data.frame())
  pe <- pe[!(pe$op == "~~" & pe$lhs == pe$rhs), , drop = FALSE]

  std_col <- NULL
  if (std == "std.all" && "std.all" %in% names(pe)) std_col <- "std.all"
  if (std == "std.lv" && "std.lv" %in% names(pe)) std_col <- "std.lv"

  group_vals <- rep("", nrow(pe))
  if (!is.null(pe$group) && length(group_labels) > 0) {
    group_vals <- vapply(pe$group, function(idx) {
      if (is.na(idx)) return("")
      label <- group_labels[as.integer(idx)]
      if (is.null(label) || !nzchar(label)) return("")
      as.character(label)
    }, character(1))
  }

  label_vals <- if ("label" %in% names(pe)) {
    ifelse(is.na(pe$label), "", pe$label)
  } else {
    rep("", nrow(pe))
  }

  data.frame(
    group = group_vals,
    path = paste(pe$lhs, pe$op, pe$rhs),
    label = label_vals,
    est = pe$est,
    se = pe$se,
    z = pe$z,
    p = pe$pvalue,
    ci_low = pe$ci.lower,
    ci_high = pe$ci.upper,
    std = if (!is.null(std_col)) pe[[std_col]] else NA_real_,
    op = pe$op,
    stringsAsFactors = FALSE
  )
}

build_invariance_summary <- function(steps, fits, fit_indices) {
  rows <- list()
  prev <- NULL
  get_val <- function(values, key) {
    val <- values[[key]]
    if (is.null(val) || length(val) == 0) return(NA_real_)
    as.numeric(val)
  }
  for (i in seq_along(steps)) {
    fit <- fits[[i]]
    fit_vals <- collect_fit_values(fit, fit_indices)
    row <- list(
      step = steps[[i]]$label,
      group_equal = steps[[i]]$constraints,
      chisq = get_val(fit_vals, "chisq"),
      df = get_val(fit_vals, "df"),
      p = get_val(fit_vals, "pvalue"),
      cfi = get_val(fit_vals, "cfi"),
      tli = get_val(fit_vals, "tli"),
      rmsea = get_val(fit_vals, "rmsea"),
      srmr = get_val(fit_vals, "srmr"),
      delta_cfi = NA_real_,
      delta_rmsea = NA_real_
    )
    if (!is.null(prev)) {
      if (!is.null(row$cfi) && !is.null(prev$cfi)) row$delta_cfi <- row$cfi - prev$cfi
      if (!is.null(row$rmsea) && !is.null(prev$rmsea)) row$delta_rmsea <- row$rmsea - prev$rmsea
    }
    prev <- row
    rows[[length(rows) + 1]] <- row
  }
  if (length(rows) == 0) return(data.frame())
  data.frame(
    step = vapply(rows, function(x) x$step, character(1)),
    group_equal = vapply(rows, function(x) x$group_equal, character(1)),
    chisq = vapply(rows, function(x) as.numeric(x$chisq), numeric(1)),
    df = vapply(rows, function(x) as.numeric(x$df), numeric(1)),
    p = vapply(rows, function(x) as.numeric(x$p), numeric(1)),
    cfi = vapply(rows, function(x) as.numeric(x$cfi), numeric(1)),
    tli = vapply(rows, function(x) as.numeric(x$tli), numeric(1)),
    rmsea = vapply(rows, function(x) as.numeric(x$rmsea), numeric(1)),
    srmr = vapply(rows, function(x) as.numeric(x$srmr), numeric(1)),
    delta_cfi = vapply(rows, function(x) as.numeric(x$delta_cfi), numeric(1)),
    delta_rmsea = vapply(rows, function(x) as.numeric(x$delta_rmsea), numeric(1)),
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
  analysis_default <- resolve_config_value("modules.sem.analysis", "sem")
  estimator_default <- resolve_config_value("modules.sem.estimator", "MLR")
  missing_default <- resolve_config_value("modules.sem.missing", "fiml")
  se_default <- resolve_config_value("modules.sem.se", "robust")
  ci_default <- resolve_config_value("modules.sem.ci", "standard")
  conf_default <- resolve_config_value("modules.sem.conf_level", 0.95)
  bootstrap_default <- resolve_config_value("modules.sem.bootstrap", FALSE)
  bootstrap_samples_default <- resolve_config_value("modules.sem.bootstrap_samples", 5000)
  std_default <- resolve_config_value("modules.sem.std", "std.all")
  fit_default <- resolve_config_value("modules.sem.fit", "chisq,df,cfi,tli,rmsea,srmr")
  r2_default <- resolve_config_value("modules.sem.r2", TRUE)
  modindices_default <- resolve_config_value("modules.sem.modindices", 0)
  residuals_default <- resolve_config_value("modules.sem.residuals", FALSE)
  invariance_default <- resolve_config_value("modules.sem.invariance", "configural,metric,scalar,strict")

  digits <- if (!is.null(opts$digits)) as.numeric(opts$digits) else digits_default
  analysis <- normalize_analysis(opts$analysis, analysis_default)
  estimator <- normalize_estimator(opts$estimator, estimator_default)
  missing <- normalize_missing(opts$missing, missing_default)
  bootstrap <- resolve_parse_bool(opts$bootstrap, default = bootstrap_default)
  se <- normalize_se(opts$se, se_default)
  if (bootstrap && se != "bootstrap") se <- "bootstrap"
  if (se == "bootstrap" && !bootstrap) bootstrap <- TRUE
  ci_type <- normalize_ci(opts$ci, ci_default)
  conf_level <- if (!is.null(opts$`conf-level`)) as.numeric(opts$`conf-level`) else conf_default
  if (is.na(conf_level) || conf_level <= 0 || conf_level >= 1) conf_level <- conf_default
  bootstrap_samples <- if (!is.null(opts$`bootstrap-samples`)) as.numeric(opts$`bootstrap-samples`) else bootstrap_samples_default
  if (is.na(bootstrap_samples) || bootstrap_samples <= 0) bootstrap_samples <- bootstrap_samples_default
  std <- normalize_std(opts$std, std_default)
  fit_indices <- resolve_parse_list(if (!is.null(opts$fit)) opts$fit else fit_default)
  if (length(fit_indices) > 0 && "chisq" %in% fit_indices) {
    if (!("df" %in% fit_indices)) fit_indices <- c(fit_indices, "df")
    if (!("pvalue" %in% fit_indices)) fit_indices <- c(fit_indices, "pvalue")
  }
  if (analysis == "invariance") {
    needed <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr")
    fit_indices <- unique(c(fit_indices, needed))
  }
  r2_flag <- resolve_parse_bool(opts$r2, default = r2_default)
  modindices_cutoff <- if (!is.null(opts$modindices)) as.numeric(opts$modindices) else modindices_default
  if (is.na(modindices_cutoff) || modindices_cutoff < 0) modindices_cutoff <- 0
  residuals_flag <- resolve_parse_bool(opts$residuals, default = residuals_default)

  df <- resolve_load_dataframe(opts)
  out_dir <- resolve_get_workspace_out_dir(df)
  template_override <- resolve_template_override(opts$template, module = "sem")

  if (!requireNamespace("lavaan", quietly = TRUE)) {
    emit_input_issue(out_dir, opts, "SEM requires the 'lavaan' package.", details = list(package = "lavaan"), status = "missing_dependency")
  }

  ordered_vars <- resolve_parse_list(opts$ordered)
  ordered_vars <- ordered_vars[nzchar(ordered_vars)]
  if (length(ordered_vars) > 0) {
    missing_ordered <- setdiff(ordered_vars, names(df))
    if (length(missing_ordered) > 0) {
      emit_input_issue(out_dir, opts, paste0("Unknown ordered variables: ", paste(missing_ordered, collapse = ", ")))
    }
    for (var in ordered_vars) {
      df[[var]] <- as.ordered(df[[var]])
    }
  }

  group_var <- ""
  if (!is.null(opts$group) && nzchar(opts$group)) {
    group_var <- as.character(opts$group)
    if (!group_var %in% names(df)) {
      emit_input_issue(out_dir, opts, paste0("Grouping variable not found: ", group_var))
    }
    df[[group_var]] <- as.factor(df[[group_var]])
  }

  group_equal <- resolve_parse_list(opts$`group-equal`)

  model_text <- ""
  if (!is.null(opts[["model-file"]]) && nzchar(opts[["model-file"]])) {
    model_path <- as.character(opts[["model-file"]])
    if (!file.exists(model_path)) {
      emit_input_issue(out_dir, opts, paste0("Model file not found: ", model_path))
    }
    model_text <- paste(readLines(model_path, warn = FALSE), collapse = "\n")
  }
  if (!nzchar(model_text) && !is.null(opts[["model"]]) && nzchar(opts[["model"]])) {
    model_text <- as.character(opts[["model"]])
  }
  if (!nzchar(model_text) && !is.null(opts[["paths"]]) && nzchar(opts[["paths"]])) {
    model_text <- as.character(opts[["paths"]])
  }

  factors_text <- if (!is.null(opts$factors)) as.character(opts$factors) else ""
  dv <- if (!is.null(opts$dv)) as.character(opts$dv) else ""
  ivs <- resolve_parse_list(opts$ivs)
  ivs <- ivs[nzchar(ivs)]
  covariates <- resolve_parse_list(opts$covariates)
  covariates <- covariates[nzchar(covariates)]
  x <- if (!is.null(opts$x)) as.character(opts$x) else ""
  mediators <- resolve_parse_list(opts$m)
  mediators <- mediators[nzchar(mediators)]
  y <- if (!is.null(opts$y)) as.character(opts$y) else ""
  serial <- resolve_parse_bool(opts$serial, default = FALSE)

  model_syntax <- ""
  if (nzchar(model_text)) {
    model_syntax <- normalize_model_syntax(model_text)
  } else if (analysis == "cfa" && nzchar(factors_text)) {
    factors <- tryCatch(parse_factor_spec(factors_text), error = function(e) {
      emit_input_issue(out_dir, opts, e$message)
    })
    model_syntax <- build_cfa_model(factors)
  } else if (analysis == "mediation") {
    model_syntax <- tryCatch(build_mediation_model(x, mediators, y, covariates, serial), error = function(e) {
      emit_input_issue(out_dir, opts, e$message)
    })
  } else if (analysis %in% c("path", "sem") && nzchar(dv) && length(ivs) > 0) {
    model_syntax <- build_path_model(dv, ivs, covariates)
  }

  if (!nzchar(model_syntax)) {
    emit_input_issue(out_dir, opts, "Model syntax is required. Use --model, --model-file, or a builder option.")
  }

  model_vars <- extract_model_vars(model_syntax)
  if (length(model_vars) == 0) {
    tokens <- regmatches(model_syntax, gregexpr("[A-Za-z\\.][A-Za-z0-9_\\.]*", model_syntax, perl = TRUE))[[1]]
    tokens <- unique(tokens)
    tokens <- tokens[nzchar(tokens)]
    if (length(tokens) > 0) {
      model_vars <- intersect(tokens, names(df))
    }
  }
  missing_vars <- setdiff(model_vars, names(df))
  if (length(missing_vars) > 0) {
    emit_input_issue(out_dir, opts, paste0("Missing variables: ", paste(missing_vars, collapse = ", ")))
  }

  analysis_label <- switch(
    analysis,
    cfa = "SEM (CFA)",
    mediation = "SEM (Mediation)",
    path = "SEM (Path analysis)",
    invariance = "SEM (Invariance)",
    "SEM"
  )

  if (analysis == "invariance") {
    if (!nzchar(group_var)) {
      emit_input_issue(out_dir, opts, "Invariance analysis requires --group.")
    }
    invariance_steps <- resolve_parse_list(opts$invariance)
    if (length(invariance_steps) == 0) {
      if (length(group_equal) > 0) {
        invariance_steps <- "custom"
      } else {
        invariance_steps <- resolve_parse_list(invariance_default)
      }
    }
    if (length(invariance_steps) == 0) {
      emit_input_issue(out_dir, opts, "Invariance analysis requires --invariance steps or --group-equal.")
    }

    step_defs <- list()
    for (step in invariance_steps) {
      label <- tolower(step)
      constraints <- character(0)
      if (label %in% c("configural", "none")) {
        constraints <- character(0)
      } else if (label %in% c("metric", "loadings")) {
        constraints <- c("loadings")
      } else if (label %in% c("scalar", "intercepts")) {
        constraints <- c("loadings", "intercepts")
      } else if (label %in% c("strict", "residuals")) {
        constraints <- c("loadings", "intercepts", "residuals")
      } else if (label == "custom" && length(group_equal) > 0) {
        constraints <- group_equal
      } else {
        emit_input_issue(out_dir, opts, paste0("Unknown invariance step: ", step))
      }
      step_defs[[length(step_defs) + 1]] <- list(label = step, constraints = paste(constraints, collapse = ", "), group_equal = constraints)
    }

    fits <- list()
    for (step in step_defs) {
      fit <- tryCatch(
        fit_sem_model("cfa", model_syntax, df, estimator, missing, se, bootstrap_samples, ordered_vars, group_var, step$group_equal),
        error = function(e) {
          emit_input_issue(out_dir, opts, paste0("Model fit failed: ", e$message), status = "fit_failed")
        }
      )
      fits[[length(fits) + 1]] <- fit
    }

    summary_df <- build_invariance_summary(step_defs, fits, fit_indices)
    if (nrow(summary_df) == 0) {
      emit_input_issue(out_dir, opts, "No invariance results could be computed.", status = "fit_failed")
    }

    group_labels <- levels(df[[group_var]])
    n_obs <- tryCatch(lavaan::nobs(fits[[1]]), error = function(e) NA_real_)
    n_obs_label <- if (length(n_obs) > 1) paste(n_obs, collapse = ", ") else n_obs
    note_tokens <- build_sem_note_tokens(estimator, missing, se, ci_type, conf_level, std, n_obs, group_labels)
    fit_sentence <- "Measurement invariance fit indices are summarized in Table 1."
    token_meta <- list(
      estimator = estimator,
      missing = missing,
      se = se,
      ci = ci_type,
      conf_level = conf_level,
      std = std,
      n_obs = ifelse(is.na(n_obs_label), "", n_obs_label),
      group_labels = if (length(group_labels) > 0) paste(group_labels, collapse = ", ") else ""
    )

    template_path <- if (!is.null(template_override)) {
      template_override
    } else {
      resolve_get_template_path("sem.invariance", "sem/invariance-template.md")
    }
    template_meta <- resolve_get_template_meta(template_path)
    table_result <- build_invariance_table_body(summary_df, digits, template_meta$table)
    nlss_table <- paste0("Table 1\n\n", table_result$body, "\n", note_tokens$note_default)

    template_context <- list(
      tokens = c(
        list(
          table_body = table_result$body,
          narrative_default = fit_sentence
        ),
        token_meta,
        note_tokens
      )
    )

    nlss_report_path <- file.path(out_dir, "report_canonical.md")
    resolve_append_nlss_report(
      nlss_report_path,
      analysis_label,
      nlss_table,
      fit_sentence,
      analysis_flags = list(
        analysis = analysis,
        group = group_var,
        invariance = invariance_steps,
        estimator = estimator,
        missing = missing,
        se = se,
        ci = ci_type,
        "conf-level" = conf_level,
        std = std,
        fit = fit_indices,
        digits = digits
      ),
      template_path = template_path,
      template_context = template_context
    )

    cat("Wrote:\n")
    cat("- ", render_output_path(nlss_report_path, out_dir), "\n", sep = "")

    if (resolve_parse_bool(opts$log, default = log_default)) {
      ctx <- resolve_get_run_context()
      resolve_append_analysis_log(
        out_dir,
        module = "sem",
        prompt = ctx$prompt,
        commands = ctx$commands,
        results = list(
          status = "ok",
          analysis = analysis,
          n = n_obs,
          fit = summary_df
        ),
        options = list(
          analysis = analysis,
          estimator = estimator,
          missing = missing,
          se = se,
          ci = ci_type,
          conf_level = conf_level,
          std = std,
          fit = fit_indices,
          group = group_var,
          invariance = invariance_steps,
          model = model_syntax
        ),
        user_prompt = resolve_get_user_prompt(opts)
      )
    }
    return(invisible(NULL))
  }

  fit <- tryCatch(
    fit_sem_model(analysis, model_syntax, df, estimator, missing, se, bootstrap_samples, ordered_vars, group_var, group_equal),
    error = function(e) {
      emit_input_issue(out_dir, opts, paste0("Model fit failed: ", e$message), status = "fit_failed")
    }
  )

  group_labels <- character(0)
  if (nzchar(group_var)) {
    group_labels <- levels(df[[group_var]])
  }

  fit_values <- collect_fit_values(fit, fit_indices)
  param_df <- build_param_df(fit, std, conf_level, ci_type, group_labels)
  if (nrow(param_df) == 0) {
    emit_input_issue(out_dir, opts, "No parameter estimates could be computed.", status = "fit_failed")
  }

  r2_values <- NULL
  if (isTRUE(r2_flag)) {
    r2_values <- tryCatch(lavaan::inspect(fit, "r2"), error = function(e) NULL)
  }
  r2_df <- build_r2_df(r2_values)

  n_obs <- tryCatch(lavaan::nobs(fit), error = function(e) NA_real_)
  n_obs_label <- if (length(n_obs) > 1) paste(n_obs, collapse = ", ") else n_obs
  note_tokens <- build_sem_note_tokens(estimator, missing, se, ci_type, conf_level, std, n_obs, group_labels)
  token_meta <- list(
    estimator = estimator,
    missing = missing,
    se = se,
    ci = ci_type,
    conf_level = conf_level,
    std = std,
    n_obs = ifelse(is.na(n_obs_label), "", n_obs_label),
    group_labels = if (length(group_labels) > 0) paste(group_labels, collapse = ", ") else ""
  )

  fit_sentence <- build_fit_sentence(fit_values, digits)
  r2_sentence <- build_r2_sentence(r2_values, digits)

  indirect_rows <- param_df[param_df$op == ":=", , drop = FALSE]
  indirect_sentences <- character(0)
  if (nrow(indirect_rows) > 0) {
    for (i in seq_len(nrow(indirect_rows))) {
      row <- indirect_rows[i, ]
      label <- row$label
      if (!nzchar(label)) label <- row$path
      sentence <- paste0(
        label,
        ": b = ",
        format_stat(row$est, digits),
        ", SE = ",
        format_stat(row$se, digits),
        ifelse(is.na(row$p), "", paste0(", p ", format_p(row$p))),
        ifelse(is.na(row$ci_low) || is.na(row$ci_high), "", paste0(", ", round(conf_level * 100), "% CI [", format_stat(row$ci_low, digits), ", ", format_stat(row$ci_high, digits), "]")),
        "."
      )
      indirect_sentences <- c(indirect_sentences, sentence)
    }
  }

  narrative_lines <- c(fit_sentence, r2_sentence, indirect_sentences)
  narrative_lines <- narrative_lines[nzchar(narrative_lines)]
  nlss_text <- paste(narrative_lines, collapse = "\n")

  template_key <- switch(
    analysis,
    cfa = "sem.cfa",
    mediation = "sem.mediation",
    path = "sem.default",
    "sem.default"
  )
  template_default <- switch(
    analysis,
    cfa = "sem/cfa-template.md",
    mediation = "sem/mediation-template.md",
    "sem/default-template.md"
  )
  template_path <- if (!is.null(template_override)) {
    template_override
  } else {
    resolve_get_template_path(template_key, template_default)
  }
  template_meta <- resolve_get_template_meta(template_path)
  table_result <- build_sem_table_body(param_df, digits, template_meta$table)
  nlss_table <- paste0("Table 1\n\n", table_result$body, "\n", note_tokens$note_default)

  template_context <- list(
    tokens = c(
      list(
        table_body = table_result$body,
        narrative_default = nlss_text,
        fit_summary = fit_sentence,
        r2_summary = r2_sentence,
        indirect_summary = paste(indirect_sentences, collapse = "\n")
      ),
      token_meta,
      note_tokens
    )
  )

  nlss_report_path <- file.path(out_dir, "report_canonical.md")
  resolve_append_nlss_report(
    nlss_report_path,
    analysis_label,
    nlss_table,
    nlss_text,
    analysis_flags = list(
      analysis = analysis,
      dv = if (analysis %in% c("path", "sem") && nzchar(dv)) dv else NULL,
      ivs = if (analysis %in% c("path", "sem") && length(ivs) > 0) ivs else NULL,
      x = if (analysis == "mediation" && nzchar(x)) x else NULL,
      m = if (analysis == "mediation" && length(mediators) > 0) mediators else NULL,
      y = if (analysis == "mediation" && nzchar(y)) y else NULL,
      covariates = if (analysis == "mediation" && length(covariates) > 0) covariates else NULL,
      serial = if (analysis == "mediation" && isTRUE(serial)) TRUE else NULL,
      estimator = estimator,
      missing = missing,
      se = se,
      ci = ci_type,
      "conf-level" = conf_level,
      std = std,
      ordered = if (length(ordered_vars) > 0) ordered_vars else NULL,
      group = if (nzchar(group_var)) group_var else NULL,
      "group-equal" = if (length(group_equal) > 0) group_equal else NULL,
      fit = if (length(fit_indices) > 0) fit_indices else NULL,
      bootstrap = bootstrap,
      "bootstrap-samples" = if (bootstrap) bootstrap_samples else NULL,
      digits = digits
    ),
    template_path = template_path,
    template_context = template_context
  )

  cat("Wrote:\n")
  cat("- ", render_output_path(nlss_report_path, out_dir), "\n", sep = "")

  modindices_df <- NULL
  if (!is.na(modindices_cutoff) && modindices_cutoff > 0) {
    modindices_df <- tryCatch(
      lavaan::modindices(fit, sort. = TRUE, minimum.value = modindices_cutoff),
      error = function(e) NULL
    )
  }

  residuals_info <- NULL
  if (isTRUE(residuals_flag)) {
    residuals_info <- tryCatch(lavaan::residuals(fit, type = "standardized"), error = function(e) NULL)
  }

  if (resolve_parse_bool(opts$log, default = log_default)) {
    ctx <- resolve_get_run_context()
    resolve_append_analysis_log(
      out_dir,
      module = "sem",
      prompt = ctx$prompt,
      commands = ctx$commands,
        results = list(
          status = "ok",
          analysis = analysis,
          n = n_obs,
          fit = fit_values,
          params_df = param_df,
          r2_df = r2_df,
          params = list(rows = nrow(param_df)),
          modindices = if (!is.null(modindices_df)) nrow(modindices_df) else 0
        ),
      options = list(
        analysis = analysis,
        estimator = estimator,
        missing = missing,
        se = se,
        ci = ci_type,
        conf_level = conf_level,
        std = std,
        fit = fit_indices,
        ordered = ordered_vars,
        group = group_var,
        group_equal = group_equal,
        bootstrap = bootstrap,
        bootstrap_samples = bootstrap_samples,
        model = model_syntax,
        modindices_cutoff = modindices_cutoff,
        residuals = residuals_flag,
        modindices = modindices_df,
        residuals_output = residuals_info
      ),
      user_prompt = resolve_get_user_prompt(opts)
    )
  }
}

main()
