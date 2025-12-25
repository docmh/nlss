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
  cat("Regression (base R)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript regression.R --csv data.csv --dv outcome --ivs x1,x2,x3\n")
  cat("  Rscript regression.R --csv data.csv --dv outcome --blocks \"age,gender;stress,trait\"\n")
  cat("  Rscript regression.R --csv data.csv --dv outcome --ivs x1,x2 --interactions x1:moderator\n")
  cat("  Rscript regression.R --csv data.csv --dv outcome --ivs x1,x2 --group site\n")
  cat("  Rscript regression.R --csv data.csv --dv binary --ivs x1,x2 --family binomial\n")
  cat("  Rscript regression.R --interactive\n")
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
  cat("  --dv NAME              Dependent variable\n")
  cat("  --ivs LIST             Comma-separated predictors\n")
  cat("  --blocks TEXT          Semicolon-separated blocks (e.g., x1,x2;x3,x4)\n")
  cat("  --interactions LIST    Comma-separated interaction terms (e.g., x1:mod)\n")
  cat("  --group NAME           Grouping variable (optional)\n")
  cat("  --family NAME          gaussian/binomial/poisson (default: gaussian)\n")
  cat("  --link NAME            Link function (optional)\n")
  cat("  --center TYPE          none/mean (default: none)\n")
  cat("  --standardize TYPE     none/predictors (default: none)\n")
  cat("  --conf-level VALUE     Confidence level (default: 0.95)\n")
  cat("  --bootstrap TRUE/FALSE Bootstrap confidence intervals (default: FALSE)\n")
  cat("  --bootstrap-samples N  Bootstrap resamples (default: 1000)\n")
  cat("  --seed N               Random seed for bootstrap (optional)\n")
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

  opts$dv <- resolve_prompt("Dependent variable", "")
  blocks <- resolve_prompt("Blocks (semicolon-separated; leave blank for --ivs)", "")
  if (blocks != "") {
    opts$blocks <- blocks
  } else {
    opts$ivs <- resolve_prompt("Predictors (comma-separated)", "")
  }
  opts$interactions <- resolve_prompt("Interactions (comma-separated, optional)", "")
  opts$group <- resolve_prompt("Grouping variable (optional)", "")

  family_default <- resolve_config_value("modules.regression.family", "gaussian")
  link_default <- resolve_config_value("modules.regression.link", "")
  center_default <- resolve_config_value("modules.regression.center", "none")
  standardize_default <- resolve_config_value("modules.regression.standardize", "none")
  conf_default <- resolve_config_value("modules.regression.conf_level", 0.95)
  bootstrap_default <- resolve_config_value("modules.regression.bootstrap", FALSE)
  bootstrap_samples_default <- resolve_config_value("modules.regression.bootstrap_samples", 1000)
  digits_default <- resolve_config_value("defaults.digits", 2)

  opts$family <- resolve_prompt("Family (gaussian/binomial/poisson)", family_default)
  opts$link <- resolve_prompt("Link (optional)", link_default)
  opts$center <- resolve_prompt("Centering (none/mean)", center_default)
  opts$standardize <- resolve_prompt("Standardize coefficients (none/predictors)", standardize_default)
  opts$`conf-level` <- resolve_prompt("Confidence level", as.character(conf_default))
  opts$bootstrap <- resolve_prompt("Bootstrap TRUE/FALSE", ifelse(isTRUE(bootstrap_default), "TRUE", "FALSE"))
  opts$`bootstrap-samples` <- resolve_prompt("Bootstrap samples", as.character(bootstrap_samples_default))
  opts$seed <- resolve_prompt("Bootstrap seed (optional)", "")
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

resolve_select_variables <- function(df, vars, group_var = NULL, default = "numeric") {
  if (exists("select_variables", mode = "function")) {
    return(get("select_variables", mode = "function")(df, vars, group_var = group_var, default = default))
  }
  available <- names(df)
  if (is.null(vars) || vars == "") {
    selected <- available[sapply(df, is.numeric)]
    if (!is.null(group_var)) selected <- setdiff(selected, group_var)
    return(selected)
  }
  requested <- trimws(strsplit(vars, ",", fixed = TRUE)[[1]])
  missing <- setdiff(requested, available)
  if (length(missing) > 0) {
    stop(paste("Unknown variables:", paste(missing, collapse = ", ")))
  }
  if (!is.null(group_var)) requested <- setdiff(requested, group_var)
  requested
}

resolve_get_template_path <- function(key, default_relative = NULL) {
  if (exists("resolve_template_path", mode = "function")) {
    return(get("resolve_template_path", mode = "function")(key, default_relative))
  }
  if (is.null(default_relative) || !nzchar(default_relative)) return(NULL)
  if (exists("get_assets_dir", mode = "function")) {
    return(file.path(get("get_assets_dir", mode = "function")(), default_relative))
  }
  file.path(getwd(), "core-stats", "assets", default_relative)
}

resolve_get_template_meta <- function(path) {
  if (exists("get_template_meta", mode = "function")) {
    return(get("get_template_meta", mode = "function")(path))
  }
  list()
}

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

parse_blocks <- function(value) {
  if (is.null(value) || value == "") return(list())
  blocks <- strsplit(as.character(value), ";", fixed = TRUE)[[1]]
  blocks <- trimws(blocks)
  blocks <- blocks[blocks != ""]
  lapply(blocks, function(block) {
    vars <- trimws(strsplit(block, ",", fixed = TRUE)[[1]])
    vars[vars != ""]
  })
}

parse_interactions <- function(value) {
  interactions <- resolve_parse_list(value)
  interactions[nzchar(interactions)]
}

parse_term_vars <- function(term) {
  term <- gsub("\\s+", "", as.character(term))
  if (!nzchar(term)) return(character(0))
  parts <- strsplit(term, "[*:]", perl = TRUE)[[1]]
  parts <- parts[parts != ""]
  parts
}

normalize_center <- function(value, default = "none") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (val %in% c("none", "no", "false")) return("none")
  if (val %in% c("mean", "center", "centre")) return("mean")
  default
}

normalize_standardize <- function(value, default = "none") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (val %in% c("none", "no", "false")) return("none")
  if (val %in% c("predictors", "pred", "x")) return("predictors")
  default
}

normalize_family <- function(value, default = "gaussian") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (val %in% c("gaussian", "normal", "linear", "lm")) return("gaussian")
  if (val %in% c("binomial", "logistic", "logit")) return("binomial")
  if (val %in% c("poisson", "count")) return("poisson")
  default
}

normalize_link <- function(value, default = NULL, family = "gaussian") {
  if (!is.null(value) && nzchar(value)) return(as.character(value))
  if (!is.null(default) && nzchar(default)) return(as.character(default))
  if (family == "gaussian") return("identity")
  if (family == "binomial") return("logit")
  if (family == "poisson") return("log")
  "identity"
}

build_family <- function(family, link) {
  if (family == "binomial") return(binomial(link = link))
  if (family == "poisson") return(poisson(link = link))
  gaussian(link = link)
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

coerce_predictors <- function(df, predictors) {
  for (var in predictors) {
    if (!var %in% names(df)) next
    if (is.numeric(df[[var]])) next
    df[[var]] <- as.factor(df[[var]])
  }
  df
}

center_predictors <- function(df, predictors, center) {
  if (center != "mean") return(df)
  for (var in predictors) {
    if (!var %in% names(df)) next
    if (!is.numeric(df[[var]])) next
    df[[var]] <- df[[var]] - mean(df[[var]], na.rm = TRUE)
  }
  df
}

compute_standardized_betas <- function(df, dv, term_names, estimates, standardize) {
  if (standardize == "none") return(rep(NA_real_, length(term_names)))
  if (!dv %in% names(df)) return(rep(NA_real_, length(term_names)))
  y <- df[[dv]]
  if (!is.numeric(y)) return(rep(NA_real_, length(term_names)))
  sd_y <- sd(y)
  if (is.na(sd_y) || sd_y == 0) return(rep(NA_real_, length(term_names)))
  betas <- rep(NA_real_, length(term_names))
  for (i in seq_along(term_names)) {
    term <- term_names[i]
    if (!term %in% names(df)) next
    if (!is.numeric(df[[term]])) next
    sd_x <- sd(df[[term]])
    if (is.na(sd_x) || sd_x == 0) next
    betas[i] <- estimates[i] * sd_x / sd_y
  }
  betas
}

safe_fit_model <- function(formula, data, family, link) {
  if (family == "gaussian") {
    return(tryCatch(lm(formula, data = data), error = function(e) NULL))
  }
  fam <- tryCatch(build_family(family, link), error = function(e) NULL)
  if (is.null(fam)) return(NULL)
  tryCatch(glm(formula, data = data, family = fam), error = function(e) NULL)
}

extract_coefficients <- function(model, data, dv, family, conf_level, standardize, boot_ci = NULL) {
  summ <- summary(model)
  coef_mat <- summ$coefficients
  term_names <- rownames(coef_mat)
  estimate <- coef_mat[, 1]
  se <- coef_mat[, 2]
  stat <- coef_mat[, 3]
  p_val <- coef_mat[, 4]

  stat_label <- ifelse(family == "gaussian", "t", "z")
  df_resid <- if (family == "gaussian") df.residual(model) else NA_real_
  crit <- if (family == "gaussian") {
    qt(1 - (1 - conf_level) / 2, df_resid)
  } else {
    qnorm(1 - (1 - conf_level) / 2)
  }
  ci_low <- estimate - crit * se
  ci_high <- estimate + crit * se

  beta <- if (family == "gaussian") {
    compute_standardized_betas(data, dv, term_names, estimate, standardize)
  } else {
    rep(NA_real_, length(term_names))
  }

  exp_b <- if (family != "gaussian") exp(estimate) else rep(NA_real_, length(term_names))
  exp_ci_low <- if (family != "gaussian") exp(ci_low) else rep(NA_real_, length(term_names))
  exp_ci_high <- if (family != "gaussian") exp(ci_high) else rep(NA_real_, length(term_names))

  boot_low <- rep(NA_real_, length(term_names))
  boot_high <- rep(NA_real_, length(term_names))
  if (!is.null(boot_ci) && nrow(boot_ci) > 0) {
    match_idx <- match(term_names, boot_ci$term)
    boot_low <- boot_ci$boot_ci_low[match_idx]
    boot_high <- boot_ci$boot_ci_high[match_idx]
  }

  data.frame(
    term = term_names,
    estimate = estimate,
    se = se,
    stat = stat,
    stat_label = stat_label,
    p = p_val,
    ci_low = ci_low,
    ci_high = ci_high,
    beta = beta,
    exp_b = exp_b,
    exp_ci_low = exp_ci_low,
    exp_ci_high = exp_ci_high,
    boot_ci_low = boot_low,
    boot_ci_high = boot_high,
    stringsAsFactors = FALSE
  )
}

calc_model_summary <- function(model, data, dv, family, link) {
  n <- nrow(data)
  if (family == "gaussian") {
    summ <- summary(model)
    f_stat <- summ$fstatistic
    f_val <- unname(f_stat["value"])
    df1 <- unname(f_stat["numdf"])
    df2 <- unname(f_stat["dendf"])
    p_val <- pf(f_val, df1, df2, lower.tail = FALSE)
    r2 <- summ$r.squared
    adj_r2 <- summ$adj.r.squared
    rmse <- sqrt(mean(resid(model)^2))
    data.frame(
      model_stat = f_val,
      model_df1 = df1,
      model_df2 = df2,
      model_p = p_val,
      r2 = r2,
      adj_r2 = adj_r2,
      rmse = rmse,
      aic = AIC(model),
      bic = BIC(model),
      deviance = NA_real_,
      null_deviance = NA_real_,
      pseudo_r2 = NA_real_,
      n = n,
      stringsAsFactors = FALSE
    )
  } else {
    null_model <- safe_fit_model(as.formula(paste(dv, "~ 1")), data, family, link)
    chisq <- NA_real_
    df_chisq <- NA_real_
    p_val <- NA_real_
    pseudo_r2 <- NA_real_
    if (!is.null(null_model)) {
      chisq <- null_model$deviance - model$deviance
      df_chisq <- null_model$df.residual - model$df.residual
      if (!is.na(chisq) && !is.na(df_chisq) && df_chisq > 0) {
        p_val <- pchisq(chisq, df_chisq, lower.tail = FALSE)
      }
      ll_null <- as.numeric(logLik(null_model))
      ll_full <- as.numeric(logLik(model))
      if (!is.na(ll_null) && ll_null != 0) {
        pseudo_r2 <- 1 - (ll_full / ll_null)
      }
    }
    data.frame(
      model_stat = chisq,
      model_df1 = df_chisq,
      model_df2 = NA_real_,
      model_p = p_val,
      r2 = NA_real_,
      adj_r2 = NA_real_,
      rmse = NA_real_,
      aic = AIC(model),
      bic = BIC(model),
      deviance = model$deviance,
      null_deviance = if (!is.null(null_model)) null_model$deviance else NA_real_,
      pseudo_r2 = pseudo_r2,
      n = n,
      stringsAsFactors = FALSE
    )
  }
}

compute_bootstrap_ci <- function(data, formula, family, link, term_names, conf_level, samples) {
  if (samples < 1) return(NULL)
  boot_mat <- matrix(NA_real_, nrow = samples, ncol = length(term_names))
  colnames(boot_mat) <- term_names

  for (i in seq_len(samples)) {
    idx <- sample(seq_len(nrow(data)), replace = TRUE)
    boot_data <- data[idx, , drop = FALSE]
    fit <- safe_fit_model(formula, boot_data, family, link)
    if (is.null(fit)) next
    coefs <- coef(fit)
    for (term in term_names) {
      if (term %in% names(coefs)) {
        boot_mat[i, term] <- unname(coefs[term])
      }
    }
  }

  alpha <- (1 - conf_level) / 2
  ci_low <- apply(boot_mat, 2, function(x) {
    if (all(is.na(x))) return(NA_real_)
    quantile(x, probs = alpha, na.rm = TRUE)
  })
  ci_high <- apply(boot_mat, 2, function(x) {
    if (all(is.na(x))) return(NA_real_)
    quantile(x, probs = 1 - alpha, na.rm = TRUE)
  })

  data.frame(
    term = term_names,
    boot_ci_low = as.numeric(ci_low),
    boot_ci_high = as.numeric(ci_high),
    stringsAsFactors = FALSE
  )
}

build_blocks_label <- function(blocks) {
  if (length(blocks) == 0) return("")
  labels <- character(0)
  for (i in seq_along(blocks)) {
    block_terms <- blocks[[i]]
    if (length(block_terms) == 0) next
    label <- paste0("Block ", i, ": ", paste(block_terms, collapse = ", "))
    labels <- c(labels, label)
  }
  paste(labels, collapse = "; ")
}

build_regression_table_body <- function(coef_df, digits, table_meta) {
  default_specs <- list(
    list(key = "model", label = "Model", drop_if_empty = TRUE),
    list(key = "group", label = "Group", drop_if_empty = TRUE),
    list(key = "term", label = "Predictor"),
    list(key = "b", label = "b"),
    list(key = "se", label = "SE"),
    list(key = "t", label = "t", drop_if_empty = TRUE),
    list(key = "z", label = "z", drop_if_empty = TRUE),
    list(key = "p", label = "p"),
    list(key = "ci_low", label = "CI low", drop_if_empty = TRUE),
    list(key = "ci_high", label = "CI high", drop_if_empty = TRUE),
    list(key = "beta", label = "beta", drop_if_empty = TRUE),
    list(key = "exp_b", label = "exp(b)", drop_if_empty = TRUE),
    list(key = "exp_ci_low", label = "exp CI low", drop_if_empty = TRUE),
    list(key = "exp_ci_high", label = "exp CI high", drop_if_empty = TRUE),
    list(key = "boot_ci_low", label = "Boot CI low", drop_if_empty = TRUE),
    list(key = "boot_ci_high", label = "Boot CI high", drop_if_empty = TRUE)
  )
  columns <- resolve_normalize_table_columns(table_meta$columns, default_specs)

  show_model <- length(unique(coef_df$model)) > 1
  show_group <- any(nzchar(coef_df$group))
  rows <- list()
  for (i in seq_len(nrow(coef_df))) {
    row <- coef_df[i, ]
    row_map <- list(
      model = if (show_model) row$model else "",
      group = if (show_group) row$group else "",
      term = row$term,
      b = format_stat(row$estimate, digits),
      se = format_stat(row$se, digits),
      t = ifelse(row$stat_label == "t", format_stat(row$stat, digits), ""),
      z = ifelse(row$stat_label == "z", format_stat(row$stat, digits), ""),
      p = format_p(row$p),
      ci_low = format_stat(row$ci_low, digits),
      ci_high = format_stat(row$ci_high, digits),
      beta = format_stat(row$beta, digits),
      exp_b = format_stat(row$exp_b, digits),
      exp_ci_low = format_stat(row$exp_ci_low, digits),
      exp_ci_high = format_stat(row$exp_ci_high, digits),
      boot_ci_low = format_stat(row$boot_ci_low, digits),
      boot_ci_high = format_stat(row$boot_ci_high, digits)
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

build_regression_narrative_rows <- function(summary_df, comparisons_df, digits, family) {
  rows <- list()
  if (nrow(summary_df) == 0) return(rows)
  for (i in seq_len(nrow(summary_df))) {
    row <- summary_df[i, ]
    group_label <- ifelse(is.null(row$group) || row$group == "", "", paste0("Group ", row$group, ", "))
    model_label <- row$model
    sentence <- ""
    delta_r2 <- ""
    delta_f <- ""
    delta_df1 <- ""
    delta_df2 <- ""
    delta_p <- ""
    delta_deviance <- ""
    delta_chisq <- ""
    if (family == "gaussian") {
      sentence <- sprintf(
        "%s%s: F(%s, %s) = %s, p %s, R2 = %s, adj. R2 = %s.",
        group_label,
        model_label,
        format_num(row$model_df1, digits),
        format_num(row$model_df2, digits),
        format_stat(row$model_stat, digits),
        format_p(row$model_p),
        format_stat(row$r2, digits),
        format_stat(row$adj_r2, digits)
      )
    } else {
      sentence <- sprintf(
        "%s%s: Chi-square(%s) = %s, p %s, McFadden R2 = %s.",
        group_label,
        model_label,
        format_num(row$model_df1, digits),
        format_stat(row$model_stat, digits),
        format_p(row$model_p),
        format_stat(row$pseudo_r2, digits)
      )
    }

    if (nrow(comparisons_df) > 0) {
      comp_row <- comparisons_df[comparisons_df$model == row$model & comparisons_df$group == row$group, , drop = FALSE]
      if (nrow(comp_row) > 0) {
        comp <- comp_row[1, ]
        if (family == "gaussian") {
          delta_r2 <- format_stat(comp$delta_r2, digits)
          delta_f <- format_stat(comp$delta_f, digits)
          delta_df1 <- format_num(comp$df1, digits)
          delta_df2 <- format_num(comp$df2, digits)
          delta_p <- format_p(comp$p)
          sentence <- paste0(
            sentence,
            sprintf(
              " Delta R2 = %s, F-change(%s, %s) = %s, p %s.",
              delta_r2,
              delta_df1,
              delta_df2,
              delta_f,
              delta_p
            )
          )
        } else {
          delta_deviance <- format_stat(comp$delta_deviance, digits)
          delta_chisq <- format_stat(comp$delta_chisq, digits)
          delta_df1 <- format_num(comp$df1, digits)
          delta_p <- format_p(comp$p)
          sentence <- paste0(
            sentence,
            sprintf(
              " Delta deviance = %s, Chi-square(%s) = %s, p %s.",
              delta_deviance,
              delta_df1,
              delta_chisq,
              delta_p
            )
          )
        }
      }
    }

    rows[[length(rows) + 1]] <- list(
      full_sentence = sentence,
      model = row$model,
      group = row$group,
      n = format_num(row$n, digits),
      f = format_stat(row$model_stat, digits),
      df1 = format_num(row$model_df1, digits),
      df2 = format_num(row$model_df2, digits),
      p = format_p(row$model_p),
      r2 = format_stat(row$r2, digits),
      adj_r2 = format_stat(row$adj_r2, digits),
      rmse = format_stat(row$rmse, digits),
      chisq = format_stat(row$model_stat, digits),
      pseudo_r2 = format_stat(row$pseudo_r2, digits),
      delta_r2 = delta_r2,
      delta_f = delta_f,
      delta_df1 = delta_df1,
      delta_df2 = delta_df2,
      delta_p = delta_p,
      delta_deviance = delta_deviance,
      delta_chisq = delta_chisq
    )
  }
  rows
}

build_regression_note_tokens <- function(family, conf_level, bootstrap, bootstrap_samples, standardize) {
  notes <- character(0)
  notes <- c(notes, "Coefficients are unstandardized (b) with standard errors.")
  if (standardize != "none" && family == "gaussian") {
    notes <- c(notes, "Standardized coefficients (beta) are reported for numeric predictors.")
  }
  if (family == "binomial") {
    notes <- c(notes, "exp(b) corresponds to odds ratios.")
  } else if (family == "poisson") {
    notes <- c(notes, "exp(b) corresponds to incidence rate ratios.")
  }
  notes <- c(notes, sprintf("CI uses %s%% confidence.", round(conf_level * 100)))
  if (isTRUE(bootstrap)) {
    notes <- c(notes, sprintf("Bootstrap percentile CIs use %s resamples.", bootstrap_samples))
  }
  list(note_default = paste(notes, collapse = " "))
}

emit_input_issue <- function(out_dir, opts, message, details = list(), status = "invalid_input") {
  log_default <- resolve_config_value("defaults.log", TRUE)
  if (resolve_parse_bool(opts$log, default = log_default)) {
    ctx <- resolve_get_run_context()
    resolve_append_analysis_log(
      out_dir,
      module = "regression",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(
        status = status,
        message = message,
        details = details
      ),
      options = list(
        dv = opts$dv,
        ivs = opts$ivs,
        blocks = opts$blocks,
        interactions = opts$interactions,
        group = opts$group,
        family = opts$family,
        link = opts$link
      ),
      user_prompt = resolve_get_user_prompt(opts)
    )
  }
  stop(message)
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
  ivs_default <- resolve_config_value("modules.regression.ivs_default", "numeric")
  family_default <- resolve_config_value("modules.regression.family", "gaussian")
  link_default <- resolve_config_value("modules.regression.link", "")
  conf_default <- resolve_config_value("modules.regression.conf_level", 0.95)
  center_default <- resolve_config_value("modules.regression.center", "none")
  standardize_default <- resolve_config_value("modules.regression.standardize", "none")
  bootstrap_default <- resolve_config_value("modules.regression.bootstrap", FALSE)
  bootstrap_samples_default <- resolve_config_value("modules.regression.bootstrap_samples", 1000)

  digits <- if (!is.null(opts$digits)) as.numeric(opts$digits) else digits_default
  df <- resolve_load_dataframe(opts)
  out_dir <- resolve_get_workspace_out_dir(df)

  if (is.null(opts$dv) || !nzchar(opts$dv)) {
    emit_input_issue(out_dir, opts, "Regression requires --dv.", details = list(dv = opts$dv))
  }
  dv <- as.character(opts$dv)
  if (!dv %in% names(df)) {
    emit_input_issue(out_dir, opts, sprintf("Dependent variable '%s' not found.", dv), details = list(dv = dv))
  }

  group_var <- if (!is.null(opts$group) && nzchar(opts$group)) as.character(opts$group) else NULL
  if (!is.null(group_var) && !group_var %in% names(df)) {
    emit_input_issue(out_dir, opts, sprintf("Grouping variable '%s' not found.", group_var), details = list(group = group_var))
  }

  blocks <- parse_blocks(opts$blocks)
  ivs <- resolve_parse_list(opts$ivs)
  interactions <- parse_interactions(opts$interactions)

  if (length(blocks) == 0) {
    if (length(ivs) == 0) {
      ivs <- resolve_select_variables(df, opts$ivs, group_var = group_var, default = ivs_default)
    }
    ivs <- setdiff(ivs, c(dv, group_var))
    if (length(ivs) == 0) {
      emit_input_issue(out_dir, opts, "Regression requires --ivs or --blocks.", details = list(ivs = opts$ivs, blocks = opts$blocks))
    }
    blocks <- list(ivs)
  } else {
    blocks <- lapply(blocks, function(vars) setdiff(vars, c(dv, group_var)))
  }

  interaction_vars <- unique(unlist(lapply(interactions, parse_term_vars)))
  missing_interaction_vars <- setdiff(interaction_vars, names(df))
  if (length(missing_interaction_vars) > 0) {
    emit_input_issue(
      out_dir,
      opts,
      sprintf("Unknown interaction variables: %s", paste(missing_interaction_vars, collapse = ", ")),
      details = list(interactions = interactions)
    )
  }

  if (length(interaction_vars) > 0) {
    base_predictors <- unique(unlist(blocks))
    missing_main <- setdiff(interaction_vars, base_predictors)
    if (length(missing_main) > 0) {
      blocks[[1]] <- unique(c(blocks[[1]], missing_main))
    }
    blocks <- c(blocks, list(interactions))
  }

  all_block_terms <- unique(unlist(blocks))
  all_block_terms <- all_block_terms[all_block_terms != ""]
  if (length(all_block_terms) == 0) {
    emit_input_issue(out_dir, opts, "Regression requires at least one predictor term.", details = list(blocks = opts$blocks))
  }

  cumulative_blocks <- list()
  for (i in seq_along(blocks)) {
    cumulative_blocks[[i]] <- unique(unlist(blocks[seq_len(i)]))
  }

  family <- normalize_family(opts$family, family_default)
  link <- normalize_link(opts$link, link_default, family)
  center <- normalize_center(opts$center, center_default)
  standardize <- normalize_standardize(opts$standardize, standardize_default)
  conf_level <- if (!is.null(opts$`conf-level`)) as.numeric(opts$`conf-level`) else conf_default
  bootstrap <- resolve_parse_bool(opts$bootstrap, default = bootstrap_default)
  bootstrap_samples <- if (!is.null(opts$`bootstrap-samples`)) as.numeric(opts$`bootstrap-samples`) else bootstrap_samples_default
  seed <- if (!is.null(opts$seed) && nzchar(opts$seed)) opts$seed else NULL
  if (!is.null(seed) && nzchar(seed)) {
    set.seed(as.numeric(seed))
  }

  group_values <- list(list(label = "", data = df))
  if (!is.null(group_var)) {
    group_levels <- unique(df[[group_var]])
    group_levels <- group_levels[!is.na(group_levels)]
    group_values <- lapply(group_levels, function(level) {
      list(
        label = as.character(level),
        data = df[df[[group_var]] == level, , drop = FALSE]
      )
    })
  }

  coef_rows <- list()
  summary_rows <- list()
  comparison_rows <- list()
  diagnostics_rows <- list()

  for (group_item in group_values) {
    group_label <- group_item$label
    data_group <- group_item$data

    term_vars <- unique(unlist(lapply(unlist(blocks), parse_term_vars)))
    vars_needed <- unique(c(dv, term_vars))
    vars_needed <- vars_needed[vars_needed != ""]
    missing_vars <- setdiff(vars_needed, names(data_group))
    if (length(missing_vars) > 0) {
      emit_input_issue(
        out_dir,
        opts,
        sprintf("Unknown variables in model: %s", paste(missing_vars, collapse = ", ")),
        details = list(vars = missing_vars)
      )
    }

    idx <- get_complete_rows(data_group[, vars_needed, drop = FALSE])
    data_model <- data_group[idx, , drop = FALSE]
    if (nrow(data_model) < 3) {
      emit_input_issue(
        out_dir,
        opts,
        sprintf("Not enough complete cases for group '%s'.", ifelse(group_label == "", "all", group_label)),
        details = list(group = group_label, n = nrow(data_model))
      )
    }

    data_model <- coerce_predictors(data_model, term_vars)
    data_model <- center_predictors(data_model, term_vars, center)

    if (family == "gaussian" && !is.numeric(data_model[[dv]])) {
      emit_input_issue(out_dir, opts, "Dependent variable must be numeric for gaussian regression.", details = list(dv = dv))
    }
    if (family == "poisson" && !is.numeric(data_model[[dv]])) {
      emit_input_issue(out_dir, opts, "Dependent variable must be numeric for poisson regression.", details = list(dv = dv))
    }
    if (family == "binomial" && !is.numeric(data_model[[dv]]) && !is.factor(data_model[[dv]])) {
      data_model[[dv]] <- as.factor(data_model[[dv]])
    }

    models <- list()
    summaries <- list()

    for (i in seq_along(cumulative_blocks)) {
      terms <- cumulative_blocks[[i]]
      terms <- terms[terms != ""]
      formula <- if (length(terms) == 0) {
        as.formula(paste(dv, "~ 1"))
      } else {
        as.formula(paste(dv, "~", paste(terms, collapse = " + ")))
      }
      fit <- safe_fit_model(formula, data_model, family, link)
      if (is.null(fit)) {
        emit_input_issue(
          out_dir,
          opts,
          sprintf("Model %s could not be estimated.", i),
          details = list(model = i, formula = deparse(formula))
        )
      }

      model_label <- paste0("Model ", i)
      boot_ci <- NULL
      if (isTRUE(bootstrap)) {
        term_names <- names(coef(fit))
        boot_ci <- compute_bootstrap_ci(data_model, formula, family, link, term_names, conf_level, bootstrap_samples)
      }
      coef_df <- extract_coefficients(fit, data_model, dv, family, conf_level, standardize, boot_ci)
      coef_df$model <- model_label
      coef_df$group <- group_label
      coef_rows[[length(coef_rows) + 1]] <- coef_df

      summary_df <- calc_model_summary(fit, data_model, dv, family, link)
      summary_df$model <- model_label
      summary_df$group <- group_label
      summary_rows[[length(summary_rows) + 1]] <- summary_df

      if (family == "gaussian") {
        shapiro <- tryCatch(shapiro.test(resid(fit)), error = function(e) NULL)
        diagnostics_rows[[length(diagnostics_rows) + 1]] <- data.frame(
          model = model_label,
          group = group_label,
          test = "Shapiro-Wilk",
          statistic = if (!is.null(shapiro)) unname(shapiro$statistic) else NA_real_,
          p = if (!is.null(shapiro)) shapiro$p.value else NA_real_,
          n = length(resid(fit)),
          stringsAsFactors = FALSE
        )
      }

      models[[length(models) + 1]] <- fit
      summaries[[length(summaries) + 1]] <- summary_df
    }

    if (length(models) > 1) {
      for (i in 2:length(models)) {
        prev <- models[[i - 1]]
        curr <- models[[i]]
        prev_summary <- summaries[[i - 1]]
        curr_summary <- summaries[[i]]
        model_label <- paste0("Model ", i)

        if (family == "gaussian") {
          comp <- anova(prev, curr)
          comp_row <- comp[2, ]
          comparison_rows[[length(comparison_rows) + 1]] <- data.frame(
            model = model_label,
            group = group_label,
            delta_r2 = curr_summary$r2 - prev_summary$r2,
            delta_f = if (!is.null(comp_row$F)) comp_row$F else NA_real_,
            df1 = if (!is.null(comp_row$Df)) comp_row$Df else NA_real_,
            df2 = if (!is.null(comp_row$Res.Df)) comp_row$Res.Df else NA_real_,
            p = if (!is.null(comp_row$`Pr(>F)`)) comp_row$`Pr(>F)` else NA_real_,
            delta_deviance = NA_real_,
            delta_chisq = NA_real_,
            stringsAsFactors = FALSE
          )
        } else {
          comp <- anova(prev, curr, test = "Chisq")
          comp_row <- comp[2, ]
          delta_dev <- if (!is.null(comp_row$Deviance)) comp_row$Deviance else NA_real_
          df1 <- if (!is.null(comp_row$Df)) comp_row$Df else NA_real_
          comparison_rows[[length(comparison_rows) + 1]] <- data.frame(
            model = model_label,
            group = group_label,
            delta_r2 = curr_summary$pseudo_r2 - prev_summary$pseudo_r2,
            delta_f = NA_real_,
            df1 = df1,
            df2 = NA_real_,
            p = if (!is.null(comp_row$`Pr(>Chi)`)) comp_row$`Pr(>Chi)` else NA_real_,
            delta_deviance = delta_dev,
            delta_chisq = delta_dev,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  coef_df <- if (length(coef_rows) > 0) do.call(rbind, coef_rows) else data.frame()
  summary_df <- if (length(summary_rows) > 0) do.call(rbind, summary_rows) else data.frame()
  comparison_df <- if (length(comparison_rows) > 0) do.call(rbind, comparison_rows) else data.frame()
  diagnostics_df <- if (length(diagnostics_rows) > 0) do.call(rbind, diagnostics_rows) else data.frame()

  if (nrow(coef_df) == 0) {
    stop("No regression results could be computed.")
  }

  blocks_label <- build_blocks_label(blocks)

  analysis_flags <- list(
    dv = dv,
    ivs = if (length(ivs) > 0) ivs else NULL,
    blocks = if (nzchar(blocks_label)) blocks_label else NULL,
    interactions = if (length(interactions) > 0) interactions else NULL,
    group = group_var,
    family = family,
    link = link,
    center = if (center != "none") center else NULL,
    standardize = if (standardize != "none") standardize else NULL,
    "conf-level" = conf_level,
    bootstrap = bootstrap,
    "bootstrap-samples" = if (bootstrap) bootstrap_samples else NULL,
    digits = digits
  )

  note_tokens <- build_regression_note_tokens(family, conf_level, bootstrap, bootstrap_samples, standardize)
  apa_text <- ""
  narrative_rows <- build_regression_narrative_rows(summary_df, comparison_df, digits, family)
  if (length(narrative_rows) > 0) {
    apa_text <- paste(vapply(narrative_rows, function(row) row$full_sentence, character(1)), collapse = "\n")
  }

  template_override <- resolve_template_override(opts$template, module = "regression")
  template_path <- if (!is.null(template_override)) {
    template_override
  } else {
    resolve_get_template_path("regression.default", "regression/default-template.md")
  }
  template_meta <- resolve_get_template_meta(template_path)
  table_result <- build_regression_table_body(coef_df, digits, template_meta$table)
  apa_table <- paste0("Table 1\n\n", table_result$body, "\n", note_tokens$note_default)

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

  apa_report_path <- file.path(out_dir, "apa_report.md")
  resolve_append_apa_report(
    apa_report_path,
    "Regression",
    apa_table,
    apa_text,
    analysis_flags = analysis_flags,
    template_path = template_path,
    template_context = template_context
  )

  cat("Wrote:\n")
  cat("- ", apa_report_path, "\n", sep = "")

  if (resolve_parse_bool(opts$log, default = log_default)) {
    ctx <- resolve_get_run_context()
    resolve_append_analysis_log(
      out_dir,
      module = "regression",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(
        coefficients_df = coef_df,
        summary_df = summary_df,
        comparisons_df = comparison_df,
        diagnostics_df = diagnostics_df
      ),
      options = list(
        dv = dv,
        ivs = ivs,
        blocks = blocks,
        interactions = interactions,
        group = group_var,
        family = family,
        link = link,
        center = center,
        standardize = standardize,
        conf_level = conf_level,
        bootstrap = bootstrap,
        bootstrap_samples = if (bootstrap) bootstrap_samples else NULL,
        digits = digits
      ),
      user_prompt = resolve_get_user_prompt(opts)
    )
  }
}

main()
