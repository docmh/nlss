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
  cat("Assumptions checks (base R)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript assumptions.R --csv data.csv --analysis ttest --vars var1,var2\n")
  cat("  Rscript assumptions.R --csv data.csv --analysis ttest --vars score --group condition\n")
  cat("  Rscript assumptions.R --csv data.csv --analysis ttest --x pre --y post\n")
  cat("  Rscript assumptions.R --csv data.csv --analysis anova --dv score --between group,gender\n")
  cat("  Rscript assumptions.R --csv data.csv --analysis anova --within pre,mid,post --between group\n")
  cat("  Rscript assumptions.R --csv data.csv --analysis regression --dv outcome --ivs age,stress\n")
  cat("  Rscript assumptions.R --csv data.csv --analysis regression --dv outcome --blocks age,gender;stress\n")
  cat("  Rscript assumptions.R --interactive\n")
  cat("\n")
  cat("Options:\n")
  cat("  --csv PATH               CSV input file\n")
  cat("  --sav PATH               SPSS .sav input file\n")
  cat("  --sep VALUE              CSV separator (default: ,)\n")
  cat("  --header TRUE/FALSE      CSV header (default: TRUE)\n")
  cat("  --rds PATH               RDS input file (data frame)\n")
  cat("  --rdata PATH             RData input file\n")
  cat("  --df NAME                Data frame object name in RData\n")
  cat("  --analysis TYPE          ttest/anova/regression/auto\n")
  cat("  --vars LIST              Variables for t-test (one-sample/independent)\n")
  cat("  --group NAME             Grouping variable (independent t-test)\n")
  cat("  --x LIST                 Paired measure 1 variables\n")
  cat("  --y LIST                 Paired measure 2 variables\n")
  cat("  --dv NAME                Dependent variable (ANOVA/regression)\n")
  cat("  --between LIST           Between-subjects factors (ANOVA)\n")
  cat("  --within LIST            Repeated measures variables (wide format)\n")
  cat("  --subject-id NAME        Subject id (optional, repeated measures)\n")
  cat("  --ivs LIST               Regression predictors\n")
  cat("  --blocks TEXT            Hierarchical blocks (semicolon-separated)\n")
  cat("  --normality TYPE         shapiro/none\n")
  cat("  --homogeneity TYPE       levene/bartlett/fligner/f/all/none\n")
  cat("  --linearity TRUE/FALSE   Regression linearity check\n")
  cat("  --homoscedasticity TRUE/FALSE  Breusch-Pagan-style test\n")
  cat("  --vif TRUE/FALSE         VIF multicollinearity check\n")
  cat("  --durbin-watson TRUE/FALSE  Durbin-Watson statistic\n")
  cat("  --outliers TRUE/FALSE    Standardized residual outliers\n")
  cat("  --influence TRUE/FALSE   Cook's distance influence\n")
  cat("  --vif-warn VALUE         VIF warning threshold (default: 5)\n")
  cat("  --vif-high VALUE         VIF high threshold (default: 10)\n")
  cat("  --outlier-z VALUE        Outlier z threshold (default: 3)\n")
  cat("  --cook-multiplier VALUE  Cook's D multiplier (default: 4)\n")
  cat("  --max-shapiro-n VALUE    Max n for Shapiro-Wilk (default: 5000)\n")
  cat("  --alpha VALUE            Decision alpha (default: 0.05)\n")
  cat("  --digits N               Rounding digits (default: 2)\n")
  cat("  --user-prompt TEXT       Original AI user prompt for logging (optional)\n")
  cat("  --log TRUE/FALSE         Write analysis_log.jsonl (default: TRUE)\n")
  cat("  --interactive            Prompt for inputs\n")
  cat("  --help                   Show this help\n")
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

resolve_select_variables <- function(df, vars, group_var = NULL, default = "numeric") {
  if (exists("select_variables", mode = "function")) {
    return(get("select_variables", mode = "function")(
      df,
      vars,
      group_var = group_var,
      default = default,
      include_numeric = FALSE
    ))
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

resolve_render_template_tokens <- function(text, tokens) {
  if (exists("render_template_tokens", mode = "function")) {
    return(get("render_template_tokens", mode = "function")(text, tokens))
  }
  text
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

interactive_options <- function() {
  cat("Interactive input selected.\n")
  input_type <- resolve_prompt("Input type (csv/sav/rds/rdata)", "csv")
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
  } else {
    stop("Unsupported input type.")
  }

  analysis_default <- resolve_config_value("modules.assumptions.analysis", "auto")
  analysis_input <- resolve_prompt("Analysis (ttest/anova/regression/auto)", analysis_default)
  analysis_norm <- normalize_analysis_type(analysis_input, default = "auto")
  opts$analysis <- analysis_norm

  if (analysis_norm == "ttest") {
    test_type <- resolve_prompt("t-test type (one-sample/independent/paired)", "one-sample")
    test_type <- normalize_ttest_type(test_type, default = "one_sample")
    if (test_type == "paired") {
      opts$x <- resolve_prompt("Measure 1 variables (comma-separated)", "")
      opts$y <- resolve_prompt("Measure 2 variables (comma-separated)", "")
    } else {
      opts$vars <- resolve_prompt("Variables (comma-separated)", "")
      if (test_type == "independent") {
        opts$group <- resolve_prompt("Grouping variable", "")
      }
    }
  } else if (analysis_norm == "anova") {
    mode <- resolve_prompt("ANOVA mode (between/within/mixed)", "between")
    mode <- tolower(mode)
    if (mode == "between") {
      opts$dv <- resolve_prompt("Dependent variable", "")
      opts$between <- resolve_prompt("Between-subjects factors (comma-separated)", "")
    } else {
      opts$within <- resolve_prompt("Repeated measures variables (comma-separated)", "")
      opts$between <- resolve_prompt("Between-subjects factors (optional)", "")
      opts$`subject-id` <- resolve_prompt("Subject id (optional)", "")
    }
  } else if (analysis_norm == "regression") {
    opts$dv <- resolve_prompt("Dependent variable", "")
    blocks <- resolve_prompt("Blocks (semicolon-separated; leave blank for --ivs)", "")
    if (blocks != "") {
      opts$blocks <- blocks
    } else {
      opts$ivs <- resolve_prompt("Predictors (comma-separated)", "")
    }
  }

  normality_default <- resolve_config_value("modules.assumptions.normality", "shapiro")
  homogeneity_default <- resolve_config_value("modules.assumptions.homogeneity", "levene")
  linearity_default <- resolve_config_value("modules.assumptions.linearity", TRUE)
  homoscedasticity_default <- resolve_config_value("modules.assumptions.homoscedasticity", TRUE)
  vif_default <- resolve_config_value("modules.assumptions.vif", TRUE)
  dw_default <- resolve_config_value("modules.assumptions.durbin_watson", TRUE)
  outliers_default <- resolve_config_value("modules.assumptions.outliers", TRUE)
  influence_default <- resolve_config_value("modules.assumptions.influence", TRUE)
  alpha_default <- resolve_config_value("modules.assumptions.alpha", 0.05)
  digits_default <- resolve_config_value("defaults.digits", 2)

  opts$normality <- resolve_prompt("Normality test (shapiro/none)", normality_default)
  if (analysis_norm %in% c("ttest", "anova")) {
    opts$homogeneity <- resolve_prompt("Homogeneity test (levene/bartlett/fligner/f/all/none)", homogeneity_default)
  } else {
    opts$linearity <- resolve_prompt("Linearity TRUE/FALSE", ifelse(isTRUE(linearity_default), "TRUE", "FALSE"))
    opts$homoscedasticity <- resolve_prompt("Homoscedasticity TRUE/FALSE", ifelse(isTRUE(homoscedasticity_default), "TRUE", "FALSE"))
    opts$vif <- resolve_prompt("VIF TRUE/FALSE", ifelse(isTRUE(vif_default), "TRUE", "FALSE"))
    opts$`durbin-watson` <- resolve_prompt("Durbin-Watson TRUE/FALSE", ifelse(isTRUE(dw_default), "TRUE", "FALSE"))
    opts$outliers <- resolve_prompt("Outliers TRUE/FALSE", ifelse(isTRUE(outliers_default), "TRUE", "FALSE"))
    opts$influence <- resolve_prompt("Influence TRUE/FALSE", ifelse(isTRUE(influence_default), "TRUE", "FALSE"))
  }

  opts$alpha <- resolve_prompt("Decision alpha", format(alpha_default, trim = TRUE))
  opts$digits <- resolve_prompt("Rounding digits", as.character(digits_default))
  opts$`user-prompt` <- resolve_prompt("User prompt (optional)", "")
  log_default <- resolve_config_value("defaults.log", TRUE)
  opts$log <- resolve_prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))
  opts
}

normalize_analysis_type <- function(value, default = "auto") {
  val <- tolower(trimws(as.character(value)))
  if (val == "" || val == "auto") return("auto")
  if (val %in% c("ttest", "t-test", "t_test", "t")) return("ttest")
  if (val %in% c("anova", "aov")) return("anova")
  if (val %in% c("regression", "reg", "lm")) return("regression")
  default
}

normalize_ttest_type <- function(value, default = "one_sample") {
  val <- tolower(trimws(as.character(value)))
  if (val %in% c("one", "one-sample", "one_sample", "onesample")) return("one_sample")
  if (val %in% c("independent", "ind", "between")) return("independent")
  if (val %in% c("paired", "within")) return("paired")
  default
}

parse_homogeneity_tests <- function(value, default = "levene") {
  if (is.null(value) || value == "") value <- default
  val <- tolower(trimws(as.character(value)))
  if (val == "none") return(character(0))
  if (val == "all") return(c("levene", "bartlett", "fligner", "f"))
  parts <- trimws(strsplit(val, ",", fixed = TRUE)[[1]])
  parts <- parts[parts != ""]
  parts <- gsub("\\s+", "", parts)
  unique(parts)
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

format_df <- function(df1, df2, digits) {
  if (is.na(df1) && is.na(df2)) return("")
  if (is.na(df2)) return(format_num(df1, digits))
  paste0(format_num(df1, digits), ", ", format_num(df2, digits))
}

safe_shapiro <- function(values, max_n = 5000) {
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

calc_var_test <- function(values, group) {
  group <- as.factor(group)
  levels <- levels(group)
  if (length(levels) != 2) {
    return(list(stat = NA_real_, df1 = NA_real_, df2 = NA_real_, p = NA_real_, note = "F-test requires two groups."))
  }
  x <- values[group == levels[1]]
  y <- values[group == levels[2]]
  test <- tryCatch(var.test(x, y), error = function(e) NULL)
  if (is.null(test)) {
    return(list(stat = NA_real_, df1 = NA_real_, df2 = NA_real_, p = NA_real_, note = "F-test failed."))
  }
  df_vals <- unname(test$parameter)
  list(
    stat = unname(test$statistic),
    df1 = df_vals[1],
    df2 = df_vals[2],
    p = test$p.value,
    note = ""
  )
}

calc_bp <- function(model) {
  res <- resid(model)
  model_frame <- model.frame(model)
  if (ncol(model_frame) <= 1) return(NULL)
  predictors <- model_frame[, -1, drop = FALSE]
  aux <- tryCatch(lm(res^2 ~ ., data = predictors), error = function(e) NULL)
  if (is.null(aux)) return(NULL)
  r2 <- summary(aux)$r.squared
  n <- length(res)
  df <- length(coef(aux)) - 1
  if (df <= 0) return(NULL)
  stat <- n * r2
  p <- pchisq(stat, df, lower.tail = FALSE)
  list(stat = stat, df1 = df, df2 = NA_real_, p = p)
}

calc_dw <- function(resid) {
  if (length(resid) < 2) return(NA_real_)
  sum(diff(resid)^2) / sum(resid^2)
}

calc_vif <- function(model) {
  mm <- model.matrix(model)
  if (ncol(mm) <= 1) return(data.frame())
  if ("(Intercept)" %in% colnames(mm)) {
    mm <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]
  }
  if (ncol(mm) == 0) return(data.frame())
  if (ncol(mm) == 1) {
    return(data.frame(term = colnames(mm), vif = 1, stringsAsFactors = FALSE))
  }
  vifs <- numeric(ncol(mm))
  for (j in seq_len(ncol(mm))) {
    fit <- tryCatch(lm(mm[, j] ~ mm[, -j, drop = FALSE]), error = function(e) NULL)
    if (is.null(fit)) {
      vifs[j] <- NA_real_
    } else {
      r2 <- summary(fit)$r.squared
      vifs[j] <- ifelse(is.na(r2) || r2 >= 1, Inf, 1 / (1 - r2))
    }
  }
  data.frame(term = colnames(mm), vif = vifs, stringsAsFactors = FALSE)
}

calc_linearity <- function(x, residuals) {
  if (!is.numeric(x)) return(list(stat = NA_real_, p = NA_real_))
  if (length(x) < 3) return(list(stat = NA_real_, p = NA_real_))
  test <- tryCatch(cor.test(x, residuals), error = function(e) NULL)
  if (is.null(test)) return(list(stat = NA_real_, p = NA_real_))
  list(stat = unname(test$estimate), p = test$p.value)
}

make_check_row <- function(analysis_type, model, assumption, test, target, group,
                           statistic, df1, df2, p, value, n, decision, note) {
  safe_text <- function(value) {
    if (is.null(value) || is.na(value)) return("")
    as.character(value)
  }
  data.frame(
    analysis_type = safe_text(analysis_type),
    model = safe_text(model),
    assumption = safe_text(assumption),
    test = safe_text(test),
    target = safe_text(target),
    group = safe_text(group),
    statistic = as.numeric(statistic),
    df1 = as.numeric(df1),
    df2 = as.numeric(df2),
    p = as.numeric(p),
    value = as.numeric(value),
    n = as.numeric(n),
    decision = safe_text(decision),
    note = safe_text(note),
    stringsAsFactors = FALSE
  )
}

decision_from_p <- function(p, alpha) {
  if (is.na(p)) return("")
  if (p < alpha) "violated" else "ok"
}

decision_from_vif <- function(vif, warn, high) {
  if (is.na(vif)) return("")
  if (is.infinite(vif)) return("high")
  if (vif >= high) return("high")
  if (vif >= warn) return("moderate")
  "ok"
}

decision_from_count <- function(count) {
  if (is.na(count)) return("")
  if (count > 0) "flag" else "ok"
}

build_assumptions_table_body <- function(checks_df, digits, table_spec = NULL) {
  default_columns <- list(
    list(key = "model", label = "Model", drop_if_empty = TRUE),
    list(key = "assumption", label = "Assumption"),
    list(key = "test", label = "Test"),
    list(key = "target", label = "Target"),
    list(key = "group", label = "Group", drop_if_empty = TRUE),
    list(key = "n", label = "n", drop_if_empty = TRUE),
    list(key = "statistic", label = "Statistic", drop_if_empty = TRUE),
    list(key = "df", label = "df", drop_if_empty = TRUE),
    list(key = "p", label = "p", drop_if_empty = TRUE),
    list(key = "value", label = "Value", drop_if_empty = TRUE),
    list(key = "decision", label = "Decision", drop_if_empty = TRUE),
    list(key = "note", label = "Note", drop_if_empty = TRUE)
  )
  columns <- resolve_normalize_table_columns(
    if (!is.null(table_spec) && !is.null(table_spec$columns)) table_spec$columns else NULL,
    default_columns
  )

  rows <- list()
  for (i in seq_len(nrow(checks_df))) {
    row <- checks_df[i, ]
    df_text <- format_df(row$df1, row$df2, digits)
    row_map <- list(
      analysis_type = row$analysis_type,
      model = row$model,
      assumption = row$assumption,
      test = row$test,
      target = row$target,
      group = row$group,
      n = ifelse(is.na(row$n), "", as.character(row$n)),
      statistic = format_stat(row$statistic, digits),
      df = df_text,
      p = format_p(row$p),
      value = format_stat(row$value, digits),
      decision = row$decision,
      note = row$note
    )
    row_vals <- vapply(columns, function(col) {
      resolve_as_cell_text(row_map[[col$key]])
    }, character(1))
    rows[[length(rows) + 1]] <- row_vals
  }

  filtered <- resolve_drop_empty_columns(columns, rows)
  columns <- filtered$columns
  rows <- filtered$rows
  headers <- vapply(columns, function(col) col$label, character(1))
  resolve_render_markdown_table(headers, rows)
}

format_apa_table <- function(checks_df, digits, note_text) {
  body <- build_assumptions_table_body(checks_df, digits, list())
  paste0("Table 1\n\n", body, "\n", note_text)
}

build_assumptions_narrative_rows <- function(checks_df, digits) {
  rows <- list()
  for (i in seq_len(nrow(checks_df))) {
    row <- checks_df[i, ]
    model_prefix <- if (nzchar(row$model)) paste0(row$model, ": ") else ""
    target <- row$target
    if (nzchar(row$group)) target <- paste0(target, " (", row$group, ")")
    stat_text <- format_stat(row$statistic, digits)
    df_text <- format_df(row$df1, row$df2, digits)
    p_text <- format_p(row$p)
    value_text <- format_stat(row$value, digits)
    parts <- c(
      paste0(model_prefix, row$assumption, " (", row$test, ") for ", target),
      if (nzchar(df_text)) paste0("df = ", df_text) else NULL,
      if (nzchar(stat_text)) paste0("statistic = ", stat_text) else NULL,
      if (nzchar(p_text)) paste0("p ", p_text) else NULL,
      if (nzchar(value_text) && !nzchar(p_text)) paste0("value = ", value_text) else NULL,
      if (!is.na(row$n)) paste0("n = ", row$n) else NULL
    )
    sentence <- paste(parts, collapse = ", ")
    if (sentence != "") sentence <- paste0(sentence, ".")
    rows[[length(rows) + 1]] <- list(
      full_sentence = sentence,
      analysis_type = row$analysis_type,
      model = row$model,
      assumption = row$assumption,
      test = row$test,
      target = row$target,
      group = row$group,
      statistic = stat_text,
      df = df_text,
      p = p_text,
      value = value_text,
      decision = row$decision,
      n = ifelse(is.na(row$n), "", as.character(row$n))
    )
  }
  rows
}

run_ttest_assumptions <- function(df, opts, settings) {
  rows <- list()
  has_group <- !is.null(opts$group) && opts$group != ""
  has_x <- !is.null(opts$x) && opts$x != ""
  has_y <- !is.null(opts$y) && opts$y != ""
  if (has_group && (has_x || has_y)) stop("Paired checks do not use --group.")

  mode <- if (has_x || has_y) {
    "paired"
  } else if (has_group) {
    "independent"
  } else {
    "one_sample"
  }

  if (mode == "paired") {
    x_vars <- resolve_parse_list(opts$x)
    y_vars <- resolve_parse_list(opts$y)
    if (length(x_vars) == 0 || length(y_vars) == 0) stop("Paired checks require --x and --y variables.")
    if (length(x_vars) != length(y_vars)) stop("--x and --y must have the same number of variables.")
    missing <- setdiff(c(x_vars, y_vars), names(df))
    if (length(missing) > 0) stop(paste("Unknown variables:", paste(missing, collapse = ", ")))
    for (i in seq_along(x_vars)) {
      x <- df[[x_vars[i]]]
      y <- df[[y_vars[i]]]
      if (!is.numeric(x) || !is.numeric(y)) {
        stop(paste("Paired variables must be numeric:", x_vars[i], y_vars[i]))
      }
      complete <- !is.na(x) & !is.na(y)
      diff_vals <- x[complete] - y[complete]
      if (settings$normality != "none") {
        shapiro <- safe_shapiro(diff_vals, settings$max_shapiro_n)
        decision <- decision_from_p(shapiro$p, settings$alpha)
        rows[[length(rows) + 1]] <- make_check_row(
          analysis_type = "ttest",
          model = "Paired",
          assumption = "Normality",
          test = "Shapiro-Wilk",
          target = paste0(x_vars[i], " - ", y_vars[i]),
          group = "",
          statistic = shapiro$w,
          df1 = NA_real_,
          df2 = NA_real_,
          p = shapiro$p,
          value = NA_real_,
          n = shapiro$n,
          decision = decision,
          note = shapiro$note
        )
      }
    }
  } else {
    group_var <- if (mode == "independent") opts$group else NULL
    if (mode == "independent" && !(group_var %in% names(df))) {
      stop("Grouping variable not found in data frame.")
    }
    vars_default <- settings$vars_default
    vars <- resolve_select_variables(df, opts$vars, group_var, default = vars_default)
    if (length(vars) == 0) stop("No numeric variables available for analysis.")

    for (var in vars) {
      vec <- df[[var]]
      if (!is.numeric(vec)) stop(paste("Variable is not numeric:", var))
      if (mode == "independent") {
        group_vals <- df[[group_var]]
        complete <- !is.na(vec) & !is.na(group_vals)
        vec <- vec[complete]
        group_vals <- as.character(group_vals[complete])
        levels <- unique(group_vals)
        if (length(levels) != 2) stop("Grouping variable must have exactly two levels for independent checks.")
        if (settings$normality != "none") {
          for (lvl in levels) {
            group_vec <- vec[group_vals == lvl]
            shapiro <- safe_shapiro(group_vec, settings$max_shapiro_n)
            decision <- decision_from_p(shapiro$p, settings$alpha)
            rows[[length(rows) + 1]] <- make_check_row(
              analysis_type = "ttest",
              model = "Independent",
              assumption = "Normality",
              test = "Shapiro-Wilk",
              target = var,
              group = lvl,
              statistic = shapiro$w,
              df1 = NA_real_,
              df2 = NA_real_,
              p = shapiro$p,
              value = NA_real_,
              n = shapiro$n,
              decision = decision,
              note = shapiro$note
            )
          }
        }

        if (length(settings$homogeneity_tests) > 0) {
          for (test_name in settings$homogeneity_tests) {
            result <- switch(
              test_name,
              levene = calc_levene(vec, group_vals),
              bartlett = calc_bartlett(vec, group_vals),
              fligner = calc_fligner(vec, group_vals),
              f = calc_var_test(vec, group_vals),
              NULL
            )
            if (is.null(result)) next
            decision <- decision_from_p(result$p, settings$alpha)
            test_label <- switch(
              test_name,
              levene = "Levene (median)",
              bartlett = "Bartlett",
              fligner = "Fligner-Killeen",
              f = "F-test",
              test_name
            )
            rows[[length(rows) + 1]] <- make_check_row(
              analysis_type = "ttest",
              model = "Independent",
              assumption = "Homogeneity",
              test = test_label,
              target = var,
              group = "",
              statistic = result$stat,
              df1 = result$df1,
              df2 = result$df2,
              p = result$p,
              value = NA_real_,
              n = length(vec),
              decision = decision,
              note = result$note
            )
          }
        }
      } else {
        if (settings$normality != "none") {
          clean <- vec[!is.na(vec)]
          shapiro <- safe_shapiro(clean, settings$max_shapiro_n)
          decision <- decision_from_p(shapiro$p, settings$alpha)
          rows[[length(rows) + 1]] <- make_check_row(
            analysis_type = "ttest",
            model = "One-sample",
            assumption = "Normality",
            test = "Shapiro-Wilk",
            target = var,
            group = "",
            statistic = shapiro$w,
            df1 = NA_real_,
            df2 = NA_real_,
            p = shapiro$p,
            value = NA_real_,
            n = shapiro$n,
            decision = decision,
            note = shapiro$note
          )
        }
      }
    }
  }

  checks_df <- if (length(rows) > 0) do.call(rbind, rows) else data.frame()
  list(checks = checks_df, mode = mode)
}

run_anova_assumptions <- function(df, opts, settings) {
  rows <- list()
  dv <- if (!is.null(opts$dv)) opts$dv else ""
  between_vars <- resolve_parse_list(opts$between)
  within_vars <- resolve_parse_list(opts$within)

  has_within <- length(within_vars) > 0
  mode <- if (has_within) {
    if (length(between_vars) > 0) "mixed" else "within"
  } else {
    "between"
  }

  if (has_within) {
    missing <- setdiff(within_vars, names(df))
    if (length(missing) > 0) stop(paste("Unknown within variables:", paste(missing, collapse = ", ")))
    for (var in within_vars) {
      if (!is.numeric(df[[var]])) stop(paste("Within variables must be numeric:", var))
    }
    if (length(between_vars) > 0) {
      missing_between <- setdiff(between_vars, names(df))
      if (length(missing_between) > 0) stop(paste("Unknown between variables:", paste(missing_between, collapse = ", ")))
    }

    data_subset <- df[, c(within_vars, between_vars), drop = FALSE]
    complete <- complete.cases(data_subset)
    data_subset <- data_subset[complete, , drop = FALSE]
    if (nrow(data_subset) == 0) stop("No complete cases for within-subject checks.")

    group_factor <- NULL
    if (length(between_vars) > 0) {
      for (var in between_vars) {
        data_subset[[var]] <- as.factor(data_subset[[var]])
      }
      group_factor <- interaction(data_subset[between_vars], drop = TRUE, sep = ":")
    }

    for (var in within_vars) {
      if (settings$normality != "none") {
        if (!is.null(group_factor)) {
          groups <- levels(group_factor)
          for (grp in groups) {
            vals <- data_subset[[var]][group_factor == grp]
            shapiro <- safe_shapiro(vals, settings$max_shapiro_n)
            decision <- decision_from_p(shapiro$p, settings$alpha)
            rows[[length(rows) + 1]] <- make_check_row(
              analysis_type = "anova",
              model = ifelse(mode == "mixed", "Mixed", "Within"),
              assumption = "Normality",
              test = "Shapiro-Wilk",
              target = var,
              group = grp,
              statistic = shapiro$w,
              df1 = NA_real_,
              df2 = NA_real_,
              p = shapiro$p,
              value = NA_real_,
              n = shapiro$n,
              decision = decision,
              note = shapiro$note
            )
          }
        } else {
          vals <- data_subset[[var]]
          shapiro <- safe_shapiro(vals, settings$max_shapiro_n)
          decision <- decision_from_p(shapiro$p, settings$alpha)
          rows[[length(rows) + 1]] <- make_check_row(
            analysis_type = "anova",
            model = "Within",
            assumption = "Normality",
            test = "Shapiro-Wilk",
            target = var,
            group = "",
            statistic = shapiro$w,
            df1 = NA_real_,
            df2 = NA_real_,
            p = shapiro$p,
            value = NA_real_,
            n = shapiro$n,
            decision = decision,
            note = shapiro$note
          )
        }
      }

      if (!is.null(group_factor) && length(settings$homogeneity_tests) > 0) {
        for (test_name in settings$homogeneity_tests) {
          result <- switch(
            test_name,
            levene = calc_levene(data_subset[[var]], group_factor),
            bartlett = calc_bartlett(data_subset[[var]], group_factor),
            fligner = calc_fligner(data_subset[[var]], group_factor),
            f = calc_var_test(data_subset[[var]], group_factor),
            NULL
          )
          if (is.null(result)) next
          decision <- decision_from_p(result$p, settings$alpha)
          test_label <- switch(
            test_name,
            levene = "Levene (median)",
            bartlett = "Bartlett",
            fligner = "Fligner-Killeen",
            f = "F-test",
            test_name
          )
          rows[[length(rows) + 1]] <- make_check_row(
            analysis_type = "anova",
            model = ifelse(mode == "mixed", "Mixed", "Within"),
            assumption = "Homogeneity",
            test = test_label,
            target = var,
            group = "",
            statistic = result$stat,
            df1 = result$df1,
            df2 = result$df2,
            p = result$p,
            value = NA_real_,
            n = nrow(data_subset),
            decision = decision,
            note = result$note
          )
        }
      }
    }

    if (length(within_vars) >= 3) {
      response_formula <- paste0("cbind(", paste(within_vars, collapse = ", "), ")")
      formula <- if (length(between_vars) > 0) {
        as.formula(paste(response_formula, "~", paste(between_vars, collapse = " + ")))
      } else {
        as.formula(paste(response_formula, "~ 1"))
      }
      fit <- tryCatch(lm(formula, data = data_subset), error = function(e) NULL)
      if (!is.null(fit)) {
        test <- tryCatch(mauchly.test(fit), error = function(e) NULL)
        if (!is.null(test)) {
          decision <- decision_from_p(test$p.value, settings$alpha)
          rows[[length(rows) + 1]] <- make_check_row(
            analysis_type = "anova",
            model = ifelse(mode == "mixed", "Mixed", "Within"),
            assumption = "Sphericity",
            test = "Mauchly",
            target = "Within",
            group = "",
            statistic = unname(test$statistic),
            df1 = NA_real_,
            df2 = NA_real_,
            p = test$p.value,
            value = NA_real_,
            n = nrow(data_subset),
            decision = decision,
            note = ""
          )
        } else {
          rows[[length(rows) + 1]] <- make_check_row(
            analysis_type = "anova",
            model = ifelse(mode == "mixed", "Mixed", "Within"),
            assumption = "Sphericity",
            test = "Mauchly",
            target = "Within",
            group = "",
            statistic = NA_real_,
            df1 = NA_real_,
            df2 = NA_real_,
            p = NA_real_,
            value = NA_real_,
            n = nrow(data_subset),
            decision = "",
            note = "Mauchly test failed."
          )
        }
      }
    }
  } else {
    if (dv == "" || !(dv %in% names(df))) stop("Dependent variable not found.")
    if (!is.numeric(df[[dv]])) stop("Dependent variable must be numeric.")
    if (length(between_vars) > 0) {
      missing_between <- setdiff(between_vars, names(df))
      if (length(missing_between) > 0) stop(paste("Unknown between variables:", paste(missing_between, collapse = ", ")))
    }
    data_subset <- df[, c(dv, between_vars), drop = FALSE]
    complete <- complete.cases(data_subset)
    data_subset <- data_subset[complete, , drop = FALSE]
    if (nrow(data_subset) == 0) stop("No complete cases for ANOVA checks.")

    group_factor <- NULL
    if (length(between_vars) > 0) {
      for (var in between_vars) {
        data_subset[[var]] <- as.factor(data_subset[[var]])
      }
      group_factor <- interaction(data_subset[between_vars], drop = TRUE, sep = ":")
    }

    if (settings$normality != "none") {
      if (!is.null(group_factor)) {
        groups <- levels(group_factor)
        for (grp in groups) {
          vals <- data_subset[[dv]][group_factor == grp]
          shapiro <- safe_shapiro(vals, settings$max_shapiro_n)
          decision <- decision_from_p(shapiro$p, settings$alpha)
          rows[[length(rows) + 1]] <- make_check_row(
            analysis_type = "anova",
            model = "Between",
            assumption = "Normality",
            test = "Shapiro-Wilk",
            target = dv,
            group = grp,
            statistic = shapiro$w,
            df1 = NA_real_,
            df2 = NA_real_,
            p = shapiro$p,
            value = NA_real_,
            n = shapiro$n,
            decision = decision,
            note = shapiro$note
          )
        }
      } else {
        vals <- data_subset[[dv]]
        shapiro <- safe_shapiro(vals, settings$max_shapiro_n)
        decision <- decision_from_p(shapiro$p, settings$alpha)
        rows[[length(rows) + 1]] <- make_check_row(
          analysis_type = "anova",
          model = "Between",
          assumption = "Normality",
          test = "Shapiro-Wilk",
          target = dv,
          group = "",
          statistic = shapiro$w,
          df1 = NA_real_,
          df2 = NA_real_,
          p = shapiro$p,
          value = NA_real_,
          n = shapiro$n,
          decision = decision,
          note = shapiro$note
        )
      }
    }

    if (!is.null(group_factor) && length(settings$homogeneity_tests) > 0) {
      for (test_name in settings$homogeneity_tests) {
        result <- switch(
          test_name,
          levene = calc_levene(data_subset[[dv]], group_factor),
          bartlett = calc_bartlett(data_subset[[dv]], group_factor),
          fligner = calc_fligner(data_subset[[dv]], group_factor),
          f = calc_var_test(data_subset[[dv]], group_factor),
          NULL
        )
        if (is.null(result)) next
        decision <- decision_from_p(result$p, settings$alpha)
        test_label <- switch(
          test_name,
          levene = "Levene (median)",
          bartlett = "Bartlett",
          fligner = "Fligner-Killeen",
          f = "F-test",
          test_name
        )
        rows[[length(rows) + 1]] <- make_check_row(
          analysis_type = "anova",
          model = "Between",
          assumption = "Homogeneity",
          test = test_label,
          target = dv,
          group = "",
          statistic = result$stat,
          df1 = result$df1,
          df2 = result$df2,
          p = result$p,
          value = NA_real_,
          n = nrow(data_subset),
          decision = decision,
          note = result$note
        )
      }
    }
  }

  checks_df <- if (length(rows) > 0) do.call(rbind, rows) else data.frame()
  list(checks = checks_df, mode = mode)
}

run_regression_assumptions <- function(df, opts, settings) {
  rows <- list()
  dv <- if (!is.null(opts$dv)) opts$dv else ""
  if (dv == "" || !(dv %in% names(df))) stop("Dependent variable not found.")

  blocks <- parse_blocks(opts$blocks)
  if (length(blocks) == 0) {
    ivs <- resolve_parse_list(opts$ivs)
    if (length(ivs) == 0) stop("Regression checks require --ivs or --blocks.")
    blocks <- list(ivs)
  }
  cumulative_blocks <- list()
  for (i in seq_along(blocks)) {
    cumulative_blocks[[i]] <- unique(unlist(blocks[seq_len(i)]))
  }

  for (i in seq_along(cumulative_blocks)) {
    predictors <- cumulative_blocks[[i]]
    missing <- setdiff(c(dv, predictors), names(df))
    if (length(missing) > 0) stop(paste("Unknown variables:", paste(missing, collapse = ", ")))
    formula <- as.formula(paste(dv, "~", paste(predictors, collapse = " + ")))
    model <- tryCatch(lm(formula, data = df, na.action = na.omit), error = function(e) NULL)
    if (is.null(model)) stop("Regression model failed.")

    model_label <- paste0("Block ", i)
    residuals <- resid(model)
    n <- length(residuals)

    if (settings$normality != "none") {
      shapiro <- safe_shapiro(residuals, settings$max_shapiro_n)
      decision <- decision_from_p(shapiro$p, settings$alpha)
      rows[[length(rows) + 1]] <- make_check_row(
        analysis_type = "regression",
        model = model_label,
        assumption = "Normality",
        test = "Shapiro-Wilk",
        target = "Residuals",
        group = "",
        statistic = shapiro$w,
        df1 = NA_real_,
        df2 = NA_real_,
        p = shapiro$p,
        value = NA_real_,
        n = shapiro$n,
        decision = decision,
        note = shapiro$note
      )
    }

    if (settings$linearity) {
      model_frame <- model.frame(model)
      predictors_data <- model_frame[, setdiff(names(model_frame), dv), drop = FALSE]
      for (pred in names(predictors_data)) {
        if (!is.numeric(predictors_data[[pred]])) next
        res <- calc_linearity(predictors_data[[pred]], residuals)
        decision <- decision_from_p(res$p, settings$alpha)
        rows[[length(rows) + 1]] <- make_check_row(
          analysis_type = "regression",
          model = model_label,
          assumption = "Linearity",
          test = "Residual correlation",
          target = pred,
          group = "",
          statistic = res$stat,
          df1 = NA_real_,
          df2 = NA_real_,
          p = res$p,
          value = NA_real_,
          n = n,
          decision = decision,
          note = ""
        )
      }
    }

    if (settings$homoscedasticity) {
      bp <- calc_bp(model)
      if (!is.null(bp)) {
        decision <- decision_from_p(bp$p, settings$alpha)
        rows[[length(rows) + 1]] <- make_check_row(
          analysis_type = "regression",
          model = model_label,
          assumption = "Homoscedasticity",
          test = "Breusch-Pagan",
          target = "Residuals",
          group = "",
          statistic = bp$stat,
          df1 = bp$df1,
          df2 = bp$df2,
          p = bp$p,
          value = NA_real_,
          n = n,
          decision = decision,
          note = ""
        )
      }
    }

    if (settings$durbin_watson) {
      dw <- calc_dw(residuals)
      rows[[length(rows) + 1]] <- make_check_row(
        analysis_type = "regression",
        model = model_label,
        assumption = "Independence",
        test = "Durbin-Watson",
        target = "Residuals",
        group = "",
        statistic = dw,
        df1 = NA_real_,
        df2 = NA_real_,
        p = NA_real_,
        value = NA_real_,
        n = n,
        decision = "",
        note = ""
      )
    }

    if (settings$outliers) {
      std_res <- tryCatch(rstandard(model), error = function(e) NULL)
      if (!is.null(std_res)) {
        max_abs <- max(abs(std_res), na.rm = TRUE)
        count <- sum(abs(std_res) > settings$outlier_z, na.rm = TRUE)
        decision <- decision_from_count(count)
        note <- paste0("|std resid| > ", settings$outlier_z, ": ", count)
        rows[[length(rows) + 1]] <- make_check_row(
          analysis_type = "regression",
          model = model_label,
          assumption = "Outliers",
          test = "Std. residuals",
          target = "Residuals",
          group = "",
          statistic = NA_real_,
          df1 = NA_real_,
          df2 = NA_real_,
          p = NA_real_,
          value = max_abs,
          n = n,
          decision = decision,
          note = note
        )
      }
    }

    if (settings$influence) {
      cooks <- tryCatch(cooks.distance(model), error = function(e) NULL)
      if (!is.null(cooks)) {
        max_cook <- max(cooks, na.rm = TRUE)
        threshold <- settings$cook_multiplier / n
        count <- sum(cooks > threshold, na.rm = TRUE)
        decision <- decision_from_count(count)
        note <- paste0("Cook's D > ", format_stat(threshold, settings$digits), ": ", count)
        rows[[length(rows) + 1]] <- make_check_row(
          analysis_type = "regression",
          model = model_label,
          assumption = "Influence",
          test = "Cook's distance",
          target = "Residuals",
          group = "",
          statistic = NA_real_,
          df1 = NA_real_,
          df2 = NA_real_,
          p = NA_real_,
          value = max_cook,
          n = n,
          decision = decision,
          note = note
        )
      }
    }

    if (settings$vif) {
      vifs <- calc_vif(model)
      if (nrow(vifs) > 0) {
        for (j in seq_len(nrow(vifs))) {
          vif_val <- vifs$vif[j]
          decision <- decision_from_vif(vif_val, settings$vif_warn, settings$vif_high)
          rows[[length(rows) + 1]] <- make_check_row(
            analysis_type = "regression",
            model = model_label,
            assumption = "Multicollinearity",
            test = "VIF",
            target = vifs$term[j],
            group = "",
            statistic = NA_real_,
            df1 = NA_real_,
            df2 = NA_real_,
            p = NA_real_,
            value = vif_val,
            n = n,
            decision = decision,
            note = ""
          )
        }
      }
    }
  }

  checks_df <- if (length(rows) > 0) do.call(rbind, rows) else data.frame()
  list(checks = checks_df)
}

build_note_tokens <- function(analysis_type, settings, homogeneity_tests) {
  homogeneity_label <- if (length(homogeneity_tests) == 0) {
    "None."
  } else {
    labels <- vapply(homogeneity_tests, function(test_name) {
      switch(
        test_name,
        levene = "Levene (median)",
        bartlett = "Bartlett",
        fligner = "Fligner-Killeen",
        f = "F-test",
        test_name
      )
    }, character(1))
    paste(labels, collapse = "; ")
  }

  note_default <- if (analysis_type == "regression") {
    parts <- c(
      if (settings$normality != "none") {
        paste0("Normality assessed with Shapiro-Wilk (alpha = ", settings$alpha, ").")
      } else {
        "Normality not assessed."
      },
      if (settings$homoscedasticity) "Homoscedasticity assessed with Breusch-Pagan." else "Homoscedasticity not assessed.",
      if (settings$durbin_watson) "Durbin-Watson reported without p-values." else NULL,
      if (settings$vif) paste0("VIF thresholds: ", settings$vif_warn, " (moderate), ", settings$vif_high, " (high).") else NULL,
      if (settings$outliers) paste0("Outliers flagged at |std resid| > ", settings$outlier_z, ".") else NULL,
      if (settings$influence) paste0("Cook's D threshold = ", settings$cook_multiplier, "/n.") else NULL
    )
    paste(parts[!is.null(parts) & nzchar(parts)], collapse = " ")
  } else {
    parts <- c(
      if (settings$normality != "none") {
        paste0("Normality assessed with Shapiro-Wilk (alpha = ", settings$alpha, ").")
      } else {
        "Normality not assessed."
      },
      paste0("Homogeneity tests: ", homogeneity_label, ".")
    )
    paste(parts, collapse = " ")
  }

  list(
    note_default = note_default,
    alpha = settings$alpha,
    homogeneity_tests = homogeneity_label,
    vif_warn = settings$vif_warn,
    vif_high = settings$vif_high,
    outlier_z = settings$outlier_z,
    cook_threshold = paste0(settings$cook_multiplier, "/n")
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
  analysis_default <- resolve_config_value("modules.assumptions.analysis", "auto")
  vars_default <- resolve_config_value("modules.assumptions.vars_default", "numeric")
  normality_default <- resolve_config_value("modules.assumptions.normality", "shapiro")
  homogeneity_default <- resolve_config_value("modules.assumptions.homogeneity", "levene")
  linearity_default <- resolve_config_value("modules.assumptions.linearity", TRUE)
  homoscedasticity_default <- resolve_config_value("modules.assumptions.homoscedasticity", TRUE)
  vif_default <- resolve_config_value("modules.assumptions.vif", TRUE)
  dw_default <- resolve_config_value("modules.assumptions.durbin_watson", TRUE)
  outliers_default <- resolve_config_value("modules.assumptions.outliers", TRUE)
  influence_default <- resolve_config_value("modules.assumptions.influence", TRUE)
  alpha_default <- resolve_config_value("modules.assumptions.alpha", 0.05)
  vif_warn_default <- resolve_config_value("modules.assumptions.vif_warn", 5)
  vif_high_default <- resolve_config_value("modules.assumptions.vif_high", 10)
  outlier_z_default <- resolve_config_value("modules.assumptions.outlier_z", 3)
  cook_multiplier_default <- resolve_config_value("modules.assumptions.cook_multiplier", 4)
  max_shapiro_default <- resolve_config_value("modules.assumptions.max_shapiro_n", 5000)

  digits <- if (!is.null(opts$digits)) as.numeric(opts$digits) else digits_default
  analysis <- normalize_analysis_type(if (!is.null(opts$analysis)) opts$analysis else analysis_default, default = "auto")
  normality <- if (!is.null(opts$normality)) tolower(opts$normality) else normality_default
  homogeneity_tests <- parse_homogeneity_tests(opts$homogeneity, homogeneity_default)
  linearity <- resolve_parse_bool(opts$linearity, default = linearity_default)
  homoscedasticity <- resolve_parse_bool(if (!is.null(opts$homoscedasticity)) opts$homoscedasticity else opts$bp,
                                         default = homoscedasticity_default)
  vif <- resolve_parse_bool(opts$vif, default = vif_default)
  durbin_watson <- resolve_parse_bool(opts$`durbin-watson`, default = dw_default)
  outliers <- resolve_parse_bool(opts$outliers, default = outliers_default)
  influence <- resolve_parse_bool(opts$influence, default = influence_default)
  alpha <- if (!is.null(opts$alpha)) as.numeric(opts$alpha) else alpha_default
  vif_warn <- if (!is.null(opts$`vif-warn`)) as.numeric(opts$`vif-warn`) else vif_warn_default
  vif_high <- if (!is.null(opts$`vif-high`)) as.numeric(opts$`vif-high`) else vif_high_default
  outlier_z <- if (!is.null(opts$`outlier-z`)) as.numeric(opts$`outlier-z`) else outlier_z_default
  cook_multiplier <- if (!is.null(opts$`cook-multiplier`)) as.numeric(opts$`cook-multiplier`) else cook_multiplier_default
  max_shapiro_n <- if (!is.null(opts$`max-shapiro-n`)) as.numeric(opts$`max-shapiro-n`) else max_shapiro_default

  out_dir <- resolve_ensure_out_dir(resolve_default_out())
  df <- resolve_load_dataframe(opts)

  if (analysis == "auto") {
    if (!is.null(opts$ivs) || !is.null(opts$blocks)) {
      analysis <- "regression"
    } else if (!is.null(opts$within) || !is.null(opts$between) || !is.null(opts$dv)) {
      analysis <- "anova"
    } else {
      analysis <- "ttest"
    }
  }

  if (analysis == "regression") {
    homogeneity_tests <- character(0)
  }

  settings <- list(
    alpha = alpha,
    digits = digits,
    vars_default = vars_default,
    normality = normality,
    homogeneity_tests = homogeneity_tests,
    linearity = linearity,
    homoscedasticity = homoscedasticity,
    vif = vif,
    durbin_watson = durbin_watson,
    outliers = outliers,
    influence = influence,
    vif_warn = vif_warn,
    vif_high = vif_high,
    outlier_z = outlier_z,
    cook_multiplier = cook_multiplier,
    max_shapiro_n = max_shapiro_n
  )

  if (normality == "none") {
    settings$normality <- "none"
  }

  if (analysis == "ttest") {
    result <- run_ttest_assumptions(df, opts, settings)
  } else if (analysis == "anova") {
    result <- run_anova_assumptions(df, opts, settings)
  } else if (analysis == "regression") {
    result <- run_regression_assumptions(df, opts, settings)
  } else {
    stop("Unknown analysis type.")
  }

  checks_df <- result$checks
  if (is.null(checks_df) || nrow(checks_df) == 0) stop("No assumptions could be computed.")

  note_tokens <- build_note_tokens(analysis, settings, homogeneity_tests)
  apa_report_path <- file.path(out_dir, "apa_report.md")
  narrative_rows <- build_assumptions_narrative_rows(checks_df, digits)
  apa_text <- paste(vapply(narrative_rows, function(row) row$full_sentence, character(1)), collapse = "\n")
  apa_table <- format_apa_table(checks_df, digits, note_tokens$note_default)

  template_key <- switch(
    analysis,
    ttest = "assumptions.ttest",
    anova = "assumptions.anova",
    regression = "assumptions.regression",
    "assumptions.ttest"
  )
  template_default <- switch(
    analysis,
    ttest = "assumptions/ttest-template.md",
    anova = "assumptions/anova-template.md",
    regression = "assumptions/regression-template.md",
    "assumptions/ttest-template.md"
  )
  template_path <- resolve_get_template_path(template_key, template_default)
  template_meta <- resolve_get_template_meta(template_path)
  table_body <- build_assumptions_table_body(checks_df, digits, template_meta$table)
  template_context <- list(
    tokens = c(
      list(
        table_body = table_body,
        narrative_default = apa_text
      ),
      note_tokens
    ),
    narrative_rows = narrative_rows
  )

  analysis_flags <- list(
    analysis = analysis,
    mode = if (!is.null(result$mode)) result$mode else NULL,
    dv = if (!is.null(opts$dv)) opts$dv else NULL,
    vars = if (!is.null(opts$vars)) opts$vars else NULL,
    x = if (!is.null(opts$x)) opts$x else NULL,
    y = if (!is.null(opts$y)) opts$y else NULL,
    group = if (!is.null(opts$group)) opts$group else NULL,
    between = if (!is.null(opts$between)) opts$between else NULL,
    within = if (!is.null(opts$within)) opts$within else NULL,
    "subject-id" = if (!is.null(opts$`subject-id`)) opts$`subject-id` else NULL,
    ivs = if (!is.null(opts$ivs)) opts$ivs else NULL,
    blocks = if (!is.null(opts$blocks)) opts$blocks else NULL,
    normality = settings$normality,
    homogeneity = if (analysis %in% c("ttest", "anova")) {
      if (length(homogeneity_tests) > 0) paste(homogeneity_tests, collapse = ", ") else "none"
    } else {
      NULL
    },
    linearity = if (analysis == "regression") linearity else NULL,
    homoscedasticity = if (analysis == "regression") homoscedasticity else NULL,
    vif = if (analysis == "regression") vif else NULL,
    "durbin-watson" = if (analysis == "regression") durbin_watson else NULL,
    outliers = if (analysis == "regression") outliers else NULL,
    influence = if (analysis == "regression") influence else NULL,
    alpha = alpha,
    digits = digits
  )

  resolve_append_apa_report(
    apa_report_path,
    "Assumption checks",
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
      module = "assumptions",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(checks_df = checks_df),
      options = list(
        analysis = analysis,
        mode = if (!is.null(result$mode)) result$mode else NULL,
        normality = settings$normality,
        homogeneity = homogeneity_tests,
        linearity = linearity,
        homoscedasticity = homoscedasticity,
        vif = vif,
        durbin_watson = durbin_watson,
        outliers = outliers,
        influence = influence,
        alpha = alpha,
        digits = digits
      ),
      user_prompt = resolve_get_user_prompt(opts)
    )
  }
}

main()
