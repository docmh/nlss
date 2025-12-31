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
  cat("  Rscript assumptions.R --csv data.csv --analysis mixed_models --formula \"score ~ time + (1|id)\"\n")
  cat("  Rscript assumptions.R --csv data.csv --analysis sem --factors \"F1=item1,item2;F2=item3,item4\"\n")
  cat("  Rscript assumptions.R --parquet data.parquet --analysis ttest --vars var1,var2\n")
  cat("  Rscript assumptions.R --interactive\n")
  cat("\n")
  cat("Options:\n")
  cat("  --csv PATH               CSV input file\n")
  cat("  --sav PATH               SPSS .sav input file\n")
  cat("  --sep VALUE              CSV separator (default: ,)\n")
  cat("  --header TRUE/FALSE      CSV header (default: TRUE)\n")
  cat("  --rds PATH               RDS input file (data frame)\n")
  cat("  --rdata PATH             RData input file\n")
  cat("  --parquet PATH           Parquet input file\n")
  cat("  --df NAME                Data frame object name in RData\n")
  cat("  --analysis TYPE          ttest/anova/regression/mixed_models/sem/auto\n")
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
  cat("  --formula TEXT           Mixed model formula (lme4)\n")
  cat("  --fixed LIST             Mixed model fixed effects\n")
  cat("  --random LIST            Mixed model random terms\n")
  cat("  --reml TRUE/FALSE        Mixed model REML estimation\n")
  cat("  --optimizer NAME         Mixed model optimizer\n")
  cat("  --maxfun N               Mixed model optimizer maxfun\n")
  cat("  --model TEXT             SEM model syntax (lavaan)\n")
  cat("  --model-file PATH        SEM model syntax file\n")
  cat("  --paths TEXT             Alias for --model\n")
  cat("  --factors TEXT            CFA builder: F1=item1,item2;F2=item3,item4\n")
  cat("  --covariates LIST        SEM covariates\n")
  cat("  --serial TRUE/FALSE      SEM serial mediation (two mediators)\n")
  cat("  --group-equal LIST       SEM group.equal constraints\n")
  cat("  --invariance LIST        SEM invariance steps\n")
  cat("  --ordered LIST           SEM ordered categorical variables\n")
  cat("  --estimator NAME         SEM estimator\n")
  cat("  --missing TYPE           SEM missing handling\n")
  cat("  --se TYPE                SEM standard errors\n")
  cat("  --ci TYPE                SEM confidence interval type\n")
  cat("  --conf-level VALUE       SEM confidence level\n")
  cat("  --bootstrap TRUE/FALSE   SEM bootstrap standard errors\n")
  cat("  --bootstrap-samples N    SEM bootstrap resamples\n")
  cat("  --std TYPE               SEM standardization\n")
  cat("  --fit LIST               SEM fit indices\n")
  cat("  --r2 TRUE/FALSE          SEM R2 reporting\n")
  cat("  --modindices N           SEM modification index cutoff\n")
  cat("  --residuals TRUE/FALSE   SEM residuals output\n")
  cat("  --normality TYPE         shapiro/none\n")
  cat("  --homogeneity TYPE       levene/bartlett/fligner/f/all/none\n")
  cat("  --linearity TRUE/FALSE   Regression linearity check\n")
  cat("  --homoscedasticity TRUE/FALSE  Breusch-Pagan-style test\n")
  cat("  --vif TRUE/FALSE         VIF multicollinearity check\n")
  cat("  --durbin-watson TRUE/FALSE  Durbin-Watson statistic\n")
  cat("  --outliers TRUE/FALSE    Standardized residual outliers\n")
  cat("  --influence TRUE/FALSE   Cook's distance influence\n")
  cat("  --random-effects TRUE/FALSE   Mixed model random-effects normality\n")
  cat("  --singular TRUE/FALSE    Mixed model singular fit check\n")
  cat("  --convergence TRUE/FALSE Mixed/SEM convergence check\n")
  cat("  --dharma TRUE/FALSE      Mixed model DHARMa diagnostics (optional)\n")
  cat("  --performance TRUE/FALSE Mixed model performance diagnostics (optional)\n")
  cat("  --mardia TRUE/FALSE      SEM multivariate normality (optional)\n")
  cat("  --mahalanobis TRUE/FALSE SEM Mahalanobis outlier check\n")
  cat("  --mahalanobis-alpha VALUE  SEM Mahalanobis alpha (default: 0.001)\n")
  cat("  --collinearity TRUE/FALSE SEM collinearity checks\n")
  cat("  --max-cor VALUE          SEM max |r| threshold\n")
  cat("  --max-kappa VALUE        SEM condition-number threshold\n")
  cat("  --heywood TRUE/FALSE     SEM Heywood case checks\n")
  cat("  --vif-warn VALUE         VIF warning threshold (default: 5)\n")
  cat("  --vif-high VALUE         VIF high threshold (default: 10)\n")
  cat("  --outlier-z VALUE        Outlier z threshold (default: 3)\n")
  cat("  --cook-multiplier VALUE  Cook's D multiplier (default: 4)\n")
  cat("  --max-shapiro-n VALUE    Max n for Shapiro-Wilk (default: 5000)\n")
  cat("  --alpha VALUE            Decision alpha (default: 0.05)\n")
  cat("  --digits N               Rounding digits (default: 2)\n")
  cat("  --template REF           Template path or template key (optional)\n")
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


resolve_get_workspace_out_dir <- function(df) {
  if (exists("get_workspace_out_dir", mode = "function")) {
    return(get("get_workspace_out_dir", mode = "function")(df))
  }
  stop("Missing get_workspace_out_dir. Ensure lib/io.R is sourced.")
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

resolve_render_template_tokens <- function(text, tokens) {
  if (exists("render_template_tokens", mode = "function")) {
    return(get("render_template_tokens", mode = "function")(text, tokens))
  }
  text
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

  analysis_default <- resolve_config_value("modules.assumptions.analysis", "auto")
  analysis_input <- resolve_prompt("Analysis (ttest/anova/regression/mixed_models/sem/auto)", analysis_default)
  analysis_norm <- normalize_analysis_type(analysis_input, default = "auto")
  opts$analysis <- analysis_input

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
  } else if (analysis_norm == "mixed_models") {
    use_formula <- resolve_prompt("Use full formula? (yes/no)", "yes")
    if (tolower(use_formula) %in% c("yes", "y")) {
      opts$formula <- resolve_prompt("Model formula (e.g., score ~ time + (1|id))", "")
    } else {
      opts$dv <- resolve_prompt("Dependent variable", "")
      opts$fixed <- resolve_prompt("Fixed effects (comma-separated)", "")
      opts$random <- resolve_prompt("Random terms (comma-separated; e.g., 1|id,time|id)", "")
    }
  } else if (analysis_norm == "sem") {
    sem_type <- resolve_prompt("SEM analysis (sem/cfa/path/mediation/invariance)", "sem")
    opts$analysis <- sem_type
    sem_type <- tolower(sem_type)
    if (sem_type == "cfa") {
      opts$factors <- resolve_prompt("Factors (F1=item1,item2;F2=item3,item4)", "")
      opts$model <- resolve_prompt("Model syntax (blank to use factors)", "")
    } else if (sem_type == "mediation") {
      opts$x <- resolve_prompt("Predictor (x)", "")
      opts$m <- resolve_prompt("Mediators (comma-separated)", "")
      opts$y <- resolve_prompt("Outcome (y)", "")
      opts$covariates <- resolve_prompt("Covariates (comma-separated, optional)", "")
      opts$serial <- resolve_prompt("Serial mediation TRUE/FALSE", "FALSE")
    } else if (sem_type == "path") {
      opts$dv <- resolve_prompt("Dependent variable", "")
      opts$ivs <- resolve_prompt("Predictors (comma-separated)", "")
      opts$covariates <- resolve_prompt("Covariates (comma-separated, optional)", "")
      opts$model <- resolve_prompt("Model syntax (blank to use dv/ivs)", "")
    } else {
      opts$model <- resolve_prompt("Model syntax", "")
    }

    estimator_default <- resolve_config_value("modules.sem.estimator", "MLR")
    missing_default <- resolve_config_value("modules.sem.missing", "fiml")
    se_default <- resolve_config_value("modules.sem.se", "robust")
    ci_default <- resolve_config_value("modules.sem.ci", "standard")
    bootstrap_default <- resolve_config_value("modules.sem.bootstrap", FALSE)
    bootstrap_samples_default <- resolve_config_value("modules.sem.bootstrap_samples", 5000)
    std_default <- resolve_config_value("modules.sem.std", "std.all")

    opts$estimator <- resolve_prompt("Estimator", estimator_default)
    opts$missing <- resolve_prompt("Missing handling", missing_default)
    opts$se <- resolve_prompt("SE type", se_default)
    opts$ci <- resolve_prompt("CI type", ci_default)
    opts$bootstrap <- resolve_prompt("Bootstrap TRUE/FALSE", ifelse(isTRUE(bootstrap_default), "TRUE", "FALSE"))
    opts$`bootstrap-samples` <- resolve_prompt("Bootstrap samples", as.character(bootstrap_samples_default))
    opts$std <- resolve_prompt("Standardization (none/std.lv/std.all)", std_default)
  }

  normality_default <- resolve_config_value("modules.assumptions.normality", "shapiro")
  homogeneity_default <- resolve_config_value("modules.assumptions.homogeneity", "levene")
  linearity_default <- resolve_config_value("modules.assumptions.linearity", TRUE)
  homoscedasticity_default <- resolve_config_value("modules.assumptions.homoscedasticity", TRUE)
  vif_default <- resolve_config_value("modules.assumptions.vif", TRUE)
  dw_default <- resolve_config_value("modules.assumptions.durbin_watson", TRUE)
  outliers_default <- resolve_config_value("modules.assumptions.outliers", TRUE)
  influence_default <- resolve_config_value("modules.assumptions.influence", TRUE)
  mm_random_default <- resolve_config_value("modules.assumptions.mixed_models.random_effects", TRUE)
  mm_singular_default <- resolve_config_value("modules.assumptions.mixed_models.singular", TRUE)
  mm_conv_default <- resolve_config_value("modules.assumptions.mixed_models.convergence", TRUE)
  mm_dharma_default <- resolve_config_value("modules.assumptions.mixed_models.dharma", FALSE)
  mm_perf_default <- resolve_config_value("modules.assumptions.mixed_models.performance", TRUE)
  mm_reml_default <- resolve_config_value("modules.mixed_models.reml", TRUE)
  mm_optimizer_default <- resolve_config_value("modules.mixed_models.optimizer", "bobyqa")
  mm_maxfun_default <- resolve_config_value("modules.mixed_models.maxfun", 100000)
  sem_mardia_default <- resolve_config_value("modules.assumptions.sem.mardia", TRUE)
  sem_mahal_default <- resolve_config_value("modules.assumptions.sem.mahalanobis", TRUE)
  sem_coll_default <- resolve_config_value("modules.assumptions.sem.collinearity", TRUE)
  sem_heywood_default <- resolve_config_value("modules.assumptions.sem.heywood", TRUE)
  sem_conv_default <- resolve_config_value("modules.assumptions.sem.convergence", TRUE)
  sem_mahal_alpha_default <- resolve_config_value("modules.assumptions.sem.mahalanobis_alpha", 0.001)
  sem_max_cor_default <- resolve_config_value("modules.assumptions.sem.max_cor", 0.9)
  sem_max_kappa_default <- resolve_config_value("modules.assumptions.sem.max_kappa", 30)
  alpha_default <- resolve_config_value("modules.assumptions.alpha", 0.05)
  digits_default <- resolve_config_value("defaults.digits", 2)

  opts$normality <- resolve_prompt("Normality test (shapiro/none)", normality_default)
  if (analysis_norm %in% c("ttest", "anova")) {
    opts$homogeneity <- resolve_prompt("Homogeneity test (levene/bartlett/fligner/f/all/none)", homogeneity_default)
  } else if (analysis_norm == "regression") {
    opts$linearity <- resolve_prompt("Linearity TRUE/FALSE", ifelse(isTRUE(linearity_default), "TRUE", "FALSE"))
    opts$homoscedasticity <- resolve_prompt("Homoscedasticity TRUE/FALSE", ifelse(isTRUE(homoscedasticity_default), "TRUE", "FALSE"))
    opts$vif <- resolve_prompt("VIF TRUE/FALSE", ifelse(isTRUE(vif_default), "TRUE", "FALSE"))
    opts$`durbin-watson` <- resolve_prompt("Durbin-Watson TRUE/FALSE", ifelse(isTRUE(dw_default), "TRUE", "FALSE"))
    opts$outliers <- resolve_prompt("Outliers TRUE/FALSE", ifelse(isTRUE(outliers_default), "TRUE", "FALSE"))
    opts$influence <- resolve_prompt("Influence TRUE/FALSE", ifelse(isTRUE(influence_default), "TRUE", "FALSE"))
  } else if (analysis_norm == "mixed_models") {
    opts$homoscedasticity <- resolve_prompt("Homoscedasticity TRUE/FALSE", ifelse(isTRUE(homoscedasticity_default), "TRUE", "FALSE"))
    opts$outliers <- resolve_prompt("Outliers TRUE/FALSE", ifelse(isTRUE(outliers_default), "TRUE", "FALSE"))
    opts$influence <- resolve_prompt("Influence TRUE/FALSE", ifelse(isTRUE(influence_default), "TRUE", "FALSE"))
    opts$`random-effects` <- resolve_prompt("Random-effects normality TRUE/FALSE", ifelse(isTRUE(mm_random_default), "TRUE", "FALSE"))
    opts$singular <- resolve_prompt("Singular fit TRUE/FALSE", ifelse(isTRUE(mm_singular_default), "TRUE", "FALSE"))
    opts$convergence <- resolve_prompt("Convergence TRUE/FALSE", ifelse(isTRUE(mm_conv_default), "TRUE", "FALSE"))
    opts$dharma <- resolve_prompt("DHARMa TRUE/FALSE", ifelse(isTRUE(mm_dharma_default), "TRUE", "FALSE"))
    opts$performance <- resolve_prompt("performance TRUE/FALSE", ifelse(isTRUE(mm_perf_default), "TRUE", "FALSE"))
    opts$reml <- resolve_prompt("REML TRUE/FALSE", ifelse(isTRUE(mm_reml_default), "TRUE", "FALSE"))
    opts$optimizer <- resolve_prompt("Optimizer", mm_optimizer_default)
    opts$maxfun <- resolve_prompt("Optimizer maxfun", as.character(mm_maxfun_default))
  } else if (analysis_norm == "sem") {
    opts$mardia <- resolve_prompt("Mardia multivariate normality TRUE/FALSE", ifelse(isTRUE(sem_mardia_default), "TRUE", "FALSE"))
    opts$mahalanobis <- resolve_prompt("Mahalanobis outliers TRUE/FALSE", ifelse(isTRUE(sem_mahal_default), "TRUE", "FALSE"))
    opts$`mahalanobis-alpha` <- resolve_prompt("Mahalanobis alpha", as.character(sem_mahal_alpha_default))
    opts$collinearity <- resolve_prompt("Collinearity TRUE/FALSE", ifelse(isTRUE(sem_coll_default), "TRUE", "FALSE"))
    opts$`max-cor` <- resolve_prompt("Max |r| threshold", as.character(sem_max_cor_default))
    opts$`max-kappa` <- resolve_prompt("Condition number threshold", as.character(sem_max_kappa_default))
    opts$heywood <- resolve_prompt("Heywood checks TRUE/FALSE", ifelse(isTRUE(sem_heywood_default), "TRUE", "FALSE"))
    opts$convergence <- resolve_prompt("Convergence TRUE/FALSE", ifelse(isTRUE(sem_conv_default), "TRUE", "FALSE"))
  }

  opts$alpha <- resolve_prompt("Decision alpha", format(alpha_default, trim = TRUE))
  opts$digits <- resolve_prompt("Rounding digits", as.character(digits_default))
  opts$template <- resolve_prompt("Template (path or key; blank for default)", "")
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
  if (val %in% c("mixed_models", "mixed-models", "mixed", "lmm", "lme4")) return("mixed_models")
  if (val %in% c("sem", "cfa", "path", "mediation", "invariance", "structural", "mi")) return("sem")
  default
}

normalize_sem_type <- function(value, default = "sem") {
  val <- if (!is.null(value) && nzchar(value)) tolower(as.character(value)) else tolower(default)
  if (val %in% c("sem", "structural")) return("sem")
  if (val %in% c("cfa", "confirmatory")) return("cfa")
  if (val %in% c("path", "path-analysis", "path_analysis")) return("path")
  if (val %in% c("mediation", "med", "indirect")) return("mediation")
  if (val %in% c("invariance", "measurement-invariance", "mi")) return("invariance")
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

normalize_reml <- function(value, default = TRUE) {
  resolve_parse_bool(value, default = default)
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

is_pkg_available <- function(name) {
  requireNamespace(name, quietly = TRUE)
}

extract_first_numeric <- function(value) {
  if (is.null(value)) return(NA_real_)
  if (is.numeric(value)) return(as.numeric(value[1]))
  if (is.character(value)) return(suppressWarnings(as.numeric(value[1])))
  if (is.list(value) && length(value) > 0) return(extract_first_numeric(value[[1]]))
  NA_real_
}

extract_named_numeric <- function(obj, keys) {
  for (key in keys) {
    if (!is.null(obj[[key]])) {
      val <- extract_first_numeric(obj[[key]])
      if (!is.na(val)) return(val)
    }
  }
  NA_real_
}

extract_test_values <- function(test) {
  if (is.null(test)) {
    return(list(stat = NA_real_, p = NA_real_, df1 = NA_real_, df2 = NA_real_, note = ""))
  }
  if (inherits(test, "htest")) {
    stat <- extract_first_numeric(test$statistic)
    p <- extract_first_numeric(test$p.value)
    df <- test$parameter
    df1 <- NA_real_
    df2 <- NA_real_
    if (!is.null(df)) {
      df_vals <- as.numeric(df)
      if (length(df_vals) > 0) df1 <- df_vals[1]
      if (length(df_vals) > 1) df2 <- df_vals[2]
    }
    note <- if (!is.null(test$method)) as.character(test$method) else ""
    return(list(stat = stat, p = p, df1 = df1, df2 = df2, note = note))
  }
  if (is.data.frame(test) && nrow(test) > 0) {
    row <- test[1, , drop = FALSE]
    stat <- extract_named_numeric(row, c("statistic", "stat", "chisq", "chi.square", "t", "z"))
    p <- extract_named_numeric(row, c("p", "p.value", "p_value", "pval"))
    df1 <- extract_named_numeric(row, c("df", "df1", "df_1"))
    df2 <- extract_named_numeric(row, c("df2", "df_2"))
    note <- if ("method" %in% names(row)) as.character(row$method[1]) else ""
    return(list(stat = stat, p = p, df1 = df1, df2 = df2, note = note))
  }
  if (is.list(test)) {
    stat <- extract_named_numeric(test, c("statistic", "stat", "chisq", "chi.square", "t", "z"))
    p <- extract_named_numeric(test, c("p.value", "p", "p_value", "pval"))
    df1 <- extract_named_numeric(test, c("df", "df1", "df_1"))
    df2 <- extract_named_numeric(test, c("df2", "df_2"))
    note <- ""
    if (!is.null(test$method)) note <- as.character(test$method)
    if (!nzchar(note) && !is.null(test$note)) note <- as.character(test$note)
    return(list(stat = stat, p = p, df1 = df1, df2 = df2, note = note))
  }
  list(stat = NA_real_, p = NA_real_, df1 = NA_real_, df2 = NA_real_, note = "")
}

calc_abs_resid_cor <- function(resid, fitted_vals) {
  if (length(resid) < 3 || length(fitted_vals) < 3) return(NULL)
  test <- tryCatch(cor.test(abs(resid), fitted_vals), error = function(e) NULL)
  if (is.null(test)) return(NULL)
  list(stat = extract_first_numeric(test$estimate), p = test$p.value, df1 = extract_first_numeric(test$parameter))
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

decision_from_threshold <- function(value, threshold) {
  if (is.na(value)) return("")
  if (value >= threshold) "flag" else "ok"
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

format_nlss_table <- function(checks_df, digits, note_text) {
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

run_mixed_models_assumptions <- function(df, opts, settings) {
  if (!is_pkg_available("lme4")) {
    stop("Mixed models require the 'lme4' package.")
  }
  rows <- list()
  formula_text <- ""
  if (!is.null(opts$formula) && !is.logical(opts$formula)) {
    formula_text <- as.character(opts$formula)
  }
  dv <- if (!is.null(opts$dv)) as.character(opts$dv) else ""
  fixed_terms <- resolve_parse_list(opts$fixed)
  random_terms_raw <- resolve_parse_list(opts$random)
  random_terms <- normalize_random_terms(random_terms_raw)

  if (nzchar(formula_text)) {
    formula <- tryCatch(as.formula(formula_text), error = function(e) NULL)
    if (is.null(formula)) stop("Invalid mixed model formula.")
  } else {
    if (!nzchar(dv)) stop("Mixed model checks require --formula or --dv.")
    if (length(random_terms) == 0) stop("Mixed model checks require --random or random effects in --formula.")
    formula <- build_model_formula(dv, fixed_terms, random_terms)
    formula_text <- paste(deparse(formula), collapse = " ")
  }

  random_terms_in_formula <- extract_random_terms_from_formula(formula_text)
  if (length(random_terms_in_formula) == 0) stop("Mixed model checks require at least one random effect term.")
  if (!nzchar(dv)) {
    dv_vars <- all.vars(formula[[2]])
    if (length(dv_vars) > 0) dv <- dv_vars[1]
  }

  vars <- unique(all.vars(formula))
  missing <- setdiff(vars, names(df))
  if (length(missing) > 0) stop(paste("Unknown variables:", paste(missing, collapse = ", ")))
  model_df <- df[, vars, drop = FALSE]
  complete <- complete.cases(model_df)
  model_df <- model_df[complete, , drop = FALSE]
  if (nrow(model_df) == 0) stop("No complete cases available for mixed-model checks.")
  if (!nzchar(dv) || !(dv %in% names(model_df))) stop("Dependent variable not found.")
  if (!is.numeric(model_df[[dv]])) stop("Dependent variable must be numeric for mixed-model checks.")
  model_df <- coerce_model_factors(model_df, vars, dv)

  control <- build_lmer_control(settings$optimizer, settings$maxfun)
  fit <- tryCatch(
    if (is.null(control)) {
      lme4::lmer(formula, data = model_df, REML = settings$reml)
    } else {
      lme4::lmer(formula, data = model_df, REML = settings$reml, control = control)
    },
    error = function(e) NULL
  )
  if (is.null(fit)) stop("Mixed model fit failed.")

  resid_vals <- tryCatch(residuals(fit), error = function(e) NULL)
  fitted_vals <- tryCatch(fitted(fit), error = function(e) NULL)
  n <- if (!is.null(resid_vals)) length(resid_vals) else NA_real_
  model_label <- "Mixed"

  if (settings$convergence) {
    conv_note <- ""
    conv_msgs <- tryCatch(fit@optinfo$conv$lme4$messages, error = function(e) NULL)
    if (!is.null(conv_msgs) && length(conv_msgs) > 0) conv_note <- paste(conv_msgs, collapse = "; ")
    decision <- ifelse(nzchar(conv_note), "flag", "ok")
    rows[[length(rows) + 1]] <- make_check_row(
      analysis_type = "mixed_models",
      model = model_label,
      assumption = "Convergence",
      test = "lme4",
      target = "Model",
      group = "",
      statistic = NA_real_,
      df1 = NA_real_,
      df2 = NA_real_,
      p = NA_real_,
      value = NA_real_,
      n = n,
      decision = decision,
      note = conv_note
    )
  }

  if (settings$singular) {
    singular <- tryCatch(lme4::isSingular(fit), error = function(e) NA)
    decision <- ifelse(isTRUE(singular), "flag", "ok")
    note <- ifelse(isTRUE(singular), "Singular fit detected.", "")
    rows[[length(rows) + 1]] <- make_check_row(
      analysis_type = "mixed_models",
      model = model_label,
      assumption = "Singularity",
      test = "lme4::isSingular",
      target = "Model",
      group = "",
      statistic = NA_real_,
      df1 = NA_real_,
      df2 = NA_real_,
      p = NA_real_,
      value = NA_real_,
      n = n,
      decision = decision,
      note = note
    )
  }

  if (settings$normality != "none" && !is.null(resid_vals)) {
    shapiro <- safe_shapiro(resid_vals, settings$max_shapiro_n)
    decision <- decision_from_p(shapiro$p, settings$alpha)
    rows[[length(rows) + 1]] <- make_check_row(
      analysis_type = "mixed_models",
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

  if (settings$random_effects) {
    ranefs <- tryCatch(lme4::ranef(fit, condVar = FALSE), error = function(e) NULL)
    if (!is.null(ranefs) && length(ranefs) > 0) {
      for (grp in names(ranefs)) {
        re_df <- ranefs[[grp]]
        if (!is.data.frame(re_df) || nrow(re_df) == 0) next
        for (term in names(re_df)) {
          vals <- re_df[[term]]
          shapiro <- safe_shapiro(vals, settings$max_shapiro_n)
          decision <- decision_from_p(shapiro$p, settings$alpha)
          rows[[length(rows) + 1]] <- make_check_row(
            analysis_type = "mixed_models",
            model = model_label,
            assumption = "Random-effects normality",
            test = "Shapiro-Wilk",
            target = term,
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
      }
    }
  }

  if (settings$homoscedasticity && !is.null(resid_vals) && !is.null(fitted_vals)) {
    cor_res <- calc_abs_resid_cor(resid_vals, fitted_vals)
    if (!is.null(cor_res)) {
      decision <- decision_from_p(cor_res$p, settings$alpha)
      rows[[length(rows) + 1]] <- make_check_row(
        analysis_type = "mixed_models",
        model = model_label,
        assumption = "Homoscedasticity",
        test = "Abs resid vs fitted",
        target = "Residuals",
        group = "",
        statistic = cor_res$stat,
        df1 = cor_res$df1,
        df2 = NA_real_,
        p = cor_res$p,
        value = NA_real_,
        n = n,
        decision = decision,
        note = ""
      )
    }
    if (isTRUE(settings$performance) && isTRUE(settings$packages$performance)) {
      perf_test <- tryCatch(performance::check_heteroscedasticity(fit), error = function(e) NULL)
      perf_vals <- extract_test_values(perf_test)
      if (!is.na(perf_vals$stat) || !is.na(perf_vals$p)) {
        decision <- decision_from_p(perf_vals$p, settings$alpha)
        rows[[length(rows) + 1]] <- make_check_row(
          analysis_type = "mixed_models",
          model = model_label,
          assumption = "Homoscedasticity",
          test = "performance::check_heteroscedasticity",
          target = "Residuals",
          group = "",
          statistic = perf_vals$stat,
          df1 = perf_vals$df1,
          df2 = perf_vals$df2,
          p = perf_vals$p,
          value = NA_real_,
          n = n,
          decision = decision,
          note = perf_vals$note
        )
      }
    }
  }

  if (settings$outliers && !is.null(resid_vals)) {
    resid_sd <- sd(resid_vals, na.rm = TRUE)
    if (!is.na(resid_sd) && resid_sd > 0) {
      std_res <- resid_vals / resid_sd
      max_abs <- max(abs(std_res), na.rm = TRUE)
      count <- sum(abs(std_res) > settings$outlier_z, na.rm = TRUE)
      decision <- decision_from_count(count)
      note <- paste0("|std resid| > ", settings$outlier_z, ": ", count)
      rows[[length(rows) + 1]] <- make_check_row(
        analysis_type = "mixed_models",
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

  if (settings$influence && isTRUE(settings$packages$influence)) {
    group_list <- tryCatch(lme4::getME(fit, "flist"), error = function(e) NULL)
    if (!is.null(group_list) && length(group_list) > 0) {
      for (grp in names(group_list)) {
        infl <- tryCatch(influence.ME::influence(fit, group = grp), error = function(e) NULL)
        if (is.null(infl)) next
        cooks <- tryCatch(influence.ME::cooks.distance(infl), error = function(e) NULL)
        if (is.null(cooks)) next
        max_cook <- max(cooks, na.rm = TRUE)
        threshold <- settings$cook_multiplier / length(cooks)
        count <- sum(cooks > threshold, na.rm = TRUE)
        decision <- decision_from_count(count)
        note <- paste0("Cook's D > ", format_stat(threshold, settings$digits), ": ", count)
        rows[[length(rows) + 1]] <- make_check_row(
          analysis_type = "mixed_models",
          model = model_label,
          assumption = "Influence",
          test = "Cook's distance (cluster)",
          target = grp,
          group = "",
          statistic = NA_real_,
          df1 = NA_real_,
          df2 = NA_real_,
          p = NA_real_,
          value = max_cook,
          n = length(cooks),
          decision = decision,
          note = note
        )
      }
    }
  }

  if (settings$dharma && isTRUE(settings$packages$dharma)) {
    sim <- tryCatch(DHARMa::simulateResiduals(fit, plot = FALSE), error = function(e) NULL)
    if (!is.null(sim)) {
      uni <- tryCatch(DHARMa::testUniformity(sim), error = function(e) NULL)
      uni_vals <- extract_test_values(uni)
      if (!is.na(uni_vals$stat) || !is.na(uni_vals$p)) {
        decision <- decision_from_p(uni_vals$p, settings$alpha)
        rows[[length(rows) + 1]] <- make_check_row(
          analysis_type = "mixed_models",
          model = model_label,
          assumption = "Residuals",
          test = "DHARMa uniformity",
          target = "Residuals",
          group = "",
          statistic = uni_vals$stat,
          df1 = uni_vals$df1,
          df2 = uni_vals$df2,
          p = uni_vals$p,
          value = NA_real_,
          n = n,
          decision = decision,
          note = uni_vals$note
        )
      }
      disp <- tryCatch(DHARMa::testDispersion(sim), error = function(e) NULL)
      disp_vals <- extract_test_values(disp)
      if (!is.na(disp_vals$stat) || !is.na(disp_vals$p)) {
        decision <- decision_from_p(disp_vals$p, settings$alpha)
        rows[[length(rows) + 1]] <- make_check_row(
          analysis_type = "mixed_models",
          model = model_label,
          assumption = "Residuals",
          test = "DHARMa dispersion",
          target = "Residuals",
          group = "",
          statistic = disp_vals$stat,
          df1 = disp_vals$df1,
          df2 = disp_vals$df2,
          p = disp_vals$p,
          value = NA_real_,
          n = n,
          decision = decision,
          note = disp_vals$note
        )
      }
    }
  }

  checks_df <- if (length(rows) > 0) do.call(rbind, rows) else data.frame()
  list(checks = checks_df, model = formula_text)
}

run_sem_assumptions <- function(df, opts, settings) {
  if (!is_pkg_available("lavaan")) {
    stop("SEM assumptions require the 'lavaan' package.")
  }
  rows <- list()
  sem_type <- settings$sem_type
  model_text <- ""
  if (!is.null(opts[["model"]]) && !is.logical(opts[["model"]]) && nzchar(opts[["model"]])) {
    model_text <- as.character(opts[["model"]])
  }
  if (!nzchar(model_text) && !is.null(opts[["paths"]]) && !is.logical(opts[["paths"]]) && nzchar(opts[["paths"]])) {
    model_text <- as.character(opts[["paths"]])
  }
  if (!nzchar(model_text) && !is.null(opts[["model-file"]]) && !is.logical(opts[["model-file"]]) && nzchar(opts[["model-file"]])) {
    model_path <- as.character(opts[["model-file"]])
    if (!file.exists(model_path)) stop(paste0("Model file not found: ", model_path))
    model_text <- paste(readLines(model_path, warn = FALSE), collapse = "\n")
  }

  factors_text <- if (!is.null(opts$factors) && !is.logical(opts$factors)) as.character(opts$factors) else ""
  covariates <- resolve_parse_list(opts$covariates)

  if (!nzchar(model_text)) {
    if (sem_type == "cfa" && nzchar(factors_text)) {
      factors <- parse_factor_spec(factors_text)
      model_text <- build_cfa_model(factors)
    } else if (sem_type == "mediation") {
      x <- if (!is.null(opts$x)) as.character(opts$x) else ""
      mediators <- resolve_parse_list(opts$m)
      y <- if (!is.null(opts$y)) as.character(opts$y) else ""
      serial <- resolve_parse_bool(opts$serial, default = FALSE)
      model_text <- build_mediation_model(x, mediators, y, covariates, serial)
    } else if (sem_type == "path") {
      dv <- if (!is.null(opts$dv)) as.character(opts$dv) else ""
      ivs <- resolve_parse_list(opts$ivs)
      model_text <- build_path_model(dv, ivs, covariates)
    } else if (nzchar(factors_text)) {
      factors <- parse_factor_spec(factors_text)
      model_text <- build_cfa_model(factors)
    }
  }

  if (!nzchar(model_text)) {
    stop("Model syntax is required. Use --model, --model-file, or a builder option.")
  }
  model_syntax <- normalize_model_syntax(model_text)
  model_vars <- extract_model_vars(model_syntax)
  if (length(model_vars) == 0) {
    tokens <- regmatches(model_syntax, gregexpr("[A-Za-z\\.][A-Za-z0-9_\\.]*", model_syntax, perl = TRUE))[[1]]
    tokens <- unique(tokens)
    tokens <- tokens[nzchar(tokens)]
    if (length(tokens) > 0) {
      model_vars <- intersect(tokens, names(df))
    }
  }
  if (length(model_vars) == 0 && nzchar(factors_text)) {
    factors <- parse_factor_spec(factors_text)
    model_vars <- unique(unlist(factors))
  }
  if (length(model_vars) == 0) stop("No model variables could be identified.")
  missing_vars <- setdiff(model_vars, names(df))
  if (length(missing_vars) > 0) stop(paste("Missing variables:", paste(missing_vars, collapse = ", ")))

  ordered_vars <- resolve_parse_list(opts$ordered)
  if (length(ordered_vars) > 0) {
    missing_ordered <- setdiff(ordered_vars, names(df))
    if (length(missing_ordered) > 0) {
      stop(paste("Unknown ordered variables:", paste(missing_ordered, collapse = ", ")))
    }
    ordered_vars <- intersect(ordered_vars, model_vars)
  }

  cont_vars <- setdiff(model_vars, ordered_vars)
  cont_vars <- cont_vars[sapply(df[cont_vars], is.numeric)]
  cont_data <- if (length(cont_vars) > 0) df[, cont_vars, drop = FALSE] else NULL
  cont_complete <- if (!is.null(cont_data)) cont_data[complete.cases(cont_data), , drop = FALSE] else NULL

  if (settings$normality != "none") {
    for (var in cont_vars) {
      vals <- df[[var]]
      vals <- vals[!is.na(vals)]
      shapiro <- safe_shapiro(vals, settings$max_shapiro_n)
      decision <- decision_from_p(shapiro$p, settings$alpha)
      rows[[length(rows) + 1]] <- make_check_row(
        analysis_type = "sem",
        model = toupper(sem_type),
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

  mardia_rows <- 0
  mardia_note <- NULL
  if (settings$mardia && isTRUE(settings$packages$mvn)) {
    if (!is.null(cont_complete) && nrow(cont_complete) > 2 && ncol(cont_complete) > 1) {
      mvn_res <- tryCatch(
        MVN::mvn(cont_complete, mvnTest = "mardia", multivariatePlot = "none"),
        error = function(e) NULL
      )
      if (!is.null(mvn_res) && !is.null(mvn_res$multivariateNormality)) {
        mvn_tbl <- mvn_res$multivariateNormality
        if (!is.data.frame(mvn_tbl)) {
          mvn_tbl <- tryCatch(as.data.frame(mvn_tbl, stringsAsFactors = FALSE), error = function(e) NULL)
        }
        if (!is.null(mvn_tbl)) {
          label_col <- NULL
          if ("Test" %in% names(mvn_tbl)) label_col <- "Test"
          if (is.null(label_col) && "test" %in% names(mvn_tbl)) label_col <- "test"
          labels <- if (!is.null(label_col)) as.character(mvn_tbl[[label_col]]) else rownames(mvn_tbl)
          if (is.null(labels) || length(labels) == 0) labels <- rep("", nrow(mvn_tbl))

          stat_col <- NULL
          if ("Statistic" %in% names(mvn_tbl)) stat_col <- "Statistic"
          if (is.null(stat_col) && "statistic" %in% names(mvn_tbl)) stat_col <- "statistic"
          p_col <- NULL
          if ("p value" %in% names(mvn_tbl)) p_col <- "p value"
          if (is.null(p_col) && "p.value" %in% names(mvn_tbl)) p_col <- "p.value"
          if (is.null(p_col) && "p" %in% names(mvn_tbl)) p_col <- "p"

          if (!is.null(stat_col) && !is.null(p_col)) {
            for (i in seq_along(labels)) {
              label <- labels[i]
              if (is.na(label)) label <- ""
              label_lower <- tolower(label)
              is_mardia <- grepl("mardia", label_lower) ||
                grepl("skew", label_lower) || grepl("kurt", label_lower) ||
                (nrow(mvn_tbl) <= 2 && !nzchar(label))
              if (!is_mardia) next
              stat_val <- extract_first_numeric(mvn_tbl[[stat_col]][i])
              p_val <- extract_first_numeric(mvn_tbl[[p_col]][i])
              test_label <- if (grepl("skew", label_lower)) {
                "Mardia (skew)"
              } else if (grepl("kurt", label_lower)) {
                "Mardia (kurtosis)"
              } else {
                "Mardia"
              }
              decision <- decision_from_p(p_val, settings$alpha)
              rows[[length(rows) + 1]] <- make_check_row(
                analysis_type = "sem",
                model = toupper(sem_type),
                assumption = "Multivariate normality",
                test = test_label,
                target = "Indicators",
                group = "",
                statistic = stat_val,
                df1 = NA_real_,
                df2 = NA_real_,
                p = p_val,
                value = NA_real_,
                n = nrow(cont_complete),
                decision = decision,
                note = ""
              )
              mardia_rows <- mardia_rows + 1
            }
          } else {
            mardia_note <- "Mardia results missing statistic or p-value columns."
          }
        } else {
          mardia_note <- "Mardia results could not be parsed."
        }
      } else {
        mardia_note <- "Mardia test failed or returned no results."
      }
    } else if (!is.null(cont_complete)) {
      mardia_note <- "Mardia not assessed (insufficient complete data)."
    } else {
      mardia_note <- "Mardia not assessed (no continuous indicators)."
    }

    if (mardia_rows == 0 && !is.null(mardia_note)) {
      rows[[length(rows) + 1]] <- make_check_row(
        analysis_type = "sem",
        model = toupper(sem_type),
        assumption = "Multivariate normality",
        test = "Mardia",
        target = "Indicators",
        group = "",
        statistic = NA_real_,
        df1 = NA_real_,
        df2 = NA_real_,
        p = NA_real_,
        value = NA_real_,
        n = if (!is.null(cont_complete)) nrow(cont_complete) else NA_real_,
        decision = "",
        note = mardia_note
      )
    }
  }

  if (settings$mahalanobis && !is.null(cont_complete)) {
    if (nrow(cont_complete) > 2 && ncol(cont_complete) > 1) {
      cov_mat <- tryCatch(cov(cont_complete), error = function(e) NULL)
      if (!is.null(cov_mat)) {
        center <- colMeans(cont_complete)
        d2 <- tryCatch(mahalanobis(cont_complete, center, cov_mat), error = function(e) NULL)
        if (!is.null(d2)) {
          cutoff <- qchisq(1 - settings$mahalanobis_alpha, df = ncol(cont_complete))
          count <- sum(d2 > cutoff, na.rm = TRUE)
          max_dist <- max(d2, na.rm = TRUE)
          decision <- decision_from_count(count)
          note <- paste0("Chi-square cutoff (alpha = ", settings$mahalanobis_alpha, "): ", format_stat(cutoff, settings$digits))
          rows[[length(rows) + 1]] <- make_check_row(
            analysis_type = "sem",
            model = toupper(sem_type),
            assumption = "Outliers",
            test = "Mahalanobis distance",
            target = "Indicators",
            group = "",
            statistic = NA_real_,
            df1 = NA_real_,
            df2 = NA_real_,
            p = NA_real_,
            value = max_dist,
            n = nrow(cont_complete),
            decision = decision,
            note = note
          )
        }
      }
    }
  }

  if (settings$collinearity && !is.null(cont_complete) && ncol(cont_complete) > 1) {
    cor_mat <- tryCatch(cor(cont_complete, use = "pairwise.complete.obs"), error = function(e) NULL)
    if (!is.null(cor_mat)) {
      max_cor <- max(abs(cor_mat[upper.tri(cor_mat)]), na.rm = TRUE)
      if (!is.finite(max_cor)) max_cor <- NA_real_
      decision <- decision_from_threshold(max_cor, settings$max_cor)
      note <- paste0("Threshold = ", settings$max_cor)
      rows[[length(rows) + 1]] <- make_check_row(
        analysis_type = "sem",
        model = toupper(sem_type),
        assumption = "Multicollinearity",
        test = "Max |r|",
        target = "Indicators",
        group = "",
        statistic = NA_real_,
        df1 = NA_real_,
        df2 = NA_real_,
        p = NA_real_,
        value = max_cor,
        n = nrow(cont_complete),
        decision = decision,
        note = note
      )
      kappa_val <- tryCatch(kappa(cor_mat), error = function(e) NA_real_)
      decision <- decision_from_threshold(kappa_val, settings$max_kappa)
      note <- paste0("Threshold = ", settings$max_kappa)
      rows[[length(rows) + 1]] <- make_check_row(
        analysis_type = "sem",
        model = toupper(sem_type),
        assumption = "Multicollinearity",
        test = "Condition number",
        target = "Indicators",
        group = "",
        statistic = NA_real_,
        df1 = NA_real_,
        df2 = NA_real_,
        p = NA_real_,
        value = kappa_val,
        n = nrow(cont_complete),
        decision = decision,
        note = note
      )
    }
  }

  fit <- NULL
  if (settings$convergence || settings$heywood) {
    estimator <- settings$estimator
    missing <- settings$missing
    se <- settings$se
    bootstrap_samples <- settings$bootstrap_samples
    group_var <- if (!is.null(opts$group)) as.character(opts$group) else ""
    if (nzchar(group_var) && !(group_var %in% names(df))) {
      stop(paste0("Grouping variable not found: ", group_var))
    }
    group_equal <- resolve_parse_list(opts$`group-equal`)
    fit_type <- if (sem_type %in% c("cfa", "invariance")) "cfa" else "sem"
    fit <- tryCatch(
      fit_sem_model(fit_type, model_syntax, df, estimator, missing, se, bootstrap_samples, ordered_vars, group_var, group_equal),
      error = function(e) NULL
    )
    if (is.null(fit)) stop("SEM model failed.")
  }

  if (!is.null(fit) && settings$convergence) {
    converged <- tryCatch(lavaan::lavInspect(fit, "converged"), error = function(e) NA)
    decision <- ifelse(isTRUE(converged), "ok", "flag")
    note <- ifelse(isTRUE(converged), "", "Model did not converge.")
    rows[[length(rows) + 1]] <- make_check_row(
      analysis_type = "sem",
      model = toupper(sem_type),
      assumption = "Convergence",
      test = "lavaan",
      target = "Model",
      group = "",
      statistic = NA_real_,
      df1 = NA_real_,
      df2 = NA_real_,
      p = NA_real_,
      value = NA_real_,
      n = NA_real_,
      decision = decision,
      note = note
    )
  }

  if (!is.null(fit) && settings$heywood) {
    pe <- tryCatch(lavaan::parameterEstimates(fit, standardized = TRUE), error = function(e) NULL)
    if (!is.null(pe) && nrow(pe) > 0) {
      var_rows <- pe[pe$op == "~~" & pe$lhs == pe$rhs, , drop = FALSE]
      neg_count <- sum(var_rows$est < 0, na.rm = TRUE)
      decision <- decision_from_count(neg_count)
      note <- paste0("Negative variances: ", neg_count)
      rows[[length(rows) + 1]] <- make_check_row(
        analysis_type = "sem",
        model = toupper(sem_type),
        assumption = "Heywood",
        test = "Negative variances",
        target = "Model",
        group = "",
        statistic = NA_real_,
        df1 = NA_real_,
        df2 = NA_real_,
        p = NA_real_,
        value = neg_count,
        n = NA_real_,
        decision = decision,
        note = note
      )
      if ("std.all" %in% names(pe)) {
        load_rows <- pe[pe$op == "=~", , drop = FALSE]
        count <- sum(abs(load_rows$std.all) > 1, na.rm = TRUE)
        decision <- decision_from_count(count)
        note <- paste0("|std.all| > 1: ", count)
        rows[[length(rows) + 1]] <- make_check_row(
          analysis_type = "sem",
          model = toupper(sem_type),
          assumption = "Heywood",
          test = "Std. loading > 1",
          target = "Model",
          group = "",
          statistic = NA_real_,
          df1 = NA_real_,
          df2 = NA_real_,
          p = NA_real_,
          value = count,
          n = NA_real_,
          decision = decision,
          note = note
        )
      }
    }
  }

  checks_df <- if (length(rows) > 0) do.call(rbind, rows) else data.frame()
  list(checks = checks_df, model = model_syntax, sem_type = sem_type, vars = model_vars)
}

build_note_tokens <- function(analysis_type, settings, homogeneity_tests, checks_df = NULL) {
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

  if (analysis_type == "regression") {
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
    note_default <- paste(parts[!is.null(parts) & nzchar(parts)], collapse = " ")
  } else if (analysis_type == "mixed_models") {
    parts <- c(
      if (settings$normality != "none") {
        paste0("Residual normality assessed with Shapiro-Wilk (alpha = ", settings$alpha, ").")
      } else {
        "Residual normality not assessed."
      },
      if (settings$random_effects) "Random-effects normality assessed with Shapiro-Wilk." else "Random-effects normality not assessed.",
      if (settings$homoscedasticity) "Homoscedasticity assessed with residual-fitted correlation." else "Homoscedasticity not assessed.",
      if (isTRUE(settings$performance) && isTRUE(settings$packages$performance)) {
        "Additional heteroscedasticity check via performance::check_heteroscedasticity."
      } else {
        NULL
      },
      if (settings$singular) "Singular fit flagged via lme4::isSingular." else NULL,
      if (settings$convergence) "Convergence warnings reported from the optimizer." else NULL,
      if (settings$outliers) paste0("Outliers flagged at |std resid| > ", settings$outlier_z, ".") else NULL,
      if (settings$influence) paste0("Cook's D threshold = ", settings$cook_multiplier, "/n (clusters when available).") else NULL,
      if (settings$dharma && isTRUE(settings$packages$dharma)) "DHARMa residual diagnostics included when available." else NULL,
      if (settings$dharma && !isTRUE(settings$packages$dharma)) "DHARMa requested but package not available." else NULL,
      if (settings$influence && !isTRUE(settings$packages$influence)) "Influence checks require influence.ME (not available)." else NULL
    )
    note_default <- paste(parts[!is.null(parts) & nzchar(parts)], collapse = " ")
  } else if (analysis_type == "sem") {
    mardia_present <- FALSE
    mardia_failed <- FALSE
    if (!is.null(checks_df) && nrow(checks_df) > 0) {
      mardia_rows <- checks_df[checks_df$assumption == "Multivariate normality", , drop = FALSE]
      if (nrow(mardia_rows) > 0) {
        mardia_present <- TRUE
        if (all(is.na(mardia_rows$p)) || any(nzchar(mardia_rows$note))) {
          mardia_failed <- TRUE
        }
      }
    }
    parts <- c(
      if (settings$normality != "none") {
        paste0("Univariate normality assessed with Shapiro-Wilk (alpha = ", settings$alpha, ").")
      } else {
        "Univariate normality not assessed."
      },
      if (settings$mardia && isTRUE(settings$packages$mvn) && mardia_present && !mardia_failed) {
        "Multivariate normality assessed with Mardia."
      } else if (settings$mardia && isTRUE(settings$packages$mvn) && mardia_present && mardia_failed) {
        "Mardia test attempted but results were unavailable."
      } else if (settings$mardia && isTRUE(settings$packages$mvn) && !mardia_present) {
        "Mardia test requested but no results were returned."
      } else if (settings$mardia) {
        "Mardia test not available (MVN missing)."
      } else {
        "Multivariate normality not assessed."
      },
      if (settings$mahalanobis) paste0("Mahalanobis outliers flagged at alpha = ", settings$mahalanobis_alpha, ".") else NULL,
      if (settings$collinearity) paste0("Collinearity thresholds: max |r| = ", settings$max_cor, ", condition number = ", settings$max_kappa, ".") else NULL,
      if (settings$heywood) "Heywood cases flagged (negative variances or |std loading| > 1)." else NULL,
      if (settings$convergence) "Convergence reported from lavaan." else NULL
    )
    note_default <- paste(parts[!is.null(parts) & nzchar(parts)], collapse = " ")
  } else {
    parts <- c(
      if (settings$normality != "none") {
        paste0("Normality assessed with Shapiro-Wilk (alpha = ", settings$alpha, ").")
      } else {
        "Normality not assessed."
      },
      paste0("Homogeneity tests: ", homogeneity_label, ".")
    )
    note_default <- paste(parts, collapse = " ")
  }

  list(
    note_default = note_default,
    alpha = settings$alpha,
    homogeneity_tests = homogeneity_label,
    vif_warn = settings$vif_warn,
    vif_high = settings$vif_high,
    outlier_z = settings$outlier_z,
    cook_threshold = paste0(settings$cook_multiplier, "/n"),
    mahalanobis_alpha = if (!is.null(settings$mahalanobis_alpha)) settings$mahalanobis_alpha else NA_real_,
    max_cor = if (!is.null(settings$max_cor)) settings$max_cor else NA_real_,
    max_kappa = if (!is.null(settings$max_kappa)) settings$max_kappa else NA_real_
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
  mm_random_default <- resolve_config_value("modules.assumptions.mixed_models.random_effects", TRUE)
  mm_singular_default <- resolve_config_value("modules.assumptions.mixed_models.singular", TRUE)
  mm_conv_default <- resolve_config_value("modules.assumptions.mixed_models.convergence", TRUE)
  mm_dharma_default <- resolve_config_value("modules.assumptions.mixed_models.dharma", FALSE)
  mm_performance_default <- resolve_config_value("modules.assumptions.mixed_models.performance", TRUE)
  sem_mardia_default <- resolve_config_value("modules.assumptions.sem.mardia", TRUE)
  sem_mahal_default <- resolve_config_value("modules.assumptions.sem.mahalanobis", TRUE)
  sem_mahal_alpha_default <- resolve_config_value("modules.assumptions.sem.mahalanobis_alpha", 0.001)
  sem_coll_default <- resolve_config_value("modules.assumptions.sem.collinearity", TRUE)
  sem_max_cor_default <- resolve_config_value("modules.assumptions.sem.max_cor", 0.9)
  sem_max_kappa_default <- resolve_config_value("modules.assumptions.sem.max_kappa", 30)
  sem_heywood_default <- resolve_config_value("modules.assumptions.sem.heywood", TRUE)
  sem_conv_default <- resolve_config_value("modules.assumptions.sem.convergence", TRUE)
  mm_reml_default <- resolve_config_value("modules.mixed_models.reml", TRUE)
  mm_optimizer_default <- resolve_config_value("modules.mixed_models.optimizer", "bobyqa")
  mm_maxfun_default <- resolve_config_value("modules.mixed_models.maxfun", 100000)
  sem_analysis_default <- resolve_config_value("modules.sem.analysis", "sem")
  sem_estimator_default <- resolve_config_value("modules.sem.estimator", "MLR")
  sem_missing_default <- resolve_config_value("modules.sem.missing", "fiml")
  sem_se_default <- resolve_config_value("modules.sem.se", "robust")
  sem_ci_default <- resolve_config_value("modules.sem.ci", "standard")
  sem_bootstrap_default <- resolve_config_value("modules.sem.bootstrap", FALSE)
  sem_bootstrap_samples_default <- resolve_config_value("modules.sem.bootstrap_samples", 5000)
  sem_std_default <- resolve_config_value("modules.sem.std", "std.all")

  digits <- if (!is.null(opts$digits)) as.numeric(opts$digits) else digits_default
  analysis_input <- if (!is.null(opts$analysis)) opts$analysis else analysis_default
  analysis <- normalize_analysis_type(analysis_input, default = "auto")
  sem_type <- normalize_sem_type(analysis_input, default = sem_analysis_default)
  normality <- if (!is.null(opts$normality)) tolower(opts$normality) else normality_default
  homogeneity_tests <- parse_homogeneity_tests(opts$homogeneity, homogeneity_default)
  linearity <- resolve_parse_bool(opts$linearity, default = linearity_default)
  homoscedasticity <- resolve_parse_bool(opts$homoscedasticity, default = homoscedasticity_default)
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
  random_effects <- resolve_parse_bool(opts$`random-effects`, default = mm_random_default)
  singular <- resolve_parse_bool(opts$singular, default = mm_singular_default)
  mm_convergence <- resolve_parse_bool(opts$convergence, default = mm_conv_default)
  dharma <- resolve_parse_bool(opts$dharma, default = mm_dharma_default)
  performance_flag <- resolve_parse_bool(opts$performance, default = mm_performance_default)
  mardia <- resolve_parse_bool(opts$mardia, default = sem_mardia_default)
  mahalanobis <- resolve_parse_bool(opts$mahalanobis, default = sem_mahal_default)
  mahalanobis_alpha <- if (!is.null(opts$`mahalanobis-alpha`)) as.numeric(opts$`mahalanobis-alpha`) else sem_mahal_alpha_default
  collinearity <- resolve_parse_bool(opts$collinearity, default = sem_coll_default)
  max_cor <- if (!is.null(opts$`max-cor`)) as.numeric(opts$`max-cor`) else sem_max_cor_default
  max_kappa <- if (!is.null(opts$`max-kappa`)) as.numeric(opts$`max-kappa`) else sem_max_kappa_default
  heywood <- resolve_parse_bool(opts$heywood, default = sem_heywood_default)
  sem_convergence <- resolve_parse_bool(opts$convergence, default = sem_conv_default)
  reml <- normalize_reml(opts$reml, default = mm_reml_default)
  optimizer <- if (!is.null(opts$optimizer) && nzchar(opts$optimizer)) as.character(opts$optimizer) else mm_optimizer_default
  maxfun <- normalize_maxfun(opts$maxfun, default = mm_maxfun_default)
  estimator <- normalize_estimator(opts$estimator, default = sem_estimator_default)
  missing <- normalize_missing(opts$missing, default = sem_missing_default)
  se <- normalize_se(opts$se, default = sem_se_default)
  ci <- normalize_ci(opts$ci, default = sem_ci_default)
  std <- normalize_std(opts$std, default = sem_std_default)
  bootstrap <- resolve_parse_bool(opts$bootstrap, default = sem_bootstrap_default)
  if (bootstrap && se != "bootstrap") se <- "bootstrap"
  if (se == "bootstrap" && !bootstrap) bootstrap <- TRUE
  bootstrap_samples <- if (!is.null(opts$`bootstrap-samples`)) as.numeric(opts$`bootstrap-samples`) else sem_bootstrap_samples_default
  if (is.na(bootstrap_samples) || bootstrap_samples <= 0) bootstrap_samples <- sem_bootstrap_samples_default

  df <- resolve_load_dataframe(opts)
  out_dir <- resolve_get_workspace_out_dir(df)

  if (analysis == "auto") {
    if (!is.null(opts$formula) || !is.null(opts$random) || !is.null(opts$fixed)) {
      analysis <- "mixed_models"
    } else if (!is.null(opts[["model"]]) || !is.null(opts[["model-file"]]) || !is.null(opts[["paths"]]) ||
               !is.null(opts$factors) || !is.null(opts$x) || !is.null(opts$m) || !is.null(opts$y) ||
               !is.null(opts$ordered) || !is.null(opts$`group-equal`) || !is.null(opts$invariance) ||
               !is.null(opts$group)) {
      analysis <- "sem"
    } else if (!is.null(opts$ivs) || !is.null(opts$blocks)) {
      analysis <- "regression"
    } else if (!is.null(opts$within) || !is.null(opts$between) || !is.null(opts$dv)) {
      analysis <- "anova"
    } else {
      analysis <- "ttest"
    }
  }

  if (analysis %in% c("regression", "mixed_models", "sem")) {
    homogeneity_tests <- character(0)
  }

  if (is.na(mahalanobis_alpha) || mahalanobis_alpha <= 0 || mahalanobis_alpha >= 1) {
    mahalanobis_alpha <- sem_mahal_alpha_default
  }
  if (is.na(max_cor) || max_cor <= 0) max_cor <- sem_max_cor_default
  if (is.na(max_kappa) || max_kappa <= 0) max_kappa <- sem_max_kappa_default

  analysis_convergence <- if (analysis == "sem") sem_convergence else mm_convergence
  packages <- list(
    performance = is_pkg_available("performance"),
    influence = is_pkg_available("influence.ME"),
    dharma = is_pkg_available("DHARMa"),
    mvn = is_pkg_available("MVN")
  )

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
    max_shapiro_n = max_shapiro_n,
    random_effects = random_effects,
    singular = singular,
    convergence = analysis_convergence,
    dharma = dharma,
    performance = performance_flag,
    mardia = mardia,
    mahalanobis = mahalanobis,
    mahalanobis_alpha = mahalanobis_alpha,
    collinearity = collinearity,
    max_cor = max_cor,
    max_kappa = max_kappa,
    heywood = heywood,
    reml = reml,
    optimizer = optimizer,
    maxfun = maxfun,
    sem_type = sem_type,
    estimator = estimator,
    missing = missing,
    se = se,
    ci = ci,
    std = std,
    bootstrap_samples = bootstrap_samples,
    packages = packages
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
  } else if (analysis == "mixed_models") {
    result <- run_mixed_models_assumptions(df, opts, settings)
  } else if (analysis == "sem") {
    result <- run_sem_assumptions(df, opts, settings)
  } else {
    stop("Unknown analysis type.")
  }

  checks_df <- result$checks
  if (is.null(checks_df) || nrow(checks_df) == 0) stop("No assumptions could be computed.")

  note_tokens <- build_note_tokens(analysis, settings, homogeneity_tests, checks_df)
  nlss_report_path <- file.path(out_dir, "report_canonical.md")
  narrative_rows <- build_assumptions_narrative_rows(checks_df, digits)
  nlss_text <- paste(vapply(narrative_rows, function(row) row$full_sentence, character(1)), collapse = "\n")
  nlss_table <- format_nlss_table(checks_df, digits, note_tokens$note_default)

  template_key <- switch(
    analysis,
    ttest = "assumptions.ttest",
    anova = "assumptions.anova",
    regression = "assumptions.regression",
    mixed_models = "assumptions.mixed_models",
    sem = "assumptions.sem",
    "assumptions.ttest"
  )
  template_default <- switch(
    analysis,
    ttest = "assumptions/ttest-template.md",
    anova = "assumptions/anova-template.md",
    regression = "assumptions/regression-template.md",
    mixed_models = "assumptions/mixed-models-template.md",
    sem = "assumptions/sem-template.md",
    "assumptions/ttest-template.md"
  )
  template_override <- resolve_template_override(opts$template, module = "assumptions")
  template_path <- if (!is.null(template_override)) {
    template_override
  } else {
    resolve_get_template_path(template_key, template_default)
  }
  template_meta <- resolve_get_template_meta(template_path)
  table_body <- build_assumptions_table_body(checks_df, digits, template_meta$table)
  template_context <- list(
    tokens = c(
      list(
        table_body = table_body,
        narrative_default = nlss_text
      ),
      note_tokens
    ),
    narrative_rows = narrative_rows
  )

  analysis_flags <- list(
    analysis = analysis,
    sem_type = if (analysis == "sem") sem_type else NULL,
    mode = if (!is.null(result$mode)) result$mode else NULL,
    dv = if (!is.null(opts$dv)) opts$dv else NULL,
    vars = if (!is.null(opts$vars)) opts$vars else NULL,
    x = if (!is.null(opts$x)) opts$x else NULL,
    y = if (!is.null(opts$y)) opts$y else NULL,
    m = if (!is.null(opts$m)) opts$m else NULL,
    covariates = if (!is.null(opts$covariates)) opts$covariates else NULL,
    serial = if (!is.null(opts$serial)) opts$serial else NULL,
    group = if (!is.null(opts$group)) opts$group else NULL,
    between = if (!is.null(opts$between)) opts$between else NULL,
    within = if (!is.null(opts$within)) opts$within else NULL,
    "subject-id" = if (!is.null(opts$`subject-id`)) opts$`subject-id` else NULL,
    ivs = if (!is.null(opts$ivs)) opts$ivs else NULL,
    blocks = if (!is.null(opts$blocks)) opts$blocks else NULL,
    formula = if (analysis == "mixed_models" && !is.null(result$model)) {
      result$model
    } else if (!is.null(opts$formula)) {
      opts$formula
    } else {
      NULL
    },
    fixed = if (!is.null(opts$fixed)) opts$fixed else NULL,
    random = if (!is.null(opts$random)) opts$random else NULL,
    model = if (analysis == "sem" && !is.null(result$model)) {
      result$model
    } else if (!is.null(opts[["model"]])) {
      opts[["model"]]
    } else {
      NULL
    },
    "model-file" = if (!is.null(opts[["model-file"]])) opts[["model-file"]] else NULL,
    paths = if (!is.null(opts[["paths"]])) opts[["paths"]] else NULL,
    factors = if (!is.null(opts$factors)) opts$factors else NULL,
    ordered = if (!is.null(opts$ordered)) opts$ordered else NULL,
    "group-equal" = if (!is.null(opts$`group-equal`)) opts$`group-equal` else NULL,
    invariance = if (!is.null(opts$invariance)) opts$invariance else NULL,
    normality = settings$normality,
    homogeneity = if (analysis %in% c("ttest", "anova")) {
      if (length(homogeneity_tests) > 0) paste(homogeneity_tests, collapse = ", ") else "none"
    } else {
      NULL
    },
    linearity = if (analysis == "regression") linearity else NULL,
    homoscedasticity = if (analysis %in% c("regression", "mixed_models")) homoscedasticity else NULL,
    vif = if (analysis == "regression") vif else NULL,
    "durbin-watson" = if (analysis == "regression") durbin_watson else NULL,
    outliers = if (analysis %in% c("regression", "mixed_models")) outliers else NULL,
    influence = if (analysis %in% c("regression", "mixed_models")) influence else NULL,
    "random-effects" = if (analysis == "mixed_models") random_effects else NULL,
    singular = if (analysis == "mixed_models") singular else NULL,
    convergence = if (analysis %in% c("mixed_models", "sem")) analysis_convergence else NULL,
    dharma = if (analysis == "mixed_models") dharma else NULL,
    performance = if (analysis == "mixed_models") performance_flag else NULL,
    reml = if (analysis == "mixed_models") reml else NULL,
    optimizer = if (analysis == "mixed_models") optimizer else NULL,
    maxfun = if (analysis == "mixed_models") maxfun else NULL,
    mardia = if (analysis == "sem") mardia else NULL,
    mahalanobis = if (analysis == "sem") mahalanobis else NULL,
    "mahalanobis-alpha" = if (analysis == "sem") mahalanobis_alpha else NULL,
    collinearity = if (analysis == "sem") collinearity else NULL,
    "max-cor" = if (analysis == "sem") max_cor else NULL,
    "max-kappa" = if (analysis == "sem") max_kappa else NULL,
    heywood = if (analysis == "sem") heywood else NULL,
    estimator = if (analysis == "sem") estimator else NULL,
    missing = if (analysis == "sem") missing else NULL,
    se = if (analysis == "sem") se else NULL,
    ci = if (analysis == "sem") ci else NULL,
    std = if (analysis == "sem") std else NULL,
    bootstrap = if (analysis == "sem") bootstrap else NULL,
    "bootstrap-samples" = if (analysis == "sem") bootstrap_samples else NULL,
    alpha = alpha,
    digits = digits
  )

  resolve_append_nlss_report(
    nlss_report_path,
    "Assumption checks",
    nlss_table,
    nlss_text,
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
        vif_warn = vif_warn,
        vif_high = vif_high,
        durbin_watson = durbin_watson,
        outlier_z = outlier_z,
        cook_multiplier = cook_multiplier,
        max_shapiro_n = max_shapiro_n,
        outliers = outliers,
        influence = influence,
        random_effects = random_effects,
        singular = singular,
        convergence = analysis_convergence,
        dharma = dharma,
        performance = performance_flag,
        reml = reml,
        optimizer = optimizer,
        maxfun = maxfun,
        sem_type = sem_type,
        mardia = mardia,
        mahalanobis = mahalanobis,
        mahalanobis_alpha = mahalanobis_alpha,
        collinearity = collinearity,
        max_cor = max_cor,
        max_kappa = max_kappa,
        heywood = heywood,
        estimator = estimator,
        missing = missing,
        se = se,
        ci = ci,
        std = std,
        bootstrap = bootstrap,
        bootstrap_samples = bootstrap_samples,
        alpha = alpha,
        digits = digits
      ),
      user_prompt = resolve_get_user_prompt(opts)
    )
  }
}

main()
