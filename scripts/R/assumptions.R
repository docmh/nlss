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
source(file.path(bootstrap_dir, "lib", "bootstrap.R"))
nlss_bootstrap()

print_usage <- function() {
  cat("R diagnostic checks with auditable dataset-backed run bundles\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript assumptions.R --csv data.csv --analysis ttest --vars var1,var2\n")
  cat("  Rscript assumptions.R --csv data.csv --analysis ttest --vars score --group condition\n")
  cat("  Rscript assumptions.R --csv data.csv --analysis ttest --x pre --y post\n")
  cat("  Rscript assumptions.R --csv data.csv --analysis anova --dv score --between group,gender\n")
  cat("  Rscript assumptions.R --csv data.csv --analysis anova --within pre,mid,post --between group\n")
  cat("  Rscript assumptions.R --csv data.csv --analysis regression --dv outcome --ivs age,stress\n")
  cat("  Rscript assumptions.R --csv data.csv --analysis regression --dv outcome --blocks \"age,gender;stress\"\n")
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
  cat("  --ordered LIST           SEM ordered categorical variables\n")
  cat("  --estimator NAME         SEM estimator\n")
  cat("  --missing TYPE           SEM missing handling\n")
  cat("  --se TYPE                SEM standard errors\n")
  cat("  --ci TYPE                SEM confidence interval type\n")
  cat("  --bootstrap TRUE/FALSE   SEM bootstrap standard errors\n")
  cat("  --bootstrap-samples N    SEM bootstrap resamples\n")
  cat("  --seed N                 Diagnostic seed (SEM: modules.sem.seed; mixed: modules.assumptions.mixed_models.seed)\n")
  cat("  --std TYPE               SEM standardization\n")
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
  cat("  --log TRUE/FALSE         Legacy JSONL only; mandatory run bundle is always written\n")
  cat("  --interactive            Prompt for inputs\n")
  cat("  --help                   Show this help\n")
}

interactive_options <- function() {
  cat("Interactive input selected.\n")
  input_type <- prompt("Input type (csv/sav/rds/rdata/parquet)", "csv")
  input_type <- tolower(input_type)
  opts <- list()

  if (input_type == "csv") {
    opts[["csv"]] <- prompt("CSV path")
    sep_default <- get_config_value("defaults.csv.sep")
    header_default <- get_config_value("defaults.csv.header")
    opts[["sep"]] <- prompt("Separator", sep_default)
    opts[["header"]] <- prompt("Header TRUE/FALSE", ifelse(isTRUE(header_default), "TRUE", "FALSE"))
  } else if (input_type == "sav") {
    opts[["sav"]] <- prompt("SAV path")
  } else if (input_type == "rds") {
    opts[["rds"]] <- prompt("RDS path")
  } else if (input_type == "rdata") {
    opts[["rdata"]] <- prompt("RData path")
    opts[["df"]] <- prompt("Data frame object name")
  } else if (input_type == "parquet") {
    opts[["parquet"]] <- prompt("Parquet path")
  } else {
    stop("Unsupported input type.")
  }

  analysis_default <- get_config_value("modules.assumptions.analysis")
  analysis_input <- prompt("Analysis (ttest/anova/regression/mixed_models/sem/auto)", analysis_default)
  analysis_norm <- normalize_analysis_type(analysis_input, default = "auto")
  opts[["analysis"]] <- analysis_input

  if (analysis_norm == "ttest") {
    test_type <- prompt("t-test type (one-sample/independent/paired)", "one-sample")
    test_type <- normalize_ttest_type(test_type, default = "one_sample")
    if (test_type == "paired") {
      opts[["x"]] <- prompt("Measure 1 variables (comma-separated)", "")
      opts[["y"]] <- prompt("Measure 2 variables (comma-separated)", "")
    } else {
      opts[["vars"]] <- prompt("Variables (comma-separated)", "")
      if (test_type == "independent") {
        opts[["group"]] <- prompt("Grouping variable", "")
      }
    }
  } else if (analysis_norm == "anova") {
    mode <- prompt("ANOVA mode (between/within/mixed)", "between")
    mode <- tolower(mode)
    if (mode == "between") {
      opts[["dv"]] <- prompt("Dependent variable", "")
      opts[["between"]] <- prompt("Between-subjects factors (comma-separated)", "")
    } else {
      opts[["within"]] <- prompt("Repeated measures variables (comma-separated)", "")
      opts[["between"]] <- prompt("Between-subjects factors (optional)", "")
      opts$`subject-id` <- prompt("Subject id (optional)", "")
    }
  } else if (analysis_norm == "regression") {
    opts[["dv"]] <- prompt("Dependent variable", "")
    blocks <- prompt("Blocks (semicolon-separated; leave blank for --ivs)", "")
    if (blocks != "") {
      opts[["blocks"]] <- blocks
    } else {
      opts[["ivs"]] <- prompt("Predictors (comma-separated)", "")
    }
  } else if (analysis_norm == "mixed_models") {
    use_formula <- prompt("Use full formula? (yes/no)", "yes")
    if (tolower(use_formula) %in% c("yes", "y")) {
      opts[["formula"]] <- prompt("Model formula (e.g., score ~ time + (1|id))", "")
    } else {
      opts[["dv"]] <- prompt("Dependent variable", "")
      opts[["fixed"]] <- prompt("Fixed effects (comma-separated)", "")
      opts[["random"]] <- prompt("Random terms (comma-separated; e.g., 1|id,time|id)", "")
    }
  } else if (analysis_norm == "sem") {
    sem_type <- prompt("SEM analysis (sem/cfa/path/mediation/invariance)", "sem")
    opts[["analysis"]] <- sem_type
    sem_type <- tolower(sem_type)
    if (sem_type == "cfa") {
      opts[["factors"]] <- prompt("Factors (F1=item1,item2;F2=item3,item4)", "")
      opts[["model"]] <- prompt("Model syntax (blank to use factors)", "")
    } else if (sem_type == "mediation") {
      opts[["x"]] <- prompt("Predictor (x)", "")
      opts[["m"]] <- prompt("Mediators (comma-separated)", "")
      opts[["y"]] <- prompt("Outcome (y)", "")
      opts[["covariates"]] <- prompt("Covariates (comma-separated, optional)", "")
      opts[["serial"]] <- prompt("Serial mediation TRUE/FALSE", "FALSE")
    } else if (sem_type == "path") {
      opts[["dv"]] <- prompt("Dependent variable", "")
      opts[["ivs"]] <- prompt("Predictors (comma-separated)", "")
      opts[["covariates"]] <- prompt("Covariates (comma-separated, optional)", "")
      opts[["model"]] <- prompt("Model syntax (blank to use dv/ivs)", "")
    } else {
      opts[["model"]] <- prompt("Model syntax", "")
    }

    estimator_default <- get_config_value("modules.sem.estimator")
    missing_default <- get_config_value("modules.sem.missing")
    se_default <- get_config_value("modules.sem.se")
    ci_default <- get_config_value("modules.sem.ci")
    bootstrap_default <- get_config_value("modules.sem.bootstrap")
    bootstrap_samples_default <- get_config_value("modules.sem.bootstrap_samples")
    std_default <- get_config_value("modules.sem.std")

    opts[["estimator"]] <- prompt("Estimator", estimator_default)
    opts[["missing"]] <- prompt("Missing handling", missing_default)
    opts[["se"]] <- prompt("SE type", se_default)
    opts[["ci"]] <- prompt("CI type", ci_default)
    opts[["bootstrap"]] <- prompt("Bootstrap TRUE/FALSE", ifelse(isTRUE(bootstrap_default), "TRUE", "FALSE"))
    opts$`bootstrap-samples` <- prompt("Bootstrap samples", as.character(bootstrap_samples_default))
    opts[["std"]] <- prompt("Standardization (none/std.lv/std.all)", std_default)
  }

  normality_default <- get_config_value("modules.assumptions.normality")
  homogeneity_default <- get_config_value("modules.assumptions.homogeneity")
  linearity_default <- get_config_value("modules.assumptions.linearity")
  homoscedasticity_default <- get_config_value("modules.assumptions.homoscedasticity")
  vif_default <- get_config_value("modules.assumptions.vif")
  dw_default <- get_config_value("modules.assumptions.durbin_watson")
  outliers_default <- get_config_value("modules.assumptions.outliers")
  influence_default <- get_config_value("modules.assumptions.influence")
  mm_random_default <- get_config_value("modules.assumptions.mixed_models.random_effects")
  mm_singular_default <- get_config_value("modules.assumptions.mixed_models.singular")
  mm_conv_default <- get_config_value("modules.assumptions.mixed_models.convergence")
  mm_dharma_default <- get_config_value("modules.assumptions.mixed_models.dharma")
  mm_perf_default <- get_config_value("modules.assumptions.mixed_models.performance")
  mm_reml_default <- get_config_value("modules.mixed_models.reml")
  mm_optimizer_default <- get_config_value("modules.mixed_models.optimizer")
  mm_maxfun_default <- get_config_value("modules.mixed_models.maxfun")
  sem_mardia_default <- get_config_value("modules.assumptions.sem.mardia")
  sem_mahal_default <- get_config_value("modules.assumptions.sem.mahalanobis")
  sem_coll_default <- get_config_value("modules.assumptions.sem.collinearity")
  sem_heywood_default <- get_config_value("modules.assumptions.sem.heywood")
  sem_conv_default <- get_config_value("modules.assumptions.sem.convergence")
  sem_mahal_alpha_default <- get_config_value("modules.assumptions.sem.mahalanobis_alpha")
  sem_max_cor_default <- get_config_value("modules.assumptions.sem.max_cor")
  sem_max_kappa_default <- get_config_value("modules.assumptions.sem.max_kappa")
  alpha_default <- get_config_value("modules.assumptions.alpha")
  digits_default <- get_config_value("defaults.digits")

  opts[["normality"]] <- prompt("Normality test (shapiro/none)", normality_default)
  if (analysis_norm %in% c("ttest", "anova")) {
    opts[["homogeneity"]] <- prompt("Homogeneity test (levene/bartlett/fligner/f/all/none)", homogeneity_default)
  } else if (analysis_norm == "regression") {
    opts[["linearity"]] <- prompt("Linearity TRUE/FALSE", ifelse(isTRUE(linearity_default), "TRUE", "FALSE"))
    opts[["homoscedasticity"]] <- prompt("Homoscedasticity TRUE/FALSE", ifelse(isTRUE(homoscedasticity_default), "TRUE", "FALSE"))
    opts[["vif"]] <- prompt("VIF TRUE/FALSE", ifelse(isTRUE(vif_default), "TRUE", "FALSE"))
    opts$`durbin-watson` <- prompt("Durbin-Watson TRUE/FALSE", ifelse(isTRUE(dw_default), "TRUE", "FALSE"))
    opts[["outliers"]] <- prompt("Outliers TRUE/FALSE", ifelse(isTRUE(outliers_default), "TRUE", "FALSE"))
    opts[["influence"]] <- prompt("Influence TRUE/FALSE", ifelse(isTRUE(influence_default), "TRUE", "FALSE"))
  } else if (analysis_norm == "mixed_models") {
    opts[["homoscedasticity"]] <- prompt("Homoscedasticity TRUE/FALSE", ifelse(isTRUE(homoscedasticity_default), "TRUE", "FALSE"))
    opts[["outliers"]] <- prompt("Outliers TRUE/FALSE", ifelse(isTRUE(outliers_default), "TRUE", "FALSE"))
    opts[["influence"]] <- prompt("Influence TRUE/FALSE", ifelse(isTRUE(influence_default), "TRUE", "FALSE"))
    opts$`random-effects` <- prompt("Random-effects normality TRUE/FALSE", ifelse(isTRUE(mm_random_default), "TRUE", "FALSE"))
    opts[["singular"]] <- prompt("Singular fit TRUE/FALSE", ifelse(isTRUE(mm_singular_default), "TRUE", "FALSE"))
    opts[["convergence"]] <- prompt("Convergence TRUE/FALSE", ifelse(isTRUE(mm_conv_default), "TRUE", "FALSE"))
    opts[["dharma"]] <- prompt("DHARMa TRUE/FALSE", ifelse(isTRUE(mm_dharma_default), "TRUE", "FALSE"))
    opts[["performance"]] <- prompt("performance TRUE/FALSE", ifelse(isTRUE(mm_perf_default), "TRUE", "FALSE"))
    opts[["reml"]] <- prompt("REML TRUE/FALSE", ifelse(isTRUE(mm_reml_default), "TRUE", "FALSE"))
    opts[["optimizer"]] <- prompt("Optimizer", mm_optimizer_default)
    opts[["maxfun"]] <- prompt("Optimizer maxfun", as.character(mm_maxfun_default))
  } else if (analysis_norm == "sem") {
    opts[["mardia"]] <- prompt("Mardia multivariate normality TRUE/FALSE", ifelse(isTRUE(sem_mardia_default), "TRUE", "FALSE"))
    opts[["mahalanobis"]] <- prompt("Mahalanobis outliers TRUE/FALSE", ifelse(isTRUE(sem_mahal_default), "TRUE", "FALSE"))
    opts$`mahalanobis-alpha` <- prompt("Mahalanobis alpha", as.character(sem_mahal_alpha_default))
    opts[["collinearity"]] <- prompt("Collinearity TRUE/FALSE", ifelse(isTRUE(sem_coll_default), "TRUE", "FALSE"))
    opts$`max-cor` <- prompt("Max |r| threshold", as.character(sem_max_cor_default))
    opts$`max-kappa` <- prompt("Condition number threshold", as.character(sem_max_kappa_default))
    opts[["heywood"]] <- prompt("Heywood checks TRUE/FALSE", ifelse(isTRUE(sem_heywood_default), "TRUE", "FALSE"))
    opts[["convergence"]] <- prompt("Convergence TRUE/FALSE", ifelse(isTRUE(sem_conv_default), "TRUE", "FALSE"))
  }

  opts[["alpha"]] <- prompt("Decision alpha", format(alpha_default, trim = TRUE))
  opts[["digits"]] <- prompt("Rounding digits", as.character(digits_default))
  opts[["template"]] <- prompt("Template (path or key; blank for default)", "")
  opts$`user-prompt` <- prompt("User prompt (optional)", "")
  log_default <- get_config_value("defaults.log")
  opts[["log"]] <- prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))
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
  stop("Unknown analysis type: ", value)
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
  stop("Unknown t-test design: ", value)
}

parse_homogeneity_tests <- function(value, default = "levene") {
  if (is.null(value) || value == "") value <- default
  val <- tolower(trimws(as.character(value)))
  if (val == "none") return(character(0))
  if (val == "all") return(c("levene", "bartlett", "fligner", "f"))
  parts <- trimws(strsplit(val, ",", fixed = TRUE)[[1]])
  parts <- parts[parts != ""]
  parts <- gsub("\\s+", "", parts)
  allowed <- c("levene", "bartlett", "fligner", "f")
  if (any(!parts %in% allowed)) stop("Unknown homogeneity test: ", paste(setdiff(parts, allowed), collapse = ", "))
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
  if (is.null(optimizer) || !nzchar(optimizer)) optimizer <- "nloptwrap"
  if (is.null(maxfun) || is.na(maxfun)) {
    return(lme4::lmerControl(optimizer = optimizer))
  }
  key <- if (optimizer == "nloptwrap") "maxeval" else if (optimizer %in% c("optim", "optimx")) "maxit" else "maxfun"
  lme4::lmerControl(optimizer = optimizer, optCtrl = setNames(list(maxfun), key))
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
  max_n <- min(max_n, 5000L)
  if (n < 3 || n > max_n) {
    return(list(w = NA_real_, p = NA_real_, n = n,
      note = paste0("Shapiro-Wilk requires at least 3 observations and n <= the effective requested cap of ", max_n, ".")))
  }
  test <- tryCatch(shapiro.test(values), error = function(e) NULL)
  if (is.null(test)) return(list(w = NA_real_, p = NA_real_, n = n, note = "Shapiro-Wilk failed."))
  list(w = unname(test$statistic), p = test$p.value, n = n, note = "")
}

calc_levene <- function(values, group) {
  group <- droplevels(as.factor(group))
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
  group <- droplevels(as.factor(group))
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

# Koenker's studentized Breusch-Pagan statistic: n R² on the fitted design.
assumptions_residual_variation <- function(model) {
  y <- model.response(model.frame(model))
  scale <- max(sum(y^2), .Machine$double.xmin)
  df.residual(model) > 0L && sum(residuals(model)^2) > .Machine$double.eps^2 * scale * length(y)
}

calc_bp <- function(model) {
  if (!assumptions_residual_variation(model)) return(NULL)
  residuals <- residuals(model); mm <- model.matrix(model)
  aux <- lm.fit(mm, residuals^2)
  df <- aux$rank - 1L
  total <- sum((residuals^2 - mean(residuals^2))^2)
  if (df < 1L || !is.finite(total) || total <= 0) return(NULL)
  stat <- length(residuals) * max(0, 1 - sum(aux$residuals^2) / total)
  list(stat = stat, df1 = df, df2 = NA_real_, p = pchisq(stat, df, lower.tail = FALSE))
}

calc_dw <- function(resid) {
  if (length(resid) < 2 || sum(resid^2) <= 0) return(NA_real_)
  sum(diff(resid)^2) / sum(resid^2)
}

calc_vif <- function(model) {
  mm <- model.matrix(model)
  mm <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]
  if (!ncol(mm)) return(data.frame())
  values <- vapply(seq_len(ncol(mm)), function(j) {
    total <- sum((mm[, j] - mean(mm[, j]))^2)
    if (!is.finite(total) || total <= 0) return(NA_real_)
    if (ncol(mm) == 1L) return(1)
    fit <- lm.fit(cbind(1, mm[, -j, drop = FALSE]), mm[, j])
    sse <- sum(fit$residuals^2)
    if (sse <= .Machine$double.eps * total) Inf else total / sse
  }, numeric(1))
  data.frame(term = colnames(mm), vif = values, stringsAsFactors = FALSE)
}

# OLS residuals are orthogonal to included predictors: their correlation is not
# a linearity test. A centered quadratic added term tests this limited curvature.
calc_linearity <- function(model, x) {
  if (!is.numeric(x) || !assumptions_residual_variation(model)) return(NULL)
  mm <- model.matrix(model)
  extra <- (x - mean(x))^2
  augmented <- lm.fit(cbind(mm, extra), model.response(model.frame(model)))
  added_df <- augmented$rank - model$rank
  residual_df <- nrow(mm) - augmented$rank
  sse <- sum(augmented$residuals^2)
  if (added_df != 1L || residual_df < 1L || !is.finite(sse) || sse <= 0) return(NULL)
  statistic <- max(0, (sum(residuals(model)^2) - sse) / added_df) / (sse / residual_df)
  list(stat = statistic, df1 = added_df, df2 = residual_df,
       p = pf(statistic, added_df, residual_df, lower.tail = FALSE))
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
    list(key = "status", label = "Status"),
    list(key = "decision", label = "Screen", drop_if_empty = TRUE),
    list(key = "note", label = "Note", drop_if_empty = TRUE)
  )
  columns <- normalize_table_columns(
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
      target = if ("target_label" %in% names(row)) row$target_label else row$target,
      group = if ("group_label" %in% names(row)) row$group_label else row$group,
      status = row$status,
      n = ifelse(is.na(row$n), "", as.character(row$n)),
      statistic = format_stat(row$statistic, digits),
      df = df_text,
      p = format_p(row$p),
      value = format_stat(row$value, digits),
      decision = row$decision,
      note = row$note
    )
    row_vals <- vapply(columns, function(col) {
      as_cell_text(row_map[[col$key]])
    }, character(1))
    rows[[length(rows) + 1]] <- row_vals
  }

  filtered <- drop_empty_columns(columns, rows)
  columns <- filtered$columns
  rows <- filtered$rows
  headers <- vapply(columns, function(col) col$label, character(1))
  render_markdown_table(headers, rows)
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
    if (sentence != "") sentence <- paste0(sentence, ". Status: ", row$status, ".", if (nzchar(row$note)) paste0(" ", row$note) else "")
    rows[[length(rows) + 1]] <- list(
      full_sentence = sentence,
      status = row$status,
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

assumptions_variables <- function(df, vars, numeric = character()) {
  absent <- setdiff(vars, names(df))
  if (length(absent)) stop("Unknown variables: ", paste(absent, collapse = ", "))
  invalid <- numeric[!vapply(df[numeric], is.numeric, logical(1))]
  if (length(invalid)) stop("Selected response/continuous variables must be numeric: ", paste(invalid, collapse = ", "))
  for (var in vars) if (is.numeric(df[[var]]) && any(is.infinite(df[[var]]))) stop("Infinite values in selected variable: ", var)
  invisible(TRUE)
}

assumptions_formula_name <- function(name) {
  if (any(grepl("`", name, fixed = TRUE))) stop("Backticks in variable names are unsupported in model formulas.")
  paste0("`", name, "`")
}

assumptions_factor <- function(values) {
  if (!is.numeric(values)) return(droplevels(as.factor(values)))
  observed <- sort(unique(values[!is.na(values)]))
  factor(match(values, observed), levels = seq_along(observed), labels = sprintf("%.17g", observed))
}

assumptions_case_selection <- function(df, rows, target, model, group = NULL, group_names = NULL) {
  grouping <- if (is.null(group)) list() else lapply(levels(group), function(level) {
    entry <- list(level = level, source_rows = rows[group == level])
    if (!is.null(group_names)) entry$values <- as.list(group_names[which(group == level)[1], , drop = FALSE])
    entry
  })
  list(target = target, model = model, included_rows = as.integer(rows),
    excluded_rows = setdiff(seq_len(nrow(df)), rows), grouping = grouping)
}

# Group by tuples of integer factor codes, never by a delimiter-based label:
# ("a:b","c") and ("a","b:c") must remain distinct cells.
assumptions_cells <- function(data, vars) {
  if (!length(vars)) return(NULL)
  codes <- lapply(data[vars], function(x) as.integer(x))
  key <- do.call(paste, c(codes, sep = ":"))
  values <- unique(key)
  labels <- vapply(values, function(k) paste(vapply(data[vars], function(x) as.character(x[which(key == k)[1]]), character(1)), collapse = ":"), character(1))
  if (anyDuplicated(labels)) labels <- paste0(labels, " [cell ", seq_along(labels), "]")
  factor(key, levels = values, labels = labels)
}

assumptions_rows <- function(analysis, model, n) {
  rows <- list()
  add <- function(assumption, test, target, group = "", stat = NA_real_, df1 = NA_real_, df2 = NA_real_,
                  p = NA_real_, value = NA_real_, nobs = n, decision = "", note = "", status = NULL) {
    if (is.null(status)) status <- if (is.finite(stat) || is.finite(p) || is.finite(value) || nzchar(decision)) "available" else "unavailable"
    if (status != "available") decision <- ""
    row <- make_check_row(analysis, model, assumption, test, target, group, stat, df1, df2, p, value, nobs, decision, note)
    row$status <- status
    rows[[length(rows) + 1L]] <<- row
  }
  normality <- function(values, target, group = "", settings) {
    result <- safe_shapiro(values, settings$max_shapiro_n)
    status <- if (is.finite(result$p)) "available" else if (length(values) < 3L || length(values) > min(5000L, settings$max_shapiro_n)) "skipped" else "unavailable"
    add("Normality", "Shapiro-Wilk", target, group, stat = result$w, p = result$p, nobs = result$n,
      decision = decision_from_p(result$p, settings$alpha), note = result$note, status = status)
  }
  homogeneity <- function(values, group, target, settings) {
    for (test in settings$homogeneity_tests) {
      result <- switch(test, levene = calc_levene(values, group), bartlett = calc_bartlett(values, group),
        fligner = calc_fligner(values, group), f = calc_var_test(values, group))
      add("Homogeneity", c(levene = "Levene (median)", bartlett = "Bartlett", fligner = "Fligner-Killeen", f = "F-test")[[test]],
        target, stat = result$stat, df1 = result$df1, df2 = result$df2, p = result$p, nobs = length(values),
        decision = decision_from_p(result$p, settings$alpha), note = result$note,
        status = if (is.finite(result$p)) "available" else "unavailable")
    }
  }
  list(add = add, normality = normality, homogeneity = homogeneity,
    get = function() if (length(rows)) do.call(rbind, rows) else data.frame())
}

run_ttest_assumptions <- function(df, opts, settings) {
  present <- function(name) !is.null(opts[[name]]) && nzchar(opts[[name]])
  paired <- present("x") || present("y")
  if (paired && present("group")) stop("Paired checks do not use --group.")
  if (paired && present("vars")) stop("Paired checks use --x/--y, not --vars.")
  mode <- if (paired) "paired" else if (present("group")) "independent" else "one_sample"
  collector <- assumptions_rows("ttest", c(paired = "Paired", independent = "Independent", one_sample = "One-sample")[[mode]], nrow(df))
  selections <- list()
  if (paired) {
    x <- parse_list(opts[["x"]]); y <- parse_list(opts[["y"]])
    if (!length(x) || length(x) != length(y)) stop("Paired checks require equally long --x and --y lists.")
    if (any(x == y)) stop("Paired measures must be distinct variables.")
    assumptions_variables(df, unique(c(x, y)), unique(c(x, y)))
    for (i in seq_along(x)) {
      rows <- which(complete.cases(df[c(x[i], y[i])]))
      target <- paste0(x[i], " - ", y[i])
      if (settings$normality != "none") collector$normality(df[[x[i]]][rows] - df[[y[i]]][rows], target, settings = settings)
      selections[[i]] <- assumptions_case_selection(df, rows, target, "Paired")
      selections[[i]]$variables <- c(x[i], y[i])
      selections[[i]]$difference <- "x - y"
    }
  } else {
    group_var <- if (mode == "independent") opts[["group"]] else NULL
    vars <- select_variables(df, opts[["vars"]], group_var, default = settings$vars_default, include_numeric = FALSE)
    if (!length(vars)) stop("No numeric variables available for analysis.")
    assumptions_variables(df, c(vars, group_var), vars)
    for (var in vars) {
      rows <- which(complete.cases(df[c(var, group_var)]))
      values <- df[[var]][rows]
      group <- if (is.null(group_var)) NULL else assumptions_factor(df[[group_var]][rows])
      if (!is.null(group) && nlevels(group) != 2L) stop("Grouping variable must have exactly two levels for independent checks.")
      if (settings$normality != "none") {
        if (is.null(group)) collector$normality(values, var, settings = settings) else for (level in levels(group))
          collector$normality(values[group == level], var, level, settings)
      }
      if (!is.null(group)) collector$homogeneity(values, group, var, settings)
      selections[[length(selections) + 1L]] <- assumptions_case_selection(df, rows, var, mode, group)
    }
  }
  list(checks = collector$get(), mode = mode, diagnostics = list(
    requested = settings[c("normality", "homogeneity_tests")], source_n = nrow(df), case_selection = selections,
    group_variable = if (present("group")) opts[["group"]] else NULL, labels = resolve_label_metadata(df)))
}

run_anova_assumptions <- function(df, opts, settings) {
  between <- unique(parse_list(opts[["between"]])); within <- parse_list(opts[["within"]])
  dv <- if (is.null(opts[["dv"]])) "" else opts[["dv"]]
  subject <- if (is.null(opts[["subject-id"]])) "" else opts[["subject-id"]]
  repeated <- length(within) > 0L
  mode <- if (repeated) if (length(between)) "mixed" else "within" else "between"
  responses <- if (repeated) within else dv
  if (repeated && (length(within) < 2L || anyDuplicated(within))) stop("Repeated checks require at least two distinct --within variables.")
  if (repeated && nzchar(dv)) stop("Wide repeated checks use --within, not --dv.")
  if (!repeated && nzchar(subject)) stop("--subject-id requires --within.")
  if (!repeated && !nzchar(dv)) stop("Between-subject checks require --dv.")
  if (length(intersect(responses, between)) || (nzchar(subject) && subject %in% c(responses, between))) stop("Response, between-factor and subject roles must be distinct.")
  selected <- c(responses, between, if (nzchar(subject)) subject)
  assumptions_variables(df, selected, responses)
  rows <- which(complete.cases(df[selected]))
  data <- df[rows, selected, drop = FALSE]
  if (!nrow(data)) stop("No complete cases for ANOVA checks.")
  if (nzchar(subject) && anyDuplicated(data[[subject]])) stop("Wide repeated-measures subject IDs must be unique among retained cases.")
  for (var in between) {
    data[[var]] <- assumptions_factor(data[[var]])
    if (nlevels(data[[var]]) < 2L) stop("Between factors require at least two observed levels: ", var)
  }
  group <- assumptions_cells(data, between)
  label <- c(between = "Between", within = "Within", mixed = "Mixed")[[mode]]
  collector <- assumptions_rows("anova", label, nrow(data))
  for (var in responses) {
    if (settings$normality != "none") {
      if (is.null(group)) collector$normality(data[[var]], var, settings = settings) else for (level in levels(group))
        collector$normality(data[[var]][group == level], var, level, settings)
    }
    if (!is.null(group)) collector$homogeneity(data[[var]], group, var, settings)
  }
  rhs <- if (length(between)) paste(assumptions_formula_name(between), collapse = " * ") else "1"
  lhs <- if (repeated) paste0("cbind(", paste(assumptions_formula_name(responses), collapse = ", "), ")") else assumptions_formula_name(dv)
  formula <- as.formula(paste(lhs, "~", rhs))
  fit <- lm(formula, data = data, na.action = na.fail)
  sphericity <- NULL
  if (repeated) {
    q <- length(within) - 1L
    test <- if (q < 2L) NULL else tryCatch({
      projected <- residuals(fit) %*% contr.helmert(length(within))
      if (df.residual(fit) < q || qr(crossprod(projected))$rank < q) stop("Repeated contrast covariance is singular or lacks residual degrees of freedom.")
      stats::mauchly.test(fit, X = ~1)
    }, error = function(e) e)
    valid <- !is.null(test) && !inherits(test, "error") && is.finite(test$p.value) && is.finite(test$statistic) && test$p.value >= 0 && test$p.value <= 1
    collector$add("Sphericity", "Mauchly", "Within",
      stat = if (valid) unname(test$statistic) else NA_real_,
      df1 = if (q > 1L) q * (q + 1L) / 2L - 1L else NA_real_,
      p = if (valid) test$p.value else NA_real_, decision = if (valid) decision_from_p(test$p.value, settings$alpha) else "",
      status = if (valid) "available" else if (q < 2L) "skipped" else "unavailable",
      note = if (valid) "Within-subject contrast space (X = ~1); covariance of full between-factor model residuals." else if (q < 2L)
        "Sphericity is automatic with two repeated levels; no Mauchly test is required." else if (inherits(test, "error")) conditionMessage(test) else "Mauchly statistic or probability unavailable.")
    sphericity <- list(projection = "X = ~1", contrast_dimension = q, residual_df = df.residual(fit),
      between_formula = rhs, statistic = if (valid) unname(test$statistic) else NULL, p = if (valid) test$p.value else NULL)
  }
  case <- assumptions_case_selection(df, rows, responses, label, group, if (length(between)) data[between] else NULL)
  case$subject_id <- if (nzchar(subject)) subject else NULL
  case$subjects <- if (nzchar(subject)) as.character(data[[subject]]) else as.character(rows)
  mm <- model.matrix(fit)
  list(checks = collector$get(), mode = mode, diagnostics = list(
    requested = settings[c("normality", "homogeneity_tests")], source_n = nrow(df), case_selection = list(case),
    labels = resolve_label_metadata(df), formula = paste(deparse(formula), collapse = " "), model_rank = fit$rank,
    residual_df = df.residual(fit), model_columns = colnames(mm), contrasts = attr(mm, "contrasts"),
    factor_levels = lapply(data[between], levels), sphericity = sphericity,
    normality_scope = "Observed response distributions within between-factor cells; repeated marginal screens are not a multivariate normality test."))
}

run_regression_assumptions <- function(df, opts, settings) {
  dv <- if (is.null(opts[["dv"]])) "" else opts[["dv"]]
  blocks <- parse_blocks(opts[["blocks"]])
  if (length(blocks) && length(parse_list(opts[["ivs"]]))) stop("Use either --ivs or --blocks, not both.")
  if (!length(blocks)) blocks <- list(parse_list(opts[["ivs"]]))
  if (!nzchar(dv) || !length(unlist(blocks))) stop("Regression checks require --dv and --ivs or --blocks.")
  predictors <- unique(unlist(blocks))
  if (dv %in% predictors) stop("The dependent variable cannot also be a predictor.")
  assumptions_variables(df, c(dv, predictors), dv)
  rows <- which(complete.cases(df[c(dv, predictors)]))
  data <- df[rows, c(dv, predictors), drop = FALSE]
  rownames(data) <- as.character(rows)
  if (!nrow(data)) stop("No jointly complete cases for regression blocks.")
  for (var in predictors) if (!is.numeric(data[[var]])) data[[var]] <- droplevels(as.factor(data[[var]]))
  collected <- list(); selections <- list(); models <- list()
  for (i in seq_along(blocks)) {
    vars <- unique(unlist(blocks[seq_len(i)]))
    formula <- as.formula(paste(assumptions_formula_name(dv), "~", paste(assumptions_formula_name(vars), collapse = " + ")))
    model <- lm(formula, data = data, na.action = na.fail)
    residuals <- residuals(model); n <- length(residuals); label <- paste0("Block ", i)
    residual_variation <- assumptions_residual_variation(model)
    collector <- assumptions_rows("regression", label, n)
    if (settings$normality != "none") {
      if (residual_variation) collector$normality(residuals, "Residuals", settings = settings) else
        collector$add("Normality", "Shapiro-Wilk", "Residuals", note = "Unavailable: no residual degrees of freedom or numerically zero residual variation.")
    }
    if (settings$linearity) for (var in vars) {
      result <- calc_linearity(model, data[[var]])
      collector$add("Linearity", "Quadratic added-term F", var,
        stat = if (is.null(result)) NA_real_ else result$stat, df1 = if (is.null(result)) NA_real_ else result$df1,
        df2 = if (is.null(result)) NA_real_ else result$df2, p = if (is.null(result)) NA_real_ else result$p,
        decision = if (is.null(result)) "" else decision_from_p(result$p, settings$alpha),
        note = if (is.null(result)) "Unavailable: numeric predictor with one estimable added quadratic term and positive residual degrees of freedom required."
          else "One centered quadratic term added to the fitted block; a limited curvature screen, not proof of linearity.")
    }
    if (settings$homoscedasticity) {
      bp <- calc_bp(model)
      collector$add("Homoscedasticity", "Breusch-Pagan", "Residuals",
        stat = if (is.null(bp)) NA_real_ else bp$stat, df1 = if (is.null(bp)) NA_real_ else bp$df1,
        p = if (is.null(bp)) NA_real_ else bp$p, decision = if (is.null(bp)) "" else decision_from_p(bp$p, settings$alpha),
        note = if (is.null(bp)) "Unavailable: nonconstant squared residuals and an estimable variance predictor required."
          else "Koenker studentized n R²; auxiliary degrees of freedom use model-matrix rank minus the intercept.")
    }
    if (settings$durbin_watson) collector$add("Independence", "Durbin-Watson", "Residuals", stat = if (residual_variation) calc_dw(residuals) else NA_real_,
      note = if (residual_variation) "Statistic only, in retained source-row order; no p-value or automatic independence conclusion."
        else "Unavailable: no residual degrees of freedom or numerically zero residual variation.")
    vectors <- list()
    for (kind in c("outliers", "influence")) if (isTRUE(settings[[kind]])) {
      values <- tryCatch(if (kind == "outliers") rstandard(model) else cooks.distance(model), error = function(e) NULL)
      valid <- residual_variation && length(values) == n && all(is.finite(values))
      threshold <- if (kind == "outliers") settings$outlier_z else settings$cook_multiplier / n
      flagged <- if (valid) if (kind == "outliers") abs(values) > threshold else values > threshold else rep(FALSE, n)
      count <- if (valid) sum(flagged) else NA_real_
      collector$add(if (kind == "outliers") "Outliers" else "Influence", if (kind == "outliers") "Std. residuals" else "Cook's distance",
        "Residuals", value = if (valid) max(if (kind == "outliers") abs(values) else values) else NA_real_,
        decision = decision_from_count(count), note = if (valid) paste0(if (kind == "outliers") "|std resid|" else "Cook's D", " > ", threshold, ": ", count)
          else "Unavailable: standardized residuals/distances include nonfinite values; no zero count inferred.")
      vectors[[kind]] <- list(source_rows = rows, values = as.numeric(values), threshold = threshold,
        count = count, flagged_source_rows = if (valid) rows[flagged] else NULL, status = if (valid) "available" else "unavailable")
    }
    vifs <- if (settings$vif) calc_vif(model) else data.frame()
    if (settings$vif && nrow(vifs)) for (j in seq_len(nrow(vifs))) {
      value <- vifs$vif[j]
      collector$add("Multicollinearity", "VIF", vifs$term[j], value = if (is.finite(value)) value else NA_real_,
        decision = decision_from_vif(value, settings$vif_warn, settings$vif_high),
        note = if (is.infinite(value)) "Infinite VIF: numerical linear dependence (auxiliary residual SS <= machine epsilon times target SS); JSON value is null, status is available and decision is high."
          else if (is.na(value)) "Unavailable: constant design column." else "Design-column VIF; factor dummy columns are not term-level GVIF.")
    }
    mm <- model.matrix(model)
    selections[[i]] <- assumptions_case_selection(df, rows, dv, label)
    models[[i]] <- list(model = label, formula = paste(deparse(formula), collapse = " "), predictors = vars,
      rank = model$rank, columns = colnames(mm), aliased_coefficients = names(coef(model))[is.na(coef(model))],
      residual_df = df.residual(model), contrasts = attr(mm, "contrasts"), factor_levels = model$xlevels,
      residuals = as.numeric(residuals), fitted = as.numeric(fitted(model)), residual_diagnostics = vectors,
      vif = if (nrow(vifs)) data.frame(term = vifs$term, value = ifelse(is.finite(vifs$vif), vifs$vif, NA_real_),
        infinite = is.infinite(vifs$vif), stringsAsFactors = FALSE) else NULL)
    collected[[i]] <- collector$get()
  }
  list(checks = do.call(rbind, collected), diagnostics = list(source_n = nrow(df),
    requested = settings[c("normality", "linearity", "homoscedasticity", "durbin_watson", "outliers", "influence", "vif")],
    case_selection = selections, missing_rule = "Jointly complete on DV and all block predictors; common cases across blocks.",
    labels = resolve_label_metadata(df), models = models))
}

run_mixed_models_assumptions <- function(df, opts, settings) {
  if (!is_pkg_available("lme4")) {
    stop("Mixed models require the 'lme4' package.")
  }
  rows <- list()
  unavailable <- function(assumption, test, target = "Residuals", note, group = "") {
    rows[[length(rows) + 1L]] <<- make_check_row("mixed_models", "Mixed", assumption,
      test, target, group, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, n,
      "", paste0("Unavailable: ", note))
  }
  formula_text <- ""
  if (!is.null(opts[["formula"]]) && !is.logical(opts[["formula"]])) {
    formula_text <- as.character(opts[["formula"]])
  }
  dv <- if (!is.null(opts[["dv"]])) as.character(opts[["dv"]]) else ""
  fixed_terms <- parse_list(opts[["fixed"]])
  random_terms_raw <- parse_list(opts[["random"]])
  random_terms <- normalize_random_terms(random_terms_raw)

  if (nzchar(formula_text)) {
    if (nzchar(dv) || length(fixed_terms) || length(random_terms)) stop("Use --formula or --dv/--fixed/--random, not both.")
    formula <- tryCatch(as.formula(formula_text), error = function(e) NULL)
    if (is.null(formula)) stop("Invalid mixed model formula.")
  } else {
    if (!nzchar(dv)) stop("Mixed model checks require --formula or --dv.")
    if (length(random_terms) == 0) stop("Mixed model checks require --random or random effects in --formula.")
    formula <- build_model_formula(dv, fixed_terms, random_terms)
    formula_text <- paste(deparse(formula), collapse = " ")
  }

  random_terms_in_formula <- reformulas::findbars(formula)
  if (length(random_terms_in_formula) == 0) stop("Mixed model checks require at least one random effect term.")
  if (!is.symbol(formula[[2]])) stop("Mixed-model response must name one numeric column; prepare transformed responses explicitly.")
  if (!nzchar(dv)) {
    dv_vars <- all.vars(formula[[2]])
    if (length(dv_vars) > 0) dv <- dv_vars[1]
  }

  vars <- unique(all.vars(formula))
  missing <- setdiff(vars, names(df))
  if (length(missing) > 0) stop(paste("Unknown variables:", paste(missing, collapse = ", ")))
  assumptions_variables(df, vars, dv)
  model_df <- df[, vars, drop = FALSE]
  rownames(model_df) <- as.character(seq_len(nrow(model_df)))
  complete <- complete.cases(model_df)
  model_df <- model_df[complete, , drop = FALSE]
  if (nrow(model_df) == 0) stop("No complete cases available for mixed-model checks.")
  if (!nzchar(dv) || !(dv %in% names(model_df))) stop("Dependent variable not found.")
  if (!is.numeric(model_df[[dv]])) stop("Dependent variable must be numeric for mixed-model checks.")
  model_df <- coerce_model_factors(model_df, vars, dv)
  group_aliases <- list()
  group_vars <- unique(unlist(lapply(random_terms_in_formula, function(bar) all.vars(bar[[3]]))))
  for (name in group_vars) {
    raw <- model_df[[name]]
    if (!is.numeric(raw) || nlevels(factor(raw)) == length(unique(raw))) next
    alias <- ".nlss_group_identity"
    while (alias %in% names(model_df)) alias <- paste0(alias, "_")
    model_df[[alias]] <- assumptions_factor(raw)
    group_aliases[[name]] <- alias
  }
  replace_groups <- function(node, group = FALSE) {
    if (group && is.symbol(node) && as.character(node) %in% names(group_aliases)) return(as.name(group_aliases[[as.character(node)]]))
    if (is.call(node)) for (i in seq_along(node)[-1L]) node[[i]] <- replace_groups(node[[i]], group || (is.symbol(node[[1]]) && as.character(node[[1]]) %in% c("|", "||") && i == 3L))
    node
  }
  formula[[3]] <- replace_groups(formula[[3]])

  control <- build_lmer_control(settings$optimizer, settings$maxfun)
  fit_warnings <- character()
  fit <- withCallingHandlers(tryCatch(
    if (is.null(control)) {
      lme4::lmer(formula, data = model_df, REML = settings$reml)
    } else {
      lme4::lmer(formula, data = model_df, REML = settings$reml, control = control)
    },
    error = function(e) e
  ), warning = function(w) { fit_warnings <<- c(fit_warnings, conditionMessage(w)) })
  if (inherits(fit, "error")) stop("Mixed model fit failed: ", conditionMessage(fit))
  fitted_frame <- model.frame(fit)
  if (any(vapply(fitted_frame, function(x) is.numeric(x) && any(!is.finite(x)), logical(1)))) stop("Nonfinite transformed values in fitted mixed-model frame.")

  resid_vals <- tryCatch(residuals(fit), error = function(e) NULL)
  fitted_vals <- tryCatch(fitted(fit), error = function(e) NULL)
  n <- if (!is.null(resid_vals)) length(resid_vals) else NA_real_
  model_label <- "Mixed"
  singular <- tryCatch(lme4::isSingular(fit), error = function(e) NA)
  conv_code <- fit@optinfo$conv$opt
  conv_msgs <- unlist(fit@optinfo$conv$lme4$messages)
  boundary_msgs <- grepl("boundary.*singular", conv_msgs, ignore.case = TRUE)
  convergence_msgs <- conv_msgs[!boundary_msgs]
  optimizer_warnings <- unique(c(unlist(fit@optinfo$warnings), fit_warnings))
  conv_ok <- length(conv_code) > 0L && all(is.finite(conv_code)) && all(conv_code == 0) &&
    length(convergence_msgs) == 0L && length(optimizer_warnings) == 0L

  if (settings$convergence) {
    conv_note <- paste(c(paste0("Optimizer return code: ", if (length(conv_code)) paste(conv_code, collapse = ", ") else "unavailable"),
      convergence_msgs, optimizer_warnings, if (any(boundary_msgs)) "Singular boundary is reported separately."), collapse = "; ")
    decision <- if (!length(conv_code) || any(!is.finite(conv_code))) "" else if (conv_ok) "ok" else "flag"
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
      note = if (nzchar(decision)) conv_note else paste0("Unavailable: ", conv_note)
    )
  }

  if (settings$singular) {
    decision <- if (is.na(singular)) "" else if (singular) "flag" else "ok"
    note <- if (is.na(singular)) "Unavailable: singularity could not be assessed." else if (singular) "Singular fit detected." else ""
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
  } else if (settings$normality != "none") {
    unavailable("Normality", "Shapiro-Wilk", note = "Residuals could not be extracted.")
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
    } else {
      unavailable("Random-effects normality", "Shapiro-Wilk", "Random effects", "Random effects could not be extracted.")
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
    } else {
      unavailable("Homoscedasticity", "Abs resid vs fitted", note = "Residual-fitted correlation could not be computed.")
    }
    if (isTRUE(settings$performance)) {
      perf_test <- tryCatch(performance::check_heteroscedasticity(fit), error = function(e) e)
      perf_vals <- extract_test_values(perf_test)
      if (inherits(perf_test, "check_heteroscedasticity") && is.numeric(perf_test) && length(perf_test) == 1L) {
        perf_vals$p <- as.numeric(perf_test)
      }
      if (is.finite(perf_vals$p) && perf_vals$p >= 0 && perf_vals$p <= 1) {
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
      } else {
        unavailable("Homoscedasticity", "performance::check_heteroscedasticity", note =
          if (inherits(perf_test, "error")) conditionMessage(perf_test) else "No finite test result returned.")
      }
    }
  } else if (settings$homoscedasticity) {
    unavailable("Homoscedasticity", "Abs resid vs fitted", note = "Residuals or fitted values could not be extracted.")
    if (settings$performance) unavailable("Homoscedasticity", "performance::check_heteroscedasticity", note = "Residuals or fitted values unavailable.")
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
    } else {
      unavailable("Outliers", "Std. residuals", note = "Residual standard deviation is unavailable or zero.")
    }
  } else if (settings$outliers) {
    unavailable("Outliers", "Std. residuals", note = "Residuals could not be extracted.")
  }

  influence_values <- list()
  if (settings$influence && isTRUE(settings$packages$influence)) {
    # influence.ME declares lme4 in Depends and uses its unqualified functions.
    attached_here <- !"package:lme4" %in% search()
    if (attached_here) {
      suppressPackageStartupMessages(library("lme4", character.only = TRUE))
      on.exit(detach("package:lme4", unload = FALSE, character.only = TRUE), add = TRUE)
    }
    # The package's updated-model path reads the stored model frame;
    # it otherwise searches global data by name, losing this function-local data.
    influence_fit <- fit
    influence_fit@call$data <- quote(data.update)
    influence_fit@call$formula <- formula
    influence_fit@call$control <- control
    influence_fit@call$REML <- settings$reml
    group_list <- tryCatch(lme4::getME(fit, "flist"), error = function(e) NULL)
    if (!is.null(group_list) && length(group_list) > 0) {
      for (grp in names(group_list)) {
        cooks <- tryCatch(stats::cooks.distance(influence.ME::influence(influence_fit, group = grp)), error = function(e) e)
        if (inherits(cooks, "error") || !length(cooks) || any(!is.finite(cooks))) {
          unavailable("Influence", "Cook's distance (cluster)", grp,
            if (inherits(cooks, "error")) conditionMessage(cooks) else "Cluster refits returned non-finite distances.")
          next
        }
        influence_values[[grp]] <- data.frame(level = rownames(cooks), cooks_distance = as.numeric(cooks))
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
    } else {
      unavailable("Influence", "Cook's distance (cluster)", "Model", "No grouping assignments available.")
    }
  } else if (settings$influence) {
    unavailable("Influence", "Cook's distance (cluster)", "Model", "Package influence.ME is not installed.")
  }

  if (settings$dharma && isTRUE(settings$packages$dharma)) {
    sim <- tryCatch(DHARMa::simulateResiduals(fit, plot = FALSE, seed = settings$seed, n = 250L, refit = FALSE), error = function(e) e)
    if (!inherits(sim, "error")) {
      uni <- tryCatch(DHARMa::testUniformity(sim, plot = FALSE), error = function(e) e)
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
      } else {
        unavailable("Residuals", "DHARMa uniformity", note = if (inherits(uni, "error")) conditionMessage(uni) else "No finite result returned.")
      }
      disp <- tryCatch(DHARMa::testDispersion(sim, plot = FALSE), error = function(e) e)
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
      } else {
        unavailable("Residuals", "DHARMa dispersion", note = if (inherits(disp, "error")) conditionMessage(disp) else "No finite result returned.")
      }
    } else {
      for (test in c("DHARMa uniformity", "DHARMa dispersion")) unavailable("Residuals", test, note = conditionMessage(sim))
    }
  } else if (settings$dharma) {
    for (test in c("DHARMa uniformity", "DHARMa dispersion")) unavailable("Residuals", test, note = "Package DHARMa is not installed.")
  }

  checks_df <- if (length(rows) > 0) do.call(rbind, rows) else data.frame()
  if (nrow(checks_df)) {
    checks_df$status <- ifelse(nzchar(checks_df$decision), "available", "unavailable")
    skipped <- checks_df$test == "Shapiro-Wilk" & (checks_df$n < 3L | checks_df$n > min(settings$max_shapiro_n, 5000L))
    checks_df$status[which(skipped)] <- "skipped"
    checks_df$note[which(skipped)] <- paste0("Skipped: Shapiro-Wilk requires 3-", min(settings$max_shapiro_n, 5000L), " observations for this request.")
  }
  included_rows <- as.integer(rownames(model.frame(fit)))
  groups <- lme4::getME(fit, "flist")
  diagnostics <- list(
    formula = formula_text, effective_formula = paste(deparse(formula), collapse = " "), group_aliases = group_aliases,
    fixed_model_columns = colnames(lme4::getME(fit, "X")), fixed_model_rank = qr(lme4::getME(fit, "X"))$rank,
    dropped_columns = attr(lme4::getME(fit, "X"), "col.dropped"), contrasts = attr(lme4::getME(fit, "X"), "contrasts"),
    factor_levels = lapply(fitted_frame[vapply(fitted_frame, is.factor, logical(1))], levels),
    requested = settings[c("normality", "random_effects", "homoscedasticity", "performance", "outliers", "influence", "singular", "convergence", "dharma")],
    optimizer = list(requested = settings$optimizer, control_key = names(control$optCtrl), control_value = unname(unlist(control$optCtrl)),
      code = conv_code, evaluations = fit@optinfo$feval, message = fit@optinfo$message,
      check_messages = conv_msgs, warnings = optimizer_warnings),
    reml_requested = settings$reml, reml_effective = lme4::isREML(fit), singular = singular,
    included_rows = included_rows, excluded_rows = setdiff(seq_len(nrow(df)), included_rows),
    grouping = lapply(names(groups), function(name) list(name = name, levels = lapply(levels(groups[[name]]), function(level)
      list(level = level, source_rows = included_rows[groups[[name]] == level])))),
    influence = influence_values, dharma = list(requested = settings$dharma, seed = settings$seed, n = 250L, refit = FALSE))
  list(checks = checks_df, model = formula_text, diagnostics = diagnostics)
}

run_sem_assumptions <- function(df, opts, settings) {
  if (!is_pkg_available("lavaan")) stop("SEM assumptions require the 'lavaan' package.")
  source_lib("sem_helpers.R")
  rows <- list()
  sem_type <- settings$sem_type
  labels <- resolve_label_metadata(df)
  present <- function(key) !is.null(opts[[key]]) && nzchar(as.character(opts[[key]]))
  sources <- c("model", "paths", "model-file")
  sources <- sources[vapply(sources, present, logical(1))]
  if (length(sources) > 1L) stop("Use only one explicit SEM syntax source: --model, --paths or --model-file.")
  model_source <- if (length(sources)) sources[1] else paste0("builder:", if (sem_type %in% c("path", "mediation")) sem_type else "cfa")
  builder_keys <- c("factors", "dv", "ivs", "x", "m", "y", "covariates", "serial")
  used_builder_keys <- if (length(sources)) character() else switch(model_source,
    "builder:path" = c("dv", "ivs", "covariates"), "builder:mediation" = c("x", "m", "y", "covariates", "serial"), "factors")
  unused_keys <- setdiff(builder_keys[vapply(builder_keys, present, logical(1))], used_builder_keys)
  model_note <- if (length(unused_keys)) paste0("Resolved model source ", model_source,
    " overrides unused builder inputs: --", paste(unused_keys, collapse = ", --"), ". These inputs did not alter fitted syntax.") else ""
  if (nzchar(model_note)) warning(model_note)
  model_text <- if (is.null(nlss_run_context$replay)) "" else nlss_run_context$replay$request$design$model_syntax
  for (key in c("model", "paths")) {
    if (!nzchar(model_text) && !is.null(opts[[key]]) && nzchar(opts[[key]])) model_text <- as.character(opts[[key]])
  }
  if (!nzchar(model_text) && !is.null(opts[["model-file"]])) {
    path <- normalize_input_path(opts[["model-file"]])
    if (!file.exists(path)) stop("Model file not found: ", render_log_path(path, workspace_root = nlss_run_context$root))
    model_text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  }
  factors_text <- if (is.null(opts[["factors"]])) "" else as.character(opts[["factors"]])
  covariates <- parse_list(opts[["covariates"]])
  if (!nzchar(model_text)) {
    if (sem_type == "mediation") {
      model_text <- build_mediation_model(if (is.null(opts[["x"]])) "" else opts[["x"]], parse_list(opts[["m"]]),
        if (is.null(opts[["y"]])) "" else opts[["y"]], covariates, parse_bool(opts[["serial"]], default = get_config_value("modules.sem.serial")))
    } else if (sem_type == "path") {
      model_text <- build_path_model(if (is.null(opts[["dv"]])) "" else opts[["dv"]], parse_list(opts[["ivs"]]), covariates)
    } else if (nzchar(factors_text)) model_text <- build_cfa_model(parse_factor_spec(factors_text))
  }
  if (!nzchar(model_text)) stop("Model syntax is required. Use --model, --model-file, or a builder option.")
  model_syntax <- normalize_model_syntax(model_text)
  design <- nlss_run_context$request$design
  design$model_syntax <- model_syntax
  nlss_resolve_request(nlss_run_context$request$options, design)
  model_vars <- extract_model_vars(model_syntax)
  if (!length(model_vars)) stop("No observed model variables could be identified.")
  absent <- setdiff(model_vars, names(df))
  if (length(absent)) stop("Missing variables: ", paste(absent, collapse = ", "))
  ordered_vars <- unique(c(parse_list(opts[["ordered"]]),
    model_vars[vapply(df[model_vars], is.ordered, logical(1))]))
  absent <- setdiff(ordered_vars, model_vars)
  if (length(absent)) stop("Ordered variables must occur in the model: ", paste(absent, collapse = ", "))
  group_var <- if (is.null(opts[["group"]])) "" else as.character(opts[["group"]])
  if (sem_type == "invariance" && !nzchar(group_var)) stop("Invariance diagnostic refits require --group.")
  if (nzchar(group_var) && !group_var %in% names(df)) stop("Grouping variable not found: ", group_var)
  if (nzchar(group_var) && group_var %in% model_vars) stop("Grouping variable must not also be an observed model variable.")
  group_equal <- parse_list(opts[["group-equal"]])
  if (length(group_equal) && !nzchar(group_var)) stop("Group-equality constraints require --group.")
  selected <- unique(c(model_vars, if (nzchar(group_var)) group_var))
  for (var in selected) {
    if (is.numeric(df[[var]]) && any(is.infinite(df[[var]]))) stop("Infinite values in selected SEM variable: ", var)
  }
  if (nzchar(group_var)) {
    df[[group_var]] <- sem_group_factor(df[[group_var]])
    if (nlevels(droplevels(df[[group_var]])) < 2L) stop("Grouped SEM diagnostic refits require at least two observed groups.")
  }
  # Explicit ordinal roles preserve factor levels or numeric code order, not label order.
  ordinal <- lapply(ordered_vars, function(var) {
    df[[var]] <<- as.ordered(if (is.numeric(df[[var]])) assumptions_factor(df[[var]]) else df[[var]])
    list(variable = var, levels = levels(df[[var]]), source_rows = which(!is.na(df[[var]])))
  })
  cont_vars <- setdiff(model_vars, ordered_vars)
  invalid <- cont_vars[!vapply(df[cont_vars], is.numeric, logical(1))]
  if (length(invalid)) stop("Continuous SEM variables must be numeric; declare ordinal roles explicitly: ", paste(invalid, collapse = ", "))
  cont_data <- df[, cont_vars, drop = FALSE]
  complete_rows <- if (length(cont_vars)) which(complete.cases(cont_data)) else integer()
  cont_complete <- cont_data[complete_rows, , drop = FALSE]
  n <- nrow(cont_complete); p <- ncol(cont_complete)
  requested <- settings[c("normality", "mardia", "mahalanobis", "collinearity", "convergence", "heywood")]
  diagnostics <- list(requested = requested, model_variables = model_vars, ordered = ordinal,
    model_specification = list(model_source = model_source,
      unused_builder_inputs = render_paths_for_log(lapply(opts[unused_keys], as.character), workspace_root = nlss_run_context$root), note = model_note),
    reporting = list(standardization_requested = settings$std, loading_screen_effective = "std.all",
      ci_requested = settings$ci, confidence_intervals = "not_computed",
      note = "The standardized-loading screen always uses std.all; --std is retained as compatibility context. Diagnostic confidence intervals are not computed; --ci does not change these checks."),
    labels = labels, source_n = nrow(df), group_variable = group_var,
    group_levels = if (nzchar(group_var)) levels(df[[group_var]]) else character(),
    screening = list(scope = "Pooled observed continuous variables, not fitted-model residuals or group-specific tests.",
      complete_source_rows = complete_rows, variables = cont_vars,
      variable_source_rows = setNames(lapply(cont_vars, function(var) which(!is.na(df[[var]]))), cont_vars)),
    mardia = list(requested = settings$mardia, use_population = TRUE, bootstrap = FALSE),
    mahalanobis = NULL, fit = NULL)
  add <- function(assumption, test, target = "Indicators", statistic = NA_real_, pvalue = NA_real_,
                  value = NA_real_, nobs = n, decision = "", note = "", status = "available") {
    if (status != "available") { decision <- ""; note <- paste0(status, ": ", note) }
    row <- make_check_row("sem", toupper(sem_type), assumption, test, target, "",
      statistic, NA_real_, NA_real_, pvalue, value, nobs, decision, note)
    row$status <- status
    rows[[length(rows) + 1L]] <<- row
  }
  if (settings$normality != "none") for (var in cont_vars) {
    values <- df[[var]][!is.na(df[[var]])]
    shapiro <- safe_shapiro(values, settings$max_shapiro_n)
    status <- if (is.finite(shapiro$p)) "available" else if (length(values) > min(5000, settings$max_shapiro_n)) "skipped" else "unavailable"
    note <- if (status == "skipped") paste0("n exceeds Shapiro limit ", min(5000, settings$max_shapiro_n), ".") else shapiro$note
    label <- resolve_variable_label(labels, var)
    target <- if (identical(label, var)) var else paste0(var, " (", label, ")")
    add("Normality", "Shapiro-Wilk", target, shapiro$w, shapiro$p, nobs = length(values),
      decision = decision_from_p(shapiro$p, settings$alpha), note = note, status = status)
  }
  if (settings$normality != "none" && !length(cont_vars)) {
    add("Normality", "Shapiro-Wilk", nobs = 0L, note = "No continuous model variables.", status = "unavailable")
  }
  if (settings$mardia) {
    mardia <- tryCatch({
      if (!isTRUE(settings$packages$mvn)) stop("MVN package unavailable.")
      if (n <= 2L || p < 2L) stop("Insufficient complete continuous data.")
      # The direct exported test retains raw numeric probabilities; mvn() is a presentation wrapper.
      MVN::mardia(cont_complete, use_population = TRUE, bootstrap = FALSE)
    }, error = function(e) e)
    if (inherits(mardia, "error")) {
      add("Multivariate normality", "Mardia", note = conditionMessage(mardia), status = "unavailable")
    } else if (!is.data.frame(mardia) || !all(c("Test", "Statistic", "p.value") %in% names(mardia)) || nrow(mardia) != 2L) {
      add("Multivariate normality", "Mardia", note = "Unexpected MVN::mardia result schema.", status = "unavailable")
    } else for (i in seq_len(nrow(mardia))) {
      stat <- mardia$Statistic[i]; prob <- mardia$p.value[i]
      valid <- is.numeric(stat) && is.numeric(prob) && is.finite(stat) && is.finite(prob) && prob >= 0 && prob <= 1
      add("Multivariate normality", if (grepl("skew", mardia$Test[i], ignore.case = TRUE)) "Mardia (skew)" else "Mardia (kurtosis)",
        statistic = if (valid) stat else NA_real_, pvalue = if (valid) prob else NA_real_,
        decision = if (valid) decision_from_p(prob, settings$alpha) else "",
        note = if (valid) "" else "Nonfinite or nonnumeric Mardia result.", status = if (valid) "available" else "unavailable")
    }
  }
  if (settings$mahalanobis) {
    distances <- tryCatch({
      if (n <= 2L || p < 2L) stop("Insufficient complete continuous data.")
      values <- stats::mahalanobis(cont_complete, colMeans(cont_complete), stats::cov(cont_complete))
      if (any(!is.finite(values))) stop("Nonfinite Mahalanobis distances.")
      values
    }, error = function(e) e)
    if (inherits(distances, "error")) add("Outliers", "Mahalanobis distance",
      note = conditionMessage(distances), status = "unavailable") else {
      cutoff <- stats::qchisq(1 - settings$mahalanobis_alpha, df = p)
      count <- sum(distances > cutoff)
      diagnostics$mahalanobis <- list(source_rows = complete_rows, distances = as.numeric(distances),
        cutoff = cutoff, df = p, alpha = settings$mahalanobis_alpha, flagged_source_rows = complete_rows[distances > cutoff], count = count)
      add("Outliers", "Mahalanobis distance", value = max(distances), decision = decision_from_count(count),
        note = paste0("Chi² cutoff (alpha = ", settings$mahalanobis_alpha, "): ", format_stat(cutoff, settings$digits)))
    }
  }
  if (settings$collinearity) {
    matrix <- tryCatch({
      if (n <= 2L || p < 2L) stop("Insufficient complete continuous data.")
      value <- stats::cor(cont_complete)
      if (any(!is.finite(value))) stop("Correlation matrix contains unavailable entries.")
      value
    }, error = function(e) e)
    if (inherits(matrix, "error")) {
      for (name in c("Max |r|", "Condition number")) add("Multicollinearity", name,
        note = conditionMessage(matrix), status = "unavailable")
    } else {
      max_cor <- max(abs(matrix[upper.tri(matrix)]))
      condition <- tryCatch(kappa(matrix), error = function(e) NA_real_)
      add("Multicollinearity", "Max |r|", value = max_cor,
        decision = decision_from_threshold(max_cor, settings$max_cor), note = paste0("Threshold = ", settings$max_cor))
      add("Multicollinearity", "Condition number", value = if (is.finite(condition)) condition else NA_real_,
        decision = if (is.finite(condition)) decision_from_threshold(condition, settings$max_kappa) else "",
        note = if (is.finite(condition)) paste0("Threshold = ", settings$max_kappa) else "Condition number unavailable; singular/nonfinite matrix.",
        status = if (is.finite(condition)) "available" else "unavailable")
    }
  }
  if (settings$convergence || settings$heywood) {
    set.seed(settings$seed)
    warnings <- character()
    fit <- withCallingHandlers(fit_sem_model(if (sem_type %in% c("cfa", "invariance")) "cfa" else "sem",
      model_syntax, df, settings$estimator, settings$missing, settings$se, settings$bootstrap_samples,
      ordered_vars, group_var, group_equal),
      warning = function(w) { warnings <<- c(warnings, conditionMessage(w)) })
    audit <- sem_fit_audit(fit, requested = settings[c("estimator", "missing", "se", "bootstrap_samples")], source_n = nrow(df))
    audit$seed <- settings$seed; audit$rng_kind <- RNGkind(); audit$warnings <- unique(warnings)
    if (sem_type == "invariance") audit$scope <- "One explicitly constrained fit; no automatic invariance sequence in standalone diagnostics."
    diagnostics$fit <- audit
    if (settings$convergence) {
      ok <- audit$fit_status$converged
      add("Convergence", "lavaan", target = "Model", nobs = sum(audit$case_selection$nobs),
        decision = if (isTRUE(ok)) "ok" else "flag", note = paste(c(if (!isTRUE(ok)) "Model did not converge.", unique(warnings)), collapse = "; "))
    }
    if (settings$heywood) {
      pe <- tryCatch(lavaan::parameterEstimates(fit, standardized = TRUE), error = function(e) e)
      for (name in c("Negative variances", "Std. loading > 1")) {
        estimates <- if (inherits(pe, "error")) numeric() else if (name == "Negative variances") {
          pe$est[pe$op == "~~" & pe$lhs == pe$rhs]
        } else pe$std.all[pe$op == "=~"]
        applicable <- !inherits(pe, "error") && length(estimates) > 0L
        valid <- applicable && all(is.finite(estimates)) && isTRUE(audit$fit_status$converged)
        count <- if (!valid) NA_real_ else if (name == "Negative variances") sum(estimates < 0) else sum(abs(estimates) > 1)
        add("Heywood", name, target = "Model", value = count, nobs = sum(audit$case_selection$nobs),
          decision = if (valid) decision_from_count(count) else "",
          note = if (valid) paste0(name, ": ", count) else if (!inherits(pe, "error") && name == "Std. loading > 1" && !any(pe$op == "=~")) {
            "No factor loadings in this model; standardized-loading screen is not applicable."
          } else "Estimates unavailable or fit did not converge; no zero count inferred.",
          status = if (valid) "available" else "unavailable")
      }
    }
  }
  list(checks = if (length(rows)) do.call(rbind, rows) else data.frame(),
    model = model_syntax, sem_type = sem_type, vars = model_vars, diagnostics = diagnostics)
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
        paste0("Residual Shapiro-Wilk requested (alpha = ", settings$alpha, "); see each row for availability.")
      } else {
        "Residual normality not assessed."
      },
      if (settings$random_effects) "Random-effects Shapiro-Wilk requested; conditional modes are shrinkage estimates, not observed random effects." else "Random-effects normality not assessed.",
      if (settings$homoscedasticity) "Residual-fitted correlation requested for homoscedasticity screening." else "Homoscedasticity not assessed.",
      if (settings$homoscedasticity && isTRUE(settings$performance)) {
        "Additional performance::check_heteroscedasticity requested; failed or unavailable checks are not passes."
      } else {
        NULL
      },
      if (settings$singular) "Singular fit flagged via lme4::isSingular." else NULL,
      if (settings$convergence) "Convergence warnings reported from the optimizer." else NULL,
      if (settings$outliers) paste0("Outliers flagged at |std resid| > ", settings$outlier_z, ".") else NULL,
      if (settings$influence) paste0("Cook's D threshold = ", settings$cook_multiplier, "/n (clusters when available).") else NULL,
      if (settings$dharma && isTRUE(settings$packages$dharma)) paste0("DHARMa requested with seed=", settings$seed, ", n=250, refit=FALSE; check individual row availability.") else NULL,
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
        paste0("Univariate Shapiro-Wilk requested (alpha = ", settings$alpha, "); see row availability.")
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
      if (settings$mahalanobis) paste0("Mahalanobis screening requested at alpha = ", settings$mahalanobis_alpha, "; unavailable distances are not zero outliers.") else NULL,
      if (settings$collinearity) paste0("Collinearity thresholds: max |r| = ", settings$max_cor, ", condition number = ", settings$max_kappa, ".") else NULL,
      if (settings$heywood) "Negative variance and |std loading| > 1 checks requested; unavailable estimates are not passes." else NULL,
      if (settings$convergence) "Convergence reported from lavaan." else NULL,
      "Continuous-data screens pool observed values across groups; they are not fitted-model residual tests. Interpret in the design and estimator context."
    )
    note_default <- paste(parts[!is.null(parts) & nzchar(parts)], collapse = " ")
  } else {
    parts <- c(
      if (settings$normality != "none") {
        paste0("Normality assessed with Shapiro-Wilk (alpha = ", settings$alpha, ").")
      } else {
        "Normality not assessed."
      },
      paste0("Homogeneity tests requested/configured: ", homogeneity_label,
        ". Available homogeneity rows: ", sum(checks_df$assumption == "Homogeneity" & checks_df$status == "available"),
        "; unavailable/skipped homogeneity rows: ", sum(checks_df$assumption == "Homogeneity" & checks_df$status != "available"),
        ". Without independent groups these checks do not apply.")
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

assumptions_number <- function(value, name, lower = 0, upper = Inf, integer = FALSE, inclusive_lower = FALSE) {
  number <- suppressWarnings(as.numeric(value))
  if (is.logical(value) || length(number) != 1L || !is.finite(number) ||
      (if (inclusive_lower) number < lower else number <= lower)) stop("Invalid numeric option --", name, ".")
  if (number > upper || (integer && number != floor(number))) stop("Invalid numeric option --", name, ".")
  number
}

main <- function() {
  opts <- nlss_run_options(commandArgs(trailingOnly = TRUE), "assumptions")
  if (parse_bool(opts[["help"]])) { print_usage(); return(invisible(NULL)) }
  if (parse_bool(opts[["interactive"]])) opts <- modifyList(opts, interactive_options())
  present <- function(key) !is.null(opts[[key]]) && length(opts[[key]]) && nzchar(as.character(opts[[key]]))
  sources <- c("csv", "sav", "rds", "rdata", "parquet")
  if (sum(vapply(sources, present, logical(1))) > 1L) stop("Supply exactly one input source.")
  if (present("df") && !present("rdata")) stop("--df selects an RData object and requires --rdata.")
  if ((present("dataset-name") || present("import-action")) && !any(vapply(sources, present, logical(1)))) stop("--dataset-name/--import-action require an explicit source; they do not select the active dataset.")
  csv_options <- c("sep", "header", "csv-decimal", "csv-encoding", "csv-col-types", "csv-na-values")
  if (!present("csv") && any(vapply(csv_options, present, logical(1)))) stop("CSV reader options require --csv.")
  input <- if (present("analysis")) opts[["analysis"]] else get_config_value("modules.assumptions.analysis")
  analysis <- normalize_analysis_type(input)
  if (analysis == "auto") {
    analysis <- if (any(vapply(c("formula", "random", "fixed"), present, logical(1)))) "mixed_models" else
      if (any(vapply(c("model", "model-file", "paths", "factors", "m", "ordered", "group-equal", "invariance"), present, logical(1)))) "sem" else
      if (present("ivs") || present("blocks")) "regression" else
      if (present("within") || present("between") || present("dv")) "anova" else "ttest"
  }
  inherited <- switch(analysis, mixed_models = "mixed_models", sem = "sem", character())
  df <- nlss_load_input(opts)
  out_dir <- get_workspace_out_dir(df)
  nlss_begin_run("assumptions", df, opts, out_dir, config_modules = inherited)
  # Defaults for another analysis family must never affect or pollute a request.
  read_option <- function(key, path = paste0("modules.assumptions.", gsub("-", "_", key))) {
    if (present(key)) opts[[key]] else get_config_value(path)
  }
  number <- function(key, path = paste0("modules.assumptions.", gsub("-", "_", key)), ...) assumptions_number(read_option(key, path), key, ...)
  settings <- list(
    alpha = number("alpha", upper = 1 - .Machine$double.eps),
    digits = number("digits", "defaults.digits", upper = 15, integer = TRUE, inclusive_lower = TRUE),
    vars_default = get_config_value("modules.assumptions.vars_default"),
    normality = tolower(read_option("normality")), homogeneity_tests = character(),
    vif_warn = number("vif-warn"), vif_high = number("vif-high"),
    outlier_z = number("outlier-z"), cook_multiplier = number("cook-multiplier"),
    max_shapiro_n = number("max-shapiro-n", upper = .Machine$integer.max, integer = TRUE))
  if (!settings$normality %in% c("none", "shapiro")) stop("--normality must be shapiro or none.")
  if (settings$vif_high < settings$vif_warn) stop("--vif-high must not be below --vif-warn.")
  roles <- list(ttest = c("vars", "group", "x", "y"), anova = c("dv", "between", "within", "subject-id"),
    regression = c("dv", "ivs", "blocks"), mixed_models = c("dv", "formula", "fixed", "random"),
    sem = c("model", "model-file", "paths", "factors", "dv", "ivs", "x", "m", "y", "covariates", "serial", "group", "group-equal", "ordered"))
  switches <- list(ttest = "homogeneity", anova = "homogeneity",
    regression = c("linearity", "homoscedasticity", "vif", "durbin-watson", "outliers", "influence"),
    mixed_models = c("homoscedasticity", "outliers", "influence", "random-effects", "singular", "convergence", "dharma", "performance", "reml", "optimizer", "maxfun", "seed"),
    sem = c("estimator", "missing", "se", "ci", "std", "bootstrap", "bootstrap-samples", "seed", "mardia", "mahalanobis", "mahalanobis-alpha", "collinearity", "max-cor", "max-kappa", "heywood", "convergence"))
  inapplicable <- setdiff(unique(c(unlist(roles), unlist(switches), "invariance")), c(roles[[analysis]], switches[[analysis]]))
  invalid <- inapplicable[vapply(inapplicable, present, logical(1))]
  if (length(invalid)) stop("Options not supported for ", analysis, " diagnostics: --", paste(invalid, collapse = ", --"),
    if ("covariates" %in% invalid && analysis == "anova") ". ANCOVA diagnostic matching is not implemented here; use the fitted analysis diagnostics." else "")
  if (analysis %in% c("ttest", "anova")) settings$homogeneity_tests <- parse_homogeneity_tests(read_option("homogeneity"))
  if (analysis %in% c("regression", "mixed_models")) for (key in c("linearity", "homoscedasticity", "vif", "durbin-watson", "outliers", "influence"))
    settings[[gsub("-", "_", key)]] <- parse_bool(read_option(key))
  settings$seed <- NULL
  if (analysis == "mixed_models") {
    for (key in c("random-effects", "singular", "convergence", "dharma", "performance"))
      settings[[gsub("-", "_", key)]] <- parse_bool(read_option(key, paste0("modules.assumptions.mixed_models.", gsub("-", "_", key))))
    settings$reml <- parse_bool(read_option("reml", "modules.mixed_models.reml"))
    settings$optimizer <- as.character(read_option("optimizer", "modules.mixed_models.optimizer"))
    settings$maxfun <- number("maxfun", "modules.mixed_models.maxfun", upper = .Machine$integer.max, integer = TRUE)
    settings$seed <- number("seed", "modules.assumptions.mixed_models.seed", upper = .Machine$integer.max, integer = TRUE, inclusive_lower = TRUE)
  }
  if (analysis == "sem") {
    settings$sem_type <- normalize_sem_type(input, get_config_value("modules.sem.analysis"))
    if (tolower(input) == "auto") settings$sem_type <- if (present("m")) "mediation" else if (present("factors")) "cfa" else if (present("dv") && present("ivs")) "path" else settings$sem_type
    domains <- list(estimator = c("ML", "MLR", "MLM", "MLMV", "MLMVS", "WLSMV", "ULSMV", "DWLS", "ULS", "GLS"),
      missing = c("fiml", "ml", "listwise", "list", "pairwise", "pair"),
      se = c("standard", "none", "default", "robust", "sandwich", "bootstrap", "boot"),
      ci = c("standard", "normal", "none", "bootstrap", "boot", "perc", "percentile", "bca", "bca.simple", "bca_simple"),
      std = c("none", "no", "false", "std.lv", "std_lv", "latent", "std.all", "std_all", "all"))
    for (key in names(domains)) {
      value <- read_option(key, paste0("modules.sem.", key))
      if (!tolower(value) %in% tolower(domains[[key]])) stop("Invalid SEM diagnostic option: --", key)
      settings[[key]] <- get(paste0("normalize_", key), mode = "function")(value)
    }
    settings$bootstrap <- parse_bool(read_option("bootstrap", "modules.sem.bootstrap")) || settings$se == "bootstrap"
    if (settings$bootstrap) settings$se <- "bootstrap"
    settings$bootstrap_samples <- number("bootstrap-samples", "modules.sem.bootstrap_samples", upper = .Machine$integer.max, integer = TRUE)
    settings$seed <- number("seed", "modules.sem.seed", upper = .Machine$integer.max, integer = TRUE, inclusive_lower = TRUE)
    for (key in c("mardia", "mahalanobis", "collinearity", "heywood", "convergence"))
      settings[[key]] <- parse_bool(read_option(key, paste0("modules.assumptions.sem.", key)))
    settings$mahalanobis_alpha <- number("mahalanobis-alpha", "modules.assumptions.sem.mahalanobis_alpha", upper = 1 - .Machine$double.eps)
    settings$max_cor <- number("max-cor", "modules.assumptions.sem.max_cor", upper = 1)
    settings$max_kappa <- number("max-kappa", "modules.assumptions.sem.max_kappa")
  }
  settings$packages <- list()
  if (analysis == "mixed_models") settings$packages <- list(performance = is_pkg_available("performance"),
    influence = is_pkg_available("influence.ME"), dharma = is_pkg_available("DHARMa"))
  if (analysis == "sem") settings$packages <- list(mvn = is_pkg_available("MVN"))
  nlss_run_seed(settings$seed)
  options <- c(list(analysis = analysis, homogeneity = settings$homogeneity_tests), settings)
  nlss_resolve_request(options, list(source_n = nrow(df), source_classes = lapply(df, class)))
  result <- switch(analysis, ttest = run_ttest_assumptions(df, opts, settings), anova = run_anova_assumptions(df, opts, settings),
    regression = run_regression_assumptions(df, opts, settings), mixed_models = run_mixed_models_assumptions(df, opts, settings),
    sem = run_sem_assumptions(df, opts, settings))
  checks_df <- result$checks
  if (is.null(checks_df) || !nrow(checks_df)) stop("No diagnostic checks requested or applicable; no completed diagnostic report was produced.")
  if (any(!checks_df$status %in% c("available", "skipped", "unavailable"))) stop("Invalid diagnostic availability status.")
  labels <- resolve_label_metadata(df)
  checks_df$target_label <- vapply(checks_df$target, function(target) resolve_variable_label(labels, target), character(1))
  group_var <- if (present("group")) opts[["group"]] else NULL
  checks_df$group_label <- if (is.null(group_var)) checks_df$group else vapply(checks_df$group, function(value) resolve_value_label(labels, group_var, value), character(1))
  result$diagnostics$labels <- labels
  if (is.null(result$diagnostics$source_n)) result$diagnostics$source_n <- nrow(df)
  options$mode <- result$mode
  design <- list(source_n = nrow(df), source_classes = lapply(df, class), mode = result$mode,
    model_syntax = if (analysis == "sem") result$model else NULL, diagnostics = result$diagnostics)
  nlss_resolve_request(options, design)
  results <- list(checks_df = checks_df, diagnostics = result$diagnostics)
  nlss_set_result(results)
  template <- resolve_template_override(opts[["template"]], module = "assumptions")
  if (is.null(template)) template <- resolve_template_path(paste0("assumptions.", analysis))
  template <- nlss_freeze_template(template, "assumptions.main")
  tokens <- build_note_tokens(analysis, settings, settings$homogeneity_tests, checks_df)
  tokens$note_default <- paste(tokens$note_default, "Availability is explicit. Diagnostic flags are screens, not a blanket analysis approval or rejection.")
  if (analysis == "sem") tokens$note_default <- paste(tokens$note_default, result$diagnostics$reporting$note,
    result$diagnostics$model_specification$note)
  if (analysis == "regression") tokens$note_default <- paste(tokens$note_default,
    "Linearity uses separate centered quadratic added-term F tests. Hierarchical blocks use common complete cases. VIF is per design column, not term GVIF.")
  narrative_rows <- build_assumptions_narrative_rows(checks_df, settings$digits)
  narrative <- paste(vapply(narrative_rows, function(row) row$full_sentence, character(1)), collapse = "\n")
  table <- build_assumptions_table_body(checks_df, settings$digits, get_template_meta(template)$table)
  flags <- c(list(analysis = analysis, mode = result$mode), opts[intersect(names(opts), roles[[analysis]])],
    settings[setdiff(names(settings), c("packages", "vars_default"))])
  if (!is.null(flags[["serial"]])) flags[["serial"]] <- parse_bool(flags[["serial"]])
  flags[["model-file"]] <- NULL
  if (analysis == "sem") flags$model <- result$model
  if (analysis == "mixed_models") flags$formula <- result$model
  nlss_stage_report(file.path(out_dir, "report_canonical.md"), "Assumption checks",
    paste0("Table 1\n\n", table, "\n", tokens$note_default), narrative,
    analysis_flags = flags, template_path = template,
    template_context = list(tokens = c(list(table_body = table, narrative_default = narrative), tokens), narrative_rows = narrative_rows))
  if (parse_bool(opts[["log"]], get_config_value("defaults.log"))) {
    ctx <- get_run_context()
    nlss_stage_log(out_dir, "assumptions", ctx$prompt, ctx$commands, results, options, get_user_prompt(opts))
  }
}

nlss_run_main("assumptions", main)
