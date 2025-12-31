# Copyright (c) 2026 Mike Hammes
# SPDX-License-Identifier: Apache-2.0
config_env <- new.env(parent = emptyenv())

resolve_script_dir <- function() {
  if (exists("get_script_dir", mode = "function")) {
    return(get("get_script_dir", mode = "function")())
  }
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", cmd_args[grep("^--file=", cmd_args)])
  if (length(file_arg) > 0 && nzchar(file_arg[1])) {
    return(dirname(normalizePath(file_arg[1], winslash = "/", mustWork = FALSE)))
  }
  frame_file <- tryCatch(sys.frames()[[1]]$ofile, error = function(e) NULL)
  if (!is.null(frame_file) && nzchar(frame_file)) {
    return(dirname(normalizePath(frame_file, winslash = "/", mustWork = FALSE)))
  }
  getwd()
}

get_config_path <- function() {
  script_dir <- resolve_script_dir()
  candidates <- c(
    file.path(script_dir, "..", "config.yml"),
    file.path(script_dir, "config.yml")
  )
  for (path in candidates) {
    if (file.exists(path)) {
      return(normalizePath(path, winslash = "/", mustWork = FALSE))
    }
  }
  normalizePath(candidates[1], winslash = "/", mustWork = FALSE)
}

get_builtin_config <- function() {
  list(
    version = 1,
    nlss_version = "1.0.0",
    defaults = list(
      output_dir = "./outputs/tmp",
      workspace_manifest = "nlss-workspace.yml",
      csv = list(
        sep = ",",
        header = TRUE
      ),
      log = TRUE,
      log_nlss_checksum = TRUE,
      digits = 2,
      interactive = FALSE
    ),
    modules = list(
      descriptive_stats = list(
        vars_default = "numeric",
        trim = 0.1,
        iqr_multiplier = 1.5,
        outlier_z = 3
      ),
      frequencies = list(
        vars_default = "non-numeric",
        include_numeric = FALSE
      ),
      data_explorer = list(
        vars_default = "all",
        max_levels = 20,
        top_n = 10
      ),
      plot = list(
        type = "auto",
        vars_default = "numeric",
        stat = "count",
        percent_base = "total",
        bins = 30,
        binwidth = NULL,
        bw = NULL,
        smooth = "none",
        se = TRUE,
        span = 0.75,
        summary = "none",
        theme = "minimal",
        palette = "default",
        alpha = 0.7,
        position = "dodge",
        format = "png",
        width = 7,
        height = 5,
        dpi = 300,
        na_action = "omit",
        figure_digits = 3,
        file_prefix = "figure"
      ),
      correlations = list(
        vars_default = "numeric",
        method = "pearson",
        missing = "pairwise",
        alternative = "two.sided",
        controls = NULL,
        p_adjust = "none",
        conf_level = 0.95,
        bootstrap = FALSE,
        bootstrap_samples = 1000,
        compare_groups = FALSE,
        coerce = FALSE
      ),
      scale = list(
        vars_default = "numeric",
        missing = "pairwise",
        score = "sum",
        omega = TRUE,
        coerce = FALSE,
        reverse_min = NULL,
        reverse_max = NULL
      ),
      efa = list(
        vars_default = "numeric",
        method = "pca",
        rotation = "varimax",
        n_factors = "eigen",
        eigen_threshold = 1,
        cor = "pearson",
        missing = "complete",
        loading_cutoff = 0.3,
        sort_loadings = TRUE,
        coerce = FALSE
      ),
      reliability = list(
        analysis = "icc",
        format = "wide",
        missing = "complete",
        conf_level = 0.95,
        icc_model = "twoway-random",
        icc_type = "agreement",
        icc_unit = "single",
        kappa_weight = "none",
        method = "pearson",
        coerce = FALSE
      ),
      crosstabs = list(
        percent = "all",
        nlss_percent = "row",
        chisq = TRUE,
        yates = FALSE,
        fisher = FALSE,
        fisher_simulate = FALSE,
        fisher_b = 2000,
        fisher_conf_level = 0.95,
        expected = TRUE,
        residuals = TRUE
      ),
      data_transform = list(
        standardize_suffix = "_z",
        percentile_suffix = "_pct",
        bins_suffix = "_bin",
        recode_suffix = "_rec",
        coerce = FALSE,
        overwrite_vars = FALSE,
        confirm_overwrite = FALSE,
        confirm_drop = FALSE
      ),
      t_test = list(
        vars_default = "numeric",
        mu = 0.0,
        alternative = "two.sided",
        var_equal = FALSE,
        conf_level = 0.95,
        bootstrap = FALSE,
        bootstrap_samples = 1000
      ),
      nonparametric = list(
        vars_default = "numeric",
        test = "auto",
        mu = 0.0,
        alternative = "two.sided",
        conf_level = 0.95,
        exact = "auto",
        continuity = TRUE,
        posthoc = "none",
        p_adjust = "holm",
        effect_size = "r"
      ),
      anova = list(
        type = "II",
        effect_size = "partial_eta",
        conf_level = 0.95,
        posthoc = "tukey",
        emmeans = "none",
        contrasts = "none",
        p_adjust = "holm",
        sphericity = "auto",
        bootstrap = FALSE,
        bootstrap_samples = 1000
      ),
      regression = list(
        ivs_default = "numeric",
        family = "gaussian",
        link = "",
        conf_level = 0.95,
        center = "none",
        standardize = "none",
        bootstrap = FALSE,
        bootstrap_samples = 1000
      ),
      power = list(
        analysis = "ttest",
        mode = "apriori",
        effect_metric = "auto",
        alpha = 0.05,
        power = 0.8,
        alternative = "two.sided",
        t_type = "two-sample",
        ratio = 1,
        mu = 0.0,
        groups = 2,
        u = 1,
        rmsea0 = 0.05,
        rmsea1 = 0.08,
        estimate_effect = FALSE
      ),
      mixed_models = list(
        reml = TRUE,
        type = "III",
        df_method = "satterthwaite",
        standardize = "none",
        emmeans = "none",
        contrasts = "none",
        p_adjust = "holm",
        conf_level = 0.95,
        optimizer = "bobyqa",
        maxfun = 100000,
        diagnostics = TRUE,
        max_shapiro_n = 5000
      ),
      sem = list(
        analysis = "sem",
        estimator = "MLR",
        missing = "fiml",
        se = "robust",
        ci = "standard",
        conf_level = 0.95,
        bootstrap = FALSE,
        bootstrap_samples = 5000,
        std = "std.all",
        fit = "chisq,df,cfi,tli,rmsea,srmr",
        r2 = TRUE,
        modindices = 0,
        residuals = FALSE,
        invariance = "configural,metric,scalar,strict"
      ),
      assumptions = list(
        analysis = "auto",
        vars_default = "numeric",
        normality = "shapiro",
        homogeneity = "levene",
        linearity = TRUE,
        homoscedasticity = TRUE,
        vif = TRUE,
        durbin_watson = TRUE,
        outliers = TRUE,
        influence = TRUE,
        alpha = 0.05,
        vif_warn = 5,
        vif_high = 10,
        outlier_z = 3,
        cook_multiplier = 4,
        max_shapiro_n = 5000,
        mixed_models = list(
          random_effects = TRUE,
          singular = TRUE,
          convergence = TRUE,
          dharma = FALSE,
          performance = TRUE
        ),
        sem = list(
          mardia = TRUE,
          mahalanobis = TRUE,
          mahalanobis_alpha = 0.001,
          collinearity = TRUE,
          max_cor = 0.9,
          max_kappa = 30,
          heywood = TRUE,
          convergence = TRUE
        )
      ),
      impute = list(
        vars_default = "all",
        engine = "auto",
        numeric_method = "median",
        categorical_method = "mode",
        skew_threshold = 1,
        suffix = "_imp",
        indicator = FALSE,
        indicator_suffix = "_miss",
        m = 5,
        maxit = 5,
        k = 5,
        seed = NULL
      ),
      missings = list(
        vars_default = "all",
        method = "auto",
        low_threshold = 0.05,
        moderate_threshold = 0.2,
        high_threshold = 0.4,
        drop_threshold = 0.6,
        indicator_threshold = 0.3,
        indicator_suffix = "_miss",
        skew_threshold = 1,
        max_patterns = 10
      ),
      research_academia = list(
        sources = "openalex,crossref",
        max_per_source = 50,
        max_total = 200,
        top_n = 10,
        timeout = 30,
        abstract_limit = 200,
        keywords_limit = 80
      ),
      init_workspace = list(
        agent = "Codex"
      ),
      metaskill_runner = list(
        meta_default = "",
        analysis_label = "Metaskill activation",
        note_default = "This entry logs metaskill activation/finalization only; analyses are logged separately."
      )
    ),
    templates = list(
      descriptive_stats = list(
        default = "descriptive-stats/default-template.md",
        robust = "descriptive-stats/robust-template.md",
        distribution = "descriptive-stats/distribution-template.md"
      ),
      frequencies = list(
        default = "frequencies/default-template.md",
        grouped = "frequencies/grouped-template.md"
      ),
      data_explorer = list(
        default = "data-explorer/default-template.md"
      ),
      plot = list(
        default = "plot/default-template.md"
      ),
      data_transform = list(
        default = "data-transform/default-template.md"
      ),
      correlations = list(
        default = "correlations/default-template.md",
        cross = "correlations/cross-correlation-template.md",
        matrix = "correlations/matrix-template.md",
        comparison = "correlations/comparison-template.md"
      ),
      crosstabs = list(
        default = "crosstabs/default-template.md",
        grouped = "crosstabs/grouped-template.md"
      ),
      scale = list(
        default = "scale/default-template.md"
      ),
      efa = list(
        default = "efa/default-template.md"
      ),
      reliability = list(
        default = "reliability/default-template.md"
      ),
      t_test = list(
        default = "t-test/default-template.md"
      ),
      nonparametric = list(
        default = "nonparametric/default-template.md",
        posthoc = "nonparametric/posthoc-template.md"
      ),
      anova = list(
        default = "anova/default-template.md",
        posthoc = "anova/posthoc-template.md",
        contrasts = "anova/contrasts-template.md"
      ),
      regression = list(
        default = "regression/default-template.md",
        model_tests = "regression/model-tests-template.md"
      ),
      power = list(
        default = "power/default-template.md"
      ),
      mixed_models = list(
        default = "mixed-models/default-template.md",
        tests = "mixed-models/tests-of-fixed-effects-template.md",
        emmeans = "mixed-models/emmeans-template.md"
      ),
      sem = list(
        default = "sem/default-template.md",
        cfa = "sem/cfa-template.md",
        mediation = "sem/mediation-template.md",
        invariance = "sem/invariance-template.md"
      ),
      assumptions = list(
        ttest = "assumptions/ttest-template.md",
        anova = "assumptions/anova-template.md",
        regression = "assumptions/regression-template.md",
        mixed_models = "assumptions/mixed-models-template.md",
        sem = "assumptions/sem-template.md"
      ),
      impute = list(
        default = "impute/default-template.md"
      ),
      missings = list(
        default = "missings/default-template.md"
      ),
      init_workspace = list(
        default = "init-workspace/default-template.md",
        scratchpad = "scratchpad/default-template.md"
      ),
      metaskill_report = list(
        default = "metaskills/report-template.md"
      ),
      metaskill_runner = list(
        default = "metaskill-runner/default-template.md",
        finalization = "metaskill-runner/finalization-template.md"
      ),
      calc = list(
        default = "calc/default-template.md"
      ),
      research_academia = list(
        default = "research-academia/default-template.md"
      )
    )
  )
}

merge_lists <- function(base, override) {
  if (is.null(override)) return(base)
  if (!is.list(base) || !is.list(override)) return(override)
  out <- base
  for (name in names(override)) {
    if (name %in% names(base)) {
      out[[name]] <- merge_lists(base[[name]], override[[name]])
    } else {
      out[[name]] <- override[[name]]
    }
  }
  out
}

load_config_file <- function(path) {
  if (!file.exists(path)) return(NULL)
  if (!requireNamespace("yaml", quietly = TRUE)) return(NULL)
  yaml::yaml.load_file(path)
}

load_config <- function() {
  base <- get_builtin_config()
  path <- get_config_path()
  user_config <- load_config_file(path)
  merge_lists(base, user_config)
}

get_config <- function() {
  if (exists("config", envir = config_env, inherits = FALSE)) {
    return(config_env$config)
  }
  config_env$config <- load_config()
  config_env$config
}

get_config_value <- function(path, default = NULL) {
  cfg <- get_config()
  if (is.null(path) || path == "") return(cfg)
  parts <- strsplit(path, ".", fixed = TRUE)[[1]]
  val <- cfg
  for (part in parts) {
    if (!is.list(val) || !part %in% names(val)) return(default)
    val <- val[[part]]
  }
  if (is.null(val)) return(default)
  val
}
