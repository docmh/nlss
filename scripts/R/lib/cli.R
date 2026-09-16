# SPDX-License-Identifier: Apache-2.0
cli_module_options <- function() {
  # Explicit entrypoint contracts; shared import options are added below. Keep
  # this registry in step with module options (covered by phase1/cli_config.R).
  list(
    anova = "between bootstrap bootstrap-samples conf-level contrast-file contrasts covariates digits dv effect-size emmeans p-adjust posthoc seed sphericity subject-id type within",
    assumptions = "alpha analysis between blocks bootstrap bootstrap-samples ci collinearity convergence cook-multiplier covariates dharma digits durbin-watson dv estimator factors fixed formula group group-equal heywood homogeneity homoscedasticity influence invariance ivs linearity m mahalanobis mahalanobis-alpha mardia max-cor max-kappa max-shapiro-n maxfun missing model model-file normality optimizer ordered outlier-z outliers paths performance random random-effects reml se seed serial singular std subject-id vars vif vif-high vif-warn within x y",
    calc = "digits expr format set unsafe",
    correlations = "alternative bootstrap bootstrap-samples coerce compare-groups conf-level controls digits group method missing p-adjust r0 seed vars x y",
    crosstabs = "chisq col cols digits expected fisher fisher-b fisher-conf-level fisher-simulate group nlss-percent percent residuals row rows seed yates",
    data_explorer = "digits max-levels top-n vars",
    data_transform = "bins bins-into bins-suffix calc coerce confirm-drop confirm-overwrite drop overwrite-vars percentile-bins percentile-into percentile-suffix recode recode-into recode-suffix rename standardize standardize-into standardize-suffix transform transform-into",
    descriptive_stats = "digits group iqr-multiplier outlier-z trim vars",
    efa = "coerce cor digits eigen-threshold group loading-cutoff method missing n-factors rotation seed sort-loadings vars",
    frequencies = "digits group include-numeric vars",
    impute = "categorical-method constant digits engine indicator indicator-suffix k m maxit method-map numeric-method seed skew-threshold suffix value-map vars",
    mi_regression = "mids formula family link conf-level maxit digits",
    init_workspace = "agent",
    metaskill_runner = "intent label meta notes phase synopsis",
    missings = "digits drop-threshold high-threshold indicator-suffix indicator-threshold low-threshold max-patterns method moderate-threshold skew-threshold vars",
    mixed_models = "conf-level contrast-file contrasts df-method diagnostics digits dv emmeans fixed formula max-shapiro-n maxfun optimizer p-adjust random reml seed standardize type",
    nonparametric = "alternative conf-level continuity digits effect-size exact group mu p-adjust posthoc subject-id test vars within x y",
    plot = "alpha bins binwidth bw caption digits dpi figure-number file_prefix file_suffix file-prefix file-suffix format group height na_action na-action note overwrite palette percent_base percent-base position se smooth span stat subtitle summary theme title type vars width x y",
    power = "alpha alternative analysis between digits dv effect-basis effect-metric effect-size estimate-effect group groups ivs mode mu n n-per-group n-total n1 n2 planning power ratio rmsea0 rmsea1 sem-df t-type u vars x y",
    regression = "blocks bootstrap bootstrap-samples center conf-level digits dv family group interactions ivs link seed standardize",
    reliability = "analysis coerce conf-level digits expect-invalid format group icc-model icc-type icc-unit id kappa-weight method missing rater score vars",
    research_academia = "max-per-source max-total query semantic-key sources timeout top-n topic year-from year-to",
    scale = "coerce digits group missing omega reverse reverse-max reverse-min score vars",
    sem = "analysis bootstrap bootstrap-samples ci conf-level covariates digits dv estimator factors fit group group-equal invariance ivs m missing model model-file modindices ordered paths r2 residuals se seed serial std x y",
    t_test = "alternative bootstrap bootstrap-samples conf-level digits expect-two-groups group mu seed var-equal vars x y"
  )
}

cli_entrypoint <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(file_arg)) return(NULL)
  name <- tools::file_path_sans_ext(basename(sub("^--file=", "", file_arg[[1]])))
  if (name %in% names(cli_module_options())) name else NULL
}

# Module-neutral selection for the shared location boundary. Input-file flags
# retain their meaning: selecting an output project does not select its active
# dataset instead of the explicitly supplied file. Publisher wiring follows in B.
cli_resolve_locations <- function(opts, use_dataset = TRUE, start = getwd()) {
  source_flags <- c("csv", "sav", "rds", "rdata", "parquet", "mids")
  has_source <- any(vapply(source_flags, function(key) !is.null(opts[[key]]), logical(1)))
  if (has_source && !is.null(opts[["dataset"]])) stop("Choose a registered --dataset or an input file, not both.", call. = FALSE)
  nlss_resolve_locations(project = opts$project, dataset = opts[["dataset"]],
                         start = start, use_dataset = use_dataset && !has_source)
}

cli_option_schema <- function(module = cli_entrypoint()) {
  split_options <- function(x) strsplit(x, " ", fixed = TRUE)[[1]]
  # Setup/recovery can parse the same target options without running the target.
  utilities <- list(
    project_create = "help project source working name df sep header csv-decimal csv-encoding csv-col-types csv-na-values",
    project_inspect = "help project verify format",
    project_report = "help action project report-id report runs revision verify",
    replay_run = "help request")
  if (!is.null(module) && module %in% names(utilities)) {
    allowed <- split_options(utilities[[module]])
    return(list(allowed = allowed, boolean = intersect(allowed, c("help", "header", "verify", "overwrite"))))
  }
  registry <- cli_module_options()
  common <- c("help", "interactive", "log", "template", "user-prompt", "project")
  input <- c("csv", "sav", "rds", "rdata", "parquet", "df", "sep", "header",
             "dataset-name", "import-action", "csv-decimal", "csv-encoding",
             "csv-col-types", "csv-na-values", "dataset")
  if (is.null(module)) {
    allowed <- unique(c(common, input, unlist(lapply(registry, split_options))))
  } else {
    if (!module %in% names(registry)) stop("Unknown CLI entrypoint: ", module, call. = FALSE)
    allowed <- c(common, split_options(registry[[module]]))
    if (!module %in% c("calc", "research_academia", "mi_regression")) allowed <- c(allowed, input)
  }
  boolean <- c("help", "interactive", "log", "header", "bootstrap", "coerce",
               "compare-groups", "chisq", "expected", "fisher", "fisher-simulate",
               "residuals", "yates", "confirm-drop", "confirm-overwrite",
               "overwrite-vars", "sort-loadings", "include-numeric", "indicator",
               "diagnostics", "reml", "continuity", "overwrite", "estimate-effect",
               "expect-invalid", "omega", "r2", "serial", "expect-two-groups",
               "var-equal", "unsafe", "collinearity", "convergence", "dharma",
               "durbin-watson", "heywood", "homoscedasticity", "influence",
               "linearity", "mahalanobis", "mardia", "outliers", "performance",
               "random-effects", "singular", "vif", "planning")
  # --se is a Boolean for plots, but an estimator name for SEM/assumptions.
  if (identical(module, "plot")) boolean <- c(boolean, "se")
  list(allowed = unique(allowed), boolean = intersect(boolean, allowed))
}

parse_args <- function(args, module = cli_entrypoint(), allowed = NULL, boolean = NULL) {
  schema <- cli_option_schema(module)
  if (is.null(allowed)) allowed <- schema$allowed
  if (is.null(boolean)) boolean <- schema$boolean
  opts <- list()
  i <- 1
  while (i <= length(args)) {
    arg <- args[i]
    if (is.na(arg) || !grepl("^--[A-Za-z][A-Za-z0-9_-]*(=|$)", arg)) {
      stop("Unexpected argument. Use named --options (see --help).", call. = FALSE)
    }
    token <- sub("^--", "", arg)
    equals <- regexpr("=", token, fixed = TRUE)[[1]]
    key <- if (equals > 0L) substr(token, 1L, equals - 1L) else token
    if (!key %in% allowed) {
      stop("Unknown option --", key, if (!is.null(module)) paste0(" for ", module),
           ". See --help.", call. = FALSE)
    }
    if (key %in% names(opts)) stop("Duplicate option --", key, ". Supply it once.", call. = FALSE)
    if (equals > 0L) {
      # Only the first '=' separates the option name from its value. Expressions,
      # formulas, labels and query strings may contain further equals signs.
      value <- substring(token, equals + 1L)
    } else if (i < length(args) && !grepl("^--", args[i + 1])) {
      value <- args[i + 1]
      i <- i + 1
    } else if (key %in% boolean) {
      value <- TRUE
    } else {
      stop("Missing value for --", key, ".", call. = FALSE)
    }
    if (key %in% boolean) {
      tryCatch(parse_bool(value), error = function(e) {
        stop("Invalid Boolean for --", key, ": expected TRUE/FALSE (or yes/no, 1/0).", call. = FALSE)
      })
    }
    opts[[key]] <- value
    i <- i + 1
  }
  if (exists("nlss_dependency_cli", mode = "function")) nlss_dependency_cli(opts, module)
  if (!is.null(module) && !isTRUE(parse_bool(opts$help)) && exists("nlss_run_context", mode = "environment")) {
    # Resolve once at the shared CLI boundary so utilities and dataset-free
    # calculations also honor --project; do not change the process cwd.
    locations <- cli_resolve_locations(opts, use_dataset = !is.null(opts[["dataset"]]))
    if (!is.null(opts[["dataset"]])) {
      if ("planning" %in% allowed) {
        if (!is.null(opts$planning) && parse_bool(opts$planning)) stop("--dataset cannot be used with parameter-only planning.", call. = FALSE)
        opts$planning <- FALSE
      }
    }
    nlss_run_context$project_selection <- locations$project_root
  }
  opts
}

parse_bool <- function(value, default = FALSE) {
  if (is.null(value)) return(default)
  if (length(value) != 1L || is.na(value)) {
    stop("Invalid Boolean: expected one TRUE/FALSE value.", call. = FALSE)
  }
  if (is.logical(value)) return(value)
  val <- tolower(trimws(as.character(value)))
  if (val %in% c("true", "t", "1", "yes", "y")) return(TRUE)
  if (val %in% c("false", "f", "0", "no", "n")) return(FALSE)
  stop("Invalid Boolean: expected TRUE/FALSE (or yes/no, 1/0).", call. = FALSE)
}

parse_list <- function(value, sep = ",") {
  if (is.null(value) || is.logical(value)) return(character(0))
  value <- as.character(value)
  if (value == "") return(character(0))
  trimws(strsplit(value, sep, fixed = TRUE)[[1]])
}

prompt <- local({
  cache <- new.env(parent = emptyenv())
  cache$lines <- NULL
  cache$index <- 1
  cache$path <- NULL

  function(label, default = NULL) {
    required_path <- is.null(default) && grepl("path", label, ignore.case = TRUE)

    repeat {
      prompt_file <- Sys.getenv("NLSS_PROMPT_FILE", "")
      if (nzchar(prompt_file)) {
        if (is.null(cache$lines) || is.null(cache$path) || !identical(cache$path, prompt_file)) {
          cache$lines <- readLines(prompt_file, warn = FALSE)
          cache$index <- 1
          cache$path <- prompt_file
        }
        answer <- ""
        if (!is.null(cache$lines) && cache$index <= length(cache$lines)) {
          answer <- cache$lines[[cache$index]]
          cache$index <- cache$index + 1
        }
        if (!is.null(default) && answer == "") answer <- default
        if (required_path && (is.na(answer) || !nzchar(answer))) {
          stop("Prompt file is missing a required path for: ", label)
        }
        return(answer)
      }

      if (is.null(default)) {
        answer <- readline(paste0(label, ": "))
      } else {
        answer <- readline(paste0(label, " [", default, "]: "))
        if (answer == "") answer <- default
      }

      if (required_path && (is.na(answer) || !nzchar(answer))) {
        cat("Path is required.\n")
        next
      }
      return(answer)
    }
  }
})
