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
    defaults = list(
      output_dir = "./outputs/tmp",
      csv = list(
        sep = ",",
        header = TRUE
      ),
      log = TRUE,
      digits = 2,
      interactive = FALSE
    ),
    modules = list(
      descriptive_stats = list(
        vars_default = "numeric"
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
      correlations = list(
        vars_default = "numeric",
        method = "pearson",
        missing = "pairwise",
        alternative = "two.sided",
        controls = NULL,
        p_adjust = "none",
        conf_level = 0.95,
        coerce = FALSE
      ),
      crosstabs = list(
        percent = "all",
        apa_percent = "row",
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
      )
    ),
    templates = list(
      descriptive_stats = list(
        default = "descriptive-stats/default-template.md"
      ),
      frequencies = list(
        default = "frequencies/default-template.md",
        grouped = "frequencies/grouped-template.md"
      ),
      data_explorer = list(
        default = "data-explorer/default-template.md"
      ),
      data_transform = list(
        default = "data-transform/default-template.md"
      ),
      correlations = list(
        default = "correlations/default-template.md",
        cross = "correlations/cross-correlation-template.md"
      ),
      crosstabs = list(
        default = "crosstabs/default-template.md",
        grouped = "crosstabs/grouped-template.md"
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
