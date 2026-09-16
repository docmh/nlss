# SPDX-License-Identifier: Apache-2.0
config_env <- new.env(parent = emptyenv())

# Anchor defaults to this library, never to the caller's working directory or
# its override file. This also works when a module is sourced by a test runner.
config_env$canonical_path <- local({
  frame_files <- vapply(sys.frames(), function(frame) {
    value <- frame$ofile
    if (is.null(value) || length(value) != 1L) "" else as.character(value)
  }, character(1))
  candidates <- frame_files[nzchar(frame_files) & basename(frame_files) == "config.R"]
  if (!length(candidates)) {
    stop("Cannot locate config.R; source the NLSS library from its installed path.", call. = FALSE)
  }
  normalizePath(file.path(dirname(tail(candidates, 1L)), "..", "..", "config.yml"),
                winslash = "/", mustWork = FALSE)
})

get_canonical_config_path <- function() {
  path <- config_env$canonical_path
  if (!file.exists(path)) {
    stop("Missing canonical NLSS configuration: scripts/config.yml. Restore the installation; no fallback defaults are used.",
         call. = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

# Kept for io.R asset lookup; the installation path is independent of caller CWD.
resolve_script_dir <- function() {
  file.path(dirname(get_canonical_config_path()), "R")
}

get_config_path <- function() {
  explicit_path <- Sys.getenv("NLSS_CONFIG_PATH", "")
  if (nzchar(explicit_path)) {
    if (!file.exists(explicit_path)) {
      stop("NLSS_CONFIG_PATH does not point to an existing configuration file.", call. = FALSE)
    }
    return(normalizePath(explicit_path, winslash = "/", mustWork = TRUE))
  }
  get_canonical_config_path()
}

read_config_yaml <- function(path) {
  if (!file.exists(path)) stop("Missing NLSS configuration file: ", path, call. = FALSE)
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Missing dependency: yaml. Install it with install.packages('yaml') to read NLSS configuration.", call. = FALSE)
  }
  config <- tryCatch(yaml::yaml.load_file(path, eval.expr = FALSE), error = function(e) {
    stop("Invalid NLSS YAML configuration: ", conditionMessage(e), call. = FALSE)
  })
  if (is.null(config)) stop("NLSS configuration is empty; expected a YAML mapping.", call. = FALSE)
  config
}

# Compatibility name retained for existing entrypoints and tests. The canonical
# YAML file is now the only source of default values and the override type shape.
get_builtin_config <- function() {
  config <- read_config_yaml(get_canonical_config_path())
  sections <- c("version", "nlss_version", "defaults", "logging", "modules", "templates")
  if (!is.list(config) || is.null(names(config)) ||
      length(setdiff(sections, names(config))) || length(setdiff(names(config), sections))) {
    stop("Invalid canonical NLSS configuration: expected version, nlss_version, defaults, logging, modules and templates.",
         call. = FALSE)
  }
  for (section in c("defaults", "logging", "modules", "templates")) {
    if (!is.list(config[[section]]) || !length(config[[section]])) {
      stop("Invalid canonical NLSS configuration: ", section, " must be a non-empty mapping.", call. = FALSE)
    }
  }
  validate_config(config, schema = config)
}

merge_lists <- function(base, override) {
  if (is.null(override)) return(base)
  if (!is.list(base) || !is.list(override)) return(override)
  out <- base
  for (name in names(override)) {
    if (name %in% names(base)) {
      out[name] <- list(merge_lists(base[[name]], override[[name]]))
    } else {
      out[name] <- list(override[[name]])
    }
  }
  out
}

validate_config <- function(config, schema = get_builtin_config(), path = "") {
  location <- if (nzchar(path)) path else "configuration"
  invalid <- function(expected) {
    stop("Invalid configuration at ", location, ": expected ", expected, ".", call. = FALSE)
  }
  if (is.list(schema)) {
    if (!is.list(config) || (length(config) &&
        (is.null(names(config)) || any(!nzchar(names(config))) || anyDuplicated(names(config))))) {
      invalid("a mapping with unique named keys")
    }
    # Templates may add named variants, but module names and every variant's
    # path remain validated. Template front matter is handled by formatting.R.
    custom_variants <- grepl("^templates[.][^.]+$", path)
    unknown <- setdiff(names(config), names(schema))
    if (length(unknown) && !custom_variants) {
      stop("Unknown configuration key: ",
           if (nzchar(path)) paste0(path, ".") else "", unknown[[1]], ".", call. = FALSE)
    }
    for (key in names(config)) {
      child_path <- if (nzchar(path)) paste(path, key, sep = ".") else key
      child_schema <- if (custom_variants) "" else schema[[key]]
      # Single-bracket assignment deliberately retains an explicit NULL.
      config[key] <- list(validate_config(config[[key]], child_schema, child_path))
    }
    return(config)
  }
  if (is.null(config)) {
    if (is.null(schema)) return(NULL)
    invalid("a non-null scalar value")
  }
  if (length(config) != 1L || is.list(config) || is.na(config)) invalid("a scalar value")
  if (is.null(schema)) {
    if (!is.atomic(config)) invalid("a scalar value or null")
    return(config)
  }
  if (is.logical(schema)) {
    token <- tolower(trimws(as.character(config)))
    if (token %in% c("true", "t", "1", "yes", "y")) return(TRUE)
    if (token %in% c("false", "f", "0", "no", "n")) return(FALSE)
    invalid("TRUE/FALSE (or yes/no, 1/0)")
  }
  if (is.numeric(schema)) {
    if (is.logical(config)) invalid("a finite number")
    number <- suppressWarnings(as.numeric(config))
    if (length(number) != 1L || !is.finite(number)) invalid("a finite number")
    return(number)
  }
  # EFA explicitly accepts either the 'eigen' policy or a numeric factor count.
  if (identical(path, "modules.efa.n_factors") && is.numeric(config) && is.finite(config)) {
    return(config)
  }
  if (is.character(schema) && !is.character(config)) invalid("text")
  config
}

load_config_file <- function(path, schema = get_builtin_config()) {
  # Keep the dependency check here as well for callers that wrap this public
  # helper in an isolated dependency environment.
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Missing dependency: yaml. Install it with install.packages('yaml') to read NLSS configuration.", call. = FALSE)
  }
  validate_config(read_config_yaml(path), schema = schema)
}

load_config <- function() {
  base <- get_builtin_config()
  path <- get_config_path()
  if (identical(path, get_canonical_config_path())) return(base)
  merge_lists(base, load_config_file(path, schema = base))
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
  if (path == "defaults.log") {
    if (is.list(cfg$logging) && "enabled" %in% names(cfg$logging)) {
      val <- cfg$logging$enabled
      if (!is.null(val)) return(val)
    }
  }
  parts <- strsplit(path, ".", fixed = TRUE)[[1]]
  val <- cfg
  for (part in parts) {
    if (!is.list(val) || !part %in% names(val)) return(default)
    val <- val[[part]]
  }
  if (is.null(val)) return(default)
  val
}
