# SPDX-License-Identifier: Apache-2.0
# A small execution boundary for migrated modules, not a general workflow engine.
nlss_run_context <- new.env(parent = emptyenv())

nlss_run_modules <- function() c("descriptive_stats", "regression", "mi_regression",
                               "frequencies", "crosstabs", "data_explorer",
                               "scale", "reliability", "t_test", "correlations",
                               "anova", "nonparametric", "mixed_models", "efa", "sem", "power", "assumptions", "plot", "data_transform", "missings", "impute")

nlss_timezone_identity <- function() {
  explicit <- Sys.getenv("TZ", "")
  if (nzchar(explicit)) return(explicit)
  # A stable zone/file identity, not today's DST offset. Avoid invoking system
  # services from a metadata read in sandboxed R sessions.
  if (.Platform$OS.type != "windows" && file.exists("/etc/localtime")) {
    zone <- Sys.readlink("/etc/localtime")
    if (!is.na(zone) && grepl("/zoneinfo/", zone, fixed = TRUE)) return(sub("^.*/zoneinfo/", "", zone))
    return(paste0("localtime-sha256:", import_hash("/etc/localtime", file = TRUE)))
  }
  zone <- suppressWarnings(Sys.timezone())
  if (is.na(zone) || !nzchar(zone)) stop("Cannot identify the local timezone for replay; set TZ explicitly.")
  zone
}

nlss_code_hash <- function() {
  root <- normalizePath(get_script_dir(), winslash = "/", mustWork = TRUE)
  files <- sort(list.files(root, pattern = "[.]R$", recursive = TRUE, full.names = TRUE))
  relative <- substring(files, nchar(root) + 2L)
  hashes <- vapply(files, import_hash, character(1), file = TRUE)
  import_hash(import_json(as.list(setNames(hashes, relative))))
}

nlss_execution_environment <- function() {
  packages <- sort(loadedNamespaces())
  list(r_version = R.version.string, platform = R.version$platform,
       packages = as.list(setNames(vapply(packages, function(p) as.character(utils::packageVersion(p)), character(1)), packages)),
       locale = Sys.getlocale(), timezone = nlss_timezone_identity(),
       system_libraries = as.list(extSoftVersion()),
       options = options()[c("contrasts", "na.action", "OutDec", "scipen", "digits", "width")])
}

nlss_project_file <- function(path, root, must_exist = TRUE) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path) ||
      grepl("^(/|[A-Za-z]:|\\\\)", path) || any(strsplit(gsub("\\\\", "/", path), "/", fixed = TRUE)[[1]] == "..")) {
    stop("Run artifact must be a project-relative path without parent traversal.")
  }
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  resolved <- normalizePath(file.path(root, path), winslash = "/", mustWork = must_exist)
  if (!startsWith(resolved, paste0(root, "/"))) stop("Run artifact escapes the project directory.")
  if (must_exist && (!file.exists(resolved) || dir.exists(resolved))) stop("Missing run artifact: ", path)
  resolved
}

nlss_verify_dataset <- function(reference, root) {
  if (identical(reference$origin, "managed_parquet")) {
    source_lib("project_store.R")
    for (kind in c("data", "dictionary")) {
      object <- nlss_verify_object(root, reference[[paste0(kind, "_sha256")]])
      key <- if (kind == "data") "snapshot_path" else "dictionary_path"
      if (!identical(reference[[key]], make_relative_path(object, root))) stop("Managed dataset path differs from its object.")
    }
  }
  for (kind in c("data", "dictionary")) {
    path_key <- if (kind == "data") "snapshot_path" else "dictionary_path"
    file <- nlss_project_file(reference[[path_key]], root)
    hash <- reference[[paste0(kind, "_sha256")]]
    if (!is.character(hash) || length(hash) != 1L || !identical(import_hash(file, file = TRUE), hash)) {
      stop("Saved ", kind, " snapshot failed SHA-256 integrity validation.")
    }
  }
  invisible(TRUE)
}

nlss_mask_prose_paths <- function(text, root) {
  if (is.null(text)) return(NULL)
  # Natural-language rationale/prompt fields can contain paths inside a sentence.
  # Do not interpret URL slashes, relative paths or already masked placeholders.
  pattern <- "(?<![[:alnum:]_:/\\\\>])(?:[A-Za-z]:[\\\\/]|/|\\\\\\\\)[^[:space:]\"'<>]+"
  vapply(as.character(text), function(value) {
    if (is.na(value)) return(value)
    positions <- gregexpr(pattern, value, perl = TRUE)
    tokens <- regmatches(value, positions)[[1]]
    if (!length(tokens)) return(value)
    replacements <- vapply(tokens, function(token) {
      suffix <- regmatches(token, regexpr("[,;:!?.)]*$", token, perl = TRUE))
      path <- substr(token, 1L, nchar(token) - nchar(suffix))
      paste0(render_log_path(gsub("\\\\", "/", path), workspace_root = root), suffix)
    }, character(1), USE.NAMES = FALSE)
    regmatches(value, positions) <- list(replacements)
    value
  }, character(1), USE.NAMES = FALSE)
}

nlss_mask_expression_paths <- function(text, root) {
  if (is.null(text)) return(NULL)
  # R division is not a filesystem path, including (x+y)/3 or x /3.
  # Only quoted string literals in authored expressions receive display masking;
  # executable requests keep their original expressions unchanged.
  quoted <- "\"(?:[^\"\\\\]|\\\\.)*\"|'(?:[^'\\\\]|\\\\.)*'"
  vapply(as.character(text), function(value) {
    if (is.na(value)) return(value)
    # Parsing only, never evaluation: STR_CONST includes R raw strings and
    # escaped quotes, whose literal boundaries cannot be inferred by prose regex.
    parsed <- tryCatch(utils::getParseData(parse(text = value, keep.source = TRUE), includeText = TRUE), error = function(e) NULL)
    if (!is.null(parsed)) {
      literals <- unique(parsed$text[parsed$token == "STR_CONST"])
    } else {
      # Non-syntactic output names can make a complete CLI rule unparseable.
      # Ordinary quoted literals remain lexical; raw syntax falls back to
      # conservative display redaction rather than risking an exposed path.
      if (grepl("[rR][\"']-*[([{]", value)) return(nlss_mask_prose_paths(value, root))
      literals <- unique(regmatches(value, gregexpr(quoted, value, perl = TRUE))[[1]])
    }
    for (literal in literals[order(nchar(literals), decreasing = TRUE)]) {
      positions <- gregexpr(literal, value, fixed = TRUE)
      found <- regmatches(value, positions)[[1]]
      if (length(found)) regmatches(value, positions) <- list(rep(nlss_mask_prose_paths(literal, root), length(found)))
    }
    value
  }, character(1), USE.NAMES = FALSE)
}

# Parameter-only planning has an honest input kind, not an empty dataset. Its
# reserved directory is deliberately separate from manifest dataset entries.
nlss_verify_input <- function(request, root, out_dir) {
  if (identical(request$storage, "managed_parquet_v1")) {
    source_lib("project_store.R")
    nlss_verify_managed_run(request, root)
  }
  if (identical(as.integer(request$schema_version), 1L)) {
    if (is.null(request$dataset) || !is.null(request$input)) stop("Dataset request requires a verified dataset reference.")
    return(nlss_verify_dataset(request$dataset, root))
  }
  if (!identical(as.integer(request$schema_version), 2L) ||
      !identical(request$module, "power") || !is.null(request$dataset) ||
      !identical(request$input, list(kind = "parameters"))) stop("Unsupported parameter-only request.")
  locations <- nlss_resolve_locations(start = root, use_dataset = FALSE)
  if (!is.null(locations$project_root)) {
    if (!identical(normalize_path(out_dir), locations$output_root)) stop("Parameter-only output must use the project's .nlss directory.")
    return(invisible(TRUE))
  }
  expected <- file.path(normalizePath(root, winslash = "/", mustWork = TRUE), "planning")
  if (!identical(out_dir, expected) ||
      !identical(normalizePath(out_dir, winslash = "/", mustWork = TRUE), expected)) stop("Planning directory must remain directly inside its project; symlinks are not supported.")
  marker <- file.path(out_dir, ".nlss-planning.json")
  if (!identical(nlss_project_file(".nlss-planning.json", out_dir), marker)) stop("Planning marker must not be a symlink.")
  saved <- read_import_json(marker)
  if (!identical(as.integer(saved$schema_version), 1L) || !identical(saved$kind, "parameters")) stop("Invalid planning directory marker.")
  if (any(file.exists(file.path(out_dir, c("import.json", "dictionary.json", "planning.parquet"))))) stop("Planning directory collides with dataset artifacts.")
  manifest_path <- file.path(root, get_workspace_manifest_name())
  manifest <- read_workspace_manifest(manifest_path)
  for (entry in normalize_manifest_datasets(manifest$datasets)) {
    path <- resolve_dataset_dir(entry, root)
    if (nzchar(path) && identical(normalize_path(path), out_dir)) stop("Planning directory is registered as a dataset; resolve the collision first.")
  }
  invisible(TRUE)
}

nlss_begin_planning_run <- function(module, opts) {
  nlss_dependency_preflight(module, opts)
  if (!identical(module, "power")) stop("Only Power currently supports parameter-only planning.")
  replay <- nlss_run_context$replay
  locations <- if (is.null(replay)) cli_resolve_locations(opts, use_dataset = FALSE)
    else nlss_resolve_locations(project = replay$root, use_dataset = FALSE)
  if (!is.null(locations$project_root)) {
    nlss_run_context$project_selection <- locations$project_root
    return(nlss_begin_run(module, NULL, opts, out_dir = locations$output_root,
                         reference = NULL, input = list(kind = "parameters")))
  }
  root <- if (is.null(replay)) get_default_out() else replay$root
  root <- normalizePath(ensure_out_dir(root), winslash = "/", mustWork = TRUE)
  out_dir <- file.path(root, "planning")
  marker <- file.path(out_dir, ".nlss-planning.json")
  if (!file.exists(out_dir) && !dir.exists(out_dir)) {
    if (!dir.create(out_dir)) stop("Could not create planning directory.")
    write_import_json(list(schema_version = 1L, kind = "parameters"), marker)
  } else if (!file.exists(marker)) {
    stop("The planning directory already exists without an NLSS planning marker. No existing files were adopted; resolve the folder collision first.")
  }
  nlss_verify_input(list(schema_version = 2L, module = module, dataset = NULL,
                        input = list(kind = "parameters")), root, out_dir)
  for (name in c(".nlss-planning.json", "runs", "report_canonical.md", "analysis_log.jsonl", "scratchpad.md")) {
    path <- file.path(out_dir, name)
    link <- Sys.readlink(path)
    if ((!is.na(link) && nzchar(link)) || (file.exists(path) &&
        !identical(normalizePath(path, winslash = "/", mustWork = TRUE), path))) stop("Planning artifacts must not be symlinks: ", name)
  }
  nlss_begin_run(module, NULL, opts, out_dir = out_dir, reference = NULL,
                 input = list(kind = "parameters"))
}

nlss_read_replay <- function(path) {
  nlss_dependency_preflight("replay_run", foundation = TRUE)
  original <- normalize_input_path(path)
  if (!is_absolute_path(original)) original <- file.path(getwd(), original)
  if (basename(original) != "request.json" || basename(dirname(dirname(original))) != "runs" ||
      any(strsplit(original, "/", fixed = TRUE)[[1]] == "..")) stop("Replay requires request.json in its published runs directory without parent traversal.")
  # Resolve the current project before opening saved artifacts. No old-layout
  # search or per-procedure storage gate is needed for a shared run directory.
  source_lib("project_store.R")
  locations <- nlss_resolve_locations(start = dirname(original), use_dataset = FALSE)
  root <- locations$project_root
  if (is.null(root)) stop("Replay requires a current NLSS project marker.")
  run_dir <- file.path(locations$output_root, "runs", basename(dirname(original)))
  relative_run <- make_relative_path(run_dir, root)
  selected <- nlss_project_path(root, original)
  if (!identical(selected$status, "present") || !identical(selected$absolute, file.path(run_dir, "request.json"))) {
    stop("Replay requires the current project's .nlss/runs layout.")
  }
  for (name in c("request.json", "result.json")) {
    nlss_managed_path(root, paste(relative_run, name, sep = "/"), file = TRUE)
  }
  path <- file.path(run_dir, "request.json")
  request <- read_import_json(path)
  if (!as.integer(request$schema_version) %in% c(1L, 2L) ||
      !request$module %in% nlss_run_modules() || !isTRUE(request$resolved)) {
    stop("Unsupported or unresolved saved analysis request.")
  }
  if (basename(path) != "request.json" || startsWith(basename(run_dir), ".") ||
      !identical(basename(run_dir), request$run_id)) {
    stop("Replay requires the published run directory matching the saved run ID; pending runs are not completed analyses.")
  }
  result <- read_import_json(file.path(run_dir, "result.json"))
  if (is.null(result) || !identical(result$status, "completed") ||
      !identical(result$artifacts$request$sha256, import_hash(path, file = TRUE))) {
    stop("Saved request failed integrity validation or does not belong to a completed run.")
  }
  for (key in c("schema_version", "run_id", "module", "input", "dataset", "storage", "project")) {
    if (!identical(result[[key]], request[[key]])) stop("Saved result does not match its request: ", key)
  }
  if (!identical(result$artifacts$request$path, "request.json") ||
      !identical(result$artifacts$output$path, "output.md")) stop("Completed run requires recorded request and output artifacts.")
  # Authenticated recorded requirements; do not replace the exact replay gate
  # with a current-version installation or current scientific defaults.
  requirements <- lapply(names(request$environment$packages), function(package)
    list(package = package, reason = "recorded replay environment",
         exact = request$environment$packages[[package]], exports = character()))
  nlss_dependency_guard("replay_run", requirements = requirements)
  nlss_verify_input(request, root, locations$output_root)
  for (name in c("request.json", "result.json", vapply(result$artifacts, `[[`, "", "path"),
                 vapply(request$templates, `[[`, "", "path"))) {
    nlss_managed_path(root, make_relative_path(file.path(run_dir, name), root), file = TRUE)
  }
  for (artifact in result$artifacts) {
    file <- nlss_project_file(artifact$path, run_dir)
    if (!identical(import_hash(file, file = TRUE), artifact$sha256)) stop("Saved output artifact failed integrity validation.")
  }
  if (request$module %in% c("data_transform", "missings", "impute")) {
    if (!isTRUE(request$design$replay$eligible)) stop("This transformation is not eligible for automatic replay: ", request$design$replay$reason)
    change <- read_import_json(nlss_project_file("data-change.json", run_dir))
    if (is.null(result$artifacts[["data-change.json"]]) ||
        !identical(change, result$results$data_change) ||
        !identical(change$input, request$dataset)) stop("Transformation lineage does not match its saved run.")
    nlss_verify_dataset(change$output, root)
    if (identical(request$module, "impute")) {
      source_lib("imputation_artifact.R")
      reference <- result$results$imputation_artifact
      recorded <- result$artifacts[["imputation-artifact.json"]]
      if (!is.null(reference)) {
        if (is.null(recorded) || !identical(read_import_json(nlss_project_file("imputation-artifact.json", run_dir)), reference)) {
          stop("Imputation artifact reference does not match its authenticated run record.")
        }
        nlss_verify_imputation_artifact(reference, root, expected_dataset = request$dataset)
      } else if (!is.null(recorded)) stop("Unexpected imputation artifact in a run without a mids object.")
    }
  }
  if (!identical(request$code_sha256, nlss_code_hash())) stop("NLSS R code differs from the saved request; exact replay refused.")
  current <- nlss_execution_environment()
  for (key in c("r_version", "platform", "locale", "timezone")) {
    if (!identical(current[[key]], request$environment[[key]])) stop("Execution environment differs: ", key)
  }
  for (package in names(request$environment$packages)) {
    if (!requireNamespace(package, quietly = TRUE) ||
        !identical(as.character(utils::packageVersion(package)), request$environment$packages[[package]])) {
      stop("Execution environment differs: package ", package)
    }
  }
  if (!identical(import_json(as.list(extSoftVersion())), import_json(request$environment$system_libraries))) {
    stop("Execution environment differs: R system libraries.")
  }
  for (template in request$templates) {
    file <- nlss_project_file(template$path, run_dir)
    if (!identical(import_hash(file, file = TRUE), template$sha256)) stop("Saved template failed integrity validation.")
  }
  list(request = request, result = result, path = path, run_dir = run_dir,
       output_root = locations$output_root, root = root)
}

nlss_run_options <- function(args, module) {
  replay <- nlss_run_context$replay
  if (is.null(replay)) return(parse_args(args, module = module))
  opts <- replay$request$cli
  opts$project <- replay$root
  if (!is.null(replay$request$user_prompt)) opts$`user-prompt` <- replay$request$user_prompt
  # JSON is data: rebuild through the ordinary strict option parser.
  tokens <- unlist(lapply(names(opts), function(name) paste0("--", name, "=", as.character(opts[[name]]))), use.names = FALSE)
  parse_args(tokens, module = module)
}

nlss_load_input <- function(opts) {
  if (!is.null(nlss_run_context$dependency_operation))
    nlss_dependency_preflight(nlss_run_context$dependency_operation, opts)
  replay <- nlss_run_context$replay
  if (is.null(replay)) {
    return(load_dataframe(opts))
  }
  if (!is.null(replay$request$input)) stop("Parameter-only replay must not load a dataset.")
  if (identical(replay$request$storage, "managed_parquet_v1")) {
    source_lib("project_store.R")
    nlss_run_context$managed <- nlss_verify_managed_run(replay$request, replay$root)
  }
  ref <- replay$request$dataset
  nlss_verify_dataset(ref, replay$root)
  df <- read_parquet_data(nlss_project_file(ref$snapshot_path, replay$root))
  attr(df, "nlss_dataset_ref") <- ref
  attr(df, "workspace_dir") <- replay$output_root
  # Replay reads the frozen input; it has no live working-data activation target.
  attr(df, "workspace_parquet_path") <- nlss_project_file(ref$snapshot_path, replay$root)
  assign(normalize_path(replay$output_root), ref, envir = nlss_dataset_context)
  df
}

nlss_begin_run <- function(module, df, opts, out_dir = get_workspace_out_dir(df), reference = attr(df, "nlss_dataset_ref"), input = NULL, config_modules = module) {
  nlss_dependency_preflight(module, opts)
  if (!module %in% nlss_run_modules()) stop("Unsupported migrated analysis module.")
  locations <- nlss_current_locations()
  if (is.null(locations$project_root)) locations <- nlss_resolve_locations(start = out_dir, use_dataset = FALSE)
  output_root <- if (is.null(locations$project_root)) out_dir else locations$output_root
  output_root <- normalizePath(ensure_out_dir(output_root), winslash = "/", mustWork = TRUE)
  # Keep the existing input-metadata directory available to data-change helpers.
  # It is no longer the implicit run destination or project root.
  input_dir <- attr(df, "workspace_dir")
  out_dir <- if (is.null(input_dir)) output_root else normalize_path(input_dir)
  root <- if (is.null(locations$project_root)) dirname(output_root) else locations$project_root
  nlss_run_context$locations <- locations
  nlss_run_context$output_root <- output_root
  nlss_run_context$input_dir <- input_dir
  schema <- if (is.null(input)) 1L else 2L
  nlss_verify_input(list(schema_version = schema, module = module, dataset = reference, input = input), root, output_root)
  lock <- file.path(out_dir, ".analysis-lock")
  if (is.null(nlss_run_context$lock) && !dir.create(lock, showWarnings = FALSE)) stop("Dataset analysis is locked. Check for a running/interrupted analysis before removing .analysis-lock.")
  nlss_run_context$lock <- lock
  if (module %in% c("data_transform", "missings", "impute") && is.null(nlss_run_context$replay) && is.null(nlss_run_context$managed)) {
    import_lock <- file.path(out_dir, ".import-lock")
    if (!dir.create(import_lock, showWarnings = FALSE)) stop("Dataset import is locked; no transformation was applied.")
    nlss_run_context$import_lock <- import_lock
    working <- attr(df, "workspace_parquet_path")
    if (is.null(working) || !identical(import_hash(working, file = TRUE), reference$data_sha256)) stop("Workspace data changed before transformation; retry with the current dataset.")
    binding_file <- file.path(out_dir, "import.json")
    binding <- read_import_json(binding_file)
    if (!identical(binding$source$source_sha256, reference$source_sha256) ||
        !identical(binding$import_version_id, reference$import_version_id)) stop("Import binding changed before transformation; retry with the current dataset.")
    nlss_run_context$working_path <- working
    nlss_run_context$binding <- binding
    nlss_run_context$binding_hash <- if (file.exists(binding_file)) import_hash(binding_file, file = TRUE) else NULL
  }
  runs <- ensure_out_dir(file.path(output_root, "runs"))
  if (!identical(normalizePath(runs, winslash = "/", mustWork = TRUE), runs)) stop("Run directory must not be a symlink.")
  staging <- tempfile(paste0(".pending-", format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC"), "-"), tmpdir = runs)
  if (!dir.create(staging)) stop("Could not stage analysis run.")
  id <- sub("^[.]pending-", "", basename(staging))
  nlss_run_context$out_dir <- out_dir
  if (!is.null(reference)) assign(normalize_path(out_dir), reference, envir = nlss_dataset_context)
  if (!is.null(reference)) assign(normalize_path(output_root), reference, envir = nlss_dataset_context)
  nlss_run_context$root <- root
  nlss_run_context$staging <- staging
  nlss_run_context$destination <- file.path(runs, id)
  nlss_run_context$reports <- list()
  nlss_run_context$figure_reports <- list()
  nlss_run_context$file_projections <- list()
  nlss_run_context$logs <- list()
  nlss_run_context$results <- NULL
  nlss_run_context$artifacts <- list()
  input_flags <- c("csv", "sav", "rds", "rdata", "parquet", "df", "sep", "header", "dataset-name", "import-action",
                   "csv-decimal", "csv-encoding", "csv-col-types", "csv-na-values", "interactive", "help", "template", "user-prompt",
                   "project", "dataset")
  if (module %in% c("power", "assumptions", "plot", "missings", "impute")) for (key in intersect(c("effect-basis", "user-prompt", "title", "subtitle", "caption", "note"), names(opts))) {
    opts[[key]] <- nlss_mask_prose_paths(opts[[key]], root)
  }
  cli <- opts[setdiff(names(opts), input_flags)]
  config <- get_config()
  # Diagnostics may inherit the selected model family's defaults; preserve only
  # those explicitly used blocks, not unrelated module settings or credentials.
  config$modules <- config$modules[intersect(unique(c(module, config_modules, "init_workspace")), names(config$modules))]
  config$templates <- config$templates[intersect(c(module, "init_workspace"), names(config$templates))]
  # Output location is derived from the request's project, never an external path.
  config$defaults$output_dir <- "."
  nlss_run_context$request <- list(schema_version = schema, run_id = id, module = module,
    resolved = FALSE, dataset = reference, cli = render_paths_for_log(cli, workspace_root = root),
    options = list(), configuration = render_paths_for_log(config, workspace_root = root),
    user_prompt = if (resolve_logging_bool("include_user_prompt", FALSE)) render_paths_for_log(get_user_prompt(opts), workspace_root = root) else NULL,
    code_sha256 = nlss_code_hash(), environment = nlss_execution_environment(),
    rng = list(kind = RNGkind(), seed = NULL, state = NULL), templates = list(),
    replay_of = if (is.null(nlss_run_context$replay)) NULL else nlss_run_context$replay$request$run_id)
  # Transformation rules are executable specifications, not filesystem paths:
  # a rename like f:factor_name must not be mistaken for a Windows drive path.
  # Source selectors were removed above; exact user-authored rules stay in the
  # private request, while human/legacy context still receives path masking.
  # Imputation constants/maps also contain authored values, not file selectors.
  if (module %in% c("data_transform", "impute")) nlss_run_context$request$cli <- cli
  if (!is.null(input)) nlss_run_context$request$input <- input
  if (!is.null(nlss_run_context$managed)) {
    project <- nlss_run_context$managed
    nlss_run_context$request$storage <- "managed_parquet_v1"
    nlss_run_context$request$project <- list(workspace_id = project$manifest$workspace_id, dataset_name = project$name)
  }
  write_import_json(nlss_run_context$request, file.path(staging, "request.json"))
  invisible(id)
}

nlss_resolve_request <- function(options, design = NULL) {
  nlss_run_context$request$options <- options
  nlss_run_context$request$design <- design
  nlss_run_context$request$resolved <- TRUE
  write_import_json(nlss_run_context$request, file.path(nlss_run_context$staging, "request.json"))
  invisible(options)
}

nlss_run_seed <- function(value = NULL, stochastic = FALSE) {
  seed <- if (is.null(value) || !nzchar(as.character(value))) {
    if (stochastic) get_config_value(paste0("modules.", nlss_run_context$request$module, ".seed"), 1L) else NULL
  } else suppressWarnings(as.numeric(value))
  if (!is.null(seed)) {
    if (length(seed) != 1L || !is.finite(seed) || seed < 0 || seed > .Machine$integer.max || seed != floor(seed)) stop("Seed must be a non-negative R integer.")
    set.seed(as.integer(seed))
  }
  nlss_run_context$request$rng <- list(kind = RNGkind(), seed = seed,
    state = if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv) else NULL)
  seed
}

nlss_freeze_template <- function(path, slot) {
  replay <- nlss_run_context$replay
  if (!is.null(replay)) {
    saved <- replay$request$templates[[slot]]
    path <- if (is.null(saved)) NULL else nlss_project_file(saved$path, replay$run_dir)
  }
  if (is.null(path) || !file.exists(path)) return(NULL)
  relative <- paste0("templates/", slot, ".md")
  destination <- file.path(nlss_run_context$staging, relative)
  ensure_out_dir(dirname(destination))
  hash <- import_hash(path, file = TRUE)
  if (!file.copy(path, destination) || !identical(hash, import_hash(destination, file = TRUE))) stop("Could not preserve analysis template.")
  nlss_run_context$request$templates[[slot]] <- list(path = relative, sha256 = hash)
  destination
}

nlss_stage_report <- function(path, analysis_label, nlss_table, nlss_text, analysis_flags = NULL, template_path = NULL, template_context = NULL) {
  call <- as.list(environment())
  call <- call[c("path", "analysis_label", "nlss_table", "nlss_text", "analysis_flags", "template_path", "template_context")]
  output <- file.path(nlss_run_context$staging, "output.md")
  start <- get_next_table_number(output)
  if (is.null(template_path) || !file.exists(template_path)) nlss_table <- renumber_tables(nlss_table, start)$text
  # Multi-table output has run-local numbering; keep the queued legacy context
  # unchanged so its append-only report can continue its own numbering.
  if (!is.null(template_context$tokens$table_number_next)) {
    template_context$tokens$table_number_next <- as.character(start + 1L)
  }
  report <- format_nlss_report(analysis_label, nlss_table, nlss_text,
    analysis_flags = analysis_flags, template_path = template_path, table_start = start, template_context = template_context)
  connection <- file(output, open = "a", encoding = "UTF-8")
  on.exit(close(connection), add = TRUE)
  if (file.info(output)$size > 0) cat("\n\n---\n\n", file = connection)
  cat(report, file = connection)
  nlss_run_context$reports <- c(nlss_run_context$reports, list(call))
  invisible(TRUE)
}

nlss_stage_log <- function(out_dir, module, prompt, commands, results, options = list(), user_prompt = NULL) {
  if (!is.null(nlss_run_context$locations$project_root)) return(invisible(TRUE))
  if (identical(module, "data_transform")) {
    original <- commands
    for (i in seq_along(commands)) {
      calc <- startsWith(original[[i]], "--calc=") || (i > 1L && identical(original[[i - 1L]], "--calc"))
      commands[[i]] <- if (calc) nlss_mask_expression_paths(original[[i]], nlss_run_context$root)
        else nlss_mask_prose_paths(original[[i]], nlss_run_context$root)
    }
    prompt <- paste(commands, collapse = " ")
    user_prompt <- nlss_mask_prose_paths(user_prompt, nlss_run_context$root)
  } else if (module %in% c("power", "assumptions", "plot", "missings", "impute")) {
    prompt <- nlss_mask_prose_paths(prompt, nlss_run_context$root)
    commands <- nlss_mask_prose_paths(commands, nlss_run_context$root)
    user_prompt <- nlss_mask_prose_paths(user_prompt, nlss_run_context$root)
  }
  if (!resolve_logging_bool("include_user_prompt", FALSE)) {
    keep <- rep(TRUE, length(commands))
    for (i in seq_along(commands)) {
      if (identical(commands[[i]], "--user-prompt")) keep[i:min(i + 1L, length(commands))] <- FALSE
      if (startsWith(commands[[i]], "--user-prompt=")) keep[i] <- FALSE
    }
    commands <- commands[keep]
    prompt <- paste(commands, collapse = " ")
    user_prompt <- NULL
  }
  nlss_run_context$logs <- c(nlss_run_context$logs, list(list(out_dir = out_dir, module = module,
    prompt = prompt, commands = commands, results = results, options = options, user_prompt = user_prompt)))
  invisible(TRUE)
}

# One visible projection of already rendered evidence, shared by statistical and
# utility publishers. No numerical calculation, input loading or parallel log.
nlss_append_project_protocol <- function(root, run_path, request, result, output,
                                         publish = NULL, path = file.path(root, "report_canonical.md")) {
  owner <- "<!-- NLSS generated analysis protocol -->"
  if (file.exists(path)) {
    if (!nlss_project_regular_file(path) || !owner %in% readLines(path, warn = FALSE)) {
      stop("Protocol destination contains an unrecognized/user-authored file; no overwrite was performed.")
    }
  } else {
    ensure_output_front_matter(path, workspace_root = root)
    cat(owner, "\n\n# Analysis protocol\n", file = path, append = TRUE, sep = "")
  }
  tag <- paste0("<!-- nlss-run: ", run_path, " -->")
  if (tag %in% readLines(path, warn = FALSE)) stop("This run already appears in the project protocol.")
  dataset <- request$dataset
  header <- c("", "---", "", tag, "", paste0("## ", request$module, " — ", result$status), "",
    paste0("Run: `", request$run_id, "`"),
    if (!is.null(dataset)) paste0("Dataset: `", dataset$dataset_id, "`; version: `", dataset$version_id, "`"),
    if (!is.null(request$input)) paste0("Input: ", request$input$kind),
    paste0("[Request](", run_path, "/request.json) · [Result](", run_path, "/result.json)",
      if (!is.null(output)) paste0(" · [Run output](", run_path, "/", basename(output), ")")), "")
  cat(paste(header, collapse = "\n"), "\n", file = path, append = TRUE)
  if (!is.null(publish)) publish()
  if ((is.null(publish) || identical(result$status, "failed")) && !is.null(output) && file.exists(output)) {
    body <- paste(readLines(output, warn = FALSE), collapse = "\n")
    body <- renumber_tables(body, get_next_table_number(path))$text
    body <- renumber_figures(body, get_next_figure_number(path))$text
    for (artifact in result$artifacts) if (!is.null(artifact$path)) {
      body <- gsub(paste0("](", artifact$path, ")"), paste0("](", run_path, "/", artifact$path, ")"), body, fixed = TRUE)
    }
    cat(body, "\n", file = path, append = TRUE)
  }
  warnings <- result$warnings
  if (is.null(warnings) && identical(result$kind, "utility")) warnings <- result$results$warnings
  if (length(warnings)) for (warning in warnings) {
    message <- if (is.list(warning)) warning$message else as.character(warning)
    cat("\nWarning: ", nlss_mask_prose_paths(message, root), "\n", file = path, append = TRUE, sep = "")
  }
  if (!is.null(result$error$message)) cat("\nError: ", nlss_mask_prose_paths(result$error$message, root), "\n", file = path, append = TRUE, sep = "")
  invisible(path)
}

# Explicit reconstruction uses the same projection, never a statistical rerun.
# No index is maintained: read published run metadata and their stored Markdown.
nlss_rebuild_project_protocol <- function(project, destination) {
  locations <- nlss_resolve_locations(project = project, use_dataset = FALSE)
  root <- locations$project_root
  source_lib("project_store.R")
  destination <- nlss_managed_path(root, destination, visible = TRUE)
  if (nlss_project_exists(destination)) stop("Reconstruction destination already exists; no file was overwritten.")
  records <- list()
  for (kind in c("runs", "utility-runs")) {
    for (directory in list.dirs(file.path(locations$output_root, kind), recursive = FALSE, full.names = TRUE)) {
      if (startsWith(basename(directory), ".")) next
      relative <- make_relative_path(directory, root)
      request <- nlss_managed_json(root, paste0(relative, "/request.json"))
      result <- nlss_managed_json(root, paste0(relative, "/result.json"))
      if (!identical(request$run_id, basename(directory)) || !identical(request$run_id, result$run_id) ||
          !result$status %in% c("completed", "partial", "failed") || is.null(result$published_at_utc)) stop("Incomplete published protocol record.")
      output <- intersect(c("output.md", "partial-output.md", "diagnostic-output.md"), list.files(directory))
      records[[length(records) + 1L]] <- list(path = relative, request = request, result = result,
        output = if (length(output)) nlss_managed_path(root, paste0(relative, "/", output[1]), file = TRUE) else NULL)
    }
  }
  ordering <- order(vapply(records, function(x) x$result$published_at_utc, ""))
  temporary <- tempfile("protocol-", tmpdir = dirname(destination))
  on.exit(unlink(temporary), add = TRUE)
  for (i in ordering) {
    record <- records[[i]]
    nlss_append_project_protocol(root, record$path, record$request, record$result, record$output, path = temporary)
  }
  if (!length(records)) stop("No published outputs to reconstruct.")
  contents <- readLines(temporary, warn = FALSE)
  contents <- contents[-seq_len(find_frontmatter_end(contents))]
  writeLines(c(build_output_front_matter(destination, workspace_root = root), contents), temporary, useBytes = TRUE)
  if (nlss_project_exists(destination) || !file.rename(temporary, destination)) stop("Could not publish reconstructed protocol.")
  invisible(destination)
}

nlss_set_result <- function(results) {
  nlss_run_context$results <- results
  invisible(results)
}

nlss_save_run_rds <- function(object, name) {
  if (!grepl("^[A-Za-z][A-Za-z0-9_-]*[.]rds$", name)) stop("Invalid analysis artifact filename.")
  path <- file.path(nlss_run_context$staging, name)
  saveRDS(object, path, version = 3L)
  if (!isTRUE(all.equal(object, readRDS(path)))) stop("Analysis artifact failed round-trip verification.")
  nlss_run_context$artifacts[[name]] <- list(path = name, sha256 = import_hash(path, file = TRUE))
  invisible(path)
}

# Figure reports have separate run-local and append-only numbering/link roots.
# The caller supplies both contexts; no regex rewrites of user captions/templates.
nlss_stage_figure_report <- function(path, analysis_label, figure_body, analysis_flags = NULL,
                                    template_path = NULL, template_context = NULL,
                                    figure_start = 1L, legacy = list()) {
  call <- as.list(environment())[c("path", "analysis_label", "figure_body", "analysis_flags",
                                   "template_path", "template_context", "figure_start")]
  if (length(setdiff(names(legacy), names(call)))) stop("Unknown legacy figure report field.")
  report <- format_nlss_figure_report(analysis_label, figure_body, analysis_flags,
    template_path, figure_number = figure_start, template_context = template_context)
  output <- file.path(nlss_run_context$staging, "output.md")
  connection <- file(output, open = "a", encoding = "UTF-8")
  on.exit(close(connection), add = TRUE)
  if (file.info(output)$size > 0) cat("\n\n---\n\n", file = connection)
  cat(report, file = connection)
  call[names(legacy)] <- legacy
  nlss_run_context$figure_reports <- c(nlss_run_context$figure_reports, list(call))
  invisible(TRUE)
}

nlss_plot_projection_path <- function(relative, out_dir) {
  if (!is.character(relative) || length(relative) != 1L || is.na(relative) ||
      !grepl("^plots/[A-Za-z0-9_-][A-Za-z0-9_.-]*[.](png|pdf|svg|jpe?g|tiff?|bmp|eps|ps)$", relative)) {
    stop("Figure projection must be a safe filename directly inside plots/.")
  }
  for (path in c(file.path(out_dir, "plots"), file.path(out_dir, relative))) {
    link <- Sys.readlink(path)
    if ((!is.na(link) && nzchar(link)) || (file.exists(path) &&
        !identical(normalizePath(path, winslash = "/", mustWork = TRUE), path))) {
      stop("Figure projection must not be a symlink.")
    }
  }
  directory <- file.path(out_dir, "plots")
  if (file.exists(directory) && !dir.exists(directory)) stop("plots/ is not a directory.")
  target <- file.path(out_dir, relative)
  if (dir.exists(target)) stop("Figure projection target is a directory.")
  target
}

nlss_save_run_file <- function(name, source = file.path(nlss_run_context$staging, name),
                               legacy_path = NULL, overwrite = FALSE) {
  if (!is.character(name) || length(name) != 1L || is.na(name) ||
      !grepl("^plots/[A-Za-z0-9_-][A-Za-z0-9_.-]*[.](png|pdf|svg|jpe?g|tiff?|bmp|eps|ps)$", name)) stop("Invalid figure artifact filename.")
  context <- nlss_run_context
  destination <- nlss_plot_projection_path(name, context$staging)
  ensure_out_dir(dirname(destination))
  source_hash <- import_hash(source, file = TRUE)
  # Existing source may be the renderer's staged file; otherwise copy it in.
  if (!identical(normalizePath(source, winslash = "/", mustWork = TRUE),
                 normalizePath(destination, winslash = "/", mustWork = FALSE))) {
    if (file.exists(destination) || !file.copy(source, destination)) stop("Could not preserve figure artifact.")
  }
  if (!identical(nlss_project_file(name, context$staging), destination) ||
      file.info(destination)$size <= 0) stop("Figure artifact is empty or outside its run.")
  if (!identical(source_hash, import_hash(destination, file = TRUE))) stop("Figure artifact copy failed integrity validation.")
  if (name %in% names(context$artifacts)) stop("Duplicate figure artifact.")
  context$artifacts[[name]] <- list(path = name, sha256 = import_hash(destination, file = TRUE))
  if (!is.null(legacy_path) && is.null(context$locations$project_root)) {
    target <- nlss_plot_projection_path(legacy_path, context$out_dir)
    if (file.exists(target) && !isTRUE(overwrite)) stop("Figure projection already exists; choose another filename or --overwrite TRUE.")
    if (target %in% vapply(context$file_projections, `[[`, character(1), "target")) stop("Duplicate figure projection target.")
    context$file_projections <- c(context$file_projections, list(list(path = name,
      relative = legacy_path, target = target, overwrite = isTRUE(overwrite))))
  }
  invisible(destination)
}

nlss_publish_run <- function(error = NULL) {
  context <- nlss_run_context
  if (is.null(context$staging)) return(invisible(NULL))
  success <- is.null(error)
  if (success && (is.null(context$results) || !isTRUE(context$request$resolved))) stop("Analysis did not supply a resolved request and result.")
  if (!success && !is.null(context$data_change)) {
    record <- file.path(context$staging, "data-change.json")
    artifact <- context$artifacts[["data-change.json"]]
    # A failed publication cannot claim committed application in standalone
    # lineage. Null also avoids claiming full rollback if recovery itself failed.
    # Preserve tampered bytes as diagnostic evidence instead of repairing them.
    if (!is.null(artifact) && file.exists(record) &&
        identical(import_hash(record, file = TRUE), artifact$sha256)) {
      context$data_change$applied <- if (is.null(context$managed) ||
        identical(context$recovery_status, "manual_recovery_required")) NULL else FALSE
      context$data_change$publication_status <- if (isTRUE(context$publication_conflict)) "conflict_not_applied" else "failed_not_committed"
      if (!is.null(context$recovery_status)) context$data_change$recovery_status <- context$recovery_status
      if (!is.null(context$managed) && !is.null(context$results)) context$results$data_change <- context$data_change
      write_import_json(context$data_change, record)
      context$artifacts[["data-change.json"]]$sha256 <- import_hash(record, file = TRUE)
    }
  }
  nlss_verify_input(context$request, context$root, if (is.null(context$output_root)) context$out_dir else context$output_root)
  context$request$environment <- nlss_execution_environment()
  context$request$rng$final_state <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv) else NULL
  if (success && !is.null(context$data_change)) nlss_verify_dataset(context$data_change$output, context$root)
  if (success && identical(context$request$module, "impute") && !is.null(context$results$imputation_artifact)) {
    reference <- context$results$imputation_artifact
    if (is.null(context$artifacts[["imputation-artifact.json"]]) ||
        !identical(read_import_json(file.path(context$staging, "imputation-artifact.json")),
                   jsonlite::fromJSON(import_json(reference), simplifyVector = FALSE))) {
      stop("Imputation artifact reference does not match its staged run record.")
    }
    nlss_verify_imputation_artifact(reference, context$root, expected_dataset = context$request$dataset)
  } else if (success && identical(context$request$module, "impute") &&
             (!is.null(context$artifacts[["imputation-artifact.json"]]) || !is.null(context$imputation_artifact))) {
    stop("Imputation result omitted its preserved mids artifact reference.")
  }
  request_path <- file.path(context$staging, "request.json")
  write_import_json(context$request, request_path)
  artifacts <- c(list(request = list(path = "request.json", sha256 = import_hash(request_path, file = TRUE))), context$artifacts)
  output <- file.path(context$staging, "output.md")
  if (success) {
    if (!file.exists(output)) stop("Analysis produced no deterministic Markdown output.")
    for (artifact in context$artifacts) {
      file <- nlss_project_file(artifact$path, context$staging)
      if (!identical(import_hash(file, file = TRUE), artifact$sha256)) stop("Staged output artifact failed integrity validation.")
    }
    artifacts$output <- list(path = "output.md", sha256 = import_hash(output, file = TRUE))
  } else if (file.exists(output)) {
    if (!file.rename(output, file.path(context$staging, "partial-output.md"))) stop("Could not mark partial analysis output.")
  }
  if (!success && file.exists(file.path(context$staging, "partial-output.md"))) {
    artifacts$partial_output <- list(path = "partial-output.md", sha256 = import_hash(file.path(context$staging, "partial-output.md"), file = TRUE))
  }
  # A failed run may preserve damaged diagnostic artifacts. Describe the bytes
  # actually retained, keeping the registered expectation visibly separate.
  if (!success) for (name in names(context$artifacts)) {
    artifact <- context$artifacts[[name]]
    actual <- tryCatch(import_hash(nlss_project_file(artifact$path, context$staging), file = TRUE), error = function(e) NULL)
    if (!identical(actual, artifact$sha256)) artifacts[[name]] <- list(path = artifact$path,
      sha256 = actual, expected_sha256 = artifact$sha256,
      status = if (is.null(actual)) "unavailable" else "changed_after_registration")
  }
  result <- list(schema_version = context$request$schema_version, run_id = context$request$run_id, module = context$request$module,
    status = if (success) "completed" else "failed", dataset = context$request$dataset,
    results = if (success) context$results else NULL, warnings = context$warnings,
    error = if (success) NULL else list(message = conditionMessage(error), class = class(error)), artifacts = artifacts)
  if (!is.null(context$request$input)) result$input <- context$request$input
  if (!is.null(context$managed)) {
    result$storage <- context$request$storage
    result$project <- context$request$project
    if (!success && !is.null(context$results)) {
      result$results <- context$results
      result$calculation_status <- "completed"
      result$publication_status <- context$data_change$publication_status
    }
  }
  write_import_json(result, file.path(context$staging, "result.json"))
  if (!is.null(context$locations$project_root)) {
    source_lib("project_store.R")
    return(nlss_publish_managed_run(result, success))
  }
  # Keep legacy projections compatible. Back up the exact affected files so a
  # normal publication error cannot leave a successful partial report/log behind.
  publication_lock <- file.path(context$root, ".publication-lock")
  if (!dir.create(publication_lock, showWarnings = FALSE)) stop("Another analysis is publishing in this project; retry after it finishes. Inspect a stale .publication-lock before recovery.")
  on.exit(unlink(publication_lock, recursive = TRUE), add = TRUE)
  targets <- c(file.path(context$out_dir, "report_canonical.md"), file.path(context$out_dir, "analysis_log.jsonl"),
               file.path(context$root, get_config_value("defaults.workspace_manifest", "nlss-workspace.yml")))
  if (success && !is.null(context$data_change) && isTRUE(context$data_change$applied)) {
    targets <- c(targets, nlss_data_change_targets())
  }
  if (success) for (projection in context$file_projections) {
    target <- nlss_plot_projection_path(projection$relative, context$out_dir)
    if (file.exists(target) && !projection$overwrite) stop("Figure projection appeared before publication; no file overwritten.")
    ensure_out_dir(dirname(target))
    targets <- c(targets, target)
  }
  backup_dir <- tempfile("nlss-projection-")
  dir.create(backup_dir)
  on.exit(unlink(backup_dir, recursive = TRUE), add = TRUE)
  existed <- file.exists(targets)
  backups <- file.path(backup_dir, as.character(seq_along(targets)))
  for (i in which(existed)) if (!file.copy(targets[i], backups[i])) stop("Could not protect legacy output before publication.")
  published <- FALSE
  on.exit({
    if (!published) for (i in seq_along(targets)) {
      if (existed[i]) {
        if (!file.copy(backups[i], targets[i], overwrite = TRUE)) warning("Could not restore legacy output after publication failure: ", basename(targets[i]))
      } else if (file.exists(targets[i]) && !dir.exists(targets[i])) unlink(targets[i])
    }
  }, add = TRUE, after = FALSE)
  nlss_log_cache$report_blocks <- character(0)
  if (success && !is.null(context$data_change) && isTRUE(context$data_change$applied)) nlss_publish_data_change()
  if (success) for (projection in context$file_projections) {
    source <- nlss_project_file(projection$path, context$staging)
    if (!file.copy(source, projection$target, overwrite = projection$overwrite) ||
        !identical(import_hash(projection$target, file = TRUE), context$artifacts[[projection$path]]$sha256)) {
      stop("Could not publish verified figure projection.")
    }
  }
  if (success) for (report in context$reports) do.call(append_nlss_report, report)
  if (success) for (report in context$figure_reports) do.call(append_nlss_figure_report, report)
  for (entry in context$logs) {
    if (success || !is.null(entry$results$status)) {
      entry$options$run_id <- context$request$run_id
      if (!is.null(context$request$input)) entry$options$input <- context$request$input
      logged <- do.call(append_analysis_log, entry)
      if (resolve_logging_bool("enabled", TRUE) && !isTRUE(logged)) stop("Required legacy analysis log was not written.")
    }
  }
  if (!file.rename(context$staging, context$destination)) stop("Could not atomically publish analysis bundle.")
  published <- TRUE
  context$staging <- NULL
  cat("Analysis run: ", make_relative_path(context$destination, context$root), " (", result$status, ")\n", sep = "")
  invisible(result)
}

nlss_run_main <- function(module, main) {
  rm(list = ls(nlss_run_context, all.names = TRUE), envir = nlss_run_context)
  nlss_run_context$warnings <- list()
  nlss_run_context$dependency_operation <- module
  on.exit({
    if (!is.null(nlss_run_context$lock)) unlink(nlss_run_context$lock, recursive = TRUE)
    if (!is.null(nlss_run_context$import_lock)) unlink(nlss_run_context$import_lock, recursive = TRUE)
  }, add = TRUE)
  replay_path <- Sys.getenv("NLSS_REPLAY_REQUEST", "")
  if (nzchar(replay_path)) {
    replay <- nlss_read_replay(replay_path)
    if (!identical(replay$request$module, module)) stop("Replay module mismatch.")
    nlss_run_context$replay <- replay
    config_env$config <- replay$request$configuration
    config_env$config$defaults$output_dir <- replay$root
    restored_options <- lapply(replay$request$environment$options, unlist, use.names = TRUE)
    options(restored_options)
    do.call(RNGkind, as.list(unlist(replay$request$rng$kind)))
  }
  failure <- NULL
  tryCatch(withCallingHandlers({
    main()
    nlss_publish_run()
  }, warning = function(w) {
    nlss_run_context$warnings <- c(nlss_run_context$warnings, list(list(message = conditionMessage(w), class = class(w))))
  }), error = function(e) failure <<- e)
  if (!is.null(failure)) {
    if (!is.null(nlss_run_context$staging)) {
      tryCatch(nlss_publish_run(failure), error = function(e) message("Could not finalize failed run: ", conditionMessage(e), "; incomplete directory remains marked .pending-."))
    }
    stop(failure)
  }
  invisible(TRUE)
}
