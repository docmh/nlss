# SPDX-License-Identifier: Apache-2.0
# Shared project storage. Objects hold bytes; datasets and runs hold meaning.
if (!exists("nlss_project_path", mode = "function")) source_lib("project_inspect.R")

nlss_managed_path <- function(root, relative, file = FALSE, visible = FALSE) {
  target <- nlss_project_path(root, relative)
  if (!target$status %in% c("present", "missing") || identical(target$path, ".")) {
    stop("Unsafe managed project path: ", target$status, call. = FALSE)
  }
  parts <- strsplit(target$path, "/", fixed = TRUE)[[1]]
  if (visible && (parts[1] == ".nlss" || get_workspace_manifest_name() %in% parts)) {
    stop("Source, working data and context must be outside managed metadata.")
  }
  current <- root
  for (i in seq_along(parts)) {
    current <- file.path(current, parts[i])
    if (i < length(parts) && file.exists(current) && !dir.exists(current)) stop("Path parent is not a directory.")
  }
  if (file && (!nlss_project_regular_file(target$absolute) || target$status != "present")) {
    stop("Expected a regular project file.")
  }
  target$absolute
}

nlss_managed_json <- function(root, relative) {
  value <- nlss_project_metadata(root, relative)
  if (value$status != "read") stop("Invalid managed metadata: ", value$status)
  value$value
}

nlss_managed_project <- function(project = NULL, dataset = NULL) {
  marker <- nlss_locate_project(project)
  root <- dirname(marker)
  manifest <- nlss_project_metadata(root, basename(marker), yaml = TRUE)
  if (manifest$status != "read" || !identical(as.integer(manifest$value$schema_version), 2L) ||
      !identical(manifest$value$storage, "managed_parquet_v1")) stop("Expected an explicitly created managed Parquet project; no migration was performed.")
  manifest <- manifest$value
  nlss_project_string(manifest$workspace_id, "Workspace ID")
  entries <- nlss_project_dataset_entries(manifest$datasets)
  if (is.null(dataset)) dataset <- manifest$active_dataset
  nlss_project_string(dataset, "Dataset selection")
  index <- which(vapply(entries, function(x) identical(x$name, dataset), logical(1)))
  if (length(index) != 1L) stop("Dataset is not registered in this project.")
  entry <- entries[[index]]
  if (!is.character(entry$id) || length(entry$id) != 1L || !grepl("^ds-[a-zA-Z0-9]+$", entry$id)) stop("Invalid managed dataset identity.")
  descriptor <- paste0(".nlss/datasets/", entry$id, "/dataset.json")
  record <- nlss_managed_json(root, descriptor)
  if (!identical(as.integer(record$schema_version), 1L) || !identical(record$dataset_id, entry$id) ||
      !identical(record$workspace_id, manifest$workspace_id)) stop("Dataset/project identity mismatch.")
  working <- nlss_managed_path(root, record$working, visible = TRUE)
  original <- nlss_managed_path(root, record$source$selected_path, visible = TRUE)
  if (identical(working, original)) stop("Working location must not replace the original source.")
  if (!identical(record$source$path, make_relative_path(nlss_object_file(root, record$source$sha256), root))) stop("Source reference must identify its preserved object.")
  list(root = root, marker = basename(marker), manifest = manifest, name = dataset,
       descriptor = descriptor, record = record,
       marker_sha256 = import_hash(marker, file = TRUE),
       descriptor_sha256 = import_hash(nlss_managed_path(root, descriptor, file = TRUE), file = TRUE))
}

nlss_object_file <- function(root, hash) {
  if (!is.character(hash) || length(hash) != 1L || is.na(hash) || !grepl("^[0-9a-f]{64}$", hash)) stop("Invalid object SHA-256.")
  nlss_managed_path(root, paste0(".nlss/objects/", hash))
}

nlss_verify_object <- function(root, hash) {
  path <- nlss_object_file(root, hash)
  if (!nlss_project_regular_file(path) || !identical(import_hash(path, file = TRUE), hash)) stop("Managed object failed integrity validation.")
  path
}

nlss_store_file <- function(root, relative, max_bytes = Inf) {
  path <- nlss_managed_path(root, relative, file = TRUE)
  before <- file.info(path)
  if (before$size > max_bytes) stop("Selected file exceeds the capture limit (", max_bytes, " bytes).")
  hash <- import_hash(path, file = TRUE)
  object <- nlss_object_file(root, hash)
  if (!file.exists(object)) {
    stage <- nlss_managed_path(root, ".nlss/staging")
    temporary <- tempfile("capture-", tmpdir = stage)
    on.exit(unlink(temporary), add = TRUE)
    if (!file.copy(path, temporary) || !identical(import_hash(temporary, file = TRUE), hash)) stop("File changed during capture; retry with stable bytes.")
    if (!identical(before[c("size", "mtime", "ctime")], file.info(path)[c("size", "mtime", "ctime")]) ||
        !identical(import_hash(nlss_managed_path(root, relative, file = TRUE), file = TRUE), hash)) stop("File changed during capture; retry with stable bytes.")
    if (!file.rename(temporary, object)) stop("Could not publish preserved object.")
    Sys.chmod(object, mode = "0400")
  }
  nlss_verify_object(root, hash)
  if (!identical(import_hash(nlss_managed_path(root, relative, file = TRUE), file = TRUE), hash)) stop("File changed during capture.")
  list(path = make_relative_path(object, root), sha256 = hash, bytes = as.numeric(before$size))
}

nlss_managed_version <- function(project, data, df) {
  root <- project$root
  dictionary <- attr(df, "nlss_import_contract")
  if (is.null(dictionary)) dictionary <- import_capture_dictionary(df)
  temporary <- tempfile("dictionary-", tmpdir = nlss_managed_path(root, ".nlss/staging"))
  on.exit(unlink(temporary), add = TRUE)
  write_import_json(dictionary, temporary)
  dict <- nlss_store_file(root, make_relative_path(temporary, root))
  id <- paste0("v-", import_hash(import_json(list(dataset = project$record$dataset_id,
    data = data$sha256, dictionary = dict$sha256))))
  reference <- list(schema_version = 1L, dataset_id = project$record$dataset_id, version_id = id,
    data_sha256 = data$sha256, dictionary_sha256 = dict$sha256,
    snapshot_path = data$path, dictionary_path = dict$path,
    source_sha256 = project$record$source$sha256, origin = "managed_parquet")
  path <- nlss_managed_path(root, paste0(".nlss/datasets/", reference$dataset_id, "/versions/", id, ".json"))
  if (file.exists(path)) {
    if (!identical(import_json(nlss_managed_json(root, make_relative_path(path, root))), import_json(reference))) stop("Existing version descriptor differs.")
  } else write_import_json(reference, path)
  reference
}

nlss_project_import_options <- function(format, opts) {
  csv <- c(sep = "sep", header = "header", `csv-decimal` = "decimal",
    `csv-encoding` = "encoding", `csv-col-types` = "col_types", `csv-na-values` = "na_values")
  allowed <- if (format == "csv") names(csv) else if (format == "rdata") "df" else character()
  if (!is.list(opts) || (length(opts) && (is.null(names(opts)) || anyDuplicated(names(opts)) ||
      any(!names(opts) %in% allowed)))) stop("Import options do not match the selected source format.")
  if (format == "csv") {
    for (flag in names(csv)) if (is.null(opts[[flag]])) opts[[flag]] <- get_config_value(paste0("defaults.csv.", csv[[flag]]))
    opts$header <- resolve_parse_bool(opts$header)
  }
  if (format == "rdata") nlss_project_string(opts$df, "--df (RData object)")
  opts
}

nlss_project_setup_result <- function(project, status) {
  list(schema_version = 1L, status = status, project = project$root,
    workspace_id = project$manifest$workspace_id,
    dataset = list(name = project$name, id = project$record$dataset_id,
      working = project$record$working, initial_version = project$record$initial_version$version_id),
    source = project$record$source,
    protocol = list(path = "report_canonical.md", available = file.exists(file.path(project$root, "report_canonical.md"))))
}

# Callable directly by an adapter; the CLI only parses options and prints JSON.
nlss_create_project <- function(project, source = NULL, working = NULL, name = NULL, import_options = list()) {
  nlss_dependency_preflight("project_create", c(list(source = source), import_options))
  root <- normalizePath(nlss_project_string(project, "--project"), winslash = "/", mustWork = TRUE)
  if (!dir.exists(root)) stop("--project must be an existing directory.")
  nlss_project_string(name, "--name", optional = TRUE)
  marker <- nlss_managed_path(root, get_workspace_manifest_name())
  area <- nlss_managed_path(root, ".nlss")
  existing <- nlss_project_exists(marker)
  current <- if (existing) nlss_managed_project(root) else NULL
  if (!existing && nlss_project_exists(area)) stop(".nlss already exists without a current marker; inspect incomplete initialization. No adoption or migration was performed.")
  if (is.null(source)) {
    if (!existing) stop("--source is required to initialize an unmarked folder.")
    if (length(import_options)) stop("Import options require --source.")
    if (!is.null(name)) current <- nlss_managed_project(root, name)
    nlss_managed_path(root, current$record$working, file = TRUE, visible = TRUE)
    if (!is.null(working) && !identical(make_relative_path(nlss_managed_path(root, working, visible = TRUE), root), current$record$working)) stop("Working location differs from the registration; no relocation was performed.")
    return(nlss_project_setup_result(current, "reused"))
  }
  original <- nlss_managed_path(root, source, file = TRUE, visible = TRUE)
  format <- tolower(tools::file_ext(original))
  if (!format %in% c("csv", "sav", "rds", "rdata", "parquet")) stop("Supported source formats: CSV, SAV, RDS, RData, Parquet.")
  opts <- nlss_project_import_options(format, import_options)
  binding <- import_source_descriptor(original, format, opts)
  # Portable source identity: moving the whole project does not change it.
  selected <- make_relative_path(original, root)
  binding$source_key <- import_hash(selected)
  entries <- if (existing) nlss_project_dataset_entries(current$manifest$datasets) else list()
  registrations <- lapply(entries, function(entry) nlss_managed_project(root, entry$name))
  matched <- which(vapply(registrations, function(p) identical(p$record$source$selected_path, selected), logical(1)))
  if (is.null(name) && length(matched) > 1L) stop("Source has multiple registrations; select --name explicitly.")
  if (is.null(name)) name <- if (length(matched) == 1L) registrations[[matched]]$name else derive_dataset_label(original, opts$df)
  nlss_project_string(name, "Dataset name")
  named <- which(vapply(registrations, function(p) identical(p$name, name), logical(1)))
  if (length(named)) {
    saved <- registrations[[named]]
    if (!identical(saved$record$source$selected_path, selected)) stop("Dataset name is bound to a different source; choose a distinct --name. No data replaced.")
    prior <- saved$record$source$import
    if (!identical(saved$record$source$sha256, binding$source_sha256) || is.null(prior) ||
        !identical(prior$format, format) || !identical(prior$options_sha256, binding$options_sha256)) stop("Source bytes or import options differ from the registration. Use a distinct --name and --working for a deliberate separate import; edited data were not reset.")
    nlss_managed_path(root, saved$record$working, file = TRUE, visible = TRUE)
    if (!is.null(working) && !identical(make_relative_path(nlss_managed_path(root, working, visible = TRUE), root), saved$record$working)) stop("Working location differs from the registration; no relocation was performed.")
    return(nlss_project_setup_result(saved, "reused"))
  }
  if (is.null(working)) working <- paste0("data/", sanitize_file_component(name), "_working.parquet")
  target <- nlss_managed_path(root, working, visible = TRUE)
  if (tolower(tools::file_ext(target)) != "parquet") stop("The editable working file must be Parquet.")
  if (nlss_project_exists(target)) stop("Working destination already exists; no file overwritten.")
  if (any(vapply(registrations, function(p) identical(p$record$working, make_relative_path(target, root)) ||
      identical(p$record$source$selected_path, make_relative_path(target, root)), logical(1)))) stop("Working destination is already registered; no file recreated or replaced.")
  # Validate the scientific contract before allocating any managed project state.
  df <- switch(format, csv = import_csv(original, opts), sav = read_sav_data(original),
    rds = readRDS(original), rdata = load_rdata_frame(original, opts$df)$df, parquet = read_parquet_data(original))
  if (!is.data.frame(df)) stop("Input does not contain a data frame.")
  prepared <- original
  if (format != "parquet") {
    df <- resolve_attach_label_metadata(df, resolve_extract_label_metadata(df))
    df <- import_prepare_analysis(df)
    prepared <- tempfile("nlss-import-", fileext = ".parquet")
    on.exit(unlink(prepared), add = TRUE)
    write_parquet_data(df, prepared)
    df <- read_parquet_data(prepared)
  }
  if (!identical(import_hash(original, file = TRUE), binding$source_sha256)) stop("Source changed during import; retry with stable bytes.")
  if (!existing) {
    if (!dir.create(area, showWarnings = FALSE)) stop("Could not reserve .nlss; inspect any interrupted initialization.")
    Sys.chmod(area, mode = "0700")
  } else {
    # Reuse existing writer locks; no new journal or initialization lock family.
    owned_locks <- character()
    on.exit(for (owned in owned_locks) unlink(owned, recursive = TRUE), add = TRUE)
    for (relative in c(".nlss/.analysis-lock", ".nlss/.publication-lock")) {
      lock <- nlss_managed_path(root, relative)
      if (!dir.create(lock, showWarnings = FALSE)) stop("Project is locked; inspect a running/interrupted operation before retrying.")
      owned_locks <- c(owned_locks, lock)
    }
    if (!identical(import_hash(marker, file = TRUE), current$marker_sha256)) stop("Project registration changed during import; retry.")
  }
  id <- paste0("ds-", generate_manifest_id())
  workspace_id <- if (existing) current$manifest$workspace_id else generate_manifest_id()
  for (relative in c("objects", "staging", "runs", paste0("datasets/", id, "/versions"))) {
    dir.create(file.path(area, relative), recursive = TRUE, showWarnings = FALSE)
  }
  frozen <- nlss_store_file(root, make_relative_path(original, root))
  if (!identical(frozen$sha256, binding$source_sha256)) stop("Source changed before preservation; import incomplete.")
  record <- list(schema_version = 1L, workspace_id = workspace_id, dataset_id = id,
    source = c(list(selected_path = selected, import = binding), frozen),
    working = make_relative_path(target, root))
  context <- list(root = root, record = record)
  data <- frozen
  if (format != "parquet") {
    stage <- tempfile("import-", tmpdir = file.path(area, "staging"), fileext = ".parquet")
    on.exit(unlink(stage), add = TRUE)
    if (!file.copy(prepared, stage)) stop("Could not stage imported working data.")
    data <- nlss_store_file(root, make_relative_path(stage, root))
  }
  record$initial_version <- nlss_managed_version(context, data, df)
  write_import_json(record, file.path(area, "datasets", id, "dataset.json"))
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(".nlss-publish-", tmpdir = dirname(target))
  on.exit(unlink(temporary), add = TRUE)
  if (!file.copy(nlss_verify_object(root, data$sha256), temporary, copy.mode = FALSE) ||
      !identical(import_hash(temporary, file = TRUE), data$sha256)) stop("Could not prepare independent working data.")
  Sys.chmod(temporary, mode = as.octmode(bitwOr(as.integer(file.info(original)$mode), 128L)))
  target <- nlss_managed_path(root, record$working, visible = TRUE)
  if (nlss_project_exists(target) || !file.rename(temporary, target)) stop("Working destination appeared or could not be published; initialization incomplete.")
  manifest <- if (existing) current$manifest else list(schema_version = 2L,
    storage = "managed_parquet_v1", workspace_id = workspace_id, active_dataset = name)
  manifest$datasets <- c(entries, list(list(name = name, id = id)))
  staged_marker <- tempfile("workspace-", tmpdir = file.path(area, "staging"), fileext = ".yml")
  on.exit(unlink(staged_marker), add = TRUE)
  yaml::write_yaml(manifest, staged_marker)
  if ((existing && !identical(import_hash(marker, file = TRUE), current$marker_sha256)) ||
      (!existing && nlss_project_exists(marker)) || !file.rename(staged_marker, marker)) stop("Could not publish project marker; inspect incomplete registration; do not delete prior data.")
  nlss_project_setup_result(nlss_managed_project(root, name), if (existing) "dataset_added" else "created")
}

nlss_managed_load <- function(opts) {
  incompatible <- intersect(names(opts), c("csv", "sav", "rds", "rdata", "parquet", "df", "sep", "header",
    "dataset-name", "import-action", "csv-decimal", "csv-encoding", "csv-col-types", "csv-na-values"))
  if (length(incompatible)) stop("Managed project selection cannot be combined with legacy source/import or interactive options.")
  project <- nlss_managed_project(opts$project, opts$dataset)
  root <- project$root
  lock <- nlss_managed_path(root, ".nlss/.analysis-lock")
  if (!dir.create(lock, showWarnings = FALSE)) stop("Managed project is locked; inspect a running/interrupted operation before recovery.")
  nlss_run_context$lock <- lock
  nlss_run_context$managed <- project
  nlss_verify_object(root, project$record$source$sha256)
  working <- nlss_managed_path(root, project$record$working, file = TRUE, visible = TRUE)
  frozen <- nlss_store_file(root, make_relative_path(working, root))
  df <- read_parquet_data(nlss_verify_object(root, frozen$sha256))
  reference <- nlss_managed_version(project, frozen, df)
  nlss_run_context$working_path <- working
  attr(df, "nlss_dataset_ref") <- reference
  attr(df, "workspace_dir") <- nlss_managed_path(root, ".nlss")
  attr(df, "workspace_parquet_path") <- working
  df
}

nlss_verify_managed_run <- function(request, root) {
  project <- nlss_managed_project(root, request$project$dataset_name)
  if (!identical(project$manifest$workspace_id, request$project$workspace_id) ||
      !identical(project$record$dataset_id, request$dataset$dataset_id)) stop("Managed run belongs to another project or dataset.")
  for (kind in c("data", "dictionary")) {
    hash <- request$dataset[[paste0(kind, "_sha256")]]
    path <- request$dataset[[if (kind == "data") "snapshot_path" else "dictionary_path"]]
    if (!identical(path, make_relative_path(nlss_verify_object(root, hash), root))) stop("Managed version must reference its content-addressed object.")
  }
  nlss_verify_object(root, request$dataset$source_sha256)
  invisible(project)
}

nlss_publish_managed_run <- function(result, success) {
  context <- nlss_run_context
  root <- context$root
  if (!is.null(context$managed)) nlss_verify_managed_run(context$request, root)
  lock <- nlss_managed_path(root, ".nlss/.publication-lock")
  if (!dir.create(lock, showWarnings = FALSE)) stop("Managed project publication is locked; inspect before recovery.")
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  for (relative in c(make_relative_path(context$staging, root), make_relative_path(context$destination, root))) nlss_managed_path(root, relative)
  changed <- success && !is.null(context$data_change) && isTRUE(context$data_change$applied)
  replaced <- published <- FALSE
  protocol <- nlss_managed_path(root, "report_canonical.md")
  targets <- c(protocol, if (changed && is.null(context$managed)) nlss_data_change_targets())
  for (target in targets) if (file.exists(target) && !nlss_project_regular_file(target)) stop("Publication target must be a regular file.")
  existed <- file.exists(targets)
  backup <- tempfile("projection-", tmpdir = context$staging)
  dir.create(backup)
  copies <- file.path(backup, seq_along(targets))
  remove_backup <- TRUE
  on.exit(if (remove_backup) unlink(backup, recursive = TRUE), add = TRUE)
  for (i in which(existed)) if (!file.copy(targets[i], copies[i])) stop("Could not protect project output.")
  on.exit({
    if (!published) for (i in seq_along(targets)) {
      restored <- tryCatch({
        nlss_managed_path(root, make_relative_path(targets[i], root))
        if (changed && is.null(context$managed) && identical(targets[i], context$working_path) &&
            !import_hash(targets[i], file = TRUE) %in% c(context$data_change$input$data_sha256, context$data_change$output$data_sha256)) {
          context$recovery_status <- "manual_recovery_required"
          stop("Working file changed again; automatic rollback refused.")
        }
        if (existed[i]) file.copy(copies[i], targets[i], overwrite = TRUE) &&
          identical(import_hash(copies[i], file = TRUE), import_hash(targets[i], file = TRUE))
        else !file.exists(targets[i]) || (!dir.exists(targets[i]) && unlink(targets[i]) == 0L)
      }, error = function(e) FALSE)
      if (!isTRUE(restored)) remove_backup <- FALSE
    }
    if (!remove_backup) {
      write_import_json(lapply(seq_along(targets), function(i) list(target = make_relative_path(targets[i], root),
        existed = existed[i], backup_file = if (existed[i]) as.character(i) else NULL)), file.path(backup, "targets.json"))
      warning("Project output recovery was incomplete; original bytes remain in the pending run's projection backup.")
    }
  }, add = TRUE, after = FALSE)
  if (changed && is.null(context$managed)) nlss_publish_data_change()
  if (changed && !is.null(context$managed)) {
    project <- context$managed
    check_current <- function() {
      working <- tryCatch(nlss_managed_path(root, project$record$working, file = TRUE, visible = TRUE),
        error = function(e) { context$publication_conflict <- TRUE; stop("Publication conflict: working location unavailable or unsafe; candidate retained, not applied.") })
      if (!identical(import_hash(working, file = TRUE), context$request$dataset$data_sha256) ||
          !identical(import_hash(nlss_managed_path(root, project$marker, file = TRUE), file = TRUE), project$marker_sha256) ||
          !identical(import_hash(nlss_managed_path(root, project$descriptor, file = TRUE), file = TRUE), project$descriptor_sha256)) {
        context$publication_conflict <- TRUE
        stop("Publication conflict: working data or registration changed; candidate retained, not applied.")
      }
      working
    }
    target <- check_current()
    working_mode <- file.info(target)$mode
    replacement <- tempfile(".nlss-publish-", tmpdir = dirname(target))
    on.exit(unlink(replacement), add = TRUE)
    candidate <- nlss_verify_object(root, context$data_change$output$data_sha256)
    if (!file.copy(candidate, replacement, copy.mode = FALSE) ||
        !identical(import_hash(replacement, file = TRUE), context$data_change$output$data_sha256)) stop("Could not stage working-file replacement.")
    Sys.chmod(replacement, mode = working_mode)
    check_current()
    if (!file.rename(replacement, target)) stop("Could not replace working file; original remains. Atomic replacement may be unavailable on this platform.")
    replaced <- TRUE
  }
  # A normal final-bundle failure restores only bytes still owned by this attempt.
  # The already frozen input is the recovery copy; no timestamped backup is added.
  on.exit({
    if (replaced && !published) {
      recovered <- tryCatch({
        target <- nlss_managed_path(root, context$managed$record$working, file = TRUE, visible = TRUE)
        if (!identical(import_hash(target, file = TRUE), context$data_change$output$data_sha256)) stop("Working file changed again; automatic rollback refused.")
        restore <- tempfile(".nlss-restore-", tmpdir = dirname(target))
        tryCatch({
          input <- nlss_verify_object(root, context$request$dataset$data_sha256)
          if (!file.copy(input, restore, copy.mode = FALSE) || !identical(import_hash(restore, file = TRUE), context$request$dataset$data_sha256)) stop("Could not prepare recovery.")
          Sys.chmod(restore, mode = working_mode)
          if (!identical(import_hash(target, file = TRUE), context$data_change$output$data_sha256) || !file.rename(restore, target)) stop("Could not safely restore working file.")
        }, finally = unlink(restore))
        TRUE
      }, error = function(e) { message("Managed recovery requires inspection: ", conditionMessage(e)); FALSE })
      context$recovery_status <- if (recovered) "restored_input" else "manual_recovery_required"
    }
  }, add = TRUE)
  result$published_at_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
  write_import_json(result, file.path(context$staging, "result.json"))
  output <- if (success) "output.md" else "partial-output.md"
  output <- if (file.exists(file.path(context$staging, output))) file.path(context$staging, output) else NULL
  nlss_append_project_protocol(root, make_relative_path(context$destination, root), context$request, result, output)
  if (nlss_project_exists(context$destination) || !file.rename(context$staging, context$destination)) stop("Could not publish managed run bundle.")
  backup <- file.path(context$destination, basename(backup))
  published <- TRUE
  context$staging <- NULL
  cat("Analysis run: ", make_relative_path(context$destination, root), " (", result$status, ")\n", sep = "")
  invisible(result)
}
