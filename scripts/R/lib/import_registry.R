# SPDX-License-Identifier: Apache-2.0
# Phase 1: bind an import to its source and snapshot the exact analysis input.
# The existing dataset workspace remains the editable compatibility interface.
nlss_dataset_context <- new.env(parent = emptyenv())

print_import_usage <- function() {
  cat("  --csv-decimal .|,      CSV decimal mark (default: .)\n",
      "  --csv-encoding NAME    CSV encoding (default: UTF-8)\n",
      "  --csv-col-types SPEC   Comma-separated name=type declarations\n",
      "  --csv-na-values LIST   Comma-separated CSV missing tokens\n",
      "  --dataset-name NAME    Distinct name for colliding source filenames\n",
      "  --import-action MODE   verify (default) or explicit new-version\n", sep = "")
}

import_hash <- function(value, file = FALSE) {
  if (!requireNamespace("digest", quietly = TRUE)) stop("Import provenance requires the 'digest' package.")
  digest::digest(value, algo = "sha256", serialize = !file && !is.character(value), file = file)
}

import_json <- function(value) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Import provenance requires the 'jsonlite' package.")
  as.character(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", na = "null", digits = NA))
}

read_import_json <- function(path) {
  if (!file.exists(path)) return(NULL)
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

write_import_json <- function(value, path) {
  ensure_out_dir(dirname(path))
  tmp <- tempfile(".nlss-json-", tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  writeBin(charToRaw(paste0(import_json(value), "\n")), tmp)
  if (!file.rename(tmp, path)) stop("Could not publish import metadata: ", path)
  invisible(path)
}

import_source_descriptor <- function(path, format, opts) {
  fields <- if (format == "csv") c("sep", "header", "csv-decimal", "csv-encoding", "csv-col-types", "csv-na-values") else if (format == "rdata") "df" else character(0)
  effective <- opts[intersect(fields, names(opts))]
  # init-workspace's multi-source map can attach a name to this scalar. That
  # bookkeeping attribute does not change which RData object is read.
  if (format == "rdata" && !is.null(effective$df)) effective$df <- as.character(effective$df)
  if (format == "csv") {
    effective$sep <- if (is.null(opts$sep)) resolve_config_value("defaults.csv.sep", ",") else opts$sep
    effective$header <- if (is.null(opts$header)) resolve_config_value("defaults.csv.header", TRUE) else resolve_parse_bool(opts$header)
    if (is.null(effective$`csv-decimal`)) effective$`csv-decimal` <- "."
    if (is.null(effective$`csv-encoding`)) effective$`csv-encoding` <- "UTF-8"
  }
  effective <- effective[sort(names(effective))]
  list(format = format, source_name = basename(path),
       source_key = import_hash(normalize_path(path)),
       source_sha256 = import_hash(path, file = TRUE),
       options = effective, options_sha256 = import_hash(effective),
       reader_version = if (format == "sav") as.character(utils::packageVersion("haven")) else if (format == "parquet") as.character(utils::packageVersion("arrow")) else R.version.string,
       contract_version = 1L)
}

import_codebook <- function(df, path, reference) {
  # Only aggregate metadata is shown; no participant-level values or identifiers.
  esc <- function(x) gsub("[\r\n|]", " ", paste(x, collapse = ", "))
  contract <- attr(df, "nlss_import_contract")
  lines <- c("# Import preview / codebook", "",
             paste0("Dataset: `", reference$dataset_id, "`; version: `", reference$version_id, "`."), "",
             paste0(nrow(df), " rows; ", ncol(df), " columns. Missing codes are masked for analysis; original definitions are retained in dictionary.json."), "",
             "| Variable | Label | Analysis type | Missing n | Value labels |", "| --- | --- | --- | ---: | --- |")
  for (name in names(df)) {
    column <- contract$columns[[name]]
    variable_label <- column$variable_label
    value_labels <- column$value_labels
    values <- if (is.null(value_labels)) "" else paste(vapply(value_labels, function(x) paste0(x$label, "=", if (is.null(x$missing_tag)) x$value else paste0("NA(", x$missing_tag, ")")), character(1)), collapse = "; ")
    lines <- c(lines, paste0("| ", esc(name), " | ", esc(variable_label), " | ", esc(class(df[[name]])), " | ", sum(is.na(df[[name]])), " | ", esc(values), " |"))
  }
  lines <- c(lines, "", "Labels describe values; they do not by themselves declare nominal/ordinal/continuous model roles.",
             "This preview does not replace semantic interpretation or a final research report.")
  writeLines(lines, path, useBytes = TRUE)
}

dataset_version_key <- function(data_hash, dictionary_hash, binding) {
  source <- binding$source
  key <- list(data_sha256 = data_hash, dictionary_sha256 = dictionary_hash,
              dataset_id = binding$dataset_id, source_key = source$source_key,
              source_sha256 = source$source_sha256, options_sha256 = source$options_sha256,
              reader_version = source$reader_version, contract_version = source$contract_version)
  paste0("v-", import_hash(import_json(key)))
}

snapshot_dataset <- function(df, copy_path, binding = NULL, dataset_dir = dirname(copy_path), update_preview = TRUE) {
  dictionary <- attr(df, "nlss_import_contract")
  if (is.null(dictionary)) dictionary <- import_capture_dictionary(df)
  data_hash <- import_hash(copy_path, file = TRUE)
  loaded_hash <- attr(df, "nlss_input_file_sha256")
  if (!is.null(loaded_hash) && !identical(loaded_hash, data_hash)) stop("Workspace data changed before snapshotting; retry the analysis.")
  dictionary_hash <- import_hash(paste0(import_json(dictionary), "\n"))
  if (is.null(binding)) binding <- read_import_json(file.path(dataset_dir, "import.json"))
  if (!is.null(binding$state) && binding$state != "ready") stop("Cannot analyze a pending/interrupted import. Recover explicitly with --import-action new-version.")
  if (!is.null(binding$source)) {
    raw_path <- resolve_manifest_path(binding$source_copy, get_default_out())
    if (!file.exists(raw_path) || !identical(import_hash(raw_path, file = TRUE), binding$source$source_sha256)) stop("Preserved source failed integrity validation; no analysis was run.")
  }
  dataset_id <- if (!is.null(binding$dataset_id)) binding$dataset_id else paste0("ds-", substr(import_hash(normalize_path(copy_path)), 1, 24))
  if (is.null(binding)) binding <- list(dataset_id = dataset_id)
  version <- dataset_version_key(data_hash, dictionary_hash, binding)
  version_dir <- file.path(dataset_dir, "versions", version)
  snapshot <- file.path(version_dir, "data.parquet")
  ref <- list(schema_version = 1L, dataset_id = dataset_id, version_id = version,
              data_sha256 = data_hash, dictionary_sha256 = dictionary_hash,
              snapshot_path = make_relative_path(snapshot, get_default_out()),
              dictionary_path = make_relative_path(file.path(version_dir, "dictionary.json"), get_default_out()),
              source_sha256 = binding$source$source_sha256,
              import_version_id = binding$import_version_id,
              origin = if (is.null(binding$source)) "legacy_or_external_working_copy" else "verified_import_or_working_copy")
  if (!dir.exists(version_dir)) {
    ensure_out_dir(dirname(version_dir))
    staging <- tempfile(".snapshot-", tmpdir = dirname(version_dir))
    dir.create(staging)
    on.exit(unlink(staging, recursive = TRUE), add = TRUE)
    if (!file.copy(copy_path, file.path(staging, "data.parquet"))) stop("Could not preserve analysis input snapshot.")
    if (!identical(import_hash(file.path(staging, "data.parquet"), file = TRUE), data_hash)) stop("Workspace data changed while preserving the snapshot.")
    write_import_json(dictionary, file.path(staging, "dictionary.json"))
    write_import_json(ref, file.path(staging, "provenance.json"))
    if (!file.rename(staging, version_dir)) stop("Could not publish analysis input snapshot.")
  } else {
    if (!file.exists(snapshot) || import_hash(snapshot, file = TRUE) != data_hash) stop("Existing immutable dataset snapshot failed integrity validation.")
    saved <- file.path(version_dir, "dictionary.json")
    if (!file.exists(saved) || import_hash(saved, file = TRUE) != dictionary_hash) stop("Existing dataset dictionary failed integrity validation.")
  }
  # Human-facing current preview is derived, never the authoritative old snapshot.
  if (isTRUE(update_preview)) {
    write_import_json(dictionary, file.path(dataset_dir, "dictionary.json"))
    import_codebook(df, file.path(dataset_dir, "codebook.md"), ref)
  }
  attr(df, "nlss_dataset_ref") <- ref
  attr(df, "workspace_parquet_path") <- normalize_path(copy_path)
  attr(df, "workspace_dir") <- normalize_path(dataset_dir)
  if (isTRUE(update_preview)) assign(normalize_path(dataset_dir), ref, envir = nlss_dataset_context)
  df
}

get_dataset_reference <- function(out_dir) {
  key <- normalize_path(out_dir)
  if (exists(key, envir = nlss_dataset_context, inherits = FALSE)) get(key, envir = nlss_dataset_context) else NULL
}

snapshot_working_dataframe <- function(path, lock_safe = FALSE) {
  # Direct Parquet/current-dataset loads also refresh the current preview. Share
  # the import lock so that refresh cannot race a transforming publication.
  lock <- file.path(dirname(path), ".import-lock")
  if (!dir.create(lock, showWarnings = FALSE)) stop("Dataset import or transformation is locked; current data/preview were not loaded or changed.")
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  snapshot_dataset(read_parquet_data(path, lock_safe = lock_safe), path)
}

load_verified_import <- function(copy_info, source_path, format, opts, read_source) {
  if (!file.exists(source_path)) stop("Input file not found: ", source_path)
  lock <- file.path(copy_info$out_dir, ".import-lock")
  if (!dir.create(lock, showWarnings = FALSE)) stop("Dataset import is locked by another or interrupted process. Check that no import is running before removing .import-lock.")
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  action <- if (is.null(opts$`import-action`)) "verify" else as.character(opts$`import-action`)
  if (length(action) != 1L || !action %in% c("verify", "new-version")) stop("--import-action must be verify or new-version.")
  path <- copy_info$copy_path
  binding_path <- file.path(copy_info$out_dir, "import.json")
  binding <- read_import_json(binding_path)
  if (!is.null(binding$state) && binding$state != "ready" && action != "new-version") stop("Previous import was interrupted. Use --import-action new-version to recover explicitly; preserved snapshots remain available.")
  source <- import_source_descriptor(source_path, format, opts)
  if (file.exists(path)) {
    if (is.null(binding) && action != "new-version") {
      stop("Existing workspace data have no verified source binding. Use --parquet for that working copy, or --import-action new-version to import the source explicitly; the old data will be preserved.")
    }
    if (!is.null(binding)) {
      if (!identical(binding$source$source_key, source$source_key)) stop("Dataset name is already bound to a different source. Use --dataset-name with a distinct name; no existing data were replaced.")
      unchanged <- identical(binding$source$source_sha256, source$source_sha256) && identical(binding$source$options_sha256, source$options_sha256) && identical(binding$source$format, source$format)
      if (unchanged && action == "verify" && (is.null(binding$state) || binding$state == "ready")) {
        df <- snapshot_dataset(read_parquet_data(path), path, binding)
        attr(df, "workspace_source_path") <- normalize_path(source_path)
        return(df)
      }
      if (action != "new-version") stop("Source contents or import options changed. Use --import-action new-version to import explicitly; the existing data version is unchanged.")
    }
    old_binding <- binding
    if (!is.null(binding$state) && binding$state != "ready") old_binding <- list(dataset_id = binding$dataset_id)
    snapshot_dataset(read_parquet_data(path), path, old_binding)
  }
  df <- read_source()
  if (!is.data.frame(df)) stop("Input does not contain a data frame.")
  df <- resolve_attach_label_metadata(df, resolve_extract_label_metadata(df))
  df <- import_prepare_analysis(df)
  # A source may change while it is being read. Never bind data to the wrong hash.
  if (!identical(import_hash(source_path, file = TRUE), source$source_sha256)) stop("Source changed during import; retry with a stable file.")
  dataset_id <- if (!is.null(binding$dataset_id)) binding$dataset_id else paste0("ds-", substr(import_hash(normalize_path(path)), 1, 24))
  # Preserve the original bytes independently of subsequent working-copy edits.
  raw_dir <- ensure_out_dir(file.path(copy_info$out_dir, "sources", source$source_sha256))
  raw_path <- file.path(raw_dir, basename(source_path))
  if (!file.exists(raw_path) && !file.copy(source_path, raw_path)) stop("Could not preserve original source file.")
  if (!identical(import_hash(raw_path, file = TRUE), source$source_sha256)) stop("Preserved source failed integrity validation.")
  binding <- list(schema_version = 1L, dataset_id = dataset_id, source = source, state = "pending",
                  source_copy = make_relative_path(raw_path, get_default_out()),
                  imported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  write_import_json(binding, binding_path)
  write_parquet_data(df, path)
  imported <- read_parquet_data(path)
  dictionary_hash <- import_hash(paste0(import_json(attr(imported, "nlss_import_contract")), "\n"))
  binding$import_version_id <- dataset_version_key(import_hash(path, file = TRUE), dictionary_hash, binding)
  binding$state <- "ready"
  write_import_json(binding, binding_path)
  df <- snapshot_dataset(imported, path, binding)
  attr(df, "workspace_source_path") <- normalize_path(source_path)
  message("Imported ", nrow(df), " rows x ", ncol(df), " columns. Preview: ", render_output_path(file.path(copy_info$out_dir, "codebook.md"), copy_info$out_dir))
  df
}
