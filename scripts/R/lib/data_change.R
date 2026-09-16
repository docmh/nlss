# SPDX-License-Identifier: Apache-2.0
# Bounded working-dataset publication, not a workflow engine.

nlss_data_column_equal <- function(actual, expected) {
  # Arrow uses a typed list wrapper for ordinary R list columns. Remove only
  # that known outer representation, never temporal units or element classes.
  plain_list <- function(value, other) {
    if (inherits(value, "arrow_list") && is.list(other) && !is.object(other)) {
      attr(value, "class") <- NULL
      attr(value, "ptype") <- NULL
    }
    value
  }
  isTRUE(all.equal(plain_list(actual, expected), plain_list(expected, actual), tolerance = 0))
}

nlss_data_change_path <- function(path, root) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  if (!startsWith(path, paste0(root, "/"))) stop("Data change target must remain inside its dataset/project.")
  relative <- substring(path, nchar(root) + 2L)
  parts <- strsplit(relative, "/", fixed = TRUE)[[1]]
  if (any(parts %in% c("", ".", ".."))) stop("Invalid data change target.")
  current <- root
  for (part in parts) {
    current <- file.path(current, part)
    link <- Sys.readlink(current)
    if ((!is.na(link) && nzchar(link)) || (file.exists(current) &&
        !identical(normalizePath(current, winslash = "/", mustWork = TRUE), current))) stop("Data change targets must not be symlinks.")
  }
  if (dir.exists(path)) stop("Data change file target is a directory.")
  path
}

nlss_prepare_data_change <- function(before_df, after_df, source_rows = NULL) {
  context <- nlss_run_context
  module <- context$request$module
  if (!module %in% c("data_transform", "missings", "impute") || !is.null(context$data_change)) stop("Data change requires one active data-changing run.")
  input <- context$request$dataset
  if (!identical(attr(before_df, "nlss_dataset_ref"), input)) stop("Transformation input differs from its resolved request.")
  if (!is.data.frame(after_df)) stop("Data change output must be a data frame.")
  if (module %in% c("data_transform", "impute")) {
    if (!is.null(source_rows) || !identical(nrow(before_df), nrow(after_df))) stop("Variable transformations must preserve source rows.")
    if (identical(module, "impute") &&
        (!identical(head(names(after_df), ncol(before_df)), names(before_df)) ||
         !all(vapply(names(before_df), function(name) nlss_data_column_equal(after_df[[name]], before_df[[name]]), logical(1))))) {
      stop("Imputation must preserve all original columns and their row order.")
    }
  } else {
    # Missing-data handling can delete rows, but cannot reorder, duplicate or
    # invent observations. The map always addresses the immutable INPUT version.
    if (!is.numeric(source_rows) || anyNA(source_rows) || any(!is.finite(source_rows)) ||
        any(source_rows != trunc(source_rows)) || length(source_rows) != nrow(after_df) ||
        any(source_rows < 1 | source_rows > nrow(before_df)) || any(diff(source_rows) <= 0)) {
      stop("Missing-data changes require an ordered, unique input-row map matching the output.")
    }
    source_rows <- as.integer(source_rows)
    same_rows <- function(recorded) {
      if (is.null(recorded)) return(FALSE)
      values <- if (is.list(recorded)) unlist(recorded, use.names = FALSE) else recorded
      if (!length(recorded)) values <- integer()
      is.numeric(values) && identical(as.numeric(values), as.numeric(source_rows))
    }
    if (!same_rows(context$request$design$source_rows)) {
      stop("Data change row map differs from its resolved request.")
    }
  }
  nlss_verify_dataset(input, context$root)
  candidate <- file.path(context$staging, "candidate.parquet")
  write_parquet_data(after_df, candidate)
  restored <- read_parquet_data(candidate)
  expected <- import_prepare_analysis(after_df)
  if (!identical(names(restored), names(expected)) || !identical(nrow(restored), nrow(expected)) ||
      !all(vapply(names(expected), function(name) nlss_data_column_equal(restored[[name]], expected[[name]]), logical(1)))) {
    stop("Transformed data failed Parquet round-trip verification.")
  }
  unchanged <- identical(nrow(before_df), nrow(restored)) && identical(names(before_df), names(restored)) &&
    all(vapply(names(restored), function(name) nlss_data_column_equal(before_df[[name]], restored[[name]]), logical(1))) &&
    identical(import_json(attr(before_df, "nlss_import_contract")), import_json(attr(restored, "nlss_import_contract")))
  replay <- context$replay
  if (is.null(replay)) {
    # Reuse the existing content-addressed version layout without changing the
    # live dictionary/codebook or the input reference used by the legacy log.
    output <- if (unchanged) input else {
      if (!is.null(context$managed)) {
        frozen <- nlss_store_file(context$root, make_relative_path(candidate, context$root))
        nlss_managed_version(context$managed, frozen, restored)
      } else {
        saved <- snapshot_dataset(restored, candidate, binding = context$binding,
          dataset_dir = context$out_dir, update_preview = FALSE)
        attr(saved, "nlss_dataset_ref")
      }
    }
    working <- if (!is.null(context$managed)) nlss_managed_path(context$root, context$managed$record$working, file = TRUE, visible = TRUE)
      else nlss_data_change_path(context$working_path, context$out_dir)
    backup <- if (unchanged || !is.null(context$locations$project_root)) NULL else paste0("backup/", tools::file_path_sans_ext(basename(working)), "-",
      format_backup_timestamp(), "-", context$request$run_id, ".parquet")
    output_path <- make_relative_path(working, context$root)
  } else {
    if (identical(module, "missings") &&
        !same_rows(replay$result$results$data_change$source_rows)) {
      stop("Replayed missing-data row map differs from its preserved output.")
    }
    output <- replay$result$results$data_change$output
    nlss_verify_dataset(output, context$root)
    dictionary_hash <- import_hash(paste0(import_json(attr(restored, "nlss_import_contract")), "\n"))
    if ((!(unchanged && identical(output, input)) && !identical(import_hash(candidate, file = TRUE), output$data_sha256)) ||
        !identical(dictionary_hash, output$dictionary_sha256)) stop("Replayed transformation differs from its preserved output version.")
    backup <- NULL
    output_path <- output$snapshot_path
  }
  # The immutable version holds the data bytes; keep a small authenticated
  # lineage record in the run instead of another complete dataset copy.
  unlink(candidate)
  change <- list(input = input, output = output, backup_path = backup,
    output_path = output_path, applied = is.null(replay) && !unchanged, unchanged = unchanged)
  if (identical(module, "missings")) {
    change$observation_basis <- "input_version_rows"
    change$source_rows <- source_rows
  }
  context$data_change <- change
  codebook <- file.path(context$staging, "codebook.md")
  import_codebook(restored, codebook, output)
  record <- file.path(context$staging, "data-change.json")
  write_import_json(change, record)
  for (name in c("codebook.md", "data-change.json")) {
    context$artifacts[[name]] <- list(path = name,
      sha256 = import_hash(file.path(context$staging, name), file = TRUE))
  }
  change
}

nlss_data_change_targets <- function() {
  context <- nlss_run_context
  change <- context$data_change
  working <- nlss_data_change_path(context$working_path, context$out_dir)
  if (!identical(import_hash(working, file = TRUE), change$input$data_sha256)) stop("Workspace data changed before publication; no transformation was applied.")
  binding_file <- nlss_data_change_path(file.path(context$out_dir, "import.json"), context$out_dir)
  current_binding <- if (file.exists(binding_file)) import_hash(binding_file, file = TRUE) else NULL
  if (!identical(current_binding, context$binding_hash)) stop("Import binding changed before publication; no transformation was applied.")
  if (!is.null(change$backup_path)) {
    backup <- nlss_data_change_path(file.path(context$out_dir, change$backup_path), context$out_dir)
    if (file.exists(backup)) stop("Transformation backup already exists; no file was overwritten.")
  }
  targets <- c(working, file.path(context$out_dir, c("dictionary.json", "codebook.md")))
  for (path in c(targets, file.path(context$out_dir, c("report_canonical.md", "analysis_log.jsonl")),
                 file.path(context$root, get_workspace_manifest_name()))) nlss_data_change_path(path, context$root)
  targets
}

nlss_publish_data_change <- function() {
  context <- nlss_run_context
  change <- context$data_change
  targets <- nlss_data_change_targets()
  nlss_verify_dataset(change$output, context$root)
  if (!is.null(change$backup_path)) {
    backup <- file.path(context$out_dir, change$backup_path)
    ensure_out_dir(dirname(backup))
    if (!file.copy(targets[1], backup, overwrite = FALSE) ||
        !identical(import_hash(backup, file = TRUE), change$input$data_sha256)) {
      if (file.exists(backup) && !dir.exists(backup)) unlink(backup)
      stop("Could not preserve verified transformation backup; working data were not replaced.")
    }
  }
  # Current projects already preserve the before-version and protect these
  # targets in the shared publisher; do not add a permanent copy of that input.
  sources <- c(nlss_project_file(change$output$snapshot_path, context$root),
               nlss_project_file(change$output$dictionary_path, context$root),
               file.path(context$staging, "codebook.md"))
  hashes <- c(change$output$data_sha256, change$output$dictionary_sha256,
              context$artifacts[["codebook.md"]]$sha256)
  for (i in seq_along(targets)) {
    if (!identical(import_hash(sources[i], file = TRUE), hashes[i]) ||
        !file.copy(sources[i], targets[i], overwrite = TRUE) ||
        !identical(import_hash(targets[i], file = TRUE), hashes[i])) stop("Could not publish verified transformed data and metadata.")
  }
  invisible(TRUE)
}
