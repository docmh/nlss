# SPDX-License-Identifier: Apache-2.0
# Immutable mice artifacts keep their existing pooling-consumer format. They may
# outlive a failed data/run publication; only a completed run establishes use.
source_lib("data_change.R")
source_lib("mi_pool.R")

nlss_mids_comparison_policy <- function() list(schema_version = 1L,
  excluded_runtime_fields = c("call", "date"),
  comparison = "exact_scientific_fields_with_generated_formula_environment_contents",
  byte_reproducibility = FALSE)

nlss_mids_input_marker <- function(mids, dataset) {
  marker <- attr(mids$data, "nlss_input_version_id", exact = TRUE)
  if (!is.null(marker) && !identical(marker, dataset$version_id)) {
    stop("Mids input-version marker disagrees with its verified dataset reference.")
  }
  invisible(TRUE)
}

nlss_mids_equal <- function(original, current, ignore_runtime = TRUE) {
  # mice::mids records call=match.call() and date=Sys.Date(). All other fields,
  # including RNG state, chains and logged events, remain comparison inputs.
  if (ignore_runtime) {
    original[c("call", "date")] <- NULL
    current[c("call", "date")] <- NULL
  }
  exact <- function(a, b) identical(a, b, single.NA = FALSE, num.eq = FALSE,
    attrib.as.set = FALSE, ignore.environment = FALSE)
  compare <- function(a, b) {
    if (inherits(a, "formula") || inherits(b, "formula")) {
      if (!inherits(a, "formula") || !inherits(b, "formula")) return(FALSE)
      ea <- environment(a); eb <- environment(b)
      attr(a, ".Environment") <- attr(b, ".Environment") <- NULL
      if (!exact(a, b)) return(FALSE)
      # Default mice formulas capture a base::lapply frame, not researcher
      # state. Compare its captured bindings, including formula strings, rather
      # than environment addresses. Exotic formula environments are not replayed.
      if (!is.environment(ea) || !is.environment(eb) ||
          !identical(parent.env(ea), environment(base::lapply)) ||
          !identical(parent.env(eb), environment(base::lapply))) return(FALSE)
      if (identical(ea, eb)) return(TRUE)
      return(exact(as.list.environment(ea, all.names = TRUE, sorted = TRUE),
        as.list.environment(eb, all.names = TRUE, sorted = TRUE)))
    }
    plain_list <- function(x) is.list(x) && (is.null(attr(x, "class")) || is.data.frame(x) || inherits(x, "mids"))
    if (plain_list(a) && plain_list(b)) {
      if (!exact(attributes(a), attributes(b)) || length(a) != length(b)) return(FALSE)
      return(all(vapply(seq_along(a), function(i) compare(a[[i]], b[[i]]), logical(1))))
    }
    exact(a, b)
  }
  compare(original, current)
}

nlss_verify_imputation_artifact <- function(reference, root, expected_dataset = NULL) {
  if (!is.list(reference) || !identical(reference$object_class, "mids") ||
      !nlss_mi_sha(reference$sha256) || !nlss_mi_sha(reference$metadata_sha256) ||
      !identical(import_json(reference$payload_comparison), import_json(nlss_mids_comparison_policy()))) {
    stop("Invalid authenticated imputation artifact reference.")
  }
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  rds <- nlss_project_file(reference$path, root)
  metadata <- nlss_project_file(reference$metadata_path, root)
  nlss_data_change_path(file.path(root, reference$path), root)
  nlss_data_change_path(file.path(root, reference$metadata_path), root)
  if (!identical(metadata, file.path(dirname(rds), "metadata.json")) ||
      !identical(basename(dirname(dirname(rds))), "imputations") ||
      !identical(import_hash(metadata, file = TRUE), reference$metadata_sha256) ||
      !identical(import_hash(rds, file = TRUE), reference$sha256)) {
    stop("Imputation artifact or metadata failed SHA-256/location verification.")
  }
  saved <- read_import_json(metadata)
  extra <- c("metadata_path", "metadata_sha256", "payload_comparison")
  if (!identical(sort(names(reference)), sort(c(names(saved), extra))) ||
      !identical(import_json(reference[names(saved)]), import_json(saved))) {
    stop("Imputation artifact reference disagrees with its preserved metadata.")
  }
  if (!is.null(expected_dataset) &&
      !identical(import_json(saved$dataset), import_json(expected_dataset))) {
    stop("Imputation artifact input differs from its resolved request.")
  }
  verified <- nlss_read_mi_artifact(rds, workspace_root = root)
  nlss_mids_input_marker(verified$mids, verified$dataset_ref)
  if (!identical(verified$metadata_sha256, reference$metadata_sha256)) stop("Imputation metadata changed while verifying.")
  verified
}

nlss_preserve_mice_artifact <- function(mids, out_dir, dataset, seed) {
  context <- nlss_run_context
  if (!identical(context$request$module, "impute") || !is.null(context$imputation_artifact) ||
      !normalizePath(out_dir, winslash = "/", mustWork = TRUE) %in% c(context$out_dir, context$output_root) ||
      !identical(import_json(dataset), import_json(context$request$dataset))) {
    stop("Imputation artifacts require one active impute run and its verified input.")
  }
  seed_valid <- is.null(seed) || (is.numeric(seed) && length(seed) == 1L && (is.na(seed) || is.finite(seed)))
  if (!seed_valid || !inherits(mids, "mids") || !is.data.frame(mids$data) || !is.numeric(mids$m) || length(mids$m) != 1L ||
      !is.finite(mids$m) || mids$m < 2L || mids$m != floor(mids$m) ||
      !is.numeric(mids$iteration) || length(mids$iteration) != 1L || !is.finite(mids$iteration) || mids$iteration < 0L ||
      mids$iteration != floor(mids$iteration) ||
      (!is.null(seed) && !is.na(seed) && !identical(as.numeric(seed), as.numeric(mids$seed)))) {
    stop("Cannot preserve an invalid mids object or mismatched generation seed.")
  }
  nlss_verify_dataset(dataset, context$root)
  nlss_mids_input_marker(mids, dataset)
  # Match the existing consumer's allowed preparation (selected columns and
  # character-to-factor conversion), before releasing any immutable artifact.
  original <- as.data.frame(arrow::read_parquet(nlss_project_file(dataset$snapshot_path, context$root)))
  if (anyDuplicated(names(mids$data)) || !all(names(mids$data) %in% names(original)) ||
      nrow(mids$data) != nrow(original)) stop("Mids original cases/columns differ from the input snapshot.")
  for (name in names(mids$data)) {
    before <- original[[name]]; engine <- mids$data[[name]]
    if (is.character(before)) before <- as.factor(before)
    same <- if (is.factor(before) || is.factor(engine)) {
      is.factor(before) && is.factor(engine) && identical(levels(before), levels(engine)) &&
        identical(is.ordered(before), is.ordered(engine)) && identical(as.character(before), as.character(engine))
    } else isTRUE(all.equal(as.vector(before), as.vector(engine), tolerance = 0, check.attributes = FALSE))
    if (!same) stop("Mids original values/missingness differ from input variable: ", name)
  }
  if (!is.null(context$replay)) {
    artifact <- context$replay$result$results$imputation_artifact
    saved <- nlss_verify_imputation_artifact(artifact, context$root, dataset)
    if (!nlss_mids_equal(saved$mids, mids)) {
      stop("Recomputed mice scientific payload differs from its preserved imputations, diagnostics, RNG state or formula context.")
    }
  } else {
    # One artifact layout, also for a new unmarked file analysis. This does not
    # create a workspace marker or register/adopt a project.
    out_dir <- ensure_out_dir(file.path(context$root, ".nlss"))
    parent <- file.path(out_dir, "imputations")
    nlss_data_change_path(file.path(parent, "mids.rds"), out_dir)
    if (!dir.exists(parent) && !dir.create(parent)) stop("Could not create imputation artifact directory.")
    staging <- tempfile(".mice-", tmpdir = parent)
    if (!dir.create(staging)) stop("Could not stage imputation artifact.")
    on.exit(unlink(staging, recursive = TRUE), add = TRUE)
    staged_rds <- file.path(staging, "mids.rds")
    saveRDS(mids, staged_rds, version = 3L)
    if (!nlss_mids_equal(mids, readRDS(staged_rds), ignore_runtime = FALSE)) {
      stop("Imputation artifact failed exact RDS round-trip verification.")
    }
    sha256 <- import_hash(staged_rds, file = TRUE)
    artifact_id <- paste0("mice-", sha256)
    destination <- file.path(parent, artifact_id)
    rds_path <- nlss_data_change_path(file.path(destination, "mids.rds"), out_dir)
    metadata_path <- nlss_data_change_path(file.path(destination, "metadata.json"), out_dir)
    artifact <- list(schema_version = 1L, artifact_id = artifact_id,
      path = make_relative_path(rds_path, context$root), sha256 = sha256, object_class = "mids",
      m = mids$m, iterations = mids$iteration, mice_version = as.character(mids$version),
      r_version = R.version.string, seed = if (is.null(seed) || is.na(seed)) NULL else seed,
      dataset = dataset, completion_mode = "single_completion",
      completion_aggregation = "mean_numeric_mode_categorical", inference_pooled = FALSE)
    write_import_json(artifact, file.path(staging, "metadata.json"))
    if (file.exists(destination) && !dir.exists(destination)) stop("Imputation artifact directory target is a file.")
    if (!dir.exists(destination) && !file.rename(staging, destination)) {
      stop("Could not publish immutable imputation artifact; no existing artifact was overwritten.")
    }
    # An identical artifact may already exist. Reuse its immutable metadata only
    # when it binds this exact input; never repair/replace content in place.
    saved <- read_import_json(metadata_path)
    if (!identical(import_json(saved), import_json(artifact))) {
      stop("Existing immutable imputation artifact metadata differs; it was not overwritten.")
    }
    artifact <- saved
    artifact$metadata_path <- make_relative_path(metadata_path, context$root)
    artifact$metadata_sha256 <- import_hash(metadata_path, file = TRUE)
    artifact$payload_comparison <- nlss_mids_comparison_policy()
    nlss_verify_imputation_artifact(artifact, context$root, dataset)
  }
  record <- nlss_data_change_path(file.path(context$staging, "imputation-artifact.json"), context$staging)
  if (file.exists(record)) stop("Run-local imputation artifact reference already exists.")
  write_import_json(artifact, record)
  context$artifacts[["imputation-artifact.json"]] <- list(path = "imputation-artifact.json",
    sha256 = import_hash(record, file = TRUE))
  context$imputation_artifact <- artifact
  artifact
}
