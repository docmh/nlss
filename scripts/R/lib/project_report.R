# SPDX-License-Identifier: Apache-2.0
# Preserve an authored Markdown report and declared evidence, not a report engine.
source_lib("project_store.R")

nlss_report_id <- function(value, field, revision = FALSE) {
  nlss_project_string(value, field)
  pattern <- if (revision) "^rev-[0-9a-f]{64}$" else "^[A-Za-z0-9][A-Za-z0-9_-]{0,127}$"
  if (!grepl(pattern, value)) stop("Invalid ", field, "; use a safe identifier.")
  value
}

nlss_report_limit <- function(kind) {
  value <- get_config_value(paste0("modules.project_report.max_", kind, "_bytes"))
  if (!is.numeric(value) || length(value) != 1L || is.na(value) || !is.finite(value) || value <= 0) stop("Invalid report capture/read limit: ", kind)
  value
}

nlss_report_project <- function(project) {
  marker <- nlss_locate_project(project)
  root <- dirname(marker)
  read <- nlss_project_metadata(root, basename(marker), yaml = TRUE)
  manifest <- read$value
  if (read$status != "read" || !identical(as.integer(manifest$schema_version), 2L) ||
      !identical(manifest$storage, "managed_parquet_v1")) stop("Report revisions require a current NLSS project.")
  nlss_project_string(manifest$workspace_id, "Workspace identity")
  # Historical evidence does not depend on today's active dataset or locators.
  area <- nlss_managed_path(root, ".nlss")
  if (!dir.exists(area)) stop("Missing managed project storage.")
  list(root = root, workspace_id = manifest$workspace_id, marker = basename(marker),
       marker_sha256 = import_hash(marker, file = TRUE))
}

nlss_report_json <- function(root, relative) {
  path <- nlss_managed_path(root, relative, file = TRUE)
  if (file.info(path)$size > nlss_report_limit("evidence")) stop("Evidence JSON exceeds the configured read limit.")
  before <- import_hash(path, file = TRUE)
  value <- jsonlite::read_json(path, simplifyVector = FALSE)
  valid <- function(x) {
    if (!is.list(x)) return(TRUE)
    if (!is.null(names(x)) && (any(!nzchar(names(x))) || anyDuplicated(names(x)))) return(FALSE)
    all(vapply(x, valid, logical(1)))
  }
  if (!is.list(value) || is.null(names(value)) || !valid(value)) stop("Invalid or duplicate-key evidence JSON.")
  if (!identical(before, import_hash(nlss_managed_path(root, relative, file = TRUE), file = TRUE))) stop("Evidence changed while being read.")
  list(value = value, reference = list(path = relative, sha256 = before))
}

nlss_report_object <- function(root, reference) {
  path <- nlss_verify_object(root, reference$sha256)
  if (!identical(reference$path, make_relative_path(path, root)) ||
      !is.numeric(reference$bytes) || length(reference$bytes) != 1L ||
      !isTRUE(reference$bytes == file.info(path)$size)) stop("Captured document reference differs from its object.")
  invisible(TRUE)
}

nlss_report_locator <- function(path) {
  nlss_project_string(path, "Captured document location")
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  if (is_absolute_path(path) || grepl("\\\\", path) || endsWith(path, "/") ||
      any(parts %in% c("", ".", "..", get_workspace_manifest_name())) || parts[1] == ".nlss") stop("Invalid captured document location.")
  # Syntax only: historical selected paths need not still exist or be safe today.
  invisible(TRUE)
}

nlss_report_claim <- function() list(scope = "evidence_integrity_only", computation = "not_reexecuted", semantic_review = "not_certified")

nlss_report_run <- function(project, id) {
  nlss_report_id(id, "run ID")
  root <- project$root
  candidates <- paste0(".nlss/", c("runs", "utility-runs"), "/", id)
  present <- vapply(candidates, function(path) nlss_project_exists(nlss_managed_path(root, path)), logical(1))
  if (sum(present) != 1L) stop("Select an existing, unambiguous completed run ID.")
  prefix <- paste0(candidates[present], "/")
  saved_request <- nlss_report_json(root, paste0(prefix, "request.json"))
  saved_result <- nlss_report_json(root, paste0(prefix, "result.json"))
  request <- saved_request$value; result <- saved_result$value
  utility <- startsWith(prefix, ".nlss/utility-runs/")
  nlss_project_string(request$module, "Evidence module")
  valid_input <- if (utility) identical(request$kind, "utility") && identical(as.integer(request$schema_version), 1L)
    else isTRUE(request$resolved) &&
      ((identical(as.integer(request$schema_version), 1L) && is.list(request$dataset) && is.null(request$input)) ||
       (identical(as.integer(request$schema_version), 2L) && is.null(request$dataset) && identical(request$input, list(kind = "parameters"))))
  if (!valid_input || !identical(request$run_id, id) ||
      (!is.null(request$project) && !identical(request$project$workspace_id, project$workspace_id)) ||
      !identical(result$status, "completed") || !is.list(result$results) || !length(result$results) ||
      !is.null(result$error)) stop("Report evidence requires a completed current-layout run in this project.")
  for (key in c("schema_version", "kind", "run_id", "module", "input", "dataset", "storage", "project")) {
    if (!identical(result[[key]], request[[key]])) stop("Saved result does not match its request: ", key)
  }
  request_artifact <- result$artifacts[[if (utility) "request.json" else "request"]]
  output_artifact <- result$artifacts[[if (utility) "output.md" else "output"]]
  if (!identical(request_artifact$path, "request.json") ||
      !identical(request_artifact$sha256, saved_request$reference$sha256) ||
      !identical(output_artifact$path, "output.md")) stop("Completed run lacks authenticated request/output artifacts.")
  artifact_file <- function(relative) {
    nlss_project_string(relative, "Run-local artifact path")
    # Validate raw relative components BEFORE any normalization. No opaque
    # artifact (including RDS, expressions or configuration) is executed.
    if (is_absolute_path(relative) || grepl("\\\\", relative) ||
        any(strsplit(relative, "/", fixed = TRUE)[[1]] %in% c("", ".", "..")) ||
        endsWith(relative, "/")) stop("Unsafe run-local artifact path.")
    nlss_managed_path(root, paste0(prefix, relative), file = TRUE)
  }
  for (artifact in c(result$artifacts, request$templates)) {
    file <- artifact_file(artifact$path)
    if (!identical(import_hash(file, file = TRUE), artifact$sha256)) stop("Saved run artifact failed SHA-256 integrity validation.")
  }
  if (!is.null(request$dataset)) {
    # Reuse the common input snapshot check, without loading statistical objects
    # or reinterpreting individual procedures' lineage/estimator payloads.
    for (key in c("snapshot_path", "dictionary_path")) nlss_managed_path(root, request$dataset[[key]], file = TRUE)
    nlss_verify_dataset(request$dataset, root)
  }
  # Deliberately no current-code/environment or replay-eligibility check.
  list(run_id = id, module = request$module, request = saved_request$reference,
       result = saved_result$reference, dataset = request$dataset)
}

nlss_report_runs <- function(runs) {
  nlss_project_string(runs, "--runs")
  if (endsWith(runs, ",")) stop("Empty run ID in --runs.")
  ids <- trimws(strsplit(runs, ",", fixed = TRUE)[[1]])
  for (id in ids) nlss_report_id(id, "run ID")
  if (anyDuplicated(ids)) stop("Duplicate run IDs are not accepted.")
  sort(ids, method = "radix")
}

nlss_report_payload <- function(record) record[setdiff(names(record), c("revision_id", "finalized_at", "finalizer_code_sha256"))]
nlss_report_revision <- function(payload) paste0("rev-", import_hash(import_json(payload)))
nlss_report_record_path <- function(id, revision) paste0(".nlss/reports/", id, "/", revision, ".json")

nlss_report_verify <- function(project, report_id, revision, verify = TRUE) {
  nlss_report_id(report_id, "report ID"); nlss_report_id(revision, "revision ID", revision = TRUE)
  record <- nlss_report_json(project$root, nlss_report_record_path(report_id, revision))$value
  if (!identical(as.integer(record$schema_version), 1L) || !identical(record$kind, "semantic_report_revision") ||
      !identical(record$workspace_id, project$workspace_id) || !identical(record$report_id, report_id) ||
      !identical(record$revision_id, revision) || !identical(nlss_report_revision(nlss_report_payload(record)), revision)) stop("Report revision identity or payload failed integrity validation.")
  nlss_project_string(record$finalized_at, "Finalization timestamp")
  nlss_project_string(record$finalizer_code_sha256, "Finalizer code hash")
  if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}[.][0-9]+Z$", record$finalized_at) ||
      !grepl("^[0-9a-f]{64}$", record$finalizer_code_sha256) ||
      !identical(record$verification, nlss_report_claim())) stop("Invalid report finalization metadata or verification scope.")
  nlss_report_locator(record$report$selected_path)
  if (tolower(tools::file_ext(record$report$selected_path)) != "md" || !isTRUE(record$report$bytes > 0)) stop("Invalid preserved Markdown report.")
  if (!is.list(record$runs) || !length(record$runs)) stop("Report revision has no declared run evidence.")
  ids <- vapply(record$runs, function(run) nlss_report_id(run$run_id, "run ID"), "")
  if (anyDuplicated(ids) || !identical(ids, sort(ids, method = "radix"))) stop("Report evidence selection is not canonical.")
  if (verify) nlss_report_object(project$root, record$report)
  if (verify) for (i in seq_along(ids)) {
    actual <- nlss_report_run(project, ids[i])
    if (!identical(import_json(actual), import_json(record$runs[[i]]))) stop("Report's pinned run evidence differs; no references were replaced.")
  }
  record
}

nlss_inspect_report <- function(project = NULL, report_id = NULL, revision = NULL, report = NULL, verify = FALSE) {
  project <- nlss_report_project(project)
  verify <- parse_bool(verify)
  if (is.null(report_id)) {
    nlss_report_locator(report)
    report_id <- paste0("report-", import_hash(report))
  }
  nlss_report_id(report_id, "report ID")
  if (is.null(revision)) {
    if (verify) stop("Select one --revision for explicit verification.")
    directory <- nlss_managed_path(project$root, paste0(".nlss/reports/", report_id))
    revisions <- sub("[.]json$", "", list.files(directory, "^rev-[a-f0-9]{64}[.]json$"))
    return(list(status = "listed", evidence_verification = "not_checked", report_id = report_id,
      revisions = lapply(revisions, function(id) nlss_report_verify(project, report_id, id, verify = FALSE))))
  }
  record <- nlss_report_verify(project, report_id, revision, verify = verify)
  visible <- NULL
  if (!is.null(report)) {
    # Comparison is optional and read-only. Missing/edited/unsafe locators do
    # not invalidate an intact historical revision and are reported as such.
    nlss_project_string(report, "--report")
    selected <- nlss_project_path(project$root, report)
    if (selected$status %in% c("present", "missing")) nlss_managed_path(project$root, report, visible = TRUE)
    visible <- nlss_inspect_file(project$root, report, expected = record$report$sha256,
                                 verify = TRUE, max_bytes = nlss_report_limit("report"))
  }
  list(status = if (verify) "verified" else "read", evidence_verification = if (verify) "verified" else "not_checked",
       revision = record, visible_report = visible)
}

nlss_save_report <- function(project = NULL, report, runs, report_id = NULL) {
  project <- nlss_report_project(project)
  root <- project$root
  ids <- nlss_report_runs(runs)
  document <- function(relative, kind) {
    path <- nlss_managed_path(root, relative, file = TRUE, visible = TRUE)
    size <- file.info(path)$size
    if (tolower(tools::file_ext(path)) != "md" || size <= 0 || size > nlss_report_limit(kind)) stop("Select a nonempty Markdown document within the configured ", kind, " byte limit.")
    make_relative_path(path, root)
  }
  report <- document(report, "report")
  if (identical(report, "report_canonical.md")) stop("Select an authored report, not the generated project protocol.")
  if (is.null(report_id)) report_id <- paste0("report-", import_hash(report))
  nlss_report_id(report_id, "report ID")
  lock <- nlss_managed_path(root, ".nlss/.analysis-lock")
  if (!dir.create(lock, showWarnings = FALSE)) stop("Managed project is locked; inspect running/interrupted operations before recovery.")
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  evidence <- lapply(ids, function(id) nlss_report_run(project, id))
  capture <- function(relative, kind) {
    document(relative, kind)
    item <- nlss_store_file(root, relative, max_bytes = nlss_report_limit(kind))
    if (!isTRUE(item$bytes > 0)) stop("Selected document became empty during capture.")
    item
  }
  captured <- c(list(selected_path = report), capture(report, "report"))
  payload <- list(schema_version = 1L, kind = "semantic_report_revision", workspace_id = project$workspace_id,
    report_id = report_id, report = captured, runs = evidence,
    verification = nlss_report_claim())
  revision <- nlss_report_revision(payload)
  relative <- nlss_report_record_path(report_id, revision)
  destination <- nlss_managed_path(root, relative)
  record <- c(payload, list(revision_id = revision, finalized_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
                           finalizer_code_sha256 = nlss_code_hash()))
  publication_lock <- nlss_managed_path(root, ".nlss/.publication-lock")
  if (!dir.create(publication_lock, showWarnings = FALSE)) stop("Managed project publication is locked; inspect before recovery.")
  on.exit(unlink(publication_lock, recursive = TRUE), add = TRUE)
  # Locks coordinate NLSS writers, not the user's editor. Verify immediately
  # before publishing, without ever replacing an authored visible document.
  check_current <- function() {
    for (i in seq_along(ids)) {
      for (key in c("request", "result")) {
        ref <- evidence[[i]][[key]]
        if (!identical(import_hash(nlss_managed_path(root, ref$path, file = TRUE), file = TRUE), ref$sha256)) stop("Run evidence changed during report save.")
      }
    }
    for (item in list(captured)) {
      path <- nlss_managed_path(root, item$selected_path, file = TRUE, visible = TRUE)
      if (!identical(import_hash(path, file = TRUE), item$sha256)) stop("Selected document changed during report save; retry after review.")
      nlss_report_object(root, item)
    }
    if (!identical(import_hash(nlss_managed_path(root, project$marker, file = TRUE), file = TRUE), project$marker_sha256)) stop("Project marker changed during report save.")
  }
  status <- "already_saved"
  if (nlss_project_exists(destination)) {
    existing <- nlss_report_verify(project, report_id, revision, verify = FALSE)
    if (!identical(import_json(nlss_report_payload(existing)), import_json(payload))) stop("Existing report revision differs; it was not overwritten.")
    check_current()
  } else {
    directory <- nlss_managed_path(root, paste0(".nlss/reports/", report_id))
    if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE, showWarnings = FALSE)) stop("Could not create report revision directory.")
    staged <- tempfile(".pending-report-", tmpdir = nlss_managed_path(root, ".nlss/staging"), fileext = ".json")
    write_import_json(record, staged)
    if (file.info(staged)$size > nlss_report_limit("evidence")) stop("Report revision exceeds the configured evidence read limit; pending record retained, not published.")
    check_current()
    # A failed publication leaves this clearly pending small record for explicit
    # inspection; no success log, duplicate report file, or age-based cleanup.
    destination <- nlss_managed_path(root, relative)
    if (nlss_project_exists(destination) || !file.rename(staged, destination)) stop("Could not publish report revision; pending evidence retained for inspection.")
    status <- "saved"
  }
  list(status = status, report_id = report_id, revision_id = revision, record_path = relative,
       report = captured, verification = payload$verification)
}
