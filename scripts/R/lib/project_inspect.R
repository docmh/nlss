# SPDX-License-Identifier: Apache-2.0
# Read-only Phase 3 boundary. Never call the legacy loader/snapshot/publisher.

nlss_project_string <- function(x, field, optional = FALSE) {
  if (is.null(x) && optional) return(NULL)
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop(field, " must be one nonempty string.", call. = FALSE)
  }
  x
}

nlss_project_exists <- function(path) {
  link <- Sys.readlink(path)
  file.exists(path) || (!is.na(link) && nzchar(link))
}

nlss_project_regular_file <- function(path) {
  if (!utils::file_test("-f", path)) return(FALSE)
  # R's file_test('-f') only excludes directories, not Unix FIFOs/devices.
  # Do not open a special file just to inspect a user-controlled reference.
  if (.Platform$OS.type != "unix") return(TRUE)
  test <- Sys.which("test")
  if (!nzchar(test)) stop("Read-only inspection needs the Unix 'test' utility for file-type checks.", call. = FALSE)
  identical(suppressWarnings(system2(test, c("-f", shQuote(path)), stdout = FALSE, stderr = FALSE)), 0L)
}

nlss_locate_project <- function(project = NULL, start = getwd(), required = TRUE) {
  marker <- get_workspace_manifest_name()
  if (!is.null(project)) {
    project <- normalize_input_path(nlss_project_string(project, "--project"))
    if (!dir.exists(project) && basename(project) != marker) {
      stop("--project must name an existing project directory or its workspace marker.", call. = FALSE)
    }
    candidate <- if (dir.exists(project)) file.path(project, marker) else project
    if (!nlss_project_exists(candidate)) stop("Selected project has no workspace marker.", call. = FALSE)
  } else {
    directory <- normalizePath(start, winslash = "/", mustWork = TRUE)
    if (!dir.exists(directory)) stop("Project discovery must start in a directory.", call. = FALSE)
    repeat {
      candidate <- file.path(directory, marker)
      if (nlss_project_exists(candidate)) break
      parent <- dirname(directory)
      if (identical(parent, directory)) {
        if (!required) return(NULL)
        stop("No project marker in this directory or its ancestors. Use --project.", call. = FALSE)
      }
      directory <- parent
    }
  }
  # A marker may not redirect discovery into another directory via a symlink.
  link <- Sys.readlink(candidate)
  if ((!is.na(link) && nzchar(link)) || !nlss_project_regular_file(candidate)) {
    stop("Project marker must be a regular file, not a directory or symlink.", call. = FALSE)
  }
  normalizePath(candidate, winslash = "/", mustWork = TRUE)
}

nlss_project_path <- function(root, path) {
  path <- nlss_project_string(path, "Registered path", optional = TRUE)
  if (is.null(path)) return(list(path = NULL, status = "unrecorded", absolute = NULL))
  path <- normalize_input_path(path)
  if (root != "/") root <- sub("/+$", "", root)
  prefix <- if (root == "/") root else paste0(root, "/")
  if (is_absolute_path(path)) {
    if (!startsWith(path, prefix)) {
      return(list(path = mask_external_log_path(path), status = "external_not_inspected", absolute = NULL))
    }
    path <- substring(path, nchar(prefix) + 1L)
  }
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  if (!length(parts) || any(parts %in% c("..", ""))) {
    return(list(path = path, status = "unsafe_path", absolute = NULL))
  }
  parts <- parts[parts != "."]
  current <- root
  for (i in seq_along(parts)) {
    current <- if (current == "/") paste0(current, parts[i]) else file.path(current, parts[i])
    link <- Sys.readlink(current)
    if (!is.na(link) && nzchar(link)) return(list(path = path, status = "symlink_not_inspected", absolute = NULL))
    if (i < length(parts) && nlss_project_exists(file.path(current, get_workspace_manifest_name()))) {
      return(list(path = path, status = "other_project_not_inspected", absolute = NULL))
    }
  }
  list(path = if (length(parts)) paste(parts, collapse = "/") else ".",
       status = if (file.exists(current)) "present" else "missing", absolute = current)
}

nlss_inspect_file <- function(root, path, expected = NULL, verify = FALSE, max_bytes = Inf) {
  target <- nlss_project_path(root, path)
  result <- target[c("path", "status")]
  result$bytes <- NULL
  result$sha256 <- NULL
  result$comparison <- "not_checked"
  if (target$status != "present") return(result)
  info <- file.info(target$absolute)
  if (!nlss_project_regular_file(target$absolute)) { result$status <- "not_a_file"; return(result) }
  result$bytes <- as.numeric(info$size)
  if (is.na(info$size) || file.access(target$absolute, 4L) != 0L) {
    result$status <- "unreadable"; return(result)
  }
  if (isTRUE(verify)) {
    if (info$size > max_bytes) { result$status <- "too_large_to_hash"; return(result) }
    result$sha256 <- tryCatch(import_hash(target$absolute, file = TRUE), error = function(e) NULL)
    after <- file.info(target$absolute)
    if (is.null(result$sha256)) {
      result$status <- "unreadable"
    } else if (!identical(info[c("size", "mtime", "ctime")], after[c("size", "mtime", "ctime")])) {
      result$status <- "changed_during_inspection"; result$sha256 <- NULL
    } else if (is.null(expected)) {
      result$comparison <- "no_recorded_hash"
    } else if (!is.character(expected) || length(expected) != 1L || is.na(expected) || !grepl("^[0-9a-f]{64}$", expected)) {
      result$comparison <- "invalid_recorded_hash"
    } else {
      result$comparison <- if (identical(result$sha256, expected)) "matches_recorded" else "differs_from_recorded"
    }
  }
  result
}

nlss_project_metadata <- function(root, path, yaml = FALSE) {
  target <- nlss_project_path(root, path)
  if (target$status != "present") return(list(status = target$status, value = NULL))
  if (!nlss_project_regular_file(target$absolute)) return(list(status = "not_a_file", value = NULL))
  size <- file.info(target$absolute)$size
  if (is.na(size) || size > 2 * 1024^2) return(list(status = "metadata_too_large", value = NULL))
  value <- tryCatch({
    if (yaml) yaml::read_yaml(target$absolute, eval.expr = FALSE) else jsonlite::read_json(target$absolute, simplifyVector = FALSE)
  }, error = function(e) NULL)
  if (!is.list(value) || is.null(names(value)) || anyDuplicated(names(value))) {
    return(list(status = "invalid_metadata", value = NULL))
  }
  list(status = "read", value = value)
}

nlss_project_dataset_entries <- function(datasets) {
  if (is.null(datasets) || identical(datasets, list())) return(list())
  if (!is.list(datasets)) stop("Manifest datasets must be a list or mapping.", call. = FALSE)
  if (is.character(datasets$name)) datasets <- list(datasets)
  entries <- lapply(seq_along(datasets), function(i) {
    entry <- datasets[[i]]
    if (!is.list(entry)) stop("Each manifest dataset must be a mapping.", call. = FALSE)
    if (is.null(entry$name) && !is.null(names(datasets))) entry$name <- names(datasets)[i]
    nlss_project_string(entry$name, "Dataset name")
    entry
  })
  if (anyDuplicated(vapply(entries, `[[`, "", "name"))) stop("Duplicate dataset names in manifest.", call. = FALSE)
  entries
}

nlss_inspect_directory <- function(root, path) {
  target <- nlss_project_path(root, path)
  result <- list(path = target$path, status = target$status, children = character())
  if (target$status != "present") return(result)
  if (!dir.exists(target$absolute)) { result$status <- "not_a_directory"; return(result) }
  if (nlss_project_exists(file.path(target$absolute, get_workspace_manifest_name()))) {
    result$status <- "other_project_not_inspected"; return(result)
  }
  result$children <- sort(list.files(target$absolute, all.files = TRUE, no.. = TRUE))
  result
}

nlss_inspect_run_directory <- function(root, path, workspace_id) {
  directory <- nlss_inspect_directory(root, path)
  names <- directory$children[!startsWith(directory$children, ".") | startsWith(directory$children, ".pending-")]
  utility <- identical(path, ".nlss/utility-runs")
  entries <- lapply(names, function(id) {
    prefix <- paste0(path, "/", id, "/")
    request <- nlss_project_metadata(root, paste0(prefix, "request.json"))
    result <- nlss_project_metadata(root, paste0(prefix, "result.json"))
    item <- list(run_id = id, path = sub("/$", "", prefix), status = "incomplete",
      recorded_status = NULL, module = NULL, dataset = NULL, input_kind = NULL,
      request = list(path = paste0(prefix, "request.json"), status = request$status),
      result = list(path = paste0(prefix, "result.json"), status = result$status),
      output = nlss_inspect_file(root, paste0(prefix, "output.md")), evidence_verification = "not_checked")
    if (startsWith(id, ".pending-")) { item$status <- "pending"; return(item) }
    if (request$status != "read" || result$status != "read") return(item)
    tryCatch({
      req <- request$value; res <- result$value
      if (!identical(req$run_id, id) || !identical(res$run_id, id) ||
          !identical(req$module, res$module) || !identical(req$schema_version, res$schema_version) ||
          (!is.null(req$project) && !identical(req$project$workspace_id, workspace_id))) stop("identity_mismatch")
      if (!is.numeric(req$schema_version) || length(req$schema_version) != 1L ||
          !isTRUE(req$schema_version %in% c(1, 2))) stop("unsupported_record")
      if (utility) {
        if (!identical(req$kind, "utility") || !identical(res$kind, "utility") ||
            !isTRUE(req$schema_version == 1)) stop("unsupported_record")
      } else {
        if (!identical(req$dataset, res$dataset) || !identical(req$input, res$input) ||
            (req$schema_version == 1 && is.null(req$dataset)) ||
            (req$schema_version == 2 && (!is.null(req$dataset) || !identical(req$input$kind, "parameters")))) stop("unsupported_record")
      }
      item$module <- nlss_project_string(req$module, "Procedure")
      item$recorded_status <- nlss_project_string(res$status, "Recorded status")
      if (!item$recorded_status %in% c("completed", "failed", "partial", "pending")) stop("unsupported_record")
      item$input_kind <- if (utility) "utility" else if (is.null(req$dataset)) "parameters" else "dataset"
      reference <- if (utility) req$request$dataset else req$dataset
      if (!is.null(reference)) item$dataset <- list(
        dataset_id = nlss_project_string(reference$dataset_id, "Recorded dataset ID"),
        version_id = nlss_project_string(reference$version_id, "Recorded input version"))
      if (item$recorded_status != "completed") {
        for (name in c("partial-output.md", "diagnostic-output.md")) {
          file <- nlss_inspect_file(root, paste0(prefix, name))
          if (file$status == "present") { item$output <- file; break }
        }
      }
      item$status <- if (item$output$status == "present") "listed" else "missing_output"
      item
    }, error = function(e) { item$status <- "invalid_metadata"; item })
  })
  list(path = directory$path, status = directory$status, entries = entries,
       published_candidates = sum(!startsWith(names, ".")), pending = sum(startsWith(names, ".pending-")),
       verification = "not_verified")
}

nlss_inspect_reports <- function(root, workspace_id) {
  directory <- nlss_inspect_directory(root, ".nlss/reports")
  entries <- list()
  for (id in directory$children[!startsWith(directory$children, ".")]) {
    revisions <- nlss_inspect_directory(root, paste0(".nlss/reports/", id))
    if (revisions$status != "present" || !length(revisions$children)) {
      entries <- c(entries, list(list(report_id = id, path = revisions$path,
        status = if (revisions$status == "present") "empty" else revisions$status)))
      next
    }
    for (name in revisions$children[endsWith(revisions$children, ".json")]) {
      path <- paste0(revisions$path, "/", name)
      read <- nlss_project_metadata(root, path)
      item <- list(report_id = id, revision_id = sub("[.]json$", "", name), path = path,
        status = read$status, evidence_verification = "not_checked")
      if (read$status == "read") item <- tryCatch({
        record <- read$value
        if (!is.numeric(record$schema_version) || !isTRUE(record$schema_version == 1) ||
            !identical(record$kind, "semantic_report_revision") ||
            !identical(record$workspace_id, workspace_id) || !identical(record$report_id, id) ||
            !identical(record$revision_id, item$revision_id)) stop("Invalid report identity")
        item$saved_at <- nlss_project_string(record$finalized_at, "Saved timestamp")
        item$visible_report <- nlss_inspect_file(root, record$report$selected_path)
        # Check only the declared object location/existence here, never its bytes
        # or the report's dependency graph. Full verification is an explicit action.
        hash <- nlss_project_string(record$report$sha256, "Report hash")
        if (!grepl("^[a-f0-9]{64}$", hash) ||
            !identical(record$report$path, paste0(".nlss/objects/", hash))) stop("Invalid report object reference")
        item$saved_report <- nlss_inspect_file(root, record$report$path)
        item$status <- if (item$saved_report$status == "present") "listed" else "missing_report"
        item
      }, error = function(e) { item$status <- "invalid_metadata"; item })
      entries <- c(entries, list(item))
    }
  }
  list(path = directory$path, status = directory$status, entries = entries, verification = "not_verified")
}

nlss_inspect_managed_dataset <- function(root, entry, workspace_id, verify) {
  id <- nlss_project_string(entry$id, "Managed dataset ID")
  if (!grepl("^ds-[a-zA-Z0-9]+$", id)) stop("Invalid managed dataset ID.", call. = FALSE)
  path <- paste0(".nlss/datasets/", id, "/dataset.json")
  descriptor <- nlss_project_metadata(root, path)
  record <- descriptor$value
  mapping <- function(value) is.list(value) && !is.null(names(value)) && !anyDuplicated(names(value))
  string <- function(value) is.character(value) && length(value) == 1L && !is.na(value) && nzchar(trimws(value))
  if (descriptor$status == "read") {
    valid <- is.numeric(record$schema_version) && length(record$schema_version) == 1L &&
      !is.na(record$schema_version) && record$schema_version == 1 &&
      string(record$dataset_id) && string(record$workspace_id) && string(record$working) &&
      mapping(record$source) && mapping(record$initial_version)
    if (valid) {
      valid <- all(vapply(record$source[c("selected_path", "path")], string, logical(1))) &&
        all(vapply(record$initial_version[c("dataset_id", "version_id", "snapshot_path", "dictionary_path")], string, logical(1)))
    }
    if (!valid) descriptor$status <- "invalid_metadata"
    else if (!identical(record$dataset_id, id) || !identical(record$workspace_id, workspace_id) ||
             !identical(record$initial_version$dataset_id, id)) descriptor$status <- "identity_mismatch"
  }
  # An invalid/unavailable descriptor remains a visible registration problem;
  # its child references are not authority to inspect additional files.
  if (descriptor$status != "read") record <- NULL
  reference <- record$initial_version
  source <- record$source
  list(name = entry$name, dataset_id = id,
       descriptor = list(path = path, status = descriptor$status),
       recorded_version_id = reference$version_id,
       reference_basis = "initial_registered_version_not_latest",
       working = nlss_inspect_file(root, record$working, reference$data_sha256, verify),
       snapshot = nlss_inspect_file(root, reference$snapshot_path, reference$data_sha256, verify),
       dictionary = nlss_inspect_file(root, reference$dictionary_path, reference$dictionary_sha256, verify),
       registered_source = nlss_inspect_file(root, source$selected_path, source$sha256, verify),
       preserved_source = nlss_inspect_file(root, source$path, source$sha256, verify),
       import_binding = NULL)
}

nlss_inspect_project <- function(project = NULL, verify = FALSE, start = getwd()) {
  verify <- parse_bool(verify)
  for (package in c("yaml", "jsonlite", "digest")) {
    if (!requireNamespace(package, quietly = TRUE)) stop("Project inspection requires the R package ", package, ".", call. = FALSE)
  }
  manifest_path <- nlss_locate_project(project, start)
  root <- dirname(manifest_path)
  manifest <- nlss_project_metadata(root, basename(manifest_path), yaml = TRUE)
  if (manifest$status != "read") stop("Cannot inspect workspace marker: ", manifest$status, call. = FALSE)
  manifest <- manifest$value
  if (!is.numeric(manifest$schema_version) || !isTRUE(manifest$schema_version == 2) ||
      !identical(manifest$storage, "managed_parquet_v1")) {
    stop("Unsupported project marker; expected current schema 2 / managed_parquet_v1. No conversion is performed.", call. = FALSE)
  }
  workspace_id <- nlss_project_string(manifest$workspace_id, "Workspace ID")
  datasets <- nlss_project_dataset_entries(manifest$datasets)
  if (anyDuplicated(vapply(datasets, function(entry) nlss_project_string(entry$id, "Managed dataset ID"), ""))) {
    stop("Duplicate managed dataset IDs in manifest.", call. = FALSE)
  }
  active <- nlss_project_string(manifest$active_dataset, "Active dataset", optional = TRUE)
  list(schema_version = 1L, kind = "project_inspection", read_only = TRUE,
       project_root = root, manifest = basename(manifest_path),
       workspace_id = workspace_id,
       layout = "managed_parquet_v1", active_dataset = active,
       active_dataset_registered = is.null(active) || active %in% vapply(datasets, `[[`, "", "name"),
       verification_scope = if (verify) "referenced_file_hashes_only" else "paths_and_sizes_only",
       protocol = nlss_inspect_file(root, "report_canonical.md"),
       datasets = lapply(datasets, function(entry) nlss_inspect_managed_dataset(root, entry, workspace_id, verify)),
       project_runs = nlss_inspect_run_directory(root, ".nlss/runs", workspace_id),
       project_utilities = nlss_inspect_run_directory(root, ".nlss/utility-runs", workspace_id),
       reports = nlss_inspect_reports(root, workspace_id),
       limitations = c("Saved statuses and input versions are recorded metadata, not integrity or replay verdicts.",
         "Optional verification checks registered dataset references only, not run/report dependencies or protocol freshness.",
         "Working hashes are compared with the initial registered version, not the latest run; differences do not prove an external edit.",
         "No files, registrations, analysis logs or historical records are created or changed."))
}

nlss_inspection_markdown <- function(result) {
  safe <- function(x) if (is.null(x)) "unrecorded" else gsub("[[:cntrl:]\\x60|<>\\[\\]]", " ", as.character(x), perl = TRUE)
  link <- function(file, label = file$path) {
    if (is.null(file)) return("unrecorded")
    # Stdout has no stable document directory: use the selected root, not CWD.
    # URI encoding also keeps filenames containing spaces/#/parentheses intact.
    if (!file$status %in% c("present", "read")) return(paste0(safe(label), " (", safe(file$status), ")"))
    target <- paste(vapply(strsplit(paste0(result$project_root, "/", file$path), "/", fixed = TRUE)[[1]],
      utils::URLencode, "", reserved = TRUE), collapse = "/")
    paste0("[", safe(label), "](<", target, ">)")
  }
  file_line <- function(label, file) paste0("- ", label, ": ", link(file), " — ", file$status,
    if (file$comparison != "not_checked") paste0("; ", file$comparison) else "")
  input_label <- function(run) {
    if (is.null(run$dataset)) return(safe(run$input_kind))
    registered <- Filter(function(x) identical(x$dataset_id, run$dataset$dataset_id), result$datasets)
    name <- if (length(registered)) registered[[1]]$name else run$dataset$dataset_id
    paste(safe(name), safe(run$dataset$version_id), sep = " / ")
  }
  lines <- c("# NLSS project inspection", "", paste0("Project: ", safe(result$project_root)), "",
    paste0("## ", link(result$protocol, "Open analysis protocol")), "",
    "The automatically maintained, SPSS-like evidence view. Authored research reports remain separate.", "",
    paste0("Active dataset: ", safe(result$active_dataset),
      if (!result$active_dataset_registered) " (not registered)" else ""),
    paste0("Verification: ", result$verification_scope, "; saved evidence not checked."))
  for (dataset in result$datasets) lines <- c(lines, "", paste0("## Data: ", safe(dataset$name)), "",
    file_line("Working data (editable)", dataset$working),
    file_line("Original source", dataset$registered_source),
    file_line("Preserved source", dataset$preserved_source),
    paste0("- Initial registered version: ", safe(dataset$recorded_version_id), " (not necessarily latest)."),
    paste0("- Descriptor: ", link(dataset$descriptor)),
    file_line("Initial snapshot", dataset$snapshot), file_line("Initial dictionary", dataset$dictionary))
  for (key in c("project_runs", "project_utilities")) {
    directory <- result[[key]]
    lines <- c(lines, "", if (key == "project_runs") "## Analyses" else "## Utilities", "",
      paste0("Storage: ", safe(directory$status), "; recorded status only, evidence not verified."), "")
    if (!length(directory$entries)) { lines <- c(lines, "No saved records found."); next }
    lines <- c(lines, "| Procedure | Input dataset / version | Recorded status / availability | Output and evidence |",
      "| --- | --- | --- | --- |")
    for (run in directory$entries) lines <- c(lines, paste0("| ", safe(run$module), " | ",
      input_label(run),
      " | ", safe(run$recorded_status), " / ", safe(run$status), " | ",
      link(run$output, "Output"), " · ", link(run$request, "Request"), " · ", link(run$result, "Result"), " |"))
  }
  lines <- c(lines, "", "## Authored report revisions", "",
    paste0("Storage: ", safe(result$reports$status), "; evidence not checked."), "")
  if (!length(result$reports$entries)) lines <- c(lines, "No saved report revisions found.")
  else {
    lines <- c(lines, "| Report | Saved at (UTC) | Availability | Preserved text / record |", "| --- | --- | --- | --- |")
    for (report in result$reports$entries) lines <- c(lines, paste0("| ",
      link(report$visible_report, if (is.null(report$visible_report)) report$report_id else report$visible_report$path), " | ",
      safe(report$saved_at), " | ", safe(report$status), " | ", link(report$saved_report, "Saved Markdown"),
      " · ", link(list(path = report$path, status = if (report$status %in% c("listed", "missing_report")) "read" else report$status), "Revision record"), " |"))
  }
  c(lines, "", "## Boundaries", "", paste0("- ", result$limitations))
}
