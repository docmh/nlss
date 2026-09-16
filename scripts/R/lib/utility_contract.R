# SPDX-License-Identifier: Apache-2.0
# Audit publication for calculations, retrieval and lifecycle events. This is
# deliberately separate from statistical input schemas and replay dispatch.
nlss_utility_check_directory <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) stop("Utility output needs one directory path.")
  candidate <- path.expand(path)
  if (!is_absolute_path(candidate)) candidate <- file.path(getwd(), candidate)
  repeat {
    link <- Sys.readlink(candidate)
    if (!is.na(link) && nzchar(link)) stop("Utility output directory must not traverse symlinks.")
    parent <- dirname(candidate)
    if (identical(parent, candidate)) break
    candidate <- parent
  }
  invisible(path)
}

nlss_publish_utility <- function(module, out_dir, request, results, output,
                                 publish, artifacts = list(), targets = NULL, status = "completed", workspace_root = NULL) {
  if (!module %in% c("calc", "research_academia", "metaskill_runner", "init_workspace")) stop("Unsupported utility audit.")
  if (length(status) != 1L || !status %in% c("completed", "partial", "failed")) stop("Invalid utility status.")
  nlss_utility_check_directory(out_dir)
  manifest <- find_workspace_manifest(out_dir)
  out_dir <- normalizePath(ensure_out_dir(out_dir), winslash = "/", mustWork = TRUE)
  root <- if (nzchar(manifest)) dirname(manifest) else out_dir
  if (!is.null(workspace_root)) {
    nlss_utility_check_directory(workspace_root)
    root <- normalizePath(workspace_root, winslash = "/", mustWork = TRUE)
    if (nzchar(manifest) && !identical(root, dirname(manifest))) stop("Utility project root conflicts with its manifest.")
  }
  if (!identical(root, out_dir) && !startsWith(out_dir, paste0(root, "/"))) stop("Utility output must be inside its project.")
  project_route <- nzchar(manifest)
  if (project_route) {
    source_lib("project_store.R")
    out_dir <- ensure_out_dir(nlss_resolve_locations(project = root, use_dataset = FALSE)$output_root)
  }
  safe_target <- function(path) {
    link <- Sys.readlink(path)
    resolved <- normalizePath(path, winslash = "/", mustWork = FALSE)
    if ((!is.na(link) && nzchar(link)) || !identical(resolved, path) ||
        !startsWith(path, paste0(root, "/")) || dir.exists(path)) stop("Unsafe utility projection target: ", basename(path))
    path
  }
  if (is.null(targets)) targets <- c(file.path(out_dir, c("report_canonical.md", "analysis_log.jsonl")),
                                    if (nzchar(manifest)) manifest)
  if (project_route) targets <- c(targets[!basename(targets) %in% c("report_canonical.md", "analysis_log.jsonl")],
                                  nlss_managed_path(root, "report_canonical.md"))
  targets <- unique(unname(vapply(targets, safe_target, character(1))))
  runs <- file.path(out_dir, "utility-runs")
  runs_link <- Sys.readlink(runs)
  if ((!is.na(runs_link) && nzchar(runs_link)) || (file.exists(runs) && !dir.exists(runs))) stop("Unsafe utility-runs directory.")
  if (!dir.exists(runs) && !dir.create(runs)) stop("Could not create utility audit directory.")
  if (!identical(normalizePath(runs, winslash = "/", mustWork = TRUE), runs)) stop("Utility audit directory must not be a symlink.")
  staging <- tempfile(paste0(".pending-", module, "-", format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC"), "-"), tmpdir = runs)
  if (!dir.create(staging)) stop("Could not stage utility audit.")
  id <- sub("^[.]pending-", "", basename(staging))
  destination <- file.path(runs, id)
  request <- list(schema_version = 1L, kind = "utility", run_id = id, module = module,
    timestamp_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    replay = list(eligible = FALSE, reason = "Utility evidence is not a statistical replay request."),
    request = request, code_sha256 = nlss_code_hash(), environment = nlss_execution_environment())
  write_import_json(request, file.path(staging, "request.json"))
  if (!is.character(output) || anyNA(output)) stop("Utility output must be text.")
  if (length(artifacts) && (is.null(names(artifacts)) || anyDuplicated(names(artifacts)) ||
      any(!grepl("^[A-Za-z][A-Za-z0-9_.-]*$", names(artifacts))) ||
      any(names(artifacts) %in% c("request.json", "result.json", "output.md", "partial-output.md", "diagnostic-output.md")))) stop("Invalid utility artifact names.")
  output_name <- if (identical(status, "failed")) "diagnostic-output.md" else "output.md"
  files <- c(setNames(list(paste(output, collapse = "\n")), output_name), artifacts)
  for (name in names(files)) {
    value <- files[[name]]
    if (is.character(value) && !anyNA(value)) value <- charToRaw(enc2utf8(paste(value, collapse = "\n")))
    if (!is.raw(value)) stop("Utility artifacts must be raw bytes or text.")
    writeBin(value, file.path(staging, name))
  }
  names_saved <- c("request.json", names(files))
  registered <- setNames(lapply(names_saved, function(name) list(path = name,
    sha256 = import_hash(file.path(staging, name), file = TRUE))), names_saved)
  result <- list(schema_version = 1L, kind = "utility", run_id = id, module = module,
    status = "pending", results = results, artifacts = registered)
  write_import_json(result, file.path(staging, "result.json"))
  lock <- file.path(if (project_route) out_dir else root, ".publication-lock")
  if (!dir.create(lock, showWarnings = FALSE)) stop("Project publication is locked; utility output was not published.")
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  targets <- unname(vapply(targets, safe_target, character(1)))
  existed <- file.exists(targets)
  backup <- tempfile(".projection-backup-", tmpdir = staging)
  if (!dir.create(backup)) stop("Could not protect utility projections.")
  remove_backup <- TRUE
  on.exit(if (remove_backup) unlink(backup, recursive = TRUE), add = TRUE)
  copies <- file.path(backup, seq_along(targets))
  for (i in which(existed)) {
    if (!file.copy(targets[i], copies[i]) || !identical(import_hash(targets[i], file = TRUE), import_hash(copies[i], file = TRUE))) stop("Could not protect utility output.")
  }
  failure <- tryCatch({
    if (project_route) {
      previous <- nlss_run_context$protocol_root
      nlss_run_context$protocol_root <- root
      on.exit({ nlss_run_context$protocol_root <- previous }, add = TRUE)
      result$status <- status
      result$published_at_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
      nlss_append_project_protocol(root, make_relative_path(destination, root), request, result,
        file.path(staging, output_name), publish = function() publish(id, staging))
    } else publish(id, staging)
    for (item in registered) {
      path <- nlss_project_file(item$path, staging)
      if (!identical(path, file.path(staging, item$path)) || !identical(import_hash(path, file = TRUE), item$sha256)) stop("Utility artifact changed during publication.")
    }
    for (target in targets) safe_target(target)
    result$status <- status
    write_import_json(result, file.path(staging, "result.json"))
    if (!file.rename(staging, destination)) stop("Could not publish utility audit bundle.")
    backup <- file.path(destination, basename(backup))
    NULL
  }, error = identity)
  if (!is.null(failure)) {
    restored <- TRUE
    for (i in seq_along(targets)) {
      ok <- tryCatch({
        safe_target(targets[i])
        if (existed[i]) file.copy(copies[i], targets[i], overwrite = TRUE) &&
          identical(import_hash(copies[i], file = TRUE), import_hash(targets[i], file = TRUE))
        else !file.exists(targets[i]) || unlink(targets[i]) == 0L
      }, error = function(e) FALSE)
      restored <- restored && isTRUE(ok)
    }
    remove_backup <- restored
    result$status <- "failed"
    result$results <- NULL
    result$error <- list(message = nlss_mask_prose_paths(conditionMessage(failure), root), projections_restored = restored)
    if (!restored) {
      result$error$recovery_backup <- basename(backup)
      write_import_json(lapply(seq_along(targets), function(i) list(target = make_relative_path(targets[[i]], root),
        existed = existed[[i]], backup_file = if (existed[[i]]) as.character(i) else NULL)), file.path(backup, "targets.json"))
    }
    if (file.exists(file.path(staging, "output.md"))) file.rename(file.path(staging, "output.md"), file.path(staging, "partial-output.md"))
    result$artifacts <- NULL
    write_import_json(result, file.path(staging, "result.json"))
    if (!restored) warning("Utility projection recovery was incomplete; original bytes remain in the pending bundle's recovery_backup directory.")
    stop(failure)
  }
  invisible(list(run_id = id, path = destination, result = result))
}
