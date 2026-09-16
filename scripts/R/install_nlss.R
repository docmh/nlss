#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Standalone skill maintenance only. Base R + existing CLI; no project/bootstrap.
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))[1]
script <- gsub("~+~", " ", script, fixed = TRUE)
lib <- file.path(dirname(normalizePath(script, winslash = "/", mustWork = TRUE)), "lib")
source(file.path(lib, "cli.R"))

nlss_install_path <- function(path) {
  if (length(path) != 1L || is.na(path) || !nzchar(path)) stop("Select a nonempty path.")
  path <- path.expand(path)
  if (file.exists(path) || dir.exists(path)) return(normalizePath(path, winslash = "/", mustWork = TRUE))
  parent <- dirname(path)
  if (identical(parent, path)) stop("Cannot resolve destination.")
  file.path(nlss_install_path(parent), basename(path))
}

nlss_install_inventory <- function(root) {
  # Do not recurse through links, including dangling links, when owning a tree.
  walk <- function(folder) {
    children <- list.files(folder, all.files = TRUE, no.. = TRUE, full.names = TRUE)
    if (any(nzchar(Sys.readlink(children), keepNA = TRUE), na.rm = TRUE)) stop("Linked files/directories are not managed installations.")
    dirs <- children[dir.exists(children)]
    c(children[!dir.exists(children)], unlist(lapply(dirs, walk), use.names = FALSE))
  }
  sort(substring(walk(root), nchar(root) + 2L))
}

nlss_install_validate <- function(root) {
  if (!dir.exists(root)) stop("Not an unpacked NLSS release: ", root)
  files <- nlss_install_inventory(root)
  required <- c(".nlss-files.tsv", ".nlss-release.dcf", "SKILL.md", "scripts/config.yml", "scripts/R/install_nlss.R")
  if (!all(required %in% files)) stop("Unrecognized installation: select an unpacked NLSS release, not a Git checkout or project.")
  manifest <- read.delim(file.path(root, ".nlss-files.tsv"), sep = "\t", quote = "", comment.char = "", stringsAsFactors = FALSE)
  if (!identical(names(manifest), c("md5", "path")) || !nrow(manifest) || anyNA(manifest) || anyDuplicated(manifest$path) ||
      any(!grepl("^[a-f0-9]{32}$", manifest$md5)) ||
      any(!grepl("^[A-Za-z0-9_. /-]+$", manifest$path)) ||
      any(grepl("(^/|(^|/)\\.\\.?(/|$)|//)", manifest$path))) stop("Invalid release inventory.")
  if (!setequal(files, c(manifest$path, ".nlss-files.tsv")))
    stop("Locally changed/incomplete installation: extra or missing files. Preserve your edits and select another destination.")
  actual <- unname(tools::md5sum(file.path(root, manifest$path)))
  if (anyNA(actual) || !identical(actual, manifest$md5))
    stop("Locally edited or damaged release. Preserve your edits; no replacement/removal was performed.")
  info <- read.dcf(file.path(root, ".nlss-release.dcf"))
  if (nrow(info) != 1L || !all(c("Name", "Version", "Payload-SHA256") %in% colnames(info)) ||
      info[1, "Name"] != "nlss" || !grepl("^[a-f0-9]{64}$", info[1, "Payload-SHA256"])) stop("Invalid NLSS release identity.")
  config_version <- grep("^nlss_version: ", readLines(file.path(root, "scripts/config.yml"), warn = FALSE), value = TRUE)
  if (!identical(config_version, paste0('nlss_version: "', info[1, "Version"], '"'))) stop("Release version mismatch.")
  list(version = info[1, "Version"], payload = info[1, "Payload-SHA256"], files = files)
}

nlss_install_target <- function(target) {
  if (isTRUE(nzchar(Sys.readlink(path.expand(target)), keepNA = TRUE))) stop("Select a real installation directory, not a symlink.")
  target <- nlss_install_path(target)
  if (basename(target) != "nlss" || target %in% c(nlss_install_path("~"), nlss_install_path(getwd())))
    stop("Destination must be the specific nlss skill folder, not home or the current working directory.")
  ancestor <- target
  repeat {
    if (any(file.exists(file.path(ancestor, c("nlss-workspace.yml", "plugin.json", ".codex-plugin/plugin.json", ".claude-plugin/plugin.json")))))
      stop("Do not use the standalone helper inside a research project or managed plugin; use the native plugin manager.")
    parent <- dirname(ancestor)
    if (identical(parent, ancestor)) break
    ancestor <- parent
  }
  target
}

nlss_install_main <- function() {
  opts <- parse_args(commandArgs(TRUE), module = NULL,
    allowed = c("help", "action", "source", "harness", "destination", "approve"), boolean = c("help", "approve"))
  if (parse_bool(opts$help)) {
    cat("Standalone NLSS skill installation (base R only; no downloads or R-package changes).\n",
      "install_nlss.R --action install|update|remove|status [--source UNPACKED_NLSS]\n",
      "  --harness codex|vibe|claude OR --destination EXACT_NLSS_FOLDER [--approve]\n",
      "Without --approve: validate and show the proposed action without writing.\n",
      "Use native managers for plugins. Restart the harness after a change.\n", sep = "")
    return(invisible(NULL))
  }
  action <- if (is.null(opts$action)) "status" else opts$action
  if (!action %in% c("install", "update", "remove", "status")) stop("Unknown maintenance action.")
  if (!is.null(opts$destination) && !is.null(opts$harness)) stop("Choose --harness or --destination, not both.")
  if (is.null(opts$destination)) {
    homes <- c(codex = "~/.agents/skills/nlss", vibe = "~/.vibe/skills/nlss", claude = "~/.claude/skills/nlss")
    if (is.null(opts$harness) || !opts$harness %in% names(homes)) stop("Select --harness codex|vibe|claude or --destination.")
    opts$destination <- homes[[opts$harness]]
  }
  target <- nlss_install_target(opts$destination)
  exists <- file.exists(target) || dir.exists(target)
  old <- if (exists) nlss_install_validate(target) else NULL
  if (action == "status") {
    cat(if (exists) paste("Installed NLSS", old$version) else "Not installed", "\nPath:", target, "\n")
    return(invisible(NULL))
  }
  if (action == "remove" && !exists) { cat("Already absent:", target, "\n"); return(invisible(NULL)) }
  if (action %in% c("install", "update")) {
    if (is.null(opts$source)) stop("Select --source: the already unpacked nlss skill directory.")
    if (isTRUE(nzchar(Sys.readlink(path.expand(opts$source)), keepNA = TRUE))) stop("Source must be an unpacked directory, not a symlink.")
    source <- nlss_install_path(opts$source)
    if (source == target || startsWith(target, paste0(source, "/")) || startsWith(source, paste0(target, "/")))
      stop("Source and destination must be separate, non-overlapping directories.")
    new <- nlss_install_validate(source)
    if (exists && identical(old$payload, new$payload) && identical(old$version, new$version)) {
      cat("Already installed NLSS", old$version, "at", target, "(unchanged).\n")
      return(invisible(NULL))
    }
    if (action == "install" && exists) stop("NLSS already exists there. Select --action update after reviewing the replacement.")
    if (action == "update" && !exists) stop("No recognized installation to update; use --action install.")
  }
  cat("Action:", action, "\nPath:", target, "\n")
  if (!is.null(old)) cat("Installed version:", old$version, "\n")
  if (action != "remove") cat("Selected version:", new$version, "\nSource:", source, "\n")
  if (!parse_bool(opts$approve)) {
    cat("No changes. Review the action, then repeat with --approve after user approval.\n")
    return(invisible(NULL))
  }
  parent <- dirname(target)
  if (!dir.exists(parent) && !dir.create(parent, recursive = TRUE)) stop("Cannot create installation parent.")
  stage <- tempfile(".nlss-stage-", tmpdir = parent)
  previous <- tempfile(".nlss-previous-", tmpdir = parent)
  on.exit(if (dir.exists(stage)) unlink(stage, recursive = TRUE), add = TRUE)
  if (action != "remove") {
    if (!dir.create(stage)) stop("Cannot create staging directory.")
    for (name in new$files) {
      dest <- file.path(stage, name)
      dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
      if (!file.copy(file.path(source, name), dest, copy.mode = TRUE)) stop("Staging failed; previous installation preserved.")
    }
    staged <- nlss_install_validate(stage)
    if (!identical(staged$payload, new$payload)) stop("Staged identity differs; previous installation preserved.")
  }
  # Recheck immediately before replacement; keep one temporary rollback only.
  if (exists) {
    nlss_install_validate(target)
    if (!file.rename(target, previous)) stop("Cannot move old installation; it remains unchanged.")
  }
  if (action != "remove" && !file.rename(stage, target)) {
    restored <- !exists || file.rename(previous, target)
    stop(if (restored) "Activation failed; previous installation preserved." else paste("Activation failed. Previous installation retained at", previous))
  }
  if (exists && unlink(previous, recursive = TRUE) != 0L)
    warning("Operation completed; temporary old installation could not be removed: ", previous)
  if (action == "remove") cat("Removed only NLSS at", target, "; reinstall a selected release to recover it.\n") else
    cat("Installed NLSS", new$version, "at", target, "\nRestart/start a new harness session and ask to use NLSS.\n")
  invisible(NULL)
}

if (sys.nframe() == 0L) {
  status <- tryCatch({ nlss_install_main(); 0L }, error = function(e) { message(conditionMessage(e)); 1L })
  quit(save = "no", status = status)
}
