resolve_config_value <- function(path, default = NULL) {
  if (exists("get_config_value", mode = "function")) {
    return(get("get_config_value", mode = "function")(path, default = default))
  }
  default
}

get_default_out <- function() {
  manifest_path <- find_workspace_manifest()
  if (nzchar(manifest_path)) return(dirname(manifest_path))
  default_out <- resolve_config_value("defaults.output_dir", "./outputs/tmp")
  if (is.null(default_out) || !nzchar(default_out)) return("./outputs/tmp")
  default_out
}

ensure_out_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  path
}

escape_regex <- function(text) {
  # Use PCRE to avoid TRE brace parsing failures in some R builds.
  gsub("([\\.^$|()\\[\\]{}*+?\\\\])", "\\\\\\1", as.character(text), perl = TRUE)
}

get_workspace_manifest_name <- function() {
  name <- resolve_config_value("defaults.workspace_manifest", "nlss-workspace.yml")
  if (is.null(name) || !nzchar(name)) return("nlss-workspace.yml")
  as.character(name)
}

ensure_yaml <- function() {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Workspace manifests require the 'yaml' package. Install it: install.packages('yaml').")
  }
}

is_absolute_path <- function(path) {
  if (is.null(path) || !nzchar(path)) return(FALSE)
  grepl("^/|^[A-Za-z]:|^\\\\\\\\", as.character(path))
}

is_wsl <- function() {
  distro <- Sys.getenv("WSL_DISTRO_NAME", unset = "")
  if (nzchar(distro)) return(TRUE)
  release <- ""
  if (exists("Sys.uname", mode = "function")) {
    release <- tryCatch(base::Sys.uname()[["release"]], error = function(e) "")
  } else if (exists("Sys.info", mode = "function")) {
    info <- tryCatch(Sys.info(), error = function(e) NULL)
    if (!is.null(info) && !is.na(info["release"])) release <- info["release"]
  }
  if (!is.null(release) && nzchar(release)) {
    return(grepl("microsoft", release, ignore.case = TRUE))
  }
  FALSE
}

is_windows <- function() {
  sysname <- Sys.info()[["sysname"]]
  !is.null(sysname) && tolower(sysname) == "windows"
}

strip_path_quotes <- function(path) {
  if (is.null(path) || !nzchar(path)) return("")
  path <- as.character(path)
  if (nchar(path) < 2) return(path)
  first <- substr(path, 1, 1)
  last <- substr(path, nchar(path), nchar(path))
  if ((first == "\"" && last == "\"") || (first == "'" && last == "'")) {
    return(substr(path, 2, nchar(path) - 1))
  }
  path
}

try_wslpath <- function(path) {
  if (!is_wsl()) return("")
  wslpath <- Sys.which("wslpath")
  if (!nzchar(wslpath)) return("")
  out <- tryCatch(system2(wslpath, c("-a", "-u", path), stdout = TRUE, stderr = TRUE), error = function(e) character(0))
  out <- out[nzchar(out)]
  if (length(out) == 0) return("")
  trimws(out[1])
}

convert_windows_to_wsl_path <- function(path) {
  if (!is_wsl()) return(path)
  converted <- try_wslpath(path)
  if (nzchar(converted)) return(converted)
  path <- gsub("\\\\", "/", path)
  if (grepl("^[A-Za-z]:/", path)) {
    drive <- tolower(substr(path, 1, 1))
    rest <- substr(path, 4, nchar(path))
    return(paste0("/mnt/", drive, "/", rest))
  }
  path
}

normalize_input_path <- function(path) {
  if (is.null(path)) return("")
  path <- as.character(path)
  if (length(path) == 0 || all(is.na(path))) return("")
  path <- path[1]
  if (is.na(path) || !nzchar(path)) return("")
  path <- trimws(path)
  if (!nzchar(path)) return("")
  path <- strip_path_quotes(path)
  path <- enc2utf8(path)
  path <- gsub("\\\\", "/", path)
  if (is_wsl() && (grepl("^[A-Za-z]:/", path) || grepl("^//", path))) {
    path <- convert_windows_to_wsl_path(path)
  }
  path.expand(path)
}

list_manifest_paths <- function(base_dir, recursive = FALSE) {
  if (is.null(base_dir) || !nzchar(base_dir)) return(character(0))
  if (!dir.exists(base_dir)) return(character(0))
  manifest_name <- get_workspace_manifest_name()
  pattern <- paste0("^", escape_regex(manifest_name), "$")
  paths <- list.files(base_dir, pattern = pattern, recursive = recursive, full.names = TRUE)
  if (length(paths) == 0) return(character(0))
  vapply(paths, normalize_path, character(1))
}

make_relative_path <- function(path, base_dir) {
  if (is.null(path) || !nzchar(path)) return("")
  path <- normalize_path(path)
  base_dir <- normalize_path(base_dir)
  if (!nzchar(base_dir)) return(path)
  base_dir <- sub("/+$", "", base_dir)
  prefix <- paste0(base_dir, "/")
  if (startsWith(path, prefix)) return(sub(paste0("^", prefix), "", path))
  path
}

resolve_workspace_root <- function(base_dir = NULL) {
  if (is.null(base_dir) || !nzchar(base_dir)) base_dir <- getwd()
  base_dir <- normalize_dir_path(base_dir)
  if (!nzchar(base_dir)) return("")
  manifest_path <- find_workspace_manifest(base_dir)
  if (!nzchar(manifest_path)) return("")
  dirname(manifest_path)
}

render_output_path <- function(path, base_dir = NULL, workspace_root = NULL) {
  if (is.null(path) || !nzchar(path)) return("")
  path <- normalize_path(path)
  if (!nzchar(path)) return("")
  if (is.null(workspace_root) || !nzchar(workspace_root)) {
    root_base <- base_dir
    if (is.null(root_base) || !nzchar(root_base)) {
      root_base <- dirname(path)
    }
    workspace_root <- resolve_workspace_root(root_base)
  } else {
    workspace_root <- normalize_dir_path(workspace_root)
  }
  if (nzchar(workspace_root) && is_path_within(path, workspace_root)) {
    if (normalize_dir_path(path) == normalize_dir_path(workspace_root)) return(".")
    return(make_relative_path(path, workspace_root))
  }
  path
}

mask_external_log_path <- function(path) {
  if (is.null(path) || !nzchar(path)) return(path)
  text <- as.character(path)
  if (!nzchar(text) || is.na(text)) return(text)
  name <- basename(text)
  if (!nzchar(name) || name %in% c(".", "..")) return("<external>")
  paste0("<external>/", name)
}

render_log_path <- function(path, workspace_root = NULL) {
  if (is.null(path)) return(path)
  if (length(path) == 0) return(path)
  if (is.na(path)) return(path)
  if (!nzchar(path)) return(path)
  text <- as.character(path)
  if (!nzchar(text) || is.na(text)) return(text)
  if (is_absolute_path(text)) {
    rendered <- render_output_path(text, workspace_root = workspace_root)
    if (!is.null(workspace_root) && nzchar(workspace_root) && is_absolute_path(rendered)) {
      return(mask_external_log_path(rendered))
    }
    return(rendered)
  }
  text
}

render_paths_for_log <- function(value, workspace_root = NULL) {
  if (is.null(value)) return(value)
  if (is.data.frame(value)) {
    out <- value
    path_cols <- grepl("(_path|_dir|path|dir)$", names(out), ignore.case = TRUE)
    if (any(path_cols)) {
      for (col in names(out)[path_cols]) {
        if (is.character(out[[col]])) {
          out[[col]] <- vapply(out[[col]], render_log_path, character(1), workspace_root = workspace_root)
        }
      }
    }
    return(out)
  }
  if (is.list(value)) {
    out <- lapply(value, function(item) render_paths_for_log(item, workspace_root = workspace_root))
    if (!is.null(names(value))) names(out) <- names(value)
    return(out)
  }
  if (is.character(value)) {
    return(vapply(value, render_log_path, character(1), workspace_root = workspace_root))
  }
  value
}

resolve_manifest_path <- function(path, base_dir) {
  if (is.null(path) || !nzchar(path)) return("")
  if (is_absolute_path(path)) return(normalize_path(path))
  normalize_path(file.path(base_dir, as.character(path)))
}

normalize_dir_path <- function(path) {
  path <- normalize_path(path)
  if (!nzchar(path)) return("")
  sub("/+$", "", path)
}

is_path_within <- function(path, base_dir) {
  path <- normalize_dir_path(path)
  base_dir <- normalize_dir_path(base_dir)
  if (!nzchar(path) || !nzchar(base_dir)) return(FALSE)
  if (path == base_dir) return(TRUE)
  startsWith(path, paste0(base_dir, "/"))
}

find_workspace_manifest <- function(base_dir = getwd()) {
  manifest_name <- get_workspace_manifest_name()
  base_dir <- normalize_dir_path(base_dir)
  if (!nzchar(base_dir)) return("")

  candidates <- character(0)
  current_path <- file.path(base_dir, manifest_name)
  if (file.exists(current_path)) candidates <- c(candidates, normalize_path(current_path))

  parent_dir <- dirname(base_dir)
  if (nzchar(parent_dir) && parent_dir != base_dir) {
    parent_path <- file.path(parent_dir, manifest_name)
    if (file.exists(parent_path)) candidates <- c(candidates, normalize_path(parent_path))
  }

  child_dirs <- list.dirs(base_dir, full.names = TRUE, recursive = FALSE)
  child_dirs <- child_dirs[child_dirs != base_dir]
  for (child_dir in child_dirs) {
    candidate <- file.path(child_dir, manifest_name)
    if (file.exists(candidate)) candidates <- c(candidates, normalize_path(candidate))
  }
  candidates <- unique(candidates)
  if (length(candidates) == 0) return("")
  if (length(candidates) > 1) {
    stop("Multiple workspace manifests detected. Workspaces must be non-nested and unique per parent directory.")
  }
  validate_workspace_manifest_path(candidates[1])
}

read_workspace_manifest <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  ensure_yaml()
  yaml::yaml.load_file(path)
}

write_workspace_manifest <- function(manifest, path) {
  if (is.null(path) || !nzchar(path)) return("")
  ensure_yaml()
  ensure_out_dir(dirname(path))
  yaml::write_yaml(manifest, path)
  normalize_path(path)
}

normalize_manifest_datasets <- function(datasets) {
  if (is.null(datasets)) return(list())
  if (is.data.frame(datasets)) {
    return(lapply(seq_len(nrow(datasets)), function(i) as.list(datasets[i, , drop = FALSE])))
  }
  if (!is.list(datasets)) return(list())
  if (!is.null(datasets$name) || (length(datasets) > 0 && is.list(datasets[[1]]))) {
    return(datasets)
  }
  if (!is.null(names(datasets)) && any(nzchar(names(datasets)))) {
    out <- list()
    for (name in names(datasets)) {
      entry <- datasets[[name]]
      if (!is.list(entry)) {
        entry <- list(value = entry)
      }
      if (is.null(entry$name) || !nzchar(entry$name)) entry$name <- name
      out[[length(out) + 1]] <- entry
    }
    return(out)
  }
  list()
}

resolve_manifest_log_seq <- function(manifest, dataset_dir, workspace_root) {
  if (is.null(manifest) || is.null(dataset_dir) || !nzchar(dataset_dir)) return(NULL)
  datasets <- normalize_manifest_datasets(manifest$datasets)
  dataset_dir <- normalize_dir_path(dataset_dir)
  for (entry in datasets) {
    entry_dir <- resolve_dataset_dir(entry, workspace_root)
    if (!nzchar(entry_dir)) next
    if (normalize_dir_path(entry_dir) == dataset_dir) {
      value <- suppressWarnings(as.integer(entry$analysis_log_seq))
      if (length(value) == 0) return(NULL)
      value <- value[1]
      if (is.na(value) || value < 0) return(NULL)
      return(value)
    }
  }
  NULL
}

update_manifest_log_seq <- function(manifest_path, dataset_dir, log_seq) {
  if (is.null(manifest_path) || !nzchar(manifest_path)) return(invisible(FALSE))
  if (is.null(log_seq) || is.na(log_seq)) return(invisible(FALSE))
  manifest <- read_workspace_manifest(manifest_path)
  if (is.null(manifest)) return(invisible(FALSE))
  workspace_root <- normalize_dir_path(dirname(manifest_path))
  dataset_dir <- normalize_dir_path(dataset_dir)
  datasets <- normalize_manifest_datasets(manifest$datasets)
  updated <- FALSE
  for (i in seq_along(datasets)) {
    entry <- datasets[[i]]
    entry_dir <- resolve_dataset_dir(entry, workspace_root)
    if (!nzchar(entry_dir)) next
    if (normalize_dir_path(entry_dir) == dataset_dir) {
      entry$analysis_log_seq <- as.integer(log_seq)
      datasets[[i]] <- entry
      updated <- TRUE
      break
    }
  }
  if (updated) {
    manifest$datasets <- datasets
    write_workspace_manifest(manifest, manifest_path)
  }
  invisible(updated)
}

resolve_manifest_dataset <- function(manifest, dataset_name) {
  if (is.null(manifest) || is.null(dataset_name) || !nzchar(dataset_name)) return(NULL)
  datasets <- normalize_manifest_datasets(manifest$datasets)
  for (entry in datasets) {
    name <- entry$name
    if (!is.null(name) && as.character(name) == as.character(dataset_name)) {
      return(entry)
    }
  }
  NULL
}

resolve_dataset_dir <- function(entry, workspace_root) {
  dir_path <- ""
  if (!is.null(entry$parquet)) {
    dir_path <- dirname(resolve_manifest_path(entry$parquet, workspace_root))
  } else if (!is.null(entry$analysis_log)) {
    dir_path <- dirname(resolve_manifest_path(entry$analysis_log, workspace_root))
  } else if (!is.null(entry$scratchpad)) {
    dir_path <- dirname(resolve_manifest_path(entry$scratchpad, workspace_root))
  } else if (!is.null(entry$nlss_report)) {
    dir_path <- dirname(resolve_manifest_path(entry$nlss_report, workspace_root))
  } else if (!is.null(entry$name) && nzchar(entry$name)) {
    dir_path <- file.path(workspace_root, sanitize_file_component(entry$name))
  }
  normalize_dir_path(dir_path)
}

resolve_dataset_from_cwd <- function(manifest, manifest_path, cwd = getwd()) {
  if (is.null(manifest)) return("")
  workspace_root <- dirname(manifest_path)
  cwd <- normalize_dir_path(cwd)
  if (!nzchar(cwd)) return("")
  datasets <- normalize_manifest_datasets(manifest$datasets)
  matches <- character(0)
  for (entry in datasets) {
    name <- entry$name
    if (is.null(name) || !nzchar(name)) next
    dataset_dir <- resolve_dataset_dir(entry, workspace_root)
    if (!nzchar(dataset_dir)) next
    if (is_path_within(cwd, dataset_dir)) {
      matches <- c(matches, as.character(name))
    }
  }
  matches <- unique(matches)
  if (length(matches) == 1) return(matches[1])
  if (length(matches) > 1) {
    stop("Multiple datasets match the current directory. Run from the workspace root or specify input.")
  }
  ""
}

validate_workspace_manifest_path <- function(manifest_path) {
  if (is.null(manifest_path) || !nzchar(manifest_path)) return("")
  manifest_path <- normalize_path(manifest_path)
  workspace_root <- dirname(manifest_path)
  manifest_name <- get_workspace_manifest_name()

  parent_dir <- dirname(workspace_root)
  while (nzchar(parent_dir) && parent_dir != workspace_root) {
    if (file.exists(file.path(parent_dir, manifest_name))) {
      stop("Nested workspace detected. Remove the manifest in the parent directory.")
    }
    next_dir <- dirname(parent_dir)
    if (next_dir == parent_dir) break
    parent_dir <- next_dir
  }

  descendants <- list_manifest_paths(workspace_root, recursive = TRUE)
  descendants <- setdiff(descendants, manifest_path)
  if (length(descendants) > 0) {
    stop("Nested workspace detected inside the workspace root. Remove nested manifests.")
  }

  parent_root <- dirname(workspace_root)
  if (nzchar(parent_root) && parent_root != workspace_root) {
    sibling_dirs <- list.dirs(parent_root, full.names = TRUE, recursive = FALSE)
    sibling_dirs <- sibling_dirs[sibling_dirs != parent_root]
    sibling_manifests <- character(0)
    for (dir_path in sibling_dirs) {
      candidate <- file.path(dir_path, manifest_name)
      if (file.exists(candidate)) sibling_manifests <- c(sibling_manifests, normalize_path(candidate))
    }
    sibling_manifests <- unique(sibling_manifests)
    sibling_manifests <- setdiff(sibling_manifests, manifest_path)
    if (length(sibling_manifests) > 0) {
      stop("Multiple workspaces detected in the same parent folder. Keep only one workspace.")
    }
  }
  manifest_path
}

generate_manifest_id <- function() {
  pool <- c(letters, LETTERS, 0:9)
  paste(sample(pool, 32, replace = TRUE), collapse = "")
}

format_manifest_time <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

build_manifest_dataset_entry <- function(row, workspace_root) {
  label <- as.character(row$dataset)
  file_label <- sanitize_file_component(label)
  dataset_dir <- file.path(workspace_root, file_label)
  copy_path <- if (is.na(row$copy_path)) "" else as.character(row$copy_path)
  if (!nzchar(copy_path)) {
    copy_path <- file.path(dataset_dir, paste0(file_label, ".parquet"))
  }
  source_path <- if (is.na(row$source_path)) "" else as.character(row$source_path)
  type_label <- if (is.na(row$type)) "" else tolower(as.character(row$type))
  log_seq_value <- NULL
  log_path <- file.path(dataset_dir, "analysis_log.jsonl")
  if (file.exists(log_path)) {
    log_seq_value <- read_last_log_seq_value(log_path)
  }
  entry <- list(
    name = label,
    parquet = make_relative_path(copy_path, workspace_root),
    analysis_log = make_relative_path(file.path(dataset_dir, "analysis_log.jsonl"), workspace_root),
    nlss_report = make_relative_path(file.path(dataset_dir, "report_canonical.md"), workspace_root),
    scratchpad = make_relative_path(file.path(dataset_dir, "scratchpad.md"), workspace_root),
    backup_dir = make_relative_path(file.path(dataset_dir, "backup"), workspace_root)
  )
  if (!is.null(log_seq_value) && !is.na(log_seq_value)) {
    entry$analysis_log_seq <- as.integer(log_seq_value)
  }
  if (nzchar(source_path) || nzchar(type_label)) {
    source <- list()
    if (nzchar(source_path)) source$path <- make_relative_path(source_path, workspace_root)
    if (nzchar(type_label)) source$format <- type_label
    entry$source <- source
  }
  entry
}

merge_manifest_entries <- function(existing_entries, new_entries) {
  existing_entries <- normalize_manifest_datasets(existing_entries)
  new_entries <- normalize_manifest_datasets(new_entries)
  merged <- list()
  order <- character(0)
  for (entry in existing_entries) {
    name <- entry$name
    if (is.null(name) || !nzchar(name)) next
    name <- as.character(name)
    merged[[name]] <- entry
    order <- c(order, name)
  }
  for (entry in new_entries) {
    name <- entry$name
    if (is.null(name) || !nzchar(name)) next
    name <- as.character(name)
    if (!is.null(merged[[name]])) {
      existing <- merged[[name]]
      if (!is.null(existing$analysis_log_seq) && !is.na(existing$analysis_log_seq)) {
        if (is.null(entry$analysis_log_seq) || is.na(entry$analysis_log_seq)) {
          entry$analysis_log_seq <- existing$analysis_log_seq
        }
      }
    }
    merged[[name]] <- entry
    if (!(name %in% order)) order <- c(order, name)
  }
  lapply(order, function(name) merged[[name]])
}

resolve_active_dataset_name <- function(existing_active, merged_names, new_names, override = NULL) {
  override <- if (!is.null(override)) as.character(override) else ""
  existing_active <- if (!is.null(existing_active)) as.character(existing_active) else ""
  if (nzchar(override) && override %in% merged_names) return(override)
  if (nzchar(existing_active) && existing_active %in% merged_names) return(existing_active)
  if (length(new_names) > 0) return(new_names[1])
  if (length(merged_names) > 0) return(merged_names[1])
  ""
}

update_workspace_manifest <- function(workspace_root, summary_df, active_dataset = NULL) {
  if (is.null(workspace_root) || !nzchar(workspace_root)) {
    stop("workspace_root is required to update the workspace manifest.")
  }
  workspace_root <- normalize_dir_path(workspace_root)
  manifest_path <- file.path(workspace_root, get_workspace_manifest_name())
  validate_workspace_manifest_path(manifest_path)
  existing <- if (file.exists(manifest_path)) read_workspace_manifest(manifest_path) else NULL
  existing_entries <- if (!is.null(existing)) existing$datasets else NULL
  new_entries <- list()
  if (!is.null(summary_df) && nrow(summary_df) > 0) {
    for (i in seq_len(nrow(summary_df))) {
      new_entries[[length(new_entries) + 1]] <- build_manifest_dataset_entry(summary_df[i, , drop = FALSE], workspace_root)
    }
  }
  merged_entries <- merge_manifest_entries(existing_entries, new_entries)
  merged_names <- vapply(merged_entries, function(entry) {
    name <- entry$name
    if (is.null(name)) "" else as.character(name)
  }, character(1))
  merged_names <- merged_names[nzchar(merged_names)]
  new_names <- vapply(new_entries, function(entry) as.character(entry$name), character(1))
  new_names <- new_names[nzchar(new_names)]
  active <- resolve_active_dataset_name(if (!is.null(existing)) existing$active_dataset else NULL, merged_names, new_names, active_dataset)
  manifest <- list(
    schema_version = if (!is.null(existing$schema_version)) existing$schema_version else 1,
    workspace_id = if (!is.null(existing$workspace_id) && nzchar(existing$workspace_id)) existing$workspace_id else generate_manifest_id(),
    created_at = if (!is.null(existing$created_at) && nzchar(existing$created_at)) existing$created_at else format_manifest_time(),
    active_dataset = if (nzchar(active)) active else NULL,
    datasets = merged_entries
  )
  write_workspace_manifest(manifest, manifest_path)
}

resolve_parse_bool <- function(value, default = FALSE) {
  if (exists("parse_bool", mode = "function")) {
    return(get("parse_bool", mode = "function")(value, default = default))
  }
  if (is.null(value)) return(default)
  if (is.logical(value)) return(value)
  val <- tolower(as.character(value))
  val %in% c("true", "t", "1", "yes", "y")
}

get_run_context <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
  script_name <- if (length(file_arg) > 0 && nzchar(file_arg[1])) basename(file_arg[1]) else ""
  trailing <- commandArgs(trailingOnly = TRUE)
  commands <- c("Rscript", script_name, trailing)
  commands <- commands[nzchar(commands)]
  prompt <- paste(commands, collapse = " ")
  list(prompt = prompt, commands = commands)
}

get_user_prompt <- function(opts = list()) {
  val <- NULL
  if (!is.null(opts$`user-prompt`)) {
    val <- as.character(opts$`user-prompt`)
  }
  if (is.null(val) || val == "") {
    env_val <- Sys.getenv("CODEX_USER_PROMPT", unset = "")
    if (nzchar(env_val)) val <- env_val
  }
  if (is.null(val) || val == "") return(NULL)
  val
}

nlss_log_cache <- new.env(parent = emptyenv())

normalize_report_text <- function(text) {
  if (is.null(text)) return(text)
  val <- as.character(text)
  if (length(val) == 0) return(text)
  val <- val[1]
  if (!nzchar(val)) return(text)
  val <- gsub("\r\n", "\n", val, fixed = TRUE)
  if (exists("ensure_markdown_block_spacing", mode = "function")) {
    val <- get("ensure_markdown_block_spacing", mode = "function")(val)
  }
  val
}

record_report_block <- function(report) {
  if (is.null(report)) return(invisible(FALSE))
  text <- as.character(report)
  if (length(text) == 0) return(invisible(FALSE))
  text <- text[1]
  text <- normalize_report_text(text)
  if (!nzchar(text)) return(invisible(FALSE))
  blocks <- character(0)
  if (exists("report_blocks", envir = nlss_log_cache, inherits = FALSE)) {
    blocks <- nlss_log_cache$report_blocks
  }
  blocks <- c(blocks, text)
  nlss_log_cache$report_blocks <- blocks
  invisible(TRUE)
}

record_metaskill_report_block <- function(report) {
  if (is.null(report)) return(invisible(FALSE))
  text <- as.character(report)
  if (length(text) == 0) return(invisible(FALSE))
  text <- text[1]
  text <- normalize_report_text(text)
  if (!nzchar(text)) return(invisible(FALSE))
  nlss_log_cache$metaskill_report_block <- text
  invisible(TRUE)
}

consume_report_blocks <- function() {
  blocks <- character(0)
  if (exists("report_blocks", envir = nlss_log_cache, inherits = FALSE)) {
    blocks <- nlss_log_cache$report_blocks
  }
  nlss_log_cache$report_blocks <- character(0)
  blocks
}

consume_metaskill_report_block <- function() {
  block <- ""
  if (exists("metaskill_report_block", envir = nlss_log_cache, inherits = FALSE)) {
    block <- nlss_log_cache$metaskill_report_block
  }
  nlss_log_cache$metaskill_report_block <- ""
  block
}

set_report_snapshot <- function(report) {
  if (is.null(report)) return(invisible(FALSE))
  text <- as.character(report)
  if (length(text) == 0) return(invisible(FALSE))
  text <- text[1]
  text <- normalize_report_text(text)
  if (!nzchar(text)) return(invisible(FALSE))
  nlss_log_cache$report_snapshot <- text
  invisible(TRUE)
}

consume_report_snapshot <- function() {
  snapshot <- ""
  if (exists("report_snapshot", envir = nlss_log_cache, inherits = FALSE)) {
    snapshot <- nlss_log_cache$report_snapshot
  }
  nlss_log_cache$report_snapshot <- ""
  snapshot
}

compress_report_text <- function(text) {
  if (is.null(text)) return(list(data = "", encoding = ""))
  text <- as.character(text)
  if (length(text) == 0) return(list(data = "", encoding = ""))
  text <- text[1]
  if (!nzchar(text)) return(list(data = "", encoding = ""))
  safe_text <- sanitize_utf8_text(text)
  safe_text <- if (length(safe_text) > 0) safe_text[1] else ""
  if (!nzchar(safe_text)) return(list(data = "", encoding = ""))
  raw_vec <- tryCatch(charToRaw(safe_text), error = function(e) raw(0))
  if (length(raw_vec) == 0) return(list(data = "", encoding = ""))
  compressed <- tryCatch(memCompress(raw_vec, type = "gzip"), error = function(e) raw(0))
  if (length(compressed) == 0) return(list(data = "", encoding = ""))
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(list(data = "", encoding = ""))
  b64 <- jsonlite::base64_enc(compressed)
  if (is.null(b64) || !nzchar(b64)) return(list(data = "", encoding = ""))
  list(data = b64, encoding = "gzip+base64")
}

resolve_nlss_skill_path <- function() {
  script_dir <- if (exists("resolve_script_dir", mode = "function")) {
    get("resolve_script_dir", mode = "function")()
  } else {
    getwd()
  }
  candidates <- c(
    file.path(script_dir, "..", "..", "SKILL.md"),
    file.path(script_dir, "..", "SKILL.md"),
    file.path(script_dir, "SKILL.md"),
    file.path(getwd(), "nlss", "SKILL.md"),
    file.path(getwd(), "SKILL.md")
  )
  for (candidate in candidates) {
    if (file.exists(candidate)) return(normalize_path(candidate))
  }
  ""
}

resolve_nlss_root_dir <- function() {
  path <- resolve_nlss_skill_path()
  if (!nzchar(path)) return("")
  normalize_path(dirname(path))
}

list_nlss_files <- function(root_dir) {
  if (is.null(root_dir) || !nzchar(root_dir) || !dir.exists(root_dir)) return(character(0))
  files <- character(0)
  skill_path <- file.path(root_dir, "SKILL.md")
  if (file.exists(skill_path)) {
    files <- c(files, skill_path)
  }
  scripts_dir <- file.path(root_dir, "scripts")
  if (dir.exists(scripts_dir)) {
    files <- c(files, list.files(scripts_dir, recursive = TRUE, full.names = TRUE, all.files = FALSE))
  }
  references_dir <- file.path(root_dir, "references")
  if (dir.exists(references_dir)) {
    files <- c(files, list.files(references_dir, recursive = TRUE, full.names = TRUE, all.files = FALSE))
  }
  if (length(files) == 0) return(character(0))
  info <- file.info(files)
  files <- files[!is.na(info$isdir) & !info$isdir]
  if (length(files) == 0) return(character(0))
  rel_paths <- vapply(files, function(path) make_relative_path(path, root_dir), character(1))
  rel_paths_lower <- tolower(rel_paths)
  exclude <- rel_paths_lower == "scripts/config.yml"
  files <- files[!exclude]
  if (length(files) == 0) return(character(0))
  vapply(files, normalize_path, character(1))
}

build_nlss_checksum <- function(root_dir, files) {
  if (length(files) == 0) return(NULL)
  rel_paths <- vapply(files, function(path) make_relative_path(path, root_dir), character(1))
  order_idx <- order(rel_paths)
  files <- files[order_idx]
  rel_paths <- rel_paths[order_idx]
  md5s <- tools::md5sum(files)
  md5s <- md5s[files]
  lines <- paste(rel_paths, unname(md5s), sep = " ")
  temp_manifest <- tempfile(pattern = "nlss-checksum-", fileext = ".txt")
  on.exit(unlink(temp_manifest), add = TRUE)
  writeLines(lines, temp_manifest, useBytes = TRUE)
  digest <- tools::md5sum(temp_manifest)
  list(
    algorithm = "md5",
    value = unname(digest[[1]]),
    files = length(files)
  )
}

hex_to_raw <- function(hex) {
  if (is.null(hex) || !nzchar(hex)) return(raw(0))
  hex <- tolower(as.character(hex))
  if (nchar(hex) %% 2 != 0) return(raw(0))
  bytes <- substring(hex, seq(1, nchar(hex), 2), seq(2, nchar(hex), 2))
  as.raw(strtoi(bytes, 16L))
}

raw_to_hex <- function(raw_vec) {
  if (length(raw_vec) == 0) return("")
  paste(sprintf("%02x", as.integer(raw_vec)), collapse = "")
}

xor_hex <- function(left, right) {
  left_raw <- hex_to_raw(left)
  right_raw <- hex_to_raw(right)
  if (length(left_raw) == 0 || length(right_raw) == 0) return("")
  if (length(left_raw) != length(right_raw)) return("")
  raw_to_hex(as.raw(bitwXor(as.integer(left_raw), as.integer(right_raw))))
}

compute_log_entry_checksum <- function(json_text) {
  temp_path <- tempfile(pattern = "nlss-entry-", fileext = ".json")
  on.exit(unlink(temp_path), add = TRUE)
  writeLines(json_text, temp_path, useBytes = TRUE)
  digest <- tools::md5sum(temp_path)
  unname(digest[[1]])
}

decode_log_line_text <- function(raw_vec) {
  if (length(raw_vec) == 0) return("")
  text <- rawToChar(raw_vec)
  decoded <- suppressWarnings(iconv(text, from = "UTF-8", to = "UTF-8", sub = NA))
  if (is.na(decoded)) {
    decoded <- suppressWarnings(iconv(text, from = "latin1", to = "UTF-8"))
  }
  if (is.na(decoded)) text else decoded
}

sanitize_utf8_text <- function(text) {
  if (is.null(text)) return(text)
  if (length(text) == 0) return(text)
  if (!is.character(text)) text <- as.character(text)
  vapply(text, function(item) {
    if (is.na(item)) return(NA_character_)
    if (!nzchar(item)) return(item)
    cleaned <- suppressWarnings(iconv(item, from = "", to = "UTF-8", sub = ""))
    if (is.na(cleaned)) {
      cleaned <- suppressWarnings(iconv(item, from = "latin1", to = "UTF-8", sub = ""))
    }
    if (is.na(cleaned)) "" else cleaned
  }, character(1))
}

sanitize_utf8_value <- function(value) {
  if (is.null(value)) return(value)
  if (is.factor(value)) return(sanitize_utf8_text(as.character(value)))
  if (is.character(value)) return(sanitize_utf8_text(value))
  if (is.list(value)) {
    if (is.data.frame(value)) {
      for (col in names(value)) {
        value[[col]] <- sanitize_utf8_value(value[[col]])
      }
      return(value)
    }
    out <- lapply(value, sanitize_utf8_value)
    if (!is.null(names(value))) names(out) <- names(value)
    return(out)
  }
  value
}

is_blank_log_line <- function(raw_vec) {
  text <- decode_log_line_text(raw_vec)
  !nzchar(trimws(text))
}

compute_log_line_checksum_raw <- function(raw_vec) {
  if (length(raw_vec) == 0) return("")
  temp_path <- tempfile(pattern = "nlss-line-", fileext = ".bin")
  on.exit(unlink(temp_path), add = TRUE)
  writeBin(raw_vec, temp_path)
  digest <- tools::md5sum(temp_path)
  unname(digest[[1]])
}

compute_log_seq_checksum <- function(seq_value) {
  if (is.null(seq_value) || is.na(seq_value)) return("")
  seq_text <- as.character(seq_value)
  if (!nzchar(seq_text)) return("")
  temp_path <- tempfile(pattern = "nlss-seq-", fileext = ".txt")
  on.exit(unlink(temp_path), add = TRUE)
  writeBin(charToRaw(seq_text), temp_path)
  digest <- tools::md5sum(temp_path)
  unname(digest[[1]])
}

read_last_log_seq_value <- function(path) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(NULL)
  last_line <- read_last_log_line_raw(path)
  if (is.null(last_line)) return(NULL)
  line_text <- decode_log_line_text(last_line$line_raw)
  if (!nzchar(trimws(line_text))) return(NULL)
  entry <- tryCatch(jsonlite::fromJSON(line_text, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(entry)) return(NULL)
  value <- suppressWarnings(as.integer(entry$log_seq))
  if (length(value) == 0) return(NULL)
  value <- value[1]
  if (is.na(value) || value < 0) return(NULL)
  value
}

read_last_log_line_raw <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  info <- file.info(path)
  if (is.na(info$size) || info$size <= 0) return(NULL)
  raw_data <- readBin(path, "raw", n = info$size)
  pos <- 1L
  total_len <- length(raw_data)
  last_line_raw <- NULL
  last_line_ending <- NULL
  while (pos <= total_len) {
    rel_newline <- which(raw_data[pos:total_len] == as.raw(0x0A))
    if (length(rel_newline) == 0) {
      line_raw <- raw_data[pos:total_len]
      line_ending <- raw(0)
      pos <- total_len + 1L
    } else {
      newline_pos <- pos + rel_newline[1] - 1L
      if (newline_pos > pos && raw_data[newline_pos - 1L] == as.raw(0x0D)) {
        line_raw <- if (newline_pos - 2L >= pos) raw_data[pos:(newline_pos - 2L)] else raw(0)
        line_ending <- raw_data[(newline_pos - 1L):newline_pos]
      } else {
        line_raw <- if (newline_pos - 1L >= pos) raw_data[pos:(newline_pos - 1L)] else raw(0)
        line_ending <- raw_data[newline_pos]
      }
      pos <- newline_pos + 1L
    }
    if (!is_blank_log_line(line_raw)) {
      last_line_raw <- line_raw
      last_line_ending <- line_ending
    }
  }
  if (is.null(last_line_raw)) return(NULL)
  list(line_raw = last_line_raw, line_ending = last_line_ending)
}

get_nlss_checksum <- function() {
  if (exists("nlss_checksum", envir = nlss_log_cache, inherits = FALSE)) {
    return(nlss_log_cache$nlss_checksum)
  }
  root_dir <- resolve_nlss_root_dir()
  if (!nzchar(root_dir)) return(NULL)
  files <- list_nlss_files(root_dir)
  checksum <- build_nlss_checksum(root_dir, files)
  nlss_log_cache$nlss_checksum <- checksum
  checksum
}

read_nlss_frontmatter <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(character(0))
  lines <- readLines(path, warn = FALSE)
  if (length(lines) < 3) return(character(0))
  if (trimws(lines[1]) != "---") return(character(0))
  end_idx <- which(trimws(lines[-1]) %in% c("---", "..."))
  if (length(end_idx) == 0) return(character(0))
  end_line <- end_idx[1] + 1
  lines[2:(end_line - 1)]
}

extract_metadata_version <- function(lines) {
  if (length(lines) == 0) return("")
  in_metadata <- FALSE
  meta_indent <- NULL
  for (line in lines) {
    if (!in_metadata) {
      if (grepl("^\\s*metadata\\s*:", line)) {
        in_metadata <- TRUE
        meta_indent <- nchar(gsub("^(\\s*).*", "\\1", line))
      }
      next
    }
    if (!nzchar(trimws(line))) next
    indent <- nchar(gsub("^(\\s*).*", "\\1", line))
    if (!is.null(meta_indent) && indent <= meta_indent) {
      in_metadata <- FALSE
      next
    }
    if (grepl("^\\s*version\\s*:", line)) {
      value <- sub("^\\s*version\\s*:\\s*", "", line)
      value <- sub("\\s+#.*$", "", value)
      value <- gsub("^\"|\"$", "", value)
      value <- gsub("^'|'$", "", value)
      value <- trimws(value)
      if (nzchar(value)) return(value)
    }
  }
  ""
}

get_nlss_version <- function() {
  if (exists("nlss_version", envir = nlss_log_cache, inherits = FALSE)) {
    return(nlss_log_cache$nlss_version)
  }
  path <- resolve_nlss_skill_path()
  version <- extract_metadata_version(read_nlss_frontmatter(path))
  if (!nzchar(version)) {
    config_version <- NULL
    if (exists("get_config_value", mode = "function")) {
      config_version <- get("get_config_value", mode = "function")("nlss_version", NULL)
    }
    if (!is.null(config_version) && nzchar(config_version)) {
      version <- as.character(config_version)
    }
  }
  if (!nzchar(version)) version <- NA_character_
  nlss_log_cache$nlss_version <- version
  version
}

resolve_agent_default <- function() {
  config_agent <- NULL
  if (exists("get_config_value", mode = "function")) {
    config_agent <- get("get_config_value", mode = "function")("modules.init_workspace.agent", NULL)
  }
  if (!is.null(config_agent) && nzchar(config_agent)) return(as.character(config_agent))
  env_agent <- Sys.getenv("CODEX_AGENT", unset = "")
  if (nzchar(env_agent)) return(env_agent)
  "Codex"
}

escape_yaml_value <- function(value) {
  if (is.null(value)) return("")
  val <- as.character(value)
  gsub("\"", "\\\\\"", val)
}

find_frontmatter_end <- function(lines) {
  if (length(lines) < 2) return(0)
  if (trimws(lines[1]) != "---") return(0)
  end_idx <- which(trimws(lines[-1]) %in% c("---", "..."))
  if (length(end_idx) == 0) return(0)
  end_idx[1] + 1
}

build_output_front_matter <- function(path, workspace_root = NULL, agent = NULL, created_at = NULL) {
  if (is.null(created_at) || !nzchar(created_at)) {
    created_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  }
  if (is.null(agent) || !nzchar(agent)) {
    agent <- resolve_agent_default()
  }
  nlss_version <- get_nlss_version()
  if (is.na(nlss_version)) nlss_version <- ""
  resolved_root <- workspace_root
  if (is.null(resolved_root) || !nzchar(resolved_root)) {
    resolved_root <- resolve_workspace_root(dirname(path))
  }
  output_path <- render_log_path(path, workspace_root = resolved_root)
  lines <- c(
    "---",
    paste0("created_at: \"", escape_yaml_value(created_at), "\""),
    paste0("path: \"", escape_yaml_value(output_path), "\""),
    paste0("os: \"", escape_yaml_value(format_log_os_string()), "\""),
    paste0("r_version: \"", escape_yaml_value(R.version.string), "\""),
    paste0("agent: \"", escape_yaml_value(agent), "\""),
    paste0("nlss_version: \"", escape_yaml_value(nlss_version), "\""),
    "---"
  )
  paste(lines, collapse = "\n")
}

report_has_body <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(FALSE)
  lines <- readLines(path, warn = FALSE)
  if (length(lines) == 0) return(FALSE)
  end_line <- find_frontmatter_end(lines)
  if (end_line == 0) return(any(nzchar(trimws(lines))))
  if (end_line >= length(lines)) return(FALSE)
  any(nzchar(trimws(lines[(end_line + 1):length(lines)])))
}

ensure_output_front_matter <- function(path, workspace_root = NULL, agent = NULL, created_at = NULL) {
  if (is.null(path) || !nzchar(path)) return(invisible(FALSE))
  front_matter <- build_output_front_matter(path, workspace_root = workspace_root, agent = agent, created_at = created_at)
  front_lines <- strsplit(front_matter, "\n", fixed = TRUE)[[1]]
  if (!file.exists(path)) {
    con <- file(path, open = "w", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    writeLines(c(front_lines, ""), con)
    return(invisible(TRUE))
  }
  lines <- readLines(path, warn = FALSE)
  if (length(lines) == 0) {
    con <- file(path, open = "w", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    writeLines(c(front_lines, ""), con)
    return(invisible(TRUE))
  }
  end_line <- find_frontmatter_end(lines)
  if (end_line == 0) {
    con <- file(path, open = "w", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    writeLines(c(front_lines, "", lines), con)
    return(invisible(TRUE))
  }
  front_section <- if (end_line > 1) lines[2:(end_line - 1)] else character(0)
  updated <- FALSE
  if (!any(grepl("^\\s*nlss_version\\s*:", front_section))) {
    nlss_version <- get_nlss_version()
    if (is.na(nlss_version)) nlss_version <- ""
    insert_line <- paste0("nlss_version: \"", escape_yaml_value(nlss_version), "\"")
    lines <- c(lines[1:(end_line - 1)], insert_line, lines[end_line:length(lines)])
    updated <- TRUE
  }
  end_line <- find_frontmatter_end(lines)
  if (end_line > 0) {
    insert_blank <- FALSE
    if (end_line >= length(lines)) {
      insert_blank <- TRUE
    } else if (nzchar(trimws(lines[end_line + 1]))) {
      insert_blank <- TRUE
    }
    if (insert_blank) {
      lines <- append(lines, "", after = end_line)
      updated <- TRUE
    }
  }
  if (updated) {
    con <- file(path, open = "w", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    writeLines(lines, con)
  }
  invisible(TRUE)
}

format_log_os_string <- function() {
  info <- Sys.info()
  if (!is.null(info) && !is.na(info["sysname"])) {
    parts <- c(info["sysname"], info["release"])
    parts <- parts[!is.na(parts) & nzchar(parts)]
    return(paste(parts, collapse = " "))
  }
  R.version$platform
}

append_analysis_log <- function(out_dir, module, prompt, commands, results, options = list(), user_prompt = NULL) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    cat("Note: jsonlite not installed; skipping analysis_log.jsonl output.\n")
    return(invisible(FALSE))
  }

  log_checksum_default <- resolve_config_value("defaults.log_nlss_checksum", FALSE)
  checksum <- NULL
  if (resolve_parse_bool(log_checksum_default, default = FALSE)) {
    checksum <- get_nlss_checksum()
  }

  log_path <- file.path(out_dir, "analysis_log.jsonl")
  manifest_path <- find_workspace_manifest(out_dir)
  manifest <- NULL
  workspace_root <- ""
  if (nzchar(manifest_path)) {
    manifest <- read_workspace_manifest(manifest_path)
    workspace_root <- dirname(manifest_path)
  }
  log_seq <- NULL
  log_commands <- render_paths_for_log(commands, workspace_root = workspace_root)
  log_options <- render_paths_for_log(options, workspace_root = workspace_root)
  log_results <- render_paths_for_log(results, workspace_root = workspace_root)
  log_prompt <- prompt
  if (!is.null(log_commands) && is.character(log_commands) && length(log_commands) > 0) {
    log_prompt <- paste(log_commands, collapse = " ")
  }

  entry <- list(
    timestamp_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    nlss_version = get_nlss_version(),
    r_version = R.version.string,
    os = format_log_os_string(),
    module = module,
    user_prompt = user_prompt,
    prompt = log_prompt,
    commands = log_commands,
    results = log_results,
    options = log_options
  )

  report_blocks <- consume_report_blocks()
  if (length(report_blocks) > 0) {
    combined <- paste(report_blocks, collapse = "")
    encoded <- compress_report_text(combined)
    if (nzchar(encoded$data)) {
      entry$report_block_b64 <- encoded$data
      entry$report_block_encoding <- encoded$encoding
    }
  }

  report_snapshot <- consume_report_snapshot()
  if (nzchar(report_snapshot)) {
    encoded_snapshot <- compress_report_text(report_snapshot)
    if (nzchar(encoded_snapshot$data)) {
      entry$report_block_full_b64 <- encoded_snapshot$data
      entry$report_block_full_encoding <- encoded_snapshot$encoding
    }
  }

  metaskill_block <- consume_metaskill_report_block()
  if (nzchar(metaskill_block)) {
    encoded_meta <- compress_report_text(metaskill_block)
    if (nzchar(encoded_meta$data)) {
      entry$metaskill_report_block_b64 <- encoded_meta$data
      entry$metaskill_report_block_encoding <- encoded_meta$encoding
    }
  }

  entry <- sanitize_utf8_value(entry)

  if (!is.null(checksum) && !is.null(checksum$value) && nzchar(checksum$value)) {
    entry$checksum_version <- 3
    log_seq_value <- NULL
    log_missing <- !file.exists(log_path)
    if (!log_missing && !is.null(manifest) && nzchar(workspace_root)) {
      log_seq_value <- resolve_manifest_log_seq(manifest, out_dir, workspace_root)
    }
    if (!log_missing && is.null(log_seq_value)) {
      log_seq_value <- read_last_log_seq_value(log_path)
    }
    if (log_missing || is.null(log_seq_value) || is.na(log_seq_value) || log_seq_value < 0) {
      log_seq_value <- 0L
    }
    log_seq <- as.integer(log_seq_value) + 1L
    entry$log_seq <- log_seq
    prev_line_checksum <- NULL
    if (file.exists(log_path)) {
      prev_line <- read_last_log_line_raw(log_path)
      if (!is.null(prev_line)) {
        prev_raw <- c(prev_line$line_raw, prev_line$line_ending)
        prev_line_checksum <- compute_log_line_checksum_raw(prev_raw)
      }
    }
    entry_json <- jsonlite::toJSON(
      entry,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      dataframe = "rows",
      digits = NA
    )
    entry_checksum <- compute_log_entry_checksum(entry_json)
    combined <- xor_hex(checksum$value, entry_checksum)
    if (!is.null(prev_line_checksum) && nzchar(prev_line_checksum)) {
      combined <- xor_hex(combined, prev_line_checksum)
    }
    seq_checksum <- compute_log_seq_checksum(log_seq)
    if (nzchar(seq_checksum)) {
      combined <- xor_hex(combined, seq_checksum)
    }
    if (nzchar(combined)) entry$checksum <- combined
  }

  json <- jsonlite::toJSON(
    entry,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    dataframe = "rows",
    digits = NA
  )

  con <- file(log_path, open = "a", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(json, con = con, sep = "\n")
  if (!is.null(log_seq) && !is.na(log_seq) && nzchar(manifest_path)) {
    update_manifest_log_seq(manifest_path, out_dir, log_seq)
  }
  invisible(TRUE)
}

read_sav_data <- function(path) {
  path <- normalize_input_path(path)
  if (requireNamespace("haven", quietly = TRUE)) {
    df <- haven::read_sav(path)
    return(as.data.frame(df, stringsAsFactors = FALSE))
  }
  if (requireNamespace("foreign", quietly = TRUE)) {
    df <- suppressWarnings(foreign::read.spss(path, to.data.frame = TRUE, use.value.labels = FALSE))
    if (!is.data.frame(df)) df <- as.data.frame(df, stringsAsFactors = FALSE)
    return(df)
  }
  stop("SPSS .sav support requires the 'haven' or 'foreign' package. Install one: install.packages('haven').")
}

configure_arrow_defaults <- function() {
  arrow_ns <- asNamespace("arrow")
  # Avoid keeping Parquet files memory-mapped/locked on Windows.
  fn_names <- c("set_use_altrep", "set_use_memory_mapping", "set_use_memory_map", "set_use_mmap")
  for (fn_name in fn_names) {
    if (exists(fn_name, envir = arrow_ns, mode = "function")) {
      try(get(fn_name, envir = arrow_ns)(FALSE), silent = TRUE)
    }
  }
  options(
    arrow.use_altrep = FALSE,
    arrow.use_memory_map = FALSE,
    arrow.use_memory_mapping = FALSE,
    arrow.use_mmap = FALSE
  )
}

ensure_arrow <- function() {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Parquet support requires the 'arrow' package. Install it: install.packages('arrow').")
  }
  configure_arrow_defaults()
}

resolve_extract_label_metadata <- function(df) {
  if (exists("extract_label_metadata", mode = "function")) {
    return(get("extract_label_metadata", mode = "function")(df))
  }
  list()
}

resolve_merge_label_metadata <- function(primary, secondary) {
  if (exists("merge_label_metadata", mode = "function")) {
    return(get("merge_label_metadata", mode = "function")(primary, secondary))
  }
  if (is.null(primary) || length(primary) == 0) return(secondary)
  if (is.null(secondary) || length(secondary) == 0) return(primary)
  primary
}

resolve_attach_label_metadata <- function(df, metadata) {
  if (exists("attach_label_metadata", mode = "function")) {
    return(get("attach_label_metadata", mode = "function")(df, metadata))
  }
  if (!is.null(metadata) && length(metadata) > 0) {
    attr(df, "nlss_labels") <- metadata
  }
  df
}

resolve_normalize_label_metadata <- function(metadata) {
  if (exists("normalize_label_metadata", mode = "function")) {
    return(get("normalize_label_metadata", mode = "function")(metadata))
  }
  metadata
}

serialize_label_metadata <- function(metadata) {
  if (is.null(metadata) || length(metadata) == 0) return("")
  if (!requireNamespace("jsonlite", quietly = TRUE)) return("")
  jsonlite::toJSON(metadata, auto_unbox = TRUE, null = "null", digits = NA)
}

parse_label_metadata <- function(value) {
  if (is.null(value) || length(value) == 0) return(list())
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(list())
  if (is.raw(value)) {
    value <- rawToChar(value)
  }
  if (!is.character(value)) return(list())
  if (!nzchar(value[1])) return(list())
  parsed <- tryCatch(jsonlite::fromJSON(value[1], simplifyVector = FALSE), error = function(e) list())
  resolve_normalize_label_metadata(parsed)
}

get_arrow_table_metadata <- function(tbl) {
  if (is.null(tbl) || !is.environment(tbl)) return(list())
  if (!"metadata" %in% ls(envir = tbl)) return(list())
  meta <- tryCatch(get("metadata", envir = tbl), error = function(e) list())
  if (!is.list(meta)) return(list())
  meta
}

replace_arrow_table_metadata <- function(tbl, metadata) {
  if (is.null(tbl) || !is.environment(tbl)) return(tbl)
  if (is.null(metadata) || !is.list(metadata)) return(tbl)
  if ("ReplaceSchemaMetadata" %in% ls(envir = tbl)) {
    fn <- get("ReplaceSchemaMetadata", envir = tbl)
    if (is.function(fn)) return(fn(metadata))
  }
  tbl
}

table_to_data_frame <- function(tbl) {
  if (is.null(tbl) || !is.environment(tbl)) return(as.data.frame(tbl, stringsAsFactors = FALSE))
  if ("to_data_frame" %in% ls(envir = tbl)) {
    fn <- get("to_data_frame", envir = tbl)
    if (is.function(fn)) {
      df <- fn()
      return(as.data.frame(df, stringsAsFactors = FALSE))
    }
  }
  as.data.frame(tbl, stringsAsFactors = FALSE)
}

read_parquet_label_metadata <- function(tbl) {
  meta <- get_arrow_table_metadata(tbl)
  if (length(meta) == 0) return(list())
  label_json <- meta[["nlss:labels"]]
  if (is.null(label_json)) return(list())
  parse_label_metadata(label_json)
}

copy_parquet_to_temp <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return("")
  temp_path <- tempfile(pattern = "nlss-parquet-", fileext = ".parquet")
  copied <- file.copy(path, temp_path, overwrite = TRUE)
  if (!isTRUE(copied)) return("")
  temp_path
}

read_parquet_data <- function(path, lock_safe = FALSE) {
  ensure_arrow()
  path <- normalize_input_path(path)
  temp_path <- ""
  if (lock_safe && is_windows() && file.exists(path)) {
    temp_path <- copy_parquet_to_temp(path)
    if (nzchar(temp_path)) path <- temp_path
  }
  read_fun <- arrow::read_parquet
  extra <- list()
  read_formals <- formals(read_fun)
  if (!is.null(read_formals)) {
    arg_names <- names(read_formals)
    if ("memory_map" %in% arg_names) extra$memory_map <- FALSE
    if ("use_memory_map" %in% arg_names) extra$use_memory_map <- FALSE
    if ("use_mmap" %in% arg_names) extra$use_mmap <- FALSE
  }
  tbl <- do.call(read_fun, c(list(path, as_data_frame = FALSE), extra))
  label_meta <- read_parquet_label_metadata(tbl)
  df <- table_to_data_frame(tbl)
  extracted <- resolve_extract_label_metadata(df)
  merged <- resolve_merge_label_metadata(label_meta, extracted)
  df <- resolve_attach_label_metadata(df, merged)
  if (nzchar(temp_path)) {
    try(unlink(temp_path), silent = TRUE)
  }
  df
}

write_parquet_data <- function(df, path, label_metadata = NULL) {
  ensure_arrow()
  path <- normalize_input_path(path)
  label_metadata <- resolve_merge_label_metadata(label_metadata, resolve_extract_label_metadata(df))
  label_json <- serialize_label_metadata(label_metadata)
  if (nzchar(label_json)) {
    tbl <- arrow::arrow_table(df)
    meta <- get_arrow_table_metadata(tbl)
    meta[["nlss:labels"]] <- label_json
    tbl <- replace_arrow_table_metadata(tbl, meta)
    arrow::write_parquet(tbl, path)
    return(invisible(TRUE))
  }
  arrow::write_parquet(df, path)
}

load_or_create_parquet <- function(copy_path, read_source) {
  if (file.exists(copy_path)) {
    return(read_parquet_data(copy_path))
  }
  df <- read_source()
  label_metadata <- resolve_extract_label_metadata(df)
  write_parquet_data(df, copy_path, label_metadata = label_metadata)
  df_out <- read_parquet_data(copy_path)
  if (!is.null(label_metadata) && length(label_metadata) > 0) {
    df_out <- resolve_attach_label_metadata(df_out, label_metadata)
  }
  df_out
}

sanitize_file_component <- function(value) {
  clean <- enc2utf8(as.character(value))
  clean <- gsub("[^A-Za-z0-9._-]", "_", clean)
  clean <- gsub("_+", "_", clean)
  if (!nzchar(clean)) clean <- "dataset"
  clean
}

derive_dataset_label <- function(path, df_name = NULL) {
  if (!is.null(df_name) && nzchar(df_name)) return(as.character(df_name))
  base <- tools::file_path_sans_ext(basename(path))
  if (!nzchar(base)) base <- "dataset"
  base
}

normalize_path <- function(path) {
  path <- normalize_input_path(path)
  if (!nzchar(path)) return("")
  enc2utf8(normalizePath(path, winslash = "/", mustWork = FALSE))
}

get_dataset_workspace_dir <- function(label) {
  file_label <- sanitize_file_component(label)
  root <- ensure_out_dir(get_default_out())
  ensure_out_dir(file.path(root, file_label))
}

get_workspace_out_dir <- function(df = NULL, label = NULL) {
  if (!is.null(df)) {
    dir_attr <- attr(df, "workspace_dir")
    if (!is.null(dir_attr) && nzchar(dir_attr)) return(ensure_out_dir(dir_attr))
    parquet_path <- attr(df, "workspace_parquet_path")
    if (!is.null(parquet_path) && nzchar(parquet_path)) {
      return(ensure_out_dir(dirname(parquet_path)))
    }
  }
  if (!is.null(label) && nzchar(label)) {
    return(get_dataset_workspace_dir(label))
  }
  ensure_out_dir(get_default_out())
}

format_backup_timestamp <- function() {
  now <- Sys.time()
  base <- format(now, "%Y%m%d%H%M%S")
  frac <- as.numeric(now) %% 1
  ms <- as.integer(floor(frac * 1000))
  paste0(base, sprintf("%03d", ms))
}

backup_workspace_parquet <- function(parquet_path) {
  if (is.null(parquet_path) || !nzchar(parquet_path)) return("")
  if (!file.exists(parquet_path)) return("")
  dataset_dir <- dirname(parquet_path)
  backup_dir <- ensure_out_dir(file.path(dataset_dir, "backup"))
  file_label <- tools::file_path_sans_ext(basename(parquet_path))
  timestamp <- format_backup_timestamp()
  backup_path <- file.path(backup_dir, paste0(file_label, "-", timestamp, ".parquet"))
  copied <- file.copy(parquet_path, backup_path, overwrite = FALSE)
  if (!isTRUE(copied)) return("")
  normalize_path(backup_path)
}

build_workspace_copy_info <- function(label) {
  file_label <- sanitize_file_component(label)
  out_dir <- get_dataset_workspace_dir(label)
  list(
    label = label,
    file_label = file_label,
    out_dir = out_dir,
    copy_path = file.path(out_dir, paste0(file_label, ".parquet"))
  )
}

load_rdata_frame <- function(path, object_name = NULL) {
  path <- normalize_input_path(path)
  env <- new.env()
  load(path, envir = env)
  if (!is.null(object_name) && nzchar(object_name) && exists(object_name, envir = env)) {
    df <- get(object_name, envir = env)
    if (!is.data.frame(df)) stop("RData object is not a data frame.")
    return(list(df = df, object_name = object_name))
  }
  objects <- ls(env)
  if (length(objects) == 0) stop("RData does not contain a data frame.")
  df_names <- objects[vapply(objects, function(name) {
    is.data.frame(get(name, envir = env))
  }, logical(1))]
  if (length(df_names) == 0) stop("RData does not contain a data frame.")
  df_name <- df_names[1]
  df <- get(df_name, envir = env)
  if (!is.data.frame(df)) stop("RData object is not a data frame.")
  list(df = df, object_name = df_name)
}

load_dataframe <- function(opts, lock_safe = FALSE) {
  ensure_out_dir(get_default_out())

  if (!is.null(opts$parquet)) {
    source_path <- normalize_input_path(opts$parquet)
    label <- derive_dataset_label(source_path)
    copy_info <- build_workspace_copy_info(label)
    if (normalizePath(source_path, winslash = "/", mustWork = FALSE) ==
        normalizePath(copy_info$copy_path, winslash = "/", mustWork = FALSE)) {
      df <- read_parquet_data(copy_info$copy_path, lock_safe = lock_safe)
    } else {
      df <- load_or_create_parquet(copy_info$copy_path, function() {
        read_parquet_data(source_path)
      })
    }
    attr(df, "workspace_parquet_path") <- normalize_path(copy_info$copy_path)
    attr(df, "workspace_source_path") <- normalize_path(source_path)
    attr(df, "workspace_dir") <- normalize_path(copy_info$out_dir)
    return(df)
  }

  if (!is.null(opts$csv)) {
    source_path <- normalize_input_path(opts$csv)
    label <- derive_dataset_label(source_path)
    copy_info <- build_workspace_copy_info(label)
    sep_default <- resolve_config_value("defaults.csv.sep", ",")
    sep <- if (!is.null(opts$sep)) opts$sep else sep_default
    header_default <- resolve_config_value("defaults.csv.header", TRUE)
    header <- resolve_parse_bool(opts$header, default = header_default)
    df <- load_or_create_parquet(copy_info$copy_path, function() {
      read.csv(source_path, sep = sep, header = header, stringsAsFactors = FALSE)
    })
    attr(df, "workspace_parquet_path") <- normalize_path(copy_info$copy_path)
    attr(df, "workspace_source_path") <- normalize_path(source_path)
    attr(df, "workspace_dir") <- normalize_path(copy_info$out_dir)
    return(df)
  }

  if (!is.null(opts$sav)) {
    source_path <- normalize_input_path(opts$sav)
    label <- derive_dataset_label(source_path)
    copy_info <- build_workspace_copy_info(label)
    df <- load_or_create_parquet(copy_info$copy_path, function() {
      df <- read_sav_data(source_path)
      if (!is.data.frame(df)) stop("SAV does not contain a data frame.")
      df
    })
    attr(df, "workspace_parquet_path") <- normalize_path(copy_info$copy_path)
    attr(df, "workspace_source_path") <- normalize_path(source_path)
    attr(df, "workspace_dir") <- normalize_path(copy_info$out_dir)
    return(df)
  }

  if (!is.null(opts$rds)) {
    source_path <- normalize_input_path(opts$rds)
    label <- derive_dataset_label(source_path)
    copy_info <- build_workspace_copy_info(label)
    df <- load_or_create_parquet(copy_info$copy_path, function() {
      df <- readRDS(source_path)
      if (!is.data.frame(df)) stop("RDS does not contain a data frame.")
      df
    })
    attr(df, "workspace_parquet_path") <- normalize_path(copy_info$copy_path)
    attr(df, "workspace_source_path") <- normalize_path(source_path)
    attr(df, "workspace_dir") <- normalize_path(copy_info$out_dir)
    return(df)
  }

  if (!is.null(opts$rdata)) {
    if (is.null(opts$df)) stop("--df is required when using --rdata")
    source_path <- normalize_input_path(opts$rdata)
    label <- derive_dataset_label(source_path, opts$df)
    copy_info <- build_workspace_copy_info(label)
    df <- load_or_create_parquet(copy_info$copy_path, function() {
      res <- load_rdata_frame(source_path, opts$df)
      res$df
    })
    attr(df, "workspace_parquet_path") <- normalize_path(copy_info$copy_path)
    attr(df, "workspace_source_path") <- normalize_path(source_path)
    attr(df, "workspace_dir") <- normalize_path(copy_info$out_dir)
    return(df)
  }

  manifest_path <- find_workspace_manifest()
  if (nzchar(manifest_path)) {
    manifest <- read_workspace_manifest(manifest_path)
    dataset_name <- resolve_dataset_from_cwd(manifest, manifest_path, getwd())
    if (!nzchar(dataset_name)) {
      dataset_name <- if (!is.null(manifest$active_dataset)) as.character(manifest$active_dataset) else ""
    }
    if (!nzchar(dataset_name)) {
      stop("No dataset specified and workspace manifest has no active_dataset. Run from a dataset folder or set active_dataset.")
    }
    entry <- resolve_manifest_dataset(manifest, dataset_name)
    if (is.null(entry)) {
      stop("Dataset not found in workspace manifest: ", dataset_name)
    }
    workspace_root <- dirname(manifest_path)
    parquet_path <- resolve_manifest_path(entry$parquet, workspace_root)
    if (!nzchar(parquet_path) || !file.exists(parquet_path)) {
      stop("Workspace parquet not found: ", parquet_path)
    }
    df <- read_parquet_data(parquet_path, lock_safe = lock_safe)
    attr(df, "workspace_parquet_path") <- normalize_path(parquet_path)
    attr(df, "workspace_dir") <- normalize_path(dirname(parquet_path))
    attr(df, "workspace_manifest_path") <- normalize_path(manifest_path)
    attr(df, "workspace_root") <- normalize_path(workspace_root)
    attr(df, "workspace_dataset") <- dataset_name
    source_path <- ""
    if (!is.null(entry$source) && !is.null(entry$source$path)) {
      source_path <- resolve_manifest_path(entry$source$path, workspace_root)
    }
    if (nzchar(source_path)) {
      attr(df, "workspace_source_path") <- normalize_path(source_path)
    }
    return(df)
  }

  stop("No input provided. Use --csv, --sav, --rds, --rdata, --parquet, or run inside a workspace directory with a manifest.")
}
