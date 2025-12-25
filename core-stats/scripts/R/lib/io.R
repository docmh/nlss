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
  gsub("([\\.^$|()\\[\\]{}*+?\\\\])", "\\\\\\1", as.character(text))
}

get_workspace_manifest_name <- function() {
  name <- resolve_config_value("defaults.workspace_manifest", "core-stats-workspace.yml")
  if (is.null(name) || !nzchar(name)) return("core-stats-workspace.yml")
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
  } else if (!is.null(entry$apa_report)) {
    dir_path <- dirname(resolve_manifest_path(entry$apa_report, workspace_root))
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
  entry <- list(
    name = label,
    parquet = make_relative_path(copy_path, workspace_root),
    analysis_log = make_relative_path(file.path(dataset_dir, "analysis_log.jsonl"), workspace_root),
    apa_report = make_relative_path(file.path(dataset_dir, "apa_report.md"), workspace_root),
    scratchpad = make_relative_path(file.path(dataset_dir, "scratchpad.md"), workspace_root),
    backup_dir = make_relative_path(file.path(dataset_dir, "backup"), workspace_root)
  )
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

core_stats_log_cache <- new.env(parent = emptyenv())

resolve_core_stats_skill_path <- function() {
  script_dir <- if (exists("resolve_script_dir", mode = "function")) {
    resolve_script_dir()
  } else {
    getwd()
  }
  candidates <- c(
    file.path(script_dir, "..", "..", "SKILL.md"),
    file.path(script_dir, "..", "SKILL.md"),
    file.path(script_dir, "SKILL.md"),
    file.path(getwd(), "core-stats", "SKILL.md"),
    file.path(getwd(), "SKILL.md")
  )
  for (candidate in candidates) {
    if (file.exists(candidate)) return(normalize_path(candidate))
  }
  ""
}

read_core_stats_frontmatter <- function(path) {
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

get_core_stats_version <- function() {
  if (exists("core_stats_version", envir = core_stats_log_cache, inherits = FALSE)) {
    return(core_stats_log_cache$core_stats_version)
  }
  path <- resolve_core_stats_skill_path()
  version <- extract_metadata_version(read_core_stats_frontmatter(path))
  if (!nzchar(version)) version <- NA_character_
  core_stats_log_cache$core_stats_version <- version
  version
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

  entry <- list(
    timestamp_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    core_stats_version = get_core_stats_version(),
    r_version = R.version.string,
    os = format_log_os_string(),
    module = module,
    user_prompt = user_prompt,
    prompt = prompt,
    commands = commands,
    results = results,
    options = options
  )

  json <- jsonlite::toJSON(
    entry,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    dataframe = "rows",
    digits = NA
  )

  log_path <- file.path(out_dir, "analysis_log.jsonl")
  con <- file(log_path, open = "a", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(json, con = con, sep = "\n")
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

copy_parquet_to_temp <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return("")
  temp_path <- tempfile(pattern = "core-stats-parquet-", fileext = ".parquet")
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
  df <- do.call(read_fun, c(list(path, as_data_frame = TRUE), extra))
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (nzchar(temp_path)) {
    try(unlink(temp_path), silent = TRUE)
  }
  df
}

write_parquet_data <- function(df, path) {
  ensure_arrow()
  path <- normalize_input_path(path)
  arrow::write_parquet(df, path)
}

load_or_create_parquet <- function(copy_path, read_source) {
  if (file.exists(copy_path)) {
    return(read_parquet_data(copy_path))
  }
  df <- read_source()
  write_parquet_data(df, copy_path)
  read_parquet_data(copy_path)
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
