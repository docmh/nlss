#!/usr/bin/env Rscript

bootstrap_dir <- {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", cmd_args[grep("^--file=", cmd_args)])
  if (length(file_arg) > 0 && nzchar(file_arg[1])) {
    dirname(normalizePath(file_arg[1], winslash = "/", mustWork = FALSE))
  } else {
    getwd()
  }
}

source(file.path(bootstrap_dir, "lib", "paths.R"))
source_lib("cli.R")
source_lib("config.R")
source_lib("io.R")
source_lib("formatting.R")

print_usage <- function() {
  cat("Initialize workspace outputs (base R)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript init_workspace.R --csv data.csv\n")
  cat("  Rscript init_workspace.R --sav data.sav\n")
  cat("  Rscript init_workspace.R --rds data.rds\n")
  cat("  Rscript init_workspace.R --rdata data.RData --df data_frame_name\n")
  cat("  Rscript init_workspace.R --parquet data.parquet\n")
  cat("  Rscript init_workspace.R --interactive\n")
  cat("\n")
  cat("Options:\n")
  cat("  --csv PATHS            CSV input file(s), comma-separated\n")
  cat("  --sav PATHS            SPSS .sav input file(s), comma-separated\n")
  cat("  --rds PATHS            RDS input file(s), comma-separated\n")
  cat("  --rdata PATHS          RData input file(s), comma-separated\n")
  cat("  --parquet PATHS        Parquet input file(s), comma-separated\n")
  cat("  --df NAMES             RData data frame name(s), comma-separated\n")
  cat("  --sep VALUE            CSV separator (default: ,)\n")
  cat("  --header TRUE/FALSE    CSV header (default: TRUE)\n")
  cat("  --agent TEXT           Agent name (default from config or CODEX_AGENT)\n")
  cat("  --user-prompt TEXT     Original AI user prompt for logging (optional)\n")
  cat("  --log TRUE/FALSE       Write analysis_log.jsonl (default: TRUE)\n")
  cat("  --interactive          Prompt for inputs\n")
  cat("  --help                 Show this help\n")
}

interactive_options <- function() {
  cat("Interactive input selected.\n")
  opts <- list()
  opts$csv <- resolve_prompt("CSV path(s) (comma-separated, blank for none)", "")
  if (nzchar(opts$csv)) {
    sep_default <- resolve_config_value("defaults.csv.sep", ",")
    header_default <- resolve_config_value("defaults.csv.header", TRUE)
    opts$sep <- resolve_prompt("Separator", sep_default)
    opts$header <- resolve_prompt("Header TRUE/FALSE", ifelse(isTRUE(header_default), "TRUE", "FALSE"))
  }
  opts$sav <- resolve_prompt("SAV path(s) (comma-separated, blank for none)", "")
  opts$rds <- resolve_prompt("RDS path(s) (comma-separated, blank for none)", "")
  opts$rdata <- resolve_prompt("RData path(s) (comma-separated, blank for none)", "")
  if (nzchar(opts$rdata)) {
    opts$df <- resolve_prompt("RData data frame name(s), comma-separated", "")
  }
  opts$parquet <- resolve_prompt("Parquet path(s) (comma-separated, blank for none)", "")
  agent_default <- resolve_agent_default()
  opts$agent <- resolve_prompt("Agent name", agent_default)
  opts$`user-prompt` <- resolve_prompt("User prompt (optional)", "")
  log_default <- resolve_config_value("defaults.log", TRUE)
  opts$log <- resolve_prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))
  opts
}

resolve_prompt <- function(label, default = NULL) {
  if (exists("prompt", mode = "function")) {
    return(get("prompt", mode = "function")(label, default = default))
  }
  if (is.null(default)) {
    answer <- readline(paste0(label, ": "))
  } else {
    answer <- readline(paste0(label, " [", default, "]: "))
    if (answer == "") answer <- default
  }
  answer
}

resolve_default_out <- function() {
  if (exists("get_default_out", mode = "function")) {
    return(get("get_default_out", mode = "function")())
  }
  "./outputs/tmp"
}

resolve_config_value <- function(path, default = NULL) {
  if (exists("get_config_value", mode = "function")) {
    return(get("get_config_value", mode = "function")(path, default = default))
  }
  default
}

resolve_parse_args <- function(args) {
  if (exists("parse_args", mode = "function")) {
    return(get("parse_args", mode = "function")(args))
  }
  opts <- list()
  i <- 1
  while (i <= length(args)) {
    arg <- args[i]
    if (grepl("^--", arg)) {
      key <- sub("^--", "", arg)
      if (grepl("=", key)) {
        parts <- strsplit(key, "=", fixed = TRUE)[[1]]
        opts[[parts[1]]] <- parts[2]
      } else if (i < length(args) && !grepl("^--", args[i + 1])) {
        opts[[key]] <- args[i + 1]
        i <- i + 1
      } else {
        opts[[key]] <- TRUE
      }
    }
    i <- i + 1
  }
  opts
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

resolve_parse_list <- function(value, sep = ",") {
  if (exists("parse_list", mode = "function")) {
    return(get("parse_list", mode = "function")(value, sep = sep))
  }
  if (is.null(value) || is.logical(value)) return(character(0))
  value <- as.character(value)
  if (value == "") return(character(0))
  trimws(strsplit(value, sep, fixed = TRUE)[[1]])
}

resolve_ensure_out_dir <- function(path) {
  if (exists("ensure_out_dir", mode = "function")) {
    return(get("ensure_out_dir", mode = "function")(path))
  }
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  path
}

resolve_load_dataframe <- function(opts) {
  if (exists("load_dataframe", mode = "function")) {
    return(get("load_dataframe", mode = "function")(opts))
  }
  stop("Missing load_dataframe. Ensure lib/io.R is sourced.")
}

resolve_build_workspace_copy_info <- function(label) {
  if (exists("build_workspace_copy_info", mode = "function")) {
    return(get("build_workspace_copy_info", mode = "function")(label))
  }
  stop("Missing build_workspace_copy_info. Ensure lib/io.R is sourced.")
}

resolve_get_template_path <- function(key, default_relative = NULL) {
  if (exists("resolve_template_path", mode = "function")) {
    return(get("resolve_template_path", mode = "function")(key, default_relative))
  }
  NULL
}

resolve_get_template_meta <- function(path) {
  if (exists("get_template_meta", mode = "function")) {
    return(get("get_template_meta", mode = "function")(path))
  }
  list()
}

resolve_render_template_tokens <- function(text, tokens) {
  if (exists("render_template_tokens", mode = "function")) {
    return(get("render_template_tokens", mode = "function")(text, tokens))
  }
  text
}

resolve_normalize_table_columns <- function(columns, default_specs) {
  if (exists("normalize_table_columns", mode = "function")) {
    return(get("normalize_table_columns", mode = "function")(columns, default_specs))
  }
  default_specs
}

resolve_drop_empty_columns <- function(columns, rows) {
  if (exists("drop_empty_columns", mode = "function")) {
    return(get("drop_empty_columns", mode = "function")(columns, rows))
  }
  list(columns = columns, rows = rows)
}

resolve_render_markdown_table <- function(headers, rows) {
  if (exists("render_markdown_table", mode = "function")) {
    return(get("render_markdown_table", mode = "function")(headers, rows))
  }
  ""
}

resolve_as_cell_text <- function(value) {
  if (exists("as_cell_text", mode = "function")) {
    return(get("as_cell_text", mode = "function")(value))
  }
  if (is.null(value) || length(value) == 0 || is.na(value)) return("")
  as.character(value)
}

resolve_append_apa_report <- function(path, analysis_label, apa_table, apa_text, analysis_flags = NULL, template_path = NULL, template_context = NULL) {
  if (exists("append_apa_report", mode = "function")) {
    return(get("append_apa_report", mode = "function")(
      path,
      analysis_label,
      apa_table,
      apa_text,
      analysis_flags = analysis_flags,
      template_path = template_path,
      template_context = template_context
    ))
  }
  stop("Missing append_apa_report. Ensure lib/formatting.R is sourced.")
}

resolve_get_run_context <- function() {
  if (exists("get_run_context", mode = "function")) {
    return(get("get_run_context", mode = "function")())
  }
  trailing <- commandArgs(trailingOnly = TRUE)
  commands <- c("Rscript", trailing)
  commands <- commands[nzchar(commands)]
  prompt <- paste(commands, collapse = " ")
  list(prompt = prompt, commands = commands)
}

resolve_append_analysis_log <- function(out_dir, module, prompt, commands, results, options = list(), user_prompt = NULL) {
  if (exists("append_analysis_log", mode = "function")) {
    return(get("append_analysis_log", mode = "function")(
      out_dir,
      module,
      prompt,
      commands,
      results,
      options = options,
      user_prompt = user_prompt
    ))
  }
  cat("Note: append_analysis_log not available; skipping analysis_log.jsonl output.\n")
  invisible(FALSE)
}

resolve_update_workspace_manifest <- function(workspace_root, summary_df, active_dataset = NULL) {
  if (exists("update_workspace_manifest", mode = "function")) {
    return(get("update_workspace_manifest", mode = "function")(
      workspace_root,
      summary_df,
      active_dataset = active_dataset
    ))
  }
  stop("Missing update_workspace_manifest. Ensure lib/io.R is sourced.")
}

resolve_get_user_prompt <- function(opts) {
  if (exists("get_user_prompt", mode = "function")) {
    return(get("get_user_prompt", mode = "function")(opts))
  }
  NULL
}

resolve_agent_default <- function() {
  config_agent <- resolve_config_value("modules.init_workspace.agent", NULL)
  if (!is.null(config_agent) && nzchar(config_agent)) return(as.character(config_agent))
  env_agent <- Sys.getenv("CODEX_AGENT", unset = "")
  if (nzchar(env_agent)) return(env_agent)
  "Codex"
}

trim_empty <- function(values) {
  values <- values[!is.na(values)]
  values <- values[nzchar(values)]
  values
}

parse_paths <- function(value) {
  if (is.null(value)) return(character(0))
  if (is.character(value) && length(value) > 1) {
    values <- value
  } else {
    values <- resolve_parse_list(value, sep = ",")
  }
  values <- trimws(as.character(values))
  trim_empty(values)
}

normalize_path <- function(path) {
  if (is.null(path) || !nzchar(path)) return("")
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

sanitize_file_component <- function(value) {
  clean <- gsub("[^A-Za-z0-9._-]", "_", as.character(value))
  clean <- gsub("_+", "_", clean)
  if (!nzchar(clean)) clean <- "dataset"
  clean
}

make_unique_labels <- function(labels) {
  seen <- list()
  out <- character(length(labels))
  for (i in seq_along(labels)) {
    base <- labels[i]
    count <- if (!is.null(seen[[base]])) seen[[base]] else 0
    count <- count + 1
    seen[[base]] <- count
    out[i] <- if (count == 1) base else paste0(base, "_", count)
  }
  out
}

escape_yaml_value <- function(value) {
  if (is.null(value)) return("")
  val <- as.character(value)
  val <- gsub("\"", "\\\\\"", val)
  val
}

build_yaml_front_matter <- function(info) {
  lines <- c(
    "---",
    paste0("created_at: \"", escape_yaml_value(info$created_at), "\""),
    paste0("path: \"", escape_yaml_value(info$workspace_path), "\""),
    paste0("os: \"", escape_yaml_value(info$os), "\""),
    paste0("r_version: \"", escape_yaml_value(info$r_version), "\""),
    paste0("agent: \"", escape_yaml_value(info$agent), "\""),
    "---"
  )
  paste(lines, collapse = "\n")
}

format_os_string <- function() {
  info <- Sys.info()
  if (!is.null(info) && !is.na(info["sysname"])) {
    parts <- c(info["sysname"], info["release"])
    parts <- parts[!is.na(parts) & nzchar(parts)]
    return(paste(parts, collapse = " "))
  }
  R.version$platform
}

build_env_info <- function(out_dir, agent_override = NULL) {
  created_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  agent_default <- resolve_agent_default()
  agent <- if (!is.null(agent_override) && nzchar(agent_override)) as.character(agent_override) else agent_default
  list(
    created_at = created_at,
    workspace_path = normalize_path(out_dir),
    os = format_os_string(),
    r_version = R.version.string,
    agent = agent
  )
}

format_type_label <- function(type) {
  type <- tolower(as.character(type))
  if (type == "csv") return("CSV")
  if (type == "sav") return("SAV")
  if (type == "rds") return("RDS")
  if (type == "rdata") return("RData")
  if (type == "parquet") return("Parquet")
  toupper(type)
}

build_dataset_specs <- function(opts, sep, header) {
  specs <- list()
  csv_paths <- parse_paths(opts$csv)
  sav_paths <- parse_paths(opts$sav)
  rds_paths <- parse_paths(opts$rds)
  rdata_paths <- parse_paths(opts$rdata)
  parquet_paths <- parse_paths(opts$parquet)

  if (length(csv_paths) > 0) {
    for (path in csv_paths) {
      specs[[length(specs) + 1]] <- list(type = "csv", path = path, sep = sep, header = header)
    }
  }
  if (length(sav_paths) > 0) {
    for (path in sav_paths) {
      specs[[length(specs) + 1]] <- list(type = "sav", path = path)
    }
  }
  if (length(rds_paths) > 0) {
    for (path in rds_paths) {
      specs[[length(specs) + 1]] <- list(type = "rds", path = path)
    }
  }
  if (length(rdata_paths) > 0) {
    df_names <- parse_paths(opts$df)
    if (length(df_names) == 0) {
      stop("--df is required when using --rdata")
    }
    if (length(df_names) == 1 && length(rdata_paths) > 1) {
      df_names <- rep(df_names, length(rdata_paths))
    }
    if (length(df_names) != length(rdata_paths)) {
      stop("--df must match the number of --rdata paths (or provide a single name).")
    }
    for (i in seq_along(rdata_paths)) {
      specs[[length(specs) + 1]] <- list(type = "rdata", path = rdata_paths[i], df = df_names[i])
    }
  }
  if (length(parquet_paths) > 0) {
    for (path in parquet_paths) {
      specs[[length(specs) + 1]] <- list(type = "parquet", path = path)
    }
  }
  specs
}

derive_spec_label <- function(spec) {
  if (!is.null(spec$df) && nzchar(spec$df)) return(as.character(spec$df))
  base <- tools::file_path_sans_ext(basename(spec$path))
  if (!nzchar(base)) base <- "dataset"
  base
}

load_dataset <- function(spec) {
  if (spec$type == "csv") {
    opts <- list(csv = spec$path, sep = spec$sep, header = spec$header)
    return(resolve_load_dataframe(opts))
  }
  if (spec$type == "sav") {
    opts <- list(sav = spec$path)
    return(resolve_load_dataframe(opts))
  }
  if (spec$type == "rds") {
    opts <- list(rds = spec$path)
    return(resolve_load_dataframe(opts))
  }
  if (spec$type == "rdata") {
    opts <- list(rdata = spec$path, df = spec$df)
    return(resolve_load_dataframe(opts))
  }
  if (spec$type == "parquet") {
    opts <- list(parquet = spec$path)
    return(resolve_load_dataframe(opts))
  }
  stop("Unsupported dataset type.")
}

prepare_dataset_outputs <- function(specs, out_dir) {
  if (length(specs) == 0) {
    empty_df <- data.frame(
      dataset = character(0),
      type = character(0),
      source_path = character(0),
      rows = integer(0),
      columns = integer(0),
      copy_path = character(0),
      stringsAsFactors = FALSE
    )
    return(list(summary_df = empty_df, labels = character(0)))
  }

  labels <- vapply(specs, derive_spec_label, character(1))

  summary_rows <- list()
  for (i in seq_along(specs)) {
    spec <- specs[[i]]
    label <- labels[i]
    df <- load_dataset(spec)
    copy_path <- attr(df, "workspace_parquet_path")
    if (is.null(copy_path) || !nzchar(copy_path)) {
      copy_info <- resolve_build_workspace_copy_info(label)
      copy_path <- copy_info$copy_path
    }
    summary_rows[[length(summary_rows) + 1]] <- data.frame(
      dataset = label,
      type = format_type_label(spec$type),
      source_path = normalize_path(spec$path),
      rows = ifelse(is.null(nrow(df)), NA_integer_, nrow(df)),
      columns = ifelse(is.null(ncol(df)), NA_integer_, ncol(df)),
      copy_path = normalize_path(copy_path),
      stringsAsFactors = FALSE
    )
  }
  summary_df <- do.call(rbind, summary_rows)
  list(summary_df = summary_df, labels = labels)
}

build_dataset_section <- function(label, source_path, type_label, rows, columns, copy_path) {
  rows_text <- ifelse(is.na(rows), "NA", as.character(rows))
  columns_text <- ifelse(is.na(columns), "NA", as.character(columns))
  copy_line <- if (nzchar(copy_path)) paste0("- Parquet copy: `", copy_path, "`\n") else ""
  paste0(
    "# ", label, "\n\n",
    "- Source: `", source_path, "`\n",
    "- Type: ", type_label, "\n",
    "- Dimensions: n = ", rows_text, ", p = ", columns_text, "\n",
    copy_line,
    "\n",
    "## Analysis Plan\n",
    "- [ ] Define analysis goals for ", label, "\n",
    "  - [ ] Specify variables and roles (IV/DV/covariates)\n",
    "  - [ ] Choose statistical tests\n",
    "- [ ] Prepare data\n",
    "  - [ ] Inspect missingness\n",
    "  - [ ] Recode items\n",
    "  - [ ] Compute scale scores\n",
    "  - [ ] Transform variables\n",
    "- [ ] Run analyses\n",
    "  - [ ] Check assumptions\n",
    "  - [ ] Execute models/tests\n",
    "- [ ] Report results\n",
    "  - [ ] Draft APA table and narrative\n",
    "  - [ ] Record decisions here\n",
    "\n",
    "## To Be Considered\n",
    "- Missing data handling:\n",
    "- Item recodings:\n",
    "- Generated scales:\n",
    "- Transformations:\n",
    "- Notes:\n"
  )
}

build_dataset_sections <- function(summary_df) {
  if (nrow(summary_df) == 0) {
    return(build_dataset_section(
      "Dataset",
      "(not specified)",
      "(not specified)",
      NA_integer_,
      NA_integer_,
      ""
    ))
  }
  sections <- vapply(seq_len(nrow(summary_df)), function(i) {
    row <- summary_df[i, ]
    build_dataset_section(
      row$dataset,
      ifelse(is.na(row$source_path), "", row$source_path),
      ifelse(is.na(row$type), "", row$type),
      row$rows,
      row$columns,
      ifelse(is.na(row$copy_path), "", row$copy_path)
    )
  }, character(1))
  paste(sections, collapse = "\n\n")
}

build_output_targets <- function(summary_df, workspace_root) {
  targets <- list()
  if (nrow(summary_df) == 0) {
    placeholder_label <- "workspace"
    placeholder_dir <- resolve_ensure_out_dir(file.path(workspace_root, sanitize_file_component(placeholder_label)))
    targets[[1]] <- list(
      label = placeholder_label,
      out_dir = placeholder_dir,
      summary_df = summary_df[0, , drop = FALSE],
      dataset_labels = character(0)
    )
    return(targets)
  }
  for (i in seq_len(nrow(summary_df))) {
    row <- summary_df[i, , drop = FALSE]
    copy_path <- as.character(row$copy_path)
    dataset_dir <- if (!is.na(copy_path) && nzchar(copy_path)) normalize_path(dirname(copy_path)) else ""
    if (!nzchar(dataset_dir)) {
      copy_info <- resolve_build_workspace_copy_info(as.character(row$dataset))
      dataset_dir <- normalize_path(copy_info$out_dir)
    }
    targets[[length(targets) + 1]] <- list(
      label = as.character(row$dataset),
      out_dir = resolve_ensure_out_dir(dataset_dir),
      summary_df = row,
      dataset_labels = as.character(row$dataset)
    )
  }
  targets
}

build_workspace_table_body <- function(summary_df, table_meta) {
  default_specs <- list(
    list(key = "dataset", label = "Dataset", drop_if_empty = FALSE),
    list(key = "type", label = "Type", drop_if_empty = FALSE),
    list(key = "rows", label = "Rows", drop_if_empty = FALSE),
    list(key = "columns", label = "Columns", drop_if_empty = FALSE),
    list(key = "source_path", label = "Source", drop_if_empty = TRUE),
    list(key = "copy_path", label = "Parquet copy", drop_if_empty = TRUE)
  )
  columns <- resolve_normalize_table_columns(table_meta$columns, default_specs)
  rows <- list()
  if (nrow(summary_df) == 0) {
    summary_df <- data.frame(
      dataset = "None",
      type = "",
      rows = "",
      columns = "",
      source_path = "",
      copy_path = "",
      stringsAsFactors = FALSE
    )
  }
  for (i in seq_len(nrow(summary_df))) {
    row <- summary_df[i, ]
    row_map <- list(
      dataset = row$dataset,
      type = row$type,
      rows = row$rows,
      columns = row$columns,
      source_path = row$source_path,
      copy_path = row$copy_path
    )
    row_cells <- lapply(columns, function(col) {
      resolve_as_cell_text(row_map[[col$key]])
    })
    rows[[length(rows) + 1]] <- row_cells
  }
  cleaned <- resolve_drop_empty_columns(columns, rows)
  headers <- vapply(cleaned$columns, function(col) col$label, character(1))
  body <- resolve_render_markdown_table(headers, cleaned$rows)
  list(body = body, columns = cleaned$columns)
}

build_apa_table <- function(table_body, note_text) {
  header <- "Table 1\nWorkspace initialization\n"
  body <- trimws(table_body)
  if (!nzchar(body)) {
    body <- "| Dataset | Type |\n| --- | --- |\n| None | |\n"
  }
  paste0(header, "\n", body, "\n\nNote. ", note_text, "\n")
}

build_apa_text <- function(info, labels) {
  count <- length(labels)
  if (count == 0) {
    return(paste0(
      "Workspace initialized at ", info$created_at, ". ",
      "Output directory: ", info$workspace_path, ". ",
      "No datasets were provided."
    ))
  }
  list_text <- paste(labels, collapse = ", ")
  paste0(
    "Workspace initialized at ", info$created_at, ". ",
    "Output directory: ", info$workspace_path, ". ",
    "Dataset copies saved for: ", list_text, "."
  )
}

args <- commandArgs(trailingOnly = TRUE)
opts <- resolve_parse_args(args)

if (!is.null(opts$help)) {
  print_usage()
  quit(status = 0)
}

if (!is.null(opts$interactive)) {
  opts <- modifyList(opts, interactive_options())
}

sep_default <- resolve_config_value("defaults.csv.sep", ",")
header_default <- resolve_config_value("defaults.csv.header", TRUE)
sep <- if (!is.null(opts$sep)) as.character(opts$sep) else sep_default
header <- resolve_parse_bool(opts$header, default = header_default)

specs <- build_dataset_specs(opts, sep, header)
workspace_root <- resolve_ensure_out_dir(resolve_default_out())

dataset_outputs <- prepare_dataset_outputs(specs, workspace_root)
summary_df <- dataset_outputs$summary_df
targets <- build_output_targets(summary_df, workspace_root)

scratchpad_template <- resolve_get_template_path(
  "init_workspace.scratchpad",
  "scratchpad/default-template.md"
)
if (is.null(scratchpad_template) || !file.exists(scratchpad_template)) {
  stop("Scratchpad template not found: ", scratchpad_template)
}
scratchpad_template_text <- paste(readLines(scratchpad_template, warn = FALSE), collapse = "\n")

template_path <- resolve_get_template_path(
  "init_workspace.default",
  "init-workspace/default-template.md"
)
if (is.null(template_path) || !file.exists(template_path)) {
  stop("APA template not found: ", template_path)
}
template_meta <- resolve_get_template_meta(template_path)

log_default <- resolve_config_value("defaults.log", TRUE)
ctx <- resolve_get_run_context()

for (target in targets) {
  env_info <- build_env_info(target$out_dir, opts$agent)
  yaml_front_matter <- build_yaml_front_matter(env_info)
  dataset_labels <- target$dataset_labels

  scratchpad_text <- resolve_render_template_tokens(
    scratchpad_template_text,
    list(
      created_at = env_info$created_at,
      workspace_path = env_info$workspace_path,
      os = env_info$os,
      r_version = env_info$r_version,
      agent = env_info$agent,
      dataset_sections = build_dataset_sections(target$summary_df)
    )
  )
  scratchpad_path <- file.path(target$out_dir, "scratchpad.md")
  scratchpad_con <- file(scratchpad_path, open = "w", encoding = "UTF-8")
  writeLines(scratchpad_text, scratchpad_con)
  close(scratchpad_con)

  table_result <- build_workspace_table_body(target$summary_df, template_meta$table)
  note_text <- if (nrow(target$summary_df) == 0) {
    "No datasets provided; workspace created without data copies."
  } else {
    "Dataset copy saved as .parquet in the dataset workspace."
  }
  apa_table <- build_apa_table(table_result$body, note_text)
  apa_text <- build_apa_text(env_info, dataset_labels)

  analysis_flags <- list(
    datasets = if (length(dataset_labels) == 0) "None" else dataset_labels
  )
  template_context <- list(
    tokens = list(
      yaml_front_matter = yaml_front_matter,
      created_at = env_info$created_at,
      workspace_path = env_info$workspace_path,
      os = env_info$os,
      r_version = env_info$r_version,
      agent = env_info$agent,
      dataset_count = as.character(length(dataset_labels)),
      dataset_list = if (length(dataset_labels) == 0) "None" else paste(dataset_labels, collapse = ", "),
      table_body = table_result$body,
      narrative_default = apa_text
    )
  )

  resolve_append_apa_report(
    file.path(target$out_dir, "apa_report.md"),
    "Workspace initialization",
    apa_table,
    apa_text,
    analysis_flags = analysis_flags,
    template_path = template_path,
    template_context = template_context
  )

  if (resolve_parse_bool(opts$log, default = log_default)) {
    resolve_append_analysis_log(
      target$out_dir,
      module = "init_workspace",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(
        workspace_dir = env_info$workspace_path,
        scratchpad_path = normalize_path(scratchpad_path),
        apa_report_path = normalize_path(file.path(target$out_dir, "apa_report.md")),
        datasets = target$summary_df
      ),
      options = list(
        csv = parse_paths(opts$csv),
        sav = parse_paths(opts$sav),
        rds = parse_paths(opts$rds),
        rdata = parse_paths(opts$rdata),
        parquet = parse_paths(opts$parquet),
        df = parse_paths(opts$df),
        sep = sep,
        header = header,
        agent = env_info$agent
      ),
      user_prompt = resolve_get_user_prompt(opts)
    )
  }
}

resolve_update_workspace_manifest(workspace_root, summary_df)
