# SPDX-License-Identifier: Apache-2.0
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

source(file.path(bootstrap_dir, "lib", "bootstrap.R"))
nlss_bootstrap()
source_lib("utility_contract.R")
source_lib("data_change.R")

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
  print_import_usage()
  cat("  --agent TEXT           Agent name (default from config or CODEX_AGENT)\n")
  cat("  --template REF         Template path or template key (optional)\n")
  cat("  --user-prompt TEXT     Original AI user prompt for logging (optional)\n")
  cat("  --log TRUE/FALSE       Write analysis_log.jsonl (default: TRUE)\n")
  cat("  --interactive          Prompt for inputs\n")
  cat("  --help                 Show this help\n")
}

interactive_options <- function() {
  cat("Interactive input selected.\n")
  opts <- list()
  opts$csv <- prompt("CSV path(s) (comma-separated, blank for none)", "")
  if (nzchar(opts$csv)) {
    sep_default <- get_config_value("defaults.csv.sep", ",")
    header_default <- get_config_value("defaults.csv.header", TRUE)
    opts$sep <- prompt("Separator", sep_default)
    opts$header <- prompt("Header TRUE/FALSE", ifelse(isTRUE(header_default), "TRUE", "FALSE"))
  }
  opts$sav <- prompt("SAV path(s) (comma-separated, blank for none)", "")
  opts$rds <- prompt("RDS path(s) (comma-separated, blank for none)", "")
  opts$rdata <- prompt("RData path(s) (comma-separated, blank for none)", "")
  if (nzchar(opts$rdata)) {
    opts$df <- prompt("RData data frame name(s), comma-separated", "")
  }
  opts$parquet <- prompt("Parquet path(s) (comma-separated, blank for none)", "")
  agent_default <- resolve_agent_default()
  opts$agent <- prompt("Agent name", agent_default)
  opts$template <- prompt("Template (path or key; blank for default)", "")
  opts$`user-prompt` <- prompt("User prompt (optional)", "")
  log_default <- get_config_value("defaults.log", TRUE)
  opts$log <- prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))
  opts
}

resolve_agent_default <- function() {
  config_agent <- get_config_value("modules.init_workspace.agent", NULL)
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
    values <- parse_list(value, sep = ",")
  }
  values <- trimws(as.character(values))
  values <- trim_empty(values)
  values <- vapply(values, normalize_input_path, character(1))
  trim_empty(values)
}

build_yaml_front_matter <- function(info) {
  paste0("---\n", yaml::as.yaml(list(created_at = info$created_at, path = info$workspace_path,
    os = info$os, r_version = info$r_version, agent = info$agent,
    nlss_version = info$nlss_version)), "---")
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

build_env_info <- function(out_dir, agent_override = NULL, workspace_root = NULL) {
  created_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  agent_default <- resolve_agent_default()
  agent <- if (!is.null(agent_override) && nzchar(agent_override)) as.character(agent_override) else agent_default
  nlss_version <- get_nlss_version()
  if (is.na(nlss_version)) nlss_version <- ""
  list(
    created_at = created_at,
    workspace_path = render_output_path(out_dir, workspace_root = workspace_root),
    os = format_os_string(),
    r_version = R.version.string,
    agent = agent,
    nlss_version = nlss_version
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
  if (!is.null(opts$`dataset-name`) && length(specs) != 1L) stop("--dataset-name requires exactly one input file.")
  shared <- c("dataset-name", "import-action", "csv-decimal", "csv-encoding", "csv-col-types", "csv-na-values")
  for (i in seq_along(specs)) specs[[i]]$import_options <- opts[intersect(shared, names(opts))]
  specs
}

derive_spec_label <- function(spec) {
  if (!is.null(spec$import_options$`dataset-name`)) return(spec$import_options$`dataset-name`)
  if (!is.null(spec$df) && nzchar(spec$df)) return(as.character(spec$df))
  base <- tools::file_path_sans_ext(basename(spec$path))
  if (!nzchar(base)) base <- "dataset"
  base
}

dataset_options <- function(spec) {
  if (!spec$type %in% c("csv", "sav", "rds", "rdata", "parquet")) stop("Unsupported dataset type.")
  opts <- spec$import_options
  if (is.null(opts)) opts <- list()
  opts[[spec$type]] <- spec$path
  if (spec$type == "csv") { opts$sep <- spec$sep; opts$header <- spec$header }
  if (spec$type == "rdata") opts$df <- spec$df
  if (spec$type == "csv") {
    for (key in c("decimal", "encoding", "col_types", "na_values")) {
      flag <- paste0("csv-", gsub("_", "-", key))
      if (is.null(opts[[flag]])) opts[[flag]] <- get_config_value(paste0("defaults.csv.", key))
    }
  }
  opts
}

preflight_datasets <- function(specs, root) {
  labels <- vapply(specs, derive_spec_label, character(1))
  if (any(!nzchar(labels) | labels %in% c(".", ".."))) stop("Invalid dataset name.")
  folders <- vapply(labels, sanitize_file_component, character(1))
  if (anyDuplicated(folders)) stop("Input names collide after sanitization; initialize separately with distinct --dataset-name values.")
  for (i in seq_along(specs)) {
    spec <- specs[[i]]
    opts <- dataset_options(spec)
    path <- file.path(root, folders[i], paste0(folders[i], ".parquet"))
    directory <- dirname(path)
    # Validate every target before the first import, including old source/version
    # directories, which the shared importer may update or reuse.
    files <- c(path, file.path(directory, c("scratchpad.md", "report_canonical.md", "analysis_log.jsonl", "import.json", "dictionary.json", "codebook.md")),
      file.path(directory, c("sources/.path-check", "versions/.path-check")),
      unlist(lapply(file.path(directory, c("sources", "versions")), list.files,
        recursive = TRUE, full.names = TRUE, all.files = TRUE), use.names = FALSE))
    for (target in files) nlss_data_change_path(target, root)
    if (dir.exists(file.path(directory, ".import-lock"))) stop("Dataset import is locked; initialization did not import any files.")
    if (file.exists(file.path(directory, ".nlss-planning.json"))) stop("Dataset name is reserved for parameter-only planning.")
    if (!file.exists(spec$path) || dir.exists(spec$path)) stop("Input file not found: ", spec$path)
    action <- if (is.null(opts$`import-action`)) "verify" else opts$`import-action`
    if (!action %in% c("verify", "new-version")) stop("--import-action must be verify or new-version.")
    working <- identical(spec$type, "parquet") && identical(normalize_path(spec$path), normalize_path(path))
    if (!working && file.exists(path)) {
      binding <- read_import_json(file.path(directory, "import.json"))
      descriptor <- import_source_descriptor(spec$path, spec$type, opts)
      if (is.null(binding) && action != "new-version") stop("Existing data have no verified source binding; use the working --parquet or explicit --import-action new-version.")
      if (!is.null(binding)) {
        if (!identical(binding$source$source_key, descriptor$source_key)) stop("Dataset name is already bound to a different source; use a distinct --dataset-name.")
        if (action != "new-version" && ((!is.null(binding$state) && !identical(binding$state, "ready")) ||
            !identical(binding$source$source_sha256, descriptor$source_sha256) ||
            !identical(binding$source$options_sha256, descriptor$options_sha256) ||
            !identical(binding$source$format, descriptor$format))) stop("Source contents or import options changed; use explicit --import-action new-version.")
      }
    }
    candidate <- switch(spec$type, csv = import_csv(spec$path, opts), sav = read_sav_data(spec$path),
      rds = readRDS(spec$path), rdata = load_rdata_frame(spec$path, spec$df)$df,
      parquet = read_parquet_data(spec$path))
    if (!is.data.frame(candidate)) stop("Input does not contain a data frame: ", basename(spec$path))
    import_prepare_analysis(candidate)
  }
  invisible(NULL)
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
    df <- load_dataframe(dataset_options(spec))
    copy_path <- attr(df, "workspace_parquet_path")
    if (is.null(copy_path) || !nzchar(copy_path)) {
      copy_info <- build_workspace_copy_info(label)
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
    "  - [ ] Draft NLSS format table and narrative\n",
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

build_dataset_sections <- function(summary_df, workspace_root = NULL) {
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
      ifelse(is.na(row$source_path), "", render_log_path(row$source_path, workspace_root = workspace_root)),
      ifelse(is.na(row$type), "", row$type),
      row$rows,
      row$columns,
      ifelse(is.na(row$copy_path), "", render_log_path(row$copy_path, workspace_root = workspace_root))
    )
  }, character(1))
  paste(sections, collapse = "\n\n")
}

build_output_targets <- function(summary_df, workspace_root) {
  targets <- list()
  if (nrow(summary_df) == 0) {
    placeholder_label <- "workspace"
    placeholder_dir <- ensure_out_dir(file.path(workspace_root, sanitize_file_component(placeholder_label)))
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
      copy_info <- build_workspace_copy_info(as.character(row$dataset))
      dataset_dir <- normalize_path(copy_info$out_dir)
    }
    targets[[length(targets) + 1]] <- list(
      label = as.character(row$dataset),
      out_dir = ensure_out_dir(dataset_dir),
      summary_df = row,
      dataset_labels = as.character(row$dataset)
    )
  }
  targets
}

build_workspace_table_body <- function(summary_df, table_meta, workspace_root = NULL) {
  default_specs <- list(
    list(key = "dataset", label = "Dataset", drop_if_empty = FALSE),
    list(key = "type", label = "Type", drop_if_empty = FALSE),
    list(key = "rows", label = "Rows", drop_if_empty = FALSE),
    list(key = "columns", label = "Columns", drop_if_empty = FALSE),
    list(key = "source_path", label = "Source", drop_if_empty = TRUE),
    list(key = "copy_path", label = "Parquet copy", drop_if_empty = TRUE)
  )
  columns <- normalize_table_columns(table_meta$columns, default_specs)
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
      source_path = render_log_path(row$source_path, workspace_root = workspace_root),
      copy_path = render_log_path(row$copy_path, workspace_root = workspace_root)
    )
    row_cells <- lapply(columns, function(col) {
      as_cell_text(row_map[[col$key]])
    })
    rows[[length(rows) + 1]] <- row_cells
  }
  cleaned <- drop_empty_columns(columns, rows)
  headers <- vapply(cleaned$columns, function(col) col$label, character(1))
  body <- render_markdown_table(headers, cleaned$rows)
  list(body = body, columns = cleaned$columns)
}

build_nlss_table <- function(table_body, note_text) {
  header <- "Table 1\nWorkspace initialization\n"
  body <- trimws(table_body)
  if (!nzchar(body)) {
    body <- "| Dataset | Type |\n| --- | --- |\n| None | |\n"
  }
  paste0(header, "\n", body, "\n\nNote. ", note_text, "\n")
}

build_nlss_text <- function(info, labels) {
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
opts <- parse_args(args)

if (parse_bool(opts$help, FALSE)) {
  print_usage()
  quit(status = 0)
}

if (parse_bool(opts$interactive, default = get_config_value("defaults.interactive", FALSE))) {
  opts <- modifyList(opts, interactive_options())
}

sep_default <- get_config_value("defaults.csv.sep", ",")
header_default <- get_config_value("defaults.csv.header", TRUE)
sep <- if (!is.null(opts$sep)) as.character(opts$sep) else sep_default
header <- parse_bool(opts$header, default = header_default)

specs <- build_dataset_specs(opts, sep, header)

scratchpad_template <- resolve_template_path(
  "init_workspace.scratchpad",
  "scratchpad/default-template.md"
)
if (is.null(scratchpad_template) || !file.exists(scratchpad_template)) {
  stop("Scratchpad template not found: ", scratchpad_template)
}
scratchpad_template_text <- paste(readLines(scratchpad_template, warn = FALSE), collapse = "\n")

template_override <- resolve_template_override(opts$template, module = "init_workspace")
if (!is.null(opts$template) && nzchar(trimws(opts$template)) && is.null(template_override)) stop("Requested initialization template was not found.")
template_path <- if (!is.null(template_override)) {
  template_override
} else {
  resolve_template_path(
    "init_workspace.default",
    "init-workspace/default-template.md"
  )
}
if (is.null(template_path) || !file.exists(template_path)) {
  stop("NLSS format template not found: ", template_path)
}
template_meta <- get_template_meta(template_path)

nlss_utility_check_directory(get_default_out())
workspace_root <- normalize_path(ensure_out_dir(get_default_out()))
manifest_path <- file.path(workspace_root, get_workspace_manifest_name())
validate_workspace_manifest_path(manifest_path)
nlss_data_change_path(manifest_path, workspace_root)
if (dir.exists(file.path(workspace_root, ".publication-lock"))) stop("Project publication is locked; initialization did not import any files.")
preflight_datasets(specs, workspace_root)
dataset_outputs <- tryCatch(prepare_dataset_outputs(specs, workspace_root), error = function(e) {
  stop(conditionMessage(e), "\nInitialization stopped. Earlier per-dataset imports may already be preserved; no initialization report was published.", call. = FALSE)
})
summary_df <- dataset_outputs$summary_df
targets <- build_output_targets(summary_df, workspace_root)
references <- lapply(targets, function(target) get_dataset_reference(target$out_dir))
projection_targets <- c(manifest_path, unlist(lapply(targets, function(target)
  file.path(target$out_dir, c("scratchpad.md", "report_canonical.md", "analysis_log.jsonl"))), use.names = FALSE))

log_default <- get_config_value("defaults.log", TRUE)
ctx <- get_run_context()
nlss_publish_utility("init_workspace", workspace_root,
  request = list(event = "workspace_initialization", datasets = references,
    import_commit_scope = "per_dataset", semantic_report_regeneration = FALSE,
    agent = if (is.null(opts$agent)) resolve_agent_default() else opts$agent, log = parse_bool(opts$log, log_default)),
  results = list(dataset_count = nrow(summary_df), datasets = references,
    scratchpads = lapply(targets, function(target) list(path = make_relative_path(file.path(target$out_dir, "scratchpad.md"), workspace_root),
      action = if (file.exists(file.path(target$out_dir, "scratchpad.md"))) "preserved" else "created"))),
  output = c("# Workspace initialization", "",
    paste0("Datasets: ", if (nrow(summary_df)) paste(summary_df$dataset, collapse = ", ") else "none (planning placeholder)."),
    "Sources and exact working versions are recorded in request.json. Existing scratchpads are preserved.",
    "Imports commit per dataset; this lifecycle event is not a statistical replay."),
  artifacts = list("report-template.md" = readBin(template_path, "raw", n = file.info(template_path)$size),
    "scratchpad-template.md" = readBin(scratchpad_template, "raw", n = file.info(scratchpad_template)$size)),
  targets = projection_targets,
  publish = function(run_id, staging) {
# Establish the protected manifest before logging so even the first import's
# command/options paths are rendered relative to the project or masked external.
invisible(update_workspace_manifest(workspace_root, summary_df))
for (target in targets) {
  env_info <- build_env_info(target$out_dir, opts$agent, workspace_root = workspace_root)
  yaml_front_matter <- build_yaml_front_matter(env_info)
  dataset_labels <- target$dataset_labels

  scratchpad_text <- render_template_tokens(
    scratchpad_template_text,
    list(
      created_at = env_info$created_at,
      workspace_path = env_info$workspace_path,
      os = env_info$os,
      r_version = env_info$r_version,
      agent = env_info$agent,
      nlss_version = env_info$nlss_version,
      dataset_sections = build_dataset_sections(target$summary_df, workspace_root = workspace_root)
    )
  )
  scratchpad_path <- file.path(target$out_dir, "scratchpad.md")
  if (!file.exists(scratchpad_path)) {
    writeLines(scratchpad_text, scratchpad_path, useBytes = TRUE)
  }

  table_result <- build_workspace_table_body(target$summary_df, template_meta$table, workspace_root = workspace_root)
  note_text <- if (nrow(target$summary_df) == 0) {
    "No datasets provided; workspace created without data copies."
  } else {
    "Dataset copy saved as .parquet in the dataset workspace."
  }
  nlss_table <- build_nlss_table(table_result$body, note_text)
  nlss_text <- build_nlss_text(env_info, dataset_labels)

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
      nlss_version = env_info$nlss_version,
      dataset_count = as.character(length(dataset_labels)),
      dataset_list = if (length(dataset_labels) == 0) "None" else paste(dataset_labels, collapse = ", "),
      table_body = table_result$body,
      narrative_default = nlss_text
    )
  )

  append_nlss_report(
    file.path(target$out_dir, "report_canonical.md"),
    "Workspace initialization",
    nlss_table,
    nlss_text,
    analysis_flags = analysis_flags,
    template_path = file.path(staging, "report-template.md"),
    template_context = template_context
  )

  if (parse_bool(opts$log, default = log_default)) {
    rendered_summary <- target$summary_df
    if (nrow(rendered_summary) > 0) {
      rendered_summary$source_path <- vapply(
        rendered_summary$source_path,
        function(p) render_output_path(p, workspace_root = workspace_root),
        character(1)
      )
      rendered_summary$copy_path <- vapply(
        rendered_summary$copy_path,
        function(p) render_output_path(p, workspace_root = workspace_root),
        character(1)
      )
    }
    logged <- append_analysis_log(
      target$out_dir,
      module = "init_workspace",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(
        utility_run_id = run_id,
        dataset = get_dataset_reference(target$out_dir),
        workspace_dir = env_info$workspace_path,
        scratchpad_path = render_output_path(scratchpad_path, workspace_root = workspace_root),
        nlss_report_path = render_output_path(file.path(target$out_dir, "report_canonical.md"), workspace_root = workspace_root),
        datasets = rendered_summary
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
      user_prompt = get_user_prompt(opts)
    )
    if (resolve_logging_bool("enabled", TRUE) && !isTRUE(logged)) stop("Required initialization JSONL projection was not written.")
  }
}

})
