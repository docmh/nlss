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
  cat("Metaskill activation/finalization logger (base R)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript metaskill_runner.R --project study --dataset sample --meta sample-description\n")
  cat("  Rscript metaskill_runner.R --csv data.csv --meta sample-description\n")
  cat("  Rscript metaskill_runner.R --parquet data.parquet --meta sample-description --intent \"describe the sample\"\n")
  cat("  Rscript metaskill_runner.R --interactive\n")
  cat("\n")
  cat("Options:\n")
  cat("  --project PATH       Select a current project (default: nearest ancestor)\n")
  cat("  --dataset NAME       Select registered working data (default: active dataset)\n")
  cat("  --csv PATH           CSV input file\n")
  cat("  --sav PATH           SPSS .sav input file\n")
  cat("  --sep VALUE          CSV separator (default: ,)\n")
  cat("  --header TRUE/FALSE  CSV header (default: TRUE)\n")
  cat("  --rds PATH           RDS input file (data frame)\n")
  cat("  --rdata PATH         RData input file\n")
  cat("  --parquet PATH       Parquet input file\n")
  cat("  --df NAME            Data frame object name in RData\n")
  cat("  --meta NAME          Metaskill name (required)\n")
  cat("  --phase TEXT         Phase label (activation/finalization; optional)\n")
  cat("  --intent TEXT        Short intent summary (optional)\n")
  cat("  --notes TEXT         Additional notes (optional)\n")
  cat("  --synopsis TEXT      Synopsis text to include in finalization report (optional)\n")
  cat("  --label TEXT         Analysis label override (optional)\n")
  cat("  --template REF       Template path or template key (optional)\n")
  cat("  --user-prompt TEXT   Original AI user prompt for logging (optional)\n")
  cat("  --log TRUE/FALSE     Optional standalone JSONL; project evidence/protocol always saved\n")
  cat("  --interactive        Prompt for inputs\n")
  cat("  --help               Show this help\n")
}

interactive_options <- function() {
  cat("Interactive input selected.\n")
  input_type <- prompt("Input type (csv/sav/rds/rdata/parquet)", "csv")
  input_type <- tolower(input_type)
  opts <- list()

  if (input_type == "csv") {
    opts$csv <- prompt("CSV path")
    sep_default <- get_config_value("defaults.csv.sep", ",")
    header_default <- get_config_value("defaults.csv.header", TRUE)
    opts$sep <- prompt("Separator", sep_default)
    opts$header <- prompt("Header TRUE/FALSE", ifelse(isTRUE(header_default), "TRUE", "FALSE"))
  } else if (input_type == "sav") {
    opts$sav <- prompt("SAV path")
  } else if (input_type == "rds") {
    opts$rds <- prompt("RDS path")
  } else if (input_type == "rdata") {
    opts$rdata <- prompt("RData path")
    opts$df <- prompt("Data frame object name")
  } else if (input_type == "parquet") {
    opts$parquet <- prompt("Parquet path")
  } else {
    stop("Unsupported input type.")
  }

  opts$meta <- prompt("Metaskill name")
  opts$phase <- prompt("Phase (activation/finalization)", "activation")
  opts$intent <- prompt("Intent (optional)", "")
  opts$notes <- prompt("Notes (optional)", "")
  opts$synopsis <- prompt("Synopsis (optional)", "")
  opts$label <- prompt("Analysis label (optional)", "")
  opts$template <- prompt("Template (path or key; blank for default)", "")
  opts$`user-prompt` <- prompt("User prompt (optional)", "")
  log_default <- get_config_value("defaults.log", TRUE)
  opts$log <- prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))
  opts
}

slugify_component <- function(value, fallback) {
  if (is.null(value) || !nzchar(value)) return(fallback)
  clean <- sanitize_file_component(value)
  clean <- gsub("_+", "_", clean)
  if (!nzchar(clean)) clean <- fallback
  clean
}

build_report_paths <- function(out_dir, meta_name, intent) {
  meta_slug <- slugify_component(meta_name, "metaskill")
  intent_slug <- slugify_component(intent, "no-intent")
  local_stamp <- format(Sys.Date(), "%Y%m%d")
  utc_stamp <- format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y%m%d")
  stamps <- unique(c(local_stamp, utc_stamp))
  paths <- character(0)
  for (stamp in stamps) {
    paths <- c(paths, file.path(out_dir, paste0("report_", stamp, "_", meta_slug, "_", intent_slug, ".md")))
  }
  unique(paths)
}

is_finalization_phase <- function(phase) {
  if (is.null(phase)) return(FALSE)
  text <- tolower(trimws(as.character(phase)))
  if (!nzchar(text)) return(FALSE)
  text %in% c("finalization", "finalise", "finalize", "completion", "complete", "completed", "finish", "finished")
}

build_activation_rows <- function(meta_name, intent, dataset_label, timestamp, notes = "") {
  rows <- list()
  rows[[length(rows) + 1]] <- list(item = "Metaskill", value = meta_name)
  if (!is.null(intent) && nzchar(intent)) {
    rows[[length(rows) + 1]] <- list(item = "Intent", value = intent)
  }
  if (!is.null(dataset_label) && nzchar(dataset_label)) {
    rows[[length(rows) + 1]] <- list(item = "Dataset", value = dataset_label)
  }
  if (!is.null(timestamp) && nzchar(timestamp)) {
    rows[[length(rows) + 1]] <- list(item = "Timestamp", value = timestamp)
  }
  if (!is.null(notes) && nzchar(notes)) {
    rows[[length(rows) + 1]] <- list(item = "Notes", value = notes)
  }
  rows
}

build_activation_table_body <- function(rows, table_spec = NULL) {
  default_columns <- list(
    list(key = "item", label = "Item"),
    list(key = "value", label = "Value")
  )
  columns <- normalize_table_columns(
    if (!is.null(table_spec$columns)) table_spec$columns else NULL,
    default_columns
  )

  table_rows <- list()
  if (length(rows) == 0) {
    rows <- list(list(item = "Metaskill", value = ""))
  }
  for (row in rows) {
    row_vals <- character(0)
    for (col in columns) {
      key <- col$key
      val <- if (!is.null(row[[key]])) row[[key]] else ""
      row_vals <- c(row_vals, as_cell_text(val))
    }
    table_rows[[length(table_rows) + 1]] <- row_vals
  }

  filtered <- drop_empty_columns(columns, table_rows)
  columns <- filtered$columns
  table_rows <- filtered$rows
  headers <- vapply(columns, function(col) {
    if (!is.null(col$label) && nzchar(col$label)) col$label else col$key
  }, character(1))
  list(
    body = render_markdown_table(headers, table_rows),
    columns = vapply(columns, function(col) col$key, character(1))
  )
}

build_activation_table <- function(table_body, analysis_label, note_text) {
  header <- paste0("Table 1\n", analysis_label, "\n")
  note_line <- if (nzchar(note_text)) paste0("Note. ", note_text) else "Note."
  paste0(header, "\n", table_body, "\n\n", note_line, "\n")
}

build_activation_text <- function(meta_name, intent, dataset_label, timestamp, phase) {
  action <- if (is_finalization_phase(phase)) "finalized" else if (tolower(phase) == "activation") "activated" else paste0("phase '", phase, "' recorded")
  text <- paste0("Metaskill \"", meta_name, "\" ", action)
  if (!is.null(dataset_label) && nzchar(dataset_label)) {
    text <- paste0(text, " for dataset \"", dataset_label, "\"")
  }
  if (!is.null(intent) && nzchar(intent)) {
    text <- paste0(text, " (intent: ", intent, ")")
  }
  if (!is.null(timestamp) && nzchar(timestamp)) {
    text <- paste0(text, " at ", timestamp)
  }
  paste0(text, ".")
}

get_dataset_label <- function(df, opts) {
  label <- ""
  attr_label <- attr(df, "workspace_dataset")
  if (!is.null(attr_label) && nzchar(attr_label)) {
    label <- as.character(attr_label)
  }
  if (!nzchar(label)) {
    parquet_path <- attr(df, "workspace_parquet_path")
    if (!is.null(parquet_path) && nzchar(parquet_path)) {
      label <- tools::file_path_sans_ext(basename(parquet_path))
    }
  }
  if (!nzchar(label)) {
    source_path <- attr(df, "workspace_source_path")
    if (!is.null(source_path) && nzchar(source_path)) {
      label <- derive_dataset_label(source_path)
    }
  }
  if (!nzchar(label)) {
    if (!is.null(opts$parquet) && nzchar(opts$parquet)) {
      label <- derive_dataset_label(opts$parquet)
    } else if (!is.null(opts$csv) && nzchar(opts$csv)) {
      label <- derive_dataset_label(opts$csv)
    } else if (!is.null(opts$sav) && nzchar(opts$sav)) {
      label <- derive_dataset_label(opts$sav)
    } else if (!is.null(opts$rds) && nzchar(opts$rds)) {
      label <- derive_dataset_label(opts$rds)
    } else if (!is.null(opts$rdata) && nzchar(opts$rdata)) {
      label <- derive_dataset_label(opts$rdata, opts$df)
    }
  }
  sanitize_file_component(label)
}

main <- function() {
  # The shared loader may acquire input locks. Release only this invocation's
  # recorded locks on success/error, as the statistical entrypoint wrapper does.
  on.exit({
    if (!is.null(nlss_run_context$lock)) unlink(nlss_run_context$lock, recursive = TRUE)
    if (!is.null(nlss_run_context$import_lock)) unlink(nlss_run_context$import_lock, recursive = TRUE)
  }, add = TRUE)
args <- commandArgs(trailingOnly = TRUE)
opts <- parse_args(args)

if (parse_bool(opts$help, FALSE)) {
  print_usage()
  quit(save = "no", status = 0)
}

interactive_default <- get_config_value("defaults.interactive", FALSE)
if (parse_bool(opts$interactive, default = interactive_default)) {
  opts <- modifyList(opts, interactive_options())
}

meta_default <- get_config_value("modules.metaskill_runner.meta_default", "")
meta_name <- if (!is.null(opts$meta)) as.character(opts$meta) else meta_default
meta_name <- trimws(meta_name)
if (!nzchar(meta_name)) {
  stop("Metaskill name is required. Use --meta <name>.")
}
intent <- if (!is.null(opts$intent)) trimws(as.character(opts$intent)) else ""
notes <- if (!is.null(opts$notes)) trimws(as.character(opts$notes)) else ""
synopsis <- if (!is.null(opts$synopsis)) trimws(as.character(opts$synopsis)) else ""
phase <- if (!is.null(opts$phase)) trimws(as.character(opts$phase)) else ""
if (!nzchar(phase)) phase <- "activation"
analysis_label <- if (!is.null(opts$label)) trimws(as.character(opts$label)) else ""
if (!nzchar(analysis_label)) {
  analysis_label <- get_config_value("modules.metaskill_runner.analysis_label", "Metaskill activation")
  if (!nzchar(analysis_label)) {
    analysis_label <- "Metaskill activation"
  }
  phase_lower <- tolower(phase)
  if (analysis_label == "Metaskill activation" && phase_lower %in% c("finalization", "finalise", "finalize", "completion", "complete", "completed", "finish", "finished")) {
    analysis_label <- "Metaskill finalization"
  }
}
note_default <- get_config_value(
  "modules.metaskill_runner.note_default",
  "This entry logs metaskill activation/finalization only; analyses are logged separately."
)

template_override <- resolve_template_override(opts$template, module = "metaskill_runner")
if (!is.null(opts$template) && nzchar(trimws(opts$template)) && is.null(template_override)) stop("Requested metaskill template was not found.")
template_path <- if (!is.null(template_override)) {
  template_override
} else {
  template_key <- "metaskill_runner.default"
  template_fallback <- "metaskill-runner/default-template.md"
  if (is_finalization_phase(phase)) {
    template_key <- "metaskill_runner.finalization"
    template_fallback <- "metaskill-runner/finalization-template.md"
  }
  resolve_template_path(template_key, template_fallback)
}
if (is.null(template_path) || !file.exists(template_path)) {
  stop("NLSS format template not found: ", template_path)
}
template_meta <- get_template_meta(template_path)

# Resolve selections through the common read-only boundary before importing.
# Working data, managed evidence and visible authored reports are different
# locations; none is derived from an old manifest's entry$parquet field.
locations <- cli_resolve_locations(opts)
root <- normalize_path(if (is.null(locations$project_root)) locations$output_root else locations$project_root)
nlss_utility_check_directory(root)
formats <- c("csv", "sav", "rds", "rdata", "parquet")
selected <- formats[vapply(formats, function(key) !is.null(opts[[key]]), logical(1))]
if (length(selected) > 1L) stop("Specify exactly one input format.")
if (!is.null(locations$project_root)) {
  candidate_dir <- locations$output_root
  report_dir <- root
} else if (length(selected)) {
  format <- selected[1]
  label <- if (!is.null(opts$`dataset-name`)) opts$`dataset-name` else derive_dataset_label(opts[[format]], if (format == "rdata") opts$df else NULL)
  if (!nzchar(label) || label %in% c(".", "..")) stop("Invalid dataset name.")
  candidate_dir <- file.path(root, sanitize_file_component(label))
  report_dir <- candidate_dir
} else {
  stop("No input provided and no project selected. Use --project or an input file.")
}
nlss_utility_check_directory(candidate_dir)
metaskill_report_path <- ""
if (is_finalization_phase(phase)) {
  expected <- build_report_paths(report_dir, meta_name, intent)
  existing <- expected[file.exists(expected) & !dir.exists(expected)]
  if (!length(existing)) stop("Metaskill report is missing. Create the semantic report before finalizing. Expected: ", paste(basename(expected), collapse = ", "))
  metaskill_report_path <- existing[1]
  if (!file.info(metaskill_report_path)$size) stop("Metaskill report is empty; finalization requires an authored report.")
}
root <- normalize_path(ensure_out_dir(root))
if (nzchar(metaskill_report_path)) nlss_data_change_path(metaskill_report_path, root)
lock_root <- if (is.null(locations$project_root)) root else locations$output_root
if (dir.exists(file.path(lock_root, ".publication-lock"))) stop("Project publication is locked; metaskill output was not changed.")
metaskill_report_bytes <- if (nzchar(metaskill_report_path)) readBin(metaskill_report_path, "raw", n = file.info(metaskill_report_path)$size) else NULL
metaskill_report_text <- NULL
if (!is.null(metaskill_report_bytes)) {
  metaskill_report_text <- rawToChar(metaskill_report_bytes)
  if (!validUTF8(metaskill_report_text)) stop("Authored metaskill reports must use valid UTF-8 text.")
  Encoding(metaskill_report_text) <- "UTF-8"
}
df <- load_dataframe(opts)
dataset_label <- if (is.null(locations$dataset_name)) get_dataset_label(df, opts) else locations$dataset_name
out_dir <- get_workspace_out_dir(df, label = dataset_label)
if (!identical(normalize_path(out_dir), normalize_path(candidate_dir))) stop("Resolved metaskill dataset differs from its preflight target.")
dataset_reference <- attr(df, "nlss_dataset_ref")

timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
rows <- build_activation_rows(meta_name, intent, dataset_label, timestamp, notes = notes)
table_result <- build_activation_table_body(rows, template_meta$table)
nlss_table <- build_activation_table(table_result$body, analysis_label, note_default)
nlss_text <- build_activation_text(meta_name, intent, dataset_label, timestamp, phase)

analysis_flags <- list(
  metaskill = meta_name,
  intent = intent,
  dataset = dataset_label,
  phase = phase
)

template_context <- list(
  tokens = list(
    metaskill_name = meta_name,
    intent = intent,
    dataset = dataset_label,
    timestamp = timestamp,
    phase = phase,
    notes = notes,
    synopsis_text = synopsis
  )
)

# Mask incidental private paths only in lifecycle displays, never in the
# substantive authored report archived below.
display <- function(value) nlss_mask_prose_paths(value, root)
analysis_label <- display(analysis_label)
nlss_table <- display(nlss_table)
nlss_text <- display(nlss_text)
analysis_flags <- lapply(analysis_flags, display)
template_context$tokens <- lapply(template_context$tokens, display)

nlss_report_path <- file.path(if (is.null(locations$project_root)) out_dir else root, "report_canonical.md")
log_enabled <- parse_bool(opts$log, default = get_config_value("defaults.log", TRUE))
results <- list(status = "ok", metaskill = meta_name, intent = intent, phase = phase,
  dataset = dataset_label, dataset_reference = dataset_reference, timestamp = timestamp,
  nlss_report_path = render_output_path(nlss_report_path, out_dir),
  metaskill_report_path = if (nzchar(metaskill_report_path)) render_output_path(metaskill_report_path, out_dir) else NULL,
  semantic_report = if (!is.null(metaskill_report_bytes)) list(path = basename(metaskill_report_path),
    sha256 = digest::digest(metaskill_report_bytes, algo = "sha256", serialize = FALSE), artifact = "semantic-report.md") else NULL,
  semantics = "Lifecycle evidence only; authored interpretation is preserved, not regenerated or statistically validated.")
artifacts <- list("report-template.md" = readBin(template_path, "raw", n = file.info(template_path)$size))
if (!is.null(metaskill_report_bytes)) artifacts[["semantic-report.md"]] <- metaskill_report_bytes
output <- format_nlss_report(analysis_label, nlss_table, nlss_text,
  analysis_flags = analysis_flags, template_path = template_path, table_start = 1L,
  template_context = template_context)
ctx <- get_run_context()
ctx$commands <- display(ctx$commands)
ctx$prompt <- paste(ctx$commands, collapse = " ")
log_results <- lapply(results, function(value) if (is.character(value)) display(value) else value)
nlss_publish_utility("metaskill_runner", out_dir,
  request = list(event = "metaskill_lifecycle", meta = meta_name, phase = phase,
    intent = intent, notes = notes, synopsis = synopsis, dataset = dataset_reference, log = log_enabled),
  results = results, output = output, artifacts = artifacts, workspace_root = root,
  publish = function(run_id, staging) {
    if (nzchar(metaskill_report_path)) nlss_data_change_path(metaskill_report_path, root)
    if (!is.null(metaskill_report_bytes) && !identical(import_hash(metaskill_report_path, file = TRUE), results$semantic_report$sha256)) stop("Authored metaskill report changed before publication; retry finalization.")
    append_nlss_report(nlss_report_path, analysis_label, nlss_table, nlss_text,
      analysis_flags = analysis_flags, template_path = file.path(staging, "report-template.md"),
      template_context = template_context)
    if (!is.null(metaskill_report_bytes)) {
      record_metaskill_report_block(metaskill_report_text, preserve_bytes = TRUE)
    }
    if (log_enabled) {
      logged <- append_analysis_log(out_dir, "metaskill_runner", ctx$prompt, ctx$commands,
      results = c(log_results, list(utility_run_id = run_id)),
      options = lapply(list(meta = meta_name, phase = phase, intent = intent, notes = notes,
        synopsis = synopsis, label = analysis_label, template = opts$template), display),
      user_prompt = display(get_user_prompt(opts)))
      if (resolve_logging_bool("enabled", TRUE) && !isTRUE(logged)) stop("Required metaskill JSONL projection was not written.")
    }
  })

cat("Metaskill ", phase, " logged.\n", sep = "")
cat("- ", render_output_path(nlss_report_path, out_dir), "\n", sep = "")
}

main()
