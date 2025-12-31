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
source(file.path(bootstrap_dir, "lib", "paths.R"))
source_lib("cli.R")
source_lib("config.R")
source_lib("io.R")
source_lib("formatting.R")


# Static analysis aliases for source_lib-defined functions.
render_output_path <- get("render_output_path", mode = "function")
source_lib <- get("source_lib", mode = "function")

print_usage <- function() {
  cat("Metaskill activation/finalization logger (base R)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript metaskill_runner.R --csv data.csv --meta sample-description\n")
  cat("  Rscript metaskill_runner.R --parquet data.parquet --meta sample-description --intent \"describe the sample\"\n")
  cat("  Rscript metaskill_runner.R --interactive\n")
  cat("\n")
  cat("Options:\n")
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
  cat("  --log TRUE/FALSE     Write analysis_log.jsonl (default: TRUE)\n")
  cat("  --interactive        Prompt for inputs\n")
  cat("  --help               Show this help\n")
}

interactive_options <- function() {
  cat("Interactive input selected.\n")
  input_type <- resolve_prompt("Input type (csv/sav/rds/rdata/parquet)", "csv")
  input_type <- tolower(input_type)
  opts <- list()

  if (input_type == "csv") {
    opts$csv <- resolve_prompt("CSV path")
    sep_default <- resolve_config_value("defaults.csv.sep", ",")
    header_default <- resolve_config_value("defaults.csv.header", TRUE)
    opts$sep <- resolve_prompt("Separator", sep_default)
    opts$header <- resolve_prompt("Header TRUE/FALSE", ifelse(isTRUE(header_default), "TRUE", "FALSE"))
  } else if (input_type == "sav") {
    opts$sav <- resolve_prompt("SAV path")
  } else if (input_type == "rds") {
    opts$rds <- resolve_prompt("RDS path")
  } else if (input_type == "rdata") {
    opts$rdata <- resolve_prompt("RData path")
    opts$df <- resolve_prompt("Data frame object name")
  } else if (input_type == "parquet") {
    opts$parquet <- resolve_prompt("Parquet path")
  } else {
    stop("Unsupported input type.")
  }

  opts$meta <- resolve_prompt("Metaskill name")
  opts$phase <- resolve_prompt("Phase (activation/finalization)", "activation")
  opts$intent <- resolve_prompt("Intent (optional)", "")
  opts$notes <- resolve_prompt("Notes (optional)", "")
  opts$synopsis <- resolve_prompt("Synopsis (optional)", "")
  opts$label <- resolve_prompt("Analysis label (optional)", "")
  opts$template <- resolve_prompt("Template (path or key; blank for default)", "")
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

resolve_load_dataframe <- function(opts) {
  if (exists("load_dataframe", mode = "function")) {
    return(get("load_dataframe", mode = "function")(opts))
  }
  stop("Missing load_dataframe. Ensure lib/io.R is sourced.")
}

resolve_get_workspace_out_dir <- function(df = NULL, label = NULL) {
  if (exists("get_workspace_out_dir", mode = "function")) {
    return(get("get_workspace_out_dir", mode = "function")(df, label = label))
  }
  "./outputs/tmp"
}

resolve_get_run_context <- function() {
  if (exists("get_run_context", mode = "function")) {
    return(get("get_run_context", mode = "function")())
  }
  list(prompt = "", commands = character(0))
}

resolve_get_user_prompt <- function(opts) {
  if (exists("get_user_prompt", mode = "function")) {
    return(get("get_user_prompt", mode = "function")(opts))
  }
  NULL
}

resolve_sanitize_file_component <- function(value) {
  if (exists("sanitize_file_component", mode = "function")) {
    return(get("sanitize_file_component", mode = "function")(value))
  }
  clean <- enc2utf8(as.character(value))
  clean <- gsub("[^A-Za-z0-9._-]", "_", clean)
  clean <- gsub("_+", "_", clean)
  if (!nzchar(clean)) clean <- "value"
  clean
}

resolve_get_template_path <- function(key, default_relative = NULL) {
  if (exists("resolve_template_path", mode = "function")) {
    return(get("resolve_template_path", mode = "function")(key, default_relative))
  }
  if (is.null(default_relative) || !nzchar(default_relative)) return(NULL)
  if (exists("get_assets_dir", mode = "function")) {
    return(file.path(get("get_assets_dir", mode = "function")(), default_relative))
  }
  file.path(getwd(), "nlss", "assets", default_relative)
}

resolve_get_template_meta <- function(path) {
  if (exists("get_template_meta", mode = "function")) {
    return(get("get_template_meta", mode = "function")(path))
  }
  list()
}

resolve_template_override <- local({
  override_impl <- NULL
  if (exists("resolve_template_override", mode = "function")) {
    override_impl <- get("resolve_template_override", mode = "function")
  }
  function(template_ref, module = NULL) {
    if (!is.null(override_impl)) {
      return(override_impl(template_ref, module = module))
    }
    NULL
  }
})

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
  if (length(value) == 0 || is.null(value) || is.na(value)) return("")
  as.character(value)
}

resolve_append_nlss_report <- function(path, analysis_label, nlss_table, nlss_text, analysis_flags = NULL, template_path = NULL, template_context = NULL) {
  if (exists("append_nlss_report", mode = "function")) {
    return(get("append_nlss_report", mode = "function")(
      path,
      analysis_label,
      nlss_table,
      nlss_text,
      analysis_flags = analysis_flags,
      template_path = template_path,
      template_context = template_context
    ))
  }
  stop("Missing report formatter. Ensure lib/formatting.R is sourced.")
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

resolve_set_report_snapshot <- function(report) {
  if (exists("set_report_snapshot", mode = "function")) {
    return(get("set_report_snapshot", mode = "function")(report))
  }
  invisible(FALSE)
}

resolve_record_metaskill_report_block <- function(report) {
  if (exists("record_metaskill_report_block", mode = "function")) {
    return(get("record_metaskill_report_block", mode = "function")(report))
  }
  invisible(FALSE)
}

slugify_component <- function(value, fallback) {
  if (is.null(value) || !nzchar(value)) return(fallback)
  clean <- resolve_sanitize_file_component(value)
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

emit_missing_report_issue <- function(out_dir, opts, meta_name, intent, expected_paths) {
  log_default <- resolve_config_value("defaults.log", TRUE)
  message <- paste0(
    "Metaskill report is missing. Create the metaskill report before finalizing.\n",
    "Expected file(s): ", paste(expected_paths, collapse = ", ")
  )
  if (resolve_parse_bool(opts$log, default = log_default)) {
    ctx <- resolve_get_run_context()
    resolve_append_analysis_log(
      out_dir,
      module = "metaskill_runner",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(
        status = "missing_metaskill_report",
        message = message,
        details = list(
          metaskill = meta_name,
          intent = intent,
          expected_paths = expected_paths
        )
      ),
      options = list(
        meta = meta_name,
        phase = if (!is.null(opts$phase)) opts$phase else "finalization",
        intent = intent
      ),
      user_prompt = resolve_get_user_prompt(opts)
    )
  }
  stop(message)
}

is_finalization_phase <- function(phase) {
  if (is.null(phase)) return(FALSE)
  text <- tolower(trimws(as.character(phase)))
  if (!nzchar(text)) return(FALSE)
  text %in% c("finalization", "finalise", "finalize", "completion", "complete", "completed", "finish", "finished")
}

resolve_derive_dataset_label <- function(path, df_name = NULL) {
  if (exists("derive_dataset_label", mode = "function")) {
    return(get("derive_dataset_label", mode = "function")(path, df_name))
  }
  ""
}

resolve_sanitize_file_component <- function(value) {
  if (exists("sanitize_file_component", mode = "function")) {
    return(get("sanitize_file_component", mode = "function")(value))
  }
  value
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
  columns <- resolve_normalize_table_columns(
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
      row_vals <- c(row_vals, resolve_as_cell_text(val))
    }
    table_rows[[length(table_rows) + 1]] <- row_vals
  }

  filtered <- resolve_drop_empty_columns(columns, table_rows)
  columns <- filtered$columns
  table_rows <- filtered$rows
  headers <- vapply(columns, function(col) {
    if (!is.null(col$label) && nzchar(col$label)) col$label else col$key
  }, character(1))
  list(
    body = resolve_render_markdown_table(headers, table_rows),
    columns = vapply(columns, function(col) col$key, character(1))
  )
}

build_activation_table <- function(table_body, analysis_label, note_text) {
  header <- paste0("Table 1\n", analysis_label, "\n")
  note_line <- if (nzchar(note_text)) paste0("Note. ", note_text) else "Note."
  paste0(header, "\n", table_body, "\n\n", note_line, "\n")
}

build_activation_text <- function(meta_name, intent, dataset_label, timestamp) {
  text <- paste0("Metaskill \"", meta_name, "\" activated")
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
      label <- resolve_derive_dataset_label(source_path)
    }
  }
  if (!nzchar(label)) {
    if (!is.null(opts$parquet) && nzchar(opts$parquet)) {
      label <- resolve_derive_dataset_label(opts$parquet)
    } else if (!is.null(opts$csv) && nzchar(opts$csv)) {
      label <- resolve_derive_dataset_label(opts$csv)
    } else if (!is.null(opts$sav) && nzchar(opts$sav)) {
      label <- resolve_derive_dataset_label(opts$sav)
    } else if (!is.null(opts$rds) && nzchar(opts$rds)) {
      label <- resolve_derive_dataset_label(opts$rds)
    } else if (!is.null(opts$rdata) && nzchar(opts$rdata)) {
      label <- resolve_derive_dataset_label(opts$rdata, opts$df)
    }
  }
  resolve_sanitize_file_component(label)
}

args <- commandArgs(trailingOnly = TRUE)
opts <- resolve_parse_args(args)

if (!is.null(opts$help)) {
  print_usage()
  quit(save = "no", status = 0)
}

interactive_default <- resolve_config_value("defaults.interactive", FALSE)
if (resolve_parse_bool(opts$interactive, default = interactive_default)) {
  opts <- interactive_options()
}

meta_default <- resolve_config_value("modules.metaskill_runner.meta_default", "")
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
  analysis_label <- resolve_config_value("modules.metaskill_runner.analysis_label", "Metaskill activation")
  if (!nzchar(analysis_label)) {
    analysis_label <- "Metaskill activation"
  }
  phase_lower <- tolower(phase)
  if (analysis_label == "Metaskill activation" && phase_lower %in% c("finalization", "finalise", "finalize", "completion", "complete", "completed", "finish", "finished")) {
    analysis_label <- "Metaskill finalization"
  }
}
note_default <- resolve_config_value(
  "modules.metaskill_runner.note_default",
  "This entry logs metaskill activation/finalization only; analyses are logged separately."
)

df <- resolve_load_dataframe(opts)
dataset_label <- get_dataset_label(df, opts)
out_dir <- resolve_get_workspace_out_dir(df, label = dataset_label)

metaskill_report_path <- ""
if (is_finalization_phase(phase)) {
  expected_paths <- build_report_paths(out_dir, meta_name, intent)
  has_report <- any(file.exists(expected_paths))
  if (!has_report) {
    emit_missing_report_issue(out_dir, opts, meta_name, intent, expected_paths)
  }
  existing_paths <- expected_paths[file.exists(expected_paths)]
  if (length(existing_paths) > 0) {
    metaskill_report_path <- existing_paths[1]
    if (exists("ensure_output_front_matter", mode = "function")) {
      get("ensure_output_front_matter", mode = "function")(metaskill_report_path)
    }
  }
}

template_override <- resolve_template_override(opts$template, module = "metaskill_runner")
template_path <- if (!is.null(template_override)) {
  template_override
} else {
  template_key <- "metaskill_runner.default"
  template_fallback <- "metaskill-runner/default-template.md"
  if (is_finalization_phase(phase)) {
    template_key <- "metaskill_runner.finalization"
    template_fallback <- "metaskill-runner/finalization-template.md"
  }
  resolve_get_template_path(template_key, template_fallback)
}
if (is.null(template_path) || !file.exists(template_path)) {
  stop("NLSS format template not found: ", template_path)
}
template_meta <- resolve_get_template_meta(template_path)

timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
rows <- build_activation_rows(meta_name, intent, dataset_label, timestamp, notes = notes)
table_result <- build_activation_table_body(rows, template_meta$table)
nlss_table <- build_activation_table(table_result$body, analysis_label, note_default)
nlss_text <- build_activation_text(meta_name, intent, dataset_label, timestamp)

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

nlss_report_path <- file.path(out_dir, "report_canonical.md")
resolve_append_nlss_report(
  nlss_report_path,
  analysis_label,
  nlss_table,
  nlss_text,
  analysis_flags = analysis_flags,
  template_path = template_path,
  template_context = template_context
)

if (is_finalization_phase(phase)) {
  metaskill_report_text <- ""
  if (nzchar(metaskill_report_path) && file.exists(metaskill_report_path)) {
    metaskill_report_text <- paste(readLines(metaskill_report_path, warn = FALSE), collapse = "\n")
  }
  if (nzchar(metaskill_report_text)) {
    resolve_record_metaskill_report_block(metaskill_report_text)
  }
}

log_default <- resolve_config_value("defaults.log", TRUE)
if (resolve_parse_bool(opts$log, default = log_default)) {
  ctx <- resolve_get_run_context()
  resolve_append_analysis_log(
    out_dir,
    module = "metaskill_runner",
    prompt = ctx$prompt,
    commands = ctx$commands,
    results = list(
      status = "ok",
      metaskill = meta_name,
      intent = intent,
      phase = phase,
      dataset = dataset_label,
      timestamp = timestamp,
      nlss_report_path = render_output_path(nlss_report_path, out_dir),
      metaskill_report_path = if (nzchar(metaskill_report_path)) render_output_path(metaskill_report_path, out_dir) else NULL
    ),
    options = list(
      meta = meta_name,
      phase = phase,
      intent = intent,
      notes = notes,
      label = analysis_label,
      template = opts$template
    ),
    user_prompt = resolve_get_user_prompt(opts)
  )
}

cat("Metaskill ", phase, " logged.\n", sep = "")
cat("- ", render_output_path(nlss_report_path, out_dir), "\n", sep = "")
