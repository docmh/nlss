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
get_user_prompt <- get("get_user_prompt", mode = "function")
parse_args <- get("parse_args", mode = "function")
parse_bool <- get("parse_bool", mode = "function")
resolve_template_path <- get("resolve_template_path", mode = "function")
source_lib <- get("source_lib", mode = "function")

print_usage <- function() {
  cat("Calc utility (base R)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript calc.R --expr \"0.05/3\"\n")
  cat("  Rscript calc.R --set \"r=0.3\" --expr \"d=2*r/sqrt(1-r^2)\"\n")
  cat("  Rscript calc.R --expr \"qnorm(0.025)|qnorm(0.975)\" --format json\n")
  cat("  Rscript calc.R --interactive\n")
  cat("\n")
  cat("Options:\n")
  cat("  --expr TEXT            Required. Expressions separated by '|'\n")
  cat("  --set TEXT             Optional. name=value pairs separated by '|'\n")
  cat("  --digits N             Rounding digits (default: 2)\n")
  cat("  --format TYPE          Output format: plain, json, csv (default: plain)\n")
  cat("  --template REF         Template path or key (optional)\n")
  cat("  --user-prompt TEXT     Original AI user prompt for logging (optional)\n")
  cat("  --log TRUE/FALSE       Write analysis_log.jsonl (default: TRUE)\n")
  cat("  --unsafe TRUE/FALSE    Allow full R evaluation in global env (default: FALSE)\n")
  cat("  --interactive          Prompt for inputs\n")
  cat("  --help                 Show this help\n")
}

resolve_config_value <- function(path, default = NULL) {
  if (exists("get_config_value", mode = "function")) {
    return(get("get_config_value", mode = "function")(path, default = default))
  }
  default
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

interactive_options <- function() {
  cat("Interactive input selected.\n")
  opts <- list()
  opts$expr <- resolve_prompt("Expressions (use | to separate)")
  opts$set <- resolve_prompt("Constants (name=value|...)", "")
  digits_default <- resolve_config_value("defaults.digits", 2)
  opts$digits <- resolve_prompt("Rounding digits", as.character(digits_default))
  opts$format <- resolve_prompt("Output format (plain/json/csv)", "plain")
  opts$template <- resolve_prompt("Template (path or key; blank for default)", "")
  opts$`user-prompt` <- resolve_prompt("User prompt (optional)", "")
  log_default <- resolve_config_value("defaults.log", TRUE)
  opts$log <- resolve_prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))
  opts$unsafe <- resolve_prompt("Unsafe eval TRUE/FALSE", "FALSE")
  opts
}

normalize_option <- function(value, name) {
  if (is.null(value)) return("")
  if (is.logical(value)) {
    if (isTRUE(value)) stop("Missing value for --", name)
    return("")
  }
  as.character(value)
}

split_pipe <- function(value) {
  value <- trimws(as.character(value))
  if (!nzchar(value)) return(character(0))
  parts <- strsplit(value, "|", fixed = TRUE)[[1]]
  parts <- trimws(parts)
  parts[nzchar(parts)]
}

is_valid_name <- function(name) {
  grepl("^[A-Za-z][A-Za-z0-9_.]*$", name)
}

detect_assignment <- function(text) {
  match <- regexpr("=", text, fixed = TRUE)
  if (match[1] == -1) return(NULL)
  pos <- as.integer(match[1])
  prev <- if (pos > 1) substr(text, pos - 1, pos - 1) else ""
  next_char <- if (pos < nchar(text)) substr(text, pos + 1, pos + 1) else ""
  if (prev %in% c("=", "<", ">", "!") || next_char == "=") return(NULL)
  left <- trimws(substr(text, 1, pos - 1))
  right <- trimws(substr(text, pos + 1, nchar(text)))
  if (!nzchar(left) || !nzchar(right)) return(NULL)
  if (!is_valid_name(left)) return(NULL)
  list(name = left, expr = right)
}

parse_required_assignment <- function(text, context) {
  assignment <- detect_assignment(text)
  if (is.null(assignment)) stop("Invalid assignment in ", context, ": ", text)
  assignment
}

build_eval_env <- function(unsafe = FALSE) {
  if (isTRUE(unsafe)) return(globalenv())
  env <- new.env(parent = emptyenv())
  allowed <- list(
    "+" = base::`+`,
    "-" = base::`-`,
    "*" = base::`*`,
    "/" = base::`/`,
    "^" = base::`^`,
    "%%" = base::`%%`,
    "%/%" = base::`%/%`,
    "abs" = base::abs,
    "sqrt" = base::sqrt,
    "log" = base::log,
    "exp" = base::exp,
    "round" = base::round,
    "min" = base::min,
    "max" = base::max,
    "sum" = base::sum,
    "mean" = base::mean,
    "sd" = stats::sd,
    "var" = stats::var,
    "pnorm" = stats::pnorm,
    "qnorm" = stats::qnorm,
    "pt" = stats::pt,
    "qt" = stats::qt,
    "pf" = stats::pf,
    "qf" = stats::qf
  )
  for (name in names(allowed)) {
    assign(name, allowed[[name]], env)
  }
  assign("pi", base::pi, env)
  assign("e", base::exp(1), env)
  env
}

eval_expression <- function(expr_text, env) {
  parsed <- tryCatch(parse(text = expr_text, keep.source = FALSE), error = function(e) e)
  if (inherits(parsed, "error") || length(parsed) == 0) {
    stop("Failed to parse expression: ", expr_text)
  }
  if (length(parsed) != 1) {
    stop("Expression must be a single statement: ", expr_text)
  }
  value <- tryCatch(eval(parsed[[1]], envir = env), error = function(e) e)
  if (inherits(value, "error")) {
    stop("Failed to evaluate expression: ", expr_text, " (", value$message, ")")
  }
  if (is.complex(value)) {
    stop("Complex results are not supported: ", expr_text)
  }
  if (!is.numeric(value) || length(value) == 0) {
    stop("Expression did not return a numeric value: ", expr_text)
  }
  value
}

apply_assignments <- function(items, env, context) {
  if (length(items) == 0) return(list())
  entries <- list()
  for (item in items) {
    assignment <- parse_required_assignment(item, context)
    value <- eval_expression(assignment$expr, env)
    assign(assignment$name, value, env)
    entries[[length(entries) + 1]] <- list(
      name = assignment$name,
      expression = assignment$expr,
      value = value
    )
  }
  entries
}

evaluate_expressions <- function(items, env) {
  results <- list()
  rows <- list()
  unnamed_index <- 1
  for (item in items) {
    assignment <- detect_assignment(item)
    if (!is.null(assignment)) {
      name <- assignment$name
      expr_text <- assignment$expr
    } else {
      name <- paste0("expr_", unnamed_index)
      expr_text <- item
      unnamed_index <- unnamed_index + 1
    }
    value <- eval_expression(expr_text, env)
    assign(name, value, env)
    results[[name]] <- value
    rows[[length(rows) + 1]] <- list(
      name = name,
      expression = expr_text,
      value = value
    )
  }
  list(results = results, rows = rows)
}

format_value_plain <- function(value, digits) {
  fmt <- function(val) {
    if (is.na(val) || !is.finite(val)) return("NA")
    format(round(val, digits), nsmall = digits, trim = TRUE, scientific = FALSE)
  }
  if (length(value) == 1) return(fmt(value))
  vals <- vapply(value, fmt, character(1))
  paste0("c(", paste(vals, collapse = ", "), ")")
}

json_escape <- function(text) {
  text <- gsub("\\\\", "\\\\\\\\", text)
  gsub("\"", "\\\\\"", text)
}

format_value_json <- function(value, digits) {
  fmt <- function(val) {
    if (is.na(val) || !is.finite(val)) return("null")
    format(round(val, digits), nsmall = digits, trim = TRUE, scientific = FALSE)
  }
  if (length(value) == 1) return(fmt(value))
  vals <- vapply(value, fmt, character(1))
  paste0("[", paste(vals, collapse = ","), "]")
}

encode_json <- function(results, digits) {
  if (length(results) == 0) return("{}")
  parts <- mapply(
    function(name, value) {
      paste0("\"", json_escape(name), "\":", format_value_json(value, digits))
    },
    names(results),
    results,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )
  paste0("{", paste(parts, collapse = ","), "}")
}

csv_escape <- function(text) {
  if (grepl("[\",\n]", text)) {
    text <- gsub("\"", "\"\"", text)
    return(paste0("\"", text, "\""))
  }
  text
}

output_results <- function(results, format, digits) {
  if (format == "json") {
    cat(encode_json(results, digits), "\n", sep = "")
    return(invisible(NULL))
  }
  if (format == "csv") {
    cat("name,value\n")
    for (name in names(results)) {
      value <- format_value_plain(results[[name]], digits)
      cat(name, ",", csv_escape(value), "\n", sep = "")
    }
    return(invisible(NULL))
  }
  for (name in names(results)) {
    value <- format_value_plain(results[[name]], digits)
    cat(name, " = ", value, "\n", sep = "")
  }
  invisible(NULL)
}

resolve_get_default_out <- function() {
  if (exists("get_default_out", mode = "function")) {
    return(get("get_default_out", mode = "function")())
  }
  "./outputs/tmp"
}

resolve_ensure_out_dir <- function(path) {
  if (exists("ensure_out_dir", mode = "function")) {
    return(get("ensure_out_dir", mode = "function")(path))
  }
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  path
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

build_calc_table_body <- function(rows, digits, table_spec = NULL) {
  default_specs <- list(
    list(key = "name", label = "Name"),
    list(key = "expression", label = "Expression"),
    list(key = "value", label = "Value")
  )
  columns <- resolve_normalize_table_columns(if (!is.null(table_spec)) table_spec$columns else NULL, default_specs)
  table_rows <- list()
  for (row in rows) {
    values <- character(0)
    for (col in columns) {
      key <- col$key
      cell <- ""
      if (!is.null(row[[key]])) {
        if (key == "value") {
          cell <- format_value_plain(row[[key]], digits)
        } else {
          cell <- resolve_as_cell_text(row[[key]])
        }
      }
      values <- c(values, cell)
    }
    table_rows[[length(table_rows) + 1]] <- values
  }
  adjusted <- resolve_drop_empty_columns(columns, table_rows)
  headers <- vapply(adjusted$columns, function(col) col$label, character(1))
  body <- resolve_render_markdown_table(headers, adjusted$rows)
  list(body = body, columns = adjusted$columns)
}

args <- commandArgs(trailingOnly = TRUE)
opts <- parse_args(args)

if (isTRUE(opts$help) || length(args) == 0) {
  print_usage()
  quit(status = 0)
}

if (parse_bool(opts$interactive, FALSE)) {
  opts <- interactive_options()
}

expr_text <- normalize_option(opts$expr, "expr")
if (!nzchar(expr_text)) stop("Missing --expr. Use --help for usage.")

set_text <- normalize_option(opts$set, "set")
digits_default <- resolve_config_value("defaults.digits", 2)
digits_text <- normalize_option(opts$digits, "digits")
digits <- if (nzchar(digits_text)) suppressWarnings(as.integer(digits_text)) else as.integer(digits_default)
if (is.na(digits) || digits < 0) stop("Invalid --digits value.")

format_text <- normalize_option(opts$format, "format")
format <- tolower(if (nzchar(format_text)) format_text else "plain")
if (!format %in% c("plain", "json", "csv")) {
  stop("Invalid --format value. Use plain, json, or csv.")
}

unsafe <- parse_bool(opts$unsafe, FALSE)
env <- build_eval_env(unsafe = unsafe)

set_items <- split_pipe(set_text)
set_entries <- apply_assignments(set_items, env, "--set")

expr_items <- split_pipe(expr_text)
if (length(expr_items) == 0) stop("No expressions to evaluate.")

evaluation <- evaluate_expressions(expr_items, env)
output_results(evaluation$results, format, digits)

analysis_flags <- list(
  expr = expr_items,
  set = if (length(set_items) > 0) set_items else NULL,
  digits = digits,
  `output-format` = format,
  unsafe = unsafe
)

analysis_label <- "Calc"
out_dir <- resolve_get_default_out()
out_dir <- resolve_ensure_out_dir(out_dir)
report_path <- file.path(out_dir, "report_canonical.md")

template_override <- resolve_template_override(opts$template, module = "calc")
template_path <- template_override
if (is.null(template_path)) {
  template_path <- resolve_template_path("calc.default", "calc/default-template.md")
}
template_meta <- resolve_get_template_meta(template_path)

table_rows <- evaluation$rows
table_result <- build_calc_table_body(table_rows, digits, if (!is.null(template_meta$table)) template_meta$table else NULL)
note_text <- paste0("Note. Values are rounded to ", digits, " decimal places.")
apa_table <- paste0("Table 1\n\n", table_result$body, "\n", note_text)

narrative_default <- paste0("Computed ", length(table_rows), " expression", ifelse(length(table_rows) == 1, "", "s"), ".")
narrative_rows <- lapply(table_rows, function(row) {
  value_text <- format_value_plain(row$value, digits)
  list(
    name = row$name,
    expression = row$expression,
    value = value_text,
    full_sentence = paste0(row$name, " = ", value_text)
  )
})

template_context <- list(
  tokens = list(
    table_body = table_result$body,
    expression_count = length(table_rows)
  ),
  narrative_rows = narrative_rows
)

resolve_append_apa_report(
  report_path,
  analysis_label,
  apa_table,
  narrative_default,
  analysis_flags = analysis_flags,
  template_path = template_path,
  template_context = template_context
)

log_default <- resolve_config_value("defaults.log", TRUE)
log_enabled <- parse_bool(opts$log, default = log_default)
if (isTRUE(log_enabled)) {
  run_context <- resolve_get_run_context()
  results_payload <- list(
    status = "success",
    count = length(table_rows),
    values = evaluation$results,
    constants = if (length(set_entries) > 0) set_entries else NULL
  )
  options_payload <- list(
    expr = expr_items,
    set = if (length(set_items) > 0) set_items else NULL,
    digits = digits,
    format = format,
    unsafe = unsafe,
    template = if (!is.null(template_override) && nzchar(template_override)) template_override else NULL
  )
  resolve_append_analysis_log(
    out_dir,
    "calc",
    run_context$prompt,
    run_context$commands,
    results_payload,
    options = options_payload,
    user_prompt = get_user_prompt(opts)
  )
}
