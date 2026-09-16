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
  cat("  --unsafe TRUE/FALSE    Allow full R evaluation in global env (default: config FALSE)\n")
  cat("  Calculation reports and immutable utility audits are written without a dataset.\n")
  cat("  --log FALSE disables JSONL, not the report or audit; utilities are not automatic replay requests.\n")
  cat("  --interactive          Prompt for inputs\n")
  cat("  --help                 Show this help\n")
}

interactive_options <- function() {
  cat("Interactive input selected.\n")
  opts <- list()
  opts$expr <- prompt("Expressions (use | to separate)")
  opts$set <- prompt("Constants (name=value|...)", "")
  digits_default <- get_config_value("defaults.digits")
  opts$digits <- prompt("Rounding digits", as.character(digits_default))
  opts$format <- prompt("Output format (plain/json/csv)", get_config_value("modules.calc.format"))
  opts$template <- prompt("Template (path or key; blank for default)", "")
  opts$`user-prompt` <- prompt("User prompt (optional)", "")
  log_default <- get_config_value("defaults.log")
  opts$log <- prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))
  opts$unsafe <- prompt("Unsafe eval TRUE/FALSE", ifelse(get_config_value("modules.calc.unsafe"), "TRUE", "FALSE"))
  opts
}

normalize_option <- function(value, name) {
  if (is.null(value)) return("")
  if (length(value) != 1L || is.logical(value) || is.na(value)) stop("Missing or invalid value for --", name)
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
    "(" = base::`(`,
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
    format(round(val, digits), nsmall = digits, trim = TRUE, scientific = FALSE, decimal.mark = ".")
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

build_calc_table_body <- function(rows, digits, table_spec = NULL) {
  default_specs <- list(
    list(key = "name", label = "Name"),
    list(key = "expression", label = "Expression"),
    list(key = "value", label = "Value")
  )
  columns <- normalize_table_columns(if (!is.null(table_spec)) table_spec$columns else NULL, default_specs)
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
          cell <- as_cell_text(row[[key]])
        }
      }
      values <- c(values, cell)
    }
    table_rows[[length(table_rows) + 1]] <- values
  }
  adjusted <- drop_empty_columns(columns, table_rows)
  headers <- vapply(adjusted$columns, function(col) col$label, character(1))
  body <- render_markdown_table(headers, adjusted$rows)
  list(body = body, columns = adjusted$columns)
}

calc_mask_expressions <- function(text, root) {
  masked <- nlss_mask_expression_paths(text, root)
  vapply(masked, function(value) {
    parsed <- tryCatch(utils::getParseData(parse(text = value, keep.source = TRUE), includeText = TRUE), error = function(e) NULL)
    comments <- unique(parsed$text[parsed$token == "COMMENT"])
    for (comment in comments[order(nchar(comments), decreasing = TRUE)]) {
      positions <- gregexpr(comment, value, fixed = TRUE)
      found <- regmatches(value, positions)[[1]]
      if (length(found)) regmatches(value, positions) <- list(rep(nlss_mask_prose_paths(comment, root), length(found)))
    }
    value
  }, character(1), USE.NAMES = FALSE)
}

calc_value_status <- function(value) {
  value <- as.numeric(value)
  ifelse(is.nan(value), "NaN", ifelse(is.na(value), "NA",
    ifelse(is.infinite(value), ifelse(value > 0, "positive_infinity", "negative_infinity"), "finite")))
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  opts <- parse_args(args, module = "calc")
  if (parse_bool(opts[["help"]], FALSE) || length(args) == 0L) { print_usage(); return(invisible(NULL)) }
  if (parse_bool(opts[["interactive"]], FALSE)) opts <- modifyList(opts, interactive_options())
  expr_text <- normalize_option(opts[["expr"]], "expr")
  if (!nzchar(expr_text)) stop("Missing --expr. Use --help for usage.")
  set_text <- normalize_option(opts[["set"]], "set")
  digits_value <- opts[["digits"]]
  if (is.null(digits_value)) digits_value <- get_config_value("defaults.digits")
  digits <- suppressWarnings(as.numeric(digits_value))
  if (length(digits_value) != 1L || is.logical(digits_value) || length(digits) != 1L ||
      !is.finite(digits) || digits < 0 || digits > 15 || digits != floor(digits)) {
    stop("--digits must be a finite integer between 0 and 15.")
  }
  digits <- as.integer(digits)
  format_text <- normalize_option(opts[["format"]], "format")
  format <- tolower(if (nzchar(format_text)) format_text else get_config_value("modules.calc.format"))
  if (!format %in% c("plain", "json", "csv")) stop("Invalid --format value. Use plain, json, or csv.")
  unsafe <- parse_bool(opts[["unsafe"]], get_config_value("modules.calc.unsafe"))
  log_enabled <- parse_bool(opts[["log"]], get_config_value("defaults.log"))
  # Bind the normal utility output location before unrestricted R can change the
  # process working directory. Unsafe external side effects remain unrestricted.
  out_dir <- get_default_out()
  nlss_utility_check_directory(out_dir)
  out_dir <- normalizePath(out_dir, winslash = "/", mustWork = FALSE)
  context <- get_run_context()
  template_ref <- normalize_option(opts[["template"]], "template")
  template_path <- resolve_template_override(template_ref, module = "calc")
  if (nzchar(template_ref) && (is.null(template_path) || !file.exists(template_path) || dir.exists(template_path))) {
    stop("--template must identify an existing template file or configured template key.")
  }
  if (is.null(template_path)) template_path <- resolve_template_path("calc.default")
  artifacts <- list()
  if (!is.null(template_path) && file.exists(template_path)) {
    if (dir.exists(template_path)) stop("Configured Calc template must be a file.")
    artifacts[["template.md"]] <- readBin(template_path, "raw", n = file.info(template_path)$size)
    frozen <- tempfile("nlss-calc-template-", fileext = ".md")
    writeBin(artifacts[["template.md"]], frozen)
    on.exit(unlink(frozen), add = TRUE)
    template_path <- frozen
  } else template_path <- NULL
  template_meta <- get_template_meta(template_path)
  set_items <- split_pipe(set_text)
  expr_items <- split_pipe(expr_text)
  if (!length(expr_items)) stop("No expressions to evaluate.")
  env <- build_eval_env(unsafe)
  warnings <- character()
  computed <- withCallingHandlers({
    constants <- apply_assignments(set_items, env, "--set")
    evaluation <- evaluate_expressions(expr_items, env)
    list(constants = constants, evaluation = evaluation)
  }, warning = function(w) warnings <<- c(warnings, conditionMessage(w)))
  set_entries <- computed$constants
  evaluation <- computed$evaluation
  reproducibility <- list(deterministic_given_recorded_inputs = !unsafe, automatic_replay = FALSE,
    reason = if (unsafe) "Unrestricted R may use randomness, external state, side effects and arbitrary packages. This audit is not an automatic replay request or a rollback of those side effects." else
      "Restricted numeric calculations use recorded expressions/constants and the recorded R environment. Utility audits are evidence, not supported automatic replay requests.")
  results_payload <- list(status = "success", count = length(evaluation$rows), values = evaluation$results,
    constants = if (length(set_entries)) set_entries else NULL, rows = evaluation$rows,
    value_status = lapply(evaluation$results, calc_value_status),
    value_shape = lapply(evaluation$results, function(value) list(length = length(value), class = class(value), dimensions = dim(value))),
    warnings = warnings, reproducibility = reproducibility)
  options_payload <- list(expr = expr_items, set = if (length(set_items)) set_items else NULL,
    digits = digits, format = format, unsafe = unsafe)
  artifacts[["values.rds"]] <- serialize(list(constants = set_entries, evaluation = evaluation), NULL, version = 3L)
  table_rows <- lapply(evaluation$rows, function(row) {
    row$expression <- calc_mask_expressions(row$expression, out_dir)
    row
  })
  table_result <- build_calc_table_body(table_rows, digits, template_meta$table)
  note <- paste0("Values are rounded to ", digits, " decimal places.")
  if (any(unlist(results_payload$value_status) != "finite")) note <- paste(note,
    "Nonfinite values display as NA (plain/CSV) or null (JSON); the audit distinguishes NA, NaN and signed infinity.")
  if (unsafe) note <- paste(note, "Unsafe R evaluation was explicitly enabled; external side effects are not protected or automatically reproducible.")
  nlss_table <- paste0("Table 1\n\n", table_result$body, "\nNote. ", note)
  narrative <- paste0("Computed ", length(table_rows), " expression", if (length(table_rows) == 1L) "" else "s", ".")
  narrative_rows <- lapply(table_rows, function(row) list(name = row$name, expression = row$expression,
    value = format_value_plain(row$value, digits), full_sentence = paste0(row$name, " = ", format_value_plain(row$value, digits))))
  template_context <- list(tokens = list(table_body = table_result$body, expression_count = length(table_rows)),
    narrative_rows = narrative_rows)
  flags <- list(expr = calc_mask_expressions(expr_items, out_dir),
    set = if (length(set_items)) calc_mask_expressions(set_items, out_dir) else NULL,
    digits = digits, `output-format` = format, unsafe = unsafe)
  output <- format_nlss_report("Calc", nlss_table, narrative, analysis_flags = flags,
    template_path = template_path, table_start = 1L, template_context = template_context)
  safeguard <- paste0("## Calculation scope\n\n", reproducibility$reason,
    if (any(unlist(results_payload$value_status) != "finite")) "\n\nNonfinite values are retained explicitly in the audit; an unavailable value is not a finite estimate." else "")
  output <- paste(output, safeguard, sep = "\n\n")
  stdout <- capture.output(output_results(evaluation$results, format, digits))
  artifacts[["stdout.txt"]] <- paste0(paste(stdout, collapse = "\n"), "\n")
  request <- list(options = options_payload, reproducibility = reproducibility,
    expressions = expr_items, constants = set_items,
    user_prompt = nlss_mask_prose_paths(get_user_prompt(opts), out_dir),
    configuration = list(digits = get_config_value("defaults.digits"), log = get_config_value("defaults.log"),
      calc = get_config_value("modules.calc")))
  nlss_publish_utility("calc", out_dir, request, results_payload, output,
    artifacts = artifacts, publish = function(run_id, staging) {
      report <- file.path(out_dir, "report_canonical.md")
      frozen_template <- if ("template.md" %in% names(artifacts)) file.path(staging, "template.md") else NULL
      append_nlss_report(report, "Calc", nlss_table, narrative, analysis_flags = flags,
        template_path = frozen_template, template_context = template_context)
      append_nlss_report(report, "Calculation scope", "", sub("^## Calculation scope\\n\\n", "", safeguard))
      if (log_enabled) {
        legacy <- results_payload
        legacy$rows <- table_rows
        if (length(legacy$constants)) legacy$constants <- lapply(legacy$constants, function(row) {
          row$expression <- calc_mask_expressions(row$expression, out_dir); row
        })
        legacy$warnings <- nlss_mask_prose_paths(legacy$warnings, out_dir)
        options <- options_payload
        options$expr <- flags$expr
        options$set <- flags$set
        options$utility_run_id <- run_id
        commands <- context$commands
        for (i in seq_along(commands)) {
          inline <- grepl("^--(expr|set)=", commands[[i]])
          if (inline) {
            prefix <- sub("^(--(expr|set)=).*", "\\1", commands[[i]])
            commands[[i]] <- paste0(prefix, calc_mask_expressions(substring(commands[[i]], nchar(prefix) + 1L), out_dir))
          } else {
            code <- i > 1L && commands[[i - 1L]] %in% c("--expr", "--set")
            commands[[i]] <- if (code) calc_mask_expressions(commands[[i]], out_dir) else nlss_mask_prose_paths(commands[[i]], out_dir)
          }
        }
        logged <- append_analysis_log(out_dir, "calc", paste(commands, collapse = " "), commands, legacy, options,
          user_prompt = nlss_mask_prose_paths(get_user_prompt(opts), out_dir))
        if (resolve_logging_bool("enabled", TRUE) && !isTRUE(logged)) stop("Required calculation JSONL projection was not written.")
      }
    })
  cat(paste(stdout, collapse = "\n"), "\n", sep = "")
  invisible(results_payload)
}

main()
