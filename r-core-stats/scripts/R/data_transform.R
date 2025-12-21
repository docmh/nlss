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
  cat("Data transformation (base R)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript data_transform.R --csv data.csv [--calc \"newvar=expr\"] [--transform \"var=log\"] [--standardize var1,var2]\n")
  cat("  Rscript data_transform.R --sav data.sav [--recode \"var=1:0,2:1\"] [--rename old:new] [--drop var1,var2]\n")
  cat("  Rscript data_transform.R --rds data.rds [--out ./outputs/tmp]\n")
  cat("  Rscript data_transform.R --rdata data.RData --df data_frame_name [--interactive]\n")
  cat("\n")
  cat("Options:\n")
  cat("  --csv PATH               CSV input file\n")
  cat("  --sav PATH               SPSS .sav input file\n")
  cat("  --sep VALUE              CSV separator (default: ,)\n")
  cat("  --header TRUE/FALSE      CSV header (default: TRUE)\n")
  cat("  --rds PATH               RDS input file (data frame)\n")
  cat("  --rdata PATH             RData input file\n")
  cat("  --df NAME                Data frame object name in RData\n")
  cat("  --calc RULES             New variables: \"newvar=expr|newvar2=expr\"\n")
  cat("  --transform RULES        Transforms: \"var=log|var2=sqrt|var3=scale\"\n")
  cat("  --transform-into MAP      Output names: \"var=newname|var2=newname2\"\n")
  cat("  --standardize LIST        Comma-separated variables to z-standardize\n")
  cat("  --standardize-suffix TXT  Suffix for z-scores (default: _z)\n")
  cat("  --standardize-into MAP    Output names: \"var=newname|var2=newname2\"\n")
  cat("  --percentile-bins RULES   Percentile bins: \"var=4|var2=5\"\n")
  cat("  --percentile-suffix TXT   Suffix for percentile bins (default: _pct)\n")
  cat("  --percentile-into MAP     Output names: \"var=newname|var2=newname2\"\n")
  cat("  --bins RULES              Custom breaks: \"var=0,10,20|var2=5,15,25\"\n")
  cat("  --bins-suffix TXT         Suffix for custom bins (default: _bin)\n")
  cat("  --bins-into MAP           Output names: \"var=newname|var2=newname2\"\n")
  cat("  --recode RULES            Recodes: \"var=1:0,2:1|var2=low:0,high:1\"\n")
  cat("  --recode-suffix TXT        Suffix for recodes (default: _rec)\n")
  cat("  --recode-into MAP          Output names: \"var=newname|var2=newname2\"\n")
  cat("  --rename MAP              Rename: \"old:new,old2:new2\"\n")
  cat("  --drop LIST               Comma-separated variables to drop\n")
  cat("  --coerce                  Coerce non-numeric vars to numeric for transforms\n")
  cat("  --overwrite-vars          Allow overwriting existing variables\n")
  cat("  --confirm-overwrite       Confirm overwriting existing variables\n")
  cat("  --confirm-drop            Confirm dropping variables\n")
  cat("  --user-prompt TEXT        Original AI user prompt for logging (optional)\n")
  cat("  --log TRUE/FALSE          Write analysis_log.jsonl (default: TRUE)\n")
  cat("  --out DIR                 Output directory (default: ./outputs/tmp)\n")
  cat("  --interactive             Prompt for inputs\n")
  cat("  --help                    Show this help\n")
}


interactive_options <- function() {
  cat("Interactive input selected.\n")
  input_type <- resolve_prompt("Input type (csv/sav/rds/rdata)", "csv")
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
  } else {
    stop("Unsupported input type.")
  }

  opts$calc <- resolve_prompt("Calculated variables (newvar=expr|newvar2=expr)", "")
  opts$transform <- resolve_prompt("Transforms (var=log|var2=sqrt|var3=scale)", "")
  opts$`transform-into` <- resolve_prompt("Transform output names (var=newname|var2=newname2)", "")
  opts$standardize <- resolve_prompt("Standardize variables (comma-separated)", "")
  standardize_suffix_default <- resolve_config_value("modules.data_transform.standardize_suffix", "_z")
  opts$`standardize-suffix` <- resolve_prompt("Standardize suffix", standardize_suffix_default)
  opts$`standardize-into` <- resolve_prompt("Standardize output names (var=newname|var2=newname2)", "")
  opts$`percentile-bins` <- resolve_prompt("Percentile bins (var=4|var2=5)", "")
  percentile_suffix_default <- resolve_config_value("modules.data_transform.percentile_suffix", "_pct")
  opts$`percentile-suffix` <- resolve_prompt("Percentile bins suffix", percentile_suffix_default)
  opts$`percentile-into` <- resolve_prompt("Percentile bin output names (var=newname|var2=newname2)", "")
  opts$bins <- resolve_prompt("Custom bins (var=0,10,20|var2=5,15,25)", "")
  bins_suffix_default <- resolve_config_value("modules.data_transform.bins_suffix", "_bin")
  opts$`bins-suffix` <- resolve_prompt("Custom bins suffix", bins_suffix_default)
  opts$`bins-into` <- resolve_prompt("Custom bin output names (var=newname|var2=newname2)", "")
  opts$recode <- resolve_prompt("Recodes (var=1:0,2:1|var2=low:0,high:1)", "")
  recode_suffix_default <- resolve_config_value("modules.data_transform.recode_suffix", "_rec")
  opts$`recode-suffix` <- resolve_prompt("Recode suffix", recode_suffix_default)
  opts$`recode-into` <- resolve_prompt("Recode output names (var=newname|var2=newname2)", "")
  opts$rename <- resolve_prompt("Rename variables (old:new,old2:new2)", "")
  opts$drop <- resolve_prompt("Drop variables (comma-separated)", "")
  coerce_default <- resolve_config_value("modules.data_transform.coerce", FALSE)
  overwrite_default <- resolve_config_value("modules.data_transform.overwrite_vars", FALSE)
  confirm_overwrite_default <- resolve_config_value("modules.data_transform.confirm_overwrite", FALSE)
  confirm_drop_default <- resolve_config_value("modules.data_transform.confirm_drop", FALSE)
  opts$coerce <- resolve_prompt("Coerce non-numeric vars for transforms TRUE/FALSE", ifelse(isTRUE(coerce_default), "TRUE", "FALSE"))
  opts$`overwrite-vars` <- resolve_prompt("Allow overwriting variables TRUE/FALSE", ifelse(isTRUE(overwrite_default), "TRUE", "FALSE"))
  opts$`confirm-overwrite` <- resolve_prompt(
    "Confirm overwriting variables TRUE/FALSE",
    ifelse(isTRUE(confirm_overwrite_default), "TRUE", "FALSE")
  )
  opts$`confirm-drop` <- resolve_prompt("Confirm dropping variables TRUE/FALSE", ifelse(isTRUE(confirm_drop_default), "TRUE", "FALSE"))
  opts$`user-prompt` <- resolve_prompt("User prompt (optional)", "")
  log_default <- resolve_config_value("defaults.log", TRUE)
  opts$log <- resolve_prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))
  opts$out <- resolve_prompt("Output directory", resolve_default_out())
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

resolve_dt_bool <- function(value, key, fallback = FALSE) {
  default_val <- resolve_config_value(paste0("modules.data_transform.", key), fallback)
  resolve_parse_bool(value, default = default_val)
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

resolve_append_apa_report <- function(path, analysis_label, apa_table, apa_text, analysis_flags = NULL, template_path = NULL) {
  if (exists("append_apa_report", mode = "function")) {
    return(get("append_apa_report", mode = "function")(
      path,
      analysis_label,
      apa_table,
      apa_text,
      analysis_flags = analysis_flags,
      template_path = template_path
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

resolve_get_user_prompt <- function(opts) {
  if (exists("get_user_prompt", mode = "function")) {
    return(get("get_user_prompt", mode = "function")(opts))
  }
  NULL
}

split_commas <- function(value) {
  if (is.null(value) || is.logical(value)) return(character(0))
  value <- as.character(value)
  if (value == "") return(character(0))
  trimws(strsplit(value, ",", fixed = TRUE)[[1]])
}

split_pipe <- function(value) {
  if (is.null(value) || is.logical(value)) return(character(0))
  value <- as.character(value)
  if (value == "") return(character(0))
  trimws(strsplit(value, "\\|")[[1]])
}

is_numeric_string <- function(value) {
  grepl("^[+-]?[0-9]*\\.?[0-9]+([eE][+-]?[0-9]+)?$", value)
}

parse_value <- function(value) {
  val <- trimws(value)
  if (val == "") return(NA)
  lower <- tolower(val)
  if (lower %in% c("na", "null")) return(NA)
  if ((startsWith(val, "'") && endsWith(val, "'")) || (startsWith(val, "\"") && endsWith(val, "\""))) {
    return(substr(val, 2, nchar(val) - 1))
  }
  if (is_numeric_string(val)) return(as.numeric(val))
  val
}

parse_calc_rules <- function(value) {
  items <- split_pipe(value)
  rules <- list()
  for (item in items) {
    if (item == "") next
    parts <- strsplit(item, "=", fixed = TRUE)[[1]]
    if (length(parts) < 2) stop("Invalid --calc rule: ", item)
    new_var <- trimws(parts[1])
    expr <- trimws(paste(parts[-1], collapse = "="))
    if (new_var == "" || expr == "") stop("Invalid --calc rule: ", item)
    rules[[length(rules) + 1]] <- list(var = new_var, expr = expr)
  }
  rules
}

parse_transform_rules <- function(value) {
  items <- split_pipe(value)
  rules <- list()
  for (item in items) {
    if (item == "") next
    parts <- strsplit(item, "=", fixed = TRUE)[[1]]
    if (length(parts) < 2) stop("Invalid --transform rule: ", item)
    var <- trimws(parts[1])
    fn <- trimws(paste(parts[-1], collapse = "="))
    if (var == "" || fn == "") stop("Invalid --transform rule: ", item)
    rules[[length(rules) + 1]] <- list(var = var, fn = fn)
  }
  rules
}

parse_recode_rules <- function(value) {
  items <- split_pipe(value)
  rules <- list()
  for (item in items) {
    if (item == "") next
    parts <- strsplit(item, "=", fixed = TRUE)[[1]]
    if (length(parts) < 2) stop("Invalid --recode rule: ", item)
    var <- trimws(parts[1])
    map_str <- trimws(paste(parts[-1], collapse = "="))
    if (var == "" || map_str == "") stop("Invalid --recode rule: ", item)
    pairs <- split_commas(map_str)
    mapping <- list()
    for (pair in pairs) {
      if (pair == "") next
      if (grepl("->", pair, fixed = TRUE)) {
        kv <- strsplit(pair, "->", fixed = TRUE)[[1]]
      } else {
        kv <- strsplit(pair, ":", fixed = TRUE)[[1]]
      }
      if (length(kv) < 2) stop("Invalid recode pair: ", pair)
      mapping[[length(mapping) + 1]] <- list(old = parse_value(kv[1]), new = parse_value(kv[2]))
    }
    if (length(mapping) == 0) stop("No recode pairs provided for ", var)
    rules[[length(rules) + 1]] <- list(var = var, mapping = mapping)
  }
  rules
}

parse_percentile_rules <- function(value) {
  items <- split_pipe(value)
  rules <- list()
  for (item in items) {
    if (item == "") next
    parts <- strsplit(item, "=", fixed = TRUE)[[1]]
    if (length(parts) < 2) stop("Invalid --percentile-bins rule: ", item)
    var <- trimws(parts[1])
    bins <- trimws(paste(parts[-1], collapse = "="))
    if (var == "" || bins == "") stop("Invalid --percentile-bins rule: ", item)
    bins_num <- suppressWarnings(as.integer(bins))
    if (is.na(bins_num) || bins_num < 2) stop("Percentile bins must be an integer >= 2 for ", var)
    rules[[length(rules) + 1]] <- list(var = var, bins = bins_num)
  }
  rules
}

parse_bins_rules <- function(value) {
  items <- split_pipe(value)
  rules <- list()
  for (item in items) {
    if (item == "") next
    parts <- strsplit(item, "=", fixed = TRUE)[[1]]
    if (length(parts) < 2) stop("Invalid --bins rule: ", item)
    var <- trimws(parts[1])
    breaks_str <- trimws(paste(parts[-1], collapse = "="))
    if (var == "" || breaks_str == "") stop("Invalid --bins rule: ", item)
    breaks_vals <- split_commas(breaks_str)
    breaks_num <- suppressWarnings(as.numeric(breaks_vals))
    if (length(breaks_num) < 2 || any(is.na(breaks_num))) stop("Custom bins require numeric breakpoints for ", var)
    rules[[length(rules) + 1]] <- list(var = var, breaks = breaks_num)
  }
  rules
}

parse_rename_map <- function(value) {
  items <- split_commas(value)
  mapping <- list()
  for (item in items) {
    if (item == "") next
    if (grepl("->", item, fixed = TRUE)) {
      parts <- strsplit(item, "->", fixed = TRUE)[[1]]
    } else {
      parts <- strsplit(item, ":", fixed = TRUE)[[1]]
    }
    if (length(parts) < 2) stop("Invalid --rename pair: ", item)
    old <- trimws(parts[1])
    new <- trimws(parts[2])
    if (old == "" || new == "") stop("Invalid --rename pair: ", item)
    mapping[[old]] <- new
  }
  mapping
}

parse_into_map <- function(value) {
  items <- split_pipe(value)
  mapping <- list()
  for (item in items) {
    if (item == "") next
    parts <- strsplit(item, "=", fixed = TRUE)[[1]]
    if (length(parts) < 2) stop("Invalid map: ", item)
    key <- trimws(parts[1])
    val <- trimws(paste(parts[-1], collapse = "="))
    if (key == "" || val == "") stop("Invalid map: ", item)
    mapping[[key]] <- val
  }
  mapping
}

normalize_transform <- function(fn) {
  lower <- tolower(fn)
  if (lower %in% c("z", "zscore", "standardize")) return("scale")
  lower
}

get_transform_function <- function(fn) {
  if (fn == "log") return(function(x) log(x))
  if (fn == "log10") return(function(x) log10(x))
  if (fn == "sqrt") return(function(x) sqrt(x))
  if (fn == "exp") return(function(x) exp(x))
  if (fn == "abs") return(function(x) abs(x))
  if (fn == "center") return(function(x) x - mean(x, na.rm = TRUE))
  if (fn == "scale") return(function(x) as.numeric(scale(x)))
  stop("Unknown transform function: ", fn)
}

confirm_action <- function(opts, message, confirm_flag) {
  if (resolve_parse_bool(opts$interactive, FALSE)) {
    answer <- resolve_prompt(paste0(message, " (yes/no)"), "no")
    if (!tolower(answer) %in% c("yes", "y")) stop("Operation cancelled.")
    return(TRUE)
  }
  if (resolve_dt_bool(opts[[confirm_flag]], gsub("-", "_", confirm_flag), FALSE)) return(TRUE)
  stop(paste0(message, " Use --", confirm_flag, " or --interactive."))
}

ensure_target_name <- function(df, target, opts, action_label) {
  if (!(target %in% names(df))) return(TRUE)
  if (!resolve_dt_bool(opts$`overwrite-vars`, "overwrite_vars", FALSE) && !resolve_parse_bool(opts$interactive, FALSE)) {
    stop("Target variable already exists: ", target, ". Use --overwrite-vars to allow overwriting.")
  }
  confirm_action(opts, paste0(action_label, " will overwrite existing variable '", target, "'."), "confirm-overwrite")
  TRUE
}

coerce_numeric <- function(vec, var, opts) {
  if (is.numeric(vec)) return(list(vec = vec, note = ""))
  if (!resolve_dt_bool(opts$coerce, "coerce", FALSE)) stop("Variable '", var, "' is not numeric. Use --coerce to convert.")
  converted <- suppressWarnings(as.numeric(vec))
  introduced_na <- any(is.na(converted) & !is.na(vec))
  note <- if (introduced_na) "coerced with NAs introduced" else "coerced"
  list(vec = converted, note = note)
}

apply_recode <- function(vec, mapping) {
  out <- vec
  for (pair in mapping) {
    old <- pair$old
    new <- pair$new
    if (is.na(old)) {
      idx <- is.na(out)
    } else if (is.numeric(out) && is.numeric(old)) {
      idx <- out == old
    } else {
      idx <- as.character(out) == as.character(old)
    }
    out[idx] <- new
  }
  out
}

build_percentile_bins <- function(vec, bins) {
  if (all(is.na(vec))) stop("Percentile bins require at least one non-missing value.")
  probs <- seq(0, 1, length.out = bins + 1)
  qs <- as.numeric(quantile(vec, probs = probs, na.rm = TRUE, type = 7))
  uniq <- unique(qs)
  if (length(uniq) < 2) stop("Not enough unique percentile breakpoints.")
  list(breaks = uniq, reduced = length(uniq) < length(qs))
}

build_custom_bins <- function(breaks) {
  sorted <- sort(breaks)
  if (any(duplicated(sorted))) stop("Custom bins contain duplicate breakpoints.")
  list(breaks = sorted, sorted = !identical(breaks, sorted))
}

make_markdown_table <- function(df) {
  if (nrow(df) == 0) {
    return(c("| Step | Action | Variable | New_Variable | Details |",
             "| --- | --- | --- | --- | --- |",
             "| 1 | none |  |  | No transformations applied. |"))
  }
  df[is.na(df)] <- ""
  header <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep <- paste0("| ", paste(rep("---", ncol(df)), collapse = " | "), " |")
  rows <- apply(df, 1, function(row) paste0("| ", paste(row, collapse = " | "), " |"))
  c(header, sep, rows)
}

args <- commandArgs(trailingOnly = TRUE)
opts <- resolve_parse_args(args)

if (!is.null(opts$help)) {
  print_usage()
  quit(status = 0)
}

if (!is.null(opts$interactive)) {
  opts <- modifyList(opts, interactive_options())
  opts$interactive <- TRUE
}

df <- resolve_load_dataframe(opts)

out_dir <- resolve_ensure_out_dir(if (!is.null(opts$out)) opts$out else resolve_default_out())

calc_rules <- if (!is.null(opts$calc)) parse_calc_rules(opts$calc) else list()
transform_rules <- if (!is.null(opts$transform)) parse_transform_rules(opts$transform) else list()
standardize_vars <- split_commas(opts$standardize)
percentile_rules <- if (!is.null(opts$`percentile-bins`)) parse_percentile_rules(opts$`percentile-bins`) else list()
bins_rules <- if (!is.null(opts$bins)) parse_bins_rules(opts$bins) else list()
recode_rules <- if (!is.null(opts$recode)) parse_recode_rules(opts$recode) else list()
rename_map <- if (!is.null(opts$rename)) parse_rename_map(opts$rename) else list()
drop_vars <- split_commas(opts$drop)

transform_into <- if (!is.null(opts$`transform-into`)) parse_into_map(opts$`transform-into`) else list()
standardize_into <- if (!is.null(opts$`standardize-into`)) parse_into_map(opts$`standardize-into`) else list()
percentile_into <- if (!is.null(opts$`percentile-into`)) parse_into_map(opts$`percentile-into`) else list()
bins_into <- if (!is.null(opts$`bins-into`)) parse_into_map(opts$`bins-into`) else list()
recode_into <- if (!is.null(opts$`recode-into`)) parse_into_map(opts$`recode-into`) else list()

standardize_suffix_default <- resolve_config_value("modules.data_transform.standardize_suffix", "_z")
percentile_suffix_default <- resolve_config_value("modules.data_transform.percentile_suffix", "_pct")
bins_suffix_default <- resolve_config_value("modules.data_transform.bins_suffix", "_bin")
recode_suffix_default <- resolve_config_value("modules.data_transform.recode_suffix", "_rec")
standardize_suffix <- if (!is.null(opts$`standardize-suffix`)) opts$`standardize-suffix` else standardize_suffix_default
percentile_suffix <- if (!is.null(opts$`percentile-suffix`)) opts$`percentile-suffix` else percentile_suffix_default
bins_suffix <- if (!is.null(opts$`bins-suffix`)) opts$`bins-suffix` else bins_suffix_default
recode_suffix <- if (!is.null(opts$`recode-suffix`)) opts$`recode-suffix` else recode_suffix_default

log_rows <- list()
add_log <- function(action, variable, new_variable, details, note = "") {
  log_rows[[length(log_rows) + 1]] <<- data.frame(
    action = action,
    variable = variable,
    new_variable = new_variable,
    details = details,
    note = note,
    stringsAsFactors = FALSE
  )
}

if (length(calc_rules) > 0) {
  for (rule in calc_rules) {
    ensure_target_name(df, rule$var, opts, "Calculation")
    result <- tryCatch(with(df, eval(parse(text = rule$expr))), error = function(e) e)
    if (inherits(result, "error")) stop("Failed to evaluate calc expression for ", rule$var, ": ", result$message)
    if (length(result) == 1) result <- rep(result, nrow(df))
    if (length(result) != nrow(df)) stop("Calc result length does not match rows for ", rule$var)
    df[[rule$var]] <- result
    add_log("calc", rule$var, rule$var, rule$expr)
  }
}

if (length(transform_rules) > 0) {
  for (rule in transform_rules) {
    var <- rule$var
    if (!(var %in% names(df))) stop("Unknown variable for transform: ", var)
    fn <- normalize_transform(rule$fn)
    target <- if (!is.null(transform_into[[var]])) {
      transform_into[[var]]
    } else if (fn == "scale") {
      paste0(var, standardize_suffix)
    } else {
      paste0(fn, "_", var)
    }
    if (target == var) {
      if (!resolve_dt_bool(opts$`overwrite-vars`, "overwrite_vars", FALSE) && !resolve_parse_bool(opts$interactive, FALSE)) {
        stop("Transform target matches source and overwrite is disabled for ", var)
      }
      confirm_action(opts, paste0("Transform will overwrite variable '", var, "'."), "confirm-overwrite")
    } else {
      ensure_target_name(df, target, opts, "Transform")
    }
    coerced <- coerce_numeric(df[[var]], var, opts)
    fn_apply <- get_transform_function(fn)
    df[[target]] <- fn_apply(coerced$vec)
    add_log("transform", var, target, fn, coerced$note)
  }
}

if (length(standardize_vars) > 0) {
  for (var in standardize_vars) {
    if (var == "") next
    if (!(var %in% names(df))) stop("Unknown variable for standardize: ", var)
    target <- if (!is.null(standardize_into[[var]])) {
      standardize_into[[var]]
    } else {
      paste0(var, standardize_suffix)
    }
    if (target == var) {
      if (!resolve_dt_bool(opts$`overwrite-vars`, "overwrite_vars", FALSE) && !resolve_parse_bool(opts$interactive, FALSE)) {
        stop("Standardize target matches source and overwrite is disabled for ", var)
      }
      confirm_action(opts, paste0("Standardize will overwrite variable '", var, "'."), "confirm-overwrite")
    } else {
      ensure_target_name(df, target, opts, "Standardize")
    }
    coerced <- coerce_numeric(df[[var]], var, opts)
    df[[target]] <- as.numeric(scale(coerced$vec))
    add_log("standardize", var, target, paste0("z-score (suffix ", standardize_suffix, ")"), coerced$note)
  }
}

if (length(recode_rules) > 0) {
  for (rule in recode_rules) {
    var <- rule$var
    if (!(var %in% names(df))) stop("Unknown variable for recode: ", var)
    target <- if (!is.null(recode_into[[var]])) {
      recode_into[[var]]
    } else {
      paste0(var, recode_suffix)
    }
    if (target == var) {
      if (!resolve_dt_bool(opts$`overwrite-vars`, "overwrite_vars", FALSE) && !resolve_parse_bool(opts$interactive, FALSE)) {
        stop("Recode target matches source and overwrite is disabled for ", var)
      }
      confirm_action(opts, paste0("Recode will overwrite variable '", var, "'."), "confirm-overwrite")
    } else {
      ensure_target_name(df, target, opts, "Recode")
    }
    df[[target]] <- apply_recode(df[[var]], rule$mapping)
    add_log("recode", var, target, paste0("pairs=", length(rule$mapping)))
  }
}

if (length(percentile_rules) > 0) {
  for (rule in percentile_rules) {
    var <- rule$var
    if (!(var %in% names(df))) stop("Unknown variable for percentile bins: ", var)
    target <- if (!is.null(percentile_into[[var]])) {
      percentile_into[[var]]
    } else {
      paste0(var, percentile_suffix)
    }
    if (target == var) {
      if (!resolve_dt_bool(opts$`overwrite-vars`, "overwrite_vars", FALSE) && !resolve_parse_bool(opts$interactive, FALSE)) {
        stop("Percentile bins target matches source and overwrite is disabled for ", var)
      }
      confirm_action(opts, paste0("Percentile bins will overwrite variable '", var, "'."), "confirm-overwrite")
    } else {
      ensure_target_name(df, target, opts, "Percentile bins")
    }
    coerced <- coerce_numeric(df[[var]], var, opts)
    bin_info <- build_percentile_bins(coerced$vec, rule$bins)
    df[[target]] <- cut(coerced$vec, breaks = bin_info$breaks, include.lowest = TRUE, right = TRUE, labels = FALSE)
    detail <- paste0("bins=", rule$bins)
    note <- coerced$note
    if (bin_info$reduced) {
      reduced_bins <- length(bin_info$breaks) - 1
      note <- trimws(paste(note, paste0("reduced to ", reduced_bins, " bins")))
    }
    add_log("percentile_bin", var, target, detail, note)
  }
}

if (length(bins_rules) > 0) {
  for (rule in bins_rules) {
    var <- rule$var
    if (!(var %in% names(df))) stop("Unknown variable for custom bins: ", var)
    target <- if (!is.null(bins_into[[var]])) {
      bins_into[[var]]
    } else {
      paste0(var, bins_suffix)
    }
    if (target == var) {
      if (!resolve_dt_bool(opts$`overwrite-vars`, "overwrite_vars", FALSE) && !resolve_parse_bool(opts$interactive, FALSE)) {
        stop("Custom bins target matches source and overwrite is disabled for ", var)
      }
      confirm_action(opts, paste0("Custom bins will overwrite variable '", var, "'."), "confirm-overwrite")
    } else {
      ensure_target_name(df, target, opts, "Custom bins")
    }
    coerced <- coerce_numeric(df[[var]], var, opts)
    bin_info <- build_custom_bins(rule$breaks)
    df[[target]] <- cut(coerced$vec, breaks = bin_info$breaks, include.lowest = TRUE, right = TRUE, labels = FALSE)
    detail <- paste0("breaks=", paste(bin_info$breaks, collapse = ","))
    note <- coerced$note
    if (bin_info$sorted) {
      note <- trimws(paste(note, "sorted breaks"))
    }
    add_log("bin", var, target, detail, note)
  }
}

if (length(rename_map) > 0) {
  for (old in names(rename_map)) {
    new <- rename_map[[old]]
    if (!(old %in% names(df))) stop("Unknown variable for rename: ", old)
    if (old == new) next
    if (new %in% names(df)) {
      if (!resolve_dt_bool(opts$`overwrite-vars`, "overwrite_vars", FALSE) && !resolve_parse_bool(opts$interactive, FALSE)) {
        stop("Rename target already exists: ", new, ". Use --overwrite-vars to allow.")
      }
      confirm_action(opts, paste0("Rename will overwrite variable '", new, "'."), "confirm-overwrite")
      df[[new]] <- NULL
    }
    names(df)[names(df) == old] <- new
    add_log("rename", old, new, "rename")
  }
}

if (length(drop_vars) > 0) {
  drop_vars <- drop_vars[drop_vars != ""]
  if (length(drop_vars) > 0) {
    missing_drop <- setdiff(drop_vars, names(df))
    if (length(missing_drop) > 0) stop("Unknown variables to drop: ", paste(missing_drop, collapse = ", "))
    confirm_action(opts, paste0("Drop variables: ", paste(drop_vars, collapse = ", "), "."), "confirm-drop")
    df[drop_vars] <- NULL
    for (var in drop_vars) {
      add_log("drop", var, "", "dropped")
    }
  }
}

log_df <- if (length(log_rows) > 0) do.call(rbind, log_rows) else {
  data.frame(action = character(0), variable = character(0), new_variable = character(0), details = character(0), note = character(0), stringsAsFactors = FALSE)
}

saveRDS(df, file.path(out_dir, "transformed_data.rds"))

if (nrow(log_df) == 0) {
  apa_text <- "No transformations applied. Data exported unchanged."
} else {
  total_steps <- nrow(log_df)
  actions <- unique(log_df$action)
  sentences <- c(sprintf("Data transformations were applied in %d step%s.", total_steps, ifelse(total_steps == 1, "", "s")))
  if ("calc" %in% actions) {
    vars <- unique(log_df$new_variable[log_df$action == "calc"])
    sentences <- c(sentences, paste0("Derived variables created: ", paste(vars, collapse = ", "), "."))
  }
  if ("transform" %in% actions) {
    vars <- unique(log_df$new_variable[log_df$action == "transform"])
    sentences <- c(sentences, paste0("Transformed variables added: ", paste(vars, collapse = ", "), "."))
  }
  if ("standardize" %in% actions) {
    vars <- unique(log_df$new_variable[log_df$action == "standardize"])
    sentences <- c(sentences, paste0("Standardized variables added: ", paste(vars, collapse = ", "), "."))
  }
  if ("recode" %in% actions) {
    vars <- unique(log_df$new_variable[log_df$action == "recode"])
    sentences <- c(sentences, paste0("Recoded variables added: ", paste(vars, collapse = ", "), "."))
  }
  if ("percentile_bin" %in% actions) {
    vars <- unique(log_df$new_variable[log_df$action == "percentile_bin"])
    sentences <- c(sentences, paste0("Percentile bins created: ", paste(vars, collapse = ", "), "."))
  }
  if ("bin" %in% actions) {
    vars <- unique(log_df$new_variable[log_df$action == "bin"])
    sentences <- c(sentences, paste0("Custom bins created: ", paste(vars, collapse = ", "), "."))
  }
  if ("rename" %in% actions) {
    vars <- unique(paste0(log_df$variable[log_df$action == "rename"], "->", log_df$new_variable[log_df$action == "rename"]))
    sentences <- c(sentences, paste0("Variables renamed: ", paste(vars, collapse = ", "), "."))
  }
  if ("drop" %in% actions) {
    vars <- unique(log_df$variable[log_df$action == "drop"])
    sentences <- c(sentences, paste0("Variables dropped: ", paste(vars, collapse = ", "), "."))
  }
  apa_text <- paste(sentences, collapse = " ")
}

table_df <- if (nrow(log_df) == 0) {
  data.frame(
    Step = 1,
    Action = "none",
    Variable = "",
    New_Variable = "",
    Details = "No transformations applied.",
    stringsAsFactors = FALSE
  )
} else {
  data.frame(
    Step = seq_len(nrow(log_df)),
    Action = log_df$action,
    Variable = log_df$variable,
    New_Variable = log_df$new_variable,
    Details = log_df$details,
    stringsAsFactors = FALSE
  )
}

apa_table <- make_markdown_table(table_df)
resolve_append_apa_report(file.path(out_dir, "apa_report.md"), "Data transformation", apa_table, apa_text)

log_default <- resolve_config_value("defaults.log", TRUE)
if (resolve_parse_bool(opts$log, default = log_default)) {
  ctx <- resolve_get_run_context()
  resolve_append_analysis_log(
    out_dir,
    module = "data_transform",
    prompt = ctx$prompt,
    commands = ctx$commands,
    results = list(transformed_df = df, transform_log_df = log_df),
    options = list(
      calc = opts$calc,
      transform = opts$transform,
      standardize = opts$standardize,
      percentile_bins = opts$`percentile-bins`,
      bins = opts$bins,
      recode = opts$recode,
      rename = opts$rename,
      drop = opts$drop,
      overwrite_vars = resolve_dt_bool(opts$`overwrite-vars`, "overwrite_vars", FALSE)
    ),
    user_prompt = resolve_get_user_prompt(opts)
  )
}
