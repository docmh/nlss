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
source_lib("data_change.R")

print_usage <- function() {
  cat("Project selection: --project DIR [--dataset NAME] (default: nearest ancestor).\n")
  cat("Data transformation (base R)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript data_transform.R --csv data.csv [--calc \"newvar=expr\"] [--transform \"var=log\"] [--standardize var1,var2]\n")
  cat("  Rscript data_transform.R --sav data.sav [--recode \"var=1:0,2:1\"] [--rename old:new] [--drop var1,var2]\n")
  cat("  Rscript data_transform.R --rds data.rds\n")
  cat("  Rscript data_transform.R --rdata data.RData --df data_frame_name [--interactive]\n")
  cat("  Rscript data_transform.R --parquet data.parquet\n")
  cat("\n")
  cat("Options:\n")
  cat("  --csv PATH               CSV input file\n")
  cat("  --sav PATH               SPSS .sav input file\n")
  cat("  --sep VALUE              CSV separator (default: ,)\n")
  cat("  --header TRUE/FALSE      CSV header (default: TRUE)\n")
  cat("  --rds PATH               RDS input file (data frame)\n")
  cat("  --rdata PATH             RData input file\n")
  cat("  --parquet PATH           Parquet input file\n")
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
  cat("  --template REF            Template path or template key (optional)\n")
  cat("  --user-prompt TEXT        Original AI user prompt for logging (optional)\n")
  cat("  --log TRUE/FALSE          Write analysis_log.jsonl (default: TRUE)\n")
  cat("  --interactive             Prompt for inputs\n")
  cat("  --help                    Show this help\n")
  cat("  General --calc expressions remain available but may be non-replayable.\n")
  cat("  Mandatory run bundles are written even with --log FALSE.\n")
  print_import_usage()
}


interactive_options <- function() {
  cat("Interactive input selected.\n")
  input_type <- prompt("Input type (csv/sav/rds/rdata/parquet)", "csv")
  input_type <- tolower(input_type)
  opts <- list()

  if (input_type == "csv") {
    opts$csv <- prompt("CSV path")
    sep_default <- get_config_value("defaults.csv.sep")
    header_default <- get_config_value("defaults.csv.header")
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

  opts$calc <- prompt("Calculated variables (newvar=expr|newvar2=expr)", "")
  opts$transform <- prompt("Transforms (var=log|var2=sqrt|var3=scale)", "")
  opts$`transform-into` <- prompt("Transform output names (var=newname|var2=newname2)", "")
  opts$standardize <- prompt("Standardize variables (comma-separated)", "")
  standardize_suffix_default <- get_config_value("modules.data_transform.standardize_suffix")
  opts$`standardize-suffix` <- prompt("Standardize suffix", standardize_suffix_default)
  opts$`standardize-into` <- prompt("Standardize output names (var=newname|var2=newname2)", "")
  opts$`percentile-bins` <- prompt("Percentile bins (var=4|var2=5)", "")
  percentile_suffix_default <- get_config_value("modules.data_transform.percentile_suffix")
  opts$`percentile-suffix` <- prompt("Percentile bins suffix", percentile_suffix_default)
  opts$`percentile-into` <- prompt("Percentile bin output names (var=newname|var2=newname2)", "")
  opts$bins <- prompt("Custom bins (var=0,10,20|var2=5,15,25)", "")
  bins_suffix_default <- get_config_value("modules.data_transform.bins_suffix")
  opts$`bins-suffix` <- prompt("Custom bins suffix", bins_suffix_default)
  opts$`bins-into` <- prompt("Custom bin output names (var=newname|var2=newname2)", "")
  opts$recode <- prompt("Recodes (var=1:0,2:1|var2=low:0,high:1)", "")
  recode_suffix_default <- get_config_value("modules.data_transform.recode_suffix")
  opts$`recode-suffix` <- prompt("Recode suffix", recode_suffix_default)
  opts$`recode-into` <- prompt("Recode output names (var=newname|var2=newname2)", "")
  opts$rename <- prompt("Rename variables (old:new,old2:new2)", "")
  opts$drop <- prompt("Drop variables (comma-separated)", "")
  coerce_default <- get_config_value("modules.data_transform.coerce")
  overwrite_default <- get_config_value("modules.data_transform.overwrite_vars")
  confirm_overwrite_default <- get_config_value("modules.data_transform.confirm_overwrite")
  confirm_drop_default <- get_config_value("modules.data_transform.confirm_drop")
  opts$coerce <- prompt("Coerce non-numeric vars for transforms TRUE/FALSE", ifelse(isTRUE(coerce_default), "TRUE", "FALSE"))
  opts$`overwrite-vars` <- prompt("Allow overwriting variables TRUE/FALSE", ifelse(isTRUE(overwrite_default), "TRUE", "FALSE"))
  opts$`confirm-overwrite` <- prompt(
    "Confirm overwriting variables TRUE/FALSE",
    ifelse(isTRUE(confirm_overwrite_default), "TRUE", "FALSE")
  )
  opts$`confirm-drop` <- prompt("Confirm dropping variables TRUE/FALSE", ifelse(isTRUE(confirm_drop_default), "TRUE", "FALSE"))
  opts$template <- prompt("Template (path or key; blank for default)", "")
  opts$`user-prompt` <- prompt("User prompt (optional)", "")
  log_default <- get_config_value("defaults.log")
  opts$log <- prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))
  opts
}

dt_bool <- function(opts, key) {
  parse_bool(opts[[key]], get_config_value(paste0("modules.data_transform.", gsub("-", "_", key))))
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

dt_numeric_string <- function(value) {
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
  if (dt_numeric_string(val)) return(as.numeric(val))
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
    bins_num <- suppressWarnings(as.numeric(bins))
    if (!is.finite(bins_num) || bins_num < 2 || bins_num != floor(bins_num) || bins_num >= .Machine$integer.max) {
      stop("Percentile bins must be an integer >= 2 for ", var)
    }
    rules[[length(rules) + 1]] <- list(var = var, bins = as.integer(bins_num))
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
  if (parse_bool(opts$interactive, FALSE)) {
    answer <- prompt(paste0(message, " (yes/no)"), "no")
    if (!tolower(answer) %in% c("yes", "y")) stop("Operation cancelled.")
    return(TRUE)
  }
  if (dt_bool(opts, confirm_flag)) return(TRUE)
  stop(paste0(message, " Use --", confirm_flag, " or --interactive."))
}

ensure_target_name <- function(df, target, opts, action_label) {
  if (!(target %in% names(df))) return(TRUE)
  if (!dt_bool(opts, "overwrite-vars") && !parse_bool(opts$interactive, FALSE)) {
    stop("Target variable already exists: ", target, ". Use --overwrite-vars to allow overwriting.")
  }
  confirm_action(opts, paste0(action_label, " will overwrite existing variable '", target, "'."), "confirm-overwrite")
  TRUE
}

coerce_numeric <- function(vec, var, opts) {
  if (is.numeric(vec)) return(list(vec = vec, note = ""))
  if (!dt_bool(opts, "coerce")) stop("Variable '", var, "' is not numeric. Use --coerce to convert.")
  converted <- suppressWarnings(as.numeric(if (is.factor(vec)) as.character(vec) else vec))
  introduced_na <- any(is.na(converted) & !is.na(vec))
  note <- if (introduced_na) "coerced with NAs introduced" else "coerced"
  list(vec = converted, note = note)
}

apply_recode <- function(vec, mapping) {
  # Every match uses the original values, not earlier replacements. Factors
  # become text so a valid new category cannot silently become missing.
  source <- if (is.factor(vec)) as.character(vec) else vec
  out <- source
  seen <- list()
  matched <- rep(FALSE, length(source))
  for (pair in mapping) {
    old <- pair$old
    new <- pair$new
    duplicate <- any(vapply(seen, function(value) {
      if (is.na(old) || is.na(value)) return(is.na(old) && is.na(value))
      isTRUE(all.equal(old, value, tolerance = 0, check.attributes = FALSE))
    }, logical(1)))
    if (duplicate) stop("Duplicate source value in recode mapping: ", as.character(old))
    seen <- c(seen, list(old))
    if (is.na(old)) {
      idx <- is.na(source)
    } else if (is.numeric(source)) {
      # Quoting a numeric code must not route through display-rounded strings.
      # Incompatible text has no numeric match; original values stay unchanged.
      code <- suppressWarnings(as.numeric(old))
      idx <- if (is.na(code)) rep(FALSE, length(source)) else !is.na(source) & source == code
    } else {
      idx <- !is.na(source) & as.character(source) == as.character(old)
    }
    if (any(matched & idx)) stop("Ambiguous recode mappings match the same source observation.")
    matched <- matched | idx
    out[idx] <- new
  }
  out
}

calc_functions <- function() {
  keys <- c("(", "+", "-", "*", "/", "^", "%%", "%/%", ":", "==", "!=", "<", "<=", ">", ">=", "!", "&",
    "abs", "sqrt", "log", "log10", "log2", "log1p", "exp", "expm1", "round", "signif", "floor", "ceiling", "trunc", "sign",
    "sum", "prod", "mean", "min", "max", "pmin", "pmax", "ifelse", "is.na", "is.finite", "is.nan", "is.infinite",
    "as.numeric", "as.integer", "as.character", "as.logical", "length", "seq_along", "rep", "c")
  c(setNames(lapply(keys, get, envir = baseenv(), inherits = FALSE), keys), list(sd = stats::sd, var = stats::var))
}

classify_calculations <- function(rules, df) {
  known <- names(df)[vapply(df, function(x) is.atomic(x) && !is.object(x) && is.null(dim(x)), logical(1))]
  column_names <- names(df)
  functions <- names(calc_functions())
  constants <- c("TRUE", "FALSE", "NA", "NA_real_", "NA_integer_", "NA_character_", "NaN", "Inf", "pi")
  classifications <- list()
  for (rule in rules) {
    expression <- tryCatch(parse(text = rule$expr), error = function(e) stop("Invalid calc expression for ", rule$var, ": ", conditionMessage(e)))
    unknown <- character()
    eligible_node <- function(node) {
      if (is.atomic(node) && length(node) == 1L) return(TRUE)
      if (is.symbol(node)) {
        name <- as.character(node)
        if (name %in% c(known, setdiff(constants, column_names))) return(TRUE)
        unknown <<- c(unknown, name)
        return(FALSE)
      }
      if (!is.call(node) || !is.symbol(node[[1]]) || !as.character(node[[1]]) %in% functions) return(FALSE)
      all(vapply(as.list(node)[-1], eligible_node, logical(1)))
    }
    eligible <- length(expression) == 1L && eligible_node(expression[[1]])
    classifications[[length(classifications) + 1L]] <- list(variable = rule$var, expression = rule$expr,
      eligible = eligible, classification = if (eligible) "deterministic_plain_vector_expression" else "general_R_expression",
      reason = if (eligible) "Only explicitly bound arithmetic/base functions and plain input vectors."
        else "Expression or dependency is outside the conservative replay whitelist; arbitrary code remains available, not sandboxed.",
      unverified_symbols = unique(unknown))
    known <- setdiff(known, rule$var)
    column_names <- union(column_names, rule$var)
    if (eligible) known <- c(known, rule$var)
  }
  eligible <- all(vapply(classifications, function(x) x$eligible, logical(1)))
  list(eligible = eligible,
    reason = if (eligible) "Deterministic built-in operations and verified plain-vector calculations."
      else "General R calculations may depend on unrecorded state or produce external side effects; automatic replay is disabled.",
    calc_classification = classifications)
}

evaluate_calculation <- function(rule, df, eligible) {
  expression <- parse(text = rule$expr)
  if (!eligible) return(with(df, eval(expression)))
  # This explicit environment avoids accidentally binding a same-named global
  # function. It is a replay boundary, not a sandbox for unrestricted --calc.
  functions <- list2env(calc_functions(), parent = emptyenv())
  for (name in c("TRUE", "FALSE", "NA", "NA_real_", "NA_integer_", "NA_character_", "NaN", "Inf", "pi")) {
    assign(name, eval(parse(text = name), envir = baseenv()), envir = functions)
  }
  eval(expression, envir = list2env(as.list(df), parent = functions))
}

clean_transform_metadata <- function(df, before, origins) {
  old <- attr(before, "nlss_import_contract", exact = TRUE)
  labels <- resolve_label_metadata(before)
  clean_labels <- list(variables = list(), values = list())
  for (name in names(df)) {
    source <- origins[[name]]
    if (!is.na(source)) {
      if (!is.null(labels$variables[[source]])) clean_labels$variables[[name]] <- labels$variables[[source]]
      if (!is.null(labels$values[[source]])) clean_labels$values[[name]] <- labels$values[[source]]
    } else {
      for (key in c("label", "labels", "variable.label", "var.label", "value.labels", "na_values", "na_range",
                    "format.spss", "format.stata", "format.sas")) attr(df[[name]], key) <- NULL
    }
  }
  # Runtime source paths, input hashes and their attachment order are not data
  # semantics. Arrow serializes frame attributes, so keep a canonical data-only
  # set to make replay bytes independent of the ordinary/replay load route.
  attributes(df) <- attributes(df)[c("names", "row.names", "class")]
  attr(df, "nlss_labels") <- normalize_label_metadata(clean_labels)
  dictionary <- import_capture_dictionary(df)
  dictionary$source_rows <- old$source_rows
  for (name in names(df)) {
    source <- origins[[name]]
    if (!is.na(source) && !is.null(old$columns[[source]])) dictionary$columns[[name]] <- old$columns[[source]]
  }
  # Source missing definitions survive only on unchanged values (including a
  # pure rename). Derived variables point back to the immutable before version.
  dictionary$transformation <- list(input_version_id = attr(before, "nlss_dataset_ref")$version_id,
    observation_basis = "input_version_rows", preserved_column_origins = as.list(origins[!is.na(origins)]))
  attr(df, "nlss_import_contract") <- dictionary
  attr(df, "nlss_import_contract") <- attr(import_prepare_storage(df), "nlss_import_contract")
  df
}

transform_value_status <- function(vec) {
  list(class = class(vec), type = typeof(vec), n = length(vec),
    missing_rows = which(is.na(vec)),
    nan_rows = if (is.numeric(vec)) which(is.nan(vec)) else integer(),
    infinite_rows = if (is.numeric(vec)) which(is.infinite(vec)) else integer())
}

build_percentile_bins <- function(vec, bins) {
  if (all(is.na(vec))) stop("Percentile bins require at least one non-missing value.")
  if (any(!is.finite(vec) & !is.na(vec))) stop("Percentile bins require finite non-missing values.")
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

get_action_label <- function(action) {
  labels <- c(
    calc = "Calculated",
    transform = "Transformed",
    standardize = "Standardized",
    recode = "Recoded",
    percentile_bin = "Percentile-binned",
    bin = "Binned",
    rename = "Renamed",
    drop = "Dropped",
    none = "None"
  )
  if (!is.null(action) && action %in% names(labels)) return(unname(labels[[action]]))
  action
}

collapse_unique <- function(values) {
  if (length(values) == 0) return("")
  vals <- as.character(values)
  vals <- vals[!is.na(vals)]
  vals <- vals[nzchar(vals)]
  if (length(vals) == 0) return("")
  paste(unique(vals), collapse = ", ")
}

build_action_codes <- function(actions) {
  if (length(actions) == 0) return("")
  codes <- vapply(actions, function(action) {
    paste0(action, " = ", get_action_label(action))
  }, character(1))
  paste(codes, collapse = "; ")
}

build_transform_table_body <- function(log_df, table_spec = NULL) {
  default_columns <- list(
    list(key = "step", label = "Step"),
    list(key = "action", label = "Action"),
    list(key = "variable", label = "Variable"),
    list(key = "new_variable", label = "New Variable"),
    list(key = "details", label = "Details"),
    list(key = "note", label = "Note", drop_if_empty = TRUE)
  )
  columns <- normalize_table_columns(
    if (!is.null(table_spec$columns)) table_spec$columns else NULL,
    default_columns
  )

  rows <- list()
  if (nrow(log_df) == 0) {
    rows <- list(list(
      step = "1",
      action = "None",
      action_code = "none",
      variable = "",
      new_variable = "",
      details = "No transformations applied.",
      note = ""
    ))
  } else {
    for (i in seq_len(nrow(log_df))) {
      action_code <- as.character(log_df$action[i])
      rows[[length(rows) + 1]] <- list(
        step = as.character(i),
        action = get_action_label(action_code),
        action_code = action_code,
        variable = as.character(log_df$variable[i]),
        new_variable = as.character(log_df$new_variable[i]),
        details = as.character(log_df$details[i]),
        note = as.character(log_df$note[i])
      )
    }
  }

  table_rows <- list()
  for (row in rows) {
    row_vals <- character(0)
    for (col in columns) {
      key <- col$key
      val <- ""
      if (key %in% names(row)) {
        val <- row[[key]]
      } else if (key == "action") {
        val <- row$action
      } else if (key == "action_code") {
        val <- row$action_code
      }
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

build_transform_summary_tokens <- function(log_df) {
  steps_total <- nrow(log_df)
  actions_present <- if (steps_total > 0) unique(as.character(log_df$action)) else character(0)
  actions_present <- actions_present[nzchar(actions_present)]
  actions_labels <- if (length(actions_present) > 0) vapply(actions_present, get_action_label, character(1)) else character(0)
  rename_pairs <- character(0)
  if (steps_total > 0) {
    rename_pairs <- paste0(
      log_df$variable[log_df$action == "rename"],
      " -> ",
      log_df$new_variable[log_df$action == "rename"]
    )
  }

  list(
    steps_total = as.character(steps_total),
    actions_present = collapse_unique(actions_labels),
    action_codes = build_action_codes(actions_present),
    calc_vars = collapse_unique(log_df$new_variable[log_df$action == "calc"]),
    transform_vars = collapse_unique(log_df$new_variable[log_df$action == "transform"]),
    standardize_vars = collapse_unique(log_df$new_variable[log_df$action == "standardize"]),
    recode_vars = collapse_unique(log_df$new_variable[log_df$action == "recode"]),
    percentile_bin_vars = collapse_unique(log_df$new_variable[log_df$action == "percentile_bin"]),
    bin_vars = collapse_unique(log_df$new_variable[log_df$action == "bin"]),
    rename_pairs = collapse_unique(rename_pairs),
    drop_vars = collapse_unique(log_df$variable[log_df$action == "drop"])
  )
}

build_transform_note_tokens <- function(log_df, summary_tokens) {
  if (nrow(log_df) == 0) {
    return(list(
      note_default = "No transformations applied.",
      action_codes = "",
      note_details = ""
    ))
  }
  note_parts <- character(0)
  if (!is.null(summary_tokens$actions_present) && nzchar(summary_tokens$actions_present)) {
    note_parts <- c(note_parts, paste0("Actions: ", summary_tokens$actions_present, "."))
  }
  has_notes <- any(nzchar(as.character(log_df$note)))
  note_details <- if (has_notes) "Notes document coercion, bin adjustments and missing/nonfinite results." else ""
  if (nzchar(note_details)) note_parts <- c(note_parts, note_details)
  list(
    note_default = paste(note_parts, collapse = " "),
    action_codes = summary_tokens$action_codes,
    note_details = note_details
  )
}

build_transform_narrative_rows <- function(log_df) {
  rows <- list()
  if (nrow(log_df) == 0) {
    rows[[1]] <- list(full_sentence = "No transformations applied.")
    return(rows)
  }
  for (i in seq_len(nrow(log_df))) {
    action_code <- as.character(log_df$action[i])
    action_label <- get_action_label(action_code)
    variable <- as.character(log_df$variable[i])
    new_variable <- as.character(log_df$new_variable[i])
    details <- as.character(log_df$details[i])
    note <- as.character(log_df$note[i])

    target_part <- ""
    if (nzchar(new_variable) && new_variable != variable) {
      target_part <- paste0(" -> ", new_variable)
    }
    detail_part <- ""
    if (nzchar(details) && !(action_code %in% c("rename", "drop") && details %in% c("rename", "dropped"))) {
      detail_part <- paste0(" (", details, ")")
    }
    note_part <- if (nzchar(note)) paste0(" Note: ", note, ".") else ""
    base <- paste0(action_label, " ", variable, target_part, detail_part, ".")
    full_sentence <- paste0("Step ", i, ": ", base, note_part)
    rows[[length(rows) + 1]] <- list(
      step = as.character(i),
      action = action_code,
      action_label = action_label,
      variable = variable,
      new_variable = new_variable,
      details = details,
      note = note,
      full_sentence = trimws(full_sentence)
    )
  }
  rows
}

main <- function() {
  opts <- nlss_run_options(commandArgs(trailingOnly = TRUE), "data_transform")
  if (!is.null(opts[["help"]])) { print_usage(); return(invisible(NULL)) }
  if (parse_bool(opts[["interactive"]], FALSE)) {
    opts <- modifyList(opts, interactive_options())
    opts[["interactive"]] <- TRUE
  }
  before <- nlss_load_input(opts)
  out_dir <- get_workspace_out_dir(before)
  nlss_begin_run("data_transform", before, opts, out_dir)
  rules <- list(calc = parse_calc_rules(opts[["calc"]]), transform = parse_transform_rules(opts[["transform"]]),
    standardize = split_commas(opts[["standardize"]]), recode = parse_recode_rules(opts[["recode"]]),
    percentile_bin = parse_percentile_rules(opts[["percentile-bins"]]), bin = parse_bins_rules(opts[["bins"]]),
    rename = parse_rename_map(opts[["rename"]]), drop = split_commas(opts[["drop"]]))
  settings <- list()
  for (key in c("standardize-suffix", "percentile-suffix", "bins-suffix", "recode-suffix")) {
    settings[[key]] <- if (is.null(opts[[key]])) get_config_value(paste0("modules.data_transform.", gsub("-", "_", key))) else opts[[key]]
  }
  for (key in c("coerce", "overwrite-vars", "confirm-overwrite", "confirm-drop")) settings[[key]] <- dt_bool(opts, key)
  into <- lapply(c("transform", "standardize", "percentile", "bins", "recode"), function(key) parse_into_map(opts[[paste0(key, "-into")]]))
  names(into) <- c("transform", "standardize", "percentile", "bins", "recode")
  settings$into <- into
  replay_policy <- classify_calculations(rules$calc, before)
  if (!is.null(nlss_run_context$replay) && !isTRUE(replay_policy$eligible)) {
    stop("Current expression classification does not permit automatic replay; no calculation was executed.")
  }
  design <- list(operation_order = names(rules), rules = rules, source_n = nrow(before),
    source_classes = lapply(before, class), source_rows = seq_len(nrow(before)), replay = replay_policy)
  nlss_resolve_request(settings, design)
  if (!isTRUE(replay_policy$eligible)) warning(replay_policy$reason, call. = FALSE)

  df <- before
  origins <- setNames(names(before), names(before))
  log_rows <- step_details <- list()
  record <- function(action, variable, target, details, note = "", source = NULL, result = NULL, extra = list()) {
    log_rows[[length(log_rows) + 1L]] <<- data.frame(action = action, variable = variable,
      new_variable = target, details = details, note = note, stringsAsFactors = FALSE)
    source_status <- if (is.null(source)) NULL else transform_value_status(source)
    result_status <- if (is.null(result)) NULL else transform_value_status(result)
    step_details[[length(step_details) + 1L]] <<- c(list(step = length(log_rows), action = action,
      variable = variable, new_variable = target, details = details, note = note,
      before = source_status, after = result_status,
      comparison_basis = if (is.null(source_status)) "No previous target; resulting missingness is recorded in after."
        else "Input source or overwritten target, at the same row positions.",
      newly_missing_rows = if (is.null(source_status) || is.null(result_status)) NULL
        else setdiff(result_status$missing_rows, source_status$missing_rows),
      newly_infinite_rows = if (is.null(source_status) || is.null(result_status)) NULL
        else setdiff(result_status$infinite_rows, source_status$infinite_rows)), extra)
  }
  target_name <- function(var, key, default) if (is.null(into[[key]][[var]])) default else into[[key]][[var]]
  assign_numeric <- function(action, var, target, method, details) {
    if (!var %in% names(df)) stop("Unknown variable for ", action, ": ", var)
    ensure_target_name(df, target, opts, get_action_label(action))
    source <- df[[var]]
    coerced <- coerce_numeric(source, var, opts)
    value <- method(coerced$vec)
    note <- coerced$note
    new_missing <- sum(is.na(value) & !is.na(source))
    new_infinite <- if (is.numeric(value)) sum(is.infinite(value) & !is.infinite(coerced$vec)) else 0L
    if (new_missing) note <- trimws(paste(note, paste0(new_missing, " new missing value(s)")))
    if (new_infinite) note <- trimws(paste(note, paste0(new_infinite, " new infinite value(s)")))
    if (new_missing || new_infinite) warning(action, " of ", var, ": ", note, call. = FALSE)
    df[[target]] <<- value
    origins[target] <<- NA_character_
    record(action, var, target, details, note, source, value)
  }

  for (i in seq_along(rules$calc)) {
    rule <- rules$calc[[i]]
    ensure_target_name(df, rule$var, opts, "Calculation")
    source <- df[[rule$var]]
    result <- tryCatch(evaluate_calculation(rule, df, replay_policy$calc_classification[[i]]$eligible),
      error = function(e) stop("Failed to evaluate calc expression for ", rule$var, ": ", conditionMessage(e)))
    if (is.matrix(result) && ncol(result) == 1L) result <- as.vector(result)
    if (is.null(result) || !(is.atomic(result) || is.list(result)) || !is.null(dim(result))) {
      stop("Calc result must be a one-dimensional atomic or list column for ", rule$var)
    }
    if (length(result) == 1L) result <- rep(result, nrow(df))
    if (length(result) != nrow(df)) stop("Calc result length does not match rows for ", rule$var)
    df[[rule$var]] <- result
    origins[rule$var] <- NA_character_
    record("calc", rule$var, rule$var, rule$expr, source = source, result = result,
      extra = list(input_variables = intersect(all.vars(parse(text = rule$expr)), names(df)),
        replay_classification = replay_policy$calc_classification[[i]]$classification))
  }
  for (rule in rules$transform) {
    fn <- normalize_transform(rule$fn)
    default <- if (fn == "scale") paste0(rule$var, settings[["standardize-suffix"]]) else paste0(fn, "_", rule$var)
    assign_numeric("transform", rule$var, target_name(rule$var, "transform", default), get_transform_function(fn), fn)
  }
  for (var in rules$standardize[nzchar(rules$standardize)]) {
    assign_numeric("standardize", var, target_name(var, "standardize", paste0(var, settings[["standardize-suffix"]])),
      get_transform_function("scale"), paste0("z-score (suffix ", settings[["standardize-suffix"]], ")"))
  }
  for (rule in rules$recode) {
    var <- rule$var
    if (!var %in% names(df)) stop("Unknown variable for recode: ", var)
    target <- target_name(var, "recode", paste0(var, settings[["recode-suffix"]]))
    ensure_target_name(df, target, opts, "Recode")
    source <- df[[var]]
    value <- apply_recode(source, rule$mapping)
    df[[target]] <- value
    origins[target] <- NA_character_
    record("recode", var, target, paste0("pairs=", length(rule$mapping)), source = source, result = value,
      extra = list(mapping = rule$mapping, matching = "simultaneous_original_values"))
  }
  for (kind in c("percentile_bin", "bin")) {
    for (rule in rules[[kind]]) {
      var <- rule$var
      if (!var %in% names(df)) stop("Unknown variable for ", kind, ": ", var)
      key <- if (kind == "percentile_bin") "percentile" else "bins"
      target <- target_name(var, key, paste0(var, settings[[paste0(key, "-suffix")]]))
      ensure_target_name(df, target, opts, get_action_label(kind))
      source <- df[[var]]
      coerced <- coerce_numeric(source, var, opts)
      bin_info <- if (kind == "percentile_bin") build_percentile_bins(coerced$vec, rule$bins) else build_custom_bins(rule$breaks)
      value <- cut(coerced$vec, breaks = bin_info$breaks, include.lowest = TRUE, right = TRUE, labels = FALSE)
      note <- coerced$note
      if (isTRUE(bin_info$reduced)) note <- trimws(paste(note, paste0("reduced to ", length(bin_info$breaks) - 1L, " bins")))
      if (isTRUE(bin_info$sorted)) note <- trimws(paste(note, "sorted breaks"))
      out_of_range <- which(!is.na(coerced$vec) & is.na(value))
      if (length(out_of_range)) note <- trimws(paste(note, length(out_of_range), "value(s) outside custom breaks; set missing"))
      if (length(out_of_range) || any(is.na(coerced$vec) & !is.na(source))) warning(kind, " of ", var, ": ", note, call. = FALSE)
      df[[target]] <- value
      origins[target] <- NA_character_
      detail <- if (kind == "percentile_bin") paste0("bins=", rule$bins) else paste0("breaks=", paste(bin_info$breaks, collapse = ","))
      record(kind, var, target, detail, note, source, value, list(breaks = bin_info$breaks,
        effective_bins = length(bin_info$breaks) - 1L, quantile_type = if (kind == "percentile_bin") 7L else NULL,
        include_lowest = TRUE, right_closed = TRUE, out_of_range_rows = out_of_range))
    }
  }
  for (old in names(rules$rename)) {
    new <- rules$rename[[old]]
    if (!old %in% names(df)) stop("Unknown variable for rename: ", old)
    if (old == new) next
    ensure_target_name(df, new, opts, "Rename")
    source <- df[[old]]
    if (new %in% names(df)) { df[[new]] <- NULL; origins <- origins[names(origins) != new] }
    names(df)[names(df) == old] <- new
    names(origins)[names(origins) == old] <- new
    record("rename", old, new, "rename", source = source, result = source)
  }
  drop_vars <- unique(rules$drop[nzchar(rules$drop)])
  if (length(drop_vars)) {
    unknown <- setdiff(drop_vars, names(df))
    if (length(unknown)) stop("Unknown variables to drop: ", paste(unknown, collapse = ", "))
    confirm_action(opts, paste0("Drop variables: ", paste(drop_vars, collapse = ", "), "."), "confirm-drop")
    for (var in drop_vars) record("drop", var, "", "dropped", source = df[[var]])
    df[drop_vars] <- NULL
    origins <- origins[!names(origins) %in% drop_vars]
  }
  if (!ncol(df)) stop("A dataset must retain at least one variable; no data were published.")
  if (length(log_rows)) df <- clean_transform_metadata(df, before, origins)
  log_df <- if (length(log_rows)) do.call(rbind, log_rows) else data.frame(action = character(), variable = character(),
    new_variable = character(), details = character(), note = character(), stringsAsFactors = FALSE)
  changes <- list(added = setdiff(names(df), names(before)), removed = setdiff(names(before), names(df)),
    modified = names(df)[vapply(names(df), function(name) name %in% names(before) && !identical(before[[name]], df[[name]]), logical(1))],
    metadata_reset = names(origins)[is.na(origins)],
    preserved_column_origins = as.list(origins[!is.na(origins)]),
    renamed = step_details[vapply(step_details, function(x) identical(x$action, "rename"), logical(1))],
    unchanged_values = identical(names(before), names(df)) && all(vapply(names(df), function(name) identical(before[[name]], df[[name]]), logical(1))))
  design$steps <- step_details
  nlss_resolve_request(settings, design)

  template <- resolve_template_override(opts[["template"]], module = "data_transform")
  if (is.null(template)) template <- resolve_template_path("data_transform.default", "data-transform/default-template.md")
  template <- nlss_freeze_template(template, "data_transform.main")
  data_change <- nlss_prepare_data_change(before, df)
  # JSON temporal values use the same lossless numeric units as Parquet, with
  # their explicit logical types beside them (jsonlite has no difftime encoder).
  json_df <- import_prepare_storage(df)
  results <- list(transformed_df = json_df,
    transformed_df_storage = attr(json_df, "nlss_import_contract")$storage,
    transform_log_df = log_df, step_details = step_details,
    column_changes = changes, labels = resolve_label_metadata(df), data_change = data_change,
    output_path = data_change$output_path, backup_path = data_change$backup_path)
  if (!is.null(nlss_run_context$managed)) {
    # The immutable Parquet/dictionary are already the complete output. Avoid a
    # second row-by-row JSON dataset in every managed transformation run.
    results$transformed_df <- results$transformed_df_storage <- NULL
    results$data_representation <- "data_change.output: preserved Parquet and dictionary"
  }

  report_log <- log_df
  for (name in names(report_log)) report_log[[name]] <- nlss_mask_prose_paths(report_log[[name]], nlss_run_context$root)
  calc_rows <- which(log_df$action == "calc")
  report_log$details[calc_rows] <- nlss_mask_expression_paths(log_df$details[calc_rows], nlss_run_context$root)
  summary_tokens <- build_transform_summary_tokens(report_log)
  note_tokens <- build_transform_note_tokens(report_log, summary_tokens)
  note_tokens$note_default <- paste(note_tokens$note_default,
    "Steps run in calculation, transformation, standardization, recode, percentile-bin, custom-bin, rename, then drop order.",
    "The calculated result is preserved as an immutable version; actual publication status is recorded separately. Replay never changes current working data or its preview.",
    if (isTRUE(changes$unchanged_values)) "Column values and names are unchanged." else "",
    if (!isTRUE(replay_policy$eligible)) replay_policy$reason else "")
  if (nrow(log_df)) {
    narrative_rows <- build_transform_narrative_rows(report_log)
    nlss_text <- paste(sprintf("Data transformations were applied in %d step%s.", nrow(log_df), if (nrow(log_df) == 1L) "" else "s"),
      paste(vapply(narrative_rows, function(row) row$full_sentence, character(1)), collapse = " "))
  } else {
    narrative_rows <- build_transform_narrative_rows(report_log)
    nlss_text <- "No transformations applied. Data exported unchanged."
  }
  table <- build_transform_table_body(report_log, get_template_meta(template)$table)
  template_context <- list(tokens = c(list(table_body = table$body, narrative_default = nlss_text), summary_tokens, note_tokens),
    narrative_rows = narrative_rows)
  flags <- c(opts[intersect(names(opts), c("calc", "transform", "standardize", "percentile-bins", "bins", "recode", "rename", "drop"))],
    settings[c("coerce", "overwrite-vars")])
  for (name in names(flags)) if (is.character(flags[[name]])) flags[[name]] <- nlss_mask_prose_paths(flags[[name]], nlss_run_context$root)
  if (!is.null(opts[["calc"]])) flags$calc <- nlss_mask_expression_paths(opts[["calc"]], nlss_run_context$root)
  nlss_set_result(results)
  nlss_stage_report(file.path(out_dir, "report_canonical.md"), "Data transformation",
    table$body, nlss_text, analysis_flags = flags, template_path = template, template_context = template_context)
  if (parse_bool(opts[["log"]], get_config_value("defaults.log"))) {
    ctx <- get_run_context()
    legacy_options <- list(calc = opts[["calc"]], transform = opts[["transform"]], standardize = opts[["standardize"]],
      percentile_bins = opts[["percentile-bins"]], bins = opts[["bins"]], recode = opts[["recode"]],
      rename = opts[["rename"]], drop = opts[["drop"]], overwrite_vars = settings[["overwrite-vars"]],
      resolved_settings = settings)
    for (name in names(legacy_options)) if (is.character(legacy_options[[name]])) {
      legacy_options[[name]] <- nlss_mask_prose_paths(legacy_options[[name]], nlss_run_context$root)
    }
    legacy_options$calc <- nlss_mask_expression_paths(opts[["calc"]], nlss_run_context$root)
    legacy_results <- results
    legacy_results$transform_log_df <- report_log
    legacy_results$step_details <- lapply(step_details, function(step) {
      step$details <- if (identical(step$action, "calc")) nlss_mask_expression_paths(step$details, nlss_run_context$root)
        else nlss_mask_prose_paths(step$details, nlss_run_context$root)
      step
    })
    redact_legacy_values <- function(value) {
      if (is.character(value)) return(nlss_mask_prose_paths(value, nlss_run_context$root))
      if (is.list(value)) return(lapply(value, redact_legacy_values))
      value
    }
    for (name in names(legacy_results$transformed_df)) {
      legacy_results$transformed_df[[name]] <- redact_legacy_values(legacy_results$transformed_df[[name]])
    }
    legacy_results$display_note <- "Workspace-external textual paths are redacted in this legacy projection; immutable data and private run results retain exact values."
    nlss_stage_log(out_dir, "data_transform", ctx$prompt, ctx$commands, legacy_results,
      legacy_options, get_user_prompt(opts))
  }
}

nlss_run_main("data_transform", main)
