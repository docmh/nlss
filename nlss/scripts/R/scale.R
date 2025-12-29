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
source_lib("data_utils.R")
source_lib("formatting.R")


# Static analysis aliases for source_lib-defined functions.
render_output_path <- get("render_output_path", mode = "function")
add_group_label_column <- get("add_group_label_column", mode = "function")
add_variable_label_column <- get("add_variable_label_column", mode = "function")
resolve_label_metadata <- get("resolve_label_metadata", mode = "function")
source_lib <- get("source_lib", mode = "function")

print_usage <- function() {
  cat("Scale analysis (base R)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript scale.R --csv data.csv --vars item1,item2 [--group group_var]\n")
  cat("  Rscript scale.R --sav data.sav --vars item1,item2 [--group group_var]\n")
  cat("  Rscript scale.R --rds data.rds --vars item1,item2\n")
  cat("  Rscript scale.R --rdata data.RData --df data_frame_name --vars item1,item2\n")
  cat("  Rscript scale.R --parquet data.parquet --vars item1,item2\n")
  cat("  Rscript scale.R --interactive\n")
  cat("\n")
  cat("Options:\n")
  cat("  --csv PATH             CSV input file\n")
  cat("  --sav PATH             SPSS .sav input file\n")
  cat("  --sep VALUE            CSV separator (default: ,)\n")
  cat("  --header TRUE/FALSE    CSV header (default: TRUE)\n")
  cat("  --rds PATH             RDS input file (data frame)\n")
  cat("  --rdata PATH           RData input file\n")
  cat("  --parquet PATH         Parquet input file\n")
  cat("  --df NAME              Data frame object name in RData\n")
  cat("  --vars LIST            Comma-separated item variables (default: all numeric)\n")
  cat("  --group NAME           Grouping variable name (optional)\n")
  cat("  --reverse LIST         Comma-separated items to reverse score\n")
  cat("  --reverse-min VALUE    Minimum scale value for reverse scoring\n")
  cat("  --reverse-max VALUE    Maximum scale value for reverse scoring\n")
  cat("  --missing TYPE         pairwise/complete (default: pairwise)\n")
  cat("  --score TYPE           sum/mean (default: sum)\n")
  cat("  --omega TRUE/FALSE     Compute omega total (default: TRUE)\n")
  cat("  --coerce TRUE/FALSE    Coerce non-numeric vars to numeric (default: FALSE)\n")
  cat("  --digits N             Rounding digits (default: 2)\n")
  cat("  --template REF         Template path or template key (optional)\n")
  cat("  --user-prompt TEXT     Original AI user prompt for logging (optional)\n")
  cat("  --log TRUE/FALSE       Write analysis_log.jsonl (default: TRUE)\n")
  cat("  --interactive          Prompt for inputs\n")
  cat("  --help                 Show this help\n")
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

  opts$vars <- resolve_prompt("Items (comma-separated, blank for numeric)", "")
  opts$group <- resolve_prompt("Grouping variable (blank for none)", "")
  opts$reverse <- resolve_prompt("Reverse-scored items (comma-separated, blank for none)", "")

  reverse_min_default <- resolve_config_value("modules.scale.reverse_min", NULL)
  reverse_max_default <- resolve_config_value("modules.scale.reverse_max", NULL)
  if (!is.null(opts$reverse) && nzchar(opts$reverse)) {
    reverse_min_text <- if (!is.null(reverse_min_default)) as.character(reverse_min_default) else ""
    reverse_max_text <- if (!is.null(reverse_max_default)) as.character(reverse_max_default) else ""
    opts$`reverse-min` <- resolve_prompt("Reverse min (blank for observed)", reverse_min_text)
    opts$`reverse-max` <- resolve_prompt("Reverse max (blank for observed)", reverse_max_text)
  }

  missing_default <- resolve_config_value("modules.scale.missing", "pairwise")
  score_default <- resolve_config_value("modules.scale.score", "sum")
  omega_default <- resolve_config_value("modules.scale.omega", TRUE)
  coerce_default <- resolve_config_value("modules.scale.coerce", FALSE)
  digits_default <- resolve_config_value("defaults.digits", 2)

  opts$missing <- resolve_prompt("Missing handling (pairwise/complete)", missing_default)
  opts$score <- resolve_prompt("Scale score (sum/mean)", score_default)
  opts$omega <- resolve_prompt("Compute omega TRUE/FALSE", ifelse(isTRUE(omega_default), "TRUE", "FALSE"))
  opts$coerce <- resolve_prompt("Coerce non-numeric TRUE/FALSE", ifelse(isTRUE(coerce_default), "TRUE", "FALSE"))
  opts$digits <- resolve_prompt("Rounding digits", as.character(digits_default))
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


resolve_get_workspace_out_dir <- function(df) {
  if (exists("get_workspace_out_dir", mode = "function")) {
    return(get("get_workspace_out_dir", mode = "function")(df))
  }
  stop("Missing get_workspace_out_dir. Ensure lib/io.R is sourced.")
}

resolve_select_variables <- function(df, vars, group_var = NULL, default = "numeric") {
  if (exists("select_variables", mode = "function")) {
    return(get("select_variables", mode = "function")(df, vars, group_var = group_var, default = default))
  }
  available <- names(df)
  if (is.null(vars) || vars == "") {
    selected <- if (default == "numeric") available[sapply(df, is.numeric)] else available
    if (!is.null(group_var)) selected <- setdiff(selected, group_var)
    return(selected)
  }
  requested <- trimws(strsplit(vars, ",", fixed = TRUE)[[1]])
  missing <- setdiff(requested, available)
  if (length(missing) > 0) {
    stop(paste("Unknown variables:", paste(missing, collapse = ", ")))
  }
  if (!is.null(group_var)) requested <- setdiff(requested, group_var)
  requested
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

resolve_round_numeric <- function(df, digits) {
  if (exists("round_numeric", mode = "function")) {
    return(get("round_numeric", mode = "function")(df, digits))
  }
  out <- df
  numeric_cols <- sapply(out, is.numeric)
  out[numeric_cols] <- lapply(out[numeric_cols], function(x) round(x, digits))
  out
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

resolve_get_user_prompt <- function(opts) {
  if (exists("get_user_prompt", mode = "function")) {
    return(get("get_user_prompt", mode = "function")(opts))
  }
  NULL
}

normalize_missing <- function(value, default = "pairwise") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (val %in% c("pairwise", "pair")) return("pairwise")
  if (val %in% c("complete", "listwise")) return("complete")
  default
}

normalize_score <- function(value, default = "sum") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (val %in% c("sum", "total")) return("sum")
  if (val %in% c("mean", "average", "avg")) return("mean")
  default
}

coerce_numeric <- function(vec) {
  if (is.numeric(vec)) return(vec)
  if (is.logical(vec)) return(as.numeric(vec))
  if (inherits(vec, "Date") || inherits(vec, "POSIXct") || inherits(vec, "POSIXlt")) {
    return(as.numeric(vec))
  }
  if (is.factor(vec)) return(as.numeric(as.character(vec)))
  if (is.character(vec)) return(as.numeric(vec))
  as.numeric(vec)
}

coerce_dataframe <- function(df, vars, coerce) {
  if (!coerce || length(vars) == 0) return(df)
  for (var in vars) {
    if (!is.numeric(df[[var]])) {
      original <- df[[var]]
      converted <- suppressWarnings(coerce_numeric(original))
      introduced_nas <- sum(is.na(converted) & !is.na(original))
      if (introduced_nas > 0) {
        cat(sprintf("Warning: coercion introduced %s NA values for %s.\n", introduced_nas, var))
      }
      df[[var]] <- converted
    }
  }
  df
}

parse_optional_numeric <- function(value) {
  if (is.null(value)) return(NULL)
  value <- as.character(value)
  if (!nzchar(value)) return(NULL)
  num <- suppressWarnings(as.numeric(value))
  if (is.na(num)) return(NULL)
  num
}

reverse_items <- function(df, items, reverse_items, reverse_min = NULL, reverse_max = NULL) {
  if (length(reverse_items) == 0) {
    return(list(df = df, info = list(items = character(0), method = "none")))
  }
  reverse_items <- intersect(reverse_items, items)
  method <- "fixed"
  if (is.null(reverse_min) || is.null(reverse_max) || is.na(reverse_min) || is.na(reverse_max)) {
    method <- "observed"
  }
  per_item <- data.frame(item = character(0), min = numeric(0), max = numeric(0), stringsAsFactors = FALSE)
  for (item in reverse_items) {
    vec <- df[[item]]
    if (method == "observed") {
      min_val <- suppressWarnings(min(vec, na.rm = TRUE))
      max_val <- suppressWarnings(max(vec, na.rm = TRUE))
      if (!is.finite(min_val) || !is.finite(max_val)) {
        min_val <- NA_real_
        max_val <- NA_real_
      }
    } else {
      min_val <- reverse_min
      max_val <- reverse_max
    }
    if (!is.na(min_val) && !is.na(max_val) && is.finite(min_val) && is.finite(max_val)) {
      df[[item]] <- max_val + min_val - vec
    }
    per_item <- rbind(
      per_item,
      data.frame(item = item, min = min_val, max = max_val, stringsAsFactors = FALSE)
    )
  }
  list(
    df = df,
    info = list(
      items = reverse_items,
      method = method,
      reverse_min = reverse_min,
      reverse_max = reverse_max,
      per_item = per_item
    )
  )
}

compute_alpha <- function(cov_mat) {
  if (is.null(cov_mat) || !is.matrix(cov_mat)) return(NA_real_)
  k <- ncol(cov_mat)
  if (k < 2) return(NA_real_)
  if (any(is.na(cov_mat))) return(NA_real_)
  total_var <- sum(cov_mat)
  sum_item_var <- sum(diag(cov_mat))
  if (is.na(total_var) || total_var <= 0) return(NA_real_)
  if (is.na(sum_item_var) || sum_item_var <= 0) return(NA_real_)
  (k / (k - 1)) * (1 - sum_item_var / total_var)
}

compute_alpha_std <- function(k, r_bar) {
  if (is.na(r_bar) || k < 2) return(NA_real_)
  denom <- 1 + (k - 1) * r_bar
  if (is.na(denom) || denom == 0) return(NA_real_)
  (k * r_bar) / denom
}

compute_item_total_r <- function(cov_mat, item_name, corrected = FALSE) {
  if (is.null(cov_mat) || !is.matrix(cov_mat)) return(NA_real_)
  idx <- match(item_name, colnames(cov_mat))
  if (is.na(idx)) return(NA_real_)
  if (any(is.na(cov_mat[idx, ]))) return(NA_real_)
  var_i <- cov_mat[idx, idx]
  if (is.na(var_i) || var_i <= 0) return(NA_real_)
  cov_i_total <- sum(cov_mat[idx, ])
  total_var <- sum(cov_mat)
  if (is.na(total_var) || total_var <= 0) return(NA_real_)
  if (corrected) {
    cov_i_rest <- cov_i_total - var_i
    var_rest <- total_var - 2 * cov_i_total + var_i
    if (is.na(var_rest) || var_rest <= 0) return(NA_real_)
    return(cov_i_rest / sqrt(var_i * var_rest))
  }
  cov_i_total / sqrt(var_i * total_var)
}

compute_alpha_if_deleted <- function(cov_mat, item_name) {
  if (is.null(cov_mat) || !is.matrix(cov_mat)) return(NA_real_)
  idx <- match(item_name, colnames(cov_mat))
  if (is.na(idx)) return(NA_real_)
  if (ncol(cov_mat) <= 2) return(NA_real_)
  sub_mat <- cov_mat[-idx, -idx, drop = FALSE]
  compute_alpha(sub_mat)
}

compute_r_bar <- function(cor_mat) {
  if (is.null(cor_mat) || !is.matrix(cor_mat)) {
    return(list(r_bar = NA_real_, r_min = NA_real_, r_max = NA_real_))
  }
  vals <- cor_mat[lower.tri(cor_mat)]
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) {
    return(list(r_bar = NA_real_, r_min = NA_real_, r_max = NA_real_))
  }
  list(r_bar = mean(vals), r_min = min(vals), r_max = max(vals))
}

compute_omega_total <- function(cor_mat, n_obs) {
  if (is.null(cor_mat) || !is.matrix(cor_mat)) {
    return(list(value = NA_real_, status = "correlation_missing"))
  }
  k <- ncol(cor_mat)
  if (k < 3) return(list(value = NA_real_, status = "insufficient_items"))
  if (any(is.na(cor_mat))) return(list(value = NA_real_, status = "correlation_missing"))
  if (is.null(n_obs) || is.na(n_obs) || n_obs < 3) {
    return(list(value = NA_real_, status = "insufficient_n"))
  }
  res <- tryCatch(
    factanal(covmat = list(cov = cor_mat, n.obs = n_obs), factors = 1, rotation = "none"),
    error = function(e) NULL
  )
  if (is.null(res)) return(list(value = NA_real_, status = "factanal_failed"))
  loadings <- as.numeric(res$loadings[, 1])
  uniq <- res$uniquenesses
  if (any(is.na(loadings)) || any(is.na(uniq))) {
    return(list(value = NA_real_, status = "loadings_missing"))
  }
  numerator <- (sum(loadings))^2
  denom <- numerator + sum(uniq)
  if (is.na(denom) || denom <= 0) return(list(value = NA_real_, status = "invalid_denominator"))
  list(value = numerator / denom, status = "ok")
}

build_item_stats <- function(items_df, cov_mat, group_label, total_n) {
  items <- names(items_df)
  rows <- list()
  for (item in items) {
    vec <- items_df[[item]]
    missing_n <- sum(is.na(vec))
    missing_pct <- ifelse(total_n > 0, missing_n / total_n * 100, NA_real_)
    valid <- vec[!is.na(vec)]
    n <- length(valid)
    mean_val <- if (n > 0) mean(valid) else NA_real_
    sd_val <- if (n > 1) sd(valid) else NA_real_
    min_val <- if (n > 0) min(valid) else NA_real_
    max_val <- if (n > 0) max(valid) else NA_real_
    item_total_r <- compute_item_total_r(cov_mat, item, corrected = FALSE)
    item_rest_r <- compute_item_total_r(cov_mat, item, corrected = TRUE)
    alpha_if_deleted <- compute_alpha_if_deleted(cov_mat, item)
    rows[[length(rows) + 1]] <- data.frame(
      item = item,
      group = group_label,
      n = n,
      missing_n = missing_n,
      missing_pct = missing_pct,
      mean = mean_val,
      sd = sd_val,
      min = min_val,
      max = max_val,
      item_total_r = item_total_r,
      item_rest_r = item_rest_r,
      alpha_if_deleted = alpha_if_deleted,
      stringsAsFactors = FALSE
    )
  }
  df <- do.call(rbind, rows)
  numeric_cols <- setdiff(names(df), c("item", "group"))
  for (col in numeric_cols) {
    df[[col]] <- as.numeric(df[[col]])
  }
  df
}

compute_reliability_summary <- function(items_df, cov_mat, cor_mat, group_label, score_method, omega_flag) {
  items <- names(items_df)
  k <- length(items)
  total_n <- nrow(items_df)
  complete_idx <- complete.cases(items_df)
  n_complete <- sum(complete_idx)
  missing_n <- total_n - n_complete
  missing_pct <- ifelse(total_n > 0, missing_n / total_n * 100, NA_real_)

  scores <- numeric(0)
  if (n_complete > 0) {
    sub_df <- items_df[complete_idx, , drop = FALSE]
    if (score_method == "mean") {
      scores <- rowMeans(sub_df)
    } else {
      scores <- rowSums(sub_df)
    }
  }

  score_mean <- if (length(scores) > 0) mean(scores) else NA_real_
  score_sd <- if (length(scores) > 1) sd(scores) else NA_real_
  score_min <- if (length(scores) > 0) min(scores) else NA_real_
  score_max <- if (length(scores) > 0) max(scores) else NA_real_

  alpha <- compute_alpha(cov_mat)
  r_stats <- compute_r_bar(cor_mat)
  alpha_std <- compute_alpha_std(k, r_stats$r_bar)

  omega_info <- list(value = NA_real_, status = "disabled")
  if (omega_flag) {
    omega_info <- compute_omega_total(cor_mat, n_complete)
  }

  data.frame(
    group = group_label,
    n_items = k,
    n_total = total_n,
    n_complete = n_complete,
    missing_n = missing_n,
    missing_pct = missing_pct,
    alpha = alpha,
    alpha_std = alpha_std,
    omega_total = omega_info$value,
    omega_status = omega_info$status,
    r_bar = r_stats$r_bar,
    r_min = r_stats$r_min,
    r_max = r_stats$r_max,
    score_method = score_method,
    score_mean = score_mean,
    score_sd = score_sd,
    score_min = score_min,
    score_max = score_max,
    stringsAsFactors = FALSE
  )
}

format_num <- function(value, digits) {
  if (is.na(value)) return("")
  format(round(value, digits), nsmall = digits, trim = TRUE)
}

format_num_text <- function(value, digits) {
  if (is.na(value)) return("NA")
  format(round(value, digits), nsmall = digits, trim = TRUE)
}

format_apa_text <- function(reliability_df, digits) {
  display <- resolve_round_numeric(reliability_df, digits)
  display$group <- as.character(display$group)
  display$group[is.na(display$group)] <- "NA"
  display$group_display <- if ("group_label" %in% names(display)) display$group_label else display$group
  lines <- character(0)

  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    label <- if (row$group == "") "Scale" else paste("Group", row$group_display)
    missing_pct <- ifelse(is.na(row$missing_pct), "NA", format_num_text(row$missing_pct, 1))
    missing_part <- paste0(
      "Missing = ",
      ifelse(is.na(row$missing_n), "NA", as.character(row$missing_n)),
      " (",
      missing_pct,
      "%)"
    )

    if (is.na(row$alpha) || is.na(row$n_items) || row$n_items < 2) {
      line <- sprintf(
        "%s: reliability could not be computed (k = %s, n = %s). %s.",
        label,
        ifelse(is.na(row$n_items), "NA", as.character(row$n_items)),
        ifelse(is.na(row$n_complete), "NA", as.character(row$n_complete)),
        missing_part
      )
      lines <- c(lines, line)
      next
    }

    alpha_text <- format_num_text(row$alpha, digits)
    alpha_std_text <- format_num_text(row$alpha_std, digits)
    omega_text <- format_num_text(row$omega_total, digits)
    r_bar_text <- format_num_text(row$r_bar, digits)

    score_mean_text <- format_num_text(row$score_mean, digits)
    score_sd_text <- format_num_text(row$score_sd, digits)

    line <- paste0(
      label,
      ": k = ", as.character(row$n_items),
      ", n = ", as.character(row$n_complete),
      ", alpha = ", alpha_text,
      ", standardized alpha = ", alpha_std_text,
      ", omega = ", omega_text,
      ", average inter-item r = ", r_bar_text,
      ". Total score (", row$score_method, ") M = ", score_mean_text,
      ", SD = ", score_sd_text,
      ". ", missing_part, "."
    )
    lines <- c(lines, line)
  }

  paste(lines, collapse = "\n")
}

build_scale_table_body <- function(item_df, digits, table_spec = NULL) {
  display <- resolve_round_numeric(item_df, digits)
  display$group <- as.character(display$group)
  display$group[is.na(display$group)] <- "NA"
  display$item_display <- if ("item_label" %in% names(display)) display$item_label else display$item
  display$group_display <- if ("group_label" %in% names(display)) display$group_label else display$group

  default_columns <- list(
    list(key = "item", label = "Item"),
    list(key = "group", label = "Group", drop_if_empty = TRUE),
    list(key = "n", label = "n"),
    list(key = "mean", label = "M"),
    list(key = "sd", label = "SD"),
    list(key = "item_total_r", label = "r_it", drop_if_empty = TRUE),
    list(key = "item_rest_r", label = "r_drop", drop_if_empty = TRUE),
    list(key = "alpha_if_deleted", label = "alpha_if_deleted", drop_if_empty = TRUE),
    list(key = "missing_pct", label = "Missing %", drop_if_empty = TRUE)
  )

  columns <- resolve_normalize_table_columns(
    if (!is.null(table_spec$columns)) table_spec$columns else NULL,
    default_columns
  )

  rows <- list()
  for (i in seq_len(nrow(display))) {
    row <- display[i, , drop = FALSE]
    row_vals <- character(0)
    for (col in columns) {
      key <- col$key
      val <- ""
      if (key %in% c("item", "group")) {
        if (key == "item") {
          val <- resolve_as_cell_text(row$item_display[1])
        } else {
          val <- resolve_as_cell_text(row$group_display[1])
        }
      } else if (key %in% c("n", "missing_n")) {
        val <- ifelse(is.na(row[[key]][1]), "", as.character(row[[key]][1]))
      } else if (key %in% c("missing_pct")) {
        val <- format_num(row[[key]][1], digits)
      } else if (key %in% names(row)) {
        cell <- row[[key]][1]
        if (is.numeric(cell)) {
          val <- format_num(cell, digits)
        } else {
          val <- resolve_as_cell_text(cell)
        }
      }
      row_vals <- c(row_vals, val)
    }
    rows[[length(rows) + 1]] <- row_vals
  }

  filtered <- resolve_drop_empty_columns(columns, rows)
  columns <- filtered$columns
  rows <- filtered$rows
  headers <- vapply(columns, function(col) {
    if (!is.null(col$label) && nzchar(col$label)) col$label else col$key
  }, character(1))
  resolve_render_markdown_table(headers, rows)
}

build_scale_note_tokens <- function(reverse_info, missing_method, score_method, omega_flag, omega_statuses) {
  item_corr_note <- "r_it = item-total correlation; r_drop = corrected item-total correlation."
  alpha_note <- "alpha_if_deleted = Cronbach's alpha without the item."

  reverse_note <- ""
  if (!is.null(reverse_info$items) && length(reverse_info$items) > 0) {
    if (!is.null(reverse_info$method) && reverse_info$method == "fixed" &&
        !is.null(reverse_info$reverse_min) && !is.null(reverse_info$reverse_max)) {
      reverse_note <- paste0(
        "Reverse-scored items: ",
        paste(reverse_info$items, collapse = ", "),
        " (min = ", reverse_info$reverse_min, ", max = ", reverse_info$reverse_max, ")."
      )
    } else {
      reverse_note <- paste0(
        "Reverse-scored items: ",
        paste(reverse_info$items, collapse = ", "),
        " (using observed min/max)."
      )
    }
  }

  missing_note <- paste0("Reliability computed using ", missing_method, " observations.")
  score_note <- paste0("Scale score based on ", score_method, " of complete cases.")

  omega_note <- ""
  if (omega_flag) {
    if (length(omega_statuses) > 0 && any(omega_statuses == "ok", na.rm = TRUE)) {
      omega_note <- "Omega total estimated via a one-factor model."
    } else {
      omega_note <- "Omega total not available."
    }
  }

  note_parts <- c(item_corr_note, alpha_note, reverse_note, missing_note, score_note, omega_note)
  note_default <- paste(note_parts[nzchar(note_parts)], collapse = " ")

  list(
    item_corr_note = item_corr_note,
    alpha_note = alpha_note,
    reverse_note = reverse_note,
    missing_note = missing_note,
    score_note = score_note,
    omega_note = omega_note,
    note_default = note_default
  )
}

build_scale_narrative_rows <- function(reliability_df, digits) {
  display <- resolve_round_numeric(reliability_df, digits)
  display$group <- as.character(display$group)
  display$group[is.na(display$group)] <- "NA"
  display$group_display <- if ("group_label" %in% names(display)) display$group_label else display$group
  rows <- list()

  for (i in seq_len(nrow(display))) {
    row <- display[i, , drop = FALSE]
    group_label <- if (row$group == "") "Scale" else paste("Group", row$group_display)

    missing_pct <- ifelse(is.na(row$missing_pct), "NA", format_num_text(row$missing_pct, 1))
    missing_text <- paste0(
      "Missing = ",
      ifelse(is.na(row$missing_n), "NA", as.character(row$missing_n)),
      " (",
      missing_pct,
      "%)"
    )

    alpha_text <- format_num_text(row$alpha, digits)
    alpha_std_text <- format_num_text(row$alpha_std, digits)
    omega_text <- format_num_text(row$omega_total, digits)
    r_bar_text <- format_num_text(row$r_bar, digits)

    score_mean_text <- format_num_text(row$score_mean, digits)
    score_sd_text <- format_num_text(row$score_sd, digits)

    if (is.na(row$alpha) || is.na(row$n_items) || row$n_items < 2) {
      line <- sprintf(
        "%s: reliability could not be computed (k = %s, n = %s). %s.",
        group_label,
        ifelse(is.na(row$n_items), "NA", as.character(row$n_items)),
        ifelse(is.na(row$n_complete), "NA", as.character(row$n_complete)),
        missing_text
      )
    } else {
      line <- paste0(
        group_label,
        ": k = ", as.character(row$n_items),
        ", n = ", as.character(row$n_complete),
        ", alpha = ", alpha_text,
        ", standardized alpha = ", alpha_std_text,
        ", omega = ", omega_text,
        ", average inter-item r = ", r_bar_text,
        ". Total score (", row$score_method, ") M = ", score_mean_text,
        ", SD = ", score_sd_text,
        ". ", missing_text, "."
      )
    }

    rows[[length(rows) + 1]] <- list(
      group = resolve_as_cell_text(row$group_display),
      group_label = group_label,
      n_items = ifelse(is.na(row$n_items), "NA", as.character(row$n_items)),
      n_total = ifelse(is.na(row$n_total), "NA", as.character(row$n_total)),
      n_complete = ifelse(is.na(row$n_complete), "NA", as.character(row$n_complete)),
      missing_n = ifelse(is.na(row$missing_n), "NA", as.character(row$missing_n)),
      missing_pct = missing_pct,
      alpha = alpha_text,
      alpha_std = alpha_std_text,
      omega_total = omega_text,
      r_bar = r_bar_text,
      r_min = format_num_text(row$r_min, digits),
      r_max = format_num_text(row$r_max, digits),
      score_method = resolve_as_cell_text(row$score_method),
      score_mean = score_mean_text,
      score_sd = score_sd_text,
      score_min = format_num_text(row$score_min, digits),
      score_max = format_num_text(row$score_max, digits),
      missing_text = missing_text,
      full_sentence = line
    )
  }

  rows
}

format_apa_table <- function(item_df, digits, note_text) {
  table_body <- build_scale_table_body(item_df, digits, table_spec = NULL)
  header <- "Table 1\nScale item analysis\n\n"
  note_line <- if (nzchar(note_text)) paste0("Note. ", note_text) else "Note."
  paste0(header, table_body, "\n", note_line, "\n")
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  opts <- resolve_parse_args(args)

  if (!is.null(opts$help)) {
    print_usage()
    quit(status = 0)
  }

  if (!is.null(opts$interactive)) {
    opts <- modifyList(opts, interactive_options())
  }

  digits_default <- resolve_config_value("defaults.digits", 2)
  log_default <- resolve_config_value("defaults.log", TRUE)
  vars_default <- resolve_config_value("modules.scale.vars_default", "numeric")
  missing_default <- resolve_config_value("modules.scale.missing", "pairwise")
  score_default <- resolve_config_value("modules.scale.score", "sum")
  omega_default <- resolve_config_value("modules.scale.omega", TRUE)
  coerce_default <- resolve_config_value("modules.scale.coerce", FALSE)
  reverse_min_default <- resolve_config_value("modules.scale.reverse_min", NULL)
  reverse_max_default <- resolve_config_value("modules.scale.reverse_max", NULL)

  digits <- if (!is.null(opts$digits)) as.numeric(opts$digits) else digits_default
  missing_method <- normalize_missing(opts$missing, default = missing_default)
  score_method <- normalize_score(opts$score, default = score_default)
  omega_flag <- resolve_parse_bool(opts$omega, default = omega_default)
  coerce_flag <- resolve_parse_bool(opts$coerce, default = coerce_default)

  reverse_min <- if (!is.null(opts$`reverse-min`)) {
    parse_optional_numeric(opts$`reverse-min`)
  } else {
    parse_optional_numeric(reverse_min_default)
  }
  reverse_max <- if (!is.null(opts$`reverse-max`)) {
    parse_optional_numeric(opts$`reverse-max`)
  } else {
    parse_optional_numeric(reverse_max_default)
  }

  df <- resolve_load_dataframe(opts)
  out_dir <- resolve_get_workspace_out_dir(df)
  group_var <- if (!is.null(opts$group) && opts$group != "") opts$group else NULL

  if (!is.null(group_var) && !(group_var %in% names(df))) {
    stop("Grouping variable not found in data frame.")
  }

  vars <- resolve_select_variables(df, opts$vars, group_var, default = vars_default)
  if (length(vars) == 0) stop("No item variables available for scale analysis.")

  missing_vars <- setdiff(vars, names(df))
  if (length(missing_vars) > 0) {
    stop(paste("Unknown variables:", paste(missing_vars, collapse = ", ")))
  }

  df <- coerce_dataframe(df, vars, coerce_flag)
  for (var in vars) {
    if (!is.numeric(df[[var]])) {
      stop(paste("Item is not numeric:", var, "(use --coerce to convert)."))
    }
  }

  reverse_items_list <- resolve_parse_list(opts$reverse)
  if (length(reverse_items_list) > 0) {
    missing_reverse <- setdiff(reverse_items_list, vars)
    if (length(missing_reverse) > 0) {
      stop(paste("Unknown reverse items:", paste(missing_reverse, collapse = ", ")))
    }
  }

  reverse_result <- reverse_items(df, vars, reverse_items_list, reverse_min, reverse_max)
  df <- reverse_result$df
  reverse_info <- reverse_result$info

  item_list <- list()
  reliability_list <- list()

  if (!is.null(group_var)) {
    group_vec <- df[[group_var]]
    group_levels <- unique(group_vec)
    for (g in group_levels) {
      idx <- if (is.na(g)) is.na(group_vec) else group_vec == g
      sub_df <- df[idx, , drop = FALSE]
      group_label <- ifelse(is.na(g), "NA", as.character(g))
      items_df <- sub_df[, vars, drop = FALSE]

      use_method <- if (missing_method == "pairwise") "pairwise.complete.obs" else "complete.obs"
      cov_mat <- tryCatch(cov(items_df, use = use_method), error = function(e) NULL)
      cor_mat <- tryCatch(cor(items_df, use = use_method), error = function(e) NULL)
      if (!is.null(cov_mat) && length(vars) == 1) {
        cov_mat <- matrix(cov_mat, nrow = 1, ncol = 1, dimnames = list(vars, vars))
      }
      if (!is.null(cor_mat) && length(vars) == 1) {
        cor_mat <- matrix(cor_mat, nrow = 1, ncol = 1, dimnames = list(vars, vars))
      }

      item_list[[length(item_list) + 1]] <- build_item_stats(items_df, cov_mat, group_label, nrow(items_df))
      reliability_list[[length(reliability_list) + 1]] <- compute_reliability_summary(
        items_df,
        cov_mat,
        cor_mat,
        group_label,
        score_method,
        omega_flag
      )
    }
  } else {
    items_df <- df[, vars, drop = FALSE]
    use_method <- if (missing_method == "pairwise") "pairwise.complete.obs" else "complete.obs"
    cov_mat <- tryCatch(cov(items_df, use = use_method), error = function(e) NULL)
    cor_mat <- tryCatch(cor(items_df, use = use_method), error = function(e) NULL)
    if (!is.null(cov_mat) && length(vars) == 1) {
      cov_mat <- matrix(cov_mat, nrow = 1, ncol = 1, dimnames = list(vars, vars))
    }
    if (!is.null(cor_mat) && length(vars) == 1) {
      cor_mat <- matrix(cor_mat, nrow = 1, ncol = 1, dimnames = list(vars, vars))
    }

    item_list[[length(item_list) + 1]] <- build_item_stats(items_df, cov_mat, "", nrow(items_df))
    reliability_list[[length(reliability_list) + 1]] <- compute_reliability_summary(
      items_df,
      cov_mat,
      cor_mat,
      "",
      score_method,
      omega_flag
    )
  }

  item_df <- do.call(rbind, item_list)
  reliability_df <- do.call(rbind, reliability_list)
  label_meta <- resolve_label_metadata(df)
  item_df <- add_variable_label_column(item_df, label_meta, var_col = "item")
  item_df <- add_group_label_column(item_df, label_meta, group_var, group_col = "group")
  reliability_df <- add_group_label_column(reliability_df, label_meta, group_var, group_col = "group")

  template_override <- resolve_template_override(opts$template, module = "scale")
  template_path <- if (!is.null(template_override)) {
    template_override
  } else {
    resolve_get_template_path("scale.default", "scale/default-template.md")
  }
  template_meta <- resolve_get_template_meta(template_path)
  apa_report_path <- file.path(out_dir, "report_canonical.md")

  omega_statuses <- character(0)
  if (!is.null(reliability_df$omega_status)) {
    omega_statuses <- reliability_df$omega_status
  }

  note_tokens <- build_scale_note_tokens(reverse_info, missing_method, score_method, omega_flag, omega_statuses)
  apa_text <- format_apa_text(reliability_df, digits)
  apa_table <- format_apa_table(item_df, digits, note_tokens$note_default)
  table_body <- build_scale_table_body(item_df, digits, template_meta$table)
  narrative_rows <- build_scale_narrative_rows(reliability_df, digits)
  template_context <- list(
    tokens = c(
      list(
        table_body = table_body,
        narrative_default = apa_text
      ),
      note_tokens
    ),
    narrative_rows = narrative_rows
  )

  analysis_flags <- list(
    vars = vars,
    group = if (!is.null(group_var) && group_var != "") group_var else "None",
    reverse = if (length(reverse_items_list) > 0) reverse_items_list else "None",
    "reverse-min" = if (length(reverse_items_list) > 0 && !is.null(reverse_min)) reverse_min else NULL,
    "reverse-max" = if (length(reverse_items_list) > 0 && !is.null(reverse_max)) reverse_max else NULL,
    missing = missing_method,
    score = score_method,
    omega = omega_flag,
    coerce = coerce_flag,
    digits = digits
  )

  resolve_append_apa_report(
    apa_report_path,
    "Scale analysis",
    apa_table,
    apa_text,
    analysis_flags = analysis_flags,
    template_path = template_path,
    template_context = template_context
  )

  cat("Wrote:\n")
  cat("- ", render_output_path(apa_report_path, out_dir), "\n", sep = "")

  if (resolve_parse_bool(opts$log, default = log_default)) {
    ctx <- resolve_get_run_context()
    resolve_append_analysis_log(
      out_dir,
      module = "scale",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(item_df = item_df, reliability_df = reliability_df),
      options = list(
        digits = digits,
        vars = vars,
        group = group_var,
        reverse = reverse_items_list,
        reverse_min = reverse_min,
        reverse_max = reverse_max,
        missing = missing_method,
        score = score_method,
        omega = omega_flag,
        coerce = coerce_flag
      ),
      user_prompt = resolve_get_user_prompt(opts)
    )
  }
}

main()
