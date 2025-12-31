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
source_lib("data_utils.R")
source_lib("formatting.R")


# Static analysis aliases for source_lib-defined functions.
render_output_path <- get("render_output_path", mode = "function")
add_group_label_column <- get("add_group_label_column", mode = "function")
add_variable_label_column <- get("add_variable_label_column", mode = "function")
resolve_label_metadata <- get("resolve_label_metadata", mode = "function")
source_lib <- get("source_lib", mode = "function")

print_usage <- function() {
  cat("Exploratory Factor Analysis (psych)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript efa.R --csv data.csv --vars item1,item2,item3\n")
  cat("  Rscript efa.R --parquet data.parquet --vars item1,item2,item3 --method pca --rotation varimax\n")
  cat("  Rscript efa.R --parquet data.parquet --vars item1,item2,item3 --n-factors 2\n")
  cat("  Rscript efa.R --interactive\n")
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
  cat("  --vars LIST            Comma-separated variables for EFA\n")
  cat("  --group NAME           Grouping variable (optional)\n")
  cat("  --method NAME          Extraction method (pca, pa, minres, ml, uls, gls, wls, alpha)\n")
  cat("  --rotation NAME        Rotation (varimax, none, promax, oblimin, etc.)\n")
  cat("  --n-factors VALUE      Numeric or 'eigen' (default: eigen > 1)\n")
  cat("  --eigen-threshold N    Eigenvalue cutoff (default: 1)\n")
  cat("  --cor NAME             Correlation (pearson, spearman, polychoric, tetrachoric)\n")
  cat("  --missing NAME         Missing handling (pairwise, complete)\n")
  cat("  --loading-cutoff N     Suppress loadings below cutoff (default: 0.30)\n")
  cat("  --sort-loadings TRUE/FALSE Sort by primary loading (default: TRUE)\n")
  cat("  --coerce TRUE/FALSE    Coerce non-numeric inputs (default: FALSE)\n")
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

  opts$vars <- resolve_prompt("Variables (comma-separated)", "")
  opts$group <- resolve_prompt("Grouping variable (optional)", "")

  method_default <- resolve_config_value("modules.efa.method", "pca")
  rotation_default <- resolve_config_value("modules.efa.rotation", "varimax")
  n_factors_default <- resolve_config_value("modules.efa.n_factors", "eigen")
  eigen_threshold_default <- resolve_config_value("modules.efa.eigen_threshold", 1)
  cor_default <- resolve_config_value("modules.efa.cor", "pearson")
  missing_default <- resolve_config_value("modules.efa.missing", "complete")
  loading_cutoff_default <- resolve_config_value("modules.efa.loading_cutoff", 0.3)
  sort_loadings_default <- resolve_config_value("modules.efa.sort_loadings", TRUE)
  coerce_default <- resolve_config_value("modules.efa.coerce", FALSE)
  digits_default <- resolve_config_value("defaults.digits", 2)

  opts$method <- resolve_prompt("Extraction method (pca/pa/minres/ml/uls/gls/wls/alpha)", method_default)
  opts$rotation <- resolve_prompt("Rotation (varimax/none/promax/oblimin)", rotation_default)
  opts$`n-factors` <- resolve_prompt("Number of factors (numeric or eigen)", n_factors_default)
  opts$`eigen-threshold` <- resolve_prompt("Eigenvalue threshold", as.character(eigen_threshold_default))
  opts$cor <- resolve_prompt("Correlation type (pearson/spearman/polychoric/tetrachoric)", cor_default)
  opts$missing <- resolve_prompt("Missing handling (pairwise/complete)", missing_default)
  opts$`loading-cutoff` <- resolve_prompt("Loading cutoff", as.character(loading_cutoff_default))
  opts$`sort-loadings` <- resolve_prompt("Sort by loading TRUE/FALSE", ifelse(isTRUE(sort_loadings_default), "TRUE", "FALSE"))
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
    if (!is.null(group_var) && nzchar(group_var)) selected <- setdiff(selected, group_var)
    return(selected)
  }
  requested <- trimws(strsplit(vars, ",", fixed = TRUE)[[1]])
  missing <- setdiff(requested, available)
  if (length(missing) > 0) {
    stop(paste("Unknown variables:", paste(missing, collapse = ", ")))
  }
  if (!is.null(group_var) && nzchar(group_var)) requested <- setdiff(requested, group_var)
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

resolve_round_numeric <- function(df, digits) {
  if (exists("round_numeric", mode = "function")) {
    return(get("round_numeric", mode = "function")(df, digits))
  }
  df
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

emit_input_issue <- function(out_dir, opts, message, details = list(), status = "invalid_input") {
  log_default <- resolve_config_value("defaults.log", TRUE)
  if (resolve_parse_bool(opts$log, default = log_default)) {
    ctx <- resolve_get_run_context()
    resolve_append_analysis_log(
      out_dir,
      module = "efa",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(
        status = status,
        message = message,
        details = details
      ),
      options = list(
        vars = opts$vars,
        group = opts$group,
        method = opts$method,
        rotation = opts$rotation,
        n_factors = opts$`n-factors`,
        eigen_threshold = opts$`eigen-threshold`,
        cor = opts$cor,
        missing = opts$missing,
        loading_cutoff = opts$`loading-cutoff`,
        sort_loadings = opts$`sort-loadings`,
        coerce = opts$coerce
      ),
      user_prompt = resolve_get_user_prompt(opts)
    )
  }
  stop(message)
}

normalize_missing <- function(value, default = "complete") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (val %in% c("pairwise", "pair")) return("pairwise")
  if (val %in% c("complete", "listwise")) return("complete")
  default
}

normalize_method <- function(value, default = "pca") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (val %in% c("pca", "principal", "components", "component")) return("pca")
  if (val %in% c("pa", "principal_axis", "principal-axis", "principalaxis")) return("pa")
  if (val %in% c("minres", "minimumresidual", "minimum-residual")) return("minres")
  if (val %in% c("ml", "mle", "maximumlikelihood")) return("ml")
  if (val %in% c("uls", "gls", "wls", "alpha")) return(val)
  default
}

normalize_rotation <- function(value, default = "varimax") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (val %in% c("none", "no", "unrotated")) return("none")
  val
}

normalize_cor <- function(value, default = "pearson") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (val %in% c("pearson", "spearman", "polychoric", "tetrachoric")) return(val)
  default
}

parse_n_factors <- function(value, default_value = "eigen") {
  raw <- if (!is.null(value) && nzchar(as.character(value))) as.character(value) else as.character(default_value)
  if (is.null(raw) || !nzchar(raw)) raw <- "eigen"
  raw <- tolower(trimws(raw))
  fixed_val <- suppressWarnings(as.numeric(raw))
  if (!is.na(fixed_val) && fixed_val > 0) {
    return(list(rule = "fixed", n = fixed_val))
  }
  if (raw %in% c("eigen", "eigen>1", "kaiser", "auto")) {
    return(list(rule = "eigen", n = NA_real_))
  }
  list(rule = "eigen", n = NA_real_)
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

format_num <- function(value, digits) {
  if (is.na(value)) return("")
  format(round(value, digits), nsmall = digits, trim = TRUE)
}

format_num_text <- function(value, digits) {
  if (is.na(value)) return("NA")
  format(round(value, digits), nsmall = digits, trim = TRUE)
}

format_p_value <- function(p_value) {
  if (is.na(p_value)) return("p = NA")
  if (p_value < 0.001) return("p < .001")
  p_txt <- format(round(p_value, 3), nsmall = 3, trim = TRUE)
  p_txt <- sub("^0", "", p_txt)
  paste0("p = ", p_txt)
}

compute_correlation <- function(df, cor_type, missing_method) {
  if (cor_type %in% c("pearson", "spearman")) {
    if (missing_method == "complete") {
      df_complete <- df[complete.cases(df), , drop = FALSE]
      if (nrow(df_complete) < 2) {
        stop("Not enough complete cases for correlation matrix.")
      }
      cor_mat <- suppressWarnings(cor(df_complete, use = "complete.obs", method = cor_type))
      n_obs <- nrow(df_complete)
    } else {
      if (nrow(df) < 2) stop("Not enough rows for correlation matrix.")
      cor_mat <- suppressWarnings(cor(df, use = "pairwise.complete.obs", method = cor_type))
      n_obs <- nrow(df)
    }
    return(list(cor_mat = cor_mat, n_obs = n_obs))
  }

  if (missing_method == "complete") {
    df <- df[complete.cases(df), , drop = FALSE]
  }
  if (nrow(df) < 2) stop("Not enough rows for correlation matrix.")

  if (cor_type == "polychoric") {
    res <- psych::polychoric(df)
    return(list(cor_mat = res$rho, n_obs = nrow(df)))
  }
  if (cor_type == "tetrachoric") {
    res <- psych::tetrachoric(df)
    return(list(cor_mat = res$rho, n_obs = nrow(df)))
  }
  stop("Unsupported correlation type.")
}

determine_n_factors <- function(eigenvalues, rule, fixed_n, threshold, max_n) {
  if (length(eigenvalues) == 0) return(NA_real_)
  if (rule == "fixed" && !is.na(fixed_n)) {
    n_val <- as.numeric(fixed_n)
  } else {
    n_val <- sum(eigenvalues > threshold)
  }
  if (is.na(n_val) || n_val < 1) n_val <- 1
  if (!is.null(max_n) && n_val > max_n) n_val <- max_n
  n_val
}

build_loadings_df <- function(loadings, h2, u2, complexity, group_label, loading_cutoff, sort_loadings, digits) {
  item_names <- rownames(loadings)
  if (is.null(item_names) || length(item_names) == 0) {
    item_names <- paste0("Item", seq_len(nrow(loadings)))
  }
  factor_names <- colnames(loadings)
  abs_loadings <- abs(loadings)
  primary_idx <- apply(abs_loadings, 1, function(x) {
    if (all(is.na(x))) return(NA_integer_)
    which.max(x)
  })
  primary_factor <- vapply(primary_idx, function(i) ifelse(is.na(i), NA_character_, factor_names[i]), character(1))
  primary_loading <- vapply(seq_len(nrow(loadings)), function(i) {
    idx <- primary_idx[i]
    if (is.na(idx)) return(NA_real_)
    loadings[i, idx]
  }, numeric(1))

  cross_text <- character(nrow(loadings))
  for (i in seq_len(nrow(loadings))) {
    idx <- which(abs_loadings[i, ] >= loading_cutoff)
    idx <- idx[!is.na(idx) & idx != primary_idx[i]]
    if (length(idx) > 0) {
      pieces <- vapply(idx, function(j) {
        sprintf("%s (%s)", factor_names[j], format_num_text(loadings[i, j], digits))
      }, character(1))
      cross_text[i] <- paste(pieces, collapse = "; ")
    } else {
      cross_text[i] <- ""
    }
  }

  df <- data.frame(
    item = item_names,
    factor = primary_factor,
    loading = primary_loading,
    h2 = h2,
    u2 = u2,
    complexity = complexity,
    cross_loading = cross_text,
    group = group_label,
    stringsAsFactors = FALSE
  )

  if (sort_loadings) {
    order_idx <- order(df$factor, -abs(df$loading), df$item, na.last = TRUE)
    df <- df[order_idx, , drop = FALSE]
  }

  df
}

build_eigen_df <- function(eigenvalues, n_items, group_label) {
  if (length(eigenvalues) == 0) return(data.frame())
  proportion <- eigenvalues / n_items
  cumulative <- cumsum(proportion)
  data.frame(
    group = group_label,
    component = seq_along(eigenvalues),
    eigenvalue = eigenvalues,
    proportion = proportion,
    cumulative = cumulative,
    stringsAsFactors = FALSE
  )
}

build_summary_row <- function(group_label, n_obs, n_items, n_factors, method, rotation, cor_type, missing_method, kmo_val, bartlett, variance_explained, eigen_threshold) {
  data.frame(
    group = group_label,
    n_obs = n_obs,
    n_items = n_items,
    n_factors = n_factors,
    method = method,
    rotation = rotation,
    cor = cor_type,
    missing = missing_method,
    eigen_threshold = eigen_threshold,
    kmo = kmo_val,
    bartlett_chi2 = ifelse(is.null(bartlett$chisq), NA_real_, bartlett$chisq),
    bartlett_df = ifelse(is.null(bartlett$df), NA_real_, bartlett$df),
    bartlett_p = ifelse(is.null(bartlett$p.value), NA_real_, bartlett$p.value),
    variance_explained = variance_explained,
    stringsAsFactors = FALSE
  )
}

build_efa_table_body <- function(loadings_df, digits, loading_cutoff, table_spec = NULL) {
  display <- resolve_round_numeric(loadings_df, digits)
  display$group <- as.character(display$group)
  display$group[is.na(display$group)] <- "NA"
  display$item_display <- if ("item_label" %in% names(display)) display$item_label else display$item
  display$group_display <- if ("group_label" %in% names(display)) display$group_label else display$group

  default_columns <- list(
    list(key = "item", label = "Item"),
    list(key = "group", label = "Group", drop_if_empty = TRUE),
    list(key = "factor", label = "Factor"),
    list(key = "loading", label = "Loading"),
    list(key = "h2", label = "h2", drop_if_empty = TRUE),
    list(key = "u2", label = "u2", drop_if_empty = TRUE),
    list(key = "complexity", label = "Complexity", drop_if_empty = TRUE),
    list(key = "cross_loading", label = "Cross-loadings", drop_if_empty = TRUE)
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
      if (key == "item") {
        val <- resolve_as_cell_text(row$item_display[1])
      } else if (key == "group") {
        val <- resolve_as_cell_text(row$group_display[1])
      } else if (key %in% c("factor", "cross_loading")) {
        val <- resolve_as_cell_text(row[[key]][1])
      } else if (key %in% names(row)) {
        cell <- row[[key]][1]
        if (key == "loading" && is.numeric(cell) && !is.na(cell)) {
          if (!is.na(loading_cutoff) && abs(cell) < loading_cutoff) {
            val <- ""
          } else {
            val <- format_num(cell, digits)
          }
        } else if (is.numeric(cell)) {
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

build_efa_note_tokens <- function(summary_df, method, rotation, cor_type, missing_method, n_factors_rule, eigen_threshold, loading_cutoff) {
  method_text <- paste0("Extraction: ", toupper(method), ".")
  rotation_text <- paste0("Rotation: ", rotation, ".")
  cor_text <- paste0("Correlation: ", cor_type, ".")
  missing_text <- paste0("Missing handling: ", missing_method, ".")
  rule_text <- if (n_factors_rule == "eigen") {
    paste0("Factors retained by eigenvalues > ", format_num_text(eigen_threshold, 2), ".")
  } else {
    "Factors retained by fixed count."
  }
  loading_text <- paste0("Loadings < ", format_num_text(loading_cutoff, 2), " suppressed.")

  kmo_text <- ""
  if (!is.null(summary_df) && nrow(summary_df) > 0) {
    if (nrow(summary_df) == 1) {
      kmo_text <- paste0("KMO = ", format_num_text(summary_df$kmo[1], 2), ".")
    } else {
      parts <- vapply(seq_len(nrow(summary_df)), function(i) {
        label <- ifelse(summary_df$group[i] == "", "Overall", summary_df$group[i])
        paste0(label, "=", format_num_text(summary_df$kmo[i], 2))
      }, character(1))
      kmo_text <- paste0("KMO by group: ", paste(parts, collapse = "; "), ".")
    }
  }

  bartlett_text <- ""
  if (!is.null(summary_df) && nrow(summary_df) > 0) {
    if (nrow(summary_df) == 1) {
      bartlett_text <- paste0(
        "Bartlett chi2(",
        ifelse(is.na(summary_df$bartlett_df[1]), "NA", as.character(summary_df$bartlett_df[1])),
        ") = ",
        format_num_text(summary_df$bartlett_chi2[1], 2),
        ", ",
        format_p_value(summary_df$bartlett_p[1]),
        "."
      )
    } else {
      parts <- vapply(seq_len(nrow(summary_df)), function(i) {
        label <- ifelse(summary_df$group[i] == "", "Overall", summary_df$group[i])
        paste0(
          label,
          " chi2(",
          ifelse(is.na(summary_df$bartlett_df[i]), "NA", as.character(summary_df$bartlett_df[i])),
          ") = ",
          format_num_text(summary_df$bartlett_chi2[i], 2),
          ", ",
          format_p_value(summary_df$bartlett_p[i])
        )
      }, character(1))
      bartlett_text <- paste0("Bartlett by group: ", paste(parts, collapse = "; "), ".")
    }
  }

  note_parts <- c(method_text, rotation_text, cor_text, missing_text, rule_text, loading_text, kmo_text, bartlett_text)
  note_default <- paste(note_parts[nzchar(note_parts)], collapse = " ")

  list(
    method = toupper(method),
    rotation = rotation,
    correlation = cor_type,
    missing = missing_method,
    eigen_threshold = format_num_text(eigen_threshold, 2),
    loading_cutoff = format_num_text(loading_cutoff, 2),
    kmo = kmo_text,
    bartlett = bartlett_text,
    note_default = note_default
  )
}

build_efa_narrative_rows <- function(summary_df, digits, n_factors_rule, eigen_threshold) {
  rows <- list()
  if (is.null(summary_df) || nrow(summary_df) == 0) return(rows)

  for (i in seq_len(nrow(summary_df))) {
    row <- summary_df[i, , drop = FALSE]
    group_label <- if (row$group == "" || is.na(row$group)) "Overall" else paste("Group", row$group)
    variance_pct <- ifelse(is.na(row$variance_explained), "NA", format_num_text(row$variance_explained * 100, digits))
    kmo_text <- format_num_text(row$kmo, digits)
    bartlett_line <- paste0(
      "Bartlett chi2(",
      ifelse(is.na(row$bartlett_df), "NA", as.character(row$bartlett_df)),
      ") = ",
      format_num_text(row$bartlett_chi2, digits),
      ", ",
      format_p_value(row$bartlett_p)
    )
    rule_text <- if (n_factors_rule == "eigen") {
      paste0("eigenvalues > ", format_num_text(eigen_threshold, 2))
    } else {
      "fixed factors"
    }
    line <- paste0(
      group_label,
      ": KMO = ",
      kmo_text,
      "; ",
      bartlett_line,
      ". Retained ",
      ifelse(is.na(row$n_factors), "NA", as.character(row$n_factors)),
      " factor(s) (",
      rule_text,
      "), explaining ",
      variance_pct,
      "% variance (n = ",
      ifelse(is.na(row$n_obs), "NA", as.character(row$n_obs)),
      ")."
    )

    rows[[length(rows) + 1]] <- list(
      group = resolve_as_cell_text(row$group),
      group_label = group_label,
      n_obs = ifelse(is.na(row$n_obs), "NA", as.character(row$n_obs)),
      n_items = ifelse(is.na(row$n_items), "NA", as.character(row$n_items)),
      n_factors = ifelse(is.na(row$n_factors), "NA", as.character(row$n_factors)),
      variance_explained = variance_pct,
      kmo = kmo_text,
      bartlett = bartlett_line,
      full_sentence = line
    )
  }

  rows
}

format_nlss_table <- function(loadings_df, digits, loading_cutoff, note_text) {
  table_body <- build_efa_table_body(loadings_df, digits, loading_cutoff, table_spec = NULL)
  header <- "Table 1\nExploratory factor analysis loadings\n\n"
  note_line <- if (nzchar(note_text)) paste0("Note. ", note_text) else "Note."
  paste0(header, table_body, "\n", note_line, "\n")
}

format_nlss_text <- function(summary_df, digits, n_factors_rule, eigen_threshold) {
  rows <- build_efa_narrative_rows(summary_df, digits, n_factors_rule, eigen_threshold)
  if (length(rows) == 0) return("")
  lines <- vapply(rows, function(row) row$full_sentence, character(1))
  paste(lines, collapse = "\n")
}

run_efa_group <- function(df_group, vars, group_label, method, rotation, cor_type, missing_method, n_factors_rule, n_factors_fixed, eigen_threshold, loading_cutoff, sort_loadings, digits) {
  df_items <- df_group[, vars, drop = FALSE]

  if (cor_type %in% c("polychoric", "tetrachoric")) {
    for (var in vars) {
      if (!is.numeric(df_items[[var]]) && !is.ordered(df_items[[var]])) {
        df_items[[var]] <- as.ordered(df_items[[var]])
      }
    }
  }

  cor_res <- compute_correlation(df_items, cor_type, missing_method)
  cor_mat <- cor_res$cor_mat
  n_obs <- cor_res$n_obs

  if (!is.matrix(cor_mat) || nrow(cor_mat) < 2) {
    stop("Correlation matrix could not be computed.")
  }
  if (any(!is.finite(cor_mat))) {
    stop("Correlation matrix contains non-finite values.")
  }

  eigenvalues <- suppressWarnings(eigen(cor_mat, symmetric = TRUE, only.values = TRUE)$values)
  n_items <- length(vars)
  n_factors <- determine_n_factors(eigenvalues, n_factors_rule, n_factors_fixed, eigen_threshold, n_items)

  if (is.na(n_factors) || n_factors < 1) {
    stop("Unable to determine number of factors.")
  }

  efa_res <- tryCatch({
    if (method == "pca") {
      psych::principal(r = cor_mat, nfactors = n_factors, rotate = rotation, scores = FALSE)
    } else {
      psych::fa(r = cor_mat, nfactors = n_factors, rotate = rotation, fm = method, n.obs = n_obs)
    }
  }, error = function(e) e)

  if (inherits(efa_res, "error")) {
    stop(efa_res$message)
  }

  loadings <- as.matrix(efa_res$loadings)
  if (nrow(loadings) == 0) {
    stop("Loadings could not be computed.")
  }

  if (!is.null(rownames(loadings))) {
    row_order <- match(vars, rownames(loadings))
    if (all(!is.na(row_order))) {
      loadings <- loadings[row_order, , drop = FALSE]
    }
  }

  factor_names <- paste0("F", seq_len(ncol(loadings)))
  colnames(loadings) <- factor_names

  h2 <- rowSums(loadings^2, na.rm = TRUE)
  u2 <- 1 - h2
  complexity <- if (!is.null(efa_res$complexity)) {
    as.numeric(efa_res$complexity)
  } else {
    rep(NA_real_, length(h2))
  }

  loadings_df <- build_loadings_df(loadings, h2, u2, complexity, group_label, loading_cutoff, sort_loadings, digits)
  eigen_df <- build_eigen_df(eigenvalues, n_items, group_label)

  kmo_val <- NA_real_
  bartlett <- list()
  kmo_res <- tryCatch(psych::KMO(cor_mat), error = function(e) NULL)
  if (!is.null(kmo_res) && !is.null(kmo_res$MSA)) {
    kmo_val <- as.numeric(kmo_res$MSA)
  }
  bartlett_res <- tryCatch(psych::cortest.bartlett(cor_mat, n = n_obs), error = function(e) NULL)
  if (!is.null(bartlett_res)) {
    bartlett <- bartlett_res
  }

  variance_explained <- sum(eigenvalues[seq_len(n_factors)]) / n_items

  summary_row <- build_summary_row(
    group_label = group_label,
    n_obs = n_obs,
    n_items = n_items,
    n_factors = n_factors,
    method = method,
    rotation = rotation,
    cor_type = cor_type,
    missing_method = missing_method,
    kmo_val = kmo_val,
    bartlett = bartlett,
    variance_explained = variance_explained,
    eigen_threshold = eigen_threshold
  )

  list(
    loadings_df = loadings_df,
    eigen_df = eigen_df,
    summary_row = summary_row
  )
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
  vars_default <- resolve_config_value("modules.efa.vars_default", "numeric")
  method_default <- resolve_config_value("modules.efa.method", "pca")
  rotation_default <- resolve_config_value("modules.efa.rotation", "varimax")
  n_factors_default <- resolve_config_value("modules.efa.n_factors", "eigen")
  eigen_threshold_default <- resolve_config_value("modules.efa.eigen_threshold", 1)
  cor_default <- resolve_config_value("modules.efa.cor", "pearson")
  missing_default <- resolve_config_value("modules.efa.missing", "complete")
  loading_cutoff_default <- resolve_config_value("modules.efa.loading_cutoff", 0.3)
  sort_loadings_default <- resolve_config_value("modules.efa.sort_loadings", TRUE)
  coerce_default <- resolve_config_value("modules.efa.coerce", FALSE)

  digits <- if (!is.null(opts$digits)) as.numeric(opts$digits) else digits_default
  method <- normalize_method(opts$method, method_default)
  rotation <- normalize_rotation(opts$rotation, rotation_default)
  cor_type <- normalize_cor(opts$cor, cor_default)
  missing_method <- normalize_missing(opts$missing, missing_default)
  loading_cutoff <- if (!is.null(opts$`loading-cutoff`)) as.numeric(opts$`loading-cutoff`) else loading_cutoff_default
  if (is.na(loading_cutoff) || loading_cutoff < 0) loading_cutoff <- loading_cutoff_default
  sort_loadings <- resolve_parse_bool(opts$`sort-loadings`, default = sort_loadings_default)
  coerce_flag <- resolve_parse_bool(opts$coerce, default = coerce_default)
  eigen_threshold <- if (!is.null(opts$`eigen-threshold`)) as.numeric(opts$`eigen-threshold`) else eigen_threshold_default
  if (is.na(eigen_threshold)) eigen_threshold <- eigen_threshold_default

  nfactors_spec <- parse_n_factors(opts$`n-factors`, default_value = n_factors_default)
  n_factors_rule <- nfactors_spec$rule
  n_factors_fixed <- nfactors_spec$n

  df <- resolve_load_dataframe(opts)
  out_dir <- resolve_get_workspace_out_dir(df)

  if (!requireNamespace("psych", quietly = TRUE)) {
    emit_input_issue(out_dir, opts, "EFA requires the 'psych' package.", details = list(package = "psych"), status = "missing_dependency")
  }

  group_var <- ""
  if (!is.null(opts$group) && nzchar(opts$group)) {
    group_var <- as.character(opts$group)
    if (!group_var %in% names(df)) {
      emit_input_issue(out_dir, opts, paste0("Grouping variable not found: ", group_var))
    }
    df[[group_var]] <- as.factor(df[[group_var]])
  }

  vars <- tryCatch(
    resolve_select_variables(df, opts$vars, group_var, default = vars_default),
    error = function(e) {
      emit_input_issue(out_dir, opts, conditionMessage(e))
      character(0)
    }
  )
  if (length(vars) < 2) {
    emit_input_issue(out_dir, opts, "EFA requires at least two variables.")
  }

  missing_vars <- setdiff(vars, names(df))
  if (length(missing_vars) > 0) {
    emit_input_issue(out_dir, opts, paste("Unknown variables:", paste(missing_vars, collapse = ", ")))
  }

  if (cor_type %in% c("pearson", "spearman")) {
    df <- coerce_dataframe(df, vars, coerce_flag)
    for (var in vars) {
      if (!is.numeric(df[[var]])) {
        emit_input_issue(out_dir, opts, paste("Variable is not numeric:", var))
      }
    }
  } else if (cor_type == "tetrachoric") {
    for (var in vars) {
      vec <- df[[var]]
      if (!is.numeric(vec) && !is.factor(vec) && !is.character(vec)) {
        emit_input_issue(out_dir, opts, paste("Variable is not categorical:", var))
      }
      levels <- unique(vec[!is.na(vec)])
      if (length(levels) > 2) {
        emit_input_issue(out_dir, opts, paste("Tetrachoric requires binary variables:", var))
      }
    }
  }

  group_levels <- ""
  if (nzchar(group_var)) {
    group_levels <- unique(df[[group_var]])
  } else {
    group_levels <- NA
  }

  loadings_list <- list()
  eigen_list <- list()
  summary_list <- list()

  for (g in group_levels) {
    if (nzchar(group_var)) {
      idx <- if (is.na(g)) is.na(df[[group_var]]) else df[[group_var]] == g
      df_group <- df[idx, , drop = FALSE]
      group_label <- ifelse(is.na(g), "NA", as.character(g))
    } else {
      df_group <- df
      group_label <- ""
    }

    result <- tryCatch({
      run_efa_group(
        df_group,
        vars,
        group_label,
        method,
        rotation,
        cor_type,
        missing_method,
        n_factors_rule,
        n_factors_fixed,
        eigen_threshold,
        loading_cutoff,
        sort_loadings,
        digits
      )
    }, error = function(e) e)

    if (inherits(result, "error")) {
      emit_input_issue(out_dir, opts, paste0("EFA failed: ", result$message), status = "fit_failed")
    }

    loadings_list[[length(loadings_list) + 1]] <- result$loadings_df
    eigen_list[[length(eigen_list) + 1]] <- result$eigen_df
    summary_list[[length(summary_list) + 1]] <- result$summary_row
  }

  loadings_df <- do.call(rbind, loadings_list)
  eigen_df <- do.call(rbind, eigen_list)
  summary_df <- do.call(rbind, summary_list)

  label_meta <- resolve_label_metadata(df)
  loadings_df <- add_variable_label_column(loadings_df, label_meta, var_col = "item")
  if (nzchar(group_var)) {
    loadings_df <- add_group_label_column(loadings_df, label_meta, group_var, group_col = "group")
    summary_df <- add_group_label_column(summary_df, label_meta, group_var, group_col = "group")
    if (!is.null(eigen_df) && nrow(eigen_df) > 0) {
      eigen_df <- add_group_label_column(eigen_df, label_meta, group_var, group_col = "group")
    }
  }

  template_override <- resolve_template_override(opts$template, module = "efa")
  template_path <- if (!is.null(template_override)) {
    template_override
  } else {
    resolve_get_template_path("efa.default", "efa/default-template.md")
  }
  template_meta <- resolve_get_template_meta(template_path)

  note_tokens <- build_efa_note_tokens(summary_df, method, rotation, cor_type, missing_method, n_factors_rule, eigen_threshold, loading_cutoff)
  nlss_text <- format_nlss_text(summary_df, digits, n_factors_rule, eigen_threshold)
  nlss_table <- format_nlss_table(loadings_df, digits, loading_cutoff, note_tokens$note_default)
  table_body <- build_efa_table_body(loadings_df, digits, loading_cutoff, template_meta$table)
  narrative_rows <- build_efa_narrative_rows(summary_df, digits, n_factors_rule, eigen_threshold)

  template_context <- list(
    tokens = c(
      list(
        table_body = table_body,
        narrative_default = nlss_text
      ),
      note_tokens
    ),
    narrative_rows = narrative_rows
  )

  analysis_flags <- list(
    vars = vars,
    group = if (nzchar(group_var)) group_var else "None",
    method = method,
    rotation = rotation,
    "n-factors" = if (n_factors_rule == "eigen") paste0("eigen > ", eigen_threshold) else n_factors_fixed,
    "eigen-threshold" = if (n_factors_rule == "eigen") eigen_threshold else NULL,
    cor = cor_type,
    missing = missing_method,
    "loading-cutoff" = loading_cutoff,
    "sort-loadings" = sort_loadings,
    coerce = coerce_flag,
    digits = digits
  )

  nlss_report_path <- file.path(out_dir, "report_canonical.md")
  resolve_append_nlss_report(
    nlss_report_path,
    "Exploratory Factor Analysis",
    nlss_table,
    nlss_text,
    analysis_flags = analysis_flags,
    template_path = template_path,
    template_context = template_context
  )

  cat("Wrote:\n")
  cat("- ", render_output_path(nlss_report_path, out_dir), "\n", sep = "")

  if (resolve_parse_bool(opts$log, default = log_default)) {
    ctx <- resolve_get_run_context()
    resolve_append_analysis_log(
      out_dir,
      module = "efa",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(
        loadings_df = loadings_df,
        summary_df = summary_df,
        eigen_df = eigen_df
      ),
      options = list(
        digits = digits,
        vars = vars,
        group = group_var,
        method = method,
        rotation = rotation,
        n_factors = if (n_factors_rule == "eigen") NA else n_factors_fixed,
        n_factors_rule = n_factors_rule,
        eigen_threshold = eigen_threshold,
        cor = cor_type,
        missing = missing_method,
        loading_cutoff = loading_cutoff,
        sort_loadings = sort_loadings,
        coerce = coerce_flag
      ),
      user_prompt = resolve_get_user_prompt(opts)
    )
  }
}

main()
