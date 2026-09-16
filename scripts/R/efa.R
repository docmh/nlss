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
  cat("  --seed N               Seed for psych extraction/rotation (canonical default)\n")
  cat("  --digits N             Rounding digits (default: 2)\n")
  cat("  --template REF         Template path or template key (optional)\n")
  cat("  --user-prompt TEXT     Original AI user prompt for logging (optional)\n")
  cat("  --log TRUE/FALSE       Write analysis_log.jsonl (default: TRUE)\n")
  cat("  --interactive          Prompt for inputs\n")
  cat("  --help                 Show this help\n")
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

  opts$vars <- prompt("Variables (comma-separated)", "")
  opts$group <- prompt("Grouping variable (optional)", "")

  method_default <- get_config_value("modules.efa.method", "pca")
  rotation_default <- get_config_value("modules.efa.rotation", "varimax")
  n_factors_default <- get_config_value("modules.efa.n_factors", "eigen")
  eigen_threshold_default <- get_config_value("modules.efa.eigen_threshold", 1)
  cor_default <- get_config_value("modules.efa.cor", "pearson")
  missing_default <- get_config_value("modules.efa.missing", "complete")
  loading_cutoff_default <- get_config_value("modules.efa.loading_cutoff", 0.3)
  sort_loadings_default <- get_config_value("modules.efa.sort_loadings", TRUE)
  coerce_default <- get_config_value("modules.efa.coerce", FALSE)
  digits_default <- get_config_value("defaults.digits", 2)

  opts$method <- prompt("Extraction method (pca/pa/minres/ml/uls/gls/wls/alpha)", method_default)
  opts$rotation <- prompt("Rotation (varimax/none/promax/oblimin)", rotation_default)
  opts$`n-factors` <- prompt("Number of factors (numeric or eigen)", n_factors_default)
  opts$`eigen-threshold` <- prompt("Eigenvalue threshold", as.character(eigen_threshold_default))
  opts$cor <- prompt("Correlation type (pearson/spearman/polychoric/tetrachoric)", cor_default)
  opts$missing <- prompt("Missing handling (pairwise/complete)", missing_default)
  opts$`loading-cutoff` <- prompt("Loading cutoff", as.character(loading_cutoff_default))
  opts$`sort-loadings` <- prompt("Sort by loading TRUE/FALSE", ifelse(isTRUE(sort_loadings_default), "TRUE", "FALSE"))
  opts$coerce <- prompt("Coerce non-numeric TRUE/FALSE", ifelse(isTRUE(coerce_default), "TRUE", "FALSE"))
  opts$digits <- prompt("Rounding digits", as.character(digits_default))
  opts$template <- prompt("Template (path or key; blank for default)", "")
  opts$`user-prompt` <- prompt("User prompt (optional)", "")
  log_default <- get_config_value("defaults.log", TRUE)
  opts$log <- prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))

  opts
}

emit_input_issue <- function(out_dir, opts, message, details = list(), status = "invalid_input") {
  nlss_run_context$request$validation_issue <- list(message = message, details = details, status = status)
  stop(message, call. = FALSE)
}

normalize_missing <- function(value, default = "complete") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (val %in% c("pairwise", "pair")) return("pairwise")
  if (val %in% c("complete", "listwise")) return("complete")
  stop("Unsupported missing handling: ", val)
}

normalize_method <- function(value, default = "pca") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (val %in% c("pca", "principal", "components", "component")) return("pca")
  if (val %in% c("pa", "principal_axis", "principal-axis", "principalaxis")) return("pa")
  if (val %in% c("minres", "minimumresidual", "minimum-residual")) return("minres")
  if (val %in% c("ml", "mle", "maximumlikelihood")) return("ml")
  if (val %in% c("uls", "gls", "wls", "alpha")) return(val)
  stop("Unsupported extraction method: ", val)
}

normalize_rotation <- function(value, default = "varimax") {
  raw <- if (!is.null(value) && value != "") value else default
  val <- tolower(raw)
  if (val %in% c("none", "no", "unrotated")) return("none")
  # Preserve legacy case-insensitive names; psych::principal does not accept
  # "Varimax", and uppercase "Promax" denotes a different psych normalization.
  if (val %in% c("varimax", "promax")) return(val)
  rotations <- c("varimax", "Varimax", "quartimax", "bentlerT", "geominT", "targetT",
    "bifactor", "TargetT", "equamax", "varimin", "specialT", "Promax", "promax",
    "cluster", "biquartimin", "TargetQ", "specialQ", "oblimin", "quartimin",
    "simplimax", "geominQ", "bentlerQ", "targetQ")
  if (raw %in% rotations) return(raw)
  idx <- match(val, tolower(rotations))
  if (is.na(idx)) stop("Unsupported rotation: ", raw)
  rotations[idx]
}

normalize_cor <- function(value, default = "pearson") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (val %in% c("pearson", "spearman", "polychoric", "tetrachoric")) return(val)
  stop("Unsupported correlation type: ", val)
}

parse_n_factors <- function(value, default_value = "eigen") {
  raw <- if (!is.null(value) && nzchar(as.character(value))) as.character(value) else as.character(default_value)
  if (is.null(raw) || !nzchar(raw)) raw <- "eigen"
  raw <- tolower(trimws(raw))
  fixed_val <- suppressWarnings(as.numeric(raw))
  if (is.finite(fixed_val) && fixed_val > 0 && fixed_val == floor(fixed_val)) {
    return(list(rule = "fixed", n = fixed_val))
  }
  if (raw %in% c("eigen", "eigen>1", "kaiser", "auto")) {
    return(list(rule = "eigen", n = NA_real_))
  }
  stop("Number of factors must be a positive integer or eigen.")
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
        warning(sprintf("Coercion introduced %s NA values for %s.", introduced_nas, var), call. = FALSE)
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

matrix_record <- function(x) {
  if (is.null(x)) return(NULL)
  x <- as.matrix(x)
  list(row_names = rownames(x), column_names = colnames(x),
    values = lapply(seq_len(nrow(x)), function(i) unname(x[i, ])))
}

compute_correlation <- function(df, cor_type, missing_method) {
  used <- if (missing_method == "complete") complete.cases(df) else rowSums(!is.na(df)) > 0
  df <- df[used, , drop = FALSE]
  if (nrow(df) < 2) stop("Not enough observed cases for correlation matrix.")
  if (cor_type %in% c("pearson", "spearman")) {
    r <- stats::cor(df, use = if (missing_method == "complete") "complete.obs" else "pairwise.complete.obs", method = cor_type)
    return(list(cor_mat = r, raw_cor = r, n_obs = nrow(df), thresholds = NULL, smoothed = FALSE))
  }
  counts <- vapply(df, function(x) length(unique(x[!is.na(x)])), integer(1))
  if (any(counts < 2)) stop("Categorical correlations require at least two observed categories in every group/item.")
  if (cor_type == "tetrachoric" && any(counts != 2)) stop("Tetrachoric correlations require exactly two observed categories per item.")
  # Preserve psych's established continuity correction and global thresholds,
  # but make its automatic smoothing an explicit, recorded step.
  correlation_warnings <- character()
  estimate <- withCallingHandlers(if (cor_type == "polychoric") {
    psych::polychoric(df, smooth = FALSE, global = TRUE, correct = .5, delete = FALSE)
  } else psych::tetrachoric(df, smooth = FALSE, global = TRUE, correct = .5, delete = FALSE),
    warning = function(w) correlation_warnings <<- c(correlation_warnings, conditionMessage(w)))
  raw <- estimate$rho
  if (any(!is.finite(raw))) stop("Categorical correlation matrix contains non-finite values.")
  r <- psych::cor.smooth(raw)
  list(cor_mat = r, raw_cor = raw, n_obs = nrow(df), thresholds = estimate$tau,
    smoothed = !isTRUE(all.equal(raw, r, tolerance = 1e-12)), warnings = correlation_warnings,
    global_thresholds_requested = TRUE,
    global_thresholds_effective = !any(grepl("global set to FALSE", correlation_warnings, fixed = TRUE)))
}

determine_n_factors <- function(eigenvalues, rule, fixed_n, threshold, max_n) {
  if (length(eigenvalues) == 0) return(NA_real_)
  if (rule == "fixed" && !is.na(fixed_n)) {
    n_val <- as.numeric(fixed_n)
  } else {
    n_val <- sum(eigenvalues > threshold)
  }
  if (is.na(n_val) || n_val < 1) n_val <- 1
  if (!is.null(max_n) && n_val > max_n) stop("Requested factors exceed the number of selected items.")
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
  display <- round_numeric(loadings_df, digits)
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

  columns <- normalize_table_columns(
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
        val <- as_cell_text(row$item_display[1])
      } else if (key == "group") {
        val <- as_cell_text(row$group_display[1])
      } else if (key %in% c("factor", "cross_loading")) {
        val <- as_cell_text(row[[key]][1])
      } else if (key %in% names(row)) {
        cell <- row[[key]][1]
        if (key == "loading" && is.numeric(cell) && !is.na(cell)) {
          if (!is.na(loading_cutoff) && abs(loadings_df$loading[i]) < loading_cutoff) {
            val <- ""
          } else {
            val <- format_num(cell, digits)
          }
        } else if (is.numeric(cell)) {
          val <- format_num(cell, digits)
        } else {
          val <- as_cell_text(cell)
        }
      }
      row_vals <- c(row_vals, val)
    }
    rows[[length(rows) + 1]] <- row_vals
  }

  filtered <- drop_empty_columns(columns, rows)
  columns <- filtered$columns
  rows <- filtered$rows
  headers <- vapply(columns, function(col) {
    if (!is.null(col$label) && nzchar(col$label)) col$label else col$key
  }, character(1))
  render_markdown_table(headers, rows)
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
        "Bartlett chi²(",
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
          " chi²(",
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
      "Bartlett chi²(",
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
      group = as_cell_text(row$group),
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
  cor_res <- compute_correlation(df_group[, vars, drop = FALSE], cor_type, missing_method)
  cor_mat <- cor_res$cor_mat
  n_obs <- cor_res$n_obs
  if (!is.matrix(cor_mat) || nrow(cor_mat) != length(vars) || any(!is.finite(cor_mat))) {
    stop("Correlation matrix could not be computed for all selected items.")
  }
  eigenvalues <- eigen(cor_mat, symmetric = TRUE, only.values = TRUE)$values
  tolerance <- sqrt(.Machine$double.eps) * max(1, max(abs(eigenvalues)))
  matrix_status <- if (min(eigenvalues) < -tolerance) "not_positive_semidefinite" else
    if (min(eigenvalues) <= tolerance) "singular" else "positive_definite"
  if (matrix_status == "not_positive_semidefinite") {
    stop("Correlation matrix is not positive semidefinite; revise pairwise missing handling or item selection.")
  }
  n_items <- length(vars)
  n_factors <- determine_n_factors(eigenvalues, n_factors_rule, n_factors_fixed, eigen_threshold, n_items)
  fallback <- n_factors_rule == "eigen" && !any(eigenvalues > eigen_threshold)
  if (fallback) warning("No eigenvalue exceeds the threshold; the retained legacy rule uses one factor.", call. = FALSE)

  fit_warnings <- character()
  efa_res <- withCallingHandlers(
    if (method == "pca") psych::principal(r = cor_mat, nfactors = n_factors,
      rotate = rotation, scores = FALSE, n.obs = n_obs) else
      psych::fa(r = cor_mat, nfactors = n_factors, rotate = rotation, fm = method, n.obs = n_obs),
    warning = function(w) fit_warnings <<- c(fit_warnings, conditionMessage(w)),
    message = function(m) {
      if (grepl("rotation not found", conditionMessage(m), ignore.case = TRUE)) stop(conditionMessage(m))
    })
  loadings <- unclass(efa_res$loadings)
  if (!is.matrix(loadings) || nrow(loadings) != n_items || any(!is.finite(loadings))) {
    stop("Finite loadings could not be computed for all selected items.")
  }
  row_order <- match(vars, rownames(loadings))
  if (anyNA(row_order)) stop("Fitted loading rows cannot be mapped to selected items.")
  loadings <- loadings[row_order, , drop = FALSE]
  psych_factor_names <- colnames(loadings)
  factor_names <- paste0("F", seq_len(ncol(loadings)))
  colnames(loadings) <- factor_names
  Phi <- if (is.null(efa_res$Phi)) diag(ncol(loadings)) else as.matrix(efa_res$Phi)
  dimnames(Phi) <- list(factor_names, factor_names)
  structure <- loadings %*% Phi
  reproduced <- structure %*% t(loadings)
  h2 <- diag(reproduced)
  u2 <- diag(cor_mat) - h2
  complexity <- if (!is.null(efa_res$complexity)) as.numeric(efa_res$complexity[row_order]) else rep(NA_real_, n_items)
  loadings_df <- build_loadings_df(loadings, h2, u2, complexity, group_label, loading_cutoff, sort_loadings, digits)
  eigen_df <- build_eigen_df(eigenvalues, n_items, group_label)
  diagnostic <- function(expr) {
    failure <- NULL
    value <- tryCatch(expr, error = function(e) { failure <<- conditionMessage(e); NULL })
    list(value = value, error = failure)
  }
  kmo <- diagnostic(psych::KMO(cor_mat))
  kmo_val <- if (matrix_status == "positive_definite" && !is.null(kmo$value$MSA) && is.finite(kmo$value$MSA)) as.numeric(kmo$value$MSA) else NA_real_
  bart <- diagnostic(psych::cortest.bartlett(cor_mat, n = n_obs))
  bartlett <- bart$value
  if (is.null(bartlett)) bartlett <- list()
  bart_status <- if (matrix_status == "singular" || is.null(bartlett$chisq) || !is.finite(bartlett$chisq) || !is.finite(bartlett$p.value)) "unavailable" else
    if (missing_method == "pairwise" || cor_type != "pearson") "approximate" else "available"
  if (bart_status == "unavailable") bartlett <- list(chisq = NA_real_, df = n_items * (n_items - 1)/2, p.value = NA_real_)
  variance_explained <- sum(h2) / n_items
  summary_row <- build_summary_row(group_label, n_obs, n_items, n_factors, method, rotation, cor_type, missing_method,
    kmo_val, bartlett, variance_explained, eigen_threshold)
  summary_row$rotation_effective <- if (n_factors == 1L) "none" else rotation
  summary_row$variance_definition <- if (method == "pca") "retained_component_variance" else "extracted_common_variance"
  summary_row$kmo_status <- if (is.finite(kmo_val)) "available" else "unavailable"
  summary_row$bartlett_status <- bart_status
  summary_row$fit_status <- if (any(h2 > 1 + tolerance | u2 < -tolerance)) "improper_solution" else
    if (length(fit_warnings)) "completed_with_warnings" else "completed"
  if (summary_row$fit_status == "improper_solution") warning("EFA solution contains a communality above one or negative uniqueness.", call. = FALSE)
  list(loadings_df = loadings_df, eigen_df = eigen_df, summary_row = summary_row,
    matrices = list(correlation = matrix_record(cor_mat), raw_correlation = matrix_record(cor_res$raw_cor),
      pattern_loadings = matrix_record(loadings), structure_loadings = matrix_record(structure), Phi = matrix_record(Phi),
      reproduced = matrix_record(reproduced), residual = matrix_record(cor_mat - reproduced),
      variance_accounted = matrix_record(efa_res$Vaccounted), thresholds = matrix_record(cor_res$thresholds)),
    diagnostics = list(matrix_status = matrix_status, correlation_smoothed = cor_res$smoothed,
      correlation_warnings = cor_res$warnings, global_thresholds_requested = cor_res$global_thresholds_requested,
      global_thresholds_effective = cor_res$global_thresholds_effective,
      kmo = list(status = summary_row$kmo_status, overall = kmo_val, item = kmo$value$MSAi,
        reason = if (matrix_status == "singular") "Numerically singular correlation matrix." else kmo$error),
      bartlett = list(status = bart_status, n_obs = n_obs,
        reason = if (matrix_status == "singular") "Numerically singular correlation matrix." else if (bart_status == "approximate") "Conventional Bartlett formula uses nominal observed-row N; pairwise/rank/latent correlations do not provide its common-sample Pearson assumptions." else bart$error),
      eigen_rule_minimum_one = fallback, fit_status = summary_row$fit_status, warnings = fit_warnings,
      rotation_requested = rotation, rotation_effective = summary_row$rotation_effective,
      psych_factor_names = psych_factor_names,
      n_rotations = if (method == "pca") eval(formals(psych::principal)$n.rotations) else eval(formals(psych::fa)$n.rotations)))
}

efa_groups <- function(df, vars, group_var, labels) {
  vector <- if (nzchar(group_var)) df[[group_var]] else rep("", nrow(df))
  values <- if (nzchar(group_var)) unique(vector) else ""
  missing_label <- nlss_missing_group_label(vector, labels, if (nzchar(group_var)) group_var else NULL)
  # Display strings do not determine partitions; preserve distinct nearby numeric values.
  rendered <- if (is.numeric(values)) format(values, digits = 17, trim = TRUE, scientific = FALSE) else as.character(values)
  lapply(seq_along(values), function(i) {
    value <- values[i]; missing <- is.na(value)
    rows <- if (missing) which(is.na(vector)) else which(!is.na(vector) & vector == value)
    items <- df[rows, vars, drop = FALSE]
    pair_n <- crossprod(!is.na(as.matrix(items)))
    list(group_index = i, group = if (missing) missing_label else rendered[i],
      value = if (!nzchar(group_var) || missing) NULL else if (is.factor(value)) as.character(value) else unname(value),
      is_missing = missing, row_indices = rows, complete_case_rows = rows[complete.cases(items)],
      item_valid_rows = lapply(items, function(x) rows[!is.na(x)]),
      pairwise_n = matrix_record(pair_n))
  })
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  opts <- nlss_run_options(args, "efa")

  if (!is.null(opts$help)) {
    print_usage()
    quit(status = 0)
  }

  if (parse_bool(opts$interactive, default = FALSE)) {
    opts <- modifyList(opts, interactive_options())
  }

  digits_default <- get_config_value("defaults.digits", 2)
  log_default <- get_config_value("defaults.log", TRUE)
  vars_default <- get_config_value("modules.efa.vars_default", "numeric")
  method_default <- get_config_value("modules.efa.method", "pca")
  rotation_default <- get_config_value("modules.efa.rotation", "varimax")
  n_factors_default <- get_config_value("modules.efa.n_factors", "eigen")
  eigen_threshold_default <- get_config_value("modules.efa.eigen_threshold", 1)
  cor_default <- get_config_value("modules.efa.cor", "pearson")
  missing_default <- get_config_value("modules.efa.missing", "complete")
  loading_cutoff_default <- get_config_value("modules.efa.loading_cutoff", 0.3)
  sort_loadings_default <- get_config_value("modules.efa.sort_loadings", TRUE)
  coerce_default <- get_config_value("modules.efa.coerce", FALSE)

  df <- nlss_load_input(opts)
  out_dir <- get_workspace_out_dir(df)
  nlss_begin_run("efa", df, opts, out_dir)

  digits <- if (!is.null(opts$digits)) as.numeric(opts$digits) else digits_default
  method <- normalize_method(opts$method, method_default)
  rotation <- normalize_rotation(opts$rotation, rotation_default)
  cor_type <- normalize_cor(opts$cor, cor_default)
  missing_method <- normalize_missing(opts$missing, missing_default)
  loading_cutoff <- if (!is.null(opts$`loading-cutoff`)) as.numeric(opts$`loading-cutoff`) else loading_cutoff_default
  if (length(loading_cutoff) != 1L || !is.finite(loading_cutoff) || loading_cutoff < 0) stop("Loading cutoff must be finite and non-negative.")
  sort_loadings <- parse_bool(opts$`sort-loadings`, default = sort_loadings_default)
  coerce_flag <- parse_bool(opts$coerce, default = coerce_default)
  eigen_threshold <- if (!is.null(opts$`eigen-threshold`)) as.numeric(opts$`eigen-threshold`) else eigen_threshold_default
  if (length(eigen_threshold) != 1L || !is.finite(eigen_threshold)) stop("Eigenvalue threshold must be finite.")
  if (length(digits) != 1L || !is.finite(digits) || digits < 0 || digits > 15 || digits != floor(digits)) stop("Digits must be an integer from 0 to 15.")
  seed <- nlss_run_seed(opts$seed, stochastic = TRUE)

  nfactors_spec <- parse_n_factors(opts$`n-factors`, default_value = n_factors_default)
  n_factors_rule <- nfactors_spec$rule
  n_factors_fixed <- nfactors_spec$n

  if (!requireNamespace("psych", quietly = TRUE)) {
    emit_input_issue(out_dir, opts, "EFA requires the 'psych' package.", details = list(package = "psych"), status = "missing_dependency")
  }

  group_var <- ""
  if (!is.null(opts$group) && nzchar(opts$group)) {
    group_var <- as.character(opts$group)
    if (!group_var %in% names(df)) {
      emit_input_issue(out_dir, opts, paste0("Grouping variable not found: ", group_var))
    }
    if (is.numeric(df[[group_var]]) && any(!is.finite(df[[group_var]]) & !is.na(df[[group_var]]))) stop("Grouping values must be finite or missing.")
  }

  if (anyDuplicated(parse_list(opts$vars))) stop("EFA item variables must be unique.")
  vars <- tryCatch(
    select_variables(df, opts$vars, group_var, default = vars_default),
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

  original_items <- df[vars]
  selected <- unique(c(vars, if (nzchar(group_var)) group_var))
  variable_types <- lapply(df[selected], class)
  variable_levels <- lapply(df[selected], function(x) if (is.factor(x)) levels(x) else NULL)
  label_meta <- resolve_label_metadata(df)
  categories <- list()
  for (var in vars) {
    x <- df[[var]]
    if (is.numeric(x) && any(!is.finite(x) & !is.na(x))) stop("Selected item contains non-finite values: ", var)
    if (cor_type %in% c("polychoric", "tetrachoric")) {
      if (!(is.numeric(x) || is.factor(x) || is.character(x) || is.logical(x))) stop("Item is not categorical: ", var)
      order <- if (is.factor(x)) levels(x) else sort(unique(x[!is.na(x)]))
      basis <- if (is.factor(x)) "declared_factor_levels" else if (is.numeric(x) || is.logical(x)) "ascending_codes" else "locale_sorted_text"
      categories[[var]] <- list(order = order, codes = seq_along(order), basis = basis, ordered = is.ordered(x))
      if (cor_type == "tetrachoric" && length(unique(x[!is.na(x)])) > 2L) {
        emit_input_issue(out_dir, opts, paste("Tetrachoric requires binary variables:", var))
      }
      if (is.character(x)) warning("Ordinal category order for ", var, " inferred from locale-sorted text; verify this order before interpreting.", call. = FALSE)
      # All ordinal representations use consecutive analysis codes. Numeric SPSS
      # codes such as 10/20/30 are categories, not twenty-one possible responses.
      df[[var]] <- match(x, order)
    }
  }
  if (cor_type %in% c("pearson", "spearman")) df <- coerce_dataframe(df, vars, coerce_flag)
  for (var in vars) {
    if (!is.numeric(df[[var]])) emit_input_issue(out_dir, opts, paste("Variable is not numeric:", var))
    if (any(!is.finite(df[[var]]) & !is.na(df[[var]]))) stop("Selected item contains non-finite values: ", var)
  }
  groups <- efa_groups(df, vars, group_var, label_meta)
  if (!length(groups)) stop("No observed groups available for EFA.")
  for (i in seq_along(groups)) {
    g <- groups[[i]]
    groups[[i]]$used_rows <- if (missing_method == "complete") g$complete_case_rows else
      g$row_indices[rowSums(!is.na(df[g$row_indices, vars, drop = FALSE])) > 0]
  }
  options <- list(digits = digits, vars = vars, group = group_var, method = method, rotation = rotation,
    n_factors = if (n_factors_rule == "eigen") NA else n_factors_fixed, n_factors_rule = n_factors_rule,
    eigen_threshold = eigen_threshold, cor = cor_type, missing = missing_method, loading_cutoff = loading_cutoff,
    sort_loadings = sort_loadings, coerce = coerce_flag, seed = seed)
  design <- list(rows = nrow(df), groups = groups, variable_types = variable_types, variable_levels = variable_levels,
    categories = categories, coercion = setNames(lapply(vars, function(v) list(
      applied = coerce_flag && !is.numeric(original_items[[v]]) && cor_type %in% c("pearson", "spearman"),
      introduced_missing_rows = which(!is.na(original_items[[v]]) & is.na(df[[v]])))), vars),
    correlation = list(method = cor_type, missing = missing_method,
      categorical_continuity_correction = if (cor_type %in% c("polychoric", "tetrachoric")) .5 else NULL,
      categorical_global_thresholds = cor_type %in% c("polychoric", "tetrachoric")),
    extraction = list(engine = if (method == "pca") "psych::principal" else "psych::fa",
      n_rotations = if (method == "pca") eval(formals(psych::principal)$n.rotations) else eval(formals(psych::fa)$n.rotations)),
    missing = list(grouping = "missing groups form a separate partition",
      pairwise_n_obs = "rows with at least one observed selected item; pairwise_n preserves actual denominators"))
  nlss_resolve_request(options, design)

  loadings_list <- eigen_list <- summary_list <- result_groups <- list()
  for (i in seq_along(groups)) {
    group <- groups[[i]]
    result <- tryCatch(run_efa_group(df[group$row_indices, , drop = FALSE], vars, group$group,
      method, rotation, cor_type, missing_method, n_factors_rule, n_factors_fixed, eigen_threshold,
      loading_cutoff, sort_loadings, digits), error = function(e) e)
    if (inherits(result, "error")) emit_input_issue(out_dir, opts, paste0("EFA failed: ", result$message), status = "fit_failed")
    for (name in c("loadings_df", "eigen_df", "summary_row")) {
      result[[name]]$group_index <- i
      result[[name]]$group_is_missing <- group$is_missing
    }
    loadings_list[[i]] <- result$loadings_df
    eigen_list[[i]] <- result$eigen_df
    summary_list[[i]] <- result$summary_row
    result_groups[[i]] <- list(group_index = i, group = group$group, value = group$value,
      is_missing = group$is_missing, matrices = result$matrices, diagnostics = result$diagnostics)
  }

  loadings_df <- do.call(rbind, loadings_list)
  eigen_df <- do.call(rbind, eigen_list)
  summary_df <- do.call(rbind, summary_list)

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
    resolve_template_path("efa.default", "efa/default-template.md")
  }
  template_path <- nlss_freeze_template(template_path, "efa.main")
  template_meta <- get_template_meta(template_path)

  note_tokens <- build_efa_note_tokens(summary_df, method, rotation, cor_type, missing_method, n_factors_rule, eigen_threshold, loading_cutoff)
  caveats <- c(
    if (rotation != "none" && any(summary_df$n_factors == 1L)) "One-factor solutions are unrotated; the requested rotation applies only to multi-factor solutions.",
    if (any(vapply(result_groups, function(g) isTRUE(g$diagnostics$eigen_rule_minimum_one), logical(1)))) "No eigenvalue exceeded the threshold in at least one group; the legacy minimum-one-factor fallback was used.",
    if (any(summary_df$bartlett_status == "approximate")) "Bartlett tests are conventional approximations for pairwise, rank or latent correlations; n is nominal and per-pair counts are retained.",
    if (any(summary_df$bartlett_status == "unavailable")) "Bartlett test unavailable for at least one group.",
    if (any(summary_df$kmo_status == "unavailable")) "KMO unavailable for at least one group.",
    if (method != "pca") "Variance explained is extracted common variance, not the sum of retained PCA eigenvalues.",
    if (any(vapply(result_groups, function(g) isTRUE(g$diagnostics$correlation_smoothed), logical(1)))) "Categorical correlation matrix was smoothed by psych; raw and fitted matrices are preserved.",
    if (any(summary_df$fit_status != "completed")) "Fit warnings or improper estimates require review before interpreting the solution.")
  note_tokens$note_default <- paste(c(note_tokens$note_default, caveats), collapse = " ")
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
    seed = seed,
    digits = digits
  )

  nlss_report_path <- file.path(out_dir, "report_canonical.md")
  nlss_stage_report(
    nlss_report_path,
    "Exploratory Factor Analysis",
    nlss_table,
    nlss_text,
    analysis_flags = analysis_flags,
    template_path = template_path,
    template_context = template_context
  )

  results <- list(loadings_df = loadings_df, summary_df = summary_df, eigen_df = eigen_df, groups = result_groups)
  nlss_set_result(results)
  if (parse_bool(opts$log, default = log_default)) {
    ctx <- get_run_context()
    nlss_stage_log(out_dir, module = "efa", prompt = ctx$prompt, commands = ctx$commands,
      results = results, options = options, user_prompt = get_user_prompt(opts))
  }
}

nlss_run_main("efa", main)
