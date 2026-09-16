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

  opts$vars <- prompt("Items (comma-separated, blank for numeric)", "")
  opts$group <- prompt("Grouping variable (blank for none)", "")
  opts$reverse <- prompt("Reverse-scored items (comma-separated, blank for none)", "")

  reverse_min_default <- get_config_value("modules.scale.reverse_min", NULL)
  reverse_max_default <- get_config_value("modules.scale.reverse_max", NULL)
  if (!is.null(opts$reverse) && nzchar(opts$reverse)) {
    reverse_min_text <- if (!is.null(reverse_min_default)) as.character(reverse_min_default) else ""
    reverse_max_text <- if (!is.null(reverse_max_default)) as.character(reverse_max_default) else ""
    opts$`reverse-min` <- prompt("Reverse min (blank for observed)", reverse_min_text)
    opts$`reverse-max` <- prompt("Reverse max (blank for observed)", reverse_max_text)
  }

  missing_default <- get_config_value("modules.scale.missing", "pairwise")
  score_default <- get_config_value("modules.scale.score", "sum")
  omega_default <- get_config_value("modules.scale.omega", TRUE)
  coerce_default <- get_config_value("modules.scale.coerce", FALSE)
  digits_default <- get_config_value("defaults.digits", 2)

  opts$missing <- prompt("Missing handling (pairwise/complete)", missing_default)
  opts$score <- prompt("Scale score (sum/mean)", score_default)
  opts$omega <- prompt("Compute omega TRUE/FALSE", ifelse(isTRUE(omega_default), "TRUE", "FALSE"))
  opts$coerce <- prompt("Coerce non-numeric TRUE/FALSE", ifelse(isTRUE(coerce_default), "TRUE", "FALSE"))
  opts$digits <- prompt("Rounding digits", as.character(digits_default))
  opts$template <- prompt("Template (path or key; blank for default)", "")
  opts$`user-prompt` <- prompt("User prompt (optional)", "")
  log_default <- get_config_value("defaults.log", TRUE)
  opts$log <- prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))
  opts
}

normalize_missing <- function(value, default = "pairwise") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (val %in% c("pairwise", "pair")) return("pairwise")
  if (val %in% c("complete", "listwise")) return("complete")
  stop("Missing handling must be pairwise or complete (listwise).")
}

normalize_score <- function(value, default = "sum") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (val %in% c("sum", "total")) return("sum")
  if (val %in% c("mean", "average", "avg")) return("mean")
  stop("Score method must be sum or mean.")
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
        warning(sprintf("coercion introduced %s NA values for %s.", introduced_nas, var), call. = FALSE)
      }
      df[[var]] <- converted
    }
  }
  df
}

parse_optional_numeric <- function(value, label) {
  if (is.null(value)) return(NULL)
  value <- as.character(value)
  if (!nzchar(value)) return(NULL)
  num <- suppressWarnings(as.numeric(value))
  if (length(num) != 1L || !is.finite(num)) stop(label, " must be a finite number.")
  num
}

reverse_items <- function(df, items, reverse_items, reverse_min = NULL, reverse_max = NULL) {
  if (length(reverse_items) == 0) {
    return(list(df = df, info = list(items = character(0), method = "none")))
  }
  reverse_items <- intersect(reverse_items, items)
  method <- "fixed"
  if (is.null(reverse_min) && is.null(reverse_max)) {
    method <- "observed"
  }
  per_item <- data.frame(item = character(0), min = numeric(0), max = numeric(0), status = character(0), stringsAsFactors = FALSE)
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
      if (any(vec < min_val | vec > max_val, na.rm = TRUE)) {
        stop("Reverse-scored item ", item, " contains values outside the declared bounds.")
      }
    }
    status <- if (is.na(min_val)) "no_observed_values" else "applied"
    if (!is.na(min_val) && !is.na(max_val) && is.finite(min_val) && is.finite(max_val)) {
      df[[item]] <- max_val + min_val - vec
    }
    per_item <- rbind(
      per_item,
      data.frame(item = item, min = min_val, max = max_val, status = status, stringsAsFactors = FALSE)
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
  if (any(!is.finite(cov_mat))) return(NA_real_)
  total_var <- sum(cov_mat)
  sum_item_var <- sum(diag(cov_mat))
  if (is.na(total_var) || total_var <= 0) return(NA_real_)
  if (is.na(sum_item_var) || sum_item_var <= 0) return(NA_real_)
  (k / (k - 1)) * (1 - sum_item_var / total_var)
}

compute_item_total_r <- function(cov_mat, item_name, corrected = FALSE) {
  if (is.null(cov_mat) || !is.matrix(cov_mat)) return(NA_real_)
  idx <- match(item_name, colnames(cov_mat))
  if (is.na(idx)) return(NA_real_)
  if (any(!is.finite(cov_mat))) return(NA_real_)
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
  if (any(!is.finite(cor_mat))) return(list(value = NA_real_, status = "correlation_missing"))
  if (is.null(n_obs) || is.na(n_obs) || n_obs < 3) {
    return(list(value = NA_real_, status = "insufficient_n"))
  }
  if (min(eigen(cor_mat, symmetric = TRUE, only.values = TRUE)$values) <= sqrt(.Machine$double.eps)) {
    return(list(value = NA_real_, status = "correlation_not_positive_definite"))
  }
  # nstart=1 uses stats' deterministic algebraic start, not random starts.
  # Unexpected estimation errors propagate to the failed-run boundary.
  res <- stats::factanal(covmat = list(cov = cor_mat, n.obs = n_obs),
    factors = 1, rotation = "none", control = list(nstart = 1))
  loadings <- as.numeric(res$loadings[, 1])
  uniq <- res$uniquenesses
  if (any(!is.finite(loadings)) || any(!is.finite(uniq))) stop("Omega estimation returned non-finite loadings or uniquenesses.")
  numerator <- (sum(loadings))^2
  denom <- numerator + sum(uniq)
  if (!is.finite(denom) || denom <= 0) stop("Omega estimation returned an invalid denominator.")
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
  # A mean of only the available correlations is not standardized alpha for
  # all k selected items; do not silently omit undefined item pairs.
  alpha_std <- compute_alpha(cor_mat)
  alpha_status <- if (k < 2) "insufficient_items" else if (is.null(cov_mat) || any(!is.finite(cov_mat))) {
    "covariance_missing"
  } else if (is.na(alpha)) "nonpositive_variance" else "ok"
  alpha_std_status <- if (k < 2) "insufficient_items" else if (is.null(cor_mat) || any(!is.finite(cor_mat))) {
    "correlation_missing"
  } else if (is.na(alpha_std)) "nonpositive_variance" else "ok"

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
    alpha_status = alpha_status,
    alpha_std_status = alpha_std_status,
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

scale_availability_text <- function(row) {
  parts <- character(0)
  for (field in c("alpha", "alpha_std", "omega")) {
    status <- row[[paste0(field, "_status")]]
    if (!is.null(status) && status != "ok" && status != "disabled") {
      label <- switch(field, alpha = "Alpha", alpha_std = "Standardized alpha", omega = "Omega")
      parts <- c(parts, paste0(label, " unavailable (", gsub("_", " ", status), ")."))
    }
  }
  paste(parts, collapse = " ")
}

format_nlss_text <- function(reliability_df, digits) {
  rows <- build_scale_narrative_rows(reliability_df, digits)
  paste(vapply(rows, function(row) row$full_sentence, character(1)), collapse = "\n")
}

build_scale_table_body <- function(item_df, digits, table_spec = NULL) {
  display <- round_numeric(item_df, digits)
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
      if (key %in% c("item", "group")) {
        if (key == "item") {
          val <- as_cell_text(row$item_display[1])
        } else {
          val <- as_cell_text(row$group_display[1])
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
    unavailable <- reverse_info$per_item$item[reverse_info$per_item$status == "no_observed_values"]
    if (length(unavailable)) reverse_note <- paste(reverse_note,
      "No observed values to reverse for", paste(unavailable, collapse = ", "), ".")
  }

  missing_note <- paste0("Reliability computed using ", missing_method,
    " observations; item descriptives use each item's available values.")
  score_note <- paste0("Scale score based on ", score_method, " of complete cases.")

  omega_note <- ""
  if (omega_flag) {
    if (length(omega_statuses) > 0 && any(omega_statuses == "ok", na.rm = TRUE)) {
      omega_note <- "Standardized omega total estimated via a one-factor maximum-likelihood model (deterministic start)."
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
  display <- round_numeric(reliability_df, digits)
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

    line <- paste(line, scale_availability_text(row))
    rows[[length(rows) + 1]] <- list(
      group = as_cell_text(row$group_display),
      group_label = group_label,
      n_items = ifelse(is.na(row$n_items), "NA", as.character(row$n_items)),
      n_total = ifelse(is.na(row$n_total), "NA", as.character(row$n_total)),
      n_complete = ifelse(is.na(row$n_complete), "NA", as.character(row$n_complete)),
      missing_n = ifelse(is.na(row$missing_n), "NA", as.character(row$missing_n)),
      missing_pct = missing_pct,
      alpha = alpha_text,
      alpha_std = alpha_std_text,
      omega_total = omega_text,
      alpha_status = as_cell_text(row$alpha_status),
      alpha_std_status = as_cell_text(row$alpha_std_status),
      omega_status = as_cell_text(row$omega_status),
      availability_text = scale_availability_text(row),
      r_bar = r_bar_text,
      r_min = format_num_text(row$r_min, digits),
      r_max = format_num_text(row$r_max, digits),
      score_method = as_cell_text(row$score_method),
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

format_nlss_table <- function(item_df, digits, note_text) {
  table_body <- build_scale_table_body(item_df, digits, table_spec = NULL)
  header <- "Table 1\nScale item analysis\n\n"
  note_line <- if (nzchar(note_text)) paste0("Note. ", note_text) else "Note."
  paste0(header, table_body, "\n", note_line, "\n")
}

scale_groups <- function(df, vars, group_var, labels) {
  group_vec <- if (is.null(group_var)) rep("", nrow(df)) else df[[group_var]]
  values <- if (is.null(group_var)) "" else unique(group_vec)
  missing_label <- nlss_missing_group_label(group_vec, labels, group_var)
  lapply(seq_along(values), function(i) {
    value <- values[i]
    missing <- is.na(value)
    rows <- if (missing) which(is.na(group_vec)) else which(!is.na(group_vec) & group_vec == value)
    items <- df[rows, vars, drop = FALSE]
    pairwise_n <- crossprod(!is.na(as.matrix(items)))
    list(group = if (missing) missing_label else as.character(value),
      value = if (is.null(group_var) || missing) NULL else if (is.factor(value)) as.character(value) else unname(value),
      is_missing = missing, row_indices = rows,
      complete_case_rows = rows[complete.cases(items)],
      item_valid_rows = lapply(items, function(x) rows[!is.na(x)]),
      pairwise_n = setNames(lapply(vars, function(v) as.list(pairwise_n[v, ])), vars))
  })
}

scale_matrix_status <- function(x) {
  if (any(!is.finite(x))) return("incomplete")
  eigenvalues <- eigen(x, symmetric = TRUE, only.values = TRUE)$values
  tolerance <- sqrt(.Machine$double.eps) * max(1, max(abs(eigenvalues)))
  if (min(eigenvalues) < -tolerance) return("not_positive_semidefinite")
  if (min(eigenvalues) <= tolerance) return("singular")
  "positive_definite"
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  opts <- nlss_run_options(args, "scale")

  if (!is.null(opts$help)) {
    print_usage()
    quit(status = 0)
  }

  if (parse_bool(opts$interactive, default = FALSE)) {
    opts <- modifyList(opts, interactive_options())
  }

  digits_default <- get_config_value("defaults.digits", 2)
  log_default <- get_config_value("defaults.log", TRUE)
  vars_default <- get_config_value("modules.scale.vars_default", "numeric")
  missing_default <- get_config_value("modules.scale.missing", "pairwise")
  score_default <- get_config_value("modules.scale.score", "sum")
  omega_default <- get_config_value("modules.scale.omega", TRUE)
  coerce_default <- get_config_value("modules.scale.coerce", FALSE)
  reverse_min_default <- get_config_value("modules.scale.reverse_min", NULL)
  reverse_max_default <- get_config_value("modules.scale.reverse_max", NULL)

  df <- nlss_load_input(opts)
  out_dir <- get_workspace_out_dir(df)
  nlss_begin_run("scale", df, opts, out_dir)
  digits <- if (!is.null(opts$digits)) as.numeric(opts$digits) else digits_default
  if (length(digits) != 1L || !is.finite(digits) || digits < 0 || digits > 15 || digits != floor(digits)) {
    stop("Digits must be an integer from 0 to 15.")
  }
  missing_method <- normalize_missing(opts$missing, default = missing_default)
  score_method <- normalize_score(opts$score, default = score_default)
  omega_flag <- parse_bool(opts$omega, default = omega_default)
  coerce_flag <- parse_bool(opts$coerce, default = coerce_default)

  reverse_min <- if (!is.null(opts$`reverse-min`)) {
    parse_optional_numeric(opts$`reverse-min`, "Reverse minimum")
  } else {
    parse_optional_numeric(reverse_min_default, "Reverse minimum")
  }
  reverse_max <- if (!is.null(opts$`reverse-max`)) {
    parse_optional_numeric(opts$`reverse-max`, "Reverse maximum")
  } else {
    parse_optional_numeric(reverse_max_default, "Reverse maximum")
  }

  if (xor(is.null(reverse_min), is.null(reverse_max))) {
    stop("Supply both reverse minimum and maximum, or neither for observed bounds.")
  }
  if (!is.null(reverse_min) && reverse_min >= reverse_max) {
    stop("Reverse minimum must be less than reverse maximum.")
  }
  group_var <- if (!is.null(opts$group) && opts$group != "") opts$group else NULL

  if (!is.null(group_var) && !(group_var %in% names(df))) {
    stop("Grouping variable not found in data frame.")
  }

  requested_items <- parse_list(opts$vars)
  requested_items <- requested_items[!requested_items %in% group_var]
  if (anyDuplicated(requested_items)) stop("Scale item variables must be unique.")
  vars <- select_variables(df, opts$vars, group_var, default = vars_default)
  if (length(vars) == 0) stop("No item variables available for scale analysis.")
  if (anyDuplicated(vars)) stop("Scale item variables must be unique.")

  missing_vars <- setdiff(vars, names(df))
  if (length(missing_vars) > 0) {
    stop(paste("Unknown variables:", paste(missing_vars, collapse = ", ")))
  }

  original_items <- df[vars]
  variable_types <- lapply(df[unique(c(vars, group_var))], class)
  variable_levels <- lapply(df[unique(c(vars, group_var))], function(x) if (is.factor(x)) levels(x) else NULL)
  label_meta <- resolve_label_metadata(df)
  df <- coerce_dataframe(df, vars, coerce_flag)
  for (var in vars) {
    if (!is.numeric(df[[var]])) {
      stop(paste("Item is not numeric:", var, "(use --coerce to convert)."))
    }
    if (any(!is.finite(df[[var]]) & !is.na(df[[var]]))) {
      stop("Scale item contains non-finite values: ", var)
    }
  }

  reverse_items_list <- parse_list(opts$reverse)
  if (anyDuplicated(reverse_items_list)) stop("Reverse-scored items must be unique.")
  if (length(reverse_items_list) > 0) {
    missing_reverse <- setdiff(reverse_items_list, vars)
    if (length(missing_reverse) > 0) {
      stop(paste("Unknown reverse items:", paste(missing_reverse, collapse = ", ")))
    }
  }

  reverse_result <- reverse_items(df, vars, reverse_items_list, reverse_min, reverse_max)
  df <- reverse_result$df
  reverse_info <- reverse_result$info

  groups <- scale_groups(df, vars, group_var, label_meta)
  if (!length(groups)) stop("No observed groups available for scale analysis.")
  options <- list(digits = digits, vars = vars, group = group_var,
    reverse = reverse_items_list, reverse_min = reverse_min, reverse_max = reverse_max,
    missing = missing_method, score = score_method, omega = omega_flag, coerce = coerce_flag)
  nlss_resolve_request(options, design = list(
    rows = nrow(df), groups = groups, reverse = reverse_info,
    variable_types = variable_types, variable_levels = variable_levels,
    analysis_types = lapply(df[vars], class),
    coercion = setNames(lapply(vars, function(v) list(
      applied = coerce_flag && !is.numeric(original_items[[v]]),
      introduced_missing_rows = which(!is.na(original_items[[v]]) & is.na(df[[v]])))), vars),
    missing = list(reliability = missing_method, item_descriptives = "variablewise",
      scores = "complete cases across all selected items",
      grouping = "missing grouping values form a separate group"),
    omega = list(method = "stats::factanal, one-factor standardized omega total",
      factors = 1L, rotation = "none", nstart = 1L, stochastic = FALSE,
      correlation = missing_method, n_obs = "complete cases", lower = 0.005)))

  item_list <- reliability_list <- diagnostics_list <- list()
  use_method <- if (missing_method == "pairwise") "pairwise.complete.obs" else "complete.obs"
  for (group in groups) {
    items_df <- df[group$row_indices, vars, drop = FALSE]
    empty <- matrix(NA_real_, length(vars), length(vars), dimnames = list(vars, vars))
    if (nrow(items_df) < 2 || (missing_method == "complete" && sum(complete.cases(items_df)) < 2)) {
      cov_mat <- cor_mat <- empty
    } else {
      cov_mat <- stats::cov(items_df, use = use_method)
      cor_mat <- stats::cor(items_df, use = use_method)
    }
    covariance_status <- scale_matrix_status(cov_mat)
    correlation_status <- scale_matrix_status(cor_mat)
    if ("not_positive_semidefinite" %in% c(covariance_status, correlation_status)) {
      warning("Group ", if (nzchar(group$group)) group$group else "(all)",
        ": pairwise matrix is not positive semidefinite; covariance-based reliability is unavailable.",
        call. = FALSE)
    }
    # Pairwise deletion can produce an impossible joint covariance matrix.
    # Keep marginal descriptives and inter-item correlations, but do not
    # publish item-total or alpha estimates from an invalid joint matrix.
    reliability_cov <- if (covariance_status == "not_positive_semidefinite") empty else cov_mat
    reliability_cor <- if (correlation_status == "not_positive_semidefinite") empty else cor_mat
    item_result <- build_item_stats(items_df, reliability_cov, group$group, nrow(items_df))
    reliability_result <- compute_reliability_summary(
      items_df, reliability_cov, reliability_cor, group$group, score_method, omega_flag)
    if (covariance_status == "not_positive_semidefinite") {
      reliability_result$alpha_status <- "covariance_not_positive_semidefinite"
    }
    if (correlation_status == "not_positive_semidefinite") {
      reliability_result$alpha_std_status <- "correlation_not_positive_semidefinite"
      if (omega_flag) reliability_result$omega_status <- "correlation_not_positive_definite"
      r_stats <- compute_r_bar(cor_mat)
      for (key in names(r_stats)) reliability_result[[key]] <- r_stats[[key]]
    }
    item_result$group_missing <- group$is_missing
    reliability_result$group_missing <- group$is_missing
    item_list[[length(item_list) + 1L]] <- item_result
    reliability_list[[length(reliability_list) + 1L]] <- reliability_result
    diagnostics_list[[length(diagnostics_list) + 1L]] <- data.frame(
      group = group$group, group_missing = group$is_missing,
      covariance_status = covariance_status, correlation_status = correlation_status,
      n_complete = length(group$complete_case_rows), stringsAsFactors = FALSE)
  }

  item_df <- do.call(rbind, item_list)
  reliability_df <- do.call(rbind, reliability_list)
  diagnostics_df <- do.call(rbind, diagnostics_list)
  item_df <- add_variable_label_column(item_df, label_meta, var_col = "item")
  item_df <- add_group_label_column(item_df, label_meta, group_var, group_col = "group")
  reliability_df <- add_group_label_column(reliability_df, label_meta, group_var, group_col = "group")
  if (!is.null(group_var)) {
    item_df$group_label[item_df$group_missing] <- item_df$group[item_df$group_missing]
    reliability_df$group_label[reliability_df$group_missing] <- reliability_df$group[reliability_df$group_missing]
  }

  template_override <- resolve_template_override(opts$template, module = "scale")
  template_path <- if (!is.null(template_override)) {
    template_override
  } else {
    resolve_template_path("scale.default", "scale/default-template.md")
  }
  template_path <- nlss_freeze_template(template_path, "scale.main")
  template_meta <- get_template_meta(template_path)
  nlss_report_path <- file.path(out_dir, "report_canonical.md")

  omega_statuses <- character(0)
  if (!is.null(reliability_df$omega_status)) {
    omega_statuses <- reliability_df$omega_status
  }

  note_tokens <- build_scale_note_tokens(reverse_info, missing_method, score_method, omega_flag, omega_statuses)
  nlss_text <- format_nlss_text(reliability_df, digits)
  nlss_table <- format_nlss_table(item_df, digits, note_tokens$note_default)
  table_body <- build_scale_table_body(item_df, digits, template_meta$table)
  narrative_rows <- build_scale_narrative_rows(reliability_df, digits)
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

  nlss_stage_report(
    nlss_report_path,
    "Scale analysis",
    nlss_table,
    nlss_text,
    analysis_flags = analysis_flags,
    template_path = template_path,
    template_context = template_context
  )

  results <- list(item_df = item_df, reliability_df = reliability_df, diagnostics_df = diagnostics_df)
  nlss_set_result(results)

  if (parse_bool(opts$log, default = log_default)) {
    ctx <- get_run_context()
    nlss_stage_log(
      out_dir,
      module = "scale",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = results,
      options = options,
      user_prompt = get_user_prompt(opts)
    )
  }
}

nlss_run_main("scale", main)
