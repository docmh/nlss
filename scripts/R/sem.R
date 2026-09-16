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
source_lib("sem_helpers.R")

print_usage <- function() {
  cat("Structural equation modeling (lavaan)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript sem.R --csv data.csv --analysis sem --model \"<lavaan syntax>\"\n")
  cat("  Rscript sem.R --parquet data.parquet --analysis cfa --factors \"F1=item1,item2;F2=item3,item4\"\n")
  cat("  Rscript sem.R --parquet data.parquet --analysis mediation --x x1 --m m1,m2 --y y1\n")
  cat("  Rscript sem.R --parquet data.parquet --analysis path --dv outcome --ivs x1,x2\n")
  cat("  Rscript sem.R --interactive\n")
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
  cat("  --analysis TYPE         sem/cfa/path/mediation/invariance\n")
  cat("  --model TEXT            lavaan model syntax (quotes recommended)\n")
  cat("  --model-file PATH       lavaan model syntax file\n")
  cat("  --paths TEXT            alias for --model (path syntax)\n")
  cat("  --factors TEXT          CFA builder: F1=item1,item2;F2=item3,item4\n")
  cat("  --dv NAME               Path builder dependent variable\n")
  cat("  --ivs LIST              Path builder predictors\n")
  cat("  --x NAME                Mediation predictor\n")
  cat("  --m LIST                Mediation mediators\n")
  cat("  --y NAME                Mediation outcome\n")
  cat("  --covariates LIST       Optional covariates\n")
  cat("  --serial TRUE/FALSE     Serial mediation (supports two mediators)\n")
  cat("  --group NAME            Multi-group analysis variable\n")
  cat("  --group-equal LIST      lavaan group.equal constraints\n")
  cat("  --invariance LIST       Invariance steps (configural,metric,scalar,strict)\n")
  cat("  --ordered LIST          Ordered categorical variables\n")
  cat("  --estimator NAME        ML, MLR, MLM, MLMV, MLMVS, WLSMV, ULSMV, DWLS\n")
  cat("  --missing TYPE          fiml/listwise/pairwise\n")
  cat("  --se TYPE               standard/robust/bootstrap\n")
  cat("  --ci TYPE               standard/bootstrap/bca\n")
  cat("  --conf-level VALUE      Confidence level (default: 0.95)\n")
  cat("  --bootstrap TRUE/FALSE  Bootstrap standard errors\n")
  cat("  --bootstrap-samples N   Bootstrap resamples (default: 5000)\n")
  cat("  --seed N                Reproducible bootstrap seed (canonical default)\n")
  cat("  --std TYPE              none/std.lv/std.all\n")
  cat("  --fit LIST              Fit indices to report\n")
  cat("  --r2 TRUE/FALSE         Report R² (default: TRUE)\n")
  cat("  --modindices N          Modification index cutoff (0 to skip)\n")
  cat("  --residuals TRUE/FALSE  Store standardized residuals in log\n")
  cat("  --digits N              Rounding digits (default: 2)\n")
  cat("  --template REF          Template path or template key (optional)\n")
  cat("  --user-prompt TEXT      Original AI user prompt for logging (optional)\n")
  cat("  --log TRUE/FALSE        Write analysis_log.jsonl (default: TRUE)\n")
  cat("  --interactive           Prompt for inputs\n")
  cat("  --help                  Show this help\n")
}

interactive_options <- function() {
  cat("Interactive input selected.\n")
  input_type <- prompt("Input type (csv/sav/rds/rdata/parquet)", "csv")
  input_type <- tolower(input_type)
  opts <- list()

  if (input_type == "csv") {
    opts[["csv"]] <- prompt("CSV path")
    sep_default <- get_config_value("defaults.csv.sep")
    header_default <- get_config_value("defaults.csv.header")
    opts[["sep"]] <- prompt("Separator", sep_default)
    opts[["header"]] <- prompt("Header TRUE/FALSE", ifelse(isTRUE(header_default), "TRUE", "FALSE"))
  } else if (input_type == "sav") {
    opts[["sav"]] <- prompt("SAV path")
  } else if (input_type == "rds") {
    opts[["rds"]] <- prompt("RDS path")
  } else if (input_type == "rdata") {
    opts[["rdata"]] <- prompt("RData path")
    opts[["df"]] <- prompt("Data frame object name")
  } else if (input_type == "parquet") {
    opts[["parquet"]] <- prompt("Parquet path")
  } else {
    stop("Unsupported input type.")
  }

  analysis_default <- get_config_value("modules.sem.analysis")
  opts[["analysis"]] <- prompt("Analysis (sem/cfa/path/mediation/invariance)", analysis_default)

  if (tolower(opts[["analysis"]]) == "cfa") {
    opts[["factors"]] <- prompt("Factors (F1=item1,item2;F2=item3,item4)", "")
    opts[["model"]] <- prompt("Model syntax (blank to use factors)", "")
  } else if (tolower(opts[["analysis"]]) == "mediation") {
    opts[["x"]] <- prompt("Predictor (x)")
    opts[["m"]] <- prompt("Mediators (comma-separated)")
    opts[["y"]] <- prompt("Outcome (y)")
    opts[["covariates"]] <- prompt("Covariates (comma-separated, optional)", "")
    opts[["serial"]] <- prompt("Serial mediation TRUE/FALSE", ifelse(get_config_value("modules.sem.serial"), "TRUE", "FALSE"))
  } else if (tolower(opts[["analysis"]]) == "path") {
    opts[["dv"]] <- prompt("Dependent variable", "")
    opts[["ivs"]] <- prompt("Predictors (comma-separated)", "")
    opts[["model"]] <- prompt("Model syntax (blank to use dv/ivs)", "")
  } else {
    opts[["model"]] <- prompt("Model syntax", "")
  }

  estimator_default <- get_config_value("modules.sem.estimator")
  missing_default <- get_config_value("modules.sem.missing")
  se_default <- get_config_value("modules.sem.se")
  ci_default <- get_config_value("modules.sem.ci")
  conf_default <- get_config_value("modules.sem.conf_level")
  bootstrap_default <- get_config_value("modules.sem.bootstrap")
  bootstrap_samples_default <- get_config_value("modules.sem.bootstrap_samples")
  std_default <- get_config_value("modules.sem.std")
  fit_default <- get_config_value("modules.sem.fit")
  r2_default <- get_config_value("modules.sem.r2")
  modindices_default <- get_config_value("modules.sem.modindices")
  residuals_default <- get_config_value("modules.sem.residuals")
  digits_default <- get_config_value("defaults.digits")

  opts[["estimator"]] <- prompt("Estimator", estimator_default)
  opts[["missing"]] <- prompt("Missing handling", missing_default)
  opts[["se"]] <- prompt("SE type", se_default)
  opts[["ci"]] <- prompt("CI type", ci_default)
  opts$`conf-level` <- prompt("Confidence level", as.character(conf_default))
  opts[["bootstrap"]] <- prompt("Bootstrap TRUE/FALSE", ifelse(isTRUE(bootstrap_default), "TRUE", "FALSE"))
  opts$`bootstrap-samples` <- prompt("Bootstrap samples", as.character(bootstrap_samples_default))
  opts[["std"]] <- prompt("Standardization (none/std.lv/std.all)", std_default)
  opts[["fit"]] <- prompt("Fit indices", fit_default)
  opts[["r2"]] <- prompt("Report R² TRUE/FALSE", ifelse(isTRUE(r2_default), "TRUE", "FALSE"))
  opts[["modindices"]] <- prompt("Modindices cutoff", as.character(modindices_default))
  opts[["residuals"]] <- prompt("Include residuals TRUE/FALSE", ifelse(isTRUE(residuals_default), "TRUE", "FALSE"))
  opts[["digits"]] <- prompt("Rounding digits", as.character(digits_default))
  opts[["template"]] <- prompt("Template (path or key; blank for default)", "")
  opts$`user-prompt` <- prompt("User prompt (optional)", "")
  log_default <- get_config_value("defaults.log")
  opts[["log"]] <- prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))
  opts
}

normalize_analysis <- function(value, default) {
  val <- if (!is.null(value) && nzchar(value)) tolower(as.character(value)) else tolower(default)
  if (val %in% c("sem", "structural")) return("sem")
  if (val %in% c("cfa", "confirmatory")) return("cfa")
  if (val %in% c("path", "path-analysis", "path_analysis")) return("path")
  if (val %in% c("mediation", "med", "indirect")) return("mediation")
  if (val %in% c("invariance", "measurement-invariance", "mi")) return("invariance")
  stop("Unknown SEM analysis: ", val)
}

normalize_estimator <- function(value, default) {
  val <- if (!is.null(value) && nzchar(value)) as.character(value) else as.character(default)
  val <- toupper(val)
  allowed <- c("ML", "MLR", "MLM", "MLMV", "MLMVS", "WLSMV", "ULSMV", "DWLS", "ULS", "GLS")
  if (val %in% allowed) return(val)
  stop("Unknown SEM estimator: ", val)
}

normalize_missing <- function(value, default) {
  val <- if (!is.null(value) && nzchar(value)) tolower(as.character(value)) else tolower(default)
  if (val %in% c("fiml", "ml")) return("fiml")
  if (val %in% c("listwise", "list")) return("listwise")
  if (val %in% c("pairwise", "pair")) return("pairwise")
  stop("Unknown SEM missing-data method: ", val)
}

normalize_se <- function(value, default) {
  val <- if (!is.null(value) && nzchar(value)) tolower(as.character(value)) else tolower(default)
  if (val %in% c("standard", "none", "default")) return("standard")
  if (val %in% c("robust", "sandwich")) return("robust")
  if (val %in% c("bootstrap", "boot")) return("bootstrap")
  stop("Unknown SEM SE method: ", val)
}

normalize_ci <- function(value, default) {
  val <- if (!is.null(value) && nzchar(value)) tolower(as.character(value)) else tolower(default)
  if (val %in% c("standard", "normal", "none")) return("standard")
  if (val %in% c("bootstrap", "boot", "perc", "percentile")) return("bootstrap")
  if (val %in% c("bca", "bca.simple", "bca_simple")) return("bca")
  stop("Unknown SEM CI method: ", val)
}

normalize_std <- function(value, default) {
  val <- if (!is.null(value) && nzchar(value)) tolower(as.character(value)) else tolower(default)
  if (val %in% c("none", "no", "false")) return("none")
  if (val %in% c("std.lv", "std_lv", "latent")) return("std.lv")
  if (val %in% c("std.all", "std_all", "all")) return("std.all")
  stop("Unknown SEM standardization: ", val)
}

format_stat <- function(value, digits) {
  if (is.na(value)) return("")
  txt <- format(round(value, digits), nsmall = digits, trim = TRUE)
  sub("^(-?)0[.]", "\\1.", txt)
}

format_p <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value < 0.001) return("< .001")
  p_txt <- format(round(p_value, 3), nsmall = 3, trim = TRUE)
  p_txt <- sub("^0", "", p_txt)
  paste("=", p_txt)
}

build_fit_sentence <- function(fit_values, digits) {
  if (length(fit_values) == 0) return("")
  parts <- character(0)
  if (!is.null(fit_values$chisq) && !is.null(fit_values$df)) {
    chisq <- format_stat(fit_values$chisq, digits)
    df <- format_stat(fit_values$df, 0)
    p_val <- if (!is.null(fit_values$pvalue)) format_p(fit_values$pvalue) else ""
    if (nzchar(p_val)) {
      parts <- c(parts, paste0("chi²(", df, ") = ", chisq, ", p ", p_val))
    } else {
      parts <- c(parts, paste0("chi²(", df, ") = ", chisq))
    }
  }
  metrics <- c("cfi", "tli", "rmsea", "srmr", "aic", "bic")
  labels <- c(cfi = "CFI", tli = "TLI", rmsea = "RMSEA", srmr = "SRMR", aic = "AIC", bic = "BIC")
  for (metric in metrics) {
    if (!is.null(fit_values[[metric]])) {
      parts <- c(parts, paste0(labels[[metric]], " = ", format_stat(fit_values[[metric]], digits)))
    }
  }
  if (length(parts) == 0) return("")
  paste0("Model fit: ", paste(parts, collapse = ", "), ".")
}

flatten_r2_values <- function(r2_values) {
  if (is.null(r2_values) || length(r2_values) == 0) return(list())
  if (is.numeric(r2_values)) {
    out <- as.list(r2_values)
    if (is.null(names(out))) names(out) <- rep("", length(out))
    return(out)
  }
  if (is.list(r2_values)) {
    out <- list()
    group_names <- names(r2_values)
    for (i in seq_along(r2_values)) {
      values <- r2_values[[i]]
      if (is.null(values)) next
      group_label <- if (!is.null(group_names) && nzchar(group_names[i])) group_names[i] else paste0("Group ", i)
      if (!is.numeric(values)) next
      val_names <- names(values)
      if (is.null(val_names) || all(!nzchar(val_names))) {
        for (j in seq_along(values)) {
          label <- paste0(group_label, ": var", j)
          out[[label]] <- values[[j]]
        }
      } else {
        for (j in seq_along(values)) {
          label <- paste0(group_label, ": ", val_names[j])
          out[[label]] <- values[[j]]
        }
      }
    }
    return(out)
  }
  list()
}

build_r2_sentence <- function(r2_values, digits) {
  flat <- flatten_r2_values(r2_values)
  if (length(flat) == 0) return("")
  labels <- names(flat)
  parts <- character(0)
  for (i in seq_along(flat)) {
    label <- labels[i]
    value <- flat[[i]]
    if (is.na(value)) next
    if (is.null(label) || !nzchar(label)) {
      parts <- c(parts, format_stat(value, digits))
    } else {
      parts <- c(parts, paste0(label, " = ", format_stat(value, digits)))
    }
  }
  if (length(parts) == 0) return("")
  paste0("R²: ", paste(parts, collapse = "; "), ".")
}

build_r2_df <- function(r2_values) {
  flat <- flatten_r2_values(r2_values)
  if (length(flat) == 0) return(data.frame())
  labels <- names(flat)
  if (is.null(labels)) labels <- rep("", length(flat))
  data.frame(
    label = labels,
    r2 = as.numeric(flat),
    stringsAsFactors = FALSE
  )
}

build_sem_table_body <- function(param_df, digits, table_meta) {
  default_specs <- list(
    list(key = "group", label = "Group", drop_if_empty = TRUE),
    list(key = "path", label = "Path"),
    list(key = "label", label = "Label", drop_if_empty = TRUE),
    list(key = "est", label = "b"),
    list(key = "se", label = "SE"),
    list(key = "z", label = "z", drop_if_empty = TRUE),
    list(key = "p", label = "p"),
    list(key = "ci_low", label = "CI low", drop_if_empty = TRUE),
    list(key = "ci_high", label = "CI high", drop_if_empty = TRUE),
    list(key = "std", label = "Std", drop_if_empty = TRUE)
  )
  columns <- normalize_table_columns(table_meta$columns, default_specs)
  show_group <- any(nzchar(param_df$group), na.rm = TRUE)

  rows <- list()
  for (i in seq_len(nrow(param_df))) {
    row <- param_df[i, ]
    row_map <- list(
      group = if (show_group) row$group else "",
      path = row$path,
      label = row$label,
      est = format_stat(row$est, digits),
      se = format_stat(row$se, digits),
      z = format_stat(row$z, digits),
      p = format_p(row$p),
      ci_low = format_stat(row$ci_low, digits),
      ci_high = format_stat(row$ci_high, digits),
      std = format_stat(row$std, digits)
    )
    row_vals <- vapply(columns, function(col) {
      as_cell_text(row_map[[col$key]])
    }, character(1))
    rows[[length(rows) + 1]] <- row_vals
  }
  drop_result <- drop_empty_columns(columns, rows)
  columns <- drop_result$columns
  rows <- drop_result$rows
  headers <- vapply(columns, function(col) col$label, character(1))
  body <- render_markdown_table(headers, rows)
  list(body = body, columns = columns)
}

build_invariance_table_body <- function(summary_df, digits, table_meta) {
  default_specs <- list(
    list(key = "step", label = "Step"),
    list(key = "group_equal", label = "Constraints", drop_if_empty = TRUE),
    list(key = "chisq", label = "Chi²"),
    list(key = "df", label = "df"),
    list(key = "p", label = "p"),
    list(key = "cfi", label = "CFI"),
    list(key = "tli", label = "TLI"),
    list(key = "rmsea", label = "RMSEA"),
    list(key = "srmr", label = "SRMR"),
    list(key = "delta_cfi", label = "Delta CFI", drop_if_empty = TRUE),
    list(key = "delta_rmsea", label = "Delta RMSEA", drop_if_empty = TRUE)
  )
  columns <- normalize_table_columns(table_meta$columns, default_specs)
  rows <- list()
  for (i in seq_len(nrow(summary_df))) {
    row <- summary_df[i, ]
    row_map <- list(
      step = row$step,
      group_equal = row$group_equal,
      chisq = format_stat(row$chisq, digits),
      df = format_stat(row$df, 0),
      p = format_p(row$p),
      cfi = format_stat(row$cfi, digits),
      tli = format_stat(row$tli, digits),
      rmsea = format_stat(row$rmsea, digits),
      srmr = format_stat(row$srmr, digits),
      delta_cfi = format_stat(row$delta_cfi, digits),
      delta_rmsea = format_stat(row$delta_rmsea, digits)
    )
    row_vals <- vapply(columns, function(col) {
      as_cell_text(row_map[[col$key]])
    }, character(1))
    rows[[length(rows) + 1]] <- row_vals
  }
  drop_result <- drop_empty_columns(columns, rows)
  columns <- drop_result$columns
  rows <- drop_result$rows
  headers <- vapply(columns, function(col) col$label, character(1))
  body <- render_markdown_table(headers, rows)
  list(body = body, columns = columns)
}

build_sem_note_tokens <- function(estimator, missing, se, ci, conf_level, std, n_obs, group_labels) {
  notes <- character(0)
  notes <- c(notes, paste0("Estimator = ", estimator, "."))
  if (nzchar(missing)) notes <- c(notes, paste0("Missing = ", toupper(missing), "."))
  if (nzchar(se)) notes <- c(notes, paste0("SE = ", se, "."))
  if (nzchar(ci)) notes <- c(notes, paste0("CI = ", ci, ", ", round(conf_level * 100), "%."))
  if (nzchar(std) && std != "none") notes <- c(notes, paste0("Standardization = ", std, "."))
  if (!is.null(n_obs) && !all(is.na(n_obs))) {
    n_text <- if (length(n_obs) > 1) paste(n_obs, collapse = ", ") else as.character(n_obs)
    if (nzchar(n_text)) notes <- c(notes, paste0("N = ", n_text, "."))
  }
  if (!is.null(group_labels) && length(group_labels) > 0) {
    notes <- c(notes, paste0("Groups: ", paste(group_labels, collapse = ", "), "."))
  }
  list(note_default = paste(notes, collapse = " "))
}

emit_input_issue <- function(out_dir, opts, message, details = list(), status = "invalid_input") {
  message <- render_paths_for_log(message, workspace_root = nlss_run_context$root)
  nlss_run_context$request$validation_issue <- list(message = message,
    details = render_paths_for_log(details, workspace_root = nlss_run_context$root), status = status)
  stop(message)
}

collect_fit_values <- function(fit, fit_indices) {
  values <- list()
  if (length(fit_indices) == 0) return(values)
  fit_vals <- lavaan::fitMeasures(fit, fit_indices)
  values <- as.list(fit_vals)
  absent <- setdiff(fit_indices, c(names(values), "all", "default"))
  if (length(absent)) stop("Unknown or unavailable requested fit indices: ", paste(absent, collapse = ", "))
  values
}

build_param_df <- function(pe, std, group_labels, primary = TRUE) {
  keep_ops <- if (primary) c("=~", "~", "~~", ":=") else c("~~", "~1", "|", "~*~")
  pe <- pe[pe$op %in% keep_ops, , drop = FALSE]
  if (nrow(pe) == 0) return(data.frame())
  if (primary) pe <- pe[!(pe$op == "~~" & pe$lhs == pe$rhs), , drop = FALSE]
  else pe <- pe[!(pe$op == "~~" & pe$lhs != pe$rhs), , drop = FALSE]

  std_col <- NULL
  if (std == "std.all" && "std.all" %in% names(pe)) std_col <- "std.all"
  if (std == "std.lv" && "std.lv" %in% names(pe)) std_col <- "std.lv"

  group_vals <- rep("", nrow(pe))
  if (!is.null(pe$group) && length(group_labels) > 0) {
    group_vals <- vapply(pe$group, function(idx) {
      if (is.na(idx) || idx < 1L || idx > length(group_labels)) return("")
      label <- group_labels[as.integer(idx)]
      if (is.na(label) || !nzchar(label)) return("")
      as.character(label)
    }, character(1))
  }

  label_vals <- if ("label" %in% names(pe)) {
    ifelse(is.na(pe$label), "", pe$label)
  } else {
    rep("", nrow(pe))
  }

  data.frame(
    group = group_vals,
    path = paste(pe$lhs, pe$op, pe$rhs),
    label = label_vals,
    est = pe$est,
    se = pe$se,
    z = pe$z,
    p = pe$pvalue,
    ci_low = pe$ci.lower,
    ci_high = pe$ci.upper,
    std = if (!is.null(std_col)) pe[[std_col]] else NA_real_,
    op = pe$op,
    estimate_status = ifelse(is.finite(pe$est), "available", "unavailable"),
    inference_status = ifelse(is.finite(pe$se) & is.finite(pe$ci.lower) & is.finite(pe$ci.upper), "available", "unavailable"),
    stringsAsFactors = FALSE
  )
}

build_invariance_summary <- function(steps, fits, fit_indices) {
  rows <- list()
  prev <- NULL
  get_val <- function(values, key) {
    val <- values[[key]]
    if (is.null(val) || length(val) == 0) return(NA_real_)
    as.numeric(val)
  }
  for (i in seq_along(steps)) {
    fit <- fits[[i]]
    fit_vals <- collect_fit_values(fit, fit_indices)
    row <- list(
      step = steps[[i]]$label,
      group_equal = steps[[i]]$constraints,
      chisq = get_val(fit_vals, "chisq"),
      df = get_val(fit_vals, "df"),
      p = get_val(fit_vals, "pvalue"),
      cfi = get_val(fit_vals, "cfi"),
      tli = get_val(fit_vals, "tli"),
      rmsea = get_val(fit_vals, "rmsea"),
      srmr = get_val(fit_vals, "srmr"),
      delta_cfi = NA_real_,
      delta_rmsea = NA_real_
    )
    if (!is.null(prev)) {
      if (!is.null(row$cfi) && !is.null(prev$cfi)) row$delta_cfi <- row$cfi - prev$cfi
      if (!is.null(row$rmsea) && !is.null(prev$rmsea)) row$delta_rmsea <- row$rmsea - prev$rmsea
    }
    prev <- row
    rows[[length(rows) + 1]] <- row
  }
  if (length(rows) == 0) return(data.frame())
  data.frame(
    step = vapply(rows, function(x) x$step, character(1)),
    group_equal = vapply(rows, function(x) x$group_equal, character(1)),
    chisq = vapply(rows, function(x) as.numeric(x$chisq), numeric(1)),
    df = vapply(rows, function(x) as.numeric(x$df), numeric(1)),
    p = vapply(rows, function(x) as.numeric(x$p), numeric(1)),
    cfi = vapply(rows, function(x) as.numeric(x$cfi), numeric(1)),
    tli = vapply(rows, function(x) as.numeric(x$tli), numeric(1)),
    rmsea = vapply(rows, function(x) as.numeric(x$rmsea), numeric(1)),
    srmr = vapply(rows, function(x) as.numeric(x$srmr), numeric(1)),
    delta_cfi = vapply(rows, function(x) as.numeric(x$delta_cfi), numeric(1)),
    delta_rmsea = vapply(rows, function(x) as.numeric(x$delta_rmsea), numeric(1)),
    stringsAsFactors = FALSE
  )
}

collect_sem_output <- function(fit, std, conf_level, ci_type, fit_indices, r2_flag, modindices_cutoff, residuals_flag) {
  ci_method <- switch(ci_type, bootstrap = "perc", bca = "bca.simple", "norm")
  pe <- lavaan::parameterEstimates(fit, standardized = (std != "none"), ci = TRUE,
    level = conf_level, boot.ci.type = ci_method)
  group_labels <- lavaan::lavInspect(fit, "group.label")
  optional <- function(requested, fun) {
    if (!requested) return(list(status = "not_requested", value = NULL))
    tryCatch(list(status = "available", value = fun()), error = function(e) {
      warning("Requested SEM output unavailable: ", conditionMessage(e))
      list(status = "unavailable", message = conditionMessage(e), value = NULL)
    })
  }
  r2 <- optional(r2_flag, function() lavaan::lavInspect(fit, "r2"))
  mi <- optional(modindices_cutoff > 0, function() lavaan::modindices(fit, sort. = TRUE, minimum.value = modindices_cutoff))
  residuals <- optional(residuals_flag, function() lavaan::residuals(fit, type = "standardized"))
  params <- build_param_df(pe, std, group_labels)
  fit_values <- collect_fit_values(fit, fit_indices)
  list(n = as.numeric(lavaan::lavInspect(fit, "nobs")), fit = fit_values,
    parameter_table = pe, parameter_specification = lavaan::parTable(fit),
    supplementary_params_df = build_param_df(pe, std, group_labels, primary = FALSE),
    params_df = params, params = list(rows = nrow(params)),
    r2_df = build_r2_df(r2$value), r2_values = r2$value,
    modindices = if (is.null(mi$value)) 0L else nrow(mi$value), modindices_df = mi$value,
    residuals_output = residuals$value,
    availability = list(r2 = r2[setdiff(names(r2), "value")],
      modindices = mi[setdiff(names(mi), "value")], residuals = residuals[setdiff(names(residuals), "value")],
      fit = lapply(fit_values, function(x) if (is.finite(x)) "available" else "unavailable")))
}

sem_audit_note <- function(audit, ci_type) {
  effective <- audit$inference$effective
  notes <- c(paste0("Effective lavaan estimator = ", effective$estimator,
    " (requested ", audit$inference$requested$estimator, "); SE = ", effective$se,
    "; test = ", paste(effective$test, collapse = ", "), "; missing = ", effective$missing, "."),
    paste0("Fit status: ", audit$fit_status$status, "."))
  if (!audit$fit_status$standard_errors_available) notes <- c(notes, "Some free-parameter standard errors or intervals are unavailable.")
  if (audit$bootstrap$enabled) notes <- c(notes, paste0("Bootstrap: ", audit$bootstrap$successful,
    "/", audit$bootstrap$attempted, " successful; ", audit$bootstrap$failed, " failed; ",
    audit$bootstrap$inadmissible, " nonadmissible draws. Finite nonadmissible draws remain in lavaan inference."),
    switch(ci_type, bca = "CI alias bca uses lavaan bca.simple (bias correction without acceleration), not full BCa.",
      bootstrap = "Percentile bootstrap confidence intervals.",
      "Normal bootstrap intervals use lavaan norm (bootstrap bias correction)."))
  paste(notes, collapse = " ")
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  opts <- nlss_run_options(args, "sem")

  if (!is.null(opts[["help"]])) {
    print_usage()
    quit(status = 0)
  }

  if (!is.null(opts[["interactive"]])) {
    opts <- modifyList(opts, interactive_options())
  }

  digits_default <- get_config_value("defaults.digits")
  log_default <- get_config_value("defaults.log")
  analysis_default <- get_config_value("modules.sem.analysis")
  estimator_default <- get_config_value("modules.sem.estimator")
  missing_default <- get_config_value("modules.sem.missing")
  se_default <- get_config_value("modules.sem.se")
  ci_default <- get_config_value("modules.sem.ci")
  conf_default <- get_config_value("modules.sem.conf_level")
  bootstrap_default <- get_config_value("modules.sem.bootstrap")
  bootstrap_samples_default <- get_config_value("modules.sem.bootstrap_samples")
  std_default <- get_config_value("modules.sem.std")
  fit_default <- get_config_value("modules.sem.fit")
  r2_default <- get_config_value("modules.sem.r2")
  modindices_default <- get_config_value("modules.sem.modindices")
  residuals_default <- get_config_value("modules.sem.residuals")
  invariance_default <- get_config_value("modules.sem.invariance")

  digits <- if (!is.null(opts[["digits"]])) as.numeric(opts[["digits"]]) else digits_default
  analysis <- normalize_analysis(opts[["analysis"]], analysis_default)
  estimator <- normalize_estimator(opts[["estimator"]], estimator_default)
  missing <- normalize_missing(opts[["missing"]], missing_default)
  bootstrap <- parse_bool(opts[["bootstrap"]], default = bootstrap_default)
  se <- normalize_se(opts[["se"]], se_default)
  if (bootstrap && se != "bootstrap") se <- "bootstrap"
  if (se == "bootstrap" && !bootstrap) bootstrap <- TRUE
  ci_type <- normalize_ci(opts[["ci"]], ci_default)
  conf_level <- if (!is.null(opts$`conf-level`)) as.numeric(opts$`conf-level`) else conf_default
  if (!is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) stop("Confidence level must be between 0 and 1.")
  bootstrap_samples <- if (!is.null(opts$`bootstrap-samples`)) as.numeric(opts$`bootstrap-samples`) else bootstrap_samples_default
  if (!is.finite(bootstrap_samples) || bootstrap_samples < 2 || bootstrap_samples != floor(bootstrap_samples) || bootstrap_samples > .Machine$integer.max) stop("Bootstrap samples must be an integer from 2 to R's integer limit.")
  if (!is.finite(digits) || digits < 0 || digits > 15 || digits != floor(digits)) stop("Digits must be an integer from 0 to 15.")
  if (ci_type != "standard" && !bootstrap) stop("Bootstrap/bca intervals require --bootstrap TRUE or --se bootstrap; no analytic fallback is permitted.")
  std <- normalize_std(opts[["std"]], std_default)
  fit_indices <- parse_list(if (!is.null(opts[["fit"]])) opts[["fit"]] else fit_default)
  if (length(fit_indices) > 0 && "chisq" %in% fit_indices) {
    if (!("df" %in% fit_indices)) fit_indices <- c(fit_indices, "df")
    if (!("pvalue" %in% fit_indices)) fit_indices <- c(fit_indices, "pvalue")
  }
  if (analysis == "invariance") {
    needed <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr")
    fit_indices <- unique(c(fit_indices, needed))
  }
  r2_flag <- parse_bool(opts[["r2"]], default = r2_default)
  modindices_cutoff <- if (!is.null(opts[["modindices"]])) as.numeric(opts[["modindices"]]) else modindices_default
  if (!is.finite(modindices_cutoff) || modindices_cutoff < 0) stop("Modification-index cutoff must be finite and non-negative.")
  residuals_flag <- parse_bool(opts[["residuals"]], default = residuals_default)

  df <- nlss_load_input(opts)
  out_dir <- get_workspace_out_dir(df)
  nlss_begin_run("sem", df, opts, out_dir)
  source_n <- nrow(df)
  source_classes <- lapply(df, class)
  seed <- nlss_run_seed(opts[["seed"]], stochastic = bootstrap)
  template_override <- resolve_template_override(opts[["template"]], module = "sem")

  if (!requireNamespace("lavaan", quietly = TRUE)) {
    emit_input_issue(out_dir, opts, "SEM requires the 'lavaan' package.", details = list(package = "lavaan"), status = "missing_dependency")
  }

  ordered_vars <- parse_list(opts[["ordered"]])
  ordered_vars <- ordered_vars[nzchar(ordered_vars)]
  if (length(ordered_vars) > 0) {
    missing_ordered <- setdiff(ordered_vars, names(df))
    if (length(missing_ordered) > 0) {
      emit_input_issue(out_dir, opts, paste0("Unknown ordered variables: ", paste(missing_ordered, collapse = ", ")))
    }
  }

  group_var <- ""
  if (!is.null(opts[["group"]]) && nzchar(opts[["group"]])) {
    group_var <- as.character(opts[["group"]])
    if (!group_var %in% names(df)) {
      emit_input_issue(out_dir, opts, paste0("Grouping variable not found: ", group_var))
    }
  }

  group_equal <- parse_list(opts$`group-equal`)
  if (length(group_equal) && !nzchar(group_var)) emit_input_issue(out_dir, opts, "Group-equality constraints require --group.")

  model_text <- ""
  if (!is.null(nlss_run_context$replay)) {
    model_text <- nlss_run_context$replay$request$design$model_syntax
  } else if (!is.null(opts[["model-file"]]) && nzchar(opts[["model-file"]])) {
    model_path <- normalize_input_path(opts[["model-file"]])
    if (!is_absolute_path(model_path)) model_path <- file.path(getwd(), model_path)
    model_path <- normalize_path(model_path)
    model_path <- file.path(normalize_path(dirname(model_path)), basename(model_path))
    # A missing file can still have a canonical parent. If even that parent is
    # unresolved, fail closed to a basename rather than expose ../ paths.
    model_location <- if (dir.exists(dirname(model_path))) render_log_path(model_path,
      workspace_root = nlss_run_context$root) else paste0("<external>/", basename(model_path))
    nlss_run_context$request$cli[["model-file"]] <- model_location
    if (!file.exists(model_path)) {
      emit_input_issue(out_dir, opts, paste0("Model file not found: ",
        model_location))
    }
    model_text <- paste(readLines(model_path, warn = FALSE), collapse = "\n")
  }
  if (!nzchar(model_text) && !is.null(opts[["model"]]) && nzchar(opts[["model"]])) {
    model_text <- as.character(opts[["model"]])
  }
  if (!nzchar(model_text) && !is.null(opts[["paths"]]) && nzchar(opts[["paths"]])) {
    model_text <- as.character(opts[["paths"]])
  }

  factors_text <- if (!is.null(opts[["factors"]])) as.character(opts[["factors"]]) else ""
  dv <- if (!is.null(opts[["dv"]])) as.character(opts[["dv"]]) else ""
  ivs <- parse_list(opts[["ivs"]])
  ivs <- ivs[nzchar(ivs)]
  covariates <- parse_list(opts[["covariates"]])
  covariates <- covariates[nzchar(covariates)]
  x <- if (!is.null(opts[["x"]])) as.character(opts[["x"]]) else ""
  mediators <- parse_list(opts[["m"]])
  mediators <- mediators[nzchar(mediators)]
  y <- if (!is.null(opts[["y"]])) as.character(opts[["y"]]) else ""
  serial <- parse_bool(opts[["serial"]], default = get_config_value("modules.sem.serial"))

  model_syntax <- ""
  if (nzchar(model_text)) {
    model_syntax <- normalize_model_syntax(model_text)
  } else if (analysis %in% c("cfa", "invariance") && nzchar(factors_text)) {
    factors <- tryCatch(parse_factor_spec(factors_text), error = function(e) {
      emit_input_issue(out_dir, opts, e$message)
    })
    model_syntax <- build_cfa_model(factors)
  } else if (analysis == "mediation") {
    model_syntax <- tryCatch(build_mediation_model(x, mediators, y, covariates, serial), error = function(e) {
      emit_input_issue(out_dir, opts, e$message)
    })
  } else if (analysis %in% c("path", "sem") && nzchar(dv) && length(ivs) > 0) {
    model_syntax <- build_path_model(dv, ivs, covariates)
  }

  if (!nzchar(model_syntax)) {
    emit_input_issue(out_dir, opts, "Model syntax is required. Use --model, --model-file, or a builder option.")
  }

  model_vars <- extract_model_vars(model_syntax)
  missing_vars <- setdiff(model_vars, names(df))
  if (length(missing_vars) > 0) {
    emit_input_issue(out_dir, opts, paste0("Missing variables: ", paste(missing_vars, collapse = ", ")))
  }
  if (length(setdiff(ordered_vars, model_vars))) emit_input_issue(out_dir, opts, "Ordered variables must occur in the model.")
  for (var in unique(c(model_vars, group_var[nzchar(group_var)]))) {
    if (is.numeric(df[[var]]) && any(is.infinite(df[[var]]))) {
      emit_input_issue(out_dir, opts, paste0("Selected SEM variable contains Inf/-Inf: ", var))
    }
  }
  for (var in ordered_vars) df[[var]] <- sem_ordered_factor(df[[var]])
  # Existing ordered factors keep their declared order. Numeric SPSS labels are
  # metadata, not factor levels; explicit ordered roles use the underlying codes.
  ordered_vars <- unique(c(ordered_vars, model_vars[vapply(df[model_vars], is.ordered, logical(1))]))
  if (nzchar(group_var)) {
    if (group_var %in% model_vars) emit_input_issue(out_dir, opts, "A multi-group variable cannot also be an observed SEM variable.")
    df[[group_var]] <- sem_group_factor(df[[group_var]])
    if (nlevels(droplevels(df[[group_var]])) < 2L) emit_input_issue(out_dir, opts, "Multi-group SEM requires at least two observed groups.")
  }
  requested <- list(estimator = estimator, missing = missing,
    se = normalize_se(opts[["se"]], se_default), ci = ci_type, bootstrap = parse_bool(opts[["bootstrap"]], bootstrap_default),
    bootstrap_samples = bootstrap_samples, conf_level = conf_level)
  resolved_options <- list(analysis = analysis, estimator = estimator, missing = missing,
    se = se, ci = ci_type, conf_level = conf_level, bootstrap = bootstrap,
    bootstrap_samples = bootstrap_samples, seed = seed, std = std, fit = fit_indices,
    ordered = ordered_vars, group = group_var, group_equal = group_equal,
    r2 = r2_flag, modindices_cutoff = modindices_cutoff, residuals = residuals_flag, digits = digits)
  design <- list(model_syntax = model_syntax, model_variables = model_vars,
    source_n = source_n, source_variable_classes = source_classes[unique(c(model_vars, group_var[nzchar(group_var)]))],
    variable_classes = lapply(df[model_vars], class), group_variable = group_var,
    ordered_variables = ordered_vars, category_levels = lapply(df[ordered_vars], levels))
  nlss_resolve_request(resolved_options, design)
  audit_fit <- function(fit) {
    audit <- sem_fit_audit(fit, requested, source_n)
    if (!audit$fit_status$converged) emit_input_issue(out_dir, opts, "SEM did not converge; estimates are not published as a completed analysis.", audit, "fit_failed")
    if (!isTRUE(audit$fit_status$admissible)) warning("SEM solution is inadmissible or its admissibility is unavailable; do not interpret it as a validated model.")
    if (!audit$fit_status$standard_errors_available) warning("SEM standard errors are unavailable for one or more free parameters.")
    audit
  }

  analysis_label <- switch(
    analysis,
    cfa = "SEM (CFA)",
    mediation = "SEM (Mediation)",
    path = "SEM (Path analysis)",
    invariance = "SEM (Invariance)",
    "SEM"
  )

  if (analysis == "invariance") {
    if (!nzchar(group_var)) {
      emit_input_issue(out_dir, opts, "Invariance analysis requires --group.")
    }
    invariance_steps <- parse_list(opts[["invariance"]])
    if (length(invariance_steps) == 0) {
      if (length(group_equal) > 0) {
        invariance_steps <- "custom"
      } else {
        invariance_steps <- parse_list(invariance_default)
      }
    }
    if (length(invariance_steps) == 0) {
      emit_input_issue(out_dir, opts, "Invariance analysis requires --invariance steps or --group-equal.")
    }

    step_defs <- list()
    for (step in invariance_steps) {
      label <- tolower(step)
      constraints <- character(0)
      if (label %in% c("configural", "none")) {
        constraints <- character(0)
      } else if (label %in% c("metric", "loadings")) {
        constraints <- c("loadings")
      } else if (label %in% c("scalar", "intercepts")) {
        if (length(ordered_vars)) emit_input_issue(out_dir, opts, "Automatic scalar/strict steps use continuous-indicator intercept constraints. For ordinal indicators specify theoretically justified custom --group-equal constraints (including thresholds where appropriate); no automatic ordinal invariance claim is made.")
        constraints <- c("loadings", "intercepts")
      } else if (label %in% c("strict", "residuals")) {
        if (length(ordered_vars)) emit_input_issue(out_dir, opts, "Automatic scalar/strict steps are continuous-indicator constraints; use explicit custom constraints for ordinal invariance.")
        constraints <- c("loadings", "intercepts", "residuals")
      } else if (label == "custom" && length(group_equal) > 0) {
        constraints <- group_equal
      } else {
        emit_input_issue(out_dir, opts, paste0("Unknown invariance step: ", step))
      }
      step_defs[[length(step_defs) + 1]] <- list(label = step, constraints = paste(constraints, collapse = ", "), group_equal = constraints)
    }

    fits <- list()
    step_results <- list()
    for (step in step_defs) {
      fit <- tryCatch(
        fit_sem_model("cfa", model_syntax, df, estimator, missing, se, bootstrap_samples, ordered_vars, group_var, step$group_equal),
        error = function(e) {
          emit_input_issue(out_dir, opts, paste0("Model fit failed: ", e$message), status = "fit_failed")
        }
      )
      audit <- audit_fit(fit)
      output <- collect_sem_output(fit, std, conf_level, ci_type, fit_indices, r2_flag, modindices_cutoff, residuals_flag)
      step_results[[length(step_results) + 1L]] <- c(list(step = step$label, constraints = step$group_equal), output, audit)
      fits[[length(fits) + 1]] <- fit
    }

    summary_df <- build_invariance_summary(step_defs, fits, fit_indices)
    if (nrow(summary_df) == 0) {
      emit_input_issue(out_dir, opts, "No invariance results could be computed.", status = "fit_failed")
    }

    group_labels <- lavaan::lavInspect(fits[[1]], "group.label")
    n_obs <- step_results[[1]]$n
    n_obs_label <- if (length(n_obs) > 1) paste(n_obs, collapse = ", ") else n_obs
    note_tokens <- build_sem_note_tokens(estimator, missing, se, ci_type, conf_level, std, n_obs, group_labels)
    note_tokens$note_default <- paste(note_tokens$note_default,
      paste(vapply(step_results, function(x) paste0(x$step, ": ", sem_audit_note(x, ci_type)), character(1)), collapse = " "),
      "Fit-index deltas compare adjacent requested steps; they are not a significance test or an automatic finding of invariance.")
    if (length(nlss_run_context$warnings)) note_tokens$note_default <- paste(note_tokens$note_default,
      paste(unique(vapply(nlss_run_context$warnings, `[[`, character(1), "message")), collapse = " "))
    design$group_levels <- group_labels
    design$steps <- lapply(step_results, function(x) x[c("step", "constraints", "fit_status", "inference", "bootstrap", "case_selection")])
    resolved_options$invariance <- invariance_steps
    nlss_resolve_request(resolved_options, design)
    fit_sentence <- "Measurement invariance fit indices are summarized in Table 1."
    token_meta <- list(
      estimator = estimator,
      missing = missing,
      se = se,
      ci = ci_type,
      conf_level = conf_level,
      std = std,
      n_obs = ifelse(is.na(n_obs_label), "", n_obs_label),
      group_labels = if (length(group_labels) > 0) paste(group_labels, collapse = ", ") else ""
    )

    template_path <- if (!is.null(template_override)) {
      template_override
    } else {
      resolve_template_path("sem.invariance", "sem/invariance-template.md")
    }
    template_path <- nlss_freeze_template(template_path, "invariance")
    template_meta <- get_template_meta(template_path)
    table_result <- build_invariance_table_body(summary_df, digits, template_meta$table)
    nlss_table <- paste0("Table 1\n\n", table_result$body, "\n", note_tokens$note_default)

    template_context <- list(
      tokens = c(
        list(
          table_body = table_result$body,
          narrative_default = fit_sentence
        ),
        token_meta,
        note_tokens
      )
    )

    nlss_report_path <- file.path(out_dir, "report_canonical.md")
    nlss_stage_report(
      nlss_report_path,
      analysis_label,
      nlss_table,
      fit_sentence,
      analysis_flags = list(
        analysis = analysis,
        group = group_var,
        invariance = invariance_steps,
        estimator = estimator,
        missing = missing,
        se = se,
        ci = ci_type,
        "conf-level" = conf_level,
        std = std,
        fit = fit_indices,
        digits = digits
      ),
      template_path = template_path,
      template_context = template_context
    )

    cat("Wrote:\n")
    cat("- ", render_output_path(nlss_report_path, out_dir), "\n", sep = "")

    results <- list(status = "ok", analysis = analysis, n = n_obs, fit = summary_df,
      steps = step_results, fit_status = lapply(step_results, `[[`, "fit_status"),
      inference = lapply(step_results, `[[`, "inference"), bootstrap = lapply(step_results, `[[`, "bootstrap"))
    nlss_set_result(results)
    if (parse_bool(opts[["log"]], default = log_default)) {
      ctx <- get_run_context()
      nlss_stage_log(
        out_dir,
        module = "sem",
        prompt = ctx$prompt,
        commands = ctx$commands,
        results = results,
        options = list(
          analysis = analysis,
          estimator = estimator,
          missing = missing,
          se = se,
          ci = ci_type,
          conf_level = conf_level,
          std = std,
          fit = fit_indices,
          group = group_var,
          invariance = invariance_steps,
          model = model_syntax
        ),
        user_prompt = get_user_prompt(opts)
      )
    }
    return(invisible(NULL))
  }

  fit <- tryCatch(
    fit_sem_model(analysis, model_syntax, df, estimator, missing, se, bootstrap_samples, ordered_vars, group_var, group_equal),
    error = function(e) {
      emit_input_issue(out_dir, opts, paste0("Model fit failed: ", e$message), status = "fit_failed")
    }
  )

  audit <- audit_fit(fit)
  group_labels <- lavaan::lavInspect(fit, "group.label")
  output <- collect_sem_output(fit, std, conf_level, ci_type, fit_indices, r2_flag, modindices_cutoff, residuals_flag)
  fit_values <- output$fit
  param_df <- output$params_df
  if (nrow(output$parameter_table) == 0) {
    emit_input_issue(out_dir, opts, "No parameter estimates could be computed.", status = "fit_failed")
  }

  r2_values <- output$r2_values
  r2_df <- output$r2_df
  design <- c(design, list(group_levels = group_labels), audit)
  nlss_resolve_request(resolved_options, design)

  n_obs <- output$n
  n_obs_label <- if (length(n_obs) > 1) paste(n_obs, collapse = ", ") else n_obs
  note_tokens <- build_sem_note_tokens(estimator, missing, se, ci_type, conf_level, std, n_obs, group_labels)
  note_tokens$note_default <- paste(note_tokens$note_default, sem_audit_note(audit, ci_type))
  if (length(nlss_run_context$warnings)) note_tokens$note_default <- paste(note_tokens$note_default,
    paste(unique(vapply(nlss_run_context$warnings, `[[`, character(1), "message")), collapse = " "))
  if (any(param_df$estimate_status == "unavailable" | param_df$inference_status == "unavailable")) {
    note_tokens$note_default <- paste(note_tokens$note_default, "Some parameter estimates or inference are unavailable; empty cells are not zero effects.")
  }
  token_meta <- list(
    estimator = estimator,
    missing = missing,
    se = se,
    ci = ci_type,
    conf_level = conf_level,
    std = std,
    n_obs = ifelse(is.na(n_obs_label), "", n_obs_label),
    group_labels = if (length(group_labels) > 0) paste(group_labels, collapse = ", ") else ""
  )

  fit_sentence <- build_fit_sentence(fit_values, digits)
  r2_sentence <- build_r2_sentence(r2_values, digits)

  indirect_rows <- param_df[param_df$op == ":=", , drop = FALSE]
  indirect_sentences <- character(0)
  if (nrow(indirect_rows) > 0) {
    for (i in seq_len(nrow(indirect_rows))) {
      row <- indirect_rows[i, ]
      label <- row$label
      if (!nzchar(label)) label <- row$path
      sentence <- paste0(
        label,
        ": b = ",
        format_stat(row$est, digits),
        ", SE = ",
        format_stat(row$se, digits),
        ifelse(is.na(row$p), "", paste0(", p ", format_p(row$p))),
        ifelse(is.na(row$ci_low) || is.na(row$ci_high), "", paste0(", ", round(conf_level * 100), "% CI [", format_stat(row$ci_low, digits), ", ", format_stat(row$ci_high, digits), "]")),
        "."
      )
      indirect_sentences <- c(indirect_sentences, sentence)
    }
  }

  narrative_lines <- c(fit_sentence, r2_sentence, indirect_sentences)
  narrative_lines <- narrative_lines[nzchar(narrative_lines)]
  nlss_text <- paste(narrative_lines, collapse = "\n")

  template_key <- switch(
    analysis,
    cfa = "sem.cfa",
    mediation = "sem.mediation",
    path = "sem.default",
    "sem.default"
  )
  template_default <- switch(
    analysis,
    cfa = "sem/cfa-template.md",
    mediation = "sem/mediation-template.md",
    "sem/default-template.md"
  )
  template_path <- if (!is.null(template_override)) {
    template_override
  } else {
    resolve_template_path(template_key, template_default)
  }
  template_path <- nlss_freeze_template(template_path, "parameters")
  template_meta <- get_template_meta(template_path)
  table_result <- build_sem_table_body(param_df, digits, template_meta$table)
  nlss_table <- paste0("Table 1\n\n", table_result$body, "\n", note_tokens$note_default)

  template_context <- list(
    tokens = c(
      list(
        table_body = table_result$body,
        narrative_default = nlss_text,
        fit_summary = fit_sentence,
        r2_summary = r2_sentence,
        indirect_summary = paste(indirect_sentences, collapse = "\n")
      ),
      token_meta,
      note_tokens
    )
  )

  nlss_report_path <- file.path(out_dir, "report_canonical.md")
  nlss_stage_report(
    nlss_report_path,
    analysis_label,
    nlss_table,
    nlss_text,
    analysis_flags = list(
      analysis = analysis,
      dv = if (analysis %in% c("path", "sem") && nzchar(dv)) dv else NULL,
      ivs = if (analysis %in% c("path", "sem") && length(ivs) > 0) ivs else NULL,
      x = if (analysis == "mediation" && nzchar(x)) x else NULL,
      m = if (analysis == "mediation" && length(mediators) > 0) mediators else NULL,
      y = if (analysis == "mediation" && nzchar(y)) y else NULL,
      covariates = if (analysis == "mediation" && length(covariates) > 0) covariates else NULL,
      serial = if (analysis == "mediation" && isTRUE(serial)) TRUE else NULL,
      estimator = estimator,
      missing = missing,
      se = se,
      ci = ci_type,
      "conf-level" = conf_level,
      std = std,
      ordered = if (length(ordered_vars) > 0) ordered_vars else NULL,
      group = if (nzchar(group_var)) group_var else NULL,
      "group-equal" = if (length(group_equal) > 0) group_equal else NULL,
      fit = if (length(fit_indices) > 0) fit_indices else NULL,
      bootstrap = bootstrap,
      "bootstrap-samples" = if (bootstrap) bootstrap_samples else NULL,
      digits = digits
    ),
    template_path = template_path,
    template_context = template_context
  )

  if (nrow(output$supplementary_params_df)) {
    supplementary <- build_sem_table_body(output$supplementary_params_df, digits, template_meta$table)
    nlss_stage_report(nlss_report_path, "SEM (Variances, intercepts and thresholds)",
      paste0("Table 1\n\n", supplementary$body),
      "These parameters belong to the same fitted model; unavailable cells are not zero estimates.",
      template_path = template_path, template_context = list(tokens = list(
        table_body = supplementary$body, note_default = note_tokens$note_default,
        narrative_default = "Additional parameters of the same fitted model.")))
  }

  cat("Wrote:\n")
  cat("- ", render_output_path(nlss_report_path, out_dir), "\n", sep = "")

  modindices_df <- output$modindices_df
  residuals_info <- output$residuals_output
  results <- c(list(status = "ok", analysis = analysis), output, audit)
  nlss_set_result(results)

  if (parse_bool(opts[["log"]], default = log_default)) {
    ctx <- get_run_context()
    nlss_stage_log(
      out_dir,
      module = "sem",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = results,
      options = list(
        analysis = analysis,
        estimator = estimator,
        missing = missing,
        se = se,
        ci = ci_type,
        conf_level = conf_level,
        std = std,
        fit = fit_indices,
        ordered = ordered_vars,
        group = group_var,
        group_equal = group_equal,
        bootstrap = bootstrap,
        bootstrap_samples = bootstrap_samples,
        seed = seed,
        model = model_syntax,
        modindices_cutoff = modindices_cutoff,
        residuals = residuals_flag,
        modindices = modindices_df,
        residuals_output = residuals_info
      ),
      user_prompt = get_user_prompt(opts)
    )
  }
}

nlss_run_main("sem", main)
