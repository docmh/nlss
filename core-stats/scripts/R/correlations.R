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
  cat("Correlations (base R)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript correlations.R --csv data.csv [--vars var1,var2] [--group group_var]\n")
  cat("  Rscript correlations.R --csv data.csv --x var1,var2 --y var3,var4 [--group group_var]\n")
  cat("  Rscript correlations.R --sav data.sav [--vars var1,var2] [--group group_var]\n")
  cat("  Rscript correlations.R --rds data.rds [--vars var1,var2]\n")
  cat("  Rscript correlations.R --rdata data.RData --df data_frame_name [--vars var1,var2]\n")
  cat("  Rscript correlations.R --parquet data.parquet [--vars var1,var2]\n")
  cat("  Rscript correlations.R --interactive\n")
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
  cat("  --vars LIST            Comma-separated variables for a full matrix (default: all numeric)\n")
  cat("  --x LIST               Comma-separated X variables (for cross-correlation)\n")
  cat("  --y LIST               Comma-separated Y variables (for cross-correlation)\n")
  cat("  --group NAME           Grouping variable name (optional)\n")
  cat("  --method TYPE          pearson/spearman/kendall (default: pearson)\n")
  cat("  --missing TYPE         pairwise/complete (default: pairwise)\n")
  cat("  --alternative TYPE     two.sided/greater/less (default: two.sided)\n")
  cat("  --controls LIST        Comma-separated control variables for partial correlations\n")
  cat("  --p-adjust TYPE        none/bonferroni/holm/hochberg/hommel/BH/BY/fdr (default: none)\n")
  cat("  --conf-level VALUE     Confidence level for Fisher CI (default: 0.95)\n")
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
    sep_default <- resolve_config_value("defaults.csv.sep", ",")
    header_default <- resolve_config_value("defaults.csv.header", TRUE)
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

  opts$vars <- prompt("Variables (comma-separated, blank for all numeric)", "")
  opts$x <- prompt("X variables (comma-separated, blank for none)", "")
  opts$y <- prompt("Y variables (comma-separated, blank for none)", "")
  opts$group <- prompt("Grouping variable (blank for none)", "")
  method_default <- resolve_config_value("modules.correlations.method", "pearson")
  missing_default <- resolve_config_value("modules.correlations.missing", "pairwise")
  alternative_default <- resolve_config_value("modules.correlations.alternative", "two.sided")
  opts$method <- prompt("Method (pearson/spearman/kendall)", method_default)
  opts$missing <- prompt("Missing handling (pairwise/complete)", missing_default)
  opts$alternative <- prompt("Alternative (two.sided/greater/less)", alternative_default)
  opts$controls <- prompt("Control variables (comma-separated, blank for none)", "")
  adjust_default <- resolve_config_value("modules.correlations.p_adjust", "none")
  conf_default <- resolve_config_value("modules.correlations.conf_level", 0.95)
  coerce_default <- resolve_config_value("modules.correlations.coerce", FALSE)
  digits_default <- resolve_config_value("defaults.digits", 2)
  opts$`p-adjust` <- prompt(
    "P-value adjustment (none/bonferroni/holm/hochberg/hommel/BH/BY/fdr)",
    adjust_default
  )
  opts$`conf-level` <- prompt("Confidence level", as.character(conf_default))
  opts$coerce <- prompt("Coerce non-numeric TRUE/FALSE", ifelse(isTRUE(coerce_default), "TRUE", "FALSE"))
  opts$digits <- prompt("Rounding digits", as.character(digits_default))
  opts$template <- prompt("Template (path or key; blank for default)", "")
  opts$`user-prompt` <- prompt("User prompt (optional)", "")
  log_default <- resolve_config_value("defaults.log", TRUE)
  opts$log <- prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))
  opts
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

resolve_parse_bool <- function(value, default = FALSE) {
  if (exists("parse_bool", mode = "function")) {
    return(get("parse_bool", mode = "function")(value, default = default))
  }
  if (is.null(value)) return(default)
  if (is.logical(value)) return(value)
  val <- tolower(as.character(value))
  val %in% c("true", "t", "1", "yes", "y")
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

resolve_get_assets_dir <- function() {
  if (exists("get_assets_dir", mode = "function")) {
    return(get("get_assets_dir", mode = "function")())
  }
  if (exists("bootstrap_dir", inherits = TRUE)) {
    return(file.path(get("bootstrap_dir", inherits = TRUE), "..", "..", "assets"))
  }
  file.path(getwd(), "core-stats", "assets")
}

resolve_get_template_path <- function(key, default_relative = NULL) {
  if (exists("resolve_template_path", mode = "function")) {
    return(get("resolve_template_path", mode = "function")(key, default_relative))
  }
  if (is.null(default_relative) || !nzchar(default_relative)) return(NULL)
  file.path(resolve_get_assets_dir(), default_relative)
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

resolve_render_template_tokens <- function(text, tokens) {
  if (exists("render_template_tokens", mode = "function")) {
    return(get("render_template_tokens", mode = "function")(text, tokens))
  }
  text
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


normalize_method <- function(value, default = "pearson") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (val %in% c("pearson", "r", "pearsonr")) return("pearson")
  if (val %in% c("spearman", "rho")) return("spearman")
  if (val %in% c("kendall", "tau")) return("kendall")
  default
}

normalize_missing <- function(value, default = "pairwise") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (val %in% c("pairwise", "pair")) return("pairwise")
  if (val %in% c("complete", "listwise")) return("complete")
  default
}

normalize_alternative <- function(value, default = "two.sided") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (val %in% c("two.sided", "two-sided", "two")) return("two.sided")
  if (val %in% c("greater", "less")) return(val)
  default
}

normalize_adjust <- function(value, default = "none") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (val %in% c("none", "no", "false")) return("none")
  if (val %in% c("bonferroni", "holm", "hochberg", "hommel")) return(val)
  if (val %in% c("bh", "fdr")) return("BH")
  if (val %in% c("by")) return("BY")
  default
}

adjust_label <- function(method) {
  if (method == "BH") return("FDR (BH)")
  if (method == "BY") return("FDR (BY)")
  method
}

select_numeric_vars <- function(df, group_var = NULL) {
  available <- names(df)
  numeric_cols <- available[sapply(df, is.numeric)]
  if (!is.null(group_var)) numeric_cols <- setdiff(numeric_cols, group_var)
  numeric_cols
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

calc_skewness <- function(x) {
  n <- length(x)
  if (n < 3) return(NA_real_)
  mean_x <- mean(x)
  sd_x <- sd(x)
  if (is.na(sd_x) || sd_x == 0) return(NA_real_)
  sum(((x - mean_x) / sd_x)^3) * (n / ((n - 1) * (n - 2)))
}

calc_kurtosis <- function(x) {
  n <- length(x)
  if (n < 4) return(NA_real_)
  mean_x <- mean(x)
  sd_x <- sd(x)
  if (is.na(sd_x) || sd_x == 0) return(NA_real_)
  term1 <- (n * (n + 1)) / ((n - 1) * (n - 2) * (n - 3))
  term2 <- (3 * (n - 1)^2) / ((n - 2) * (n - 3))
  term1 * sum(((x - mean_x) / sd_x)^4) - term2
}

calc_ci <- function(r, n, conf_level) {
  if (is.na(r) || n < 4 || abs(r) >= 1) return(c(NA_real_, NA_real_))
  z <- atanh(r)
  se <- 1 / sqrt(n - 3)
  z_crit <- qnorm((1 + conf_level) / 2)
  ci <- tanh(z + c(-1, 1) * z_crit * se)
  c(ci[1], ci[2])
}

get_complete_rows <- function(df) {
  if (nrow(df) == 0) return(logical(0))
  idx <- complete.cases(df)
  if (any(idx)) {
    finite_idx <- rep(TRUE, nrow(df))
    for (col in names(df)) {
      finite_idx <- finite_idx & is.finite(df[[col]])
    }
    idx <- idx & finite_idx
  }
  idx
}

build_pairs <- function(vars) {
  if (length(vars) < 2) {
    return(data.frame(var1 = character(0), var2 = character(0), stringsAsFactors = FALSE))
  }
  combo <- combn(vars, 2)
  data.frame(var1 = combo[1, ], var2 = combo[2, ], stringsAsFactors = FALSE)
}

build_cross_pairs <- function(x_vars, y_vars) {
  if (length(x_vars) == 0 || length(y_vars) == 0) {
    return(build_pairs(unique(c(x_vars, y_vars))))
  }
  grid <- expand.grid(var1 = x_vars, var2 = y_vars, stringsAsFactors = FALSE)
  grid <- grid[grid$var1 != grid$var2, , drop = FALSE]
  if (nrow(grid) == 0) return(grid)
  key <- paste(pmin(grid$var1, grid$var2), pmax(grid$var1, grid$var2), sep = "||")
  grid <- grid[!duplicated(key), , drop = FALSE]
  grid
}

prepare_partial <- function(df_use, var1, var2, controls, method) {
  data <- df_use[, c(var1, var2, controls), drop = FALSE]
  if (method != "pearson") {
    data <- as.data.frame(lapply(data, function(col) rank(col, ties.method = "average")))
  }
  x <- data[[var1]]
  y <- data[[var2]]
  controls_df <- data[, controls, drop = FALSE]
  if (ncol(controls_df) == 0) return(list(x = x, y = y))

  x_res <- tryCatch(
    resid(lm(x ~ ., data = data.frame(x = x, controls_df))),
    error = function(e) NULL
  )
  y_res <- tryCatch(
    resid(lm(y ~ ., data = data.frame(y = y, controls_df))),
    error = function(e) NULL
  )
  if (is.null(x_res) || is.null(y_res)) {
    return(list(x = rep(NA_real_, length(x)), y = rep(NA_real_, length(y))))
  }
  list(x = x_res, y = y_res)
}

safe_cor_test <- function(x, y, method, alternative, conf_level) {
  test <- tryCatch(
    suppressWarnings(cor.test(x, y, method = method, alternative = alternative, conf.level = conf_level)),
    error = function(e) NULL
  )
  if (is.null(test)) return(list(r = NA_real_, p = NA_real_))
  list(r = unname(test$estimate), p = test$p.value)
}

compute_pair <- function(df_sub, var1, var2, group_label, method, alternative,
                         conf_level, missing_method, controls, complete_idx) {
  total_n <- nrow(df_sub)
  pair_vars <- unique(c(var1, var2, controls))
  if (missing_method == "complete" && !is.null(complete_idx)) {
    df_use <- df_sub[complete_idx, pair_vars, drop = FALSE]
  } else {
    df_use <- df_sub[, pair_vars, drop = FALSE]
    row_idx <- get_complete_rows(df_use)
    df_use <- df_use[row_idx, , drop = FALSE]
  }

  n_used <- nrow(df_use)
  missing_n <- total_n - n_used
  missing_pct <- ifelse(total_n > 0, missing_n / total_n * 100, NA_real_)

  r <- NA_real_
  p_val <- NA_real_
  ci_low <- NA_real_
  ci_high <- NA_real_

  if (n_used >= 3) {
    if (length(controls) > 0) {
      partial_data <- prepare_partial(df_use, var1, var2, controls, method)
      test <- safe_cor_test(partial_data$x, partial_data$y, "pearson", alternative, conf_level)
      r <- test$r
      p_val <- test$p
    } else {
      test <- safe_cor_test(df_use[[var1]], df_use[[var2]], method, alternative, conf_level)
      r <- test$r
      p_val <- test$p
    }

    if (!is.na(r) && (method == "pearson" || length(controls) > 0)) {
      ci_vals <- calc_ci(r, n_used, conf_level)
      ci_low <- ci_vals[1]
      ci_high <- ci_vals[2]
    }
  }

  data.frame(
    var1 = var1,
    var2 = var2,
    group = group_label,
    method = method,
    alternative = alternative,
    controls = ifelse(length(controls) > 0, paste(controls, collapse = ","), ""),
    partial = length(controls) > 0,
    n = n_used,
    total_n = total_n,
    missing_n = missing_n,
    missing_pct = missing_pct,
    r = r,
    p_value = p_val,
    ci_low = ci_low,
    ci_high = ci_high,
    stringsAsFactors = FALSE
  )
}

build_diagnostics <- function(df_sub, vars, group_label) {
  rows <- list()
  total_n <- nrow(df_sub)
  for (var in vars) {
    vec <- df_sub[[var]]
    missing_n <- sum(is.na(vec))
    missing_pct <- ifelse(total_n > 0, missing_n / total_n * 100, NA_real_)
    valid <- vec[!is.na(vec)]
    n <- length(valid)

    shapiro_w <- NA_real_
    shapiro_p <- NA_real_
    if (n >= 3 && n <= 5000) {
      test <- tryCatch(shapiro.test(valid), error = function(e) NULL)
      if (!is.null(test)) {
        shapiro_w <- unname(test$statistic)
        shapiro_p <- test$p.value
      }
    }

    rows[[length(rows) + 1]] <- data.frame(
      variable = var,
      group = group_label,
      n = n,
      total_n = total_n,
      missing_n = missing_n,
      missing_pct = missing_pct,
      skewness = calc_skewness(valid),
      kurtosis = calc_kurtosis(valid),
      shapiro_w = shapiro_w,
      shapiro_p = shapiro_p,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}


format_num <- function(value, digits) {
  if (is.na(value)) return("NA")
  format(round(value, digits), nsmall = digits, trim = TRUE)
}

format_r <- function(value, digits) {
  if (is.na(value)) return("NA")
  r_txt <- format(round(value, digits), nsmall = digits, trim = TRUE)
  sub("^(-?)0", "\\1", r_txt)
}

format_ci <- function(low, high, digits) {
  if (is.na(low) || is.na(high)) return("NA")
  paste0("[", format_r(low, digits), ", ", format_r(high, digits), "]")
}

format_p <- function(p_value) {
  if (is.na(p_value)) return("NA")
  if (p_value < 0.001) return("< .001")
  p_txt <- format(round(p_value, 3), nsmall = 3, trim = TRUE)
  p_txt <- sub("^0", "", p_txt)
  paste("=", p_txt)
}

method_title <- function(method, partial) {
  base <- switch(method,
                 pearson = "Pearson",
                 spearman = "Spearman",
                 kendall = "Kendall",
                 method)
  if (partial) paste("Partial", base) else base
}

method_text <- function(method, partial) {
  base <- switch(method,
                 pearson = "Pearson's r",
                 spearman = "Spearman's rho",
                 kendall = "Kendall's tau",
                 method)
  if (partial) paste("partial", base) else base
}

format_apa_table <- function(summary_df, digits, conf_level, adjust_method, missing_method, alternative) {
  display <- summary_df
  display$group <- as.character(display$group)
  display$group[is.na(display$group)] <- "NA"
  use_group <- !all(display$group == "")

  partial <- nrow(display) > 0 && any(display$partial)
  method <- if (nrow(display) > 0) display$method[1] else ""
  title <- paste(method_title(method, partial), "correlations")

  ci_label <- paste0(round(conf_level * 100), "% CI")
  include_p_adj <- adjust_method != "none"
  has_ci <- any(!is.na(display$ci_low))

  headers <- c("Variable 1", "Variable 2", "r")
  if (has_ci) headers <- c(headers, ci_label)
  headers <- c(headers, "p")
  if (include_p_adj) headers <- c(headers, "p_adj")
  headers <- c(headers, "n")
  if (use_group) headers <- c("Group", headers)

  sections <- character(0)
  groups <- unique(display$group)
  for (g in groups) {
    subset <- display[display$group == g, , drop = FALSE]
    header <- paste0("Table 1\n", title)
    if (use_group) header <- paste0(header, "\nGroup: ", g)
    md <- paste0(header, "\n\n| ", paste(headers, collapse = " | "), " |\n")
    md <- paste0(md, "| ", paste(rep("---", length(headers)), collapse = " | "), " |\n")

    for (i in seq_len(nrow(subset))) {
      row <- subset[i, ]
      p_val <- row$p_value
      p_adj <- row$p_adjusted
      row_vals <- c()
      if (use_group) row_vals <- c(row_vals, g)
      row_vals <- c(
        row_vals,
        row$var1,
        row$var2,
        ifelse(is.na(row$r), "", format_r(row$r, digits))
      )
      if (has_ci) {
        row_vals <- c(row_vals, ifelse(is.na(row$ci_low), "", format_ci(row$ci_low, row$ci_high, digits)))
      }
      row_vals <- c(row_vals, ifelse(is.na(p_val), "", format_p(p_val)))
      if (include_p_adj) row_vals <- c(row_vals, ifelse(is.na(p_adj), "", format_p(p_adj)))
      row_vals <- c(row_vals, ifelse(is.na(row$n), "", as.character(row$n)))
      md <- paste0(md, "| ", paste(row_vals, collapse = " | "), " |\n")
    }

    note_parts <- c()
    note_parts <- c(note_parts, paste("Note.", if (alternative != "two.sided") "One-tailed tests." else "Two-tailed tests."))
    note_parts <- c(note_parts, paste0("Missing values handled ", missing_method, "."))
    if (partial && any(display$controls != "")) {
      controls <- unique(display$controls[display$controls != ""])
      note_parts <- c(note_parts, paste0("Partial correlations control for ", paste(controls, collapse = "; "), "."))
    }
    if (include_p_adj) {
      note_parts <- c(note_parts, paste0("p-values adjusted using ", adjust_label(adjust_method), "."))
    }
    if (has_ci) {
      note_parts <- c(note_parts, paste(ci_label, "computed via Fisher's z.", sep = " "))
    }
    md <- paste0(md, "\n", paste(note_parts, collapse = " "), "\n")
    sections <- c(sections, md)
  }

  paste(sections, collapse = "\n\n")
}

format_apa_text <- function(summary_df, digits, conf_level, adjust_method, missing_method, alternative) {
  display <- summary_df
  display$group <- as.character(display$group)
  display$group[is.na(display$group)] <- "NA"
  lines <- character(0)

  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    label <- if (row$group == "") {
      paste(row$var1, "with", row$var2)
    } else {
      paste("Group", row$group, ",", row$var1, "with", row$var2)
    }

    missing_pct <- ifelse(is.na(row$missing_pct), "NA", format_num(row$missing_pct, 1))
    missing_part <- paste("Missing =", ifelse(is.na(row$missing_n), "NA", as.character(row$missing_n)),
                          "(", missing_pct, "%)", sep = " ")

    if (is.na(row$r) || is.na(row$n) || row$n < 3) {
      line <- sprintf(
        "%s: correlation could not be computed (n = %s). %s.",
        label,
        ifelse(is.na(row$n), "NA", as.character(row$n)),
        missing_part
      )
      lines <- c(lines, line)
      next
    }

    p_val <- if (adjust_method != "none" && !is.na(row$p_adjusted)) row$p_adjusted else row$p_value
    p_text <- format_p(p_val)
    stat_text <- method_text(row$method, row$partial)
    r_text <- format_r(row$r, digits)
    ci_text <- ""
    if (!is.na(row$ci_low) && !is.na(row$ci_high)) {
      ci_text <- paste0(", ", round(conf_level * 100), "% CI ", format_ci(row$ci_low, row$ci_high, digits))
    }

    line <- paste0(
      label, ": ",
      stat_text, " = ", r_text,
      ci_text,
      ", p ", p_text,
      ", n = ", as.character(row$n),
      ". ", missing_part, "."
    )
    lines <- c(lines, line)
  }

  note_parts <- c()
  note_parts <- c(note_parts, if (alternative != "two.sided") "Note. One-tailed tests." else "Note. Two-tailed tests.")
  note_parts <- c(note_parts, paste0("Missing values handled ", missing_method, "."))
  if (adjust_method != "none") {
    note_parts <- c(note_parts, paste0("p-values adjusted using ", adjust_label(adjust_method), "."))
  }

  paste(c(lines, paste(note_parts, collapse = " ")), collapse = "\n")
}

format_ci_cell <- function(low, high, digits) {
  if (is.na(low) || is.na(high)) return("")
  format_ci(low, high, digits)
}

format_p_cell <- function(value) {
  if (is.na(value)) return("")
  format_p(value)
}

format_r_cell <- function(value, digits) {
  if (is.na(value)) return("")
  format_r(value, digits)
}

build_correlations_table_body <- function(summary_df, digits, conf_level, adjust_method, table_spec = NULL) {
  display <- summary_df
  display$group <- as.character(display$group)
  display$group[is.na(display$group)] <- "NA"

  ci_label <- paste0(round(conf_level * 100), "% CI")
  default_columns <- list(
    list(key = "group", label = "Group", drop_if_empty = TRUE),
    list(key = "var1", label = "Variable 1"),
    list(key = "var2", label = "Variable 2"),
    list(key = "r", label = "r"),
    list(key = "ci", label = ci_label, drop_if_empty = TRUE),
    list(key = "p", label = "p"),
    list(key = "p_adj", label = "p_adj", drop_if_empty = TRUE),
    list(key = "n", label = "n")
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
      if (key == "group") {
        val <- resolve_as_cell_text(row$group)
      } else if (key == "var1") {
        val <- resolve_as_cell_text(row$var1)
      } else if (key == "var2") {
        val <- resolve_as_cell_text(row$var2)
      } else if (key == "r") {
        val <- format_r_cell(row$r, digits)
      } else if (key == "ci") {
        val <- format_ci_cell(row$ci_low, row$ci_high, digits)
      } else if (key == "p") {
        val <- format_p_cell(row$p_value)
      } else if (key == "p_adj") {
        val <- format_p_cell(row$p_adjusted)
      } else if (key == "n") {
        val <- ifelse(is.na(row$n), "", as.character(row$n))
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
  label_tokens <- list(ci_label = ci_label)
  headers <- vapply(columns, function(col) {
    label <- if (!is.null(col$label) && nzchar(col$label)) col$label else col$key
    resolve_render_template_tokens(label, label_tokens)
  }, character(1))
  resolve_render_markdown_table(headers, rows)
}

build_correlations_note_tokens <- function(summary_df, conf_level, adjust_method, missing_method, alternative) {
  display <- summary_df
  partial <- nrow(display) > 0 && any(display$partial)
  controls <- unique(display$controls[display$controls != ""])
  ci_label <- paste0(round(conf_level * 100), "% CI")
  tail_note <- if (alternative != "two.sided") "One-tailed tests." else "Two-tailed tests."
  missing_note <- paste0("Missing values handled ", missing_method, ".")
  partial_note <- ""
  if (partial && length(controls) > 0) {
    partial_note <- paste0("Partial correlations control for ", paste(controls, collapse = "; "), ".")
  }
  p_adjust_note <- ""
  if (adjust_method != "none") {
    p_adjust_note <- paste0("p-values adjusted using ", adjust_label(adjust_method), ".")
  }
  ci_note <- ""
  if (nrow(display) > 0 && any(!is.na(display$ci_low))) {
    ci_note <- paste(ci_label, "computed via Fisher's z.")
  }
  parts <- c(tail_note, missing_note, partial_note, p_adjust_note, ci_note)
  note_default <- paste(parts[nzchar(parts)], collapse = " ")
  list(
    ci_label = ci_label,
    tail_note = tail_note,
    missing_note = missing_note,
    partial_note = partial_note,
    p_adjust_note = p_adjust_note,
    ci_note = ci_note,
    note_default = note_default
  )
}

build_correlations_narrative_rows <- function(summary_df, digits, conf_level, adjust_method) {
  display <- summary_df
  display$group <- as.character(display$group)
  display$group[is.na(display$group)] <- "NA"
  rows <- list()
  for (i in seq_len(nrow(display))) {
    row <- display[i, , drop = FALSE]
    label <- if (row$group == "") {
      paste(row$var1, "with", row$var2)
    } else {
      paste("Group", row$group, ",", row$var1, "with", row$var2)
    }

    missing_pct_str <- ifelse(is.na(row$missing_pct), "NA", format_num(row$missing_pct, 1))
    missing_n_str <- ifelse(is.na(row$missing_n), "NA", as.character(row$missing_n))
    missing_text <- paste("Missing =", missing_n_str, "(", missing_pct_str, "%)", sep = " ")

    n_str <- ifelse(is.na(row$n), "NA", as.character(row$n))
    stat_text <- method_text(row$method, row$partial)
    r_text <- format_r_cell(row$r, digits)
    ci_text <- ""
    ci_only <- ""
    if (!is.na(row$ci_low) && !is.na(row$ci_high)) {
      ci_only <- format_ci(row$ci_low, row$ci_high, digits)
      ci_text <- paste0(", ", round(conf_level * 100), "% CI ", ci_only)
    }

    p_val <- if (adjust_method != "none" && !is.na(row$p_adjusted)) row$p_adjusted else row$p_value
    p_text <- format_p_cell(p_val)

    if (is.na(row$r) || is.na(row$n) || row$n < 3) {
      line <- sprintf(
        "%s: correlation could not be computed (n = %s). %s.",
        label,
        n_str,
        missing_text
      )
    } else {
      line <- paste0(
        label, ": ",
        stat_text, " = ", r_text,
        ci_text,
        ", p ", p_text,
        ", n = ", n_str,
        ". ", missing_text, "."
      )
    }

    rows[[length(rows) + 1]] <- list(
      label = label,
      group = resolve_as_cell_text(row$group),
      var1 = resolve_as_cell_text(row$var1),
      var2 = resolve_as_cell_text(row$var2),
      stat_text = stat_text,
      r = r_text,
      ci = ci_only,
      ci_text = ci_text,
      p = p_text,
      n = n_str,
      missing_n = missing_n_str,
      missing_pct = missing_pct_str,
      missing_text = missing_text,
      full_sentence = line
    )
  }
  rows
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
  conf_default <- resolve_config_value("modules.correlations.conf_level", 0.95)
  method_default <- resolve_config_value("modules.correlations.method", "pearson")
  missing_default <- resolve_config_value("modules.correlations.missing", "pairwise")
  alternative_default <- resolve_config_value("modules.correlations.alternative", "two.sided")
  adjust_default <- resolve_config_value("modules.correlations.p_adjust", "none")
  coerce_default <- resolve_config_value("modules.correlations.coerce", FALSE)
  digits <- if (!is.null(opts$digits)) as.numeric(opts$digits) else digits_default
  conf_level <- if (!is.null(opts$`conf-level`)) as.numeric(opts$`conf-level`) else conf_default
  method <- normalize_method(opts$method, default = method_default)
  missing_method <- normalize_missing(opts$missing, default = missing_default)
  alternative <- normalize_alternative(opts$alternative, default = alternative_default)
  adjust_method <- normalize_adjust(opts$`p-adjust`, default = adjust_default)
  coerce_flag <- resolve_parse_bool(opts$coerce, default = coerce_default)

  df <- resolve_load_dataframe(opts)
  out_dir <- resolve_get_workspace_out_dir(df)
  group_var <- if (!is.null(opts$group) && opts$group != "") opts$group else NULL

  vars <- resolve_parse_list(opts$vars)
  x_vars <- resolve_parse_list(opts$x)
  y_vars <- resolve_parse_list(opts$y)
  controls <- resolve_parse_list(opts$controls)
  if (!is.null(group_var)) controls <- setdiff(controls, group_var)

  if (length(x_vars) == 0 && length(y_vars) == 0 && length(vars) == 0) {
    vars <- select_numeric_vars(df, group_var)
  }

  pairs <- NULL
  if (length(x_vars) > 0 || length(y_vars) > 0) {
    if (length(x_vars) == 0 || length(y_vars) == 0) {
      vars <- unique(c(x_vars, y_vars))
      pairs <- build_pairs(vars)
    } else {
      pairs <- build_cross_pairs(x_vars, y_vars)
    }
  } else {
    pairs <- build_pairs(vars)
  }

  if (nrow(pairs) == 0) stop("No variable pairs available for correlation analysis.")

  all_vars <- unique(c(pairs$var1, pairs$var2, controls, if (!is.null(group_var)) group_var))
  missing_vars <- setdiff(all_vars, names(df))
  if (length(missing_vars) > 0) {
    stop(paste("Unknown variables:", paste(missing_vars, collapse = ", ")))
  }

  if (length(controls) > 0 && method == "kendall") {
    stop("Partial correlations are not supported for Kendall's tau.")
  }

  df <- coerce_dataframe(df, c(pairs$var1, pairs$var2, controls), coerce_flag)
  for (var in unique(c(pairs$var1, pairs$var2, controls))) {
    if (!is.numeric(df[[var]])) {
      stop(paste("Variable is not numeric:", var, "(use --coerce to convert)."))
    }
  }

  summary_list <- list()
  diagnostics_list <- list()

  if (!is.null(group_var)) {
    group_vec <- df[[group_var]]
    group_levels <- unique(group_vec)
    for (g in group_levels) {
      idx <- if (is.na(g)) is.na(group_vec) else group_vec == g
      df_sub <- df[idx, , drop = FALSE]
      group_label <- ifelse(is.na(g), "NA", as.character(g))

      complete_idx <- NULL
      if (missing_method == "complete") {
        complete_df <- df_sub[, unique(c(pairs$var1, pairs$var2, controls)), drop = FALSE]
        complete_idx <- get_complete_rows(complete_df)
      }

      for (i in seq_len(nrow(pairs))) {
        row <- pairs[i, ]
        summary_list[[length(summary_list) + 1]] <- compute_pair(
          df_sub, row$var1, row$var2, group_label,
          method, alternative, conf_level, missing_method, controls, complete_idx
        )
      }

      diagnostics_vars <- unique(c(pairs$var1, pairs$var2, controls))
      diagnostics_list[[length(diagnostics_list) + 1]] <- build_diagnostics(df_sub, diagnostics_vars, group_label)
    }
  } else {
    complete_idx <- NULL
    if (missing_method == "complete") {
      complete_df <- df[, unique(c(pairs$var1, pairs$var2, controls)), drop = FALSE]
      complete_idx <- get_complete_rows(complete_df)
    }
    for (i in seq_len(nrow(pairs))) {
      row <- pairs[i, ]
      summary_list[[length(summary_list) + 1]] <- compute_pair(
        df, row$var1, row$var2, "",
        method, alternative, conf_level, missing_method, controls, complete_idx
      )
    }

    diagnostics_vars <- unique(c(pairs$var1, pairs$var2, controls))
    diagnostics_list[[length(diagnostics_list) + 1]] <- build_diagnostics(df, diagnostics_vars, "")
  }

  summary_df <- do.call(rbind, summary_list)
  diagnostics_df <- do.call(rbind, diagnostics_list)

  summary_df$missing_method <- missing_method
  summary_df$conf_level <- conf_level

  summary_df$p_adjusted <- NA_real_
  summary_df$p_adjust_method <- adjust_method
  if (adjust_method != "none") {
    groups <- unique(summary_df$group)
    for (g in groups) {
      idx <- summary_df$group == g & !is.na(summary_df$p_value)
      if (any(idx)) {
        summary_df$p_adjusted[idx] <- p.adjust(summary_df$p_value[idx], method = adjust_method)
      }
    }
  }

  use_cross_template <- length(x_vars) > 0 && length(y_vars) > 0
  template_override <- resolve_template_override(opts$template, module = "correlations")
  template_path <- if (!is.null(template_override)) {
    template_override
  } else if (use_cross_template) {
    resolve_get_template_path("correlations.cross", "correlations/cross-correlation-template.md")
  } else {
    resolve_get_template_path("correlations.default", "correlations/default-template.md")
  }

  analysis_flags <- list(
    vars = if (length(x_vars) == 0 || length(y_vars) == 0) vars else NULL,
    x = if (length(x_vars) > 0) x_vars else NULL,
    y = if (length(y_vars) > 0) y_vars else NULL,
    group = if (!is.null(group_var) && group_var != "") group_var else "None",
    method = method,
    missing = missing_method,
    alternative = alternative,
    controls = if (length(controls) > 0) controls else "None",
    "p-adjust" = adjust_method,
    "conf-level" = conf_level,
    coerce = coerce_flag,
    digits = digits
  )

  apa_report_path <- file.path(out_dir, "apa_report.md")
  apa_table <- format_apa_table(summary_df, digits, conf_level, adjust_method, missing_method, alternative)
  apa_text <- format_apa_text(summary_df, digits, conf_level, adjust_method, missing_method, alternative)
  template_meta <- resolve_get_template_meta(template_path)
  table_body <- build_correlations_table_body(summary_df, digits, conf_level, adjust_method, template_meta$table)
  note_tokens <- build_correlations_note_tokens(summary_df, conf_level, adjust_method, missing_method, alternative)
  narrative_rows <- build_correlations_narrative_rows(summary_df, digits, conf_level, adjust_method)
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
  resolve_append_apa_report(
    apa_report_path,
    "Correlations",
    apa_table,
    apa_text,
    analysis_flags = analysis_flags,
    template_path = template_path,
    template_context = template_context
  )

  cat("Wrote:\n")
  cat("- ", apa_report_path, "\n", sep = "")

  if (resolve_parse_bool(opts$log, default = log_default)) {
    ctx <- resolve_get_run_context()
    resolve_append_analysis_log(
      out_dir,
      module = "correlations",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(summary_df = summary_df, diagnostics_df = diagnostics_df),
      options = list(
        digits = digits,
        conf_level = conf_level,
        method = method,
        missing = missing_method,
        alternative = alternative,
        p_adjust = adjust_method,
        coerce = coerce_flag,
        vars = vars,
        x = x_vars,
        y = y_vars,
        controls = controls,
        group = group_var
      ),
      user_prompt = resolve_get_user_prompt(opts)
    )
  }
}

main()
