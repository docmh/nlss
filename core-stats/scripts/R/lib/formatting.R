round_numeric <- function(df, digits) {
  out <- df
  numeric_cols <- sapply(out, is.numeric)
  out[numeric_cols] <- lapply(out[numeric_cols], function(x) round(x, digits))
  out
}

format_percent <- function(value, digits) {
  if (is.na(value)) return("")
  format(round(value, digits), nsmall = digits, trim = TRUE)
}

humanize_flag_name <- function(name) {
  mapping <- c(
    "vars" = "Variables",
    "group" = "Grouping variable",
    "digits" = "Rounding digits",
    "method" = "Method",
    "missing" = "Missing handling",
    "alternative" = "Alternative hypothesis",
    "controls" = "Control variables",
    "p-adjust" = "P-value adjustment",
    "conf-level" = "Confidence level",
    "coerce" = "Coerce non-numeric",
    "reverse" = "Reverse-scored items",
    "reverse-min" = "Reverse min",
    "reverse-max" = "Reverse max",
    "score" = "Scale score",
    "omega" = "Omega total",
    "x" = "X variables",
    "y" = "Y variables",
    "m" = "Mediators",
    "include-numeric" = "Include numeric variables",
    "percent" = "Percentages",
    "include-expected" = "Include expected counts",
    "include-residuals" = "Include residuals",
    "max-levels" = "Max levels",
    "top-n" = "Top levels",
    "rows" = "Row variables",
    "cols" = "Column variables",
    "row" = "Row variable",
    "col" = "Column variable",
    "apa-percent" = "APA percent",
    "chisq" = "Chi-square test",
    "yates" = "Yates correction",
    "fisher" = "Fisher's exact test",
    "fisher-simulate" = "Fisher simulation",
    "fisher-b" = "Fisher replications",
    "fisher-conf-level" = "Fisher confidence level",
    "mu" = "Test value (mu)",
    "var-equal" = "Equal variances",
    "bootstrap" = "Bootstrap",
    "bootstrap-samples" = "Bootstrap samples",
    "test" = "Test selection",
    "exact" = "Exact p-values",
    "continuity" = "Continuity correction",
    "family" = "Family",
    "link" = "Link",
    "interactions" = "Interactions",
    "center" = "Centering",
    "standardize" = "Standardized coefficients",
    "formula" = "Formula",
    "fixed" = "Fixed effects",
    "random" = "Random effects",
    "reml" = "REML estimation",
    "df-method" = "DF method",
    "emmeans" = "Marginal means",
    "contrasts" = "Contrasts",
    "optimizer" = "Optimizer",
    "maxfun" = "Optimizer maxfun",
    "estimator" = "Estimator",
    "se" = "Standard errors",
    "ci" = "Confidence interval",
    "diagnostics" = "Diagnostics",
    "conf-level" = "Confidence level",
    "type" = "Sum of squares type",
    "effect-size" = "Effect size",
    "posthoc" = "Post-hoc method",
    "covariates" = "Covariates",
    "ordered" = "Ordered variables",
    "model" = "Model",
    "model-file" = "Model file",
    "factors" = "Factors",
    "serial" = "Serial mediation",
    "group-equal" = "Group constraints",
    "invariance" = "Invariance steps",
    "std" = "Standardization",
    "fit" = "Fit indices",
    "r2" = "R2 reporting",
    "modindices" = "Mod indices cutoff",
    "residuals" = "Residuals",
    "sphericity" = "Sphericity test",
    "analysis" = "Analysis",
    "mode" = "Mode",
    "dv" = "Dependent variable",
    "between" = "Between-subjects factors",
    "within" = "Within-subjects variables",
    "subject-id" = "Subject ID",
    "ivs" = "Predictors",
    "blocks" = "Blocks",
    "normality" = "Normality test",
    "homogeneity" = "Homogeneity test",
    "linearity" = "Linearity",
    "homoscedasticity" = "Homoscedasticity",
    "durbin-watson" = "Durbin-Watson",
    "outliers" = "Outliers",
    "influence" = "Influence",
    "alpha" = "Alpha",
    "vif" = "VIF",
    "vif-warn" = "VIF warning threshold",
    "vif-high" = "VIF high threshold",
    "outlier-z" = "Outlier z threshold",
    "cook-multiplier" = "Cook's D multiplier",
    "max-shapiro-n" = "Max Shapiro n",
    "low-threshold" = "Low missingness threshold",
    "moderate-threshold" = "Moderate missingness threshold",
    "high-threshold" = "High missingness threshold",
    "drop-threshold" = "Drop threshold",
    "indicator-threshold" = "Indicator threshold",
    "indicator-suffix" = "Indicator suffix",
    "skew-threshold" = "Skewness threshold",
    "max-patterns" = "Max patterns"
  )
  if (!is.null(name) && name %in% names(mapping)) return(unname(mapping[[name]]))
  label <- gsub("[-_]", " ", as.character(name))
  words <- strsplit(label, " ", fixed = TRUE)[[1]]
  words <- words[words != ""]
  if (length(words) == 0) return(label)
  words <- paste0(toupper(substr(words, 1, 1)), substr(words, 2, nchar(words)))
  paste(words, collapse = " ")
}

format_flag_value <- function(value) {
  if (is.null(value) || length(value) == 0) return("")
  if (is.logical(value)) {
    val <- ifelse(is.na(value), "NA", ifelse(value, "Yes", "No"))
    return(paste(val, collapse = ", "))
  }
  if (is.numeric(value)) {
    return(paste(format(value, trim = TRUE), collapse = ", "))
  }
  val <- as.character(value)
  val <- val[nzchar(val)]
  if (length(val) == 0) return("")
  paste(val, collapse = ", ")
}

format_analysis_flags <- function(flags) {
  if (is.null(flags)) return("")
  if (is.character(flags) && length(flags) == 1) return(flags)
  if (!is.list(flags)) return(format_flag_value(flags))
  names_list <- names(flags)
  if (is.null(names_list)) {
    vals <- vapply(flags, format_flag_value, character(1))
    vals <- vals[nzchar(vals)]
    if (length(vals) == 0) return("")
    return(paste(vals, collapse = "; "))
  }
  lines <- character(0)
  for (name in names_list) {
    value <- flags[[name]]
    value_text <- format_flag_value(value)
    if (!nzchar(value_text)) next
    label <- humanize_flag_name(name)
    lines <- c(lines, paste0("- ", label, ": ", value_text))
  }
  if (length(lines) == 0) return("")
  paste(lines, collapse = "\n")
}

get_assets_dir <- function() {
  script_dir <- if (exists("get_script_dir", mode = "function")) {
    get("get_script_dir", mode = "function")()
  } else {
    getwd()
  }
  file.path(script_dir, "..", "..", "assets")
}

is_absolute_path <- function(path) {
  grepl("^(~|/|[A-Za-z]:)", path)
}

resolve_template_path <- function(key, default_relative = NULL) {
  configured <- NULL
  if (exists("resolve_config_value", mode = "function")) {
    configured <- get("resolve_config_value", mode = "function")(paste0("templates.", key), NULL)
  }
  if (isFALSE(configured)) return(NULL)
  candidate <- configured
  if (is.null(candidate) || !nzchar(candidate)) candidate <- default_relative
  if (is.null(candidate) || !nzchar(candidate)) return(NULL)
  resolved <- if (is_absolute_path(candidate)) path.expand(candidate) else file.path(get_assets_dir(), candidate)
  if (!is.null(configured) && nzchar(configured) && !file.exists(resolved)) {
    if (!is.null(default_relative) && nzchar(default_relative)) {
      fallback <- file.path(get_assets_dir(), default_relative)
      if (file.exists(fallback)) return(fallback)
    }
  }
  resolved
}

resolve_template_config_path <- function(key) {
  configured <- NULL
  if (exists("resolve_config_value", mode = "function")) {
    configured <- get("resolve_config_value", mode = "function")(paste0("templates.", key), NULL)
  }
  if (isFALSE(configured) || is.null(configured) || !nzchar(configured)) return(NULL)
  resolved <- if (is_absolute_path(configured)) path.expand(configured) else file.path(get_assets_dir(), configured)
  if (!file.exists(resolved)) return(NULL)
  resolved
}

resolve_template_file_path <- function(path) {
  if (is.null(path) || !nzchar(path)) return(NULL)
  path <- as.character(path)
  if (is_absolute_path(path)) {
    expanded <- path.expand(path)
    if (file.exists(expanded)) return(expanded)
  }
  if (file.exists(path)) {
    return(normalizePath(path, winslash = "/", mustWork = FALSE))
  }
  assets_path <- file.path(get_assets_dir(), path)
  if (file.exists(assets_path)) return(assets_path)
  NULL
}

resolve_template_override <- function(template_ref, module = NULL) {
  if (is.null(template_ref) || is.logical(template_ref)) return(NULL)
  ref <- trimws(as.character(template_ref)[1])
  if (!nzchar(ref)) return(NULL)

  file_path <- resolve_template_file_path(ref)
  if (!is.null(file_path)) return(file_path)

  ref_key <- ref
  if (startsWith(ref_key, "templates.")) {
    ref_key <- sub("^templates\\.", "", ref_key)
  }
  if (grepl("\\.", ref_key)) {
    return(resolve_template_config_path(ref_key))
  }
  if (!is.null(module) && nzchar(module)) {
    return(resolve_template_config_path(paste0(module, ".", ref_key)))
  }
  NULL
}

get_template_path <- function(analysis_label) {
  label <- tolower(trimws(analysis_label))
  if (label == "descriptive statistics") {
    path <- resolve_template_path("descriptive_stats.default", "descriptive-stats/default-template.md")
    if (!is.null(path) && file.exists(path)) return(path)
  }
  if (label == "frequencies") {
    path <- resolve_template_path("frequencies.default", "frequencies/default-template.md")
    if (!is.null(path) && file.exists(path)) return(path)
  }
  if (label == "data exploration" || label == "data explorer") {
    path <- resolve_template_path("data_explorer.default", "data-explorer/default-template.md")
    if (!is.null(path) && file.exists(path)) return(path)
  }
  if (label == "data transformation" || label == "data transform") {
    path <- resolve_template_path("data_transform.default", "data-transform/default-template.md")
    if (!is.null(path) && file.exists(path)) return(path)
  }
  if (label == "correlations") {
    path <- resolve_template_path("correlations.default", "correlations/default-template.md")
    if (!is.null(path) && file.exists(path)) return(path)
  }
  if (label == "scale analysis" || label == "scale") {
    path <- resolve_template_path("scale.default", "scale/default-template.md")
    if (!is.null(path) && file.exists(path)) return(path)
  }
  if (label == "cross-tabulations") {
    path <- resolve_template_path("crosstabs.default", "crosstabs/default-template.md")
    if (!is.null(path) && file.exists(path)) return(path)
  }
  if (label == "anova") {
    path <- resolve_template_path("anova.default", "anova/default-template.md")
    if (!is.null(path) && file.exists(path)) return(path)
  }
  if (label == "regression") {
    path <- resolve_template_path("regression.default", "regression/default-template.md")
    if (!is.null(path) && file.exists(path)) return(path)
  }
  if (label == "mixed models") {
    path <- resolve_template_path("mixed_models.default", "mixed-models/default-template.md")
    if (!is.null(path) && file.exists(path)) return(path)
  }
  if (label == "sem (cfa)") {
    path <- resolve_template_path("sem.cfa", "sem/cfa-template.md")
    if (!is.null(path) && file.exists(path)) return(path)
  }
  if (label == "sem (mediation)") {
    path <- resolve_template_path("sem.mediation", "sem/mediation-template.md")
    if (!is.null(path) && file.exists(path)) return(path)
  }
  if (label == "sem (invariance)") {
    path <- resolve_template_path("sem.invariance", "sem/invariance-template.md")
    if (!is.null(path) && file.exists(path)) return(path)
  }
  if (label %in% c("sem", "structural equation modeling", "sem (path analysis)")) {
    path <- resolve_template_path("sem.default", "sem/default-template.md")
    if (!is.null(path) && file.exists(path)) return(path)
  }
  if (label %in% c("mixed models emmeans", "mixed models marginal means")) {
    path <- resolve_template_path("mixed_models.emmeans", "mixed-models/emmeans-template.md")
    if (!is.null(path) && file.exists(path)) return(path)
  }
  if (label %in% c("anova post-hoc", "anova posthoc", "anova post hoc")) {
    path <- resolve_template_path("anova.posthoc", "anova/posthoc-template.md")
    if (!is.null(path) && file.exists(path)) return(path)
  }
  NULL
}

escape_regex <- function(text) {
  gsub("([][{}()^$.|*+?\\\\])", "\\\\\\1", text)
}

escape_replacement <- function(text) {
  text <- gsub("\\\\", "\\\\\\\\", text)
  gsub("\\$", "\\\\\\$", text)
}

parse_yaml_text <- function(text) {
  if (!nzchar(trimws(text))) return(list())
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Template front matter requires the 'yaml' package.")
  }
  meta <- yaml::yaml.load(text)
  if (is.null(meta)) return(list())
  meta
}

parse_template_file <- function(path) {
  lines <- readLines(path, warn = FALSE)
  if (length(lines) == 0) return(list(meta = list(), body = ""))
  if (trimws(lines[1]) != "---") {
    return(list(meta = list(), body = paste(lines, collapse = "\n")))
  }
  end_idx <- which(trimws(lines[-1]) %in% c("---", "..."))
  if (length(end_idx) == 0) {
    stop("Template front matter not closed in: ", path)
  }
  end_line <- end_idx[1] + 1
  yaml_text <- paste(lines[2:(end_line - 1)], collapse = "\n")
  body_lines <- if (end_line < length(lines)) lines[(end_line + 1):length(lines)] else character(0)
  meta <- parse_yaml_text(yaml_text)
  list(meta = meta, body = paste(body_lines, collapse = "\n"))
}

get_template_meta <- function(path) {
  if (is.null(path) || !file.exists(path)) return(list())
  data <- parse_template_file(path)
  if (!is.list(data$meta)) return(list())
  data$meta
}

normalize_token_map <- function(tokens) {
  if (is.null(tokens)) return(list())
  if (!is.list(tokens)) return(list())
  out <- list()
  for (name in names(tokens)) {
    value <- tokens[[name]]
    if (is.null(value)) next
    if (is.character(value)) {
      if (length(value) == 0) next
      out[[name]] <- paste(value, collapse = "\n")
    } else if (is.numeric(value) || is.logical(value)) {
      out[[name]] <- paste(as.character(value), collapse = ", ")
    } else {
      out[[name]] <- as.character(value)
    }
  }
  out
}

render_template_tokens <- function(text, tokens) {
  rendered <- text
  if (length(tokens) == 0) return(rendered)
  for (name in names(tokens)) {
    value <- tokens[[name]]
    if (is.null(value)) next
    value <- as.character(value)
    pattern <- paste0("\\{\\{\\s*", escape_regex(name), "\\s*\\}\\}")
    rendered <- gsub(pattern, escape_replacement(value), rendered, perl = TRUE)
  }
  rendered
}

as_cell_text <- function(value) {
  if (length(value) == 0 || is.null(value)) return("")
  if (is.na(value)) return("")
  as.character(value)
}

find_default_column_spec <- function(default_specs, key) {
  if (length(default_specs) == 0) return(NULL)
  for (spec in default_specs) {
    if (!is.null(spec$key) && spec$key == key) return(spec)
  }
  NULL
}

normalize_table_columns <- function(columns, default_specs) {
  if (is.null(columns) || length(columns) == 0) return(default_specs)
  specs <- list()
  if (is.character(columns)) {
    columns <- as.list(columns)
  }
  for (col in columns) {
    key <- NULL
    label <- NULL
    drop_if_empty <- NULL
    if (is.character(col) && length(col) == 1) {
      key <- col
    } else if (is.list(col)) {
      if (!is.null(col$key)) {
        key <- col$key
      } else if (length(col) == 1 && is.character(col[[1]])) {
        key <- col[[1]]
      }
      if (!is.null(col$label)) label <- col$label
      if (!is.null(col$drop_if_empty)) drop_if_empty <- col$drop_if_empty
    }
    if (is.null(key) || !nzchar(key)) next
    default <- find_default_column_spec(default_specs, key)
    if (is.null(label) || !nzchar(label)) {
      label <- if (!is.null(default$label)) default$label else key
    }
    if (is.null(drop_if_empty)) {
      drop_if_empty <- if (!is.null(default$drop_if_empty)) default$drop_if_empty else FALSE
    }
    specs[[length(specs) + 1]] <- list(
      key = key,
      label = label,
      drop_if_empty = isTRUE(drop_if_empty)
    )
  }
  specs
}

drop_empty_columns <- function(columns, rows) {
  if (length(columns) == 0) return(list(columns = columns, rows = rows))
  if (length(rows) == 0) return(list(columns = columns, rows = rows))
  drop_flags <- logical(length(columns))
  for (i in seq_along(columns)) {
    if (!isTRUE(columns[[i]]$drop_if_empty)) next
    values <- vapply(rows, function(row) {
      if (length(row) < i) return("")
      val <- row[[i]]
      if (is.null(val) || is.na(val)) return("")
      as.character(val)
    }, character(1))
    drop_flags[i] <- all(!nzchar(values))
  }
  if (any(drop_flags)) {
    columns <- columns[!drop_flags]
    rows <- lapply(rows, function(row) row[!drop_flags])
  }
  list(columns = columns, rows = rows)
}

render_markdown_table <- function(headers, rows) {
  if (length(headers) == 0) return("")
  md <- paste0("| ", paste(headers, collapse = " | "), " |\n")
  md <- paste0(md, "| ", paste(rep("---", length(headers)), collapse = " | "), " |\n")
  for (row in rows) {
    row_vals <- row
    if (length(row_vals) < length(headers)) {
      row_vals <- c(row_vals, rep("", length(headers) - length(row_vals)))
    }
    md <- paste0(md, "| ", paste(row_vals, collapse = " | "), " |\n")
  }
  md
}

normalize_rows_tokens <- function(rows) {
  if (is.null(rows)) return(list())
  if (is.data.frame(rows)) {
    rows <- lapply(seq_len(nrow(rows)), function(i) {
      as.list(rows[i, , drop = FALSE])
    })
  }
  if (!is.list(rows)) return(list())
  out <- list()
  for (row in rows) {
    if (is.null(row)) next
    if (is.data.frame(row)) {
      row <- as.list(row[1, , drop = FALSE])
    }
    out[[length(out) + 1]] <- normalize_token_map(row)
  }
  out
}

render_narrative_rows <- function(row_template, rows, base_tokens, join = "\n", drop_empty = TRUE) {
  if (is.null(row_template) || !nzchar(row_template)) return("")
  rows_norm <- normalize_rows_tokens(rows)
  if (length(rows_norm) == 0) return("")
  lines <- character(0)
  for (row in rows_norm) {
    tokens <- base_tokens
    if (length(row) > 0) {
      tokens[names(row)] <- row
    }
    line <- render_template_tokens(row_template, tokens)
    line <- trimws(line)
    if (drop_empty && !nzchar(line)) next
    lines <- c(lines, line)
  }
  paste(lines, collapse = join)
}

expand_template_tokens <- function(tokens, base_tokens) {
  out <- list()
  if (length(tokens) == 0) return(out)
  for (name in names(tokens)) {
    value <- tokens[[name]]
    if (is.null(value)) next
    value <- as.character(value)
    out[[name]] <- render_template_tokens(value, base_tokens)
  }
  out
}

extract_table_numbers <- function(text) {
  matches <- gregexpr("Table\\s+[0-9]+", text, perl = TRUE)
  if (matches[[1]][1] == -1) return(integer(0))
  values <- regmatches(text, matches)[[1]]
  nums <- suppressWarnings(as.integer(sub("Table\\s+", "", values)))
  nums[!is.na(nums)]
}

get_next_table_number <- function(path) {
  if (!file.exists(path)) return(1)
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  nums <- extract_table_numbers(text)
  if (length(nums) == 0) return(1)
  max(nums) + 1
}

renumber_tables <- function(text, start_number) {
  matches <- gregexpr("Table\\s+[0-9]+", text, perl = TRUE)
  if (matches[[1]][1] == -1) return(list(text = text, count = 0))
  replacements <- sprintf("Table %d", seq_along(matches[[1]]) + start_number - 1)
  regmatches(text, matches) <- list(replacements)
  list(text = text, count = length(replacements))
}

trim_trailing_whitespace <- function(text) {
  sub("[\r\n[:space:]]+$", "", text)
}

split_table_note <- function(apa_table) {
  note_match <- regexpr("\nNote\\.", apa_table, perl = TRUE)
  if (note_match[1] == -1) {
    return(list(table = trim_trailing_whitespace(apa_table), note = ""))
  }
  table_part <- substr(apa_table, 1, note_match[1] - 1)
  note_part <- substr(apa_table, note_match[1] + 1, nchar(apa_table))
  list(table = trim_trailing_whitespace(table_part), note = trimws(note_part))
}

strip_table_header <- function(table_text) {
  lines <- strsplit(table_text, "\n", fixed = TRUE)[[1]]
  if (length(lines) == 0) return(table_text)
  while (length(lines) > 0 && trimws(lines[1]) == "") {
    lines <- lines[-1]
  }
  if (grepl("^Table\\s+[0-9]+", lines[1])) {
    lines <- lines[-1]
    if (length(lines) > 0 && !grepl("^\\|", lines[1]) && nzchar(trimws(lines[1]))) {
      lines <- lines[-1]
    }
  }
  while (length(lines) > 0 && trimws(lines[1]) == "") {
    lines <- lines[-1]
  }
  paste(lines, collapse = "\n")
}

format_note_body <- function(note_text) {
  note_text <- trimws(note_text)
  if (note_text == "") return("None.")
  note_text <- sub("^\\*?Note\\*?\\.?\\s*", "", note_text)
  if (note_text == "") return("None.")
  note_text
}

format_template_report <- function(template_path, analysis_label, analysis_flags, table_number, apa_table, apa_text, template_context = NULL) {
  template_data <- parse_template_file(template_path)
  template <- template_data$body
  meta <- template_data$meta
  flags_text <- format_analysis_flags(analysis_flags)
  if (!nzchar(flags_text)) flags_text <- "None."
  split <- split_table_note(apa_table)
  table_body <- strip_table_header(split$table)
  note_body <- format_note_body(split$note)
  narrative_default <- apa_text
  base_tokens <- list(
    analysis_label = analysis_label,
    analysis_flags = flags_text,
    table_number = as.character(table_number),
    table_body = table_body,
    note_body = note_body,
    narrative = narrative_default,
    note_default = note_body,
    narrative_default = narrative_default
  )
  ctx_tokens <- list()
  narrative_rows <- NULL
  if (is.list(template_context)) {
    if (!is.null(template_context$tokens)) {
      ctx_tokens <- normalize_token_map(template_context$tokens)
    }
    if (!is.null(template_context$narrative_rows)) {
      narrative_rows <- template_context$narrative_rows
    }
  }
  if (length(ctx_tokens) > 0) {
    base_tokens[names(ctx_tokens)] <- ctx_tokens
  }
  if (is.list(meta) && "note" %in% names(meta)) {
    note_template <- meta$note$template
    if (!is.null(note_template) && nzchar(note_template)) {
      note_body <- render_template_tokens(note_template, base_tokens)
      base_tokens$note_body <- note_body
    }
  }
  if (is.list(meta) && "narrative" %in% names(meta)) {
    row_template <- meta$narrative$row_template
    if (!is.null(row_template) && nzchar(row_template)) {
      join <- if (!is.null(meta$narrative$join)) as.character(meta$narrative$join) else "\n"
      drop_empty <- if (!is.null(meta$narrative$drop_empty)) isTRUE(meta$narrative$drop_empty) else TRUE
      narrative <- render_narrative_rows(row_template, narrative_rows, base_tokens, join = join, drop_empty = drop_empty)
      if (nzchar(narrative)) {
        base_tokens$narrative <- narrative
      }
    } else if (!is.null(meta$narrative$template) && nzchar(meta$narrative$template)) {
      narrative <- render_template_tokens(meta$narrative$template, base_tokens)
      base_tokens$narrative <- narrative
    }
  }
  meta_tokens <- list()
  if (is.list(meta) && "tokens" %in% names(meta)) {
    meta_tokens <- normalize_token_map(meta$tokens)
  }
  meta_tokens <- expand_template_tokens(meta_tokens, base_tokens)
  render_template_tokens(template, c(meta_tokens, base_tokens))
}

format_apa_report <- function(analysis_label, apa_table, apa_text, analysis_flags = NULL, template_path = NULL, table_start = 1, template_context = NULL) {
  flags_text <- format_analysis_flags(analysis_flags)
  if (!is.null(template_path) && file.exists(template_path)) {
    return(format_template_report(template_path, analysis_label, analysis_flags, table_start, apa_table, apa_text, template_context = template_context))
  }
  analysis_block <- analysis_label
  if (nzchar(flags_text)) {
    analysis_block <- paste0(analysis_label, "\n", flags_text)
  }
  paste0(
    "# APA Report\n\n",
    "Analysis: ", analysis_block, "\n\n",
    "## APA Table\n\n",
    apa_table,
    "\n\n## APA Narrative\n\n",
    apa_text,
    "\n"
  )
}

append_apa_report <- function(path, analysis_label, apa_table, apa_text, analysis_flags = NULL, template_path = NULL, template_context = NULL) {
  table_start <- get_next_table_number(path)
  resolved_template <- template_path
  if (is.null(resolved_template)) {
    resolved_template <- get_template_path(analysis_label)
  }
  if (is.null(resolved_template) || !file.exists(resolved_template)) {
    renumbered <- renumber_tables(apa_table, table_start)
    apa_table <- renumbered$text
  }
  report <- format_apa_report(
    analysis_label,
    apa_table,
    apa_text,
    analysis_flags = analysis_flags,
    template_path = resolved_template,
    table_start = table_start,
    template_context = template_context
  )
  if (file.exists(path)) {
    info <- file.info(path)
    if (!is.na(info$size) && info$size > 0) {
      report <- paste0("\n\n---\n\n", report)
    }
  }
  con <- file(path, open = "a", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  cat(report, file = con, sep = "")
}
