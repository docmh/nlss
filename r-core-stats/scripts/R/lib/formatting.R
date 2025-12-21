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
    "x" = "X variables",
    "y" = "Y variables",
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
    "fisher-conf-level" = "Fisher confidence level"
  )
  if (!is.null(name) && name %in% names(mapping)) return(unname(mapping[[name]]))
  label <- gsub("[-_]", " ", as.character(name))
  words <- strsplit(label, " ", fixed = TRUE)[[1]]
  words <- words[words != ""]
  words <- paste0(toupper(substr(words, 1, 1)), substr(words, 2))
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

get_template_path <- function(analysis_label) {
  label <- tolower(trimws(analysis_label))
  if (label == "descriptive statistics") {
    path <- file.path(get_assets_dir(), "descriptive-stats", "default-template.md")
    if (file.exists(path)) return(path)
  }
  if (label == "correlations") {
    path <- file.path(get_assets_dir(), "correlations", "default-template.md")
    if (file.exists(path)) return(path)
  }
  if (label == "cross-tabulations") {
    path <- file.path(get_assets_dir(), "crosstabs", "default-template.md")
    if (file.exists(path)) return(path)
  }
  NULL
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

format_descriptive_report <- function(template_path, analysis_flags, table_number, apa_table, apa_text) {
  template <- paste(readLines(template_path, warn = FALSE), collapse = "\n")
  flags_text <- format_analysis_flags(analysis_flags)
  if (!nzchar(flags_text)) flags_text <- "None."
  split <- split_table_note(apa_table)
  table_body <- strip_table_header(split$table)
  note_body <- format_note_body(split$note)
  report <- template
  report <- gsub("<analysis flags in human readable form>", flags_text, report, fixed = TRUE)
  report <- gsub("<running number>", as.character(table_number), report, fixed = TRUE)
  table_placeholders <- c(
    "<actual table with columns: Variable, Group (if used), n, Missing n, Missing %, M, SD, Min, Max, 95% CI>",
    "<actual table with columns: Variable 1, Variable 2, r/rho/tau, 95% CI (if reported), p, n, Group (if used)>",
    "<actual table with columns: X Variable, Y Variable, r/rho/tau, 95% CI (if reported), p, n, Group (if used)>",
    "<actual table with columns: Row variable, Column variable, Row level, Column level, n, Row %, Column %, Total %>",
    "<actual table with columns: Group, Row variable, Column variable, Row level, Column level, n, Row %, Column %, Total %>"
  )
  for (placeholder in table_placeholders) {
    report <- gsub(placeholder, table_body, report, fixed = TRUE)
  }
  note_placeholders <- c(
    "<optional note about missingness, rounding, grouping, or CI method>",
    "<optional note about method, missing-data handling, p-value adjustment, controls, or CI>",
    "<optional note about missingness, percentages, or grouping>"
  )
  for (placeholder in note_placeholders) {
    report <- gsub(placeholder, note_body, report, fixed = TRUE)
  }
  report <- gsub("<APA 7 narrative text>", apa_text, report, fixed = TRUE)
  report
}

format_apa_report <- function(analysis_label, apa_table, apa_text, analysis_flags = NULL, template_path = NULL, table_start = 1) {
  flags_text <- format_analysis_flags(analysis_flags)
  if (!is.null(template_path) && file.exists(template_path)) {
    return(format_descriptive_report(template_path, analysis_flags, table_start, apa_table, apa_text))
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

append_apa_report <- function(path, analysis_label, apa_table, apa_text, analysis_flags = NULL, template_path = NULL) {
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
    table_start = table_start
  )
  if (file.exists(path)) {
    info <- file.info(path)
    if (!is.na(info$size) && info$size > 0) {
      report <- paste0("\n\n---\n\n", report)
    }
  }
  cat(report, file = path, append = TRUE, sep = "")
}
