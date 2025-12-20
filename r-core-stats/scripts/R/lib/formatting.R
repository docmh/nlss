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

format_apa_report <- function(analysis_label, apa_table, apa_text) {
  paste0(
    "# APA Report\n\n",
    "Analysis: ", analysis_label, "\n\n",
    "## APA Table\n\n",
    apa_table,
    "\n\n## APA Narrative\n\n",
    apa_text,
    "\n"
  )
}
