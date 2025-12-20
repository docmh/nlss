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
