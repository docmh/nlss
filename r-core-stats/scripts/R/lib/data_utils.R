get_levels <- function(vec) {
  if (is.factor(vec)) {
    return(as.character(levels(vec)))
  }
  values <- unique(vec[!is.na(vec)])
  if (length(values) == 0) return(character(0))
  if (is.numeric(values)) {
    return(as.character(sort(values)))
  }
  as.character(sort(values))
}

select_variables <- function(df, vars, group_var = NULL, default = "numeric", include_numeric = FALSE) {
  available <- names(df)
  if (is.null(vars) || vars == "") {
    if (default == "all") {
      selected <- available
    } else if (default == "non-numeric") {
      if (include_numeric) {
        selected <- available
      } else {
        selected <- available[!sapply(df, is.numeric)]
        if (length(selected) == 0) selected <- available
      }
    } else {
      selected <- available[sapply(df, is.numeric)]
    }
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
