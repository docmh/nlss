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

normalize_label_string <- function(value) {
  if (is.null(value) || length(value) == 0) return("")
  if (is.na(value[1])) return("")
  text <- as.character(value[1])
  text <- trimws(text)
  if (!nzchar(text)) return("")
  text
}

is_numeric_string <- function(values) {
  if (is.null(values) || length(values) == 0) return(FALSE)
  vals <- as.character(values)
  vals <- vals[nzchar(vals)]
  if (length(vals) == 0) return(FALSE)
  all(grepl("^\\s*-?\\d+(\\.\\d+)?\\s*$", vals))
}

extract_value_label_map <- function(labels) {
  if (is.null(labels)) return(NULL)
  if (is.list(labels) && !is.atomic(labels)) {
    if (!is.null(names(labels)) && any(nzchar(names(labels)))) {
      out <- list()
      for (name in names(labels)) {
        code <- normalize_label_string(name)
        label <- normalize_label_string(labels[[name]])
        if (!nzchar(code) || !nzchar(label)) next
        out[[code]] <- label
      }
      if (length(out) > 0) return(out)
    }
    labels <- unlist(labels, use.names = TRUE)
  }
  if (!is.atomic(labels)) return(NULL)
  if (length(labels) == 0) return(NULL)
  label_names <- names(labels)
  if (is.null(label_names) || all(!nzchar(label_names))) return(NULL)
  label_names <- as.character(label_names)
  label_values <- as.character(unname(labels))
  names_are_codes <- is_numeric_string(label_names) && !is_numeric_string(label_values)
  if (names_are_codes) {
    codes <- label_names
    labels_out <- label_values
  } else {
    codes <- label_values
    labels_out <- label_names
  }
  out <- list()
  for (i in seq_along(codes)) {
    code <- normalize_label_string(codes[i])
    label <- normalize_label_string(labels_out[i])
    if (!nzchar(code) || !nzchar(label)) next
    out[[code]] <- label
  }
  if (length(out) == 0) return(NULL)
  out
}

extract_variable_label <- function(vec) {
  if (is.null(vec)) return("")
  label <- normalize_label_string(attr(vec, "label", exact = TRUE))
  if (nzchar(label)) return(label)
  label <- normalize_label_string(attr(vec, "variable.label", exact = TRUE))
  if (nzchar(label)) return(label)
  label <- normalize_label_string(attr(vec, "var.label", exact = TRUE))
  if (nzchar(label)) return(label)
  ""
}

extract_df_variable_labels <- function(df) {
  labels <- attr(df, "variable.labels", exact = TRUE)
  if (is.null(labels)) return(list())
  out <- list()
  if (is.list(labels)) {
    for (name in names(labels)) {
      label <- normalize_label_string(labels[[name]])
      if (nzchar(label)) out[[as.character(name)]] <- label
    }
    return(out)
  }
  if (is.atomic(labels) && !is.null(names(labels))) {
    for (name in names(labels)) {
      label <- normalize_label_string(labels[[name]])
      if (nzchar(label)) out[[as.character(name)]] <- label
    }
  }
  out
}

extract_df_value_labels <- function(df) {
  labels <- attr(df, "value.labels", exact = TRUE)
  if (is.null(labels) || !is.list(labels)) return(list())
  out <- list()
  for (name in names(labels)) {
    map <- extract_value_label_map(labels[[name]])
    if (!is.null(map)) out[[as.character(name)]] <- map
  }
  out
}

extract_label_metadata <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(list())
  var_labels <- extract_df_variable_labels(df)
  value_labels <- extract_df_value_labels(df)
  for (name in names(df)) {
    vec <- df[[name]]
    label <- extract_variable_label(vec)
    if (nzchar(label)) var_labels[[name]] <- label
    map <- NULL
    labels_attr <- attr(vec, "labels", exact = TRUE)
    if (is.null(labels_attr)) {
      labels_attr <- attr(vec, "value.labels", exact = TRUE)
    }
    map <- extract_value_label_map(labels_attr)
    if (!is.null(map)) value_labels[[name]] <- map
  }
  out <- list()
  if (length(var_labels) > 0) out$variables <- var_labels
  if (length(value_labels) > 0) out$values <- value_labels
  out
}

normalize_label_metadata <- function(metadata) {
  if (is.null(metadata) || !is.list(metadata)) return(list())
  out <- list()
  if (is.list(metadata$variables)) {
    vars <- list()
    for (name in names(metadata$variables)) {
      label <- normalize_label_string(metadata$variables[[name]])
      if (nzchar(label)) vars[[as.character(name)]] <- label
    }
    if (length(vars) > 0) out$variables <- vars
  }
  if (is.list(metadata$values)) {
    vals <- list()
    for (name in names(metadata$values)) {
      map <- extract_value_label_map(metadata$values[[name]])
      if (!is.null(map)) vals[[as.character(name)]] <- map
    }
    if (length(vals) > 0) out$values <- vals
  }
  out
}

merge_label_metadata <- function(primary, secondary) {
  base <- normalize_label_metadata(primary)
  extra <- normalize_label_metadata(secondary)
  if (length(extra) == 0) return(base)
  if (length(base) == 0) return(extra)
  if (is.list(extra$variables)) {
    if (!is.list(base$variables)) base$variables <- list()
    for (name in names(extra$variables)) {
      base$variables[[name]] <- extra$variables[[name]]
    }
  }
  if (is.list(extra$values)) {
    if (!is.list(base$values)) base$values <- list()
    for (name in names(extra$values)) {
      base$values[[name]] <- extra$values[[name]]
    }
  }
  base
}

resolve_label_metadata <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(list())
  existing <- attr(df, "nlss_labels", exact = TRUE)
  extracted <- extract_label_metadata(df)
  merged <- merge_label_metadata(existing, extracted)
  merged
}

attach_label_metadata <- function(df, metadata) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  merged <- merge_label_metadata(attr(df, "nlss_labels", exact = TRUE), metadata)
  if (length(merged) > 0) {
    attr(df, "nlss_labels") <- merged
  }
  df
}

resolve_variable_label <- function(labels, name) {
  if (is.null(name) || length(name) == 0 || is.na(name)) return("")
  var_name <- as.character(name[1])
  if (!is.list(labels) || !is.list(labels$variables)) return(var_name)
  label <- labels$variables[[var_name]]
  if (is.null(label) || !nzchar(label)) return(var_name)
  as.character(label)
}

resolve_value_label <- function(labels, var_name, value) {
  if (is.null(value) || length(value) == 0) return("")
  if (is.na(value[1])) return("NA")
  val <- as.character(value[1])
  if (is.null(var_name) || !nzchar(var_name)) return(val)
  if (!is.list(labels) || !is.list(labels$values)) return(val)
  mapping <- labels$values[[as.character(var_name)]]
  if (is.null(mapping) || !is.list(mapping)) return(val)
  mapped <- mapping[[val]]
  if (is.null(mapped) || !nzchar(mapped)) {
    map_names <- names(mapping)
    if (!is.null(map_names) && length(map_names) > 0) {
      vals <- vapply(mapping, function(x) normalize_label_string(x), character(1))
      hit <- which(vals == val)
      if (length(hit) > 0) {
        label <- normalize_label_string(map_names[hit[1]])
        if (nzchar(label)) return(label)
      }
    }
    return(val)
  }
  as.character(mapped)
}

escape_label_regex <- function(text) {
  gsub("([][{}()^$.|*+?\\\\])", "\\\\\\1", as.character(text))
}

resolve_term_label <- function(labels, term) {
  if (is.null(term) || length(term) == 0 || is.na(term[1])) return("")
  term_text <- as.character(term[1])
  if (!is.list(labels) || !is.list(labels$variables)) return(term_text)
  vars <- names(labels$variables)
  if (is.null(vars) || length(vars) == 0) return(term_text)
  vars <- vars[nzchar(vars)]
  if (length(vars) == 0) return(term_text)
  vars <- vars[order(nchar(vars), decreasing = TRUE)]
  out <- term_text
  if (is.list(labels$values)) {
    segments <- strsplit(out, ":", fixed = TRUE)[[1]]
    if (length(segments) > 0) {
      for (idx in seq_along(segments)) {
        seg <- segments[idx]
        seg_out <- seg
        for (var in vars) {
          mapping <- labels$values[[var]]
          if (is.null(mapping) || !is.list(mapping)) next
          if (!startsWith(seg, var)) next
          remainder <- substring(seg, nchar(var) + 1)
          remainder <- normalize_label_string(remainder)
          if (!nzchar(remainder)) next
          map_names <- names(mapping)
          map_values <- vapply(mapping, function(x) normalize_label_string(x), character(1))
          has_match <- (!is.null(map_names) && remainder %in% map_names) || remainder %in% map_values
          if (!has_match) next
          var_label <- resolve_variable_label(labels, var)
          level_label <- resolve_value_label(labels, var, remainder)
          if (!nzchar(level_label)) level_label <- remainder
          seg_out <- paste0(var_label, "=", level_label)
          break
        }
        segments[idx] <- seg_out
      }
      out <- paste(segments, collapse = ":")
    }
  }
  for (var in vars) {
    label <- labels$variables[[var]]
    label <- normalize_label_string(label)
    if (!nzchar(label) || label == var) next
    pattern <- paste0("(?<![A-Za-z0-9_\\.])", escape_label_regex(var), "(?![A-Za-z0-9_\\.])")
    out <- gsub(pattern, label, out, perl = TRUE)
  }
  out
}

add_variable_label_column <- function(df, labels, var_col = "variable", out_col = NULL) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  if (!var_col %in% names(df)) return(df)
  if (is.null(out_col)) out_col <- paste0(var_col, "_label")
  df[[out_col]] <- vapply(df[[var_col]], function(name) resolve_variable_label(labels, name), character(1))
  df
}

add_value_label_column <- function(df, labels, var_col, value_col, out_col = NULL) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  if (!var_col %in% names(df) || !value_col %in% names(df)) return(df)
  if (is.null(out_col)) out_col <- paste0(value_col, "_label")
  df[[out_col]] <- mapply(
    function(var_name, value) resolve_value_label(labels, var_name, value),
    df[[var_col]],
    df[[value_col]],
    USE.NAMES = FALSE
  )
  df
}

add_group_label_column <- function(df, labels, group_var, group_col = "group", out_col = NULL) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  if (is.null(group_var) || !nzchar(group_var)) return(df)
  if (!group_col %in% names(df)) return(df)
  if (is.null(out_col)) out_col <- paste0(group_col, "_label")
  df[[out_col]] <- vapply(
    df[[group_col]],
    function(value) resolve_value_label(labels, group_var, value),
    character(1)
  )
  df
}

add_term_label_column <- function(df, labels, term_col = "term", out_col = NULL) {
  if (is.null(df) || !is.data.frame(df)) return(df)
  if (!term_col %in% names(df)) return(df)
  if (is.null(out_col)) out_col <- paste0(term_col, "_label")
  df[[out_col]] <- vapply(df[[term_col]], function(term) resolve_term_label(labels, term), character(1))
  df
}

resolve_display_column <- function(df, base_col) {
  label_col <- paste0(base_col, "_label")
  if (!is.null(df) && is.data.frame(df) && label_col %in% names(df)) return(label_col)
  base_col
}

resolve_row_display <- function(row, base_col) {
  label_col <- paste0(base_col, "_label")
  if (!is.null(row) && label_col %in% names(row)) {
    return(as.character(row[[label_col]][1]))
  }
  if (!is.null(row) && base_col %in% names(row)) {
    return(as.character(row[[base_col]][1]))
  }
  ""
}
