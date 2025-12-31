# SPDX-License-Identifier: Apache-2.0
read_contrast_json <- function(path) {
  if (is.null(path) || !nzchar(path)) return(NULL)
  if (!file.exists(path)) {
    stop(paste0("Contrast JSON not found: ", path))
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Contrast JSON requires the 'jsonlite' package.")
  }
  spec <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (!is.list(spec)) stop("Contrast JSON must be an object.")
  spec
}

normalize_contrast_mode <- function(value, default = "none") {
  val <- if (!is.null(value) && value != "") as.character(value) else default
  val_trim <- trimws(val)
  val_lower <- tolower(val_trim)
  if (val_lower %in% c("none", "no", "false")) return("none")
  if (val_lower %in% c("pairwise", "pairs")) return("pairwise")
  if (val_lower %in% c("custom", "json")) return("custom")
  val_trim
}

split_emmeans_term <- function(term_label) {
  if (is.null(term_label) || !nzchar(term_label)) return(character(0))
  pieces <- unlist(strsplit(term_label, "[\\*:\\+]", perl = TRUE))
  pieces <- trimws(pieces)
  pieces[pieces != ""]
}

build_emmeans_labels <- function(emm, term_label) {
  emm_df <- as.data.frame(emm)
  factor_cols <- names(emm_df)[!vapply(emm_df, is.numeric, logical(1))]
  term_factors <- split_emmeans_term(term_label)
  cols <- factor_cols
  if (length(term_factors) > 0) {
    match_cols <- intersect(term_factors, factor_cols)
    if (length(match_cols) > 0) cols <- match_cols
  }
  if (length(cols) == 0) {
    return(list(labels = rep("", nrow(emm_df)), columns = character(0)))
  }
  labels <- apply(emm_df[, cols, drop = FALSE], 1, function(row) {
    values <- as.character(row)
    if (length(cols) == 1) return(values[1])
    paste(paste0(cols, "=", values), collapse = ", ")
  })
  list(labels = labels, columns = cols)
}

coerce_contrast_weights <- function(weights, labels) {
  if (is.null(weights)) stop("Contrast weights are missing.")
  if (is.list(weights) && !is.data.frame(weights)) {
    if (!is.null(names(weights)) && any(nzchar(names(weights)))) {
      vec <- rep(0, length(labels))
      names(vec) <- labels
      for (name in names(weights)) {
        if (!(name %in% labels)) {
          stop(paste0("Unknown contrast level: ", name))
        }
        value <- suppressWarnings(as.numeric(weights[[name]]))
        if (length(value) != 1 || is.na(value)) {
          stop(paste0("Contrast weight must be a single numeric value for level: ", name))
        }
        vec[name] <- value
      }
      return(unname(vec))
    }
    numeric_vec <- suppressWarnings(as.numeric(unlist(weights, use.names = FALSE)))
    if (any(is.na(numeric_vec))) stop("Contrast weights must be numeric.")
    if (length(numeric_vec) != length(labels)) {
      stop(paste0(
        "Contrast weight length (", length(numeric_vec),
        ") does not match emmeans rows (", length(labels), ")."
      ))
    }
    return(numeric_vec)
  }
  if (is.atomic(weights)) {
    numeric_vec <- suppressWarnings(as.numeric(weights))
    if (!is.null(names(weights)) && any(nzchar(names(weights)))) {
      vec <- rep(0, length(labels))
      names(vec) <- labels
      for (name in names(weights)) {
        if (!(name %in% labels)) {
          stop(paste0("Unknown contrast level: ", name))
        }
        value <- suppressWarnings(as.numeric(weights[[name]]))
        if (length(value) != 1 || is.na(value)) {
          stop(paste0("Contrast weight must be a single numeric value for level: ", name))
        }
        vec[name] <- value
      }
      return(unname(vec))
    }
    if (any(is.na(numeric_vec))) stop("Contrast weights must be numeric.")
    if (length(numeric_vec) != length(labels)) {
      stop(paste0(
        "Contrast weight length (", length(numeric_vec),
        ") does not match emmeans rows (", length(labels), ")."
      ))
    }
    return(numeric_vec)
  }
  stop("Unsupported contrast weight format.")
}

build_custom_contrast_list <- function(contrast_map, labels) {
  if (is.null(contrast_map)) stop("Contrast JSON is missing 'contrasts'.")
  if (!is.list(contrast_map) || is.null(names(contrast_map)) || any(!nzchar(names(contrast_map)))) {
    stop("Custom contrasts must be an object with named contrasts.")
  }
  out <- list()
  for (name in names(contrast_map)) {
    out[[name]] <- coerce_contrast_weights(contrast_map[[name]], labels)
  }
  out
}

resolve_contrast_spec <- function(contrast_mode, contrast_file) {
  spec <- read_contrast_json(contrast_file)
  if (!is.null(spec)) {
    method_val <- if (!is.null(spec$method)) as.character(spec$method) else ""
    has_method <- nzchar(method_val)
    has_contrasts <- !is.null(spec$contrasts)
    if (has_method && has_contrasts) {
      stop("Contrast JSON must define either 'method' or 'contrasts', not both.")
    }
    if (has_method) {
      return(list(
        mode = "method",
        method = method_val,
        args = if (is.list(spec$args)) spec$args else list(),
        term = if (!is.null(spec$term)) as.character(spec$term) else "",
        label = method_val,
        source = contrast_file
      ))
    }
    if (has_contrasts) {
      return(list(
        mode = "custom",
        contrasts = spec$contrasts,
        term = if (!is.null(spec$term)) as.character(spec$term) else "",
        label = "custom",
        source = contrast_file
      ))
    }
    stop("Contrast JSON must include 'method' or 'contrasts'.")
  }
  if (contrast_mode == "none") return(list(mode = "none", label = "none"))
  if (contrast_mode == "pairwise") {
    return(list(mode = "pairwise", method = "pairwise", args = list(), label = "pairwise"))
  }
  if (contrast_mode == "custom") {
    stop("Custom contrasts require --contrast-file.")
  }
  list(mode = "method", method = contrast_mode, args = list(), label = contrast_mode)
}

build_contrast_method <- function(contrast_spec, emm, term_label) {
  if (is.null(contrast_spec) || contrast_spec$mode == "none") return(NULL)
  if (contrast_spec$mode %in% c("pairwise", "method")) {
    return(list(method = contrast_spec$method, args = contrast_spec$args))
  }
  if (contrast_spec$mode == "custom") {
    labels <- build_emmeans_labels(emm, term_label)
    method_list <- build_custom_contrast_list(contrast_spec$contrasts, labels$labels)
    return(list(method = method_list, args = list()))
  }
  stop(paste0("Unsupported contrast mode: ", contrast_spec$mode))
}

format_contrast_label <- function(contrast_spec) {
  if (is.null(contrast_spec)) return("none")
  label <- if (!is.null(contrast_spec$label)) contrast_spec$label else contrast_spec$mode
  if (!is.null(contrast_spec$source) && nzchar(contrast_spec$source) && label == "custom") {
    label <- paste0("custom (", basename(contrast_spec$source), ")")
  }
  label
}
