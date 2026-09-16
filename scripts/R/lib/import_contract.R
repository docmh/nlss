# SPDX-License-Identifier: Apache-2.0
# Import metadata describes the original source, not the current row order.
# In particular, missing$observations must never be reapplied by row number to a
# transformed dataset. Modelling receives ordinary vectors and ordinary NAs.

import_atomic_value <- function(value) {
  if (length(value) == 0L || is.na(value[1])) return(NULL)
  value <- unname(value[1])
  if (is.numeric(value) && is.infinite(value)) return(as.character(value))
  value
}

import_plain_vector <- function(vec) {
  if (inherits(vec, c("haven_labelled", "haven_labelled_spss", "labelled"))) {
    attributes(vec) <- NULL
  }
  vec
}

import_code_label_map <- function(map, vec) {
  lapply(names(map), function(code) {
    value <- code
    if (is.numeric(vec)) {
      number <- suppressWarnings(as.numeric(code))
      if (!is.na(number)) value <- number
    }
    list(value = import_atomic_value(value), label = as.character(map[[code]]))
  })
}

import_missing_mask <- function(vec, na_values, na_range) {
  values <- import_plain_vector(vec)
  missing <- rep(FALSE, length(values))
  if (length(na_values)) missing <- missing | values %in% na_values
  if (length(na_range) == 2L && is.numeric(values)) {
    in_range <- !is.na(values) & values >= na_range[1] & values <= na_range[2]
    missing <- missing | in_range
  }
  missing & !is.na(values)
}

import_capture_dictionary <- function(df) {
  if (!is.data.frame(df)) stop("Import requires a data frame.")
  if (anyDuplicated(names(df)) || any(!nzchar(names(df)))) {
    stop("Imported column names must be non-empty and unique.")
  }
  contract <- attr(df, "nlss_import_contract", exact = TRUE)
  if (is.null(contract)) {
    contract <- list(schema_version = 1L, source_rows = nrow(df), columns = list())
  }
  if (!is.list(contract) || !identical(as.integer(contract$schema_version), 1L) ||
      !is.list(contract$columns)) stop("Unsupported or invalid NLSS import contract.")
  has_haven <- requireNamespace("haven", quietly = TRUE)
  frame_labels <- attr(df, "nlss_labels", exact = TRUE)
  for (name in names(df)) {
    # Retain source labels and missing provenance after normalizing, filtering,
    # imputing, or reordering data; capture only genuinely new columns.
    if (!is.null(contract$columns[[name]])) next
    vec <- df[[name]]
    plain <- import_plain_vector(vec)
    column <- list(source_type = typeof(plain), source_classes = class(vec))
    for (key in c("label", "variable.label", "var.label")) {
      label <- attr(vec, key, exact = TRUE)
      if (!is.null(label) && length(label)) {
        column$variable_label <- as.character(label[1])
        break
      }
    }
    if (is.null(column$variable_label)) {
      labels <- attr(df, "variable.labels", exact = TRUE)
      if (!is.null(labels) && name %in% names(labels)) {
        column$variable_label <- as.character(labels[[name]])
      } else if (!is.null(frame_labels$variables[[name]])) {
        column$variable_label <- as.character(frame_labels$variables[[name]])
      }
    }
    labels <- attr(vec, "labels", exact = TRUE)
    if (is.null(labels)) labels <- attr(vec, "value.labels", exact = TRUE)
    if (is.null(labels)) {
      df_labels <- attr(df, "value.labels", exact = TRUE)
      if (is.list(df_labels)) labels <- df_labels[[name]]
    }
    if (is.list(labels) && length(labels)) {
      column$value_labels <- import_code_label_map(labels, plain)
    } else if (!is.null(labels) && length(labels)) {
      column$value_labels <- lapply(seq_along(labels), function(i) {
        entry <- list(value = import_atomic_value(labels[i]), label = names(labels)[i])
        if (has_haven && is.double(labels) && haven::is_tagged_na(labels[i])) {
          entry$missing_tag <- haven::na_tag(labels[i])
        } else if (is.na(labels[i])) {
          entry$system_missing <- TRUE
        }
        entry
      })
    } else if (is.list(frame_labels$values[[name]])) {
      # Legacy Parquet stores only the code-to-label map on the data frame.
      # Retain it without guessing a numeric type for genuine string codes.
      column$value_labels <- import_code_label_map(frame_labels$values[[name]], plain)
    }
    if (is.factor(vec)) {
      column$factor_levels <- levels(vec)
      column$ordered <- is.ordered(vec)
    }
    na_values <- attr(vec, "na_values", exact = TRUE)
    na_range <- attr(vec, "na_range", exact = TRUE)
    user_rows <- which(import_missing_mask(vec, na_values, na_range))
    tagged_rows <- if (has_haven && is.double(plain)) which(haven::is_tagged_na(plain)) else integer()
    if (length(na_values) || length(na_range) || length(tagged_rows)) {
      missing <- list(observation_basis = "original_source_rows")
      if (length(na_values)) missing$na_values <- lapply(na_values, import_atomic_value)
      if (length(na_range)) missing$na_range <- lapply(na_range, import_atomic_value)
      missing$observations <- c(
        lapply(user_rows, function(row) {
          list(row = row, kind = "user_defined", value = import_atomic_value(plain[row]))
        }),
        lapply(tagged_rows, function(row) {
          list(row = row, kind = "tagged", tag = haven::na_tag(plain[row]))
        })
      )
      column$missing <- missing
    }
    contract$columns[[name]] <- column
  }
  contract
}

import_prepare_analysis <- function(df, dictionary = NULL) {
  if (!is.null(dictionary)) attr(df, "nlss_import_contract") <- dictionary
  contract <- import_capture_dictionary(df)
  for (name in names(df)) {
    vec <- df[[name]]
    missing <- import_missing_mask(vec, attr(vec, "na_values", exact = TRUE),
                                   attr(vec, "na_range", exact = TRUE))
    vec <- import_plain_vector(vec)
    if (any(missing)) vec[missing] <- NA
    # Tagged NAs have ordinary NA semantics, but Arrow does not preserve their
    # payloads. The exact source tags are therefore retained in the dictionary.
    if (is.double(vec) && requireNamespace("haven", quietly = TRUE)) {
      tagged <- haven::is_tagged_na(vec)
      if (any(tagged)) vec[tagged] <- NA_real_
    }
    for (key in c("label", "labels", "variable.label", "var.label", "value.labels",
                  "na_values", "na_range", "format.spss", "format.stata", "format.sas")) {
      attr(vec, key) <- NULL
    }
    df[[name]] <- vec
  }
  attr(df, "nlss_import_contract") <- contract
  df
}

import_prepare_storage <- function(df) {
  df <- import_prepare_analysis(df)
  contract <- attr(df, "nlss_import_contract", exact = TRUE)
  contract$storage <- list()
  for (name in names(df)) {
    vec <- df[[name]]
    storage <- NULL
    # Explicit doubles avoid Arrow time32 truncation of fractional hms values
    # and timestamp unit rounding. The dictionary records logical R types.
    if (inherits(vec, "POSIXt")) {
      storage <- list(kind = "POSIXct", timezone = attr(vec, "tzone", exact = TRUE))
      df[[name]] <- as.numeric(as.POSIXct(vec))
    } else if (inherits(vec, "Date")) {
      storage <- list(kind = "Date")
      df[[name]] <- as.numeric(vec)
    } else if (inherits(vec, "difftime")) {
      storage <- list(kind = if (inherits(vec, "hms")) "hms" else "difftime",
                      units = as.character(attr(vec, "units", exact = TRUE)))
      df[[name]] <- as.numeric(vec, units = storage$units)
    }
    if (!is.null(storage)) contract$storage[[name]] <- storage
  }
  attr(df, "nlss_import_contract") <- contract
  df
}

import_restore_storage <- function(df, contract = NULL) {
  if (is.null(contract)) contract <- attr(df, "nlss_import_contract", exact = TRUE)
  if (is.null(contract)) return(import_prepare_analysis(df))
  if (!is.list(contract) || !identical(as.integer(contract$schema_version), 1L)) {
    stop("Unsupported or invalid NLSS import contract.")
  }
  for (name in intersect(names(contract$storage), names(df))) {
    storage <- contract$storage[[name]]
    if (!is.numeric(df[[name]]) && !inherits(df[[name]], c("Date", "POSIXt", "difftime"))) {
      stop("Invalid stored temporal column: ", name)
    }
    values <- as.numeric(df[[name]])
    kind <- as.character(unlist(storage$kind, use.names = FALSE))[1]
    if (identical(kind, "POSIXct")) {
      timezone <- as.character(unlist(storage$timezone, use.names = FALSE))
      vec <- structure(values, class = c("POSIXct", "POSIXt"))
      if (length(timezone)) attr(vec, "tzone") <- timezone
    } else if (identical(kind, "Date")) {
      vec <- structure(values, class = "Date")
    } else if (kind %in% c("difftime", "hms")) {
      units <- as.character(unlist(storage$units, use.names = FALSE))[1]
      vec <- structure(values, class = if (kind == "hms") c("hms", "difftime") else "difftime",
                       units = units)
    } else {
      stop("Unsupported stored temporal type: ", kind)
    }
    df[[name]] <- vec
  }
  import_prepare_analysis(df, dictionary = contract)
}

import_csv <- function(path, opts = list()) {
  scalar <- function(name, default) {
    value <- opts[[name]]
    config_keys <- c(sep = "sep", header = "header", `csv-decimal` = "decimal",
                     `csv-encoding` = "encoding", `csv-col-types` = "col_types",
                     `csv-na-values` = "na_values")
    if (is.null(value) && exists("get_config_value", mode = "function")) {
      value <- get_config_value(paste0("defaults.csv.", config_keys[[name]]), default = default)
    }
    if (is.null(value)) return(default)
    if (length(value) != 1L || is.na(value)) stop("Invalid CSV option: --", name)
    as.character(value)
  }
  sep <- scalar("sep", ",")
  decimal <- scalar("csv-decimal", ".")
  encoding <- scalar("csv-encoding", "UTF-8")
  if (!decimal %in% c(".", ",")) stop("--csv-decimal must be '.' or ','.")
  if (nchar(sep) != 1L || identical(sep, decimal)) {
    stop("CSV separator must be one character and differ from the decimal mark.")
  }
  header_text <- tolower(scalar("header", "true"))
  if (!header_text %in% c("true", "false", "1", "0", "yes", "no", "t", "f")) {
    stop("--header must be true or false.")
  }
  header <- header_text %in% c("true", "1", "yes", "t")
  na_text <- scalar("csv-na-values", "NA")
  na_values <- if (nzchar(na_text)) strsplit(na_text, ",", fixed = TRUE)[[1]] else character()
  types_text <- scalar("csv-col-types", "")
  specified <- character()
  if (nzchar(types_text)) {
    specs <- strsplit(types_text, ",", fixed = TRUE)[[1]]
    for (spec in specs) {
      pair <- strsplit(spec, "=", fixed = TRUE)[[1]]
      if (length(pair) != 2L || any(!nzchar(trimws(pair)))) {
        stop("--csv-col-types requires name=type pairs separated by commas.")
      }
      name <- trimws(pair[1])
      if (name %in% names(specified)) stop("Duplicate CSV type declaration: ", name)
      specified[name] <- tolower(trimws(pair[2]))
    }
  }
  allowed <- c("auto", "character", "string", "numeric", "double", "integer", "logical", "date", "factor", "skip")
  if (any(!specified %in% allowed)) stop("Unsupported CSV column type: ", paste(setdiff(specified, allowed), collapse = ", "))
  # Read lexical values first: leading-zero identifiers and large integers must
  # not be irreversibly coerced before the caller can choose a column type.
  df <- withCallingHandlers(
    read.csv(path, sep = sep, header = header, dec = decimal, fileEncoding = encoding,
             stringsAsFactors = FALSE, colClasses = "character", na.strings = na_values,
             check.names = FALSE, row.names = NULL),
    warning = function(w) {
      # Missing final newline is valid CSV; invalid encodings and unfinished
      # quoted fields can otherwise return a silently truncated data frame.
      if (grepl("incomplete final line", conditionMessage(w), fixed = TRUE)) {
        invokeRestart("muffleWarning")
      }
      stop("CSV import could not read all data reliably: ", conditionMessage(w),
           ". Check --csv-encoding and the CSV structure.", call. = FALSE)
    })
  if (anyDuplicated(names(df)) || any(!nzchar(names(df)))) {
    stop("CSV column names must be non-empty and unique.")
  }
  unknown <- setdiff(names(specified), names(df))
  if (length(unknown)) stop("Unknown CSV columns in --csv-col-types: ", paste(unknown, collapse = ", "))
  inferred <- list()
  for (name in names(df)) {
    values <- df[[name]]
    type <- if (name %in% names(specified)) specified[[name]] else "auto"
    if (type == "skip") next
    if (type %in% c("date", "logical", "numeric", "double", "integer")) {
      values[!is.na(values) & !nzchar(trimws(values))] <- NA_character_
    }
    if (type == "auto") {
      observed <- trimws(values)
      observed <- observed[!is.na(observed) & nzchar(observed)]
      leading_zero <- length(observed) && all(grepl("^[0-9]+$", observed)) && any(grepl("^0[0-9]+$", observed))
      if (leading_zero) {
        converted <- values
        inferred[[name]] <- list(type = "character", reason = "leading_zero_identifier")
      } else {
        converted <- type.convert(values, as.is = TRUE, dec = decimal, na.strings = na_values,
                                  numerals = "no.loss")
      }
    } else if (type %in% c("character", "string")) {
      converted <- values
    } else if (type == "factor") {
      converted <- factor(values)
    } else if (type == "date") {
      converted <- as.Date(values, format = "%Y-%m-%d")
      valid <- is.na(values) | (!is.na(converted) & format(converted, "%Y-%m-%d") == values)
      if (!all(valid)) stop("Invalid ISO date in CSV column: ", name)
    } else if (type == "logical") {
      converted <- suppressWarnings(as.logical(values))
      if (any(!is.na(values) & is.na(converted))) stop("Invalid logical value in CSV column: ", name)
    } else {
      lexical <- if (decimal == ",") gsub(",", ".", values, fixed = TRUE) else values
      converted <- suppressWarnings(as.numeric(lexical))
      invalid <- !is.na(values) & is.na(converted) & trimws(values) != "NaN"
      if (any(invalid)) stop("Invalid numeric value in CSV column: ", name)
      if (type == "integer") {
        integer_values <- suppressWarnings(as.integer(converted))
        if (any(!is.na(converted) & (is.na(integer_values) | converted != integer_values))) {
          stop("Non-integer or out-of-range value in CSV column: ", name)
        }
        converted <- integer_values
      }
    }
    df[[name]] <- converted
  }
  skipped <- names(specified)[specified == "skip"]
  if (length(skipped)) df <- df[setdiff(names(df), skipped)]
  contract <- import_capture_dictionary(df)
  contract$csv <- list(separator = sep, decimal = decimal, encoding = encoding, header = header,
                       na_values = as.list(na_values), column_types = as.list(specified),
                       inference = inferred)
  attr(df, "nlss_import_contract") <- contract
  df
}
