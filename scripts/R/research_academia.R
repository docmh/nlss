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
source_lib("utility_contract.R")

print_usage <- function() {
  cat("Research (Academia) utility\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript research_academia.R --query \"topic\"\n")
  cat("  Rscript research_academia.R --query \"topic\" --sources openalex,crossref --top-n 10 --max-per-source 50 --max-total 200\n")
  cat("  Rscript research_academia.R --interactive\n")
  cat("\n")
  cat("Options:\n")
  cat("  --query TEXT           Required. Search topic or query\n")
  cat("  --sources LIST         Sources: openalex,crossref,semantic_scholar (default: config)\n")
  cat("  --year-from YYYY       Optional lower publication year bound\n")
  cat("  --year-to YYYY         Optional upper publication year bound\n")
  cat("  --max-per-source N     Max results per source (default: config)\n")
  cat("  --max-total N          Max unique results overall (default: config)\n")
  cat("  --top-n N              Top results to highlight (default: config)\n")
  cat("  --timeout N            Request timeout seconds (default: config)\n")
  cat("  --template REF         Template path or key (optional)\n")
  cat("  --user-prompt TEXT     Original AI user prompt for logging (optional)\n")
  cat("  --semantic-key TEXT    Semantic Scholar API key (optional; can use env NLSS_SEMANTIC_SCHOLAR_API_KEY)\n")
  cat("  --log TRUE/FALSE       Write analysis_log.jsonl (default: TRUE)\n")
  cat("  --interactive          Prompt for inputs\n")
  cat("  --help                 Show this help\n")
}

research_out_dir <- function() {
  checked_out <- function(path) {
    nlss_utility_check_directory(path)
    ensure_out_dir(path)
  }
  manifest_path <- find_workspace_manifest(getwd())
  if (nzchar(manifest_path)) {
    manifest <- read_workspace_manifest(manifest_path)
    dataset_name <- resolve_dataset_from_cwd(manifest, manifest_path, getwd())
    if (!nzchar(dataset_name) && !is.null(manifest$active_dataset)) dataset_name <- as.character(manifest$active_dataset)
    if (nzchar(dataset_name)) {
      entry <- resolve_manifest_dataset(manifest, dataset_name)
      if (is.null(entry)) stop("The selected workspace dataset does not exist.")
      # Check the unnormalized manifest path: resolution would hide a symlink.
      for (field in c("parquet", "analysis_log", "scratchpad", "nlss_report")) {
        if (is.null(entry[[field]])) next
        candidate <- as.character(entry[[field]])
        if (!is_absolute_path(candidate)) candidate <- file.path(dirname(manifest_path), candidate)
        nlss_utility_check_directory(dirname(candidate))
        break
      }
      return(checked_out(resolve_dataset_dir(entry, dirname(manifest_path))))
    }
    return(checked_out(dirname(manifest_path)))
  }
  checked_out(get_default_out())
}

interactive_options <- function() {
  cat("Interactive input selected.\n")
  opts <- list()
  opts$query <- prompt("Search query/topic")
  opts$sources <- prompt("Sources (openalex,crossref,semantic_scholar)", get_config_value("modules.research_academia.sources"))
  opts$`year-from` <- prompt("Year from (optional)", "")
  opts$`year-to` <- prompt("Year to (optional)", "")
  opts$`max-per-source` <- prompt("Max per source", as.character(get_config_value("modules.research_academia.max_per_source")))
  opts$`max-total` <- prompt("Max total", as.character(get_config_value("modules.research_academia.max_total")))
  opts$`top-n` <- prompt("Top N", as.character(get_config_value("modules.research_academia.top_n")))
  opts$timeout <- prompt("Timeout seconds", as.character(get_config_value("modules.research_academia.timeout")))
  opts$template <- prompt("Template (path or key; blank for default)", "")
  opts$`user-prompt` <- prompt("User prompt (optional)", "")
  opts$`semantic-key` <- prompt("Semantic Scholar API key (optional)", "")
  log_default <- get_config_value("defaults.log")
  opts$log <- prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))
  opts
}

normalize_option <- function(value, name) {
  if (is.null(value)) return("")
  if (is.logical(value)) {
    if (isTRUE(value)) stop("Missing value for --", name)
    return("")
  }
  as.character(value)
}

research_sources_args <- function(args) {
  out <- character(); i <- 1L
  while (i <= length(args)) {
    token <- args[i]
    if (token == "--sources" || startsWith(token, "--sources=")) {
      values <- if (startsWith(token, "--sources=")) substring(token, 11L) else character()
      i <- i + 1L
      while (i <= length(args) && !startsWith(args[i], "--")) { values <- c(values, args[i]); i <- i + 1L }
      if (!length(values) || !any(nzchar(values))) stop("Missing value for --sources.")
      out <- c(out, "--sources", paste(values, collapse = ","))
    } else { out <- c(out, token); i <- i + 1L }
  }
  out
}

split_sources <- function(value) {
  if (is.null(value) || is.logical(value)) return(character(0))
  parts <- as.character(value)
  parts <- parts[!is.na(parts)]
  if (length(parts) == 0) return(character(0))
  tokens <- unlist(lapply(parts, function(part) {
    if (!nzchar(part)) return(character(0))
    strsplit(part, "[,\\s]+", perl = TRUE)[[1]]
  }), use.names = FALSE)
  tokens <- trimws(tokens)
  tokens <- tokens[nzchar(tokens)]
  tokens
}

normalize_source <- function(source) {
  src <- trimws(as.character(source))
  src <- gsub("^['\"]|['\"]$", "", src)
  src <- tolower(gsub("[[:space:]-]+", "_", src))
  if (src %in% c("semantic", "semantic_scholar", "semantic_scholar_api", "semanticscholar")) return("semantic_scholar")
  if (src %in% c("openalex", "open_alex")) return("openalex")
  if (src %in% c("crossref", "cross_ref")) return("crossref")
  src
}

normalize_doi <- function(doi) {
  if (is.null(doi) || !nzchar(doi)) return("")
  text <- tolower(trimws(as.character(doi)))
  text <- sub("^https?://(dx\\.)?doi\\.org/", "", text)
  text <- sub("^doi:\\s*", "", text)
  text
}

normalize_title <- function(title) {
  if (is.null(title) || !nzchar(title)) return("")
  text <- tolower(title)
  text <- gsub("[^a-z0-9 ]+", " ", text)
  text <- gsub("\\s+", " ", text)
  trimws(text)
}

coerce_scalar_text <- function(value) {
  if (is.null(value)) return("")
  if (is.list(value) && length(value) == 1) {
    value <- value[[1]]
  }
  if (is.vector(value) && length(value) > 1) {
    value <- paste(value, collapse = " ")
  }
  value <- as.character(value)
  if (length(value) == 0) return("")
  value <- value[1]
  if (is.na(value)) return("")
  trimws(value)
}

strip_html <- function(text) {
  if (is.null(text) || !nzchar(text)) return("")
  text <- gsub("<[^>]+>", " ", text)
  text <- gsub("&[^;]+;", " ", text)
  text <- gsub("\\s+", " ", text)
  trimws(text)
}

extract_openalex_abstract <- function(index) {
  if (is.null(index) || !is.list(index) || length(index) == 0) return("")
  words <- names(index)
  if (length(words) == 0) return("")
  positions <- integer(0)
  tokens <- character(0)
  for (word in words) {
    locs <- index[[word]]
    if (is.null(locs)) next
    if (is.list(locs)) locs <- unlist(locs, use.names = FALSE)
    if (length(locs) == 0) next
    locs <- suppressWarnings(as.integer(locs))
    locs <- locs[!is.na(locs)]
    if (length(locs) == 0) next
    positions <- c(positions, locs)
    tokens <- c(tokens, rep(word, length(locs)))
  }
  if (length(positions) == 0) return("")
  ord <- order(positions)
  text <- paste(tokens[ord], collapse = " ")
  text <- gsub("\\s+", " ", text)
  trimws(text)
}

extract_keywords_openalex <- function(item, limit = 8) {
  keywords <- character(0)
  if (!is.null(item$keywords) && length(item$keywords) > 0) {
    for (kw in item$keywords) {
      name <- ""
      if (is.list(kw) && !is.null(kw$display_name)) name <- coerce_scalar_text(kw$display_name)
      if (!nzchar(name) && is.character(kw)) name <- coerce_scalar_text(kw)
      if (nzchar(name)) keywords <- c(keywords, name)
    }
  }
  if (length(keywords) == 0 && !is.null(item$concepts) && length(item$concepts) > 0) {
    concept_names <- character(0)
    scores <- numeric(0)
    for (concept in item$concepts) {
      name <- ""
      score <- NA_real_
      if (is.list(concept)) {
        if (!is.null(concept$display_name)) name <- coerce_scalar_text(concept$display_name)
        if (!is.null(concept$score)) score <- suppressWarnings(as.numeric(concept$score))
      } else if (is.character(concept)) {
        name <- coerce_scalar_text(concept)
      }
      if (nzchar(name)) {
        concept_names <- c(concept_names, name)
        scores <- c(scores, ifelse(is.na(score), 0, score))
      }
    }
    if (length(concept_names) > 0) {
      ord <- order(-scores)
      keywords <- concept_names[ord]
    }
  }
  keywords <- unique(keywords)
  keywords <- keywords[nzchar(keywords)]
  if (length(keywords) == 0) return("")
  if (length(keywords) > limit) keywords <- keywords[1:limit]
  paste(keywords, collapse = "; ")
}

extract_keywords_crossref <- function(item, limit = 8) {
  keywords <- character(0)
  if (!is.null(item$subject) && length(item$subject) > 0) {
    keywords <- vapply(item$subject, coerce_scalar_text, character(1))
  }
  keywords <- keywords[nzchar(keywords)]
  if (length(keywords) == 0) return("")
  if (length(keywords) > limit) keywords <- keywords[1:limit]
  paste(keywords, collapse = "; ")
}

extract_keywords_semantic <- function(item, limit = 8) {
  keywords <- character(0)
  if (!is.null(item$fieldsOfStudy) && length(item$fieldsOfStudy) > 0) {
    fields <- item$fieldsOfStudy
    if (is.list(fields) && length(fields) > 0 && is.list(fields[[1]])) {
      for (field in fields) {
        name <- ""
        if (!is.null(field$category)) name <- coerce_scalar_text(field$category)
        if (!nzchar(name) && !is.null(field$name)) name <- coerce_scalar_text(field$name)
        if (nzchar(name)) keywords <- c(keywords, name)
      }
    } else {
      keywords <- vapply(fields, coerce_scalar_text, character(1))
    }
  }
  keywords <- keywords[nzchar(keywords)]
  if (length(keywords) == 0) return("")
  if (length(keywords) > limit) keywords <- keywords[1:limit]
  paste(keywords, collapse = "; ")
}

split_name_tokens <- function(text) {
  tokens <- strsplit(text, "\\s+", perl = TRUE)[[1]]
  tokens[nzchar(tokens)]
}

initials_from_given <- function(given) {
  if (is.null(given) || !nzchar(given)) return("")
  given <- gsub("[^A-Za-z\\-\\s]", "", given)
  tokens <- split_name_tokens(given)
  if (length(tokens) == 0) return("")
  initials <- character(0)
  for (token in tokens) {
    if (!nzchar(token)) next
    parts <- strsplit(token, "-", fixed = TRUE)[[1]]
    parts <- parts[nzchar(parts)]
    if (length(parts) == 0) next
    part_initials <- vapply(parts, function(x) substr(x, 1, 1), character(1))
    initials <- c(initials, paste0(part_initials, ".", collapse = "-"))
  }
  paste(initials, collapse = " ")
}

format_author_name <- function(family, given) {
  family <- trimws(as.character(family))
  given <- trimws(as.character(given))
  if (!nzchar(family) && !nzchar(given)) return("")
  if (!nzchar(family)) return(given)
  if (!nzchar(given)) return(family)
  initials <- initials_from_given(given)
  if (!nzchar(initials)) return(family)
  paste0(family, ", ", initials)
}

format_author_from_display <- function(display_name) {
  if (is.null(display_name) || !nzchar(display_name)) return("")
  name <- trimws(as.character(display_name))
  if (grepl(",", name)) {
    parts <- strsplit(name, ",", fixed = TRUE)[[1]]
    family <- trimws(parts[1])
    given <- trimws(paste(parts[-1], collapse = " "))
    return(format_author_name(family, given))
  }
  tokens <- split_name_tokens(name)
  if (length(tokens) == 0) return("")
  if (length(tokens) == 1) return(tokens[1])
  family <- tokens[length(tokens)]
  given <- paste(tokens[-length(tokens)], collapse = " ")
  format_author_name(family, given)
}

format_author_list <- function(authors) {
  if (is.null(authors) || length(authors) == 0) return("")
  formatted <- vapply(authors, function(author) {
    if (is.list(author)) {
      family <- if (!is.null(author$family)) author$family else ""
      given <- if (!is.null(author$given)) author$given else ""
      display <- if (!is.null(author$display)) author$display else ""
      out <- format_author_name(family, given)
      if (!nzchar(out)) out <- format_author_from_display(display)
      return(out)
    }
    format_author_from_display(author)
  }, character(1))
  formatted <- formatted[nzchar(formatted)]
  if (length(formatted) == 0) return("")
  if (length(formatted) <= 20) {
    if (length(formatted) == 1) return(formatted[1])
    return(paste0(paste(formatted[1:(length(formatted) - 1)], collapse = ", "), ", & ", formatted[length(formatted)]))
  }
  head <- formatted[1:19]
  tail <- formatted[length(formatted)]
  paste0(paste(head, collapse = ", "), ", ..., ", tail)
}

sentence_case <- function(text) {
  if (is.null(text) || !nzchar(text)) return("")
  text <- trimws(as.character(text))
  if (!nzchar(text)) return("")
  first <- substr(text, 1, 1)
  rest <- if (nchar(text) > 1) tolower(substr(text, 2, nchar(text))) else ""
  paste0(toupper(first), rest)
}

format_reference <- function(item) {
  authors <- format_author_list(item$authors)
  year <- if (!is.null(item$year) && nzchar(item$year)) item$year else "n.d."
  title <- sentence_case(item$title)
  source <- if (!is.null(item$source) && nzchar(item$source)) item$source else ""
  volume <- if (!is.null(item$volume) && nzchar(item$volume)) item$volume else ""
  issue <- if (!is.null(item$issue) && nzchar(item$issue)) item$issue else ""
  pages <- if (!is.null(item$pages) && nzchar(item$pages)) item$pages else ""
  doi <- if (!is.null(item$doi) && nzchar(item$doi)) item$doi else ""
  url <- if (!is.null(item$url) && nzchar(item$url)) item$url else ""
  source_block <- ""
  if (nzchar(source)) {
    source_block <- paste0("*", source, "*")
    if (nzchar(volume)) {
      volume_block <- paste0("*", volume, "*")
      if (nzchar(issue)) {
        volume_block <- paste0(volume_block, "(", issue, ")")
      }
      source_block <- paste0(source_block, ", ", volume_block)
    }
    if (nzchar(pages)) {
      source_block <- paste0(source_block, ", ", pages)
    }
    source_block <- paste0(source_block, ".")
  }
  link <- ""
  if (nzchar(doi)) {
    link <- paste0("https://doi.org/", doi)
  } else if (nzchar(url)) {
    link <- url
  }
  parts <- c(
    if (nzchar(authors)) authors else "Unknown author",
    paste0("(", year, ")."),
    if (nzchar(title)) paste0(title, ".") else "",
    source_block
  )
  ref <- paste(parts[nzchar(parts)], collapse = " ")
  if (nzchar(link)) ref <- paste0(ref, " ", link)
  trimws(ref)
}

reference_sort_key <- function(item) {
  if (!is.null(item$authors) && length(item$authors) > 0) {
    author <- item$authors[[1]]
    if (is.list(author) && !is.null(author$family) && nzchar(author$family)) {
      return(tolower(author$family))
    }
    if (is.list(author) && !is.null(author$display) && nzchar(author$display)) {
      tokens <- split_name_tokens(author$display)
      if (length(tokens) > 0) return(tolower(tokens[length(tokens)]))
    }
    if (is.character(author)) {
      tokens <- split_name_tokens(author)
      if (length(tokens) > 0) return(tolower(tokens[length(tokens)]))
    }
  }
  if (!is.null(item$title) && nzchar(item$title)) return(tolower(item$title))
  "zzzz"
}

research_redact <- function(value) {
  if (is.list(value)) {
    for (i in seq_along(value)) {
      key <- if (!is.null(names(value))) names(value)[i] else ""
      value[i] <- list(if (grepl("^(api[-_]?key|x-api-key|authorization|access[-_]?token|token|secret|password|headers)$", key, ignore.case = TRUE)) "<redacted>" else research_redact(value[[i]]))
    }
    if (!is.null(names(value))) names(value) <- research_redact(names(value))
    return(value)
  }
  if (!is.character(value)) return(value)
  for (secret in research_secrets[nzchar(research_secrets)]) value <- gsub(secret, "<redacted>", value, fixed = TRUE)
  value <- gsub("(?i)([?&](?:api[-_]?key|access[-_]?token|token|secret|password)=)[^&#[:space:]]+", "\\1<redacted>", value, perl = TRUE)
  gsub("(https?://)[^/@[:space:]]+:[^/@[:space:]]+@", "\\1<redacted>@", value, perl = TRUE)
}

fetch_json <- function(url, source, timeout, headers = list(), max_retries = 2L) {
  for (attempt in seq_len(max_retries + 1L)) {
    started <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
    status <- NA_integer_; status_observed <- TRUE; body <- ""; error <- ""
    if (transport == "fixture") {
      position <- fixture_position[[source]] + 1L; fixture_position[[source]] <<- position
      packet <- fixtures$sources[[source]]
      if (length(packet) < position) error <- "fixture_exhausted"
      else {
        packet <- packet[[position]]
        status <- if (is.null(packet$status)) NA_integer_ else suppressWarnings(as.integer(packet$status))
        if (length(status) != 1L) status <- NA_integer_
        if (!is.null(packet$body)) body <- jsonlite::toJSON(packet$body, auto_unbox = TRUE, null = "null", digits = NA)
        if (!is.null(packet$text)) body <- as.character(packet$text)[1]
        if (isTRUE(packet$transport_error)) error <- "transport_error"
      }
    } else if (requireNamespace("curl", quietly = TRUE)) {
      response <- tryCatch({
        handle <- curl::new_handle(timeout = timeout, followlocation = FALSE)
        if (length(headers)) curl::handle_setheaders(handle, .list = headers)
        curl::curl_fetch_memory(url, handle = handle)
      }, error = function(e) NULL)
      if (is.null(response)) error <- "transport_error"
      else { status <- response$status_code; body <- rawToChar(response$content) }
    } else {
      if (length(headers) && "x-api-key" %in% names(headers)) stop("Sending an API key requires the R package 'curl'. Use the existing configured environment or install it explicitly.")
      status_observed <- FALSE
      old_timeout <- getOption("timeout"); options(timeout = timeout)
      body <- tryCatch(suppressWarnings({
        connection <- base::url(url, open = "rb", encoding = "UTF-8")
        tryCatch(paste(readLines(connection, warn = FALSE), collapse = "\n"), finally = close(connection))
      }), error = function(e) "", finally = options(timeout = old_timeout))
      if (!nzchar(body)) error <- "transport_error"
    }
    parsed <- tryCatch(jsonlite::fromJSON(body, simplifyVector = FALSE), error = function(e) NULL)
    if (!nzchar(error)) {
      if (status_observed && (is.na(status) || status < 200L || status >= 300L)) error <- if (is.na(status)) "transport_error" else paste0("http_", status)
      else if (!is.list(parsed)) error <- "invalid_json"
    }
    snapshot <- list(source = source, attempt = attempt, transport = transport, started_utc = started,
      finished_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
      endpoint = sub("[?].*$", "", url), status = status, http_status_observed = status_observed,
      error = if (nzchar(error)) error else NULL,
      response = if (is.list(parsed)) research_redact(parsed) else research_redact(body))
    response_snapshots[[length(response_snapshots) + 1L]] <<- snapshot
    if (!is.na(status) && status == 429L && attempt <= max_retries) {
      if (transport == "live") Sys.sleep(c(1, 2, 4)[min(attempt, 3L)])
      next
    }
    return(list(data = if (!nzchar(error)) research_redact(parsed) else NULL, status = status, error = error))
  }
}

fetch_source <- function(source, query, max_results, year_from, year_to, timeout, mailto, api_key, user_agent) {
  items <- list(); cursor <- "*"; seen <- character(); offset <- 0L; error <- ""; exhausted <- FALSE
  limit <- min(if (source == "openalex") 200L else 100L, max_results)
  repeat {
    endpoint <- switch(source,
      openalex = build_openalex_url(query, limit, cursor, year_from, year_to, mailto),
      crossref = build_crossref_url(query, limit, offset, year_from, year_to, mailto),
      semantic_scholar = build_semantic_url(query, limit, offset))
    headers <- if (nzchar(user_agent)) list(`User-Agent` = user_agent) else list()
    if (source == "semantic_scholar" && nzchar(api_key)) headers[["x-api-key"]] <- api_key
    response <- fetch_json(endpoint, source, timeout, headers, if (source == "semantic_scholar") 3L else 2L)
    if (nzchar(response$error)) { error <- response$error; break }
    if ((source == "crossref" && !is.list(response$data$message)) ||
        (source == "openalex" && !is.null(response$data$meta) && !is.list(response$data$meta))) {
      error <- "invalid_response_schema"; break
    }
    next_cursor <- if (source == "openalex") response$data$meta$next_cursor else NULL
    if (!is.null(next_cursor) && (!is.character(next_cursor) || length(next_cursor) != 1L || is.na(next_cursor))) {
      error <- "invalid_response_schema"; break
    }
    batch <- switch(source, openalex = response$data$results, crossref = response$data$message$items, semantic_scholar = response$data$data)
    if (!is.list(batch)) { error <- "invalid_response_schema"; break }
    if (!length(batch)) { exhausted <- TRUE; break }
    parser <- switch(source, openalex = parse_openalex_item, crossref = parse_crossref_item, semantic_scholar = parse_semantic_item)
    parsed <- tryCatch(suppressWarnings(lapply(batch, parser)), error = function(e) NULL)
    valid_item <- function(item) {
      scalar_text <- c("title", "year", "source", "doi", "url", "keywords", "abstract", "volume", "issue", "pages")
      scalar_author <- function(author) is.list(author) && all(vapply(author[c("family", "given", "display")],
        function(x) is.null(x) || (is.character(x) && length(x) == 1L && !is.na(x)), logical(1)))
      all(vapply(item[scalar_text], function(x) is.character(x) && length(x) == 1L && !is.na(x), logical(1))) &&
        is.list(item$authors) && all(vapply(item$authors, scalar_author, logical(1))) &&
        all(vapply(item[c("citations", "relevance_raw")], function(x) is.numeric(x) && length(x) == 1L && (is.na(x) || is.finite(x)), logical(1))) &&
        (is.na(item$citations) || item$citations >= 0)
    }
    if (is.null(parsed) || !all(vapply(parsed, valid_item, logical(1)))) { error <- "invalid_item_schema"; break }
    items <- c(items, head(parsed, max_results - length(items)))
    if (length(items) >= max_results) break
    if (source == "openalex") {
      next_cursor <- coerce_scalar_text(next_cursor)
      if (!nzchar(next_cursor)) { exhausted <- TRUE; break }
      if (next_cursor %in% c(seen, cursor)) { error <- "repeated_cursor"; break }
      seen <- c(seen, cursor); cursor <- next_cursor
    } else {
      if (length(batch) < limit) { exhausted <- TRUE; break }
      offset <- offset + limit
    }
  }
  list(items = items, status = if (nzchar(error)) if (length(items)) "partial" else "failed" else "success",
    error = if (nzchar(error)) error else NULL, received_n = length(items), limit_reached = length(items) >= max_results,
    source_exhausted = exhausted)
}

build_openalex_url <- function(query, per_page, cursor, year_from, year_to, mailto) {
  q <- utils::URLencode(query, reserved = TRUE)
  params <- c(
    paste0("search=", q),
    paste0("per-page=", per_page),
    paste0("cursor=", utils::URLencode(cursor, reserved = TRUE))
  )
  filters <- character(0)
  if (nzchar(year_from) || nzchar(year_to)) {
    from <- if (nzchar(year_from)) paste0(year_from, "-01-01") else ""
    to <- if (nzchar(year_to)) paste0(year_to, "-12-31") else ""
    if (nzchar(from)) filters <- c(filters, paste0("from_publication_date:", from))
    if (nzchar(to)) filters <- c(filters, paste0("to_publication_date:", to))
  }
  if (length(filters) > 0) {
    params <- c(params, paste0("filter=", utils::URLencode(paste(filters, collapse = ","), reserved = TRUE)))
  }
  if (nzchar(mailto)) {
    params <- c(params, paste0("mailto=", utils::URLencode(mailto, reserved = TRUE)))
  }
  paste0("https://api.openalex.org/works?", paste(params, collapse = "&"))
}

parse_openalex_item <- function(item) {
  title <- coerce_scalar_text(if (!is.null(item$title)) item$title else "")
  year <- if (!is.null(item$publication_year)) as.character(item$publication_year) else ""
  doi <- normalize_doi(if (!is.null(item$doi)) item$doi else "")
  url <- if (!is.null(item$id)) item$id else ""
  cited <- if (!is.null(item$cited_by_count)) as.integer(item$cited_by_count) else 0L
  relevance <- if (!is.null(item$relevance_score)) as.numeric(item$relevance_score) else NA_real_
  source_name <- ""
  if (!is.null(item$primary_location$source$display_name)) {
    source_name <- coerce_scalar_text(item$primary_location$source$display_name)
  }
  authors <- list()
  if (!is.null(item$authorships) && length(item$authorships) > 0) {
    for (auth in item$authorships) {
      display <- ""
      family <- ""
      given <- ""
      if (!is.null(auth$author$display_name)) display <- auth$author$display_name
      if (nzchar(display)) {
        authors[[length(authors) + 1]] <- list(display = display)
      } else {
        authors[[length(authors) + 1]] <- list(family = family, given = given)
      }
    }
  }
  keywords <- extract_keywords_openalex(item)
  abstract <- ""
  if (!is.null(item$abstract_inverted_index)) {
    abstract <- extract_openalex_abstract(item$abstract_inverted_index)
  }
  if (!nzchar(abstract) && !is.null(item$abstract)) {
    abstract <- coerce_scalar_text(item$abstract)
  }
  abstract <- strip_html(abstract)
  list(
    source_name = "openalex",
    title = title,
    year = year,
    authors = authors,
    source = source_name,
    doi = doi,
    url = url,
    citations = cited,
    relevance_raw = relevance,
    keywords = keywords,
    abstract = abstract,
    volume = if (!is.null(item$biblio$volume)) as.character(item$biblio$volume) else "",
    issue = if (!is.null(item$biblio$issue)) as.character(item$biblio$issue) else "",
    pages = if (!is.null(item$biblio$first_page) || !is.null(item$biblio$last_page)) {
      first <- if (!is.null(item$biblio$first_page)) as.character(item$biblio$first_page) else ""
      last <- if (!is.null(item$biblio$last_page)) as.character(item$biblio$last_page) else ""
      if (nzchar(first) && nzchar(last)) paste0(first, "-", last) else if (nzchar(first)) first else last
    } else "",
    source_record_id = coerce_scalar_text(item$id)
  )
}

build_crossref_url <- function(query, rows, offset, year_from, year_to, mailto) {
  q <- utils::URLencode(query, reserved = TRUE)
  params <- c(
    paste0("query.bibliographic=", q),
    paste0("rows=", rows),
    paste0("offset=", offset)
  )
  filters <- character(0)
  if (nzchar(year_from)) filters <- c(filters, paste0("from-pub-date:", year_from, "-01-01"))
  if (nzchar(year_to)) filters <- c(filters, paste0("until-pub-date:", year_to, "-12-31"))
  if (length(filters) > 0) {
    params <- c(params, paste0("filter=", utils::URLencode(paste(filters, collapse = ","), reserved = TRUE)))
  }
  if (nzchar(mailto)) {
    params <- c(params, paste0("mailto=", utils::URLencode(mailto, reserved = TRUE)))
  }
  paste0("https://api.crossref.org/works?", paste(params, collapse = "&"))
}

extract_crossref_year <- function(item) {
  if (!is.null(item$issued$`date-parts`) && length(item$issued$`date-parts`) > 0) {
    year <- item$issued$`date-parts`[[1]][[1]]
    if (!is.null(year) && !is.na(year)) return(as.character(year))
  }
  if (!is.null(item$published$`date-parts`) && length(item$published$`date-parts`) > 0) {
    year <- item$published$`date-parts`[[1]][[1]]
    if (!is.null(year) && !is.na(year)) return(as.character(year))
  }
  ""
}

parse_crossref_item <- function(item) {
  title <- ""
  if (!is.null(item$title) && length(item$title) > 0) title <- coerce_scalar_text(item$title[[1]])
  year <- extract_crossref_year(item)
  doi <- normalize_doi(if (!is.null(item$DOI)) item$DOI else "")
  url <- if (!is.null(item$URL)) item$URL else ""
  cited <- if (!is.null(item$`is-referenced-by-count`)) as.integer(item$`is-referenced-by-count`) else 0L
  relevance <- if (!is.null(item$score)) as.numeric(item$score) else NA_real_
  source_name <- ""
  if (!is.null(item$`container-title`) && length(item$`container-title`) > 0) {
    source_name <- coerce_scalar_text(item$`container-title`[[1]])
  }
  authors <- list()
  if (!is.null(item$author) && length(item$author) > 0) {
    for (auth in item$author) {
      family <- if (!is.null(auth$family)) auth$family else ""
      given <- if (!is.null(auth$given)) auth$given else ""
      authors[[length(authors) + 1]] <- list(family = family, given = given)
    }
  }
  keywords <- extract_keywords_crossref(item)
  abstract <- strip_html(coerce_scalar_text(if (!is.null(item$abstract)) item$abstract else ""))
  list(
    source_name = "crossref",
    title = title,
    year = year,
    authors = authors,
    source = source_name,
    doi = doi,
    url = url,
    citations = cited,
    relevance_raw = relevance,
    keywords = keywords,
    abstract = abstract,
    volume = if (!is.null(item$volume)) as.character(item$volume) else "",
    issue = if (!is.null(item$issue)) as.character(item$issue) else "",
    pages = coerce_scalar_text(if (!is.null(item$page)) item$page else "")
  )
}

build_semantic_url <- function(query, limit, offset) {
  q <- utils::URLencode(query, reserved = TRUE)
  fields <- utils::URLencode("title,authors,year,venue,externalIds,citationCount,url,abstract,fieldsOfStudy", reserved = TRUE)
  paste0("https://api.semanticscholar.org/graph/v1/paper/search?query=", q,
         "&limit=", limit, "&offset=", offset, "&fields=", fields)
}

parse_semantic_item <- function(item) {
  title <- coerce_scalar_text(if (!is.null(item$title)) item$title else "")
  year <- coerce_scalar_text(if (!is.null(item$year)) item$year else "")
  doi <- ""
  if (!is.null(item$externalIds$DOI)) doi <- normalize_doi(item$externalIds$DOI)
  url <- if (!is.null(item$url)) item$url else ""
  cited <- if (!is.null(item$citationCount)) as.integer(item$citationCount) else 0L
  source_name <- coerce_scalar_text(if (!is.null(item$venue)) item$venue else "")
  authors <- list()
  if (!is.null(item$authors) && length(item$authors) > 0) {
    for (auth in item$authors) {
      display <- if (!is.null(auth$name)) auth$name else ""
      authors[[length(authors) + 1]] <- list(display = display)
    }
  }
  keywords <- extract_keywords_semantic(item)
  abstract <- strip_html(coerce_scalar_text(if (!is.null(item$abstract)) item$abstract else ""))
  list(
    source_name = "semantic_scholar",
    title = title,
    year = year,
    authors = authors,
    source = source_name,
    doi = doi,
    url = url,
    citations = cited,
    relevance_raw = NA_real_,
    keywords = keywords,
    abstract = abstract,
    volume = "",
    issue = "",
    pages = ""
  )
}

dedupe_items <- function(items) {
  if (length(items) == 0) return(list())
  seen <- new.env(parent = emptyenv())
  out <- list()
  for (item in items) {
    doi_key <- normalize_doi(item$doi)
    title_key <- normalize_title(item$title)
    key <- if (nzchar(doi_key)) paste0("doi:", doi_key) else if (nzchar(title_key)) paste0("title:", title_key) else ""
    if (!nzchar(key)) next
    if (exists(key, envir = seen, inherits = FALSE)) {
      idx <- get(key, envir = seen, inherits = FALSE)
      current <- out[[idx]]
      current_score <- if (!is.na(current$relevance_raw)) current$relevance_raw else current$citations
      new_score <- if (!is.na(item$relevance_raw)) item$relevance_raw else item$citations
      if (is.na(current_score)) current_score <- 0
      if (is.na(new_score)) new_score <- 0
      if (new_score > current_score) out[[idx]] <- item
    } else {
      out[[length(out) + 1]] <- item
      assign(key, length(out), envir = seen)
    }
  }
  out
}

compute_relevance <- function(items, query, year_from, year_to) {
  if (length(items) == 0) return(items)
  tokens <- unique(regmatches(tolower(query), gregexpr("[\\p{L}\\p{N}_]+", tolower(query), perl = TRUE))[[1]])
  tokens <- tokens[nzchar(tokens)]
  if (length(tokens) == 0) tokens <- character(0)
  source_names <- vapply(items, function(x) x$source_name, character(1))
  raw_scores <- vapply(items, function(x) if (!is.null(x$relevance_raw)) x$relevance_raw else NA_real_, numeric(1))
  source_max <- tapply(raw_scores, source_names, function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE))
  cites <- vapply(items, function(x) if (!is.null(x$citations)) as.numeric(x$citations) else 0, numeric(1))
  max_cites <- max(c(0, cites[is.finite(cites)]))
  years <- suppressWarnings(as.integer(vapply(items, function(x) if (!is.null(x$year)) x$year else NA_character_, character(1))))
  year_vals <- years[!is.na(years)]
  year_min <- if (length(year_vals) > 0) min(year_vals) else NA_integer_
  year_max <- if (length(year_vals) > 0) max(year_vals) else NA_integer_
  if (nzchar(year_from)) year_min <- suppressWarnings(as.integer(year_from))
  if (nzchar(year_to)) year_max <- suppressWarnings(as.integer(year_to))
  for (i in seq_along(items)) {
    item <- items[[i]]
    source <- item$source_name
    raw <- item$relevance_raw
    max_raw <- if (!is.null(source_max[[source]])) source_max[[source]] else NA_real_
    source_norm <- if (!is.na(raw) && !is.na(max_raw) && max_raw > 0) raw / max_raw else 0.5
    title_text <- tolower(paste(coerce_scalar_text(item$title), coerce_scalar_text(item$abstract), sep = " "))
    match_ratio <- 0
    if (length(tokens) > 0 && nzchar(title_text)) {
      words <- regmatches(title_text, gregexpr("[\\p{L}\\p{N}_]+", title_text, perl = TRUE))[[1]]
      hits <- tokens %in% words
      match_ratio <- sum(hits) / length(tokens)
    }
    cite_norm <- if (!is.na(item$citations) && max_cites > 0) log1p(item$citations) / log1p(max_cites) else 0
    recency_norm <- 0
    if (!is.na(year_min) && !is.na(year_max) && !is.na(years[i]) && year_max > year_min) {
      recency_norm <- (years[i] - year_min) / (year_max - year_min)
    }
    relevance <- 0.5 * source_norm + 0.2 * match_ratio + 0.2 * cite_norm + 0.1 * recency_norm
    items[[i]]$relevance <- relevance
    items[[i]]$match_ratio <- match_ratio
    items[[i]]$source_norm <- source_norm
  }
  items
}

truncate_text <- function(text, limit) {
  if (is.null(text) || !nzchar(text)) return("")
  text <- as.character(text)
  if (nchar(text) <= limit) return(text)
  paste0(substr(text, 1, limit - 3), "...")
}

build_table_body <- function(items_df, digits, table_spec = NULL) {
  default_cols <- list(
    list(key = "rank", label = "Rank"),
    list(key = "title", label = "Title"),
    list(key = "year", label = "Year"),
    list(key = "authors", label = "Authors"),
    list(key = "source", label = "Source"),
    list(key = "citations", label = "Cited By"),
    list(key = "link", label = "DOI/URL"),
    list(key = "relevance", label = "Relevance"),
    list(key = "keywords", label = "Keywords", drop_if_empty = TRUE),
    list(key = "abstract", label = "Abstract", drop_if_empty = TRUE)
  )
  columns <- default_cols
  if (!is.null(table_spec) && !is.null(table_spec$columns)) {
    columns <- normalize_table_columns(table_spec$columns, default_cols)
  }
  rows <- list()
  if (nrow(items_df) > 0) {
    for (i in seq_len(nrow(items_df))) {
      row <- items_df[i, , drop = FALSE]
      values <- vapply(columns, function(col) {
        key <- col$key
        value <- if (!is.null(row[[key]])) row[[key]] else ""
        as_cell_text(value)
      }, character(1))
      rows[[length(rows) + 1]] <- values
    }
  }
  adjusted <- drop_empty_columns(columns, rows)
  headers <- vapply(adjusted$columns, function(col) col$label, character(1))
  body <- render_markdown_table(headers, adjusted$rows)
  list(body = body, columns = adjusted$columns)
}

format_item_table_row <- function(item, rank, digits, abstract_limit, keywords_limit) {
  link <- ""
  if (nzchar(item$doi)) link <- paste0("https://doi.org/", item$doi) else if (nzchar(item$url)) link <- item$url
  list(
    rank = as.character(rank),
    title = truncate_text(coerce_scalar_text(item$title), 140),
    year = item$year,
    authors = truncate_text(format_author_list(item$authors), 80),
    source = truncate_text(coerce_scalar_text(item$source), 60),
    citations = if (!is.null(item$citations)) as.character(item$citations) else "",
    link = truncate_text(link, 60),
    relevance = format(round(item$relevance, digits), nsmall = digits, trim = TRUE),
    keywords = truncate_text(coerce_scalar_text(item$keywords), keywords_limit),
    abstract = truncate_text(coerce_scalar_text(item$abstract), abstract_limit)
  )
}

format_most_relevant_sections <- function(items, digits) {
  if (length(items) == 0) return("No relevant items found.")
  lines <- c("# Most Relevant", "")
  for (item in items) {
    title <- coerce_scalar_text(item$title)
    if (!nzchar(title)) title <- "Untitled"
    authors <- format_author_list(item$authors)
    if (!nzchar(authors)) authors <- "Unknown author"
    year <- coerce_scalar_text(item$year)
    if (!nzchar(year)) year <- "n.d."
    journal <- coerce_scalar_text(item$source)
    if (!nzchar(journal)) journal <- "Unknown source"
    cited_by <- if (!is.null(item$citations)) as.character(item$citations) else ""
    if (!nzchar(cited_by)) cited_by <- "n/a"
    relevance <- if (!is.null(item$relevance)) format(round(item$relevance, digits), nsmall = digits, trim = TRUE) else "n/a"
    keywords <- coerce_scalar_text(item$keywords)
    if (!nzchar(keywords)) keywords <- "Not available."
    abstract <- coerce_scalar_text(item$abstract)
    if (!nzchar(abstract)) abstract <- "Not available."
    lines <- c(
      lines,
      paste0("## ", title),
      "",
      paste0(authors, ", ", year, ", ", journal),
      "",
      paste0("Cited by ", cited_by, ", Relevance: ", relevance),
      "",
      paste0("Keywords: ", keywords),
      "",
      paste0("**Abstract** ", abstract),
      ""
    )
  }
  paste(lines, collapse = "\n")
}

format_source_label <- function(sources) {
  labels <- vapply(sources, function(src) {
    if (src == "openalex") return("OpenAlex")
    if (src == "crossref") return("Crossref")
    if (src == "semantic_scholar") return("Semantic Scholar")
    src
  }, character(1))
  paste(labels, collapse = ", ")
}

args <- commandArgs(trailingOnly = TRUE)
if (nzchar(Sys.getenv("NLSS_REPLAY_REQUEST", ""))) stop("Literature retrieval is a utility snapshot, not a statistical replay.")
opts <- parse_args(research_sources_args(args), module = "research_academia")
if (parse_bool(opts$help, FALSE) || !length(args)) { print_usage(); quit(status = 0L) }
if (parse_bool(opts$interactive, FALSE)) opts <- interactive_options()
semantic_key <- normalize_option(opts$`semantic-key`, "semantic-key")
if (!nzchar(semantic_key)) semantic_key <- Sys.getenv("NLSS_SEMANTIC_SCHOLAR_API_KEY", "")
if (!nzchar(semantic_key)) semantic_key <- Sys.getenv("SEMANTIC_SCHOLAR_API_KEY", "")
research_secrets <- unique(c(semantic_key, Sys.getenv("NLSS_SEMANTIC_SCHOLAR_API_KEY", ""), Sys.getenv("SEMANTIC_SCHOLAR_API_KEY", "")))
query <- normalize_option(opts$query, "query")
if (!nzchar(query)) query <- normalize_option(opts$topic, "topic")
if (!nzchar(trimws(query))) stop("Missing --query. Use --help for usage.")
query <- research_redact(query)
sources_text <- normalize_option(opts$sources, "sources")
if (!nzchar(sources_text)) sources_text <- get_config_value("modules.research_academia.sources")
sources <- unique(vapply(split_sources(sources_text), normalize_source, character(1)))
if (!length(sources) || any(!sources %in% c("openalex", "crossref", "semantic_scholar"))) stop("Invalid --sources: use openalex, crossref or semantic_scholar.")
sources_report <- sources
integer_option <- function(flag, key, minimum = 1L) {
  value <- if (is.null(opts[[flag]])) get_config_value(key) else opts[[flag]]
  number <- suppressWarnings(as.numeric(value))
  if (length(number) != 1L || !is.finite(number) || number != floor(number) || number < minimum || number > .Machine$integer.max) stop("Invalid --", flag, ": expected a bounded whole number.")
  as.integer(number)
}
year_option <- function(flag) {
  value <- normalize_option(opts[[flag]], flag)
  if (nzchar(value) && (!grepl("^[0-9]{4}$", value) || as.integer(value) < 1000L)) stop("Invalid --", flag, ": expected a four-digit publication year.")
  value
}
year_from <- year_option("year-from"); year_to <- year_option("year-to")
if (nzchar(year_from) && nzchar(year_to) && as.integer(year_from) > as.integer(year_to)) stop("--year-from must not exceed --year-to.")
max_per_source <- integer_option("max-per-source", "modules.research_academia.max_per_source")
max_total <- integer_option("max-total", "modules.research_academia.max_total")
top_n <- top_n_requested <- integer_option("top-n", "modules.research_academia.top_n")
timeout <- integer_option("timeout", "modules.research_academia.timeout")
abstract_limit <- integer_option("abstract-limit", "modules.research_academia.abstract_limit")
keywords_limit <- integer_option("keywords-limit", "modules.research_academia.keywords_limit")
log_enabled <- parse_bool(opts$log, get_config_value("defaults.log"))
template_override <- resolve_template_override(opts$template, module = "research_academia")
if (!is.null(opts$template) && nzchar(as.character(opts$template)) && is.null(template_override)) stop("The requested research template could not be resolved.")
template_path <- if (!is.null(template_override)) template_override else resolve_template_path("research_academia.default", "research-academia/default-template.md")
if (is.null(template_path) || !file.exists(template_path)) stop("The research template is unavailable.")
template_meta <- get_template_meta(template_path)
mailto <- Sys.getenv("NLSS_CONTACT_EMAIL", "")
user_agent <- if (nzchar(mailto)) paste0("NLSS/1.0 (mailto:", mailto, ")") else "NLSS/1.0"
fixture_path <- Sys.getenv("NLSS_RESEARCH_FIXTURES", "")
transport <- if (nzchar(fixture_path)) "fixture" else "live"
fixtures <- NULL
if (transport == "fixture") {
  fixtures <- tryCatch(jsonlite::fromJSON(fixture_path, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(fixtures) || !identical(fixtures$schema_version, 1L) || !is.list(fixtures$sources)) stop("Invalid literature fixture document; no live fallback is allowed.")
}
fixture_position <- setNames(as.list(rep(0L, 3L)), c("openalex", "crossref", "semantic_scholar"))
response_snapshots <- list(); all_items <- list(); source_status <- list()
for (source in sources) {
  first_response <- length(response_snapshots) + 1L
  fetched <- fetch_source(source, query, max_per_source, year_from, year_to, timeout, mailto, semantic_key, user_agent)
  all_items <- c(all_items, fetched$items)
  fetched$items <- NULL; fetched$response_indices <- seq.int(first_response, length(response_snapshots))
  source_status[[source]] <- fetched
}
source_states <- vapply(source_status, function(x) x$status, character(1))
lookup_status <- if (all(source_states == "success")) "success" else if (any(source_states != "failed")) "partial" else "failed"
raw_count <- length(all_items)
year_filter_excluded <- list()
if (nzchar(year_from) || nzchar(year_to)) {
  retain <- vapply(all_items, function(item) {
    year <- suppressWarnings(as.integer(item$year))
    length(year) == 1L && !is.na(year) && (!nzchar(year_from) || year >= as.integer(year_from)) && (!nzchar(year_to) || year <= as.integer(year_to))
  }, logical(1))
  year_filter_excluded <- all_items[!retain]; all_items <- all_items[retain]
}

deduped <- dedupe_items(all_items)
deduped <- compute_relevance(deduped, query, year_from, year_to)

if (length(deduped) == 0) {
  cat(if (lookup_status == "success") "Successful retrieval retained no matching results.\n" else "No usable results retained; one or more scholarly sources failed.\n")
}

relevance_vec <- vapply(deduped, function(x) x$relevance, numeric(1))
year_vec <- suppressWarnings(as.integer(vapply(deduped, function(x) if (!is.null(x$year)) x$year else NA_character_, character(1))))
year_vec[is.na(year_vec)] <- -Inf
title_vec <- vapply(deduped, function(x) if (!is.null(x$title)) x$title else "", character(1))
deduped <- deduped[order(-relevance_vec, -year_vec, title_vec)]
if (length(deduped) > max_total) deduped <- deduped[1:max_total]

top_n <- min(top_n, length(deduped))
top_items <- if (top_n > 0) deduped[1:top_n] else list()

comprehensive_rows <- list()
for (i in seq_along(deduped)) {
  comprehensive_rows[[length(comprehensive_rows) + 1]] <- format_item_table_row(deduped[[i]], i, get_config_value("defaults.digits"), abstract_limit, keywords_limit)
}
top_rows <- list()
for (i in seq_along(top_items)) {
  top_rows[[length(top_rows) + 1]] <- format_item_table_row(top_items[[i]], i, get_config_value("defaults.digits"), abstract_limit, keywords_limit)
}

comprehensive_df <- if (length(comprehensive_rows) > 0) do.call(rbind, lapply(comprehensive_rows, as.data.frame, stringsAsFactors = FALSE)) else data.frame()
top_df <- if (length(top_rows) > 0) do.call(rbind, lapply(top_rows, as.data.frame, stringsAsFactors = FALSE)) else data.frame()

analysis_label <- "Research (Academia)"
out_dir <- research_out_dir()
report_path <- file.path(out_dir, "report_canonical.md")

table_spec <- NULL
top_spec <- NULL
if (!is.null(template_meta$tables$comprehensive)) table_spec <- template_meta$tables$comprehensive
if (is.null(table_spec) && !is.null(template_meta$table)) table_spec <- template_meta$table
if (!is.null(template_meta$tables$top)) top_spec <- template_meta$tables$top

digits <- get_config_value("defaults.digits")
comprehensive_table <- build_table_body(comprehensive_df, digits, table_spec)
top_table <- build_table_body(top_df, digits, top_spec)

search_date <- format(Sys.Date(), "%Y-%m-%d")
source_label <- format_source_label(sources_report)
comprehensive_note <- paste0("Results aggregated from ", source_label, " on ", search_date, "; deduplicated by DOI/title. Relevance is heuristic.")
top_note <- if (length(deduped) == 0) {
  "No results to rank."
} else {
  paste0("Top ", top_n, " results sorted by relevance score.")
}
nlss_table <- paste0("Table 1\n\n", comprehensive_table$body, "\n", "Note. ", comprehensive_note)

if (length(deduped) == 0) {
  narrative_text <- paste0("Query: \"", query, "\". No usable records were retained. Retrieval status: ", lookup_status,
    "; consult the per-source status before interpreting an empty result.")
} else {
  narrative_text <- paste0("Query: \"", query, "\". Retrieved ", length(deduped),
                           " unique items across ", length(sources_report), " source", ifelse(length(sources_report) == 1, "", "s"),
                           ". The highest-ranked items are highlighted below; ranking is a search heuristic.")
}

sorted_refs <- deduped
if (length(sorted_refs) > 0) {
  keys <- vapply(sorted_refs, reference_sort_key, character(1))
  sorted_refs <- sorted_refs[order(keys)]
}
build_references_text <- function(items) {
  if (length(items) == 0) return("No references found.")
  refs <- items
  keys <- vapply(refs, reference_sort_key, character(1))
  refs <- refs[order(keys)]
  out <- vapply(refs, format_reference, character(1))
  out <- out[nzchar(out)]
  if (length(out) == 0) return("No references found.")
  paste(out, collapse = "\n\n")
}

references_text <- build_references_text(sorted_refs)
references_top_text <- build_references_text(top_items)

table_start <- as.integer(get_next_table_number(report_path))
template_context <- list(
  tokens = list(
    comprehensive_table_body = comprehensive_table$body,
    top_table_body = top_table$body,
    comprehensive_note_body = comprehensive_note,
    top_note_body = top_note,
    table_number_next = as.character(table_start + 1),
    references = references_text,
    references_top = references_top_text,
    most_relevant_sections = format_most_relevant_sections(top_items, digits)
  )
)

analysis_flags <- list(
  query = query,
  sources = sources_report,
  `year-from` = if (nzchar(year_from)) year_from else NULL,
  `year-to` = if (nzchar(year_to)) year_to else NULL,
  `max-per-source` = max_per_source,
  `max-total` = max_total,
  `top-n` = top_n,
  timeout = timeout
)

results_payload <- list(
  status = lookup_status, query = query, total_results = length(deduped), top_n = top_n,
  sources = sources_report, source_status = source_status, raw_count = raw_count,
  transport = transport, year_filter_excluded = year_filter_excluded,
  items = deduped, replay_eligible = FALSE, completion_kind = "literature_retrieval_snapshot"
)
options_payload <- c(analysis_flags, list(top_n_requested = top_n_requested,
  abstract_limit = abstract_limit, keywords_limit = keywords_limit, digits = digits))
request_payload <- list(query = query, options = options_payload, transport = transport,
  template = list(snapshot = "template.md", redaction = "Credential-bearing text is removed before preservation."),
  ranking = "0.5 source-normalized API score + 0.2 query-word overlap + 0.2 log citation count + 0.1 recency; heuristic, not study quality.",
  year_filter = "Applied to all sources after retrieval; missing publication years are excluded when a bound is requested.",
  user_prompt = research_redact(get_user_prompt(opts)))
notice <- paste0("\n\n## Retrieval status and limitations\n\nStatus: ", lookup_status, ". ",
  paste(vapply(names(source_status), function(source) {
    x <- source_status[[source]]
    paste0(source, ": ", x$status, " (", x$received_n, " records",
      if (!is.null(x$error)) paste0("; ", x$error) else "", if (isTRUE(x$limit_reached)) "; configured limit reached" else "", ")")
  }, character(1)), collapse = "; "), ".\n\n",
  "This is a bounded literature search, not proof of exhaustive coverage, study quality or statistical reproducibility. ",
  "Relevance is heuristic. A failed source is not evidence that no literature exists. ",
  "Interpret and verify the original studies when writing the semantic research report.",
  if (transport == "fixture") "\n\nFixture transport: offline test evidence only, not a live literature search." else "", "\n")
template_text <- research_redact(paste(readLines(template_path, warn = FALSE), collapse = "\n"))
snapshot_template <- tempfile("nlss-research-template-", fileext = ".md")
writeLines(template_text, snapshot_template, useBytes = TRUE)
render_report <- function(path, frozen_template) {
  context <- template_context
  context$tokens$table_number_next <- as.character(get_next_table_number(path) + 1L)
  append_nlss_report(path, analysis_label, research_redact(nlss_table), research_redact(narrative_text), analysis_flags = research_redact(analysis_flags),
    template_path = frozen_template, template_context = research_redact(context))
  cat(notice, file = path, append = TRUE)
  record_report_block(notice)
}
preview <- tempfile("nlss-research-output-", fileext = ".md")
render_report(preview, snapshot_template)
output <- research_redact(paste(readLines(preview, warn = FALSE), collapse = "\n"))
invisible(consume_report_blocks())
publication <- nlss_publish_utility("research_academia", out_dir,
  research_redact(request_payload), research_redact(results_payload), output,
  status = switch(lookup_status, success = "completed", partial = "partial", failed = "failed"),
  artifacts = list("responses.json" = import_json(research_redact(response_snapshots)),
    "template.md" = template_text),
  publish = function(run_id, staging) {
    if (lookup_status != "failed") render_report(report_path, file.path(staging, "template.md"))
    if (log_enabled) {
      recorded <- research_redact(results_payload)
      recorded$utility_run_id <- run_id
      safe_commands <- c("Rscript", "research_academia.R", "--query", query, "--sources", paste(sources, collapse = ","))
      logged <- append_analysis_log(out_dir, "research-academia", paste(safe_commands, collapse = " "), safe_commands,
        recorded, options = research_redact(options_payload), user_prompt = research_redact(get_user_prompt(opts)))
      if (resolve_logging_bool("enabled", TRUE) && !isTRUE(logged)) stop("Research compatibility log publication failed.")
    }
  })
cat("Literature retrieval: ", lookup_status, "; ", length(deduped), " unique items. Audit: ",
  file.path("utility-runs", publication$run_id), "\n", sep = "")
if (lookup_status == "failed") stop("All requested scholarly sources failed; inspect the preserved retrieval-status audit. No successful empty search is claimed.", call. = FALSE)
if (lookup_status == "partial") warning("Some scholarly sources failed or returned incomplete pages; retained results are a partial search.", call. = FALSE)
