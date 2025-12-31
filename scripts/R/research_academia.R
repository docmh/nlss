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
source(file.path(bootstrap_dir, "lib", "paths.R"))
source_lib("cli.R")
source_lib("config.R")
source_lib("io.R")
source_lib("formatting.R")

# Static analysis aliases for source_lib-defined functions.
get_user_prompt <- get("get_user_prompt", mode = "function")
parse_args <- get("parse_args", mode = "function")
parse_bool <- get("parse_bool", mode = "function")
parse_list <- get("parse_list", mode = "function")
prompt <- get("prompt", mode = "function")
source_lib <- get("source_lib", mode = "function")

print_usage <- function() {
  cat("Research (Academia) utility (web-required)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript research_academia.R --query \"topic\" --web TRUE\n")
  cat("  Rscript research_academia.R --query \"topic\" --sources openalex,crossref --top-n 10 --max-per-source 50 --max-total 200 --web TRUE\n")
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
  cat("  --web TRUE/FALSE       Require web search (default: env NLSS_WEB_SEARCH)\n")
  cat("  --interactive          Prompt for inputs\n")
  cat("  --help                 Show this help\n")
}

resolve_config_value <- function(path, default = NULL) {
  if (exists("get_config_value", mode = "function")) {
    return(get("get_config_value", mode = "function")(path, default = default))
  }
  default
}

resolve_get_default_out <- function() {
  if (exists("get_default_out", mode = "function")) {
    return(get("get_default_out", mode = "function")())
  }
  "./outputs/tmp"
}

resolve_ensure_out_dir <- function(path) {
  if (exists("ensure_out_dir", mode = "function")) {
    return(get("ensure_out_dir", mode = "function")(path))
  }
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  path
}

resolve_find_workspace_manifest <- function(base_dir = getwd()) {
  if (exists("find_workspace_manifest", mode = "function")) {
    return(get("find_workspace_manifest", mode = "function")(base_dir))
  }
  ""
}

resolve_read_workspace_manifest <- function(path) {
  if (exists("read_workspace_manifest", mode = "function")) {
    return(get("read_workspace_manifest", mode = "function")(path))
  }
  NULL
}

resolve_resolve_dataset_from_cwd <- function(manifest, manifest_path, cwd = getwd()) {
  if (exists("resolve_dataset_from_cwd", mode = "function")) {
    return(get("resolve_dataset_from_cwd", mode = "function")(manifest, manifest_path, cwd))
  }
  ""
}

resolve_resolve_manifest_dataset <- function(manifest, dataset_name) {
  if (exists("resolve_manifest_dataset", mode = "function")) {
    return(get("resolve_manifest_dataset", mode = "function")(manifest, dataset_name))
  }
  NULL
}

resolve_resolve_dataset_dir <- function(entry, workspace_root) {
  if (exists("resolve_dataset_dir", mode = "function")) {
    return(get("resolve_dataset_dir", mode = "function")(entry, workspace_root))
  }
  ""
}

resolve_get_research_out_dir <- function() {
  manifest_path <- resolve_find_workspace_manifest(getwd())
  if (nzchar(manifest_path)) {
    manifest <- resolve_read_workspace_manifest(manifest_path)
    if (!is.null(manifest)) {
      dataset_name <- resolve_resolve_dataset_from_cwd(manifest, manifest_path, getwd())
      if (!nzchar(dataset_name) && !is.null(manifest$active_dataset)) {
        dataset_name <- as.character(manifest$active_dataset)
      }
      if (nzchar(dataset_name)) {
        entry <- resolve_resolve_manifest_dataset(manifest, dataset_name)
        if (!is.null(entry)) {
          workspace_root <- dirname(manifest_path)
          dataset_dir <- resolve_resolve_dataset_dir(entry, workspace_root)
          if (nzchar(dataset_dir)) return(resolve_ensure_out_dir(dataset_dir))
        }
      }
    }
  }
  resolve_ensure_out_dir(resolve_get_default_out())
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

resolve_append_nlss_report <- function(path, analysis_label, nlss_table, nlss_text, analysis_flags = NULL, template_path = NULL, template_context = NULL) {
  if (exists("append_nlss_report", mode = "function")) {
    return(get("append_nlss_report", mode = "function")(
      path,
      analysis_label,
      nlss_table,
      nlss_text,
      analysis_flags = analysis_flags,
      template_path = template_path,
      template_context = template_context
    ))
  }
  stop("Missing report formatter. Ensure lib/formatting.R is sourced.")
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
    if (is.null(override_impl)) return(NULL)
    override_impl(template_ref, module = module)
  }
})

resolve_get_template_path <- function(key, default_relative = NULL) {
  if (exists("resolve_template_path", mode = "function")) {
    return(get("resolve_template_path", mode = "function")(key, default_relative))
  }
  NULL
}

resolve_get_next_table_number <- function(path) {
  if (exists("get_next_table_number", mode = "function")) {
    return(get("get_next_table_number", mode = "function")(path))
  }
  1
}

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
  if (is.null(value) || length(value) == 0 || is.na(value)) return("")
  as.character(value)
}

interactive_options <- function() {
  cat("Interactive input selected.\n")
  opts <- list()
  opts$query <- prompt("Search query/topic")
  opts$sources <- prompt("Sources (openalex,crossref,semantic_scholar)", resolve_config_value("modules.research_academia.sources", "openalex,crossref"))
  opts$`year-from` <- prompt("Year from (optional)", "")
  opts$`year-to` <- prompt("Year to (optional)", "")
  opts$`max-per-source` <- prompt("Max per source", as.character(resolve_config_value("modules.research_academia.max_per_source", 50)))
  opts$`max-total` <- prompt("Max total", as.character(resolve_config_value("modules.research_academia.max_total", 200)))
  opts$`top-n` <- prompt("Top N", as.character(resolve_config_value("modules.research_academia.top_n", 10)))
  opts$timeout <- prompt("Timeout seconds", as.character(resolve_config_value("modules.research_academia.timeout", 30)))
  opts$template <- prompt("Template (path or key; blank for default)", "")
  opts$`user-prompt` <- prompt("User prompt (optional)", "")
  opts$`semantic-key` <- prompt("Semantic Scholar API key (optional)", "")
  log_default <- resolve_config_value("defaults.log", TRUE)
  opts$log <- prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))
  opts$web <- prompt("Web search enabled TRUE/FALSE", "TRUE")
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

extract_sources_override <- function(args) {
  if (length(args) == 0) return(NULL)
  i <- 1L
  while (i <= length(args)) {
    arg <- args[i]
    if (grepl("^--sources=", arg)) {
      tokens <- character(0)
      base <- sub("^--sources=", "", arg)
      if (nzchar(base)) tokens <- c(tokens, base)
      j <- i + 1L
      while (j <= length(args) && !grepl("^--", args[j])) {
        tokens <- c(tokens, args[j])
        j <- j + 1L
      }
      if (length(tokens) == 0) return("")
      return(paste(tokens, collapse = ","))
    }
    if (arg == "--sources") {
      tokens <- character(0)
      j <- i + 1L
      while (j <= length(args) && !grepl("^--", args[j])) {
        tokens <- c(tokens, args[j])
        j <- j + 1L
      }
      if (length(tokens) == 0) return("")
      return(paste(tokens, collapse = ","))
    }
    i <- i + 1L
  }
  NULL
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

ensure_jsonlite <- function() {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    cat("Missing dependency: jsonlite. Install it: install.packages('jsonlite').\n", file = stderr())
    quit(status = 2)
  }
}

normalize_source <- function(source) {
  src <- trimws(as.character(source))
  src <- gsub("^['\"]|['\"]$", "", src)
  src <- tolower(gsub("[\\s-]+", "_", src))
  src <- gsub("[^a-z_]", "", src)
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

fetch_json <- function(url, timeout = 30, headers = NULL, max_retries = 2, return_status = FALSE) {
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = timeout)

  fetch_text_base <- function(request_url) {
    warn_msg <- ""
    err_msg <- ""
    text <- withCallingHandlers(
      tryCatch({
        con <- url(request_url, open = "rb", encoding = "UTF-8")
        on.exit(close(con), add = TRUE)
        lines <- readLines(con, warn = FALSE)
        paste(lines, collapse = "")
      }, error = function(e) {
        err_msg <<- conditionMessage(e)
        ""
      }),
      warning = function(w) {
        warn_msg <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    )
    msg <- trimws(paste(warn_msg, err_msg))
    status <- NA_integer_
    if (grepl("HTTP status was", msg)) {
      code <- sub(".*'([0-9]+)'.*", "\\1", msg)
      if (grepl("^[0-9]+$", code)) status <- as.integer(code)
    }
    list(status = status, text = text, message = msg)
  }

  fetch_text_curl <- function(request_url, headers, timeout) {
    out <- list(status = NA_integer_, text = "", message = "")
    resp <- tryCatch({
      handle <- curl::new_handle()
      curl::handle_setopt(handle, timeout = timeout)
      if (!is.null(headers) && length(headers) > 0) {
        curl::handle_setheaders(handle, .list = headers)
      }
      curl::curl_fetch_memory(request_url, handle = handle)
    }, error = function(e) {
      out$message <<- conditionMessage(e)
      NULL
    })
    if (is.null(resp)) return(out)
    out$status <- resp$status_code
    if (length(resp$content) > 0) {
      out$text <- rawToChar(resp$content)
    }
    out
  }

  backoff <- c(1, 2, 4)
  attempts <- max_retries + 1L
  use_curl <- requireNamespace("curl", quietly = TRUE)
  resp <- NULL
  for (i in seq_len(attempts)) {
    resp <- if (use_curl) {
      fetch_text_curl(url, headers, timeout)
    } else {
      fetch_text_base(url)
    }
    if (!is.na(resp$status) && resp$status == 429 && i < attempts) {
      Sys.sleep(backoff[min(i, length(backoff))])
      next
    }
    break
  }

  if (!nzchar(resp$text)) {
    if (isTRUE(return_status)) {
      return(list(status = resp$status, data = NULL, message = resp$message))
    }
    return(NULL)
  }
  parsed <- tryCatch(jsonlite::fromJSON(resp$text, simplifyVector = FALSE), error = function(e) NULL)
  if (isTRUE(return_status)) {
    return(list(status = resp$status, data = parsed, message = resp$message))
  }
  parsed
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
    abstract = abstract
  )
}

fetch_openalex <- function(query, max_results, year_from, year_to, timeout, mailto) {
  per_page <- min(200, max_results)
  cursor <- "*"
  items <- list()
  while (length(items) < max_results && nzchar(cursor)) {
    url <- build_openalex_url(query, per_page, cursor, year_from, year_to, mailto)
    response <- fetch_json(url, timeout = timeout)
    if (is.null(response) || is.null(response$results)) break
    for (item in response$results) {
      items[[length(items) + 1]] <- parse_openalex_item(item)
      if (length(items) >= max_results) break
    }
    next_cursor <- if (!is.null(response$meta$next_cursor)) as.character(response$meta$next_cursor) else ""
    if (!nzchar(next_cursor) || next_cursor == cursor) break
    cursor <- next_cursor
  }
  items
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

fetch_crossref <- function(query, max_results, year_from, year_to, timeout, mailto) {
  items <- list()
  rows <- min(100, max_results)
  offset <- 0
  repeat {
    url <- build_crossref_url(query, rows, offset, year_from, year_to, mailto)
    response <- fetch_json(url, timeout = timeout)
    if (is.null(response) || is.null(response$message$items)) break
    batch <- response$message$items
    if (length(batch) == 0) break
    for (item in batch) {
      items[[length(items) + 1]] <- parse_crossref_item(item)
      if (length(items) >= max_results) break
    }
    if (length(items) >= max_results) break
    if (length(batch) < rows) break
    offset <- offset + rows
  }
  items
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
    pages = "",
    abstract = abstract
  )
}

fetch_semantic_scholar <- function(query, max_results, timeout, api_key = "", user_agent = "") {
  items <- list()
  limit <- min(100, max_results)
  offset <- 0
  headers <- list()
  if (nzchar(user_agent)) headers[["User-Agent"]] <- user_agent
  if (nzchar(api_key)) headers[["x-api-key"]] <- api_key
  if (nzchar(api_key) && !requireNamespace("curl", quietly = TRUE)) {
    cat("Note: Semantic Scholar API key provided but R package 'curl' is not installed; the key may not be sent.\n")
  }
  repeat {
    url <- build_semantic_url(query, limit, offset)
    response <- fetch_json(url, timeout = timeout, headers = headers, max_retries = 3, return_status = TRUE)
    if (!is.null(response$status) && response$status == 429) {
      cat("Semantic Scholar rate limit reached (HTTP 429). Consider setting --semantic-key or NLSS_SEMANTIC_SCHOLAR_API_KEY and retrying later.\n")
      break
    }
    data <- response$data
    if (is.null(data) || is.null(data$data)) break
    batch <- data$data
    if (length(batch) == 0) break
    for (item in batch) {
      items[[length(items) + 1]] <- parse_semantic_item(item)
      if (length(items) >= max_results) break
    }
    if (length(items) >= max_results) break
    if (length(batch) < limit) break
    offset <- offset + limit
  }
  items
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
  tokens <- unique(tolower(split_name_tokens(query)))
  tokens <- tokens[nzchar(tokens)]
  if (length(tokens) == 0) tokens <- character(0)
  source_names <- vapply(items, function(x) x$source_name, character(1))
  raw_scores <- vapply(items, function(x) if (!is.null(x$relevance_raw)) x$relevance_raw else NA_real_, numeric(1))
  source_max <- tapply(raw_scores, source_names, function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE))
  cites <- vapply(items, function(x) if (!is.null(x$citations)) as.numeric(x$citations) else 0, numeric(1))
  max_cites <- if (length(cites) > 0) max(cites, na.rm = TRUE) else 0
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
      hits <- vapply(tokens, function(tok) grepl(paste0("\\b", tok, "\\b"), title_text), logical(1))
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
    columns <- resolve_normalize_table_columns(table_spec$columns, default_cols)
  }
  rows <- list()
  if (nrow(items_df) > 0) {
    for (i in seq_len(nrow(items_df))) {
      row <- items_df[i, , drop = FALSE]
      values <- vapply(columns, function(col) {
        key <- col$key
        value <- if (!is.null(row[[key]])) row[[key]] else ""
        resolve_as_cell_text(value)
      }, character(1))
      rows[[length(rows) + 1]] <- values
    }
  }
  adjusted <- resolve_drop_empty_columns(columns, rows)
  headers <- vapply(adjusted$columns, function(col) col$label, character(1))
  body <- resolve_render_markdown_table(headers, adjusted$rows)
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
opts <- parse_args(args)

if (isTRUE(opts$help) || length(args) == 0) {
  print_usage()
  quit(status = 0)
}

if (parse_bool(opts$interactive, FALSE)) {
  opts <- interactive_options()
}

web_env <- parse_bool(Sys.getenv("NLSS_WEB_SEARCH", unset = "FALSE"), FALSE)
web_enabled <- parse_bool(opts$web, default = web_env)
if (!isTRUE(web_enabled)) {
  cat("Web search is disabled. Set --web TRUE or NLSS_WEB_SEARCH=1.\n", file = stderr())
  quit(status = 2)
}

query <- normalize_option(opts$query, "query")
if (!nzchar(query)) {
  query <- normalize_option(opts$topic, "topic")
}
if (!nzchar(query)) stop("Missing --query. Use --help for usage.")

sources_text <- normalize_option(opts$sources, "sources")
sources_override <- extract_sources_override(args)
if (!is.null(sources_override) && nzchar(trimws(sources_override))) {
  sources_text <- sources_override
}
if (length(sources_text) > 1) {
  sources_text <- paste(sources_text, collapse = ",")
}
if (!nzchar(sources_text)) {
  sources_text <- resolve_config_value("modules.research_academia.sources", "openalex,crossref")
}
sources_raw <- split_sources(sources_text)
if (length(sources_raw) > 0) {
  sources_raw <- vapply(sources_raw, function(x) {
    token <- trimws(as.character(x))
    token <- gsub("^['\"]|['\"]$", "", token)
    token
  }, character(1))
}
sources <- unique(vapply(sources_raw, normalize_source, character(1)))
valid_sources <- c("openalex", "crossref", "semantic_scholar")
sources <- sources[sources %in% valid_sources]
probe_tokens <- tolower(c(sources_text, args))
probe_tokens <- probe_tokens[!is.na(probe_tokens)]
if (any(grepl("semantic", probe_tokens)) && !("semantic_scholar" %in% sources)) sources <- c(sources, "semantic_scholar")
if (any(grepl("crossref", probe_tokens)) && !("crossref" %in% sources)) sources <- c(sources, "crossref")
if (any(grepl("openalex", probe_tokens)) && !("openalex" %in% sources)) sources <- c(sources, "openalex")
sources <- unique(sources)
if (length(sources) == 0) stop("No valid --sources provided.")

sources_report <- unique(c(
  sources,
  vapply(split_sources(sources_text), normalize_source, character(1))
))
sources_report <- sources_report[nzchar(sources_report)]
sources_report <- sources_report[sources_report %in% valid_sources]
if (length(sources_report) == 0) sources_report <- sources
sources <- sources_report

year_from <- normalize_option(opts$`year-from`, "year-from")
year_to <- normalize_option(opts$`year-to`, "year-to")

max_per_source <- normalize_option(opts$`max-per-source`, "max-per-source")
max_total <- normalize_option(opts$`max-total`, "max-total")
top_n <- normalize_option(opts$`top-n`, "top-n")
timeout_text <- normalize_option(opts$timeout, "timeout")

max_per_source <- if (nzchar(max_per_source)) as.integer(max_per_source) else as.integer(resolve_config_value("modules.research_academia.max_per_source", 50))
max_total <- if (nzchar(max_total)) as.integer(max_total) else as.integer(resolve_config_value("modules.research_academia.max_total", 200))
top_n <- if (nzchar(top_n)) as.integer(top_n) else as.integer(resolve_config_value("modules.research_academia.top_n", 10))
timeout <- if (nzchar(timeout_text)) as.integer(timeout_text) else as.integer(resolve_config_value("modules.research_academia.timeout", 30))

if (is.na(max_per_source) || max_per_source <= 0) stop("Invalid --max-per-source value.")
if (is.na(max_total) || max_total <= 0) stop("Invalid --max-total value.")
if (is.na(top_n) || top_n <= 0) stop("Invalid --top-n value.")
if (is.na(timeout) || timeout <= 0) stop("Invalid --timeout value.")

abstract_limit <- resolve_config_value("modules.research_academia.abstract_limit", 200)
keywords_limit <- resolve_config_value("modules.research_academia.keywords_limit", 80)
abstract_limit <- if (!is.null(abstract_limit)) as.integer(abstract_limit) else 200
keywords_limit <- if (!is.null(keywords_limit)) as.integer(keywords_limit) else 80
if (is.na(abstract_limit) || abstract_limit <= 0) abstract_limit <- 200
if (is.na(keywords_limit) || keywords_limit <= 0) keywords_limit <- 80

ensure_jsonlite()

mailto <- Sys.getenv("NLSS_CONTACT_EMAIL", unset = "")
user_agent <- if (nzchar(mailto)) paste0("NLSS/1.0 (mailto:", mailto, ")") else "NLSS/1.0"
semantic_key <- normalize_option(opts$`semantic-key`, "semantic-key")
if (!nzchar(semantic_key)) semantic_key <- Sys.getenv("NLSS_SEMANTIC_SCHOLAR_API_KEY", unset = "")
if (!nzchar(semantic_key)) semantic_key <- Sys.getenv("SEMANTIC_SCHOLAR_API_KEY", unset = "")
semantic_key <- trimws(semantic_key)

all_items <- list()
if ("openalex" %in% sources) {
  all_items <- c(all_items, fetch_openalex(query, max_per_source, year_from, year_to, timeout, mailto))
}
if ("crossref" %in% sources) {
  all_items <- c(all_items, fetch_crossref(query, max_per_source, year_from, year_to, timeout, mailto))
}
if ("semantic_scholar" %in% sources) {
  all_items <- c(all_items, fetch_semantic_scholar(query, max_per_source, timeout, semantic_key, user_agent))
}

deduped <- dedupe_items(all_items)
deduped <- compute_relevance(deduped, query, year_from, year_to)

if (length(deduped) == 0) {
  cat("No results found.\n")
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
  comprehensive_rows[[length(comprehensive_rows) + 1]] <- format_item_table_row(deduped[[i]], i, resolve_config_value("defaults.digits", 2), abstract_limit, keywords_limit)
}
top_rows <- list()
for (i in seq_along(top_items)) {
  top_rows[[length(top_rows) + 1]] <- format_item_table_row(top_items[[i]], i, resolve_config_value("defaults.digits", 2), abstract_limit, keywords_limit)
}

comprehensive_df <- if (length(comprehensive_rows) > 0) do.call(rbind, lapply(comprehensive_rows, as.data.frame, stringsAsFactors = FALSE)) else data.frame()
top_df <- if (length(top_rows) > 0) do.call(rbind, lapply(top_rows, as.data.frame, stringsAsFactors = FALSE)) else data.frame()

analysis_label <- "Research (Academia)"
out_dir <- resolve_get_research_out_dir()
report_path <- file.path(out_dir, "report_canonical.md")

template_override <- resolve_template_override(opts$template, module = "research_academia")
template_path <- if (!is.null(template_override)) {
  template_override
} else {
  resolve_get_template_path("research_academia.default", "research-academia/default-template.md")
}
template_meta <- resolve_get_template_meta(template_path)

table_spec <- NULL
top_spec <- NULL
if (!is.null(template_meta$tables$comprehensive)) table_spec <- template_meta$tables$comprehensive
if (is.null(table_spec) && !is.null(template_meta$table)) table_spec <- template_meta$table
if (!is.null(template_meta$tables$top)) top_spec <- template_meta$tables$top

digits <- resolve_config_value("defaults.digits", 2)
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
  narrative_text <- paste0("Query: \"", query, "\" returned no results across ", length(sources_report), " source", ifelse(length(sources_report) == 1, "", "s"), ".")
} else {
  narrative_text <- paste0("Query: \"", query, "\". Retrieved ", length(deduped),
                           " unique items across ", length(sources_report), " source", ifelse(length(sources_report) == 1, "", "s"),
                           ". The most relevant items appear in Table ", as.character(resolve_get_next_table_number(report_path) + 1), ".")
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

table_start <- as.integer(resolve_get_next_table_number(report_path))
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
  timeout = timeout,
  web = web_enabled
)

resolve_append_nlss_report(
  report_path,
  analysis_label,
  nlss_table,
  narrative_text,
  analysis_flags = analysis_flags,
  template_path = template_path,
  template_context = template_context
)

cat("Retrieved ", length(deduped), " unique items.\n", sep = "")
if (length(top_items) > 0) {
  cat("Top results:\n")
  for (i in seq_along(top_items)) {
    cat("- ", top_items[[i]]$title, "\n", sep = "")
  }
}

log_default <- resolve_config_value("defaults.log", TRUE)
log_enabled <- parse_bool(opts$log, default = log_default)
if (isTRUE(log_enabled)) {
  run_context <- resolve_get_run_context()
  results_payload <- list(
    status = "success",
    query = query,
    total_results = length(deduped),
    top_n = top_n,
    sources = sources_report,
    items = lapply(deduped, function(item) {
      list(
        title = item$title,
        year = item$year,
        source = item$source,
        doi = item$doi,
        url = item$url,
        citations = item$citations,
        relevance = item$relevance,
        keywords = truncate_text(coerce_scalar_text(item$keywords), keywords_limit),
        abstract = truncate_text(coerce_scalar_text(item$abstract), abstract_limit)
      )
    })
  )
  options_payload <- list(
    sources = sources_report,
    `year-from` = if (nzchar(year_from)) year_from else NULL,
    `year-to` = if (nzchar(year_to)) year_to else NULL,
    `max-per-source` = max_per_source,
    `max-total` = max_total,
    `top-n` = top_n,
    timeout = timeout,
    template = if (!is.null(template_override) && nzchar(template_override)) template_override else NULL
  )
  resolve_append_analysis_log(
    out_dir,
    "research-academia",
    run_context$prompt,
    run_context$commands,
    results_payload,
    options = options_payload,
    user_prompt = get_user_prompt(opts)
  )
}
