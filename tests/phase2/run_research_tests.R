#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Public CLI, deterministic transport fixtures and independent ranking references.
args <- commandArgs(TRUE)
script <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])[1]
repo <- normalizePath(file.path(dirname(script), "../.."), winslash = "/")
if ("--help" %in% args) { cat("Usage: run_research_tests.R [--root PATH] [--keep N] [--match REGEX]\n"); quit(status = 0) }
if (length(args) %% 2L || (length(args) && any(!args[seq(1L, length(args), 2L)] %in% c("--root", "--keep", "--match")))) stop("Invalid runner options")
arg <- function(key, default) { i <- which(args == key); if (length(i) > 1L) stop("Repeated runner option"); if (length(i)) args[i + 1L] else default }
for (p in c("yaml", "jsonlite", "digest")) if (!requireNamespace(p, quietly = TRUE)) stop("Missing test dependency: ", p)
cfg <- yaml::read_yaml(Sys.getenv("NLSS_TESTS_CONFIG", file.path(repo, "tests/tests.yml")))$tests
forced <- arg("--root", Sys.getenv("NLSS_TEST_ROOT", ""))
collection <- file.path(if (nzchar(forced)) forced else file.path(repo, cfg$output_dir), "phase2-research")
keep <- suppressWarnings(as.numeric(arg("--keep", Sys.getenv("NLSS_KEEP_RUNS", as.character(cfg$keep_runs_default)))))
if (length(keep) != 1L || !is.finite(keep) || keep < 0 || keep != floor(keep)) stop("Invalid --keep")
pattern <- arg("--match", ".*"); invisible(grepl(pattern, "validate regex"))
work <- file.path(collection, paste0("run-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid())); dir.create(work, recursive = TRUE)
work <- normalizePath(work, winslash = "/")
json <- function(value, path) jsonlite::write_json(value, path, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null", digits = NA)
read_json <- function(path) jsonlite::fromJSON(path, simplifyVector = FALSE)
text <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
sha <- function(path) digest::digest(file = path, algo = "sha256")
utc <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
source_paths <- sort(list.files(file.path(repo, "scripts/R"), "[.]R$", recursive = TRUE, full.names = TRUE))
source_hash <- setNames(vapply(source_paths, sha, character(1)), substring(source_paths, nchar(repo) + 2L))
started <- utc(); json(list(started_utc = started, argv = args, runner_sha256 = sha(script), production_files = as.list(source_hash)), file.path(work, "started.json"))
checks <- 0L; results <- list(); registered <- character()
check <- function(ok, label) { if (!isTRUE(ok)) stop(label, call. = FALSE); checks <<- checks + 1L }
near <- function(actual, expected, label) check(length(actual) == length(expected) && all(abs(unlist(actual) - expected) < 1e-12), label)
test <- function(name, code) {
  registered <<- c(registered, name); if (!grepl(pattern, name)) return(invisible(NULL))
  previous <- getwd(); on.exit(setwd(previous)); before <- checks; start <- proc.time()[["elapsed"]]
  error <- tryCatch({ force(code); NULL }, error = conditionMessage)
  results[[length(results) + 1L]] <<- list(test = name, passed = is.null(error), checks = checks - before,
    seconds = unname(proc.time()[["elapsed"]] - start), message = if (is.null(error)) "OK" else error)
  cat(if (is.null(error)) "[PASS] " else "[FAIL] ", name, if (is.null(error)) "" else paste0(": ", error), "\n", sep = "")
}
oa <- list(id = "https://openalex.org/W1", title = "Stress and coping", doi = "https://doi.org/10.1234/SHARED", publication_year = 2020L,
  relevance_score = 10, cited_by_count = 9L, authorships = list(list(author = list(display_name = "Jane Doe"))),
  primary_location = list(source = list(display_name = "Journal One")), keywords = list(list(display_name = "stress")),
  abstract_inverted_index = list(Stress = list(0L), predicts = list(1L), coping = list(2L)))
cr <- list(title = list("Stress and coping"), DOI = "10.1234/shared", issued = list(`date-parts` = list(list(2020L))),
  score = 20, `is-referenced-by-count` = 8L, author = list(list(family = "Doe", given = "Jane")),
  `container-title` = list("Journal One"), abstract = "<p>Stress predicts coping.</p>")
cr2 <- list(title = list("Coping methods"), DOI = "10.1234/second", issued = list(`date-parts` = list(list(2022L))),
  score = 5, `is-referenced-by-count` = 3L, author = list(list(family = "Smith", given = "Alex")),
  `container-title` = list("Journal Two"), abstract = paste(rep("Coping methods evidence.", 20), collapse = " "), subject = list("Methods"))
ss <- list(title = "Stress trial", year = 2021L, citationCount = 0L, authors = list(list(name = "Pat Lee")),
  venue = "Journal Three", url = "https://example.org/paper", abstract = "Stress trial evidence.", fieldsOfStudy = list("Psychology"))
packet <- function(body, status = 200L) list(status = status, body = body)
fixtures <- function() list(schema_version = 1L, sources = list(
  openalex = list(packet(list(results = list(oa), meta = list(next_cursor = NULL)))),
  crossref = list(packet(list(message = list(items = list(cr, cr2))))),
  semantic_scholar = list(packet(list(data = list(ss))))))
context <- function(name, fixture = fixtures(), override = list()) {
  base <- file.path(work, "cases", name); project <- file.path(base, "project"); dir.create(project, recursive = TRUE)
  config <- file.path(base, "config.yml"); f <- file.path(base, "responses.json")
  defaults <- list(defaults = list(output_dir = file.path(project, "out")), logging = list(include_user_prompt = TRUE))
  defaults <- utils::modifyList(defaults, override)
  yaml::write_yaml(defaults, config); json(fixture, f)
  list(base = base, project = project, directory = file.path(project, "out"), config = config, fixture = f)
}
tokens <- function(options) unlist(Map(function(k, v) c(paste0("--", k), as.character(v)), names(options), options), use.names = FALSE)
invoke <- function(context, options = list(query = "stress coping", sources = "openalex,crossref,semantic_scholar"), expected = 0L, env = character()) {
  setwd(context$project); argv <- if (is.list(options)) tokens(options) else options
  log <- tempfile("cli-", context$base, fileext = ".log")
  record <- list(argv = argv, started_utc = utc(), entrypoint_sha256 = sha(file.path(repo, "scripts/R/research_academia.R")))
  json(record, paste0(log, ".process.json"))
  old <- Sys.getenv(c("NLSS_CONFIG_PATH", "NLSS_RESEARCH_FIXTURES", "NLSS_SEMANTIC_SCHOLAR_API_KEY", "SEMANTIC_SCHOLAR_API_KEY", "NLSS_REPLAY_REQUEST", "NLSS_PROMPT_FILE", "NLSS_CONTACT_EMAIL"), unset = NA_character_)
  on.exit({ for (key in names(old)) if (is.na(old[[key]])) Sys.unsetenv(key) else do.call(Sys.setenv, setNames(list(old[[key]]), key)) }, add = TRUE)
  Sys.unsetenv(names(old)); Sys.setenv(NLSS_CONFIG_PATH = context$config, NLSS_RESEARCH_FIXTURES = context$fixture)
  if (length(env)) do.call(Sys.setenv, as.list(env))
  status <- system2(file.path(R.home("bin"), "Rscript"), shQuote(c(file.path(repo, "scripts/R/research_academia.R"), argv)), stdout = log, stderr = log)
  record$finished_utc <- utc(); record$exit_code <- status; json(record, paste0(log, ".process.json"))
  check(if (expected == 0L) status == 0L else status != 0L, paste("Unexpected CLI exit", status, text(log)))
  paths <- list.files(file.path(context$directory, "utility-runs"), "^result[.]json$", recursive = TRUE, full.names = TRUE)
  paths <- paths[!grepl("/[.]pending-", paths)]
  if (!length(paths)) return(list(log = log, status = status))
  path <- dirname(tail(sort(paths), 1L)); result <- read_json(file.path(path, "result.json")); request <- read_json(file.path(path, "request.json"))
  check(request$kind == "utility" && isFALSE(request$replay$eligible), "Research falsely declares statistical replay")
  for (artifact in result$artifacts) check(identical(sha(file.path(path, artifact$path)), artifact$sha256), "Utility artifact hash differs")
  output <- file.path(path, if (result$status == "failed") "diagnostic-output.md" else "output.md")
  list(log = log, status = status, path = path, result = result, request = request, values = result$results,
    pages = read_json(file.path(path, "responses.json")), markdown = text(output))
}
test("research_three_sources_independent_ranking_smoke", {
  c <- context("ranking"); x <- invoke(c)
  check(x$values$status == "success" && x$result$status == "completed", "Successful retrieval status differs")
  near(x$values$raw_count, 4, "Raw source records before DOI dedupe")
  near(x$values$total_results, 3, "DOI normalization dedupe")
  check(identical(vapply(x$values$items, function(i) i$title, character(1)), c("Stress and coping", "Coping methods", "Stress trial")), "Independent ranking order differs")
  near(lapply(x$values$items, function(i) i$relevance), c(.9, .125 + .1 + .2 * log(4) / log(9) + .1, .4), "Independent weighted relevance scores")
  check(nchar(x$values$items[[2]]$abstract) > 200, "Audit truncated the retrieved abstract")
  check(grepl("Doe, J.", x$markdown, fixed = TRUE) && grepl("10.1234/shared", x$markdown, fixed = TRUE), "Author/reference formatting lost")
  check(length(x$pages) == 3L && all(vapply(x$pages, function(p) p$transport == "fixture", logical(1))), "Per-source fixture responses not preserved")
})
for (source in c("openalex", "crossref", "semantic_scholar")) test(paste0("research_source_", source, "_smoke"), {
  x <- invoke(context(source), list(query = "mentions openalex crossref semantic scholar", sources = source))
  check(identical(unlist(x$values$sources), source), "Query text silently activated extra scholarly sources")
  check(all(vapply(x$pages, function(p) p$source == source, logical(1))), "Unrequested source executed")
})
test("research_source_equals_whitespace_aliases", {
  x <- invoke(context("source-syntax"), c("--query", "stress", "--sources=open-alex", "crossref", "semantic", "--max-total", "2", "--top-n", "1"))
  near(x$values$total_results, 2, "Overall result cap")
  near(x$values$top_n, 1, "Top N cap")
  check(length(x$values$sources) == 3L, "Space-separated source aliases lost")
})
test("research_year_bounds_all_sources_and_missing_years", {
  f <- fixtures(); f$sources$semantic_scholar[[1]]$body$data <- list(ss, utils::modifyList(ss, list(title = "Undated trial", year = NULL), keep.null = TRUE))
  x <- invoke(context("years", f), list(query = "stress coping", sources = "openalex,crossref,semantic_scholar", `year-from` = 2021, `year-to` = 2022))
  check(all(vapply(x$values$items, function(i) as.integer(i$year) %in% 2021:2022, logical(1))), "Requested year window not applied to every source")
  check(length(x$values$year_filter_excluded) == 3L, "Missing/out-of-range years not audited")
})
test("research_query_regex_metacharacters_are_literal", {
  x <- invoke(context("literal-query"), list(query = "stress [ coping + C++", sources = "crossref"))
  check(x$values$status == "success" && all(is.finite(vapply(x$values$items, function(i) i$relevance, numeric(1)))), "Query punctuation broke ranking")
})
test("research_successful_empty_not_network_failure", {
  f <- fixtures(); f$sources$openalex <- list(packet(list(results = list(), meta = list(next_cursor = NULL))))
  x <- invoke(context("empty", f), list(query = "stress", sources = "openalex"))
  check(x$values$status == "success" && x$values$total_results == 0L && x$result$status == "completed", "Valid zero hits mislabeled as failure")
})
test("research_partial_failure_remains_partial_under_custom_template", {
  f <- fixtures(); f$sources$crossref <- list(packet(list(message = "Unavailable"), 503L))
  c <- context("partial", f); template <- file.path(c$base, "custom.md"); writeLines("# Minimal custom presentation", template)
  x <- invoke(c, list(query = "stress", sources = "openalex,crossref", template = template))
  check(x$values$status == "partial" && x$result$status == "partial", "Partial source failure falsely completed")
  check(grepl("Status: partial", x$markdown, fixed = TRUE) && grepl("http_503", x$markdown, fixed = TRUE), "Custom template hid source failure")
})
for (failure in c("http", "json", "schema", "item_schema", "negative_citations", "crossref_envelope", "openalex_meta", "author_schema", "transport", "fixture_exhausted")) test(paste0("research_all_sources_failed_", failure), {
  source <- if (failure %in% c("crossref_envelope", "author_schema")) "crossref" else "openalex"
  malformed_author <- cr; malformed_author$author <- list(list(family = c("Doe", "Jones"), given = "A"))
  f <- fixtures(); f$sources[[source]] <- switch(failure,
    http = list(packet(list(message = "Unauthorized"), 401L)), json = list(list(status = 200L, text = "not JSON")),
    schema = list(packet(list(wrong = list()))),
    item_schema = list(packet(list(results = list(utils::modifyList(oa, list(publication_year = c(2020, 2021))))))),
    negative_citations = list(packet(list(results = list(utils::modifyList(oa, list(cited_by_count = -3)))))),
    crossref_envelope = list(packet(list(message = "Invalid atomic envelope"))),
    openalex_meta = list(packet(list(results = list(oa), meta = "Invalid atomic pagination metadata"))),
    author_schema = list(packet(list(message = list(items = list(malformed_author))))),
    transport = list(list(transport_error = TRUE)), fixture_exhausted = list())
  c <- context(paste0("failed-", failure), f)
  x <- invoke(c, list(query = "stress", sources = source), expected = 1L)
  check(x$values$status == "failed" && x$result$status == "failed", "Network/schema failure falsely reported zero-hit success")
  check(file.exists(file.path(x$path, "diagnostic-output.md")) && !file.exists(file.path(x$path, "output.md")), "Failed retrieval has normal completed output")
  check(!file.exists(file.path(c$directory, "report_canonical.md")), "All-failed search appended success-shaped canonical report")
})
test("research_pagination_retains_earlier_page_on_later_failure", {
  f <- fixtures(); f$sources$openalex <- list(packet(list(results = list(oa), meta = list(next_cursor = "cursor-2"))), packet(list(message = "Unavailable"), 500L))
  x <- invoke(context("partial-pages", f), list(query = "stress", sources = "openalex"))
  check(x$values$status == "partial" && x$values$total_results == 1L && length(x$pages) == 2L, "Earlier page lost or later failure hidden")
})
test("research_repeated_cursor_stops_without_infinite_loop", {
  f <- fixtures(); f$sources$openalex <- list(packet(list(results = list(oa), meta = list(next_cursor = "*"))))
  x <- invoke(context("repeat-cursor", f), list(query = "stress", sources = "openalex"))
  check(x$values$status == "partial" && x$values$source_status$openalex$error == "repeated_cursor", "Pagination cycle not disclosed")
})
test("research_rate_limit_bounded_retries_record_each_attempt", {
  f <- fixtures(); f$sources$semantic_scholar <- c(rep(list(packet(list(message = "Rate limited"), 429L)), 2), list(packet(list(data = list(ss)))))
  x <- invoke(context("retry", f), list(query = "stress", sources = "semantic_scholar"))
  check(x$values$status == "success" && length(x$pages) == 3L, "Bounded retry response history lost")
  near(lapply(x$pages, function(p) p$status), c(429, 429, 200), "Actual retry statuses")
  check(all(vapply(x$pages, function(p) isTRUE(p$http_status_observed), logical(1))), "Fixture HTTP statuses not marked observed")
})
test("research_base_transport_does_not_invent_http_status", {
  # Isolated transport unit: base::url reads a local file, never an API endpoint.
  sandbox <- new.env(parent = baseenv()); sandbox$transport <- "live"; sandbox$research_secrets <- character()
  sandbox$response_snapshots <- list(); sandbox$requireNamespace <- function(package, ...) if (package == "curl") FALSE else base::requireNamespace(package, ...)
  expressions <- parse(file.path(repo, "scripts/R/research_academia.R"))
  for (expression in expressions) if (is.call(expression) && identical(expression[[1]], as.name("<-")) &&
    is.symbol(expression[[2]]) && as.character(expression[[2]]) %in% c("research_redact", "fetch_json")) eval(expression, sandbox)
  local_response <- file.path(work, "base-transport-response.json"); json(list(results = list()), local_response)
  result <- sandbox$fetch_json(paste0("file://", local_response), "openalex", 1L)
  check(identical(result$error, "") && is.list(result$data), "Successful base transport changed body parsing")
  check(is.na(result$status) && is.na(sandbox$response_snapshots[[1]]$status), "Base transport invented an observed HTTP status")
  check(isFALSE(sandbox$response_snapshots[[1]]$http_status_observed), "Base transport status limitation not disclosed")
})
test("research_credential_echo_redacted_everywhere", {
  dummy <- "FAKE_TEST_CREDENTIAL_DO_NOT_USE_4711"
  f <- fixtures(); f$sources$semantic_scholar[[1]]$body$token <- dummy
  f$sources$semantic_scholar[[1]]$body[[dummy]] <- "Credential echoed in a JSON property name"
  f$sources$semantic_scholar[[1]]$body$data[[1]]$abstract <- paste("Echo", dummy)
  f$sources$semantic_scholar[[1]]$body$data[[1]]$url <- paste0("https://example.org/paper?api_key=", dummy)
  c <- context("credential", f)
  x <- invoke(c, list(query = "stress", sources = "semantic_scholar", `semantic-key` = dummy, `user-prompt` = paste("Question", dummy)))
  for (path in list.files(c$directory, recursive = TRUE, full.names = TRUE)) {
    check(!grepl(dummy, rawToChar(readBin(path, "raw", file.info(path)$size)), fixed = TRUE), paste("Credential leaked into", basename(path)))
  }
  check(!grepl(dummy, text(x$log), fixed = TRUE), "Credential leaked into console output")
  check(grepl("<redacted>", text(file.path(x$path, "responses.json")), fixed = TRUE), "Credential-bearing response redaction not recorded")
})
test("research_log_false_preserves_mandatory_snapshot", {
  c <- context("no-log"); x <- invoke(c, list(query = "stress", sources = "openalex", log = FALSE))
  check(!file.exists(file.path(c$directory, "analysis_log.jsonl")), "Log FALSE did not suppress compatibility log")
  check(file.exists(file.path(x$path, "responses.json")) && file.exists(file.path(x$path, "template.md")), "Log FALSE discarded audit artifacts")
})
test("research_templates_default_comprehensive_and_override", {
  for (kind in c("default", "comprehensive", "custom")) {
    c <- context(paste0("template-", kind)); opts <- list(query = "stress", sources = "crossref")
    if (kind == "comprehensive") opts$template <- file.path(repo, "assets/research-academia/comprehensive-template.md")
    if (kind == "custom") { opts$template <- file.path(c$base, "custom.md"); writeLines(c("# CUSTOM_RESEARCH", "{{references}}", "{{comprehensive_table_body}}"), opts$template) }
    x <- invoke(c, opts)
    marker <- switch(kind, default = "Most Relevant", comprehensive = "Comprehensive Results", custom = "CUSTOM_RESEARCH")
    check(grepl(marker, x$markdown, fixed = TRUE), paste("Template behavior missing", kind))
  }
})
test("research_workspace_root_without_dataset_and_no_manifest_mutation", {
  c <- context("workspace-root"); yaml::write_yaml(list(version = 1L, datasets = list()), file.path(c$project, "nlss-workspace.yml")); c$directory <- c$project
  before <- sha(file.path(c$project, "nlss-workspace.yml"))
  x <- invoke(c, list(query = "stress", sources = "openalex", log = FALSE))
  check(identical(before, sha(file.path(c$project, "nlss-workspace.yml"))), "Dataset-free literature lookup changed manifest")
  check(!length(list.files(c$project, "[.]parquet$", recursive = TRUE)), "Literature lookup fabricated a dataset")
})
test("research_private_config_cli_precedence_and_topic_alias", {
  c <- context("config", override = list(modules = list(research_academia = list(sources = "crossref", top_n = 1L, max_total = 1L))))
  x <- invoke(c, list(topic = "stress coping", `max-total` = 2))
  near(x$values$total_results, 2, "CLI cap overrides private config")
  near(x$values$top_n, 1, "Private top N honored")
  check(identical(unlist(x$values$sources), "crossref"), "Private source default ignored")
})
test("research_symlink_output_paths_rejected_before_creation", {
  for (mode in c("config", "manifest")) {
    c <- context(paste0("symlink-", mode)); destination <- file.path(c$base, "destination"); dir.create(destination)
    link <- file.path(c$project, "linked"); check(file.symlink(destination, link), "Could not prepare output symlink fixture")
    if (mode == "config") {
      yaml::write_yaml(list(defaults = list(output_dir = file.path(link, "nested"))), c$config)
      c$directory <- file.path(link, "nested")
    } else {
      yaml::write_yaml(list(version = 1L, active_dataset = "linked", datasets = list(list(name = "linked", parquet = "linked/working.parquet"))), file.path(c$project, "nlss-workspace.yml"))
      c$directory <- link
    }
    x <- invoke(c, list(query = "stress", sources = "openalex"), expected = 1L)
    check(grepl("must not traverse symlinks", text(x$log), fixed = TRUE), "Output symlink guard did not retain original path")
    check(!length(list.files(destination, all.files = TRUE, no.. = TRUE)), "Unsafe output created files through symlink")
  }
})
test("research_invalid_options_refused_before_network_or_publication", {
  c <- context("invalid")
  for (argv in list("--help", c("--help", "TRUE"), "--help=TRUE")) {
    x <- invoke(c, argv)
    check(grepl("Usage:", text(x$log), fixed = TRUE), "Explicit or bare help flag did not print usage")
  }
  for (argv in list(c("--sources", "openalex"), c("--query", "stress", "--sources", "openalex,bogus"),
    c("--query", "stress", "--max-per-source", "1.5"), c("--query", "stress", "--max-total", "0"),
    c("--query", "stress", "--timeout", "Inf"), c("--query", "stress", "--year-from", "2022", "--year-to", "2020"),
    c("--query", "stress", "--year-from", "2020.5"), c("--query", "stress", "--top-n", "-1"),
    c("--query", "stress", "--log", "perhaps"), c("--query", "stress", "--unexpected", "TRUE"),
    c("--query", "stress", "--template", "does-not-exist.md"))) {
    invoke(c, argv, expected = 1L)
    check(!dir.exists(file.path(c$directory, "utility-runs")), "Invalid request began publication")
  }
})
test("research_fixture_mode_invalid_document_never_falls_back_live", {
  c <- context("invalid-fixture"); writeLines("not valid JSON", c$fixture)
  x <- invoke(c, list(query = "stress", sources = "openalex"), expected = 1L)
  check(grepl("no live fallback", text(x$log), fixed = TRUE), "Invalid offline transport lacks no-fallback diagnostic")
})
if (!length(results)) stop("No matching tests")
json(list(suite = "phase2-research", started_utc = started, finished_utc = utc(), available_cases = length(registered),
  available_test_names = registered, selected_pattern = pattern, checks = checks,
  source_changed_during_run = !identical(source_hash, setNames(vapply(source_paths, sha, character(1)), names(source_hash))), tests = results), file.path(work, "results.json"))
passed <- sum(vapply(results, function(x) x$passed, logical(1)))
cat(sprintf("Research: %d/%d passed; %d assertions. Results: %s\n", passed, length(results), checks, file.path(work, "results.json")))
if (!nzchar(forced) && keep > 0) {
  paths <- sort(list.dirs(collection, recursive = FALSE, full.names = TRUE), decreasing = TRUE)
  paths <- paths[grepl("^run-[0-9]{14}-[0-9]+$", basename(paths))]
  if (length(paths) > keep) for (path in setdiff(tail(paths, -keep), work)) unlink(path, recursive = TRUE)
}
if (passed != length(results)) quit(status = 1L)
