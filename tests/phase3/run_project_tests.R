#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Focused public-contract checks for the standalone, read-only project inspector.
# Fixtures and subprocess configuration are private to this runner. Statistical
# discovery, import and publication are deliberately not changed by these tests.

arguments <- commandArgs(TRUE)
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
script <- normalizePath(script[1], winslash = "/", mustWork = TRUE)
repo <- normalizePath(file.path(dirname(script[1]), "../.."), winslash = "/", mustWork = TRUE)
if (identical(arguments, "--help")) {
  cat("Usage: run_project_tests.R [--root PATH] [--keep N] [--match REGEX]\n",
      "Honors NLSS_TEST_ROOT, NLSS_KEEP_RUNS and NLSS_TESTS_CONFIG. Forced roots are never pruned.\n", sep = "")
  quit(status = 0L)
}
if (length(arguments) && (length(arguments) %% 2L ||
    any(!arguments[seq.int(1L, length(arguments), 2L)] %in% c("--root", "--keep", "--match")))) {
  stop("Unknown or incomplete runner option. Use --help.", call. = FALSE)
}
option <- function(name, fallback) {
  index <- which(arguments == name)
  if (!length(index)) return(fallback)
  if (length(index) != 1L || index == length(arguments) || startsWith(arguments[index + 1L], "--")) {
    stop("Repeated or incomplete runner option: ", name, call. = FALSE)
  }
  arguments[index + 1L]
}
required <- c("yaml", "jsonlite", "digest", "arrow")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Project tests require installed packages: ", paste(missing, collapse = ", "))
absolute <- function(path) {
  if (!grepl("^(/|[A-Za-z]:|\\\\)", path)) path <- file.path(repo, path)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}
configuration_path <- absolute(Sys.getenv("NLSS_TESTS_CONFIG", "tests/tests.yml"))
configuration <- yaml::read_yaml(configuration_path, eval.expr = FALSE)$tests
required_path <- function(value, label) {
  if (!is.character(value) || length(value) != 1L || !nzchar(value)) stop("Missing tests.yml setting: ", label)
  path <- absolute(value)
  if (!file.exists(path)) stop("Missing configured fixture: ", label)
  path
}
golden_path <- required_path(configuration$golden_dataset, "tests.golden_dataset")
note_path <- required_path(configuration$phase3$study_document, "tests.phase3.study_document")
output_base <- absolute(configuration$output_dir)
forced_root <- option("--root", Sys.getenv("NLSS_TEST_ROOT", ""))
keep_text <- option("--keep", Sys.getenv("NLSS_KEEP_RUNS", as.character(configuration$keep_runs_default)))
if (length(keep_text) != 1L || !grepl("^[0-9]+$", keep_text)) stop("--keep/NLSS_KEEP_RUNS must be a non-negative integer.")
keep <- suppressWarnings(as.integer(keep_text))
if (is.na(keep)) stop("--keep/NLSS_KEEP_RUNS is out of range.")
match_pattern <- option("--match", "")
if (nzchar(match_pattern)) invisible(tryCatch(grepl(match_pattern, ""), error = function(e) stop("Invalid --match regex.")))
started_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
run_root <- if (nzchar(forced_root)) absolute(forced_root) else file.path(output_base, format(Sys.time(), "%Y%m%d%H%M%S"))
work <- file.path(run_root, "phase3", paste0("run-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid()))
if (file.exists(work)) stop("Refusing to reuse an existing test work directory.")
dir.create(work, recursive = TRUE, showWarnings = FALSE)
work <- normalizePath(work, winslash = "/", mustWork = TRUE)
summary_path <- file.path(work, "results.json")
sha256 <- function(path) digest::digest(file = path, algo = "sha256")
text_file <- function(path) if (file.exists(path)) paste(readLines(path, warn = FALSE), collapse = "\n") else ""
timestamp <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
source_paths <- sort(unique(c(script[1], configuration_path, golden_path, note_path,
  file.path(repo, "scripts/config.yml"), file.path(repo, "scripts/R/project_inspect.R"),
  list.files(file.path(repo, "scripts/R"), pattern = "[.]R$", recursive = TRUE, full.names = TRUE))))
source_hashes <- function() setNames(lapply(source_paths, sha256), vapply(source_paths, function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (startsWith(path, paste0(repo, "/"))) substring(path, nchar(repo) + 2L) else path
}, character(1)))
initial_hashes <- source_hashes()
results <- list()
commands <- list()
current_assertions <- list()
current_test <- "setup"
save_results <- function(exit_status = NULL, finished_at = NULL) {
  jsonlite::write_json(list(schema_version = 1L, suite = "phase3-project", owner = "nlss-project-test-runner",
    started_at = started_at, finished_at = finished_at, exit_status = exit_status,
    command = c(file.path(R.home("bin"), "Rscript"), script[1], arguments), match = match_pattern,
    source_sha256 = initial_hashes, source_unchanged = identical(initial_hashes, source_hashes()),
    environment = list(r = R.version.string, platform = R.version$platform,
      packages = as.list(vapply(required, function(package) as.character(utils::packageVersion(package)), character(1)))),
    tests = results, commands = commands), summary_path, auto_unbox = TRUE, null = "null", digits = NA, pretty = TRUE)
}
check <- function(condition, label) {
  passed <- isTRUE(condition)
  current_assertions[[length(current_assertions) + 1L]] <<- list(assertion = label, passed = passed)
  if (!passed) stop(label, call. = FALSE)
  invisible(TRUE)
}
expect_error <- function(code, pattern = NULL) {
  problem <- tryCatch({ force(code); NULL }, error = function(error) conditionMessage(error))
  check(!is.null(problem), "Invalid input is rejected with an error")
  if (!is.null(pattern)) check(grepl(pattern, problem, ignore.case = TRUE), paste("Error identifies", pattern))
  invisible(problem)
}
private_config <- file.path(work, "config.yml")
baseline_config <- yaml::read_yaml(file.path(repo, "scripts/config.yml"), eval.expr = FALSE)
reset_config <- function() yaml::write_yaml(baseline_config, private_config)
reset_config()
Sys.setenv(NLSS_CONFIG_PATH = private_config)
runtime <- new.env(parent = globalenv())
source(file.path(repo, "scripts/R/lib/bootstrap.R"), local = runtime)
runtime$nlss_bootstrap(envir = runtime)
runtime$source_lib("project_inspect.R")

# Trap every data-loading route that could discover neighbours or load/publish data.
for (function_name in c("find_workspace_manifest", "validate_workspace_manifest_path", "load_dataframe",
    "snapshot_working_dataframe", "snapshot_dataset", "load_verified_import", "get_default_out",
    "ensure_out_dir", "write_workspace_manifest", "update_workspace_manifest", "append_analysis_log",
    "append_nlss_report", "nlss_begin_run", "nlss_publish_run")) {
  assign(function_name, local({ name <- function_name
    function(...) stop("Forbidden loading/publication helper invoked: ", name, call. = FALSE)
  }), envir = runtime)
}
tree_state <- function(root) {
  records <- list()
  regular_file <- function(path) {
    if (.Platform$OS.type == "unix") {
      executable <- Sys.which("test")
      if (!nzchar(executable)) stop("POSIX file inventory requires the standard test executable.")
      return(system2(executable, c("-f", shQuote(path)), stdout = FALSE, stderr = FALSE) == 0L)
    }
    isTRUE(file_test("-f", path))
  }
  visit <- function(path, relative) {
    info <- file.info(path)
    link <- Sys.readlink(path)
    linked <- !is.na(link) && nzchar(link)
    regular <- !linked && !isTRUE(info$isdir) && regular_file(path)
    records[[relative]] <<- list(type = if (linked) "symlink" else if (isTRUE(info$isdir)) "directory" else if (regular) "file" else "special",
      bytes = as.numeric(info$size), mtime = as.numeric(info$mtime), mode = as.character(info$mode),
      link = if (linked) link else NULL, sha256 = if (regular) sha256(path) else NULL)
    if (!linked && isTRUE(info$isdir)) {
      for (name in sort(list.files(path, all.files = TRUE, no.. = TRUE))) {
        visit(file.path(path, name), if (relative == ".") name else paste(relative, name, sep = "/"))
      }
    }
  }
  visit(root, ".")
  records
}
readonly <- function(root, code) {
  before <- tree_state(root)
  result <- tryCatch(force(code), error = identity)
  check(identical(before, tree_state(root)), "Inspection preserves every path, file hash, mtime, mode and symlink")
  if (inherits(result, "error")) stop(result)
  result
}
write_text <- function(path, lines) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}
write_json <- function(value, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(value, path, auto_unbox = TRUE, null = "null", digits = NA)
}
marker <- "nlss-workspace.yml"
write_manifest <- function(root, datasets = list(), active = NULL, ...) {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  yaml::write_yaml(c(list(schema_version = 2L, storage = "managed_parquet_v1",
    workspace_id = "test-project-id", active_dataset = active, datasets = datasets), list(...)), file.path(root, marker))
  invisible(root)
}
fixture <- function(suffix = "project", with_data = FALSE) {
  base <- file.path(work, "cases", current_test, suffix)
  root <- file.path(base, "project")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  entry <- list(name = "sample", id = "ds-test", parquet = "data/current.parquet")
  descriptor_path <- file.path(root, ".nlss/datasets/ds-test/dataset.json")
  if (with_data) {
    write_text(file.path(root, entry$parquet), "Synthetic byte fixture: never loaded as a dataframe.")
    data_hash <- sha256(file.path(root, entry$parquet))
    snapshot <- paste0(".nlss/objects/", data_hash)
    dir.create(dirname(file.path(root, snapshot)), recursive = TRUE)
    file.copy(file.path(root, entry$parquet), file.path(root, snapshot))
    dictionary <- ".nlss/objects/dictionary-fixture"
    write_json(list(schema_version = 1L, columns = list()), file.path(root, dictionary))
    source <- "original.csv"
    write_text(file.path(root, source), c("x,y", "1,2"))
    source_hash <- sha256(file.path(root, source))
    preserved <- paste0(".nlss/objects/", source_hash)
    file.copy(file.path(root, source), file.path(root, preserved))
    entry$dataset <- list(dataset_id = entry$id, version_id = "v-recorded",
      data_sha256 = data_hash, dictionary_sha256 = sha256(file.path(root, dictionary)),
      snapshot_path = snapshot, dictionary_path = dictionary)
    write_json(list(schema_version = 1L, workspace_id = "test-project-id", dataset_id = entry$id,
      working = entry$parquet, initial_version = entry$dataset,
      source = list(selected_path = source, path = preserved, sha256 = source_hash)), descriptor_path)
    write_manifest(root, list(entry[c("name", "id")]), "sample")
  } else write_manifest(root)
  list(base = base, root = root, entry = entry, descriptor = descriptor_path)
}
inspect <- function(context, verify = FALSE, ...) {
  readonly(context$base, runtime$nlss_inspect_project(project = context$root, verify = verify, ...))
}
run_cli <- function(options = character(), cwd = work, module = "project_inspect", timeout = 30L) {
  output_path <- file.path(work, "command-stdout.txt")
  error_path <- file.path(work, "command-stderr.txt")
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(cwd)
  executable <- file.path(R.home("bin"), "Rscript")
  command_arguments <- c(file.path(repo, "scripts/R", paste0(module, ".R")), options)
  start <- timestamp()
  status <- suppressWarnings(system2(executable, shQuote(command_arguments), stdout = output_path, stderr = error_path, timeout = timeout))
  result <- list(test = current_test, command = c(executable, command_arguments), cwd = cwd,
    started_at = start, finished_at = timestamp(), exit_status = as.integer(status),
    stdout = text_file(output_path), stderr = text_file(error_path))
  commands[[length(commands) + 1L]] <<- result
  save_results()
  result
}
cli_json <- function(options, cwd = work) {
  command <- run_cli(c(options, "--format", "json"), cwd)
  check(command$exit_status == 0L, paste("JSON CLI succeeds:", command$stderr))
  check(jsonlite::validate(command$stdout), "JSON stdout is exactly one valid JSON document")
  jsonlite::fromJSON(command$stdout, simplifyVector = FALSE)
}
cli_failure <- function(options, cwd = work, pattern = NULL, timeout = 30L) {
  command <- run_cli(options, cwd, timeout = timeout)
  check(command$exit_status == 2L, paste("Invalid CLI exits 2, without timeout:", paste(options, collapse = " ")))
  check(!nzchar(command$stdout) && nzchar(command$stderr), "Failure produces feedback on stderr and no stdout")
  if (!is.null(pattern)) check(grepl(pattern, command$stderr, ignore.case = TRUE), paste("CLI error identifies", pattern))
  invisible(command)
}
real_context <- NULL
real_project <- function() {
  if (!is.null(real_context)) return(real_context)
  base <- file.path(work, "real-import")
  root <- file.path(base, "project")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  input <- file.path(base, "golden_input.csv")
  check(file.copy(golden_path, input), "Private real import copies configured golden dataset")
  original <- file.path(root, "Original.parquet")
  arrow::write_parquet(read.csv(input), original)
  command <- run_cli(c("--project", root, "--source", "Original.parquet",
    "--working", "data/current.parquet", "--name", "golden"),
    cwd = root, module = "project_create", timeout = 120L)
  check(command$exit_status == 0L, paste("Private current project creation succeeds:", command$stderr))
  check(file.exists(file.path(root, marker)), "Creator publishes the current workspace marker")
  check(file.copy(note_path, file.path(root, "research_note.md")), "Ordinary document fixture copied")
  manifest <- yaml::read_yaml(file.path(root, marker), eval.expr = FALSE)
  descriptor <- jsonlite::read_json(file.path(root, ".nlss/datasets", manifest$datasets[[1]]$id, "dataset.json"))
  real_context <<- list(base = base, root = root, input = original, entry = descriptor)
  real_context
}
test <- function(name, code) {
  if (nzchar(match_pattern) && !grepl(match_pattern, name)) return(invisible(NULL))
  current_test <<- name
  current_assertions <<- list()
  old <- getwd()
  old_options <- options("yaml.eval.expr")
  on.exit({ setwd(old); options(old_options) }, add = TRUE)
  reset_config()
  start <- timestamp()
  elapsed <- proc.time()[["elapsed"]]
  error <- tryCatch({ force(code); NULL }, error = function(problem) conditionMessage(problem))
  results[[length(results) + 1L]] <<- list(test = name, passed = is.null(error), started_at = start,
    finished_at = timestamp(), seconds = unname(proc.time()[["elapsed"]] - elapsed),
    message = if (is.null(error)) "OK" else error, assertions = current_assertions)
  save_results()
  cat(if (is.null(error)) "[PASS] " else "[FAIL] ", name,
    if (is.null(error)) "" else paste0(": ", error), "\n", sep = "")
}
save_results()

task_a_fixture <- function(name = "project") {
  root <- file.path(work, "cases", current_test, name)
  write_text(file.path(root, "original.csv"), c("score", "1", "2"))
  write_text(file.path(root, "data/current.csv"), c("score", "2", "3"))
  write_json(list(schema_version = 1L, workspace_id = "task-a", dataset_id = "ds-sample",
    working = "data/current.csv", source = list(selected_path = "original.csv")),
    file.path(root, ".nlss/datasets/ds-sample/dataset.json"))
  yaml::write_yaml(list(schema_version = 2L, storage = "managed_parquet_v1", workspace_id = "task-a",
    active_dataset = "sample", datasets = list(list(name = "sample", id = "ds-sample"))), file.path(root, marker))
  root
}

test("task_f_all_statistical_entrypoints_use_common_contracts", {
  utilities <- c("calc", "research_academia", "metaskill_runner", "init_workspace", "check_integrity", "reconstruct_reports")
  modules <- setdiff(names(configuration$scripts$modules), utilities)
  check(length(modules) == 21L, "Registry includes all 21 statistical entrypoints")
  calls <- function(x) {
    if (!is.call(x) && !is.expression(x) && !is.pairlist(x)) return(character())
    c(if (is.call(x) && is.symbol(x[[1]])) as.character(x[[1]]),
      unlist(lapply(as.list(x), function(child) if (is.call(child) || is.expression(child) || is.pairlist(child)) calls(child) else character()), use.names = FALSE))
  }
  for (module in modules) {
    path <- file.path(repo, "scripts/R", paste0(module, ".R"))
    used <- calls(parse(path))
    check(all(c("nlss_bootstrap", "nlss_run_main", "nlss_begin_run", "nlss_set_result") %in% used),
      paste(module, "uses shared bootstrap, execution, input binding and result publication"))
    check(!any(c("nlss_create_project", "nlss_publish_managed_run", "nlss_append_project_protocol") %in% used),
      paste(module, "does not opt in to a project-specific storage writer"))
  }
})

test("task_a_current_creator_and_separate_locations", {
  root <- file.path(work, "cases", current_test, "project")
  dir.create(root, recursive = TRUE)
  original <- file.path(root, "original.parquet")
  arrow::write_parquet(read.csv(golden_path), original)
  creator <- new.env(parent = globalenv())
  source(file.path(repo, "scripts/R/lib/bootstrap.R"), local = creator)
  creator$nlss_bootstrap(creator)
  creator$source_lib("project_store.R")
  creator$nlss_create_project(root, "original.parquet", "data/current.parquet", "sample")
  locations <- readonly(root, runtime$nlss_resolve_locations(project = root))
  check(identical(locations$project_root, root), "Current creator marker selects its project root")
  check(identical(locations$output_root, file.path(root, ".nlss")), "Output root is .nlss, not the working directory")
  check(identical(locations$working_path, file.path(root, "data/current.parquet")), "Visible working path is unchanged")
  check(identical(locations$source_path, original), "Original source path is unchanged")
  check(file.exists(locations$metadata_path) && basename(locations$metadata_path) == "dataset.json", "Existing descriptor is resolved without another registry")
})

test("task_a_explicit_ancestor_and_missing_selection", {
  root <- task_a_fixture()
  peer <- task_a_fixture("peer")
  deep <- file.path(root, "notes/analysis/deep")
  dir.create(deep, recursive = TRUE)
  check(identical(readonly(root, runtime$nlss_resolve_locations(start = deep))$project_root, root), "Nearest ancestor works from deep directory")
  selected <- readonly(dirname(root), runtime$nlss_resolve_locations(project = file.path(peer, marker), start = deep))
  check(identical(selected$project_root, peer), "Explicit marker wins over cwd project")
  check(identical(readonly(root, runtime$nlss_resolve_locations(project = root, start = file.path(root, "absent")))$project_root, root),
    "Explicit project does not depend on an unused discovery start")
  expect_error(readonly(root, runtime$nlss_resolve_locations(project = file.path(root, "absent"), start = deep)), "existing project")
  expect_error(readonly(root, runtime$nlss_resolve_locations(dataset = "absent", start = deep)), "not registered")
  yaml::write_yaml(list(schema_version = 1L), file.path(dirname(deep), marker))
  expect_error(readonly(root, runtime$nlss_resolve_locations(start = deep)), "Unsupported project marker")
})

test("task_a_dataset_free_and_source_selectors", {
  root <- task_a_fixture()
  for (flag in c("csv", "sav", "rds", "rdata", "parquet", "mids")) {
    opts <- c(list(project = root), setNames(list("user-selected-file"), flag))
    if (flag == "rdata") opts$df <- "user_selected_object"
    before <- opts
    result <- readonly(root, runtime$cli_resolve_locations(opts))
    check(identical(result$output_root, file.path(root, ".nlss")) && is.null(result$working_path), paste(flag, "input is not replaced by active registered dataset"))
    check(identical(opts, before), paste(flag, "source/import options remain unchanged"))
  }
  selected <- readonly(root, runtime$cli_resolve_locations(list(project = root, dataset = "sample")))
  check(identical(selected$working_path, file.path(root, "data/current.csv")), "Registered location resolution is independent of file extension")
  parameters <- readonly(root, runtime$cli_resolve_locations(list(project = root), use_dataset = FALSE))
  check(is.null(parameters$dataset_name) && is.null(parameters$metadata_path) && is.null(parameters$working_path), "Parameter-only selection does not invent or resolve a dataset")
  expect_error(readonly(root, runtime$cli_resolve_locations(list(project = root, dataset = "sample"), use_dataset = FALSE)), "not applicable")
  expect_error(readonly(root, runtime$cli_resolve_locations(list(project = root, dataset = "sample", csv = "input.csv"))), "not both")
})

test("task_a_unmarked_standalone_is_readonly", {
  root <- task_a_fixture("child-project")
  base <- dirname(root)
  result <- readonly(base, runtime$nlss_resolve_locations(start = base))
  check(is.null(result$project_root) && is.null(result$manifest_path), "Child project is not silently selected or adopted")
  check(is.null(result$working_path) && !file.exists(file.path(base, marker)), "Standalone resolution creates no working copy or marker")
  expected <- runtime$normalize_input_path(runtime$get_config_value("defaults.output_dir"))
  if (!runtime$is_absolute_path(expected)) expected <- file.path(base, expected)
  check(identical(result$output_root, runtime$normalize_path(expected)), "Standalone output uses configuration without creating the target")
  expect_error(readonly(base, runtime$nlss_resolve_locations(dataset = "sample", start = base)), "requires a project")
  expect_error(readonly(base, runtime$nlss_locate_project(start = base)), "No project marker")
  check(is.null(readonly(base, runtime$nlss_locate_project(start = base, required = FALSE))), "Optional discovery returns absence without suppressing invalid selection")
})

test("task_a_root_move_and_missing_working_file", {
  root <- task_a_fixture()
  before <- readonly(root, runtime$nlss_resolve_locations(project = root))
  old_tree <- tree_state(root)
  moved <- file.path(dirname(root), "moved study ü")
  check(file.rename(root, moved), "Private project root moves as one folder")
  check(identical(old_tree, tree_state(moved)), "Whole-root move preserves original, working and metadata bytes")
  after <- readonly(moved, runtime$nlss_resolve_locations(project = moved))
  for (key in c("manifest_path", "output_root", "working_path", "metadata_path", "source_path")) {
    check(identical(substring(before[[key]], nchar(root) + 1L), substring(after[[key]], nchar(moved) + 1L)), paste(key, "retains its registered relative location"))
  }
  check(file.rename(after$working_path, paste0(after$working_path, ".user-moved")), "User moves the private working fixture")
  missing <- readonly(moved, runtime$nlss_resolve_locations(project = moved))
  check(identical(missing$working_path, after$working_path) && !file.exists(missing$working_path), "Resolver returns the registration, without reading data or inventing a relink")
})

test("task_a_unsupported_and_invalid_metadata_no_writes", {
  root <- task_a_fixture()
  yaml::write_yaml(list(schema_version = 1L), file.path(root, marker))
  expect_error(readonly(root, runtime$nlss_resolve_locations(project = root)), "Unsupported project marker")
  root <- task_a_fixture()
  write_text(file.path(root, marker), "schema_version: [")
  expect_error(readonly(root, runtime$nlss_resolve_locations(project = root)), "Cannot read project marker")
  root <- task_a_fixture()
  write_json(list(schema_version = 1L, workspace_id = "another-project", dataset_id = "ds-sample"),
    file.path(root, ".nlss/datasets/ds-sample/dataset.json"))
  expect_error(readonly(root, runtime$nlss_resolve_locations(project = root)), "identity mismatch")
  parameters <- readonly(root, runtime$nlss_resolve_locations(project = root, use_dataset = FALSE))
  check(is.null(parameters$metadata_path), "Dataset-free resolution does not open an unrelated descriptor")
})

test("task_a_direct_io_source_without_bootstrap", {
  root <- task_a_fixture()
  isolated <- new.env(parent = globalenv())
  previous <- getwd()
  setwd(file.path(repo, "scripts/R/lib"))
  source("io.R", local = isolated)
  setwd(previous)
  check(!exists("nlss_locate_project", envir = isolated, inherits = FALSE), "Standalone io source need not preload inspector")
  selected <- readonly(root, isolated$nlss_resolve_locations(project = root))
  check(identical(selected$project_root, root), "Lazy discovery library stays anchored after cwd changes")
})

test("discovery_nearest_ancestor_no_child_or_sibling_search", {
  context <- fixture()
  deep <- file.path(context$root, "a/b/c")
  dir.create(deep, recursive = TRUE)
  write_manifest(file.path(context$base, "sibling"))
  write_manifest(file.path(context$root, "unrelated-child"))
  located <- readonly(context$base, runtime$nlss_locate_project(start = deep))
  check(identical(located, file.path(context$root, marker)), "Deep discovery selects nearest ancestor despite child and sibling markers")
  nested <- file.path(context$root, "a/b")
  write_manifest(nested)
  check(identical(readonly(context$base, runtime$nlss_locate_project(start = deep)), file.path(nested, marker)),
    "A nearer nested marker wins without inspecting neighbouring projects")
  expect_error(readonly(context$base, runtime$nlss_locate_project(start = context$base)), "No project marker")
})

test("explicit_directory_marker_and_missing_selection_no_fallback", {
  context <- fixture()
  directory <- inspect(context)
  file_selection <- readonly(context$base, runtime$nlss_inspect_project(file.path(context$root, marker)))
  check(identical(directory$project_root, file_selection$project_root), "Explicit directory and marker select the same root")
  readonly(context$base, cli_failure(c("--project", file.path(context$root, "absent")), context$root))
  readonly(context$base, cli_failure(c("--project", file.path(context$root, "absent", marker)), context$root))
})

test("marker_symlink_directory_and_fifo_are_rejected", {
  context <- fixture()
  for (kind in c("directory", "symlink", "fifo")) {
    root <- file.path(context$base, kind)
    dir.create(root)
    target <- file.path(root, marker)
    if (kind == "directory") dir.create(target)
    if (kind == "symlink") {
      if (.Platform$OS.type == "windows") {
        if (!isTRUE(suppressWarnings(file.symlink(file.path(context$root, marker), target)))) {
          current_assertions[[length(current_assertions) + 1L]] <- list(assertion = "Marker symlink requires OS symlink permission", skipped = TRUE)
          next
        }
      } else check(file.symlink(file.path(context$root, marker), target), "Private marker symlink created")
    }
    if (kind == "fifo") {
      if (.Platform$OS.type == "windows" || !nzchar(Sys.which("mkfifo"))) {
        current_assertions[[length(current_assertions) + 1L]] <- list(assertion = "FIFO case requires POSIX mkfifo", skipped = TRUE)
        next
      }
      check(system2(Sys.which("mkfifo"), shQuote(target)) == 0L, "Private FIFO marker created")
    }
    readonly(context$base, cli_failure(c("--project", root), pattern = "regular file", timeout = 8L))
  }
})

test("current_manifest_arrays_and_active_selection", {
  context <- fixture()
  forms <- list(list(), list(list(name = "one", id = "ds-one")),
    list(list(name = "one", id = "ds-one"), list(name = "two", id = "ds-two")))
  for (index in seq_along(forms)) {
    write_manifest(context$root, forms[[index]])
    result <- readonly(context$base, cli_json(c("--project", context$root)))
    check(is.list(result$datasets) && is.null(names(result$datasets)) && length(result$datasets) == index - 1L,
      paste("Current marker serializes", index - 1L, "dataset registrations as an array"))
  }
  write_manifest(context$root, forms[[2]], "missing")
  check(!inspect(context)$active_dataset_registered, "Unknown active selection remains visibly unregistered")
})

test("malformed_manifest_shapes_duplicate_names_and_schema", {
  context <- fixture()
  for (datasets in list("bad", list("bad"), list(list(name = c("one", "two"))),
      list(list(name = "same"), list(name = "same")), list(list(name = "")))) {
    write_manifest(context$root, datasets)
    expect_error(inspect(context))
  }
  write_text(file.path(context$root, marker), "schema_version: [broken")
  expect_error(inspect(context), "marker")
  write_text(file.path(context$root, marker), c("schema_version: 99", "datasets: []"))
  expect_error(inspect(context), "schema")
  write_manifest(context$root, list(list(name = "one", id = "../outside")))
  expect_error(inspect(context), "ID")
})

test("yaml_expressions_remain_inert_with_global_evaluation_enabled", {
  context <- fixture()
  sentinel <- file.path(context$root, "expression-executed")
  expression <- paste0("file.create(", deparse(sentinel), ")")
  write_text(file.path(context$root, marker), c("schema_version: 2", "storage: managed_parquet_v1", "workspace_id: test", "datasets: []", paste("unrelated_field: !expr", expression)))
  options(yaml.eval.expr = TRUE)
  readonly(context$base, suppressWarnings(runtime$nlss_inspect_project(context$root)))
  check(!file.exists(sentinel), "YAML !expr is never executed even when the process option enables evaluation")
})

test("reference_paths_anchor_at_project_root_and_exclude_external_paths", {
  context <- fixture(with_data = TRUE)
  deep <- file.path(context$root, "sample/deep")
  dir.create(deep, recursive = TRUE)
  result <- readonly(context$base, runtime$nlss_inspect_project(start = deep, verify = TRUE))
  check(result$datasets[[1]]$snapshot$comparison == "matches_recorded", "Snapshot paths remain project-relative from dataset subdirectories")
  check(result$datasets[[1]]$dictionary$comparison == "matches_recorded", "Dictionary paths remain project-relative")
  external <- write_text(file.path(context$base, "outside.csv"), "external fixture bytes")
  for (path in c(external, "../outside.csv", "sample//data.parquet")) {
    value <- readonly(context$base, runtime$nlss_inspect_file(context$root, path, verify = TRUE))
    check(value$status %in% c("external_not_inspected", "unsafe_path") && is.null(value$sha256),
      "Outside and unsafe paths cannot trigger hashing")
  }
  value <- readonly(context$base, runtime$nlss_inspect_file(context$root, file.path(context$root, context$entry$parquet), verify = TRUE))
  check(value$comparison == "no_recorded_hash", "An absolute path within the selected project is supported")
  check(runtime$nlss_project_path("/", "tmp")$absolute == "/tmp", "Filesystem root is not stripped to a relative path")
})

test("symlink_and_nested_project_reference_contents_are_not_read", {
  context <- fixture(with_data = TRUE)
  outside <- write_text(file.path(context$base, "outside.csv"), "external source")
  linked <- file.path(context$root, "linked.csv")
  if (isTRUE(suppressWarnings(file.symlink(outside, linked)))) {
    value <- readonly(context$base, runtime$nlss_inspect_file(context$root, "linked.csv", verify = TRUE))
    check(value$status == "symlink_not_inspected" && is.null(value$sha256), "Symlink content is not hashed")
  } else current_assertions[[length(current_assertions) + 1L]] <- list(assertion = "Symlink requires OS permission", skipped = TRUE)
  child <- file.path(context$root, "child")
  write_manifest(child)
  write_text(file.path(child, "data.parquet"), "nested scientific bytes")
  original_hash <- runtime$import_hash
  runtime$import_hash <- function(...) stop("Forbidden content hash in nested project")
  value <- tryCatch(readonly(context$base, runtime$nlss_inspect_file(context$root, "child/data.parquet", verify = TRUE)),
    finally = { runtime$import_hash <- original_hash })
  check(value$status == "other_project_not_inspected", "Nested project boundary is recognized before any content hashing")
})

test("metadata_directories_and_nonregular_inputs_are_not_valid_files", {
  context <- fixture(with_data = TRUE)
  check(file.rename(context$descriptor, paste0(context$descriptor, ".saved")), "Private descriptor moved aside")
  dir.create(context$descriptor)
  check(inspect(context)$datasets[[1]]$descriptor$status == "not_a_file", "Metadata directory is not parsed")
  dir.create(file.path(context$root, "directory.parquet"))
  check(runtime$nlss_inspect_file(context$root, "directory.parquet")$status == "not_a_file", "Data directory is not a file")
  if (.Platform$OS.type != "windows" && nzchar(Sys.which("mkfifo"))) {
    fifo <- file.path(context$root, "stream.parquet")
    check(system2(Sys.which("mkfifo"), shQuote(fifo)) == 0L, "Private FIFO input created")
    value <- readonly(context$base, runtime$nlss_inspect_file(context$root, "stream.parquet", verify = TRUE))
    check(value$status == "not_a_file" && is.null(value$sha256), "FIFO rejected before hashing")
  }
})

test("current_descriptors_missing_malformed_and_mismatched_are_visible", {
  context <- fixture(with_data = TRUE)
  check(file.rename(context$descriptor, paste0(context$descriptor, ".saved")), "Private descriptor moved aside")
  check(inspect(context)$datasets[[1]]$descriptor$status == "missing", "Missing descriptor remains missing")
  for (value in c("{bad", "[]", '{"source":"bad"}')) {
    write_text(context$descriptor, value)
    check(inspect(context)$datasets[[1]]$descriptor$status == "invalid_metadata", "Malformed descriptor stays visible")
  }
  value <- jsonlite::read_json(paste0(context$descriptor, ".saved"))
  value$workspace_id <- "other-project"
  write_json(value, context$descriptor)
  result <- inspect(context)$datasets[[1]]
  check(result$descriptor$status == "identity_mismatch" && result$working$status == "unrecorded",
    "Mismatched descriptor does not authorize following its references")
})

test("data_hashing_is_opt_in_and_invalid_expected_hashes_are_explicit", {
  context <- fixture(with_data = TRUE)
  original_hash <- runtime$import_hash
  runtime$import_hash <- function(...) stop("Default inspection must not hash data")
  result <- tryCatch(inspect(context), finally = { runtime$import_hash <- original_hash })
  check(result$verification_scope == "paths_and_sizes_only", "Default scope explicitly limits verification to paths and sizes")
  for (name in c("working", "snapshot", "dictionary", "preserved_source")) {
    file <- result$datasets[[1]][[name]]
    check(file$comparison == "not_checked" && is.null(file$sha256), paste(name, "does not claim hash verification by default"))
  }
  value <- readonly(context$base, runtime$nlss_inspect_file(context$root, context$entry$parquet, "invalid", verify = TRUE))
  check(value$comparison == "invalid_recorded_hash", "Malformed recorded hash is distinguished from a data mismatch")
  value <- readonly(context$base, runtime$nlss_inspect_file(context$root, context$entry$parquet, verify = TRUE))
  check(value$comparison == "no_recorded_hash" && nzchar(value$sha256), "Current hash alone does not invent provenance")
})

test("snapshot_and_dictionary_hashes_distinguish_tamper_from_missing", {
  context <- fixture(with_data = TRUE)
  original <- inspect(context, verify = TRUE)$datasets[[1]]
  check(original$snapshot$comparison == "matches_recorded" && original$dictionary$comparison == "matches_recorded", "Original immutable references match actual bytes")
  write_text(file.path(context$root, context$entry$dataset$snapshot_path), "Changed snapshot fixture")
  write_text(file.path(context$root, context$entry$dataset$dictionary_path), "Changed dictionary fixture")
  changed <- inspect(context, verify = TRUE)$datasets[[1]]
  check(changed$snapshot$comparison == "differs_from_recorded" && changed$dictionary$comparison == "differs_from_recorded", "Snapshot and dictionary tampering are independently visible")
  path <- file.path(context$root, context$entry$dataset$snapshot_path)
  check(file.rename(path, paste0(path, ".saved")), "Private snapshot moved aside")
  check(inspect(context, verify = TRUE)$datasets[[1]]$snapshot$status == "missing", "Missing snapshot does not receive an integrity verdict")
})

test("preserved_source_hash_is_checked_against_current_descriptor", {
  context <- fixture(with_data = TRUE)
  original <- inspect(context, verify = TRUE)$datasets[[1]]$preserved_source
  check(original$comparison == "matches_recorded", "Preserved source matches the descriptor hash")
  write_text(file.path(context$root, original$path), "Changed preserved source fixture")
  check(inspect(context, verify = TRUE)$datasets[[1]]$preserved_source$comparison == "differs_from_recorded", "Changed preserved source is detected")
  binding_path <- context$descriptor
  binding <- jsonlite::read_json(binding_path, simplifyVector = FALSE)
  binding$source$sha256 <- "bad"
  write_json(binding, binding_path)
  check(inspect(context, verify = TRUE)$datasets[[1]]$preserved_source$comparison == "invalid_recorded_hash", "Invalid source hash is not silently accepted")
})

test("stale_manifest_working_difference_is_not_called_corruption", {
  context <- fixture(with_data = TRUE)
  write_text(file.path(context$root, context$entry$parquet), "Legitimately changed editable working bytes")
  result <- inspect(context, verify = TRUE)
  dataset <- result$datasets[[1]]
  check(dataset$working$comparison == "differs_from_recorded" && dataset$snapshot$comparison == "matches_recorded", "Working mismatch and intact earlier snapshot are distinguished")
  check(dataset$reference_basis == "initial_registered_version_not_latest", "Manifest version is explicitly qualified")
  markdown <- paste(runtime$nlss_inspection_markdown(result), collapse = "\n")
  check(grepl("not necessarily latest", markdown, fixed = TRUE) && grepl("do not prove an external edit", markdown, fixed = TRUE), "Rendered explanation does not infer corruption or external edits")
})

test("cli_invalid_options_are_clear_and_write_nothing", {
  context <- fixture()
  invalid <- list(c("--unknown"), c("--project"), c("--note"), c("--verify", "perhaps"),
    c("--format", "xml"), c("--format", "json", "--format", "markdown"), c("unexpected-positional"),
    c("--log", "TRUE"), c("--out-dir", context$root))
  for (options in invalid) readonly(context$base, cli_failure(options, context$root))
  command <- readonly(context$base, run_cli("--help", context$root))
  check(command$exit_status == 0L && grepl("--project", command$stdout, fixed = TRUE), "Help documents the standalone selection option")
})

test("cli_configured_defaults_and_explicit_overrides", {
  context <- fixture(with_data = TRUE)
  write_text(file.path(context$root, "alternate-note.md"), "Configured scientific context")
  configuration <- baseline_config
  configuration$modules$project_inspect <- list(format = "json", verify = TRUE)
  yaml::write_yaml(configuration, private_config)
  result <- readonly(context$base, run_cli(c("--project", context$root)))
  check(result$exit_status == 0L && jsonlite::validate(result$stdout), "Configured format selects clean JSON output")
  decoded <- jsonlite::fromJSON(result$stdout, simplifyVector = FALSE)
  check(decoded$datasets[[1]]$working$comparison == "matches_recorded", "Configured verification default is honored")
  result <- readonly(context$base, run_cli(c("--project", context$root, "--verify", "FALSE", "--format", "markdown")))
  check(result$exit_status == 0L && startsWith(result$stdout, "# NLSS project inspection"), "Explicit format overrides its configured default")
  check(grepl("paths_and_sizes_only", result$stdout, fixed = TRUE), "Explicit verification overrides configured value")
})

test("real_cli_json_markdown_outputs_preserve_entire_project", {
  context <- real_project()
  json <- readonly(context$base, cli_json(c("--project", context$root)))
  check(json$read_only && json$kind == "project_inspection" && length(json$datasets) == 1L, "Real project has one structured read-only inspection result")
  check(!"research_note" %in% names(json), "CLI does not discover or fingerprint a user document")
  result <- readonly(context$base, run_cli(c("--project", file.path(context$root, marker), "--format", "markdown")))
  check(result$exit_status == 0L && startsWith(result$stdout, "# NLSS project inspection"), "Real project also renders Markdown to stdout")
  check(!nzchar(result$stderr), "Successful ordinary inspection does not emit unexpected diagnostics")
})



test("object_store_and_unregistered_contents_are_not_walked", {
  context <- fixture()
  write_text(file.path(context$root, ".nlss/objects/deep/invalid.json"), "unread object")
  write_text(file.path(context$root, "unregistered/deep/import.json"), "unread unrelated metadata")
  original_listing <- get("list.files", envir = baseenv())
  assign("list.files", function(path = ".", ...) {
    if (grepl("objects|unregistered", path)) stop("Forbidden descendant listing")
    original_listing(path, ...)
  }, envir = runtime)
  result <- tryCatch(inspect(context), finally = { rm("list.files", envir = runtime) })
  check(length(result$datasets) == 0L, "Object/unregistered contents are not traversed or adopted")
})

test("all_loader_and_publication_helpers_are_bypassed", {
  context <- fixture(with_data = TRUE)
  result <- inspect(context, verify = TRUE)
  check(result$datasets[[1]]$snapshot$comparison == "matches_recorded", "Inspection succeeds while every discovery/load/publish helper is trapped")
  check(!file.exists(file.path(context$root, "sample/.import-lock")), "Inspection does not acquire or create an import lock")
})

test("real_import_references_match_golden_source_snapshot_dictionary", {
  context <- real_project()
  result <- readonly(context$base, cli_json(c("--project", context$root, "--verify", "TRUE")))
  dataset <- result$datasets[[1]]
  check(result$active_dataset == "golden" && dataset$name == "golden", "Current creator active dataset and registry names are preserved")
  for (name in c("working", "snapshot", "dictionary", "preserved_source")) {
    check(dataset[[name]]$comparison == "matches_recorded", paste("Real", name, "matches its actual saved SHA-256"))
  }
  check(identical(dataset$preserved_source$sha256, sha256(context$input)), "Preserved source independently matches its original Parquet bytes")
  check(dataset$dataset_id == context$entry$dataset_id && dataset$recorded_version_id == context$entry$initial_version$version_id,
    "Real dataset ID and recorded version are represented without invention")
})

test("real_project_root_rename_preserves_relative_references", {
  context <- real_project()
  before <- readonly(context$base, cli_json(c("--project", context$root, "--verify", "TRUE")))
  previous_tree <- tree_state(context$root)
  renamed <- file.path(context$base, "renamed project with spaces")
  check(file.rename(context$root, renamed), "Private real project root renamed")
  real_context$root <- renamed
  context$root <- renamed
  check(identical(previous_tree, tree_state(renamed)), "Root rename preserves project contents and metadata")
  after <- readonly(context$base, cli_json(c("--project", renamed, "--verify", "TRUE")))
  check(identical(before$datasets, after$datasets),
    "Current dataset references survive root rename unchanged")
  check(after$project_root == normalizePath(renamed, winslash = "/"), "Output identifies the renamed root")
})

passed <- sum(vapply(results, function(result) result$passed, logical(1)))
source_stable <- identical(initial_hashes, source_hashes())
exit_status <- if (length(results) && passed == length(results) && source_stable) 0L else 1L
save_results(exit_status, timestamp())
cat(sprintf("Phase 3 project: %d/%d grouped cases passed. Results: %s\n", passed, length(results), summary_path))
if (!length(results)) cat("No cases matched --match; this is not a passing run.\n")
if (!source_stable) cat("Source identity changed during this run; results cannot certify a stable revision.\n")

# Only old, positively identified phase3 runner directories are eligible for
# retention. Other suites, ordinary fixtures and explicit caller roots survive.
if (!nzchar(forced_root) && keep > 0L && dir.exists(output_base)) {
  timestamp_roots <- list.dirs(output_base, full.names = TRUE, recursive = FALSE)
  timestamp_roots <- timestamp_roots[grepl("^[0-9]{14}$", basename(timestamp_roots))]
  timestamp_roots <- timestamp_roots[!vapply(timestamp_roots, function(path) {
    link <- Sys.readlink(path)
    !is.na(link) && nzchar(link)
  }, logical(1))]
  candidates <- unlist(lapply(timestamp_roots, function(root) {
    phase <- file.path(root, "phase3")
    if (!dir.exists(phase) || nzchar(Sys.readlink(phase))) return(character())
    list.dirs(phase, full.names = TRUE, recursive = FALSE)
  }), use.names = FALSE)
  candidates <- sort(candidates[grepl("^run-[0-9]{14}-[0-9]+$", basename(candidates))], decreasing = TRUE)
  if (length(candidates) > keep) for (candidate in candidates[seq.int(keep + 1L, length(candidates))]) {
    if (identical(candidate, work) || nzchar(Sys.readlink(candidate))) next
    previous <- tryCatch(jsonlite::read_json(file.path(candidate, "results.json")), error = function(e) NULL)
    if (identical(previous$owner, "nlss-project-test-runner") && identical(previous$suite, "phase3-project") &&
        !is.null(previous$finished_at) && !is.null(previous$exit_status)) unlink(candidate, recursive = TRUE)
  }
}
quit(status = exit_status)
