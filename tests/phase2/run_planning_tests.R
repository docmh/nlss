#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Independent public-CLI acceptance of parameter-only study-planning runs.
args <- commandArgs(TRUE)
script <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])
repo <- normalizePath(file.path(dirname(script[1]), "../.."), winslash = "/")
script <- normalizePath(script[1], winslash = "/")
if ("--help" %in% args) {
  cat("Usage: Rscript tests/phase2/run_planning_tests.R [--root PATH] [--keep N] [--match REGEX]\n",
      "Honors NLSS_TEST_ROOT, NLSS_KEEP_RUNS and NLSS_TESTS_CONFIG; private offline projects/configuration.\n", sep = "")
  quit(status = 0)
}
if (length(args) %% 2L || (length(args) && any(!args[seq(1L, length(args), 2L)] %in% c("--root", "--keep", "--match")))) stop("Unknown/incomplete runner option")
arg <- function(name, fallback) {
  i <- which(args == name)
  if (length(i) > 1L) stop("Repeated runner option: ", name)
  if (length(i)) args[i + 1L] else fallback
}
required <- c("yaml", "jsonlite", "digest", "pwr", "semPower", "arrow")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing test packages: ", paste(missing, collapse = ", "))
absolute <- function(path) normalizePath(if (grepl("^(/|[A-Za-z]:|\\\\)", path)) path else file.path(repo, path), winslash = "/", mustWork = FALSE)
cfg <- yaml::read_yaml(absolute(Sys.getenv("NLSS_TESTS_CONFIG", "tests/tests.yml")), eval.expr = FALSE)$tests
test_pattern <- arg("--match", ".*")
invisible(grepl(test_pattern, "validate regular expression"))
forced_root <- arg("--root", Sys.getenv("NLSS_TEST_ROOT", ""))
output_base <- absolute(cfg$output_dir)
run_root <- if (nzchar(forced_root)) absolute(forced_root) else file.path(output_base, format(Sys.time(), "%Y%m%d%H%M%S"))
keep <- suppressWarnings(as.integer(arg("--keep", Sys.getenv("NLSS_KEEP_RUNS", as.character(cfg$keep_runs_default)))))
if (length(keep) != 1L || is.na(keep) || keep < 0L) stop("Invalid --keep")
work <- file.path(run_root, "phase2-planning", paste0("run-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid()))
dir.create(work, recursive = TRUE)
baseline <- yaml::read_yaml(file.path(repo, "scripts/config.yml"), eval.expr = FALSE)
private_config <- file.path(work, "config.yml")
reset_config <- function() yaml::write_yaml(baseline, private_config)
reset_config()
Sys.setenv(NLSS_CONFIG_PATH = private_config, OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1")
text_file <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
read_json <- function(path) jsonlite::fromJSON(path, simplifyVector = TRUE)
raw_json <- function(path) jsonlite::fromJSON(path, simplifyVector = FALSE)
write_json <- function(object, path) jsonlite::write_json(object, path, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null", na = "null")
sha <- function(path) digest::digest(file = path, algo = "sha256")
check <- function(ok, message) if (!isTRUE(ok)) stop(message, call. = FALSE)
results <- list()
test <- function(name, code) {
  if (!grepl(test_pattern, name)) return(invisible(NULL))
  previous <- getwd(); on.exit(setwd(previous), add = TRUE)
  reset_config(); start <- proc.time()[["elapsed"]]
  error <- tryCatch({ force(code); NULL }, error = function(e) conditionMessage(e))
  results[[length(results) + 1L]] <<- list(module = "planning", test = name, passed = is.null(error),
    seconds = unname(proc.time()[["elapsed"]] - start), message = if (is.null(error)) "OK" else error)
  cat(if (is.null(error)) "[PASS] " else "[FAIL] ", name, if (is.null(error)) "" else paste0(": ", error), "\n", sep = "")
}
new_case <- function(name, manifest = TRUE) {
  base <- file.path(work, "cases", name); project <- file.path(base, "project")
  dir.create(project, recursive = TRUE)
  if (manifest) yaml::write_yaml(list(version = 1L, datasets = list()), file.path(project, "nlss-workspace.yml"))
  setwd(project)
  list(base = base, project = project, planning = file.path(project, "planning"), manifest = file.path(project, "nlss-workspace.yml"))
}
run_module <- function(module, options, failure = FALSE) {
  log <- tempfile(module, tmpdir = work, fileext = ".log")
  status <- system2(file.path(R.home("bin"), "Rscript"), c(shQuote(file.path(repo, "scripts/R", paste0(module, ".R"))), shQuote(options)), stdout = log, stderr = log)
  check(if (failure) status != 0L else status == 0L, paste(module, "unexpected exit", status, text_file(log)))
  invisible(log)
}
runs <- function(context) {
  paths <- list.files(context$project, "^request[.]json$", recursive = TRUE, full.names = TRUE)
  dirname(paths[grepl("/runs/[^.][^/]+/request[.]json$", paths)])
}
base_options <- c("--analysis", "ttest", "--mode", "apriori", "--effect-size", "0.5", "--power", "0.8")
new_run <- function(context, options = base_options, module = "power", planning = TRUE) {
  before <- runs(context)
  log <- run_module(module, options)
  added <- setdiff(runs(context), before)
  check(length(added) == 1L, "Did not publish exactly one new terminal run")
  request_path <- file.path(added, "request.json"); result_path <- file.path(added, "result.json")
  request <- read_json(request_path); result <- read_json(result_path)
  check(identical(result$status, "completed") && isTRUE(request$resolved), "Run is not a resolved completed analysis")
  check(identical(request$run_id, basename(added)) && identical(result$run_id, request$run_id), "Run directory/identities differ")
  check(identical(result$artifacts$request$sha256, sha(request_path)), "Request/result hash association differs")
  check(length(result$results) > 0L && length(request$options) > 0L && length(request$environment$packages) > 0L, "Resolved scientific/environment context is missing")
  for (artifact in result$artifacts) check(identical(sha(file.path(added, artifact$path)), artifact$sha256), "Artifact SHA-256 differs")
  check(length(request$templates) > 0L, "Run has no preserved template")
  for (template in request$templates) check(identical(sha(file.path(added, template$path)), template$sha256), "Template SHA-256 differs")
  if (planning) {
    check(identical(as.integer(request$schema_version), 2L) && identical(as.integer(result$schema_version), 2L), "Planning run does not use schema v2")
    check(identical(request$input$kind, "parameters") && identical(result$input$kind, "parameters"), "Planning input kind is not parameters")
    check(is.null(request$dataset) && is.null(result$dataset), "Planning run invents or binds a dataset")
    check(identical(dirname(dirname(added)), context$planning), "Planning output is not project/planning/runs/id")
    marker <- read_json(file.path(context$planning, ".nlss-planning.json"))
    check(identical(as.integer(marker$schema_version), 1L) && identical(marker$kind, "parameters"), "Planning ownership marker is missing/invalid")
  } else {
    check(identical(as.integer(request$schema_version), 1L) && identical(as.integer(result$schema_version), 1L), "Existing dataset-backed schema changed")
    check(!is.null(request$dataset), "Explicit dataset run lacks its immutable reference")
    for (pair in list(c("snapshot_path", "data_sha256"), c("dictionary_path", "dictionary_sha256"))) check(
      identical(sha(file.path(context$project, request$dataset[[pair[1]]])), request$dataset[[pair[2]]]), "Dataset snapshot/dictionary hash differs")
  }
  check(!dir.exists(file.path(dirname(dirname(added)), ".analysis-lock")) && !dir.exists(file.path(context$project, ".publication-lock")), "Completed run retained a lock")
  list(path = added, request_path = request_path, request = request, result = result,
       markdown = text_file(file.path(added, "output.md")), log = log)
}
snapshot <- function(paths) {
  existing <- paths[file.exists(paths) & !dir.exists(paths)]
  setNames(vapply(existing, sha, character(1)), existing)
}
protected <- function(context) file.path(context$planning, c("report_canonical.md", "analysis_log.jsonl"))
tree_snapshot <- function(path) {
  files <- sort(list.files(path, recursive = TRUE, full.names = TRUE, all.files = TRUE))
  snapshot(files)
}
replay <- function(context, bundle, planning = TRUE) {
  before <- tree_snapshot(bundle$path)
  output <- new_run(context, c("--request", bundle$request_path), module = "replay_run", planning = planning)
  check(identical(output$result$results, bundle$result$results), "Replay changed raw statistical results")
  check(identical(output$markdown, bundle$markdown), "Replay changed deterministic Markdown")
  check(identical(output$request$replay_of, bundle$request$run_id), "Replay origin is not recorded")
  check(identical(before, tree_snapshot(bundle$path)), "Replay modified its selected original run")
  output
}
failed <- function(context, options, module = "power") {
  paths <- c(context$manifest, protected(context))
  before_files <- snapshot(paths); before_runs <- runs(context)
  log <- run_module(module, options, failure = TRUE)
  check(identical(before_files, snapshot(paths)), "Rejected request changed protected manifest/report/log")
  added <- setdiff(runs(context), before_runs)
  for (path in added) {
    result <- read_json(file.path(path, "result.json"))
    check(identical(result$status, "failed") && !is.null(result$error) && is.null(result$results), "Invalid request published a successful result")
    check(!file.exists(file.path(path, "output.md")), "Failed run has normal output.md")
    run_module("replay_run", c("--request", file.path(path, "request.json")), failure = TRUE)
  }
  invisible(list(log = log, runs = added))
}
seed_dataset <- function(context, dataset_name = "sample") {
  input <- file.path(context$base, "sample.rds")
  saveRDS(data.frame(x = seq_len(20L), y = c(seq_len(10L), seq_len(10L) + 2), group = rep(c("A", "B"), each = 10L)), input)
  run_module("init_workspace", c("--rds", input, "--dataset-name", dataset_name))
  run_module("descriptive_stats", c("--rds", input, "--dataset-name", dataset_name, "--vars", "x"))
  manifest <- yaml::read_yaml(context$manifest, eval.expr = FALSE)
  check(identical(manifest$active_dataset, dataset_name), "Private fixture did not register its active dataset")
  list(input = input, directory = file.path(context$project, dataset_name))
}
resign_request <- function(bundle, mutate) {
  request <- mutate(raw_json(bundle$request_path))
  write_json(request, bundle$request_path)
  result_path <- file.path(bundle$path, "result.json")
  result <- raw_json(result_path); result$artifacts$request$sha256 <- sha(bundle$request_path)
  write_json(result, result_path)
}
assert_no_dataset <- function(context) {
  files <- list.files(context$planning, recursive = TRUE, all.files = TRUE)
  check(!any(grepl("[.]parquet$|(^|/)(dictionary[.]json|import[.]json|codebook[.]md|versions|sources)(/|$)", files)), "Planning created fake imported data artifacts")
  if (file.exists(context$manifest)) {
    manifest <- yaml::read_yaml(context$manifest, eval.expr = FALSE)
    check(!"planning" %in% names(manifest$datasets), "Planning was registered as a dataset")
    check(!identical(manifest$active_dataset, "planning"), "Planning became the active dataset")
  }
}
with_private_library <- function(context, packages, code) {
  library <- file.path(context$base, "private-library"); dir.create(library)
  for (package in packages) check(file.symlink(find.package(package), file.path(library, package)), "Cannot create a private dependency-isolation library")
  variables <- c("R_LIBS", "R_LIBS_USER", "R_LIBS_SITE", "R_PROFILE_USER")
  previous <- Sys.getenv(variables, unset = NA_character_)
  on.exit(for (variable in variables) {
    if (is.na(previous[[variable]])) Sys.unsetenv(variable) else do.call(Sys.setenv, setNames(list(previous[[variable]]), variable))
  }, add = TRUE)
  Sys.setenv(R_LIBS = library, R_LIBS_USER = library, R_LIBS_SITE = "", R_PROFILE_USER = "")
  force(code)
}

test("planning_empty_project_smoke", {
  context <- new_case("empty-project"); manifest_hash <- sha(context$manifest)
  bundle <- new_run(context)
  check(identical(manifest_hash, sha(context$manifest)), "Planning modified an empty project dataset manifest")
  assert_no_dataset(context)
  replay(context, bundle)
})
test("planning_explicit_true_smoke", {
  context <- new_case("explicit-planning")
  new_run(context, c(base_options, "--planning", "TRUE"))
  assert_no_dataset(context)
})
test("planning_configured_root_without_manifest", {
  context <- new_case("configured-root", manifest = FALSE)
  configuration <- baseline; configuration$defaults$output_dir <- context$project
  yaml::write_yaml(configuration, private_config)
  bundle <- new_run(context)
  assert_no_dataset(context)
  replay(context, bundle)
})
test("planning_ignores_active_dataset_smoke", {
  context <- new_case("active-dataset"); dataset <- seed_dataset(context)
  before <- tree_snapshot(dataset$directory); manifest_hash <- sha(context$manifest)
  bundle <- new_run(context)
  check(identical(before, tree_snapshot(dataset$directory)) && identical(manifest_hash, sha(context$manifest)), "Automatic planning modified or loaded the active dataset")
  check(!grepl("sample.rds|sample.parquet|sample/versions", text_file(bundle$request_path)), "Unrelated active dataset leaked into planning request")
  assert_no_dataset(context)
})
test("planning_ignores_current_dataset_directory", {
  context <- new_case("dataset-cwd"); dataset <- seed_dataset(context)
  before <- tree_snapshot(dataset$directory); setwd(dataset$directory)
  new_run(context)
  check(identical(before, tree_snapshot(dataset$directory)), "Planning from dataset cwd modified or bound that dataset")
})
test("planning_directory_cwd_reuses_project", {
  context <- new_case("planning-cwd"); new_run(context)
  setwd(context$planning); new_run(context)
  check(!dir.exists(file.path(context$planning, "planning")), "Planning cwd created a nested planning project")
})
test("planning_explicit_false_keeps_dataset_v1", {
  context <- new_case("explicit-dataset"); dataset <- seed_dataset(context)
  bundle <- new_run(context, c(base_options, "--planning", "FALSE"), planning = FALSE)
  check(identical(dirname(dirname(bundle$path)), dataset$directory), "Explicit FALSE did not use active dataset")
  check(!dir.exists(context$planning), "Explicit dataset execution created a planning directory")
  replay(context, bundle, planning = FALSE)
})
test("planning_explicit_source_keeps_dataset_v1", {
  context <- new_case("source-dataset"); dataset <- seed_dataset(context)
  bundle <- new_run(context, c("--rds", dataset$input, base_options), planning = FALSE)
  check(!dir.exists(context$planning), "Explicit source execution created planning artifacts")
  replay(context, bundle, planning = FALSE)
})
for (kind in c("source", "dataset_name", "estimate_effect")) test(paste0("planning_true_rejects_", kind), {
  context <- new_case(paste0("conflict-", kind))
  options <- c(base_options, "--planning", "TRUE")
  if (kind == "source") {
    input <- file.path(context$base, "sample.rds"); saveRDS(data.frame(x = 1:10), input)
    options <- c(options, "--rds", input)
  } else if (kind == "dataset_name") options <- c(options, "--dataset-name", "sample")
  else options <- c(options, "--estimate-effect", "TRUE", "--vars", "x")
  failed(context, options)
  check(!file.exists(file.path(context$project, "sample", "import.json")), "Conflicting planning request imported a dataset")
})
for (kind in c("explicit_false", "estimate_effect")) test(paste0("planning_without_data_rejects_", kind), {
  context <- new_case(paste0("missing-data-", kind))
  options <- c(base_options, if (kind == "explicit_false") c("--planning", "FALSE") else c("--estimate-effect", "TRUE", "--vars", "x"))
  failed(context, options)
  check(!dir.exists(context$planning), "Dataset-required request silently became parameter-only planning")
})
test("planning_dataset_name_without_source_cannot_bind_active_dataset", {
  context <- new_case("dataset-name-without-source"); dataset <- seed_dataset(context)
  before <- tree_snapshot(dataset$directory)
  failed(context, c(base_options, "--dataset-name", "different_requested_dataset"))
  check(identical(before, tree_snapshot(dataset$directory)), "A purported dataset-name selection analyzed or modified the unrelated active dataset")
  check(!dir.exists(context$planning) && !dir.exists(file.path(context$project, "different_requested_dataset")), "Invalid source rename created a planning/dataset directory")
})
test("planning_estimated_effect_uses_active_dataset", {
  context <- new_case("estimated-effect"); dataset <- seed_dataset(context)
  bundle <- new_run(context, c("--analysis", "correlation", "--mode", "posthoc", "--estimate-effect", "TRUE", "--x", "x", "--y", "y"), planning = FALSE)
  check(identical(dirname(dirname(bundle$path)), dataset$directory), "Data-estimated effect did not bind the active dataset")
  check(!dir.exists(context$planning), "Data-estimated effect created a parameter-only run")
  replay(context, bundle, planning = FALSE)
})
test("planning_sem_df_is_preserved_in_replay_smoke", {
  context <- new_case("sem-df")
  bundle <- new_run(context, c("--analysis", "sem", "--mode", "apriori", "--sem-df", "120", "--rmsea0", "0.05", "--rmsea1", "0.08", "--power", "0.8"))
  check(identical(as.numeric(bundle$request$cli$`sem-df`), 120), "Canonical SEM degrees of freedom is absent from saved replay CLI")
  replay(context, bundle)
})
test("planning_legacy_sem_df_alias_replays", {
  context <- new_case("sem-df-alias")
  bundle <- new_run(context, c("--analysis", "sem", "--mode", "apriori", "--df", "120", "--rmsea0", "0.05", "--rmsea1", "0.08"))
  check(identical(as.numeric(bundle$request$cli$`sem-df`), 120) && is.null(bundle$request$cli$df), "Legacy numeric df alias was not canonicalized before shared input stripping")
  replay(context, bundle)
})
default_cases <- list(
  ttest_one = c("--analysis", "ttest", "--t-type", "one-sample", "--effect-size", ".5"),
  ttest_paired = c("--analysis", "ttest", "--t-type", "paired", "--effect-size", ".5"),
  anova = c("--analysis", "anova", "--effect-size", ".25"),
  correlation = c("--analysis", "correlation", "--effect-size", ".3"),
  regression = c("--analysis", "regression", "--effect-size", ".15"))
for (kind in names(default_cases)) test(paste0("planning_irrelevant_canonical_defaults_are_harmless_", kind), {
  context <- new_case(paste0("canonical-defaults-", kind))
  new_run(context, default_cases[[kind]])
})
inapplicable <- list(
  groups = c("--groups", "3"), u = c("--u", "3"), sem_df = c("--sem-df", "120"),
  rmsea0 = c("--rmsea0", ".01"), rmsea1 = c("--rmsea1", ".1"), df = c("--df", "120"),
  mu_two_sample = c("--mu", "1"))
for (kind in names(inapplicable)) test(paste0("planning_rejects_inapplicable_explicit_", kind), {
  context <- new_case(paste0("inapplicable-", kind))
  failed(context, c(base_options, inapplicable[[kind]]))
})
other_inapplicable <- list(
  alternative_anova = c("--analysis", "anova", "--effect-size", ".25", "--alternative", "greater"),
  t_type_correlation = c("--analysis", "correlation", "--effect-size", ".3", "--t-type", "paired"),
  ratio_anova = c("--analysis", "anova", "--effect-size", ".25", "--ratio", "2"),
  ratio_one_sample = c("--analysis", "ttest", "--t-type", "one-sample", "--effect-size", ".5", "--ratio", "2"),
  mu_paired = c("--analysis", "ttest", "--t-type", "paired", "--effect-size", ".5", "--mu", "1"))
for (kind in names(other_inapplicable)) test(paste0("planning_rejects_inapplicable_explicit_", kind), {
  context <- new_case(paste0("inapplicable-", kind))
  failed(context, other_inapplicable[[kind]])
})
unused_import <- list(sep = c("--sep", ";"), header = c("--header", "FALSE"),
  decimal = c("--csv-decimal", ","), encoding = c("--csv-encoding", "UTF-8"),
  col_types = c("--csv-col-types", "x=numeric"), na_values = c("--csv-na-values", "99"),
  import_action = c("--import-action", "new-version"))
for (kind in names(unused_import)) test(paste0("planning_rejects_inapplicable_import_", kind), {
  context <- new_case(paste0("unused-import-", kind))
  failed(context, c(base_options, unused_import[[kind]]))
  check(!dir.exists(context$planning), "Ignored import options silently created a parameter-only run")
})
surplus_roles <- list(
  correlation = c("--analysis", "correlation", "--x", "x", "--y", "y", "--vars", "x"),
  regression = c("--analysis", "regression", "--dv", "y", "--ivs", "x", "--group", "group"),
  anova = c("--analysis", "anova", "--dv", "y", "--group", "group", "--ivs", "x"),
  one_sample = c("--analysis", "ttest", "--t-type", "one-sample", "--vars", "x", "--group", "group"),
  independent = c("--analysis", "ttest", "--t-type", "two-sample", "--vars", "x", "--group", "group", "--x", "x"),
  paired = c("--analysis", "ttest", "--t-type", "paired", "--x", "x", "--y", "y", "--vars", "x"))
for (kind in names(surplus_roles)) test(paste0("planning_rejects_surplus_estimation_roles_", kind), {
  context <- new_case(paste0("surplus-roles-", kind)); dataset <- seed_dataset(context)
  before <- snapshot(file.path(dataset$directory, c("report_canonical.md", "analysis_log.jsonl")))
  failed(context, c("--estimate-effect", "TRUE", "--mode", "posthoc", surplus_roles[[kind]]))
  check(identical(before, snapshot(names(before))), "Rejected surplus roles modified the prior dataset report/log")
})
test("planning_unused_roles_are_disclosed_context_not_estimation", {
  context <- new_case("unused-role-context")
  bundle <- new_run(context, c(base_options, "--vars", "score", "--group", "condition"))
  check(is.null(bundle$result$results$effect_estimation) && !isTRUE(bundle$request$options$estimate_effect), "Variable names silently enabled effect estimation")
  notes <- paste(unlist(bundle$result$results$notes), collapse = " ")
  check(grepl("variable|role", notes, ignore.case = TRUE) && grepl("not used|not estimat|context|ignored", notes, ignore.case = TRUE), "Unused variable roles were not disclosed as context")
})
test("planning_rdata_object_and_sem_df_are_distinct", {
  context <- new_case("rdata-object-sem-df")
  model_data <- data.frame(x = 1:20, y = rep(1:10, 2))
  input <- file.path(context$base, "sample.RData"); save(model_data, file = input)
  bundle <- new_run(context, c("--rdata", input, "--df", "model_data", "--analysis", "sem", "--mode", "apriori", "--sem-df", "120", "--rmsea0", ".05", "--rmsea1", ".08"), planning = FALSE)
  check(identical(as.numeric(bundle$request$cli$`sem-df`), 120) && is.null(bundle$request$cli$df), "RData object name collided with SEM degrees of freedom")
  replay(context, bundle, planning = FALSE)
})
test("planning_replay_freezes_configuration_and_template", {
  context <- new_case("frozen-configuration")
  template <- file.path(context$base, "private-template.md")
  check(file.copy(file.path(repo, "assets/power/default-template.md"), template), "Could not copy private template")
  writeLines(c(readLines(template, warn = FALSE), "PLANNING_ORIGINAL_TEMPLATE"), template)
  configuration <- baseline; configuration$modules$power$alpha <- .025; configuration$templates$power$default <- template
  yaml::write_yaml(configuration, private_config)
  bundle <- new_run(context)
  check(grepl("PLANNING_ORIGINAL_TEMPLATE", bundle$markdown, fixed = TRUE), "Private template was not used")
  writeLines("MUTATED_TEMPLATE_MUST_NOT_APPEAR", template)
  configuration$modules$power$alpha <- .2; configuration$modules$power$power <- .9; configuration$templates$power$default <- "missing-template.md"
  yaml::write_yaml(configuration, private_config)
  repeated <- replay(context, bundle)
  check(!grepl("MUTATED_TEMPLATE", repeated$markdown, fixed = TRUE), "Replay reread a mutable external template")
})
test("planning_mandatory_result_survives_legacy_log_opt_out", {
  context <- new_case("log-opt-out")
  configuration <- baseline; configuration$logging$include_outputs <- FALSE
  yaml::write_yaml(configuration, private_config)
  bundle <- new_run(context, c(base_options, "--log", "FALSE"))
  check(!file.exists(file.path(context$planning, "analysis_log.jsonl")), "--log FALSE still wrote a legacy JSONL projection")
  check(length(bundle$result$results) > 0L, "Legacy log/output opt-out removed mandatory results")
  replay(context, bundle)
})
test("planning_requires_no_import_packages", {
  context <- new_case("no-import-packages")
  with_private_library(context, c("yaml", "jsonlite", "digest", "pwr", "semPower"), {
    probe <- file.path(context$base, "availability.log")
    status <- system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote("stopifnot(!requireNamespace('arrow', quietly=TRUE), !requireNamespace('haven', quietly=TRUE))")), stdout = probe, stderr = probe)
    check(status == 0L, paste("Private library did not exclude import packages:", text_file(probe)))
    bundle <- new_run(context)
    check(!any(c("arrow", "haven") %in% names(bundle$request$environment$packages)), "Parameter-only run unnecessarily loaded import packages")
    replay(context, bundle)
  })
})
for (package in c("pwr", "semPower")) test(paste0("planning_missing_dependency_", package), {
  context <- new_case(paste0("missing-package-", package))
  with_private_library(context, setdiff(c("yaml", "jsonlite", "digest", "pwr", "semPower"), package), {
    options <- if (package == "pwr") base_options else c("--analysis", "sem", "--sem-df", "120")
    rejected <- failed(context, options)
    check(grepl(package, text_file(rejected$log), fixed = TRUE), "Missing dependency error does not identify its required package")
  })
})
test("planning_disabled_prompt_is_private", {
  context <- new_case("prompt-disabled")
  configuration <- baseline; configuration$logging$include_user_prompt <- FALSE
  yaml::write_yaml(configuration, private_config)
  bundle <- new_run(context, c(base_options, "--user-prompt", "PRIVATE_PLANNING_PROMPT_83013"))
  files <- list.files(context$planning, "[.](json|jsonl|md)$", recursive = TRUE, full.names = TRUE)
  check(!any(vapply(files, function(path) grepl("PRIVATE_PLANNING_PROMPT_83013", text_file(path), fixed = TRUE), logical(1))), "Disabled user prompt leaked into saved planning artifacts")
  check(is.null(bundle$request$user_prompt) && is.null(bundle$request$cli$`user-prompt`), "Disabled user prompt survived in resolved replay request")
})
test("planning_enabled_prompt_and_basis_mask_external_paths", {
  context <- new_case("prompt-masking")
  external <- file.path(context$base, "external-evidence.pdf")
  bundle <- new_run(context, c(base_options, "--user-prompt", paste("Plan from", external), "--effect-basis", paste("Pilot estimate from", external)))
  files <- list.files(context$planning, "[.](json|jsonl|md)$", recursive = TRUE, full.names = TRUE)
  check(!any(vapply(files, function(path) grepl(context$base, text_file(path), fixed = TRUE), logical(1))), "External private paths leaked into planning artifacts")
  check(grepl("<external>/external-evidence.pdf", bundle$request$user_prompt, fixed = TRUE), "Enabled prompt lacks masked evidence context")
  check(grepl("Pilot estimate", text_file(bundle$request_path), fixed = TRUE), "Substantive effect-basis context was discarded")
  check(is.null(bundle$request$configuration$modules$research_academia), "Unrelated module configuration was saved")
})
test("planning_path_masking_preserves_scientific_urls_and_text", {
  context <- new_case("masking-preserves-context")
  external <- file.path(context$base, "effect.csv")
  evidence <- paste("Researcher-agreed minimum effect; see https://example.org/paper and", external)
  bundle <- new_run(context, c(base_options, "--effect-basis", evidence, "--user-prompt", paste("Plan from", evidence)))
  request_text <- text_file(bundle$request_path)
  check(grepl("https://example.org/paper", request_text, fixed = TRUE) && grepl("Researcher-agreed minimum effect", request_text, fixed = TRUE), "Path privacy masking discarded scientific prose or a literature URL")
  check(!grepl(context$base, request_text, fixed = TRUE) && grepl("<external>/effect.csv", request_text, fixed = TRUE), "Mixed URL/prose evidence retained a private path")
  replay(context, bundle)
})
for (kind in c("alpha", "power", "effect")) test(paste0("planning_invalid_calculation_preserves_publications_", kind), {
  context <- new_case(paste0("failed-calculation-", kind)); new_run(context)
  options <- if (kind == "alpha") c(base_options, "--alpha", "1") else if (kind == "power") c("--analysis", "ttest", "--effect-size", ".5", "--power", "1") else c("--analysis", "ttest", "--effect-size", "0")
  failed(context, options)
  check(!dir.exists(file.path(context$planning, ".analysis-lock")), "Failed calculation retained its analysis lock")
})
test("planning_publication_lock_preserves_owner", {
  context <- new_case("publication-lock"); new_run(context)
  lock <- file.path(context$project, ".publication-lock"); dir.create(lock)
  owner <- file.path(lock, "owner.txt"); writeLines("another live publisher", owner); owner_hash <- sha(owner)
  failed(context, base_options)
  check(dir.exists(lock) && identical(sha(owner), owner_hash), "Contending publication removed/modified another publisher's lock")
})
test("planning_analysis_lock_preserves_owner", {
  context <- new_case("analysis-lock"); new_run(context)
  lock <- file.path(context$planning, ".analysis-lock"); dir.create(lock)
  owner <- file.path(lock, "owner.txt"); writeLines("another live analysis", owner); owner_hash <- sha(owner)
  failed(context, base_options)
  check(dir.exists(lock) && identical(sha(owner), owner_hash), "Contending execution removed/modified another analysis's lock")
})
for (kind in c("unowned_directory", "existing_dataset", "regular_file", "invalid_marker")) test(paste0("planning_rejects_collision_", kind), {
  context <- new_case(paste0("collision-", kind))
  if (kind == "existing_dataset") seed_dataset(context, "planning") else if (kind == "regular_file") writeLines("researcher's file", context$planning) else {
    dir.create(context$planning)
    if (kind == "invalid_marker") write_json(list(schema_version = 1L, kind = "dataset"), file.path(context$planning, ".nlss-planning.json"))
    else writeLines("researcher's notes", file.path(context$planning, "notes.md"))
  }
  before <- if (dir.exists(context$planning)) tree_snapshot(context$planning) else snapshot(context$planning)
  failed(context, base_options)
  check(identical(before, if (dir.exists(context$planning)) tree_snapshot(context$planning) else snapshot(context$planning)), "Collision handling modified the unrelated planning path")
})
test("planning_owned_directory_rejects_dataset_import", {
  context <- new_case("reverse-collision"); new_run(context)
  before <- tree_snapshot(context$planning)
  input <- file.path(context$base, "planning.rds"); saveRDS(data.frame(x = 1:10), input)
  failed(context, c("--rds", input, "--vars", "x"), module = "descriptive_stats")
  check(identical(before, tree_snapshot(context$planning)), "Dataset import modified the owned parameter-planning directory")
})
for (kind in c("directory", "runs", "marker")) test(paste0("planning_rejects_symlink_", kind), {
  context <- new_case(paste0("symlink-", kind))
  outside <- file.path(context$base, "outside"); dir.create(outside)
  sentinel <- file.path(outside, "untouched.txt"); writeLines("must stay outside", sentinel)
  if (kind == "directory") check(file.symlink(outside, context$planning), "This platform cannot create test symlinks") else {
    new_run(context)
    if (kind == "runs") {
      check(file.rename(file.path(context$planning, "runs"), file.path(context$planning, "saved-runs")), "Could not move private fixture runs")
      check(file.symlink(outside, file.path(context$planning, "runs")), "This platform cannot create test symlinks")
    } else {
      marker <- file.path(context$planning, ".nlss-planning.json"); target <- file.path(outside, "marker.json")
      check(file.rename(marker, target) && file.symlink(target, marker), "This platform cannot create test marker symlink")
    }
  }
  before <- tree_snapshot(outside)
  failed(context, base_options)
  check(identical(before, tree_snapshot(outside)), "Planning followed a symlink and modified files outside its owned directory")
})
for (kind in c("request", "template", "output")) test(paste0("planning_replay_rejects_tampered_", kind), {
  context <- new_case(paste0("tamper-", kind)); bundle <- new_run(context)
  path <- switch(kind, request = bundle$request_path, template = file.path(bundle$path, bundle$request$templates[[1]]$path), output = file.path(bundle$path, "output.md"))
  if (kind == "request") {
    request <- raw_json(path); request$options$alpha <- .123; write_json(request, path)
  } else writeLines(c(readLines(path, warn = FALSE), "TAMPERED_CONTENT"), path)
  failed(context, c("--request", bundle$request_path), module = "replay_run")
})
for (kind in c("code", "r_version", "platform", "locale", "timezone", "package_version", "package_missing", "system_libraries")) test(paste0("planning_replay_refuses_environment_", kind), {
  context <- new_case(paste0("environment-", kind)); bundle <- new_run(context)
  resign_request(bundle, function(request) {
    if (kind == "code") request$code_sha256 <- paste(rep("0", 64), collapse = "")
    else if (kind == "package_version") request$environment$packages$pwr <- "0.0.0-impossible"
    else if (kind == "package_missing") request$environment$packages$nlssImpossiblePackage83013 <- "1.0"
    else if (kind == "system_libraries") request$environment$system_libraries$zlib <- "0.0.0-impossible"
    else request$environment[[kind]] <- "impossible-environment-83013"
    request
  })
  failed(context, c("--request", bundle$request_path), module = "replay_run")
})
for (kind in c("input_kind", "unexpected_dataset", "unsupported_module", "run_id", "template_traversal", "template_absolute")) test(paste0("planning_replay_rejects_invalid_request_", kind), {
  context <- new_case(paste0("invalid-replay-", kind)); bundle <- new_run(context)
  resign_request(bundle, function(request) {
    if (kind == "input_kind") request$input$kind <- "dataset"
    else if (kind == "unexpected_dataset") request$dataset <- list(dataset_id = "invented")
    else if (kind == "unsupported_module") request$module <- "descriptive_stats"
    else if (kind == "run_id") request$run_id <- "different-published-id"
    else request$templates[[1]]$path <- if (kind == "template_traversal") "../../outside.md" else file.path(context$base, "outside.md")
    request
  })
  failed(context, c("--request", bundle$request_path), module = "replay_run")
})
for (kind in c("schema", "input", "dataset", "run_id", "module", "missing_output", "output_path", "request_path")) test(paste0("planning_replay_rejects_invalid_result_", kind), {
  context <- new_case(paste0("invalid-result-", kind)); bundle <- new_run(context)
  result_path <- file.path(bundle$path, "result.json"); result <- raw_json(result_path)
  if (kind == "schema") result$schema_version <- 1L
  else if (kind == "input") result$input <- list(kind = "dataset")
  else if (kind == "dataset") result$dataset <- list(dataset_id = "invented")
  else if (kind == "run_id") result$run_id <- "wrong-run-id"
  else if (kind == "module") result$module <- "descriptive_stats"
  else if (kind == "missing_output") result$artifacts$output <- NULL
  else if (kind == "output_path") result$artifacts$output$path <- "request.json"
  else result$artifacts$request$path <- "output.md"
  write_json(result, result_path)
  failed(context, c("--request", bundle$request_path), module = "replay_run")
})
for (kind in c("renamed_directory", "pending_directory", "renamed_request", "missing_marker", "invalid_marker")) test(paste0("planning_replay_rejects_location_", kind), {
  context <- new_case(paste0("replay-location-", kind)); bundle <- new_run(context)
  request_path <- bundle$request_path
  if (kind %in% c("renamed_directory", "pending_directory")) {
    destination <- file.path(dirname(bundle$path), if (kind == "pending_directory") paste0(".pending-", basename(bundle$path)) else "different-run-name")
    check(file.rename(bundle$path, destination), "Could not rename private fixture run")
    request_path <- file.path(destination, "request.json")
  } else if (kind == "renamed_request") {
    request_path <- file.path(bundle$path, "other-request.json")
    check(file.copy(bundle$request_path, request_path), "Could not copy private request")
  } else {
    marker <- file.path(context$planning, ".nlss-planning.json")
    if (kind == "missing_marker") check(file.rename(marker, file.path(context$base, "moved-marker.json")), "Could not move private marker")
    else write_json(list(schema_version = 1L, kind = "dataset"), marker)
  }
  failed(context, c("--request", request_path), module = "replay_run")
})

summary_path <- file.path(work, "results.json")
jsonlite::write_json(list(schema_version = 1L, suite = "phase2-planning", modules = "power", test_pattern = test_pattern,
  tests = results, source_sha256 = sha(file.path(repo, "scripts/R/power.R")), test_sha256 = sha(script),
  r_source_sha256 = as.list(vapply(sort(list.files(file.path(repo, "scripts/R"), "[.]R$", recursive = TRUE, full.names = TRUE)), sha, character(1))),
  environment = list(r = R.version.string, packages = as.list(vapply(required, function(p) as.character(utils::packageVersion(p)), character(1))))),
  summary_path, auto_unbox = TRUE, pretty = TRUE, digits = NA)
passed <- sum(vapply(results, function(result) result$passed, logical(1)))
cat(sprintf("Phase 2 planning: %d/%d cases passed. Results: %s\n", passed, length(results), summary_path))
if (!nzchar(forced_root) && keep > 0L && dir.exists(output_base)) {
  candidates <- sort(list.dirs(output_base, full.names = TRUE, recursive = FALSE), decreasing = TRUE)
  candidates <- candidates[grepl("^[0-9]{14}$", basename(candidates))]
  if (length(candidates) > keep) for (path in setdiff(candidates[seq.int(keep + 1L, length(candidates))], run_root)) {
    if (identical(dirname(path), output_base) && grepl("^[0-9]{14}$", basename(path))) unlink(path, recursive = TRUE)
  }
}
quit(status = if (length(results) > 0L && passed == length(results)) 0 else 1)
