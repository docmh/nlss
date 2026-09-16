#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Focused offline dependency contracts. Host libraries and user projects remain
# untouched; symlinks expose packages to private child libraries selectively.
script <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))[1], winslash = "/")
repo <- normalizePath(file.path(dirname(script), "../.."), winslash = "/")
source(file.path(repo, "scripts/R/lib/bootstrap.R")); nlss_bootstrap()
args <- parse_args(commandArgs(TRUE), module = NULL, allowed = c("root", "keep", "help"), boolean = "help")
if (!is.null(args$help)) { cat("run_dependency_tests.R [--root DIR] [--keep N]\n"); quit(status = 0) }
settings <- yaml::read_yaml(file.path(repo, "tests/tests.yml"))$tests
root <- if (!is.null(args$root)) args$root else Sys.getenv("NLSS_TEST_ROOT", "")
forced <- nzchar(root)
if (!forced) root <- file.path(repo, settings$output_dir)
keep <- as.integer(if (!is.null(args$keep)) args$keep else Sys.getenv("NLSS_KEEP_RUNS", settings$keep_runs_default))
stopifnot(length(keep) == 1L, !is.na(keep), keep >= 0L)
work <- file.path(root, paste0("phase5-dependencies-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid()))
dir.create(work, recursive = TRUE); work <- normalizePath(work, winslash = "/")
stamp <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
started <- stamp(); checks <- list(); commands <- list()
source_files <- c(list.files(file.path(repo, "scripts/R"), "[.]R$", recursive = TRUE, full.names = TRUE),
  script, file.path(repo, "scripts/config.yml"), file.path(repo, "tests/tests.yml"))
hashes <- setNames(lapply(source_files, function(p) digest::digest(file = p, algo = "sha256")), sub(paste0(repo, "/"), "", source_files, fixed = TRUE))
save_result <- function(status = NULL) jsonlite::write_json(list(started = started, updated = stamp(),
  exit_status = status, source_sha256 = hashes, checks = checks, commands = commands),
  file.path(work, "results.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
check <- function(label, value) {
  checks[[length(checks) + 1L]] <<- list(label = label, passed = isTRUE(value))
  save_result(if (!isTRUE(value)) 1L else NULL)
  if (!isTRUE(value)) stop("FAIL: ", label)
  cat("PASS", label, "\n")
}
error <- function(expr) inherits(tryCatch({ force(expr); NULL }, error = identity), "error")
required <- function(operation, ...) names(nlss_dependency_requirements(operation, list(...)))
check("every executable has an explicit dependency entry", setequal(nlss_dependency_operations(),
  tools::file_path_sans_ext(list.files(file.path(repo, "scripts/R"), "[.]R$"))))
check("unknown operation is not dependency free", error(required("not_an_operation")))
check("--help FALSE cannot bypass requirements", "lavaan" %in% required("sem", help = FALSE))
for (operation in nlss_dependency_operations()) check(paste("help needs no packages:", operation),
  length(nlss_dependency_requirements(operation, list(help = TRUE))) == 0L)
check("CSV data still needs Parquet infrastructure", "arrow" %in% required("regression", csv = "sample.csv"))
check("SAV requests haven", "haven" %in% required("sem", sav = "survey.sav"))
check("project creation checks raw SAV plus Parquet", all(c("haven", "arrow") %in% required("project_create", source = "survey.SAV")))
check("project reuse does not need a data loader", !"arrow" %in% required("project_create"))
check("SEM requests lavaan", "lavaan" %in% required("sem"))
check("Power SEM alias uses semPower not pwr", "semPower" %in% required("power", analysis = "cfa") && !"pwr" %in% required("power", analysis = "cfa"))
check("parameter-only power needs no arrow", !"arrow" %in% required("power", planning = TRUE))
check("Power dataset selection needs arrow", "arrow" %in% required("power", dataset = "survey"))
check("Power explicit source needs arrow", "arrow" %in% required("power", csv = "survey.csv"))
check("default mixed-model inference", all(c("performance", "lmerTest", "reformulas") %in% required("mixed_models")))
check("Kenward-Roger option adds pbkrtest", "pbkrtest" %in% required("mixed_models", `df-method` = "kr"))
check("Type I none skips inference extras", !any(c("lmerTest", "pbkrtest", "car") %in% required("mixed_models", `df-method` = "none", type = "I")))
check("Wald Type III uses car", "car" %in% required("mixed_models", `df-method` = "none"))
check("between ANOVA default uses car", "car" %in% required("anova", between = "group"))
check("accepted spaced ANOVA type retains dependency", "car" %in% required("anova", type = " II "))
check("spaced disabled marginal means do not install emmeans", !"emmeans" %in% required("mixed_models", emmeans = " none "))
check("within ANOVA does not invent Type II dependency", !"car" %in% required("anova", within = "a,b"))
check("contrasts request emmeans", "emmeans" %in% required("anova", `contrast-file` = "custom.json"))
check("MI retains all required engines", all(c("mice", "broom", "arrow", "digest", "jsonlite") %in% required("mi_regression")))
check("simple imputation does not request MICE or VIM", !any(c("mice", "VIM") %in% required("impute", engine = "simple")))
check("explicit imputation engines", "mice" %in% required("impute", engine = "mice") && "VIM" %in% required("impute", engine = "knn"))
check("accepted spaced engine and diagnostic family retain dependencies", "mice" %in% required("impute", engine = " mice ") &&
  "lavaan" %in% required("assumptions", analysis = " sem "))
check("assumption family defaults are honored", "MVN" %in% required("assumptions", analysis = "sem"))
check("disabled Mardia does not request MVN", !"MVN" %in% required("assumptions", analysis = "sem", mardia = FALSE))
check("auto mixed diagnostics use actual formula", all(c("performance", "influence.ME") %in% required("assumptions", formula = "y~x+(1|id)")))
check("disabled mixed diagnostics stay disabled", !any(c("performance", "influence.ME", "DHARMa") %in%
  required("assumptions", analysis = "mixed", performance = FALSE, influence = FALSE, dharma = FALSE)))
check("read-only inspection requests no statistical libraries", !any(c("arrow", "lavaan", "pwr", "mice") %in% required("project_inspect")))
check("standalone history minimal dependencies", identical(required("check_integrity"), "jsonlite") &&
  setequal(required("reconstruct_reports"), c("jsonlite", "digest")))
saved_config <- get_config(); config_env$config$modules$mixed_models$df_method <- "kenward-roger"
check("requirements follow canonical effective configuration", "pbkrtest" %in% required("mixed_models"))
check("CLI overrides configuration requirements", !"pbkrtest" %in% required("mixed_models", `df-method` = "none"))
config_env$config <- saved_config
probe <- function(package, ...) list(status = if (package == "jsonlite") "absent" else "ready", version = NULL, detail = "fixture")
missing <- nlss_dependency_check("sem", probe = probe)
check("structured missing status and reason", missing$status == "missing_dependency" && missing$missing[[1]]$package == "jsonlite" && nzchar(missing$missing[[1]]$reason))
check("base JSON handles control characters and Unicode", identical(jsonlite::fromJSON(nlss_dependency_json(list(text = "Ä\n\t\"\\")))$text, "Ä\n\t\"\\"))
check("actual absent package", nlss_dependency_probe("nlssWaveMissingPackage")$status == "absent")
check("actual incompatible version", nlss_dependency_probe("stats", exact = "0.0.1")$status == "incompatible")
check("actual incompatible API", nlss_dependency_probe("stats", exports = "nlssAbsentAPI")$status == "incompatible")

# One private library of symlinks, never package copies or removal from host libs.
private <- file.path(work, "library"); dir.create(private)
host_paths <- .libPaths()
installed <- unique(unlist(lapply(host_paths, list.files)))
for (name in installed) {
  source <- find.package(name, quiet = TRUE)
  if (length(source) && !startsWith(source, paste0(.Library, "/")))
    stopifnot(file.symlink(source, file.path(private, name)))
}
child <- function(operation, args = character(), absent = character(), expected = 0L, extra_env = character()) {
  links <- file.path(private, absent)
  sources <- vapply(absent, function(p) find.package(p, quiet = TRUE)[1], character(1))
  # Only validated test-created symlinks are removed, not their targets.
  for (path in links) if (nzchar(Sys.readlink(path))) unlink(path)
  on.exit(for (i in seq_along(links)) if (!is.na(sources[i])) file.symlink(sources[i], links[i]), add = TRUE)
  log <- file.path(work, paste0("command-", length(commands) + 1L, ".log"))
  variables <- c(R_LIBS = private, R_LIBS_USER = private, R_LIBS_SITE = private, NLSS_R_LIBRARY = private, extra_env)
  command <- c("--vanilla", file.path(repo, "scripts/R", paste0(operation, ".R")), args)
  start <- stamp()
  status <- system2(file.path(R.home("bin"), "Rscript"), shQuote(command), stdout = log, stderr = log,
    env = paste0(names(variables), "=", shQuote(variables)))
  commands[[length(commands) + 1L]] <<- list(command = command, started = start, finished = stamp(),
    exit_status = status, expected = expected, absent = as.list(absent), log = basename(log))
  save_result()
  text <- readLines(log, warn = FALSE)
  check(paste(operation, "exit", expected, "without", paste(absent, collapse = ",")), status == expected)
  invisible(list(text = text, status = status))
}
diagnostic <- function(result) {
  line <- result$text[startsWith(result$text, "{\"schema_version\":")]
  if (length(line) != 1L) return(NULL)
  jsonlite::fromJSON(line, simplifyVector = FALSE)
}
tree <- function(root) {
  files <- sort(list.files(root, recursive = TRUE, all.files = TRUE, full.names = TRUE, include.dirs = TRUE))
  setNames(lapply(files, function(p) if (dir.exists(p)) "directory" else digest::digest(file = p, algo = "sha256")), files)
}
project <- file.path(work, "project"); dir.create(project)
file.copy(file.path(repo, settings$golden_dataset), file.path(project, "sample.csv"))
before <- tree(project)
for (missing_package in c("yaml", "jsonlite", "digest", "arrow")) {
  result <- child("project_create", c("--project", project, "--source", "sample.csv"), missing_package, 42L)
  check(paste("project preflight reports", missing_package), identical(diagnostic(result)$status, "missing_dependency"))
  check("raw folder unchanged on dependency failure", identical(before, tree(project)))
}
child("project_create", c("--project", project, "--source", "sample.csv"))
before <- tree(project)
result <- child("sem", c("--project", project, "--model", "f =~ x1+x2+x3"), "lavaan", 42L)
check("statistical failure before writes", identical(before, tree(project)) && diagnostic(result)$missing[[1]]$package == "lavaan")
child("power", c("--project", project, "--effect-size", "0.5"), "pwr", 42L)
child("calc", c("--project", project, "--expr", "1+1"), "jsonlite", 42L)
check("planning and utility failures preserve project", identical(before, tree(project)))
child("project_inspect", c("--project", project, "--format", "json"), c("arrow", "lavaan", "pwr"))
child("project_create", c("--project", project), "arrow")
check("read-only calls do not write", identical(before, tree(project)))
for (operation in nlss_dependency_operations())
  child(operation, "--help", c("yaml", "jsonlite", "arrow", "lavaan"))
child("t_test", c("--help", "FALSE", "--project", project, "--vars", "x1"), "arrow", 42L)
check("false help cannot import or publish", identical(before, tree(project)))
empty_log <- file.path(work, "empty.jsonl"); writeLines(character(), empty_log)
child("check_integrity", empty_log, "jsonlite", 42L)
child("reconstruct_reports", empty_log, "digest", 42L)
result <- child("dependency_resolver", c("--operation", "sem"), "yaml", 42L)
check("recovery utility works without foundational YAML", diagnostic(result)$status == "missing_dependency")
child("dependency_resolver", c("--operation", "power", "--", "--analysis", "ttest"))
prompt_file <- file.path(work, "power-prompts.txt")
writeLines(c("planning", "sem", "apriori", "auto", "0.05", "0.8", "10", "0.05", "0.08", "", "", "", "2", "FALSE"), prompt_file)
private_config <- file.path(work, "interactive-config.yml")
yaml::write_yaml(list(defaults = list(output_dir = file.path(work, "interactive-output"))), private_config)
child("power", "--interactive", "pwr", extra_env = c(NLSS_PROMPT_FILE = prompt_file, NLSS_CONFIG_PATH = private_config))
check("interactive SEM overrides t-test default before dependency check", dir.exists(file.path(work, "interactive-output", "planning")))
before <- tree(project)
result <- child("power", c("--interactive", "--project", project), "semPower", 42L,
  c(NLSS_PROMPT_FILE = prompt_file, NLSS_CONFIG_PATH = private_config))
check("interactive missing final engine is reported before writes", diagnostic(result)$missing[[1]]$package == "semPower" && identical(before, tree(project)))
child("power", c("--project", project, "--effect-size", "0.5"))
request <- list.files(file.path(project, ".nlss/runs"), "^request.json$", recursive = TRUE, full.names = TRUE)[1]
check("current planning request prepared for replay dependency checks", file.exists(request))
before <- tree(project)
child("dependency_resolver", c("--operation", "replay-run", "--", "--request", request))
result <- child("replay_run", c("--request", request), "pwr", 42L)
check("replay names its missing recorded package", diagnostic(result)$missing[[1]]$package == "pwr")
child("dependency_resolver", c("--operation", "replay-run", "--", "--request", request), "pwr", 42L)
child("dependency_resolver", c("--operation", "replay-run"), expected = 44L)
check("replay checks/failures leave evidence and working data unchanged", identical(before, tree(project)))

probe_env <- new.env(parent = environment(nlss_dependency_probe))
probe_env$loadNamespace <- function(...) stop("fixture: shared library cannot be loaded")
unloadable_probe <- nlss_dependency_probe; environment(unloadable_probe) <- probe_env
check("unloadable is distinct from absent/incompatible", unloadable_probe("stats")$status == "unloadable")

# Actual install.packages execution against a tiny local CRAN-layout repository,
# not a mocked installer or an actual statistical package replacement.
fixture <- file.path(work, "package-source", "nlssWaveFixture")
dir.create(file.path(fixture, "R"), recursive = TRUE)
writeLines(c("Package: nlssWaveFixture", "Version: 1.0.0", "Title: Isolated NLSS Installer Fixture",
  "Description: Tests installation and fresh process loading without network access.",
  "Imports: nlssWaveDependency (>= 1.0.0)", "Suggests: nlssWaveSuggestion",
  "Author: NLSS test suite", "Maintainer: NLSS Tests <tests@example.invalid>", "License: Apache License (== 2.0)"), file.path(fixture, "DESCRIPTION"))
writeLines("export(answer)", file.path(fixture, "NAMESPACE")); writeLines("answer <- function() nlssWaveDependency::answer()", file.path(fixture, "R", "answer.R"))
dependency <- file.path(dirname(fixture), "nlssWaveDependency")
dir.create(file.path(dependency, "R"), recursive = TRUE)
description <- readLines(file.path(fixture, "DESCRIPTION"))
description <- description[!grepl("^(Imports|Suggests):", description)]
description[1] <- "Package: nlssWaveDependency"
writeLines(description, file.path(dependency, "DESCRIPTION"))
writeLines("export(answer)", file.path(dependency, "NAMESPACE")); writeLines("answer <- function() 42L", file.path(dependency, "R", "answer.R"))
repository <- file.path(work, "repository"); contrib <- file.path(repository, "src", "contrib"); dir.create(contrib, recursive = TRUE)
prior <- getwd(); setwd(contrib)
build_status <- vapply(c(dependency, fixture), function(path) system2(file.path(R.home("bin"), "R"),
  c("CMD", "build", shQuote(path)), stdout = file.path(work, paste0(basename(path), "-build.log")),
  stderr = file.path(work, paste0(basename(path), "-build.log"))), integer(1))
setwd(prior); check("real local source packages built", all(build_status == 0L))
tools::write_PACKAGES(contrib, type = "source")
repo_url <- paste0("file://", repository)
target <- file.path(work, "installed-fixture")
req <- list(list(package = "nlssWaveFixture", reason = "isolated installation fixture", exports = "answer"))
initial <- nlss_dependency_check("calc", requirements = req)
plan <- nlss_dependency_install_plan(initial, repo_url, target, "source")
check("install plan contains only direct and necessary transitive requirements", length(plan$packages) == 2L &&
  plan$approval == "nlssWaveDependency@1.0.0,nlssWaveFixture@1.0.0")
available <- plan$available
extra <- available[rep(1L, 2L), , drop = FALSE]
rownames(extra) <- extra[, "Package"] <- c("yaml", "jsonlite")
extra[, "Version"] <- "999.0"
extra[, intersect(colnames(extra), c("Depends", "Imports", "LinkingTo", "Suggests"))] <- NA_character_
available <- rbind(available, extra)
available["nlssWaveFixture", "Imports"] <- "yaml (>= 999.0), jsonlite (>= 1.0)"
available["nlssWaveFixture", "Suggests"] <- "nlssWaveSuggestion"
transitive <- nlss_dependency_install_plan(initial, repo_url, target, "source", available = available)
check("R plans only necessary transitive upgrades, not Suggests or latest versions",
  setequal(vapply(transitive$packages, `[[`, "", "package"), c("nlssWaveFixture", "yaml")))
upgrade <- Filter(function(x) x$package == "yaml", transitive$packages)[[1]]
check("necessary upgrade is exposed for approval", upgrade$action == "replace" && !is.null(upgrade$installed) && upgrade$version == "999.0")
declined <- nlss_dependency_install(plan, NULL, req, file.path(repo, "scripts/R/lib/dependency_resolver.R"))
check("approval refusal does not even create library", declined$status == "approval_required" && !dir.exists(target))
expanded <- plan; expanded$approval <- paste0(plan$approval, ",newDependency@1.0.0")
check("expanded plan requires renewed approval", nlss_dependency_install(expanded, plan$approval, req,
  file.path(repo, "scripts/R/lib/dependency_resolver.R"))$status == "approval_required" && !dir.exists(target))
installed_result <- nlss_dependency_install(plan, plan$approval, req, file.path(repo, "scripts/R/lib/dependency_resolver.R"))
check("real isolated installation and fresh load verification", installed_result$status == "installed")
fresh <- nlss_dependency_fresh_check(req, target, file.path(repo, "scripts/R/lib/dependency_resolver.R"))
check("fresh retry sees installed package", fresh[[1]]$status == "ready")
child("dependency_resolver", c("--operation", "calc", "--library", target))
check("custom library selection is exported in check response", any(grepl(target,
  readLines(file.path(work, paste0("command-", length(commands), ".log"))), fixed = TRUE)))
check("host library was not modified", !length(find.package("nlssWaveFixture", quiet = TRUE)))
check("offline/unavailable repository stops planning", error(nlss_dependency_install_plan(initial, paste0("file://", work, "/absent-repo"), target, "source")))
bad <- plan; bad$library <- file.path(work, "failed-install")
bad$repository <- paste0("file://", work, "/absent-repo")
bad$available[, "Repository"] <- paste0(bad$repository, "/src/contrib")
check("install failure is not success-shaped", nlss_dependency_install(bad, bad$approval, req,
  file.path(repo, "scripts/R/lib/dependency_resolver.R"))$status == "installation_failed")
unwritable <- file.path(work, "unwritable"); dir.create(unwritable); Sys.chmod(unwritable, "0555")
check("non-writable library rejected", error(nlss_dependency_install_library(unwritable, repo)))
Sys.chmod(unwritable, "0755")
check("project library rejected", error(nlss_dependency_install_library(file.path(project, "R-library"), repo)))
check("installation library rejected", error(nlss_dependency_install_library(file.path(repo, "R-library"), repo)))
check("plugin cache library rejected", error(nlss_dependency_install_library(file.path(work, "plugins/cache/R-library"), repo)))
prior <- getwd(); setwd(work)
child("dependency_resolver", c("--operation", "power", "--action", "plan", "--repo", repo_url,
  "--library", file.path(repo, "assets", "forbidden-library")), "pwr", 44L)
setwd(prior)
check("installation root guard works from an unrelated working folder", !dir.exists(file.path(repo, "assets", "forbidden-library")))
check("sources unchanged during tests", identical(hashes, setNames(lapply(source_files, function(p) digest::digest(file = p, algo = "sha256")), names(hashes))))
save_result(0L)
if (!forced && keep > 0L) {
  prior_runs <- sort(list.dirs(root, recursive = FALSE, full.names = TRUE), decreasing = TRUE)
  prior_runs <- prior_runs[grepl("^phase5-dependencies-[0-9]+-[0-9]+$", basename(prior_runs))]
  if (length(prior_runs) > keep) for (path in prior_runs[-seq_len(keep)])
    if (file.exists(file.path(path, "results.json"))) unlink(path, recursive = TRUE)
}
cat(length(checks), "dependency checks passed. Evidence:", work, "\n")
