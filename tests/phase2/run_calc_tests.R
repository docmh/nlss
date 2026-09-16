#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Dataset-free public CLI acceptance; numerical expectations use base R/stats,
# never Calc's evaluator. Utility publication is tested separately from inference.
args <- commandArgs(TRUE)
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))[[1]]
repo <- normalizePath(file.path(dirname(script), "../.."), winslash = "/")
script <- normalizePath(script, winslash = "/")
if ("--help" %in% args) { cat("Usage: run_calc_tests.R [--root PATH] [--keep N] [--match REGEX]\n"); quit(status = 0) }
if (length(args) %% 2L || (length(args) && any(!args[seq(1L, length(args), 2L)] %in% c("--root", "--keep", "--match")))) stop("Unknown/incomplete test option")
arg <- function(name, fallback) { i <- which(args == name); if (length(i) > 1L) stop("Duplicate test option"); if (length(i)) args[i + 1L] else fallback }
for (package in c("yaml", "jsonlite", "digest")) if (!requireNamespace(package, quietly = TRUE)) stop("Missing test package: ", package)
absolute <- function(path) normalizePath(if (grepl("^(/|[A-Za-z]:|\\\\)", path)) path else file.path(repo, path), winslash = "/", mustWork = FALSE)
cfg <- yaml::read_yaml(absolute(Sys.getenv("NLSS_TESTS_CONFIG", "tests/tests.yml")), eval.expr = FALSE)$tests
pattern <- arg("--match", ".*"); invisible(grepl(pattern, "validate regex"))
forced <- arg("--root", Sys.getenv("NLSS_TEST_ROOT", ""))
keep_text <- arg("--keep", Sys.getenv("NLSS_KEEP_RUNS", as.character(cfg$keep_runs_default)))
keep <- suppressWarnings(as.numeric(keep_text))
if (length(keep) != 1L || !is.finite(keep) || keep < 0L || keep != floor(keep)) stop("Invalid --keep")
collection <- file.path(if (nzchar(forced)) absolute(forced) else absolute(cfg$output_dir), "phase2-calc")
work <- file.path(collection, paste0("run-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid()))
dir.create(work, recursive = TRUE)
config <- file.path(work, "config.yml")
reset_config <- function(extra = list()) yaml::write_yaml(modifyList(list(logging = list(include_checksum = FALSE), defaults = list(digits = 12L)), extra), config)
reset_config()
Sys.setenv(NLSS_CONFIG_PATH = config)
Sys.unsetenv(c("NLSS_REPLAY_REQUEST", "NLSS_PROMPT_FILE"))
check <- function(ok, message) if (!isTRUE(ok)) stop(message, call. = FALSE)
text <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
json <- function(path) jsonlite::read_json(path, simplifyVector = FALSE)
write_json <- function(value, path) jsonlite::write_json(value, path, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null", na = "null")
sha <- function(path) digest::digest(file = path, algo = "sha256")
utc <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
source_files <- sort(list.files(file.path(repo, "scripts/R"), "[.]R$", recursive = TRUE, full.names = TRUE))
source_before <- setNames(vapply(source_files, sha, character(1)), substring(source_files, nchar(repo) + 2L))
write_json(list(started_utc = utc(), runner_sha256 = sha(script), selected_pattern = pattern, production_files = as.list(source_before)), file.path(work, "started.json"))
near <- function(actual, expected, label, tolerance = 1e-12) {
  a <- as.numeric(actual); b <- as.numeric(expected)
  check(length(a) == length(b), paste(label, "length differs"))
  check(identical(is.na(a), is.na(b)) && identical(is.nan(a), is.nan(b)), paste(label, "missing/nonfinite type differs"))
  check(identical(is.infinite(a), is.infinite(b)) && identical(sign(a[is.infinite(a)]), sign(b[is.infinite(b)])), paste(label, "infinity differs"))
  finite <- is.finite(b)
  check(all(abs(a[finite] - b[finite]) <= tolerance * pmax(1, abs(b[finite]))), paste(label, "numeric values differ"))
  numeric_checks <<- numeric_checks + length(b)
}
snapshot <- function(paths) { paths <- paths[file.exists(paths) & !dir.exists(paths)]; setNames(vapply(paths, sha, character(1)), paths) }
tree <- function(path) snapshot(sort(list.files(path, full.names = TRUE, recursive = TRUE, all.files = TRUE)))
numeric_checks <- 0L; registered <- character(); results <- list(); processes <- list(); command_index <- 0L
test <- function(name, code) {
  registered <<- c(registered, name)
  if (!grepl(pattern, name)) return(invisible(NULL))
  reset_config(); Sys.unsetenv("NLSS_PROMPT_FILE")
  previous <- getwd(); on.exit(setwd(previous), add = TRUE)
  before <- numeric_checks; start <- proc.time()[["elapsed"]]
  failure <- tryCatch({ force(code); NULL }, error = conditionMessage)
  results[[length(results) + 1L]] <<- list(test = name, passed = is.null(failure),
    numeric_checks = numeric_checks - before, seconds = unname(proc.time()[["elapsed"]] - start),
    message = if (is.null(failure)) "OK" else failure)
  cat(if (is.null(failure)) "[PASS] " else "[FAIL] ", name, if (is.null(failure)) "" else paste0(": ", failure), "\n", sep = "")
}
new_case <- function(name, manifest = TRUE) {
  base <- file.path(work, "cases", name); project <- file.path(base, "project")
  dir.create(project, recursive = TRUE)
  if (manifest) yaml::write_yaml(list(version = 1L, datasets = list()), file.path(project, "nlss-workspace.yml"))
  list(base = base, project = project)
}
tokens <- function(options) unlist(lapply(names(options), function(name) c(paste0("--", name), as.character(options[[name]]))), use.names = FALSE)
run_cli <- function(context, options, failure = FALSE, module = "calc", cwd = context$project) {
  if (is.list(options)) options <- tokens(options)
  command_index <<- command_index + 1L
  prefix <- file.path(context$base, paste0("command-", command_index))
  stdout <- paste0(prefix, ".stdout"); stderr <- paste0(prefix, ".stderr")
  record <- list(command = "Rscript", arguments = c(file.path(repo, "scripts/R", paste0(module, ".R")), options),
    started_utc = utc(), status = "running")
  write_json(record, paste0(prefix, ".json"))
  old <- getwd(); setwd(cwd); on.exit(setwd(old), add = TRUE)
  exit <- system2(file.path(R.home("bin"), "Rscript"), shQuote(record$arguments), stdout = stdout, stderr = stderr)
  record$status <- "finished"; record$exit_code <- exit; record$finished_utc <- utc()
  write_json(record, paste0(prefix, ".json")); processes[[length(processes) + 1L]] <<- record
  if (failure) {
    check(exit != 0L, "Expected CLI failure returned success")
    check(nzchar(text(stderr)), "Expected failure supplied no informational stderr")
  } else check(exit == 0L, paste("CLI failed:", text(stderr)))
  list(stdout = text(stdout), stderr = text(stderr), exit_code = exit)
}
runs <- function(context) {
  paths <- list.dirs(file.path(context$project, "utility-runs"), full.names = TRUE, recursive = FALSE)
  paths[!startsWith(basename(paths), ".")]
}
run <- function(context, options, cwd = context$project) {
  before <- runs(context)
  process <- run_cli(context, options, cwd = cwd)
  added <- setdiff(runs(context), before); check(length(added) == 1L, "Expected one immutable utility run")
  request <- json(file.path(added, "request.json")); result <- json(file.path(added, "result.json"))
  check(identical(request$kind, "utility") && identical(result$kind, "utility") &&
    identical(request$module, "calc") && identical(result$status, "completed"), "Incorrect utility identity/status")
  check(is.null(request$dataset) && is.null(result$dataset) && !isTRUE(request$replay$eligible), "Utility fabricated dataset/statistical replay support")
  for (artifact in result$artifacts) check(identical(sha(file.path(added, artifact$path)), artifact$sha256), paste("Artifact hash differs:", artifact$path))
  check(identical(text(file.path(added, "stdout.txt")), process$stdout), "Successful stdout differs from preserved stdout")
  check(!length(list.files(context$project, "[.]parquet$", recursive = TRUE)) && !dir.exists(file.path(context$project, "planning")), "Calc fabricated dataset/planning artifacts")
  list(path = added, request = request, result = result, values = readRDS(file.path(added, "values.rds")),
    stdout = process$stdout, markdown = text(file.path(added, "output.md")), process = process)
}
expect_values <- function(actual, expected) {
  for (name in names(expected)) near(actual$values$evaluation$results[[name]], expected[[name]], name)
}
test("calc_help_and_no_args_do_not_publish", {
  context <- new_case("help")
  for (options in list("--help", character())) {
    actual <- run_cli(context, options)
    check(grepl("Calc utility", actual$stdout, fixed = TRUE), "Help text missing")
  }
  check(!length(runs(context)) && !file.exists(file.path(context$project, "report_canonical.md")), "Help published output")
})
test("calc_operators_parentheses_and_constants_independent", {
  context <- new_case("operators")
  actual <- run(context, list(expr = "add=2+3|sub=7-2|mul=3*4|div=3/4|power=2^5|mod=8%%3|integer=8%/%3|negative=-7|grouped=(2+3)*4|circle=pi|natural=e", format = "json"))
  expect_values(actual, list(add = 2+3, sub = 7-2, mul = 3*4, div = 3/4, power = 2^5,
    mod = 8%%3, integer = 8%/%3, negative = -7, grouped = (2+3)*4, circle = pi, natural = exp(1)))
  check(isTRUE(actual$result$results$reproducibility$deterministic_given_recorded_inputs), "Restricted calculation lost deterministic classification")
  check(is.list(jsonlite::fromJSON(actual$stdout)), "JSON stdout is not a standalone object")
})
test("calc_math_functions_independent", {
  context <- new_case("math")
  actual <- run(context, list(expr = "a=abs(-3)|b=sqrt(2)|c=log(8,base=2)|d=exp(2)|rounding=round(1.234,2)|minimum=min(1,3,-2)|maximum=max(1,3,-2)|total=sum(1,3,-2)|average=mean(7)|sd_single=sd(1)|var_single=var(1)"))
  expect_values(actual, list(a=abs(-3), b=sqrt(2), c=log(8,base=2), d=exp(2), rounding=round(1.234,2),
    minimum=min(1,3,-2), maximum=max(1,3,-2), total=sum(1,3,-2), average=mean(7), sd_single=sd(1), var_single=var(1)))
})
test("calc_distribution_functions_independent", {
  context <- new_case("distributions")
  actual <- run(context, list(expr = "pn=pnorm(1.1)|qn=qnorm(0.975)|pt=pt(1.1,12)|qt=qt(0.975,12)|pf=pf(1.5,3,20)|qf=qf(0.95,3,20)|tail=pnorm(1.1,lower.tail=FALSE)|logp=pnorm(2,log.p=TRUE)"))
  expect_values(actual, list(pn=pnorm(1.1), qn=qnorm(0.975), pt=pt(1.1,12), qt=qt(0.975,12),
    pf=pf(1.5,3,20), qf=qf(0.95,3,20), tail=pnorm(1.1,lower.tail=FALSE), logp=pnorm(2,log.p=TRUE)))
})
test("calc_sequential_bindings_and_parameter_derivations", {
  context <- new_case("bindings")
  actual <- run(context, list(set = "r=0.3|k=3|rsq=r^2", expr = "d=2*r/sqrt(1-r^2)|f2=rsq/(1-rsq)|alpha=0.05/k|d=d+1|d/2"))
  expect_values(actual, list(d=2*0.3/sqrt(1-0.3^2)+1, f2=0.3^2/(1-0.3^2), alpha=0.05/3, expr_1=(2*0.3/sqrt(1-0.3^2)+1)/2))
  check(length(actual$values$evaluation$rows)==5L && length(actual$values$constants)==3L, "Ordered expression/constants rows lost")
  near(actual$values$evaluation$rows[[1]]$value,2*0.3/sqrt(1-0.3^2),"Earlier rebound value")
})
test("calc_unnamed_assignment_collision_retains_chronological_rows", {
  context <- new_case("name-collision")
  actual <- run(context, list(expr = "expr_1=7|3|expr_1+1"))
  expect_values(actual,list(expr_1=3,expr_2=4))
  near(vapply(actual$values$evaluation$rows,function(row)row$value,numeric(1)),c(7,3,4),"Chronological values")
})
test("calc_unsafe_vector_statistics_independent", {
  context <- new_case("vectors")
  actual <- run(context, list(set = "x=c(1,2,4,8)", expr = "cdf=pnorm(x)|avg=mean(x)|stdev=sd(x)|variance=var(x)|vector=x^2", unsafe = TRUE, format = "json"))
  x <- c(1,2,4,8)
  expect_values(actual,list(cdf=pnorm(x),avg=mean(x),stdev=sd(x),variance=var(x),vector=x^2))
  check(!isTRUE(actual$result$results$reproducibility$deterministic_given_recorded_inputs), "Unsafe call claimed deterministic reproduction")
})
test("calc_unsafe_seeded_randomness_remains_non_replayable", {
  context <- new_case("random")
  actual <- run(context,list(expr="draw={set.seed(173);rnorm(4)}",unsafe=TRUE))
  set.seed(173); expect_values(actual,list(draw=rnorm(4)))
  check(!isTRUE(actual$request$replay$eligible) && !isTRUE(actual$result$results$reproducibility$automatic_replay), "Seeded unrestricted code incorrectly replayable")
})
test("calc_nonfinite_values_statuses_and_warnings_preserved", {
  context <- new_case("nonfinite")
  actual <- run(context,list(expr="missing=NA_real_|nan=0/0|positive=1/0|negative=-1/0|warning=sqrt(-1)",format="json"))
  expect_values(actual,list(missing=NA_real_,nan=NaN,positive=Inf,negative=-Inf,warning=NaN))
  check(identical(unname(unlist(actual$result$results$value_status)),c("NA","NaN","positive_infinity","negative_infinity","NaN")), "Nonfinite kinds collapsed in audit")
  check(length(actual$result$results$warnings)>0L && grepl("nonfinite|Nonfinite",actual$markdown), "Nonfinite warning/status not disclosed")
  check(all(vapply(jsonlite::fromJSON(actual$stdout),is.null,logical(1))), "Nonfinite JSON values are not null")
})
for (format in c("plain","json","csv")) test(paste0("calc_",format,"_stdout_and_unrounded_audit"), {
  context <- new_case(paste0("stdout-",format))
  actual <- run(context,list(expr="value=1/3",format=format,digits=3))
  expected <- switch(format,plain="value = 0.333",json='{"value":0.333}',csv="name,value\nvalue,0.333")
  check(identical(actual$stdout,expected),paste(format,"stdout changed"))
  expect_values(actual,list(value=1/3))
})
test("calc_json_decimal_point_independent_of_unsafe_OutDec", {
  context <- new_case("decimal")
  actual <- run(context,list(expr='value={options(OutDec=",");1/3}',unsafe=TRUE,format="json",digits=3))
  near(jsonlite::fromJSON(actual$stdout)$value,0.333,"JSON decimal point")
})
test("calc_unsafe_matrix_raw_shape_is_preserved", {
  context <- new_case("matrix")
  actual <- run(context,list(expr="value=matrix(1:6,nrow=2)",unsafe=TRUE,format="json"))
  check(identical(dim(actual$values$evaluation$results$value),c(2L,3L)),"Raw RDS matrix shape lost")
  near(actual$values$evaluation$results$value,matrix(1:6,nrow=2),"Matrix entries")
})
test("calc_private_config_and_cli_precedence", {
  context <- new_case("config")
  reset_config(list(defaults=list(digits=4L),modules=list(calc=list(format="json",unsafe=FALSE))))
  actual <- run(context,list(expr="value=1/3"))
  check(identical(actual$stdout,'{"value":0.3333}'),"Private format/digits defaults ignored")
  second <- run(context,list(expr="value=1/3",format="csv",digits=2))
  check(identical(second$stdout,"name,value\nvalue,0.33"),"CLI precedence lost")
})
test("calc_log_false_keeps_report_audit_and_template", {
  context <- new_case("no-log")
  actual <- run(context,list(expr="value=2",log=FALSE))
  check(!file.exists(file.path(context$project,"analysis_log.jsonl")) && file.exists(file.path(context$project,"report_canonical.md")),"Log FALSE disabled wrong outputs")
  check(file.exists(file.path(actual$path,"template.md")) && grepl("Calculator Output",actual$markdown,fixed=TRUE),"Canonical template was not frozen/used")
  check(identical(sha(file.path(actual$path,"template.md")),sha(file.path(repo,"assets/calc/default-template.md"))),"Default template bytes differ")
})
test("calc_project_active_dataset_manifest_remains_untouched", {
  context <- new_case("active")
  manifest <- file.path(context$project,"nlss-workspace.yml")
  yaml::write_yaml(list(version=1L,active_dataset="survey",datasets=list(list(name="survey",directory="survey"))),manifest)
  before <- sha(manifest)
  run(context,list(expr="2+2"))
  check(identical(sha(manifest),before),"Utility changed dataset registry/selection")
})
test("calc_no_manifest_fallback_does_not_fabricate_one", {
  context <- new_case("fallback",manifest=FALSE)
  reset_config(list(defaults=list(output_dir=context$project)))
  run(context,list(expr="2+2"))
  check(!file.exists(file.path(context$project,"nlss-workspace.yml")),"Calc fabricated a workspace manifest")
})
test("calc_current_child_and_unsafe_setwd_keep_project_output", {
  context <- new_case("cwd"); child<-file.path(context$project,"notes");dir.create(child)
  actual <- run(context,list(expr='value={setwd("notes");2+3}',unsafe=TRUE))
  expect_values(actual,list(value=5))
  check(!dir.exists(file.path(child,"utility-runs")),"Unsafe setwd redirected utility publication")
  run(context,list(expr="2+3"),cwd=child)
})
test("calc_config_template_key_and_cli_override_freeze_exact_bytes", {
  context<-new_case("templates"); configured<-file.path(context$base,"configured.md"); custom<-file.path(context$base,"custom.md")
  writeLines(c("---","tokens:","  title: Configured calculation","---","# {{title}}","{{table_body}}"),configured)
  writeLines(c("---","table:","  columns:","    - key: value","      label: Numeric answer","---","# CLI calculation","{{table_body}}"),custom)
  reset_config(list(templates=list(calc=list(default=configured))))
  first<-run(context,list(expr="answer=2+2",template="default"))
  check(grepl("Configured calculation",first$markdown,fixed=TRUE),"Configured template/key ignored")
  second<-run(context,list(expr="answer=2+2",template=custom))
  check(grepl("CLI calculation",second$markdown,fixed=TRUE)&&grepl("Numeric answer",second$markdown,fixed=TRUE),"CLI template/table columns ignored")
  saved<-sha(file.path(second$path,"template.md")); writeLines("Changed source template",custom)
  check(identical(saved,sha(file.path(second$path,"template.md"))),"Preserved template followed mutable source")
})
test("calc_custom_template_cannot_hide_unsafe_scope", {
  context<-new_case("scope"); template<-file.path(context$base,"minimal.md");writeLines("# Minimal calculation",template)
  actual<-run(context,list(expr="2+2",unsafe=TRUE,template=template))
  check(grepl("Unrestricted R",actual$markdown,fixed=TRUE)&&grepl("Unrestricted R",text(file.path(context$project,"report_canonical.md")),fixed=TRUE),"Custom template hid unrestricted-R limitation")
})
test("calc_explicit_invalid_templates_fail_before_unsafe_evaluation", {
  for (variant in c("missing-file", "unknown-key", "directory", "malformed-yaml")) {
    context <- new_case(paste0("invalid-template-", variant))
    template <- switch(variant, "missing-file"=file.path(context$base,"absent.md"),
      "unknown-key"="not-a-configured-key", "directory"=context$base,
      "malformed-yaml"=file.path(context$base,"malformed.md"))
    if (variant == "malformed-yaml") writeLines(c("---", "table: [", "---", "# Invalid"), template)
    actual <- run_cli(context,list(expr='value=as.integer(file.create("unwanted-effect.txt"))',unsafe=TRUE,template=template),failure=TRUE)
    check(!file.exists(file.path(context$project,"unwanted-effect.txt")) && !nzchar(actual$stdout) &&
      !length(runs(context)) && !file.exists(file.path(context$project,"report_canonical.md")),
      "Invalid explicit template executed unrestricted code or published output")
  }
})
test("calc_symlink_output_root_fails_before_unsafe_evaluation", {
  context <- new_case("symlink-root", manifest=FALSE)
  target <- file.path(context$base,"target"); alias <- file.path(context$base,"alias")
  dir.create(target)
  check(file.symlink(target,alias),"Unable to create isolated output-root symlink fixture")
  reset_config(list(defaults=list(output_dir=file.path(alias,"nested"))))
  actual <- run_cli(context,list(expr='value=as.integer(file.create("unwanted-effect.txt"))',unsafe=TRUE),failure=TRUE)
  check(!file.exists(file.path(context$project,"unwanted-effect.txt")) && !nzchar(actual$stdout) &&
    !length(list.files(target,all.files=TRUE,no..=TRUE)) && grepl("symlink",actual$stderr,fixed=TRUE),
    "Output-root alias guard ran too late or failed to protect its target")
})
test("calc_paths_comments_and_division_masked_only_in_human_projections", {
  context<-new_case("privacy"); input<-file.path(context$base,"private-values.txt");writeLines("12",input)
  expression<-paste0("value=as.numeric(readLines(",encodeString(input,quote='"'),"))/3 # /private/person/notes.txt")
  actual<-run(context,c(paste0("--expr=",expression),"--unsafe","TRUE","--format","json"))
  expect_values(actual,list(value=4))
  private<-text(file.path(actual$path,"request.json"))
  check(grepl(input,private,fixed=TRUE),"Exact private expression path was lost")
  for(path in file.path(context$project,c("report_canonical.md","analysis_log.jsonl"))){
    public<-text(path);check(!grepl(input,public,fixed=TRUE)&&!grepl("/private/person",public,fixed=TRUE),"Human projection exposed external path")
    check(grepl("<external>/private-values.txt",public,fixed=TRUE)&&grepl("/3",public,fixed=TRUE),"Display masking changed division or lost filename")
  }
})
test("calc_interactive_prompt_file_uses_configured_defaults", {
  context<-new_case("interactive"); prompts<-file.path(context$base,"prompts.txt")
  writeLines(c("alpha=0.05/3","","4","plain","","","TRUE","FALSE"),prompts)
  Sys.setenv(NLSS_PROMPT_FILE=prompts)
  actual<-run_cli(context,"--interactive")
  check(grepl("alpha = 0.0167",actual$stdout,fixed=TRUE)&&length(runs(context))==1L,"Interactive calculation failed")
})
test("calc_successive_calls_preserve_runs_and_continue_table_numbers", {
  context<-new_case("successive"); first<-run(context,list(expr="value=2")); preserved<-tree(first$path)
  second<-run(context,list(expr="value=3"))
  check(!identical(first$path,second$path)&&identical(preserved,snapshot(names(preserved))),"New calculation overwrote earlier audit")
  check(grepl("Table 1",first$markdown,fixed=TRUE)&&grepl("Table 1",second$markdown,fixed=TRUE)&&grepl("Table 2",text(file.path(context$project,"report_canonical.md")),fixed=TRUE),"Run-local/append-only numbering diverged")
})
invalid <- list(
  options=list(c("--expr","1","--digits","1.2"),c("--expr","1","--digits","Inf"),c("--expr","1","--digits","-1"),c("--expr","1","--digits","16"),c("--expr","1","--format","xml"),c("--expr","1","--unsafe","maybe"),c("--expr","1","--log","maybe"),c("--expr","1","--expr","2"),c("--unknown","1"),c("--csv","data.csv")),
  expressions=list(c("--expr","1+"),c("--expr","1;2"),c("--expr","TRUE"),c("--expr","1i","--unsafe","TRUE"),c("--expr","numeric(0)","--unsafe","TRUE"),c("--expr","||"),c("--set","1a=3","--expr","1")),
  restricted=list(c("--expr","c(1,2)"),c("--expr","base::sqrt(2)"),c("--expr",'system("echo unexpected")'),c("--expr","x<-3"),c("--expr","getwd()"),c("--expr","rnorm(2)"))
)
for(family in names(invalid)) test(paste0("calc_invalid_",family,"_refused_without_success_output"),{
  for(i in seq_along(invalid[[family]])){
    context<-new_case(paste0("invalid-",family,"-",i)); actual<-run_cli(context,invalid[[family]][[i]],failure=TRUE)
    check(!nzchar(actual$stdout)&&!length(runs(context))&&!file.exists(file.path(context$project,"report_canonical.md")),"Invalid request published success-shaped output")
  }
})
test("calc_publication_lock_leaves_no_stdout_success_or_projection_change", {
  context<-new_case("locked"); run(context,list(expr="1")); before<-tree(context$project); prior<-runs(context)
  dir.create(file.path(context$project,".publication-lock"))
  actual<-run_cli(context,list(expr="2"),failure=TRUE)
  check(!nzchar(actual$stdout)&&identical(prior,runs(context))&&identical(before,snapshot(names(before))),"Contended publication changed protected outputs or claimed stdout success")
})
test("calc_unsafe_external_side_effect_is_not_claimed_as_rolled_back", {
  context<-new_case("unsafe-effect");dir.create(file.path(context$project,".publication-lock"))
  actual<-run_cli(context,list(expr='value=as.integer(file.create("explicit-test-effect.txt"))',unsafe=TRUE),failure=TRUE)
  check(file.exists(file.path(context$project,"explicit-test-effect.txt"))&&!nzchar(actual$stdout)&&!length(runs(context)),"Unsafe side-effect/publication boundary is incorrect")
})
test("calc_report_directory_target_refused_without_replacement", {
  context<-new_case("directory-target");dir.create(file.path(context$project,"report_canonical.md"))
  actual<-run_cli(context,list(expr="2"),failure=TRUE)
  check(dir.exists(file.path(context$project,"report_canonical.md"))&&!nzchar(actual$stdout),"Utility replaced unsafe directory target or emitted success")
})
test("calc_statistical_replay_refuses_utility_record", {
  context<-new_case("no-replay"); actual<-run(context,list(expr="2"))
  prior<-tree(context$project)
  run_cli(context,c("--request",file.path(actual$path,"request.json")),failure=TRUE,module="replay_run")
  check(identical(prior,tree(context$project)),"Refused utility replay changed prior outputs")
})
test("calc_legacy_checksum_log_and_reconstructible_blocks_retained", {
  context<-new_case("legacy");reset_config(list(logging=list(include_checksum=TRUE)))
  run(context,list(expr="1"));run(context,list(expr="2"))
  log<-file.path(context$project,"analysis_log.jsonl");entries<-lapply(readLines(log),jsonlite::fromJSON)
  check(all(vapply(entries,function(row)!is.null(row$checksum)&&nzchar(row$report_block_b64),logical(1))),"Legacy checksum/report blocks absent")
  near(vapply(entries,function(row)row$log_seq,numeric(1)),c(1,2),"Legacy log sequence",0)
  run_cli(context,log,module="check_integrity")
})
source_after <- setNames(vapply(file.path(repo,names(source_before)),sha,character(1)),names(source_before))
check(identical(source_before,source_after),"Production source changed during tests; run from a frozen disposable copy.")
summary <- list(schema_version=1L,suite="phase2-calc",registered_cases=length(registered),selected_pattern=pattern,
  passed=sum(vapply(results,function(x)isTRUE(x$passed),logical(1))),failed=sum(vapply(results,function(x)!isTRUE(x$passed),logical(1))),
  cases=length(results),numeric_checks=numeric_checks,tests=results,processes=processes,source_files=as.list(source_before),
  runner_sha256=sha(script),finished_utc=utc())
write_json(summary,file.path(work,"results.json"))
cat(sprintf("Phase 2 calc: %d/%d cases passed; %d numeric comparisons. Results: %s\n",summary$passed,summary$cases,numeric_checks,file.path(work,"results.json")))
if (keep > 0L) {
  previous <- sort(list.dirs(collection,recursive=FALSE,full.names=TRUE))
  previous <- previous[grepl("^run-[0-9]{14}-[0-9]+$",basename(previous))]
  if(length(previous)>keep) for(path in head(previous,length(previous)-keep)) if(path!=work) unlink(path,recursive=TRUE)
}
if(!length(results)||summary$failed)quit(status=1L)
