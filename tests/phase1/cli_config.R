# SPDX-License-Identifier: Apache-2.0
#!/usr/bin/env Rscript
# Standalone offline regression tests; no repository configuration is modified.
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[[1]]
script_path <- normalizePath(sub("^--file=", "", script_arg), winslash = "/", mustWork = TRUE)
repo <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo, "scripts", "R", "lib", "cli.R"))
source(file.path(repo, "scripts", "R", "lib", "config.R"))
if (!requireNamespace("yaml", quietly = TRUE)) stop("These tests require yaml.")
test_config <- yaml::yaml.load_file(file.path(repo, "tests", "tests.yml"))$tests
run_root <- Sys.getenv("NLSS_TEST_ROOT", "")
if (!nzchar(run_root)) {
  run_root <- file.path(repo, test_config$output_dir, paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-phase1-cli"))
}
run_root <- normalizePath(run_root, winslash = "/", mustWork = FALSE)
test_dir <- file.path(run_root, "tmp", "phase1-cli-config")
dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)
messages <- character()
check <- function(label, value) {
  if (!isTRUE(value)) stop("FAIL: ", label, call. = FALSE)
  messages <<- c(messages, paste("PASS", label))
  cat(tail(messages, 1L), "\n")
}
expect_error <- function(label, expr, pattern) {
  message <- tryCatch({ force(expr); "" }, error = conditionMessage)
  check(label, nzchar(message) && grepl(pattern, message, fixed = TRUE))
}

check("embedded equals survives", identical(parse_args("--calc=score=(x1+x2)/2", module = "data_transform")$calc,
                                          "score=(x1+x2)/2"))
check("empty explicit string survives", identical(parse_args("--expr=", module = "calc")$expr, ""))
check("negative numeric values survive", identical(parse_args(c("--mu", "-1.5"), module = "t_test")$mu, "-1.5"))
check("formula with spaces survives", identical(parse_args(c("--formula", "y ~ x + (1 | id)"), module = "mixed_models")$formula,
                                               "y ~ x + (1 | id)"))
check("Boolean flags may omit a value", identical(parse_args("--bootstrap", module = "t_test")$bootstrap, TRUE))
check("SEM se is not a Boolean", identical(parse_args(c("--se", "bootstrap"), module = "sem")$se, "bootstrap"))
check("plot aliases remain accepted", identical(parse_args(c("--percent_base", "group"), module = "plot")$percent_base, "group"))
check("plot caption note remains an ordinary plot option", identical(parse_args(c("--note", "Figure explanation"), module = "plot")$note, "Figure explanation"))
for (module in c("descriptive_stats", "data_transform")) {
  expect_error("retired context flag rejected", parse_args(c("--context-note", "study.md"), module = module), "Unknown option --context-note")
}
for (token in c("true", "T", "1", "yes", " Y ")) check(paste("true token", token), parse_bool(token))
for (token in c("false", "F", "0", "no", " N ")) check(paste("false token", token), identical(parse_bool(token), FALSE))
check("null Boolean uses explicit default", parse_bool(NULL, TRUE))
for (value in list("flase", "", NA, c(TRUE, FALSE))) {
  expect_error("invalid Boolean rejected", parse_bool(value), "Invalid Boolean")
}
expect_error("Boolean validated before data access", parse_args(c("--bootstrap", "flase"), module = "t_test"), "Invalid Boolean for --bootstrap")
expect_error("plot se validated as Boolean", parse_args(c("--se", "maybe"), module = "plot"), "Invalid Boolean for --se")
expect_error("unknown flag rejected", parse_args(c("--bootstrap-sampels", "10"), module = "t_test"), "Unknown option --bootstrap-sampels")
expect_error("wrong module flag rejected", parse_args(c("--mu", "1"), module = "regression"), "Unknown option --mu")
expect_error("missing value rejected", parse_args("--csv", module = "t_test"), "Missing value for --csv")
expect_error("duplicate flag rejected", parse_args(c("--mu=1", "--mu=2"), module = "t_test"), "Duplicate option --mu")
expect_error("positional argument rejected", parse_args("data.csv", module = "t_test"), "Unexpected argument")
expect_error("malformed option rejected", parse_args("--=abc", module = "t_test"), "Unexpected argument")
check("new import flags accepted", length(parse_args(c("--dataset-name", "survey", "--import-action", "new-version",
      "--csv-decimal", ",", "--csv-encoding", "UTF-8", "--csv-col-types", "id=character", "--csv-na-values", "NA"), module = "regression")) == 6L)

# Check the handwritten registry against literal opts accesses in every shared-
# parser entrypoint. Dynamic helper keys are covered explicitly in the registry.
literal_opts <- function(node) {
  if (missing(node)) return(character())
  found <- character()
  if (is.call(node)) {
    operator <- as.character(node[[1]])[[1]]
    if (operator %in% c("$", "[[") && length(node) >= 3L && identical(node[[2]], as.name("opts"))) {
      key <- node[[3]]
      if ((operator == "$" && is.symbol(key)) || is.character(key)) found <- as.character(key)
    }
    for (child in as.list(node)) found <- c(found, literal_opts(child))
  } else if (is.expression(node) || is.list(node)) {
    for (child in node) found <- c(found, literal_opts(child))
  }
  unique(found)
}
for (module in names(cli_module_options())) {
  module_path <- file.path(repo, "scripts", "R", paste0(module, ".R"))
  consumed <- literal_opts(parse(module_path))
  check(paste("registry covers", module), !length(setdiff(consumed, cli_option_schema(module)$allowed)))
}

yaml_path <- file.path(repo, "scripts", "config.yml")
cfg <- load_config_file(yaml_path)
check("shipped configuration validates", is.list(cfg))
expect_error("retired inspector note default rejected", validate_config(list(modules = list(project_inspect = list(note = "study.md")))), "modules.project_inspect.note")
expect_error("retired context capture limit rejected", validate_config(list(modules = list(project_report = list(max_context_bytes = 12)))), "modules.project_report.max_context_bytes")
impute_keys <- c("skew_threshold", "suffix", "indicator", "indicator_suffix", "m", "maxit", "k", "seed")
check("imputation keys are under impute", all(impute_keys %in% names(cfg$modules$impute)))
check("imputation keys absent from research", !any(impute_keys %in% names(cfg$modules$research_academia)))
expect_error("unknown module key rejected", validate_config(list(modules = list(impute = list(maxitt = 4)))), "modules.impute.maxitt")
expect_error("unknown config section rejected", validate_config(list(loging = list(enabled = TRUE))), "loging")
expect_error("bad Boolean config rejected", validate_config(list(logging = list(enabled = "maybe"))), "logging.enabled")
expect_error("bad number config rejected", validate_config(list(defaults = list(digits = "many"))), "defaults.digits")
expect_error("vector scalar config rejected", validate_config(list(defaults = list(digits = c(2, 3)))), "defaults.digits")
expect_error("mapping shape rejected", validate_config(list(modules = "oops")), "modules")
expect_error("null nonnullable config rejected", validate_config(list(logging = list(enabled = NULL))), "logging.enabled")
check("quoted Boolean normalizes", identical(validate_config(list(logging = list(enabled = "false")))$logging$enabled, FALSE))
check("quoted number normalizes", identical(validate_config(list(defaults = list(digits = "4")))$defaults$digits, 4))
check("numeric EFA count accepted", identical(validate_config(list(modules = list(efa = list(n_factors = 3))))$modules$efa$n_factors, 3))
check("custom template variant accepted", identical(validate_config(list(templates = list(t_test = list(custom = "templates/my.md"))))$templates$t_test$custom,
                                                   "templates/my.md"))
expect_error("custom template path still validates", validate_config(list(templates = list(t_test = list(custom = TRUE)))), "templates.t_test.custom")
expect_error("unknown template module rejected", validate_config(list(templates = list(t_tset = list(default = "a.md")))), "templates.t_tset")

isolated_config <- file.path(test_dir, "config.yml")
yaml::write_yaml(merge_lists(get_builtin_config(), cfg), isolated_config)
Sys.setenv(NLSS_CONFIG_PATH = isolated_config)
check("explicit configuration path honored", identical(get_config_path(), normalizePath(isolated_config, winslash = "/")))
changed <- yaml::yaml.load_file(isolated_config)
changed$modules$impute$m <- 11
yaml::write_yaml(changed, isolated_config)
check("imputation YAML override is effective", identical(load_config()$modules$impute$m, 11))
Sys.setenv(NLSS_CONFIG_PATH = file.path(test_dir, "absent.yml"))
expect_error("missing explicit configuration rejected", get_config_path(), "NLSS_CONFIG_PATH")
Sys.setenv(NLSS_CONFIG_PATH = isolated_config)

# Simulate an unavailable yaml namespace in a private function environment.
dependency_env <- new.env(parent = environment(load_config_file))
dependency_env$requireNamespace <- function(...) FALSE
without_yaml <- load_config_file
environment(without_yaml) <- dependency_env
expect_error("missing yaml is actionable", without_yaml(isolated_config), "Missing dependency: yaml")

# Real command entrypoint: errors must occur without creating a workspace/log.
negative_dir <- file.path(test_dir, "invalid-cli")
dir.create(negative_dir, showWarnings = FALSE)
previous_wd <- getwd()
setwd(negative_dir)
for (arguments in list(c("--bootstrap", "flase"), c("--bootstrap-sampels", "10"), c("--csv"))) {
  output <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
    c(shQuote(file.path(repo, "scripts", "R", "t_test.R")), shQuote(arguments)), stdout = TRUE, stderr = TRUE))
  check(paste("entrypoint rejects", paste(arguments, collapse = " ")), !is.null(attr(output, "status")) && attr(output, "status") != 0L)
}
check("invalid commands create no artifacts", !length(list.files(negative_dir, all.files = TRUE, no.. = TRUE)))
setwd(previous_wd)
writeLines(messages, file.path(run_root, "phase1-cli-config.log"))
cat("CLI/config checks:", length(messages), "passed.\n")
