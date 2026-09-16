# SPDX-License-Identifier: Apache-2.0
#!/usr/bin/env Rscript
# Offline tests; canonical installation files are never modified.
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[[1]]
script_path <- normalizePath(sub("^--file=", "", script_arg), winslash = "/", mustWork = TRUE)
repo <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
if (!requireNamespace("yaml", quietly = TRUE)) stop("These tests require yaml.")
settings <- yaml::yaml.load_file(file.path(repo, "tests", "tests.yml"))$tests
run_root <- Sys.getenv("NLSS_TEST_ROOT", "")
if (!nzchar(run_root)) {
  run_root <- file.path(repo, settings$output_dir, paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-phase2-bootstrap"))
}
run_root <- normalizePath(run_root, winslash = "/", mustWork = FALSE)
test_dir <- file.path(run_root, "tmp", "phase2-bootstrap-config")
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
prior_override <- Sys.getenv("NLSS_CONFIG_PATH", unset = NA_character_)
Sys.unsetenv("NLSS_CONFIG_PATH")
loader <- new.env(parent = globalenv())
source(file.path(repo, "scripts", "R", "lib", "bootstrap.R"), local = loader)
target <- new.env(parent = globalenv())
loaded <- loader$nlss_bootstrap(target)
check("bootstrap loads fixed required libraries", all(c("paths.R", "cli.R", "config.R", "io.R",
       "data_utils.R", "formatting.R", "run_contract.R") %in% loaded$libraries))
check("bootstrap exports functions in target environment", exists("load_dataframe", envir = target, inherits = FALSE) &&
       exists("get_config_value", envir = target, inherits = FALSE))
check("bootstrap anchors script path", identical(target$get_script_dir(), file.path(repo, "scripts", "R")))
canonical_path <- file.path(repo, "scripts", "config.yml")
canonical <- yaml::yaml.load_file(canonical_path, eval.expr = FALSE)
check("built-in compatibility API reads canonical YAML", isTRUE(all.equal(target$get_builtin_config(), canonical)))
check("effective defaults equal canonical YAML", isTRUE(all.equal(target$load_config(), canonical)))
check("canonical output directory replaces former duplicate fallback", identical(target$get_config_value("defaults.output_dir"), canonical$defaults$output_dir))

override_path <- file.path(test_dir, "override.yml")
yaml::write_yaml(list(defaults = list(digits = "4", csv = list(header = "false")),
                      templates = list(regression = list(custom = "my-template.md"))), override_path)
Sys.setenv(NLSS_CONFIG_PATH = override_path)
effective <- target$load_config()
check("partial numeric override normalizes", identical(effective$defaults$digits, 4))
check("partial Boolean override normalizes", identical(effective$defaults$csv$header, FALSE))
check("partial override preserves other defaults", identical(effective$modules, target$get_builtin_config()$modules))
check("custom template variants remain supported", identical(effective$templates$regression$custom, "my-template.md"))
check("override does not become default source", identical(target$get_builtin_config()$defaults$digits, as.numeric(canonical$defaults$digits)))
check("NULL-valued defaults remain named keys", "binwidth" %in% names(effective$modules$plot) && is.null(effective$modules$plot$binwidth))
check("merge preserves explicit nullable keys", "x" %in% names(target$merge_lists(list(x = NULL), list(x = NULL))))
expect_error("unknown override key rejects", target$validate_config(list(defaults = list(digtis = 4))), "defaults.digtis")
expect_error("wrong scalar type rejects", target$validate_config(list(defaults = list(digits = "many"))), "defaults.digits")
expect_error("invalid Boolean rejects", target$validate_config(list(logging = list(enabled = "maybe"))), "logging.enabled")
expect_error("mapping shape rejects", target$validate_config(list(modules = "bad")), "modules")
expect_error("invalid custom template rejects", target$validate_config(list(templates = list(regression = list(custom = TRUE)))), "templates.regression.custom")
expect_error("unknown template module rejects", target$validate_config(list(templates = list(typo = list(default = "a.md")))), "templates.typo")
writeLines("defaults: [", override_path)
expect_error("invalid YAML is explicit", target$load_config(), "Invalid NLSS YAML")
writeLines(character(), override_path)
expect_error("empty override rejects", target$load_config(), "configuration is empty")
Sys.setenv(NLSS_CONFIG_PATH = file.path(test_dir, "missing.yml"))
expect_error("missing explicit override rejects", target$load_config(), "NLSS_CONFIG_PATH")
Sys.unsetenv("NLSS_CONFIG_PATH")

# A copied, deliberately incomplete installation proves that neither an
# external caller's config nor a complete override silently replaces defaults.
copy_root <- file.path(test_dir, "installation")
copy_lib <- file.path(copy_root, "scripts", "R", "lib")
dir.create(copy_lib, recursive = TRUE, showWarnings = FALSE)
invisible(file.copy(file.path(repo, "scripts", "R", "lib", "config.R"), file.path(copy_lib, "config.R"), overwrite = TRUE))
copy_env <- new.env(parent = globalenv())
source(file.path(copy_lib, "config.R"), local = copy_env)
yaml::write_yaml(canonical, override_path)
Sys.setenv(NLSS_CONFIG_PATH = override_path)
expect_error("missing canonical defaults never use override as fallback", copy_env$load_config(), "Missing canonical NLSS configuration")
copy_config <- file.path(copy_root, "scripts", "config.yml")
invisible(file.copy(canonical_path, copy_config, overwrite = TRUE))
changed <- canonical
changed$defaults$output_dir <- "./canonical-only"
yaml::write_yaml(changed, copy_config)
check("canonical value changes need no R edit", identical(copy_env$get_builtin_config()$defaults$output_dir, "./canonical-only"))
Sys.unsetenv("NLSS_CONFIG_PATH")
check("canonical value is effective without override", identical(copy_env$load_config()$defaults$output_dir, "./canonical-only"))
writeLines("[]", copy_config)
expect_error("invalid canonical structure rejects", copy_env$load_config(), "Invalid canonical NLSS configuration")
writeLines("defaults: [", copy_config)
expect_error("invalid canonical YAML rejects", copy_env$load_config(), "Invalid NLSS YAML")
dependency_env <- new.env(parent = environment(target$read_config_yaml))
dependency_env$requireNamespace <- function(...) FALSE
without_yaml <- target$read_config_yaml
environment(without_yaml) <- dependency_env
expect_error("missing yaml is actionable", without_yaml(canonical_path), "Missing dependency: yaml")

expect_error("missing library fails before sourcing", loader$nlss_bootstrap(new.env(), lib_dir = copy_lib), "Missing mandatory NLSS libraries")
broken_lib <- file.path(test_dir, "broken-libraries")
dir.create(broken_lib, showWarnings = FALSE)
invisible(file.copy(list.files(file.path(repo, "scripts", "R", "lib"), full.names = TRUE), broken_lib, overwrite = TRUE))
writeLines("# intentionally missing mandatory CLI functions", file.path(broken_lib, "cli.R"))
expect_error("missing function never uses inherited fallback", loader$nlss_bootstrap(new.env(parent = target), lib_dir = broken_lib), "Missing mandatory NLSS functions")
previous_wd <- getwd()
setwd(test_dir)
target$source_lib("data_utils.R")
check("source_lib works independently of caller CWD", exists("select_variables", envir = target, inherits = FALSE))
setwd(previous_wd)
expect_error("source_lib missing file is explicit", target$source_lib("does-not-exist.R"), "Missing mandatory NLSS library")
expect_error("source_lib rejects traversal", target$source_lib("../config.R"), "one library filename")
if (is.na(prior_override)) Sys.unsetenv("NLSS_CONFIG_PATH") else Sys.setenv(NLSS_CONFIG_PATH = prior_override)
writeLines(messages, file.path(run_root, "phase2-bootstrap-config.log"))
cat("Bootstrap/config checks:", length(messages), "passed.\n")
