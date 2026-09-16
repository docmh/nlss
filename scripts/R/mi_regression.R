# SPDX-License-Identifier: Apache-2.0
#!/usr/bin/env Rscript
bootstrap_dir <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]), winslash = "/", mustWork = TRUE))
source(file.path(bootstrap_dir, "lib", "bootstrap.R"))
nlss_bootstrap()
source_lib("mi_pool.R")

main <- function() {
  opts <- nlss_run_options(commandArgs(TRUE), "mi_regression")
  if (!is.null(opts$help)) {
    cat("Multiple-imputation regression: fit each completed dataset, then pool model estimates.\n",
        "Usage: Rscript mi_regression.R --mids <mids.rds|artifact-directory> --formula 'y ~ x + group'\n",
        "Options: --family gaussian|binomial|poisson --link NAME --conf-level 0.95 --maxit 25\n",
        "         --digits 2 --template REF --log TRUE/FALSE --user-prompt TEXT\n",
        "Supported: Gaussian identity (lm), binomial logit/probit/cloglog, Poisson log.\n",
        "Formula: named variables, + - * : parentheses, intercept 0/1. No transforms, offsets, weights, subsets or mixed models.\n",
        "The preserved mids artifact and its input snapshot must pass integrity checks.\n", sep = "")
    quit(status = 0L)
  }
  if (!is.null(opts$interactive)) stop("MI regression requires an explicit mids artifact and model formula; interactive mode is not supported.")
  if (is.null(opts$mids) || is.null(opts$formula)) stop("MI regression requires --mids and --formula.")
  replay <- nlss_run_context$replay
  artifact_path <- if (is.null(replay)) normalize_input_path(opts$mids) else nlss_project_file(replay$request$imputation$path, replay$root)
  artifact <- nlss_read_mi_artifact(artifact_path, if (is.null(replay)) NULL else replay$root)
  out_dir <- dirname(dirname(dirname(artifact$artifact_path)))
  nlss_begin_run("mi_regression", artifact$mids$data, opts, out_dir, artifact$dataset_ref)
  nlss_run_context$request$cli$mids <- make_relative_path(artifact$artifact_path, artifact$workspace_root)
  nlss_run_context$request$imputation <- list(path = make_relative_path(artifact$artifact_path, artifact$workspace_root),
    sha256 = artifact$metadata$sha256, metadata_sha256 = artifact$metadata_sha256)
  if (!is.null(replay) && !identical(import_json(nlss_run_context$request$imputation), import_json(replay$request$imputation))) stop("Saved imputation artifact or metadata changed.")
  config <- function(key, fallback) get_config_value(paste0("modules.mi_regression.", key), fallback)
  family <- if (is.null(opts$family)) config("family", "gaussian") else opts$family
  link <- if (is.null(opts$link)) config("link", "") else opts$link
  if (!nzchar(link)) link <- switch(family, gaussian = "identity", binomial = "logit", poisson = "log", "")
  conf_level <- if (is.null(opts$`conf-level`)) config("conf_level", .95) else as.numeric(opts$`conf-level`)
  maxit <- if (is.null(opts$maxit)) config("maxit", 25) else as.numeric(opts$maxit)
  digits <- if (is.null(opts$digits)) get_config_value("defaults.digits", 2) else as.numeric(opts$digits)
  if (length(digits) != 1L || !is.finite(digits) || digits < 0 || digits > 15 || digits != floor(digits)) stop("Digits must be an integer from 0 to 15.")
  nlss_resolve_request(list(formula = opts$formula, family = family, link = link,
    conf_level = conf_level, maxit = maxit, digits = digits, m = artifact$mids$m),
    design = list(missing = "multiple_imputation", imputation_artifact = nlss_run_context$request$imputation))
  fit <- nlss_fit_mi(artifact$artifact_path, opts$formula, family, link, conf_level, artifact$workspace_root, maxit)
  nlss_save_run_rds(fit$fits, "fits.rds")
  nlss_save_run_rds(fit$pooled, "pooled.rds")
  table <- fit$coefficients
  fmt <- function(x) ifelse(is.na(x), "", formatC(x, digits = digits, format = "f"))
  rows <- lapply(seq_len(nrow(table)), function(i) c(as.character(table$term[i]),
    fmt(table$estimate[i]), fmt(table$std.error[i]), fmt(table$statistic[i]), fmt(table$df[i]),
    fmt(table$p.value[i]), fmt(table$conf.low[i]), fmt(table$conf.high[i])))
  body <- render_markdown_table(c("Term", "Estimate", "SE", "Statistic", "df", "p", "CI low", "CI high"), rows)
  note <- paste0("Rubin-pooled estimates from ", fit$metadata$m, " imputations; ",
    round(100 * conf_level), "% confidence intervals. ", fit$metadata$df_method,
    ". Coefficient scale: ", fit$metadata$coefficient_scale, ".")
  narrative <- paste0("The same ", family, " model was fitted to each completed dataset: ", fit$metadata$formula,
    ". Estimates and within-/between-imputation uncertainty were pooled with mice::pool. ",
    fit$metadata$imputation_diagnostics_note)
  template <- resolve_template_override(opts$template, "mi_regression")
  if (is.null(template)) template <- resolve_template_path("mi_regression.default", "mi-regression/default-template.md")
  template <- nlss_freeze_template(template, "mi_regression.default")
  nlss_stage_report(file.path(out_dir, "report_canonical.md"), "Multiple-imputation regression",
    paste0("Table 1\n\n", body, "\n", note), narrative,
    analysis_flags = nlss_run_context$request$options, template_path = template,
    template_context = list(tokens = list(table_body = body, note_default = note, narrative_default = narrative)))
  results <- list(coefficients_df = table, inference_pooled = TRUE, m = fit$metadata$m,
    model_family = family, link = link, diagnostics = fit$diagnostics,
    pooling = fit$metadata, imputation_artifact = nlss_run_context$request$imputation)
  nlss_set_result(results)
  if (parse_bool(opts$log, get_config_value("defaults.log", TRUE))) {
    context <- get_run_context()
    nlss_stage_log(out_dir, "mi_regression", context$prompt, context$commands, results,
      nlss_run_context$request$options, get_user_prompt(opts))
  }
}
nlss_run_main("mi_regression", main)
