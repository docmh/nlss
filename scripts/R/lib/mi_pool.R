# SPDX-License-Identifier: Apache-2.0
# Deliberately small adapter: established lm/glm fits, established mice pooling.

nlss_mi_require <- function() {
  needed <- c("mice", "digest", "jsonlite", "arrow", "broom")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("MI inference requires R packages: ", paste(missing, collapse = ", "), ".", call. = FALSE)
}

nlss_mi_hash <- function(path) digest::digest(file = path, algo = "sha256")
nlss_mi_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
nlss_mi_sha <- function(x) nlss_mi_string(x) && grepl("^[0-9a-f]{64}$", x)

nlss_mi_resolve <- function(path, root) {
  if (!nlss_mi_string(path) || grepl("^(/|[A-Za-z]:|\\\\)", path)) {
    stop("MI provenance paths must be workspace-relative.", call. = FALSE)
  }
  resolved <- normalizePath(file.path(root, path), winslash = "/", mustWork = TRUE)
  if (!startsWith(resolved, paste0(root, "/"))) stop("MI provenance path escapes its workspace.", call. = FALSE)
  resolved
}

nlss_read_mi_artifact <- function(artifact_path, workspace_root = NULL) {
  nlss_mi_require()
  if (!nlss_mi_string(artifact_path)) stop("Specify one preserved mids artifact path.", call. = FALSE)
  if (dir.exists(artifact_path)) artifact_path <- file.path(artifact_path, "mids.rds")
  artifact_path <- normalizePath(artifact_path, winslash = "/", mustWork = TRUE)
  source_lib("project_store.R")
  metadata_path <- file.path(dirname(artifact_path), "metadata.json")
  if (!nlss_project_regular_file(artifact_path) || !nlss_project_regular_file(metadata_path)) {
    stop("MI artifact requires regular mids.rds and adjacent metadata.json files.", call. = FALSE)
  }
  metadata_hash <- nlss_mi_hash(metadata_path)
  metadata <- jsonlite::read_json(metadata_path, simplifyVector = FALSE)
  if (!identical(metadata$schema_version, 1L) || !identical(metadata$object_class, "mids") ||
      !nlss_mi_sha(metadata$sha256) ||
      !identical(metadata$artifact_id, paste0("mice-", metadata$sha256)) ||
      !identical(basename(dirname(artifact_path)), metadata$artifact_id) ||
      !identical(basename(artifact_path), "mids.rds")) {
    stop("MI artifact metadata or content-addressed location is invalid.", call. = FALSE)
  }
  relative <- paste0(".nlss/imputations/", metadata$artifact_id, "/mids.rds")
  if (!identical(metadata$path, relative) || !endsWith(artifact_path, paste0("/", relative))) {
    stop("MI artifacts must use the current .nlss/imputations layout.", call. = FALSE)
  }
  # The existing metadata declares the relative location. Strip that suffix,
  # not a fixed number of parents; no old-layout search or new project adoption.
  root <- substr(artifact_path, 1L, nchar(artifact_path) - nchar(relative) - 1L)
  if (!is.null(workspace_root) && !identical(normalize_path(workspace_root), root)) stop("MI artifact belongs to a different project.", call. = FALSE)
  locations <- nlss_resolve_locations(start = root, use_dataset = FALSE)
  selected <- nlss_run_context$project_selection
  if ((!is.null(locations$project_root) && !identical(locations$project_root, root)) ||
      (!is.null(selected) && !identical(selected, root))) stop("MI artifact belongs to a different selected project.", call. = FALSE)
  workspace_root <- root
  for (path in c(relative, make_relative_path(metadata_path, root))) nlss_managed_path(root, path, file = TRUE)
  if (!identical(nlss_mi_hash(artifact_path), metadata$sha256)) stop("MI artifact SHA-256 verification failed.", call. = FALSE)
  mids <- readRDS(artifact_path)
  if (!identical(nlss_mi_hash(artifact_path), metadata$sha256)) stop("MI artifact changed while reading.", call. = FALSE)
  if (!inherits(mids, "mids") || !is.data.frame(mids$data) ||
      length(mids$m) != 1L || !is.finite(mids$m) || mids$m < 2 || mids$m != as.integer(mids$m)) {
    stop("MI inference requires an actual mids object with at least two imputations.", call. = FALSE)
  }
  if (!identical(as.numeric(metadata$m), as.numeric(mids$m)) ||
      !identical(as.numeric(metadata$iterations), as.numeric(mids$iteration)) ||
      !identical(metadata$mice_version, as.character(mids$version)) ||
      (!is.null(metadata$seed) && !identical(as.numeric(metadata$seed), as.numeric(mids$seed)))) {
    stop("MI artifact metadata disagrees with the preserved mids object.", call. = FALSE)
  }
  ref <- metadata$dataset
  if (!is.list(ref) || !nlss_mi_string(ref$dataset_id) || !nlss_mi_string(ref$version_id) ||
      !nlss_mi_sha(ref$data_sha256) || !nlss_mi_sha(ref$dictionary_sha256)) {
    stop("MI inference requires a verified input dataset reference in artifact metadata.", call. = FALSE)
  }
  if (!identical(nlss_mi_resolve(metadata$path, workspace_root), artifact_path)) {
    stop("MI artifact location disagrees with its workspace-relative metadata path.", call. = FALSE)
  }
  snapshot <- nlss_mi_resolve(ref$snapshot_path, workspace_root)
  dictionary <- nlss_mi_resolve(ref$dictionary_path, workspace_root)
  if (!identical(nlss_mi_hash(snapshot), ref$data_sha256) ||
      !identical(nlss_mi_hash(dictionary), ref$dictionary_sha256)) {
    stop("MI input snapshot or dictionary failed SHA-256 verification.", call. = FALSE)
  }
  provenance_path <- if (identical(ref$origin, "managed_parquet")) {
    nlss_managed_path(workspace_root, paste0(".nlss/datasets/", ref$dataset_id, "/versions/", ref$version_id, ".json"), file = TRUE)
  } else file.path(dirname(snapshot), "provenance.json")
  if (!file.exists(provenance_path)) stop("MI input snapshot provenance.json is missing.", call. = FALSE)
  provenance <- jsonlite::read_json(provenance_path, simplifyVector = FALSE)
  keys <- c("schema_version", "dataset_id", "version_id", "data_sha256", "dictionary_sha256", "snapshot_path", "dictionary_path", "source_sha256", "import_version_id", "origin")
  if (!identical(provenance[keys], ref[keys])) stop("MI dataset reference disagrees with immutable snapshot provenance.", call. = FALSE)
  # Phase 1 mice receives a selected subset, converting character to factor.
  # Compare every preserved original value and NA position, not imputed values.
  original <- as.data.frame(arrow::read_parquet(snapshot))
  if (anyDuplicated(names(mids$data)) || !all(names(mids$data) %in% names(original)) || nrow(original) != nrow(mids$data)) {
    stop("MI original data do not match the referenced dataset snapshot.", call. = FALSE)
  }
  for (name in names(mids$data)) {
    before <- original[[name]]
    engine <- mids$data[[name]]
    if (is.character(before)) before <- as.factor(before)
    same <- if (is.factor(before) || is.factor(engine)) {
      is.factor(before) && is.factor(engine) && identical(levels(before), levels(engine)) &&
        identical(is.ordered(before), is.ordered(engine)) && identical(as.character(before), as.character(engine))
    } else {
      isTRUE(all.equal(as.vector(before), as.vector(engine), tolerance = 0, check.attributes = FALSE))
    }
    if (!same) stop("MI original values or missingness disagree with the snapshot for variable: ", name, ".", call. = FALSE)
  }
  if (!identical(nlss_mi_hash(snapshot), ref$data_sha256) ||
      !identical(nlss_mi_hash(dictionary), ref$dictionary_sha256) ||
      !identical(nlss_mi_hash(metadata_path), metadata_hash)) stop("MI provenance changed while validating.", call. = FALSE)
  list(mids = mids, metadata = metadata, dataset_ref = ref, workspace_root = workspace_root,
       artifact_path = artifact_path, metadata_sha256 = metadata_hash)
}

nlss_mi_formula <- function(formula, columns) {
  if (!nlss_mi_string(formula)) stop("MI formula must be one nonempty text string.", call. = FALSE)
  parsed <- tryCatch(parse(text = formula, keep.source = FALSE), error = function(e) NULL)
  if (length(parsed) != 1L || !is.call(parsed[[1]]) || !identical(parsed[[1]][[1]], as.name("~")) ||
      length(parsed[[1]]) != 3L || !is.symbol(parsed[[1]][[2]])) {
    stop("MI formula must be response ~ predictors with one response column.", call. = FALSE)
  }
  validate <- function(node) {
    if (is.symbol(node)) {
      name <- as.character(node)
      if (name == "." || !name %in% columns) stop("Unknown or implicit MI formula variable: ", name, ".", call. = FALSE)
      return(invisible(NULL))
    }
    if (is.numeric(node) && length(node) == 1L && node %in% c(0, 1)) return(invisible(NULL))
    if (is.call(node) && is.symbol(node[[1]]) && as.character(node[[1]]) %in% c("+", "-", "*", ":", "(") &&
        ((as.character(node[[1]]) %in% c("+", "-") && length(node) %in% c(2L, 3L)) ||
         (as.character(node[[1]]) %in% c("*", ":") && length(node) == 3L) ||
         (as.character(node[[1]]) == "(" && length(node) == 2L))) {
      for (child in as.list(node)[-1L]) validate(child)
      return(invisible(NULL))
    }
    stop("Unsupported MI formula expression. Use column names and +, -, *, :, parentheses, or intercept 0/1; precompute transformations explicitly.", call. = FALSE)
  }
  validate(parsed[[1]][[2]])
  validate(parsed[[1]][[3]])
  out <- stats::as.formula(parsed[[1]], env = baseenv())
  if (as.character(out[[2]]) %in% all.vars(out[[3]])) stop("MI response cannot also be a predictor.", call. = FALSE)
  out
}

nlss_fit_mi <- function(artifact_path, formula, family = "gaussian", link = NULL,
                        conf_level = 0.95, workspace_root = NULL, maxit = 25L) {
  if (!nlss_mi_string(family) || !family %in% c("gaussian", "binomial", "poisson")) {
    stop("Supported MI families are gaussian, binomial, and poisson only; no fallback fit was run.", call. = FALSE)
  }
  if (is.null(link)) link <- switch(family, gaussian = "identity", binomial = "logit", poisson = "log")
  supported_links <- switch(family, gaussian = "identity", binomial = c("logit", "probit", "cloglog"), poisson = "log")
  if (!nlss_mi_string(link) || !link %in% supported_links) stop("Unsupported MI link for ", family, ": ", paste(link, collapse = ","), ".", call. = FALSE)
  if (!is.numeric(conf_level) || length(conf_level) != 1L || !is.finite(conf_level) || conf_level <= 0 || conf_level >= 1) stop("MI confidence level must be between 0 and 1.", call. = FALSE)
  if (!is.numeric(maxit) || length(maxit) != 1L || !is.finite(maxit) || maxit < 1 || maxit != as.integer(maxit)) stop("MI GLM maxit must be a positive integer.", call. = FALSE)
  artifact <- nlss_read_mi_artifact(artifact_path, workspace_root)
  mids <- artifact$mids
  model_formula <- nlss_mi_formula(formula, names(mids$data))
  model_vars <- all.vars(model_formula)
  response <- as.character(model_formula[[2]])
  for (name in model_vars) {
    x <- mids$data[[name]]
    if (!is.factor(x) && !identical(class(x), "numeric") && !identical(class(x), "integer") && !identical(class(x), "logical")) {
      stop("Unsupported MI model column type for ", name, "; explicitly prepare numeric or factor columns before imputation.", call. = FALSE)
    }
  }
  completed <- mice::complete(mids, action = "all")
  if (length(completed) != mids$m) stop("MI artifact did not yield every imputation.", call. = FALSE)
  first <- completed[[1L]]
  response_coding <- list(variable = response, type = class(first[[response]]))
  if (family == "binomial") {
    y <- first[[response]]
    if (is.factor(y)) {
      if (nlevels(y) != 2L) stop("MI binomial response must have exactly two factor levels or numeric 0/1 coding.", call. = FALSE)
      response_coding$failure <- levels(y)[1L]
      response_coding$success <- levels(y)[2L]
    } else {
      response_coding$failure <- 0
      response_coding$success <- 1
    }
  }
  # Freeze contrasts explicitly; ambient options(contrasts=...) cannot change fits.
  factor_vars <- setdiff(model_vars[vapply(first[model_vars], is.factor, logical(1))], response)
  contrasts <- lapply(first[factor_vars], function(x) {
    if (nlevels(x) < 2L) stop("MI factor predictor requires at least two levels.", call. = FALSE)
    if (is.ordered(x)) stats::contr.poly(nlevels(x)) else stats::contr.treatment(levels(x), base = 1L)
  })
  fits <- vector("list", mids$m)
  diagnostics <- vector("list", mids$m)
  expected_columns <- NULL
  expected_df <- NULL
  for (i in seq_len(mids$m)) {
    dat <- completed[[i]]
    prefix <- paste0("MI fit ", i, "/", mids$m, ": ")
    if (!identical(names(dat), names(first)) || nrow(dat) != nrow(mids$data) ||
        !identical(row.names(dat), row.names(first))) stop(prefix, "completed row/column identities changed.", call. = FALSE)
    for (name in model_vars) {
      x <- dat[[name]]
      base <- mids$data[[name]]
      if (!identical(class(x), class(base)) || (is.factor(x) && !identical(levels(x), levels(base)))) stop(prefix, "column classes or factor levels changed for ", name, ".", call. = FALSE)
      if (anyNA(x) || (is.numeric(x) && any(!is.finite(x)))) stop(prefix, "model variables contain missing or non-finite values; no rows are silently dropped.", call. = FALSE)
    }
    y <- dat[[response]]
    if (family == "gaussian" && (!is.numeric(y) || is.factor(y))) stop(prefix, "Gaussian response must be numeric.", call. = FALSE)
    if (family == "binomial") {
      values <- if (is.factor(y)) as.integer(y) - 1L else as.numeric(y)
      if (!all(values %in% c(0, 1)) || length(unique(values)) != 2L) stop(prefix, "binomial response must contain both outcomes with stable 0/1 or two-level factor coding.", call. = FALSE)
    }
    if (family == "poisson" && (!is.numeric(y) || any(y < 0 | y != floor(y)))) stop(prefix, "Poisson response must contain nonnegative integer counts.", call. = FALSE)
    fit <- tryCatch(withCallingHandlers({
      if (family == "gaussian") {
        stats::lm(model_formula, data = dat, na.action = stats::na.fail, singular.ok = FALSE, contrasts = contrasts, x = TRUE, y = TRUE)
      } else {
        fam <- if (family == "binomial") stats::binomial(link = link) else stats::poisson(link = link)
        stats::glm(model_formula, data = dat, family = fam, na.action = stats::na.fail, singular.ok = FALSE,
                   contrasts = contrasts, x = TRUE, y = TRUE, control = stats::glm.control(maxit = maxit))
      }
    }, warning = function(w) stop("model warning: ", conditionMessage(w), call. = FALSE)),
    error = function(e) stop(prefix, conditionMessage(e), call. = FALSE))
    if (family != "gaussian" && (!isTRUE(fit$converged) || isTRUE(fit$boundary))) stop(prefix, "GLM did not converge to an interior solution.", call. = FALSE)
    if (fit$rank != ncol(fit$x) || any(!is.finite(stats::coef(fit))) || any(!is.finite(stats::vcov(fit))) ||
        any(diag(stats::vcov(fit)) <= 0) || stats::df.residual(fit) <= 0) stop(prefix, "singular, degenerate, or non-finite model uncertainty; pooling aborted.", call. = FALSE)
    if (is.null(expected_columns)) {
      expected_columns <- colnames(fit$x)
      expected_df <- stats::df.residual(fit)
    }
    if (!identical(colnames(fit$x), expected_columns) || !identical(stats::df.residual(fit), expected_df)) stop(prefix, "model terms or complete-data degrees of freedom changed.", call. = FALSE)
    fits[[i]] <- fit
    diagnostics[[i]] <- list(imputation = i, n = stats::nobs(fit), rank = fit$rank,
      df_residual = stats::df.residual(fit), converged = if (family == "gaussian") TRUE else fit$converged,
      boundary = if (family == "gaussian") FALSE else fit$boundary,
      iterations = if (family == "gaussian") NULL else fit$iter,
      residual_deviance = stats::deviance(fit), aic = stats::AIC(fit), warnings = character())
  }
  pooled <- withCallingHandlers(mice::pool(fits, dfcom = expected_df, rule = "rubin1987"),
                               warning = function(w) stop("MI pooling warning: ", conditionMessage(w), call. = FALSE))
  table <- as.data.frame(summary(pooled, type = "all", conf.int = TRUE, conf.level = conf_level, exponentiate = FALSE))
  fields <- c("term", "estimate", "std.error", "statistic", "df", "p.value", "conf.low", "conf.high", "m", "riv", "lambda", "fmi", "ubar", "b", "t", "dfcom")
  table <- table[, fields, drop = FALSE]
  table$term <- as.character(table$term)
  if (!identical(table$term, expected_columns) || any(table$m != mids$m) ||
      any(!is.finite(as.matrix(table[, setdiff(fields, "term"), drop = FALSE])))) stop("MI pooling returned incomplete or non-finite term estimates.", call. = FALSE)
  list(coefficients = table, fits = fits, pooled = pooled, diagnostics = diagnostics,
       metadata = list(schema_version = 1L, inference_pooled = TRUE, pooling_rule = "rubin1987",
         df_method = "Barnard-Rubin adjustment via mice::pool", complete_data_df = expected_df,
         m = mids$m, formula = paste(deparse(model_formula, width.cutoff = 500L), collapse = " "),
         family = family, link = link, conf_level = conf_level, maxit = maxit,
         coefficient_scale = if (family == "gaussian") "response" else "link",
         response_coding = response_coding, contrasts = contrasts,
         mice_version = as.character(utils::packageVersion("mice")), r_version = R.version.string,
         imputation_iterations = mids$iteration, imputation_logged_events = mids$loggedEvents,
         imputation_diagnostics_note = "Stored chain diagnostics and logged events must be reviewed; successful model fitting is not proof of imputation convergence or an adequate missingness model."),
       dataset_ref = artifact$dataset_ref,
       artifact = c(artifact$metadata, list(metadata_sha256 = artifact$metadata_sha256)))
}
