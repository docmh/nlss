# SPDX-License-Identifier: Apache-2.0
# Shared SEM syntax and lavaan boundary, used explicitly by SEM and its diagnostics.

normalize_model_syntax <- function(text) {
  if (is.null(text)) return("")
  # lavaan already parses semicolon separators. Replacing them globally would
  # turn a semicolon inside a model comment into executable model syntax.
  trimws(gsub("\r\n?", "\n", paste(text, collapse = "\n")))
}

parse_factor_spec <- function(text) {
  if (is.null(text) || !nzchar(text)) return(list())
  factors <- list()
  for (chunk in strsplit(text, ";", fixed = TRUE)[[1]]) {
    if (!nzchar(trimws(chunk))) next
    pair <- trimws(strsplit(chunk, "=", fixed = TRUE)[[1]])
    if (length(pair) != 2L || !nzchar(pair[1])) stop("Invalid factor specification: ", chunk)
    items <- trimws(strsplit(pair[2], ",", fixed = TRUE)[[1]])
    if (!length(items) || any(!nzchar(items)) || anyDuplicated(items) || pair[1] %in% names(factors)) {
      stop("Empty or duplicate factor/items in specification: ", chunk)
    }
    factors[[pair[1]]] <- items
  }
  factors
}

build_cfa_model <- function(factors) {
  paste(vapply(names(factors), function(name) paste0(name, " =~ ", paste(factors[[name]], collapse = " + ")), character(1)), collapse = "\n")
}

build_path_model <- function(dv, ivs, covariates) {
  if (!nzchar(dv) || !length(ivs)) return("")
  paste0(dv, " ~ ", paste(unique(c(ivs, covariates)), collapse = " + "))
}

build_mediation_model <- function(x, mediators, y, covariates, serial = FALSE) {
  if (!nzchar(x) || !nzchar(y) || !length(mediators)) return("")
  roles <- c(x, mediators, y, covariates)
  if (anyDuplicated(roles)) stop("Mediation predictor, mediators, outcome and covariates must be distinct.")
  cov_text <- if (length(covariates)) paste0(" + ", paste(covariates, collapse = " + ")) else ""
  if (isTRUE(serial)) {
    if (length(mediators) != 2L) stop("Serial mediation currently supports exactly two mediators.")
    m1 <- mediators[1]; m2 <- mediators[2]
    lines <- c(paste0(m1, " ~ a1*", x, cov_text),
      paste0(m2, " ~ a2*", x, " + d21*", m1, cov_text),
      paste0(y, " ~ c_prime*", x, " + b1*", m1, " + b2*", m2, cov_text),
      paste0("indirect_", m1, " := a1*b1"), paste0("indirect_", m2, " := a2*b2"),
      "indirect_serial := a1*d21*b2",
      paste0("total_indirect := indirect_", m1, " + indirect_", m2, " + indirect_serial"),
      "total := c_prime + total_indirect")
  } else {
    lines <- paste0(mediators, " ~ a", seq_along(mediators), "*", x, cov_text)
    lines <- c(lines, paste0(y, " ~ c_prime*", x, " + ",
      paste0("b", seq_along(mediators), "*", mediators, collapse = " + "), cov_text),
      paste0("indirect_", mediators, " := a", seq_along(mediators), "*b", seq_along(mediators)),
      paste0("total_indirect := ", paste0("indirect_", mediators, collapse = " + ")),
      "total := c_prime + total_indirect")
  }
  paste(lines, collapse = "\n")
}

extract_model_vars <- function(model_syntax) {
  if (is.null(model_syntax) || !nzchar(model_syntax)) return(character())
  # lavaan's parser handles modifiers, intercept-only models and thresholds;
  # never reinterpret failed lavaan syntax with a heuristic token parser.
  lavaan::lavNames(lavaan::lavaanify(model_syntax, auto = FALSE), "ov")
}

sem_group_factor <- function(values) {
  if (!is.numeric(values)) return(as.factor(values))
  codes <- ifelse(is.na(values), NA_character_, sprintf("%.17g", values))
  factor(codes, levels = unique(codes[!is.na(codes)]))
}

sem_ordered_factor <- function(values) {
  if (!is.numeric(values)) return(as.ordered(values))
  # Match the numeric values before converting display labels: factor()'s
  # default character conversion can merge distinct adjacent double codes.
  observed <- sort(unique(values[!is.na(values)]))
  factor(match(values, observed), levels = seq_along(observed),
         labels = sprintf("%.17g", observed), ordered = TRUE)
}

fit_sem_model <- function(analysis, model_syntax, df, estimator, missing, se,
                          bootstrap_samples, ordered_vars, group_var, group_equal) {
  args <- list(model = model_syntax, data = df, estimator = estimator, missing = missing, se = se)
  if (length(ordered_vars)) args$ordered <- ordered_vars
  if (nzchar(group_var)) args$group <- group_var
  if (length(group_equal)) args$group.equal <- group_equal
  if (se == "bootstrap") args$bootstrap <- bootstrap_samples
  do.call(if (analysis == "cfa") lavaan::cfa else lavaan::sem, args)
}

sem_fit_audit <- function(fit, requested = list(), source_n = NULL) {
  options <- lavaan::lavInspect(fit, "options")
  converged <- isTRUE(lavaan::lavInspect(fit, "converged"))
  admissible <- tryCatch(lavaan::lavInspect(fit, "post.check"), error = function(e) NA)
  pt <- lavaan::parTable(fit)
  negative <- pt[pt$op == "~~" & pt$lhs == pt$rhs & is.finite(pt$est) & pt$est < 0,
    intersect(c("lhs", "op", "rhs", "group", "est"), names(pt)), drop = FALSE]
  free <- pt$free > 0
  se_available <- !any(free) || ("se" %in% names(pt) && all(is.finite(pt$se[free])))
  fit_status <- list(converged = converged, admissible = admissible,
    status = if (!converged) "not_converged" else if (is.na(admissible)) "admissibility_unavailable" else if (!admissible) "inadmissible" else "available",
    standard_errors_available = se_available, negative_variances = negative,
    iterations = lavaan::lavInspect(fit, "iterations"),
    n_parameters = lavaan::lavInspect(fit, "npar"))
  effective <- options[intersect(c("estimator", "estimator.orig", "missing", "se", "test", "information",
    "meanstructure", "fixed.x", "conditional.x", "bootstrap", "parameterization", "group.equal"), names(options))]
  effective$ordered <- lavaan::lavNames(fit, "ov.ord")
  boot <- list(enabled = identical(options$se, "bootstrap"), status = "not_requested")
  if (boot$enabled) {
    draws <- lavaan::lavInspect(fit, "boot")
    failed <- unique(as.integer(attr(draws, "error.idx")))
    nonfinite <- which(!apply(is.finite(draws), 1L, all))
    failed <- sort(unique(c(failed, nonfinite)))
    inadmissible <- unique(as.integer(attr(draws, "nonadmissible")))
    boot <- list(enabled = TRUE, status = if (length(failed) || length(inadmissible)) "available_with_warnings" else "available",
      requested = if (is.list(options$bootstrap)) options$bootstrap$R else options$bootstrap,
      attempted = nrow(draws), successful = nrow(draws) - length(failed), failed = length(failed),
      failed_indices = failed, inadmissible = length(inadmissible), inadmissible_indices = inadmissible,
      admissible_successful = nrow(draws) - length(union(failed, inadmissible)),
      nonadmissible_policy = "lavaan retains finite nonadmissible draws for SE and CI; error draws are excluded",
      lavaan_seed = attr(draws, "seed"))
  }
  rows <- lavaan::lavInspect(fit, "case.idx")
  if (!is.list(rows)) rows <- list(rows)
  groups <- lavaan::lavInspect(fit, "group.label")
  if (!length(groups)) groups <- "All"
  names(rows) <- groups
  included <- sort(unique(as.integer(unlist(rows, use.names = FALSE))))
  cases <- list(source_n = source_n, group_order = groups,
    groups = lapply(seq_along(rows), function(i) list(group = groups[[i]], source_rows = as.integer(rows[[i]]))),
    included_source_rows = included,
    excluded_source_rows = if (is.null(source_n)) integer() else setdiff(seq_len(source_n), included),
    nobs = as.numeric(lavaan::lavInspect(fit, "nobs")),
    norig = as.numeric(lavaan::lavInspect(fit, "norig")),
    missing = options$missing, fixed_x = options$fixed.x,
    coverage = lavaan::lavInspect(fit, "coverage"))
  list(fit_status = fit_status, inference = list(requested = requested, effective = effective), bootstrap = boot, case_selection = cases)
}
