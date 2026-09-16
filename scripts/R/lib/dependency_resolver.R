# SPDX-License-Identifier: Apache-2.0
# Setup diagnostics deliberately use only base/recommended R. No project state.
nlss_dependency_operations <- function() {
  c("anova", "assumptions", "calc", "check_integrity", "correlations", "crosstabs",
    "data_explorer", "data_transform", "descriptive_stats", "efa", "frequencies",
    "impute", "init_workspace", "metaskill_runner", "mi_regression", "missings",
    "mixed_models", "nonparametric", "plot", "power", "project_create",
    "project_inspect", "project_report", "reconstruct_reports", "regression",
    "reliability", "replay_run", "research_academia", "scale", "sem", "t_test",
    "dependency_resolver", "install_nlss", "run_nlss")
}

nlss_dependency_entrypoint <- function() {
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!length(arg)) return(NULL)
  name <- tools::file_path_sans_ext(basename(sub("^--file=", "", arg[[1]])))
  if (name %in% nlss_dependency_operations()) name else NULL
}

nlss_dependency_library <- function() {
  selected <- Sys.getenv("NLSS_R_LIBRARY", "")
  if (!nzchar(selected)) selected <- strsplit(Sys.getenv("R_LIBS_USER"), .Platform$path.sep, fixed = TRUE)[[1]][1]
  if (is.na(selected) || !nzchar(selected) || grepl("%", selected, fixed = TRUE))
    stop("Set NLSS_R_LIBRARY to a user R library outside the installation and research projects.")
  normalizePath(path.expand(selected), winslash = "/", mustWork = FALSE)
}

nlss_dependency_activate_library <- function() {
  library <- nlss_dependency_library()
  if (dir.exists(library)) .libPaths(unique(c(library, .libPaths())))
  invisible(library)
}

# Only the small setup response needs a base-R JSON encoder. Scientific records
# continue to use their existing jsonlite serializer and schema.
nlss_dependency_json <- function(x) {
  quote <- function(s) {
    chars <- utf8ToInt(enc2utf8(as.character(s)))
    paste0('"', paste(vapply(chars, function(n) {
      if (n == 34L) '\\"' else if (n == 92L) '\\\\' else
        if (n < 32L) sprintf("\\u%04x", n) else intToUtf8(n)
    }, character(1)), collapse = ""), '"')
  }
  if (is.null(x)) return("null")
  if (is.list(x) || length(x) != 1L) {
    parts <- vapply(as.list(x), nlss_dependency_json, character(1))
    if (!is.null(names(x)) && length(x))
      return(paste0("{", paste(paste0(vapply(names(x), quote, character(1)), ":", parts), collapse = ","), "}"))
    return(paste0("[", paste(parts, collapse = ","), "]"))
  }
  if (is.na(x)) return("null")
  if (is.logical(x)) return(if (x) "true" else "false")
  if (is.numeric(x)) return(if (is.finite(x)) as.character(x) else "null")
  quote(x)
}

nlss_dependency_probe <- function(package, exact = NULL, exports = character()) {
  path <- find.package(package, quiet = TRUE)
  if (!length(path)) return(list(status = "absent", version = NULL, detail = "Not installed in the active libraries."))
  version <- tryCatch(as.character(utils::packageVersion(package)), error = function(e) NA_character_)
  if (!is.null(exact) && !identical(version, exact))
    return(list(status = "incompatible", version = version, detail = paste("Exact replay requires", exact)))
  loaded <- tryCatch({ loadNamespace(package); TRUE }, error = function(e) conditionMessage(e))
  if (!isTRUE(loaded)) return(list(status = "unloadable", version = version, detail = loaded))
  missing <- setdiff(exports, getNamespaceExports(package))
  if (length(missing)) return(list(status = "incompatible", version = version,
    detail = paste("Required API not exported:", paste(missing, collapse = ", "))))
  list(status = "ready", version = version, detail = NULL)
}

nlss_dependency_requirements <- function(operation, opts = list(), foundation = FALSE) {
  operation <- gsub("-", "_", operation, fixed = TRUE)
  if (!operation %in% nlss_dependency_operations()) stop("Unknown dependency operation: ", operation)
  requirements <- list()
  add <- function(packages, reason, exports = character()) for (package in packages) {
    old <- requirements[[package]]
    requirements[[package]] <<- list(package = package,
      reason = paste(unique(c(if (!is.null(old)) old$reason, reason)), collapse = "; "),
      exports = unique(c(if (!is.null(old)) old$exports, exports)))
  }
  # Help and the dependency utility itself work without optional R packages.
  if (isTRUE(parse_bool(opts$help)) || operation %in% c("dependency_resolver", "install_nlss", "run_nlss")) return(requirements)
  add("jsonlite", "structured input/output")
  if (!operation %in% c("check_integrity", "reconstruct_reports")) add("yaml", "configuration")
  if (operation != "check_integrity") add("digest", "evidence identity")
  if (foundation || operation %in% c("check_integrity", "reconstruct_reports")) return(requirements)
  value <- function(key, path = paste0("modules.", operation, ".", gsub("-", "_", key))) {
    x <- opts[[key]]
    if (is.null(x) || identical(x, "")) get_config_value(path) else x
  }
  present <- function(key) !is.null(opts[[key]]) && nzchar(as.character(opts[[key]]))
  active <- function(x) length(x) == 1L && !is.na(x) && !tolower(trimws(as.character(x))) %in% c("", "none", "no", "false")
  data_operations <- c("anova", "assumptions", "correlations", "crosstabs", "data_explorer",
    "data_transform", "descriptive_stats", "efa", "frequencies", "impute", "init_workspace",
    "metaskill_runner", "mi_regression", "missings", "mixed_models", "nonparametric", "plot",
    "regression", "reliability", "scale", "sem", "t_test")
  data_input <- operation %in% data_operations
  if (operation == "power") {
    selectors <- c("csv", "sav", "rds", "rdata", "parquet", "dataset")
    data_input <- any(vapply(selectors, present, logical(1))) ||
      parse_bool(value("estimate-effect")) || (!is.null(opts$planning) && !parse_bool(opts$planning))
    analysis <- tolower(value("analysis"))
    add(if (analysis %in% c("sem", "cfa")) "semPower" else "pwr", "selected power analysis")
  }
  if (operation == "project_create") data_input <- present("source")
  if (data_input) add("arrow", "Parquet working data and preserved input")
  if (present("sav") || (operation == "project_create" && present("source") &&
      tolower(tools::file_ext(opts$source)) == "sav")) add("haven", "SPSS import with labels and user-missings")
  if (operation == "sem") add("lavaan", "SEM estimation")
  if (operation == "efa") {
    add("psych", "factor extraction and diagnostics")
    if (!tolower(value("rotation")) %in% c("none", "no", "unrotated", "varimax", "promax", "cluster"))
      add("GPArotation", "selected factor rotation")
  }
  if (operation == "plot") {
    add("ggplot2", "figure rendering")
    if (tolower(trimws(value("palette"))) == "viridis") add("viridisLite", "selected figure palette")
  }
  if (operation %in% c("anova", "mixed_models")) {
    if (active(value("emmeans")) || active(value("contrasts")) || present("contrast-file"))
      add("emmeans", "selected marginal means or contrasts")
    type <- toupper(gsub("[^A-Za-z0-9]", "", value("type")))
    if (operation == "anova" && !present("within") && type %in% c("II", "III", "2", "3"))
      add("car", "Type II/III between-subject ANOVA")
    if (operation == "mixed_models") {
      add(c("lme4", "reformulas", "performance"), "mixed model fitting and mandatory fit summaries")
      method <- tolower(gsub("[^a-z-]", "", value("df-method")))
      if (!method %in% c("none", "no", "false")) add("lmerTest", "requested denominator degrees of freedom")
      if (method %in% c("kenward-roger", "kenwardroger", "kr")) add("pbkrtest", "Kenward-Roger inference")
      if (method %in% c("none", "no", "false") && !type %in% c("I", "1")) add("car", "Type II/III Wald tests")
    }
  }
  if (operation == "impute") {
    engine <- tolower(trimws(value("engine")))
    # Existing auto semantics are not changed by setup: do not install an engine
    # merely to change which one auto selects. Explicit engines fail closed.
    if (engine == "auto") engine <- if (requireNamespace("mice", quietly = TRUE)) "mice" else
      if (requireNamespace("VIM", quietly = TRUE)) "knn" else "simple"
    if (engine == "mice") add("mice", "selected multiple-imputation engine")
    if (engine %in% c("knn", "k-nn", "k_nn")) add("VIM", "selected k-nearest-neighbour imputation engine")
  }
  if (operation == "mi_regression") add(c("mice", "broom"), "pooled multiple-imputation inference")
  if (operation == "assumptions") {
    family <- tolower(trimws(value("analysis")))
    if (family %in% c("", "auto")) family <- if (any(vapply(c("formula", "random", "fixed"), present, logical(1)))) "mixed_models" else
      if (any(vapply(c("model", "model-file", "paths", "factors", "m", "ordered", "group-equal", "invariance"), present, logical(1)))) "sem" else "base"
    if (family %in% c("mixed_models", "mixed-models", "mixed", "lmm", "lme4")) {
      add(c("lme4", "reformulas"), "mixed diagnostic refit")
      if (parse_bool(value("influence"))) add("influence.ME", "requested influence diagnostic")
      for (key in c("performance", "dharma")) if (parse_bool(value(key, paste0("modules.assumptions.mixed_models.", key))))
        add(if (key == "dharma") "DHARMa" else key, paste("requested", key, "diagnostic"))
    }
    if (family %in% c("sem", "cfa", "path", "mediation", "invariance", "structural", "mi")) {
      add("lavaan", "SEM diagnostic refit")
      if (parse_bool(value("mardia", "modules.assumptions.sem.mardia"))) add("MVN", "requested Mardia diagnostic", exports = "mardia")
    }
  }
  requirements
}

nlss_dependency_check <- function(operation, opts = list(), foundation = FALSE,
                                  probe = nlss_dependency_probe, requirements = NULL) {
  if (is.null(requirements)) requirements <- nlss_dependency_requirements(operation, opts, foundation)
  packages <- lapply(requirements, function(req) c(req[c("package", "reason")],
    probe(req$package, exact = req$exact, exports = req$exports)))
  missing <- Filter(function(x) x$status != "ready", packages)
  list(schema_version = 1L, kind = "dependency_check", status = if (length(missing)) "missing_dependency" else "ready",
    operation = operation, r_version = R.version.string, libraries = as.list(.libPaths()),
    target_library = nlss_dependency_library(), packages = unname(packages), missing = unname(missing),
    remedy = if (length(missing)) "Use dependency_resolver.R to plan installation; obtain approval, install, then retry the original command. Exact replay may require restoration of its recorded environment." else NULL)
}

nlss_dependency_guard <- function(operation, opts = list(), foundation = FALSE, requirements = NULL) {
  result <- nlss_dependency_check(operation, opts, foundation, requirements = requirements)
  if (result$status == "ready") return(invisible(result))
  if (!is.null(nlss_dependency_entrypoint())) {
    cat(nlss_dependency_json(result), "\n", sep = "")
    # Exit before entrypoint-specific handlers that may themselves need jsonlite.
    quit(save = "no", status = 42L)
  }
  stop(structure(list(message = "NLSS missing dependency", call = NULL, diagnostic = result),
    class = c("nlss_missing_dependency", "error", "condition")))
}

nlss_dependency_preflight <- function(operation, opts = list(), foundation = FALSE) {
  nlss_dependency_guard(operation, opts, foundation = TRUE)
  if (!foundation) nlss_dependency_guard(operation, opts)
  invisible(TRUE)
}

# CLI parsing is shared by all entrypoints; defer option-dependent checks until
# interactive answers reach the common input/planning boundary.
nlss_dependency_cli <- function(opts, module = NULL) {
  entry <- nlss_dependency_entrypoint()
  if (is.null(entry) || entry == "dependency_resolver") return(invisible(NULL))
  operation <- if (is.null(module)) entry else module
  if (isTRUE(parse_bool(opts$help))) return(invisible(NULL))
  nlss_dependency_preflight(operation, opts, foundation = TRUE)
  # A few existing entrypoints use presence rather than value for --interactive.
  # Conservatively defer whenever supplied; the resolved boundary always checks.
  interactive <- !is.null(opts$interactive)
  if (operation %in% c("init_workspace", "metaskill_runner"))
    interactive <- interactive || isTRUE(get_config_value("defaults.interactive"))
  if (!interactive || operation %in% c("calc", "research_academia")) nlss_dependency_preflight(operation, opts)
  invisible(NULL)
}

nlss_dependency_request <- function(operation, opts = list()) {
  initial <- nlss_dependency_check(operation, opts, foundation = TRUE)
  if (initial$status != "ready") return(initial)
  nlss_dependency_check(operation, opts)
}

nlss_dependency_install_library <- function(library, installation, project = NULL) {
  library <- normalizePath(path.expand(library), winslash = "/", mustWork = FALSE)
  if (!grepl("^(/|[A-Za-z]:/)", library)) stop("Installation library must be an absolute path.")
  # Resolve existing ancestors too, so a symlink cannot hide an installation or
  # project target. No directory or config file is created during planning.
  ancestor <- library
  suffix <- character()
  while (!file.exists(ancestor) && !dir.exists(ancestor)) {
    suffix <- c(basename(ancestor), suffix)
    ancestor <- dirname(ancestor)
  }
  ancestor <- normalizePath(ancestor, winslash = "/", mustWork = TRUE)
  library <- do.call(file.path, as.list(c(ancestor, suffix)))
  within <- function(path, root) identical(path, root) || startsWith(path, paste0(root, "/"))
  forbidden <- c(installation, project, getwd(), R.home(), .Library.site)
  forbidden <- vapply(forbidden[nzchar(forbidden)], normalizePath, character(1), winslash = "/", mustWork = FALSE)
  if (any(vapply(forbidden, function(root) within(library, root), logical(1))))
    stop("Choose a user R library outside the installation, current working folder, projects and system libraries.")
  if (grepl("/plugins/cache(/|$)", library)) stop("Package libraries must be outside plugin caches.")
  cursor <- ancestor
  repeat {
    if (file.exists(file.path(cursor, "nlss-workspace.yml"))) stop("Package libraries must be outside research projects.")
    if (identical(dirname(cursor), cursor)) break
    cursor <- dirname(cursor)
  }
  if (!dir.exists(ancestor) || file.access(ancestor, 2L) != 0L) stop("Target library or its existing parent is not writable.")
  library
}

nlss_dependency_install_plan <- function(check, repository, library, type = .Platform$pkgType,
                                         available = NULL) {
  if (!type %in% c("source", .Platform$pkgType)) stop("Choose source or this R platform's binary package type; no automatic source/binary fallback.")
  if (length(repository) != 1L || !grepl("^(https://|file://)", repository))
    stop("Choose one explicit HTTPS or local file repository.")
  packages <- vapply(check$missing, `[[`, character(1), "package")
  if (!length(packages)) return(list(status = "ready", operation = check$operation, packages = list(), approval = ""))
  if (identical(check$operation, "replay_run")) stop("Restore the recorded replay environment explicitly; this installer does not choose historical versions.")
  if (is.null(available)) available <- withCallingHandlers(
    utils::available.packages(repos = repository, type = type, fields = "SystemRequirements"),
    warning = function(w) stop(conditionMessage(w), call. = FALSE))
  if (any(!packages %in% rownames(available))) stop("Required packages unavailable in the selected repository for this R/platform: ",
    paste(setdiff(packages, rownames(available)), collapse = ", "))
  # Use R's own install.packages dependency planner (including version bounds),
  # not a second dependency solver. This is the single non-exported R adapter;
  # fail closed if a future R removes/changes its interface.
  resolver <- get0("getDependencies", envir = asNamespace("utils"), inherits = FALSE)
  needed_formals <- c("pkgs", "dependencies", "available", "lib", "binary")
  if (!is.function(resolver) || !all(needed_formals %in% names(formals(resolver))))
    stop("This R version does not expose the tested dependency planner; no installation attempted.")
  selected <- withCallingHandlers(resolver(packages,
    dependencies = if (type == "source") c("Depends", "Imports", "LinkingTo") else c("Depends", "Imports"),
    available = available, lib = library, binary = type != "source"),
    warning = function(w) stop(conditionMessage(w), call. = FALSE))
  rows <- lapply(sort(unique(selected)), function(package) {
    old <- tryCatch(as.character(utils::packageVersion(package, lib.loc = unique(c(library, .libPaths())))), error = function(e) NULL)
    list(package = package, version = unname(available[package, "Version"]), installed = old,
      action = if (is.null(old)) "install" else "replace",
      system_requirements = if ("SystemRequirements" %in% colnames(available)) unname(available[package, "SystemRequirements"]) else NULL)
  })
  list(status = "approval_required", operation = check$operation, repository = repository,
    library = library, type = type, packages = rows,
    approval = paste(vapply(rows, function(x) paste0(x$package, "@", x$version), character(1)), collapse = ","),
    available = available)
}

nlss_dependency_fresh_check <- function(requirements, library, resolver_path) {
  input <- tempfile("nlss-dependency-check-", fileext = ".rds")
  output <- tempfile("nlss-dependency-result-", fileext = ".rds")
  on.exit(unlink(c(input, output)), add = TRUE)
  saveRDS(list(requirements = requirements, libraries = unique(c(library, .libPaths()))), input)
  expression <- paste0("a <- commandArgs(TRUE); source(a[1]); x <- readRDS(a[2]); .libPaths(x$libraries); ",
    "result <- lapply(x$requirements, function(r) c(r[c('package','reason')], ",
    "nlss_dependency_probe(r$package, exact=r$exact, exports=r$exports))); saveRDS(result,a[3])")
  code <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", "-e", shQuote(expression),
    shQuote(resolver_path), shQuote(input), shQuote(output)))
  if (code != 0L || !file.exists(output)) stop("Fresh R dependency verification failed.")
  readRDS(output)
}

nlss_dependency_install <- function(plan, approval, requirements, resolver_path) {
  if (identical(plan$status, "ready")) return(plan)
  if (is.null(approval) || !nzchar(approval) || !identical(approval, plan$approval)) {
    result <- plan; result$available <- NULL
    result$message <- "No installation: obtain approval for this exact package/version set, repository and library."
    return(result)
  }
  plan$library <- nlss_dependency_install_library(plan$library,
    normalizePath(file.path(dirname(resolver_path), "../../.."), winslash = "/", mustWork = TRUE))
  if (!dir.exists(plan$library) && !dir.create(plan$library, recursive = TRUE)) stop("Could not create the approved user library.")
  if (file.access(plan$library, 2L) != 0L) stop("Target library is not writable.")
  # The fresh installer has no analysis namespaces loaded (important on Windows).
  # The approved explicit package list and metadata constrain installation:
  # dependencies=FALSE prevents a fresh resolver from expanding approval.
  input <- tempfile("nlss-install-", fileext = ".rds")
  on.exit(unlink(input), add = TRUE)
  saveRDS(list(plan = plan, libraries = unique(c(plan$library, .libPaths()))), input)
  expression <- paste0("x <- readRDS(commandArgs(TRUE)[1]); p <- x$plan; .libPaths(x$libraries); ",
    "Sys.setenv(R_LIBS=paste(.libPaths(),collapse=.Platform$path.sep)); ",
    "options(install.packages.check.source='no'); utils::install.packages(",
    "vapply(p$packages, `[[`, '', 'package'), lib=p$library, repos=p$repository, ",
    "type=p$type, available=p$available, dependencies=FALSE, Ncpus=1L)")
  code <- system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", "-e", shQuote(expression), shQuote(input)))
  # install.packages can return normally after a failed build: verify ALL approved
  # versions as well as the operation's requirements in a fresh process.
  approved <- lapply(plan$packages, function(x) list(package = x$package, exact = x$version,
    reason = "approved installation", exports = character()))
  packages <- nlss_dependency_fresh_check(c(approved, requirements), plan$library, resolver_path)
  ok <- code == 0L && all(vapply(packages, function(x) x$status == "ready", logical(1)))
  list(status = if (ok) "installed" else "installation_failed", operation = plan$operation,
    library = plan$library, repository = plan$repository, packages = unname(packages),
    message = if (ok) "Fresh R load check passed. Retry the original request with the same library selection." else
      "Do not run the analysis. Inspect install diagnostics; system changes or a wider package plan require new approval. Some approved packages may already have installed.")
}
