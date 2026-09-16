# SPDX-License-Identifier: Apache-2.0
#!/usr/bin/env Rscript
# Independent, seeded authoritative-package numerical checks; no NLSS generator.
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[[1L]]
script_path <- normalizePath(sub("^--file=", "", script_arg), winslash = "/", mustWork = TRUE)
repo <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo, "scripts", "R", "lib", "mi_pool.R"))
nlss_mi_require()
cfg <- yaml::read_yaml(file.path(repo, "tests", "tests.yml"))$tests
root <- Sys.getenv("NLSS_TEST_ROOT", "")
if (!nzchar(root)) root <- file.path(repo, cfg$output_dir, paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-phase2-mi"))
dir.create(root, recursive = TRUE, showWarnings = FALSE)
work <- tempfile("mi-pool-", tmpdir = normalizePath(root, winslash = "/"))
dir.create(work)
checks <- character()
check <- function(value, message) {
  if (!isTRUE(value)) stop("FAIL: ", message, call. = FALSE)
  checks <<- c(checks, paste("PASS", message))
  cat(tail(checks, 1L), "\n")
}
fails <- function(expr, pattern, message) {
  error <- tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
  check(!is.null(error) && grepl(pattern, error, ignore.case = TRUE), paste0(message, if (is.null(error)) "" else paste0(" [", error, "]")))
}
near <- function(actual, expected, message) {
  check(isTRUE(all.equal(actual, expected, tolerance = 1e-11, check.attributes = FALSE)), message)
}
json <- function(x, path) jsonlite::write_json(x, path, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
sha <- function(path) digest::digest(file = path, algo = "sha256")

set.seed(241022)
n <- 240L
original <- data.frame(x = rnorm(n), z = rnorm(n), group = factor(sample(c("Control", "Treatment", "Other"), n, TRUE)),
                       ordered = ordered(sample(c("low", "mid", "high"), n, TRUE), levels = c("low", "mid", "high")))
original$y <- 0.8 + 0.7 * original$x - 0.3 * original$z + 0.5 * (original$group == "Treatment") + rnorm(n)
original$binary <- rbinom(n, 1L, plogis(-0.3 + 0.45 * original$x + 0.25 * original$z))
original$binary_factor <- factor(original$binary, levels = c(0, 1), labels = c("no", "yes"))
original$count <- rpois(n, exp(0.1 + 0.22 * original$x - 0.12 * original$z))
original$x[seq(3, 180, by = 7)] <- NA_real_
original$z[seq(4, 190, by = 9)] <- NA_real_
original$group[seq(5, 170, by = 15)] <- NA
# Avoid redundant response dummies in the imputation predictor matrix.
predictors <- mice::make.predictorMatrix(original)
predictors[, c("binary", "binary_factor")] <- 0L
imp <- mice::mice(original, m = 4, maxit = 3, seed = 73091, predictorMatrix = predictors, printFlag = FALSE)

create_artifact <- function(mids, label, data = mids$data) {
  project <- file.path(work, label)
  dir.create(project, recursive = TRUE)
  dataset <- file.path(project, "sample")
  version <- file.path(dataset, "versions", "v-test")
  dir.create(version, recursive = TRUE)
  arrow::write_parquet(data, file.path(version, "data.parquet"))
  json(list(schema_version = 1L, columns = as.list(names(data))), file.path(version, "dictionary.json"))
  reference <- list(schema_version = 1L, dataset_id = "ds-test", version_id = "v-test",
    data_sha256 = sha(file.path(version, "data.parquet")), dictionary_sha256 = sha(file.path(version, "dictionary.json")),
    snapshot_path = "sample/versions/v-test/data.parquet", dictionary_path = "sample/versions/v-test/dictionary.json",
    source_sha256 = NULL, import_version_id = NULL, origin = "legacy_or_external_working_copy")
  json(reference, file.path(version, "provenance.json"))
  temporary <- file.path(project, "staged.rds")
  saveRDS(mids, temporary, version = 3L)
  hash <- sha(temporary)
  id <- paste0("mice-", hash)
  artifact <- file.path(dataset, "imputations", id)
  dir.create(artifact, recursive = TRUE)
  file.rename(temporary, file.path(artifact, "mids.rds"))
  metadata <- list(schema_version = 1L, artifact_id = id, path = paste0("sample/imputations/", id, "/mids.rds"),
    sha256 = hash, object_class = "mids", m = mids$m, iterations = mids$iteration,
    mice_version = as.character(mids$version), r_version = R.version.string,
    seed = if (is.na(mids$seed)) NULL else mids$seed, dataset = reference,
    completion_mode = "single_completion", completion_aggregation = "mean_numeric_mode_categorical", inference_pooled = FALSE)
  json(metadata, file.path(artifact, "metadata.json"))
  list(path = artifact, root = project, metadata = metadata, version = version)
}
artifact <- create_artifact(imp, "valid")
fields <- c("estimate", "std.error", "statistic", "df", "p.value", "conf.low", "conf.high", "m", "riv", "lambda", "fmi", "ubar", "b", "t", "dfcom")
cases <- list(
  list(name = "linear interactions", formula = "y ~ x * group + z", family = "gaussian", link = "identity", level = .95),
  list(name = "linear no intercept", formula = "y ~ 0 + x + group", family = "gaussian", link = "identity", level = .9),
  list(name = "linear subtraction", formula = "y ~ (x + z) * group - z:group", family = "gaussian", link = "identity", level = .99),
  list(name = "linear ordered factor", formula = "y ~ x + ordered", family = "gaussian", link = "identity", level = .95),
  list(name = "logistic binary", formula = "binary ~ x + z + group", family = "binomial", link = "logit", level = .95),
  list(name = "probit factor", formula = "binary_factor ~ x + group", family = "binomial", link = "probit", level = .9),
  list(name = "cloglog binary", formula = "binary ~ x + z", family = "binomial", link = "cloglog", level = .95),
  list(name = "Poisson counts", formula = "count ~ x + z + group", family = "poisson", link = "log", level = .95),
  list(name = "linear intercept only", formula = "y ~ 1", family = "gaussian", link = "identity", level = .95)
)
completed <- mice::complete(imp, action = "all")
for (case in cases) {
  # Expected fits use direct stats calls, never NLSS helpers or transformed output.
  expected_fits <- lapply(completed, function(frame) {
    formula <- stats::as.formula(case$formula)
    if (case$family == "gaussian") stats::lm(formula, data = frame) else {
      fam <- if (case$family == "binomial") stats::binomial(link = case$link) else stats::poisson(link = case$link)
      stats::glm(formula, data = frame, family = fam)
    }
  })
  expected_pool <- mice::pool(expected_fits, rule = "rubin1987")
  expected <- as.data.frame(summary(expected_pool, type = "all", conf.int = TRUE, conf.level = case$level))
  actual <- nlss_fit_mi(artifact$path, case$formula, family = case$family, link = case$link, conf_level = case$level, workspace_root = artifact$root)
  check(identical(actual$coefficients$term, as.character(expected$term)), paste(case$name, "terms match direct mice"))
  for (field in fields) near(actual$coefficients[[field]], expected[[field]], paste(case$name, field, "matches direct mice"))
  check(length(actual$fits) == imp$m && all(vapply(actual$fits, inherits, logical(1), what = "lm")), paste(case$name, "preserves every fit"))
  check(all(vapply(actual$diagnostics, function(x) x$n == n && x$converged, logical(1))), paste(case$name, "all model diagnostics recorded"))
  if (case$name == "probit factor") check(identical(actual$metadata$response_coding$success, "yes") && identical(actual$metadata$response_coding$failure, "no"), "binomial factor event coding is explicit")
  if (case$name == "linear interactions") baseline <- actual
}
old_contrasts <- getOption("contrasts")
options(contrasts = c("contr.sum", "contr.helmert"))
frozen <- nlss_fit_mi(artifact$path, "y ~ x * group + z")
options(contrasts = old_contrasts)
near(frozen$coefficients[fields], baseline$coefficients[fields], "ambient contrasts cannot alter resolved model")
check(identical(frozen$artifact$sha256, artifact$metadata$sha256) && frozen$metadata$inference_pooled &&
      identical(frozen$metadata$pooling_rule, "rubin1987"), "exact artifact and inference pooling recorded")
check(identical(frozen$metadata$imputation_logged_events, imp$loggedEvents), "imputation logged events are exposed without declaring chain convergence")

for (formula in c("y ~ log(x)", "y ~ I(x^2)", "y ~ .", "y ~ x; system('false')", "y ~ base::system('false')", "log(y) ~ x", "y ~ x + unknown", "y ~ x + offset(z)", "y ~ x | group", "y ~ y + x", "y ~ x^2", "y ~ x / group")) {
  fails(nlss_fit_mi(artifact$path, formula), "formula|response|Unknown|Unsupported", paste("reject unsafe or unsupported formula", formula))
}
fails(nlss_fit_mi(artifact$path, "y ~ x", family = "quasipoisson"), "families", "unsupported family never falls back")
fails(nlss_fit_mi(artifact$path, "y ~ x", family = "gaussian", link = "log"), "link", "unsupported family-link combination rejected")
fails(nlss_fit_mi(artifact$path, "y ~ x", conf_level = 1), "confidence", "invalid confidence level rejected")
fails(nlss_fit_mi(artifact$path, "y ~ x", maxit = 1.5), "positive integer", "fractional GLM iteration limit rejected")
fails(nlss_fit_mi(artifact$path, "binary ~ x + z", family = "binomial", maxit = 1), "converge|warning", "nonconvergent GLM cannot be pooled")
fails(nlss_fit_mi(artifact$path, "y ~ x", family = "binomial"), "response", "continuous response cannot be silently treated as binomial")
fails(nlss_fit_mi(artifact$path, "y ~ x", family = "poisson"), "counts", "noninteger response cannot be silently treated as Poisson")
fails(nlss_fit_mi(artifact$path, "group ~ x", family = "binomial"), "two factor", "multilevel binomial response rejected")
fails(nlss_fit_mi(artifact$path, "group ~ x", family = "gaussian"), "numeric", "categorical Gaussian response rejected")

bad <- create_artifact(imp, "metadata-m")
bad$metadata$m <- imp$m + 1L
json(bad$metadata, file.path(bad$path, "metadata.json"))
fails(nlss_fit_mi(bad$path, "y ~ x"), "metadata disagrees", "altered metadata imputation count rejected")
bad <- create_artifact(imp, "metadata-ref")
bad$metadata$dataset$dataset_id <- "ds-other"
json(bad$metadata, file.path(bad$path, "metadata.json"))
fails(nlss_fit_mi(bad$path, "y ~ x"), "provenance", "altered dataset metadata rejected")
bad <- create_artifact(imp, "metadata-seed")
bad$metadata$seed <- 999L
json(bad$metadata, file.path(bad$path, "metadata.json"))
fails(nlss_fit_mi(bad$path, "y ~ x"), "metadata disagrees", "altered seed metadata rejected")
bad <- create_artifact(imp, "artifact-tamper")
con <- file(file.path(bad$path, "mids.rds"), "ab")
writeBin(as.raw(1L), con)
close(con)
fails(nlss_fit_mi(bad$path, "y ~ x"), "SHA-256", "altered mids bytes rejected")
bad <- create_artifact(imp, "dictionary-tamper")
writeLines("{}", file.path(bad$version, "dictionary.json"))
fails(nlss_fit_mi(bad$path, "y ~ x"), "SHA-256", "altered source dictionary rejected")
bad <- create_artifact(imp, "snapshot-tamper")
arrow::write_parquet(transform(original, y = y + 1), file.path(bad$version, "data.parquet"))
fails(nlss_fit_mi(bad$path, "y ~ x"), "SHA-256", "altered source snapshot rejected")
bad <- create_artifact(imp, "other-valid-snapshot", transform(original, y = y + 1))
fails(nlss_fit_mi(bad$path, "y ~ x"), "original values", "valid hashes cannot bind imputations to unrelated original data")
one <- imp
one$m <- 1L
bad <- create_artifact(one, "only-one")
fails(nlss_fit_mi(bad$path, "y ~ x"), "at least two", "single imputation cannot claim pooled MI inference")
bad <- create_artifact(imp, "missing-reference")
bad$metadata$dataset <- NULL
json(bad$metadata, file.path(bad$path, "metadata.json"))
fails(nlss_fit_mi(bad$path, "y ~ x"), "dataset reference", "unreferenced mids artifact rejected")
not_mids <- unclass(imp)
bad <- create_artifact(not_mids, "not-mids")
fails(nlss_fit_mi(bad$path, "y ~ x"), "actual mids", "non-mids RDS cannot become an imputation artifact through metadata")
bad <- create_artifact(imp, "missing-metadata")
file.rename(file.path(bad$path, "metadata.json"), file.path(bad$path, "metadata.saved.json"))
fails(nlss_fit_mi(bad$path, "y ~ x"), "adjacent Phase 1", "artifact without metadata rejected")

# Deterministic invalid fits use as.mids only to construct controlled fixtures.
make_completed <- function(frames) {
  base <- frames[[1L]]
  base$x[1L] <- NA_real_
  long <- do.call(rbind, lapply(seq.int(0L, length(frames)), function(i) {
    data <- if (i == 0L) base else frames[[i]]
    data.frame(.imp = i, .id = seq_len(nrow(data)), data, check.names = FALSE)
  }))
  mice::as.mids(long)
}
complete <- completed[[1L]]
collinear <- complete
collinear$duplicate <- 2 * collinear$z
bad <- create_artifact(make_completed(rep(list(collinear), 3)), "collinear")
fails(nlss_fit_mi(bad$path, "y ~ z + duplicate"), "singular|rank", "rank-deficient fits cannot be pooled")
remaining_na <- imp
remaining_na$imp$x[1L, 1L] <- NA_real_
bad <- create_artifact(remaining_na, "uncompleted-missing")
fails(nlss_fit_mi(bad$path, "y ~ x"), "missing", "unfilled model values cannot silently drop rows")
nonfinite <- imp
nonfinite$imp$x[1L, 1L] <- Inf
bad <- create_artifact(nonfinite, "nonfinite")
fails(nlss_fit_mi(bad$path, "y ~ x"), "non-finite", "nonfinite completed predictors rejected")
separated <- complete
separated$binary <- as.integer(separated$z > 0)
bad <- create_artifact(make_completed(rep(list(separated), 3)), "separation")
fails(nlss_fit_mi(bad$path, "binary ~ z", family = "binomial"), "converge|warning|interior", "separated logistic fit is not silently pooled")
writeLines(checks, file.path(root, "phase2-mi-pool.log"))
json(list(passed = length(checks), failed = 0L, checks = checks), file.path(root, "phase2-mi-pool.json"))
cat("MI pooling checks passed:", length(checks), "\n")
