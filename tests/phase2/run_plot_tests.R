#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0
# Public CLI acceptance. Numerical references use base R or ggplot2 directly;
# no NLSS scientific helper, historical output, or saved plot is an oracle.
args <- commandArgs(TRUE)
script <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])[1]
repo <- normalizePath(file.path(dirname(script), "../.."), winslash = "/")
script <- normalizePath(script, winslash = "/")
if ("--help" %in% args) { cat("Usage: run_plot_tests.R [--root PATH] [--keep N] [--match REGEX]\n"); quit(status = 0) }
if (length(args) %% 2L || (length(args) && any(!args[seq(1L, length(args), 2L)] %in% c("--root", "--keep", "--match")))) stop("Unknown/incomplete runner option")
arg <- function(name, fallback) { i <- which(args == name); if (length(i) > 1L) stop("Repeated runner option"); if (length(i)) args[i + 1L] else fallback }
pattern <- arg("--match", ".*"); invisible(grepl(pattern, "validate regex"))
required <- c("yaml", "jsonlite", "digest", "arrow", "haven", "ggplot2")
for (package in required) if (!requireNamespace(package, quietly = TRUE)) stop("Missing test package: ", package)
absolute <- function(path) normalizePath(if (grepl("^(/|[A-Za-z]:|\\\\)", path)) path else file.path(repo, path), winslash = "/", mustWork = FALSE)
cfg <- yaml::read_yaml(absolute(Sys.getenv("NLSS_TESTS_CONFIG", "tests/tests.yml")), eval.expr = FALSE)$tests
forced <- arg("--root", Sys.getenv("NLSS_TEST_ROOT", ""))
keep <- suppressWarnings(as.integer(arg("--keep", Sys.getenv("NLSS_KEEP_RUNS", as.character(cfg$keep_runs_default)))))
if (length(keep) != 1L || is.na(keep) || keep < 0L) stop("Invalid --keep")
collection <- file.path(if (nzchar(forced)) absolute(forced) else absolute(cfg$output_dir), "phase2-plot")
work <- file.path(collection, paste0("run-", format(Sys.time(), "%Y%m%d%H%M%S"), "-", Sys.getpid()))
dir.create(work, recursive = TRUE)
config <- file.path(work, "config.yml")
reset_config <- function() yaml::write_yaml(list(defaults = list(digits = 7L), modules = list(plot = list(width = 4, height = 3, dpi = 72L))), config)
reset_config(); Sys.setenv(NLSS_CONFIG_PATH = config, OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1")
check <- function(ok, message) if (!isTRUE(ok)) stop(message, call. = FALSE)
read_text <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
read_json <- function(path) jsonlite::fromJSON(path, simplifyVector = TRUE)
raw_json <- function(path) jsonlite::fromJSON(path, simplifyVector = FALSE)
write_json <- function(value, path) jsonlite::write_json(value, path, auto_unbox = TRUE, pretty = TRUE, digits = NA, null = "null", na = "null")
sha <- function(path) digest::digest(file = path, algo = "sha256")
numeric_checks <- 0L
json_numeric_checks <- 0L
near <- function(actual, expected, label, tolerance = 2e-7) {
  actual <- as.numeric(unlist(actual)); expected <- as.numeric(unlist(expected))
  check(length(actual) == length(expected) && length(expected) > 0L, paste(label, "length differs"))
  check(identical(is.na(actual), is.na(expected)), paste(label, "missing mask differs"))
  finite <- is.finite(expected)
  check(identical(is.infinite(actual), is.infinite(expected)), paste(label, "infinite mask differs"))
  check(all(abs(actual[finite] - expected[finite]) <= tolerance * pmax(1, abs(expected[finite]))), paste(label, "differs from independent reference"))
  numeric_checks <<- numeric_checks + length(expected)
}
# The RDS artifact preserves exact R data structures. JSON must independently
# carry the same numerical fields, including ragged boxplot outlier vectors.
json_frame_equivalent <- function(frame, rows, label) {
  before <- numeric_checks
  on.exit(json_numeric_checks <<- json_numeric_checks + numeric_checks - before, add = TRUE)
  if (is.null(frame)) { check(is.null(rows), paste(label, "invented JSON table")); return(invisible(NULL)) }
  check(length(rows) == nrow(frame), paste(label, "JSON row count differs"))
  for (column in names(frame)) {
    values <- frame[[column]]
    cells <- lapply(rows, function(row) row[[column]])
    if (is.list(values)) {
      for (i in seq_along(values)) {
        if (!length(values[[i]])) check(!length(cells[[i]]), paste(label, column, "invented list-cell value"))
        else near(vapply(as.list(cells[[i]]), function(x) if (is.null(x)) NA_real_ else as.numeric(x), numeric(1)),
          values[[i]], paste(label, column, "list-cell JSON parity"), 2e-14)
      }
    } else if (is.numeric(values)) {
      if (length(values)) near(vapply(cells, function(x) if (is.null(x)) NA_real_ else as.numeric(x), numeric(1)),
        # jsonlite's maximum-precision decimal serialization can differ from
        # the exact RDS double in its last significant digits.
        as.numeric(values), paste(label, column, "JSON parity"), 2e-14)
    } else if (is.logical(values)) {
      actual <- vapply(cells, function(x) if (is.null(x)) NA else as.logical(x), logical(1))
      check(identical(unname(actual), unname(values)), paste(label, column, "logical JSON parity"))
    } else {
      actual <- vapply(cells, function(x) if (is.null(x)) NA_character_ else as.character(x), character(1))
      check(identical(unname(actual), unname(as.character(values))), paste(label, column, "category JSON parity"))
    }
  }
}
results <- list()
test <- function(name, code) {
  if (!grepl(pattern, name)) return(invisible(NULL))
  previous <- getwd(); on.exit(setwd(previous)); reset_config(); before <- numeric_checks; json_before <- json_numeric_checks; start <- proc.time()[["elapsed"]]
  error <- tryCatch({ force(code); NULL }, error = conditionMessage)
  results[[length(results) + 1L]] <<- list(test = name, passed = is.null(error), numeric_checks = numeric_checks - before,
    independent_numeric_checks = numeric_checks - before - (json_numeric_checks - json_before), json_numeric_checks = json_numeric_checks - json_before,
    seconds = unname(proc.time()[["elapsed"]] - start), message = if (is.null(error)) "OK" else error)
  cat(if (is.null(error)) "[PASS] " else "[FAIL] ", name, if (is.null(error)) "" else paste0(": ", error), "\n", sep = "")
}
set.seed(10913L)
n <- 54L
sample <- data.frame(x = rep(seq(-2, 2, length.out = 9), 6), y = rnorm(n), z = rnorm(n),
  g = factor(rep(c("B", "A", "C"), each = 18), levels = c("B", "A", "C", "unused")),
  category = factor(rep(c("yes", "no", "maybe"), length.out = n), levels = c("maybe", "no", "yes")))
sample$y <- .5 + .6 * sample$x + .3 * sample$x^2 + sample$y
sample$x[c(3, 20)] <- NA; sample$y[c(8, 41)] <- NA; sample$z[13] <- NA
sample$g[29] <- NA; sample$category[c(5, 32)] <- NA
new_case <- function(name, data = sample, format = "rds") {
  base <- file.path(work, "cases", name); project <- file.path(base, "project"); dir.create(project, recursive = TRUE)
  yaml::write_yaml(list(version = 1L, datasets = list()), file.path(project, "nlss-workspace.yml"))
  input <- file.path(base, paste0("sample.", format))
  switch(format, rds = saveRDS(data, input), csv = write.csv(data, input, row.names = FALSE),
    sav = haven::write_sav(data, input), parquet = arrow::write_parquet(data, input), RData = { survey <- data; save(survey, file = input) })
  list(base = base, project = project, input = input, data = data, format = format,
    directory = file.path(project, if (format == "RData") "survey" else "sample"),
    source = c(paste0("--", tolower(format)), input, if (format == "RData") c("--df", "survey")))
}
tokens <- function(options) unlist(Map(function(key, value) c(paste0("--", key), as.character(value)), names(options), options), use.names = FALSE)
run_cli <- function(context, argv, module = "plot", failure = FALSE) {
  previous <- getwd(); on.exit(setwd(previous)); setwd(context$project)
  log <- tempfile(paste0(module, "-"), context$base, fileext = ".log")
  status <- system2(file.path(R.home("bin"), "Rscript"), c(shQuote(file.path(repo, "scripts/R", paste0(module, ".R"))), shQuote(argv)), stdout = log, stderr = log)
  check(if (failure) status != 0L else status == 0L, paste(module, "unexpected exit", status, read_text(log)))
  log
}
runs <- function(context) {
  paths <- list.files(context$directory, "^request[.]json$", recursive = TRUE, full.names = TRUE, all.files = TRUE)
  dirname(paths[grepl("/runs/[^.][^/]+/request[.]json$", paths)])
}
run <- function(context, options, source = TRUE, module = "plot") {
  before <- runs(context)
  log <- run_cli(context, c(if (source) context$source, if (is.list(options)) tokens(options) else options), module)
  added <- setdiff(runs(context), before); check(length(added) == 1L, "Expected exactly one published run")
  request_path <- file.path(added, "request.json"); result_path <- file.path(added, "result.json")
  req <- read_json(request_path); res <- read_json(result_path); raw <- raw_json(result_path)
  check(res$status == "completed" && isTRUE(req$resolved), "Unresolved or failed run")
  check(req$module == "plot" && res$module == "plot", "Wrong module identity")
  check(req$schema_version == 1L && res$schema_version == 1L, "Dataset schema changed")
  check(req$run_id == basename(added) && identical(req$run_id, res$run_id), "Run identity mismatch")
  check(identical(res$artifacts$request$sha256, sha(request_path)), "Request/result hash mismatch")
  for (artifact in res$artifacts) check(identical(sha(file.path(added, artifact$path)), artifact$sha256), "Published artifact hash mismatch")
  for (template in req$templates) check(identical(sha(file.path(added, template$path)), template$sha256), "Preserved template hash mismatch")
  check(length(req$templates) > 0L && length(req$environment$packages) > 0L && length(req$options) > 0L, "Execution context missing")
  check(identical(sha(file.path(context$project, req$dataset$snapshot_path)), req$dataset$data_sha256), "Snapshot hash mismatch")
  check(identical(sha(file.path(context$project, req$dataset$dictionary_path)), req$dataset$dictionary_sha256), "Dictionary hash mismatch")
  check(file.exists(file.path(added, "plot-data.rds")), "Machine-readable full-precision plotting data absent")
  plots <- readRDS(file.path(added, "plot-data.rds"))
  check(length(plots) > 0L && length(res$results$plots) > 0L, "Plot numerical audit absent")
  for (i in seq_along(plots)) {
    json_frame_equivalent(plots[[i]]$data, raw$results$plots[[i]]$data, "Prepared plotting data")
    json_frame_equivalent(plots[[i]]$summary, raw$results$plots[[i]]$summary, "Plot summary")
    check(length(plots[[i]]$layers) == length(raw$results$plots[[i]]$layers), "JSON graphical layer count differs")
    for (k in seq_along(plots[[i]]$layers)) json_frame_equivalent(plots[[i]]$layers[[k]], raw$results$plots[[i]]$layers[[k]], paste("Graphical layer", k))
  }
  figures <- res$results$figures
  check(is.data.frame(figures) && nrow(figures) == length(plots), "Figure rows absent or differ from plotted artifacts")
  near(figures$figure_number, seq_len(nrow(figures)), "Deterministic run-local figure numbers", 0)
  for (relative in figures$figure_path) {
    check(!grepl("^/|(^|/)[.][.](/|$)", relative), "Figure escapes run directory")
    image <- file.path(added, relative)
    check(file.exists(image) && file.info(image)$size > 300, "Empty or absent image artifact")
    check(any(vapply(res$artifacts, function(a) identical(a$path, relative), logical(1))), "Figure image is not hash-recorded")
  }
  markdown <- read_text(file.path(added, "output.md"))
  for (relative in figures$figure_path) check(grepl(relative, markdown, fixed = TRUE), "Run-local Markdown image link missing")
  check(!dir.exists(file.path(context$directory, ".analysis-lock")) && !dir.exists(file.path(context$project, ".publication-lock")), "Completed run retained lock")
  list(path = added, request_path = request_path, request = req, result = res, plots = plots, figures = figures, markdown = markdown, log = log)
}
snapshot <- function(paths) { paths <- paths[file.exists(paths) & !dir.exists(paths)]; setNames(vapply(paths, sha, character(1)), paths) }
tree <- function(path) snapshot(sort(list.files(path, recursive = TRUE, full.names = TRUE, all.files = TRUE)))
protected <- function(context) c(file.path(context$project, "nlss-workspace.yml"), file.path(context$directory, c("report_canonical.md", "analysis_log.jsonl", paste0(basename(context$directory), ".parquet"))),
  list.files(file.path(context$directory, "plots"), full.names = TRUE, recursive = TRUE, all.files = TRUE))
failed <- function(context, options, module = "plot", source = TRUE) {
  initialized <- file.exists(file.path(context$directory, "import.json"))
  before <- snapshot(protected(context)); previous <- runs(context)
  log <- run_cli(context, c(if (source) context$source, if (is.list(options)) tokens(options) else options), module, failure = TRUE)
  # The independently authorized initial import can precede an analysis error;
  # existing initialized workspaces must retain all projection/data bytes.
  if (initialized) check(identical(before, snapshot(protected(context))), "Failed request changed protected data/report/log/manifest/images")
  for (path in setdiff(runs(context), previous)) {
    res <- read_json(file.path(path, "result.json"))
    check(res$status == "failed" && !file.exists(file.path(path, "output.md")), "Failed request published ordinary output")
    run_cli(context, c("--request", file.path(path, "request.json")), "replay_run", failure = TRUE)
  }
  invisible(log)
}
replay <- function(context, original) {
  before <- tree(original$path); working <- snapshot(file.path(context$directory, paste0(basename(context$directory), ".parquet")))
  again <- run(context, c("--request", original$request_path), source = FALSE, module = "replay_run")
  check(identical(again$plots, original$plots), "Replay raw plotting data differ")
  check(identical(again$markdown, original$markdown), "Replay deterministic Markdown differs")
  check(identical(again$request$replay_of, original$request$run_id), "Missing replay origin")
  check(identical(before, tree(original$path)) && identical(working, snapshot(names(working))), "Replay modified original run or working data")
  again
}
selection <- function(bundle, rows, figure = 1L, total = nrow(sample)) {
  actual <- bundle$plots[[figure]]$cases
  near(actual$source_rows, rows, "Included source row identities", 0)
  excluded <- setdiff(seq_len(total), rows)
  if (length(excluded)) near(actual$excluded_rows, excluded, "Excluded source row identities", 0)
  else check(!length(actual$excluded_rows), "Invented excluded source rows")
  near(bundle$figures$n[figure], length(rows), "Figure retained N", 0)
  near(bundle$figures$missing_n[figure], length(excluded), "Figure missing/excluded N", 0)
}
layer <- function(bundle, number = 1L, figure = 1L) bundle$plots[[figure]]$layers[[number]]
layer_near <- function(actual, reference, fields, label) {
  check(nrow(actual) == nrow(reference), paste(label, "layer row count differs"))
  for (field in fields) {
    check(field %in% names(actual) && field %in% names(reference), paste(label, "missing numeric field", field))
    near(actual[[field]], reference[[field]], paste(label, field))
  }
}
build <- function(plot) suppressMessages(suppressWarnings(ggplot2::ggplot_build(plot)$data))
# Reference input is reconstructed solely from the fixture, not module metadata.
selected <- function(vars, group = FALSE) {
  use <- complete.cases(sample[unique(c(vars, if (group) "g"))]); value <- sample[use, , drop = FALSE]
  if (group) value$g <- droplevels(value$g)
  list(data = value, rows = which(use))
}

for (binning in c("bins", "binwidth")) for (grouped in c(FALSE, TRUE)) test(paste0("plot_histogram_", binning, if (grouped) "_grouped" else "_smoke"), {
  data <- selected("x", grouped); options <- list(type = "histogram", vars = "x")
  options[[binning]] <- if (binning == "bins") 9L else .4
  if (grouped) options$group <- "g"
  actual <- run(new_case(paste0("hist-", binning, "-", grouped)), options)
  ref <- ggplot2::ggplot(data$data, if (grouped) ggplot2::aes(x, fill = g) else ggplot2::aes(x))
  ref <- ref + if (binning == "bins") ggplot2::geom_histogram(bins = 9, position = "identity") else ggplot2::geom_histogram(binwidth = .4, position = "identity")
  layer_near(layer(actual), build(ref)[[1]], c("x", "xmin", "xmax", "count", "density", "ncount", "ndensity", "y", "group"), "Histogram")
  selection(actual, data$rows)
  near(sum(layer(actual)$count), length(data$rows), "Histogram frequency sum", 0)
  check(grepl("Plot Type", actual$markdown, fixed = TRUE) && grepl("Output Format", actual$markdown, fixed = TRUE), "Plot/output-format flags missing")
  check(!grepl("Sum of squares type|Input format", actual$markdown, ignore.case = TRUE), "Unrelated model/input flags leaked into figure report")
})
for (bandwidth in c("auto", "numeric")) test(paste0("plot_density_", bandwidth, "_grouped"), {
  data <- selected("y", TRUE); options <- list(type = "density", vars = "y", group = "g")
  if (bandwidth == "numeric") options$bw <- .35
  actual <- run(new_case(paste0("density-", bandwidth)), options)
  ref <- ggplot2::ggplot(data$data, ggplot2::aes(y, fill = g)) + if (bandwidth == "numeric") ggplot2::geom_density(bw = .35) else ggplot2::geom_density()
  layer_near(layer(actual), build(ref)[[1]], c("x", "density", "scaled", "ndensity", "count", "n", "y", "group"), "Density")
  selection(actual, data$rows)
})
for (kind in c("box", "violin")) test(paste0("plot_", kind, "_quantiles", if (kind == "violin") "_smoke" else ""), {
  data <- selected("y", TRUE); actual <- run(new_case(kind), list(type = kind, vars = "y", group = "g"))
  ref <- ggplot2::ggplot(data$data, ggplot2::aes(g, y, fill = g))
  if (kind == "box") ref <- ref + ggplot2::geom_boxplot()
  else ref <- ref + ggplot2::geom_violin(trim = FALSE) + ggplot2::geom_boxplot(width = .2)
  refs <- build(ref); box_index <- if (kind == "box") 1L else 2L
  layer_near(layer(actual, box_index), refs[[box_index]], c("ymin", "lower", "middle", "upper", "ymax", "notchupper", "notchlower", "group"), "Box quantiles and whiskers")
  if (kind == "violin") layer_near(layer(actual), refs[[1]], c("density", "scaled", "ndensity", "count", "n", "y", "violinwidth", "group"), "Violin density")
  for (i in seq_len(nrow(refs[[box_index]]))) near(c(layer(actual, box_index)$outliers[[i]], 0), c(refs[[box_index]]$outliers[[i]], 0), "Box outliers")
  selection(actual, data$rows)
})
for (method in c("none", "lm", "loess")) test(paste0("plot_scatter_", method, if (method == "lm") "_smoke" else ""), {
  data <- selected(c("x", "y"), TRUE); options <- list(type = "scatter", x = "x", y = "y", group = "g", smooth = method, span = .9, se = TRUE)
  context <- new_case(paste0("scatter-", method)); actual <- run(context, options)
  ref <- ggplot2::ggplot(data$data, ggplot2::aes(x, y, colour = g)) + ggplot2::geom_point()
  if (method != "none") ref <- ref + ggplot2::geom_smooth(method = method, se = TRUE, span = .9)
  refs <- build(ref); layer_near(layer(actual), refs[[1]], c("x", "y", "group"), "Scatter observations")
  if (method != "none") layer_near(layer(actual, 2), refs[[2]], c("x", "y", "ymin", "ymax", "se", "group"), paste(method, "smooth and CI"))
  selection(actual, data$rows)
  if (method == "lm") replay(context, actual)
})
test("plot_scatter_implicit_se_replay", {
  context <- new_case("implicit-se"); actual <- run(context, c("--type", "scatter", "--x", "x", "--y", "y", "--smooth", "lm", "--se"))
  check(isTRUE(actual$request$options$se), "Implicit Boolean was not resolved")
  replay(context, actual)
})
test("plot_smoothing_without_confidence_band", {
  context <- new_case("smooth-no-ci"); data <- selected(c("x", "y"))
  for (method in c("lm", "loess")) {
    actual <- run(context, list(type = "scatter", x = "x", y = "y", smooth = method, se = FALSE, span = .8))
    ref <- ggplot2::ggplot(data$data, ggplot2::aes(x, y)) + ggplot2::geom_point() + ggplot2::geom_smooth(method = method, se = FALSE, span = .8)
    layer_near(layer(actual, 2), build(ref)[[2]], c("x", "y"), paste(method, "without interval"))
    check(!any(c("ymin", "ymax", "se") %in% names(layer(actual, 2))), "Disabled confidence band was still calculated")
    check(identical(actual$request$options$se, FALSE), "Explicit false smoother flag not resolved")
  }
})
for (summary in c("none", "mean", "median")) test(paste0("plot_line_", summary, "_grouped"), {
  data <- selected(c("x", "y"), TRUE); actual <- run(new_case(paste0("line-", summary)), list(type = "line", x = "x", y = "y", group = "g", summary = summary))
  ref_data <- data$data
  if (summary != "none") ref_data <- aggregate(y ~ x + g, ref_data, if (summary == "mean") mean else median)
  ref <- ggplot2::ggplot(ref_data, ggplot2::aes(x, y, colour = g)) + ggplot2::geom_line()
  layer_near(layer(actual), build(ref)[[1]], c("x", "y", "group"), paste("Line", summary))
  selection(actual, data$rows)
})
for (grouped in c(FALSE, TRUE)) test(paste0("plot_qq_", if (grouped) "grouped_smoke" else "ungrouped"), {
  data <- selected("y", grouped); options <- list(type = "qq", vars = "y")
  if (grouped) options$group <- "g"
  actual <- run(new_case(paste0("qq-", grouped)), options)
  ref <- ggplot2::ggplot(data$data, if (grouped) ggplot2::aes(sample = y, colour = g) else ggplot2::aes(sample = y)) + ggplot2::stat_qq() + ggplot2::stat_qq_line()
  refs <- build(ref)
  layer_near(layer(actual), refs[[1]], c("sample", "theoretical", "x", "y", "group"), "QQ quantiles")
  layer_near(layer(actual, 2), refs[[2]], c("x", "y", "group"), "QQ reference line")
  selection(actual, data$rows)
})
test("plot_heatmap_listwise_correlations_smoke", {
  vars <- c("x", "y", "z"); data <- selected(vars); actual <- run(new_case("heatmap"), list(type = "corr-heatmap", vars = paste(vars, collapse = ",")))
  corr <- cor(data$data[vars]); summary <- actual$plots[[1]]$summary
  near(summary$r, as.numeric(corr), "Heatmap listwise correlations")
  selection(actual, data$rows)
  check(grepl("listwise", jsonlite::toJSON(actual$request$design), ignore.case = TRUE), "Heatmap effective listwise cases undisclosed")
  check(any(abs(corr - cor(sample[vars], use = "pairwise.complete.obs")) > 1e-5), "Fixture does not distinguish listwise and pairwise")
})
for (stat in c("count", "percent")) for (position in c("dodge", "stack", "fill")) test(paste0("plot_bar_", stat, "_", position, if (stat == "percent" && position == "dodge") "_smoke" else ""), {
  rows <- which(complete.cases(sample[c("category", "g")]))
  data <- sample[rows, ]; options <- list(type = "bar", vars = "category", group = "g", stat = stat, position = position, `percent-base` = "group")
  actual <- run(new_case(paste0("bar-", stat, "-", position)), options)
  tab <- as.data.frame(table(category = data$category, g = data$g)); names(tab)[3] <- "n"
  # Declared unused factor levels remain zero cells in historical percent bars;
  # count bars use the observed-case stat_count path.
  tab$pct <- ave(tab$n, tab$g, FUN = function(v) if (sum(v)) 100 * v / sum(v) else rep(0, length(v)))
  ref <- if (stat == "count") ggplot2::ggplot(data, ggplot2::aes(category, fill = g)) + ggplot2::geom_bar(position = position)
    else ggplot2::ggplot(tab, ggplot2::aes(category, pct, fill = g)) + ggplot2::geom_col(position = position)
  layer_near(layer(actual), build(ref)[[1]], c("x", "y", "ymin", "ymax", "group"), paste("Bar", stat, position))
  normalized_rates <- stat == "percent" && position == "fill"
  expected_label <- if (normalized_rates) "Normalized within-group proportions" else if (position == "fill") "Proportion within category" else if (stat == "percent") "Percent within group" else "Count"
  check(identical(actual$plots[[1]]$labels$y, expected_label), "Bar axis does not state its actual denominator/scale")
  if (stat == "percent" && position == "stack") check(grepl("100%|100 percent", actual$figures$figure_note), "Grouped-percent stack caveat missing")
  if (normalized_rates) {
    reference_rows <- which(tab$category == levels(tab$category)[1])
    rates <- tab$pct[reference_rows] / sum(tab$pct[reference_rows])
    composition <- tab$n[reference_rows] / sum(tab$n[reference_rows])
    check(any(abs(rates - composition) > .005), "Fixture must distinguish normalized rates from observed group composition")
    cells <- layer(actual)[layer(actual)$x == 1, , drop = FALSE]
    near(cells$ymax - cells$ymin, rates, "Fill-normalized within-group rates")
    check(grepl("not.*(composition|group proportions)|differ.*composition", actual$figures$figure_note, ignore.case = TRUE),
      "Rate-normalized fill must not imply observed group composition")
  }
  selection(actual, rows)
})
test("plot_bar_percent_total_ungrouped_and_grouped", {
  context <- new_case("bar-total")
  for (grouped in c(FALSE, TRUE)) {
    vars <- c("category", if (grouped) "g"); rows <- which(complete.cases(sample[vars])); data <- sample[rows, ]
    options <- list(type = "bar", vars = "category", stat = "percent", `percent-base` = "total", position = "dodge")
    if (grouped) options$group <- "g"
    actual <- run(context, options)
    tab <- if (grouped) as.data.frame(table(category = data$category, g = data$g)) else as.data.frame(table(category = data$category))
    names(tab)[ncol(tab)] <- "n"; tab$pct <- 100 * tab$n / sum(tab$n)
    ref <- ggplot2::ggplot(tab, if (grouped) ggplot2::aes(category, pct, fill = g) else ggplot2::aes(category, pct)) + ggplot2::geom_col(position = "dodge")
    layer_near(layer(actual), build(ref)[[1]], c("x", "y", "ymin", "ymax", "group"), "Total-based percent")
    near(sum(actual$plots[[1]]$summary$pct), 100, "Total percentage sum")
    check(identical(actual$plots[[1]]$labels$y, "Percent of retained observations"), "Total percent axis denominator missing")
    selection(actual, rows)
  }
})

for (format in c("rds", "csv", "RData", "sav", "parquet")) test(paste0("plot_import_", format), {
  context <- new_case(paste0("import-", format), format = format)
  actual <- run(context, list(type = "scatter", x = "x", y = "y"))
  rows <- which(complete.cases(sample[c("x", "y")]))
  selection(actual, rows)
  # Base write.csv emits decimal text, not the exact binary double payload.
  tolerance <- if (format == "csv") 2e-14 else 0
  near(layer(actual)$x, sample$x[rows], "Imported scatter X", tolerance)
  near(layer(actual)$y, sample$y[rows], "Imported scatter Y", tolerance)
  check(identical(actual$request$dataset$source_sha256, sha(context$input)), "Source hash absent")
})
test("plot_sav_labels_and_user_missing_values", {
  data <- data.frame(x = as.numeric(sample$x), y = as.numeric(sample$y), g = rep(c(2, 1, 3), each = 18))
  data$y[4] <- 99; data$g[6] <- 9
  data$y <- haven::labelled_spss(data$y, labels = c(Missing = 99), na_values = 99, label = "Outcome score")
  data$g <- haven::labelled_spss(data$g, labels = c(Control = 1, Treatment = 2, Followup = 3, Missing = 9), na_values = 9, label = "Condition")
  context <- new_case("sav-labels", data, "sav")
  actual <- run(context, list(type = "scatter", x = "x", y = "y", group = "g"))
  y <- as.numeric(data$y); y[y == 99] <- NA; g <- as.numeric(data$g); g[g == 9] <- NA
  rows <- which(complete.cases(data.frame(x = data$x, y = y, g = g)))
  selection(actual, rows)
  near(layer(actual)$x, data$x[rows], "Labelled X", 0); near(layer(actual)$y, y[rows], "Labelled Y", 0)
  check(length(unique(layer(actual)$group)) == 3L, "Labelled grouping collapsed")
  audit <- jsonlite::toJSON(actual$request$design, auto_unbox = TRUE)
  check(grepl("Outcome score", audit, fixed = TRUE) && grepl("Condition", audit, fixed = TRUE), "Variable labels absent from design audit")
  check(grepl("Treatment", jsonlite::toJSON(actual$plots), fixed = TRUE), "Value labels absent from plotting audit")
  replay(context, actual)
})
test("plot_nonsyntactic_variable_names", {
  data <- sample; names(data)[1:3] <- c("Time (days)", "Score + followup", "Mixed`name")
  context <- new_case("nonsyntactic", data)
  actual <- run(context, list(type = "scatter", x = "Time (days)", y = "Score + followup", group = "g", smooth = "lm"))
  rows <- which(complete.cases(sample[c("x", "y", "g")]))
  near(layer(actual)$x, sample$x[rows], "Nonsyntactic X", 0); near(layer(actual)$y, sample$y[rows], "Nonsyntactic Y", 0)
  check(actual$figures$x == "Time (days)" && actual$figures$y == "Score + followup", "Original names lost")
  second <- run(context, list(type = "histogram", vars = "Mixed`name", bins = 7))
  near(sum(layer(second)$count), sum(!is.na(sample$z)), "Backtick variable histogram N", 0)
})
test("plot_missing_categories_are_distinct_from_literal_missing", {
  data <- data.frame(category = c("Missing", NA, "NA", "Missing", "NA", NA, "other"), y = c(1, 2, NA, 4, 5, 6, 7))
  context <- new_case("missing-collision", data)
  actual <- run(context, list(type = "bar", vars = "category", `na-action` = "keep"))
  check(nrow(layer(actual)) == 4L, "Literal Missing and missing category merged")
  near(sort(actual$plots[[1]]$summary$n), c(1, 2, 2, 2), "Distinct category counts", 0)
  selection(actual, seq_len(nrow(data)), total = nrow(data))
  other <- run(context, list(type = "box", vars = "y", group = "category", `na-action` = "keep"))
  selection(other, which(!is.na(data$y)), total = nrow(data))
  check(grepl("missing", other$figures$figure_note, ignore.case = TRUE), "Mixed kept/omitted missing cases undisclosed")
})
test("plot_numeric_category_identity_close_codes", {
  codes <- c(1, 1 + 1e-15, 1 + 2e-15)
  data <- data.frame(category = rep(codes, c(2, 3, 4)), y = seq_len(9))
  actual <- run(new_case("close-codes", data), list(type = "bar", vars = "category"))
  check(nrow(layer(actual)) == 3L, "Distinct adjacent numeric category codes collapsed")
  near(sort(actual$plots[[1]]$summary$n), c(2, 3, 4), "Distinct numeric category frequencies", 0)
})
test("plot_line_summary_preserves_adjacent_numeric_x", {
  codes <- c(1, 1 + 1e-15, 1 + 2e-15)
  data <- data.frame(x = rep(codes, each = 2), y = c(1, 3, 5, 9, 11, 19))
  actual <- run(new_case("line-close-x", data), list(type = "line", x = "x", y = "y", summary = "mean"))
  near(layer(actual)$x, codes, "Distinct adjacent line positions", 0)
  near(layer(actual)$y, c(2, 7, 15), "Distinct adjacent line means", 0)
})
test("plot_category_labels_do_not_merge_codes", {
  data <- data.frame(category = haven::labelled(rep(c(10, 20, 30), c(2, 3, 4)), labels = c(Same = 10, Same = 20, Other = 30)))
  actual <- run(new_case("duplicate-labels", data), list(type = "bar", vars = "category"))
  check(nrow(layer(actual)) == 3L, "Duplicate value labels merged distinct raw codes")
  near(sort(actual$plots[[1]]$summary$n), c(2, 3, 4), "Duplicate-label categories", 0)
})
test("plot_nonfinite_exclusions_are_recorded", {
  data <- sample; data$x[c(1, 2)] <- c(Inf, -Inf); data$unused <- Inf
  actual <- run(new_case("nonfinite", data), list(type = "scatter", x = "x", y = "y"))
  rows <- which(is.finite(data$x) & is.finite(data$y))
  selection(actual, rows)
  near(actual$plots[[1]]$cases$nonfinite_rows, c(1, 2), "Nonfinite source identities", 0)
  check(grepl("non.?finite|infinite", paste(actual$figures$figure_note, jsonlite::toJSON(actual$result$warnings)), ignore.case = TRUE), "Nonfinite exclusions undisclosed")
})
test("plot_multiple_figures_numbering_and_no_overwrite", {
  context <- new_case("multiple")
  first <- run(context, list(type = "histogram", vars = "x,y", bins = 8L))
  check(nrow(first$figures) == 2L, "Multiple selected variables did not produce multiple figures")
  images <- tree(file.path(context$directory, "plots")); original <- tree(first$path)
  second <- run(context, list(type = "histogram", vars = "x,y", bins = 8L, `figure-number` = 1L))
  after <- tree(file.path(context$directory, "plots"))
  check(length(after) == 4L && identical(images, after[names(images)]), "Non-overwrite figure collision damaged previous files")
  check(identical(original, tree(first$path)), "Later plot modified immutable run")
  check(identical(first$markdown, second$markdown), "Legacy numbering leaked into deterministic run Markdown")
})
test("plot_overwrite_changes_only_legacy_image", {
  context <- new_case("overwrite")
  first <- run(context, list(type = "histogram", vars = "x", bins = 3L, `figure-number` = 1L))
  immutable <- tree(first$path); original <- tree(file.path(context$directory, "plots"))
  second <- run(context, list(type = "histogram", vars = "x", bins = 9L, `figure-number` = 1L, overwrite = TRUE))
  current <- tree(file.path(context$directory, "plots"))
  check(length(current) == 1L && identical(names(current), names(original)) && !identical(current, original), "Explicit overwrite did not replace only its legacy image")
  check(identical(immutable, tree(first$path)), "Legacy overwrite damaged immutable original bundle")
  check(identical(unname(current), sha(file.path(second$path, second$figures$figure_path))), "Legacy overwrite differs from new recorded artifact")
})
test("plot_x_selector_and_auto_routing", {
  context <- new_case("x-selector")
  a <- run(context, list(type = "histogram", x = "x")); check(a$figures$plot_type == "histogram", "Univariate --x unsupported")
  b <- run(context, list(x = "x", y = "y")); check(b$figures$plot_type == "scatter", "Auto numeric routing differs")
  c <- run(context, list(x = "category", y = "y")); check(c$figures$plot_type == "box", "Auto categorical/numeric routing differs")
  d <- run(context, list(type = "bar", vars = "category", group = "category", stat = "percent"))
  near(sum(layer(d)$y), 100, "Same categorical axis and group percent", 0)
})
test("plot_no_log_keeps_auditable_bundle", {
  context <- new_case("no-log")
  run(context, list(type = "histogram", vars = "x"))
  before <- snapshot(file.path(context$directory, "analysis_log.jsonl"))
  actual <- run(context, list(type = "histogram", vars = "x", log = FALSE))
  check(identical(before, snapshot(names(before))), "--log FALSE altered legacy JSONL")
  check(length(actual$result$artifacts) >= 4L && length(actual$plots) == 1L, "--log FALSE suppressed required audit")
})
test("plot_replay_preserves_source_working_config_and_template_independence", {
  context <- new_case("replay-independent")
  template <- file.path(context$base, "custom.md")
  writeLines(c("PLOT CUSTOM TEMPLATE", "{{figure_body}}", "{{narrative}}"), template)
  actual <- run(context, list(type = "line", x = "x", y = "y", summary = "median", template = template, title = "Study trajectory", subtitle = "Repeated observations", caption = "Median outcome", note = "A contextual note."))
  check(grepl("PLOT CUSTOM TEMPLATE", actual$markdown, fixed = TRUE), "Selected template did not render")
  check(identical(actual$plots[[1]]$labels$title, "Study trajectory") && identical(actual$plots[[1]]$labels$subtitle, "Repeated observations") &&
    identical(actual$plots[[1]]$labels$caption, "Median outcome"), "Figure title/subtitle/caption were not applied to plot")
  saveRDS(data.frame(changed = 1:3), context$input)
  arrow::write_parquet(data.frame(changed = 1:4), file.path(context$directory, "sample.parquet"))
  writeLines("CHANGED TEMPLATE", template)
  yaml::write_yaml(list(modules = list(plot = list(summary = "mean", width = 5, height = 4))), config)
  again <- replay(context, actual)
  check(!grepl("CHANGED TEMPLATE", again$markdown, fixed = TRUE), "Replay read mutable external template")
})
for (format in c("png", "pdf", "svg", "jpeg", "jpg", "tiff", "tif", "bmp", "eps", "ps")) test(paste0("plot_image_format_", format), {
  context <- new_case(paste0("format-", format)); actual <- run(context, list(type = "histogram", vars = "x", format = format, width = 4, height = 3, dpi = 72L))
  image <- file.path(actual$path, actual$figures$figure_path[1]); con <- file(image, "rb"); bytes <- readBin(con, "raw", 256); close(con)
  if (format == "png") {
    check(identical(as.integer(bytes[1:8]), c(137L, 80L, 78L, 71L, 13L, 10L, 26L, 10L)), "PNG signature differs")
    to_uint <- function(v) sum(as.integer(v) * 256^(3:0))
    near(c(to_uint(bytes[17:20]), to_uint(bytes[21:24])), c(288, 216), "PNG physical output dimensions", 0)
  } else if (format == "pdf") check(rawToChar(bytes[1:4]) == "%PDF", "PDF signature differs")
  else if (format == "svg") check(grepl("<svg", read_text(image), fixed = TRUE), "SVG content absent")
  else if (format %in% c("jpeg", "jpg")) check(identical(as.integer(bytes[1:2]), c(255L, 216L)), "JPEG signature differs")
  else if (format == "bmp") check(rawToChar(bytes[1:2]) == "BM", "BMP signature differs")
  else if (format %in% c("eps", "ps")) check(rawToChar(bytes[1:4]) == "%!PS", "PostScript signature differs")
  else check(rawToChar(bytes[1:2]) %in% c("II", "MM"), "TIFF signature differs")
})
test("plot_styling_and_legacy_option_aliases", {
  context <- new_case("styles"); rows <- which(complete.cases(sample[c("category", "g")]))
  for (i in 1:3) {
    theme <- c("minimal", "classic", "bw")[i]; palette <- c("default", "viridis", "greys")[i]
    actual <- run(context, list(type = "bars", vars = "category", group = "g", stat = "percent", percent_base = "total", na_action = "omit",
      theme = theme, palette = palette, alpha = .35, file_prefix = "study", file_suffix = "overview"))
    check(actual$request$options$theme == theme && actual$request$options$palette == palette, "Style options not resolved")
    near(layer(actual)$alpha, rep(.35, nrow(layer(actual))), "Bar alpha")
    near(sum(actual$plots[[1]]$summary$pct), 100, "Styles preserve percentages")
    check(grepl("study-001-.*overview[.]png$", actual$figures$figure_path), "Legacy filename aliases ignored")
    selection(actual, rows)
  }
})
for (artifact_kind in c("image", "data", "template", "request", "output")) test(paste0("plot_replay_rejects_tampered_", artifact_kind), {
  context <- new_case(paste0("tamper-", artifact_kind)); actual <- run(context, list(type = "histogram", vars = "x"))
  path <- switch(artifact_kind, image = file.path(actual$path, actual$figures$figure_path[1]), data = file.path(actual$path, "plot-data.rds"),
    template = file.path(actual$path, actual$request$templates[[1]]$path), request = actual$request_path, output = file.path(actual$path, "output.md"))
  if (artifact_kind == "request") {
    value <- raw_json(path); value$options$bins <- 17L; write_json(value, path)
  } else { connection <- file(path, "ab"); writeBin(charToRaw("TAMPERED"), connection); close(connection) }
  failed(context, c("--request", actual$request_path), module = "replay_run", source = FALSE)
})
test("plot_replay_rejects_changed_environment", {
  context <- new_case("changed-environment"); actual <- run(context, list(type = "histogram", vars = "x"))
  req <- raw_json(actual$request_path); req$environment$r_version <- "changed-test-version"; write_json(req, actual$request_path)
  result_path <- file.path(actual$path, "result.json"); res <- raw_json(result_path); res$artifacts$request$sha256 <- sha(actual$request_path); write_json(res, result_path)
  log <- failed(context, c("--request", actual$request_path), module = "replay_run", source = FALSE)
  check(grepl("environment|r_version|R version", read_text(log), ignore.case = TRUE), "Environment mismatch was not the refusal cause")
})
test("plot_rejects_invalid_options_without_publication", {
  context <- new_case("invalid-options"); run(context, list(type = "histogram", vars = "x"))
  invalid <- list(type = "not-a-plot", stat = "counts", `percent-base` = "row", bins = 0L, binwidth = -1,
    bw = 0, smooth = "gam", span = 0, summary = "sum", `na-action` = "ignore", alpha = 2, position = "jitter",
    theme = "unknown", palette = "unknown", format = "docx", width = 0, height = -1, dpi = 0,
    `figure-number` = 0, digits = -1, `file-prefix` = "../escape", `file-suffix` = "..")
  for (option in names(invalid)) {
    opts <- list(type = "histogram", vars = "x"); opts[[option]] <- invalid[[option]]; failed(context, opts)
  }
  for (opts in list(c("--type", "histogram", "--type", "density", "--vars", "x"),
    c("--type", "histogram", "--vars", "absent"), c("--type", "scatter", "--x", "x"),
    c("--type", "histogram", "--vars", "category"), c("--type", "histogram", "--vars", "x", "--se", "maybe"),
    c("--type", "histogram", "--vars", "x", "--unknown-option", "x"),
    c("--type", "histogram", "--vars", "x", "--na-action", "keep", "--na_action", "omit"),
    c("--type", "bar", "--vars", "category", "--percent-base", "total", "--percent_base", "group"))) failed(context, opts)
})
test("plot_all_missing_and_empty_selections_fail", {
  for (data in list(data.frame(x = c(NA_real_, NA_real_)), data.frame(x = numeric()))) {
    context <- new_case(paste0("empty-", nrow(data)), data)
    failed(context, list(type = "histogram", vars = "x"))
  }
})
test("plot_unavailable_smooth_is_not_fabricated", {
  data <- data.frame(x = rep(1, 8), y = seq_len(8))
  actual <- run(new_case("unavailable-smooth", data), list(type = "scatter", x = "x", y = "y", smooth = "lm"))
  near(layer(actual)$y, data$y, "Scatter remains usable without smooth", 0)
  check(!nrow(layer(actual, 2)), "Unestimable constant-X smoother fabricated")
  check(actual$plots[[1]]$layer_status[[2]]$status == "unavailable", "Unestimable smoother status hidden")
  check(length(actual$result$warnings) > 0L, "Unestimable smoother warning absent")
})
test("plot_constant_correlation_heatmap_fails", {
  context <- new_case("constant-heatmap", data.frame(x = rep(1, 8), y = seq_len(8)))
  failed(context, list(type = "corr-heatmap", vars = "x,y"))
})
test("plot_rdata_init_reuses_named_object_identity", {
  context <- new_case("init-rdata", format = "RData")
  survey <- sample; alternative <- sample; alternative$x <- alternative$x + 100
  save(survey, alternative, file = context$input)
  run_cli(context, c(context$source, "--agent", "Codex"), module = "init_workspace")
  actual <- run(context, list(type = "histogram", vars = "x"))
  near(sum(layer(actual)$count), sum(!is.na(sample$x)), "Initialized RData is reusable", 0)
  # The other object exists in the same unchanged source bytes; its identity
  # must not be silently adopted as the already initialized survey dataset.
  wrong <- c("--rdata", context$input, "--df", "alternative", "--dataset-name", "survey", "--type", "histogram", "--vars", "x")
  failed(context, wrong, source = FALSE)
})
test("plot_publication_fault_injection", {
  path <- if (!is.null(cfg$phase2$plot_publication_contract)) absolute(cfg$phase2$plot_publication_contract) else file.path(repo, "tests/phase2/plot_publication_contract.R")
  log <- file.path(work, "publication-injection.log")
  previous <- Sys.getenv("NLSS_TEST_ROOT", unset = NA_character_)
  on.exit(if (is.na(previous)) Sys.unsetenv("NLSS_TEST_ROOT") else Sys.setenv(NLSS_TEST_ROOT = previous), add = TRUE)
  Sys.setenv(NLSS_TEST_ROOT = file.path(work, "publication-injection"))
  status <- system2(file.path(R.home("bin"), "Rscript"), shQuote(path), stdout = log, stderr = log)
  if (is.na(previous)) Sys.unsetenv("NLSS_TEST_ROOT") else Sys.setenv(NLSS_TEST_ROOT = previous)
  check(status == 0L && !grepl("[FAIL]", read_text(log), fixed = TRUE), paste("Publication fault injection failed", read_text(log)))
})

if (!length(results)) stop("No tests selected")
summary_path <- file.path(work, "results.json")
write_json(list(schema_version = 1L, suite = "phase2-plot", execution_contract = "resolved-request-v1", test_pattern = pattern,
  source_sha256 = list(plot = sha(file.path(repo, "scripts/R/plot.R")), runner = sha(script)), numeric_checks = numeric_checks,
  independent_numeric_checks = numeric_checks - json_numeric_checks, json_numeric_checks = json_numeric_checks, tests = results,
  environment = list(r = R.version.string, packages = as.list(vapply(required, function(p) as.character(packageVersion(p)), character(1))))), summary_path)
passed <- sum(vapply(results, function(x) x$passed, logical(1)))
cat(sprintf("Phase 2 plot: %d/%d cases passed; %d numeric comparisons (%d independent/contract, %d JSON parity). Results: %s\n", passed, length(results), numeric_checks, numeric_checks - json_numeric_checks, json_numeric_checks, summary_path))
if (!nzchar(forced) && keep > 0L) {
  dirs <- sort(list.dirs(collection, recursive = FALSE, full.names = TRUE), decreasing = TRUE)
  dirs <- dirs[grepl("^run-[0-9]{14}-[0-9]+$", basename(dirs))]
  if (length(dirs) > keep) for (path in setdiff(tail(dirs, -keep), work)) unlink(path, recursive = TRUE)
}
if (passed != length(results)) quit(status = 1L)
