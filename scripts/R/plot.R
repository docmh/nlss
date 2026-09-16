# SPDX-License-Identifier: Apache-2.0
#!/usr/bin/env Rscript

bootstrap_dir <- {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", cmd_args[grep("^--file=", cmd_args)])
  if (length(file_arg) > 0 && nzchar(file_arg[1])) {
    dirname(normalizePath(file_arg[1], winslash = "/", mustWork = FALSE))
  } else {
    getwd()
  }
}
source(file.path(bootstrap_dir, "lib", "bootstrap.R"))
nlss_bootstrap()

print_usage <- function() {
  cat("Plots (ggplot2)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript plot.R --csv data.csv --type histogram --vars age\n")
  cat("  Rscript plot.R --csv data.csv --type bar --vars gender --stat percent\n")
  cat("  Rscript plot.R --csv data.csv --type scatter --x age --y score --group condition\n")
  cat("  Rscript plot.R --csv data.csv --type box --y score --group condition\n")
  cat("  Rscript plot.R --csv data.csv --type corr-heatmap --vars x1,x2,x3\n")
  cat("  Rscript plot.R --interactive\n")
  cat("\n")
  cat("Options:\n")
  cat("  --csv PATH             CSV input file\n")
  cat("  --sav PATH             SPSS .sav input file\n")
  cat("  --sep VALUE            CSV separator (default: ,)\n")
  cat("  --header TRUE/FALSE    CSV header (default: TRUE)\n")
  cat("  --rds PATH             RDS input file (data frame)\n")
  cat("  --rdata PATH           RData input file\n")
  cat("  --parquet PATH         Parquet input file\n")
  cat("  --df NAME              Data frame object name in RData\n")
  cat("  --type NAME            Plot type (auto, histogram, density, box, violin, bar, scatter, line, qq, corr-heatmap)\n")
  cat("  --vars LIST            Comma-separated variables\n")
  cat("  --x NAME               X variable\n")
  cat("  --y NAME               Y variable\n")
  cat("  --group NAME           Grouping variable (optional)\n")
  cat("  --stat NAME            Bar stat: count or percent (default: count)\n")
  cat("  --percent-base NAME    Percent base: total or group (default: total)\n")
  cat("  --bins N               Histogram bins (default: 30)\n")
  cat("  --binwidth VALUE       Histogram binwidth (optional)\n")
  cat("  --bw VALUE             Density bandwidth (optional)\n")
  cat("  --smooth NAME          Scatter smooth: none, loess, lm (default: none)\n")
  cat("  --se TRUE/FALSE        Smooth SE band (default: TRUE)\n")
  cat("  --span VALUE           Loess span (default: 0.75)\n")
  cat("  --summary NAME         Line summary: none, mean, median (default: none)\n")
  cat("  --na-action NAME       omit or keep (default: omit)\n")
  cat("  --alpha VALUE          Transparency (default: 0.7)\n")
  cat("  --position NAME        Bar position: dodge, stack, fill (default: dodge)\n")
  cat("  --theme NAME           Theme: minimal, classic, bw (default: minimal)\n")
  cat("  --palette NAME         Palette: default, viridis, greys (default: default)\n")
  cat("  --title TEXT           Figure title (optional)\n")
  cat("  --subtitle TEXT        Figure subtitle (optional)\n")
  cat("  --caption TEXT         Figure caption text (optional)\n")
  cat("  --note TEXT            Figure note text (optional)\n")
  cat("  --format NAME          Output format: png, pdf, svg (default: png)\n")
  cat("  --width VALUE          Figure width in inches (default: 7)\n")
  cat("  --height VALUE         Figure height in inches (default: 5)\n")
  cat("  --dpi N                Raster DPI (default: 300)\n")
  cat("  --file-prefix TEXT     Filename prefix (default: figure)\n")
  cat("  --file-suffix TEXT     Filename suffix (optional)\n")
  cat("  --figure-number N      Starting figure number override (optional)\n")
  cat("  --overwrite TRUE/FALSE Overwrite existing files (default: FALSE)\n")
  cat("  --digits N             Rounding digits (default: 2)\n")
  cat("  --template REF         Template path or template key (optional)\n")
  cat("  --user-prompt TEXT     Original AI user prompt for logging (optional)\n")
  cat("  --log TRUE/FALSE       Write analysis_log.jsonl (default: TRUE)\n")
  cat("  --interactive          Prompt for inputs\n")
  cat("  --help                 Show this help\n")
}

interactive_options <- function() {
  cat("Interactive input selected.\n")
  input_type <- prompt("Input type (csv/sav/rds/rdata/parquet)", "csv")
  input_type <- tolower(input_type)
  opts <- list()

  if (input_type == "csv") {
    opts$csv <- prompt("CSV path")
    sep_default <- get_config_value("defaults.csv.sep", ",")
    header_default <- get_config_value("defaults.csv.header", TRUE)
    opts$sep <- prompt("Separator", sep_default)
    opts$header <- prompt("Header TRUE/FALSE", ifelse(isTRUE(header_default), "TRUE", "FALSE"))
  } else if (input_type == "sav") {
    opts$sav <- prompt("SAV path")
  } else if (input_type == "rds") {
    opts$rds <- prompt("RDS path")
  } else if (input_type == "rdata") {
    opts$rdata <- prompt("RData path")
    opts$df <- prompt("Data frame object name")
  } else if (input_type == "parquet") {
    opts$parquet <- prompt("Parquet path")
  } else {
    stop("Unsupported input type.")
  }

  type_default <- get_config_value("modules.plot.type", "auto")
  opts$type <- prompt("Plot type", type_default)
  opts$vars <- prompt("Variables (comma-separated, blank for defaults)", "")
  opts$x <- prompt("X variable (blank if using --vars)", "")
  opts$y <- prompt("Y variable (blank if using --vars)", "")
  opts$group <- prompt("Group variable (optional)", "")
  opts$stat <- prompt("Bar stat (count/percent)", get_config_value("modules.plot.stat", "count"))
  opts$percent_base <- prompt("Percent base (total/group)", get_config_value("modules.plot.percent_base", "total"))
  opts$bins <- prompt("Histogram bins", as.character(get_config_value("modules.plot.bins", 30)))
  opts$binwidth <- prompt("Histogram binwidth (blank for default)", "")
  opts$bw <- prompt("Density bandwidth (blank for default)", "")
  opts$smooth <- prompt("Smooth (none/loess/lm)", get_config_value("modules.plot.smooth", "none"))
  opts$se <- prompt("Smooth SE TRUE/FALSE", ifelse(isTRUE(get_config_value("modules.plot.se", TRUE)), "TRUE", "FALSE"))
  opts$summary <- prompt("Line summary (none/mean/median)", get_config_value("modules.plot.summary", "none"))
  opts$na_action <- prompt("Missing handling (omit/keep)", get_config_value("modules.plot.na_action", "omit"))
  opts$theme <- prompt("Theme (minimal/classic/bw)", get_config_value("modules.plot.theme", "minimal"))
  opts$palette <- prompt("Palette (default/viridis/greys)", get_config_value("modules.plot.palette", "default"))
  opts$title <- prompt("Figure title (optional)", "")
  opts$subtitle <- prompt("Figure subtitle (optional)", "")
  opts$caption <- prompt("Figure caption (optional)", "")
  opts$note <- prompt("Figure note (optional)", "")
  opts$format <- prompt("Output format (png/pdf/svg)", get_config_value("modules.plot.format", "png"))
  opts$width <- prompt("Figure width (inches)", as.character(get_config_value("modules.plot.width", 7)))
  opts$height <- prompt("Figure height (inches)", as.character(get_config_value("modules.plot.height", 5)))
  opts$dpi <- prompt("DPI", as.character(get_config_value("modules.plot.dpi", 300)))
  opts$file_prefix <- prompt("Filename prefix", get_config_value("modules.plot.file_prefix", "figure"))
  opts$file_suffix <- prompt("Filename suffix (optional)", "")
  opts$overwrite <- prompt("Overwrite existing TRUE/FALSE", "FALSE")
  opts$digits <- prompt("Rounding digits", as.character(get_config_value("defaults.digits", 2)))
  opts$template <- prompt("Template (path or key; blank for default)", "")
  opts$`user-prompt` <- prompt("User prompt (optional)", "")
  log_default <- get_config_value("defaults.log", TRUE)
  opts$log <- prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))
  opts
}


# One engine: no alternate renderer may silently change grouping or statistics.
plot_choice <- function(value, choices, name) {
  value <- tolower(trimws(as.character(value)))
  if (length(value) != 1L || is.na(value) || !value %in% choices)
    stop("--", name, " must be one of: ", paste(choices, collapse = ", "), ".")
  value
}

plot_number <- function(value, name, lower = 0, upper = Inf, integer = FALSE, optional = FALSE, inclusive = FALSE) {
  if (is.null(value) || identical(value, "")) {
    if (optional) return(NULL)
    stop("--", name, " requires a number.")
  }
  number <- suppressWarnings(as.numeric(value))
  if (is.logical(value) || length(number) != 1L || !is.finite(number) ||
      number > upper || (if (inclusive) number < lower else number <= lower) ||
      (integer && number != floor(number))) stop("Invalid numeric value for --", name, ".")
  if (integer) as.integer(number) else number
}

plot_text <- function(value, default = "") {
  if (is.null(value)) return(default)
  if (is.logical(value) || length(value) != 1L || is.na(value)) stop("Plot text and variable options require a value.")
  trimws(as.character(value))
}

normalize_plot_type <- function(value) {
  type <- gsub("_", "-", tolower(plot_text(value, "auto")), fixed = TRUE)
  aliases <- list(histogram = c("hist", "histogram"), density = c("dens", "density"),
    box = c("boxplot", "box"), violin = c("violin", "violinplot"), bar = c("bar", "bars"),
    scatter = c("scatter", "scatterplot"), line = c("line", "lineplot"),
    qq = c("qq", "qqplot", "qq-plot"),
    "corr-heatmap" = c("corr", "correlation", "corr-heatmap", "correlation-heatmap", "heatmap"))
  for (name in names(aliases)) if (type %in% aliases[[name]]) return(name)
  plot_choice(type, c("auto", names(aliases)), "type")
}

plot_options <- function(opts) {
  for (name in c("percent-base", "na-action", "file-prefix", "file-suffix")) {
    alias <- gsub("-", "_", name, fixed = TRUE)
    if (!is.null(opts[[name]]) && !is.null(opts[[alias]]) &&
        !identical(as.character(opts[[name]]), as.character(opts[[alias]])))
      stop("Conflicting --", name, " and --", alias, " values.")
    if (is.null(opts[[name]])) opts[[name]] <- opts[[alias]]
    opts[[alias]] <- NULL
  }
  defaults <- get_config_value("modules.plot")
  value <- function(name) {
    supplied <- opts[[name]]
    if (is.null(supplied) || identical(supplied, "")) defaults[[gsub("-", "_", name, fixed = TRUE)]] else supplied
  }
  out <- list(type = normalize_plot_type(value("type")),
    vars = plot_text(opts$vars), x = plot_text(opts$x), y = plot_text(opts$y), group = plot_text(opts$group),
    stat = plot_choice(value("stat"), c("count", "percent"), "stat"),
    percent_base = plot_choice(value("percent-base"), c("total", "group"), "percent-base"),
    bins = plot_number(value("bins"), "bins", integer = TRUE, upper = .Machine$integer.max),
    binwidth = plot_number(value("binwidth"), "binwidth", optional = TRUE),
    bw = plot_number(value("bw"), "bw", optional = TRUE),
    smooth = plot_choice(value("smooth"), c("none", "loess", "lm"), "smooth"),
    se = parse_bool(value("se")), span = plot_number(value("span"), "span"),
    summary = plot_choice(value("summary"), c("none", "mean", "median"), "summary"),
    na_action = plot_choice(value("na-action"), c("omit", "keep"), "na-action"),
    alpha = plot_number(value("alpha"), "alpha", upper = 1, inclusive = TRUE),
    position = plot_choice(value("position"), c("dodge", "stack", "fill"), "position"),
    theme = plot_choice(value("theme"), c("minimal", "classic", "bw"), "theme"),
    palette = plot_choice(value("palette"), c("default", "viridis", "greys"), "palette"),
    format = plot_choice(sub("^[.]", "", value("format")), c("png", "pdf", "svg", "jpeg", "jpg", "tiff", "tif", "bmp", "eps", "ps"), "format"),
    width = plot_number(value("width"), "width"),
    height = plot_number(value("height"), "height"),
    dpi = plot_number(value("dpi"), "dpi", integer = TRUE, upper = .Machine$integer.max),
    file_prefix = plot_text(value("file-prefix")), file_suffix = plot_text(value("file-suffix")),
    figure_number = plot_number(opts[["figure-number"]], "figure-number", integer = TRUE, optional = TRUE, upper = .Machine$integer.max),
    figure_digits = plot_number(defaults$figure_digits, "figure-digits", integer = TRUE, upper = 12),
    overwrite = parse_bool(opts$overwrite, FALSE),
    digits = plot_number(if (is.null(opts$digits)) get_config_value("defaults.digits") else opts$digits,
      "digits", lower = 0, upper = 15, inclusive = TRUE, integer = TRUE),
    title = plot_text(opts$title), subtitle = plot_text(opts$subtitle),
    caption = plot_text(opts$caption), note = plot_text(opts$note),
    log = parse_bool(opts$log, get_config_value("defaults.log")))
  for (name in c("file_prefix", "file_suffix")) {
    value <- out[[name]]
    if (grepl("[/\\\\]", value) || value %in% c(".", ".."))
      stop("Plot filename components must not contain directory paths.")
    if (nzchar(value)) out[[name]] <- sanitize_file_component(value)
  }
  if (!nzchar(out$file_prefix)) stop("Plot filename prefix must not be empty.")
  out
}

build_plot_requests <- function(df, opts) {
  type <- opts$type
  x <- opts$x; y <- opts$y; group <- opts$group
  explicit <- parse_list(opts$vars)
  named <- unique(c(explicit, x, y, group))
  unknown <- setdiff(named[nzchar(named)], names(df))
  if (length(unknown)) stop("Unknown variables: ", paste(unknown, collapse = ", "))
  numeric <- function(var) nzchar(var) && is.numeric(df[[var]])
  vars <- if (length(explicit)) unique(explicit) else character()
  if (!length(vars) && !nzchar(x) && !nzchar(y)) {
    default <- if (type == "bar") "all" else get_config_value("modules.plot.vars_default")
    vars <- select_variables(df, NULL, if (nzchar(group)) group else NULL, default = default, include_numeric = TRUE)
  }
  if (type == "auto") {
    if (nzchar(y)) type <- if (nzchar(x) && numeric(x) && numeric(y)) "scatter" else "box"
    else if (nzchar(x)) type <- if (numeric(x)) "histogram" else "bar"
    else if (length(vars)) type <- if (!numeric(vars[1])) "bar" else if (nzchar(group)) "box" else "histogram"
    else stop("No variables available for plotting.")
  }
  make <- function(x = "", y = "", vars = character(), group = group)
    list(type = type, x = x, y = y, group = group, vars = vars)
  if (type == "corr-heatmap") {
    if (nzchar(x) || nzchar(y) || nzchar(group)) stop("Correlation heatmaps use --vars; --x, --y and --group are not supported.")
    if (length(vars) < 2L) stop("corr-heatmap requires at least two numeric variables.")
    return(list(make(vars = vars, group = "")))
  }
  if (type %in% c("scatter", "line")) {
    if (!nzchar(x) || !nzchar(y)) stop("Scatter/line plots require numeric --x and --y.")
    if (length(explicit)) stop("Scatter/line plots use --x and --y, not --vars.")
    return(list(make(x, y, group = group)))
  }
  if (type %in% c("box", "violin")) {
    axis <- if (nzchar(x)) x else group
    if (!nzchar(axis)) stop("Box and violin plots require --group or --x for the categorical axis.")
    if (nzchar(y) && length(explicit)) stop("Choose --y or --vars for box/violin responses, not both.")
    responses <- if (nzchar(y)) y else vars
    responses <- setdiff(responses, axis)
    if (!length(responses)) stop("No numeric response selected for box/violin plot.")
    return(lapply(responses, function(var) make(axis, var, group = group)))
  }
  if (nzchar(y)) stop("Univariate plots use --x or --vars, not --y.")
  if (nzchar(x) && length(explicit)) stop("Choose --x or --vars for a univariate plot, not both.")
  variables <- if (nzchar(x)) x else vars
  if (!length(variables)) stop("No variables available for plotting.")
  lapply(variables, function(var) make(var, group = group))
}

# Categories are identified before formatting or labelling; duplicate human
# labels and adjacent floating-point codes can never merge statistical groups.
plot_categories <- function(values, variable, source_rows, dictionary, keep_missing) {
  codes <- if (is.factor(values)) levels(values) else sort(unique(values[!is.na(values)]))
  raw <- if (is.factor(values)) as.character(values) else values
  ids <- match(raw, codes)
  display <- if (is.numeric(codes)) sprintf("%.17g", codes) else as.character(codes)
  value_labels <- dictionary$columns[[variable]]$value_labels
  for (i in seq_along(codes)) {
    hits <- vapply(value_labels, function(entry) {
      !is.null(entry$value) && length(entry$value) == 1L &&
        identical(as.character(entry$value), as.character(codes[i])) &&
        (!is.numeric(codes) || identical(as.numeric(entry$value), as.numeric(codes[i])))
    }, logical(1))
    if (any(hits)) display[i] <- as.character(value_labels[[which(hits)[1L]]]$label)
  }
  missing <- keep_missing && anyNA(values)
  if (missing) {
    ids[is.na(values)] <- length(codes) + 1L
    display <- c(display, "Missing")
  }
  duplicates <- duplicated(display) | duplicated(display, fromLast = TRUE)
  if (any(duplicates)) display[duplicates] <- paste0(display[duplicates], " [category ", which(duplicates), "]")
  display <- make.unique(display, sep = " [duplicate] ")
  entries <- lapply(seq_along(display), function(i) list(id = i,
    raw_value = if (i <= length(codes)) unname(codes[i]) else NULL,
    is_missing = i > length(codes), label = display[i],
    source_rows = source_rows[which(ids == i)]))
  list(values = factor(ids, levels = seq_along(display), labels = display),
    metadata = list(variable = variable, ordered = is.ordered(values), levels = entries))
}

prepare_plot_data <- function(df, request, na_action, dictionary) {
  type <- request$type; x <- request$x; y <- request$y; group <- request$group
  numeric_vars <- switch(type, "corr-heatmap" = request$vars, bar = character(),
    box = y, violin = y, scatter = unique(c(x, y)), line = unique(c(x, y)), x)
  categorical <- unique(c(if (type %in% c("bar", "box", "violin")) x else character(), group[nzchar(group)]))
  invalid <- numeric_vars[!vapply(df[numeric_vars], is.numeric, logical(1))]
  if (length(invalid)) stop("Plot requires numeric variables: ", paste(invalid, collapse = ", "))
  invalid_cat <- categorical[!vapply(df[categorical], function(v) is.atomic(v) && !is.complex(v), logical(1))]
  if (length(invalid_cat)) stop("Unsupported categorical variable: ", paste(invalid_cat, collapse = ", "))
  n <- nrow(df); missing_num <- nonfinite <- missing_cat <- rep(FALSE, n)
  for (var in numeric_vars) {
    missing_num <- missing_num | is.na(df[[var]])
    nonfinite <- nonfinite | (!is.na(df[[var]]) & !is.finite(df[[var]]))
  }
  for (var in categorical) missing_cat <- missing_cat | is.na(df[[var]])
  keep <- !missing_num & !nonfinite & (na_action == "keep" | !missing_cat)
  source_rows <- which(keep)
  if (!length(source_rows)) stop("No usable observations for requested ", type, " plot.")
  if (any(nonfinite)) warning("Non-finite numeric observations omitted from ", type, " plot (n = ", sum(nonfinite), ").", call. = FALSE)
  aliases <- if (type == "corr-heatmap") setNames(request$vars, request$vars) else {
    mapping <- c(.x = x)
    if (nzchar(y)) mapping <- c(mapping, .y = y)
    if (nzchar(group)) mapping <- c(mapping, .group = group)
    mapping
  }
  data <- data.frame(row.names = seq_along(source_rows))
  categories <- list()
  for (alias in names(aliases)) {
    var <- aliases[[alias]]
    values <- df[[var]][source_rows]
    if (alias == ".group" || (alias == ".x" && type %in% c("bar", "box", "violin"))) {
      converted <- plot_categories(values, var, source_rows, dictionary, na_action == "keep")
      data[[alias]] <- converted$values
      categories[[var]] <- converted$metadata
    } else data[[alias]] <- values
  }
  stats <- list(total_n = n, n = sum(keep), missing_n = sum(!keep),
    missing_pct = 100 * sum(!keep) / n, missing_kept_n = sum(keep & missing_cat),
    nonfinite_n = sum(nonfinite))
  list(data = data, aliases = as.list(aliases), categories = categories,
    cases = list(source_rows = source_rows, excluded_rows = which(!keep),
      numeric_missing_rows = which(missing_num), nonfinite_rows = which(nonfinite),
      missing_categorical_rows = which(missing_cat), stats = stats,
      selection = if (type == "corr-heatmap") "listwise_complete_finite" else "joint_required_variables"))
}

plot_palette <- function(plot, palette, fill = FALSE, color = FALSE) {
  if (palette == "viridis") {
    if (fill) plot <- plot + ggplot2::scale_fill_viridis_d()
    if (color) plot <- plot + ggplot2::scale_color_viridis_d()
  } else if (palette == "greys") {
    if (fill) plot <- plot + ggplot2::scale_fill_grey()
    if (color) plot <- plot + ggplot2::scale_color_grey()
  }
  plot
}

build_plot <- function(info, request, opts, labels) {
  type <- request$type; data <- info$data; grouped <- nzchar(request$group)
  summary <- NULL
  x_label <- resolve_variable_label(labels, request$x)
  y_label <- resolve_variable_label(labels, request$y)
  group_label <- resolve_variable_label(labels, request$group)
  if (type == "corr-heatmap") {
    correlation <- stats::cor(data, use = "everything", method = "pearson")
    if (!any(is.finite(correlation[upper.tri(correlation)]))) stop("Correlation heatmap has no estimable between-variable correlation.")
    summary <- as.data.frame(as.table(correlation), stringsAsFactors = FALSE)
    names(summary) <- c("var1", "var2", "r")
    summary$n <- nrow(data)
    summary$r_label <- ifelse(is.finite(summary$r), format(round(summary$r, opts$digits), nsmall = opts$digits), "NA")
    names_display <- vapply(request$vars, function(var) resolve_variable_label(labels, var), character(1))
    if (anyDuplicated(names_display)) names_display <- paste0(names_display, " [", request$vars, "]")
    names(names_display) <- request$vars
    plot <- ggplot2::ggplot(summary, ggplot2::aes(x = var1, y = var2, fill = r)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
        limits = c(-1, 1), na.value = "grey80") +
      ggplot2::geom_text(ggplot2::aes(label = r_label), size = 3) +
      ggplot2::scale_x_discrete(labels = names_display) + ggplot2::scale_y_discrete(labels = names_display) +
      ggplot2::labs(x = NULL, y = NULL, fill = "Pearson r")
  } else if (type %in% c("scatter", "line")) {
    mapping <- if (grouped) ggplot2::aes(x = .x, y = .y, color = .group, group = .group) else ggplot2::aes(x = .x, y = .y)
    if (type == "line" && opts$summary != "none") {
      # aggregate() factors numeric grouping columns via rounded strings. Map
      # exact numeric x identities to integers before grouping, then restore x.
      x_values <- sort(unique(data$.x))
      by <- data.frame(.x_id = match(data$.x, x_values))
      if (grouped) by$.group <- data$.group
      summary <- stats::aggregate(data$.y, by = by, FUN = if (opts$summary == "median") stats::median else mean)
      names(summary)[ncol(summary)] <- ".y"
      summary$.x <- x_values[summary$.x_id]
      summary$.x_id <- NULL
      summary <- summary[c(".x", if (grouped) ".group", ".y")]
      data <- summary
    }
    plot <- ggplot2::ggplot(data, mapping)
    if (type == "scatter") {
      plot <- plot + ggplot2::geom_point(alpha = opts$alpha)
      if (opts$smooth != "none") plot <- plot + ggplot2::geom_smooth(
        method = opts$smooth, formula = y ~ x, se = opts$se, span = opts$span, n = 80L, level = 0.95)
    } else plot <- plot + ggplot2::geom_line(alpha = opts$alpha)
    plot <- plot_palette(plot, opts$palette, color = grouped)
    plot <- plot + ggplot2::labs(x = x_label, y = y_label, color = group_label)
  } else if (type %in% c("box", "violin")) {
    separate_group <- grouped && request$group != request$x
    mapping <- if (separate_group) ggplot2::aes(x = .x, y = .y, fill = .group) else ggplot2::aes(x = .x, y = .y, fill = .x)
    plot <- ggplot2::ggplot(data, mapping)
    if (type == "violin") plot <- plot + ggplot2::geom_violin(alpha = opts$alpha, trim = FALSE)
    plot <- plot + if (type == "violin") ggplot2::geom_boxplot(alpha = opts$alpha, width = 0.2) else ggplot2::geom_boxplot(alpha = opts$alpha)
    plot <- plot_palette(plot, opts$palette, fill = TRUE)
    plot <- plot + ggplot2::labs(x = x_label, y = y_label, fill = if (separate_group) group_label else x_label)
  } else if (type == "bar") {
    counts <- if (grouped) as.data.frame(table(.x = data$.x, .group = data$.group), stringsAsFactors = FALSE)
      else as.data.frame(table(.x = data$.x), stringsAsFactors = FALSE)
    names(counts)[ncol(counts)] <- "n"
    counts$.x <- factor(counts$.x, levels = levels(data$.x))
    if (grouped) counts$.group <- factor(counts$.group, levels = levels(data$.group))
    denominator <- if (grouped && opts$percent_base == "group") ave(counts$n, counts$.group, FUN = sum) else rep(sum(counts$n), nrow(counts))
    counts$pct <- ifelse(denominator > 0, 100 * counts$n / denominator, 0)
    counts$denominator <- denominator
    counts$.height <- if (opts$stat == "percent") counts$pct else counts$n
    summary <- counts
    mapping <- if (grouped) ggplot2::aes(x = .x, y = .height, fill = .group) else ggplot2::aes(x = .x, y = .height)
    if (opts$stat == "percent") plot <- ggplot2::ggplot(counts, mapping) + ggplot2::geom_col(position = opts$position, alpha = opts$alpha)
    else {
      mapping <- if (grouped) ggplot2::aes(x = .x, fill = .group) else ggplot2::aes(x = .x)
      plot <- ggplot2::ggplot(data, mapping) + ggplot2::geom_bar(position = opts$position, alpha = opts$alpha)
    }
    ylabel <- if (opts$position == "fill") {
      if (opts$stat == "percent" && grouped && opts$percent_base == "group") "Normalized within-group proportions" else "Proportion within category"
    } else if (opts$stat == "percent") {
      if (grouped && opts$percent_base == "group") "Percent within group" else "Percent of retained observations"
    } else "Count"
    plot <- plot_palette(plot, opts$palette, fill = grouped)
    plot <- plot + ggplot2::labs(x = x_label, y = ylabel)
    if (grouped) plot <- plot + ggplot2::labs(fill = group_label)
  } else if (type == "qq") {
    mapping <- if (grouped) ggplot2::aes(sample = .x, color = .group, group = .group) else ggplot2::aes(sample = .x)
    plot <- ggplot2::ggplot(data, mapping) + ggplot2::stat_qq() + ggplot2::stat_qq_line()
    plot <- plot_palette(plot, opts$palette, color = grouped)
    plot <- plot + ggplot2::labs(x = "Theoretical normal quantiles", y = x_label, color = group_label)
  } else {
    mapping <- if (grouped) ggplot2::aes(x = .x, fill = .group, group = .group) else ggplot2::aes(x = .x)
    plot <- ggplot2::ggplot(data, mapping)
    if (type == "histogram") {
      plot <- plot + ggplot2::geom_histogram(bins = if (is.null(opts$binwidth)) opts$bins else NULL,
        binwidth = opts$binwidth, position = "identity", alpha = opts$alpha)
    } else {
      plot <- plot + ggplot2::geom_density(bw = if (is.null(opts$bw)) "nrd0" else opts$bw, alpha = opts$alpha)
    }
    plot <- plot_palette(plot, opts$palette, fill = grouped)
    plot <- plot + ggplot2::labs(x = x_label, y = if (type == "density") "Density" else "Count", fill = group_label)
  }
  theme <- switch(opts$theme, minimal = ggplot2::theme_minimal(), classic = ggplot2::theme_classic(), bw = ggplot2::theme_bw())
  plot <- plot + theme
  if (type == "corr-heatmap") plot <- plot + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  list(plot = plot, summary = summary)
}

build_default_title <- function(request, stat, labels) {
  x <- resolve_variable_label(labels, request$x); y <- resolve_variable_label(labels, request$y)
  switch(request$type, histogram = paste("Distribution of", x), density = paste("Density of", x),
    qq = paste("Q-Q Plot of", x), bar = paste(if (stat == "percent") "Percent of" else "Counts of", x),
    box = paste(y, "by", x), violin = paste(y, "by", x), scatter = paste(y, "vs", x),
    line = paste(y, "over", x), "corr-heatmap" = "Correlation heatmap")
}

build_figure_note <- function(info, request, opts) {
  stats <- info$cases$stats
  notes <- character()
  if (stats$missing_n > 0) notes <- c(notes, paste0("Missing or non-finite values omitted (n = ",
    stats$missing_n, ", ", round(stats$missing_pct, opts$digits), "%)."))
  if (stats$missing_kept_n > 0) notes <- c(notes, paste0("Missing categories retained (n = ",
    stats$missing_kept_n, "); their labelled category is distinct from literal values."))
  if (nzchar(request$group)) notes <- c(notes, paste0("Grouped by ", request$group, "."))
  if (request$type == "corr-heatmap") notes <- c(notes, "Pearson correlations use the same listwise-complete finite observations; colour limits are -1 to 1.")
  if (request$type == "bar" && opts$stat == "percent")
    notes <- c(notes, paste0("Percent denominator: ", if (opts$percent_base == "group" && nzchar(request$group)) "retained observations within each group" else "all retained observations", "."))
  if (request$type == "bar" && opts$stat == "percent" && opts$percent_base == "group" &&
      nzchar(request$group) && opts$position == "stack")
    notes <- c(notes, "Stacked segments use separate group denominators; the stack total is not a sample percentage and can exceed 100%.")
  if (request$type == "bar" && opts$position == "fill")
    notes <- c(notes, "Fill positioning rescales each x-category stack to proportion 1 after the selected count/percent calculation.")
  if (request$type == "bar" && opts$stat == "percent" && opts$percent_base == "group" &&
      nzchar(request$group) && opts$position == "fill")
    notes <- c(notes, "Within-group percentages are rescaled to sum to 1 within each x-category. With unequal group sizes, these segments do not represent the observed group composition of that category.")
  if (request$type == "scatter" && opts$smooth != "none")
    notes <- c(notes, paste0("Smoother: ", opts$smooth, if (opts$se) "; pointwise 95% confidence band." else "; no confidence band."))
  if (request$type == "line") notes <- c(notes, paste0("Line summary: ", opts$summary,
    "; points are connected in x order within each group, not interpreted as individual trajectories."))
  if (nzchar(opts$note)) notes <- c(notes, opts$note)
  if (!length(notes)) "None." else paste(notes, collapse = " ")
}

build_figure_markdown <- function(rows) paste(vapply(rows, function(row) paste(
  paste("Figure", row$figure_number, ".", row$figure_title),
  paste0("![Figure ", row$figure_number, ". ", row$figure_title, "](", row$figure_path, ")"),
  paste("Note.", row$figure_note), sep = "\n"), character(1)), collapse = "\n\n")

figure_context <- function(rows) list(tokens = rows[[1L]], narrative_rows = rows)

plot_filename <- function(request, index, settings) {
  parts <- c(request$type, request$x, request$y, request$group, settings$file_suffix)
  slug <- substr(sanitize_file_component(paste(parts[nzchar(parts)], collapse = "-")), 1L, 80L)
  paste0(settings$file_prefix, "-", sprintf("%0*d", settings$figure_digits, index), "-", slug, ".", settings$format)
}

unique_plot_path <- function(relative, out_dir, overwrite, reserved) {
  candidate <- relative; index <- 0L
  while (!overwrite && (file.exists(file.path(out_dir, candidate)) || candidate %in% reserved)) {
    index <- index + 1L
    ext <- tools::file_ext(relative)
    candidate <- paste0(sub(paste0("[.]", ext, "$"), "", relative), "-", index, ".", ext)
  }
  candidate
}

save_plot <- function(built, path, settings) {
  # Use explicit installed R devices, not ggsave's optional ragg/svglite choice.
  bitmap_type <- if (is.null(settings$bitmap_type)) getOption("bitmapType") else settings$bitmap_type
  if (settings$format %in% c("png", "jpeg", "jpg", "tiff", "tif", "bmp")) {
    device <- switch(settings$format, png = grDevices::png, jpeg = grDevices::jpeg, jpg = grDevices::jpeg,
      tiff = grDevices::tiff, tif = grDevices::tiff, bmp = grDevices::bmp)
    device(path, width = settings$width, height = settings$height, units = "in", res = settings$dpi, type = bitmap_type)
  }
  else if (settings$format == "pdf") grDevices::pdf(path, width = settings$width, height = settings$height, onefile = TRUE)
  else if (settings$format == "svg") grDevices::svg(path, width = settings$width, height = settings$height, onefile = TRUE)
  else grDevices::postscript(path, width = settings$width, height = settings$height,
    onefile = settings$format == "ps", horizontal = FALSE, paper = "special")
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  grid::grid.draw(ggplot2::ggplot_gtable(built))
}

# ggplot statistic frames include ragged list columns (e.g. boxplot outliers).
# Keep native frames in RDS and emit explicit JSON rows, without data-frame
# simplification trying to recycle differently sized vectors into a column.
plot_json_table <- function(frame) {
  if (is.null(frame)) return(NULL)
  lapply(seq_len(nrow(frame)), function(i) lapply(frame, function(column) {
    value <- if (is.list(column)) column[[i]] else column[i]
    if (is.factor(value)) as.character(value) else if (is.numeric(value)) unname(as.numeric(value)) else unname(value)
  }))
}

plot_layer_rows <- function(layer) {
  if (!nrow(layer)) return(0L)
  columns <- intersect(c("x", "y", "middle", "lower", "upper"), names(layer))
  if (!length(columns)) return(0L)
  sum(Reduce(`&`, lapply(layer[columns], function(value) is.finite(as.numeric(value)))))
}

main <- function() {
  opts <- nlss_run_options(commandArgs(trailingOnly = TRUE), "plot")
  if (isTRUE(opts$help)) { print_usage(); return(invisible(NULL)) }
  if (isTRUE(opts$interactive)) opts <- interactive_options()
  df <- nlss_load_input(opts)
  out_dir <- get_workspace_out_dir(df)
  nlss_begin_run("plot", df, opts)
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Plotting requires the 'ggplot2' package. Install it: install.packages('ggplot2').")
  settings <- plot_options(opts)
  settings$bitmap_type <- if (is.null(nlss_run_context$replay)) getOption("bitmapType") else nlss_run_context$replay$request$options$bitmap_type
  for (name in c("title", "subtitle", "caption", "note"))
    settings[[name]] <- nlss_mask_prose_paths(settings[[name]], nlss_run_context$root)
  requests <- build_plot_requests(df, settings)
  dictionary <- attr(df, "nlss_import_contract", exact = TRUE)
  if (is.null(dictionary)) dictionary <- read_import_json(nlss_project_file(attr(df, "nlss_dataset_ref")$dictionary_path, nlss_run_context$root))
  labels <- resolve_label_metadata(df)
  prepared <- lapply(requests, function(request) prepare_plot_data(df, request, settings$na_action, dictionary))
  design <- list(engine = "ggplot2", figure_requests = requests,
    variables = lapply(unique(unlist(lapply(prepared, function(info) info$aliases))), function(var)
      list(variable = var, classes = class(df[[var]]), label = resolve_variable_label(labels, var))),
    labels = labels, cases = lapply(prepared, function(info) info$cases),
    categories = lapply(prepared, function(info) info$categories),
    device = list(name = paste0("grDevices::", switch(settings$format, jpg = "jpeg", tif = "tiff", eps = "postscript", ps = "postscript", settings$format)), bitmap_type = settings$bitmap_type,
      cairo = unname(capabilities("cairo")), width = settings$width, height = settings$height, dpi = settings$dpi),
    numbering = list(run_local = "starts_at_one", legacy_start = settings$figure_number),
    image_replay = "Preserved image bytes are verified; numerical layers and Markdown are deterministic in the recorded environment. PDF/SVG device metadata need not be byte-identical.")
  settings$engine <- "ggplot2"
  settings$effective_types <- vapply(requests, function(request) request$type, character(1))
  nlss_resolve_request(settings, design)
  template <- resolve_template_override(opts$template, module = "plot")
  if (is.null(template)) template <- resolve_template_path("plot.default", "plot/default-template.md")
  template <- nlss_freeze_template(template, "plot")
  canonical_start <- if (is.null(settings$figure_number)) get_next_figure_number(file.path(out_dir, "report_canonical.md")) else settings$figure_number
  rows <- legacy_rows <- audits <- list()
  reserved <- character()
  for (i in seq_along(requests)) {
    request <- requests[[i]]; info <- prepared[[i]]
    figure_warnings <- character()
    capture <- function(expr) withCallingHandlers(expr, warning = function(w) {
      figure_warnings <<- c(figure_warnings, conditionMessage(w))
    })
    generated <- capture(build_plot(info, request, settings, labels))
    title <- if (nzchar(settings$title)) settings$title else build_default_title(request, settings$stat, labels)
    caption <- if (nzchar(settings$caption)) settings$caption else title
    generated$plot <- generated$plot + ggplot2::labs(title = title,
      subtitle = if (nzchar(settings$subtitle)) settings$subtitle else NULL,
      caption = if (nzchar(settings$caption)) settings$caption else NULL)
    built <- capture(ggplot2::ggplot_build(generated$plot))
    layers <- lapply(built$data, as.data.frame)
    status <- lapply(seq_along(layers), function(k) list(layer = k,
      geom = class(generated$plot$layers[[k]]$geom)[1L], stat = class(generated$plot$layers[[k]]$stat)[1L],
      status = if (plot_layer_rows(layers[[k]]) > 0L) "available" else "unavailable",
      rows = nrow(layers[[k]]), usable_rows = plot_layer_rows(layers[[k]])))
    if (!length(layers) || !any(vapply(layers, plot_layer_rows, integer(1)) > 0L)) stop("Requested plot has no estimable graphical layer.")
    for (entry in status) if (entry$status == "unavailable")
      capture(warning("Figure ", i, " layer ", entry$layer, " (", entry$stat, ") is unavailable.", call. = FALSE))
    name <- paste0("plots/", plot_filename(request, i, settings))
    staged <- file.path(nlss_run_context$staging, name)
    ensure_out_dir(dirname(staged))
    capture(save_plot(built, staged, settings))
    legacy_name <- unique_plot_path(paste0("plots/", plot_filename(request, canonical_start + i - 1L, settings)),
      out_dir, settings$overwrite, reserved)
    reserved <- c(reserved, legacy_name)
    nlss_save_run_file(name, source = staged, legacy_path = legacy_name, overwrite = settings$overwrite)
    note <- build_figure_note(info, request, settings)
    figure_warnings <- unique(nlss_mask_prose_paths(figure_warnings, nlss_run_context$root))
    if (length(figure_warnings)) note <- paste(note, "Warnings:", paste(figure_warnings, collapse = "; "))
    rows[[i]] <- list(figure_number = i, figure_title = caption, figure_caption = caption,
      figure_note = note, figure_path = name, plot_type = request$type,
      vars = paste(request$vars, collapse = ", "), x = request$x, y = request$y,
      group = request$group, n = info$cases$stats$n, missing_n = info$cases$stats$missing_n,
      missing_pct = info$cases$stats$missing_pct)
    legacy_rows[[i]] <- rows[[i]]
    legacy_rows[[i]]$figure_number <- canonical_start + i - 1L
    legacy_rows[[i]]$figure_path <- legacy_name
    audits[[i]] <- list(request = request, aliases = info$aliases, cases = info$cases,
      categories = info$categories, data = info$data, summary = generated$summary,
      layers = layers, layer_status = status, warnings = figure_warnings,
      labels = list(title = title, subtitle = settings$subtitle, caption = settings$caption,
        x = generated$plot$labels$x, y = generated$plot$labels$y,
        fill = generated$plot$labels$fill, colour = generated$plot$labels$colour))
  }
  nlss_save_run_rds(audits, "plot-data.rds")
  plot_types <- unique(settings$effective_types)
  analysis_flags <- settings[c("engine", "vars", "x", "y", "group", "na_action", "theme", "palette", "width", "height", "dpi")]
  analysis_flags$plot_type <- paste(plot_types, collapse = ", ")
  analysis_flags$output_format <- settings$format
  if (any(!plot_types %in% c("qq", "corr-heatmap"))) analysis_flags$opacity <- settings$alpha
  if ("histogram" %in% plot_types) {
    binning <- if (is.null(settings$binwidth)) "bins" else "binwidth"
    analysis_flags[[binning]] <- settings[[binning]]
  }
  if ("density" %in% plot_types) analysis_flags$bw <- if (is.null(settings$bw)) "nrd0 (automatic)" else settings$bw
  if ("scatter" %in% plot_types) {
    analysis_flags$smooth <- settings$smooth
    if (settings$smooth != "none") analysis_flags$smoother_confidence_band <- settings$se
    if (settings$smooth == "loess") analysis_flags$span <- settings$span
  }
  if ("bar" %in% plot_types) {
    analysis_flags$stat <- settings$stat
    if (settings$stat == "percent") analysis_flags$percent_base <- settings$percent_base
    analysis_flags$position <- settings$position
  }
  if ("line" %in% plot_types) analysis_flags$summary <- settings$summary
  nlss_stage_figure_report(file.path(out_dir, "report_canonical.md"), "Plots", build_figure_markdown(rows),
    analysis_flags = analysis_flags, template_path = template, template_context = figure_context(rows), figure_start = 1L,
    legacy = list(figure_body = build_figure_markdown(legacy_rows), template_context = figure_context(legacy_rows), figure_start = canonical_start))
  json_audits <- lapply(audits, function(audit) {
    audit$data <- plot_json_table(audit$data)
    audit$summary <- plot_json_table(audit$summary)
    audit$layers <- lapply(audit$layers, plot_json_table)
    audit
  })
  nlss_set_result(list(figures = rows, plots = json_audits))
  if (settings$log) {
    context <- get_run_context()
    legacy_options <- settings
    legacy_options$type <- unique(settings$effective_types)
    legacy_options$vars <- if (nzchar(settings$vars)) parse_list(settings$vars) else {
      if (!nzchar(settings$x) && !nzchar(settings$y)) unique(unlist(lapply(requests, function(request) {
        if (length(request$vars)) request$vars else if (request$type %in% c("box", "violin")) request$y else request$x
      }), use.names = FALSE)) else character()
    }
    nlss_stage_log(out_dir, "plot", context$prompt, context$commands,
      results = list(figures = legacy_rows), options = legacy_options, user_prompt = get_user_prompt(opts))
  }
}

nlss_run_main("plot", main)
