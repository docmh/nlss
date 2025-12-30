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
source(file.path(bootstrap_dir, "lib", "paths.R"))
source_lib("cli.R")
source_lib("config.R")
source_lib("io.R")
source_lib("data_utils.R")
source_lib("formatting.R")


# Static analysis aliases for source_lib-defined functions.
render_output_path <- get("render_output_path", mode = "function")
source_lib <- get("source_lib", mode = "function")

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
  input_type <- resolve_prompt("Input type (csv/sav/rds/rdata/parquet)", "csv")
  input_type <- tolower(input_type)
  opts <- list()

  if (input_type == "csv") {
    opts$csv <- resolve_prompt("CSV path")
    sep_default <- resolve_config_value("defaults.csv.sep", ",")
    header_default <- resolve_config_value("defaults.csv.header", TRUE)
    opts$sep <- resolve_prompt("Separator", sep_default)
    opts$header <- resolve_prompt("Header TRUE/FALSE", ifelse(isTRUE(header_default), "TRUE", "FALSE"))
  } else if (input_type == "sav") {
    opts$sav <- resolve_prompt("SAV path")
  } else if (input_type == "rds") {
    opts$rds <- resolve_prompt("RDS path")
  } else if (input_type == "rdata") {
    opts$rdata <- resolve_prompt("RData path")
    opts$df <- resolve_prompt("Data frame object name")
  } else if (input_type == "parquet") {
    opts$parquet <- resolve_prompt("Parquet path")
  } else {
    stop("Unsupported input type.")
  }

  type_default <- resolve_config_value("modules.plot.type", "auto")
  opts$type <- resolve_prompt("Plot type", type_default)
  opts$vars <- resolve_prompt("Variables (comma-separated, blank for defaults)", "")
  opts$x <- resolve_prompt("X variable (blank if using --vars)", "")
  opts$y <- resolve_prompt("Y variable (blank if using --vars)", "")
  opts$group <- resolve_prompt("Group variable (optional)", "")
  opts$stat <- resolve_prompt("Bar stat (count/percent)", resolve_config_value("modules.plot.stat", "count"))
  opts$percent_base <- resolve_prompt("Percent base (total/group)", resolve_config_value("modules.plot.percent_base", "total"))
  opts$bins <- resolve_prompt("Histogram bins", as.character(resolve_config_value("modules.plot.bins", 30)))
  opts$binwidth <- resolve_prompt("Histogram binwidth (blank for default)", "")
  opts$bw <- resolve_prompt("Density bandwidth (blank for default)", "")
  opts$smooth <- resolve_prompt("Smooth (none/loess/lm)", resolve_config_value("modules.plot.smooth", "none"))
  opts$se <- resolve_prompt("Smooth SE TRUE/FALSE", ifelse(isTRUE(resolve_config_value("modules.plot.se", TRUE)), "TRUE", "FALSE"))
  opts$summary <- resolve_prompt("Line summary (none/mean/median)", resolve_config_value("modules.plot.summary", "none"))
  opts$na_action <- resolve_prompt("Missing handling (omit/keep)", resolve_config_value("modules.plot.na_action", "omit"))
  opts$theme <- resolve_prompt("Theme (minimal/classic/bw)", resolve_config_value("modules.plot.theme", "minimal"))
  opts$palette <- resolve_prompt("Palette (default/viridis/greys)", resolve_config_value("modules.plot.palette", "default"))
  opts$title <- resolve_prompt("Figure title (optional)", "")
  opts$subtitle <- resolve_prompt("Figure subtitle (optional)", "")
  opts$caption <- resolve_prompt("Figure caption (optional)", "")
  opts$note <- resolve_prompt("Figure note (optional)", "")
  opts$format <- resolve_prompt("Output format (png/pdf/svg)", resolve_config_value("modules.plot.format", "png"))
  opts$width <- resolve_prompt("Figure width (inches)", as.character(resolve_config_value("modules.plot.width", 7)))
  opts$height <- resolve_prompt("Figure height (inches)", as.character(resolve_config_value("modules.plot.height", 5)))
  opts$dpi <- resolve_prompt("DPI", as.character(resolve_config_value("modules.plot.dpi", 300)))
  opts$file_prefix <- resolve_prompt("Filename prefix", resolve_config_value("modules.plot.file_prefix", "figure"))
  opts$file_suffix <- resolve_prompt("Filename suffix (optional)", "")
  opts$overwrite <- resolve_prompt("Overwrite existing TRUE/FALSE", "FALSE")
  opts$digits <- resolve_prompt("Rounding digits", as.character(resolve_config_value("defaults.digits", 2)))
  opts$template <- resolve_prompt("Template (path or key; blank for default)", "")
  opts$`user-prompt` <- resolve_prompt("User prompt (optional)", "")
  log_default <- resolve_config_value("defaults.log", TRUE)
  opts$log <- resolve_prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))
  opts
}

resolve_prompt <- function(label, default = NULL) {
  if (exists("prompt", mode = "function")) {
    return(get("prompt", mode = "function")(label, default = default))
  }
  if (is.null(default)) {
    answer <- readline(paste0(label, ": "))
  } else {
    answer <- readline(paste0(label, " [", default, "]: "))
    if (answer == "") answer <- default
  }
  answer
}

resolve_config_value <- function(path, default = NULL) {
  if (exists("get_config_value", mode = "function")) {
    return(get("get_config_value", mode = "function")(path, default = default))
  }
  default
}

resolve_parse_args <- function(args) {
  if (exists("parse_args", mode = "function")) {
    return(get("parse_args", mode = "function")(args))
  }
  opts <- list()
  i <- 1
  while (i <= length(args)) {
    arg <- args[i]
    if (grepl("^--", arg)) {
      key <- sub("^--", "", arg)
      if (grepl("=", key)) {
        parts <- strsplit(key, "=", fixed = TRUE)[[1]]
        opts[[parts[1]]] <- parts[2]
      } else if (i < length(args) && !grepl("^--", args[i + 1])) {
        opts[[key]] <- args[i + 1]
        i <- i + 1
      } else {
        opts[[key]] <- TRUE
      }
    }
    i <- i + 1
  }
  opts
}

resolve_parse_bool <- function(value, default = FALSE) {
  if (exists("parse_bool", mode = "function")) {
    return(get("parse_bool", mode = "function")(value, default = default))
  }
  if (is.null(value)) return(default)
  if (is.logical(value)) return(value)
  val <- tolower(as.character(value))
  val %in% c("true", "t", "1", "yes", "y")
}

resolve_parse_list <- function(value, sep = ",") {
  if (exists("parse_list", mode = "function")) {
    return(get("parse_list", mode = "function")(value, sep = sep))
  }
  if (is.null(value) || is.logical(value)) return(character(0))
  value <- as.character(value)
  if (value == "") return(character(0))
  trimws(strsplit(value, sep, fixed = TRUE)[[1]])
}

resolve_parse_number <- function(value, default = NULL) {
  if (is.null(value)) return(default)
  if (is.numeric(value)) return(as.numeric(value))
  val <- as.character(value)
  if (!nzchar(val)) return(default)
  num <- suppressWarnings(as.numeric(val))
  if (is.na(num)) return(default)
  num
}

resolve_parse_integer <- function(value, default = NULL) {
  num <- resolve_parse_number(value, default = default)
  if (is.null(num) || is.na(num)) return(default)
  as.integer(round(num))
}

resolve_ensure_out_dir <- function(path) {
  if (exists("ensure_out_dir", mode = "function")) {
    return(get("ensure_out_dir", mode = "function")(path))
  }
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  path
}

resolve_load_dataframe <- function(opts) {
  if (exists("load_dataframe", mode = "function")) {
    return(get("load_dataframe", mode = "function")(opts))
  }
  stop("Missing load_dataframe. Ensure lib/io.R is sourced.")
}

resolve_get_workspace_out_dir <- function(df) {
  if (exists("get_workspace_out_dir", mode = "function")) {
    return(get("get_workspace_out_dir", mode = "function")(df))
  }
  stop("Missing get_workspace_out_dir. Ensure lib/io.R is sourced.")
}

resolve_select_variables <- function(df, vars, group_var = NULL, default = "numeric", include_numeric = FALSE) {
  if (exists("select_variables", mode = "function")) {
    return(get("select_variables", mode = "function")(
      df,
      vars,
      group_var = group_var,
      default = default,
      include_numeric = include_numeric
    ))
  }
  available <- names(df)
  if (is.null(vars) || vars == "") {
    if (default == "all") {
      selected <- available
    } else if (default == "non-numeric") {
      if (include_numeric) {
        selected <- available
      } else {
        selected <- available[!sapply(df, is.numeric)]
        if (length(selected) == 0) selected <- available
      }
    } else {
      selected <- available[sapply(df, is.numeric)]
    }
    if (!is.null(group_var)) selected <- setdiff(selected, group_var)
    return(selected)
  }
  requested <- trimws(strsplit(vars, ",", fixed = TRUE)[[1]])
  missing <- setdiff(requested, available)
  if (length(missing) > 0) {
    stop(paste("Unknown variables:", paste(missing, collapse = ", ")))
  }
  if (!is.null(group_var)) requested <- setdiff(requested, group_var)
  requested
}

resolve_get_template_path <- function(key, default_relative = NULL) {
  if (exists("resolve_template_path", mode = "function")) {
    return(get("resolve_template_path", mode = "function")(key, default_relative))
  }
  NULL
}

resolve_get_template_meta <- function(path) {
  if (exists("get_template_meta", mode = "function")) {
    return(get("get_template_meta", mode = "function")(path))
  }
  list()
}

resolve_template_override <- local({
  override_impl <- NULL
  if (exists("resolve_template_override", mode = "function")) {
    override_impl <- get("resolve_template_override", mode = "function")
  }
  function(template_ref, module = NULL) {
    if (!is.null(override_impl)) {
      return(override_impl(template_ref, module = module))
    }
    NULL
  }
})

resolve_append_nlss_figure_report <- function(path, analysis_label, figure_body, analysis_flags = NULL, template_path = NULL, template_context = NULL, figure_start = NULL) {
  if (exists("append_nlss_figure_report", mode = "function")) {
    return(get("append_nlss_figure_report", mode = "function")(
      path,
      analysis_label,
      figure_body,
      analysis_flags = analysis_flags,
      template_path = template_path,
      template_context = template_context,
      figure_start = figure_start
    ))
  }
  stop("Missing report formatter. Ensure lib/formatting.R is sourced.")
}

resolve_get_run_context <- function() {
  if (exists("get_run_context", mode = "function")) {
    return(get("get_run_context", mode = "function")())
  }
  trailing <- commandArgs(trailingOnly = TRUE)
  commands <- c("Rscript", trailing)
  commands <- commands[nzchar(commands)]
  prompt <- paste(commands, collapse = " ")
  list(prompt = prompt, commands = commands)
}

resolve_append_analysis_log <- function(out_dir, module, prompt, commands, results, options = list(), user_prompt = NULL) {
  if (exists("append_analysis_log", mode = "function")) {
    return(get("append_analysis_log", mode = "function")(
      out_dir,
      module,
      prompt,
      commands,
      results,
      options = options,
      user_prompt = user_prompt
    ))
  }
  cat("Note: append_analysis_log not available; skipping analysis_log.jsonl output.\n")
  invisible(FALSE)
}

resolve_get_user_prompt <- function(opts) {
  if (exists("get_user_prompt", mode = "function")) {
    return(get("get_user_prompt", mode = "function")(opts))
  }
  NULL
}

resolve_sanitize_file_component <- function(value) {
  if (exists("sanitize_file_component", mode = "function")) {
    return(get("sanitize_file_component", mode = "function")(value))
  }
  clean <- enc2utf8(as.character(value))
  clean <- gsub("[^A-Za-z0-9._-]", "_", clean)
  clean <- gsub("_+", "_", clean)
  if (!nzchar(clean)) clean <- "figure"
  clean
}

require_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Plotting requires the 'ggplot2' package. Install it: install.packages('ggplot2').")
  }
}

add_plot_component <- function(plot, component) {
  if (is.null(component)) return(plot)
  ggplot2::ggplot_add(component, plot)
}

add_plot_layers <- function(plot, ...) {
  layers <- list(...)
  for (layer in layers) {
    plot <- add_plot_component(plot, layer)
  }
  plot
}

resolve_plot_engine <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return("base")
  ok <- TRUE
  suppressWarnings(tryCatch({
    df <- data.frame(x = c(1, 2), y = c(1, 2))
    x <- y <- NULL
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y))
    p <- p + ggplot2::geom_point()
  }, error = function(e) {
    ok <<- FALSE
  }))
  if (ok) return("ggplot2")
  "base"
}

open_graphics_device <- function(path, format, width, height, dpi) {
  fmt <- tolower(format)
  if (fmt == "png") {
    grDevices::png(path, width = width, height = height, units = "in", res = dpi)
    return(invisible(TRUE))
  }
  if (fmt == "pdf") {
    grDevices::pdf(path, width = width, height = height)
    return(invisible(TRUE))
  }
  if (fmt == "svg") {
    grDevices::svg(path, width = width, height = height)
    return(invisible(TRUE))
  }
  stop("Unsupported output format: ", format)
}

get_palette_colors <- function(n, palette) {
  if (n <= 0) return(character(0))
  pal <- tolower(palette)
  if (pal == "viridis" && requireNamespace("viridisLite", quietly = TRUE)) {
    return(viridisLite::viridis(n))
  }
  if (pal == "greys") return(grDevices::gray.colors(n))
  grDevices::hcl.colors(n, "Set2")
}

draw_violin_base <- function(values_list, positions, colors, alpha, ylab) {
  n <- length(values_list)
  if (n == 0) return(invisible(NULL))
  all_values <- unlist(values_list)
  if (length(all_values) == 0) return(invisible(NULL))
  ylim <- range(all_values, na.rm = TRUE)
  graphics::plot(
    NA,
    xlim = c(0.5, n + 0.5),
    ylim = ylim,
    xaxt = "n",
    xlab = "",
    ylab = ylab
  )
  graphics::axis(1, at = positions, labels = names(values_list))
  for (i in seq_along(values_list)) {
    vals <- values_list[[i]]
    vals <- vals[is.finite(vals)]
    if (length(vals) < 2) next
    dens <- stats::density(vals, na.rm = TRUE)
    dens$y <- dens$y / max(dens$y) * 0.4
    col <- grDevices::adjustcolor(colors[i], alpha.f = alpha)
    graphics::polygon(
      c(positions[i] - dens$y, rev(positions[i] + dens$y)),
      c(dens$x, rev(dens$x)),
      col = col,
      border = colors[i]
    )
  }
  graphics::boxplot(values_list, add = TRUE, at = positions, boxwex = 0.15, outline = FALSE)
}

build_plot_base <- function(df, request, opts, digits) {
  type <- request$type
  x <- request$x
  y <- request$y
  group <- request$group
  vars <- request$vars
  na_action <- opts$na_action
  alpha <- opts$alpha
  stat <- opts$stat
  percent_base <- opts$percent_base
  palette <- opts$palette

  if (type == "corr-heatmap") {
    num_vars <- vars
    non_numeric <- num_vars[!vapply(num_vars, function(var) is_numeric_column(df, var), logical(1))]
    if (length(non_numeric) > 0) {
      stop("corr-heatmap requires numeric variables. Non-numeric: ", paste(non_numeric, collapse = ", "))
    }
    data_info <- prepare_plot_data(df, num_vars, character(0), na_action)
    plot_df <- data_info$df
    stats <- data_info$stats
    draw <- function(file_path, format, width, height, dpi, title, subtitle) {
      open_graphics_device(file_path, format, width, height, dpi)
      on.exit(grDevices::dev.off(), add = TRUE)
      corr <- stats::cor(plot_df[, num_vars, drop = FALSE], use = "pairwise.complete.obs")
      n <- ncol(corr)
      cols <- grDevices::colorRampPalette(c("#2166AC", "white", "#B2182B"))(100)
      graphics::par(mar = c(6, 6, 3, 2))
      graphics::image(1:n, 1:n, t(corr[n:1, ]), col = cols, axes = FALSE, xlab = "", ylab = "", main = title)
      graphics::axis(1, at = 1:n, labels = colnames(corr), las = 2)
      graphics::axis(2, at = 1:n, labels = rev(colnames(corr)), las = 2)
      for (i in seq_len(n)) {
        for (j in seq_len(n)) {
          label <- format(round(corr[j, i], digits), nsmall = digits)
          graphics::text(i, n - j + 1, labels = label, cex = 0.8)
        }
      }
      if (!is.null(subtitle) && nzchar(subtitle)) {
        graphics::mtext(subtitle, side = 3, line = 0.2, cex = 0.8)
      }
    }
    return(list(engine = "base", stats = stats, draw = draw))
  }

  if (type == "scatter" || type == "line") {
    numeric_vars <- c(x, y)
    categorical_vars <- if (nzchar(group)) group else character(0)
    data_info <- prepare_plot_data(df, numeric_vars, categorical_vars, na_action)
    plot_df <- data_info$df
    stats <- data_info$stats
    if (!is_numeric_column(df, x) || !is_numeric_column(df, y)) {
      stop("Scatter/line plots require numeric --x and --y.")
    }
    if (nzchar(group) && group %in% names(plot_df)) {
      plot_df[[group]] <- coerce_categorical(plot_df[[group]])
    }
    draw <- function(file_path, format, width, height, dpi, title, subtitle) {
      open_graphics_device(file_path, format, width, height, dpi)
      on.exit(grDevices::dev.off(), add = TRUE)
      graphics::par(mar = c(5, 5, 3, 2))
      if (type == "scatter") {
        if (nzchar(group)) {
          groups <- factor(plot_df[[group]])
          colors <- get_palette_colors(length(levels(groups)), palette)
          colors <- grDevices::adjustcolor(colors, alpha.f = alpha)
          graphics::plot(plot_df[[x]], plot_df[[y]], col = colors[groups], pch = 19, xlab = x, ylab = y, main = title)
          graphics::legend("topright", legend = levels(groups), col = colors, pch = 19, bty = "n")
        } else {
          graphics::plot(plot_df[[x]], plot_df[[y]], pch = 19, xlab = x, ylab = y, main = title)
        }
        if (opts$smooth != "none") {
          if (opts$smooth == "lm") {
            fit <- stats::lm(plot_df[[y]] ~ plot_df[[x]])
            graphics::abline(fit, col = "red")
          } else {
            fit <- stats::loess(plot_df[[y]] ~ plot_df[[x]], span = opts$span)
            ord <- order(plot_df[[x]])
            graphics::lines(plot_df[[x]][ord], fit$fitted[ord], col = "red")
          }
        }
      } else {
        if (opts$summary != "none") {
          summary_fun <- if (opts$summary == "median") stats::median else mean
          if (nzchar(group)) {
            agg <- aggregate(plot_df[[y]], by = list(plot_df[[x]], plot_df[[group]]), FUN = summary_fun, na.rm = TRUE)
            names(agg) <- c(x, group, y)
            groups <- factor(agg[[group]])
            colors <- get_palette_colors(length(levels(groups)), palette)
            colors <- grDevices::adjustcolor(colors, alpha.f = alpha)
            graphics::plot(NA, xlim = range(agg[[x]], na.rm = TRUE), ylim = range(agg[[y]], na.rm = TRUE), xlab = x, ylab = y, main = title)
            for (i in seq_along(levels(groups))) {
              subset <- agg[groups == levels(groups)[i], , drop = FALSE]
              ord <- order(subset[[x]])
              graphics::lines(subset[[x]][ord], subset[[y]][ord], col = colors[i])
            }
            graphics::legend("topright", legend = levels(groups), col = colors, lty = 1, bty = "n")
          } else {
            agg <- aggregate(plot_df[[y]], by = list(plot_df[[x]]), FUN = summary_fun, na.rm = TRUE)
            names(agg) <- c(x, y)
            ord <- order(agg[[x]])
            graphics::plot(agg[[x]][ord], agg[[y]][ord], type = "l", xlab = x, ylab = y, main = title)
          }
        } else {
          ord <- order(plot_df[[x]])
          graphics::plot(plot_df[[x]][ord], plot_df[[y]][ord], type = "l", xlab = x, ylab = y, main = title)
        }
      }
      if (!is.null(subtitle) && nzchar(subtitle)) {
        graphics::mtext(subtitle, side = 3, line = 0.2, cex = 0.8)
      }
    }
    return(list(engine = "base", stats = stats, draw = draw))
  }

  if (type == "box" || type == "violin") {
    if (!nzchar(x) || !nzchar(y)) stop("Box/violin plots require --y and --group/--x.")
    numeric_vars <- c(y)
    categorical_vars <- unique(c(x, if (nzchar(group)) group else character(0)))
    data_info <- prepare_plot_data(df, numeric_vars, categorical_vars, na_action)
    plot_df <- data_info$df
    stats <- data_info$stats
    plot_df[[x]] <- coerce_categorical(plot_df[[x]])
    draw <- function(file_path, format, width, height, dpi, title, subtitle) {
      open_graphics_device(file_path, format, width, height, dpi)
      on.exit(grDevices::dev.off(), add = TRUE)
      graphics::par(mar = c(5, 5, 3, 2))
      groups <- factor(plot_df[[x]])
      colors <- get_palette_colors(length(levels(groups)), palette)
      colors <- grDevices::adjustcolor(colors, alpha.f = alpha)
      if (type == "box") {
        graphics::boxplot(plot_df[[y]] ~ groups, col = colors, xlab = x, ylab = y, main = title)
      } else {
        values_list <- split(plot_df[[y]], groups)
        positions <- seq_along(values_list)
        draw_violin_base(values_list, positions, colors, alpha, y)
        graphics::title(main = title)
      }
      if (!is.null(subtitle) && nzchar(subtitle)) {
        graphics::mtext(subtitle, side = 3, line = 0.2, cex = 0.8)
      }
    }
    return(list(engine = "base", stats = stats, draw = draw))
  }

  if (type == "bar") {
    if (!nzchar(x)) stop("Bar plots require --vars or --x.")
    numeric_vars <- character(0)
    categorical_vars <- unique(c(x, if (nzchar(group)) group else character(0)))
    data_info <- prepare_plot_data(df, numeric_vars, categorical_vars, na_action)
    plot_df <- data_info$df
    stats <- data_info$stats
    plot_df[[x]] <- coerce_categorical(plot_df[[x]])
    if (nzchar(group) && group %in% names(plot_df)) {
      plot_df[[group]] <- coerce_categorical(plot_df[[group]])
    }
    draw <- function(file_path, format, width, height, dpi, title, subtitle) {
      open_graphics_device(file_path, format, width, height, dpi)
      on.exit(grDevices::dev.off(), add = TRUE)
      graphics::par(mar = c(6, 5, 3, 2))
      if (stat == "percent") {
        if (nzchar(group)) {
          tab <- table(plot_df[[x]], plot_df[[group]])
          if (percent_base == "group") {
            tab <- apply(tab, 2, function(z) if (sum(z) == 0) z else 100 * z / sum(z))
          } else {
            total <- sum(tab)
            tab <- if (total == 0) tab else 100 * tab / total
          }
        } else {
          tab <- table(plot_df[[x]])
          total <- sum(tab)
          tab <- if (total == 0) tab else 100 * tab / total
        }
        ylab <- "Percent"
      } else {
        tab <- if (nzchar(group)) table(plot_df[[x]], plot_df[[group]]) else table(plot_df[[x]])
        ylab <- "Count"
      }
      if (nzchar(group)) {
        colors <- get_palette_colors(ncol(tab), palette)
        colors <- grDevices::adjustcolor(colors, alpha.f = alpha)
        beside <- opts$position == "dodge"
        graphics::barplot(tab, beside = beside, col = colors, legend.text = colnames(tab), xlab = x, ylab = ylab, main = title)
      } else {
        colors <- get_palette_colors(length(tab), palette)
        colors <- grDevices::adjustcolor(colors, alpha.f = alpha)
        graphics::barplot(tab, col = colors, xlab = x, ylab = ylab, main = title)
      }
      if (!is.null(subtitle) && nzchar(subtitle)) {
        graphics::mtext(subtitle, side = 3, line = 0.2, cex = 0.8)
      }
    }
    return(list(engine = "base", stats = stats, draw = draw))
  }

  if (type == "histogram" || type == "density" || type == "qq") {
    if (!nzchar(x)) stop("Histogram/density/qq plots require --vars or --x.")
    numeric_vars <- c(x)
    categorical_vars <- if (nzchar(group)) group else character(0)
    data_info <- prepare_plot_data(df, numeric_vars, categorical_vars, na_action)
    plot_df <- data_info$df
    stats <- data_info$stats
    if (!is_numeric_column(df, x)) stop("Histogram/density/qq plots require numeric variables.")
    if (nzchar(group) && group %in% names(plot_df)) {
      plot_df[[group]] <- coerce_categorical(plot_df[[group]])
    }
    draw <- function(file_path, format, width, height, dpi, title, subtitle) {
      open_graphics_device(file_path, format, width, height, dpi)
      on.exit(grDevices::dev.off(), add = TRUE)
      graphics::par(mar = c(5, 5, 3, 2))
      if (type == "histogram") {
        breaks <- NULL
        values <- plot_df[[x]]
        if (!is.null(opts$binwidth)) {
          range_vals <- range(values, na.rm = TRUE)
          breaks <- seq(range_vals[1], range_vals[2] + opts$binwidth, by = opts$binwidth)
        }
        if (nzchar(group)) {
          groups <- factor(plot_df[[group]])
          colors <- get_palette_colors(length(levels(groups)), palette)
          colors <- grDevices::adjustcolor(colors, alpha.f = alpha)
          if (is.null(breaks)) {
            graphics::hist(values[groups == levels(groups)[1]], col = colors[1], border = "white", xlab = x, main = title)
          } else {
            graphics::hist(values[groups == levels(groups)[1]], breaks = breaks, col = colors[1], border = "white", xlab = x, main = title)
          }
          if (length(levels(groups)) > 1) {
            for (i in 2:length(levels(groups))) {
              if (is.null(breaks)) {
                graphics::hist(values[groups == levels(groups)[i]], col = colors[i], border = "white", add = TRUE)
              } else {
                graphics::hist(values[groups == levels(groups)[i]], breaks = breaks, col = colors[i], border = "white", add = TRUE)
              }
            }
            graphics::legend("topright", legend = levels(groups), fill = colors, bty = "n")
          }
        } else {
          if (is.null(breaks)) {
            graphics::hist(values, col = grDevices::adjustcolor("#4C72B0", alpha.f = alpha), border = "white", xlab = x, main = title)
          } else {
            graphics::hist(values, breaks = breaks, col = grDevices::adjustcolor("#4C72B0", alpha.f = alpha), border = "white", xlab = x, main = title)
          }
        }
      } else if (type == "density") {
        values <- plot_df[[x]]
        if (nzchar(group)) {
          groups <- factor(plot_df[[group]])
          colors <- get_palette_colors(length(levels(groups)), palette)
          colors <- grDevices::adjustcolor(colors, alpha.f = alpha)
          dens <- stats::density(values[groups == levels(groups)[1]], bw = opts$bw)
          graphics::plot(dens, col = colors[1], main = title, xlab = x)
          if (length(levels(groups)) > 1) {
            for (i in 2:length(levels(groups))) {
              dens <- stats::density(values[groups == levels(groups)[i]], bw = opts$bw)
              graphics::lines(dens, col = colors[i])
            }
            graphics::legend("topright", legend = levels(groups), col = colors, lty = 1, bty = "n")
          }
        } else {
          dens <- stats::density(values, bw = opts$bw)
          graphics::plot(dens, col = grDevices::adjustcolor("#4C72B0", alpha.f = alpha), main = title, xlab = x)
        }
      } else {
        stats::qqnorm(plot_df[[x]], main = title)
        stats::qqline(plot_df[[x]], col = "red")
      }
      if (!is.null(subtitle) && nzchar(subtitle)) {
        graphics::mtext(subtitle, side = 3, line = 0.2, cex = 0.8)
      }
    }
    return(list(engine = "base", stats = stats, draw = draw))
  }

  stop("Unsupported plot type: ", type)
}

normalize_plot_type <- function(value) {
  if (is.null(value) || !nzchar(value)) return("auto")
  type <- tolower(trimws(as.character(value)))
  type <- gsub("_", "-", type)
  if (type %in% c("hist", "histogram")) return("histogram")
  if (type %in% c("dens", "density")) return("density")
  if (type %in% c("boxplot", "box")) return("box")
  if (type %in% c("violin", "violinplot")) return("violin")
  if (type %in% c("bar", "bars")) return("bar")
  if (type %in% c("scatter", "scatterplot")) return("scatter")
  if (type %in% c("line", "lineplot")) return("line")
  if (type %in% c("qq", "qqplot", "qq-plot")) return("qq")
  if (type %in% c("corr", "correlation", "corr-heatmap", "correlation-heatmap", "heatmap")) return("corr-heatmap")
  type
}

is_numeric_column <- function(df, var) {
  if (is.null(var) || !nzchar(var)) return(FALSE)
  if (!var %in% names(df)) return(FALSE)
  is.numeric(df[[var]])
}

infer_plot_type <- function(df, x, y, vars, group) {
  if (nzchar(x) && nzchar(y)) {
    if (is_numeric_column(df, x) && is_numeric_column(df, y)) return("scatter")
    if (!is_numeric_column(df, x) && is_numeric_column(df, y)) return("box")
    return("bar")
  }
  if (length(vars) > 0) {
    if (!is_numeric_column(df, vars[1])) return("bar")
    if (nzchar(group)) return("box")
    return("histogram")
  }
  "histogram"
}

coerce_categorical <- function(vec) {
  if (is.factor(vec)) return(vec)
  factor(vec, exclude = NULL)
}

replace_missing_category <- function(vec) {
  if (is.factor(vec)) {
    levels(vec) <- unique(c(levels(vec), "Missing"))
    vec[is.na(vec)] <- "Missing"
    return(vec)
  }
  out <- as.character(vec)
  out[is.na(out)] <- "Missing"
  out
}

prepare_plot_data <- function(df, numeric_vars, categorical_vars, na_action) {
  total_n <- nrow(df)
  if (total_n == 0) {
    return(list(df = df, stats = list(
      total_n = 0,
      n = 0,
      missing_n = 0,
      missing_pct = 0,
      missing_kept_n = 0
    )))
  }
  missing_num <- rep(FALSE, total_n)
  for (var in numeric_vars) {
    if (var %in% names(df)) missing_num <- missing_num | is.na(df[[var]])
  }
  missing_cat <- rep(FALSE, total_n)
  for (var in categorical_vars) {
    if (var %in% names(df)) missing_cat <- missing_cat | is.na(df[[var]])
  }
  na_action <- if (nzchar(na_action)) tolower(na_action) else "omit"
  missing_kept_n <- if (na_action == "keep") sum(!missing_num & missing_cat) else 0
  missing_dropped_n <- if (na_action == "keep") sum(missing_num) else sum(missing_num | missing_cat)

  df_plot <- df
  if (na_action == "keep" && length(categorical_vars) > 0) {
    for (var in categorical_vars) {
      if (var %in% names(df_plot)) {
        df_plot[[var]] <- replace_missing_category(df_plot[[var]])
      }
    }
  }
  vars_required <- unique(c(numeric_vars, categorical_vars))
  if (length(vars_required) > 0) {
    complete_rows <- complete.cases(df_plot[, vars_required, drop = FALSE])
    df_plot <- df_plot[complete_rows, , drop = FALSE]
  }
  n_used <- nrow(df_plot)
  missing_pct <- if (total_n > 0) (missing_dropped_n / total_n) * 100 else 0
  list(df = df_plot, stats = list(
    total_n = total_n,
    n = n_used,
    missing_n = missing_dropped_n,
    missing_pct = missing_pct,
    missing_kept_n = missing_kept_n
  ))
}

apply_plot_theme <- function(plot, theme_name) {
  theme_name <- tolower(theme_name)
  if (theme_name == "classic") return(add_plot_component(plot, ggplot2::theme_classic()))
  if (theme_name == "bw") return(add_plot_component(plot, ggplot2::theme_bw()))
  add_plot_component(plot, ggplot2::theme_minimal())
}

apply_plot_palette <- function(plot, palette, use_fill = FALSE, use_color = FALSE) {
  palette <- tolower(palette)
  if (palette == "viridis" && requireNamespace("viridisLite", quietly = TRUE)) {
    if (use_fill) plot <- add_plot_component(plot, ggplot2::scale_fill_viridis_d())
    if (use_color) plot <- add_plot_component(plot, ggplot2::scale_color_viridis_d())
    return(plot)
  }
  if (palette == "greys") {
    if (use_fill) plot <- add_plot_component(plot, ggplot2::scale_fill_grey())
    if (use_color) plot <- add_plot_component(plot, ggplot2::scale_color_grey())
  }
  plot
}

build_default_title <- function(type, x, y, stat) {
  if (type == "histogram") return(paste("Distribution of", x))
  if (type == "density") return(paste("Density of", x))
  if (type == "qq") return(paste("Q-Q Plot of", x))
  if (type == "bar") {
    if (stat == "percent") return(paste("Percent of", x))
    return(paste("Counts of", x))
  }
  if (type == "box") return(paste(y, "by", x))
  if (type == "violin") return(paste(y, "by", x))
  if (type == "scatter") return(paste(y, "vs", x))
  if (type == "line") return(paste(y, "over", x))
  if (type == "corr-heatmap") return("Correlation heatmap")
  "Plot"
}

build_figure_note <- function(stats, na_action, note_override, digits) {
  if (!is.null(note_override) && nzchar(note_override)) return(note_override)
  if (is.null(stats) || is.null(stats$total_n)) return("None.")
  if (stats$total_n == 0) return("No observations available.")
  na_action <- if (nzchar(na_action)) tolower(na_action) else "omit"
  if (na_action == "keep" && stats$missing_kept_n > 0 && stats$missing_n == 0) {
    return(paste("Missing values shown as 'Missing' (n =", stats$missing_kept_n, ")."))
  }
  if (stats$missing_n > 0) {
    pct <- round(stats$missing_pct, digits)
    return(paste("Missing values omitted (n =", stats$missing_n, ",", pct, "%)."))
  }
  if (na_action == "keep" && stats$missing_kept_n > 0) {
    return(paste("Missing values shown as 'Missing' (n =", stats$missing_kept_n, ")."))
  }
  "None."
}

build_figure_markdown <- function(figure_number, title, path, note) {
  lines <- c(
    paste("Figure", figure_number, ".", title),
    paste0("![Figure ", figure_number, ". ", title, "](", path, ")"),
    paste("Note.", note)
  )
  paste(lines, collapse = "\n")
}

build_slug <- function(type, x, y, group, suffix) {
  parts <- c(type, x, y, group)
  parts <- parts[nzchar(parts)]
  if (!is.null(suffix) && nzchar(suffix)) parts <- c(parts, suffix)
  slug <- resolve_sanitize_file_component(paste(parts, collapse = "-"))
  if (nchar(slug) > 80) slug <- substr(slug, 1, 80)
  slug
}

ensure_unique_path <- function(path, overwrite = FALSE) {
  if (overwrite || !file.exists(path)) return(path)
  ext <- tools::file_ext(path)
  base <- sub(paste0("\\.", ext, "$"), "", path)
  idx <- 1
  repeat {
    candidate <- paste0(base, "-", idx, ".", ext)
    if (!file.exists(candidate)) return(candidate)
    idx <- idx + 1
  }
}

build_plot_requests <- function(df, type, vars, x, y, group) {
  requests <- list()
  if (type == "corr-heatmap") {
    if (length(vars) < 2) stop("corr-heatmap requires at least two numeric variables.")
    requests[[1]] <- list(type = type, vars = vars, x = "", y = "", group = "")
    return(requests)
  }

  if (nzchar(x) && nzchar(y)) {
    requests[[1]] <- list(type = type, x = x, y = y, group = group, vars = character(0))
    return(requests)
  }

  if (length(vars) == 0) {
    stop("No variables available for plotting.")
  }

  for (var in vars) {
    if (type %in% c("box", "violin")) {
      if (!nzchar(group) && !nzchar(x)) {
        stop("Box and violin plots require --group or --x for the categorical axis.")
      }
      plot_x <- if (nzchar(x)) x else group
      requests[[length(requests) + 1]] <- list(type = type, x = plot_x, y = var, group = group, vars = character(0))
    } else {
      requests[[length(requests) + 1]] <- list(type = type, x = var, y = "", group = group, vars = character(0))
    }
  }
  requests
}

build_plot <- function(df, request, opts, digits, engine = "ggplot2") {
  if (engine == "base") {
    return(build_plot_base(df, request, opts, digits))
  }
  type <- request$type
  x <- request$x
  y <- request$y
  group <- request$group
  vars <- request$vars
  na_action <- opts$na_action
  alpha <- opts$alpha
  stat <- opts$stat
  percent_base <- opts$percent_base

  if (type == "corr-heatmap") {
    num_vars <- vars
    non_numeric <- num_vars[!vapply(num_vars, function(var) is_numeric_column(df, var), logical(1))]
    if (length(non_numeric) > 0) {
      stop("corr-heatmap requires numeric variables. Non-numeric: ", paste(non_numeric, collapse = ", "))
    }
    numeric_vars <- num_vars
    data_info <- prepare_plot_data(df, numeric_vars, character(0), na_action)
    plot_df <- data_info$df
    if (ncol(plot_df) < 2) stop("corr-heatmap requires at least two numeric variables.")
    corr <- stats::cor(plot_df[, num_vars, drop = FALSE], use = "pairwise.complete.obs")
    corr_df <- as.data.frame(as.table(corr), stringsAsFactors = FALSE)
    names(corr_df) <- c("var1", "var2", "r")
    corr_df$r_label <- format(round(corr_df$r, digits), nsmall = digits)
    plot <- ggplot2::ggplot(corr_df, ggplot2::aes_string(x = "var1", y = "var2", fill = "r"))
    plot <- add_plot_layers(
      plot,
      ggplot2::geom_tile(color = "white"),
      ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0),
      ggplot2::geom_text(ggplot2::aes_string(label = "r_label"), size = 3),
      ggplot2::labs(x = NULL, y = NULL, fill = "r"),
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    )
    return(list(plot = plot, stats = data_info$stats))
  }

  if (type == "scatter" || type == "line") {
    numeric_vars <- c(x, y)
    categorical_vars <- if (nzchar(group)) group else character(0)
    data_info <- prepare_plot_data(df, numeric_vars, categorical_vars, na_action)
    plot_df <- data_info$df
    if (!is_numeric_column(df, x) || !is_numeric_column(df, y)) {
      stop("Scatter/line plots require numeric --x and --y.")
    }
    if (nzchar(group) && group %in% names(plot_df)) {
      plot_df[[group]] <- coerce_categorical(plot_df[[group]])
    }
    aes_args <- list(x = x, y = y)
    if (nzchar(group)) aes_args$color <- group
    plot <- ggplot2::ggplot(plot_df, do.call(ggplot2::aes_string, aes_args))
    if (type == "scatter") {
      plot <- add_plot_component(plot, ggplot2::geom_point(alpha = alpha))
      if (opts$smooth != "none") {
        plot <- add_plot_component(plot, ggplot2::geom_smooth(method = opts$smooth, se = opts$se, span = opts$span))
      }
    } else {
      if (opts$summary != "none") {
        summary_fun <- if (opts$summary == "median") stats::median else mean
        if (nzchar(group)) {
          plot_df <- aggregate(plot_df[[y]], by = list(plot_df[[x]], plot_df[[group]]), FUN = summary_fun, na.rm = TRUE)
          names(plot_df) <- c(x, group, y)
        } else {
          plot_df <- aggregate(plot_df[[y]], by = list(plot_df[[x]]), FUN = summary_fun, na.rm = TRUE)
          names(plot_df) <- c(x, y)
        }
        plot <- ggplot2::ggplot(plot_df, do.call(ggplot2::aes_string, aes_args))
        plot <- add_plot_component(plot, ggplot2::geom_line())
      } else {
        plot <- add_plot_component(plot, ggplot2::geom_line())
      }
    }
    plot <- apply_plot_palette(plot, opts$palette, use_color = nzchar(group))
    return(list(plot = plot, stats = data_info$stats))
  }

  if (type == "box" || type == "violin") {
    if (!nzchar(x) || !nzchar(y)) stop("Box/violin plots require --y and --group/--x.")
    numeric_vars <- c(y)
    categorical_vars <- unique(c(x, if (nzchar(group)) group else character(0)))
    data_info <- prepare_plot_data(df, numeric_vars, categorical_vars, na_action)
    plot_df <- data_info$df
    plot_df[[x]] <- coerce_categorical(plot_df[[x]])
    if (nzchar(group) && group %in% names(plot_df)) {
      plot_df[[group]] <- coerce_categorical(plot_df[[group]])
    }
    aes_args <- list(x = x, y = y)
    fill_var <- if (nzchar(group) && group != x) group else x
    aes_args$fill <- fill_var
    plot <- ggplot2::ggplot(plot_df, do.call(ggplot2::aes_string, aes_args))
    if (type == "box") {
      plot <- add_plot_component(plot, ggplot2::geom_boxplot(alpha = alpha))
    } else {
      plot <- add_plot_component(plot, ggplot2::geom_violin(alpha = alpha, trim = FALSE))
      plot <- add_plot_component(plot, ggplot2::geom_boxplot(width = 0.2, alpha = alpha))
    }
    plot <- apply_plot_palette(plot, opts$palette, use_fill = TRUE)
    return(list(plot = plot, stats = data_info$stats))
  }

  if (type == "bar") {
    if (!nzchar(x)) stop("Bar plots require --vars or --x.")
    numeric_vars <- character(0)
    categorical_vars <- unique(c(x, if (nzchar(group)) group else character(0)))
    data_info <- prepare_plot_data(df, numeric_vars, categorical_vars, na_action)
    plot_df <- data_info$df
    plot_df[[x]] <- coerce_categorical(plot_df[[x]])
    if (nzchar(group) && group %in% names(plot_df)) {
      plot_df[[group]] <- coerce_categorical(plot_df[[group]])
    }
    if (stat == "percent") {
      tab <- if (nzchar(group)) {
        counts <- as.data.frame(table(x = plot_df[[x]], group = plot_df[[group]], useNA = "no"))
        names(counts) <- c(x, group, "n")
        if (percent_base == "group") {
          counts$pct <- ave(counts$n, counts[[group]], FUN = function(z) if (sum(z) == 0) 0 else 100 * z / sum(z))
        } else {
          total <- sum(counts$n)
          counts$pct <- if (total == 0) 0 else 100 * counts$n / total
        }
        counts
      } else {
        counts <- as.data.frame(table(x = plot_df[[x]], useNA = "no"))
        names(counts) <- c(x, "n")
        total <- sum(counts$n)
        counts$pct <- if (total == 0) 0 else 100 * counts$n / total
        counts
      }
      aes_args <- list(x = x, y = "pct")
      if (nzchar(group)) aes_args$fill <- group
      plot <- ggplot2::ggplot(tab, do.call(ggplot2::aes_string, aes_args))
      plot <- add_plot_layers(
        plot,
        ggplot2::geom_col(position = opts$position),
        ggplot2::labs(y = "Percent")
      )
    } else {
      aes_args <- list(x = x)
      if (nzchar(group)) aes_args$fill <- group
      plot <- ggplot2::ggplot(plot_df, do.call(ggplot2::aes_string, aes_args))
      plot <- add_plot_component(plot, ggplot2::geom_bar(position = opts$position))
    }
    plot <- apply_plot_palette(plot, opts$palette, use_fill = nzchar(group))
    return(list(plot = plot, stats = data_info$stats))
  }

  if (type == "histogram" || type == "density" || type == "qq") {
    if (!nzchar(x)) stop("Histogram/density/qq plots require --vars or --x.")
    numeric_vars <- c(x)
    categorical_vars <- if (nzchar(group)) group else character(0)
    data_info <- prepare_plot_data(df, numeric_vars, categorical_vars, na_action)
    plot_df <- data_info$df
    if (!is_numeric_column(df, x)) stop("Histogram/density/qq plots require numeric variables.")
    if (nzchar(group) && group %in% names(plot_df)) {
      plot_df[[group]] <- coerce_categorical(plot_df[[group]])
    }
    if (type == "histogram") {
      aes_args <- list(x = x)
      if (nzchar(group)) aes_args$fill <- group
      plot <- ggplot2::ggplot(plot_df, do.call(ggplot2::aes_string, aes_args))
      if (!is.null(opts$binwidth)) {
        plot <- add_plot_component(plot, ggplot2::geom_histogram(binwidth = opts$binwidth, position = "identity", alpha = alpha))
      } else {
        plot <- add_plot_component(plot, ggplot2::geom_histogram(bins = opts$bins, position = "identity", alpha = alpha))
      }
    } else if (type == "density") {
      aes_args <- list(x = x)
      if (nzchar(group)) aes_args$fill <- group
      plot <- ggplot2::ggplot(plot_df, do.call(ggplot2::aes_string, aes_args))
      if (!is.null(opts$bw)) {
        plot <- add_plot_component(plot, ggplot2::geom_density(bw = opts$bw, alpha = alpha))
      } else {
        plot <- add_plot_component(plot, ggplot2::geom_density(alpha = alpha))
      }
    } else {
      plot <- ggplot2::ggplot(plot_df, ggplot2::aes_string(sample = x))
      plot <- add_plot_layers(
        plot,
        ggplot2::stat_qq(),
        ggplot2::stat_qq_line()
      )
    }
    plot <- apply_plot_palette(plot, opts$palette, use_fill = nzchar(group))
    return(list(plot = plot, stats = data_info$stats))
  }

  stop("Unsupported plot type: ", type)
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  opts <- resolve_parse_args(args)

  if (isTRUE(opts$help)) {
    print_usage()
    return(invisible(NULL))
  }

  if (isTRUE(opts$interactive)) {
    opts <- interactive_options()
  }

  engine <- resolve_plot_engine()
  if (engine == "base") {
    cat("Note: ggplot2 unavailable or incompatible; using base R plotting.\n")
  }

  df <- resolve_load_dataframe(opts)
  out_dir <- resolve_get_workspace_out_dir(df)
  plots_dir <- resolve_ensure_out_dir(file.path(out_dir, "plots"))

  type_default <- resolve_config_value("modules.plot.type", "auto")
  type <- normalize_plot_type(if (!is.null(opts$type)) opts$type else type_default)
  x <- if (!is.null(opts$x)) trimws(as.character(opts$x)) else ""
  y <- if (!is.null(opts$y)) trimws(as.character(opts$y)) else ""
  group <- if (!is.null(opts$group)) trimws(as.character(opts$group)) else ""

  vars_default <- resolve_config_value("modules.plot.vars_default", "numeric")
  if (type == "bar") vars_default <- "non-numeric"
  vars_raw <- if (!is.null(opts$vars)) as.character(opts$vars) else ""
  vars <- resolve_select_variables(df, vars_raw, group_var = if (nzchar(group)) group else NULL, default = vars_default, include_numeric = TRUE)
  vars <- vars[nzchar(vars)]

  if (type == "auto") {
    type <- infer_plot_type(df, x, y, vars, group)
  }
  type <- normalize_plot_type(type)
  if (type != "corr-heatmap" && (nzchar(x) || nzchar(y)) && !nzchar(vars_raw)) {
    vars <- character(0)
  }

  stat <- if (!is.null(opts$stat)) tolower(as.character(opts$stat)) else resolve_config_value("modules.plot.stat", "count")
  if (!stat %in% c("count", "percent")) stat <- "count"
  percent_base <- if (!is.null(opts$`percent-base`)) {
    tolower(as.character(opts$`percent-base`))
  } else if (!is.null(opts$percent_base)) {
    tolower(as.character(opts$percent_base))
  } else {
    resolve_config_value("modules.plot.percent_base", "total")
  }
  if (!percent_base %in% c("total", "group")) percent_base <- "total"

  bins <- resolve_parse_integer(opts$bins, default = resolve_config_value("modules.plot.bins", 30))
  binwidth <- resolve_parse_number(opts$binwidth, default = resolve_config_value("modules.plot.binwidth", NULL))
  bw <- resolve_parse_number(opts$bw, default = resolve_config_value("modules.plot.bw", NULL))
  smooth <- if (!is.null(opts$smooth)) tolower(as.character(opts$smooth)) else resolve_config_value("modules.plot.smooth", "none")
  if (!smooth %in% c("none", "loess", "lm")) smooth <- "none"
  se <- resolve_parse_bool(opts$se, default = resolve_config_value("modules.plot.se", TRUE))
  span <- resolve_parse_number(opts$span, default = resolve_config_value("modules.plot.span", 0.75))
  summary <- if (!is.null(opts$summary)) tolower(as.character(opts$summary)) else resolve_config_value("modules.plot.summary", "none")
  if (!summary %in% c("none", "mean", "median")) summary <- "none"
  na_action <- if (!is.null(opts$`na-action`)) tolower(as.character(opts$`na-action`)) else NULL
  if (is.null(na_action) || !nzchar(na_action)) {
    na_action <- if (!is.null(opts$na_action)) tolower(as.character(opts$na_action)) else resolve_config_value("modules.plot.na_action", "omit")
  }
  if (!na_action %in% c("omit", "keep")) na_action <- "omit"
  alpha <- resolve_parse_number(opts$alpha, default = resolve_config_value("modules.plot.alpha", 0.7))
  position <- if (!is.null(opts$position)) tolower(as.character(opts$position)) else resolve_config_value("modules.plot.position", "dodge")
  if (!position %in% c("dodge", "stack", "fill")) position <- "dodge"
  theme_name <- if (!is.null(opts$theme)) tolower(as.character(opts$theme)) else resolve_config_value("modules.plot.theme", "minimal")
  palette <- if (!is.null(opts$palette)) tolower(as.character(opts$palette)) else resolve_config_value("modules.plot.palette", "default")
  format <- if (!is.null(opts$format)) tolower(as.character(opts$format)) else resolve_config_value("modules.plot.format", "png")
  format <- gsub("^\\.", "", format)
  width <- resolve_parse_number(opts$width, default = resolve_config_value("modules.plot.width", 7))
  height <- resolve_parse_number(opts$height, default = resolve_config_value("modules.plot.height", 5))
  dpi <- resolve_parse_integer(opts$dpi, default = resolve_config_value("modules.plot.dpi", 300))
  file_prefix <- if (!is.null(opts$`file-prefix`)) as.character(opts$`file-prefix`) else NULL
  if (is.null(file_prefix) || !nzchar(file_prefix)) {
    file_prefix <- if (!is.null(opts$file_prefix)) as.character(opts$file_prefix) else resolve_config_value("modules.plot.file_prefix", "figure")
  }
  file_suffix <- if (!is.null(opts$`file-suffix`)) as.character(opts$`file-suffix`) else NULL
  if (is.null(file_suffix) || !nzchar(file_suffix)) {
    file_suffix <- if (!is.null(opts$file_suffix)) as.character(opts$file_suffix) else ""
  }
  overwrite <- resolve_parse_bool(opts$overwrite, default = FALSE)
  digits <- resolve_parse_integer(opts$digits, default = resolve_config_value("defaults.digits", 2))

  opts$stat <- stat
  opts$percent_base <- percent_base
  opts$bins <- bins
  opts$binwidth <- binwidth
  opts$bw <- bw
  opts$smooth <- smooth
  opts$se <- se
  opts$span <- span
  opts$summary <- summary
  opts$na_action <- na_action
  opts$alpha <- alpha
  opts$position <- position
  opts$theme <- theme_name
  opts$palette <- palette

  requests <- build_plot_requests(df, type, vars, x, y, group)
  if (length(requests) == 0) stop("No plot requests generated.")

  figure_digits <- resolve_parse_integer(resolve_config_value("modules.plot.figure_digits", 3), default = 3)
  figure_start <- resolve_parse_integer(opts$`figure-number`, default = NULL)
  if (is.null(figure_start)) {
    if (exists("get_next_figure_number", mode = "function")) {
      figure_start <- get("get_next_figure_number", mode = "function")(file.path(out_dir, "report_canonical.md"))
    } else {
      figure_start <- 1
    }
  }

  figure_rows <- list()
  figure_blocks <- character(0)
  for (i in seq_along(requests)) {
    req <- requests[[i]]
    plot_result <- build_plot(df, req, opts, digits, engine = engine)

    fig_num <- figure_start + i - 1
    fig_label <- sprintf("%0*d", figure_digits, fig_num)
    slug <- build_slug(req$type, req$x, req$y, req$group, file_suffix)
    filename <- paste0(file_prefix, "-", fig_label, "-", slug, ".", format)
    file_path <- ensure_unique_path(file.path(plots_dir, filename), overwrite = overwrite)
    figure_rel_path <- file.path("plots", basename(file_path))
    figure_rel_path <- gsub("\\\\", "/", figure_rel_path)

    title_default <- build_default_title(req$type, req$x, req$y, stat)
    title <- if (!is.null(opts$title) && nzchar(opts$title)) opts$title else title_default
    caption <- if (!is.null(opts$caption) && nzchar(opts$caption)) opts$caption else title
    note <- build_figure_note(plot_result$stats, na_action, opts$note, digits)

    if (!is.null(plot_result$engine) && plot_result$engine == "base") {
      plot_result$draw(file_path, format, width, height, dpi, title, opts$subtitle)
    } else {
      plot_obj <- plot_result$plot
      plot_obj <- apply_plot_theme(plot_obj, theme_name)
      ggplot2::ggsave(filename = file_path, plot = plot_obj, width = width, height = height, dpi = dpi, units = "in")
    }

    figure_rows[[length(figure_rows) + 1]] <- list(
      figure_number = fig_num,
      figure_title = caption,
      figure_caption = caption,
      figure_note = note,
      figure_path = figure_rel_path,
      plot_type = req$type,
      vars = if (length(req$vars) > 0) paste(req$vars, collapse = ", ") else "",
      x = req$x,
      y = req$y,
      group = req$group,
      n = plot_result$stats$n,
      missing_n = plot_result$stats$missing_n,
      missing_pct = round(plot_result$stats$missing_pct, digits)
    )

    figure_blocks <- c(figure_blocks, build_figure_markdown(fig_num, caption, figure_rel_path, note))
  }

  figure_body <- paste(figure_blocks, collapse = "\n\n")
  first_row <- figure_rows[[1]]
  template_override <- resolve_template_override(opts$template, module = "plot")
  template_path <- if (!is.null(template_override)) {
    template_override
  } else {
    resolve_get_template_path("plot.default", "plot/default-template.md")
  }
  template_context <- list(
    tokens = list(
      figure_number = first_row$figure_number,
      figure_title = first_row$figure_title,
      figure_caption = first_row$figure_caption,
      figure_note = first_row$figure_note,
      figure_path = first_row$figure_path,
      plot_type = first_row$plot_type,
      n = first_row$n,
      missing_n = first_row$missing_n,
      missing_pct = first_row$missing_pct
    ),
    narrative_rows = figure_rows
  )

  analysis_flags <- list(
    type = type,
    engine = engine,
    vars = if (length(vars) > 0) paste(vars, collapse = ", ") else "None",
    x = if (nzchar(x)) x else "None",
    y = if (nzchar(y)) y else "None",
    group = if (nzchar(group)) group else "None",
    stat = stat,
    percent_base = percent_base,
    bins = bins,
    binwidth = if (!is.null(binwidth)) binwidth else "auto",
    bw = if (!is.null(bw)) bw else "auto",
    smooth = smooth,
    se = se,
    span = span,
    summary = summary,
    na_action = na_action,
    alpha = alpha,
    position = position,
    theme = theme_name,
    palette = palette,
    format = format,
    size = paste0(width, "x", height, " in"),
    dpi = dpi
  )

  resolve_append_nlss_figure_report(
    file.path(out_dir, "report_canonical.md"),
    "Plots",
    figure_body,
    analysis_flags = analysis_flags,
    template_path = template_path,
    template_context = template_context,
    figure_start = figure_start
  )

  cat("Wrote:\n")
  cat("- ", render_output_path(file.path(out_dir, "report_canonical.md"), out_dir), "\n", sep = "")
  for (row in figure_rows) {
    cat("- ", render_output_path(file.path(out_dir, row$figure_path), out_dir), "\n", sep = "")
  }

  log_default <- resolve_config_value("defaults.log", TRUE)
  if (resolve_parse_bool(opts$log, default = log_default)) {
    ctx <- resolve_get_run_context()
    resolve_append_analysis_log(
      out_dir,
      module = "plot",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(figures = figure_rows),
      options = list(
        type = type,
        engine = engine,
        vars = vars,
        x = x,
        y = y,
        group = group,
        stat = stat,
        percent_base = percent_base,
        bins = bins,
        binwidth = binwidth,
        bw = bw,
        smooth = smooth,
        se = se,
        span = span,
        summary = summary,
        na_action = na_action,
        alpha = alpha,
        position = position,
        theme = theme_name,
        palette = palette,
        format = format,
        width = width,
        height = height,
        dpi = dpi,
        file_prefix = file_prefix,
        file_suffix = file_suffix
      ),
      user_prompt = resolve_get_user_prompt(opts)
    )
  }
}

main()
