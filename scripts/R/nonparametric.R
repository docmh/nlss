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
  cat("Nonparametric tests (base R)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript nonparametric.R --csv data.csv --vars var1,var2 [--mu 0]\n")
  cat("  Rscript nonparametric.R --csv data.csv --vars var1 --group group_var\n")
  cat("  Rscript nonparametric.R --csv data.csv --x var1 --y var2\n")
  cat("  Rscript nonparametric.R --csv data.csv --within pre,mid,post --subject-id id\n")
  cat("  Rscript nonparametric.R --sav data.sav --vars var1,var2\n")
  cat("  Rscript nonparametric.R --rds data.rds --vars var1,var2\n")
  cat("  Rscript nonparametric.R --rdata data.RData --df data_frame_name --vars var1,var2\n")
  cat("  Rscript nonparametric.R --parquet data.parquet --vars var1,var2\n")
  cat("  Rscript nonparametric.R --interactive\n")
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
  cat("  --test TYPE            auto/wilcoxon/mann_whitney/kruskal/friedman (default: auto)\n")
  cat("  --vars LIST            Comma-separated variables (one-sample or group tests)\n")
  cat("  --group NAME           Grouping variable (independent tests)\n")
  cat("  --x LIST               Comma-separated measure 1 variables (paired)\n")
  cat("  --y LIST               Comma-separated measure 2 variables (paired)\n")
  cat("  --within LIST           Comma-separated repeated measures (Friedman)\n")
  cat("  --subject-id NAME      Subject ID for repeated measures (Friedman)\n")
  cat("  --mu VALUE             One-sample test value (default: 0.0)\n")
  cat("  --alternative TYPE     two.sided/greater/less (default: two.sided)\n")
  cat("  --exact TRUE/FALSE     Exact p-values when possible (default: auto)\n")
  cat("  --continuity TRUE/FALSE Continuity correction (default: TRUE)\n")
  cat("  --conf-level VALUE     Confidence level (default: 0.95)\n")
  cat("  --posthoc TYPE          none/pairwise (default: none)\n")
  cat("  --p-adjust TYPE        none/bonferroni/holm/hochberg/hommel/BH/BY/fdr (default: holm)\n")
  cat("  --effect-size TYPE     r/rb/eta_H_sq/kendall_w (legacy epsilon_sq alias; default: r)\n")
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

  test_default <- get_config_value("modules.nonparametric.test", "auto")
  opts$test <- prompt("Test type (auto/wilcoxon/mann_whitney/kruskal/friedman)", test_default)

  opts$vars <- prompt("Variables (comma-separated, blank for defaults)", "")
  opts$group <- prompt("Grouping variable (blank if not used)", "")
  opts$x <- prompt("Measure 1 variables (comma-separated, paired; blank if not used)", "")
  opts$y <- prompt("Measure 2 variables (comma-separated, paired; blank if not used)", "")
  opts$within <- prompt("Repeated measures (comma-separated, Friedman; blank if not used)", "")
  opts$`subject-id` <- prompt("Subject ID (Friedman; blank if not used)", "")

  mu_default <- get_config_value("modules.nonparametric.mu", 0.0)
  alternative_default <- get_config_value("modules.nonparametric.alternative", "two.sided")
  exact_default <- get_config_value("modules.nonparametric.exact", "auto")
  continuity_default <- get_config_value("modules.nonparametric.continuity", TRUE)
  conf_default <- get_config_value("modules.nonparametric.conf_level", 0.95)
  posthoc_default <- get_config_value("modules.nonparametric.posthoc", "none")
  adjust_default <- get_config_value("modules.nonparametric.p_adjust", "holm")
  effect_default <- get_config_value("modules.nonparametric.effect_size", "r")
  digits_default <- get_config_value("defaults.digits", 2)

  opts$mu <- prompt("Test value (mu)", as.character(mu_default))
  opts$alternative <- prompt("Alternative (two.sided/greater/less)", alternative_default)
  opts$exact <- prompt("Exact p-values TRUE/FALSE/auto", as.character(exact_default))
  opts$continuity <- prompt("Continuity correction TRUE/FALSE", ifelse(isTRUE(continuity_default), "TRUE", "FALSE"))
  opts$`conf-level` <- prompt("Confidence level", as.character(conf_default))
  opts$posthoc <- prompt("Post-hoc (none/pairwise)", posthoc_default)
  opts$`p-adjust` <- prompt("P-value adjustment", adjust_default)
  opts$`effect-size` <- prompt("Effect size (r/rb/eta_H_sq/kendall_w; legacy epsilon_sq alias)", effect_default)
  opts$digits <- prompt("Rounding digits", as.character(digits_default))
  opts$template <- prompt("Template (path or key; blank for default)", "")
  opts$`user-prompt` <- prompt("User prompt (optional)", "")
  log_default <- get_config_value("defaults.log", TRUE)
  opts$log <- prompt("Write JSONL log TRUE/FALSE", ifelse(isTRUE(log_default), "TRUE", "FALSE"))
  opts
}

format_num <- function(value, digits) {
  if (is.na(value)) return("")
  format(round(value, digits), nsmall = digits, trim = TRUE)
}

format_stat <- function(value, digits) {
  if (is.na(value)) return("")
  txt <- format(round(value, digits), nsmall = digits, trim = TRUE)
  sub("^(-?)0", "\\1", txt)
}

format_p <- function(p_value) {
  if (is.na(p_value)) return("")
  if (p_value < 0.001) return("< .001")
  p_txt <- format(round(p_value, 3), nsmall = 3, trim = TRUE)
  p_txt <- sub("^0", "", p_txt)
  paste("=", p_txt)
}

format_ci <- function(low, high, digits) {
  if (is.na(low) || is.na(high)) return("")
  paste0("[", format_stat(low, digits), ", ", format_stat(high, digits), "]")
}

format_effect_label <- function(effect_size) {
  if (effect_size == "r") return("r")
  if (effect_size == "rb") return("r_rb")
  if (effect_size == "eta_H_sq") return("eta_H_sq")
  if (effect_size == "kendall_w") return("W")
  effect_size
}

format_test_label <- function(mode, paired = FALSE) {
  if (mode == "wilcoxon_one_sample") return("Wilcoxon signed-rank")
  if (mode == "wilcoxon_paired") return("Wilcoxon signed-rank")
  if (mode == "mann_whitney") return("Mann-Whitney U")
  if (mode == "kruskal") return("Kruskal-Wallis")
  if (mode == "friedman") return("Friedman")
  mode
}

format_stat_label <- function(mode) {
  if (mode %in% c("wilcoxon_one_sample", "wilcoxon_paired")) return("V")
  if (mode == "mann_whitney") return("U")
  if (mode == "kruskal") return("H")
  if (mode == "friedman") return("Q")
  ""
}


# Group values retain their storage identity. Display strings never select rows.
nonparam_group_order <- function(values, label_meta, group_var) {
  observed <- unique(values[!is.na(values)])
  if (is.factor(values)) observed <- observed[order(as.integer(observed))]
  else observed <- sort(observed)
  order <- lapply(seq_along(observed), function(i) list(
    group_id = i, value = observed[i],
    value_hex = if (is.numeric(observed) || inherits(observed, c("Date", "POSIXt")))
      sprintf("%a", as.numeric(observed[i])) else NULL,
    label = resolve_value_label(label_meta, group_var, as.character(observed[i])),
    row_indices = which(!is.na(values) & values == observed[i])))
  labels <- vapply(order, "[[", character(1), "label")
  collisions <- duplicated(labels) | duplicated(labels, fromLast = TRUE)
  for (i in which(collisions)) order[[i]]$label <- paste0(labels[i], " [group ", i, "]")
  order
}

normalize_choice <- function(value, default, choices, name) {
  value <- if (is.null(value) || identical(value, "")) default else value
  value <- tolower(as.character(value))
  found <- names(choices)[vapply(choices, function(x) length(value) == 1L && value %in% x, logical(1))]
  if (length(found) != 1L) stop("Invalid --", name, ": ", paste(value, collapse = ", "))
  found
}

bound_status <- function(value) {
  if (is.na(value)) "unavailable" else if (is.infinite(value))
    if (value > 0) "positive_infinity" else "negative_infinity" else "finite"
}

# Calculate the requested test independently from its interval: a failed interval
# must not erase an otherwise valid R test, nor turn an invalid test into success.
nonparam_wilcox <- function(x, y = NULL, paired = FALSE, mu = 0,
                            alternative, conf_level, exact, continuity, effect_size) {
  messages <- character(0)
  invoke <- function(ci) withCallingHandlers(
    stats::wilcox.test(x, y, paired = paired, mu = mu, alternative = alternative,
      conf.int = ci, conf.level = conf_level, exact = exact, correct = continuity),
    warning = function(w) { messages <<- c(messages, conditionMessage(w)); invokeRestart("muffleWarning") })
  test <- invoke(FALSE)
  if (!is.finite(test$statistic) || !is.finite(test$p.value))
    stop("The requested rank test has no estimable finite statistic and p-value (check all-zero differences or fully tied observations).")
  interval <- tryCatch(invoke(TRUE), error = function(e) {
    messages <<- c(messages, paste("Confidence interval unavailable:", conditionMessage(e)))
    NULL
  })
  signed <- is.null(y) || paired
  if (signed) {
    diffs <- if (paired) x - y - mu else x - mu
    nonzero <- diffs[diffs != 0]
    ranks <- rank(abs(nonzero))
    n <- length(nonzero)
    ties <- anyDuplicated(ranks) > 0L
    zero_n <- sum(diffs == 0)
    # Conditional sign-flip variance, including tied absolute ranks.
    variance <- sum(ranks^2) / 4
    z <- (sum(ranks[nonzero > 0]) - sum(ranks) / 2) / sqrt(variance)
    rb <- sum(sign(nonzero) * ranks) / sum(ranks)
  } else {
    n1 <- length(x); n2 <- length(y); n <- n1 + n2
    ranks <- rank(c(x, y))
    tie_sizes <- as.numeric(table(ranks))
    ties <- any(tie_sizes > 1)
    zero_n <- NA_integer_
    variance <- n1 * n2 / 12 * (n + 1 - sum(tie_sizes^3 - tie_sizes) / (n * (n - 1)))
    # R's W is already the Mann-Whitney U; do not subtract the rank offset twice.
    z <- (unname(test$statistic) - n1 * n2 / 2) / sqrt(variance)
    rb <- 2 * unname(test$statistic) / (n1 * n2) - 1
  }
  low <- if (!is.null(interval$conf.int)) interval$conf.int[1] else NA_real_
  high <- if (!is.null(interval$conf.int)) interval$conf.int[2] else NA_real_
  level <- if (!is.null(interval$conf.int)) attr(interval$conf.int, "conf.level") else NA_real_
  if (is.null(level)) level <- NA_real_
  ci_available <- !is.na(low) && !is.na(high) && is.finite(level) && level > 0
  ci_status <- if (!ci_available) "unavailable" else if (level < conf_level) "reduced_coverage" else "available"
  if (!ci_available) messages <- c(messages, "Requested rank-test confidence interval is unavailable; the primary test remains estimable.")
  for (message in unique(messages)) warning(message, call. = FALSE)
  list(statistic = unname(test$statistic), p = test$p.value,
    z = if (is.finite(z)) z else NA_real_, rank_variance = variance,
    effect_size_value = if (effect_size == "rb") rb else z / sqrt(n),
    effect_size_label = format_effect_label(effect_size), ci_low = low, ci_high = high,
    ci_low_status = bound_status(low), ci_high_status = bound_status(high),
    ci_status = ci_status, actual_ci_level = level,
    location_estimate = if (!is.null(interval$estimate)) unname(interval$estimate) else NA_real_,
    estimate_label = if (!is.null(interval$estimate)) names(interval$estimate)[1] else "",
    ci_estimand = if (is.null(y)) "population_pseudomedian" else if (paired)
      "paired_difference_pseudomedian" else "population_location_shift",
    method = test$method, exact_used = grepl("exact", test$method, fixed = TRUE),
    continuity_used = grepl("continuity", test$method, fixed = TRUE),
    n_nonzero = if (signed) n else NA_integer_, zero_diff_n = zero_n, ties = ties,
    status = "available")
}

summary_row <- function(entry, mode) {
  x <- entry$x; y <- entry$y
  med <- function(v) if (length(v)) median(v) else NA_real_
  spread <- function(v) if (length(v)) IQR(v) else NA_real_
  signed <- mode %in% c("wilcoxon_one_sample", "wilcoxon_paired")
  diffs <- if (mode == "wilcoxon_paired") x - y else if (mode == "wilcoxon_one_sample") x - entry$mu else NULL
  list(test_id = entry$test_id, test_type = format_test_label(mode),
    variable = entry$variable, measure_1 = if (mode == "wilcoxon_paired") entry$variables[1] else "",
    measure_2 = if (mode == "wilcoxon_paired") entry$variables[2] else "",
    group = if (is.null(entry$group_var)) "" else entry$group_var,
    group_1 = if (mode == "mann_whitney") as.character(entry$groups[[1]]$value) else "",
    group_2 = if (mode == "mann_whitney") as.character(entry$groups[[2]]$value) else "",
    group_1_id = if (mode == "mann_whitney") entry$groups[[1]]$group_id else NA_integer_,
    group_2_id = if (mode == "mann_whitney") entry$groups[[2]]$group_id else NA_integer_,
    group_1_label = if (mode == "mann_whitney") entry$groups[[1]]$label else "",
    group_2_label = if (mode == "mann_whitney") entry$groups[[2]]$label else "",
    n_1 = if (signed || mode == "mann_whitney") length(x) else NA_integer_,
    n_2 = if (mode == "mann_whitney") length(y) else NA_integer_,
    n_total = entry$n_complete,
    median_1 = if (!is.null(y)) med(x) else NA_real_, median_2 = med(y),
    iqr_1 = if (!is.null(y)) spread(x) else NA_real_, iqr_2 = spread(y),
    median = if (mode == "wilcoxon_one_sample") med(x) else NA_real_,
    iqr = if (mode == "wilcoxon_one_sample") spread(x) else NA_real_,
    median_diff = if (signed) med(diffs) else if (mode == "mann_whitney") med(x) - med(y) else NA_real_,
    iqr_diff = if (signed) spread(diffs) else NA_real_,
    group_summary = "", within_summary = "", stat_label = format_stat_label(mode),
    df = NA_real_, ci_low = NA_real_, ci_high = NA_real_,
    ci_low_status = "not_applicable", ci_high_status = "not_applicable", ci_status = "not_applicable",
    actual_ci_level = NA_real_, ci_estimand = "", location_estimate = NA_real_, estimate_label = "",
    mu = if (mode == "wilcoxon_one_sample") entry$mu else NA_real_,
    z = NA_real_, rank_variance = NA_real_, method = "", exact_used = FALSE,
    continuity_used = FALSE, n_nonzero = NA_integer_, zero_diff_n = NA_integer_, ties = FALSE,
    status = "available")
}

compute_nonparametric <- function(entry, mode, alternative, conf_level, exact, continuity,
                                  effect_size, posthoc_effect_size, posthoc, p_adjust, digits) {
  row <- summary_row(entry, mode)
  rank_modes <- c("wilcoxon_one_sample", "wilcoxon_paired", "mann_whitney")
  comparisons <- list()
  if (mode %in% rank_modes) {
    values <- nonparam_wilcox(entry$x, entry$y, mode == "wilcoxon_paired", entry$mu,
      alternative, conf_level, exact, continuity, effect_size)
    row <- modifyList(row, values)
  } else {
    test <- if (mode == "kruskal") stats::kruskal.test(entry$samples) else stats::friedman.test(entry$matrix)
    if (!is.finite(test$statistic) || !is.finite(test$p.value))
      stop("The requested omnibus rank test is not estimable (check fully tied data).")
    row$statistic <- unname(test$statistic); row$p <- test$p.value
    row$df <- unname(test$parameter); row$method <- test$method
    k <- if (mode == "kruskal") length(entry$samples) else ncol(entry$matrix)
    n <- entry$n_complete
    row$effect_size_label <- format_effect_label(effect_size)
    row$effect_size_value <- if (mode == "kruskal") {
      if (n > k) (row$statistic - k + 1) / (n - k) else NA_real_
    } else row$statistic / (n * (k - 1))
    if (is.na(row$effect_size_value)) warning("The requested omnibus effect size is unavailable: n must exceed k.", call. = FALSE)
    row$ties <- if (mode == "kruskal") anyDuplicated(unlist(entry$samples)) > 0L else
      any(apply(entry$matrix, 1L, function(x) anyDuplicated(x) > 0L))
    items <- if (mode == "kruskal") entry$samples else as.data.frame(entry$matrix)
    item_labels <- if (mode == "kruskal") vapply(entry$groups, "[[", character(1), "label") else entry$variables
    description <- paste(vapply(seq_along(items), function(i) paste0(item_labels[i], ": ",
      format_stat(median(items[[i]]), digits), " (IQR ", format_stat(IQR(items[[i]]), digits), ")"), character(1)), collapse = "; ")
    if (mode == "kruskal") row$group_summary <- description else row$within_summary <- description
    if (posthoc == "pairwise") for (pair in combn(seq_along(items), 2L, simplify = FALSE)) {
      i <- pair[1]; j <- pair[2]; paired <- mode == "friedman"
      x <- items[[i]]; y <- items[[j]]
      values <- tryCatch(nonparam_wilcox(x, y, paired, 0, alternative, conf_level,
        exact, continuity, posthoc_effect_size), error = function(e) {
          warning("Post-hoc comparison unavailable: ", item_labels[i], " vs ", item_labels[j], ": ", conditionMessage(e), call. = FALSE)
          list(statistic = NA_real_, p = NA_real_, z = NA_real_, rank_variance = NA_real_,
            effect_size_value = NA_real_, effect_size_label = format_effect_label(posthoc_effect_size),
            ci_low = NA_real_, ci_high = NA_real_, ci_low_status = "unavailable", ci_high_status = "unavailable",
            ci_status = "unavailable", actual_ci_level = NA_real_, location_estimate = NA_real_,
            estimate_label = "", ci_estimand = if (paired) "paired_difference_pseudomedian" else "population_location_shift",
            method = "", exact_used = FALSE, continuity_used = FALSE,
            n_nonzero = if (paired) sum(x != y) else NA_integer_,
            zero_diff_n = if (paired) sum(x == y) else NA_integer_,
            ties = if (paired) anyDuplicated(abs((x-y)[x!=y])) > 0L else anyDuplicated(c(x,y)) > 0L,
            status = "unavailable", reason = conditionMessage(e))
        })
      if (is.null(values$reason)) values$reason <- ""
      comparisons[[length(comparisons) + 1L]] <- as.data.frame(c(list(
        test_id = entry$test_id, comparison_id = length(comparisons) + 1L,
        test_type = paste0(mode, "_posthoc"), variable = entry$variable,
        group = row$group, group_1 = if (paired) entry$variables[i] else as.character(entry$groups[[i]]$value),
        group_2 = if (paired) entry$variables[j] else as.character(entry$groups[[j]]$value),
        group_1_id = if (paired) i else entry$groups[[i]]$group_id,
        group_2_id = if (paired) j else entry$groups[[j]]$group_id,
        group_1_label = item_labels[i], group_2_label = item_labels[j], n_1 = length(x),
        n_2 = if (paired) NA_integer_ else length(y), n_total = if (paired) length(x) else length(x) + length(y),
        median_1 = median(x), median_2 = median(y), iqr_1 = IQR(x), iqr_2 = IQR(y),
        stat_label = if (paired) "V" else "U", df = NA_real_), values), stringsAsFactors = FALSE)
    }
  }
  row$effect_size_status <- if (is.finite(row$effect_size_value)) "available" else "unavailable"
  diag <- list(test_id = entry$test_id, test_type = mode, variable = entry$variable,
    n_total = entry$n_complete, n_nonzero = row$n_nonzero, zero_diff_n = row$zero_diff_n,
    ties = row$ties, exact_used = row$exact_used, continuity_used = row$continuity_used,
    method = row$method, ci_status = row$ci_status, actual_ci_level = row$actual_ci_level)
  diagnostics <- if (mode == "mann_whitney") do.call(rbind, lapply(seq_along(entry$groups), function(i)
    as.data.frame(c(diag, list(group_id = entry$groups[[i]]$group_id,
      group = as.character(entry$groups[[i]]$value), group_label = entry$groups[[i]]$label,
      n = length(if (i == 1L) entry$x else entry$y))), stringsAsFactors = FALSE))) else {
    if (mode == "kruskal") diag <- c(diag, list(group = entry$group_var,
      group_levels = paste(vapply(entry$groups, "[[", character(1), "label"), collapse = ","),
      group_sizes = paste(lengths(entry$samples), collapse = ",")))
    if (mode == "friedman") diag$k <- ncol(entry$matrix)
    as.data.frame(diag, stringsAsFactors = FALSE)
  }
  comparisons <- if (length(comparisons)) do.call(rbind, comparisons) else data.frame()
  if (nrow(comparisons)) {
    # The family is all planned comparisons for this outcome, not only the ones
    # whose inference happened to be available.
    comparisons$p_adj <- stats::p.adjust(comparisons$p, method = p_adjust, n = nrow(comparisons))
    comparisons$family_size <- nrow(comparisons)
  }
  list(summary = as.data.frame(row, stringsAsFactors = FALSE), diagnostics = diagnostics, posthoc = comparisons)
}
format_nlss_table <- function(summary_df, digits, note_text) {
  display <- summary_df
  display$variable_display <- if ("variable_label" %in% names(display)) display$variable_label else display$variable
  display$group_display <- if ("group_label" %in% names(display)) display$group_label else display$group
  headers <- c("Test", "Variable", "Statistic", "df", "p", "Effect")
  rows <- list()
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    rows[[length(rows) + 1]] <- c(
      as_cell_text(row$test_type),
      as_cell_text(row$variable_display),
      format_stat(row$statistic, digits),
      format_stat(row$df, digits),
      format_p(row$p),
      format_stat(row$effect_size_value, digits)
    )
  }
  table <- render_markdown_table(headers, rows)
  paste0(table, "\n\n", note_text)
}

format_nlss_text <- function(summary_df, digits, conf_level) {
  rows <- build_nonparam_narrative_rows(summary_df, digits, conf_level)
  if (length(rows) == 0) return("")
  paste(vapply(rows, function(row) row$full_sentence, character(1)), collapse = "\n")
}

build_nonparam_table_body <- function(summary_df, digits, table_meta) {
  display <- summary_df
  display$variable_display <- if ("variable_label" %in% names(display)) display$variable_label else display$variable
  display$measure_1_display <- if ("measure_1_label" %in% names(display)) display$measure_1_label else display$measure_1
  display$measure_2_display <- if ("measure_2_label" %in% names(display)) display$measure_2_label else display$measure_2
  display$group_display <- if ("group_label" %in% names(display)) display$group_label else display$group
  display$group_1_display <- if ("group_1_label" %in% names(display)) display$group_1_label else display$group_1
  display$group_2_display <- if ("group_2_label" %in% names(display)) display$group_2_label else display$group_2
  default_specs <- list(
    list(key = "test_type", label = "Test"),
    list(key = "variable", label = "Variable"),
    list(key = "measure_1", label = "Measure 1", drop_if_empty = TRUE),
    list(key = "measure_2", label = "Measure 2", drop_if_empty = TRUE),
    list(key = "group", label = "Group", drop_if_empty = TRUE),
    list(key = "group_1", label = "Group 1", drop_if_empty = TRUE),
    list(key = "group_2", label = "Group 2", drop_if_empty = TRUE),
    list(key = "n_1", label = "n1", drop_if_empty = TRUE),
    list(key = "n_2", label = "n2", drop_if_empty = TRUE),
    list(key = "n_total", label = "n", drop_if_empty = TRUE),
    list(key = "median_1", label = "Mdn1", drop_if_empty = TRUE),
    list(key = "median_2", label = "Mdn2", drop_if_empty = TRUE),
    list(key = "iqr_1", label = "IQR1", drop_if_empty = TRUE),
    list(key = "iqr_2", label = "IQR2", drop_if_empty = TRUE),
    list(key = "median", label = "Mdn", drop_if_empty = TRUE),
    list(key = "iqr", label = "IQR", drop_if_empty = TRUE),
    list(key = "median_diff", label = "Mdn diff", drop_if_empty = TRUE),
    list(key = "iqr_diff", label = "IQR diff", drop_if_empty = TRUE),
    list(key = "group_summary", label = "Group medians", drop_if_empty = TRUE),
    list(key = "within_summary", label = "Condition medians", drop_if_empty = TRUE),
    list(key = "stat_label", label = "Stat", drop_if_empty = TRUE),
    list(key = "statistic", label = "Value"),
    list(key = "df", label = "df", drop_if_empty = TRUE),
    list(key = "p", label = "p"),
    list(key = "effect_size_label", label = "Effect", drop_if_empty = TRUE),
    list(key = "effect_size_value", label = "Effect size"),
    list(key = "ci_low", label = "CI low", drop_if_empty = TRUE),
    list(key = "ci_high", label = "CI high", drop_if_empty = TRUE)
  )

  columns <- normalize_table_columns(table_meta$columns, default_specs)
  rows <- list()
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    row_tokens <- list(
      test_type = as_cell_text(row$test_type),
      variable = as_cell_text(row$variable_display),
      measure_1 = as_cell_text(row$measure_1_display),
      measure_2 = as_cell_text(row$measure_2_display),
      group = as_cell_text(row$group_display),
      group_1 = as_cell_text(row$group_1_display),
      group_2 = as_cell_text(row$group_2_display),
      n_1 = format_stat(row$n_1, digits),
      n_2 = format_stat(row$n_2, digits),
      n_total = format_stat(row$n_total, digits),
      median_1 = format_stat(row$median_1, digits),
      median_2 = format_stat(row$median_2, digits),
      iqr_1 = format_stat(row$iqr_1, digits),
      iqr_2 = format_stat(row$iqr_2, digits),
      median = format_stat(row$median, digits),
      iqr = format_stat(row$iqr, digits),
      median_diff = format_stat(row$median_diff, digits),
      iqr_diff = format_stat(row$iqr_diff, digits),
      group_summary = as_cell_text(row$group_summary),
      within_summary = as_cell_text(row$within_summary),
      stat_label = as_cell_text(row$stat_label),
      statistic = format_stat(row$statistic, digits),
      df = format_stat(row$df, digits),
      p = format_p(row$p),
      effect_size_label = as_cell_text(row$effect_size_label),
      effect_size_value = format_stat(row$effect_size_value, digits),
      ci_low = format_stat(row$ci_low, digits),
      ci_high = format_stat(row$ci_high, digits)
    )

    row_values <- vapply(columns, function(col) as_cell_text(row_tokens[[col$key]]), character(1))
    rows[[length(rows) + 1]] <- row_values
  }

  drop_result <- drop_empty_columns(columns, rows)
  columns <- drop_result$columns
  rows <- drop_result$rows
  headers <- vapply(columns, function(col) col$label, character(1))
  list(
    body = render_markdown_table(headers, rows),
    columns = columns,
    rows = rows
  )
}

build_nonparam_narrative_rows <- function(summary_df, digits, conf_level) {
  display <- summary_df
  display$variable_display <- if ("variable_label" %in% names(display)) display$variable_label else display$variable
  display$measure_1_display <- if ("measure_1_label" %in% names(display)) display$measure_1_label else display$measure_1
  display$measure_2_display <- if ("measure_2_label" %in% names(display)) display$measure_2_label else display$measure_2
  display$group_display <- if ("group_label" %in% names(display)) display$group_label else display$group
  display$group_1_display <- if ("group_1_label" %in% names(display)) display$group_1_label else display$group_1
  display$group_2_display <- if ("group_2_label" %in% names(display)) display$group_2_label else display$group_2
  rows <- list()
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    stat_label <- as_cell_text(row$stat_label)
    stat_val <- format_stat(row$statistic, digits)
    df_val <- format_stat(row$df, digits)
    p_txt <- format_p(row$p)
    effect_label <- as_cell_text(row$effect_size_label)
    effect_val <- format_stat(row$effect_size_value, digits)
    ci_txt <- format_ci(row$ci_low, row$ci_high, digits)
    row_conf <- if ("actual_ci_level" %in% names(row)) row$actual_ci_level else conf_level
    conf_label <- if (!is.na(row_conf)) paste0(round(row_conf * 100, 2), "%") else ""
    if (identical(row$ci_status, "unavailable")) ci_txt <- ""

    effect_text <- if (nzchar(effect_label) && nzchar(effect_val)) paste0(", ", effect_label, " = ", effect_val) else ""
    ci_text <- if (nzchar(ci_txt)) paste0(", ", conf_label, " location CI ", ci_txt) else
      if (identical(row$ci_status, "unavailable")) ", location CI unavailable" else ""

    stat_text <- if (nzchar(stat_label)) {
      if (nzchar(df_val)) {
        paste0(stat_label, "(", df_val, ") = ", stat_val)
      } else {
        paste0(stat_label, " = ", stat_val)
      }
    } else {
      stat_val
    }

    full_sentence <- paste0(
      as_cell_text(row$test_type),
      " test for ",
      if (nzchar(as_cell_text(row$variable_display))) as_cell_text(row$variable_display) else "the specified measures",
      if (nzchar(as_cell_text(row$group_display))) paste0(" by ", as_cell_text(row$group_display)) else "",
      if (nzchar(as_cell_text(row$group_1_display)) && nzchar(as_cell_text(row$group_2_display))) {
        paste0(" (", as_cell_text(row$group_1_display), " vs ", as_cell_text(row$group_2_display), ")")
      } else {
        ""
      },
      " yielded ",
      stat_text,
      ", p ",
      p_txt,
      effect_text,
      ci_text,
      "."
    )
    full_sentence <- paste0(full_sentence, " Method: ", row$method, ".",
      if (identical(row$ci_status, "reduced_coverage")) " The attainable interval coverage is below the requested level." else "",
      if (identical(row$effect_size_status, "unavailable")) " The requested effect size is unavailable." else "")

    rows[[length(rows) + 1]] <- list(
      test_type = as_cell_text(row$test_type),
      variable = as_cell_text(row$variable_display),
      measure_1 = as_cell_text(row$measure_1_display),
      measure_2 = as_cell_text(row$measure_2_display),
      group = as_cell_text(row$group_display),
      group_1 = as_cell_text(row$group_1_display),
      group_2 = as_cell_text(row$group_2_display),
      n_1 = format_stat(row$n_1, digits),
      n_2 = format_stat(row$n_2, digits),
      n_total = format_stat(row$n_total, digits),
      median_1 = format_stat(row$median_1, digits),
      median_2 = format_stat(row$median_2, digits),
      iqr_1 = format_stat(row$iqr_1, digits),
      iqr_2 = format_stat(row$iqr_2, digits),
      median = format_stat(row$median, digits),
      iqr = format_stat(row$iqr, digits),
      median_diff = format_stat(row$median_diff, digits),
      iqr_diff = format_stat(row$iqr_diff, digits),
      stat_label = stat_label,
      statistic = stat_val,
      df = df_val,
      p = p_txt,
      effect_size_label = effect_label,
      effect_size_value = effect_val,
      ci = ci_txt,
      conf_level = conf_label,
      full_sentence = full_sentence
    )
  }
  rows
}

build_nonparam_note_tokens <- function(alternative, conf_level, exact, continuity, effect_size_label, posthoc, p_adjust) {
  parts <- character(0)
  if (!is.null(alternative) && nzchar(alternative)) {
    parts <- c(parts, paste0("Alternative: ", alternative, "."))
  }
  if (!is.null(exact)) {
    parts <- c(parts, paste0("Exact inference requested: ", ifelse(exact, "yes", "no"), "; actual method is recorded per test."))
  }
  if (!is.null(continuity)) {
    parts <- c(parts, paste0("Continuity correction requested for asymptotic tests: ", ifelse(continuity, "yes", "no"), "."))
  }
  if (!is.null(conf_level) && !is.na(conf_level)) {
    parts <- c(parts, paste0("Confidence level: ", round(conf_level * 100), "%."))
  }
  if (!is.null(effect_size_label) && nzchar(effect_size_label)) {
    parts <- c(parts, paste0("Effect size: ", effect_size_label, "."))
  }
  if (!is.null(posthoc) && posthoc == "pairwise") {
    parts <- c(parts, paste0("Post-hoc: pairwise Wilcoxon with ", p_adjust, " adjustment."))
  }
  list(note_default = paste(parts, collapse = " "))
}

build_posthoc_note_tokens <- function(posthoc, p_adjust) {
  parts <- character(0)
  if (!is.null(posthoc) && posthoc == "pairwise") {
    parts <- c(parts, paste0("Post-hoc: pairwise Wilcoxon with ", p_adjust, " adjustment."))
  }
  list(note_default = paste(parts, collapse = " "))
}

build_posthoc_table_body <- function(posthoc_df, digits, table_meta) {
  display <- posthoc_df
  display$variable_display <- if ("variable_label" %in% names(display)) display$variable_label else display$variable
  display$group_display <- if ("group_label" %in% names(display)) display$group_label else display$group
  display$group_1_display <- if ("group_1_label" %in% names(display)) display$group_1_label else display$group_1
  display$group_2_display <- if ("group_2_label" %in% names(display)) display$group_2_label else display$group_2
  default_specs <- list(
    list(key = "variable", label = "Variable"),
    list(key = "group", label = "Group", drop_if_empty = TRUE),
    list(key = "group_1", label = "Group 1", drop_if_empty = TRUE),
    list(key = "group_2", label = "Group 2", drop_if_empty = TRUE),
    list(key = "n_1", label = "n1", drop_if_empty = TRUE),
    list(key = "n_2", label = "n2", drop_if_empty = TRUE),
    list(key = "median_1", label = "Mdn1", drop_if_empty = TRUE),
    list(key = "median_2", label = "Mdn2", drop_if_empty = TRUE),
    list(key = "iqr_1", label = "IQR1", drop_if_empty = TRUE),
    list(key = "iqr_2", label = "IQR2", drop_if_empty = TRUE),
    list(key = "stat_label", label = "Stat", drop_if_empty = TRUE),
    list(key = "statistic", label = "Value"),
    list(key = "p", label = "p"),
    list(key = "p_adj", label = "p_adj", drop_if_empty = TRUE),
    list(key = "effect_size_label", label = "Effect", drop_if_empty = TRUE),
    list(key = "effect_size_value", label = "Effect size", drop_if_empty = TRUE),
    list(key = "ci_low", label = "CI low", drop_if_empty = TRUE),
    list(key = "ci_high", label = "CI high", drop_if_empty = TRUE)
  )

  columns <- normalize_table_columns(table_meta$columns, default_specs)
  rows <- list()
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    row_tokens <- list(
      variable = as_cell_text(row$variable_display),
      group = as_cell_text(row$group_display),
      group_1 = as_cell_text(row$group_1_display),
      group_2 = as_cell_text(row$group_2_display),
      n_1 = format_stat(row$n_1, digits),
      n_2 = format_stat(row$n_2, digits),
      median_1 = format_stat(row$median_1, digits),
      median_2 = format_stat(row$median_2, digits),
      iqr_1 = format_stat(row$iqr_1, digits),
      iqr_2 = format_stat(row$iqr_2, digits),
      stat_label = as_cell_text(row$stat_label),
      statistic = format_stat(row$statistic, digits),
      p = format_p(row$p),
      p_adj = format_p(row$p_adj),
      effect_size_label = as_cell_text(row$effect_size_label),
      effect_size_value = format_stat(row$effect_size_value, digits),
      ci_low = format_stat(row$ci_low, digits),
      ci_high = format_stat(row$ci_high, digits)
    )

    row_values <- vapply(columns, function(col) as_cell_text(row_tokens[[col$key]]), character(1))
    rows[[length(rows) + 1]] <- row_values
  }

  drop_result <- drop_empty_columns(columns, rows)
  columns <- drop_result$columns
  rows <- drop_result$rows
  headers <- vapply(columns, function(col) col$label, character(1))
  list(
    body = render_markdown_table(headers, rows),
    columns = columns,
    rows = rows
  )
}

build_posthoc_narrative_rows <- function(posthoc_df, digits) {
  display <- posthoc_df
  display$variable_display <- if ("variable_label" %in% names(display)) display$variable_label else display$variable
  display$group_display <- if ("group_label" %in% names(display)) display$group_label else display$group
  display$group_1_display <- if ("group_1_label" %in% names(display)) display$group_1_label else display$group_1
  display$group_2_display <- if ("group_2_label" %in% names(display)) display$group_2_label else display$group_2
  rows <- list()
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    stat_label <- as_cell_text(row$stat_label)
    stat_val <- format_stat(row$statistic, digits)
    p_txt <- format_p(row$p)
    p_adj_txt <- format_p(row$p_adj)
    effect_label <- as_cell_text(row$effect_size_label)
    effect_val <- format_stat(row$effect_size_value, digits)

    effect_text <- if (nzchar(effect_label) && nzchar(effect_val)) paste0(", ", effect_label, " = ", effect_val) else ""
    p_adj_text <- if (nzchar(p_adj_txt)) paste0(", p_adj ", p_adj_txt) else ""

    full_sentence <- paste0(
      "Pairwise comparison of ",
      as_cell_text(row$group_1_display),
      " vs ",
      as_cell_text(row$group_2_display),
      " yielded ",
      if (nzchar(stat_label)) paste0(stat_label, " = ", stat_val) else stat_val,
      ", p ",
      p_txt,
      p_adj_text,
      effect_text,
      "."
    )

    if (identical(row$status, "available")) full_sentence <- paste0(full_sentence,
      " Method: ", row$method, ". ",
      if (identical(row$ci_status, "unavailable")) "Location CI unavailable." else paste0(
        round(100 * row$actual_ci_level, 2), "% location CI ", format_ci(row$ci_low, row$ci_high, digits), "."),
      if (identical(row$ci_status, "reduced_coverage")) " Attainable coverage is below the requested level." else "")
    if (identical(row$status, "unavailable")) full_sentence <- paste0(
      "Pairwise comparison of ", row$group_1_display, " vs ", row$group_2_display,
      " is unavailable: ", row$reason, ".")
    rows[[length(rows) + 1]] <- list(
      variable = as_cell_text(row$variable_display),
      group = as_cell_text(row$group_display),
      group_1 = as_cell_text(row$group_1_display),
      group_2 = as_cell_text(row$group_2_display),
      n_1 = format_stat(row$n_1, digits),
      n_2 = format_stat(row$n_2, digits),
      median_1 = format_stat(row$median_1, digits),
      median_2 = format_stat(row$median_2, digits),
      iqr_1 = format_stat(row$iqr_1, digits),
      iqr_2 = format_stat(row$iqr_2, digits),
      stat_label = stat_label,
      statistic = stat_val,
      p = p_txt,
      p_adj = p_adj_txt,
      effect_size_label = effect_label,
      effect_size_value = effect_val,
      full_sentence = full_sentence
    )
  }
  rows
}

format_posthoc_table <- function(posthoc_df, digits, note_text) {
  headers <- c("Comparison", "Statistic", "p", "p_adj")
  rows <- list()
  for (i in seq_len(nrow(posthoc_df))) {
    row <- posthoc_df[i, ]
    comparison <- paste(as_cell_text(row$group_1), as_cell_text(row$group_2), sep = " vs ")
    rows[[length(rows) + 1]] <- c(
      comparison,
      format_stat(row$statistic, digits),
      format_p(row$p),
      format_p(row$p_adj)
    )
  }
  table <- render_markdown_table(headers, rows)
  paste0(table, "\n\n", note_text)
}


main <- function() {
  opts <- nlss_run_options(commandArgs(trailingOnly = TRUE), "nonparametric")
  if (!is.null(opts$help)) { print_usage(); return(invisible(NULL)) }
  if (parse_bool(opts$interactive, FALSE)) opts <- modifyList(opts, interactive_options())
  df <- nlss_load_input(opts)
  out_dir <- get_workspace_out_dir(df)
  nlss_begin_run("nonparametric", df, opts, out_dir)
  issue <- function(message) {
    nlss_run_context$request$validation_issue <- list(status = "invalid_input", message = message)
    stop(message, call. = FALSE)
  }
  present <- function(value) !is.null(value) && !identical(value, "")
  option <- function(flag) if (is.null(opts[[flag]])) get_config_value(paste0("modules.nonparametric.", gsub("-", "_", flag))) else opts[[flag]]
  number <- function(flag, value, low = -Inf, high = Inf, integer = FALSE, open = FALSE) {
    value <- suppressWarnings(as.numeric(value))
    if (length(value) != 1L || !is.finite(value) || value < low || value > high ||
        (open && (value == low || value == high)) || (integer && value != floor(value)))
      issue(paste0("Invalid numeric option --", flag, "."))
    value
  }
  digits <- number("digits", if (is.null(opts$digits)) get_config_value("defaults.digits") else opts$digits, 0, 15, TRUE)
  mu <- number("mu", option("mu"))
  conf_level <- number("conf-level", option("conf-level"), 0, 1, open = TRUE)
  test_choice <- normalize_choice(opts$test, get_config_value("modules.nonparametric.test"), list(
    auto = c("auto", "automatic"), wilcoxon = c("wilcoxon", "wilcox", "signed", "signed-rank", "signed_rank"),
    mann_whitney = c("mann_whitney", "mann-whitney", "mannwhitney", "ranksum", "rank-sum", "u"),
    kruskal = c("kruskal", "kruskal-wallis", "kruskal_wallis"), friedman = "friedman"), "test")
  alternative <- normalize_choice(opts$alternative, get_config_value("modules.nonparametric.alternative"),
    list(two.sided = c("two.sided", "two-sided", "two"), greater = "greater", less = "less"), "alternative")
  posthoc <- normalize_choice(opts$posthoc, get_config_value("modules.nonparametric.posthoc"),
    list(none = c("none", "off", "false"), pairwise = c("pairwise", "wilcoxon", "pair")), "posthoc")
  requested_effect <- normalize_choice(opts[["effect-size"]], get_config_value("modules.nonparametric.effect_size"),
    list(r = "r", rb = c("rb", "rbc", "rank-biserial", "rank_biserial"),
      epsilon_sq = c("epsilon_sq", "epsilonsq", "epsilon", "epsilon2"),
      eta_H_sq = c("eta_h_sq", "rank_eta_h_sq"),
      kendall_w = c("kendall_w", "kendallw", "w")), "effect-size")
  exact_value <- option("exact")
  exact <- if (length(exact_value) == 1L && tolower(as.character(exact_value)) %in% c("auto", "null", "na")) NULL else parse_bool(exact_value)
  continuity <- parse_bool(option("continuity"))
  p_adjust <- option("p-adjust")
  if (length(p_adjust) != 1L || !p_adjust %in% stats::p.adjust.methods) issue("Unknown p-value adjustment method.")
  label_meta <- resolve_label_metadata(df)
  has_group <- present(opts$group); has_x <- present(opts$x); has_y <- present(opts$y)
  has_within <- present(opts$within); has_subject <- present(opts[["subject-id"]])
  if (has_group && (has_x || has_y)) issue("Paired tests do not use --group.")
  if (has_within && (has_group || has_x || has_y || present(opts$vars))) issue("Friedman tests use --within and --subject-id only.")
  if (has_within && !has_subject) issue("Friedman tests require --subject-id.")
  if (has_subject && !has_within) issue("--subject-id is supported only with --within for Friedman.")
  if (has_within && !test_choice %in% c("auto", "friedman")) issue("--within conflicts with the requested test.")
  group_var <- if (has_group) opts$group else NULL
  groups <- NULL
  if (has_group) {
    if (length(group_var) != 1L || !group_var %in% names(df)) issue("Grouping variable not found.")
    groups <- nonparam_group_order(df[[group_var]], label_meta, group_var)
    if (length(groups) < 2L) issue("Grouping variable must have at least two non-missing levels.")
  }
  mode <- if (test_choice == "friedman" || has_within) "friedman" else if (test_choice == "auto") {
    if (has_x || has_y) "wilcoxon_paired" else if (has_group) {
      if (length(groups) == 2L) "mann_whitney" else "kruskal"
    } else "wilcoxon_one_sample"
  } else if (test_choice == "wilcoxon") {
    if (has_group) issue("Wilcoxon signed-rank does not use --group.")
    if (has_x || has_y) "wilcoxon_paired" else "wilcoxon_one_sample"
  } else test_choice
  omnibus <- mode %in% c("kruskal", "friedman")
  if (mode %in% c("kruskal", "mann_whitney") && !has_group) issue("Independent rank tests require --group.")
  if (mode == "mann_whitney" && length(groups) != 2L) issue("Mann-Whitney requires exactly two group levels.")
  if (!omnibus && posthoc != "none") issue("Post-hoc comparisons are supported only for Kruskal-Wallis and Friedman.")
  if (mode != "wilcoxon_one_sample" && present(opts$mu) && mu != 0) issue("--mu is supported only for one-sample Wilcoxon; other tests use a zero location-shift null.")
  if (omnibus && posthoc == "none" && present(opts$alternative) && alternative != "two.sided")
    issue("One-sided alternatives apply only to rank tests or requested post-hoc comparisons, not omnibus tests.")
  effect_size <- if (mode == "kruskal") "eta_H_sq" else if (mode == "friedman") "kendall_w" else {
    if (requested_effect %in% c("r", "rb")) requested_effect else "r"
  }
  if (requested_effect == "epsilon_sq") warning("Compatibility alias epsilon_sq denotes the existing untruncated rank eta_H_sq = (H-k+1)/(n-k), not epsilon squared H/(n-1).", call. = FALSE)
  posthoc_effect_size <- if (requested_effect %in% c("r", "rb")) requested_effect else "r"
  vars <- x_vars <- y_vars <- within_vars <- character(0)
  if (mode == "wilcoxon_paired") {
    if (!(has_x && has_y)) issue("Paired Wilcoxon requires both --x and --y.")
    if (present(opts$vars)) issue("Paired Wilcoxon uses --x and --y, not --vars.")
    x_vars <- parse_list(opts$x); y_vars <- parse_list(opts$y)
    if (!length(x_vars) || length(x_vars) != length(y_vars)) issue("--x and --y must have the same positive number of variables.")
    if (anyDuplicated(data.frame(x = x_vars, y = y_vars)) || any(x_vars == y_vars))
      issue("Paired variable pairs must be distinct and must not compare a variable to itself.")
    selected <- unique(c(x_vars, y_vars))
  } else if (mode == "friedman") {
    within_vars <- parse_list(opts$within)
    if (!has_subject || length(within_vars) < 2L) issue("Friedman requires --subject-id and at least two repeated measures.")
    if (anyDuplicated(within_vars) || opts[["subject-id"]] %in% within_vars) issue("Friedman measures must be distinct from each other and the subject ID.")
    selected <- within_vars
  } else {
    if (anyDuplicated(parse_list(opts$vars))) issue("Test variables must be distinct.")
    vars <- select_variables(df, opts$vars, group_var, default = get_config_value("modules.nonparametric.vars_default"), include_numeric = FALSE)
    if (!length(vars)) issue("No numeric variables available for analysis.")
    selected <- vars
  }
  required <- unique(c(selected, group_var, if (mode == "friedman") opts[["subject-id"]]))
  if (length(setdiff(required, names(df)))) issue(paste("Unknown variables:", paste(setdiff(required, names(df)), collapse = ", ")))
  for (variable in selected) {
    if (!is.numeric(df[[variable]])) issue(paste("Variable is not numeric:", variable))
    if (any(!is.finite(df[[variable]]) & !is.na(df[[variable]])))
      issue(paste("Non-finite numeric observations are not supported:", variable))
  }
  if (mode == "friedman" && anyDuplicated(df[[opts[["subject-id"]]]][!is.na(df[[opts[["subject-id"]]]])]))
    issue("Friedman requires one row per unique non-missing subject ID; duplicate subjects must be resolved explicitly.")
  entries <- tests <- list()
  count <- if (mode == "friedman") 1L else if (mode == "wilcoxon_paired") length(x_vars) else length(vars)
  for (i in seq_len(count)) {
    variables <- if (mode == "friedman") within_vars else if (mode == "wilcoxon_paired") c(x_vars[i], y_vars[i]) else vars[i]
    fields <- unique(c(variables, group_var, if (mode == "friedman") opts[["subject-id"]]))
    complete <- complete.cases(df[, fields, drop = FALSE]); rows <- which(complete)
    name <- paste(variables, collapse = if (mode == "friedman") "," else " vs ")
    selected_groups <- groups
    group_rows <- if (has_group) lapply(groups, function(g) intersect(rows, g$row_indices)) else NULL
    if (has_group) {
      if (mode == "mann_whitney" && any(lengths(group_rows) == 0)) issue(paste("Both groups need observations for:", name))
      if (mode == "kruskal") {
        retained <- lengths(group_rows) > 0L
        selected_groups <- groups[retained]; group_rows <- group_rows[retained]
        if (length(selected_groups) < 2L) issue(paste("At least two groups need observations for:", name))
      }
    }
    if (!length(rows)) issue(paste("No complete observations for:", name))
    entries[[i]] <- list(test_id = i, variable = name, variables = variables,
      n_complete = length(rows), mu = if (mode == "wilcoxon_one_sample") mu else 0,
      group_var = group_var, groups = selected_groups)
    if (mode == "mann_whitney") {
      entries[[i]]$x <- df[[variables[1]]][group_rows[[1]]]
      entries[[i]]$y <- df[[variables[1]]][group_rows[[2]]]
    } else if (mode == "kruskal") {
      entries[[i]]$samples <- lapply(group_rows, function(r) df[[variables[1]]][r])
    } else if (mode == "friedman") {
      entries[[i]]$matrix <- as.matrix(df[rows, variables, drop = FALSE])
    } else {
      entries[[i]]$x <- df[[variables[1]]][rows]
      if (mode == "wilcoxon_paired") entries[[i]]$y <- df[[variables[2]]][rows]
      differences <- if (mode == "wilcoxon_paired") entries[[i]]$x - entries[[i]]$y else entries[[i]]$x - mu
      if (any(!is.finite(differences))) issue(paste("Numeric differences overflow for:", name))
    }
    tests[[i]] <- list(test_id = i, variable = name, variables = variables,
      row_indices = rows, excluded_row_indices = which(!complete), n_total = nrow(df),
      n_complete = length(rows), missing_by_variable = lapply(df[, fields, drop = FALSE], function(v) which(is.na(v))),
      group_ids = if (has_group) vapply(selected_groups, "[[", integer(1), "group_id") else NULL,
      group_row_indices = group_rows,
      paired_row_indices = if (mode == "wilcoxon_paired") rows else NULL,
      zero_difference_row_indices = if (mode %in% c("wilcoxon_one_sample", "wilcoxon_paired")) rows[differences == 0] else NULL,
      subject_ids = if (mode == "friedman") df[[opts[["subject-id"]]]][rows] else NULL,
      subject_id_hex = if (mode == "friedman" && (is.numeric(df[[opts[["subject-id"]]]]) || inherits(df[[opts[["subject-id"]]]], c("Date", "POSIXt"))))
        sprintf("%a", as.numeric(df[[opts[["subject-id"]]]][rows])) else NULL,
      posthoc_pairs = if (omnibus && posthoc == "pairwise") combn(if (mode == "kruskal")
        vapply(selected_groups, "[[", integer(1), "group_id") else seq_along(variables), 2L, simplify = FALSE) else NULL)
  }

  uses_wilcox <- !omnibus
  resolved_test <- if (mode %in% c("wilcoxon_one_sample", "wilcoxon_paired")) "wilcoxon" else mode
  resolved_options <- list(test = resolved_test, mode = mode, vars = vars, x = x_vars, y = y_vars,
    group = group_var, within = within_vars, subject_id = if (mode == "friedman") opts[["subject-id"]] else NULL,
    mu = if (mode == "wilcoxon_one_sample") mu else NULL,
    alternative = if (uses_wilcox || posthoc == "pairwise") alternative else NULL,
    exact = if (uses_wilcox || posthoc == "pairwise") exact else NULL,
    exact_policy = if (uses_wilcox || posthoc == "pairwise") if (is.null(exact)) "auto" else if (exact) "requested" else "asymptotic" else NULL,
    continuity = if (uses_wilcox || posthoc == "pairwise") continuity else NULL,
    conf_level = if (uses_wilcox || posthoc == "pairwise") conf_level else NULL,
    posthoc = if (omnibus) posthoc else NULL, p_adjust = if (omnibus) p_adjust else NULL,
    effect_size = effect_size, requested_effect_size = requested_effect,
    posthoc_effect_size = if (omnibus && posthoc == "pairwise") posthoc_effect_size else NULL, digits = digits)
  design <- list(rows = nrow(df), variables = selected, tests = tests,
    variable_classes = lapply(df[, required, drop = FALSE], class),
    factor_levels = lapply(df[, required, drop = FALSE], function(v) if (is.factor(v)) levels(v) else NULL),
    group_order = groups, group_order_rule = if (has_group) "observed factor levels in declared order; otherwise sorted non-missing raw values" else NULL,
    missing_group_row_indices = if (has_group) which(is.na(df[[group_var]])) else NULL,
    missing = if (mode == "friedman") "Complete blocks including non-missing unique subject ID; same blocks for every post-hoc comparison." else
      "Complete cases separately for each outcome or paired-variable pair, including non-missing group.",
    effect_size = if (effect_size == "eta_H_sq") "Untruncated rank eta_H_sq = (H-k+1)/(n-k), not epsilon squared H/(n-1); may be negative, unavailable n<=k." else
      if (effect_size == "kendall_w") "Kendall W = tie-corrected Friedman Q / (n*(k-1))." else
      if (effect_size == "rb") "Signed rank balance / total absolute ranks; independent 2U/(n1*n2)-1." else
        "r = uncorrected-for-continuity, tie-adjusted standardized rank statistic z / sqrt(N); N=nonzero signed differences or n1+n2.",
    posthoc_family = if (omnibus && posthoc == "pairwise") "All planned pairs per outcome/omnibus test, including unavailable pairs in p.adjust n; no significance filtering." else NULL,
    exact_policy = "Use the installed stats::wilcox.test policy; record actual method and exact/continuity flags per test, with ties/zeros and warnings. No simulated inference or RNG.",
    intervals = "R location/pseudomedian confidence intervals, not effect-size or difference-of-medians CIs; actual coverage and non-finite bounds explicitly recorded.")
  nlss_resolve_request(resolved_options, design)
  calculated <- lapply(entries, compute_nonparametric, mode = mode, alternative = alternative,
    conf_level = conf_level, exact = exact, continuity = continuity, effect_size = effect_size,
    posthoc_effect_size = posthoc_effect_size, posthoc = posthoc, p_adjust = p_adjust, digits = digits)
  summary_df <- do.call(rbind, lapply(calculated, "[[", "summary"))
  diagnostics_df <- do.call(rbind, lapply(calculated, "[[", "diagnostics"))
  posthoc_df <- do.call(rbind, lapply(calculated, "[[", "posthoc"))
  if (is.null(posthoc_df)) posthoc_df <- data.frame()
  for (column in c("variable", "measure_1", "measure_2", "group"))
    summary_df <- add_variable_label_column(summary_df, label_meta, var_col = column)
  if (nrow(posthoc_df)) {
    for (column in c("variable", "group")) posthoc_df <- add_variable_label_column(posthoc_df, label_meta, var_col = column)
    if (mode == "friedman") for (column in c("group_1", "group_2"))
      posthoc_df <- add_variable_label_column(posthoc_df, label_meta, var_col = column)
  }
  design$inference <- diagnostics_df
  nlss_resolve_request(resolved_options, design)
  note_tokens <- build_nonparam_note_tokens(if (uses_wilcox) alternative else NULL,
    if (uses_wilcox) conf_level else NULL, if (uses_wilcox) exact else NULL,
    if (uses_wilcox) continuity else NULL, format_effect_label(effect_size), posthoc, p_adjust)
  note_tokens$note_default <- paste(note_tokens$note_default,
    "Methods and actual interval coverage are recorded per test.",
    "Wilcoxon CIs describe location/pseudomedian, not the effect size or a difference of sample medians.",
    if (uses_wilcox && effect_size == "r") "r uses tie-adjusted z without continuity correction." else "",
    if (effect_size == "eta_H_sq") "Untruncated rank eta_H_sq = (H-k+1)/(n-k); this bias-adjusted rank effect can be negative." else "")
  if (any(summary_df$ci_status %in% c("unavailable", "reduced_coverage")))
    note_tokens$note_default <- paste(note_tokens$note_default, "Some requested intervals are unavailable or have reduced coverage; consult each row.")
  template_override <- resolve_template_override(opts$template, module = "nonparametric")
  template_path <- if (!is.null(template_override)) template_override else resolve_template_path("nonparametric.default", "nonparametric/default-template.md")
  template_path <- nlss_freeze_template(template_path, "nonparametric.main")
  meta <- get_template_meta(template_path)
  narrative_conf <- if (uses_wilcox) conf_level else NA_real_
  narrative_rows <- build_nonparam_narrative_rows(summary_df, digits, narrative_conf)
  text <- paste(vapply(narrative_rows, "[[", character(1), "full_sentence"), collapse = "\n")
  context <- list(tokens = c(list(table_body = build_nonparam_table_body(summary_df, digits, meta$table)$body,
    narrative_default = text), note_tokens), narrative_rows = narrative_rows)
  nlss_stage_report(file.path(out_dir, "report_canonical.md"), "Nonparametric tests",
    format_nlss_table(summary_df, digits, note_tokens$note_default), text,
    analysis_flags = resolved_options, template_path = template_path, template_context = context)
  if (nrow(posthoc_df)) {
    notes <- build_posthoc_note_tokens(posthoc, p_adjust)
    notes$note_default <- paste(notes$note_default, "Family includes all planned comparisons per outcome.",
      "Direction is first minus second; r uses tie-adjusted z without continuity correction.",
      "Unavailable comparisons remain explicit; intervals are unadjusted location/pseudomedian intervals.")
    path <- if (!is.null(template_override)) template_override else resolve_template_path("nonparametric.posthoc", "nonparametric/posthoc-template.md")
    path <- nlss_freeze_template(path, "nonparametric.posthoc")
    meta <- get_template_meta(path)
    rows <- build_posthoc_narrative_rows(posthoc_df, digits)
    text <- paste(vapply(rows, "[[", character(1), "full_sentence"), collapse = "\n")
    context <- list(tokens = c(list(table_body = build_posthoc_table_body(posthoc_df, digits, meta$table)$body,
      narrative_default = text), notes), narrative_rows = rows)
    nlss_stage_report(file.path(out_dir, "report_canonical.md"), "Nonparametric post-hoc",
      format_posthoc_table(posthoc_df, digits, notes$note_default), text,
      analysis_flags = resolved_options, template_path = path, template_context = context)
  }
  results <- list(summary_df = summary_df, posthoc_df = posthoc_df, diagnostics_df = diagnostics_df)
  nlss_set_result(results)
  if (parse_bool(opts$log, get_config_value("defaults.log"))) {
    ctx <- get_run_context()
    nlss_stage_log(out_dir, "nonparametric", ctx$prompt, ctx$commands, results,
      options = resolved_options, user_prompt = get_user_prompt(opts))
  }
}

nlss_run_main("nonparametric", main)
