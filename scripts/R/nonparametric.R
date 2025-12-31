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
source(file.path(bootstrap_dir, "lib", "paths.R"))
source_lib("cli.R")
source_lib("config.R")
source_lib("io.R")
source_lib("data_utils.R")
source_lib("formatting.R")


# Static analysis aliases for source_lib-defined functions.
render_output_path <- get("render_output_path", mode = "function")
add_group_label_column <- get("add_group_label_column", mode = "function")
add_variable_label_column <- get("add_variable_label_column", mode = "function")
resolve_label_metadata <- get("resolve_label_metadata", mode = "function")
source_lib <- get("source_lib", mode = "function")

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
  cat("  --effect-size TYPE     r/rb/epsilon_sq/kendall_w (default: r)\n")
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

  test_default <- resolve_config_value("modules.nonparametric.test", "auto")
  opts$test <- resolve_prompt("Test type (auto/wilcoxon/mann_whitney/kruskal/friedman)", test_default)

  opts$vars <- resolve_prompt("Variables (comma-separated, blank for defaults)", "")
  opts$group <- resolve_prompt("Grouping variable (blank if not used)", "")
  opts$x <- resolve_prompt("Measure 1 variables (comma-separated, paired; blank if not used)", "")
  opts$y <- resolve_prompt("Measure 2 variables (comma-separated, paired; blank if not used)", "")
  opts$within <- resolve_prompt("Repeated measures (comma-separated, Friedman; blank if not used)", "")
  opts$`subject-id` <- resolve_prompt("Subject ID (Friedman; blank if not used)", "")

  mu_default <- resolve_config_value("modules.nonparametric.mu", 0.0)
  alternative_default <- resolve_config_value("modules.nonparametric.alternative", "two.sided")
  exact_default <- resolve_config_value("modules.nonparametric.exact", "auto")
  continuity_default <- resolve_config_value("modules.nonparametric.continuity", TRUE)
  conf_default <- resolve_config_value("modules.nonparametric.conf_level", 0.95)
  posthoc_default <- resolve_config_value("modules.nonparametric.posthoc", "none")
  adjust_default <- resolve_config_value("modules.nonparametric.p_adjust", "holm")
  effect_default <- resolve_config_value("modules.nonparametric.effect_size", "r")
  digits_default <- resolve_config_value("defaults.digits", 2)

  opts$mu <- resolve_prompt("Test value (mu)", as.character(mu_default))
  opts$alternative <- resolve_prompt("Alternative (two.sided/greater/less)", alternative_default)
  opts$exact <- resolve_prompt("Exact p-values TRUE/FALSE/auto", as.character(exact_default))
  opts$continuity <- resolve_prompt("Continuity correction TRUE/FALSE", ifelse(isTRUE(continuity_default), "TRUE", "FALSE"))
  opts$`conf-level` <- resolve_prompt("Confidence level", as.character(conf_default))
  opts$posthoc <- resolve_prompt("Post-hoc (none/pairwise)", posthoc_default)
  opts$`p-adjust` <- resolve_prompt("P-value adjustment", adjust_default)
  opts$`effect-size` <- resolve_prompt("Effect size (r/rb/epsilon_sq/kendall_w)", effect_default)
  opts$digits <- resolve_prompt("Rounding digits", as.character(digits_default))
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

resolve_default_out <- function() {
  if (exists("get_default_out", mode = "function")) {
    return(get("get_default_out", mode = "function")())
  }
  "./outputs/tmp"
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

resolve_select_variables <- function(df, vars, group_var = NULL, default = "numeric") {
  if (exists("select_variables", mode = "function")) {
    return(get("select_variables", mode = "function")(
      df,
      vars,
      group_var = group_var,
      default = default,
      include_numeric = FALSE
    ))
  }
  available <- names(df)
  if (is.null(vars) || vars == "") {
    selected <- available[sapply(df, is.numeric)]
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

resolve_get_levels <- function(vec) {
  if (exists("get_levels", mode = "function")) {
    return(get("get_levels", mode = "function")(vec))
  }
  if (is.factor(vec)) {
    return(as.character(levels(vec)))
  }
  values <- unique(vec[!is.na(vec)])
  if (length(values) == 0) return(character(0))
  if (is.numeric(values)) {
    return(as.character(sort(values)))
  }
  as.character(sort(values))
}

resolve_get_template_path <- function(key, default_relative = NULL) {
  if (exists("resolve_template_path", mode = "function")) {
    return(get("resolve_template_path", mode = "function")(key, default_relative))
  }
  if (is.null(default_relative) || !nzchar(default_relative)) return(NULL)
  if (exists("get_assets_dir", mode = "function")) {
    return(file.path(get("get_assets_dir", mode = "function")(), default_relative))
  }
  file.path(getwd(), "nlss", "assets", default_relative)
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

resolve_normalize_table_columns <- function(columns, default_specs) {
  if (exists("normalize_table_columns", mode = "function")) {
    return(get("normalize_table_columns", mode = "function")(columns, default_specs))
  }
  default_specs
}

resolve_drop_empty_columns <- function(columns, rows) {
  if (exists("drop_empty_columns", mode = "function")) {
    return(get("drop_empty_columns", mode = "function")(columns, rows))
  }
  list(columns = columns, rows = rows)
}

resolve_render_markdown_table <- function(headers, rows) {
  if (exists("render_markdown_table", mode = "function")) {
    return(get("render_markdown_table", mode = "function")(headers, rows))
  }
  ""
}

resolve_as_cell_text <- function(value) {
  if (exists("as_cell_text", mode = "function")) {
    return(get("as_cell_text", mode = "function")(value))
  }
  if (length(value) == 0 || is.null(value) || is.na(value)) return("")
  as.character(value)
}

resolve_append_nlss_report <- function(path, analysis_label, nlss_table, nlss_text, analysis_flags = NULL, template_path = NULL, template_context = NULL) {
  if (exists("append_nlss_report", mode = "function")) {
    return(get("append_nlss_report", mode = "function")(
      path,
      analysis_label,
      nlss_table,
      nlss_text,
      analysis_flags = analysis_flags,
      template_path = template_path,
      template_context = template_context
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

emit_input_issue <- function(out_dir, opts, message, details = list(), status = "invalid_input") {
  log_default <- resolve_config_value("defaults.log", TRUE)
  if (resolve_parse_bool(opts$log, default = log_default)) {
    ctx <- resolve_get_run_context()
    resolve_append_analysis_log(
      out_dir,
      module = "nonparametric",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(
        status = status,
        message = message,
        details = details
      ),
      options = details,
      user_prompt = resolve_get_user_prompt(opts)
    )
  }
  stop(message)
}

normalize_alternative <- function(value, default = "two.sided") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (val %in% c("two.sided", "two-sided", "two")) return("two.sided")
  if (val %in% c("greater", "less")) return(val)
  default
}

normalize_test <- function(value, default = "auto") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (val %in% c("auto", "automatic")) return("auto")
  if (val %in% c("wilcoxon", "wilcox", "signed", "signed-rank", "signed_rank")) return("wilcoxon")
  if (val %in% c("mann_whitney", "mann-whitney", "mannwhitney", "ranksum", "rank-sum", "u")) return("mann_whitney")
  if (val %in% c("kruskal", "kruskal-wallis", "kruskal_wallis")) return("kruskal")
  if (val %in% c("friedman")) return("friedman")
  default
}

normalize_exact <- function(value, default = "auto") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(as.character(val))
  if (val %in% c("auto", "null", "na")) return(NULL)
  resolve_parse_bool(val, default = FALSE)
}

normalize_posthoc <- function(value, default = "none") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (val %in% c("none", "off", "false")) return("none")
  if (val %in% c("pairwise", "wilcoxon", "pair")) return("pairwise")
  default
}

normalize_effect_size <- function(value, default = "r") {
  val <- if (!is.null(value) && value != "") value else default
  val <- tolower(val)
  if (val %in% c("r")) return("r")
  if (val %in% c("rb", "rbc", "rank-biserial", "rank_biserial")) return("rb")
  if (val %in% c("epsilon_sq", "epsilonsq", "epsilon", "epsilon2")) return("epsilon_sq")
  if (val %in% c("kendall_w", "kendallw", "w")) return("kendall_w")
  default
}

resolve_effect_size_choice <- function(requested, mode, default) {
  choice <- normalize_effect_size(requested, default)
  if (mode %in% c("kruskal")) {
    if (choice != "epsilon_sq") choice <- "epsilon_sq"
  } else if (mode %in% c("friedman")) {
    if (choice != "kendall_w") choice <- "kendall_w"
  } else {
    if (!choice %in% c("r", "rb")) choice <- "r"
  }
  choice
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
  if (effect_size == "epsilon_sq") return("epsilon_sq")
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

calc_r_from_z <- function(z, n) {
  if (is.na(z) || is.na(n) || n <= 0) return(NA_real_)
  z / sqrt(n)
}

calc_rank_biserial_signed <- function(diffs) {
  diffs <- diffs[!is.na(diffs)]
  diffs <- diffs[diffs != 0]
  n <- length(diffs)
  if (n == 0) {
    return(list(value = NA_real_, w_pos = NA_real_, w_neg = NA_real_, n = 0))
  }
  ranks <- rank(abs(diffs))
  w_pos <- sum(ranks[diffs > 0])
  w_neg <- sum(ranks[diffs < 0])
  denom <- n * (n + 1) / 2
  value <- if (denom > 0) (w_pos - w_neg) / denom else NA_real_
  list(value = value, w_pos = w_pos, w_neg = w_neg, n = n)
}

calc_z_signed <- function(w_pos, n) {
  if (is.na(w_pos) || is.na(n) || n <= 0) return(NA_real_)
  mu <- n * (n + 1) / 4
  sigma <- sqrt(n * (n + 1) * (2 * n + 1) / 24)
  if (sigma == 0) return(NA_real_)
  (w_pos - mu) / sigma
}

calc_u_from_w <- function(w_stat, n1) {
  if (is.na(w_stat) || is.na(n1)) return(NA_real_)
  w_stat - n1 * (n1 + 1) / 2
}

calc_z_mann_whitney <- function(u_stat, n1, n2) {
  if (is.na(u_stat) || n1 <= 0 || n2 <= 0) return(NA_real_)
  mu <- n1 * n2 / 2
  sigma <- sqrt(n1 * n2 * (n1 + n2 + 1) / 12)
  if (sigma == 0) return(NA_real_)
  (u_stat - mu) / sigma
}

calc_rank_biserial_mann_whitney <- function(u_stat, n1, n2) {
  if (is.na(u_stat) || n1 <= 0 || n2 <= 0) return(NA_real_)
  (2 * u_stat) / (n1 * n2) - 1
}

calc_epsilon_sq <- function(h_stat, n, k) {
  if (is.na(h_stat) || n <= k || k < 2) return(NA_real_)
  (h_stat - k + 1) / (n - k)
}

calc_kendall_w <- function(q_stat, n, k) {
  if (is.na(q_stat) || n <= 0 || k < 2) return(NA_real_)
  q_stat / (n * (k - 1))
}

safe_wilcox_test <- function(...) {
  tryCatch(suppressWarnings(wilcox.test(...)), error = function(e) NULL)
}

extract_wilcox_flags <- function(test) {
  method <- if (!is.null(test) && !is.null(test$method)) tolower(test$method) else ""
  list(
    exact_used = grepl("exact", method),
    continuity_used = grepl("continuity", method)
  )
}

build_group_summary <- function(values, groups, digits) {
  levels <- get_group_levels(groups)
  if (length(levels) == 0) return("")
  parts <- character(0)
  for (lvl in levels) {
    vals <- values[groups == lvl]
    med <- ifelse(length(vals) > 0, median(vals, na.rm = TRUE), NA_real_)
    iqr <- ifelse(length(vals) > 0, IQR(vals, na.rm = TRUE), NA_real_)
    part <- paste0(lvl, ": ", format_stat(med, digits), " (IQR ", format_stat(iqr, digits), ")")
    parts <- c(parts, part)
  }
  paste(parts, collapse = "; ")
}

get_group_levels <- function(vec) {
  if (is.factor(vec)) vec <- droplevels(vec)
  resolve_get_levels(vec)
}

build_within_summary <- function(df, within_vars, digits) {
  parts <- character(0)
  for (var in within_vars) {
    vals <- df[[var]]
    med <- ifelse(length(vals) > 0, median(vals, na.rm = TRUE), NA_real_)
    iqr <- ifelse(length(vals) > 0, IQR(vals, na.rm = TRUE), NA_real_)
    part <- paste0(var, ": ", format_stat(med, digits), " (IQR ", format_stat(iqr, digits), ")")
    parts <- c(parts, part)
  }
  paste(parts, collapse = "; ")
}

build_summary_one_sample <- function(df, vars, mu, alternative, conf_level, exact, continuity, effect_size) {
  rows <- list()
  diagnostics <- list()
  test_label <- format_test_label("wilcoxon_one_sample")

  for (var in vars) {
    vec <- df[[var]]
    if (!is.numeric(vec)) stop(paste("Variable is not numeric:", var))
    clean <- vec[!is.na(vec)]
    n <- length(clean)
    median_val <- ifelse(n > 0, median(clean), NA_real_)
    iqr_val <- ifelse(n > 0, IQR(clean), NA_real_)
    diffs <- clean - mu
    rank_info <- calc_rank_biserial_signed(diffs)
    zero_diffs <- sum(diffs == 0, na.rm = TRUE)
    ties <- length(unique(abs(diffs[diffs != 0]))) < length(diffs[diffs != 0])

    test <- if (n > 0) {
      safe_wilcox_test(
        clean,
        mu = mu,
        alternative = alternative,
        conf.int = TRUE,
        conf.level = conf_level,
        exact = exact,
        correct = continuity
      )
    } else {
      NULL
    }

    stat_val <- if (!is.null(test)) unname(test$statistic) else rank_info$w_pos
    p_val <- if (!is.null(test)) test$p.value else NA_real_
    ci_low <- if (!is.null(test) && !is.null(test$conf.int)) test$conf.int[1] else NA_real_
    ci_high <- if (!is.null(test) && !is.null(test$conf.int)) test$conf.int[2] else NA_real_

    z_val <- calc_z_signed(rank_info$w_pos, rank_info$n)
    r_val <- calc_r_from_z(z_val, rank_info$n)
    rb_val <- rank_info$value
    effect_val <- if (effect_size == "rb") rb_val else r_val

    flags <- extract_wilcox_flags(test)
    diagnostics[[length(diagnostics) + 1]] <- data.frame(
      test_type = "wilcoxon_one_sample",
      variable = var,
      n_total = n,
      n_nonzero = rank_info$n,
      zero_diff_n = zero_diffs,
      ties = ties,
      exact_used = flags$exact_used,
      continuity_used = flags$continuity_used,
      stringsAsFactors = FALSE
    )

    rows[[length(rows) + 1]] <- data.frame(
      test_type = test_label,
      variable = var,
      measure_1 = "",
      measure_2 = "",
      group = "",
      group_1 = "",
      group_2 = "",
      n_1 = n,
      n_2 = NA_real_,
      n_total = n,
      median_1 = NA_real_,
      median_2 = NA_real_,
      iqr_1 = NA_real_,
      iqr_2 = NA_real_,
      median = median_val,
      iqr = iqr_val,
      median_diff = ifelse(n > 0, median(diffs, na.rm = TRUE), NA_real_),
      iqr_diff = ifelse(n > 0, IQR(diffs, na.rm = TRUE), NA_real_),
      group_summary = "",
      within_summary = "",
      stat_label = format_stat_label("wilcoxon_one_sample"),
      statistic = stat_val,
      df = NA_real_,
      p = p_val,
      effect_size_label = format_effect_label(effect_size),
      effect_size_value = effect_val,
      ci_low = ci_low,
      ci_high = ci_high,
      mu = mu,
      stringsAsFactors = FALSE
    )
  }

  list(summary = do.call(rbind, rows), diagnostics = do.call(rbind, diagnostics))
}

build_summary_paired <- function(df, x_vars, y_vars, alternative, conf_level, exact, continuity, effect_size) {
  rows <- list()
  diagnostics <- list()
  test_label <- format_test_label("wilcoxon_paired")

  for (i in seq_along(x_vars)) {
    x_name <- x_vars[i]
    y_name <- y_vars[i]
    x <- df[[x_name]]
    y <- df[[y_name]]
    if (!is.numeric(x) || !is.numeric(y)) {
      stop(paste("Paired variables must be numeric:", x_name, y_name))
    }
    complete <- !is.na(x) & !is.na(y)
    x <- x[complete]
    y <- y[complete]
    n <- length(x)

    median_1 <- ifelse(n > 0, median(x), NA_real_)
    median_2 <- ifelse(n > 0, median(y), NA_real_)
    iqr_1 <- ifelse(n > 0, IQR(x), NA_real_)
    iqr_2 <- ifelse(n > 0, IQR(y), NA_real_)
    diffs <- x - y
    median_diff <- ifelse(n > 0, median(diffs), NA_real_)
    iqr_diff <- ifelse(n > 0, IQR(diffs), NA_real_)

    rank_info <- calc_rank_biserial_signed(diffs)
    zero_diffs <- sum(diffs == 0, na.rm = TRUE)
    ties <- length(unique(abs(diffs[diffs != 0]))) < length(diffs[diffs != 0])

    test <- if (n > 0) {
      safe_wilcox_test(
        x,
        y,
        paired = TRUE,
        alternative = alternative,
        conf.int = TRUE,
        conf.level = conf_level,
        exact = exact,
        correct = continuity
      )
    } else {
      NULL
    }

    stat_val <- if (!is.null(test)) unname(test$statistic) else rank_info$w_pos
    p_val <- if (!is.null(test)) test$p.value else NA_real_
    ci_low <- if (!is.null(test) && !is.null(test$conf.int)) test$conf.int[1] else NA_real_
    ci_high <- if (!is.null(test) && !is.null(test$conf.int)) test$conf.int[2] else NA_real_

    z_val <- calc_z_signed(rank_info$w_pos, rank_info$n)
    r_val <- calc_r_from_z(z_val, rank_info$n)
    rb_val <- rank_info$value
    effect_val <- if (effect_size == "rb") rb_val else r_val

    flags <- extract_wilcox_flags(test)
    diagnostics[[length(diagnostics) + 1]] <- data.frame(
      test_type = "wilcoxon_paired",
      variable = paste0(x_name, " vs ", y_name),
      n_total = n,
      n_nonzero = rank_info$n,
      zero_diff_n = zero_diffs,
      ties = ties,
      exact_used = flags$exact_used,
      continuity_used = flags$continuity_used,
      stringsAsFactors = FALSE
    )

    rows[[length(rows) + 1]] <- data.frame(
      test_type = test_label,
      variable = paste0(x_name, " vs ", y_name),
      measure_1 = x_name,
      measure_2 = y_name,
      group = "",
      group_1 = "",
      group_2 = "",
      n_1 = n,
      n_2 = NA_real_,
      n_total = n,
      median_1 = median_1,
      median_2 = median_2,
      iqr_1 = iqr_1,
      iqr_2 = iqr_2,
      median = NA_real_,
      iqr = NA_real_,
      median_diff = median_diff,
      iqr_diff = iqr_diff,
      group_summary = "",
      within_summary = "",
      stat_label = format_stat_label("wilcoxon_paired"),
      statistic = stat_val,
      df = NA_real_,
      p = p_val,
      effect_size_label = format_effect_label(effect_size),
      effect_size_value = effect_val,
      ci_low = ci_low,
      ci_high = ci_high,
      mu = NA_real_,
      stringsAsFactors = FALSE
    )
  }

  list(summary = do.call(rbind, rows), diagnostics = do.call(rbind, diagnostics))
}

build_summary_mann_whitney <- function(df, vars, group_var, alternative, conf_level, exact, continuity, effect_size) {
  rows <- list()
  diagnostics <- list()
  group_vals <- df[[group_var]]
  levels <- get_group_levels(group_vals)
  if (length(levels) < 2) stop("Grouping variable must have at least two levels.")
  group1 <- as.character(levels[1])
  group2 <- as.character(levels[2])
  test_label <- format_test_label("mann_whitney")

  for (var in vars) {
    vec <- df[[var]]
    if (!is.numeric(vec)) stop(paste("Variable is not numeric:", var))
    complete <- !is.na(vec) & !is.na(group_vals)
    vec <- vec[complete]
    groups <- as.character(group_vals[complete])

    x1 <- vec[groups == group1]
    x2 <- vec[groups == group2]
    n1 <- length(x1)
    n2 <- length(x2)

    median_1 <- ifelse(n1 > 0, median(x1), NA_real_)
    median_2 <- ifelse(n2 > 0, median(x2), NA_real_)
    iqr_1 <- ifelse(n1 > 0, IQR(x1), NA_real_)
    iqr_2 <- ifelse(n2 > 0, IQR(x2), NA_real_)

    test <- if (n1 > 0 && n2 > 0) {
      safe_wilcox_test(
        x1,
        x2,
        alternative = alternative,
        conf.int = TRUE,
        conf.level = conf_level,
        exact = exact,
        correct = continuity
      )
    } else {
      NULL
    }

    w_stat <- if (!is.null(test)) unname(test$statistic) else NA_real_
    u_stat <- calc_u_from_w(w_stat, n1)
    p_val <- if (!is.null(test)) test$p.value else NA_real_
    ci_low <- if (!is.null(test) && !is.null(test$conf.int)) test$conf.int[1] else NA_real_
    ci_high <- if (!is.null(test) && !is.null(test$conf.int)) test$conf.int[2] else NA_real_

    z_val <- calc_z_mann_whitney(u_stat, n1, n2)
    r_val <- calc_r_from_z(z_val, n1 + n2)
    rb_val <- calc_rank_biserial_mann_whitney(u_stat, n1, n2)
    effect_val <- if (effect_size == "rb") rb_val else r_val

    ties <- length(unique(c(x1, x2))) < length(c(x1, x2))
    flags <- extract_wilcox_flags(test)
    diagnostics[[length(diagnostics) + 1]] <- data.frame(
      test_type = "mann_whitney",
      variable = var,
      group = group1,
      n = n1,
      ties = ties,
      exact_used = flags$exact_used,
      continuity_used = flags$continuity_used,
      stringsAsFactors = FALSE
    )
    diagnostics[[length(diagnostics) + 1]] <- data.frame(
      test_type = "mann_whitney",
      variable = var,
      group = group2,
      n = n2,
      ties = ties,
      exact_used = flags$exact_used,
      continuity_used = flags$continuity_used,
      stringsAsFactors = FALSE
    )

    rows[[length(rows) + 1]] <- data.frame(
      test_type = test_label,
      variable = var,
      measure_1 = "",
      measure_2 = "",
      group = group_var,
      group_1 = group1,
      group_2 = group2,
      n_1 = n1,
      n_2 = n2,
      n_total = n1 + n2,
      median_1 = median_1,
      median_2 = median_2,
      iqr_1 = iqr_1,
      iqr_2 = iqr_2,
      median = NA_real_,
      iqr = NA_real_,
      median_diff = median_1 - median_2,
      iqr_diff = NA_real_,
      group_summary = "",
      within_summary = "",
      stat_label = format_stat_label("mann_whitney"),
      statistic = u_stat,
      df = NA_real_,
      p = p_val,
      effect_size_label = format_effect_label(effect_size),
      effect_size_value = effect_val,
      ci_low = ci_low,
      ci_high = ci_high,
      mu = NA_real_,
      stringsAsFactors = FALSE
    )
  }

  list(summary = do.call(rbind, rows), diagnostics = do.call(rbind, diagnostics))
}

build_summary_kruskal <- function(df, vars, group_var, effect_size, digits) {
  rows <- list()
  diagnostics <- list()
  group_vals <- df[[group_var]]
  levels <- get_group_levels(group_vals)
  if (length(levels) < 2) stop("Grouping variable must have at least two levels.")
  test_label <- format_test_label("kruskal")

  for (var in vars) {
    vec <- df[[var]]
    if (!is.numeric(vec)) stop(paste("Variable is not numeric:", var))
    complete <- !is.na(vec) & !is.na(group_vals)
    vec <- vec[complete]
    groups <- as.character(group_vals[complete])
    n_total <- length(vec)

    test <- if (n_total > 0) {
      tryCatch(kruskal.test(vec ~ groups), error = function(e) NULL)
    } else {
      NULL
    }

    h_stat <- if (!is.null(test)) unname(test$statistic) else NA_real_
    df_val <- if (!is.null(test)) unname(test$parameter) else NA_real_
    p_val <- if (!is.null(test)) test$p.value else NA_real_
    epsilon_sq <- calc_epsilon_sq(h_stat, n_total, length(levels))

    group_summary <- build_group_summary(vec, groups, digits)
    size_table <- table(groups)
    diagnostics[[length(diagnostics) + 1]] <- data.frame(
      test_type = "kruskal",
      variable = var,
      group = group_var,
      n_total = n_total,
      group_levels = paste(names(size_table), collapse = ","),
      group_sizes = paste(as.integer(size_table), collapse = ","),
      stringsAsFactors = FALSE
    )

    rows[[length(rows) + 1]] <- data.frame(
      test_type = test_label,
      variable = var,
      measure_1 = "",
      measure_2 = "",
      group = group_var,
      group_1 = "",
      group_2 = "",
      n_1 = NA_real_,
      n_2 = NA_real_,
      n_total = n_total,
      median_1 = NA_real_,
      median_2 = NA_real_,
      iqr_1 = NA_real_,
      iqr_2 = NA_real_,
      median = NA_real_,
      iqr = NA_real_,
      median_diff = NA_real_,
      iqr_diff = NA_real_,
      group_summary = group_summary,
      within_summary = "",
      stat_label = format_stat_label("kruskal"),
      statistic = h_stat,
      df = df_val,
      p = p_val,
      effect_size_label = format_effect_label(effect_size),
      effect_size_value = epsilon_sq,
      ci_low = NA_real_,
      ci_high = NA_real_,
      mu = NA_real_,
      stringsAsFactors = FALSE
    )
  }

  list(summary = do.call(rbind, rows), diagnostics = do.call(rbind, diagnostics))
}

build_summary_friedman <- function(df, within_vars, subject_id, effect_size, digits) {
  rows <- list()
  diagnostics <- list()
  test_label <- format_test_label("friedman")

  if (!subject_id %in% names(df)) stop("Subject ID column not found: ", subject_id)

  data_subset <- df[, c(subject_id, within_vars), drop = FALSE]
  complete <- complete.cases(data_subset)
  data_subset <- data_subset[complete, , drop = FALSE]
  n_total <- nrow(data_subset)

  for (var in within_vars) {
    if (!is.numeric(data_subset[[var]])) {
      stop(paste("Repeated measure is not numeric:", var))
    }
  }

  test <- if (n_total > 0) {
    tryCatch(friedman.test(as.matrix(data_subset[, within_vars, drop = FALSE])), error = function(e) NULL)
  } else {
    NULL
  }

  q_stat <- if (!is.null(test)) unname(test$statistic) else NA_real_
  df_val <- if (!is.null(test)) unname(test$parameter) else NA_real_
  p_val <- if (!is.null(test)) test$p.value else NA_real_
  k <- length(within_vars)
  w_val <- calc_kendall_w(q_stat, n_total, k)

  within_summary <- build_within_summary(data_subset[, within_vars, drop = FALSE], within_vars, digits)
  diagnostics[[length(diagnostics) + 1]] <- data.frame(
    test_type = "friedman",
    variable = paste(within_vars, collapse = ","),
    n_total = n_total,
    k = k,
    stringsAsFactors = FALSE
  )

  rows[[length(rows) + 1]] <- data.frame(
    test_type = test_label,
    variable = paste(within_vars, collapse = ","),
    measure_1 = "",
    measure_2 = "",
    group = "",
    group_1 = "",
    group_2 = "",
    n_1 = NA_real_,
    n_2 = NA_real_,
    n_total = n_total,
    median_1 = NA_real_,
    median_2 = NA_real_,
    iqr_1 = NA_real_,
    iqr_2 = NA_real_,
    median = NA_real_,
    iqr = NA_real_,
    median_diff = NA_real_,
    iqr_diff = NA_real_,
    group_summary = "",
    within_summary = within_summary,
    stat_label = format_stat_label("friedman"),
    statistic = q_stat,
    df = df_val,
    p = p_val,
    effect_size_label = format_effect_label(effect_size),
    effect_size_value = w_val,
    ci_low = NA_real_,
    ci_high = NA_real_,
    mu = NA_real_,
    stringsAsFactors = FALSE
  )

  list(summary = do.call(rbind, rows), diagnostics = do.call(rbind, diagnostics))
}

build_posthoc_kruskal <- function(df, var, group_var, alternative, conf_level, exact, continuity, effect_size, p_adjust) {
  group_vals <- df[[group_var]]
  levels <- get_group_levels(group_vals)
  if (length(levels) < 2) return(data.frame())
  vec <- df[[var]]
  complete <- !is.na(vec) & !is.na(group_vals)
  vec <- vec[complete]
  groups <- as.character(group_vals[complete])

  pairs <- combn(levels, 2, simplify = FALSE)
  rows <- list()
  p_values <- numeric(0)

  for (pair in pairs) {
    g1 <- pair[1]
    g2 <- pair[2]
    x1 <- vec[groups == g1]
    x2 <- vec[groups == g2]
    n1 <- length(x1)
    n2 <- length(x2)

    test <- if (n1 > 0 && n2 > 0) {
      safe_wilcox_test(
        x1,
        x2,
        alternative = alternative,
        conf.int = TRUE,
        conf.level = conf_level,
        exact = exact,
        correct = continuity
      )
    } else {
      NULL
    }

    w_stat <- if (!is.null(test)) unname(test$statistic) else NA_real_
    u_stat <- calc_u_from_w(w_stat, n1)
    p_val <- if (!is.null(test)) test$p.value else NA_real_
    p_values <- c(p_values, p_val)

    z_val <- calc_z_mann_whitney(u_stat, n1, n2)
    r_val <- calc_r_from_z(z_val, n1 + n2)
    rb_val <- calc_rank_biserial_mann_whitney(u_stat, n1, n2)
    effect_val <- if (effect_size == "rb") rb_val else r_val

    rows[[length(rows) + 1]] <- data.frame(
      test_type = "kruskal_posthoc",
      variable = var,
      group = group_var,
      group_1 = g1,
      group_2 = g2,
      n_1 = n1,
      n_2 = n2,
      median_1 = ifelse(n1 > 0, median(x1), NA_real_),
      median_2 = ifelse(n2 > 0, median(x2), NA_real_),
      iqr_1 = ifelse(n1 > 0, IQR(x1), NA_real_),
      iqr_2 = ifelse(n2 > 0, IQR(x2), NA_real_),
      stat_label = format_stat_label("mann_whitney"),
      statistic = u_stat,
      df = NA_real_,
      p = p_val,
      p_adj = NA_real_,
      effect_size_label = format_effect_label(effect_size),
      effect_size_value = effect_val,
      ci_low = if (!is.null(test) && !is.null(test$conf.int)) test$conf.int[1] else NA_real_,
      ci_high = if (!is.null(test) && !is.null(test$conf.int)) test$conf.int[2] else NA_real_,
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0) return(data.frame())
  df_out <- do.call(rbind, rows)
  if (length(p_values) == nrow(df_out)) {
    df_out$p_adj <- p.adjust(p_values, method = p_adjust)
  }
  df_out
}

build_posthoc_friedman <- function(df, within_vars, subject_id, alternative, conf_level, exact, continuity, effect_size, p_adjust) {
  if (!subject_id %in% names(df)) return(data.frame())
  data_subset <- df[, c(subject_id, within_vars), drop = FALSE]
  complete <- complete.cases(data_subset)
  data_subset <- data_subset[complete, , drop = FALSE]
  if (nrow(data_subset) == 0) return(data.frame())

  pairs <- combn(within_vars, 2, simplify = FALSE)
  rows <- list()
  p_values <- numeric(0)

  for (pair in pairs) {
    v1 <- pair[1]
    v2 <- pair[2]
    x1 <- data_subset[[v1]]
    x2 <- data_subset[[v2]]
    n <- length(x1)

    test <- if (n > 0) {
      safe_wilcox_test(
        x1,
        x2,
        paired = TRUE,
        alternative = alternative,
        conf.int = TRUE,
        conf.level = conf_level,
        exact = exact,
        correct = continuity
      )
    } else {
      NULL
    }

    stat_val <- if (!is.null(test)) unname(test$statistic) else NA_real_
    p_val <- if (!is.null(test)) test$p.value else NA_real_
    p_values <- c(p_values, p_val)

    diffs <- x1 - x2
    rank_info <- calc_rank_biserial_signed(diffs)
    z_val <- calc_z_signed(rank_info$w_pos, rank_info$n)
    r_val <- calc_r_from_z(z_val, rank_info$n)
    rb_val <- rank_info$value
    effect_val <- if (effect_size == "rb") rb_val else r_val

    rows[[length(rows) + 1]] <- data.frame(
      test_type = "friedman_posthoc",
      variable = paste(within_vars, collapse = ","),
      group = "",
      group_1 = v1,
      group_2 = v2,
      n_1 = n,
      n_2 = NA_real_,
      median_1 = ifelse(n > 0, median(x1), NA_real_),
      median_2 = ifelse(n > 0, median(x2), NA_real_),
      iqr_1 = ifelse(n > 0, IQR(x1), NA_real_),
      iqr_2 = ifelse(n > 0, IQR(x2), NA_real_),
      stat_label = format_stat_label("wilcoxon_paired"),
      statistic = stat_val,
      df = NA_real_,
      p = p_val,
      p_adj = NA_real_,
      effect_size_label = format_effect_label(effect_size),
      effect_size_value = effect_val,
      ci_low = if (!is.null(test) && !is.null(test$conf.int)) test$conf.int[1] else NA_real_,
      ci_high = if (!is.null(test) && !is.null(test$conf.int)) test$conf.int[2] else NA_real_,
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0) return(data.frame())
  df_out <- do.call(rbind, rows)
  if (length(p_values) == nrow(df_out)) {
    df_out$p_adj <- p.adjust(p_values, method = p_adjust)
  }
  df_out
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
      resolve_as_cell_text(row$test_type),
      resolve_as_cell_text(row$variable_display),
      format_stat(row$statistic, digits),
      format_stat(row$df, digits),
      format_p(row$p),
      format_stat(row$effect_size_value, digits)
    )
  }
  table <- resolve_render_markdown_table(headers, rows)
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

  columns <- resolve_normalize_table_columns(table_meta$columns, default_specs)
  rows <- list()
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    row_tokens <- list(
      test_type = resolve_as_cell_text(row$test_type),
      variable = resolve_as_cell_text(row$variable_display),
      measure_1 = resolve_as_cell_text(row$measure_1_display),
      measure_2 = resolve_as_cell_text(row$measure_2_display),
      group = resolve_as_cell_text(row$group_display),
      group_1 = resolve_as_cell_text(row$group_1_display),
      group_2 = resolve_as_cell_text(row$group_2_display),
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
      group_summary = resolve_as_cell_text(row$group_summary),
      within_summary = resolve_as_cell_text(row$within_summary),
      stat_label = resolve_as_cell_text(row$stat_label),
      statistic = format_stat(row$statistic, digits),
      df = format_stat(row$df, digits),
      p = format_p(row$p),
      effect_size_label = resolve_as_cell_text(row$effect_size_label),
      effect_size_value = format_stat(row$effect_size_value, digits),
      ci_low = format_stat(row$ci_low, digits),
      ci_high = format_stat(row$ci_high, digits)
    )

    row_values <- vapply(columns, function(col) resolve_as_cell_text(row_tokens[[col$key]]), character(1))
    rows[[length(rows) + 1]] <- row_values
  }

  drop_result <- resolve_drop_empty_columns(columns, rows)
  columns <- drop_result$columns
  rows <- drop_result$rows
  headers <- vapply(columns, function(col) col$label, character(1))
  list(
    body = resolve_render_markdown_table(headers, rows),
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
    stat_label <- resolve_as_cell_text(row$stat_label)
    stat_val <- format_stat(row$statistic, digits)
    df_val <- format_stat(row$df, digits)
    p_txt <- format_p(row$p)
    effect_label <- resolve_as_cell_text(row$effect_size_label)
    effect_val <- format_stat(row$effect_size_value, digits)
    ci_txt <- format_ci(row$ci_low, row$ci_high, digits)
    conf_label <- if (!is.na(conf_level)) paste0(round(conf_level * 100), "%") else ""

    effect_text <- if (nzchar(effect_label) && nzchar(effect_val)) paste0(", ", effect_label, " = ", effect_val) else ""
    ci_text <- if (nzchar(ci_txt)) paste0(", ", conf_label, " CI ", ci_txt) else ""

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
      resolve_as_cell_text(row$test_type),
      " test for ",
      if (nzchar(resolve_as_cell_text(row$variable_display))) resolve_as_cell_text(row$variable_display) else "the specified measures",
      if (nzchar(resolve_as_cell_text(row$group_display))) paste0(" by ", resolve_as_cell_text(row$group_display)) else "",
      if (nzchar(resolve_as_cell_text(row$group_1_display)) && nzchar(resolve_as_cell_text(row$group_2_display))) {
        paste0(" (", resolve_as_cell_text(row$group_1_display), " vs ", resolve_as_cell_text(row$group_2_display), ")")
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

    rows[[length(rows) + 1]] <- list(
      test_type = resolve_as_cell_text(row$test_type),
      variable = resolve_as_cell_text(row$variable_display),
      measure_1 = resolve_as_cell_text(row$measure_1_display),
      measure_2 = resolve_as_cell_text(row$measure_2_display),
      group = resolve_as_cell_text(row$group_display),
      group_1 = resolve_as_cell_text(row$group_1_display),
      group_2 = resolve_as_cell_text(row$group_2_display),
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
    parts <- c(parts, paste0("Exact p-values: ", ifelse(exact, "yes", "no"), "."))
  }
  if (!is.null(continuity)) {
    parts <- c(parts, paste0("Continuity correction: ", ifelse(continuity, "yes", "no"), "."))
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

  columns <- resolve_normalize_table_columns(table_meta$columns, default_specs)
  rows <- list()
  for (i in seq_len(nrow(display))) {
    row <- display[i, ]
    row_tokens <- list(
      variable = resolve_as_cell_text(row$variable_display),
      group = resolve_as_cell_text(row$group_display),
      group_1 = resolve_as_cell_text(row$group_1_display),
      group_2 = resolve_as_cell_text(row$group_2_display),
      n_1 = format_stat(row$n_1, digits),
      n_2 = format_stat(row$n_2, digits),
      median_1 = format_stat(row$median_1, digits),
      median_2 = format_stat(row$median_2, digits),
      iqr_1 = format_stat(row$iqr_1, digits),
      iqr_2 = format_stat(row$iqr_2, digits),
      stat_label = resolve_as_cell_text(row$stat_label),
      statistic = format_stat(row$statistic, digits),
      p = format_p(row$p),
      p_adj = format_p(row$p_adj),
      effect_size_label = resolve_as_cell_text(row$effect_size_label),
      effect_size_value = format_stat(row$effect_size_value, digits),
      ci_low = format_stat(row$ci_low, digits),
      ci_high = format_stat(row$ci_high, digits)
    )

    row_values <- vapply(columns, function(col) resolve_as_cell_text(row_tokens[[col$key]]), character(1))
    rows[[length(rows) + 1]] <- row_values
  }

  drop_result <- resolve_drop_empty_columns(columns, rows)
  columns <- drop_result$columns
  rows <- drop_result$rows
  headers <- vapply(columns, function(col) col$label, character(1))
  list(
    body = resolve_render_markdown_table(headers, rows),
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
    stat_label <- resolve_as_cell_text(row$stat_label)
    stat_val <- format_stat(row$statistic, digits)
    p_txt <- format_p(row$p)
    p_adj_txt <- format_p(row$p_adj)
    effect_label <- resolve_as_cell_text(row$effect_size_label)
    effect_val <- format_stat(row$effect_size_value, digits)

    effect_text <- if (nzchar(effect_label) && nzchar(effect_val)) paste0(", ", effect_label, " = ", effect_val) else ""
    p_adj_text <- if (nzchar(p_adj_txt)) paste0(", p_adj ", p_adj_txt) else ""

    full_sentence <- paste0(
      "Pairwise comparison of ",
      resolve_as_cell_text(row$group_1_display),
      " vs ",
      resolve_as_cell_text(row$group_2_display),
      " yielded ",
      if (nzchar(stat_label)) paste0(stat_label, " = ", stat_val) else stat_val,
      ", p ",
      p_txt,
      p_adj_text,
      effect_text,
      "."
    )

    rows[[length(rows) + 1]] <- list(
      variable = resolve_as_cell_text(row$variable_display),
      group = resolve_as_cell_text(row$group_display),
      group_1 = resolve_as_cell_text(row$group_1_display),
      group_2 = resolve_as_cell_text(row$group_2_display),
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
    comparison <- paste(resolve_as_cell_text(row$group_1), resolve_as_cell_text(row$group_2), sep = " vs ")
    rows[[length(rows) + 1]] <- c(
      comparison,
      format_stat(row$statistic, digits),
      format_p(row$p),
      format_p(row$p_adj)
    )
  }
  table <- resolve_render_markdown_table(headers, rows)
  paste0(table, "\n\n", note_text)
}

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  opts <- resolve_parse_args(args)

  if (!is.null(opts$help)) {
    print_usage()
    quit(status = 0)
  }

  if (!is.null(opts$interactive)) {
    opts <- modifyList(opts, interactive_options())
  }

  digits_default <- resolve_config_value("defaults.digits", 2)
  log_default <- resolve_config_value("defaults.log", TRUE)
  vars_default <- resolve_config_value("modules.nonparametric.vars_default", "numeric")
  test_default <- resolve_config_value("modules.nonparametric.test", "auto")
  mu_default <- resolve_config_value("modules.nonparametric.mu", 0.0)
  alternative_default <- resolve_config_value("modules.nonparametric.alternative", "two.sided")
  conf_default <- resolve_config_value("modules.nonparametric.conf_level", 0.95)
  exact_default <- resolve_config_value("modules.nonparametric.exact", "auto")
  continuity_default <- resolve_config_value("modules.nonparametric.continuity", TRUE)
  posthoc_default <- resolve_config_value("modules.nonparametric.posthoc", "none")
  p_adjust_default <- resolve_config_value("modules.nonparametric.p_adjust", "holm")
  effect_default <- resolve_config_value("modules.nonparametric.effect_size", "r")

  digits <- if (!is.null(opts$digits)) as.numeric(opts$digits) else digits_default
  df <- resolve_load_dataframe(opts)
  out_dir <- resolve_get_workspace_out_dir(df)

  has_group <- !is.null(opts$group) && opts$group != ""
  has_x <- !is.null(opts$x) && opts$x != ""
  has_y <- !is.null(opts$y) && opts$y != ""
  has_within <- !is.null(opts$within) && opts$within != ""
  has_subject <- !is.null(opts$`subject-id`) && opts$`subject-id` != ""
  has_vars <- !is.null(opts$vars) && opts$vars != ""

  if (has_group && (has_x || has_y)) {
    emit_input_issue(
      out_dir,
      opts,
      "Paired tests do not use --group.",
      details = list(group = opts$group, x = opts$x, y = opts$y)
    )
  }
  if (has_within && (has_group || has_x || has_y || has_vars)) {
    emit_input_issue(
      out_dir,
      opts,
      "Friedman tests use --within and --subject-id only.",
      details = list(within = opts$within, group = opts$group, x = opts$x, y = opts$y, vars = opts$vars)
    )
  }
  if (has_within && !has_subject) {
    emit_input_issue(
      out_dir,
      opts,
      "Friedman tests require --subject-id.",
      details = list(within = opts$within)
    )
  }

  test_choice <- normalize_test(opts$test, test_default)
  mode <- ""
  if (test_choice == "friedman" || has_within) {
    mode <- "friedman"
  } else if (test_choice == "kruskal") {
    if (!has_group) {
      emit_input_issue(out_dir, opts, "Kruskal-Wallis requires --group.", details = list(test = test_choice))
    }
    mode <- "kruskal"
  } else if (test_choice == "mann_whitney") {
    if (!has_group) {
      emit_input_issue(out_dir, opts, "Mann-Whitney requires --group.", details = list(test = test_choice))
    }
    mode <- "mann_whitney"
  } else if (test_choice == "wilcoxon") {
    if (has_group) {
      emit_input_issue(out_dir, opts, "Wilcoxon signed-rank does not use --group.", details = list(group = opts$group))
    }
    mode <- if (has_x || has_y) "wilcoxon_paired" else "wilcoxon_one_sample"
  } else {
    if (has_x || has_y) {
      mode <- "wilcoxon_paired"
    } else if (has_group) {
      group_vals <- df[[opts$group]]
      levels <- get_group_levels(group_vals)
      if (length(levels) == 2) {
        mode <- "mann_whitney"
      } else if (length(levels) > 2) {
        mode <- "kruskal"
      } else {
        emit_input_issue(out_dir, opts, "Grouping variable must have at least two levels.", details = list(group = opts$group))
      }
    } else {
      mode <- "wilcoxon_one_sample"
    }
  }

  alternative <- normalize_alternative(opts$alternative, alternative_default)
  conf_level <- if (!is.null(opts$`conf-level`)) as.numeric(opts$`conf-level`) else conf_default
  exact_flag <- normalize_exact(opts$exact, exact_default)
  continuity_flag <- resolve_parse_bool(opts$continuity, default = continuity_default)
  posthoc <- normalize_posthoc(opts$posthoc, posthoc_default)
  p_adjust <- if (!is.null(opts$`p-adjust`)) opts$`p-adjust` else p_adjust_default
  effect_size <- resolve_effect_size_choice(opts$`effect-size`, mode, effect_default)
  effect_label <- format_effect_label(effect_size)

  result <- NULL
  posthoc_df <- data.frame()

  if (mode == "wilcoxon_paired") {
    if (!(has_x && has_y)) {
      emit_input_issue(out_dir, opts, "Paired Wilcoxon requires both --x and --y.", details = list(x = opts$x, y = opts$y))
    }
    x_vars <- resolve_parse_list(opts$x)
    y_vars <- resolve_parse_list(opts$y)
    if (length(x_vars) == 0 || length(y_vars) == 0) {
      emit_input_issue(out_dir, opts, "Paired Wilcoxon requires --x and --y variables.", details = list(x = opts$x, y = opts$y))
    }
    if (length(x_vars) != length(y_vars)) {
      emit_input_issue(out_dir, opts, "--x and --y must have the same number of variables.", details = list(x = x_vars, y = y_vars))
    }
    missing <- setdiff(c(x_vars, y_vars), names(df))
    if (length(missing) > 0) {
      emit_input_issue(out_dir, opts, "Missing paired variables.", details = list(missing = missing))
    }
    result <- build_summary_paired(df, x_vars, y_vars, alternative, conf_level, exact_flag, continuity_flag, effect_size)
  } else if (mode == "wilcoxon_one_sample") {
    vars <- resolve_select_variables(df, opts$vars, NULL, default = vars_default)
    if (length(vars) == 0) stop("No numeric variables available for analysis.")
    mu_value <- if (!is.null(opts$mu)) as.numeric(opts$mu) else mu_default
    result <- build_summary_one_sample(df, vars, mu_value, alternative, conf_level, exact_flag, continuity_flag, effect_size)
  } else if (mode == "mann_whitney") {
    if (!has_group) {
      emit_input_issue(out_dir, opts, "Mann-Whitney requires --group.", details = list(test = mode))
    }
    if (!opts$group %in% names(df)) {
      emit_input_issue(out_dir, opts, "Grouping variable not found.", details = list(group = opts$group))
    }
    group_vals <- df[[opts$group]]
    levels <- get_group_levels(group_vals)
    if (length(levels) != 2) {
      emit_input_issue(
        out_dir,
        opts,
        "Mann-Whitney requires exactly two group levels.",
        details = list(group = opts$group, levels = as.character(levels))
      )
    }
    vars <- resolve_select_variables(df, opts$vars, opts$group, default = vars_default)
    if (length(vars) == 0) stop("No numeric variables available for analysis.")
    result <- build_summary_mann_whitney(df, vars, opts$group, alternative, conf_level, exact_flag, continuity_flag, effect_size)
  } else if (mode == "kruskal") {
    if (!has_group) {
      emit_input_issue(out_dir, opts, "Kruskal-Wallis requires --group.", details = list(test = mode))
    }
    if (!opts$group %in% names(df)) {
      emit_input_issue(out_dir, opts, "Grouping variable not found.", details = list(group = opts$group))
    }
    vars <- resolve_select_variables(df, opts$vars, opts$group, default = vars_default)
    if (length(vars) == 0) stop("No numeric variables available for analysis.")
    result <- build_summary_kruskal(df, vars, opts$group, effect_size, digits)
  posthoc_effect_size <- if (effect_size %in% c("r", "rb")) effect_size else "r"
  if (posthoc == "pairwise" && length(vars) > 0) {
    posthoc_df <- do.call(rbind, lapply(vars, function(var) {
      build_posthoc_kruskal(df, var, opts$group, alternative, conf_level, exact_flag, continuity_flag, posthoc_effect_size, p_adjust)
    }))
    if (is.null(posthoc_df)) posthoc_df <- data.frame()
  }
  } else if (mode == "friedman") {
    within_vars <- resolve_parse_list(opts$within)
    if (length(within_vars) < 2) {
      emit_input_issue(out_dir, opts, "Friedman requires at least two repeated measures.", details = list(within = opts$within))
    }
    missing <- setdiff(c(within_vars, opts$`subject-id`), names(df))
    if (length(missing) > 0) {
      emit_input_issue(out_dir, opts, "Missing Friedman variables.", details = list(missing = missing))
    }
    result <- build_summary_friedman(df, within_vars, opts$`subject-id`, effect_size, digits)
    if (posthoc == "pairwise") {
      posthoc_effect_size <- if (effect_size %in% c("r", "rb")) effect_size else "r"
      posthoc_df <- build_posthoc_friedman(df, within_vars, opts$`subject-id`, alternative, conf_level, exact_flag, continuity_flag, posthoc_effect_size, p_adjust)
      if (is.null(posthoc_df)) posthoc_df <- data.frame()
    }
  }

  summary_df <- result$summary
  diagnostics_df <- result$diagnostics
  if (nrow(summary_df) == 0) stop("No valid nonparametric tests could be computed.")
  label_meta <- resolve_label_metadata(df)
  summary_df <- add_variable_label_column(summary_df, label_meta, var_col = "variable")
  summary_df <- add_variable_label_column(summary_df, label_meta, var_col = "measure_1")
  summary_df <- add_variable_label_column(summary_df, label_meta, var_col = "measure_2")
  summary_df <- add_group_label_column(summary_df, label_meta, opts$group, group_col = "group")
  summary_df <- add_group_label_column(summary_df, label_meta, opts$group, group_col = "group_1")
  summary_df <- add_group_label_column(summary_df, label_meta, opts$group, group_col = "group_2")
  if (!is.null(posthoc_df) && nrow(posthoc_df) > 0) {
    posthoc_df <- add_variable_label_column(posthoc_df, label_meta, var_col = "variable")
    posthoc_df <- add_group_label_column(posthoc_df, label_meta, opts$group, group_col = "group")
    posthoc_df <- add_group_label_column(posthoc_df, label_meta, opts$group, group_col = "group_1")
    posthoc_df <- add_group_label_column(posthoc_df, label_meta, opts$group, group_col = "group_2")
  }

  uses_wilcox <- mode %in% c("wilcoxon_one_sample", "wilcoxon_paired", "mann_whitney")
  note_tokens <- build_nonparam_note_tokens(
    alternative = if (uses_wilcox) alternative else NULL,
    conf_level = if (uses_wilcox) conf_level else NULL,
    exact = if (uses_wilcox) exact_flag else NULL,
    continuity = if (uses_wilcox) continuity_flag else NULL,
    effect_size_label = effect_label,
    posthoc = posthoc,
    p_adjust = p_adjust
  )

  nlss_report_path <- file.path(out_dir, "report_canonical.md")
  narrative_conf <- if (uses_wilcox) conf_level else NA
  nlss_text <- format_nlss_text(summary_df, digits, narrative_conf)
  nlss_table <- format_nlss_table(summary_df, digits, note_tokens$note_default)

  template_override <- resolve_template_override(opts$template, module = "nonparametric")
  template_path <- if (!is.null(template_override)) {
    template_override
  } else {
    resolve_get_template_path("nonparametric.default", "nonparametric/default-template.md")
  }
  template_meta <- resolve_get_template_meta(template_path)
  table_result <- build_nonparam_table_body(summary_df, digits, template_meta$table)
  narrative_rows <- build_nonparam_narrative_rows(summary_df, digits, narrative_conf)
  template_context <- list(
    tokens = c(
      list(
        table_body = table_result$body,
        narrative_default = nlss_text
      ),
      note_tokens
    ),
    narrative_rows = narrative_rows
  )

  resolved_test <- if (test_choice == "auto") {
    if (mode %in% c("wilcoxon_one_sample", "wilcoxon_paired")) "wilcoxon" else mode
  } else {
    test_choice
  }
  analysis_flags <- list(
    test = resolved_test,
    mode = mode,
    vars = if (exists("vars")) vars else NULL,
    x = if (exists("x_vars")) x_vars else NULL,
    y = if (exists("y_vars")) y_vars else NULL,
    group = if (has_group) opts$group else NULL,
    within = if (exists("within_vars")) within_vars else NULL,
    "subject-id" = if (has_subject) opts$`subject-id` else NULL,
    mu = if (mode == "wilcoxon_one_sample") mu_value else NULL,
    alternative = if (uses_wilcox) alternative else NULL,
    exact = if (uses_wilcox && !is.null(exact_flag)) exact_flag else NULL,
    continuity = if (uses_wilcox) continuity_flag else NULL,
    "conf-level" = if (uses_wilcox) conf_level else NULL,
    posthoc = if (mode %in% c("kruskal", "friedman")) posthoc else NULL,
    "p-adjust" = if (mode %in% c("kruskal", "friedman")) p_adjust else NULL,
    "effect-size" = effect_size,
    digits = digits
  )

  resolve_append_nlss_report(
    nlss_report_path,
    "Nonparametric tests",
    nlss_table,
    nlss_text,
    analysis_flags = analysis_flags,
    template_path = template_path,
    template_context = template_context
  )

  if (nrow(posthoc_df) > 0) {
    posthoc_note_tokens <- build_posthoc_note_tokens(posthoc, p_adjust)
    posthoc_table <- format_posthoc_table(posthoc_df, digits, posthoc_note_tokens$note_default)
    posthoc_template_path <- if (!is.null(template_override)) {
      template_override
    } else {
      resolve_get_template_path("nonparametric.posthoc", "nonparametric/posthoc-template.md")
    }
    posthoc_meta <- resolve_get_template_meta(posthoc_template_path)
    posthoc_table_body <- build_posthoc_table_body(posthoc_df, digits, posthoc_meta$table)
    posthoc_rows <- build_posthoc_narrative_rows(posthoc_df, digits)
    posthoc_text <- paste(vapply(posthoc_rows, function(row) row$full_sentence, character(1)), collapse = "\n")
    posthoc_context <- list(
      tokens = c(
        list(
          table_body = posthoc_table_body$body,
          narrative_default = posthoc_text
        ),
        posthoc_note_tokens
      ),
      narrative_rows = posthoc_rows
    )

    resolve_append_nlss_report(
      nlss_report_path,
      "Nonparametric post-hoc",
      posthoc_table,
      posthoc_text,
      analysis_flags = analysis_flags,
      template_path = posthoc_template_path,
      template_context = posthoc_context
    )
  }

  cat("Wrote:\n")
  cat("- ", render_output_path(nlss_report_path, out_dir), "\n", sep = "")

  if (resolve_parse_bool(opts$log, default = log_default)) {
    ctx <- resolve_get_run_context()
    resolve_append_analysis_log(
      out_dir,
      module = "nonparametric",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(summary_df = summary_df, posthoc_df = posthoc_df, diagnostics_df = diagnostics_df),
      options = list(
        test = resolved_test,
        mode = mode,
        vars = if (exists("vars")) vars else NULL,
        x = if (exists("x_vars")) x_vars else NULL,
        y = if (exists("y_vars")) y_vars else NULL,
        group = if (has_group) opts$group else NULL,
        within = if (exists("within_vars")) within_vars else NULL,
        subject_id = if (has_subject) opts$`subject-id` else NULL,
        mu = if (mode == "wilcoxon_one_sample") mu_value else NA,
        alternative = if (uses_wilcox) alternative else NULL,
        exact = ifelse(uses_wilcox && !is.null(exact_flag), exact_flag, NA),
        continuity = if (uses_wilcox) continuity_flag else NA,
        conf_level = if (uses_wilcox) conf_level else NA,
        posthoc = if (mode %in% c("kruskal", "friedman")) posthoc else NULL,
        p_adjust = if (mode %in% c("kruskal", "friedman")) p_adjust else NULL,
        effect_size = effect_size,
        digits = digits
      ),
      user_prompt = resolve_get_user_prompt(opts)
    )
  }
}

main()
