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

print_usage <- function() {
  cat("t-tests (base R)\n")
  cat("\n")
  cat("Usage:\n")
  cat("  Rscript t_test.R --csv data.csv --vars var1,var2 [--mu 0]\n")
  cat("  Rscript t_test.R --csv data.csv --vars var1,var2 --group group_var\n")
  cat("  Rscript t_test.R --csv data.csv --x var1,var2 --y var3,var4\n")
  cat("  Rscript t_test.R --sav data.sav --vars var1,var2 [--mu 0]\n")
  cat("  Rscript t_test.R --rds data.rds --vars var1,var2\n")
  cat("  Rscript t_test.R --rdata data.RData --df data_frame_name --vars var1,var2\n")
  cat("  Rscript t_test.R --parquet data.parquet --vars var1,var2\n")
  cat("  Rscript t_test.R --interactive\n")
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
  cat("  --vars LIST            Comma-separated variables (one-sample/independent)\n")
  cat("  --group NAME           Grouping variable (independent samples)\n")
  cat("  --x LIST               Comma-separated measure 1 variables (paired)\n")
  cat("  --y LIST               Comma-separated measure 2 variables (paired)\n")
  cat("  --mu VALUE             One-sample test value (default: 0.0)\n")
  cat("  --alternative TYPE     two.sided/greater/less (default: two.sided)\n")
  cat("  --var-equal TRUE/FALSE Assume equal variances (independent; default: FALSE)\n")
  cat("  --conf-level VALUE     Confidence level (default: 0.95)\n")
  cat("  --bootstrap TRUE/FALSE Bootstrap confidence intervals (default: FALSE)\n")
  cat("  --bootstrap-samples N  Bootstrap resamples (default: 1000)\n")
  cat("  --seed N               Random seed for bootstrap (optional)\n")
  cat("  --digits N             Rounding digits (default: 2)\n")
  cat("  --expect-two-groups TRUE/FALSE  Informational output when group levels != 2 (default: FALSE)\n")
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

  test_type <- resolve_prompt("Test type (one-sample/independent/paired)", "one-sample")
  test_type <- tolower(test_type)
  if (test_type == "paired") {
    opts$x <- resolve_prompt("Measure 1 variables (comma-separated)", "")
    opts$y <- resolve_prompt("Measure 2 variables (comma-separated)", "")
  } else {
    opts$vars <- resolve_prompt("Variables (comma-separated)", "")
    if (test_type == "independent") {
      opts$group <- resolve_prompt("Grouping variable", "")
    } else {
      mu_default <- resolve_config_value("modules.t_test.mu", 0.0)
      opts$mu <- resolve_prompt("Test value (mu)", as.character(mu_default))
    }
  }

  alternative_default <- resolve_config_value("modules.t_test.alternative", "two.sided")
  conf_default <- resolve_config_value("modules.t_test.conf_level", 0.95)
  var_equal_default <- resolve_config_value("modules.t_test.var_equal", FALSE)
  bootstrap_default <- resolve_config_value("modules.t_test.bootstrap", FALSE)
  bootstrap_samples_default <- resolve_config_value("modules.t_test.bootstrap_samples", 1000)
  digits_default <- resolve_config_value("defaults.digits", 2)

  opts$alternative <- resolve_prompt("Alternative (two.sided/greater/less)", alternative_default)
  opts$`conf-level` <- resolve_prompt("Confidence level", as.character(conf_default))
  opts$`var-equal` <- resolve_prompt("Equal variances TRUE/FALSE", ifelse(isTRUE(var_equal_default), "TRUE", "FALSE"))
  opts$bootstrap <- resolve_prompt("Bootstrap TRUE/FALSE", ifelse(isTRUE(bootstrap_default), "TRUE", "FALSE"))
  opts$`bootstrap-samples` <- resolve_prompt("Bootstrap samples", as.character(bootstrap_samples_default))
  opts$seed <- resolve_prompt("Bootstrap seed (optional)", "")
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

resolve_append_apa_report <- function(path, analysis_label, apa_table, apa_text, analysis_flags = NULL, template_path = NULL, template_context = NULL) {
  if (exists("append_apa_report", mode = "function")) {
    return(get("append_apa_report", mode = "function")(
      path,
      analysis_label,
      apa_table,
      apa_text,
      analysis_flags = analysis_flags,
      template_path = template_path,
      template_context = template_context
    ))
  }
  stop("Missing append_apa_report. Ensure lib/formatting.R is sourced.")
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

emit_group_level_issue <- function(out_dir, opts, group_var, levels, expected = FALSE) {
  level_count <- length(levels)
  level_text <- if (level_count > 0) paste(as.character(levels), collapse = ", ") else "None"
  message <- sprintf(
    "Grouping variable '%s' has %d non-missing level(s); expected exactly two. Levels: %s.",
    group_var,
    level_count,
    level_text
  )

  if (expected) {
    cat("EXPECTED_NEGATIVE: t_test group levels\n")
    cat(message, "\n")
  }

  log_default <- resolve_config_value("defaults.log", TRUE)
  if (resolve_parse_bool(opts$log, default = log_default)) {
    ctx <- resolve_get_run_context()
    resolve_append_analysis_log(
      out_dir,
      module = "t_test",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(
        status = ifelse(expected, "expected_invalid_input", "invalid_input"),
        message = message,
        details = list(
          group_var = group_var,
          level_count = level_count,
          levels = as.character(levels)
        )
      ),
      options = list(
        mode = "independent",
        group = group_var,
        vars = opts$vars,
        expect_two_groups = expected
      ),
      user_prompt = resolve_get_user_prompt(opts)
    )
  }

  if (expected) {
    quit(status = 0)
  }
  stop(message)
}

emit_input_issue <- function(out_dir, opts, message, details = list(), status = "invalid_input") {
  log_default <- resolve_config_value("defaults.log", TRUE)
  if (resolve_parse_bool(opts$log, default = log_default)) {
    ctx <- resolve_get_run_context()
    resolve_append_analysis_log(
      out_dir,
      module = "t_test",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(
        status = status,
        message = message,
        details = details
      ),
      options = list(
        mode = "paired",
        vars = opts$vars,
        x = opts$x,
        y = opts$y,
        group = opts$group
      ),
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

format_test_type <- function(value) {
  if (is.null(value) || !nzchar(value)) return("")
  if (value == "one_sample") return("One-sample")
  if (value == "independent") return("Independent")
  if (value == "paired") return("Paired")
  value
}

safe_shapiro <- function(values) {
  n <- length(values)
  if (n < 3 || n > 5000) return(list(w = NA_real_, p = NA_real_))
  test <- tryCatch(shapiro.test(values), error = function(e) NULL)
  if (is.null(test)) return(list(w = NA_real_, p = NA_real_))
  list(w = unname(test$statistic), p = test$p.value)
}

calc_d_one_sample <- function(mean_diff, sd_val) {
  if (is.na(sd_val) || sd_val == 0) return(NA_real_)
  mean_diff / sd_val
}

calc_d_paired <- function(mean_diff, sd_diff) {
  if (is.na(sd_diff) || sd_diff == 0) return(NA_real_)
  mean_diff / sd_diff
}

calc_d_independent <- function(mean_diff, sd1, sd2, n1, n2) {
  if (n1 < 2 || n2 < 2 || is.na(sd1) || is.na(sd2)) return(NA_real_)
  pooled <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  if (is.na(pooled) || pooled == 0) return(NA_real_)
  mean_diff / pooled
}

bootstrap_ci <- function(values, stat_fn, n_boot, conf_level) {
  stats <- numeric(0)
  n <- length(values)
  for (i in seq_len(n_boot)) {
    sample_idx <- sample.int(n, size = n, replace = TRUE)
    stats[i] <- stat_fn(values[sample_idx])
  }
  alpha <- (1 - conf_level) / 2
  quantile(stats, probs = c(alpha, 1 - alpha), na.rm = TRUE)
}

bootstrap_ci_independent <- function(x1, x2, stat_fn, n_boot, conf_level) {
  n1 <- length(x1)
  n2 <- length(x2)
  stats <- numeric(0)
  for (i in seq_len(n_boot)) {
    s1 <- sample(x1, size = n1, replace = TRUE)
    s2 <- sample(x2, size = n2, replace = TRUE)
    stats[i] <- stat_fn(s1, s2)
  }
  alpha <- (1 - conf_level) / 2
  quantile(stats, probs = c(alpha, 1 - alpha), na.rm = TRUE)
}

build_summary_one_sample <- function(df, vars, mu, alternative, conf_level, bootstrap, bootstrap_samples, seed) {
  rows <- list()
  diagnostics <- list()
  for (var in vars) {
    vec <- df[[var]]
    if (!is.numeric(vec)) stop(paste("Variable is not numeric:", var))
    clean <- vec[!is.na(vec)]
    n <- length(clean)
    mean_val <- ifelse(n > 0, mean(clean), NA_real_)
    sd_val <- ifelse(n > 1, sd(clean), NA_real_)
    mean_diff <- ifelse(n > 0, mean_val - mu, NA_real_)
    test <- if (n >= 2) {
      tryCatch(t.test(clean, mu = mu, alternative = alternative, conf.level = conf_level), error = function(e) NULL)
    } else {
      NULL
    }
    t_stat <- if (!is.null(test)) unname(test$statistic) else NA_real_
    df_val <- if (!is.null(test)) unname(test$parameter) else NA_real_
    p_val <- if (!is.null(test)) test$p.value else NA_real_
    ci_low <- if (!is.null(test)) test$conf.int[1] else NA_real_
    ci_high <- if (!is.null(test)) test$conf.int[2] else NA_real_
    d_val <- calc_d_one_sample(mean_diff, sd_val)

    boot_ci_low <- NA_real_
    boot_ci_high <- NA_real_
    boot_d_low <- NA_real_
    boot_d_high <- NA_real_
    if (bootstrap && n >= 2) {
      boot_ci <- bootstrap_ci(
        clean,
        function(x) mean(x) - mu,
        n_boot = bootstrap_samples,
        conf_level = conf_level
      )
      boot_ci_low <- boot_ci[1]
      boot_ci_high <- boot_ci[2]
      boot_d <- bootstrap_ci(
        clean,
        function(x) calc_d_one_sample(mean(x) - mu, sd(x)),
        n_boot = bootstrap_samples,
        conf_level = conf_level
      )
      boot_d_low <- boot_d[1]
      boot_d_high <- boot_d[2]
    }

    shapiro <- safe_shapiro(clean)
    diagnostics[[length(diagnostics) + 1]] <- data.frame(
      test_type = "one_sample",
      variable = var,
      group = "",
      n = n,
      shapiro_w = shapiro$w,
      shapiro_p = shapiro$p,
      stringsAsFactors = FALSE
    )

    rows[[length(rows) + 1]] <- data.frame(
      test_type = "one_sample",
      variable = var,
      measure_1 = "",
      measure_2 = "",
      group_1 = "",
      group_2 = "",
      n_1 = n,
      n_2 = NA_real_,
      mean_1 = mean_val,
      mean_2 = NA_real_,
      sd_1 = sd_val,
      sd_2 = NA_real_,
      mean_diff = mean_diff,
      t = t_stat,
      df = df_val,
      p = p_val,
      d = d_val,
      ci_low = ci_low,
      ci_high = ci_high,
      boot_ci_low = boot_ci_low,
      boot_ci_high = boot_ci_high,
      boot_d_ci_low = boot_d_low,
      boot_d_ci_high = boot_d_high,
      mu = mu,
      stringsAsFactors = FALSE
    )
  }
  list(summary = do.call(rbind, rows), diagnostics = do.call(rbind, diagnostics))
}

build_summary_independent <- function(df, vars, group_var, alternative, var_equal, conf_level, bootstrap, bootstrap_samples, seed) {
  rows <- list()
  diagnostics <- list()
  group_vals <- df[[group_var]]
  levels <- unique(group_vals[!is.na(group_vals)])
  if (length(levels) != 2) stop("Grouping variable must have exactly two levels for independent t-tests.")
  group1 <- as.character(levels[1])
  group2 <- as.character(levels[2])

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
    mean1 <- ifelse(n1 > 0, mean(x1), NA_real_)
    mean2 <- ifelse(n2 > 0, mean(x2), NA_real_)
    sd1 <- ifelse(n1 > 1, sd(x1), NA_real_)
    sd2 <- ifelse(n2 > 1, sd(x2), NA_real_)
    mean_diff <- ifelse(n1 > 0 && n2 > 0, mean1 - mean2, NA_real_)

    test <- if (n1 >= 2 && n2 >= 2) {
      tryCatch(t.test(x1, x2, alternative = alternative, var.equal = var_equal, conf.level = conf_level), error = function(e) NULL)
    } else {
      NULL
    }
    t_stat <- if (!is.null(test)) unname(test$statistic) else NA_real_
    df_val <- if (!is.null(test)) unname(test$parameter) else NA_real_
    p_val <- if (!is.null(test)) test$p.value else NA_real_
    ci_low <- if (!is.null(test)) test$conf.int[1] else NA_real_
    ci_high <- if (!is.null(test)) test$conf.int[2] else NA_real_
    d_val <- calc_d_independent(mean_diff, sd1, sd2, n1, n2)

    boot_ci_low <- NA_real_
    boot_ci_high <- NA_real_
    boot_d_low <- NA_real_
    boot_d_high <- NA_real_
    if (bootstrap && n1 >= 2 && n2 >= 2) {
      boot_ci <- bootstrap_ci_independent(
        x1,
        x2,
        function(a, b) mean(a) - mean(b),
        n_boot = bootstrap_samples,
        conf_level = conf_level
      )
      boot_ci_low <- boot_ci[1]
      boot_ci_high <- boot_ci[2]
      boot_d <- bootstrap_ci_independent(
        x1,
        x2,
        function(a, b) {
          mean_diff_boot <- mean(a) - mean(b)
          calc_d_independent(mean_diff_boot, sd(a), sd(b), length(a), length(b))
        },
        n_boot = bootstrap_samples,
        conf_level = conf_level
      )
      boot_d_low <- boot_d[1]
      boot_d_high <- boot_d[2]
    }

    shapiro1 <- safe_shapiro(x1)
    shapiro2 <- safe_shapiro(x2)
    var_test <- if (n1 >= 2 && n2 >= 2) {
      tryCatch(var.test(x1, x2), error = function(e) NULL)
    } else {
      NULL
    }
    diagnostics[[length(diagnostics) + 1]] <- data.frame(
      test_type = "independent",
      variable = var,
      group = group1,
      n = n1,
      shapiro_w = shapiro1$w,
      shapiro_p = shapiro1$p,
      var_test_f = NA_real_,
      var_test_p = NA_real_,
      stringsAsFactors = FALSE
    )
    diagnostics[[length(diagnostics) + 1]] <- data.frame(
      test_type = "independent",
      variable = var,
      group = group2,
      n = n2,
      shapiro_w = shapiro2$w,
      shapiro_p = shapiro2$p,
      var_test_f = if (!is.null(var_test)) unname(var_test$statistic) else NA_real_,
      var_test_p = if (!is.null(var_test)) var_test$p.value else NA_real_,
      stringsAsFactors = FALSE
    )

    rows[[length(rows) + 1]] <- data.frame(
      test_type = "independent",
      variable = var,
      measure_1 = "",
      measure_2 = "",
      group_1 = group1,
      group_2 = group2,
      n_1 = n1,
      n_2 = n2,
      mean_1 = mean1,
      mean_2 = mean2,
      sd_1 = sd1,
      sd_2 = sd2,
      mean_diff = mean_diff,
      t = t_stat,
      df = df_val,
      p = p_val,
      d = d_val,
      ci_low = ci_low,
      ci_high = ci_high,
      boot_ci_low = boot_ci_low,
      boot_ci_high = boot_ci_high,
      boot_d_ci_low = boot_d_low,
      boot_d_ci_high = boot_d_high,
      mu = NA_real_,
      stringsAsFactors = FALSE
    )
  }
  list(summary = do.call(rbind, rows), diagnostics = do.call(rbind, diagnostics))
}

build_summary_paired <- function(df, x_vars, y_vars, alternative, conf_level, bootstrap, bootstrap_samples, seed) {
  rows <- list()
  diagnostics <- list()
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
    mean1 <- ifelse(n > 0, mean(x), NA_real_)
    mean2 <- ifelse(n > 0, mean(y), NA_real_)
    sd1 <- ifelse(n > 1, sd(x), NA_real_)
    sd2 <- ifelse(n > 1, sd(y), NA_real_)
    diff_vals <- x - y
    mean_diff <- ifelse(n > 0, mean(diff_vals), NA_real_)
    sd_diff <- ifelse(n > 1, sd(diff_vals), NA_real_)

    test <- if (n >= 2) {
      tryCatch(t.test(x, y, paired = TRUE, alternative = alternative, conf.level = conf_level), error = function(e) NULL)
    } else {
      NULL
    }
    t_stat <- if (!is.null(test)) unname(test$statistic) else NA_real_
    df_val <- if (!is.null(test)) unname(test$parameter) else NA_real_
    p_val <- if (!is.null(test)) test$p.value else NA_real_
    ci_low <- if (!is.null(test)) test$conf.int[1] else NA_real_
    ci_high <- if (!is.null(test)) test$conf.int[2] else NA_real_
    d_val <- calc_d_paired(mean_diff, sd_diff)

    boot_ci_low <- NA_real_
    boot_ci_high <- NA_real_
    boot_d_low <- NA_real_
    boot_d_high <- NA_real_
    if (bootstrap && n >= 2) {
      boot_ci <- bootstrap_ci(
        diff_vals,
        function(x) mean(x),
        n_boot = bootstrap_samples,
        conf_level = conf_level
      )
      boot_ci_low <- boot_ci[1]
      boot_ci_high <- boot_ci[2]
      boot_d <- bootstrap_ci(
        diff_vals,
        function(x) calc_d_paired(mean(x), sd(x)),
        n_boot = bootstrap_samples,
        conf_level = conf_level
      )
      boot_d_low <- boot_d[1]
      boot_d_high <- boot_d[2]
    }

    shapiro <- safe_shapiro(diff_vals)
    diagnostics[[length(diagnostics) + 1]] <- data.frame(
      test_type = "paired",
      variable = paste0(x_name, " - ", y_name),
      group = "",
      n = n,
      shapiro_w = shapiro$w,
      shapiro_p = shapiro$p,
      stringsAsFactors = FALSE
    )

    rows[[length(rows) + 1]] <- data.frame(
      test_type = "paired",
      variable = paste0(x_name, " - ", y_name),
      measure_1 = x_name,
      measure_2 = y_name,
      group_1 = "",
      group_2 = "",
      n_1 = n,
      n_2 = NA_real_,
      mean_1 = mean1,
      mean_2 = mean2,
      sd_1 = sd1,
      sd_2 = sd2,
      mean_diff = mean_diff,
      t = t_stat,
      df = df_val,
      p = p_val,
      d = d_val,
      ci_low = ci_low,
      ci_high = ci_high,
      boot_ci_low = boot_ci_low,
      boot_ci_high = boot_ci_high,
      boot_d_ci_low = boot_d_low,
      boot_d_ci_high = boot_d_high,
      mu = NA_real_,
      stringsAsFactors = FALSE
    )
  }
  list(summary = do.call(rbind, rows), diagnostics = do.call(rbind, diagnostics))
}

format_apa_table <- function(summary_df, digits, note_text) {
  headers <- c(
    "Test",
    "Variable",
    "Group 1",
    "Group 2",
    "n1",
    "n2",
    "M1",
    "M2",
    "SD1",
    "SD2",
    "Mean diff",
    "t",
    "df",
    "p",
    "d",
    "CI"
  )
  rows <- list()
  for (i in seq_len(nrow(summary_df))) {
    row <- summary_df[i, ]
    rows[[length(rows) + 1]] <- c(
      format_test_type(row$test_type),
      row$variable,
      row$group_1,
      row$group_2,
      ifelse(is.na(row$n_1), "", as.character(row$n_1)),
      ifelse(is.na(row$n_2), "", as.character(row$n_2)),
      format_num(row$mean_1, digits),
      format_num(row$mean_2, digits),
      format_num(row$sd_1, digits),
      format_num(row$sd_2, digits),
      format_num(row$mean_diff, digits),
      format_stat(row$t, digits),
      format_num(row$df, digits),
      format_p(row$p),
      format_stat(row$d, digits),
      format_ci(row$ci_low, row$ci_high, digits)
    )
  }
  table_md <- paste0("| ", paste(headers, collapse = " | "), " |\n")
  table_md <- paste0(table_md, "| ", paste(rep("---", length(headers)), collapse = " | "), " |\n")
  for (row in rows) {
    table_md <- paste0(table_md, "| ", paste(row, collapse = " | "), " |\n")
  }
  paste0("Table 1\n\n", table_md, "\n", note_text)
}

format_apa_text <- function(summary_df, digits, conf_level, alternative, var_equal) {
  lines <- character(0)
  for (i in seq_len(nrow(summary_df))) {
    row <- summary_df[i, ]
    test_label <- switch(
      row$test_type,
      one_sample = "one-sample t-test",
      independent = "independent-samples t-test",
      paired = "paired-samples t-test",
      "t-test"
    )
    if (is.na(row$t) || is.na(row$df)) {
      line <- sprintf("%s: %s could not be computed (n = %s).",
                      row$variable,
                      test_label,
                      ifelse(is.na(row$n_1), "NA", as.character(row$n_1)))
      lines <- c(lines, line)
      next
    }

    ci_text <- format_ci(row$ci_low, row$ci_high, digits)
    p_text <- format_p(row$p)

    if (row$test_type == "one_sample") {
      line <- sprintf(
        "%s: %s against mu = %s, M = %s, SD = %s, t(%s) = %s, p %s, d = %s, %s%% CI %s.",
        row$variable,
        test_label,
        format_stat(row$mu, digits),
        format_num(row$mean_1, digits),
        format_num(row$sd_1, digits),
        format_num(row$df, digits),
        format_stat(row$t, digits),
        p_text,
        format_stat(row$d, digits),
        round(conf_level * 100),
        ci_text
      )
    } else if (row$test_type == "independent") {
      variance_text <- ifelse(var_equal, "equal variances assumed", "Welch correction")
      line <- sprintf(
        "%s: %s (%s; %s: M = %s, SD = %s, n = %s; %s: M = %s, SD = %s, n = %s), t(%s) = %s, p %s, d = %s, %s%% CI %s.",
        row$variable,
        test_label,
        variance_text,
        row$group_1,
        format_num(row$mean_1, digits),
        format_num(row$sd_1, digits),
        ifelse(is.na(row$n_1), "NA", as.character(row$n_1)),
        row$group_2,
        format_num(row$mean_2, digits),
        format_num(row$sd_2, digits),
        ifelse(is.na(row$n_2), "NA", as.character(row$n_2)),
        format_num(row$df, digits),
        format_stat(row$t, digits),
        p_text,
        format_stat(row$d, digits),
        round(conf_level * 100),
        ci_text
      )
    } else {
      line <- sprintf(
        "%s: %s, %s (M = %s, SD = %s) vs %s (M = %s, SD = %s), t(%s) = %s, p %s, d = %s, %s%% CI %s.",
        row$variable,
        test_label,
        row$measure_1,
        format_num(row$mean_1, digits),
        format_num(row$sd_1, digits),
        row$measure_2,
        format_num(row$mean_2, digits),
        format_num(row$sd_2, digits),
        format_num(row$df, digits),
        format_stat(row$t, digits),
        p_text,
        format_stat(row$d, digits),
        round(conf_level * 100),
        ci_text
      )
    }
    lines <- c(lines, line)
  }
  paste(lines, collapse = "\n")
}

build_ttest_table_body <- function(summary_df, digits, table_meta) {
  default_specs <- list(
    list(key = "test_type", label = "Test"),
    list(key = "variable", label = "Variable"),
    list(key = "measure_1", label = "Measure 1", drop_if_empty = TRUE),
    list(key = "measure_2", label = "Measure 2", drop_if_empty = TRUE),
    list(key = "group_1", label = "Group 1", drop_if_empty = TRUE),
    list(key = "group_2", label = "Group 2", drop_if_empty = TRUE),
    list(key = "n_1", label = "n1"),
    list(key = "n_2", label = "n2", drop_if_empty = TRUE),
    list(key = "mean_1", label = "M1"),
    list(key = "mean_2", label = "M2", drop_if_empty = TRUE),
    list(key = "sd_1", label = "SD1"),
    list(key = "sd_2", label = "SD2", drop_if_empty = TRUE),
    list(key = "mean_diff", label = "Mean diff"),
    list(key = "t", label = "t"),
    list(key = "df", label = "df"),
    list(key = "p", label = "p"),
    list(key = "d", label = "d"),
    list(key = "ci_low", label = "CI low", drop_if_empty = TRUE),
    list(key = "ci_high", label = "CI high", drop_if_empty = TRUE),
    list(key = "boot_ci_low", label = "Boot CI low", drop_if_empty = TRUE),
    list(key = "boot_ci_high", label = "Boot CI high", drop_if_empty = TRUE),
    list(key = "boot_d_ci_low", label = "Boot d CI low", drop_if_empty = TRUE),
    list(key = "boot_d_ci_high", label = "Boot d CI high", drop_if_empty = TRUE)
  )
  columns <- resolve_normalize_table_columns(table_meta$columns, default_specs)
  rows <- list()
  for (i in seq_len(nrow(summary_df))) {
    row <- summary_df[i, ]
    row_map <- list(
      test_type = format_test_type(row$test_type),
      variable = row$variable,
      measure_1 = row$measure_1,
      measure_2 = row$measure_2,
      group_1 = row$group_1,
      group_2 = row$group_2,
      n_1 = ifelse(is.na(row$n_1), "", as.character(row$n_1)),
      n_2 = ifelse(is.na(row$n_2), "", as.character(row$n_2)),
      mean_1 = format_num(row$mean_1, digits),
      mean_2 = format_num(row$mean_2, digits),
      sd_1 = format_num(row$sd_1, digits),
      sd_2 = format_num(row$sd_2, digits),
      mean_diff = format_num(row$mean_diff, digits),
      t = format_stat(row$t, digits),
      df = format_num(row$df, digits),
      p = format_p(row$p),
      d = format_stat(row$d, digits),
      ci_low = format_stat(row$ci_low, digits),
      ci_high = format_stat(row$ci_high, digits),
      boot_ci_low = format_stat(row$boot_ci_low, digits),
      boot_ci_high = format_stat(row$boot_ci_high, digits),
      boot_d_ci_low = format_stat(row$boot_d_ci_low, digits),
      boot_d_ci_high = format_stat(row$boot_d_ci_high, digits)
    )
    row_vals <- vapply(columns, function(col) {
      resolve_as_cell_text(row_map[[col$key]])
    }, character(1))
    rows[[length(rows) + 1]] <- row_vals
  }
  drop_result <- resolve_drop_empty_columns(columns, rows)
  columns <- drop_result$columns
  rows <- drop_result$rows
  headers <- vapply(columns, function(col) col$label, character(1))
  body <- resolve_render_markdown_table(headers, rows)
  list(body = body, columns = columns)
}

build_ttest_note_tokens <- function(alternative, conf_level, var_equal, bootstrap, bootstrap_samples, mu_value, test_type) {
  alt_note <- ifelse(alternative == "two.sided", "Two-tailed tests.", "One-tailed tests.")
  ci_note <- paste0(round(conf_level * 100), "% confidence intervals.")
  var_note <- ifelse(test_type == "independent",
                     ifelse(var_equal, "Equal variances assumed.", "Welch correction used."),
                     "")
  boot_note <- ifelse(bootstrap, paste0("Bootstrap CIs use ", bootstrap_samples, " resamples."), "")
  mu_note <- ifelse(test_type == "one_sample", paste0("Test value mu = ", mu_value, "."), "")
  parts <- c(alt_note, ci_note, var_note, mu_note, boot_note)
  parts <- parts[nzchar(parts)]
  note_default <- paste(parts, collapse = " ")
  list(note_default = note_default)
}

build_ttest_narrative_rows <- function(summary_df, digits, conf_level, alternative, var_equal) {
  rows <- list()
  apa_text <- format_apa_text(summary_df, digits, conf_level, alternative, var_equal)
  lines <- strsplit(apa_text, "\n", fixed = TRUE)[[1]]
  for (i in seq_len(nrow(summary_df))) {
    row <- summary_df[i, ]
    full_sentence <- if (i <= length(lines)) lines[i] else ""
    rows[[length(rows) + 1]] <- list(
      full_sentence = full_sentence,
      test_type = format_test_type(row$test_type),
      variable = row$variable,
      measure_1 = row$measure_1,
      measure_2 = row$measure_2,
      group_1 = row$group_1,
      group_2 = row$group_2,
      n_1 = row$n_1,
      n_2 = row$n_2,
      mean_1 = format_num(row$mean_1, digits),
      mean_2 = format_num(row$mean_2, digits),
      sd_1 = format_num(row$sd_1, digits),
      sd_2 = format_num(row$sd_2, digits),
      mean_diff = format_num(row$mean_diff, digits),
      t = format_stat(row$t, digits),
      df = format_num(row$df, digits),
      p = format_p(row$p),
      d = format_stat(row$d, digits),
      ci = format_ci(row$ci_low, row$ci_high, digits),
      conf_level = round(conf_level * 100),
      mu = format_stat(row$mu, digits)
    )
  }
  rows
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
  vars_default <- resolve_config_value("modules.t_test.vars_default", "numeric")
  mu_default <- resolve_config_value("modules.t_test.mu", 0.0)
  alternative_default <- resolve_config_value("modules.t_test.alternative", "two.sided")
  var_equal_default <- resolve_config_value("modules.t_test.var_equal", FALSE)
  conf_default <- resolve_config_value("modules.t_test.conf_level", 0.95)
  bootstrap_default <- resolve_config_value("modules.t_test.bootstrap", FALSE)
  bootstrap_samples_default <- resolve_config_value("modules.t_test.bootstrap_samples", 1000)

  digits <- if (!is.null(opts$digits)) as.numeric(opts$digits) else digits_default
  df <- resolve_load_dataframe(opts)
  out_dir <- resolve_get_workspace_out_dir(df)
  expect_two_groups <- resolve_parse_bool(opts$`expect-two-groups`, default = FALSE)

  has_group <- !is.null(opts$group) && opts$group != ""
  has_x <- !is.null(opts$x) && opts$x != ""
  has_y <- !is.null(opts$y) && opts$y != ""
  if (has_group && (has_x || has_y)) {
    emit_input_issue(
      out_dir,
      opts,
      "Paired tests do not use --group.",
      details = list(group = opts$group, x = opts$x, y = opts$y)
    )
  }

  mode <- if (has_x || has_y) {
    "paired"
  } else if (has_group) {
    "independent"
  } else {
    "one_sample"
  }

  alternative <- normalize_alternative(opts$alternative, alternative_default)
  var_equal <- resolve_parse_bool(opts$`var-equal`, default = var_equal_default)
  conf_level <- if (!is.null(opts$`conf-level`)) as.numeric(opts$`conf-level`) else conf_default
  bootstrap <- resolve_parse_bool(opts$bootstrap, default = bootstrap_default)
  bootstrap_samples <- if (!is.null(opts$`bootstrap-samples`)) as.numeric(opts$`bootstrap-samples`) else bootstrap_samples_default
  seed <- if (!is.null(opts$seed) && opts$seed != "") opts$seed else NULL
  if (!is.null(seed) && nzchar(seed)) {
    set.seed(as.numeric(seed))
  }

  if (mode == "paired") {
    if (!(has_x && has_y)) {
      emit_input_issue(
        out_dir,
        opts,
        "Paired t-tests require both --x and --y.",
        details = list(x = opts$x, y = opts$y)
      )
    }
    x_vars <- resolve_parse_list(opts$x)
    y_vars <- resolve_parse_list(opts$y)
    if (length(x_vars) == 0 || length(y_vars) == 0) {
      emit_input_issue(
        out_dir,
        opts,
        "Paired t-tests require --x and --y variables.",
        details = list(x = opts$x, y = opts$y)
      )
    }
    if (length(x_vars) != length(y_vars)) {
      emit_input_issue(
        out_dir,
        opts,
        "--x and --y must have the same number of variables.",
        details = list(x = x_vars, y = y_vars)
      )
    }
    missing <- setdiff(c(x_vars, y_vars), names(df))
    if (length(missing) > 0) {
      emit_input_issue(
        out_dir,
        opts,
        paste("Unknown variables:", paste(missing, collapse = ", ")),
        details = list(missing = missing)
      )
    }
    result <- build_summary_paired(df, x_vars, y_vars, alternative, conf_level, bootstrap, bootstrap_samples, seed)
  } else if (mode == "independent") {
    group_var <- opts$group
    if (!(group_var %in% names(df))) stop("Grouping variable not found in data frame.")
    group_vals <- df[[group_var]]
    levels <- unique(group_vals[!is.na(group_vals)])
    if (length(levels) != 2) {
      emit_group_level_issue(out_dir, opts, group_var, levels, expected = expect_two_groups)
    }
    vars <- resolve_select_variables(df, opts$vars, group_var, default = vars_default)
    if (length(vars) == 0) stop("No numeric variables available for analysis.")
    result <- build_summary_independent(
      df,
      vars,
      group_var,
      alternative,
      var_equal,
      conf_level,
      bootstrap,
      bootstrap_samples,
      seed
    )
  } else {
    vars <- resolve_select_variables(df, opts$vars, NULL, default = vars_default)
    if (length(vars) == 0) stop("No numeric variables available for analysis.")
    mu_value <- if (!is.null(opts$mu)) as.numeric(opts$mu) else mu_default
    result <- build_summary_one_sample(
      df,
      vars,
      mu_value,
      alternative,
      conf_level,
      bootstrap,
      bootstrap_samples,
      seed
    )
  }

  summary_df <- result$summary
  diagnostics_df <- result$diagnostics
  if (nrow(summary_df) == 0) stop("No valid t-tests could be computed.")

  mu_value <- if (!is.null(opts$mu)) as.numeric(opts$mu) else mu_default
  note_tokens <- build_ttest_note_tokens(
    alternative = alternative,
    conf_level = conf_level,
    var_equal = var_equal,
    bootstrap = bootstrap,
    bootstrap_samples = bootstrap_samples,
    mu_value = mu_value,
    test_type = mode
  )
  apa_report_path <- file.path(out_dir, "apa_report.md")
  apa_text <- format_apa_text(summary_df, digits, conf_level, alternative, var_equal)
  apa_table <- format_apa_table(summary_df, digits, note_tokens$note_default)
  template_override <- resolve_template_override(opts$template, module = "t_test")
  template_path <- if (!is.null(template_override)) {
    template_override
  } else {
    resolve_get_template_path("t_test.default", "t-test/default-template.md")
  }
  template_meta <- resolve_get_template_meta(template_path)
  table_result <- build_ttest_table_body(summary_df, digits, template_meta$table)
  narrative_rows <- build_ttest_narrative_rows(summary_df, digits, conf_level, alternative, var_equal)
  template_context <- list(
    tokens = c(
      list(
        table_body = table_result$body,
        narrative_default = apa_text
      ),
      note_tokens
    ),
    narrative_rows = narrative_rows
  )

  analysis_flags <- list(
    vars = if (exists("vars")) vars else NULL,
    x = if (exists("x_vars")) x_vars else NULL,
    y = if (exists("y_vars")) y_vars else NULL,
    group = if (exists("group_var")) group_var else NULL,
    mu = if (mode == "one_sample") mu_value else NULL,
    alternative = alternative,
    "var-equal" = if (mode == "independent") var_equal else NULL,
    "conf-level" = conf_level,
    bootstrap = bootstrap,
    "bootstrap-samples" = if (bootstrap) bootstrap_samples else NULL,
    digits = digits
  )

  resolve_append_apa_report(
    apa_report_path,
    "t-tests",
    apa_table,
    apa_text,
    analysis_flags = analysis_flags,
    template_path = template_path,
    template_context = template_context
  )

  cat("Wrote:\n")
  cat("- ", apa_report_path, "\n", sep = "")

  if (resolve_parse_bool(opts$log, default = log_default)) {
    ctx <- resolve_get_run_context()
    resolve_append_analysis_log(
      out_dir,
      module = "t_test",
      prompt = ctx$prompt,
      commands = ctx$commands,
      results = list(summary_df = summary_df, diagnostics_df = diagnostics_df),
      options = list(
        mode = mode,
        vars = if (exists("vars")) vars else NULL,
        x = if (exists("x_vars")) x_vars else NULL,
        y = if (exists("y_vars")) y_vars else NULL,
        group = if (exists("group_var")) group_var else NULL,
        mu = ifelse(mode == "one_sample", mu_value, NA),
        alternative = alternative,
        var_equal = ifelse(mode == "independent", var_equal, NA),
        conf_level = conf_level,
        bootstrap = bootstrap,
        bootstrap_samples = bootstrap_samples,
        digits = digits,
        expect_two_groups = expect_two_groups
      ),
      user_prompt = resolve_get_user_prompt(opts)
    )
  }
}

main()
