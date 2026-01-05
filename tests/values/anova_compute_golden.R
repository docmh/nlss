#!/usr/bin/env Rscript
# SPDX-License-Identifier: Apache-2.0

args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(flag, default = NULL) {
  idx <- which(args == flag)
  if (length(idx) == 0) return(default)
  if (idx[length(idx)] + 1 > length(args)) return(default)
  args[idx[length(idx)] + 1]
}

trim_arg <- function(x) {
  if (is.null(x)) return(NULL)
  x <- trimws(x)
  if (x == "") return(NULL)
  x
}

split_list <- function(x) {
  x <- trim_arg(x)
  if (is.null(x)) return(character(0))
  parts <- unlist(strsplit(x, ",", fixed = TRUE))
  parts <- trimws(parts)
  parts[parts != ""]
}

extract_table <- function(obj) {
  if (is.data.frame(obj)) return(obj)
  if (is.list(obj) && length(obj) > 0 && is.data.frame(obj[[1]])) return(obj[[1]])
  NULL
}

label_stratum <- function(stratum_name, subject_id, within_name) {
  if (is.null(stratum_name) || !nzchar(stratum_name)) return("")
  label <- gsub("^Error:\\s*", "", stratum_name)
  if (!is.null(subject_id) && nzchar(subject_id)) {
    if (grepl(paste0(subject_id, ":", within_name), label, fixed = TRUE)) return("Within")
    if (grepl(subject_id, label, fixed = TRUE)) return("Between")
  }
  label
}

compute_between_rows <- function(df, dv, between_vars, covariates = character(0)) {
  required <- unique(c(dv, between_vars, covariates))
  data_subset <- df[, required, drop = FALSE]
  data_subset <- data_subset[complete.cases(data_subset), , drop = FALSE]
  if (nrow(data_subset) == 0) stop("No complete cases for between ANOVA")
  for (var in between_vars) {
    data_subset[[var]] <- as.factor(data_subset[[var]])
  }
  for (var in covariates) {
    data_subset[[var]] <- suppressWarnings(as.numeric(as.character(data_subset[[var]])))
  }
  between_term <- if (length(between_vars) > 0) paste(between_vars, collapse = " * ") else "1"
  cov_term <- if (length(covariates) > 0) paste(covariates, collapse = " + ") else ""
  rhs <- between_term
  if (nzchar(cov_term)) rhs <- paste(rhs, "+", cov_term)
  fit <- lm(stats::as.formula(paste(dv, "~", rhs)), data = data_subset)
  anova_tbl <- anova(fit)
  residual_df <- df.residual(fit)
  residual_ss <- sum(resid(fit)^2, na.rm = TRUE)
  ss_total <- sum(anova_tbl$`Sum Sq`[rownames(anova_tbl) != "Residuals"], na.rm = TRUE) + residual_ss
  ms_error <- if (!is.na(residual_df) && residual_df > 0) residual_ss / residual_df else NA_real_
  rows <- list()
  term_names <- rownames(anova_tbl)
  for (i in seq_len(nrow(anova_tbl))) {
    term <- term_names[i]
    if (term %in% c("(Intercept)", "Residuals")) next
    row <- anova_tbl[i, ]
    df1 <- as.numeric(row$Df)
    ss <- as.numeric(row$`Sum Sq`)
    ms <- if ("Mean Sq" %in% names(row)) as.numeric(row$`Mean Sq`) else ss / df1
    f_val <- if ("F value" %in% names(row)) as.numeric(row$`F value`) else ms / ms_error
    p_val <- if ("Pr(>F)" %in% names(row)) as.numeric(row$`Pr(>F)`) else NA_real_
    eta_sq <- if (!is.na(ss_total) && ss_total > 0) ss / ss_total else NA_real_
    partial_eta <- if (!is.na(residual_ss) && (ss + residual_ss) > 0) ss / (ss + residual_ss) else NA_real_
    omega_sq <- if (!is.na(ms_error) && !is.na(ss_total) && (ss_total + ms_error) > 0) {
      (ss - df1 * ms_error) / (ss_total + ms_error)
    } else {
      NA_real_
    }
    partial_omega <- if (!is.na(ms_error) && !is.na(residual_ss) && (ss + residual_ss + ms_error) > 0) {
      (ss - df1 * ms_error) / (ss + residual_ss + ms_error)
    } else {
      NA_real_
    }
    rows[[length(rows) + 1]] <- data.frame(
      mode = "between",
      model = "Between",
      term = term,
      dv = dv,
      between = paste(between_vars, collapse = ","),
      within = "",
      subject_id = "",
      df1 = df1,
      df2 = residual_df,
      ss = ss,
      ms = ms,
      f = f_val,
      p = p_val,
      eta_sq = eta_sq,
      partial_eta_sq = partial_eta,
      omega_sq = omega_sq,
      partial_omega_sq = partial_omega,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

compute_between <- function(df, dv, group_var) {
  rows <- compute_between_rows(df, dv, c(group_var))
  if (is.null(rows) || nrow(rows) == 0) stop("No between ANOVA rows")
  rows[1, , drop = FALSE]
}

compute_within <- function(df, subject_id, within_vars) {
  required <- c(subject_id, within_vars)
  data_subset <- df[, required, drop = FALSE]
  data_subset <- data_subset[complete.cases(data_subset), , drop = FALSE]
  if (nrow(data_subset) == 0) stop("No complete cases for within ANOVA")
  for (var in within_vars) {
    if (!is.numeric(data_subset[[var]])) {
      data_subset[[var]] <- suppressWarnings(as.numeric(as.character(data_subset[[var]])))
    }
  }
  within_name <- "within"
  long_data <- reshape(
    data_subset,
    varying = within_vars,
    v.names = "dv",
    timevar = within_name,
    times = within_vars,
    idvar = subject_id,
    direction = "long"
  )
  long_data[[within_name]] <- factor(long_data[[within_name]], levels = within_vars)
  fit <- aov(stats::as.formula(paste("dv ~", within_name, "+ Error(", subject_id, "/", within_name, ")")), data = long_data)
  summaries <- summary(fit)

  ss_total <- 0
  for (name in names(summaries)) {
    table <- extract_table(summaries[[name]])
    if (is.null(table) || !("Sum Sq" %in% names(table))) next
    ss_total <- ss_total + sum(table$`Sum Sq`, na.rm = TRUE)
  }

  target_table <- NULL
  for (name in names(summaries)) {
    table <- extract_table(summaries[[name]])
    if (is.null(table) || !("Sum Sq" %in% names(table))) next
    row_names <- trimws(rownames(table))
    if (!(within_name %in% row_names)) next
    if ("Residuals" %in% row_names) {
      target_table <- table
      break
    }
    if (is.null(target_table)) target_table <- table
  }
  if (is.null(target_table)) stop("Within summary table missing")

  row_names <- trimws(rownames(target_table))
  resid_idx <- which(row_names == "Residuals")
  if (length(resid_idx) == 0) stop("Residual row not found in within table")
  error_ss <- as.numeric(target_table$`Sum Sq`[resid_idx[1]])
  error_df <- as.numeric(target_table$Df[resid_idx[1]])
  ms_error <- if (!is.na(error_df) && error_df > 0) error_ss / error_df else NA_real_

  term_idx <- which(row_names == within_name)
  if (length(term_idx) == 0) stop("Within term row not found")
  ss <- as.numeric(target_table$`Sum Sq`[term_idx[1]])
  df1 <- as.numeric(target_table$Df[term_idx[1]])
  ms <- if ("Mean Sq" %in% names(target_table)) as.numeric(target_table$`Mean Sq`[term_idx[1]]) else ss / df1
  f_val <- if ("F value" %in% names(target_table)) as.numeric(target_table$`F value`[term_idx[1]]) else NA_real_
  p_val <- if ("Pr(>F)" %in% names(target_table)) as.numeric(target_table$`Pr(>F)`[term_idx[1]]) else NA_real_

  eta_sq <- if (!is.na(ss_total) && ss_total > 0) ss / ss_total else NA_real_
  partial_eta <- if (!is.na(error_ss) && (ss + error_ss) > 0) ss / (ss + error_ss) else NA_real_
  omega_sq <- if (!is.na(ms_error) && !is.na(ss_total) && (ss_total + ms_error) > 0) {
    (ss - df1 * ms_error) / (ss_total + ms_error)
  } else {
    NA_real_
  }
  partial_omega <- if (!is.na(ms_error) && !is.na(error_ss) && (ss + error_ss + ms_error) > 0) {
    (ss - df1 * ms_error) / (ss + error_ss + ms_error)
  } else {
    NA_real_
  }

  list(
    mode = "within",
    model = "Within",
    term = within_name,
    dv = "",
    between = "",
    within = paste(within_vars, collapse = ","),
    subject_id = subject_id,
    df1 = df1,
    df2 = error_df,
    ss = ss,
    ms = ms,
    f = f_val,
    p = p_val,
    eta_sq = eta_sq,
    partial_eta_sq = partial_eta,
    omega_sq = omega_sq,
    partial_omega_sq = partial_omega
  )
}

compute_mixed_rows <- function(df, subject_id, within_vars, between_vars, covariates = character(0)) {
  required <- unique(c(subject_id, within_vars, between_vars, covariates))
  data_subset <- df[, required, drop = FALSE]
  data_subset <- data_subset[complete.cases(data_subset), , drop = FALSE]
  if (nrow(data_subset) == 0) stop("No complete cases for mixed ANOVA")
  for (var in between_vars) {
    data_subset[[var]] <- as.factor(data_subset[[var]])
  }
  for (var in covariates) {
    data_subset[[var]] <- suppressWarnings(as.numeric(as.character(data_subset[[var]])))
  }
  for (var in within_vars) {
    if (!is.numeric(data_subset[[var]])) {
      data_subset[[var]] <- suppressWarnings(as.numeric(as.character(data_subset[[var]])))
    }
  }
  within_name <- "within"
  long_data <- reshape(
    data_subset,
    varying = within_vars,
    v.names = "dv",
    timevar = within_name,
    times = within_vars,
    idvar = subject_id,
    direction = "long"
  )
  long_data[[within_name]] <- factor(long_data[[within_name]], levels = within_vars)
  between_term <- if (length(between_vars) > 0) paste(between_vars, collapse = " * ") else ""
  fixed_term <- if (nzchar(between_term)) paste(between_term, "*", within_name) else within_name
  if (length(covariates) > 0) {
    fixed_term <- paste(fixed_term, "+", paste(covariates, collapse = " + "))
  }
  formula <- stats::as.formula(paste("dv ~", fixed_term, "+ Error(", subject_id, "/", within_name, ")"))
  fit <- aov(formula, data = long_data)
  summaries <- summary(fit)

  ss_total <- 0
  for (name in names(summaries)) {
    table <- extract_table(summaries[[name]])
    if (is.null(table) || !("Sum Sq" %in% names(table))) next
    ss_total <- ss_total + sum(table$`Sum Sq`, na.rm = TRUE)
  }

  rows <- list()
  for (name in names(summaries)) {
    table <- extract_table(summaries[[name]])
    if (is.null(table) || !("Sum Sq" %in% names(table))) next
    row_names <- trimws(rownames(table))
    resid_idx <- which(row_names == "Residuals")
    error_ss <- if (length(resid_idx) > 0) as.numeric(table$`Sum Sq`[resid_idx[1]]) else NA_real_
    error_df <- if (length(resid_idx) > 0) as.numeric(table$Df[resid_idx[1]]) else NA_real_
    ms_error <- if (!is.na(error_df) && error_df > 0) error_ss / error_df else NA_real_
    model_label <- label_stratum(name, subject_id, within_name)

    for (i in seq_len(nrow(table))) {
      term <- row_names[i]
      if (term %in% c("Residuals", "(Intercept)")) next
      row <- table[i, ]
      df1 <- as.numeric(row$Df)
      ss <- as.numeric(row$`Sum Sq`)
      ms <- if ("Mean Sq" %in% names(row)) as.numeric(row$`Mean Sq`) else ss / df1
      f_val <- if ("F value" %in% names(row)) as.numeric(row$`F value`) else NA_real_
      p_val <- if ("Pr(>F)" %in% names(row)) as.numeric(row$`Pr(>F)`) else NA_real_
      eta_sq <- if (!is.na(ss_total) && ss_total > 0) ss / ss_total else NA_real_
      partial_eta <- if (!is.na(error_ss) && (ss + error_ss) > 0) ss / (ss + error_ss) else NA_real_
      omega_sq <- if (!is.na(ms_error) && !is.na(ss_total) && (ss_total + ms_error) > 0) {
        (ss - df1 * ms_error) / (ss_total + ms_error)
      } else {
        NA_real_
      }
      partial_omega <- if (!is.na(ms_error) && !is.na(error_ss) && (ss + error_ss + ms_error) > 0) {
        (ss - df1 * ms_error) / (ss + error_ss + ms_error)
      } else {
        NA_real_
      }
      rows[[length(rows) + 1]] <- data.frame(
        mode = "mixed",
        model = model_label,
        term = term,
        dv = "",
        between = paste(between_vars, collapse = ","),
        within = paste(within_vars, collapse = ","),
        subject_id = subject_id,
        df1 = df1,
        df2 = error_df,
        ss = ss,
        ms = ms,
        f = f_val,
        p = p_val,
        eta_sq = eta_sq,
        partial_eta_sq = partial_eta,
        omega_sq = omega_sq,
        partial_omega_sq = partial_omega,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

pick_row <- function(df_rows, model, term) {
  model <- trimws(model)
  term <- trimws(term)
  matches <- df_rows[trimws(df_rows$model) == model & trimws(df_rows$term) == term, , drop = FALSE]
  if (nrow(matches) == 0) stop(paste("No matching row for", model, term))
  idx <- which(!is.na(matches$f))
  if (length(idx) > 0) return(matches[idx[1], , drop = FALSE])
  matches[1, , drop = FALSE]
}

# Defaults
root <- getwd()
data_path <- trim_arg(get_arg("--data", file.path("tests", "data", "golden_dataset.csv")))
out_path <- trim_arg(get_arg("--out", file.path("tests", "values", "anova_golden.csv")))
posthoc_out <- trim_arg(get_arg("--posthoc-out", file.path("tests", "values", "anova_posthoc_golden.csv")))
contrasts_out <- trim_arg(get_arg("--contrasts-out", file.path("tests", "values", "anova_contrasts_golden.csv")))
assumptions_out <- trim_arg(get_arg("--assumptions-out", file.path("tests", "values", "anova_assumptions_golden.csv")))

df <- read.csv(data_path, stringsAsFactors = FALSE)

between_row <- compute_between(df, "outcome_anova", "group3")
within_row <- compute_within(df, "id", c("pre_score", "mid_score", "post_score"))
between_factorial_rows <- compute_between_rows(df, "outcome_anova", c("group3", "gender"))
mixed_rows <- compute_mixed_rows(df, "id", c("pre_score", "mid_score", "post_score"), c("group3"))

between_factorial_group3 <- pick_row(between_factorial_rows, "Between", "group3")
between_factorial_group3$mode <- "between_factorial_group3"
between_factorial_gender <- pick_row(between_factorial_rows, "Between", "gender")
between_factorial_gender$mode <- "between_factorial_gender"
between_factorial_interaction <- pick_row(between_factorial_rows, "Between", "group3:gender")
between_factorial_interaction$mode <- "between_factorial_interaction"

mixed_between_group3 <- pick_row(mixed_rows, "Between", "group3")
mixed_between_group3$mode <- "mixed_between_group3"
mixed_within_main <- pick_row(mixed_rows, "Within", "within")
mixed_within_main$mode <- "mixed_within_within"
mixed_interaction <- pick_row(mixed_rows, "Within", "group3:within")
mixed_interaction$mode <- "mixed_interaction_group3_within"

rows <- rbind(
  as.data.frame(between_row, stringsAsFactors = FALSE),
  as.data.frame(within_row, stringsAsFactors = FALSE),
  between_factorial_group3,
  between_factorial_gender,
  between_factorial_interaction,
  mixed_between_group3,
  mixed_within_main,
  mixed_interaction
)

# Ensure numeric columns stay numeric
num_cols <- c("df1", "df2", "ss", "ms", "f", "p", "eta_sq", "partial_eta_sq", "omega_sq", "partial_omega_sq")
for (col in num_cols) {
  rows[[col]] <- as.numeric(rows[[col]])
}

# Write CSV
options(scipen = 999)
write.csv(rows, out_path, row.names = FALSE)
cat("Wrote golden values to", out_path, "\n")

compute_posthoc_tukey <- function(df, dv, group_var, contrast_label) {
  data_subset <- df[, c(dv, group_var), drop = FALSE]
  data_subset <- data_subset[complete.cases(data_subset), , drop = FALSE]
  data_subset[[group_var]] <- as.factor(data_subset[[group_var]])
  fit <- aov(stats::as.formula(paste(dv, "~", group_var)), data = data_subset)
  tukey <- TukeyHSD(fit)
  table <- as.data.frame(tukey[[group_var]])
  table$contrast <- rownames(table)
  row <- table[table$contrast == contrast_label, , drop = FALSE]
  if (nrow(row) == 0) stop("Tukey contrast not found")
  parts <- strsplit(contrast_label, "-", fixed = TRUE)[[1]]
  group_1 <- if (length(parts) >= 1) parts[1] else ""
  group_2 <- if (length(parts) >= 2) parts[2] else ""
  data.frame(
    case_id = paste0("posthoc_between_tukey_", gsub(" ", "", contrast_label)),
    term = group_var,
    group = "",
    group_1 = group_1,
    group_2 = group_2,
    method = "tukey",
    mean_diff = row$diff,
    t = NA_real_,
    df = NA_real_,
    p = NA_real_,
    p_adj = row$`p adj`,
    ci_low = row$lwr,
    ci_high = row$upr,
    stringsAsFactors = FALSE
  )
}

compute_posthoc_within <- function(df, within_vars, pair, p_adjust = "holm", conf_level = 0.95) {
  data_subset <- df[, within_vars, drop = FALSE]
  data_subset <- data_subset[complete.cases(data_subset), , drop = FALSE]
  v1 <- pair[1]
  v2 <- pair[2]
  test <- t.test(data_subset[[v1]], data_subset[[v2]], paired = TRUE, conf.level = conf_level)
  mean_diff <- mean(data_subset[[v1]] - data_subset[[v2]], na.rm = TRUE)
  p_adj <- p.adjust(c(test$p.value), method = p_adjust)[1]
  data.frame(
    case_id = paste0("posthoc_within_paired_", v1, "_", v2),
    term = "within",
    group = "Overall",
    group_1 = v1,
    group_2 = v2,
    method = "paired",
    mean_diff = mean_diff,
    t = unname(test$statistic),
    df = unname(test$parameter),
    p = test$p.value,
    p_adj = p_adj,
    ci_low = test$conf.int[1],
    ci_high = test$conf.int[2],
    stringsAsFactors = FALSE
  )
}

compute_posthoc_between_pairwise <- function(df, dv, group_var, contrast_label, p_adjust = "holm", conf_level = 0.95) {
  data_subset <- df[, c(dv, group_var), drop = FALSE]
  data_subset <- data_subset[complete.cases(data_subset), , drop = FALSE]
  data_subset[[group_var]] <- as.factor(data_subset[[group_var]])
  levels <- unique(data_subset[[group_var]])
  pairs <- combn(as.character(levels), 2, simplify = FALSE)
  p_vals <- numeric(0)
  rows <- list()
  for (pair in pairs) {
    g1 <- pair[1]
    g2 <- pair[2]
    x <- data_subset[[dv]][data_subset[[group_var]] == g1]
    y <- data_subset[[dv]][data_subset[[group_var]] == g2]
    test <- t.test(x, y, paired = FALSE, conf.level = conf_level)
    mean_diff <- mean(x, na.rm = TRUE) - mean(y, na.rm = TRUE)
    rows[[length(rows) + 1]] <- list(
      term = group_var,
      group = "",
      group_1 = g1,
      group_2 = g2,
      contrast = paste(g1, g2, sep = "-"),
      mean_diff = mean_diff,
      t = unname(test$statistic),
      df = unname(test$parameter),
      p = test$p.value,
      p_adj = NA_real_,
      ci_low = test$conf.int[1],
      ci_high = test$conf.int[2],
      method = "pairwise"
    )
    p_vals <- c(p_vals, test$p.value)
  }
  p_adj_vals <- p.adjust(p_vals, method = p_adjust)
  for (i in seq_along(rows)) {
    rows[[i]]$p_adj <- p_adj_vals[i]
  }
  matches <- Filter(function(row) row$contrast == contrast_label, rows)
  if (length(matches) == 0) stop("Pairwise contrast not found")
  row <- matches[[1]]
  data.frame(
    case_id = paste0("posthoc_between_pairwise_", gsub(" ", "", contrast_label)),
    term = row$term,
    group = row$group,
    group_1 = row$group_1,
    group_2 = row$group_2,
    method = row$method,
    mean_diff = row$mean_diff,
    t = row$t,
    df = row$df,
    p = row$p,
    p_adj = row$p_adj,
    ci_low = row$ci_low,
    ci_high = row$ci_high,
    stringsAsFactors = FALSE
  )
}

compute_posthoc_within_grouped <- function(df, within_vars, group_var, group_value, pair, p_adjust = "holm", conf_level = 0.95) {
  required <- c(within_vars, group_var)
  data_subset <- df[, required, drop = FALSE]
  data_subset <- data_subset[complete.cases(data_subset), , drop = FALSE]
  data_subset[[group_var]] <- as.factor(data_subset[[group_var]])
  subset_data <- data_subset[data_subset[[group_var]] == group_value, , drop = FALSE]
  if (nrow(subset_data) == 0) stop("No rows for grouped within posthoc")
  pairs <- combn(within_vars, 2, simplify = FALSE)
  p_vals <- numeric(0)
  rows <- list()
  for (pair_vals in pairs) {
    v1 <- pair_vals[1]
    v2 <- pair_vals[2]
    test <- t.test(subset_data[[v1]], subset_data[[v2]], paired = TRUE, conf.level = conf_level)
    mean_diff <- mean(subset_data[[v1]] - subset_data[[v2]], na.rm = TRUE)
    rows[[length(rows) + 1]] <- list(
      term = "within",
      group = paste0(group_var, "=", group_value),
      group_1 = v1,
      group_2 = v2,
      contrast = paste(v1, v2, sep = "-"),
      mean_diff = mean_diff,
      t = unname(test$statistic),
      df = unname(test$parameter),
      p = test$p.value,
      p_adj = NA_real_,
      ci_low = test$conf.int[1],
      ci_high = test$conf.int[2],
      method = "paired"
    )
    p_vals <- c(p_vals, test$p.value)
  }
  p_adj_vals <- p.adjust(p_vals, method = p_adjust)
  for (i in seq_along(rows)) {
    rows[[i]]$p_adj <- p_adj_vals[i]
  }
  match_label <- paste(pair[1], pair[2], sep = "-")
  matches <- Filter(function(row) row$contrast == match_label, rows)
  if (length(matches) == 0) stop("Grouped within contrast not found")
  row <- matches[[1]]
  data.frame(
    case_id = paste0("posthoc_within_grouped_", group_var, group_value, "_", pair[1], "_", pair[2]),
    term = row$term,
    group = row$group,
    group_1 = row$group_1,
    group_2 = row$group_2,
    method = row$method,
    mean_diff = row$mean_diff,
    t = row$t,
    df = row$df,
    p = row$p,
    p_adj = row$p_adj,
    ci_low = row$ci_low,
    ci_high = row$ci_high,
    stringsAsFactors = FALSE
  )
}

posthoc_rows <- rbind(
  compute_posthoc_tukey(df, "outcome_anova", "group3", "B-A"),
  compute_posthoc_between_pairwise(df, "outcome_anova", "group3", "B-A"),
  compute_posthoc_within(df, c("pre_score", "mid_score", "post_score"), c("pre_score", "mid_score")),
  compute_posthoc_within_grouped(df, c("pre_score", "mid_score", "post_score"), "group3", "A", c("pre_score", "mid_score"))
)
write.csv(posthoc_rows, posthoc_out, row.names = FALSE)
cat("Wrote post-hoc golden values to", posthoc_out, "\n")

compute_contrast_trt_vs_ctrl <- function(df, dv, group_var, p_adjust = "holm", conf_level = 0.95) {
  if (!requireNamespace("emmeans", quietly = TRUE)) {
    stop("emmeans package is required for planned contrasts golden values")
  }
  data_subset <- df[, c(dv, group_var), drop = FALSE]
  data_subset <- data_subset[complete.cases(data_subset), , drop = FALSE]
  data_subset[[group_var]] <- as.factor(data_subset[[group_var]])
  fit <- aov(stats::as.formula(paste(dv, "~", group_var)), data = data_subset)
  emm <- emmeans::emmeans(fit, specs = as.formula(paste("~", group_var)))
  cont <- emmeans::contrast(emm, method = "trt.vs.ctrl")
  summary_tbl <- summary(cont, infer = c(TRUE, TRUE), adjust = p_adjust, level = conf_level)
  summary_tbl <- as.data.frame(summary_tbl)
  row <- summary_tbl[summary_tbl$contrast == "B - A", , drop = FALSE]
  if (nrow(row) == 0) stop("Contrast B - A not found")
  data.frame(
    case_id = "contrast_trt_vs_ctrl_B_minus_A",
    term = group_var,
    contrast = row$contrast,
    estimate = row$estimate,
    se = row$SE,
    df = row$df,
    t = row$t.ratio,
    p = NA_real_,
    p_adj = row$p.value,
    ci_low = row$lower.CL,
    ci_high = row$upper.CL,
    method = "trt.vs.ctrl",
    stringsAsFactors = FALSE
  )
}

contrast_rows <- compute_contrast_trt_vs_ctrl(df, "outcome_anova", "group3")
compute_contrast_custom <- function(df, dv, group_var, p_adjust = "holm", conf_level = 0.95) {
  if (!requireNamespace("emmeans", quietly = TRUE)) {
    stop("emmeans package is required for planned contrasts golden values")
  }
  data_subset <- df[, c(dv, group_var), drop = FALSE]
  data_subset <- data_subset[complete.cases(data_subset), , drop = FALSE]
  data_subset[[group_var]] <- as.factor(data_subset[[group_var]])
  fit <- aov(stats::as.formula(paste(dv, "~", group_var)), data = data_subset)
  emm <- emmeans::emmeans(fit, specs = as.formula(paste("~", group_var)))
  methods <- list(A_vs_B = c(1, -1, 0), A_vs_C = c(1, 0, -1))
  cont <- emmeans::contrast(emm, method = methods)
  summary_tbl <- summary(cont, infer = c(TRUE, TRUE), adjust = p_adjust, level = conf_level)
  summary_tbl <- as.data.frame(summary_tbl)
  row <- summary_tbl[summary_tbl$contrast == "A_vs_B", , drop = FALSE]
  if (nrow(row) == 0) stop("Contrast A_vs_B not found")
  data.frame(
    case_id = "contrast_custom_A_vs_B",
    term = group_var,
    contrast = row$contrast,
    estimate = row$estimate,
    se = row$SE,
    df = row$df,
    t = row$t.ratio,
    p = NA_real_,
    p_adj = row$p.value,
    ci_low = row$lower.CL,
    ci_high = row$upper.CL,
    method = "custom",
    stringsAsFactors = FALSE
  )
}

contrast_rows <- rbind(
  contrast_rows,
  compute_contrast_custom(df, "outcome_anova", "group3")
)
write.csv(contrast_rows, contrasts_out, row.names = FALSE)
cat("Wrote contrast golden values to", contrasts_out, "\n")

compute_assumptions <- function(df) {
  # Levene (median) for between: outcome_anova ~ group3
  data_subset <- df[, c("outcome_anova", "group3"), drop = FALSE]
  data_subset <- data_subset[complete.cases(data_subset), , drop = FALSE]
  group_factor <- as.factor(data_subset$group3)
  levene_fit <- lm(abs(data_subset$outcome_anova - tapply(data_subset$outcome_anova, group_factor, median, na.rm = TRUE)[as.character(group_factor)]) ~ group_factor)
  levene_res <- anova(levene_fit)
  levene_row <- data.frame(
    case_id = "assumption_between_levene_outcome_anova",
    assumption = "Homogeneity",
    test = "Levene (median)",
    target = "outcome_anova",
    group = "",
    statistic = levene_res$`F value`[1],
    df1 = levene_res$Df[1],
    df2 = levene_res$Df[2],
    p = levene_res$`Pr(>F)`[1],
    stringsAsFactors = FALSE
  )

  # Mauchly for within (pre/mid/post) without between vars
  wide <- df[, c("pre_score", "mid_score", "post_score"), drop = FALSE]
  wide <- wide[complete.cases(wide), , drop = FALSE]
  fit <- lm(cbind(pre_score, mid_score, post_score) ~ 1, data = wide)
  mauchly <- mauchly.test(fit)
  mauchly_row <- data.frame(
    case_id = "assumption_within_mauchly_within",
    assumption = "Sphericity",
    test = "Mauchly",
    target = "Within",
    group = "",
    statistic = unname(mauchly$statistic),
    df1 = NA_real_,
    df2 = NA_real_,
    p = mauchly$p.value,
    stringsAsFactors = FALSE
  )
  rbind(levene_row, mauchly_row)
}

assumption_rows <- compute_assumptions(df)
write.csv(assumption_rows, assumptions_out, row.names = FALSE)
cat("Wrote assumption golden values to", assumptions_out, "\n")
