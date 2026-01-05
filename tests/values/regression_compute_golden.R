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

split_list <- function(x, sep = ",") {
  x <- trim_arg(x)
  if (is.null(x)) return(character(0))
  parts <- unlist(strsplit(x, sep, fixed = TRUE))
  parts <- trimws(parts)
  parts[parts != ""]
}

parse_blocks <- function(value) {
  if (is.null(value)) return(list())
  if (is.list(value)) return(value)
  blocks <- split_list(value, sep = ";")
  lapply(blocks, function(block) split_list(block, sep = ","))
}

parse_interactions <- function(value) {
  if (is.null(value)) return(character(0))
  if (is.list(value) || is.vector(value)) {
    vals <- unlist(value)
    vals <- vals[nzchar(vals)]
    return(as.character(vals))
  }
  split_list(value, sep = ",")
}

parse_term_vars <- function(term) {
  term <- gsub("\\s+", "", as.character(term))
  if (!nzchar(term)) return(character(0))
  parts <- strsplit(term, "[*:]", perl = TRUE)[[1]]
  parts <- parts[parts != ""]
  parts
}

normalize_center <- function(value) {
  val <- if (!is.null(value) && value != "") value else "none"
  val <- tolower(val)
  if (val %in% c("none", "no", "false")) return("none")
  if (val %in% c("mean", "center", "centre")) return("mean")
  "none"
}

normalize_standardize <- function(value) {
  val <- if (!is.null(value) && value != "") value else "none"
  val <- tolower(val)
  if (val %in% c("none", "no", "false")) return("none")
  if (val %in% c("predictors", "pred", "x")) return("predictors")
  "none"
}

normalize_family <- function(value) {
  val <- if (!is.null(value) && value != "") value else "gaussian"
  val <- tolower(val)
  if (val %in% c("gaussian", "normal", "linear", "lm")) return("gaussian")
  if (val %in% c("binomial", "logistic", "logit")) return("binomial")
  if (val %in% c("poisson", "count")) return("poisson")
  "gaussian"
}

normalize_link <- function(value, family) {
  if (!is.null(value) && nzchar(value)) return(as.character(value))
  if (family == "gaussian") return("identity")
  if (family == "binomial") return("logit")
  if (family == "poisson") return("log")
  "identity"
}

as_scalar <- function(value) {
  value <- suppressWarnings(as.numeric(value))
  if (length(value) == 0) return(NA_real_)
  value[1]
}

build_family <- function(family, link) {
  if (family == "binomial") return(binomial(link = link))
  if (family == "poisson") return(poisson(link = link))
  gaussian(link = link)
}

get_complete_rows <- function(df) {
  if (nrow(df) == 0) return(logical(0))
  idx <- complete.cases(df)
  if (any(idx)) {
    num_cols <- names(df)[sapply(df, is.numeric)]
    if (length(num_cols) > 0) {
      finite_idx <- rep(TRUE, nrow(df))
      for (col in num_cols) {
        finite_idx <- finite_idx & is.finite(df[[col]])
      }
      idx <- idx & finite_idx
    }
  }
  idx
}

coerce_predictors <- function(df, predictors) {
  for (var in predictors) {
    if (!var %in% names(df)) next
    if (is.numeric(df[[var]])) next
    df[[var]] <- as.factor(df[[var]])
  }
  df
}

center_predictors <- function(df, predictors, center) {
  if (center != "mean") return(df)
  for (var in predictors) {
    if (!var %in% names(df)) next
    if (!is.numeric(df[[var]])) next
    df[[var]] <- df[[var]] - mean(df[[var]], na.rm = TRUE)
  }
  df
}

compute_standardized_betas <- function(df, dv, term_names, estimates, standardize) {
  if (standardize == "none") return(rep(NA_real_, length(term_names)))
  if (!dv %in% names(df)) return(rep(NA_real_, length(term_names)))
  y <- df[[dv]]
  if (!is.numeric(y)) return(rep(NA_real_, length(term_names)))
  sd_y <- sd(y)
  if (is.na(sd_y) || sd_y == 0) return(rep(NA_real_, length(term_names)))
  betas <- rep(NA_real_, length(term_names))
  for (i in seq_along(term_names)) {
    term <- term_names[i]
    if (!term %in% names(df)) next
    if (!is.numeric(df[[term]])) next
    sd_x <- sd(df[[term]])
    if (is.na(sd_x) || sd_x == 0) next
    betas[i] <- estimates[i] * sd_x / sd_y
  }
  betas
}

safe_fit_model <- function(formula, data, family, link) {
  if (family == "gaussian") {
    return(tryCatch(lm(formula, data = data), error = function(e) NULL))
  }
  fam <- tryCatch(build_family(family, link), error = function(e) NULL)
  if (is.null(fam)) return(NULL)
  tryCatch(glm(formula, data = data, family = fam), error = function(e) NULL)
}

compute_bootstrap_ci <- function(data, formula, family, link, term_names, conf_level, samples) {
  if (samples < 1) return(NULL)
  boot_mat <- matrix(NA_real_, nrow = samples, ncol = length(term_names))
  colnames(boot_mat) <- term_names

  for (i in seq_len(samples)) {
    idx <- sample(seq_len(nrow(data)), replace = TRUE)
    boot_data <- data[idx, , drop = FALSE]
    fit <- safe_fit_model(formula, boot_data, family, link)
    if (is.null(fit)) next
    coefs <- coef(fit)
    for (term in term_names) {
      if (term %in% names(coefs)) {
        boot_mat[i, term] <- unname(coefs[term])
      }
    }
  }

  alpha <- (1 - conf_level) / 2
  ci_low <- apply(boot_mat, 2, function(x) {
    if (all(is.na(x))) return(NA_real_)
    quantile(x, probs = alpha, na.rm = TRUE)
  })
  ci_high <- apply(boot_mat, 2, function(x) {
    if (all(is.na(x))) return(NA_real_)
    quantile(x, probs = 1 - alpha, na.rm = TRUE)
  })

  data.frame(
    term = term_names,
    boot_ci_low = as.numeric(ci_low),
    boot_ci_high = as.numeric(ci_high),
    stringsAsFactors = FALSE
  )
}

extract_coefficients <- function(model, data, dv, family, conf_level, standardize, boot_ci = NULL) {
  summ <- summary(model)
  coef_mat <- summ$coefficients
  term_names <- rownames(coef_mat)
  estimate <- coef_mat[, 1]
  se <- coef_mat[, 2]
  stat <- coef_mat[, 3]
  p_val <- coef_mat[, 4]

  stat_label <- ifelse(family == "gaussian", "t", "z")
  df_resid <- if (family == "gaussian") df.residual(model) else NA_real_
  crit <- if (family == "gaussian") {
    qt(1 - (1 - conf_level) / 2, df_resid)
  } else {
    qnorm(1 - (1 - conf_level) / 2)
  }
  ci_low <- estimate - crit * se
  ci_high <- estimate + crit * se

  beta <- if (family == "gaussian") {
    compute_standardized_betas(data, dv, term_names, estimate, standardize)
  } else {
    rep(NA_real_, length(term_names))
  }

  exp_b <- if (family != "gaussian") exp(estimate) else rep(NA_real_, length(term_names))
  exp_ci_low <- if (family != "gaussian") exp(ci_low) else rep(NA_real_, length(term_names))
  exp_ci_high <- if (family != "gaussian") exp(ci_high) else rep(NA_real_, length(term_names))

  boot_low <- rep(NA_real_, length(term_names))
  boot_high <- rep(NA_real_, length(term_names))
  if (!is.null(boot_ci) && nrow(boot_ci) > 0) {
    match_idx <- match(term_names, boot_ci$term)
    boot_low <- boot_ci$boot_ci_low[match_idx]
    boot_high <- boot_ci$boot_ci_high[match_idx]
  }

  data.frame(
    term = term_names,
    estimate = estimate,
    se = se,
    stat = stat,
    stat_label = stat_label,
    p = p_val,
    ci_low = ci_low,
    ci_high = ci_high,
    beta = beta,
    exp_b = exp_b,
    exp_ci_low = exp_ci_low,
    exp_ci_high = exp_ci_high,
    boot_ci_low = boot_low,
    boot_ci_high = boot_high,
    stringsAsFactors = FALSE
  )
}

calc_model_summary <- function(model, data, dv, family, link) {
  n <- nrow(data)
  if (family == "gaussian") {
    summ <- summary(model)
    f_stat <- summ$fstatistic
    f_val <- unname(f_stat["value"])
    df1 <- unname(f_stat["numdf"])
    df2 <- unname(f_stat["dendf"])
    p_val <- pf(f_val, df1, df2, lower.tail = FALSE)
    r2 <- summ$r.squared
    adj_r2 <- summ$adj.r.squared
    f2 <- if (!is.na(r2) && r2 < 1) r2 / (1 - r2) else NA_real_
    rmse <- sqrt(mean(resid(model)^2))
    return(data.frame(
      model_stat = f_val,
      model_df1 = df1,
      model_df2 = df2,
      model_p = p_val,
      r2 = r2,
      adj_r2 = adj_r2,
      f2 = f2,
      rmse = rmse,
      aic = AIC(model),
      bic = BIC(model),
      deviance = NA_real_,
      null_deviance = NA_real_,
      pseudo_r2 = NA_real_,
      n = n,
      stringsAsFactors = FALSE
    ))
  }

  null_model <- safe_fit_model(as.formula(paste(dv, "~ 1")), data, family, link)
  chisq <- NA_real_
  df_chisq <- NA_real_
  p_val <- NA_real_
  pseudo_r2 <- NA_real_
  model_dev <- as_scalar(model$deviance)
  null_dev <- NA_real_
  if (!is.null(null_model)) {
    null_dev <- as_scalar(null_model$deviance)
    chisq <- as_scalar(null_model$deviance - model$deviance)
    df_chisq <- as_scalar(null_model$df.residual - model$df.residual)
    if (!is.na(chisq) && !is.na(df_chisq) && df_chisq > 0) {
      p_val <- pchisq(chisq, df_chisq, lower.tail = FALSE)
    }
    ll_null <- as.numeric(logLik(null_model))
    ll_full <- as.numeric(logLik(model))
    if (!is.na(ll_null) && ll_null != 0) {
      pseudo_r2 <- 1 - (ll_full / ll_null)
    }
  }
  data.frame(
    model_stat = chisq,
    model_df1 = df_chisq,
    model_df2 = NA_real_,
    model_p = p_val,
    r2 = NA_real_,
    adj_r2 = NA_real_,
    f2 = NA_real_,
    rmse = NA_real_,
    aic = as_scalar(AIC(model)),
    bic = as_scalar(BIC(model)),
    deviance = model_dev,
    null_deviance = null_dev,
    pseudo_r2 = pseudo_r2,
    n = n,
    stringsAsFactors = FALSE
  )
}

run_regression_case <- function(df, dv, ivs = NULL, blocks = NULL, interactions = NULL,
                                group_var = NULL, family = "gaussian", link = NULL,
                                center = "none", standardize = "none",
                                conf_level = 0.95, bootstrap = FALSE, bootstrap_samples = 1000,
                                seed = NULL) {
  family <- normalize_family(family)
  link <- normalize_link(link, family)
  center <- normalize_center(center)
  standardize <- normalize_standardize(standardize)

  if (!is.null(seed)) set.seed(as.numeric(seed))

  blocks_list <- parse_blocks(blocks)
  ivs_list <- if (is.null(ivs)) character(0) else as.character(ivs)
  if (length(blocks_list) == 0) {
    blocks_list <- list(ivs_list)
  }
  blocks_list <- lapply(blocks_list, function(vars) setdiff(vars, c(dv, group_var)))

  interactions_list <- parse_interactions(interactions)
  if (length(interactions_list) > 0) {
    interaction_vars <- unique(unlist(lapply(interactions_list, parse_term_vars)))
    base_predictors <- unique(unlist(blocks_list))
    missing_main <- setdiff(interaction_vars, base_predictors)
    if (length(missing_main) > 0) {
      blocks_list[[1]] <- unique(c(blocks_list[[1]], missing_main))
    }
    blocks_list <- c(blocks_list, list(interactions_list))
  }

  all_block_terms <- unique(unlist(blocks_list))
  all_block_terms <- all_block_terms[all_block_terms != ""]
  if (length(all_block_terms) == 0) stop("No predictors provided")

  cumulative_blocks <- list()
  for (i in seq_along(blocks_list)) {
    cumulative_blocks[[i]] <- unique(unlist(blocks_list[seq_len(i)]))
  }

  group_values <- list(list(label = "", data = df))
  if (!is.null(group_var) && nzchar(group_var)) {
    group_levels <- unique(df[[group_var]])
    group_levels <- group_levels[!is.na(group_levels)]
    group_values <- lapply(group_levels, function(level) {
      list(label = as.character(level), data = df[df[[group_var]] == level, , drop = FALSE])
    })
  }

  coef_rows <- list()
  summary_rows <- list()
  comparison_rows <- list()
  diagnostics_rows <- list()
  model_tests_rows <- list()

  for (group_item in group_values) {
    group_label <- group_item$label
    data_group <- group_item$data

    term_vars <- unique(unlist(lapply(unlist(blocks_list), parse_term_vars)))
    vars_needed <- unique(c(dv, term_vars))
    vars_needed <- vars_needed[vars_needed != ""]

    idx <- get_complete_rows(data_group[, vars_needed, drop = FALSE])
    data_model <- data_group[idx, , drop = FALSE]

    data_model <- coerce_predictors(data_model, term_vars)
    data_model <- center_predictors(data_model, term_vars, center)

    if (family == "gaussian" && !is.numeric(data_model[[dv]])) {
      stop("Dependent variable must be numeric for gaussian regression.")
    }
    if (family == "poisson" && !is.numeric(data_model[[dv]])) {
      stop("Dependent variable must be numeric for poisson regression.")
    }
    if (family == "binomial" && !is.numeric(data_model[[dv]]) && !is.factor(data_model[[dv]])) {
      data_model[[dv]] <- as.factor(data_model[[dv]])
    }

    models <- list()
    summaries <- list()

    for (i in seq_along(cumulative_blocks)) {
      terms <- cumulative_blocks[[i]]
      terms <- terms[terms != ""]
      formula <- if (length(terms) == 0) {
        as.formula(paste(dv, "~ 1"))
      } else {
        as.formula(paste(dv, "~", paste(terms, collapse = " + ")))
      }
      fit <- safe_fit_model(formula, data_model, family, link)
      if (is.null(fit)) stop("Model could not be estimated")

      model_label <- paste0("Model ", i)
      boot_ci <- NULL
      if (isTRUE(bootstrap)) {
        term_names <- names(coef(fit))
        boot_ci <- compute_bootstrap_ci(data_model, formula, family, link, term_names, conf_level, bootstrap_samples)
      }
      coef_df <- extract_coefficients(fit, data_model, dv, family, conf_level, standardize, boot_ci)
      coef_df$model <- model_label
      coef_df$group <- group_label
      coef_rows[[length(coef_rows) + 1]] <- coef_df

      summary_df <- calc_model_summary(fit, data_model, dv, family, link)
      summary_df$model <- model_label
      summary_df$group <- group_label
      summary_rows[[length(summary_rows) + 1]] <- summary_df

      if (nrow(summary_df) > 0) {
        summary_row <- summary_df[1, ]
        if (family == "gaussian") {
          y <- data_model[[dv]]
          y_mean <- mean(y, na.rm = TRUE)
          ss_total <- sum((y - y_mean)^2, na.rm = TRUE)
          resid_vals <- resid(fit)
          ss_resid <- sum(resid_vals^2, na.rm = TRUE)
          ss_reg <- ss_total - ss_resid
          df_reg <- summary_row$model_df1
          df_resid <- summary_row$model_df2
          df_total <- if (!is.na(df_reg) && !is.na(df_resid)) df_reg + df_resid else NA_real_
          ms_reg <- if (!is.na(df_reg) && df_reg > 0) ss_reg / df_reg else NA_real_
          ms_resid <- if (!is.na(df_resid) && df_resid > 0) ss_resid / df_resid else NA_real_
          model_tests_rows[[length(model_tests_rows) + 1]] <- data.frame(
            model = model_label,
            group = group_label,
            source = "Regression",
            ss = ss_reg,
            df = df_reg,
            ms = ms_reg,
            f = summary_row$model_stat,
            p = summary_row$model_p,
            deviance = NA_real_,
            chisq = NA_real_,
            stringsAsFactors = FALSE
          )
          model_tests_rows[[length(model_tests_rows) + 1]] <- data.frame(
            model = model_label,
            group = group_label,
            source = "Residual",
            ss = ss_resid,
            df = df_resid,
            ms = ms_resid,
            f = NA_real_,
            p = NA_real_,
            deviance = NA_real_,
            chisq = NA_real_,
            stringsAsFactors = FALSE
          )
          model_tests_rows[[length(model_tests_rows) + 1]] <- data.frame(
            model = model_label,
            group = group_label,
            source = "Total",
            ss = ss_total,
            df = df_total,
            ms = NA_real_,
            f = NA_real_,
            p = NA_real_,
            deviance = NA_real_,
            chisq = NA_real_,
            stringsAsFactors = FALSE
          )
        } else {
          df_null <- if (!is.null(fit$df.null)) fit$df.null else NA_real_
          df_resid <- if (!is.null(fit$df.residual)) fit$df.residual else NA_real_
          model_tests_rows[[length(model_tests_rows) + 1]] <- data.frame(
            model = model_label,
            group = group_label,
            source = "Model",
            ss = NA_real_,
            df = summary_row$model_df1,
            ms = NA_real_,
            f = NA_real_,
            p = summary_row$model_p,
            deviance = NA_real_,
            chisq = summary_row$model_stat,
            stringsAsFactors = FALSE
          )
          model_tests_rows[[length(model_tests_rows) + 1]] <- data.frame(
            model = model_label,
            group = group_label,
            source = "Residual",
            ss = NA_real_,
            df = df_resid,
            ms = NA_real_,
            f = NA_real_,
            p = NA_real_,
            deviance = summary_row$deviance,
            chisq = NA_real_,
            stringsAsFactors = FALSE
          )
          model_tests_rows[[length(model_tests_rows) + 1]] <- data.frame(
            model = model_label,
            group = group_label,
            source = "Null",
            ss = NA_real_,
            df = df_null,
            ms = NA_real_,
            f = NA_real_,
            p = NA_real_,
            deviance = summary_row$null_deviance,
            chisq = NA_real_,
            stringsAsFactors = FALSE
          )
        }
      }

      if (family == "gaussian") {
        shapiro <- tryCatch(shapiro.test(resid(fit)), error = function(e) NULL)
        diagnostics_rows[[length(diagnostics_rows) + 1]] <- data.frame(
          model = model_label,
          group = group_label,
          test = "Shapiro-Wilk",
          statistic = if (!is.null(shapiro)) unname(shapiro$statistic) else NA_real_,
          p = if (!is.null(shapiro)) shapiro$p.value else NA_real_,
          n = length(resid(fit)),
          stringsAsFactors = FALSE
        )
      }

      models[[length(models) + 1]] <- fit
      summaries[[length(summaries) + 1]] <- summary_df
    }

    if (length(models) > 1) {
      for (i in 2:length(models)) {
        prev <- models[[i - 1]]
        curr <- models[[i]]
        prev_summary <- summaries[[i - 1]]
        curr_summary <- summaries[[i]]
        model_label <- paste0("Model ", i)

        if (family == "gaussian") {
          comp <- anova(prev, curr)
          comp_row <- comp[2, ]
          delta_f2 <- NA_real_
          if (!is.na(curr_summary$r2) && curr_summary$r2 < 1) {
            delta_f2 <- (curr_summary$r2 - prev_summary$r2) / (1 - curr_summary$r2)
          }
          comparison_rows[[length(comparison_rows) + 1]] <- data.frame(
            model = model_label,
            group = group_label,
            delta_r2 = curr_summary$r2 - prev_summary$r2,
            delta_f2 = delta_f2,
            delta_f = if (!is.null(comp_row$F)) comp_row$F else NA_real_,
            df1 = if (!is.null(comp_row$Df)) comp_row$Df else NA_real_,
            df2 = if (!is.null(comp_row$Res.Df)) comp_row$Res.Df else NA_real_,
            p = if (!is.null(comp_row$`Pr(>F)`)) comp_row$`Pr(>F)` else NA_real_,
            delta_deviance = NA_real_,
            delta_chisq = NA_real_,
            stringsAsFactors = FALSE
          )
        } else {
          comp <- anova(prev, curr, test = "Chisq")
          comp_row <- comp[2, ]
          delta_dev <- if (!is.null(comp_row$Deviance)) comp_row$Deviance else NA_real_
          df1 <- if (!is.null(comp_row$Df)) comp_row$Df else NA_real_
          comparison_rows[[length(comparison_rows) + 1]] <- data.frame(
            model = model_label,
            group = group_label,
            delta_r2 = curr_summary$pseudo_r2 - prev_summary$pseudo_r2,
            delta_f2 = NA_real_,
            delta_f = NA_real_,
            df1 = df1,
            df2 = NA_real_,
            p = if (!is.null(comp_row$`Pr(>Chi)`)) comp_row$`Pr(>Chi)` else NA_real_,
            delta_deviance = delta_dev,
            delta_chisq = delta_dev,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  list(
    coefficients_df = if (length(coef_rows) > 0) do.call(rbind, coef_rows) else data.frame(),
    summary_df = if (length(summary_rows) > 0) do.call(rbind, summary_rows) else data.frame(),
    comparisons_df = if (length(comparison_rows) > 0) do.call(rbind, comparison_rows) else data.frame(),
    diagnostics_df = if (length(diagnostics_rows) > 0) do.call(rbind, diagnostics_rows) else data.frame(),
    model_tests_df = if (length(model_tests_rows) > 0) do.call(rbind, model_tests_rows) else data.frame(),
    blocks = blocks_list,
    interactions = interactions_list,
    family = family,
    link = link,
    center = center,
    standardize = standardize,
    bootstrap = bootstrap,
    bootstrap_samples = if (isTRUE(bootstrap)) bootstrap_samples else NA_real_
  )
}

find_coef_row <- function(df, model, term, group = "") {
  rows <- df[df$model == model & df$term == term & df$group == group, , drop = FALSE]
  if (nrow(rows) == 0) stop("Coefficient row not found for ", term)
  rows[1, , drop = FALSE]
}

find_summary_row <- function(df, model, group = "") {
  rows <- df[df$model == model & df$group == group, , drop = FALSE]
  if (nrow(rows) == 0) stop("Summary row not found for ", model)
  rows[1, , drop = FALSE]
}

find_comparison_row <- function(df, model, group = "") {
  rows <- df[df$model == model & df$group == group, , drop = FALSE]
  if (nrow(rows) == 0) stop("Comparison row not found for ", model)
  rows[1, , drop = FALSE]
}

find_diagnostic_row <- function(df, model, test = "Shapiro-Wilk", group = "") {
  rows <- df[df$model == model & df$test == test & df$group == group, , drop = FALSE]
  if (nrow(rows) == 0) stop("Diagnostics row not found for ", model)
  rows[1, , drop = FALSE]
}

find_model_test_row <- function(df, model, source, group = "") {
  rows <- df[df$model == model & df$source == source & df$group == group, , drop = FALSE]
  if (nrow(rows) == 0) stop("Model test row not found for ", model, " ", source)
  rows[1, , drop = FALSE]
}

collapse_blocks <- function(blocks) {
  if (is.null(blocks) || length(blocks) == 0) return("")
  parts <- vapply(blocks, function(block) paste(block, collapse = ","), character(1))
  paste(parts, collapse = ";")
}

add_case_meta <- function(df_row, case_id, dv, ivs, blocks, interactions, group, group_var, family, link,
                          center, standardize, bootstrap, bootstrap_samples) {
  if ("group" %in% names(df_row)) {
    df_row$group <- NULL
  }
  data.frame(
    case_id = case_id,
    dv = dv,
    ivs = ivs,
    blocks = blocks,
    interactions = interactions,
    group = group,
    group_var = group_var,
    family = family,
    link = link,
    center = center,
    standardize = standardize,
    bootstrap = bootstrap,
    bootstrap_samples = bootstrap_samples,
    df_row,
    stringsAsFactors = FALSE
  )
}

# Inputs

data_path <- trim_arg(get_arg("--data", file.path("tests", "data", "golden_dataset.csv")))
out_coef <- trim_arg(get_arg("--out-coef", file.path("tests", "values", "regression_coefficients_golden.csv")))
out_summary <- trim_arg(get_arg("--out-summary", file.path("tests", "values", "regression_summary_golden.csv")))
out_comparisons <- trim_arg(get_arg("--out-comparisons", file.path("tests", "values", "regression_comparisons_golden.csv")))
out_diagnostics <- trim_arg(get_arg("--out-diagnostics", file.path("tests", "values", "regression_diagnostics_golden.csv")))
out_model_tests <- trim_arg(get_arg("--out-model-tests", file.path("tests", "values", "regression_model_tests_golden.csv")))

if (is.null(data_path) || !file.exists(data_path)) stop("Missing --data path.")
if (is.null(out_coef) || is.null(out_summary) || is.null(out_comparisons) || is.null(out_diagnostics) || is.null(out_model_tests)) {
  stop("Missing output path(s).")
}

options(scipen = 999, digits = 15)

raw_df <- read.csv(data_path, stringsAsFactors = FALSE)

res_ols <- run_regression_case(
  raw_df,
  dv = "outcome_reg",
  ivs = c("x1", "x2", "x3"),
  family = "gaussian"
)
res_blocks <- run_regression_case(
  raw_df,
  dv = "outcome_reg",
  blocks = "x1,x2;x3",
  family = "gaussian"
)
res_interaction <- run_regression_case(
  raw_df,
  dv = "outcome_reg",
  ivs = c("x1"),
  interactions = c("x1:mediator"),
  center = "mean",
  family = "gaussian"
)
res_standardize <- run_regression_case(
  raw_df,
  dv = "outcome_reg",
  ivs = c("x1", "x2", "x3"),
  standardize = "predictors",
  family = "gaussian"
)
res_boot <- run_regression_case(
  raw_df,
  dv = "outcome_reg",
  ivs = c("x1", "x2"),
  bootstrap = TRUE,
  bootstrap_samples = 50,
  seed = 123,
  family = "gaussian"
)
res_group <- run_regression_case(
  raw_df,
  dv = "outcome_reg",
  ivs = c("x1", "x2"),
  group_var = "site",
  family = "gaussian"
)
res_logistic <- run_regression_case(
  raw_df,
  dv = "binary_outcome",
  ivs = c("x1", "x2"),
  family = "binomial"
)
res_logistic_inter <- run_regression_case(
  raw_df,
  dv = "binary_outcome",
  ivs = c("x1"),
  interactions = c("x1:mediator"),
  family = "binomial"
)
res_poisson <- run_regression_case(
  raw_df,
  dv = "count_outcome",
  blocks = "x1;x2",
  family = "poisson"
)

coef_rows <- list()
coef_rows[[length(coef_rows) + 1]] <- add_case_meta(
  find_coef_row(res_ols$coefficients_df, "Model 1", "x1"),
  "ols_basic_x1",
  dv = "outcome_reg",
  ivs = "x1,x2,x3",
  blocks = collapse_blocks(res_ols$blocks),
  interactions = "",
  group = "",
  group_var = "",
  family = res_ols$family,
  link = res_ols$link,
  center = res_ols$center,
  standardize = res_ols$standardize,
  bootstrap = res_ols$bootstrap,
  bootstrap_samples = res_ols$bootstrap_samples
)
coef_rows[[length(coef_rows) + 1]] <- add_case_meta(
  find_coef_row(res_standardize$coefficients_df, "Model 1", "x1"),
  "ols_standardized_x1",
  dv = "outcome_reg",
  ivs = "x1,x2,x3",
  blocks = collapse_blocks(res_standardize$blocks),
  interactions = "",
  group = "",
  group_var = "",
  family = res_standardize$family,
  link = res_standardize$link,
  center = res_standardize$center,
  standardize = res_standardize$standardize,
  bootstrap = res_standardize$bootstrap,
  bootstrap_samples = res_standardize$bootstrap_samples
)
coef_rows[[length(coef_rows) + 1]] <- add_case_meta(
  find_coef_row(res_boot$coefficients_df, "Model 1", "x1"),
  "ols_bootstrap_x1",
  dv = "outcome_reg",
  ivs = "x1,x2",
  blocks = collapse_blocks(res_boot$blocks),
  interactions = "",
  group = "",
  group_var = "",
  family = res_boot$family,
  link = res_boot$link,
  center = res_boot$center,
  standardize = res_boot$standardize,
  bootstrap = res_boot$bootstrap,
  bootstrap_samples = res_boot$bootstrap_samples
)
coef_rows[[length(coef_rows) + 1]] <- add_case_meta(
  find_coef_row(res_interaction$coefficients_df, "Model 2", "x1:mediator"),
  "ols_interaction_x1_mediator",
  dv = "outcome_reg",
  ivs = "x1",
  blocks = collapse_blocks(res_interaction$blocks),
  interactions = "x1:mediator",
  group = "",
  group_var = "",
  family = res_interaction$family,
  link = res_interaction$link,
  center = res_interaction$center,
  standardize = res_interaction$standardize,
  bootstrap = res_interaction$bootstrap,
  bootstrap_samples = res_interaction$bootstrap_samples
)
coef_rows[[length(coef_rows) + 1]] <- add_case_meta(
  find_coef_row(res_group$coefficients_df, "Model 1", "x1", group = "S1"),
  "ols_grouped_s1_x1",
  dv = "outcome_reg",
  ivs = "x1,x2",
  blocks = collapse_blocks(res_group$blocks),
  interactions = "",
  group = "S1",
  group_var = "site",
  family = res_group$family,
  link = res_group$link,
  center = res_group$center,
  standardize = res_group$standardize,
  bootstrap = res_group$bootstrap,
  bootstrap_samples = res_group$bootstrap_samples
)
coef_rows[[length(coef_rows) + 1]] <- add_case_meta(
  find_coef_row(res_logistic$coefficients_df, "Model 1", "x1"),
  "logistic_x1",
  dv = "binary_outcome",
  ivs = "x1,x2",
  blocks = collapse_blocks(res_logistic$blocks),
  interactions = "",
  group = "",
  group_var = "",
  family = res_logistic$family,
  link = res_logistic$link,
  center = res_logistic$center,
  standardize = res_logistic$standardize,
  bootstrap = res_logistic$bootstrap,
  bootstrap_samples = res_logistic$bootstrap_samples
)
coef_rows[[length(coef_rows) + 1]] <- add_case_meta(
  find_coef_row(res_poisson$coefficients_df, "Model 1", "x1"),
  "poisson_x1",
  dv = "count_outcome",
  ivs = "",
  blocks = collapse_blocks(res_poisson$blocks),
  interactions = "",
  group = "",
  group_var = "",
  family = res_poisson$family,
  link = res_poisson$link,
  center = res_poisson$center,
  standardize = res_poisson$standardize,
  bootstrap = res_poisson$bootstrap,
  bootstrap_samples = res_poisson$bootstrap_samples
)

coef_golden <- do.call(rbind, coef_rows)
write.csv(coef_golden, out_coef, row.names = FALSE)

summary_rows <- list()
summary_rows[[length(summary_rows) + 1]] <- add_case_meta(
  find_summary_row(res_ols$summary_df, "Model 1"),
  "ols_basic_model1",
  dv = "outcome_reg",
  ivs = "x1,x2,x3",
  blocks = collapse_blocks(res_ols$blocks),
  interactions = "",
  group = "",
  group_var = "",
  family = res_ols$family,
  link = res_ols$link,
  center = res_ols$center,
  standardize = res_ols$standardize,
  bootstrap = res_ols$bootstrap,
  bootstrap_samples = res_ols$bootstrap_samples
)
summary_rows[[length(summary_rows) + 1]] <- add_case_meta(
  find_summary_row(res_blocks$summary_df, "Model 2"),
  "ols_blocks_model2",
  dv = "outcome_reg",
  ivs = "",
  blocks = collapse_blocks(res_blocks$blocks),
  interactions = "",
  group = "",
  group_var = "",
  family = res_blocks$family,
  link = res_blocks$link,
  center = res_blocks$center,
  standardize = res_blocks$standardize,
  bootstrap = res_blocks$bootstrap,
  bootstrap_samples = res_blocks$bootstrap_samples
)
summary_rows[[length(summary_rows) + 1]] <- add_case_meta(
  find_summary_row(res_group$summary_df, "Model 1", group = "S1"),
  "ols_grouped_s1",
  dv = "outcome_reg",
  ivs = "x1,x2",
  blocks = collapse_blocks(res_group$blocks),
  interactions = "",
  group = "S1",
  group_var = "site",
  family = res_group$family,
  link = res_group$link,
  center = res_group$center,
  standardize = res_group$standardize,
  bootstrap = res_group$bootstrap,
  bootstrap_samples = res_group$bootstrap_samples
)
summary_rows[[length(summary_rows) + 1]] <- add_case_meta(
  find_summary_row(res_logistic$summary_df, "Model 1"),
  "logistic_model1",
  dv = "binary_outcome",
  ivs = "x1,x2",
  blocks = collapse_blocks(res_logistic$blocks),
  interactions = "",
  group = "",
  group_var = "",
  family = res_logistic$family,
  link = res_logistic$link,
  center = res_logistic$center,
  standardize = res_logistic$standardize,
  bootstrap = res_logistic$bootstrap,
  bootstrap_samples = res_logistic$bootstrap_samples
)
summary_rows[[length(summary_rows) + 1]] <- add_case_meta(
  find_summary_row(res_poisson$summary_df, "Model 2"),
  "poisson_model2",
  dv = "count_outcome",
  ivs = "",
  blocks = collapse_blocks(res_poisson$blocks),
  interactions = "",
  group = "",
  group_var = "",
  family = res_poisson$family,
  link = res_poisson$link,
  center = res_poisson$center,
  standardize = res_poisson$standardize,
  bootstrap = res_poisson$bootstrap,
  bootstrap_samples = res_poisson$bootstrap_samples
)

summary_golden <- do.call(rbind, summary_rows)
write.csv(summary_golden, out_summary, row.names = FALSE)

comparison_rows <- list()
comparison_rows[[length(comparison_rows) + 1]] <- add_case_meta(
  find_comparison_row(res_blocks$comparisons_df, "Model 2"),
  "ols_blocks_model2",
  dv = "outcome_reg",
  ivs = "",
  blocks = collapse_blocks(res_blocks$blocks),
  interactions = "",
  group = "",
  group_var = "",
  family = res_blocks$family,
  link = res_blocks$link,
  center = res_blocks$center,
  standardize = res_blocks$standardize,
  bootstrap = res_blocks$bootstrap,
  bootstrap_samples = res_blocks$bootstrap_samples
)
comparison_rows[[length(comparison_rows) + 1]] <- add_case_meta(
  find_comparison_row(res_logistic_inter$comparisons_df, "Model 2"),
  "logistic_interaction_model2",
  dv = "binary_outcome",
  ivs = "x1",
  blocks = collapse_blocks(res_logistic_inter$blocks),
  interactions = "x1:mediator",
  group = "",
  group_var = "",
  family = res_logistic_inter$family,
  link = res_logistic_inter$link,
  center = res_logistic_inter$center,
  standardize = res_logistic_inter$standardize,
  bootstrap = res_logistic_inter$bootstrap,
  bootstrap_samples = res_logistic_inter$bootstrap_samples
)
comparison_rows[[length(comparison_rows) + 1]] <- add_case_meta(
  find_comparison_row(res_poisson$comparisons_df, "Model 2"),
  "poisson_blocks_model2",
  dv = "count_outcome",
  ivs = "",
  blocks = collapse_blocks(res_poisson$blocks),
  interactions = "",
  group = "",
  group_var = "",
  family = res_poisson$family,
  link = res_poisson$link,
  center = res_poisson$center,
  standardize = res_poisson$standardize,
  bootstrap = res_poisson$bootstrap,
  bootstrap_samples = res_poisson$bootstrap_samples
)

comparison_golden <- do.call(rbind, comparison_rows)
write.csv(comparison_golden, out_comparisons, row.names = FALSE)

diag_row <- find_diagnostic_row(res_ols$diagnostics_df, "Model 1")

diagnostics_golden <- add_case_meta(
  diag_row,
  "ols_basic_shapiro",
  dv = "outcome_reg",
  ivs = "x1,x2,x3",
  blocks = collapse_blocks(res_ols$blocks),
  interactions = "",
  group = "",
  group_var = "",
  family = res_ols$family,
  link = res_ols$link,
  center = res_ols$center,
  standardize = res_ols$standardize,
  bootstrap = res_ols$bootstrap,
  bootstrap_samples = res_ols$bootstrap_samples
)
write.csv(diagnostics_golden, out_diagnostics, row.names = FALSE)

model_test_rows <- list()
model_test_rows[[length(model_test_rows) + 1]] <- add_case_meta(
  find_model_test_row(res_ols$model_tests_df, "Model 1", "Regression"),
  "ols_basic_regression",
  dv = "outcome_reg",
  ivs = "x1,x2,x3",
  blocks = collapse_blocks(res_ols$blocks),
  interactions = "",
  group = "",
  group_var = "",
  family = res_ols$family,
  link = res_ols$link,
  center = res_ols$center,
  standardize = res_ols$standardize,
  bootstrap = res_ols$bootstrap,
  bootstrap_samples = res_ols$bootstrap_samples
)
model_test_rows[[length(model_test_rows) + 1]] <- add_case_meta(
  find_model_test_row(res_logistic$model_tests_df, "Model 1", "Model"),
  "logistic_model",
  dv = "binary_outcome",
  ivs = "x1,x2",
  blocks = collapse_blocks(res_logistic$blocks),
  interactions = "",
  group = "",
  group_var = "",
  family = res_logistic$family,
  link = res_logistic$link,
  center = res_logistic$center,
  standardize = res_logistic$standardize,
  bootstrap = res_logistic$bootstrap,
  bootstrap_samples = res_logistic$bootstrap_samples
)
model_test_rows[[length(model_test_rows) + 1]] <- add_case_meta(
  find_model_test_row(res_poisson$model_tests_df, "Model 1", "Model"),
  "poisson_model",
  dv = "count_outcome",
  ivs = "",
  blocks = collapse_blocks(res_poisson$blocks),
  interactions = "",
  group = "",
  group_var = "",
  family = res_poisson$family,
  link = res_poisson$link,
  center = res_poisson$center,
  standardize = res_poisson$standardize,
  bootstrap = res_poisson$bootstrap,
  bootstrap_samples = res_poisson$bootstrap_samples
)

model_tests_golden <- do.call(rbind, model_test_rows)
write.csv(model_tests_golden, out_model_tests, row.names = FALSE)

cat("Wrote regression golden values to", out_coef, "\n")
