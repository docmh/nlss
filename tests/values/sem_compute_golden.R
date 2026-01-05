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

require_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Required package '%s' is not installed.", pkg))
  }
}

data_path <- trim_arg(get_arg("--data", file.path("tests", "data", "golden_dataset.csv")))
out_fit <- trim_arg(get_arg("--out-fit", file.path("tests", "values", "sem_fit_golden.csv")))
out_params <- trim_arg(get_arg("--out-params", file.path("tests", "values", "sem_params_golden.csv")))
out_r2 <- trim_arg(get_arg("--out-r2", file.path("tests", "values", "sem_r2_golden.csv")))
out_invariance <- trim_arg(get_arg("--out-invariance", file.path("tests", "values", "sem_invariance_golden.csv")))
out_modindices <- trim_arg(get_arg("--out-modindices", file.path("tests", "values", "sem_modindices_golden.csv")))

if (is.null(data_path) || !file.exists(data_path)) {
  stop("Missing --data path.")
}
if (is.null(out_fit) || is.null(out_params) || is.null(out_r2) ||
    is.null(out_invariance) || is.null(out_modindices)) {
  stop("Missing output path(s).")
}

options(scipen = 999, digits = 15)
require_pkg("lavaan")

df <- read.csv(data_path, stringsAsFactors = FALSE)

fit_indices <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr")

collect_fit_values <- function(fit, indices) {
  values <- list()
  if (length(indices) == 0) return(values)
  fit_vals <- tryCatch(lavaan::fitMeasures(fit, indices), error = function(e) NULL)
  if (is.null(fit_vals)) return(values)
  if (is.numeric(fit_vals)) {
    values <- as.list(fit_vals)
  } else if (is.list(fit_vals)) {
    values <- fit_vals
  }
  values
}

build_param_df <- function(fit, std, conf_level, ci_type, group_labels) {
  boot_ci_type <- NULL
  if (ci_type == "bootstrap") boot_ci_type <- "perc"
  if (ci_type == "bca") boot_ci_type <- "bca.simple"
  pe <- lavaan::parameterEstimates(
    fit,
    standardized = (std != "none"),
    ci = TRUE,
    level = conf_level,
    boot.ci.type = boot_ci_type
  )

  keep_ops <- c("=~", "~", "~~", ":=")
  pe <- pe[pe$op %in% keep_ops, , drop = FALSE]
  if (nrow(pe) == 0) return(data.frame())
  pe <- pe[!(pe$op == "~~" & pe$lhs == pe$rhs), , drop = FALSE]

  std_col <- NULL
  if (std == "std.all" && "std.all" %in% names(pe)) std_col <- "std.all"
  if (std == "std.lv" && "std.lv" %in% names(pe)) std_col <- "std.lv"

  group_vals <- rep("", nrow(pe))
  if (!is.null(pe$group) && length(group_labels) > 0) {
    group_vals <- vapply(pe$group, function(idx) {
      if (is.na(idx)) return("")
      label <- group_labels[as.integer(idx)]
      if (is.null(label) || !nzchar(label)) return("")
      as.character(label)
    }, character(1))
  }

  label_vals <- if ("label" %in% names(pe)) {
    ifelse(is.na(pe$label), "", pe$label)
  } else {
    rep("", nrow(pe))
  }

  data.frame(
    group = group_vals,
    path = paste(pe$lhs, pe$op, pe$rhs),
    label = label_vals,
    est = pe$est,
    se = pe$se,
    z = pe$z,
    p = pe$pvalue,
    ci_low = pe$ci.lower,
    ci_high = pe$ci.upper,
    std = if (!is.null(std_col)) pe[[std_col]] else NA_real_,
    op = pe$op,
    stringsAsFactors = FALSE
  )
}

flatten_r2_values <- function(r2_values) {
  if (is.null(r2_values) || length(r2_values) == 0) return(list())
  if (is.numeric(r2_values)) {
    out <- as.list(r2_values)
    if (is.null(names(out))) names(out) <- rep("", length(out))
    return(out)
  }
  if (is.list(r2_values)) {
    out <- list()
    group_names <- names(r2_values)
    for (i in seq_along(r2_values)) {
      values <- r2_values[[i]]
      if (is.null(values)) next
      group_label <- if (!is.null(group_names) && nzchar(group_names[i])) group_names[i] else paste0("Group ", i)
      if (!is.numeric(values)) next
      val_names <- names(values)
      if (is.null(val_names) || all(!nzchar(val_names))) {
        for (j in seq_along(values)) {
          label <- paste0(group_label, ": var", j)
          out[[label]] <- values[[j]]
        }
      } else {
        for (j in seq_along(values)) {
          label <- paste0(group_label, ": ", val_names[j])
          out[[label]] <- values[[j]]
        }
      }
    }
    return(out)
  }
  list()
}

build_r2_df <- function(r2_values) {
  flat <- flatten_r2_values(r2_values)
  if (length(flat) == 0) return(data.frame())
  labels <- names(flat)
  if (is.null(labels)) labels <- rep("", length(flat))
  data.frame(
    label = labels,
    r2 = as.numeric(flat),
    stringsAsFactors = FALSE
  )
}

build_invariance_summary <- function(steps, fits, fit_indices) {
  rows <- list()
  prev <- NULL
  get_val <- function(values, key) {
    val <- values[[key]]
    if (is.null(val) || length(val) == 0) return(NA_real_)
    as.numeric(val)
  }
  for (i in seq_along(steps)) {
    fit <- fits[[i]]
    fit_vals <- collect_fit_values(fit, fit_indices)
    row <- list(
      step = steps[[i]]$label,
      group_equal = steps[[i]]$constraints,
      chisq = get_val(fit_vals, "chisq"),
      df = get_val(fit_vals, "df"),
      p = get_val(fit_vals, "pvalue"),
      cfi = get_val(fit_vals, "cfi"),
      tli = get_val(fit_vals, "tli"),
      rmsea = get_val(fit_vals, "rmsea"),
      srmr = get_val(fit_vals, "srmr"),
      delta_cfi = NA_real_,
      delta_rmsea = NA_real_
    )
    if (!is.null(prev)) {
      if (!is.null(row$cfi) && !is.null(prev$cfi)) row$delta_cfi <- row$cfi - prev$cfi
      if (!is.null(row$rmsea) && !is.null(prev$rmsea)) row$delta_rmsea <- row$rmsea - prev$rmsea
    }
    prev <- row
    rows[[length(rows) + 1]] <- row
  }
  if (length(rows) == 0) return(data.frame())
  do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
}

find_param_row <- function(param_df, path, op = "", label = "", group = "") {
  if (nrow(param_df) == 0) return(NULL)
  for (i in seq_len(nrow(param_df))) {
    row <- param_df[i, ]
    if (nzchar(path) && row$path != path) next
    if (nzchar(op) && row$op != op) next
    if (nzchar(label) && row$label != label) next
    if (nzchar(group) && row$group != group) next
    return(row)
  }
  NULL
}

find_r2_row <- function(r2_df, label) {
  if (nrow(r2_df) == 0) return(NULL)
  for (i in seq_len(nrow(r2_df))) {
    row <- r2_df[i, ]
    if (nzchar(label) && row$label != label) next
    return(row)
  }
  NULL
}

fit_rows <- list()
param_rows <- list()
r2_rows <- list()
invariance_rows <- list()
mod_rows <- list()

# Path model
model_path <- "outcome_reg ~ x1 + x2"
fit_path <- lavaan::sem(
  model_path,
  data = df,
  estimator = "ML",
  missing = "listwise",
  se = "standard"
)
fit_vals <- collect_fit_values(fit_path, fit_indices)
fit_rows[[length(fit_rows) + 1]] <- data.frame(
  case_id = "path_basic_fit",
  analysis = "path",
  chisq = fit_vals$chisq,
  df = fit_vals$df,
  pvalue = fit_vals$pvalue,
  cfi = fit_vals$cfi,
  tli = fit_vals$tli,
  rmsea = fit_vals$rmsea,
  srmr = fit_vals$srmr,
  stringsAsFactors = FALSE
)

param_df <- build_param_df(fit_path, std = "std.all", conf_level = 0.95, ci_type = "standard", group_labels = character(0))
row_x1 <- find_param_row(param_df, "outcome_reg ~ x1", op = "~")
row_x2 <- find_param_row(param_df, "outcome_reg ~ x2", op = "~")
if (is.null(row_x1) || is.null(row_x2)) stop("Missing path parameter rows.")
param_rows[[length(param_rows) + 1]] <- data.frame(
  case_id = "path_basic_x1",
  analysis = "path",
  group = row_x1$group,
  path = row_x1$path,
  label = row_x1$label,
  op = row_x1$op,
  est = row_x1$est,
  se = row_x1$se,
  z = row_x1$z,
  p = row_x1$p,
  ci_low = row_x1$ci_low,
  ci_high = row_x1$ci_high,
  std = row_x1$std,
  stringsAsFactors = FALSE
)
param_rows[[length(param_rows) + 1]] <- data.frame(
  case_id = "path_basic_x2",
  analysis = "path",
  group = row_x2$group,
  path = row_x2$path,
  label = row_x2$label,
  op = row_x2$op,
  est = row_x2$est,
  se = row_x2$se,
  z = row_x2$z,
  p = row_x2$p,
  ci_low = row_x2$ci_low,
  ci_high = row_x2$ci_high,
  std = row_x2$std,
  stringsAsFactors = FALSE
)

r2_df <- build_r2_df(tryCatch(lavaan::inspect(fit_path, "r2"), error = function(e) NULL))
row_r2 <- find_r2_row(r2_df, "outcome_reg")
if (is.null(row_r2)) stop("Missing path R2 row.")
r2_rows[[length(r2_rows) + 1]] <- data.frame(
  case_id = "path_basic_r2_outcome_reg",
  analysis = "path",
  label = row_r2$label,
  r2 = row_r2$r2,
  stringsAsFactors = FALSE
)

# CFA model with two factors (also used for modindices)
model_cfa <- paste(
  "F1 =~ f1_1 + f1_2 + f1_3_rev + f1_4",
  "F2 =~ f2_1 + f2_2 + f2_3 + f2_4_rev",
  sep = "\n"
)
fit_cfa <- lavaan::cfa(
  model_cfa,
  data = df,
  estimator = "ML",
  missing = "listwise",
  se = "standard"
)
fit_vals <- collect_fit_values(fit_cfa, fit_indices)
fit_rows[[length(fit_rows) + 1]] <- data.frame(
  case_id = "cfa_basic_fit",
  analysis = "cfa",
  chisq = fit_vals$chisq,
  df = fit_vals$df,
  pvalue = fit_vals$pvalue,
  cfi = fit_vals$cfi,
  tli = fit_vals$tli,
  rmsea = fit_vals$rmsea,
  srmr = fit_vals$srmr,
  stringsAsFactors = FALSE
)

param_df <- build_param_df(fit_cfa, std = "std.all", conf_level = 0.95, ci_type = "standard", group_labels = character(0))
row_loading <- find_param_row(param_df, "F1 =~ f1_1", op = "=~")
row_cov <- find_param_row(param_df, "F1 ~~ F2", op = "~~")
if (is.null(row_loading) || is.null(row_cov)) stop("Missing CFA parameter rows.")
param_rows[[length(param_rows) + 1]] <- data.frame(
  case_id = "cfa_basic_loading_f1_1",
  analysis = "cfa",
  group = row_loading$group,
  path = row_loading$path,
  label = row_loading$label,
  op = row_loading$op,
  est = row_loading$est,
  se = row_loading$se,
  z = row_loading$z,
  p = row_loading$p,
  ci_low = row_loading$ci_low,
  ci_high = row_loading$ci_high,
  std = row_loading$std,
  stringsAsFactors = FALSE
)
param_rows[[length(param_rows) + 1]] <- data.frame(
  case_id = "cfa_basic_cov_f1_f2",
  analysis = "cfa",
  group = row_cov$group,
  path = row_cov$path,
  label = row_cov$label,
  op = row_cov$op,
  est = row_cov$est,
  se = row_cov$se,
  z = row_cov$z,
  p = row_cov$p,
  ci_low = row_cov$ci_low,
  ci_high = row_cov$ci_high,
  std = row_cov$std,
  stringsAsFactors = FALSE
)

modindices_df <- lavaan::modindices(fit_cfa, sort. = TRUE, minimum.value = 1)
if (nrow(modindices_df) == 0) stop("No modindices rows found.")
mod_row <- modindices_df[1, ]
mod_rows[[length(mod_rows) + 1]] <- data.frame(
  case_id = "cfa_modindices_top1",
  analysis = "cfa",
  lhs = as.character(mod_row$lhs),
  op = as.character(mod_row$op),
  rhs = as.character(mod_row$rhs),
  mi = as.numeric(mod_row$mi),
  epc = as.numeric(mod_row$epc),
  `sepc.all` = as.numeric(mod_row$`sepc.all`),
  stringsAsFactors = FALSE
)

# CFA grouped
df_group <- df
df_group$group2 <- as.factor(df_group$group2)
fit_cfa_group <- lavaan::cfa(
  model_cfa,
  data = df_group,
  estimator = "ML",
  missing = "listwise",
  se = "standard",
  group = "group2"
)
group_labels <- levels(df_group$group2)
param_df <- build_param_df(fit_cfa_group, std = "std.all", conf_level = 0.95, ci_type = "standard", group_labels = group_labels)
row_group <- find_param_row(param_df, "F1 =~ f1_1", op = "=~", group = "control")
if (is.null(row_group)) {
  row_group <- find_param_row(param_df, "F1 =~ f1_1", op = "=~")
}
if (is.null(row_group)) stop("Missing grouped CFA parameter row.")
param_rows[[length(param_rows) + 1]] <- data.frame(
  case_id = "cfa_group_control_loading_f1_1",
  analysis = "cfa",
  group = row_group$group,
  path = row_group$path,
  label = row_group$label,
  op = row_group$op,
  est = row_group$est,
  se = row_group$se,
  z = row_group$z,
  p = row_group$p,
  ci_low = row_group$ci_low,
  ci_high = row_group$ci_high,
  std = row_group$std,
  stringsAsFactors = FALSE
)

# Mediation model (single mediator)
model_mediation <- paste(
  "mediator ~ a1*x1",
  "outcome_reg ~ c_prime*x1 + b1*mediator",
  "indirect_mediator := a1*b1",
  "total_indirect := indirect_mediator",
  "total := c_prime + total_indirect",
  sep = "\n"
)
fit_med <- lavaan::sem(
  model_mediation,
  data = df,
  estimator = "ML",
  missing = "listwise",
  se = "standard"
)
fit_vals <- collect_fit_values(fit_med, fit_indices)
fit_rows[[length(fit_rows) + 1]] <- data.frame(
  case_id = "mediation_basic_fit",
  analysis = "mediation",
  chisq = fit_vals$chisq,
  df = fit_vals$df,
  pvalue = fit_vals$pvalue,
  cfi = fit_vals$cfi,
  tli = fit_vals$tli,
  rmsea = fit_vals$rmsea,
  srmr = fit_vals$srmr,
  stringsAsFactors = FALSE
)

param_df <- build_param_df(fit_med, std = "std.all", conf_level = 0.95, ci_type = "standard", group_labels = character(0))
row_indirect <- find_param_row(param_df, "indirect_mediator := a1*b1", op = ":=")
if (is.null(row_indirect)) stop("Missing mediation indirect row.")
param_rows[[length(param_rows) + 1]] <- data.frame(
  case_id = "mediation_indirect_mediator",
  analysis = "mediation",
  group = row_indirect$group,
  path = row_indirect$path,
  label = row_indirect$label,
  op = row_indirect$op,
  est = row_indirect$est,
  se = row_indirect$se,
  z = row_indirect$z,
  p = row_indirect$p,
  ci_low = row_indirect$ci_low,
  ci_high = row_indirect$ci_high,
  std = row_indirect$std,
  stringsAsFactors = FALSE
)

r2_df <- build_r2_df(tryCatch(lavaan::inspect(fit_med, "r2"), error = function(e) NULL))
row_r2_med <- find_r2_row(r2_df, "mediator")
if (is.null(row_r2_med)) stop("Missing mediation R2 row.")
r2_rows[[length(r2_rows) + 1]] <- data.frame(
  case_id = "mediation_r2_mediator",
  analysis = "mediation",
  label = row_r2_med$label,
  r2 = row_r2_med$r2,
  stringsAsFactors = FALSE
)

# Invariance (configural + metric)
df_inv <- df
df_inv$group2 <- as.factor(df_inv$group2)
model_inv <- "F1 =~ f1_1 + f1_2 + f1_3_rev + f1_4"
steps <- list(
  list(label = "configural", constraints = "", group_equal = character(0)),
  list(label = "metric", constraints = "loadings", group_equal = c("loadings"))
)
fits <- list()
for (step in steps) {
  fit <- lavaan::cfa(
    model_inv,
    data = df_inv,
    estimator = "ML",
    missing = "listwise",
    se = "standard",
    group = "group2",
    group.equal = step$group_equal
  )
  fits[[length(fits) + 1]] <- fit
}
summary_df <- build_invariance_summary(steps, fits, fit_indices)
if (nrow(summary_df) == 0) stop("Missing invariance summary.")
summary_df$case_id <- ifelse(summary_df$step == "configural", "invariance_configural", "invariance_metric")
summary_df$analysis <- "invariance"
summary_df <- summary_df[, c("case_id", "analysis", "step", "group_equal", "chisq", "df", "p", "cfi", "tli", "rmsea", "srmr", "delta_cfi", "delta_rmsea")]

fit_out <- do.call(rbind, fit_rows)
param_out <- do.call(rbind, param_rows)
r2_out <- do.call(rbind, r2_rows)
mod_out <- do.call(rbind, mod_rows)

write.csv(fit_out, out_fit, row.names = FALSE)
write.csv(param_out, out_params, row.names = FALSE)
write.csv(r2_out, out_r2, row.names = FALSE)
write.csv(summary_df, out_invariance, row.names = FALSE)
write.csv(mod_out, out_modindices, row.names = FALSE)
