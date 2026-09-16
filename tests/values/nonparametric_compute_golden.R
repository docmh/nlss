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

data_path <- trim_arg(get_arg("--data", file.path("tests", "data", "golden_dataset.csv")))
out_summary <- trim_arg(get_arg("--out", file.path("tests", "values", "nonparametric_golden.csv")))
out_posthoc <- trim_arg(get_arg("--posthoc-out", file.path("tests", "values", "nonparametric_posthoc_golden.csv")))
out_diagnostics <- trim_arg(get_arg("--diagnostics-out", file.path("tests", "values", "nonparametric_diagnostics_golden.csv")))

if (is.null(data_path) || !file.exists(data_path)) {
  stop("Missing --data path.")
}
if (is.null(out_summary) || is.null(out_posthoc) || is.null(out_diagnostics)) {
  stop("Missing output path(s).")
}

options(scipen = 999, digits = 15)

df <- read.csv(data_path, stringsAsFactors = FALSE)

summary_cols <- c(
  "case_id", "test_type", "variable", "measure_1", "measure_2",
  "group", "group_1", "group_2",
  "n_1", "n_2", "n_total",
  "median_1", "median_2", "iqr_1", "iqr_2",
  "median", "iqr", "median_diff", "iqr_diff",
  "statistic", "df", "p", "effect_size_value", "ci_low", "ci_high", "mu",
  "opt_test", "opt_mode", "opt_vars", "opt_x", "opt_y", "opt_group",
  "opt_within", "opt_subject_id",
  "opt_mu", "opt_alternative", "opt_exact", "opt_continuity", "opt_conf_level",
  "opt_posthoc", "opt_p_adjust", "opt_effect_size", "opt_digits"
)

posthoc_cols <- c(
  "case_id", "test_type", "variable", "group", "group_1", "group_2",
  "n_1", "n_2", "statistic", "p", "p_adj", "effect_size_value", "ci_low", "ci_high"
)

diag_cols <- c(
  "case_id", "test_type", "variable", "group",
  "n_total", "n_nonzero", "zero_diff_n", "n", "k",
  "ties", "exact_used", "continuity_used", "group_levels", "group_sizes"
)

make_row <- function(columns, values) {
  row <- as.list(setNames(rep(NA, length(columns)), columns))
  for (name in names(values)) {
    row[[name]] <- values[[name]]
  }
  as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
}


# Five fixture-specific references, not a second implementation of the module.
# Primary inference/intervals are stats::wilcox.test / kruskal.test / friedman.test.
# Rank effects use pairwise superiority or invert base R's uncorrected one-sided
# normal p-value; no NLSS function or module source is loaded.
summary_rows <- diag_rows <- posthoc_rows <- list()
wtest <- function(x, y = NULL, paired = FALSE, alternative = "two.sided", correct = TRUE, level = .95)
  suppressWarnings(stats::wilcox.test(x, y, paired = paired, alternative = alternative,
    exact = FALSE, correct = correct, conf.int = TRUE, conf.level = level))
rank_r <- function(x, y = NULL, paired = FALSE) {
  ref <- suppressWarnings(stats::wilcox.test(x, y, paired = paired, exact = FALSE,
    correct = FALSE, alternative = "less"))
  n <- if (is.null(y)) sum(x != 0) else if (paired) sum(x != y) else length(x)+length(y)
  qnorm(ref$p.value) / sqrt(n)
}
bounds <- function(test) list(statistic = unname(test$statistic), p = test$p.value,
  ci_low = test$conf.int[1], ci_high = test$conf.int[2],
  ci_low_status = if (is.infinite(test$conf.int[1])) "negative_infinity" else "finite",
  ci_high_status = if (is.infinite(test$conf.int[2])) "positive_infinity" else "finite")
summary_cols <- c(summary_cols,"ci_low_status","ci_high_status")
posthoc_cols <- c(posthoc_cols,"ci_low_status","ci_high_status")

x <- na.omit(df$skewed_var)
fit <- wtest(x, alternative="greater", level=.9)
ranks <- rank(abs(x[x!=0]))
summary_rows[[1]] <- make_row(summary_cols, c(list(case_id="wilcoxon_one_sample_rb",
  test_type="Wilcoxon signed-rank", variable="skewed_var", n_1=length(x), n_total=length(x),
  median=median(x), iqr=IQR(x), median_diff=median(x), iqr_diff=IQR(x),
  effect_size_value=sum(sign(x[x!=0])*ranks)/sum(ranks), mu=0,
  opt_test="wilcoxon", opt_mode="wilcoxon_one_sample",opt_vars="skewed_var",opt_mu=0,
  opt_alternative="greater",opt_exact=FALSE,opt_continuity=TRUE,opt_conf_level=.9,
  opt_effect_size="rb",opt_digits=3), bounds(fit)))
diag_rows[[1]] <- make_row(diag_cols,list(case_id="diag_wilcoxon_one_sample",
  test_type="wilcoxon_one_sample",variable="skewed_var",group="",n_total=length(x),
  n_nonzero=sum(x!=0),zero_diff_n=sum(x==0),ties=anyDuplicated(ranks)>0,
  exact_used=FALSE,continuity_used=TRUE))

complete <- complete.cases(df[,c("pre_score","post_score")])
x <- df$pre_score[complete]; y <- df$post_score[complete]
fit <- wtest(x,y,paired=TRUE,correct=FALSE)
summary_rows[[2]] <- make_row(summary_cols,c(list(case_id="wilcoxon_paired_r",
  test_type="Wilcoxon signed-rank",variable="pre_score vs post_score",measure_1="pre_score",
  measure_2="post_score",n_1=length(x),n_total=length(x),median_1=median(x),median_2=median(y),
  iqr_1=IQR(x),iqr_2=IQR(y),median_diff=median(x-y),iqr_diff=IQR(x-y),
  effect_size_value=rank_r(x,y,TRUE), opt_test="wilcoxon",opt_mode="wilcoxon_paired",
  opt_x="pre_score",opt_y="post_score",opt_alternative="two.sided",opt_exact=FALSE,
  opt_continuity=FALSE,opt_conf_level=.95,opt_effect_size="r",opt_digits=2),bounds(fit)))
diag_rows[[2]] <- make_row(diag_cols,list(case_id="diag_wilcoxon_paired",test_type="wilcoxon_paired",
  variable="pre_score vs post_score",group="",n_total=length(x),n_nonzero=sum(x!=y),
  zero_diff_n=sum(x==y),ties=anyDuplicated(abs((x-y)[x!=y]))>0,
  exact_used=FALSE,continuity_used=FALSE))

complete <- complete.cases(df[,c("outcome_anova","group2")])
d <- df[complete,]; x <- d$outcome_anova[d$group2=="control"]; y <- d$outcome_anova[d$group2=="treatment"]
fit <- wtest(x,y)
summary_rows[[3]] <- make_row(summary_cols,c(list(case_id="mann_whitney_rb",
  test_type="Mann-Whitney U",variable="outcome_anova",group="group2",group_1="control",
  group_2="treatment",n_1=length(x),n_2=length(y),n_total=length(x)+length(y),
  median_1=median(x),median_2=median(y),iqr_1=IQR(x),iqr_2=IQR(y),median_diff=median(x)-median(y),
  effect_size_value=mean(outer(x,y,">"))-mean(outer(x,y,"<")),opt_test="mann_whitney",
  opt_mode="mann_whitney",opt_vars="outcome_anova",opt_group="group2",opt_alternative="two.sided",
  opt_exact=FALSE,opt_continuity=TRUE,opt_conf_level=.95,opt_effect_size="rb",opt_digits=2),bounds(fit)))
for (group in c("control","treatment")) diag_rows[[length(diag_rows)+1L]] <- make_row(diag_cols,list(
  case_id=paste0("diag_mann_whitney_",group),test_type="mann_whitney",variable="outcome_anova",
  group=group,n=sum(d$group2==group),ties=anyDuplicated(c(x,y))>0,exact_used=FALSE,continuity_used=TRUE))

d <- df[complete.cases(df[,c("outcome_anova","group3")]),]
samples <- split(d$outcome_anova,d$group3)
fit <- stats::kruskal.test(samples)
summary_rows[[4]] <- make_row(summary_cols,list(case_id="kruskal_outcome_anova",
  test_type="Kruskal-Wallis",variable="outcome_anova",group="group3",n_total=nrow(d),
  statistic=unname(fit$statistic),df=unname(fit$parameter),p=fit$p.value,
  effect_size_value=(unname(fit$statistic)-length(samples)+1)/(nrow(d)-length(samples)),
  opt_test="kruskal",opt_mode="kruskal",opt_vars="outcome_anova",opt_group="group3",
  opt_posthoc="pairwise",opt_p_adjust="holm",opt_effect_size="eta_H_sq",opt_digits=3))
diag_rows[[5]] <- make_row(diag_cols,list(case_id="diag_kruskal_outcome_anova",
  test_type="kruskal",variable="outcome_anova",group="group3",n_total=nrow(d),
  group_levels=paste(names(samples),collapse=","),group_sizes=paste(lengths(samples),collapse=",")))
pair_fits <- lapply(combn(names(samples),2,simplify=FALSE),function(pair) wtest(samples[[pair[1]]],samples[[pair[2]]]))
x <- samples$A; y <- samples$B
posthoc_rows[[1]] <- make_row(posthoc_cols,c(list(case_id="kruskal_posthoc_A_B",
  test_type="kruskal_posthoc",variable="outcome_anova",group="group3",group_1="A",group_2="B",
  n_1=length(x),n_2=length(y),p_adj=p.adjust(vapply(pair_fits,function(f)f$p.value,numeric(1)),"holm")[1],
  effect_size_value=rank_r(x,y)),bounds(pair_fits[[1]])))

within <- c("pre_score","mid_score","post_score")
d <- df[complete.cases(df[,c("id",within)]),within]
fit <- stats::friedman.test(as.matrix(d))
summary_rows[[5]] <- make_row(summary_cols,list(case_id="friedman_pre_mid_post",test_type="Friedman",
  variable=paste(within,collapse=","),n_total=nrow(d),statistic=unname(fit$statistic),
  df=unname(fit$parameter),p=fit$p.value,effect_size_value=unname(fit$statistic)/(nrow(d)*(length(within)-1)),
  opt_test="friedman",opt_mode="friedman",opt_within=paste(within,collapse=","),
  opt_subject_id="id",opt_posthoc="pairwise",opt_p_adjust="BH",opt_effect_size="kendall_w",opt_digits=3))
diag_rows[[6]] <- make_row(diag_cols,list(case_id="diag_friedman_pre_mid_post",
  test_type="friedman",variable=paste(within,collapse=","),group="",n_total=nrow(d),k=length(within)))
pair_fits <- lapply(combn(within,2,simplify=FALSE),function(pair)wtest(d[[pair[1]]],d[[pair[2]]],TRUE))
posthoc_rows[[2]] <- make_row(posthoc_cols,c(list(case_id="friedman_posthoc_pre_mid",
  test_type="friedman_posthoc",variable=paste(within,collapse=","),group="",group_1="pre_score",
  group_2="mid_score",n_1=nrow(d),p_adj=p.adjust(vapply(pair_fits,function(f)f$p.value,numeric(1)),"BH")[1],
  effect_size_value=rank_r(d$pre_score,d$mid_score,TRUE)),bounds(pair_fits[[1]])))
write.csv(do.call(rbind,summary_rows),out_summary,row.names=FALSE)
write.csv(do.call(rbind,posthoc_rows),out_posthoc,row.names=FALSE)
write.csv(do.call(rbind,diag_rows),out_diagnostics,row.names=FALSE)
