# Copyright (c) 2026 Mike Hammes
# SPDX-License-Identifier: Apache-2.0
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript prepare_labeled_dataset.R <input_csv> <output_rds> [output_rdata] [rdata_object] [output_sav]")
}

input_path <- args[1]
output_path <- args[2]
rdata_path <- if (length(args) >= 3 && nzchar(args[3])) args[3] else ""
rdata_object <- if (length(args) >= 4 && nzchar(args[4])) args[4] else "labeled_dataset"
sav_path <- if (length(args) >= 5 && nzchar(args[5])) args[5] else ""

df <- read.csv(input_path, stringsAsFactors = FALSE)

var_labels <- list(
  gender = "LBL_Gender",
  group3 = "LBL_Group3",
  group2 = "LBL_Group2",
  education = "LBL_Education",
  ordinal_var = "LBL_Ordinal",
  cat_var = "LBL_CatVar",
  cat_var2 = "LBL_CatVar2",
  outcome_anova = "LBL_OutcomeAnova",
  outcome_reg = "LBL_OutcomeReg",
  x1 = "LBL_X1",
  x2 = "LBL_X2",
  x3 = "LBL_X3",
  mediator = "LBL_Mediator",
  satisfaction = "LBL_Satisfaction",
  pre_score = "LBL_PreScore",
  mid_score = "LBL_MidScore",
  post_score = "LBL_PostScore",
  f1_1 = "LBL_F1_1",
  f1_2 = "LBL_F1_2",
  f1_3_rev = "LBL_F1_3",
  f1_4 = "LBL_F1_4",
  time = "LBL_Time",
  score = "LBL_Score"
)

var_labels <- var_labels[names(var_labels) %in% names(df)]
if (length(var_labels) > 0) {
  for (name in names(var_labels)) {
    attr(df[[name]], "label") <- var_labels[[name]]
  }
  attr(df, "variable.labels") <- var_labels
}

value_labels <- list(
  gender = c("LBL_Female" = "F", "LBL_Male" = "M", "LBL_Other" = "O"),
  group3 = c("LBL_GroupA" = "A", "LBL_GroupB" = "B", "LBL_GroupC" = "C"),
  group2 = c("LBL_Control" = "control", "LBL_Treatment" = "treatment"),
  education = c("LBL_Edu1" = 1, "LBL_Edu2" = 2, "LBL_Edu3" = 3, "LBL_Edu4" = 4, "LBL_Edu5" = 5),
  ordinal_var = c("LBL_Ord1" = 1, "LBL_Ord2" = 2, "LBL_Ord3" = 3, "LBL_Ord4" = 4, "LBL_Ord5" = 5),
  cat_var = c("LBL_Cat1" = "cat1", "LBL_Cat2" = "cat2", "LBL_Cat3" = "cat3", "LBL_Cat4" = "cat4", "LBL_Cat5" = "cat5", "LBL_Cat6" = "cat6"),
  cat_var2 = c("LBL_Low" = "low", "LBL_Med" = "med", "LBL_High" = "high"),
  time = c("LBL_PreTime" = "pre", "LBL_MidTime" = "mid", "LBL_PostTime" = "post")
)

for (name in names(value_labels)) {
  if (name %in% names(df)) {
    attr(df[[name]], "labels") <- value_labels[[name]]
  }
}

saveRDS(df, output_path)
cat("Wrote:", output_path, "\n")

if (nzchar(rdata_path)) {
  assign(rdata_object, df)
  save(list = rdata_object, file = rdata_path)
  cat("Wrote:", rdata_path, "\n")
}

if (nzchar(sav_path)) {
  if (!requireNamespace("haven", quietly = TRUE)) {
    stop("Saving .sav requires the 'haven' package.")
  }
  df_sav <- df
  if (length(value_labels) > 0) {
    for (name in names(value_labels)) {
      if (name %in% names(df_sav)) {
        var_label <- attr(df_sav[[name]], "label", exact = TRUE)
        df_sav[[name]] <- haven::labelled(df_sav[[name]], labels = value_labels[[name]], label = var_label)
      }
    }
  }
  haven::write_sav(df_sav, sav_path)
  cat("Wrote:", sav_path, "\n")
}
