# SPDX-License-Identifier: Apache-2.0
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript mixed_models_prep.R <input_csv> <output_csv>")
}
input_path <- args[1]
output_path <- args[2]

df <- read.csv(input_path, stringsAsFactors = FALSE)
long_df <- reshape(
  df,
  varying = c("pre_score", "mid_score", "post_score"),
  v.names = "score",
  timevar = "time",
  times = c("pre", "mid", "post"),
  idvar = "id",
  direction = "long"
)
long_df$time <- factor(long_df$time, levels = c("pre", "mid", "post"))
long_df$group3 <- factor(long_df$group3)
long_df$gender <- factor(long_df$gender)
long_df <- long_df[!is.na(long_df$score), ]
output <- long_df[, c("id", "time", "score", "group3", "gender", "x1")]
write.csv(output, output_path, row.names = FALSE)
cat("Wrote:", output_path, "\n")
