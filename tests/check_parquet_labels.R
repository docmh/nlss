args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript check_parquet_labels.R <parquet_path> <label>")
}

parquet_path <- args[1]
label <- args[2]

script_dir <- {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", cmd_args[grep("^--file=", cmd_args)])
  if (length(file_arg) > 0 && nzchar(file_arg[1])) {
    dirname(normalizePath(file_arg[1], winslash = "/", mustWork = FALSE))
  } else {
    getwd()
  }
}

repo_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "scripts", "R", "lib", "io.R"))
source(file.path(repo_root, "scripts", "R", "lib", "data_utils.R"))

df <- read_parquet_data(parquet_path, lock_safe = FALSE)
labels <- attr(df, "nlss_labels")
if (!is.list(labels)) stop("Missing nlss_labels metadata.")

check_equal <- function(actual, expected, name) {
  if (is.null(actual) || !nzchar(actual)) {
    stop("Missing label for ", name)
  }
  if (as.character(actual) != expected) {
    stop("Label mismatch for ", name, ": expected ", expected, ", got ", actual)
  }
}

check_equal(resolve_variable_label(labels, "gender"), "LBL_Gender", "variables.gender")
check_equal(resolve_variable_label(labels, "education"), "LBL_Education", "variables.education")
check_equal(resolve_variable_label(labels, "ordinal_var"), "LBL_Ordinal", "variables.ordinal_var")
check_equal(resolve_value_label(labels, "group2", "control"), "LBL_Control", "values.group2.control")
check_equal(resolve_value_label(labels, "group2", "treatment"), "LBL_Treatment", "values.group2.treatment")

cat("META_OK", label, "\n")
