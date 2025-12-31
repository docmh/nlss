get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", cmd_args[grep("^--file=", cmd_args)])
  if (length(file_arg) > 0 && nzchar(file_arg[1])) {
    return(dirname(normalizePath(file_arg[1], winslash = "/", mustWork = FALSE)))
  }
  frame_file <- tryCatch(sys.frames()[[1]]$ofile, error = function(e) NULL)
  if (!is.null(frame_file) && nzchar(frame_file)) {
    return(dirname(normalizePath(frame_file, winslash = "/", mustWork = FALSE)))
  }
  getwd()
}

source_lib <- function(file_name) {
  script_dir <- get_script_dir()
  lib_path <- file.path(script_dir, "lib", file_name)
  if (!file.exists(lib_path)) {
    stop("Missing lib file: ", lib_path)
  }
  source(lib_path)
}
