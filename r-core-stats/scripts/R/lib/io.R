get_default_out <- function() {
  "./outputs/tmp"
}

ensure_out_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  path
}

get_run_context <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
  script_name <- if (length(file_arg) > 0 && nzchar(file_arg[1])) basename(file_arg[1]) else ""
  trailing <- commandArgs(trailingOnly = TRUE)
  commands <- c("Rscript", script_name, trailing)
  commands <- commands[nzchar(commands)]
  prompt <- paste(commands, collapse = " ")
  list(prompt = prompt, commands = commands)
}

get_user_prompt <- function(opts = list()) {
  val <- NULL
  if (!is.null(opts$`user-prompt`)) {
    val <- as.character(opts$`user-prompt`)
  }
  if (is.null(val) || val == "") {
    env_val <- Sys.getenv("CODEX_USER_PROMPT", unset = "")
    if (nzchar(env_val)) val <- env_val
  }
  if (is.null(val) || val == "") return(NULL)
  val
}

append_analysis_log <- function(out_dir, module, prompt, commands, results, options = list(), user_prompt = NULL) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    cat("Note: jsonlite not installed; skipping analysis_log.jsonl output.\n")
    return(invisible(FALSE))
  }

  entry <- list(
    timestamp_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    module = module,
    user_prompt = user_prompt,
    prompt = prompt,
    commands = commands,
    results = results,
    options = options
  )

  json <- jsonlite::toJSON(
    entry,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    dataframe = "rows",
    digits = NA
  )

  log_path <- file.path(out_dir, "analysis_log.jsonl")
  cat(json, file = log_path, sep = "\n", append = TRUE)
  invisible(TRUE)
}

read_sav_data <- function(path) {
  if (requireNamespace("haven", quietly = TRUE)) {
    df <- haven::read_sav(path)
    return(as.data.frame(df, stringsAsFactors = FALSE))
  }
  if (requireNamespace("foreign", quietly = TRUE)) {
    df <- suppressWarnings(foreign::read.spss(path, to.data.frame = TRUE, use.value.labels = FALSE))
    if (!is.data.frame(df)) df <- as.data.frame(df, stringsAsFactors = FALSE)
    return(df)
  }
  stop("SPSS .sav support requires the 'haven' or 'foreign' package. Install one: install.packages('haven').")
}

load_dataframe <- function(opts) {
  if (!is.null(opts$csv)) {
    sep <- if (!is.null(opts$sep)) opts$sep else ","
    header <- parse_bool(opts$header, default = TRUE)
    df <- read.csv(opts$csv, sep = sep, header = header, stringsAsFactors = FALSE)
    return(df)
  }

  if (!is.null(opts$sav)) {
    df <- read_sav_data(opts$sav)
    if (!is.data.frame(df)) stop("SAV does not contain a data frame.")
    return(df)
  }

  if (!is.null(opts$rds)) {
    df <- readRDS(opts$rds)
    if (!is.data.frame(df)) stop("RDS does not contain a data frame.")
    return(df)
  }

  if (!is.null(opts$rdata)) {
    env <- new.env()
    load(opts$rdata, envir = env)
    if (is.null(opts$df)) stop("--df is required when using --rdata")
    if (!exists(opts$df, envir = env)) stop("Data frame not found in RData.")
    df <- get(opts$df, envir = env)
    if (!is.data.frame(df)) stop("RData object is not a data frame.")
    return(df)
  }

  stop("No input provided. Use --csv, --sav, --rds, --rdata, or --interactive.")
}
