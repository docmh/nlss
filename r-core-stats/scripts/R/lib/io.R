get_default_out <- function() {
  "./outputs/tmp"
}

ensure_out_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  path
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
