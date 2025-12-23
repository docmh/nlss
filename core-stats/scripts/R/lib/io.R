resolve_config_value <- function(path, default = NULL) {
  if (exists("get_config_value", mode = "function")) {
    return(get("get_config_value", mode = "function")(path, default = default))
  }
  default
}

get_default_out <- function() {
  default_out <- resolve_config_value("defaults.output_dir", "./outputs/tmp")
  if (is.null(default_out) || !nzchar(default_out)) return("./outputs/tmp")
  default_out
}

ensure_out_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  path
}

resolve_parse_bool <- function(value, default = FALSE) {
  if (exists("parse_bool", mode = "function")) {
    return(get("parse_bool", mode = "function")(value, default = default))
  }
  if (is.null(value)) return(default)
  if (is.logical(value)) return(value)
  val <- tolower(as.character(value))
  val %in% c("true", "t", "1", "yes", "y")
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
  con <- file(log_path, open = "a", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(json, con = con, sep = "\n")
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

ensure_arrow <- function() {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("Parquet support requires the 'arrow' package. Install it: install.packages('arrow').")
  }
}

read_parquet_data <- function(path) {
  ensure_arrow()
  df <- arrow::read_parquet(path, as_data_frame = TRUE)
  as.data.frame(df, stringsAsFactors = FALSE)
}

write_parquet_data <- function(df, path) {
  ensure_arrow()
  arrow::write_parquet(df, path)
}

load_or_create_parquet <- function(copy_path, read_source) {
  if (file.exists(copy_path)) {
    return(read_parquet_data(copy_path))
  }
  df <- read_source()
  write_parquet_data(df, copy_path)
  read_parquet_data(copy_path)
}

sanitize_file_component <- function(value) {
  clean <- gsub("[^A-Za-z0-9._-]", "_", as.character(value))
  clean <- gsub("_+", "_", clean)
  if (!nzchar(clean)) clean <- "dataset"
  clean
}

derive_dataset_label <- function(path, df_name = NULL) {
  if (!is.null(df_name) && nzchar(df_name)) return(as.character(df_name))
  base <- tools::file_path_sans_ext(basename(path))
  if (!nzchar(base)) base <- "dataset"
  base
}

normalize_path <- function(path) {
  if (is.null(path) || !nzchar(path)) return("")
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

get_dataset_workspace_dir <- function(label) {
  file_label <- sanitize_file_component(label)
  root <- ensure_out_dir(get_default_out())
  ensure_out_dir(file.path(root, file_label))
}

get_workspace_out_dir <- function(df = NULL, label = NULL) {
  if (!is.null(df)) {
    dir_attr <- attr(df, "workspace_dir")
    if (!is.null(dir_attr) && nzchar(dir_attr)) return(ensure_out_dir(dir_attr))
    parquet_path <- attr(df, "workspace_parquet_path")
    if (!is.null(parquet_path) && nzchar(parquet_path)) {
      return(ensure_out_dir(dirname(parquet_path)))
    }
  }
  if (!is.null(label) && nzchar(label)) {
    return(get_dataset_workspace_dir(label))
  }
  ensure_out_dir(get_default_out())
}

format_backup_timestamp <- function() {
  now <- Sys.time()
  base <- format(now, "%Y%m%d%H%M%S")
  frac <- as.numeric(now) %% 1
  ms <- as.integer(floor(frac * 1000))
  paste0(base, sprintf("%03d", ms))
}

backup_workspace_parquet <- function(parquet_path) {
  if (is.null(parquet_path) || !nzchar(parquet_path)) return("")
  if (!file.exists(parquet_path)) return("")
  dataset_dir <- dirname(parquet_path)
  backup_dir <- ensure_out_dir(file.path(dataset_dir, "backup"))
  file_label <- tools::file_path_sans_ext(basename(parquet_path))
  timestamp <- format_backup_timestamp()
  backup_path <- file.path(backup_dir, paste0(file_label, "-", timestamp, ".parquet"))
  copied <- file.copy(parquet_path, backup_path, overwrite = FALSE)
  if (!isTRUE(copied)) return("")
  normalize_path(backup_path)
}

build_workspace_copy_info <- function(label) {
  file_label <- sanitize_file_component(label)
  out_dir <- get_dataset_workspace_dir(label)
  list(
    label = label,
    file_label = file_label,
    out_dir = out_dir,
    copy_path = file.path(out_dir, paste0(file_label, ".parquet"))
  )
}

load_rdata_frame <- function(path, object_name = NULL) {
  env <- new.env()
  load(path, envir = env)
  if (!is.null(object_name) && nzchar(object_name) && exists(object_name, envir = env)) {
    df <- get(object_name, envir = env)
    if (!is.data.frame(df)) stop("RData object is not a data frame.")
    return(list(df = df, object_name = object_name))
  }
  objects <- ls(env)
  if (length(objects) == 0) stop("RData does not contain a data frame.")
  df_names <- objects[vapply(objects, function(name) {
    is.data.frame(get(name, envir = env))
  }, logical(1))]
  if (length(df_names) == 0) stop("RData does not contain a data frame.")
  df_name <- df_names[1]
  df <- get(df_name, envir = env)
  if (!is.data.frame(df)) stop("RData object is not a data frame.")
  list(df = df, object_name = df_name)
}

load_dataframe <- function(opts) {
  ensure_out_dir(get_default_out())

  if (!is.null(opts$parquet)) {
    source_path <- as.character(opts$parquet)
    label <- derive_dataset_label(source_path)
    copy_info <- build_workspace_copy_info(label)
    if (normalizePath(source_path, winslash = "/", mustWork = FALSE) ==
        normalizePath(copy_info$copy_path, winslash = "/", mustWork = FALSE)) {
      df <- read_parquet_data(copy_info$copy_path)
    } else {
      df <- load_or_create_parquet(copy_info$copy_path, function() {
        read_parquet_data(source_path)
      })
    }
    attr(df, "workspace_parquet_path") <- normalize_path(copy_info$copy_path)
    attr(df, "workspace_source_path") <- normalize_path(source_path)
    attr(df, "workspace_dir") <- normalize_path(copy_info$out_dir)
    return(df)
  }

  if (!is.null(opts$csv)) {
    source_path <- as.character(opts$csv)
    label <- derive_dataset_label(source_path)
    copy_info <- build_workspace_copy_info(label)
    sep_default <- resolve_config_value("defaults.csv.sep", ",")
    sep <- if (!is.null(opts$sep)) opts$sep else sep_default
    header_default <- resolve_config_value("defaults.csv.header", TRUE)
    header <- resolve_parse_bool(opts$header, default = header_default)
    df <- load_or_create_parquet(copy_info$copy_path, function() {
      read.csv(source_path, sep = sep, header = header, stringsAsFactors = FALSE)
    })
    attr(df, "workspace_parquet_path") <- normalize_path(copy_info$copy_path)
    attr(df, "workspace_source_path") <- normalize_path(source_path)
    attr(df, "workspace_dir") <- normalize_path(copy_info$out_dir)
    return(df)
  }

  if (!is.null(opts$sav)) {
    source_path <- as.character(opts$sav)
    label <- derive_dataset_label(source_path)
    copy_info <- build_workspace_copy_info(label)
    df <- load_or_create_parquet(copy_info$copy_path, function() {
      df <- read_sav_data(source_path)
      if (!is.data.frame(df)) stop("SAV does not contain a data frame.")
      df
    })
    attr(df, "workspace_parquet_path") <- normalize_path(copy_info$copy_path)
    attr(df, "workspace_source_path") <- normalize_path(source_path)
    attr(df, "workspace_dir") <- normalize_path(copy_info$out_dir)
    return(df)
  }

  if (!is.null(opts$rds)) {
    source_path <- as.character(opts$rds)
    label <- derive_dataset_label(source_path)
    copy_info <- build_workspace_copy_info(label)
    df <- load_or_create_parquet(copy_info$copy_path, function() {
      df <- readRDS(source_path)
      if (!is.data.frame(df)) stop("RDS does not contain a data frame.")
      df
    })
    attr(df, "workspace_parquet_path") <- normalize_path(copy_info$copy_path)
    attr(df, "workspace_source_path") <- normalize_path(source_path)
    attr(df, "workspace_dir") <- normalize_path(copy_info$out_dir)
    return(df)
  }

  if (!is.null(opts$rdata)) {
    if (is.null(opts$df)) stop("--df is required when using --rdata")
    source_path <- as.character(opts$rdata)
    label <- derive_dataset_label(source_path, opts$df)
    copy_info <- build_workspace_copy_info(label)
    df <- load_or_create_parquet(copy_info$copy_path, function() {
      res <- load_rdata_frame(source_path, opts$df)
      res$df
    })
    attr(df, "workspace_parquet_path") <- normalize_path(copy_info$copy_path)
    attr(df, "workspace_source_path") <- normalize_path(source_path)
    attr(df, "workspace_dir") <- normalize_path(copy_info$out_dir)
    return(df)
  }

  stop("No input provided. Use --csv, --sav, --rds, --rdata, --parquet, or --interactive.")
}
