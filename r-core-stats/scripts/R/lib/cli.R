parse_args <- function(args) {
  opts <- list()
  i <- 1
  while (i <= length(args)) {
    arg <- args[i]
    if (grepl("^--", arg)) {
      key <- sub("^--", "", arg)
      if (grepl("=", key)) {
        parts <- strsplit(key, "=", fixed = TRUE)[[1]]
        opts[[parts[1]]] <- parts[2]
      } else if (i < length(args) && !grepl("^--", args[i + 1])) {
        opts[[key]] <- args[i + 1]
        i <- i + 1
      } else {
        opts[[key]] <- TRUE
      }
    }
    i <- i + 1
  }
  opts
}

parse_bool <- function(value, default = FALSE) {
  if (is.null(value)) return(default)
  if (is.logical(value)) return(value)
  val <- tolower(as.character(value))
  val %in% c("true", "t", "1", "yes", "y")
}

parse_list <- function(value, sep = ",") {
  if (is.null(value) || is.logical(value)) return(character(0))
  value <- as.character(value)
  if (value == "") return(character(0))
  trimws(strsplit(value, sep, fixed = TRUE)[[1]])
}

prompt <- function(label, default = NULL) {
  if (is.null(default)) {
    answer <- readline(paste0(label, ": "))
  } else {
    answer <- readline(paste0(label, " [", default, "]: "))
    if (answer == "") answer <- default
  }
  answer
}
