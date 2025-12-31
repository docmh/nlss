args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) {
  quit(status = 1)
}
pkg <- args[1]
if (!nzchar(pkg)) {
  quit(status = 1)
}
if (requireNamespace(pkg, quietly = TRUE)) {
  quit(status = 0)
}
quit(status = 1)
