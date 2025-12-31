# Copyright (c) 2026 Mike Hammes
# SPDX-License-Identifier: Apache-2.0
cat("[INFO] R version: ", R.version$version.string, "\n", sep = "")
cat("[INFO] R.home(): ", R.home(), "\n", sep = "")
cat("[INFO] Rscript: ", Sys.which("Rscript"), "\n", sep = "")
cat("[INFO] libPaths: ", paste(.libPaths(), collapse = "; "), "\n", sep = "")
cat("[INFO] R_LIBS_USER: ", Sys.getenv("R_LIBS_USER"), "\n", sep = "")
cat("[INFO] R_LIBS_SITE: ", Sys.getenv("R_LIBS_SITE"), "\n", sep = "")
has_haven <- requireNamespace("haven", quietly = TRUE)
cat("[INFO] haven available: ", ifelse(has_haven, "yes", "no"), "\n", sep = "")
if (has_haven) {
  cat("[INFO] haven version: ", as.character(utils::packageVersion("haven")), "\n", sep = "")
}
