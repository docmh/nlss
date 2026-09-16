# SPDX-License-Identifier: Apache-2.0
# Run from any directory: Rscript tests/phase1/import_contract_unit.R
script_arg <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])
repo <- normalizePath(file.path(dirname(script_arg[1]), "../.."), winslash = "/")
source(file.path(repo, "scripts/R/lib/import_contract.R"))
for (package in c("haven", "arrow", "hms", "jsonlite")) {
  if (!requireNamespace(package, quietly = TRUE)) stop("Install package: ", package)
}
root <- Sys.getenv("NLSS_TEST_ROOT", unset = tempdir())
dir.create(root, recursive = TRUE, showWarnings = FALSE)
out <- tempfile("import-contract-unit-", tmpdir = root)
dir.create(out)
expect_error <- function(expr, pattern) {
  error <- tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
  stopifnot(!is.null(error), grepl(pattern, error, fixed = TRUE))
}

original <- data.frame(
  numeric = haven::labelled_spss(c(1, 2, 97, 98, 99, NA),
    labels = c(Low = 1, High = 2, Refused = 99), na_values = 99,
    na_range = c(97, 98), label = "Measurement"),
  text = haven::labelled_spss(c("a", "b", "x", "a", "b", NA),
    labels = c(Alpha = "a", Beta = "b", Refused = "x"), na_values = "x"),
  tagged = haven::labelled(c(1, haven::tagged_na("a"), 2, NA, haven::tagged_na("z"), 3),
    labels = c(Refused = haven::tagged_na("a"), Unknown = haven::tagged_na("z"))),
  ordered = ordered(c("b", "a", "c", "b", NA, "a"), levels = c("a", "b", "c")))
analysis <- import_prepare_analysis(original)
stopifnot(identical(analysis$numeric, c(1, 2, NA_real_, NA_real_, NA_real_, NA_real_)),
          identical(analysis$text, c("a", "b", NA_character_, "a", "b", NA_character_)),
          identical(analysis$tagged, c(1, NA_real_, 2, NA_real_, NA_real_, 3)),
          identical(analysis$ordered, original$ordered),
          !inherits(analysis$numeric, "haven_labelled"))
contract <- attr(analysis, "nlss_import_contract")
stopifnot(identical(contract$columns$numeric$variable_label, "Measurement"),
          identical(unlist(contract$columns$numeric$missing$na_values), 99),
          identical(unlist(contract$columns$numeric$missing$na_range), c(97, 98)),
          identical(vapply(contract$columns$numeric$missing$observations, `[[`, integer(1), "row"), 3:5),
          identical(vapply(contract$columns$tagged$missing$observations, `[[`, character(1), "tag"), c("a", "z")),
          identical(contract$columns$tagged$value_labels[[1]]$missing_tag, "a"),
          identical(import_prepare_analysis(analysis), analysis))

# Reordered/filtered or imputed data must not reapply source-row missing codes.
changed <- analysis[c(6, 5, 1), ]
changed$numeric[2] <- 42
changed <- import_prepare_analysis(changed)
stopifnot(identical(changed$numeric, c(NA_real_, 42, 1)),
          identical(attr(changed, "nlss_import_contract")$columns$numeric, contract$columns$numeric))

legacy <- data.frame(x = c(1, 2), text = c("001", "002"))
attr(legacy, "nlss_labels") <- list(variables = list(x = "Legacy measure"),
  values = list(x = list(`1` = "Low", `2` = "High"), text = list(`001` = "First", `002` = "Second")))
legacy_contract <- import_capture_dictionary(legacy)
stopifnot(identical(legacy_contract$columns$x$variable_label, "Legacy measure"),
          identical(legacy_contract$columns$x$value_labels[[1]]$value, 1),
          identical(legacy_contract$columns$text$value_labels[[1]]$value, "001"))

temporal <- data.frame(
  date = as.Date(c("1970-01-01", "2024-02-29", NA)),
  datetime = as.POSIXct(c("2024-01-01 12:34:56", "2024-01-02 00:00:00", NA), tz = "Europe/Berlin") + c(.123456, .999999, 0),
  time = hms::hms(c(1.123456789, -0.123456789, NA)),
  duration = as.difftime(c(.123456789, 24.987654321, NA), units = "hours"))
storage <- import_prepare_storage(temporal)
storage_contract <- attr(storage, "nlss_import_contract")
json <- jsonlite::toJSON(storage_contract, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
attr(storage, "nlss_import_contract") <- NULL
arrow::write_parquet(storage, file.path(out, "temporal.parquet"))
restored <- import_restore_storage(as.data.frame(arrow::read_parquet(file.path(out, "temporal.parquet"))), parsed)
for (name in names(temporal)) {
  stopifnot(identical(class(restored[[name]]), class(temporal[[name]])),
            identical(as.numeric(restored[[name]]), as.numeric(temporal[[name]])))
}
stopifnot(identical(attr(restored$datetime, "tzone"), "Europe/Berlin"),
          identical(attr(restored$duration, "units"), "hours"))

csv <- file.path(out, "german.csv")
writeLines(c("id;score;group", "001;1,5;A", "002;2,5;B", "003;MISSING;A"), csv)
read <- import_csv(csv, list(sep = ";", `csv-decimal` = ",", `csv-na-values` = "MISSING"))
stopifnot(identical(read$id, c("001", "002", "003")),
          identical(read$score, c(1.5, 2.5, NA_real_)),
          identical(attr(read, "nlss_import_contract")$csv$inference$id$reason, "leading_zero_identifier"))
explicit <- import_csv(csv, list(sep = ";", `csv-decimal` = ",", `csv-na-values` = "MISSING",
                                 `csv-col-types` = "id=integer,score=numeric,group=factor"))
stopifnot(identical(explicit$id, 1:3), identical(explicit$score, c(1.5, 2.5, NA_real_)), is.factor(explicit$group))
expect_error(import_csv(csv, list(sep = ";", `csv-col-types` = "typo=character")), "Unknown CSV columns")
expect_error(import_csv(csv, list(sep = ";", `csv-col-types` = "id=unknown")), "Unsupported CSV column type")
expect_error(import_csv(csv, list(sep = ";", `csv-col-types` = "score=numeric")), "Invalid numeric value")
expect_error(import_csv(csv, list(sep = ";", header = "flase")), "--header")
expect_error(import_csv(csv, list(`csv-decimal` = ",")), "differ from the decimal mark")
expect_error(import_csv(csv, list(sep = ";", `csv-col-types` = "id=integer,id=character")), "Duplicate CSV")
expect_error(import_capture_dictionary(setNames(data.frame(a = 1, b = 2), c("x", "x"))), "unique")

# Empty NA-token list keeps literal NA identifiers; precision-sensitive integer
# identifiers must not be rounded even without a leading zero.
csv2 <- file.path(out, "identifiers.csv")
writeLines(c("id,text", "9007199254740993,NA", "9007199254740995,ok"), csv2)
large <- import_csv(csv2, list(`csv-na-values` = ""))
stopifnot(identical(large$id, c("9007199254740993", "9007199254740995")), identical(large$text, c("NA", "ok")))
encoded <- file.path(out, "latin1.csv")
connection <- file(encoded, "wb")
writeBin(charToRaw("x,label\n1,Gr\374n\n2,Blau\n"), connection)
close(connection)
expect_error(import_csv(encoded), "could not read all data reliably")
decoded <- import_csv(encoded, list(`csv-encoding` = "latin1"))
stopifnot(nrow(decoded) == 2L, identical(decoded$label, c("Gr\u00fcn", "Blau")))
blanks <- file.path(out, "blanks.csv")
writeLines(c("id,x,date,flag", "001,1,2024-01-01,TRUE", "002,,,", " ,3,2024-01-03,FALSE"), blanks)
blank_data <- import_csv(blanks, list(`csv-col-types` = "x=numeric,date=date,flag=logical"))
stopifnot(identical(blank_data$id, c("001", "002", " ")), is.na(blank_data$x[2]),
          is.na(blank_data$date[2]), is.na(blank_data$flag[2]))
cat("PASS import contract: labels, user missings, tagged NA, idempotence, temporal precision, CSV types and invalid options\n")
cat("Artifacts:", out, "\n")
