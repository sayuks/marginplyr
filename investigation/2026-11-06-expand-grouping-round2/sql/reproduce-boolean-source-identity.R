# Run from a package checkout; --assert requires the desired identity contract.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
input <- data.frame(g = "a", payload = 7L)
names(input)[[2L]] <- "TRUE"
DBI::dbWriteTable(con, "input", input)
source <- mutate(tbl(con, "input"), z = row_number(g))
expected <- tibble::tibble(g = "a", `TRUE` = 7L, z = 1L)
stopifnot(identical(collect(source), expected))
stopifnot(identical(collect(select(source, everything())), expected))
stopifnot(identical(collect(mutate(source)), expected))
renamed <- collect(rename(source, payload = all_of("TRUE")))
stopifnot(identical(renamed$payload, 7L))
cat("Ordinary source, all-column selection, no-op mutation and explicit rename passed.\n")
margin <- expand_with_margins(source)
stopifnot(identical(as.character(tbl_vars(margin)), names(expected)))
actual <- tryCatch(collect(margin), error = identity)
cat("Expansion result: ", if (inherits(actual, "error")) {
  conditionMessage(actual)
} else {
  paste(names(actual), collapse = ", ")
}, "\n", sep = "")
sql <- as.character(dbplyr::sql_render(margin))
cat("Rendered SQL:\n", sql, "\n", sep = "")
raw <- tryCatch(DBI::dbGetQuery(con, sql), error = identity)
cat("Raw result: ", if (inherits(raw, "error")) {
  conditionMessage(raw)
} else {
  paste(names(raw), collapse = ", ")
}, "\n", sep = "")
root_control <- collect(expand_with_margins(tbl(con, "input")))
stopifnot(identical(root_control, tibble::as_tibble(input)))
safe <- mutate(rename(tbl(con, "input"), payload = all_of("TRUE")),
               z = row_number(g))
stopifnot(identical(collect(expand_with_margins(safe)), collect(safe)))
cat("Unwrapped source and pre-aliased window controls passed.\n")
DBI::dbDisconnect(con)
if ("--assert" %in% commandArgs(trailingOnly = TRUE) &&
    !identical(actual, expected)) {
  stop("Included-only expansion must preserve the executable source's public columns and values.")
}
