# Runs every `tests/testthat/test-duckdb-*.R` file against the installed
# DuckDB, after asserting that DuckDB is at or above DESCRIPTION's floor and
# below 2.0 (`duckdb_floor_accept()` in `duckdb-floor.R`). Against the working
# tree, as `release-matrix.yaml`'s `structure` job is.
#
# `MARGINPLYR_REQUIRED_SUGGESTS=duckdb` turns a missing DuckDB into a failure
# rather than a skip, and with it `duckdb_json_test_mode()` requires the json
# extension the job provisions beforehand. Both variables are set here, as
# `verify-suite-coverage.R` sets `NOT_CRAN`, so a local run matches CI.
#
# Run it locally, with the DuckDB under test first on the library path, with:
#
#     Rscript .github/scripts/verify-duckdb-floor.R
source(".github/scripts/duckdb-floor.R")

installed <- if (nzchar(system.file(package = "duckdb"))) {
  utils::packageVersion("duckdb")
}
message(duckdb_floor_accept(installed, duckdb_floor()))

Sys.setenv(NOT_CRAN = "true", MARGINPLYR_REQUIRED_SUGGESTS = "duckdb")
files <- list.files("tests/testthat", pattern = "^test-duckdb-.*[.]R$")
expected <- sort(sub("^test-(.*)[.]R$", "\\1", files))
if (length(expected) == 0L) {
  stop("No tests/testthat/test-duckdb-*.R file was found.", call. = FALSE)
}
results <- testthat::test_local(filter = "^duckdb-", stop_on_failure = TRUE)

# A file the filter stopped selecting would leave a green run behind it.
ran <- unique(sub("^test-(.*)[.]R$", "\\1", as.data.frame(results)$file))
missing <- setdiff(expected, ran)
if (length(missing) > 0L) {
  stop(
    "These DuckDB test files did not run: ",
    paste0("test-", missing, ".R", collapse = ", "),
    call. = FALSE
  )
}
message(
  "Ran ", length(expected), " DuckDB test files against DuckDB ",
  format(installed), "."
)
