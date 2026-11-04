# Run from the package root with Arrow, DuckDB and dtplyr installed.
pkgload::load_all(".", quiet = TRUE, export_all = FALSE)

data <- tibble::tibble(k = 1:2, v = c("a", "b"), w = c(TRUE, FALSE))
inspect <- function(input) {
  inspect_grouping(input, .by = tidyselect::where(is.logical),
                   .grouping = rollup(tidyselect::where(is.integer)),
                   .format = "list")
}
expected <- tibble::tibble(
  set_id = 1:2,
  fixed = list("w", "w"),
  included = list("k", character()),
  omitted = list(character(), "k"),
  grouping_bits = list(c(k = 0L), c(k = 1L)),
  grouping_id = 0:1
)
stopifnot(identical(inspect(data), expected))

reader <- arrow::as_record_batch_reader(data)
stopifnot(
  identical(inspect(reader), expected),
  identical(as.data.frame(reader$read_next_batch()), as.data.frame(data))
)
cat("Arrow reader inspection preserved its unread first batch.\n")

calls <- 0L
source_guard <- function(x) {
  calls <<- calls + 1L
  stop("source rows reached")
}
step <- dplyr::filter(dtplyr::lazy_dt(data), source_guard(k))
stopifnot(identical(inspect(step), expected), identical(calls, 0L))
observed <- tryCatch(dplyr::collect(step), error = conditionMessage)
stopifnot(grepl("source rows reached", observed, fixed = TRUE),
          identical(calls, 1L))
cat("Typed dtplyr inspection left the filter unevaluated.\n")

probe_duckdb <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "CREATE SEQUENCE source_seq START 1")
  source <- dplyr::tbl(con, dbplyr::sql(paste(
    "SELECT CAST(1 AS INTEGER) AS k,",
    "CAST(nextval('source_seq') AS VARCHAR) AS v, TRUE AS w"
  )))
  stopifnot(identical(inspect(source), expected))
  value <- DBI::dbGetQuery(con, "SELECT nextval('source_seq') AS n")$n
  stopifnot(length(value) == 1L, as.integer(value) == 1L)
}
probe_duckdb()
cat("Typed DuckDB inspection left the source sequence unadvanced.\n")
cat(R.version.string, R.version$platform, "\n")
for (package in c("arrow", "duckdb", "dtplyr", "data.table", "dbplyr", "DBI")) {
  cat(package, as.character(utils::packageVersion(package)), "\n")
}
