Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(marginplyr))

run_case <- function(derived, existing_temp = FALSE, destination = "facts") {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  source <- data.frame(g = c("b", "a"), payload = c(11L, 22L))
  DBI::dbWriteTable(con, "facts", source)
  if (existing_temp) {
    DBI::dbExecute(con, "CREATE TEMP TABLE facts AS SELECT * FROM main.facts")
  }
  input <- tbl(con, "facts")
  result <- expand_with_margins(
    input, .grouping = rollup(g), .id = "TRUE",
    .margin_label = NULL, .sort = "last"
  )
  if (derived) result <- filter(result, payload > 0L)
  expected <- collect(result)
  actual <- tryCatch(
    compute(result, name = destination, temporary = TRUE,
            overwrite = existing_temp, analyze = FALSE),
    error = identity
  )
  source_after <- DBI::dbReadTable(con, DBI::Id(schema = "main", table = "facts"))
  temp_after <- if (DBI::dbExistsTable(con, DBI::Id(schema = "temp", table = "facts"))) {
    DBI::dbReadTable(con, DBI::Id(schema = "temp", table = "facts"))
  } else NULL
  list(
    derived = derived, existing_temp = existing_temp,
    expected_rows = nrow(expected),
    actual_rows = if (inherits(actual, "error")) NA_integer_ else nrow(collect(actual)),
    error = if (inherits(actual, "error")) conditionMessage(actual) else NULL,
    persistent_source_unchanged = identical(source, source_after),
    temporary_rows_after = if (is.null(temp_after)) NA_integer_ else nrow(temp_after)
  )
}

observations <- list(
  direct_new = run_case(FALSE),
  derived_new = run_case(TRUE),
  derived_different_name = run_case(TRUE, destination = "saved"),
  direct_existing = run_case(FALSE, TRUE),
  derived_existing = run_case(TRUE, TRUE)
)
print(observations)
stopifnot(
  observations$direct_new$actual_rows == 4L,
  observations$derived_different_name$actual_rows == 4L,
  !is.null(observations$direct_existing$error),
  observations$direct_existing$temporary_rows_after == 2L,
  all(vapply(observations, `[[`, logical(1), "persistent_source_unchanged"))
)
if ("--assert" %in% commandArgs(trailingOnly = TRUE)) {
  # A refusal must preserve the old destination; success must copy all rows.
  stopifnot(
    observations$derived_new$actual_rows == observations$derived_new$expected_rows,
    if (is.null(observations$derived_existing$error)) {
      observations$derived_existing$actual_rows == observations$derived_existing$expected_rows
    } else observations$derived_existing$temporary_rows_after == 2L
  )
}
