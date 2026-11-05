Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(".", quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(marginplyr))

args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1L]] else tempfile(fileext = ".csv")
grid <- expand.grid(
  name = c("TRUE", "FaLsE"), role = c("id", "payload"),
  sort = c("none", "first", "last"),
  transform = c("direct", "filter", "head"),
  destination = c("same", "case", "qualified", "different"),
  existing_temp = c(FALSE, TRUE), stringsAsFactors = FALSE
)
run <- function(case) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  source <- data.frame(g = c("b", "a"), payload = c(11L, 22L))
  if (case$role == "payload") names(source)[[2L]] <- case$name
  DBI::dbWriteTable(con, "facts", source)
  if (case$existing_temp) DBI::dbExecute(con, "CREATE TEMP TABLE facts AS SELECT * FROM main.facts")
  result <- expand_with_margins(
    tbl(con, "facts"), .grouping = rollup(g), .margin_label = NULL,
    .id = if (case$role == "id") case$name else NULL, .sort = case$sort
  )
  if (case$transform == "filter") result <- filter(result, TRUE)
  if (case$transform == "head") result <- head(result, 4L)
  expected <- collect(result)
  destination <- switch(case$destination,
    same = "facts", case = "FACTS",
    qualified = DBI::Id(schema = "temp", table = "facts"), different = "saved"
  )
  computed <- tryCatch(compute(
    result, name = destination, temporary = TRUE,
    overwrite = case$existing_temp, analyze = FALSE
  ), error = identity)
  error <- if (inherits(computed, "error")) conditionMessage(computed) else ""
  actual <- if (!inherits(computed, "error")) collect(computed) else NULL
  temp <- if (DBI::dbExistsTable(con, DBI::Id(schema = "temp", table = "facts"))) {
    DBI::dbReadTable(con, DBI::Id(schema = "temp", table = "facts"), check.names = FALSE)
  } else NULL
  data.frame(
    expected_rows = nrow(expected),
    actual_rows = if (is.null(actual)) NA_integer_ else nrow(actual),
    names_match = if (is.null(actual)) NA else identical(names(actual), names(expected)),
    persistent_source_unchanged = identical(source, DBI::dbReadTable(con, DBI::Id(schema = "main", table = "facts"), check.names = FALSE)),
    temp_rows_after = if (is.null(temp)) NA_integer_ else nrow(temp),
    error = error
  )
}
observations <- lapply(seq_len(nrow(grid)), function(i) {
  result <- tryCatch(run(grid[i, ]), error = function(e) data.frame(harness_error = conditionMessage(e)))
  if (i %% 48L == 0L) cat(i, "/", nrow(grid), "fixtures completed\n")
  result
})
stopifnot(all(vapply(observations, function(x) !"harness_error" %in% names(x), logical(1))))
results <- cbind(grid, do.call(rbind, observations))
write.csv(results, output, row.names = FALSE)
print(with(results, table(transform, destination, existing_temp, actual_rows, useNA = "ifany")))
cat("CSV:", output, "\n")
stopifnot(all(results$persistent_source_unchanged))
