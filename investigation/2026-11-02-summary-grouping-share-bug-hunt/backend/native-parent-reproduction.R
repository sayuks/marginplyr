# Investigation signal; --assert fails on the documented contract violation.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
run <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  total <- "--total" %in% commandArgs(TRUE)
  data <- if (total) tibble(k = "east", g = "a") else tibble(g = "a", h = "x")
  source <- copy_to(con, data, "parent_source")
  DBI::dbExecute(con, "CREATE SEQUENCE parent_stamp")
  dots <- list(value = dbplyr::sql("nextval('parent_stamp')"),
    fraction = if (total) quote(share_of_total(value)) else quote(share_of_parent(value)))
  query <- summarize_with_margins(source, !!!dots,
    .by = if (total) "k" else NULL,
    .grouping = if (total) rollup(g) else rollup(g, h), .id = "set")
  before <- tryCatch(DBI::dbGetQuery(con, "SELECT currval('parent_stamp')"), error = identity)
  stopifnot(inherits(before, "error"))
  sql <- as.character(dbplyr::sql_render(query))
  stopifnot(grepl("GROUPING SETS", sql, fixed = TRUE))
  result <- collect(query)
  ids <- if (total) 1:2 else 1:3
  values <- result$value[match(ids, result$set)]
  expected <- if (total) c(values[[1L]] / values[[2L]], 1) else
    c(values[[1L]] / values[[2L]], values[[2L]] / values[[3L]], 1)
  expected <- expected[match(result$set, ids)]
  print(result)
  cat("Expected fractions by displayed parent values:\n")
  print(expected)
  cat("Generated SQL:\n", sql, "\n")
  stopifnot(!isTRUE(all.equal(result$fraction, expected)))
  if ("--assert" %in% commandArgs(TRUE)) stopifnot(isTRUE(all.equal(result$fraction, expected)))
}
run()
