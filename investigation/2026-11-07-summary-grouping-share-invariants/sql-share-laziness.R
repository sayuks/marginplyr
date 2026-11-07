Sys.setenv(TESTTHAT_PARALLEL = "false")
args <- commandArgs(trailingOnly = TRUE)
backend <- if (length(args)) args[[1L]] else "duckdb"
pkgload::load_all(".", quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))

observe <- function(backend) {
  con <- if (backend == "duckdb") {
    DBI::dbConnect(duckdb::duckdb(shared_home = FALSE), config = list(threads = "1"))
  } else {
    DBI::dbConnect(RPostgres::Postgres())
  }
  on.exit(DBI::dbDisconnect(con))
  source <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), x = c(1L, 3L)), "share_lazy_facts"
  )
  DBI::dbExecute(con, "CREATE TEMP SEQUENCE share_lazy_reads")
  currval <- function() {
    DBI::dbGetQuery(con, "SELECT currval('share_lazy_reads') AS reads")$reads
  }
  unread <- function() inherits(tryCatch(currval(), error = identity), "error")
  stopifnot(unread())
  array <- if (backend == "duckdb") "[10,20]" else "ARRAY[10,20]"
  value <- dbplyr::sql(paste0(
    "SUM(x) + 0 * nextval('share_lazy_reads') + unnest(", array, ")"
  ))
  options(marginplyr.audit_sql = TRUE)
  query <- summarize_with_margins(
    source, value = !!value, parent = share_of_parent(value), total = share_of_total(value),
    .grouping = rollup(g), .id = "set", .margin_label = NULL, .sort = "last"
  )
  sql <- as.character(dbplyr::sql_render(query))
  audit <- last_sent_queries()
  print(audit["purpose"])
  stopifnot(grepl("share_lazy_reads", sql, fixed = TRUE), unread())
  stopifnot(all(audit$purpose %in% c("result", "selection_proxy", "share_dialect", "share_dialect_control")))
  result <- dplyr::collect(query)
  stopifnot(nrow(result) == 6L, currval() > 0)
  cat(backend, "sequence unread after construction and SQL render; after collection",
      as.double(currval()), "calls for", nrow(result), "rows\n")
}

observe(backend)
