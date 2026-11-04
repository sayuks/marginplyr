suppressPackageStartupMessages({library(dplyr); pkgload::load_all(".", quiet = TRUE)})
main <- function() {
  con <- DBI::dbConnect(RPostgres::Postgres(), host = Sys.getenv("MARGIN_CONTRACTS_PG_SOCKET"), port = 55673L, dbname = "postgres")
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "CREATE TEMPORARY SEQUENCE serial_source")
  volatile <- tbl(con, dbplyr::sql("SELECT g, v FROM (VALUES ('a',1),('b',3)) AS x(g,v)"), vars = c("g","v"))
  for (mode in c("error", "keep")) for (sort in c("first","last","none")) {
    DBI::dbExecute(con, "ALTER SEQUENCE serial_source RESTART")
    q <- summarize_with_margins(volatile, value = max(v) * 0L + !!dbplyr::sql("nextval('serial_source')"),
      p = share_of_parent(value), t = share_of_total(value), .grouping = rollup(g),
      .duplicates = mode, .sort = sort, .id = "set", .check_share_source = FALSE)
    stopifnot(!DBI::dbGetQuery(con, "SELECT is_called FROM serial_source")$is_called)
    got <- suppressWarnings(collect(q))
    vals <- as.double(got$value)
    want <- ifelse(got$set==2, 1, vals / vals[got$set==2])
    print(got)
    stopifnot(identical(got$p,want), identical(got$t,want), DBI::dbGetQuery(con, "SELECT last_value FROM serial_source")$last_value == 3L)
  }
  cat("PostgreSQL displayed-denominator comparisons passed: 6\n")
}
main()
