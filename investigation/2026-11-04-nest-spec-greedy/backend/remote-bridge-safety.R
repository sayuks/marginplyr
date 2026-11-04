Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", getwd()), quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
observe <- function(expr) tryCatch(force(expr), error = identity)
options(marginplyr.audit_sql = TRUE)
calls <- 0L
for (graph in c("root", "join", "union", "grouped", "window", "collapsed")) {
  input <- tibble(g = c("b", "a", "c", "a"), row = 1:4)
  reader <- arrow::RecordBatchReader$create(
    arrow::RecordBatch$create(input[1:2, ]), arrow::RecordBatch$create(input[3:4, ]))
  left <- select(reader, everything())
  right <- arrow::Table$create(tibble(g = c("a", "b", "c"), other = 1:3))
  q <- switch(graph, root = left,
    join = inner_join(left, right, by = "g"),
    union = union_all(left, select(right, g, row = other)),
    grouped = group_by(left, g),
    window = head(arrange(left, desc(row)), 2L),
    collapsed = collapse(left))
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  sql <- arrow::to_duckdb(q, con, table_name = paste0("bridge_", graph), auto_disconnect = FALSE)
  statements <- character()
  trace("dbSendQuery", where = asNamespace("DBI"), print = FALSE,
        tracer = quote({ statements <<- c(statements, as.character(statement)) }))
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    forced <- FALSE
    result <- observe(verb(sql, .grouping = { forced <- TRUE; stop("unforced") }))
    stopifnot(inherits(result, "marginplyr_error"), !forced, !length(statements))
    calls <- calls + 1L
  }
  for (typed in c(FALSE, TRUE)) {
    result <- if (typed) inspect_grouping(sql, .grouping = grouping_set(where(is.integer)), .format = "list") else
      inspect_grouping(sql, .grouping = grouping_set(row), .format = "list")
    stopifnot(identical(result$included, list(if (typed && graph == "join") c("row", "other") else "row")))
    calls <- calls + 1L
  }
  untrace("dbSendQuery", where = asNamespace("DBI"))
  stopifnot(length(statements) == 2L, all(grepl("LIMIT 0", statements, fixed = TRUE)))
  remaining <- as_tibble(as.data.frame(reader))
  stopifnot(identical(remaining, input))
  cat(graph, "2 nesting refusals,2 inspection plans,2 LIMIT0; reader all 4 rows retained\n")
  duckdb::duckdb_unregister_arrow(con, paste0("bridge_", graph))
  DBI::dbDisconnect(con, shutdown = TRUE)
}
cat(calls, "boundary calls completed\n")
for (package in c("DBI", "duckdb", "arrow")) cat(package, as.character(packageVersion(package)), "\n")
