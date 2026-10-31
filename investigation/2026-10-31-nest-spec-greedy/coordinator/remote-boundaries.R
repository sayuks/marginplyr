# Dated remote admission, metadata and recording observations, not package tests.
pkgload::load_all(".", quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
source("tests/testthat/helper-optional-backends.R")
old <- options(marginplyr.audit_sql = TRUE)
observe <- function(expr) tryCatch(force(expr), error = identity)
calls <- 0L
for (engine in c("RSQLite", "duckdb")) {
  stopifnot(suggest_available(engine))
  con <- if (engine == "RSQLite") DBI::dbConnect(RSQLite::SQLite(), ":memory:") else
    DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  input <- tibble(g = c("b", "a", "b"), v = c(3L, 1L, 2L))
  root <- copy_to(con, input, "remote_boundary", temporary = TRUE)
  graphs <- list(
    cross = cross_join(root, select(root, w = v)),
    anti = anti_join(root, filter(root, v == 1L), by = "v"),
    semi = semi_join(root, filter(root, v > 1L), by = "g"),
    ranked = filter(mutate(root, rank = min_rank(v)), rank <= 2L),
    empty = filter(root, FALSE)
  )
  statements <- character()
  trace("dbSendQuery", where = asNamespace("DBI"), print = FALSE,
        tracer = quote({statements <<- c(statements, as.character(statement))}))
  count <- 0L
  for (q in graphs) {
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      before <- length(statements)
      forced <- FALSE
      result <- observe(verb(q, .grouping = { forced <- TRUE; stop("selection evaluated") }))
      stopifnot(inherits(result, "marginplyr_error"), !forced,
                length(statements) == before, nrow(last_sent_queries()) == 0L)
      count <- count + 1L
    }
    invalid <- list(
      function(q) inspect_grouping(q, .format = NULL),
      function(q) inspect_grouping(q, .duplicates = "k"),
      function(q) inspect_grouping(q, .by = all_of("absent")),
      function(q) inspect_grouping(q, .grouping = rollup()),
      function(q) inspect_grouping(q, .grouping = !!rlang::call2(
        "grouping_sets", rlang::missing_arg(), rlang::sym("g")
      )),
      function(q) inspect_grouping(q, .grouping = rollup(named = g))
    )
    for (f in invalid) {
      before <- length(statements)
      result <- observe(f(q))
      stopifnot(inherits(result, "error"), length(statements) == before,
                nrow(last_sent_queries()) == 0L)
      count <- count + 1L
    }
    for (typed in c(FALSE, TRUE)) {
      before <- length(statements)
      result <- observe(if (typed) inspect_grouping(q, .grouping = grouping_set(where(is.integer)), .format = "list") else
        inspect_grouping(q, .grouping = rollup(g), .format = "list"))
      record <- last_sent_queries(details = TRUE)
      if (engine == "RSQLite" && typed) {
        stopifnot(inherits(result, "marginplyr_error"), length(statements) == before, nrow(record) == 0L)
      } else {
        stopifnot(!inherits(result, "error"), length(statements) == before + as.integer(engine == "duckdb"),
                  nrow(record) == as.integer(engine == "duckdb"))
        if (engine == "duckdb") stopifnot(all(grepl("LIMIT 0", record$sql, fixed = TRUE)))
      }
      count <- count + 1L
    }
  }
  untrace("dbSendQuery", where = asNamespace("DBI"))
  stopifnot(length(statements) == if (engine == "duckdb") 10L else 0L,
            all(grepl("LIMIT 0", statements, fixed = TRUE)))
  cat(engine, ":", count, "calls;", length(statements), "statements, all LIMIT 0; no refusal forced its specification\n")
  calls <- calls + count
  DBI::dbDisconnect(con, shutdown = identical(engine, "duckdb"))
}
stopifnot(suggest_available("arrow"))
for (mode in c("reader", "query", "union", "join", "empty")) {
  input <- tibble(g = c("b", "a", "b"), v = 1:3)
  reader <- arrow::RecordBatchReader$create(arrow::RecordBatch$create(input))
  query <- select(reader, everything())
  q <- switch(mode, reader = reader, query = query, union = union_all(query, query),
              join = inner_join(query, arrow::Table$create(input), by = c("g", "v")), empty = filter(query, v < 0L))
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    forced <- FALSE
    result <- observe(verb(q, .by = { forced <- TRUE; stop("selection evaluated") }))
    stopifnot(inherits(result, "marginplyr_error"), !forced, nrow(last_sent_queries()) == 0L)
    calls <- calls + 1L
  }
  for (typed in c(FALSE, TRUE)) {
    plan <- if (typed) inspect_grouping(q, .grouping = rollup(where(is.character)), .format = "list") else
      inspect_grouping(q, .grouping = rollup(g), .format = "list")
    stopifnot(identical(plan$included, list("g", character())), nrow(last_sent_queries()) == 0L)
    calls <- calls + 1L
  }
  stopifnot(identical(as_tibble(as.data.frame(reader)), input))
}
options(old)
cat("Arrow: 20 calls, five readers and all 15 original rows preserved; total", calls, "calls\n")
