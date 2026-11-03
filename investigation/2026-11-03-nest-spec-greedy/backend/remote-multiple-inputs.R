# Dated safety probes: SQL refusals/metadata and multiple one-shot Arrow roots.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", getwd()), quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
options(marginplyr.audit_sql = TRUE)
observe <- function(expr) tryCatch(force(expr), error = identity)
for (engine in c("RSQLite", "duckdb")) {
  con <- if (engine == "RSQLite") DBI::dbConnect(RSQLite::SQLite(), ":memory:") else
    DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  root <- copy_to(con, tibble(g = c("b", "a", NA_character_), row = 1:3),
                  "multi_input_boundary", temporary = TRUE)
  graphs <- list(
    coalesced = mutate(root, h = coalesce(g, "missing")),
    cross = cross_join(root, rename(root, h = g, row2 = row)),
    anti = anti_join(root, filter(root, row > 2L), by = c("g", "row")),
    window_limit = head(mutate(arrange(root, desc(row)), position = row_number()), 1L),
    branched = union_all(filter(root, row < 3L), filter(root, row > 1L)))
  statements <- character()
  trace("dbSendQuery", where = asNamespace("DBI"), print = FALSE,
        tracer = quote({ statements <<- c(statements, as.character(statement)) }))
  calls <- 0L
  for (q in graphs) {
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      forced <- FALSE
      before <- length(statements)
      result <- observe(verb(q, .grouping = {forced <- TRUE; stop("must remain unforced")}))
      stopifnot(inherits(result, "marginplyr_error"), !forced,
                length(statements) == before, nrow(last_sent_queries()) == 0L)
      calls <- calls + 1L
    }
    for (invalid in list(function(x) inspect_grouping(x, .format = "invalid"),
                         function(x) inspect_grouping(x, .grouping = grouping_set(all_of("absent"))))) {
      before <- length(statements)
      result <- observe(invalid(q))
      stopifnot(inherits(result, "error"), length(statements) == before)
      calls <- calls + 1L
    }
    before <- length(statements)
    plan <- inspect_grouping(q, .grouping = grouping_set(everything()), .format = "list")
    stopifnot(identical(plan$included, list(colnames(q))),
              length(statements) - before == as.integer(engine == "duckdb"))
    calls <- calls + 1L
  }
  untrace("dbSendQuery", where = asNamespace("DBI"))
  stopifnot(all(grepl("LIMIT 0", statements, fixed = TRUE)))
  cat(engine, calls, "calls;", length(statements), "statements, all LIMIT 0\n")
  DBI::dbDisconnect(con, shutdown = identical(engine, "duckdb"))
}
checks <- 0L
for (shape in c("union", "join", "reverse_join", "hidden_right")) {
  input <- tibble(g = c("a", "b"), row = 1:2)
  left_reader <- arrow::RecordBatchReader$create(arrow::RecordBatch$create(input))
  right_reader <- arrow::RecordBatchReader$create(arrow::RecordBatch$create(input))
  left <- select(left_reader, everything())
  right <- select(right_reader, everything())
  q <- switch(shape, union = union_all(left, right),
              join = inner_join(left, right, by = c("g", "row")),
              reverse_join = inner_join(right, left, by = c("g", "row")),
              hidden_right = select(inner_join(left, rename(right, other = row), by = "g"), g, row))
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    forced <- FALSE
    result <- observe(verb(q, .by = {forced <- TRUE; stop("must remain unforced")}))
    stopifnot(inherits(result, "marginplyr_error"), !forced, nrow(last_sent_queries()) == 0L)
    checks <- checks + 1L
  }
  for (typed in c(FALSE, TRUE)) {
    plan <- if (typed) inspect_grouping(q, .grouping = rollup(where(is.character)), .format = "list") else
      inspect_grouping(q, .grouping = rollup(g), .format = "list")
    stopifnot(identical(plan$included, list("g", character())), nrow(last_sent_queries()) == 0L)
    checks <- checks + 1L
  }
  stopifnot(identical(as_tibble(as.data.frame(left_reader)), input),
            identical(as_tibble(as.data.frame(right_reader)), input))
}
cat("Arrow", checks, "calls; 8 readers retained all 16 original source rows\n")
cat("Baseline: 4b34fdecb2d8a41034b3d9b5d5af8e51592555e1\n")
for (package in c("DBI", "RSQLite", "duckdb", "arrow")) {
  cat(package, as.character(utils::packageVersion(package)), "\n")
}
