# Dated SQL admission and three-reader Arrow inspection boundary probe.
Sys.setenv(TESTTHAT_PARALLEL = "false")
pkgload::load_all(Sys.getenv("MARGINPLYR_HUNT_SOURCE", "."), quiet = TRUE)
suppressPackageStartupMessages(library(dplyr))
options(marginplyr.audit_sql = TRUE)
observe <- function(x) tryCatch(force(x), error = identity)
for (engine in c("RSQLite", "duckdb")) {
  con <- if (engine == "RSQLite") DBI::dbConnect(RSQLite::SQLite(), ":memory:") else DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  root <- copy_to(con, tibble(g = c("a", "b", NA_character_), row_id = 1:3), "frontier", temporary = TRUE)
  one <- root |> filter(row_id <= 2L)
  two <- root |> filter(row_id >= 2L)
  graphs <- list(
    join_union = left_join(root, rename(union_all(one, two), row2 = row_id), by = "g"),
    union_join = union_all(inner_join(root, rename(one, row2 = row_id), by = "g"), inner_join(root, rename(two, row2 = row_id), by = "g")),
    double_join = left_join(left_join(root, rename(one, row2 = row_id), by = "g"), rename(two, row3 = row_id), by = "g"),
    nested_limits = head(arrange(head(arrange(root, desc(row_id)), 2L), row_id), 1L),
    grouped_window = root |> group_by(g) |> mutate(position = row_number()) |> ungroup(),
    projected_join = select(left_join(root, rename(one, row2 = row_id), by = "g"), g, row_id)
  )
  statements <- character()
  trace("dbSendQuery", where = asNamespace("DBI"), print = FALSE,
        tracer = quote({ statements <<- c(statements, as.character(statement)) }))
  trace("dbSendStatement", where = asNamespace("DBI"), print = FALSE,
        tracer = quote({ statements <<- c(statements, as.character(statement)) }))
  calls <- 0L
  for (q in graphs) {
    for (verb in list(nest_with_margins, nest_by_with_margins)) {
      forced <- FALSE
      before <- length(statements)
      answer <- observe(verb(q, .grouping = {forced <- TRUE; stop("unforced specification")}, .check_margin_label = TRUE))
      stopifnot(inherits(answer, "marginplyr_error"), !forced,
                length(statements) == before, nrow(last_sent_queries()) == 0L)
      calls <- calls + 1L
    }
    for (invalid in list(function(x) inspect_grouping(x, .format = "bad"), function(x) inspect_grouping(x, .grouping = grouping_set(all_of("missing"))))) {
      before <- length(statements)
      answer <- observe(invalid(q))
      stopifnot(inherits(answer, "error"), length(statements) == before)
      calls <- calls + 1L
    }
    for (typed in c(FALSE, TRUE)) {
      before <- length(statements)
      plan <- observe(if (typed) inspect_grouping(q, .grouping = rollup(where(is.character)), .format = "list") else inspect_grouping(q, .grouping = rollup(g), .format = "list"))
      if (typed && engine == "RSQLite") stopifnot(inherits(plan, "error"), grepl("doesn.t report column types", conditionMessage(plan)), length(statements) == before) else stopifnot(identical(plan$included, list("g", character())), length(statements) - before == as.integer(engine == "duckdb"))
      calls <- calls + 1L
    }
  }
  untrace("dbSendStatement", where = asNamespace("DBI"))
  untrace("dbSendQuery", where = asNamespace("DBI"))
  stopifnot(all(grepl("LIMIT 0", statements, fixed = TRUE)))
  cat(engine, calls, "boundary calls;", length(statements), "statements; all LIMIT 0\n")
  DBI::dbDisconnect(con, shutdown = identical(engine, "duckdb"))
}
calls <- readers <- rows <- 0L
for (shape in c("join_union", "union_join", "double_join", "hidden_join", "semi_union", "anti_join")) {
  input <- tibble(g = c("a", "b"), row_id = 1:2)
  roots <- lapply(seq_len(3L), function(i) arrow::RecordBatchReader$create(arrow::RecordBatch$create(input)))
  qs <- lapply(roots, function(x) select(x, everything()))
  a <- qs[[1L]]; b <- qs[[2L]]; cc <- qs[[3L]]
  q <- switch(shape,
    join_union = inner_join(a, union_all(b, cc), by = c("g", "row_id")),
    union_join = union_all(inner_join(a, b, by = c("g", "row_id")), cc),
    double_join = inner_join(inner_join(a, b, by = c("g", "row_id")), cc, by = c("g", "row_id")),
    hidden_join = select(inner_join(inner_join(a, rename(b, row2 = row_id), by = "g"), rename(cc, row3 = row_id), by = "g"), g, row_id),
    semi_union = semi_join(a, union_all(b, cc), by = c("g", "row_id")),
    anti_join = anti_join(a, inner_join(b, cc, by = c("g", "row_id")), by = c("g", "row_id")))
  for (verb in list(nest_with_margins, nest_by_with_margins)) {
    forced <- FALSE
    answer <- observe(verb(q, .by = {forced <- TRUE; stop("unforced fixed keys")}))
    stopifnot(inherits(answer, "marginplyr_error"), !forced, nrow(last_sent_queries()) == 0L)
    calls <- calls + 1L
  }
  for (typed in c(FALSE, TRUE)) {
    plan <- if (typed) inspect_grouping(q, .grouping = rollup(where(is.character)), .format = "list") else inspect_grouping(q, .grouping = rollup(g), .format = "list")
    stopifnot(identical(plan$included, list("g", character())), nrow(last_sent_queries()) == 0L)
    calls <- calls + 1L
  }
  for (reader in roots) {
    stopifnot(identical(as_tibble(as.data.frame(reader)), input))
    readers <- readers + 1L; rows <- rows + nrow(input)
  }
}
cat("Arrow", calls, "boundary calls;", readers, "readers retained all", rows, "source rows\n")
cat("Baseline: 78ebbc36200bdc5cef70838dd4e772c5c4a1401c\n")
for (p in c("DBI", "RSQLite", "duckdb", "arrow")) cat(p, as.character(packageVersion(p)), "\n")
