# SQL recording boundaries after PR #780

Investigated: 2026-10-13
Repository snapshot: `0e3f1dcca46f4d51d4d99cd200cf73540c96133d`

## Evidence and limits

PR #780 was merged at 2026-10-13 06:10:30 UTC; its merge commit matched
the inspected local HEAD. It restored construction-record ownership under
reentry. It did not implement the broader retrieval/audit request.

This investigation read recording sites and their execution paths, relevant
tests, ADRs 0020/0027/0036/0037, and the published reference/guide sections.
It also ran the SQLite probe below. This was not an execution audit of every
backend, nor proof that tracing DBI observes every driver-internal statement.
Historical investigations dated 2026-10-01 were read as evidence from that
date, not as adopted requirements or proof of present tool coverage.

## Recording inventory at the inspected commit

`R/sent-queries.R` held two columns, `purpose` and `sql`. Recording required
the option to be exactly `TRUE` at call entry and the input to be SQL.

| Work | Recording site | Timing and content |
| --- | --- | --- |
| Result query | `finalize_margin_operation()` | `result`, rendered before returning the unexecuted result |
| Zero-row selection metadata | `grouping_selection_proxy()` | `selection_proxy`, before internal `collect()`; DuckDB was the SQL backend with this capability |
| Observed Margin label collision | `check_observed_label_collision()` | `observed_label_collision`, before the requested scan's `collect()` |
| Share dialect question and numeric control | `probe_share_dialect_answer()` | `share_dialect` / `share_dialect_control`, before each internal `collect()` |
| RPostgres probe savepoint controls | `execute_share_probe_control()` | `share_dialect_transaction`, literal SQL before `dbExecute()` |

These were five static recording sites and six purpose values. Rendering was
not interception of the arguments passed to DBI. The literal control strings
were recorded before the intended invocation, without recording its outcome.

The following work had no execution record:

- Later `collect()`, including finite collection; `pull()`; display previews;
  and `compute()`. SQLite and DuckDB package-owned result methods did not
  change this boundary.
- SQLite materialization's destination metadata, savepoint, CREATE, INSERT,
  ANALYZE, release, and failure cleanup.
- Arbitrary SQL called by user callbacks, repeated binding, individual
  fetches, result cleanup, timings, counts, execution outcomes, and conditions.

`nest_with_margins()` and `nest_by_with_margins()` rejected SQL and Arrow
inputs through `assert_nest_possible()`. The latter's internal collections
therefore operated on local/dtplyr results, not on an omitted SQL nesting path.
Non-SQL calls produced no SQL rows; that did not establish absence of work.

The record began after forcing `.data`. Each independent tracked call replaced
the previous record. During A's callback, tracked B owned its own record;
after B exited, A's earlier record was restored. B's rows were not retained
as history. A failure left the exiting outer call's partial record. Neither
the condition nor the outcome was stored. Worker records were not merged.

All render errors became `sql = NA_character_` in `render_sent_query_sql()`;
their causes were not retained. Share probes suppressed warnings/messages and
classified errors internally. Their measured dialect verdict was cached;
cache reuse and internal classifications were not exposed by the SQL rows.

Source paths: `R/sent-queries.R`, `R/margin-operation.R`,
`R/backend-metadata.R`, `R/margin-label.R`, `R/share.R`,
`R/sqlite-typed-order.R`, `R/duckdb-env-expansion.R`,
`R/nest_with_margins.R`, `R/nest_by_with_margins.R`, and `R/utils.R`.
Tests read included `test-sent-queries*.R`, `test-postgres-share-probe.R`,
`test-sqlite-b-direct-compute.R`, and `test-query-policy.R`.
The query-policy catalog guarded execution entry points; it did not establish
complete SQL audit coverage.

## SQLite counterexample

Environment: R 4.6.1, DBI 1.3.0, dbplyr 2.6.0, RSQLite 3.53.3.
The following probe was run in an independent R process against an in-memory
database. The trace served only as measurement instrumentation.

```r
pkgload::load_all(".", quiet = TRUE)
local({
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  source_table <- dplyr::copy_to(
    con, data.frame(g = c("a", "b"), v = c(2, 5)), "audit_source"
  )
  old <- options(marginplyr.audit_sql = TRUE)
  on.exit(options(old), add = TRUE)
  assign("audit_probe_sql", character(), envir = .GlobalEnv)
  trace("dbSendQuery", where = asNamespace("DBI"), print = FALSE,
    tracer = quote(audit_probe_sql <<- c(audit_probe_sql, as.character(statement))))
  on.exit(untrace("dbSendQuery", where = asNamespace("DBI")), add = TRUE)
  on.exit(rm("audit_probe_sql", envir = .GlobalEnv), add = TRUE)
  query <- summarize_with_margins(
    source_table, z = sum(v, na.rm = TRUE),
    .grouping = rollup(g), .id = "sid", .sort = "last"
  )
  record <- last_sent_queries()
  print(c(construction_sends = length(audit_probe_sql), rows = nrow(record)))
  for (op in c("collect_n", "pull", "compute")) {
    assign("audit_probe_sql", character(), envir = .GlobalEnv)
    value <- switch(op,
      collect_n = dplyr::collect(query, n = 1L),
      pull = dplyr::pull(query, z),
      compute = dplyr::compute(query, name = "audit_report", temporary = FALSE)
    )
    print(list(operation = op, sends = length(audit_probe_sql),
      record_unchanged = identical(record, last_sent_queries())))
    writeLines(audit_probe_sql)
  }
})
```

The measured operations and SQL were:

| Operation | Observed `dbSendQuery()` invocations | Construction record |
| --- | --- | --- |
| Construct result | 0 | One `result` row |
| `collect(query, n = 1L)` | 1, SELECT with `LIMIT 1` | Unchanged |
| `pull(query, z)` | 1, SELECT projecting `z` | Unchanged |
| `compute(query, name = "audit_report", temporary = FALSE)` | 8: three metadata requests, SAVEPOINT, CREATE TABLE, INSERT, ANALYZE, RELEASE | Unchanged |

One metadata statement contained a `$name` placeholder. Its SQL text alone
did not describe the bound value. The count of eight was specific to this
fixture and dependency environment, not a contract for every `compute()`.

## Documentation findings

The relevant sections of the three published pages were fetched using Python
`urllib` after web-tool retrieval failed. They included #780's ownership
clarifications; no missing deployment of those sections was found. This was
a semantic section comparison, not a whole-page byte comparison.

| Source / section | Finding |
| --- | --- |
| `R/summarize_with_margins.R`, *When marginplyr queries your data* | “reports what a call actually sent” was stronger than the render-before-invocation implementation; the section was inherited by expand and both nesting references |
| `vignettes/database_backends.qmd`, *Record the SQL marginplyr sends* | “in the order it sent them” described an output containing the unexecuted `result` row |
| `R/sent-queries.R`, *What the record promises* | “Every other row is a query marginplyr sends” did not account precisely for a failed invocation after recording |
| Same, *Four answers* | `NA` was attributed to a statement having no SQL form, although the implementation caught every render error |
| `R/inspect-grouping.R`, *Formats and ordinary tibble behavior* | No source-row read was explained, but the possible zero-row SQL request and its recording were not connected there |
| ADR 0027, original text and later clarification | Original send/receipt wording remained, with the later amendment directing a narrower reading |

The references correctly stated construction-only scope, no outcomes, and
exclusion of later collection/materialization. Nevertheless, `pull()`, repeated
retrieval, transformed SQL, and condition history had no unified example.
The issue was not solved by changing the introduction alone: the desired
observation scope exceeded the implemented feature.

A related source claim that `is.data.frame()` identified absence of an external
system exactly had already been withdrawn by ADR 0020's *two of the four
pairs, and the word exact* amendment; the public query-policy explanation
still contained that claim.

Published pages read:
[record accessor](https://sayuks.github.io/marginplyr/man/last_sent_queries.html),
[database guide](https://sayuks.github.io/marginplyr/vignettes/database_backends.html),
[summary reference](https://sayuks.github.io/marginplyr/man/summarize_with_margins.html).

## Observation boundaries checked against primary sources

The [DBI retrieval flow](https://dbi.r-dbi.org/reference/dbSendQuery.html#the-data-retrieval-flow)
separated sending, binding, fetching, completion checks, and cleanup.
The [binding contract](https://dbi.r-dbi.org/reference/dbBind.html) allowed
repeated binding to one result. A statement or result identifier alone could
not count executions. A request error did not prove the server performed no
work; retrieval completion did not establish transaction commit.

Installed dbplyr 2.6.0's `pull.tbl_sql()` selected columns before collection;
finite collection could alter SQL. `db_collect.DBIConnection()` could warn
about incomplete retrieval after `dbFetch()` returned. DBI-only condition
handlers would therefore miss a warning emitted outside their boundary.
Conversely, an outer R-operation handler could not observe a condition already
handled or muffled by an inner handler. Package-owned probes would require
observation within their own handling boundary to explain their outcomes.

No verified universal interceptor, automatic result-lineage mechanism,
cross-process capture, or durable storage mechanism was established by this
investigation. An implementation choice would require backend-specific
coverage and noninterference evidence.
