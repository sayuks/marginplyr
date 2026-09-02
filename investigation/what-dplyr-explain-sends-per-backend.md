# What dplyr::explain() sends, per backend

Investigated: 2026-09-02

Prompted by #379, which is #377's research ticket for the `"explain"` capture
method #318 proposes. #318 argues that `explain()` "sends a real statement to
the backend — no data rows come back (it's a plan, not a result), so it's still
a schema-shaped operation rather than a data read". ADR 0020 enumerates its
exemptions rather than deriving them from a shape, so this note records the
facts under that claim and stops there. The decision is #382's.

Measured on R 4.6.1 (aarch64-apple-darwin23) with dplyr 1.2.1, dbplyr 2.6.0,
DBI 1.3.0, RSQLite 3.53.3 (SQLite library 3.53.3), duckdb 1.5.5 (DuckDB library
v1.5.5), dtplyr 1.3.3, arrow 25.0.1. Source was read from the installed
libraries and from `tidyverse/dplyr` at d5e94e7, `tidyverse/dbplyr` at f478e20,
`r-dbi/bigrquery` at fdb0f1e, and `sparklyr/sparklyr` at 7f006a6. Scripts are
throwaway; every command is quoted so each result can be reproduced.

## 1. What `explain()` dispatches to

`dplyr::explain()` is a generic with no default method
(`R/explain.R` in dplyr):

```r
explain <- function(x, ...) {
  UseMethod("explain")
}
```

With dplyr, dbplyr, dtplyr and arrow all attached, the methods present are:

```r
methods("explain")
#> [1] explain.arrow_dplyr_query* explain.ArrowTabular*
#> [3] explain.Dataset*           explain.RecordBatchReader*
#> [5] explain.tbl_sql*
```

There is no `explain.dtplyr_step`, no `explain.tbl_lazy`, and no
`explain.data.frame`. A local data frame raises, a dtplyr step raises, and so
does a `tbl_lazy` that is not a `tbl_sql`:

```r
dt <- lazy_dt(data.table(x = 1:3, y = c("a","b","c")))
dplyr::explain(dt |> filter(x > 1) |> summarise(n = .N))
#> Error in UseMethod("explain") : no applicable method for 'explain' applied
#>   to an object of class "c('dtplyr_step_subset', 'dtplyr_step')"

dplyr::explain(data.frame(x = 1))
#> Error in UseMethod("explain") : no applicable method for 'explain' applied
#>   to an object of class "data.frame"

lf <- dbplyr::lazy_frame(g = "a", v = 1)
class(lf)
#> [1] "tbl_TestConnection" "tbl_lazy"           "tbl"
dplyr::explain(lf)
#> Error in UseMethod("explain") : no applicable method for 'explain' applied
#>   to an object of class "c('tbl_TestConnection', 'tbl_lazy', 'tbl')"
```

The last one is the asymmetry worth naming: `show_query()` has a `tbl_lazy`
method, `explain()` does not. Every `dbplyr::simulate_*()` connection produces
a `tbl_lazy` that is not a `tbl_sql`, so `explain()` raises on the whole
simulator path — which is how marginplyr verifies Snowflake, Spark SQL, Oracle,
SQL Server and the rest (`R/summarize_with_margins.R`, *Database backend
coverage*).

```r
methods("show_query")
#> [1] show_query.arrow_dplyr_query* show_query.ArrowTabular*
#> [3] show_query.Dataset*           show_query.dtplyr_step*
#> [5] show_query.RecordBatchReader* show_query.tbl_lazy*
```

### Arrow: client-side, no SQL

`arrow:::explain.arrow_dplyr_query`, `explain.ArrowTabular` and
`explain.Dataset` are all one line:

```r
function (x, ...) { show_exec_plan(x) }
```

and `arrow:::show_exec_plan` builds an Acero plan in this process and prints it:

```r
function (x) {
    result <- as_record_batch_reader(as_adq(x))
    plan <- result$Plan()
    on.exit({ plan$.unsafe_delete(); result$.unsafe_delete() })
    cat(plan$ToString())
    invisible(x)
}
```

No SQL is generated and no statement is sent. The printed form carries no
`<SQL>`/`<PLAN>` markers at all — it is a different shape from the dbplyr one:

```r
tb <- arrow::arrow_table(g = c("a","b"), v = c(1,2))
utils::capture.output(dplyr::explain(tb |> group_by(g) |> summarise(n = n())))
#> [1] "ExecPlan with 4 nodes:"
#> [2] "3:SinkNode{}"
#> [3] "  2:GroupByNode{keys=[\"g\"], aggregates=["
#> [4] "  \thash_count_all(*),"
#> [5] "  ]}"
#> [6] "    1:ProjectNode{projection=[g]}"
#> [7] "      0:TableSourceNode{}"
```

On a `FileSystemDataset` the only difference measured was the source node:
`0:SourceNode{}` instead of `0:TableSourceNode{}`.

### SQL: `explain.tbl_sql` in dbplyr

```r
explain.tbl_sql <- function(x, ...) {
  force(x)
  show_query(x)
  cat_line()
  cat_line("<PLAN>")
  cat_line(remote_query_plan(x, ...))

  invisible(x)
}
```

`show_query()` renders client-side and sends nothing. The round trip is
`remote_query_plan()`:

```r
remote_query_plan <- function(x, ...) {
  dbplyr_explain(remote_con(x), db_sql_render(remote_con(x), x$lazy_query), ...)
}

dbplyr_explain <- function(con, sql, ...) {
  check_2ed(con)

  sql <- sql_query_explain(con, sql, ...)
  n <- length(sql)
  for (i in seq_len(n - 1)) {
    db_execute(con, sql[[i]], "Can't explain query.")
  }
  expl <- db_get_query(con, sql[[n]], "Can't explain query.")

  out <- utils::capture.output(print(expl))
  paste(out, collapse = "\n")
}
```

Three things follow from that body and matter later. `db_get_query()` is
`DBI::dbGetQuery()` wrapped in an error handler that appends `Using SQL: <sql>`
to a `Can't explain query.` abort. A dialect whose `sql_query_explain()` returns
more than one statement — Oracle is the only one shipped — has every statement
but the last sent through `db_execute()`, i.e. `DBI::dbExecute()`. And the plan
text is not the backend's own text: it is `print()` on the returned data frame,
captured.

## 2. What statement `EXPLAIN` becomes, per dialect

`sql_query_explain()` dispatches on `sql_dialect(con)`, and
`sql_dialect.default` returns the connection unchanged, so a connection whose
package registers no method reaches the `DBIConnection` default:

```r
sql_query_explain.DBIConnection <- function(con, sql, ...) {
  sql_glue2(con, "EXPLAIN {sql}")
}
```

`sql_query_explain()` itself renders without sending, so it can be called on a
simulated connection even though `explain()` cannot (§1). Against every
`dbplyr::simulate_*()` connection in dbplyr 2.6.0, for
`SELECT g, SUM(v) AS v FROM d GROUP BY g`:

```
postgres   EXPLAIN (FORMAT 'text') SELECT g, SUM(v) AS v FROM d GROUP BY g
spark_sql  EXPLAIN SELECT g, SUM(v) AS v FROM d GROUP BY g
dbi        EXPLAIN SELECT g, SUM(v) AS v FROM d GROUP BY g
mysql      EXPLAIN SELECT g, SUM(v) AS v FROM d GROUP BY g
odbc       EXPLAIN SELECT g, SUM(v) AS v FROM d GROUP BY g
hana       EXPLAIN SELECT g, SUM(v) AS v FROM d GROUP BY g
oracle     EXPLAIN PLAN FOR SELECT g, SUM(v) AS v FROM d GROUP BY g
           ||  SELECT PLAN_TABLE_OUTPUT FROM TABLE(DBMS_XPLAN.DISPLAY())
mariadb    EXPLAIN SELECT g, SUM(v) AS v FROM d GROUP BY g
teradata   EXPLAIN SELECT g, SUM(v) AS v FROM d GROUP BY g
access     EXPLAIN SELECT g, SUM(v) AS v FROM d GROUP BY g
sqlite     EXPLAIN QUERY PLAN SELECT g, SUM(v) AS v FROM d GROUP BY g
redshift   EXPLAIN SELECT g, SUM(v) AS v FROM d GROUP BY g
snowflake  EXPLAIN SELECT g, SUM(v) AS v FROM d GROUP BY g
hive       EXPLAIN SELECT g, SUM(v) AS v FROM d GROUP BY g
impala     EXPLAIN SELECT g, SUM(v) AS v FROM d GROUP BY g
mssql      EXPLAIN SELECT g, SUM(v) AS v FROM d GROUP BY g
```

Only four dialects override the default: postgres, sqlite, oracle and redshift.
`sparklyr` adds a fifth from its own side,
`sql_query_explain.spark_connection`, which is
`build_sql("EXPLAIN ", sql, con = con)` (`R/spark_sql.R`) — the same text as the
default. `bigrquery` registers none: grepping its `R/` for `explain` and
`EXPLAIN` returns nothing, so a `BigQueryConnection` gets `EXPLAIN {sql}` too.

The statement embeds the caller's query verbatim, table names and literals
included. Traced on SQLite:

```r
trace(DBI::dbGetQuery, tracer = quote(message("[sent] ", as.character(statement))))
dplyr::explain(tbl(con, "d") |> filter(g == "secret-value"))
#> [sent] SELECT *
#> FROM `d` AS `q01`
#> WHERE (0 = 1)
#> [sent] EXPLAIN QUERY PLAN SELECT *
#> FROM `d`
#> WHERE (`g` = 'secret-value')
```

(The first statement is `dplyr::tbl()`'s own zero-row read, not `explain()`'s.)

### What was measured

Both engines were given `tbl(con, "d") |> group_by(g) |> summarise(n = n())`.
`explain()` sent exactly one statement on each, and no other.

SQLite:

```
[dbGetQuery] EXPLAIN QUERY PLAN SELECT `g`, COUNT(*) AS `n`
FROM `d`
GROUP BY `g`
```

DuckDB:

```
[dbGetQuery] EXPLAIN SELECT g, COUNT(*) AS n
FROM d
GROUP BY g
```

### What the vendors say

All pages fetched 2026-09-02.

**SQLite** — the statement is prepared as usual and the listing is returned in
place of the run:

> An SQL statement can be preceded by the keyword "EXPLAIN" or by the phrase
> "EXPLAIN QUERY PLAN". Either modification causes the SQL statement to behave
> as a query and to return information about how the SQL statement would have
> operated if the EXPLAIN keyword or phrase had been omitted.

— *EXPLAIN*, https://www.sqlite.org/lang_explain.html, §2. Its §2.1 is headed
*EXPLAIN operates at run-time, not at prepare-time*:

> The EXPLAIN and EXPLAIN QUERY PLAN prefixes affect the behavior of running a
> prepared statement using sqlite3_step(). The process of generating a new
> prepared statement using sqlite3_prepare() or similar is (mostly) unaffected
> by EXPLAIN.

**DuckDB** — stated outright, twice:

> Note that the query is not actually executed – therefore, we can only see the
> estimated cardinality (EC) for each operator, which is calculated by using the
> statistics of the base tables and applying heuristics for each operator.

— *EXPLAIN: Inspect Query Plans*,
https://duckdb.org/docs/current/guides/meta/explain

> To see the query plan of a query without executing it, run: `EXPLAIN query;`
> The output of EXPLAIN contains the estimated cardinalities for each operator.

— *Profiling Queries*,
https://duckdb.org/docs/current/sql/statements/profiling. `EXPLAIN ANALYZE`
"runs the query" (same page), and dbplyr never generates it.

The clause "by using the statistics of the base tables" is the vendor's own
account of the `~200,000 rows` measured in §4.

**PostgreSQL** — the reference page states the converse rather than the
proposition. It never says a plain `EXPLAIN` does not execute; it says `ANALYZE`
is what makes it execute:

> The ANALYZE option causes the statement to be actually executed, not only
> planned.

— *EXPLAIN*, https://www.postgresql.org/docs/current/sql-explain.html. The
contrast is drawn on the chapter page:

> With this option, EXPLAIN actually executes the query, and then displays the
> true row counts and true run time accumulated within each plan node, along
> with the same estimates that a plain EXPLAIN shows.

— *14.1. Using EXPLAIN*,
https://www.postgresql.org/docs/current/using-explain.html, §14.1.2. Planning
reads statistics, not rows: "In order to allow the PostgreSQL query planner to
make reasonably informed decisions when optimizing queries, the pg_statistic
data should be up-to-date for all tables used in the query"
(sql-explain.html, *Notes*).

**Snowflake** — the only one of the six whose documentation addresses cost
directly:

> EXPLAIN compiles the SQL statement, but does not execute it, so EXPLAIN does
> not require a running warehouse.

> Although EXPLAIN does not consume any compute credits, the compilation of the
> query does consume Cloud Service credits, just as other metadata operations
> do.

— *EXPLAIN*, https://docs.snowflake.com/en/sql-reference/sql/explain,
*Usage notes*. So it is not free, and it is not stated to be free: cloud
services credits are billed above a daily threshold —

> Usage for cloud services is charged only if the daily consumption of cloud
> services exceeds 10% of the daily usage of virtual warehouses.

— *Understanding compute cost*,
https://docs.snowflake.com/en/user-guide/cost-understanding-compute.

Snowflake's plan carries byte counts derived from the caller's data:
`partitionsAssigned` is "the number of partitions from the referenced object
that are left after compile-time pruning", `bytesAssigned` "the number of bytes
contained in the partitionsAssigned", and both are "upper bound estimates for
query execution" (*EXPLAIN*, *Output* and *Usage notes*). The plan is also
warehouse-dependent: "The EXPLAIN plan might differ depending on the size of the
current warehouse. If you run EXPLAIN outside of a current warehouse, Snowflake
constructs the EXPLAIN plan based on the capacity of an XSMALL warehouse."

**Spark SQL** — the documentation says neither that `EXPLAIN` executes nor that
it does not:

> The EXPLAIN statement is used to provide logical/physical plans for an input
> statement. By default, this clause provides information about a physical plan
> only.

— *EXPLAIN*,
https://spark.apache.org/docs/latest/sql-ref-syntax-qry-explain.html. The page
carries no execution claim in either direction, and no equivalent of Postgres's
`ANALYZE` warning or DuckDB's "not actually executed". What it does document is
that analysis happens — `EXTENDED` yields an "analyzed logical plan" which
"translates unresolvedAttribute and unresolvedRelation into fully typed
objects" — so catalog and schema resolution occur. `EXPLAIN COST` reports
statistics where they exist: "COST — If plan node statistics are available,
generates a logical plan and the statistics." dbplyr and sparklyr both send the
unqualified form, so no mode is selected.

**BigQuery** — GoogleSQL has no `EXPLAIN` statement. The clearest vendor
statement is in the Teradata migration guide, whose `EXPLAIN ...` row reads:

> EXPLAIN ...
> Not used in BigQuery.
> Similar features are the query plan explanation in the BigQuery web UI and the
> slot allocation visible in the INFORMATION_SCHEMA views and in audit logging
> in Cloud Monitoring.

— *Teradata SQL translation guide*,
https://cloud.google.com/bigquery/docs/migration/teradata-sql. *Introduction to
SQL in BigQuery* (https://cloud.google.com/bigquery/docs/introduction-sql)
enumerates the supported statement categories — query, procedural, DDL, DML,
DCL, TCL, load and export — and `EXPLAIN` is in none of them; it is also absent
from the reserved-keyword list on
https://cloud.google.com/bigquery/docs/reference/standard-sql/lexical. The
feature that exists instead is not a statement:

> Embedded within query jobs, BigQuery includes diagnostic query plan and timing
> information. This is similar to the information provided by statements such as
> EXPLAIN in other database and analytical systems. This information can be
> retrieved from the API responses of methods such as jobs.get.

— *Query plan and timeline*,
https://cloud.google.com/bigquery/docs/query-plan-explanation.

The question #379 asks — whether there is a statement about `EXPLAIN` matching
the one `investigation/query-cost-across-lazy-backends.md` records for
`LIMIT 0` — therefore has no subject on BigQuery. There is nothing to exempt.
What BigQuery documents for the preflight purpose is the dry run, and it does
carry the exemption:

> Dry runs don't use query slots, and you are not charged for performing a dry
> run.

— *Run a query*, https://cloud.google.com/bigquery/docs/running-queries. The
same section names its limit for external data: "A dry run of a federated query
that uses an external data source might report a lower bound of 0 bytes of
data, even if rows are returned." A dry run is reached through
`JobConfiguration.dryRun` or `bq query --dry_run`, neither of which dbplyr's
`explain()` path can produce.

This was searched for and not found, and the absence is the finding. Twelve
Google Cloud pages were read in full and grepped case-insensitively for
`explain` — the GoogleSQL query-syntax, DDL, DML, other-statements and lexical
references, *Introduction to SQL in BigQuery*, *Overview of BigQuery analytics*,
*Run a query*, *Estimate and control costs*, *Get query performance insights*,
and the Snowflake and Teradata translation guides. Only the two named above
mention it, and no page documents an `EXPLAIN` statement or its billing.
`EXPLAIN ANALYZE` was searched for on `cloud.google.com` and not found either;
`ML.EXPLAIN_PREDICT`, `ML.EXPLAIN_FORECAST` and `ML.GLOBAL_EXPLAIN` exist but
are BigQuery ML table-valued functions that run as ordinary queries.

## 3. Return value

dplyr's `explain()` documents `@return The first argument, invisibly.`
(`man/explain.Rd`, *Value*: "The first argument, invisibly."), and the
behaviour was confirmed:

```r
res <- dplyr::explain(q)
identical(res, q)
#> [1] TRUE
withVisible(dplyr::explain(q))$visible
#> [1] FALSE
```

It prints with `cat()`, not `message()` — dbplyr's NEWS records the change under
*dbplyr 1.4.0*: "`show_query()` and `explain()` use `cat()` rather than
message." and, under *dbplyr 1.0.0*: "`explain()` and `show_query()` now
invisibly return the first argument, making them easier to use inside a
pipeline." So `utils::capture.output()` does capture it, with no
`type = "message"`:

```r
utils::capture.output(dplyr::explain(q))
#> [1] "<SQL>"
#> [2] "SELECT `g`, COUNT(*) AS `n`"
#> [3] "FROM `d`"
#> [4] "GROUP BY `g`"
#> [5] ""
#> [6] "<PLAN>"
#> [7] "  id parent notused                       detail"
#> [8] "1  6      0     216                       SCAN d"
#> [9] "2  8      0       0 USE TEMP B-TREE FOR GROUP BY"
```

#318's premise is therefore right about `explain()` and wrong about the plan.
dbplyr exports `remote_query_plan()`, documented as giving "the query plan (as
computed by the remote database)" with `x` "currently must be a `tbl_sql`"
(`man/remote_name.Rd`), and it *returns* the plan without printing:

```r
p <- dbplyr::remote_query_plan(q)
class(p); length(p)
#> [1] "character"
#> [1] 1
```

`remote_query()` is its client-side counterpart and returns the SQL with no
round trip. Neither has an arrow or a dtplyr equivalent.

`explain()`'s `...` reaches `sql_query_explain()`, which calls
`check_dots_used()`, so the only extra argument any shipped dialect accepts is
Postgres's `format`:

```r
dbplyr::sql_query_explain(simulate_postgres(), sql("SELECT 1"), format = "json")
#> EXPLAIN (FORMAT 'json') SELECT 1
dbplyr::sql_query_explain(simulate_postgres(), sql("SELECT 1"), analyze = TRUE)
#> Error: Arguments in `...` must be used.
#> x Problematic argument:
#> * analyze = TRUE
```

No documented argument turns `explain()` into `EXPLAIN ANALYZE`.

## 4. Stability of the printed output

The two markers `<SQL>` and `<PLAN>` are literals in `explain.tbl_sql`, so they
are invariant across every SQL backend and every dbplyr version that has that
body. They are also the only invariant part. Nothing below them is:

**The plan text is a printed data frame, so it depends on `getOption("width")`.**
`dbplyr_explain()` renders it with `utils::capture.output(print(expl))`:

```r
options(width = 80); cat(dbplyr::remote_query_plan(q))
#>   id parent notused                       detail
#> 1  6      0     216                       SCAN d
#> 2  8      0       0 USE TEMP B-TREE FOR GROUP BY

options(width = 30); cat(dbplyr::remote_query_plan(q))
#>   id parent notused
#> 1  6      0     216
#> 2  8      0       0
#>                         detail
#> 1                       SCAN d
#> 2 USE TEMP B-TREE FOR GROUP BY
```

**It depends on the caller's schema.** The same SQLite query before and after
`CREATE INDEX idx_g ON d(g)`:

```
<PLAN>
  id parent notused detail
1  2      0     216 SCAN d
```

```
<PLAN>
  id parent notused                           detail
1  3      0      63 SEARCH d USING INDEX idx_g (g=?)
```

**It depends on the caller's data.** DuckDB's plan carries cardinality
estimates. The same query text against a 3-row and a 200,000-row table:

```
[15] small: │           ~1 row          │
     big  : │          ~5 rows          │
[30] small: │          ~3 rows          │
     big  : │       ~200,000 rows       │
[38] small: │          ~3 rows          │
     big  : │       ~200,000 rows       │
[47] small: │          ~3 rows          │
     big  : │       ~200,000 rows       │
```

Those numbers come from statistics DuckDB reads while planning. Over a Parquet
file the estimate is the file's exact row count, which means the footer was
read:

```r
arrow::write_parquet(data.frame(g = ..., v = ...), pq)   # 12345 rows
tb <- tbl(con, sql(sprintf("SELECT * FROM read_parquet('%s')", pq)))
dplyr::explain(tb |> group_by(g) |> summarise(n = n(), .groups = "drop"))
#> │        ~7,803 rows        │
#> │        ~12,345 rows       │
#> │        READ_PARQUET       │
#> │        READ_PARQUET       │
#> │        ~12,345 rows       │
```

**It depends on a session setting the caller controls.** DuckDB's
`explain_output`:

```r
DBI::dbExecute(con, "SET explain_output = 'physical_only'")   # 20 lines
DBI::dbExecute(con, "SET explain_output = 'optimized_only'")  # 29 lines
DBI::dbExecute(con, "SET explain_output = 'all'")             # 62 lines
```

DuckDB documents the three values and names `physical_only` "The default
setting" (*EXPLAIN: Inspect Query Plans*,
https://duckdb.org/docs/current/guides/meta/explain).

**And it is drawn with box-drawing characters on DuckDB.** For the same
aggregate over the same three-row table, the block under `<PLAN>` ran to 3 lines
on SQLite and 42 on DuckDB, the latter a stack of `┌─┴─┐` frames. The two
backends share no line but `<SQL>` and `<PLAN>`.

**What the vendors say about the format.** SQLite states the strongest position
of the six, and states it twice:

> The output from EXPLAIN and EXPLAIN QUERY PLAN is intended for interactive
> analysis and troubleshooting only. The details of the output format are
> subject to change from one release of SQLite to the next. Applications should
> not use EXPLAIN or EXPLAIN QUERY PLAN since their exact behavior is variable
> and only partially documented.

— *EXPLAIN*, https://www.sqlite.org/lang_explain.html, §2.

> Warning: The data returned by the EXPLAIN QUERY PLAN command is intended for
> interactive debugging only. The output format may change between SQLite
> releases. Applications should not depend on the output format of the EXPLAIN
> QUERY PLAN command.

> Alert: As warned above, the EXPLAIN QUERY PLAN output format did change
> substantially with the version 3.24.0 release (2018-06-04). Additional minor
> changes occurred in version 3.36.0 (2021-06-18). Further changes are possible
> in subsequent releases.

— *EXPLAIN QUERY PLAN*, https://www.sqlite.org/eqp.html, §1. The same page
documents the raw four-column shape dbplyr receives: "each node of the tree
consists of four fields: An integer node id, an integer parent id, an auxiliary
integer field that is not currently used, and a description of the node" — the
`id`, `parent`, `notused`, `detail` columns printed above.

PostgreSQL carries no "subject to change" sentence about the format, and the
closest statement is about the content:

> Also note that the numbers, and even the selected query strategy, might vary
> between PostgreSQL releases due to planner improvements.

— *EXPLAIN*, https://www.postgresql.org/docs/current/sql-explain.html,
*Examples*. It endorses machine parsing through a different route: "Non-text
output contains the same information as the text output format, but is easier
for programs to parse" (`FORMAT`, *Parameters*) — and `FORMAT` is the one
extra argument dbplyr's Postgres method exposes.

DuckDB and Spark document nothing about output-format stability in either
direction. Three DuckDB pages (`guides/meta/explain`,
`guides/meta/explain_analyze`, `sql/statements/profiling`) and Spark's
`sql-ref-syntax-qry-explain` were read in full; none contains a stability
statement. Spark documents something else that bears on it — the printed plan
is not necessarily the plan that runs:

> When true, enable adaptive query execution, which re-optimizes the query plan
> in the middle of query execution, based on accurate runtime statistics.

— *Performance Tuning*,
https://spark.apache.org/docs/latest/sql-performance-tuning.html, on
`spark.sql.adaptive.enabled`, "enabled by default since Apache Spark 3.2.0".

Snowflake documents no stability guarantee either, but offers `EXPLAIN USING
{ TABULAR | JSON | TEXT }` with `TABULAR` as the default, and names
`RESULT_SCAN` and the JSON form as the supported ways to post-process the
output (*EXPLAIN*, *Usage notes*). dbplyr sends the unqualified form, so the
default applies.

## 5. Does `EXPLAIN` ever fail where the query would succeed?

Yes, and the clearest case is a backend marginplyr's *Database backend coverage*
list does not name but dbplyr reaches: **BigQuery**. (It appears in
`R/summarize_with_margins.R` only in the cost paragraph, as the vendor whose
byte billing `LIMIT` does not reduce.) `bigrquery` registers no
`sql_query_explain` method, so `explain()` emits literal `EXPLAIN <sql>`, and
GoogleSQL has no such statement (§2). The query itself would run.

**Snowflake** documents one failure of its own, the only one on its EXPLAIN
page:

> If any of the database objects in the EXPLAIN statement are INFORMATION_SCHEMA
> objects, the statement fails with error `EXPLAIN command has insufficient
> privilege on object <objName>`.

— *EXPLAIN*, https://docs.snowflake.com/en/sql-reference/sql/explain,
*Usage notes*.

**PostgreSQL** restricts `EXPLAIN` to an enumerated list of statements — "Any
SELECT, INSERT, UPDATE, DELETE, MERGE, VALUES, EXECUTE, DECLARE, CREATE TABLE
AS, or CREATE MATERIALIZED VIEW AS statement" (*EXPLAIN*, *Parameters*) — but
dbplyr only ever renders a `SELECT`, so the restriction is not reachable from
this path. Neither PostgreSQL page states a case where `EXPLAIN` errors on a
statement the bare form would run.

**SQLite** documents a divergence rather than a failure, and it too is out of
reach from dbplyr:

> Some PRAGMA statements do their work during sqlite3_prepare() rather than
> during sqlite3_step(). Those PRAGMA statements are unaffected by EXPLAIN. […]
> For consistent results, avoid using EXPLAIN on PRAGMA statements.

— *EXPLAIN*, https://www.sqlite.org/lang_explain.html, §2.1. It also records
that "The authorizer callback is invoked regardless of the presence of EXPLAIN
or EXPLAIN QUERY PLAN".

**DuckDB and Spark document no failure case.** Spark's *Error Conditions* page
(https://spark.apache.org/docs/latest/sql-error-conditions.html) contains no
occurrence of `EXPLAIN` at all, and its EXPLAIN reference names no excluded
statement. DuckDB's three EXPLAIN pages name none either.

**Oracle is the one dialect where `explain()` writes.** Its
`sql_query_explain()` returns two statements, and `dbplyr_explain()` sends every
statement but the last through `db_execute()`, i.e. `DBI::dbExecute()`:

```
oracle  EXPLAIN PLAN FOR SELECT …  ||  SELECT PLAN_TABLE_OUTPUT FROM TABLE(DBMS_XPLAN.DISPLAY())
```

That first statement populates a `PLAN_TABLE`; the second reads it back. Every
other dialect sends one statement through `DBI::dbGetQuery()`.

### What was measured

No such case was found on SQLite or DuckDB. `remote_query_plan()` succeeded on
every marginplyr query shape tried:

```
######## SQLite ########
summarize .by                ok (146 chars of plan)
summarize rollup             ok (815 chars of plan)
summarize cube               ok (1070 chars of plan)
share_of_parent              FAILED (see below)
expand_with_margins          ok (311 chars of plan)

######## DuckDB ########
summarize .by                ok (1663 chars of plan)
summarize rollup             ok (1273 chars of plan)
summarize cube               ok (1273 chars of plan)
share_of_parent              ok (10985 chars of plan)
expand_with_margins          ok (2391 chars of plan)
```

The SQLite `share_of_parent` line is not an `EXPLAIN` failure: marginplyr
refused to build the query at all, because SQLite converts a non-numeric value
to a number rather than refusing it (ADR 0020's second exemption). No plan was
requested.

Both engines accepted `EXPLAIN` over the `UNION ALL` shape marginplyr's
portable path generates (three branches, 57 lines of output on SQLite) and over
a `WITH`-rendered form of the same query
(`sql_options(cte = TRUE)`), so neither the union nor the CTE rendering is a
problem for the statement dbplyr wraps.

## 6. What the statement references, and what comes back

The statement references the caller's table by name and carries their literals,
because it is their query with a prefix (§2). Nothing about the prefix changes
what the query names.

What comes back is a plan, and on three of the six dialects the vendor
documents that the plan carries a quantity derived from the caller's data.
DuckDB's estimated cardinality is "calculated by using the statistics of the
base tables"; Snowflake's `bytesAssigned` is "the number of bytes contained in
the partitionsAssigned", itself the count left "after compile-time pruning";
PostgreSQL's estimates come from `pg_statistic`. All three are quoted above with
their URLs, and the DuckDB one was measured: a Parquet source's estimate was the
file's exact row count.

Whether that makes `EXPLAIN` a read of the caller's data is the question ADR
0020 answers, and this note does not.

One fact about this repository's own gate is worth recording, since it was
measured rather than assumed. `explain()` reaches `DBI::dbGetQuery()` — traced
above — and `DBI::dbGetQuery` is already in `lazy_execution_entry_points()` in
`tests/testthat/test-query-policy.R`, so the runtime counter that traces that
binding sees an `explain()` call without any edit. The snapshot beside it does
not: it walks the bodies bound in marginplyr's namespace for calls to the
catalog's own names, and a body calling `explain()` names `dplyr::explain`,
which is not one of them. #377 lists whether to add it as an open question.

## 7. Where a claim could not be measured

BigQuery, Snowflake and Spark were not measured. `bigrquery`, `sparklyr` and
`odbc` are not installed in this environment, and none of the three has a
`dbplyr::simulate_*()` path that would help: `explain()` raises on every
simulated connection (§1), and dbplyr ships no BigQuery simulator at all
(`grep "^simulate_"` over dbplyr's exports lists sixteen, none of them
BigQuery). Everything recorded about those three is vendor documentation,
quoted with its URL and the date it was fetched.

PostgreSQL was not measured either. RPostgres 1.4.10 is installed but no server
was available, so what is recorded for it is the rendered statement — which is
the simulator's answer and needs no server — and the PostgreSQL documentation.
Oracle likewise: the two-statement form above is rendered, not sent.

## 8. Three dialects outside the ticket's enumerated set

#379 enumerated RSQLite, DuckDB, Postgres, BigQuery, Snowflake and Spark.
Sections 1–7 stop there. This section covers SQL Server, SAP HANA and Teradata,
which dbplyr also reaches, because §2 established that dbplyr's default is a
bare `EXPLAIN {sql}` and the question of which dialects that default is *valid*
in does not stop at the six the ticket named. Same investigation, same date;
nothing above is overturned.

None of the three was measured — no driver and no server for any of them here —
so everything below is vendor documentation, quoted with its URL and the date
it was fetched, in the form §7 already fixes for BigQuery, Snowflake and Spark.

### SQL Server: no `EXPLAIN`, except on Synapse dedicated SQL pool

Microsoft's `EXPLAIN (Transact-SQL)` topic exists, and its Applies-to banner is
a single product:

> "**Applies to:** Azure Synapse Analytics (dedicated SQL pool only)"

— *EXPLAIN (Transact-SQL)*,
<https://learn.microsoft.com/en-us/sql/t-sql/queries/explain-transact-sql?view=azure-sqldw-latest>
(fetched 2026-09-02). Its syntax is
`EXPLAIN [WITH_RECOMMENDATIONS] SQL_statement [;]`, it returns an XML document
rather than a result set of plan rows, and the same page adds "This syntax is
not supported by serverless SQL pool" and "**EXPLAIN** is not supported in a
user transaction". *T-SQL statements in dedicated SQL pool*
(<https://learn.microsoft.com/en-us/azure/synapse-analytics/sql-data-warehouse/sql-data-warehouse-reference-tsql-statements>,
fetched 2026-09-02) lists `EXPLAIN` under "Query statements"; the SQL Server
T-SQL reference has no corresponding entry.

For SQL Server, Azure SQL Database, Azure SQL Managed Instance and Fabric SQL
database the documented equivalent is a session setting issued as its own
batch, not a modifier on the statement. *SET SHOWPLAN_ALL (Transact-SQL)*
(<https://learn.microsoft.com/en-us/sql/t-sql/statements/set-showplan-all-transact-sql?view=sql-server-ver17>,
fetched 2026-09-02):

> "Causes Microsoft SQL Server not to execute Transact-SQL statements. Instead,
> SQL Server returns detailed information about how the statements would be
> executed (a query plan) [...]"

> "SET SHOWPLAN_TEXT and SET SHOWPLAN_ALL cannot be specified inside a stored
> procedure; they must be the only statements in a batch."

`SET SHOWPLAN_XML` carries the same "must be the only statement in a batch"
restriction (*SET SHOWPLAN_XML (Transact-SQL)*,
<https://learn.microsoft.com/en-us/sql/t-sql/statements/set-showplan-xml-transact-sql?view=sql-server-ver17>,
fetched 2026-09-02).

`SET STATISTICS PROFILE` is **not** an equivalent and should not be read as one:
*SET STATISTICS PROFILE (Transact-SQL)*
(<https://learn.microsoft.com/en-us/sql/t-sql/statements/set-statistics-profile-transact-sql?view=sql-server-ver17>,
fetched 2026-09-02) says "each executed query returns its regular result set,
followed by an additional result set that shows a profile of the query
execution" — an actual-plan option, which executes.

### SAP HANA: `EXPLAIN PLAN ... FOR`, and a privilege

*EXPLAIN PLAN Statement (Data Manipulation)*, SAP HANA Platform 2.0 SPS 08,
<https://help.sap.com/docs/SAP_HANA_PLATFORM/4fe29514fd584807ac9f2a04f6754767/20d9ec5575191014a251e58ecf90997a.html>
(fetched 2026-09-02), gives the syntax as

```text
EXPLAIN PLAN [ SET STATEMENT_NAME = <statement_name> ] FOR <explain_plan_entry>
```

and adds two things that matter here: "The result of the evaluation is stored in
the EXPLAIN_PLAN_TABLE view for examination" — so it returns no plan rows to the
caller — and "You must have the OPTIMIZER ADMIN system privilege". The SAP HANA
Cloud page
(<https://help.sap.com/docs/hana-cloud-database/sap-hana-cloud-sap-hana-database-sql-reference-guide/explain-plan-statement-data-manipulation>,
fetched 2026-09-02) splits it into `EXPLAIN RECOMPILED PLAN ... FOR <subquery>`
and a plan-cache form.

Whether a bare `EXPLAIN <select>` parses on HANA **could not be established from
the documentation**, and is recorded as unestablished rather than guessed. What
is established is that the reference documents no such statement: the *Data
Manipulation Statements* index
(<https://help.sap.com/docs/SAP_HANA_PLATFORM/4fe29514fd584807ac9f2a04f6754767/209eaa85751910149a30f95c936075be.html>,
fetched 2026-09-02) lists `EXPLAIN PLAN` as its only EXPLAIN entry. Confirming
the parser's behaviour needs a live server.

### Teradata: supported, and one form partially executes

*EXPLAIN Request Modifier*, Teradata Vantage SQL Data Manipulation Language,
Analytics Database 20.00, publication B035-1146-200K,
<https://docs.teradata.com/r/Enterprise_IntelliFlex_VMware/SQL-Data-Manipulation-Language/Query-and-Workload-Analysis-Statements/EXPLAIN-Request-Modifier>
(fetched 2026-09-02):

> "The Optimizer processes an explained request in the same way that the request
> would be processed without the EXPLAIN modifier, except that the SQL within
> the request is not actually executed. However, for a dynamic plan, the request
> is partially executed."

> "To EXPLAIN a request, you must have the permissions that are required to
> execute that request."

The syntax is `[ STATIC | DYNAMIC ] EXPLAIN [ IN XML [NODDLTEXT] ] SQL_request`,
with `STATIC` the default. So the bare form dbplyr would send does not execute;
`DYNAMIC EXPLAIN` does, partially.

### Retrieval note

docs.teradata.com and help.sap.com are client-rendered single-page applications,
and a plain fetch of either topic URL returns a navigation shell with none of the
text above. The quotes were retrieved from each vendor's own content API backing
those exact pages — FluidTopics at
`docs.teradata.com/api/khub/maps/.../topics/{id}/content` and
`help.sap.com/http.svc/pagecontent?deliverable_id=...&file_path=...` — both
first-party endpoints on the vendor domains. The Microsoft quotes came from
direct fetches of the Learn pages.
