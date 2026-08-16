# Schema reads vs. data reads for share-source typing

Investigated: 2026-08-08
Revised: 2026-08-16 — investigation/share-source-eligibility-on-coercing-dialects.md

This note follows up on `git show e08c3fa` (the one-row `probe_share_sources()`
read) and the handoff that reopened it. The open question was whether some
form of *schema* read — one that does not fetch a data row — can answer the
same question the one-row probe answers: the type a backend will give a share
source's summary expression (`max(region)`, `sum(x)`, `n()`, …). Tested against
in-memory RSQLite 3.53.3 (via RSQLite) and DuckDB 1.5.5 (via the `duckdb`
package, both current in the environment), DBI 1.3.0, dbplyr 2.6.0. Scripts
referenced below are throwaway and not committed; the commands to reproduce
each result are quoted inline.

## What was run

### 1. `DBI::dbSendQuery()` + `DBI::dbColumnInfo()`, never calling `dbFetch()`

Against the rendered SQL for `tb |> group_by(region) |> summarise(lab =
max(region), tot = sum(amt), cnt = n(), flag = any(amt > 1))` on a 3-row
table with an `NA` in `amt`:

```r
res <- DBI::dbSendQuery(con, as.character(sql))
info <- DBI::dbColumnInfo(res)   # no dbFetch() call anywhere
DBI::dbClearResult(res)
```

On RSQLite this returned real, non-`NA`-carrying types for every computed
column: `lab` character, `tot` double, `cnt` integer, `flag` integer. That
looked like the schema-only answer the handoff's "unverified lead" hoped for.

It is not one. Two further measurements against a 3,000,000-row table
(`SELECT SUM(x) AS tot, COUNT(*) AS cnt FROM big`) show why:

```r
system.time({ res <- dbSendQuery(con, big_q); dbFetch(res, n = -1); dbClearResult(res) })
#>    user  system elapsed
#>   0.054   0.005   0.058
system.time({ res <- dbSendQuery(con, big_q); dbColumnInfo(res); dbClearResult(res) })
#>    user  system elapsed
#>   0.048   0.004   0.052
system.time({ res <- dbSendQuery(con, big_q); dbClearResult(res) })   # neither call
#>    user  system elapsed
#>   0.047   0.004   0.052
```

`dbSendQuery()` alone — before `dbColumnInfo()` or `dbFetch()` is ever called —
already costs the same as fetching the full result. RSQLite's `dbSendQuery()`
executes the statement to completion and buffers the result immediately; nothing
about it is deferred until `dbFetch()`. `dbColumnInfo()` just reads metadata off
that already-materialized buffer.

A row-consumption check confirms the buffer is intact and complete, not merely
that timing looks similar:

```r
res <- dbSendQuery(con, "SELECT region, SUM(amt) AS tot FROM t GROUP BY region")
info <- dbColumnInfo(res)               # queried first
rows <- dbFetch(res, n = -1)             # then fetched from the same handle
nrow(rows)   # 2 — both groups, values unchanged from a fresh, untouched fetch
```

Calling `dbColumnInfo()` first does not shrink, consume, or alter what
`dbFetch()` later returns. The two calls read the same fully-computed result;
`dbColumnInfo()` is a data read that happens not to be exposed to the caller as
row values, not a schema-only operation.

### 2. The same call, wrapped so the query truly cannot compute a row

```r
sql0 <- paste0("SELECT * FROM (", as.character(sql), ") AS q_ LIMIT 0")
res0 <- dbSendQuery(con, sql0)
info0 <- dbColumnInfo(res0)
dbClearResult(res0)
```

Against the 3,000,000-row table this timed at 0 elapsed seconds — genuinely
deferred, nothing computed. But every computed column reported `type =
"logical"`: `tot`, `cnt`, `lab`, `flag` all lost their real type, while the
passthrough column (`region`, a declared base-table column) kept `character`.
This is the same emptiness the handoff already measured for
`collect(head(<query>, 0))` (`investigation` handoff, fact 3), reproduced here
one layer lower, at the DBI `dbColumnInfo()` call instead of dbplyr's
`collect()`. Re-running `collect(head(q, 0))` directly in this session
reproduced the identical result: every computed column `<lgl>`, `NA` typed.

SQLite assigns a value's type when the value is computed, not when the column
is declared (this is `sqlite3_column_type()` semantics, and it is the reason a
declared base-table column keeps its type while an expression does not: the
declared column has *static* type affinity from the `CREATE TABLE` statement,
which is available in the schema without evaluating anything, while an
expression's result type has no such static record). A `LIMIT 0` prevents any
row from ever being computed, and there is no such thing on this dialect as a
type of a row that was never computed.

### 3. `dbplyr::db_query_fields()` / `dbplyr:::dbplyr_query_fields()`

`db_query_fields()` is not exported from the installed dbplyr (2.6.0):

```
Error: 'db_query_fields' is not an exported object from 'namespace:dbplyr'
```

The current internal equivalent is `dbplyr:::dbplyr_query_fields()`. Its
source, read directly from the installed namespace:

```r
function (con, sql, ...) {
    check_2ed(con)
    sql <- sql_query_fields(con, sql, ...)
    df <- db_get_query(con, sql, "Can't query fields.")
    names(df)
}
```

and `sql_query_fields.DBIConnection` (the method dispatched to for both
RSQLite and DuckDB connections, since neither defines its own):

```r
function (con, sql, ...) {
    sql <- as_table_source(sql, con)
    sql_query_select(con, sql("*"), dbplyr_sql_subquery(con, sql),
      where = sql("0 = 1"))
}
```

It is exactly a `WHERE 0 = 1` subquery — the same construct as fact 2 above —
and the outer function discards whatever type information `db_get_query()`
might carry, keeping only `names(df)`. Running it directly confirmed both
halves of that reading:

```r
dbplyr:::dbplyr_query_fields(con, sql)
#> [1] "region" "tot"    "cnt"
```

No types at all, by construction — not merely typeless on SQLite the way
`WHERE 0=1` is elsewhere. This function cannot answer the type question on any
dialect; it was never designed to.

### 4. DuckDB, all of the above

Every method above — raw `dbColumnInfo()`, `LIMIT 0`-wrapped `dbColumnInfo()`,
and `collect(head(q, 0))` — reported correct computed types on DuckDB with
zero rows: `tot` numeric, `cnt` numeric, `flag` logical, `lab` character. This
reconfirms the prior session's fact 6 (DuckDB types columns, not values) and
extends it to `dbColumnInfo()` specifically, which had not been tried before.
DuckDB needs none of the above investigation to get a genuine, zero-row,
zero-cost schema read — it already has one through the existing
`collect(head(x, 0))` route.

## What this establishes

On RSQLite — the dialect that reproduces #106, and the representative
available for the generic `"sql"` backend kind, which covers every dbplyr
connection that is neither DuckDB nor Postgres — no method tried reports a
computed expression's real type without the backend fully computing at least
one row:

- `DBI::dbColumnInfo()` without `dbFetch()` *looks* like a schema-only read
  but is not one: RSQLite executes and buffers the whole result inside
  `dbSendQuery()` itself, before `dbColumnInfo()` or `dbFetch()` are ever
  called. Measured cost is identical whether or not `dbFetch()` follows.
- The only way measured to make RSQLite skip that computation (`LIMIT 0`,
  at either the dbplyr or the raw-DBI layer) also makes every computed
  column typeless (`logical`, `NA`). This is not an implementation gap;
  SQLite types values, not columns, and a `LIMIT 0` query computes no value.
- `dbplyr`'s own field-introspection entry point discards type information
  by construction and is itself built on `WHERE 0=1`, so it inherits both
  problems and adds a third (no types returned at all, ever).

DuckDB has no such problem on any method tried, because it types columns
rather than values and therefore answers a zero-row query correctly. Its
existing `can_read_schema`/`collect_selection_proxy` route already gets this
for free today.

The tradeoff on RSQLite is exact, not approximate: a query can be made to
return zero rows (cheap, no data computed, no data read by any reasonable
reading of that phrase) or it can be made to report real computed-expression
types (only by computing — and, on the methods tried, fully materializing —
at least one row). No measured method gets both on this dialect.

## Revisions (2026-08-16)

`investigation/share-source-eligibility-on-coercing-dialects.md` establishes
that the premise underneath the tradeoff above does not hold on RSQLite: the
real computed-expression type, once obtained, is not evidence that a share
source is eligible on that dialect.

Measured there: `sum(txt)` over a text column returns a genuine `numeric` `0`
on RSQLite and raises a binder error on DuckDB, so `is_share_source_type()`
receives an eligible type for an ineligible source and accepts it. End to end,
`share_of_total()` over such a source returns the #106 symptom — an all-missing
share column with `1` on the grand total row — while the local backend raises.
#106's own reproduction, `max(region)`, remains fixed, because a character
result is a wrong type and this is not.

Both branches of the tradeoff stated above therefore fail to answer the
question the eligible-type rule exists to answer, on the one dialect this note
was written about. The successor note records the question that is answerable
instead — whether the dialect converts rather than refuses — which needs one
query per dialect that reads none of the caller's data and references none of
their tables.

Nothing measured in this note was contradicted. What changed is what the
measurements were taken to be evidence for.
