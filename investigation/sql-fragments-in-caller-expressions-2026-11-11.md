# SQL fragments substituted into caller expressions

Investigated: 2026-11-11
Repository snapshot: `953283665ed1fcb1470f1b8452c959d2c07a4f24`

#1569 asked for an audit, alongside its repair, of every other SQL fragment
marginplyr builds and places where a surrounding operator could bind to part
of it. The defect it repaired was in `grouping_id_sql_expr()` in
`R/grouping-context.R`: the multi-column mask was a bare sum of `GROUPING()`
terms, and dbplyr splices an `sql` object into the caller's translated
expression verbatim, so `grouping_id() * 10L` rendered as
`GROUPING(a) * 2.0 + GROUPING(b) * 10`. The evidence for that defect is in
`investigation/summary-spec-share-differential-hunt-2026-11-11.md`.

## Method

Every construction of SQL text under `R/` was located by searching for
`dbplyr::sql_glue2`, `dbplyr::sql(`, `dbplyr::build_sql` and
`dbplyr::sql_expr`. Each site was read for two questions: does the fragment
reach an expression the caller wrote, and if so, is it a single operand to
whatever surrounds it. A fragment that is a function call, a `CASE ... END`, a
parenthesized subquery or a literal is self-delimiting.

## Findings

Only `rewrite_grouping_expr()` substitutes SQL into a caller's expression. Its
callers in `R/summary-selections.R` pass `sql = FALSE` and receive R integer
constants; the native adapter in `R/grouping-adapter-native.R` is the one
caller with `sql = TRUE`. It produces three shapes:

| Helper | Fragment | Self-delimiting |
| --- | --- | --- |
| `grouping_bit(x)` | `GROUPING(x)` | yes, a function call |
| one-column `grouping_id(x)` | `GROUPING(x)`, weight 1 | yes, a function call |
| `grouping_id()` over a plan with no dimensions | `0` | yes, a literal |
| multi-column `grouping_id()` | the weighted sum | no, until #1569 |

The remaining sites build SQL that is never an operand of a caller's
expression:

- `grouping_set_id_sql_expr()` (`R/grouping-adapter-native.R`) is its own
  output column, and is a `CASE ... END` or a literal besides.
- The display flags in `summarize_margin_native()` are their own columns, each
  a single `GROUPING()` call.
- `guard_share_unnest()` and `sql_single_share_array()` (`R/share.R`) replace
  the sole argument inside a caller's `UNNEST(...)`, which the call's own
  parentheses delimit. The replacement is a function call with a subscript on
  DuckDB and a parenthesized scalar subquery elsewhere; the caller's array
  text is placed inside `[...]` or as one item of a `SELECT` list.
- `reconstruct_factor.tbl_duckdb_connection()` (`R/factor.R`) is a whole
  `CAST(... AS ENUM ...)` assigned to a column.
- The typed `NULL` prototypes in `R/grouping-adapter-union.R` and
  `R/backend-metadata.R` are `CAST(NULL AS ...)`, `NULL`, or a parenthesized
  scalar subquery, each a whole projection.
- `R/sqlite-boolean-names.R`, `R/sqlite-typed-order.R`,
  `R/duckdb-csv-metadata.R`, `bind_duckdb_share_values()` in `R/share.R`, the
  shared-input wrapper in `R/grouping-adapter-union.R` and
  `sql_render.marginplyr_duckdb_selection()` build whole queries.

So the multi-column mask was the only fragment of that shape, and no other site
needed a change.

`tests/testthat/test-native-grouping-id-precedence.R` executes each operator
around a multi-column mask on DuckDB against masks written out by hand,
checks the PostgreSQL rendering, and executes `grouping_bit()` and a
one-column `grouping_id()` inside arithmetic.
