# [P1] Parenthesize a multi-column grouping_id() inside caller arithmetic on native grouping sets

## What to build

On a backend running native `GROUP BY GROUPING SETS`, an expression around a `grouping_id()` of two or more columns must evaluate against the whole bit mask, as it does on every other backend. Today the mask is spliced into the caller's expression as an unparenthesized sum, so the surrounding operator binds to one term and the value is wrong with no diagnostic.

## Contract and minimal reproduction

`?grouping_bit`: "`grouping_id()` combines those flags as a bit mask; its last argument is the least-significant bit", and expressions around a helper "follow the type rules of their backend" — type, not value. `?summarize_with_margins`: the portable adapter has "the same semantics" as the native query. ADR 0009 defines the mask.

```r
library(dplyr)
library(marginplyr)
d <- data.frame(a = c("x", "x", "y", "y"), b = c("p", "q", "p", "q"), v = 1:4)
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
q <- summarize_with_margins(
  copy_to(con, d, "t"), level = grouping_id(), weight = grouping_id() * 10L,
  depth = 3L - grouping_id(), .grouping = rollup(a, b), .id = "id"
)
collect(q) |> distinct(id, level, weight, depth) |> arrange(id)
show_query(q)
```

Expected (`rollup(a, b)` has masks 0, 1, 3 in plan order; the local data frame, dtplyr, SQLite and Arrow return this): `weight` `0, 10, 30`; `depth` `3, 2, 0`.

Actual on DuckDB: `weight` `0, 10, 12`; `depth` `3, 4, 2`. The bare `level` is correct. Rendered SQL: `GROUPING(a) * 2.0 + GROUPING(b) * 10` and `3 - GROUPING(a) * 2.0 + GROUPING(b)`.

Also wrong: `grouping_id() / 2`, `10L * grouping_id()`, `-grouping_id()`, `sum(v) * grouping_id()`, and `(grouping_id()) * 10L` with the caller's own parentheses.

## Cause and counterevidence

`grouping_id_sql_expr()` returns the reduced sum as bare SQL and `rewrite_grouping_expr()` substitutes it into the caller's expression.

Correct today on DuckDB: `+`, a right-hand constant subtraction, `%/%`, `%%`, `^`, comparisons, a function call around the helper, `grouping_bit()`, a one-column `grouping_id()`, and `.duplicates = "keep"`, which takes the portable adapter. PostgreSQL was checked only as text through `dbplyr::simulate_postgres()`, which renders the same unparenthesized sum; it was not executed.

The only existing test of arithmetic around the helper uses `+` on SQLite, which substitutes a literal. Searches of the tracker for grouping_id with arithmetic, precedence and parentheses found no report.

## Acceptance criteria

- [ ] On DuckDB, every binary arithmetic operator on either side of a multi-column `grouping_id()`, and unary minus, returns the value R arithmetic gives on the documented mask.
- [ ] Rendered PostgreSQL SQL carries the same grouping.
- [ ] Bare helpers, `grouping_bit()`, one-column masks, fixed-key arguments and the result's numeric type keep their current behaviour.
- [ ] A regression test compares native results with masks computed independently of marginplyr.
- [ ] Audit the other SQL fragments substituted into caller expressions for the same shape and record the outcome.

## Environment and dependencies

main `fbf39cd2780365af59f4b25c869e4332a57f19ff`, 2026-11-11. R 4.6.1, arm64 macOS 26.6.2, dplyr 1.2.1, dbplyr 2.6.0, dtplyr 1.3.3, data.table 1.18.6.1, duckdb 1.5.5, arrow 25.0.1, tidyselect 1.2.1, rlang 1.3.0. Evidence and the rejected alternatives are in `investigation/summary-spec-share-differential-hunt-2026-11-11.md`. No repair was implemented.

## Blocked by

None (can start immediately).
