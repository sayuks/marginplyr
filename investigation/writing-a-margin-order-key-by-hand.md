# Writing a Margin order key by hand

Investigated: 2026-09-02

Evidence gathered while deciding #373 — whether reversing a dimension belongs
in `.sort`, or whether the answer is a recipe writing the sort key as an
`arrange()` the caller controls. ADR 0018 had already rejected `.bits`, an
argument handing the Grouping bits to the caller, "on measurement: the caller
must drop those columns afterwards, and `arrange()` followed by dropping a
summarized column is exactly the shape dbplyr flattens, so the order is lost on
lazy backends". Whether that reaches a caller-written key was the question, and
the answer turned out to depend on which column is dropped.

## Environment

All measurements on 2026-09-02, one machine, macOS (Darwin 25.5.0).

| Component | Version |
|---|---|
| R | 4.6.1 |
| dplyr | 1.2.1 |
| dbplyr | 2.6.0 |
| rlang | 1.3.0 |
| duckdb (R package) | 1.5.5 |
| DuckDB engine | v1.5.5 |
| RSQLite | 3.53.3 |
| SQLite engine | 3.53.3 |

DuckDB exercises the native `GROUP BY GROUPING SETS` adapter and SQLite the
portable `UNION ALL` one, so every claim below that names both names both
adapters.

## Dropping a Grouping bit keeps the order; dropping a computed column loses it

The reversed-dimension key, written as the reporter wrote it:

```r
retail_sales |>
  summarize_with_margins(
    revenue = sum(revenue, na.rm = TRUE),
    yb = grouping_bit(year), rb = grouping_bit(region),
    .grouping = rollup(year, region)
  ) |>
  arrange(yb, is.na(year), desc(year), rb, is.na(region), region) |>
  select(-yb, -rb)
```

Returned 2026 above 2025 with the subtotals attached on a local data frame, on
DuckDB, and on SQLite. The `select()` is what ADR 0018's rejection describes,
and it did not lose the order on either backend. The rendered DuckDB query says
why:

```sql
SELECT
  CASE WHEN ("..marginplyr_grouping_1" = 1) THEN 'Total' ... END AS "year",
  CASE WHEN ("..marginplyr_grouping_2" = 1) THEN 'Total' ... END AS region,
  revenue
FROM (
  SELECT "year", region, SUM(revenue) AS revenue,
         GROUPING("year") AS yb, GROUPING(region) AS rb, ...
  FROM retail_sales
  GROUP BY GROUPING SETS (("year", region), ("year"), ())
) AS q01
ORDER BY yb, ("year" IS NULL), "year" DESC, rb, (region IS NULL), region
```

A Grouping bit is produced by the aggregate query, which the labelling
projection already wraps, so `yb` and `rb` are resolvable in the `FROM` of the
query carrying the `ORDER BY` even after the projection stops selecting them.
The `UNION ALL` adapter puts them in a subquery for the same reason. This is
ADR 0018's own implementation rule — the sort key must be resolvable in the
`FROM` clause of the query that carries the `ORDER BY` — reached from the
caller's side.

The same recipe under `.margin_label = NULL` returned the same row order on
DuckDB. There the dimensions keep their input types, so `desc(year)` orders
integers rather than the labelled character column; `.sort` has that property
too, since it also orders the result's own columns after labelling.

A column the caller computes *after* the summary is in the query that carries
the `ORDER BY`, not below it, and dropping it takes the order with it. The
measure-ordered case is that shape:

```r
retail_sales |>
  summarize_with_margins(
    revenue = sum(revenue, na.rm = TRUE),
    rb = grouping_bit(region), sb = grouping_bit(store),
    .grouping = rollup(region, store)
  ) |>
  mutate(.by = region,
         region_revenue = max(ifelse(sb == 1, revenue, NA), na.rm = TRUE)) |>
  arrange(rb, desc(region_revenue), region, sb, desc(revenue), store) |>
  select(region, store, revenue)
```

On DuckDB:

```
Error in `collect()`:
! Failed to collect lazy table.
Caused by error in `DBI::dbSendQuery()`:
! Binder Error: Referenced column "region_revenue" not found in FROM clause!
Candidate bindings: "region", "revenue", "rb"

LINE 22: ORDER BY rb, region_revenue DESC, region, sb, revenue DESC, store
```

Which is the failure ADR 0018 describes, with the binder error it names.

## The measure-ordered case needs a window function, not a self-join

#373 and its triage both say the second presentation request — regions
ordered by revenue, stores ordered by revenue inside them, subtotals still
adjacent —
"needs the parent measure on every row and so a self-join back onto the
subtotal rows". A grouped `mutate()` reaches it instead, and it is the
`mutate()` above: the region's own subtotal row is already in the result, so
`max(ifelse(sb == 1, revenue, NA))` over the region partition puts that measure
on every row of the region without reading the input again. dbplyr renders it
as a window function.

Keeping the helper columns, it returned West's stores, West's subtotal, East's
stores, East's subtotal, and the grand total — on a local data frame, on
DuckDB, and on SQLite, all three agreeing. So the case is reachable while the
result stays lazy; what is not reachable is dropping the helper columns from a
lazy result, per the section above.

## Two ways past the binder error, one of which is not a promise

`collect()` before the `select()` returned the intended order, which is the
local case and says nothing about SQL.

`compute()` before the `select()` also returned it, on DuckDB. That is a
measurement and not a guarantee: `compute()` materializes a table, a table has
no row order in SQL, and the subsequent `select()` renders a fresh query with
no `ORDER BY` at all. It happened to return insertion order. It is recorded
here so a later reader does not rediscover it and take it for a supported
route; `vignettes/recipes.qmd` deliberately does not show it.

## A hand-written key and `.sort` agree on a labelled numeric dimension

`.sort` orders the result's own columns, which are character once
`.margin_label` has been applied, and a hand-written `arrange()` names those
same columns — but the rendered SQL names them ambiguously, since the
aggregate subquery holds a column of the same name holding the pre-label
value. Whether
the two agree therefore depends on the dialect resolving `ORDER BY <name>` to
the output column.

Measured on a four-row input with `year` an integer taking `999` and `1000`,
where character and numeric order disagree. `.sort = "last"` put 1000 before
999; the hand-written key with `desc(year)` put 999 before 1000 — character
order in both directions — and local, DuckDB, and SQLite all three returned
that. So the dialects tested resolve to the output column and the two keys do
not diverge on this input.
