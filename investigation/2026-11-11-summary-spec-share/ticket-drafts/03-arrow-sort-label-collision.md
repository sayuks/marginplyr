# [P2] Give Arrow Margin-order terms names that cannot collide after truncation

## What to build

A Margin order on an Arrow input must collect for every Grouping plan and every valid `.id` name. Today two different Grouping-bit sort terms can receive the same truncated field name, and `collect()` fails with Arrow's own error.

## Contract and minimal reproduction

`?summarize_with_margins`, *Margin order*: asking for a Margin order "composes with every `.duplicates` policy the verb accepts with no diagnostic, and lazy inputs stay lazy", and the order is one "`dplyr::collect()` and `dplyr::compute()` both observe". *Arrow inputs* accepts summaries, and `.id` has no length limit.

```r
library(dplyr)
library(marginplyr)
sales <- data.frame(
  region = c("E", "W", "E", "W"), channel = c("web", "web", "shop", "shop"),
  segment = c("b2b", "b2c", "b2c", "b2b"), brand = c("x", "y", "x", "y"),
  tier = c("hi", "lo", "lo", "hi"), revenue = c(10, 20, 30, 40)
)
one_way <- grouping_sets(
  grouping_set(region), grouping_set(channel), grouping_set(segment),
  grouping_set(brand), grouping_set(tier)
)
arrow::arrow_table(sales) |>
  summarize_with_margins(revenue = sum(revenue), .grouping = one_way,
                         .margin_label = NULL, .sort = "last") |>
  collect()
```

Expected: the ten rows the local data frame, DuckDB, SQLite and dtplyr return, one ascending pair per dimension in plan order.

Actual: `Invalid: Invalid sort key column: Multiple matches for FieldRef.Name(dplyr::if_else(.data[["..marginplyr_sort_1"]] %in% c(1L, 2L, ...)`.

A long `.id` breaks a plain rollup:

```r
arrow::arrow_table(sales) |>
  summarize_with_margins(revenue = sum(revenue),
                         .grouping = rollup(region, channel), .sort = "last",
                         .id = "margin_grouping_set_identifier") |>
  collect()
```

With `.id = "gid"` the same call returns seven rows.

## Cause and counterevidence

`margin_order_terms()` gives each dimension `dplyr::if_else(.data[[sort_id]] %in% <ids>, 1L, 0L)`. Arrow names an arranged expression by its first deparsed line followed by `...` — the convention `absorbed_expression_label()` reproduces — so distinct identifier vectors that wrap after the same prefix share one name. A longer identifier column leaves fewer identifiers on that line.

Tables, record batches and in-memory datasets fail under both Margin orders; `expand_with_margins()` fails the same way. A randomized comparison hit it on 5 of 200 generated specifications. `.sort = "none"`, four one-way dimensions, cubes of three and four, a seven-level rollup, and every non-Arrow backend pass.

Closed #541 deduplicated identical Grouping-bit terms and scoped itself to plans "whose varying bit expressions are unique". These expressions are unique; only their names collide.

## Acceptance criteria

- [ ] Both reproductions collect to the rows and order the local data frame returns, for `.sort = "last"` and `"first"`.
- [ ] Cover Arrow tables, record batches and datasets, one-way unions of five to seven dimensions, unions of rollups, `.duplicates = "drop"`, and `.id` names from three to at least sixty characters.
- [ ] No sort helper column appears in the collected result, and the result stays lazy until collected.
- [ ] The expansion verb's Margin order is covered by the same repair or a recorded follow-up.

## Environment and dependencies

main `fbf39cd2780365af59f4b25c869e4332a57f19ff`, 2026-11-11. R 4.6.1, arm64 macOS 26.6.2, dplyr 1.2.1, dbplyr 2.6.0, dtplyr 1.3.3, data.table 1.18.6.1, duckdb 1.5.5, arrow 25.0.1, tidyselect 1.2.1, rlang 1.3.0. Evidence and the rejected alternatives are in `investigation/summary-spec-share-differential-hunt-2026-11-11.md`. No repair was implemented.

## Blocked by

None (can start immediately).
