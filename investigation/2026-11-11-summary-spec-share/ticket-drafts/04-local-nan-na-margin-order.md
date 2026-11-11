# [P3] Keep NaN and NA key groups contiguous in a local Margin order

## What to build

When a double fixed key or leading dimension holds both `NaN` and `NA`, a local Margin order must keep each of the two groups contiguous with its own margin rows, as dtplyr, DuckDB and Arrow already do. Today the local result interleaves them.

## Contract and minimal reproduction

`?summarize_with_margins`, *Margin order*: "fixed `.by` keys sort first because they come first, so each partition is one contiguous block whose internal order does not depend on any other partition", and "A margin row therefore sits with the rows it summarizes".

```r
library(dplyr)
library(marginplyr)
d <- data.frame(x = c(NaN, NA, NaN, NA, 1, 1),
                g = c("a", "a", "b", "b", "a", "b"),
                v = c(1L, 2L, 4L, 8L, 16L, 32L))
summarize_with_margins(d, s = sum(v), .by = x, .grouping = rollup(g),
                       .sort = "last")
```

`summarise(d, s = sum(v), .by = x)` returns three groups, so `NaN` and `NA` are separate partitions.

Expected, and returned by `dtplyr::lazy_dt(d)`, DuckDB and Arrow: after the `x = 1` block, `NaN a, NaN b, NaN Total, NA a, NA b, NA Total`.

Actual on a data frame, tibble or direct data.table: `NaN a, NA a, NaN b, NA b, NaN Total, NA Total`.

`.sort = "first"` and `rollup(x, g)` with `.margin_label = NULL` reproduce.

## Cause and counterevidence

The local key is `is.na(x), x`, evaluated by `dplyr::arrange()`. `is.na()` is true for both values and `arrange()` ties them, so the later terms interleave two groups `summarise()` keeps apart. The section's own key formula literally yields this order; what it contradicts is the contiguity the section derives from that formula and agreement with the lazy backends. The repair therefore includes deciding which of the two the section states, and no relative order of `NaN` and `NA` is promised today.

A string Margin label converts the dimension to character and orders correctly. SQLite stores `NaN` as `NULL` and has one group.

`bug-hunt-2026-09-26.md` set aside NA/NaN sorting differences from a dtplyr type matrix without addressing contiguity. `test-margin-order.R` and ADR 0018 hold no `NaN` case. Two investigators reached this independently.

## Acceptance criteria

- [ ] In both reproductions each of the `NaN` and `NA` groups is one contiguous run containing its margin rows, under both Margin orders.
- [ ] The Margin order section and ADR 0018 state what a local key does with `NaN` beside `NA`, including whether their relative order is specified.
- [ ] Double keys without `NaN`, packed, matrix and list keys, and factor keys keep their current order.
- [ ] A test asserts contiguity independently of the documented `arrange()` key, which reproduces the tie.

## Environment and dependencies

main `fbf39cd2780365af59f4b25c869e4332a57f19ff`, 2026-11-11. R 4.6.1, arm64 macOS 26.6.2, dplyr 1.2.1, dbplyr 2.6.0, dtplyr 1.3.3, data.table 1.18.6.1, duckdb 1.5.5, arrow 25.0.1, tidyselect 1.2.1, rlang 1.3.0. Evidence and the rejected alternatives are in `investigation/summary-spec-share-differential-hunt-2026-11-11.md`. No repair was implemented.

## Blocked by

None (can start immediately).
