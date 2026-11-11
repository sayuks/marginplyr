# [P1] Keep dtplyr fixed keys named T or F intact while calculating contextual shares

## What to build

A Parent share or Total share over a dtplyr step whose fixed `.by` key is named `T` or `F` must partition by that column, as the same call does without a share and as every other backend does. Today the key is replaced by the logical constant, the fixed partitions merge, and the shares are wrong with no diagnostic.

## Contract and minimal reproduction

`?share_of_total`: "Fixed `.by` columns partition both calculations, so a fixed key never contributes to another partition's denominator", and *Lazy execution boundaries* lists dtplyr as supported. `?summarize_with_margins`: "Columns in `.by` retain their input types". ADR 0017 fixes the partitioning.

```r
library(dplyr)
library(marginplyr)
d <- data.frame(T = c("a", "a", "b", "b"), g = c("x", "y", "x", "y"),
                v = c(1, 2, 3, 4))
collect(summarize_with_margins(
  dtplyr::lazy_dt(d), zz = sum(v), sh = share_of_total(zz),
  .by = T, .grouping = rollup(g)
))
```

Expected, from ordinary summaries at `(T, g)` and `(T)`, and returned by the local data frame: `T` stays `a, a, b, b, a, b`; `sh` is `1/3, 2/3, 3/7, 4/7, 1, 1`.

Actual: `T` is `TRUE` on every row and `sh` is `0.333, 0.667, 1.0, 1.333, 1, 1`. No error or warning.

`share_of_parent()`, a key named `F` (becoming `FALSE`), the `across()` share form with `cube(g)`, two fixed keys and a two-level rollup reproduce.

## Cause and counterevidence

`show_query()` shows ``[, `:=`(T = TRUE)]`` before `by = .(T)`. The share window stage groups by bare key symbols, which dtplyr reads as logical constants; `margin_safe_column_reads()` aliases data.table's special symbols and dot-dot names but neither `T` nor `F`. The Margin-label and nesting paths already read these columns through a pronoun.

Correct today: the same call without a share on dtplyr; shares on a local data frame, a direct data.table, SQLite and DuckDB; `T`/`F` as a variable dimension, source name, share output name or `.id`. The caller's table is not modified.

Closed #603 covers the label-collision check for these names, closed #952 the special-symbol list for share fixed keys, and `test-dtplyr-special-nesting.R` the nesting verbs; none reaches this path.

## Acceptance criteria

- [ ] Both helpers return the explicit-grain shares and the original key values and type for fixed keys named `T` and `F` on a dtplyr step.
- [ ] Cover one and several fixed keys, direct and `across()` shares, rollups of one and two levels, a cube, both Margin orders and `.id`.
- [ ] A regression test asserts values against ordinary per-grain summaries, not against another marginplyr path.
- [ ] The caller's data.table stays unmodified and the no-share path, special-symbol keys and dot-dot keys keep their current results.

## Environment and dependencies

main `fbf39cd2780365af59f4b25c869e4332a57f19ff`, 2026-11-11. R 4.6.1, arm64 macOS 26.6.2, dplyr 1.2.1, dbplyr 2.6.0, dtplyr 1.3.3, data.table 1.18.6.1, duckdb 1.5.5, arrow 25.0.1, tidyselect 1.2.1, rlang 1.3.0. Evidence and the rejected alternatives are in `investigation/summary-spec-share-differential-hunt-2026-11-11.md`. No repair was implemented.

## Blocked by

None (can start immediately).
