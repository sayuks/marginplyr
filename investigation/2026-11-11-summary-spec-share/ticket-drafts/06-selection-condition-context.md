# [P3] Blame the caller's call when a column selection fails in a Margin verb

## What to build

When tidyselect refuses a selection in `.by`, `.grouping`, or a summary's `across()`, `pick()`, `if_any()` or `if_all()`, the condition must keep tidyselect's class, message and cause and be blamed on a call the caller wrote. Today it names an internal function, and a summary selection also quotes an argument the caller did not write.

## Contract and minimal reproduction

`CONTEXT.md`, *Condition context*: "an argument is quoted as the caller spelled it, and the blamed call is the Margin verb the caller wrote, rather than the columns, the expressions, and the calls marginplyr allocated". ADR 0022's amendment: the argument, the grouping values and the blamed call are one context owed in the caller's terms.

```r
library(dplyr)
library(marginplyr)
d <- tibble::tibble(g = c("a", "b"), x = 1:2)
summarize_with_margins(d, n = n(), .by = zz)
summarize_with_margins(d, n = n(), .grouping = rollup(zz))
summarize_with_margins(d, across(zz, sum), .grouping = rollup(g))
summarize_with_margins(d, n = n(), across(zz, sum), .grouping = rollup(g))
```

Expected: the first three are blamed on `summarize_with_margins()`, as `summarise(d, n = n(), .by = zz)` is blamed on `summarise()`. The fourth reads as native dplyr's does: ``In argument: `across(zz, sum)`. Caused by error in `across()`: Can't select columns that don't exist.``

Actual: the first two display ``Error in `resolve_column_selection()```, the third ``Error in `resolve_summary_selection()```, and the fourth inserts two layers:

```
ℹ In argument: `across(zz, sum)`.
Caused by error in `across()`:
ℹ In argument: `(function (x) ...`.
Caused by error in `resolve_summary_selection()`:
! Can't select columns that don't exist.
```

## Cause and counterevidence

`resolve_column_selection()` and `resolve_summary_selection()` call `tidyselect::eval_select()` without an error call, so tidyselect blames its caller's frame, and `with_margin_error_call()` reassigns the call only for a Package condition. The share `across()` path already passes the caller's own call.

All four Margin verbs and `inspect_grouping()` reproduce the `.by` and `.grouping` form on local, dtplyr, SQLite, DuckDB and Arrow inputs. On lazy inputs every summary `across()` and `pick()` position displays the internal call. `all_of("zz")` and `starts_with(1)` reproduce with their own inner context intact. The class, message and cause chain are correct, so no value is affected.

ADRs 0008 and 0026 describe this condition as carrying "tidyselect's blamed call", which it does not. Existing tests compare the root class and message only. Tracker searches for the two function names found only implementation pull requests.

## Acceptance criteria

- [ ] A failed `.by` or `.grouping` selection is blamed on the Margin verb or `inspect_grouping()` call the caller wrote, for all four verbs and both summary spellings.
- [ ] A failed summary selection shows no internal function name and no generated-function argument, at first and later positions, on local and lazy inputs.
- [ ] The condition's class, message and cause stay tidyselect's; the predicate and nested-specification refusals keep their Package conditions.
- [ ] Tests assert the blamed call and rendered context, and ADRs 0008 and 0026 describe the call actually carried.

## Environment and dependencies

main `fbf39cd2780365af59f4b25c869e4332a57f19ff`, 2026-11-11. R 4.6.1, arm64 macOS 26.6.2, dplyr 1.2.1, dbplyr 2.6.0, dtplyr 1.3.3, data.table 1.18.6.1, duckdb 1.5.5, arrow 25.0.1, tidyselect 1.2.1, rlang 1.3.0. Evidence and the rejected alternatives are in `investigation/summary-spec-share-differential-hunt-2026-11-11.md`. No repair was implemented.

## Blocked by

None (can start immediately).
