# [P2] Quote each unnamed local summary as written when several share a Margin summary

## What to build

A condition raised by one of several unnamed summaries on a local input must name that summary as the caller wrote it, and conditions from different unnamed summaries must stay separate. Today every such argument is quoted as one internal wrapper, and their warnings collapse into a single Repeated condition.

## Contract and minimal reproduction

`CONTEXT.md`, *Condition context*: "an argument is quoted as the caller spelled it, ... rather than the columns, the expressions, and the calls marginplyr allocated". *Repeated condition*: occurrences agree on "the argument they are attributed to as the caller wrote it — and are distinct conditions otherwise". ADR 0022 owns the restatement.

```r
library(dplyr)
library(marginplyr)
d <- data.frame(g = c("a", "a", "b"), v = 1:3, w = c("1", "x", "4"))
summarize_with_margins(d, sum(v), sum(as.numeric(w)), .grouping = rollup(g))
```

Expected, as `summarise(d, sum(v), sum(as.numeric(w)), .by = g)` reports: ``In argument: `sum(as.numeric(w))`.``

Actual: ``In argument: `((function (x, ns, pos = -1, envir = as.environment(pos)) ...`.`` An error from `summarize_with_margins(d, mean(v), max(w + 1), .grouping = rollup(g))` carries the same line.

Distinct arguments collapse:

```r
summarize_with_margins(
  d, sum(if (n() == 3L) as.integer(w) else v),
  max(if (n() < 3L) as.integer(w) else v), .grouping = rollup(g)
)
```

Expected: two warnings, one per argument, as the same call with `a =` and `b =` gives. Actual: one warning with "1 further grouping set raised this warning".

## Cause and counterevidence

`wrap_assigned_local_summaries()` wraps every unnamed local summary in a call whose head is an inlined function object. dplyr's label for each truncates to the same string, so the restatement finds several candidates whose caller labels differ and leaves dplyr's quotation, and the deduplication key collides too. ADR 0022 assumes the rewrite is never visible where the caller's own expression would not have been truncated; here it is.

Correct today: exactly one unnamed summary, one named beside one unnamed, and all named. Data frames, tibbles, grouped tibbles, direct data.tables and `.grouping = NULL` reproduce; dtplyr and DuckDB are unaffected. Two unnamed `across()` calls are quoted as `dplyr::across(...)`.

Closed #430 concerned unnamed summaries named from the rewritten expression. `test-execution-conditions.R` covers named collisions only.

## Acceptance criteria

- [ ] With two or more unnamed summaries, a warning and an error each quote the caller's own expression, for ordinary expressions and for unnamed `across()`.
- [ ] Warnings from different unnamed summaries are reported separately; one summary warning in several grouping sets is still reported once with its count.
- [ ] Tests assert the rendered message at a default and a narrow console width, as ADR 0022 requires of this restatement.
- [ ] Data-frame-valued unnamed summaries still expand as ADR 0028 decides, and named summaries are unchanged.

## Environment and dependencies

main `fbf39cd2780365af59f4b25c869e4332a57f19ff`, 2026-11-11. R 4.6.1, arm64 macOS 26.6.2, dplyr 1.2.1, dbplyr 2.6.0, dtplyr 1.3.3, data.table 1.18.6.1, duckdb 1.5.5, arrow 25.0.1, tidyselect 1.2.1, rlang 1.3.0. Evidence and the rejected alternatives are in `investigation/summary-spec-share-differential-hunt-2026-11-11.md`. No repair was implemented.

## Blocked by

None (can start immediately).
