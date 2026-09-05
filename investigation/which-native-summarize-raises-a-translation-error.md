# Which of the native adapter's two summarizes raises a translation error

Investigated: 2026-09-04

`summarize_margin_native()` hands dplyr the caller's rewritten expressions
twice: once ungrouped through `native_summary_output_names()` to learn the
output names, and once grouped to build the query. #411 caught the translation
error around the first of them, and ADR 0022's amendment records that "which of
the two dbplyr reaches first is not a contract". #432 asked whether the second
can raise one of its own.

## What was searched for

Seventeen expression shapes, each handed to a `dplyr::summarize()` over
`dplyr::ungroup(tb)` and to a `dplyr::summarize()` over
`dplyr::group_by(tb, dplyr::pick(dplyr::all_of("a")))` — the two calls the
adapter issues — on a duckdb connection holding
`data.frame(a = c("x", "y"), b = c("p", "q"), v = c(1, 2))`:

`sum(v)`, `lag(v)`, `cumsum(v)`, `first(v)`, `nth(v, 2)`, `v`, `max(a)`,
`sum(v) / 0`, `switch(a, x = 1)`, `Recall(v)`, `seq_len(3)`, `list(v)`,
`sapply(v, identity)`, `zzz_no_such(v)`, `sum(.data$nope)`,
`mean(nonexistent)`, and `across(everything(), mean)`.

## What was found

No shape failed the grouped call while the ungrouped one succeeded. The two
that failed — `mean(nonexistent)` and `across(everything(), mean)` — failed
both, so the ungrouped call reaches them first in each case. The remainder
translated under both.

This is evidence and not a proof: it bounds the search rather than the
behaviour, and dbplyr is free to translate the grouped call differently in a
later release. Nothing was changed to rest on it. ADR 0022's amendment is
authoritative for what does rest on it, which is nothing — the tests assert the
restored context through the verb, so a dbplyr that moved the translation to
the grouped call fails them rather than dropping the restatement silently.

## What was not measured

Whether a real PostgreSQL connection blames the same call as duckdb. No
PostgreSQL driver is named in `optional_suggest_spec()`, so the suite has no
connection to reach the native path with; `dbplyr::simulate_postgres()` covers
the translation through the same adapter code, and is what the tests use.
