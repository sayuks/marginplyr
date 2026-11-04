# Grouping selection evaluation and metadata boundaries

Investigated: 2026-11-04
Baseline: `7a05a1e42d53af62c057a30ad1892ffb35d4eef8` (`main`)

This track examined whether retained selections kept the caller's first answer
and whether name inspection distinguished a selector that needs types from one
that needs only column names. The November 3 selection frontier and the
November 4 dots, warning-handler, nesting follow-up, and tidyselect records
informed the choice of these two boundaries. Their comparison counts were
outside this replay. [November 3 selection frontier](../../2026-11-03-nest-spec-greedy/spec/README.md),
[November 4 dots](../../2026-11-04-nest-spec-greedy/spec/README.md),
[warning handlers](../../2026-11-04-nest-grouping-bug-hunt/spec/README.md),
[nesting follow-up](../../2026-11-04-nesting-grouping-followup/README.md),
[tidyselect](../../2026-11-04-tidyselect/README.md).

## GS1. A parenthesized callee could replace a retained selection

With `a = c("x", "y")` and `b = c(1L, 1L)`, bind `rollup` in the caller to a
function returning `"a"`. Bind the parenthesis operator to return its input
except when given a function other than that original closure, in which case
it returns a function selecting `"b"`. Native tidyselect evaluated
`(rollup)()` as a selection of `a`. Inspection of `grouping_sets(rollup())`
also selected `a`. Inspection of `grouping_sets((rollup)())` selected `b`.
With those keys, both Nest verbs could merge two separate source-row cells into
one.

The constructor spelling allowed preflight to evaluate the argument and keep
its first answer. In typed resolution, the original call spelling still
evaluated its parenthesized head and exposed the generated retained-value
closure to the caller's operator. The stateless witness changed the answer as
a result. An independent stateful mask counted the two lookups while the
original constructor ran only once. A genuine nested Grouping specification
instead went through preflight alone. Plain calls, parentheses around the
whole call, and ordinary parentheses gave the expected result. These controls
separated repeated head evaluation from repeated invocation of the original
constructor or a different interpretation of nested specifications. ADR 0008's
*Amendment: retain a constructor-shaped call's selection value (#946)* specified
the preflight answer, and ADR 0019 governed lookup of the caller's constructor.
[ADR 0008](../../../design/adr/0008-centralize-grouping-specification-kind-rules.md),
[ADR 0019](../../../design/adr/0019-resolve-contextual-helper-names-by-static-spelling.md).

The replay completed 1,125 calls across all five recognized constructor
spellings, bare and both namespace qualifiers, three parenthesis shapes, and
inspection plus all four Margin verbs. It measured 75 wrong successful
selections, 75 replay-only errors, 75 replay-only warnings, and 900 matching
controls. The stateful warning was delivered as the original condition object.
The error was identified in the condition chain.

Title searches across all issue and pull-request states used `constructor`,
`parenthesized`, `parenthesis`, and `call head`. #946 and PR #960 addressed the
bare constructor result. #1038 and PR #1057 addressed caller namespace
operators. #1240 addressed name ambiguity before selection. A concurrent issue
on the same baseline, #1273, covered the parenthesized-callee retention seam.
This investigation linked to that existing report. [#946](https://github.com/sayuks/marginplyr/issues/946),
[PR #960](https://github.com/sayuks/marginplyr/pull/960),
[#1038](https://github.com/sayuks/marginplyr/issues/1038),
[PR #1057](https://github.com/sayuks/marginplyr/pull/1057),
[#1240](https://github.com/sayuks/marginplyr/issues/1240),
[#1273](https://github.com/sayuks/marginplyr/issues/1273).

## GS2. Direct selection-context names triggered typed metadata

An unsafe-to-type derived dtplyr step added a column through a counted
callback. Direct selectors `tidyselect::peek_vars()`,
`names(tidyselect::peek_data())`, and
`tidyselect::peek_vars()[c(3L, 1L)]` all needed only that step's names. Native
tidyselect on the corresponding zero-row names and a fixed list of those
selected names provided the oracle. The direct calls reported the unsafe
typed-metadata refusal. Putting each same expression inside
`tidyselect::all_of()` returned the intended plan, including reordered
columns. In the 108-case comparison across three source shapes and six
inspection positions, all 18 direct selections on the derived step failed
this way and the other 90 matched. The upstream callback was never called.
The same step correctly refused an actual type-based predicate.

On live DuckDB, `tidyselect::peek_vars()[99L]` directly produced the same
missing-selection diagnostic as the wrapped form and the native selector.
Tracing the connection's `dbSendQuery()` method recorded one input query with
`LIMIT 0` for each direct call and zero for each wrapped call. These were six
direct and six wrapped calls across `.by` and each constructor. The error was
available from names alone and should have preceded metadata acquisition. The
inspection reference also specified that name-only inspection did not need
a typed read of derived dtplyr input. [ADR 0005](../../../design/adr/0005-reject-local-errors-before-backend-reads.md),
[inspection reference](../../../man/inspect_grouping.Rd).

The helper-argument classifier already recognized the relevant
`peek_vars()` and `names(peek_data())` results, and the outer classifier did
not reach that reader for these direct expressions. Wrapping the same value
isolated the position of the expression as the difference. Local and safe
dtplyr-root controls ruled out invalid selector syntax, and the live query
trace separated a metadata acquisition from a diagnostic-only difference.

All-state issue and pull-request searches used `peek_vars` and
`names(peek_data())` in general search and `peek_vars`, `peek_data`,
`selection metadata`, and `name-only` in titles. #342 and #346 covered
earlier specific name-only syntax. #1265 covered tidyselect's environment
pronoun. PR #841's review found and repaired the helper-argument form, which
was the passing control here. The direct-selection seam was independently
actionable. [#342](https://github.com/sayuks/marginplyr/issues/342),
[#346](https://github.com/sayuks/marginplyr/issues/346),
[#1265](https://github.com/sayuks/marginplyr/issues/1265),
[PR #841](https://github.com/sayuks/marginplyr/pull/841).

## Replay and limits

Run from the checkout root. Set `MARGINPLYR_HUNT_SOURCE` to use a different
source directory, such as an archive of the baseline.

```sh
Rscript investigation/2026-11-04-nesting-grouping-boundaries/spec/retained-head.R
Rscript investigation/2026-11-04-nesting-grouping-boundaries/spec/direct-names.R
```

Both scripts had an observation mode that asserted the dated result and a
`--assert` mode that asserted the desired behavior and exited 1 on the
investigated SHA. The adjacent logs recorded both statuses and the R and
dependency versions. The retained runs used a disposable archive of the
baseline. R 4.6.1 ran on aarch64 macOS Tahoe 26.6.2, with dplyr 1.2.1,
tidyselect 1.2.1, rlang 1.3.0, dtplyr 1.3.3, and DuckDB 1.5.5. The
DuckDB tracer was removed and the connection closed when the replay exited.
[Retained-head log](retained-head.log), [Direct-names log](direct-names.log).

The bounded cases established the lexical and metadata behavior for these
written forms. Arbitrary caller functions, other backend drivers, hosted SQL,
other R and dependency versions, and other spelling families were outside the
measurement.
