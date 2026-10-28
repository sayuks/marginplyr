# Summarize, Grouping specification, and contextual-share bug hunt

Investigated: 2026-10-28
Repository snapshot: `ac6ad79ff6026abe5fc45538d6dfefadf03a8ca3`

The investigation started from `main` in a new worktree on branch
`codex/margin-bug-hunt`. Local `main` and fetched `origin/main` resolved to the
snapshot above. Package implementation, package tests, generated documentation,
and dependency metadata were not changed. This note records observations at
that snapshot; the linked tickets own the requested repairs.

Three independently actionable findings were published with `bug` and
`ready-for-agent` labels. None required another ticket to complete first.
The user's instruction delegated ticket granularity and publication, so the
interactive breakdown checkpoint in `to-tickets` was omitted.

| Finding | Ticket | Priority | Observed effect |
| --- | --- | --- | --- |
| F1 | [#952](https://github.com/sayuks/marginplyr/issues/952) | P2 | dtplyr shares failed for data.table-special fixed-key names |
| F2 | [#953](https://github.com/sayuks/marginplyr/issues/953) | P2 | An empty local `pick()` silently selected all columns |
| F3 | [#954](https://github.com/sayuks/marginplyr/issues/954) | P3 | A constructor-shaped selection was evaluated twice and changed the grouping grain |

## F1: special dtplyr fixed keys prevented contextual shares

With a fixed `.N` key equal to P in both input rows, a dimension g equal to a,b,
and a measure x equal to 1,3, an ordinary Margin summary succeeded. Adding
`share_of_total()` caused construction to raise
`vctrs_error_names_must_be_unique`, reporting `.N` at locations 1 and 7. The
expected scalar totals were 1,3,4 and the expected double shares were
0.25,0.75,1.

```r
source <- dtplyr::lazy_dt(data.frame(
  .N = c("P", "P"), g = c("a", "b"), x = c(1, 3)
))
summarize_with_margins(
  source, value = sum(x), fraction = share_of_total(value),
  .by = .N, .grouping = rollup(g), .margin_label = NULL
)
```

All six names `.N`, `.I`, `.SD`, `.GRP`, `.NGRP`, and `.BY` failed with both
share helpers and direct/`across()` forms: 24 affected construction cases.
The corresponding 24 cases placing the name in a Margin dimension succeeded,
as did ordinary summaries and a fixed-only Grand total plan. The observation
harness checked distinct and missing partitions, no premature summary callback,
and unchanged source tables.

The supported cause was in `apply_windowed_shares()`: `vapply()` preserved the
original key names while producing safe temporary names. `rlang::syms()` then
produced a named grouping expression, for example
`.N = ..marginplyr_dtplyr_column_1`. This recreated the public column beside
the temporary one. `margin_safe_column_reads()`'s restoration then collided.
The printed grouping expressions in the capture provide the distinguishing
probe for this cause.

Closed #703 concerned original-key retention for special Margin dimensions;
those dimension controls passed here. This finding instead concerned fixed
keys in the windowed-share path. ADRs 0010 and 0017 supplied the share contract;
ADRs 0020 and 0029 supplied lazy-construction and source-safety boundaries.

## F2: an omitted local pick selection became everything

```r
data <- tibble::tibble(x = 1:2)
dplyr::summarise(data, z = nrow(dplyr::pick()))
# Error: Must supply at least one input to `pick()`.
summarize_with_margins(data, z = nrow(dplyr::pick()))
# z = 2L
```

`parse_pick_selection()` replaced an empty argument list with `everything()`.
The local selection rewrite therefore erased dplyr's required-input refusal.
A static-reader test titled "empty pick selection reads every column" encoded
the same mistaken premise. No distinct empty-call meaning was found in the
public reference or ADRs read for this investigation.

Qualified and parenthesized spellings and empty input reproduced the discrepancy.
Explicit `pick(everything())` and `pick(c())` matched dplyr and retained distinct
valid meanings. dtplyr 1.3.3 itself accepted empty `pick()`, so its answer was
not used as an oracle for local dplyr behavior. Closed #495 had consolidated
reader logic while requiring unchanged behavior; it had not established that
the empty-call meaning was correct.

## F3: a constructor-shaped ordinary selection was read twice

```r
data <- data.frame(a = c("u", "u"), b = c("x", "y"), value = c(1, 3))
reads <- 0L
rollup <- function(...) {
  reads <<- reads + 1L
  if (reads == 1L) "a" else "b"
}
inspect_grouping(data, .grouping = grouping_sets(rollup()))
# included: (b); reads: 2
```

The first returned selection named a, so the requested summary grain contained
one a group with total 4. Re-evaluation instead selected b and produced two
groups with totals 1 and 3. The internal preflight observation reported
`name_only = FALSE`; the second invocation was not a discarded names-only pass.

The reproduced scope crossed all five parent constructors with all five masked
constructor spellings. Inspection and summarization yielded 125 failed
intended-behavior assertions. Ordinary function selections, real nested
specification returns, and explicit qualified controls passed.

`read_grouping_arg()` retained a non-specification preflight value only for a
bare non-column name. A constructor-shaped call returning an ordinary selection
lost its value, and typed selection resolution evaluated the original call
again. Closed #873 had repaired the bare-name case. ADR 0019 specified that a
constructor used the caller's own binding; ADR 0008's evaluation-budget
amendments distinguished selections already read during preflight from those
not yet read. The call-shaped remainder was recorded as a separate ticket.

## Exploration and negative findings

The retained scripts live in
[`2026-10-28-summarize-grouping-share-hunt`](2026-10-28-summarize-grouping-share-hunt).
Counts below describe different feedback loops, not a single exhaustive domain.

| Harness | Observations | Outcome at the investigated snapshot |
| --- | --- | --- |
| `grouping/generated-plan-oracle.R` | 1,500 generated trees; zero skipped by the 1,000-occurrence bound | Independent included/fixed/Grouping-bit oracle passed |
| `grouping/tidyselect-backend-probes.R` | 23 expressions × 4 backends | 88 successful selections and 4 paired refusals |
| `grouping/dtplyr-metadata-probes.R` | 6 transformations | Printed storage types agreed in the five readable cases; vector assignment was refused by the metadata guard |
| `summary/explore-local-expressions.R` | 108 cases | No additional mismatch |
| `summary/explore-across.R` | 588 combinations | No additional mismatch |
| `summary/explore-key-types.R` | 15 key representations | No additional mismatch |
| `summary/explore-nested-masks.R` | 25 cases | No contract mismatch |
| `backend/backend-differential.R` | 40 seeded inputs × 4 lazy backends × 3 sort arguments | 450 matching row multisets; 30 expected dtplyr limited-input metadata refusals |
| `backend/special-names.R` | 24 names × 4 lazy backends × 2 sort arguments | All 192 row-multiset comparisons passed |
| `share/independent-oracle.R` | 24 seeded datasets × 9 plan/duplicate variants × 4 backends | All 864 result-relative denominator checks passed |
| `share/empty-types.R` | 32 empty-input/type observations | No additional confirmed defect |
| `share/name-sweep.R` | 416 role/name/backend calls | F1 was the newly confirmed defect |
| `share/dtplyr-special-fixed-key-observations.R` | 24 affected fixed-key cases, 24 dimension controls, fixed-only control | F1 scope reproduced; controls passed |
| `share/capture-boundaries.R` | 7 assertions | All passed |

The Grouping specification oracle covered nested unions/products, rollups,
cubes, composite dimensions, fixed keys, duplicate occurrences, plan order,
and non-syntactic names including commas, backticks, newlines, and Japanese
text. The local summary probes covered sequential outputs, selection/function
factories, frame expansion/packing, `cur_column()`, lexical environments,
visible language evaluation, nested masks, plain frames/tibbles, and classed,
packed, list, matrix, and nonfinite keys.

The broad backend differential used dtplyr, Arrow tables, live SQLite, and live
DuckDB against a collected-input local control. It varied fixed keys, missing
values, character/numeric/logical/factor dimensions, rollup/cube/explicit sets,
input filtering/limiting/mutation, identifiers, and all sort arguments. Its
comparison normalized integer/double helper representations and factors, and
compared row multisets; it did not independently prove Margin order or arbitrary
attribute preservation. Existing Margin-order tests supplied the order checks.

The share oracle used local frames, dtplyr, SQLite, and DuckDB, with default
unspecified row order. It checked each ratio against the source value displayed
on the same backend's Grand total or immediately less detailed parent row,
within missing-safe fixed partitions. It included a second count-based measure,
negative/zero/missing measures, retained repeated levels, arbitrary set order,
and Grand-total-only plans. Native and portable DuckDB paths were exercised by
duplicate-policy variants. This avoided mistaking SQL `SUM(NULL)` versus R
`sum(NA)` differences for a share defect. Arrow shares were deliberately refused
by the documented contract and were not numerically compared.

The following suspicions were not promoted to tickets:

- Static recognition ignoring a caller binding of `across` followed ADR 0019.
- Bare `bquote()` was not a protected capture under the public reference;
  qualified capture and evaluated `.()` controls passed.
- Two nested `pick()` differences from plain dplyr followed ADR 0019's
  independent-mask ownership; the inner-operation oracle agreed with marginplyr.
- Raw typed-missing labels could not represent a missing raw value and were
  correctly refused.
- A forged specification containing non-quosure arguments could leak an rlang
  error, but exported constructors did not produce that shape.
- A source using `base::with()` with a locally shadowed earlier-summary name
  was conservatively refused. The concrete example did not read the earlier
  summary, but no dedicated with/within promise was found and #130's Decisions
  chose over-reporting for undecidable dependencies. `share/scope-boundaries.R`
  preserves this unresolved limitation alongside working local/lambda controls.

## Replay and runtime

Run commands from the checkout root in fresh R processes. All delivered scripts
accept an optional first argument naming the checkout root, defaulting to the
working directory. They load that checkout and perform no implementation edits.

```sh
Rscript investigation/2026-10-28-summarize-grouping-share-hunt/share/repro-dtplyr-special-fixed-key.R
Rscript investigation/2026-10-28-summarize-grouping-share-hunt/summary/repro-empty-pick.R
Rscript investigation/2026-10-28-summarize-grouping-share-hunt/grouping/constructor-selection-repro.R
```

Each exited **1**, independently replayed by the coordinating agent. These are
intentional desired-behavior failures, not passing package tests. Their
assertions target a successful repair; no repair was applied or tested.
Adjacent `.log`/`.txt` files retain captures with terminal trailing whitespace removed. The other harnesses can be run by
substituting their paths from the table; exploratory observations that print
refusals are not represented as assertions of every diagnostic's equality.
The seeded wide matrices used seeds 20261028, 28471, and 1221 respectively.

`versions.txt` records the installed versions: R 4.6.1, dplyr 1.2.1,
dbplyr 2.6.0, dtplyr 1.3.3, data.table 1.18.6.1, Arrow 25.0.1,
DuckDB 1.5.5, RSQLite 3.53.3, rlang 1.3.0, and tidyselect 1.2.1.
No live PostgreSQL, hosted database, other R/dependency version, arbitrary
user-defined S3 method, timing/interruption stress, or unbounded Grouping plan
was established by these probes. Finite fuzz coverage cannot establish the
absence of further bugs.

## Existing-suite and publication checks

The focused existing package suite completed with terminal exit 0 for six files:
`grouping-interface`, `grouping-plan`, `summarize-operation`, `share`,
`share-backends`, and `margin-order`:

```sh
NOT_CRAN=true Rscript -e 'devtools::test(filter="^(grouping-interface|grouping-plan|summarize-operation|share|share-backends|margin-order)$", reporter="summary")'
```

The initial invocation stopped when testthat tried to open a snapshot in the
sandboxed read-only worktree. It was not counted as a pass. A permitted rerun
completed; the package snapshot files had no diff. SQL's normal missing-value
removal warning was observed.

This change contained investigation material excluded by `.Rbuildignore` and
was Repository-only under `design/agents/local-checks.md`. No package-affecting
Review-ready check was required. Applicable checks and the review record were
published on the pull request; CI monitoring was outside the request.
