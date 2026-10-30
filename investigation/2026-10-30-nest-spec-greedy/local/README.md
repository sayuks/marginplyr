# Local nesting equality and recursive-factor audit

Investigated: 2026-10-30
Baseline: `28c449aa03fd5289916f0673e41c848cc126999e` (`main`)

## Finding: display conversion split one source equality group

The local track reproduced one previously unhandled nesting defect. A valid
vctrs character class supplied a case-insensitive `vec_proxy_equal()` and an
ordinary case-preserving `as.character()` method. Source keys `A` and `a`
therefore represented one equality group. Direct `vctrs::vec_group_loc()`,
ordinary `dplyr::nest_by()`, and `summarize_with_margins()` each returned one
group containing both source rows.

Both `nest_with_margins()` and `nest_by_with_margins()` with
`.grouping = grouping_set(g)` and the default non-missing Margin label
returned two one-row cells instead. No actual subtotal or omitted dimension
was needed. A `rollup(g)` split the same detail group while retaining its
correct two-row Grand total cell. The `NULL`-label, `NA_character_`-label,
and fixed `.by` controls retained the original equality group.

This concerned source membership rather than preserving an arbitrary column
class or attribute. The source's equality relation was already accepted by
ordinary dplyr grouping, and the non-missing label was a documented display
conversion. The public nesting reference promised one cell per source group;
closed [#620](https://github.com/sayuks/marginplyr/issues/620) also stated that
a display choice must not change which source rows belong to a nested group.
That issue and its empty comment list were read before classifying the new
candidate. Its close-double and DST examples concerned distinct source groups
merging under identical display text. This fixture established the inverse
remaining boundary: equal source keys split under different display text.

### Cause and controls

The independently testable hypotheses were:

1. Display columns refined the source equality relation during the fold.
   Removing display conversion with a typed-missing label would retain the
   two-row group; a fixed `.by` key would also remain correct.
2. Union or slicing destroyed the class's equality semantics. The
   typed-missing-label control would then also split the group.
3. The source class itself was not groupable by dplyr. Ordinary
   `dplyr::nest_by()` would then fail or split the same source group.

The controls supported the first hypothesis. The typed identity copies in
`execute_margin_nest()` retained the original key. However,
`nest_expanded_margins()` grouped by both those identities and `group_cols`,
whose dimension values had already been converted to character by
`label_margin_branch()`. Equal typed identity values did not undo the finer
partition introduced by the displayed character key.

`reproduce-equality.R` retained the minimal two-row fixture and upstream and
public controls. Its ordinary invocation exited zero after asserting the
dated incorrect result. Its `--assert` invocation required correct source
membership and was independently run to terminal exit 1:

```sh
Rscript investigation/2026-10-30-nest-spec-greedy/local/reproduce-equality.R
Rscript investigation/2026-10-30-nest-spec-greedy/local/reproduce-equality.R --assert
```

The adjacent `.log` and `-assert.log` recorded both outcomes. No fix was
applied. The minimal consumer did not use `.keep`, `.id`, `.sort`, a fixed key,
an omitted dimension, or an empty source; removing any of those from a larger
fixture was therefore unnecessary to its reproduction.

### Boundary sweep

`equality-boundaries.R` completed 540 calls over two-row, reversed two-row,
four-row, one-row, and empty sources; single-set, rollup, and fixed-key
requests; both nesting verbs; both `.keep` modes; all three `.sort` choices;
and string, `NULL`, and `NA_character_` labels. An independent
`vctrs::vec_group_loc()` source oracle detected 72 wrong memberships, all in
the populated equivalent-key string-label dimension cases. The other 468
calls matched, and 180 Grand total membership controls also passed. Every serialized source was unchanged, and every `.keep`
projection retained its exact original classed source key.

The adjacent CSV preserved the complete case ledger. These were finite local
measurements with one explicit equality class, not a claim about all S3
methods or dtplyr. The coordinating track supplied an independent numeric
class reproduction.

## Recursive factors and upstream exclusion

`packed-factor.R` examined a packed tibble, a recursive packed tibble, a list
of factor vectors, and a factor array, each containing a declared NA level
and an actually missing factor code. It varied payload, dimension, and fixed
roles; both nesting verbs; both keep modes; and all sort choices, with typed
missing labels.

Of 144 calls, 120 were accepted and 378 exact source projections retained the
factor values and levels. The other 24 used the factor array as a grouping
key and reached `expressionStackOverflowError`. `factor-array-control.R`
reproduced that recursion in ordinary `dplyr::group_by()` and direct
`vctrs::vec_group_loc()` too, so it was excluded as the previously observed
upstream grouping limitation rather than published as a new package defect.
Factor arrays used as payloads passed.

The initial factor sweep mistakenly attempted to project fixed keys into
cells with `.keep = FALSE`; the oracle was corrected to apply projection
checks only when the column belonged in the cell. No production behavior was
changed, and the completed run above was rerun from the corrected script.

## Scope and replay

This track read `CLAUDE.md`, `AGENTS.md`, `CONTEXT.md`, the public nesting and
label references, ADRs 0016 and 0018, and the October 29 local exhaustive and
deep-hunt records. Those prior measurements guided case selection and were
not counted as new runs. All retained scripts loaded this checkout through
`pkgload::load_all()` and wrote dated investigation evidence only.

```sh
Rscript investigation/2026-10-30-nest-spec-greedy/local/equality-boundaries.R
Rscript investigation/2026-10-30-nest-spec-greedy/local/packed-factor.R
Rscript investigation/2026-10-30-nest-spec-greedy/local/factor-array-control.R
```

The local track ran no full test suite, changed no package implementation or
permanent package test, and published no tracker mutation. Arbitrary S3/S4
methods, remote nesting support, other dependency versions, and large-scale
performance were outside these completed measurements.
