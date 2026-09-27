# Independent audit of the #706 test savings

Baseline: `9af58d87ca8c49ff267df66d368e1059885f5b38`, read without edits. This
is a throwaway comparison, not a production patch or a review-ready check.
No repository or issue was modified.

## What the previous 119-line figure measures

The five original test files total 7,432 physical lines. The previous relaxed
proposal totals 7,313 (-119). That proposal combined test reorganization,
removal of zero-row type assertions, and narrowing of label/order/plan cases.
119 denotes physical test-source lines, not tests or independent behaviors.

Three competing drafts restore every `.id`/share assertion removed from
`test-margin-id.R`, `test-share.R`, and `test-share-backends.R`, and both
`collect(n = 0L)` share assertions in `test-sqlite-declared-boundary.R`.
The local and other-backend files therefore match the baseline byte for byte.

| Draft | Consolidated SQLite file | Net lines removed across five files | Fraction of previous saving |
|---|---:|---:|---:|
| Previous relaxed proposal | 61 | 119 | 100% |
| Same cases, types restored | 67 | 106 | 89.1% |
| Also restore exercised source paths | 74 | 99 | 83.2% |
| Also retain label/order/plan matrices | 92 | 81 | 68.1% |

The six lines added to the 61-line consolidated file assert an integer `sid`
where present and double `p`, `t`, and `z_across` where present, at full direct
collection, finite direct collection, and collection after direct compute.
Restoring assertions outside that file costs seven further physical lines,
including the existing two-line explanatory comment in the backend helper.
Thus the marginal cost of keeping the zero-row guarantee under the same test
organization is 13 lines; the remaining 106 lines of the previous saving are
available without withdrawing it.

## Paths and matrices

The 67-line draft is not equivalent to the old pair's exercised code. Namespace
instrumentation with covr reports 1,396 covered probes versus the original
1,414. A local default expansion assertion and a sorted one-set expansion
case restore those 18 probes with seven lines; the 74-line draft covers exactly
the same 1,414 probes. The additional sorted one-set shape reaches
`margin_grouping_bit_ids()`'s constant-bit return, unlike a rollup; the local
case reaches the local metadata/expansion path.

The 92-line draft further retains the summary matrix over three labels
(`"Total"`, `NULL`, `NA_character_`) and three sort choices, and the expansion
matrix over the same axes plus one-set versus rollup plans. It retains the raw
SQLite empty CAST probe and a populated default expansion assertion, and adds
checks of every computed public schema. Fixtures and assertions are shared.
An integer source column and a double source column remain checked. It also
covers exactly the original 1,414 probes.

The full matrix's source-dimension assertion is kept outside finite collection:
the original expansion test checked finite names, identifier, and row count,
not its source dimension's type; the original summary matrix did not perform
finite collection. Requiring character `g` at that added boundary fails on the
unchanged baseline for unsorted typed-missing summary and rollup expansion.
The final draft keeps finite identifier/share assertions without inventing a
new source-dimension guarantee.

Probe equality is evidence of executed source locations, not proof of all
branch outcomes or regression equivalence. The shared-fixture drafts are not
literal copies of every original fixture. In particular, the separate root
fixture formerly used an integer grouping dimension whereas the shared source
uses a character dimension. Existing `test-sqlite-typed-order.R` separately
covers integer and double source dimensions with text labels across both
verbs, plans, sort modes, and identifier choices. The retained original
nonempty all-missing share-prefix tests remain intact.

## Validation

The unchanged baseline's two SQLite test files, and all three competing pairs,
passed focused `testthat::test_dir()` runs under namespace instrumentation.
The initial broader source-dimension assertion described above failed and was
removed before the final matrix run passed. The final matrix has five test
blocks (four boundary tests plus the consolidated empty test), with no failures,
errors, or skips reported. Only the two modified files needed this focused
execution; the other three restored files were copied directly from baseline.

Environment: covr 3.6.5.9001, RSQLite 3.53.3, dbplyr 2.6.0. Reproduce from the
repository directory with:

```sh
Rscript /private/tmp/marginplyr-706-preserved-tests/coverage.R original
Rscript /private/tmp/marginplyr-706-preserved-tests/coverage.R matrix-preserved
```

`preserved-tests.patch` is the portable test-only patch for the 92-line draft.
`comparison-counts.json` and `coverage-comparison.json` hold the small results.
Coverage RDS/CSV files and intermediate drafts are temporary evidence only.

## Interpretation

The conservative full-matrix comparison still obtains 81 of the previous
119 removed test lines while retaining zero-row `.id` and share types. The
remaining 38-line difference is a generous upper bound on the previous
proposal's extra test-source saving, not a measured marginal cost of the
promise: it also removes matrix/path/driver evidence. Under the same
organization the measured promise-specific difference is 13 lines.

If an independent implementation audit establishes 14 implementation lines
removed, adding them gives at most 52 lines for the generous comparison,
not 133 lines. Neither figure measures maintenance effort by itself. This
independent test audit supplies no basis for recommending withdrawal merely
from the previous 119-line test reduction.
