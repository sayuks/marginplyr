# Zero-row type policy comparison for #706

Investigated: 2026-09-27
Revised: 2026-09-27 — investigation/zero-row-type-policy-followup-2026-09-27/README.md
Baseline: 9af58d87ca8c49ff267df66d368e1059885f5b38
Status: throwaway comparison; no package policy adopted

## Question and scope

The user wanted types preserved for nonempty results, including all-missing
columns, but placed little value on zero-row type stability if it required
substantial implementation or maintenance. They explicitly accepted backend
integer/double differences for Grouping helpers after the DuckDB experiment.

This experiment compared removing zero-row-specific declarations with extending
the existing SQLite declaration mechanism to direct Grouping helper outputs.
Both patches were applied to disposable copies of the same baseline. Neither
patch was applied to the production branch. The deltas below counted R source
only, including changed lines; they excluded future docs, tests, and comments.

| Variant | Actual diff | Files | Scope |
| --- | --- | --- | --- |
| Relaxed | +5 / -19; net -14 | 3 | Removed `.id` declarations; retained share restoration and source anchors |
| Extended | +10 / -7; net +3 | 1 | Registered direct Grouping outputs alongside `.id` in existing declarations |

The small deltas did not mean the variants were complete production patches.
The extended patch in particular exposed a result-class change described below.

## Why withdrawing zero-row promises did not remove the materializer

At the baseline, `R/sqlite-typed-order.R` contained 322 lines, but those lines
also preserved source-column types, nonempty all-missing shares, margin order,
and compute destination behavior. They were not a zero-row-only subsystem.

The collector's `all(is.na(out[[name]]))` covered both zero rows and nonempty
all-missing columns. Removing it lost the latter guarantee. Ordinary SQLite
compute also lost the numeric type of a nonempty all-missing share unless the
typed destination was retained. The relaxed patch therefore retained share
declarations and their shared collector/materializer.

Removing `.id` declarations alone also lost nonempty all-missing source-key
types on some unsorted compute paths. The relaxed patch changed admission to
the typed result to depend on source anchors as well as declared outputs.
This preserved the source types in the measured matrix without retaining an
incidental dependence on the `.id` declaration.

Consequently, a relaxed public promise did not imply intentionally making every
empty result untyped. Empty shares still had double type through the same code
needed for nonempty shares. A special `nrow(out) > 0` guard would have added
code solely to remove that incidental behavior; it was not part of the patch.

## Measurements

`probe.R` ran 114 query cases at five result boundaries: direct collect, collect
with `n = 1`, collect with `n = 0`, compute then collect, and compute then
collect with `n = 1`. Each variant produced 570 observations: 352 nonempty and
218 empty. The matrix included `.id` presence, three sort modes, margin labels,
one set versus rollup, summary and expansion, character/integer/double missing
source keys, missing/zero share denominators, direct and across shares, helper
spellings, and empty input with a one-row grand total.

| Observation against baseline | Relaxed | Extended |
| --- | --- | --- |
| Nonempty column values/types changed | 0 | 0 |
| Nonempty result classes changed | 0 | 4 |
| Empty observations changed | 127 | 200 |
| Public names or row counts changed | 0 | 0 |

The relaxed variant changed only empty `.id` columns from integer to logical
within this matrix. Its nonempty observations matched the baseline completely.

The extended variant restored integer for empty direct `grouping_bit()` and
`grouping_id()` outputs. It recognized unnamed outputs, namespace-qualified
calls, and redundant parentheses. It did not declare arbitrary enclosing
expressions or helpers inside an `across()` lambda. For example,
`across(v, ~ grouping_id())` did not receive the direct-call guarantee. General
expression result types could not be inferred merely from the presence of a
helper: `as.character(grouping_id())` intentionally returned character values.

The extension entered the specialized collector on additional paths. In four
nonempty observations (two one-set sorted queries at two collect limits), this
changed tibble to plain data.frame while preserving columns and values. That
was an existing collector behavior newly reached by the extension, and needed
resolution before adoption. It was not evidence that a type guarantee required
changing result class.

Some empty enclosing expressions also changed from integer/character to logical
after compute, because the specialized materializer declared only known output
types. This was outside the prototype's direct-call guarantee but illustrated
that broadening the path affected more than the newly declared columns.

The raw results were saved in the three `.rds` files. `analyze.R` produced
`results-summary.txt` and both difference CSVs. These were focused experiments,
not a full package test run or a review-ready check. They did not prove behavior
on every database driver or retest every compute destination/transaction case.

## Other backends and the clarified requirement

`backends.R` measured local, dtplyr, Arrow, and DuckDB paths on populated and
empty inputs. Local, dtplyr, Arrow, and DuckDB portable execution returned
integer for `.id`, `grouping_bit()`, and `grouping_id()`. DuckDB native GROUPING
execution returned integer `.id` and double helpers, including with real rows.
The user accepted the latter numeric storage difference, so this comparison
did not propose a cross-backend integer conversion layer.

Versions measured: R 4.6.1 (2026-06-24), dplyr 1.2.1, dbplyr 2.6.0,
RSQLite 3.53.3, DBI 1.3.0, dtplyr 1.3.3, arrow 25.0.1, duckdb 1.5.5,
and rlang 1.3.0.

## Recommendation made from this evidence

The recommendation was to exclude zero-row R column types consistently from
the public guarantee for `.id`, shares, and Grouping helper outputs. Nonempty
types, including all-missing shares and typed source dimensions, would remain
protected. Backend numeric representations would remain acceptable for helpers.
The proposed boundary concerned output rows: an empty input that produced a
grand-total row still required the nonempty guarantee.

The evidence did not support claiming large code savings. Retaining the shared
restoration was necessary, and the `.id` cleanup saved only 14 net source lines.
The strongest reason for the recommendation was the user's limited need for
empty-schema stability relative to the future promise, test scope, and
expression-shape rules that extending it would establish.

The extension itself reused an appropriate mechanism and was small. For a
product requiring schema-stable empty results, it would have been a reasonable
direction after resolving class preservation and deciding the direct/across
boundary. The comparison did not establish a line count for that completed,
broader implementation.

Relaxing the promise would still require editing the `.id` and share reference
contracts, relevant ADR amendments, and zero-type assertions. Tests for empty
row counts, column names, empty-input grand totals, nonempty missing values,
source types, and ordering would remain. This would be an explicit contract
change, not merely a documentation clarification.

The earlier narrow helper prototype on this branch did not exercise source-key
preservation or the additional collector paths. This matrix qualified its
inference that registering helper columns alone was sufficient for adoption.

## Revisions (2026-09-27)

The [follow-up comparison](../zero-row-type-policy-followup-2026-09-27/README.md)
measured an additional 119-line test consolidation and a broader +50-line
helper extension. It also reproduced a dynamic-name defect in that extension.
The +3-line measurement above remained the direct-call-only prototype's size;
it did not establish the cost of a complete helper guarantee.

## Replay

From this prototype branch's repository root, with the measured dependencies
available:

```sh
evidence="$PWD/investigation/zero-row-type-policy-2026-09-27"
scratch=$(mktemp -d)
for variant in base relaxed extended; do
  mkdir "$scratch/$variant"
  git archive 9af58d87ca8c49ff267df66d368e1059885f5b38 | tar -x -C "$scratch/$variant"
  if [ "$variant" != base ]; then
    git -C "$scratch/$variant" apply "$evidence/$variant.patch"
  fi
  Rscript "$evidence/probe.R" "$scratch/$variant" "$scratch/$variant.rds"
done
Rscript "$evidence/analyze.R" "$scratch"
```

`backends.R` was run from an unmodified baseline checkout. It used in-memory
databases and the repository's optional-dependency guard.
