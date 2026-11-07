# Deferred label namespace and native DuckDB boundary probes

Investigated: 2026-11-07
Baseline: `0b5143133c023ccb4efb6fe27f1ef551e9f49186`
Scope: standalone investigation; no package repair or regression-test addition

## Confirmed finding: generated label calls use caller namespace dispatch

A single immutable dtplyr row contained character `g = "north"` and integer
payload `13`. An included-only expansion with the default Margin label
returned the expected row. After construction, a selective caller `:::`
dispatcher replaced only the lookup of `margin_display_character` in the
marginplyr namespace. The same expansion then returned `g = "MASK"`, with
one unwanted callback and unchanged payload. Ordinary source collection
returned `north`; deleting the dispatcher restored the original expansion.
No omitted set, sort, identifier, factor, summary expression or authored
namespace call was necessary.

This violated the opening row-copy contract of `expand_with_margins()`:
an included dimension is retained rather than replaced by caller code the
caller never requested. Package-generated label conversion belongs to the
Margin operation. `marginplyr_private_call()` qualified its private helper
name but left the namespace operator caller-resolved in deferred dtplyr
execution.

The retained minimal script first asserts the observed symptom and its
restoration/native controls. Its `--assert` mode then asserts the desired
unchanged result and intentionally fails on this baseline. This is evidence
of an unfixed defect, not a failing package test.

## Hypotheses and discriminating evidence

The ranked predictions, communicated before the larger matrix, were:

1. The generated namespace operator remained caller-resolved. Changing only
   that operator would change labels; restoring it would recover the same
   already-built query. Confirmed by the minimal witness and finite matrix.
2. The failure was only bare private-helper lookup. A selective namespace
   dispatcher should then be irrelevant. Refuted: the exact qualified lookup
   was intercepted; the earlier bare-helper repair was already present.
3. Ordinary source execution itself required the intercepted operation.
   Native collection and ordinary mutation/union should then call the
   dispatcher. Refuted: all such controls invoked zero selective callbacks.
4. Construction had damaged or permanently changed the source. Deleting
   the dispatcher should then fail to recover the result. Refuted: exact
   restoration and byte-identical source serializations held throughout.

`label-namespace.R` completed 648 observations:

- Three generated operations: character display, factor encoding, factor
  reconstruction.
- Four graphs: direct root, filter, cache named `copy`, and two renames over
  a root.
- Three plans: one included set, rollup, and repeated included occurrences
  around an omitted occurrence; all three sort choices.
- Operator installed before Margin construction or after it; counting,
  corrupting and raising dispatcher modes.

There were 216 callback-only observations with correct values, 216 silent
corrupt results, and 216 collection errors. All 648 constructions invoked zero
selective callbacks; all source, ordinary mutation, and ordinary union controls
invoked zero and retained their expected values. Every restored expansion
matched its expected complete frame after canonicalization by public occurrence
identifier and source payload. Every caller table remained byte-identical.
The total selective execution-call count was 744: plans with repeated included
sets invoked some conversions more than once. Callback errors were captured
as observations; the matrix itself ended at exit zero after checking the
classification.

The matrix compared complete canonicalized frames with `identical()`, including
column order, classes, values and multiplicities. It did not independently
establish physical Margin order, and no source mutation or source-row exposure
is claimed. Its initial draft used a rename derived from a cache and repeatedly
collected that ordinary input; native dtplyr cache renaming invalidated its own
later controls. That incomplete draft stopped and supplied no final count.
The retained final matrix uses renames over the root, preserving repeatable
ordinary controls.

## Native DuckDB comparison

`duckdb-native.R` completed 810 comparisons, all matching. Nine source types
were exercised as both a Grouping dimension and carried payload: list,
struct, map, BLOB, Date, timestamp, interval, decimal and ENUM. The matrix
crossed empty/nonempty input, five plan shapes, three sort choices, and direct
collection, direct temporary materialization, or zero-row finite collection.
The five shapes were an included set, rollup, omitted-first union, repeated
included set, and a product with an empty set.

The reference was the local expansion of an independently collected exact
input, with a typed-missing Margin label and unique integer fixed keys.
Complete canonicalized frames were compared using `identical()`. All 360
nonzero-collection sorted comparisons also matched the reference's complete
physical frame. The remaining 450 comparisons made no physical-order claim.
These are finite cross-backend correspondences, not an independent proof of
the shared Margin order implementation or arbitrary backend type behavior.

Initial exploratory comparisons without unique fixed keys reported differing
payload order inside equal Margin keys. That tie order was unspecified and
was excluded from findings. The final matrix made those order assumptions
explicit. An initial matrix draft used the nonexistent constructor name
`grouping_product`; it stopped before producing any observations. The retained
script uses `grouping_spec` and supplied all 810 final comparisons.

A separate dtplyr metadata lead was refuted: caller-local `setnames` did not
change the zero-row proxy. dtplyr injects its ordinary data.table wrappers into
its execution environment. Both direct typed inspection and expansion retained
the actual character dimension. This supplied no finding or finite-matrix
count.

## Duplicate comparison

The coordinator supplied all-state exports containing 721 Issues (including
bodies and comments) and PR bodies. Relevant complete report discussions read
were #491, #1143, #1241, #1281, #1303, #1311, #1326, #1341, #1346, #1355,
#1418 and #1422. Complete PR #497 and #1168 bodies/comments distinguished the
private-name visibility and ordinary label-helper repairs.

Live all-state `gh api` issue searches included Issues and PRs:

| Query suffix after `repo:sayuks/marginplyr` | Matches |
| --- | ---: |
| `"namespace" "label"` | 41 |
| `"margin_display_character"` | 0 |
| `"namespace" "factor"` | 23 |
| `"namespace operator"` | 21 |

Closest reports were [#1143](https://github.com/sayuks/marginplyr/issues/1143)
and [PR #1168](https://github.com/sayuks/marginplyr/pull/1168), protecting
ordinary helper-name lookup for generated labels;
[#491](https://github.com/sayuks/marginplyr/issues/491) and
[PR #497](https://github.com/sayuks/marginplyr/pull/497), making private
helpers visible during deferred factor evaluation;
[#1311](https://github.com/sayuks/marginplyr/issues/1311), protecting summary
selection and nested masks; [#1346](https://github.com/sayuks/marginplyr/issues/1346)
and [#1355](https://github.com/sayuks/marginplyr/issues/1355), protecting
share and order namespace calls; and
[#1422](https://github.com/sayuks/marginplyr/issues/1422), protecting generated
source copying. None specified the generated private label namespace lookup.
The new witness needs none of the selection, summary, share, sorting or
copy interception boundaries in those reports. The coordinator owned the
publication-time refresh and independent archive replay.

One repair slice covers generated label display, encoding and reconstruction;
these are variants of the same deferred namespace boundary. No prerequisite
refactor or blocking Issue was established. Ordinary caller-authored namespace
expressions must retain ordinary lookup.

## Replay and environment

All probes loaded the disposable archive of the exact baseline; the submitting
worktree's package sources were unchanged. Supply that archive's absolute root
as the first argument. For example:

```sh
Rscript minimal-label-namespace.R /absolute/path/to/baseline-source
Rscript minimal-label-namespace.R /absolute/path/to/baseline-source --assert
Rscript label-namespace.R /absolute/path/to/baseline-source /private/tmp/label-replay
Rscript duckdb-native.R /absolute/path/to/baseline-source /private/tmp/duckdb-replay
```

Retained final terminal outcomes were: minimal observation exit 0
(`bbcfd7`), desired-contract assertion exit 1 (`dbafdf`), label matrix
exit 0 (session `49819`), and native DuckDB matrix exit 0 (session `63319`).
All yielded handles were collected through terminal completion. The final
minimal assertion was rerun after collecting the matrix handle; an earlier
assertion launched before that collection is not used as the final execution
record. `TESTTHAT_PARALLEL=false` was explicit, and no suite, package build,
rendering or package implementation edit ran in this lane.

`environment.txt` records R 4.6.1 on arm64 macOS and installed dependency
versions, including dplyr 1.2.1, dbplyr 2.6.0, dtplyr 1.3.3, data.table
1.18.6.1, DuckDB 1.5.5, Arrow 25.0.1 and RSQLite 3.53.3. Actual package
source was pinned by the baseline SHA above, independently of the installed
package version. Hosted storage, live PostgreSQL/SQL Server, other dependency
versions/platforms, arbitrary caller extension classes, and unbounded resource
limits were not established by these probes.
