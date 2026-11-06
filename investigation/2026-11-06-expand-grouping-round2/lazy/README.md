# Deferred expansion and Arrow row-origin order challenge

Investigated: 2026-11-06
Baseline: `9ffb24614a71512170298a3827bc11c80354e742`

This track investigated Arrow and dtplyr expansion in a private `git archive`
copy of the baseline. It read `CLAUDE.md`, `AGENTS.md`, `CONTEXT.md`, the local
check policy, ADRs 0002, 0016, 0018, 0020 and 0029, and the November 5 greedy and
adversarial investigations and November 6 deep lazy investigation before
choosing the probes. It established no new confirmed package defect. The
finite observations below were new executions, rather than credited reruns of
those earlier notes.

## Hypotheses and results

| Hypothesis | Distinguishing observation | Disposition |
| --- | --- | --- |
| Arrow's deferred reader or partition isolation changes the expected source-row order under a requested Margin order. | Map returned source-row and occurrence identifiers to independently enumerated Grouping bits and source-value ranks, using the returned fixed key and no additional sort. | Refuted in 768 row-origin comparisons, conditional on correct dimension transformations. |
| A volatile dtplyr source changes membership or runs repeatedly across grouping-set occurrences, particularly through grouping and cached aliases. | Reset a pinned RNG seed before ordinary source evaluation and expansion, compare independent explicit row copies, count predicate invocations, and compare caller-owned tables. | Refuted in 270 admitted comparisons; 27 `head()` shapes were safely refused before executing the source. |
| A valid Arrow expansion becomes semantically different from ordinary branch composition under downstream verbs. | Compare canonical public values/schema through the same ordinary computed arrangement and consumption routes. | 69 exact matches; 48 ordinary-control failures excluded. |

### Arrow row-origin order

[The order oracle](arrow-order.R) completed **768** comparisons: eight
physical dimension types, four source containers, four independently enumerated
Grouping families, both requested Margin orders and three consumption routes.
The types were UTF-8 text, int32, double, date32, zoned timestamp, int64 values
beyond exact R-double representation, decimal128 values differing beyond R-double
precision, and dictionary text with an unused declared value. Each source had
four rows, including a missing grouping value and a missing fixed key.

Sources were Table, RecordBatch, IPC FileSystemDataset and a UnionDataset
containing the same Dataset twice. The families were a two-dimension rollup,
a true composite rollup, a cube, and a union with repeated Grand total
occurrences. Every result retained its occurrence identifier and source row
identifier. The independent oracle enumerated each grouping-set membership and
used known source-value ranks. It read the returned fixed key, source-row
identifier and occurrence identifier, then mapped those identifiers to the
expected dimension ranks and Grouping bits. It compared that physical
row-origin sequence without imposing another sort. Tied payload rows were not
required to retain an unspecified internal order. Direct collection, compute
followed by collection and a RecordBatchReader round trip all passed.

This provided additional row-origin ordering evidence beyond the earlier deep
investigation's canonicalized array comparisons. It did not compare returned
`g` or `h` values or their physical Arrow arrays: their key ranks were derived
from source-row and occurrence identifiers. Consequently these observations
established the expected row-origin sequence conditional on correct dimension
transformations, not dimension-value preservation, physical type fidelity or
an independent guarantee of the returned key values. The separate composition
probe below compared public values/schema only for its ordinary text and
numeric fixture. The [CSV](arrow-order.csv) retained all 768 results.

### Supplemental Arrow-native dimension values and types

[The separate exact-value probe](arrow-exact-supplement.R) completed **256**
additional comparisons: the same eight source dimension types, four source
containers, four independently specified families and both Margin orders.
It materialized each query directly with `arrow::as_arrow_table()`, retaining
`g` and `h` as Arrow arrays throughout validation. It first independently
checked the source-row identifier multiset for every occurrence, including the
doubled multiplicities in UnionDataset fixtures. Only the small integer `rid`
and `sid` identifiers crossed into R to construct zero-based source indices.

Expected `g` and `h` used Arrow-native `Take()` over the original source column,
with missing indices exactly where the explicitly enumerated set omitted the
dimension. Fixed key `f` and payload identifier `rid` took their original source
values at the same indices; `sid` was checked as an int32 array after the
independent occurrence/multiplicity check. Every expected public column was
compared with both Arrow-native type equality and array `Equals()`, with exact
public column-name order checked too. All **256** results passed every check.
Int64 and decimal dimension values were never converted to R doubles.

This supplemented the row-origin oracle with direct materialization checks of
actual dimension values and physical types, including typed missing values.
It did not expand that value/type result to every consumption route tested by
`arrow-order.R`: compute and RecordBatchReader conversions were not separately
checked by this supplemental script. No ordinary Arrow conversion discrepancy
was observed in the direct native path. The [CSV](arrow-exact-supplement.csv)
and [raw terminal log](arrow-exact-supplement.log) retained the 256 results and
zero-exit terminal summary.

The preliminary API control successfully printed a precisely retained int64
`Take()` result with a missing index, then hit an extra closing parenthesis in
its last expression. That incomplete control supplied none of the 256 matrix
counts. The complete supplemental matrix passed in a fresh process.


### dtplyr random membership and source ownership

[The volatile-source oracle](dtplyr-volatile.R) completed **297** observations:
zero/one/eight source rows; eleven source shapes; three Grouping families; and
all three Margin-order choices. Shapes included row filtering, literal slicing,
random sampling, ordering, a named cache, ordered and grouped caches, a grouped
filter, a repeated-source union and a self-derived join. A lexical predicate
counted its invocations and randomly selected rows. Seed `8791` was reset before
ordinary source evaluation and before expansion consumption, so differences in
evaluation order or count could change the compared rows.

All **270** admitted comparisons matched independently assembled row copies,
schema, types and multiplicities. Construction invoked the predicate zero
times; consumption used the ordinary source's exact count, including two
ordinary source occurrences in union and join shapes. Every caller-owned source
table remained unchanged. Canonicalization made these value/evaluation tests,
not additional physical-order measurements. The [CSV](dtplyr-volatile.csv)
retained per-fixture counts, and the [log](dtplyr-volatile.log) retained the
terminal summary.

All **27** `head()`-over-filter shapes produced the documented Package condition
that faithful dtplyr metadata could not be determined without risking source-row
execution. Construction executed no predicate. A deferred bare `head` call can
resolve to a caller function rather than the ordinary truncation operation;
this track did not establish a public promise requiring its admission. The
conservative refusal under ADR 0020 was not promoted to a defect.

### Downstream Arrow composition

[The composition oracle](composition.R) completed **117** comparisons: thirteen
operations, all three initial Margin-order choices and three consumption routes.
Operations covered identity, filtering, projection, renaming, distinct values,
grouped aggregation, a grouped rank, self/left/right joins, repeated union,
intersection and difference. The ordinary comparison explicitly built the two
branches from known grouping-set membership, then applied the same computed
ordering expressions as the Margin order before applying the downstream verb.
Column order was normalized only for comparing public values and schema; this
probe did not promise a derived result's Margin order.

There were **69** exact matches and **48** ordinary-control failures. Thirty
failures were inherited computed-sort fields after distinct, aggregation,
left-side joins or union; eighteen were ordinary unsupported intersection or
difference operations. The computed-sort failures could be reproduced without
marginplyr, so they were not counted as new defects. Complete bodies/comments
of [#895](https://github.com/sayuks/marginplyr/issues/895),
[#1023](https://github.com/sayuks/marginplyr/issues/1023) and
[#541](https://github.com/sayuks/marginplyr/issues/541) were read to distinguish
those repaired Margin-input boundaries from ordinary downstream Arrow behavior.
No issue was created or modified by this track.

The rank operation produced eighteen ordinary Arrow absorbing-fallback warnings
across actual and control executions: `In dense_rank(z):`, `Expression not
supported in Arrow`, and `Pulling data into R`. This was the caller's later
ordinary operation, not a summary expression evaluated by a Margin verb.
The [warnings](composition-warnings.txt), [CSV](composition.csv) and
[terminal log](composition.log) retained the observations.

## Harness corrections and limits

The first composition experiment compared different public column orders and
included `head()`/`tail()` over results with unspecified order. Those apparent
value differences were not counted. The final comparison normalized column
names, used the corresponding first/last ordinary computed key, and omitted
head/tail from that value-equivalence matrix.

A first dtplyr experiment put a conditional inside `.by`; ordinary selection
evaluation could not resolve its bare column and grouped fixtures correctly
refused an explicitly supplied `.by`. The final script chose between two calls
outside selection. Its next version incorrectly treated an undeclared payload
column as a dimension in the repeated-set oracle; the final oracle omitted only
columns declared as dimensions. Preliminary runs supplied none of the final
counts. The first Arrow-order script used an unquoted reserved word as a list
name; that parse failure preceded all comparisons and was corrected before the
recorded 768-comparison execution.

The environment was arm64 macOS, R 4.6.1, Arrow 25.0.1, dtplyr 1.3.3,
data.table 1.18.6.1 and dplyr 1.2.1. Full [session information](environment.txt)
and [package versions](package-versions.txt) were retained. Arrow initialization
printed macOS cache-size `sysctlbyname` permission diagnostics; the native
controls and final executions still completed. Hosted stores, other versions
or platforms, arbitrary Arrow extension types and caller-defined data.table
classes were not measured. No package suite, build or render ran.

## Replay and terminal evidence

From the package root, execute each script separately in a fresh R process:

```sh
Rscript investigation/2026-11-06-expand-grouping-round2/lazy/arrow-order.R
Rscript investigation/2026-11-06-expand-grouping-round2/lazy/arrow-exact-supplement.R
Rscript investigation/2026-11-06-expand-grouping-round2/lazy/dtplyr-volatile.R
Rscript investigation/2026-11-06-expand-grouping-round2/lazy/composition.R
```

The scripts explicitly disabled testthat parallelism. The executed originals
lived under `/private/tmp/marginplyr-lazy-round2/`, with the archived package as
the working directory. Retained copies changed only evidence-output destinations
to their own directory; all copied scripts parsed successfully. Original
execution handles were collected to terminal exit zero: Arrow-order session
`31743`, supplemental native-array session `72319`, final dtplyr session
`32610`, and final composition session `24757`.
The retained tool transcript for session `31743` reported `match 768`, zero
mismatches/errors and terminal exit zero; no separate raw Arrow-order log was saved. The other terminal summaries are in their logs. No owned execution remained.
Only research evidence was added; no implementation or package test changed.
