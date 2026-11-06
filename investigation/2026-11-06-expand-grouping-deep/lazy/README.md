# Lazy source-graph and physical-type expansion probes

Investigated: 2026-11-06
Baseline: `547d77095c2923573216d002f50bef21778247f9`

This track investigated `expand_with_margins()` on Arrow and dtplyr, extending
rather than counting the earlier [November 5 lazy probes](../../2026-11-05-expand-grouping-greedy/lazy/README.md)
and [November 6 metadata probes](../../2026-11-06-expand-grouping-hunt/lazy/README.md).
The contracts were read in `CONTEXT.md`, ADR 0002, ADR 0016, ADR 0020 including
its evaluated-input amendment, and ADR 0029 including its immutable/cache
isolation amendments. The typed-metadata and deferred-source implementations
were read before selecting the boundary cases. No new confirmed package defect
was established by the retained finite comparisons.

## Hypotheses and observations

| Hypothesis | Distinguishing observation | Disposition |
| --- | --- | --- |
| A Grand total set before a detail set corrupts Arrow's physical result type or values through the deferred reader. | Compare each public Arrow column using `Equals()` against explicit ordinary branches with exact typed missing Scalars. | Refuted for the 972 supported comparisons below. |
| Deferred expansion mishandles `RecordBatch`, IPC `FileSystemDataset`, or `UnionDataset` sources, especially a repeated Grand total set. | Compare four independently constructed source containers under three family shapes and three consumption routes. | Refuted within those fixtures. |
| A reused or cached dtplyr source graph is evaluated again for each grouping-set branch or changes caller tables. | Compare lexical predicate counts to one ordinary source evaluation, and compare both original tables byte-for-byte. | Refuted in all 486 graph fixtures. |
| Two cached roots or a self-join lose a local binding while expansion isolates branches. | Use a cache called `copy`, a second independently named cache, and derived ordering on both sides. | Refuted for the cached join/union and self-join fixtures. |
| A columnless Arrow Dataset loses its row multiplicity when identifiers or a repeated empty grouping set are added. | Compare the explicitly collected source row count with the expected identifier vector. | Refuted in all 216 columnless fixtures. |
| Expansion of an earlier dtplyr expansion has silently wrong values. | Re-expand one two-set lazy rollup with a different public identifier. | No silent result: the call refused unknown derived metadata under ADR 0020. This was not treated as a false-refusal finding. |

## Exact Arrow physical comparisons

[The physical-source script](arrow-source-families.R) completed **1,188**
observations: 11 physical types, four source containers, three Grouping
families, all three Margin-order choices, and direct collection, compute and
RecordBatchReader consumption. Of these, **972** matched exact public-column
Arrow arrays and **216** reached an ordinary Arrow ordering limitation. The
[CSV](arrow-source-families.csv) retained every observation and both diagnostics
for the excluded ordering cases; the [terminal log](arrow-source-families.log)
retained the successful terminal summary.

The source types were wide signed/unsigned integers, decimal128, struct,
variable/fixed-size lists, map, dictionary, ordered dictionary, nanosecond zoned
timestamp and fixed-size binary. Wide integers and decimal precision were not
converted to R doubles. Each source had three rows, including a missing key.
The containers were a Table, RecordBatch, IPC file Dataset, and UnionDataset
containing the same file Dataset twice. The families were Grand total first,
a repeated Grand total, and a rollup with a composite `(g, rid)` dimension.
Ordinary branches were assembled from explicit included-column lists; neither
inspection nor another Margin verb supplied the expected family.

For list, fixed-size-list and map dimensions, first/last Margin order failed
because Arrow could not sort that physical type. An ordinary `arrange(source,
g)` also failed for the identical physical type. The two Arrow execution paths
used different wording: `Unsupported type for RecordBatch sorting` versus
`Sorting not supported for type`. These **216** observations were exclusions,
not matches and not new package failures. Successful comparisons canonicalized
by occurrence and source row identifier, so they established schema, values,
types and multiplicities; they did not independently establish physical Margin
order for Arrow.

The initial exploratory run completed the comparisons but hit a harness error
when base data-frame printing was given tibble's `n` argument. A second run
used an overly strict equality comparison between the two native ordering
messages. The final script recorded each message and compared the unsupported
physical-type/order boundary instead. Only the final complete run supplied the
counts above. The initial `UnionDataset$create()` fixture spelling was also
corrected to Arrow's public `open_dataset(list(...))` constructor before any
matrix result was counted.

## dtplyr source graphs and physical Margin order

[The graph oracle](dtplyr-graph-oracle.R) completed **486** comparisons with no
mismatch. It crossed zero/one/three source rows; left/full/inner joins, union,
intersection, difference, cached join, cached union and a self-join; three
Grouping families; NULL/string Margin labels; and all three Margin orders.
The families were Grand total first, a repeated detail set, and a Cartesian
product of two rollups. Two source predicates independently counted their
own invocation, while both caller-owned data.tables were retained for an
immutability comparison.

For every fixture the script first explicitly evaluated the ordinary source to
obtain its rows and invocation counts, reset the counters, and constructed the
expansion. Construction had to execute neither predicate. One collection had
to use the same predicate counts as the ordinary source, even when a source
contributed to two inputs of a self-join. Expected rows were independently
assembled from explicit sets. The sorted cases also compared the physical
result sequence to a separately constructed interleaved Grouping-bit,
missingness and value order; the fixture's character values used ordinary
ASCII spellings. Every schema/value/multiplicity, requested-order,
evaluation-count and caller-immutability assertion passed. These observations
were narrower than a guarantee for arbitrary dtplyr classes or derivations.

The [CSV](dtplyr-graph-oracle.csv) retained the native and expansion predicate
counts; its [log](dtplyr-graph-oracle.log) recorded the final successful run.
An earlier successful value-only run was not added to the final count after
the independent physical-order check was included.

## Columnless Arrow containers

[The columnless oracle](arrow-columnless.R) completed **216** passing
comparisons: zero/one/three initial rows; Table, RecordBatch, IPC Dataset and
UnionDataset sources; one or two empty grouping sets; all three Margin orders;
and all three consumption routes. After dropping every source column, the
script explicitly collected that source to establish its surviving row count.
Expansion had exactly that count per grouping-set occurrence and only the
integer identifier column. The [CSV](arrow-columnless.csv) retained expected
and actual row counts; the [log](arrow-columnless.log) recorded exit zero.

## Prior reports and exclusions

The earlier literal-zero dtplyr projection finding #1366 was already repaired
on the tested baseline: the metadata classifier explicitly admitted `0L` when
the declared output columns were empty. It was not re-reported.

All-state `gh issue list` searches for `dtplyr re-expansion` and
`dtplyr metadata expansion` found no specific re-expansion report. The latter
returned the related closed metadata/admission reports #1366, #1281, #837,
#892 and others. The full body and empty comments of #1241 were read to compare
the `copy` cache-name boundary; that repaired boundary passed in the new graph
matrix. The safe derived-metadata refusal was not published as a new defect.
No issue was opened or edited by this track.

## Replay and limits

From the investigated package root, run each script separately in a fresh R
process. All retained executions reached terminal exit zero and were collected
through their original handles. The scripts wrote their CSVs under this
track's evidence directory. No package source or package test was changed.

```sh
Rscript investigation/2026-11-06-expand-grouping-deep/lazy/arrow-source-families.R
Rscript investigation/2026-11-06-expand-grouping-deep/lazy/dtplyr-graph-oracle.R
Rscript investigation/2026-11-06-expand-grouping-deep/lazy/arrow-columnless.R
```

The environment was arm64 macOS, R 4.6.1, Arrow 25.0.1, dtplyr 1.3.3,
data.table 1.18.6.1, dplyr 1.2.1, vctrs 0.7.3, rlang 1.3.0,
tidyselect 1.2.1 and bit64 4.8.6. [Session information](environment.txt) and
[package versions](package-versions.txt) were retained. Arrow initialization
printed macOS cache-size `sysctlbyname` permission diagnostics inside the
sandbox; the native controls and final matrix executions still completed.
Testthat parallelism was disabled explicitly. No suite, package build or
render ran in this track. Hosted Arrow datasets, other dependency versions,
arbitrary extension types and arbitrary caller-defined classes were outside
these finite measurements.
