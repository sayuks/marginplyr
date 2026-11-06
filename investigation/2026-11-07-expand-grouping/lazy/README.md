# Deferred dtplyr copy ownership and hidden Arrow partitions

Investigated: 2026-11-07
Baseline: `e1dabaabb392a3791177e5c48e983f780c7c93ec`

This track investigated `expand_with_margins()` through a private `git archive`
copy of the baseline. It read the November 5 lazy greedy note and the November
6 lazy metadata, deep and round-two notes before choosing new boundaries.
Contracts read included `CONTEXT.md`, ADRs 0011, 0015, 0016, 0019, 0020, 0029
and 0033. No implementation or package test was changed.

## Confirmed finding: generated dtplyr copies resolved the caller's namespace operator

The [minimal witness](minimal-copy-namespace.R) constructed an immutable dtplyr
input with one character column and one row, then constructed an included-only
expansion. Only after construction, the root environment gained a namespace
dispatcher intercepting `data.table::copy` and returning a zero-row function.
The caller had written no copy expression. Ordinary collection of the input
returned one row, collection of the already-built expansion returned zero,
and removing the dispatcher restored one row in the same query.

```text
ordinary rows: 1 expanded rows: 0 restored rows: 1
Error: nrow(actual) == 1L is not TRUE
Execution halted
```

The desired-contract invocation exited one; its [log](minimal-copy-namespace-assert.log)
retained that failure. A single included grouping set was sufficient: neither
a Grand total set, a sort request, a summary, a user source expression nor an
identifier was needed. The violated row-copy promise was the opening contract
of `expand_with_margins()`. ADR 0029 assigned branch isolation to marginplyr;
the copy was package-generated work, rather than an authored namespace call.

The distinguishing predictions were: a package-generated copy lookup would
invoke the selective dispatcher even with one included set; an ordinary
dtplyr source defect would also affect ordinary collection, mutation or union;
and a stale or mutated input would remain wrong after removing the binding.
The first prediction held and the two alternative explanations were refuted
by the controls below. The observed execution passed the qualified generated
head through the caller's root environment. Qualification fixed the ordinary
`copy` name collision addressed by #1241 but left the namespace operator
caller-resolved. ADR 0019's generated-namespace amendment and #1355/#1346
provided analogous ownership precedents; the copy was not itself a Contextual
helper, and authored constructor/source expressions retained ordinary lookup.

### Finite matrix and separating controls

[The matrix](copy-namespace.R) completed **216** observations: root, filtered
root, a cache named `copy`, and a renamed cache; single, rollup and repeated
included sets; all three Margin orders; bindings present before construction
or installed before collection; and counting, empty-result or raising
dispatchers. All 216 invoked the selective callback at collection, a total of
408 callbacks. The 72 counting cases kept correct values but invoked unwanted
caller code; 72 cases silently returned no rows; and 72 raised
`COPY_NAMESPACE_INTERCEPT`.

Every construction invoked zero selective callbacks. Every ordinary source,
ordinary identity mutation and ordinary repeated union matched its control and
invoked zero selective callbacks. Removing the namespace binding restored all
216 expected expansions, including the renamed cache. Serialized caller-owned
source bytes stayed unchanged. The [CSV](copy-namespace.csv) retained each
case; the [observation log](copy-namespace.log) retained exit-zero evidence
assertions. A second [desired-contract run](copy-namespace-assert.log) exited
one at `all(result$desired_values)`.

[Shared-boundary probes](copy-namespace-boundaries.R) established the same
one-to-zero row corruption through expansion, summary, nesting and row-wise
nesting. The last verb performed its documented execution during the call.
An authored source filter containing `data.table::copy(rid)` invoked its
dispatcher once under ordinary collection and twice under the single-set
expansion; construction invoked none. Thus a repair had to retain the one
authored callback while protecting the additional generated one. The
[log](copy-namespace-boundaries.log) and [CSV](copy-namespace-boundaries.csv)
retained those observations.

[The copy-as-identity probe](copy-identity.R) tested four source shapes with
and without an identifier. Returning `identity` for the generated copy lost
the included detail keys in five of eight cases: all four cases without an
identifier and the renamed cache with an identifier. Detail rows displayed
`Total`; the latter also duplicated the Grand total occurrence identifier.
All eight original caller tables remained byte-identical. Source destruction
was therefore not established. Two native data.table shallow-copy warnings
were observed in the direct-root identifier case; they were recorded as
warnings rather than counted as new package defects. The [CSV](copy-identity.csv)
and [log](copy-identity.log) retained the eight observations and warning counts.

### Duplicate comparison

All-state Issue searches used `dtplyr source binding`, `dtplyr "copy"
"namespace"`, `"deferred" "copy" "dtplyr"` and `"namespace operator"`.
All-state PR search used `dtplyr "copy" "namespace"`. Complete bodies and
comments were read for #1241, #1271, #1311, #1346, #1355 and #1143. #1241
covered bare copy/cache naming; #1271 covered repeated source evaluation;
#1311 covered generated summary selections and nested masks; #1346 covered
shares; #1355 covered Margin order; and #1143 covered Margin-label helpers.
None described the deferred-copy namespace boundary. The repair tasks were
closed on the tested main, and the new witness still reproduced there. The
coordinator owned final main freshness, additional duplicate checks and Issue
publication, which produced [#1422](https://github.com/sayuks/marginplyr/issues/1422).
No Issue was published or edited by this track.

## Refuted hypothesis: hidden Arrow partition names would collide with expansion internals

[The Arrow oracle](arrow-hidden-partitions.R) completed **288** new comparisons.
It used three rows with ordinary, URI-escaped slash and missing Hive partition
values. The hidden key names were `..marginplyr_arrow_boundary_1`,
`..marginplyr_arrow_dimension_1`, `.env` and `a%b`. Source shapes dropped the key
through `WithSchema()`, projected it away, placed the Dataset on the right of a
join, or renamed it into the grouping dimension. Zero-prefixed directory
filtering was disabled explicitly so the dot-prefixed fixtures had real rows.

NULL and string labels, all three Margin orders and direct collection,
compute followed by collection, and a RecordBatchReader round trip all matched
independently assembled row copies. Source collection had exactly three rows
before each family. Comparison canonicalized by occurrence and source row
identifier, so these were schema/value/multiplicity checks, not independent
physical-order checks. The [CSV](arrow-hidden-partitions.csv) retained all
288 matches, and the [log](arrow-hidden-partitions.log) retained exit zero.

An initial exploratory opening used Arrow's default hidden-directory filter
and read zero rows from the dot-prefixed directory; it supplied none of the
288 comparison counts. Setting `selector_ignore_prefixes = character()`
corrected the fixture before the complete retained run. Initial DuckDB
structured-key exploratory calls also accidentally supplied a Grouping
specification in `.by`; those selection errors supplied no finding or count.
The corrected DuckDB list/struct/ordinary-key controls passed all nine
NULL-label/sort cases, and a precise decimal SQL control retained its
`DECIMAL(38,8)` type and exact string value.

## Replay, environment and limits

From the investigated package root, run each script separately in a fresh R
process. The optional `--source=/absolute/archive/path` selects the source
copy loaded by `pkgload`; output remains beside the script.

```sh
Rscript investigation/2026-11-07-expand-grouping/lazy/minimal-copy-namespace.R --assert
Rscript investigation/2026-11-07-expand-grouping/lazy/copy-namespace.R
Rscript investigation/2026-11-07-expand-grouping/lazy/copy-namespace-boundaries.R
Rscript investigation/2026-11-07-expand-grouping/lazy/copy-identity.R
Rscript investigation/2026-11-07-expand-grouping/lazy/arrow-hidden-partitions.R
```

All observation scripts reached terminal exit zero. The matrix and minimal
desired-contract `--assert` invocations reached the expected terminal exit one.
The Arrow run's original session `49701` was polled to terminal exit zero.
All other final runs returned their terminal status directly; no owned
execution remained. No package suite, build or render ran in this track.

The environment was arm64 macOS, R 4.6.1, dplyr 1.2.1, dtplyr 1.3.3,
data.table 1.18.6.1, Arrow 25.0.1, DuckDB 1.5.5, dbplyr 2.6.0 and rlang 1.3.0.
[Session information](environment.txt) was retained. Arrow printed native
macOS cache-size `sysctlbyname` permission diagnostics but the retained matrix
completed. Testthat parallelism was disabled explicitly. Other platforms,
dependency versions, arbitrary Arrow extensions, hosted storage and arbitrary
custom vector/table classes were not measured. The completed finite controls
did not establish absence of bugs outside those fixtures.
