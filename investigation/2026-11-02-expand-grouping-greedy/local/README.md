# Local expansion additional boundary evidence

Investigated: 2026-11-02
Baseline: `978d7bef4247bf1348af5b90bfab47924db25500` (`origin/main`)
Scope: dated exploratory evidence only; no implementation or package tests

Four hypotheses were measured against the public row-copy, factor-level,
typed-missing-label, empty-plan, and Margin-order contracts. No additional
confirmed defect emerged from those initial local probes. A subsequent small
live SQLite query-graph probe confirmed the composition defect recorded below.
The October 30 and 31
expansion notes supplied prior coverage; their runs were not counted here.

## Additional vector families

Hypothesis: local prototype construction or branch combination might still
reject a supported native class, retain the wrong values, or change a carried
column relative to ordinary dplyr/vctrs branch composition.

`additional-types.R` used an ordinary `bind_rows()` oracle with row-sized
`vec_init()` prototypes. The 27 requested families included ts/mts, zoo,
yearmon/yearqtr, haven-labelled integer/string/SPSS vectors, glue, hms, blob,
bit/bitwhich, hexmode/octmode/roman, difftime, two Surv variants, sf geometry,
s2 geography, XML nodesets, vctrs unspecified, AsIs integer/string vectors,
and language/environment lists. It crossed zero/one/three requested rows,
direct/packed columns, dimension/fixed/payload roles, and three sort choices.
The five fixture-error rows did not represent three admitted source rows for
every requested family: bit/bitwhich exposed only one vctrs element, and
xml_nodeset was not admitted as a vctrs vector.

Of 1,373 recorded observations, 1,240 matched the ordinary value/attribute
oracle after alignment by occurrence and source-row identifiers. Thirty zoo
cases failed ordinary combination. Eighty cases failed package ordering and
ordinary source-key ordering (mts 8; s2 and each Surv variant 24). Five ordinary
fixture errors were excluded. Eighteen package-only refusals were inspected
separately rather than promoted from the raw classification into findings.

Those eighteen refusals concerned direct/packed sf geometry dimensions.
`sfc-controls.R` established that the `vec_init()` geometry prototype was
`POINT EMPTY`, for which both `is.na()` and vctrs missingness were false.
The ordinary union control succeeded but had not supplied a typed-missing
geometry, so it did not prove the NULL-label contract. The package's refusal
to claim a representable typed-missing value was justified. An s2-geography
prototype, by contrast, was missing and its unsorted expansion cases matched.
Read-only Issue searches `sf geometry` and `factor expand` across open and
closed issues identified closed #993/#1000 as existing sf boundaries; neither
was claimed as a new finding. Their complete bodies and comments were read.

This oracle aligned row bags and checked full `all.equal()` column comparison;
it did not independently establish the requested Margin-order sequence or
warning propagation. Successful serialization comparisons established source
immutability for completed value comparisons, not for every error branch.

## Factor identity and ordering

Hypothesis: NA-level identity, sentinel-like levels, or a restored synthetic
level might move values or rows under a Margin order.

`factor-order.R` calculated expected factor integer codes and levels directly.
All 1,944 comparisons matched. Fixtures crossed ordered/unordered factors,
ordinary/empty/Unicode/decomposed-accent/sentinel-like/NA levels, true source
missingness, zero/one/seven rows, explicit and persistent fixed keys,
included-only/rollup/duplicate-occurrence plans, NULL/NA/text labels where
admitted, both synthetic-level positions, and all three sorts. The independent
key used fixed missingness/value followed by dimension bit/missingness/value
and occurrence. It checked ungrouped results, exact factor levels/classes,
integer codes, and key sequences.

An initial harness wrote an `if` expression as `.by`, which evaluated the
environment's factor rather than selecting the column, and created persistent
groups with `.drop = FALSE`, which the public contract refuses. The final run
used injected fixed-column selections and supported default-drop groups; the
final CSV and log were regenerated in full. Those harness observations were
not package defects or counted coverage.

## Zero-column cardinality

Hypothesis: adding an identifier or sorting a zero-column frame might invent
or lose rows, especially for duplicate empty sets and identity products.

`zero-column.R` recorded 1,944 cases over data.frame/tibble, zero/one/four
rows, twelve specifications, three duplicate policies, absent/ordinary/`..1`
identifiers, and three sorts. There were 1,026 matching successful calls and
918 required Package-condition refusals. Success checked row count, exact
public names, identifier values and integer type, and unchanged source bytes.
The constructor algebra supplied expected cardinality: NULL, empty
`grouping_set()`, and identity `grouping_spec()` each represented one set;
explicit unions/products multiplied empty-set occurrences. Empty
`grouping_sets()`, dimension-free rollup/cube (including empty selections),
and duplicate plans under the error policy were refused as documented.

## Structured local Margin order

Hypothesis: partly missing versus wholly missing structured rows might be
misordered, or a fixed structured key might use scalar-component missingness
instead of one row flag.

`structured-order.R` made 180 comparisons over integer matrices, zero-width
matrices, packed and nested packed frames, and plain lists containing NULL,
scalar NA, zero-length and multi-element vectors. It crossed zero/one/eight
rows, dimension/fixed roles, rollup/cube, and three sorts. Every result matched
the ordinary branch-value oracle and independently assembled order key.
Missingness was calculated from scalar components for structured rows and
from NULL for plain lists; value ordering remained delegated to dplyr.
No classed-array sorting promise was inferred. The source serialization was
unchanged in every case.

## Replay and limits

Run each script from the repository root with
`MARGINPLYR_HUNT_OUTPUT` pointing to an output directory to retain its CSV.
The default output destination was `tempdir()`; stdout contained classified
counts. A zero exit from an exploratory runner meant its measurements
completed, and its CSV classifications determined which cases established
successful behavior. Adjacent logs and CSVs recorded the final runs.

No fix, package regression, full-suite run, build, render, external service,
arbitrary custom-class enumeration, dependency-version matrix, or other OS
was part of this track. The results established only the stated finite
inputs, and did not prove every class proxy, caller method, row order, or
future dependency behavior.

## SQLite Boolean aliases across binary relation graphs

An additional probe ran against an independent archived source copy of the
same baseline, under `/private/tmp/marginplyr-local-sql-978d7bef.TUzWsd/source`.
The copy was made with `git archive`; it contained no working-tree evidence
additions. SQLite used a private in-memory connection and one-row fixtures.

The ranked hypotheses were (1) ordinary outer classes on a second-input
composition bypassed final alias repair, (2) unsupported query-graph nodes
blocked the repair even when the Margin class survived, and (3) ordinary
SQLite/dbplyr composition itself failed for these source-table names.
`sql-alias-graph.R --assert` established a fast expected-behavior replay.
Its final run exited 1 after the ordinary source and explicitly computed
expansion controls had passed exact public-name/value equality assertions.
The target `left_join()` with the expansion as its second input instead raised
`no such column: TRUE`. The input held `g = "a"` and integer payload
`TRUE = 7L`; expansion of `rollup(g)` collected directly as the expected
detail and Margin rows. Joining one ordinary key row to that expansion should
have returned the original detail row, as both controls did.

The observation mode recorded 48 calls: sixteen operations applied to the
ordinary source table, explicitly computed expansion, and unmaterialized
expansion. All 32 ordinary-source/materialized controls completed. Ten of the
sixteen unmaterialized-expansion calls failed: both sides of left/inner joins,
the second input of union/union_all, and both sides of intersect/setdiff.
Direct collection, head, collapse, collapse followed by head, and the first
input of union/union_all completed. The logs retained public names, values,
rendered SQL, classes, row counts, and exact diagnostics. These measurements
were not a complete type/order oracle or a claim that different operations
should have the same result cardinality.

Cause mode distinguished the two package-created boundaries. A union with
the expansion first kept `marginplyr_sqlite_typed_result` and succeeded; the
reversed union had an ordinary outer table class and failed. Explicitly
applying the existing derived-query repair helper to the reversed union
made that query execute. A first-input join retained the special class but
had a `lazy_multi_join_query` root; applying the same helper directly still
failed. The helper's traversal admitted select/union nodes and left that join
node untouched. The evidence therefore supported both hypotheses (1) and
(2), and the successful controls refuted (3) for the measured calls. These
were diagnosis-only helper invocations, not package implementation changes.

`collapse()` followed by head changed the Boolean column's name to `column2`
for all three input forms. That shared ordinary behavior was excluded from
the finding. No additional Margin-order or declared-type guarantee for
derived results was requested: the affected contract was an executable query
over its public schema, under ADR 0031's ordinary downstream-semantics boundary.
ADR 0020 still excluded an implicit materialization workaround.

Open/closed Issue searches were `Boolean join`, `Boolean union`, and
`SQLite composition`; the relevant matches were closed #1095, #826, and #976.
The complete bodies and comments of those three were read. The PR search
`Boolean SQLite` found merged #1134 and older related repairs; #1134's full
body and comments were read. #1095/#1134 had established limiting/filtering
and select/union layer repair, #826 direct collection/materialization, and
#976 re-expansion of the public schema. None recorded this binary relation
graph boundary. The coordinator refetched `origin/main` and confirmed it still
named `978d7bef4247bf1348af5b90bfab47924db25500`; its separate FALSE-payload
witness used integer value 19 and reproduced joins on both sides, second-input
union_all, and second-input intersect while ordinary controls passed.
The independent witness was retained under the sibling `sql/` track.
The confirmed binary-relation boundary became
[#1136](https://github.com/sayuks/marginplyr/issues/1136), with no blocker.

The SQL replay defaulted to loading the repository root. Set
`MARGINPLYR_HUNT_SOURCE` to replay against a separate pinned source copy.
Observation and cause modes exited zero on the final script; the expected
behavior assertion mode intentionally remained red. The original process
handles were retained through terminal completion, and no execution remained
active at handoff.
