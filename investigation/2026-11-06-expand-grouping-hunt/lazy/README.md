# Lazy expansion and empty-projection metadata probes

Investigated: 2026-11-06
Baseline: `952a0d9b18fd3ff4631f153432f6024ad62fcac2`

This track investigated Arrow and dtplyr expansion and Grouping-plan inspection.
It read the November 4 boundary/invariants notes and the November 5 greedy and
adversarial lazy-backend evidence before selecting probes. Earlier measured
counts did not count as executions in this track. The primary contracts read
were ADR 0016, ADR 0020, ADR 0029, the public *Fixed columns and grouping
dimensions* and *When marginplyr queries your data* sections, and the source
metadata classifier and deferred expansion adapters.

## Confirmed finding: literal zero-column dtplyr projections were refused

An immutable `dtplyr::lazy_dt(data.frame(g = "a"))` followed by an empty
`dplyr::select()` carried an unambiguous empty schema. Ordinary collection
returned zero rows and zero columns, as data.table's documented columnless
boundary required. Expanding it with `.id = "sid"` instead raised:

```text
Can't safely determine column metadata for `.data` from this dtplyr step.
```

Expanding the explicitly collected input, or a direct empty dtplyr root,
returned the expected empty integer identifier. No recovery of source rows
lost by a columnless data.table was requested. Typed Grouping-plan inspection
likewise refused before a `where()` predicate could be called, although no
columns existed to test. Name-only inspection already returned the correct
empty plan.

The [replay](dtplyr-empty-projection.R) crossed three source sizes, four source
shapes, three equivalent empty selectors, two grouping families, and two
identifier choices. Its 180 construction/inspection observations contained 90
false refusals and 90 admitted representation controls. Empty projection over
a direct root or a row-filtered root was encoded by dtplyr with literal `0L`
in `j`, which the subset metadata classifier rejected as a non-call.
Empty projection after arranging or caching the same filtered input used a
literal column-drop expression and was admitted. The 180 direct-empty-root
and name-only inspection controls passed. Upstream source predicates ran
zero times during construction/inspection, and the empty-selection predicate
was never invoked.

The ordinary observation replay exited zero after recording the mismatch and
controls. `Rscript investigation/2026-11-06-expand-grouping-hunt/lazy/dtplyr-empty-projection.R --assert`
exited one at `all(r$desired)`. The latter command asserted the desired
contract without altering package code. The [CSV](dtplyr-empty-projection.csv)
and both logs retain the observed split.

This was a deterministic empty-schema admission gap rather than the unknown,
value-dependent schema refusal decided by [#625](https://github.com/sayuks/marginplyr/issues/625).
That issue's body and comments were read in full. The admitted-composition
precedents [#837](https://github.com/sayuks/marginplyr/issues/837) and
[#892](https://github.com/sayuks/marginplyr/issues/892) were read in full and
covered grouping wrappers and ordering, respectively. The columnless input
and result limitations discussed by
[#184](https://github.com/sayuks/marginplyr/issues/184) were kept intact: every
expected expansion result here had zero rows.

Duplicate searches used `gh issue list --state all` and `gh pr list --state all`
with `"empty projection"` and `"zero-column" "metadata"`. The first query
returned no issue or PR. The second returned only unrelated issues #461,
#40 and #23 and no PR. Broader `dtplyr metadata` and `zero-column dtplyr metadata`
searches supplied the precedents above but no report of this literal-zero
projection boundary. The parent investigation owned final main freshness and
issue publication.

## Additional executed coverage

The [independent row-copy oracle](expansion-oracle.R) completed **720** exact
comparisons with no mismatch: Arrow and dtplyr, zero/one/four source rows,
identity/filter/arrange/projection sources, rollup/cube/product/repeated-union/
union families, NULL/string labels, and all three `.sort` choices. A fixed
key with a source missing value and a payload row identifier were retained.
Expected rows were assembled from explicit grouping-set lists; the oracle
never used `inspect_grouping()` or another Margin verb to derive expected
rows. Comparison sorted by occurrence and source row identifier, so these
checks established values, types, schema and multiplicities; they did not
independently establish requested Margin order. The run exited zero.

The [Arrow metadata exploratory probe](arrow-metadata.R) exercised Date,
zoned POSIXct, bare units attributes, factor/ordered-factor, difftime and hms
as dimensions with NULL and string labels. It found no new contract violation.
Different timestamp/string formatting and difftime units followed ordinary
Arrow representations; bare attributes were delegated by ADR 0016, and
string-labelled Arrow dictionary dimensions were documented as text. No
cross-backend attribute equality was claimed.

Additional exploratory controls checked zero-column Arrow projections from
zero/one/three rows, with repeated empty grouping sets, with/without `.id`;
all six results kept the expected zero/two/six row counts. dtplyr `distinct()`,
`drop_na()` and `tail()` derived metadata remained conservatively refused;
these arbitrary call representations were not treated as confirmed defects,
since lexical dispatch and their own schema behavior needed a separate
admission argument. Caller functions named `setnames` did not alter dtplyr's
ordinary generated call because dtplyr installed its own data.table wrappers;
that lead was refuted.

## Environment and limits

Executed on arm64 macOS with R 4.6.1, dplyr 1.2.1, dtplyr 1.3.3,
data.table 1.18.6.1 and Arrow 25.0.1. Testthat parallel execution was disabled;
no package suite, build or rendering ran in this track. SQL was investigated
by the parent track. Hosted sources, other backend versions and arbitrary
custom table/vector classes remained outside these measured fixtures.
