# SQL summary and share boundary investigation

Investigated: 2026-11-02
Repository snapshot: `978d7bef4247bf1348af5b90bfab47924db25500`

The checkout was loaded with pkgload. This lane changed no package code or
package tests. The environment was recorded in `../environment.txt`.

## Confirmed: native DuckDB shares reused an earlier volatile value

ADRs 0010/0017 required the denominator to be the source value displayed on
the matching parent or Grand total row. ADR 0014's #919 amendment explicitly
kept stateful and nondeterministic numeric scalar sources eligible.

`native-parent-reproduction.R` used a one-row DuckDB table with g = a and h = x,
a fresh sequence, a numeric source `nextval('parent_stamp')`, Parent share,
and rollup(g, h). Construction left the sequence uninitialized. Collection
returned source values four, five and six, with fractions two, five-sixths
and one. The detail fraction should have been four-fifths. The sequence made
the discrepancy reproducible without random sampling or an assertion about
which grouping-set row the engine evaluated first.

The same script's `--total` mode needed only k = east and g = a. With fixed
k, rollup(g), and Total share, the displayed detail and Grand total values
were three and four; the returned detail fraction disagreed with three-fourths.
Thus multiple Parent windows were sufficient to expose the defect but were
not necessary: a fixed key exposed a Total-share path too.

Both report modes checked the actual bad behavior and exited zero. Their
`--assert` modes required ratios computed from the displayed source values and
exited one. They were red-capable investigation signals, not package tests.

### Hypotheses and independent controls

The ranked hypotheses were optimizer reevaluation of the volatile projection,
duplicated staging in generated SQL, and an incorrect parent-row oracle.
The sequence, one-row examples, explicit Grouping set identifiers, passing
portable controls, and independent ordinary SQL distinguished the first.
The generated SQL contained one grouping relation, rather than the self-joins
removed by #939. A textual single relation did not establish single evaluation.

`../share/ordinary-window-control.R` independently used hand-written SQL,
without a Margin API call. Its native grouping relation and two nested windows
returned seven rows but advanced the sequence fourteen times. A contrasting
MATERIALIZED CTE kept seven evaluations and coherent ratios. That was a
cause-isolating backend control, not a selected repair or a request to perform
construction-time materialization. ADR 0020's query boundary still applied.

`native-parent-matrix.R` completed 96 public constructions/collections and
128 share-column comparisons against the displayed-row oracle:

- One or two variable dimensions; fixed keys absent or present, including
  a missing fixed partition.
- Parent alone, Total alone, or both together.
- Native versus portable aggregation, verified from rendered SQL. Keeping
  occurrences with a requested identifier selected the portable path.
- Sequence alone or sum(x) plus sequence, and text or typed-missing labels.

Thirty-two comparisons failed, all native. All 64 portable comparisons agreed.
Native failures included unpartitioned two-dimension Parent shares and fixed-key
Parent-only/Total-only requests for both one and two dimensions. In combined
requests, Parent failed on those fixtures while Total agreed. The request
shape therefore affected which projected value remained stable. These were
observed sufficient conditions, not an exhaustive classification of optimizer
behavior. All construction-time sequence checks remained uninitialized.

The earlier 16-case `sequence-share-probe.R` separated dimension count, helper,
aggregate presence and adapter without fixed keys. Its two failures were the
two-dimension native Parent cases. `volatile-shares.R` was the exploratory
discovery loop: it completed 36 SQLite cases, then stopped at its first
DuckDB native Parent mismatch. Its planned remaining cases were not counted.

### Duplicate comparison and regression coverage

All-state issue searches `native volatile` and `displayed denominator`, and
the corresponding PR searches, found the closest previous report in
[#919](https://github.com/sayuks/marginplyr/issues/919) and its merged fix
[#939](https://github.com/sayuks/marginplyr/pull/939). Their complete bodies
and comments were read. No other matching live report was found. The relevant
nearby historical results concerned other grouping identities or scopes.

The public contract was the same as #919; the residual mechanism differed.
That fix removed staged-result self-joins. Existing native volatile tests
covered an unpartitioned one-dimension rollup. Its multi-level/fixed-key test
selected portable aggregation with keep plus an identifier. Those controls did
not cover the native failures above. `../share/README-sql-followup.md` retained
the independent reading and ordinary-engine evidence. One follow-up ticket
was recommended for native visible-value binding, covering both helpers and
preserving the existing passing paths, with no blocking ticket.

## Bounded negative results

`duckdb-key-types.R` checked 14 key types: integer, bigint, decimal, hugeint,
date, timestamp, time, interval, UUID, blob, list, fixed array, STRUCT and MAP.
Fixed/variable roles, native/portable aggregation and unsorted/last Margin
order made 112 calls. Every case passed the finite numerical-share oracle and
direct materialization checks. The DuckDB connection explicitly enabled matrix
array conversion, as required by that driver's array error. This did not
establish behavior under its default unsupported array conversion.

`name-boundaries.R` checked 33 output names in ordinary-summary, share-output
and identifier roles on SQLite and DuckDB: 198 Margin cases. Of these, 183
passed collection, direct computation, names and values. Twelve failures used
names refused by ordinary dplyr too (triple dots and an overflowing dot index).
The other three were DuckDB computation of a column named `.data`.
`data-output-control.log` independently showed ordinary dbplyr collection of
that name succeeding and its computation failing with the same tidyselect
diagnostic. These were not promoted to package-specific findings. Comparison
normalized outer data-frame/tibble class because this loop measured values,
schema and materialization rather than widening ADR 0016's class guarantee.

The SQL observations concerned DuckDB 1.5.5 and RSQLite 3.53.3. They established
no other engine/version, optimizer configuration, hosted service, arbitrary
extension class, interruption/concurrency guarantee, or unbounded plan size.
No implementation strategy was selected.

## Replay

Run the retained scripts from the checkout root. The native reproducer accepts
`--total` and `--assert`; the latter intentionally fails on this snapshot.
The matrix and bounded negative scripts record their adjacent CSVs. Logs retain
the observed terminal outputs and rendered SQL. The discovery loop deliberately
stops at its first wrong ratio; its exit one is not a package-check failure.
