# SQL composition boundaries

Investigated: 2026-11-03
Baseline: `4b34fdecb2d8a41034b3d9b5d5af8e51592555e1`

## Feedback loops and findings

`composition-probe.R` made 1,152 exploratory comparisons over live SQLite and
DuckDB, four public-name families, three sort choices, sixteen compositions
and three SQL-option choices. Its ordinary reference was an explicitly
materialized expansion. The raw CSV classified 716 matches, 302 ordinary
errors, 110 differences and 24 expanded-query errors. Those classifications
were leads, not a count of defects or a proof that every compared population
had the same semantic preconditions.

`candidate-controls.R` and `cause-controls.R` narrowed those leads. Row-number,
window filtering and finite-head comparisons lacked a complete deterministic
ordering key after derivation, so their differing rows were excluded. Ordered
DuckDB self-unions failed on ordinary arranged sources too. Some SQLite
semi/anti-join controls executed but lost Boolean public names themselves;
those were not valid exact-schema oracles for a package-only defect. Bare
table CTE rendering also had an ordinary dbplyr failure, so the final CTE
control used an ordinary filter that produced a query the backend supported.

Two remaining observations had successful ordinary controls and deterministic
public-schema assertions:

- Collapsing a sorted SQLite result exposed physical ordering columns that
  direct collection hid. The one-row integer included-only example required
  neither omitted dimensions nor Boolean names. The collapsed query reported
  only `g`, but its collection contained `g` and two private ordering columns.
- Direct CTE collection of a SQLite expansion carrying `TRUE` failed with
  `undefined columns selected`. Raw DBI execution exposed `column2`, whereas
  default direct collection and the filtered-source CTE control retained
  `TRUE` and the expected values.

`focused-boundaries.R` ran 432 fixtures and recorded both boundaries, yielding
864 observations. It crossed empty/one/three-row sources, `TRUE`, `fAlSe` and
ordinary names, four public roles, three sort choices, two label choices and
included-only/rollup plans. Every direct default collection succeeded. All
288 sorted collapse observations had extra columns; 144 unsorted controls
had the expected schema. All 288 Boolean-word CTE observations failed; all
144 ordinary-name CTE observations matched the schema. All 36 ordinary
filtered-source CTE controls succeeded. This matrix checked schema boundaries;
it was not a general physical-type or independent Margin-order oracle.

## Cause probes

For collapse the ranked explanations were direct-collector-only hiding,
ordinary collapse limitations, and special-name/label conversion. Raw direct
SQL and collapsed SQL exposed the same physical columns; ordinary collapse
preserved its schema, and an integer included-only NULL-label witness failed.
Selecting all reported public columns after collapse could still retain the
hidden columns because the query optimizer kept a star projection. These
observations supported a missing public-schema boundary when the direct
renderer became an ordinary opaque relation.

For CTE collection the ranked explanations were public aliases lifted into a
CTE, an invalid source schema, and a collector-only problem. The ordinary
filtered-source CTE control succeeded. Raw direct companion SQL already
returned the wrong Boolean-word name, while the stored ordinary public query
rendered the correct name under the same option. That distinguished direct
typed-companion execution from the public lazy graph repaired by PR #1150.

No package source was modified to test these explanations.

## Independent witnesses, main and duplicate checks

`independent-witness.R` used two different values, mixed-case `fAlSe`, an
omitted-first plan, a NULL label, first Margin order and an occurrence ID.
Both boundary assertions failed independently. A fresh main fetch still named
the baseline SHA. Observation mode against a separately archived copy of that
SHA reproduced both defects; `current-main-confirmation.log` records it.

`independent-dtplyr-witness.R` additionally confirmed the lazy track's separate
scalar-prototype finding against that archived source: a distance of five and
payload 19 combined successfully through ordinary typed mutation/union, while
an omitted-first expansion with `NA_character_` raised a dist/numeric mismatch.
The caller's table remained unchanged. Its assertion exited 1 as expected.

All-state Issue searches used `collapse`, `collapse SQLite`, `SQLite internal
columns`, `hidden SQLite`, `CTE SQLite`, `Boolean CTE`, and `sql_options`.
Pull-request searches used `SQLite collapse` and `Boolean CTE`. Selected search
JSON is retained here. Complete bodies and comments were read for #976, #826,
#1136, PR #989 and PR #1150, with copies retained here. #976/#989 covered
re-expansion by a second Margin verb. #826 covered default direct Boolean
results. #1136/#1150 covered binary inputs, including their derived-query CTE
paths. Neither the ordinary collapse witness nor direct-companion CTE witness
was covered by those repaired acceptance paths. No matching separate report
was found in those searches.

## Replay and limits

Run from the repository root. Each command intentionally exited 1 on the
baseline after its ordinary controls succeeded:

```sh
Rscript investigation/2026-11-03-expand-grouping-greedy/sql/reproduce-collapse-schema.R --assert
Rscript investigation/2026-11-03-expand-grouping-greedy/sql/reproduce-cte-boolean.R --assert
Rscript investigation/2026-11-03-expand-grouping-greedy/sql/independent-witness.R --assert-collapse
Rscript investigation/2026-11-03-expand-grouping-greedy/sql/independent-witness.R --assert-cte
Rscript investigation/2026-11-03-expand-grouping-greedy/sql/independent-dtplyr-witness.R --assert
```

Observation modes printed the measurements and exited zero. A successful matrix
runner meant that measurements completed, not that a defect was fixed. All
scripts accepted `MARGINPLYR_HUNT_SOURCE` for a separately pinned source copy.
`TESTTHAT_PARALLEL=false` was explicit. Every owned execution was collected to
terminal completion, including sessions `82441`, `89743`, `66519`, `42106`,
`1758` and `73380`; the first two and both matrix/cause sessions exited zero,
the primary CTE assertion exited one and the archived-main observation exited
zero. Immediate assertion executions returned their terminal exit one without
a live session. The environment file at the bundle root records versions.

Two initial minimal scripts had harness-only faults: lazy-table `names()` was
used instead of `colnames()`, and an unquoted `$TRUE` did not parse. Both were
corrected before the retained red assertions. They supplied no defect evidence.

Fixtures used private in-memory databases. No live PostgreSQL/SQL Server,
hosted source, other operating system, other dependency version or arbitrary
backend graph was tested. Derived Margin order and direct-result type
restoration were not added to the schema contract. The successful finite
observations established only the stated fixtures.
