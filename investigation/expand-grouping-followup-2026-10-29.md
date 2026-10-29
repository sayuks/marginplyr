# Expansion and Grouping specification follow-up bug hunt

Investigated: 2026-10-29
Baseline: `59d55a7867ffc0255f75be32a233d9d475bcb7fb` (`main`)
Scope: investigation and independently actionable tickets; no implementation fixes

Four parallel tracks investigated Grouping specification evaluation, local and
dtplyr expansion, Arrow expansion, and SQL source composition. The main branch
already included the fixes from the earlier
[2026-10-29 investigation](expand-grouping-bug-hunt-2026-10-29.md).
Those findings were used for deduplication rather than counted again.
All observations below refer to the pinned baseline, not to a promise about a
later package version.

The environment was macOS arm64, R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0,
tidyselect 1.2.1, rlang 1.3.0, vctrs 0.7.3, dtplyr 1.3.3,
data.table 1.18.6.1, Arrow 25.0.1, DuckDB 1.5.5, RSQLite 3.53.3,
RPostgres 1.4.10, PostgreSQL 17.11, and sf 1.1.2. PostgreSQL used a disposable
cluster with a private Unix socket and no TCP listener; it was stopped after
the probes. No live SQL Server was available.

## Findings and ticket boundaries

| Finding | Priority | Ticket | Independently verifiable behavior |
| --- | --- | --- | --- |
| A1 | P2 | [#992](https://github.com/sayuks/marginplyr/issues/992) | Isolate expansion columns from hidden Arrow Dataset partitions |
| L1 | P2 | [#993](https://github.com/sayuks/marginplyr/issues/993) | Put active sf geometry in its promised leading Margin-column position |
| S1 | P2 | [#994](https://github.com/sayuks/marginplyr/issues/994) | Enclose ordered SQL Server inputs without illegal inner ORDER BY |

All three tickets were published with `ready-for-agent` and no blockers. Each
could be fixed and verified independently; none required a preceding refactor.
The Arrow value, identifier, alias-suffix, and ordering manifestations were
kept together because they crossed the same hidden source-schema boundary.
The SQL Server finding was a static translation defect, established by emitted
SQL and the vendor's documented dialect rule, rather than a measured server
error. The other two findings were executed against their actual backends.

## Replay and evidence bundle

Run from the repository root:

```sh
Rscript investigation/2026-10-29-expand-grouping-followup/reproduce.R
Rscript investigation/2026-10-29-expand-grouping-followup/reproduce.R --assert
```

The first command prints observations after requiring the successful controls.
The second requires the documented behavior and exits nonzero on this baseline.
Its four false predicates were the ordinary Arrow hidden `sid` case, the Arrow
hidden dimension-alias case, sf Margin-column order, and SQL Server enclosure.
It took about two seconds; a retained
[output log](2026-10-29-expand-grouping-followup/reproduce.log) records the run. Logs omit trailing whitespace.
This script is dated evidence, not an installed regression suite or a fix.

The adjacent scripts preserve the larger selection, capture, Arrow-type,
partition, sf-order, and SQL-composition matrices. They load the working tree
and write detailed results under `/tmp`. The SQL-composition script adds a
PostgreSQL connection when `PGHOST` is set, using the driver's standard
connection environment variables; otherwise it runs SQLite and DuckDB. The
measured three-backend run used all three connections. The focused replay
requires Arrow and sf as well as the ordinary development dependencies.

## A1: hidden Arrow partitions replaced values and identifiers

An ordinary Hive Dataset partition named `sid` was omitted from the selected
public input, leaving `g = 9, 8` and `row_id = 1, 2`. Expansion of `rollup(g)`
with `.margin_label = NULL` and `.id = "sid"` returned partition values as
Grouping set identifiers: within each grouping-set occurrence, identifiers
varied with the source row. Ordered by `row_id` and whether `g` was missing,
the expected identifiers were `1, 2, 1, 2`; the observed identifiers were
`1, 1, 2, 2`. The four rows existed, but their occurrence identity was wrong.
No internal-looking caller name was needed for that reproduction.

A second fixture made the value corruption visible. Parquet files held
`g = 9, 8`; a hidden integer Hive partition named
`..marginplyr_arrow_dimension_1` held `1, 2`. Selecting only `g` and `row_id`
returned the correct two rows. Expansion with the same rollup and identifier
silently returned `g = 1, 2` in both occurrences, replacing both source values
and the omitted-dimension missing labels. Comparing row counts alone would
have passed this result.

Three discriminating controls succeeded: an included-only grouping set,
ordinary Arrow mutation followed by `union_all()` over the same selected
query, and expansion after explicitly computing that query. The partition
fixtures were checked by raw collection and by the presence of two files in
two partitions before expansion. The ordinary hidden-identifier control used
the same `sid` output name. String labels and NULL labels reproduced the
dimension corruption under all three sort modes.

Further witnesses reached an underscore fallback after a public payload had
occupied the first private alias; a hidden `..marginplyr_arrow_id_1` corrupted
an identifier requested as `..1`; and hidden `..marginplyr_sort_1` corrupted
an unexposed occurrence identifier used for Margin order. The latter returned
`g = 9, NA, 8, NA` where `8, 9, NA, NA` was required. Filtered, collapsed, and
renamed query variants retained the defect.

The observed source of the discrepancy was alias allocation against public
query names while Arrow retained hidden partition names in its source graph.
The expansion union's projection could match those partition names and restore
the partition value over a computed column. The dimension alias workaround in
`expand_margin_union()` and identifier/sort allocation therefore shared this
boundary. Closed #750 repaired visible partition dimensions; #979 repaired
dot-dot-name validation. Neither covered hidden partition collisions with
these new columns. ADRs 0016 and 0020 supplied the property-ownership and
construction-time laziness constraints for a fix.

The retained partition matrix ran 840 cases: five scenarios, integer/string
partitions, seven input shapes, empty/populated inputs, NULL/text labels, and
three sort choices. It recorded 600 passes, 96 value differences, 16 ordering
differences, and 128 parse errors from type collisions. Table, InMemoryDataset,
and explicitly materialized controls passed all 360 of their cases; an
unrelated hidden partition name passed all 168 of its cases. The ordinary
hidden `sid` witness was an additional focused case, outside that matrix.

## L1: active sf geometry moved behind the payload

The minimal input was an sf frame with `g = c("b", "a")`, integer row IDs,
and two point geometries in active column `geometry`. Expansion with fixed
`.by = geometry`, `.grouping = rollup(g)`, `.id = "sid"`, and a NULL label
returned columns `g, sid, rid, geometry`. The required order was
`geometry, g, sid, rid`.

The same columns carried by a tibble returned the required order. Changing
only the sf identifier name to `..1` also returned the required order. The
ordinary-name path in `reorder_margin_columns()` dispatched to sf selection,
whose method moved active geometry to the end; the supported dot-dot-name
path took a different projection. Ordinary sf selection confirmed that method
behavior, but did not remove marginplyr's own promise: ADR 0016 explicitly
assigns leading Margin columns in Grouping-plan order to marginplyr while
leaving arbitrary class and attribute restoration to dplyr/vctrs.

The focused matrix ran 216 cases: three input geometry positions,
empty/two-row inputs, absent/ordinary/dot-dot identifiers, three sort choices,
and four fixed/dimension/composite plan shapes. All 144 absent/ordinary-ID
cases violated column order; all 72 dot-dot-ID cases had the required order.
Values and row counts matched the tibble controls, and the source was unchanged
in every case. Closed #951 concerned intermediate sf metadata during nesting;
this finding concerned expansion's final public column order.

## S1: source enclosure made an ordered SQL Server query invalid

An arranged lazy frame using `simulate_mssql()` rendered this valid input:

```sql
SELECT * FROM [df] ORDER BY [v]
```

Calling `expand_with_margins()` with default arguments, without any Grouping
specification or identifier, rendered:

```sql
SELECT [g], [v]
FROM (
  SELECT * FROM [df] ORDER BY [v]
) AS [q01]
```

[Microsoft's ORDER BY limitations][mssql-order] prohibited ORDER BY in a
derived table or subquery without the qualifying limiting clauses. The emitted
inner SELECT had neither TOP nor OFFSET. Ordinary mutation of the ordered
source kept ORDER BY at the top level, and ordered-head input carried TOP 2.
An unordered input had no such placement defect. Included-only and rollup
plans also retained the invalid inner ordering.

The generic enclosure in `sql_margin_input_relation()` rendered the entire
input to opaque SQL before wrapping it, so dbplyr's normal treatment of a
non-limiting input order could no longer remove it. The enclosure had been
extended to retain PostgreSQL input selection in #977. This was a separate
SQL Server dialect consequence; the PostgreSQL controls in this investigation
succeeded. A second investigator independently reran the SQL generator and
read the vendor restriction. No exact SQL Server error message, live execution
result, or SQL Server version coverage was inferred from that static evidence.
The ticket requires preserving row-selection limits and usable window-order
metadata while correcting the dialect-specific enclosure.

[mssql-order]: https://learn.microsoft.com/en-us/sql/t-sql/queries/select-order-by-clause-transact-sql?view=sql-server-ver17#limitations

## Successful comparisons, excluded leads, and limits

- Grouping selection differential used seed `20261029`, four column families,
  and expression trees of depth at most two: 1,834 generated expressions,
  1,823 comparable expressions, 1,474 matching successful selections, 349
  matching External-condition classes, and no mismatch. Eleven double-bang
  ASTs were excluded because the direct tidyselect oracle and public rlang
  capture did not perform the same injection phase. Qualified/computed heads,
  selection operators, typed predicates, helper arguments reading selection
  data, and lexical wrappers were exercised.
- The capture matrix made 1,515 comparisons across all five constructors,
  three public consumers, nested quosure environments, parentheses through
  eleven pairs, injection, splicing, embracing/forwarding, and masked
  constructor-shaped calls returning fourteen ordinary selection values.
  Answers or condition classes matched ordinary-function controls, and those
  constructor-shaped functions ran once per call. Existing #260/#873/#946/#975
  supplied evaluation-budget and typed-selection boundaries; no new Grouping
  specification finding was established.
- The main Arrow type matrix made 2,052 comparisons over 38 valid families,
  Table/RecordBatch/InMemoryDataset inputs, empty/populated data, three labels,
  and three sorts. It recorded 1,902 matches and 150 errors also raised by the
  ordinary Arrow controls. Numeric widths, decimal precision, temporal units
  and time zones, binary widths, extension vectors, lists and structs were
  included. Seven invalid initial fixture builders were excluded before
  package admission. A supplemental dictionary/map/narrow-decimal/float16
  matrix completed 864 cases: 762 matches, 42 shared backend errors, and 60
  invalid text-label oracle comparisons excluded from interpretation. No
  package-only type discrepancy was established by either matrix.
- The live SQL matrix made 2,820 comparisons across SQLite, DuckDB, and
  PostgreSQL, nine name sets, seven input derivations, three plans, two labels,
  and three sorts, including selected-input and direct-materialization checks.
  It recorded 2,774 matches. Thirty SQLite Boolean-word union-input failures
  were also exposed by ordinary backend projection/renaming, and sixteen
  PostgreSQL `.env` materialization refusals also arose in ordinary compute;
  neither was promoted into a new package finding. Six source derivations
  involving ordinary `.env` ordering already failed before expansion and were
  excluded. The SQL oracle compared names, row counts, and character-normalized
  row bags; it was not a general physical SQL-type oracle or an ordering proof.
- A seeded local/data.frame/data.table/dtplyr exploration made 3,500
  iterations over twenty key-type families, explicit occurrence plans,
  fixed keys, duplicate retention, labels, and sort choices. Of these, 272
  inputs were refused by source conversion, 2,397 matched, 691 had matching
  package-call refusals, and 135 dtplyr-only failures also failed ordinary
  upstream union/order controls. Four remaining sort candidates matched
  data.table's ordinary NA/NaN fixed-key tie order; a fifth disappeared when
  the plain-list oracle derived encounter ranks from the source and plan
  rather than from the already-sorted output. These were not new findings.
  That exploratory oracle required those manual classifications; the retained
  focused sf script carries its separate, fully checked 216-case matrix.
- Valid zero-column inputs produced 576 additional calls without a candidate.
  An immutable dtplyr graph matrix made 216 calls: 162 matched local results
  and 54 raised the documented metadata refusal. No source mutation occurred
  in those 216 graph cases.

The four focused existing test files for the grouping interface, Grouping
plan, inspection, and expansion operation passed on the baseline. No package
regression or implementation fix was committed. The retained `--assert` replay
intentionally failed the new contracts rather than claiming they were fixed.

The investigation did not enumerate arbitrary R code or closure environments,
unbounded recursive/exponential plans, every package version, all SQL dialects,
or every external service. Prior algebra/capture investigations were read for
context and were not counted as new executable coverage. The SQL Server
limitation remained an explicit live-execution gap. Successful comparisons
established only the measured inputs.

## Publication checks

The changes were confined to dated investigation prose and evidence scripts
under the existing package build exclusion. Under
[Local review-ready checks](../design/agents/local-checks.md), they were
Repository-only; the package Review-ready check was not required. The package
linters, context-budget verifier, document-reference verifier, focused baseline
tests, and diff checks were run separately. Their final outcomes and the
Standards/Spec review record belong to the pull request.
