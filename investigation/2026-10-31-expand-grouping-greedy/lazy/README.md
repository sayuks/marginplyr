# Lazy expansion composition and hidden-boundary probes

Investigated: 2026-10-31
Baseline: `339315c387f31deace77566f5d3294b15b323865` (`main`)

The Arrow/dtplyr exploration established no new marginplyr defect. It extended the October 30
Arrow filesystem/type and dtplyr mutation sweeps to composition, repeated
expansion, cache derivations, and the newer filesystem isolation boundary.
Only dated evidence was added; no implementation or regression tests changed.

## Composition differential

`composition-matrix.R` made 3,960 classified comparisons: dtplyr/Arrow,
0/1/4 source rows, 22 derived shapes, five Grouping plans, NULL/text labels,
and all three Margin orders. Shapes included filters, projections, renaming,
relocation, ordering, limits, distinct, union/intersection/difference, self and
filter joins, collapse, compute, rename after compute, nested compute/rename,
and expansion followed by expansion with and without an intervening compute.
Plans covered included-only, rollup, cube, omitted-first, and duplicate
occurrences, with a fixed key throughout.

| Backend | Matching values and requested key sequence | Documented metadata refusal | Ordinary source construction/collection failed |
| --- | ---: | ---: | ---: |
| Arrow | 1,800 | 0 | 180 |
| dtplyr | 1,530 | 360 | 90 |

There was no package-only error, value difference, or requested-key-order
difference. The control explicitly collected each admitted source before
local expansion. Row bags compared scalar values and R types after factor
normalization; requested order compared fixed keys, dimensions, and occurrence
identifiers, allowing payload ties. This was not an independent local-expansion
oracle, physical Arrow-type proof, complete column-order proof, warning
comparison, or source-mutation audit.

The dtplyr refusals covered limits, distinct, and repeated expansion sources
whose derived metadata the public query policy did not admit. Ordinary source
failures were unsupported Arrow intersection/difference and dtplyr collapse.
The first run's full-join fixture incorrectly used `names(step)` for dtplyr's
public columns; its empty Arrow fixture used ordinary `filter(FALSE)`, which
Arrow itself refused. Those two builders were corrected, and all 360 affected
comparisons were rerun. The final CSV and summary replaced their initial
classifications. The two terminal logs retain that distinction.

## Hidden filesystem boundary names

`partition-boundary-controls.R` asserted 36 cases over hidden Hive partition
names `..marginplyr_arrow_boundary_1`, its underscore suffix, and an unrelated
name; integer/text partition values; full-schema projection/explicitly narrowed
schema; and all sorts. Two files and the raw public values were required before
each expansion. Every source row appeared once per occurrence, detail values
and typed-missing values matched, public columns had their required order,
and requested dimension/occurrence key order matched. Equal-key payload order
was not asserted. Every case passed.

## Excluded decimal sorting lead

A two-row Arrow decimal64 dimension with values 2.25 and 1.125 expanded under
`.sort = "last"` with detail rows in descending value order. The minimal
expected-order assertion exited 1. The ranked hypotheses were an ordinary
Arrow decimal comparator defect, a generated multi-key sorting interaction,
and an incorrectly generated package order key.

`decimal-order-controls.R` then compared ordinary single-column, missingness,
and union ordering against package ordering for decimal32, decimal64, and
decimal128. Ordinary decimal64 union ordering with the same identifier,
missingness, and value terms returned the same wrong order, and ordinary
single-column sorting raised `NotImplemented: decimal64(18, 4)`. Decimal128
controls returned ascending order. The three full-key union controls matched
package values and retained physical types, excluding this as a new package
finding under ADR 0016. Decimal32's single-column control also raised
`NotImplemented`, while its multi-key controls and package result agreed on
ascending order. This evidence did not establish an upstream cause beyond the
ordinary control boundary or claim an upstream report was filed.

## Independent SQLite Boolean-word replay

`independent-sqlite-boolean.R` independently reproduced the coordinator's
SQLite composition finding using `g = "x"` and a `TRUE` payload of integer 19.
Direct rollup collection retained both payload copies and the public name.
Both `head(result, 1)` and `filter(result, TRUE)` raised `no such column: TRUE`,
with an absent or ordinary occurrence identifier. Ordinary union controls
collected the required values, but their Boolean payload was named `column2`
instead of `TRUE`; they were therefore successful execution/value controls,
not proofs of correct public column names. The script's `--assert` mode
required derived package collection, public names, and payload values and
exited 1 on the baseline. Observation mode printed the failures and exited
zero. The coordinator retained the primary diagnosis and ticket boundary.

## Replay and limits

From the repository root:

```sh
Rscript investigation/2026-10-31-expand-grouping-greedy/lazy/composition-matrix.R
Rscript investigation/2026-10-31-expand-grouping-greedy/lazy/partition-boundary-controls.R
Rscript investigation/2026-10-31-expand-grouping-greedy/lazy/decimal-order-controls.R
```

The composition runner writes a classified CSV under `MARGINPLYR_HUNT_OUTPUT`
(default `/tmp`); setting `MARGINPLYR_HUNT_SHAPES=full_join,empty` selects the
corrected shapes. An exit zero from that diagnostic runner is not a promise
that every attempted backend source was supported. The partition and decimal scripts
require their documented successful controls. Their final invocations exited
zero; the SQLite expected-behavior assertion intentionally exited 1. `jarl check` passed for this evidence directory.

The environment used R 4.6.1, dplyr 1.2.1, Arrow 25.0.1, dtplyr 1.3.3, and
data.table 1.18.6.1 on macOS arm64. Arrow emitted sandbox-denied CPU-cache
`sysctlbyname` diagnostics during initialization in the retained composition
and filesystem logs; execution continued to the recorded terminal outcomes.
The finite probes did not enumerate arbitrary query graphs, callback code,
custom physical types, dependency versions, filesystems, or operating systems.
Earlier investigations were read for coverage boundaries and were not counted
as new execution.
