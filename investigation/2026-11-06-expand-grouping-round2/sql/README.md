# SQL source-schema and rendering boundary hunt

Investigated: 2026-11-06
Baseline: `9ffb24614a71512170298a3827bc11c80354e742`

This track used a disposable `git archive` of the baseline. The November 2
[physical-type/name track](../../2026-11-02-expand-grouping-greedy/sql/README.md),
November 3 [composition track](../../2026-11-03-expand-grouping-greedy/sql/README.md),
November 5 [adversarial investigation](../../2026-11-05-expand-grouping-adversarial/README.md)
and [greedy investigation](../../2026-11-05-expand-grouping-greedy/README.md),
and November 6 [composition](../../2026-11-06-expand-grouping-hunt/sql/README.md)
and [projection-width tracks](../../2026-11-06-expand-grouping-deep/sql/README.md)
supplied previous coverage and exclusions. Their measurements did not count
as new executions. The baseline included the #1383/#1384 repairs.

## S1: included-only expansion corrupted an ordinary derived input's schema (#1398)

The [primary replay](reproduce-boolean-source-identity.R) created a one-row
SQLite source with character `g = "a"` and integer `TRUE = 7`, then added
`z = row_number(g)` using ordinary dplyr. Direct collection returned exactly
`g`, `TRUE`, `z` and their expected values. All-column selection, no-op mutation
and explicit renaming of the Boolean-word payload succeeded too.

Calling `expand_with_margins(source)` with no optional argument raised
`undefined columns selected`. This was the identity expansion: one included
set, no omitted dimension, no identifier, no Margin order, and no source
sharing CTE. The returned lazy table still advertised the expected schema.
Executing its rendered SQL directly returned `g`, `column2`, `z`. The first
zero-row type-anchor arm wrapped the original source using `SELECT *`; its
Boolean-word output name became `column2` in the compound query. The later
branch correctly restored `TRUE` from a private alias, but the first arm
supplied the result names. Collection then indexed the absent public name.

An unwrapped source and a source renamed to an ordinary payload name before
the window operation both expanded correctly. The three ranked explanations
were: a leaking source type-anchor name, a shared materialized CTE alias defect,
and an intrinsically unexecutable source. Raw SQL/schema inspection supported
the first. The single-set witness excluded the shared CTE explanation; the
successful source, all-column and pre-aliased controls excluded intrinsic
source failure. No instrumentation or implementation change was needed.

[Observation mode](identity-reproduce.log) exited zero after checking its
controls and printing the defect. [Desired-contract mode](identity-reproduce-assert.log)
intentionally exited one on the same SHA. The coordinator independently
reproduced the identity failure with a different three-row `FaLsE` payload and
window expression in a separate archive. That witness was independent evidence,
not part of this track's counts.

The [focused identity oracle](identity-oracle.R) completed 90 comparisons:
five names (`v`, `TRUE`, `FALSE`, `TrUe`, `FaLsE`), zero/one/three rows, and six
source graphs (root, scalar mutation, window mutation, collapse, union, join).
Every ordinary source, all-column selection and no-op mutation control passed.
There were 42 exact public-schema/value matches and 48 expansion failures.
Each Boolean-word name failed for window, collapse, union and join sources
at all three sizes; the ordinary name and every root/scalar-mutation source
passed. The [CSV](identity-oracle.csv) and [terminal log](identity-oracle.log)
retained all final classifications.

The published ticket was [#1398](https://github.com/sayuks/marginplyr/issues/1398). The contract was the public expansion promise to emit one copy of every input
row for each grouping set, together with ADR 0031's public result boundary.
The narrow finding needed no unsupported downstream transformation. It was
separate from #1095/#1136/#1180 and #1349: those concerned ordinary consumers
of an already-built Margin result, direct CTE rendering, or destination
shadowing during compute, whereas this input was an ordinary derived table
before its first Margin operation. The coordinator owned all-state duplicate
searches, complete prior discussions, current-main confirmation and publication.

### Ordinary backend limits kept separate

The [mutating controls](boolean-source-controls.R) showed that ordinary
mutation replacing `g` and an ordinary branch union also failed on these
Boolean-named derived sources. Ordinary rename failed for opaque collapse
and union sources, although it succeeded for the window source. Those
observations did not establish a new promise for arbitrary ordinary derived
operations. The identity witness instead used ordinary source/selection/no-op
operations that actually succeeded, and required no branch mutation or union.
The [log](boolean-source-controls.log) preserved that distinction.

The [minimization run](minimize-boolean-source.R) crossed root/scalar/window/
collapse/union sources with included-only/rollup plans, absent/present
identifiers and none/last sort. Its [final log](minimize-boolean-source.log)
showed the single-set and no-identifier failures alongside successful controls;
it supplied separating observations rather than additional independent bugs.

## Additional finite coverage

| Probe | Final observations | Disposition |
| --- | ---: | --- |
| [Source-graph/name matrix](graphs.R) | 1,728 | SQLite: 660 exact matches, 132 candidate expansion errors, 72 ordinary-source errors. DuckDB: 828 exact matches, 36 ordinary-source errors. No collected value mismatch. |
| [Rendering options](render-options.R) | 928 executed option comparisons | All matched default collection: 416 SQLite and 512 DuckDB. Twelve SQLite default-construction/collection populations were excluded first because they already hit S1. |
| [Volatile payload copies](volatile.R) | 96 | Eight SQLite Boolean-name populations hit S1; the other 88 retained equal observed random payloads across every copy of each source row and the expected row counts. |

The broad graph matrix used eight payload spellings (`v`, `TRUE`, `FALSE`,
`.env`, `.data`, `..0`, `NULL`, and a private-prefix collision), eighteen
source graphs, both NULL/text labels, all three sort choices and both live
engines. Its graphs included filtering, distinct, ordered limit, window and
rank filtering, collapse, min/max slicing, union/set operations, semi/anti/
left joins and grouped counts. Explicit source collection supplied the input
rows; local expansion supplied the comparison result. This broad comparison
was not an independent proof of all local Grouping semantics or physical order.
The [CSV](graphs.csv) retained every row classification. The 132 candidate
errors were not all asserted as new package-only multi-set defects, because
ordinary mutating controls also failed as described above. S1's 90-case
identity oracle supplied the stronger controlled boundary instead.

Rendering comparisons crossed `cte`, `use_star` and `qualify_all_columns`
booleans with ordinary/Boolean/pronoun/dot-like payload names, four source
graphs, two labels and none/last sort. The [log](render-options.log) recorded
the twelve prior default failures; the [CSV](render-options.csv) contained
only the 928 actually executed option comparisons. Comparisons canonicalized
rows, so they established schema and row-value multiplicity under these options,
not an independent Margin-order proof.

The volatile probe used `RANDOM()` payloads in ordinary, window and repeated
expansion sources. It checked observed equality per original row identifier
and six/twelve total rows, rather than directly counting backend evaluations.
The [CSV](volatile.csv) and [log](volatile.log) retained the 88 successful
observations and eight known schema errors. It did not establish general
volatile semantics for other functions or unmeasured source graphs.

## Replay, environment and limits

Run each script separately from a checkout or archive of the investigated
package. Set `MARGINPLYR_HUNT_OUTPUT` to a disposable directory for matrix
CSVs/RDS files; the default was `/private/tmp/marginplyr-sql-round2`.

```sh
Rscript investigation/2026-11-06-expand-grouping-round2/sql/reproduce-boolean-source-identity.R --assert
Rscript investigation/2026-11-06-expand-grouping-round2/sql/identity-oracle.R
Rscript investigation/2026-11-06-expand-grouping-round2/sql/graphs.R
Rscript investigation/2026-11-06-expand-grouping-round2/sql/render-options.R
Rscript investigation/2026-11-06-expand-grouping-round2/sql/volatile.R
```

The first command deliberately failed its desired-contract assertion; without
`--assert` it completed after successful controls. The other commands completed
at exit zero and printed classifications, including known failures. They were
investigation probes, not passing package regression tests or Review-ready
results. All used private in-memory databases and disconnected their owned
connections. No child process or external service was started. Testthat
parallel execution was explicitly disabled in the matrix and primary replays; no package tests ran.

The broad graph handle `47639`, final minimization `20034`, mutating controls
`75780`, final identity oracle `59732`, rendering `38734` and volatile `72212`
were retained through exit zero. Primary observation and desired-contract
executions completed directly at exits zero and one. Initial minimization
and control drafts had syntax/printing errors; the first identity-oracle draft
shadowed `identity()` in its error handler. Those drafts were corrected before
the final runs and supplied no counts. Published matrix scripts only added
configurable output-directory handling to the measured scratch scripts.

The [environment](environment.txt) was arm64 macOS, R 4.6.1, dplyr 1.2.1,
dbplyr 2.6.0, RSQLite 3.53.3, DuckDB 1.5.5, rlang 1.3.0, vctrs 0.7.3 and
tidyselect 1.2.1. No live PostgreSQL/SQL Server, hosted store, alternate engine
build/version, other operating system, arbitrary collation, driver extension
class or unbounded resource behavior was measured. No package code, package
test, build, rendering or implementation fix was changed or run for this track.
