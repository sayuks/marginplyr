# SQL expansion projection-width and selection boundaries

Investigated: 2026-11-06
Baseline: `547d77095c2923573216d002f50bef21778247f9`

The earlier [SQL composition oracle](../../2026-11-06-expand-grouping-hunt/sql/README.md)
supplied prior source-shape coverage and exclusions. This track instead asked
whether a valid wide public result became invalid through private Margin-order
columns, and independently challenged the Grouping-selection finding on live
databases. Previous measurements were not counted as new runs.

## S1: a valid public width exceeded SQLite's limit through private order keys

[column-limit.R](column-limit.R) created a two-row SQLite table with 1,999
integer columns: `g = c(2L, 1L)` and 1,998 payload columns. Its plan contained
only the included `g` set. Ordinary source arrangement returned the exact
2-by-1,999 result. Unsorted expansion preserved the source exactly. Last Margin
order instead added two private order columns to the 1,999 public columns,
and collection raised `too many columns in result set`.

The [independent desired-contract run](column-limit-assert.log) used a separate
archive of freshly fetched main at the baseline SHA. Both successful controls
passed before the assertion intentionally exited one. A separate
[1,998-column threshold control](column-limit-control.log) passed all three
comparisons and its desired-contract assertion, exiting zero. It had 1,998
public columns and two private order columns. No source already exceeding
SQLite's public-schema limit was submitted.

The [earlier width probe](wide-projection.R) and its
[log](wide-projection.log) checked 490, 499, 500, 501 and 510 included dimensions.
All fifteen expansion comparisons across none/first/last sorting and all five
ordinary arranged-source controls matched. Those successful cases did not
reach the limit because they had no thousands of additional payload columns.

ADR 0018's one-occurrence amendment made Grouping bits constant in this plan,
and ADR 0031's B-direct boundary included one-set expansions. An ordinary
arranged query expressed the requested public result without exceeding the
engine limit. The package's private projection width was therefore the
distinguishing boundary. A second agent independently read the contracts and
implementation and agreed with the narrow P3 disposition. This established no
promise for arbitrary plans or source schemas beyond backend limits.

The new independent ticket was [#1384](https://github.com/sayuks/marginplyr/issues/1384).
Closed [#980](https://github.com/sayuks/marginplyr/issues/980), whose complete
body and comments were read, concerned compound SELECT term count rather than
result width; this witness had one grouping-set occurrence. No repair was made.

## Independent G1 confirmation

[injected-qualified-independent.R](injected-qualified-independent.R) changed the
specification track's source names to `u` and `v`, used two different rows and
a rollup, and tested live SQLite and DuckDB. Lexical `u` named column `v`.
Direct typed tidyselect selected `v`; a forwarded publicly qualified
parenthesis selected `u` in inspection and omitted `u` in the expanded copies.
An identity-wrapper control selected `v` and matched explicitly assembled
literal branches, including integer missing values and occurrence identifiers.
Both databases returned the wrong dimension for the qualified wrapper.

The [observation log](injected-qualified-independent.log) retained both wrong
results. The [desired-contract log](injected-qualified-independent-assert.log)
was produced against the separate fresh-main archive and intentionally exited
one after all successful controls. This was additional evidence for
[#1383](https://github.com/sayuks/marginplyr/issues/1383), not another unique
defect. The independent script initially had a syntax typo, corrected before
either final replay. The width harness initially compared differing base
data-frame row names, corrected before its successful controls were recorded.

## Replay and environment

Run each script separately from an archive of the investigated package root,
using the absolute evidence path when it lies outside that archive:

```sh
Rscript investigation/2026-11-06-expand-grouping-deep/sql/wide-projection.R
Rscript investigation/2026-11-06-expand-grouping-deep/sql/column-limit.R --assert
MARGINPLYR_HUNT_WIDTH=1998 Rscript investigation/2026-11-06-expand-grouping-deep/sql/column-limit.R --assert
Rscript investigation/2026-11-06-expand-grouping-deep/sql/injected-qualified-independent.R --assert
```

The first command and the 1,998-column control exited zero. The other two
assertion commands intentionally failed on the baseline because their desired
contracts were not met. Ordinary observation modes were also available without
`--assert`. Fresh-main assertion handles `64698` and `92906` exited one;
threshold-control handle `55643` exited zero. The earlier width handle `34550`
and observation handles `5795`/`13473` exited zero. All were retained through
terminal completion. Private in-memory connections owned no external service
or child process and were disconnected by the completed scripts.

The [environment record](environment.txt) held arm64 macOS, R 4.6.1, dplyr
1.2.1, dbplyr 2.6.0, RSQLite 3.53.3 and DuckDB 1.5.5. Testthat parallelism was
explicitly disabled. Other SQL dialects, alternate build limits, other versions,
hosted stores and general performance bounds were not measured. No package
source, package test, build or rendering was changed or run for this track.
