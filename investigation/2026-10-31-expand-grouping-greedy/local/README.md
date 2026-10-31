# Local expansion follow-up evidence

Investigated: 2026-10-31
Baseline: `339315c387f31deace77566f5d3294b15b323865` (`main`)
Scope: dated evidence only; no implementation or package tests changed

## Confirmed independent findings

### L1: non-array S3 prototypes lost their class

A one-row tibble held `stats::dist(matrix(c(0, 1), ncol = 1L))` as one key
column. Included-only expansion and ordinary dplyr grouping succeeded. A
rollup with a NULL Margin label raised `vctrs_error_ptype2`, saying it could
not combine a dist key with a double. Ordinary mutation using a row-sized
`vec_init()` value followed by `union_all()` succeeded, retaining the
classed numeric values and adding a missing copy of the original payload row.
A native `logLik()` column reproduced the same boundary.

The ranked hypotheses were scalar-subsetting prototype loss, unsupported
ordinary branch combination, and rejection by missingness validation. The
scalar NA subset was an unclassed double; `vec_init()` retained the source
class. Both missing prototypes passed ordinary missingness detection, and
ordinary branch combination succeeded. The public failure occurred during
branch type combination. These observations supported prototype loss and
rejected the other two explanations. `margin_column_info()` constructed
packed and array prototypes through vctrs but left these non-array S3 vectors
on the scalar-subsetting route.

The generic typed-missing Margin-label contract and the ordinary dplyr/vctrs
composition supplied the expectation. This did not request arbitrary
attribute restoration under ADR 0016, preservation of model metadata, or
reconstruction of a pairwise-distance matrix. The classed numeric entries
were the input's row-vector values. Closed #1071 covered a one-dimensional
table with an array dimension; these valid non-array classes remained outside
that repair. This was separate from #1069's missingness-validation boundary.

`prototype-matrix.R` made 1,944 comparisons: dist/logLik, zero/one/three rows,
direct/packed fixture construction, tibble/data.frame/data.table inputs,
dimension/fixed/payload roles, NULL/NA labels, three occurrence plans, and
three sort choices. All ordinary controls succeeded. Expansion produced
1,512 matches and 432 package-only type errors, 216 for each class. Every
failure involved an actual direct dimension; all actual packed, fixed, and
payload controls passed. Every source remained unchanged. Ordinary data.table
construction unpacked the requested one-column packed frame, so the CSV
recorded both requested and actual packed shape. Comparison aligned row and
occurrence identifiers; it did not establish the full requested Margin order.

### L2: public column-name validation reached inner vector metadata

The minimal input held an ordinary string dimension and an unchanged payload:
`g = "a"`, `payload = summary(1)[1L]`. The payload's native class was
summaryDefault/table, with one inner name, Min. Included-only expansion
succeeded. Default rollup expansion failed with
`vctrs_error_names_must_be_unique`, naming Min. at locations 1 and 2.
Ordinary `union_all()` of the original and a branch with g replaced by Total
succeeded, emitted its ordinary inner-name repair messages, and kept both
payload values equal to one. No typed-missing class prototype was needed for
this witness. The top-level input names g and payload were valid and unique.

The ranked explanations were a globally scoped name-repair callback,
malformed omitted prototypes, and invalid top-level input names. A callback
probe over the secondary typed-missing-key witness observed valid outer
names twice and then the inner names Min. and an empty string. The callback
that `bind_margin_branches()` supplied rejected those inner names; default
`vec_rbind()` and ordinary union succeeded. The unchanged-payload witness
ruled out prototype construction. Included-only expansion and direct outer
name checks ruled out malformed public names.

The expansion contract promised one copy of each source row per occurrence;
ADR 0016 delegated column attributes to ordinary dplyr/vctrs composition.
The finding required letting ordinary composition handle its inner metadata,
without weakening public-column validation or specifying arbitrary restored
attributes. Closed #784 preserved caller column names with triple-dot numeric
suffixes. Its public-name callback was a different boundary from the inner
vector metadata reached here. L1 could be repaired independently: L2 already
failed when no classed dimension was omitted.

`inner-name-matrix.R` made 1,836 comparisons across zero/one/three/six rows,
direct/packed fixtures, tibble/data.frame/data.table inputs,
dimension/fixed/payload roles, NULL/NA/text labels where admitted, three
occurrence plans, and three sorts. All ordinary `bind_rows()` controls
succeeded. Expansion produced 1,188 matches and 648 package-only failures.
All zero-row cases passed; failures included nonempty fixed/payload cases
with the default text label. Actual packed cases also failed. Every source remained
unchanged. The comparison aligned row and occurrence identifiers and was not
a full Margin-order oracle.

The matrix used ordinary `bind_rows()` for multi-branch controls. Sequential
`union_all()` changed summaryDefault to table after its first combination,
then itself refused a third original summaryDefault branch; that separate
ordinary limitation was excluded from the matrix oracle. Both minimal
findings used successful two-branch `union_all()` controls. Neither ticket
required resolving that ordinary three-branch limitation.

## Additional executed coverage and exclusions

`extended-types.R` made 1,404 exploratory comparisons over 26 families,
zero/one/three rows, direct/packed shape, dimension/fixed/payload roles, and
three sorts. It included native and classed arrays, zero-width and 4D arrays,
zero-column and nested packed frames, factor NA levels, list/list_of variants,
POSIXlt, lubridate Duration/Period/Interval, wk WKT/WKB/coordinate records,
numeric_version, distributional distributions, complex values, and difftime.
The raw counts were 1,350 `all.equal()` matches, 48 operation-only errors, and six
representation differences; these were signals to classify, not new findings.

All 48 errors involved sorting Date or factor arrays, directly or in packed
keys. `classify-extended-signals.R` reproduced ordinary `arrange()` failures
for all 48 corresponding source-key cases. They were excluded as ordinary
class-ordering limitations. Unsorted typed-missing expansion of those
classes passed after the earlier repairs. The six numeric_version differences
were the empty-integer-vector versus NULL representation of a missing version.
Both representations reported missing and compared equal through vctrs;
the package's scalar-subset representation also supported base version
comparison. No value-corruption finding was inferred from `all.equal()`'s
internal representation complaint.

Preliminary class-admission probes rejected Matrix sparse/dense objects,
rle, and qr at ordinary tibble construction. AsIs-factor and malformed
integer64 fixtures also failed their ordinary controls. They were excluded
before counting executable coverage above. The hunt did not establish support
for arbitrary custom classes, pairwise-distance reconstruction, unrestricted
class attributes, every dependency version, or other operating systems.

## Replay

Run from the repository root:

```sh
Rscript investigation/2026-10-31-expand-grouping-greedy/local/reproduce-dist-prototype.R
Rscript investigation/2026-10-31-expand-grouping-greedy/local/reproduce-inner-name-repair.R
```

Observation mode required successful controls, printed the package failures,
and exited zero. Adding `--assert` required the intended public behavior;
both assertion runs exited one on the baseline. Adjacent logs and
`replay-status.json` recorded those outcomes. These were intentionally red
reproducers, not a fixed regression suite.

Matrix output defaulted to temporary directories. Set
`MARGINPLYR_HUNT_OUTPUT` to a directory to retain CSV output and redirect
stdout for a log. The retained CSVs and logs accompanied their scripts.
Jarl passed over this evidence directory. Broader publication checks and
independent witnesses belonged to the coordinating investigation.
