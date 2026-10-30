# Local expansion evidence

Investigated: 2026-10-30
Baseline: `28c449aa03fd5289916f0673e41c848cc126999e` (`main`)

Only investigation evidence was written. No implementation or package test was
changed. Run the scripts from the repository root; matrix output defaults to a
temporary directory and can be retained with `MARGINPLYR_HUNT_OUTPUT`.

## Confirmed findings

### L1: classed structured missingness validation (#1069)

A one-by-one Date matrix expanded successfully under an included-only plan.
An unsorted rollup with a NULL Margin label raised
`expressionStackOverflowError`. The ordinary mutation/union control returned
the expected two-by-one Date matrix with one original and one missing row.
A packed dimension containing a factor matrix failed in the same validation
boundary although the direct factor-matrix control succeeded after #1040.

The ranked hypotheses were recursive representability validation, prototype
shape loss, and unsupported ordinary branch combination. The prototype had the
expected shape and all scalar components were missing; ordinary combination
succeeded. Calling `vec_detect_missing()` on the Date prototype reproduced the
recursion, whereas `is.na()` returned the expected missing components.
These controls isolated the first hypothesis. The public matrix/array and
packed-dimension label contracts supplied the expected behavior.

A table-valued matrix exposed a false refusal at the same validation gate.
Its prototype had shape one by two and both counts were `NA_integer_`.
Ordinary mutation/union yielded a two-by-two table with an entirely missing
omitted row. Its vctrs equality proxy instead expanded into `Var1`, `Var2`,
and `Freq`; the surviving column dimnames made `Var2` nonmissing. Consequently
`vec_detect_missing()` returned two FALSE flags for the one-row prototype.
Unclassing the prototype returned one TRUE flag. The Package condition saying
that a typed missing value could not be represented was therefore not evidence
that its scalar components lacked a missing value.

The focused matrix ran 300 comparisons: nine families, zero/one/three rows,
two matrix widths and a three-dimensional shape (Surv used its ordinary
two-column matrix),
direct/packed dimensions, and NULL/NA labels. Every ordinary control succeeded
and every included-only result matched its input. Rollups produced 126 matches,
138 recursion errors, and 36 false table refusals. Date, POSIXct, and difftime
contributed 36 recursion errors each; Surv contributed 12; packed factor
matrices/arrays contributed 18. Plain integer, units, integer64, and direct
factor controls contributed the matching results.

Related rvar observations also had `is.na()` and all draws missing while their
vctrs equality proxy retained a nonmissing chain-count field; ordinary
mutation/union represented the missing draws successfully. This was additional
classed-array evidence for the validation boundary, not a separate ticket or
an instruction to add a new package dependency.

Ordinary sorting of several classed-array families also recursed. Those
upstream sorting failures were excluded: the adopted finding required only
unsorted row copying. No general guarantee of arbitrary attribute restoration
or repair of vctrs ordering was inferred.

### L2: one-dimensional table prototype loss (#1071)

A native `table(c("a", "b", "b"))` column was accepted by ordinary grouping
and included-only expansion. An unsorted rollup with a NULL label raised
`vctrs_error_ptype2`, reporting incompatible `table[1d]` and integer branches.
Ordinary mutation with `vec_init()` followed by union returned four rows and
retained the table type and one-dimensional shape.

The ranked hypotheses were prototype extraction stripping the table shape,
ordinary union not supporting the type, and a sorting-only defect. Ordinary
union succeeded and unsorted expansion failed, excluding the latter two.
The prototype extraction in `margin_column_info()` used `x[NA_integer_]` for
a one-dimensional table and received an ordinary integer instead of a table.
`vec_init(x, 1L)` retained the shape and class. This was independent of L1:
one-dimensional table missingness itself returned TRUE and passed validation.

The focused 216-case matrix crossed integer/double counts, zero/one/three rows,
table/unclassed-array inputs, NULL/NA labels, rollup/reversed/duplicate plan
occurrences, and all three sorts. All 108 table expansions failed with the type
incompatibility; all 108 plain-array controls matched the ordinary value/type
oracle after alignment by row and occurrence identifiers. Every ordinary
mutation/union succeeded, and all 216 caller inputs remained unchanged.
This matrix compared value bags and types; it did not independently establish
all Margin-order relationships.

## Additional candidate sweep and exclusions

`additional-type-sweep.R` exercised 17 admitted type families across three row
counts, dimension/fixed/payload roles, NULL/NA/text labels, and three sorts:
1,377 comparisons. The families included POSIXlt, zoo yearmon/yearqtr, haven
labelled/labelled_spss, Surv, ts, table, complex, raw, distributional
distributions, posterior rvar, and Date/POSIXct/difftime/units/integer64 matrices.
Expression and timeDate objects failed their vctrs slicing prerequisite in six
attempts and were excluded before the comparison count.

The raw classifications were 1,089 identical matches, 90 Package refusals,
162 external errors, and 36 identity-only differences. These were candidate
classifications, not 288 independent defects. The external errors included
144 classed-array/table signals investigated above and 18 rvar fixed-key
sorting failures also reproduced by ordinary vctrs ordering. Of the Package
refusals, 18 rejected raw typed missing values, 54 rejected nonmissing labels
for structured dimensions, and 18 were the rvar missingness observations.

The 36 identity-only rvar signals all reported `all.equal()` TRUE. A separate
payload control established identical draw matrices as well as `all.equal()`
TRUE. Environment identity was not reported as value corruption. The raw
sweep did not use a separate full sorting oracle, did not promise every
custom vector class, and did not turn excluded ordinary failures into tickets.

Duplicate searches for Date matrices, `table[1d]`, `vec_detect_missing`, and
Surv found the earlier factor-specific closed #1040 and no earlier matching
table-prototype report. #1069 and #1071 were published by this investigation.
The earlier dated expansion notes supplied coverage boundaries; their old
executions were not counted as new measurements.

## Replays

The three `reproduce-*.R` scripts print successful controls and captured
failures. Their `--assert` modes require the intended result and each exited 1
on this baseline, as recorded in `replay-status.json` and adjacent assertion
logs. Matrix scripts exited zero while recording the baseline defects; that
exit status did not claim that the defects were fixed. Jarl passed over this
evidence directory. Package checks and broader publication validation belonged
to the parent pull request.
