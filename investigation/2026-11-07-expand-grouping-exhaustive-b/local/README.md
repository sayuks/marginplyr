# Recursive local keys and extension-vector expansion hunt

Investigated: 2026-11-07
Baseline: `0b5143133c023ccb4efb6fe27f1ef551e9f49186` (fetched main)
Scope: standalone investigation evidence; no package implementation or tests

The November 6 structured/list-array/mixed-key investigations and November 7
named-label investigation supplied earlier coverage and exclusions. Their
measurements were not counted as executions here. ADR 0012 supplied factor
identity and typed-missing labels, ADR 0016 supplied ordinary branch-composition
and packed-key boundaries, and ADR 0018 supplied physical Margin order.

## Questions and finite comparisons

The first hypothesis was that combinations of recursively packed keys, arrays
and list values might expose a row-copy or ordering defect not reached by the
earlier isolated shape matrices. The second was that real extension-vector
prototypes might be falsely refused or lose values when omitted. The third was
that a classed matrix failure might originate in package-specific sorting
rather than in ordinary dplyr ordering. Each was tested against ordinary
mutation, union and ordering controls.

[recursive-keys.R](recursive-keys.R) generated 80 deterministic schemas for each
of zero, one and seven source rows, using seed 20261107. Each source had three
independently shaped dimensions, a separately generated fixed key and a source
row identifier. Packed keys had up to three nested frames and zero through
three fields; arrays had up to four ranks and included zero trailing extents
and dimension labels. Leaves included nonfinite numbers, NA-level factors,
ordered factors, temporal vectors, heterogeneous lists and integer list-of
vectors. The POSIXlt factory used its ordinary vctrs row-sliced representation;
this did not measure an untouched AsIs POSIXlt object.

The script independently enumerated rollup, cube, reversed/repeated union and
composite-product occurrences. Ordinary branches replaced omitted dimensions
with row-sized vctrs prototypes and combined with ordinary `union_all()`.
Requested first/last order came from an independent key table containing fixed
missingness/value terms, dimension Grouping bits and missingness/value terms,
and the occurrence identifier. Ordinary `arrange()` ordered that key table.
Exact full-frame identity checked public names/order, values, types, structured
shape and physical row order. Serialized caller input was checked each time.

The final [CSV](recursive-keys.csv) and [log](recursive-keys.log) recorded
**2,880 exact matches**, with unchanged source bytes and no excluded comparison.
The unsorted comparisons also matched observed branch order; no additional
unsorted-order promise was inferred.

[extension-keys.R](extension-keys.R) made **2,916 classifications** across
integer64 values beyond exact double precision, haven labelled/SPSS/tagged-NA
vectors, units, hms, difftime, three Surv shapes and two Matrix S4 classes.
It crossed zero/one/seven rows, zero/one/two packed wrappers,
dimension/fixed-key/payload roles, NULL/NA/text labels and all three Margin
orders. The [CSV](extension-keys.csv) and [log](extension-keys.log) recorded:

- 1,953 exact ordinary-composition matches;
- 207 required non-missing packed/array label refusals;
- 270 ordinary Surv sorting-recursion errors; and
- 486 fixture exclusions, because ordinary tibble construction refused the
  dense/sparse S4 Matrix columns before a Margin operation could run.

All constructed source objects stayed byte-identical. The
[result validator](validate-results.R) checked all counts, source immutability,
and the specific packed/array refusal fragments for every required refusal.
No generic Package condition was accepted as evidence of the intended refusal.

[survival-controls.R](survival-controls.R) separately tested eighteen nonempty
Surv fixtures across right/interval/multistate forms, one/seven rows and three
packing depths. Ordinary value-only `arrange(g)` and sorted expansion both
raised their recursion errors; unsorted expansion returned exactly twice the
source row count. The [CSV](survival-controls.csv) and
[log](survival-controls.log) retained those separating controls. No
package-specific defect or implementation ticket followed from this lane.

## Oracle corrections, executions and replay

The preliminary recursive run used depth-first rather than size-ordered cube
enumeration, misspelled the Cartesian-product constructor as `grouping()`, and
used `vec_order()` where heterogeneous lists required ordinary dplyr ordering.
Those three harness defects were corrected before the complete rerun. Its
preliminary observations were excluded from the final counts; transient binary
diagnostics were removed. No package source was changed to obtain a match.

From the worktree root, with a separate `git archive` export of the baseline
as `package-root`:

```sh
Rscript investigation/2026-11-07-expand-grouping-exhaustive-b/local/recursive-keys.R <package-root> <output.csv>
Rscript investigation/2026-11-07-expand-grouping-exhaustive-b/local/extension-keys.R <package-root> <output.csv>
Rscript investigation/2026-11-07-expand-grouping-exhaustive-b/local/survival-controls.R <package-root> <output.csv>
Rscript investigation/2026-11-07-expand-grouping-exhaustive-b/local/validate-results.R investigation/2026-11-07-expand-grouping-exhaustive-b/local
```

Original execution handles were retained to terminal completion: `78676`
(preliminary recursive run), `43318` (final recursive matrix), `81334`
(extension matrix) and `2196` (Surv controls), all exit zero. The final standalone
result validator also exited zero. Executions were serialized and explicitly
disabled testthat parallel execution. No full suite, build or render supplied
this evidence.

The logged environment was arm64 macOS Tahoe 26.6.2, R 4.6.1, dplyr 1.2.1,
vctrs 0.7.3, tibble 3.3.1, rlang 1.3.0, haven 2.5.5, bit64 4.8.6,
units 1.0-1, hms 1.1.4, survival 3.8-11 and Matrix 1.7-6.
These finite comparisons did not establish arbitrary extension classes,
all shape combinations, unlimited size/performance, warning identity,
other versions/platforms or lazy-backend behavior.
