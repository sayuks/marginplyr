# Local list-array validation and additional vector-family investigation

Investigated: 2026-11-06
Baseline: `952a0d9b18fd3ff4631f153432f6024ad62fcac2` (`origin/main`)
Scope: dated investigation evidence; no package implementation or tests changed

## Confirmed finding: representable list-array missing rows were refused

A one-row tibble held a one-cell list matrix with integer 7 and payload 19.
Ordinary `vec_init()` produced a one-row matrix whose sole cell was NULL.
Ordinary mutation and `union_all()` returned the original and omitted rows,
retaining the two-by-one list-matrix shape, exact cells and both payload copies.
`expand_with_margins()` with `rollup(g)` and a NULL Margin label instead raised
`marginplyr_error`: a typed-missing label could not be represented for `g`.

The public *Display labels and grouping identity* section admitted local
matrix/array dimensions with NULL or NA labels, including empty inputs, and
promised row-shaped typed missing components. ADR 0016 delegated combination
to dplyr/vctrs and did not require arbitrary attribute restoration. The witness
required only ordinary list arrays supported by those operations.

The minimizing feedback loop was [list-array.R](list-array.R). Its observation
mode required the successful ordinary control and printed the failure; its
`--assert` mode intentionally exited one. A single row and cell sufficed, and
the payload only made row-copy preservation explicit. The root investigator
also supplied an independent rank-three witness with different values, actual
NULL source cells and a two-row source.

The primary and cause probes distinguished three explanations:

1. A false representability test would reject a valid NULL-cell prototype
   before branch execution, while ordinary construction/combination succeeded.
2. A branch-union limitation would also fail the ordinary union control.
3. A general list-array admission problem would reject included-only or
   payload controls too.

The observations supported the first explanation and refuted the latter two.
The structured prototype validator used scalar `is.na()` for every array.
For a list matrix holding NULL, that predicate was FALSE for the cell, while
`vec_detect_missing()` reported the row missing. Component checks had avoided
class-proxy recursion and auxiliary dimension labels under closed #1069; the
list-cell missingness semantics were a distinct residual boundary. No change
to implementation was made during this investigation.

## Finite list-array matrix and related controls

[list-array.R](list-array.R) attempted 1,944 fixtures, crossing zero/one/three
rows; zero/one/two cells along the second dimension; matrix/rank-three shape;
plain/list-of storage; direct/packed/nested-packed keys; dimension/fixed/payload
roles; NULL/NA labels; and all three Margin orders.

- 972 list-of array fixtures were excluded before expansion because ordinary
  construction raised `dim<-.vctrs_list_of() not supported`.
- 756 admitted plain-list-array fixtures matched the independent ordinary
  branch composition in exact values, attributes, shape, occurrence identifiers
  and requested physical order.
- 216 admitted positive-width dimension fixtures raised only the false
  representability refusal. These covered both ranks, all source row counts,
  both typed-missing label spellings, all orders and all three nested shapes.

There were no other differences or ordinary execution errors among the
admitted fixtures. Zero-width dimensions and fixed-key/payload controls
succeeded. Every admitted fixture also required serialized caller input to
remain unchanged after ordinary and package execution. The oracle enumerated
its own grouping sets, used row-sized typed missing rows, and constructed
Margin-order keys independently from occurrence membership.

[list-array-controls.R](list-array-controls.R) checked one-, two- and
three-dimensional list arrays with actual NULL source cells. Ordinary union,
included-only and payload controls succeeded. All four Margin verbs reached
the same false validation refusal on rollup. The control also retained required
refusals for raw-array missing labels and non-missing structured-key labels.
Those shared-gate observations did not independently establish a complete
summary/nesting value oracle.

## Additional temporal and scalar families

[temporal-types.R](temporal-types.R) explored 15 families over zero/one/three
rows, direct/packed shape, dimension/fixed/payload roles, NULL/NA/text labels
and all three Margin orders. Families were ts, zoo, yearmon, yearqtr, Period,
Duration, Interval, AsIs complex/raw, expression, call/pairlist lists, blob,
s2 geography and wk WKT. The full final run made 2,430 comparisons. The final
log and classified CSV recorded 1,983 exact matches, 168 required admission
refusals, 135 fixture exclusions and 144 ordinary-control failures, with no
remaining package-only error or difference. Fixture exclusions were 54 empty ts
constructors and 81 expression-vector source assignments. Ordinary failures
were 84 zoo combination cases (`indexes overlap`) and 60 unsupported s2
ordering cases. The required refusals were non-missing packed labels and
unrepresentable raw typed-missing components.

The ordinary oracle copied every source row, replaced omitted dimensions with
row-sized `vec_init()` values or character display labels, combined branches,
and constructed the documented Margin-order key. It compared actual physical
order when requested and aligned occurrence/source-row identifiers only for
unsorted results. Public names, values, shape and column attributes were
compared. It did not establish warning identity/deduplication or arbitrary
caller-class coverage.

An initial oracle compared different identifier column positions, producing
spurious differences. A second version omitted ordinary sorting and therefore
misclassified unsupported s2 ordering as package-only. Those oracle errors
were corrected, the complete matrix rerun, and their partial counts were not
used as successful evidence. Packed text-label refusals and raw typed-missing
refusals were classified under the documented admission contract.

## Prior reports, replay and limitations

All-state GitHub searches used `"list" "matrix"`, `"list" "array"`,
`"prototype" "missing"`, `"list-backed"`, `"list matrix"`, `"list array"`,
`"list-matrix"` and `"list-array"`; relevant PR searches used list/matrix,
list/array and issue 1069. No exact report of the NULL-cell local list-array
boundary was found. Full #1069, #896 and #833 bodies/comments and PR #1079's
body/comments were read. #1069/#1079 concerned classed/atomic array validation,
#896 plain-list ordering and #833 dtplyr expression construction. None supplied
this missing local list-array acceptance case. Root-owned current-main and
publication checks were recorded separately.

Run from the investigated checkout root:

```sh
Rscript investigation/2026-11-06-expand-grouping-hunt/local/list-array.R
Rscript investigation/2026-11-06-expand-grouping-hunt/local/list-array.R --assert
Rscript investigation/2026-11-06-expand-grouping-hunt/local/list-array-controls.R
Rscript investigation/2026-11-06-expand-grouping-hunt/local/temporal-types.R
```

Set `MARGINPLYR_HUNT_OUTPUT` to retain per-case CSVs; otherwise scripts used a
temporary directory. Observation/control commands exited zero; the minimal
expected-behavior assertion intentionally exited one on the baseline.
Executions retained their original handles until terminal completion, and
`TESTTHAT_PARALLEL=false` was explicit. No full suite/build/render was run.

The environment was arm64 macOS Tahoe 26.6.2, R 4.6.1, dplyr 1.2.1,
vctrs 0.7.3, tibble 3.3.1, rlang 1.3.0, lubridate 1.9.5, zoo 1.9-0,
blob 1.3.0, s2 1.1.11 and wk 0.9.5. Finite observations did not establish
unrestricted user classes, other package/OS versions, live backends, arbitrary
Grouping-plan sizes or performance bounds.
