# Local nesting integrity and Margin order probes

Investigated: 2026-11-06
Baseline: `9ffb24614a71512170298a3827bc11c80354e742` (`origin/main`)

## Outcome

No new package defect was established by this lane. The source was loaded
from a disposable `git archive` of the baseline, with `TESTTHAT_PARALLEL=false`.
No package implementation or package tests were edited.

## Completed evidence

`matrix.R` constructed 30 typed fixtures and exercised each as a variable
Grouping dimension, fixed key, and payload, with both keep policies, all three
sort requests, and both nesting verbs: 1,080 public calls. Membership was
derived independently from the original input using `vctrs::vec_group_loc()`;
cell columns and values were compared with original source slices. Source
serialization was compared before and after every call, including refusals.

Of these calls, 1,008 passed the semantic comparison. For their 672 sorted
variants, an independent key was assembled from fixed-key row missingness,
variable-dimension Grouping bits, row missingness and displayed values, with
the set identifier as a tiebreaker. `vctrs::vec_order()` found the returned
order already correct for every such call. This checked ADR 0018 directly,
including packed, matrix, list and custom-equality keys.

The fixtures covered atomic missingness, NA/NaN and signed zero, raw/complex
values, ordinary and ordered factors with NA levels and missing codes, Date,
POSIXct, AsIs POSIXlt, difftime, hms, integer64, package versions, list and
list-of-data-frame values, matrices and arrays, deeply packed frames, zero-column
packed frames, packed matrices/lists, case-insensitive equality, AsIs factors
and lists, and factor matrices/arrays.

`grouped-empty.R` completed 72 grouped-input cases: `.drop=TRUE` inputs
matched their explicit fixed-key controls; `.drop=FALSE` inputs produced the
documented Package condition and did not mutate their source. Two rowwise
inputs produced the documented refusal. Twelve zero-column input cases
confirmed cell row counts and the distinct keyless-empty shapes of the verbs.

## Excluded signals and discriminating controls

- Twelve raw-dimension calls refused an unrepresentable typed-missing label.
- Twelve AsIs-factor dimension calls failed during branch combination. Ordinary
  `dplyr::union_all()` over that original input raised the same cast failure.
  Vctrs slicing itself changed the class, so this was excluded under ADR 0016.
- Forty-eight factor matrix/array fixed/dimension calls raised grouping
  recursion errors. Direct `vctrs::vec_group_loc()` reproduced that failure
  without marginplyr. Factor matrices/arrays used only as payloads survived.
- The first exact oracle flagged six kept AsIs-POSIXlt dimension comparisons
  because ordinary grouped slicing repeated the `AsIs` class. Every field and
  membership matched; the retained `native-controls.R` proved exact equality
  with cells made by native sliced branch union followed by nesting. The
  corrected oracle ignored only duplicate `AsIs` class entries for this fixture.
- An early invocation passed quosures through `rlang::exec()` as literal
  selection values and therefore tested the harness incorrectly. The retained
  script uses direct public calls. Those early refusals were not counted.

`native-controls.R` also calibrated the independent comparisons with
deliberately wrong membership and reversed non-tied ordering. No source code
was modified to produce those negative controls.

## Prior evidence and tracker search

The lane read the October 29 nesting bug-hunt and deep-hunt notes and their
relevant local coverage, along with nesting source contracts and ADRs 0012,
0016 and 0018. Historical counts were not treated as runs on this baseline.

All-state GitHub searches used `AsIs` and `factor array` for issues, and
`AsIs` for PRs. They returned closed #1236/#1275 (AsIs POSIXlt combinations)
and #1040/#1069 (typed-missing array validation), among other related reports.
The raw factor-array equality recursion observed here was distinct from the
fixed missing-prototype validation; it also occurred in native grouping.
No issue was published by this lane because no new defect remained.

## Replay and limits

Run from the repository root, supplying a disposable baseline source copy
and an external output directory:

```sh
Rscript investigation/2026-11-06-nest-grouping-hunt/local/matrix.R <source-copy> <output-directory>
Rscript investigation/2026-11-06-nest-grouping-hunt/local/grouped-empty.R <source-copy>
Rscript investigation/2026-11-06-nest-grouping-hunt/local/native-controls.R <source-copy>
```

The adjacent logs and CSV recorded the dated invocations. No existing package
suite, full package check, alternate OS or R/dependency version was run by this
lane. These finite fixtures did not cover arbitrary user classes or all valid
Grouping specifications; the other campaign lanes covered constructors and
lazy backends. The default unsorted outer order was not asserted.

The environment was R 4.6.1, dplyr 1.2.1, vctrs 0.7.3, tibble 3.3.1,
dtplyr 1.3.3, bit64 4.8.6, and hms 1.1.4 on the maintainer Mac.
