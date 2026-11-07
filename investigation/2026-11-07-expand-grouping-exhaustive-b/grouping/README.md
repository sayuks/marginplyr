# Grouping receiver dispatch and algebra hunt

Investigated: 2026-11-07
Tested package SHA: `0b5143133c023ccb4efb6fe27f1ef551e9f49186`

This lane used a disposable Git archive of the tested main revision. It read
CONTEXT, the Grouping constructor and inspection contracts, ADR 0008 including
its capture/continuation amendments, ADR 0013, ADR 0026, the November 6 deep
hunt, and the November 7 expansion/capture and pattern-rebinding evidence.
Those previous measurements informed exclusions and were not counted here.
No package implementation, package test or installed file was changed.

## G1: names dispatch on the selection input was bypassed

A data frame subclass had an ordinary `names()` method. Outside a selection
context it returned the literal input names. In a selection context it read
`tidyselect::peek_data()` and reversed those names when the first metadata
column was character. The two-column input had character `a` and integer `b`.
Ordinary `names(x)` returned `a, b`; ordinary dplyr grouping on `a` succeeded.
Direct typed tidyselect selected `b` from
`names(tidyselect::peek_data())[1L]`.

With fixed key `a`, the same expression in a Grouping set falsely raised:

```text
Columns cannot appear in both `.by` and `.grouping`:
ℹ `a`.
```

The literal `b` and an opaque identity wrapper around the exact selection
produced identical correct results. Inspection and expansion both failed;
the same separating controls reproduced through summarization and both Nest
verbs. Source serialization was unchanged.

[name-dispatch.R](name-dispatch.R) retained the one-row minimal witness. Its
ordinary-grouping, direct-selection and opaque-control assertions passed,
then its desired-contract equality intentionally exited one on the tested SHA.
[name-dispatch-controls.R](name-dispatch-controls.R) classified the false
refusal across all five consumers, comparing exact literal and typed-control
results. Its terminal exit was zero.

### Silent fixed-key consequence

[name-dispatch-fixed.R](name-dispatch-fixed.R) removed the need for a second
Grouping dimension. With `a = A, B` and `b = 1, 1`, putting the same selection in
`.by` silently fixed on `a` instead of `b`. Inspection reported the wrong fixed
key; a count summary returned two rows with one each instead of one row with
two; both Nest verbs returned two cells instead of one. The opaque wrapper
selected `b`. Its terminal exit was zero. These were consequences of G1 rather
than separate repair slices.

### Separating predictions and finite matrix

After the initial witness, the separating hypotheses were:

1. Names-only classification bypassed receiver dispatch. A faithful typed
   wrapper should invoke the method and restore direct tidyselect's answer;
   the discarded plain-list path should invoke it zero times. Confirmed.
2. The class violated ordinary column-name/grouping admission. Ordinary names
   or dplyr grouping should fail. Refuted by their successful controls.
3. The method or source changed between calls. Literal/opaque results or
   serialized source should drift. Refuted by exact repeated comparisons.

[name-dispatch-matrix.R](name-dispatch-matrix.R) crossed eight spellings with
`.by` and all five constructors, then inspection and expansion: 96 cases.
Spellings covered bare, qualified, parenthesized and aliased `names()`, both
indexing operators, a one-element range, and `all_of()`/`c()` wrappers. Every
case falsely refused overlap, had an exact literal/opaque-control match,
recorded zero typed method callbacks on the failing path and at least one on
the opaque path, and preserved the source bytes. The complete per-case
[name-dispatch-matrix.csv](name-dispatch-matrix.csv) and terminal log were
retained; the matrix exited zero.

The shared classifier recognized `names(peek_data())` as a names-only lookup.
Its names proxy was an unclassed list, while its local typed proxy was the
caller input, whose `names()` method could run ordinary caller dispatch.
Subscript and leaf-helper wrappers inherited that proof. This violated the
public tidy-select meaning of fixed keys and Grouping dimensions, the
names-alone invariant stated in #342, and ADR 0013's shared inspection
semantics. The evidence established this finite subclass/context-sensitive
method family; it made no claim that all names methods or local classes failed.

### Recommended ticket

One independent P2 slice: preserve receiver dispatch when resolving Grouping
selection-context names. It had no blocker or prerequisite refactoring.
Acceptance should cover direct typed agreement for fixed keys and all five
constructors, the silent fixed-key grain and false-overlap witnesses,
inspection and expansion, ordinary names-only early-error behavior, evaluation
budgets, and the existing metadata-only lazy-input boundary. No new selector
syntax or full-source read followed from the finding.

## Independent algebra oracle

[algebra.R](algebra.R), seed 71107, generated 5,000 valid bounded-depth ASTs
from singleton sets, unions, rollups, cubes and Cartesian products over five
columns. Ordinary multi-column selections and overlapping composite dimensions
were included. A separate declarative enumerator constructed each unoptimized
family, normalized membership in first-appearance dimension order and retained
the first membership occurrence for duplicate-drop expectations.

Fourteen generated families exceeded the 2,000-occurrence comparison bound
and were excluded. The other 4,986 shapes supplied 9,972 comparisons under
keep/drop. Every exact included-vector list matched, including occurrence order
and empty sets. The terminal exit was zero. This established membership and
included-column order for those finite shapes, not every inspection field,
arbitrary resource bounds or backend execution.

## Duplicate assessment

All-state snapshots supplied by the coordinator contained Issue bodies and
comments and PR bodies. Additional live `gh api` searches included Issues and
PRs together, without a state filter:

| Query within `repo:sayuks/marginplyr` | Results |
| --- | ---: |
| `"names" "peek_data"` | 6 |
| `"names" "dispatch"` | 58 |
| `"names.selector_frame"` | 0 |
| `"names.data.frame"` | 0 |
| `"colnames" "peek_data"` | 0 |

Complete bodies/comments were read for [#1277](https://github.com/sayuks/marginplyr/issues/1277),
[#995](https://github.com/sayuks/marginplyr/issues/995),
[#825](https://github.com/sayuks/marginplyr/issues/825),
[#1039](https://github.com/sayuks/marginplyr/issues/1039),
[#342](https://github.com/sayuks/marginplyr/issues/342), and
[#77](https://github.com/sayuks/marginplyr/issues/77), together with
[PR #1284](https://github.com/sayuks/marginplyr/pull/1284) and its review comments.
#1277/PR #1284 added names-only context lookups but did not report receiver
method dispatch. #995 concerned implicit-class generic operands and injected
classed selectors, while #825 concerned helper arguments requiring types and
#1039 caller namespace lookup. #77 established grouping-based input admission.
None reported G1's `names()` receiver-dispatch boundary.

## Replay, execution ownership and limits

From any package root, passing the tested disposable source directory:

```sh
Rscript investigation/2026-11-07-expand-grouping-exhaustive-b/grouping/algebra.R /path/to/source
Rscript investigation/2026-11-07-expand-grouping-exhaustive-b/grouping/name-dispatch.R /path/to/source
Rscript investigation/2026-11-07-expand-grouping-exhaustive-b/grouping/name-dispatch-controls.R /path/to/source
Rscript investigation/2026-11-07-expand-grouping-exhaustive-b/grouping/name-dispatch-fixed.R /path/to/source
Rscript investigation/2026-11-07-expand-grouping-exhaustive-b/grouping/name-dispatch-matrix.R /path/to/source /path/to/results.csv
```

The minimal witness intentionally failed its desired-contract assertion. Every
other final script reached terminal exit zero. Algebra execution 3773,
consumer controls 97085, matrix 3863, minimal-witness execution with terminal
chunk `77d303`, and fixed-key execution 84552 were collected through terminal
completion. No process or child work remained. Reproduction processes ran
serially and set `TESTTHAT_PARALLEL=false`; no package suite, snapshot suite,
build, rendering, backend connection or server was started.

An initial global `names.data.frame` exploratory method recursively asked for
selection metadata while formatting its absent-context diagnostic. Execution
20749 was cancelled through its retained handle and collected at terminal
exit one. That attempt was excluded from counts and claims. The final evidence
used only the subclass method with a reentrancy guard. Preliminary local
inspection without a fixed key returned the correct canonical answer and was
not called a failure; the discarded pass's early refusal and already-settled
fixed key were the observable defects.

The provisioned environment was arm64 macOS, R 4.6.1, dplyr 1.2.1,
dbplyr 2.6.0, tidyselect 1.2.1, rlang 1.3.0, vctrs 0.7.3,
tibble 3.3.1 and pkgload 1.5.3; full session information was retained in the
algebra and matrix logs. Other classes, platforms, versions, warning identity,
live SQL/backend metadata acquisition, and unbounded plans were not measured
by this lane. These development probes supplied no Review-ready result.
