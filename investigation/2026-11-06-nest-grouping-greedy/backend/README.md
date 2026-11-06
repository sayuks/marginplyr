# Backend metadata dispatch hunt

Investigated: 2026-11-06
Tested source: `952a0d9b18fd3ff4631f153432f6024ad62fcac2` (main at hunt start).

## A caller's bracket method changed the inspected Grouping grain

The [replay](bracket-method.R) built an immutable two-row dtplyr source with
character `g = c("a", "b")`, integer payload `v`, and an arithmetic mutation
of `v`. Its caller environment bound the S3 method name `[.data.table`, while
leaving the generic `[` and the global S3 method registry unchanged. The
method delegated to the ordinary registered data.table method for real rows
and changed `g` to integer only for zero-row results.

Native collection retained both character keys. Typed
`grouping_set(where(is.character))` inspection nevertheless returned the empty
grouping set, and both Nest verbs returned one keyless cell instead of two
cells identified by `g`. Selecting the same column as a typed fixed key
through `.by` produced the same six failures. The method saw two rows during
native collection and zero rows during metadata acquisition. Every caller
source snapshot and the registered S3 method remained identical.

ADR 0020's dtplyr metadata amendment #625 required a faithful derived schema
or the established safe metadata refusal.
ADR 0013 required the inspected Grouping plan to describe the input's grouping
semantics, and the nesting reference promised one cell per group in each set.
The observed disappearance of a real character key violated those contracts.
No source mutation or unrequested processing of real source rows was inferred
from this witness; the defect was false metadata and false partitioning.

The ordinary [output](bracket-method.txt) returned exit 0 after its native,
unmasked and source/registry controls passed. The desired-behavior
[`--assert` run](bracket-method-assert.txt) returned exit 1 at
`all(minimal$good)`, requiring retention of the real selected key. The
coordinator independently ran that assertion with `MARGINPLYR_HUNT_SOURCE`
pointing to a separate Git archive of the same SHA; execution session `92606`
also ended with exit 1 after the controls passed.

## Ranked hypotheses and controls

1. The admission predicate checked the generic binding but omitted the
   applicable S3 method. Prediction: leaving `[` ordinary and masking only
   `[.data.table` would retain the false zero-row schema; removing the method
   binding would remove the failure. The minimal replay observed both.
2. Metadata used a different environment from native execution. Prediction:
   native collection would not reach the method. Native collection did reach
   it with two rows, refuting this as the distinguishing cause.
3. The selector or source character values were invalid independently of
   method dispatch. Prediction: the unmasked arithmetic graph or a masked root
   would lose `g` too. Both retained it.

Source inspection after the replay located the distinction in
`dtplyr_metadata_safe_step()`: generated bracket operations checked
`dtplyr_function_binding("[", ..., base::\`[\`)`, whereas full joins used
`dtplyr_merge_binding()` to check both generic and applicable S3 method.
`isolated_dtplyr_proxy()` then evaluated the derived zero-row graph in the
input's effective environment. The generic check alone did not establish
which S3 method would execute.

The [54-case matrix](bracket-method-matrix.csv) crossed masked and unmasked
callers, a root, arithmetic mutation and row filter, typed dimension/fixed-key
and name-only selections, and inspection/both Nest verbs. Exactly the six
masked arithmetic typed-selection cases failed; the other 48 matched the
expected key and two groups. Name-only inspection invoked the method zero
times. Root and filter metadata controls avoided the generated arithmetic
bracket path. Nest calls still performed their documented explicit execution
when collected or made row-wise. All snapshots and registry checks passed.
[Full matrix output](bracket-method-matrix.txt).

An exploratory join matrix draft was excluded: its simple forwarding wrapper
lost a join operand's native NSE lookup, so native collection itself failed.
The completed 54-case matrix contained no join case and established no join
defect. Removing the arithmetic operation by using an identity mutation also
removed the false metadata result; that operation was load-bearing.

## Existing reports and source-sharing control

The complete issue bodies and comments for [#1326] and [#1341], and the PR body
and comments for [#1333] and [#1352], were retrieved. #1326's witness masked the
generic bracket function, and #1333 admitted ordinary generic lookup while
refusing a custom generic. This witness left that repaired generic lookup
ordinary and instead changed its S3 method. #1341/#1352 covered full joins'
separate merge entry point and, including their review follow-up, explicitly
checked method lookup without forcing active or lazy registration bindings.
That repair did not add the equivalent method validation to generated
brackets. The new witness therefore identified an uncovered dispatch boundary
after both earlier repairs.

All-state Issue and PR searches used `dtplyr S3 method metadata` and
`dtplyr bracket method`; adjacent JSON files retained the returned titles and
links. The former found selector-dispatch reports #995/#1344/#1350 and PRs
#1013/#1352; the latter found #1341 and no PR. These searches were supplemented
by the directly fetched bracket and merge records above. The coordinator
re-fetched main at the same tested SHA and published [#1359].

The prior [source-reuse replay] also ran on the tested SHA with `--assert` and
returned exit 0. Both Nest verbs, on uncached and caller-cached graphs, ran an
alternating predicate once per collection and retained source row 1 in both
detail and total cells. Deferred construction and `show_query()` ran it zero
times. This refuted a regression of the repaired branch-sharing defect in the
tested shape. [Control output](source-reuse-control.txt).

## Replay and limits

Run from the tested package root, optionally setting `MARGINPLYR_HUNT_SOURCE`
to an immutable source archive:

```sh
evidence=investigation/2026-11-06-nest-grouping-greedy/backend
Rscript "$evidence/bracket-method.R"
Rscript "$evidence/bracket-method.R" --assert
MARGINPLYR_MATRIX_OUTPUT=/tmp/bracket-method-matrix.csv Rscript "$evidence/bracket-method.R" --matrix
Rscript investigation/2026-11-04-nesting-grouping-boundaries/backend/dtplyr-source-reuse.R --assert
```

Environment: R 4.6.1, macOS arm64, dplyr 1.2.1, dtplyr 1.3.3, data.table
1.18.6.1, tidyselect 1.2.1 and vctrs 0.7.3. Testthat parallelism was explicitly
disabled. Every execution ended terminally with no retained child process.
No package source, permanent test, dependency or global method registration
was modified, and no package build or full suite was run by this track.

The finite controls did not establish behavior for arbitrary methods,
registered-method replacement, every derived graph, hosted backends, other
library versions or resource exhaustion. SQL and Arrow nesting remained
documented refusals and supplied no nesting-support claim here.

[#1326]: https://github.com/sayuks/marginplyr/issues/1326
[#1341]: https://github.com/sayuks/marginplyr/issues/1341
[#1333]: https://github.com/sayuks/marginplyr/pull/1333
[#1352]: https://github.com/sayuks/marginplyr/pull/1352
[#1359]: https://github.com/sayuks/marginplyr/issues/1359
[source-reuse replay]: ../../2026-11-04-nesting-grouping-boundaries/backend/dtplyr-source-reuse.R
