# dtplyr full-join metadata dispatch investigation

Investigated: 2026-11-05
Tested source: `0403c9150394cbae21b59f34c75791d0873b8451` (the dedicated hunt worktree, main at hunt start).

## Finding

Typed inspection and both Nest verbs accepted an immutable `full_join()` graph
whose generated bare `merge()` resolved to a caller function. That function
delegated to the ordinary data.table method for real rows, but returned integer
`g` on zero rows. Ordinary collection preserved character `g = c("a", "b")`.
Typed `grouping_set(where(is.character))` instead compiled the empty set, and
both Nest verbs returned one keyless cell rather than two cells keyed by `g`.
The same failure removed the typed fixed key selected through `.by`.

ADR 0020, amendment #625, required faithful derived types or a Package condition
when schema could not safely be established. ADR 0013 required the inspected
Grouping plan to match the backend-independent grouping semantics. The nesting
reference promised one nested data frame per group in each grouping set.
No source mutation was observed; the defect was false metadata and partitions.

`full-join-merge.R` minimized the case to a two-row plain data.table, its
immutable self full join, one ordinary caller binding and one typed selector.
The name-only inspection selected `g` without invoking the binding. Removing
only the binding made all three typed consumers produce the expected plan or
partitions. The replay optionally required desired behavior with `--assert`;
that invocation returned terminal exit 1. The observational invocation returned
exit 0 after printing all three false verdicts and passing its controls.

## Ranked hypotheses and discriminating controls

1. The full-join metadata whitelist omitted validation of generated `merge()`
   dispatch. Prediction: keeping data, binding, selector and consumer constant
   while changing the join style away from full would remove the failure.
2. The proxy used a different lexical environment from ordinary execution.
   Prediction: only the metadata path would reach the caller binding. The
   native control reached the same binding with two rows in each argument,
   whereas preparation reached it with zero rows. This refuted an environment
   mismatch as the distinguishing cause.
3. `where(is.character)` itself selected the wrong native column. Prediction:
   removing the caller binding while keeping the same graph and selector would
   preserve the failure. All three unmasked controls passed, refuting this.

After the minimized replay, `merge-frontier-controls.R` varied only join
style, caller binding, typed selector position and public consumer. Of 72
observations, 66 passed the selected-key and group-count checks. Exactly the six masked full-join
cases failed: both selector positions across all three consumers. Full, left,
right, inner, semi and anti joins were represented. The matrix recorded
callback argument row counts, distinguishing the zero-row schema invocation
from the rowwise Nest verb's documented explicit collection.

Source inspection then located the separating branch: the metadata safety
predicate checked generated bracket dispatch for non-full joins and admitted
full joins through plain-root/type checks, after which isolated proxy execution
evaluated the generated bare merge. No package implementation was modified.

## Adjacent call controls

The initial call-mask pilot also tried `setnames`, `copy` and `funion` bindings
on rename, arithmetic and union graphs. Those caller functions were not
invoked, and typed inspection and nesting selected the expected dimension.
The merge pilot was corrected twice before the minimized replay: its original
second positional argument selected `.by`, and an unexported generic lookup
failed its native control. Those pilot outputs were exploratory and supplied
no finding or matrix count. The final reproduction explicitly named
`.grouping` and called the registered data.table merge method.

## Duplicate searches

All-state GitHub issue and PR searches used `dtplyr merge in:title,body`.
An additional all-state issue query used `"full_join" "metadata"` and returned
no match. Adjacent issue #1281 addressed lexical arithmetic on joined operands,
and PR #1296 validated that arithmetic in the effective execution environment.
Issue #1326 and PR #1333 addressed generated brackets and head dispatch. Their
complete bodies/comments were fetched. The full-join merge entry point was
not the reported witness or repaired branch in those records. JSON query
results and relevant issue/PR snapshots were retained beside the replay.

## Replay and limits

From the tested package root:

```sh
Rscript <evidence>/full-join-merge.R
Rscript <evidence>/full-join-merge.R --assert
Rscript <evidence>/merge-frontier-controls.R
```

The reproduction also accepted `MARGINPLYR_HUNT_SOURCE` for an immutable
archive of the tested source. The focused commands ran with testthat parallel
execution disabled and completed without retained child processes. No full
suite, package build, dependency installation or implementation change was
performed by this track. Environment: R 4.6.1, macOS arm64, dplyr 1.2.1,
dtplyr 1.3.3, data.table 1.18.6.1, tidyselect 1.2.1 and vctrs 0.7.3.
The matrix established this finite graph/callback boundary, not arbitrary
extension behavior or every backend. SQL and Arrow Nest support was not
inferred; their documented refusal remained outside this focused witness.
