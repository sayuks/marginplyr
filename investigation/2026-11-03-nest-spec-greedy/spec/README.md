# Grouping specification selection frontier

Investigated: 2026-11-03
Baseline: `4b34fdecb2d8a41034b3d9b5d5af8e51592555e1` (`main`)

This track established no new independently actionable defect in the completed
finite selection differential. It changed no package implementation or permanent
regression test. The source was a disposable `git archive` of the baseline;
testthat parallel execution was explicitly disabled and all executions reached
terminal completion before the next run.

## Readings and scope

The readings included `CLAUDE.md` and its `AGENTS.md` closure, `CONTEXT.md`, ADRs
0007, 0008, 0013 and 0026, the Grouping compiler's names-only classification,
preflight, retained-value and fixed-key readers, and the selection-budget and
leading-selection tests. The October 30
[specification lifecycle record](../../2026-10-30-nest-spec-greedy/spec/README.md),
October 31 [closure and identity record](../../2026-10-31-nest-spec-greedy/spec/README.md),
and November 2 [selection-effect record](../../2026-11-02-nest-grouping-hunt/spec/README.md)
and [injection/algebra record](../../2026-11-02-expand-grouping-greedy/spec/README.md)
informed the remaining boundaries. Their earlier counts were not counted as new
observations.

The candidate risks were that special dots names or forwarded defaults lost
their lexical environment, that names-only classification evaluated a typed
selector against dummy values, or that a multi-column selection stopped behaving
as one composite dimension when wrapped in a nested `grouping_set()`. The
separating observation was exact agreement with direct typed tidyselect after
capturing through an ordinary public `enquo()` boundary, followed by an
independent small family expansion.

## Completed differential

`selection-frontier.R` ran 57 written selectors against three input shapes and
eight positions: `.by`, each of the five constructors, and a nested
`grouping_set()` composite under `rollup()` or `cube()`. The inputs were a
populated numeric/character/factor frame, its zero-row counterpart, and a
populated list/Date/logical frame.

The selectors included empty and populated forwarded dots, `..1`, `..2`,
`...elt()`, closure defaults forwarding to special dots or another default,
empty variadic closures, typed proxy-dependent helpers, `peek_vars()`
reordering, parenthesized and qualified helper heads, attributed matrix/array
selection values, identity and changed renaming, typed predicates, empty or
invalid logical/missing/list/language selectors, `.data` subscripts, and empty
arguments inside a selection. The oracle did not call marginplyr's compiler or
family-expansion helpers.

All 1,368 final comparisons were accounted for:

| Observation | Count |
| --- | ---: |
| Exact included-family or fixed-key match | 1,000 |
| Matching External-condition class | 264 |
| Required empty rollup/cube or composite refusal | 80 |
| Required changed-renaming refusal | 24 |

No unexplained mismatch remained. The final execution exited zero and retained
its full `sessionInfo()` in `selection-frontier.log`. The empty-dots forms that
were reported on November 2 succeeded or received the documented empty-unit
refusal on this baseline; that observation was not a new defect or a replacement
for the earlier dated finding.

### Oracle correction

The first 1,026-position run passed a constructed quosure directly to
tidyselect while the public package call captured it again. For
`.data[[..1]]`, ordinary tidy-evaluation capture resolved the subscript to its
constant before selection; the uncaptured oracle instead rejected a
nonconstant subscript. All 18 apparent differences belonged to that fixture
asymmetry. `selection-frontier-initial-oracle.log` retained that failed oracle
run. The corrected oracle captured through `enquo()` too, and the final run
also added both composite positions. The initial differences were not package
findings and were excluded from the final totals.

## Replay and limits

Run from the investigated checkout root:

```sh
Rscript investigation/2026-11-03-nest-spec-greedy/spec/selection-frontier.R
```

The script loaded that checkout through `pkgload::load_all()`, wrote its CSV to
a fresh temporary output directory, and asserted that every comparison was
accounted for. `MARGINPLYR_HUNT_SOURCE` and `MARGINPLYR_HUNT_OUTPUT` allowed an
explicit disposable source and output directory; the retained execution used
the archived baseline. R 4.6.1 ran on aarch64 macOS Tahoe 26.6.2, with dplyr
1.2.1, tidyselect 1.2.1, rlang 1.3.0, tibble 3.3.1 and pkgload 1.5.3.

These observations did not establish equality of complete External-condition
objects/messages/backtraces, warning-effect counts, all possible closure bodies,
arbitrary forged specification fields, unbounded nesting or family size, remote
execution, physical nesting memberships, every input class, or other R and
dependency versions. No full package suite, package build, installation or site
render was run by this track. No candidate required a new duplicate search or
Issue publication.
