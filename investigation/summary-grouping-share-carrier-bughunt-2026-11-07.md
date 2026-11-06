# Carrier and dispatch bug hunt for Margin summaries, Grouping specifications and shares

Investigated: 2026-11-07
Repository snapshot: `e1dabaabb392a3791177e5c48e983f780c7c93ec`

The investigation began from freshly fetched main in the new worktree
`/private/tmp/marginplyr-summary-grouping-share-hunt-20261107`, on branch
`investigation/summary-grouping-share-hunt-20261107`. Four investigators divided
ordinary summary evaluation, Grouping selection, contextual-share dependency
planning and live SQL result comparisons. The coordinator fetched main again
before publication; it remained at the tested SHA. No package implementation
repair or package regression test was added.

The [November 6 deep hunt](summary-grouping-share-deep-bughunt-2026-11-06.md)
and [boundary hunt](summary-grouping-share-exhaustive-bughunt-2026-11-06.md),
their lane evidence and earlier November 5 summaries supplied repaired controls
and unexplored adjacent boundaries. Their finite results were not counted as
executions of this investigation. The deliberate search did not establish
exhaustiveness over arbitrary R programs.

## Confirmed findings and repair slices

The user delegated ticket granularity and publication without a confirmation
checkpoint. The `to-tickets` workflow therefore used each independently
verifiable repair boundary as a slice. All seven tickets carried `bug` and
`ready-for-agent`, explicit acceptance criteria and no blockers. No necessary
prefactoring prerequisite was established. This research submission did not
repair or close any ticket.

| Boundary | Established effect | Ticket |
| --- | --- | --- |
| Plain list transformations | Concatenation, append and a literal subset discarded a local closure's invocation-time dependency. | [#1413](https://github.com/sayuks/marginplyr/issues/1413) |
| Structural list writes | NULL deletion retained an obsolete element map; single-bracket replacement lost the replacement closure. | [#1414](https://github.com/sayuks/marginplyr/issues/1414) |
| Partial list-name lookup | A uniquely partial dollar or explicit partial double-bracket lookup missed a known closure's dependency. | [#1415](https://github.com/sayuks/marginplyr/issues/1415) |
| Environment-valued expressions | Blocks, selected branches and identity lost a known summary-mask environment's identity. | [#1416](https://github.com/sayuks/marginplyr/issues/1416) |
| Literal delayed closure bindings | delayedAssign installed a callable whose invocation-time dependency was not retained. | [#1417](https://github.com/sayuks/marginplyr/issues/1417) |
| Implicit pattern-method rebinding | Changing a method during helper-operand evaluation invalidated a name-only proof and falsely rejected a valid typed Grouping selection. | [#1420](https://github.com/sayuks/marginplyr/issues/1420) |
| Authored namespace function shapes | Ordinary across name prediction invented outputs instead of respecting the caller's namespace lookup, producing false collisions. | [#1421](https://github.com/sayuks/marginplyr/issues/1421) |

The [share lane](2026-11-07-summary-grouping-share-hunt/share/findings.md),
[Grouping lane](2026-11-07-summary-grouping-share-hunt/grouping/findings.md)
and [ordinary-summary lane](2026-11-07-summary-grouping-share-hunt/summary/findings.md)
retain minimized public witnesses, contract sources, separating hypotheses,
repaired and legal controls, supported causes and duplicate comparisons.
Each desired-contract mode requires the intended result or existing refusal
and intentionally returns one on the investigated source. Observation modes
assert the dated failure; control modes use the intended contract and pass.

## Completed finite comparisons

| Lane | Finite scope | Observation |
| --- | --- | --- |
| Contextual-share dependencies | Both helpers, both prohibited dependency directions, three eager input classes and fourteen witness forms | 168 missing refusals, 84 repaired refusals and 168 legal reads across 420 comparisons; a separate 252-control execution passed. |
| Grouping pattern dispatch | Three pattern modifiers, four input classes, six selection positions and inspection/summarization | 144 false refusals; 144 direct typed references and 432 public controls agreed. |
| Ordinary across shape prediction | Both namespace operators, two expression entrances, one/two functions, three identifier/key roles and three eager input classes | 72 false collisions with 72 exactly matching literal-list controls; four related share requests raised malformed-name errors. |
| Ordinary cardinality/value controls | Six frame/scalar/empty/multiple-value pairs across three eager classes | Six exact accepted results and twelve paired errors; no condition-equality claim. |
| Live SQL numeric boundaries | SQLite and actual native/portable DuckDB paths, fixed/missing partitions, all sort requests, volatile and extreme/nonfinite numeric summaries | All 204 collections matched shares recomputed from displayed denominator rows. |
| Live SQL names | Fourteen spellings, five identifier/key/output roles, both backends and sorted/unsorted requests | 260 successful results retained expected public names and ratios; twenty positive dot-dot-index cases hit upstream name restrictions. |

The [SQL lane](2026-11-07-summary-grouping-share-hunt/sql/findings.md)
states its physical-path assertions, upstream controls and limits. Counts
describe separate, sometimes repeated finite comparisons rather than one
independent sample. Serializations in the three finding lanes were unchanged.
The numeric oracle did not assert Margin order; the name oracle checked public
column order, not row order.

## Independent replay and publication

The coordinator replayed retained finding scripts from a disposable Git archive
of the named main SHA, in separate sequential R processes. The
[replay table](2026-11-07-summary-grouping-share-hunt/coordinator/replay-results.csv)
records script digests, arguments and all 17 expected/observed terminal statuses.
Seven desired-contract modes intentionally returned one; observation, control
and SQL modes returned zero. The
[execution record](2026-11-07-summary-grouping-share-hunt/coordinator/execution-record.md)
names the retained handles, environment, setup exclusions and source identity.
These research executions were not Review-ready or package-suite results.

Duplicate checking began with all-state exports of 711 issue bodies and the
paginated PR archive. Lane notes name relevant full-body/comment reads and
live queries. At publication, newly opened #1418 and #1419 were also read in
full: they concerned registered dtplyr bracket/converter admission, rather
than the implicit pattern-method mutation or ordinary across shape boundary
reported here. The seven new tickets were fetched after creation to verify
their titles, bodies, labels and independent blocking status. A later narrow
clarification to #1421 retained static share-source provenance restrictions
while excluding malformed planned schema names as an acceptable refusal.
Tracker exports stayed in task-local scratch storage.

No guarantee was established for arbitrary reflection or effects, every custom
class or carrier, unbounded Grouping plans, all SQL dialects/collations,
hosted services, concurrency, interruption, other versions or other platforms.
The lane notes retain unconfirmed and refuted probes separately from findings.

The cumulative submission added dated prose, standalone research harnesses
and captured evidence. Its consumers were research readers and explicit replay
commands; it changed no package code/test, dependency, installed file, generated
documentation, generation source, build rule or verification tool. The unchanged
source-package exclusion omitted investigation content. These content and
consumer effects justified Review-ready source scope: package build/check,
complete suite and snapshots omitted. No NEWS entry was needed for this
research-only submission. The submitted SHA, fresh-main compatibility assessment,
mandatory baseline outcome and review record belonged on the PR. CI monitoring
was outside the requested scope.
