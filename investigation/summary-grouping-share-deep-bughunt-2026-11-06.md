# Deep bug hunt for Margin summaries, Grouping specifications and shares

Investigated: 2026-11-06
Revised: 2026-11-06 — investigation/summary-grouping-share-exhaustive-bughunt-2026-11-06.md
Repository snapshot: `547d77095c2923573216d002f50bef21778247f9`

The investigation started from fetched main in the new worktree
`/private/tmp/marginplyr-summary-grouping-share-deep-20261106`, on branch
`investigation/summary-grouping-share-deep-20261106`. Four investigators used
separate disposable archives for ordinary Margin summaries, Grouping family
algebra, contextual-share dependencies and live DuckDB results. The coordinator
fetched main again before publication; it still resolved to the tested SHA.

The preceding [November 6 hunt](summary-grouping-share-bughunt-2026-11-06.md),
[November 5 hunt](summary-grouping-share-bughunt-2026-11-05.md) and targeted
November 3/4 evidence supplied repaired controls and remaining boundaries.
This note supplemented those dated records; it did not amend or supersede
them. No package implementation repair was attempted.

## Confirmed dependency boundaries

The [share lane](2026-11-06-summary-grouping-share-deep/share/findings.md)
records the minimal expressions, ranked hypotheses, separating controls,
contract sources, environment and duplicate comparisons. Each boundary affected
both Parent and Total shares and both forbidden dependency directions: a later
ordinary summary could read an earlier share, and a requested share source
could depend on an earlier ordinary summary alias.

| Boundary | Observed effect on the tested snapshot | Ticket disposition |
| --- | --- | --- |
| A computed member target on an explicit mask environment | Concatenated, lexical and locally assigned indices returned input payloads beside the displayed share; literal-member and computed-name lookup controls refused. | [#1385](https://github.com/sayuks/marginplyr/issues/1385) |
| Unproven inheritance or stop-frame options | A scalar rlang lookup with a literal binding name bypassed the refusal when its search options were expressions; equivalent proven literal options refused. | [#1386](https://github.com/sayuks/marginplyr/issues/1386) |
| Bulk environment reads | rlang multi-binding lookup and base environment-to-list conversion hid dependencies; scalar and base multi-binding lookup controls refused. | [#1388](https://github.com/sayuks/marginplyr/issues/1388) |
| Known-call closure transfers | identity/invisible return and literal reflective assignment lost a closure's invocation-time dependency after a local binding removal; repaired bare-copy, literal-list and literal-factory controls refused. | [#1389](https://github.com/sayuks/marginplyr/issues/1389), independent follow-up to closed #1361 |

The 360-comparison matrix covered both helpers, both directions and three eager
input classes. It established 168 missing refusals, 96 correct refusals and
96 legal independent reads. Observation mode asserted the dated failures;
each selected contract mode required the intended refusal and returned nonzero.
Controls-only contract modes checked that the same assertions could pass for
the existing refusal and independent-read controls. Input serialization checks
found no mutation of the caller's data.

A separate 120-comparison closure-transfer matrix established 36 missing
refusals, 60 correct refusals and 24 legal retained-binding reads. Its
controls-only contract mode covered 84 comparisons. The new transfer paths
were distinguished from the enumerated carriers repaired under #1361; the
closed report was neither reopened nor modified.

The user delegated ticket granularity and publication without a confirmation
checkpoint. The `to-tickets` workflow therefore treated each independently
verifiable repair boundary as a slice, with explicit acceptance criteria,
`bug` and `ready-for-agent` labels and no blocking edges.

## Additional completed comparisons

| Lane | Completed finite comparison | Result |
| --- | --- | --- |
| [Ordinary summaries](2026-11-06-summary-grouping-share-deep/summary/findings.md) | 1,156 ordered pairs from 34 per-group value kinds; 156 sequential selections; 882 across corner requests; 144 unrelated-share interference comparisons | No new acceptance/value discrepancy. Both-errored cases did not establish condition equality. |
| [Grouping family algebra](2026-11-06-summary-grouping-share-deep/grouping/findings.md) | 1,500 seeded specifications across keep/drop duplicate policies | 2,882 exact ordered family/bit/set-id matches and 118 required empty-unit refusals. The oracle independently enumerated families rather than calling the package's compiler. |
| [Typed DuckDB keys](2026-11-06-summary-grouping-share-deep/sql/findings.md) | 480 collections across ten key types, four measures, fixed/unfixed partitions, auto/portable aggregation and three sort requests | Both Total-share columns and the Parent-share column matched ratios recomputed from displayed typed denominator rows. Margin order was not asserted. |
| DuckDB semantic sources | Seven ordinary semantic-type summaries paired with two share shapes; numeric DECIMAL controls | All 14 semantic-source share requests refused; both numeric controls succeeded. Two JSON fixtures and one BIT fixture failed at setup and established no share result. |

Counts describe separate, sometimes overlapping finite probes, not the complete
input space. The lane records distinguish harness-only errors and corrected
oracles from package findings. The completed work established no guarantee for
arbitrary R reflection, arbitrary extension classes, unbounded Grouping plans,
all backend dialects/collations, hosted services, concurrent execution,
interruption, other dependency versions or other platforms.

## Revisions (2026-11-06)

The native/portable coverage claim in the typed-key row above is withdrawn.
The [later adapter audit](2026-11-06-summary-grouping-share-exhaustive/sql/findings.md#prior-adapter-label-audit)
loaded this note's exact snapshot and established that `.aggregate` was an
ordinary summary name, not an adapter option. Both loop labels selected the
native adapter. The 480 observed collections and their numeric matches were
unchanged; they did not establish portable-adapter coverage. The later
investigation independently asserted physical paths for its own finite matrix.

## Replay and publication boundary

The coordinator replayed the retained harnesses in separate R processes from
an archive of the tested main SHA. The
[replay result table](2026-11-06-summary-grouping-share-deep/coordinator/replay-results.csv)
records script digests, arguments and expected/observed terminal statuses;
adjacent transcripts retain outputs. An initial sandboxed process launcher
ended with `Operation not permitted` before obtaining a harness verdict.
That execution was collected with exit one; a separately retained execution
with child-process permission supplied the actual replay evidence.
The final 17 expected/observed terminal statuses all matched. The
[execution record](2026-11-06-summary-grouping-share-deep/coordinator/execution-record.md)
names the retained handles, tested SHA and intentionally failing modes.

Duplicate checking used all-state exports of 695 issue bodies and 684 PR
bodies. The PR export used REST pagination because the first CLI list returned
only 300 PRs. The share lane records relevant full-body/comment reads and
repaired controls. Additional live Issue/PR searches for env_get_list returned
no matches. An initial combined search argument was rejected by the CLI;
explicit repository flags supplied the successful searches. The exports were
task-local scratch data rather than tracked copies of the issue tracker.

Replay by changing into an archive of the named SHA and passing an absolute
retained script path to Rscript. The scripts load that source through pkgload
and explicitly disable testthat parallel execution. The coordinator recorded
fresh environment evidence alongside its transcripts. All owned executions
and DuckDB connections were collected before final verification.

The submission added dated research prose, standalone R harnesses and captured
evidence under investigation, which the unchanged source-package exclusions
omitted. It changed no package code, package tests, dependencies, installed
files, generated documentation, generation sources or verification tools.
Those content and consumer effects justified Review-ready source scope:
package build/check, complete suite and snapshots omitted. The submitted SHA,
current-main compatibility assessment, terminal six-check result and review
record belong on the PR. This evidence submission did not repair or close the
reported defects. CI monitoring was outside the requested scope.
