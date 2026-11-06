# Boundary bug hunt for Margin summaries, Grouping specifications and shares

Investigated: 2026-11-06
Repository snapshot: `9ffb24614a71512170298a3827bc11c80354e742`

The investigation began from freshly fetched main in the new worktree
`/private/tmp/marginplyr-summary-grouping-share-exhaustive-20261106`, on branch
`investigation/summary-grouping-share-exhaustive-20261106`. Four investigators
divided ordinary summary evaluation, Grouping selection, contextual-share
dependencies and live SQL comparisons. The coordinator fetched main again
before publication; it remained at the tested SHA. No package implementation
repair or package regression test was added.

The [earlier November 6 hunt](summary-grouping-share-bughunt-2026-11-06.md),
[deep hunt](summary-grouping-share-deep-bughunt-2026-11-06.md), and November 5
lane evidence supplied repaired controls and remaining boundaries. Earlier
finite matrices were read for orientation rather than counted as executions
of this investigation. The title describes the deliberate boundary search;
it does not claim exhaustive coverage of arbitrary R programs.

## Confirmed findings and independent repair slices

The user explicitly delegated ticket granularity and publication without a
confirmation checkpoint. The `to-tickets` workflow therefore treated each
independently verifiable boundary as a repair slice, with acceptance criteria,
`bug` and `ready-for-agent` labels and no blockers. No prefactoring prerequisite
was established. The tickets request repairs; this research PR does not close
them.

| Boundary | Established effect | Independent ticket |
| --- | --- | --- |
| Unnamed frame-subclass casts | A compatible cross-frame unit conversion was skipped, producing 1, 100 where ordinary dplyr produced 1, 1. | [#1399](https://github.com/sayuks/marginplyr/issues/1399) |
| Locally rebound Grouping callees | The name-only proof described an old lexical function; fixed-key calls falsely refused and dtplyr inspection could silently select the wrong column. | [#1400](https://github.com/sayuks/marginplyr/issues/1400) |
| Visible closure-valued syntax | Blocks, branches and visible factory results lost a later invocation's forbidden share/alias dependency. | [#1401](https://github.com/sayuks/marginplyr/issues/1401) |
| Known base closure transfers | force, literal get and match.fun lost the returned closure's invocation-time dependency. | [#1402](https://github.com/sayuks/marginplyr/issues/1402) |
| Literal list element writes | Appending or replacing a closure after list construction lost its dependency provenance. | [#1403](https://github.com/sayuks/marginplyr/issues/1403) |
| Aliased mask environments | Saving a recognized environment path in a local variable bypassed repaired field/accessor dependency checks. | [#1404](https://github.com/sayuks/marginplyr/issues/1404) |

The [summary lane](2026-11-06-summary-grouping-share-exhaustive/summary/findings.md),
[Grouping lane](2026-11-06-summary-grouping-share-exhaustive/grouping/findings.md),
and [share lane](2026-11-06-summary-grouping-share-exhaustive/share/findings.md)
retain minimized public-interface witnesses, contract sources, discriminating
controls, hypotheses, likely causes, versions and duplicate comparisons.
Observation modes assert the dated failures. Desired-contract modes require
the correct result or existing dependency refusal and intentionally return
nonzero on the investigated source. Green controls exercise the same intended
assertions without requiring an implementation repair.

## Completed finite comparisons

| Lane | Completed comparison | Observation |
| --- | --- | --- |
| Ordinary summaries | Three eager input classes, fixed/included/rollup roles, bare-column/selected-frame expressions | All 18 unnamed cases bypassed the original frame cast; all 18 named controls retained correct values. |
| Other ordinary-summary boundaries | 120 dtplyr special-name comparisons and 180 eager control-flow/value-shape comparisons | No additional value or acceptance discrepancy. Nine paired refusals in the latter established no diagnostic-equality claim. |
| Grouping callback rebinding | 68 public-consumer probes, each paired with typed-wrapper and literal-selection references | 48 false overlap refusals, five silent wrong dtplyr inspection plans and 15 recovered eager plans. All 68 wrapper controls matched literal references. |
| Closure dependency flow | 480 comparisons across both share helpers, both dependency directions and four eager/lazy input classes | 192 missing refusals, 96 correct refusals and 192 legal reads; all input serializations unchanged. |
| Environment-value aliases | 204 comparisons across both share helpers, both dependency directions and three eager inputs | 72 missing refusals, 60 correct refusals and 72 legal reads; input serializations unchanged. The lane explicitly excluded its non-equivalent dtplyr exploratory outcomes. |
| Live DuckDB | 384 collections across four string comparison relations, eight numeric aggregates, fixed/variable key placement, actual native/portable adapters and three sort requests | Both share columns matched displayed denominator rows at tolerance 1e-12. Margin order was not asserted. |

Counts describe distinct finite probes, with some overlapping controls; they
are not a single independent sample or a full-suite result. The
[SQL lane](2026-11-06-summary-grouping-share-exhaustive/sql/findings.md)
records its database-equivalence oracle and physical-path assertions. No
guarantee was established for arbitrary reflection, unknown closure carriers,
every custom class, unbounded Grouping plans, all dialects/collations, hosted
services, concurrency, interruption, other versions or other platforms.

## Earlier adapter-coverage correction

An audit against the earlier deep hunt's exact source SHA
`547d77095c2923573216d002f50bef21778247f9` established that its `.aggregate`
loop argument became an ordinary summary column. Both labels produced native
GROUPING SETS SQL. Its observed numeric matches were retained, but its
native/portable coverage claim was withdrawn through narrow dated revisions
in the SQL lane and overview. The
[audit evidence](2026-11-06-summary-grouping-share-exhaustive/sql/findings.md#prior-adapter-label-audit)
records the command, result and citation search. Original findings and
scripts were left intact. This corrected an evidence claim and introduced no
package behavior or new decision.

## Independent replay, duplicate checks and publication

The coordinator replayed the finding harnesses in separate R processes from
an archive of the tested main SHA, using copied research scripts. The
[final replay table](2026-11-06-summary-grouping-share-exhaustive/coordinator/replay-results.csv)
records script digests, arguments and all 15 expected/observed terminal
statuses. Six desired-contract modes returned one; observation/control modes
returned zero. The
[execution record](2026-11-06-summary-grouping-share-exhaustive/coordinator/execution-record.md)
separately records the first matrix path failure and its corrected replay.
No package failure was inferred from that harness setup error. All owned
executions and database connections were collected before final verification.
Two auxiliary controls were also replayed after making their CSV output
script-relative; their 180/120 comparison rows matched the original CSVs
exactly. The execution record identifies their separate submitted-source SHA.

The initial all-state exports contained 702 issue bodies and 691 PR bodies;
the PR export used REST pagination. The publication-time CLI refresh returned
705 issues and 691 PRs. The three new reports #1396, #1397 and #1398 were read
in full with comments and described authored namespace evaluation, registered
head dispatch and derived SQLite expansion respectively. None covered these
six demonstrated boundaries. Lane notes record the other all-state searches,
relevant full-body/comment reads and repaired controls. Tracker exports were
scratch data rather than committed copies of GitHub. Each published ticket's
title, body and labels were checked after creation.

Replay from an archive of the named SHA, passing the retained harnesses by
absolute path. Every harness disables testthat parallel execution. The
coordinator driver accepts source root, artifact root and output directory;
it copies harnesses to its output before starting sequential child processes.
The SQL audit additionally names the older SHA it tests. Logs, CSVs and lane
environment records accompany the scripts.

The cumulative submission added dated prose, standalone research harnesses
and captured evidence, and added dated corrections to two older research
notes. The unchanged source-package exclusions omitted investigation content.
No package code/test, dependency, installed file, generated documentation,
generation source, build rule or verification tool changed; no package-check
consumer was established. These content and consumer effects justified
Review-ready source scope: package build/check, complete suite and snapshots
omitted. The submitted SHA, current-main assessment, mandatory baseline result
and review record belong on the PR. No NEWS entry was needed for this
research-only submission. CI monitoring was outside the requested scope.
