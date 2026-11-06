# Expansion and Grouping specification deep boundary hunt

Investigated: 2026-11-06
Baseline: `547d77095c2923573216d002f50bef21778247f9` (freshly fetched `main`)
Scope: dated investigation evidence and implementation tickets; no fixes

A new main-based worktree divided the public surface across local structured
expansion, Grouping specification capture/normalization, deferred Arrow/dtplyr
source graphs, and live SQL boundaries. The November 5
[adversarial](2026-11-05-expand-grouping-adversarial/README.md) and
[greedy](2026-11-05-expand-grouping-greedy/README.md) investigations and the
November 6 [boundary hunt](expand-grouping-bughunt-2026-11-06.md) supplied prior
coverage and exclusions. Those observations were not credited as new runs.
The tested baseline already included the #1363/#1364/#1366 repairs.

## Findings and ticket boundaries

| Finding | Published ticket | Independently verifiable behavior |
| --- | --- | --- |
| G1 | [#1383](https://github.com/sayuks/marginplyr/issues/1383), P2 | Preserve ordinary qualified Grouping-selection boundaries through embracing, injection and numbered-dot forwarding. |
| S1 | [#1384](https://github.com/sayuks/marginplyr/issues/1384), P3 | Keep valid wide included-only SQLite expansions within result-column limits under Margin order. |

`to-tickets` produced two independent implementation slices, both labelled
`bug` and `ready-for-agent`, with no blockers or prerequisite prefactoring.
The user delegated granularity and publication, so the breakdown quiz was
omitted. No existing issue was closed or changed, and no fix was attempted.

### G1: forwarding removed an ordinary selection's evaluation boundary

A public wrapper used `{{ column }}` inside publicly qualified base
parentheses in a Grouping constructor. A source column `a` and a lexical
binding `a = "b"` distinguished tidyselect's data selection walk from ordinary
qualified-call evaluation. Direct typed tidyselect selected `b`; Grouping
inspection and Margin operations selected `a`. The same qualified call without
forwarding, an identity wrapper, and a fixed-key selection selected `b`.
Syntactic parentheses correctly selected `a` and supplied the opposite control.

The [specification track](2026-11-06-expand-grouping-deep/spec/README.md)
recorded 400 wrong results and 1,000 successful controls in a 1,400-comparison
constructor/consumer matrix. A further 200 comparisons reproduced the wrong
selection through embracing, explicit quosure injection and two numbered-dot
forms. Valid calls also falsely refused duplicate sets or fixed-key overlap.
The whole forwarded argument was replaced by its inner captured argument,
discarding the qualified boundary before selection classification. Closed
#1315/PR #1330 repaired retained-call classification, while #897/#1214/#1234
covered syntactic forwarding boundaries; their complete discussions were read.

An [independent live-SQL replay](2026-11-06-expand-grouping-deep/sql/README.md#independent-g1-confirmation)
changed names, values and family shape and confirmed wrong inspection and
expansion on SQLite and DuckDB. Explicit branch/value/type controls passed.
This extended the same finding rather than creating per-backend tickets.

### S1: private ordering columns exceeded an otherwise valid public width

A two-row SQLite source held one included grouping dimension and 1,998
payload columns. Ordinary arrangement and unsorted expansion both returned the
exact 1,999-column public result. Last Margin order projected two additional
private columns and collection failed with `too many columns in result set`.
A 1,998-column source succeeded with the same two private columns. A separate
fresh-main archive reproduced the failing and passing sides of the boundary.

The [SQL track](2026-11-06-expand-grouping-deep/sql/README.md) retained the
replays, exact diagnostics, successful controls and source/declared-type
contract assessment against ADR 0018 and ADR 0031. Closed #980 concerned the
number of compound SELECT terms, whereas this plan held one occurrence.
The finding requested admissible translation for the demonstrated valid
schema, not support for arbitrary backend-exceeding schemas or unlimited plans.

## Additional coverage and exclusions

| Probe | Final observations | Result and limits |
| --- | ---: | --- |
| [Local structured arrays](2026-11-06-expand-grouping-deep/local/README.md) | 11,340 | 9,612 exact matches; 1,728 ordinary sorting recursion failures; zero package-only differences or fixture failures. |
| Local record/list boundaries | 567 | 486 exact matches; 81 ordinary AsIs-factor combination failures. Sixteen separating fixtures independently confirmed ordinary value-order failures and exact unsorted unions. |
| [Arrow physical source families](2026-11-06-expand-grouping-deep/lazy/README.md) | 1,188 | 972 exact physical-array matches; 216 ordinary unsupported list/map ordering exclusions. Canonicalized comparisons did not establish physical Arrow Margin order. |
| dtplyr multi-root graphs | 486 | All schema/values/multiplicities, physical Margin order, predicate-evaluation counts and caller-table immutability comparisons passed. |
| Columnless Arrow containers | 216 | All row counts and occurrence identifiers matched across Table, RecordBatch, Dataset and UnionDataset consumption routes. |
| Included-only SQLite dimension widths | 20 | Fifteen expansion and five ordinary-arrangement comparisons matched at widths 490, 499, 500, 501 and 510. |

Track notes recorded hypotheses, ordinary controls, fixture corrections, final
counts and exclusions. No count from an interrupted, erroneous or preliminary
matrix was added to a final run. Unsupported ordinary operations were not
promoted to package defects. Existing safe refusal of unknown derived dtplyr
metadata was retained as an exclusion under ADR 0020.

## Main freshness, duplicate checks and replay

Main was fetched again before publication and still named the baseline SHA.
Both findings were independently replayed in a separate `git archive` copy of
that revision, using fresh R processes and explicit desired-contract assertions.
Each defect assertion exited one after successful separating controls; the
1,998-column threshold assertion exited zero. These failures demonstrated
unfixed defects and were not added to package regression tests.

Duplicate checks used an all-state 695-issue snapshot, the latest 300 all-state
PRs and a publication-time recent-issue refresh. Live `gh api` search queries
covered issues and PRs together, including closed reports:

- `repo:sayuks/marginplyr "qualified" "parentheses"`: 21 matches, including
  #1315, duplicate #1322, #1288, #1273 and PRs #1297/#1330. Full relevant
  discussions distinguished whole-argument normalization from those repairs.
- `repo:sayuks/marginplyr "qualified" "quosure"`: six matches, including
  PR #1330; no equivalent whole-argument report.
- `repo:sayuks/marginplyr "parentheses" "forward"`: ten matches, including
  #897/#1214/#1234/#1227/#925; the relevant complete bodies/comments were read.
- `repo:sayuks/marginplyr "too many columns"`: zero matches.
- `repo:sayuks/marginplyr "column" "limit" "SQLite"`: 33 matches, including
  prior direct-result, LIMIT, Boolean-alias and compound-width investigations;
  no equivalent one-occurrence result-column report.
- `repo:sayuks/marginplyr "projected" "order" "columns" is:issue`: one
  closed Arrow alias report, #992, outside the SQLite boundary.

The specification lane's direct `gh ... --search` requests encountered a
sandbox host rejection. The coordinator's successful live `gh api` searches
above supplied publication-time checking; this limitation left no pending
duplicate check. The recent refresh found unrelated open #1382 and no duplicate.

Replay commands and logs were retained in the
[evidence bundle](2026-11-06-expand-grouping-deep). Each lane recorded its
environment and terminal executions. All owned executions were collected.
The environment was arm64 macOS, R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0,
rlang 1.3.0, tidyselect 1.2.1, vctrs 0.7.3, dtplyr 1.3.3,
data.table 1.18.6.1, Arrow 25.0.1, RSQLite 3.53.3 and DuckDB 1.5.5.
Hosted stores, live PostgreSQL/SQL Server, other platforms/versions, arbitrary
caller extension classes and unlimited plan/resource bounds were not measured.

Only dated evidence and standalone replays under the existing investigation
exclusion were added. Package code/tests, examples, vignettes, installed files,
generated package documentation, dependency metadata, build rules and check
tooling were unchanged. Nothing invoked the new scripts from those consumers;
they documented observed behavior without changing package/check execution.
Those effects justified Review-ready `source` scope. The PR held its pinned
verification and Standards/Spec review records; package build/check, complete
suite and snapshots were omitted. No NEWS entry was required for research
without a user-facing behavior change. CI was not monitored.
