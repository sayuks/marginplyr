# Greedy bug hunt for Margin summaries, Grouping specifications, and shares

Investigated: 2026-11-05
Tested main: `0403c9150394cbae21b59f34c75791d0873b8451`
Branch: `investigation/summary-grouping-share-greedy-20261105`

The investigation started from main in a new worktree at
`/private/tmp/marginplyr-summary-share-greedy-20261105`. Fetched origin/main
matched that snapshot at preparation and again before publication. The
[earlier November 5 hunt](../summary-grouping-share-bughunt-2026-11-05.md),
[November 4 hunt](../summary-grouping-share-bughunt-2026-11-04.md), and
[November 4 follow-up](../summary-spec-total-share-followup-2026-11-04.md)
supplied repaired controls and duplicate leads. Four lanes examined ordinary
summary evaluation, Grouping specification resolution and algebra, contextual
share dependencies and deferred execution, and live SQL names/types.

## Confirmed findings

Four independently verifiable repair slices were selected. All had no blockers;
the user delegated ticket granularity and publication. All four received
`bug` and `ready-for-agent` labels. The repair tickets are linked below.

| Slice | Observable failure | Evidence |
| --- | --- | --- |
| [#1343](https://github.com/sayuks/marginplyr/issues/1343): local summary naming | A caller namespace operator intercepted package-generated dynamic across naming glue and replaced the public output name. | [Summary lane](summary/README.md) |
| [#1344](https://github.com/sayuks/marginplyr/issues/1344): Grouping selection | A name-only proxy supplied integer placeholders to a stringr pattern's S3 length method, falsely rejecting a selection that typed tidyselect accepted. | [Grouping lane](spec/README.md) |
| [#1345](https://github.com/sayuks/marginplyr/issues/1345): share dependencies | Literal mask-accessor forms bypassed both forbidden dependency directions while repaired equivalent forms refused. | [Share lane](share/README.md) |
| [#1346](https://github.com/sayuks/marginplyr/issues/1346): deferred dtplyr shares | Namespace operators in the lazy root intercepted package-generated helpers and corrupted ratios or raised at collection. | [Share lane](share/README.md) |

The distinction from earlier tickets was established from their complete issue
bodies, comments, repairs and separating controls. Those tickets were not
reopened, closed, or otherwise changed by this investigation. The lane records
hold the search queries, matching reports, environment and detailed scope.

## Independent confirmation and coverage

[coordinator/replay.R](coordinator/replay.R) archived the immutable tested SHA
outside this worktree and ran the desired-behaviour assertion for each finding
in a separate R process. All four returned exit 1 at their intended contract
assertion; [the result table](coordinator/replay-results.csv) and per-call logs
preserve that independent confirmation. A failure asserted by these research
scripts was evidence of a bug, not a failing package test introduced by this PR.

The summary lane recorded 850 discovery comparisons (848 matched outcomes and
two reserved-helper spelling refusals excluded from findings), followed by a
108-case naming matrix with 54 wrong-name cases and 162 successful literal or
delegating controls. The Grouping lane checked 2,697 independent composition
comparisons with one oversized tree excluded, plus 72 pattern-dispatch failures
and 144 typed controls. The share lane checked 192 accessor cases (96 missing
refusals) and 96 deferred namespace cases (32 wrong ratios, 32 collection
errors, and 32 unwanted callbacks). Each lane states its comparison limits.

The [SQL lane](backend/README.md) completed 736 name/role/order/backend
comparisons: 672 supported cases matched the eager oracle, and 64 refusals
were explained by existing input or upstream output-name constraints.
Separate ordinary-backend controls checked those refusals and the accepted
names repaired by #971. A DuckDB arithmetic binding probe rejected six
nonnumeric semantic types and retained integer/decimal types; unavailable
JSON/INET extensions remained outside the evidence.

No package implementation, package test, exported documentation, dependency,
build rule, or verification tool was changed. The added scripts and data were
standalone dated research under the existing source-package exclusion. The
investigation used focused probes, not the complete package suite. It did not
establish absence of bugs beyond the finite cases and available environments.

## Replay and verification boundary

Run lane scripts with the source root as the working directory. The scripts
load that source through pkgload. `--assert` on the four coordinator targets
requests the intended contract and deliberately failed on the tested SHA;
ordinary observation modes checked the recorded failures and controls. See
each lane for its artifact-output locations and optional dependencies.

The submission's effects justified Review-ready `source`: only research prose,
probes and recorded observations were added; package-check inputs and their
consumers were unchanged. The PR verification record identifies its committed
SHA, six-check baseline and explicit omission of package build/check, complete
suite and snapshots. That gate did not repair or certify the recorded bugs.
