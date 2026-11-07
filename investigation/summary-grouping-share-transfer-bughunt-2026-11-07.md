# Transfer and prediction bug hunt for Margin summaries, Grouping specifications and shares

Investigated: 2026-11-07
Repository snapshot: `0b5143133c023ccb4efb6fe27f1ef551e9f49186`

The investigation began from main in the new worktree `.worktrees/summary-grouping-share-bughunt-2026-11-07`, on branch `investigation/summary-grouping-share-bughunt-2026-11-07`. Remote main was fetched before worktree creation and matched that snapshot. Four investigators divided ordinary Margin summaries, Grouping specifications, contextual-share dependency planning and independent boundary comparisons. They used disposable Git archives and sequential R processes per lane, with testthat parallel execution disabled. No package implementation or package regression test was changed.

The [earlier November 7 carrier hunt](summary-grouping-share-carrier-bughunt-2026-11-07.md), [November 6 deep hunt](summary-grouping-share-deep-bughunt-2026-11-06.md) and [November 6 boundary hunt](summary-grouping-share-exhaustive-bughunt-2026-11-06.md) supplied repaired controls and adjacent unexplored boundaries. Their results were not counted as executions of this investigation. The investigated main included the preceding carrier, dispatch and namespace repairs through PR #1426.

## Confirmed findings and repair slices

The user delegated ticket granularity and publication without a confirmation checkpoint. The `to-tickets` workflow therefore used five independently verifiable repair boundaries. Each ticket had `bug` and `ready-for-agent`, acceptance criteria and no blockers; no necessary prefactoring prerequisite was established. No ticket was repaired or closed by this research submission.

| Boundary | Established effect | Ticket |
| --- | --- | --- |
| Predictions across preceding summaries | Earlier rebinding of a function configuration or unpack option made an obsolete output-name prediction falsely conflict with a public key or identifier. | [#1428](https://github.com/sayuks/marginplyr/issues/1428) |
| Qualified formula-shaped Grouping selections | A public-qualified ordinary call ran against integer placeholders, silently changing fixed partitions or falsely rejecting a union as duplicate. | [#1429](https://github.com/sayuks/marginplyr/issues/1429) |
| Known unchanged-list returns | Identity, invisible and force lost a closure list's invocation-time dependencies. | [#1430](https://github.com/sayuks/marginplyr/issues/1430) |
| Literal list-name writes | An obsolete member-name map missed prohibited dependencies and falsely refused an independent member. | [#1431](https://github.com/sayuks/marginplyr/issues/1431) |
| Nested literal containers | Literal paths through a nested closure list lost the inner callable's dependency. | [#1432](https://github.com/sayuks/marginplyr/issues/1432) |

The [ordinary-summary lane](2026-11-07-summary-grouping-share-transfer-hunt/summary/findings.md), [Grouping lane](2026-11-07-summary-grouping-share-transfer-hunt/grouping/findings.md) and [share lane](2026-11-07-summary-grouping-share-transfer-hunt/share/findings.md) retain minimized public witnesses, violated contracts, observed versus expected behavior, separating controls, supported causes and duplicate comparisons. The ordinary-summary and Grouping cases concern predictions made from a function shape that did not establish the expression's actual behavior. The three share cases concern distinct propagation or mutation boundaries, rather than one wide refactor: known list-return transfer, list name-map mutation and nested literal container resolution can each be repaired and verified independently.

## Completed finite comparisons

| Lane | Finite scope | Observation |
| --- | --- | --- |
| Ordinary output prediction | Four entrances, three public-name roles and three eager input classes | 36 false refusals; 36 literal controls, 36 genuine-collision refusals and nine reverse controls behaved as intended. |
| Ordinary value/type comparison | All 400 ordered pairs from 20 per-group return shapes | No new acceptance or value discrepancy. |
| Grouping family algebra | 1,500 seeded recursive specifications across all five constructors, with duplicate removal | Exact ordered set families matched an independent naive expansion. |
| Formula-shaped Grouping selection | Four input classes, fixed/union entrances, inspection/summarization and both namespace operators | 16 public-qualified failures, 16 private-qualified controls, 32 parenthesized controls and three genuine-formula refusal controls. |
| Share list dependencies | Both helpers and prohibited directions, three eager classes, seven forms and retained/removed bindings | 60 missing refusals, 24 correct refusals and 84 legal calls across 168 comparisons; separate control mode passed 108 comparisons. |
| Legal renamed member | Both helpers and directions, three eager classes, named versus positional lookup | 12 false refusals and 12 legal positional controls. |
| Share numeric oracle | 24 seeds, local/dtplyr, three sort requests, sum/mean/count, missing keys and positive/negative/zero/missing values | 144 collections matched 9,540 displayed-denominator ratio assertions. |
| Nested operation inputs | Seven positional/exact/partial/reordered/nested input spellings | All seven correctly refused the enclosing prohibited share dependency. |
| Typed fixed keys | Seven key kinds, local/dtplyr and three sort requests | All 33 accepted results matched both displayed-denominator shares; nine dtplyr requests met separately confirmed upstream restrictions. |

The [coordinator lane](2026-11-07-summary-grouping-share-transfer-hunt/coordinator/findings.md) records the supplementary nested-input and typed-key checks. Counts describe separate, sometimes overlapping finite comparisons. Both-errored ordinary-summary pairs did not establish condition equality. The typed-key oracle did not assert Margin order, and nine upstream dtplyr restrictions were not treated as passing result comparisons.

## Independent replay and publication

The coordinator independently replayed the final finding harnesses, controls,
Grouping-family oracle and numeric comparisons from a separate disposable Git
archive of the named main SHA. The [replay table](2026-11-07-summary-grouping-share-transfer-hunt/coordinator/replay-results.csv)
records script digests, arguments, retained execution identities and expected
versus observed terminal statuses. Six desired-contract modes intentionally
returned one; observation and control modes returned zero. The supplementary
coordinator comparisons are recorded separately in the same table. The
[execution record](2026-11-07-summary-grouping-share-transfer-hunt/coordinator/execution-record.md)
records source identity, environment, harness-only adjustments and limits.
Freshly fetched main still resolved to the tested SHA before publication.
The five created tickets were fetched again; their published bodies and
`bug`/`ready-for-agent` labels matched the prepared tickets.

Duplicate discovery used all-state exports of 721 issue bodies/labels/comments and 703 pull requests through REST pagination, followed by live recent-issue checks. The lane notes name the relevant full-body/comment reads and distinguish the repaired controls from each new failure. Tracker exports remained in task-local scratch storage. The coordinator read #1389, #1413, #1414, #1415, #1421 and #1400 with their comments before choosing the slices; other lane-specific reads are recorded alongside their evidence.

No guarantee was established for arbitrary R programs or reflection, every custom class or carrier, unbounded Grouping plans, all SQL dialects/collations, hosted services, concurrency, interruption, another dependency version or another platform. These finite probes did not establish exhaustiveness over the R language.

The cumulative submission added only dated prose, standalone research harnesses and captured evidence. Its consumers were research readers, explicit replay commands and repository source linting. No package code/test, dependency, installed file, generated package documentation, generation source, build exclusion or verification tool changed. The unchanged source-package exclusion omitted investigation content. These content and consumer effects justified Review-ready source scope: package build/check, complete suite and snapshots omitted. No NEWS entry was needed for this research-only submission. The submitted SHA, fresh-main compatibility assessment, mandatory baseline outcome and separate review record belonged on the PR. CI monitoring was outside the requested scope.
