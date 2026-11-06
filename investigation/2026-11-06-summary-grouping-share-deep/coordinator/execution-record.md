# Coordinator replay execution record

Investigated: 2026-11-06
Tested package SHA: `547d77095c2923573216d002f50bef21778247f9`

All retained scripts loaded a disposable archive of this SHA from
`/private/tmp/marginplyr-deep-sql`. Script digests and arguments are in
[replay-results.csv](replay-results.csv); [environment.txt](environment.txt)
records R, platform and dependency versions. Each execution ran sequentially
with testthat parallel execution disabled. No package implementation was edited.

| Execution | Original retained terminal session | Terminal outcome |
| --- | --- | --- |
| Initial sandboxed coordinator launcher | 98492 | Exit 1: Operation not permitted before obtaining a harness verdict. No package conclusion was drawn. |
| Fourteen-script independent replay with child-process permission | 96822 | Exit 0: all 14 expected terminal statuses matched. |
| Three-mode closure-transfer independent replay | 48064 | Exit 0: observation 0, desired-contract 1, controls-only 0; 17 combined statuses matched. |
| Grouping replay after numeric-literal formatting | 37828 | Exit 0: 2,882 exact matches and 118 required refusals; final digest/transcript selected in the result table. |

The successful main replay covered observation and red/green modes for each
of index/options/bulk, four ordinary-summary scripts, the seeded Grouping
family oracle, typed DuckDB keys and exploratory semantic source types.
The second replay appended the closure-transfer modes to the same result
table. The four desired-contract modes intentionally exited one because the
required Package condition was absent. Their green controls exited zero;
no implementation repair was asserted.

The first source-scope Review-ready invocation rejected two research literals
without a leading zero. Changing .4/.3 to 0.4/0.3 preserved the complete R
parse tree against commit `4bd16cb3c7a1ab911c72153de14076bb7fd04eb3`.
The final Grouping script was then replayed against the original package SHA;
its result row and coordinator transcript were refreshed with the new script
digest. The earlier lane transcript remained unchanged. This formatting
correction changed neither the seeded fixture nor its measured verdicts.

The parent coordinators used synchronous processx execution with child-tree
cleanup enabled. All child results were collected before parent completion.
The typed-key script disconnected its DuckDB connection on exit. The local
source archives and task-local tracker exports were scratch material; the
retained scripts/transcripts were the reviewable evidence. No execution or
database connection owned by this investigation remained live afterwards.

The main replay also independently executed the final assertion-bearing
type-pair script after the lane's original combined harness had stopped in a
later harness-only construction phase. Its 1,156 comparisons completed with
zero differences and exit zero. Initial harness mistakes and oracle corrections
remain described in the lane notes; they are not package findings.
