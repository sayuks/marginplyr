# Accepted execution plan and ownership map

Investigated: 2026-09-30
Target: fixed HEAD, current recorded dependencies, synthetic data, isolated child R processes and private libraries.

Execution order: INSERT error/interrupt/SIGINT pilot; acquisition/write/late-result/release boundaries; outer transaction, temporary, attached, `.env`, and option variants; real lock and invalid-connection cleanup failures; collection/type repair; audit/options; dialect probe/cache; local warning replay. Every sequence observes before experimental disposal and tests the same session without namespace/cache resets. Separate commit/rollback controls omit retry before the caller decision.

Caller owns input, old destination/indexes, connection, outer transaction, and user-expression side effects. marginplyr owns its savepoint, temporary options, warning buffer, per-call audit record, and measured dialect cache. Dependencies own result handoff/finalization and backend transaction failure semantics. Ordinary failure must restore destination and release owned work; interrupted-operation guarantees are assessed separately. Failed audit prefixes remain readable, unknown probe attempts retry, and valid measured answers remain cached.

Pilot acceptance requires normal seed success, one reached event after completed INSERT, distinct conditions, same-process immediate observation, and independent supervision. Corrected repetitions 2-4 passed all three mechanisms. Non-arriving/invalid observations remain excluded. Cleanup implementations are never mocked.

Limits: one case at a time, at most two experiment R processes, at most 100 source rows, 20 MiB per database, 60 seconds per case, 500 MiB retained bundle. Native cancellation, OOM, SIGKILL, disk exhaustion, and production/environment mutation are excluded.

The report boundary table and cases.csv map completed effects, existing evidence references, additions, classifications, and limits. No product/GitHub changes belong to this stage. Completion means evidence for adopted boundaries, not a finding count.
