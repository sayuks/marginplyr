# Live backend share comparisons

Investigated: 2026-11-06
Repository snapshot: `952a0d9b18fd3ff4631f153432f6024ad62fcac2`

The coordinator ran these scripts from a disposable archive of the snapshot,
with testthat parallel execution disabled. The scripts did not modify package
sources. All three final executions exited zero.

| Harness | Completed scope | Observation |
| --- | --- | --- |
| `backend-oracle.R` | 30 seeded fixtures, five Grouping specifications, with/without fixed partitions, on SQLite and DuckDB: 600 direct collections | Public keys, occurrence identifiers, ordinary summaries, and Total shares matched an eager oracle. Fixtures included negative, fractional, zero, missing and empty data; specifications included cube, product, reordered sets and retained duplicate grand totals. |
| `postgres-volatile.R` | Fixed/unfixed partitions, two aggregation requests, three sort requests, and three share requests: 36 direct collections | Both share kinds used their displayed denominator values with a PostgreSQL sequence advancing in the source summary. |
| `duckdb-key-types.R` | Nine key types, fixed/variable placement, two aggregation requests and three sort requests: 108 direct collections | Parent and Total shares matched displayed summaries for date, timestamp, Boolean, decimal, blob, list, struct, map and wide-integer keys. |

The first differential harness exposed two oracle assumptions, rather than
package defects. SQL SUM over an all-missing group returns NULL while R sum
with missing removal returns zero. The final summary explicitly coalesced
that NULL to zero. Empty ordinary SQLite summaries could have logical storage;
ADR 0033 delegates arbitrary summary-result types, so the value comparison
normalized numeric/logical storage. Package-owned shares retained double
storage. These adjustments did not change package code.

The PostgreSQL script initialized a disposable PostgreSQL 17.11 cluster with
trust authentication confined to a private Unix socket, disabled TCP listening,
and stopped it on exit. The initial sandboxed initialization failed at shmget;
the authorized unsandboxed replay completed. The terminal execution handle was
`25255`, exit zero. The final random-oracle handle was `28689`, exit zero;
the typed-key handle was `42045`, exit zero. Their final logs are adjacent.

Replay from an archive of the named snapshot, passing an absolute path to the
retained script. The PostgreSQL replay needs initdb/pg_ctl on PATH and permission
to allocate shared memory. It starts and stops its own cluster. No preexisting
database or user table was used. `environment.txt` records installed versions.

These were finite value comparisons, not an exhaustive backend or ordering
proof. Hosted SQL services, other collations, concurrency, interruptions,
nonfinite source values and arbitrary extension classes were not covered.
No new SQL defect was confirmed in these loops.
