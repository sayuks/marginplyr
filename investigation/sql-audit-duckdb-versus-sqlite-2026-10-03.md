# DuckDB as a local SQL audit sink

Investigated: 2026-10-03

## Scope and assessment

This source-only investigation compared small independently committed audit
events with analytical use of accumulated history. It installed no packages,
ran no benchmark/crash test, and changed no product decision or earlier note.
The DuckDB `stable` documentation links redirected to `current`, labeled 1.5;
the separate LTS pages were labeled 1.4.

DuckDB was a viable transactional disk sink. SQLite remained the first candidate
for simple local event persistence, particularly if separate R processes might
write one history file. If duckdb was already available and one process owned
the audit database, using it instead could avoid installing RSQLite. Analysis of
large accumulated histories was a plausible DuckDB use case. These were fit
assessments, not claims that DuckDB was incapable of small writes, slower than
SQLite, or used more memory in this workload.

## Findings

| Topic | Observed official evidence | Audit implication/inference |
| --- | --- | --- |
| Persistence and transactions | DuckDB supported file-backed databases and ACID transactions. Its crash guide described replaying an adjacent WAL on reopening the persistent database. | Start and terminal events could be committed independently without retaining the complete history in R. An explicit database file was necessary; in-memory mode was insufficient. |
| Small transactions | The 1.4 LTS concurrency page described bulk optimization and many small transactions as outside the primary design goal. The checked 1.5 current page did not include that caution. | The LTS statement was a workload-design clue, not a prohibition or a benchmark. Its version scope mattered. |
| In-process concurrency | Current documentation allowed one read-write process, or multiple read-only processes with no writer. Multiple writer threads within the owning process were supported; appends did not conflict. | One private audit instance in an R process was plausible. Independent R processes could not simply keep read-write handles open to the same native file. |
| Additional multi-process options | Current documentation described Quack client-server writes, labeled beta since 1.5.2, and DuckLake with a PostgreSQL catalog as another solution. | Multi-process writing was possible through additional architecture; that was different from a simple local-file sink. No announced future maturity was treated as achieved. |
| Memory | `memory_limit` applied to the buffer manager; vectors, query results, and some aggregate states were outside it. | Disk-backed history did not imply zero memory use or a strict cap on all process memory. Retrieval needed to return only requested records. |

Sources: [DuckDB persistence](https://duckdb.org/docs/current/connect/overview),
[transactions](https://duckdb.org/docs/current/sql/statements/transactions),
[crash recovery](https://duckdb.org/docs/current/guides/troubleshooting/crashes),
[1.4 LTS concurrency](https://duckdb.org/docs/lts/connect/concurrency),
[1.5 current concurrency](https://duckdb.org/docs/current/connect/concurrency),
and [memory-limit caveat](https://duckdb.org/docs/current/configuration/pragmas#memory-limit).

The DuckDB team's May 12, 2026 Quack article explicitly described small appends
for centralized observability and measured one INSERT transaction per row. Its
comparison used PostgreSQL and Arrow Flight, not SQLite or this R audit design.
It supplied a counterexample to treating the older bulk-optimization caution as
proof that small-event storage was unsuitable. Its reported throughput was not
adopted as an estimate for this implementation.
[Quack article, Small Writes](https://duckdb.org/2026/05/12/quack-remote-protocol#small-writes)

SQLite's official guidance emphasized application-local storage and allowed
processes to take turns as its single writer. That simpler local coordination
was one basis for retaining SQLite as the first candidate. DuckDB's analytical
orientation and its LTS guidance to analyze data held in SQLite or other stores
also supported considering DuckDB for downstream history analysis. No crossover
history size, per-event overhead, or comparative throughput was established.
[Appropriate SQLite uses](https://www.sqlite.org/whentouse.html),
[DuckDB LTS concurrency](https://duckdb.org/docs/lts/connect/concurrency)

## R integration and storage boundaries

The inspected repository `DESCRIPTION` already listed both duckdb and RSQLite
in Suggests. Either selected sink would still require its package to be present;
Suggests did not guarantee installation. The R DuckDB reference documented
`dbConnect()` and a memory default, plus instance reuse for connections pointing
to one file. This research did not equate the latest reference behavior with
every supported installed package version or verify a user installation.
[R DuckDB connection](https://r.duckdb.org/reference/duckdb.html)

Either DBI-based sink would need a private connection and separate audit database
outside the target transaction, excluded from global observation with a re-entry
guard. Independent start/end commits did not form one atomic transaction with
target execution. An audit-sink failure still required a stop/gap/fallback policy;
switching engines did not solve that policy automatically. These were the same
design inferences documented in the earlier
[disk-first investigation](sql-audit-disk-first-and-transactions-2026-10-02.md).
