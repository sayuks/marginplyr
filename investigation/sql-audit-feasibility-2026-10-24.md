# SQL audit observation and persistence feasibility

Investigated: 2026-10-24
Repository snapshot: `26cfe5da96adf102ce8f2f3aedaf88e6f0611692`

## Scope

This investigation checked the intended existing-connection interface,
recording-failure stop behavior, and persistent storage against small
experiments and primary sources. It selected no production mechanism and
changed no product code, dependency, database server, or user configuration.

The experiment environment was R 4.6.1, DBI 1.3.0, dbplyr 2.6.0,
RSQLite 3.53.3, DuckDB 1.5.5, and RPostgres 1.4.10. Execution probes used
independent R processes and in-memory SQLite only. PostgreSQL findings were
source readings, not server-side measurements.

## Observing existing connections

| Experiment | Observed result | Limit |
| --- | --- | --- |
| Error from an entry tracer | Prevented the function body; a selected SQLite connection's CREATE did not run, while another connection still answered SELECT | Demonstrated one interception site, not all driver entry points |
| Trace a DBI generic | Saw standard `dbGetQuery()` dispatch, but not a generic function reference saved before tracing | A binding is not every existing reference to a function |
| Trace the SQLite method | Saw dispatch through the saved generic; a saved MethodDefinition called directly bypassed it | Method tracing did not rewrite all earlier references either |
| Exit tracer and `returnValue()` | Distinguished a normal NULL result from no normal return in a simple function | Did not recover the error diagnostic or distinguish every unwind cause |
| Function's own `on.exit()` | Replaced the trace exit handler | A trace exit handler did not reliably observe completion |
| Trace twice, then untrace | Second trace replaced the first; untrace did not restore the first | Simple start/stop could remove another tool's instrumentation |
| Restore saved definition | Restored an earlier trace for one ordinary function and one test S4 method | Imports, caches, and intervening third-party changes were not tested |
| `tracingState(FALSE)` | Disabled the entry tracer that otherwise refused execution | Trace alone did not enforce unconditional stop behavior |

The [R trace reference](https://stat.ethz.ch/R-manual/R-devel/library/base/html/trace.html)
also described tracing as non-nested, global disablement, and loss of an exit
tracer to `on.exit(add = FALSE)`. Installed DBI query/execute helpers used
that `on.exit()` form; RSQLite's send method also cleared exit handlers with
`on.exit(NULL, add = FALSE)` on its success path.

Reproduction scripts, to run separately in disposable R processes:

```sh
Rscript investigation/sql-audit-observation-2026-10-24/trace-boundaries.R
Rscript investigation/sql-audit-observation-2026-10-24/s4-dispatch.R
```

The scripts instrument only their process. They are evidence probes, not a
proposed recorder or acceptance tests proving complete observation.

## A driver path outside ordinary SQL send generics

Installed RPostgres 1.4.10's `db_append_table()` constructed `COPY ... FROM
STDIN` and called `connection_copy_data(conn@ptr, sql, value)`, which entered
native code through `.Call()`. Ordinary Postgres connections selected this
path when `copy = NULL`. Public `dbAppendTable()` and the insertion portion
of `dbWriteTable()` reached it without calling `dbSendQuery()`,
`dbSendStatement()`, or `dbExecute()` for that COPY statement.

Sources: the installed RPostgres namespace's `db_append_table()` and
`connection_copy_data()`,
the [table API](https://rpostgres.r-dbi.org/reference/postgres-tables.html),
and upstream [`tables.R`](https://github.com/r-dbi/RPostgres/blob/main/R/tables.R).

Observing the high-level table method could establish that an append was
requested without exposing its internal SQL. The driver-side function was
an additional observation candidate, so this did not establish impossibility
on ordinary connections. It established that SQL-argument DBI generics alone
were insufficient. COPY's input payload was also a separate route from bind
parameters; retaining bind values did not decide whether to retain that payload.

## Persistent storage comparison

| Medium | Evidence read | Assessment for this requirement |
| --- | --- | --- |
| SQLite / RSQLite | Transactional event writes; SELECT filtering and LIMIT | Strong candidate for independently committed events and partial history reads |
| JSON Lines | One JSON value per line; no transaction, locking, or disk-sync contract in the format | Simple stream, but matching the desired persistence would require additional mechanisms |
| RDS | Ordinary save/read interface stores one R object | Natural snapshot; event append needs another protocol or many files |
| DuckDB | Persistent transactions and recovery; ordinary native-file writing owned by one process | Viable candidate; no measured speed or resource ranking against SQLite |

Sources: [SQLite transactions](https://www.sqlite.org/lang_transaction.html),
[SELECT](https://www.sqlite.org/lang_select.html), [JSON Lines](https://jsonlines.org/),
[RDS](https://stat.ethz.ch/R-manual/R-devel/library/base/html/readRDS.html),
[DuckDB transactions](https://duckdb.org/docs/2.0/sql/statements/transactions),
and [DuckDB concurrency](https://duckdb.org/docs/2.0/connect/concurrency).

The architectural inference was to use a separate local audit database and
private connection, committing a start before the target operation and a
terminal event afterwards. The caller's target rollback would then not roll
back audit history. This would not make target execution and audit writes
atomic together. An unmatched start would remain incomplete evidence.
The observer would need to exclude its own storage connection.

`DESCRIPTION` already listed RSQLite in Suggests. That did not establish its
availability for every user. Requiring it for auditing would be an optional
feature dependency, including for callers whose target was PostgreSQL or DuckDB.

## Durability and type boundaries

[RSQLite's connection reference](https://rsqlite.r-dbi.org/reference/SQLite.html)
still specified `synchronous = "off"`. Explicit configuration was therefore
needed; SQLite engine defaults were not RSQLite defaults. The
[SQLite synchronous reference](https://www.sqlite.org/pragma.html#pragma_synchronous)
distinguished WAL with FULL from rollback DELETE with EXTRA as durable
configurations. WAL was not a prerequisite for persistence. The journal and
synchronization mode remained choices, not measured performance conclusions.

SQLite's guarantees depended on its documented
[storage assumptions](https://www.sqlite.org/atomiccommit.html). Ordinary R
exit, R-process failure, and system/power failure were different cases.
No power-loss, disk-full, contention, or filesystem experiment was performed.

[SQLite types](https://www.sqlite.org/datatype3.html) provided TEXT/BLOB storage,
not an R-value schema. An R serialization blob could avoid another encoder
but would still need version and type rules; the
[serialization reference](https://stat.ethz.ch/R-manual/R-devel/library/base/html/serialize.html)
did not provide a fixed long-term interchange format. Values such as NA,
NaN, integer64, dates, time zones, raw vectors, and streams still required
explicit retention behavior. Neither storage choice established safe handling
of arbitrary reference objects or permission to consume a stream.

## Unresolved technical gates

Existing-connection capture remained possible but unverified beyond the small
examples. No universal interceptor was established. Further evaluation needed
driver-specific public-path coverage, handled and propagated conditions,
cleanup ordering, recording failure followed by caller recovery, existing
instrumentation, saved references, and global tracing state. Unsupported paths
and undetectable interference had to be distinguished; absence of a record
could not prove absence of database work.

Storage still needed parameter round trips, bounded live memory, interruption
recovery, write-failure behavior, and measured overhead. No historical storage
recommendation or new user assent supplied that evidence.
