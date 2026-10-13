# Storage media for SQL audit events

Investigated: 2026-10-02

## Scope and conclusion

This source-only investigation compared persistent local storage for generated
SQL and execution attempts, including attempts that returned an error. No
packages were installed, benchmarks run, or product decisions changed.

SQLite was the strongest candidate when indexed history, status lookup, and
transactional writes mattered. JSON Lines was the lighter candidate for an
append-only event stream. CSV was better suited to exported tables, RDS to R
snapshots, and Parquet to batched analytical export. These suitability judgments
were inferences from the documented capabilities below, not measured results.

## Comparison

| Medium | Documented capabilities observed | Implication for this audit use case |
| --- | --- | --- |
| SQLite through RSQLite | SQLite supported transactional INSERT/UPDATE; RSQLite opened a local database path. SQLite storage classes included TEXT and BLOB, rather than arbitrary R list objects. | Start and terminal events could share an attempt ID; indexes and views could reconstruct status without overwriting history. Nested parameters would need JSON text or serialized blobs and an explicit type representation. Connection lifecycle, schema, locks, and durability settings added implementation work and an RSQLite dependency. |
| JSON Lines | Its specification required UTF-8 and one valid JSON value per line; objects and arrays were allowed. | Start and terminal records could be appended with the same attempt ID. Nested parameters were natural JSON structures, although exact R types and missing values still needed conventions. Updating a prior line or indexing history was application work. The format specified no transaction, locking, or disk-sync guarantee. |
| CSV | `write.table()` supported append; list columns were converted through `as.character()`. `write.csv()` ignored attempts to change its `append` argument. | Flat exported records were convenient; nested parameters needed explicit encoding. Preserving both start and terminal events was possible as separate rows, but transactional updates and concurrent writers required additional machinery. |
| RDS | `saveRDS()`/`readRDS()` documented serialization of one R object, including use of a connection; the named-file API had no append argument. | A complete R history snapshot could preserve nested R structures. Per-event streaming would need a custom connection/serialization protocol or separate files; it was not the ordinary one-object snapshot workflow. |
| Parquet | Arrow documented that Parquet files could not be appended to; additional files in a dataset were the alternative. Parquet documented columnar storage and support for complex data. | Batched history export was plausible. A live start/end ledger required extra files or rewriting/buffering, which added complexity for small individual events. |

Sources: [SQLite transactions](https://www.sqlite.org/lang_transaction.html),
[RSQLite connection](https://rsqlite.r-dbi.org/reference/SQLite.html),
[SQLite storage classes](https://www.sqlite.org/datatype3.html),
[JSON Lines format](https://jsonlines.org/),
[R table output](https://stat.ethz.ch/R-manual/R-devel/library/utils/html/write.table.html),
[R serialization](https://stat.ethz.ch/R-manual/R-devel/library/base/html/readRDS.html),
[Arrow Parquet writer](https://arrow.apache.org/docs/r/reference/write_parquet.html),
and [Parquet overview](https://parquet.apache.org/docs/overview/).

## Error survival and attempt boundaries

SQLite documented that explicit transactions persisted until commit or rollback,
and that some errors could roll back a transaction. The resulting design
inference was to keep audit storage in a separate local database file and
connection, outside the caller's transaction, with short independently committed
writes. A rollback of the target query transaction would then not encompass the
audit transaction. Merely adding another table on the caller's connection would
not provide that separation. [SQLite transactions](https://www.sqlite.org/lang_transaction.html)

A committed start record before invoking the target operation, followed by a
separately persisted terminal record on normal return or an observed error,
could retain the attempt history. This was an event-design inference applicable
to either SQLite or JSON Lines. A start without a terminal record only established
an incomplete observation: it did not prove the driver sent SQL, that the server
completed it, or that the operation failed. An observed driver error also did not
necessarily establish the remote transaction's final outcome.

Because RSQLite issued its own DBI calls, a global DBI observer would need to
exclude its private audit connection and suppress observer re-entry during audit
writes. This was an architectural inference from using DBI for both observation
and storage, not a feature supplied automatically by the storage format.

## SQLite durability and concurrency

RSQLite's documented `dbConnect()` default was `synchronous = "off"`; `dbname =
""` was a temporary file removed on close, and `":memory:"` was temporary memory.
A persistent audit therefore required an explicit path and durability settings.
[RSQLite connection](https://rsqlite.r-dbi.org/reference/SQLite.html)

SQLite documented that WAL with `synchronous=FULL` synchronized each commit.
WAL with `NORMAL` retained consistency but could lose committed transactions
after power loss or a system crash; application crashes were distinguished from
those failures. `OFF` omitted synchronization and could risk corruption after
system failure. Thus a stronger durability requirement favored explicit WAL plus
FULL, at the cost of synchronization on each commit. These were documented
guarantees under SQLite's storage assumptions, not tests of a particular disk.
[SQLite synchronous setting](https://www.sqlite.org/pragma.html#pragma_synchronous)

WAL allowed readers alongside a writer, but still allowed only one writer at a
time, required participants on the same host, and used additional WAL/shared-memory
files. A multi-process design would therefore open a private connection in each
process, keep transactions short, and handle lock contention; WAL did not make
parallel writes unlimited. SQLite provided a busy timeout, not an unconditional
write-success guarantee. Process-separated JSONL files would avoid a shared
writer lock at the cost of merging their histories. These deployment choices
were inferences; neither concurrency nor crash recovery was executed here.
[SQLite WAL](https://www.sqlite.org/wal.html),
[SQLite busy timeout](https://www.sqlite.org/pragma.html#pragma_busy_timeout)

Failure of the audit sink itself was a separate policy question. The investigated
formats did not automatically supply an in-memory retry queue or a visible
unsaved-state API. Ordinary target SQL errors and audit disk/permission/lock
errors therefore needed distinct handling in an implementation.
