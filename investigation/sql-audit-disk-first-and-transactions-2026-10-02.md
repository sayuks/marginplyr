# Disk-first SQL history and transaction boundaries

Investigated: 2026-10-02

## Evidence and scope

This follow-up investigated avoiding an accumulated in-memory history while
requiring reliable persistent events. It introduced no implementation or new
experiment. The earlier [storage comparison](sql-audit-storage-media-2026-10-02.md)
and [standard-R probe](sql-audit-storage-without-extra-packages-2026-10-02.md)
remained unchanged. The design possibilities below were inferences from those
observations and the official interfaces, not adopted decisions.

CSV append through `write.table()` documented adding output to a file, but
supplied no commit/rollback interface or transactional append guarantee. The
earlier probe also found that an incomplete multiline tail disturbed ordinary
CSV parsing. Plain append therefore did not meet the same transactional contract
as SQLite. [R table output](https://stat.ethz.ch/R-manual/R-devel/library/utils/html/write.table.html)

Writing a replacement file and renaming it addressed replacement of a whole
snapshot, not transactional append of an event. R documented OS-dependent
restrictions on `file.rename()`, required checking its return value, and only
called within-directory renaming portable. That interface did not establish a
power-loss durability guarantee. Adding locking, journals, recovery, and disk
synchronization to standard-R CSV would amount to a substantially more complex
storage implementation. [R file operations](https://stat.ethz.ch/R-manual/R-devel/library/base/html/files.html)

## Disk-first design possibilities

A disk-first sink could persist each event as it arose and retain only active
operation metadata in R, rather than queue the whole history. A retrieval
accessor could load the requested range on demand; reading all records would
still allocate the returned history. SQLite could filter in its query, whereas
CSV range retrieval would need streaming/parsing or an index. Neither medium
implied zero memory use: one event, serialization buffers, and storage-library
caches still occupied memory. This was a resource-management design inference.

For independent SQLite auditing, the inferred sequence was:

1. Commit a start event on a private connection to a separate audit database.
2. Invoke the target operation on the caller's connection.
3. Commit a normal-return or observed-error event on the audit connection.

SQLite documented independent connections, transactions, and explicit commit/
rollback. Keeping the audit outside the target transaction avoided rolling back
audit history with target data. A global DBI observer would also need to exclude
its own audit connection and prevent observer re-entry. A start without an end
remained an incomplete observation, not proof of a remote failure or completion.
These steps did not create an atomic transaction spanning target execution and
audit persistence. [SQLite transactions](https://www.sqlite.org/lang_transaction.html)

## Failure-policy tradeoff

An ordinary target SQL error and an audit-storage error were different events.
Continuing after a target error was compatible with persisting its error record.
Disk full, lock, or I/O failure at the audit sink could prevent that persistence;
SQLite explicitly documented such failure conditions.
[SQLite transaction error handling](https://www.sqlite.org/lang_transaction.html)

Without an accumulated retry queue, complete history could not be promised while
arbitrarily continuing new executions during a persistent sink failure. The
remaining choices were to stop new observed executions until a usable sink was
restored/switched, or to continue with an explicit recording gap. If the start
write failed, the observer could prevent the target invocation; if the end write
failed after execution, stopping later calls could not undo that execution or
recover its missing terminal record automatically. This was a policy choice,
not a storage-format feature.

The inspected `DESCRIPTION` already listed RSQLite in Suggests, so requiring it
for a selected transactional sink could remain an optional-feature requirement.
Invoking the SQLite command-line shell instead would require an external
`sqlite3` executable rather than eliminate dependencies. No availability of that
executable on users' machines was established.
[SQLite command-line shell](https://www.sqlite.org/cli.html)
