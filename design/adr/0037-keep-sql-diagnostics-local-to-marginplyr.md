---
status: accepted
---

# Keep SQL diagnostics local to marginplyr

Accepted on 2026-10-04. marginplyr explains its own internal SQL; it does not
become a connection-wide execution monitor or persistent audit store. This
scope decision supersedes the unimplemented proposal in ADR 0036. It adopts
no new public API or record schema, and does not claim that the diagnostic
extensions below are implemented.

## Need and responsibility

The caller needs to understand the internal work performed while marginplyr
constructs a result, distinguish generated SQL from requests to execute, and
inspect subsequent retrievals such as `collect()` and `pull()`, including
their errors and warnings. These are distinct needs. The maintainer accepted
meeting the first within marginplyr and deferring the broader retrieval need
to a separate evaluation; a construction record does not satisfy that need.

marginplyr knows why its internal queries are needed. That explanation
justifies a package feature even where a driver can log SQL. Later retrieval
may transform the query and run after marginplyr has returned, so its
observation belongs with connection/driver tooling and the caller's R
operation. Organizational audit belongs with database-side auditing and the
organization's retention and access controls. A package diagnostic can be
supporting evidence, not an authoritative organization-wide audit trail.

## Adopted limits

Keep recording opt-in and limited to one construction call's record plus
state needed by active nested calls. Do not accumulate completed calls,
automatically write files, or introduce storage dependencies. This avoids
history growth with session length; it is not a fixed byte limit on a large
individual SQL statement or deeply nested calls. The caller can explicitly
save a record using existing R tools.

Settle #777 and fix #778 under ADR 0027 before expanding diagnostics. Preserve
`last_sent_queries()`, its two-column result, and its activation setting for
that fix. Renaming the accessor, adding fields, and migration are separate
interface decisions. The reentry amendment in ADR 0027 owns the policy and
regression expectations.

A subsequent small extension should distinguish generated SQL, invocation of
an internal backend operation, and the outcome observed there. Its design
must identify the source of a SQL string: a render is not an intercepted DBI
argument. Record only what the observation site establishes; neither an
attempt nor an R error establishes the database's final outcome. Observe at
package-owned calls without changing their result, condition propagation, or
warning settings, and without issuing additional SQL or fetching extra data.

Purpose, SQL/source, observed stage/outcome, condition summaries, and order
are the candidates to specify next. Exact fields, names, compatibility, and
condition-observation coverage remain unresolved. Whole condition objects,
results, connections, and calling environments must not become retained
history. Timestamps, row counts, bound values, source locations, and replay
are not requirements of the minimum extension. Bound values can themselves
contain substantial data; omitting result snapshots is not enough to bound
such a log.

## Alternatives and deferred work

Keeping only generated result SQL would miss the distinctive need to explain
internal work. A combined generated/executed history would instead require
tracking later transformations, repeated execution and binding, partial
fetches, cleanup, and conditions outside DBI. Those costs are disproportionate
to the package-specific need. Defer connection-wide capture, including any
promise to capture every later retrieval or display preview.

No capture mechanism is selected for that deferred work. In particular,
`trace()` providing entry/exit instrumentation does not establish complete
method coverage or safe coexistence and restoration. Driver logging,
connection wrappers, and an explicitly scoped R diagnostic are alternatives
to evaluate against actual backends and workloads. Existing tools have not
been established to satisfy the complete requested combination.

No persistent format is selected. If the separate work later requires
durable history, choose its guarantees and write-failure policy before its
format. Avoid implementing custom locking and recovery merely to eliminate
an optional dependency. SQLite, DuckDB, JSON Lines, CSV, and RDS answer
different storage/use questions; none settles what was observed or whether a
missing terminal record means failure.

Reopen the deferred work when a concrete workflow identifies the required
backends and retrieval paths, acceptable connection setup changes, whether
missing records are tolerable, and whether history must survive the process.
That evaluation is not a prerequisite for #778. Earlier recommendations and
past assent to them are not independent technical evidence.

## Evidence boundary

ADR 0027 and `R/sent-queries.R` describe the current construction record;
ADR 0016 and ADR 0031 delimit the existing result-class exceptions. The
[DBI retrieval flow](https://dbi.r-dbi.org/reference/dbSendQuery.html#the-data-retrieval-flow)
and [binding contract](https://dbi.r-dbi.org/reference/dbBind.html) distinguish
submission, binding, fetching, and cleanup. Installed dbplyr 2.6.0's
`pull.tbl_sql()` selects columns before collection, and its
`db_collect.DBIConnection()` can emit an incomplete-fetch warning after
`dbFetch()` returns. These observations do not establish a universal
interceptor or cross-backend coverage.

The dated [existing-tool investigation](../../investigation/dbi-query-audit-packages-2026-10-01.md)
and [storage investigation](../../investigation/sql-audit-storage-media-2026-10-02.md)
hold their evidence and limitations, not this scope decision. Their related
follow-ups remain historical evidence; the media recommendations in them are
not adopted requirements.
