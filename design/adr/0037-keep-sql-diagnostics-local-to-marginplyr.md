---
status: accepted
---

# Keep SQL diagnostics local to marginplyr

Accepted on 2026-10-04. marginplyr explains its own internal SQL; it does not
become a connection-wide execution monitor or persistent audit store. This
scope decision supersedes the unimplemented proposal in ADR 0036. The original
decision adopted no new public API or record schema; the 2026-10-24 amendment
below selects the focused extension. Neither decision claims implementation.

## Need and responsibility

The 2026-10-04 discussion distinguished internal construction work and generated
SQL from subsequent retrievals such as `collect()` and `pull()`, including
their errors and warnings. The original decision met the first need within
marginplyr and deferred the broader retrieval need to a separate evaluation;
a construction record does not satisfy that broader need. The 2026-10-24
amendment records the maintainer's narrower requirement for this task.

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

The reentry work for #777 and #778 preserved `last_sent_queries()`, its
two-column result, and its activation setting under ADR 0027. That ADR's
reentry amendment still owns the policy and regression expectations. The
2026-10-24 amendment below settles the separate diagnostic interface extension.

A subsequent small extension should distinguish generated SQL, invocation of
an internal backend operation, and the outcome observed there. Its design
must identify the source of a SQL string: a render is not an intercepted DBI
argument. Record only what the observation site establishes; neither an
attempt nor an R error establishes the database's final outcome. Observe at
package-owned calls without changing their result, condition propagation, or
warning settings, and without issuing additional SQL or fetching extra data.

The 2026-10-24 amendment resolves the previously open interface, fields, and
observation boundary through its specification. Whole condition objects,
results, connections, and calling environments must not become retained
history. Timestamps, row counts, general bound-value capture, source locations,
and replay are not requirements of this extension. SQL and diagnostic text can
themselves be large; omitting result snapshots does not impose a byte limit.

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

## Amendment: inspect internal queries on demand (2026-10-24)

The maintainer narrowed this task to auxiliary SQL completed inside public
Margin construction and Grouping-plan inspection: label-collision counts,
zero-row metadata, dialect questions, and their controls. Later retrieval,
connection-wide recording, and tracking derived results were considered and
removed from this task by that explicit requirement change. This is not an
inference from the earlier ADR or agreement with a past recommendation.

Retain the existing option, latest-call lifetime, active-parent restoration,
and two-column default of `last_sent_queries()`. Adopt an `include_result`
argument for selecting internal rows and a `details` argument for viewing the
SQL source, owned-operation status, and condition summaries. Both are view
choices over information recorded when the existing audit option is enabled;
neither reruns queries or reconstructs earlier calls. The
[internal SQL specification](../specs/sql-audit.md) owns defaults, field meanings,
empty/error cases, observation sites, and acceptance scenarios. This extension
is accepted but unimplemented.

Keep one call's own record in memory and let the caller copy or export it.
This replaces the automatic SQLite history previously accepted during the
broader discussion. Inspecting auxiliary work needs neither a durable store
nor a driver-wide interceptor. Preserving the existing API avoids a second
activation mechanism and keeps existing consumers of the two-column result
compatible. A SQL-only record was considered insufficient for distinguishing
a normally returned check, an expected probe error, and an unobserved outcome;
the detailed view supplies that distinction at the owned boundary.

The record describes preparation and the observed internal operation, not
database receipt, server completion, or success of the entire public call.
A label-count query can return normally before validation rejects a collision.
Retain errors and warnings observed inside existing probe handling without
changing their propagation or suppression. No extra SQL, automatic EXPLAIN,
extra fetch, result snapshot, or replay is part of this decision.

The [2026-10-13 recording investigation](../../investigation/sql-audit-recording-2026-10-13.md)
establishes the existing recording sites, missing outcomes, and documentation
overstatements. Broader capture investigations remain evidence for their tested
mechanisms, not obligations of this smaller feature. Acceptance of the design
does not establish that its condition-observation paths have been implemented
or verified.
