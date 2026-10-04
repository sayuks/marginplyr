---
status: superseded by ADR-0037
---

# Accumulate recorded SQL across calls

## Disposition (2026-10-04)

This proposal was not adopted or implemented. The maintainer accepted the
narrower scope in [ADR 0037](0037-keep-sql-diagnostics-local-to-marginplyr.md)
after reconsidering the need for the feature independently of earlier
recommendations. The text below preserves the proposal as evaluated; its
requirements and candidate APIs are not the current decision.

Accumulation does not resolve attribution or configuration isolation under
reentry. ADR 0027's *Amendment: restore the active call's record under reentry*
settles #777 separately; #778 remains the implementation work.

## Purpose and status

The primary purpose is to let callers inspect marginplyr's internal SQL and
the SQL they subsequently execute to retrieve its results. The requested
scope also includes ordinary DBI queries on audited connections, even when no
Margin verb is used. This proposal describes client-side observation, not a
replacement for a database server's organizational audit system.

Issue #777 concerns which construction call owns the last record under
reentry. Accumulation changes that contract; later retrieval, execution
outcomes, and persistent storage are additional requirements beyond that
issue. This ADR remains proposed: the observation mechanism, storage medium,
and outstanding policies below have not been adopted or implemented.

## Accumulation and activation

The SQL record accumulates across sequential and reentrant calls rather than
being replaced at each call. This removes the need to choose which overlapping
call owns the readable record (#777), revisiting ADR 0027's rejection of
accumulation in *One call, and which call* and *Considered options*.

A query is recorded immediately before its execution is invoked, so execution
failure leaves the recorded row readable. A result query returned unexecuted
is also recorded, immediately before it is returned to the caller. This keeps
ADR 0027's distinction between result SQL and execution while extending the
record across calls.

`get_sql_log()` returns the accumulated record as a tibble without consuming
it. An empty record returns an empty tibble. Its name includes generated SQL
that has not been sent. The earlier in-process-only reader and `clear = TRUE`
semantics need revisiting for persistent storage without a full memory copy.
Recording remains opt-in.
Disabling recording stops new entries but does not prevent reading or explicitly
clearing existing entries.

Starting recording requires one user-facing control. Requiring both a dedicated
connection wrapper and the option is rejected as redundant; ordinary
`DBI::dbConnect()` usage should remain intact. Recording can target all DBI
connections in the process or selected connections. A control function can
accept multiple connections as separate arguments or as a list, including a
list stored in a variable and passed as an ordinary R value. An option is a
startup default, and `audit_sql()` controls recording during execution; neither
requires the other as a second activation step. The mechanism for observing
DBI execution remains unverified.

## Execution and retrieval

The record also distinguishes unexecuted SQL from execution requests and their
success or failure, including later execution of a returned result. Execution
requests and successful completion are separate facts; calling `collect()`
alone establishes neither submission to the database nor successful execution.
Every execution attempt has its own history entry rather than overwriting
previous attempts. Generated-query linkage is retained where it can be
established; ordinary DBI statements need no preceding marginplyr construction
record. Failed attempts retain their diagnostic. How to link later transformed
SQL to a construction record remains unresolved.

Recording also covers operations that retrieve database results into R,
including `collect()` and `pull()`. A construction record alone does not
satisfy this requirement: later retrieval must record the SQL actually
submitted and the observed execution and retrieval outcome. Coverage must
not depend on callers using a new retrieval function. Actual submitted SQL is
recorded even when downstream operations change the original query. Including
display-preview retrieval is recommended, but not yet adopted. The mechanism
and its coverage across retrieval entry points remain unverified.

The observation sites include statement submission, parameter binding,
retrieval, and cleanup, including applicable Arrow equivalents. A result
identifier associates subsequent fetches with the statement that produced it.
The candidate for preserving ordinary connections is R's tracing mechanism;
tracing has not been selected as the production implementation. Method
coverage, exit-handler behavior, other tracing users, and restoration must be
verified. A DBI invocation records a client request; a failed invocation does
not establish whether the server received or executed the statement. Retrieval
completion does not establish transaction commit.

## Recorded information and conditions

Each execution attempt records its start and end times, elapsed duration,
actual bound parameters, and a connection identifier. These identify when,
where, and with which values a statement was requested.

Source filenames, line numbers, calling paths, and console-origin metadata
are excluded from the record.

The record includes fetched-row count, retrieval completion, and affected-row
count as separate observations. Unknown or inapplicable observations remain
missing, distinct from known zero. The auditor issues no additional SQL or
fetch solely to obtain these observations. Each attempt also records its DBI
entry point, warnings, and requested fetch limit where applicable.
Warnings retain their diagnostic separately from the success or failure of an
attempt. Observing them does not suppress their normal delivery or change the
caller's warning-to-error setting.

Failures also identify the observed phase: execution request, parameter
binding, result retrieval, or result cleanup. This distinguishes a request
failure from an error while fetching or freeing its result.
SQL errors are recorded before propagation and do not disable recording.
Subsequent statements the caller executes remain audited; this does not
suppress the original error or automatically continue an aborted script.

## Persistence

Specifying a destination enables automatic persistent recording. CSV was
selected for this initially; the medium is reopened over transactional writes.
Storage choices prioritize avoiding additional package installation. This
revisits ADR 0027's rejection of an on-disk log; the write-failure policy and
file representation remain unresolved.

Persistent recording must not retain a full duplicate history in memory.
An unbounded in-memory retry queue after storage failure is rejected because
it would put pressure on memory. The default store when no destination is
given and reading a selected range of disk history remain to be settled.

Result data are not duplicated into the log. Observations such as row counts
use results the caller already requested. Storage recommendations remain
conditional: optional SQLite is the candidate for transactional persistence;
choosing it would require accepting its runtime dependency when recording is
used. CSV, JSON Lines, and DuckDB have not been selected as the replacement.

## Delivery order and open decisions

First validate interception with isolated probes against the existing SQLite
and DuckDB backends. Cover collection, pull, downstream query changes,
direct DBI calls, repeated binds, partial retrieval, errors, warnings, and
start/stop with selected connections. Check that observation adds no SQL or
fetch and preserves results and conditions. Publish a supported-path matrix
before promising coverage beyond the measured methods.

After that evidence, settle the persistence medium, destination default,
write-failure policy, bounded history reader, explicit clearing and stopping
interfaces, generated-query linkage, preview inclusion, and migration from
`last_sent_queries()`. Storage failure must not silently present a complete
record, and recovery must not accumulate an unbounded memory queue.

Implement the public feature only after the remaining decisions are accepted.
Runnable replay, offline response fixtures, a live console sink, and a scoped
capture API are deferred.

The observation boundary is SQL submitted from R through DBI, together with
unexecuted SQL generated by marginplyr. SQL added inside a driver or executed
inside the database, such as trigger statements, is outside this record.
Construction-only recording is insufficient for this requirement. The
representation and observation mechanism remain to be settled before this
proposal is adopted.
