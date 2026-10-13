# Existing audit features absent from the proposed SQL log

Investigated: 2026-10-01

## Comparison baseline

The comparison used the discussion recorded in proposed ADR 0036 as read on
this date: `sql_log()` included generated SQL and accumulated execution history,
linked query/attempt identifiers, execution states and failure diagnostics,
explicit clearing, all/selected connections, option startup defaults, and
`audit_sql()` accepting separate connections or a list. These were already in
the proposal, not missing features. They were proposed behavior, not evidence
of an implemented interceptor. This note established evidence for possible
extensions; it adopted none of them.

The source revisions below were those examined in
`investigation/dbi-query-audit-packages-2026-10-01.md`. This follow-up performed
source/documentation reads only; it ran no new package installation or probe.
The earlier note recorded the SQLite checks used for dblog's bind evidence.

## Features the existing sources supplied

| Feature not yet specified in the proposal | Primary-source evidence read | What it added and its boundary |
| --- | --- | --- |
| Execution start time, end time, elapsed duration | [otelsdk span conversion][otel-data] exposed `start_time` and `duration`; [DatabaseConnector SQL helpers][dc-sql] measured `Sys.time()` around the DBI call | Enabled ordering against wall time and diagnosing slow operations. OTel's end could be derived from start plus duration; DatabaseConnector logged elapsed time after normal return. Neither established transaction commit or engine-only execution time. |
| Actual parameters for each prepared execution | [dblog result methods][dblog-result] passed `params` into the logged `dbBind()` call; the earlier SQLite probe retained `list(3L)` and `list(4L)` separately | Distinguished repeated executions of identical placeholder SQL. Per-attempt history alone did not specify retaining bound values. Interpolating values into SQL was not required by this evidence. |
| Connection and result identifiers in each record | [dblog object naming][dblog-logger] assigned `conn1`, `res1`, etc. and reused them in calls | Linked send/bind/fetch/clear operations and separated connections. These session-local object names differed from the already-proposed query/attempt IDs; no durable database-session identity was established. |
| Console/file output and pluggable logger | [dblog logger source][dblog-logger] supplied exported `make_text_logger(path)` and `make_collect_logger(display)`; [its public driver API][dblog-driver] accepted `logger` | Added live display or persistence beyond an accessor's in-memory result. The earlier custom callback probe used private `dblog:::make_logger()`, so it was not evidence of a polished public callback constructor. |
| Runnable R replay | [dblog README][dblog-readme] demonstrated evaluating its collected R-code log in a fresh session | Recorded DBI calls and their arguments for reproduction, rather than SQL text alone. Driver/environment availability and backend operations still mattered; replay was not a promise of identical external database state. |
| Scoped capture with automatic stop | [dittodb capture implementation][dittodb-capture] used `on.exit(stop_db_capturing())` in `capture_db_requests(expr, ...)` | Limited observation to an expression and stopped it when evaluation exited, including failure. It did not restore an outer capture state; a future scoped marginplyr API's restoration semantics remained unspecified. |
| Offline fixtures and SQL assertions | [dittodb mock implementation][dittodb-mock] replaced database access with stored responses; [its `expect_sql()` source][dittodb-expect] asserted an intercepted SQL diagnostic | Supported tests without a live database. `expect_sql()` was experimental and documented support for `dbSendQuery()` and implementations of `dbGetQuery()` that used it, not every DBI entry point. This was a separate test/replay capability, not a missing status column. |
| Redaction of optional result snapshots | [dittodb capture source][dittodb-capture] passed fetched data through [column redactors][dittodb-redact] before writing fixtures | Reduced retained response data. It redacted response data-frame columns, not SQL literals or bound parameters. Without storing result payloads this feature had no direct role in the proposed log; SQL/parameter redaction would need separate evidence/design. |
| Correlation with a wider trace and external export | [otel span documentation][otel-parent-source] described the active parent span; [otelsdk span data][otel-data] exposed trace/span/parent IDs; [file][otel-file] and [HTTP][otel-http] providers exported spans | Related DB work to a request/job and enabled outside telemetry systems. Query/attempt linkage alone did not provide this relationship. Built-in DBI spans' incomplete SQL coverage remained as established in the earlier note. |

The main omissions relevant to an execution log were time/duration, actual bind
parameters, visible connection/result IDs, output destinations, and scoped
lifetime. Offline mocks, replay, result snapshots, and distributed tracing
were additional capabilities with their own scope. A source feature's
existence did not decide whether marginplyr needed it.

## Sources and verification limits

All features in the table were source/documentation observations on the
investigation date. Only the dblog parameter examples referred to the earlier
executed SQLite probe. No new check established cross-backend behavior,
redaction of SQL/parameters, database-side completion, or automatic capture of
every driver's internal SQL. The design comparison meant "not specified in
the read proposal," not "impossible" or "already rejected."

[dblog-result]: https://github.com/r-dbi/dblog/blob/ade9249447af9a05a5c404c7b2d5e49d00efc24c/R/Result.R
[dblog-logger]: https://github.com/r-dbi/dblog/blob/ade9249447af9a05a5c404c7b2d5e49d00efc24c/R/make-log-call.R
[dblog-driver]: https://github.com/r-dbi/dblog/blob/ade9249447af9a05a5c404c7b2d5e49d00efc24c/R/Driver.R
[dblog-readme]: https://github.com/r-dbi/dblog/blob/ade9249447af9a05a5c404c7b2d5e49d00efc24c/README.md
[dittodb-capture]: https://github.com/ropensci/dittodb/blob/a9bcdfe0ab5f19b08c649923ae9aa83cb25d48df/R/capture-requests.R
[dittodb-mock]: https://github.com/ropensci/dittodb/blob/a9bcdfe0ab5f19b08c649923ae9aa83cb25d48df/R/mock-db.R
[dittodb-expect]: https://github.com/ropensci/dittodb/blob/a9bcdfe0ab5f19b08c649923ae9aa83cb25d48df/R/expect-sql.R
[dittodb-redact]: https://github.com/ropensci/dittodb/blob/a9bcdfe0ab5f19b08c649923ae9aa83cb25d48df/R/redact.R
[dc-sql]: https://github.com/OHDSI/DatabaseConnector/blob/128c84fa4f2fa254667dd17dc30210f7c1420a63/R/Sql.R
[otel-data]: https://github.com/r-lib/otelsdk/blob/0cf2ad93631944d28142e941ae9ac5cab107f478/src/tracer.c
[otel-parent-source]: https://github.com/r-lib/otel/blob/288c44c30f90c9973f0b49ae418e2d5df98b3953/R/tracer-provider-noop.R
[otel-file]: https://github.com/r-lib/otelsdk/blob/0cf2ad93631944d28142e941ae9ac5cab107f478/R/tracer-provider-file.R
[otel-http]: https://github.com/r-lib/otelsdk/blob/0cf2ad93631944d28142e941ae9ac5cab107f478/R/tracer-provider-http.R
