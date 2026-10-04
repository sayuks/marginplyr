# Existing R packages for recording DBI queries

Investigated: 2026-10-01

## Question and scope

This investigation asked whether an existing package could record every SQL
execution through ordinary DBI connections, including direct `dbGetQuery()`,
dbplyr collection/materialization, failures, and repeated parameter binding.
The desired interface also included all-connections activation, selection of
one or several existing connections, and generated-but-unexecuted SQL.

The search examined dblog, dittodb, DBI's OpenTelemetry integration, otel's
automatic instrumentation, DatabaseConnector, and connector. It did not
establish that no other package existed. Documentation and source readings
were distinguished below from executed SQLite probes. A normally returning R
call established that the call returned; it did not establish transaction
commit. An error did not establish that the database executed nothing.

## Findings

Existing logging packages were found, but none of the examined interfaces
provided the requested combination without adaptation.

| Candidate | Activation observed | SQL and execution evidence observed | Gap against the requested interface |
| --- | --- | --- | --- |
| dblog | Wrap a driver before `dbConnect()`; use a console, file, or collecting logger | Logged supported DBI calls, SQL arguments, repeated `dbBind()` calls, and normal/failed return indication | Changed connection construction; no public all-existing-connections activation or list selection; no error condition text in logger callback; delegated backend internals were opaque |
| dittodb | `start_db_capturing()` / `stop_db_capturing()` around ordinary connection construction | Saved responses as fixtures keyed by cleaned SQL hash | Existing connection creation had to occur after capture started; repeated SQL overwrote fixtures; no append-only failure/outcome history or selected-connections argument |
| DBI + otel/otelsdk | Global telemetry configuration, including environment variables; ordinary connections | Spans for selected high-level DBI operations with timing and R-level status | No full SQL text or individual connection identity in DBI spans; direct send, execute, bind, and fetch entry points had no built-in hooks |
| otel automatic instrumentation | Environment variable selecting namespaces/functions | Function-name spans | Did not record SQL arguments or establish per-query outcomes |
| DatabaseConnector + ParallelLogger | `options(LOG_DATABASECONNECTOR_SQL = TRUE)` plus a configured logger | Its query/execute helpers logged SQL before the call and timing after return; errors produced separate reports | Logging was in its helpers, not a global hook on arbitrary DBI connections; SQL log text was truncated to 150 characters by default |
| connector | `connector.logging` option/environment variable when creating its connector collection | Logged connector operations, table/resource names, driver and database | Recorded `read_cnt()`/`write_cnt()` etc.; did not intercept direct DBI SQL or subsequent dbplyr execution |

No examined integration recorded an arbitrary SQL string merely because a lazy
query had been generated. Generation would need a separate observation point.
The source boundaries for each row follow.

## dblog

The [README at the inspected commit][dblog-readme] described driver wrapping
and demonstrated dbplyr use. `dblog(RSQLite::SQLite(), logger = logger)`
created the logging driver; `DBI::dbConnect()` returned a subclass wrapping the
native connection. The [exported driver API][dblog-driver] accepted a driver
or connector, not an already-created connection or connection list. Its
[namespace][dblog-namespace] exposed no start/stop or global activation API.

The [connection methods][dblog-connection] delegated calls such as
`dbGetQuery(conn@conn, ...)` and `dbWriteTable(conn@conn, ...)` to the original
connection. Consequently the logger saw that high-level call, rather than every
statement or driver retry inside its implementation. The [result methods][dblog-result]
explicitly logged `dbBind()`, `dbFetch()`, and `dbClearResult()` on wrapped
results; the inspected methods did not provide Arrow equivalents.

[The logging implementation][dblog-logger] registered `on.exit()` before
evaluation and logged after the call ended. The callback received the call and
`withVisible()` result, or `NULL` when evaluation did not return normally.
It did not receive the caught condition, its message, or its class. The text
logger rendered the latter case as `try(<call>)`. This distinguished a failed
R call in the log without recording its actual diagnostic. It did not record
an in-flight execution before control returned.

The README claimed a CRAN installation route. A retrieval of the CRAN source
repository index using `available.packages(repos = "https://cran.r-project.org",
filters = list())` on the investigation date did not contain dblog, while it
listed dittodb 0.1.11. The inspected dblog source was development version
0.0.0.9050. Its [r-dbi r-universe page][dblog-universe] advertised an installable
development build. The README's CRAN sentence was therefore not used as
evidence of a released CRAN package.

## dittodb

[The capture documentation][dittodb-docs] explicitly required `dbConnect()`
inside the capturing interval. [The source][dittodb-capture] traced DBI
generics and selected backend methods, set a shared fixture path on connection
creation, and selected response paths from statement type and SQL hash.
`start_db_capturing(path, redact_columns)` had no connection-selection argument.
Its trace list contained neither `dbBind()` nor `dbSendStatement()`.

`recordFetch` wrote the response with `dput()` to the selected path. It did not
append timestamped execution events; successive fetches of the same hashed SQL
could replace the same file. `dbSendQueryTrace` chose a path rather than writing
a SQL/error event. The source provided no per-attempt condition/outcome ledger.
These were source conclusions, strengthened by the probes below; the
documentation's phrase about recording all queries was not treated as a
guarantee of full audit coverage.

## DBI OpenTelemetry and automatic instrumentation

The inspected [DBI query helper][dbi-otel] attached `db.operation.name`,
`db.collection.name`, and `db.system.name`. The last was derived from the
object class's package. It did not attach the statement or an individual
connection identifier. [The generic `dbGetQuery()`][dbi-getquery] started this
span before dispatch; [the generic `dbExecute()`][dbi-execute] did not.

A source search across DBI's `R/` found no OTel hook in `dbSendQuery()`,
`dbSendStatement()`, `dbBind()`, or `dbFetch()`. These lower-level entry points
were relevant to dbplyr and prepared statements. [DBI's own OTel test][dbi-test]
counted a parameterized `dbGetQuery(..., params = list(1:8))` as one span, not
eight separately identified executions. This ruled out treating built-in
spans as a complete statement/execution ledger from source alone.

[otel's configuration documentation][otel-env] offered global environment
configuration and instrumentation-scope filters. Scope meant a package or
tracer, not an individual `con`. [Automatic tracing source][otel-rtrace]
started a span named after a function and ended it on exit; it did not extract
SQL arguments. [The load hook][otel-onload] read
`OTEL_R_INSTRUMENT_PKGS`. The reference page for zero-code instrumentation
spelled this variable differently, so the inspected executable source was the
authority for that spelling.

[The OTel status documentation][otel-status] described automatic `ok`/`error`
status for spans ended through its local-active-span mechanism. That was an
R-scope status, not a database commit guarantee. [otelsdk's recording API][otelsdk-record]
returned recorded spans and metrics and described memory providers for tests.
The DBI integration was read from source and its upstream test; it was not
executed in this investigation.

## DatabaseConnector and connector

[DatabaseConnector's querying guide][databaseconnector-docs] described an
option enabling SQL trace logs through ParallelLogger. In [its SQL helpers][databaseconnector-sql],
`querySql()` and `executeSql()` logged before calling DBI, measured elapsed time
after normal return, and wrote failures to an error-report file. [The shared
helper][databaseconnector-helper] checked the option, required ParallelLogger
to be installed, and truncated SQL log text with `maxLength = 150`.

[Its DBI implementations][databaseconnector-dbi] were methods for its own
connection classes. The inspected `dbGetQuery()` and `dbSendQuery()` methods
did not call the SQL logging helper; they delegated to the underlying
connection or low-level implementation. Therefore enabling that option did
not install a session-wide hook recording direct DBI operations on an
unmodified connection. No executable check of this package was performed.

[connector's option definitions][connector-options] offered a global
`connector.logging` setting. [Its `connect()` implementation][connector-connect]
applied `add_logs()` when that setting was enabled. [The logging implementation][connector-logs]
dispatched on `ConnectorLogger` for its own operations and called its logger
after `NextMethod()` returned. [The DBI log messages][connector-dbi-logs]
contained the resource name, driver class, and database name, not SQL text.
[Its DBI connector][connector-dbi] retained an ordinary underlying `conn`;
accessing that connection directly bypassed those S3 logging methods. These
were source readings, not executed probes.

## Executable verification

Fresh `Rscript` processes were run on local in-memory SQLite with
DBI 1.3.0, RSQLite 3.53.3, and dbplyr 2.6.0. It installed only temporary copies
of dblog 0.0.0.9050 and dittodb 0.1.11.9000 from the inspected commits. It then
downloaded dittodb 0.1.11 from the CRAN source repository, installed it in a
separate temporary library, and repeated the new- and existing-connection
probes. The released version produced the same observations as the development
version in those probes.

The harness and raw logs were retained for the session at
`/private/tmp/marginplyr-dbi-audit-2026-10-01-KwHxLG/probe.R` and neighboring
`dblog.log`, `dittodb-new.log`, `dittodb-existing.log`, and `*-results.rds`.
The released dittodb logs/results were in the neighboring `release/`
directory. Those paths were temporary evidence, not durable repository
dependencies.

| Probe | dblog observed | dittodb observed with connection made after capture started |
| --- | --- | --- |
| Direct `SELECT` | One `dbGetQuery` event, normal result | One SELECT response fixture |
| Missing-table `SELECT` | One event with `result = NULL`; diagnostic absent from callback | Original SQLite error; no new fixture |
| `CREATE` / `INSERT` using `dbExecute()` | Send-statement, rows-affected, clear-result events | Rows-affected fixture only; no execution/outcome ledger |
| `dbWriteTable()` | One high-level call with the data-frame argument; no generated CREATE/INSERT SQL entries | Not probed as a separate case |
| dbplyr `collect()` | SQL send and fetch lifecycle recorded | Metadata and result response fixtures |
| dbplyr `compute(..., analyze = FALSE)` | Temporary-table creation SQL recorded | Rows-affected fixture; no creation SQL/outcome ledger |
| Prepared SQL with two `dbBind()` calls | Both binding calls and parameters recorded separately | One response fixture for the prepared SQL |
| Same `SELECT random()` twice | Not probed | One fixture path, not two events |

The dblog `dbWriteTable()` probe returned normally but produced exactly one
high-level log event. It contained the table name and data-frame argument, not
the CREATE/INSERT statements generated inside RSQLite. This supplied executed
evidence that logging supported DBI calls was not equivalent to capturing
every SQL statement a driver sent.

For both repeated parameter binding and repeated random SELECT, the
released-version fixture was read with `dget()` and its content was verified to
equal the second response and differ from the first. This established
replacement of the earlier response in those cases.

For the existing-connection case, a fresh process created its SQLite connection
before `start_db_capturing()`. A subsequent ordinary SELECT raised dittodb's
`Database capture failed` diagnostic saying the connection had been created
before capture started. No fixture was written. The diagnostic arose from
capture infrastructure; it was not evidence that SQLite had executed nothing.

These small reproductions expressed the decisive probes:

```r
logger <- dblog::make_collect_logger()
con <- DBI::dbConnect(dblog::dblog(RSQLite::SQLite(), logger = logger),
                      dbname = ":memory:")
try(DBI::dbGetQuery(con, "SELECT * FROM audit_missing"))
logger$retrieve()  # The failed call was rendered as try(...).
DBI::dbDisconnect(con)
```

```r
# Run in a fresh process to avoid leftover capture state.
library(dittodb)
library(DBI)
path <- tempfile()
dir.create(path)
start_db_capturing(path)
con <- dbConnect(RSQLite::SQLite(), dbname = ":memory:")
dbGetQuery(con, "SELECT random() AS repeat_probe")
dbGetQuery(con, "SELECT random() AS repeat_probe")
stop_db_capturing()
dbDisconnect(con)
list.files(path, recursive = TRUE)  # One SELECT response file was observed.
```

The probes did not cover every DBI backend, Arrow entry point, cancellation,
transport failure, transaction rollback, or driver-internal statement. Their
results were evidence for these SQLite calls and package versions only.

## Inspected source revisions

| Repository | Version read | Pinned commit |
| --- | --- | --- |
| r-dbi/dblog | 0.0.0.9050 | `ade9249447af9a05a5c404c7b2d5e49d00efc24c` |
| ropensci/dittodb | 0.1.11.9000 | `a9bcdfe0ab5f19b08c649923ae9aa83cb25d48df` |
| r-dbi/DBI | 1.3.0.9014 | `2481a2204602232c3804d3e8f8520170164c4294` |
| r-lib/otel | 0.2.0.9000 | `288c44c30f90c9973f0b49ae418e2d5df98b3953` |
| r-lib/otelsdk | 0.2.4.9000 | `0cf2ad93631944d28142e941ae9ac5cab107f478` |
| OHDSI/DatabaseConnector | 7.2.0 | `128c84fa4f2fa254667dd17dc30210f7c1420a63` |
| NovoNordisk-OpenSource/connector | 1.0.0.9004 | `93620e6bca65325c80655192b04d9be082e9cf0e` |

[dblog-readme]: https://github.com/r-dbi/dblog/blob/ade9249447af9a05a5c404c7b2d5e49d00efc24c/README.md
[dblog-driver]: https://github.com/r-dbi/dblog/blob/ade9249447af9a05a5c404c7b2d5e49d00efc24c/R/Driver.R
[dblog-namespace]: https://github.com/r-dbi/dblog/blob/ade9249447af9a05a5c404c7b2d5e49d00efc24c/NAMESPACE
[dblog-connection]: https://github.com/r-dbi/dblog/blob/ade9249447af9a05a5c404c7b2d5e49d00efc24c/R/Connection.R
[dblog-result]: https://github.com/r-dbi/dblog/blob/ade9249447af9a05a5c404c7b2d5e49d00efc24c/R/Result.R
[dblog-logger]: https://github.com/r-dbi/dblog/blob/ade9249447af9a05a5c404c7b2d5e49d00efc24c/R/make-log-call.R
[dblog-universe]: https://r-dbi.r-universe.dev/dblog
[dittodb-docs]: https://dittodb.jonkeane.com/reference/capture_requests.html
[dittodb-capture]: https://github.com/ropensci/dittodb/blob/a9bcdfe0ab5f19b08c649923ae9aa83cb25d48df/R/capture-requests.R
[dbi-otel]: https://github.com/r-dbi/DBI/blob/2481a2204602232c3804d3e8f8520170164c4294/R/otel.R
[dbi-getquery]: https://github.com/r-dbi/DBI/blob/2481a2204602232c3804d3e8f8520170164c4294/R/dbGetQuery.R
[dbi-execute]: https://github.com/r-dbi/DBI/blob/2481a2204602232c3804d3e8f8520170164c4294/R/dbExecute.R
[dbi-test]: https://github.com/r-dbi/DBI/blob/2481a2204602232c3804d3e8f8520170164c4294/tests/testthat/test-otel.R
[otel-env]: https://otel.r-lib.org/reference/environmentvariables.html
[otel-status]: https://otel.r-lib.org/reference/tracing-constants.html
[otel-rtrace]: https://github.com/r-lib/otel/blob/288c44c30f90c9973f0b49ae418e2d5df98b3953/R/rtrace.R
[otel-onload]: https://github.com/r-lib/otel/blob/288c44c30f90c9973f0b49ae418e2d5df98b3953/R/onload.R
[otelsdk-record]: https://otelsdk.r-lib.org/reference/with_otel_record.html
[databaseconnector-docs]: https://ohdsi.github.io/DatabaseConnector/articles/Querying.html
[databaseconnector-sql]: https://github.com/OHDSI/DatabaseConnector/blob/128c84fa4f2fa254667dd17dc30210f7c1420a63/R/Sql.R
[databaseconnector-helper]: https://github.com/OHDSI/DatabaseConnector/blob/128c84fa4f2fa254667dd17dc30210f7c1420a63/R/HelperFunctions.R
[databaseconnector-dbi]: https://github.com/OHDSI/DatabaseConnector/blob/128c84fa4f2fa254667dd17dc30210f7c1420a63/R/DBI.R
[connector-options]: https://github.com/NovoNordisk-OpenSource/connector/blob/93620e6bca65325c80655192b04d9be082e9cf0e/R/connector-options.R
[connector-connect]: https://github.com/NovoNordisk-OpenSource/connector/blob/93620e6bca65325c80655192b04d9be082e9cf0e/R/connect.R
[connector-logs]: https://github.com/NovoNordisk-OpenSource/connector/blob/93620e6bca65325c80655192b04d9be082e9cf0e/R/logger_generics.R
[connector-dbi-logs]: https://github.com/NovoNordisk-OpenSource/connector/blob/93620e6bca65325c80655192b04d9be082e9cf0e/R/logger_log_dbi.R
[connector-dbi]: https://github.com/NovoNordisk-OpenSource/connector/blob/93620e6bca65325c80655192b04d9be082e9cf0e/R/dbi.R
