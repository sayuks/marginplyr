# Superseded broad SQL-audit discussion

Archived: 2026-10-24.

The maintainer subsequently narrowed this task to auxiliary SQL completed
inside public marginplyr construction/inspection calls. The text below preserves
the earlier discussion, including past agreements and unanswered proposals; it
is not the active specification. See [the current specification](sql-audit.md).
Earlier agreements are not technical evidence or automatic requirements for
the narrower feature.

---

# SQL audit scope and recording design

Status: discussion draft. Connection-wide coverage is reopened for consideration
of public-operation-related SQL only. SQLite persistence, initial driver set,
recording-failure behavior, connection interface, parameter retention, and
process-local capture have prior agreement; their scope-dependent details need
rechecking if coverage changes. The complete specification, API, and
implementation remain undecided.
Started: 2026-10-13. Decisions updated: 2026-10-24.
Product implementation is outside this discussion's scope.

## User requirement and decision boundary

The caller wants to inspect what marginplyr APIs do internally and which SQL
retrieves their results, without silently losing relevant work. The discussion
must cover later `collect()` / `pull()`, repeated and partial retrieval,
success, errors, warnings, and the existing public documentation.

The agreed future scope conflicts with ADR 0037's construction-only boundary,
exclusion of accumulated records and automatic persistence, and deferral of
later retrieval. A replacement decision will be finalized with the complete
specification; those earlier limits must not remove requirements agreed here.
The implemented construction record remains governed by ADR 0027. PR #780's
merge establishes the reentry fix, not completion of this requirement.
Earlier recommendations, agreement with them, and ADR 0036's unimplemented
proposal are not independent evidence for the new design.

The handoff reports preferences for low complexity, few extra packages, and
avoiding a full in-memory history. These guide comparison; they do not decide
the observation scope or authorize silent omissions.

## Decisions agreed on 2026-10-15

The maintainer accepted the recommendations presented in the first round:

- A SQL recording interval has an explicit start and stop and selected
  connections. It includes later ordinary `collect()` / `pull()` / `compute()`
  and other SQL on those connections, not only recognized Margin descendants.
- SQL audit history is saved automatically and remains readable after R exits.
  Persistence must not retain a full duplicate history in memory.
- Generated SQL, execution requests, retrieval outcomes, and cleanup are
  distinguishable. Repeated executions remain separate attempts, conditions
  are retained, and a warning is not itself a failed operation. Client
  observations do not establish a server outcome they did not observe.

This first-round assent settled the intended behavior, not technical
feasibility or complete capture. Later rounds resolve the driver set,
recording-failure policy, connection interface, storage, and process scope
below. An interceptor and public API remain undecided; product implementation
is not authorized. Detailed recommendations below remain proposals unless
covered by an agreed decision.

## Decisions agreed on 2026-10-24

The maintainer accepted the second round's recommendations:

- The initial supported set is RSQLite, DuckDB, and RPostgres. Audit coverage
  must be verified for each before it is advertised as supported. Reject
  activation for an unsupported selected connection. This limits initial
  delivery, not the long-term request; it establishes no coverage for an
  untested driver version or execution path.
- If a start record cannot be persisted, do not invoke the corresponding SQL
  operation. If recording an outcome fails after invocation, report incomplete
  evidence and refuse further ordinary audited work. Do not imply that the
  database work did not happen or was undone.
- Required cleanup and recovery must remain possible. Preserve an original
  SQL error alongside a recording failure, and leave the caller's transaction
  commit/rollback decision with the caller. Do not silently continue without
  recording or substitute an unbounded in-memory retry queue.

These are acceptance requirements, not demonstrated properties of a capture
mechanism. How to enforce the failure state across direct DBI calls, nested
operations, and caller error recovery remains a feasibility question. The
stop policy concerns recording failure; an ordinary SQL error successfully
recorded is not itself failure of the recorder.

The maintainer also accepted the third round's recommendations:

- Keep ordinary `DBI::dbConnect()` and existing connections. Start recording
  with the selected connections and a destination; continue using ordinary
  `collect()`, `pull()`, and other supported operations. No dedicated connection
  construction or replacement retrieval function is required by the intended
  interface. This does not select a capture mechanism. A verified inability
  to meet coverage or stop behavior requires presenting the gap and an
  alternative before changing this requirement.
- Retain the actual SQL argument and each execution's bound parameter values
  separately, without interpolating the values into the SQL or copying fetched
  result data into history. Two executions of `WHERE region = ?` with `Tokyo`
  and `Osaka` must remain distinguishable. SQL literals, parameter values, and
  diagnostics may themselves contain sensitive or large values. No automatic
  redaction or parameter-omission mode has been adopted.

Parameter type representation, size behavior, and stream handling still require
specification. Recording must not silently truncate values or consume a stream
solely to construct its record.

The maintainer accepted the fourth round's recommendations:

- Use a dedicated local SQLite audit file through RSQLite. RSQLite is required
  when using the audit feature, including when the target is PostgreSQL or
  DuckDB. Use a private storage connection, commit the start before invoking
  target work, and commit its outcome separately. Target rollback must not
  roll back the audit history. Retain active state and transient event buffers
  only; read requested history ranges from disk.
- Capture is local to the R process, with one active writer process per history
  file. Parallel work requires explicit activation in each worker, its own
  connections, and a separate file. Parent activation alone captures none of
  the worker's activity. Do not promise automatic propagation or history
  merging in the initial feature. Each history identifies its process scope
  so that it cannot imply coverage of the whole parallel job.

This selects the storage medium and process boundary, not a journal mode,
measured latency, crash guarantee, or capture mechanism. The storage connection
is separate from the selected target connections and its writes are not caller
SQL activity. No dependency metadata or product implementation has changed.

## Scope reconsideration on 2026-10-24

The maintainer asked about retaining only SQL originating from or related to
marginplyr public functions. This reopens the first round's inclusion of
unrelated same-connection SQL. It is a request to evaluate an alternative,
not adoption of the proposal below. Earlier assent does not establish that
connection-wide retention is necessary for the underlying requirement.

The new recommendation is an operation-related history: within the recording
interval and supported connection boundary, include public marginplyr work and
the later execution of results derived from it. Exclude independent work on
the same connection. This is a scope requirement, not a verified mechanism.
The [related-scope investigation](../../investigation/sql-audit-related-scope-2026-10-24.md)
records the existing API paths, absent audit provenance, and limitations of a
single result attribute.

| Candidate boundary | Later `collect()` / `pull()` | Unrelated SQL on the same connection | Assessment |
| --- | --- | --- | --- |
| Public construction call only | Excluded after return | Excluded outside the call | Does not meet the retrieval requirement |
| Public operation and its derived results | Included through supported relationships | Excluded | Recommended for the stated purpose; provenance remains to be designed |
| All selected-connection work in the interval | Included without needing provenance to select it | Included | Prior choice; broader persistence than the requirement itself demands |

Recommended meaning of related work:

- Public-call execution includes work delegated to dbplyr, DBI, and the driver,
  including metadata, zero-row queries, probes, controls, and cleanup. The
  package containing the SQL string's construction does not decide inclusion.
- Later ordinary collection, pulling, display preview, and materialization
  belong when their input derives from a recorded Margin operation. Track
  repeated execution separately and retain each operation's errors, warnings,
  partial retrieval, and cleanup. Preserve generated-versus-executed evidence.
- Supported transformations must propagate all known origins. A related object
  on either side of a join makes the whole resulting query related; a query
  can have several origins. Do not remove allegedly unrelated clauses from an
  executed SQL statement. Preserve the relation through `compute()`'s returned
  lazy result and through `collapse()`, without treating collapse as retrieval.
- Attribution should identify caller expressions evaluated during a public
  call separately from package-internal work. Argument evaluation and nested
  calls must not disappear because the old construction record starts later.
  This is a candidate causal boundary, not a claim that every SQL call in a
  callback was authored by marginplyr.
- An independent `dbGetQuery()` or `dbWriteTable()` outside those operations
  is excluded even on the selected connection. Auxiliary writes required by a
  related operation are still included. Observation for classification does
  not require first persisting all SQL and filtering it only for display.

Do not infer relation solely from equal SQL text, shared table names, or the
same connection. A manually extracted SQL string, a reopened materialized
table, or a transformation that loses origin information needs a defined
boundary. Explicit association and refusal at a detectable unsupported path
are options to settle; undetectable loss must remain a stated limitation until
resolved. Unknown relation cannot silently mean unrelated. No unqualified
promise of automatic complete lineage is supported by the evidence.

This narrower proposal still goes beyond ADR 0037: it retains later related
retrieval and durable accumulated history. It does not return to the old
construction-only extension. A provenance mechanism may also require revisiting
ADR 0016's class/attribute boundary; ADR 0013 alone does not prohibit it.

If adopted, revisit connection selection, callback/argument boundaries,
caller-owned transaction controls, explicit association after provenance loss,
and the scope of refusal after recording failure. Independent bulk writes
would no longer drive the initial feature's payload policy, although a related
operation could still use a bulk path. Q9 and Q10 remain unanswered and are
paused while this more fundamental scope question is considered.

## Established facts

The dated [recording investigation](../../investigation/sql-audit-recording-2026-10-13.md)
holds the inspected commit, call paths, SQLite measurements, documentation
discrepancies, and verification limits. In that snapshot:

- The two-column `last_sent_queries()` record covered one construction call's
  prepared SQL and unexecuted result render; it carried no execution outcome.
- Later collection, pulling, and materialization did not update it, including
  package-owned SQLite execution. A measured `compute()` issued eight DBI
  requests while leaving the construction record unchanged.
- Nested calls had separate active records but no retained joint history.
- Descriptions of actual sending and send order exceeded those observations.

## Agreed direction and details to settle

The prior agreement uses the SQL recording interval and selected connections
as the coverage boundary, including unrelated same-connection work. The scope
reconsideration above proposes adding an operation/result relationship as the
retention criterion. Both retain marginplyr's explanation and ordinary later
retrieval/execution; neither proposes replacing each `collect()` or `pull()`
with a different retrieval function.

This is broader than ADR 0037. Restricting capture to automatically recognized
Margin descendants risks dropping transformed queries, joins, and callbacks;
that recognition has not been established. Capturing every connection for the
whole session instead increases volume and unrelated content. These were
alternatives to the selected boundary, not impossibility claims.

Treat the following as different facts, with source and observation boundary:

| Fact to expose | Candidate recording time | Meaning |
| --- | --- | --- |
| Prepared SQL | At the actual preparation/render site | SQL representation exists; no execution inferred |
| Request | Before invoking the observed execution boundary | Operation was about to be requested, with actual SQL argument when available |
| Return / failure | At that boundary's exit or observed condition | That client operation returned or failed; server final state may remain unknown |
| Retrieval progress | Each observed fetch return | Rows returned to R and known completion state, separate from total rows in the source |
| Cleanup | Its own invocation and outcome | Result/savepoint cleanup, independent of the earlier retrieval outcome |

The record must distinguish a pre-invocation entry from proof of server receipt.
Missing terminal evidence is unknown/incomplete, not inferred failure or
success. Generated SQL must not be substituted for the actual request SQL.
Raw SQL and parameter values are retained separately when placeholders are
used; their precise representation and supported types remain to be settled.

Record each attempt separately, even when SQL repeats. Record warnings
separately from success/failure. Explain probe failures that are handled as
part of ordinary dialect detection without labelling the enclosing operation
as failed. Preserve parent/child attribution and all covered nested work.
Unknown attribution remains unknown; SQL-text similarity does not prove
descent from a particular result.

Include SELECT, zero-row and metadata queries, dialect probes, control SQL,
and materialization statements at supported observation boundaries. A small
operation record should explain no-SQL outcomes such as validation refusal,
non-SQL work, and cached probe reuse. This does not require logging every R
expression or storing result data.

Observe the existing operation without extra target SQL, fetches, or changed
warning delivery. Audit-storage writes are separate work on a private connection
and must not be reported as caller activity. A generic outer handler alone
cannot capture internally handled
conditions; DBI handlers alone cannot capture warnings emitted later by
dbplyr. The implementation must cover these distinct sites or explicitly
identify the remaining gap. Server-internal statements and other processes
are not established by these client observations.

## Examples the specification must explain

The entries below are illustrative behavior, not API names or fixed schemas.

```r
q <- summarize_with_margins(sales_db,
  revenue = sum(revenue, na.rm = TRUE), .grouping = rollup(region))
x <- dplyr::collect(q, n = 10)
y <- dplyr::pull(q, revenue)
z <- dplyr::collect(q, n = 10)
```

1. Construction A explains each internal query that runs and records q's
   generated SQL as unexecuted. Cached work is distinguished from missing work.
2. Retrieval B records its actual SQL, which can contain `LIMIT 10`, and the
   observed fetch/outcome. Returning ten rows does not prove there are ten
   rows in the unrestricted result.
3. Retrieval C records the SQL selecting `revenue`, rather than copying A's
   result SQL. A failure retains its observed phase and diagnostic.
4. Retrieval D is another attempt, even when its SQL equals B's. A warning
   does not by itself change normal return into failure; warning-to-error
   behavior follows the caller's settings.

Further acceptance scenarios to settle:

| Scenario | Required distinction to evaluate |
| --- | --- |
| `compute(q)` | Metadata, DDL, data writes, transaction controls, and cleanup; R return versus commit |
| Repeated `dbBind()` and partial `dbFetch()` | Statement/result identity versus binding attempts and fetch pages; unknown versus zero |
| A calls B, then resumes | Both histories plus attribution, even if B fails and A catches it |
| `.data` promise calls another Margin verb | Work preceding the outer construction record is still in the chosen interval |
| SQL render or argument validation fails | Failure without a SQL execution, not an empty successful audit |
| `print(q)` / preview | Implicit retrieval within the interval, with its actual limited query |
| Recording stops before a later retrieval | Explicit coverage boundary, never a claim that later work was recorded |
| Unsupported backend or worker process | Visible unsupported/incomplete coverage, not an unexplained empty record |
| Process interruption or storage failure | Durable start without terminal outcome versus lost evidence; recovery policy unresolved |

## Responsibility and persistence alternatives

The recommended user experience combines package explanations and execution
observations. Whether one package or a companion component implements that
experience remains open. Package-owned later SQLite work is a reason to
reconsider a categorical construction-only boundary. It does not by itself
prove feasibility of observing every driver.

Automatic persistence to a dedicated SQLite file without a full duplicate in
memory is agreed. A bounded in-memory record with manual export alone would
not satisfy that decision. JSON Lines was considered, but its framing alone
does not supply transactional persistence; matching that behavior would need
additional machinery. SQLite avoids choosing that work solely to avoid the
RSQLite feature dependency. This is a design tradeoff, not a measured speed
or resource ranking against other formats.

The recording-failure policy and process-local, separate-file scope above are
agreed. Retention limits, crash durability, and reader contention remain to be
settled. Surviving normal R exit does not yet specify guarantees for power loss
or a process crash. SQLite storage does not select `trace()` or another capture
mechanism.
Client evidence can support an organizational audit; it does not independently
establish server receipt, transaction commit, or a tamper-resistant server log.

## Feasibility gates before selecting an implementation

The [2026-10-24 investigation](../../investigation/sql-audit-feasibility-2026-10-24.md)
records trace experiments, a native RPostgres COPY path, and storage source
readings. Simple DBI-generic tracing is insufficient: saved references can
bypass it, exit handlers can be replaced, existing tracing can be lost, and
global tracing state can disable entry guards. A broader mechanism remains
unverified, not proven impossible.

Before claiming coverage, map supported public operations to actual DBI and
driver observation sites, including metadata, transactions, table write/append,
Arrow/native execution, and retries. Preserve ordinary warning/error behavior
and cleanup while enforcing the agreed recorder-failure state after the caller
catches an error. A driver name alone cannot stand in for that evidence.

RPostgres COPY illustrates a further data decision: write payloads are not
`dbBind()` parameters. The agreement to retain bound values does not silently
authorize a requirement to retain every table-write payload. SQL capture on
that path and payload retention need distinct acceptance criteria.

## Documentation changes to design with the behavior

Keep the implemented contract and the proposed feature clearly separate.
The public contract should state coverage and timing in its opening paragraph,
then link to examples covering construction, later retrieval, and failure.

- In the existing accessor, describe prepared SQL and recording order;
  remove claims of observed actual sending. Describe `NA` as failed rendering
  without inferring a cause that was not retained.
- In the database guide, show generated result SQL separately from internal
  requests and later `collect()` / `pull()` / `compute()` examples. An export of
  construction rows must not be presented as complete execution history.
- Update the shared query-policy roxygen section and its inherited references;
  connect inspection's zero-row SQL with the recording surface. Correct the
  already-withdrawn exactness claim about `is.data.frame()`.
- Give a single coverage table for supported SQL/conditions, observation sites,
  exclusions, and retention. Put the per-driver guarantee beside its evidence.
- Preserve compatibility deliberately: the current two-column accessor cannot
  silently become a complete execution history. Naming/migration is undecided.

Generated `man/` files must follow their roxygen sources when those edits are
made. No public documentation in this draft announces an unimplemented feature.

## Decision frontier

The first four rounds are recorded above, with the inclusion of unrelated SQL
reopened. The active question is:

11. Retention scope: change from all selected-connection SQL to SQL belonging to
    public marginplyr operations and their derived results? Recommendation:
    adopt the operation-related boundary described above, retaining later
    ordinary retrieval and all delegated auxiliary work. Provenance completeness
    is an acceptance requirement to establish, not an existing capability.

The fifth round's unanswered questions are paused pending that scope choice:

9. Durability: prioritize committed evidence at each observation boundary, or
   permit delayed batch writes for throughput? Recommendation: synchronously
   commit required observations, including start before target invocation and
   terminal evidence before returning control to the caller. Do not defer
   these commits to interval stop or normal R exit. Use an explicitly configured
   SQLite durability mode; default RSQLite settings are not sufficient evidence
   for this requirement. SQLite's documented options include WAL with FULL and
   rollback DELETE with EXTRA; selecting and testing the configuration is an
   implementation obligation, not proof already obtained. Accept the additional
   write latency without inventing a performance estimate. A crash between the
   independent target and audit commits can still leave a committed start with
   an unknown outcome. Process-crash and system/power-loss behavior need separate
   validation, with the latter conditional on SQLite's storage assumptions;
   no unconditional no-loss guarantee is proposed.
10. Bulk input: retain the full data supplied through native table-write/COPY
    paths, or retain SQL and explicit input metadata without that payload?
    Recommendation: record every covered SQL statement and its outcome, plus
    the bulk operation's target, supplied column names/types, and input row
    count when available without consuming a stream. Do not copy the bulk
    dataset into the initial audit history. Mark payload retention explicitly
    as omitted by policy, never as empty data or an unknown SQL statement.
    Requested input rows and observed affected rows are different facts.
    Preserve the already agreed actual values for ordinary bound parameters,
    including batched binding; do not relabel those as bulk payload to omit
    them. Consequently INSERT through binding retains values while native COPY
    does not. Explain that route-dependent boundary and that COPY cannot be
    reconstructed from this history alone. Keeping whole input datasets would
    be a broader data-reproduction feature with its own size and stream rules.

Later rounds depend on those answers: mechanism feasibility; exact condition
coverage; operation and attempt fields and parameter representation; failure
recovery; retention; compatibility/API; and the documentation's exact guarantee.
These are open,
not silently deferred out of the request. Shared understanding of the complete
specification must be confirmed before finalizing the replacement for ADR 0037.
