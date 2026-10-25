# SQL diagnostic bug hunt

Investigated: 2026-10-25
Repository snapshot: `c62ecdb01d31ca5c2c9ab8baed0dda970dd844e5`

## Scope and result

The investigation examined the complete SQL diagnostic feature after PR #808,
rather than a branch diff. Three defects were reproduced: diagnostic rendering
consumed lifecycle warning state, an interrupted call could strand its record
owner, and one signal with overlapping condition classes was stored twice.
No package implementation or regression test was changed during the hunt.

The contract read was [Internal SQL diagnostics](../design/specs/sql-audit.md),
[ADR 0027](../design/adr/0027-record-the-sql-marginplyr-sends.md), and
[ADR 0037](../design/adr/0037-keep-sql-diagnostics-local-to-marginplyr.md).
Later collection, materialization, arbitrary callback SQL, private driver
statements, and persistent history were outside that contract. Their absence
from the record was not treated as a defect.

| Finding | Priority assessed on the investigation date | Evidence |
| --- | --- | --- |
| Auditing consumed a lifecycle warning and changed failure to success | P2 | Live SQLite and DuckDB, without driver mocks |
| Recoverable interruption stranded an active owner and hid later records | P2 | Targeted cleanup injection and native SIGINT stress without package-function replacement |
| One compound condition signal produced two identical summaries | P3 | Controlled invocation-boundary fixture through the public verb and accessor |

## Environment and controls

The host ran R 4.6.1 on `aarch64-apple-darwin23`. Installed dependencies were
dplyr 1.2.1, dbplyr 2.6.0, DBI 1.3.0, RSQLite 3.53.3, DuckDB 1.5.5,
RPostgres 1.4.10, rlang 1.3.0, lifecycle 1.0.5, testthat 3.3.2, and
pkgload 1.5.3. The disposable PostgreSQL server was version 17.11 and was
stopped after the live checks.

Three agents examined condition boundaries, ownership/lifetime, and internal
SQL/probe paths independently. The coordinating process ran the existing
focused suite, investigated render side effects, and independently reran the
counterexamples. Follow-up checks challenged candidate findings against the
contract before accepting them.

Warning-frequency comparisons used separate R processes for auditing off and
on. `TESTTHAT` was unset inside those processes: dbplyr suppressed one of its
ordinary aggregation warnings in the testing environment. DuckDB connections
used `shared_home = FALSE`. Fixtures contained synthetic data only.

The first focused-suite run used the working tree in its parent process but
an older installed marginplyr in subprocesses. It produced 531 passing
expectations plus subprocess failures such as `unused argument (details =
TRUE)`. Those failures were an environment mismatch, not feature defects.
Installing the inspected working tree in a disposable library and putting
that library first produced 564 passing expectations, with no failures,
warnings, or skips:

```sh
mkdir -p /tmp/marginplyr-sql-audit-library
R CMD INSTALL --library=/tmp/marginplyr-sql-audit-library .
R_LIBS=/tmp/marginplyr-sql-audit-library Rscript -e \
  'devtools::test(filter = "sent-quer|sql-audit", reporter = "summary", stop_on_failure = TRUE)'
```

These commands describe the corrected setup; the original run used another
disposable directory. The passing suite did not establish absence of the
three additional counterexamples below.

## Recorded coverage

| Experiment | Cases and observations on the inspected snapshot |
| --- | --- |
| Live SQLite, DuckDB, and PostgreSQL | 15 auditing-off/on pairs: inspection, a successful observed label check, collision refusal, a cold dialect probe, and cache reuse on each backend |
| PostgreSQL probe controls | 24 controlled driver-boundary pairs: four savepoint/rollback/release failure positions, crossed with error/warning/interrupt and `warn = 0`/`2` |
| Backend completion and cleanup | 16 controlled pairs crossing return/warning/error/interrupt in the operation body and its `on.exit()` cleanup |
| Ordinary conditions and exits | Warning levels `-1`, `0`, `1`, and `2`; caller-handler nonlocal exits; unknown completion; original condition propagation |
| Ownership and retained values | Six public spellings including the summarise alias; delayed/active-binding inputs; nested option changes; copied views; weak-reference collection of attributed condition environments |

The 15 live pairs agreed on DBI generic call sequences, results, warnings,
errors, and expected record purposes/statuses. PostgreSQL checks also preserved
the caller transaction and allowed a subsequent constant query. The 24 control
cases used `postgres_probe_fixture()` doubles, not live server failures. They
preserved escaping conditions and recorded separate cleanup outcomes. The
normal condition/cleanup matrix likewise found no audit-dependent difference.

[Retained evidence](2026-10-25-sql-audit-bug-hunt/evidence/manifest.txt)
distinguishes completed live runs, controlled fixtures, and rejected
candidates. The raw probe logs' `SQL_COUNT` counted generic DBI boundary
crossings, including nested forwarding. For example, PostgreSQL's cold-share
case reported ten crossings for four controls and two collect operations.
It did not establish ten wire submissions.

## Finding 1: lifecycle warning state was consumed

`render_sent_query_sql()` temporarily set `rlib_warning_verbosity = "quiet"`
and muffled diagnostic-render warnings. That protected rlang's warning
frequency path, but lifecycle 1.0.5 used its own verbosity and deprecation
state. Local inspection of `dbplyr:::deprecate_ignore_case()`,
`lifecycle:::deprecate_warn0()`, and `lifecycle:::lifecycle_verbosity()` showed
that lifecycle marked a deprecation as warned before signaling its condition.
Muffling that signal did not undo the mark.

A lazy input containing `str_like(..., ignore_case = FALSE)` reached that
deprecation while rendering an internal query. With auditing off, the owned
operation delivered the warning. With auditing on, diagnostic rendering
consumed it first, so the real invocation delivered none. At `warn = 2`, the
unaudited public call errored while the audited call returned normally.
At `warn = 0`, both returned but auditing removed the user-visible warning.

The discrepancy was reproduced independently on DuckDB's zero-row selection
metadata and SQLite's observed Margin label collision query. It therefore
occurred inside the promised construction boundary, without relying on later
user collection. A separate SQLite deferred-result experiment exhibited the
same cause. Closed issue #655 had covered the ordinary dbplyr aggregation
warning; it did not establish protection of lifecycle's separate state.

From the repository root, the retained harness ran either backend with fresh
processes for each option/verbosity pair:

```sh
Rscript investigation/2026-10-25-sql-audit-bug-hunt/lifecycle-warning.R sqlite
Rscript investigation/2026-10-25-sql-audit-bug-hunt/lifecycle-warning.R duckdb
```

The original SQLite measurements were:

```text
AUDIT off WARN 0 RESULT returned WARNINGS 2
AUDIT on  WARN 0 RESULT returned WARNINGS 0
AUDIT off WARN 2 RESULT errored  WARNINGS 2
AUDIT on  WARN 2 RESULT returned WARNINGS 0
```

`WARNINGS` counted signals received by a calling handler. lifecycle emitted
both `signalCondition(wrn)` and `warning(wrn)`, so two signals represented one
ordinary displayed warning. This was distinct from finding 3's single-signal
duplication. The retained harness exited nonzero when warning delivery or
the public return/error outcome differed.

## Finding 2: interruption stranded ownership

The tracked entry points protected record publication and exit-handler
registration together with `suspendInterrupts()`. Their exit handlers called
`restore_sent_queries()` without equivalent protection. The local R help for
`suspendInterrupts()` described protection of cleanup handlers; `on.exit()`
alone did not supply that protection.

A targeted native SIGINT at restoration entry left `sent_queries$active`
true after the public call had unwound. A subsequent independent SQL call
then treated the stale owner as an active parent. Its own result row was
discarded when that stale parent was restored, and later independent calls
continued to expose the old empty record. The R process survived throughout;
this was not the excluded process-termination case.

The first targeted fixture replaced the restoration binding and enlarged the
delivery window. It was supporting fault-injection evidence, not proof of an
uninstrumented failure. Bounded native stress then reproduced the same state
with no package-function replacement in three separate processes:

| Run | Calls before detection | Caught native interrupts | Next independent SQL record |
| --- | ---: | ---: | --- |
| Coordinating process 1 | 3,750 | 3,718 | Zero rows; owner still active |
| Coordinating process 2 | 2,153 | 2,110 | Zero rows; owner still active |
| Independent agent | 6,075 | 6,027 | Zero rows; owner still active |

Another independent run completed 10,000 calls with 9,954 caught interrupts
without reproducing the leak. A time-limit experiment also found none in
10,000 calls with 98 timeouts. These were timing-dependent observations, not
failure-rate estimates or proof that an ordinary single Ctrl-C always failed.
The uninstrumented stress located the ownership failure; the targeted cleanup
fixture and unprotected restoration identified the likely vulnerable seam.

The retained interrupt harness described its targeted and explicit native
modes in its header. Native delivery required a POSIX host and was bounded;
absence of a reproduced leak was reported separately from verification of a
fix. The sender targeted only its invoking R process and was cleaned up.

```sh
Rscript investigation/2026-10-25-sql-audit-bug-hunt/interrupt-owner.R
Rscript investigation/2026-10-25-sql-audit-bug-hunt/interrupt-owner.R --native
```

The retained targeted mode injected one SIGINT through a temporary active
binding immediately before the ownership flag was cleared; it did not replace
the restoration function. It was an adapted fixture, distinct from the first
wrapper-based experiment. Native mode used neither fixture. Exit 1 denoted a
reproduced ownership violation; native exit 2 denoted an inconclusive bounded
run, including delivery outside the comparison window.

During preservation, the adapted native harness reproduced the leak after
4,688 calls and 4,646 caught SIGINTs, followed by another empty independent SQL
record. Its output was retained as `portable-native-replay.log` in the evidence
directory. The portable targeted and compound-condition harnesses also exited
at their intended invariant failures; their replay logs were retained there.

An early stress harness put `allowInterrupts()` outside `tryCatch()`. A pending
interrupt could then escape before the handler was registered, terminating
the harness without checking ownership. The corrected experiment registered
the handler first, opened delivery only around the public call, and inspected
the owner after return or caught interruption inside a suspended region.
That distinction prevented a harness failure from being mistaken for a
package-state failure.

## Finding 3: a compound condition was stored twice

A controlled `collect()` boundary signaled one base-R condition built with
`errorCondition("driver failed", class = "warning")`. The public detailed
accessor retained two identical summaries for that one signal because the
observer's warning and error handlers both appended it. An enclosing generic
condition handler received the condition once. The result was reproduced
with both `signalCondition()` and `stop()`.

```sh
Rscript investigation/2026-10-25-sql-audit-bug-hunt/compound-condition.R
```

The original observations for both signaling forms were one external signal,
two stored summaries, and an errored operation. This was a legal custom
condition fixture, not evidence that ordinary database drivers routinely
emitted overlapping severity classes. Priority was consequently assessed as
P3. Distinct signals with identical text were not duplicates merely because
their summaries matched.

## Candidates rejected or left unconfirmed

Nested conditions initially looked like ownership contamination. A live SQLite
experiment using a separately registered dbplyr dialect/translator showed that
a child warning also reached the outer collection observer when it propagated
through that boundary. Muffling it inside the child kept the parent's record
unchanged. The contract described conditions seen at each owned boundary,
not exclusive attribution to their originating call, so this was rejected.

Other excluded candidates were unspecified row ordering under `.sort =
"none"`, caller-induced getter side effects, and S4 condition inheritance
that base R's own category handlers did not dispatch on. A custom
`conditionMessage()` method that interrupted on its second evaluation changed
the audited outcome, but distinguishing formatter-induced interruption from
a real user interrupt required a policy decision; it was not accepted as a
fourth confirmed defect. Normal formatter errors, warnings, messages, and
restart isolation passed their focused checks.

## Reproduction and future comparisons

The companion scripts were dated diagnostic harnesses outside the source
package, not additions to its test suite. They loaded the selected working
tree and contained no corrective implementation. Their commands belonged in
fresh R processes; interruption experiments deliberately left damaged state
inside their disposable process when the defect was detected.

The evidence supported three independent repair slices. No prefactor or
blocking relationship was established: warning state, unwind ownership, and
single-signal accounting each had a separately demonstrable outcome. The
feature contract remained with the specification and ADRs cited above; this
note recorded measurements and rejected interpretations on its date.
