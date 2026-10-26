# SQL audit comprehensive follow-up bug hunt

Investigated: 2026-10-26
Repository snapshot: `bfcdaf76a36d102132cd039c68ba8bc05a4fd465`

## Result and scope

One new P3 defect was reproduced through the public Margin API: diagnostic SQL
rendering and condition-summary formatting delivered additional general R
conditions to the caller. It was
published as [#891](https://github.com/sayuks/marginplyr/issues/891), an
independent repair slice with no blockers, labelled `bug` and `ready-for-agent`.
The maintainer delegated ticket granularity and publication, so the
`to-tickets` workflow required no granularity confirmation. No package
implementation, package test, dependency, generated documentation, or public
contract was changed in this investigation.

The investigation began in a new worktree from `main`; the remote main ref
matched the snapshot above when checked. Three parallel roles examined
rendering/conditions, invocation lifecycle/retention, and backend SQL. The
coordinating process reran the focused package suite, ownership/storage
controls, earlier repaired examples, and the new defect reproduction.

The observation boundary was taken from
[Internal SQL diagnostics](../design/specs/sql-audit.md),
[ADR 0027](../design/adr/0027-record-the-sql-marginplyr-sends.md), and
[ADR 0037](../design/adr/0037-keep-sql-diagnostics-local-to-marginplyr.md).
The audit explained package-owned auxiliary SQL and prepared result SQL.
Later collection, materialization, arbitrary callback SQL, driver-private
statements, persistent history, and server receipt remained outside its scope.
Later renders and collections were used as comparison controls and checked
for changes to the saved construction record.

The [companion runbook](2026-10-26-sql-audit-exhaustive/README.md) names the
scripts, fresh-output arguments, saved observations, expected exits, and
offline replay commands. The scripts load the checkout selected at execution
time; the snapshot above identifies the code investigated.

## General conditions escaped diagnostic work

The fixture used an ordinary in-memory SQLite connection and dbplyr's
connection/dialect extension surface. Its SUM translator signaled a fixed
`simpleCondition` before forwarding all arguments to the original SQLite SUM
translator. It replaced no marginplyr recorder, dbplyr render function, or
driver operation. A public summary requested a two-set rollup and an observed
Margin label collision check.

| Observation | Audit off | Audit on |
| --- | ---: | ---: |
| Caller-visible general condition notifications during construction | 1 | 4 |
| Collected values | `a = 1`, `b = 2`, `Total = 3` | Identical |
| Result class | Lazy SQL result | Identical |

All delivered conditions were identical to the original leaf. The real
collision-count invocation accounted for one notification. Diagnostic
rendering added one for that count and two for the result's grouping-set
branches. The internal row was `returned`, the compatibility result was
`prepared`, and their condition-summary tibbles were empty. Reading and later
collection did not change the stored record.

`render_sent_query_sql()` handled interrupts, warnings, errors, and messages,
but its condition handler let a condition outside those classes continue to
outer handlers. This explained the additional delivery. ADR 0037's *Adopted
limits* required observation without changing condition propagation. The
empty severity summaries were consistent with their schema; adding general
conditions to that schema was not the proposed repair.

The [earlier extension investigation](sql-audit-extensions-and-options-2026-10-26.md)
reported diagnostic messages as #858. Its repaired ordinary and bare message
reproductions each delivered one message with auditing off and one with it on
on this snapshot. The new leaf inherited from `simpleCondition` and
`condition`, with none of the four severity classes, so the completed message
repair did not cover it. P3 reflected the need for a translator extension;
no stock SQLite translator was shown to emit this notification.

```sh
Rscript investigation/2026-10-26-sql-audit-exhaustive/generic-conditions.R
```

The command printed one notification off and four on, then exited 1 at the
desired equality assertion. The coordinating process independently obtained
the same result after the investigating agent's repeated runs. Both captures
were retained. #891 asks for diagnostic-only isolation while preserving real
invocation notifications, condition identity, restart ownership, result
behavior, and the existing audit schema.

A second reproduction reached the same isolation gap during condition-summary
formatting. A stable S3 warning formatter signaled a secondary
`simpleCondition` only when called by `sent_query_condition_message()`, then
returned the original warning text. The actual invocation's warning, result
classes, and collected values remained unchanged. With ordinary warnings and
`warn = 0`, construction delivered one original warning off and five
conditions on: the original plus four secondary diagnostic notifications.
The standalone reproduction exited 1 at the delivery-equality assertion and
was independently repeated by the coordinator. An eight-pair variation
crossed ordinary/bare warnings with `warn = -1/0/1/2`: every outcome agreed,
and every delivery comparison differed. The bare-warning `warn = 0` case
delivered one off and four on. These were two diagnostic sites of the same
general-condition isolation defect, so #891 covered both rather than splitting
the repair by internal function.

```sh
Rscript investigation/2026-10-26-sql-audit-exhaustive/formatter-generic-minimal.R
```

The formatter's diagnostic-only disturbance used a stack check to isolate the
summary seam; it was a controlled extension fixture, not evidence that a stock
driver emitted this condition. General notifications remained outside the
stored severity schema. The defect concerned their delivery to outer handlers.

## Passing controls

The host used R 4.6.1 on arm64 macOS. Package versions were dplyr 1.2.1,
dbplyr 2.6.0, DBI 1.3.0, RSQLite 3.53.3, DuckDB 1.5.5, RPostgres 1.4.10,
rlang 1.3.0, lifecycle 1.0.5, testthat 3.3.2, and pkgload 1.5.3.
The version transcript was retained with the evidence.

| Control | Observed result |
| --- | --- |
| Focused existing suite | All 17 Sent-query/SQL-audit test files passed without failures, warnings, or skips using an isolated installation of the checkout |
| New live backend comparison | SQLite 102, DuckDB 102, PostgreSQL 103: all 307 off/on pairs agreed on the compared fields |
| New simulated dialect comparison | 16 exported dbplyr simulators, 33 scenarios each: all 528 pairs agreed |
| New rendering/formatter comparison | All 128 S3 condition/formatter/warning-level pairs agreed on result classes or errors and caller-visible warning/message delivery |
| New lifecycle comparison | All 600 off/on pairs agreed on caller outcome, condition summaries, and cleanup actions; 540 unambiguous cases also asserted stored terminal status |
| New resumption/retention comparison | All 24 cases passed; all 88 weak references cleared while the latest record remained retained |
| Existing public lifetime fixture | All 80 cases passed |
| Existing storage/copy fixture | Nine formatter cases, 45 cleared weak references, 405 replacements, 11 inactive settings, 26 invalid accessor calls, and two active copies passed |
| Earlier repaired extension examples | Ordinary and bare messages each delivered 1 off/1 on; the malformed-option example returned the inspection tibble in both controls |

The live matrix covered base, empty, previously summarized, and previous
Margin-result inputs. Scenarios included duplicate grouping sets, Cartesian
products and composite dimensions, empty summaries, contextual helpers,
`across()`, fixed keys, cold/warm shares, collision refusals, missing/NULL/list
labels, numeric/logical/date dimensions, unusual column names, and invalid
calls. Refusals were compared alongside successful construction; a pair was
not necessarily a successful SQL algorithm. Successful lazy results numbered
68 on SQLite, 78 on DuckDB, and 79 on PostgreSQL. The saved backend summary
gave the separate refusal counts.

The matrix compared construction SQL at the DBI generics, condition class and
diagnostic, result class/columns, RNG state, later render, and collected row
multisets. It retained 430 audited rows: 98 selection-metadata, 81 collision,
13 dialect-question, nine numeric-control, four transaction-control, and 225
result rows. All 205 internal rows matched observed generic DBI crossings;
all 225 result rows matched later rendering. This was client-side evidence,
not network or server observation. Only PostgreSQL savepoint suffixes were
normalized, and only consecutive forwarding copies of its savepoint controls
were collapsed at the DBI generics.

DuckDB collection differed in original row order in 26 pairs. These were
unsorted results or rows tied under the requested Margin key, including
expansion rows differing in carried source columns. Sorted row multisets
agreed. The comparator did not establish a total order within tied keys.
The PostgreSQL 17.11 instance used a disposable Unix-socket-only cluster.
Its caller transaction remained active after the cold probe; `SELECT 9`
succeeded before explicit rollback. That server was stopped after the run.

The S3 render matrix crossed ordinary/bare warnings and messages, eight
formatter behaviors, and `warn = -1/0/1/2`. Secondary formatter behaviors
included errors, warnings, messages, bare signals, invalid return values, and
public inspection reentry. Secondary disturbances were limited to diagnostic
formatting so the original signal remained a stable paired control. Stored
summary column types and absence of retained attributes were also checked.
Its `warn = 1` fixtures printed their expected primary warnings.

The lifecycle matrix crossed no/synthetic/native recovered interruption,
five body outcomes, five cleanup outcomes, four nested-call modes, and two
warning levels. Controlled collection supplied the conditions; the public
SQLite Margin verb, recorder, and accessor were exercised unchanged. The
retention fixture crossed synthetic/native recovery, four later outcomes,
and nested depths zero, one, and three. It checked retained summaries, parent
restoration, terminal status, held views, and weak-reference collection.
Native recovery here meant an R interrupt through rlang and a caller restart,
not timing-driven operating-system cancellation.

## Bounded and rejected candidates

- A cleanup restart could replace a pending error/interrupt unwind while the
  row kept `errored`/`interrupted`. The body had already been interrupted;
  unlike #822, no resumed body followed by an unrelated exit was established.
  The 60 ambiguous permutations were retained and compared for control-flow
  transparency but excluded from the 540 terminal-status assertions. They
  were not ticketed as a new contract violation.
- A deliberately counter-dependent formatter changed behavior when extra
  diagnostic rendering changed its invocation count. It did not isolate a
  diagnostic-only disturbance from the original operation, so it did not
  establish an independent defect.
- An S4-condition extension exercise did not consistently reach R's ordinary
  condition handlers or the intended summary formatter. Its paired results
  were not counted as additional formatter-isolation coverage.
- dbplyr's temporary subquery-name counter could change during rendering;
  subsequent query building reset it. No resulting SQL or data difference
  was established, so this was not ticketed.

The feature's internal invocation sites were also checked against the source
inventory: metadata collection, observed collisions, dialect probes, numeric
controls, and PostgreSQL savepoint controls all used the owned observer. No
additional missing owned SQL site was established.

The finite matrices did not prove exhaustive correctness, performance,
crash recovery, arbitrary extension purity, server completion, or safety for
all native cancellation timings. Arrow and dtplyr received no new live
backend matrix in this run; their existing focused tests remained included.
Fixes and post-fix regression tests were left to #891. No CI monitoring was
performed.
