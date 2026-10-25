# SQL audit cancellation and resumption bug hunt

Investigated: 2026-10-25
Repository snapshot: `b6f9faba946521aeb92fcf2c5655a2c3bf608ec8`

## Findings and scope

Two defects were reproduced after the fixes described in the
[earlier hunt](sql-audit-bug-hunt-2026-10-25.md) and its
[followup](sql-audit-followup-bug-hunt-2026-10-25.md). Cancellation during
diagnostic rendering could leave session warning options muted. A recovered
interrupt followed by an unrelated nonlocal exit could leave a misleading
terminal status. No package implementation or existing test was changed.

| Finding | Priority assessed on the investigation date | Strongest evidence | Repair ticket |
| --- | --- | --- | --- |
| Diagnostic rendering left warning options at `quiet` after cancellation | P2 | Two native SIGINT runs against an unmodified internal renderer with valid lazy queries; deterministic public SQLite transition fixture | [#821](https://github.com/sayuks/marginplyr/issues/821) |
| A recovered interrupt survived as the status of a later unmatched exit | P3 | Independent public SQLite boundary fixtures using both a resumable signal and a recoverable R interrupt | [#822](https://github.com/sayuks/marginplyr/issues/822) |

The tickets were independent repair slices with no blocking edges. Neither
required a preliminary refactor. The observation contract came from
[Internal SQL diagnostics](../design/specs/sql-audit.md),
[ADR 0027](../design/adr/0027-record-the-sql-marginplyr-sends.md), and
[ADR 0037](../design/adr/0037-keep-sql-diagnostics-local-to-marginplyr.md).
ADR 0036 was superseded proposal history, not a requirement to accumulate
records. Later retrieval, driver-private SQL, and persistent history stayed
outside the investigation's feature boundary.

The [companion runbook](2026-10-25-sql-audit-cancellation-resumption/README.md)
maps commands to retained evidence and exit meanings. The scripts load the
checkout selected at execution time; the snapshot above identifies what was
investigated, not a promise that later executions use it.

## Warning options remained muted after cancellation

`render_sent_query_sql()` in `R/sent-queries.R` changed
`rlib_warning_verbosity` and `lifecycle_verbosity` to `quiet` before registering
the `on.exit()` that restored them. An interrupt between those steps left no
restoration handler. The restoration itself also ran without an interrupt
shield. Those were two vulnerable transitions in one temporary-setting
lifetime; the native observations did not locate the exact instruction hit.

A public SQLite fixture injected one `rlang::interrupt()` immediately after
the option change and before restoration registration. It used a temporary
in-memory `trace()` on the renderer and called `summarize_with_margins()` with
an observed Margin label collision check. The original interrupt escaped, but
both options remained `quiet` instead of their initial `default` values.
Two later warning markers with fresh frequency identifiers produced no
warnings under the leaked settings and two after manual restoration. This
established an effect beyond the interrupted package call.

```sh
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/render-options-window.R
```

The historical output is retained in
`evidence/root/render-options-window.log` and its RDS beside it. This was
deterministic public-path fault injection, not unmodified public-path native
evidence.

### Native evidence at the unchanged internal renderer

Two independent processes rendered valid `tbl_lazy` aggregate queries through
the original `render_sent_query_sql()`. No package function or recording
binding was replaced. A forked sender delivered one SIGINT per request and
acknowledged it before the parent began another attempt, preventing an
uncontrolled stream of pending signals.

| Run | Attempts at detection | Caught interrupts | Settings after interruption |
| --- | ---: | ---: | --- |
| Coordinating process | 959 | 959 | `quiet`, `quiet` |
| Independent agent | 2,106 | 2,106 | `quiet`, `quiet` |

Both processes killed and reaped the sender, observed no additional pending
interrupt when checking for delivery after cleanup, saved RDS evidence, and
exited with the intended violation code. An independent local inspection left
the muted options unchanged, and arithmetic still returned `2`. The retained
row had no condition summaries. That was compatible with interruption before
the render observer became active, but was not a native stack attribution.

```sh
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/native-options-valid.R internal 45
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/native-options-ack-valid.R internal 40
```

The positive evidence is retained as
`evidence/root/native-options-valid-internal.{log,rds}` and
`evidence/conditions/native-options-ack-valid-internal.{log,rds}`. Counts were
timing-dependent observations, not estimated failure rates. A bounded run
without a counterexample was inconclusive.

### Why the public native symptom did not identify the renderer

Unmodified public calls also leaked `lifecycle_verbosity = "quiet"`, but the
same symptom occurred with auditing disabled. Those calls left
`rlib_warning_verbosity` at its initial value. They could not establish that
the audit renderer caused the public symptom.

| Public run | Attempts | Caught interrupts | Settings after interruption |
| --- | ---: | ---: | --- |
| Auditing enabled | 2,957 | 2,957 | `default`, `quiet` |
| Auditing disabled | 1,191 | 1,191 | `default`, `quiet` |

These observations are retained in
`evidence/root/native-public-{on,off}.log` and
`evidence/root/native-public-{on,off}-public.rds`. One later warning marker
was suppressed in these lifecycle-only cases; the two-marker loss belonged
to the deterministic renderer fixture above.

The namespace inventory found another temporary-setting site:
`prepare_grouping_plan()` used `rlang::with_options()` around its discarded
name-only compilation pass, and `discarded_pass_verbosity()` selected `quiet`
unless the caller requested lifecycle errors. Installed rlang 1.3.0's
`local_options()` changed the options before registering its deferred restore.
A native control repeating only
`rlang::with_options(1L, lifecycle_verbosity = "quiet")` reproduced the
lifecycle-only leak after two attempts and two caught interrupts. It completed
sender cleanup and evidence preservation. This demonstrated an alternative
mechanism used by the public path, without attributing the earlier public
interrupt to an exact stack location.

```sh
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/native-rlang-options-ack.R internal 30
```

The lifecycle-only observation was kept separate from the two SQL-audit repair
tickets. It was evidence for further work on the shared temporary-setting
path, not an additional audit-render defect counted from the same symptom.

Public native attempts that isolated the audit path remained inconclusive:

| Isolation attempt | Attempts / caught interrupts | Observation |
| --- | --- | --- |
| Opaque valid selection bypassing the discarded name-only pass | 4,845 / 4,844 | No option leak within 55 seconds |
| Watch only the rlib option, auditing enabled | 2,487 / 2,486 | No rlib leak within 50 seconds |
| Watch only the rlib option, auditing disabled | 2,212 / 2,211 | No rlib leak within 45 seconds |

The opaque selection was `.grouping = rollup(identity("v"))`; it retained
valid SQL construction while making `preflight$name_only` false. Two
additional public-off stack-capture attempts, of 13,125 / 13,124 and
4,852 / 4,851, also failed to reproduce the leak. None established safety or
an audit-specific native reproduction through the complete public API.

## A recovered interrupt was mistaken for the later terminal outcome

`with_sent_query()` changed its local status to `interrupted` as soon as its
calling handler observed an interrupt. Normal return later replaced that
value with `returned`. An unrelated nonlocal exit after recovery performed
neither update, so the exit handler stored the earlier `interrupted` value.
The specification's `unknown` outcome was required when the invocation ended
without an established terminal observation.

A public SQLite `summarize_with_margins()` fixture replaced only the owned
`collect()` boundary. A caller recovered from an interrupt and then the
fixture invoked a distinct restart to leave the operation. Auditing off and
on returned the same escape value and delivered the same condition; the
audited row incorrectly retained `interrupted`.

Two agents independently reproduced this sequence. Both a synthetic
`signalCondition()` interrupt and an `rlang::interrupt()` recovered by a
caller-provided restart demonstrated it. The latter exercised R's interrupt
machinery, but was still a controlled fixture rather than an OS SIGINT
observation. The coordinator repeated the public fixture. These were
independent confirmations of one defect, not multiple findings.

```sh
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/resumed-exit.R
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/resumed-interrupt-controls.R
```

The eight-cell control matrix separated the signal from the final path. Normal
return produced `returned`; a later terminating error or interrupt produced
the corresponding terminal status. The recovered-interrupt-then-unrelated-exit
cell alone retained `interrupted` where `unknown` was expected. The earlier
signal summary remained recorded once. This was diagnostic misclassification,
not a measured change in the caller's control flow.

The result differed from #816's abandoned `invoking` state: the operation had
an exit handler and stored a terminal-looking value, but that value described
a signal from which execution had already recovered.

## Passing comparisons and retention checks

R was 4.6.1 on arm64 macOS. The development library held dplyr 1.2.1,
dbplyr 2.6.0, DBI 1.3.0, RSQLite 3.53.3, DuckDB 1.5.5, RPostgres 1.4.10,
rlang 1.3.0, lifecycle 1.0.5, pkgload 1.5.3, testthat 3.3.2, and callr 3.8.0.

The checkout was installed into a disposable library and that library was
propagated to child processes through `R_LIBS`. All 14 focused SQL-diagnostic
test files passed, with 1,094 expectation markers and no failures, warning
markers, or skips. The first attempt had used an older installed package in
child processes and failed with `unused argument (details = TRUE)`; it was
replaced by the isolated-library run, not treated as a package defect.

| Additional check | Measured result |
| --- | --- |
| SQLite and DuckDB audit off/on live comparisons | 111 cases per backend; 222 matching pairs |
| Inspection results within those live cases | 40 pairs with column values and tibble attributes compared |
| Caller transaction and real SQL-failure comparisons | Ten matching off/on pairs across SQLite and DuckDB |
| Simulated SQL dialects | 16 dialects, eight cases each; 128 matching pairs |
| Recorded internal SQL versus observed DBI crossings | 131 statements matched across 261 recorded rows: 128 prepared, 127 returned, six errored |
| Public record lifetime | 80 cases passed across entry spellings, input kinds, option changes, nested errors/restarts, exit-handler reentry, and fresh-call replacement |
| Formatter and fallback | Nine branches passed, including attributed, empty, invalid, and missing text |
| Retained-object probes | 45 weak references cleared while the target record was still retained |
| Copied views and replacement | Deep mutation independence over 405 replacements; two copies held during active invocation remained independent |
| Invalid settings and accessor arguments | 11 inactive option values and 26 invalid accessor calls preserved the established behavior |

The live and simulated comparisons totaled 360 pairs. Collected numerical
values, result classes, inspection attributes, escaped conditions, rendered
SQL, and caller transaction state were compared where applicable. DuckDB row
order was normalized only for `.sort = "none"`, whose order was unspecified;
explicitly ordered cases were compared as returned. The DBI observations were
generic boundary crossings, not proof of network submission.

The retained captures can be checked offline using the companion runbook.
No new live PostgreSQL comparison was run in this hunt. The focused tests and
passing controls were not the full Review-ready check and did not establish
the absence of untested defects.

## Candidates and failed experimental assumptions

An invocation condition inheriting from both `interrupt` and `error` was
classified as `errored`, while rendering prioritized `interrupt`. Twelve
audit off/on comparisons agreed on propagation and results. The contract did
not define a priority for overlapping classes, so this asymmetry was retained
as a candidate rather than counted as a confirmed defect.

The first internal native harness passed a literal SQL object directly to
`sql_render()`, which had no applicable method. Its leak observations were
not used as evidence for valid-query rendering; the two positive runs above
used actual lazy queries. Early repeated-signal runs also halted after detecting
changed settings. They did not establish complete cleanup or a usable
continuing session. The single-request acknowledgement runs supplied that
missing evidence, rather than relabelling the earlier failures as completed
experiments.

Some original harnesses reused a generic RDS destination, while their logs
named that generic path. Preservation copied the matched observations under
distinct names and recorded the correspondence in the companion manifest.
The replay forms select a new output directory and leave dated captures alone.

## Lessons for another hunt

Start with the owned observation boundary and split investigation by condition
handling, record lifetime, and backend operations. Independent confirmations
can improve confidence, but the same failing sequence remains one finding.
Challenge proposed causes with auditing disabled before assigning a public
symptom to the observer: another option setter produced the same lifecycle
symptom in this investigation.

For cancellation, pair a deterministic public transition fixture with native
execution of unchanged code. Keep their evidence levels explicit. Limit a
sender to its disposable parent process, acknowledge each signal before the
next attempt, and verify sender reaping and continued execution before calling
a native experiment complete. Faster interruption can spend the whole run
before the interesting transition; vary delivery timing and report bounded
negative results as inconclusive.

Recoverable signals need a matrix crossing recovery with the later exit path.
A test that covers normal return after recovery, and a separate test that
covers nonlocal exit, does not cover their composition. Preserve the original
signal, the returned value, and the recorded terminal status independently.

Keep the checkout, installed child-process library, and dependency versions
aligned. Save small synthetic captures, exact command variants, exit meanings,
and rejected interpretations beside the note. Offline replay makes measured
claims checkable without reproducing signal timing or provisioning a server;
later execution still needs its own runtime and snapshot record.
