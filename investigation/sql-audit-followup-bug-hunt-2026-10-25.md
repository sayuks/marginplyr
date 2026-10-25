# SQL audit followup bug hunt

Investigated: 2026-10-25
Repository snapshot: `0135a8615b8a6ce9d42de7fc75e94578a5d0d4cf`

## Findings and scope

Two additional defects were reproduced after the fixes for the three findings
in the [earlier investigation](sql-audit-bug-hunt-2026-10-25.md). An interrupted
internal observer could retain an `invoking` row after unwinding, and diagnostic
rendering could swallow a condition inheriting from both interrupt and another
severity. The findings below distinguish native observation, public-path fault
injection, and legal custom-condition fixtures. No package implementation,
existing test, or generated documentation was changed.

The contract was [Internal SQL diagnostics](../design/specs/sql-audit.md),
[ADR 0027](../design/adr/0027-record-the-sql-marginplyr-sends.md),
and [ADR 0037](../design/adr/0037-keep-sql-diagnostics-local-to-marginplyr.md).
[ADR 0036](../design/adr/0036-accumulate-recorded-sql-across-calls.md) was
superseded proposal history rather than a requirement for this hunt.
Later collection, arbitrary callback SQL, driver-private SQL, and persistent
history remained outside its scope.

| Finding | Priority assessed on the investigation date | Strongest evidence |
| --- | --- | --- |
| An interrupted observer retained `invoking` after unwinding | P2 | Native SIGINT in two unmodified internal-observer runs; public SQLite fault injection at both vulnerable transitions |
| Diagnostic rendering swallowed compound interrupt conditions | P3 | Public SQLite off/on comparisons with a controlled first-render condition |

Companion scripts and logs are in
[2026-10-25-sql-audit-followup](2026-10-25-sql-audit-followup/).

## Interrupted status publication

`with_sent_query()` stored `invoking` before initializing its terminal-status
variable and registering its exit update. Its exit update also ran without an
interrupt shield. A cancellation between publication and registration could
therefore leave the published state without a cleanup handler; cancellation
during the terminal update could leave the same state. The owning operation
had unwound, while the accessor still presented an invocation in progress.
The specification required an unmatched invocation to become `unknown` after
unwinding rather than remain `invoking`.

Two fresh R processes reproduced the stale state with native SIGINT at the
unmodified internal observer. The fixture established a SQL recording scope
through `inspect_grouping()`, then called the internal observer with a literal
query and a synthetic scalar operation. Between attempts it cleared the saved
rows. It replaced no package function or recording binding. A suspended region
contained setup and post-call inspection, with interrupt delivery enabled only
around the observer call.

| Native run | Attempts at detection | Caught interrupts | State after the interrupted call |
| --- | ---: | ---: | --- |
| First process | 497 | 26 | `invoking`; no row snapshot retained |
| Second process | 831 | 44 | `invoking`, zero condition summaries in the retained row snapshot |

The saved second run's absence of condition summaries was consistent with
cancellation before the invocation observer was installed, but the native runs alone did not
identify the precise instruction interrupted. A faster signal stream had
mostly interrupted initial row allocation: 45,086 attempts and 44,982 caught
interrupts produced no stale state. That run did not establish safety at the
later transition.

```sh
Rscript investigation/2026-10-25-sql-audit-followup/native-status.R
```

Exit 1 reports the reproduced invariant violation. Exit 2 reports an
inconclusive bounded run. The retained script uses the lower signal frequency
that reached later transitions. It targets only its invoking disposable R
process and terminates and reaps its sender.

The internal fixture ran outside a live public entry frame, so it was not
presented as native reproduction through the complete public API. Its status
update code did not depend on that distinction. A separate public SQLite
label-check fixture installed a temporary active setter on the private rows
binding during diagnostic rendering. It injected one synthetic interrupt at
either the first `invoking` store or just before the terminal store. Both
cases retained `invoking` after the public call unwound, with the original
interrupt caught and no condition summaries:

```sh
Rscript investigation/2026-10-25-sql-audit-followup/status-window.R entry_after_store
Rscript investigation/2026-10-25-sql-audit-followup/status-window.R exit_before_store
```

These deterministic runs exited 1 at their intended invariant failure. They
were controlled transition experiments, not measurements of ordinary user
interrupt frequency. Unmodified public SQLite native runs of 1,663 attempts /
1,662 caught interrupts and 578 attempts / 577 caught interrupts did not
reproduce the stale status. The latter retained a `returned` label-count row
in 575 cases because interruption arrived later during result construction.
A longer public run also found no stale status in 5,558 attempts / 5,557 caught
interrupts; 5,513 of those attempts retained a returned label-count row.
Consequently, no unmodified full-public-API native reproduction was established.

The earlier ownership fix kept a later independent call from inheriting an
abandoned owner. This finding concerned the completed call's row state, not a
reappearance of that ownership failure. The two publication/update windows
were one repair slice, not two independently counted findings.

## Compound interrupts during diagnostic rendering

`render_sent_query_sql()` still used separate warning and interrupt calling
handlers with an outer error catcher. The warning handler invoked its local
`muffleWarning` restart even when the condition also inherited from interrupt.
For an interrupt/error condition, the interrupt handler first recorded
`propagated`, then the outer error catcher recorded `handled` and returned
missing SQL. Both routes allowed construction to continue after cancellation.

The public SQLite reproduction replaced only the renderer boundary in a
disposable process. The first render raised a single controlled condition;
subsequent renders used the original implementation. With auditing off, that
first render belonged to the real label-check invocation. With auditing on,
it belonged to diagnostic rendering. Both used the same synthetic source and
public call.

| Condition classes | Audit off | Audit on | Stored render summaries with audit on |
| --- | --- | --- | --- |
| `warning`, `interrupt`, `condition` | Original interrupt escaped | Public call returned | One `suppressed` summary |
| `interrupt`, `error`, `condition` | Original interrupt escaped | Public call returned | Two summaries for the one signal: `propagated`, then `handled` |

The audited label-count row became `returned` and the result row `prepared`.
The swallowed interrupt thus changed control flow as well as the diagnosis;
the duplicate summary was another symptom of the same render-classification
gap. The invocation-side compound-condition fix in the inspected snapshot did
not cover this rendering path.

```sh
Rscript investigation/2026-10-25-sql-audit-followup/render-compound.R off warning_interrupt
Rscript investigation/2026-10-25-sql-audit-followup/render-compound.R on warning_interrupt
Rscript investigation/2026-10-25-sql-audit-followup/render-compound.R off error_interrupt
Rscript investigation/2026-10-25-sql-audit-followup/render-compound.R on error_interrupt
```

The off cases exited 0 after observing the expected interruption. Each on case
exited 1 because the public call returned instead. These were legal custom R
conditions, not evidence that a standard database driver normally emitted
overlapping severity classes; that limited the priority assessment to P3.

## Passing controls

R was 4.6.1 on arm64 macOS. Dependencies were dplyr 1.2.1, dbplyr 2.6.0,
DBI 1.3.0, RSQLite 3.53.3, DuckDB 1.5.5, RPostgres 1.4.10, rlang 1.3.0,
lifecycle 1.0.5, testthat 3.3.2, and pkgload 1.5.3. Three agents examined
rendering, lifetime, and backend operations independently; the coordinating
process ran focused tests, challenged findings, and repeated counterexamples.

The working tree was installed into a disposable library before the focused
suite, so subprocess tests used the same snapshot as the parent:

```sh
audit_library=$(mktemp -d)
R CMD INSTALL --library="$audit_library" .
R_LIBS="$audit_library" Rscript -e \
  'devtools::test(filter = "sent-quer|sql-audit", reporter = "summary", stop_on_failure = TRUE)'
```

The original run used `/tmp/marginplyr-sql-hunt-v2-library`; the commands above
allocate the destination for a fresh replay.

All 12 focused files passed, with 941 passing expectation markers and no
failures, warnings, or skips. This was focused feature verification, not the
repository's full Review-ready check.

| Additional control | Observed result |
| --- | --- |
| Live SQLite, DuckDB, and PostgreSQL 17 | All 34 audit off/on comparisons agreed on DBI-boundary SQL, lazy result class/variables, inspection column contents, escaping error class/message, and caller-visible warnings |
| Recorded internal SQL versus DBI SQL | All 34 matched, normalizing generated savepoint names and removing PostgreSQL generic-forwarding duplicates |
| PostgreSQL caller transaction | Transaction remained active and a subsequent `SELECT 9` succeeded |
| PostgreSQL control failures | All 24 fixture comparisons agreed: four control positions, error/warning/interrupt, and `warn = 0`/`2` |
| Public lifetime matrix | 576 combinations passed; 480 executed the nested grouping callback, across six entry spellings, SQL/local inputs, three nested calls, and option changes |
| Early input forcing | All 24 failure/active-binding checks passed |
| Saved views | Stable over 30 record replacements and two copies read during active rendering |
| Retention | Weak references to a call holder, connection holder, and SQLite external pointer all cleared after GC |

The live comparisons covered inspection, typed selection, successful and
colliding observed-label checks, missing labels, no checked columns, expansion,
summarization, cold dialect questions, cached questions, and local inputs.
They compared generic DBI crossings, not network submissions. They did not
compare collected numerical result values or inspection tibble attributes.
Captured synthetic observations were retained as the six `*-off.rds` /
`*-on.rds` files; `compare-live.R` and
`check-coverage.R` replayed their assertions without a running server. The
temporary PostgreSQL server was stopped.

## Unconfirmed candidates

Diagnostic-render warning options were temporarily changed before their
restoration handler was registered. A suspected cancellation leak of
`rlib_warning_verbosity` or `lifecycle_verbosity` was not reproduced in three
lower-frequency public native runs totaling 5,352 attempts and 5,043 caught
interrupts. It was not counted as another defect. An independent stress run
exited outside its comparison window and supplied no conclusion about leaks.

The SQL Server path's `db_sql_render()` preprocessing differed from the
diagnostic `sql_render()` entry. No incorrect record was reproduced, and the
contract promised a prepared representation rather than exact DBI arguments;
the code difference alone was not a finding.

## Lessons for another bug hunt

The contract inventory separated owned internal work from later collection
and driver-private statements before any missing row was treated as a defect.
Independent rendering, lifetime, and backend investigations reduced overlap;
the coordinating process challenged each finding against that same boundary.

Fresh R processes separated audit off/on comparisons when warning frequency,
probe caches, or first-use initialization could affect the next call. Installing
the inspected tree into a disposable library also kept subprocess tests on
the parent's snapshot. Neither loading the tree only in the parent nor
installing without propagating the library path supplied that guarantee.

The strongest status evidence combined a deterministic transition fixture with
native reproduction at an unchanged observer. Neither was relabelled as an
unmodified public-path reproduction. Lower-frequency interrupts reached states
that the faster stream missed; increasing the interrupt count alone did not
increase useful coverage. Interrupted harness setup or delivery outside the
comparison window supplied no verdict about package state.

Saved observations made the SQL comparison independently replayable. Generic
DBI forwarding was accounted for before comparing counts or statement order;
matching those boundaries was not promoted to a network-submission guarantee.
The rejected options and SQL Server candidates remained separate from confirmed
findings, so a later investigator could reuse the attempted controls without
inheriting an unsupported bug claim.

The retained scripts were diagnostic fixtures outside the source package.
Artifact preparation added condition-identity checks, portable internal lookups,
and an explicit public-stress detection flag; it did not repair package code.
The companion README distinguishes the retained replay forms from the historical
run variants and maps each conclusion to its evidence.
