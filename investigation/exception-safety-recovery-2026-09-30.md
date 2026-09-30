# Exception safety and recovery fault hunt

Investigated: 2026-09-30
Target HEAD: `535e7c87de1149bfcf9c90588c0952e217b33806`
Status: execution completed within verified boundaries; product fixes were not made

## Baseline and evidence

276 matrix runs completed; 256 calibrated runs were admitted across 27 case families, including 54 actual SIGINT runs. 20 completed runs were excluded as calibration/observation revisions. Six SQLite minimal reproductions, three diagnostic minimal reproductions, and ordinary DBI/dbplyr controls supplement the matrix. Run counts are evidence coverage, not finding counts.

The fixed Git archive was installed in a private library. R 4.6.1, DBI 1.3.0, RSQLite/SQLite 3.53.3, dbplyr 2.6.0, dplyr 1.2.1, rlang 1.3.0, and DuckDB 1.5.5 were used on Darwin arm64. The [evidence archive](exception-safety-recovery-2026-09-30/README.md), including `manifest.json`, `dependencies.csv`, and `loaded-environment.json`, record source identity and actual loaded paths. The repository remained clean.

All live-connection database observations preserved source and unrelated sentinel rows. Ordinary failures inside the protected materialization body restored the old destination, schema, and named index. These statements do not cover the intentionally invalidated connection or native SQLite-wide aborts.

`cases.csv` in the evidence archive links every completed matrix run to before/after state, conditions, handle validity, audit/options, recovery, active savepoint probes, and classification. Existing tests were read as evidence; they were not rerun or changed. The repository sources read were the root AGENTS.md/CONTEXT.md, public package documentation, the SQLite B-direct specification, and ADRs 0015, 0020, 0021, 0027, and 0031. The fixed [SQLite specification](https://github.com/sayuks/marginplyr/blob/535e7c87de1149bfcf9c90588c0952e217b33806/design/specs/sqlite-b-direct.md#savepoint-ownership-and-failure-behavior), [transaction ownership decision](https://github.com/sayuks/marginplyr/blob/535e7c87de1149bfcf9c90588c0952e217b33806/design/adr/0031-preserve-sqlite-typed-dimensions-under-margin-order.md#b-direct-amendment-2026-09-26), and [public Errors and warnings reference source](https://github.com/sayuks/marginplyr/blob/535e7c87de1149bfcf9c90588c0952e217b33806/R/marginplyr-package.R) were the contract evidence; this note did not create or resolve guarantees.

## Confirmed violation F1: ordinary acquisition failure leaves an owned savepoint

Evidence type: **controlled ordinary error after a real SAVEPOINT**, not a naturally observed backend failure. Three matrix repetitions and three minimal repetitions agreed.

The public seed was `summarize_with_margins()` on two valid rows, followed by direct `dplyr::compute()`. Its healthy result had totals 2, 5, and 7 in Margin order. Injection occurred at the successful `dbExecute(SAVEPOINT)` return, while named `dbBegin()` was still completing. The savepoint existed; no destination write had started. Real rollback/release implementations were unchanged.

After failure, input and the old destination remained intact, but the private savepoint remained. The minimized follow-up `DBI::dbBegin(con)` raised `cannot start a transaction within a transaction`. A later persistent compute succeeded on the same connection; a separate connection could not see its new table. Thus successful subsequent operations had acquired unexpected uncommitted transaction semantics.

ADR 0031, *B-direct amendment / Transaction ownership and audit scope*, requires failure rollback and release of the owned savepoint. The B-direct specification's *Savepoint ownership and failure behavior* requires ordinary failures to release owned work and permit later valid operations. Outermost successful release is supposed to complete atomic materialization.

`sqlite_with_compute_savepoint()` acquires its savepoint before installing its error handler. Dependency result cleanup does not release that savepoint. The observed acquisition exception bypassed the owned rollback path. The frequency of naturally occurring ordinary exceptions at this exact handoff was not measured; this finding is explicitly limited to the demonstrated controlled boundary.

Minimal reproduction: the archived `scripts/minimal.R.txt` (restore its original suffix in a disposable bundle), mode `error`. Results: `results/minimal--error--1/minimal.json` and repetitions 2-3. The script records healthy control, immediate state, next transaction, retry, and separate-connection visibility before disposal.

## Confirmed violation F2: warning replay loses a later External error

Evidence type: **public summary conditions and real warning conversion**. The minimized case uses no namespace tracing or cleanup mock. Three minimal repetitions agreed.

```r
summary_fun <- function(v) {
  if (length(v) > 1L) {
    stop(structure(
      list(message = "late failure", call = NULL),
      class = c("late_failure", "error", "condition")
    ))
  }
  warning("early warning", call. = FALSE)
  sum(v)
}
summarize_with_margins(
  data.frame(g = c("a", "b"), v = c(2, 5)),
  z = summary_fun(v), .grouping = rollup(g)
)
```

The detailed grouping-set branch completed and buffered warnings. The Grand total branch raised `late_failure`. The package exit handler then replayed a buffered warning through real `warning(cnd)`.

At `warn = 1`, the escaping dplyr error retained `late_failure` as its parent. At `warn = 2`, the escaping condition was a warning-derived `simpleError` with no parent: the later error's class, diagnostic, and cause were lost. Disabling only the warning source preserved the original cause even at `warn = 2`. Normal summary retry succeeded and the input data frame remained unchanged.

The public *Errors and warnings* contract and ADRs 0015/0021 promise preservation of External errors' class, diagnostic, and cause. No warning-as-error exception was found. `summarize_margin_union()` registers `report_branch_warnings()` with `on.exit()`; replay can raise during unwind and supersede the later error. R's runtime conversion explains the mechanism, while package-owned replay creates the contract violation.

Minimal reproduction and controls: the archived `scripts/diagnostic-minimal.R.txt`; results: `results/controls/diagnostic-minimal*.json`. The `local_replay` matrix independently recorded arrival at the late branch before its error and exit replay.

## Safety concerns requiring specification decisions

**Interrupted materialization:** controlled `rlang::interrupt()` and supervisor-delivered SIGINT agreed at the INSERT checkpoint. The pilot distinguished error and interrupt classes, proved successful INSERT effects, and observed the same process/connection before teardown.

Interrupts after DROP, CREATE, index, INSERT, ANALYZE, or final result-object preparation left the reached state and owned savepoint. After DROP the old destination was missing on that connection; after CREATE the replacement was empty; after INSERT it contained three result rows. Another connection still saw previously committed state. Healthy compute/retry succeeded but did not release the original savepoint. The minimized next transaction failed and subsequent persistent writes remained invisible externally.

Separate caller-ownership experiments omitted B retry before final commit/rollback, so retry overwrite could not hide the interrupted destination:

| Boundary / condition | Caller commit | Caller rollback |
| --- | --- | --- |
| CREATE / ordinary error | Old value 42 and old index retained; prior caller row committed | Old destination retained; prior caller row removed |
| CREATE / interrupt or SIGINT | Empty replacement persisted; prior caller row committed | Old destination/index restored; caller row removed |
| INSERT / ordinary error | Old destination retained; prior caller row committed | Old destination retained; caller row removed |
| INSERT / interrupt or SIGINT | Three-row replacement persisted; caller row committed | Old destination/index restored; caller row removed |

Immediate observations preceded caller commit/rollback. Test-side rollback was never evidence of package cleanup.

**Decision needed:** whether catchable interruption before successful release receives ordinary-failure restoration. The implementation specification names ordinary failures; the ADR uses broader failure wording. Until resolved, interruption residue is a demonstrated safety concern, not an unqualified confirmed interruption-contract violation.

**Cleanup/diagnostic precedence:** actual rollback-to restored the old table, but controlled interrupt or actual SIGINT before release left the savepoint and superseded the primary execution error. Buffered-warning replay under `warn = 2` also turned later interrupts into warning-derived errors. Without replay, the same late boundary propagated interrupt. Define interruption shielding, precedence, and causal reporting during unwind separately from F2's ordinary-error contract.

R interrupts differ from errors. DBI 1.3.0 added interrupt rollback to `dbWithTransaction()`, but this path calls named transaction methods instead, and its inner dbplyr materializer receives `in_transaction = FALSE`. [R condition handling](https://search.r-project.org/R/refmans/base/html/conditions.html), [DBI changelog](https://dbi.r-dbi.org/news/index.html), [DBI transactions](https://dbi.r-dbi.org/reference/transactions.html).

## Boundary evidence and ownership verdicts

Caller-owned state comprised input, old destination/indexes, connection, outer transaction, and user-expression side effects. Package-owned state comprised savepoints, temporary options, warning buffer, per-call audit record, and measured dialect cache. DBI/dbplyr/drivers owned result acquisition/finalization and backend transaction semantics.

| Adopted boundary | Protected state / residue | Classification |
| --- | --- | --- |
| Before savepoint acquisition | No savepoint or destination mutation; E/I propagated | No issue within verified scope |
| After acquisition | Ordinary error and I left savepoint; later transaction/persistence affected | F1 for E; specification concern for I |
| After DROP/CREATE/index/INSERT | E restored old data/schema/index; I retained partial work | Body E passed; interruption concern |
| After ANALYZE/final result preparation | E restored destination and removed new statistics; I retained reached state | Body E passed; interruption concern |
| After successful release | Outermost work persisted; owned savepoint absent; outer caller decision still worked | Expected completed-operation boundary |
| Actual UNIQUE violation | Old destination/index restored, retry succeeded | Existing ordinary-failure reference, not a new normal-input fault claim |
| Reader lock at release and cleanup release | Real rollback-to restored old table; release failed; causal lock error retained; savepoint remained | Expected cleanup failure/upstream limit |
| Invalidated connection | Real rollback failed with original failure as parent; no automatic reconnect | Expected invalid-resource limit |
| Between rollback-to/release | Data restored; savepoint remained; primary error superseded by interrupt | Specification concern |
| Collection acquisition handoff | Result remained valid until next ordinary query closed it; ordinary dbplyr behaved identically | Upstream handoff behavior |
| After fetch | Real registered cleanup invalidated handle; E/I propagated; retry succeeded | No issue within verified scope |
| Declared integer type repair | Empty expansion fetched; result already invalid; no DB mutation; retry succeeded | No issue within verified scope |
| Audit-render verbosity interval | Original value/absence restored; E creates allowed NA SQL; I propagates | No issue within verified scope |
| Lifecycle/preflight and automatic-name options | Temporary options restored for E/I/SIGINT | No issue within verified scope |
| Recorded collision query before fetch | Failed call retained its row; next call replaced attribution; handle invalid | No issue within verified scope |
| Real SQL warnings at warn=2 | Audit on/off outcomes matched; options restored; explicit NA-removal recovery succeeded | Expected warning conversion |
| Deprecated `.env` compute warning | Real savepoint rollback/release; old destination retained | No issue within verified scope |
| Share control-fetch failure | No unknown cache entry; healthy request reprobed; next request reused answer | No issue within verified scope |
| Failure after measured dialect write | Valid measured `refuses` retained and reused | No issue within verified scope |
| Later local branch / warning replay | Input/recovery preserved; original ordinary cause lost with replay at warn=2 | F2 / interruption-precedence concern |

Temporary, attached-schema, `.env`, and `in_transaction = TRUE` representatives agreed with core results. They are sensitivity checks, not a complete cross-product. The recorded cache/audit contracts were honored: records and valid measured cache entries were not unconditionally restored to pre-call state.

## Natural failures, controls, calibration problems, and limits

A real concurrent reader in rollback-journal mode with `busy_timeout = 0` caused both final RELEASE and cleanup RELEASE to raise `database is locked`. The old table was restored; the savepoint remained even after releasing only the reader lock and successfully running later operations. Plain named DBI transactions reproduced both release failures and residue. This was not counted as a package-only automatic-repair defect; the contract explicitly preserves causes without claiming completed cleanup.

The independent observer was itself blocked in the lock experiment. That unavailable read was recorded, not interpreted as an absent table. Same-connection observations completed before releasing the lock. Intentional connection invalidation was recorded before disconnect; automatic rollback on disconnect is upstream behavior and not proof of product cleanup.

Ordinary dbplyr collection reproduced valid-handle residue at acquisition handoff and invalid handles after fetch. Raw serialization of a query containing shared connection references changed while a live result existed. This was an observer limitation, not input corruption: direct query SQL and source structure were unchanged, and subsequent values were correct.

Calibration problems were retained and excluded: initial interrupt-message reporting assumed a non-null message; the first locked observer aborted rather than recording unavailability; early deferred-warning cases did not collect the result; early type-repair hooks missed the executing S3 registration and used a summary with a nonempty Grand total row. The corrected type-repair pilot used empty expansion, the actual S3 method table, and the real integer-conversion expression. Arrival was required before evidence admission.

SIGINT was delivered only after a flushed reached notification and PID/case match, to the newly launched worker. The same process performed immediate observations. This demonstrates actual R interruption at explicit checkpoints, not interruption inside SQLite native execution. No timing guess based on a fixed sleep was used.

Unverified: native in-statement cancellation and whole-transaction automatic abort; other OS/R/dependency versions; second interruption inside an interrupt handler; additional result-finalizer failures; long-query elapsed-time limits; server backend resource ownership. Arrow/dtplyr replicas were not added absent a distinct owned resource. OOM, SIGKILL, disk exhaustion, existing processes/databases, production faults, and environmental upgrades were excluded.

Successful release and persistent commit differ when a caller owns the outer transaction. Native SQLite interruption may cancel more than one statement and needs separate observation. [SQLite savepoints](https://www.sqlite.org/lang_savepoint.html), [SQLite transaction error handling](https://www.sqlite.org/lang_transaction.html).

## Replay and disposal

The scripts were archived as text evidence. Restore them in a new disposable bundle, provision the recorded private-library versions, and then run with an unused repetition number (see the archive README):

```sh
python3 scripts/manage.py insert_after sigint new 20
python3 scripts/manage.py begin_after error overwrite 20
python3 scripts/manage.py local_replay error new 20
python3 scripts/manage.py collect_repair interrupt new 20
```

The manager refuses existing run directories, owns the child process, observes independent persistence, requests disposal after result artifacts exist, waits for exit, and removes disposable SQLite/DuckDB files. No namespace reload, cache reset, reconnect, or test rollback precedes each product-state verdict. Active savepoint probes occur after passive observation/recovery and are omitted from caller commit/rollback cases.

The local experiment bundle retained the fixed source, private libraries, scripts, and evidence for replay; the committed archive retained scripts, source hashes, dependency versions, and JSON observations without binary libraries; the bundle remained below 500 MiB, fixtures used two rows, databases remained small, and no valid case reached its 60-second watchdog. Experiment databases/processes were removed. No product code, permanent tests, contracts, CI, commits, pushes, Issues, or PRs changed. Review-ready checks were not required because no package-affecting change was made.

The deprecated-cte cases set `warn = 2` in the harness after their initial snapshot; that intentional change is not an option-restoration failure. Final verification was recorded in the archive's `final-verification.json`; worker termination is verified through owned manager exit results and disposal markers. Independent `ps` inventory was unavailable under the sandbox.
