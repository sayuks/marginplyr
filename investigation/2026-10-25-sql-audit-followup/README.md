# SQL audit followup evidence runbook

Investigated: 2026-10-25
Repository snapshot: `0135a8615b8a6ce9d42de7fc75e94578a5d0d4cf`

The [investigation note](../sql-audit-followup-bug-hunt-2026-10-25.md)
contains the findings, their scope, and the environment versions. This directory
retains the supporting experiments and observations. It is excluded from the
source package; these scripts are investigation harnesses, not regression tests.

## Running and interpreting experiments

Run every command from the repository root, in a fresh `Rscript` process.
Harnesses that call `pkgload::load_all()` load the checkout present at execution
time. The snapshot above identifies the investigated package source; saving a
harness beside it does not pin a later execution to that snapshot or to the
recorded dependency versions. Use an isolated checkout and record the runtime
again when comparing a historical result with a later revision. Keep new output
outside `evidence/` so the dated observations remain intact.

Most runtime harnesses require the package's development dependencies, including
`pkgload`, `dplyr`, and `dbplyr`. The tables name additional direct dependencies.
The two offline comparators read the retained RDS captures without loading the
package or contacting a database. A nonzero exit alone is not a finding: confirm
that the console reached the intended invariant check rather than failing setup.

### Offline replay

| Script | Arguments and purpose | Expected exit and retained evidence |
| --- | --- | --- |
| `compare-live.R` | None. Compare all 34 retained audit off/on pairs, normalize generated savepoint identifiers, and check terminal states and SQL sources. | `0`, with 34 `PASS` lines; `evidence/live-comparison.log`. |
| `check-coverage.R` | None. Compare recorded internal SQL with retained DBI generic crossings; collapse PostgreSQL transaction-control forwarding duplicates. | `0`, with 34 `PASS` lines; `evidence/live-coverage.log`. |

```sh
Rscript investigation/2026-10-25-sql-audit-followup/compare-live.R
Rscript investigation/2026-10-25-sql-audit-followup/check-coverage.R
```

Both scripts read the six `evidence/{sqlite,duckdb,postgres}-{off,on}.rds`
captures at fixed paths. They replay assertions about those observations; they
do not rerun SQL or automatically compare newly generated captures. The live
comparison covered lazy result classes and variables, inspection column
contents, escaping error classes/messages, and caller-visible warnings. It did
not compare collected numerical results or inspection tibble attributes. DBI
generic crossings establish boundary observations, not network submissions.

### Controlled fixtures and public-path controls

| Script | Classification, arguments, and additional dependencies | Expected exit and retained evidence |
| --- | --- | --- |
| `status-window.R` | Public SQLite fault injection. Argument: `entry_after_store` or `exit_before_store` (default: the former). Requires `DBI`, `RSQLite`, `testthat`. A temporary active setter raises one interrupt at the selected state transition. | `1` on the investigated defect, after reporting the original interrupt and stale `invoking`; `evidence/status-entry.log`, `evidence/status-exit.log`. `0` means that stale state was absent. |
| `render-compound.R` | Public SQLite renderer fixture. Arguments: `off` or `on`, then `warning_interrupt` or `error_interrupt` (default: `warning_interrupt`). Also accepts exploratory `warning_error` and `all`. Requires `DBI`, `RSQLite`, `testthat`. Only the first renderer call raises the controlled condition. | For the two reported interrupt cases: off `0` after interruption; on `1` after unexpected normal return at the inspected snapshot. Logs: `evidence/render-{off,on}-{warning,error}.log`. |
| `probe-control-matrix.R` | Driver-boundary doubles; no live PostgreSQL server. No arguments. Requires `DBI`, `testthat` and the checkout's PostgreSQL probe test helpers. Crosses four transaction-control failure positions, three condition kinds, and two warning settings. | Normal completion is `0`; inspect all 24 `BEHAVIOR_IDENTICAL` results for `TRUE`, because the harness prints discrepancies without failing them. Logs: `evidence/pg-control-failures.log`, `evidence/probe-control-replay.log`. |
| `lifetime-matrix.R` | Public lifecycle control with local and in-memory SQLite inputs; no replaced bindings. No arguments. Requires `DBI`, `RSQLite`. Crosses entry spellings, nesting, input kinds, and audit-option changes. | `0`; 576 combinations, 480 executed outer callbacks. Logs: `evidence/lifetime-matrix.log`, `evidence/lifetime-replay.log`. |
| `promise-copy-retention.R` | Public early-input, copied-view, and weak-reference controls using in-memory SQLite. No arguments. Requires `DBI`, `RSQLite`, `rlang`. | `0`; 24 input checks, 30 record replacements, three cleared weak references. Log: `evidence/promise-copy-replay.log`. |
| `active-copy-translation.R` | Simulated SQL translator extension; no database or replaced implementation binding. No arguments. Reads and saves views during rendering, performs a nested public call, and signals a translation warning. | `0`; saved views remain stable during updates and replacement. Log: `evidence/active-copy-replay.log`. |

The finding commands and their expected symptoms are also shown in the
investigation note. Run off/on fixture cases in separate processes. Controlled
conditions establish the behavior at the exercised boundary; they do not show
that a standard driver emits those conditions in ordinary use. The probe
doubles establish control-flow equivalence, not live-server recovery after
native interruption.

### Live backend capture

`live-matrix.R` takes `BACKEND AUDIT OUTPUT [HOST PORT]`, where `BACKEND` is
`sqlite`, `duckdb`, or `postgres`, and `AUDIT` is `off` or `on`. It requires
`DBI` and the selected driver: `RSQLite`, `duckdb`, or `RPostgres`. SQLite uses
an in-memory database; DuckDB uses a disposable connection with
`shared_home = FALSE`. PostgreSQL requires an already running disposable server,
database `postgres`, and working connection authentication; the script does not
provision that server. Successful capture exits `0`.

```sh
audit_sqlite_off=$(mktemp)
audit_sqlite_on=$(mktemp)
audit_duckdb_off=$(mktemp)
audit_duckdb_on=$(mktemp)
Rscript investigation/2026-10-25-sql-audit-followup/live-matrix.R sqlite off "$audit_sqlite_off"
Rscript investigation/2026-10-25-sql-audit-followup/live-matrix.R sqlite on "$audit_sqlite_on"
Rscript investigation/2026-10-25-sql-audit-followup/live-matrix.R duckdb off "$audit_duckdb_off"
Rscript investigation/2026-10-25-sql-audit-followup/live-matrix.R duckdb on "$audit_duckdb_on"
```

For PostgreSQL, use the same command shape with `postgres` and append the
server's host and port. Run both audit settings separately. The retained
captures contain 11 cases per setting for SQLite and DuckDB, and 12 for
PostgreSQL. The extra PostgreSQL case checks a cold dialect question inside a
caller-owned transaction and a subsequent `SELECT 9`. Synthetic input data and
temporary tables were used. `evidence/backend-manifest.txt` summarizes the
captured boundary coverage.

### Bounded native interruption experiments

These scripts use `parallel::mcparallel()` and native signals, so they require
a Unix-like runtime with fork support. Each forks a sender targeting only its
invoking disposable R process, then terminates and reaps that sender. Run them
as standalone processes. Timing-dependent counts need not repeat; a bounded run
without a counterexample is inconclusive.

| Script | Arguments, scope, and additional dependencies | Exit interpretation and retained evidence |
| --- | --- | --- |
| `native-status.R` | None. Internal observer seam with a literal SQL value and scalar operation; no package function replacement. | `1` after detecting post-unwind `invoking`; `2` for an inconclusive bounded run. Logs: `evidence/native-status-run{1,2}.log`. Run 2 also retains `evidence/native-status-run2-rows.rds` and `evidence/native-status-run2-summary.log`; run 1 has no saved row snapshot. |
| `native-public-status.R` | None. Complete public SQLite label-check path, with no package-function or binding replacement. Requires `DBI`, `RSQLite`. | `1` means a detected invariant violation; `2` is inconclusive. Logs: `evidence/native-public-status-run{1,2,3}.log`; none reproduced the stale state. The retained source corresponds to the longer third run, not all historical timing variants. |
| `warning-options.R` | None. Public construction over simulated SQL, checking global warning-verbosity restoration after native interruption; no package-function replacement. | `1` means an observed option leak; `2` means no leak within the comparison window or delivery outside it. Logs: `evidence/warning-options-run{1,2,3,4}.log`. The retained lower-frequency source is not the high-frequency run 1 variant. |

The internal observer reproduction does not establish native reproduction
through the complete public API. Its saved rows, the public transition fixture,
and the unsuccessful full-public stress runs answer different questions; keep
their evidence levels separate. Likewise, the warning-option runs did not
establish absence of a cancellation leak.

## Reusing the investigation method

- Read the feature contract before proposing defects; distinguish preparation,
  invocation, later collection, and driver-private work.
- Divide independent searches by rendering, record lifetime, and backend
  operations, then independently challenge each proposed counterexample.
- Compare auditing off and on in fresh processes, including conditions,
  warning-to-error settings, cleanup, and surviving caller transactions.
- Install the inspected checkout into a disposable library before tests that
  start child R processes. A loaded parent namespace does not select the child
  process's installed package. The focused-suite setup and its historical
  result are recorded in the note and `evidence/focused-suite.log`.
- Use deterministic transition fixtures to locate a suspected window, then
  distinguish them from native signals and unmodified public-path evidence.
- Record negative results and failed harness assumptions. Faster signal
  delivery can repeatedly hit an earlier transition and never exercise the
  suspected later one.
- Save the snapshot, runtime, command variant, raw outcome, and smallest useful
  state alongside a finding. Preserve historical logs when improving a harness;
  do not attribute an old result to a later script variant.
- Report exactly what comparisons measured. Passing controls constrain a bug
  claim; they do not replace the full Review-ready check or prove the absence
  of untested failures.
