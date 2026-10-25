# SQL audit cancellation and resumption evidence runbook

Investigated: 2026-10-25
Repository snapshot: `b6f9faba946521aeb92fcf2c5655a2c3bf608ec8`

The [investigation note](../sql-audit-cancellation-and-resumption-2026-10-25.md)
records two defects, their independent confirmations, passing controls, and
rejected interpretations. This directory preserves diagnostic harnesses and
small synthetic captures. It is excluded from the source package; none of
these scripts repairs the package or adds a package regression test.

## Checkout and output selection

Run each probe in a fresh `Rscript` process. The commands below start at the
repository root. Root-level probes resolve the checkout from their script
location; `controls/` live probes require the repository root as the working
directory. They load that checkout with `pkgload::load_all()`. The recorded
snapshot and dependency versions describe the investigation, not a later
rerun. Use an isolated checkout and record its runtime for a historical
comparison.

Root-level probes print `RERUN_OUTPUT` and write into a new temporary directory
by default. Set `HUNT_OUTPUT_DIR` to a fresh directory to retain a rerun where
you choose. `reproduction-setup.R` rejects the historical `evidence/` directory
as an output destination. Controls take explicit fresh output filenames and
reject existing files and their retained `controls/evidence/` directory.
Keep new results separate from the dated captures.

Most probes require the development dependencies, including `pkgload`,
`dplyr`, `dbplyr`, and `rlang`. Public SQLite fixtures additionally require
`DBI`, `RSQLite`, and `testthat`; the warning-marker check also uses
`lifecycle`. Live controls require `DBI` and the selected driver, `RSQLite`
or `duckdb`. The copied-view and retention controls also use `testthat`.

A nonzero exit alone is not evidence of a defect. Confirm the expected
diagnostic before interpreting an exit: missing packages, invalid arguments,
or a setup error can also exit with code 1.

## Deterministic defect reproductions

| Probe | Boundary and expected result on the investigated snapshot | Retained observation |
| --- | --- | --- |
| `render-options-window.R` | Public SQLite observed-collision check. Temporary `trace()` injects one R interrupt after the renderer changes options and before restoration is registered. Asserts the original interrupt, `quiet/quiet` leakage, and zero versus two later warning markers. Intended exit 1. | `evidence/root/render-options-window.{log,rds}` |
| `resumed-exit.R` | Public SQLite collection-boundary double. Crosses synthetic and recoverable R interrupts with normal return or an unrelated restart. Return is `returned`; the escape incorrectly retains `interrupted` rather than `unknown`. Intended exit 1. | `evidence/root/resumed-exit-confirm.log` |
| `resumed-interrupt-controls.R` | Eight cells: presence of a resumable interrupt crossed with return, unrelated restart, terminal error, and terminal interrupt. Asserts that only cell six has the established misclassification. Intended exit 1. | `evidence/conditions/resumed-interrupt-controls.rds` |
| `compound-invocation-matrix.R` | Twelve off/on comparisons of overlapping severity classes. Asserts equivalent outcomes and one delivery of the original condition. Exit 0. The status-priority question was not accepted as a defect. | `evidence/conditions/compound-invocation-matrix.rds` |

```sh
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/render-options-window.R
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/resumed-exit.R
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/resumed-interrupt-controls.R
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/compound-invocation-matrix.R
```

These are baseline diagnostic fixtures. Some explicitly assert the known
failure shape, so a future repair may fail that assertion rather than turn
the script green. Compare the printed state and adapt the fixture deliberately;
do not use these dated scripts as an unattended post-fix regression suite.
The public fixtures establish behavior at controlled boundaries, not ordinary
driver frequency or unmodified public-path native cancellation.

## Bounded native experiments

Native probes use `parallel::mcparallel()` and require a Unix-like runtime
with fork support. They send SIGINT only to their own disposable parent R
process. The sender waits for a request, sends one signal, and acknowledges it
before the next attempt; the parent kills and reaps it during cleanup.

Exit 1 after `VIOLATION` and preserved evidence means a counterexample was
observed. Exit 2 after `INCONCLUSIVE` means the bounded run found none; it
does not establish safety. Exit 3 after `OUTSIDE-WINDOW interrupt` means
delivery escaped the comparison window and supplies no defect verdict.
Native counts are timing-dependent and need not repeat.

| Probe and arguments | Scope | Historical result and capture |
| --- | --- | --- |
| `native-options-valid.R internal 45` | Unchanged internal renderer, valid lazy query; no replaced package functions or bindings | 959 attempts / 959 caught; `quiet/quiet`; sender reaped, pending delivery check zero. `evidence/root/native-options-valid-internal.{log,rds}` |
| `native-options-ack-valid.R internal 40` | Independently prepared valid-query probe at the same unchanged renderer | 2,106 / 2,106; same leak and cleanup checks. `evidence/conditions/native-options-ack-valid-internal.{log,rds}` |
| `native-public-confirm.R public 45` | Complete public construction, auditing enabled by default; `AUDIT_MODE=off` disables it | Enabled 2,957 / 2,957; disabled 1,191 / 1,191. Both leaked only lifecycle verbosity. `evidence/root/native-public-{on,off}*` |
| `native-rlib.R public 50` | Complete public path, watches the rlib option separately and restores lifecycle-only leaks between attempts; `AUDIT_MODE=off` is the control | Enabled 2,487 / 2,486 in 50 seconds; disabled 2,212 / 2,211 in 45 seconds. Both inconclusive. `evidence/root/native-rlib-{on,off}*` |
| `native-options-opaque.R public 55` | Audited construction with a valid opaque selection bypassing the discarded name-only compilation pass | 4,845 / 4,844; inconclusive. `evidence/conditions/native-options-opaque-on-public*` |
| `native-rlang-options-ack.R internal 30` | Repeats only rlang's temporary-option helper inside the interruptible loop, without calling the audit renderer | 2 / 2; lifecycle-only leak, completed cleanup. `evidence/conditions/native-rlang-options-ack*` |

```sh
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/native-options-valid.R internal 45
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/native-options-ack-valid.R internal 40
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/native-public-confirm.R public 45
AUDIT_MODE=off Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/native-public-confirm.R public 40
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/native-options-opaque.R public 55
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/native-rlang-options-ack.R internal 30
```

The internal native positives do not establish an unmodified public-path
audit-specific native reproduction. The public lifecycle-only leaks also
occurred with auditing off; the separate preflight/helper path was the reason
not to attribute them to the renderer. Their warning-marker counts were one
versus two, unlike the zero-versus-two renderer transition fixture.

Two public-off stack-capture variants were retained only as historical logs
and inconclusive RDS under `evidence/conditions/native-options-stack*`.
Their 13,125 / 13,124 and 4,852 / 4,851 observations are not outputs of the
opaque-selection replay script. `native-public-confirm.R` and `native-rlib.R`
accept only `public`; their exploratory literal-query internal branch supplied
no valid-query evidence and is disabled.

## Offline comparison of saved controls

These commands inspect the retained captures without loading marginplyr or
opening a database. The live-value comparator uses dplyr to normalize only
the cases whose row order was unspecified; the other two use base R.

```sh
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/controls/compare.R
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/controls/compare-simulators.R
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/controls/validate-captures.R
```

All three should exit 0. They assert 222 SQLite/DuckDB off/on pairs, 128
simulated-dialect pairs, and ten transaction/failure pairs. The capture
validator also checks that the seven RDS contain no environments, pointers,
functions, or weak references, checks a small set of text markers, and reads
the historical focused log for 14 files and 1,094 expectation markers.
Those object/text checks are bounded checks over synthetic artifacts, not
a general credential detector.

The live comparator checks values and their attributes, result classes,
escaping conditions, SQL at DBI generic crossings, and the corresponding
recorded internal SQL. Those crossings are not network-send proof.
`controls/evidence/portability-validation.log` records checks against both
retained and separately generated captures, plus output-overwrite refusals.

## Fresh control captures

Create an output directory, then run each audit setting in a separate process:

```sh
audit_capture=$(mktemp -d)
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/controls/differential.R sqlite off "$audit_capture/sqlite-off.rds"
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/controls/differential.R sqlite on "$audit_capture/sqlite-on.rds"
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/controls/differential.R duckdb off "$audit_capture/duckdb-off.rds"
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/controls/differential.R duckdb on "$audit_capture/duckdb-on.rds"
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/controls/simulators.R off "$audit_capture/sim-off.rds"
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/controls/simulators.R on "$audit_capture/sim-on.rds"
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/controls/transaction-failures.R "$audit_capture/transaction-failures.rds"
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/controls/compare.R "$audit_capture"
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/controls/compare-simulators.R "$audit_capture"
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/controls/validate-captures.R "$audit_capture"
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/controls/matrix.R "$audit_capture/matrix.log"
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/controls/storage-extra.R "$audit_capture/storage.log"
```

SQLite uses in-memory databases. DuckDB uses disposable connections with
`shared_home = FALSE`. Successful capture/control commands exit 0.
`matrix.R` asserts 80 public lifetime cases. `storage-extra.R` asserts nine
formatter branches, 45 cleared weak references, 405 replacements, 11 inactive
settings, 26 invalid accessor calls, and two independent active snapshots.
Their preservation-time results are in
`controls/evidence/matrix-portable.log` and
`controls/evidence/storage-extra-portable.log`.

For focused tests that start child R processes, first install the selected
checkout into a disposable library and propagate it:

```sh
audit_library=$(mktemp -d)
R CMD INSTALL --library="$audit_library" .
R_LIBS="$audit_library" Rscript -e 'testthat::test_local(filter="sent-quer|sql-audit", reporter="summary")'
```

The investigation's successful install and focused results are in
`controls/evidence/install.log` and `controls/evidence/focused.log`.
The initial stale-installed-package failure is explained in the note; that
failed run is not substituted for the successful snapshot-aligned evidence.

## Provenance and later reuse

`evidence-manifest.tsv` maps each finding artifact to its original path within
the temporary hunt directory and its SHA256. `controls/MANIFEST.tsv` records
sizes, hashes, and origins of the controls, including separately labelled
preservation-time validation logs. Historical logs retain the paths printed
by their original runs. In particular, the independent valid-query native log
names a generic RDS destination; the manifest identifies the matched
2,106-attempt observation saved under the distinct `ack-valid` name.

The replay scripts changed checkout/output selection, made the accepted
failure predicates explicit, and added braces required by lint. Historical
log/RDS bytes were preserved. The compound-class replay retains its original
speculative status comparison as `historical_probe` without treating it as
a specification assertion. Use new outputs for later investigations instead
of editing the dated captures to resemble a later runtime.
