# SQL audit investigation replay

Investigated: 2026-10-28
Repository snapshot: `ac6ad79ff6026abe5fc45538d6dfefadf03a8ca3`

This directory accompanies [the investigation note](../sql-audit-bughunt-2026-10-28.md).
It contains synthetic fixtures and captured observations, with no package
implementation changes. Run commands from the checkout root. Scripts load
that checkout; the snapshot above identifies the tested code. Every command
below completed with exit 0 on that snapshot. These finite checks found no
new defect; they are not a proof of correctness for every backend or timing.

Create a fresh output directory:

```sh
audit_run=$(mktemp -d)
audit_scripts=investigation/2026-10-28-sql-audit-bughunt
```

## New condition and ownership matrices

```sh
Rscript "$audit_scripts/conditions/exit-matrix.R"
Rscript "$audit_scripts/conditions/severity-matrix.R"
Rscript "$audit_scripts/conditions/dbi-fetch-recovery.R"
Rscript "$audit_scripts/ownership/ownership-matrix.R" "$audit_run/ownership"
Rscript "$audit_scripts/ownership/process-boundaries.R" "$audit_run/processes"
```

Counts were 16 cleanup pairs, 2,304 severity pairs, 24 live DBI recovery pairs,
2,173 ownership assertions (including 36 valid getter pairs, 14 invalid flags,
and 510 snapshots at depth eight), and 31 process assertions. Process checks
need POSIX fork, callr, and permission to open a local PSOCK socket.
The condition fixtures use testthat for temporary backend-boundary injection;
see their [scope and limits](conditions/README.md).

The final severity script was hashed before and after its successful frozen
run; both hashes were
`d289ab3afeb253e1aff83e93c2b2853dbbb5b8d6d487977902401e94732b306e`.
An earlier pilot was excluded because its source changed while it ran.
The final severity log includes ten deliberate warning-delivery diagnostics.

## New live backend checks and saved-capture replay

```sh
sh "$audit_scripts/sites/run-sites.sh" "$audit_run/backends"
Rscript "$audit_scripts/sites/validate-captures.R" "$audit_run/backends"
Rscript "$audit_scripts/sites/collation-controls.R"
```

The runner requires `initdb` and `pg_ctl` on PATH, plus DBI, RSQLite, DuckDB,
RPostgres, and the package development dependencies. It creates and stops a
disposable PostgreSQL cluster on a private Unix socket with TCP disabled.
It does not use a caller database. See the
[backend scope](sites/README.md) for cleanup limits and observation boundaries.

The coordinator's retained `sites/capture/` passed 726 normal and 13 failure,
notice, timeout, and transaction pairs, with 776 internal DBI crossings per
audit setting. Three collation controls agreed with SQLite's comparison
relation. Unsorted collected results were compared as complete row multisets.
PostgreSQL server notices did not establish R-warning observation.

The retained RDS files can be checked without any running database:

```sh
Rscript "$audit_scripts/sites/validate-captures.R" "$audit_scripts/sites/capture"
```

## Existing controls rerun on the selected snapshot

```sh
Rscript investigation/2026-10-26-sql-audit-exhaustive/simulator-deep.R \
  off "$audit_run/sim-off.rds"
Rscript investigation/2026-10-26-sql-audit-exhaustive/simulator-deep.R \
  on "$audit_run/sim-on.rds"
Rscript investigation/2026-10-27-sql-audit-bughunt/compare-simulators.R "$audit_run"
Rscript investigation/2026-10-27-sql-audit-bughunt/differential.R
Rscript investigation/2026-10-27-sql-audit-bughunt/lifetime/nonsql-matrix.R "$audit_run/nonsql"
Rscript investigation/2026-10-27-sql-audit-bughunt/lifetime/arrow-reader.R "$audit_run/reader"
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/controls/storage-extra.R \
  "$audit_run/storage.log"
Rscript tools/check-sent-query-interrupt.R
```

Observed counts were 528 simulator pairs, 480 seeded SQLite/DuckDB pairs,
432 non-SQL pairs, 48 reader pairs, and the storage/copy counts in the note.
The native check caught 9,986 interrupts in 10,000 iterations. Its exit 0
establishes observed invariants; exit 1 reports failure and exit 2 is
inconclusive. Interrupt counts depend on scheduling. Arrow initialization
printed three sandbox CPU-cache diagnostics; both matrices exited 0.

## Focused suite and repository checks

Subprocess fixtures require an installed copy of the selected checkout:

```sh
mkdir "$audit_run/library"
R CMD INSTALL --library="$audit_run/library" .
R_LIBS="$audit_run/library" NOT_CRAN=true Rscript -e \
  'testthat::test_local(filter = "sent-quer|sql-audit", reporter = "summary")'
jarl check .
LINTR_ERROR_ON_LINT=true Rscript -e \
  'pkgload::load_all(".", quiet = TRUE); lintr::lint_package()'
Rscript .github/scripts/verify-context-budget.R
Rscript .github/scripts/verify-doc-references.R
```

The corrected focused run covered 18 files with no test failures, warnings,
or skips. Its expected `Interrupting test` line did not prevent assertions
from running. Only the corrected isolated-library run was retained as passing
evidence. All new files were investigation material under the existing source
package exclusion, so the Package-affecting Review-ready check was not required.
No CI run was monitored.

`MANIFEST.sha256` hashes the delivered scripts and evidence, excluding itself.
Run `shasum -a 256 -c MANIFEST.sha256` from this directory to verify it.
Text transcript trailing spaces were removed; RDS observations were unchanged.
