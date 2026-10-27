# SQL audit investigation replay

Investigated: 2026-10-27
Repository snapshot: `13dd7c30ac38fe3fc82e74c561d8795a6c4b6d17`

This directory accompanies [the investigation note](../sql-audit-bughunt-2026-10-27.md).
Run commands from the checkout root. Scripts load that checkout and do not
repair package behavior. Dependencies and versions are recorded in
`evidence/versions.txt`; backend and lifetime captures also retain session data.
Text transcripts have terminal trailing whitespace removed. RDS captures are
unchanged. `MANIFEST.sha256` hashes the delivered scripts and evidence.

Use a fresh workspace so an older capture cannot satisfy a replay:

```sh
audit_run=$(mktemp -d /private/tmp/marginplyr-audit-replay-XXXXXX)
audit_scripts=investigation/2026-10-27-sql-audit-bughunt
```

## Finding replay

```sh
Rscript "$audit_scripts/reproduce-label-env.R" "$audit_run/label-env.rds"
```

On the investigated snapshot this exits **1**, after verifying successful
unchecked, ordinary-name, and raw-SQL controls. Its desired public behavior
fails in four checked cases: a noncolliding `.env` dimension and a real
collision, each with auditing off/on. A successful repair should make the
command exit 0. The retained `evidence/label-env.log` and `label-env.rds` hold
the independent coordinating-process reproduction.

The backend special-name harness has a different purpose: it asserts the
investigated baseline, including the known failure. Its exit 0 does not mean
F1 was repaired. Its 216 matrix observations and 24 additional single-column
controls are retained under `backends/captured-20261027-portable-02/`.

## Differential and condition controls

```sh
Rscript "$audit_scripts/differential.R"
Rscript "$audit_scripts/compound-probe.R"
```

Expected exits are 0. The first compares 480 SQLite/DuckDB pairs with seed
20261027, with 288 successful collection pairs and 192 matching refusals or
collection errors on the snapshot. It compares row multisets, warning and
error diagnostics/classes, and post-collection record stability. The second
uses controlled collection conditions through the public SQLite share probe
and compares 48 pairs. A stock driver is not claimed to produce those classes.
Their final transcripts are retained under `evidence/`.

## Lifetime and non-SQL controls

```sh
Rscript "$audit_scripts/lifetime/lifetime-matrix.R" "$audit_run/lifetime"
Rscript "$audit_scripts/lifetime/arrow-reader.R" "$audit_run/reader"
Rscript "$audit_scripts/lifetime/nonsql-matrix.R" "$audit_run/nonsql"
Rscript "$audit_scripts/lifetime/nonsql-sql-parent.R" "$audit_run/sql-parent"
```

Each command requires a new output directory and exits 0. Counts are 31,
48, 432, and 216 respectively. The scripts are independent. Their retained
`run-*` directories contain summary RDS/session information; `run-nonsql`
also holds all 432 compared outcomes. The corresponding logs identify the
captured runs. The native SIGINT transcript is `lifetime/native-interrupt.log`;
the existing stress command is:

```sh
Rscript tools/check-sent-query-interrupt.R
```

That POSIX stress check exits 0 for the observed successful invariants, 1 for
a failure, or 2 when no conclusive window was observed. Its timing-dependent
interrupt count need not match the captured 9,975.

## Live backend SQL correspondence

```sh
sh "$audit_scripts/backends/run-backends.sh" "$audit_run/backends" 56138
```

The runner needs `initdb`, `pg_ctl`, and the R dependencies; it also looks in
`pg_config --bindir`. It creates a disposable Unix-socket-only PostgreSQL
cluster, runs SQLite/DuckDB/PostgreSQL captures, stops PostgreSQL, and removes
the cluster. The output directory is caller-selected. It asserts 135 internal
SQL correspondence cases, the 216 special-name baseline observations plus
24 minimal controls, and 16 live scenarios including a caller transaction.
See `backends/README.md` for direct Rscript arguments and capture schemas.
The final retained capture is `captured-20261027-portable-02/`; the first
exploratory capture was not included.

## Simulators

The unchanged harness from the prior investigation was rerun against this
snapshot. Generate fresh observations and compare them with:

```sh
Rscript investigation/2026-10-26-sql-audit-exhaustive/simulator-deep.R \
  off "$audit_run/sim-off.rds"
Rscript investigation/2026-10-26-sql-audit-exhaustive/simulator-deep.R \
  on "$audit_run/sim-on.rds"
Rscript "$audit_scripts/compare-simulators.R" "$audit_run"
```

All exit 0, comparing 528 pairs across 16 simulators. Fresh captures for this
snapshot and their comparison log are retained in `evidence/`.

## Focused package suite and repository checks

The subprocess fixtures need an installed copy of the investigated checkout,
not only a parent-process `load_all()`:

```sh
mkdir "$audit_run/library"
R CMD INSTALL --library="$audit_run/library" .
R_LIBS="$audit_run/library" NOT_CRAN=true Rscript -e \
  'testthat::test_local(filter = "sent-quer|sql-audit", reporter = "summary")'
```

The corrected retained run covered 18 files and exited 0. Its expected
`Interrupting test` notification did not prevent the relevant assertions from
running. The initial stale-install attempt was excluded from passing evidence.

The delivered change was repository-only investigation material under the
source-package exclusion. The package-affecting Review-ready check was not
required by `design/agents/local-checks.md`. Both linters and the applicable
repository verifiers were run before publishing:

```sh
jarl check .
LINTR_ERROR_ON_LINT=true Rscript -e \
  'pkgload::load_all(".", quiet = TRUE); lintr::lint_package()'
Rscript .github/scripts/verify-context-budget.R
Rscript .github/scripts/verify-doc-references.R
```

No CI run was monitored.
