# SQL audit follow-up evidence runbook

Investigated: 2026-10-26
Snapshot: `bfcdaf76a36d102132cd039c68ba8bc05a4fd465`

The [investigation note](../sql-audit-comprehensive-followup-2026-10-26.md)
states the conclusions, scope, and limitations. All inputs were synthetic.
Package implementation was unchanged. Commands below run from the checkout
root; generators load that checkout rather than a historical installation.
`MANIFEST.sha256` covers scripts and retained evidence.

## Reproduce the defect

These standalone public-path checks use pkgload, dplyr, dbplyr, DBI, and
RSQLite. They extend normal dbplyr translation and do not replace the package
recorder or renderer. An optional argument saves observations to a fresh RDS
path. The investigated snapshot exited 1 at the final transparency assertion
in each command, after asserting unchanged values, classes, and original
condition identity:

```sh
Rscript investigation/2026-10-26-sql-audit-exhaustive/generic-conditions.R
Rscript investigation/2026-10-26-sql-audit-exhaustive/formatter-generic-minimal.R
```

The first printed **1 off / 4 on**. The second printed **1 off / 5 on**;
its two original warning lines were expected. The coordinating process's
separate captures are named `independent-*` in `evidence/`.

`formatter-generic.R <fresh-output.rds>` crosses ordinary/bare original
warnings and four warning levels. It exited 1 after saving all eight
observations: outcomes agreed, while secondary general-condition delivery
differed. #891 tracks one repair covering both diagnostic sites.

## Replay saved observations without a database

```sh
Rscript investigation/2026-10-26-sql-audit-exhaustive/compare-conditions.R \
  investigation/2026-10-26-sql-audit-exhaustive/evidence
Rscript investigation/2026-10-26-sql-audit-exhaustive/compare-lifecycle.R \
  investigation/2026-10-26-sql-audit-exhaustive/evidence
Rscript investigation/2026-10-26-sql-audit-exhaustive/compare-deep.R \
  investigation/2026-10-26-sql-audit-exhaustive/evidence
```

All three exited 0. The first validates both the passing controls and the
captured defect; this is not a claim that the defect was repaired. The last
requires all three live backend pairs and the simulator pair, asserts exact
case counts, and optionally writes a summary only to an explicitly supplied
fresh second argument. It compares collected row multisets; it does not
assert an order within tied keys. PostgreSQL normalization is limited to
generated savepoint names and consecutive forwarding copies of savepoint
controls seen at the traced DBI generics.

## Generate fresh positive controls

Create a fresh output directory first and pass individual output filenames.
Generators refuse an existing RDS destination. In these examples the files
must not exist:

```sh
Rscript investigation/2026-10-26-sql-audit-exhaustive/render-matrix.R /tmp/fresh-render.rds
Rscript investigation/2026-10-26-sql-audit-exhaustive/lifecycle-matrix.R /tmp/fresh-lifecycle.rds
Rscript investigation/2026-10-26-sql-audit-exhaustive/resumption-retention.R /tmp/fresh-retention.rds
Rscript investigation/2026-10-26-sql-audit-exhaustive/deep-matrix.R sqlite off /tmp/fresh-sqlite-off.rds
Rscript investigation/2026-10-26-sql-audit-exhaustive/deep-matrix.R sqlite on /tmp/fresh-sqlite-on.rds
Rscript investigation/2026-10-26-sql-audit-exhaustive/simulator-deep.R off /tmp/fresh-sim-off.rds
Rscript investigation/2026-10-26-sql-audit-exhaustive/simulator-deep.R on /tmp/fresh-sim-on.rds
```

The rendering matrix's supported counted mode is S3, its default; an
exploratory S4 mode is not counted as formatter coverage. Expected primary
warnings are printed by the deliberately unmuffled `warn = 1` controls.
Lifecycle probes additionally need testthat for controlled collection
boundaries. Real-backend generators need the selected DBI driver. Use
`duckdb` in place of `sqlite` for its 102 scenarios. `cases.R` supplies shared
case definitions to the live and simulated generators.

For PostgreSQL, supply a disposable local `postgres` database using a Unix
socket and port as arguments four and five:

```text
Rscript .../deep-matrix.R postgres off <fresh-output.rds> <socket-directory> <port>
Rscript .../deep-matrix.R postgres on  <fresh-output.rds> <socket-directory> <port>
```

The investigated PostgreSQL 17.11 cluster was disposable and stopped after
the run. The extra scenario began a caller transaction, ran a cold share
probe, checked `SELECT 9`, and rolled back. No credentials or user data were
retained.

The live generator stored an exploratory `option_changes` field. It was a
heuristic, was excluded from comparison, and is not evidence of option-state
equivalence. The render/formatter controls separately asserted restoration of
their three selected warning options. The source inventory audit covered
owned SQL sites; the live matrix compared generic DBI calls rather than
driver-private statements or network traffic.

## Existing controls rerun

The existing lifetime and storage fixtures were rerun with fresh log paths:

```sh
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/controls/matrix.R /tmp/fresh-lifetime.log
Rscript investigation/2026-10-25-sql-audit-cancellation-resumption/controls/storage-extra.R /tmp/fresh-storage.log
```

Their captured logs are `lifetime.log` and `storage.log`. The repaired
ordinary/bare-message and malformed-option reproductions came from
`investigation/2026-10-26-sql-audit-extensions/`; their fresh logs are retained
under their original script stems.

The focused package suite first installed the checkout into an isolated
library and propagated that library to subprocesses:

```sh
audit_library=$(mktemp -d)
R CMD INSTALL --library="$audit_library" .
R_LIBS="$audit_library" NOT_CRAN=true Rscript -e \
  'testthat::test_local(filter = "sent-quer|sql-audit", reporter = "summary")'
```

It exited 0. The expected `Interrupting test` line belonged to its controlled
recovered-interrupt fixture. `focused-suite.log` retains the result. This
repository-only evidence change did not require the package Review-ready
check; both linters and the context-budget verifier were run separately.
