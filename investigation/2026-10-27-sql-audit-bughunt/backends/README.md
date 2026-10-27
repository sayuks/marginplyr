# Portable backend replay for the SQL-audit bug hunt

Investigated: 2026-10-27

These scripts replayed the backend portion of the investigation at main commit
`13dd7c30ac38fe3fc82e74c561d8795a6c4b6d17`. They loaded the checkout in the
current working directory with `pkgload::load_all(".")`; no installed marginplyr
copy stood in for the investigated sources. They made no product edits.

## Run from the investigated repository root

The shell runner found `initdb`, `pg_ctl`, and `Rscript` on `PATH`, falling back
to `pg_config --bindir` for PostgreSQL binaries. It created a disposable UTF-8
cluster, listened only on a private Unix socket, ran each R script in a fresh
process, stopped PostgreSQL, and removed the cluster. EXIT/HUP/INT/TERM traps
covered ordinary failure and cancellation. As with any cleanup handler, SIGKILL
and process/host termination were outside that guarantee.

```sh
sh /path/to/harness/run-backends.sh /private/tmp/sql-audit-backend-replay 56138
```

Choose a fresh output directory for every run. The runner refused to overwrite
a directory containing its `capture-started.txt` marker. All three standalone
R scripts also accepted explicit connection and output arguments:

```sh
Rscript /path/to/harness/internal-matrix.R /path/to/postgres/socket 56138 /path/to/output
Rscript /path/to/harness/label-special-names.R /path/to/postgres/socket 56138 /path/to/output
Rscript /path/to/harness/internal-live.R /path/to/postgres/socket 56138 /path/to/output
```

Standalone invocation expected the PostgreSQL `postgres` database and the
configured/default RPostgres user to exist. The shell runner selected its own
cluster owner's OS username through `PGUSER`. The required R packages were
pkgload, DBI, dbplyr, dplyr, RSQLite, duckdb, RPostgres, and marginplyr's Imports.
No dependency installation was part of the replay.

## Captured evidence

`captured-20261027-portable-02/` was the run of these portable scripts. Earlier
absolute-path scratch scripts and their logs under `/private/tmp` were separate
exploratory captures, and were not inputs to this replay. Every RDS contained
its own UTC capture time, investigated Git commit, R/package versions, and
PostgreSQL server version.

- `internal-matrix.log` and `.rds`: 135 cases across SQLite, DuckDB, PostgreSQL;
  summarize, expand, inspect; normal, factor, empty, missing, Unicode-named
  inputs; and string, missing, absent Margin labels. The assertions required
  exact internal SQL agreement with `DBI::dbSendQuery`, returned status, and
  empty condition summaries. Two unused-factor-level collision refusals were
  expected public outcomes; they were not SQL failures.
- `label-special-names.log` and `.rds`: 216 construction cases across nine
  names, three backends, audit on/off, checking on/off, and summarize/expand.
  This matrix reproduced expected baseline failures as well as successes; it
  did not assert that every product call succeeded. `.env` with checking on
  reproduced #930, while `...`/`..1` met dplyr's existing name restriction.
  An additional 24 one-column controls compared `.env = "a"` with `.env =
  "Total"`, audit on/off, and checking on/off on all three live backends.
- `internal-live.log` and `.rds`: 16 live scenarios covering label checks,
  collision refusal, cold/warm share probes, and a cold PostgreSQL probe inside
  a caller transaction. All seven transaction-internal SQL rows matched
  `dbSendQuery`; the probe error was handled and the transaction stayed usable.

The RDS shapes were `list(metadata, cases)` for the ordinary matrix,
`list(metadata, cases, env_controls)` for special names, and
`list(metadata, cases, transaction)` for live scenarios. Cases retained class
vectors, plain diagnostic text, SQL strings, and accessor tibbles; they did not
serialize result queries or connections.

`backend-harness.R` registered connection cleanup before setup and protected
connection acquisition against interrupts. Capture scripts armed trace cleanup
before enabling a trace, and printed their untrace in the captured log. Only
`dbSendQuery` was traced: also tracing `dbSendStatement` double-counted a control
delegated through both DBI functions on the tested drivers. These were observed
R calls, not a claim of wire-level interception or general driver coverage.

The shell output also retained initdb/start/stop/server logs and the exact Git,
Rscript, and PostgreSQL versions. The temporary socket/cluster itself was not
retained.
