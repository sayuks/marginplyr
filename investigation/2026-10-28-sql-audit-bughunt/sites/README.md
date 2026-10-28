# Additional live SQL-audit backend checks

Investigated: 2026-10-28
Repository snapshot: `ac6ad79ff6026abe5fc45538d6dfefadf03a8ca3`

Run from the investigated marginplyr checkout root. R scripts load that checkout
with `pkgload::load_all('.')`; no installed marginplyr copy supplies its code.
The harness requires R/pkgload and the package dependencies, DBI, RSQLite,
duckdb, and RPostgres, plus `initdb` and `pg_ctl` on PATH. It creates a disposable
UTF-8 PostgreSQL cluster and a private Unix socket, with TCP listening disabled.

```sh
sh /path/to/sites/run-sites.sh /private/tmp/sql-audit-sites-fresh-output
Rscript /path/to/sites/validate-captures.R /private/tmp/sql-audit-sites-fresh-output
```

Use a fresh output directory. The runner refuses existing capture markers or
RDS files. EXIT/HUP/INT/TERM traps stop the server and remove its cluster; the
runner does not guarantee cleanup after SIGKILL or host/process failure.

The final observed capture is `capture/`. Retain its two RDS
files, two R logs, and `validation.log` for replay evidence; initdb/server logs
were setup diagnostics rather than package evidence. The RDS metadata stores
capture time, exact repository SHA, package/R versions, and server version.
The shared `backend-harness.R` was reused from the 2026-10-27 investigation.

- `sites-matrix.R`: 726 paired audit-off/on cases on SQLite, DuckDB, PostgreSQL.
  There are 35 grouping column names, summarize/expand/inspect, safe/colliding
  source values; 13 string labels with safe/colliding source values; and six
  SQL source types. Unsorted results are compared as row multisets by sorting
  complete row serializations, preserving column types. Both audit settings
  produce the same collected result or refusal, construction warnings, later
  collection warnings, and internal DBI SQL strings. The audited internal
  render strings equal the observed `dbSendQuery` arguments; internal rows
  return and compatibility result rows stay prepared. Collection leaves the
  construction record unchanged.
- `sites-failures.R`: 13 paired cases cover missing source tables, unsupported
  SQL functions, PostgreSQL server notices under four R `warn` settings,
  statement timeout, a cold probe in a read-only transaction, and a probe in
  an already aborted transaction. The RPostgres server WARNING text did not
  arrive as an R warning in the tested driver; empty R warning/condition
  capture here does not establish retention of server notices.
- `validate-captures.R`: independent assertions require all 249 planned
  collisions to produce the expected Package condition, all 477 other normal
  cases to succeed, all paired comparable fields to agree, and the recorded
  operation/status counts to match the observed capture.

`dbSendQuery` traces are R-side generic crossings, not server or wire-level
interception. These checks neither establish a performance bound nor verify
every private driver statement, every SQL type, or every backend. There was
no confirmed new defect in these checks.
