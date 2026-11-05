# SQL summary and share boundary probes

Investigated: 2026-11-05
Tested SHA: `0403c9150394cbae21b59f34c75791d0873b8451`

## Hypotheses and results

The first hypothesis was that generated SQL key, source, share, or identifier
references could reinterpret unusual public names. `name-matrix.R` made 736
calls: 23 spellings, eight roles, two Margin orders, and live SQLite/DuckDB.
Each call requested both share kinds over a two-dimension rollup, with missing
fixed and variable keys and a negative summary contribution. Its oracle was
the eager result, with numeric storage normalized and unspecified row order
canonicalized by keys and Grouping set identifier. The 672 supported cases
matched values and public column positions; this comparison did not assert
Margin row order. There were 32 eager-input refusals and 32 SQL-output
refusals for `...` and `..1`. `validate-results.R` checked all 736 recorded
verdicts against that classification.

`dot-controls.R` established that ordinary dbplyr summarization also refused
those two output names. It also checked four accepted dot-shaped names from
closed [#971](https://github.com/sayuks/marginplyr/issues/971): `..0`, `..00`,
`..01`, and `..-1`. All eight backend/name pairs retained successful ordinary
summaries, ordered Margin summaries, and Total shares of .25/.75/1. These
observations supplied no new defect. The matrix used `.check_share_source =
FALSE` on both SQL backends to admit SQLite's converting-dialect contract.

The second hypothesis was that the DuckDB arithmetic source binding could
accept a nonnumeric semantic SQL type after the INTERVAL repair in
[#1229](https://github.com/sayuks/marginplyr/issues/1229).
`duckdb-arithmetic-types.R` directly tested the generated multiply/subtract
binding: VARCHAR, BOOLEAN, INTERVAL, INTEGER[], BIT, and UUID were refused;
INTEGER and DECIMAL(38,0) retained numeric types. This was a binding probe,
not an exhaustive public share-source test. Exploratory JSON and INET attempts
could not load their DuckDB extensions, so neither type was evaluated and no
conclusion was drawn about them. No extension was installed.

## Replay and limits

Run each R script from a source checkout of the tested SHA. `name-matrix.R`
wrote `name-results.csv`; the checked-in log captured its original execution.
`validate-results.R` verified the saved classification. The matrix log named
expected upstream refusals and was not a list of confirmed package bugs.

The environment was R 4.6.1 on aarch64 macOS, dplyr 1.2.1, dbplyr 2.6.0,
RSQLite 3.53.3, DuckDB 1.5.5, and DBI 1.3.0. The name matrix used in-memory
connections and `shared_home = FALSE` for DuckDB. Each connection was closed.
No hosted service, PostgreSQL, concurrent execution, or exhaustive SQL type
matrix was covered by this lane. Earlier dated probes remained evidence for
their own snapshots, not a pass for this one.
