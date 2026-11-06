# Live SQL numeric and public-name boundaries

Investigated: 2026-11-07
Repository snapshot: `e1dabaabb392a3791177e5c48e983f780c7c93ec`

This lane established no additional package defect. It compared Parent shares
and Total shares with their displayed denominator values under ADRs 0010,
0014 and 0017. It also checked valid public SQL identifier spellings against
the repaired generated-reference boundaries in ADRs 0031 and 0032.

## Numeric and staged-value comparisons

`sql-boundaries.R` completed 204 collections: 96 SQLite and 108 DuckDB.
Each backend crossed fixed/unfixed partitions, duplicate error/keep policies
and all three sort requests. The fixture contained positive, negative, zero
and missing measures and a missing fixed key. SQLite used eight source
expressions; DuckDB used nine. Sources included ordinary sums, one volatile
scalar, very large and very small scales, infinities and missing constants;
DuckDB additionally exercised NaN and a high-precision decimal sum.

The oracle recomputed both shares from the displayed source on the matching
Parent/Grand total set row within each fixed partition, applying missing and
zero-denominator rules and own-denominator one. All 204 matched at tolerance
1e-12. For SQL NaN, collected NaN and R missing values were compared as missing;
this did not establish SQL-side NaN storage normalization.

The script asserted every query's physical aggregation path from rendered SQL.
DuckDB error policy used native GROUPING SETS; keep policy with the requested
identifier forced portable aggregation on a plan without repeated occurrences.
SQLite used the portable path. SQLite's Converting dialect required the
documented explicit source-eligibility opt-out; its numeric fixtures were
constructed intentionally. DuckDB retained normal eligibility checking.

## Public names and upstream exclusions

`sql-names.R` crossed fourteen spellings, fixed-key/dimension/source/share/id
roles, SQLite/DuckDB and no sort versus Margin order last: 280 cases. Thirteen
spellings were supported in every role. All 260 such collections retained the
expected public column names/order and Total shares. They included Boolean
words, lexical pronoun spellings, data.table special names, quotes, a backtick,
embedded whitespace/newline, a SQL keyword and an internal-prefix spelling.

The remaining twenty cases used the positive dot-dot index spelling `..1`.
They reported the upstream tibble name restriction. The six independent
`sql-upstream-controls.R` checks established the same restriction on ordinary
dbplyr collection, grouping and output collection on both backends. Closed
#971 explicitly excluded unsupported positive dot-dot names; #1002 distinguished
them from supported signed/whitespace names. These refusals were not counted as
new package findings. The final name harness asserted their diagnostic fragment.

## Replay, setup exclusions and limits

Run each script by absolute path; the first two take a source-root path and
write results next to the retained script. The upstream controls need no
package source. The coordinator's sequential archive replay produced the
adjacent CSVs/logs and all three expected zero exit statuses. The script
digests and original handles are in the coordinator record.

Earlier exploratory attempts used an invalid conditional tidyselect symbol,
an incorrect expected identifier position and an injection wrapper that moved
a dynamically named `.data` output into formal argument matching. A temporary
syntax error was also corrected before the final invocation. None of those
harness failures was a package finding or part of the completed counts. An
initial upstream control stopped before collection and therefore did not yet
reach name repair; the retained controls explicitly collected their results.

R was 4.6.1 on arm64 macOS, with RSQLite 3.53.3, DuckDB 1.5.5, dbplyr 2.6.0,
dplyr 1.2.1 and pkgload 1.5.3. Connections were private in-memory databases and
were disconnected after each backend; DuckDB used one thread. The ordinary SQL
missing-value-removal warning was a backend diagnostic, not a package failure.

The numeric comparisons asserted ratios, not Margin row order, declared type
restoration, construction-time query counts, direct compute or later dplyr
composition. The name comparisons asserted public column order, not row order.
They did not cover SQL collations, arbitrary structured keys, every numeric
representation or dialect, hosted services, concurrency, interruption, other
versions or other platforms. Earlier notes supplied other finite controls;
their counts were not added here.
