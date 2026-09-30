# Near-floor public results on live PostgreSQL

Investigated: 2026-09-30
Revised: 2026-09-30 — `investigation/rds-text-conversion-2026-09-30.md`
Source: issue #745; package commit `032690964f6c94a1430c3c094968309bd6ee17eb`
Host: macOS arm64; PostgreSQL 17.11 (Homebrew)

## Result

**No violation found** in the exercised public calls. A near-floor R 4.5.2
graph and a current-dependency R 4.5.2 graph returned identical Grouping plans,
summaries, Parent/Total shares, and expansions from live PostgreSQL. A separate
R 4.6.1 current-dependency control also matched. Each configuration used the
same marginplyr source tarball, SHA-256
`195b0855b3c1991ee5085bbc44d7fe42f6dcc8bd6d713e24e7a39189d9209acc`,
built from the named commit and installed into a separate case library. The
[`source-archives.csv`](2026-09-30-postgres-near-floor/source-archives.csv)
records the driver and transitive source hashes too. The installation logs
identify the tarball installed in each case.

The server ran in a newly initialized disposable cluster bound only to a Unix
socket under `/private/tmp/marginplyr-745-20260930`. The probe created only
temporary tables, a view, a sequence, and a function on its own connection.
No existing database, normal R library, or permanent CI configuration was
modified. All three R processes restricted `.libPaths()` to the case overlay,
its existing isolated dependency library, and the matching R standard library.

## Configurations and graph checks

| Case | R | Target versions | Installed graph |
| --- | --- | --- | --- |
| near | 4.5.2 | dplyr 1.2.0; dbplyr 2.6.0; cli 3.6.2; glue 1.6.2; rlang 1.1.7; tidyselect 1.2.1; vctrs 0.7.1 | 37 packages; 185 declared constraints; 0 violations |
| current-r45 | 4.5.2 | dplyr 1.2.1; dbplyr 2.6.0; cli 3.6.6; glue 1.8.1; rlang 1.3.0; tidyselect 1.2.1; vctrs 0.7.3 | 37 packages; 185 declared constraints; 0 violations |
| current-r46 | 4.6.1 | the same targeted versions as current-r45 | 39 packages; 199 declared constraints; 0 violations |

All three overlays installed RPostgres 1.4.10, hms 1.1.4, timechange 0.4.0,
and lubridate 1.9.5 from the same four source archives. Those packages were
added without upgrading the targeted core packages. The probe asserted the
loaded target versions before connecting. Each case's `installed-manifest.csv`
and `dependency-constraints.csv` record the full selected package graph,
actual versions, library paths, and every Depends/Imports/LinkingTo constraint;
`loaded-manifest.csv` records the namespaces actually loaded in its fresh
process. The exact differences from the same-R control are in
[`version-differences-current-r45.csv`](2026-09-30-postgres-near-floor/version-differences-current-r45.csv):
only cli, dplyr, glue, rlang, and vctrs differed. The cross-R graph difference
is recorded separately in `version-differences-current-r46.csv`.

## Public calls and observed values

The [probe](2026-09-30-postgres-near-floor/probe.R) used five rows across two
fixed `period` partitions, including a missing `store`. It inspected
`rollup(region, store)` and called `summarize_with_margins()` with an ordinary
sum and with `share_of_parent()` plus `share_of_total()`. It also called
`expand_with_margins()`. Both Margin verbs used `.by = period`, `.id = "set_id"`,
and `.sort = "last"`. The probe asserted the exact eleven sorted summary
rows, grouping-set identifiers, totals, and independently calculated shares;
it asserted fifteen expanded rows, five per set. The rendered summary and
share SQL used native `GROUPING SETS` without `UNION ALL`; expansion used its
supported `UNION ALL` path. The saved SQL and `last_sent_queries()` records are
under each case directory.

For all three lazy results, `collect()` and direct `compute()` followed by
`collect()` returned identical objects within each case. The
[`comparison-r45.csv`](2026-09-30-postgres-near-floor/comparison-r45.csv) and
[`comparison-r46.csv`](2026-09-30-postgres-near-floor/comparison-r46.csv)
compare seven objects each: the Grouping plan and both retrieval paths for
summary, shares, and expansion. Every comparison matched complete values,
column names, row counts, `typeof()` per column, and column classes. The
summary and shares each had eleven rows, expansion fifteen, and inspection
three. The collected integer aggregate used `bit64::integer64` in every case;
share columns were doubles, and expanded source values remained integers.

To observe premature reads, the source was a temporary view with a volatile
PostgreSQL function in its row predicate and value expression. The function
incremented a separate sequence whenever a source row was evaluated. The
[`construction-reads.csv`](2026-09-30-postgres-near-floor/near/construction-reads.csv)
records zero reads after constructing the source, inspecting the plan, and
building/rendering each lazy result. After the requested collection and
materialization, each case recorded 160 function calls. The sent-query record
contained only synthetic dialect probes and result SQL during construction;
no unrequested source-row query was observed.

The first harness assertion used `as.numeric()` on an `integer64` sequence
value and read its underlying bits as a tiny floating-point number. A fresh
probe changed that conversion to `as.numeric(as.character(...))`; the final
counter was 160 in every case. That was a **harness-only failure**, with no
product result difference. PostgreSQL initialization initially met a sandbox
shared-memory denial; initialization and R connections succeeded outside that
sandbox against the same disposable path. No marginplyr compatibility bug,
declaration/guard mismatch, upstream difference, or invalid dependency graph
was found by these calls, so there is no failing public call to minimize.

## Evidence and limits

The adjacent [artifact directory](2026-09-30-postgres-near-floor/) retains the
exact [commands](2026-09-30-postgres-near-floor/commands.md), package and
source manifests, installation and execution logs, rendered SQL, collected
results, loaded locations, constraint audits, and comparison CSVs. The
repository development suite was run once with `devtools::test()` and reported
21,741 passes, zero failures, warnings, or skips; its log is preserved there.

This establishes only PostgreSQL 17.11 on macOS arm64, the three recorded
graphs, and the five-row public-call sample. PostgreSQL on other versions or
operating systems, R 4.1–4.4, other satisfiable near-floor combinations, older
RPostgres or transitive-driver versions, and the full package suite under the
near-floor graph remain unverified. The R 4.6.1 comparison is corroborating
evidence; the R 4.5.2 comparison isolates the tested dependency differences
without an R-version change.

## Revisions (2026-09-30)

The [text-conversion investigation](rds-text-conversion-2026-09-30.md) established
that all three typed `values.rds` objects could be represented by decimal
`digits17` `.dput` files, preserving values, types and attributes under strict
`identical()` checks. The probe and comparison scripts write and read
`values.dput`; the probe also checks its round trip. The original RDS files and
scripts remain available in Git snapshot
`056bae562385e627d6eb4ca2410183d594c0960f`. The public-call assertions and the
recorded comparison CSVs were unchanged; no database experiment was rerun.
