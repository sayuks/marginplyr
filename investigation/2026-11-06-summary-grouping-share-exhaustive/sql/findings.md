# DuckDB collation and extreme finite share probes

Investigated: 2026-11-06
Repository snapshot: `9ffb24614a71512170298a3827bc11c80354e742`

No additional package defect was established by this lane. ADRs 0010 and
0017 supplied the displayed-denominator and numeric-value contracts. ADR
0010's database-equivalence amendment supplied the Parent matching boundary;
ADR 0020 supplied the construction-time execution boundary.

[collation-numeric-probe.R](collation-numeric-probe.R) crossed four DuckDB
string comparison relations (default, NOCASE, NOACCENT, and their composition),
eight scalar numeric aggregates, fixed/variable key placement, native/portable
aggregation, and all three Margin order requests. The fixture included
case/accent-equivalent spellings and a missing key. The aggregates included
ordinary sum/min/max/mean, very small and very large finite doubles, and two
decimal scales.

The portable path was selected with `.duplicates = "keep"` and `.id` on a
plan without repeated occurrences. The native path used `"error"`. Every
query's rendered SQL was checked for the presence or absence of native
`GROUP BY GROUPING SETS`; a label in a loop was not treated as path evidence.
The oracle asked DuckDB to compare displayed key spellings under the same
collation, then recomputed both shares from displayed denominator rows. It
therefore did not assume that R string equality describes database groups.

All **384 collections** matched both expected share columns at tolerance
1e-12. The [CSV](collation-numeric-results.csv),
[transcript](collation-numeric-probe.log), and
[environment](environment.txt) retained the evidence. The retained original
execution was `67264`, collected with terminal exit zero. DuckDB used a
temporary home and one thread, and its connection was disconnected on exit.

Two earlier exploratory invocations were harness setup work, not contract
evidence. The first used invalid sort strings and ended with exit one. The
second used an unrecognized aggregate-option spelling, which was an ordinary
summary rather than an adapter selector, and ended with exit zero. Neither
was counted above. The retained script corrected both issues and asserted
the actual adapter path before collection.

These probes did not assert Margin order, exact numeric storage classes,
construction-time query counts, or later materialization/composition. They
covered neither zero/negative denominators, empty input, non-finite
portability, SQLite/PostgreSQL collations, hosted services, other versions,
nor concurrent/interrupted execution. The preceding dated notes supplied
other finite controls; their counts were not added to this lane's count.

## Prior adapter-label audit

[adapter-label-audit.R](adapter-label-audit.R) was run from a disposable
archive of `547d77095c2923573216d002f50bef21778247f9`, the exact source named
by the preceding deep SQL note. It established that `.aggregate` was absent
from the public formals and became an ordinary output column. Rendered SQL
contained native GROUPING SETS for both `"auto"` and `"portable"` labels.
The [stdout record](adapter-label-audit.txt) retained both observations;
the execution ended with exit zero. This narrowed the older coverage claim
without disputing the recorded numeric results. Narrow dated revisions were
added to the earlier SQL lane and its overview; their original evidence was
left intact. The required full-path and basename citation searches found the
overview's relative lane link and no maintained decision depending on the
withdrawn adapter-coverage claim.
