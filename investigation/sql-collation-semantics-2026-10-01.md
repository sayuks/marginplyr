# SQL collation investigation: successful controls and limits

Investigated: 2026-10-01
Target: `416f040a6ae2dbce378fab9ce08a265584e7a78b`

## Evidence retained here

This note records the broader coverage and negative results that would be lost
if only the confirmed bug ticket were retained. The Parent-share defect,
minimal reproduction, independent denominator SQL, diagnosis and repair
acceptance criteria were reserved for a separate GitHub Issue. They are not
duplicated here.
The observations below describe the investigated snapshot, not later behavior.

The prior [external-consumer investigation](downstream-integration-2026-10-01.md)
found no violation in 395 cases; its ordinary `a/a/b` fixture did not exercise
explicit nondefault column collations with distinct equivalent spellings.
The [PostgreSQL near-floor investigation](2026-09-30-postgres-near-floor.md)
examined dependency/public retrieval boundaries. This investigation added
explicit collation DDL, measured equivalence classes and per-source SQL oracles.
Existing share and SQLite typed-order tests informed the selected paths rather
than being treated as evidence that these collation combinations had run.

## Fixed environment and protocol

The starting tracked tree was clean. The committed source was archived,
built and installed without modification into an isolated library. The 39
copied dependency packages satisfied all 175 recorded dependency constraints;
loaded namespace paths were checked. Product internals, generated Margin SQL
and test helpers were not reused as expected-value logic.

| Component | Measured version/configuration |
|---|---|
| Host / R | macOS arm64 / 4.6.1 |
| dplyr / dbplyr / DBI | 1.2.1 / 2.6.0 / 1.3.0 |
| RSQLite / SQLite runtime and header | 3.53.3 / 3.53.3 |
| RPostgres / PostgreSQL | 1.4.10 / Homebrew 17.11 arm64 |
| PostgreSQL cluster | UTF8, locale C, private Unix socket, no TCP listener |
| PostgreSQL collations | libc C; ICU und-u-ks-level2, deterministic=false |
| ICU / recorded and actual collation version | 78.3.0 / 153.136 |

Every source had integer `rid` and `amount`; string columns used explicit
`TEXT COLLATE BINARY`, `NOCASE` or `RTRIM` in SQLite, and `text COLLATE "C"`
or `hunt.case_insensitive` in PostgreSQL. No `copy_to()` collation was assumed.
The ICU declaration was `CREATE COLLATION hunt.case_insensitive
(provider=icu, locale='und-u-ks-level2', deterministic=false)`. Catalogs and
stored bytes were checked before measurement. Actual ICU comparisons equated
A/a and É/é/decomposed é, but distinguished E/É. SQLite NOCASE was treated
as ASCII case equivalence, not general Unicode folding.

The discriminating inputs included A/a/B with amounts 2/5/11; A/A-space/B
with the same amounts; equivalent parent spellings in different children;
the same child keys in fixed partitions with different totals; NULL keys and
Total/total/Total-space label candidates; zero, negative and empty summaries.
The maximum source had five rows, two variable dimensions and four occurrences.

For each source shape, ordinary equality, GROUP BY, required joins and ORDER BY
were checked first. Expected grouping-set occurrences and Parent/Total
correspondence were defined from the public contracts (ADRs 0010 and 0017).
Separate ordinary GROUP BY statements read that same source for each set;
`rid` memberships established equivalence classes. R only checked the small
integer arithmetic. Output rows were mapped to DB equivalence classes while
preserving every row's multiplicity; no lowercasing, trimming, reaggregation
or distinct operation was used to hide differences. Representative spelling
and unspecified ties were allowed. Finite ratios used tolerance 1e-12.

Calibration accepted equivalent representative substitutions and rejected
inserted/deleted rows, group splits/merges, wrong occurrence membership and
missing/wrong denominators. Schema/types and structural first/last Margin
order were checked separately; none was compared as a multiset. Expansion
checks retained row IDs, occurrence copy counts and omitted-dimension values.

## Measured coverage and outcomes

Each configuration was retrieved by direct collect and supported direct
compute followed by collect. Calibration and upstream controls were additional
experiments, outside these counts. The defect observations refer to one root cause, not separate bugs.

| Database/collation | Configurations | Retrievals | No violation | Defect observations |
|---|---:|---:|---:|---:|
| SQLite BINARY | 62 | 124 | 124 | 0 |
| SQLite NOCASE | 62 | 124 | 98 | 26 |
| SQLite RTRIM | 62 | 124 | 98 | 26 |
| PostgreSQL C | 63 | 126 | 126 | 0 |
| PostgreSQL ICU as declared above | 67 | 134 | 134 | 0 |
| Total | 316 | 632 | 580 | 52 |

Aggregate-only checks covered a single grouping set, rollups, fixed keys,
multiple/composite dimensions, cube, count/sum/distinct count and duplicate
occurrences. Shares covered Parent, Total and both helpers. Retrieval checks
covered NULL/NA/character labels, id present/absent, none/first/last order,
summary and expansion. Inputs included source tables, select/rename, direct
column views and CAST/CASE/concatenation/explicit COLLATE derivatives. The
same key was used separately as a fixed key and variable dimension.
PostgreSQL native and portable paths were checked with aligned options.

All 632 observations passed the measured public-column/type and structural
Margin-order checks. No aggregate group split/merge, row multiplication or
Total-share value violation was observed. An independent second mapping of
raw keys against recorded DB equality confirmed all non-Parent comparison
columns. All 260 PostgreSQL retrievals matched their independent oracles.

Label collision checks rejected an equivalent-only Total label under the
matching DB equivalence and accepted non-equivalent controls; id-preserving
unchecked calls matched expectations. No additional contract question remained
in the adopted cases. SQLite ordinary dbplyr compute changed NOCASE/RTRIM
column metadata to BINARY; regrouping that ordinary computed input changed
parent counts from two to three. This matched SQLite CTAS behavior and was
classified as upstream behavior, without adding a metadata-retention promise.
Direct Margin retrieval values and correspondence were examined separately.

## Repeating the checks and limits

The bug ticket draft contained the self-contained SQLite reproduction, DDL,
stored-byte and equality probes, ordinary grouping/join oracle and BINARY
control, all rerun successfully against the investigated installation. For
broader rechecking, use the fixed source and versions above in a disposable
library/DB, declare each column's collation explicitly, reconstruct the small
fixtures and public workflow families above, calibrate the comparator, then
run both direct retrievals. Check ICU catalog availability and actual equality
before using that setting; record a concrete blocker and continue SQLite if
it cannot be established. This is a reconstruction protocol, not a claim
that the exact 316-case harness is available from this repository.

Only this note was retained in the repository after consolidation. The original
harness and bulk evidence were copied to the local isolated experiment area
`/private/tmp/mp-collation-4uesblmr/publication/unpublished-originals` before
removing their unpublished repository copies. That temporary archive is an
optional local aid, not a durable dependency of the note or the Issue.
Existing notes were not rewritten. DBs, binaries and large logs stayed outside
the repository. The dedicated PostgreSQL cluster was stopped. Normal R
libraries, existing databases and server-wide defaults were not changed.

The completed matrix left no execution blocker or unresolved classification.
Helper/catalog-recording errors in early attempts were corrected and the final
stable probes rerun; failed attempts were not counted as successful evidence.
Sandbox shared-memory/socket restrictions were resolved using authorized
execution of the same private cluster. The experiment remained below its
600-configuration, two-hour and 2-GiB limits (247 MiB before consolidation).

Untested: other dependency/driver/OS/DB versions or databases; custom SQLite
collations; other ICU locales/strengths or accent-insensitive settings;
arbitrary Unicode; three variable dimensions; the full option Cartesian
product; different index/scan choices; arbitrary mutate expressions;
regrouping a materialized Margin result; finite collection; these fixtures
inside an external consumer; unsupported SQL nesting. Unicode workflow
coverage was limited to the measured ICU setting. These were not reported
as passing cases or inferred guarantees.

Primary sources read: [SQLite Collating Sequences](https://www.sqlite.org/datatype3.html#collation),
[SQLite CREATE TABLE AS SELECT](https://www.sqlite.org/lang_createtable.html#section_2_1),
and [PostgreSQL 17 Collation Support](https://www.postgresql.org/docs/17/collation.html).
