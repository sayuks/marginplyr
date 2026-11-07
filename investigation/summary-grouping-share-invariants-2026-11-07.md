# Margin summary, Grouping selection, and share invariants

Investigated: 2026-11-07
Repository snapshot: `db3a7e4f5215574a71a9af60698d0c945af0cb83`

The investigation started from fetched main in the new worktree
`/private/tmp/marginplyr-summary-grouping-share-invariants-20261107`. The
ordinary summary, Grouping selection, contextual-share dependency, and live
database comparisons ran in parallel during discovery. Freshly fetched main
still resolved to the tested snapshot before ticket publication. This
submission records observations and standalone replay scripts.

The November 7 [transfer](summary-grouping-share-transfer-bughunt-2026-11-07.md)
and [carrier](summary-grouping-share-carrier-bughunt-2026-11-07.md) hunts and
the November 6 [boundary](summary-grouping-share-exhaustive-bughunt-2026-11-06.md)
and [deep](summary-grouping-share-deep-bughunt-2026-11-06.md) hunts supplied
repaired controls, prior exclusions and the physical adapter checks. Their
historical results were kept separate from the executions below.

## Confirmed findings

| Boundary | Observation on the pinned snapshot | Issue |
| --- | --- | --- |
| Grouping context-name selection on dtplyr | Typed tidyselect chose b. A direct fixed-key selection chose a, giving two counts of one instead of one count of two. Direct dimension selection falsely refused overlap in all five constructors. | [#1439](https://github.com/sayuks/marginplyr/issues/1439) |
| Ordinary reads of special dtplyr keys | A rollup sum of the input column .N returned 2, 1, 3 instead of 20, 20, 40. Other special names read data.table's engine variables. | [#1440](https://github.com/sayuks/marginplyr/issues/1440) |
| Unnamed frame summary schema | Returning a frame with field detail in one grouping set and total in another exposed a bare internal union error with no condition call. A common field or a named packed summary succeeded. | [#1441](https://github.com/sayuks/marginplyr/issues/1441) |
| SQL share-source cardinality | A set-returning SQL expression yielded either zero result rows or two rows per grouping row. Both shares accepted the two-row source and used one of two values from a single Grand total set occurrence. | [#1442](https://github.com/sayuks/marginplyr/issues/1442) |
| Writes to known nested lists | Inner member replacements, deletion and renaming left stale closure identities, admitting prohibited dependencies or refusing legal independent reads. | [#1443](https://github.com/sayuks/marginplyr/issues/1443) |
| List replacement dispatch | A local replacement function left the list unchanged. Planning instead used the syntactic replacement to decide what the later read would invoke. | [#1444](https://github.com/sayuks/marginplyr/issues/1444) |
| Known lists crossing inline call frames | Supplied and forwarded actuals and visible list-returning factories lost invocation-time closure dependencies. | [#1445](https://github.com/sayuks/marginplyr/issues/1445) |
| Static plain-list indices | Literal negative and logical keys, a literal index concatenation, and an explicit drop option could select a known closure while planning lost its identity. | [#1446](https://github.com/sayuks/marginplyr/issues/1446) |
| First ordinary across beside contextual shares | The early selection used the input subclass's list view. It silently selected the wrong column in a one-set summary and refused valid rollups. | [#1447](https://github.com/sayuks/marginplyr/issues/1447) |

Each issue has a public acceptance boundary, the `bug` and
`ready-for-agent` labels, and no blockers. The independently verifiable
boundaries supported separate vertical slices. The two directions of an
individual share dependency failure stayed together, as did actual and return
carriers across the finite inline call boundary.

### Selection and ordinary summary controls

- [Grouping receiver](2026-11-07-summary-grouping-share-invariants/grouping-receiver.R)
  used an independently constructed zero-row data.table for typed tidyselect.
  The receiver's names method changed its answer only during active selection.
  Wrapping the same selector in identity chose b and exercised the typed path.
  The failing name-only proof discarded the lazy metadata receiver's class.
  Closed #1434 concerned eager subclasses. Closed #1418 and #1419 concerned
  constructing typed metadata through registered bracket and conversion
  methods. This witness only changed the names method.
- [Special keys](2026-11-07-summary-grouping-share-invariants/summary-special-key.R)
  confirmed 30 wrong collected summaries across five names, two read forms,
  and explicit fixed, inherited fixed and rollup roles. There were 72
  safe-renamed equalities and 42 other matching comparisons, including real
  counts. The original dtplyr steps stayed lazy and their source data.tables
  stayed unchanged. The grouping-key copy removed the original source
  spelling from the summary mask. Ordinary dtplyr could itself refuse some
  special-name reads with locked bindings. A safe-name ordinary control
  established the expected calculation without that ambiguity. Closed #695,
  #703, #952 and #1047 covered generated reads and share-specific paths.
- [Unnamed frame fields](2026-11-07-summary-grouping-share-invariants/summary-branch-schema.R)
  reproduced the condition class and missing call on three eager classes.
  The six common-schema and named-packing controls passed. ADR 0015 assigns
  caller-rewritable refusals to Package conditions. Closed #430 and #1041
  concerned incompatible names introduced by marginplyr itself.
- [First across](2026-11-07-summary-grouping-share-invariants/summary-across-storage.R)
  established three wrong schemas, six rollup schema refusals, and nine
  false refusals from a populated-frame list conversion, with 54 ordinary,
  direct-share and unwrapped controls. The early planner dispatched
  `as.list()` to construct its selection input and mask. The same stored
  input worked with ordinary dplyr and direct named shares. Closed #1365
  concerned columns of a summary value. Closed #701 and #694 concerned the
  summary mask and packed selection timing.

### Share dependency and execution controls

[The local dependency matrix](2026-11-07-summary-grouping-share-invariants/share-dependencies.R)
crossed Parent and Total shares, both prohibited dependency directions, and
plain data.frame, tibble and eager data.table. It checked exact affected
values, the Package-condition fragments and source serialization.

| Family | Missing refusals | False refusals | Passing controls |
| --- | ---: | ---: | ---: |
| Nested writes | 48 | 24 | 48 |
| Replacement dispatch | 24 | 24 | 24 |
| Inline actuals and factories | 60 | 0 | 60 |
| Static indices | 60 | 0 | 36 |
| Total | 192 | 48 | 168 |

Changing only a nested write to a flat write, using actual base replacement,
calling a known member directly, or using an ordinary positive literal
separated the failing planner boundaries. Controls also checked retained
formal bindings and independently selected members. The relevant earlier
repairs were #1403, #1413–#1415 and #1430–#1432. The inline actuals also differ
from #1232, which concerned formal values in function position. The static
index findings concern the finite demonstrated forms.

[The SQL cardinality probe](2026-11-07-summary-grouping-share-invariants/sql-share-cardinality.R)
used a rollup of two rows with x equal to 1 and 3. The source added an integer
array's unnested elements to the sum. A singleton yielded 11, 13, 14 with
shares divided by 14. An empty array yielded no rows, while the two-element
array yielded 11, 21, 13, 23, 14, 24 with detail shares divided by 24. Both
Grand total rows returned one. Each cardinality was observed on live DuckDB
and PostgreSQL with both native GROUPING SETS and the portable adapter, with
the physical path asserted from rendered SQL. The default source check stayed
enabled. The [sequence control](2026-11-07-summary-grouping-share-invariants/sql-share-laziness.R)
observed zero evaluations of the source expression through construction and
SQL rendering, followed by six on explicit collection. The share reference's
backend eligibility section and ADRs 0010 and 0017 supply the scalar and
denominator contract. Closed #602 concerned scalar constants in portable
summaries. Closed #623 and #652 concerned local or dtplyr validation.

## Wider comparisons

| Probe | Result and boundary |
| --- | --- |
| [Grouping-family oracle](2026-11-07-summary-grouping-share-transfer-hunt/grouping/plan-fuzz.R) | Replaying 1,500 seeded recursive specifications on the new snapshot matched the independently enumerated ordered set families under duplicate removal. |
| [Ordinary summary comparison](2026-11-07-summary-grouping-share-invariants/summary-backends.R) | 2,217 of 2,640 cross-backend comparisons met a loose row, name and value oracle. Direct backend controls explained 60 Arrow absorbing pronoun calls, 114 DuckDB and 249 SQLite differences in empty or all-missing SQL aggregates and SQLite Boolean output storage. Those outcomes were excluded from package findings. |
| [Collated SQL results](2026-11-07-summary-grouping-share-invariants/sql-collated-results.R) | 144 SQLite and 144 PostgreSQL comparisons matched independently assembled groups, displayed-denominator shares and observed Margin order. The matrix used collated columns and derived expressions, missing fixed or dimension keys, both sorted modes, duplicate occurrences, CTE options and direct collection or materialization. It normalized equivalent spellings and numeric aggregate representations. |
| [Valid SQL share values](2026-11-07-summary-grouping-share-invariants/sql-share-valid-values.R) | 276 SQLite, DuckDB and PostgreSQL collections or materializations matched ratios recomputed from the displayed denominator rows. The oracle checked share types and ratios, without making a row-order claim. |

The SQL comparison initially passed CTE options through to collection of an
already materialized base table. An upstream dbplyr interpolation failure at
that endpoint did not establish a Margin defect. The final comparison used
CTE options at the requested Margin collection or materialization boundary
and ordinary collection of the physical result. The unordered comparator also
needed explicit tie-breakers for equal aggregate values. The counts above use
the corrected comparator.

The matrices cover finite cases on one platform and dependency graph.
Arbitrary reflection, extension classes, larger plans, hosted services,
concurrency, interruption and other versions or operating systems remained
outside these observations. Equal Margin order keys do not imply a public
tie-order guarantee.

## Replay and duplicate checks

Run the evidence scripts from a Git archive of the snapshot named above, using
absolute script paths. R was 4.6.1 on arm64 macOS Tahoe 26.6.2. The loaded
packages included dplyr 1.2.1, dbplyr 2.6.0, dtplyr 1.3.3, data.table 1.18.6.1,
tidyselect 1.2.1, rlang 1.3.0, vctrs 0.7.3, pkgload 1.5.3, DBI 1.3.0, RSQLite
3.53.3, RPostgres 1.4.10, DuckDB 1.5.5 and Arrow 25.0.1. PostgreSQL 17.11 ran in a
disposable cluster over a local Unix socket. Set `PGHOST`, `PGPORT` and
`PGDATABASE` for the PostgreSQL share probes. The collated-results probe takes
`postgres` or `sqlite`. Its PostgreSQL mode uses
`MARGIN_INVARIANTS_PG_SOCKET` and port 55737 in a disposable cluster with ICU.

| Script | Observed exit codes by mode |
| --- | --- |
| `grouping-receiver.R` | observation 0, contract 1 |
| `summary-special-key.R`, `summary-branch-schema.R`, `summary-across-storage.R` | observe 0, controls 0, contract 1 for each |
| `share-dependencies.R` | observation 0, control 0, contract 1. An optional second argument selects a named family. |
| `sql-share-cardinality.R` | observation 0, control 0, contract 1 on each of DuckDB and PostgreSQL, selected by the second argument. |

Observation modes assert the dated behavior. Contract modes intentionally
return nonzero where the intended behavior is absent. Controls assert the
known legal or already guarded behavior. The replay and the source check have
separate purposes.

Duplicate checks used 729 all-state issue bodies and 707 PR bodies fetched
through paginated GitHub API results, plus a recent-issue refresh before
publication. Searches included context-name receiver dispatch, special keys
and measures, unnamed union fields, first ordinary across and list views,
set-returning expressions and scalar cardinality, closure carriers, list
writes and static indices. Relevant full threads included #1434, #1418,
#1419, #695, #703, #952, #1047, #430, #1041, #623, #652, #602, #1232,
#1430–#1432, #1413, #1414, #1403, #701, #694 and #1365. The published issues
retain the duplicate distinctions and were fetched again to verify the
published bodies and labels.

The research files are standalone evidence for readers, explicit replay and
repository source linting. The existing source-package exclusion covers the
investigation directory, and the submitted diff changes no package-build
inputs or verification rules. This justified Review-ready source scope, with package build/check,
complete suite and snapshots omitted. The pinned verification and review
records belong on the PR.
