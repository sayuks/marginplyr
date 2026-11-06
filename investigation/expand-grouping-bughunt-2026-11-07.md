# Expansion and Grouping specification deferred-boundary bug hunt

Investigated: 2026-11-07
Baseline: `e1dabaabb392a3791177e5c48e983f780c7c93ec` (freshly fetched main)
Scope: dated investigation evidence and implementation tickets; no fixes

A new worktree from main divided the hunt across local named labels and packed
keys, Grouping specification capture and authored continuations, deferred
dtplyr copies and Arrow partitions, and live SQLite declared-source types.
The November 5 greedy/adversarial investigations and November 6
[boundary](expand-grouping-bughunt-2026-11-06.md),
[deep](expand-grouping-deep-bughunt-2026-11-06.md) and
[second deep](2026-11-06-expand-grouping-round2/README.md) investigations
supplied earlier coverage and exclusions. Their measurements were not added
to this hunt's counts. The baseline contained the #1396/#1398/#1400 repairs.

## Finding and ticket boundary

| Finding | Published ticket | Independently verifiable behavior |
| --- | --- | --- |
| D1 | [#1422](https://github.com/sayuks/marginplyr/issues/1422), P2, `bug`, `ready-for-agent` | Keep package-generated deferred dtplyr source copies independent of caller namespace operators. |

`to-tickets` produced one independent implementation slice, without blockers
or prerequisite refactoring. The user delegated ticket granularity and
publication, so its breakdown quiz was omitted. Existing reports were not
closed or modified, and no repair was attempted.

### D1: generated copying let caller code remove or alter expanded rows

A one-row immutable dtplyr input expanded through a single included set.
After construction, a selective caller `::` dispatcher replaced only the
generated `data.table::copy` lookup with an empty-result function. Ordinary
source collection returned one row; expansion silently returned zero.
Removing the dispatcher restored the same query's expected row. No omitted
dimension, identifier, sorting, summary expression or authored copy call was
necessary. This violated the opening row-copy contract of
`expand_with_margins()`; ADR 0029 assigned branch isolation to the package.

The [lazy evidence](2026-11-07-expand-grouping/lazy/README.md) recorded 216
observations across roots, filters, named/renamed caches, three grouping
families, all Margin orders, early/late bindings and three selective callback
modes. There were 72 unwanted-callback cases with correct values, 72 silent
empty results and 72 callback errors. Every ordinary input/mutation/union
control invoked zero callbacks, every restored-binding expansion matched,
construction invoked none, and serialized caller tables stayed unchanged.

The generated deferred-copy head was namespace-qualified but its namespace
operator remained caller-resolved. The qualification from #1241/PR #1262
protected ordinary helper/cache names, leaving this second lookup boundary.
Existing #1346/#1355 concerned deferred share/order operations; D1's
included-only expansion required neither.

The coordinator's [independent witness](2026-11-07-expand-grouping/independent/README.md)
used a separate archive, two different keys and payload `13, 29`. A dispatcher
installed after construction returned a genuine copy whose payload was `-99`.
Source/mutation/union controls returned `13, 29` with zero callbacks, while
expansion returned `-99, -99` with one callback. Keys and caller source bytes
were unchanged, and removing the dispatcher restored the expected expansion.
Its desired-contract assertion intentionally exited one after successful
controls. The defect was independently assessed against the earlier reports
before publication.

Supplemental probes reached the same generated-copy boundary through all four
Margin verbs. A caller-authored source copy still needed its own ordinary
lookup: it ran once under source collection and twice through expansion,
including the additional generated copy. Replacing generated copying with
identity corrupted detail keys in five of eight probes, but all eight caller
tables remained byte-identical. Source destruction was not established, and
these same-cause observations did not become separate tickets.

## Additional measured coverage

| Probe | Final observations | Result and limits |
| --- | ---: | --- |
| [Local mixed named labels](2026-11-07-expand-grouping/local/README.md) | 12,672 | 12,384 exact row/value/type/schema/physical-order matches; 288 required factor NA-level refusals, with their specific diagnostic and NULL remedy checked. |
| Packed inner names | 1,296 | All exact matches over duplicate/empty/missing/special names, nesting depth, column role, source size and Margin order. |
| [Authored namespace wrappers](2026-11-07-expand-grouping/spec/README.md) | 1,500 | 750 admissible fixtures matched direct typed selection under inspection and expansion, including lookup counts; 90 incompatible direct-selection fixtures were excluded. |
| Constructor capture | 1,331 | 1,000 exact ordinary-capture comparisons and 331 opaque captured-injection checks passed. |
| Nested typed continuations | 400 | 200 bounded random shapes under keep/drop matched complete inspection and occurrence order; 986 leaf callbacks followed authored depth-first order. |
| [Hidden Arrow partitions](2026-11-07-expand-grouping/lazy/README.md) | 288 | URI/missing/hidden partition graphs matched exact schema, values and multiplicities under three consumption routes; canonicalization did not establish physical order. |
| [SQLite source types](2026-11-07-expand-grouping/sql/README.md) | 2,400 | All source-value/type/schema/multiplicity comparisons matched across ten declarations, both extended-type settings and five source graphs; physical order was not independently checked. |

Lane notes recorded invalid initial oracles and fixture corrections before
the final complete runs. Their preliminary or interrupted observations were
not added to the final counts. SQLite empty computed/inline columns required
collecting the exact empty source rather than slicing a populated result;
mixed integer/BLOB storage also exposed ordinary driver inference. Neither
was promoted to a package defect. A suspected `sum` column masking problem
was refuted in the coordinator's separate archive. No arbitrary-class,
unbounded-family, warning-identity or general performance claim followed from
these finite comparisons.

## Main freshness, duplicate checks and replay

Main was fetched at worktree creation and again before independent
confirmation; both named the baseline SHA. The coordinator searched an
all-state 711-Issue snapshot and 300 recent all-state PRs, then refreshed live
all-state Issue/PR searches:

- `repo:sayuks/marginplyr "namespace" "copy"`: 52 matches.
- `repo:sayuks/marginplyr "deferred" "copy"`: 26 matches.
- `repo:sayuks/marginplyr "data.table::copy"`: four matches.
- `repo:sayuks/marginplyr "namespace" "copies"`: 18 matches.

Complete #1241/#1271/#1311/#1346/#1355/#1143 report discussions and relevant
repair material distinguished bare helper/cache naming, source reevaluation,
summary selection, share/order and label operations. PR #1262's copy repair
and PR #1357's generated-order repair supplied separate boundaries. A final
recent-Issue refresh found concurrent #1413–#1421 reports, including
registered dtplyr bracket/converter dispatch and Grouping pattern-method
rebinding; none reported D1. The new ticket was published in English with
acceptance criteria and no blockers.

The [evidence bundle](2026-11-07-expand-grouping) retained scripts, complete
CSVs, terminal logs, environments and per-lane replay commands. Explicit
desired-contract failures documented the unfixed defect; they were not added
to package tests. All owned executions were collected through terminal
completion. Logs were normalized only for trailing whitespace/final blank
lines. No complete package suite, build or rendering substituted for focused
feedback.

The environment was arm64 macOS, R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0,
vctrs 0.7.3, rlang 1.3.0, tidyselect 1.2.1, dtplyr 1.3.3,
data.table 1.18.6.1, Arrow 25.0.1, RSQLite 3.53.3 and DuckDB 1.5.5.
Hosted storage, live PostgreSQL/SQL Server, other platforms/versions,
arbitrary caller classes and unlimited plans remained outside measured scope.

## Submission effects

Only dated evidence under the existing investigation exclusion was added.
Package code/tests, examples, vignettes, installed files, generated package
documentation, dependencies, build rules and check tooling were unchanged;
no package/check consumer invoked the probes. These content and consumer
effects justified Review-ready `source` scope. The cumulative baseline was
successful full-checked ancestor `59b28081c9c768f77e24e9dbdc324267df26a7df`,
recorded in [PR #1412](https://github.com/sayuks/marginplyr/pull/1412#issuecomment-6697674740).
Its tree and fetched main's tree were identical. The prior full result
certified only its own SHA. Package build/check, complete suite and snapshots
were omitted by this submission's source scope. The exact submitted revision,
six-check result, final compatibility assessment and Standards/Spec review
record belonged on the PR. Research without a behavior change required no
NEWS entry. CI was not monitored.
