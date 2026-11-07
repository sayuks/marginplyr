# Systematic expansion and Grouping specification validation

Investigated: 2026-11-07
Tested main: `a79def28240a5d213d3b9c1a19da93c313820b51`
Scope: functional results, schema, attributes and backend compatibility; no repair

A fresh fetch of main supplied a new worktree and branch for this investigation.
The final pre-submission fetch still named the tested main. The investigation
confirmed one previously unreported SQLite finite-collection defect and
published [#1484](https://github.com/sayuks/marginplyr/issues/1484). A concurrent
report, [#1481](https://github.com/sayuks/marginplyr/issues/1481), was independently
reproduced and retained as an existing report. No implementation was changed.

## Oracle and environment

The public `expand_with_margins()`, Grouping constructor and
`inspect_grouping()` references supplied the contracts. Relevant decisions
included ADRs 0008/0026 for specification grammar and evaluation,
0009/0013 for plan inspection and identity, 0011/0012 for missing keys and
factor labels, 0016 for dplyr/vctrs attribute delegation, 0018 for Margin order,
0020/0029 for lazy-source ownership, and 0031/0033 for SQLite and declared types.

The recent [deferred-boundary](expand-grouping-bughunt-2026-11-07.md),
[receiver-dispatch](expand-grouping-receiver-dispatch-bughunt-2026-11-07.md),
[typed-boundary](expand-grouping-typed-boundaries-bughunt-2026-11-07.md) and
[inspection](inspect-grouping-bughunt-2026-11-07.md) notes supplied historical
evidence and nearby reports. Their old measurements were not counted as new
executions. Selected historical witnesses were replayed against the tested main.

Local expansion expectations came from manually enumerated sets and ordinary
dplyr/vctrs branches, with independent factor-code reconstruction and sort keys.
The Grouping oracle used its own list-based union, Cartesian product, prefixes
and subset enumeration, comparing all six inspection columns and occurrence
order. Backend row-copy expectations used explicit branches and compared full
public frames, column types and multiplicities; requested order was checked
separately. `.sort = "none"` supplied no physical-order oracle. Arbitrary
outer-class or bare-attribute preservation was not assumed under ADR 0016.

The environment was arm64 macOS, R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0,
tidyselect 1.2.1, rlang 1.3.0, vctrs 0.7.3, tibble 3.3.1, dtplyr 1.3.3,
data.table 1.18.6.1, Arrow 25.0.1, DuckDB 1.5.5, RSQLite 3.53.3 and
DBI 1.3.0. These were observed versions, not established compatibility floors.
All database execution used fresh in-memory databases, temporary tables and
dummy data. Reproduction scripts, CSVs, logs, source-reading records and session
information remained in `/private/tmp/marginplyr-systematic-evidence-20261107`;
the user requested temporary reproduction code, so no scripts were committed.

## Confirmed new defect: unsorted finite SQLite collection

[#1484](https://github.com/sayuks/marginplyr/issues/1484) records the expected
behavior, observations, dependencies and acceptance criteria. It was labelled
`bug` and `ready-for-agent`, read back after publication, and required no blocking
ticket or prerequisite refactor. The user delegated ticket granularity and
publication, so the `to-tickets` breakdown quiz was omitted. No prior Issue was
closed or modified.

The minimal reproduction needed an ordinary RSQLite connection to `:memory:`
and a temporary table copied from a one-row data frame with character
`g = NA_character_`. Build an expansion with `.grouping = rollup(g)` and
`.margin_label = NULL`, leaving `.sort` at `"none"` and omitting `.id`.
Direct collection with `n = 1` returned one logical missing value rather than
one character missing value. The caller's source was unchanged.

| Observation on that source | Expected `g` | Measured `g` |
| --- | --- | --- |
| Direct source collection with `n = 0`, `1` or `2` | character | character |
| Unsorted expansion, `n = 0` | character, zero length | logical, zero length |
| Unsorted expansion, `n = 1` or `2` | character missing values | logical missing values |
| Unsorted expansion, unlimited collection | character | character |
| Same finite expansion with first/last Margin order | character | character |
| Explicit empty-set-first plan with finite collection | character | character |

Integer and double all-missing inputs reproduced the same logical conversion.
A requested `.id` did not prevent it; the identifier itself remained integer.
A nonmissing source exposed the loss for a zero-row prefix, while populated
prefixes could hide it through driver inference. An extended-types Date source
also became logical at the unsorted zero-prefix boundary. These additional
conditions were not needed by the minimal witness.

A 432-case finite-collection matrix crossed three scalar types, empty/populated
all-missing sources, NULL/NA labels, identifier absence/presence, detail-first
and empty-set-first plans, all sort choices, and zero/one/unlimited limits. It found
48 source-type violations and 384 exact type controls. Every case retained the
expected row limit, missing values, identifier values/type and unchanged source.
Additional populated first/last controls were exercised in the other matrices.
An independent fresh process confirmed character/integer/double inputs at
limits zero, one, two and unlimited, with source, sorted and reverse-plan
controls. Desired-contract assertions failed specifically on character versus
logical; observed-result assertions passed. No skipped test supplied evidence.

The demonstrated impact was silent schema change in a successful direct
collection, affecting type-stable joins, schema checks and empty-result
pipelines. The first introducing commit was not established. Finite limiting
appeared to wrap the source anchor; that mechanism remained an implementation
diagnosis to verify, not a prescribed repair.

The minimum reproduction dependencies were installed marginplyr and its normal
Imports, plus DBI and RSQLite; an ordinary data frame sufficed. pkgload was used
only to load the investigated development source. Arrow, DuckDB, dtplyr,
data.table, tibble and external services were not required for this failure.

## Existing reports and current-main rechecks

All-state exports contained 762 Issue bodies and 716 PR bodies, below the
2,000-record retrieval limit. Queries combined SQLite with finite/source,
finite/unsorted and finite/anchor; live Issue and PR searches included
`SQLite finite`, Date, Grouping, factor displays, level accessors and selection
proxies. Relevant full discussions were read. A final recent-report refresh
identified concurrent #1481/#1482 and PR #1483; only #1481 reached this scope.

The finite witness differed from closed
[#527](https://github.com/sayuks/marginplyr/issues/527) (unlimited total-first
types), [#651](https://github.com/sayuks/marginplyr/issues/651) (identifier
projection), [#654](https://github.com/sayuks/marginplyr/issues/654) and
[PR #659](https://github.com/sayuks/marginplyr/pull/659) (sorted limit validation
and warnings), [#665](https://github.com/sayuks/marginplyr/issues/665)/
[#666](https://github.com/sayuks/marginplyr/issues/666) and
[PR #671](https://github.com/sayuks/marginplyr/pull/671) (declared identifiers
and shares), and [#862](https://github.com/sayuks/marginplyr/issues/862)/
[PR #875](https://github.com/sayuks/marginplyr/pull/875) (unchanged fixed keys
and payloads without an identifier). Their reported witnesses were not treated
as new failures merely because their repairs touched the same boundary.

| Previous report | Minimal separating condition and measured outcome on tested main |
| --- | --- |
| [#1449](https://github.com/sayuks/marginplyr/issues/1449) | `c(a = c(a1, a2))` selected original names under all five constructors, matching typed tidyselect; the false rename refusal did not recur. |
| [#1457](https://github.com/sayuks/marginplyr/issues/1457) | Qualified constructor-callee selection received its typed context and ran once; twelve literal-plan/expansion comparisons passed. |
| [#1450](https://github.com/sayuks/marginplyr/issues/1450) | Classed displayed values equal to the chosen text label were correctly refused; 256 exact results and 32 expected refusals. |
| [#1451](https://github.com/sayuks/marginplyr/issues/1451) | Stored used/unused factor levels were checked despite a classed level accessor; 288 exact results and 192 expected refusals. |
| [#1452](https://github.com/sayuks/marginplyr/issues/1452) | Deferred dtplyr collision/omitted-label namespace witnesses satisfied their desired assertions. |
| [#1453](https://github.com/sayuks/marginplyr/issues/1453) | A zero-row, zero-column dtplyr input retained an empty integer identifier under the historical namespace witness. |
| [#1454](https://github.com/sayuks/marginplyr/issues/1454) | Derived SQLite Boolean-word columns allowed the noncollision check and refused the real text collision. |
| [#1455](https://github.com/sayuks/marginplyr/issues/1455) | Unsorted SQLite Date dimensions with text labels retained their expected text displays. |
| [#1481](https://github.com/sayuks/marginplyr/issues/1481) | A registered tibble selection proxy reordered DuckDB zero-row metadata to `b, a`. Native and wrapped first-name selection chose `b`; direct inspection incorrectly fixed on `a`. This existing open report reproduced independently. |

The local constructor witnesses required no optional backend. The factor
replays used optional dtplyr/data.table only for their dtplyr controls.
#1452/#1453 required dtplyr/data.table; #1454/#1455 required DBI/RSQLite;
#1481 required DBI/DuckDB and tidyselect, with pkgload only for source loading.
None of the #1449–#1455 failures above persisted in its tested desired-contract
replay. This was a selected recheck, not a replay of every historical finding.

## Finite coverage

Counts below describe distinct experiments, not a single pooled test total.
Cases, individual public calls and backend observations have different units.

| Experiment | Final observations and outcome |
| --- | --- |
| Local expansion | 10,080 cases: 8,976 exact full-frame/type/order/source-byte matches, 360 required structured-string refusals, 384 required raw typed-missing refusals, 360 invalid structured-data.table fixture exclusions. |
| Local companion matrix | 486 cases / 1,134 calls: 324 exact duplicate-drop/no-identifier equivalences and 162 expected duplicate refusals. |
| Independent Grouping algebra | 7,152 comparisons from 596 seeded ASTs across keep/drop/error, ordinary/opaque typed selection and fixed-key absence/presence; all exact plans or expected duplicate refusals, all input/specification bytes unchanged. |
| Grouping boundaries | 81 grammar/evaluation/prior-repair checks, 12 identity comparisons, 24 small attributed-kind comparisons and 90 local typed-predicate checks passed. |
| Backend row-copy matrix | 810 local/dtplyr/Arrow/SQLite/DuckDB observations matched complete public frames and unchanged sources; 540 additionally matched independently calculated Margin order keys. |
| Native DuckDB types | 810 historical native-type comparisons replayed successfully. |
| SQLite type endpoints | 216 observations: 180 exact, 12 unsorted finite zero-prefix type violations, 24 Date/POSIXct compute class differences described below. |
| Focused SQLite finite matrix | 432 cases: 48 type violations, 384 type controls; all non-type invariants passed, as detailed above. |
| Two-stage expansion | 180 observations: 144 exact results, 36 documented dtplyr metadata refusals; all nine pairs of sort choices, NULL/text labels and empty/populated data. |
| SQL window metadata/materialization | 12 window-order comparisons passed. Twelve materializations retained expected cardinality; all eight sorted results exactly matched direct values/types/order after normalizing the outer frame class. |

Local dimensions included character, nonfinite numeric, complex, factors with
unused and NA levels, Date, zoned POSIXct, difftime, lists, list-of, packed
frames, matrices and raw. Carriers included plain frames, tibbles, direct
data.table and persistent grouped input. Plans included rollup, cube,
composite dimensions, repeated explicit sets, products and included-only sets.
Missing fixed keys, duplicate source rows, per-dimension labels, both factor
level positions, every sort and duplicate choice, and integer identifiers were
exercised. Comparisons retained classed-column attributes and compared bare
attributes against ordinary composition rather than promising restoration.

Grouping checks included nonsyntactic/Japanese names, overlapping units,
zero selections, missing/trailing/named arguments, sibling reuse, cycle
refusal, ambiguous nested names, active bindings and single-evaluation
retention. Inspection widths 0, 1, 30, 31, 32 and 96 retained complete bits.
At width 31 the mask reached 2,147,483,647; beyond 31 inspection returned
`NA_integer_`, while the bare summary helper correctly refused. Small summary
Grouping identifiers/bits and expansion identifiers agreed with inspection.
Four generated ASTs above 2,048 occurrences were excluded; adopted families
contained at most 1,080 occurrences.

Backend comparisons included root, derived and materialized inputs. The
two-stage experiment expanded `rollup(a)` beside fixed `f`, then `(b), (), (b)`
beside fixed `f` and the first occurrence identifier. Immutable dtplyr refused
its derived typed metadata rather than processing source rows, as documented.
On SQL, no sorting retained usable input window-order metadata; first/last
cleared it. The requested Margin order survived direct materialization.

## Corrections, excluded observations and open questions

Preliminary harness errors supplied no final counts: an unavailable row-wise
predicate spelling, sanitized nonsyntactic fixture names, an oracle putting
`.id` after payload, an unsupported Arrow constant-FALSE filter, and a
data-frame printer argument. The 360 local data.table exclusions came from
conversion expanding a packed/matrix field and removing the selected column;
they established no product failure. Their initial classification was corrected
after inspecting the actual missing-column diagnostic. SQLite outer
data.frame/tibble differences were normalized while column classes and
attributes remained checked. No product change was made to obtain agreement.

Two new hypotheses were not confirmed as independent defects:

- A factor `levels()` accessor returning uppercase names did not corrupt stored
  codes. Eight local/dtplyr controls showed the base character encoder reading
  stored levels and preserving the declared domain; the hypothesis was refuted.
- Small attributed-kind cube products matched bare kinds under all duplicate
  policies. Whether a missed optimization changes very large resource bounds
  remained unmeasured, related to closed #1464; no functional difference or
  new ticket was established.

The 24 SQLite Date/POSIXct typed-NULL compute comparisons lost their datetime
class and returned stored numbers. Ordinary `compute()` of the same source
also lost that class. This was an observed difference from direct collection;
it was not established as an independent package defect because ADR 0016's
delegation and ADR 0031's source-type materialization boundary needed a more
specific datetime interpretation. Character/integer/double materializations
passed. No second ticket was published on that unresolved interpretation.

Live PostgreSQL, SQL Server and hosted stores, other R/dependency versions and
platforms, arbitrary caller methods/classes, all cross-backend inspection
proxies, unbounded plans and resource/performance behavior remained unverified.
The finite comparisons did not establish absence of other defects. No full
package suite, build or rendering was substituted for these focused probes.
All investigation execution handles were collected to terminal completion;
intentional desired-contract failures were recorded as failures.

## Submission effects

Only this dated note was added. Its consumers were readers; it introduced no
package code, tests, installed files, dependency, generated-documentation,
example, vignette, build-rule or check-tooling change. The cumulative comparison
used the merge-base with freshly fetched main, the tested SHA above. These
content and consumer effects justified Review-ready `source`; no ancestor's
full result was inherited. Package build/check, complete suite and snapshots
were omitted. The PR carries the submitted SHA, current-main compatibility
assessment, retained check outcome and separate Standards/Spec review record.
Research without a package behavior change required no NEWS entry. CI was not
monitored.
