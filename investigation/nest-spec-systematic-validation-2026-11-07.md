# Nesting and Grouping specification systematic validation

Investigated: 2026-11-07
Tested main: `a79def28240a5d213d3b9c1a19da93c313820b51`

The investigation started in a new worktree made from main, on
`investigation/nest-spec-systematic-20261107`. Independent source archives of
that SHA were loaded with `pkgload::load_all()` for the executions below.
The investigation assessed functional correctness, without changing the
implementation. Database checks used only new in-memory SQLite and DuckDB
connections with dummy data; Arrow dataset checks used temporary Parquet files.

No previously unreported marginplyr defect was confirmed in this finite corpus.
No new Issue was therefore published under `to-tickets`. Previously reported
failures were tested against the pinned main rather than inferred fixed from
their closed tracker state. The historical notes were left unchanged because
these results did not overturn their observations at their earlier SHAs.

## Contract and method

The public references for `nest_with_margins()`, `nest_by_with_margins()`,
the five Grouping specification constructors and `inspect_grouping()` supplied
the expected behavior. Relevant decisions included ADRs 0001, 0008, 0009,
0012, 0013, 0015, 0016, 0018, 0020, 0026, 0029 and 0033. In particular,
[ADR 0016][attributes] separated properties constructed by marginplyr from
arbitrary attributes delegated to dplyr and vctrs, and [ADR 0018][order]
separated opt-in Margin order from Grouping-plan order.

The Nest membership oracle manually enumerated the grouping sets, partitioned
source rows by their active keys, then matched each outer key and `.id` to its
source-row multiset. It compared cell column names, values and types, original
keys under `.keep = TRUE`, outer cardinality, integer identifiers, ungrouped or
row-wise metadata, and source serialization. It did not use the package's
resolved plan to generate its expectations. The separate order oracle built
the lexicographic key from fixed-key missingness/value, each dimension's
Grouping bit/missingness/value, and `.id`.

The Grouping specification oracle independently expanded five node kinds and
compared every column of the list-format inspection result: set occurrence,
included and omitted dimensions, fixed and variable keys, Grouping bits and
Grouping identifiers. Error, drop and keep policies were assessed separately;
keep was used for inspection, not for either Nest verb, which refuses it.

## Executed coverage

Counts below describe fresh executions on the tested SHA. Assertions and calls
are different units, and overlapping lanes are not summed into a single total.

| Area | Executed corpus | Observed result |
| --- | --- | --- |
| Nest membership, projection and grouping | 3,888 calls: six fixtures, six manually enumerated plans, tibble/plain frame/immutable dtplyr, no fixed key/explicit fixed key/existing groups, both verbs, keep modes and all three sort options | Zero mismatches in membership, values/types, key correspondence, identifiers, grouping state or input bytes. |
| Independent Margin order | 576 calls: character, integer and factor keys, declared NA level versus actual factor missingness, six plans, optional fixed key, local/dtplyr, both verbs and keep modes, first/last | Exact outer order and key values/types matched. |
| Nest shapes and refusals | 138 explicit assertions | Empty schemas, 0-by-0 input, local populated zero-column input, all-key zero-column cells, `.key` overlap with payload, NULL key behavior, duplicate policies, row-wise input, grouped-plus-by and drop-false refusals matched. |
| Exact duplicate source rows | 144 calls: three identical source records, optional payload/fixed key, tibble/plain frame/dtplyr, both verbs, keep modes and all sort options | Detail cells retained all three duplicates; Grand total cells retained all four source rows, or three in their fixed partition. Dropping a repeated grouping set did not drop source duplicates. |
| Column types | 1,248 calls: 13 dimension/payload types, empty/populated, frame/tibble/data.table/dtplyr, both verbs, keep modes and sort options | 1,080 admitted results matched; the 168 remaining outcomes were classified below with native controls. |
| Factor levels and labels | 256 calls: ordered/unordered, explicit NA level and actual missing code, empty/populated, local/dtplyr, keep modes, both verbs, label and level-position options | 128 exact level/cell matches and 128 declared-level refusals, including refusal with observed checks disabled. |
| Named labels and collisions | 384 pairs, 768 calls: eight named-label forms and their reversed name order, empty/populated, both check flags, label positions, sort options and verbs | 432 admitted results and 336 expected observed/declared collision refusals. Named-label order did not change either result or diagnostic; kept factor codes and Grand total membership matched. |
| dtplyr ownership and derivation | Eight source shapes through both verbs: keyed root, filter, rename, join, union, cache, cached rename and cached order | All 16 results retained the expected Grand total payload and source bytes. |
| sf payload compatibility | 144 calls: two geometry columns with CRS 4326/3857, duplicate and missing points, optional factor fixed key, rollup, empty/populated, both verbs, keep, sort and three label forms | Geometry values and CRS matched source slices; kept keys and source bytes matched. |
| Grouping algebra | 2,808 complete comparisons: 312 specifications, three duplicate policies, local/dtplyr/Arrow | Zero differences, including nested products/unions and nine-dimension cubes with up to 1,024 occurrences. |
| Grouping boundaries | 155 assertions under a 96 MiB R vector-heap limit | Duplicate rejection/drop, 0/1/30/31/32/40 dimensions, nested-name ambiguity, unusual names, cyclic references versus shared subtrees, named/empty constructor arguments and Option arguments matched. |
| SQL inspection and Nest admission | Two inspections and both Nest refusals per in-memory SQLite/DuckDB source | Name-only inspection read no rows; SQLite refused typed selection, DuckDB used a LIMIT-0 proxy. Both Nest verbs refused SQL inputs. Native collection alone triggered the dummy runtime error; original dummy tables matched. |
| Arrow inspection and admission | Table, RecordBatch, Dataset, reader and reader query, empty/populated | 30 exact inspection plans and 20 expected Nest refusals; subsequent native reads retained all source rows, including reader batches. |

The six membership fixtures covered ordinary and missing character keys,
numeric NA/NaN and positive/negative infinity, factors with unused levels,
empty input and singleton input. The six plans covered rollup, cube, union
with a different dimension order, composite rollup, a single set and the
keyless Grand total. The boundary checks included all five parent and child
constructor kinds in three parenthesized forms, and verified that named
constructor arguments were refused without forcing their values.

## Candidate classification

Three apparent cross-backend differences in the type matrix were reduced to
small controls in the temporary evidence area; none established a new package
defect.

| Minimal condition | Expected boundary and measured outcome | Scope and disposition |
| --- | --- | --- |
| A rollup with complex columns on dtplyr | Combining native data.tables with `funion(all = TRUE)` raised `unsupported column type found in x or y: [complex]`; collecting the Margin result raised the same diagnostic. | 24 matrix cases. A native data.table limitation, not evidence of an incorrect Nest result. Complex local cases were assessed separately. |
| Convert a tibble with packed frame column `dim` to data.table | Native conversion produced `dim.a`, `dim.b`, `rid`; no column named `dim` reached marginplyr. Selecting `dim` then raised `Can't select columns that don't exist.` | 24 data.table and 24 dtplyr cases. Input conversion changed the schema before the public call. |
| Omit a raw dimension with a typed-missing label | A raw vector has no typed missing representation. The call raised `marginplyr_error`, naming `dim` and requesting a supported label or changed grouping. | 96 cases over four carriers. Explicit refusal, not silently corrupted values. |

Other documented differences were also retained as boundaries: Nest cells were
data frames without a shared element-subclass guarantee; `nest_by_with_margins()`
returned a local row-wise frame; SQL and Arrow nesting were refused; and a
populated zero-column frame lost its rows during native dtplyr input conversion.
These were not counted as package findings. Arbitrary object or bare-column
attribute preservation was not imposed as an additional contract.

Two oracle mistakes were corrected before accepting results. Empty Grouping
bits initially acquired an oracle-only `names = character(0)` attribute.
Another harness converted a row-wise input to dtplyr before testing the
row-wise-input refusal. Corrected executions passed; neither initial mismatch
was attributed to marginplyr. A source name `...` was excluded where native
tibble/dplyr construction itself refused it.

## Prior reports and main rechecks

The recent [Nest boundaries note][boundaries], [cell-conversion note][cells],
[registry note][registry] and [deep boundary note][deep] supplied historical
candidates and controls. Their earlier counts were not reused as execution
evidence for this investigation.

| Existing report | Minimal expectation and fresh measurement on the tested main |
| --- | --- |
| [#1456][identity], repaired in [PR #1460][fix1460] | Character keys east/east/west with payloads 17/34/51 and an omitted typed-missing dimension produced one Grand total cell containing all three values through each Nest verb. The selective generated-helper callback ran zero times. Historical contract replay exited 0. |
| [#1457][callee], repaired in PR #1460 | A qualified parenthesis callee with a captured selector retained the active tidyselect context. All 40 public-consumer comparisons passed; violations were zero, versus the historical 30. Contract replay exited 0. |
| [#1452][labels], repaired in PR #1460 | All 16 explicit collision-check controls kept the Package refusal, with zero selective callbacks; all four omitted-label controls retained Total rather than the substituted display. Both contract replays exited 0. |
| [#1466][converter], repaired in [PR #1479][fix1479] | A caller-local list conversion method did not receive populated Nest payloads. Both verbs and included/rollup controls retained source values with zero callbacks. Desired-behavior replay exited 0. |
| [#1464][budget], repaired in [PR #1479][fix1479] | A repeated-unit cube with 20 units and a 20-factor repeated product refused duplicates under the 96 MiB heap limit. Drop returned the two intended sets, `(a)` and `()`. |

All-state CLI exports contained 762 Issues and 716 PRs. Searches over their
titles and bodies used `nest`, `grouping`, `Margin label`, `Margin order`,
`complex`, `packed`, `raw` and `as.data.table.list`; relevant report bodies,
comments and closing-PR identifiers were also retrieved. Among the broader Nest/Grouping matches,
the open report was [#468][mutable], an upstream mutable-dtplyr reporting task,
not a newly discovered failure of the supported immutable path. There was no
confirmed new candidate requiring a ticket split, blocker edge or new dependency.
No existing Issue or PR was modified.

Main was fetched again after the executions and still resolved to the tested
SHA. Tracker closure alone was not used to certify any replay.

## Environment, artifacts and limits

Executions used arm64 macOS Tahoe 26.6.2, R 4.6.1 and C.UTF-8 locale; dplyr
1.2.1, tidyr 1.3.2, tibble 3.3.1, dtplyr 1.3.3, data.table 1.18.6.1,
rlang 1.3.0, tidyselect 1.2.1, vctrs 0.7.3, Arrow 25.0.1, DBI 1.3.0,
dbplyr 2.6.0, DuckDB 1.5.5 and RSQLite 3.53.3. sf 1.1.2 supplied the geometry
compatibility checks; it was not a dependency of the ordinary Nest or plan
oracles. pkgload 1.5.3 loaded the archived implementation.

Reproduction programs, native controls, oracle corrections, CSV summaries and
logs stayed under `/private/tmp/marginplyr-nest-systematic-evidence/`, outside
the submitting worktree. They were temporary investigation material, not new
package tests or committed reproduction code. Each execution handle was
collected to terminal completion. The checked-in historical probes were run
in disposable archives with contract/desired expectations rather than their
old failing-observation expectations.

This was finite functional validation, not a proof over arbitrary input graphs,
specifications, S3 methods or vector classes. Hosted SQL systems, other package
versions, other R versions and operating systems remained untested. Detailed
NA-versus-NaN tie ordering was not independently specified by the order oracle.
No physical ordering assertion was made for `.sort = "none"`, and no additional
public promise was inferred for source-row order inside a cell. The findings
did not establish that every historical issue was fixed: only the rechecks
listed above received fresh executable evidence.

The submission added only this dated evidence note. It changed no runtime
code, package tests, dependencies, generated help or build configuration, and
required no NEWS entry. Its consumers were investigation readers; it neither
established a new package contract nor changed a package-check input. Those
effects justified Review-ready `source`; the PR verification record identifies
the separately checked note commit and explicitly records the heavy omissions.

[attributes]: ../design/adr/0016-delegate-result-class-and-attributes-to-dplyr.md
[order]: ../design/adr/0018-order-margin-results-by-grouping-structure.md
[boundaries]: nest-grouping-boundaries-2026-11-07.md
[cells]: nest-grouping-contract-sweep-2026-11-07.md
[registry]: nest-spec-bughunt-2026-11-07.md
[deep]: nest-spec-deep-bughunt-2026-11-07.md
[identity]: https://github.com/sayuks/marginplyr/issues/1456
[callee]: https://github.com/sayuks/marginplyr/issues/1457
[labels]: https://github.com/sayuks/marginplyr/issues/1452
[converter]: https://github.com/sayuks/marginplyr/issues/1466
[budget]: https://github.com/sayuks/marginplyr/issues/1464
[mutable]: https://github.com/sayuks/marginplyr/issues/468
[fix1460]: https://github.com/sayuks/marginplyr/pull/1460
[fix1479]: https://github.com/sayuks/marginplyr/pull/1479
