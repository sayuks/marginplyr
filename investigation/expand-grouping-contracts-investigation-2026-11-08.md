# Expansion and Grouping specification contract investigation

Investigated: 2026-11-08
Investigation SHA: `2298c561633591844576992ef98e039af264a6c4`
Scope: functional investigation; no implementation changes

The investigation began from freshly fetched main in a new worktree. Three
fresh-context agents independently investigated implementation, public contracts,
and coherence across processing paths at the same SHA. Each used its own
worktree and disposable archived source. Their initial reports did not depend
on one another's candidates. The coordinator independently prepared input
generators and a row-copy reference calculation.

No previously unreported functional defect was confirmed. No Issue was created
or modified: the user authorized independent ticket splitting and publication
through `to-tickets`, but no confirmed nonduplicate repair slice remained to
publish. Executable promising hypotheses from all three initial reports were
resolved before preparing this note. The remaining contract question and
environmental limits are recorded below, without claiming absence of other bugs.

## Contracts and independent expectations

The expansion, Grouping-constructor and inspection references supplied the
public contracts. CONTEXT and ADRs 0008/0026 supplied grammar and selection
evaluation rules; 0009/0013 supplied occurrence identity and inspection;
0012/0016 supplied factor restoration and the boundary of attribute guarantees;
0018 supplied Margin order; 0020/0029 supplied lazy-read and input-protection
boundaries; 0031–0033 supplied SQLite and declared-type boundaries.

Reading covered capture and preflight, selection and family compilation,
replacement and branch combination, source sharing, identifier allocation,
factor restoration, finalization, and backend metadata. Recent dated notes and
fix history identified known conditions; they were not counted as new evidence.
The investigation covered existing implementation paths beyond the latest diff.

The coordinator manually enumerated five families: rollup, cube, composite
rollup, repeated explicit sets and a product. Expected rows were constructed
by source-row indices repeated once per occurrence, then replacing only
omitted dimensions. A separate AST calculation in the contract lane expanded
ordered union, prefixes, subsets and products. Neither used an internal plan
as the sole authority for expected rows.

Input generation was separate from these expected families. Explicit selectors
kept tracking columns from changing selection meaning. Some source rows had
identical tracking IDs and payload, preserving actual duplicate multiplicities.
Comparisons included complete public column names/order, values, R types,
factor domains/codes, relevant classed-column attributes and structured shapes.
Unspecified row orders were normalized as bags; requested Margin orders were
also checked with independent bit/missingness/value/occurrence keys. Arbitrary
object attributes and data-frame row names were not added to the contract.

## Finite execution evidence

Rows describe separate measurements with overlapping inputs; their counts are
not a count of unique covered cases.

| Probe | Observed result |
| --- | --- |
| Coordinator indexed-row oracle | 2,760 exact full-frame matches across 13 type factories, zero/one/four-row inputs, five manually enumerated plans, keep/drop, NULL/NA/mixed/text labels where representable, and both factor-label positions. |
| Coordinator transformations | 600 exact matches under consistent special-name renaming, column permutation, row permutation and unrelated-factor addition; five scalar/structured families, all sorts and keep/drop. |
| Comparator negative controls | All eight corruptions were detected: missing/extra row, wrong occurrence, changed payload, identifier type drift, fixed-key type drift, column permutation and changed factor domain. |
| Direct repeated expansion | 156 exact accepted comparisons across local, dtplyr, Arrow, SQLite and DuckDB, empty/populated input, all sorts and two families, through up to three generations with distinct occurrence IDs. Twelve dtplyr second-generation calls refused unsafe metadata. |
| dtplyr collect-first controls | All twelve refusals matched the Package condition; explicitly collecting the first result made the second expansion match its independent expectation, with caller-table bytes unchanged. |
| Implementation-lane type boundaries | 93 exact full-frame matches and 15 required structured-text-label refusals from 108 conditions. Included integer64 beyond exact double precision, units, hms, complex, expression, list-of, list-array, zero-width matrix, three-dimensional array and empty/nested packed columns. |
| Implementation-lane algebra | 649 bounded ASTs under keep/drop/error matched independent set algebra and identity; 117 actual expansions also matched exact frames. One generated family beyond 1,024 occurrences was excluded. |
| Implementation-lane backend and restoration | 186 exact backend frame comparisons, including 36 direct SQLite/DuckDB materializations; 24 exact local/dtplyr factor comparisons with sentinel-like stored levels, NA levels, true missing codes and factor payload. Positive-row zero-column local frames also retained multiplicities and identifiers. |
| Implementation-lane physical order and evaluation | 28 exact physical-order/frame comparisons, including eight SQL computes. dtplyr callback counts and DuckDB sequence state established one source evaluation without extra discarded work. Ten one/two-set cases retained usable window ordering on SQLite/MSSQL/MySQL/PostgreSQL simulators and live DuckDB. |
| Contract-lane algebra and identity | 3,998 comparisons matched from 1,500 seeded specification candidates bounded to 128 occurrences; keep/drop/error, overlapping composite dimensions, empty products, nested unions/products and non-syntactic names. Sampled actual expansions also matched. |
| Contract-lane types and order | 2,412 exact matches across 12 types in dimension/fixed/payload roles, zero/one/five rows, NULL/text labels where representable, both label positions, keep/drop and all sorts. Included complex, temporal, NA-level ordered factor, list/list-of, packed frame, matrix and three-dimensional array keys. |
| Contract-lane boundaries | 19 control/refusal groups, 72 zero-column comparisons and 288 four-backend name/fixed-key/occurrence comparisons passed. Widths 0/1/30/31/32/64 retained exact bits, masks and rows; masks beyond 31 dimensions were the documented NA integer. |
| Coherence-lane derived inputs | 3,240 exact comparisons across local/dtplyr/Arrow/SQLite/DuckDB, nine input shapes, six handwritten plans, NULL/text labels, all sorts and keep/drop. Shapes included filters, empty inputs, rename roundtrips, ordering, unions, many-to-many joins, derivations and materialized inputs. |
| dtplyr alias metadata | 108 comparisons of six source/target type families at zero/one/two rows retained prototypes and factor levels, with unchanged root bytes. Caller bindings for setnames/setcolorder/setorder did not override dtplyr execution wrappers. |
| Arrow native types | 128 collect/compute endpoints matched independently constructed native typed-null unions for map, large-list, fixed-binary, decimal, zoned nanosecond timestamp, date64, nanosecond duration and null; empty/populated inputs, rename roundtrips and one/repeated sets. |
| Volatile source and failure paths | 24 direct/materialized endpoints across dtplyr/Arrow/SQLite/DuckDB and all sorts retained the same source rows and payload per occurrence. dtplyr construction ran no callback; each dtplyr execution processed its source once. Deferred error and Mutable-step refusal controls left caller tables unchanged. |

## Hypotheses and separating controls

| Hypothesis | Evidence and execution | Verdict and next evidence |
| --- | --- | --- |
| Duplicate rows and duplicate sets could deduplicate, leak between branches or alter payload while preserving row count. | Independent row-index and AST expectations checked exact multiplicities, occurrence IDs and every carried column; deliberate corruptions failed comparison. | Refuted within the measured families. A new input with an exact bag difference would reopen its branch path. |
| Typed replacement, factor restoration or internal-looking names could change a fixed key or structured value. | Type/role matrices, consistent renaming and empty-input cases retained exact values/types/domains/shapes. | Refuted for these types and names. Arbitrary extension methods remain unmeasured. |
| A second expansion could lose SQLite source types or previous identifiers. | Initial tests inserted an ordinary rename between expansions and observed empty derived-query type loss. Direct tests retained the result boundary and used distinct IDs without the rename; SQLite matched through all three generations. | The initial expectation crossed ADR 0031's downstream-delegation boundary. It was not a new defect; direct composition passed. |
| dtplyr repeated expansion or derived head(0) refusal could be an empty-input bug. | The Package condition matched ADR 0020's faithful-metadata rule. Root-schema and collect-first controls succeeded with unchanged caller data. | Documented refusal, excluded from accepted-result counts. Broadening safe metadata admission would need separate evidence. |
| SQLite sorted volatile-source values could differ by occurrence. | The initial coherence comparison differed only in data-frame row names. A minimized payload-only control matched 36 collections; comparison of the public columns then matched all 24 endpoints. | Oracle error, not a value defect. Preliminary failed runs were not counted as passes. |
| Raw omission or constructor syntax failures could be unknown refusals. | Included-only raw succeeded; omitted raw had no representable typed missing value. Empty-position/trailing-comma controls and constructor callback counts matched the published grammar. | Refuted/excluded. No Issue slice followed from these observations. |
| Empty hms display or unshared SQL expansion could lose type/window metadata. | Empty hms returned logical(0) in the initial oracle's as.character call; marginplyr correctly normalized its display to character(0). Window-order metadata and subsequent LAG SQL matched with sorted/no-order controls on five dialects. | Oracle error and refuted implementation lead. Existing empty-display assertions detect the hms regression; expansion was outside the old #493 summary report. |
| Generated deferred helpers or native Arrow nulls could change metadata or mutate inputs. | Caller-helper controls, alias-overwrite prototypes, native schema/null-validity comparisons and serialized source-byte controls all matched. | Refuted for measured graphs and methods. New versions, extension types or failure modes would need a new execution. |

Existing tests were inspected for what their assertions actually detect.
The duplicate-policy test in `test-expand-operation.R` checks counts and names,
so it alone would not detect an equal-count payload/occurrence swap. The
independent frame comparisons addressed that distinction. Packed/matrix and
Margin-order tests already assert inner values/types/shapes and occurrence
tiebreaks. Constructor-argument tests cover authored empty positions, splices
and capture counts. Metadata/shared-source tests cover safety refusals, cached
types and immutability. DuckDB's sequence witness detects extra discarded
source evaluations; this investigation's SQL/Arrow volatile probes established
equal visible copies, not exact evaluation counts on every backend. Existing
tests were not described as covering every cross-product measured here.

## Existing observations, report searches and limits

The earlier [functional-validation note][previous] already recorded an unresolved
Date/POSIXct typed-NULL SQLite materialization question. The public-contract and
coherence lanes did not remeasure it or classify it as newly confirmed or
refuted. It still required an adopted interpretation of ADR 0016/0031's temporal
materialization boundary before an implementation ticket could be justified.

All-state snapshots retrieved 772 Issue bodies and 727 PR bodies, below a limit
of 1,500 each. Topic searches included volatile/random/evaluated-source and
SQLite source-sharing terms in Issues and PRs, including closed/merged reports.
Known source-sharing reports included [#1243][i1243], [#1271][i1271],
[#1272][i1272], [PR #1262][p1262] and [PR #1290][p1290]. Constructor-empty
history included [#1489][i1489]. These searches identified existing boundaries;
search result absence was not treated as evidence of functional correctness.

Hosted databases, live PostgreSQL and other SQL servers, other OS/dependency
versions, arbitrary S3/S4/Arrow extension methods, remote filesystems,
resource exhaustion and exhaustive interruption/native-cancellation schedules
were untested. Large exponential specification families were bounded in the
generated-algebra probes. The coordinator rechecked the merged reports against
branch creation/replacement/combination/restoration, constructor compilation,
identity, order, source sharing, metadata and failure-path boundaries; there was
no unexplained executable functional signal left in that coverage map.
Finite successful results do not establish absence
of defects outside those measurements.

## Environment and replay

The observed environment was arm64 macOS Tahoe 26.6.2, R 4.6.1,
dplyr 1.2.1, dbplyr 2.6.0, tibble 3.3.1, vctrs 0.7.3, rlang 1.3.0,
tidyselect 1.2.1, dtplyr 1.3.3, data.table 1.18.6.1, Arrow 25.0.1,
DuckDB 1.5.5, DBI 1.3.0 and RSQLite 3.53.3. Probes ran serially per worktree
with TESTTHAT_PARALLEL=false. Original execution handles were retained and
collected to terminal results. Only synthetic data in private in-memory
SQLite/DuckDB databases was used; owned connections were disconnected.

Temporary scripts, source archives, logs, result files, environment records and
hypothesis ledgers remained outside the submitting worktree:

- Coordinator: `/private/tmp/marginplyr-contracts-root-20261108`, including
  `oracle.R`, `transformations.R`, `chained.R`, `collect-first.R` and their logs.
- Implementation: `/private/tmp/marginplyr-A-2298c561-20261108`, including
  `type-probes.R`, `algebra.R`, `backend-probes.R`, `edge-controls.R`,
  `sort-probes.R`, `evaluation-probes.R`, `window-order.R` and final logs.
- Public contracts: `/tmp/marginplyr-research-b-2298c561`, including `algebra.R`,
  `typed-order.R`, `controls.R`, `empty-backends.R`, `width.R` and final logs.
- Coherence: `/tmp/marginplyr-coherence-ef2f32af28cb`, including
  `coherence-matrix.R`, `dtplyr-alias-metadata.R`, `dtplyr-step-calls.R`,
  `arrow-native-assert.R`, `deferred-and-failure.R` and final logs.

Run each R script individually with Rscript. These temporary artifacts have no
repository retention guarantee. Preliminary oracle errors and their logs were
retained separately from successful final measurements.

## Submission effects

The submission added dated investigation prose for readers. It changed no
package code, tests, examples, vignettes, generated or installed files,
dependencies, build exclusions or verification tools. No package/check consumer
invoked the temporary scripts. These effects justified Review-ready source
scope from the merge-base with freshly fetched main; package build/check,
complete suite and snapshots were omitted. The pinned submitted SHA, current-main
assessment, mandatory outcomes and separate review record belonged on the PR.
There was no behavior change requiring a NEWS entry. CI was not monitored.

[previous]: expand-grouping-functional-validation-2026-11-08.md
[i1243]: https://github.com/sayuks/marginplyr/issues/1243
[i1271]: https://github.com/sayuks/marginplyr/issues/1271
[i1272]: https://github.com/sayuks/marginplyr/issues/1272
[i1489]: https://github.com/sayuks/marginplyr/issues/1489
[p1262]: https://github.com/sayuks/marginplyr/pull/1262
[p1290]: https://github.com/sayuks/marginplyr/pull/1290
