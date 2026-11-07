# Expansion and Grouping specification functional validation

Investigated: 2026-11-08
Tested main: `ad7284118124f11a39ac29b10f1d101399d79524`
Scope: functional investigation and dated evidence; no implementation changes

A fresh fetch supplied a new worktree from main. Three fresh-context agents
independently investigated implementation, public contracts, and consistency
across execution paths. Their first reports shared no candidate hypotheses.
Each used a separate archive of the tested SHA, while the coordinator prepared
an independent row-copy oracle. Later reports were integrated before checking
remaining boundaries. The second fetch still named the tested SHA.

No previously unreported functional defect was confirmed. No Issue was created
or modified. The user authorized independent ticket granularity and publication
through `to-tickets`, but there was no confirmed nonduplicate repair slice to
publish. An extended-type materialization observation remained a contract
question already recorded by the previous investigation, as detailed below.

## Contracts, history and method

The public expansion, Grouping constructor and inspection references supplied
the expected behavior. CONTEXT and ADRs 0002/0003, 0007/0008/0026,
0009/0013, 0011/0012/0016, 0018/0020/0029 and 0031–0033 supplied the
metadata, selection, identity, missingness, attribute, order and backend
boundaries. Implementation reading covered specification capture and
compilation, branch construction and replacement, eager/lazy branch combination,
identifier allocation, factor reconstruction, finalization, and backend metadata
and direct-result boundaries. Investigation was not limited to the recent diff.

The November 7 [deferred-boundary][deferred], [receiver-dispatch][receiver],
[typed-boundary][typed] and [systematic validation][previous] notes, and their
nearby repair history, supplied prior evidence and exclusions. The tested main
included the #1481 context-name proxy and #1484 SQLite finite-collection
repairs. No historical execution was counted as a new measurement.

Input generation and expected results were separate. The coordinator's oracle
used five manually enumerated occurrence lists: rollup, cube, composite rollup,
explicit repeated sets and a product. Each occurrence copied every source row
and replaced only omitted dimensions. Fixed keys and payload were copied into
every branch. A separate generated-specification oracle implemented ordered
set union, product, prefixes and subsets independently of package internals.
An inspected internal plan was never the sole source of expected rows.

Comparisons checked complete public column names/order, values, column types,
factor domains/codes and relevant classed-column attributes, as well as the
source-row × grouping-set-occurrence multiset. Explicit selectors kept tracking
IDs from changing selection meaning. Duplicate source rows also retained
duplicate tracking values, so accidental deduplication remained observable.
Unordered results were aligned by occurrence and tracking ID. Independent
Margin-order keys were checked separately in the contract lane; backend bag
comparisons did not establish every backend's physical order.

R probes ran serially per disposable source copy with
`TESTTHAT_PARALLEL=false`. All execution handles were collected to terminal
completion. Only synthetic data and private in-memory SQLite/DuckDB databases
were used. Reproduction code, CSVs and logs remained in temporary storage as
requested; they were not added to package tests or this commit.

## Finite coverage

These rows describe separate measurements. Their overlapping inputs are not
added into a claim about unique covered cases.

| Probe | Observations and result |
| --- | --- |
| Coordinator row-copy oracle | 6,840 exact full-frame matches across 14 type factories, zero/one/four-row sources including duplicates, five manually enumerated plans, NULL/NA/text labels where applicable, both factor-label positions, keep/drop and all Margin orders. |
| Oracle negative controls | Nine deliberate changes were detected: missing/extra rows, wrong occurrence, changed payload, identifier/source type drift, changed factor domain, extra public column and reordered columns. |
| Implementation exploration | 2,151 exact preliminary full-frame matches over scalar, list and base-S3 families. The other preliminary classifications were excluded from success/defect counts and resolved with separating controls. |
| Factor restoration boundary | 32 exact local/dtplyr comparisons crossed sentinel-like stored levels and their underscore successors, an NA level, true missing codes, unused levels, empty input, six repeated occurrences, mixed NULL/text labels and identifiers named like reconstruction variables. |
| Public names and POSIXlt | 15 focused controls passed for supported triple-dot/internal-like names and row-sliced AsIs POSIXlt. Visible numeric dot-dot input refusal matched ordinary selection. |
| Independent specification algebra | 350 bounded generated specifications × keep/drop/error: 1,050 comparisons passed. Duplicate refusals asserted the duplicate-specific diagnostic as well as the Package condition class. |
| Labels, types and transformations | 720 exact comparisons covered special names, Date/zoned POSIXct/difftime/ordered and NA-level factors, NULL/text/mixed/NA labels, factor position, all sorts, duplicate policies, row/column permutation, irrelevant-column addition and implicit fixed keys. |
| Empty/internal-identifier boundary | 216 exact comparisons across zero/one/three rows, six plan families, NULL/text labels, all sorts and public identifier presence/absence. |
| Consistent renaming | 72 exact comparisons renamed dimensions, fixed keys, payload and occurrence identifiers, then renamed results back. Composite cubes, repeated grand totals and packed/list keys were included. |
| Backend/derived-source correspondence | 216 exact frame comparisons over tibble, data.table, immutable dtplyr, Arrow Table, SQLite and DuckDB; roots, filters, projections, rename roundtrips, empty inputs and many-to-many joins; NULL/text labels and all sorts. Source-row bags, types and public column order matched; caller tables remained unchanged. |
| Special names across backends | 216 exact comparisons over 18 public names in dimension/fixed-key/identifier roles on dtplyr, Arrow, SQLite and DuckDB. |
| SQLite finite direct collection | 192 exact comparisons covered missing/populated sources, roots, input limits zero/one/two, filtering, opaque CTE sources, none/last sort, CTE rendering on/off and collection limits zero/one/two/unlimited. Fixed missing doubles and Date/time/BLOB payloads were included. |

## Hypotheses and separating evidence

| Hypothesis | Evidence and execution result | Disposition and next evidence |
| --- | --- | --- |
| Repeated occurrences or eager union could lose rows, mix branches or change payload. | The independent full-frame and generated-algebra oracles retained multiplicities and occurrence identity; negative controls rejected all three corruptions. | Refuted for the measured families. A new failing input would reopen the corresponding branch boundary. |
| Factor sentinels could collide with stored values, or a helper-like `.id` could change reconstruction arguments. | The 32 focused controls preserved exact codes, levels, ordered status, integer identifiers and source bytes. `test-factor.R` and `test-factor-source-values.R` already assert the underlying sentinel/code distinctions. | Refuted for these combinations. No source-code repair was proposed. |
| Internal-like names or empty input could corrupt branch names/types. | Literal name controls and empty/internal-ID comparisons matched. `bind_margin_branches()` protects public names; `test-branch-names.R`, `test-expand-inner-names.R` and the dot-dot identifier tests cover the relevant successful paths. | Refuted within supported names. A source column named `..1` reaches the existing ordinary tidyselect refusal; supported output identifiers are a separate path. |
| Raw omission or untouched AsIs POSIXlt errors could be new typed-missing defects. | All 48 initial raw-omission observations refused a type with no missing raw value. POSIXlt ordinary unsliced union was not a valid oracle; three row-sliced controls matched the package's intentional preparation. | Refuted/excluded. `validate_margin_label()` and `prepare_packed_union_input()` already handle these boundaries; packed-key tests exercise them. |
| A caller's `setnames` binding could falsify derived dtplyr metadata. | Native collection, metadata selection and expansion all remained correct with unchanged source bytes. dtplyr's execution wrappers supplied the intended helper. | Refuted by the focused callback probe. |
| Derived/joined sources or reordered schema could invalidate backend row copying. | The backend and special-name matrices matched independently collected source rows. Arrow carried-type/attribute controls also matched after correcting the comparison's row alignment. | Refuted for the measured graphs; no general arbitrary-method or remote-storage claim followed. |
| The #1484 repair could still lose types behind input limits or CTE rendering. | All 192 direct-collection cases retained public schema, values, types and multiplicities. | Refuted for these adjacent conditions. Direct materialization's extended-type question was kept separate. |

Preliminary harness mistakes supplied no defect evidence: the coordinator first
placed `.id` after payload and allowed a loop variable to shadow `base::sort`;
the corrected oracle first passed 540 calibration cases, then all 6,840 cases.
Other lanes corrected expected dot-dot name repair, unsupported Arrow
constant-FALSE filtering, bare tidyselect pronouns, POSIXlt representation and
signal-recording code. Preliminary failed or partial executions were not
reported as passing final runs.

## Existing extended-type materialization question

The [previous note's corrections and open questions][previous] already recorded
Date/POSIXct typed-NULL SQLite materialization losing its datetime class. This
hunt independently reproduced that observation; it was not a new finding.

The minimal source had one Date dimension in SQLite with
`extended_types = TRUE`. Expanding `rollup(day)` with a NULL label and last
Margin order directly collected as Date, including a typed missing margin.
Direct compute followed by collection returned integer day counts instead.
The source declaration was `DATE`, while the destination declaration was
`NUM`. A 24-condition probe crossed Date/POSIXct, dimension/fixed/payload roles,
none/last sort and extended types on/off. The twelve extended-type cases lost
the class; the twelve disabled controls already had numeric source columns.

The coordinator changed the date and added an independent payload/occurrence
identifier. Values, row correspondence, identifiers and source data remained
correct; the same declaration/class difference persisted. Ordinary
`compute(source)` also lost the class. Explicitly creating a `DATE` destination
and inserting the independently written union retained Date, showing physical
preservation was possible without proving it was an adopted package guarantee.

The contract agent reviewed ADR 0016, ADR 0031 and the B-direct specification
independently. ADR 0031's general direct-collection/materialization wording
supported the concern, but its motivating types and the specification's
explicit source-dimension guarantees named character/integer/double. The
specification excluded new extended R attribute guarantees, and ADR 0016
retained dplyr delegation outside the stated exception. Public documentation
promised source types for direct collection and a typed public table for
materialization. The existing Date NULL-label test asserted direct collection;
the Date materialization test used a text label and its declared character
type. Neither settled typed-NULL Date materialization.

Consequently this was neither promoted to a confirmed defect nor declared
unconditionally outside the contract. The missing evidence was an adopted
interpretation extending the direct materialization guarantee to temporal SQL
declarations/classes. The earlier open question remained open.

The 96 materialized endpoints in the SQLite adjacency matrix differed from
the strict source-type expectation on their Date/time payload. Additional
empty-BLOB controls exposed driver metadata loss also present under ordinary
CTAS. Normalizing temporal values and excluding the compute BLOB prototype
made a restricted values/core-type comparison pass, but that was not an exact
schema/attribute pass and was not counted as one in the coverage table.

## Freshness, report searches and remaining bounds

All-state snapshots contained 765 Issues and 721 PR bodies, below their
retrieval limits. Local searches combined SQLite with Date/POSIX/temporal/
extended-type and compute/materialization terms. Complete #640, #667, #1455
and #1484 discussions were read. Live open/closed Issue-and-PR searches for
SQLite/Date/compute returned no results, so absence was not inferred from
those searches alone. The full snapshots and prior-note search established
the existing observation above. Relevant existing repair links were
[#1455](https://github.com/sayuks/marginplyr/issues/1455),
[#1481](https://github.com/sayuks/marginplyr/issues/1481) and
[#1484](https://github.com/sayuks/marginplyr/issues/1484); none was reopened.

Evidence directories were `/private/tmp/marginplyr-hunt-20261108-root`, and
the corresponding `-a`, `-b` and `-c` directories. They held each lane's
scripts, logs, CSVs, environment and hypothesis ledger. The coordinator's
commands included `Rscript .../oracle.R`, `Rscript .../oracle-controls.R`
and `Rscript .../sqlite-date-independent.R`, with the recorded directory
substituted for `...`. These temporary artifacts had no repository retention
guarantee.

The observed environment was arm64 macOS 26.6.2, R 4.6.1, dplyr 1.2.1,
dbplyr 2.6.0, tibble 3.3.1, vctrs 0.7.3, rlang 1.3.0, tidyselect 1.2.1,
dtplyr 1.3.3, data.table 1.18.6.1, Arrow 25.0.1, DuckDB 1.5.5,
DBI 1.3.0 and RSQLite 3.53.3. Hosted stores, live PostgreSQL/SQL Server,
other versions/platforms, arbitrary extension methods, unbounded plans,
resource exhaustion and exhaustive interruption schedules remained untested.
No executable promising hypothesis remained with an unexplained functional
signal; the contract question above required a policy decision. These finite
results did not establish absence of other defects.

## Submission effects

This commit added only dated investigation prose. Its consumers were readers;
no package/check consumer invoked the temporary probes. Package source, tests,
examples, vignettes, generated/installed files, dependencies, build exclusions
and check tools were unchanged. These effects justified Review-ready `source`
against the merge-base with freshly fetched main. Package build/check,
complete suite and snapshots were omitted; no ancestor's full result was
inherited. The submitted SHA, compatibility assessment, six-check outcomes
and separate Standards/Spec review belonged on the PR. A dated investigation
without a behavior change required no NEWS entry. CI was not monitored.

[deferred]: expand-grouping-bughunt-2026-11-07.md
[receiver]: expand-grouping-receiver-dispatch-bughunt-2026-11-07.md
[typed]: expand-grouping-typed-boundaries-bughunt-2026-11-07.md
[previous]: expand-spec-systematic-validation-2026-11-07.md
