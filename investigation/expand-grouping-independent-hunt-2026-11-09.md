# Independent expansion and Grouping specification investigation

Investigated: 2026-11-09
Revised: 2026-11-09 — ordinary-rlang control and the accepted injection boundary in ADR 0007
Tested main: `5835fd6788d0fddd4cf42a2aa524a62072f36814`
Scope: functional investigation; no implementation changes

A fresh fetch supplied the investigation SHA and a new branch/worktree from
main. Three fresh-context agents used separate worktrees at that same SHA.
Their first candidate reports were independent: implementation and branch
construction, public contracts, and coherence between processing stages.
The coordinator prepared a separate row-copy reference. Candidate reports were
shared between lanes only after all three initial reports arrived, then the remaining
algebra, native-type and backend questions were executed. A final investigation
fetch still resolved to the tested SHA.

No previously unreported functional defect was confirmed. No Issue was created
or changed. The authorized `to-tickets` workflow therefore had no confirmed
nonduplicate repair slice to publish. The observations below were not promoted
to defects merely because two executions differed.

## Contracts, history and comparison method

The expansion, Grouping-constructor and inspection references supplied the
public expectations. CONTEXT and ADRs 0001–0009, 0012/0013/0016/0018,
0020/0026/0029 and 0031–0033 supplied the lifecycle, selection, occurrence,
factor, attribute, order, metadata and direct-result boundaries. Reading covered
the existing branch creation/replacement/combination and restoration paths,
not just recent commits.

The November 8 [functional validation][functional],
[language/replacement investigation][language] and
[inspection investigation][inspection], and the November 9
[S4/source-admission investigation][s4] supplied historical evidence. The
tested main included the subsequent fixes for
[#1517](https://github.com/sayuks/marginplyr/issues/1517),
[#1528](https://github.com/sayuks/marginplyr/issues/1528) and
[#1533](https://github.com/sayuks/marginplyr/issues/1533).
Historical passes were not counted as executions in this investigation.

Input generation and expectation generation were separate. The coordinator
generated small sources independently of six handwritten occurrence lists:
rollup, cube, composite rollup, repeated explicit sets, Cartesian product and
singleton. The reference copied the source for each occurrence, replaced only
omitted dimensions, and assigned that occurrence's integer identifier. Fixed
keys and payload remained source values. A separate AST oracle used base R
ordered unions, prefixes, combinations and Cartesian loops; no package Plan
supplied its expected sets or bits.

Explicit column selections kept tracking identifiers out of selection meaning.
Unordered results were aligned by occurrence and source-row identifiers or
compared as complete row multisets. The backend matrix also used genuinely
duplicate rows with equal tracking identifiers. Comparisons included public
column names/order, values, types, classed temporal attributes, factor domains
and codes where promised, and unchanged payload. A separate Margin-order
reference built missingness/value keys and bits from handwritten sets.
Seven deliberate corruptions established that the coordinator comparator
detected missing/extra rows, wrong occurrence, changed payload/type/factor
domain, and an extra public column.

All probes used R, `TESTTHAT_PARALLEL=false`, synthetic data and owned temporary
or in-memory databases. Each worktree ran at most one reproduction at a time.
Original execution handles were retained to terminal completion; owned database
connections were disconnected. No package suite, build or render supplied
exploratory evidence.

## Finite execution evidence

Rows overlap; their counts are not added as unique covered inputs.

| Probe | Observations and result |
| --- | --- |
| Coordinator row-copy reference | 3,096 exact full-frame matches across 18 factories, zero/one/four rows, six handwritten families, representable NULL/NA/text labels, both factor-label positions and keep/drop. Factories included NA-level and ordered factors, Date, zoned POSIXct, difftime, hms, Period, integer64, lists/list-of, packed frames, matrices and arrays. |
| Metamorphic properties | 1,944 exact comparisons crossed all source factories, three families and all sort choices with consistent renaming, reversed input columns/rows, and an unrelated column. Result payload order was assessed relative to input order. Seven comparator corruption controls were detected. |
| Identifier and duplicate boundaries | 144 comparisons covered absent/public/internal-looking/dot-dot identifiers, true duplicate rows, structured/empty/NA-level sources and three sort choices. Zero-column data frames with zero/one/three source rows retained twice their row count under two empty-set occurrences. |
| Independent order and mixed labels | 324 exact ordered frames used independently constructed keys for NULL labels. Another 36 comparisons covered reordered named-list mixed labels, factor-label positions, repeated composite sets and implicit versus explicit fixed missing keys. Sources remained byte-identical. |
| Public-contract type controls | 297 of 306 type/row/role/backend conditions matched explicit typed-missing mutation and ordinary union. Nine complex dtplyr conditions raised the same unsupported-complex diagnostic in ordinary union. Another 66 display-conversion comparisons matched, 22 observed collisions were refused, and 54 zero-width matrix/array/packed-key conditions matched. |
| Independent specification algebra | 120 AST families, maximum depth three and 1–30 occurrences, crossed three input column orders and keep/drop/error: 909 complete inspection-list matches and 171 required duplicate-specific Package conditions; no differences or skips. Composite dimensions and overlapping units were included. |
| Arrow native types | 72 complete-frame and physical-field-type matches: decimal32/64/128/256, float16, uint64, duration(ns), time32(ms), time64(ns), binary, large_binary and fixed_size_binary(2); empty/two-row sources and fixed/included-only/omitted roles with NULL labels. Ordinary Arrow typed-null mutation/union supplied the delegated control. |
| Backend and derived-input correspondence | 540 exact frames across local/dtplyr/Arrow/SQLite/DuckDB, empty/four-row sources, roots, renamed/projected/filtered sources and many-to-many joins, three handwritten families, NULL/text labels and all sorts. Another 18 factor frames matched exactly; two Arrow NULL-factor cases checked factor class and values without imposing its unpromised unused-level domain. These were bag comparisons, not a general backend physical-order proof. |
| Deferred execution and reuse | 42 cases covered dtplyr root/filter/cache/renamed-cache/shared-parent-join/union graphs, repeated collection, failure then successful reuse, source bytes and native reuse; Arrow UDF failure/retry, plan display, repeated collection and compute also passed. Display ran no callback and later executions received fresh shared payloads. |

Backend expectations started from each backend's own precollected source,
separating ordinary upload conversion from expansion. Root/projection cases
carried Date/POSIXct payloads; joined sources used plain numeric temporal values
to stay within dtplyr's faithful-metadata admission boundary. Arrow's text
factor labels were expected as character under the public reference.

## Hypotheses, separating controls and disposition

| Hypothesis and evidence | Execution result | Disposition and next evidence |
| --- | --- | --- |
| Duplicate occurrences, branch combination or identifier allocation could lose/mix rows or change non-key columns. | Complete row-copy, identifier, corruption-control and backend comparisons above agreed, including equal tracking IDs. | Refuted for measured families. A full-frame counterexample would reopen that branch boundary. |
| Empty sources, restored factors or less common native types could silently change types/attributes. | Empty/typed/native-type controls matched; ordinary dtplyr complex errors were reproduced separately. | Refuted in the measured support boundary. Other extension methods or dependency versions need their own ordinary-operation control. |
| Fixed missing keys, label-name order, input schema order or internal-looking names could change semantics. | Metamorphic, mixed-label and independently ordered frames matched. | Refuted for these cases; no output-order guarantee was inferred where the reference left order unspecified. |
| Overlapping nested families could misapply duplicates or renumber retained occurrences. | Independent AST inspection comparison matched sets, omitted/fixed lists, integer bits, masks and IDs; duplicate refusals were diagnostic-specific. | Refuted within the depth/occurrence bound. Existing count-only expansion tests alone would not prove full row correspondence; the separate row-copy oracle supplied it. |
| Deferred failures or shared input graphs could mutate sources or poison a reused query. | Failure/retry and repeated-execution controls matched with unchanged caller tables. | Refuted for tested graphs and ordinary errors. Exhaustive interruption schedules were not tested. |
| A bare constructor-shaped caller selector could lose tidyselect context. | A function bound to `rollup` using `peek_data()` failed bare and succeeded under an authored namespace wrapper/direct typed selection. | Same observation as the November 8 inspection note. ADR 0008's unqualified preflight budget and narrower authored-namespace amendment did not establish the requested wider context guarantee. No new defect; a contract decision would be needed. |
| Top-level factory capture could lose an empty argument carried through `do.call`/`alist`. | Stored `do.call(grouping_set, alist(, !!!list()))` refused argument 1; inline capture accepted the empty set. No-splice and filled-splice controls agreed; other quote/eval/bquote/function controls retained the refusal. | Unconfirmed. Explicit injection selects an outer tidy-evaluation boundary, so raw-language inequality alone did not prove a residual #1517 violation. A non-injection witness or an adopted injection-boundary requirement is needed. |
| A factor domain could become stale between lazy construction and execution. | Same-domain value changes passed. Adding a used level after construction could map that value to NA under a text label; domain-order and NA-level changes also exposed captured restoration metadata. | Contract question below, not a ticket-ready defect. Repeating the observation would not settle schema-change semantics. |

The ordinary type/union tests exercise factor reconstruction, packed keys and
accepted classed values, while the focused quoted-factory tests cover their own
capture forms. None was treated as detecting a different construction-time
schema-change condition or every injection boundary merely because its file
covered a nearby helper.

## Construction-time factor-domain question

The following observation was reproduced on the tested SHA:

```r
library(marginplyr)
DT <- data.table::data.table(k = factor(c("a", "b")), payload = 1:2)
src <- dtplyr::lazy_dt(DT, immutable = TRUE)
q <- expand_with_margins(src, .grouping = rollup(k), .id = "set")
dplyr::collect(q)
data.table::set(DT, j = "k", value = factor(c("z", "b")))
dplyr::collect(src) # k: z, b
dplyr::collect(q)   # detail k: NA, b; margins: Total, Total
expand_with_margins(dplyr::collect(src), .grouping = rollup(k), .id = "set")
# detail k: z, b; margins: Total, Total
```

Eighteen separating conditions showed that source observation changes within
the captured domain preserved values. Changing the domain itself made the
construction-time factor snapshot stale. ADR 0002 establishes one typed
snapshot per operation, and ADRs 0012/0016 establish reconstruction from that
metadata. Neither supplied a measured guarantee for changing the input's
schema/prototype after constructing a lazy result. Therefore this was neither
declared harmless nor filed as a confirmed contract violation. The missing
evidence was an adopted interpretation covering such between-stage schema
changes, followed by an acceptance case for the selected behavior.

The older SQLite Date/POSIXct direct-materialization interpretation question
remained historical context from [functional validation][functional]. This
investigation did not resolve that question or count it as a new finding.

## Harness corrections, freshness and tracker checks

Preliminary failures were kept separate from package evidence. The coordinator
corrected an unexported fixture helper, normalized empty hms displays to the
documented character type, and replaced a function-valued nested constructor
head with the required constructor spelling. The final matrices were rerun.
The AST reference corrected top-level selection-leaf admission, zero-dimension
bit names and cube subset ranking before its final run. Arrow fixture casts
replaced unsupported constructors; moving a time-type union control into Arrow
removed four R-side hms/difftime differences. Backend controls corrected an
ordinary factor argument position and the documented Arrow unused-level
boundary; dtplyr join fixtures were restricted to admitted plain roots before
the final full matrix. The first type matrix ended in a presentation-only
`print()` error after saving all observations; its nine dependency errors were
independently reproduced and that run was not counted twice.

All-state tracker snapshots contained 796 Issue bodies and 745 PR bodies,
below their 2,000-item limits. Searches covered constructor/unqualified/
namespace/selection-context/factory, alist/do.call/injection, and factor
domain/schema/freshness terms. Related closed reports
[#946](https://github.com/sayuks/marginplyr/issues/946),
[#1396](https://github.com/sayuks/marginplyr/issues/1396),
[#1400](https://github.com/sayuks/marginplyr/issues/1400),
[#1457](https://github.com/sayuks/marginplyr/issues/1457) and #1517 were compared
with the existing notes and repair history. No confirmed new nonduplicate
defect remained to publish; no existing report was reopened.

After integrating all reports, the coordinator revisited capture/grammar, set
algebra, branch creation/replacement/combination, identifier presence/absence,
factor/structured/empty restoration, fixed missing keys, public/internal names,
metadata, backend correspondence, input protection and failure/reuse paths.
No executable promising hypothesis remained with an unexplained functional
signal. The explicit contract questions above were retained with their next
required evidence rather than reported as resolved. Finite results did not
establish absence of other defects.

## Environment, replay and limits

The environment was arm64 macOS 26.6.2, R 4.6.1, marginplyr 0.1.0,
dplyr 1.2.1, dbplyr 2.6.0, tibble 3.3.1, vctrs 0.7.3, rlang 1.3.0,
tidyselect 1.2.1, dtplyr 1.3.3, data.table 1.18.6.1, Arrow 25.0.1,
DuckDB 1.5.5, DBI 1.3.0 and RSQLite 3.53.3. Other platforms/versions,
unprovisioned live servers, hosted storage, arbitrary extension methods,
unbounded plans and exhaustive interruptions were unverified. They require
separate environment-specific evidence; simulator output was not server proof.

Temporary evidence remained under `/private/tmp/marginplyr-hunt-evidence-root`
and sibling `-a`, `-b`, `-c` directories. The coordinator scripts were
`oracle.R`, `metamorphic.R`, `identifier-boundaries.R` and
`order-and-labels.R`, each run as `Rscript SCRIPT SOURCE_WORKTREE`.
Agent scripts included `arrow-physical-types.R`, `algebra-oracle.R`,
`backend-frame-oracle.R`, `coherence-retry.R` and
`factor-freshness-controls.R`. Logs, CSV/RDS outcomes, environments, document
versions and short hypothesis ledgers were retained there. These temporary
paths carry no repository retention guarantee; the minimum observations above
retain the essential contract questions without adding package tests.

## Submission effects

Only this dated investigation note was added. Its consumers were readers;
temporary probes were not package/check inputs. No package source, tests,
examples, vignettes, generated/installed files, dependencies, build exclusions
or verification tools changed. These effects justified Review-ready `source`
against the merge-base with freshly fetched main. Package build/check,
complete suite and snapshots were omitted, with no ancestor's full result
inherited. Source scope supplied no timing proposal. The submitted SHA,
integration assessment, mandatory checks and separate Standards/Spec review
belonged on the PR. This research-only record required no NEWS entry.
CI monitoring was outside the requested work.

## Revisions (2026-11-09)

The `do.call`/`alist` observation above was resolved as a difference in written
injection boundaries, with no implementation repair selected. On the same
tested package SHA, R 4.6.1 and rlang 1.3.0, ordinary `enquo()` capture produced
the same inline result as marginplyr. A control using no marginplyr function
isolated the cause:

```r
ordinary <- function(...) rlang::enquos(..., .ignore_empty = "trailing")
capture <- function(x) rlang::eval_tidy(rlang::enquo(x))

length(do.call(ordinary, alist(, !!!list())))          # 1
length(capture(do.call(ordinary, alist(, !!!list())))) # 0
```

The outer capture removed the empty splice before the called function ran,
leaving its empty argument trailing. Ordinary `enquo0()` retained the missing
argument, as did constructing the argument list before public capture.
Non-injection and filled-splice controls agreed. The matching result therefore
did not establish extra marginplyr language rewriting of the kind #1517 fixed.
This did not mean that the result difference was unobservable: a factory
inspecting the changed language could select a different dimension, and the
ordinary-rlang control selected that same dimension.

The owner adopted rlang's written injection boundary and explicitly excluded
this observation from repair work. Replacing capture globally with `enquo0()`
or adding an `alist()` spelling exception was not selected. The durable
decision is [ADR 0007, public injection keeps its written evaluation boundary](../design/adr/0007-capture-user-expressions-at-public-verbs.md#amendment-public-injection-keeps-its-written-evaluation-boundary).
The [accepted boundary specification](../design/specs/grouping-context-and-factor-schema-boundaries.md#2-public-injection-follows-its-written-evaluation-boundary)
records it separately from the two implementation targets. No repair Issue
was created for this injection observation.

The follow-up probe and complete control matrix were retained as
`/private/tmp/marginplyr-grill-injection-facts-20261109.R`, `.log` and `.rds`.
Its execution returned exit 0. These temporary files carried no repository
retention guarantee; the control above retained the separating evidence.

The owner separately resolved the caller-context and schema-lifetime questions
by adopting the accepted targets in ADRs 0008 and 0002/0012. Freshly fetched
main still named `5835fd6788d0fddd4cf42a2aa524a62072f36814`; a new disposable
archive reproduced both witnesses. Searches over 799 Issues and 748 PRs found
no identical implementation ticket. The independent follow-ups were
[#1550](https://github.com/sayuks/marginplyr/issues/1550) for factor loss
prevention and [#1551](https://github.com/sayuks/marginplyr/issues/1551) for
faithful caller context, both without blockers. These were newly adopted
contract targets, not a claim that the earlier investigation had already
established those wider guarantees. Package implementation remained pending.

[functional]: expand-grouping-functional-validation-2026-11-08.md
[language]: expand-grouping-language-and-replacement-scans-2026-11-08.md
[inspection]: inspect-grouping-deep-bughunt-2026-11-08.md
[s4]: expand-grouping-s4-and-source-admission-2026-11-09.md
