# Expansion and Grouping specification systematic bug hunt

Investigated: 2026-10-29
Baseline: `4c63bb917ed7ed31ff6aadf060a5eae7a32cd004` (`main`)
Scope: investigation, reproducible evidence, and implementation tickets; no fixes

A new worktree was created from local `main`; a fetch confirmed that
`origin/main` named the same commit. Four investigators divided local
expansion, lazy expansion, Grouping specification evaluation, and independent
reproduction/SQL composition. The earlier
[expansion hunt](expand-grouping-bug-hunt-2026-10-29.md),
[follow-up](expand-grouping-followup-2026-10-29.md),
[deep hunt](expand-grouping-deep-hunt-2026-10-29.md), October 25–26 expansion
notes, and recent nesting/summary investigations supplied prior coverage and
closed-ticket boundaries. Their old measurements were not counted as new
execution evidence.

The user delegated ticket boundaries and publication without a quiz. The four
findings became four independent `bug`, `ready-for-agent` tickets through the
`to-tickets` workflow. None needed a blocker or a prefactoring ticket.
Only this dated note and evidence under the excluded `investigation/` tree
were added. Package implementation, package tests, generated documentation,
dependencies, and build configuration were not changed.

## Findings and ticket boundaries

| Finding | Priority | Ticket | Independently verifiable behavior |
| --- | --- | --- | --- |
| A1 | P2 | [#1037](https://github.com/sayuks/marginplyr/issues/1037) | Preserve expansion values and identifiers over schema-hidden Directory partitions |
| G1 | P2 | [#1038](https://github.com/sayuks/marginplyr/issues/1038) | Retain the first selection returned by a qualified constructor-shaped call |
| G2 | P2 | [#1039](https://github.com/sayuks/marginplyr/issues/1039) | Resolve caller-defined qualified selections against typed metadata |
| L1 | P2 | [#1040](https://github.com/sayuks/marginplyr/issues/1040) | Validate typed-missing factor-array dimensions without recursive failure |

G1 and G2 shared an uncommon but ordinary R precondition: the captured caller
environment bound `::` or `:::`. Neither changed a package namespace or forged
a specification. They were split because retention of an already-read value
and classification before a typed read were separate mechanisms, with separate
minimal reproducers and acceptance conditions.

## A1: Directory partitions replaced expansion values

One Parquet row with integer `g = 9` was saved under directory `7`. A Dataset
was opened using `DirectoryPartitioning` with integer field `sid`, but its
explicit public schema contained only `g`. Ordinary collection returned the
correct public input. Expanding `rollup(g)` with a NULL label and `.id = "sid"`
returned `sid = 7` in both occurrences, rather than 1 for the included row and
2 for the omitted row. The row count was correct; occurrence identity was not.

The ranked hypotheses were a missing partition name in the reservation scan,
an ordinary Arrow union failure, and a Margin-order-only failure. Successful
full-schema projection/materialization controls supported the first. Ordinary
Arrow mutation plus union returned the correct identifiers, rejecting the
second. Unsorted expansion still failed, rejecting the third.

`arrow_source_names()` read public source schemas and field names from
Hive-style `name=value` directories. A positional Directory path contained the
partition value but not its field name. That live field therefore escaped
reservation when omitted from the Dataset schema. A partition named like the
private dimension alias replaced both included values and omitted labels;
staged and suffixed aliases and hidden sorting aliases exposed the same
boundary. The ordinary identifier witness required no internal-looking caller
name.

The 648-case matrix crossed six collision/control scenarios, nine input shapes,
empty/populated sources, NULL/text labels, and three sorts. It recorded 536
passes, 96 value differences, 16 Margin-order differences, and no errors. All
112 differences involved populated explicit-schema, WithSchema, filtered, or
union-derived narrowed Directory inputs. All 108 unrelated-partition cases
passed; full-schema projection, Table, computed, caller-arranged, and joined
shapes each passed their 72 cases. The ordering comparison ignored ties and
unspecified order. This was a public-value/order oracle, not an exhaustive
Arrow physical-type oracle.

Focused ordinary-union, Table, computed-input, full-schema projection, and
repaired schema-hidden Hive controls passed. A trace observed zero tracked
collect/compute/as_arrow_table entries during construction and a positive
count after explicit collection; it did not enumerate every C++ operation.
An independent investigator reproduced the wrong identifier and successful
ordinary union with a separate row-identifier fixture.

Closed #992 covered query-hidden fields, and #1018 explicitly concerned Hive
partition expressions absent from Dataset schemas. The Hive witness passed on
this baseline. This finding was its unhandled Directory counterpart. ADRs
0016 and 0018 supplied the result/order ownership; ADR 0020 constrained the
repair to preserve lazy construction.

## G1: qualified constructor selection values were replayed

A caller-defined `::` returned a function whose first selection answer was
`region` and later answer was `amount`. The captured argument still spelled
`marginplyr::rollup()` inside `grouping_sets()`. Inspection selected `amount`
and called the function twice, although preflight had already read `region`.
Binding the identical function directly to bare `rollup` selected `region`
with one call. The constructor-shaped argument returned a selection here,
rather than an actual nested rollup specification.

The ranked explanations were a retained binding covering only bare callees,
a general failure to retain selection-valued answers, and an extra evaluation
from expansion branches. Bare and computed get/list-index heads retained the
first answer. Inspection and expansion had the same counts. Those controls
rejected the latter two explanations. `read_grouping_arg()` retained the answer
under the terminal constructor name in a child environment, but the qualified
AST resolved the namespace operator again instead of consulting that binding.

The matrix recorded 600 public-consumer comparisons: five constructor
spellings, five heads, three parenthesis depths around calls/heads, two return
kinds, local/immutable dtplyr inputs, and inspection/expansion. All 120 qualified
selection-returning cases mismatched the first-answer plan or one-call budget.
The remaining 480 controls matched: 120 bare, 240 computed-head, and 120
qualified specification-returning cases. Computed-head specification returns
received the documented spelling refusal; they were refusal controls rather
than admitted nested specifications. The parent constructor was grouping_sets;
other parent kinds were not enumerated. An independent minimal reproduction
confirmed `amount`/two calls versus `region`/one call.

Closed #946 and duplicate #954 repaired bare constructor-shaped selection
retention. The qualified operator-binding case remained after that repair.
CONTEXT.md distinguished these constructors from Contextual helpers: the
caller function ran. ADR 0008's constructor-result evaluation budget supplied
the first-answer expectation. A bare lexical callee was a measured workaround.

## G2: qualified spelling bypassed typed selection

A qualified leaf-helper call with a constant argument was captured in an
environment whose namespace operator returned a function selecting character
columns through `tidyselect::peek_data()`. The frame held character `region`
and integer `amount`. Direct typed tidyselect selected `region`; inspection
with the same selection as `.by` returned no fixed key. A rollup received the
false refusal that at least one dimension was required. A bare all_of with a
qualified base-function argument reproduced the same classification boundary.

The ranked hypotheses were an assumed namespace callee, lost typed metadata,
and an unusable selection context. Wrapping the identical expression in
ordinary `identity(...)` made every measured consumer agree with typed
selection. That and the direct tidyselect control rejected the latter two.
The leaf classifier trusted recognized namespace/helper spellings and constant
arguments; helper-argument analysis looked qualified functions up directly in
their namespace. Neither established that the captured head actually resolved
through R's ordinary namespace operator. The integer names proxy therefore
answered a type-dependent selection.

The typed matrix recorded 896 primary comparisons and 896 opaque identity
controls. Twenty leaf scenarios crossed ten optimized helper names with both
namespace operators; twelve helper-argument scenarios crossed six base
functions with both operators. Two parenthesis depths, local/dtplyr inputs,
`.by`, five constructor positions, and expansion's fixed/dimension overlap
refusal were exercised.

There were 704 primary mismatches and 192 matches. All 448 dtplyr cases
mismatched. Local `.by`, rollup, cube, and overlap positions each contributed
64 mismatches; local set/union/product positions each contributed 64 matches
because their canonical typed pass recovered the selection. Leaf scenarios
contributed 440 mismatches and 120 matches, and helper arguments 264 mismatches
and 72 matches. All 896 opaque controls agreed. For overlap, expansion
incorrectly accepted a call whose actual fixed selection and dimension both
named region; literal and opaque controls correctly refused it.

An independent fixed-key witness reproduced `region` versus an empty key.
Closed #924 concerned a foreign helper namespace; #975 concerned a
pronoun-shaped subscript head. Neither was this owning-namespace spelling
resolved by a captured operator binding. The ticket required conservative
classification without speculative caller execution, while retaining genuine
names-only preflight and ADR 0020's query boundary.

## L1: factor-array missing validation recursed

A factor containing `a` was assigned dimensions one by one and placed in a
tibble column `g`. An included-only expansion returned it unchanged. An
unsorted rollup with `.margin_label = NULL` raised:

```text
expressionStackOverflowError / stackOverflowError
evaluation nested too deeply: infinite recursion / options(expressions=)?
```

Ordinary mutation with `vctrs::vec_init(g, 1L)` followed by union_all returned
the expected two-by-one factor matrix, preserving its level and containing the
original code and a typed-missing code. A shaped integer control expanded
successfully. An independent investigator reproduced both results.

The hypotheses were recursive representability validation, factor restoration
destroying shape, and unsupported branch combination. The no-restoration
NULL/no-NA-level witness still failed before branches; ordinary combination
succeeded. The omitted-prototype check in `validate_margin_label()` called
`vec_detect_missing()` and reproduced the recursion directly.

Ordinary factor-array group_by and arrange also recursed. Those upstream
boundaries alone were excluded. The adopted finding was narrower: unsorted
expansion performed row copying, and its successful ordinary mutation/union
control needed neither grouping nor sorting. The public *Display labels and
grouping identity* reference explicitly admitted local matrix/array dimensions
with NULL/NA labels, including empty input, and promised omitted shape and
typed missing components. That supplied an existing expectation without adding
arbitrary class/attribute restoration under ADR 0016.

The focused matrix crossed 0/1/3 rows, absent/present factor NA level,
matrix/3D shapes, and NULL/NA labels: 24 cases. All ordinary mutation/union
controls preserved shape, levels, factor codes, row identifiers, and occurrence
identifiers. Included-only expansion produced the 18 valid answers and six
correct NA-level collision refusals. Every rollup instead recursed: 18 valid
results were blocked, and six invalid NA-label calls failed before their
required Package condition. These were manifestations of one validation gate,
not separate tickets. NULL retained its separate declared-NA-level meaning.

## Additional coverage and excluded leads

| Harness | Executed scope | Result on the baseline |
| --- | --- | --- |
| Local type matrix | 22 families × labels × 0/1/3 rows × dimension/fixed/payload roles × three sorts: 1,782 cases | Three known empty-hms text-label signals; remaining expected success/refusal checks passed |
| Local re-expansion | 11 types × two successive three-way sorts × 0/1/3 rows: 297 cases | Four occurrence pairs, source-row multiplicity, fixed keys and types matched |
| Local boundary matrix | 126 zero-column, 45 zero-width/4D-array, and 21 keyed/indexed data.table cases | All 192 passed, including source ownership |
| Grouped inputs and encoding | 216 grouped cases plus ten encoding/name examples | 108 admitted cases matched, 108 drop=FALSE calls correctly refused, all name examples matched |
| dtplyr metadata sweep | 840 attempted name/type/size/arithmetic derivations | 418 ordinary source failures; 196 documented metadata refusals; all 226 admitted comparisons matched |
| dtplyr graph fuzz | Seed 28131029, 600 graphs of depth at most five | 492 matches, 99 documented metadata refusals, nine ordinary source failures; no new difference |
| SQL re-expansion | 216 SQLite/DuckDB comparisons of two successive rollups, special names, empty/populated sources and first-operation sorts | 204 public-column/value-bag matches; 12 correct case-equivalent identifier refusals |

The type families included list/list_of, packed/nested frames, matrices/arrays,
factors with NA levels, temporal classes, integer64 above 2^53, units, blobs,
and lubridate classes. The SQL comparison normalized values to character and
ignored unspecified ordering; it did not prove physical SQL types or all
Margin-order relations. Local re-expansion explicitly checked row identifiers
rather than counts alone.

The empty-hms text-label signal was already excluded by the earlier deep
hunt: ordinary hms character conversion produced logical(0), and ordinary
union shared the type error. Replacing active sf geometry with text also
invalidated ordinary sf mutation, so it was not ticketed. Ordinary factor-array
sorting/grouping failure was distinguished from L1's unsorted copy boundary.

Several first-pass oracles were corrected before interpreting their counts.
Reconstructing NA-level factors had incorrectly recoded missing values as an
NA level; the corrected oracle retained the original codes. A hand-built
zero-column frame used names=NULL instead of the native constructor's
character(0), and ordinary dplyr refused it. An array oracle initially omitted
the wrong scalar column. Grouped base/data.table inputs had already become
tibbles through ordinary dplyr, so the corrected comparison used the actual
ungrouped grouped-input value. The SQL scanner initially treated its 12
case-equivalent identifier refusals as raw signals; explicit classification
removed them. None became an issue.

The investigation did not enumerate arbitrary R expressions, custom classes,
operator bindings, unbounded specification trees, every partitioning mode,
every dependency version, every SQL dialect, or other operating systems.
No live PostgreSQL or SQL Server execution was added. Successful comparisons
established only the measured inputs. Rare namespace-operator bindings were
reported because of concrete public-plan and evaluation mismatches, not merely
because an operator had been rebound.

## Replaying the evidence

Companion scripts and logs were retained under
[`2026-10-29-expand-grouping-systematic-hunt`](2026-10-29-expand-grouping-systematic-hunt).
Run from the repository root. The focused observations printed successful
controls and captured failures; their `--assert` modes required the intended
behavior and exited 1 on this baseline:

```sh
Rscript investigation/2026-10-29-expand-grouping-systematic-hunt/lazy/reproduce-directory-partition.R --assert
Rscript investigation/2026-10-29-expand-grouping-systematic-hunt/spec/reproduce-namespace-capture.R --assert
Rscript investigation/2026-10-29-expand-grouping-systematic-hunt/spec/reproduce-namespace-typed.R --assert
Rscript investigation/2026-10-29-expand-grouping-systematic-hunt/local/factor-array-reproduce.R --assert
Rscript investigation/2026-10-29-expand-grouping-systematic-hunt/local/factor-array-focus.R --assert
```

The matrix runners were diagnostic reports with explicit successful controls;
their exit zero did not claim that reported defects were fixed. Independent
witnesses were retained in the root subdirectory. Replay terminal outcomes
were saved in `replay-status.json`; adjacent replay logs recorded stdout/stderr.
Text logs omit trailing whitespace. Larger CSV/RDS files retained classified observations. Matrix output used a
temporary directory, overridable with MARGINPLYR_HUNT_OUTPUT. The manifest
recorded retained artifact bytes. Fixture-only lint cleanup removed an unused
internal-name diagnostic and unused variables, made an assignment explicit,
and expressed intentionally nested parentheses as syntax data.

## Environment and publication boundary

The measured environment was aarch64 macOS, R 4.6.1, marginplyr 0.1.0 loaded
from the pinned checkout with pkgload 1.5.3, dplyr 1.2.1, dbplyr 2.6.0,
rlang 1.3.0, tidyselect 1.2.1, vctrs 0.7.3, dtplyr 1.3.3,
data.table 1.18.6.1, Arrow 25.0.1, DuckDB 1.5.5, and RSQLite 3.53.3.
SQL probes used private in-memory SQLite/DuckDB connections. Arrow files were
disposable local fixtures. No external service was needed for a finding.

Nine existing focused test files for Grouping specifications, inspection,
expansion, metamorphic semantics, cycles, injection, selection budgets, and
selection dispatch passed: 6,143 assertions, zero failures/warnings/skips.
They did not exercise the new boundaries above.

These additions were Repository-only under
[Local review-ready checks](../design/agents/local-checks.md), so the
Package-affecting Review-ready check was not required. Final package-aware
lintr, jarl, document-reference, context-budget, replay, diff, and Standards/Spec
review outcomes belonged to the pull request. CI was not monitored.
