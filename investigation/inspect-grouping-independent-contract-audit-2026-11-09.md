# Grouping inspection: independent contract and compatibility audit

Investigated: 2026-11-09
Tested main: `5835fd6788d0fddd4cf42a2aa524a62072f36814`

The investigation found no newly confirmed functional defect in the finite
paths below. No Issue was created: none of the executed candidates established
an unreported contract violation. This was an evidence record, not a claim that
all possible inputs were correct. No package implementation or test was changed.

## Preparation and independent candidate generation

Freshly fetched main supplied the tested SHA. A new investigation branch and
worktree started there; three fresh-context agents used separate detached
worktrees at the same SHA. They independently generated candidates from
implementation, public contracts and processing-path consistency, without sharing
candidates until all three initial reports had arrived. The parent separately
prepared hand-enumerated families and input-generating programs.

The team read `CLAUDE.md`, its `@AGENTS.md` closure, `CONTEXT.md`, the domain,
local-check and investigation instructions. Relevant ADRs read across the team
included 0002, 0005, 0007, 0008, 0009, 0013, 0015, 0019, 0020, 0026, 0027 and
0029. The [constructor][constructors], [inspection][inspection] and
[identity-helper][identity] references supplied the public contracts.
Implementation exploration followed public capture, evaluation and environments,
nested-name admission, column selection, metadata acquisition, Plan expansion,
normalization, duplicate handling, identifiers and formatting. It was not confined
to recent changes.

The six recent inspection notes, from the [2026-11-07 investigation][prior-first]
through the [named-source investigation][prior-last], and relevant repair history
were examined. The tested main included PRs #1511, #1524 and #1542, including
the evaluated-capture, reader-inference and named-source repairs. Their historical
findings and execution counts were not counted as discoveries or executions here.

## Reference calculations and executed coverage

Expected families and generated input syntax were separate. The parent wrote
the ordered sets of 11 small specifications by hand, then used simple R membership
operations to derive fixed, included and omitted vectors, named integer bits and
weighted identifiers. The contract agent independently enumerated generated AST
models. Neither used the internal Plan compiler as its expected-value generator.

List comparisons checked all six columns, exact values, vector names and column
types, and the ordinary ungrouped tibble class. Text comparisons used its published
display grammar; ambiguous separator names were not treated as serialization
failures. Operation comparisons exercised shared package paths and were recorded
separately from agreement with independent enumeration.

Counts in this table describe different comparisons and are not a coverage score.
Each row gives the hypothesis, its evidence and result, disposition, and the next
evidence that would justify reopening it.

| Hypothesis and contract | Execution result and disposition | Next evidence |
| --- | --- | --- |
| Expansion or normalization changes ordered sets, dimensions, fixed keys, bits or IDs. Constructor reference; ADRs 0008/0009/0013. | Parent: 11 hand-enumerated specifications across three duplicate policies, 33 comparisons, matched. Contract agent: 318 seeded ASTs, keep/drop, explicit/existing grouped keys and both formats, 2,544 exact six-column comparisons, matched. All five constructors, nesting, composites, empty selections and overlapping dimensions were represented. | A distinct constructible grammar or selection boundary. |
| Duplicate optimizations lose first-occurrence order or reject unique families. Constructor reference; ADR 0008. | Contract agent: 15 models with 1,024/1,025 occurrences, 30 keep/drop plans and 15 duplicate refusals matched; 318 error-policy models gave 210 exact unique plans and 108 expected refusals. Unique families at 255/256/257 occurrences and 512-occurrence cube/product plans matched. Parent: 48 overlapping cube/product/union drop comparisons and expected refusals, using 8/9/10/12 cube units, matched independently ranked binary subsets. | Another selection-dependent optimization boundary. |
| Fixed keys or bit width changes identity types. Inspection reference; ADRs 0009/0013. | Parent widths 0/1/29/30/31/32/33/63/64 and contract-agent widths 0/1/2/30/31/32/64/96 used only two sets. IDs were integer through 31 dimensions and `NA_integer_` beyond; complete named integer bits survived. Two selected fixed keys retained their selection order. | Another accepted key-resolution form or dependency version. |
| Text formatting or non-syntactic names loses exact identity. Inspection Formats section; ADR 0013. | Both formats matched over generated separator, Unicode and backtick names. Exact list names and integer vectors survived. Text's documented display ambiguity was permitted, rather than reported as a defect. | A new accepted name/encoding form that changes exact list values. |
| Inspection IDs disagree with Margin operations. Inspection Grouping identity section; ADR 0009. | Parent compared `.id` and bare `grouping_id()` against its hand oracle. Contract agent checked 64 operations over data.frame/tibble/data.table/dtplyr, empty/populated data and duplicate keep/drop. Occurrence IDs, masks, zero fixed bits and independent base-R key-count/sum expectations matched. | Another backend/version or summary shape; shared-path agreement alone is insufficient. |
| Retained constructor-shaped selections repeat namespace lookup or change returned values. ADR 0008 retention amendments. | Implementation agent: 80 comparisons over ten returned value shapes and eight syntactic forms matched native tidyselect selection or exact external class/message, with matching callback/namespace counts. Qualified and parenthesized callee controls also matched. | A distinct observable lookup or retained-value boundary. |
| Forwarding loses lexical scope, typed context or the one-evaluation budget. ADRs 0007/0008/0026. | Implementation agent: 352 typed-context/forwarding comparisons across callback syntax, wrappers, all constructors, local/dtplyr and fixed-key controls matched direct selections. Independent lexical functions and captured environments retained their meanings. | A new constructible forwarding or binding shape. |
| A column/specification name collision becomes a silent selection under nesting. Constructor reference; ADR 0026. | Implementation agent: 400 comparisons across all five parent constructors and specification kinds, bare/parenthesized/injected/explicit-column spellings, root/two-ancestor nesting and local/dtplyr matched: 96 ambiguity refusals, 52 invalid-nesting refusals and 252 exact plans. Calibrated active bindings verified per-position evaluation counts; input and specification serialization stayed unchanged. | A distinct accepted spelling or binding boundary. |
| Public capture still changes quoted factory syntax or authored empty positions. Constructor reference; ADR 0007. | Implementation agent: 100 stored/inline comparisons across five constructors, ten factory forms and ordinary/interior-empty inputs matched exact plans or the same refusal class/message. Quote, expression, substitute, bquote, function bodies, evaluation and parsed language were represented. | A distinct quote/evaluation boundary, beyond the repaired reports. |
| Derived typed metadata differs from explicitly collected input. Inspection typed-selection section; ADRs 0002/0020/0029. | Consistency agent: 1,652 exact selected-name comparisons and 90 expected grouped-`.by`/overlap Package-condition class comparisons matched, over 60 dtplyr and seven Arrow shapes, 13 predicates and both selection positions. Empty/singleton/populated roots, immutable/Mutable inputs, projection, rename, relocation, ordering, filtering, grouping, aliases, arithmetic and constants were represented. | Another constructible type/derivation or dependency version. |
| Inspection changes inputs, consumes readers, performs source work, or leaks recording ownership. Inspection reference; ADRs 0020/0027/0029/0037. | Consistency agent: 93 assertions matched for safe/unsafe dtplyr source callbacks, Mutable table bytes, Arrow reader success/refusal, and local/SQL reentry with audited/unaudited inner and outer calls and failures. Parent ownership and saved entry-time audit settings survived; three reader rows remained after inspection. Explicit collection of isolated copies calibrated the source-row observers. | Another callback/control-transfer path with an independently calibrated observer. |
| A zero-row DuckDB metadata read still evaluates source expressions. Inspection laziness section; ADR 0020. | Parent sequence witnesses in a projected expression, filter, ordering, materialized CTE, lateral join and union did not advance during typed inspection. Independent `nextval()` calls before/after calibrated the sequence. Each inspection recorded catalog admission and its typed snapshot separately. Seven CTE/declaration/FROM-first/TABLE controls also matched. | Another SQL preparation shape with independently observable source work. |
| A further reader-like function necessarily samples values during binding. ADR 0020. | Parent `sniff_csv()` typed inspection still succeeded after numeric/character/broken contents and deletion of the temporary CSV. Its fixed output schema supplied no witness of source-row processing. No defect was inferred merely from its function name. | An independently observable value read during binding, not just a zero-row result or SQL count. |

## Candidate rejection and existing coverage

The implementation agent checked relevant existing tests in
`test-grouping-parenthesized-callees.R`, `test-authored-grouping-namespaces.R`,
`test-forwarded-grouping-boundaries.R` and `test-grouping-env-pronoun.R` against
the specific capture and environment conditions. The contract agent checked
`test-inspect-grouping.R` and `test-grouping-plan.R` against exact formats, width,
duplicate numbering, grouping order and bounded duplicate witnesses. Those tests
covered many ordinary cases; the additional probes supplied independent
enumeration and combinations rather than establishing a missing failing case.

The consistency agent checked backend and query-policy tests against source-work,
Mutable-input isolation, reader preservation and recording ownership. Arrow object
serialization changed even after native schema access alone; that observation
was not promoted to input corruption. Source values and reader consumption were
observed independently.

An unqualified caller constructor returning a selection could run in preflight
without typed selection context. The earlier deep inspection note had already
recorded that difference, and ADR 0008 retained that unqualified reading while
giving authored namespace calls a distinct context contract. It was not promoted
to a new defect. Forged private specification fields and unbounded resource
behavior were likewise not substituted for constructible public failures.

Harness failures were excluded. The parent's initial zero-width fixture used
`paste0()` on an empty vector incorrectly. Its optimized-product expectation
initially failed to normalize a column repeated across factors; canonicalizing
independent expected families resolved that mismatch. The implementation agent
corrected a native tidyselect control that passed a double quosure as a formula.
The consistency agent isolated the copy used for explicit-collect expectations
from Mutable inputs. One run read a script while it was being edited and produced
an invalid fixture call; its final matrix used a fixed script. A reader fixture
was corrected to pass RecordBatches instead of a Table. These corrections changed
the harnesses, not package source.
Stopped executions were not reported as passing comparisons.

## Provenance and limits

All exploration used R, one execution at a time per worktree, with
`TESTTHAT_PARALLEL=false`. No full suite, build or render was used for exploratory
feedback. The parent's terminal handles were 4148 (fixture failure), 17007
(corrected oracle, exit 0), 27283 (SQL witnesses, exit 0), 4135 (reader control,
exit 0) and 99149 (corrected cube comparisons, exit 0). Earlier synchronous cube
expectation failures were retained as harness failures. All owned executions
were collected before another target ran in their worktree.
The consistency agent's matrix, state and observer-calibration sessions
23751/88216/67407 each ended at exit 0; the other agents likewise collected every
execution to a terminal result.

Parent evidence remained in `/private/tmp/inspect-root-evidence/`, with R harnesses
under `/private/tmp/inspect-root-*.R`. Agent evidence remained under
`/private/tmp/inspect-hunt-a-evidence/`, `/private/tmp/inspect-hunt-b-evidence/`
and `/private/tmp/inspect-hunt-c-evidence/`. The agent records included short
hypothesis/evidence/result/verdict/next entries and existing-test assessments.
These were temporary investigation artifacts, not package replay assets.

Open/closed Issue and PR searches covered `inspect_grouping`, `grouping metadata`
and inspection/Grouping PR titles. Relevant repaired reports included #1489,
#1505, #1506, #1514, #1516, #1518, #1519, #1525, #1526 and #1533. No new report
was justified by the executed evidence, so there were no new Issue links or
blocking edges to publish through the requested `to-tickets` workflow.

Environment: arm64 macOS Tahoe 26.6.2, R 4.6.1, marginplyr 0.1.0 loaded from the
tested source; dplyr 1.2.1, dbplyr 2.6.0, rlang 1.3.0, tidyselect 1.2.1,
tibble 3.3.1, vctrs 0.7.3, dtplyr 1.3.3, data.table 1.18.6.1, DBI 1.3.0 and
DuckDB 1.5.5. Database work used an owned in-memory connection and synthetic
data; connections were closed and temporary input files removed. Arrow 25.0.1
supplied the in-memory table/query/reader controls.

After integrating the independent reports, the team checked both display and
exact formats, local and lazy paths, successful and rejected inputs, name/type/
value/order controls, input ownership, source work separately from metadata, and
consecutive/reentrant record state. No concrete executable promising hypothesis
remained in these lanes. A final main fetch still named the tested SHA.

Other OS/dependency versions, live PostgreSQL/SQL Server, hosted sources,
arbitrary dispatched methods and unbounded expression/SQL graphs were not
established. They require their own fixtures and observable evidence. Finite
matching comparisons do not prove universal correctness.

## Submission effects

Only this dated note was added. Its consumers were investigation readers; it
changed no package code, tests, installed files, dependencies, generated
documentation, build behavior or verification tooling. The cumulative baseline
was the merge-base with freshly fetched main. These effects justified
Review-ready `source`, with package build/check, complete suite and snapshots
omitted. No NEWS entry was needed. Revision-specific verification, main
compatibility and review records belonged on the PR.

[constructors]: ../R/grouping-spec.R
[inspection]: ../R/inspect-grouping.R
[identity]: ../R/grouping-context.R
[prior-first]: inspect-grouping-bughunt-2026-11-07.md
[prior-last]: inspect-grouping-named-source-admission-2026-11-09.md
