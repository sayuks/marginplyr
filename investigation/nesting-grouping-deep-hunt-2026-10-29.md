# Nesting and Grouping specification deep bug hunt

Investigated: 2026-10-29
Baseline: `4c63bb917ed7ed31ff6aadf060a5eae7a32cd004` (`main`)
Scope: investigation and implementation tickets; no implementation changes

## Outcome

Four investigators examined `nest_with_margins()`,
`nest_by_with_margins()`, the five Grouping specification constructors,
Grouping-plan inspection, and Grouping identity helpers in a new main-based
worktree. One independently reproduced defect was published as
[#1043 — Keep dtplyr typed grouping faithful with active arithmetic bindings](https://github.com/sayuks/marginplyr/issues/1043),
with `bug` and `ready-for-agent` labels and no blockers.

The finding crossed typed inspection and both nesting verbs through one
arithmetic-metadata admission boundary. One independently verifiable repair
therefore formed one ticket; no prefactoring or dependent ticket was needed.
The user delegated ticket granularity and publication, so the `to-tickets`
approval interview was omitted. No parent issue was modified.

No package implementation, permanent package test, generated documentation,
dependency metadata, build exclusion, or public contract was changed. The
scripts and logs were dated investigation evidence under the existing
`^investigation$` source-package exclusion.

## F1: active arithmetic dispatch produced false typed metadata

The input contained integer `v = 1:2`; an immutable dtplyr mutation derived
`z = v + 1L`. The addition operator was an active binding. Its first two
lookups returned base addition. The third returned a function that produced
`character()` on zero-length input and ordinary addition on populated input.
Later lookups again returned base addition.

This fixture deliberately separated metadata dispatch from source values:
five sequential ordinary collections, including collection at the third
lookup, all returned integer `z = 2:3`. The alternative function agreed with
base addition on every populated evaluation. The binding therefore did not
explain the discrepancy by returning different source values.

With a fresh fixture, `inspect_grouping()` using
`.by = where(is.character)` nevertheless reported `z` as a fixed key. Explicit
collection followed by the same inspection reported no fixed keys. Both
nesting verbs selected that false key and produced two outer rows with `z = 2`
and `z = 3`, each containing one source row and only `v` in its payload.
Nesting the explicitly collected input instead produced one cell containing
both source rows and both integer columns, `v` and `z`.

The Grouping specification route exposed the same false type. With a genuine
character column `g` beside the integer columns,
`rollup(where(is.character))` inspected as `(g, z)`, `(g)`, and `()`, whereas
explicit collection selected only `g` and expanded to `(g)` and `()`. Both
nesting verbs using that false dimension subsequently failed with an external
data.table union type mismatch between integer and character internal keys.
The fixed-key route established silent incorrect grouping independently of
that later diagnostic.

### Cause, predictions and controls

The ranked hypotheses were:

1. The safety classifier read a changing operator binding and treated one
   observed base function as stable dispatch. Fixing the binding to base
   addition would remove the discrepancy; a fixed custom operator would be
   refused.
2. Empty-input type inference was wrong even for ordinary fixed arithmetic.
   A fixed base-addition control would then retain the discrepancy.
3. Inspection processed the source rows. A callback on the alternative
   operator would then see populated input while the plan was being built.

The controls supported the first explanation. The base-function binding gave
the correct plan and nested membership. A fixed custom value-dependent
operator was refused by the existing metadata guard. The alternative callback
recorded only zero-length input during the faulty inspection, so this finding
did not establish a full-source read.

`dtplyr_metadata_safe_step()` asked
`dtplyr_value_stable_arithmetic()` whether the derived expression used stable
arithmetic. The latter used `get()` to compare the resolved operator with its
base function. Two reads answered the numeric and scalar stability questions;
synthetic proxy evaluation performed the third lookup. The earlier base
answers did not establish that the active binding would supply base arithmetic
to that evaluation. The proxy's character column was then trusted by typed
selection.

The inspection help and ADR 0020 supplied the faithful metadata or refusal
boundary. Refusing an operator whose dispatch could not establish faithful
metadata was already an available response; executing arbitrary dispatch over
empty input and trusting its type was the admission defect. The ticket did not
request arbitrary value-dependent metadata inference or a new source-reading
exemption.

Closed #1017 concerned repeated updates within one mutation, #996 and #1024
concerned cached input graphs, and #995 concerned typed selector dispatch.
Their repairs did not establish the stability of this active arithmetic
binding. The three upstream/release issues open at the initial tracker query
held no duplicate of this finding.

### Independent reproduction

All evidence lived under
`investigation/2026-10-29-nesting-grouping-deep-hunt/`. The coordinating
investigator independently constructed the one-column fixed-key fixture and
verified the five populated collections and all three public consumers:

```sh
Rscript investigation/2026-10-29-nesting-grouping-deep-hunt/coordinator/independent-active-operator.R
```

The command exited zero after asserting the dated wrong-key and wrong-cell
observations, and its adjacent text log preserved them. It was an observation
replay, not a regression test claiming the defect was repaired. The backend
track retained the Grouping specification manifestation, controls, and an
assertion mode requiring faithful metadata or an early Package condition:

```sh
Rscript investigation/2026-10-29-nesting-grouping-deep-hunt/backends/reproduce-active-arithmetic.R --assert
```

The coordinating replay of that command exited 1 on the baseline, after its
ordinary-collection and fixed-operator controls passed. The failure required
correct public behavior; report mode exited zero while recording the dated
defects. `coordinator/independent-assert-replay.txt` retained the red result,
and `coordinator/stable-operator-control.txt` separately retained both nesting
verbs’ fixed-base membership control.

## Additional search coverage

The earlier nesting and typed-specification notes from October 25–26, the
same-day exhaustive nesting evidence, and the latest expansion and summary
hunts supplied prior coverage and closed-ticket boundaries. Those historical
measurements were not counted as runs on this baseline. The contracts read
included `CONTEXT.md`, the public nesting and inspection help, the architecture's
Lifecycle, Grouping plan, and Answering a review sections, and the relevant
ADRs on capture, kinds, identity, inspection, result delegation, laziness,
ambiguity and Mutable steps.

| Retained evidence | New completed measurements and oracle | Outcome on the baseline |
| --- | --- | --- |
| `local/simultaneous-names.R` | Seed 29102971; 1,200 inputs with multiple simultaneous special source/output names, scalar/list/packed/matrix/factor columns, both verbs and keep policies, zero to eleven rows; independent source grouping and row projections | 1,124 accepted calls, 76 documented rowwise `.rows` refusals, 30,498 matching cell-column projections; source serialization unchanged |
| `local/precision-time-shaped.R` | Extreme integer64 values, repeated DST instants, nonfinite doubles, signed zero, complex missingness, factor-matrix payloads, packed factors and one-dimensional arrays; zero/one/six rows, both verbs, keep policies and sort settings | 684 accepted calls and 4,494 matching cell-column projections; source serialization unchanged |
| `spec/algebra-special-names.R` | Seed 20261029; 600 generated ASTs, five constructor kinds, overlapping composites, empty sets/products, non-syntactic names and parentheses; independent ordered set-family expansion, local/dtplyr and all duplicate policies | 3,600 inspections agreed on exact sets, order, stable deduplication, fixed keys, bits and identifiers, or the expected duplicate refusal |
| `spec/evaluation-boundaries.R` | Five constructors, local/dtplyr, inspection and both nest consumers, name-only/typed children and parentheses; ordinary active selections, nested names and constructor-shaped ordinary functions | 540 consumer comparisons and 60 ambiguity controls matched; six calls sharing a wrapper promise forced it once in the first call and zero times in the next five |
| `spec/wide-identity.R` | Widths 0, 1, 30, 31, 32, 33, 64 and 65, local/dtplyr, inspection and helper outputs, fixed keys and nesting cardinality | 80 checks passed; bits remained complete, implicit IDs became missing above 31, explicit supported masks remained exact and overwide implicit helpers refused |
| `backends/cached-dag-differential.R` | 9,720 immutable dtplyr graphs combining cached/shared roots, filters, arithmetic, copies, joins/set operations, ordering, relocation and grouping; explicit collection as oracle, callback row counts and source-copy comparison | 5,400 admitted nesting comparisons agreed, no premature row-predicate reads or source mutations; 4,320 inspections refused and no ordinary collection control failed |
| `backends/metadata-boundaries.R` | Repeated updates and derived schema compositions; isolated prototype comparison and lexical arithmetic controls | 204 typed comparisons: 164 matched and 40 refused; 32 lexical nesting calls produced no premature callback |
| `coordinator/empty-boundaries.R` | Named/AsIs/dimensional scalar list-column names and simultaneous special payload/output names; local/dtplyr, empty/nonempty inputs | 40 scalar-shape result comparisons and 196 fixed-key-free rowwise reconstruction checks passed |

The coordinator also replayed the 1,200-input local sweep successfully. This
was corroboration of the same cases, not another 1,200 independent inputs.
The DAG refusal counter covered metadata safety, empty typed rollup selections,
and fixed/dimension overlap. Those refusals were not compared at the same
granularity as admitted results, so the 4,320 count did not assert that every
refusal had independently proved optimal admission.

The per-track dated READMEs described the fixtures, commands, harness
corrections and narrower limits. The rows above did not imply a full Cartesian
product of every parameter named in different rows.

## Rejected candidates and limits

- Factor-matrix grouping raised stack overflow through nesting, ordinary
  `dplyr::group_by()`, and direct `vctrs::vec_group_loc()`. It was excluded as
  an upstream grouping limitation. Factor matrices in payloads survived.
- Ordinary union converted the POSIXlt fixture to POSIXct too. Exact instants
  were preserved, so that conversion did not establish a package restoration
  defect under ADR 0016.
- Input `..1` names were refused by tidyselect and ordinary nesting as well;
  generated dot-dot output names and valid signed-name payloads were checked
  separately. Bare `.data` and `.env` selection fixtures likewise followed
  upstream pronoun rules; explicit name selections exercised those columns.
- An active arithmetic binding that always returned the same base operator
  was read more than once. Extra reads alone were not treated as a finding;
  the false metadata and wrong source membership above made F1 actionable.
- SQL and Arrow nesting refusals followed the documented local/dtplyr scope.
  The investigation did not request support for list-column nesting there.
- Arbitrary specification field methods, unbounded/exponentially large plans,
  other operating systems and dependency versions, every remote inspection
  backend, and large-scale performance were outside these completed probes.
  The new local sweeps exercised sort settings but supplied no additional
  independent general row-order oracle.

This finite search did not prove absence of further defects. It produced one
reproducible ticket without implementing its repair.

## Verification and environment

The focused existing suite completed with exit zero and no reported failures
across 14 files, covering capture, cycles, dot-dot names, dtplyr special nesting,
interface/plan/inspection, selection budgets, injected specifications,
nest operations, `.rows`, nested data masks, packed grouping and sf nesting.
The exact invocation and output were retained in the coordinator evidence.

All changed files were Repository-only under `design/agents/local-checks.md`;
the package-affecting Review-ready check was therefore not required. Both
linters and the context-budget and document-reference verifiers were run before
push, with their final terminal outcomes recorded on the pull request. The
first jarl run identified an internal namespace access in an investigation
probe; changing its accessor preserved the probe and allowed a clean rerun.
No CI monitoring was performed.

Measurements used macOS arm64, R 4.6.1, marginplyr 0.1.0, dplyr 1.2.1,
dtplyr 1.3.3, data.table 1.18.6.1, dbplyr 2.6.0, rlang 1.3.0,
tidyselect 1.2.1, tibble 3.3.1, vctrs 0.7.3, tidyr 1.3.2 and bit64 4.8.6.
The logs retained session details. No dependency was installed or changed for
this investigation.
