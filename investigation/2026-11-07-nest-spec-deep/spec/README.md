# Wide Grouping plans and retained selection occurrences

Investigated: 2026-11-07
Tested main: `0b5143133c023ccb4efb6fe27f1ef551e9f49186`

No new actionable defect was established in this lane. Two finite public-API
matrices added coverage beyond the four-column family algebra and authored
namespace/capture/continuation matrices in the earlier November 7 notes. Their
historical executions were not counted here. No implementation or permanent
package test was changed.

## Wide plans: 5,328 exact comparisons

`wide-plan.R` crossed 0, 1, 2, 15, 30, 31, 32, 33, 64 and 96 variable
dimensions; eight deterministic column permutations; grouping-set unions,
rollups, cubes of up to six composite dimensions and overlapping Cartesian
products; `keep`, `drop` and `error`; list/text inspection; and absent fixed
keys, explicit `.by` or pre-existing grouped fixed keys. Zero-dimensional
fixtures used the union family alone, avoiding invalid empty rollups/cubes.
Special source names included commas, equals signs, parentheses, backticks,
backslashes, newlines, carriage returns, leading/trailing spaces, Japanese,
emoji, `.env`, `.data` and the noncanonical numbered-dot-like name `..01`.

All 4,416 successful-plan comparisons matched the independent complete-tibble
oracle exactly. All 912 duplicate-error comparisons raised the required
Package condition. The oracle modeled family order directly, canonicalized
dimension membership in first-appearance order and checked every inspection
field. Grouping identifiers used left-to-right multiply-and-add accumulation,
independently of the package's matrix multiplication. The 31-column signed
integer boundary, `NA_integer_` beyond 31 columns and complete named Grouping
bits were included. Text names were treated as human-readable output, not as
serialization, as ADR 0013 requires.

## Retained occurrences: 2,280 comparisons and 5,580 binding reads

`retained-occurrences.R` reused each specification over five public inspection
calls. One to four independent occurrences of the same active binding rotated
through character, integer, predicate-function or nested-specification answers.
The matrix crossed all five constructors where the grammar admitted that
answer, and six wrapper forms: bare, syntactic parentheses, five syntactic
pairs, a public namespace pair, a parenthesized public namespace callee and a
mixed qualified/syntactic pair.

All 1,900 successful plans matched literal-answer references, and every
expected per-occurrence/per-call read count matched. Three hundred bare
function-binding cases matched direct tidyselect's condition class and exact
diagnostic: a bare function-valued symbol is not a predicate call. Eighty
parenthesized-qualified-callee specification returns received the known
unrecognized-nested-spelling Package refusal, after one binding read. Total
observed active-binding reads were 5,580. This finite comparison refuted stale
retained values shared across occurrences or later calls, and wrapper-induced
rereading for the admitted forms.

The initial oracle incorrectly expected the 300 native refusals and 80 grammar
refusals to produce plans. `retained-occurrences-initial-oracle.log` records
that rejected oracle. Its mismatch count was not a finding. The final script
paired native refusals independently and classified the known spelling
boundary explicitly before the retained complete rerun; the final failures
files were empty lists.

## Hypotheses and dispositions

| Contract and boundary | Distinguishing observation | Disposition |
| --- | --- | --- |
| ADR 0013's complete inspection result at 31/32 dimensions | Exact integer identifiers at 31, missing identifiers after it, complete bits at every size | Refuted over the wide matrix |
| ADR 0008's duplicate-drop composition and first-occurrence order | Union/product/cube output differing from an independently enumerated family | Refuted over the finite families |
| ADR 0008's retained-value evaluation budget | An active binding read twice per occurrence, or a prior call's answer reused | Refuted over the retention matrix |
| Implicit matrix classes dispatch during ordinary selection | A names-only atomic matrix selector invoking a typed-dependent custom proxy | Refuted exploratory probe: direct tidyselect refused matrices before such dispatch; ordinary internal primitives did not dispatch on implicit classes |
| Malformed argument elements imply a new public validation promise | A constructor-built object reaching an invalid internal argument shape | No such public construction was established; ADR 0015 does not promote forged internal objects into public guarantees |

The exploratory implicit-class executions and forged-object source inspection
were excluded from matrix counts. No new candidate required a publication decision. All-state
tracker discovery used `gh issue list --state all` with recent titles and
searches `capture binding evaluation occurrence`, `active binding
specification`, `malformed specification` and `quosure specification invalid`;
the PR search used `grouping retention`. Complete bodies/comments of #1400,
#262, #1321 and #1245 distinguished earlier callee-rebinding, malformed-field
and duplicate-compaction reports. The earlier lane notes distinguished
constructor-valued parentheses from ordinary selection-call spellings.

## Replay and limits

From the repository root, select the tested source explicitly and an existing
output directory:

```sh
MARGINPLYR_HUNT_SOURCE=/path/to/pinned-source \
MARGINPLYR_HUNT_OUTPUT=/path/to/output \
Rscript investigation/2026-11-07-nest-spec-deep/spec/wide-plan.R

MARGINPLYR_HUNT_SOURCE=/path/to/pinned-source \
MARGINPLYR_HUNT_OUTPUT=/path/to/output \
Rscript investigation/2026-11-07-nest-spec-deep/spec/retained-occurrences.R
```

Both final executions terminated with exit zero, through retained terminal
sessions 81461 and 88513 respectively. The initial retention execution 50202
terminated with exit one for the invalid oracle; all owned executions were
collected. The scripts explicitly disabled testthat parallel execution and
started no subprocess, database connection or server. No suite, package
build/check, rendering or Review-ready invocation ran in this lane.

The provisioned environment was arm64 macOS, R 4.6.1, source-loaded marginplyr
0.1.0, dplyr 1.2.1, tidyselect 1.2.1, rlang 1.3.0, vctrs 0.7.3, tibble 3.3.1
and pkgload 1.5.3. Other platforms/library versions, arbitrary dispatch methods,
unbounded family sizes, general warning identity and remote metadata/query
counts remained outside these local comparisons. Finite successful matrices
did not establish exhaustive correctness of arbitrary R programs.
