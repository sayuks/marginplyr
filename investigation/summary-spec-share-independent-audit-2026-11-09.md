# Independent audit of Margin summaries, Grouping specifications and shares

Investigated: 2026-11-09
Repository snapshot: `5835fd6788d0fddd4cf42a2aa524a62072f36814`

The investigation began from freshly fetched main in the new worktree
`/private/tmp/marginplyr-independent-root-20261109`, branch
`investigate/summary-spec-share-independent-20261109`. Re-fetches before
Issue publication resolved to the same SHA. The package implementation,
package tests, dependencies, public specifications and generated documentation
were unchanged by this investigation.

Three fresh-context investigators used separate detached worktrees at that
SHA. A started from implementation, expression rewriting and share staging;
B from public contracts, Grouping specifications and admission; C from backend
consistency, condition propagation and input preservation. Their initial
candidates were generated independently before reports were shared. The
coordinator separately prepared input generators and explicit-grain reference
calculations, then independently replayed the confirmed findings.

Each worktree ran at most one reproduction at a time, explicitly setting
`TESTTHAT_PARALLEL=false`. Execution handles were retained through terminal
completion; no broad suite, build or renderer was used for discovery. SQL
work used only process-owned in-memory SQLite and DuckDB databases with
synthetic temporary tables. DuckDB used `shared_home=FALSE`; both connections
were disconnected on exit, with DuckDB shutdown. No existing database or
outside user data was accessed.

## Sources and independent expectations

The investigators read CLAUDE.md and its AGENTS.md reference, CONTEXT.md,
the local-check, integration, issue-tracker and investigation procedures.
Public summary, Grouping-constructor, Grouping-helper and share references
supplied the contracts. Relevant decisions included ADRs 0007, 0008, 0010,
0016, 0017, 0018, 0019, 0021, 0026, 0028, 0029, 0033 and 0035.
Required documents were read through complete ranges; truncated reads were
completed with the missing ranges.

The November 7 carrier/transfer investigations, November 8 discovery,
callable-mutation and member/geometry investigations, and November 9
evaluation-boundary investigation supplied historical exclusions and repaired
controls. Implementation history through the investigated main was examined,
including the first/later across function repairs and parenthesized identity
repair. Historical executions were not counted as new evidence.

Expected-value generation was separate from input generation. The coordinator
listed each intended grouping set explicitly, aggregated it with ordinary
dplyr, separately aggregated fixed-key denominators, and compared values,
names, types and cardinality with R assertions. B independently expanded
generated set/union/rollup/cube/product trees without package plan internals.
C's SQL oracle performed separate ordinary database-backed summaries at each
explicit grain and denominator grain, preserving each engine's NULL aggregate
semantics. Agreement between helpers sharing internal code was never the
sole correctness oracle.

## Confirmed findings

The `to-tickets` workflow published three independently verifiable repair
slices with `bug` and `ready-for-agent`. Each had contract sources, a minimal
reproduction, expected/observed results, impact, SHA/environment, acceptance
criteria, dependencies and no blockers. No prerequisite prefactoring was
established. The user delegated splitting and publication without a
confirmation checkpoint. No repair was implemented.

| Boundary | Established effect | Issue |
| --- | --- | --- |
| Known closure-value returns | Seven base operations lost the returned callable's invocation dependency and bypassed documented share refusals. | [#1545](https://github.com/sayuks/marginplyr/issues/1545) |
| Ordinary across argument planning | Grouping helpers in `.cols` or `.names` reached their outside-summary stubs when share schema planning evaluated the raw arguments. | [#1546](https://github.com/sayuks/marginplyr/issues/1546) |
| Ordinary source selector lifetime | Adding a share froze the first input-column choice across grouping sets and silently changed the source aggregate and ratio. | [#1547](https://github.com/sayuks/marginplyr/issues/1547) |

### Known closure-value returns

This minimized witness used only a local synthetic input:

```r
library(dplyr)
library(marginplyr)
d <- data.frame(g = c("a", "b"), x = c(1, 3))
summarize_with_margins(
  d, prior = sum(x),
  value = (function(prior) {
    f <- base::structure(function() sum(prior))
    base::rm(prior)
    f()
  })(11),
  p = share_of_total(value), .grouping = rollup(g)
)
```

The required answer was a Package condition: the invoked closure reads an
earlier summary alias after its formal binding is removed. The observed
values were `1, 3, 4` and shares `.25, .75, 1`, without refusal. Replacing
structure with identity, defining the function directly, or removing the
formal before closure construction restored the refusal. Retaining the
formal or using an independent constant function was legal.

Saving a known closure and invoking its return through `base::unname()`,
`base::withVisible()$value`, `base::eval(base::quote(...))`, `base::evalq()`,
literal `base::get0()` or `base::forceAndCall(1L, base::identity, ...)`
reproduced. Named argument variants and double-bracket access to withVisible's
value reproduced. The immediate lookup was not the missing dependency: the
closure was created while its formal still hid the mask, and its returned
callable proof was lost before invocation in the changed scope.

The 432-call matrix crossed seven failing transfers plus direct/identity
controls, four inputs, both helpers, both dependency directions and three
binding/body conditions. It found 112 missing refusals and 32 proper refusals;
all 288 legal controls returned the expected values. Serialized inputs stayed
unchanged. With an input column named prior, eager source cases used earlier
summary aliases while immutable dtplyr cases could use input values instead.
The reverse arrangement also admitted ordinary reads of an earlier share.

The coordinator independently verified seven missing refusals, two proper
refusals and nine legal controls from its own archive. The desired-contract
assertion exited one. Existing known-closure transfer tests covered repaired
identity/force/get/match.fun and converters, and closure-list tests covered
some operation names, but not these direct callable-return paths.

### Grouping helpers in ordinary across arguments

This witness deliberately kept the problematic ordinary output unrelated to
the only share source:

```r
summarize_with_margins(
  tibble(g = c("a", "b"), x = c(1, 3)),
  across(if (grouping_bit(g) == 0L) "x" else "x", sum,
         .names = "ordinary"),
  total = sum(x),
  across(starts_with("total"), share_of_total, .names = "{.col}_p"),
  .grouping = rollup(g)
)
```

Explicit grains `(g)` and `()` give ordinary and total values `1, 3, 4` and
shares `.25, .75, 1`. Both selector arms select x. Instead, construction
reached grouping_bit's outside-summary stub. The name-template counterpart,
`across(x, sum, .names = if (grouping_bit(g) == 0L) "ordinary" else "ordinary")`,
failed too. grouping_id and both shares reproduced. A direct share of total,
a literal ordinary selector/template, and no-share controls succeeded.

Thus the failure did not require treating a dynamically named or packed
ordinary output as an eligible source. total was an explicit, unique,
self-contained numeric scalar. Name-pattern share selection was documented
as supported. Ordinary n() controls worked in the carrier mask, separating
missing Grouping-helper rewriting from absence of an ordinary dplyr mask.

The first-across resolver evaluated raw columns before the Grouping rewrite;
its planner then evaluated the raw name template during output-name
prediction. The 90-call argument matrix found 24 selector/template false
refusals; its 30 no-share cases and 24 literal/function-argument share controls
matched explicit ordinary dplyr grains. Twelve dynamic-unpack source
refusals were separately excluded below. The coordinator independently
verified eight source-related failures and both unrelated-source witnesses.
B independently reproduced the stronger unrelated-source distinction.
The desired-contract replay exited one.

Existing first-across tests exercised Grouping helpers in function arguments,
ordinary selectors, and name/warning callbacks. They did not assert these two
helper-bearing argument paths. B also observed the raw-selector failure under
a parenthesized ordinary across; the reported boundary was not claimed to be
exclusive to one spelling.

### A stable output schema did not preserve ordinary input selection

The following source had one statically named numeric scalar at every grain:

```r
d <- tibble(g = "a", x = 1, y = 10)
calls <- 0L
choose <- function() {
  calls <<- calls + 1L
  if (calls == 1L) "x" else "y"
}
summarize_with_margins(
  d, across(all_of(choose()), sum, .names = "measure"),
  p = share_of_total(measure), .grouping = rollup(g)
)
calls
```

With fresh selector state for each comparison, ordinary dplyr at explicit
grains `(g)` and `()` returned measure `1, 10` with two selections. The
no-share Margin control agreed. Adding the share returned measure `1, 1`,
shares `1, 1` rather than `.1, 1`, and one selection. On two detail groups,
the independent values `1, 3, 40` became `1, 3, 4`. The wrong aggregate was
already displayed, so choosing a duplicate denominator occurrence could not
explain it.

The first-across path resolved the input selection once and retained it in
every source branch; only unrelated ordinary across outputs resumed their
original selector. The source output name measure and its scalar type/count
stayed constant, so this was not changing output schema or an earlier-summary
alias dependency.

The 150-call matrix found 30 source mismatches across three eager classes,
both helpers and five fixed-key/duplicate/sort configurations. Its 120
plain/literal/unrelated direct/unrelated pattern-share controls matched the
explicit-grain values, denominators and selector counts. Inputs stayed
unchanged. Both parentheses forms also froze the selector: ordinary dplyr and
no-share Margin per-group values `1, 30, 40` with three selections became
`1, 3, 4` with one selection. A later unresolved selector source and an identity
wrapper were refused without running the selector; those were admission
controls, not additional value findings.

An initial interpretation excluded this difference because a private header
said source summaries retained their planned schema, and a test expected one
source name-template evaluation. That interpretation was withdrawn after
checking what the test asserted: its selector always returned x, and only its
name-template count was pinned. Neither that test nor ADR 0019's own-planning
sentence established a public exception allowing ordinary input choices to
freeze while output schema stayed identical. Ordinary summary semantics and
ADR 0017's stateful-source amendment supplied the value contract. A, B and
the coordinator independently confirmed the difference; the coordinator's
desired-contract assertion exited one.

## Refuted candidates and finite coverage

| Hypothesis | Evidence and result | Disposition and next evidence |
| --- | --- | --- |
| Ordinary Total denominators mix fixed partitions, missing keys or occurrences | The coordinator's 1,400 explicit-set comparisons covered 70 independently generated empty/small inputs, five families, fixed-key presence and both missing-value sum policies. Values, names, types and cardinality matched. | Refuted for this finite family; arbitrary classes were not inferred. |
| Nonfinite local summaries break missing-value or ratio arithmetic | Forty-eight local/dtplyr comparisons covered positive/negative infinity, their sum, NaN, NA and zero, both sum policies and fixed-key presence. Ordinary grain sums and separate R division matched. | Refuted for these local cases; no portable SQL nonfinite guarantee was inferred. |
| Source/output overlap or unusual names break later ordinary masks | Seventy-eight comparisons crossed 13 names, direct/across/unnamed-frame forms and fixed-key presence. Per-set ordinary dplyr results matched, including changed summary aliases. | Refuted; independent per-set comparison avoided accidental name repair during oracle combination. |
| Composite/product/union overlap corrupts plan order or root admission | B's 702 generated comparisons gave 120 duplicate refusals, 198 no-root share refusals and 384 matching accepted plan/value/type/identifier results. | Refuted for generated depth-at-most-two families and identity/empty/overlap controls. |
| Structured keys break denominator equality or Margin order | Forty local comparisons covered numeric NA/NaN/Inf, factor NA levels, list/list_of, matrix, packed-frame, array and zero-width keys, fixed/dimension roles and first/last ordering. | Values, types and exact independently constructed order matched; arbitrary extension ordering remained unproven. |
| Constructor/caller-name ambiguity changes with parentheses or injection | One hundred parent-kind/child-kind/argument-form comparisons used active bindings and asserted the admitted grammar, refusal and read count. | Matched ADRs 0008/0026; new factory behavior would require new evidence. |
| Unnamed-frame carriers change local values or later summary masks | C's 78 comparisons used 13 frame/NULL/empty/field shapes, later pick absent/present and three eager classes. Accepted values and refusal outcomes matched ordinary dplyr. | No new discrepancy; both-error pairs did not establish diagnostic identity. |
| dtplyr failure paths mutate shared input graphs | Twenty fresh-input cases crossed five root/cache/join/union graphs with four summary/cardinality/label failures, then repeated successful summaries. | Inputs stayed unchanged and recovery values/shares matched. An initial ordinary collect control itself sorted/renamed upstream input and was excluded. |
| An unrelated multirow ordinary result silently recycles a scalar share source | Named, unnamed-frame and post-share multirow cases were compared with a scalar control. | Dplyr 1.2.1 refused non-scalar summaries before local shares; no new defect. |
| SQL empty/NULL/zero/type/name/duplicate intersections differ from ordinary SQL | Twenty-four SQLite and 24 DuckDB comparisons used separate per-grain and denominator queries, two missing fixed keys, rollup/cube/duplicate plans, typed-missing/string labels, sorting and source/output overlap. | All matched values, row counts, fixed partitions, occurrences and declared types; sorted cases matched independent order. SQLite explicitly asserted the known numeric source with `.check_share_source=FALSE`. |
| Dynamic false unpacking is another helper-context bug | General-call FALSE unpack expressions and identity(FALSE) both had unavailable static source schemas; unrelated-source controls succeeded. | Excluded as the existing static source-eligibility boundary. |
| A custom frame cast could add a protected source field after validation | No lawful, concrete subclass establishing this behavior was produced. | Unconfirmed. Required next evidence: valid vctrs type/cast laws and an ordinary public dplyr comparison showing an actual overwritten source. |

Harness-only parsing mistakes, NULL-versus-empty set representation and cube
occurrence order were corrected before the final comparisons. The name oracle
initially introduced dplyr/tibble dot-name repair while combining expected
branches; comparing grains individually removed that artifact. An initial
selector used invalid bare-column code inside a tidyselect block and was
replaced with a valid string-valued selector. None was counted as a defect.

After the three reports were integrated, the coordinator revisited selection
lifetimes, parentheses, raw argument rewriting, source eligibility and known
transfer neighbors. The arithmetic-selector finding emerged during that
recheck. No promising executable hypothesis remained unclassified at the
checkpoint; finding count did not determine the stopping point.

## Tracker reconciliation, evidence and limits

Duplicate discovery exported all-state bodies for 796 Issues and 745 PRs,
searched titles and bodies, and fetched relevant bodies/comments. Queries
included withVisible, forceAndCall, get0, evalq, unname, structure, closure
transfer, first across, selector/source, selection cache and planned schema.
Selected comparisons included #130, #1389, #1401, #1402, #1430, #1482, #1491,
#701, #871, #1083, #1447, #1527, #1529 and #1530. Direct recent-Issue/open-PR
queries supplemented indexed searches, which returned no results for two
queries and were not treated as sufficient evidence alone.

The repaired transfer/converter and across-function cases remained passing
controls. The three new slices concerned respectively returned callable
proofs, pre-rewrite Grouping-helper argument evaluation, and retained ordinary
input selections. They could be verified and repaired independently.
Concurrent PR #1544 recorded a Grouping-inspection audit with no new defect;
its body did not overlap these reports. Published #1545–#1547 were fetched
back and their bodies, open state and labels verified.

Raw R scripts, logs, assertion results and checkpoints were kept outside the
submitting worktree under `/private/tmp/mplyr-independent-evidence-{root,a,b,c}`.
The three minimal reproductions above and the Issue bodies retained the
essential public witnesses. Coordinator observation-mode replays exited zero;
the three desired-contract modes exited one on the investigated SHA. Those
are reproduced failures, not successful package-check results.

The additional nonfinite comparisons used a disposable archive of research
commit `af7744a5cf088c5a6ce43d5b2f1532df6ef4f3ef` during its source check.
That commit added only this note to the investigated main; its package code,
tests, dependencies and installed files were byte-unchanged. Its execution
handle reached exit zero, and all reproduction work was collected before the
source check's terminal result was handled. Defect reproductions remained
evidence for the original main SHA, separately from this supplementary run.

Environment: R 4.6.1 on arm64 macOS Tahoe 26.6.2, dplyr 1.2.1, dbplyr 2.6.0,
rlang 1.3.0, vctrs 0.7.3, tibble 3.3.1, data.table 1.18.6.1, dtplyr 1.3.3,
DBI 1.3.0, RSQLite 3.53.3 and DuckDB 1.5.5. SQL checks used no unavailable
backend. Live PostgreSQL, Arrow execution, sf execution, direct SQL
materialization, other platforms/versions, arbitrary dispatch/reflection,
concurrency and unbounded plans were not established by this round. The
finite matrices were not an exhaustive proof over R programs or backends.

This submission added only a dated research note. Its consumers were
investigation readers; it changed no execution, package test, dependency,
installed file, generation source, build exclusion or verification behavior.
Those effects justified Review-ready source scope and no NEWS entry.
Package build/check, the complete suite and snapshots were omitted. The
submitted SHA, fetched-main compatibility assessment, mandatory baseline
outcome and separate review record belonged on the PR. CI monitoring was
outside the requested work.
