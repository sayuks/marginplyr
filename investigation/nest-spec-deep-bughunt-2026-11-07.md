# Nesting and Grouping specification deep boundary bug hunt

Investigated: 2026-11-07
Tested main: `0b5143133c023ccb4efb6fe27f1ef551e9f49186`

The investigation created a new worktree from freshly fetched `main`, on
branch `investigation/nest-spec-deep-20261107`. Three parallel lanes examined
local nesting, Grouping specification semantics and lazy inspection. The
coordinator added local/dtplyr result comparisons and minimized a generated
expression failure, which another lane independently replayed. No package
implementation, permanent test, dependency, generated documentation or
maintained contract was changed.

The recent [registry-boundary investigation][prior] and its linked November
6/7 notes supplied known failures and separating controls. Prior execution
counts were not added to the new comparisons below. The search deliberately
extended those finite corpora; it did not establish exhaustive correctness for
arbitrary R programs or extension methods.

## Finding and repair slice

Both dtplyr-backed nesting verbs allowed a caller's root `::` binding to
intercept package-generated nesting helpers. A previously constructed lazy
nest over region east/west and units 13/29 collected two cells containing
-99/-99 when only the generated data.table converter lookup was changed.
Removing the binding restored the same result, ordinary source collection
retained 13/29, and the source's serialized bytes stayed unchanged.

An independent replay used district north/south and weight 41/67. Selective
lookups for base min/seq_len produced missing outer keys; vctrs slicing
produced SHADOW keys; base list or the data.table converter changed the cell
values to -311. All ten target/verb combinations silently changed keys or
payload while native/restored/source-ownership controls passed. Both
independent desired-contract assertion modes exited one after their controls.

The [coordinating record][coordinator] and [independent local record][local]
retain minimal witnesses, ranked hypotheses, exact commands, outputs,
controls, environment and duplicate comparisons. The namespace qualification
in the prior bare-helper repairs did not isolate the operator that later
resolved those qualified calls. The separate ordering/source-copy repairs did
not cover these generated nesting operations.

The user delegated ticket granularity without confirmation. `to-tickets`
therefore published one complete repair slice, [#1427][ticket], labelled `bug`
and `ready-for-agent`, with no blockers. It covers faithful outer keys,
cell contents and source membership across the generated nesting-expression
boundary. No prefactoring prerequisite was established. This investigation
contained evidence only and did not fix or close the implementation ticket.

## Additional comparisons and exclusions

| Lane | New completed comparison | Observation |
| --- | --- | --- |
| [Local type frontier][local] | 672 calls over fourteen exotic type families, empty/populated inputs, both verbs, keep choices and all Margin orders | 540 semantic/cell/order matches and 132 native, known or documented refusals; no additional new defect. |
| [Wide Grouping plans][spec] | 5,328 comparisons across up to 96 dimensions, 31/32-column boundaries, four finite family forms, duplicate policies, fixed keys and unusual names | 4,416 exact complete-plan matches and 912 correct duplicate refusals. |
| [Retained selection occurrences][spec] | 2,280 comparisons reusing specifications, with 5,580 active-binding reads | 1,900 exact plans, 300 paired native predicate-symbol refusals and 80 known spelling refusals; evaluation budgets matched. |
| [dtplyr metadata][backend] | 15,720 inspection calls across sixteen type fixtures, seventeen graph shapes and ten predicates | 13,624 exact metadata selections, 2,000 safe refusals and 96 correct key-overlap refusals; no row-predicate elements read or source bytes changed. |
| [Arrow schema/readers][backend] | 1,200 physical-type/input-form/predicate comparisons | Every selection matched; 480 fresh readers retained all 960 source rows. |
| [Remote and registry controls][backend] | 96 fresh remote calls and two desired-contract dispatch replays | Admission/read boundaries held; the previous registered bracket/converter examples received safe refusal before callbacks. |
| [Local/dtplyr result differential][coordinator] | 720 paired comparisons over fifteen types and all sort choices | 520 exact matches, eight NA/NaN differences paired with each backend's native ordering and 192 refusal pairs. |
| [Generated nesting namespace matrix][coordinator] | 192 cases and six additional helper-frontier probes | 144 unwanted callbacks and 48 correct post-collection row-wise controls; the six probes reached the same unresolved generated lookup boundary. |

The local oracle independently enumerated source groups, cell projections and
Margin-order terms. Arbitrary input-class/attribute restoration remained
delegated under ADR 0016. Native Surv grouping recursion, native unsliced
AsIs POSIXlt payload combination and posterior cache mutation were separated
from package-owned behavior. The known omitted-rvar proxy refusal was already
recorded in #1069 and was not filed again.

The Grouping oracle modeled union, prefixes, subsets and products separately
from package expansion, compared every inspection field and modeled integer
identifier overflow. The backend oracle selected from empty native results,
with source-row evaluation and serialization checks. The lane records describe
the exact asserted properties; counts of safe refusals did not prove support
for a refused transformation. Native raw/complex union failures and initial
test-harness/oracle mistakes were explicitly excluded from new findings.

## Freshness, replay and limits

Main was fetched again before issue publication and still named the tested
SHA. All-state CLI exports held 721 Issues and 703 PRs; targeted searches and
complete relevant discussions distinguished #1089/#1238/#1355/#1422 and their
repairs. The created issue's title, body and labels were fetched and checked.
Tracker exports stayed outside this worktree. Lane records provide replay
commands, source/output overrides and terminal execution identities.

All owned hunting executions were collected before final verification.
Reproductions used the provisioned arm64 macOS R 4.6.1 library: dplyr 1.2.1,
tidyr 1.3.2, dtplyr 1.3.3, data.table 1.18.6.1, rlang 1.3.0, tidyselect 1.2.1
and vctrs 0.7.3. Backend logs additionally recorded DBI 1.3.0, dbplyr 2.6.0,
RSQLite 3.53.3, DuckDB 1.5.5 and Arrow 25.0.1. Other versions/platforms,
arbitrary dispatch, hosted services, unbounded plans/graphs, interruption
behavior and resource limits remained unestablished.

## Submission effects

The cumulative diff added dated research prose, standalone R reproductions
and captured text/CSV evidence. These artifacts were human/replay evidence,
with no package-check, installed-file, generation or verification-tool
consumer; the unchanged source-package exclusion omitted investigation
content. Package code/tests, examples, vignettes, dependencies, generated
documentation and build rules were unchanged. Those effects justified
Review-ready `source`: package build/check, complete suite and snapshots
omitted. The exact submitted SHA, current-main assessment, mandatory checks
and formal review record belonged on the PR. No NEWS entry was needed for
this research-only submission. CI monitoring was outside the requested scope.

[prior]: nest-spec-bughunt-2026-11-07.md
[coordinator]: 2026-11-07-nest-spec-deep/coordinator/README.md
[local]: 2026-11-07-nest-spec-deep/local/README.md
[spec]: 2026-11-07-nest-spec-deep/spec/README.md
[backend]: 2026-11-07-nest-spec-deep/backend/README.md
[ticket]: https://github.com/sayuks/marginplyr/issues/1427
