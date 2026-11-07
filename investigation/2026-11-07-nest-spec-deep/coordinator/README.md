# Coordinating differential, minimized witness and duplicate comparison

Investigated: 2026-11-07
Tested main: `0b5143133c023ccb4efb6fe27f1ef551e9f49186`

## Confirmed generated nesting lookup

`nest-namespace.R` constructed an immutable dtplyr nest before installing a
selective root namespace operator. Only its generated data.table converter
changed behavior. The cells contained -99/-99 instead of source values 13/29;
ordinary source collection and removal of the binding retained 13/29, and
serialized source bytes stayed unchanged. The final observation execution
`17472` exited zero. The desired-contract command below exited one after its
controls, at the literal payload equality assertion. This was intentional
research evidence, not a failing package test.

```sh
Rscript investigation/2026-11-07-nest-spec-deep/coordinator/nest-namespace.R
Rscript investigation/2026-11-07-nest-spec-deep/coordinator/nest-namespace.R --assert
```

The scripts accepted a source root as their first argument, allowing a
different source archive to be loaded. The second command's default source
was the repository root. The local lane independently changed names, values
and callback effects and confirmed ten silent wrong-key/payload outcomes.

The ranked hypotheses were generated late call-head lookup, ordinary input
expression lookup, source mutation and construction-time evaluation. Native
input/grouped-count/explicit-tidyr controls, serialization and late rebinding
separated them. The generated lookup hypothesis alone matched the observations.
The source selected no namespace expression; the callback names came from the
package-generated cell/representative expressions.

`nest-namespace-matrix.R` crossed four targets, both verbs, early/late bindings,
one included set/rollup, keep choices and all three Margin orders. All 192
classifications matched: 144 generated callback errors and 48 correct
post-collection row-wise controls. Every case also checked native collection,
grouped count, explicit tidyr nesting, source bytes and restored results.
The final retained execution `20079` exited zero. The initial expectation that
a late binding would change an already-collected row-wise result was false;
that oracle was corrected before the final complete rerun.

`nest-namespace-frontier.R` added six targeted callback probes for zero-payload
cell construction, special payload-name restoration and declared-NA-factor
identity construction. All six intercepted the selected generated lookup;
restoration/source controls passed. Execution `2725` exited zero. These probes
established a reachable callback boundary, not additional silent-value cases.

An initial replacement converter recursively called the namespace operator
under test. It was corrected to delegate through the exported function lookup
before the retained run. An initial native tidyr control used `.by`, which
dtplyr 1.3.3 treated as a nesting-column name. The final native control used
explicit `data = -region`. Neither harness failure established package behavior.

## Ordinary local/dtplyr differential

`dt-nest-differential.R` compared 720 local/lazy result pairs across fifteen
dimension types, zero/four rows, both verbs, keep choices, fixed-key presence
and all Margin orders. It compared complete outer keys and original cell
projections after sorting only within-cell source identifiers; backend element
classes were intentionally delegated. It recorded 520 exact matches, eight
NA/NaN order differences and 192 refusal pairs. Execution `78972` exited zero.

The eight differences swapped distinct missing numeric keys. The separate
`missing-order-controls.R` replay paired each backend with its own native
arrange expression: local nesting and native arrange returned NA before NaN;
dtplyr nesting and native arrange returned NaN before NA. All eight backend/
verb/order comparisons matched native value ordering, with both missing kinds
after nonmissing values. Source membership and cell projections also matched.
No package-specific order defect was established by these comparisons.

`native-refusals.R` paired unsupported shapes with ordinary backend behavior:
complex included-only nesting worked but ordinary data.table union refused;
plain-list grouping was refused by native tidyr/data.table; AsIs matrix input
construction itself recursed before a Margin verb. Raw keys lacked a
representable typed-missing Margin value and received the Package refusal.
The complete control script exited zero, reporting each refusal. An earlier
ad-hoc probe had stopped at the native matrix constructor and was superseded
by per-construction error capture. The initial differential also corrected a
zero-length detail field and made fixed-key selection explicit before its
retained run; neither was a package finding.

## Duplicate search and publication

All-state CLI exports held 721 Issues and 703 PRs before publication. They
were scratch data outside the submitting worktree, not committed tracker
copies. Live Issue searches used `"namespace" "nest"` and
`"namespace" "nesting"`; the PR search used `"namespace" "nest"`.
The local lane additionally searched `"as.data.table" "namespace"`.

Complete relevant discussions distinguished [#1089][display]/[PR #1119][display-pr]
and [#1238][list]/[PR #1259][list-pr], which qualified bare display/cell helpers,
from [#1355][order]/[PR #1357][order-pr] and [#1422][copy]/[PR #1426][copy-pr],
which protected ordering and source-copy namespace operations. No matching
nesting namespace repair was found. Main was fetched again before publication
and still named the tested SHA. One independently verifiable repair slice was
published as [#1427][ticket], with `bug` and `ready-for-agent`, no blockers and
no prerequisite prefactor. The created title, body and labels were fetched
and checked. The user had delegated granularity and waived confirmation.

All coordinating executions were collected before submission. All scripts
disabled testthat parallel execution and launched no subprocess or server.
No complete suite, package build or render was used as a hunting loop. The
environment file recorded arm64 macOS, R 4.6.1, dplyr 1.2.1, tidyr 1.3.2,
dtplyr 1.3.3, data.table 1.18.6.1 and vctrs 0.7.3. These finite comparisons did
not establish arbitrary class dispatch, all possible expressions, other OS or
dependency versions, interruptions or resource limits.

[display]: https://github.com/sayuks/marginplyr/issues/1089
[display-pr]: https://github.com/sayuks/marginplyr/pull/1119
[list]: https://github.com/sayuks/marginplyr/issues/1238
[list-pr]: https://github.com/sayuks/marginplyr/pull/1259
[order]: https://github.com/sayuks/marginplyr/issues/1355
[order-pr]: https://github.com/sayuks/marginplyr/pull/1357
[copy]: https://github.com/sayuks/marginplyr/issues/1422
[copy-pr]: https://github.com/sayuks/marginplyr/pull/1426
[ticket]: https://github.com/sayuks/marginplyr/issues/1427
