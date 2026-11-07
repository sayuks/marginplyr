# Local nesting types and independent deferred-namespace replay

Investigated: 2026-11-07
Tested package SHA: `0b5143133c023ccb4efb6fe27f1ef551e9f49186`

This lane extended the recent November 6 and November 7 Nest investigations
without changing package implementation or permanent package tests. CONTEXT,
the Nest references and ADRs 0016/0018 supplied membership, payload, result
shape and Margin order boundaries. ADR 0019's *isolate generated namespace
operations* amendment supplied the generated-function ownership boundary.

## Independent confirmation: deferred Nest namespace dispatch

`namespace-independent.R` independently replayed the coordinator's candidate
with different source names and values. An immutable dtplyr source held
`district = north/south` and `weight = 41/67`. A selective root namespace
operator was installed after ordinary lazy nesting construction. For the
row-wise verb the binding was present when the verb performed its documented
collection. No caller summary expression was supplied.

| Selective lookup | Both verbs' observation | Callback reads per verb |
| --- | --- | --- |
| `base::min` | Both outer keys became missing; cells retained 41/67. | 2 |
| `base::seq_len` | Both outer keys became missing; cells retained 41/67. | 2 |
| `vctrs::vec_slice` | Both outer keys became `SHADOW`; cells retained 41/67. | 2 |
| `base::list` | Outer keys stayed correct; both payloads became -311. | 4 |
| `data.table::as.data.table` | Outer keys stayed correct; both payloads became -311. | 2 |

All ten target/verb comparisons silently changed package-generated results.
The coordinator published the shared repair as
[#1427](https://github.com/sayuks/marginplyr/issues/1427), labelled `bug` and
`ready-for-agent`, with no blockers.
Ordinary input collection, explicit native tidyr nesting and ordinary grouped
counts retained 41/67 and invoked zero selected callbacks. Removing the binding
restored results identical to the original. Serialized source bytes stayed
unchanged. The independent observation invocation retained session `81288`
through exit 0; desired-contract mode retained `32830` through exit 1 at
`all(r$matched)`, after every native/restoration/source control passed.
The CSV contains all ten completed comparisons.

The separating hypotheses were generated call-head lookup in the lazy root,
source-copy interception, an ordinary native nesting limitation, or source
mutation. Only the generated Nest lookup hypothesis matched these controls:
the callback targeted only one displayed helper/cell constructor at a time,
ordinary upstream controls never read it, and source bytes were unchanged.
The same result built before a binding changed was affected, establishing a
deferred boundary rather than a construction-time source selection.

The lane read complete bodies/comments for #1089, #1238, #1355 and #1422 and
repair PRs #1119, #1259, #1357 and #1426. #1089/#1119 qualified the previously
bare representative helpers; #1238/#1259 qualified inner and outer list
constructors. #1355/#1357 protected generated ordering lookups, and #1422/#1426
protected generated source copies. Their scoped repairs did not protect the
shown Nest cell/representative namespace operations. All-state Issue/PR
searches used `namespace nest`, and an Issue search used
`"as.data.table" "namespace"`; the latter returned no match. The parent
investigator owned the wider duplicate search and ticket publication. The five
lookups supported one shared Nest generated-expression repair slice, with no
prefactoring prerequisite established by this lane.

An early native tidyr control incorrectly used `.by`, which dtplyr 1.3.3 read
as a named nesting column rather than the intended fixed-key option. The final
control explicitly selected `data = weight`. A preliminary replacement list
constructor also qualified its own delegation through the operator under test,
causing harness recursion. The final constructor delegated through the
resolved exported function. Neither preliminary failure was a package finding;
retained final logs contain the corrected controls and silent wrong results.

## Exotic local type frontier

`exotic-frontier.R` made 336 calls with typed-missing labels and another 336
with `--display` selecting a string label. Fourteen type families crossed
empty/populated sources, both Nest verbs, both keep choices and all three
Margin orders. The types were hms, units, integer64 above 2^53, haven labelled,
SPSS-labelled, tagged NA, zoo yearmon/yearqtr, AsIs POSIXlt, blob, Surv,
distributional normal, sf geometry and posterior rvar. Each populated source
also held a fixed key with a missing value, a second character dimension, an
integer source identifier and a second copy of the typed column as payload.

An independently enumerated cube and original-source vctrs groups supplied
expected source memberships. Successful results had to match complete cell
projections, required column order and original within-cell source order.
For requested Margin orders, an independent key used fixed-key missingness,
Grouping bits derived from the explicit sets, dimension missingness/values
and the public Grouping set identifier. Native dplyr ordering had to leave the
returned row sequence unchanged. The scripts asserted every classified outcome.

The typed-missing matrix recorded 252 semantic matches and 84 refusals. Twelve
populated AsIs POSIXlt payload cases failed ordinary branch combination; 24
Surv grouping cases recursed; 24 sfc and 24 rvar omissions received a safe
representability refusal. The string-label matrix recorded 288 semantic
matches and 48 documented structured-key label refusals (AsIs POSIXlt and
Surv). No new local semantic mismatch was established.

`frontier-controls.R` minimized the exclusions. A Surv dimension recursed under
native tidyr nesting and included-only Margin nesting, whereas Margin
expansion succeeded. A direct AsIs POSIXlt dimension and four verb/keep
controls succeeded after its existing repair; adding the same unsliced class
as payload made ordinary dplyr union and Margin nesting raise the same cast
condition. That did not establish an additional package type-restoration
obligation under ADR 0016. rvar serialization changes also occurred under
ordinary vctrs grouping, through posterior's mutable cache metadata; the
matrix retained serialization observations separately from semantic verdicts.
Two included-only rvar Margin controls retained their source memberships.
The known rvar omitted-prototype proxy behavior was already recorded by closed
#1069, whose full body/comments were read, and was not filed again. Closed
#1236/#1275 supplied the completed direct/packed POSIXlt dimension comparison;
closed #1178 supplied the repaired array-shaped scalar-display boundary.

The control oracle rejected deliberately missing, duplicated and extra source
memberships. The final matrix logs retained sessionInfo and dependency
versions. Historical probe counts from older investigations were not added to
these new 672 comparisons.

## Replay and limits

Run each command separately from the tested source root, optionally replacing
the source/output paths. Assertion mode intentionally exits 1 on this SHA.

```sh
Rscript investigation/2026-11-07-nest-spec-deep/local/exotic-frontier.R . /tmp/output
Rscript investigation/2026-11-07-nest-spec-deep/local/exotic-frontier.R . /tmp/output --display
Rscript investigation/2026-11-07-nest-spec-deep/local/frontier-controls.R .
Rscript investigation/2026-11-07-nest-spec-deep/local/namespace-independent.R . /tmp/output
Rscript investigation/2026-11-07-nest-spec-deep/local/namespace-independent.R . /tmp/output --assert
```

Output directories must exist. The retained matrix output directory was this
lane's directory. Final typed-missing execution `32038` and display execution
`56865` were collected to terminal exit 0. These were direct R executions with
`TESTTHAT_PARALLEL=false`; no suite, package build, renderer or Review-ready
invocation was started by this lane. Additional exploratory controls remain in
`triage-exotic.R` and `rvar-controls.R` with their dated logs.

The environment was R 4.6.1 on aarch64 macOS, dplyr 1.2.1, dtplyr 1.3.3,
data.table 1.18.6.1, tidyr 1.3.2, vctrs 0.7.3, tibble 3.3.1, rlang 1.3.0 and
posterior 1.7.0. These finite probes did not establish all arbitrary classes,
all Grouping specifications, upstream methods, interruptions, resource limits,
other dependency versions or operating systems. Arbitrary attribute
restoration was not asserted.
