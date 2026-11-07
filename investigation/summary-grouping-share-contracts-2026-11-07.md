# Grouping proxy, share dependency and unnamed summary contracts

Investigated: 2026-11-07
Repository snapshot: `0d99de31aa3c23ac36df363e2a655d1c12c0de89`

The dedicated worktree started at the merge of PR #1460. The two handoff
reproductions each exited zero in observation mode and one at the expected
contract assertion. A fresh fetch immediately before issue publication still
resolved main to the tested commit. R 4.6.1 ran on arm64 macOS with dplyr
1.2.1, dtplyr 1.3.3, data.table 1.18.6.1, tidyselect 1.2.1, rlang 1.3.0,
vctrs 0.7.3 and pkgload 1.5.3.

The earlier [invariant](summary-grouping-share-invariants-2026-11-07.md),
[transfer](summary-grouping-share-transfer-bughunt-2026-11-07.md) and
[carrier](summary-grouping-share-carrier-bughunt-2026-11-07.md) hunts supplied
the repaired controls and the scope of earlier reports. Their results were
kept separate from the executions below.

## A. The dtplyr data proxy changed tidyselect's active names

[The replay](2026-11-07-summary-grouping-share-contracts/grouping-proxy.R)
used an immutable dtplyr step with character column a equal to x/y and
integer column b equal to 1/1. A registered
`tidyselect_data_proxy.data.table` method presented b before a. Direct typed
tidyselect on a zero-row data.table, the same selector inside identity, and
literal b agreed. A direct selection of the first name from `peek_vars()`
instead fixed on a. The count had two rows of one where b had one row of two.
With a fixed, the direct selector also falsely reported overlap with a
through all five Grouping constructors. The literal and wrapped plans included
b. Actually choosing dimension a gave the overlap diagnostic. Without
proxy registration, the selector chose the default first column. Every
observed proxy call received zero rows and source serialization stayed equal.

The execution supports a names-only classification gap.
`is_selection_name_lookup()` treats a no-argument `peek_vars()` as settled
by names. Its answer comes from the sequence established by the tidyselect
data proxy. The synthetic receiver in `grouping_name_proxy()` has a
different proxy method from the typed data.table receiver. ADR 0013 requires
inspection to share Grouping-plan semantics, and ADR 0020 constrains
dtplyr metadata acquisition to work that leaves source rows unread.

#1434, #1439 and the open #1463 concern the dispatched `names()` of
different receivers. #1418 and #1419 concern constructing typed metadata.
#995 concerns typed dispatch in selector operands. #1449 repairs the names
tidyselect gives its result. This witness changes the proxy that supplies
the active variables before those result names are repaired.
[#1469](https://github.com/sayuks/marginplyr/issues/1469)

## B and C. Finite paths to a closure's invocation

The [share matrix](2026-11-07-summary-grouping-share-contracts/share-carriers.R)
used a two-row tibble with x equal to 1/3 and a separate input prior equal to
10/20. Each witness stored a local closure that reads the enclosing prior
formal and invoked it after removing that binding. A dependent share source
read the earlier ordinary alias and returned 1/3/4. In the reverse direction,
a later ordinary summary returned input values 10/20/30 beside displayed
shares 0.25/0.75/1. ADRs 0010 and 0017 and the share reference require the
Package condition in both directions.

Each table entry crossed Parent and Total share with the two dependency
directions. Across the seven families, 80 calls missed the intended refusal.
Every case also ran a retained-formal and an independent-closure control.
The alternative member returned 13, so the expected 11 checked member identity
where more than one callable was available. The direct list witness was
refused for the intended reason. The repaired group checked identity,
literal subsets, known lists carried through inline formals, a factory that
returns the captured closure in a list, exact member invocation, flat and
literal-if writes, nested literal paths, and legal defaults, empty iterations
and environment-field replacement. Source serialization stayed equal.

| Boundary | Separating evidence and earlier scope | Issue |
| --- | --- | --- |
| Block and selected-branch results (B) | Four forms include a multi-statement block and literal if and switch. `scope_value_closure()` follows these forms for a closure itself. `scope_closure_list()` has no corresponding result handling outside factory traversal. #1401 concerns a closure, #1416 a mask environment and #1430 unchanged transfers through known functions. | [#1470](https://github.com/sayuks/marginplyr/issues/1470) |
| Plain-list transformations | Seven forms include two-member reversal, repetition and tail, removal or replacement of names, a names-only structure, and conversion of a plain list. #1413 repairs concatenation, append and literal subsets. #1431 concerns name assignment to the original list binding. | [#1471](https://github.com/sayuks/marginplyr/issues/1471) |
| Literal element accessors | Exact `getElement()` and `.subset2()` select the member also reached by direct dollar or double brackets. The known list member reader covers dollar and brackets. #1345 and #1385 concern access to a mask environment. #1415 and #1446 concern partial names and bracket indices. | [#1472](https://github.com/sayuks/marginplyr/issues/1472) |
| Inline factory formals | An actual closure, symbol default and dots argument became members of the returned list. An actual that was already a list and an inline factory creating the member in its own body were recognized. #1445 repairs those controls. #1232 covers literal values used in function position. | [#1473](https://github.com/sayuks/marginplyr/issues/1473) |
| Known invocation through `do.call()` | Direct dollar invocation of the member refused. Using the same member as the target with a literal empty argument list succeeded. The planner's separate deferred-call reader handles reflective lookup primitives from #173 and PR #180. | [#1474](https://github.com/sayuks/marginplyr/issues/1474) |
| Single-iteration loop write | A flat member replacement occurred once over literal `1L`; the equivalent flat and literal-if writes refused. An empty loop preserved the independent member. The statement reader carries the index binding but omits the demonstrated member change. #1403, #1443 and #1444 concern flat and nested writes and replacement dispatch. | [#1475](https://github.com/sayuks/marginplyr/issues/1475) |
| Fresh local environment fields | A literal `list2env()` and an ordinary field installation after `new.env()` held the same visible closure. Replacement by an independent closure stayed legal. #1140 and #1185 concern rebinding an environment used as a lookup target. #1404 and #1416 concern an alias of the mask environment itself. | [#1476](https://github.com/sayuks/marginplyr/issues/1476) |

Calculated indices made with `paste0()` and `seq_len()`, and a nonlocal
member write inside `local()`, also admitted the dependent call in the finite
matrix. These observations stayed outside the issues. The literal index
repair in #1446 did not establish a contract for calculated indices. The
`local()` case would need to pin which frame owns the member and which
effects can be propagated through nonlocal replacement, beyond the
inherited-removal repair in #1269 and PR #1299. The matrix exercised local
tibbles only. It established no backend or arbitrary R evaluation guarantee.

## D. A summary frame's displayed names differed from its columns

[The frame replay](2026-11-07-summary-grouping-share-contracts/summary-frame-names.R)
returned a one-row frame subclass with stored column out. The method for
`names()` on populated values reported the reserved public name instead.
Ordinary dplyr expanded out with values 1 and 3 by g. The Margin call falsely
refused the same frame for a fixed key, a Grouping dimension, or a Grouping
set identifier. Reporting out recovered all three outputs. Storing a
genuine collision still failed even when the method reported out.

The runtime validation in `local_assigned_summary_value()` reads
`names(value)`. The expansion in `new_local_summary_value()` uses stored
columns. ADR 0028 describes expansion of the frame's own columns. #1365
fixed the conversion used to carry those columns, #1447 fixed selection on
summary input, and #1312 distinguished temporary Assigned summary names
from real output names. This case isolates the runtime name check.
[#1477](https://github.com/sayuks/marginplyr/issues/1477)

## Replay and publication

Run the scripts from the repository root at the tested SHA, using absolute
script paths if the evidence is stored separately. For example:

```sh
evidence=investigation/2026-11-07-summary-grouping-share-contracts
TESTTHAT_PARALLEL=false Rscript "$evidence/grouping-proxy.R" observe
TESTTHAT_PARALLEL=false Rscript "$evidence/share-carriers.R" contract flow
TESTTHAT_PARALLEL=false Rscript "$evidence/summary-frame-names.R" observe
```

[Captured output](2026-11-07-summary-grouping-share-contracts/replay.txt)
records the observation and intended-contract exit statuses. Observation
asserts the dated behavior. Contract requires the intended values or
refusals. It exited one for each published boundary. The repaired share
group passed in both modes. Each process registered any probe-specific S3
method for its own lifetime.

Duplicate searches used all-state GitHub issue and PR bodies and a live
refresh through #1468. Search terms covered `tidyselect_data_proxy`,
`peek_vars`, closure lists with block, switch or default, literal element
accessors, list transforms, `do.call`, local scope, environment field and
unnamed frame output. Full bodies and comments for the closely related
reports informed the distinctions above. Each new ticket has a public
acceptance boundary and can be started independently.

The submission adds dated prose and standalone research scripts and output.
Their consumers are readers, explicit replay and repository source linting.
The existing source-package exclusion covers the investigation directory.
