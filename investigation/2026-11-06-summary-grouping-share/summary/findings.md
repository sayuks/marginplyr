# Margin summary evaluation lane

Investigated: 2026-11-06
Repository snapshot: `952a0d9b18fd3ff4631f153432f6024ad62fcac2`

The reproductions ran from a disposable source archive of the investigated
snapshot. They used R 4.6.1 on aarch64 macOS, pkgload 1.5.3, dplyr 1.2.1,
rlang 1.3.0, vctrs 0.7.3, tibble 3.3.1 and data.table 1.18.6.1, with
`TESTTHAT_PARALLEL=false`. No package implementation or package test changed.

## S1. Classed frame expansion dispatched over the row value

An unnamed summary returned a one-row data-frame subclass with an `out`
column containing the sum of `x`. Its `as.list()` method returned a different
list representation, with `out = 99L`. With `x = 1:3`, ordinary dplyr expanded
the actual column as six; the Margin summary returned 99. The difference did
not require a selection rewrite, a share, a nonempty Grouping specification,
or more than one group. The `frame(dplyr::pick(x))` spelling reproduced it too.

The reference's *Relationship to dplyr summaries* promised to expand an
unnamed local data-frame value's columns exactly as dplyr does. ADR 0028
placed that expansion into dplyr's active summary mask, and ADR 0016 delegated
ordinary column behavior to dplyr and vctrs. The witness concerned values and
acceptance, rather than restoring an arbitrary outer class or attribute.

Three falsifiable explanations separated the observation: input-class
reconstruction would vary between eager classes; selection rewriting would
require the `pick()` spelling; converting the evaluated frame through its
`as.list()` method would affect both spellings and cease when the summary was
named and remained packed. The latter prediction matched the matrix.
`local_assigned_summary_value()` checked the original frame and then
`new_local_summary_value()` used `as.list(value)` to populate its carrier.
That added a dispatch over the actual row value before dplyr expanded it.

Ordinary dplyr also called the custom method on zero-row prototypes; this
finding did not assert that dplyr never called it. The probe recorded the row
counts of every invocation. dplyr's calls had zero rows, whereas the Margin
path added one positive-row call for each evaluated group. A method refusing
only positive-row conversion gave a successful ordinary summary and a
Margin-only refusal. Naming the same summary kept its packed values and did
not dispatch over positive-row frames.

[frame-dispatch.R](frame-dispatch.R) covered data.frame, tibble and eager
data.table inputs; an empty grouping set, fixed key, included Grouping set and
rollup; and direct-column and `pick()` summary arguments. Its observation mode
exited zero after 24 wrong-value cases, 24 false refusals and 24 passing named
packing controls. Values were six versus 99 for an unpartitioned report and
`3, 3, 6` versus `99, 99, 99` for the rollup. The
[observation transcript](frame-dispatch.log) records all cases. `--assert`
exited one at `identical(actual$out, expected$out)`, as recorded in the
[contract transcript](frame-dispatch-contract.log). Both modes completed via
the retained shell executions; the observation's terminal session was 81522.

GitHub duplicate checks included all-state Issue and PR searches for
`"as.list" "summary"` and `"unnamed" "subclass"`, plus local body/title
searches over the complete exports of 682 Issues and 674 PRs obtained on the
investigation date. The latter searched literal `as.list` and the conjunction
of `unnamed` with `subclass|as.list`. The conjunction matched neither export.
The literal matches were [#101](https://github.com/sayuks/marginplyr/issues/101),
[#165](https://github.com/sayuks/marginplyr/issues/165),
[#301](https://github.com/sayuks/marginplyr/issues/301) and
[PR #170](https://github.com/sayuks/marginplyr/pull/170), concerning expression
walks or query accounting. Related frame reports
[#435](https://github.com/sayuks/marginplyr/issues/435),
[#619](https://github.com/sayuks/marginplyr/issues/619),
[#1020](https://github.com/sayuks/marginplyr/issues/1020) and
[#1041](https://github.com/sayuks/marginplyr/issues/1041) concerned packing,
sequential visibility, incompatible types and unspecified values. No matching
row-value dispatch report was found. The custom frame was an adversarial
fixture; the lane did not establish this behavior for a particular third-party
data-frame subclass.

## Additional completed comparisons

- [across-count-controls.R](across-count-controls.R) compared 60 ordinary
  across requests against dplyr over function factories, populated dots,
  dynamic unpacking, naming and selection callbacks, and named packing.
  Results and callback counts agreed in all comparisons.
- [dynamic-across-controls.R](dynamic-across-controls.R) compared another 50
  requests with unpacking changing between calls: FALSE, TRUE and a string
  template. It crossed top-level, parenthesized and identity-wrapped calls,
  dynamic selections, names, function factories and named packing. Results
  and callback counts agreed.
- [summary-type-controls.R](summary-type-controls.R) compared 169 ordered
  pairs of per-group values, including frames, matrices, lists, unspecified
  missing values and NULL. Accepted results agreed and no comparison had
  only one successful side. Paired errors were excluded from diagnostic
  equality, so this did not establish equivalent error messages.

A separate probe observed an internal carrier name in the parent diagnostic
of a rejected multirow unnamed `pick()` result. The outer argument label was
restored. ADR 0022 explicitly left parent diagnostics unchanged; this was
recorded as a boundary observation rather than an additional confirmed bug.
Quoted-language captures agreed in a small exploratory sweep, while stored
formula and function bodies were rewritten under ADR 0019's explicit traversal
rule and were not treated as unexpected value differences.

## Replay and scope

Run the scripts from an archive of the snapshot above, passing their absolute
paths to `Rscript`. `frame-dispatch.R --assert` intentionally fails on that
snapshot; its default mode asserts the dated failures and controls. The three
additional comparison scripts print only mismatches, so an exit of zero with
no `DIFF` lines was their observed successful outcome. The latter compare a
finite matrix, do not constrain arbitrary caller code, and make no claims
about remote backends, hosted services, other R versions or other platforms.
