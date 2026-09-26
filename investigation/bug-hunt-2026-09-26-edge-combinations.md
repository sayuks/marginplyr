# Bug hunt across evaluation, names, and backend boundaries

Investigated: 2026-09-26
Baseline: `049428fc4029ed59707d955b31af87b55e89fa9d`

Six independent repair units were established after #691. Eight executable
probes failed their intended-result assertions. The full existing testthat
suite and all twelve probes from the two preceding September 26 investigations
passed. No package implementation or package-test file was changed.

## Reproduction

Run from the repository root:

```sh
Rscript investigation/bug-hunt-2026-09-26-edge-combinations.R
```

An optional argument selects `unpack`, `selection`, `predicate`, `dimension`,
`share`, `case`, `internal_case`, or `dot_data`. The combined invocation took
about two seconds, reported eight failures, and exited 1. Each probe asserts
the intended outcome after passing controls, rather than passing because a bug
exists. SQL uses in-memory connections closed on exit. No package namespace is
modified. The `case` probe accepts either preserved values or a Package
condition naming both conflicting spellings; the proposed ticket chooses the
explicit-refusal policy below. The internal-name probe requires correct values.

The measured environment was R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0, dtplyr 1.3.3,
data.table 1.18.6.1, RSQLite 3.53.3, DuckDB 1.5.5, Arrow 25.0.1,
tibble 3.3.1, rlang 1.3.0, and testthat 3.3.2. These describe the installation,
not minimum supported versions.

## 1. P2: dynamic FALSE unpacking reevaluates an ordinary function factory

The `unpack` probe summarizes one row `x = 1` using an unnamed `across()`.
Its `.fns` factory increments a counter and returns a function adding that
counter's value. Ordinary dplyr and a Margin call with literal `.unpack = FALSE`
each invoked the factory once and returned `x = 2`. Changing only `.unpack`
to `identity(FALSE)` invoked it twice and returned `x = 3`.

No grouping key, contextual share, or frame-valued function was needed.
`wrap_local_frame_summaries()` in
[summary-selections.R](../R/summary-selections.R) wrapped the dynamic call in
`local_assigned_summary_value()`. Moving the call out of dplyr's top-level
`across()` expansion changed `.fns` evaluation. Call-stack observation placed
both invocations inside execution of that wrapper, rather than share planning.

Separate R processes loaded clean git archives. Parent `ac088ec` passed the
same probe; the investigated HEAD after `c8b6b1c` failed it. The latter commit
changed this dynamic-unpack wrapping decision. This was the one newly
established recent regression, rather than an inferred introduction date.

A 72-case comparison crossed six FALSE spellings, a factory returning a
function or function list, one or two groups, and tibble/data.frame/eager
data.table. All 36 static controls matched dplyr; all 36 expression, delayed
binding, and active binding cases differed. With two groups the factory ran
four times where dplyr ran it once. This finding does not claim that every
TRUE/string-unpack expression has the same evaluation timing as FALSE.

[ADR 0019](../design/adr/0019-resolve-contextual-helper-names-by-static-spelling.md)
distinguishes ordinary functions from Contextual helpers, and
[ADR 0028](../design/adr/0028-expand-a-data-frame-summary-marginplyr-named.md)
owns actual-value frame checking. A repair must preserve the internal-key and
share-source collision checks from #683/#685, including dynamic unpacking.

## 2. P2: an unrelated share executes a packed across selector during planning

The `selection` probe uses `total = sum(x)` followed by a named
`packed = across(...)`. Its selector returns `x` on the first invocation and
`y` thereafter. With input `x = 1, y = 10`, replacing a constant `p = 1` with
`p = share_of_total(total)` changed the selector count from one to two and the
packed output from `x = 1` to `y = 10`.

The `predicate` probe establishes a failure without side effects. With
`x = 1, y = -10`, `where(~ all(.x > 0))` worked under ordinary dplyr and the
Margin control without a share. Adding the unrelated share raised
`Predicate must return TRUE or FALSE, not NA`, despite nonmissing actual data.
The intended packed columns were `x` and the preceding `total`.

`analyze_ordinary_summaries()` in [share.R](../R/share.R) treated the explicit
packed name as an output and called `known_across_source_names()` for its
dependencies. That executed the selector against a one-row missing proxy.
The original selector remained for actual execution. Stack observation
distinguished these two invocations.

All 24 comparisons crossing Parent/Total shares, placement before/after the
share, three local frame classes, and callback/active-binding selectors added
one invocation over the control. The same probe failed in clean archives of
`89eba19`, `eb424ef`, and `ac088ec`; an earlier introduction was not established.
It needs no repair of finding 1. This is a different expression and path from
the `.names` callback defect closed as #684.

[ADR 0010](../design/adr/0010-compute-parent-shares-as-a-contextual-summary.md)
scopes source-dependency restrictions to actual sources; a caller-named
`across()` remains packed and is ineligible as a share source. Preserving its
ordinary evaluation does not relax that refusal or source uniqueness.

## 3. P2: dtplyr special names fail as grouping dimensions

The `dimension` probe expands a lazy table with `.N = c("b", "a")` and
`value = 1:2` under `rollup(.N)`. Collection failed with
`cannot change value of locked binding for '.N'`. The equivalent ordinary
dtplyr union of the source and its scalar-labelled copy returned the correct
four rows. A typed-missing Margin label also let the expansion succeed.
The unrelated `.N` payload regression from #689 passed, separating this case
from the already repaired count expression.

The affected spellings were `.N`, `.I`, `.SD`, `.GRP`, and `.NGRP`.
`label_margin_branch()` read the special-named dimension while casting it and
constructing a full-length label. Summaries had a second failing route:
`summarize_margin_union()` copied the grouping key under an internal name,
introducing the same special-variable read even with a typed-missing label.
These are package-generated expressions; ordinary dtplyr grouping accepted
the input. A follow-up check reproduced the summary failure for all five
spellings used as fixed `.by` keys, through the same internal-key copy.

The scope matrix crossed five names, zero/one/two rows, three orders,
one-set/rollup plans, text/missing labels, and summary/expansion: 270 of 360
cases failed. All 90 missing-labelled expansions succeeded. Every check
preserved the source. `.BY` was a passing control. Nesting restrictions that
also occurred in ordinary tidyr/dtplyr were excluded.

This is a supported-equivalent integration defect at the Margin boundary.
The required behavior is the same public names, keys, values, and laziness
through a safe internal representation, including the corrected #689 case.

## 4. P2: dtplyr special names fail as otherwise eligible share sources

The `share` probe needs just one input value, 2, and no grouping specification.
Both ordinary dtplyr and a Margin summary accepted `.I = sum(value)`.
Appending `p = share_of_total(.I)` failed at collection with the same locked
binding error. Renaming only the source to `total` succeeded; the local
Margin equivalent returned `.I = 2, p = 1`.

The affected source names were `.I`, `.GRP`, and `.NGRP`. They all worked as
share-output names in the controls. Generated denominator copying and ratio
expressions in [share.R](../R/share.R) read the source as a data.table special
variable. This path required neither text labels nor the grouping-key copy
from finding 3, so it is an independently verifiable repair.

All 216 cases crossing those three names, zero/one/two rows, three orders,
two label forms, Parent/Total helpers, and fixed-key presence failed.
The source remained unchanged. The repair must preserve the numeric scalar
eligibility and zero/missing-denominator rules in
[ADR 0010](../design/adr/0010-compute-parent-shares-as-a-contextual-summary.md)
and [ADR 0017](../design/adr/0017-calculate-total-shares-against-the-grand-total-set.md).

## 5. P1: SQL identifier equivalence corrupts public columns and internal keys

The `case` probe uses SQLite input `g = "a", v = 1L` and requests summary
`G = sum(v)` under `rollup(g)` with a missing label. Local Margin and ordinary
dbplyr grouped-summary controls retained the value 1. The Margin SQL result
returned `G = c(1L, NA)`. Text labels and ordering could replace numeric values
with the grouping values instead. No refusal preceded the incorrect result.

The databases matched quoted ASCII identifiers ignoring case. Package-added
projections read the first of two aliases after the inner query had both
spellings. A 60-case matrix across SQLite/DuckDB, five collision roles, three
orders, and two labels produced 54 failures. Roles were grouping/summary,
two summaries, grouping/identifier, grouping/share output, and carried
expansion columns. The few passing configurations did not make the names safe
under the other ordinary Margin options.

The `internal_case` probe demonstrates a separate consequence of the same
equivalence rule without ambiguous public names. An unrelated input column
`..MARGINPLYR_KEY_1 = c("WRONG", "WRONG")` collided with the allocated
`..marginplyr_key_1`. Two groups `a` and `b`, each with count 1, became one
`WRONG` group with count 2. A one-set SQLite summary was sufficient.
DuckDB's native path passed, while a repeated-set plan with `.id` forced its
portable path and produced the same incorrect aggregation.

`new_margin_internal_names()` used exact R string equality, and the portable
grouping-key allocation inherited it. SQLite's separate typed-order allocator
already normalized its own names; that did not protect these grouping keys.

One proposed repair unit owns this invariant: reject public names equal under
the verified backend's identifier rule with an actionable Package condition;
allocate internal aliases that avoid the same equality, so valid input is
preserved. This distinguishes a caller conflict from a package-created one.
The verified rule is ASCII case folding: the databases distinguished `Ä` from
`ä`. General Unicode lowercasing would reject distinct names incorrectly.
Local case-sensitive names remain supported. This is consistent with
[ADR 0015](../design/adr/0015-separate-package-conditions-from-internal-invariants.md)
and the public-column boundary in
[ADR 0031](../design/adr/0031-preserve-sqlite-typed-dimensions-under-margin-order.md).
No extra source-row query is needed to compare names (ADR 0020).

## 6. P2: a .data summary name becomes a function argument

The `dot_data` probe dynamically names an ordinary count summary `.data`.
Ordinary dplyr accepted it. A local Margin summary under `rollup(g)` instead
raised `formal argument ".data" matched by multiple actual arguments`.
SQLite's portable summary path failed too. A local `.data` share output and
DuckDB native share output were passing controls, demonstrating that the name
was not uniformly forbidden by the public interface.

`summarize_margin_branch()` in
[grouping-adapter-union.R](../R/grouping-adapter-union.R) used `rlang::inject()`
to put captured named summaries into a call already carrying `.data = .data`.
That promoted the output name to a formal argument before dplyr captured dots.
Ordinary dplyr splicing of the same quosures succeeded; explicitly injecting
them before argument matching reproduced the failure.

The repair should preserve dynamic `.data` naming through this call boundary.
It must retain the deliberate package refusals for `.by` and `.groups`, which
are a separate public admission policy. Controls for `.env`, `.drop`,
`summarize_dots`, and `group_vars` succeeded. No broader reservation of dplyr's
formal names is needed. The public `...` parameter in
[summarize_with_margins()](../R/summarize_with_margins.R) promises dplyr's
name-value summary pairs; ADR 0016 additionally governs result class and
attributes. A bare formal-matching error is not an intentional Package condition.

## Breadth, independent checks, and limits

| Experiment | Observed scope and outcome |
| --- | --- |
| Full testthat suite | `Rscript -e 'Sys.setenv(NOT_CRAN="true"); testthat::test_local(reporter="summary")'` exited 0. |
| Prior intended-result probes | Both preceding September 26 scripts passed all twelve probes. |
| Local nesting | 768 combinations: 16 key types, empty/populated, fixed/rollup, keep/drop inner keys, three orders, two nesting verbs. Original payloads and retained keys matched in every case. |
| Grouping-plan properties | Seed 26092603; 200 recursive specifications with keep/drop duplicates, giving 400 plan-algebra and independent dplyr-summary comparisons. All passed, including overlapping products and unusual names. |
| Local summary shapes | 184 combinations; eight grouping-helper calls were intentionally invalid outside a Margin verb, and the other 176 matched their controls. |
| Local callback evaluation | 192 exploratory cases led to findings 1 and 2; the narrowed 72 and 24 case matrices above established their scope. |
| Local/dtplyr/Arrow types | 1,056 comparisons, 528 per lazy backend. dtplyr matched throughout. Arrow's 84 differences were its own text rendering, difftime units, or refusal of factor missing-value levels. |
| Special backend names | 1,380 initial comparisons. Ordinary-backend controls and valid `all_of()` selection separated special-variable failures from ordinary unsupported inputs. In the corrected 460-case Arrow matrix, 410 admitted-result comparisons matched and 50 cases were rejected by both paths; identical diagnostics were not asserted. |
| dtplyr shares and mutability | 200 share comparisons matched. Twenty-eight mutable-graph Margin refusals and fourteen safe inspections passed without changing caller data. |
| SQLite/DuckDB ordinary matrix | 2,448 comparisons; all 1,836 populated cases matched. Empty inputs had 108 backend-delegated type differences in ordinary count/Grouping identifier outputs. Keys, package identifiers, and shares agreed. |
| SQL unusual names | 158 cases exposed the `.data` share-output route; other admitted names passed. |
| DuckDB input types | 288 comparisons; 72 differences already arose during copying input or ordinary backend string conversion. The other 216 matched. |
| SQLite destinations | Sixteen qualified schema/temp combinations matched ordinary dbplyr admission, and admitted materializations matched collection. |
| Context budget | 20,008 bytes within the 22,005-byte baseline. |

Differential matrices treating refusal on both sides as a matching outcome
did not establish equal condition classes or diagnostics. The SQL value
comparison normalized empty SUM semantics and did not assert
all attributes or backend-delegated types. The broader type comparisons were
not claims of identical character rendering across engines. Live PostgreSQL,
other operating systems, and alternate dependency versions were not exercised.
Arrow's absorption boundary was observed through outcomes and existing tests,
not an instrumented proof about every possible backend execution entry point.

Each selected finding was independently rerun by the coordinating investigator;
the SQL corruption probes also received a second investigator's independent
check. Existing open Issues were inspected, and relevant closed reports were
read to distinguish #683–#689 and older exact-name/argument-matching defects
from these findings.

The proposed six tickets have no blocking edges: shared source files alone
do not create a semantic dependency. Finding 5 combines public and private
SQL collisions because both require one backend identifier-equivalence rule.
Findings 3 and 4 stay separate because labelling/group-key preparation and
share-source arithmetic each have their own minimal failing operation.

These artifacts are excluded from the source package by `.Rbuildignore`.
No package-affecting change, strict coverage run, source-tarball check, or site
render was part of this investigation. Repairs and package regression tests
belong to the resulting implementation tickets.
