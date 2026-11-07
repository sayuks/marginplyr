# Grouping inspection capture and compatibility investigation

Investigated: 2026-11-08
Tested main: `ad7284118124f11a39ac29b10f1d101399d79524`

A new worktree and branch started from freshly fetched main. Three fresh-context
agents independently generated candidates from implementation, public contracts,
and consistency between processing paths. They used isolated archives of the
same SHA and did not exchange candidates before their initial reports. The
parent separately generated small specifications and independent ordered-set
expectations. One previously unreported defect was confirmed and filed as
[#1489][issue]. No package implementation or package test was changed.

## Contracts and investigation boundary

The [constructor][constructors], [inspection][inspection] and [identity-helper][helpers]
references supplied the public contracts. The investigation traced argument
capture and evaluation, nested-name admission, selection, backend metadata,
family expansion, normalization, duplicate handling, masks, formatting and the
corresponding Margin-operation identifiers. It was not limited to recent edits.
Relevant decisions read across the agents included ADRs 0002, 0005, 0007, 0008,
0009, 0013, 0018, 0019, 0020, 0026, 0027, 0029, 0032, 0033 and 0036.

The two recent [inspection][prior] [investigations][systematic], adjacent Nest,
expansion and summary investigations, and the relevant fix history were examined.
Their historical comparison counts were not counted as executions here. The
starting main included the #1461–#1465, #1469 and #1481 repairs. Full related
Issue/PR bodies and comments distinguished the new failure from #261, #925,
#1144, #1320 and PR #1412's protected-capture trailing-comma repair.

## Confirmed defect: an authored empty argument disappeared

The constructor contract refused an argument left empty, naming its constructor
and position; only an authored trailing comma was omitted. This minimal public
call instead succeeded:

```r
library(marginplyr)
d <- data.frame(a = c("x", "y"))
inspect_grouping(d, .grouping = grouping_set(, !!!list()))
# One row: included = "()", grouping_id = 0L.

s <- grouping_set(, !!!list())
inspect_grouping(d, .grouping = s)
# Argument 1 of `grouping_set()` is empty.
```

The raw constructor had an empty position followed by a splice argument. The
public consumer's outer capture expanded the empty splice first, leaving that
missing position trailing; constructor capture then omitted it. Comparing raw
and injection-processing captures isolated that boundary. The same dependency
versions correctly refused the stored-specification control, so this was a
package capture-composition defect rather than a claim that rlang's injection
itself malfunctioned.

The effect reached execution. A summary with that literal specification returned
`n = 2L`; expansion and both Nest verbs also accepted it. Nesting the invalid
`grouping_set()` before `a` in `grouping_sets()` silently added a Grand total set.
All five constructors accepted `ctor(a, , !!!list())` through inspection and all
four Margin verbs, while storing the same specification first refused Argument
2. Qualified, parenthesized, nested and whole-specification embrace forms
reproduced. Three isolated source copies independently confirmed the failure.

Empty carriers including `NULL`, pairlists and quosure lists reached the same
cause. Nonempty suffix splices, including a list containing `NULL`, correctly
retained and refused the empty position. Genuine trailing commas, empty
positions inside an ordinary `c()` selection, lexical dots forwarding, opaque
injected quosures and named missing positions supplied separating controls.
These variants were kept in one ticket because they crossed the same boundary.

The existing test named "captured selections retain empty positions around
splices" included this missing-position shape, but its consumer used
`eval(expr)`. The constructor therefore saw the raw expression. That test could
pass while the literal public call above accepted invalid input. PR #1412's
review and repair addressed protected constructor capture, not this earlier
outer-capture transformation.

All-state Issue/PR searches included `"empty splice"`, `"empty" "constructor"`
and `"missing argument" "grouping"`. The full 765-Issue inventory and 300 recent
PRs were also searched locally; server-side searches covered older PRs. The
closest reports above concerned diagnostics, forwarding or a different capture
boundary. Main was fetched again before publication and still named the tested
SHA. [#1489][issue] was read back as open with `bug` and `ready-for-agent` labels,
no blockers and no required prefactoring. Its contract, minimal reproduction,
expected/actual result, impact, environment and acceptance criteria were recorded.

## Executed coverage and hypothesis dispositions

Counts below measure different things and are not added into a coverage score.
Every row describes terminally completed executions on the tested SHA.

| Hypothesis / evidence | Execution result and disposition | Next evidence |
| --- | --- | --- |
| Overlapping/composite dimensions or nested products could change ordered sets or duplicate identities. | Parent: 553 generated models, 2,946 exact full plans and 372 expected duplicate refusals across all policies and both formats. A: 120 keep/drop plan comparisons. B: 360 independent keep/drop comparisons. No discrepancy. | A new algebraic boundary, rather than repeating these finite models. |
| Inspection masks or occurrence identifiers could disagree with execution. | Parent: 67 local and 10 live DuckDB/SQLite summary comparisons matched `.id` and bare `grouping_id()`. These shared-path comparisons were separate from the independent arithmetic oracle. | Other backend/version evidence. |
| Lazy construction could change a name-only plan or process rows. | Parent: 1,272 comparisons over 53 models, three policies, two formats and dtplyr, Arrow, DuckDB and SQLite matched the independently checked local plans/refusals. A private DuckDB sequence stayed uncalled through inspection; explicit calibration returned 1. | Other SQL preparation shapes or remote engines. |
| Separator/control names or wide fixed keys could corrupt formatting, types or width limits. | B: 56 text/list and local/dtplyr execution comparisons over 14 unusual names; 16 inspection/summary pairs with 40 reversed fixed keys, 32 variable dimensions and four sets across local/dtplyr/SQLite/DuckDB. Exact vectors, integer masks/NA, fixed order and helper subsets matched. DuckDB native and portable paths were distinguished. | Other name encodings and dependency versions. |
| Name/specification overlap, captured environments or options could bypass admission or change evaluation. | Bare/parenthesized collisions, promises, embrace, captured quosures, aliases, independent reuse and explicit disambiguation matched their contracts. B's 20 invalid options refused before a deferred specification ran; counted/classed valid options also matched. The empty-splice case above was confirmed separately. | A distinct capture boundary. |
| Derived typed metadata could differ from actual types or mutate caller data. | C: 1,284 exact dtplyr metadata comparisons over 14 constructible type families and derivations; 14 matching overlap refusals. Three matrix/array/packed fixtures were rejected by data.table construction before package entry. No mismatch. | Other constructible type/derivation families. |
| Derived SQL input, repeated calls or failure/reentry could alter state or ownership. | C: 125 SQLite/DuckDB comparisons and 38 ownership/immutability assertions matched. Covered renamed/reordered/derived schemas, value and row-order controls, nested success/failure, audit-setting isolation, Mutable second roots and unchanged caller tables. | Other callback/control-transfer or backend conditions. |

The parent oracle enumerated set families with simple R operations, retaining
ordered occurrences, and independently calculated fixed/included/omitted vectors,
bits and weighted integer masks. Input generation was separate. No internal Plan
generator supplied its expectations. Text was checked as display; list output
was checked for exact names, values and column types. Physical Margin-result
order was not inferred from inspection order.

Initial harness errors were excluded: the parent corrected NULL deletion of an
empty set and cube subset ranking; name controls corrected tibble's `...`/`..1`
input refusal, tidyselect pronouns and `.id` placement; C corrected an expectation
that an inner record survived after its outer call resumed and allowed numeric
remote helper types. The final corrected executions supplied the counts above.
No provisional mismatch was reported as another package defect.

An unqualified caller function named like a constructor could observe no active
selection context during its preflight invocation, while an ordinary selection
callback observed typed context. This difference was reproduced, but ADR 0008's
retained-selection amendment explicitly kept the unqualified preflight reading;
its later authored-namespace amendment had narrower scope. No violation of the
accepted contract was established, so no second Issue was filed. Broadening that
context guarantee would require an explicit contract decision and a one-call
callback/context regression. Eager injection inside a named constructor argument
also differed between literal and stored forms; injection's outer evaluation
boundary prevented treating that observation alone as another confirmed defect.

## Reproduction evidence, environment and limits

Programs, compact hypothesis/evidence/result/disposition/next records, logs and
CSV/RDS observations were kept outside the submitting worktree under
`/tmp/marginplyr-inspect-{parent,A,B,C}`. They were temporary evidence, not
repository replay assets. In the parent directory, `oracle.R` and `lazy-oracle.R`
exited 0. `replay-current.R` exited 0 in observation mode and exited 1 with
`--contract` because the literal call failed the required Package-condition
assertion; the stored control passed first. That assertion could pass after a
repair. A's `probe-A5-empty-boundary.R` SHA-256 was
`7699303379d5f1a77126c060f50c1f7ec7af0c488420875353f8614bb339d025`.
The Issue retained a self-contained minimal public reproduction.

All original execution handles were collected to terminal completion, including
the parent's oracle 53569 and backend comparison 23784, A's final reproduction
42979, B's independent literal replay 81220, and C's 34317/63560/22271. Each copy
ran at most one reproduction at a time with testthat parallel execution disabled.
No complete suite, package build or render was used as exploratory feedback.

Environment: arm64 macOS Tahoe 26.6.2; R 4.6.1; marginplyr 0.1.0 from the tested
SHA; dplyr 1.2.1, dbplyr 2.6.0, tidyselect 1.2.1, rlang 1.3.0, tibble 3.3.1,
vctrs 0.7.3, dtplyr 1.3.3, data.table 1.18.6.1, DBI 1.3.0, DuckDB 1.5.5,
RSQLite 3.53.3 and Arrow 25.0.1. Only private temporary databases and synthetic
data were used, and their connections were closed.

The final cross-lane check included display and exact list forms, local and lazy
inputs, successful and rejected calls, metadata versus observable row processing,
input immutability and recorded state. No concrete executable promising hypothesis
remained untested within these lanes. Other OS/dependency versions, live
PostgreSQL/SQL Server, hosted datasets, arbitrary dispatched methods, arbitrary
SQL preparation effects, unbounded expression graphs and general resource limits
were not established. They require their own fixtures and observable execution
controls; finite passing comparisons do not establish universal correctness.

## Submission effects

The branch adds this dated note only. Its consumers are investigation readers;
it adds no package/check consumer, runtime change, generated documentation,
dependency, package test, build rule or verification tooling. The cumulative
baseline is the merge-base with fetched main, the tested SHA above. These effects
justify Review-ready `source`; no ancestor's full result is inherited. Package
build/check, complete suite and snapshots are omitted. No NEWS entry is required
for this research-only record. Exact submitted-SHA verification, current-main
compatibility and separate Standards/Spec review records belong on the PR.

[issue]: https://github.com/sayuks/marginplyr/issues/1489
[constructors]: ../R/grouping-spec.R
[inspection]: ../R/inspect-grouping.R
[helpers]: ../R/grouping-context.R
[prior]: inspect-grouping-bughunt-2026-11-07.md
[systematic]: inspect-grouping-systematic-2026-11-07.md
