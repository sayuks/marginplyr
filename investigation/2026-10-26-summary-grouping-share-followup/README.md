# Summary, Grouping specification, and contextual-share follow-up hunt

Investigated: 2026-10-26
Baseline: `bfcdaf76a36d102132cd039c68ba8bc05a4fd465` (`main` and `origin/main`)

The investigation examined `summarize_with_margins()`, Grouping specification
construction and inspection, and Total shares with Parent-share controls.
It found three independent P2 defects. No package implementation, tests,
generated documentation, dependencies, or build configuration were changed.

The work used a new managed worktree, on branch
`investigation/margin-bug-hunt-2026-10-26`. The earlier
[summary/grouping/share hunt](../2026-10-26-summary-grouping-share-bug-hunt/README.md)
was a separate investigation; the baseline above already included its fixes.
The three remaining open issues at the start were release work (#554) and
upstream reporting (#256 and #468), rather than duplicates of these findings.

## Replay

Run from the repository root:

```sh
Rscript investigation/2026-10-26-summary-grouping-share-followup/reproduce.R --observe
Rscript investigation/2026-10-26-summary-grouping-share-followup/reproduce.R
```

The first command recorded the outcomes in
[reproduce-output.txt](reproduce-output.txt). The second ran the same twelve
public-contract assertions and exited 1 on the investigated baseline. Each
assertion would pass once its individual defect was corrected; ordinary-dplyr,
renaming, and no-share controls had to pass before the associated probe ran.
The script exercised the working tree through `pkgload::load_all()` and did
not substitute internal functions or alter package bindings.

Measured environment: R 4.6.1 on macOS; dplyr 1.2.1, dbplyr 2.6.0,
dtplyr 1.3.3, data.table 1.18.6.1, Arrow 25.0.1, DuckDB 1.5.5,
RSQLite 3.53.3, DBI 1.3.0, and testthat 3.3.2.

## F1: parentheses hid an injected nested specification

Ticket: [#897](https://github.com/sayuks/marginplyr/issues/897)

A wrapper that embraced an argument with `grouping_sets({{ x }})` accepted a
`grouping_set(a)`. Adding redundant parentheses,
`grouping_sets(({{ x }}))`, made inspection refuse the same specification.
The lower-level equivalent used a quosure containing `grouping_set(a)`:
`grouping_sets(!!q)` succeeded, while `grouping_sets(((!!q)))` failed.

The diagnostic said that `grouping_set(a)` was a Grouping specification but
was not written as a recognized nested specification. This contradicted
[grouping_set()'s *How an argument is read*](../../R/grouping-spec.R), which
made redundant parentheses transparent however many pairs deep, together with
[ADR 0007](../../design/adr/0007-capture-user-expressions-at-public-verbs.md)'s
quosure-capture boundary.

The minimized control held the input, injected expression, and environment
constant. A scope matrix crossed four admitted parent kinds, constructor/name
payloads, and all five public consumers: 40 plain cases succeeded and all
40 parenthesized cases falsely refused. An equivalently wrapped column-selection
quosure succeeded, narrowing the failure to nested-specification recognition.

Three hypotheses were considered: parentheses peeling stopped at an inner
quosure; the environment was lost; or tidyselect refused the selection.
Inspection of the captured argument found that `nested_arg_expr()` removed
parentheses but left an inner quosure. `read_grouping_arg()` then failed to
recognize a constructor or bound name and took the selection path. The same
environment worked without parentheses, and tidyselect was reached only after
misclassification. This supported the first hypothesis.

Acceptance should preserve nested-specification recognition, ambiguity and
cycle checks, admitted-kind rules, captured environments, and evaluation
counts through the transparent parentheses. This ticket had no blocker.

## F2: valid special names failed during generated column reads

Ticket: [#898](https://github.com/sayuks/marginplyr/issues/898)

A data frame with a grouping column named `..0` was valid for both ordinary
`dplyr::summarize(.by = all_of("..0"))` and
`group_by(across(all_of("..0")))` followed by `summarize()`. Inspection also
accepted that Grouping specification. The Margin summary failed while copying
the key to `..marginplyr_key_1`, with
`indexing '...' with non-positive index 0`.

The failure affected both a fixed `.by` key and a Grouping dimension. A
character/integer/factor/Date by empty/nonempty by fixed/variable matrix
reproduced it in all 16 cases. Public `rename()` before the Margin operation
and renaming back afterwards matched the ordinary-dplyr result in every case.
This separated the defect from an invalid input name or an unsupported key
type. The public selection contract in
[summarize_with_margins()](../../R/summarize_with_margins.R) did not exclude
this valid column name.

The related share-source path accepted ordinary numeric scalar summaries
named `..0` and `..00`, returning `1, 3, 4`. Adding either contextual share then failed
when its denominator projection read the source through `.data[[source]]`.
The share source contract in [share_of_total()](../../R/share.R) required a
unique preceding named scalar, which these sources satisfied.

The hypotheses were invalid caller names, a key-type problem, or generated
column reads invoking R's special dot-name lookup. Ordinary-dplyr and rename
controls rejected the first two. `summarize_margin_union()`'s private key copy
and the share denominator projection both reached `margin_column_pronoun()`,
whose generated `.data[[name]]` read triggered the special lookup. The ticket
combined these two public manifestations of that mechanism.

Names `..1` and `...` were excluded from the requested cross-backend guarantee:
vctrs/dplyr explicitly reserved them in ordinary lazy paths. In contrast,
`..0` as `.id`, with and without shares and under all three `.sort` values,
succeeded in six controls. This finding concerned reads of caller columns,
not every occurrence of the string. This ticket had no blocker.

## F3: named scalar selectors were misclassified as expanded frame sources

Ticket: [#899](https://github.com/sayuks/marginplyr/issues/899)

For rows `g = a, b` and `value = 1, 3`, the explicitly named ordinary summary
`source = sum(pick(value))` returned the scalar values `1, 3, 4` over
`rollup(g)`. Adding `share = share_of_total(source)` falsely refused it as
having been expanded from a data-frame-valued summary, and suggested rewriting
it as a top-level named summary. It already was one.

`source = sum(across(value, identity))` had the same outcome. Both Total and
Parent shares were affected. A twelve-case matrix crossed local tibble, eager
data.table, and dtplyr inputs with both selector forms and both helpers. All
no-share controls returned `1, 3, 4`; the eight eager cases falsely refused,
while all four dtplyr cases returned the expected shares `0.25, 0.75, 1`.
The direct `source = sum(value)` control succeeded.

The [*Eligible source summaries* contract](../../R/share.R) and
[ADR 0010](../../design/adr/0010-compute-parent-shares-as-a-contextual-summary.md)
admitted a top-level named, self-contained plain numeric scalar. They excluded
columns expanded from an unnamed data-frame-valued summary, which did not
describe either expression above.

The hypotheses were an ineligible source value, an unsupported selector, or
incorrect static provenance. No-share execution proved the scalar values;
direct aggregation and dtplyr controls proved the share calculation. In
`analyze_ordinary_summaries()`, the local deferred-selection branch assigned
`eligibility = "expanded"` to selector-containing expressions other than a
top-level `across()`, including explicitly named scalar expressions. This
supported the provenance hypothesis. Independent investigators reproduced it.

Acceptance should admit these named scalar sources while retaining scalar,
type, unique-name, and preceding-alias checks. It should continue to refuse
actual frame-valued sources and unsupported expanded-frame provenance, and
preserve sequential local selection semantics. This ticket had no blocker.

## Exploration coverage

Counts below describe separate harnesses, rather than distinct global inputs.
Randomized comparisons used fixed seeds. Temporary exploratory harnesses were
not added to the package test suite; the minimized contract replay was retained.

| Area | Measured exploration | Result |
| --- | --- | --- |
| Grouping plans | 1,500 nested random expansion/mask cases | Independent expansion oracle agreed on sets, occurrence order, and masks |
| Column selection | 1,028 tidyselect differential cases | Included columns agreed |
| Nested grammar | 175 parent/child/spelling, 75 ambiguous-name, 10 empty-argument, 4 environment/cycle cases | Expected acceptance/refusal held |
| dtplyr metadata | 11 symbol-copy/arithmetic cases | Collected metadata agreed |
| Injected nested specifications | 40 plain and 40 parenthesized cases | F1 isolated |
| Local summaries | 1,029 probes across 55 summary expressions, 18 evaluation-count cases, 114 name cases, 507 key-type/empty cube comparisons, 228 result-size/frame cases, 91 injection/parenthesis cases, and 16 special-key controls | F2 isolated; other compared behavior agreed |
| Shares | 400 denominator-oracle checks across 250 seeded local scenarios, 140 cross-backend cases, 80 scalar/type/cardinality cases | No additional denominator or eligibility-value defect |
| Share names | 384 source/output/direct/across/naming-factory cases | 342 passed; 42 reserved-name failures were checked against upstream controls; valid `..0`/`..00` reads supplemented F2 |
| Named selector sources | 12 local/data.table/dtplyr cases | Eight eager refusals formed F3; four lazy controls passed |
| Lazy result comparison | 270 local versus dtplyr/SQLite/DuckDB/Arrow executions | Two differences came from empty SQL aggregates |
| Lazy names | 112 special-name executions across four backends | No additional Margin-specific failure |
| Derived lazy inputs | 132 combinations of input transformation and Margin order | Nine documented dtplyr metadata refusals, three delegated empty-output type differences, and three upstream Arrow failures |
| Lazy key types | 160 prototype/label/order combinations | 28 differences were backend text conversions, transfer behavior, or the documented Arrow factor/dictionary exception |

The backend exploration covered rollup, cube, explicit unions, Cartesian
products, repeated grouping sets, missing fixed keys, typed-missing and string
labels, and all three Margin order choices. The general differential seed was
82314. Grouping plan fuzzing used seed 715; the share denominator oracle
used seed 72619. These were bounded probes, not proof of exhaustiveness.

## Candidates excluded

- Empty SQL `SUM`/`MIN`/`MAX` and ordinary empty-column types differed from R.
  Plain backend aggregation and the delegated ordinary-summary contract
  explained these results. Package-declared identifiers and helper types were
  checked separately.
- An Arrow query ordered by a measure and then summarized after dropping that
  measure failed with `Invalid sort key column`. Plain Arrow/dplyr aggregation
  over the same input failed too. This was recorded as an upstream limitation,
  rather than a new Margin-specific ticket.
- Arrow dictionary keys used value ordering where local factors used level
  ordering. ADR 0018's Arrow dictionary exception explicitly authorized that
  key cast. Non-missing Margin labels also followed the documented loss of
  factor reconstruction on Arrow.
- An earlier summary named `amount` made
  `source = with(list(amount = 10), amount)` look dependent on that alias.
  The conservative unknown-scope policy in the architecture's *Static
  expression reading* and the dependency-walk headers explained this
  over-reporting; no arbitrary NSE-scope guarantee was added by this hunt.
- A summary output named `.by`, invalid lazy names `..1`/`...`, trailing
  constructor commas, and late versus injected specification capture followed
  their documented or upstream rules.

## Validation boundary

The focused existing tests were run with:

```sh
Rscript -e 'testthat::test_local(filter = "^(grouping-plan|grouping-context|inspect-grouping|summarize-operation|share)$", reporter = "summary")'
```

The matching grouping-plan, inspect-grouping, share, and summarize-operation
files completed without failures. An initial sandboxed attempt could not write
testthat's snapshot file; the successful invocation ran with access to the
managed worktree, and its snapshot handling left no tracked changes.

The retained files were investigation artifacts excluded by the existing
`^investigation$` build rule. No production code or package-check inputs were
changed, so this was Repository-only under
[the local-check authority](../../design/agents/local-checks.md). The full
Review-ready check was therefore not required. Both repository linters and
the context-budget verifier were run before publication; their results were
recorded in the pull request. CI monitoring was outside this request.
