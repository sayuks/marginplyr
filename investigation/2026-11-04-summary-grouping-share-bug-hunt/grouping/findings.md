# Grouping family and forwarded-selection frontier

Investigated: 2026-11-04
Repository snapshot: `78ebbc36200bdc5cef70838dd4e772c5c4a1401c`

This lane established no new actionable Grouping specification defect in its
completed finite loops. It changed no package implementation or package
regression test and made no tracker write. The standalone scripts loaded the
investigated checkout through `pkgload::load_all()`. These observations did not
constitute a package-suite or Review-ready result.

## Contracts and prior evidence

The required readings were recorded with content digests, extents and complete
accepted ranges in `required-readings.csv`. `CONTEXT.md` supplied Grouping
specification, Grouping plan, Grouping set identifier and Grouping identifier
vocabulary. ADR 0008 supplied constructor grammar, duplicate handling,
preflight/evaluation budgets and retained selection semantics; ADRs 0007, 0013
and 0026 supplied capture, public inspection and ambiguity contracts. ADR 0005
supplied the early-failure ordering obligation.

The relevant earlier evidence was the November 3 summary/Grouping/share
[Grouping lane](../../2026-11-03-summary-grouping-share-bug-hunt/grouping/findings.md),
the November 3 expansion
[specification lane](../../2026-11-03-expand-grouping-greedy/spec/README.md), and
the November 3 nesting
[selection frontier](../../2026-11-03-nest-spec-greedy/spec/README.md).
Those dated results were not counted again. The investigated source contained
the warning-promotion preflight repair associated with closed #1184 and merged
[PR #1198](https://github.com/sayuks/marginplyr/pull/1198).

The ranked investigation hypotheses were:

1. Overlapping composite dimensions could produce a wrong expanded family,
   order, duplicate decision, Grouping bit or Grouping identifier.
2. A correct inspected family could be consumed as different grouping-set
   occurrences during ordinary scalar summarization.
3. R's noncanonical numbered-dots spellings could be classified as names-only
   or lose their forwarding frame, particularly when the promise needed typed
   selection context.

The completed observations refuted these hypotheses for the retained cases.

## Independent family and summary oracle

`algebra-oracle.R` generated 600 deterministic random trees over four dimensions
with seed 20261104. Nine trees whose expansion exceeded 100 occurrences were
excluded by the explicit research bound. The remaining 591 trees exercised all
five constructors, nested unions/products, composite rollup/cube dimensions,
overlapping dimensions, empty grouping sets and identity products. Every tree
was checked under all three duplicate policies.

The oracle represented trees separately from their R expressions. It built
constructor expressions using literal `all_of()` selections, independently
computed families, normalized dimension membership and computed masks by
membership and powers of two. Cube combinations came from a binary grid and
ordering key; product combinations came from an index grid. It did not call the
package's compiler, expansion, preflight or bit/id helpers to obtain expected
answers.

All 1,773 duplicate-policy calls were accounted for:

| Observation | Count |
| --- | ---: |
| Exact included/omitted family, occurrence order, bit and identifier match | 1,589 |
| Required duplicate-set Package refusal | 184 |

The first 80 generated trees were also eligible for summary consumption when
their family had at most 30 occurrences. Seventy-five qualified. For each
occurrence, its observed count and sum were compared with ordinary dplyr
summarization over that independently computed set. The data included numeric,
character, factor and logical dimensions with missing values. Included keys
were compared after explicit sorting; no unspecified physical result order was
claimed. All 75 summary calls agreed across every retained occurrence.

Initial oracle-only mismatches were corrected before these totals: typed
grouping keys require `.margin_label = NULL` to retain their types, an empty
dimension vector must be `character()` rather than `NULL`, and empty Grouping
bits are an unnamed `integer()`. Those fixture distinctions were not package
findings. The final script asserted every comparison and exited zero.

## Noncanonical numbered-dots differential

The ordinary-summary investigator found a separate noncanonical-dots candidate
and asked whether Grouping selection shared it. `dots-extended.R` extended the
previous day's dots harness with these seven expressions:

- `all_of(..01)`;
- ``all_of(`..+1`)`` and ``all_of(`.. 1`)``;
- negative-index and suffix controls;
- a pair of leading-zero references; and
- a qualified `base::c()` wrapper.

The written backticks above denote R identifier quoting. The actual runnable
spellings are retained in the script and the CSV rather than reconstructed from
this display.

The matrix used three data shapes, eight supplied-dots shapes and six public
positions: fixed `.by` and each of the five constructors. The dots arguments
included empty, literal, typed deferred, multiple deferred, explicit missing,
missing first, and deferred empty selections. Each direct/package pair created
fresh promises inside an actual dots frame. The independent expected selection
came from direct typed `tidyselect::eval_select()` with the same written
expression and lexical environment. A calibrated force counter checked promise
evaluation as well as the selected names or condition class.

All 1,008 comparisons were accounted for:

| Observation | Count |
| --- | ---: |
| Selected-column match | 372 |
| Matching External-condition leading class | 612 |
| Required empty rollup/cube refusal | 24 |

Force counts matched in every pair. No Grouping consumer extension to the
ordinary-summary candidate was established. The final execution exited zero.

An additional proposed missing-warning-option trigger was refuted before it
reached the package: base R rejected `options(warn = NULL)` with `option 'warn'
cannot be deleted`. It was not a supported caller option or a package finding.

## Tracker search and replay

The lane retained the results of these all-state title searches in
`issue-search.json` and `pr-search.json`:

```sh
gh issue list --state all --limit 100 --search 'grouping selection' --json number,title,state,url
gh pr list --state all --limit 100 --search 'grouping selection' --json number,title,state,url
```

The selected closed reports included #1090, #1139, #1144, #1184, #825, #868,
#873, #889, #924, #946, #975, #995, #1038 and #1039. #1184's complete
body/comments were retrieved to identify the implemented preflight repair;
`gh pr list --state all --limit 30 --search '1184'` identified merged #1198 and
the earlier research PR #1189. No confirmed new defect remained that required
a candidate-specific publication or duplicate judgment.

Run from the investigated checkout root:

```sh
Rscript investigation/2026-11-04-summary-grouping-share-bug-hunt/grouping/algebra-oracle.R
Rscript investigation/2026-11-04-summary-grouping-share-bug-hunt/grouping/dots-extended.R
```

Final retained shell executions were `61898` for the algebra oracle and `27659`
for the dots differential; both were collected through `write_stdin` with
terminal exit status zero. Earlier corrected oracle attempts also reached
terminal completion before the next execution. Testthat parallel execution was
explicitly disabled. No owned child process, connection or server remained.

An artifact-targeted `jarl check` passed after two mechanical research-script
style corrections; the final algebra replay above followed those corrections.
The adjacent logs and CSVs retained the final observations. `environment.txt`
recorded R 4.6.1 on aarch64 macOS; the complete algebra log also recorded dplyr
1.2.1, tidyselect 1.2.1, rlang 1.3.0, tibble 3.3.1 and pkgload 1.5.3.

## Limits

These finite negative observations established neither arbitrary caller-code
correctness nor complete External-condition object/message/backtrace equality.
They did not cover every active binding or setter effect, forged specification
fields, unbounded nesting/family size, fixed-key summary partitions, remote
backend execution, share denominators, every extension class, other dependency
versions or operating systems, concurrency, interruption, package build,
rendering or the complete test suite. The random-tree oracle used explicit
literal column selections; the dots matrix supplied the separate lexical and
typed-selection boundary.
