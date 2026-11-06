# Authored namespace lookup in ordinary across output prediction

Investigated: 2026-11-07
Repository snapshot: `e1dabaabb392a3791177e5c48e983f780c7c93ec`

This lane used `pkgload::load_all()` against the freshly fetched main worktree,
with `TESTTHAT_PARALLEL=false`. It changed no package implementation or package
regression test. The preceding November 5 ordinary-summary lane, November 6
deep lane and November 6 exhaustive lane supplied repaired controls and areas
already covered by finite matrices. Those older executions are not counted
as executions of this investigation.

## Confirmed: a qualified ordinary function expression invents output names

Ticket: [#1421](https://github.com/sayuks/marginplyr/issues/1421).

The minimal input held one integer column `x` with values one and three.
The caller's namespace operator returned the valid named function list
`list(s = sum, m = max)` for the authored expression `base::sum`, delegating
other namespace requests normally. Ordinary dplyr across evaluation, using
the name template `answer_{.fn}`, produced only `answer_s = 4L` and
`answer_m = 3L` after one namespace lookup.

The same ordinary Margin summary, with the Grouping set identifier named
`answer_1`, was refused before the namespace operator ran:

```text
`.id` (`answer_1`) conflicts with a summary output.
```

No summary actually produced `answer_1`. The equivalent literal function-list
control returned exactly the two ordinary summary values plus integer
identifier one. Removing the identifier also allowed the authored namespace
summary to produce its ordinary columns. A fixed key or Margin dimension
named for the invented output was likewise incorrectly refused as overwritten.

A second entrance used an authored qualified list-constructor call as `.fns`.
The namespace operator made `base::list(fake = mean)` return a function list
named `s` (and optionally `m`); prediction still used the syntactic `fake`
argument tag and refused the nonexistent `answer_fake` output. Both public
and private namespace qualification reproduced both entrances.

### Contract and separating hypotheses

The summary reference's *Relationship to dplyr summaries* delegates ordinary
name-value expressions to dplyr's data-mask semantics. ADR 0019's rule and
*isolate generated namespace operations* amendment keep caller-authored
namespace expressions as ordinary code in their captured environment.
Closed #607 and #629 require uncertain names to remain unknown instead of
causing speculative collision refusals, with actual outputs checked later.
This finding concerns ordinary `.fns` expressions; it does not make the
Contextual helper `across()` itself subject to caller replacement.

After establishing a deterministic differential, hypotheses were ranked:

1. Namespace-qualified function-shape prediction assumed the namespace's
   binding while execution used the caller's namespace operation. Replacing
   only that expression with its equivalent literal list should recover.
2. The contextual-share selector was generally broken. A share-free summary
   and ordinary/literal controls should then also fail independently of
   namespace qualification.
3. Only first-across naming-template planning was broken. Explicit naming
   should then eliminate the discrepancy.

The share-free identifier witness refuted the second hypothesis. Its explicit
template refuted the third. Both the namespace-value and qualified-list
variants supported the first. Inspection found two contributing predictions:
`known_across_function_names()` read a function directly from a loaded
namespace and treated it as one function, without establishing that the
authored namespace operation would return that value.
`is_known_function_list_call()` accepted a base-qualified list spelling
without establishing which namespace operation would run. The correct
literal-name proof for ordinary bare list calls did not apply to that branch.

### Red-capable replay and finite boundary matrix

From the named source root, run:

```sh
Rscript /path/to/minimal-across-namespace.R
Rscript /path/to/minimal-across-namespace.R --assert
Rscript /path/to/across-namespace-shape.R
Rscript /path/to/across-namespace-shape.R --assert
Rscript /path/to/across-namespace-shape.R --controls-only
```

Each script also accepts a source-root path as its first argument. It uses
the current directory when no path is supplied, including a disposable
source archive. Observation mode asserts the dated defect and returns zero;
desired-contract mode requires exact ordinary values and returns one on this
snapshot. The controls-only matrix applies that same desired-result assertion
to the equivalent literal function list and returns zero.

The 72-comparison matrix crossed three eager input classes (plain data.frame,
tibble and data.table), both namespace operators, identifier/fixed-key/rollup
roles, one/two named functions and the two namespace-expression entrances.
All 72 authored cases produced the speculative collision refusal after zero
namespace calls. All 72 literal controls exactly matched independently built
ordinary-dplyr branch results. Every input serialization was unchanged.
The CSV retains expected column names, values, diagnostics and lookup counts.

The completed replay driver ran observation, contract and controls modes for
the matrix, observation/contract modes for the minimal witness and the
ordinary-cardinality control. The terminal outcomes were respectively
0, 1, 0, 0, 1, 0; the expected intentional failures were collected. Retained
shell session `61963` completed with exit zero after collecting all six child
executions. The adjacent logs retain each child result. This is research
evidence, not a Review-ready or full-suite result.

### Related share-planning symptom

`share-namespace-symptom.R` added a following Total-share across request to
the same authored namespace function-list summary. Both namespace operators
produced a malformed-name error rather than a result or an eligibility
diagnostic: an unpartitioned call reported
`Can't transform a data frame with NA or "" names`, and a rollup reported
`Names can't be empty` with an empty name at location three. The equivalent
literal function-list controls returned both expected share columns.

These four cases identify a related shape-mismatch consequence. They do not
establish that a dynamically resolved function list must become an eligible
statically named share source. A repair must preserve the existing static
source-provenance restrictions; it must not manufacture invalid schema names
while deciding admissibility.

### Duplicate comparison and recommended ticket

All-state GitHub Issue searches used `"namespace" "across"`,
`"namespace" "function list"`, and `"across" "operator"`. All-state PR
searches used `"namespace" "across"` and `"function list"` (limit 100).
The coordinator's 711-issue body export was additionally searched for
namespace plus prediction/function-list/fns vocabulary. The complete bodies
and comments of #607, #629, #716, #1343, #1396 and #1400, and PRs #617 and
#644, were read.

- Closed #607 / merged #617 repaired unknown bound-function-list shape.
  The remaining namespace-value branch still treated a namespace's function
  as the value the caller would obtain.
- Closed #629 / merged #644 repaired ordinary unqualified frame/list callee
  prediction, explicitly retaining qualified standard constructors. The new
  witness replaces the authored namespace operation around such a spelling.
- Closed #1343 concerned package-generated naming glue, whereas these calls
  were deliberately authored ordinary `.fns` expressions.
- Closed #1396 concerned authored namespace evaluation around nested Grouping
  specifications; the new failure occurs in ordinary across output prediction.
- Closed #1400 concerned typed Grouping selection after local rebinding, not
  summary output names or namespace-expression values.

No separate report of either demonstrated ordinary-summary entrance was
established by those searches. Recommended independent slice, with no blocker:
**[P2] Respect authored namespace lookup when predicting across function shapes**.

Acceptance criteria should require correct identifier/fixed-key/rollup results
for both demonstrated entrances and namespace operators; preserve actual-output
collision protection, ordinary namespace evaluation counts and genuine standard
namespace predictions; introduce no prediction-only evaluation or source read;
and keep static share-source restrictions while avoiding malformed planned
schema names. The three eager classes and literal-list controls supply bounded
public-interface regression scenarios. No prefactoring prerequisite was found.

## Additional completed negative comparisons

`ordinary-cardinality-controls.R` crossed three eager classes with six pairs
of a one-row frame summary and zero/two/scalar/NULL ordinary values, in both
written orders where relevant. All 18 cases agreed in acceptance: 12 paired
errors and six exact accepted results. The paired errors established no
condition-message, class or cause equality. An initial exploratory version
had printed large backtraces; the retained compact script reran the completed
finite scope and asserted its verdict.

The initial matrix preparation incorrectly used data-frame bracket selection
on a data.table input. It stopped at fixture selection before establishing
that combination; replacing that setup step with dplyr selection supplied the
completed 72-case matrix. That harness error is not a package finding.

## Environment and limits

The source environment was arm64 macOS, R 4.6.1, dplyr 1.2.1, rlang 1.3.0,
tidyselect 1.2.1, vctrs 0.7.3, tibble 3.3.1 and data.table 1.18.6.1. The
original source worktree stayed unmodified. All owned R executions completed.
The coordinator owned independent archive replay, current-main confirmation,
final duplicate checking, issue publication and submission verification.

The comparisons did not establish behavior for every custom namespace operation,
arbitrary reflective code, custom method side effects, every lazy backend,
concurrency, interruption, other dependency versions or other platforms. No
package suite was used as a hunting loop and no repair was attempted.
