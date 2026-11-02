# Grouping specification selection effects and empty dots

Investigated: 2026-11-02
Baseline: `978d7bef4247bf1348af5b90bfab47924db25500` (`main`)

This track established one new public-interface defect: a valid selection
helper argument containing an empty `...` binding escaped from the name-only
classifier as an internal missing-value error. No package implementation or
permanent package test was changed.

## Readings and previous evidence

The readings included `CLAUDE.md` and its `AGENTS.md` closure, `CONTEXT.md`,
ADRs 0008, 0009, 0013, 0019 and 0026, the Grouping specification compiler,
inspection and selection readers, and relevant selection-budget, qualified
selection and leading-error tests. The October 30
[specification lifecycle record](../../2026-10-30-nest-spec-greedy/spec/README.md),
October 31 [closure/identity record](../../2026-10-31-nest-spec-greedy/spec/README.md)
and [selection/expansion record](../../2026-10-31-expand-grouping-greedy/spec/README.md)
informed the remaining boundaries. Those earlier executions were context and
were not counted as new observations.

The documented `.by` and constructor positions resolve column selections with
tidyselect; an empty selection is valid except where a rollup/cube unit must
contain a dimension. A valid empty helper argument therefore has a distinct
answer from an empty constructor argument or an omitted formal argument.

## Confirmed: empty dots escape the helper-argument classifier

The complete minimal wrapper was:

```r
f <- function(...) {
  inspect_grouping(tibble::tibble(a = 1L), .by = all_of(c(...)))
}
f()
#> Error in FUN(X[[i]], ...): argument "value" is missing, with no default
```

The same expression resolved directly with `tidyselect::eval_select()` in the
wrapper's environment selected no columns. Passing `"a"` through the dots
selected `a` in both APIs. The Grouping specification spelling
`grouping_set(all_of(c(...)))` had the same empty-dots failure.

`empty-dots.R` executed 360 calls across `.by` and all five constructors,
`all_of()` and `any_of()`, direct `c(...)`, `unlist(list(...))`, and a control
wrapping the entire helper in `identity()`. It exercised inspection,
`nest_with_margins()`, `nest_by_with_margins()`, expansion and summarization.
All calls used the local two-column input declared by the script.

- 120 empty-dots calls leaked `getvarError`, naming internal `value`. Eighty
  were false refusals of valid empty selections; forty replaced the required
  empty-rollup/cube Package condition with that internal error.
- 180 populated-dots calls succeeded.
- 60 empty-dots wrapper controls produced 40 valid empty selections and 20
  required empty-rollup/cube Package conditions.

The final report run exited zero. The retained desired-behavior assertion
exited one at `!inherits(minimal, "error")`, after printing the actual error and
the successful direct tidyselect control. The final CSV retained every call.
An early development fixture used `all_of(list(...))`, whose nonempty list is
not a valid native selection. That shape was removed from the final matrix;
its native failure was a harness limitation, not a package finding.

### Causal controls and ranked hypotheses

The failure was narrowed against three hypotheses:

1. The name-only helper-argument reader read empty dots as an ordinary lexical
   binding and then forced a missing marker through its local `value`.
2. Tidy-evaluation capture lost the wrapper's dots environment.
3. Retention of a constructor-shaped selection corrupted the selection value.

`empty-dots-cause.R` confirmed the first. For empty dots,
`selection_binding_deferred(env, "...")` answered `FALSE`, and a direct call
of `is_name_only_helper_arg(quote(...), env)` reproduced the missing `value`
error. The same reader answered normally for populated dots. Wrapping the
whole helper in `identity(all_of(c(...)))` deferred the outer selection to its
ordinary typed pass and returned the correct empty fixed keys. An explicit
`all_of(character())` worked too.

The second hypothesis was refuted by direct evaluation of the same captured
quosure in the same environment; the third by the `.by`-only minimal call,
which contained no constructor. These controls neither changed package code
nor implemented a proposed repair.

### Duplicate search and recommended ticket

All-state GitHub issue and pull-request searches used `dots`, `"empty dots"`,
`"empty" "selection"`, `"argument" "value" "missing"`, and retained-selection
terms. The selected raw search responses were retained alongside this note.
The full bodies/comments of #351, #925 and #1138 were read, together with #1139
and the body/comments of PR #1142. No matching empty-dots classifier report was
found in those results.

- [#351](https://github.com/sayuks/marginplyr/issues/351) concerned an empty
  argument written inside a selection, such as `c(, a)`, rather than a valid
  `c(...)` whose dots binding held zero arguments.
- [#925](https://github.com/sayuks/marginplyr/issues/925) concerned an omitted
  formal forwarded through embrace and parentheses.
- [#1138](https://github.com/sayuks/marginplyr/issues/1138) concerned populated
  dots in inline local summary selectors. Its raw quoted selection was resolved
  by the local-summary rewrite in a different evaluation scope. This finding
  arose earlier in Grouping-plan preparation, in the name-only helper-argument
  classifier, and required no summary selector. Each repair was independently
  verifiable and neither gated the other.
- [#1139](https://github.com/sayuks/marginplyr/issues/1139) concerned an already
  resolved fixed-key overlap preceding independent typed metadata acquisition;
  it did not cover empty helper arguments.

Recommended slice: **[P2] Preserve empty dots in Grouping selection-helper
arguments**, with no blockers. It should make valid empty forwarded selections
work across `.by` and Grouping specification positions, preserve the required
empty-rollup/cube refusals, and retain populated forwarding, evaluation budgets
and the no-unrequested-read contract. The public wrapper and direct tidyselect
control provide an independent regression seam. No prefactoring prerequisite
was established.

## Refuted: retained ordinary selection values and condition/effect counts

`retained-values.R` compared seventeen ordinary values with direct
`tidyselect::eval_select()`: character/numeric/negative/empty selections,
missing and logical invalid selectors, a renaming vector, a classed vector, a
predicate, language, a quosure, an expression, a pairlist and an environment.
Each value was returned by a caller function named as a constructor, through a
caller-defined qualified head, or by an active external binding, with zero to
two redundant parentheses, inside each of the five constructors.

All 765 final comparisons were accounted for: 264 correct selections, 420
matching External-condition classes and complete messages, 45 required
renaming refusals and 36 required empty-unit refusals. Every value-producing
caller was read once by the public operation. The initial classification of
the renaming vector expected tidyselect's own refusal; it was corrected to the
documented Package-condition refusal before the final successful replay.

`selection-effects.R` executed another 200 combinations of the five
constructors, absent/typed `.by`, five caller-effect shapes, and zero through
three redundant parentheses. The shapes were an ordinary helper closure, an
active binding, a constructor-shaped caller function, a caller-defined
qualified constructor, and an injected closure quosure. Every result selected
`a`; every caller ran exactly once and emitted one warning retaining the
`probe_warning` class. The log/CSV recorded event order and warning messages.
They did not assert complete warning-object identity or every possible
condition attribute.

## Replay, environment and limits

Run from the investigated checkout root:

```sh
Rscript investigation/2026-11-02-nest-grouping-hunt/spec/empty-dots.R
Rscript investigation/2026-11-02-nest-grouping-hunt/spec/empty-dots.R --assert
Rscript investigation/2026-11-02-nest-grouping-hunt/spec/empty-dots-cause.R
Rscript investigation/2026-11-02-nest-grouping-hunt/spec/retained-values.R
Rscript investigation/2026-11-02-nest-grouping-hunt/spec/selection-effects.R
```

Only `--assert` was expected to exit one on the investigated SHA. Every final
report/control entry exited zero; the logs retained `sessionInfo()` where
applicable. The environment was R 4.6.1 on aarch64 macOS Tahoe 26.6.2, dplyr
1.2.1, tidyselect 1.2.1, rlang 1.3.0, tibble 3.3.1 and pkgload 1.5.3.
Testthat parallel execution was explicitly disabled.

These were finite probes, not proof that arbitrary selections were correct.
They did not execute remote services, claim every backend shared the measured
local symptom, exercise unbounded specification trees, inspect arbitrary
forged specification fields, or establish behavior for other R/dependency
versions. They did not rerun the package suite or repair the finding.
