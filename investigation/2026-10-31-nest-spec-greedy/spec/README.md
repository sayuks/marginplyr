# Grouping specification closure and identity composition observations

Investigated: 2026-10-31
Baseline: `339315c3` (`main`)

This track examined Grouping specification construction and inspection, with
Grouping identity helpers as an independent consumer of each resolved plan.
It established no new reproducible defect in the completed finite probes.
No package implementation or permanent test was changed.

## Readings and prior boundaries

The readings included `CLAUDE.md`, its `AGENTS.md` closure, `CONTEXT.md`,
ADRs 0008, 0013, 0019, and 0026, the Grouping compiler and inspection/helper
implementations, and selection-budget/helper-argument tests. The October 30
[specification lifecycle record](../../2026-10-30-nest-spec-greedy/spec/README.md),
October 29 [algebra/reentrant-selection record](../../2026-10-29-nest-spec-exhaustive/specification/README.md),
[dispatch follow-up](../../2026-10-29-nest-spec-dispatch-followup/README.md),
and October 30 [expansion hunt](../../expand-grouping-exhaustive-hunt-2026-10-30.md)
informed the additional boundaries. Their old counts were not counted as
new execution. The open Issue list was read; it held the release and two
upstream-report tasks, with no open target defect.

## Closure defaults, helper call heads, and typed selections

`closure-selection-differential.R` used seed `31103119`. Twenty selection
bodies covered direct and closure-mediated proxy names/types, factor
predicates, zero-length answers, numeric positions, reordered names, names
with a non-ASCII spelling, unknown columns, renaming, and invalid logical
subscripts. Four closure forms placed the body directly, inside an ordinary
call, in a default argument, or behind one default forwarding to another.
Three helper-head forms used zero, one, or two parenthesis pairs. Eight
randomized four-column inputs varied character, integer, logical, and factor
types. All five constructors consumed the selection.

The oracle resolved each original selection directly against typed
`tidyselect::eval_select()` and independently expanded the resulting column
names into the constructor's set family. It did not use the package's
expansion helpers. The completed 9,600 comparisons produced:

- 7,008 exact included-set matches;
- 1,440 matching External-condition classes for invalid selections;
- 480 required renaming refusals; and
- 672 required empty rollup/cube-unit refusals.

No difference was measured. These comparisons did not assert complete
External-condition message or traceback equality, evaluation-count equality,
or arbitrary class-specific dispatch. The final log included `sessionInfo()`.

## Generated composition and Grouping identity

`identity-composition.R` used seed `31103147` for 300 generated ASTs of up to
three constructor levels. The generator admitted all five kinds, nested
families in unions/products, overlapping rollup/cube units, composite
Grouping sets, empty sets/products, and columns whose names contained a comma,
an equals sign, a backtick, or Japanese characters. A separate recursive
oracle expanded the AST; no package compiler helper supplied its sets.

All 900 policy observations were accounted for: 808 inspections matched exact
set membership/order, fixed keys, and named Grouping bits, while 92 duplicate
families received the required Package condition under the error policy.
Dropping duplicates was compared with stable deduplication of the independent
family; retaining them preserved occurrence order.

For every generated AST, a Margin summary also evaluated `grouping_id()`, one
`grouping_bit()` per fixed/variable column, and a second Grouping identifier
whose arguments were an independently randomized subset in independently
randomized order. That subset could include the fixed key. All 2,748 summary
rows matched the independent absence masks and the inspected Grouping set
identifier. No sampled AST crossed the probe's 1,024-occurrence bound.

## Replay and limits

Run from the investigated checkout root:

```sh
Rscript investigation/2026-10-31-nest-spec-greedy/spec/closure-selection-differential.R
Rscript investigation/2026-10-31-nest-spec-greedy/spec/identity-composition.R
```

Each entry point loaded that checkout with `pkgload::load_all()` and its final
run exited zero. Adjacent logs retained the results. These were negative
observations, not an assertion that the interfaces were free of bugs or that a
previously reported defect was repaired. Other R/dependency versions, other
operating systems, arbitrary specification-field mutation, unbounded ASTs,
remote query execution, and every possible custom class/condition were outside
these probes. The local/backend tracks examined their own nesting boundaries.
