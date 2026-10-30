# Grouping specification capture and lifecycle probes

Investigated: 2026-10-30
Baseline: `28c449aa03fd5289916f0673e41c848cc126999e` (`main`)

This track examined the five Grouping specification constructors and their
inspection/selection consumers. It established no new compiler or capture
defect within the finite lifecycle probes below. A separately requested
independent validation confirmed the local nesting membership finding; see
[independent-equality-validation.md](independent-equality-validation.md).
No package implementation or permanent test was edited.

## Primary readings and previous evidence

The readings included `CLAUDE.md`, `AGENTS.md`, `CONTEXT.md`, ADRs 0008 and
0026, the Grouping compiler, the selection-expression readers, and the capture,
cyclic-specification and evaluation-budget tests. The 2026-10-29
[nesting/Grouping deep-hunt specification record](../../2026-10-29-nesting-grouping-deep-hunt/spec/README.md),
[exhaustive specification record](../../2026-10-29-nest-spec-exhaustive/specification/README.md),
[dispatch follow-up](../../2026-10-29-nest-spec-dispatch-followup/README.md),
and [expansion systematic-hunt note](../../expand-grouping-systematic-hunt-2026-10-29.md)
informed the additional seams and duplicate search. The issue-title index
included the closed typed-selection, capture, cyclic-reference, and qualified
selection issues; no old symptom was promoted as a new compiler finding.

## Changed data and reused specification objects

`lifecycle-data-evolution.R` used seed `301030411`. Fourteen selectors covered
typed predicates, mutable captured name vectors, helper closures reading the
tidyselect data proxy, name patterns, numeric positions, empty selection,
column order, and helpers whose arguments read only proxy names. For each of
the five constructors, it reused both the captured specification and an
independently serialized/unserialized copy over twelve generated inputs whose
column names and types changed. The captured name-vector binding changed too.
Both local and immutable dtplyr inputs were inspected.

An independent oracle used direct typed `tidyselect::eval_select()` answers,
then expanded the respective set, union, rollup, cube, or one-family product
without calling the package compiler. Stable deduplication, exact included
columns, Grouping bits, and occurrence identifiers were compared. Of 3,360
comparisons, 3,128 matched the expected plans and 232 empty rollup/cube
selections received the expected Package condition. There was no measured
stale-plan or stale-type result across the input and environment changes.

The first harness attempt tried to mutate the empty environment of a constant
`NULL` quosure. That harness error was corrected by excluding the empty
environment from rebinding before the retained successful run. It was not a
package finding.

## Capture rebinding and injected environments

`capture-rebinding.R` compared four constructors admitting nested Grouping
specifications, zero through four parenthesis pairs, inspection and both
nesting verbs, and five successive child rebindings. One half used an active
nested-name binding; the other injected a quosure whose captured environment
was distinct from the outer constructor's environment. All 600 public-call
comparisons matched a freshly constructed control. The active nested name was
read once per operation. Changing the outer binding did not override the
injected quosure's own environment.

## Helper-head masking candidate

`helper-head-probe.R` examined bare, parenthesized, and qualified `all_of`
heads in the presence of a caller function of the same name that would inspect
column types. Direct tidyselect, fixed-key inspection, and Grouping-set
inspection all selected the same column in fifteen comparisons. The caller
masking function was read zero times after a calibrated direct call read it
once. This rejected the proposed parenthesized-head lexical-lookup mismatch in
the tested shapes.

## Replay and limits

Run from the investigated checkout root:

```sh
Rscript investigation/2026-10-30-nest-spec-greedy/spec/lifecycle-data-evolution.R
Rscript investigation/2026-10-30-nest-spec-greedy/spec/capture-rebinding.R
Rscript investigation/2026-10-30-nest-spec-greedy/spec/helper-head-probe.R
```

The adjacent logs recorded terminal success for all three final runs. The
lifecycle log included `sessionInfo()`. These were finite negative observations,
not proof that every Grouping specification or backend was correct. Unbounded
resource stress, arbitrary mutation of package internals, arbitrary custom
field methods on forged specification objects, other dependency versions, and
live SQL services were outside these probes.
