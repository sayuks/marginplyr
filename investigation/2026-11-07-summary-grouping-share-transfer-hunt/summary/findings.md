# Preceding-summary rebinding and ordinary across output prediction

Investigated: 2026-11-07
Repository snapshot: `0b5143133c023ccb4efb6fe27f1ef551e9f49186`

This lane used R 4.6.1, dplyr 1.2.1, rlang 1.3.0, vctrs 0.7.3,
tibble 3.3.1 and data.table 1.18.6.1 on arm64 macOS. It loaded the exact
source with `pkgload::load_all()` and set `TESTTHAT_PARALLEL=false`.
No implementation or package regression test was edited.

## Confirmed: output prediction trusts a binding that an earlier summary changes

The smallest witness used input `x = c(1, 3)` and a lexical variable `fns`
initially bound to `sum`. The first ordinary summary assigned the named
function list `list(s = sum, m = max)` to that binding and returned zero.
The second ordinary summary was `across(x, fns, .names = "answer_{.fn}")`.
Ordinary dplyr returned `ready = 0L`, `answer_s = 4`, and `answer_m = 3`.
The Margin call requesting `.id = "answer_1"` refused before the first
summary ran:

```text
`.id` (`answer_1`) conflicts with a summary output.
```

No actual summary would produce `answer_1`. Using the nonexistent predicted
name for a fixed key or Grouping dimension likewise falsely refused an
overwrite. Replacing the function expression with its equivalent literal
named list retained all ordinary dplyr values and accepted each public name.

Three further entrances reproduced the same timing defect. An earlier summary
rebound the ordinary bare `list` callee before a later function-list call, or
rebound the caller's namespace operator before an authored `base::sum`
expression. Another changed an ordinary `.unpack` binding from FALSE to TRUE:
a frame-valued across with outer name `answer` actually produced `answer_z`,
but requesting `.id = "answer"` falsely refused the old packed output name.
In each case planning established the old output shape and
used that shape to justify a collision, although ordinary evaluation would
see the newer value.

### Contract, hypotheses and cause

The summary reference's *Relationship to dplyr summaries* delegates ordinary
name-value expressions to dplyr. ADR 0019's rule keeps non-Contextual names
and caller-authored namespace operations under ordinary lookup. Closed
#607, #629 and #1421 require an established output shape before it can justify
a speculative collision refusal; none requires an ordinary later expression
to use the calling environment's value from before preceding summaries ran.

The deterministic differential separated these hypotheses:

1. Prediction read a valid initial function shape but failed to account for
   an earlier summary changing the binding. Keeping the actual shape in a
   literal list should remove the refusal.
2. Rebinding was unsupported by dplyr itself. The ordinary dplyr control
   would then refuse or use the old function.
3. The identifier implementation alone was defective. Fixed-key and variable
   dimension roles should then work.

The ordinary control refuted hypothesis 2, all three roles refuted hypothesis
3, and all literal controls supported hypothesis 1. `predictable_local_across_names()`
walked every dot and asked `known_across_output_names()` against the captured
environment before executing any ordinary summary. Its shape proofs checked
the environment's entry-time function/list/namespace/unpack bindings without
invalidating them across preceding dots. `execute_margin_summary()` consumed
the resulting predictable names in public-name collision checks before any
branch ran.

### Finite replay and controls

`across-prior-rebinding.R <source-root>` completed 36 forward observations
(three eager classes, identifier/fixed/variable roles and four entrances).
Every forward observation produced the false refusal. It also completed
36 exact literal-list/literal-unpack controls, 36 genuine-collision refusals and nine
reverse controls in which a known list changed to one function. Input
serializations remained unchanged. The reverse controls confirmed that a
shape initially classified as unknown did not cause the false refusal.

`--assert` instead requires the desired public result and exited one at the
first confirmed failure. `--controls-only` completed with exit zero. The
observation run completed with exit zero and wrote the adjacent CSV.
The retained final execution handles were `99159` (observation), `24928`
(controls), and the unyielded contract execution `4c1baf` (exit one).
All were collected through their original execution interfaces.

Initial harness attempts failed before completing their intended matrix:
a data.table constructor treated `key` as its formal, a custom namespace
delegate recursively invoked itself, the classifier expected a plural
diagnostic, and a final CSV option used an invalid `Sys.getenv()` unset
value. The retained script corrected those harness errors and reran every
claimed matrix cell. None was counted as a package finding.

### Duplicate assessment and recommended slice

The coordinator's all-state export contained 721 issues. This lane searched
title/body text for across/function-list plus rebinding, earlier-summary,
preceding-summary, mutation and prediction vocabulary. Live all-state Issue
queries were `"across" "rebinding"` and
`"across" "preceding" "predict"`; a live all-state PR query was
`"across" "rebind"`, each with limit 100. These returned no report of this
ordinary-summary temporal shape boundary.

The full bodies and comments of #607, #629, #693, #700 and #1421 were read. #607 started
with an already unknown bound list. #629 started with an already shadowed
ordinary list callee. #1421 started with an already shadowed namespace
operator and was implemented in merged PR #1426. #693 and #700 concerned
callback count and evaluation phase for dynamic unpack values, rather than
entry-time output predictions invalidated by an earlier ordinary summary.
The new witness started with a valid output-shape proof and invalidated it by executing an earlier
ordinary summary; it reproduced after those repairs on the pinned snapshot.

Recommended independent P2 slice, with no blocker:
**Keep ordinary across output predictions valid across preceding summaries**.
Acceptance should cover all four demonstrated entrances and public-name
roles, preserve exact ordinary values and evaluation timing, retain genuine
collision protection and no-prediction-only-evaluation, and avoid widening
static share-source eligibility. No prerequisite prefactor was found.

## Additional completed comparisons

`value-matrix.R` compared all 400 ordered pairs from 20 return shapes against
ordinary dplyr across two groups on a tibble input. The shapes included
integer, double, missing logical, NULL, character, raw, complex, factor,
Date, POSIXct, POSIXlt, difftime, matrix, array, list, list-of, plain/tibble
frames, a one-row zero-column frame and unspecified values. There was no
value or acceptance discrepancy. Paired errors established acceptance only,
not condition class/message/cause equality. Its retained handle `82582`
completed with exit zero.

`rebinding-probes.R` additionally compared function argument mutation during
column selection or naming. Those ordinary dplyr calls retained the function
value forced before those callbacks, and supplied no independent discrepancy.
The required boundary was an earlier ordinary summary, before across forced
its function expression. These finite comparisons establish no claim about
all reflective expressions, arbitrary method side effects, other backends,
dependency versions or platforms. The coordinator owned independent archive
replay, current-main confirmation and publication.
