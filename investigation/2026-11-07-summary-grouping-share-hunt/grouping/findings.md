# Pattern-method rebinding during Grouping selection

Investigated: 2026-11-07
Tested package SHA: `e1dabaabb392a3791177e5c48e983f780c7c93ec`

This lane investigated Grouping specification capture, typed-selection proofs
and their recently repaired boundaries on the fetched-main worktree. It loaded
that source with pkgload without editing implementation or package tests. Its
retained harnesses explicitly disabled testthat parallel execution. No full
suite, package build/check, snapshots or rendering ran in this lane.

## G1: changing an implicit method invalidated a name-only pattern proof

The [minimal witness](dynamic-dispatch.R) used one character column `a`, an
ordinary inline stringr regex pattern, and the public `vars` argument of
`matches()`. That argument installed an ordinary `length.stringr_pattern` method
and returned `tidyselect::peek_vars()`. The method read the selection metadata
through an active binding; it returned one for character metadata and two for
integer metadata. Base function coercion over a pairlist constructed the
method without executing its body. It accepted its required `x` argument.

Direct typed tidyselect selected `a`. Unwrapped dtplyr inspection instead
raised an External condition:

```text
Direct typed selection: a
Unwrapped outcome: In argument: matches(...).
Caused by error in matches():
  stringr patterns must be length 1.
Unwrapped metadata: integer
Typed-wrapper selection: a
Typed-wrapper metadata: character, character, character
```

The generated method was absent when marginplyr classified the pattern. The
helper's `vars` promise installed it after the first pattern check but before
`matches()` asked the pattern's length again. Its implicit dispatch then read
integer name placeholders instead of faithful character metadata. The method
was valid on the direct typed path, so the observed refusal was introduced by
the names-only pass rather than by an invalid caller selection.

Wrapping only that same `vars` expression in `base::identity()` restored the
typed path and returned the expected plan. Installing the same method before
calling marginplyr also succeeded: the existing method-presence guard then
correctly selected typed metadata. A method-free ordinary `vars = peek_vars()`
control succeeded as well. The generated method was cleaned up between runs;
no package namespace or registered S3 method table was modified.

### Separating hypotheses

1. If the generated method or selection was invalid, direct typed tidyselect
   and the typed-wrapper control would also fail. Both succeeded.
2. If `vars` had no usable selection context, its ordinary literal/name-only
   control would fail. That control succeeded.
3. If the proof described only the method bindings before helper-operand
   evaluation, installing the method first or forcing faithful metadata with a
   wrapper would succeed, while installing it during evaluation would fail.
   Both comparisons showed that difference with the same runtime method.

The third hypothesis survived. The proof's explicit-callee rebound set did
not establish that an implicit S3 method lookup remained unchanged. The
pattern-consumption guard checked existing bindings; no binding existed at
that point. An assignment to a known method name could therefore make the
proof false before the pattern was consumed. This was a boundary in the
existing conservative proof, not a request to execute arbitrary caller code
in advance or construct a general R effect interpreter.

### Finite public-consumer comparisons

The [matrix](dynamic-dispatch-matrix.R) completed 144 distinct conditions:
three pattern modifiers (fixed, regex and collation), four input classes
(tibble, data.frame, data.table and immutable dtplyr), six positions (fixed
`.by` and the five constructors), and inspection versus summarization.

All 144 unwrapped calls falsely refused with the same pattern-length fragment
and recorded integer metadata. Each had a separate direct typed reference and
three public controls: the typed operand wrapper, a method installed before
classification, and a literal column selection. All 432 public controls
succeeded and matched their corresponding literal result exactly; all 144
direct typed references selected `a`. Input serialization was unchanged.
The separate controls-only invocation repeated those 144 conditions without
running the known failures and returned zero. Counts describe finite probes
with overlapping controls, not an independent statistical sample.

### Contract and recommended repair slice

ADR 0008's selection-evaluation and leading-selection amendments require an
unproven selection to use faithful metadata while preserving caller evaluation
budgets and order. ADR 0013 requires inspection and Margin operations to share
Grouping semantics. The Grouping selection documentation states that predicates
use captured typed metadata, without processing lazy source rows. A method
that sees an integer placeholder in place of a character dimension violates
that contract even though the backend itself correctly answered its question.

One independently verifiable slice, with no blockers, was recommended:
**Keep Grouping metadata faithful after pattern-method rebinding.** Acceptance
should cover the demonstrated implicit-dispatch mutation during helper operand
evaluation for fixed keys and all constructors, inspection and summarization,
and eager/immutable dtplyr inputs. Use faithful typed metadata whenever the
existing proof cannot establish safe pattern consumption. Preserve caller
scope/evaluation budgets, ordinary proven name-only patterns, repaired static
method controls, unchanged input, and the refusal for derived lazy inputs whose
types cannot be established without processing source rows. No additional data
read or speculative callback execution is justified by this finding.

## Duplicate comparison

Live all-state issue searches for `"dispatch" "assignment"` and
`"length.stringr_pattern"` returned no matching issue. The search
`"method" "rebinding"` returned closed #1400. The all-state PR search
`"pattern" "dispatch"` returned merged #1379, #1352 and earlier research
PRs. The coordinator's export supplied all 711 issue bodies; a local
method/dispatch plus rebind/assignment search identified no additional report
of this Grouping boundary. Full bodies and comments of #1344, #1363 and #1400,
and merged repair PRs #1352 and #1379, were read.

- [#1344](https://github.com/sayuks/marginplyr/issues/1344) covered a length
  method present when the pattern was classified. Its repaired preinstalled
  control succeeded here. The new witness installed that method afterwards.
- [#1363](https://github.com/sayuks/marginplyr/issues/1363) added vctrs proxy
  methods to that existing-binding check. This witness used length dispatch,
  no proxy override, and changed the lookup state during the helper call.
- [#1400](https://github.com/sayuks/marginplyr/issues/1400) addressed explicit
  locally rebound callees and their lexical function lookup. This witness
  never called the rebound method by name; genuine tidyselect `matches()`
  reached it through implicit dispatch after the proof had completed.
- [PR #1352](https://github.com/sayuks/marginplyr/pull/1352) and
  [PR #1379](https://github.com/sayuks/marginplyr/pull/1379) implemented the
  earlier static method-presence guards. Their retained public controls did
  not cover this demonstrated installation-after-proof boundary.

The first attempt to use `gh search issues` with an embedded repository query
was rejected by the CLI's query formatting. Corrected `gh issue list --state
all --search` queries above completed; no absence conclusion used the failed
query. The coordinator owned final-main refresh and publication.

## Replay and environment

Run the scripts by absolute path while the working directory is an archive of
the named package SHA:

```sh
Rscript /path/to/grouping/dynamic-dispatch.R
Rscript /path/to/grouping/dynamic-dispatch.R --assert
Rscript /path/to/grouping/dynamic-dispatch-matrix.R
Rscript /path/to/grouping/dynamic-dispatch-matrix.R --controls-only
```

The minimal observation returned zero; `--assert` intentionally returned one
because it required the unwrapped result to match the successful typed
reference. The matrix and controls-only invocations returned zero. The matrix
writes CSVs next to its script unless `MARGINPLYR_HUNT_OUTPUT` supplies another
directory. The coordinator's independent archive replay is separate evidence.

A first exploratory method constructor produced a zero-argument function and
correctly failed as a caller error; replacing its body-list with a pairlist
established the valid `x` formal before the retained runs. An initial matrix
fixture matched an extra column named `value`; that fixture was corrected to
an unrelated `z` before the reported 144-condition run. Neither setup failure
was counted as a package defect or a completed comparison.

R was 4.6.1 on aarch64-apple-darwin23, with dplyr 1.2.1, tidyselect 1.2.1,
stringr 1.6.0, vctrs 0.7.3, rlang 1.3.0, dtplyr 1.3.3, data.table 1.18.6.1
and pkgload 1.5.3. The scripts used separate R processes, established no
external connection and left no owned process running. No claim was made for
arbitrary side effects, every custom class, all Grouping families, hosted SQL
services, concurrency, interruption, other versions or other platforms.
