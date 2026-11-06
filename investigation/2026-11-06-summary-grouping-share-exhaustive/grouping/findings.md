# Grouping selection proofs after local callee rebinding

Investigated: 2026-11-06
Tested package SHA: `9ffb24614a71512170298a3827bc11c80354e742`

This lane loaded the investigated worktree through pkgload without changing
package implementation or package tests. Every retained R harness explicitly
disabled testthat parallel execution and ran to terminal completion before the
next began. The coordinator owned fresh-main confirmation and ticket publication.

The preceding November 6 Grouping notes supplied the repaired stringr
length/proxy controls, family algebra coverage and qualified-forwarding
boundaries. The lane did not count those earlier matrices as new executions.
No earlier note was amended or superseded.

## G1: a callback's new callee was proved using its old lexical binding

The [minimal reproduction](callee-rebinding.R) used ordinary `matches()` with a
caller-defined callback. That callback assigned a locally constructed closure
to the name `paste0`, then called it. Base R's `as.vector()` constructed the
closure from a symbol, using its documented function coercion. A nonfunction
binding named `paste0` in the enclosing environment made variable-position
lookup and function-position lookup distinguishable. The generated closure
read an active binding whose string selected character column `a` with typed
metadata and integer column `b` with name-placeholder metadata. It returned a
valid string in both contexts; it raised no error itself.

Direct typed tidyselect selected `a`. Summarization with fixed key `b` and
that Grouping dimension instead refused a nonexistent overlap:

```text
Direct typed selection: a
Summary outcome: Columns cannot appear in both `.by` and `.grouping`:
  `b`.
Summary callback metadata: integer
Typed-wrapper control: east, west
Control callback metadata: character
```

Wrapping the callback result in ordinary `base::identity()` selected `a` and
produced the intended independent `east` and `west` groups. The unchanged
caller callback therefore exposed a false preflight result, rather than an
invalid predicate, invalid proxy or backend-specific source-row evaluation.
The witness used no namespace mutation, S3 method override, package internals,
or source-data read; the active binding read only the selection metadata.

Three separating hypotheses were considered:

1. The generated closure or active binding was invalid in a selection. The
   direct typed reference and typed-wrapper control would then fail too.
2. The observed overlap represented a real overlap or a dtplyr-only issue.
   Literal `all_of("a")` and eager controls would then reproduce it.
3. The static proof resolved a call against an enclosing function while
   ignoring a preceding local assignment that changed that call's target.
   Preserving the same runtime callback but exposing an ordinary literal
   closure, or removing the nonfunction shadow, would defer to typed metadata.

The direct/literal/wrapper observations refuted the first two. The
[separating controls](callee-rebinding-controls.R) confirmed the third's
predictions: a literal `function()` in place of `as.vector()` and removal of
the enclosing nonfunction binding each preserved the intended `a` dimension.
Those are independent successful controls for the same missing proof boundary,
not additional bugs.

### Finite public-consumer matrix

The [matrix](callee-rebinding-matrix.R) crossed four input classes (tibble,
data.frame, data.table and immutable dtplyr), all five Grouping constructors,
fixed-key and Grouping-dimension positions, inspection and summarization.
Unfixed inspection was also compared. It completed 68 plain-call probes,
each with a separately executed typed-wrapper and literal-selection reference:

| Observation | Cases |
| --- | ---: |
| False fixed-key overlap refusal | 48 |
| Silent wrong Grouping plan on dtplyr inspection without fixed keys | 5 |
| Eager unfixed inspection whose canonical typed pass recovered the correct plan | 15 |
| Typed-wrapper references exactly matching literal references | 68 |

The [CSV](callee-rebinding-matrix.csv) records the complete fixture keys,
metadata observations and errors. All four caller inputs had unchanged
serializations after execution. The five silent dtplyr cases used its
name-only inspection shortcut: both compilation passes saw integer name
placeholders, so the returned plan included `b` rather than the requested
character column `a`. The eager unfixed cases reached typed canonical
compilation and returned `a`; they still exposed the earlier integer pass.

### Contract, likely cause and recommended slice

ADR 0008's selection-evaluation budgets and leading-selection amendment,
ADR 0005's preflight boundary and ADR 0013's correspondence between inspection
and Margin operations require an unproven typed selection to reach faithful
metadata. A refusal or returned plan based on invented column types violates
that existing contract; accepting new selection syntax is unnecessary.

In the investigated implementation, `is_name_only_helper_arg()` treated
`as.vector()` as an unconditionally safe name operation. Its recursive
function-body walk checked each expression against the original lexical
bindings, without carrying local assignments forward. The assignment's left
side read as an ordinary atomic value, while `selection_argument_function()`
skipped that same nonfunction in call position and found base `paste0`.
The resulting proof described the old function rather than the locally
constructed closure that actually ran. That combination incorrectly admitted
this callback to the names-only pass. The separating controls kept runtime
selection semantics while changing only the proof boundary.

One independent repair slice with no blockers was recommended: preserve typed
selection when a callback constructs/rebinds a local callee whose behavior is
not established by the lexical proof, covering `.by`, all constructors,
inspection and summarization. Retain ordinary proven name-only callbacks,
caller evaluation budgets and lazy query policy. No implementation approach
was prescribed or attempted.

### Duplicate comparison

The coordinator's all-state exports contained 702 Issue bodies and 691 PR
bodies. This lane searched them for `as.vector`, `local callee`,
`callee rebinding`, rebound/callee combinations and assignment/selection
combinations. No record described this local-rebinding and closure-construction
boundary. Live all-state searches for `"as.vector" "selection"` found no
Issue; `"name-only" "callee"` found closed [#924], and
`"callee" "selection"` located the related merged [PR #1284]. Full bodies
and comments of those two related reports were read.

[#924] concerned an unrelated package export named like a tidyselect helper;
the witness here called genuine `matches()` and had no foreign namespace.
[PR #1284] repaired function-position lookup through nonfunction lexical
bindings and deferred namespace exports. Its review explicitly covered the
nonfunction lookup distinction, but did not establish that a function body
could rebind that callee while retaining the lexical proof. The present
witness required the repaired lookup plus the untracked assignment.
Stringr method-family reports [#1344] and [#1363] concerned downstream S3
consumption of standard patterns; this witness used an ordinary character
pattern and registered no method. The coordinator owned the final live-main
and duplicate disposition.

[#924]: https://github.com/sayuks/marginplyr/issues/924
[PR #1284]: https://github.com/sayuks/marginplyr/pull/1284
[#1344]: https://github.com/sayuks/marginplyr/issues/1344
[#1363]: https://github.com/sayuks/marginplyr/issues/1363

## Additional rejected hypotheses and limits

Small unretained probes checked whether implicit methods on unclassed atomic
values reached the whitelist's internal generics; none did for the tested
character/list/integer inputs. A standard stringr private `type` method-table
probe did not replace its namespace-resolved method and established no defect.
Those exploratory probes were removed rather than presented as coverage of
all possible S3 dispatch. No new defect was claimed for those hypotheses.

The retained evidence established this specific finite local proof failure,
not arbitrary R reflection, every closure constructor, SQL/Arrow selection
capabilities, hosted backends, unlimited specifications, concurrency,
interruption, other dependency versions or other platforms. No full suite,
package build/check, snapshot run, rendering or implementation repair was
performed by this lane.

## Replay and environment

Run from an archive of the named package SHA; pass the retained scripts by
absolute path if they are outside that archive. `MARGINPLYR_HUNT_OUTPUT`
controls the matrix CSV destination and defaults to R's temporary directory.

```sh
Rscript /path/to/grouping/callee-rebinding.R
Rscript /path/to/grouping/callee-rebinding.R --assert
Rscript /path/to/grouping/callee-rebinding-controls.R
MARGINPLYR_HUNT_OUTPUT=/path/to/output Rscript /path/to/grouping/callee-rebinding-matrix.R
```

The final observation/desired-contract pair was collected in execution
`98ac72`: observation returned 0 and `--assert` intentionally returned 1.
The complete matrix execution `7eb3e5` and separating-control execution
`258a0a` returned 0. These were terminal tool results with no retained child
process, database connection or server. Observation modes assert the dated
failure; the explicit contract mode can pass only when the observed public
result matches the successful typed control. They are research harnesses,
not package regression tests or evidence that a repair occurred.

An initial separating-control harness left its callback as an unforced formal,
which correctly selected the typed path and could not reproduce the original
classification. Forcing that formal before capture restored the intended
fixture. Only the final successful control execution was counted above.

R was 4.6.1 on aarch64-apple-darwin23, with dplyr 1.2.1, tidyselect 1.2.1,
rlang 1.3.0, vctrs 0.7.3, dtplyr 1.3.3, data.table 1.18.6.1 and pkgload 1.5.3.
The retained transcripts print the same versions. All script executions had
terminated when the lane returned its results.
