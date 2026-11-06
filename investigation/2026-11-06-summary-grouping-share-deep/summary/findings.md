# Ordinary Margin summary comparisons

Investigated: 2026-11-06
Repository snapshot: `547d77095c2923573216d002f50bef21778247f9`

The lane executed standalone R probes from a disposable archive of that SHA
at `/private/tmp/summary-deep-source`, with `TESTTHAT_PARALLEL=false`.
[The environment record](environment.txt) identifies R and dependency versions.
No package implementation, package test, generated document or dependency
declaration changed. No additional confirmed defect was established in this
lane.

## Contracts and previous findings

The summary reference's *Relationship to dplyr summaries* supplied the ordinary
data-mask, sequential-output and unnamed-frame expectations. ADR 0028 supplied
the distinction between an Assigned summary name and a caller-written packed
name; ADR 0016 supplied the delegation of ordinary result types to dplyr and
vctrs. The lane read both ADRs in full, together with `CLAUDE.md`, `AGENTS.md`
and `CONTEXT.md` at the investigated revision. The November 6 summary lane's
retained findings supplied the prior 13-by-13 type comparison, across callback
controls and the frame-subclass dispatch finding (#1365). The latter repair's
stored-column construction was present at the tested SHA.

## Completed comparisons

| Probe | Hypothesis and separating observation | Observed result |
| --- | --- | --- |
| [Per-group values](value-type-controls.R) | Wrapping unnamed values could alter dplyr's cross-group common type or acceptance. Compare the same two-group ordinary summary through dplyr and marginplyr, varying each group's value independently. | All 1,156 ordered pairs from 34 value kinds agreed in acceptance and accepted result values. |
| [Sequential selections](sequential-selection-controls.R) | Rewriting a later selection could miss a preceding overwritten column or expanded frame output. Compare explicit payload selections after six preceding-summary shapes, through 13 expressions and caller-named/unnamed forms. | All 156 comparisons agreed. |
| [Across corners](across-corner-controls.R) | Ordinary across rewriting could change function-list, name-template or unpacking behavior. Cross 14 function forms, seven name forms, three unpack values and three written shapes. | All 882 comparisons agreed in acceptance and accepted results. |
| [Share interference](share-interference-controls.R) | Staging an unrelated Total share could alter ordinary selection results or callback evaluation counts. Compare the same ordinary summary with and without the share, over 12 forms, three unpack values, caller naming and position before/after the ordinary source. | All 144 comparisons agreed in ordinary values and the four recorded callback counts. |

The value-kind matrix included data frames, empty frames, tibbles, differently
ordered/overlapping frame schemas, nested frame columns, matrix columns,
matrices, arrays, lists, NULL, unspecified values, Date, POSIXct, POSIXlt,
difftime, factors, ordered factors, raw and complex values. Both-errored cases
were treated as agreement in acceptance; the probes did not establish equal
condition classes, diagnostics or causes for those pairs.

The across matrix included duplicate/empty/missing function names, an empty
function list, invalid function values, scalar/vector/empty/missing name
templates, and FALSE/TRUE/string unpacking. Shapes were top-level,
parenthesized and caller-named. The interference matrix included forwarded
numbered dots and ellipsis, selection callbacks, function factories, naming
callbacks, unpacking callbacks, parenthesized/wrapped across, pick and scalar
consumers of pick.

## Oracle corrections and execution evidence

The initial combined type/sequence harness completed its type phase with
`PAIRS 1156 DIFFS 0`, then stopped because its next phase constructed the
invalid R expression `quote(x = sum(x))`. That was a harness error, not a
package finding. [The original transcript](initial-type-harness.log) retains
that terminal error. The retained type script contains the identical completed
phase plus a zero-difference assertion. The sequential phase was corrected to
represent its dot name separately from its expression and ran to exit zero
through retained execution `4280`.

The first interference fixture let a first across overwrite `x`, then made the
share source read `x`. Five cases differed because the share-bearing call
correctly refused that dependency on an earlier summary alias. The oracle was
therefore changed to calculate the share source from an untouched `z` column.
The final 144-case probe then completed through retained execution `67539`,
exit zero, with `INTERFERENCE 144 DIFFS 0`.
The across corner probe completed through retained execution `9395`, exit
zero, with `ACROSS 882 DIFFS 0`. All lane executions were collected.

Each retained script ends with an assertion requiring zero differences.
Replay it by changing into a disposable archive of the named SHA and passing
the script's absolute path to `Rscript`. These are finite research harnesses,
not package regression tests. The successful comparisons establish neither
arbitrary R-expression equivalence nor condition equality, S3-method side
effects, remote backend behavior, hosted services, concurrency, interruption,
other dependency versions or other platforms.
