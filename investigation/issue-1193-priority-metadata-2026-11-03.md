# Duplicate test priority metadata and retained proposal recovery (#1193)

Investigated: 2026-11-03
Revised: 2026-11-03 — agent recovery workflow proposal in Revisions below (#1193)

## Scope and environment

This investigation diagnosed [#1193](https://github.com/sayuks/marginplyr/issues/1193)
and evaluated repair options. It did not change production metadata, runner code,
tests or retained original run artifacts, and did not run a measurement-only
suite. The dedicated worktree was `/private/tmp/marginplyr-1193`, branch
`investigate/1193-priority-metadata`, based on fetched main
`cea382e16797cfa612ae4ba6288ff403b013aa23`.

The diagnostic environment was macOS Tahoe 26.6.2, aarch64, R 4.6.1 and
testthat 3.3.2. Retained evidence came from successful full SHA
`1c26360d4c3fed4da9250b3870ca636dae23c925`, run
`20261103T122403Z-32687-7faf7928b8` in the #1185/#1186 worktree. Its terminal
record said `status = passed`, `heavy_check = passed`; timing evidence was
eligible, with four workers and 204 files. The original records were read only;
proposal experiments used copies under an automatically removed R temporary
directory. That full result certifies its own SHA, not this investigation.

## Reproduction and ranked hypotheses

The exact issue reproduction was run against fetched main:

```sh
Rscript -e 'source("tools/review-ready-priority.R"); review_ready_priority_read(readLines("DESCRIPTION"))'
```

It exited 1 with `priority must contain unique literal test filename stems`.
This isolates the symptom to metadata parsing, without workers or package
execution. The ranked hypotheses and distinguishing observations were:

1. Concurrent priority additions produced duplicate membership during
   integration: each branch should be valid individually, and the merge invalid.
2. Historical metadata parsing independently blocks reuse: repairing the target
   DESCRIPTION alone should still fail against the retained run.
3. Native dispatch and proposal validation have different contracts, with an
   integration coverage gap: native discovery should select every file once,
   while existing proposal fixtures could pass with invalid tracked metadata.

All three predictions were confirmed.

## Introduction through a clean textual merge

Both priority commits started from DESCRIPTION blob `1f66e483` and independently
registered `dtplyr-label-helpers` at different positions:

| Revision | Priority entries | Duplicate entries |
| --- | ---: | ---: |
| `3d631db7` (common main baseline) | 202 | 0 |
| `1a338e2a` (#1171 priority adoption) | 204 | 0 |
| `41ad3c5e` (#1170 priority adoption) | 203 | 0 |
| `a3b389a5` (merge of #1173) | 204 | 0 |
| `4b34fdec` (merge of #1174) | 205 | 1 |

`git show 1a338e2a -- DESCRIPTION` showed an insertion after
`sql-env-label-collision`; `git show 41ad3c5e -- DESCRIPTION` showed another
after `share-scope-callees`. `git show -m 4b34fdec -- DESCRIPTION` showed the
merge retained both. Thus the defect was introduced by the combined state at
`4b34fdecb2d8a41034b3d9b5d5af8e51592555e1`, rather than either isolated
priority adoption. This is a semantic conflict despite a clean textual merge.

The [#1174 verification record](https://github.com/sayuks/marginplyr/pull/1174#issuecomment-6602198443)
assessed fetched main `3d631db7` against `41ad3c5e`. It therefore did not
establish compatibility with the later `a3b389a5` state containing the other
registration. The integration workflow's *Check the current base* requires
reassessment if main advances; a clean tree merge does not establish semantic
compatibility. This record alone cannot establish which merge-time checks a
human or automation performed.

## Why package execution passed and proposal generation failed

The installed testthat 3.3.2 implementations of `find_test_scripts()`,
`find_test_start_first()` and `order_test_scripts()` were inspected directly.
`order_test_scripts()` returns `unique(c(unlist(mch), paths))`: repeated
registrations do not repeat execution, and the first match determines order.
Native discovery on fetched main selected all 204 distinct files exactly once.
Removing only the later duplicate preserved the entire ordered filename vector,
not just membership.

In `tools/review-ready-priority.R`, `review_ready_priority_read()` instead
rejects duplicates. `review_ready_priority_evidence()` calls that strict reader
on the archived DESCRIPTION before `review_ready_priority_propose()` reads the
target's priority. Hence repairing only the tracked DESCRIPTION cannot recover
an otherwise eligible old run containing the duplicate.

`finish_review_ready_timing()` in `tools/review-ready-check-lib.R` invokes
proposal generation after recording the successful full outcome. Proposal
failure returns `eligible = FALSE` and a diagnostic rather than changing the
check's verdict. Preserving the full outcome follows ADR 0040; the failure to
produce optimization artifacts is the defect, not a hidden package-test failure.

The CI-invoked `tools/test-review-ready-priority.R` exercised disposable fixture
metadata but did not parse this repository's tracked DESCRIPTION. It was run
unchanged on the affected main and exited 0 with
`Priority proposal contracts passed.` The existing fixtures therefore did not
cover this integration defect. Source-scope baseline checks also had no call to
the priority reader; proposal generation was reached only by full finalization.

## Retained-evidence experiment

The [diagnostic script](issue-1193-priority-metadata-2026-11-03/diagnose.R) was run
from the worktree root as:

```sh
Rscript investigation/issue-1193-priority-metadata-2026-11-03/diagnose.R
```

It archived the measured SHA to disposable source, copied the retained
invocation/result/timing evidence, and removed the target DESCRIPTION's later
duplicate. The unchanged proposal code still returned the issue's diagnostic.
All preceding eligibility checks passed, locating this second blocker at the
archived-priority read.

A private, in-memory diagnostic variant normalized only the historical priority
with stable `unique()`. It kept the current-priority strict reader, rejected
nonliteral/empty historical entries through that reader, and left archived
bytes, hashes, dates, raw intervals and results unchanged. All normal evidence
checks still ran. Proposal generation succeeded and produced explanation/RDS
artifacts; no patch was needed after target deduplication. These artifacts were
discarded with the temporary workspace; no proposal was adopted.

| Four-worker model | Seconds |
| --- | ---: |
| native | 676.224 |
| registration | 676.224 |
| candidate | 678.048 |
| selected | 676.224 |

There were no new registrations or removals after deduplication and no selected
optional reordering. The modeled gain was zero; these are schedule models, not
observed runtimes or speedups. The experiment demonstrated recoverability of
that run under the proposed narrow historical interpretation, rather than
certifying production changes.

## Recommended separate repair

1. Remove the later `dtplyr-label-helpers` occurrence from tracked DESCRIPTION.
   Keep the earlier occurrence and every other entry in place. Assert strict
   parsing succeeds, and compare native ordered discovery before/after for
   exact equality and one occurrence of every discovered file.
2. Keep current metadata parsing strict. Add an explicit historical-reader path
   that accepts only repeated literal stems and normalizes them by first
   occurrence, matching native testthat semantics. Preserve all existing
   age, input-byte/hash, dependency, worker and terminal-result checks. Do not
   rewrite the retained manifest or make the general reader silently forgiving.
   Report when historical duplicates were normalized. Add a fixture with a
   duplicate in the measured DESCRIPTION and a unique target; prove recovery,
   unchanged evidence and native modeled order. Retain rejection tests for
   nonliteral/empty entries and all other ineligible evidence.
3. At the start of the existing CI-invoked priority contract script, validate the
   real tracked DESCRIPTION using the strict reader. Add a duplicate-current
   fixture and a combined-priority merge fixture. This catches the combined
   state without another suite or another workflow. Reassess current main when
   integrating concurrent priority proposals, as the integration workflow and
   runner guide already require; do not concatenate independently valid lists.

Steps 1 and 3 restore metadata and prevent recurrence. Step 2 additionally
recovers eligible evidence already poisoned by the duplicate, without an extra
measurement-only suite. If historical tolerance is not adopted, state that
these old runs cannot generate proposals with the strict reader and defer to
the next ordinarily required full check. A new full run solely to measure
priority is not the approved fallback.

For implementation, use focused tool fixtures and the actual native discovery
comparison. Choose Review-ready scope from the resulting repair's effects:
runner/fixture changes are not this investigation's prose-only `source` case.
Keep #1185/#1186 repairs separate. An eligible retained run must still be
revalidated immediately before actual proposal adoption, and its original
seven-day deadline must not be renewed.

## Revisions (2026-11-03)

The maintainer requested that the improvement proposal also specify how an
implementing AI agent should respond if duplicates escape prevention and are
discovered during Review-ready. This addition supplements the repair proposal;
it does not change the findings above. The workflow below is a proposed
implementation requirement, not an already-installed runner behavior or an
amendment to the operational authorities.

### Detection and ownership

Extend prevention beyond CI: validate the submitted priority in both local
scopes before heavy admission, using the same strict reader. Report duplicate
stems, their positions, pinned SHA and whether the invalid input is submitted
metadata, archived measurement metadata or a proposed patch. The runner should
return a failing preflight result for invalid submitted metadata and should
never edit committed source or silently discard duplicate registrations.
Preserve all five mandatory baseline checks for valid submissions.

An agent encountering a literal duplicate in the submitted priority should
repair it within the active implementation task, even when inherited from main
and even when that implementation is otherwise finished. Opening another issue
does not discharge the repair. No repeated permission request is needed for
the mechanically established repair. Preserve the first occurrence and remove
later occurrences only after strict validation of the normalized list and an
exact comparison of native ordered discovery, including every discovered file
once. Perform that comparison on disposable copies while a check is active.

### Response by detection point

| Detection point | Required agent response |
| --- | --- |
| Before invocation or a failing metadata preflight | Collect any failed invocation's terminal result. Repair and commit the metadata, then invoke Review-ready for the new clean SHA using the scope required by the implementation. Without an applicable successful full result, a package-affecting implementation still requires `full`. |
| During admission waiting or active checking, with a confirmed duplicate in the submitted SHA | Follow *Required fix confirmed* in `design/agents/local-checks.md`: cancel through the original retained execution handle, collect terminal completion and owned child work, then repair and commit. Invoke a fresh check for the new SHA; a cancelled or incomplete full invocation supplies no full success or eligible measurement. Do not edit the submitting worktree while the original invocation remains active. |
| At finalization after a successful full result | Collect the original terminal outcome first. Preserve the full success for its measured SHA and separately report the proposal failure. Repair tracked metadata in the same task, recover/recalculate a proposal from eligible evidence, inspect its disposition, commit and validate the resulting SHA under the scope rule below. Do not claim final review readiness while tracked metadata remains invalid. |
| Only in archived metadata, with valid submitted metadata | Use the proposed historical reader, report normalization, and revalidate the complete retained evidence. Recalculate only the proposal and inspect its disposition. Historical duplication alone requires neither a metadata commit nor another suite. |
| In a generated patch or the combined state after integration | Reject the invalid patch or combined state before review readiness. Keep its evidence for diagnosis; repair the target list and recalculate against current main and still-eligible evidence. Verify the resulting SHA. Do not blindly apply a patch or accept a clean textual merge as proof of valid priority membership. |

An observed terminal outcome takes precedence over a planned cancellation. If
the invocation finished first, collect it and use the appropriate completed
case; never retroactively label a completed full result cancelled or passed
based on an intermediate transcript. Preserve actual failed checks as failures.

### Verification and bounded recovery

The repair should explicitly amend *Boundary and scope* in
`design/agents/local-checks.md` to cover pure duplicate removal after a
successful full result. Permit `source` for the repaired SHA only when:

- the only check-affecting change since that full SHA is the priority field;
- package code, test/helper bytes and membership, installed files, dependency
  declarations and dispatch machinery are unchanged;
- the evidence remains eligible under the ordinary age/environment checks;
- the change removes only later identical literal registrations, and strict
  parsing plus exact native ordered discovery prove unchanged execution; and
- any accompanying measured adoption satisfies the existing proposal inspection
  and adoption requirements.

Record both SHAs, the duplicate removal and ordered-discovery evidence, the
scope reason, baseline outcomes and heavy omission. This is a narrow proposed
extension to the measured-priority adoption rule, not blanket permission to
use `source` for DESCRIPTION changes. Until it is adopted in that authority,
apply the existing scope judgment and use `full` when effects are uncertain.
Production changes to the reader, runner or fixtures require their own
applicable verification; they cannot be hidden inside this metadata-only
exception.

If the recovery mechanism is unavailable or evidence has expired or changed,
still repair the tracked duplicate, report the proposal diagnostic and defer
optimization to the next ordinarily required successful full check. Do not
patch manifests, renew retention, weaken eligibility, or start a full suite
solely to obtain replacement timings. The scope required to validate an actual
repair remains a separate obligation.

If the list contains glob registrations, empty entries or another ambiguity,
or removing duplicates changes the native ordered file vector, stop automatic
deduplication. Retain the diagnostic, establish the intended dispatch from the
specification, and seek a maintainer decision only where that intent is
unresolved. Preserve the implementation and its valid check evidence while
reporting the concrete remaining blocker.

After a verified repair, the validation run's optional proposal does not start
another optimization cycle. A newly detected invalid tracked priority is a
real unresolved defect and still needs repair; it is not waived by the
no-second-adoption rule. Diagnose repeated recurrence, including intervening
integration, rather than blindly repeating the same patch/check cycle.

### Implementation homes and acceptance cases

Put the executable validation and diagnostics in the shared local preflight
and priority tooling. Put the agent recovery steps, evidence preservation and
proposal disposition in `tools/review-ready-runner.md`, and the narrow scope
exception in `design/agents/local-checks.md`. Keep CI validation of the actual
tracked DESCRIPTION. These authorities should own the installed behavior;
this dated note remains the proposal and its rationale.

Use disposable process/priority fixtures to cover invalid preflight in both
scopes, cancellation and child collection before resubmission, successful full
outcome preservation after proposal failure, historical-only normalization,
invalid proposal rejection, and a concurrent merge introducing a duplicate.
Verify exact native dispatch equivalence and the metadata-only scope boundary,
including rejection of that exception when test inputs or dispatch machinery
change. Assert no automatic source editing, no full-check retry by proposal
recovery, and no final-ready report with an unresolved tracked duplicate.
These are tooling acceptance cases, not a request for another measurement-only
package suite.
