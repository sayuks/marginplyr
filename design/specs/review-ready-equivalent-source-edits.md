# Review-ready scope for equivalent source edits

Status: accepted on 2026-11-05 for
[#1292](https://github.com/sayuks/marginplyr/issues/1292); implemented in the
repository runner with operational guidance adopted together.
[ADR 0041](../adr/0041-check-generated-documentation-before-omitting-heavy-checks.md)
records the decision. [Local checks](../agents/local-checks.md) remains the
operational authority. Per-SHA verification remains separate from adoption.

## Rule

Both scopes always regenerate roxygen documentation and require its output to
match the committed submission. Source scope may omit heavy verification for
explanatory text or formatting changes whose execution and verification effects
are established as unchanged. Other package-affecting changes, and uncertain
effects, require full scope.

The AI chooses scope from the complete applicable diff and records its evidence.
The runner does not automatically classify edits. Each submitted SHA receives a
fresh invocation; a source result explicitly records that package build/check,
the complete suite and snapshots were omitted.

## Mandatory checks

Run the existing dependency-declaration scan and required optional test Suggests
presence/version check, then the new documentation consistency check, followed
by package spelling, `jarl check .`, and package-aware lintr after
`pkgload::load_all()`. All six are mandatory for both scopes before heavy-slot
admission. Source scope ends after they pass; full scope then follows the
existing admission and source-tarball workflow.

Documentation consistency runs `roxygen2::roxygenise()` in the disposable
pinned source copy. Compare generated filenames and contents with the submitted
revision, including additions and deletions in `man/` and changes to
`NAMESPACE`. Detect other source mutations, including DESCRIPTION updates, so
later checks cannot silently use regenerated content different from the pinned
submission. A generation failure or mismatch ends the invocation before heavy
admission. Report the difference; correction and a new commit precede a fresh
invocation. The check does not repair the submitting worktree.

Declare roxygen2 in `Config/Needs/review`. Retain the existing runner interface,
pinned-SHA evidence, terminal-handle ownership, artifact retention and CI
responsibilities. Regeneration and consistency alone do not validate Rd content
or execute examples.

## Eligible edits and evidence

Ordinary explanatory comments, whitespace or line-break formatting, and
prose-only roxygen edits are candidates. Every changed R file must have identical
parsed expressions before and after, with source references discarded:

```r
before <- parse(file = baseline_file, keep.source = FALSE)
after <- parse(file = submitted_file, keep.source = FALSE)
stopifnot(identical(before, after))
```

Read both inputs from the selected immutable revisions, not from an uncommitted
working tree. Failure to parse or unequal expressions requires full scope.
Expression-rewriting refactors and lint or coverage directives are outside this
exception even if the intended behavior is equivalent. Additions, deletions and
renames are outside it; the exception covers edits within existing files.

For roxygen edits, inspect the committed generated diff as well. No generated
change, or a change confined to explanatory prose or formatting, may qualify.
Changes to NAMESPACE, usage, executable examples or other behavior/structural
metadata require full scope. Successful regeneration does not make them eligible.

Parsed-expression equality is necessary but insufficient. The AI also establishes
that consumers of the changed text do not change their behavior: source scans,
generation, snapshots and source-location-sensitive output can distinguish text
whose parsed expressions agree. Inspect the relevant consumers and use a focused
check when it is needed to settle an effect. Inconclusive evidence selects full
scope; the six mandatory checks still run at either scope.

## Comparison boundary and record

Use the latest verifiable successful full-checked ancestor of the submitted SHA
as the comparison baseline when one is available. Otherwise use the merge-base
with `main`. Assess every change from that baseline through the submitted SHA,
not just the final commit. An earlier full result establishes only its own SHA,
never a full result for the submission. No earlier successful full is required.

Record the baseline and submitted SHAs, the basis for the baseline, changed
files, parsed-expression results, generated-documentation diff assessment and
source-text consumer findings with the scope reason. Account for the whole diff:
a final comment-only commit cannot hide earlier unverified execution changes.
Other edits in the submission still need their own scope justification under
local checks; this exception does not waive unrelated full-check obligations.

Test-source edits also remain subject to existing timing-evidence eligibility.
Permission to omit a heavy check does not make byte-different tests eligible for
an earlier timing proposal.

## Implementation acceptance

Extend the existing runner process fixtures to establish that both scopes run
the documentation check, a consistent source request avoids heavy admission,
and stale, added or deleted generated output and generation failures stop before
admission. Verify that the submitting worktree stays unchanged and that no
changed generated content reaches later checks as if it were committed.

Update the runner contract, review-tool dependencies, prerequisite fixtures and
six-check reporting together. Reconcile ADR 0039, the original runner
specification, local checks and the runner guide at adoption. Keep the existing
CI regeneration check and its installation/renderer environment. Package-aware
lint and the required full verification of runner changes remain obligations of
implementation, not outcomes established by this design-only change.

## Measurements

The [parse measurement](../../investigation/review-ready-parse-comparison-2026-11-05.md)
records 0.3075 seconds median for reading, parsing both sides and comparing all
293 R/test source files on the measured host. Including a new R process took
0.413 seconds median. The
[roxygen measurement](../../investigation/review-ready-roxygen-generation-2026-11-05.md)
records 1.440 to 2.145 seconds including R startup, with median 2.100 seconds.
Those measurements support mandatory mechanical evidence rather than relying
on AI recollection of execution or manual generation.
