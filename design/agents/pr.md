# Pull-request preparation: `pr`

Use `pr` to write the body and `mattpocock-skills:code-review` to conduct the
separate review. Keep the skill's `Summary`, `Evidence`, and `Merge Danger`
sections. This guide maps their contents to repository-owned decisions.

Apply the NEWS policy below when preparing or reviewing a change.

## Sources to apply

| Subject | Authority and application |
|---|---|
| Vocabulary | [Domain docs](domain.md) maps the skill's `GLOSSARY.md` to [`CONTEXT.md`](../../CONTEXT.md). Use that vocabulary without creating another glossary. |
| Verification | [Local checks: Boundary and scope](local-checks.md#boundary-and-scope) owns the repository entry, clean committed worktree, scope judgment and revision freshness. [Recording the outcome](local-checks.md#recording-the-outcome) owns the verification record. Before/After evidence supplements that record. |
| Runner operation | [Setup and invocation](../../tools/review-ready-runner.md#setup-and-invocation) owns the `--scope`/`--reason` interface, checkout updates and retired-CLI policy. |
| Review | [Code review](code-review.md) owns the Standards/Spec axes, comparison against the merge-base with `main`, and pushing the branch and opening the PR before publishing its review record. Apply [Answering a review](../architecture.md#answering-a-review) when evaluating findings. |
| Published history | [What is reviewed, and where the record goes](code-review.md#what-is-reviewed-and-where-the-record-goes) owns ordinary follow-up commits and, for an unavoidable exceptional rewrite, invalidation and fresh review records. |

## NEWS and contributor credit

Add a bullet to the current version section of `NEWS.md` for every user-visible
change: new features, bug fixes and substantial documentation improvements.
This applies to maintainer changes and external contributions alike. Internal
refactoring and minor documentation edits may omit a bullet.

Assess the effect on someone using the documented public interface, rather
than the files touched. A change needs a bullet when it changes results,
types, names or Margin order; accepted inputs or options; actionable
diagnostics; backend support; execution timing or queries; or meaningful
performance characteristics. A new vignette or a substantial new usage guide
also merits a bullet. Pure refactoring, tests, CI maintenance and typo fixes
may omit one when they change none of those user-facing effects. For a
borderline change, identify the public behavior or guidance that differs.

These examples make the [Tidyverse NEWS criterion][news-guide] concrete for
this repository; they are not an exhaustive checklist from that guide. The
guide recommends highlighting sweeping documentation changes and new
vignettes; this repository requires substantial documentation improvements
to be recorded.

Complete and commit the required NEWS entry before invoking Review-ready.
If contributor credit needs a PR number that is not yet available, open a
draft PR first, then add the number, commit, run Review-ready and mark the PR
ready. Later NEWS edits follow [revision freshness and scope][local-scope];
a prior result certifies only its submitted SHA.

NEWS necessity, accuracy and contributor credit are checked during preparation
and review. No additional NEWS CI gate is required: whether the change affects
users needs semantic judgment, and a NEWS file diff alone cannot establish it.

Describe the resulting behavior from the user's perspective. For a PR whose
author is not listed in `DESCRIPTION`'s `Authors@R`, end the bullet with
`(@handle, #PR)` before the final period. Use the author's GitHub handle and
the actual PR number. Authors already listed in `Authors@R` may omit their
handle; keep the related issue or PR number. Additional credit for reporters
or reviewers is optional and may be included for a clear contribution.

For example, with illustrative placeholders:

```markdown
* `example()` now preserves missing values (@contributor, #1234).
```

This follows the [Tidyverse NEWS guide][news-guide].

## Body and comments

Put the smallest suitable diagram, diff sketch or tree in `Summary`. Put
concrete Before/After evidence in `Evidence`, followed by links to two PR
comments: the verification record and the review record. Keep Door and Blast
Radius in `Merge Danger`.

Name the revision in each record. Update the body links when a new record is
published, keeping the old record's revision explicit; a body edit gives no
later SHA the coverage of an earlier check or review. Detailed records belong
on the PR. An implementation issue receives a pointer to the PR rather than a
second review record, as [Code review](code-review.md) requires.

The following arrangement is illustrative only. Angle-bracket fields are
placeholders, and none of the example describes completed checks or reviews.
Replace every field with observed evidence, or say that it is pending. Use
actual PR-comment permalinks in the published body.

### Illustrative body

````markdown
## Summary

```diff
 AGENTS.md
+  -> design/agents/pr.md -> domain, verification and review authorities
```

Closes #<issue>.

## Evidence

- **Before:** <observed gap or failing command, with revision>
  **After:** <observed change or passing command, with revision>
- Verification: <permalink to verification comment for submitted SHA>
- Review: <permalink to review comment for reviewed SHA>

## Merge Danger

**Door:** two-way

<Why this example can be reverted. Choose one-way when applicable.>

**Blast Radius:** documentation

<Affected workflows or readers. Use the actual change's scope.>
````

### Illustrative verification comment

Fill this from the retained invocation's terminal result under
[Recording the outcome](local-checks.md#recording-the-outcome).

```text
Review-ready
Submitted SHA: <40-character SHA>
Scope: <source or full>
Reason: <concrete reason for this scope from the diff and its effects>
Test dependency-declaration scan: <outcome>
Required optional test Suggests presence/version: <outcome>
Roxygen regeneration and committed-source consistency: <outcome>
Package spelling: <outcome>
jarl check .: <outcome>
Package-aware lintr after pkgload::load_all(): <outcome>
Heavy verification: <use the applicable branch below>
Terminal outcome: <exit status or timeout/cancellation outcome>
Execution handle: <retained invocation identity>
Artifact directory: <worktree-root>/.review-ready/runs/<run-id>/
NOTE dispositions: <each NOTE and its disposition, or none>
```

For `full`, fill Heavy verification with the source-tarball build/check outcome
and its ERROR/WARNING/NOTE counts, including the complete suite and snapshots.
For `source`, fill it with the exact omission:
`package build/check, complete suite and snapshots omitted`.

### Illustrative review comment

Use [Code review](code-review.md) for the record and its dispositions. Keep the
two axes visible, including when both have zero findings:

```text
Reviewed SHA: <40-character pushed SHA>
Comparison: git diff <main-ref>...<reviewed-SHA> (merge-base with main)
Standards: <findings and disposition/evidence for each, or zero findings>
Spec: <originating issue/spec; findings and dispositions, or zero findings>
```

If no review ran, replace that example with an explicit statement:
`No code review ran for <SHA>.` A verification pass is not a review record.

[news-guide]: https://style.tidyverse.org/news.html#in-development
[local-scope]: local-checks.md#boundary-and-scope
