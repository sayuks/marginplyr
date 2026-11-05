# Main compatibility and conflict resolution

## Check the current base

Before invoking either Review-ready scope, before opening a pull request and
again before merging it, fetch remote `main` and pin the revision assessed
together with the branch HEAD. This is agent preparation; the runner's six
checks do not perform it:

```sh
git fetch origin main
base_sha=$(git rev-parse FETCH_HEAD)
branch_sha=$(git rev-parse HEAD)
git merge-tree --write-tree "$base_sha" "$branch_sha"
```

Stop if fetching or resolving either revision fails; a stale local `main`
does not establish compatibility. `merge-tree` examines the prospective merge
without changing the index or working tree. Inspect its output and exit status:
conflicts need resolution, and an execution error leaves the assessment pending.
A clean tree merge alone does not establish semantic compatibility. Inspect
both sides' changes since their merge-base, including adjacent contracts,
specifications, issues, commits and affected tests. Determine whether their
combined behavior requires adaptation or focused verification.

Record the fetched base SHA, branch HEAD, conflict and semantic assessment,
evidence, and integration decision in the work record. Keep the preparation
outside the submitting worktree; publish it on the PR when available. Fetch
again and reassess if the base advances before integration. A later branch HEAD
also needs a new assessment; an earlier pair of SHAs does not cover it.
Before PR publication or merge, fetch and compare the base/HEAD pair with the
recorded assessment. Reuse its conflict and semantic judgment when both SHAs
are unchanged; reassess when either changes.
Complete required integration and commit its result before submitting that
revision to Review-ready. If only main advances and the branch remains
compatible without modification, reassess the new pair; the unchanged branch
SHA retains its own applicable check evidence.

## Decide whether to integrate

Being behind `main` alone requires no merge or rebase. If the changes are
compatible and neither integration nor repository up-to-date requirements
demand it, record that conclusion and proceed with the branch unchanged.
Integrate when needed to resolve conflicts, adapt interacting changes, or
satisfy an actual repository requirement; state which need applies.

Choose the integration method under
[the published-history policy](code-review.md#what-is-reviewed-and-where-the-record-goes).
After a review snapshot or record is published, use ordinary follow-up commits,
including a merge commit where needed. An unavoidable exceptional rewrite
follows that policy's invalidation and fresh-review procedure.

## Resolve intent

For each conflict, establish what each side intends from the relevant
specifications, issues, commits and tests. Resolve autonomously when those
sources establish the intended result, preserving both sides' compatible
intent. For example, adapt an added validation to a renamed function when the
contract and rename are established. Record what was retained, adapted or
omitted and why, with the evidence that supports each decision.

When the branches demand incompatible behavior, or the evidence cannot
establish the intended result, present the alternatives and consequences and
obtain the maintainer's decision before choosing behavior. Deleting versus
retaining data, or preserving versus removing an old API, are specification
choices when neither result is already established. Keep that resolution
pending while doing independent work. A choice of `ours` or `theirs`, clean
conflict markers, or passing checks cannot decide the specification.

## Verify and review the resulting revision

Inspect the resolution diff against both parents (or the corresponding
pre-integration revisions) and inspect affected behavior. Account for both
sides' intent even where Git merged text automatically. Commit the resolved
result, then apply [Local checks](local-checks.md) to that revision: its scope,
clean-commit boundary, retained execution handle, bounded investigation,
outcome record and SHA freshness remain authoritative. A pre-integration pass
does not certify the later HEAD.

Review the resolution under [Code review](code-review.md), including
[Answering a review](../architecture.md#answering-a-review). If a conflict is
found after a review record was published, preserve that history, add the
resolution in follow-up commits, and record the resulting SHA and review of
the new resolution diff. Keep the earlier record's revision explicit. Apply
the review policy's second-run boundary rather than repeating settled review
of unchanged behavior.

Before merging, repeat the current-base assessment above and ensure the
resulting revision has the applicable verification and review evidence.
Merging into `main` still requires normal maintainer approval and required CI;
authorization to resolve a conflict does not bypass either gate.
