# CI queue efficiency

Status: design agreed on 2026-11-09; implementation proposed. Inspection baseline:
`2cd91ea24726115d1506e0dc42ae77d3ad0c496c`.

## Agreed constraints

- Reduce avoidable queue waiting. No numerical latency target was selected.
- A long individual run is acceptable when its verification is necessary.
- Preserve verification quality. Redundant checks may be removed, combined,
  or corrected when their guarantees remain intact.
- Retain the existing PR verification scope and detection point. Queue
  reduction alone does not justify moving PR checks to main or scheduled runs.
- Use the existing system without additional budget. Avoid excessive
  operational machinery.
- This session covers design and documentation. Implementation and CI run-state
  changes are separate work.
- Preserve independent checks after a failure for a still-current revision.
  A subsequent update still supersedes the failed revision's remaining work.
- Do not require before/after latency measurements or an effectiveness study.
  Verify implementation correctness and preservation of existing checks.

## Agreed lifetime of routine automatic runs

Keep the newest revision for each open PR and the newest revision of main.
A newer update supersedes the older automatic run for that same PR or branch;
it does not supersede another open PR's newest run. Close or merge retires
the corresponding PR's automatic runs, including queued work. Manual release
verification remains outside this routine cancellation policy.

## Initial change set

- Apply consistent latest-revision cancellation to routine automatic workflows,
  including PostgreSQL transactions and the altdoc build. Keep manual release
  groups distinct from automatic groups.
- Retire a closed PR's routine automatic work through one small, trusted,
  API-only close-event workflow. Keep the target limited to that PR and the
  known routine workflows. Resolve GitHub event-policy eligibility and reliable
  API association before implementation is considered complete.
- Combine the four base-R repository-hygiene jobs, preserving every
  check and its individually identifiable outcome, including checks after an
  earlier failure. Keep the existing package-aware lint job separate.
- Retain the heavy verification contracts and their PR triggers. Defer replacing
  simulated suite coverage with aggregated backend results: the existing
  backend artifacts do not yet establish equivalent per-test coverage.

Use native GitHub Actions mechanisms and a bounded event handler. Do not add
a custom scheduler, periodic reconciler, persistent monitoring, or a new
performance-measurement service. Use ordinary event-driven retirement without
adding recovery infrastructure for missing events or arbitrary manual reruns.

## Implementation boundaries

Use workflow-specific group identities. PR updates share a stable PR identity;
main updates have a separate branch identity. Manual release verification has
a separate namespace, rather than relying on its own cancellation flag to
protect it from an automatic run. Retain the separate Pages deployment
serialization; verification consolidation does not redesign deployment.

The close handler runs only trusted default-branch control code. It executes
no PR code and consumes no PR artifacts or caches. Give cancellation permission
only to that handler and limit any necessary Actions event-policy allowance to
the handler. Ordinary PR test workflows retain their existing trust boundary.

Require positive run-to-PR identification from GitHub API evidence. A branch
name or PR-controlled run-name alone is insufficient. Exclude main pushes,
manual runs, unrelated PRs, completed runs, and the handler itself. Recheck live
PR state and handle reopening and natural completion without widening the
selection. Bound pagination/retry work and report an inability to identify or
stop a target; do not turn uncertainty into repository-wide cancellation.

Preserve every lightweight check when combining jobs. Keep distinct step
outcomes, run the remaining checks after an earlier check fails when setup
succeeded, and fail the combined job if any check failed. Keep this base-R job
independent of the package dependency installation and the Mac lint job.

Reconcile the independently maintained executable workflow contracts and
affected command fixtures with the intended change. Do not weaken those gates
or package verification to make the new workflow structure pass.

## Correctness verification

Correctness verification covers same-PR updates, different PRs, main updates,
close/merge, conflicts, forks, reopening, manual release isolation, existing
run migration, cancellation completion, and preservation of the existing checks.
These are functional acceptance cases, not a queue-latency benchmark.

## Evidence boundary

Job count alone does not establish duplication. The provisioned development
matrix, source-tarball matrix, actual dependency isolation, simulated backend
coverage, generated-document comparison, and rendered-site checks answer
different questions. Each proposed consolidation needs an explicit account of
the guarantees it preserves.

The ordinary PR event does not execute workflows for conflicting PRs. A
`pull_request: closed` sentinel alone therefore does not cover the agreed close
policy. A trusted `pull_request_target: closed` handler is the selected design;
its availability must be checked against the applicable event policy.
([GitHub events](https://docs.github.com/en/actions/reference/workflows-and-actions/events-that-trigger-workflows),
[target-event policy](https://docs.github.com/en/actions/reference/security/securely-using-pull_request_target))

Native concurrency does not guarantee dispatch-time ordering. The lifetime
policy describes the useful revisions and normal event handling, without
claiming an atomic, continuously reconciled latest-SHA invariant.
([GitHub concurrency](https://docs.github.com/en/actions/how-tos/write-workflows/choose-when-workflows-run/control-workflow-concurrency))
