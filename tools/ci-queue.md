# Routine CI lifetime

The policy is [CI queue efficiency](../design/specs/ci-queue-efficiency.md).
Automatic concurrency groups use workflow/ref identities. Manual dispatches
use a separate per-run namespace. The Pages deployment job retains its `pages`
serialization group; altdoc cancellation remains scoped to the build job.

Dispatch-enabled workflows use a new `auto-<ref>` namespace so new automatic
runs cannot cancel pre-change manual runs sharing the former workflow/ref
group. Pre-change automatic runs in those old groups finish naturally or are
retired by the close handler; no backlog cancellation is performed at rollout.
Workflows without manual dispatch retain their former automatic groups.

## Close-event eligibility

`retire-pr-ci.yaml` runs only default-branch control code, without checkout,
artifacts, caches, package installation or PR-controlled script interpolation.
It needs `actions: write` and `pull-requests: read`; routine test workflows
retain their read-only permissions. `actions/github-script` supplies the
authenticated API client without provisioning R and JSON dependencies in the
privileged handler. Its external API/time fixtures execute that JavaScript
with Node; R owns the fixture command and workflow assertions.

GitHub's [target-event policy][target-policy] can block this event. Read the
repository's applicable policies, including parents, before activation:

```sh
gh api 'repos/sayuks/marginplyr/actions/policies?has_parents=true'
```

When no applicable custom policy already permits this handler, create the
path-scoped policy from `tools/ci-close-event-policy.json`:

```sh
gh api --method POST repos/sayuks/marginplyr/actions/policies \
  --input tools/ci-close-event-policy.json
```

Do not widen a repository-wide event allowlist. Existing applicable policies
layer with the new policy and must also permit the handler. The workflow
becomes operational after it reaches the default branch; fixtures establish
its behavior before that point without cancelling live runs.

## API selection and limits

Only known routine workflows with `event: pull_request`, an active status, the
event's head repository/ref tuple, and creation no later than the close are
candidates. A unique run `pull_requests` association identifies the target PR.
When that array is empty, an all-state [PR lookup][pulls] must identify exactly
one PR with the same head repository/ref, and the run SHA must occur in the
target's API commit history. Ambiguous or missing
associations are reported as failures without cancelling those runs. This also
supports historical runs and fork runs without introducing PR artifacts or
trusting their run names. It does not promise identification when the API has
lost historical commits, a head repository is deleted, or one head branch has
been used for multiple PRs.

Each of five active statuses has at most three pages of 100 runs, filtered by
the head branch as a search optimization. More than 50 candidates fails without
cancellation. Requests have a ten-second timeout and no configured retries.
The fallback PR lookup reads at most 100 PRs and rejects a full page; commit
membership reads at most three pages of 100 commits and never infers membership
from an incomplete or missing result.
Before each cancellation, the live PR must still be closed at the event's
`closed_at`; a reopening or later close invalidates this event. The run's live
eligibility and attempt are rechecked too. A reopening immediately after the
last API read remains an ordinary event race, not an atomic guarantee.

Cancellation requests are observed together for at most twelve polls, five
seconds apart, inside a ten-minute job deadline. Natural completion is accepted;
an API rejection, changed attempt or unobserved completion fails visibly.
There is no force cancellation, periodic recovery or manual backlog cleanup.

Run `Rscript tools/test-ci-queue.R` for workflow lifetime, failure-continuation
and API behavior fixtures. `tools/verify-ci-artifacts.R` and
`tools/test-ci-artifacts.R` retain independent executable-contract checks.

[target-policy]: https://docs.github.com/en/actions/reference/security/securely-using-pull_request_target
[pulls]: https://docs.github.com/en/rest/pulls/pulls
