---
status: accepted
---

# Run Review-ready from the target worktree

For [#1114](https://github.com/sayuks/marginplyr/issues/1114), use a
repository-tracked script from each target worktree as the Review-ready entry
point. Git supplies that worktree's runner version; a separately installed
standalone command is no longer the intended delivery boundary. This avoids
synchronizing an installed copy, at the cost of old worktrees retaining their
own runner versions rather than automatically receiving a common replacement.

Use R for the entry point, supervision, and evidence issuance and validation.
Treat each clone's pinned local `main` as the accepted registration source for
limited workloads; adopting a
registration into `main` replaces installation as the acceptance event.
Changes to the trust core still require Full and dedicated contract/process
verification. Pre-migration worktrees must incorporate the required changes;
unlimited compatibility with old runner versions is not a goal.

Prefer base R and few external R dependencies. Admit a package only when its
role is a natural fit and its concrete benefits outweigh its dependency and
maintenance costs; researching a package does not select it for adoption.
Use `processx` for asynchronous process lifecycle, `ps` for process identity
and inspection, `filelock` for OS advisory locking, and `openssl` for hashing,
HMAC authentication, and secure key generation. These focused APIs avoid
reproducing process handles, identity checks, stale-lock management, and
cryptographic primitives. Declare and validate their development-tool setup,
including OpenSSL's system requirement and its `askpass`/`sys` dependency closure.

Cut over only after old waiting and running requests finish or their owners
cancel them and confirm cleanup. Do not require concurrent operation of the
old installed launcher and the new runner.

When the branch changes the trust core, use the pinned main manager as the
outer supervisor holding Full capacity and required verification. Exercise
the candidate runner through dedicated process fixtures under that boundary.
Otherwise use the worktree runner matching the accepted contract. Permit
status and explicit-request recovery from any compatible worktree, even after
the originating worktree has disappeared.

Guarantee recovery across manager/process termination with confirmed shutdown
and conservative refusal on uncertain evidence. Do not promise preservation
of the latest record across OS crashes or power loss; damaged or unavailable
records require inspection and recovery rather than a successful-check claim.

Retain completed local history and diagnostics for seven days. Perform automatic
pruning from normal check startup and completion with base R, without a
resident service or OS scheduler. Physical deletion may wait until the next
invocation. Collect check-owned by-products under a request directory;
temporary work is removed after confirmed shutdown. Apply the policy to
attributed old outputs too, while committed research data requires a separate
Git change. Old-history deletion failures are reported and retried without
failing a new check. Automatically recover orphaned requests only when complete
shutdown is established; unknown or damaged ownership remains protected.

Give rendering subprocesses a request-owned HOME and cache environment while
preserving the selected tools and R library paths. Do not depend on personal
home settings or credentials, or fall back to shared caches when isolation is
unsupported. Verify the supported renderer/cache configuration before rollout.
Leave OS-created crash reports to the OS. Keep empty shared lock targets
while their protocols operate and remove them only at complete retirement.

Shared admission, execution ownership, cancellation, recovery, and limited
workload eligibility remain obligations. Preserve the stage evidence reuse
implemented by [PR #1115](https://github.com/sayuks/marginplyr/pull/1115)
under ADR 0038, including actual-work capacity, environment identities,
authenticated observations, and failure invalidation. Its evidence store must
participate in bounded retention without resurrecting an older success.
Origin references do not extend the original execution's seven-day deadline.
Retire expired origins and dependent reuse coherently; retiring a negative
observation must also invalidate the older successes it still vetoes. Keep
unrelated valid evidence usable without permanent tombstones.

Rotate authentication keys after seven days from creation through the same
invocation-driven maintenance. Reauthenticate valid retained records without
changing their original identities, ordering, outcomes, or deadlines, then
retire unneeded old keys. Compact the inventory to retained evidence and
necessary operating/recovery state. Maintenance and issuance must not expose
partially updated trust state.

The complete contract and required implementation evidence are recorded in the
[repository runner specification](../specs/review-ready-repository-runner.md).
These decisions do not assert that the replacement has been implemented.
[Local checks](../agents/local-checks.md) and
[shared admission](../../tools/review-ready-admission.md) remain the operational
authority until implementation and migration establish the replacement.
