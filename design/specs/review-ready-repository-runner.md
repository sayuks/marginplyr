# Repository-owned Review-ready runner

Design for [#1114](https://github.com/sayuks/marginplyr/issues/1114), under
[ADR 0039](../adr/0039-run-review-ready-from-the-target-worktree.md).
Status: accepted through 2026-11-01, including the merged evidence-reuse
integration. The policy interview is complete. This is a design, not an
implemented replacement or authorization to delete existing data.

The approved direction replaces the issue's proposed automatic refresh of an
installed CLI with repository-owned delivery, eliminating that copy's
synchronization requirement. [Local checks](../agents/local-checks.md) and
[shared admission](../../tools/review-ready-admission.md) remain the operational
authority until implementation and the accepted cutover establish the new
entry point.

The implementation baseline includes [PR #1115](https://github.com/sayuks/marginplyr/pull/1115),
merged at `be9e81e9c3d977b8a2607f3971468b5acf64faf1`. Stage evidence reuse
under [ADR 0038](../adr/0038-reuse-unaffected-review-ready-stage-evidence.md)
is implemented at that baseline. Preserving it is part of replacing the
runner; it is not an unimplemented future capability.

## Accepted direction

Each target worktree uses its own repository-tracked entry script and runner
version. Updating that worktree through Git updates those sources without a
separate installation step. A common installed executable and an authoritative
checkout supplying the latest runner to every branch are not required.

Use R for the entry point, supervision, and evidence issuance and validation.
This includes replacing the supervisor's Python invocation from the R gate;
an R wrapper around the Python supervisor or evidence implementation does not
fulfill this direction.

Prefer base R and keep external R dependencies few. Select an external
package only when it naturally owns the required task and offers a clear,
substantial benefit over the base-R alternative. Assess the concrete APIs,
transitive dependencies, supported platforms, and maintenance burden; existing
installation or transitive availability alone is not an adoption reason.
No package is selected merely because it appeared in the tooling investigation.

Use `processx` for asynchronous process handles, output collection, exit status,
signaling, and cleanup primitives. Use `ps` for PID/start-time identity,
process ownership inspection, and recovery observations. These tasks justify
the two dependencies; they do not replace the request and recovery protocol.
Use `filelock` for OS advisory locking, retaining its lock object for the
manager's lifetime and explicitly releasing it after confirmed cleanup.
An available OS lock is not evidence that a previous request's children
stopped. Use the request records and process observations before granting
capacity. Do not replace this protocol with stale-directory deletion.
Use `openssl` for SHA-256, HMAC-SHA256, and cryptographically secure key bytes.
These existing cryptographic APIs justify the additional focused dependency.
Declare and validate its `askpass`/`sys` dependency closure and OpenSSL system
requirement as development-tool setup, without adding package runtime Imports.
Use an explicit versioned byte encoding and authentication-verification
contract with fixtures; adopting the package alone does not establish those
protocol properties. Keep keys out of external command arguments and logs.
Use base R for ordinary synchronous orchestration. Do not introduce direct
`callr` API use or a general worker/pipeline framework for this runner.

Worktrees predating this runner must incorporate the required changes before
using it. Compatible older runner versions may cooperate; unsupported versions
must stop before execution and explain the required update. Supporting every
historical version is not required.

Before cutover, old waiting and running requests must finish or their owners
must cancel them and confirm owned-process and workspace cleanup. Do not
silently cancel another owner's work. Retire the old launch entry points and
refresh participating sessions before allowing new requests under the new
protocol. Concurrent old-installed and new-runner operation is not required;
the migration must establish that the old generation cannot continue
launching checks through a supported entry point.

Use each clone's local `main`, pinned at invocation acceptance, as the accepted
registration authority for limited workloads. Adopting a registration into
`main` is the acceptance event, replacing installation. Candidate-branch
changes to selection, admission, trusted registration, or other trust-core
machinery require Full plus dedicated contract/process verification. An
implementation must not turn candidate-source hashes into their own limited
execution approval.

Read the accepted registration and classification code from pinned main Git
objects. For ordinary requests, use the worktree runner whose trust core agrees
with that accepted contract. If the candidate changes the trust core, pinned
main's accepted manager owns the outer Full slot, required verification plan,
cancellation, and cleanup. Test the candidate runner through dedicated process
fixtures inside that boundary. Passing only a `Full` value to the candidate
manager is insufficient: the accepted outer manager must enforce it. If an
accepted manager or required verification route is unavailable or incompatible,
stop with an update diagnostic rather than accept an unverified replacement.

Any compatible worktree may inspect the user's shared requests and recover
one explicitly selected request without the originating worktree being present.
Status and recovery do not require a clean submitted HEAD or launch a check.
Keep unknown record versions visible; refuse recovery mutations and identify
the required compatible runner. Do not execute code from a disappeared source
or guess that its processes have stopped.

## Invocation and immutable execution

The planned public entry is an R script invoked from the target repository
root. The implementation selects its tracked filename; `ENTRY_SCRIPT` below
stands for that path. Its modes preserve the existing check/status/recovery
distinction:

```sh
Rscript ENTRY_SCRIPT
Rscript ENTRY_SCRIPT --status
Rscript ENTRY_SCRIPT --recover REQUEST
```

Git updates the tracked runner without an installer refresh or Git hook. The
entry performs no automatic fetch and exposes no caller-selected check/skip
path. Invoking a check requires the resulting clean committed HEAD. A
detached or intentionally older commit is eligible when its runner, gate, and
registration protocols are supported. Unsupported revisions require the
necessary update or backport rather than a silent change of target.

At acceptance, pin submitted HEAD, local main, the accepted registration, and
the selected runner/manager source generation. Pin the unique merge-base when
the comparison establishes one; otherwise retain the unavailable comparison
and its reason. Limited classification requires the registered comparison
evidence. Comparison errors, ambiguous or absent bases, empty differences,
and unsupported changes retain the existing conservative Full selection and
required artifact routes. Full does not supply a missing comparison base or
resolve an artifact assertion that cannot be established.
Materialize that runner/manager's source and registered helper-source inputs;
later Git updates must not replace them. Revalidate submitted HEAD and
cleanliness before admission, cancelling a stale request. A later main update
does not replace the pinned authority or comparison base.

If the submitted source, accepted manager/registration, or objects required to
obtain them cannot be obtained, stop with the missing identity and an
update/setup diagnostic. Missing local main prevents obtaining this design's
accepted authority and therefore stops setup; this explicitly replaces the
installed manager's ability to select Full without local main. A comparison
failure under an available accepted manager follows the conservative selection
and unresolved-artifact rules above. Do not silently
choose another checkout, revision, authority, or unrestricted fallback. A
missing checkout cannot turn an incomplete invocation into success; shared
ownership state remains available for compatible-worktree recovery.

Retain the existing per-user, cross-clone Full capacity and four internal test
workers. Derive capacity from the work actually executed: a cumulative Full
obligation whose package unit is validly reused may use independent supervision
when all remaining work has an accepted limited-workload contract. Eligible
Documentation and Repository work also remains independently supervised.
Preserve the cumulative comparison and mapped verification, the same execution
handle, cancellation propagation, ownership observations, cleanup, and terminal
result. The same-agent bounded investigation loop remains part of waiting/checking.

R waiting may load the accepted supervisor components, but must not start
package dependency checks, marginplyr loading, lint, build, rendering, or tests.
Measure that waiting overhead when validating the R implementation. Keep the
existing macOS/Linux and local-filesystem scope; Windows supervision is not
added by adopting cross-platform R packages. Resolve common state from the OS
account identity, independently of environment HOME/cache overrides. Record
the supervisor packages as direct development-tool dependencies rather than
relying on incidental transitive installation or adding runtime Imports.

## Existing evidence contract to preserve

Inherit the behavioral contract at PR #1115's pinned merge above:
[ADR 0038](../adr/0038-reuse-unaffected-review-ready-stage-evidence.md), the
[verification follow-up specification](review-ready-verification-followups.md),
the implemented [stage-reuse contract](../agents/local-checks.md#reusing-unaffected-stages),
and [shared admission](../../tools/review-ready-admission.md).
This specification owns the amendments for repository delivery, R supervision
and evidence machinery, main-based registration authority, bounded retention
and key renewal, private renderer storage, and their rollout. Other behavior
and conservative refusals remain obligations. Python representations and
installed-entry mechanics may change under those amendments; their replacement
must preserve the decisions, actual work, provenance, and lifecycle guarantees.

Keep cumulative Full/Documentation/Repository obligations distinct from the
executed, reused, and omitted units. Retain original executing origins across
repeated reuse and compatible clones; inherited evidence is not a new Full
execution. Common source checks, dependency checks required by the cumulative
path, and affected-artifact contracts still execute at the submitted SHA.

Carry the complete registered source, tool, dependency, environment, and
clock-dependent identities into the R implementation. Missing evidence or an
unknown identity schedules execution of the required work; assertion failure
does not become a successful Full fallback. Revalidate before gate start and
at terminal issuance. Invalidation before validation can strengthen the same
request's plan; invalidation after validation starts fails that request.

Preserve the actual child-environment policy for known-format Codex and Claude
execution metadata. Apply it consistently to the identity probe, documentation
proof, gate, and descendants, while retaining unknown values, configuration,
and supervisor transport. Evidence must describe that effective environment.
The renderer's private HOME/cache and ordered R libraries must be accounted for
too. Normalize request-specific paths only through registered rules establishing
their irrelevance; do not omit HOME or cache configuration merely to obtain a
match. Otherwise execute the affected unit. Identity probes do not authorize
running verification stages before admission; include their cost in startup
and waiting-overhead validation.

Preserve authenticated terminal evidence, pre-cleanup stage observations,
complete issuance ordering independent of wall clocks, and the veto from later
failed, unavailable, or unobserved executions. A dead manager or an available
OS lock cannot remove that veto. Missing or corrupt evidence must never make
an older successful origin eligible again.

The seven-day policy must cover these added artifacts as well as logs. The
baseline store has no collector: its index requires every indexed evidence
and observation file and the associated request record to remain available.
Deleting individual files with `unlink()` alone would break that store's
completeness checks. The replacement therefore needs a coordinated evidence
retirement protocol, not only file deletion. The accepted evidence retirement
and key lifecycle below govern that replacement store.

## Failure model and persisted state

Support manager/process termination, including abrupt termination between
spawning a child and registering that child, without treating a released OS
lock or dead manager as shutdown evidence. Persist the request's identity and
recovery metadata before spawning work. Validate state and process ownership
before changing capacity or marking a request terminal. Persistence failures
before spawning prevent execution; failures after work starts preserve an
uncertain request and protect affected capacity until shutdown is established.

Write replacement records completely before publishing them, and detect
incomplete, missing, corrupt, or incompatible records. Establish the guarantee
with interrupted-write, manager-death, and recovery process fixtures; replacing
the current sync calls with an assumed equivalent is insufficient.

OS crashes and power loss do not carry a guarantee that the latest record is
preserved. On restart, refuse unsafe admission or recovery when state or boot
evidence is unavailable or damaged, and require inspection and restoration.
Lost or interrupted execution never becomes successful check evidence.

## Retention and automatic pruning

Retain terminal request history, combined logs, and failed/retried attempt
diagnostics for seven days after terminal completion with confirmed shutdown
and temporary-work cleanup. Apply one period to successful, failed, and
cancelled requests. Remove temporary copies, installations, render outputs,
fixture/backend data, and isolated caches after confirmed shutdown rather than
retaining them for seven days.

Run automatic pruning at normal check startup and completion using base R file
discovery and `unlink()`. Do not add a resident service or OS scheduler. When
the runner is unused, physical deletion waits until a later invocation.

If an older, safely completed request's history cannot be removed, report the
failure and retry at a later invocation without failing a new check solely for
that failure. This does not weaken current-request shutdown and temporary-work
cleanup: their failure still prevents successful completion and protects
affected capacity.

Inventory every Review-ready-created by-product before selecting the full
pruning scope, including outputs outside the common request directory. The
scope includes logs, diagnostic and retry bundles, request metadata, source
copies and archives, check trees, generated documentation and figures,
temporary installations and libraries, fixture repositories and data, progress
and transport files, sockets, and tool caches written by a check. Select
cleanup by the owned directory and recorded provenance rather than by log
extensions alone. Permanent historical storage is not required.

Identify execution-control state separately: nonterminal requests and
uncertain cleanup cannot be forgotten by age alone. The supported handling of
stale or damaged control state must be specified before claiming that all
such files can expire. List shared tool caches, OS-created reports, and
historical artifacts copied into the repository too; do not assume exclusive
ownership or authorize a broad deletion merely from their names or location.

At normal check startup, automatically recover a request whose manager is gone
only after the same complete ownership and shutdown observations required by
explicit recovery establish that all owned processing stopped. Record an
interrupted outcome and confirmed cleanup before applying retention. Do not
signal another owner's live work as a side effect of pruning. Leave unconfirmed,
corrupt, or unknown-version records unchanged and explain the required action.

Apply the policy to attributed old request history, diagnostics, and temporary
outputs at their existing locations too. Establish provenance individually for
manual captures. Runtime pruning does not modify Git-tracked historical data;
any removal of committed research outputs is a separate reviewed Git change
that accounts for citations.

The [file inventory](../../investigation/review-ready-file-inventory-2026-11-01.md)
records the producing functions, fixed names and variable directory families,
baseline cleanup, observed storage, and candidate deletion boundaries. Its
[evidence-storage extension](../../investigation/review-ready-evidence-retention-2026-11-01.md)
adds the artifacts and retirement constraints established after PR #1115.

### Evidence retirement and key lifecycle

The seven-day deadline for reusable evidence belongs to the original executing
request's terminal completion with confirmed shutdown and temporary-work
cleanup. Reuse, references from newer requests, index updates, and authentication
renewal do not extend it. Expired or missing origins make dependent reuse
unavailable; execute the required units when planning. A dependent request's
own raw outcome and logs retain their own terminal-history deadline without
becoming a new executing origin.

When retiring a failed, unavailable, or unobserved execution observation,
retire the earlier successful units it still vetoes in the same transaction.
Account for observation order rather than cleanup completion order, including
unknown source/environment observations that veto broader sets of origins.
Keep unrelated valid evidence usable. Do not accumulate permanent tombstones
or restore eligibility merely because a later failure has expired.

Serialize retirement, inventory updates, authentication renewal, and issuance
through the evidence-store protocol. Validation must not observe an incomplete
transition, including after manager termination. Revalidate referenced origins
at gate start and terminal publication. Expiration before validation can
strengthen the same request's execution plan; loss of required eligibility
after validation starts fails the invocation. An active operation may delay
physical collection needed for safe completion or recovery, but does not
extend an origin's eligibility deadline.

Rotate evidence authentication keys after seven days from creation. Perform
that work at normal startup/completion; when unused, physical rotation waits
until the next normal invocation. Verify and reauthenticate still-retained
evidence without changing its original request/SHA, execution and completion
times, observation order, outcome, executed/reused distinction, or expiration.
Authentication renewal is not a new execution. Retire old keys after confirming
that no protected operation or recovery state needs them. Key rotation alone
must not invalidate still-valid origins.

Compact the authenticated inventory to retained evidence and necessary active
or recovery control state. An index is not an exemption for accumulated
history. Preserve complete provenance and ordering across compaction without
retaining an unbounded retired-record list. Interrupted or failed maintenance
must not expose negative-observation removal without the corresponding origin
invalidation. Existing rules for unavailable reuse, protected recovery state,
and nonfatal old-history deletion failures still apply.

### Storage layout

Group check-owned artifacts under one request directory in the account-wide
state root, separate from the developer's Git worktree. Its children
are `logs/`, `diagnostics/`, `work/`, `tmp/`, and `cache/`, alongside request
control metadata. `work/` contains disposable source copies, archives, check
trees, temporary libraries, and generated publications. Configure subprocess
temporary roots before starting them so fixtures and backend data are owned
by that request too.

The request root is the OS account home plus
`.local/state/marginplyr/review-ready/requests/<request-id>/`. Use the accepted
renderer isolation below rather than assuming `TMPDIR` relocates every cache.
Existing shared caches are outside automatic deletion; OS reports follow the
boundary below.

Keep global, empty shared lock targets while their protocols operate, including
admission and evidence-publication locks. They contain no history and do not
grow with requests. Do not delete or recreate them during normal pruning;
remove them only after the corresponding protocol is fully retired.
Request-specific lock files may be collected with their safely retired request.
OS-generated crash reports remain under OS policy, outside automatic
Review-ready deletion. Copies retained by this runner follow the seven-day
diagnostic policy.

## Renderer and cache isolation

Give Quarto and its descendants a request-owned HOME, temporary directories,
and cache environment. Preserve the selected R, Quarto, and Deno executable
identities and absolute paths, and the gate's ordered absolute R library paths,
including its temporary installed package. Do not implicitly copy personal
home settings or credentials into the render environment. Keep the outer
supervisor's HOME and account-derived state/lock identity unchanged.

Register and verify the supported renderer/cache behavior, including Quarto
app-directory and Sass storage, Deno, project scratch, R user caches, and
temporary files. Configure those resolved destinations inside the owned root.
Unknown or unsupported configurations stop with a diagnostic naming the tool
or location; they do not fall back to shared-cache writes. Existing shared
caches are not deleted, moved, or initialized by the check.

This is a contract for supported tools and registered artifact families, not
an OS sandbox against arbitrary native filesystem side effects. Check the
actual resolved destinations and observed writes in fixtures and real renders.
Do not infer exclusive ownership from a shared directory's before/after
differences while other work may run.

Start validation with empty isolated caches. Preserve README/help/NAMESPACE
and figure regeneration comparisons, vignette execution, and site verification
with its existing markers. During an authorized retry, keep the same tarball,
tools, render HOME/cache, and library environment; do not reset caches between
attempts. Remove owned cache and render trees only after all writers stopped.

The delivery choice supersedes the common-installation direction in
[resource admission](review-ready-resource-admission.md). Admission and
process-lifecycle requirements remain constraints except for the explicitly
accepted R-supervisor startup and delivery changes described here.

## Component research

The [R tooling investigation](../../investigation/review-ready-r-tools-2026-10-31.md)
records the first-party evidence on focused testing, scheduling, profiling,
incremental pipelines, and process-management components. `processx`, `ps`,
`filelock`, and `openssl` are selected. The failure model, seven-day history,
rendering environment, and ownership boundaries are accepted; implementation
evidence remains to be produced.

The evidence-storage extension also records the R cryptographic-component
comparison supporting the accepted OpenSSL R package. Its hashing, HMAC, and
secure-random APIs justify the dependency; installed availability alone does
not. The `askpass`/`sys` closure and OpenSSL system dependency are part of setup.
A versioned byte encoding and authentication-verification contract still
require explicit implementation and fixtures.

## Decision status

All policy branches, including the PR #1115 integration and its evidence-store
lifecycle and cryptographic component, are accepted. No interview questions
remain open.
Protocol numbers, protected-file registration, record representation, and
fixture/API names are implementation choices constrained by this contract.

## Implementation acceptance and rollout

Use process fixtures in isolated test state before real rollout. Preserve the
existing lifecycle scenarios while replacing Python-specific test harness
mechanics as needed. For the pinned baseline, map every declared evidence and
admission regression case, the R gate verifier's cases, and registered contract
fixtures to a preserved case, an equivalent replacement, or an amendment
explicitly owned by this specification. Record the mapping and corresponding
new assertions in the implementation's validation record. An unchanged test
count is not evidence of equivalent assertions; a case may not disappear
silently during the language change.

Report inherited-behavior verification separately from the new retirement,
key-renewal, renderer-isolation, and authority-model verification. Establish
the preserved contracts in isolated state, then their interaction with the
amendments. Both are required before operational cutover.

The [contract audit](../../investigation/review-ready-1115-contract-audit-2026-11-01.md)
records the inspected baseline and case inventory. The following cases establish
the replacement contract in addition to the inherited requirements:

| Case | Required observation |
| --- | --- |
| Git update, old commit, detached HEAD | The selected tracked version runs without reinstall; unsupported versions stop with an actionable diagnostic |
| HEAD/cleanliness changes while waiting | Stale request cancels without checking a newer commit; main-only updates leave pinned identities unchanged |
| Source or accepted manager unavailable | No silent source substitution, fetch, self-approval, or unrestricted gate fallback |
| Comparison unavailable with accepted manager available | Preserve conservative Full selection and required routes without inventing a base; unresolved artifact verification fails; missing main authority separately stops setup |
| Trust-core/registration/required-fixture changes | Accepted main manager holds Full and enforces its required verification; protocol equality alone does not grant capability |
| Concurrent clones/worktrees | One real Full slot; limited work follows its existing independent supervision and bounded workloads |
| Existing stage reuse | Preserve cumulative obligations, original execution origins, current mandatory checks, actual-work capacity, and same-request promotion |
| Origin eligibility | Same OS user and repository, ancestor commit, deterministic selection, authorized terminal-success request with confirmed cleanup/capacity release and matching observations; reject partial success, same-tree non-ancestors, and inherited results presented as executions |
| Stage boundaries | Keep Full and Documentation build/check, generation, and Documentation assertions distinct; Full may discharge matching Documentation obligations, never the reverse; Repository makes no package-execution claim |
| Source and contract identity | Preserve all tracked path/type/mode/object inputs and whole DESCRIPTION; exclusions need audited registration; protected expectation normalization preserves strict assertions and independent positive/negative contracts |
| Unknown source and external inputs | Cover Git replacements/grafts/attributes/configuration, committed links/submodules, executable or non-text exclusions, undeclared startup/build hooks, and unsupported native resolution; retain the applicable execution or refusal instead of assuming identity |
| Environment contents and changes | Compare actual tool/runtime/native assets, ordered libraries and installed contents, content and change metadata, symlink targets and directory structure before/after execution; equal package versions alone do not suffice; fingerprint secret environment values without storing their plaintext |
| Real reuse after replacement | Establish a new R-runner Full origin, then demonstrate the protected follow-up in another clone with private render paths and unchanged identity guarantees |
| Child environment | Probe, documentation proof, gate and descendants use the same registered environment; known-format Codex/Claude IDs are actually absent, unknown values/configuration stay inputs, and an outdated visible-name audit cannot qualify |
| Time-dependent eligibility | Preserve version-bound checker identity, effective leeway, equality/future boundaries, original timestamp inventory, current archive timestamps and clock rollback handling; unknown conditions require execution |
| NOTE and retry eligibility | A manual NOTE explanation does not make an origin reusable; require shared-policy classifications, exact check arguments/counts, complete attempts, and the preserved eligible first failure and unchanged retry identities |
| Stage observation and recovery | Bind reports to authorized work; retain failed/unavailable observations before cleanup, including cleanup failure and manager loss; incomplete/empty reports and pending observations cannot become successful origins; recovery repairs observations, not successful execution claims |
| Evidence ordering and publication | Use observation order, complete authenticated inventory and original references; missing/corrupt/cyclic evidence and unobserved work fence older successes; revalidate the pinned origin/unit plan under issuance synchronization, including a failure arriving after worker validation, without silently choosing a different origin |
| Cache reads versus interrupted writes | Unavailable cache reads disable reuse without blocking an executed gate's cleanup; interrupted observation/publication writes preserve required recovery ownership; a request relying on reuse cannot pass after issuance failure |
| Authentication protocol | Establish deterministic versioned bytes, known-answer hash/HMAC cases, malformed/ambiguous input rejection, wrong-key/tampered record/index rejection, schema/provenance checks, private key owner/mode/length checks, and verification across renewal |
| Evidence retirement | Expiration and interrupted/concurrent pruning cannot resurrect successes, leave an inconsistent issuance inventory, or silently omit an obligation |
| Key rotation | Still-valid origins survive authentication renewal with their original deadlines; no incomplete rotation exposes partially migrated evidence or prematurely removes a needed key |
| Cancellation, manager death, spawn-registration gap | Owned children are tracked and stopped; uncertain cleanup protects capacity and never produces a pass |
| Recovery from another worktree | Compatible code can recover a stopped request without the originating checkout; live, corrupt, or unknown state is not guessed away |
| Seven-day boundary, future/invalid timestamps | Use the validated terminal time, not mtime; test before/at the boundary and refuse untrustworthy age calculations |
| Concurrent pruning and terminal handoff | Do not delete a live manager, active writer, or request still delivering its terminal result |
| Partial deletion or interrupted pruning | Old-history failure is reported/retried without turning a partially removed history directory into a new active-recovery blocker |
| Automatic recovery | Complete stopped-ownership evidence precedes interrupted outcome and retention; a surviving child prevents recovery |
| Current-request cleanup failure | No terminal success or premature capacity release, regardless of history-pruning policy |
| Renderer isolation | Actual README, help/figures, vignettes, and site render from empty private caches with the correct installed package; registered writes stay owned |
| Renderer failure, retry, cancellation | Preserve same-request render identities/cache through retry and remove them only after writer shutdown |
| Deletion boundaries | Original clones/worktrees, Git-tracked research, shared caches, installed developer libraries/tools, and OS reports remain untouched |

Apply request ownership and path validation to every deletion. Cover symlinks,
Git alternates, unrelated temporary names, missing timestamps, and partially
written legacy bundles. Associate retained diagnostics with their request so
the collector does not need to guess from filename prefixes. Demonstrate that
temporary R, fixture, backend, generation, and site data are enclosed by the
request's roots even after interruption.

Exercise evidence retirement with inverted observation/cleanup order, broad
unknown-input vetoes, repeated origins and dependent references, an origin
expiring during reuse, and concurrent terminal publication. Exercise rotation
with idle intervals, valid reusable origins, and termination before old-key
retirement or during record/index replacement. Establish that renewal changes
authentication only, retains the original deadlines, and leaves no permanent
negative-history archive.

Cover a retained request containing both an inherited unit and its own newly
executed unit when the inherited origin expires. Retire the dependent reuse
without destroying the independent unit's valid provenance. Before accepting
the R authentication implementation, document the supported comparison
mechanism preserving the protections of the baseline's `hmac.compare_digest`.
Equal-output tests and timing measurements alone do not establish that property.

Bootstrap the first implementation through the currently required installed
CLI's Full check plus dedicated new-runner fixtures. Until local main contains
an accepted R manager, the new entry stops rather than adopting its own
candidate implementation as authority. Fixture runs use isolated test state
under their admitted outer verification, not competing normal user requests.

At migration, verify old waiting/running requests have finished or been
cancelled by their owners with confirmed cleanup; resolve old recovery state
before retiring its entry points. Refresh instructions and participating
sessions, then demonstrate real Full, Documentation, and supported Repository
invocations through the new R entry. Preserve one handle through each terminal
result. Missing dependencies or unsupported process/lock inspection must fail
setup rather than bypass the boundary.

Establish reusable origins through actual executions under the new R
implementation. The old Python machinery is itself part of the registered
source and environment identity; rereading or reauthenticating its records
cannot establish that those identities match the replacement. Do not import
legacy results as newly executed R evidence. Attribute and retire their
history and control artifacts under the accepted migration/retention policy.

Update installation/update guidance, local-check entry points and retention
wording, direct-entry delegation, registered contracts, dependency declarations,
and relevant CI fixtures together. Until that rollout, existing instructions
still require the installed common command for actual checks. This design
neither runs migration nor deletes current or historical artifacts.

Preserve the evidence reuse implemented by PR #1115 under ADR 0038 while
replacing its Python machinery and introducing bounded storage. Do not add
new limited-execution recognition routes as part of this delivery change.
