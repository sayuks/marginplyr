# Repository-owned R delivery

This is the R generation's setup and operating guide under
[ADR 0039](../design/adr/0039-run-review-ready-from-the-target-worktree.md).
The [runner specification](../design/specs/review-ready-repository-runner.md)
owns acceptance. Until its drained cutover is established, use the installed
command required by [local checks](../design/agents/local-checks.md).

## Setup and authority

Install the packages in `DESCRIPTION`'s `Config/Needs/review` field. The runner
directly validates processx, ps, filelock, openssl, jsonlite, and openssl's
askpass/sys closure. OpenSSL headers and libraries are development requirements
of the openssl R package. A working R C compiler is also needed for the small
`tools/review-ready-signals.c` receiver: base R exposes interrupts but no
SIGTERM/SIGHUP receiver. It records cancellation; processx and ps control and
inspect children. Its monotonic clock measures shutdown and mailbox deadlines
independently of the UTC timestamps used for history and evidence. The pinned
source is compiled inside each request during setup, before admission waiting.
These are development tools, not package
runtime Imports.

The review and website setup also include cpp11 for the development-dependency
audit performed by devtools when it installs the source for README generation.

The entry is `Rscript tools/review-ready.R`, from the target root, with no
check/skip options. `--status` and `--recover REQUEST` operate without a clean
HEAD or the originating checkout. They do not run a check. Unknown control
versions remain visible and require compatible recovery code.

For checks, local main supplies accepted R sources through pinned Git objects.
A missing main or accepted R manager stops setup. The manager and registered
helpers are materialized before waiting; later Git updates cannot substitute
them. `tools/review-ready-registry.R` owns the R generation's classification
and workload registration. A candidate that changes the trust core uses main's
outer Full supervisor and gate. The accepted runner fixture reads the candidate
implementation as its test subject. A candidate hash is not its own acceptance.

## Drained cutover

After the accepted implementation and its required verification are available
on local main, run `Rscript tools/migrate-review-ready.R` from that revision.
It checks legacy status, retires the installed check entry, rechecks status and
live launcher processes, and refuses unfinished or damaged ownership. Owners
finish/cancel their own work and confirm cleanup. A retained legacy adapter
supports status/recovery during an interrupted cutover; it cannot launch a
check. `delivery.json` is published only after the drain and history attribution.
Normal R requests require that marker. Updating a worktree subsequently updates
its tracked runner without reinstalling an executable.

Refresh participating sessions to acknowledge the repository entry, same-agent
bounded investigation, retained execution handle, cancellation propagation,
and terminal cleanup transitions in local checks. Verify actual Full,
Documentation and supported Repository invocations and actual cross-clone
reuse with a new R Full origin. A process fixture's checker stand-in is not
that execution evidence. The first implementation is bootstrapped through the
installed command's Full check; it cannot adopt itself as local main authority.

## Request storage and retention

The OS account database supplies the shared root independently of HOME/XDG:
`.local/state/marginplyr/review-ready/requests/<request>/`. A request contains
`logs/`, `diagnostics/`, `work/`, `tmp/`, `cache/`, its manager lock, and a
checksummed control record. State is published by a closed complete write and
same-directory rename. Manager termination is recoverable; latest-write
durability across OS crashes or power loss is not promised.

Owned processes must stop before work, temporary files and caches are removed.
Terminal history and diagnostics expire seven days after confirmed completion.
Startup/completion maintenance recovers only confirmed stopped ownership, and
protects live, unknown or damaged state. Safely retired histories are renamed
before recursive deletion so partial deletion cannot become an active blocker.
Older-history deletion failures are reported and retried. The current request's
cleanup failure still protects capacity. Shared empty lock files remain.

Migration attributes terminal legacy request directories separately and keeps
their original completion times. Unattributed manual captures, shared caches,
Git worktrees, installed developer libraries/tools, tracked research and OS
crash reports are outside runtime deletion. Historical diagnostics outside the
common request tree require individual provenance; a matching filename alone
is insufficient.

## Evidence transactions and authentication

`tools/review-ready-evidence.R` owns issuance, validation and collection. The
v2 store holds a complete authenticated inventory in a generation directory.
An atomic `current.json` replacement publishes retirement, ordering and key
changes together under the permanent evidence lock. Readers use the same lock.
Unpublished generations cannot authorize reuse. A terminal origin is bound to
the authorized request and its pre-cleanup observation, source/environment,
actual executed units and original completion deadline.

The byte protocol length-prefixes UTF-8 strings, sorts unique map keys, retains
array order, distinguishes null and booleans, and accepts finite numbers only.
Sealing first normalizes through the store's JSON representation, then applies
the v2 encoding. SHA-256 and HMAC-SHA256 use openssl. Key files require the
account owner, mode 0600 and exactly 32 secure-random bytes. Secret environment
values are represented by fingerprints, never plaintext.

Verification blinds both fixed-length authenticators with a fresh random
HMAC key before comparing the blinded results. This follows the random-key
[double-HMAC comparison strategy](https://www.paragonie.com/blog/2015/11/preventing-timing-attacks-on-string-comparison-with-double-hmac-strategy):
comparison behavior cannot reveal a prefix of the original MAC. It does not
claim that R's `identical()` is a constant-time primitive. Known-answer and
tamper fixtures establish the protocol separately from that comparison argument.

Keys renew after seven days through the same generation transaction. Renewal
changes authentication, not origin identity, sequence, outcome or deadline.
Retirement removes the successes vetoed by an expiring negative observation in
that same transaction. References do not extend origin eligibility. A retained
request's own independent executed unit remains distinct from inherited reuse.

## Renderer isolation

Each request starts with empty private HOME, XDG, Deno, R user cache/config/data,
and temporary roots. Absolute ordered R libraries and selected installed tools
are preserved. Metadata queries check the required render/Pandoc and TypeScript
features and the actual Quarto/Deno cache, HOME and temporary destinations.
Compatible versions and private storage layouts are accepted; unavailable
features, external storage and unsupported overrides name their refusal reason.
CI continues to use current stable Quarto. Actual versions and content remain
evidence inputs, so accepting an update does not reuse the previous tools'
results. After capability and identity queries finish, the worker empties its
owned cache tree before starting validation. Retry keeps the same tarball, tool
identities and private caches until all writers stop.
Shared caches and personal home settings are not copied into the request.

Registered request storage roles are normalized in environment
identity; HOME and cache configuration remain represented. The same effective
child environment reaches the identity probe, documentation proof, gate and
descendants. The metadata-removal rules of ADR 0038 still apply.
The R startup paper-size value is recorded before child startup. Known-format
processx/ps tree markers with value `YES` are supervisor transport: they remain
in children for cleanup and are excluded from package-input identity. Unknown
marker names/values remain inputs. The mechanism is documented by
[processx process cleanup](https://processx.r-lib.org/articles/cleanup.html).

## Verification entry

`Rscript tools/test-review-ready-runner.R` runs the new authentication and
isolated process contracts. `jarl check .`, package-aware lintr, the existing
admission/evidence fixtures and R gate verifier remain part of bootstrap Full.
CI also runs the R process contracts with their declared tool dependencies.
Acceptance records distinguish these fixtures from real package checks and
operational rollout.
