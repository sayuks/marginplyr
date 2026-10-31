---
status: accepted
---

# Reuse unaffected Review-ready stage evidence

For [#1110](https://github.com/sayuks/marginplyr/issues/1110), retain the
cumulative final-tree comparison against the unique merge-base with local
`main`. Verify changed artifacts at the submitted SHA and permit prior
successful evidence to discharge only the stage obligations shown to be
unaffected. Moving the comparison base to the last successful commit was
rejected because it would omit cumulative obligations rather than establish
that they remain satisfied.

## Agreed direction

A mixed branch may retain package implementation changes already verified by
Full while adding a verification-only follow-up. Direct artifact verification
establishes the follow-up's contract; reuse must separately establish why the
earlier stage result applies to the submitted revision. Report executed,
omitted, and reused stages separately. A limited result is not a new Full pass.

Executable tooling changes are candidates as well as expectation, fixture,
snapshot, and diagnostic changes. Eligibility requires an explicit contract
and verification appropriate to the changed behavior; file location, a small
diff, or the presence of a fixture does not establish eligibility. Unknown
effects retain Full, and a package-suite pass does not discharge an unresolved
tooling contract.

## Evidence and invalidation

Accept only new-format successful records emitted by the local
common CLI for the same user and repository, at an ancestor of the submitted
commit. Another clone is eligible under those conditions. Require completed
stages, a successful terminal outcome, and confirmed cleanup. Do not convert
old logs, CI results, or agent reports into reusable records. Repeated reuse
must retain a reference to the original execution rather than turn an inherited
result into a claim of execution.

Reuse depends on matching each stage's complete registered inputs, verifier
implementation, and relevant tool, dependency, and execution-environment
identities. There is no age-only expiry. An unavailable or changed identity
invalidates the affected stage; rerun it rather than assume compatibility.

Treat build and check as one reuse unit. Keep Full and Documentation evidence
distinct: a Documentation result cannot discharge a Full obligation.
Generation and Documentation assertions are separate units. Run the common
source checks and changed-artifact contract verification at the submitted SHA
on every invocation.

Include all of `DESCRIPTION` in the registered inputs, without field-level
normalization for review-tool dependencies. Register inputs read from outside
the tarball too, including the progress reporter and documentation generation
sources. Compare relevant input and environment identities before and after
execution. A change or an undeclared external input makes the result
unavailable for reuse.

Accept only origins with zero NOTEs or with every NOTE classified as allowed
by the shared policy. A manual explanation of an unclassified NOTE does not
make it reusable. A later failure under the same stage inputs and environment
prevents selecting an older success instead. A contract validation failure
at the submitted SHA fails that invocation; switching to Full cannot replace
the failure.

## Independent contracts and trust

A changed verifier's success is insufficient to establish its eligibility.
Require an independent contract and negative cases that still reject missing
or incorrect values. For the motivating dependency expectation correction,
check the existing declarations and actual tool use independently, reproduce
the original mismatch, and verify that the correction passes while missing or
incorrect dependencies fail.

Changes to selection, admission, evidence issuance or validation, trusted
contract registration, or installation of that trusted machinery
require Full plus dedicated contract and process verification. They cannot
certify themselves for limited execution. Other tooling logic and fixtures
remain candidates when an existing trusted verification route establishes
their contract. Limited certification of the trust core is outside this
decision's scope.

Recognize supported changes mechanically through trusted, route-specific
rules. An expectation correction must preserve the protected verification
structure and satisfy its independent positive and negative contracts.
Executable tooling changes need their own registered recognition route.
Unknown forms require Full and dedicated verification. Reinstalling changed
source bytes does not establish eligibility.

## Environment and time

Use the existing CLI's cooperative same-user trust boundary. Do not update
relevant tools or libraries during execution. Compare their content and
change metadata before and after execution and reuse validation; observed
changes or unavailable identities prevent reuse. Whole-environment isolation
and complete detection of a transient change followed by restoration are
outside the guarantee.

Use one registered child environment for the identity probe, documentation
proof, actual gate and its descendants. Remove known-format Codex and Claude
Code execution metadata there; retain unknown values, genuine configuration
and supervisor transport. The manager keeps its original environment. Match
evidence against that effective child environment.

Reevaluate time-dependent conditions on every invocation. Preserve the
origin's inspected paths and timestamps and account for the submitted
archive's timestamps. Bind the lightweight future-timestamp evaluator to
the supported R checker implementation and test its boundary behavior.
Unknown R implementations or unregistered time-dependent inputs make the
build/check unit ineligible for reuse rather than suppressing the check.

## Considered options

Omitting execution IDs only from the fingerprint while scanning source readers
was rejected. Computed callees can consume those IDs without containing a
recognizable name; actual child-environment removal establishes their absence.

## Execution and migration

Admission follows the work actually executed. Running the Full build/check
unit requires the common Full slot. When that unit is validly reused and all
remaining work qualifies for limited execution, use independent supervision.
Resolve any strengthening before validation starts. Preserve one common CLI
invocation, execution ownership, cancellation, cleanup, and terminal record.

Existing logs cannot supply the new evidence format. Establish a new-format
baseline execution before reusing its stages. The named regression case
establishes that baseline, applies the expectation-only correction, and proves
that the follow-up executes its contract without repeating Full.

## Specification and adoption

The [verification follow-up specification](../specs/review-ready-verification-followups.md)
records the stage inputs, evidence validation, case matrix, and required
regressions. This design has one implementation scope; its exclusions are
not promises of subsequent implementation.

[Local checks](../agents/local-checks.md) and
[shared admission](../../tools/review-ready-admission.md) own operation and
migration of the implementation. The classifier and stage contracts live in
`tools/review-ready-admission.py`; `tools/review-ready-evidence.py` registers
inputs, protected follow-ups and evidence validation. CI package checks and
coverage requirements are outside this change.
