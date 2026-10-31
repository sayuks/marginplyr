# Verification-only Review-ready follow-ups

Design for [#1110](https://github.com/sayuks/marginplyr/issues/1110), under
[ADR 0038](../adr/0038-reuse-unaffected-review-ready-stage-evidence.md).
The maintainer accepted this specification on 2026-10-31.
The implementation uses the common CLI and the operating guidance in
[local checks](../agents/local-checks.md#reusing-unaffected-stages).

## Obligations and execution plan

Derive required verification from the cumulative final-tree difference between
the submitted clean commit and its unique merge-base with pinned local `main`.
Keep the submitted, main, and base identities throughout the invocation.
Derive a separate stage execution plan: each obligation is executed, validly
reused, or omitted because the cumulative classification does not require it.
Missing evidence is never an omission reason for a required stage.

Keep Full and Documentation build/check units distinct. Full evidence can
discharge the build/check obligation of Documentation only when its complete
input and environment contract also matches; the reverse is prohibited.
Generation and Documentation assertions have separate evidence. Run dependency
syntax and required-Suggest checks whenever the cumulative package path
requires them, even when build/check is reused. Common spelling, jarl,
package-aware lintr, reference/context/invocation checks, and required
changed-artifact contracts execute at the submitted SHA.

## Registered source and environment inputs

Default to including every tracked path in a stage's source identity. Exclude
a path only through that stage's explicit, audited non-input registration.
The registration must cover paths, file types, modes, bytes, additions,
deletions, and both sides of renames. Unknown paths or comparisons cannot
establish independence. Tarball exclusion alone is insufficient.

| Unit or check | Required source inputs in addition to its own implementation and explicit dependencies |
|---|---|
| Build/check | Package sources, tests and lifecycle files, examples, vignettes, data, installed files, all of DESCRIPTION, NAMESPACE, build exclusions, check configuration, NOTE policy, progress reporter, check arguments and registered source timestamps |
| Dependency syntax | All of DESCRIPTION and test source files, including supported R and Rin forms; R scanner implementation |
| Required Suggests | All of DESCRIPTION, optional-backend registration and guard; resolved installed availability and versions |
| Generation | Archived sources except proven non-inputs; README.qmd, README.md, R/roxygen sources, NAMESPACE, help and figures, installation inputs and renderer configuration |
| Documentation assertions | Package sources and metadata; all test sources, helpers, setup and teardown; runner and exact registration; both README and LICENSE forms; help and vignette sources |
| Artifact routes | Inspected artifacts and inventory, verifier dependencies, protected workflow/configuration contracts, command fixtures, and pinned comparison identities |

Register R implementation, OS/architecture, resolved executables and relevant
runtime assets, ordered library paths and visible installed package contents,
startup profiles and environment files, relevant build configuration,
environment variables, locale, timezone, and renderer identities. Package
versions alone do not establish identity. Normalize invocation-specific
paths/tokens only where a registered rule establishes their irrelevance.
Undeclared external inputs prevent reuse. Capture content and change metadata
before and after execution and around reuse validation. Preserve the
cooperative environment boundary in the ADR; do not claim hermetic execution.

Reevaluate registered clock-dependent conditions at the submitted revision.
Store the origin check's timestamp inventory and account for current archive
timestamps. Require a version-bound timestamp contract, including effective
leeway, equality and future boundaries, clock rollback, and unknown R behavior.
If the inventory cannot establish the current condition without rebuilding,
execute build/check. Other clock-dependent stage behavior needs its own
registered contract or is ineligible for reuse.

## Evidence records and selection

The common supervisor emits a structured success record only after the
invocation reaches terminal success and confirms owned-process and workspace
cleanup. Bind it to the request, repository, user, submitted/main/base
identities, supervisor and contract versions, stage input/environment digests,
actual completed stages, check arguments, final exit/timeout and condition
counts, NOTE classifications, and attempts/retry disposition. Validate record
integrity, schema, provenance in common CLI state, and all referenced origins.
Partial output, a dead process, or a copied agent report cannot issue evidence.

An origin must belong to the same repository and local user and be an ancestor
of the submitted commit. Another clone is allowed under those conditions.
Require matching registered identities and all required completion data. Reject
unknown schemas, corrupt or unavailable records, missing origins, non-ancestors,
unclassified NOTEs, and later same-input/environment stage failures. A zero
exit without those facts is insufficient. A policy-authorized retry retains
both attempts and is reusable only if its complete retry and final-success
conditions are established. References to reused evidence always lead to the
actual executing origin; inheritance never becomes a new execution claim.

Search compatible records deterministically and record the chosen origin per
unit. There is no age-only expiry. If no acceptable origin exists, execute the
required unit. Eligibility failures can strengthen the plan before validation;
actual assertion failures terminate the invocation. An unresolved artifact
contract remains unresolved even when a package check passes.

## Recognition, admission, and reporting

Keep route-specific change recognizers and independent contracts inside the
trusted machinery. A recognizer must establish both semantic eligibility and
the permitted workload. Expected-value or fixture recognition must preserve
protected assertion structure and check independent positive and negative
cases. Tooling logic needs an explicit route; motivation, file location, small
diff size, or agent approval does not substitute for one. Do not recognize
arbitrary R program equivalence. Reinstallation alone cannot confer a limited
capability. Changes to the trust core require Full plus dedicated contract
and process verification, as the ADR specifies.

Use the common Full slot whenever the plan actually runs Full build/check or
another workload that requires it. A fully qualified limited plan with reused
package evidence uses independent supervision under the existing bounded
workload rules. Retain the same request, handle, identities, ownership,
cancellation, cleanup, and terminal completion through any pre-validation
promotion. No caller path/skip switches are introduced. Revalidate reuse
eligibility before gate start; observed invalidation after validation starts
fails the invocation rather than granting a late promotion or silently rerunning.

Report cumulative required verification separately from executed workloads.
List each executed, reused, or omitted stage, reuse origin and reason,
comparison identities, capacity, worker policy, timestamp reevaluation,
NOTE/retry dispositions, and final outcome and cleanup. A limited execution
with inherited Full evidence is not a new Full pass. Repository evidence does
not imply that a package build/check ran; Documentation evidence does not
imply that the full suite or snapshots ran.

## Case matrix

All limited cases require a trusted recognition route, current source checks,
current affected-artifact verification, and reusable evidence for any cumulative
obligation they do not execute. The following are cases to validate, not a
path-based allowlist.

| Change or evidence case | Eligibility and required outcome |
|---|---|
| Named #1107 dependency expected-vector correction | Preserve strict equality; independently establish already-declared tool use; execute verify-cran-preflight.R and affected dependency contracts; reuse matching package units without building/checking again |
| Expected value, fixture, snapshot, or diagnostic update in a repository verifier | Limited only for recognized structures with an independent contract and negative cases; changes under package tests retain package obligations |
| CI script, workflow wiring, or configuration contract | Use a registered route with syntax, behavior and wiring coverage appropriate to its change; retain unresolved CI validation when a route is absent |
| Registered tooling executable logic | Limited only when its existing trusted route establishes both the changed contract and bounded workload |
| Weakened assertion or unknown executable change | No expectation-only recognition; require Full and dedicated verification, and do not certify unresolved contracts |
| Selection, admission, installer, evidence, trusted registration | Full plus dedicated contract/process verification; no self-certification |
| Review/check-tool dependency declaration | Whole-DESCRIPTION identity changes; invalidate affected package, generation, dependency and environment evidence and execute the resulting obligations |
| Corrected assertion about unchanged declarations | Declaration identity remains unchanged; the assertion is still verified independently at the submitted SHA |
| Earlier package implementation plus eligible artifact follow-up | Preserve cumulative Full obligations; reuse matching ancestor units and execute the follow-up contracts |
| Package behavior or tests change along with artifact follow-up | Invalidate package evidence and run Full plus affected artifact verification |
| Eligible follow-up after Documentation | Reuse only matching Documentation/generation/assertion units; strengthen if new cumulative obligations require Full |
| Follow-up after Repository | Do not infer package evidence; execute any newly required Documentation or Full units |
| Missing, stale, incompatible, non-ancestor, corrupt, or partial evidence | No reuse; run required units or fail when source/comparison/setup cannot be established |
| Environment changes, unregistered external input, unsupported time behavior | No reuse; observed changes during validation fail the invocation |
| Known artifact failure at the submitted SHA | Terminal failure; no Full fallback that masks the failure |

## Regression and delivery requirements

Exercise the named correction with a new-format baseline origin. Demonstrate
failure on the original expectation, success after correction, and rejection
of missing/incorrect dependencies. The follow-up must execute its actual
contract and finish without package build/check or full-suite reconstruction.
Old logs are not imported to manufacture the baseline.

The correction's existing direct commands are:

```sh
Rscript .github/scripts/verify-cran-preflight.R
Rscript .github/scripts/verify-review-ready-check.R
```

Also require the trusted route to compare the unchanged declarations with the
actual `roxygen2::roxygenise()` and `devtools::build_readme()` calls in
`tools/review-ready-generated.R`. The two commands' expected vectors alone
are not that independent evidence. Inspecting these calls does not require
executing documentation generation or installing/building the package.

Cover mixed branches, repeated reuse, Documentation and Repository origins,
new package obligations, changed declarations, weakened assertions, unknown
logic, missing/corrupt/stale origins, incompatible dependencies, environment
changes, timestamp boundaries, cleanup failures, retry/NOTE dispositions, and
same-input failures after a pass. Verify admission and cancellation through
the installed common CLI. Tests must distinguish actual execution, reuse, and
omission, including negative cases where limited success must be refused.

Update the authoritative classifier/verifier and workload registrations,
local-check freshness/comparison guidance, shared admission guidance, installer
capabilities and applicable ADR references together. Revise #1102's blanket
Full boundary only for the routes established here; preserve #1108 supervision
and workload limits. Existing sessions refresh before their next invocation.
The implementation itself changes the trust core and requires Full plus the
dedicated verifications. CI package checks and coverage requirements do not
change.
