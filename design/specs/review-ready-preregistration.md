# Register tests before Review-ready measurement

Status: accepted, 2026-11-05; runner implementation pending. The maintainer
accepted registration before full measurement, the existing reordering
thresholds for every registered file, and an agent-owned main compatibility
assessment before either Review-ready scope.

This amends the registration and reordering decisions in
[ADR 0040](../adr/0040-learn-local-test-order-and-admit-heavy-checks-fifo.md)
and the [earlier specification](review-ready-adaptive-order-fifo.md).
The active runner still permits unregistered files and generates measured
registration proposals. Until implementation adopts this amendment, follow
the existing [runner guide](../../tools/review-ready-runner.md) for those
proposals. Design acceptance does not establish implementation or a check result.

## Registration before invocation

Register every discovered package test file in `DESCRIPTION`'s
`Config/testthat/start-first` before committing the revision submitted to
Review-ready. Registration denotes dispatch order, not successful execution or
eligible timing evidence. Helpers and standalone tooling fixtures are outside
this list; use the same native testthat discovery as package-test dispatch.

Preserve the relative order of retained entries, remove deleted names, and
append all new names in native discovery order. A rename removes its old name
and appends its new name. Values remain unique literal filename stems, with
the `test-` prefix and file extension removed. This preserves native append-last
dispatch when only missing entries are appended in discovery order.

Extend the existing priority-metadata preflight in both `source` and `full`:
require exact membership equality between native discovery and registered
stems, as well as unique literal entries. Report missing names, obsolete names
and duplicates, then fail before the six checks or heavy admission. The runner
does not repair the submitted source. The agent corrects and commits metadata
before submitting a fresh invocation.

Run this registration-completeness check in pull-request CI as a mandatory
failing check, including when local Review-ready was not invoked. Review-ready
and CI must call the same validation function with the same native discovery
rules and diagnostics. Extend the existing priority verification in
`.github/workflows/lint.yaml` to check the submitted checkout itself; fixture
coverage alone does not validate that checkout's registration. Discovery and
metadata comparison need no execution of package tests or timing measurement.

An all-skipped or empty test file still needs registration. It supplies no
promotion evidence. Native dispatch must continue selecting every discovered
file exactly once; list completeness alone does not establish that property.

## Measurement and adoption

Keep the existing full-check recorder and evidence eligibility rules. Every
file, including a newly appended one, uses the same stable-insertion rule:
move left only across eligible observations with a duration difference greater
than both 50% and ten seconds. Adopt a candidate only when the eight-worker
model estimates at least ten seconds of whole-suite improvement over the
current committed order. There is no separate measured insertion rule for
newly registered files.

Inspect and record the first successful full result's proposal as before.
If order is unchanged, keep the full-checked SHA and finish without another
commit or `source` invocation solely for priority. If an eligible reordering
is adopted, commit it and verify its new SHA under the existing measured
priority-only scope exception. Other changes retain their own scope obligation;
new tests still require full verification.

Preserve the seven-day evidence deadline, dependency and test-input freshness,
recalculation against changed priority, timing-failure dispositions, and the
boundary that validation alone does not start a second adoption cycle.

## Main compatibility before verification

The [integration workflow](../agents/integration.md) owns the mandatory
agent preparation before either scope and reassessment before PR publication
and merge. Resolve required integration before checking the resulting SHA.
Compatible branches may proceed without merging main. The six runner checks
remain unchanged; this decision adds no runner fetch or seventh check.

The [review workflow](../agents/code-review.md) uses the assessed fetched main
SHA as the fixed point, so an outdated local `main` cannot add imported main
changes to the review diff.

## Implementation acceptance

- Prove both scopes reject missing, obsolete, duplicate and invalid entries
  before baseline execution or heavy admission, while accepting complete lists
  containing all-skipped files.
- Prove CI invokes the shared validator on its actual checkout and fails for
  missing, obsolete, duplicate or invalid entries even without a local
  Review-ready result. Retain regression fixtures for each failure case.
- Prove native ordered discovery is identical before and after appending all
  missing names in discovery order, and every file is selected exactly once.
- Exercise additions, deletion and rename; helper files must not become entries.
- Prove newly appended and previously registered files use the same comparison
  margins and whole-suite adoption threshold, including boundary cases.
- Update proposal explanations and fixtures to use the current complete order
  as the baseline. Preserve unavailable-evidence and validation-cycle behavior.
- Reconcile ADR 0040, the earlier specification, local checks and the runner
  guide when implementation adopts this design. Receive the applicable
  change-specific checks and full Review-ready result for that implementation.
