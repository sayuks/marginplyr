# Independent archive replay and submission boundary

Investigated: 2026-11-07
Tested package SHA: `e1dabaabb392a3791177e5c48e983f780c7c93ec`

The coordinator created a disposable source with Git archive from the named
main revision, extracted it into
`/private/tmp/marginplyr-hunt-20261107-replay/source`, and loaded that directory
through pkgload in separate R processes. No installed marginplyr substituted
for the archived source. The adjacent environment record retains sessionInfo.

## Retained final execution

The delivered `replay.R` driver was invoked from the investigation worktree:

```sh
Rscript investigation/2026-11-07-summary-grouping-share-hunt/coordinator/replay.R \
  /private/tmp/marginplyr-hunt-20261107-replay/source \
  /private/tmp/marginplyr-summary-grouping-share-hunt-20261107/investigation/2026-11-07-summary-grouping-share-hunt \
  /private/tmp/marginplyr-hunt-20261107-verified
```

The original execution was session `27286`. It was retained through terminal
exit zero. The driver collected all 17 sequential child executions; no owned
child remained. It copied each harness to the output tree before execution,
so research outputs stayed outside the submitting worktree. Its working
directory was the archived source, and testthat parallel execution was disabled.

The adjacent replay-results.csv records the final script MD5, exact arguments,
expected exit and observed exit for each mode. Seven desired-contract modes
returned one for their intended failing assertion: five share families, one
ordinary-summary boundary and one Grouping boundary. Ten observation/control/
SQL modes returned zero. Every terminal status matched its expectation.
The retained logs contain the asserted failures rather than treating them as
successful repairs. The driver itself returned zero only after collecting all
children and checking every expected status.

The share observation established 420 comparisons and the separate control
mode 252. The summary matrix established 72 false collisions with matching
literal controls. The Grouping matrix and controls-only run each covered the
same 144 finite conditions; the latter repeated controls rather than adding
new independent cases. SQL modes established 204 numeric matches, 260 supported
name matches, twenty asserted upstream name refusals and six ordinary upstream
controls. Agent-owned additional minimal/cardinality/share-symptom runs are
separately documented in their lanes and were not presented as coordinator
replays.

## Earlier executions and setup exclusions

Before validating the delivered driver, the coordinator used sequential
scratch drivers retained as sessions `66641`, `18510` and `8109`; all ended
with zero after collecting their expected children. The first replay of the
summary matrix emitted a missing-Git-metadata diagnostic from its informational
SHA lookup in an archive. That lookup was guarded in the final harness; all
three summary modes were replayed with that exact final script. Output paths
in the SQL harnesses were made script-relative before the final replay.

The SQL lane describes earlier fixture, injection, ordering and syntax errors.
Those were harness setup work, not package defects. A sandboxed merge-tree
assessment initially failed while trying to write a temporary Git object;
the authorized Git operation completed separately. Two malformed CLI search
queries with embedded repository selectors were rejected; corrected explicit
repository searches supplied the actual absence evidence. No failed setup
request was silently counted as a result.

## Publication and verification boundary

The initial issue export contained 711 all-state bodies; PR bodies were fetched
with REST pagination. Relevant full bodies/comments were read by each lane.
The coordinator additionally read new #1418/#1419 and verified all seven
created issues' bodies and `bug`/`ready-for-agent` labels. Their independent
blocking status was explicit. Tracker exports and publication scratch files
were not committed.

The worktree began at fetched main, the tested revision above. A prepublication
refresh resolved to the same SHA and a permitted merge-tree assessment was
clean. The final submitted commit's fetched-main assessment and Review-ready
result were recorded on its PR, rather than assigning a future commit the
archive replay's package-code identity.

Only dated research prose, standalone harnesses and captured evidence were
submitted. No implementation, package regression test, installed artifact,
dependency, generated documentation, build exclusion or verification command
changed. The unchanged investigation exclusion and absence of package/runtime
consumers supported source scope, alongside explicit research link, parse and
digest/status checks. The mandatory repository consistency stage and six-check
baseline remained required. Package build/check, complete suite and snapshots
were omitted; no full-result timing proposal was produced. CI monitoring was
outside the request.
