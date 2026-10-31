# Review-ready evidence storage after PR #1115

Investigated: 2026-11-01

## Scope and method

This note examined [PR #1115][pr] at merged commit
`be9e81e9c3d977b8a2607f3971468b5acf64faf1`, extending the
[earlier file inventory](review-ready-file-inventory-2026-11-01.md), which had
examined `a2820e0faf42472dcb714a2e7355e081a15122c6`. The inspection used
source and documentation reads. No check, fixture, installation, recovery,
render, or data deletion was executed.

GitHub's issue and pull-request queries for `sayuks/marginplyr#1116` returned
not found during this investigation. The latest merged pull request returned
was #1115. This note identified its source by its confirmed number and SHA;
it did not attribute those changes to #1116.

## Implemented behavior affecting the replacement design

PR #1115 had implemented the evidence reuse specified by ADR 0038. It retained
cumulative verification obligations and separately planned execution or reuse.
`full_required()` required the shared Full slot for actual Full build/check
execution, rather than for every cumulative Full selection. Reused obligations
still required current common checks and affected-artifact contracts.
[evidence], [admission], [operation]

Only an ancestor's actual executing origin could discharge a stage. Repeated
reuse preserved that origin, including across clones. The implementation
checked registered sources and environment, timestamp conditions, terminal
outcome, cleanup, NOTE policy, and retry details. Its actual identity-probe,
documentation-proof, and gate child environments removed registered
known-format Codex and Claude execution IDs while keeping unknown values,
configuration, and supervisor transport. [evidence], [operation]

## Additional generated artifacts

`S` meant the OS account's shared
`.local/state/marginplyr/review-ready/` directory. `Q` meant
`S/requests/<request-id>/`. `G` meant the R gate's private temporary workspace.
The table covered additions to the earlier inventory; its existing source,
render, backend, diagnostic, socket, and fixture families still applied.

| Artifact | Producer and observed source behavior | Cleanup candidate |
| --- | --- | --- |
| `S/evidence.key` | `signing_key()` created 32 OS-random bytes with exclusive creation, owner-only permissions, and owner/length validation; no rotation or deletion route existed | Requires an explicit key lifecycle; deleting it alone invalidates existing authentication |
| `S/evidence.lock` | `append_evidence()` and `complete_observation_index()` serialized issuance with an OS lock; the empty target remained | Keep while that lock protocol operates; retire only with the drained protocol |
| `S/evidence-index.json`, `S/evidence-index.new` | Complete authenticated issuance inventory and increasing sequence; `save()` used a replacement intermediate | Retire entries and interrupted intermediates through coordinated store maintenance |
| `Q/observation.json`, `Q/observation.new` | Pre-cleanup stage observations, including failed and unavailable execution; recovery could finish an interrupted observation-index publication | Retained evidence-store history; age alone cannot remove its failure veto safely |
| `Q/evidence.json`, `Q/evidence.new` | Terminal evidence after confirmed cleanup and capacity release; only successful eligible executing units could supply reuse | Retained with the request, subject to origin/dependent retirement |
| `Q/workspace/stage-report.json`, `stage-report.json.new` | R's `review_ready_report()` wrote a replacement report; the worker removed an old report before starting | Remove with the owned workspace after stopping writers and retaining required observations |
| `Q/workspace/worker-result.json` | Worker wrote status, before/after identities, repository, and stage report directly; no `.new` sibling was registered | Remove with the owned workspace after preserving recovery observations |
| `G/timestamp-inventory/**` | `review_ready_timestamps()` expanded the source tarball to inspect file timestamps | Remove with the gate's owned temporary workspace |
| `tempdir()/review-ready-r-implementation-*` | The environment probe's `checksum()` wrote deparsed R checker functions and removed each with `on.exit()` | Owned temporary files, including interrupted-probe remnants |

Sources: [evidence], [admission], [gate library], [environment probe].

The inspected implementation did not create an `evidence-input.json`, a
separate evidence database, or a new installed evidence-module file. Evidence
source was embedded into the existing standalone command and private
supervisor copy. Its Python fixture imports could create bytecode within the
already inventoried `__pycache__` family. [evidence], [admission], [fixtures]

The additional fixtures used private temporary roots containing synthetic Git
repositories/clones, copied tracked sources, synthetic installed-package
files, external configuration inputs, isolated state with all evidence files
above, `execution-environment.json`, `dependency`, and `mutate-environment`.
Cleanup targeted their owned root after collecting owned processing. Strings
such as `fixture-environment.sh`, `changed-environment.sh`, and `claude-profile`
were environment values, with no corresponding creation operation found in
these fixtures. [fixtures], [lifecycle fixtures]

## Why deletion needs evidence-store maintenance

`records()` required equality between the signed issuance inventory and all
available evidence/observation files. Reading a record also required its
associated `record.json` and referenced original evidence and observations.
Removing one request directory without updating that structure disabled reuse
through completeness checks. Missing cache data scheduled execution rather
than omission; interrupted publication could still require recovery.
[evidence], [operation]

`execution_is_current()` used stage observation sequence, independent of wall
clock and delayed cleanup, to reject older successes after failed or
unavailable execution. Unknown source or environment could veto a broader set
of origins. Merely deleting a failure and its index entry could consequently
make an older success appear eligible. [evidence]

A terminal-time retention rule could expire that failure first: a success
observed at sequence 1 might finish cleanup after a failure observed at
sequence 2. Removing sequence 2 at its retention boundary therefore also
needed to retire the earlier success's affected reusable units, even if that
success's terminal history had not expired. Keeping its raw outcome/log for
the remainder of its retention period did not require keeping it reusable.
This was a consequence derived from the inspected ordering and validation
rules, not an executed GC experiment. [evidence]

No collector, retirement transaction, or key-rotation API was present at the
inspected SHA. The proposed replacement needed those designs if it combined
the accepted seven-day storage policy with preserved stage reuse. A safe
design could remove a failure together with the origins it still vetoed,
rather than accumulating permanent failure tombstones. It also needed to
handle missing origins, dependent references, concurrent validation/issuance,
active requests, and interrupted maintenance. These were design constraints,
not claims that the baseline implemented them. [evidence], [admission]

## R cryptographic components

The local R 4.6.1 `tools` namespace exposed `sha256sum()`, including a raw-byte
input, but no corresponding HMAC or cryptographically secure random-byte API
was identified. Hashing alone did not replace the Python implementation's
HMAC-SHA256 authentication and `os.urandom(32)` key generation. [base hash],
[evidence]

The OpenSSL R package documented SHA-256 on raw inputs, HMAC-SHA256 through
the same function with a non-null key, and `rand_bytes()` for cryptographically
secure random bytes. Those APIs covered the concrete authentication and
key-generation needs without passing a key through an external command's
arguments. Local `openssl` 2.4.2 metadata declared `askpass` as an Import,
with `sys` behind that dependency, and OpenSSL >= 1.0.2 as a system requirement.
Installed availability was not an adoption decision. [openssl hash],
[openssl random]

`digest::hmac()` also exposed HMAC-SHA256 with an explicit algorithm selection;
local digest 0.6.39 imported only base `utils`. However, digest's own
DESCRIPTION advised using a comprehensive cryptographic library such as
OpenSSL for cryptographic purposes. Digest also did not supply the required
secure-random API. These findings favored considering the OpenSSL R package
as the additional focused component. [digest description], [digest hmac]

Adopting that package would not by itself establish the complete evidence
protocol. The Python baseline authenticated canonical JSON bytes with sorted
keys, compact separators, and ASCII escaping, and compared authentication codes
with `hmac.compare_digest()`. An R replacement needed an explicit versioned
encoding and authentication-verification contract with fixtures. No matching
timing-safe comparison API was identified in the reviewed OpenSSL R public
documentation; this was a remaining implementation question, not evidence that
ordinary string equality preserved the existing mechanism. [evidence],
[openssl hash]

[pr]: https://github.com/sayuks/marginplyr/pull/1115
[evidence]: https://github.com/sayuks/marginplyr/blob/be9e81e9c3d977b8a2607f3971468b5acf64faf1/tools/review-ready-evidence.py
[admission]: https://github.com/sayuks/marginplyr/blob/be9e81e9c3d977b8a2607f3971468b5acf64faf1/tools/review-ready-admission.py
[operation]: https://github.com/sayuks/marginplyr/blob/be9e81e9c3d977b8a2607f3971468b5acf64faf1/design/agents/local-checks.md
[gate library]: https://github.com/sayuks/marginplyr/blob/be9e81e9c3d977b8a2607f3971468b5acf64faf1/tools/review-ready-check-lib.R
[environment probe]: https://github.com/sayuks/marginplyr/blob/be9e81e9c3d977b8a2607f3971468b5acf64faf1/tools/review-ready-environment.R
[fixtures]: https://github.com/sayuks/marginplyr/blob/be9e81e9c3d977b8a2607f3971468b5acf64faf1/tools/test-review-ready-evidence.py
[lifecycle fixtures]: https://github.com/sayuks/marginplyr/blob/be9e81e9c3d977b8a2607f3971468b5acf64faf1/tools/test-review-ready-admission.py
[base hash]: https://stat.ethz.ch/R-manual/R-devel/library/tools/html/sha256sum.html
[openssl hash]: https://raw.githubusercontent.com/jeroen/openssl/master/man/hash.Rd
[openssl random]: https://raw.githubusercontent.com/jeroen/openssl/master/R/rand.R
[digest description]: https://raw.githubusercontent.com/eddelbuettel/digest/master/DESCRIPTION
[digest hmac]: https://eddelbuettel.github.io/digest/man/hmac/
