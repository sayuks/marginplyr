# Duplicate-repair source-scope boundary (#1193)

Investigated: 2026-11-03

This acceptance audit answered the Spec review's missing boundary evidence for
implementation SHA `344f09ba985e300a11306722992e6524b1e784fb`. Scope selection
was an AI judgment under *Boundary and scope* in
`design/agents/local-checks.md`; the runner did not infer scope from a diff.
The audit verified the factual inputs to that judgment using controlled copies,
rather than adding an automatic scope classifier.

## Method and observed cases

Environment: macOS Tahoe 26.6.2, R 4.6.1, testthat 3.3.2. The baseline was the
successful retained full SHA `1c26360d4c3fed4da9250b3870ca636dae23c925`, run
`20261103T122403Z-32687-7faf7928b8`. The audit archived that SHA to disposable
baseline/candidate directories and copied invocation/result/timing evidence.
It compared hashes of every tracked baseline file, called the implemented
proposal entry with its ordinary eligibility checks, and compared native
discovery. The original records' hashes were unchanged after all cases.

Run from the worktree root:

```sh
Rscript investigation/issue-1193-scope-boundary-2026-11-03.R
```

The command exited 0. No package tests, package checks or new timings were run;
all temporary copies were removed after collection.

| Controlled candidate difference from the full SHA | Observed evidence/discovery | Scope judgment under local checks |
| --- | --- | --- |
| Only remove later identical literal entries from DESCRIPTION priority | Every other tracked file matched; strict reader accepted; all 204 native filenames and their order matched; retained evidence remained eligible | `source` exception permitted |
| The same removal plus a byte change to `tests/testthat/helper-optional-backends.R` | Evidence refused with `test/helper content changed` | Reject exception; changed test/helper input requires `full` |
| The same removal plus a one-worker setting in native entry `tests/testthat.R` | Evidence refused with `test/helper content changed: tests/testthat.R` | Reject exception; dispatch and test input changed, requiring `full` |
| The same removal plus changing the runner's `TESTTHAT_CPUS` from four to one | Only DESCRIPTION and `tools/review-ready-check-lib.R` differed; proposal evidence remained eligible | Reject exception; dispatch machinery changed, requiring `full` |

The last case distinguishes proposal eligibility from the scope allowance:
valid historical measurements alone do not prove that a candidate has only
the permitted metadata change. The entire candidate diff must also satisfy
local checks. No new runner enforcement was assumed by this audit.

The implementation itself changed reader/runner/fixture code, and was checked
with `full` rather than claiming the metadata-only exception. Its full result
was 204/204 files and 0 ERROR/WARNING/NOTE; that result certifies `344f09ba`.
This later acceptance evidence did not change production behavior.
