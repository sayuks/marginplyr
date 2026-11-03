# Maintainer-Mac FIFO Review-ready adoption

Investigated: 2026-11-03

## Revision and environment

The adopted runner revision was
`604f1aebfeb954234e8c672d773c5906670621ec` (main after PR #1163, #1156).
The dedicated branch `issue-1157-fifo-adoption` and worktree
`/Users/sayuks/Documents/marginplyr/.worktrees/issue-1157-fifo-adoption`
were created from that committed main revision. No runner, package, test,
dispatch or tracked priority code was changed for this adoption.

The host was `Sasakis-Mac-mini.local`, user `sayuks`, arm64, macOS 26.6.2
(25G83), Darwin 25.6.0. Tools were R 4.6.1 (2026-06-24), testthat 3.3.2,
filelock 1.0.3, processx 3.9.0, ps 1.9.3 and rcmdcheck 1.4.0.9000.

## Real-account drain

At 08:12:06 UTC, after the isolated fixtures had terminated, a process-command
snapshot found no Review-ready/check-tarball/R CMD check owner or waiter.
Under the existing FIFO metadata lock, `state.rds` had version 1, counter 2,
an empty request list and no invocation guard files. The existing account
heavy lock was acquired without waiting, then released; its inode was
107827163. The drain observation exited 0 through its original shell result.

No old execution needed cancellation. No unrelated session was signalled,
and no old worktree was inventoried or migrated. The installed launcher was
not recreated. Neither stable lock target nor the acceptance counter was
deleted or reset. A later old nonparticipating runner could still preserve
heavy exclusion without participating in FIFO; no fleet-wide upgrade was claimed.

## Isolated real-process evidence

The existing `tools/test-review-ready-runner.R` command/process fixtures were
executed on this Mac against the adopted revision, from 08:10:03 to
08:11:56 UTC. The retained outer `exec_command` session was **65217**,
with harness PID **20507**, and its terminal result was **exit 0**.
Its processx child handles were retained by the harness through waits and
cleanup. This was isolated fixture evidence, not a real marginplyr full result:
the fixtures used tiny committed packages, replacement spelling/pkgload/lintr
tools and jarl, a replacement account-home adapter, and isolated shared state.
Real Git clones/worktrees, R subprocesses, OS locks and package checks ran.

A disposable wrapper added observation only: it printed each start's PID and
worktree, saved each normally collected finish output/status, and printed
selected successful assertion labels. It did not change fixture assertions,
runner code or scheduling. The wrapper, transcript and PID logs were captured
under `/private/tmp/marginplyr-1157-evidence` and
`/private/tmp/marginplyr-1157-fixtures.log`; the fixture's temporary repositories
and owned children were collected by its existing cleanup. The observations
below preserve the useful evidence without making those temporary paths a
permanent result store. The uninstrumented suite remains the reproduction entry.

| Boundary | Observed evidence |
|---|---|
| Acceptance after baseline | Source and failing-baseline requests left queue state unchanged. A first launch blocked in spelling registered as 16 after the second launch registered as 15. |
| Clone/worktree FIFO | B in `separate-clone` (PID 22598), C in `linked-worktree` (22647), D in `another-worktree` (22700) registered as 5, 6, 7; all exited 0. Admission elapsed waits were 5.457, 10.526, 15.459 seconds, respectively. Barriers proved B alone held capacity and D neither overlapped nor overtook C. |
| Automatic admission / suspended owner | B was suspended while capacity was released and D arrived; C and D did not start. Resuming B admitted B, then releasing each fixture admitted C and D automatically. |
| Bounded waiting | The oldest waiter's descriptor samples at 1/2/4 seconds were 16/16/16. The FIFO follower's were 15/15/15. These finite observations support the bounded-wait regression, not an unbounded-duration measurement. |
| Waiting cancellation | PID 22538, sequence 4, returned 130 with `cancelled`, no capacity acquisition and no source-tarball check; releasing capacity could not start it later. |
| Dead waiter removal | Sequence 9 (PID 24964) cancelled with 130; killing head PID 24915 let sequence 10's earlier count fall 2 -> 1 -> 0. Killing middle PID 25133 let sequence 13 fall 2 -> 1 while its live head remained; cancelling that head made it 0. Surviving test waiters were then cancelled and collected through their handles. |
| Fresh ownership / state safety | Retry sequence 14 had a fresh identity; an ended interrupted-registration guard was reclaimed. Corrupt/incompatible state and ambiguous live ownership failed visibly; retention preserved sequencing and live coordination. |
| Ordinary owned-child cleanup | PID 26048, sequence 18, returned 130. Its fixture acquired the released heavy lock only after the observed child had ended. A live inspection error also kept capacity occupied until owned children were collected, then returned the original failure. |

For abrupt running-parent SIGKILL (PID 26753), only **3/15** observed
descendants had stopped after one second. The harness explicitly stopped and
waited for survivors before completion. This measurement does not establish
strict crash-time exclusion; capacity may become available before every
descendant has stopped. It differs from ordinary cancellation's cleanup contract.

## Real marginplyr full result

The clean committed worktree ran:

```sh
Rscript tools/review-ready.R --scope full --reason 'Issue #1157 requires actual maintainer-Mac adoption evidence for the committed FIFO runner, including all five baselines and one complete source-tarball check; process fixtures alone cannot certify adoption.'
```

The original retained `exec_command` session was **96586**, runner PID
**29779**, started **2026-11-03T08:12:23.687858Z**. That same handle was polled
through baseline, admission, package checking, cleanup and terminal **exit 0**;
no replacement invocation supplied its result. The submitted SHA was the
adopted revision above. The real account allocated sequence **3**, with zero
earlier waiting requests and automatic admission after **0.015 seconds**.

| Mandatory baseline | Outcome |
|---|---|
| Mechanical test dependency-declaration scan | Passed |
| Required optional test Suggests presence/version | Passed |
| Package spelling | Passed |
| `jarl check .` | Passed, `All checks passed!` |
| Package-aware lintr after `pkgload::load_all()` | Passed, `No lints found.` |

The source tarball was built once and checked once with `R CMD check --as-cran`.
The native transcript announced **4 test processes**; progress completed
**201/201 files**, including the complete suite and snapshots. Its test summary
was **FAIL 0 / WARN 0 / SKIP 0 / PASS 175884**. Examples, vignette rebuilds,
and PDF/HTML manual checks passed. The check process returned **0**, timeout
**FALSE**, **0 ERRORs / 0 WARNINGs / 0 NOTEs**, `Status: OK`. No NOTE disposition
was needed. `result.rds` recorded `status = passed`, `heavy_check = passed` for
the same SHA and reason. Ordinary committed-priority `test_check()` dispatch
was used; no coverage run or native-crash retry was added.

Artifacts remained under the submitting worktree's
`.review-ready/runs/20261103T081223Z-29779-745310c5db2/`: `invocation.rds`,
`console.log`, `result.rds`, `source.tar`, `source/`,
`marginplyr_0.1.0.tar.gz`, `check-result.rds`,
`check/marginplyr.Rcheck/00check.log`, `check/marginplyr.Rcheck/tests/testthat.Rout`,
progress/timing data and run-local temporary/cache files. The ordinary timing
recorder also reported eligible evidence; it was not an adoption prerequisite.
No output was moved into shared coordination or exempted from the existing
seven-day eligibility policy from invocation start.

### Real-account handoff and cleanup

At 08:16:22 UTC the shared state showed sequence 3 active, guarded by
`request-10fe45a05a891acd0017e72c541a5d8f-29779-1793693543.313690.lock`.
After the successful terminal result, an initial all-account-empty probe
exited 1 at 08:26:23 because another worktree had started. The targeted
ownership observation at **08:26:58 UTC** exited **0**: sequence 3's record
and guard were gone, the counter had advanced to **4**, and the same heavy-lock
inode **107827163** remained. Sequence 4 was active under PID **49162**.
No attempt was made to acquire its occupied heavy lock or stop its children.

That live invocation's known console path was
`/private/tmp/marginplyr-1158/.review-ready/runs/20261103T082425Z-49162-c00a229e9fbf/console.log`.
Read-only observation found submitted SHA
`51453fa4327f5a323d4c5661a24dba8e23adff0c`, accepted sequence **4**, and
`Heavy package-check capacity acquired. FIFO sequence 4; elapsed wait 65.990 seconds.`
This supplied an additional actual-account cross-worktree handoff observation
after our sequence 3 released capacity. The other invocation retained its own
execution owner; neither its original handle nor its terminal check result
was claimed by this adoption record. Its active record was left intact.

## Investigation during the real full invocation

A separate disposable Git archive of the submitted SHA was used for static
investigation. The hypotheses were admission before baseline, overtaking a
suspended owner or unsafe dead-owner pruning, ordinary release before child
collection, and accidental dependence on timing setup. The runner's call order
and ownership cleanup, with the process evidence above, refuted those concerns
at the tested boundaries. No additional focused test or required fix was found.
The original full handle supplied the terminal result above. No additional
investigation execution or owned child work remained at that transition.

## Limits

The process fixtures established cross-clone/worktree contention on this Mac
with isolated state. The real-account drain and real full result established
operational adoption separately; they did not run multiple complete marginplyr
suites solely to reproduce the fixture schedule. No timing collector deployment,
artifact retrieval, priority update, local history database or dedicated timing
CI was required for FIFO cutover. Ordinary `test_check()` and committed test
priorities were preserved. Package portability remained CI's separate concern.
