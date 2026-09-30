# Competing-condition evidence after extending summary capture

Investigated: 2026-09-30
Source snapshot: ac8e7a2ca96ed90bc5fb13499573b1e26ed48d51
Scope: follow-up evidence for the partial #756 implementation

## Regression and correction

The Spec review of snapshot 7a90266 independently interrupted
`combine_margin_branches()` after all three warning-producing summary
evaluations. Under `warn = 2`, public `summarize_with_margins()` returned a
warning-conversion error instead of cancellation. The summary's exiting
interrupt catcher covered branch execution but ended before result assembly.
This reproduction did not involve a failing caller warning handler.

The additional public-operation regression in `test-competing-conditions.R`
interrupted combination and order restoration after the effects `2, 5, 7`,
under `warn = 1/2`. The test failed before commit 5222a71 extended the existing
catcher through result assembly. After that change, all 74 assertions in the
competing-condition file passed, including the four added configurations.
Input and options were unchanged and subsequent valid summaries succeeded.

## Repeated checkpoint measurement

The original supervisor was repeated on the clean source snapshot named above:

```sh
python3 tools/competing-conditions/run.py /private/tmp/marginplyr-756-acceptance-assembly
```

It terminated with exit status 0. All 30 worker verdicts and exit statuses
passed: ten controlled notifications, ten actual supervisor-delivered SIGINT,
and ten no-notification controls. The reached-checkpoint protocol, immediate
state assertions, and environment were the same as those recorded in
`investigation/competing-conditions-acceptance-2026-09-30.md`. Each worker's
environment, checkpoint identity, signal delivery where applicable, raw public
outcome, immediate state, and log were preserved byte-for-byte in
`investigation/competing-conditions-assembly-2026-09-30.rds`, alongside the
manifest and result summary. The 232-file archive was round-trip checked;
the ten actual-SIGINT outcome objects were also read through gzip connections
and checked as interrupts outside the error class.

The earlier archive was retained as evidence of its earlier source, not
replaced. The connection correction in that note's revisions section applies
to this archive too. These measurements did not establish actual second SIGINT,
native-statement or timeout cancellation, or another R/OS environment.
External warning-handler failure capture and complete public-contract
publication remained unresolved #756 acceptance.
