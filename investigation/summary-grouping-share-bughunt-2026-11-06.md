# Bug hunt for Margin summaries, Grouping specifications, and contextual shares

Investigated: 2026-11-06
Repository snapshot: `952a0d9b18fd3ff4631f153432f6024ad62fcac2`

The investigation started from fetched main in the new worktree
`/private/tmp/marginplyr-summary-grouping-share-greedy-20261106`, on branch
`investigation/summary-grouping-share-greedy-20261106`. Four investigators
examined ordinary summary evaluation, Grouping selection and plans,
contextual-share dependency validation, and live backend results. Each lane
used a separate disposable archive of the tested SHA. A fetch immediately
before ticket publication still resolved main to that SHA.

The [November 5 summary hunt](summary-grouping-share-bughunt-2026-11-05.md),
the [November 4 contract hunt](margin-contracts-bug-hunt-2026-11-04.md), its
[follow-up](summary-spec-total-share-followup-2026-11-04.md), and their retained
reproductions supplied repaired controls and remaining boundaries. The lane
notes distinguish earlier failures from residual behavior after their repairs.
No previous investigation note was amended or superseded.

## Confirmed findings and tickets

| Area | Established effect | Independent ticket |
| --- | --- | --- |
| Margin summary | Expanding an unnamed data-frame subclass called its list conversion on actual row values, changing six to 99 or falsely rejecting the summary. Ordinary dplyr and named packing retained the actual values. | [#1365](https://github.com/sayuks/marginplyr/issues/1365) |
| Grouping specification | Standard stringr patterns with a metadata-dependent vctrs proxy method ran against integer name placeholders and falsely refused. Typed tidyselect and an identity wrapper selected the intended column. The earlier length-method repair remained effective. | [#1363](https://github.com/sayuks/marginplyr/issues/1363) |
| Parent and Total shares | Copying a locally defined closure, storing it in a literal list, or returning it from a literal factory lost its invocation-time dependency after binding removal, bypassing both dependency rules. | [#1361](https://github.com/sayuks/marginplyr/issues/1361) |
| Parent and Total shares | A nonliteral name in an inheriting rlang environment lookup bypassed both dependency rules, although the literal-name and equivalent base dynamic-lookup controls refused. | [#1362](https://github.com/sayuks/marginplyr/issues/1362) |
| Parent and Total shares | Equivalent explicit environment paths, including a literal nested function's parent traversal, bypassed the dependency rules. Direct literal access and equivalent base-lookup controls refused. | [#1368](https://github.com/sayuks/marginplyr/issues/1368) |

The user delegated ticket granularity and publication without a confirmation
checkpoint. The `to-tickets` workflow therefore produced one independently
verifiable repair slice per boundary, each with acceptance criteria, `bug`
and `ready-for-agent` labels, and no blockers. Ticket publication did not
authorize or include implementation repairs.

The [summary lane](2026-11-06-summary-grouping-share/summary/findings.md),
[Grouping lane](2026-11-06-summary-grouping-share/grouping/findings.md), and
[share lane](2026-11-06-summary-grouping-share/share/findings.md) record
minimal witnesses, competing hypotheses, separating controls, contracts,
environment and duplicate comparisons. Observation modes assert the dated
failures; `--assert` modes require the desired contract and intentionally
return nonzero on the investigated source. They are research harnesses,
rather than package regression tests or evidence of a repair.

## Additional completed comparisons

| Probe | Finite result |
| --- | --- |
| Ordinary summary callback/expansion behavior | 60 across comparisons, 50 dynamically changing unpacking comparisons and 169 ordered summary-type pairs produced no additional value or acceptance discrepancy. |
| Ordinary Grouping selection correspondence | 48 selections across six positions and four input classes gave 1,128 exact plan matches and 24 required empty-dimension refusals. This used a separately selected literal plan, not an independent family-algebra oracle. |
| Numeric shares and typed keys | 144 local/dtplyr operations covering 4,166 rows agreed on the finite numeric and denominator-key fixture matrix. |
| Randomized Total shares | 600 SQLite/DuckDB collections matched eager values across 30 seeded fixtures, five Grouping specifications and fixed/unfixed partitions, including missing, zero, negative, fractional and empty data. |
| Stateful SQL shares | 36 live PostgreSQL collections matched their displayed sequence-valued summaries and denominators across fixed keys, aggregation requests, sort requests and share kinds. |
| Typed DuckDB keys | 108 collections matched both shares for nine key types, fixed/variable placement, aggregation requests and sort requests. |

The [SQL lane](2026-11-06-summary-grouping-share/sql/findings.md) explains its
oracle normalization, ordinary SQL aggregate/type differences and private
PostgreSQL lifecycle. These counts describe finite, sometimes overlapping
probes. They establish no exhaustive guarantee for arbitrary R expressions,
unbounded Grouping plans, every S3 method, hosted services, all collations,
concurrency, interruption, other dependency versions or other platforms.

## Replay, publication and verification boundary

The coordinator independently replayed each retained finding in fresh R
processes from a disposable archive of the investigated main SHA. The
[replay results](2026-11-06-summary-grouping-share/coordinator/replay-results.csv)
and adjacent transcripts record the expected observation/contract status
pairs of 0/1. After separating contract assertions from dated failure
assertions, the final six-script replay completed through retained handle
`73300`, exit zero, with all 12 expected terminal statuses. The
[share harness controls](2026-11-06-summary-grouping-share/coordinator/share-harness-mode-controls.csv)
also replaced witnesses with existing correct-refusal controls in disposable
script copies: all four contract modes could return zero. That checked the
harness branches, not an implementation repair. All executions and the disposable
PostgreSQL server were collected before final verification.

Duplicate checking used live all-state Issue/PR searches and complete
title/body exports obtained with a limit of 2,000: 682 Issues and 674 PRs
were returned at that retrieval. The lane records name relevant queries,
matching reports, full-body/comment reads and successful repaired controls.
These exports were task-local scratch data; the retained note links the
reports that distinguish each finding. A post-publication check verified
the created issues and labels.

Replay by archiving the named SHA, changing into that source root and passing
the absolute path of the retained script to Rscript. The scripts load that
source through pkgload and disable testthat parallel execution. Environment
details are recorded per lane and in
[the backend environment](2026-11-06-summary-grouping-share/sql/environment.txt).

The submission added dated research prose, standalone R harnesses and captured
evidence under `investigation/`, which the unchanged source-package exclusions
omit. It changed no package code, package tests, dependency declaration,
installed file, generated documentation, generation source or verification
tool. These effects justified Review-ready `source`: package build/check,
the complete suite and snapshots were omitted. The submitted SHA, six-check
outcome, current-main compatibility assessment and review record belong on
the PR. CI monitoring was outside the requested scope.
