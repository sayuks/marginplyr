# Grouping selection warning handlers and the metadata boundary

Investigated: 2026-11-04
Baseline: `78ebbc36200bdc5cef70838dd4e772c5c4a1401c` (`main`)

This track established one residual preparation defect after #1184/#1198:
a warning promoted to an error by the caller's calling handler, with an ordinary
warning option, passed the early-selection boundary. No package implementation
or permanent regression test was changed. Every execution used an independent
disposable archive of the baseline and reached terminal completion.

## Readings and candidate boundaries

The readings included `CLAUDE.md`, `AGENTS.md`, `CONTEXT.md`, ADRs 0005, 0007,
0008, 0013, 0015 and 0026, the specification constructors, the compiler and its
selection classifier, and the relevant leading-error and evaluation-budget
tests. The October 30 lifecycle, October 31 closure/identity, November 2
selection-effect and November 3 lexical/dots/selection-frontier records informed
the additional boundaries; their comparison counts were not reused as new
evidence. The November 3 warning-promotion finding and the complete body and
comments of #1184 and PR #1198 were read before judging this residual report.

The remaining distinction was how a warning became an error: R's default
handler under `warn = 2`, or an ordinary caller's `withCallingHandlers()` warning
handler calling `stop()` on its own condition under `warn = 0` or `warn = 1`.
The same original selector, caller condition and input names supplied direct
tidyselect, local, fixed-key and lazy-input controls.

## Confirmed: caller-promoted errors arrived after metadata or were replaced

With `options(warn = 0)`, the minimal warning handler was
`warning = function(cnd) stop(promoted)`, where `promoted` was a condition
inheriting a caller-owned error class. The selection was `one_of("absent")`.

- Direct typed tidyselect, local inspection and a live DuckDB fixed `.by`
  returned that exact original condition object.
- Live DuckDB inspection with `.grouping = grouping_set(one_of("absent"))`
  invoked the actual DBI `dbSendQuery()` method once, sending a zero-row query
  against the input, before returning that same error. The fixed-key control
  invoked it zero times.
- Inspection of a derived dtplyr step with the same first selection and a later
  `where(is.numeric)` returned the metadata-safety Package condition instead.
  The caller's warning handler never received the warning, so its original
  error was lost and the message advised collection that could not fix the
  absent column.

`handler-promotion.R` was the minimized replay. Its observation mode exited
zero. Its `--assert` mode exited one at `identical(typed, promoted) is not TRUE`,
with the extra DuckDB query also printed. The assertion requires both exact
condition preservation and zero input-metadata queries, so it can distinguish a
repair from merely replacing one diagnostic with another. The script restores
options, removes its DBI trace and disconnects its database on exit.

### Contract, cause and separating controls

ADR 0005 requires an error decidable from the call and locally available input
metadata to precede backend contact. `one_of("absent")` is classified as
names-only. ADR 0015 and the *External condition* glossary entry require the
caller's error to propagate unchanged. This finding rests on that preparation
ordering, not on a claim that all zero-row metadata queries are forbidden:
ADR 0020 permits the appropriate metadata read after locally decidable errors.

The ranked hypotheses and separating observations were:

| Candidate | Observation and disposition |
| --- | --- |
| Ordinary-warning suppression hid the caller's early failure | Confirmed: `with_discarded_selections()` used `suppressWarnings()` when `warn < 2`; changing only `warn` to 2 preserved the caller error before metadata |
| Capturing or propagating the caller condition changed it | Refuted: direct, local, fixed-key and all surviving DuckDB errors were `identical()` to the original object, including its call, token and nested payload |
| The first selection required typed input data | Refuted: the same missing-name selector failed before metadata as `.by`, and the `warn = 2` counterpart failed before metadata in every supported consumer |

The later typed predicate did not run in any stopping-handler case. No package
instrumentation or repair was added.

### Completed matrix

`handler-promotion-matrix.R` completed 390 cases across all five constructors,
warning options 0/1/2, stopping and nonstopping-muffling calling handlers, and
inspection, both nesting verbs, expansion and summarization. Local and dtplyr
inputs covered all five consumers; live DuckDB covered its three supported
consumers (inspection, expansion and summarization). SQL nesting was excluded
because it is refused at admission, before this preparation boundary.

Of 195 stopping-handler cases:

- 50 derived-dtplyr cases under warning options 0/1 replaced the original error
  with the metadata refusal, with zero handler deliveries;
- 30 live DuckDB cases under those options made one avoidable actual input
  metadata query each before preserving the exact caller error;
- all 65 warning-option-2 controls preserved the exact caller error before
  metadata, including the 15 DuckDB controls with zero queries; and
- the remaining 50 local controls preserved the exact caller error.

All 195 nonstopping-muffling controls succeeded, delivered the selection warning
once and evaluated the typed predicate twice, once for each input column. The
45 valid DuckDB controls acquired metadata once. This preserved the important
control that allowing an ordinary warning to continue must not duplicate its
delivery or the caller's typed selection work.

The final matrix exited zero and its CSV retained every case. An initial
development matrix attempted SQL nesting too; those 60 admission refusals were
excluded rather than counted as evidence about the selection boundary.

## Warning identity frontier without metadata failures

`warning-handler-frontier.R` completed 600 local comparisons against direct
typed tidyselect. Five selector forms produced one warning, distinct warnings,
or repeated identical warning occurrences, with or without a selected column.
The positions were all five constructors, absent/typed `.by`, four warning
options (-1/0/1/2), and muffling, escalating or continuing caller handlers.

There were 492 exact warning class/message/call sequence and error-class
matches. The other 108 cases were empty rollup/cube selections under ordinary
warning options: the existing quiet discarded pass raised the required empty
unit Package condition before canonical warning delivery. Those were recorded
as `quiet_empty_unit_refusal`, not promoted as a new defect. The first oracle
incorrectly demanded native warning delivery before that already-established
empty-unit refusal; correcting that expectation accounted for all 600 cases.
The final command exited zero. These successful local comparisons did not prove
the metadata ordering that the separate live/derived-input reproductions failed.

## Duplicate search and recommended slice

`duplicate-search.txt` retains all-state issue searches for `warning handler
metadata`, `calling handler grouping` and `warning promoted`, plus the all-state
PR search `warning preflight`. The complete closest bodies/comments are retained
in `issue-1184.json` and `pr-1198.json`.

#1184's concrete request named `options(warn = 2)`; PR #1198 implemented and
verified that route, including caller-muffled warnings under that option.
Its broader acceptance wording about warning-promoted errors also explains why
this residual belongs to the same preparation contract. The closed repair did
not establish the ordinary-warning-option route. No separate matching issue
for a calling handler promoting a selector warning under option 0/1 was found.

Recommended follow-up: **[P2] Preserve caller-promoted Grouping selection errors
before metadata acquisition**, with no blockers. It should cover the shared
preparation seam across its supported consumers, preserve the original caller
condition, avoid input metadata contact for the names-only error and keep
nonstopping warning counts and predicate counts unchanged. No prefactoring
prerequisite or dependency on the separate nesting findings was established.
The coordinator owned final current-main confirmation and publication.

## Replay, environment and limits

From the investigated checkout root:

```sh
Rscript investigation/2026-11-04-nest-grouping-bug-hunt/spec/handler-promotion.R
Rscript investigation/2026-11-04-nest-grouping-bug-hunt/spec/handler-promotion.R --assert
Rscript investigation/2026-11-04-nest-grouping-bug-hunt/spec/handler-promotion-matrix.R
Rscript investigation/2026-11-04-nest-grouping-bug-hunt/spec/warning-handler-frontier.R
```

Each script accepts `MARGINPLYR_HUNT_SOURCE` for an explicit disposable source.
The matrix accepts `MARGINPLYR_HUNT_OUTPUT` for CSV placement; the frontier
accepts a CSV filename as its first argument. Adjacent text records retain
terminal output and `sessionInfo()`. R 4.6.1 ran on aarch64 macOS Tahoe 26.6.2
with dplyr 1.2.1, tidyselect 1.2.1, rlang 1.3.0, dtplyr 1.3.3,
data.table 1.18.6.1, DuckDB 1.5.5, dbplyr 2.6.0 and DBI 1.3.0.

These were finite focused reproductions, not a complete package suite or a
Review-ready result. Other warning restarts, arbitrary handler mutation or
reentrancy, other dependency/R/OS versions, hosted SQL services and unbounded
specification trees were outside this evidence. The investigation performed
no package build, installation, render, implementation repair or permanent test
change.
