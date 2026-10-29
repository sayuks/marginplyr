# Boundary bug hunt: summaries, Grouping specifications, and contextual shares

Investigated: 2026-10-29
Repository snapshot: `4c63bb917ed7ed31ff6aadf060a5eae7a32cd004`
Branch: `codex/summary-spec-share-exhaustive-hunt`

A new managed worktree was created from local `main` at the snapshot above.
The investigation loaded that checkout through pkgload. Five investigators
split local summary evaluation, Grouping specification/inspection, contextual
shares, and backend boundaries, with independent SQL validation. The requested
scope was investigation and tickets; no package implementation, existing
package test, dependency metadata, or generated reference was changed.

The earlier notes [summary/spec/share](../summary-spec-share-bughunt-2026-10-29.md),
[summary/share follow-up](../summary-grouping-share-followup-bughunt-2026-10-29.md),
and [earlier backend and summary hunt](../2026-10-29-summary-grouping-share-bug-hunt/README.md)
guided the remaining boundaries. Their runs were not counted as new evidence,
and their historical text was not amended. Relevant issue bodies and comments
were read before distinguishing the remaining paths from closed repairs.

The user delegated ticket boundaries and publication, so the interactive
breakdown checkpoint in `to-tickets` was omitted. The published tickets carried
`bug` and `ready-for-agent`; none blocked another.

## Confirmed findings

| Finding | Ticket | Priority | Established effect |
| --- | --- | --- | --- |
| F1 | [#1041](https://github.com/sayuks/marginplyr/issues/1041) | P2 | A compatible unspecified/frame summary acquired an extra Assigned summary column and broke a rollup |
| F2 | [#1042](https://github.com/sayuks/marginplyr/issues/1042) | P2 | Nested lexical removal hid forbidden contextual-share dependencies |
| F3 | [#1044](https://github.com/sayuks/marginplyr/issues/1044) | P2 | Signed-plus and whitespace dot-dot SQL outputs were interpreted as R dot arguments |
| F4 | [#1045](https://github.com/sayuks/marginplyr/issues/1045) | P2 | Visible environment reparenting invalidated the external-lookup exemption without restoring dependency checks |

F1's ordinary grouped summary produced only `g` and `z`, with `z = NA, 5`.
The Margin summary added an all-missing Assigned summary column; the rollup
then refused its compatible Grand total branch. Explicitly named packing and
incompatible typed-missing controls distinguished this from closed #1020.
The [summary evidence](summary/README.md) records the minimized calls,
carrier-only causal control, per-group evaluation counts and acceptance scope.

F2 and F4 both admitted a later ordinary summary that should have been refused.
With displayed shares `.25, .75, 1`, it silently read the same-named input
payload and returned `100, 300, 400`. F2 concerned the static bound-name set
after nested `rm()`/`remove()`; F4 concerned an existing environment whose parent
was visibly changed to the active summary environment. The [share evidence](share/README.md)
separates the two state changes, both dependency directions, ordinary controls,
and the narrower earlier repairs #130, #162, #173 and #872. It does not demand
analysis of opaque arbitrary side effects.

F3 affected admitted names `..+0`, `..+1`, and `.. 1` on SQLite and native/portable
DuckDB. Ordinary summaries collected successfully. Generated bare references
raised non-positive-dot-index or incorrect-dot-context errors before reaching
the SQL engine. The already quoted `..-1` and `.env` controls passed. This was
the SQL boundary left outside the exact names exercised by #971 and the
local/dtplyr read repair in #1002. The [independent SQL evidence](backend/README-sql-signed-outputs.md)
records rendering, direct execution/materialization, lexical controls and the
bare-reference versus identifier translation probe.

## Coverage and negative evidence

These are bounded loops with different units. Their counts are not summed into
an exhaustive-input claim.

| Loop | New executed scope | Observation |
| --- | --- | --- |
| Grouping family algebra | 3,534 plans, including 2,463 duplicate plans | 20,133 assertions passed for ordered sets, identifiers and duplicate policies |
| Captured specification bindings | Nested kinds, ambiguity, parentheses, active/delayed bindings and injected quosure environments | 1,690 assertions passed |
| Arrow metadata | 22 physical fields, 320 comparisons over five input shapes | 366 assertions passed, including unconsumed readers |
| dtplyr typed metadata | 2,016 inspections over 21 graph forms, three source sizes and mutable/immutable roots | 6,049 assertions passed; 288 documented refusals, no inspection callback execution or source mutation |
| Local result shapes | 34 by 34 result pairs, named/unnamed: 2,312 comparisons | 260 equal successes, 2,040 paired refusals, 12 differences all belonging to F1 |
| Local expression forms | 50 forms over three sizes, prior-alias presence and naming: 600 comparisons | 548 equal successes, 52 paired refusals, no extra difference |
| F1 focused matrix | 48 affected layouts and 20 controls | All affected layouts reproduced; all controls passed |
| F2 scope removal | 38 forbidden dependencies and nine controls | All 38 refusals were missing; all controls passed |
| F4 environment mutation | 48 forbidden dependencies and eleven controls | All 48 refusals were missing; all controls passed |
| Typed share denominator oracle | 16 key representations, local/dtplyr, three rollups, fixed/unfixed: 192 attempts | 162 results and 8,828 scalar ratios passed; 30 ordinary no-share refusals, no additional share-only failure |
| Backend output-name sweep | 66 spellings, four roles, two sorts, two duplicate policies, three remote inputs: 3,168 attempts | 156 raw discrepancies; 96 F3 cases, 48 ordinary SQL name refusals, twelve ordinary ungrouped dtplyr `.SD` refusals |
| Independent SQL name replay | 1,080 name/role/plan/sort/content/adapter cases | 612 target-name rendering failures; 432 repaired-name cases and 93 ordinary/lexical controls passed |
| dtplyr `.SD` exclusion | 60 supported-boundary comparisons and 24 controls | All passed; the ordinary ungrouped Grand total refusal explained the broad-sweep signal |
| DuckDB typed-key/materialization oracle | Eleven available types, fixed/dimension roles, three sorts, two adapters, ordinary/Parent/Total summaries: 396 cases | No direct/computed discrepancy or displayed-denominator mismatch |

The [Grouping evidence](grouping/README.md) describes its exact finite spaces.
No new Grouping specification expansion/inspection finding was confirmed in
those spaces. Concurrent tickets #1038, #1039, #1040 and #1043 described distinct
qualified-head, factor-array and active-arithmetic boundaries that this lane
had not exercised; their full bodies and comments were read and those limits
were retained explicitly.

The broad name sweep's raw signals were diagnostic observations. Ordinary
SQLite/DuckDB controls reproduced the unsupported `...` output/identifier
restriction. The twelve `.SD` signals all included an ungrouped Grand total
whose ordinary dtplyr aggregate already failed. Keeping a fixed partition made each ordinary
branch and the Margin summary succeed, including both share roles and Margin
order; those raw signals were not promoted to another ticket. The
[focused exclusion](summary/dtplyr-sd-output-triage.md) records those controls.
The compact CSV preserves the discrepancy descriptions; internal error-object
binary dumps were omitted. Trailing whitespace was removed from text captures.

The typed-key loop included array, struct, map, blob, UUID, interval, timetz,
decimal, date, NaN and infinity keys. It checked Parent and Total shares against
displayed source values within missing-safe matching partitions, and compared
direct collection with direct materialization. The JSON fixture was refused
before the loop because the DuckDB JSON extension was unavailable. An initial
invocation stopped on that fixture; it was not counted as a completed loop.
The retained completed invocation recorded the fixture refusal and all 396
available-type cases. No external extension was installed.

## Replaying and interpreting failures

Run from the checkout root. Subdirectory notes provide the complete commands
and controls. The minimized desired-behavior loops include:

```sh
Rscript investigation/2026-10-29-summary-spec-share-exhaustive/summary/reproduce-unspecified-frame.R --assert
Rscript investigation/2026-10-29-summary-spec-share-exhaustive/share/scope-removal.R --assert
Rscript investigation/2026-10-29-summary-spec-share-exhaustive/backend/reproduce-sql-signed-outputs.R --assert
Rscript investigation/2026-10-29-summary-spec-share-exhaustive/share/environment-reparent.R --assert
```

Each deliberately exits 1 at the investigated snapshot. Report modes retain the
bad output and verify controls while exiting 0. These are red-capable
investigation signals, not package regression tests or claims that a repair
was implemented. The coordinator independently replayed the minimized calls;
`replay-status.json` and adjacent coordinator logs retain terminal outcomes.

The existing focused package suite exited 0:

```sh
NOT_CRAN=true Rscript -e 'data.table::setDTthreads(1L); devtools::test(filter="^(grouping-(interface|plan|backends)|summarize-operation|share(-backends)?|local-summary-types|share-lookup-environments|share-pronouns|share-computed-pronouns|margin-order|metamorphic-margin-semantics)$", reporter="summary")'
```

`existing-suite.log` retains its output. Package snapshots had no diff.
The runtime versions are in `versions.txt`. No live PostgreSQL, hosted SQL,
Spark, alternative dependency version, arbitrary S3 extension, interruption
stress or unbounded Grouping plan was established. The finite probes do not
prove absence of further defects.

## Publication boundary

All committed changes were dated investigation material under the package's
build exclusion. They were Repository-only under
`design/agents/local-checks.md`; the Package-affecting Review-ready check was
not required. Package-aware lintr ran after `load_all()`. Jarl, context-budget,
maintained-document references and whitespace checks were run before
publication; their terminal results are retained beside this note. The PR
carries the review snapshot and disposition record. CI monitoring was outside
the requested work.
