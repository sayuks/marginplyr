# Adversarial nesting and Grouping specification follow-up

Investigated: 2026-11-05
Tested package SHA: `0403c9150394cbae21b59f34c75791d0873b8451`
Environment: R 4.6.1, aarch64-apple-darwin23, macOS Tahoe 26.6.2

Four investigators used a new worktree from fetched `origin/main` to examine
`nest_with_margins()`, `nest_by_with_margins()`, all five Grouping
specification constructors and `inspect_grouping()`. The earlier
[November 5 boundary hunt](../2026-11-05-nesting-grouping-spec-hunt/README.md),
[November 4 boundary hunt](../2026-11-04-nesting-grouping-boundaries/README.md),
[nesting follow-up](../2026-11-04-nesting-grouping-followup/README.md) and
[greedy hunt](../2026-11-04-nest-spec-greedy/README.md) supplied starting
counterexamples and controls. The tested SHA included their completed repairs.

## Findings and tickets

The two findings were published in English with `bug` and `ready-for-agent`.
Each repair could be implemented and verified independently, with no blocking
edges. The user delegated ticket granularity and publication. No package
implementation or permanent regression test was changed.

| Issue | Observed failure | Evidence |
| --- | --- | --- |
| [#1341](https://github.com/sayuks/marginplyr/issues/1341) | A full join's generated `merge()` accepted caller dispatch during typed metadata acquisition and silently removed a real Grouping dimension or fixed key. | [Backend finding and controls](backend/README.md) |
| [#1342](https://github.com/sayuks/marginplyr/issues/1342) | A provably duplicate product prefix followed by a two-set suffix reached caller predicates, a dtplyr metadata refusal, or a DuckDB metadata statement before duplicate rejection. | [Specification finding and controls](spec/README.md) |

### Full-join metadata changed source partitions

An immutable dtplyr self full join had two character keys, `a` and `b`.
A caller's `merge` binding delegated to the native data.table method and
changed the key's type only when given zero rows. Explicit native collection
retained both character keys. Name-only inspection selected the key without
invoking that binding. Typed inspection instead reported the empty grouping
set, and each Nest verb returned one keyless cell instead of two keyed cells.
Typed fixed-key selection failed through the same boundary. Removing only
the binding restored all three results; no caller table changed.

The ranked hypotheses separated generated full-join dispatch, an environment
mismatch, and incorrect selector behavior. The same binding saw real rows
during native collection and empty roots during metadata acquisition. The
unmasked and other-join controls isolated the full-join safety branch.
ADR 0020's dtplyr amendment required faithful metadata or safe refusal.
The earlier bracket/head repair in #1326 and PR #1333 did not cover this
separate generated merge call.

### A two-set suffix concealed a known duplicate

The first product factor contained the same singleton set twice. Its suffix
contained a typed-selection grouping set and an empty grouping set. Each
suffix branch always contributed one occurrence, so pairs 1/3 and 2/4 were
equal regardless of the predicate's answer. Local inspection and both Nest
verbs nevertheless invoked the predicate twice before duplicate rejection.
Unsafe derived dtplyr input received the metadata-safety refusal instead.
Live DuckDB inspection sent one `LIMIT 0` statement, observed at the actual
DBI boundary; its Sent query record separately showed `selection_proxy`.
The singleton typed suffix and two-set literal suffix rejected before that
work. No upstream dtplyr source callback ran.

The ranked hypotheses separated missing multi-set suffix evidence, required
selection order, and another preflight layer. The controls supported the
first: incomplete-prefix evidence was limited to `single_suffix()`.
ADR 0005 required locally decidable errors before metadata acquisition.
#1266, #1305 and PR #1340 covered the related single-set suffix cases.

## Completed finite coverage

| Boundary | Outcome and scope |
| --- | --- |
| [Structured local keys](local/README.md) | 816 calls: 768 source-membership matches and 48 documented raw typed-missing refusals. Seventeen families crossed empty/populated input, fixed keys, keep choices, both verbs and sort choices. |
| [Extension vectors](local/README.md) | 504 calls matched cell membership and source-value projections for labelled/SPSS/tagged-NA, blob, record, list-of and units vectors. Sources remained unchanged. |
| [Deep family algebra](spec/deep-family-algebra.R) | 1,500 deterministic trees, at most six constructor levels and 64 pre-deduplication occurrences, produced 9,000 exact inspection or duplicate-refusal matches across three policies and two fixed-key routes. All five constructor kinds had 300 roots. |
| [Join dispatch matrix](backend/merge-frontier-controls.R) | 72 observations across six join styles, caller binding presence, two typed selector positions and three consumers. Six masked full-join cases failed; the other 66 passed the selected-key and group-count checks. |
| [Product preflight matrix](spec/non-singleton-suffix-boundaries.R) | 21 observations: 14 matching controls and seven failures, comprising three local predicate evaluations, three dtplyr diagnostic replacements and one actual DuckDB metadata statement. |
| [sf disposition](local/README.md#sf-display-conversion-boundary-not-promoted-to-a-finding) | Eight active-geometry display failures also failed the equivalent native dplyr composition; sixteen fixed/payload controls succeeded. This did not establish an additional package defect under ADR 0016. |
| [Known product resource frontier](spec/README.md#known-resource-frontier-reproduced-without-a-new-bug-claim) | At width 20, 21 final sets still exceeded a 96 MiB vector heap; the width-eight nine-set control matched exactly. PR #1250 had already recorded this distinct performance frontier. No duplicate bug ticket was opened. |

The local oracle independently grouped original keys with vctrs and checked
cell row identifiers, with source serialization before and after each call.
It exercised sorting for membership interference rather than independently
establishing every Margin-order term or outer display value. The algebra
oracle calculated unions, prefixes, subsets and products without using
marginplyr's compiler or mask helpers. Its successful plans included exact
fixed/included/omitted columns, dimension order, named Grouping bits, Grouping
set identifiers and Grouping identifiers. Algebra correctness did not
establish preflight timing, which the positive product finding measured
separately.

## Independent replay and duplicate checks

The coordinator extracted a separate Git archive of the tested SHA and
replayed both findings there. The full-join desired-behavior invocation,
terminal session `73580`, exited 1 after native, name-only, unmasked and
source-immutability controls passed. The product observation invocation,
session `13463`, exited 0 after asserting the 14-control/seven-failure
matrix and measuring the actual DuckDB statement. Its desired-behavior
invocation exited 1. All handles reached terminal completion.

The prior November 5 `classed-scalars.R --assert` replay also ran in that
archive. Session `54237` exited 0 across `.id`, `.key`, Option argument and
inspection-format controls. Those previously reported scalar failures were
not filed again.

All-state issue and PR searches, with saved query snapshots in the track
directories, covered full joins, caller merge dispatch, product suffixes,
guaranteed duplicates and the adjacent earlier reports. Additional
coordinator searches used `"full_join" "metadata"` for both issues and PRs,
and `"merge" "binding" "dtplyr"` for issues. The first two returned no
matches. The last returned #1033, #717, #301, #778 and #303, whose subjects
did not describe full-join typed metadata. The track notes distinguish the
closest reports. Fetching `origin/main` again immediately before publication
returned the same tested SHA, so both findings were present on that main.

## Replay

Run one command at a time from a checkout of the tested source. CSV-producing
probes default to temporary output; the checked-in CSVs and logs are the dated
observations. Both finding probes offer `--assert` to require desired behavior
and intentionally exit 1 on the investigated revision.
Stored log captures omit trailing horizontal whitespace; CSV values retain
their captured contents. `MANIFEST.sha256` identifies the retained files.

```sh
evidence=investigation/2026-11-05-nest-grouping-adversarial-followup
Rscript "$evidence/backend/full-join-merge.R"
Rscript "$evidence/backend/full-join-merge.R" --assert
Rscript "$evidence/backend/merge-frontier-controls.R"
Rscript "$evidence/spec/non-singleton-suffix-boundaries.R"
Rscript "$evidence/spec/non-singleton-suffix-boundaries.R" --assert
Rscript "$evidence/spec/deep-family-algebra.R"
Rscript "$evidence/local/local-structured.R"
Rscript "$evidence/local/local-extension.R"
Rscript "$evidence/local/sf-disposition.R"
R_MAX_VSIZE=96M Rscript "$evidence/spec/optional-prefix-small-output.R" 8
R_MAX_VSIZE=96M Rscript "$evidence/spec/optional-prefix-small-output.R" 20
```

The probes used dplyr 1.2.1, tidyr 1.3.2, tibble 3.3.1, rlang 1.3.0,
tidyselect 1.2.1, vctrs 0.7.3, dtplyr 1.3.3, data.table 1.18.6.1,
DBI 1.3.0, DuckDB 1.5.5, haven 2.5.5, blob 1.3.0, units 1.0.1 and sf
1.1.2. The finite probes did not establish behavior for arbitrary S3/S4
methods, all possible selection/query graphs, hosted backends, other library
versions or other operating systems. No fix or post-fix result was claimed.

Only dated evidence under the existing investigation exclusion was submitted.
Package code, tests, helpers, generated documentation, installed files,
dependencies, exclusions and check machinery retained their baseline bytes.
The source-scope Review-ready result, submitted revision and main compatibility
assessment belong to the PR's verification record.
