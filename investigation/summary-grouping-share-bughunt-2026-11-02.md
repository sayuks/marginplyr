# Bug hunt: summaries, Grouping specifications, and contextual shares

Investigated: 2026-11-02
Repository snapshot: `978d7bef4247bf1348af5b90bfab47924db25500`

The investigation began in a new worktree created from fetched main, on
`codex/bughunt-summary-grouping-share-20261102`. A later fetch before ticket
preparation still resolved to the snapshot above. Four investigators divided
ordinary summary evaluation, Grouping specifications, contextual-share scope,
and SQL execution. The coordinator independently replayed all four findings.

The October 29–31 investigations guided the remaining boundaries, especially
the [October 30 follow-up](summary-spec-total-share-bughunt-2026-10-30.md) and
[October 31 hunt](summary-grouping-total-share-bughunt-2026-10-31.md). Their
dated evidence was left intact. Closed issues and merged fixes were checked
against the investigated snapshot rather than counted as new findings.

The requested scope was investigation and ticket publication. No package
implementation, package regression tests, dependency metadata, generated
documentation or build exclusions were changed. The user delegated ticket
granularity and publication, so the interactive breakdown checkpoint of
`to-tickets` was omitted. Four independent P2 deliveries were recommended;
none needed another as a blocker.

| Finding | Delivery | Established effect |
| --- | --- | --- |
| F1 | Preserve populated dots forwarding in local summary selectors | Ordinary dplyr accepted the inline selector; the Margin rewrite failed |
| F2 | Reject known fixed-key overlap before typed metadata | DuckDB sent an avoidable metadata query; dtplyr hid the overlap diagnostic |
| F3 | Track visible list2env rebinding in share dependency analysis | Forbidden dependencies were accepted and a derived column read input payload instead of the displayed share |
| F4 | Keep native DuckDB shares bound to displayed volatile values | Eligible sequence summaries produced fractions inconsistent with their visible parent or Grand total |

## Confirmed findings and evidence

### F1: R dots forwarding became a raw selection symbol

An unpartitioned data.frame with x = 1:3 was sufficient. Ordinary
`dplyr::summarise()` accepted an inline function forwarding `...` to across
and returned x = six. The Margin verb raised `'...' used in an incorrect
context`. Forwarding `..1` instead produced a nonexistent-column error.
The same boundary affected if_any and if_all.

All 432 investigated cases failed only on the Margin side; 45 wrapper controls
agreed. An independent formal-capture control showed ordinary capture receiving
the populated expression, while resolving a quoted raw dots symbol reproduced
the failure. The first-dot preflight and loss of the caller's actual dots
binding were refuted by preceded-summary and wrapper controls. Closed
#621/#925 concerned different argument boundaries; #827/#1002 concerned column
names, rather than R dots-argument references.

[The summary lane](2026-11-02-summary-grouping-share-bug-hunt/summary/README.md)
holds the contract, minimized call, matrix, root boundary and duplicate searches.

### F2: mixed preflight missed an already resolved plan error

With fixed a and a Grouping specification beginning with a, the overlap was
already invalid. Adding a later type predicate delayed the overlap check until
metadata acquisition. The 130-call matrix included five constructors and five
consumers, excluding unsupported SQL nesting. All 65 names-only controls
reported the overlap without a query. Typed dtplyr calls substituted 25 metadata
refusals; typed DuckDB calls invoked 15 actual zero-row queries before the
overlap. A temporary DBI trace observed invocation, rather than treating a Sent
query record as receipt evidence.

The mixed path resolved leading selections but did not apply the fixed-key
overlap rule to those resolved values. Closed #1090 and merged #1120 repaired
invalid selections themselves, leaving this plan-level rule after metadata.
[The Grouping lane](2026-11-02-summary-grouping-share-bug-hunt/grouping/README.md)
holds the minimal replay, ADR 0005 argument, controls and duplicate comparison.

### F3: a visible environment binding replacement retained an old proof

A literal list2env call replaced the environment binding used by a subsequent
get. The source then read a preceding ordinary summary, although planning
treated the binding as an independent external environment. In the opposite
direction, a displayed share of .25/.75/one accompanied a derived column of
100/300/400 from the same-named input payload. Both calls should have been
refused. Replacing only list2env with assign produced the intended refusals.

The matrix established 120 missing refusals and 216 successful controls,
covering three eager input classes, both helpers and directions, fixed/missing
partitions and five setter spellings. The absent invalidation concerned a
literal visible binding replacement, not arbitrary opaque callback effects.
#1045 concerned parent replacement; #1091/#1133 concerned indirect callee
resolution. Those repairs were present and did not cover this direct setter.
[The share lane](2026-11-02-summary-grouping-share-bug-hunt/share/README.md)
holds the exact calls, causal separation, source contract and searches.

### F4: one textual SQL relation did not bind one volatile value

A one-row native DuckDB two-dimension rollup returned sequence values four,
five and six, with Parent fractions two, five-sixths and one. The detail ratio
should have been four-fifths. A one-dimension rollup with a fixed key also
returned a Total fraction of three for displayed detail/Grand total values
three/four. Construction left the sequence uninitialized in both cases.

The 96-call matrix compared 128 share columns with displayed-row ratios:
32 native comparisons disagreed, and all 64 portable comparisons agreed.
The failures included aggregate-plus-sequence sources, both label forms and
missing fixed partitions. Which helpers were requested changed the affected
projection. A separate hand-written SQL control reproduced reevaluation without
the Margin API; a materialized-CTE control isolated a stable value boundary.

#919/#939 had removed hidden self-joins under the same public contract. The
residual native shapes used one grouping relation and windows but still
reevaluated the source. Existing multi-level/fixed regression coverage selected
portable aggregation. This was a focused follow-up to that completed fix,
rather than a claim that all volatile shares or all multi-level tests failed.
[The backend lane](2026-11-02-summary-grouping-share-bug-hunt/backend/README.md)
holds the final measured scope, deterministic reproductions and limitations.

## Bounded exploration and exclusions

These counts described separate loops, with overlapping cases where a finding
was minimized or expanded. They did not constitute a complete API input space.

| Loop | Completed scope | Result |
| --- | --- | --- |
| Local scopes | 336 comparisons | 276 matched, 56 paired errors; four parsed-language differences fell under the documented non-rewrite boundary |
| Local forwarding discovery | 120 comparisons | Four finding examples, 92 matched, 24 paired refusals |
| Forwarding finding matrix | 432 investigated cases plus 45 controls | 432 divergences; all 45 controls agreed |
| Retained Grouping selections | 690 comparisons | Accepted values and one-read behavior agreed with direct typed tidyselect |
| Mixed fixed-key preflight | 130 public calls | 25 masked overlap conditions and 15 extra metadata invocations |
| Visible share rebinding | 120 investigated calls plus 216 controls | 120 missing refusals; all controls passed |
| DuckDB key types | 14 types, 112 public calls | Numerical shares and direct materialization passed |
| SQL output names | 198 public cases | 183 passed; 15 failures were paired upstream name/computation limitations |
| SQL sequence shape probe | 16 calls | Two native two-dimension Parent failures |
| SQL volatile discovery | 36 SQLite calls then first DuckDB case | SQLite agreed; the first DuckDB mismatch stopped the exploratory loop |
| Native/portable share matrix | 96 calls, 128 share columns | 32 native mismatches; 64 portable columns all agreed |

The source package's complete suite was not run as a bug-hunting loop. The
focused scripts were research artifacts outside the package, and their deliberate
red modes were not installed regression tests. No repair was attempted. Results
were limited to the tested grammars, classes, finite fixtures and installed
versions. They established no live PostgreSQL/hosted service, other dependency
version, arbitrary callback/extension class, unbounded plan, concurrency or
interruption guarantee.

## Replay and publication

The evidence directory is
[`2026-11-02-summary-grouping-share-bug-hunt`](2026-11-02-summary-grouping-share-bug-hunt).
Each lane's README names its commands and outcomes; the scripts run from the
checkout root and load that checkout through pkgload. Adjacent CSVs and logs
retain measured results; log/text trailing whitespace was normalized for
publication. The manifest recorded retained evidence hashes. DuckDB drivers
used `shared_home = FALSE`.

The four finding harnesses offered an intentional exit-one `--assert` mode.
Their report modes checked controls and recorded the investigated behavior.
The backend reproducer also offered `--total` for the fixed-key Total example.
`environment.txt` recorded R 4.6.1 on aarch64 macOS, dplyr 1.2.1, dbplyr 2.6.0,
dtplyr 1.3.3, data.table 1.18.6.1, Arrow 25.0.1, DuckDB 1.5.5, RSQLite 3.53.3,
DBI 1.3.0, rlang 1.3.0, tidyselect 1.2.1 and pkgload 1.5.3.

The publication scope was source: the new research notes, standalone harnesses
and retained evidence changed no package input, generation or verification
tooling. Investigation files were excluded from the tarball and were not
consumed by package or documentation generation. This effect assessment, rather
than the directory name alone, justified omitting package build/check, the
complete suite and snapshots. The clean committed SHA, required five-check
outcomes, execution handle and terminal result, fetched-main assessment and
review record were to be recorded on the pull request. CI monitoring was outside
the requested scope.
