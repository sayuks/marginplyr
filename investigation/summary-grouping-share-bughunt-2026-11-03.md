# Bug hunt: summaries, Grouping specifications, and Total shares

Investigated: 2026-11-03
Repository snapshot: `4b34fdecb2d8a41034b3d9b5d5af8e51592555e1`

The investigation began in a new worktree created from fetched main, on branch
`codex/summary-grouping-share-bughunt-2026-11-03`. Four investigators divided
ordinary summary evaluation, Grouping specification selection/preflight,
contextual-share scope and typed denominators, and live SQL boundaries. A
fresh fetch before ticket publication still resolved main to the snapshot
above. The coordinator independently replayed the five non-PostgreSQL minimal
signals and directly executed the PostgreSQL finding matrix and red signal.

The [November 2 investigation](summary-grouping-share-bughunt-2026-11-02.md)
and the October 28–31 notes it cited guided remaining boundaries. Their dated
evidence was left intact. Closed reports and merged repairs were compared
with the investigated code rather than counted again. Existing source/test
contracts, ADRs and ordinary backend controls supplied the expected behavior.

The requested scope was investigation and ticket publication. The changes
added research notes, standalone R harnesses and retained evidence only. No
package implementation, package regression test, dependency metadata,
generated documentation or build exclusion was changed. The user explicitly
delegated ticket granularity and publication, so `to-tickets`' interactive
breakdown checkpoint was omitted. Six independent P2 issues were published
with `bug` and `ready-for-agent`; none required another as a blocker.

| Finding | Ticket | Established effect |
| --- | --- | --- |
| F1 | [#1182](https://github.com/sayuks/marginplyr/issues/1182) | Populated numbered dots forwarding to pick failed only in the Margin call |
| F2 | [#1183](https://github.com/sayuks/marginplyr/issues/1183) | Across names lost dplyr's vector glue context, causing wrong names and false share-source refusals |
| F3 | [#1184](https://github.com/sayuks/marginplyr/issues/1184) | Warning-promoted Grouping errors reached metadata work or were replaced by a metadata refusal |
| F4 | [#1185](https://github.com/sayuks/marginplyr/issues/1185) | A visible binding setter with unresolved list keys retained a stale external-lookup proof |
| F5 | [#1186](https://github.com/sayuks/marginplyr/issues/1186) | dtplyr special-measure protection rewrote a lexical value into an input-column read |
| F6 | [#1175](https://github.com/sayuks/marginplyr/issues/1175) | Valid PostgreSQL summary names collided through truncated private denominator aliases |

## Confirmed findings

### F1: populated pick dots references were captured as raw symbols

An unpartitioned data.frame with x = 1:3 was sufficient. Ordinary dplyr
accepted an inline dots function summing `pick(..1)` and returned six. The
Margin call reported a nonexistent column named `..1`. All-dots pick and
across first-dots controls succeeded. The 504-case differential established
288 Margin-only errors and 216 agreements. The remaining pick boundary was
distinct from the across/if_any/if_all repair of closed #1138 / merged #1149.

[The summary lane](2026-11-03-summary-grouping-share-bug-hunt/summary/findings.md#f1-local-pick-first-dots-references-lost-promise-capture)
held the contracts, constructor/class/position matrix, runtime capture
distinction, related report comparisons and minimal replay.

### F2: scalar-pair glue expansion changed valid output names

For x/y summaries, a pure template using `seq_along(.col)` gave ordinary
dtplyr x_1/y_2 but Margin dtplyr x_1/y_1. A local no-share control retained the
correct names; requesting a Total share of y_2 falsely refused it as unknown.
Across-style share outputs also took the scalar-pair names. A literal
two-name vector was valid for two ordinary outputs but falsely refused by
several Margin paths. No external mutation was needed for those examples.

The 228-case naming matrix established eight wrong ordinary name sets, 96
false source refusals and eight wrong share-output name sets; 116 remaining
cases/controls agreed. Supplementary callback controls exposed extra glue
interpolation evaluation beside a share. These consequences were kept in one
ticket because the complete-context naming boundary connected prediction,
normalization, source registration and share-output expansion. Closed #100's
invalid two-names-for-one-output case did not cover the valid complete vector.

[The summary lane](2026-11-03-summary-grouping-share-bug-hunt/summary/findings.md#f2-across-names-were-expanded-one-scalar-pair-at-a-time)
held the pure reproduction, complete/scalar glue controls, matrix and prior
naming-report comparisons.

### F3: quiet preflight lost warnings that the caller made errors

Under warn = 2, a names-only Grouping selection of an absent column through
one_of was already an error. Live DuckDB invoked its zero-row metadata query
before returning that error; the same fixed-key selection invoked none. On a
derived dtplyr input, a later typed selection instead exposed the metadata
safety refusal, losing the original selector condition. A temporary DBI trace
counted actual query invocation, rather than treating Sent query records as
proof of receipt. No caller source-row scan was claimed.

The 260-call matrix established 60 extra DuckDB metadata invocations and 90
dtplyr replacement conditions. The boundary remained after #1090 / #1120's
leading-error repair because the discarded preflight suppressed the originating
warning. ADR 0005 supplied the locally decidable-error ordering contract.
[The Grouping lane](2026-11-03-summary-grouping-share-bug-hunt/grouping/findings.md)
held the constructor/consumer matrix, warning/lifecycle controls, query trace,
ranked alternatives and duplicate comparison.

### F4: unreadable list keys were treated as no binding replacement

A visible base list2env setter replaced an external-environment binding with
the summary's current environment. Wrapping its literal list in a brace pair
retained the old external-lookup proof and bypassed the documented dependency
refusal; removing only the braces produced the intended refusal. In the later
ordinary-summary direction, displayed shares of .25/.75/one accompanied
derived values 100/300/400 from the same-named input payload. The reverse
direction admitted an alias-dependent share source.

The matrix established 72 missing refusals and 168 controls across both
helpers, directions and eager input classes. This direct visible setter with
unresolved keys remained outside #1140's direct literal-list repair and
#1153's reflective-evaluation traversal. [The share lane](2026-11-03-summary-grouping-share-bug-hunt/share/findings.md#s1-nonliteral-list2env-values-retained-a-stale-lookup-proof)
held the minimal syntax pair, conservative-analysis contract and isolation
controls. Planning was not permitted to execute key-producing expressions.

### F5: a lexical special-name read became an input read

A dtplyr input had an otherwise unused .BY column with one/three. A local
summary introduced a .BY binding of five/seven through literal list2env and
summed it. Adding an independent contextual share made the call succeed with
one/three/four rather than lexical twelve. Local-input and renamed-column
controls returned twelve. Ordinary dtplyr and the no-share Margin call refused
the locked special binding, so the defect was converting refusal into a wrong
successful answer; the ticket allowed correct lexical values or a targeted
Package refusal without requiring new upstream grammar support.

The matrix established 112 wrong lexical results and 224 successful controls
over seven special names, both helpers, source/independent roles, direct/across
sources and local/lambda forms. Construction touched zero source rows and
collection touched four in the dedicated control. This literal-key ownership
boundary was independent of F4's unresolved-key lookup proof.
[The share lane](2026-11-03-summary-grouping-share-bug-hunt/share/findings.md#s2-dtplyr-input-aliasing-rewrote-list2env-owned-lexical-values)
held the translated expression and comparisons with closed #1033/#1049.

### F6: PostgreSQL truncated two private aliases to the same name

Two valid 36-byte ordinary summary names with a shared prefix survived ordinary
PostgreSQL summarisation. Requesting both Total shares caused collection to
fail on an ambiguous private denominator column. Both Parent shares and
portable aggregation reproduced it. UTF-8 public names below the same server
limit also failed. One-share and short-name two-share controls succeeded.

A disposable PostgreSQL 17.11 server reported a maximum identifier length of
63 bytes and notices showing the private aliases truncating to the same
identifier. The generated prefix alone consumed 28 bytes. The minimized
32-case matrix established eight failures and 24 successful controls.
[The SQL lane](2026-11-03-summary-grouping-share-bug-hunt/backend/findings.md)
held source-length thresholds, the public-name controls, allocator/source
citations, duplicate searches and server cleanup/replay details.

## Bounded exploration and limitations

The counts described separate loops; they overlapped where a finding was
minimized or expanded and did not constitute an exhaustive API input space.

| Additional loop | Completed scope | Result |
| --- | --- | --- |
| Typed visible denominators | 16 types × fixed/variable × local/dtplyr: 64 calls | 49 successes and 888 matching ratios; 15 paired no-share/backend errors |
| PostgreSQL typed keys | 14 types × fixed/variable × native/portable × sort request: 112 calls | All share-value checks agreed; row order was not asserted |
| Finite PostgreSQL NUMERIC range | Seven exponents × both helpers: 14 calls | All matched the database ratio oracle; overflow/underflow hypothesis refuted |
| Summary forwarding/capture discovery | Populated dots and wrapper/function scopes | F1 isolated; paired ordinary failures and existing conservative refusals were excluded |
| Naming context and literal vectors | Pure glue vectors and callback counts | F2 boundaries separated from aggregation and actual protected-name collisions |
| Share lexical discovery | Setter/lookup forms and seven special names | F4/F5 isolated; direct bindings and ordinary refusals retained as controls |

Typed key probes included packed/matrix/list, integer64, missing-level factors,
NaN/NA and semantic classes locally, and numeric extremes, arrays, jsonb,
bytea, range and zoned timestamps on PostgreSQL. Paired ordinary/backend errors
were recorded rather than described as share defects. SQL native and portable
value checks did not establish every materialization or subsequent-verb path.

The evidence covered the retained finite fixtures, syntax grammars, input
classes and installed versions. It established no other dependency-version,
hosted-service, arbitrary extension-class/callback, unbounded-plan, concurrency
or interruption guarantee. The complete package suite was not used as a
bug-hunting loop. No implementation repair was attempted.

## Replay and publication

The evidence directory was
[`2026-11-03-summary-grouping-share-bug-hunt`](2026-11-03-summary-grouping-share-bug-hunt).
Each lane's findings file named the exact commands, contracts, duplicate
queries and relevant limitations. Six minimized finding scripts offered
report and deliberate `--assert` modes. Report modes checked successful
controls and the investigated behavior; `--assert` required the desired
behavior and exited one on this snapshot. These were research signals, not
installed regression tests. The coordinator's sequential replay and adjacent
logs retained its independent observations.

`environment.txt` recorded R 4.6.1 on aarch64 macOS and the installed backend
versions, including dplyr 1.2.1, dbplyr 2.6.0, dtplyr 1.3.3, data.table
1.18.6.1, Arrow 25.0.1, DuckDB 1.5.5, RSQLite 3.53.3 and RPostgres 1.4.10.
The manifest recorded retained artifact hashes. Every owned research execution
and disposable PostgreSQL server was collected/stopped before submission.

The publication scope was `source`: these standalone research files were
excluded from the package and consumed by no package, documentation generation
or verification tooling. Their effects changed no package-check input. This
effect assessment justified omitting package build/check, the complete suite
and snapshots. The clean committed SHA, five baseline outcomes, original
execution handle, terminal result, fetched-main compatibility assessment and
review record were to be recorded on the pull request. CI monitoring was
outside the requested scope.
