# Expansion and Grouping specification receiver-dispatch hunt

Investigated: 2026-11-07
Baseline: `0b5143133c023ccb4efb6fe27f1ef551e9f49186` (freshly fetched main)
Scope: dated investigation evidence and implementation tickets; no repair

A new worktree from main divided the hunt across recursive local keys and real
extension vectors, Grouping receiver dispatch and algebra, deferred dtplyr
labels and native DuckDB types, and special public names and spatial columns.
The November 6 [boundary](expand-grouping-bughunt-2026-11-06.md) and
[deep](expand-grouping-deep-bughunt-2026-11-06.md) investigations and November 7
[deferred-boundary](expand-grouping-bughunt-2026-11-07.md),
[carrier](summary-grouping-share-carrier-bughunt-2026-11-07.md) and
[registry-boundary](nest-spec-bughunt-2026-11-07.md) investigations supplied
earlier coverage and exclusions. Their measurements were not counted as new
executions. The baseline included the #1413–#1422 integration.

## Findings and implementation slices

| Finding | Ticket | Independently verifiable behavior |
| --- | --- | --- |
| D1 | [#1433](https://github.com/sayuks/marginplyr/issues/1433), P2 | Keep generated dtplyr label display, factor encoding and reconstruction independent of caller namespace operators. |
| G1 | [#1434](https://github.com/sayuks/marginplyr/issues/1434), P2 | Preserve receiver dispatch in Grouping context-name selections, avoiding wrong fixed grains and false overlap refusals. |

`to-tickets` produced two independent behavior slices, each labelled `bug` and
`ready-for-agent`, with no blockers or prerequisite prefactoring. The user
delegated ticket granularity and publication, so the breakdown quiz was omitted.
No existing report was closed or changed, and no implementation was attempted.
Diagnosis stopped at reproducible evidence and tickets; repair and regression
test implementation were outside this task.

### D1: a generated label operation accepted a caller's replacement helper

An included-only expansion of one immutable dtplyr row should have retained its
character dimension. Installing a selective caller `:::` dispatcher after
construction instead changed `north` to `MASK`. Native input collection stayed
correct, and removing the dispatcher restored the same already-built result.
No omitted set, sort, identifier, factor or summary was needed. This violated
the opening row-copy contract of `expand_with_margins()`.

The [lazy lane](2026-11-07-expand-grouping-exhaustive-b/lazy/README.md) recorded
648 observations across three generated label operations, four source graphs,
three plans, all sort modes, early/late binding and three dispatcher modes.
There were 216 unwanted-callback observations with correct values, 216 silent
corrupt results and 216 callback errors. Every native source/mutation/union
control invoked zero selective callbacks. Construction invoked none, every
restored result matched, and source bytes stayed unchanged. Physical Margin
order and source destruction were not established by that matrix.

The [independent replay](2026-11-07-expand-grouping-exhaustive-b/coordinator/README.md)
used a separate archive with two different keys and payload `17, 43`. The same
query returned `changed:east, changed:west` only while the selective dispatcher
was installed. Its desired-contract assertion intentionally exited one after
the native, restoration and source-ownership controls passed.

The generated private helper name was qualified, but its namespace operator
remained caller-resolved in deferred label execution. Display, encoding and
reconstruction shared that boundary and belonged in one repair slice.

### G1: a names-only proof bypassed the selection input's method

A data-frame subclass's `names()` method returned literal names outside a
selection context and used the active typed metadata inside one. Direct typed
tidyselect selected `b`; the same selection with an opaque wrapper selected `b`
through public inspection and Margin consumers. The direct spelling instead
used an unclassed names proxy. Beside fixed `a`, it falsely refused overlap on
`a` before invoking the typed method.

The [Grouping lane](2026-11-07-expand-grouping-exhaustive-b/grouping/README.md)
recorded 96 failing comparisons across eight spellings, fixed-key and all five
constructor positions, and inspection/expansion. Literal and opaque typed
controls matched exactly; the failing path invoked the typed method zero times.
Five public consumers reproduced the boundary, with byte-identical sources.
With the selection in `.by` alone, inspection silently chose `a` instead of
`b`: a count summary returned two rows of one instead of one row of two, and
both Nest verbs returned two cells instead of one.

The coordinator changed the subclass and column names and used two rows in a
separate baseline archive. Direct typed selection and ordinary dplyr grouping
passed; inspection and expansion falsely refused overlap while the opaque
control produced the correct fixed/included plan. Its desired-contract
assertion intentionally exited one. The evidence established this finite
context-sensitive receiver-method family, not every input class or names
method. The public tidy-select contract and ADR 0013 supplied the expected
behavior; no new syntax or source-row read was required.

## Additional finite comparisons

| Probe | Final observations | Outcome and limits |
| --- | ---: | --- |
| [Recursive local keys](2026-11-07-expand-grouping-exhaustive-b/local/README.md) | 2,880 | Exact full-frame/schema/type/physical-order matches, with source bytes unchanged. |
| Real extension vectors | 2,916 | 1,953 exact matches; 207 specifically validated label refusals; 270 ordinary Surv sorting-recursion exclusions; 486 ordinary Matrix fixture refusals. |
| Surv separating controls | 18 | Ordinary value-only arrangement reproduced the sorting failure; unsorted expansion cardinality was correct. |
| [Independent Grouping algebra](2026-11-07-expand-grouping-exhaustive-b/grouping/README.md) | 9,972 | Exact included-vector and occurrence-order matches under keep/drop for 4,986 bounded random ASTs; 14 oversized generated families excluded. |
| [Native DuckDB types](2026-11-07-expand-grouping-exhaustive-b/lazy/README.md) | 810 | Exact complete-frame correspondence to local expansion; 360 comparisons also checked physical order, 450 made no order claim. |
| [Special public names](2026-11-07-expand-grouping-exhaustive-b/coordinator/README.md) | 1,248 | Final ordinary-composition classifications and limits were retained with the coordinator's CSV and log. |
| Spatial columns | 216 | Value/schema/order checks and explicit unsupported-prototype or ordinary-name-repair classifications were retained separately from outer-class/attribute comparisons. |

Lane notes recorded the corrected oracle/fixture mistakes and excluded their
preliminary runs. Unsupported ordinary operations, unspecified tie order,
arbitrary outer-class differences and a source name ordinary selection itself
refused did not become implementation tickets. No count from one lane was
added to another lane's count. These finite comparisons did not prove absence
of defects, all extension classes, all warning/condition identities, general
performance or unbounded plans.

## Freshness, duplicate comparison and publication

Main was fetched at worktree creation and again before publication; both named
the baseline SHA. Independent reproductions ran from a separate archive of
that same current-main SHA. All final execution handles were collected through
terminal completion; scripts asserting the two desired contracts deliberately
failed because the defects were not repaired.

The initial all-state export contained 721 Issue bodies/comments and the
available all-state PR bodies. Lane notes retained complete relevant report
discussions and all-state live queries. The coordinator refreshed queries
including both Issues and PRs: `"namespace" "label"` returned 42 matches and
`"names" "peek_data"` returned six. The full newly published
[#1427](https://github.com/sayuks/marginplyr/issues/1427) report covered nesting
cells, keys and identity expressions; D1's included-only expansion had no
nesting expression. #1143/PR #1168 protected bare label-helper lookup, #491
private-helper visibility, and #1422 source copying. None reported D1's label
namespace operation. For G1, complete #1277 and #995 bodies distinguished
context-name fast paths from implicit-class generic operands and injected
selectors. The lane's other complete comparisons established no duplicate.

A final recent-Issue refresh also found concurrent #1428–#1432. The complete
[#1429](https://github.com/sayuks/marginplyr/issues/1429) body concerned an
ordinary qualified formula-shaped call and a different early classifier
shortcut, rather than G1's names receiver dispatch. The other titles concerned
across predictions and summary closure provenance. Created #1433/#1434 were
read back with their bodies and labels. Evidence links were published after
the investigation branch was pushed.

The [evidence bundle](2026-11-07-expand-grouping-exhaustive-b) retained scripts,
CSVs, logs, environments and replay commands. No full suite, build or rendering
substituted for the focused probes. The environment was arm64 macOS, R 4.6.1,
dplyr 1.2.1, dbplyr 2.6.0, rlang 1.3.0, tidyselect 1.2.1, vctrs 0.7.3,
dtplyr 1.3.3, data.table 1.18.6.1, Arrow 25.0.1, DuckDB 1.5.5 and
RSQLite 3.53.3. Hosted stores, live PostgreSQL/SQL Server and other
platforms/versions remained unmeasured.

## Submission effects

Only dated evidence and standalone probes under the existing investigation
exclusion were added. Package source/tests, generated package documentation,
examples, vignettes, installed files, dependencies, build rules and check
tooling were unchanged. No package/check consumer invoked the new probes.
These content and consumer effects justified Review-ready `source` scope.
The cumulative comparison used the merge-base with freshly fetched main,
`0b5143133c023ccb4efb6fe27f1ef551e9f49186`; no ancestor's full result was
inherited. Package build/check, complete suite and snapshots were omitted.
The submitted SHA, compatibility assessment, six-check outcome and review
record belonged on the PR. Research without a behavior change required no NEWS
entry. CI was not monitored.
