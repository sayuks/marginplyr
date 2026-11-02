# Expansion and Grouping specification binary-boundary bug hunt

Investigated: 2026-11-02
Baseline: `978d7bef4247bf1348af5b90bfab47924db25500` (`origin/main`)
Scope: dated investigation evidence and implementation tickets; no fixes

A new worktree was created from fetched main. Four parallel tracks explored
Grouping specification capture/algebra, local expansion, Arrow/dtplyr, and SQL.
The October 31 [greedy hunt](expand-grouping-greedy-hunt-2026-10-31.md), October
30 [exhaustive hunt](expand-grouping-exhaustive-hunt-2026-10-30.md), and the
October 29 follow-up/deep/systematic investigations supplied prior coverage and
Issue boundaries. Their old executions were not counted as new evidence.

The investigation first built finite public-contract and ordinary-backend
feedback loops, classified differences, minimized candidates, and tested
ranked explanations. The user expressly requested no implementation, so fixes
and package regression tests were deferred to tickets. The user also delegated
ticket boundaries without a quiz. `to-tickets` produced one independent P2
`bug`, `ready-for-agent` ticket; no prefactoring ticket or blocking edge was
needed. No existing Issue was closed or modified.

## Confirmed finding and ticket boundary

| Finding | Ticket | Independently verifiable behavior |
| --- | --- | --- |
| S1 | [#1136](https://github.com/sayuks/marginplyr/issues/1136) | Use SQLite Boolean-word expansions on either side of ordinary joins and set operations |

A one-row SQLite source held g equal to a and integer TRUE payload 7.
`expand_with_margins()` with a rollup of g collected directly, and its repaired
head/filter path succeeded. A left join with the expansion on either side
raised `no such column: TRUE`. Original-source and explicitly materialized
controls retained the public name and correct values. Right-input union-all
and union failed while left-input versions succeeded; intersection and set
difference failed on either side.

The primary 48-observation loop compared sixteen compositions over original,
materialized and expanded inputs: ten expanded-input failures and successful
controls throughout. A minimized expected-behavior assertion first checked the
successful ordinary controls and then failed on the expanded right-input join.
An independent witness used FALSE payload 19 and g equal to b, reproducing
both join failures, right-input union-all and intersection, with every ordinary
control successful. Both assertion modes intentionally exited 1.

Three explanations were ranked before cause probes: incomplete multi-input
query traversal, loss of the final-render result class, and an ordinary SQLite
limitation. A left-input join retained the special result class, but applying
the existing repair helper explicitly still did not repair its multi-join graph.
A right-input union lost that class, and explicit repair made it execute.
Successful ordinary source and materialized controls ruled out invalid input
schemas. This established two implementation boundaries within one public
binary-composition behavior, which fit one independently verifiable ticket.

The contract was a usable query over public columns. ADR 0031 still delegated
derived-result types/order to dbplyr; no new Margin-order or type-restoration
promise was inferred. ADR 0020 excluded implicit materialization or an
unrequested data read as a workaround. Closed #826 covered the direct Boolean
boundary; #1095 and merged #1134 repaired select/union-layer limiting/filtering.
Their acceptance cases did not establish both input positions of binary graphs.
Closed #976 concerned private sorting columns on re-expansion, a different
boundary. Complete Issue bodies/comments and the #1134 discussion were read.

Fresh all-state searches used Boolean, Boolean join, Boolean union, SQLite
join, SQLite intersect and SQLite composition, plus Boolean SQLite pull
requests. No separate binary-graph report was found. A fresh main fetch before
publication still named the baseline SHA. Evidence and the search dispositions
were retained in the [local track](2026-11-02-expand-grouping-greedy/local/README.md)
and [SQL track](2026-11-02-expand-grouping-greedy/sql/README.md).

## Additional measured coverage and excluded leads

| Track | New finite observations | Disposition |
| --- | ---: | --- |
| Phase-correct injection differential | 4,488 | 4,284 matches; 204 required empty-dimension refusals |
| Independent nested family algebra | 3,000 | 2,758 matches; 242 expected duplicate refusals |
| Injected heads and identifier boundaries | Three head forms; dimensions 0/1/30/31/32/40 | One caller read and first answer retained; mask overflow behaved as documented |
| Additional local vector families | 1,373 | 1,240 matches; ordinary ordering/combination limitations, five fixture exclusions, eighteen justified sf missing-prototype refusals |
| Factor identity/order | 1,944 | All matched independent integer-code/level/order expectations |
| Zero-column cardinality | 1,944 | 1,026 matches; 918 required refusals |
| Structured local ordering | 180 | All matched independent rowwise missingness/order keys |
| Arrow/dtplyr public-name/role matrix | 4,800 | All matched independent row copies and requested keys; caller tables unchanged |
| dtplyr join/set typed inspection | 2,400 admitted; 240 unsupported cross-join attempts | All admitted metadata comparisons matched; cross-join source failures excluded |
| Arrow nested types/source graphs | 672 corrected observations | 445 matches; shared source/sort limitations and five ordinary dictionary-level-order differences excluded |
| SQL physical-type/value differential | 292 classified observations | 285 matches; three required STRUCT label refusals; four unadmitted source populations |
| SQL public-name boundaries | 4,928 | 4,848 matches; 64 prohibited-name refusals; sixteen ordinary DuckDB `.data` compute failures |

The evidence bundle's four track READMEs describe independent oracles, exact
fixtures, hypotheses, corrections and limits. Successful scalar/name matrices
were not claimed as complete physical-type, warning-propagation or arbitrary
callback proofs. Where an ordinary control failed, no package-only finding was
inferred from the mismatch count.

Several initial harnesses produced false signals. Injection negation accidentally
formed a different splicing AST; an empty family oracle attached names to
`integer()`; local fixed-selection fixtures initially used unsupported spelling
and drop settings; a RecordBatch fixture called an unavailable method; and a
SQL comparator initially put identifiers after payload and sorted a name that
ordinary dplyr could not handle. Every affected population was corrected and
rerun, with initial evidence distinguished where retained. Those observations
were not promoted into findings or added to final passing counts.

The sf typed-missing candidate was excluded because ordinary combination used
an empty geometry that was not missing according to either sf or vctrs. Arrow
chunked-dictionary candidates had identical decoded values and key types while
ordinary dictionary unification also varied level order. DuckDB `.data`
computation failed identically on an ordinary source. These were measured
exclusions, not assumptions that all third-party failures were harmless.

## Replay, environment and limits

The [evidence bundle](2026-11-02-expand-grouping-greedy) contains scripts,
classified CSVs, terminal logs and environment records. The primary replay can
load a pinned disposable source with `MARGINPLYR_HUNT_SOURCE`; the independent
witness loads the repository root. From that root:

```sh
Rscript investigation/2026-11-02-expand-grouping-greedy/local/sql-alias-graph.R --assert
Rscript investigation/2026-11-02-expand-grouping-greedy/sql/independent-alias-graph.R --assert
```

Both commands intentionally exited 1 against the investigated baseline. Normal
observation mode exited zero after asserting successful controls and printing
failures. Matrix-runner exit zero meant measurements completed, not that the
reported defect was fixed. `TESTTHAT_PARALLEL=false` was explicit; every owned
execution was collected through a terminal outcome.

The environment was macOS arm64, R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0,
vctrs 0.7.3, rlang 1.3.0, tidyselect 1.2.1, Arrow 25.0.1,
dtplyr 1.3.3, data.table 1.18.6.1, DuckDB 1.5.5 and RSQLite 3.53.3.
Fixtures used disposable files, isolated data.table roots and private in-memory
SQL databases. No live PostgreSQL/SQL Server, hosted database, other OS,
dependency-version matrix, arbitrary custom-class/callback enumeration,
unbounded family proof or full reader-consumption audit was attempted.
Successful comparisons established only the stated fixtures on the pinned SHA.

All additions were dated evidence under the existing `investigation/` package
exclusion. They changed no package code, tests, generated documentation,
dependencies, build rules or execution tooling. The pre-review scope was
therefore `source`: the five mandatory checks, with package build/check,
complete suite and snapshots omitted. The exact committed SHA, terminal
Review-ready outcome, integration assessment and Standards/Spec review record
belonged to the pull request. CI was not monitored.
