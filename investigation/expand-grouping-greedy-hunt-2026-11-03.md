# Expansion and Grouping specification composition-boundary bug hunt

Investigated: 2026-11-03
Baseline: `4b34fdecb2d8a41034b3d9b5d5af8e51592555e1` (`origin/main`)
Scope: dated investigation evidence and implementation tickets; no fixes

A dedicated worktree was created from freshly fetched main. Four parallel
tracks investigated Grouping specification capture, local expansion,
Arrow/dtplyr and SQL. The November 2
[binary-boundary hunt](expand-grouping-greedy-hunt-2026-11-02.md), October 31
[greedy hunt](expand-grouping-greedy-hunt-2026-10-31.md), October 30
[exhaustive hunt](expand-grouping-exhaustive-hunt-2026-10-30.md), and relevant
recent nesting investigations supplied previous coverage and ticket boundaries.
Their measurements were not counted as new executions.

Public-contract feedback loops and ordinary backend controls supplied the
initial observations. Candidates were minimized, ranked explanations were
tested, and separate witnesses challenged the remaining findings. The user
requested no implementation and delegated ticket granularity without a quiz.
`to-tickets` produced three independent P2 `bug`, `ready-for-agent` issues.
Each delivered one verifiable public behavior; no prefactoring ticket or
blocking edge was needed. No existing issue was closed or modified.

## Findings and ticket boundaries

| Finding | Ticket | Independently verifiable behavior |
| --- | --- | --- |
| S1 | [#1179](https://github.com/sayuks/marginplyr/issues/1179) | Collapse a sorted SQLite Margin result while exposing only its public schema |
| S2 | [#1180](https://github.com/sayuks/marginplyr/issues/1180) | Collect direct SQLite Boolean-word results with CTE SQL options |
| LZY1 | [#1181](https://github.com/sayuks/marginplyr/issues/1181) | Expand one-row dtplyr dist/logLik dimensions with compatible typed missing values |

### S1: ordinary collapse exposed physical sort columns

A one-row SQLite source held integer `g = 1`. Included-only expansion with a
NULL Margin label and last Margin order collected only `g`. After `collapse()`
the lazy table still reported only `g`, but collection returned `g` plus two
private ordering columns. Ordinary collapse of the arranged source retained
only `g`. This witness required no omitted branch, identifier, Boolean name
or text-label conversion.

The direct renderer included physical ordering columns as ADR 0031 intended;
the direct collector removed them. Collapse reused that SQL through an opaque
relation whose registered schema named only public columns. An all-public
selection could remain a star projection and retain the physical columns.
These observations supported a missing public-schema boundary, rather than an
ordinary collapse limitation or special-name conversion failure.

Across 432 focused fixtures, all 288 sorted collapsed results exposed extra
columns; the 144 unsorted controls retained their schema. A different two-row
mixed-case FALSE witness and a separate investigator's included-only witness
reproduced the defect. Their expected-schema assertion modes exited one.
Closed #976 and merged #989 repaired re-expansion by another Margin verb,
not this ordinary collapse boundary. The contract concerned public schema;
no persistence of a previous Margin order or direct type restoration through
ordinary derived verbs was inferred.

### S2: direct CTE collection lost Boolean-word names

A one-row SQLite source held `g = "a"` and integer `TRUE` payload 7. Default
rollup expansion collected both copies correctly. Collecting the same direct
result with `sql_options(cte = TRUE)` raised `undefined columns selected`.
Raw execution of that rendered SQL returned `g` and `column2`. An ordinary
filtered-source CTE control retained `g`, `TRUE` and the original value.

All 288 Boolean-word observations in the focused matrix failed CTE collection;
all 144 ordinary-name observations and all 36 ordinary filtered-source CTE
controls succeeded. Empty/one/three-row sources, four public roles, two label
forms, one-set/rollup plans and three sort choices were represented. The raw
typed companion already had the wrong name, while the stored public lazy
query rendered the correct schema with the same option. This distinguished
CTE alias lifting in direct companion execution from a collector-only failure
or invalid source schema.

An independent two-row omitted-first FALSE witness and a separate
investigator's mixed-case witness reproduced the defect. Their expected
collection assertions exited one. Closed #826 supplied the direct Boolean
contract; #1095/#1134 repaired derived limiting/filtering; #1136/#1150 repaired
binary inputs and their CTE paths. This was a residual direct-companion path,
separate from those successful derived-query acceptance cases.

### LZY1: one-row dtplyr omitted prototypes lost scalar classes

A one-row immutable dtplyr input held a `logLik` dimension and row identifier.
Rollup expansion with a NULL Margin label raised
`dt_join_type_mismatch_error` on collection, reporting logLik/numeric branches.
Ordinary typed mutation using `vec_init()` followed by `union_all()` preserved
the class, original/missing values and both row identifiers. A one-distance
`dist` dimension reached the same boundary.

The faithful zero-row proxy retained the class; scalar NA subsetting returned
numeric, while vector initialization retained the class. Successful ordinary
combination ruled out a general backend union limitation. Three-row assignment
could retain the original class, so the failure was measured at the one-row
boundary rather than generalized to all sizes.

The completed 1,728-comparison matrix had 1,584 matches and 144 package-only
errors, 72 per class. Every ordinary control succeeded and every caller table
remained unchanged. Failures covered root/filter/cache inputs, both missing
label spellings, included-first/omitted-first/retained-duplicate plans,
absent/present identifiers and none/last sort. Empty, three-row, included-only,
fixed-key and payload controls passed. A separate distance-of-five witness
with payload 19 and an omitted-first plan reproduced against archived main.
Closed #1093/#1130 explicitly covered local scalar dimensions; #833/#860
covered list keys. Neither covered this lazy scalar-prototype path.

## Additional measured coverage and exclusions

| Probe | New finite observations | Disposition |
| --- | ---: | --- |
| Lexical/default/alias selection differential | 7,530 | 7,330 matches; 200 required empty-dimension refusals |
| Dots/dot-dot selection differential | 1,584 | 588 matches; 954 matching External-condition classes; 42 required refusals; all caller force counts matched |
| Additional local vector/container families | 4,166 | 3,579 matches; fixture exclusions, ordinary combination/order/record-container limitations and delegated inner attributes accounted for the remainder |
| Independent repeated local expansion | 2,808 | All row-copy, factor-code/level, occurrence and requested-order comparisons matched |
| Arrow reader graphs | 88 | 44 inspections succeeded, 44 expansions refused; every reader retained its three rows |
| Arrow UnionDataset/partition graphs | 960 | All exact value/type/schema, multiplicity and requested-key comparisons matched |
| dtplyr scalar classes | 1,728 | 1,584 matches; 144 instances of LZY1 |
| SQL composition exploration | 1,152 | Leads classified against ordinary controls; unsupported or nondeterministic populations excluded before conclusions |
| Focused SQLite schema boundaries | 864 | Two observations for each of 432 fixtures; established the S1/S2 boundary populations above |

The [specification](2026-11-03-expand-grouping-greedy/spec/README.md),
[local](2026-11-03-expand-grouping-greedy/local/README.md),
[lazy](2026-11-03-expand-grouping-greedy/lazy/README.md), and
[SQL](2026-11-03-expand-grouping-greedy/sql/README.md) track records describe
the independent oracles, fixtures, exact classifications and limits. Selection
probes checked selected dimensions, not a new proof of all expanded families.
Schema probes did not imply general physical-type or complete order proofs.

Several initial harnesses required corrections. Bare-table CTE collection had
an ordinary dbplyr failure, so successful CTE controls used identity filters.
Window/head comparisons lacked deterministic order after derivation; ordinary
DuckDB ordered unions and SQLite Boolean semi/anti-join schemas also failed
their intended controls. They were excluded as package-only findings. Local
record-column row counts were wrong after ordinary relocation too, and hilo
inner attribute differences had equal proxies. The corrected repeated-expansion
oracle restored factor codes and NA levels independently rather than trusting
ordinary factor combination. Cached dtplyr rename controls and an uninferable
Arrow partition fixture were corrected before final counts. The track records
distinguish initial evidence from complete reruns; no initial partial pass was
added to the final counts.

## Main, duplicate checks, replay and environment

A fresh main fetch before issue publication still named the baseline SHA.
SQL witnesses and the separate dtplyr distance witness reproduced against a
disposable archive of that SHA. All-state Issue and relevant PR searches,
their matching links and complete prior-report dispositions are recorded in
the lazy and SQL track records. No separate matching report for these three
boundaries was found. The new issues carry successful controls, the tested SHA,
environment, expected/observed behavior, acceptance criteria and no blockers.

The [evidence bundle](2026-11-03-expand-grouping-greedy) holds replay scripts,
classified CSVs, terminal logs, prior-report/search evidence and environments.
From the repository root, these expected-behavior assertions intentionally
exited one on the investigated baseline:

```sh
Rscript investigation/2026-11-03-expand-grouping-greedy/sql/reproduce-collapse-schema.R --assert
Rscript investigation/2026-11-03-expand-grouping-greedy/sql/reproduce-cte-boolean.R --assert
Rscript investigation/2026-11-03-expand-grouping-greedy/lazy/reproduce-dtplyr-loglik.R --assert
```

Observation modes exited zero after successful controls and printed the
unfixed failure. Independent witnesses supplied separate assertions. Every
owned execution was collected through its terminal outcome, and testthat
parallel execution was explicitly disabled. No full suite, package build or
render was used as an exploratory substitute for the focused feedback loops.

The environment was macOS arm64, R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0,
vctrs 0.7.3, rlang 1.3.0, tidyselect 1.2.1, dtplyr 1.3.3,
data.table 1.18.6.1, Arrow 25.0.1, DuckDB 1.5.5 and RSQLite 3.53.3.
Fixtures used isolated tables, disposable files and private in-memory SQL
databases. No live PostgreSQL/SQL Server, hosted service, other OS/version
matrix, arbitrary caller-class/callback enumeration, unbounded Grouping family
or general performance proof was attempted. Successful finite comparisons
established only the listed fixtures on the pinned revision.

All additions were dated evidence under the existing investigation exclusion.
Package code, tests, installed files, generated documentation, dependencies,
build rules and execution tooling were unchanged. Their effects justified
`source` Review-ready scope: the five required checks, with package build/check,
complete suite and snapshots omitted. The exact committed SHA, retained
execution handle, terminal outcome, current-main compatibility assessment and
Standards/Spec review record belonged on the pull request. CI was not monitored.
