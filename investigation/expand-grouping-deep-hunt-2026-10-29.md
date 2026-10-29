# Expansion and Grouping specification deep bug hunt

Investigated: 2026-10-29
Baseline: `ee4321789ca60b6259c53df2852f1574aaede584` (`main`)
Scope: investigation, reproducible evidence, and implementation tickets; no fixes

Four parallel tracks examined local expansion, Grouping specification capture
and selection, Arrow/dtplyr inputs, and SQL materialization and types. The
[earlier hunt](expand-grouping-bug-hunt-2026-10-29.md), its
[follow-up](expand-grouping-followup-2026-10-29.md), and the October 25–26 notes
supplied prior coverage and closed-ticket boundaries. Their measurements were
not counted as new coverage here. This note records observations on the pinned
baseline, not claims about later releases.

The measured environment was macOS arm64, R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0,
rlang 1.3.0, vctrs 0.7.3, tidyselect 1.2.1, dtplyr 1.3.3,
data.table 1.18.6.1, Arrow 25.0.1, DuckDB 1.5.5, RSQLite 3.53.3,
RPostgres 1.4.10, and PostgreSQL 17.11. PostgreSQL used a disposable cluster
with a private Unix socket and no TCP listener; the cluster was stopped after
measurement. DuckDB fixtures used private in-memory connections and Arrow
fixtures used temporary local Parquet files.

## Findings and ticket boundaries

| Finding | Priority | Ticket | Independently verifiable repair |
| --- | --- | --- | --- |
| S2 | P2 | [#1016](https://github.com/sayuks/marginplyr/issues/1016) | Reject invalid DuckDB index requests before writing a destination |
| D1 | P2 | [#1017](https://github.com/sayuks/marginplyr/issues/1017) | Inspect every repeated dtplyr update before trusting metadata |
| A1 | P2 | [#1018](https://github.com/sayuks/marginplyr/issues/1018) | Protect Arrow expansion against schema-hidden partition names |
| S1 | P2 | [#1019](https://github.com/sayuks/marginplyr/issues/1019) | Retain grouping and window-order metadata through DuckDB computation |

All four issues were published with `ready-for-agent` and no blockers. The
maintainer delegated the ticket split without a confirmation round. Each
repair could be verified independently. The two DuckDB findings shared a
method but differed in behavior and acceptance: pre-write validation did not
require metadata reconstruction, and metadata reconstruction did not require
index validation. The Arrow manifestations shared one partition-name boundary;
the dtplyr manifestations shared one repeated-update admission defect. No
prefactoring ticket was necessary.

## Replaying the findings

Run these commands from the repository root:

```sh
Rscript investigation/2026-10-29-expand-grouping-deep-hunt/lazy/reproduce-dtplyr-duplicate.R
Rscript investigation/2026-10-29-expand-grouping-deep-hunt/lazy/reproduce-arrow-narrow.R
Rscript investigation/2026-10-29-expand-grouping-deep-hunt/sql/duck-compute-boundary.R
```

The two lazy-backend replays checked successful controls and printed the
observations. Each also accepted `--assert`, requiring the intended behavior;
both assertion runs exited 1 on the baseline. Their retained observation and
assertion logs accompanied the scripts. The DuckDB replay required both
measured defects and successful ordinary controls, and exited zero. That
replay was evidence of the baseline failures, not a regression suite claiming
they had been repaired. All fixtures were disposable.

The primary investigator independently reproduced all four findings using
separate witnesses retained as `local/independent-findings.R` and
`local/independent-arrow.R` inside the evidence directory. In particular, the
Arrow identifier check compared each identifier with its row's included or
omitted occurrence; comparing identifier vectors without their associated
values could miss the corruption.

## D1: repeated dtplyr updates bypassed metadata safety

An immutable lazy input held integer `g = 11:12` and `v = 1:2`. A single
`mutate()` first assigned `g = v`, then `g = factor(g)`. Ordinary collection
returned factor values `1, 2`. Expanding `rollup(g)` with the default text
Margin label and `.id = "sid"` returned missing values for both detail rows.
Expanding the explicitly collected input retained `1, 2, Total, Total`.
The caller's source table was unchanged.

The mutation stored two entries named `g`. The metadata guard iterated those
names but retrieved each expression by name, inspecting the first entry twice.
It admitted the later factor creation and evaluated it over zero-row roots,
obtaining a factor proxy with no levels. Factor restoration then replaced the
actual detail values with missing values. A single unsafe update was refused
by the existing guard.

A later update whose type depended on row count likewise produced a character
column in the actual input and an integer column in the proxy. Numeric
Grouping specification selection included that column incorrectly; character
selection and typed fixed-key selection omitted it. A recording callback also
ran on zero-length synthetic input during construction. This established
unsafe expression evaluation on empty input, not a full-source read.

The retained 90-comparison matrix covered zero, one, and three input rows;
factor, row-dependent type, and callback derivations; four typed inspection
variants; and six expansion label/sort combinations. It recorded 65 matching
answers, 10 different plans, 12 value/type differences, and three package-only
expansion errors. All caller tables remained unchanged. All 30 callback cases
used the unsafe empty-input path even where their answers matched.

ADRs 0002 and 0020 governed faithful metadata and refusal without unrequested
source reads; ADR 0012 governed factor restoration. Closed #782 addressed
scalar overwrite metadata, #995 selector dispatch, and #996 cached row
predicates. The uninspected later duplicate entry required none of those
previous fixtures.

## A1: narrowed Arrow schemas concealed live partition constraints

Two Hive partitions `sid=1` and `sid=2` held Parquet values `g = 9, 8` with
row identifiers `1, 2`. Opening the Dataset with an explicit schema containing
only `g` and the row identifier retained the partition declaration. Ordinary
collection returned the correct two public columns. Expansion of `rollup(g)`
with a NULL label and `.id = "sid"` replaced the Grouping set identifiers
with the per-source partition values, in both grouping-set occurrences.

A partition named `..marginplyr_arrow_dimension_1` replaced both detail values
and omitted-dimension missing labels with `1, 2`. A partition named
`..marginplyr_sort_1` made a sorted expansion without a public identifier return
`9, NA, 8, NA` instead of `8, 9, NA, NA`. Suffix collisions and a private alias
for a requested `..1` identifier reproduced the same boundary.

The name scan read the Dataset's narrowed public schema, which did not name
the partition constraints still participating in Arrow's union projection.
Explicit-schema opening, `WithSchema()`, and a narrowed UnionDataset all
reproduced. Closed #992's full-schema Dataset followed by ordinary query
projection passed, as did explicit materialization and Table controls. This
was a deeper remaining boundary after that repair.

The 576-case matrix crossed six collision/control scenarios, eight input
shapes, empty/populated sources, NULL/text labels, and three sort modes. It
recorded 436 passes, 120 value differences, 20 order differences, and no
errors. All 216 full-schema projection/materialization/Table controls and all
96 unrelated-partition cases passed. Included-only expansion and ordinary
Arrow mutation plus union over the narrowed source passed as well.
ADRs 0016, 0018, and 0033 assigned the affected result properties to marginplyr;
ADR 0020 constrained a repair to remain lazy.

## S1: DuckDB computation erased current lazy metadata

A DuckDB input with ordinary columns `g = a, b`, `v = 1, 2`, and a `.env`
payload expanded into four rows under a rollup with NULL labels. Grouping the
result by `g` and summarizing returned three rows: `a = 1`, `b = 2`, and
missing `g = 3`. Computing that grouped result first discarded its grouping;
the same summary then returned one row with total 6.

The `.env` expansion carried a package subclass whose dedicated `compute()`
method returned a plain lazy table without reconstructing current grouping or
window-order metadata. An ordinary DuckDB lazy table retained both through
computation. The matrix covered `.env` as payload, dimension, fixed key, or
identifier, empty/populated input, and temporary/persistent destinations: all
16 cases lost grouping `g` and caller-applied window ordering `v`.

This concerned metadata from ordinary verbs applied by the caller, rather
than a promise of physical table order or of Margin order surviving arbitrary
pipelines. ADRs 0016 and 0018 delegated those later verbs to ordinary dbplyr
semantics; the package's own method changed those semantics. Closed #730 had
covered `.env` values and supported materialization, not this metadata loss.

## S2: invalid DuckDB index requests wrote the destination before failing

A destination containing only `sentinel = 777` was overwritten with the four
expansion rows when `compute()` requested an index on an absent column. The
call then raised a DuckDB binder error while creating that index. Ordinary
dbplyr computation rejected the absent index column before any write and
retained the sentinel. For a previously absent destination, the failed package
call left a newly created expansion table while the ordinary call left none.

The dedicated method skipped ordinary index-column preflight. The 16-case
matrix crossed ordinary/package methods, ordinary/unique index arguments,
temporary/persistent tables, and create/overwrite requests. All eight package
cases changed destination state before error; all eight ordinary controls
preserved it. This finding concerned locally knowable invalid columns. It did
not require transactional recovery for every backend failure, such as duplicate
values encountered while building a valid unique index.

## Additional coverage and excluded leads

- Grouping specification selection used seed `10292026`: 367 admitted
  depth-three expressions, local and immutable-root dtplyr inputs, and `.by`
  plus all five constructors. The 4,404 comparisons had 2,276 matching successful
  selections, 2,052 matching External-condition classes, and 76 expected
  empty-dimension refusals. One injection AST was excluded before interpretation.
  The [selection evidence](2026-10-29-expand-grouping-deep-hunt/spec/README.md)
  records the oracle correction and the exact scope. There was no new
  independent constructor finding.
- Constructor-result capture added 585 comparisons: 135 successful matches
  and 450 matching condition classes. Constructor-shaped caller functions ran
  once per public call. Twelve implicit-cast comparisons added no discrepancy
  and invoked no custom cast method; closed #995 was not reported again.
- Local expansion covered 1,026 cases across 19 type families, empty/populated
  inputs, NULL/NA/text labels, three sorts, and dimension/fixed/payload roles.
  There were 963 direct oracle matches, six differences in bare-vector name
  attributes, and 57 refusals. Ordinary subscript/mutation/union reproduced the
  name attributes; ordinary hms character conversion plus union reproduced its
  empty-input type error; ordinary `arrange()` reproduced Surv recursion. Other
  refusals were the documented non-missing packed/array-label refusals. These
  were excluded under ADR 0016, not promoted to package findings. An invalid
  timeDate fixture was excluded before admission because tibble refused it.
- An independent local ordering oracle used seed `29102026` for 1,000 random
  explicit plans. It derived occurrence copies, typed missing values, Grouping
  bits, per-row missingness, fixed-key priority, and identifier ties independently
  before applying vctrs ordering. List, packed, matrix, factor, temporal,
  integer64, and scalar keys, repeated sets, empty inputs, and absent/public
  identifiers all matched exact output values, types, names, and order.
- PostgreSQL added 1,485 calls across 33 type families, five derived shapes,
  three plans, and three sorts. The 1,395 successful calls matched cardinality,
  public columns, and per-source copy multiplicity. Ninety JSON/point/XML sort
  errors also occurred in ordinary backend controls. A separate `pg_typeof()`
  matrix had 198 exact physical-type matches. The broader matrix was not a full
  row-value oracle. Its scripts required an explicitly supplied disposable
  PostgreSQL database; no live service was needed for the focused findings.
- Empty SQLite BLOB dimensions collected directly as `blob(0)` but materialized
  as logical(0). Ordinary computation over the same empty source reproduced the
  loss, so it was excluded as a backend composition limitation under ADR 0016.

The ten focused existing test files covering the grouping interface, selection
budget, injected and cyclic specifications, Grouping plan, inspection, expansion,
packed/matrix keys, and metamorphic semantics passed on the baseline. Passing
measurements established only their exercised inputs. The hunt did not enumerate
arbitrary caller code, all custom classes, every Dataset implementation,
unbounded plans, every package version, or unavailable SQL services.

## Publication boundary

Changes were confined to this dated note and evidence under `investigation/`,
which the existing source-package exclusion covered. They were Repository-only
under [Local review-ready checks](../design/agents/local-checks.md); no package
implementation or regression suite was changed. Final lint, reference,
context-budget, replay, diff, and Standards/Spec review outcomes were recorded
in the pull request.
