# Expansion and Grouping specification greedy bug hunt

Investigated: 2026-10-31
Baseline: `339315c387f31deace77566f5d3294b15b323865` (`main`)
Scope: dated investigation evidence and implementation tickets; no fixes

A dedicated worktree was created from local `main`; fetching confirmed that
`origin/main` named the same commit. Four parallel tracks investigated local
expansion, Grouping specification semantics, Arrow/dtplyr composition, and SQL
composition with independent reproductions. The October 30
[exhaustive expansion hunt](expand-grouping-exhaustive-hunt-2026-10-30.md),
October 29 [follow-up](expand-grouping-followup-2026-10-29.md),
[deep hunt](expand-grouping-deep-hunt-2026-10-29.md), and
[systematic hunt](expand-grouping-systematic-hunt-2026-10-29.md), together with
recent nesting and summary investigations, supplied prior coverage and Issue
boundaries. Their measurements were not counted as new execution.

The user delegated ticket granularity and publication without a quiz. Three
independent P2 findings became `bug`, `ready-for-agent` tickets through
`to-tickets`; none needed a prefactoring ticket or a blocker. No package
implementation, regression tests, generated files, metadata, or build rules
were changed. The replay assertion modes intentionally remained red.

## Findings and ticket boundaries

| Finding | Ticket | Independently verifiable behavior |
| --- | --- | --- |
| L1 | [#1093](https://github.com/sayuks/marginplyr/issues/1093) | Construct compatible typed-missing dist/logLik dimensions without losing their scalar class |
| L2 | [#1094](https://github.com/sayuks/marginplyr/issues/1094) | Combine supported columns using ordinary inner-name rules while retaining exact public column names |
| S1 | [#1095](https://github.com/sayuks/marginplyr/issues/1095) | Keep Boolean-word SQLite expansions executable through ordinary downstream limiting/filtering |

L1 reached omitted-value construction; L2 reached eager branch combination even
when the affected column was merely payload; S1 reached the stored SQLite query
after an ordinary downstream verb disabled the direct companion. Each boundary
had its own successful control and could be fixed and verified independently.

## L1: scalar class prototypes lost their class

A one-row tibble held the distance between 0 and 1 as a `dist` key and a
source-row identifier. Included-only expansion succeeded. A NULL-label rollup
raised `vctrs_error_ptype2`, reporting incompatible `dist` and `double` branches.
Ordinary mutation using a row-sized `vec_init()` value followed by `union_all()`
returned the original distance, a typed-missing distance, and both payload
copies. An independent three-distance fixture reproduced the same contrast.

The ranked hypotheses were class loss during NA subsetting, an unsupported
ordinary union, and missingness validation refusing a valid prototype.
Subsetting returned a bare double while `vec_init()` retained `dist`; ordinary
union succeeded; the public error arose during type combination. These
observations supported prototype construction as the cause. The same boundary
affected `logLik`, whose class also disappeared on scalar NA subsetting.

The focused 1,944-comparison matrix produced 1,512 matches and 432 package-only
type errors, split equally between direct dist and logLik dimensions. Every
ordinary control succeeded and every source remained unchanged. Packed,
fixed-key, and payload controls passed. Closed #1071 covered one-dimensional
array-shaped tables; these non-array scalar vectors remained on the other
prototype path. The NULL-label contract and ordinary composition supplied the
expectation without adding arbitrary attribute restoration under ADR 0016.

## L2: public-column name checks reached vector-internal names

The minimal input was one tibble row with `g = "a"` and a payload containing
the first element of `summary(1)`. Default expansion of `rollup(g)` raised
`vctrs_error_names_must_be_unique`, naming repeated inner `Min.` elements.
Ordinary union of the input and a copy with `g = "Total"` returned both payload
values equal to 1, applying its ordinary inner-name repair. Included-only
expansion succeeded. No omitted classed-dimension prototype was needed.

The ranked hypotheses were a name-repair callback reaching inner metadata,
a malformed missing prototype, and invalid public input names. The public names
were valid; ordinary union succeeded; and the branch binder's callback observed
both outer column names and inner vector names. Default `vec_rbind()` succeeded
on the same branches. These controls supported callback scope as the cause.
A full summary vector used as a missing-labelled dimension manifested the same
boundary as an empty-inner-name refusal.

The focused 1,836-comparison matrix produced 1,188 matches and 648 package-only
name errors. Every ordinary bind_rows control succeeded and every source
remained unchanged. Failures included direct tibble and packed columns in
dimension/fixed/payload roles; zero-row cases passed. Direct data.frame and
data.table cases could pass, so no all-container failure was inferred.
Three-branch controls used ordinary bind_rows because sequential union_all
itself changed the class before its third input. Closed #784 promised exact
public names, not a stricter policy for class-internal names. L2 required both
that promise and ordinary class combination to hold together.

## S1: direct SQLite alias recovery did not survive derived queries

A one-row SQLite table held `g = "a"` and integer payload `TRUE = 7`.
Default expansion of `rollup(g)` collected directly with the correct public
schema and two payload copies. Collecting `head(result, 1)` or an identity
filter instead raised `no such column: TRUE`. Neither a public identifier nor
Margin order was required. A second investigator reproduced it with payload
19 and absent/present identifiers.

The ranked hypotheses were discarded companion alias recovery, an ordinary
SQLite/dbplyr limitation, and an identifier/order-specific failure. The direct
companion was active before the downstream verb and inactive afterwards;
derived SQL used the unrecovered aliases in the stored lazy query. Both the
source table and an explicitly computed expansion supported ordinary limiting
with the exact public name. Ordinary manual unions also executed and retained
the values, but named their Boolean column `column2`; they were execution/value
controls, not public-name controls. These distinctions were retained rather
than claiming ordinary union preserved the spelling.

The 384-case focused matrix covered TRUE/FALSE and mixed-case spellings,
four public roles, empty/populated input, NULL/text labels, three sorts, and
head/identity-filter derivations. Every direct package result matched local
expansion in names, typed values, and row bags. All ordinary controls executed;
all package derivations raised the missing Boolean-word column error.
Closed #826 covered direct collection/materialization. ADR 0031 delegated
later verbs to ordinary dbplyr semantics; S1 required usable queries over the
public schema, without adding a Margin-order or direct-type guarantee for
derived results. ADR 0020 excluded implicit materialization during construction.

## Additional executed coverage and excluded leads

| Probe | Executed scope | Result on the baseline |
| --- | --- | --- |
| Additional local types | 1,404 comparisons across 26 families, empty/1/3 rows, direct/packed and dimension/fixed/payload roles, three sorts | 1,350 all.equal matches, 48 ordinary-sorting failures, six semantically equal numeric_version representations; no additional finding |
| Grouping selection differential | 18,348 raw comparisons across 1,529 expressions, two frames and six selection positions | Injection-phase comparisons were excluded as described in the specification track; 17,160 comparable cases had no unexplained difference |
| Independent grouping-family oracle | 3,072 inspection/expansion call pairs over four dimensions, subset pairs, duplicate policies, empty/populated and explicit/persistent fixed groups | 3,008 matching pairs and 64 matching required duplicate refusals |
| Arrow/dtplyr composition | 3,960 comparisons across 22 source shapes, three sizes, five plans, two labels, three sorts | 3,330 matches, 360 documented dtplyr metadata refusals, 270 ordinary source failures; no new package-only difference |
| Hidden Arrow filesystem boundary names | 36 asserted partition-name/schema/order cases | All passed |
| SQL repeated expansion/computation | 912 classified observations across two live backends, seven name sets and nine derivations | 752 matches, 18 inherited/documented errors, 142 failures before the second expansion; Boolean failures isolated as S1 |
| DuckDB index shapes | Ten requests, two index kinds, ordinary/package paths: 40 executions | Malformed-request failures and destination effects shared with ordinary computation; excluded |

The Grouping specification probe's direct tidyselect oracle and public capture
performed different injection phases on raw `!!`/`!!!` ASTs. All 1,188
comparisons containing those injection forms were excluded as a population,
including matches, leaving 17,160 comparable cases: 16,596 matching answers or
conditions, 554 required empty-dimension refusals, and ten required nested-kind
refusals. Earlier capture investigations supplied context but were not counted
as a new injection proof. The family oracle independently checked set
membership, fixed columns, occurrence IDs, Grouping bits/identifiers, and
expanded values rather than trusting inspection alone.

The Arrow/dtplyr oracle compared collected-source local expansion, normalized
scalar values/types, and requested key sequences, allowing payload ties. It
was not an independent local semantic oracle, complete physical-type proof,
warning comparison, or source-mutation audit. Two initial source builders were
corrected and all 360 affected cases rerun. A decimal64 order candidate was
excluded because ordinary Arrow union/full-key ordering returned the same wrong
order, and ordinary single-column ordering refused the type. Decimal128
controls sorted correctly; no upstream report was filed.

Seven initial DuckDB SQL window differences disappeared when occurrence plus
source-row order removed ties between expanded copies. Ordinary SQLite ordered
union and `.env` projection controls reproduced separate upstream limitations.
The SQL oracle used character-normalized row bags and was not a general
physical-type or complete-order proof. The focused Boolean matrix and ordinary
public-name table controls, rather than an undifferentiated source-error count,
established S1.

## Replay, environment, and publication

The [evidence bundle](2026-10-31-expand-grouping-greedy) contains track READMEs,
replay scripts, classified CSVs, terminal logs, and environment versions.
Retained text logs omit trailing whitespace; CSV data were left intact.
Run from the repository root:

```sh
Rscript investigation/2026-10-31-expand-grouping-greedy/local/reproduce-dist-prototype.R --assert
Rscript investigation/2026-10-31-expand-grouping-greedy/local/reproduce-inner-name-repair.R --assert
Rscript investigation/2026-10-31-expand-grouping-greedy/sql/boolean-composition.R --assert
```

Each assertion command exited 1 on the pinned baseline. Observation mode
required successful controls, printed the failures, and exited zero. Matrix
runner success meant measurements completed, not that the reported defects
were fixed. The independent witnesses used different dist/SQLite values.

The environment was macOS arm64 with R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0,
vctrs 0.7.3, rlang 1.3.0, tidyselect 1.2.1, dtplyr 1.3.3,
data.table 1.18.6.1, Arrow 25.0.1, DuckDB 1.5.5, and RSQLite 3.53.3.
Fixtures used disposable files and private in-memory databases. No external
database service was needed for a finding. No live PostgreSQL/SQL Server,
other operating system, full dependency-version matrix, arbitrary callback or
class enumeration, or unbounded/exponential plan proof was attempted.
Successful comparisons established only their stated inputs.

All additions were dated evidence under the existing source-package exclusion
for investigation notes, a Repository-only change under
[Local review-ready checks](../design/agents/local-checks.md). The session's
common-CLI instruction was honored separately. Its exact committed snapshot
and terminal outcome, package linters/verifiers, and Standards/Spec review
record belonged to the pull request. CI was not monitored.
