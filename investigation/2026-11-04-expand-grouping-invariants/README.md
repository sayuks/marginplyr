# Expansion and Grouping specification invariants

Investigated: 2026-11-04
Baseline: `7a05a1e42d53af62c057a30ad1892ffb35d4eef8` (fetched `origin/main`)

A dedicated worktree hosted four parallel investigations. The [earlier
November 4 expansion hunt](../2026-11-04-expand-grouping/README.md), the
[November 3 composition hunt](../expand-grouping-greedy-hunt-2026-11-03.md),
the [November 2 expansion hunt](../expand-grouping-greedy-hunt-2026-11-02.md)
and the [tidyselect hunt](../2026-11-04-tidyselect/README.md) supplied prior
witnesses and exclusions. The fresh baseline included the shared-source SQL
and dtplyr cache fixes from #1262. Earlier measurements did not count toward
this investigation.

## Findings

| Finding | Repair issue | Observed failure |
| --- | --- | --- |
| D1 | [#1271](https://github.com/sayuks/marginplyr/issues/1271) | An immutable dtplyr expansion lost a row copy when the source predicate changed its answer |
| A1 | [#1272](https://github.com/sayuks/marginplyr/issues/1272) | An Arrow expansion changed a native volatile payload or row membership across copies |
| G1 | [#1273](https://github.com/sayuks/marginplyr/issues/1273) | A caller-resolved parenthesized callee ran its Grouping selector twice |
| G2 | [#1280](https://github.com/sayuks/marginplyr/issues/1280) | Literal variadic forwarding made a name-only selection request typed metadata |
| D2 | [#1281](https://github.com/sayuks/marginplyr/issues/1281) | dtplyr join metadata used the wrong lexical dispatch and silently changed the expansion |
| S1 | [#1282](https://github.com/sayuks/marginplyr/issues/1282) | SQLite typed computation rebound the input name to its empty destination |

Each issue described one independently verifiable behavior and had no blocking
edge. The user delegated the issue split. The two source-sharing issues used
separate tickets because the dtplyr deferred query and Arrow execution graph
have distinct execution boundaries. The six issues received `bug` and
`ready-for-agent` labels. The remote main ref matched the baseline before each
batch of issues was published.

### D1 and A1. One evaluated source within a collection

The public [expansion contract](../../man/expand_with_margins.Rd) describes
one copy of each input row for each grouping set. In the
[dtplyr probe](local-dtplyr-source-evaluation.R), a one-row filter accepted its
row on its first call and rejected it on the second. Construction invoked it
zero times. Rollup collection invoked it twice and returned only the detail
occurrence. An ordinary source collection ran the predicate once. Expansion
after explicitly caching the filtered step produced both copies, while
placing the filter after a cache reproduced the defect. Of 48 comparisons,
18 rollups failed and 30 matched. The caller's table stayed intact. The
single-set and cache placement controls separated repeated query evaluation
from branch mutation and eager evaluation during construction.

The [Arrow probe](local-arrow-source-evaluation.R) used native
`arrow_random()` over an Arrow Table with one stable row identifier.
Across 16 collections the copies never shared the same payload. Moving the
kernel into a filter admitted exactly one copy in 20 of 32 collections.
The other collections admitted neither copy five times and both seven times.
One evaluation of the input predicate permits either zero or two copies.
Both materialized controls agreed with that invariant. The Arrow results
stayed lazy through construction. The payload and filter witnesses distinguish
repeated source evaluation from the way a Margin label is represented.
Closed #1243 and merged #1262 shared a SQL source within a statement, and
#1003/#1241 dealt with dtplyr cache mutation and names.

### G1 and G2. Selector evaluation and metadata

In the [parenthesized-callee probe](spec-parenthesized-callee.R), a caller
binding of the parenthesis operator supplied an ordinary selector for a
constructor-shaped call. The selector returned column `a` once, then `b`.
Inspection and expansion both selected `b` after two reads. All 300 cases
across five parents, five child spellings, three qualification modes, one
or two pairs of callee parentheses and two consumers reproduced. All 150
direct-head controls selected `a` after one read. A warning repeated and an
error reachable only on the second invocation escaped. The case extends the
evaluation budget in [ADR 0008](../../design/adr/0008-centralize-grouping-specification-kind-rules.md)
and the ordinary lookup covered by ADR 0019. #946 and #1038 established
retention for direct and qualified constructor selectors. The new witness
changes lookup of the parenthesized callee itself. #1240 concerns ambiguity
around a nested specification name.

The [variadic probe](spec-variadic-names.R) passed literal strings or no
arguments to a wrapper that selected fixed keys or Grouping dimensions via
`c(...)` or `all_of(c(...))`. All eight derived dtplyr cases reported
unsafe typed metadata. The 16 local and dtplyr root controls and the inlined
derived selections matched ordinary tidyselect. A genuinely type-dependent
`where()` control was refused without invoking its predicate. Four forwarded
missing-name cases sent one DuckDB `LIMIT 0` each at the DBI boundary. The
inlined missing-name control sent none. [ADR 0020](../../design/adr/0020-ask-before-reading-a-lazy-input.md)
exempts name-only dtplyr inspection from typed metadata and
[ADR 0005](../../design/adr/0005-reject-local-errors-before-backend-reads.md)
requires locally known errors before a metadata read. #1144 covered an
internal empty-dots error. #1265 covers lexical environment-pronoun values
while #1277 covers direct selection-context lookups.

### D2. Lexical dispatch in typed dtplyr join inspection

The [join probe](local-dtplyr-join-dispatch.R) computed integer column `w`
on the left of a join. In the right environment, a caller function bound to
the addition operator returned character for an empty input and performed
ordinary addition otherwise. Ordinary collection evaluated it on one row
and returned integer `w = 11L`. Typed inspection called it on zero rows and
selected `w` as a character dimension. Expansion with the same predicate
produced three rows and character `w`. The unmasked control produced two
rows and retained the integer payload. In the converse witness, both Margin
consumers forced an operator promise from the left environment that ordinary
joined collection left untouched. The probe distinguished the environment
checked for safe metadata from the effective environment of the combined
query. ADR 0020 allows a faithful zero-row proxy or a Package condition
when faithful metadata cannot be established. #1043 concerned an active
binding on a single derived step and #1024 concerned a cached graph mutation.

### S1. SQLite source rebinding during direct materialization

The [SQLite probe](sqlite-compute-source-shadow.R) put two rows in
`main.facts`. Expansion of the unqualified source with a Margin order
collected four rows. Direct `compute()` to temporary `facts` returned a
table with zero rows. A DBI trace showed a zero-row `CREATE TEMPORARY TABLE`
followed by an `INSERT` whose unqualified source now resolved to the new
temporary table. Ordinary dbplyr created and populated its destination in
one statement and copied the original source. Source qualification or a
different destination name preserved the expanded rows.

Across 48 cases, 21 materializations yielded zero rows, six raised a
union-column-count error, and 21 matched. The matrix crossed bare, filtered,
opaque and qualified sources with same-name, case-equivalent, explicitly
temporary and different destinations, plus sort-only, occurrence-only and
combined results. The persistent source stayed intact and failed writes
rolled back the destination. [ADR 0031](../../design/adr/0031-preserve-sqlite-typed-dimensions-under-margin-order.md)
governs direct typed materialization. #661/#667 covered destination resolution
and #755 covered interruption recovery. This witness concerned input name
resolution between two statements during a successful or failed computation.

## Additional coverage

| Probe | Completed observations |
| --- | --- |
| [Inspection and expansion correspondence](inspection-correspondence.R) | 300 matches across five backends, widths 0, 1, 30, 31 and 32, implicit or explicit fixed keys, duplicate retention or removal, and three sort requests. This checked plan fields, exact row copies and types, and ungrouped output. It canonicalized copies by occurrence and source identifier, so it did not verify physical Margin order. |
| [Grouping identity correspondence](inspection-identity.R) | 60 matches across the same five backends, 0, 1, 2, 30, 31 and 32 dimensions and unique or retained-duplicate plans. The contextual default mask matched the inspected value through width 31, explicit fixed keys contributed zero, and width 32 produced the required helper refusal and inspection missing value. |
| [Independent Grouping family oracle](spec-family-invariants.R) | 98 seeded nested specifications gave 294 inspection comparisons and 196 expansion comparisons. The oracle checked membership, first-occurrence order, duplicate refusals and row copies. Two generated specifications exceeded the fixture cap of 250 occurrences and were excluded. |
| [Arrow physical values](local-arrow-physical-types.R) | 48 exact Arrow array comparisons crossed 24 types with dimension and payload roles, including wide integers, decimals and nested types. No R downcast served as the equality oracle. |

Additional SQL exploration exercised public names and composition across live
SQLite, DuckDB and a private PostgreSQL 17.11 server. It covered direct
execution, CTE rendering, compute, collapse, reinput, head, filter, mutate,
joins and unions. Native controls also failed for sorted union on the three
engines and for certain pronoun names in ordinary dbplyr compute. Those
observations did not establish package defects. SQLite collation comparisons
matched the admitted ordinary controls. A view over a table whose name matched
the generated source CTE prefix expanded correctly in SQLite and DuckDB.
These SQL exploratory comparisons are supplementary observations. The
focused SQLite replay supplies the retained evidence for S1.

Duplicate checking covered open and closed GitHub issues and PRs. Search
terms included backend plus `volatile expansion` or `shared source`,
`parenthesized callee`, `parenthesized call head`, `variadic name-only grouping`,
`dots metadata dtplyr`, `dtplyr join environment` or `join dispatch`, and
`SQLite temporary shadow` or `SQLite compute same name`. The adjacent reports
and their dispositions are named with the findings.

## Replay and limits

Run from the repository root in fresh R processes. The expected-behavior
assertions below intentionally exited with status 1 against the investigated
baseline. Omit `--assert` for observation mode and use `--matrix` for the
expanded SQLite comparison.

```sh
evidence=investigation/2026-11-04-expand-grouping-invariants
for script in local-dtplyr-source-evaluation local-arrow-source-evaluation \
  spec-parenthesized-callee spec-variadic-names local-dtplyr-join-dispatch \
  sqlite-compute-source-shadow
do
  Rscript "$evidence/$script.R" --assert
done
```

The environment was macOS arm64, R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0,
rlang 1.3.0, tidyselect 1.2.1, vctrs 0.7.3, dtplyr 1.3.3,
data.table 1.18.6.1, Arrow 25.0.1, DuckDB 1.5.5 and RSQLite 3.53.3.
The Arrow volatile counts describe the observed draws. The oracle checks a
structural invariant independent of any particular draw. The finite matrices
cover the reported fixtures on this SHA. They leave open other backend
versions, hosted services, extension classes and unbounded plan sizes.

The additions are dated evidence and standalone replays under the existing
investigation build exclusion. The submission therefore used Review-ready
`source` scope. The five checks and the submitted SHA are recorded on the PR.
