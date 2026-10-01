# SQL query boundaries and order-sensitive Margin composition

Investigated: 2026-10-01
Target: `73f84cada8bba2d1b194c5f74d4d9d77a89a468c` (marginplyr 0.1.0)
Starting worktree: clean

## Result and disposition

One actionable defect was confirmed and published as
[#769: Preserve LIMIT boundaries when composing SQLite Margin results](https://github.com/sayuks/marginplyr/issues/769).
A valid, deterministically limited SQLite lazy input was rejected by a Margin
summary, including a one-set summary, and by expansion. The preprocessing,
independent SQL, ordinary summary-then-union, and materialized-input controls
succeeded. No executable Margin path with equivalent preinputs silently
changed rows, multiplicities, computed values, grouping grain, or Parent/Total
share denominators in the exercised cases. This is bounded negative evidence, not a guarantee for arbitrary queries.

The principal matrix contained 18 workflows, four configurations (DuckDB
native/portable, SQLite portable, PostgreSQL native), and direct/bounded input
paths: **143 of 144 Margin results agreed with independent SQL; one SQLite
direct LIMIT path failed to construct**. All 54 backend/workflow preprocessing
triples agreed, and all 54 hand-total checks passed after the harness corrections
described below. Additional boundary checks produced 134 passes and two
manifestations of #769 (sorted direct summary construction and direct expansion).
Their counts were DuckDB 62/62, PostgreSQL 35/35, SQLite 37/39.
A user-authorized extension added 18 order-sensitive workflows, independent
follow-up controls, nonadditive summaries, repeated Margin composition and
additional grouping specifications; its separately classified results follow
below. Counts name assertions, not distinct bugs or SQL statements.

The ticket was approved as one independent vertical slice, with no blockers.
Summary, expansion, and the collapse supplement share the package-owned input
composition problem; they were not published as separate bugs. No product fix,
contract change, dependency change, CI change, or merge was performed.

## Plan, contract, and evidence reused

The approved campaign asked whether Margin processing aggregates only the
result of user preprocessing. It prioritized successful-but-wrong results and
used small deterministic integer/string fixtures, static databases, and one
fixed dependency graph. It compared direct composition (A), explicit DB-local
preprocessing materialization (B), and independent ordinary SQL (C). A/B equality
alone was insufficient. SQL structure established exercised paths and helped
explain failures; formatting or subquery counts were not correctness criteria.

`CLAUDE.md` and its `AGENTS.md` reference, `CONTEXT.md`, the relevant ADRs,
public roxygen, implementations, tests, and dated investigations were read.
The governing contracts were the lazy-input and native/portable summary
semantics in `summarize_with_margins()`, expansion row preservation, ADR 0010
(Parent shares), ADR 0014 (SQL denominator mapping), ADR 0017 (Grand total
within fixed partitions), ADR 0018 (result-order/window-metadata scope),
ADR 0020 (requested execution), and ADR 0031 (SQLite direct result boundary).

| Boundary and public route | Implementation inspected | Existing evidence | Added observation |
| --- | --- | --- | --- |
| Preprocessing into `summarize_with_margins()` | `prepare_margin_operation()` and `stage_margin_summaries()` | Public grouped-input/fixed-key contract; grouping-interface/backend tests | Explicit ungrouping of preprocessing partition `h`; deliberate implicit `p` grouping compared with explicit `.by` |
| Native summary projection into grouping sets | `native_summary_select()`, `summarize_margin_native()`, custom grouping-set SQL builder | DuckDB/backend tests and live PostgreSQL near-floor note | LIMIT, window, overwritten columns, and preaggregated columns through the transplanted SELECT |
| Portable source reuse and branch union | `summarize_margin_union()`, private key mutation, branch summarization, `sql_margin_type_anchor()` | Branch-union tests; metamorphic row duplication and expand-then-summarize | DISTINCT without a diagnostic ID, asymmetric UNION ALL, and input-LIMIT anchors |
| Summary into contextual shares | `apply_joined_shares()`, Parent mapping, `build_total_denominator()` | Share/backend tests and metamorphic conservation/duplication | Preprocessed numerator and denominator population/grain; independent Parent and Total SQL joins |
| Margin result into collection/materialization and later verbs | `finalize_margin_operation()`, portable window-order restoration, SQLite typed collect/compute | Margin-order and SQLite typed-order tests; composed share tests | Sorted direct compute, finite collect, CTE rendering, filter/select, and multiplicity-preserving joins |
| Preprocessing into expansion | `expand_margin_union()` and finalizer | Metamorphic source duplication and prior expansion checks | LIMIT, DISTINCT, and asymmetric-union source multisets repeated per occurrence |

The `recipes.qmd` and `get_started.qmd` reporting workflows supplied public
composition and presentation context. `metamorphic-testing-margin-semantics.md`
and `test-metamorphic-margin-semantics.R` already exercised source duplication,
share conservation, and expansion relations. `test-share-backends.R` already
compared result-side filter/select/mutate with a completed result; margin-order
tests exercised retained window metadata, partly through simulation. Those
observations are not a substitute for preprocessing windows executed by a DB.

`2026-09-30-postgres-near-floor.md` established live native summaries/shares,
expansion, collect/compute parity, and construction-read observations on its
recorded versions and fixture. `downstream-integration-2026-10-01.md` established
installed external-consumer/public-generic routes and comparator calibration.
This campaign reused their isolation and comparison methods, rather than
regenerating consumer packages, version matrices, or their evidence directories.
The collation repair #767 was in the target HEAD; special collation rules were
excluded. Existing coverage was assessed from fixture construction and
assertions, not from a missing search token.

## Fixed environment and isolation

The experiment used macOS arm64, R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0,
rlang 1.3.0, tidyselect 1.2.1, DBI 1.3.0, duckdb 1.5.5 (engine `v1.5.5`),
RSQLite 3.53.3 (engine `3.53.3`), and RPostgres 1.4.10 against live PostgreSQL
17.11 (Homebrew). lintr was 3.4.0, pkgload 1.5.3, and jarl 0.6.0.

The dedicated root was
`/private/tmp/marginplyr-sql-boundaries-20261001-dgg12lyo`.
An exact `git archive` of the target was installed into its own library. The
existing installed dependencies were copied read-only with their hard closure;
`R_LIBS_USER` named the copy, `R_LIBS_SITE=NULL`, `Rscript --vanilla` and a private
TMPDIR were used. Experiments used installed public functions, not load_all or
namespace replacements. Testthat and its hard closure were added only for the
package-aware lint environment after experiments; no existing dependency
version was replaced. The experiments did not use testthat. Additional lint-only
packages absent from the recorded initial closure were testthat 3.3.2, brio 1.1.5, jsonlite 2.0.0, praise 1.0.0, waldo 0.6.2, diffobj 0.3.8, crayon 1.5.3.

DuckDB used `shared_home = FALSE`, a 512 MB engine memory limit in the principal
matrix, and one thread. SQLite used new in-memory connections. PostgreSQL used
a newly initialized cluster, private Unix socket, port 55483, no TCP listener,
32 MB shared buffers and a 10-second statement timeout. Sandbox shared-memory
initialization and socket connection restrictions required sandbox-external
commands against this same private cluster. No existing database or ordinary
R library was modified. Connections were disconnected after each script and
the private server was stopped before publication.

The following sorted `Package Version` closure was copied for the experiment
and lint tooling. Its SHA-256, with LF separators and no final LF, was
`5b7bc25e5152b722a33bcdc5064cb8834722c15b264322be8eb0a3b7ac82fc2c`.
Base/recommended packages came from the same R installation.

```text
DBI 1.3.0
R6 2.6.1
RPostgres 1.4.10
RSQLite 3.53.3
bit 4.6.0
bit64 4.8.6
blob 1.3.0
cachem 1.1.0
callr 3.8.0
cli 3.6.6
codetools 0.2-20
cpp11 0.5.5
dbplyr 2.6.0
desc 1.4.3
digest 0.6.39
dplyr 1.2.1
duckdb 1.5.5
evaluate 1.0.5
fastmap 1.2.0
fs 2.1.0
generics 0.1.4
glue 1.8.1
grDevices 4.6.1
graphics 4.6.1
highr 0.12
hms 1.1.4
knitr 1.51
lifecycle 1.0.5
lintr 3.4.0
lubridate 1.9.5
magrittr 2.0.5
memoise 2.0.1
methods 4.6.1
otel 0.2.0
pillar 1.11.1
pkgbuild 1.4.8
pkgconfig 2.0.3
pkgload 1.5.3
processx 3.9.0
ps 1.9.3
purrr 1.2.2
rex 1.2.2
rlang 1.3.0
rprojroot 2.1.1
stats 4.6.1
stringi 1.8.9
stringr 1.6.0
tibble 3.3.1
tidyr 1.3.2
tidyselect 1.2.1
timechange 0.4.0
tools 4.6.1
utf8 1.2.6
utils 4.6.1
vctrs 0.7.3
withr 3.0.3
xfun 0.60
xml2 1.6.0
xmlparsedata 1.0.5
yaml 2.3.12
```

Primary sources consulted were [dbplyr 2.6.0 Verb translation](https://dbplyr.tidyverse.org/articles/translation-verb.html),
[collapse](https://dbplyr.tidyverse.org/reference/collapse.tbl_sql.html),
[compute](https://dbplyr.tidyverse.org/reference/compute.tbl_sql.html), and
[collect](https://dbplyr.tidyverse.org/reference/collect.tbl_sql.html), plus the
installed 2.6.0 `check_set_op_sqlite()` body. DB references were
[PostgreSQL 17 SELECT](https://www.postgresql.org/docs/17/sql-select.html),
[SQLite SELECT](https://www.sqlite.org/lang_select.html),
[window functions](https://www.sqlite.org/windowfunctions.html), and
[WITH](https://www.sqlite.org/lang_with.html). DuckDB's initial stable/1.5 URLs
could not be retrieved. The official documentation was then read at the
[1.5.5 release commit](https://github.com/duckdb/duckdb-web/tree/41ccb8002e1500024199a0beaf5b32046d1bf348/docs/current/sql):
SELECT, LIMIT, window functions, and WITH; that commit's configuration identifies
version 1.5.5. This avoided treating the later 1.5.6 website as the tested version.
Collapse was treated as subquery construction, and ordinary CTEs as a possible
optimizer-dependent representation, not an absolute materialization barrier.

## Fixtures, expected order, and principal results

The main fixture had the following integer IDs and values and ordinary text
keys. `p` was the Margin fixed key, `r,s` the dimensions, and `h` a preprocessing
partition that crossed both. Every window partition was explicitly ungrouped
before Margin processing, except the separate intentional implicit-key control.

| id | p | r | s | h | v |
| ---: | --- | --- | --- | --- | ---: |
| 1 | P | a | u | h1 | 8 |
| 2 | P | a | u | h2 | 2 |
| 3 | P | a | v | h1 | 5 |
| 4 | P | b | u | h2 | 20 |
| 5 | P | b | v | h1 | 1 |
| 6 | Q | a | u | h1 | 7 |
| 7 | Q | a | v | h2 | 3 |
| 8 | Q | b | u | h1 | 11 |

A distinct/union fixture had tuples `(P,a,u,2)` twice, `(P,a,v,5)`, and
`(P,b,u,11)`. It had no diagnostic row ID. Tuple multiplicities were retained
by comparison rather than altered with DISTINCT or diagnostic columns.

Every principal workflow used `rows=n()`, `total=sum(v, na.rm=TRUE)`, Parent and
Total shares, `.by=p`, `rollup(r,s)`, and a public occurrence identifier. Sets
were distinct, so DuckDB `.duplicates="drop"` (native) and `"keep"` with `.id`
(portable) were semantically equivalent. Actual GROUPING SETS/UNION ALL routes
were confirmed by rendering. SQLite shares explicitly disabled source checking
only for these independently established numeric aggregates. `.sort="none"`
results were compared as multisets.

| Case | Preprocessing boundary and hand-checked totals | Principal outcome |
| --- | --- | --- |
| Raw control | P=36, Q=21 | All eight direct/bounded paths agreed |
| Global top 3 | `v DESC,id ASC`; IDs 4,8,1; P=28, Q=11 | Seven paths agreed; SQLite direct failed (#769) |
| Per-h top 1 | ROW_NUMBER by h, `v DESC,id ASC`; IDs 4,8; P=20,Q=11 | All agreed |
| Global ranks | ROW_NUMBER over all input, `v DESC,id ASC`; P=24,Q=12 | All agreed |
| Per-h ranks | Same order, partition h; P=15,Q=6 | All agreed |
| Rank filter | Per-h rank<=2; IDs 1,4,7,8; P=28,Q=14 | All agreed |
| Cumulative | Partition h, id ASC, ROWS UNBOUNDED PRECEDING through CURRENT ROW; P=59,Q=78 | All agreed |
| Lag | Partition h, id ASC, leading NULL; P=15,Q=28 | All agreed; all-NULL SQL group sums remained NULL |
| Overwrite | `v=2*v+1`, then `v=v+3`; P=92,Q=54 | All agreed |
| DISTINCT | All four tuple columns; 3 rows, P=18 | All agreed |
| Asymmetric UNION ALL | Original four rows plus its two v<=2 rows; 6 rows, P=24 | All agreed |
| UNION ALL then DISTINCT | Three unique tuples, P=18 | All agreed |
| Preaggregation/filter | AVG by p,r,s, then AVG>4; 5 rows, P=30,Q=18 | All agreed |
| LIMIT then overwrite | Top 3 then `2*v+4`; P=64,Q=26 | All agreed, including SQLite's different nested shape |
| DISTINCT then rank filter | Unique tuples then ROW_NUMBER by v DESC,p,r,s, rank<=2; P=16 | All agreed |
| UNION ALL then aggregate/filter | SUM by p,r,s, then SUM>5; P=19 | All agreed after correcting the harness's mistaken 17 |
| Preaggregation/filter then LIMIT | AVG>4, order v DESC,p,r,s, top 3; P=20,Q=18 | All agreed |
| Empty fixed partitions | No input rows and no fixed partition exists | All returned zero rows with the same public columns |

Correct global-top-3 output had eight rollup rows; P/a/u's Total share was
8/28 and P/b/u's was 20/28, not fractions of the original P total 36.
Preaggregation/filter output used the five aggregate rows: P/a/u=5,
P/a/v=5, P/a subtotal=10, P total=30, hence detail Parent shares 1/2 and
Total shares 1/6. P's retained underlying detail sum 35, the combined retained
detail sum 53, and the original overall sum 57 were not substituted for 48,
the sum of retained group averages. No additivity promise was imposed on AVG.

C independently specified sets `(p,r,s)`, `(p,r)`, `(p)`, and simple SQL
aggregates. Parent joins matched the same p,r subtotal for detail and the same
p Grand total for subtotal; Total joined the same p Grand total for every row.
Neither set enumeration nor denominator correspondence came from an internal
Margin plan. SQL NULL/zero denominator handling and root shares followed the
public contract. Plain SQL preprocessing, direct lazy preprocessing and its
computed table were checked for values, multiplicity, public columns and
numeric/text types before result comparison. Physical table order was not
assumed to persist after compute. dbplyr warnings that ORDER BY is ignored in
subqueries without LIMIT were retained in temporary logs; such order was not
used as a population or output-order oracle. Window order and input LIMIT ties
were specified separately.

Additional results were:

- Sorted direct collect/result-compute parity and finite collect at n=0,2,100
  passed for LIMIT, cumulative, and preaggregated inputs on all executable
  configurations. SQLite LIMIT could not build a direct Margin query; its
  retrieval checks used explicitly computed preprocessing and were recorded
  separately, rather than relabelled as a successful direct path.
- On these representative completed results, filter removed Grand totals,
  select retained public fields, and a many-to-many lookup join deliberately
  duplicated P rows. Remaining shares preserved their original denominators,
  and expected row multiplicities were retained. This used a completed bounded
  Margin result as the operation boundary, with the base result independently
  verified; it did not ask shares to renormalize after filter.
- CTE rendering returned the same values, including the actual SQLite dedicated
  sorted result boundary. Declared occurrence integers and double shares were
  checked on nonempty representative direct results. No new type inference
  promise was asserted for ordinary SQL sums, including empty results.
- Expansion repeated exactly the LIMIT, DISTINCT, and asymmetric-union input
  multisets per occurrence for executable direct and bounded paths. SQLite
  direct LIMIT was the second public manifestation of #769.
- Intentional `group_by(p)` after window preprocessing agreed with explicit
  `.by=p`; removing fixed keys agreed with independent pooled SQL. Empty input
  without fixed keys produced the single Grand total row with both shares one.
- Retained duplicate set occurrences remained distinct: two detail occurrences
  and one Grand total gave seven rows and did not multiply denominators. This
  was a supplemental portable occurrence check, not a native/portable comparison
  with non-equivalent duplicate semantics.
- A tie fixture changed id 3's value to 8. Ordering by v DESC,id ASC retained
  IDs 4,8,1. All backends agreed; SQLite's intervening mutation caused a nested
  query shape that succeeded. Unordered head and unspecified ties were not used.

## Extended hunt after the resource-limit change

On 2026-10-01 the user authorized exceeding the proposed default limits and
asked for a thorough hunt. The campaign expanded by targeted semantic factors,
without rerunning earlier campaigns or an option Cartesian product. Additional
inputs still had at most ten rows (a union added two rows), with four rollup/cube
occurrences or three selected dimensions; input joins deliberately expanded
population. The original principal results above remain a distinct phase.

| Extension | Observed checks | Disposition |
| --- | --- | --- |
| 18 additional preprocessing workflows | DuckDB 80/86, PostgreSQL 48/52, SQLite 47/52 passed | Eleven failed assertions belonged to one non-equivalent LIMIT/window input comparison; three ordinary limited-union inputs failed upstream; one SQLite union-then-LIMIT direct summary failed as #769 |
| Actual folded input and explicitly collapsed LIMIT-before-window | DuckDB 10/10, PostgreSQL 6/6, SQLite 6/6 | All preinput and A/B/C checks agreed after separating upstream processing order from Margin preservation |
| Duplicate Parent selection, AVG/distinct-count shares, expanded window input, second Margin, joined/ranked input | DuckDB 70/70, PostgreSQL 46/46, SQLite 46/46 | All agreed with independently enumerated sets/denominators |
| Cube, explicit subsets, composite rollup, repeated explicit sets | DuckDB 44/44, PostgreSQL 26/26, SQLite 20/24 | Four direct SQLite LIMIT constructions were #769; all bounded controls and other paths agreed |

These are assertion counts, not a count of SQL statements or independent bugs.
Repeated occurrences were compared as bags and retained rather than collapsed
by a result DISTINCT. The additional specification matrix used literal key
lists authored in the oracle, not `inspect_grouping()` or the internal plan.
Native repeated explicit sets without an ID were independently compared as a
bag, on DuckDB and PostgreSQL, for cumulative and preaggregated inputs.
Contextual share paths used IDs to force equivalent occurrence semantics.

The new preprocessing definitions were:

| Workflow | Correct processing order / distinguishing observation | Final classification |
| --- | --- | --- |
| Filter then cumulative; cumulative then filter | v>=5 before/after h-partitioned id-ordered cumulative; P/Q totals 41/51 versus 43/53 | Both agreed on all paths |
| Filter then rank; rank then filter | id>=3 before/after h-ranked v DESC,id; excluded early rows still affect the latter rank | Both agreed |
| LIMIT then cumulative | Top-three selection should precede h/id cumulative; see upstream observation below | Initial input non-equivalent; actual-input and collapsed-boundary follow-ups agreed |
| Cumulative then LIMIT | The folded projection ordered by the window alias v, as in the upstream observation; the final replay explicitly nested top-three before projection to retain the original-v order | Actual folded input preserved; explicitly bounded original-order replay also agreed (P=30,Q=32) |
| Neighbor frame | h/id, ROWS 1 PRECEDING to 1 FOLLOWING; P=87,Q=60 | Agreed |
| Lag then filter | h/id lag, keep lag>5, then use lag as measure | Agreed, including NULL handling |
| Asymmetric union then cumulative | Add IDs 101/102 as real input rows, then h/id cumulative | Agreed |
| Limited union operands | Two separately ordered top-two operands, preserving overlap | Ordinary dbplyr failed before Margin; upstream limitation, not Margin evidence |
| Union then LIMIT | Union full source and two shifted-ID rows; v DESC,id top four | SQLite direct Margin failed as #769; other direct and bounded paths agreed |
| DISTINCT after window | Rank every duplicate tuple, then DISTINCT p,r,s,rank | Agreed; equal duplicate tuples had indistinguishable base fields, so swapping their assigned ranks leaves the observed bag unchanged |
| Projection then DISTINCT | Map v<=5 to 2, remove duplicates on p,r,s,v without row ID | Agreed |
| Aggregate then overwrite/filter | AVG at p/r/s grain, then 2*average+4 and threshold>10 | Agreed |
| Two preaggregations | AVG p/r/s; retain >4; SUM by p/r with s='u' | Agreed; source grain remained the retained aggregate rows |
| Window rename/filter | Rename old v away, w to v, keep cumulative>10 | Agreed |
| Zero-sum partitions | Set id4=-16,id8=-10; fixed totals are zero | Agreed; nonroot zero-denominator shares NULL, root shares one |
| NULL then cumulative | Set id2/id5 NULL before h/id cumulative | Agreed with backend SUM/window NULL behavior |

A second Margin consumed the first completed report after removing its Grand
totals and selecting total as v. Its input intentionally contained detail and
subtotal rows; independent nested SQL counted those rows, rather than asserting
that they form raw details. A pre-Margin many-to-many join duplicated P rows
with a separate join key used for tie-breaking, then ranked by h and retained
three rows per h. Both preinput bags and final shares matched independent SQL.

Repeated `rollup(r,r,s)` yielded detail, r subtotal, repeated r subtotal, and
root occurrences. Parent selection skipped the equally detailed repeated
occurrence, as ADR 0010 requires. AVG and COUNT(DISTINCT v) were recalculated
independently at each grouping grain and used that same statistic for each
share denominator; no sum-of-child-means requirement was imposed. Expansion of
cumulative, rank-filtered, DISTINCT, union, and preaggregated inputs retained
precomputed values and source multiplicities for every occurrence.

The specification matrix covered `cube(r,s)`, explicit detail/s/root sets,
`rollup(grouping_set(r,s),h)`, and repeated explicit detail sets. Composite
preaggregation retained h in its own ordinary grouping keys so the selected
Margin dimension actually existed. Input grouping metadata remained explicitly
ungrouped before `.by=p`.

### Upstream order observation, separated from Margin responsibility

Ordinary dbplyr 2.6.0 folded a window mutate after head into the same SELECT as
LIMIT. With the original v retained and a separate w, selected IDs 4/8/1 were
unchanged but their cumulative w was 22/32/8, rather than 20/19/8 from the
explicit selected-input subquery. This was observed before any Margin call on
all three engines. With transmute assigning the window expression to v,
ORDER BY v resolved to that selected alias as well: the actual preinput was
IDs 8/7/4 with measures 32/25/22 (fixed totals Q=57,P=22). Independent SQL for
that actual SELECT, preprocessing compute, and every direct/bounded Margin
path agreed. They were not compared against an incompatible limited-input
reference to claim a Margin bug.

An explicit dplyr `collapse()` before window calculation supplied a nested
expression with the intended selected relation; on all three engines it gave
IDs 4/8/1 and measures 20/19/8 (P=28,Q=19), and all Margin paths agreed. Here
collapse served as a SQL-expression boundary, without proving physical
materialization or an optimizer barrier. This is a different use from the
SQLite Margin LIMIT collapse supplement in #769, which remained invalid.

Ordinary dbplyr `union_all()` of immediately limited operands also failed
without Margin: SQLite refused construction, while DuckDB/PostgreSQL rejected
the generated compound SQL during preprocessing compute. Independently nested
operand SQL was valid. These two upstream observations did not establish a
package-owned contract violation: Margin preserved its actual valid ordinary
input, or never received a valid input. No marginplyr implementation ticket
was opened for them, and no obligation to repair arbitrary upstream query
translation or supply an implicit snapshot was inferred. The next upstream
investigation can start from the compact actual/collapsed window pair below.

## Calibration, changes, and candidate classification

Pilot LIMIT, cumulative, DISTINCT, and preaggregated inputs agreed with hand
expectations and independent SQL before expansion. Comparator calibration
rejected row insertion/deletion/duplication, key changes, stale values and wrong
share denominators. The self-contained replay below additionally substitutes
full input for limited input, doubles the selected relation, changes selected
values, and switches cumulative partition h to p; every changed oracle result
is rejected. Comparison does not reaggregate or deduplicate a Margin result.
Numeric representations were normalized only where small integers were exact;
shares/AVG used tolerance 1e-12. Empty value comparisons retained column names
while ordinary driver-inferred zero-length column types were evaluated separately
from package-declared type guarantees.

| Candidate/observation | Classification and disposition |
| --- | --- |
| SQLite LIMIT directly into one-set/rollup summary or expansion | Confirmed Margin composition defect; #769; new isolated reproduction, independent SQL and bounded control |
| SQLite collapse supplement renders ORDER BY on compound arms | Related boundary symptom included in #769; not an established remedy, and not evidence of optimizer reordering |
| Ordinary dbplyr UNION ALL of immediately limited operands also refuses | Upstream behavior; distinguish from the successful ordinary aggregate-then-union control and Margin-owned anchor |
| UNION aggregate expected 17 | Harness arithmetic error: four copies of value 2 give 8; retained groups sum to 8+11=19. Corrected and rerun on DuckDB/SQLite; PostgreSQL used the corrected expectation |
| Empty comparison/hand-total assertions initially failed | Harness-only zero-length type/name comparison. Corrected without changing query or expected rows; rerun on DuckDB/SQLite |
| Reused lookup copy got an expression-derived table name | Harness table-name collision. An explicit private name and overwrite were used; all join paths reran successfully |
| Compact replay used across to select grouping h | Harness translation error: grouped across selection excludes grouping columns. Replay changed to explicit .data reads and was rerun |
| First full runner could not connect to PostgreSQL inside sandbox | Environment restriction; DuckDB/SQLite completed logs were retained. PostgreSQL then ran against the same disposable cluster outside sandbox. No whole-matrix success was inferred from process status |
| lintr lacked test helper symbols in the initial isolated library | Lint environment lacked testthat. Copied its closure, then package-aware load_all/lint passed; no suppression or unrelated source edit |
| LIMIT/window preinput mismatch | Ordinary dbplyr processing order/alias resolution; initial comparison inputs non-equivalent. Actual-input and collapsed-boundary follow-ups passed; no Margin bug ticket |
| AVG/distinct-count oracle initially changed preprocessing SUM too | Verifier error: global substitution rewrote cumulative preprocessing. Changed only the aggregate template before inserting untouched preprocessing SQL; all affected paths reran and passed |
| Composite oracle initially used c(r,s) and omitted h from preaggregation | Verifier input/specification mismatch: composite uses grouping_set(r,s), and input must retain h. Both corrected; the complete specification matrix reran |
| Materialized/direct preinputs | Equivalence was established for final valid comparisons; incompatible intended LIMIT/window input was separated rather than used as bug evidence |
| Other successful exercised boundaries | No issue found within the tested fixture and versions |
| Arbitrary queries/other versions/platforms | Unexecuted; no generalized correctness conclusion |

No candidate required a new product-specification decision. The rejected
hypotheses and corrected harness/environment problems were not published as
product Issues. Source reads for test setup, independent oracles, preprocessing
compute and result collect/compute were explicitly requested by the harness.
The construction Sent-query record has the scope of ADR 0027, not that of a
complete DBI execution ledger. This campaign did not add volatile counters or
claim a new proof of all construction-time read behavior; ADR 0020's existing
policy gates remained the contract authority.

## Confirmed minimum and cause

The minimum retains two source rows so ordering selects a distinguishable
subset. The code below runs against installed, unmodified target code, reports
the actual failures, and asserts the independent and bounded controls. It
requires no external fixture or earlier scratch file.

The equivalent SQLite DDL/data and stand-alone reference were also executed
on a new in-memory connection:

```sql
CREATE TEMP TABLE facts (id INTEGER, g TEXT, v INTEGER);
INSERT INTO facts VALUES (1, 'a', 2), (2, 'b', 9);
SELECT g, SUM(v) AS total
FROM (SELECT * FROM facts ORDER BY v DESC,id ASC LIMIT 1) x
GROUP BY g
UNION ALL
SELECT 'Total', SUM(v)
FROM (SELECT * FROM facts ORDER BY v DESC,id ASC LIMIT 1) x;
```

```r
library(dplyr)
library(marginplyr)

reproduce_limit <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  remote <- copy_to(
    con, data.frame(id = 1:2, g = c("a", "b"), v = c(2L, 9L)), "facts"
  )
  limited <- head(arrange(remote, desc(.data$v), .data$id), 1L)
  limited_sql <- paste(
    "SELECT * FROM facts ORDER BY v DESC, id ASC LIMIT 1"
  )
  expected <- DBI::dbGetQuery(con, paste(
    "SELECT g, SUM(v) AS total FROM (", limited_sql, ") x GROUP BY g",
    "UNION ALL SELECT 'Total', SUM(v) FROM (", limited_sql, ") x"
  ))
  stopifnot(identical(expected$g, c("b", "Total")))
  stopifnot(all(expected$total == 9L))
  bounded <- compute(limited, temporary = TRUE, analyze = FALSE)
  ordinary <- union_all(
    summarise(limited, total = sum(.data$v), .by = "g"),
    summarise(limited, total = sum(.data$v))
  )
  stopifnot(all(collect(ordinary)$total == 9L))
  calls <- list(
    one = function() {
      summarize_with_margins(
        limited, total = sum(.data$v), .grouping = grouping_set("g")
      )
    },
    rollup = function() {
      summarize_with_margins(
        limited, total = sum(.data$v), .grouping = rollup("g")
      )
    },
    expansion = function() {
      expand_with_margins(
        limited, .grouping = rollup("g")
      )
    },
    collapsed = function() {
      summarize_with_margins(
        collapse(limited), total = sum(.data$v), .grouping = rollup("g")
      )
    }
  )
  errors <- lapply(calls, function(call) {
    tryCatch(collect(call()), error = function(error) conditionMessage(error))
  })
  result <- collect(summarize_with_margins(
    bounded, total = sum(.data$v), .grouping = rollup("g")
  ))
  stopifnot(identical(result$g, expected$g))
  stopifnot(all(result$total == expected$total))
  list(expected = expected, actual = errors, bounded = result)
}

print(reproduce_limit())
```

Observed independent and bounded output was `(b,9)` and `(Total,9)`. `one`,
`rollup`, and `expansion` returned `SQLite does not support set operations on
LIMITs`. `collapsed` returned `ORDER BY clause should come after UNION ALL not
before` from SQLite during collection. The one-set expected output was `(b,9)`;
expansion should have retained the selected id 2/value 9 row once per occurrence.

Confirmed mechanism: dbplyr 2.6.0's `check_set_op_sqlite()` rejects operands
whose immediate lazy-query node holds a limit. The package-created zero-row
source anchor can retain that limit and is combined with summary branches or
the final projection. For one-set summary the finalizer anchor alone suffices;
multiple requested grouping sets are unnecessary. Ordinary aggregate branches
nest the same limited input and successfully union, so the package-owned
composition cannot be dismissed as SQLite not supporting aggregate-over-LIMIT.
The collapse supplement retained ORDER BY metadata and rendered ordering on
compound arms. This diagnoses a boundary mechanism, not a particular repair.
A fix must retain type anchors and existing retrieval contracts without an
unrequested materialization. Impact is inability to report a selected input
without a caller-requested intermediate table; no silent corruption was found.

## Self-contained normal-control replay

Extract this next R block as `controls.R` outside the repository. It regenerates
twelve representative workflows (including upstream-order observations), validates preprocessing and hand totals, calculates
all three sets and denominators independently in SQL, and compares direct and
bounded results. It also independently checks AVG and distinct-count share
denominators on bounded inputs. The actual folded LIMIT/window case is an
upstream observation, not an endorsement of its processing order. An explicitly nested window-then-LIMIT
control preserves original-v selection separately, so it differs from
LIMIT-then-window (P/Q=30/32 versus 28/19). It deliberately asserts the known SQLite LIMIT diagnostic at
this target; that assertion documents the investigated version rather than
accepting it as correct behavior. The minimal block above supplies its fix
acceptance expectation. No old logs, RDS, CSV, DB, or artifact directory is
needed by either block.

```r
library(dplyr)
library(marginplyr)

boundary_data <- function() {
  data.frame(
    id = 1:8, p = c(rep("P", 5), rep("Q", 3)),
    r = c("a", "a", "a", "b", "b", "a", "a", "b"),
    s = c("u", "u", "v", "u", "v", "u", "v", "u"),
    h = c("h1", "h2", "h1", "h2", "h1", "h1", "h2", "h1"),
    v = c(8L, 2L, 5L, 20L, 1L, 7L, 3L, 11L)
  )
}

boundary_bag <- function(data) {
  data <- as.data.frame(data)
  data[] <- lapply(data, function(column) {
    if (inherits(column, "integer64")) {
      return(as.numeric(as.character(column)))
    }
    if (is.integer(column)) as.double(column) else column
  })
  data <- data[sort(names(data))]
  if (!nrow(data)) data[] <- lapply(data, function(column) character())
  if (nrow(data)) {
    data <- data[do.call(order, c(unname(data), list(na.last = TRUE))), ]
  }
  rownames(data) <- NULL
  data
}

boundary_same <- function(left, right) {
  isTRUE(all.equal(boundary_bag(left), boundary_bag(right), tolerance = 1e-12))
}

boundary_reference <- function(sql) {
  pieces <- c(
    paste("SELECT p,r,s,1 AS occurrence,COUNT(*) AS rows,SUM(v) AS total",
          "FROM pre GROUP BY p,r,s"),
    paste("SELECT p,r,'Total' AS s,2 AS occurrence,COUNT(*) AS rows,",
          "SUM(v) AS total FROM pre GROUP BY p,r"),
    paste("SELECT p,'Total' AS r,'Total' AS s,3 AS occurrence,",
          "COUNT(*) AS rows,SUM(v) AS total FROM pre GROUP BY p")
  )
  paste0(
    "WITH pre AS (", sql, "), a AS (",
    paste(pieces, collapse = " UNION ALL "), ") SELECT c.*,",
    "CASE WHEN c.occurrence=3 THEN 1.0 ",
    "WHEN c.total IS NULL OR d.total IS NULL OR d.total=0 THEN NULL ",
    "ELSE c.total*1.0/d.total END AS parent,",
    "CASE WHEN c.occurrence=3 THEN 1.0 ",
    "WHEN c.total IS NULL OR t.total IS NULL OR t.total=0 THEN NULL ",
    "ELSE c.total*1.0/t.total END AS whole ",
    "FROM a c LEFT JOIN a d ON c.p=d.p AND ",
    "((c.occurrence=1 AND d.occurrence=2 AND c.r=d.r) ",
    "OR (c.occurrence=2 AND d.occurrence=3)) ",
    "LEFT JOIN a t ON c.p=t.p AND t.occurrence=3"
  )
}

boundary_report <- function(input, duplicates) {
  total <- rlang::sym("total")
  summarize_with_margins(
    input, rows = n(), total = sum(.data$v, na.rm = TRUE),
    parent = share_of_parent(!!total), whole = share_of_total(!!total),
    .by = "p", .grouping = rollup("r", "s"), .id = "occurrence",
    .duplicates = duplicates, .check_share_source = FALSE
  )
}

boundary_cases <- function(facts, dups) {
  cumulative <- facts |>
    group_by(across(all_of("h"))) |>
    dbplyr::window_order(.data$id) |>
    dbplyr::window_frame(-Inf, 0) |>
    mutate(w = cumsum(.data$v)) |>
    transmute(
      id = .data$id, p = .data$p, r = .data$r, s = .data$s,
      h = .data$h, v = .data$w
    ) |>
    ungroup()
  ranked <- facts |>
    group_by(across(all_of("h"))) |>
    dbplyr::window_order(desc(.data$v), .data$id) |>
    mutate(rank = row_number()) |>
    filter(.data$rank <= 2L) |>
    ungroup()
  limited <- head(arrange(facts, desc(.data$v), .data$id), 3L)
  window_input <- function(input) {
    input |>
      group_by(across(all_of("h"))) |>
      dbplyr::window_order(.data$id) |>
      dbplyr::window_frame(-Inf, 0) |>
      mutate(w = cumsum(.data$v)) |>
      transmute(id = .data$id, p = .data$p, r = .data$r,
                s = .data$s, h = .data$h, v = .data$w) |>
      ungroup()
  }
  cumulative_expr <- paste(
    "SUM(v) OVER (PARTITION BY h ORDER BY id",
    "ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)"
  )
  after_filter <- window_input(filter(facts, .data$v >= 5L))
  before_filter <- facts |>
    group_by(across(all_of("h"))) |>
    dbplyr::window_order(.data$id) |>
    dbplyr::window_frame(-Inf, 0) |>
    mutate(w = cumsum(.data$v)) |>
    filter(.data$v >= 5L) |>
    transmute(id = .data$id, p = .data$p, r = .data$r,
              s = .data$s, h = .data$h, v = .data$w) |>
    ungroup()
  window_then_limit <- facts |>
    group_by(across(all_of("h"))) |>
    dbplyr::window_order(.data$id) |>
    dbplyr::window_frame(-Inf, 0) |>
    mutate(w = cumsum(.data$v)) |>
    ungroup() |>
    arrange(desc(.data$v), .data$id) |>
    head(3L) |>
    dplyr::collapse() |>
    transmute(id = .data$id, p = .data$p, r = .data$r,
              s = .data$s, h = .data$h, v = .data$w)
  neighbors <- facts |>
    group_by(across(all_of("h"))) |>
    dbplyr::window_order(.data$id) |>
    dbplyr::window_frame(-1, 1) |>
    mutate(w = sum(.data$v, na.rm = TRUE)) |>
    transmute(id = .data$id, p = .data$p, r = .data$r,
              s = .data$s, h = .data$h, v = .data$w) |>
    ungroup()
  list(
    limit = list(
      head(arrange(facts, desc(.data$v), .data$id), 3L),
      "SELECT * FROM facts ORDER BY v DESC,id ASC LIMIT 3", c(P = 28, Q = 11)
    ),
    cumulative = list(
      cumulative,
      paste("SELECT id,p,r,s,h,SUM(v) OVER (PARTITION BY h ORDER BY id",
            "ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS v",
            "FROM facts"),
      c(P = 59, Q = 78)
    ),
    ranked = list(
      ranked,
      paste("SELECT * FROM (SELECT *,ROW_NUMBER() OVER",
            "(PARTITION BY h ORDER BY v DESC,id ASC) AS rank FROM facts) z",
            "WHERE rank<=2"), c(P = 28, Q = 14)
    ),
    distinct = list(
      distinct(dups), "SELECT DISTINCT p,r,s,v FROM dups", c(P = 18)
    ),
    union = list(
      union_all(dups, filter(dups, .data$v <= 2L)),
      "SELECT * FROM dups UNION ALL SELECT * FROM dups WHERE v<=2", c(P = 24)
    ),
    aggregated = list(
      summarise(facts, v = mean(.data$v), .by = c("p", "r", "s")) |>
        filter(.data$v > 4L),
      paste("SELECT p,r,s,AVG(v*1.0) AS v FROM facts GROUP BY p,r,s",
            "HAVING AVG(v*1.0)>4"), c(P = 30, Q = 18)
    ),
    filter_then_window = list(
      after_filter,
      paste("SELECT id,p,r,s,h,", cumulative_expr,
            "AS v FROM facts WHERE v>=5"), c(P = 41, Q = 51)
    ),
    window_then_filter = list(
      before_filter,
      paste("SELECT id,p,r,s,h,w AS v FROM (SELECT *,", cumulative_expr,
            "AS w FROM facts) z WHERE v>=5"), c(P = 43, Q = 53)
    ),
    actual_limit_window = list(
      window_input(limited),
      paste("SELECT id,p,r,s,h,", cumulative_expr,
            "AS v FROM facts ORDER BY v DESC,id LIMIT 3"), c(P = 22, Q = 57)
    ),
    collapsed_limit_window = list(
      window_input(dplyr::collapse(limited)),
      paste("SELECT id,p,r,s,h,", cumulative_expr, "AS v FROM",
            "(SELECT * FROM facts ORDER BY v DESC,id LIMIT 3) z"),
      c(P = 28, Q = 19)
    ),
    fixed_window_then_limit = list(
      window_then_limit,
      paste("SELECT id,p,r,s,h,w AS v FROM (SELECT *,", cumulative_expr,
            "AS w FROM facts ORDER BY v DESC,id LIMIT 3) z"),
      c(P = 30, Q = 32)
    ),
    neighbors = list(
      neighbors,
      paste("SELECT id,p,r,s,h,SUM(v) OVER (PARTITION BY h ORDER BY id",
            "ROWS BETWEEN 1 PRECEDING AND 1 FOLLOWING) AS v FROM facts"),
      c(P = 87, Q = 60)
    )
  )
}

run_boundary_controls <- function(backend) {
  con <- switch(
    backend,
    duckdb = DBI::dbConnect(duckdb::duckdb(shared_home = FALSE)),
    sqlite = DBI::dbConnect(RSQLite::SQLite(), ":memory:"),
    postgres = {
      host <- Sys.getenv("BOUNDARY_PGHOST")
      stopifnot(nzchar(host))
      DBI::dbConnect(
        RPostgres::Postgres(), host = host,
        port = as.integer(Sys.getenv("BOUNDARY_PGPORT")),
        user = "marginprobe", dbname = "postgres"
      )
    }
  )
  on.exit({
    if (backend == "duckdb") {
      DBI::dbDisconnect(con, shutdown = TRUE)
    } else {
      DBI::dbDisconnect(con)
    }
  }, add = TRUE)
  facts <- copy_to(con, boundary_data(), "facts", temporary = TRUE)
  dups <- copy_to(
    con, data.frame(p = "P", r = c("a", "a", "a", "b"),
                    s = c("u", "u", "v", "u"), v = c(2L, 2L, 5L, 11L)),
    "dups", temporary = TRUE
  )
  oracle <- function(sql) {
    DBI::dbGetQuery(con, boundary_reference(sql))
  }
  limited_sql <- "SELECT * FROM facts ORDER BY v DESC,id LIMIT 3"
  selected <- oracle(limited_sql)
  stopifnot(!boundary_same(selected, oracle("SELECT * FROM facts")))
  doubled <- paste(
    "SELECT * FROM (", limited_sql, ") x UNION ALL",
    "SELECT * FROM (", limited_sql, ") y"
  )
  stopifnot(!boundary_same(selected, oracle(doubled)))
  stale <- paste(
    "SELECT id,p,r,s,h, v+1 AS v FROM (", limited_sql, ") x"
  )
  stopifnot(!boundary_same(selected, oracle(stale)))
  wrong_partition <- selected
  wrong_partition$p[[1L]] <- "outside"
  stopifnot(!boundary_same(selected, wrong_partition))
  wrong_denominator <- selected
  wrong_denominator$whole[[1L]] <- 0
  stopifnot(!boundary_same(selected, wrong_denominator))
  stopifnot(!boundary_same(selected, rbind(selected, selected[1L, ])))
  stopifnot(!boundary_same(selected, selected[-1L, ]))
  cumulative_sql <- paste(
    "SELECT id,p,r,s,h,SUM(v) OVER (PARTITION BY h ORDER BY id",
    "ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS v FROM facts"
  )
  stopifnot(!boundary_same(
    oracle(cumulative_sql),
    oracle(sub("PARTITION BY h", "PARTITION BY p", cumulative_sql))
  ))
  cases <- boundary_cases(facts, dups)
  policies <- if (backend == "duckdb") c("drop", "keep") else "drop"
  for (name in names(cases)) {
    case <- cases[[name]]
    bounded <- compute(case[[1L]], temporary = TRUE, analyze = FALSE)
    expected_input <- DBI::dbGetQuery(con, case[[2L]])
    stopifnot(boundary_same(collect(case[[1L]]), expected_input))
    stopifnot(boundary_same(collect(bounded), expected_input))
    hand <- DBI::dbGetQuery(con, paste0(
      "SELECT p,SUM(v) AS v FROM (", case[[2L]], ") z GROUP BY p ORDER BY p"
    ))
    stopifnot(all(as.numeric(as.character(hand$v)) == unname(case[[3L]])))
    expected <- DBI::dbGetQuery(con, boundary_reference(case[[2L]]))
    for (policy in policies) {
      answer <- tryCatch(
        collect(boundary_report(case[[1L]], policy)),
        error = function(error) error
      )
      if (backend == "sqlite" && name == "limit") {
        stopifnot(inherits(answer, "error"))
        stopifnot(grepl("set operations on LIMITs", conditionMessage(answer)))
      } else {
        stopifnot(!inherits(answer, "error"))
        stopifnot(boundary_same(answer, expected))
      }
      stopifnot(boundary_same(
        collect(boundary_report(bounded, policy)), expected
      ))
      for (measure in c("AVG(v*1.0)", "COUNT(DISTINCT v)")) {
        reference <- boundary_reference("__PREPROCESSING__")
        reference <- gsub("SUM(v)", measure, reference, fixed = TRUE)
        reference <- sub("__PREPROCESSING__", case[[2L]], reference,
                         fixed = TRUE)
        expression <- if (measure == "AVG(v*1.0)") {
          rlang::expr(mean(.data$v, na.rm = TRUE))
        } else {
          rlang::expr(n_distinct(.data$v))
        }
        total <- rlang::sym("total")
        query <- summarize_with_margins(
          bounded, rows = n(), total = !!expression,
          parent = share_of_parent(!!total), whole = share_of_total(!!total),
          .by = "p", .grouping = rollup("r", "s"), .id = "occurrence",
          .duplicates = policy, .check_share_source = FALSE
        )
        stopifnot(boundary_same(collect(query),
                                DBI::dbGetQuery(con, reference)))
      }
      cat(backend, name, policy, "verified\n")
    }
  }
}

for (backend in commandArgs(TRUE)) run_boundary_controls(backend)
```

To prepare a fresh isolated replay, use a new directory and this fixed source
commit, install it only into that directory's library, and copy the recorded
non-base packages from a provisioned read-only library. Do not silently install
newer versions. Before running, verify `R.version.string`, all recorded package
versions and canonical `.libPaths()`/`find.package()` locations. One way to obtain
source without changing a checkout is:

```sh
set -eu
probe_root=$(mktemp -d /private/tmp/marginplyr-boundaries-replay-XXXXXX)
mkdir -p "$probe_root/source" "$probe_root/library" "$probe_root/cwd"
git archive 73f84cada8bba2d1b194c5f74d4d9d77a89a468c | tar -x -C "$probe_root/source"
# Copy the recorded dependency closure into probe_root/library first.
export R_LIBS_USER="$probe_root/library"
export R_LIBS_SITE=NULL
export TMPDIR="$probe_root/cwd"
R CMD INSTALL --library="$probe_root/library" "$probe_root/source"
# Save the two R blocks above as minimum.R and controls.R in probe_root.
Rscript --vanilla "$probe_root/minimum.R"
Rscript --vanilla "$probe_root/controls.R" duckdb sqlite
```

For PostgreSQL, initialize a new cluster under that replay root with
`initdb -D "$probe_root/pgdata" -A trust -U marginprobe --no-instructions`,
create a private socket directory, and start it with no TCP listener, a free
port, and a short statement timeout. Set `BOUNDARY_PGHOST` to that absolute
private socket and `BOUNDARY_PGPORT` to the chosen port, then run
`Rscript --vanilla "$probe_root/controls.R" postgres`. The replay refuses a
PostgreSQL connection without an explicitly supplied socket. Stop only that
private cluster afterwards. This reproduces the same public paths with new
static data, not an old snapshot or existing database.

The embedded scripts were extracted from this note into a second fresh directory
and rerun against all three engines; the minimum and twelve control workflows
reproduced their observations. That replay did not source the campaign's
harness or use its result files. Dependencies and the exact source remain
external prerequisites; the full historic experiment graph is recorded above.

## Checks, limits, and next point

The final source tree and both executable R blocks passed:

- `jarl check .` with repository `jarl.toml` (DPLYR and expect_not additions).
- `LINTR_ERROR_ON_LINT=true Rscript --vanilla -e 'pkgload::load_all(".", quiet=TRUE); lintr::lint_package()'` with testthat available in the isolated tool library.
- For each independently extracted R unit: `jarl check --extend-select DPLYR,expect_not <unit.R>` and `lintr::lint(<unit.R>)` after loading dplyr and installed marginplyr; zero findings were asserted. Markdown was not assumed to be inspected by the package linters.
- `Rscript .github/scripts/verify-context-budget.R` passed (20008/22005 bytes). No always-loaded context was changed.

The note is a repository-only change excluded from the source package. No
package-affecting change or review-ready package gate was claimed. No code
review round was run; that decision is recorded in the note PR.

The approved principal plan used 18 definitions and five small supplements.
The initial 400-query proposal was exceeded; the first runner counted
assertions rather than all statements, and calibration corrections and replay
added executions. The subsequent user-authorized extension explicitly removed
that ceiling. There was no complete SQL-statement ledger, so no exact SQL
count or claim of meeting that original bound is supplied. Engine memory and
statement-time limits remained in place for the principal and extended
matrices; execution stayed sequential within each matrix. The final campaign
scratch directory occupied approximately 236 MB, with logs far below 100 MB.
Peak combined process memory was not independently measured. Exploration
ended after each adopted boundary had a classified observation and the related
paths had been checked; final artifact replay and lint followed.

Unverified scope includes other DB/R/dependency versions and platforms, arbitrary
summary expressions, larger grouping plans, INTERSECT/EXCEPT, additional set operators,
nonstandard collations/types, concurrently updated data, non-deterministic
functions, window frames beyond the explicit cumulative and neighboring ROWS frames, and comprehensive instrumentation of
construction-time source reads. SQLite direct LIMIT finite-collect/result-compute
remains blocked by #769, rather than being omitted from scope. Empty ordinary
aggregate type inference and ordering after further dplyr verbs were not given
new guarantees. Simulation supplied no execution evidence here.

The next engineering step is #769's composition fix and regression tests,
starting with the two-row public minimum and checking preservation of the
SQLite type-anchor/result boundary. This note supplies evidence, not the chosen
fix. A continuation of this campaign appends a dated section here without
replacing earlier observations or creating a second investigation file.
