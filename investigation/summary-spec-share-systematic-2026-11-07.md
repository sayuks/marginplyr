# Systematic verification of Margin summaries, Grouping specifications and shares

Investigated: 2026-11-07
Repository snapshot: `a79def28240a5d213d3b9c1a19da93c313820b51`

The investigation started from fetched main in the new worktree
`/private/tmp/marginplyr-summary-spec-share-systematic-20261107`, on branch
`investigation/summary-spec-share-systematic-20261107`. A fresh fetch before
Issue publication still resolved to the tested snapshot. Four investigators
used independent disposable Git archives and sequential reproduction processes
per archive, with `TESTTHAT_PARALLEL=false`. SQLite and DuckDB connections
were newly created in-memory databases containing temporary dummy tables only.
This was functional verification; no security assessment or implementation
repair was performed. Reproduction scripts and raw output remained in
temporary investigation storage, as requested.

## Contract and historical evidence

The public references for `summarize_with_margins()`, the five Grouping
constructors, `inspect_grouping()`, the Grouping helpers and both share helpers
supplied the expected behavior. ADRs 0007–0009, 0010, 0013–0014, 0016–0020,
0026, 0028–0029 and 0031–0033 supplied capture, expansion, identity,
denominator, attribute, lazy-input, selection and declared-type boundaries.
The corresponding source and ordinary backend operations were primary
evidence; internal compiler output was not used as its own oracle.

The November 7 [contracts](summary-grouping-share-contracts-2026-11-07.md),
[invariants](summary-grouping-share-invariants-2026-11-07.md),
[transfer](summary-grouping-share-transfer-bughunt-2026-11-07.md) and
[carrier](summary-grouping-share-carrier-bughunt-2026-11-07.md) notes, and
relevant sections of the November 6
[boundary](summary-grouping-share-exhaustive-bughunt-2026-11-06.md),
[deep](summary-grouping-share-deep-bughunt-2026-11-06.md) and
[initial](summary-grouping-share-bughunt-2026-11-06.md) hunts identified
previous reports and repaired controls. Their dated executions were not
counted as executions of this investigation.

## Confirmed new defect: known closure converters lose share dependencies

[#1482](https://github.com/sayuks/marginplyr/issues/1482) records one
independent repair slice, with `bug` and `ready-for-agent` labels and no
blockers. It includes expected behavior, the observed values, a minimized
prototype, actual dependencies and public acceptance criteria. No prerequisite
refactoring was established. The published body and labels were fetched and
checked after creation.

The dummy input had `g = c("a", "b")`, `x = c(1, 3)` and
`prior = c(10, 20)`. An inline function bound a local `prior` to 11, captured
it in `f <- function() sum(prior)`, transferred that closure through
`rlang::as_function(f)` or `compiler::cmpfun(f)`, removed the local `prior`,
then invoked the transferred function. The removal made invocation read the
enclosing summary mask. Both dependency directions were exercised:

| Call arrangement | Required result | Observed result |
| --- | --- | --- |
| Earlier `prior = sum(x)`, then the closure expression as `measure`, then a share of `measure` | A Package condition refusing a source dependent on an earlier alias | Eager inputs accepted `measure` 1/3/4 and shares 0.25/0.75/1; dtplyr accepted input-derived `measure` 10/20/30 instead |
| `measure = sum(x)`, then a share named `prior`, then the closure expression as `after` | A Package condition refusing an ordinary summary's earlier-share dependency | `after` was 10/20/30 beside displayed shares 0.25/0.75/1 |

The complete converter matrix crossed both helpers, both directions, both
converters and plain data.frame, tibble, eager data.table and explicitly
collected immutable dtplyr input. Its 192 calls established 32 missing
refusals, 32 correct direct/`match.fun()` refusals and 128 legal
retained-formal or independent-closure controls. Source serialization stayed
equal. A coordinator replay in a separate archive independently reproduced
the eight eager converter failures and eight refusal controls.

The cross-backend source values show why the self-contained-source rule
matters: acceptance did not merely omit a diagnostic; the same expression
read different values on eager and dtplyr inputs. The later-summary case
also displayed values unrelated to the preceding visible share. No database
execution claim was established for these closure expressions.

Closed [#1402](https://github.com/sayuks/marginplyr/issues/1402) enumerated
`force`, literal `get` and `match.fun`; closed
[#1389](https://github.com/sayuks/marginplyr/issues/1389) enumerated
`identity`, `invisible` and literal assignment. Those repaired controls did
not report this converter boundary. #130 concerned function scoping and
formula interpretation. Formula/string conversion and arbitrary caller
wrappers were outside the new ticket. The minimal failure needed marginplyr,
dplyr and rlang; the compiler variant used `compiler`, shipped with R.
data.table and dtplyr were compatibility dependencies only. No database,
Arrow or new mandatory package dependency was needed to establish it.

## Confirmed existing defect: wrapped covered-cube duplicate rejection

The broad bounded-rejection contract already belonged to closed
[#1464](https://github.com/sayuks/marginplyr/issues/1464), so this witness was
recorded as a remaining shape of that report, without a duplicate Issue.
PR [#1479](https://github.com/sayuks/marginplyr/pull/1479), included in the
tested main, repaired the directly nested covered-cube control.

In a fresh R process with `R_MAX_VSIZE=96M`, a one-row frame with twenty
integer columns `x1` through `x20` was inspected using
`grouping_spec(grouping_sets(cube(everything())), grouping_set(everything()))`
under the default duplicate policy. Every one of its 1,048,576 choices
produced the complete key set. The expected result was a bounded duplicate
Package condition. The observed result was `simpleError` with
`vector memory limit of 96.0 Mb reached, see mem.maxVSize()`.
The coordinator independently reproduced that condition in another archive.

Removing only the singleton `grouping_sets()` wrapper produced the expected
duplicate condition naming positions 1 and 2 under the same heap limit.
Twelve controls across both product factor orders passed: large direct
rejection, three-column wrapped rejection, small keep multiplicity/order and
large drop compaction. Small keep retained eight occurrences with identifiers
1 through 8; large drop returned one complete set and Grouping identifier
zero. This was a resource-bounded functional diagnostic failure, not a claim
about every large plan. The failing witness required no optional backend.

The direct optimization in `expand_single_cube_product()` applied neighboring
singleton coverage before enumeration. The singleton-union wrapper did not
take that path. Closed #1274, #1306 and duplicate #1314 concerned compact
duplicate-drop expansion; their passing drop contract was distinct from the
remaining default-policy failure.

## Completed finite comparisons

Counts below describe different finite comparisons and assertions; they are
not one combined sample or a full package-suite result.

| Area | Executed scope | Result |
| --- | --- | --- |
| Ordinary aggregation | 168 cases across data.frame, tibble, data.table, dtplyr, SQLite, native DuckDB and Arrow | Per-occurrence ordinary backend summaries matched sums, counts, means, keys, column order and integer identifiers |
| Local types and attributes | 176 cases with empty/nonempty, fixed/dimension roles, NULL/NA-character labels and both Margin orders | Integer, double, logical, Date, POSIXct timezone, factor/ordered/NA-level, list, matrix and packed-frame controls matched their owned contracts |
| Grouped inputs, factors and dtplyr derivations | 113 cases | Implicit/explicit fixed keys, ungrouped output, rowwise and `.drop = FALSE` refusals, factor levels/label position and immutable root/filter/select/rename/cache/join/union controls passed |
| Grouping selection | 52 selectors, 1,664 comparisons | Independent typed tidyselect plus family algebra matched complete plans or expected refusals for all five constructors and all duplicate policies |
| Larger overlapping Grouping families | Seed 7112026; 40 nine-unit cubes and three union/product forms, all duplicate policies; 360 comparisons | Complete 512/1,024-occurrence families matched ordered keys, set identifiers, bits and masks or duplicate conditions |
| Share numeric values | 192 collections across eager, dtplyr, SQLite and native/portable DuckDB; 4,512 ratio assertions | Missing/fixed keys, empty/one-row/random input, negative/zero measures, duplicates and sort combinations matched displayed denominator rows |
| Total-share specification combinations | 112 requests; 844 ratio assertions | Rollup, cube, union, product, duplicate grand totals, total-only plans and missing-grand-total refusals passed |
| Share eligibility | 544 source type/cardinality probes | Legal numeric scalars and documented ineligible types/cardinalities met the selected local/backend expectations |
| SQLite direct boundaries | 288 configurations; `collect(n = 0/1/Inf)` and direct temporary `compute()` | Public columns, cardinality, integer `.id`, numeric direct helpers and double shares passed across six source storage kinds, empty/fixed combinations, sorts and bare/parenthesized/block/literal-across helper forms |
| Structured fixed-key denominator matching | 54 local cases | List, packed frame, matrix, three-dimensional array, factor NA level/missing code and Date partitions matched ordinary per-key sums and both share ratios across three sorts and three labels |

Duplicate input facts and missing grouping keys were included in the
aggregation oracle. It independently summarized each Grouping-set occurrence
using ordinary dplyr on the same backend, then assembled the expected result.
The share oracle recomputed ratios from displayed denominator rows, with
structural occurrence and fixed-key matching rather than Margin labels.
DuckDB native and portable paths were checked from rendered SQL. The
structured-key controls checked source serialization; database controls
retained the dummy source table's contents.

## Rechecked reports and refuted hypotheses

The previous #1420 pattern-method reproduction passed its assertion mode on
this snapshot. Existing contract replays for #1440 (special dtplyr keys),
#1441 (unnamed frame schemas), #1447 (first ordinary across input storage)
and #1477 (summary-frame names) each exited zero. Selected repaired
#1470–#1476 carrier witnesses and legal controls also passed. The #1442 SQL
cardinality replay passed six native/portable DuckDB cases: zero-element and
two-element sources refused at collection; singleton sources retained the
expected three rows and ratios. PostgreSQL was not rerun. These were
re-established repairs, not new findings credited from old notes.

Several preliminary comparator assumptions were rejected:

- Ordinary SQL all-missing SUM and empty aggregate representations differ
  from R. Same-backend ordinary dplyr was the aggregation reference; arbitrary
  aggregate storage was not normalized into a package promise.
- Grouping a plain frame or eager data.table changes its class under dplyr.
  ADR 0016 delegates that class and arbitrary attributes; factor restoration
  and package-created types were checked separately.
- The SQLite matrix initially required every finite unsorted source-key
  prefix to retain the unlimited key's type. Its 192 mismatches were empty
  or all-NULL prefixes inferred as logical, while package-owned declarations
  passed. A minimal replay at limits 0, 1 and Inf matched the delegated
  ordinary public query exactly. The SQLite B-direct specification delegates
  unsorted collection plus declared-type repair; no new source-key defect
  was established. Sorted and unlimited key-type comparisons passed.
- The plain-list fixed-key fixture had four distinct detail combinations and
  three totals, rather than nine rows. Correcting that oracle cardinality
  yielded all 54 passing structured-key cases; denominator assertions were
  unchanged.
- Installing a selection proxy inside an operand occurred after proxy
  dispatch in the explored fixture. Native selection and inspection agreed;
  this did not establish a selection-time mutation defect.

Observation harnesses intentionally accepted the dated failures as data and
exited zero. Such an exit established completed reproduction, not satisfaction
of the intended contract. The corrected comparison matrices completed with
their assertions passing. No reproduction or owned child remained active.

## Tracker reconciliation, environment and limits

All-state discovery captured 762 Issue and 716 PR bodies, 1,478 items through
#1480. Candidate searches covered closure transfers, `cmpfun`, `as_function`,
duplicate/wrapped/cube/heap/bounded expansion, selection proxies, summary,
factor, attributes, grouped and immutable inputs. Full relevant threads
included #130, #1389, #1402, #1464, #1274, #1306, #1314 and PRs #1286/#1479,
plus the repaired summary reports. Live all-state `cmpfun` Issue and PR
searches were empty; `as_function share` found #130 and no PR. Broad capped
searches were discovery only; no absence claim relied on a truncated list.
The publication-time refresh also read newly filed
[#1481](https://github.com/sayuks/marginplyr/issues/1481), a DuckDB tibble
selection-proxy report. It was distinct from #1482 and was not independently
replayed by this investigation.

R was 4.6.1 on arm64 macOS. Relevant installed versions were dplyr 1.2.1,
dbplyr 2.6.0, dtplyr 1.3.3, data.table 1.18.6.1, tibble 3.3.1, vctrs 0.7.3,
rlang 1.3.0, tidyselect 1.2.1, DBI 1.3.0, RSQLite 3.53.3, DuckDB 1.5.5,
Arrow 25.0.1 and pkgload 1.5.3. A normal dbplyr diagnostic explained SQL
missing-value removal. Arrow's sandbox-denied CPU cache-information
diagnostics did not prevent the corresponding assertions from completing.

No general proof was established for infinite/backend-specific NaN
portability, arbitrary reflection, converters,
selector effects, extension classes, unbounded plans, hosted services,
PostgreSQL or other live database servers, other dependency versions/R
versions/operating systems, concurrency or interruption. Database execution of
the new closure witness and a broader resource bound for wrapped Grouping
plans remained unverified. The confirmed new ticket and existing #1464
shape remained implementation work; this submission changed only dated prose.

Temporary evidence was held under
`/private/tmp/marginplyr-systematic-evidence`,
`/private/tmp/marginplyr-summary-audit.vKQ0kF`,
`/private/tmp/marginplyr-grouping-audit-a79def2` and
`/private/tmp/marginplyr-share-audit.3Yz6qu`. Those paths identify the local
research session, not durable repository artifacts. The essential input,
scope, expected/observed results and tracker links are recorded above.
The unchanged source-package exclusion covered investigation content. Its
consumers were readers only; package code, tests, dependencies, installed
files, generation sources and verification tools were unchanged. This
justified Review-ready source scope, with package build/check, complete suite
and snapshots omitted. No NEWS entry was needed for this research-only
submission. The pinned local-check result and review record belonged on the
PR; CI monitoring was outside the requested scope.
