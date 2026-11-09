# Margin summary, Grouping specification and share evaluation boundaries

Investigated: 2026-11-09
Repository snapshot: `5c667c956fd50ba39be621bf1b617a17abda7b14`

The investigation began from freshly fetched main in the new worktree
`/private/tmp/marginplyr-summary-hunt-20261109`, branch
`investigation/summary-spec-share-20261109`. The prepublication main fetch
resolved to the same SHA. No package implementation, package test, dependency,
public specification or generated documentation was changed.

Three fresh-context investigators used separate detached worktrees at that
same SHA. A started from implementation and summary/share staging; B from
Grouping specifications, fixed partitions and public admission rules; C from
backend consistency, execution ownership and condition propagation. They
reported independently before candidates were shared. The coordinator
separately prepared small inputs and explicit-grain reference calculations.

Each worktree ran at most one reproduction at a time, with
`TESTTHAT_PARALLEL=false`. Terminal execution results were retained and
collected. Database work used process-owned in-memory SQLite/DuckDB databases
and synthetic temporary tables. No existing database or outside user data was
queried. DuckDB's initial default connection printed its shared-home notice;
the retained SQL minimal replay used `shared_home=FALSE` and owned cleanup.

## Sources and independent expectations

AGENTS.md, CLAUDE.md and its AGENTS reference, CONTEXT.md, the local-check,
integration, issue-tracker and investigation procedures were read. Public
summary, Grouping-constructor, Grouping-helper and share references supplied
the behavioral expectations. Relevant ADRs included 0007, 0008, 0010, 0014,
0015, 0016, 0017, 0019, 0021, 0022, 0026, 0028, 0029, 0032 and 0033.

The November 7 contracts, invariants, carrier and transfer notes and November 8
summary discovery, callable-mutation and member/geometry notes supplied repaired
controls and historical exclusions. Implementation and repair history through
PR #1524 were inspected. Historical executions were not counted as new evidence.

Input generation and expected-value generation were separate. The numeric
oracle listed each plan's sets explicitly, summarized the input at each grain
with ordinary dplyr, and separately aggregated fixed-key denominators. The
Grouping oracle independently expanded generated set/union/rollup/cube/product
trees before comparing public inspection and summary results. Neither used
marginplyr plan internals or agreement between share helpers as an oracle.
Ordinary SQL-backed per-grain summaries distinguished database aggregate
semantics from R arithmetic. For volatile summaries, ratios were also checked
against the denominator values actually displayed in the result, as ADR 0017
requires.

## Confirmed findings and repair slices

The `to-tickets` workflow published five independent repair slices, each with
`bug` and `ready-for-agent`, no blockers, contract sources, minimal reproduction,
expected/observed results, environment and acceptance criteria. Splitting and
publication had been delegated without a confirmation checkpoint. No
prerequisite prefactoring was established.

| Boundary | Observation | Issue |
| --- | --- | --- |
| First ordinary across source | Its function argument ran before branch-specific rewriting and was reused across sets; a Grouping-helper factory failed, and a stateful factory changed ordinary values. | [#1527](https://github.com/sayuks/marginplyr/issues/1527) |
| Later bare across source | The source validator removed dplyr's top-level expansion phase; an ordinary factory saw live group sizes and changed the aggregate. | [#1529](https://github.com/sayuks/marginplyr/issues/1529) |
| Later parenthesized across source | Generated namespace isolation hid the known helper shape; a scalar validator rejected the whole expanded frame. | [#1530](https://github.com/sayuks/marginplyr/issues/1530) |
| Caller error wrapping a Package condition | Branch restatement discarded the caller's error class, diagnostic, payload and cause chain. | [#1531](https://github.com/sayuks/marginplyr/issues/1531) |
| Standard-quoted SQL comment markers | A raw text search disabled the documented direct UNNEST cardinality check, admitting zero or multiple rows per grouping key. | [#1532](https://github.com/sayuks/marginplyr/issues/1532) |

### First ordinary across source

With `g = c("a", "b")` and `x = c(1, 3)`, an ordinary across function list
captured `grouping_bit(g)` as its offset and returned `sum(x) + offset`.
The explicit detail grain had bit zero; the empty grain had bit one. The
ordinary Margin result was therefore `1, 3, 5`. Adding a Total share of that
output raised the helper stub's outside-summary error. `grouping_id(g)`
reproduced. The numeric source was self-contained and scalar.

A second witness used a factory counter: ordinary dplyr at the two explicit
grains, and the no-share Margin control, returned `2, 4, 6` with two factory
constructions. Adding the share returned `2, 4, 5` with one construction and
ratios `.4, .8, 1`. This changed the source itself, rather than only choosing
a denominator occurrence.

`plan_first_local_across()` evaluated the function argument in its zero-row
carrier before Grouping-helper branch rewriting. `cache_first_across_fns()`
then supplied that value to every source branch. The existing first-selector
tests used an independent across beside a separately named share source;
they did not exercise this source-cache lifetime. Closed #871, #1083 and
#1447 supplied timing/storage controls, while #700 concerned dynamic unpacking.

### Later bare across source

For `g = c("a", "b", "b")`, `x = c(1, 10, 20)`, a constant preceding
summary followed by `across(x, list(s = factory(dplyr::n())))` produced
`1, 30, 31` in explicit ordinary dplyr grain summaries and the no-share Margin
control. The factory forced its argument and returned a closure adding that
offset. Dplyr's top-level expansion supplied zero at each grain. Adding a share
changed the values to `2, 32, 34`: the factory instead saw group sizes one,
two and three. The first-position n() factory remained a passing control.

`wrap_share_sources()` wrapped the ordinary across in `check_share_across()`.
Dplyr no longer recognized an unnamed top-level across, and evaluated the
function argument per group. This was independent of the first-source cache.
Existing dynamic-unpack and intentionally parenthesized-evaluation tests did
not detect this bare, later-source boundary.

### Later parenthesized across source

After a constant preceding summary, `(across)(x, sum, .names = "measure")`
returned the expected scalar values without a share. Adding a share reported
`Detected type tbl_df/tbl/data.frame`. Removing only the head parentheses,
or moving that across to the first position, succeeded. Whole-call parentheses
and both share helpers reproduced.

Local selection isolation changed the deferred helper head into a generated
function-value namespace call. Source wrapping then failed to recognize across
and selected `check_share_scalar()` for its whole frame. The source-wrapper
trace distinguished this loss of identity from the recognized bare-call
evaluation change above. Closed #972 / PR #987 had fixed parentheses inside
the function argument during source-name prediction; that control passed.
The demonstrated crossing involved the helper head/whole call and #1311's
later namespace isolation, after source-name admission.

The 432-case stateful matrix crossed three eager classes, function forms,
first/later position, both helpers, all Margin orders, fixed partitions and
retained duplicate occurrences. All 288 admitted list cases changed ordinary
values or factory counts. The 144 unknown single-factory forms were correctly
refused for unavailable static source names. A separate 24-case syntax matrix
had eight parenthesized false type refusals, two bare later-factory value
differences and fourteen passing controls. Each mechanism had a separating
control and could be repaired independently.

### Caller-owned error chains

An actual caught `marginplyr_error` became the parent of a caller-owned
`rlang::error_cnd("caller_summary_failure", ...)`. Ordinary dplyr retained
that exact caller object. Named and unnamed-frame Margin summaries replaced
the chain with the Package cause alone. Changing only the parent to an
ordinary other-package condition preserved the caller's error. Wrapping an
actual invalid nested Margin operation reproduced without forging a class.

`restate_branch_error()` promoted any `cnd$parent$parent` inheriting
`marginplyr_error`, without establishing ownership of the intervening wrapper.
That branch originated in the generated collision-error repair for #700/#701.
Eight initial comparisons and 48 scope comparisons established twelve losses
across local classes, frame/named shapes and single/rollup plans. Additional
wrapping, local across, direct Package-condition stopping and dtplyr collection
were passing controls. Existing propagated-error tests used no Package cause;
across-naming tests used an ordinary naming cause. Closed #107 concerned a
different share-selection wrapping seam.

### Quoted SQL comment markers

The direct source
`SUM(x) + UNNEST(CAST([10,20] AS INTEGER[])) + LENGTH('--')` contained standard
quotes and no SQL comment. It returned six rows under `rollup(g)` for two
input rows, including two rows for the single Grand total occurrence. An empty
array silently removed every row. Replacing only `'--'` with `'xx'` added the
same numeric constant and correctly raised the exactly-one-value error. A
singleton returned values `13, 15, 16` and the expected double ratios.

The scanner tokenized standard quotes, then raw-searched the original text
for comment markers and declined checking. Initial 48-case and adjacent
42-case matrices found respectively sixteen and twenty missing refusals.
They crossed string/identifier/escaped-quote spellings, array sizes,
Parent/Total, native/portable SQL and fixed-key presence. All fourteen singleton
cases in the adjacent matrix matched independent values and displayed-denominator
ratios. Actual comments and scalar subqueries were exclusion controls.
An owned sequence stayed untouched through construction/rendering and advanced
only at collection; source tables stayed identical.

Closed #1442 / PR #1460 established the direct UNNEST check. Its ordinary
syntax tests passed, but did not cross quoted markers with invalid array sizes.
PostgreSQL simulated rendering also omitted the guard; live PostgreSQL was
not executed, so the live finding was bounded to DuckDB.

## Refuted candidates and finite coverage

| Hypothesis | Evidence and execution result | Disposition and next boundary |
| --- | --- | --- |
| Fixed keys, missing keys, duplicate occurrences or sorting corrupt Total denominators | The coordinator's 2,592 explicit-set comparisons matched local/dtplyr results, values, types, names and cardinality across empty/single/multiple groups, factor/Date dimensions, six families, fixed-key presence and all sort modes. | Refuted for this finite input family; arbitrary classes and backend versions were not inferred. |
| Overlapping composite rollups/cubes or product/union expansion lose set order | B's 100 generated depth-at-most-two families produced 500 successful plan/share comparisons over all duplicate policies, occurrence order/IDs, root admission and fixed-key denominators. | Refuted for generated cases; this did not prove arbitrary specification factories. |
| SQL zero/missing sums expose a share calculation difference | C's 480 comparisons initially differed in 56 SQL cases. Explicit ordinary SQL-backed grain controls reproduced all differences: all-NULL SUM versus R zero, or SQLite ordinary all-missing storage types. | Excluded as dependency aggregate/storage semantics. |
| Volatile sources are reevaluated inconsistently, or immutable dtplyr inputs are changed | C's 266 comparisons/assertions matched displayed Parent/Total denominators across four backends, duplicates, cube/rollup, missing/fixed keys and sorting. Caller data.table serialization stayed identical. | Refuted for selected paths; arbitrary derivations, concurrency and interruption were not established. |
| Structured or unusual numeric keys change ordinary output when shares are added | Thirty local shape/value comparisons with list, packed-frame, integer64, NaN/NA and matrix keys, fixed/dimension roles and sorting matched their no-share ordinary outputs. | No additional defect established; the primary ratio oracle remained the explicit-grain calculation. |
| Nonfinite local sources violate arithmetic or missing-value rules | Twenty-four local/dtplyr comparisons covered positive/negative infinity, their sum, NaN, NA and zero, with fixed keys absent/present. Simple R sums and division matched values, ratios and double share types. | Refuted for these local cases; no SQL nonfinite portability claim followed. |
| Unusual names break generated summary/share references | A 468-call, four-backend name sweep had 414 exact successes after unspecified SQL row order was normalized. Remaining refusals were checked against ordinary per-grain calls, proper all_of selection and backend constraints. | No new report. An earlier order-sensitive comparator was discarded. |
| dtplyr special payloads are a new ordinary-summary regression | Grouped ordinary calls succeeded for some .SD-like names while rollups failed. Aligning the grand-total grain made ordinary dtplyr fail identically. Sixty payload controls and 64 name/form/optimization conditions matched detail and total paths; fixed-key controls and source immutability passed. | Excluded as an upstream ungrouped-grain limitation; #1047's fixed-key scope stayed intact. |
| An unrelated zero/multiple-row summary inflates local share rows | Dplyr 1.2.1 itself rejected the non-scalar ordinary output. | Refuted on this dependency version. |
| Identity-tagged share selection must be accepted | Tidyselect and the share planner intentionally disallowed renaming; existing tests asserted that boundary, and no public acceptance promise was found. | Not adopted as a bug or an inferred specification change. |

The first algebra harness placed `.id` after summaries rather than immediately
after keys; the oracle was corrected before the retained successful comparison.
An initial A harness used an invalid `quote()` argument and was corrected before
the complete matrix. Neither harness error was counted as a product defect.
The coordinator independently asserted all five findings in separate executions.
After integrating the three reports and checking the special-name and syntax
intersections, no promising executable hypothesis remained pending in these
finite lanes. Finding count did not determine that checkpoint.

## Replays, tracker reconciliation and limits

The [replay directory](2026-11-09-summary-spec-share) contains five minimal
witnesses and both independent oracles. The following commands run from this
research branch, whose package sources were unchanged from the pinned SHA.
For a separate checkout of the investigated SHA, use absolute paths to these
retained scripts while working from that checkout's root:

```sh
Rscript investigation/2026-11-09-summary-spec-share/first-source-minimal.R
Rscript investigation/2026-11-09-summary-spec-share/later-source-minimal.R
Rscript investigation/2026-11-09-summary-spec-share/parenthesized-source-minimal.R
Rscript investigation/2026-11-09-summary-spec-share/condition-cause.R
Rscript investigation/2026-11-09-summary-spec-share/unnest-marker-minimal.R
Rscript investigation/2026-11-09-summary-spec-share/numeric-oracle.R
Rscript investigation/2026-11-09-summary-spec-share/grouping-oracle.R
```

The first three scripts asserted the observed failure with exit zero and used
`--assert-contract` for the desired, failing contract assertion (exit one).
The condition witness asserted the lost caller object with exit zero. The SQL
witness checked legal and same-valued refusal controls, then intentionally
exited one when the required refusal was absent. Oracle failures stopped with
their differences; successful runs wrote evidence only in temporary directories.
These research witnesses were not package regression tests or a green package
check. Raw discovery code/logs remained under
`/private/tmp/margin-hunt-{root,a,b,c}-evidence` and coordinator temporary scripts.

All-state tracker searches included `across cache factory`, `across factory`,
`first across share`, `cached fns`, `across source evaluation`,
`condition parent summary`, `marginplyr_error parent`,
`condition propagate cause`, `error unwrap`, `UNNEST`, and `UNNEST quote`.
Relevant Issue bodies/comments and related PRs were reconciled with the prior
repairs named above, including #716, #972/#987, #1047, #1311, #1440 and
#1442/#1460. The open Issue/PR inventory and latest main were checked before
publication. Concurrent inspection/expansion reports #1525, #1526 and #1528
concerned different boundaries. No duplicate of the five repair slices or
pending matching PR was found. The GitHub connector denied writes with 403;
the repository-specified `gh` CLI published the tickets successfully.

R was 4.6.1, aarch64-apple-darwin23, on macOS Tahoe 26.6.2, C.UTF-8,
Asia/Tokyo. Relevant packages were marginplyr 0.1.0 from pinned source,
dplyr 1.2.1, dbplyr 2.6.0, dtplyr 1.3.3, data.table 1.18.6.1,
DBI 1.3.0, DuckDB 1.5.5, RSQLite 3.53.3, rlang 1.3.0, vctrs 0.7.3,
tibble 3.3.1, tidyselect 1.2.1 and pkgload 1.5.3.

The observations did not establish live PostgreSQL/other hosted-engine
behavior, other OS/R/dependency versions, arbitrary reflective caller programs,
exhaustive extension classes, interruption or concurrency. SQL infinity/NaN
portability remained outside the documented portable value guarantee. No live
Arrow execution matrix was repeated in this investigation. Additional evidence
would require isolated runs on those engines/versions and contract-specific
oracles; no unsupported inference was used to open another ticket.
