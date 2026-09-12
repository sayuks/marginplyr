# Targeted mutation testing of lazy-query execution policy

Investigated: 2026-09-12

## Question

Can the existing marginplyr test suite detect a small, plausible implementation
change that makes a Margin verb read a lazy input before the caller asks it to?
This campaign is independent of the earlier focused mutation campaign and does
not seek a repository-wide mutation score.

This note records the contract, the production seams that can execute or
materialize a lazy input, the existing protection around each seam, the
isolated mutation results, and the evidence used to classify the survivor.

## Contract

### Protected behavior

ADR 0020, *Ask before reading a lazy input*, is authoritative. A Margin verb
builds and returns a lazy query without executing it; `dplyr::show_query()` also
executes nothing. A caller's data is not read until the caller executes the
returned query. The operational statement in `AGENTS.md`'s *Queries against a
lazy input* section additionally makes any new `collect()`, `compute()`, or
other execution entry point in `R/` a public-contract change rather than an
implementation detail.

The contract governs reads marginplyr **causes**, including a read carried out
inside another package. This matters for an Arrow summary that falls back from
Arrow execution to R: no `collect()` need appear in marginplyr's source for the
backend to materialize the caller's input. ADR 0025, *Refuse a summary a backend
would absorb*, applies ADR 0020's caller-choice rule to that path.

The backend kind is not a proxy for query cost or locality. ADR 0020's
*two of the four pairs* amendment corrects its earlier claim that
`is.data.frame(.data)` is exact: it is conservative for an in-memory Arrow
`Table`, but conservative is the required direction. The evidence in
`investigation/query-cost-across-lazy-backends.md` establishes why neither a
small row limit nor a backend kind bounds the cost of a read.

The corresponding public documentation is the *When marginplyr queries your
data* section in `R/summarize_with_margins.R:622-680`. It gives every
`.check_margin_label` argument the default `is.data.frame(.data)` because an
observed-label check reads grouping-column values. In contrast,
`.check_share_source` defaults to `TRUE` because its source check is constructed
without reading a caller row.

### Allowed reads and non-reads

The following cases do not violate the contract:

1. **Zero-row typed selection proxy.** `grouping_selection_proxy()` may execute
   `collect(head(.data, 0L))`, and only for a backend kind granted
   `collect_selection_proxy`. `R/grouping-backend.R:150-201` grants it to
   `dtplyr` and DuckDB. This is an enumerated exemption, not a general license
   for bounded reads; granting it to another backend extends the exemption and
   requires a backend-specific justification (ADR 0020, exemption 1).
2. **Table-free share-dialect question and control.** A SQL contextual-share
   request with `.check_share_source = TRUE` may send one probe and, if the
   probe raises, one control. Both use `SELECT 1 AS z` scaffolding and reference
   none of the caller's tables (`R/share.R:2204-2431`). A measured answer is
   cached per dialect; an unanswered attempt is not cached and may cost two
   more table-free queries on the next request (ADR 0020, exemption 2 and its
   *only an answer is remembered* amendment).
3. **Explicitly requested observed-label scan.** Setting
   `.check_margin_label = TRUE` asks marginplyr to aggregate the relevant
   grouping columns and collect the one-row result
   (`R/margin-label.R:239-302`). A declared factor-level collision is found in
   the typed metadata already acquired and therefore needs no opt-in or second
   read (`R/margin-label.R:192-208`). A typed-missing label, or a check with no
   non-factor column left, sends no query.
4. **Caller execution of the returned query.** `collect()`, `compute()`, or
   another execution operation performed by the caller after the Margin verb
   returns is the point at which the promised lazy result is meant to run.
5. **`nest_by_with_margins()`.** This is the one public-verb exception. Its
   row-wise return value is necessarily local, so it collects the common nested
   result at `R/nest_by_with_margins.R:135`; the empty-input reconstruction may
   additionally collect a zero-row input at lines 141-143. The collection is
   documented as part of the return type, not silently introduced by ADR 0020.
6. **Metadata and query construction.** `dplyr::tbl_vars()`,
   `arrow::schema()`, ordinary lazy dplyr verbs, `dbplyr::sql_render()`, and
   `dplyr::show_query()` construct or inspect metadata without executing the
   caller's data. ADR 0027 explicitly rejects treating SQL auditing as a third
   query exemption: rendering the Sent-query record is client-side and the
   caller's result remains unexecuted.
7. **Direct-reader inspection through a query.** `inspect_grouping()` may turn
   a direct Arrow `RecordBatchReader` into
   `dplyr::select(reader, dplyr::everything())` and inspect that unexecuted query
   (`R/inspect-grouping.R:153-181`). It may not normalize through
   `arrow::as_arrow_table()`, which consumes the reader. Reusable Margin verbs
   reject a direct reader or a query whose source graph contains one; nesting
   asks the caller to collect it (ADR 0020's direct-reader amendment and
   `R/utils.R:172-187`).

### Previous campaign boundary

`investigation/focused-mutation-testing-margin-semantics.md` mutated Grouping
plan expansion, share denominator mapping, and share value calculation. Its
*Code deliberately outside this campaign* section explicitly excluded backend
adapters, dialect probing, query audit, and lazy execution. This campaign stays
inside that excluded lazy-query boundary and does not revisit the prior
campaign's functions or mutants.

## Execution and materialization seams

The architecture gives typed metadata one home:
`R/backend-metadata.R`. `design/architecture.md`'s *Backend metadata* section
states that Arrow uses schema metadata and that backends requiring a zero-row
collection do it only there. Its *Portable adapter* section locates the
indirect Arrow read at branch summary construction, and its *Test seams* and
*Structural gates* sections distinguish public behavior from namespace-wide
call-shape assertions.

The earlier backend API audit is historical evidence rather than authority for
current state. Its sections *Arrow can provide the typed proxy directly from
schema metadata* and *The typed zero-row proxy is collected twice* establish
the intended split subsequently implemented: Arrow schema inspection, one
zero-row proxy for dtplyr and DuckDB, and reuse of that snapshot. The completed
`investigation/backend-api-improvement-plan.md` records the same split under
*Build and reuse backend metadata efficiently*.

| Seam | Production behavior that prevents or permits execution | Existing protection |
|---|---|---|
| Column-name discovery | `get_col_names()` uses `select()` plus `tbl_vars()` and never converts the selected lazy object (`R/backend-metadata.R:1-5`). | The reachability snapshot in `tests/testthat/test-query-policy.R:157-173` adds `get_col_names` if an execution entry point is introduced there. |
| Typed selection proxy | Arrow converts `arrow::schema(.data)` to a frame; capability backends collect `head(.data, 0L)`; all others reuse `.data` (`R/backend-metadata.R:38-49`). | `test-query-policy.R:621-635` snapshots the capability grant as exactly `dtplyr` and `duckdb`. Operation tests count one proxy collection for dtplyr and DuckDB and zero for Arrow (`test-expand-operation.R:134-212`, `test-summarize-operation.R:200-230`, `test-nest-operation.R:67-93`, `test-grouping-backends.R:574-772`). `test-sent-queries.R:546-599` records a DuckDB proxy and no RSQLite proxy. |
| Mutable-step inspection proxy | Inspection copies the step onto an isolated zero-row root and calls dtplyr's registered `as.data.table` method directly, avoiding `collect()` and `as_tibble()` (`R/backend-metadata.R:7-36`, `R/grouping-plan.R:266-303`). | `test-query-policy.R:517-587` proves the `as_tibble` family is observable and that named and typed inspection of root and derived Mutable steps invokes no entry point. `test-grouping-backends.R:2329-2368` checks the table is not written. |
| Observed Margin-label collision | `validate_margin_label()` returns before `check_observed_label_collision()` when `.check_margin_label` is false (`R/margin-label.R:145-209`); the latter builds one aggregate and collects it. | `test-grouping-interface.R:1131-1150` fixes every formal default. `test-margin-label.R:902-945` and lines 1026-1074 fix lazy silence and opt-in behavior; lines 866-900 fix the no-column/no-query route. `test-query-policy.R:371-454` observes zero entry points for default Arrow Margin calls. `test-sent-queries.R:603-622` records the opted-in scan. |
| Share-source validation | `share_source_checker()` routes local values to the already materialized result, dtplyr validation into the deferred summary, and SQL to a table-free dialect verdict (`R/share.R:2110-2202`). | `test-share-backends.R:792-830` instruments result-field, cardinality, and collection requests and requires all three to stay zero. Lines 1277-1560 cover probe caching, unanswered attempts, the control, and one-query Converting dialects. `test-sent-queries.R:624-683` records the probe/control purposes. |
| Arrow absorbed summary | `summarize_margin_branch()` handles Arrow's fallback warning before the backend reads; the later lazy-input/local-result class check is only a post-read backstop (`R/grouping-adapter-union.R:308-358`). | `test-query-policy.R:371-508` counts actual entry-point invocation for every absorbing and refusing Arrow shape. `test-grouping-backends.R:135-186` checks the refusal, lines 328-379 and 473-539 gate the warning reading and backend premise, and lines 541-572 check the post-read backstop. |
| Direct Arrow reader | `inspect_grouping()` constructs an unexecuted selection query; reusable Margin verbs use `arrow_input_has_reader_source()` to reject direct and nested reader sources (`R/inspect-grouping.R:153-181`, `R/grouping-backend.R:11-37`, `R/utils.R:172-187`). | `test-query-policy.R:371-454` counts `arrow::as_arrow_table()` and requires direct-reader inspection to remain at zero. `test-verb-argument-admission.R:811-922` asserts refusal or inspection as appropriate and then consumes the original reader to prove all five rows remain. |
| Lazy result return | Executors and finalization compose lazy dplyr operations; `record_sent_query("result", result)` renders a record but returns `result` unchanged (`R/margin-operation.R:348-377`). | The reachability snapshot fixes the known call graph; backend operation tests assert `tbl_lazy`, `arrow_dplyr_query`, or `dtplyr_step` before their own explicit collection. The Arrow runtime zeroes in `test-query-policy.R:403-430` distinguish a lazy-looking result from a verb that already read. |
| Row-wise nesting exception | `nest_by_with_margins()` unconditionally collects the common result to construct a local row-wise data frame (`R/nest_by_with_margins.R:103-166`). | `test-nest-operation.R:427-492` contrasts lazy `nest_with_margins()` with local `nest_by_with_margins()`; the documented exception is intentionally present in the reachability snapshot. |

### Execution-entry-point catalog

`tests/testthat/test-query-policy.R:66-121` enumerates the catalog rather than
inferring it: `dplyr::collect`, `compute`, `pull`, and `explain`;
`base::as.data.frame`; `tibble::as_tibble`; DBI query, statement, fetch, and
table-read functions; `dbplyr::remote_query_plan`; and
`arrow::as_arrow_table`. The catalog snapshot makes a removed entry visible,
and the transitive reachability snapshot records all marginplyr functions that
can reach one. Runtime tracing at lines 175-347 closes the structural scan's
main blind spot: execution performed by another package.

The controls are part of the protection, not incidental tests. DuckDB proves
that nested execution calls are counted (`test-query-policy.R:349-367`); Arrow
and dtplyr positive and negative controls show the subject filters are neither
always true nor always false (lines 371-401 and 510-557); attached bindings and
an entry point nested inside a forced subject remain visible (lines 589-619).

## Mutation design

The eight adopted mutants came from the seams above rather than from a generic
operator pass. Every one remained valid R, was ordinarily reachable, and
represented a plausible off-by-one, refactor, dispatch, or guard error. The
expected killer was recorded before execution.

| ID | Production location and exact mutation | Contract violation and expected consequence | Expected killer |
|---|---|---|---|
| M1 | `R/backend-metadata.R:44`: `head(.data, n = 0L)` to `head(.data, n = 1L)` | The enumerated zero-row exemption becomes a one-row read. dtplyr and DuckDB materialize caller data during preparation while returning the same schema and final query shape. | No certain killer: catalog, reachability, capability, and collection count do not change. |
| M2 | `R/backend-metadata.R:41`: `as.data.frame(arrow::schema(.data))` to `as.data.frame(.data)` | Arrow metadata inspection becomes full data conversion before a verb returns. | Arrow runtime zeroes in `test-query-policy.R`. |
| M3 | `R/grouping-adapter-union.R:169`: `ignore.case = TRUE` to `FALSE` | The installed Arrow warning begins with capitalized `Pulling`; the pre-read handler misses it, Arrow absorbs, and only the post-read guard refuses. | The absorbing-shape runtime loop in `test-query-policy.R`; the Arrow warning gate is secondary. |
| M4 | `R/inspect-grouping.R:160`: unexecuted `dplyr::select()` normalization to `arrow::as_arrow_table()` | Direct reader inspection consumes its one-shot input. | The direct-reader runtime zero in `test-query-policy.R`; residual-row tests are secondary. |
| M5 | `R/backend-metadata.R:28-35`: isolated zero-row `as.data.table` path to `tibble::as_tibble(.data)` | Typed inspection of a Mutable dtplyr step materializes the caller's whole step instead of the isolated proxy. | Reachability snapshot plus Mutable-step runtime zero in `test-query-policy.R`. |
| M6 | `R/grouping-backend.R:199`: generic SQL gains `collect_selection_proxy` | The enumerated exemption is extended to an unjustified backend kind; RSQLite sends a new caller-table query. | Capability snapshot in `test-query-policy.R`; RSQLite Sent-query assertions are secondary. |
| M7 | `R/utils.R:178`: recursive `arrow_input_has_reader_source(x)` to direct-only `inherits(x, "RecordBatchReader")` | A query whose root, join-right, or union-right source contains a one-shot reader bypasses the reusable-input refusal. The verb can construct multiple branches from a non-reusable source. | Reader/query admission and residual-row tests in `test-verb-argument-admission.R`. |
| M8 | `R/margin-label.R:200`: the false guard gains `&& !inherits(.data, "tbl_lazy")` | `.check_margin_label = FALSE` stops suppressing the observed-value scan for dbplyr inputs. A default RSQLite call reads grouping-column values. | RSQLite silence in `test-margin-label.R` and Sent-query audit; `test-query-policy.R` alone was expected to miss this backend-specific change. |

M7 tests the direct-reader boundary named in ADR 0020's amendment and requested
for this campaign. Its immediate defect is admission of a non-reusable source,
not a read during the public verb itself; it was retained because that guard is
the production seam preventing later branch execution from consuming the same
stream more than once.

One further candidate removed `vars = "z"` from the table-free dialect probe.
It was screened out rather than counted: with dbplyr simulation it made query
construction unanswerable instead of cleanly isolating the intended extra
field-discovery query. Replacing the probe with a caller-table read would
require a multi-site redesign and was also rejected as an unrealistic mutant.

Changing Grouping-plan expansion, share denominator mapping, or share value
calculation was excluded because it repeats
`investigation/focused-mutation-testing-margin-semantics.md`. Collecting a
finalized result and repository-wide call mutations were excluded as broader
than a targeted seam test.

## Isolated execution

The source state was commit `361d335688d2dbb9acbf03a7309ec4de75c9d05f`.
The working tree was clean before the investigation. Every mutant lived in a
deep, non-symlinked copy under
`/private/tmp/marginplyr-query-policy.4CnpKf`; no mutant was written into the
source checkout.

The observed environment was R 4.6.1 on Darwin arm64, with testthat 3.3.2,
pkgload 1.5.3, dplyr 1.2.1, dbplyr 2.6.0, Arrow 25.0.1, dtplyr 1.3.3, DuckDB
1.5.5, RSQLite 3.53.3, and DBI 1.3.0. All optional backends required by the
focused tests were installed. Arrow printed sandboxed `sysctlbyname` cache-size
notices; the same notices appeared in the passing baseline, were not test
failures, and were not used as mutation outcomes.

The unmodified focused baseline covering `query-policy`,
`verb-argument-admission`, `margin-label`, `sent-queries`, and `share-backends`
passed in 20.23 seconds. Each mutant first ran `query-policy`; a survivor then
ran its closest seam-specific files. Only M1 survived that stage, so only M1
ran the complete `tests/testthat` suite. No timeout, load failure, missing
backend, or environment failure was counted as a kill.

| ID | Focused result and evidence | Full suite | Final disposition |
|---|---|---|---|
| M1 | SURVIVED `query-policy`, `grouping-backends`, and `sent-queries` | Passed in about 66 seconds | SURVIVED; **TEST GAP** after runtime triage |
| M2 | `test-query-policy.R:404,416,423,441,495` observed 1–4 entry-point invocations where zero was required, over summary, expansion, inspection, absorbing, and refusing Arrow shapes | Not run | KILLED |
| M3 | `test-query-policy.R:441` observed four invocations for each of the four absorbing Arrow shapes; the same warning and refusal path otherwise remained reachable | Not run | KILLED |
| M4 | `test-query-policy.R:423` observed one `arrow::as_arrow_table()` invocation during direct-reader inspection | Not run | KILLED |
| M5 | The reachability snapshot added `mutable_dtplyr_selection_proxy`; `test-query-policy.R:577` observed one invocation for typed root and derived Mutable-step inspection | Not run | KILLED |
| M6 | The capability snapshot added `sql`; independently, `test-sent-queries.R` recorded `selection_proxy` with `SELECT * FROM sent_queries LIMIT 0` where RSQLite was required to record only `result` | Not run | KILLED |
| M7 | `test-verb-argument-admission.R:833,895` observed lost refusals for reader-query summary/expansion and reader sources at query root, join-right, and union-right | Not run | KILLED |
| M8 | `test-margin-label.R:1047` observed a default RSQLite collision query and refusal; `test-sent-queries.R` recorded unexpected `observed_label_collision` rows before `result` | Not run | KILLED |

### Summary

| Result | Count |
|---|---:|
| Adopted mutants designed | 8 |
| Adopted mutants executed | 8 |
| KILLED | 7 |
| SURVIVED focused and full suite | 1 |
| Confirmed TEST GAP | 1 |
| EQUIVALENT / CONTRACT-PRESERVING | 0 |
| INVALID MUTANT | 0 |
| ERROR / ENVIRONMENT BLOCKED | 0 |
| INCONCLUSIVE | 0 |

The counts are a disposition ledger, not a mutation score. The campaign was
stopped after these seams; no surviving operator set was expanded and no
repository-wide run was made.

## Confirmed test gaps

### M1 — zero-row selection proxy changed to one row

Classification: **TEST GAP**. Severity: **High**.

ADR 0020 exempts `collect(head(.data, 0L))`, not an arbitrary small sample.
The mutant reads one row and every selected column from a dtplyr or DuckDB
input while a Margin verb is still preparing its Grouping plan. The caller did
not set an option asking for that read. The operation's returned lazy query and
its eventual result can remain identical, which is why result assertions do
not distinguish the mutant.

The triage observer wrapped the actual `head()` and `collect()` dispatch used
by a public `summarize_with_margins()` call over a three-row DuckDB table. It
recorded the `n` handed to `head()`, the rows in the materialized selection
proxy, the Sent SQL, and the class of the returned Margin query. The unmodified
copy produced:

```text
requested_n=0
materialized_rows=0
selection_sql=SELECT * FROM mutation_probe LIMIT 0
result_class=.../tbl_sql/tbl_lazy/tbl
```

The mutant produced:

```text
requested_n=1
materialized_rows=1
selection_sql=SELECT * FROM mutation_probe LIMIT 1
result_class=.../tbl_sql/tbl_lazy/tbl
```

This directly observes execution and materialization; it is not inferred from
a changed return value. It also rules out equivalence: one caller row crossed
the backend boundary in the mutant and none did in the baseline. The row limit
does not reduce the defect to metadata inspection; ADR 0020 explicitly rejects
bounded rows as a cost argument, and a hosted DuckDB service cannot be told
apart from a local one by `grouping_backend()`.

A minimal public reproducer is:

```r
con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
remote <- dplyr::copy_to(
  con,
  data.frame(group = c("E", "E", "W"), value = 1:3),
  "mutation_probe",
  temporary = TRUE
)

options(marginplyr.audit_sql = TRUE)
query <- summarize_with_margins(
  remote,
  total = sum(value),
  .grouping = rollup(where(is.character)),
  .margin_label = NULL
)
last_sent_queries()
```

The Sent-query record exposes `LIMIT 0` versus `LIMIT 1`; the triage wrapper is
what additionally proves the recorded statement was executed and returned one
row. Disconnect the connection and restore the option after the reproduction.

The existing suite missed M1 for four separate but aligned reasons:

- the execution-entry-point catalog still contains the same `collect()`;
- the transitive reachability set still contains the same functions;
- the capability snapshot still names exactly `dtplyr` and `duckdb`;
- backend tests count one `head()`/`collect()` and Sent-query tests assert the
  purpose and source table, but none asserts `head()`'s `n`, the proxy's row
  count, or the `LIMIT 0` property.

The smallest durable test is an assertion at the selection-proxy seam, beside
the existing dtplyr and DuckDB proxy tests: call
`grouping_selection_proxy()` for each granted backend and require
`nrow(proxy) == 0L`. That states the contract directly without snapshotting a
dialect's complete SQL rendering. If the existing wrapper is extended, also
recording and asserting `n == 0L` gives a more local failure. Deriving the
backend cases from the capability table would keep a future justified grant
from escaping the same row-count assertion.

Severity is High because this is a direct breach of the package's top-level
lazy-input contract and can transmit actual caller values before consent. It
is limited to capability-granted backends and one row per preparation, but the
read includes all selected columns and backend kind does not establish that
the source is local or free.

There were no other survivors, so no survivor remains unclassified.

## Existing protection that worked

The runtime half of `test-query-policy.R` was the strongest protection. It
killed metadata conversion, direct-reader materialization, Mutable-step
materialization, and an Arrow warning-handler regression by observing execution
while the verb ran. M3 is particularly important: the outward Package
condition still arrived from the post-read guard, but the runtime counter saw
the forbidden read that an error-only test would miss.

The structural snapshots did complementary work. M5 introduced a newly direct
entry point and changed the transitive function set; M6 changed no call body but
extended the enumerated capability, and the separate capability snapshot caught
it. The RSQLite Sent-query assertions independently showed the concrete extra
`LIMIT 0` query for M6.

The adjacent suites cover boundaries the generic query-policy file does not.
M7 and M8 both passed `test-query-policy.R`: reader-query admission was caught
by the source-graph cases in `test-verb-argument-admission.R`, while the SQL-only
opt-out regression was caught by the explicit RSQLite silence case and the
Sent-query audit. Those are genuine parts of the current protection rather than
redundant outcome tests.

## Recommendation

Add one durable zero-row assertion at the selection-proxy seam. The current
suite is strong against new execution entry points, new exempt backend kinds,
Arrow fallback/materialization, direct-reader consumption, and opt-in guard
drift, but it does not protect the defining quantitative property of exemption
1: zero rows. M1 survived the entire suite and directly materialized caller
data, so the gap is confirmed rather than hypothetical.

No production change is indicated by this campaign: the unmutated code used
`n = 0L` and satisfied the contract. Production code and the durable test suite
were not changed. The only source-checkout artifact added was this historical
investigation note; all mutations, the runtime observer, and execution artifacts
remained under `/private/tmp`.
