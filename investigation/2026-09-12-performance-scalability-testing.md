# Performance and scalability testing of Margin operations

Investigated: 2026-09-12

This note records the contract reading, experiment design, controlled
measurements, and classifications from the 2026-09-12 investigation. It
deliberately does not repeat the earlier semantic, mutation, or
lazy-query-policy campaigns.

Production code, permanent tests, and CI were outside the scope of this phase.

## Summary

The controlled series covered all five public operations, the local, dtplyr,
in-memory Arrow, live RSQLite, and live DuckDB backends, and the following
independent dimensions: source rows, plan dimensions/set count, duplicate
occurrences, fixed-key cardinality, ordinary-summary count, share count and
unique share-source count, and requested Margin order. Construction and
collection were measured separately for lazy inputs.

Most growth matched the contract. Eager output/allocation followed result or
cell-payload size; the balanced dtplyr union had exactly `S - 1` union nodes and
depth `log2(S)`; DuckDB native summaries retained one base scan; same-kind
shares retained one join; typed-proxy count stayed one; and collision checking
retained one query as dimensions grew.

Two Parent-share planning costs were confirmed:

1. A dbplyr Parent-share query repeats the complete staged Margin summary once
   on the left and once in every parent-mapping branch. A 32-dimension rollup
   rendered 33 copies of `GROUPING SETS` and 911,061 bytes of SQL, versus two
   copies and 53,814 bytes for the corresponding Total share. DuckDB factored
   the repeated aggregate into one common subplan and one base scan, but that
   happens only after the client has constructed, transferred, and the backend
   has parsed the expanded SQL.
2. `parent_set_ids()` compares every occurrence with every possible parent and
   performs set membership inside that comparison, even though the accepted
   specification is a pure rollup. Its isolated median grew from 0.00042 seconds
   at 17 sets to 0.113 seconds at 257 sets. This is a real algorithmic issue but
   has low observed impact at ordinary dimension counts.

The first is medium severity and the second low severity. No result justifies
changing production code, permanent tests, or CI during this phase.

## Question

When one independently increases source rows, grouping dimensions, grouping-set
occurrences, fixed-key cardinality, summary expressions, contextual-share
calculations, input columns, or duplicate occurrences, do the public Margin
operations exhibit growth not justified by their result contract or selected
backend strategy?

The observations of interest are structural as well as temporal: branch and
scan counts, SQL or lazy-query size and depth, backend executions,
materialization, metadata/proxy acquisitions, and work per grouping-set
occurrence. Absolute speed and comparisons between backend engines are not the
question.

## Sources and authority

The repository sources read for this contract were:

- [`CLAUDE.md`](../CLAUDE.md), its [`AGENTS.md`](../AGENTS.md) reference, and
  [`CONTEXT.md`](../CONTEXT.md);
- [`design/architecture.md`](../design/architecture.md), especially *Lifecycle*,
  *Backend metadata*, *Grouping plan*, *Contextual shares*, *Native adapter*,
  *Portable adapter*, and *Test seams*;
- [ADR 0002](../design/adr/0002-acquire-typed-metadata-once.md),
  [ADR 0005](../design/adr/0005-reject-local-errors-before-backend-reads.md),
  [ADR 0008](../design/adr/0008-centralize-grouping-specification-kind-rules.md),
  [ADR 0010](../design/adr/0010-compute-parent-shares-as-a-contextual-summary.md),
  [ADR 0013](../design/adr/0013-inspect-grouping-plans-as-ordinary-tibbles.md),
  [ADR 0014](../design/adr/0014-select-parent-share-adapters-from-prepared-backend-kind.md),
  [ADR 0017](../design/adr/0017-calculate-total-shares-against-the-grand-total-set.md),
  [ADR 0018](../design/adr/0018-order-margin-results-by-grouping-structure.md),
  [ADR 0020](../design/adr/0020-ask-before-reading-a-lazy-input.md),
  [ADR 0025](../design/adr/0025-refuse-a-summary-a-backend-would-absorb.md),
  [ADR 0027](../design/adr/0027-record-the-sql-marginplyr-sends.md), and
  [ADR 0029](../design/adr/0029-refuse-a-step-a-backend-would-write-to.md);
- the public verbs and their execution paths in
  [`R/inspect-grouping.R`](../R/inspect-grouping.R),
  [`R/summarize_with_margins.R`](../R/summarize_with_margins.R),
  [`R/expand_with_margins.R`](../R/expand_with_margins.R),
  [`R/nest_with_margins.R`](../R/nest_with_margins.R), and
  [`R/nest_by_with_margins.R`](../R/nest_by_with_margins.R);
- the shared implementation in
  [`R/margin-operation.R`](../R/margin-operation.R),
  [`R/backend-metadata.R`](../R/backend-metadata.R),
  [`R/grouping-plan.R`](../R/grouping-plan.R),
  [`R/grouping-backend.R`](../R/grouping-backend.R),
  [`R/grouping-adapter-union.R`](../R/grouping-adapter-union.R),
  [`R/grouping-adapter-native.R`](../R/grouping-adapter-native.R),
  [`R/margin-label.R`](../R/margin-label.R),
  [`R/share.R`](../R/share.R), and [`R/sent-queries.R`](../R/sent-queries.R);
- the operation, backend, query-policy, SQL-audit, share, and branch-combination
  tests under [`tests/testthat/`](../tests/testthat/).

Repository artifacts are authoritative for the implementation and decisions.
Earlier investigation notes below are evidence as of their own dates.

## Earlier campaigns and the boundary of this investigation

The 2026-09-12 checkout contained these completed campaigns:

| Campaign | Evidence | What it already established | Boundary here |
|---|---|---|---|
| Metamorphic testing | [`metamorphic-testing-margin-semantics.md`](metamorphic-testing-margin-semantics.md), [`test-metamorphic-margin-semantics.R`](../tests/testthat/test-metamorphic-margin-semantics.R) | 25 relation families and 7,349 checks covered algebraic and representation-changing relations across plan forms, row permutations, labels, duplicate policies, local and lazy backends. The one RSQLite typed-missing/order violation was repaired by `cba20fa` through `sql_margin_type_anchor()` and has durable coverage. | Do not rerun its semantic relation matrix or repaired counterexample. Reuse only its small data generators and backend-normalization conventions if useful. |
| Property-based testing | [`property-based-testing-margin-semantics.md`](property-based-testing-margin-semantics.md), [`property-based-testing-margin-semantics.R`](property-based-testing-margin-semantics.R) | Seven property families, 3,350 generated cases, and 21,408 assertions found a dtplyr fixed-key backtick defect and an Arrow repeated-bit-order defect. They were repaired by `0272022` and `cfb7e60`, respectively, with durable backend/order coverage. | Do not repeat the seeded randomized sweep or repaired counterexamples. Deterministic scaling series may borrow its generators after fixing all but one dimension. |
| Focused mutation testing | [`focused-mutation-testing-margin-semantics.md`](focused-mutation-testing-margin-semantics.md) | 53 focused semantic mutants: 50 killed and three survivors semantically triaged into two equivalents and one test gap. The exact local `NaN`-share assertion was added by `cd2b7b1`; no production defect remained. | Do not rerun these semantic mutants. Use its finding that a surviving mutant needs direct runtime triage rather than treating suite status as performance evidence. |
| Lazy-query-policy mutation testing | [`targeted-mutation-testing-lazy-query-policy.md`](targeted-mutation-testing-lazy-query-policy.md) | Seven of eight execution/materialization mutants were killed. The survivor changed the exempt typed proxy from zero to one row; `81c1fc5` added the direct zero-row assertion while the production path remained compliant. Runtime tracing and the structural execution-entry-point catalog proved the other seams. | Do not rerun the mutants. Reuse the entry-point counter, Sent-query audit, and zero-row/materialization instrumentation to count behavior across scale. |
| Public contract coverage | merge `361d335` and commit `c538ae2`, with the public seams described in [`design/architecture.md`](../design/architecture.md) | The British/American summary aliases were made identical and documented `retail_sales` domains gained direct assertions. The repository contained no separate `investigation/*contract*.md` note on 2026-09-12. | Treat these as interface coverage, not as performance coverage. Do not repeat them. |

Two earlier performance investigations are directly relevant:

- [`parent-share-local-benchmark.md`](parent-share-local-benchmark.md) measured a
  100,000-row, five-set local rollup after one warm-up. Integrating scalar
  validation into summary execution and removing grouping-set-proportional
  full-input rescans reduced the observed median from 0.342 to 0.194 seconds.
  The durable contract is not that timing; it is that share validation adds no
  validation-only source pass or query.
- [`backend-api-audit.md`](backend-api-audit.md) found a second typed-proxy
  acquisition, per-dimension label queries, and Arrow query execution for
  schema. [`backend-api-improvement-plan.md`](backend-api-improvement-plan.md)
  records their implementation: one reused proxy, Arrow schema metadata, and
  one batched collision query. The 2026-09-12 implementation agrees:
  `prepare_margin_operation()` passes the one `data_proxy` to
  `margin_column_info()`, and `check_observed_label_collision()` builds one
  aggregate over all columns it must inspect.
- [`get-col-names-api.md`](get-col-names-api.md) established that the selected
  `dplyr::tbl_vars()` route reads names as metadata rather than collecting a
  zero-row result. Its dtplyr and Arrow timing ratios were directional evidence,
  not package benchmarks; the durable seam is `get_col_names()`.
- [`query-cost-across-lazy-backends.md`](query-cost-across-lazy-backends.md)
  established why row-bounded reads are not cost-bounded and why backend kind
  cannot stand in for query price. It also measured the opt-in collision scan
  on one local DuckDB shape. This investigation counts the scan and its growth;
  it does not rank vendor engines or infer cost from returned rows.
- [`share-source-schema-vs-data-read.md`](share-source-schema-vs-data-read.md)
  showed on RSQLite that `dbSendQuery()` may execute and buffer an aggregate
  before `dbFetch()` or `dbColumnInfo()`. Execution instrumentation must
  therefore count the send/engine work and not infer laziness from fetch count.
- [`what-arrow-does-with-an-untranslatable-summary.md`](what-arrow-does-with-an-untranslatable-summary.md)
  and [`arrow-r-input-shapes-and-dplyr-fallback.md`](arrow-r-input-shapes-and-dplyr-fallback.md)
  established the full-input Arrow absorption that ADR 0025 now refuses. A
  lazy-looking result class alone is not evidence of absent materialization.
- [`what-dplyr-explain-sends-per-backend.md`](what-dplyr-explain-sends-per-backend.md)
  established that `dplyr::explain()` itself sends SQL on live database
  backends and that plan text changes with backend versions and settings. It is
  an exploratory live-plan metric, not a stable byte-for-byte assertion.

## Performance contract derived from the design

Notation used below:

- `N`: source row count;
- `C`: source column count;
- `D`: resolved variable grouping-dimension count;
- `F`: fixed `.by` key count;
- `K`: fixed-key cardinality (number of fixed partitions);
- `S`: grouping-set occurrence count after duplicate policy;
- `S0`: occurrence count before `.duplicates = "drop"` removes repeats;
- `E`: expanded ordinary-summary expression/output count;
- `H`: contextual-share output count;
- `U`: number of distinct source summaries used by those shares;
- `R`: staged/result row count.

These are qualitative contracts, not promises of a machine-independent
constant factor.

### Grouping specification compilation and `inspect_grouping()`

Implementation path: public constructors capture expressions;
`prepare_grouping_plan()` performs one structural preflight, obtains column
names and at most one typed proxy, and calls `compile_grouping_spec()`;
`expand_grouping_family()` dispatches to `grouping_set`, `grouping_sets`,
`rollup`, `cube`, or `grouping_spec` product expansion; `format_grouping_plan()`
formats the plan for inspection.

Expected growth:

- a rollup of `D` units produces `S = D + 1` occurrences;
- a cube of `D` units produces `S = 2^D` occurrences;
- `grouping_sets()` adds the occurrence counts of its children;
- `grouping_spec()` takes their Cartesian product, so its count multiplies;
- normalization, duplicate keys, masks, and formatted included/omitted lists
  necessarily scale with the plan representation, approximately `O(S * D)`;
- compilation and inspection are independent of `N`, except for the explicitly
  permitted acquisition mechanism of the typed schema proxy. A name-only
  inspection of a Mutable dtplyr step avoids even that execution by using a
  names proxy; typed inspection uses an isolated zero-row root.

Backend behavior and existing protection:

- local inputs reuse the object as proxy; Arrow uses `arrow::schema()` without
  execution; dtplyr and DuckDB collect `head(.data, 0)` once; generic SQL,
  PostgreSQL, and unrecognized lazy inputs do not gain that capability;
- ADR 0002 and operation tests assert one typed snapshot; Arrow tests assert
  zero collection; [`test-query-policy.R`](../tests/testthat/test-query-policy.R)
  catalogs execution entry points and the capability grant;
- recognized nested arguments are evaluated once per top-level call and their
  preflight record is reused across compilation passes (ADR 0008).

Potential scaling question not yet answered by those protections: whether the
implementation constants of `expand_cube()`, Cartesian-product expansion, or
duplicate normalization become disproportionate before the unavoidable `S * D`
plan size does.

### `summarize_with_margins()` on the portable path

Implementation path: `prepare_margin_operation()` -> summary planning and
selection -> `stage_margin_summaries()` -> `summarize_margin_union()` -> one
`summarize_margin_branch()` per occurrence -> `combine_margin_branches()` ->
optional share execution -> common finalization.

Expected growth:

- construction creates one summary branch per retained occurrence, so branch
  count is `S`, and expanded summary expressions are rewritten per branch;
- on local data each branch evaluates its aggregate over the source. Source
  processing is therefore expected to grow approximately with `S * N` and
  `S * E`, subject to group cardinality and the aggregate's own cost;
- a lazy portable backend returns one unexecuted result query. `S` branches do
  not mean `S` client round trips, but the result statement contains `S`
  source branches (plus the generic-SQL zero-row type anchor where applicable),
  and an engine may plan or scan each branch separately;
- eager branches are combined in one `bind_rows()` pass, linear in total branch
  output. Lazy branches are paired and halved; the architecture states
  `O(S log S)` combination work and `O(log S)` nesting depth, replacing the old
  quadratic/linear-depth fold;
- SQL size is expected to be linear in branch count times the repeated branch
  projection/aggregation. It must not grow faster merely because of the
  association used while constructing the union;
- increasing `K` changes output groups and `R`, but not branch count, typed
  metadata count, or the number of SQL statements returned;
- `.sort != "none"` adds one final order, not one order per branch. Execution
  of that order is expected to follow the backend's sort cost in `R`, while the
  constructed key grows with `F + D` and any occurrence-id lists it embeds.
  `margin_order_terms()` compares each nonconstant Grouping-bit occurrence list
  with lists already emitted, which admits a worst-case planning term near
  `O(D^2 * S)` while still emitting at most `D` bit terms. The sort series must
  distinguish that client-side construction from the backend's sort.

Backend behavior and existing protection:

- local, dtplyr, Arrow, generic SQL, and native-capability backends forced off
  native execution use the portable adapter;
- Arrow refuses an Absorbing summary before it reads the input, and contextual
  shares are rejected before ordinary-summary staging;
- [`test-branch-union.R`](../tests/testthat/test-branch-union.R) proves the
  eager path does not call pairwise `union_all()`, executes a 512-occurrence
  local expansion, collects a 512-occurrence dtplyr expansion without the old
  C-stack failure, and requires a flat top-level SQL union;
- operation tests require lazy result classes and exactly one typed proxy for
  dtplyr/DuckDB and none for Arrow;
- ADR 0021 confines repeated user conditions to the portable per-occurrence
  evaluation and reports equal repetitions once. That is diagnostic
  deduplication, not fewer expression evaluations.

### `summarize_with_margins()` on native `GROUPING SETS`

Implementation path: the same preparation and summary planning, followed by
`summarize_margin_native()`, one grouped `summarize()`, attachment of one
`lazy_grouping_sets_query`, and rendering through
`sql_build.lazy_grouping_sets_query()`.

Expected growth:

- one result statement contains one `GROUPING SETS` clause; the number of
  grouping-set entries is `S` and total identifier membership is bounded by
  `S * D`;
- ordinary summaries appear once rather than once per union branch, so their
  syntactic contribution is approximately `O(E)`;
- a requested occurrence identifier, contextual share, or Margin order may
  require `grouping_set_id_sql_expr()`, whose `CASE` identifies occurrences by
  every dimension and is expected to contribute `O(S * D)` SQL text;
- display labels add at most one grouping flag per labelled dimension;
- source execution is one database statement. Whether the engine implements
  it with one physical scan is backend-plan behavior and must be checked with
  the live backend's plan rather than assumed from the SQL spelling.

Backend behavior and existing protection:

- DuckDB and PostgreSQL advertise native grouping sets. A retained duplicate
  occurrence with an identifier can force the portable path when occurrence
  identity cannot be expressed safely; generic SQL remains portable;
- native tests assert `GROUPING SETS` SQL and native/portable/local semantic
  equivalence. They do not establish SQL-size slopes or physical scan count.

### `expand_with_margins()`

Implementation path: common preparation -> `execute_margin_expand()` ->
`expand_margin_union()` -> one labelled copy of the input per occurrence ->
`combine_margin_branches()` -> common finalization.

Expected growth:

- the result contains each source row once per occurrence, so result rows,
  eager allocation, and the minimum execution work are `S * N` by contract;
- query construction has `S` branches on every lazy backend; there is no
  native grouping-sets expansion path;
- wider input is material to the result: branch width and output bytes grow
  with `C`; label expressions grow with omitted dimensions;
- fixed-key cardinality does not change output row count, because expansion
  copies rows rather than reducing them;
- `.sort` adds one final ordering over `S * N` rows, not branch-local orders.

Backend behavior and existing protection:

- local execution is eager; dtplyr, Arrow, and SQL execution remains lazy until
  caller collection;
- generic SQL may prepend one zero-row type anchor. This changes branch syntax,
  not result rows or a caller-data scan;
- branch-combination and lazy-materialization protections are the same as for
  portable summary, and operation tests assert `UNION ALL` and lazy return.

The `S * N` increase is expected and justified. Faster-than-output growth in
allocation, repeated metadata acquisition, query depth, or source scans is the
target of the experiment.

### `nest_with_margins()` and `nest_by_with_margins()`

Implementation path: common preparation -> `execute_margin_nest()` -> portable
expansion -> factor restoration for payload columns -> one grouped summarize
that constructs each cell. `nest_by_with_margins()` then collects the common
result and makes it row-wise.

Expected growth:

- nesting first expands every source row for every occurrence, so total rows
  represented across all cells remain `S * N`; time and cell payload allocation
  should follow that size;
- outer row count depends on `K`, grouping-value cardinality, and `S`, but a
  larger `K` must not change branch count or metadata acquisitions;
- `.keep = TRUE` copies `F + D` grouping columns into cells, increasing payload
  width linearly while keeping row counts unchanged;
- `nest_with_margins()` stays lazy for immutable dtplyr steps and is eager for
  local frames. SQL and Arrow inputs are outside the nesting admission
  contract;
- `nest_by_with_margins()` intentionally materializes once to return a local
  row-wise data frame. Its empty-result reconstruction may perform a second
  zero-row collection, as documented in ADR 0020; that exception must be
  measured separately from nonempty inputs.

Existing protection includes typed-proxy count tests, lazy-versus-row-wise
collection tests, and the Mutable-step refusal before branch construction.
There is no durable scaling assertion over cell count, payload bytes, or query
depth beyond the shared branch-union tests.

### Contextual Parent and Total shares

Implementation path: static request planning -> ordinary Margin summary stage
-> `execute_shares()` -> one adapter pass for each requested kind ->
`apply_joined_shares()`. Parent shares call `parent_set_ids()`, build one
mapping branch for every occurrence having a parent, add one join key per
dimension, and join once. Total shares select the Grand total rows and join
once on fixed keys (or one constant key).

Expected growth:

- all requests of one kind share one denominator mapping and one join. The join
  count is therefore the number of distinct requested kinds, at most two, and
  must not grow with `H` or `U`;
- ratio expressions and denominator columns grow approximately with `H` and
  unique source count `U`;
- denominator names are allocated one source at a time against the growing set
  of names already used. A faster-than-linear construction slope in `U` may be
  name-collision bookkeeping rather than share joins and should be instrumented
  separately;
- Parent target discovery in `parent_set_ids()` compares each occurrence with
  later candidate occurrences and checks their dimensions. The implementation
  therefore admits approximately `O(S^2 * D)` planning work even though a pure
  rollup has an immediately adjacent parent. This is a high-value scaling
  target, not yet a confirmed issue;
- Parent lazy mapping creates `O(S)` mapping branches and combines them through
  the shared balanced union, with its `O(S log S)` construction and `O(log S)`
  depth contract. Adding matching keys inspects occurrence membership for each
  dimension, approximately `O(S * D)` outside backend execution;
- Total mapping is one filter/projection and does not grow a branch family with
  `S`;
- local and dtplyr source validation is embedded in the ordinary summary, so it
  adds no input rescan. SQL validation asks a table-free dialect question, not
  the caller's staged result.

Backend behavior and existing protection:

- local and unrecognized non-SQL lazy results use row-matched joins; dtplyr adds
  only its temporary backtick-safe join-name rewrite; DuckDB, PostgreSQL, and
  generic SQL use one missing-safe SQL join per kind;
- Arrow shares are refused before summary-query staging;
- [`test-share-backends.R`](../tests/testthat/test-share-backends.R) requires one
  join for multiple same-kind shares, no staged-result type/cardinality/result
  read, at-most-once caching of an answered dialect verdict, retry of an
  unanswered verdict, and one versus two table-free dialect queries;
- [`parent-share-local-benchmark.md`](parent-share-local-benchmark.md) is
  evidence that the former per-set input rescans were removed. No timing
  threshold is durable.

### Metadata, collision checks, and Sent queries

The cross-cutting contract is:

- one typed selection proxy per top-level operation, independent of `N`, `D`,
  `S`, `E`, and `H`; its width is `C`;
- Arrow obtains it from schema without execution; dtplyr and DuckDB use exactly
  one zero-row collection; other kinds do not acquire a typed local proxy;
- `margin_column_info()` reuses that proxy. For summary it reads dimensions and
  fixed keys; for expansion and nesting every carried source column can matter,
  so metadata work may grow linearly with `C` but not `N`;
- an opted-in observed Margin-label check issues one aggregate/collect for all
  eligible dimension columns. Query/execution count stays one as `D` grows,
  while aggregate expressions and scan width grow linearly. With the lazy
  default it issues none;
- a first answered SQL-dialect share probe costs one query for a Converting
  dialect or two for a refusing dialect, references no caller table, and is
  reused across later connections carrying that dialect. An unanswered attempt
  is deliberately retried on the next share request;
- the returned lazy result remains unexecuted. `last_sent_queries()` can expose
  the result statement plus internal SQL sent by marginplyr, but it records SQL,
  not dtplyr or Arrow execution. It therefore complements rather than replaces
  entry-point instrumentation.

## Scaling dimensions selected for controlled experiments

Each series changes one dimension only. Values below are deliberately small
enough to identify slopes and query shape without turning the campaign into a
stress contest; the final maximum can be reduced when a backend is already
clearly nonlinear.

| Series | Changed dimension and tentative range | Fixed variables | Internal path stressed | Why it is high value |
|---|---|---|---|---|
| P1 | `D = 1, 2, 3, 4, 5, 6` under `rollup()` | tiny `N`, one summary, no sort/share | plan expansion, masks, portable/native grouping lists | Gives `S = D + 1`, separating dimension growth from exponential set growth. |
| P2 | `D = 1..8` under `cube()` (`S = 2..256`) | tiny `N`, one added same-type/cardinality grouping column per step, fixed non-grouping columns, one summary | combination, query depth, SQL size, set-id CASE, Parent target planning | The minimum series that exposes behavior in `S`; stop before larger output hides construction. |
| P3 | explicit `grouping_sets()` with `S = 1, 2, 4, 8, 16, 32, 64` | fixed `D = 6`, tiny `N`; select unique subsets in a stable order | occurrence-proportional work without changing `D` | Separates `S` from `D` and from cube generation itself. |
| P4 | duplicate occurrences `S0 = 1, 2, 4, 8, 16, 32, 64` | one or two unique sets, `.duplicates = "drop"` then `"keep"`, fixed `N/D` | pre-drop normalization versus retained branch work; native fallback with identifier | Shows whether dropped duplicates leak into execution and whether retained occurrences cost more than linearly. |
| D1 | `N = 10^2, 10^3, 10^4, 10^5` | fixed `D`, `S`, cardinalities, `C`, `E` | local execution, lazy collect, collision scan | Query construction/count/SQL size should be invariant; execution should follow the designed passes and result size. |
| D2 | `K = 1, 4, 16, 64` fixed-key partitions | fixed `N`, `S`, `F = 1`, balanced rows per partition | group cardinality, Parent/Total joins, nesting outer rows | Distinguishes more groups/output rows from more scans or branches. |
| X1 | expanded summary outputs `E = 1, 2, 4, 8, 16, 32` | fixed `N/D/S`; no shares | static expression walks, per-branch rewrites, SQL projections | Portable path should be `S * E`; native syntax should add each summary once. |
| X2 | same-kind shares `H = 1, 2, 4, 8, 16`, then one Parent plus one Total kind | fixed `N/D/S/E`; reuse one source, then vary `U` separately | request planning, denominator naming, ratio projection, join reuse | Same-kind join count must stay one; adding the second kind must add one pass/join, not one per output. |
| W1 | source width `C = 4, 8, 16, 32, 64` with unchanged referenced columns | fixed `N/D/S/E` | metadata/proxy acquisition and carried-column paths | Summary should not build extra grouping branches; expand/nest legitimately carry wider output. Metadata count must remain one. |
| O1 | `.sort = "none"` versus one requested order across `D/S` series | same input and plan | sort-key construction, set-id staging, final arrange | Confirms one outer order and bounds growth of embedded occurrence-id expressions. |
| N1 | nesting with `S = 1, 2, 4, 8, 16` and separately `.keep` false/true | fixed `N/C/K` | expansion, cell construction, row-wise collection | Expected payload is `S * N`; distinguishes justified duplication from extra intermediate growth. |

`P2` and `P3` must not be interpreted together as one two-dimensional series.
`X2` first holds `U = 1`; a separate subseries changes `U` with `H` fixed so
denominator-column allocation can be distinguished from share-output count.

## Metrics and their interpretation

No case is classified from elapsed time alone.

| Metric | Applies to | Meaning |
|---|---|---|
| median elapsed and spread after warm-up | local construction/execution and lazy construction/collect measured separately | Directional evidence for a slope or a change point; never a durable absolute threshold. |
| allocated bytes and peak/resident memory where available | local summary, expand, nesting; lazy construction separately from collect | Whether growth follows unavoidable result/query size or indicates repeated copying/materialization. |
| `S`, output `R`, and output bytes | every executed series | Denominator for interpreting time and allocation. `expand`/nest payload growth proportional to `S * N` is expected. |
| typed-proxy calls and rows materialized | every operation/backend | ADR 0002's count must remain at most one; granted collection must materialize zero rows regardless of scale. |
| execution-entry-point invocations | Arrow, dtplyr, and deliberately instrumented lazy inputs | Detects a lazy-looking query that already executed or materialized during construction. |
| Sent-query count by purpose | live SQL with audit enabled | Distinguishes one result statement, one optional selection proxy, one opt-in label scan, and dialect probe/control. It does not count engine scans. |
| SQL bytes and normalized node/operator counts | SQL backends and simulators | Detects faster-than-branch growth hidden by formatting. Count top-level `UNION ALL`, `GROUPING SETS` entries, `LEFT JOIN`, `CASE/WHEN`, and outer `ORDER BY`. |
| lazy graph depth and node count | dtplyr and dbplyr | Balanced union depth should be logarithmic; total union nodes/branches should be linear in `S`. Use backend representation only in scratch instrumentation, not as a proposed public contract. |
| live query plan scans/aggregates/unions | DuckDB and any other available live database | Separates one submitted statement from physical source scans. Simulator SQL is not live-plan evidence. |
| collision-check aggregate count and execution count | lazy label-check series | Batched query count must remain one; expression count may grow with eligible `D`. |
| share join, mapping-branch, and target-comparison counts | contextual-share series | Same-kind join count is invariant in `H`; Parent target discovery is the suspected `S^2 * D` planning term. |

For lazy backends, every recorded case has two phases: build/render and
caller-requested execution/collection. Table creation and `dplyr::tbl()`
metadata traffic is measured or cleared before the marginplyr call, because
[`query-cost-across-lazy-backends.md`](query-cost-across-lazy-backends.md)
established that `dplyr::tbl()` itself can send a zero-row query.

## Backend matrix

The candidate execution matrix is deliberately within-backend:

| Backend | Construction phase | Execution phase | Structural emphasis | Limit |
|---|---|---|---|---|
| local data frame | eager call | same call | allocations, source passes, `S * N`, Parent planning | No separate lazy phase. |
| dtplyr immutable step | build step | `collect()` | one zero-row proxy, union depth, no early materialization | Mutable roots are correctly refused and are not a scale target. |
| Arrow table/dataset/query | build query | `collect()` | schema-only proxy, no entry point during build, portable branch graph | Contextual shares are intentionally unsupported. Dataset storage locality must be stated. |
| live DuckDB | build/render native summary or portable expansion | `collect()` plus `EXPLAIN` used explicitly by the investigation | proxy count, native scan plan, portable branch scans, SQL bytes | DuckDB results do not stand for remote-network engines. |
| live RSQLite, if installed | build/render portable query | `collect()` plus SQLite query plan where meaningful | generic-SQL type anchor, union branches, share join | No native grouping sets. |
| dbplyr simulators | build/render only | none | dialect-specific SQL shape | Never classify simulator output as live execution, scan, or cost evidence. |

Unavailable network databases are recorded as untested. No simulator result is
promoted to a claim about a live PostgreSQL, warehouse, or service.

## Suspicion thresholds and classification

A result becomes a focused follow-up when any of these occurs:

- proxy acquisitions increase with `S`, `E`, or `H`;
- build-time execution entry points appear outside ADR 0020's enumerated
  exemptions or the documented row-wise nesting collection;
- portable union nodes, top-level branches, or SQL bytes grow faster than the
  repeated branch text explains;
- same-kind share `LEFT JOIN` count grows with `H`;
- opted-in label-check execution count grows with `D`;
- row count changes SQL/query counts or construction graph shape;
- a zero-row or metadata-only case grows with source `N`;
- local Parent-share work shows a source pass per set beyond the ordinary
  summaries, suggesting the removed rescan pattern returned;
- native SQL submits one statement but its live plan gains more source scans
  than the grouping-set strategy justifies;
- nested/eager allocation grows materially faster than output bytes plus a
  bounded construction overhead.

Every suspicious case is reduced to a smaller deterministic series and assigned
one of:

- `CONFIRMED SCALABILITY ISSUE`;
- `EXPECTED / JUSTIFIED GROWTH`;
- `BACKEND-OWNED COST`;
- `MEASUREMENT ARTIFACT`;
- `INCONCLUSIVE`.

A confirmed issue requires both a reproducible minimal series and an identified
implementation path. A slower observation without those is not an issue.

## Preliminary durable-protection map

The 2026-09-12 checkout already protects several performance invariants without
wall-clock thresholds:

1. one typed proxy per operation and zero Arrow proxy collection;
2. zero caller rows in the exempt proxy, through query-policy instrumentation;
3. one batched observed-label query rather than one query per dimension;
4. no build-time Arrow materialization and refusal of Absorbing summaries;
5. eager branch combination without pairwise `union_all()`;
6. bounded lazy union depth, exercised at 512 occurrences;
7. flat portable SQL union shape;
8. one share join per requested kind, with all same-kind outputs batched;
9. no staged-result read for SQL share-source validation;
10. answered dialect verdict caching and bounded table-free probe/control
    queries;
11. one outer Margin order rather than branch-local sorting;
12. a per-call Sent-query record that distinguishes the caller's result query
    from marginplyr's internal SQL.

If this investigation confirms a regression risk, protection should prefer the
same forms: call counts, branch/node counts, maximum graph depth, absence of an
execution entry point, SQL operator shape, or algorithmic comparison counts.
Only a slope that cannot be stated through a stable structural seam belongs in
a developer benchmark, and no elapsed-time threshold belongs in ordinary CRAN
tests.

## Measurements

### Environment and method

The reproducible harness is
[`performance-scalability-testing.R`](performance-scalability-testing.R). It
loaded the working tree with pkgload and ran under R 4.6.1 with dplyr 1.2.1,
dbplyr 2.6.0, dtplyr 1.3.3, RSQLite 3.53.3, DuckDB 1.5.5, and Arrow 25.0.1.
RSQLite used an in-memory database; DuckDB used an isolated in-memory
connection; Arrow cases used an in-memory `Table`.

Elapsed observations are warm-run medians of three to five executions. They
show direction only. `Rprofmem()` supplied total allocated bytes, not peak
resident memory. Each live lazy result was constructed and rendered first,
then collected once on purpose. Table creation, `dplyr::tbl()` discovery, and
the explicit `EXPLAIN` were outside construction timings. DuckDB scan counts
used `EXPLAIN (FORMAT JSON)` because its text tree elides wide unions after 16
branches. Arrow emitted sandbox-only `sysctlbyname` cache-size diagnostics;
the queries and results still completed, so those messages were classified as
a measurement-environment artifact rather than package behavior.

No network database was available. PostgreSQL, BigQuery, Snowflake, Spark,
Athena, and other service execution therefore remain untested; no simulator
result was treated as live execution evidence.

### Scaling results

The table reports endpoints or selected turning points instead of every raw
timing row. All values come from the harness above unless a row names the
additional focused instrumentation below.

| Changed dimension | Range and fixed variables | Metrics and observed growth | Expected growth | Classification |
|---|---|---|---|---|
| Source rows, local operations | `N = 1,000..64,000`; rollup `S = 4`, `D = 3` | Summary result stayed 15 rows; expand rows were exactly `4N` and result bytes 113 KB -> 7.17 MB. Nest and nest-by payload rows were exactly `4N`; allocations grew 0.45 MB -> 24.4 MB. | Summary aggregate work may follow `N`; expansion/nesting storage must follow `S*N`. | `EXPECTED / JUSTIFIED GROWTH` |
| Source rows, inspection | `N = 1..1,000,000`; cube `D = 4`, `S = 16` | Median remained 3.38-3.52 ms and returned 16 rows. | Independent of source rows after metadata discovery. | `EXPECTED / JUSTIFIED GROWTH` |
| Cube plan size | `D = 1..10`, `S = 2..1,024`; one-row local input | Inspection result grew 2.5 KB -> 1.82 MB, allocation 20 KB -> 663 KB, median 1.89 -> 26.1 ms. | Plan representation and formatting are `O(S*D)`. | `EXPECTED / JUSTIFIED GROWTH` |
| Local cube | `D = 1..8`, `S = 2..256`; `N = 256` | Summary rows were exactly `3^D` (3 -> 6,561) and allocation 61 KB -> 7.92 MB. Expansion rows were exactly `S*N` (512 -> 65,536) and allocation 41 KB -> 10.6 MB. | Branch count is `S`; summary output cardinality and expanded output width explain the increase. | `EXPECTED / JUSTIFIED GROWTH` |
| Duplicate occurrences | `S = 1..128`; fixed `D = 1`, `N = 256`, `.duplicates = "keep"` | Summary result rows 2 -> 256, allocation 52 KB -> 2.97 MB, median 6.4 -> 249 ms. Expansion rows 256 -> 32,768, allocation 30 KB -> 1.98 MB, median 2.7 -> 126 ms. | One branch/result occurrence per retained duplicate, with one eager combine pass. | `EXPECTED / JUSTIFIED GROWTH` |
| dtplyr cube expansion | `D = 1..9`, `S = 2..512`; `N = 256` | Union nodes were exactly `S-1` (1 -> 511); depth exactly `D` (1 -> 9); query object 16 KB -> 15.95 MB; result rows exactly `S*N`. | Nodes linear in `S`, depth `log2(S)`, representation/output approximately `O(S*D)` and `O(S*N)`. | `EXPECTED / JUSTIFIED GROWTH` |
| Arrow cube | `D = 1..6`, `S = 2..64`; `N = 256` | Summary query 18 KB -> 1.11 MB and plan text 580 -> 38,343 bytes; expand query 9 KB -> 718 KB. Build/collect stayed separated and construction called no `collect()`. | Portable branches grow with `S*D`; execution result grows with aggregate/output rows. | `EXPECTED / JUSTIFIED GROWTH` |
| RSQLite portable cube | `D = 1..6`, `S = 2..64`; `N = 256` | Summary SQL 438 -> 38,842 bytes, `UNION ALL` 2 -> 64, source references/scans 3 -> 65. Expansion SQL 161 -> 7,087 bytes with the same counts. | `S` result branches plus one zero-row type anchor; repeated branch text is `O(S*D)`. | `EXPECTED / JUSTIFIED GROWTH` |
| DuckDB native cube summary | `D = 1..6`, `S = 2..64`; `N = 256` | SQL 83 -> 991 bytes; one `GROUPING SETS`, zero unions, one source reference, one physical base scan throughout. | One native aggregate statement; grouping-list text follows `S*D`, source scan count need not follow `S`. | `EXPECTED / JUSTIFIED GROWTH` |
| DuckDB cube expansion | same series | SQL 91 -> 5,859 bytes; unions 1 -> 63; source references and JSON-plan base scans 2 -> 64; result rows 512 -> 16,384. | Expansion has no native adapter and must emit/read one source branch per set to return `S*N` rows. | `EXPECTED / JUSTIFIED GROWTH` |
| Fixed-key cardinality | local `K = 1..2,048`; fixed `N = 32,768`, one dimension, `S = 2` | Result rows grew 3 -> 4,096 and bytes 1.2 KB -> 198 KB. Plain median stayed 10-14 ms until the largest case; Total share reached 30 ms and one join. | Group/denominator output grows with `K`; branch and join counts remain fixed. | `EXPECTED / JUSTIFIED GROWTH` |
| Ordinary summaries and same-kind shares | DuckDB `E/H/U = 1,4,16,64`; fixed one-dimension rollup | Plain SQL 86 -> 1,972 bytes. With 64 shares, reused-source SQL was 37,340 bytes and 64-unique-source SQL 47,450 bytes. Every same-kind case had exactly one `LEFT JOIN`; build/render growth followed projected expressions and denominator columns. | Syntax/query object linear in expressions/sources; one mapping and join per kind. | `EXPECTED / JUSTIFIED GROWTH` |
| Requested Margin order | local rollup `D = 8..128`, one source/result value per occurrence | At `D = 128`, no-sort median was 1.177 s and `.sort = "last"` 1.500 s; result rows/bytes were identical. The sort added a bounded 27% in this endpoint series, not a second branch family. | One final order and `O(D)` emitted terms over an already `O(S*D)` result shape. | `EXPECTED / JUSTIFIED GROWTH` |
| Parent-target discovery | isolated `parent_set_ids()` on rollup `D = 16..256`, `S = 17..257` | Median 0.00042, 0.00107, 0.00403, 0.0177, 0.113 s. Doubling increasingly cost about 2.5x, 3.8x, 4.4x, then 6.4x. | A pure rollup's next distinct parent can be found without all-pairs subset tests; current implementation is approximately `O(S^2*D)`. | `CONFIRMED SCALABILITY ISSUE` |
| Parent lazy query shape | live DuckDB rollup `D = 4,8,16,32`; one summary/share, `N = 256` | Parent SQL 7.4, 25.5, 135, 911 KB; copies of `GROUPING SETS` and CTE scans 5, 9, 17, 33. Total-share controls were 2.5, 5.1, 15.1, 53.8 KB and stayed at two copies/scans. DuckDB base scan stayed one. | Parent mapping needs `O(S)` logical rows/branches, but not `S` textual copies of the whole `O(S*D)` staged aggregate. | `CONFIRMED SCALABILITY ISSUE` |

The width-only timing probe (`C = 4..256`, one referenced grouping column)
retained one proxy acquisition and a stable returned-summary size. Local
medians stayed between 6.7 and 9.7 ms; DuckDB construction was noisy
(12.7-44.5 ms with no monotone slope). The structural contract held, but the
timing series is `INCONCLUSIVE` and adds no issue. The explicit unique-set
series proposed as P3 was not run after duplicate occurrences, cube sets, and
rendered branch counts separately identified the same occurrence path; doing
so would add no independent cost mechanism.

### Metadata, execution, and materialization counts

Tracing `grouping_selection_proxy()` and the `dplyr::collect()` generic at
`D = 1` and `D = 8` produced the same counts at both sizes:

| Backend/operation | Proxy calls | Construction-time `collect()` calls | Interpretation |
|---|---:|---:|---|
| local inspect/summary/expand/nest | 1 | 0 | The local object is the proxy. |
| local nest-by | 1 | 1 | The explicit generic call is a local no-op used to produce the row-wise result. |
| dtplyr inspect/summary/expand/nest | 1 | 1 | Exactly the permitted zero-row typed proxy; result stays lazy. |
| dtplyr nest-by | 1 | 2 | Zero-row proxy plus the documented result collection. |
| RSQLite inspect/summary/expand | 1 | 0 | Generic SQL lacks the typed-proxy collection capability. |
| DuckDB inspect/summary/expand | 1 | 1 | Exactly the permitted zero-row typed proxy. |
| Arrow inspect/summary/expand | 1 | 0 | `arrow::schema()` supplies metadata without execution. |

With SQL auditing enabled, every lazy SQL case recorded one caller result.
RSQLite recorded no selection-proxy query and DuckDB exactly one. Enabling
`.check_margin_label = TRUE` at `D = 1` and `D = 8` recorded exactly one
`observed_label_collision` query on each database. Its SQL grew from 85 to 598
bytes on SQLite and 83 to 596 bytes on DuckDB because the one aggregate gained
dimension expressions; query count did not grow.

These counts establish absence of duplicate metadata acquisition and of an
unrequested full-result materialization in the exercised construction paths.
They do not claim that a zero-row SQL query is free, nor that an in-memory Arrow
Table represents a remote Dataset.

## Confirmed scalability issues

### 1. Parent-share SQL repeats the complete staged summary

- **Affected API/backend:** `summarize_with_margins()` with
  `share_of_parent()` on lazy dbplyr backends. The confirmed live backend is
  DuckDB's native `GROUPING SETS` path; the repetition is constructed in the
  shared dbplyr Parent mapping before backend execution.
- **Minimal scaling series:** rollup `D = 4, 8, 16, 32`, hence
  `S = 5, 9, 17, 33`, with `N = 256`, one numeric aggregate, one Parent share,
  no display label, and share-source probing disabled so it could not confound
  the result.
- **Observed behavior:** SQL grew 7,448 -> 25,548 -> 135,053 -> 911,061 bytes;
  render median grew 0.091 -> 0.205 -> 0.525 -> 1.712 seconds. The SQL held
  `S` copies of `GROUPING SETS`, `S - 1` mapping branches, and the JSON plan
  held `S` CTE scans. The corresponding Total share held two copies/scans at
  every size and rendered 2,512 -> 53,814 bytes.
- **Expected behavior:** Parent mapping legitimately produces one mapping row
  family per child occurrence, but that family can reference one staged
  relation. The complete staged aggregate does not semantically need to be
  rendered once per child.
- **Root cause:** [`build_lazy_parent_mapping()`](../R/share.R) filters the
  staged `result` independently for every `child_id`, then
  `combine_margin_branches()` unions those derived queries. dbplyr renders each
  branch by recursively inlining `result`; that result contains the native
  `GROUPING SETS` and its occurrence-id `CASE`, already `O(S*D)` text. The
  composition therefore approaches `O(S^2*D)` text, or roughly cubic growth
  for a rollup where `S = D + 1`.
- **Supporting evidence:** exact `GROUPING SETS`, `UNION ALL`, source-reference,
  and JSON-plan node counts in the focused DuckDB series. DuckDB's optimizer
  created `__common_subplan_1`, keeping the physical base scan at one; that
  confirms common computation only after the oversized statement has been
  built and parsed.
- **User impact:** deep rollups pay client allocation/rendering, statement
  transfer, parsing, and plan complexity before executing a small result. An
  optimizer without DuckDB's common-subplan factoring may also repeat the
  aggregate execution. The network-backend half is unmeasured, so that latter
  risk is not claimed as observed.
- **Severity:** **medium**. It is restricted to Parent shares on deep lazy
  rollups, but 16 dimensions already produced 135 KB and 32 produced 911 KB of
  SQL.

### 2. Parent occurrence discovery performs all-pairs subset checks

- **Affected API/backend:** `share_of_parent()` on every backend, before
  adapter selection.
- **Minimal scaling series:** pure rollup `D = 16, 32, 64, 128, 256`, hence
  `S = D + 1`; only `parent_set_ids(plan)` was timed after plan compilation to
  remove backend and query-construction costs.
- **Observed behavior:** median rose from 0.00042 seconds at 17 sets to 0.113
  seconds at 257 sets, with the multiplier increasing on later doublings.
- **Expected behavior:** rollup occurrences are ordered prefixes. The next
  strictly less detailed distinct occurrence, with duplicates skipped, can be
  derived in one forward pass plus bounded set comparison rather than testing
  every occurrence against every other occurrence.
- **Root cause:** [`parent_set_ids()`](../R/share.R) loops over every child,
  then `vapply()` loops over all occurrences and evaluates
  `length(parent) < length(child) && all(parent %in% child)`. This is
  approximately `O(S^2*D)` for the accepted plan shape.
- **Supporting evidence:** the isolated series above and direct source
  inspection; local public-call controls showed Parent shares increasingly
  separating from the corresponding Total-share path as dimensions grew.
- **User impact:** negligible at ordinary small rollups; visible only for very
  wide hierarchies, and smaller than the lazy SQL-expansion issue on the
  measured range.
- **Severity:** **low**.

## Existing design that worked

The following protections were not merely present in source; the controlled
series observed them holding:

1. The typed proxy was acquired exactly once for every exercised public
   operation at both one and eight dimensions. Arrow performed no collection;
   dtplyr and DuckDB performed one zero-row collection; SQLite performed none.
2. Lazy summary/expand/nest construction did not collect the caller's result.
   The only second dtplyr collection was `nest_by_with_margins()`'s documented
   conversion to a local row-wise frame.
3. Observed-label checking remained one query from one to eight dimensions,
   confirming that the former per-dimension scans remain batched.
4. The eager branch path showed no quadratic fold signature: 128 duplicate
   occurrences produced proportional rows/allocations and one list combine.
5. dtplyr's 512-branch cube had depth nine rather than 512 and collected
   successfully. Node count was exactly `S - 1`.
6. SQLite portable SQL was a flat branch family with exactly one union per
   retained set after adding the type anchor. No nested-union depth appeared.
7. DuckDB native summaries retained one `GROUPING SETS`, one source reference,
   and one physical base scan as cube size increased to 64 sets.
8. Same-kind shares from one or 64 outputs used one `LEFT JOIN`; increasing
   unique sources added denominator columns, not joins.
9. Total-share mapping remained one mapping branch and two references/scans of
   the staged relation as rollup dimensions increased. It did not reuse the
   Parent mapping's per-occurrence union.
10. `inspect_grouping()` remained source-row-count independent through one
    million rows, and expansion/nesting duplicated exactly the payload their
    public result promises rather than a larger intermediate count.

## Recommended durable protection

Priority is based on stability and the cost pattern, not on elapsed values.

1. **After repairing repeated Parent SQL, add a structural query-shape
   assertion.** A deep-rollup Parent query should render the staged aggregate
   once and reference it from `O(S)` mapping logic. Assert a constant count of
   native `GROUPING SETS` aggregate definitions, plus linear mapping-branch or
   reference count. Do not assert SQL bytes or render time. This is suitable
   for a durable regression test once the implementation has a named
   staged-relation/CTE seam.
2. **Keep a developer slope benchmark for Parent query construction/rendering.**
   Until a portable staged-relation representation is chosen, record
   dimensions, SQL bytes, aggregate-definition count, query-object bytes, and
   render time for `D = 4, 8, 16, 32`. This is more informative than a brittle
   ordinary test and covers the parser/allocator cost DuckDB's optimizer cannot
   erase.
3. **Treat `parent_set_ids()` as a low-priority algorithm benchmark, not a CRAN
   timing test.** If it is rewritten, introduce an explicit comparison counter
   in scratch verification and require linear/near-linear candidate visits.
   A unit test of private list-walking syntax would couple to the opaque plan
   representation and is not justified by the sub-millisecond ordinary range.
4. **Retain the existing structural protections unchanged.** Proxy acquisition,
   zero-row materialization, batched collision queries, balanced unions, flat
   SQL, and one share join per kind already have stable assertions. This run
   found no reason to duplicate them with wall-clock thresholds.
5. **Do not automate the source-width timing probe.** Its structural count was
   already covered and its elapsed signal was non-monotone. It is unsuitable
   as a durable benchmark in its present form.

## Final recommendation

Introduce a **narrow developer performance harness**, using this investigation
script as the prototype, rather than a general benchmark suite or an ordinary
test threshold. Its highest-value tracked series is Parent-share lazy query
construction: SQL/graph structural counts first, elapsed and allocation as
trend columns. A second small series can retain local `parent_set_ids()` slope.

There is not enough value in continuously timing every backend/API combination.
Most measured growth is dictated directly by result size (`S*N`), plan size
(`S*D`), or a backend engine, and existing structural tests protect the
marginplyr-owned invariants more reliably. The broad matrix should remain a
dated investigation rerun when adapters or grouping-plan representation
change.

No wall-clock `expect_lt()` belongs in CRAN checks or ordinary unit tests. If a
scheduled benchmark is later added, it should report trends without gating a
release and should pin package/backend versions so a dependency change is not
misreported as a marginplyr regression.

## Implementation follow-up (2026-09-12)

Issue #550 selected the report's first recommendation without changing the
investigation above: a SQL Parent mapping now carries the small child-to-Parent
occurrence plan as one non-materializing inline relation and joins the staged
summary to it once. The separate `parent_set_ids()` issue is tracked by #551.

Re-running the same live DuckDB rollup series on the #550 implementation gave:

| `D` | Before SQL bytes | After SQL bytes | After `GROUPING SETS` definitions | After CTE scans | After base scans |
|---:|---:|---:|---:|---:|---:|
| 4 | 7,448 | 4,949 | 2 | 2 | 1 |
| 8 | 25,548 | 9,669 | 2 | 2 | 1 |
| 16 | 135,053 | 24,791 | 2 | 2 | 1 |
| 32 | 911,061 | 76,775 | 2 | 2 | 1 |

The Parent query still has one final `LEFT JOIN`; the inline plan adds one
`INNER JOIN`. The after-series render medians were 0.061, 0.088, 0.144, and
0.285 seconds, versus 0.091, 0.205, 0.525, and 1.712 seconds before the change.
Those timings remain directional evidence only. The durable result is the
constant staged-summary definition/reference count, not the elapsed ratio.
