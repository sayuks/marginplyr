# Backend API improvement implementation plan

Investigated: 2026-07-28
Status: implemented
Completed: 2026-07-28
Primary input: [`backend-api-audit.md`](backend-api-audit.md)

## Implementation outcome

The phases were implemented in order:

- `deca1ca` — fix internal name reservation;
- `8c73bd0` — migrate SQL construction to `sql_glue2()`;
- `97832dc` — reuse typed backend metadata proxies;
- `8018e5c` — batch margin-label collision checks;
- `d344f97` — wrap native grouping sets in a dbplyr lazy-query node;
- `e9a4d4f` — replace private S3 helpers with backend-neutral dplyr verbs;
- `f9004df` — align backend and call inspection with public APIs.

Verification completed:

- full `testthat::test_local()` suite;
- source-package build with vignettes;
- local, dtplyr, Arrow, simulated dbplyr, and live DuckDB tests;
- `R CMD check --as-cran --no-manual`: one expected `New submission`
  NOTE and no ERROR or WARNING.

The PDF manual check could not run locally because `pdflatex` is not
installed. All Rd, examples, vignettes, HTML, namespace, dependency, and code
checks completed successfully.

## Objective

Apply every recommended improvement from the backend API audit while
preserving marginplyr's exported interface and documented behaviour.

The implementation must:

- fix the internal-name collision bugs before doing structural cleanup;
- replace superseded dbplyr SQL construction APIs;
- reduce avoidable backend queries and Arrow query execution;
- isolate the native `GROUPING SETS` extension at a documented dbplyr seam;
- remove private backend-specific S3 methods that current public dplyr APIs
  make unnecessary;
- retain the custom summary-selection logic that marginplyr genuinely needs;
- leave all exported function names and arguments unchanged.

This plan is intentionally explicit so that a subsequent GPT-5.6 Terra run
can implement it without reopening the design decisions.

## Fixed design decisions

1. **Do not change the public interface.** All exported functions, arguments,
   result column order, grouping structure, errors, and lazy/eager behaviour
   remain compatible unless this plan explicitly fixes a bug.
2. **The grouping compiler owns the output namespace.** It computes input,
   summary-output, and internal reserved names once. Backend adapters receive
   the resulting reserved-name vector instead of rediscovering columns with
   `colnames()`.
3. **The typed selection proxy is retained.** It is required for
   `where()` and similar predicates. Arrow constructs it from schema metadata;
   dtplyr and DuckDB continue to use a zero-row collection.
4. **A typed proxy is constructed at most once per top-level operation.**
   Margin column prototypes and factor metadata reuse it.
5. **Native SQL remains a dbplyr adapter.** Use `lazy_query()`, `op_vars()`,
   and `sql_build()` as the seam. Keep all assumptions about dbplyr query
   objects inside `R/grouping_sets.R`.
6. **Use feature-based minimum versions.** Require `arrow (>= 13.0.0)` and
   `dtplyr (>= 1.3.2)`. Do not raise the dtplyr minimum to 1.3.3 unless a test
   demonstrates that 1.3.2 cannot satisfy the interface.
7. **Do not remove the summary-selection AST rewrite or dtplyr `.names`
   normalization.** Both are verified compatibility logic.
8. **Do not add timing assertions.** Performance tests are directional and
   unsuitable for CRAN. Verify query shape and backend results instead.

## Audit coverage

| Audit finding | Implementation phase |
|---|---|
| 1. Internal-name reservation | Phase 1 |
| 2. Superseded SQL construction | Phase 2 |
| 3. Direct lazy-query mutation | Phase 5 |
| 4. Per-dimension label queries | Phase 4 |
| 5. Arrow schema metadata | Phase 3 |
| 6. Duplicate typed-proxy collection | Phase 3 |
| 7. Private S3 helper families | Phase 6 |
| 8. Conditional nesting collection | Phase 7 |
| 9. Summary-selection compatibility | Retained and regression-tested in phases 1 and 6 |
| 10. Manual call inspection | Phase 7 |
| 11. Concrete `.drop` class guard | Phase 7 |
| 12. Unexported Arrow superclass | Phases 6 and 7 |

## Intended internal module shape

### Grouping-plan module

Interface:

- compiled grouping plan;
- resolved summary expressions;
- one vector of reserved output names.

Implementation responsibilities:

- tidyselect evaluation;
- known summary-output discovery;
- grouping-column overwrite validation;
- reservation of input, summary, and internal names.

This prevents union and native adapters from independently guessing the
column namespace.

### Backend metadata adapter

Interface:

- `grouping_selection_proxy(.data, backend)`;
- `margin_column_info(data_proxy, dimensions, backend)`.

Implementation responsibilities:

- local/dtplyr/DuckDB zero-row prototype handling;
- Arrow schema conversion;
- factor/prototype extraction;
- no duplicate schema query.

### Native dbplyr adapter

Interface:

- `summarize_grouping_sets()` returns a normal ungrouped `tbl_lazy`;
- `op_vars.lazy_grouping_sets_query()` exposes output variables;
- `sql_build.lazy_grouping_sets_query()` renders native grouping sets.

Implementation responsibilities:

- native grouping flags;
- custom lazy-query node;
- SQL construction and quoting;
- no query-object manipulation outside this file.

### Result-finishing module

Interface:

- keys-first column order;
- optional sorting;
- factor reconstruction.

Implementation responsibilities:

- use ordinary dplyr verbs that work across supported backends;
- no marginplyr-owned S3 dispatch.

## Implementation phases

Each phase should be committed separately after its targeted tests pass.
Do not combine phase 5, the higher-risk dbplyr query refactor, with unrelated
cleanup.

### Phase 0 — Establish the baseline

Actions:

1. Read this plan and the audit before editing.
2. Inspect `git status` and preserve the two investigation documents and any
   unrelated user changes.
3. Run the existing test suite and record the baseline.
4. Run focused live tests when Arrow, dtplyr, and DuckDB are installed.

Commands:

```sh
Rscript -e 'testthat::test_local()'
R CMD build .
R CMD check --as-cran marginplyr_*.tar.gz
```

Acceptance:

- baseline failures, if any, are recorded before implementation;
- no generated documentation or unrelated files are silently overwritten.

### Phase 1 — Fix internal-name reservation

Priority: P0
Risk: low
Target files:

- `R/summarize_with_margins.R`
- `R/grouping-adapter-union.R`
- `R/grouping_sets.R`
- `R/internal-names.R`
- `R/summary-selections.R`
- `tests/testthat/test-grouping-backends.R`
- `tests/testthat/test-grouping-interface.R`

Implementation:

1. In `summarize_with_margins()`, store the known summary names once:

   - explicitly named dots;
   - `known_summary_output_names()`.

2. Continue using that vector for `check_summary_group_overwrite()`.
3. Construct one `reserved_names` vector from:

   - `data_vars`;
   - explicitly named summary outputs;
   - known data-frame/across outputs.

4. Pass `reserved_names` into both `summarize_margin_union()` and
   `summarize_grouping_sets()`.
5. In the union adapter:

   - generate key names with `new_margin_internal_names()`;
   - stop using `colnames(.data)`;
   - use `get_col_names(result, dplyr::everything())` for the final runtime
     grouping-column overwrite check.

6. In the native adapter:

   - remove `make_grouping_flag_names()`;
   - use `new_margin_internal_names()` with the grouping-flag prefix;
   - stop using `colnames(.data)`.

7. In `summary_all_of_expr()`, obtain source variables through
   `get_col_names()` rather than `colnames(data_proxy)`.
8. Keep the runtime overwrite check even when preflight names are known. It
   protects against summary expressions whose output names cannot be inferred
   statically.

Required regression tests:

- an input column named `..marginplyr_key_1` on local, dtplyr, and Arrow;
- an `across(..., .names = "..marginplyr_key_1")` summary on every union
  backend;
- an `across(..., .names = "..marginplyr_grouping_1")` summary on simulated
  PostgreSQL and live DuckDB;
- collisions with an internal name that already has appended underscores;
- a summary attempting to overwrite a visible grouping column on dtplyr and
  Arrow;
- one and multiple `across()` functions, with named and unnamed function
  lists;
- `.names` using `{.col}` and `{.fn}`, named tidyselect inputs, and
  `.unpack`;
- non-syntactic input and summary names.

Acceptance:

- all collisions either receive a unique internal name or the existing clear
  grouping-overwrite error;
- no adapter uses `colnames()` to inspect a lazy result;
- local, dtplyr, Arrow, and native SQL return unchanged user-visible names.

Suggested commit:

```text
Fix internal name reservation across backends
```

### Phase 2 — Move SQL construction to `sql_glue2()`

Priority: P1
Risk: low to medium
Target files:

- `R/factor.R`
- `R/grouping-context.R`
- `R/grouping_sets.R`
- `tests/testthat/test-factor.R`
- `tests/testthat/test-grouping-backends.R`

Implementation:

1. Replace `sql_call2()` in `grouping_sql_expr()` with
   `dbplyr::sql_glue2(con, "GROUPING({.id var})")`.
2. Build weighted `grouping_id()` terms with `.sql` fragments and escaped
   numeric values through `sql_glue2()`.
3. Replace the DuckDB ENUM `CAST` builder with identifier and value-vector
   interpolation through `sql_glue2()`.
4. Render each grouping set with `{.id vars*}` so the empty set becomes `()`.
5. Combine already escaped set fragments into one string, then inject that
   single string through `{.sql ...}` for the outer `GROUPING SETS (...)`.
6. Remove uses of `build_sql()`, `sql_call2()`, `ident()`, and manual
   `sql_vector()` where the new glue expression fully replaces them.
7. Keep `sql_build()` itself; it is the intended custom-query generic.

Required tests:

- exact SQL semantics on PostgreSQL, MySQL, MSSQL, SQLite, and other existing
  simulators;
- empty grouping set;
- multiple and duplicate grouping sets;
- non-syntactic identifiers;
- embedded apostrophes in the margin label and factor levels;
- factors with an explicit `NA` level;
- `grouping_bit()` and `grouping_id()` weights and column ordering;
- live DuckDB collection, not only SQL rendering.

Acceptance:

- no superseded SQL builder remains in `R/`;
- existing dialects generate equivalent quoted SQL;
- live DuckDB results and factor restoration are unchanged.

Suggested commit:

```text
Use sql_glue2 for backend SQL construction
```

### Phase 3 — Build and reuse backend metadata efficiently

Priority: P1
Risk: low to medium
Target files:

- `R/grouping-plan.R`
- `R/margin-label.R`
- `R/summarize_with_margins.R`
- `R/expand_with_margins.R`
- `R/nest_with_margins.R`
- `tests/testthat/test-grouping-backends.R`
- `tests/testthat/test-grouping-plan.R`

Implementation:

1. In `grouping_selection_proxy()`:

   - for Arrow backends, return
     `as.data.frame(arrow::schema(.data))`;
   - for dtplyr and DuckDB, retain
     `collect(head(.data, 0L))`;
   - otherwise retain the current object.

2. Change the internal `margin_column_info()` interface to receive the
   already-created `data_proxy` rather than recollecting from `.data`.
3. Update every top-level caller to pass its existing proxy.
4. Preserve the current behaviour when `backend$can_read_schema` is false.
5. Keep `can_restore_factors = FALSE` for Arrow.
6. Do not replace this logic with `dplyr::tbl_ptype()` or `compute()`.

Required tests:

- Arrow `Table`, `RecordBatch`, `Dataset`, and `arrow_dplyr_query`;
- projected and computed Arrow query columns;
- Arrow dictionary, factor, and ordered-factor inputs;
- `.margin_label = NULL` retains detail values and correct typed missing
  margins;
- `where(is.numeric)` and `where(is.character)` still work on dtplyr, Arrow,
  and DuckDB;
- `margin_column_info()` uses a supplied sentinel proxy without accessing the
  source data again;
- local and dtplyr factor reconstruction remains unchanged.

Acceptance:

- an Arrow grouping operation does not execute a zero-row query merely to
  discover schema;
- a top-level operation constructs at most one typed proxy;
- all existing tidyselect predicates and factor semantics remain supported.

Suggested commit:

```text
Reuse typed backend metadata proxies
```

### Phase 4 — Batch margin-label collision checks

Priority: P1
Risk: medium
Target files:

- `R/summarize_with_margins.R`
- `tests/testthat/test-grouping-backends.R`
- `tests/testthat/test-grouping-interface.R`

Implementation:

1. Replace the per-column filter/summarise/collect loop in
   `assert_margin_name()` with:

   - one injected-symbol expression per dimension;
   - one `summarise(!!!checks)`;
   - one `collect()`.

2. Use `rlang::sym()` injection in the aggregate. Do not use
   `.data[[col]]` inside Arrow aggregation.
3. Generate separate expressions for:

   - `any(is.na(column), na.rm = TRUE)` when the margin label is missing;
   - `any(column == margin_name, na.rm = TRUE)` otherwise.

4. Normalize aggregate outputs with `isTRUE()` so empty or missing backend
   results cannot propagate `NA` into error handling.
5. Keep the existing user-facing error text and column ordering.

Required tests:

- zero dimensions and empty data;
- all-missing and partially missing columns;
- `NA_character_` margin label;
- factor grouping columns;
- non-syntactic column names;
- simultaneous collisions in multiple columns;
- local, dtplyr, Arrow, live DuckDB;
- simulated PostgreSQL translation of the aggregate expressions.

Acceptance:

- one call to `summarise()` and one call to `collect()` checks all dimensions;
- errors identify exactly the colliding columns;
- results match the old implementation on all existing cases.

Do not add elapsed-time expectations to the package tests.

Suggested commit:

```text
Batch margin label collision checks
```

### Phase 5 — Introduce a proper custom dbplyr lazy-query node

Priority: P1
Risk: medium to high
Target files:

- `R/grouping_sets.R`
- `NAMESPACE` through roxygen2
- `tests/testthat/test-grouping-backends.R`

Implementation:

1. Replace direct field/class mutation of the existing select query with:

   ```r
   result$lazy_query <- dbplyr::lazy_query(
     "grouping_sets",
     x = result$lazy_query,
     grouping_sets = plan$sets,
     group_vars = character()
   )
   ```

2. Add:

   ```r
   op_vars.lazy_grouping_sets_query <- function(op) {
     dbplyr::op_vars(op$x)
   }
   ```

   Register it as the S3 method for the exported `dbplyr::op_vars()` generic.

3. Rewrite `sql_build.lazy_grouping_sets_query()` to:

   - build `op$x`;
   - render `op$grouping_sets`;
   - replace only the built select query's `group_by`;
   - avoid removing classes or temporary fields from `op`.

4. Keep this method and all remaining query-representation assumptions in
   `R/grouping_sets.R`.
5. When `.margin_label = NULL`, do not create internal `GROUPING()` flag
   columns. User-written `grouping_bit()` and `grouping_id()` expressions
   remain independently rewritten.
6. Ensure the result is ungrouped through the custom node's `group_vars`.

Required tests:

- `tbl_vars()` and `group_vars()` immediately after summarisation;
- downstream `select()`, `mutate()`, `filter()`, `arrange()`, and
  `summarise()`;
- SQL subquery generation after every downstream verb;
- empty and duplicate grouping sets;
- `.margin_label = NULL` produces no hidden grouping-flag SQL;
- user `grouping_bit()` and `grouping_id()` still render with a null label;
- simulated PostgreSQL and live DuckDB collection;
- current SQL fallback behaviour for unsupported dialects is unchanged.

Acceptance:

- no code prepends a class to an existing dbplyr lazy query;
- the custom node exposes correct variables and no persistent grouping;
- downstream verbs work without inspecting marginplyr internals;
- native SQL output and collected data match pre-refactor behaviour.

If this phase fails in a way that requires dbplyr internals outside
`R/grouping_sets.R`, stop and document the failure rather than spreading the
dependency across the package.

Suggested commit:

```text
Wrap grouping sets in a dbplyr lazy query
```

### Phase 6 — Remove obsolete backend-specific S3 shims

Priority: P2
Risk: medium
Target files:

- `DESCRIPTION`
- `R/summarize_with_margins.R`
- `R/margin-label.R`
- `NAMESPACE` through roxygen2
- `tests/testthat/test-grouping-backends.R`

Implementation:

1. Set:

   ```text
   arrow (>= 13.0.0)
   dtplyr (>= 1.3.2)
   ```

   in `Suggests`.

2. Replace `summarize_impl` and its methods with one ordinary helper using:

   ```r
   dplyr::summarise(.data, ..., .by = dplyr::all_of(.by))
   ```

3. Remove the unused `.margin_pairs` argument.
4. Replace `relocate_before_union_all()` and `relocate_post_proc()` methods
   with one backend-neutral keys-first helper based on:

   ```r
   dplyr::select(.data, dplyr::all_of(cols), dplyr::everything())
   ```

5. Replace `arrange_impl()` methods by resolving sort columns to a character
   vector and splicing `rlang::syms()` into `dplyr::arrange()`.
6. Remove all obsolete S3 registrations and roxygen tags.
7. Do not remove dtplyr summary-name normalization.

Required tests:

- local, dbplyr fallback, dtplyr, Arrow, and DuckDB output column order;
- `.sort = TRUE` and `.sort = FALSE`;
- grouping columns with non-syntactic names;
- Arrow union followed by column ordering and sorting;
- `.by` summaries on Arrow;
- existing dtplyr `.names` case still returns `n_value`, not `value`.

Acceptance:

- `summarize_impl`, `relocate_before_union_all`,
  `relocate_post_proc`, and `arrange_impl` S3 registrations are gone;
- ordinary dplyr verbs produce identical result order and values on all
  supported backends;
- DESCRIPTION minimums reflect the features actually required.

Suggested commit:

```text
Use backend-neutral dplyr result helpers
```

### Phase 7 — Apply low-risk idiomatic cleanups

Priority: P3
Risk: low
Target files:

- `R/nest_by_with_margins.R`
- `R/grouped-input.R`
- `R/grouping-plan.R`
- `R/grouping-context.R`
- `R/grouping-backend.R`
- relevant tests

Implementation:

1. Call `dplyr::collect(result)` unconditionally before
   `nest_by_with_margins()` applies `rowwise()`. The data-frame method is a
   no-op.
2. Replace the concrete `grouped_df` guard around `.drop = FALSE` with the
   public `group_by_drop_default()` generic alone. Retain the explicit
   `rowwise_df` rejection.
3. Replace manual `::`/`:::` AST indexing with `rlang::call_name()` and
   `rlang::call_ns()` where equivalent.
4. Replace the unexported `ArrowTabular` superclass in backend detection with
   public leaf classes:

   - `Table`;
   - `RecordBatch`;
   - `Dataset`;
   - `arrow_dplyr_query`.

5. Continue rejecting `RecordBatchReader` unless it is explicitly supported
   and tested.
6. Remove dead helpers revealed by these changes; do not add pass-through
   wrappers.

Required tests:

- local and dtplyr `nest_by_with_margins()` rowwise structure;
- empty nesting result;
- grouped and ungrouped local input with both `.drop` settings;
- grouped dbplyr, dtplyr, and Arrow input;
- qualified and unqualified grouping helper calls;
- every supported public Arrow leaf class and rejected
  `RecordBatchReader`.

Acceptance:

- concrete class checks remain only where no public capability generic
  exists;
- the cleanups do not alter errors, rowwise structure, or grouping output.

Suggested commit:

```text
Align backend checks with public APIs
```

### Phase 8 — Regenerate, verify, and review

Actions:

1. Regenerate `NAMESPACE` and documentation with the repository's normal
   roxygen2 workflow.
2. Confirm no exported function or help topic changed unintentionally.
3. Run:

   ```sh
   Rscript -e 'testthat::test_local()'
   R CMD build .
   R CMD check --as-cran marginplyr_*.tar.gz
   ```

4. Run focused live backend checks for:

   - dtplyr;
   - Arrow Table, Dataset, and computed query;
   - DuckDB native grouping sets;
   - dbplyr simulated PostgreSQL native SQL;
   - at least one simulated fallback SQL dialect.

5. Run `git diff --check`.
6. Review generated SQL before and after the refactor for semantic
   equivalence, allowing only deliberate formatting differences.
7. Review the final diff for:

   - accidental public-interface changes;
   - stale S3 registrations;
   - superseded SQL APIs;
   - remaining `colnames()` use on lazy objects;
   - duplicate zero-row collections;
   - new backend-specific branching outside the adapters.

Final acceptance criteria:

- all tests pass;
- `R CMD check --as-cran` reports no ERROR, WARNING, or NOTE attributable to
  these changes;
- local, dtplyr, Arrow, dbplyr fallback, PostgreSQL simulation, and live
  DuckDB agree on values, names, ordering, and grouping structure;
- native and fallback strategies both handle internal-name collisions;
- no exported interface changes;
- only justified backend seams remain.

Suggested final commit, only if phase commits require a verification update:

```text
Complete backend API modernization
```

## Explicitly out of scope

- renaming the package;
- changing `.by`, `.grouping`, `.margin_label`, `.duplicates`, or `.sort`;
- adding new exported helpers;
- removing `tidyselect::eval_select()`;
- replacing the summary-expression AST rewrite;
- forcing every SQL backend to use native grouping sets;
- adding performance benchmarks to CRAN tests;
- changing documentation examples unrelated to minimum supported versions.

## Handoff instructions for the next implementation run

1. Implement phases in order.
2. Add regression tests before each bug fix or structural refactor.
3. Run targeted tests after every phase and commit only that phase.
4. Stop after a failing phase long enough to diagnose the invariant; do not
   compensate by adding another backend-specific workaround elsewhere.
5. Preserve the investigation documents.
6. Do not push until the complete check matrix passes and the user explicitly
   requests a push.
