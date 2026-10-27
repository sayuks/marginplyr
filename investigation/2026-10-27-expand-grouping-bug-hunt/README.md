# Expansion and Grouping specification bug hunt

Investigated: 2026-10-27
Baseline: `13dd7c30ac38fe3fc82e74c561d8795a6c4b6d17` (local `main`)

This investigation examined `expand_with_margins()`, the five Grouping
specification constructors, `inspect_grouping()`, and the Grouping identity
helpers. Two independent defects were reproduced and minimized. No package
implementation, package tests, generated documentation, or installed files
were changed. Scratch probes and in-memory query inspection supplied the
feedback loops; repair and regression-test implementation were outside the
requested scope.

## Environment and replay

The measurements used macOS arm64, R 4.6.1, marginplyr 0.1.0 loaded from the
checkout, dplyr 1.2.1, dbplyr 2.6.0, rlang 1.3.0, tidyselect 1.2.1, vctrs
0.7.3, tibble 3.3.1, dtplyr 1.3.3, data.table 1.18.6.1, DBI 1.3.0,
RSQLite/SQLite 3.53.3, DuckDB 1.5.5, and Arrow 25.0.1. SQLite and DuckDB
were live in-process databases. No live PostgreSQL server or external
warehouse was exercised.

From the repository root, with the listed replay dependencies installed:

```sh
Rscript investigation/2026-10-27-expand-grouping-bug-hunt/reproduce.R
```

The [replay](reproduce.R) exercised the actual public APIs. At the baseline,
it deliberately exited with status 1: 26 assertion groups passed and 25
failed. Five failed groups covered F1, and twenty covered the 240 F2 matrix
cases. The [captured output](reproduce-output.txt) preserved the diagnostics
and counts. The replay accepted an actionable `marginplyr_error` mentioning
raw and missing labels as a safe F2 outcome; it rejected the observed silent
substitution of ordinary values. It was investigation evidence rather than a
test added to the installed package.

The maintained contracts read for this investigation included
[`CONTEXT.md`](../../CONTEXT.md), the public expansion, Grouping specification,
inspection, and Grouping identity references, and ADRs
[0008](../../design/adr/0008-centralize-grouping-specification-kind-rules.md),
[0009](../../design/adr/0009-distinguish-grouping-set-identifiers-from-grouping-identifiers.md),
[0012](../../design/adr/0012-distinguish-factor-na-levels-from-missing-margin-values.md),
[0015](../../design/adr/0015-separate-package-conditions-from-internal-invariants.md),
[0016](../../design/adr/0016-delegate-result-class-and-attributes-to-dplyr.md),
[0018](../../design/adr/0018-order-margin-results-by-grouping-structure.md),
[0020](../../design/adr/0020-ask-before-reading-a-lazy-input.md),
[0026](../../design/adr/0026-refuse-a-nested-name-two-readings-claim.md), and
[0031](../../design/adr/0031-preserve-sqlite-typed-dimensions-under-margin-order.md).
Those artifacts owned the decisions; this note recorded the measurements.

## F1: SQLite Boolean-word source columns and `.env` did not compose

Priority: P2. The two-column input below was sufficient; a Grouping
specification, labels, ordering, and an identifier were unnecessary.

```r
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
source <- dplyr::copy_to(
  con, tibble::tibble(`TRUE` = "a", .env = 1L), "source"
)
expand_with_margins(source)
```

The default one-set plan should have copied its one source row unchanged.
Instead, construction raised:

```text
Cannot translate a <rlang_ctxt_pronoun> object to SQL.
```

Direct source collection and `inspect_grouping(source)` succeeded. The same
failure reproduced with `FALSE`, `TrUe`, and `FaLsE`; replacing the
Boolean-word name with `NULL`, `NA`, `F`, or `T`, or replacing `.env` with
`.data`, allowed expansion construction and collection. Rollup cases
reproduced with missing and string labels and all three `.sort` modes. A
source `.env` with `.id = "TRUE"`, and a source `TRUE` with `.id = ".env"`,
both collected and materialized successfully: both conflicting names needed
to be source columns for this failure.

Three hypotheses distinguished the boundary: interference between SQLite's
Boolean-name protection and `.env` quoting; a general `.env` payload problem;
and a Boolean-word column problem alone. The successful one-name controls
ruled out the latter two. In
[`sql_margin_input_relation()`](../../R/grouping-adapter-union.R), the
Boolean-name protection introduced a `rename()` projection retaining a bare
`.env` expression. Its immediate `sql_render()` interpreted that expression
as a lexical pronoun.
[`expand_margin_union()`](../../R/grouping-adapter-union.R) prepared that
relation before
[`finalize_margin_operation()`](../../R/margin-operation.R) reached its
generated `.env` quoting. Inspection of the renamed projection, followed by
an in-memory call to the existing `sql_env_output_query()`, made the render
succeed. No package source was edited for that probe. An equivalent raw SQL
union also returned the expected source and margin rows.

The same relation-preparation boundary affected an additional public caller:
`summarize_with_margins(head(source, 1L), n = dplyr::n())` failed with the
same diagnostic, while the unlimited summary returned `n = 1L`. These were
one repair family rather than two tickets.

ADR 0031's *SQL expansion payload amendment* covered generated `.env`
references, and its *Boolean-word public names amendment* covered private
intermediate aliases. Completed Issues
[#730](https://github.com/sayuks/marginplyr/issues/730) and
[#826](https://github.com/sayuks/marginplyr/issues/826), including their
comments, were read. The newly measured combination failed after those
repairs; no matching open repair ticket was found.

## F2: raw-containing omitted dimensions received ordinary `00` values

Priority: P2. A single raw dimension and a two-set rollup reproduced the
defect. Including a genuine source `00` made the false replacement visible:

```r
data <- tibble::tibble(g = as.raw(c(0L, 1L)), p = 1:2)
result <- expand_with_margins(
  data, .grouping = rollup(g), .margin_label = NULL, .id = "sid"
)
```

The result's `(g, sid, p)` rows were `(00, 1, 1)`, `(01, 1, 2)`,
`(00, 2, 1)`, and `(00, 2, 2)`. All four `is.na(g)` values were `FALSE`.
The two omitted-dimension rows therefore held valid raw values rather than
the typed missing values promised by *Display labels and grouping identity*
in the inherited
[`summarize_with_margins()` reference](../../R/summarize_with_margins.R).
Their identifiers remained correct; the failure was the displayed dimension
value, not its Grouping set identifier.

The shared-verb matrix reproduced the same result in all 240 cases:

- Four local shapes: scalar raw, raw matrix, raw 3D array, and a packed tibble
  containing raw and integer components; plus scalar raw in a `data.table`.
- Four verbs: expansion, summary, nesting, and row-wise nesting.
- Two missing-label spellings, both `.check_margin_label` values, and all
  three `.sort` values.

For matrix and array keys, the omitted raw components were zero-filled.
For packed keys, the integer component became missing but the raw component
was `00`, leaving the omitted key non-missing under
`vctrs::vec_detect_missing()`. Summaries returned a Grand total under raw
`00`, and nesting returned its complete payload under that same valid value.
An empty raw-input summary also returned a Grand total with `g = 00` and
`total = 0`; empty expansion and nesting returned zero rows.

In [`margin_column_info()`](../../R/backend-metadata.R), scalar prototypes
were made with `x[NA_integer_]`, and packed/array prototypes with
`vctrs::vec_init(x, 1L)`. Both initialized raw components to `00`: raw had
no missing-value representation. The
[`Margin label validation`](../../R/margin-label.R) established no
representability condition before those prototypes were used. Disabling
observed collision checks did not change the result, nor should a normal
source `00` have made an impossible missing value representable.

Sixteen controls across the four verbs succeeded with raw as a fixed key,
raw outside the omitted dimensions, an included-only raw grouping set, or a
non-missing string label for a scalar raw dimension. Those uses did not
require raw missing values. The recommended repair boundary was an
actionable refusal when an actually omitted dimension could not represent
its requested missing label, retaining those supported cases. A scalar raw
dimension could instead use a non-missing display label; a structured raw
key would need conversion or unpacking.

The publication-time tracker recheck found an independently filed matching
repair, [#923](https://github.com/sayuks/marginplyr/issues/923), against the
same baseline. Its complete body, comments, and labels were read. No duplicate
ticket was created; the raw 3D-array and 240-case shared-verb evidence belonged
with that repair. Completed [#605](https://github.com/sayuks/marginplyr/issues/605) and
[#631](https://github.com/sayuks/marginplyr/issues/631) concerned packed and
matrix key support, rather than raw missing-value representability.

## Exploration coverage

These counts described exploratory probes run on the baseline, not a claim
that all possible calls or backends were verified. The retained replay above
preserved the findings and selected controls; this table summarized the
broader scratch exploration.

| Area | Executed probes and observations |
| --- | --- |
| Constructor algebra | Seed `20261027`, 1,500 generated ASTs across all five kinds; independent expected ordered grouping-set families and Grouping bits, including nested unions/products, composite dimensions, empty sets, repeated columns, and duplicate dropping; no mismatch. |
| Typed inspection | 16 selectors compared exactly on local input versus dtplyr, Arrow, and DuckDB: 48 comparisons matched. Name/type predicates, set arithmetic, positions, `all_of()`, `any_of()`, and `names(peek_data())` were included. |
| Reference lifecycle | Five constructor print/invisible-return checks, shared-reference depths 0 through 7 with 255 expected leaf-binding reads, and a 30-binding chain; no extra read or false cycle. |
| Local expansion types | 423 initial combinations of empty/populated inputs, labels, ordering, and character, numeric, logical, factor, NA-level factor, list, temporal, complex, raw, matrix, array, and packed keys. Row multiplicity, detail values, and payload copies matched; the initial checks did not assert raw missingness, which F2's subsequent probe exposed. |
| Local names and groups | 1,406 special-name pairs, 400 seeded multi-set/name/label cases, and 240 grouped-input versus equivalent `.by` comparisons; no additional mismatch. Source `...` and `..1` names were separated from otherwise valid generated identifier names. |
| Grouping identity | 840 valid-name/backend/input/expression combinations checked helper values against the row's `.id`: local, dtplyr, Arrow, SQLite, and DuckDB; empty/populated input; direct, parenthesized, block, arithmetic, bit, and `across()` forms. Another 120 cases with source `...`/`..1` names reached tidyselect's reserved-name refusal. |
| Backend expansion | 480 differential constructions/collections and 976 special-name constructions/collections across SQLite, DuckDB, dtplyr, and Arrow; no additional confirmed defect. |
| Materialization | 132 SQLite/DuckDB cases: 12 construction failures belonged to F1; the remaining 120 direct collect/compute cases passed. |
| Derived dtplyr metadata | 19 derived forms and four typed predicates: 36 inspections matched and 40 were refused under the documented metadata boundary. |
| Arrow physical types | 40 expansion checks spanning dictionary/factor, temporal, integer64, list/struct, binary, and non-finite numeric payloads; another 24 fixed-size-list/map/dictionary/struct payload or fixed-key expansions and 20 typed inspections; no mismatch against source collection. |

Existing tests also passed:

```sh
Rscript -e 'testthat::test_local(".", filter="^(expand-operation|grouping-plan|inspect-grouping|grouping-interface|margin-id|contextual-helpers|grouping-backends)$", reporter="summary")'
```

The cyclic-grouping and injected-nested-grouping test files passed separately.
The focused suite did not detect either new finding.

## Excluded observations and limits

- A `.data` payload's direct `compute()` failed on the ordinary dbplyr result
  path, but plain source `compute()` failed identically. The dedicated
  SQLite result paths activated by identifiers, ordering, non-missing labels,
  or Boolean-word outputs handled it. This inherited limitation was not
  promoted to another finding.
- dtplyr collection over raw branch unions raised an unsupported-type error;
  ordinary `union_all()` over raw inputs did too. F2 did not claim a new
  dtplyr union capability.
- Replacing a specification's internal argument quosures with arbitrary
  values raised an rlang quosure error. That forged representation was not
  produced by the documented constructors; ADR 0015's invariant boundary
  prevented treating it as a promised public-input rejection.
- A shadowed selection helper followed tidyselect's own helper-mask behavior;
  the initially suspected constructor discrepancy was withdrawn.
- SQLite source Date/logical representations, backend logical-to-text
  conventions, Arrow dictionary ordering, and unspecified UNION branch order
  were compared with direct backend controls rather than counted as failures
  of the package's row-copy contract.

The hunt did not exhaust exponential cube sizes, arbitrary extension-vector
classes, every Arrow dataset partition schema, live PostgreSQL, external SQL
warehouses, or every possible reentrant condition. No conclusion about those
boundaries followed from the passing probes. CI monitoring was outside the
requested work.

## Ticket boundaries

| Finding | Tracker | Blocking edges |
| --- | --- | --- |
| F1 | [#928](https://github.com/sayuks/marginplyr/issues/928), new `bug` / `ready-for-agent` ticket | None |
| F2 | [#923](https://github.com/sayuks/marginplyr/issues/923), existing `bug` / `ready-for-agent` ticket; [additional evidence](https://github.com/sayuks/marginplyr/issues/923#issuecomment-6441549674) | None |

F1 and F2 required independent fixes and neither blocked the other. Each
repair could be verified end to end without a preparatory refactor. The
`to-tickets` breakdown used those two complete behavior slices, reusing #923
for F2; the user had delegated granularity and explicitly waived a breakdown
approval round.

The branch contained repository-only investigation evidence under the existing
`investigation/` build exclusion. The package-affecting Review-ready check was
therefore not required by
[`design/agents/local-checks.md`](../../design/agents/local-checks.md).

Before publication, `jarl check .`, package-aware `lintr::lint_package()`
after `pkgload::load_all()` (cache disabled), and `git diff --check` passed.
`Rscript .github/scripts/verify-context-budget.R` passed at 20,008 bytes
against the 22,005-byte baseline.
