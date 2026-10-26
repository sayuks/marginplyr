# Expansion and Grouping specification bug hunt

Investigated: 2026-10-26
Baseline: `6b91254016dbcce60cc2cc69071f01f69eb8084e`

## Outcome

Four independently repairable runtime defects and one false documentation claim
were established on this baseline. The runtime findings were reproduced through
public calls and checked against working ordinary-backend controls. The
Grouping specification constructors and Grouping identity algorithms passed the
independent randomized models described below.

| Finding | Observable failure | Ticket boundary |
| --- | --- | --- |
| 1 | dtplyr refused a typed-missing branch for a classed list dimension. | Classed list dimensions omitted by a Grouping plan. |
| 2 | Arrow refused a fixed-size-list dimension even in a one-set expansion. | Typed prototypes for Arrow fixed-size-list dimensions. |
| 3 | SQLite changed unchanged fixed-key and payload types to logical without a public identifier. | Source-column typing at the direct expansion boundary. |
| 4 | SQLite could collect an identifier named `.data`, but could not materialize it. | Quoted public names in the owned SQLite materializer. |
| 5 | The reference promised dplyr-equivalent predicates on typed lazy inputs, but value predicates selected a different plan. | Document the metadata-only limit of lazy selection predicates. |

Each boundary was independently verifiable and had no blocking dependency on
another. No package implementation, generated documentation, or test-suite
source was changed. The accompanying script was investigation evidence,
excluded from the source package by the repository's investigation exclusion.

## Reproduction

Run from the repository root:

```sh
Rscript investigation/bug-hunt-2026-10-26-expand-grouping.R
```

The script loaded the working tree through `pkgload::load_all()` and exercised
five intended-result probes for the four runtime defects. All five failed on
the investigated baseline, after their passing controls; the command exited 1.
The separate `predicates` demonstration printed the local/lazy discrepancy and
passed its type-predicate controls. It did not assert that lazy inputs ought to
read source values.

A probe can be selected by its name: `dtplyr`, `arrow-one`, `arrow-rollup`,
`sqlite-types`, `sqlite-id`, or `predicates`. The runtime probes assert intended
behavior, rather than passing by recognizing the defects, and do not alter the
package namespace. The SQLite fixtures used disposable in-memory connections.

Measured installation: R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0, dtplyr 1.3.3,
data.table 1.18.6.1, Arrow 25.0.1, RSQLite 3.53.3, DuckDB 1.5.5, DBI 1.3.0,
vctrs 0.7.3, rlang 1.3.0, tibble 3.3.1, blob 1.3.0, and testthat 3.3.2.
These were measured versions, not minimum-version claims.

## Finding 1: dtplyr classed list dimensions

```r
x <- tibble::tibble(g = vctrs::list_of(1L, 2L), payload = 1:2)
expand_with_margins(
  dtplyr::lazy_dt(x),
  .grouping = rollup(g), .margin_label = NULL, .id = "sid"
)
# Error in dt_squash(): Invalid input
```

Construction raised `rlang_error` before a lazy result was returned. The same
local call returned four rows, including two rows with typed-missing lists.
An ordinary dtplyr union of the input with a branch assigning
`vctrs::vec_init(g, length(g))` also collected four rows and retained the
`vctrs_list_of` class. A one-set expansion retaining `g` succeeded.

The omitted-dimension path in `label_margin_branch()` in
[margin-label.R](../R/margin-label.R) converted a bare list prototype to a
list-producing expression for dtplyr. A classed list did not take that branch;
its prototype was injected directly into a mutation that dtplyr could not
parse. Thus the failure was in an expression marginplyr generated, despite an
available backend operation that represented the requested value.

`vctrs_list_of`, `AsIs` lists, and `blob` each failed with both `NULL` and
`NA_character_` labels and with 0, 1, and 2 rows: 18 dimension cases. The same
columns used as fixed keys or carried payloads passed 36 boundary cases.
`list_of` prototypes containing data frames, nested lists, and Dates reproduced
the same failure and passed the ordinary-backend control. The shared path also
failed through `summarize_with_margins()`.

The expansion reference promised one input-row copy per grouping set and
supported typed-missing labels. This finding did not ask for restoration of an
arbitrary attribute that ordinary dplyr had already discarded; its control
constructed and retained the list type successfully.

## Finding 2: Arrow fixed-size-list dimensions

```r
x <- arrow::Table$create(
  g = arrow::Array$create(
    list(1:2, 3:4),
    type = arrow::fixed_size_list_of(arrow::int32(), 2)
  ),
  payload = 1:2
)
expand_with_margins(
  x, .grouping = grouping_set(g), .margin_label = NULL, .id = "sid"
)
```

Construction raised:

```text
Can't create Array<fixed_size_list_of(int32(), list_size = 2L)> from object of type arrow_fixed_size_list / vctrs_list_of / vctrs_vctr / list
```

Even this one-set plan omitted no dimension. Both `inspect_grouping()` by name
and inspection with `where(is.list)` accepted the dimension. Keeping the list as
payload, or as a fixed key while another dimension rolled up, also succeeded.
Empty/nonempty input, one-set/rollup, and `NULL`/`NA_character_` labels produced
eight dimension failures, beside eight successful inspection/payload/fixed-key
boundaries.

`margin_column_info()` in [backend-metadata.R](../R/backend-metadata.R) built an
R missing prototype and passed its classed Arrow list representation to
`arrow::Scalar$create()` for the declared fixed-size-list type. That conversion
raised even when the operation would never need an omitted value.

The ordinary Arrow control created a missing scalar from `list(NULL)` with the
same Arrow field type, then unioned an included branch with a missing branch.
Both direct collection and materialization returned four correctly typed rows.
This distinguished the generated-prototype defect from an unsupported backend
cast. The controls and the intended-result assertions are in `arrow-one` and
`arrow-rollup` in the reproduction script.

## Finding 3: SQLite source-column types without an identifier

```r
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
x <- dplyr::copy_to(con, tibble::tibble(
  g = c("a", "b"), fixed = rep(NA_real_, 2),
  payload = rep(NA_integer_, 2)
), "source")
y <- expand_with_margins(
  x, .by = fixed, .grouping = grouping_set(g), .sort = "last"
)
vapply(dplyr::collect(y), typeof, character(1))
# fixed       g           payload
# "logical"   "character" "logical"
DBI::dbDisconnect(con)
```

The fixed key should have remained double and the unchanged payload integer.
Direct `compute()` followed by collection produced the same wrong types.
Adding `.id = "sid"` retained both source types, as did ordinary dbplyr mutation,
arrangement, and materialization of those source columns. Empty input reproduced
the failure too.

With `.sort = "none"`, direct collection retained the types, but direct
materialization lost them for both one-set and rollup plans without `.id`.
Sorted multi-set expansion retained the types; the sorted one-set/no-id path
did not. A 16-cell follow-up crossed empty/all-missing data, one/multiple sets,
none/last sorting, and identifier absence/presence, observing both endpoints.

The `type_anchor_columns` choice in `expand_with_margins()` in
[expand_with_margins.R](../R/expand_with_margins.R) withheld source-column anchors
from some one-set or unsorted/no-id paths. A non-missing label independently
installed the dedicated SQLite declared-type boundary. Its type anchor then
projected untyped `NULL` for carried columns that lacked an anchor.

[ADR 0031's B-direct amendment](../design/adr/0031-preserve-sqlite-typed-dimensions-under-margin-order.md#retained-guarantees-and-smaller-materialization)
retained source-column typing at the direct collection/materialization boundary,
including text labels and one-set expansions. The failing columns were unchanged
source columns, not ordinary aggregate expressions whose types the amendment
left to SQLite.

## Finding 4: SQLite identifier named .data

```r
y <- expand_with_margins(
  x, .grouping = rollup(g), .margin_label = NULL,
  .id = ".data", .sort = "last"
)
dplyr::collect(y)                         # Correct four-row result
dplyr::compute(y, name = "materialized")  # Refused by generated selection
```

Here `x` was a live SQLite table copied from two rows with `g = c("b", "a")`
and an integer payload, as constructed by the `sqlite-id` probe. Direct
collection retained the integer identifier `1, 1, 2, 2`. The direct
materialization failed:

```text
Can't select columns with `.data`.
`.data` must be numeric or character, not a <rlang_data_pronoun> object.
```

All 12 combinations of 0/2 rows, none/first/last sorting, and `NULL`/text labels
failed at materialization while collecting correctly. A safe identifier passed.
Adding an unrelated `.env` payload column made the `.data` materialization pass
as well. The failed call left the database table list unchanged; no destination
leak was observed.

`compute.marginplyr_sqlite_typed_result()` in
[sqlite-typed-order.R](../R/sqlite-typed-order.R) already bypassed dbplyr's
bare-symbol output selection for `.env`, by rendering the type anchor directly
for `db_compute()`. A `.data` output missed that route and reached the ambiguous
selection instead. This was distinct from finding 3's missing type anchors.

Ordinary dbplyr materialization could fail for an existing `.data` column too.
The finding concerned the dedicated materializer's direct-result contract in
[ADR 0031](../design/adr/0031-preserve-sqlite-typed-dimensions-under-margin-order.md#b-direct-amendment-2026-09-26),
where marginplyr already owned the public projection and quoted-name workaround.
It did not extend that contract to subsequent arbitrary dplyr transformations or
to DuckDB's ordinary materializer.

## Finding 5: predicate equivalence claim

The *Fixed columns and grouping dimensions* section in
[summarize_with_margins.R](../R/summarize_with_margins.R), inherited by expansion,
stated:

> Local data and the lazy backends whose types are available without a query answer a predicate as dplyr does.

The `predicates` probe contradicted that statement with a value-dependent
selection. For two rows whose character column `a` contained `x, y`,
`grouping_set(where(~ any(.x == "x")))` selected `(a)` locally and `()` for both
dtplyr and Arrow. With the same predicate in `.by`, the local summary had two
partitions with count 1; the dtplyr summary had no `a` column and one count of 2.
The type-based `where(is.character)` selected `(a)` on all three inputs.

Local selection received actual columns; lazy selection received schema-derived
or zero-row columns from `grouping_selection_proxy()`. A value predicate did
not describe the same question on those inputs. No declaration restricting the
quoted claim to metadata-based predicates was found in the inspected reference,
constructor documentation, or ADR 0002.

The ticket boundary was documentation: remove the false equivalence claim,
state the metadata-only boundary, and direct callers needing value-based
selection to collect explicitly. It did not require automatic source reads,
which would contradict [ADR 0020](../design/adr/0020-ask-before-reading-a-lazy-input.md),
or a new attempt to infer arbitrary predicate behavior. This was separate from
closed #453/#523, which concerned the diagnostic for backends with no readable
typed proxy at all.

## Search and controls

The investigation split constructors/inspection/identity, local/dtplyr
expansion, and live backend expansion into independent work, with public-call
reproductions checked again by the coordinating agent. The following were
completed measurements; cell counts denote combinations, not distinct defects.

| Probe | Executed range | Observation |
| --- | --- | --- |
| Recursive specifications | Seed 20261026; 1,000 recursively generated specifications over all five kinds, up to three generated levels, eight names including space/comma/equals/backtick | Independent set models agreed on membership, plan order, keep/drop/error duplicates, exact list-format names, and Grouping bits. Included overlapping composite dimensions, empty selections, and identity products. |
| Contextual identity | Seed 20261026; 200 recursive specifications, 0–10 rows, missing keys, optional fixed keys | Default and reversed explicit Grouping identifiers and every Grouping bit agreed with inspection and independently calculated masks. |
| Nested depth | Finite valid specifications at depths 10, 25, 50, 100, 200 | Expanded to the expected single grouping set. |
| Local/dtplyr types | 14 types × three labels × three sorts = 126 cases | No additional package-specific defect; ordinary backend raw/complex union and list-order restrictions were distinguished. |
| Local/dtplyr names | 52 names × four roles × two types × three sorts = 1,248 cells | Valid-name results agreed; invalid dot-dot names and backend collection restrictions were separated. |
| Factors | Ordered/NA-level/missing variants × three sizes × three labels × three sorts = 216 cases | 180 successful comparisons agreed; 36 were the same intentional NA-level refusal on both inputs. |
| Live backend names | 36 names × Arrow/SQLite/DuckDB × identifier presence × two labels × two sorts = 864 cells, each attempting direct collect/compute | 776 completed both endpoints without value/name mismatch; 72 hit external name-repair refusals; 16 hit `.data` materialization failures. SQLite's owned boundary was finding 4; DuckDB's ordinary counterpart was excluded. |
| Arrow physical types | 13 physical types × one-set/rollup × two labels × empty/nonempty = 104 cells | 84 completed direct collection/materialization with expected cardinality; eight were finding 2; 12 were external non-missing list/struct/map-to-text cast refusals. |
| Empty SQL results | SQLite/DuckDB × four dimension types × two labels × two sorts × identifier presence × three set orders = 192 cells | No execution errors; 16 cells, at 20 collection/materialization endpoints, exposed finding 3. |
| SQLite identifier neighborhood | Two sizes × three sorts × five identifier names × two labels = 60 cells | All 12 `.data` cases failed compute; safe and `.env` names passed; dot-dot construction refusals were excluded. |
| Typed selection comparison | 14 selector forms × local/dtplyr/Arrow = 42 comparisons with ordinary tidyselect | All matched for these name/type-based selectors, including helper arguments inspecting typed metadata. |
| Simulated SQL | 16 dbplyr simulators × two labels × three sorts = 96 renders | All cube expansions with an identifier rendered successfully. This established translation only. |
| Window order | One-set/rollup × SQLite/DuckDB = four cells | `.sort = "none"` retained usable input window ordering, also checked through cumulative-window execution. |

Additional probes covered zero-column data frames/tibbles and Arrow queries,
zero-width packed/matrix columns, one-/three-dimensional arrays, repeated grouping
sets, nested list prototypes, and fixed versus variable keys. Those did not
establish another defect.

The coordinating agent ran `devtools::test()` for expand-operation,
grouping-interface, grouping-plan, inspect-grouping, captured-grouping-helpers,
cyclic-grouping, and verb-argument-admission; all passed. The specification
investigator also passed the packed-grouping and matrix-grouping tests and
rechecked the relevant constructor/inspection files. These were focused checks,
not a full review-ready or release-matrix run.

The open-issue inventory contained #554, #468, and #256; it was checked for
duplication alongside targeted all-state searches for list types and predicates. No open duplicate
was identified. Findings were grouped by independent repair and observable
acceptance, rather than by failing cell or backend symptom count.

## Exclusions and limits

A caller-defined binding named `everything` initially looked like a name-only
selection-classifier defect. Ordinary tidyselect's helper mask also ignored
that binding, so the proposed counterexample did not distinguish marginplyr and
was rejected. Backend raw/complex union restrictions, list sorting, POSIXlt root
collection, AsIs factor casting, and DuckDB integer64 input conversion also
failed before or without the Margin operation; no new issues were assigned to
them.

The initial exploratory Arrow type scripts were not counted as completed
matrices: one stopped while printing binary data with an embedded NUL and
another used an invalid empty-input filter. The corrected physical-type and
empty-input probes above supplied the reported evidence.

The random fixtures were finite and small. Passing them did not prove arbitrary
R syntax, deeply generated cubes, arbitrary custom S3 classes, or all backend
compositions correct. SQL simulation did not execute a database server. The
investigation did not run live PostgreSQL or another SQL service, another R
version, or another operating system. No CI monitoring was performed.
