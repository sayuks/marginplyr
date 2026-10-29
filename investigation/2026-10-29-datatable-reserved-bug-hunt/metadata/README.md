# Special-name metadata and derived-input audit

Investigated: 2026-10-29
Baseline: `4c63bb917ed7ed31ff6aadf060a5eae7a32cd004` (`main`)

This sub-investigation used R 4.6.1, data.table 1.18.6.1, and dtplyr 1.3.3.
It changed no package implementation or package tests. It compared typed
Grouping-plan inspection of derived dtplyr inputs with inspection after an
explicit ordinary collection, and checked isolation from caller tables and
row predicates. The name inventory included the documented special symbols,
the previously recorded internal bindings, bracket/construction formal names,
join prefixes, `..` spellings, and occupied safe-name candidates. Positive
`..j` and `...` names were R/tibble validation controls rather than claims
that data.table reserved those names globally.

## Finding: a discarded root name invalidated a safe visible schema

A raw data.table could contain a column named `..1` after `setnames()`. An
ordinary dtplyr selection removing that column exposed only the safe columns
`value` and `flag`, and ordinary collection returned that safe schema and its
original values. Local selection directly from the original minimally repaired
source succeeded too. The following public calls nevertheless failed:

```r
source <- data.table::data.table(special = "a", value = 1L, flag = TRUE)
data.table::setnames(source, "special", "..1")
step <- dplyr::select(dtplyr::lazy_dt(source), value, flag)
dplyr::collect(step) # successful: only value and flag
inspect_grouping(step, .grouping = rollup(where(is.integer)))
# Column 1 must not have names of the form ... or ..j.
```

Name-only inspection of `rollup(value)` succeeded. Typed fixed-key selection
failed in the same way. All four Margin verbs failed even with an ordinary
name-only `rollup(value)` and explicit `.by = flag`, because they acquired typed
metadata while preparing the operation. Thus the difference was not a
selection of an unsupported visible name, or an ordinary backend collection
failure. Renaming the root column away instead of selecting it away provided
an additional successful control.

The subset branch of `dtplyr_selection_proxy()` recursively requested its
parent's metadata before extracting the retained columns. Its root path used
`grouping_selection_proxy()` and ordinary zero-row collection, which rebuilt a
tibble with the discarded root name still present. That collection applied
name validation before the safe subset was reconstructed. The rename control
instead reached the isolated dtplyr proxy path and did not materialize the
invalid intermediate tibble. ADR 0002's schema snapshot and ADR 0013's
inspection contract concerned the caller's visible input schema; nothing in
the replay asked marginplyr to admit a positive `..j` or `...` column in that
visible schema.

[`hidden-source-names.R`](hidden-source-names.R) recorded 126 comparisons:
three discarded names (`..1`, `...`, `..2`), three row counts (0, 1, 3), drop
and rename derivations, and seven operations. All 54 drop-derivation calls
requiring typed metadata failed with
`tibble_error_column_names_cannot_be_dot_dot`. The nine name-only inspections
and all 63 rename-control calls matched their local materialized controls.
Every ordinary collected/local control succeeded, and no caller table changed.
The default replay exited zero after recording the finding. Its `--assert`
mode exited one with the desired-behavior assertion, preserved in
[`hidden-source-red-console.txt`](hidden-source-red-console.txt).

Issue bodies and comments for #947, #968, and #1002 were read during deduplication,
and the repository issue inventory was searched for the hidden/discarded-source
boundary. #947 concerned a visible `.SD` column rebuilt through a refusing list
converter. #968 concerned an identifier created by a Margin operation. #1002
concerned admitted signed/whitespace names in generated reads. None established
this safe-visible-input/discarded-root boundary. A single independent P2 ticket
was recommended for metadata acquisition through all affected public consumers;
there was no implementation prerequisite or blocker.

## Complete matrix and isolation results

[`inspection-matrix.R`](inspection-matrix.R) covered 67 names, three column
types (character, integer, factor), 18 derivations, three typed predicates,
and fixed/dimension selection roles. The derivations covered roots, selection,
renaming, row filters, ordering, grouping, mutations, cached aliases, and a
shared-source union. There were 21,588 records: 24 upstream construction
failures were recorded once rather than expanding each into six inspection
records, leaving 21,564 attempted typed inspections.

| Classification | Records | Interpretation |
| --- | ---: | --- |
| Matched materialized local control | 18,738 | No additional name-specific discrepancy |
| Local-control refusal | 1,528 | Included incompatible explicit `.by` requests on already grouped inputs |
| Upstream collection error | 840 | Ordinary collection already failed before any Margin operation |
| Upstream construction error | 24 | A valid derived input was not constructed |
| Inspection error | 452 | 36 exposed the discarded-root finding; 416 were metadata-safety refusals described below |
| Plan mismatch | 6 | `.BY` ordinary-selection name drift, isolated upstream below |

The 416 metadata-safety refusals comprised 390 factor-root shared-union cases,
eight `T`/`F` grouping cases whose dtplyr derivation introduced scalar updates,
and 18 `copied = .` cases lowered to dtplyr's whole-table placeholder. Those
inputs fell outside the implemented value-stable metadata grammar. No repair
was inferred from their successful explicit materialization.

[`isolation-matrix.R`](isolation-matrix.R) added 832 checks across thirteen
special/contextual names, zero and three rows, immutable and Mutable roots,
eight derivations including cached/shared graphs, and two typed predicates.
It recorded 780 matching controls, 48 upstream errors, and four upstream `.BY`
name mismatches. No row predicate ran during inspection and no caller table
changed in any of those checks. The core matrix likewise observed no source
table changes.

## Excluded upstream lead: ordinary `.BY` selection changed its name

The six core mismatches and four isolation mismatches selected a column named
`.BY` through ordinary dtplyr `select()`. `tbl_vars(step)` declared `.BY`, but
ordinary collection returned `BY`. Direct data.table selection
`source[, .(.BY, value)]` produced the same changed name, with no Margin
operation involved. Inspection of the logical step therefore differed from
inspection of the physically materialized table. This was recorded as an
upstream name-drift lead rather than a separate package ticket.
[`upstream-by-selection.R`](upstream-by-selection.R) isolated the discrepancy
and verified source preservation. It was distinct from the summary audit's
package-created lexical `.BY` binding rewrite.

## Replay

Run from the repository root:

```sh
Rscript investigation/2026-10-29-datatable-reserved-bug-hunt/metadata/inspection-matrix.R
Rscript investigation/2026-10-29-datatable-reserved-bug-hunt/metadata/isolation-matrix.R
Rscript investigation/2026-10-29-datatable-reserved-bug-hunt/metadata/hidden-source-names.R
Rscript investigation/2026-10-29-datatable-reserved-bug-hunt/metadata/hidden-source-names.R --assert
Rscript investigation/2026-10-29-datatable-reserved-bug-hunt/metadata/upstream-by-selection.R
```

The complete CSV records and captured consoles were retained beside this note.
The audit did not cover every possible derived dtplyr graph or arbitrary
value-dependent mutation, and it did not establish compatibility with another
R/data.table/dtplyr version.
