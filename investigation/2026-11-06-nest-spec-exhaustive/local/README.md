# Local nesting name and shape crossings

Investigated: 2026-11-06
Tested package SHA: `547d77095c2923573216d002f50bef21778247f9`
Environment: R 4.6.1, aarch64-apple-darwin23; dependency versions and session details in [environment.txt](environment.txt).

This track examined both Nest verbs without changing package implementation or
permanent tests. The nesting reference, `CONTEXT.md`, and ADRs 0003, 0016, 0018
and 0033 supplied the source-cell, grouping, result-shape, missing-label and
declared-type contracts. The [earlier multikey oracle] and [structured-key
track] supplied prior coverage and controls. This track extended their finite
input sets with simultaneous special-name payloads and higher-dimensional,
zero-width and deeply packed keys. Neither original matrix established a new
package defect. A later independent replay confirmed the backend track's
root-subclass finding, as described below.

## Name crossings

`name-crossings.R` completed 612 comparisons, all matching. Each populated
source held three rows, one selected dimension and 25 simultaneously present
payload columns. The dimension took each of 26 names: ordinary `g`, `.rows`,
`.by`, `.groups`, `.data`, `.env`, `..01`, `.SD`, `.N`, `.I`, `T`, `F`, converter
formals, braces, whitespace, slash, accented/non-ASCII text and three internal
name-prefix lookalikes. Empty sources retained the same schema.

The checks crossed both verbs, both `.keep` choices, all three `.sort` choices
and empty/populated sources. The documented `.rows` rowwise-identifier refusal
was excluded, leaving 612 accepted combinations. Every call used the same
two-set specification: the selected dimension and the Grand total set, with a
public integer Grouping set identifier. Literal `all_of()` selection kept the
source-column meaning explicit for `.data`, `.env` and dot-like spellings.

Every comparison asserted public column names/order, rowwise identifiers when
requested, source-row membership, cell column names/order, original integer
payload values and unchanged serialized source. Empty results had zero outer
rows because the plan still contained a dimension. The checks exercised sort
choices for interference with these properties; they did not independently
verify the Margin order key, which the earlier multikey oracle had done.

## Shape crossings

`shape-crossings.R` completed 768 comparisons, all matching. Sixteen key
families crossed the same two verbs, keep/sort choices and empty/populated
sources, with and without an additional fixed key:

- four-dimensional numeric, logical, character, complex and list arrays;
- numeric zero-column matrices and numeric/list zero-width three-dimensional
  arrays;
- zero-column packed frames, packed array fields, three nested frame levels,
  packed factor fields with both a declared NA level and a separate missing
  code, and packed zero-column matrices;
- AsIs numeric matrices, four-dimensional arrays and lists.

The label was typed missing (`NULL`). An independent `vctrs::vec_group_loc()`
over the original fixed/included keys supplied expected memberships. Both the
detail set and Grand total set retained their source-row cardinality and
order within cells. Cell column names/order and source slices matched, and
every serialized source remained unchanged. These successful measurements do
not extend ADR 0016 into an arbitrary attribute-restoration guarantee.

## Rejected hypotheses and upstream boundaries

An initial exploratory name matrix included source columns literally named
`...` and `..1`. Those names were refused at input selection, before the nesting
fold. `refused-source-names.R` independently confirmed eight matching refusal
boundaries across those two source names and native `tidyr::nest()`, native
`dplyr::nest_by()` and both Margin Nest verbs. Renaming only the payload to
`value` gave eight successful controls. This was not promoted to a package
finding. Output `.key` names and source names have different contracts.

All-state Issue and PR searches used `payload names`; the complete bodies and
comments of [#784] and [#950] were read. #784 explicitly retained the existing
prohibited-name rules. #950 explicitly excluded source names refused by the
input-selection boundary while establishing dot-dot output-list-column names.
An initial bare-symbol attempt for `.data`, `.env` and `..01` also asked a
different selector question from the literal-name oracle. The final matrix
used `all_of()` and passed. Neither exploratory failure established a defect.

## Independent assessment of root-subclass head dispatch

The coordinator requested an independent contract assessment of
`backend/root-subclass-head.R`. `root-head-independent-controls.R` used a fresh
class name and method: its `head()` delegated normally and retyped `g` only on
zero-row output. Native local `dplyr::summarise(.by = where(is.character))`
returned the two character groups `a` and `b` with one row each. Native dtplyr
grouping followed by collection gave the same groups. Neither operation
invoked the method. Name-only inspection returned `(g)` without invoking it.
Typed Grouping inspection instead returned `()` after invoking the method
with the populated two-row source. The original source serialization stayed
unchanged in this independent retyping witness.

This was a package contract violation rather than the class/attribute
delegation boundary in ADR 0016. The computed Grouping plan selected the wrong
dimension; no input-class restoration was requested. ADR 0020's amendment
*a zero-row result does not permit full-source dtplyr work* required faithful
schema without processing source rows, or an explicit refusal. ADR 0029's
inspection amendment required isolated zero-row roots before evaluation. The
root branch of `zero_row_dtplyr_proxy_input()` called `utils::head()` on the
original root, and dispatch reached the subclass method before that isolation
could hold. Ordinary grouping/collection controls did not add that call.

Observation mode completed directly with exit 0. Desired-behavior mode
(`. --assert`) completed directly with exit 1 at the expected `(g)` assertion.
Both outcomes are captured in the adjacent logs. These independent controls
confirmed the primary finding; they did not create an additional ticket.

## Replay and limits

Run one command at a time from the tested package root. The optional first
argument selects another source checkout; matrix scripts accept a second
argument naming a CSV output file. Without it they write into R's temporary
directory. Checked-in CSVs and logs are the dated observations.

```sh
evidence=investigation/2026-11-06-nest-spec-exhaustive/local
Rscript "$evidence/name-crossings.R" . "$evidence/name-crossings.csv"
Rscript "$evidence/shape-crossings.R" . "$evidence/shape-crossings.csv"
Rscript "$evidence/refused-source-names.R"
Rscript "$evidence/root-head-independent-controls.R"
Rscript "$evidence/root-head-independent-controls.R" . --assert
```

The final name matrix retained session `28695` through terminal exit 0;
the shape matrix retained session `77100` through terminal exit 0. The refusal
controls completed directly with exit 0. Every execution was a direct R
process with no owned subprocess work. This track started no suite, build,
render or Review-ready invocation. The finite checks did not cover all custom
classes/methods, all Grouping specifications, lazy backends, resource limits,
interruption or other dependency versions and operating systems.

[earlier multikey oracle]: ../../2026-11-06-nest-grouping-greedy/local/README.md
[structured-key track]: ../../2026-11-05-nest-grouping-adversarial-followup/local/README.md
[#784]: https://github.com/sayuks/marginplyr/issues/784
[#950]: https://github.com/sayuks/marginplyr/issues/950
