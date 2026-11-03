# Local nesting: empty displays and scalar-array collision checks

Investigated: 2026-11-03
Baseline: `4b34fdecb2d8a41034b3d9b5d5af8e51592555e1` (`main`)

## Scope and outcome

This track investigated local `nest_with_margins()` and
`nest_by_with_margins()` source memberships, cell projections, empty results,
and unusual source-key classes. The October 30/31 nesting investigations and
November 2 [local composition record](../../2026-11-02-nest-grouping-hunt/local/README.md)
supplied earlier findings and limits; their measurements were not counted as
runs on this baseline. Two additional display boundaries were confirmed.
No package implementation, permanent test, dependency or prior note changed.

The nesting reference promised one source cell per group, original values
under `.keep`, and zero outer rows for an empty keyed input. Its inherited
*Display labels and grouping identity* section said that non-missing labels
converted ordinary dimensions to character. The `.check_margin_label`
argument promised a comparison between each dimension's source displays and
its non-missing label. Those were the direct contracts tested here.
ADR 0016 separated these constructed properties from arbitrary attribute
restoration. Neither finding depended on extending ADR 0033's limited list
of declared output types to ordinary dimension columns.

## L1: empty hms displays acquired the wrong type or failed branch combination

The minimal fixture was a zero-row tibble with one column,
`hms::hms(numeric())`. Both nesting verbs with `rollup(g)` and their default
non-missing label raised `vctrs_error_ptype2`: the included branch's generated
dimension had type logical, while the omitted branch's dimension was
character. A single included `grouping_set(g)` avoided combination but
returned a logical zero-length outer key instead of the documented character
display. Expansion and summary reached the same boundary.

The ranked hypotheses were that empty source grouping was unsupported, that
source-class reconstruction caused the mismatch, or that the generated display
conversion supplied an unexpected empty type. Ordinary empty dplyr grouping
and `nest_by()` succeeded. Populated hms values, a fixed hms key, typed-missing
labels, and empty plain character/difftime dimensions succeeded. The same
failure occurred with tibble, plain frame, data.table and immutable dtplyr
sources. These controls separated source grouping and reconstruction from
display conversion.

The direct converter observation settled the remaining hypothesis:
`as.character(hms::hms(numeric()))` returned `logical(0)` with hms 1.1.4,
while its populated conversion returned character. The package's generated
conversion retained that result in included branches. The missing branch
already supplied a character display. A hand-written conversion followed by
ordinary union reproduced the type incompatibility too; this was an upstream
converter behavior reaching a package-constructed display boundary, rather
than a claim that ordinary unions themselves were defective.

`display-conversion.R --assert` asserted successful zero-row character-key
nesting through both verbs and returned terminal exit 1 on this baseline.
Its observation mode returned terminal exit 0 after recording the dated
failure and controls. The final 576-call matrix crossed hms/difftime/character,
zero/two source rows, four source representations, single/rollup/cube plans,
explicit collision-check choices, and four Margin verbs. There were 480
character-output matches, 64 errors (every empty hms rollup/cube case), and
32 wrong display types (every empty hms single-set case). Those results were
retained in `display-conversion.csv`. Nine direct converter families were
also measured; only hms returned a non-character empty result.

## L2: scalar-array display methods broke observed-label checking

An installed-package witness used a one-element `posterior::rvar`, with four
draws all equal to one and a source-row identifier. Its public vector shape
was one-dimensional with one element. `as.character()` returned the scalar
display `1 ± 0` while retaining a one-dimensional array shape. Both nesting
verbs with a single included grouping set and the non-colliding default
label raised an External condition from the generated predicate:
`condition must be a logical vector, not a logical 1D array`. No rollup,
omitted value, Margin order or multi-set combination was necessary.

The ranked hypotheses were unsupported source grouping, general inability to
display that class, and the collision predicate assuming an unshaped character
vector. Ordinary `tidyr::nest(.by = g)` and unsorted dplyr `.by` grouping
succeeded. Disabling only `.check_margin_label` produced the correct cell and
display. Typed-missing labels and fixed-key nesting succeeded too. An
ordinary one-dimensional integer array whose base display discarded its
dimension also succeeded. Independently evaluating the generated comparison
and `if_else()` reproduced the shape error, while an unshaped comparison
counted zero non-collisions or one matching display as required.

`array-display-collision.R` removed the posterior dependency from the witness.
Its one-dimensional integer-array subclass changed only `as.character()` to
retain the input's one-dimensional shape. Native tidyr nesting succeeded.
All four Margin verbs with collision checking disabled returned the correct
character display; all four checked calls failed with the same shape error.
A label equal to the actual display also produced that External condition
instead of the required Package condition for an observed collision. The
script's desired-behavior mode returned terminal exit 1. It demonstrated the
same mechanism without requiring a new test dependency.

The final posterior matrix made 64 populated nesting calls over one/three
source rows, tibble/plain-frame sources, included-only/rollup specifications,
absent/present labels, check choices and both verbs. Ordinary tidyr controls
succeeded for every source fixture. All 32 unchecked calls succeeded, while
all 32 checked calls produced the external shape error. These counts were
separate from the dependency-free witness and the initial exotic-key sweep.

This was an admitted scalar array: the non-missing-label refusal in
`validate_margin_label()` distinguished matrices and arrays with more than one
dimension. The reference's matrix/higher-dimensional refusal did not require
refusing this one-dimensional source. The generated observed-label predicate
in `check_observed_label_collision()` fed its shaped comparison directly to
`dplyr::if_else()`. The successful display construction and unsorted native
controls separated the failure from arbitrary class restoration or unsupported
source grouping.

## Wider finite source-key sweep

`exotic-membership.R` made 1,024 calls over 16 key families, populated/empty
sources, fixed/dimension roles, included/rollup plans, missing/display labels,
keep choices and both nesting verbs. The families included POSIXlt, difftime,
hms, units, haven labelled/SPSS vectors, fs paths, hex/oct modes, blobs,
list-of vectors, zero-column and deeply packed frames, three-dimensional AsIs
arrays, spatial geometry and posterior rvar values. Every fixture also carried
deep packed factor/matrix values and reference-bearing payloads.

The independent source-group oracle matched every admitted cell membership
and all 5,768 checked cell-column projections. There were 956 admitted matches,
56 expected packed/array non-missing-label or spatial-missing refusals,
four empty hms combination failures and eight populated rvar collision-check
failures. No additional mismatch was found. Source serialization stayed
unchanged after native grouping had materialized the rvar equality-proxy
cache; native grouping itself filled that cache in the initial development
run, so that earlier serialization signal was not a target mutation finding.

## Duplicate checks and exclusions

All-state Issue/PR searches used the following terms through `gh`, with JSON
results retained beside this note: `hms`, `rvar`,
`"non-missing" "empty" "label"`, `"collision" "array"`, and
`"logical" "character" "label"`. Searches found no hms report. The rvar
search found closed [#1069](https://github.com/sayuks/marginplyr/issues/1069)
and [PR #1077](https://github.com/sayuks/marginplyr/pull/1077), which concerned
typed-missing representability rather than non-missing-label checking.

Full bodies and comments were read and retained for
[#427](https://github.com/sayuks/marginplyr/issues/427),
[#834](https://github.com/sayuks/marginplyr/issues/834),
[#896](https://github.com/sayuks/marginplyr/issues/896),
[#997](https://github.com/sayuks/marginplyr/issues/997),
[#1040](https://github.com/sayuks/marginplyr/issues/1040),
[#1069](https://github.com/sayuks/marginplyr/issues/1069),
[#1071](https://github.com/sayuks/marginplyr/issues/1071), and
[#1093](https://github.com/sayuks/marginplyr/issues/1093). Their boundaries
were multidimensional-label refusal, list-key ordering, scalar output names,
typed-missing validation, omitted-value prototypes, or dtplyr scalar-overwrite
coercion in an omitted branch. L1 concerned the included display conversion
on every measured backend instead. Neither finding here
required those closed repairs to be reopened. The two findings were
independently repairable and neither blocked the other.

One broader posterior matrix terminated with exit 139 while a generated
summary reached `vctrs::vec_group_loc()` on a direct-constructor empty/one-draw
fixture. The retained `rvar-incomplete-summary-crash.log` recorded the crash;
it was not assigned to marginplyr, included in completed counts, or published
as another finding. A subsequent data.table representation failed an ordinary
tidyr control with a native vector-size error; that representation was excluded
from the completed posterior matrix. The failed control was retained in
`rvar-incomplete-data-table.log`. No claimed pass depended on either execution.
Ordinary `dplyr::nest_by()` additionally requested ordering that posterior
refused; the retained unsorted controls used tidyr nesting and `.by` grouping.

## Replay and environment

Run the R entry points from the investigated checkout root. Every entry point
loaded that checkout using `pkgload::load_all()`; development execution used
an independent archive of the baseline under `/private/tmp`.

```sh
Rscript investigation/2026-11-03-nest-spec-greedy/local/exotic-membership.R
Rscript investigation/2026-11-03-nest-spec-greedy/local/display-conversion.R
Rscript investigation/2026-11-03-nest-spec-greedy/local/display-conversion.R --assert
Rscript investigation/2026-11-03-nest-spec-greedy/local/rvar-label-collision.R
Rscript investigation/2026-11-03-nest-spec-greedy/local/rvar-label-collision.R --assert
Rscript investigation/2026-11-03-nest-spec-greedy/local/array-display-collision.R
Rscript investigation/2026-11-03-nest-spec-greedy/local/array-display-collision.R --assert
```

The three `--assert` commands intentionally failed on this unfixed baseline;
observation modes recorded dated symptoms and were not passing regression
tests. All execution handles were retained through terminal outcomes. Final
observation sessions were 42417 (exotic), 53651 (empty displays), and 66905
(posterior collision matrix), each terminal exit 0. Session 81487 was the
collected exit-139 experiment, and session 94557 the collected failing native
data.table control. Adjacent logs retained the minimal desired-behavior failures.

Logs recorded R 4.6.1, macOS Tahoe 26.6.2, dplyr 1.2.1, vctrs 0.7.3,
tibble 3.3.1, hms 1.1.4, posterior 1.7.0 and dtplyr 1.3.3. These finite
measurements did not establish all S3/S4 behavior, another dependency/R/OS
version, freedom from source-backend limitations, arbitrary attributes or
resource-exhaustion behavior. No full package suite or Review-ready invocation
ran in this track; the coordinating agent owned publication verification.
