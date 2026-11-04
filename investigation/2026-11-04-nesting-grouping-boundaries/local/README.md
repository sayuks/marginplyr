# Local Nest branch and label boundaries

Investigated: 2026-11-04
Baseline: `7a05a1e42d53af62c057a30ad1892ffb35d4eef8` (fetched main)

Both local Nest verbs were checked against ordinary tidyr/dplyr nesting,
vctrs row slicing, and the public Margin label and packed dimension
contracts. The [November 3 local hunt](../../2026-11-03-nest-spec-greedy/local/README.md)
identified shaped source displays. The [November 4 follow-up](../../2026-11-04-nesting-grouping-followup/README.md)
identified bare AsIs POSIXlt keys and empty row-wise payloads. Their counts
were kept separate from this investigation.

## A packed key retained an incompatible AsIs POSIXlt component

A one-row tibble carried a packed key `g = tibble(ts = I(timestamp))` with a
UTC POSIXlt timestamp and an integer payload. Both Nest verbs and expansion
failed with `vctrs_error_ptype2` when a rollup used `NULL` as its Margin
label. The included branch's `g$ts` was an AsIs POSIXlt, while the omitted
branch's `g$ts` was its native packed data-frame representation.

Ordinary `tidyr::nest()` and `dplyr::nest_by()` accepted the key. Native
`vec_slice()` of the packed source key recursively normalized its component.
An ordinary dplyr union of that key and its `vec_init()` missing row
returned two rows with the original and typed-missing components. Both
Margin Nest verbs also worked when given the already sliced key. The
included-only Margin call and the summary rollup worked. These controls
separated key admission and missing-value construction from branch
combination. The source was unchanged in the matrix.

The [packed replay](packed-posixlt.R) crossed direct, one-level and
two-level packing with tibble/plain-frame sources, empty/populated/missing
timestamps, both missing-label spellings and both set orders. It compared
both Nest verbs and expansion, including both keep choices for the Nest
verbs. For normalized inputs it checked typed missingness, source membership,
payload order in each expansion branch and original key components under
`.keep`. The direct top-level case passed after #1236. The packed cases
still failed because preparation only sliced dimensions whose outer
representation was not already a data frame. The reference explicitly
admitted typed-missing packed dimensions, with vctrs and dplyr owning
their composed representation under ADR 0016.

The 360 matrix calls gave 120 matches for direct timestamp keys and 240
branch-type errors across the two packed depths. Every row-sliced control
passed. [Observation output](packed-posixlt.log)

The original unnormalized key also produced a cast error as a fixed key or
payload across two branches. A raw native dplyr union of two unchanged copies
failed with that cast error too. The packed-dimension finding used the
stronger typed-missing composition contract and its successful native
control.

All-state Issue and PR searches via `gh` used `"POSIXlt" "packed"` and
`"AsIs" "POSIXlt"`. The full body of closed
[#1236](https://github.com/sayuks/marginplyr/issues/1236) and body and comments
of merged [PR #1259](https://github.com/sayuks/marginplyr/pull/1259) specified
the direct timestamp key and verified its coverage. Earlier
[PR #1130](https://github.com/sayuks/marginplyr/pull/1130) dealt with
construction of supported scalar-class missing values. The new witness
kept a valid omitted value and placed the timestamp inside an existing
packed frame, beyond the tested direct key.

## Named-list labels leaked scalar shape and class

The [label replay](named-list-labels.R) used `g = 1:2` and
`.margin_label = list(g = matrix("Total"))`. The single-element matrix met
the package's character scalar guard. Both Nest verbs raised
`dims [product 1] do not match the length of object [2]` during observed
collision checking. Turning checking off made each result's character
display a three-by-one matrix. The same scalar passed directly, and
`list(g = "Total")` returned the equivalent plain character display with
checking enabled. A dimensioned label matching an observed source value
also reached the dimensional comparison error instead of the public
collision diagnostic in multirow inputs.

The direct scalar form passed through `as.list()`, which extracted the
string from its enclosing shape. The named-list form retained the original
element. Base comparison could then fail before the generated predicate
converted the comparison result to a vector. Even when checking was
disabled, the classed or shaped label reached the omitted branch and
influenced union or display shape. The reference accepts a character scalar
or `NULL` per dimension. The same character value and the typed-missing
form were used as controls. ADR 0012 supplies the named-list and collision
rules.

The 1,440 label comparisons crossed zero to three input rows, integer,
character and factor dimensions, checked and unchecked calls, noncolliding,
potentially colliding and typed-missing labels, five scalar wrappers, and
the four Margin verbs. The matrix recorded 64 dimensional comparison
errors, 136 branch-type errors from base `noquote()` elements, 280 successful
calls with output dimensions or class different from the plain scalar
control, 112 name-metadata-only differences from named inner scalars, and
848 exact matches. The name-metadata-only differences were recorded
separately from the failures. Direct controls removed an inner scalar's
name before passing it as an unnamed scalar, since retaining that name in
the direct form changes the dimension selected by the label. [Observation output](named-list-labels.log)

All-state Issue and PR searches used `"margin_label" "matrix" "list"`,
`"label" "dims" "product"`, and `"label" "scalar" "array"`. Closed
[#1178](https://github.com/sayuks/marginplyr/issues/1178) and merged
[PR #1191](https://github.com/sayuks/marginplyr/pull/1191) concerned
a one-dimensional **source** display with an ordinary scalar label.
Closed [#834](https://github.com/sayuks/marginplyr/issues/834) concerned a
multidimensional **source** dimension. The [older scalar-shape matrix](../../2026-10-29-nest-spec-dispatch-followup/nesting/scalar-shapes.R)
supplied labels directly. The named-list element is the differentiating
boundary in this investigation.

## Replay

Run from the repository root:

```sh
Rscript investigation/2026-11-04-nesting-grouping-boundaries/local/packed-posixlt.R
Rscript investigation/2026-11-04-nesting-grouping-boundaries/local/packed-posixlt.R --assert
Rscript investigation/2026-11-04-nesting-grouping-boundaries/local/named-list-labels.R
Rscript investigation/2026-11-04-nesting-grouping-boundaries/local/named-list-labels.R --assert
```

Observation mode verifies the controls and prints the result counts.
Assertion mode requires matching behavior and was expected to fail on the
investigated revision. Runs used R 4.6.1, dplyr 1.2.1, tidyr 1.3.2,
tibble 3.3.1 and vctrs 0.7.3. Evidence is finite and covers the listed
R and package versions.
