# Local named-label and packed-inner-name boundary hunt

Investigated: 2026-11-07
Baseline: `e1dabaabb392a3791177e5c48e983f780c7c93ec` (fresh main worktree)
Scope: dated investigation evidence only; no package implementation or tests

## Independent hypotheses and comparisons

The November 5 local row/type/order matrix and November 6 structured, list-array,
and mixed-key investigations supplied prior coverage and exclusions. Their
measurements were not counted as executions in this hunt. This lane instead
asked whether mixed named NULL/NA/text labels composed with factor reconstruction,
factor-level placement, repeated and reordered grouping sets, persistent groups,
source containers, and physical Margin order. A separate probe targeted names
inside packed keys, including duplicate, empty, missing and reserved names.

ADR 0012 supplied factor NA-level/code and synthetic-level rules; ADR 0016
supplied ordinary branch-composition and packed typed-missing boundaries;
ADR 0018 supplied physical order, row-vector missingness and fixed-key partition
ordering. The probes enumerated their grouping sets independently and never
used the implementation's inspected family as their oracle.

## Named-label matrix

[named-labels.R](named-labels.R) made 12,672 final comparisons across eleven
scalar/list families in a permutation of left/right pairings, zero/one/three
source rows, four named NULL/NA/text label pairs, four explicitly enumerated
families, first/last synthetic factor levels, four source containers and all
three Margin orders. The containers were tibble, data.frame, data.table and
persistently grouped tibble; the latter used its persistent fixed key.
The families were rollup, cube, a reversed grouping-set union and a union
retaining a repeated occurrence. Families were written independently of
inspection, including their dimension encounter order.

The result was **12,384 exact matches and 288 required refusals**, with no
package-only error, missing refusal, or value/type/schema/order difference.
The 288 refusals selected `NA_character_` for a factor with a declared NA
level; the other label spellings and unused levels were handled separately.
The [saved-result assertion](validate-results.R) checked all 288 diagnostics
explicitly: every refusal said `NA_character_` was already a factor level and
offered named-list `NULL` to preserve that level. Thus a generic package error
could not supply the recorded required-refusal evidence.
The oracle reconstructed factor codes and levels explicitly, separating
missing codes from stored NA-level observations, and used ordinary character
conversion or row-sized vctrs missing values for other omitted dimensions.
It combined ordinary branches and independently assembled physical order from
fixed-key missingness/value terms, occurrence membership bits, dimension
missingness/value terms and occurrence identifiers. Every call also checked
serialized caller input remained identical. The retained
[CSV](named-labels.csv) and [terminal log](named-labels.log) recorded all final
cases. Terminal session `37226` exited zero and was collected through its
original handle.

Preliminary fixture corrections were excluded from all final counts.

The initial named-label fixture put an `if` expression containing a bare
column name into tidyselect, which evaluated that branch outside a column
selection. The call was corrected to inject the preselected `.by` expression.
A second oracle correction derived dimension order from the explicitly written
grouping-set family, so a reversed union expected `b, a` rather than `a, b`.
Both preliminary executions were stopped and collected with exit one before
the final full rerun. A 48-case smoke run over every family and source container
matched after those corrections.

## Packed inner-name matrix

[packed-inner-names.R](packed-inner-names.R) made **1,296 exact matches** with
no ordinary-control error, package-only error or difference. Eight internal
name layouts crossed zero/one/two additional packing wrappers,
zero/one/three source rows, dimension/fixed-key/payload roles, tibble and
data.frame sources, and all three Margin orders. The layouts contained
duplicate, empty and missing names; pronouns; numbered dots and ellipsis;
Boolean words; a newline and a backtick; and Japanese/emoji names.
The ordinary branches and independently assembled order compared public
names, inner names, values, prototypes, packed shapes, row copies, occurrence
identifiers and requested physical order. Serialized caller input stayed
identical. The final [CSV](packed-inner-names.csv) and
[terminal log](packed-inner-names.log) retained the outcomes. Session `65367`
was collected with exit zero before the saved-result validation ran.

## Additional hypothesis disposition

An apparent masking risk in the collision checker was refuted independently
by the coordinator in a separate baseline archive: a nonfactor dimension
named `sum`, a default nonmissing label and `rollup(sum)` returned the expected
source and Margin rows. Ordinary summary collision counting also succeeded.
Parentheses around a symbol in the aggregate head did not establish the
predicted data-mask value lookup failure. This independent control was not
included in this lane's matrix counts.

## Replay

```sh
Rscript investigation/2026-11-07-expand-grouping/local/named-labels.R <package-root> <output.csv>
Rscript investigation/2026-11-07-expand-grouping/local/packed-inner-names.R <package-root> <output.csv>
Rscript investigation/2026-11-07-expand-grouping/local/validate-results.R investigation/2026-11-07-expand-grouping/local
```

Both execution probes explicitly disabled testthat parallel execution. The package root
used during execution was a separate archive of the baseline, loaded with
`pkgload::load_all()`; no build, rendering, or full suite supplied this evidence.

The recorded environment was arm64 macOS Tahoe 26.6.2, R 4.6.1, dplyr 1.2.1,
vctrs 0.7.3, tibble 3.3.1, rlang 1.3.0 and data.table 1.18.6.1. These finite
comparisons did not establish every type pairing, arbitrary extension-class
behavior, every syntax, other dependency/OS versions, unbounded plan size or
performance, or lazy-backend execution. No new local implementation ticket
followed from a confirmed local defect in this lane.
