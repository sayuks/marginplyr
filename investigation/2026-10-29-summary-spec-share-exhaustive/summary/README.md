# Local summary evaluation: unspecified values and frame expansion

Investigated: 2026-10-29
Repository snapshot: `4c63bb9`

The investigation ran in a new worktree created from `main`. It changed no
package implementation, existing package test, or generated reference. The
scripts in this directory were standalone investigation harnesses run against
`pkgload::load_all()` of that snapshot. Run them from the repository root.
Package versions were captured in `versions.txt`.

## Finding: a compatible unspecified/frame result invented an output column

```r
data <- data.frame(g = c("a", "b", "b"), x = 1:3)
value <- function(x) {
  if (length(x) == 1L) NA else data.frame(z = sum(x))
}
dplyr::summarise(data, value(x), .by = g)
# g: a, b; z: NA, 5
summarize_with_margins(data, value(x), .by = g)
# g: a, b; `value(x)`: NA, NA; z: NA, 5
summarize_with_margins(data, value(x), .grouping = rollup(g))
# Union branch 2 does not have the columns of branch 1.
# Only in branch 1: value(x)
```

An untyped logical `NA` was unspecified under vctrs' common-type rules. It was
compatible with a data frame: ordinary dplyr selected the frame's schema and
filled the first group's `z` with a typed missing value. The Margin verb
retained a second, all-missing column under the Assigned summary name. That
extra detail-branch column then made an otherwise compatible rollup refuse its
Grand total branch. Ordinary detail and grand summaries combined to the expected
three rows `a: NA`, `b: 5`, and `Total: 6` in `z`.

The focused replay established 48 affected combinations: plain data frame,
tibble, eager data.table; both group encounter orders; ordinary and
`pick()`-containing rewritten expressions; `.by`, included-only Grouping
specification, fixed-plus-included grouping, and rollup. Every single-set case
had the extra Assigned summary name; every rollup raised the branch-schema
error. Expression counts were two for the single-set cases and three for the
rollup cases, excluding repeated evaluation as the explanation.

Twenty controls passed. Twelve explicitly named mixed summaries retained
ordinary packing and common-type behavior. Six typed `NA_integer_`, `NA_real_`,
and `NA_character_` controls, in both encounter orders, retained the ordinary
incompatible-type refusal. Homogeneous frame and homogeneous unspecified
summaries agreed with dplyr. The typed-missing distinction mattered: this
finding did not ask the package to accept arbitrary frame/scalar mixtures.

## Causal separation and prior work

The candidate hypotheses were original-type compatibility, scalar carrier
schema, private grouping-key restoration, and repeated callback evaluation.
`carrier-control.R` reproduced the extra output with only dplyr/vctrs by wrapping
each non-frame result into a one-column frame. No private keys were involved.
Its common-prototype control had only `z`, while its wrapped summary had both
`value(x)` and `z`. The focused callback counts excluded duplicate execution.

`local_assigned_summary_value()` checked each original value's common type
before wrapping the value. That check admitted this compatible pair, but the
per-value carrier still turned the unspecified scalar into a frame with an
Assigned summary name. dplyr then combined that carrier's columns with the
actual frame's columns. Original-type compatibility and carrier schema were
therefore separate obligations.

Closed [#1020](https://github.com/sayuks/marginplyr/issues/1020) was read in full,
including its empty comment list. Its requested delivery was preservation of
ordinary dplyr's refusal for **incompatible** frame/non-frame pairs. The dated
follow-up note recorded that incompatible-result finding. The implementation
at this snapshot and `test-local-summary-types.R` covered those refusals and
homogeneous unspecified scalar/frame cases. They did not cover one unspecified
value mixed with a frame. Closed #430 and #619 concerned Assigned summary names
and immediate expansion, respectively. None of those findings established this
compatible-result schema divergence.

ADR 0028's local expansion/common-type contract was the expected-behavior
basis. The finding was within a single grouping-set branch. It did not request
support for arbitrary schema changes between grouping sets: the rollup's
ordinary branch schemas were identical and only the invented carrier column
made the Margin branches differ.

## Bounded sweep and negative evidence

`local-differential.R` compared 34 result shapes in every ordered pair, both
unnamed and explicitly named, for 2,312 ordinary-dplyr/Margin comparisons. It
recorded 260 matching successful results, 2,040 pairs where both refused, and
12 successful-result differences. Every difference was this finding: an
unspecified `NA` paired in either order with six frame shapes (plain, tibble,
zero-column, differently named, packed, and list-column frames). The comparison
checked values and attributes for successful calls; it did not assert exact
error wording/class equality where both verbs refused. The report printer
raised ordinary formatting warnings for packed shape displays; those were not
counted as package findings.

`expression-differential.R` compared 50 expressions across zero-, one-, and
four-row input, with/without a preceding ordinary summary and with/without an
explicit output name: 600 comparisons. These covered scalar and frame
closures, `pick()`, `across()`, dynamic function/name factories, literal and
dynamic unpacking, packed and list outputs, parenthesized helper heads,
sequential selections, ordinary pronouns, `if_any()`, `if_all()`, and vector or
matrix summaries. It recorded 548 matching results and 52 cases where both
refused; no additional difference was found. These counts bounded the probes;
they were not a claim of exhaustive R evaluation or S3 compatibility.

## Replay and desired acceptance

- `Rscript investigation/2026-10-29-summary-spec-share-exhaustive/summary/reproduce-unspecified-frame.R`
  completed in report mode and wrote the 48 affected cases plus 20 controls.
- The same command with `--assert` exited 1 against the desired behavior.
- `carrier-control.R`, `local-differential.R`, and
  `expression-differential.R` completed successfully; their CSVs and logs
  preserved the observations.

A single independent ticket was recommended, without blockers:

- Unnamed local summaries combining an unspecified missing value with a frame
  produce the same expanded schema and values as ordinary dplyr.
- A rollup whose ordinary branches agree on that schema completes with the
  expected detail and Grand total rows.
- Plain data frames, tibbles, eager data.tables, both encounter orders, fixed
  keys, and rewritten selections preserve that behavior.
- Explicitly named packing, homogeneous scalar/frame results, and genuine
  incompatible-type refusals remain correct.
- Each caller expression is evaluated once per group. Public-interface
  regression coverage demonstrates the fixed behavior.
