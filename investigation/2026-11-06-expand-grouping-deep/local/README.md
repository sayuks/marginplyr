# Local expansion structured-key and record/list boundary investigation

Investigated: 2026-11-06
Baseline: `547d77095c2923573216d002f50bef21778247f9` (`main`)
Scope: dated evidence only; no implementation or package-test changes

## Questions and independent comparison

The November 4 [boundary hunt](../../2026-11-04-expand-grouping/README.md),
November 5 [local comparison](../../2026-11-05-expand-grouping-greedy/local/README.md)
and November 6 [list-array investigation](../../2026-11-06-expand-grouping-hunt/local/README.md)
supplied previously measured families and defects. Their counts were not reused
as executions here. The baseline already contained the list-array missing-row
repair for #1364, so that prior witness was not treated as a new defect.

The new probes asked whether typed-missing row construction, branch combination
and Margin order continued to agree with ordinary dplyr/vctrs composition when
keys had zero extents, dimension labels, multiple ranks, nested packing or
record/list storage. ADR 0016 supplied the ordinary-composition boundary;
ADR 0018 supplied fixed-key missingness, Grouping-bit ordering and row-vector
missingness. ADR 0012 supplied the distinction between factor NA levels and
missing codes.

Both probes enumerated the two grouping sets independently: the complete
dimension and the empty set. Their ordinary branches copied the source or
replaced the omitted dimension with `vctrs::vec_init()` at source-row size,
then combined the branches with `dplyr::union_all()`. The reference Margin order
was independently assembled from fixed-key missingness/value terms, the
explicit occurrence's Grouping bit, dimension missingness/value terms and the
occurrence identifier. Exact comparisons included source-row identifiers,
public column order, values, types, shape, attributes and requested physical
row order. They established no arbitrary attribute-restoration promise.

## Structured-key matrix

[structured-edges.R](structured-edges.R) crossed ten storage/classes (integer,
logical, nonfinite double, complex, character, plain list, factor, ordered
factor, Date and POSIXct), zero/one/three source rows, rank-one through rank-three
shapes including zero trailing extents, absent/present dimension labels,
zero/one/two packed wrappers, dimension/fixed-key/payload roles and all three
Margin orders. The retained [CSV](structured-edges/results.csv) and
[terminal log](structured-edges.log) carried the final outcomes.

The final run recorded 11,340 comparisons: 9,612 exact matches and 1,728
ordinary-composition failures, with no fixture error, package-only error or
difference. The 1,728 failures held Date, POSIXct, factor or ordered-factor
arrays as dimension or fixed keys and requested sorting. Both the ordinary
reference and expansion raised `expressionStackOverflowError`.

The [separating controls](array-sort-controls.R) and their
[terminal log](array-sort-controls.log) additionally compared ordinary
value-only `arrange()` with sorted expansion over sixteen class/width/packing
fixtures. This avoided inferring an upstream limit solely from the oracle's
own `vec_detect_missing()` call: value-only ordering failed independently too.
All sixteen unsorted ordinary-union and expansion comparisons matched.

An initial fixture wrote a one-element dimension label for a zero extent
through `paste0()` recycling. That fixture error was corrected with
`recycle0 = TRUE` before the full final rerun. The preliminary excluded cases
were not counted as successful evidence.

## Record and list boundaries

[record-list-edges.R](record-list-edges.R) executed 567 comparisons across a
two-field vctrs record, list-of raw vectors, list-of data-frame rows, lists of
matrices, AsIs lists, AsIs factors, and heterogeneous lists containing a
language object, an environment and a function. It crossed the same three
source sizes, three packing depths, three roles and three orders. The final
[CSV](record-list-edges.csv) and [terminal log](record-list-edges.log) recorded
486 exact matches and 81 ordinary-composition errors, with no package-only
error or difference.

All 81 excluded cases held an AsIs factor, directly or in a packed key.
Ordinary `union_all()` refused the factor/integer combination; expansion
reached the same combination boundary. Its vctrs diagnostic named privately
renamed branch columns. That diagnostic path was recorded rather than promoted
to a data-integrity finding: the underlying ordinary operation was unsupported,
and ADR 0022's summary-context restatement did not establish a general rewrite
contract for arbitrary vctrs diagnostics.

## Replay and limits

From the package checkout root:

```sh
MARGINPLYR_HUNT_OUTPUT=investigation/2026-11-06-expand-grouping-deep/local/structured-edges \
  Rscript investigation/2026-11-06-expand-grouping-deep/local/structured-edges.R
MARGINPLYR_HUNT_OUTPUT=investigation/2026-11-06-expand-grouping-deep/local/record-list-edges.csv \
  Rscript investigation/2026-11-06-expand-grouping-deep/local/record-list-edges.R
Rscript investigation/2026-11-06-expand-grouping-deep/local/array-sort-controls.R
```

All scripts explicitly disabled testthat parallel execution and used fresh R
processes. Owned executions were observed to terminal completion through their
original handles: `94952` (final structured matrix), `68030` (final record/list
matrix) and `95978` (separating controls), each exiting zero. The saved CSVs were
also checked to contain no package-only difference, package-only error or
fixture error. Captured logs had trailing spaces and final blank lines removed.
No package build, rendering or full test suite supplied this
evidence. Environment: arm64 macOS Tahoe 26.6.2, R 4.6.1, dplyr 1.2.1,
vctrs 0.7.3, tibble 3.3.1, rlang 1.3.0 and tidyselect 1.2.1.

The fixtures used tibbles and finite two-occurrence plans. They did not establish
arbitrary extension classes, other input containers, other package/OS versions,
unrestricted Grouping-plan sizes, performance bounds or lazy-backend behavior.
No new local implementation ticket followed from an ordinary unsupported
operation or a fixture error.
