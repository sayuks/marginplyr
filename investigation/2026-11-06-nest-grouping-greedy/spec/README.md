# Grouping specification family boundaries

Investigated: 2026-11-06
Tested package SHA: `952a0d9b18fd3ff4631f153432f6024ad62fcac2`

This track checked ordinary Grouping specification algebra using an independent
set-family oracle. The November 5 [family investigation] supplied the starting
oracle; this run changed its column vocabulary and bounded generated-tree count,
then added explicit dimension-width and empty-selection controls. The five
public constructors and `inspect_grouping(.format = "list")` were the exercised
seams. No product implementation, package tests, or dependency declarations were
changed.

## Results

All 1,588 comparisons matched the independent expected result. This track did
not establish a new defect or recommend a ticket.

| Boundary | Comparisons | Observed result |
| --- | ---: | --- |
| Generated families over non-syntactic column names | 1,500 | All complete inspection tibbles or expected duplicate refusals matched |
| Variable-dimension widths 0, 1, 30, 31, 32, 33 and 64 | 78 | Exact dimensions, bits, identifiers and duplicate policy matched |
| Empty selections and empty composite dimensions | 10 | Six valid plans and four Package-condition refusals matched |

The generated matrix used seed `20261106` and 250 trees, 50 rooted at each
constructor kind. Families had at most 64 occurrences before duplicate
handling. Each tree ran with `error`, `drop` and `keep`, with and without an
explicit fixed key. Column selections alternated literal character vectors,
`all_of()` and positions. The source names contained spaces, commas, equals
signs, a backtick, a tab, a newline, composed/decomposed Unicode, `.rows` and
`..0`. The fixed-key name contained whitespace and a newline. The oracle
compared complete ordinary inspection tibbles, including occurrence order,
fixed/included/omitted columns, named Grouping bits and Grouping identifiers.
There were 1,386 successful-plan comparisons and 114 expected duplicate
refusals. The set-family calculation called none of marginplyr's expansion,
deduplication or mask helpers.

The dimension boundary covered a union, a product with its identity factor,
a composite rollup and a composite cube. Through 31 variable dimensions the
Grouping identifier matched the independently calculated bit mask; at widths
32, 33 and 64 it was `NA_integer_` while every named Grouping bit remained
complete. The width-zero union/product controls correctly applied duplicate
policy to their two empty occurrences. Composite constructors were omitted at
width zero because that is their explicitly invalid empty-dimension case.

Empty direct selections formed the empty grouping set in singleton, union and
product positions. Empty selection arguments alongside an actual rollup/cube
dimension did not add a dimension. An all-empty rollup/cube and a composite
dimension resolving to empty each raised a Package condition. These controls
followed the constructor grammar in [ADR 0008] and the exact inspection values
in [ADR 0013].

## Replay and limits

Run from the investigated package root, with this evidence directory available:

```sh
Rscript investigation/2026-11-06-nest-grouping-greedy/spec/family-boundaries.R
```

`MARGINPLYR_HUNT_SOURCE` selects another source copy, and
`MARGINPLYR_HUNT_OUTPUT` selects an output directory. The final execution used
terminal session `57642` and completed at exit 0. The adjacent log and CSV files
recorded the results. The process used R 4.6.1 on aarch64 macOS Tahoe 26.6.2,
dplyr 1.2.1, tidyselect 1.2.1, rlang 1.3.0, tibble 3.3.1 and vctrs 0.7.3.

This finite oracle established these algebra, naming and identifier boundaries;
it did not establish arbitrary expression evaluation, backend metadata timing,
unbounded family resource use, or result-cell payload correctness. It ran no
complete test suite, package build, installation or render. No fix or post-fix
result was claimed.

[family investigation]: ../../2026-11-05-nest-grouping-adversarial-followup/spec/README.md
[ADR 0008]: ../../../design/adr/0008-centralize-grouping-specification-kind-rules.md
[ADR 0013]: ../../../design/adr/0013-inspect-grouping-plans-as-ordinary-tibbles.md
