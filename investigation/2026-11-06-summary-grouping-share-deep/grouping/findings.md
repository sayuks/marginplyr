# Grouping family algebra comparison

Investigated: 2026-11-06
Tested package SHA: `547d77095c2923573216d002f50bef21778247f9`

This lane loaded a disposable git archive of the tested SHA through pkgload.
No package implementation or package test was changed. The completed
`family-oracle.R` run used seed `11062026` and generated 1,500 finite Grouping
specifications. Each specification was inspected with duplicate policies
`keep` and `drop`, giving 3,000 comparisons. There were 2,882 exact matches
for the ordered grouping-set family, Grouping bits and Grouping set
identifiers, and 118 required refusals of empty rollup/cube dimensions.
No new defect was established by that run.

The independent oracle recursively enumerated set union, hierarchical
prefixes, subsets and Cartesian products. It deduplicated by exact column
membership while preserving the earliest occurrence. The fixture used six
columns, including comma, parentheses and backtick characters in names.
Generated nested unions/products were bounded to depth two; rollup and cube
arguments included ordinary selections and composite grouping sets.

The first attempted harness used a numbered-dot column name refused by
tibble. It was replaced with an admitted backtick-containing name before
the complete run. The cube oracle was also corrected before the complete
run to order subsets by descending cardinality and combination order.
Neither discarded harness attempt was included in the comparison count.

`family-oracle-output.txt` retained the final output and session information.
The shell execution yielded terminal session `66354`; the same handle was
collected through `write_stdin` to `exit_code = 0`. No owned execution or
child process remained active when this lane finished.

The run established finite family algebra and inspection correspondence for
these fixtures, not arbitrary tidyselect expressions, source-row execution,
backend translations, unlimited plans, every caller class, or other platforms
and dependency versions. Additional metadata-selection hypotheses were not
executed or reported as findings. The lane published no issue and ran no
full suite, package build/check, snapshots or rendering.

Replay from an archive of the named SHA, using the absolute retained script
path as the argument to `Rscript`. The script disables testthat parallel
execution and loads the archived source root from the working directory.
