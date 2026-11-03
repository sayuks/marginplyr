# Local expansion type and repeated-operation boundaries

Investigated: 2026-11-03
Baseline: `4b34fdecb2d8a41034b3d9b5d5af8e51592555e1` (`origin/main`)
Scope: dated exploratory evidence; no package changes or regression tests

The October 30, October 31 and November 2 expansion notes supplied prior
coverage and existing ticket boundaries. Their measurements were not counted
as executions in this track. New probes explored additional vector/container
combinations and repeated expansion, against the row-copy contract, ADR 0012's
factor identity, ADR 0016's delegated class/attribute boundary and ADR 0018's
Margin order. No new package-only defect was established in this local track.

## Additional vector/container matrix

Hypothesis: prototype construction, local branch combination or Margin-column
placement could reject a supported column or change its values relative to an
ordinary dplyr branch composition.

`additional-types.R` crossed 22 requested families, zero/one/three requested
rows, direct/packed/nested-packed shapes, dimension/fixed/payload roles,
tibble/data.frame/data.table containers and three sort options. Families
included complex, expression, POSIXlt, timeDate, mcmc, sparse Matrix,
distributional normal/mixture/hilo, posterior integer/logical/factor/ordered
rvar, wk WKT/WKB, JSON/HTML/UTF-8 strings, list-of integers/frames, zero-width
packed frames and numeric versions. Some constructors supplied fewer than the
requested three vctrs elements; ordinary container conversion also expanded or
renamed some keys. These were fixture exclusions rather than admitted inputs.

The final run recorded 4,166 observations:

| Classification | Count |
| --- | ---: |
| Matching class/value/attribute comparison | 3,579 |
| Ordinary fixture exclusions | 161 |
| Ordinary branch-combination errors | 39 |
| Shared ordinary ordering errors | 356 |
| Shared data.table row-size limitations | 10 |
| hilo data.table field-name attribute differences | 21 |

The ordinary value oracle used row-sized `vec_init()` values, ordinary mutate,
relocation and branch binding. Comparisons aligned occurrence and source-row
identifiers, so this matrix did not independently prove the requested physical
Margin-order sequence. Input serialization was checked after each paired
control/target sequence, including failures; the paired execution was not an
isolated proof about every class's error-path mutation behavior.

`ordinary-controls.R` minimized the remaining observations. A one-row
data.table whose first column became a hilo record was reported as three rows
after ordinary `relocate()`. Ordinary mutate then raised the same subscript
error as expansion. A three-row source expanded to six underlying values but
both ordinary binding/relocation and expansion reported three rows, the number
of fields in the first record column. The controls therefore refuted a
package-only row-copy failure for this unsupported ordinary container shape.
The 21 payload differences had identical classes and numerical proxy fields;
only blank field-name attributes differed. ADR 0016 did not promise restoration
of those arbitrary class-internal attributes. No Issue was proposed for them.

An initial mixture constructor omitted its required weights and stopped before
measurement. An initial matrix failed while aligning the data.table record
result, and the first structural-classification rerun contained an invalid
single-index data.table control. Those harnesses were corrected and the whole
matrix rerun. Their partial observations were excluded from the final counts.
The final structural control also excluded converted fixtures lacking the
requested key column rather than treating environment fallback as a selection
defect.

## Repeated expansion and independent order

Hypothesis: expanding an already expanded result could lose occurrence
identity, source missingness, factor levels, payload rows or the second
operation's Margin order, particularly with duplicate sets and prior margins
fixed into the next operation.

`re-expansion.R` made 2,808 comparisons across eight key families,
zero/one/four rows, four first plans, three second plans, optional prior
occurrence/fixed keys, NULL/text labels where admitted and three sort choices.
All 2,808 matched. The plans covered rollup, cube, omitted-first and retained
duplicate occurrences. Key families were integer, factor, ordered factor,
factor with an NA level, Date, plain list, packed frame and integer matrix.
The oracle enumerated grouping sets itself, copied source rows and replaced
omitted values directly; it did not trust `inspect_grouping()` as its oracle.
It constructed factor integer codes/levels independently, including NA-level
identity and synthetic levels from two successive text labels. Requested order
used fixed missingness/value, dimension bit/missingness/value and occurrence
tiebreaks; ordinary vctrs supplied only delegated value ordering. Unordered
comparisons aligned the two occurrence identifiers and original source row.

The initial factor oracle combined factors directly. Ordinary combination
dropped an NA level and ordered factors with unequal declared levels were
incompatible, although expansion's restoration contract supported them. The
corrected oracle combined integer codes and restored independently enumerated
levels, then reran the entire matrix. Only the corrected 2,808 passing cases
were counted. An initial terminal-print formatting error happened after writing
its exploratory CSV; neither its counts nor that execution was a final pass.

## Replay and limits

From the repository root:

```sh
Rscript investigation/2026-11-03-expand-grouping-greedy/local/additional-types.R
Rscript investigation/2026-11-03-expand-grouping-greedy/local/ordinary-controls.R
Rscript investigation/2026-11-03-expand-grouping-greedy/local/re-expansion.R
```

All three final commands exited zero. Set `MARGINPLYR_HUNT_SOURCE` to load a
pinned disposable source and `MARGINPLYR_HUNT_OUTPUT` to change the matrix
artifact directory. The adjacent CSVs and logs recorded the completed runs;
`environment.txt` recorded the package versions and tested SHA.
`TESTTHAT_PARALLEL=false` was explicit. Original execution handles were
retained to terminal completion and no owned execution remained at handoff.

These were finite macOS/R-version observations. They did not enumerate
arbitrary S3/S4 methods, callback/reentrancy behavior, all record/container
combinations, warning propagation, dependency versions or other operating
systems. No full suite, package build, render, remote service or implementation
repair was part of this track.
