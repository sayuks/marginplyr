# Mixed local structured dimensions and name boundaries

Investigated: 2026-11-06
Baseline: `9ffb24614a71512170298a3827bc11c80354e742`

The earlier November 6 deep hunt compared isolated structured dimensions and
record/list families. This follow-up instead combined two independently typed
dimensions with a fixed key and payload, including simultaneous special names.
ADR 0016 supplied the ordinary mutation/union and factor boundaries; ADR 0012
supplied typed-missing labels; ADR 0018 supplied row-vector missingness and
physical Margin order. No new package defect was established.

[mixed-local.R](mixed-local.R) crossed eleven left and eleven right dimension
types, five name pairs and three Margin-order choices: 1,815 comparisons per
fixed-key layout. Types were integer, factor with an explicit NA level and a
missing code, ordered factor, Date, heterogeneous list, integer list-of,
integer matrix, NULL-cell list matrix, packed frame, nested packed frame and
zero-column packed frame. Name pairs included ordinary names, simultaneous
`.data`/`.env`, `..01`/`..-1`, `TRUE`/`FALSE` and private-prefix-like names.

The oracle enumerated the four cube sets itself. It constructed omitted values
with row-sized vctrs prototypes, combined the branches under ordinary safe
names, then restored public names. This prevented ordinary tibble name repair
from changing the oracle's schema. Its physical-order key independently
interleaved Grouping bits, rowwise missingness and values, with the occurrence
identifier last. The implementation under investigation supplied neither the
expected family nor its order. Every comparison also checked the original
input remained identical.

The distinct-fixed-key layout used three partitions, one missing. The shared
layout put two source rows in the same nonmissing partition and one in a missing
partition, exercising value and missingness comparisons within a fixed key.
Both final runs matched exactly: **3,630 comparisons**, no package errors,
ordinary-control failures or differences. The CSVs and logs retained each final
result; a separate R assertion checked both CSV row counts and every status.

Early harness attempts used bare injected pronoun names rather than `all_of()`,
placed the expected identifier after payload, and allowed ordinary union name
repair to reject the noncanonical-name fixtures. The final oracle corrected
those fixture errors. Preliminary runs were not added to the final totals or
treated as findings. The safe-name correction was followed by a complete
1,815-comparison rerun before the shared-key run.

From any directory, with a separate archive of the baseline as `package-root`:

```sh
Rscript <absolute-evidence>/mixed-local.R <package-root> <output.csv>
Rscript <absolute-evidence>/mixed-local.R <package-root> <output.csv> shared
```

The final executions were terminal sessions `95085` and `15827`, both collected
with exit zero. Earlier sessions `98974` and `5816` also ended before subsequent
runs. The environment was arm64 macOS, R 4.6.1, dplyr 1.2.1, vctrs 0.7.3,
tibble 3.3.1 and rlang 1.3.0; full session information was retained in the logs.
This matrix used local tibbles, three source rows, NULL labels and one finite
cube. It did not establish every local subclass, arbitrary size or text-label
combination. No package code or package tests were edited.
