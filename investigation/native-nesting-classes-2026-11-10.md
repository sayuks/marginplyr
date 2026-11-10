# Native nesting container classes

Investigated: 2026-11-10

## Scope and reproduction

The probe compared native nesting with Margin nesting at package-source SHA
`5010a9750e567a30ab548ea7ae26ef7105841228`. The worktree's additional files were
design and investigation artifacts; package sources were unchanged. It ran on
R 4.6.1 with dplyr 1.2.1, tidyr 1.3.2, dtplyr 1.3.3, data.table 1.18.6.1,
tibble 3.3.1, vctrs 0.7.3, rlang 1.3.0 and pkgload 1.5.3. marginplyr reported
version 0.1.0.

The [R probe](native-nesting-classes-2026-11-10.R) accepted a source directory:

```sh
Rscript investigation/native-nesting-classes-2026-11-10.R .
```

Its [captured output](native-nesting-classes-2026-11-10-output.txt) contained 64
observations: four input representations (plain data.frame, tibble, direct
data.table and immutable dtplyr), four shapes (ordinary payload, zero-column
payload with source rows, keyless empty payload and keyless empty zero-column
input), two nesting operations and native/Margin evaluation. The command
returned exit 0. It executed no package test suite. There were no recorded
warnings.

Native `nest` used `tidyr::nest(input, data = -dplyr::all_of(keys))`, selecting
the payload explicitly. Native `nest_by` used `dplyr::nest_by()`; its dtplyr
reference collected the input before local nesting. Margin calls used the same
keys through `.by`, with no variable dimensions. Lazy `nest` results were
collected to inspect their cells. Recorded outer classes distinguished the
returned lazy step from its collected result.

## Observations

For dtplyr with an ordinary payload, native `nest` and Margin `nest` both
produced data.table inner frames in an ordinary list. The rowwise operation
had a different reference:

| dtplyr rowwise nesting, ordinary payload | Inner frames | Container column |
| --- | --- | --- |
| Collect input, then local `dplyr::nest_by()` | tibble | `vctrs_list_of` / `vctrs_vctr` / list |
| `nest_by_with_margins()` | data.table | list |

Both rowwise outer results had class `rowwise_df` / `tbl_df` / `tbl` /
`data.frame`. Exact reference matching would therefore require changing the
inner representation and list-container representation, not just preserving an
existing input class. No allocation or runtime cost was benchmarked.

For three input rows with keys `a`, `a`, `b` and no payload columns, native
dtplyr `nest` returned two data.table cells, each of size 0 by 0. Margin `nest`
returned two tibble cells of sizes 2 by 0 and 1 by 0. The same Margin fallback
to zero-column tibbles also appeared in rowwise nesting. The native rowwise
reference retained those row counts in tibbles as well.

Thus matching native dtplyr `nest`'s data.table cell representation in this case
would not preserve the source-row count represented by each cell. A class-name
copy would not resolve that structural difference.

For a keyless empty input, native dtplyr `nest` returned one outer row holding
an empty data.table, whereas Margin `nest` returned zero outer rows. Margin
rowwise nesting and its native reference returned one outer row. These were
observations of distinct result-shape rules, not failures of the probe.

## Limits

These were versioned observations, not a promise to retain literal upstream
class names across dependency updates. The probe did not test arbitrary frame
subclasses, custom S3/S4/S7 column methods, factors, Margin-label conversions,
SQL or Arrow, and did not establish implementation of a new class guarantee.
It established the generated-container differences used in the design
interview. A native-reference class contract and an exact generated-container
class contract were separate choices.
