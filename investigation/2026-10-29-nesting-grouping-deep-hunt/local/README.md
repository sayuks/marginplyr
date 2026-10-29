# Local simultaneous-name and extreme-value nesting audit

Investigated: 2026-10-29
Baseline: `4c63bb917ed7ed31ff6aadf060a5eae7a32cd004` (`main`)

The local track found no new actionable defect in its completed probes. It
read the prior local source-row, ordering, zero-payload, and direct-data.table
coverage in `investigation/2026-10-29-nest-spec-exhaustive/local/README.md`,
and concentrated on simultaneous special names and additional extreme-value
and shape combinations. No implementation or package-test file was changed.

## Completed evidence

| Script | Completed observations |
| --- | --- |
| `simultaneous-names.R` | Seed `29102971`; 1,200 generated inputs/calls: 1,124 successful calls, 76 documented rowwise `.rows` outer-key refusals, and 30,498 matching cell-column source projections |
| `precision-time-shaped.R` | 684 successful calls and 4,494 matching cell-column source projections; three separate factor-matrix grouping controls explained the excluded key roles |

The simultaneous-name sweep selected four to eight source names at once from
ordinary names, data-mask pronouns, `.rows`, data.table special names, `T`/`F`,
R constants, converter argument names, internal-name prefixes, numeric names,
whitespace and Unicode. It independently varied `.key` and `.id`, including
dot-dot output names, while excluding documented name collisions. The fixtures
mixed scalar values, NA-level factors with actual missing codes and a sentinel
spelling, matrices, packed frames, and list values. There were zero to eleven
source rows, up to four fixed/dimension keys, both nesting verbs, both `.keep`
modes, and each `.sort` choice. Every successful result had the expected public
column order and grouping class; rowwise results had the expected identifiers.

The second sweep included integer64 values at positive/negative signed limits
and adjacent values above double precision, POSIXlt values spanning the repeated
New York DST hour, nonfinite doubles and signed zero, partially missing complex
values, factor-matrix payloads with an NA level and a genuinely missing code,
packed factors, and one-dimensional arrays. Except for the independently
unsupported factor-matrix key roles, each shape occupied payload, fixed-key,
and grouping-dimension roles. It crossed empty/one/six-row inputs, both verbs,
both keep modes, and all three sort choices. The list column was named `.rows`
throughout this sweep.

Both scripts grouped the source independently with `vctrs::vec_group_loc()`,
matched cell row identifiers without relying on output order, and compared
cell columns with the original source rows. Integer64 comparisons used their
exact decimal representations, avoiding conversion to double. POSIXlt
comparisons used instants because the independent union control converted the
same input to POSIXct. Source serialization was identical before and after
every public sweep call. The sort settings exercised construction and source
membership; these scripts did not supply an additional independent row-order
oracle.

## Rejected lead and delegation controls

A factor carrying a two-column matrix shape caused
`expressionStackOverflowError` when it was used as a fixed key. The same input
already caused that condition in ordinary `dplyr::group_by()` and directly in
`vctrs::vec_group_loc()`. `precision-time-shaped.R` retained all three controls
and asserted their common `stackOverflowError` ancestry. A factor matrix used
as a payload succeeded and retained the original cells. The key-role refusal
therefore supplied no independently actionable package defect under ADR 0016.

The script also asserted that ordinary `dplyr::union_all()` converted its
POSIXlt fixture to POSIXct. The source instants survived every nesting cell;
class differences on this route were not treated as a new restoration promise.
A preliminary packed-NA-factor probe likewise retained source factor codes,
including the distinction between a declared NA level and a missing code; that
case was subsequently included in the retained structured-factor sweep.

## Replay and limits

Run from the investigated repository root. Both scripts loaded that checkout
with `pkgload::load_all()` and exited zero when their stated assertions passed:

```sh
Rscript investigation/2026-10-29-nesting-grouping-deep-hunt/local/simultaneous-names.R
Rscript investigation/2026-10-29-nesting-grouping-deep-hunt/local/precision-time-shaped.R
```

Adjacent logs recorded the completed runs. The new sweeps used local tibbles;
prior direct-data.table, sf, dtplyr, and ordering observations were read as dated
context and were not counted as runs on this baseline. These finite cases did
not establish arbitrary S3/S4 method behavior, other dependency versions or
operating systems, unbounded plans, large-scale performance, or absence of all
other nesting defects. No ticket was proposed by this track.
