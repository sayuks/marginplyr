# Bug hunt after the five September 26 fixes

Investigated: 2026-09-26
Baseline: `89eba19c630f15c62bf30ffa9b609794e005d25a`

Seven reproducible defects were established after the fixes for #673–#677.
The previous investigation's five intended-result probes all passed at this
baseline. The full testthat suite also passed. No package implementation or
test-suite file was changed during this investigation.

## Reproduction

Run from the repository root:

```sh
Rscript investigation/bug-hunt-2026-09-26-followup.R
```

All seven probes failed their intended-result assertions, and the command
exited with status 1. Select a single probe with `key`, `names`, `source`,
`prior`, `array`, `packed`, or `dtplyr`. Each includes passing controls before
the failing public call or assertion. The script does not alter the package
namespace. These are executable findings, not tests that pass by recognizing
the bugs.

The measured environment was R 4.6.1, dplyr 1.2.1, dbplyr 2.6.0,
tibble 3.3.1, rlang 1.3.0, dtplyr 1.3.3, data.table 1.18.6.1,
RSQLite 3.53.3, DuckDB 1.5.5, Arrow 25.0.1, and testthat 3.3.2.
These versions describe the tested installation, not minimum-version claims.

## 1. P1: a dynamic frame can silently replace an internal grouping key

```r
data <- tibble::tibble(g = c("a", "b"), v = 1:2)
frame <- function(x) {
  stats::setNames(data.frame(sum(x)), "..marginplyr_key_1")
}
summarize_with_margins(data, frame(v), .grouping = grouping_set(g))
```

The result contained only `g = c("1", "2")`: the actual keys `a,b` were lost,
and the requested summary column disappeared. An explicitly named summary
`..marginplyr_key_1 = sum(v)` preserved both columns correctly, establishing
that the spelling itself was not forbidden. A dynamically returned public
column named `g` was correctly refused.

In [summary-selections.R](../R/summary-selections.R),
`wrap_local_frame_summaries()` guarded recognized frame constructors before
dplyr expanded them into its summary mask. An ordinary function returning a
frame did not receive that guard. The branch's post-execution check in
`summarize_margin_union()` in
[grouping-adapter-union.R](../R/grouping-adapter-union.R) excluded the active
internal grouping keys from the output-name check. Its subsequent rename
therefore converted the overwritten internal key back to public `g`.

The same failure was measured with an unnamed `frame(pick(v)[[1]])`, an
unpacked `across()` returning that name, a fixed `.by = g`, and fixed `g`
combined with a rollup of another dimension. Both unsorted and sorted fixed-key
cases were affected. A rollup of `g` alone eventually refused the collision in
the branch where `g` was omitted; that refusal did not protect the one-set or
fixed-key cases.

The public summary contract forbids overwriting grouping keys, including
through unnamed frames.
[ADR 0028](../design/adr/0028-expand-a-data-frame-summary-marginplyr-named.md)
owns runtime output checking. A safe refusal is consistent with that design;
preserving both the original key and the requested summary would also avoid
the public defect. The probe accepts either outcome, so it does not require a
particular repair strategy.

## 2. P2: an unrelated share causes `.names` to execute twice

A single-row, single-set call used an ordinary `across(v, sum,
.names = name())` before `ratio = share_of_total(total)`. The callback
incremented a counter and returned `out1`, then `out2`. Ordinary dplyr and
the same Margin call with `ratio = 1` each ran it once and produced `out1`.
Adding the share ran it twice and produced `out2`. A subsequent summary reading
the counter also changed from 1 to 2.

`analyze_ordinary_summaries()` in [share.R](../R/share.R) used
`known_across_output_names()` for ordinary selections before the last share.
The latter evaluated `parsed$names` with `rlang::eval_tidy()` in
[summary-selections.R](../R/summary-selections.R). The original callback then
remained for actual summary evaluation. No additional grouping set or repeated
summary function explained the extra evaluation.

This was a distinct remaining evaluation path from the `.unpack` callback
fixed by #675. The `.unpack` reproduction from the previous investigation
passed. The missing dimension was an ordinary `.names` expression combined
with a later, unrelated contextual share.

## 3. P2: unnamed frames bypass share-source uniqueness after #674

```r
data <- tibble::tibble(g = c("a", "b"), v = c(1, 3))
summarize_with_margins(
  data, total = sum(v), tibble::tibble(total = max(v)),
  p = share_of_total(total), .grouping = rollup(g)
)
```

The call returned `total = c(1, 3, 3)` and `p = c(1/3, 1, 1)` without refusal.
Writing the second summary as `total = max(v)` instead correctly raised the
Package condition that a source must be defined exactly once. The public
*Eligible source summaries* contract in [share.R](../R/share.R) explicitly
refused overwritten names and expanded-frame sources; this was not ordinary
dplyr overwrite behavior the share API had agreed to accept.

The frame could occur before or after the share. An ordinary function returning
the frame and `across(..., .unpack = "{inner}")` also bypassed the check.
`analyze_ordinary_summaries()` classified constructor argument names as
`frame_candidate`; `confirmed_share_records()` excluded those records when
validating the source. The actual expanded names were not used to reinstate
the uniqueness check after evaluation.

Separate R processes loaded a clean git archive of
`72b01832f88bf59f626c18a9100e99d7596d4cdd` (`f632034^`, before the #674
frame-name change). That archive correctly refused the literal-tibble example;
the investigated baseline accepted it. This established a regression across
the #674 changes, without identifying the first individual commit for every
dynamic variant.

## 4. P2: a later share prevents ordinary `across()` selecting a prior summary

```r
summarize_with_margins(
  data.frame(v = 1L), total = sum(v),
  dplyr::across(total, identity, .names = "copy_{.col}"),
  p = share_of_total(total), .grouping = grouping_set()
)
# Can't select columns that don't exist.
# Column `total` doesn't exist.
```

Ordinary dplyr and the identical Margin call with `p = 1` both returned
`total`, `copy_total`, and `p`. Keeping the share but replacing the bare
`total` selector with `starts_with("tot")` also returned those expected values.
The prior-summary selection was an ordinary `across()`, not the contextual
share's source-selection shorthand.

The public local-selection contract in
[summarize_with_margins.R](../R/summarize_with_margins.R) specifically permitted
ordinary local selections to see preceding ordinary outputs. The pre-analysis
called `across_output_provenance()` in [share.R](../R/share.R), which called
`known_across_source_names()` against the original input proxy without the
out-of-bounds deferral used earlier in the same analysis. `total` existed only
in the later evaluation mask, so the planning read raised before that mask
could be used.

The clean pre-#674 archive also failed findings 1, 2, and 4. Those three were
older holes exposed by this investigation, not established regressions of the
five latest fixes.

## 5. P2: packed matrix keys reverse wholly-missing row placement

```r
data <- tibble::tibble(
  g = tibble::tibble(
    m = matrix(c(NA_real_, NA_real_, NA_real_, 1), nrow = 2L),
    a = c(NA_real_, NA_real_)
  ),
  value = 1:2
)
expand_with_margins(data, .by = g, .sort = "last")$value
# 1 2; expected 2 1
```

The first row's key was wholly missing; the second was partly missing. The
Margin result placed the wholly-missing row first. Ordinary `dplyr::arrange()`
and a Margin control that flattened `m` into two scalar fields both returned
the expected order `2,1`.

`margin_missing_last_expr()` in [margin-operation.R](../R/margin-operation.R)
generated `rowSums(is.na(g)) == ncol(g)`. Here `is.na(g)` had three scalar
columns, while `ncol(g)` counted two packed fields. The expression therefore
reported `FALSE,TRUE`, exactly reversing the two rows' whole-key missingness.

[ADR 0018](../design/adr/0018-order-margin-results-by-grouping-structure.md),
*structured local keys have rowwise missingness*, required every scalar
component to be missing. The fixed-key reproduction avoided Margin label
conversion entirely. All four verbs with fixed `.by` and both `first` and
`last` ordering reproduced the defect (eight combinations).

## 6. P2: array fixed keys fail only when Margin order is requested

```r
data <- tibble::tibble(
  g = array(c(2L, 1L), dim = c(2L, 1L, 1L)), value = 1:2
)
expand_with_margins(data, .by = g, .sort = "last")
# `condition` must be a logical vector, not a logical 3D array.
```

Ordinary `dplyr::arrange(data, g)` succeeded, ordering `value` as `2,1`.
Changing only the key to its two-dimensional matrix equivalent made the Margin
call succeed. Unsorted Margin calls also accepted the array key.

`margin_structured_sort_columns()` in
[margin-operation.R](../R/margin-operation.R) recognized data-frame and matrix
columns only. A higher-dimensional array therefore reached the scalar
`if_else(is.na(g), ...)` path, which rejected it. The fixed-key case required
neither an omitted dimension nor display-label conversion. All four verbs and
both order choices reproduced the failure (eight combinations).

The explicit structured-key amendment named packed frames and matrices, not
all arrays. The narrower evidence here was that an otherwise accepted local
fixed key, with a working ordinary dplyr comparator, failed in the package's
added missingness expression. No claim was made that array display-label
conversion had a separately specified shape contract.

## 7. P2: dtplyr expansion introduces a `.N` collision while adding labels

```r
source <- dtplyr::lazy_dt(tibble::tibble(g = c("x", "y"), .N = c(7, 9)))
dplyr::collect(expand_with_margins(source, .grouping = rollup(g)))
# cannot change value of locked binding for '.N'
```

The `.N` column was unrelated to grouping or any expression the caller wrote.
An ordinary dtplyr `union_all(source, mutate(source, g = "Total"))` returned
the correct four rows and preserved `.N = c(7,9,7,9)`. Local expansion and the
same lazy expansion with a typed-missing label also succeeded.

`label_margin_branch()` in [margin-label.R](../R/margin-label.R) introduced
`rep(label, dplyr::n())`. dtplyr translated `n()` into its special `.N`
binding, which conflicted with the input column. Direct dtplyr with that same
generated expression reproduced the error; direct scalar label replacement
and a full-size `rep(label, length(g))` control did not. That isolated the
package-introduced count expression rather than union or final sorting.

Empty, one-row, and two-row character inputs all failed. A factor dimension
with an explicit NA level also failed with a NULL label, through the analogous
count expression used for its missing-value sentinel. Both the original input
and the lazy source remained unchanged after failure.

The expression predated #677; no recent introduction was established. This
refined the previous investigation's classification of `.N` as a general
upstream input restriction: the equivalent expansion was supported, and the
failure depended on a count operation marginplyr added. It remained separate
from the hidden-ID projection bug #677 fixed.

## Breadth, independent checks, and limits

The local summary and grouping investigators independently reran each other's
findings; the summary investigator also reran the dtplyr reproduction. All
seven combined probes were then rerun in one process from the delivered
script. The safe-key probe deliberately accepted either a collision refusal
or an intact result after independent scrutiny of its original assertion.

The exploration also covered:

- **Full existing suite:** `Rscript -e 'Sys.setenv(NOT_CRAN="true");
  testthat::test_local(reporter="summary")'` exited 0, with no test failures
  or skips reported. This did not validate the missing combinations above.
- **Previous findings:** `Rscript investigation/bug-hunt-2026-09-26.R`
  passed all five probes.
- **dtplyr names:** 600 combinations of unusual names, key/output positions,
  ordering, and contextual shares. Comparison operators, backticks, and
  `.BY`/`BY` coexistence did not reproduce the previously fixed defects.
- **dtplyr types:** 240 cases including factors, ordered factors, Date,
  POSIXct, empty inputs, and shares. Other differences were separated using
  direct dtplyr controls: special-variable lookup, unsupported list/raw
  grouping, and NA/NaN ordering were not promoted to additional findings.
- **Local structured keys:** 16 types, empty/populated input, three order
  choices, and four verbs (384 combinations). The 16 array cases requesting
  order failed; the other cases succeeded. Nesting with `.keep = TRUE`
  additionally checked that nested data matched original rows. The later
  packed-matrix fixture exposed finding 5 outside that initial matrix.
- **SQLite destination safety:** a temporary overwrite with a same-named
  attached-only table preserved that table and its sentinel value.
- **SQLite materialization:** 1,152 direct-versus-materialized comparisons
  matched the selected fixed key, dimension, integer identifier, and Total
  share columns in both values and types. The matrix crossed four input types
  (integer, double, character, logical), empty/populated input, fixed-key
  presence, three label choices, three order choices, and one-set, rollup, and
  repeated-set plans. It exercised summary, expansion, and eligible Total
  share calls. Outer data-frame classes were normalized; ordinary aggregate
  and expansion payload columns were outside this comparison.
- **SQLite finite collection:** the 768 sorted cases each matched the full
  result's selected-column value prefix at `n = 0`, `1`, and `2` (2,304
  comparisons). The initial whole-result comparison also encountered ordinary
  aggregate `z` changing its inferred empty type at `n = 0`; ADR 0031 did not
  promise a type for such aggregate outputs. Restricting the probe to its
  promised columns removed those mismatches. This finite-prefix experiment
  did not establish exact equality of every column's R type or metadata.
- **Agent context budget:** the verifier passed at 20,008 of 22,005 bytes.

The findings did not rely on simulated backend behavior. No live PostgreSQL
server, other operating system,
or alternate dependency version was exercised. Existing-suite coverage of
Arrow and DuckDB was not an independent new exhaustive exploration of those
backends.

No strict line-coverage run, source-tarball check, or website rendering was
required for these investigation-only artifacts, which `.Rbuildignore`
excluded from the package. Package fixes and their regression tests were
outside this bug-hunt deliverable and remained to be implemented.
