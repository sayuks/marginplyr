# Column-name discovery across dplyr backends

Research date: 2026-07-26

## Conclusion

For `marginplyr`, the best common implementation is:

```r
get_col_names <- function(data, ...) {
  selected <- dplyr::select(.data = data, ...)
  as.character(dplyr::tbl_vars(selected))
}
```

This preserves the current function's tidyselect semantics while removing its
class-specific zero-row `collect()` methods. It works from backend metadata for
local data frames, dbplyr lazy tables, dtplyr steps, and Arrow dplyr queries.
It therefore does not scan rows or materialize a zero-row result.

`tbl_vars()` is exported and documented, and the relevant backends formally
register methods for it. dplyr nevertheless marks it with
`@keywords internal`, so this should remain a small private compatibility seam
with backend tests rather than become part of marginplyr's public API. Within
that constraint, it is more stable than reading backend object fields directly
and more portable than `names()` or `colnames()`.

No package source was changed as part of this investigation.

## Why this fits the existing helper

`get_col_names()` does more than retrieve all names: its `...` accepts
tidyselect expressions. The public `dplyr::select()` generic is designed for a
data frame, a data-frame extension, or a lazy data frame, returns the same
backend type, preserves grouping variables, and defines the selected names and
order. Delegating selection to it preserves the backend's own supported
selection language and error behavior.

- [dplyr `select()` documentation](https://dplyr.tidyverse.org/reference/select.html)
- [dbplyr `select.tbl_lazy()` documentation](https://dbplyr.tidyverse.org/reference/select.tbl_lazy.html)
- [dtplyr `select.dtplyr_step()` documentation](https://dtplyr.tidyverse.org/reference/select.dtplyr_step.html)

After selection, `tbl_vars()` is the common metadata accessor:

- dplyr's data-frame method returns `names(x)`.
- dbplyr's method returns the variables stored in the lazy query.
- dtplyr's method returns the step's recorded variables.
- Arrow registers a method for `arrow_dplyr_query` that returns the names of
  its selected-column expressions.

Primary source:

- [dplyr `tbl_vars()` and its data-frame method](https://github.com/tidyverse/dplyr/blob/v1.2.1/R/tbl.R#L32-L65)
- [dbplyr `tbl_vars.tbl_lazy()`](https://github.com/tidyverse/dbplyr/blob/v2.6.0/R/tbl-lazy.R#L57-L61)
- [dtplyr `tbl_vars.dtplyr_step()`](https://github.com/tidyverse/dtplyr/blob/v1.3.3/R/step.R#L39-L48)
- [Arrow `tbl_vars.arrow_dplyr_query()` and `select()` methods](https://github.com/apache/arrow/blob/apache-arrow-25.0.0/r/R/dplyr-select.R#L18-L30)

`tbl_vars()` returns a character subclass carrying grouping metadata.
`as.character()` deliberately restores the plain character-vector contract of
the current helper.

## Candidate comparison

| Approach | Local data frame | dbplyr `tbl_lazy` | dtplyr step | Arrow Table/Dataset/query | Evaluation cost | Assessment |
|---|---|---|---|---|---|---|
| `names(x)` | Correct | Not a column API | Returns internal step fields | Correct for Arrow objects and queries | Metadata only | Not portable |
| `colnames(x)` | Correct | Correct | `NULL` | Correct for Table/Dataset, but `NULL` for `arrow_dplyr_query` | Metadata only where supported | Not portable after `select()` |
| `dplyr::tbl_vars(dplyr::select(x, ...))` | Correct | Correct | Correct | Correct because `select()` produces an Arrow query | Metadata only | Recommended |
| `tidyselect::eval_select()` directly on `x` | Correct | Supported through a proxy | Supported through a proxy | A raw Table/Dataset is not a supported vector-like input | Metadata only where supported | Useful API, but not one common path here |
| Backend fields/schema (`op_vars`, `x$vars`, `x$schema`) | Possible | Possible | Possible | Possible | Metadata only | Couples marginplyr to backend internals |
| `collect(head(select(x, ...), 0))` | Unnecessary | Executes a remote query | Executes the data.table expression | Executes the Arrow query | Backend execution and conversion | Avoid |

### `names()` and `colnames()`

Base R defines `names()` as a generic accessor for an object's names, not
specifically for table columns. `colnames()` is intended for matrix-like
objects and, for data frames, eventually uses `names()`.

- [R `names()` documentation](https://stat.ethz.ch/R-manual/R-devel/library/base/html/names.html)
- [R `colnames()` documentation](https://stat.ethz.ch/R-manual/R-devel/library/base/help/rownames.html)

That distinction matters for lazy objects. dbplyr explicitly reports that its
`names.tbl_lazy()` method is for internal use and suggests `colnames()`;
`colnames()` works there because `dimnames.tbl_lazy()` reads the lazy query's
stored variables. In contrast, a dtplyr step has a `dim()` method but no
corresponding column-dimnames method, so `colnames()` is `NULL`.

- [dbplyr `dimnames.tbl_lazy()` and `names.tbl_lazy()`](https://github.com/tidyverse/dbplyr/blob/v2.6.0/R/tbl-lazy.R#L31-L38)
- [dbplyr warning from `names.tbl_lazy()`](https://github.com/tidyverse/dbplyr/blob/v2.6.0/R/tbl-lazy.R#L102-L112)
- [dtplyr step metadata methods](https://github.com/tidyverse/dtplyr/blob/v1.3.3/R/step.R#L39-L48)

Arrow exposes efficient `names()` methods for Tables and Datasets, backed by
column names and the schema respectively. It also exposes
`names.arrow_dplyr_query()` for the names after selection or renaming.
However, `colnames()` is not a common solution because an
`arrow_dplyr_query` does not implement column dimnames.

- [Arrow `names.Table()`](https://github.com/apache/arrow/blob/apache-arrow-25.0.0/r/R/table.R#L161-L162)
- [Arrow `names.Dataset()` and `dimnames.Dataset()`](https://github.com/apache/arrow/blob/apache-arrow-25.0.0/r/R/dataset.R#L541-L548)
- [Arrow `names.arrow_dplyr_query()`](https://github.com/apache/arrow/blob/apache-arrow-25.0.0/r/R/dplyr.R#L178-L180)

Consequently, neither base accessor is a portable replacement across all four
families.

### Direct tidyselect evaluation

`tidyselect::eval_select()` is the official package-level API for evaluating a
defused selection and returns a named vector of selected locations. Its `data`
argument is normally a named list, data frame, atomic vector, or another
vector-like object implementing `names()` and `[[`.

- [tidyselect `eval_select()` documentation](https://tidyselect.r-lib.org/reference/eval_select.html)

tidyselect also offers extension generics for data-frame-like proxies.
dbplyr and dtplyr provide such proxies, but Arrow performs selection in its own
`select()` method by constructing a schema-derived simulated data frame.

- [tidyselect proxy API](https://tidyselect.r-lib.org/reference/tidyselect_data_proxy.html)
- [dbplyr's tidyselect proxy](https://github.com/tidyverse/dbplyr/blob/v2.6.0/R/tbl-lazy.R#L86-L97)
- [Arrow's schema-based column selection](https://github.com/apache/arrow/blob/apache-arrow-25.0.0/r/R/dplyr-select.R#L93-L112)

Calling `eval_select()` directly would therefore need an Arrow adapter and
would partially duplicate backend logic. It could also diverge from
`select()` in grouping-column retention and backend-specific limitations.
Using the public `select()` generic first is the more faithful abstraction.

## Query-execution behavior

The proposed path only constructs a selection and reads recorded names:

- dbplyr stores output variables in its lazy-query tree; its accessor reads
  those variables.
- dtplyr stores output variables on each lazy step; its accessor reads that
  vector.
- Arrow's `select()` records selected expressions in an
  `arrow_dplyr_query`; its accessor reads their names.
- a local data frame is selected locally and its names are read normally.

In contrast, `collect()` is explicitly the operation that retrieves a lazy
result into a local object. `head(..., 0)` reduces the number of returned rows,
but wrapping it in `collect()` still asks the backend to execute the query and
convert its result.

- [dplyr `collect()` documentation](https://dplyr.tidyverse.org/reference/compute.html)
- [Arrow Dataset lazy-evaluation documentation](https://arrow.apache.org/docs/r/articles/dataset.html#querying-datasets)
- [Arrow dplyr query execution](https://arrow.apache.org/docs/r/reference/acero.html)

A local 100-call comparison made during this investigation found the
metadata-only proposal approximately 2.2 times faster for dtplyr, 3.2 times
faster for an Arrow Table, and 8.6 times faster for an Arrow Dataset than the
current zero-row collection path. These figures are directional rather than
portable benchmarks; avoiding backend execution is the substantive benefit.

## Compatibility notes

1. Keep `dplyr::select()` rather than evaluating selections independently.
   This retains renamed selections, ordering, grouping-column retention, and
   each backend's documented tidyselect limitations. For example, dbplyr
   documents that predicate selections such as `where(is.numeric)` are not
   supported.
2. Convert `tbl_vars()` with `as.character()` so grouping attributes and its
   internal character subclass do not leak into marginplyr internals.
3. The proposed common implementation makes the five package-specific
   `get_col_names` S3 registrations unnecessary. Removing them should be a
   separate implementation change accompanied by tests for data frames,
   simulated and live dbplyr tables, dtplyr, Arrow Table, Arrow Dataset, and
   post-`select()` Arrow queries.
4. Because Arrow and dtplyr are optional dependencies without minimum versions
   in `DESCRIPTION`, CI should continue to exercise their current CRAN
   releases. If marginplyr intends to support substantially older releases,
   the first release introducing this change should test and document an
   explicit compatibility floor.
