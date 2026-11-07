# dtplyr Nest cell conversion and derived Grouping metadata

Investigated: 2026-11-07

Tested main: `0d99de31aa3c23ac36df363e2a655d1c12c0de89`

Environment: R 4.6.1 on arm64 macOS Tahoe 26.6.2, dplyr 1.2.1, dtplyr 1.3.3,
data.table 1.18.6.1, tidyselect 1.2.1, vctrs 0.7.3, rlang 1.3.0, and
pkgload 1.5.3.

## Caller-local list method changed Nest cell values

The Nest reference describes cells as data frames holding the input's
non-key columns. The converter amendment in
[ADR 0016](../design/adr/0016-delegate-result-class-and-attributes-to-dplyr.md#amendment-the-cell-expression-names-the-converter-and-it-names-the-one-each-backends-own-nesting-gives)
places cell construction on the package side of that boundary.

An immutable dtplyr source held district values `E, E, W` and integer
positions `1, 2, 3`. A local `as.data.table.list` method delegated to the
stock method after replacing the `pos` element of the fresh list with
the same number of `-99L` values. It was installed after constructing
`nest_with_margins()` and before collecting it. The binding was also present
when `nest_by_with_margins()` ran and returned its row-wise result.

| Grouping plan | Districts and Grouping set identifiers | Expected `pos` by cell | Observed `pos` by cell, both verbs | Callbacks per verb |
| --- | --- | --- | --- | --- |
| One included grouping set | `E/1`, `W/1` | `1:2`, `3L` | `c(-99L, -99L)`, `-99L` | 2 |
| Rollup | `E/1`, `W/1`, `NA/2` | `1:2`, `3L`, `1:3` | `c(-99L, -99L)`, `-99L`, `rep(-99L, 3L)` | 3 |

The assertions checked the outer districts, Grouping set identifiers, and
cell column names as well as the integer vectors. The replacement preserved
each cell's row count. The original lazy input still collected `1:3` with
zero callbacks. Removing the method restored the same deferred result and a
fresh row-wise result. Source serialization matched before and after. The
earlier local `length.Date` and `as.character.Date` controls each recorded
zero callbacks on a rollup whose payload also contained a Date column.

In `nest_cell_expr()`, the package-generated call resolved the
`data.table` export through the bound namespace operator. The resolved
`as.data.table()` was still a generic and dispatched on the fresh plain list
in the lazy input's environment.

### Duplicate comparison and disposition

GitHub search covered all states for Issues and PRs, using the terms
`"as.data.table.list"`, `"Nest" "S3"`, and `"Nest" "dispatch"`.
The paginated repository export contained 752 Issues and 711 PRs across all
states. A local scan of titles and bodies matched the exact method name or
Nest combined with caller dispatch, conversion, method, or generated-cell
terms. The relevant matches separated into these mechanisms.

| Existing report | Boundary it covered |
| --- | --- |
| [#1427](https://github.com/sayuks/marginplyr/issues/1427), [PR #1438](https://github.com/sayuks/marginplyr/pull/1438) | The caller replaced the `::` operator used to find generated Nest helpers. This reproduction left the operator intact. |
| [#1238](https://github.com/sayuks/marginplyr/issues/1238), [PR #1259](https://github.com/sayuks/marginplyr/pull/1259) | The caller replaced the generated payload or wrapper's `list()` call. This reproduction used the ordinary list constructor. |
| [#1419](https://github.com/sayuks/marginplyr/issues/1419), [PR #1426](https://github.com/sayuks/marginplyr/pull/1426) | A replacement registered dtplyr-step converter received a zero-row input during Grouping preparation. This reproduction dispatched on a plain list containing populated cell values. |
| [PR #426](https://github.com/sayuks/marginplyr/pull/426), [#947](https://github.com/sayuks/marginplyr/issues/947), [#717](https://github.com/sayuks/marginplyr/issues/717) | Converter formals and special column names. The reproduction used ordinary column names. |
| [#1456](https://github.com/sayuks/marginplyr/issues/1456), [PR #1460](https://github.com/sayuks/marginplyr/pull/1460) | The caller could split rows across cells by affecting the omitted-dimension identity. The reproduction kept the cell counts and outer identities. |

A second fetch before filing returned the tested SHA. One independent
ticket covers both Nest verbs because they share the generated converter
boundary. [Issue #1466](https://github.com/sayuks/marginplyr/issues/1466)

## Derived-step metadata controls

The second candidate was a local function binding intercepting a stock
dtplyr operation on a derived input. The probes used a two-row immutable
source with character `area` and integer `units`. For each operation they
compared typed `inspect_grouping()` with `grouping_set(where(is.integer))`,
both Nest verbs with the same selection, and native collection, before and
after installing the same-named function in the lazy root frame.

| Derived operation | Names tested | Result |
| --- | --- | --- |
| Mutate, rename, relocate | `copy`, `setnames`, `setcolorder` | Four matching outputs and zero callbacks per binding |
| Union all, intersection, difference | `funion`, `fintersect`, `fsetdiff` | Four matching outputs and zero callbacks per binding |

In dtplyr 1.3.3, `dt_eval()` installed the stock bindings for all six
names into its query evaluation environment via `add_dt_wrappers()`.
The six installed functions were not standard S3 generics.
`dtplyr_metadata_safe_step()` separately restricted mutates, call steps,
and set operations to their respective metadata-safe shapes. The checks
found no counterexample for the six bindings. Related reports covered
merge, bracket, registered converter, or generated namespace-operator
dispatch. [#1341](https://github.com/sayuks/marginplyr/issues/1341),
[#1359](https://github.com/sayuks/marginplyr/issues/1359),
[#1418](https://github.com/sayuks/marginplyr/issues/1418),
[#1419](https://github.com/sayuks/marginplyr/issues/1419),
[#1422](https://github.com/sayuks/marginplyr/issues/1422)

Disposition: no ticket for the derived-step candidate.

## Cell-value reproduction

Save the R block to a temporary file and run it from the tested checkout
root with `Rscript`. The default run exits 0 after asserting the observed
corruption and the controls. Pass `--desired` to assert faithful cells and
zero callbacks. That run completed the controls and exited 1 with
`Generated Nest cells must retain the source payload without caller callbacks.`

```r
pkgload::load_all(".", quiet = TRUE)

replay <- function(grouping, keys, sets, expected, desired) {
  source <- data.table::data.table(district = c("E", "E", "W"), pos = 1:3)
  before <- serialize(source, NULL)
  input <- dtplyr::lazy_dt(source, immutable = TRUE)
  collect <- dplyr::collect
  cells <- function(result) {
    order <- match(keys, result$district)
    stopifnot(
      nrow(result) == length(keys), !anyNA(order),
      identical(result$set[order], sets),
      all(vapply(result$data, is.data.frame, logical(1))),
      all(vapply(result$data, function(cell) {
        identical(names(cell), "pos")
      }, logical(1)))
    )
    lapply(result$data[order], function(cell) cell$pos)
  }

  hits <- 0L
  deferred <- nest_with_margins(input, .grouping = grouping,
                                .margin_label = NULL, .id = "set")
  baseline_rowwise <- nest_by_with_margins(input, .grouping = grouping,
                                            .margin_label = NULL, .id = "set")
  stopifnot(
    inherits(deferred, "dtplyr_step"), inherits(baseline_rowwise, "rowwise_df"),
    identical(cells(collect(deferred)), expected),
    identical(cells(baseline_rowwise), expected)
  )
  original <- getS3method("as.data.table", "list",
                          envir = asNamespace("data.table"))
  as.data.table.list <- function(x, ...) {
    if (identical(names(x), "pos")) {
      hits <<- hits + 1L
      x$pos <- rep(-99L, length(x$pos))
    }
    original(x, ...)
  }
  native <- collect(input)
  stopifnot(identical(native$district, source$district),
            identical(native$pos, 1:3), hits == 0L)

  lazy_values <- cells(collect(deferred))
  lazy_hits <- hits
  hits <- 0L
  rowwise <- nest_by_with_margins(input, .grouping = grouping,
                                  .margin_label = NULL, .id = "set")
  stopifnot(inherits(rowwise, "rowwise_df"))
  rowwise_values <- cells(rowwise)
  rowwise_hits <- hits

  rm(as.data.table.list)
  stopifnot(
    identical(cells(collect(deferred)), expected),
    identical(cells(nest_by_with_margins(input, .grouping = grouping,
                                         .margin_label = NULL, .id = "set")),
              expected),
    hits == rowwise_hits, identical(serialize(source, NULL), before)
  )

  print(list(keys = keys, sets = sets, expected = expected,
             lazy = lazy_values, lazy_hits = lazy_hits,
             rowwise = rowwise_values, rowwise_hits = rowwise_hits))
  if (desired) {
    return(identical(lazy_values, expected) && identical(rowwise_values, expected) &&
      lazy_hits == 0L && rowwise_hits == 0L)
  }
  overwritten <- lapply(expected, function(pos) rep(-99L, length(pos)))
  stopifnot(identical(lazy_values, overwritten), identical(rowwise_values, overwritten),
            lazy_hits == length(keys), rowwise_hits == length(keys))
  TRUE
}

desired <- identical(commandArgs(trailingOnly = TRUE), "--desired")
included <- replay(grouping_set(district), c("E", "W"), c(1L, 1L),
                   list(1:2, 3L), desired)
rolled_up <- replay(rollup(district), c("E", "W", NA_character_), c(1L, 1L, 2L),
                    list(1:2, 3L, 1:3), desired)
sessionInfo()
if (desired && !(included && rolled_up)) {
  stop("Generated Nest cells must retain the source payload without caller callbacks.",
       call. = FALSE)
}
```
