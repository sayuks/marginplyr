get_col_names <- function(data, ...) {
  selected <- dplyr::select(.data = data, ...)
  # Drop the grouping metadata attached to dplyr's variable-name vector.
  as.character(dplyr::tbl_vars(selected))
}

grouping_selection_proxy <- function(.data,
                                     backend = grouping_backend(.data)) {
  if (identical(backend$kind, "arrow")) {
    return(as.data.frame(arrow::schema(.data)))
  }
  if (backend$collect_selection_proxy) {
    return(dplyr::collect(utils::head(.data, n = 0L)))
  }
  .data
}

# The selection proxy's columns, as a plain named list.
#
# `[` is not read here, and a data frame subclass is the reason. The public API
# admits any object dplyr can group (#77), so the proxy for a local backend is
# the caller's own object, and a subclass is free to give `[` other semantics:
# `data.table`'s reads a character index as a join key and errors rather than
# selecting columns. `[[` is the operator such a subclass keeps as column
# extraction, and reading one name at a time is also what keeps this from
# constructing an object of the subclass at all -- the list below is what the
# metadata is read from, so no subclass behaviour reaches the rest of this file.
#
# Every name is known to be a column by the time this runs, having been resolved
# against the same data by tidyselect, so a `NULL` from `[[` reports a defect
# rather than anything a caller can rewrite their way out of -- either a proxy
# that does not answer for its own columns, or a subclass whose `[[` is not
# column extraction. It is still worth stopping on, and stopping bare: silence
# here would report the dimension as an absent prototype and label its margin
# rows `NA` instead.
proxy_columns <- function(data_proxy, dimensions) {
  columns <- lapply(dimensions, function(col) data_proxy[[col]])
  names(columns) <- dimensions
  absent <- dimensions[vapply(columns, is.null, logical(1))]
  if (length(absent) > 0L) {
    stop(
      "The selection proxy has no column ",
      paste0("`", absent, "`", collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  columns
}

margin_column_info <- function(data_proxy,
                               dimensions,
                               backend) {
  if (length(dimensions) == 0L) {
    return(list(factors = list(), prototypes = list()))
  }

  if (!backend$can_read_schema) {
    return(list(factors = list(), prototypes = list()))
  }

  schema <- proxy_columns(data_proxy, dimensions)

  prototypes <- lapply(schema, function(x) x[NA_integer_])
  factors <- if (backend$can_restore_factors) {
    lapply(
      names(schema)[vapply(schema, is.factor, logical(1))],
      function(col) {
        x <- schema[[col]]
        list(
          col = col,
          levels = levels(x),
          ordered = is.ordered(x),
          has_na_in_level = anyNA(levels(x)),
          preserve_missing_value = backend$can_encode_factor_missing_values
        )
      }
    )
  } else {
    list()
  }

  list(factors = factors, prototypes = prototypes)
}
