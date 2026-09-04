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
    proxy <- utils::head(.data, n = 0L)
    record_sent_query("selection_proxy", proxy)
    return(dplyr::collect(proxy))
  }
  .data
}

# The selection proxy's columns, as a plain named list.
#
# `[` is not read here, and a data frame subclass is the reason. The public API
# admits any object dplyr can group (#77), so the proxy for a local backend is
# the caller's own object, and a subclass is free to give `[` other semantics:
# `data.table`'s reads a character index as a join key and errors rather than
# selecting columns. Reading one name at a time is also what keeps this from
# constructing an object of the subclass at all -- the list below is what the
# metadata is read from, so no subclass behaviour reaches the rest of this file.
#
# `[[` is not merely the other base operator. It is the read dplyr itself
# performs on any data frame it accepts -- `dplyr:::pull.data.frame()` is
# `.data[[var]]` -- so routing this through `dplyr::pull()` would reach the same
# operator with a tidyselect resolution on top, and a subclass that redefined it
# would already be failing inside dplyr. #77 admits exactly what dplyr can
# group, so depending on the read dplyr depends on adds no assumption.
#
# That is a boundary rather than a guarantee, and it is worth naming because the
# failure on the wrong side of it is silent. A subclass whose `[[` returned a
# wrong value rather than `NULL` would have that value read as the column's
# levels and prototype, so the Margin label would be added to the wrong factor
# and no diagnostic would say so -- the check below catches only the absent
# case. No detection is available that dplyr does not already need: a column
# read is a column read, and a class breaking it is producing wrong answers in
# the pipeline that grouped it long before reaching here. No such class is
# known, and marginplyr is not the layer that would find one.
#
# Every name is known to be a column by the time this runs, having been resolved
# against the same data by tidyselect, so a `NULL` from `[[` reports a defect
# rather than anything a caller can rewrite their way out of -- either a proxy
# that does not answer for its own columns, or a subclass whose `[[` is not
# column extraction. It is still worth stopping on, and stopping bare: silence
# here would report a column with no levels and no prototype as one the input
# declared that way.
proxy_columns <- function(data_proxy, cols) {
  columns <- stats::setNames(
    lapply(cols, function(col) data_proxy[[col]]),
    cols
  )
  absent <- cols[vapply(columns, is.null, logical(1))]
  if (length(absent) > 0L) {
    stop(
      "The selection proxy has no column",
      if (length(absent) == 1L) " " else "s ",
      paste0("`", absent, "`", collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  columns
}

# `carried` names the input columns that reach the result beside the Margin
# dimensions -- a fixed `.by` key, a column the verb passes through, or one it
# folds into a cell. They cross the same branch union and lose a declared NA
# level in the same place, so `factors` covers them too (#415, #421). Which
# columns those are is settled by `prepare_margin_operation()`'s
# `carried_columns`.
#
# `prototypes` stays keyed to the dimensions alone. It stands for the value a
# branch omitting a dimension writes, and only a dimension is ever omitted.
#
# No query is added. The selection proxy already holds every column for each
# kind that can restore factors, so reading more of it is a second read of the
# one snapshot ADR 0002 acquired rather than a second acquisition.
margin_column_info <- function(data_proxy,
                               dimensions,
                               backend,
                               carried = character()) {
  read <- unique(c(dimensions, carried))
  if (length(read) == 0L) {
    return(list(factors = list(), prototypes = list()))
  }

  if (!backend$can_read_schema) {
    return(list(factors = list(), prototypes = list()))
  }

  schema <- proxy_columns(data_proxy, read)

  prototypes <- lapply(schema[dimensions], function(x) x[NA_integer_])
  factors <- if (backend$can_restore_factors) {
    lapply(
      names(schema)[vapply(schema, is.factor, logical(1))],
      function(col) {
        x <- schema[[col]]
        has_na_in_level <- anyNA(levels(x))
        list(
          col = col,
          levels = levels(x),
          ordered = is.ordered(x),
          has_na_in_level = has_na_in_level,
          preserve_missing_value = backend$can_encode_factor_missing_values,
          # Whether this column takes the encode-and-rebuild route even where
          # its Margin label is missing and so adds no level. All three terms
          # are necessary and none is implied by the others: only an NA level
          # is at risk, only a union that drops one puts it there, and only a
          # sentinel keeps a value on that level apart from the typed missing
          # a margin row carries, which `as.character()` spells the same way
          # (ADR 0012). Settled here so that both sides of the route --
          # `label_margin_branch()` and `restore_margin_factors()` -- read one
          # answer rather than deciding twice, which is what they would do:
          # neither is handed the backend.
          encode_missing_label = has_na_in_level &&
            backend$drops_na_factor_level_on_union &&
            backend$can_encode_factor_missing_values
        )
      }
    )
  } else {
    list()
  }

  list(factors = factors, prototypes = prototypes)
}
