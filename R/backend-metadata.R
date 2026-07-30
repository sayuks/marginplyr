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

margin_column_info <- function(data_proxy,
                               dimensions,
                               backend) {
  if (length(dimensions) == 0L) {
    return(list(factors = list(), prototypes = list()))
  }

  if (!backend$can_read_schema) {
    return(list(factors = list(), prototypes = list()))
  }

  schema <- data_proxy[dimensions]

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
