reconstruct_factor <- function(data, info, .margin_name, position = "last") {
  UseMethod("reconstruct_factor")
}

factor_missing_sentinel <- function(info, .margin_name) {
  sentinel <- "..marginplyr_missing_factor_code"
  reserved <- c(info$levels, .margin_name)
  while (sentinel %in% reserved) {
    sentinel <- paste0(sentinel, "_")
  }
  sentinel
}

encode_factor_for_margin <- function(x,
                                     missing_sentinel,
                                     preserve_missing_value) {
  result <- as.character(x)
  if (!isTRUE(preserve_missing_value)) {
    return(result)
  }

  result[is.na(x)] <- missing_sentinel
  result
}

reconstruct_factor_vector <- function(x,
                                      new_levels,
                                      ordered,
                                      missing_sentinel) {
  missing_value <- !is.na(x) & x == missing_sentinel
  codes <- match(x, new_levels)
  codes[missing_value] <- NA_integer_
  structure(
    codes,
    levels = new_levels,
    class = if (ordered) c("ordered", "factor") else "factor"
  )
}

margin_factor_levels <- function(info, .margin_name, position) {
  other_levels <- info$levels[vapply(
    info$levels,
    function(level) !identical(level, .margin_name),
    logical(1)
  )]
  # The assignment is this function's return value, which codetools reads as a
  # dead store.
  # nolint start: object_usage_linter.
  new_levels <- if (identical(position, "first")) {
    c(.margin_name, other_levels)
  } else {
    c(other_levels, .margin_name)
  }
  # nolint end
}

restore_margin_factors <- function(.data,
                                   factor_info,
                                   margin_labels,
                                   position = "last") {
  if (length(factor_info) == 0L) {
    return(.data)
  }

  Reduce(
    function(data, info) {
      label <- margin_labels[[info$col]]
      if (is_missing_margin_label(label)) {
        return(data)
      }
      reconstruct_factor(data, info, label, position = position)
    },
    factor_info,
    init = .data
  )
}

#' @exportS3Method
#' @noRd
reconstruct_factor.data.frame <- function(data,
                                          info,
                                          .margin_name,
                                          position = "last") {
  col <- info$col
  new_levels <- margin_factor_levels(info, .margin_name, position)
  ord <- info$ordered
  missing_sentinel <- factor_missing_sentinel(info, .margin_name)
  # Every value is injected rather than named. A bare name here resolves
  # against the data mask first, so a source column called `new_levels`,
  # `ord`, or `missing_sentinel` would supply the argument instead of the
  # local, silently rebuilding the factor from the wrong levels.
  dplyr::mutate(
    .data = data,
    "{col}" := !!rlang::expr(
      reconstruct_factor_vector(
        !!margin_column_pronoun(col),
        new_levels = !!new_levels,
        ordered = !!ord,
        missing_sentinel = !!missing_sentinel
      )
    )
  )
}

#' @exportS3Method
#' @noRd
reconstruct_factor.dtplyr_step <- reconstruct_factor.data.frame

# https://github.com/duckdb/duckdb-r/issues/188#issuecomment-2294095426
#' @exportS3Method
#' @noRd
reconstruct_factor.tbl_duckdb_connection <- function(data,
                                                     info,
                                                     .margin_name,
                                                     position = "last") {
  # Both are read only from the glue string below, which codetools cannot see.
  # nolint start: object_usage_linter.
  col <- info$col
  new_levels <- margin_factor_levels(info, .margin_name, position)
  # nolint end
  con <- dbplyr::remote_con(data)
  sql_query <- dbplyr::sql_glue2(
    con,
    "CAST({.id col} AS ENUM {new_levels*})"
  )
  # Injected for the same reason as the data frame method: a source column
  # named `sql_query` would otherwise replace the cast with that column.
  dplyr::mutate(
    .data = data,
    "{col}" := !!sql_query
  )
}
