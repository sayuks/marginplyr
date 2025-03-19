# nolint start: line_length_linter
#' Works like `dplyr::nest_by()` with margins
#'
#' This function considers each margin (such as total) as a new category,
#' works like [dplyr::nest_by()].
#' See [Get started](https://sayuks.github.io/withmargins/vignettes/get_started.html)
#' for more details.
#'
#' @inherit union_all_with_margins
#' @inheritParams dplyr::nest_by
#' @param .data A data frame.
#' @param .sort A Logical (default to `TRUE`). If `TRUE`, sort the result
#'   by the column order specified in `.without_all` and `.margins` and
#'   `.with_all`.
#' @details
#' * Works like `dplyr::nest_by(<data>, dplyr::pick({{ .without_all }} , {{ .margins }} , {{ .with_all }})`
#'   on the result of `union_all_with_margins()`. The result is a row-wise
#'   data frame grouped by row.
#' * Only works for a local data frame.
#' @return A data frame.
#' @family summarize and expand data with margins
#' @export
#' @examples
#' nest_by_with_margins(
#'   mtcars,
#'   .margins = c(cyl, vs),
#'   .without_all = am,
#'   .with_all = gear
#' )
# nolint end
nest_by_with_margins <- function(.data,
                                 .margins = NULL,
                                 .without_all = NULL,
                                 .with_all = NULL,
                                 .margin_name = "(all)",
                                 .sort = TRUE,
                                 .key = "data",
                                 .keep = FALSE) {
  assert_string_scalar(.margin_name)
  # Allow `NA_character_`
  # since the `.key` argument in dplyr::nest_by()
  # seems to work with `NA_character_`.
  assert_string_scalar(.key)
  assert_logical_scalar(.sort)
  # In the `.keep` argument of dplyr::nest_by(),
  # it seems that it is not an error even if it is not logical,
  # but only logical is allowed for safety.
  assert_logical_scalar(.keep)
  stopifnot(
    # As of the end of 2023, lazy tables often do not support tidyr::nest()
    ".data must be a data frame (not lazy)" =
      is.data.frame(.data)
  )

  .data <- union_all_with_margins(
    .data = .data,
    .margins = {{ .margins }},
    .without_all = {{ .without_all }},
    .with_all = {{ .with_all }},
    .margin_name = .margin_name,
    # Not sort here;
    # it would be faster to sort after nest,
    # as there are fewer rows.
    .sort = FALSE
  )

  .data <- dplyr::nest_by(
    .data = .data,
    dplyr::pick({{ .without_all }}, {{ .margins }}, {{ .with_all }}),
    .key = .key,
    .keep = .keep
  )

  if (.sort) {
    .data <- dplyr::arrange(
      .data = .data,
      dplyr::pick(c({{ .without_all }}, {{ .margins }}, {{ .with_all }}))
    )
  }

  .data
}
