# nolint start: line_length_linter
#' Works like `tidyr::nest()` with margins
#'
#' This function considers each margin (such as total) as a new category,
#' works like [tidyr::nest()].
#' See [Get started](https://sayuks.github.io/withmargins/vignettes/get_started.html)
#' for more details.
#'
#' @inherit union_all_with_margins
#' @inheritParams tidyr::nest
#' @param .data A data frame.
#' @param .sort A Logical (default to `TRUE`). If `TRUE`, sort the result
#'   by the column order specified in `.without_all` and `.margins` and
#'   `.with_all`.
#' @param .key A string. The name of the resulting nested column.
#'   Passed to the `.key` argument of [tidyr::nest()]
#' @details
#' * Works like `tidyr::nest(<data>, .by = c({{ .without_all }}, {{ .margins }}, {{ .with_all }}))`
#'   on the result of [union_all_with_margins()].
#' * Only works for a local data frame.
#' @return A data frame.
#' @family summarize and expand data with margins
#' @export
#' @examples
#' nest_with_margins(
#'   mtcars,
#'   .margins = c(cyl, vs),
#'   .without_all = am,
#'   .with_all = gear
#' )
# nolint end
nest_with_margins <- function(.data,
                              .margins = NULL,
                              .without_all = NULL,
                              .with_all = NULL,
                              .margin_name = "(all)",
                              .sort = TRUE,
                              .key = NULL,
                              .names_sep = NULL) {
  assert_logical_scalar(.sort)
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

  .data <- tidyr::nest(
    .data = .data,
    .by = c({{ .without_all }}, {{ .margins }}, {{ .with_all }}),
    .key = .key,
    .names_sep = .names_sep
  )

  if (.sort) {
    .data <- dplyr::arrange(
      .data = .data,
      dplyr::pick(c({{ .without_all }}, {{ .margins }}, {{ .with_all }}))
    )
  }

  .data
}
