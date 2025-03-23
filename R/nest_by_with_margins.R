# nolint start: line_length_linter
#' Works like `dplyr::nest_by()` with margins
#'
#' This function considers each margin (such as total) as a new category,
#' works like [dplyr::nest_by()].
#' See [Get started](https://sayuks.github.io/withmargins/vignettes/get_started.html)
#' for more details.
#'
#' @inherit nest_with_margins
#' @details
#' * Works like `dplyr::nest_by(<data>, dplyr::pick({{ .without_all }} , {{ .margins }} , {{ .with_all }})`
#'   with margins.
#' * Only works for a local data frame.
#' @return A row-wise data frame grouped by `without_all`, `.margins`
#' and `.with_all`.
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
                                 .key = "data") {
  assert_string_scalar(.margin_name)
  # Allow `NA_character_`
  # since the `.key` argument in dplyr::nest_by()
  # seems to work with `NA_character_`.
  assert_string_scalar(.key)
  assert_logical_scalar(.sort)
  stopifnot(
    # As of the end of 2023, lazy tables often do not support tidyr::nest()
    ".data must be a data frame (not lazy)" =
      is.data.frame(.data)
  )

  .data <- nest_with_margins(
    .data = .data,
    .margins = {{ .margins }},
    .without_all = {{ .without_all }},
    .with_all = {{ .with_all }},
    .margin_name = .margin_name,
    .sort = .sort,
    .key = .key
  )

  dplyr::rowwise(
    data = .data,
    c({{ .without_all }}, {{ .margins }}, {{ .with_all }})
  )

}
