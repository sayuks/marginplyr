# nolint start: line_length_linter
#' `UNION ALL` with margins
#'
#' This function considers each margin (such as total) as a new category,
#' duplicate the rows and merge them vertically (like `UNION ALL` in SQL).
#' See [Get started](https://sayuks.github.io/withmargins/vignettes/get_started.html)
#' for more details.
#'
#' @inherit summarize_with_margins
#' @details
#' * Consider each margin as a new category, duplicate the rows and merge them
#'   vertically (like `UNION ALL` in SQL).
#' * The use of arguments in common with `summarize_with_margins()` is the same
#'   as for it.
#' * __Be aware that the number of rows can be huge.__
#' @references
#'  * Online documentation: [Get started](https://sayuks.github.io/withmargins/vignettes/get_started.html)
#' @family summarize and expand data with margins
#' @export
#' @examples
#' union_all_with_margins(
#'   mtcars,
#'   .margins = c(cyl, vs),
#'   .without_all = am,
#'   .with_all = gear
#' )
# nolint end
union_all_with_margins <- function(.data,
                                   .margins = NULL,
                                   .without_all = NULL,
                                   .with_all = NULL,
                                   .margin_name = "(all)",
                                   .sort = FALSE) {
  .f <- function(.data, ..., .margin_pairs, .by) {
    dplyr::mutate(.data = .data, !!!.margin_pairs)
  }

  with_margins(
    .data = .data,
    ... = NULL,
    .margins = {{ .margins }},
    .without_all = {{ .without_all }},
    .with_all = {{ .with_all }},
    .margin_name = .margin_name,
    .f = .f,
    .sort = .sort
  )
}
