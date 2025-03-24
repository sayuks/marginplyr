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
#' @param .keep A logical. Should the grouping columns be kept in the list
#'   column.
#' @details
#' * Works like `tidyr::nest(<data>, .by = c({{ .without_all }}, {{ .margins }}, {{ .with_all }}))`
#'   with margins.
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
nest_with_margins2 <- function(.data,
                              .margins = NULL,
                              .without_all = NULL,
                              .with_all = NULL,
                              .margin_name = "(all)",
                              .check_margin_name = TRUE,
                              .sort = TRUE,
                              .key = "data",
                              .keep = FALSE) {
  assert_logical_scalar(.check_margin_name)
  assert_logical_scalar(.sort)
  assert_logical_scalar(.keep)
  assert_string_scalar(.margin_name)
  assert_string_scalar(.key)
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
    .check_margin_name = .check_margin_name,
    .sort = .sort
  )

  .data <- dplyr::summarize(
    .data = .data,
    "{.key}" := list(dplyr::pick(dplyr::everything())),
    .by = c({{ .without_all }}, {{ .margins }}, {{ .with_all }})
  )

  if (.keep) {
    .data <- dplyr::mutate(
      .data = dplyr::rowwise(.data),
      "{.key}" := list({
        dplyr::mutate(
          .data =  dplyr::pick(dplyr::all_of(.key))[[1]],
          dplyr::pick(!dplyr::all_of(.key)),
          .before = 1
        )
      })
    )
    .data <- dplyr::ungroup(.data)
  }

  # .data

  # .f <- if (.keep) {
  #   function(.data,  .margin_pairs, .by) {
  #     res <- .f_base(.data = .data, .margin_pairs = .margin_pairs, .by = .by)
  #
  #     res <- dplyr::mutate(
  #       .data = dplyr::rowwise(res),
  #       "{.key}" := list({
  #         d <- dplyr::mutate(
  #           .data =  dplyr::pick(dplyr::all_of(.key))[[1]],
  #           dplyr::pick(!dplyr::all_of(.key))
  #         )
  #
  #         dplyr::relocate(
  #           .data = d,
  #           c({{ .without_all }}, {{ .margins }}, {{ .with_all }})
  #         )
  #       })
  #     )
  #
  #     dplyr::ungroup(res)
  #   }
  # } else {
  #   .f_base
  # }
  #
  # .data <- with_margins(
  #   .data = .data,
  #   .margins = {{ .margins }},
  #   .without_all = {{ .without_all }},
  #   .with_all = {{ .with_all }},
  #   .margin_name = .margin_name,
  #   .check_margin_name = TRUE,
  #   .f = .f,
  #   .sort = .sort
  # )

  .data

}

utils::globalVariables(":=")
