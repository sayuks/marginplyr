# nolint start: line_length_linter
#' Works like `tidyr::nest()` with margins
#'
#' This function considers each margin (such as total) as a new category,
#' works like [tidyr::nest()].
#' See [Get started](https://sayuks.github.io/marginplyr/vignettes/get_started.html)
#' for more details.
#'
#' @inherit union_all_with_margins
#' @inheritParams tidyr::nest
#' @param .data A data frame.
#' @param .sort A Logical (default to `TRUE`). If `TRUE`, sort the result
#'   by the column order specified in `.by` and `.rollup` and
#'   `.cube`.
#' @param .key A string. The name of the resulting nested column.
#' @param .keep A logical. Should the grouping columns be kept in the list
#'   column.
#' @details
#' * Works like `tidyr::nest(<data>, .by = c({{ .by }}, {{ .rollup }}, {{ .cube }}))`
#'   with margins.
#' * Only works for a local data frame.
#' @return A data frame.
#' @family summarize and expand data with margins
#' @export
#' @examples
#' nest_with_margins(
#'   mtcars,
#'   .rollup = c(cyl, vs),
#'   .by = am,
#'   .cube = gear
#' )
# nolint end
nest_with_margins <- function(.data,
                              .rollup = NULL,
                              .by = NULL,
                              .cube = NULL,
                              .margin_name = "(all)",
                              .check_margin_name = TRUE,
                              .sort = TRUE,
                              .key = "data",
                              .keep = FALSE) {
  assert_nest_possible(.data)
  assert_logical_scalar(.check_margin_name)
  assert_logical_scalar(.sort)
  assert_logical_scalar(.keep)
  assert_string_scalar(.margin_name)
  assert_string_scalar(.key)
  stopifnot(!is.na(.key))

  l <- list(
    .rollup = get_col_names(.data, {{ .rollup }}),
    .by = get_col_names(.data, {{ .by }}),
    .cube = get_col_names(.data, {{ .cube }})
  )

  margin_cols <- l$.rollup
  without_all_cols <- l$.by
  with_all_cols <- l$.cube

  # early stop if there are no columns for which margins are calculated
  stopifnot(
    "At least one column must be specified in `.rollup` or `.cube`" =
      length(margin_cols) > 0 || length(with_all_cols) > 0
  )

  # .rollup, .by and .cube must not contain common variables
  assert_column_intersect(l)

  group_cols <- c(margin_cols, without_all_cols, with_all_cols)

  if (.key %in% group_cols) {
    stop(
      sprintf("`.key` (%s) ", .key),
      "must not be the same as the column name specified in ",
      "`.rollup`, `.cube`, or `.by`."
    )
  }

  if (.keep) {
    all_cols <- get_col_names(.data, dplyr::everything())

    tmp_cols <- paste0(group_cols, "_COPY__TMP_")

    dup <- tmp_cols[tmp_cols %in% all_cols]

    if (length(dup) > 0) {
      stop(
        sprintf("If `.keep = TRUE`, column name(s) %s ", toString(dup)),
        "will duplicate the column name with temporary column(s) ",
        "created by an internal process. Consider renaming the column(s)."
      )
    }

    .data <- dplyr::mutate(
      .data = .data,
      dplyr::across(
        .cols = c({{ .by }}, {{ .rollup }}, {{ .cube }}),
        .fns = identity,
        .names = "{.col}_COPY__TMP_"
      )
    )

    .f <- function(.data, .margin_pairs, .by) {
      mp <- names(.margin_pairs)
      v1 <- c(.by, mp)
      v2 <- paste0(v1, "_COPY__TMP_")
      names(v2) <- v1

      res <- dplyr::summarize(
        .data =  dplyr::group_by(.data, dplyr::pick(dplyr::all_of(.by))),
        "{.key}" := list({
          d <- dplyr::rename(
            .data = dplyr::pick(!dplyr::all_of(mp)),
            dplyr::all_of(v2)
          )

          d <- dplyr::relocate(
            .data = d,
            c(
              dplyr::all_of(without_all_cols),
              dplyr::all_of(margin_cols),
              dplyr::all_of(with_all_cols)
            )
          )

          dplyr::mutate(
            .data = d,
            !!!.margin_pairs
          )
        }),
        !!!.margin_pairs
      )
      dplyr::ungroup(res)
    }
  } else {
    .f <- function(.data, .margin_pairs, .by) {
      res <- dplyr::summarize(
        .data =  dplyr::group_by(.data, dplyr::pick(dplyr::all_of(.by))),
        "{.key}" := list(dplyr::pick(!dplyr::all_of(names(.margin_pairs)))),
        !!!.margin_pairs
      )
      dplyr::ungroup(res)
    }
  }

  with_margins(
    .data = .data,
    .rollup = {{ .rollup }},
    .by = {{ .by }},
    .cube = {{ .cube }},
    .margin_name = .margin_name,
    .check_margin_name = .check_margin_name,
    .f = .f,
    .sort = .sort
  )
}

utils::globalVariables(":=")
