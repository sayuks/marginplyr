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
nest_with_margins <- function(.data,
                              .margins = NULL,
                              .without_all = NULL,
                              .with_all = NULL,
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
    .margins = get_col_names(.data, {{ .margins }}),
    .without_all = get_col_names(.data, {{ .without_all }}),
    .with_all = get_col_names(.data, {{ .with_all }})
  )

  margin_cols <- l$.margins
  without_all_cols <- l$.without_all
  with_all_cols <- l$.with_all

  # early stop if there are no columns for which margins are calculated
  stopifnot(
    "At least one column must be specified in `.margins` or `.with_all`" =
      length(margin_cols) > 0 || length(with_all_cols) > 0
  )

  # .margins, .without_all and .with_all must not contain common variables
  assert_column_intersect(l)

  group_cols <- c(margin_cols, without_all_cols, with_all_cols)

  if (.key %in% group_cols) {
    stop(
      sprintf("`.key` (%s) ", .key),
      "must not be the same as the column name specified in ",
      "`.margins`, `.with_all`, or `.without_all`."
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
        .cols = c({{ .without_all }}, {{ .margins }}, {{ .with_all }}),
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
            c({{ .without_all }}, {{ .margins }}, {{ .with_all }})
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
    .margins = {{ .margins }},
    .without_all = {{ .without_all }},
    .with_all = {{ .with_all }},
    .margin_name = .margin_name,
    .check_margin_name = .check_margin_name,
    .f = .f,
    .sort = .sort
  )
}

utils::globalVariables(":=")
