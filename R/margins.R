# nolint start: line_length_linter
#' Grouped operations with margins
#'
#' See below for more details
#' - \url{https://sayuks.github.io/withmargins/}
#'
#' @param .data
#' * For `summarise_with_margins()` and `union_all_with_margins()` :
#'   a data frame or lazy table.
#'    * Lazy tables created by `{arrow}` does not work.
#'    If you want to work with it, it is an easy way to convert to
#'    a duckdb back-end using `arrow::to_duckdb()` in advance.
#' * For `nest_with_margins()` and `nest_by_with_margins()`:
#'   a data frame (not lazy table).
#' @param ... Name-value pairs as used in [dplyr::summarise()].
#' @param .margins <[`tidy-select`][dplyr_tidy_select]> Grouping columns which
#'   margins are calculated, starting from the highest parent of the hierarchy.
#' @param .without_all <[`tidy-select`][dplyr_tidy_select]> Additional group
#'   variables without hierarchy to which `.margin_name` will _NOT_ be added.
#' @param .with_all <[`tidy-select`][dplyr_tidy_select]> Additional group
#'   variables without hierarchy to which `.margin_name` will be added.
#' @param .margin_name A string representing margin name
#'   (Defaults to `"(all)"`). `NA_character_` is also allowed.
#' @param .sort A Logical. If `TRUE`, sort the result by the column order
#'   specified in `.without_all` and `.margins` and `.with_all`.
#'   * `summarise_with_margins()` defaults to `is.data.frame(.data)`,
#'     `union_all_with_margins()` defaults to `FALSE`.
#'     * This is because pipelines using lazy tables should perform the
#'       SQL `ORDER BY` as last as possible.
#'     * As a result of sorting, in the case of lazy tables,
#'       `NA` may come first, unlike R.
#'     * See \href{https://dbplyr.tidyverse.org/reference/arrange.tbl_lazy.html}{`arrange()` documentation of `{dbplyr}`}
#'       for details.
#'   * `nest_with_margins()` and `nest_by_with_margins()` default to `TRUE`.
#' @param .key The name of the resulting nested column.
#'   * For `nest_with_margins()`, passed to `.key` argument of [tidyr::nest()]
#'   * For `nest_by_with_margins()`, passed to `.key` argument
#'     of [dplyr::nest_by()]
#' @param .names_sep Passed to `.names_sep` argument of [tidyr::nest()].
#' @param .keep Should the grouping columns be kept in the list column.
#'   Passed to `.keep` argument of [dplyr::nest_by()].
#' @details
#' __`summarise_with_margins()`__
#' * This is similar to [dplyr::summarise()] but creates an additional
#'   `.margin_name` category for each grouping variable. It assumes a hierarchy
#'   of groups and the higher level groups should be provided first.
#' * Regular groups, not used for totals/subtotals can be provided through
#'   the `.without_all` argument and will be used as parent groups.
#' * If you want to create its own total margin (such as `"(all)"`)
#'   for a variable that is a regular group and has no hierarchy,
#'   specify it with `.with_all`. If there is more than one `.with_all`,
#'   all combinations of them are generated.
#'
#' __`union_all_with_margins()`__
#' * Consider each margin as a new category, duplicate the rows and merge them
#'   vertically (like `UNION ALL` in SQL).
#' * The use of arguments in common with `summarise_with_margins()` is the same
#'   as for it.
#' * __Be aware that the number of rows can be huge.__
#'
#' __`nest_with_margins()`__
#' * Run like `tidyr::nest(<data>, .by = c({{ .without_all }}, {{ .margins }}, {{ .with_all }}))`
#’   on the result of `union_all_with_margins()`.
#' * Only works for a local data frame.
#'
#' __`nest_by_with_margins()`__
#' * Run like `dplyr::nest_by(<data>, dplyr::pick({{ .without_all }} , {{ .margins }} , {{ .with_all }})`
#'   on the result of `union_all_with_margins()`. The result is a row-wise
#'   data frame grouped by row.
#' * Only works for a local data frame.
#'
#' @return A data frame. If `.data` is a lazy table,
#' the output is also a lazy table.
#' * Missing values are kept as missing.
#' * The order of the columns is as follows from left to right:
#' `.without_all`, `.margins`, `.with_all` and the remaining columns.
#' * Column types specified with `.margins` or `.with_all`:
#'    * Columns that are not originally factor or character
#'     (e.g. numeric, integer, logical) are converted to characters.
#'    This is required to add the `.margin_name` category.
#'    * Originally the factor column (assuming a local data frame):
#'      * Remains as factor. (If ordered, remains as ordered).
#'      * The level of the factor depends on the case:
#'        * If `.margin_name` is ___NOT___ a `NA_character` (such as `"(all)"`):
#'            * `.margin_name` is added to the
#'            beginning of the level of the factor.
#'        * If `.margin_name` is a `NA_character`:
#'            * If the column has `<NA>` in the level:
#'                * Error occurs. Because, although the values with level `<NA>`
#'                are not equal to `NA_character_`, they will be identical and
#'                indistinguishable when merging `NA_character_`
#'                as a new category.
#'            * If the column does not have `<NA>` in the level:
#'                * Just keeps the original level of factor.
#'              `<NA>` is not included in the level.
#'              This is consistent with the default
#'              [base::factor()] behavior (`exclude = NA`).
#'
#' @references
#'  * Main idea is borrowed from \href{https://gist.github.com/moodymudskipper/6347418d82fea2160178422aa574dec2}{here}.
#' @export
#' @examples
#' # Make a hierarchical margin for `cyl`, `vs`.
#' summarise_with_margins(
#'   mtcars,
#'   n = dplyr::n(),
#'   mpg = mean(mpg, na.rm = TRUE),
#'   .margins = c(cyl, vs),
#' )
#'
#' # `am` does not create a hierarchy, but is an overall group variable.
#' summarise_with_margins(
#'   mtcars,
#'   n = dplyr::n(),
#'   mpg = mean(mpg, na.rm = TRUE),
#'   .margins = c(cyl, vs),
#'   .without_all = am
#' )
#'
#' # `gear` creates "(all)" margins on itself.
#' summarise_with_margins(
#'   mtcars,
#'   n = dplyr::n(),
#'   mpg = mean(mpg, na.rm = TRUE),
#'   .margins = c(cyl, vs),
#'   .without_all = am,
#'   .with_all = gear
#' )
#'
#' # If `.with_all` has multiple columns, all combinations are generated.
#' summarise_with_margins(
#'   mtcars,
#'   n = dplyr::n(),
#'   mpg = mean(mpg, na.rm = TRUE),
#'   .margins = c(cyl, vs),
#'   .without_all = am,
#'   .with_all = c(gear, carb)
#' )
#'
#' # union_all_with_margins()
#' union_all_with_margins(
#'   mtcars,
#'   .margins = c(cyl, vs),
#'   .without_all = am,
#'   .with_all = gear
#' )
#'
#' # nest_with_margins()
#' nest_with_margins(
#'   mtcars,
#'   .margins = c(cyl, vs),
#'   .without_all = am,
#'   .with_all = gear
#' )
#'
#' #' nest_by_with_margins()
#' nest_by_with_margins(
#'   mtcars,
#'   .margins = c(cyl, vs),
#'   .without_all = am,
#'   .with_all = gear
#' )
# nolint end
summarise_with_margins <- function(.data,
                                   ...,
                                   .margins = NULL,
                                   .without_all = NULL,
                                   .with_all = NULL,
                                   .margin_name = "(all)",
                                   .sort = is.data.frame(.data)) {
  .f <- function(.data, ..., .margin_pairs, .by) {
    dplyr::summarise(
      .data = .data,
      ...,
      !!!.margin_pairs,
      .by = tidyselect::all_of(.by)
    )
  }

  with_margins(
    .data = .data,
    ...,
    .margins = {{ .margins }},
    .without_all = {{ .without_all }},
    .with_all = {{ .with_all }},
    .margin_name = .margin_name,
    .f = .f,
    .sort = .sort
  )
}

#' @export
#' @rdname summarise_with_margins
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

#' @export
#' @rdname summarise_with_margins
nest_with_margins <- function(.data,
                              .margins = NULL,
                              .without_all = NULL,
                              .with_all = NULL,
                              .margin_name = "(all)",
                              .sort = TRUE,
                              .key = NULL,
                              .names_sep = NULL) {
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
      .data,
      dplyr::pick(c({{ .without_all }}, {{ .margins }}, {{ .with_all }}))
    )
  }

  .data
}

#' @export
#' @rdname summarise_with_margins
nest_by_with_margins <- function(.data,
                                 .margins = NULL,
                                 .without_all = NULL,
                                 .with_all = NULL,
                                 .margin_name = "(all)",
                                 .sort = TRUE,
                                 .key = "data",
                                 .keep = FALSE) {
  stopifnot(
    # As of the end of 2023, lazy tables often do not support tidyr::nest()
    ".data must be a data frame (not lazy)" =
      is.data.frame(.data),
    # Allow `NA_character_`
    # since the `.key` argument in dplyr::nest_by()
    # seems to work with `NA_character_`.
    "`.key` must be a character vector of length 1." =
      is.character(.key) && length(.key) == 1,
    # In the `.keep` argument of dplyr::nest_by(),
    # it seems that it is not an error even if it is not logical,
    # but only logical is allowed for safety.
    "`.keep` must be a `TRUE` or `FALSE`." =
      isTRUE(.keep) || isFALSE(.keep)
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

#' Assert whether columns are duplicated in each set
#' @param lst A named list.
#' @noRd
#' @examples
#' l <- list(
#'   g1 = "A",
#'   g2 = c("A", "B"),
#'   g3 = c("A", "B", "D")
#' )
#'
#' try(assert_column_intersect(l))
assert_column_intersect <- function(lst) {
  stopifnot(
    is.list(lst),
    !is.null(names(lst)), # assuming named list
    length(lst) >= 2
  )

  res <- lapply(
    utils::combn(lst, 2, simplify = FALSE),
    function(x) {
      list(
        list_names = c(names(x)[1], names(x)[2]),
        common_vars = intersect(x[[1]], x[[2]])
      )
    }
  )
  # remove elements from the list where the length of common_vars is 0
  res <- Filter(function(x) length(x$common_vars) > 0, res)

  # if success (no intersections), early return
  if (length(res) == 0) {
    return(invisible())
  }

  # modify error message
  msg <- sapply(
    res,
    function(l) {
      # for nice looking
      l <- lapply(l, function(x) paste0("`", x, "`"))

      paste0(
        paste(l$list_names, collapse = ", "),
        ": ",
        paste(l$common_vars, collapse = ", ")
      )
    }
  )

  stop(
    paste(
      "The following pairs have common columns:",
      paste(msg, collapse = "\n"),
      sep = "\n"
    )
  )
}

#' Assert whether margin_name is included in each column element
#' @param data A data frame (lazy or not)
#' @param margin_name A character vector of length 1.
#'   `NA_character_` is allowed.
#' @importFrom rlang .data
#' @noRd
#' @examples
#' d <- data.frame(
#'   x = c(NA_character_, "a"),
#'   y = c("b", NA_character_)
#' )
#'
#' assert_margin_name(d, "all")
#' try(assert_margin_name(d, "a"))
#' try(assert_margin_name(d, NA_character_))
assert_margin_name <- function(data, margin_name) {
  res <- sapply(
    colnames(data),
    function(x) {
      elements <- dplyr::distinct(data, dplyr::pick(tidyselect::all_of(x)))

      # %in% may ignore NA_character_ for lazy table, so separate the cases
      # nolint start: object_usage_lintr
      if (is.na(margin_name)) {
        elements <- dplyr::filter(elements, is.na(.data[[x]]))
      } else {
        elements <- dplyr::filter(elements, .data[[x]] == margin_name)
      }
      # nolint end

      elements <- dplyr::pull(elements)

      length(elements) > 0
    }
  )

  if (!any(res)) {
    return(invisible())
  }

  bad_cols <- paste0("`", names(res[res]), "`", collapse = ", ")

  if (!is.na(margin_name)) {
    margin_name <- paste0('"', margin_name, '"')
  }

  stop(
    margin_name,
    " is not allowed as a margin name because it is already contained in ",
    "the following columns: ",
    bad_cols
  )
}

#' Get variable names from tidy-select
#' @param data A data frame (lazy or not)
#' @param ... <[`tidy-select`][dplyr_tidy_select]>
#' @noRd
#' @references
#' * \url{https://tidyselect.r-lib.org/articles/tidyselect.html}
#' * \url{https://tidyselect.r-lib.org/articles/syntax.html}
#' * \url{https://rlang.r-lib.org/reference/expr.html}
get_col_names <- function(data, ...) {
  x <- tidyselect::eval_select(rlang::expr(c(...)), data)

  names(x)
}

#' Get all subsets
#'
#' Generate 2^n subsets from the input vector of length n.
#' Containing an empty set.
#'
#' @param x A input vector.
#' @param rev A logical. Whether to reverse the order of subset
#' when the length of subset is less than length of `x`.
#' @return A list of vectors. Each element represents one subset
#' @examples
#' x <- c("A", "B", "C")
#' get_all_subsets(x)
#' get_all_subsets(x, rev = TRUE)
#' @noRd
get_all_subsets <- function(x, rev = FALSE) {
  stopifnot(
    is.logical(rev) && length(rev) == 1 && !is.na(rev)
  )

  # Original input length
  n <- length(x)

  # Generate 2^n subsets
  subsets <- lapply(
    0:n,
    function(i) {
      res <- utils::combn(x, i, simplify = FALSE)

      # Use rev() so that totals are calculated from the columns to the left
      # when using in with_margins()
      if (rev) {
        if (0 < i && i < n) {
          res <- rev(res)
        }
      }

      res
    }
  )

  # Flattening the list of subsets
  subsets <- unlist(subsets, recursive = FALSE)

  subsets
}

#' Subset like a hierarchy
#'
#' @param x A vector
#' @return A list. Contains an empty set.
#' @noRd
#' @examples
#' x <- c("A", "B", "C")
#' get_hierarchy(x)
get_hierarchy <- function(x) {
  lapply(0:length(x), function(i) x[0:i])
}

#' @inheritParams summarise_with_margin
#' @importFrom rlang :=
#' @param .f A function that returns data. Arguments are as follows:
#'    * `.data`: input data
#'    * `...`: Name-value pairs. Used with verbs such as
#'      `dplyr::summarise()` etc.
#'    * `.margin_pairs`: Name-value pairs defining margin values.
#'    * `.by`: grouping variables.
#' @noRd
with_margins <- function(.data,
                         ...,
                         .margins = NULL,
                         .without_all = NULL,
                         .with_all = NULL,
                         .margin_name = "(all)",
                         .f,
                         .sort) {
  stopifnot(
    "`.margin_name` must be a character vector of length 1." =
      is.character(.margin_name) && length(.margin_name) == 1,
    "`.f` must be a function." = is.function(.f),
    is.character(.margin_name) && length(.margin_name) == 1,
    "`.sort` must be a `TRUE` or `FALSE`." =
      isTRUE(.sort) || isFALSE(.sort)
  )

  .data <- dplyr::ungroup(.data)

  l <- list(
    .margins = get_col_names(.data, {{ .margins }}),
    .without_all = get_col_names(.data, {{ .without_all }}),
    .with_all = get_col_names(.data, {{ .with_all }})
  )

  # early stop if there are no columns for which margins are calculated
  stopifnot(
    "At least one column must be specified in `.margins` or `.with_all`" =
      length(l$.margins) > 0 || length(l$.with_all) > 0
  )

  # .margins, .without_all and .with_all must not contain common variables
  assert_column_intersect(l)

  # columns where the margin is calculated
  margin_vars_all <- c(l$.margins, l$.with_all)

  factor_cols <- character()

  # if local data frame, get column names of factor in margin_vars_all
  # (lazy tables often do not support factor and tidyselect::where())
  if (is.data.frame(.data)) {
    factor_cols <- get_col_names(
      .data,
      tidyselect::all_of(margin_vars_all) & tidyselect::where(is.factor)
    )
  }

  if (length(factor_cols) > 0) {
    # get factor column information in margin_vars_all as a named list
    factor_info <- lapply(
      dplyr::select(.data = .data, tidyselect::all_of(factor_cols)),
      function(x) {
        list(
          levels = levels(x),
          ordered = is.ordered(x)
        )
      }
    )

    # When .margin_name is NA, if the level of factor has NA and if values with
    # level NA actually exists, it is treated as the same when the factor is
    # reconstructed, so that they are indistinguishable. If the level of the
    # factor is NA and there is no level NA value, this problem does not occur,
    # but to avoid complications, make an error in all cases where .margin_name
    # is NA and the level of the factor has NA.
    if (is.na(.margin_name)) {
      na_level_cols <- names(Filter(function(x) anyNA(x$levels), factor_info))

      if (length(na_level_cols) > 0) {
        stop(
          "If `.margin_name` is a `NA_character_`, ",
          "the following factor columns specified in ",
          "`.margins` or `.with_all` must not contain <NA> in the level: ",
          paste0("`", na_level_cols, "`", collapse = ", ")
        )
      }
    }
  }

  # .margin_name to be added, so convert to string
  .data <- dplyr::mutate(
    .data = .data,
    dplyr::across(tidyselect::all_of(margin_vars_all), as.character)
  )

  # .margin_name must not be included in columns where the margin is calculated
  assert_margin_name(
    dplyr::select(.data = .data, tidyselect::all_of(margin_vars_all)),
    .margin_name
  )

  l_margins <- get_hierarchy(l$.margins)

  l_with_all <- get_all_subsets(l$.with_all, rev = TRUE)

  # append all combinations of two lists
  l_group_vars <- lapply(
    l_margins,
    function(x) lapply(l_with_all, function(y) c(x, y))
  )

  # flatten nested list to single list
  l_group_vars <- unlist(l_group_vars, recursive = FALSE)

  # append .without_all at the beginning
  l_group_vars <- lapply(l_group_vars, function(x) c(l$.without_all, x))

  .data <- purrr::map(
    l_group_vars,
    function(group_vars) {
      margins <- setdiff(margin_vars_all, group_vars)
      margin_pairs <- rep(.margin_name, length(margins))
      names(margin_pairs) <- margins

      # apply the specific function
      .f(.data = .data, ..., .margin_pairs = margin_pairs, .by = group_vars)
    }
  )

  # dplyr::bind_rows doesn't support lazy tables
  .data <- Reduce(dplyr::union_all, .data)

  # reconstruct factors
  if (length(factor_cols) > 0) {
    .data <- purrr::reduce2(
      factor_cols,
      factor_info,
      function(data, col, info) {
        dplyr::mutate(
          .data = data,
          "{col}" := factor( # nolint object_usage_linter
            .data[[col]],
            # force .margin_name to the beginning of the level
            levels = union(.margin_name, info$levels),
            # If .margin_name is not NA and there is NA in the original level,
            # keep it (set exclude = NULL).
            # The case where .margin_name is NA and level contains NA is
            # not present here, as it is an error in the prior step.
            # This means that if .margin_name is NA,
            # anyNA(info$levels) always returns FALSE, so exclude = NA.
            # This excludes NA from the level.
            # This is consistent with the default base::factor().
            exclude = if (anyNA(info$levels)) NULL else NA,
            ordered = info$ordered
          )
        )
      },
      .init = .data
    )
  }

  # relocate group columns to the left
  .data <- dplyr::relocate(
    .data = .data,
    c({{ .without_all }}, {{ .margins }}, {{ .with_all }})
  )

  # for ease of viewing
  if (.sort) {
    .data <- dplyr::arrange(
      .data = .data,
      dplyr::pick(c({{ .without_all }}, {{ .margins }}, {{ .with_all }}))
    )
  }

  .data
}
