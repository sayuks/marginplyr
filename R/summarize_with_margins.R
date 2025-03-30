# nolint start: line_length_linter
#' Works like `dplyr::summarize()` with margins
#'
#' This function considers each margin (such as total) as a new category,
#' works like [dplyr::summarize()].
#' See [Get started](https://sayuks.github.io/withmargins/vignettes/get_started.html)
#' for more details.
#'
#' @param .data A data frame or lazy table.
#'   * Lazy tables created by `{arrow}` does not work.
#'     If you want to work with it, it is an easy way to convert to
#'     a duckdb back-end using `arrow::to_duckdb()` in advance.
#' @param ... Name-value pairs as used in [dplyr::summarize()].
#' @param .margins <[`tidy-select`][dplyr_tidy_select]> Grouping columns which
#'   margins are calculated, starting from the highest parent of the hierarchy.
#' @param .without_all <[`tidy-select`][dplyr_tidy_select]> Additional group
#'   variables without hierarchy to which `.margin_name` will _NOT_ be added.
#' @param .with_all <[`tidy-select`][dplyr_tidy_select]> Additional group
#'   variables without hierarchy to which `.margin_name` will be added.
#' @param .margin_name A string representing margin name
#'   (Defaults to `"(all)"`). `NA_character_` is also allowed.
#' @param .check_margin_name A logical (defaults to `is.data.frame(.data)`).
#'   If `TRUE`, checks whether the string in `.margin_name' already exists
#'   in the column specified by `.margins` or `.with_all` and returns an error
#'   if it already exists.
#' @param .sort A logical (defaults to `is.data.frame(.data)`).
#'   If `TRUE`, sort the result by the column order
#'   specified in `.without_all` and `.margins` and `.with_all`.
#'   This is because pipelines using lazy tables should perform the
#'   SQL `ORDER BY` as last as possible.
#'   As a result of sorting, in the case of lazy tables,
#'   `NA` may come first, unlike R.
#'   See [`arrange()` documentation of `{dbplyr}`](https://dbplyr.tidyverse.org/reference/arrange.tbl_lazy.html)
#'   for details.
#' @details
#' * This is similar to [dplyr::summarize()] but creates an additional
#'   `.margin_name` category for each grouping variable. It assumes a hierarchy
#'   of groups and the higher level groups should be provided first.
#' * Regular groups, not used for totals/subtotals can be provided through
#'   the `.without_all` argument and will be used as parent groups.
#' * If you want to create its own total margin (such as `"(all)"`)
#'   for a variable that is a regular group and has no hierarchy,
#'   specify it with `.with_all`. If there is more than one `.with_all`,
#'   all combinations of them are generated.
#'
#' @return A data frame. If `.data` is a lazy table,
#'   the output is also a lazy table.
#' @section About returned data:
#'   * Missing values are kept as missing.
#'   * The order of the columns is as follows from left to right:
#'   `.without_all`, `.margins`, `.with_all` and the remaining columns.
#'   * Column types specified with `.margins` or `.with_all`:
#'      * Columns that are not originally factor or character
#'       (e.g. numeric, integer, logical, .etc) are converted to characters.
#'      This is required to add the `.margin_name` category.
#'      * Originally the factor column (assuming a local data frame):
#'        * Remains as factor. (If ordered, remains as ordered).
#'        * The level of the factor depends on the case:
#'          * If `.margin_name` is ___NOT___ a `NA_character`
#'            (such as `"(all)"`):
#'              * `.margin_name` is added to the
#'              beginning of the level of the factor.
#'          * If `.margin_name` is a `NA_character`:
#'              * If the column has `<NA>` in the level:
#'                  * Error occurs. Because, although the values with level
#'                    `<NA>` are not equal to `NA_character_`, they will be
#'                    identical and indistinguishable when merging
#'                    `NA_character_` as a new category.
#'              * If the column does not have `<NA>` in the level:
#'                  * Just keeps the original level of factor.
#'                `<NA>` is not included in the level.
#'                This is consistent with the default
#'                [factor()] behavior (`exclude = NA`).
#' @references
#'  * Online documentation: [Get started](https://sayuks.github.io/withmargins/vignettes/get_started.html)
#'  * Main idea is borrowed from [this gist](https://gist.github.com/moodymudskipper/6347418d82fea2160178422aa574dec2).
#' @family summarize and expand data with margins
#' @export
#' @examples
#' # Make a hierarchical margin for `cyl`, `vs`.
#' summarize_with_margins(
#'   mtcars,
#'   n = dplyr::n(),
#'   mpg = mean(mpg, na.rm = TRUE),
#'   .margins = c(cyl, vs),
#' )
#'
#' # `am` does not create a hierarchy, but is an overall group variable.
#' summarize_with_margins(
#'   mtcars,
#'   n = dplyr::n(),
#'   mpg = mean(mpg, na.rm = TRUE),
#'   .margins = c(cyl, vs),
#'   .without_all = am
#' )
#'
#' # `gear` creates "(all)" margins on itself.
#' summarize_with_margins(
#'   mtcars,
#'   n = dplyr::n(),
#'   mpg = mean(mpg, na.rm = TRUE),
#'   .margins = c(cyl, vs),
#'   .without_all = am,
#'   .with_all = gear
#' )
#'
#' # If `.with_all` has multiple columns, all combinations are generated.
#' summarize_with_margins(
#'   mtcars,
#'   n = dplyr::n(),
#'   mpg = mean(mpg, na.rm = TRUE),
#'   .margins = c(cyl, vs),
#'   .without_all = am,
#'   .with_all = c(gear, carb)
#' )
# nolint end
summarize_with_margins <- function(.data,
                                   ...,
                                   .margins = NULL,
                                   .without_all = NULL,
                                   .with_all = NULL,
                                   .margin_name = "(all)",
                                   .check_margin_name = is.data.frame(.data),
                                   .sort = is.data.frame(.data)) {
  assert_lazy_table(.data)
  assert_logical_scalar(.check_margin_name)
  assert_logical_scalar(.sort)
  assert_string_scalar(.margin_name)

  with_margins(
    .data = .data,
    ...,
    .margins = {{ .margins }},
    .without_all = {{ .without_all }},
    .with_all = {{ .with_all }},
    .margin_name = .margin_name,
    .check_margin_name = .check_margin_name,
    .f = summarize_impl,
    .sort = .sort
  )
}

summarize_impl <- function(.data,
                           ...,
                           .margin_pairs,
                           .by) {
  UseMethod("summarize_impl")
}

#' @method summarize_impl default
summarize_impl.default <- function(.data,
                                   ...,
                                   .margin_pairs,
                                   .by) {
  # Do not use `.by` because it is experimental
  res <- dplyr::summarize(
    .data = dplyr::group_by(.data, dplyr::pick(dplyr::all_of(.by))),
    ...,
    !!!.margin_pairs,
  )
  dplyr::ungroup(res)
}

#' @method summarize_impl arrow_dplyr_query
summarize_impl.arrow_dplyr_query <- function(.data,
                                             ...,
                                             .margin_pairs,
                                             .by) {
  res <- dplyr::summarize(
    # dplyr::pick() doesn't support arrow tables
    .data = dplyr::group_by(.data, dplyr::across(dplyr::all_of(.by))),
    ...,
    !!!.margin_pairs,
  )
  dplyr::ungroup(res)
}

summarize_impl.ArrowTabular <- summarize_impl.arrow_dplyr_query
summarize_impl.Dataset <- summarize_impl.arrow_dplyr_query

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
assert_margin_name <- function(data, col_names, margin_name) {
  assert_string_scalar(margin_name)
  stopifnot(
    is.character(col_names) && !anyNA(col_names)
  )
  data <- dplyr::select(.data = data, dplyr::all_of(col_names))
  res <- sapply(
    col_names,
    function(x) {

      # %in% may ignore NA_character_ for lazy table, so separate the cases
      elements <- if (is.na(margin_name)) {
        dplyr::filter(data, is.na(.data[[x]]))
      } else {
        dplyr::filter(data, .data[[x]] == margin_name)
      }

      elements <- dplyr::summarize(
        .data = elements,
        n = dplyr::n()
      )
      elements <- dplyr::collect(elements)
      elements$n[[1]] > 0
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
  UseMethod("get_col_names")
}

#' @method get_col_names default
get_col_names.default <- function(data, ...) {
  colnames(dplyr::select(.data = data, ...))
}

# see ?dplyr::compute() for details.
#' @method get_col_names arrow_dplyr_query
get_col_names.arrow_dplyr_query <- function(data, ...) {
  data <- dplyr::select(.data = data, ...)
  # dplyr::compute() also works.　However, it creates a temporary table.
  # It would be less expensive  to collect the data with zero rows.
  # dplyr::filter(.data = data, FALSE) will occur an error:
  # filter expressions must be either an expression or a list of expressions
  data <- utils::head(x = data, n = 0)
  data <- dplyr::collect(data)
  colnames(data)
}

get_col_names.ArrowTabular <- get_col_names.arrow_dplyr_query
get_col_names.Dataset <- get_col_names.arrow_dplyr_query

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
  assert_logical_scalar(rev)

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

#' @inheritParams summarize_with_margins
#' @param .f A function that returns data. Arguments are as follows:
#'    * `.data`: input data
#'    * `...`: Name-value pairs. Used with verbs such as
#'      `dplyr::summarize()` etc.
#'    * `.margin_pairs`: Name-value pairs defining margin values.
#'    * `.by`: grouping variables.
#' @noRd
with_margins <- function(.data,
                         ...,
                         .margins = NULL,
                         .without_all = NULL,
                         .with_all = NULL,
                         .margin_name = "(all)",
                         .check_margin_name,
                         .f,
                         .sort) {
  assert_string_scalar(.margin_name)
  assert_logical_scalar(.check_margin_name)
  assert_logical_scalar(.sort)
  stopifnot("`.f` must be a function." = is.function(.f))

  .data <- dplyr::ungroup(.data)

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

  # columns where the margin is calculated
  margin_vars_all <- c(margin_cols, with_all_cols)

  # if local data frame, get column names of factor in margin_vars_all
  # (lazy tables often do not support factor and dplyr::where())
  factor_cols <-
    if (inherits(.data, c("data.frame", "tbl_duckdb_connection"))) {
      tmp_df <- dplyr::collect(utils::head(x = data, n = 0))
      get_col_names(
        tmp_df,
        dplyr::all_of(margin_vars_all) & dplyr::where(is.factor)
      )
    } else {
      character()
    }

  if (inherits(.data, "tbl_duckdb_connection") &&
        is.na(.margin_name) &&
        length(factor_cols) > 0) {
    stop(
      paste0(
        "If `.data` is a tbl_duckdb_connection class object and ",
        "the columns that specified in `.margins` or `.with_all` contains ",
        "column(s) of type factor",
        sprintf("(%s), ", toString(factor_cols)),
        "`.margin_name` must not be a missing value. ",
        "The reason is that duckdb does not allow NAs to be registered ",
        "at the level of factor."
      )

    )
  }

  if (length(factor_cols) > 0) {
    # get factor column information in margin_vars_all as a named list
    factor_info <- lapply(
      factor_cols,
      function(col) {
        x <- tmp_df[[col]]
        lvl <- levels(x)
        list(
          col = col,
          levels = lvl,
          ordered = is.ordered(x),
          has_na_in_level = anyNA(lvl)
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
      na_level_cols <- Filter(function(x) x$has_na_in_level, factor_info)
      na_level_cols <- vapply(na_level_cols, function(x) x$col, character(1))

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
    dplyr::across(dplyr::all_of(margin_vars_all), as.character)
  )

  # .margin_name must not be included in columns where the margin is calculated
  if (.check_margin_name) {
    assert_margin_name(
      .data,
      margin_vars_all,
      .margin_name
    )
  }

  l_margins <- get_hierarchy(margin_cols)

  l_with_all <- get_all_subsets(with_all_cols, rev = TRUE)

  # append all combinations of two lists
  l_group_vars <- lapply(
    l_margins,
    function(x) lapply(l_with_all, function(y) c(x, y))
  )

  # flatten nested list to single list
  l_group_vars <- unlist(l_group_vars, recursive = FALSE)

  # append .without_all at the beginning
  l_group_vars <- lapply(l_group_vars, function(x) c(without_all_cols, x))

  .data <- lapply(
    l_group_vars,
    function(group_vars) {
      margins <- setdiff(margin_vars_all, group_vars)
      margin_pairs <- rep(.margin_name, length(margins))
      names(margin_pairs) <- margins

      # apply the specific function
      res <- .f(
        .data = .data,
        ...,
        .margin_pairs = margin_pairs,
        .by = group_vars
      )

      relocate_before_union_all(
        res,
        c(without_all_cols, margin_cols, with_all_cols)
      )
    }
  )

  # dplyr::bind_rows() doesn't support lazy tables
  .data <- Reduce(dplyr::union_all, .data)

  # reconstruct factors
  if (length(factor_cols) > 0) {
    .data <- Reduce(
      function(data, info) {
        reconstruct_factor(data, info, .margin_name)
      },
      x = factor_info,
      init = .data
    )
  }

  # relocate group columns to the left
  # dplyr::relocate() doesn't work well with arrow tables
  .data <- relocate_post_proc(
    .data,
    {{ .without_all }}, {{ .margins }}, {{ .with_all }}
  )

  # for ease of viewing
  if (.sort) {
    .data <- arrange_impl(
      .data,
      {{ .without_all }},
      {{ .margins }},
      {{ .with_all }}
    )
  }

  .data
}

relocate_before_union_all <- function(.data, cols_first) {
  UseMethod("relocate_before_union_all")
}

# return as is
#' @method relocate_before_union_all default
relocate_before_union_all.default <- function(.data, cols_first) {
  .data
}

#' @method relocate_before_union_all arrow_dplyr_query
relocate_before_union_all.arrow_dplyr_query <- function(data, cols_first) {
  # Relocate group columns to the left.
  # Arrow tables causes an error in dplyr::union_all()
  # if the columns are not in the same order.
  # dplyr::relocate() doesn't work well with arrow tables.
  dplyr::select(
    .data = data,
    dplyr::all_of(cols_first),
    dplyr::everything()
  )
}

# nolint start: line_length_linter
relocate_before_union_all.ArrowTabular <- relocate_before_union_all.arrow_dplyr_query
relocate_before_union_all.Dataset <- relocate_before_union_all.arrow_dplyr_query
# nolint end

relocate_post_proc <- function(.data, ...) {
  UseMethod("relocate_post_proc")
}

#' @method relocate_post_proc default
relocate_post_proc.default <- function(.data, ...) {
  dplyr::relocate(
    .data = .data,
    ...
  )
}

# return data as is
#' @method relocate_post_proc arrow_dplyr_query
relocate_post_proc.arrow_dplyr_query <- function(.data, ...) {
  .data
}

relocate_post_proc.ArrowTabular <- relocate_post_proc.arrow_dplyr_query
relocate_post_proc.Dataset <- relocate_post_proc.arrow_dplyr_query

arrange_impl <- function(.data, ...) {
  UseMethod("arrange_impl")
}

#' @method arrange_impl default
arrange_impl.default <- function(.data, ...) {
  dplyr::arrange(
    .data = .data,
    # dplyr::pick() doesn't support arrow tables
    dplyr::pick(...)
  )

}

#' @method arrange_impl arrow_dplyr_query
arrange_impl.arrow_dplyr_query <- function(.data, ...) {
  dplyr::arrange(
    .data = .data,
    # It seems that dplyr::pick() doesn't support arrow tables
    dplyr::across(c(...))
  )
}

arrange_impl.ArrowTabular <- arrange_impl.arrow_dplyr_query
arrange_impl.Dataset <- arrange_impl.arrow_dplyr_query

utils::globalVariables(c(".data", ":="))
