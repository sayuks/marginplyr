#' Summarize data with SQL-style grouping operations
#'
#' `summarize_with_margins()` extends [dplyr::summarize()] with grouping sets,
#' rollups, cubes, totals, and subtotals. The same interface works with local
#' data frames and lazy tables.
#'
#' @param .data A data frame or lazy table.
#' @param ... Name-value pairs as used in [dplyr::summarize()]. Contextual
#'   helpers [grouping()] and [grouping_id()] can also be used here.
#' @param .by <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns included in
#'   every grouping set. These columns never receive `.margin_label`.
#' @param .grouping A grouping specification made with [grouping_set()],
#'   [grouping_sets()], [rollup()], [cube()], or [grouping_spec()]. `NULL`
#'   represents one empty grouping set.
#' @param .margin_label A character scalar used to display columns omitted from
#'   a grouping set. The default is `"Total"`. Use `NULL` to keep typed missing
#'   values instead of inserting a display label.
#' @param .check_margin_label A logical scalar. If `TRUE`, check whether the
#'   display label already occurs in a grouping column.
#' @param .duplicates One of `"error"`, `"drop"`, or `"keep"`, controlling
#'   duplicate grouping sets after expansion.
#' @param .sort A logical scalar. If `TRUE`, sort by `.by` followed by grouping
#'   dimensions. It defaults to `TRUE` for local data frames and `FALSE` for
#'   lazy tables.
#'
#' @return An ungrouped data frame, or a lazy table when `.data` is lazy.
#'
#' @details
#' `grouping_sets()` forms a union of grouping families. `grouping_spec()`
#' combines its arguments by Cartesian product, matching comma-separated SQL
#' `GROUP BY` items. `grouping_set()` is also used to keep multiple columns
#' together as one composite dimension inside `rollup()` or `cube()`.
#'
#' Grouping specifications accept column selections, not arbitrary SQL
#' expressions. Create computed grouping columns with [dplyr::mutate()] first.
#'
#' Confirmed SQL backends use one `GROUP BY GROUPING SETS` query. Other lazy
#' backends use a portable `UNION ALL` adapter with the same semantics.
#'
#' @family summarize and expand data with margins
#' @export
#' @examples
#' summarize_with_margins(
#'   mtcars,
#'   n = dplyr::n(),
#'   mean_mpg = mean(mpg),
#'   .grouping = rollup(cyl, vs)
#' )
#'
#' summarize_with_margins(
#'   mtcars,
#'   n = dplyr::n(),
#'   level = grouping_id(cyl, vs, gear),
#'   .grouping = grouping_spec(rollup(cyl, vs), cube(gear))
#' )
#'
#' summarize_with_margins(
#'   mtcars,
#'   n = dplyr::n(),
#'   .grouping = grouping_sets(
#'     grouping_set(cyl, vs),
#'     grouping_set(cyl, gear),
#'     grouping_set()
#'   )
#' )
summarize_with_margins <- function(.data,
                                   ...,
                                   .by = NULL,
                                   .grouping = NULL,
                                   .margin_label = "Total",
                                   .check_margin_label = is.data.frame(.data),
                                   .duplicates = c("error", "drop", "keep"),
                                   .sort = is.data.frame(.data)) {
  assert_lazy_table(.data)
  assert_logical_scalar(.check_margin_label)
  assert_logical_scalar(.sort)
  .margin_label <- normalize_margin_label(.margin_label)
  .duplicates <- match.arg(.duplicates)

  dots <- rlang::enquos(...)
  grouping_quo <- rlang::enquo(.grouping)
  grouping_spec <- rlang::eval_tidy(grouping_quo)

  .data <- dplyr::ungroup(.data)
  by <- get_col_names(.data, {{ .by }})
  data_vars <- get_col_names(.data, dplyr::everything())
  data_proxy <- grouping_selection_proxy(.data)
  plan <- compile_grouping_spec(
    grouping_spec,
    data_vars = data_vars,
    data_proxy = data_proxy,
    .by = by,
    .duplicates = .duplicates
  )
  overwritten_groups <- intersect(
    names(dots)[nzchar(names(dots))],
    c(plan$by, plan$dimensions)
  )
  if (length(overwritten_groups) > 0L) {
    stop(
      "Summary results cannot overwrite grouping column",
      if (length(overwritten_groups) == 1L) " " else "s ",
      paste0("`", overwritten_groups, "`", collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  column_info <- margin_column_info(.data, plan$dimensions)
  validate_margin_label(
    .data,
    dimensions = plan$dimensions,
    .margin_label = .margin_label,
    .check_margin_label = .check_margin_label,
    column_info = column_info
  )

  result <- if (supports_grouping_sets(.data, plan)) {
    summarize_grouping_sets(
      .data,
      dots = dots,
      plan = plan,
      .margin_label = .margin_label
    )
  } else {
    summarize_margin_union(
      .data,
      dots = dots,
      plan = plan,
      .margin_label = .margin_label,
      column_info = column_info
    )
  }

  finish_margin_result(
    result,
    plan = plan,
    factor_info = column_info$factors,
    .margin_label = .margin_label,
    .sort = .sort
  )
}

summarize_impl <- function(.data,
                           ...,
                           .margin_pairs,
                           .by) {
  UseMethod("summarize_impl")
}

#' @exportS3Method
#' @noRd
summarize_impl.default <- function(.data,
                                   ...,
                                   .margin_pairs,
                                   .by) {
  result <- dplyr::summarize(
    .data = dplyr::group_by(.data, dplyr::pick(dplyr::all_of(.by))),
    ...,
    !!!.margin_pairs,
    .groups = "drop"
  )
  result
}

#' @exportS3Method
#' @noRd
summarize_impl.arrow_dplyr_query <- function(.data,
                                             ...,
                                             .margin_pairs,
                                             .by) {
  result <- dplyr::summarize(
    .data = dplyr::group_by(.data, dplyr::across(dplyr::all_of(.by))),
    ...,
    !!!.margin_pairs,
    .groups = "drop"
  )
  result
}

#' @exportS3Method
#' @noRd
summarize_impl.ArrowTabular <- summarize_impl.arrow_dplyr_query
#' @exportS3Method
#' @noRd
summarize_impl.Dataset <- summarize_impl.arrow_dplyr_query

assert_margin_name <- function(data, col_names, margin_name) {
  assert_string_scalar(margin_name)
  stopifnot(is.character(col_names), !anyNA(col_names))

  data <- dplyr::select(.data = data, dplyr::all_of(col_names))
  found <- vapply(
    col_names,
    function(col) {
      rows <- if (is.na(margin_name)) {
        dplyr::filter(data, is.na(.data[[col]])) # nolint: object_usage_linter
      } else {
        dplyr::filter( # nolint: object_usage_linter
          data,
          .data[[col]] == margin_name # nolint: object_usage_linter
        )
      }
      count <- dplyr::collect(dplyr::summarize(rows, n = dplyr::n()))
      nrow(count) > 0L && count$n[[1]] > 0L
    },
    logical(1)
  )

  if (!any(found)) {
    return(invisible(NULL))
  }

  bad_cols <- paste0("`", names(found)[found], "`", collapse = ", ")
  label <- if (is.na(margin_name)) "NA" else paste0('"', margin_name, '"')
  stop(
    label,
    " is already present in grouping column",
    if (sum(found) == 1L) " " else "s ",
    bad_cols,
    ". Choose another `.margin_label` or set ",
    "`.check_margin_label = FALSE`.",
    call. = FALSE
  )
}

get_col_names <- function(data, ...) {
  UseMethod("get_col_names")
}

#' @exportS3Method
#' @noRd
get_col_names.default <- function(data, ...) {
  colnames(dplyr::select(.data = data, ...))
}

#' @exportS3Method
#' @noRd
get_col_names.arrow_dplyr_query <- function(data, ...) {
  data <- dplyr::select(.data = data, ...)
  data <- dplyr::collect(utils::head(x = data, n = 0L))
  colnames(data)
}

#' @exportS3Method
#' @noRd
get_col_names.ArrowTabular <- get_col_names.arrow_dplyr_query
#' @exportS3Method
#' @noRd
get_col_names.Dataset <- get_col_names.arrow_dplyr_query
#' @exportS3Method
#' @noRd
get_col_names.dtplyr_step <- get_col_names.arrow_dplyr_query

relocate_before_union_all <- function(.data, cols_first) {
  UseMethod("relocate_before_union_all")
}

#' @exportS3Method
#' @noRd
relocate_before_union_all.default <- function(.data, cols_first) {
  dplyr::relocate(.data, dplyr::all_of(cols_first))
}

#' @exportS3Method
#' @noRd
relocate_before_union_all.arrow_dplyr_query <- function(.data, cols_first) {
  dplyr::select(
    .data,
    dplyr::all_of(cols_first),
    dplyr::everything()
  )
}

#' @exportS3Method
#' @noRd
relocate_before_union_all.ArrowTabular <-
  relocate_before_union_all.arrow_dplyr_query
#' @exportS3Method
#' @noRd
relocate_before_union_all.Dataset <- relocate_before_union_all.arrow_dplyr_query

#' @exportS3Method
#' @noRd
relocate_before_union_all.dtplyr_step <- function(.data, cols_first) {
  dplyr::relocate(.data, dplyr::all_of(cols_first))
}

relocate_post_proc <- function(.data, ...) {
  UseMethod("relocate_post_proc")
}

#' @exportS3Method
#' @noRd
relocate_post_proc.default <- function(.data, ...) {
  dplyr::relocate(.data, ...)
}

#' @exportS3Method
#' @noRd
relocate_post_proc.arrow_dplyr_query <- function(.data, ...) .data
#' @exportS3Method
#' @noRd
relocate_post_proc.ArrowTabular <- relocate_post_proc.arrow_dplyr_query
#' @exportS3Method
#' @noRd
relocate_post_proc.Dataset <- relocate_post_proc.arrow_dplyr_query

arrange_impl <- function(.data, ...) {
  UseMethod("arrange_impl")
}

#' @exportS3Method
#' @noRd
arrange_impl.default <- function(.data, ...) {
  dplyr::arrange(.data, dplyr::pick(...))
}

#' @exportS3Method
#' @noRd
arrange_impl.arrow_dplyr_query <- function(.data, ...) {
  dplyr::arrange(.data, dplyr::across(c(...)))
}
