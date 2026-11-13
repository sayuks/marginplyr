#' Nest by SQL-style grouping margins
#'
#' [nest_by_with_margins()] creates one row-wise nested data frame for every
#' group in every grouping set. It is a margin-aware counterpart of
#' [dplyr::nest_by()], and the row-wise counterpart of [nest_with_margins()];
#' it works with local data frames and `dtplyr` steps.
#'
#' @section Differences from nest_by():
#' - Fixed keys are selected with `.by` rather than `...`, and `.grouping`
#'   adds the Margin dimensions; the result is row-wise over all of them and
#'   `.id` when supplied.
#' - Row order is unspecified. Use `.sort` for margins next to the rows they
#'   cover, or [dplyr::arrange()] for any other order.
#' - A character `.margin_label` changes a non-factor grouping dimension to
#'   character in the outer columns; see Value. Use `.margin_label = NULL` or
#'   `NA_character_` to keep its type.
#' - With `.keep = TRUE`, the nested data holds the original, pre-margin
#'   values of every outer key, so an outer `East / Total` subtotal keeps the
#'   original store values inside instead of `"Total"`. This is the closest
#'   margin-aware analogue of dplyr's rule that `.keep` retains grouping
#'   columns.
#' - With no grouping keys, an empty input returns one row holding the empty
#'   input, as [dplyr::nest_by()] does.
#'
#' @section When marginplyr queries your data:
#' A row-wise data frame exists only locally, so a `dtplyr` input is expanded
#' and nested lazily and then collected before it is made row-wise: unlike
#' the other Margin verbs, [nest_by_with_margins()] processes a `dtplyr`
#' step's rows when called. Use [nest_with_margins()] to keep the result lazy.
#'
#' @inheritParams nest_with_margins
#' @inheritSection nest_with_margins Nested data frames
#' @inheritSection summarize_with_margins Fixed columns and grouping dimensions
#' @inheritSection summarize_with_margins Grouped and row-wise inputs
#' @inheritSection summarize_with_margins Option arguments
#' @inheritSection summarize_with_margins Result metadata
#' @inheritSection summarize_with_margins Grouping set identifiers
#' @inheritSection summarize_with_margins Margin order
#' @inheritSection summarize_with_margins Display labels and grouping identity
#' @param .key A non-missing string naming the list column. Unlike
#'   [nest_with_margins()] and [tidyr::nest()], `NULL` is not converted to
#'   `"data"`; this follows [dplyr::nest_by()].
#' @details dplyr reserves `.rows` in row-wise grouping metadata. A fixed `.by`
#'   key, grouping dimension, or `.id` named `.rows` is therefore refused
#'   before nesting. Rename that outer key or use [nest_with_margins()] for an
#'   ungrouped result. A payload column or `.key` named `.rows` is supported.
#' @return A row-wise data frame, a tibble subclass whatever the class of
#'   `.data`, with one row per group per grouping set. Its columns are the
#'   fixed `.by` keys, then the grouping dimensions, then `.id` when supplied,
#'   then the list column named by `.key`; it is grouped row-wise by all but
#'   the list column. See *Result metadata*.
#'
#'   With a character `.margin_label`, the default, an outer grouping
#'   dimension that is not a factor becomes character so that it can hold the
#'   label, while the nested columns keep their original types. A factor
#'   dimension instead gains the label as a new level.
#' @family summarize and expand data with margins
#' @export
#' @examples
#' january_sales <- dplyr::filter(
#'   retail_sales,
#'   year == 2026L,
#'   month == "Jan"
#' )
#'
#' # A row-wise result makes it natural to calculate one report summary from
#' # every nested detail table.
#' nested_by <- january_sales |>
#'   nest_by_with_margins(
#'     .grouping = rollup(region, store)
#'   )
#' nested_by |>
#'   dplyr::mutate(
#'     records = nrow(data),
#'     units = sum(data$units),
#'     revenue = sum(data$revenue)
#'   )
#' nested_by$data[[1]]
#'
#' # Existing groups become additional fixed row-wise keys.
#' grouped_rowwise <- retail_sales |>
#'   dplyr::group_by(year) |>
#'   nest_by_with_margins(
#'     .grouping = rollup(region, store)
#'   )
#' dplyr::group_vars(grouped_rowwise)
#'
#' # .keep = TRUE retains original key values inside each detail table.
#' kept_keys <- nest_by_with_margins(
#'   .data = january_sales,
#'   .grouping = rollup(region, store),
#'   .keep = TRUE
#' )
#' east_total <- dplyr::filter(
#'   kept_keys,
#'   region == "East",
#'   store == "Total"
#' )
#' east_total$data[[1]][c("region", "store")]
#'
#' # With no grouping keys, nest_by semantics retain one empty nested table.
#' empty_sales <- january_sales[0, ]
#' nrow(nest_by_with_margins(.data = empty_sales))
#'
#' # A dtplyr input is nested lazily, then collected to make the local
#' # row-wise result. Runs only when a recent enough dtplyr is installed.
#' source(system.file("suggests", "guard.R", package = "marginplyr"))
#' if (marginplyr_suggest_available("dtplyr")) {
#'   nested_by_dt <- january_sales |>
#'     dtplyr::lazy_dt() |>
#'     nest_by_with_margins(
#'       .grouping = rollup(region, store)
#'     )
#'   nested_by_dt |>
#'     dplyr::mutate(
#'       records = nrow(data),
#'       revenue = sum(data$revenue)
#'     )
#'   nested_by_dt$data[[1]]
#' }
nest_by_with_margins <- function(.data,
                                 .by = NULL,
                                 .grouping = NULL,
                                 .margin_label = "Total",
                                 .margin_label_position = c("last", "first"),
                                 .check_margin_label = is.data.frame(.data),
                                 .duplicates = c("error", "drop"),
                                 .id = NULL,
                                 .sort = c("none", "last", "first"),
                                 .key = "data",
                                 .keep = FALSE) {
  force(.data)
  suspendInterrupts({
    # The scope is read from this live frame by active_sent_query_scope().
    sent_query_scope <- reset_sent_queries() # nolint: object_usage_linter.
  })
  call <- rlang::current_call()
  grouping_quo <- capture_public_grouping(
    rlang::enquo0(.grouping), substitute(.grouping)
  )
  by_quo <- rlang::enquo(.by)

  nest_margin_pipeline(
    .data = .data,
    by_quo = by_quo,
    grouping_quo = grouping_quo,
    .margin_label = .margin_label,
    .margin_label_position = .margin_label_position,
    .check_margin_label = .check_margin_label,
    .duplicates = .duplicates,
    .sort = .sort,
    .id = .id,
    .key = .key,
    .keep = .keep,
    call = call,
    rowwise = TRUE
  )
}
