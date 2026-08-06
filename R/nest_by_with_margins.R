#' Nest by SQL-style grouping margins
#'
#' This is the row-wise counterpart of [nest_with_margins()].
#'
#' @inheritParams nest_with_margins
#' @inheritSection summarize_with_margins Fixed columns and grouping dimensions
#' @inheritSection summarize_with_margins Grouped and row-wise inputs
#' @inheritSection summarize_with_margins Result class and attributes
#' @inheritSection summarize_with_margins Grouping set identifiers
#' @inheritSection summarize_with_margins Margin order
#' @inheritSection summarize_with_margins Display labels and grouping identity
#' @inheritSection summarize_with_margins Backend extension design
#' @inheritSection nest_with_margins Relationship to tidyr and dplyr
#' @param .key A non-missing string naming the list column. Unlike
#'   [nest_with_margins()] and [tidyr::nest()], `NULL` is not converted to
#'   `"data"`; this follows [dplyr::nest_by()].
#' @details A `dtplyr` result is collected before it is made row-wise because
#'   row-wise data frames are local objects.
#'
#' [dplyr::nest_by()] is not a stable upstream API and may eventually be
#' deprecated in favor of [tidyr::nest()]. This margin-aware wrapper remains
#' intentional because its row-wise return shape is useful for per-margin
#' models and reports. Both public nesting interfaces use the same private
#' margin-operation pipeline, so they share one grouping plan and nesting
#' contract without invoking each other.
#' @return A row-wise data frame grouped by the visible grouping columns and
#'   `.id` when supplied. This is the return shape whatever the class of
#'   `.data`, because a row-wise data frame is always a tibble subclass; see
#'   *Result class and attributes*.
#'   Result row order is unspecified unless `.sort` asks for a Margin order;
#'   see *Margin order*, or use [dplyr::arrange()] for any other presentation
#'   order.
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
#' # dtplyr performs the expansion and nesting lazily, then
#' # nest_by_with_margins() collects it to create a local row-wise data frame.
#' if (requireNamespace("dtplyr", quietly = TRUE)) {
#'   lazy_nested <- january_sales |>
#'     dtplyr::lazy_dt() |>
#'     nest_with_margins(
#'       .grouping = rollup(region, store)
#'     )
#'
#'   # Inspect the data.table expression before the row-wise collection.
#'   print(dplyr::show_query(lazy_nested))
#'
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
  call <- rlang::current_call()
  grouping_quo <- rlang::enquo(.grouping)
  by_quo <- rlang::enquo(.by)

  result <- nest_margin_pipeline(
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
    call = call
  )

  result <- dplyr::collect(result)
  empty_outer_names <- c(.id, .key)
  if (
    nrow(result) == 0L &&
      identical(colnames(result), empty_outer_names)
  ) {
    empty_data <- dplyr::collect(
      utils::head(dplyr::ungroup(.data), n = 0L)
    )
    result <- dplyr::summarize(
      empty_data,
      "{.key}" := list(dplyr::pick(dplyr::everything()))
    )
    if (!is.null(.id)) {
      result <- dplyr::mutate(result, "{.id}" := 1L)
      result <- dplyr::select(
        result,
        dplyr::all_of(.id),
        dplyr::everything()
      )
    }
  }
  group_cols <- setdiff(colnames(result), .key)
  dplyr::rowwise(result, dplyr::all_of(group_cols))
}
