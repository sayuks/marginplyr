#' Nest by SQL-style grouping margins
#'
#' @description
#' [nest_by_with_margins()] returns a row-wise data frame with one row for
#' every group in every grouping set, holding that group's rows as a nested
#' data frame. It is a margin-aware counterpart of [dplyr::nest_by()], and
#' the row-wise counterpart of [nest_with_margins()]; it works with local
#' data frames and `dtplyr` steps.
#'
#' Use it when the next step computes one value per nested table, because
#' [dplyr::mutate()] on a row-wise result sees each row's table as `data`.
#' Use [nest_with_margins()] to keep the result ungrouped or a `dtplyr` result
#' lazy; *[Choosing a nesting verb][nest_with_margins]* in
#' [nest_with_margins()] compares the two.
#'
#' @section Differences from nest_by():
#' - Fixed keys are selected with `.by` rather than `...`, and `.grouping`
#'   adds the Margin dimensions; the result is row-wise over all of them and
#'   `.id` when supplied.
#' - Row order is unspecified. Use `.sort` for margins next to the rows they
#'   cover, or [dplyr::arrange()] for any other order.
#' - A character `.margin_label` changes a non-factor Margin dimension to
#'   character in the outer columns; see Value. Use `.margin_label = NULL` or
#'   `NA_character_` to keep its type.
#' - With `.keep = TRUE`, the nested data holds the original, pre-margin
#'   values of every outer key, so an outer `East / Total` subtotal keeps the
#'   original store values inside instead of `"Total"`. This is the closest
#'   margin-aware analogue of dplyr's rule that `.keep` retains grouping
#'   columns. The kept keys come first in each nested data frame, in the
#'   order of the outer columns, followed by the other columns in input
#'   order; [dplyr::nest_by()] keeps input order. Because the kept keys share
#'   their names with the outer columns, [tidyr::unnest()] needs `names_sep`
#'   for such a result.
#' - dplyr reserves `.rows` in row-wise grouping metadata, so a fixed `.by`
#'   key, Margin dimension, or `.id` named `.rows` is refused before
#'   nesting. Rename that outer key or use [nest_with_margins()] for an
#'   ungrouped result. A payload column or `.key` named `.rows` is supported.
#' - The list column is a regular list of data frames rather than the
#'   `vctrs::list_of` column [dplyr::nest_by()] returns; see *Nested data
#'   frames*.
#' - An empty input returns one row holding the empty input only when the
#'   call names no key at all: no `.by` column, no group, and no Margin
#'   dimension. This is what [dplyr::nest_by()] does without grouping
#'   columns. Any key gives zero rows, a Margin dimension included: on an
#'   empty input, `.grouping = rollup(region)` returns no rows, although its
#'   grand-total set omits `region` and [summarize_with_margins()] returns
#'   one total row for it.
#'
#' @section dtplyr inputs:
#' A row-wise data frame exists only locally, so a `dtplyr` input is expanded
#' and nested lazily and then collected before it is made row-wise: unlike
#' the other Margin verbs, [nest_by_with_margins()] processes a `dtplyr`
#' step's rows when called. Use [nest_with_margins()] to keep the result lazy.
#' For the same reason, `.check_margin_label` checks a `dtplyr` step's
#' values by default.
#'
#' @inheritParams nest_with_margins
#' @param .check_margin_label Whether to check that no value of a Margin
#'   dimension equals that dimension's display label. The default is `TRUE`
#'   for both inputs this verb accepts, a local data frame and a `dtplyr`
#'   step, whose rows it reads when called; see *dtplyr inputs* below.
#'
#'   A label equal to a declared factor level is refused whatever this
#'   argument says. A typed-missing label, `NULL` or `NA_character_`, takes
#'   its dimension out of the check. Left unchecked, a colliding value gives
#'   the result a margin row and a source row that no key column tells apart;
#'   keeping `.id` in the result distinguishes them.
#' @inheritSection nest_with_margins Nested data frames
#' @section Behavior shared with the other Margin verbs:
#' These rules are stated once, in [margin_verbs], for every Margin verb:
#'
#' - [Fixed columns and Margin dimensions][margin_verbs]
#' - [Grouped and row-wise inputs][margin_verbs]
#' - [Option arguments][margin_verbs]
#' - [Result metadata][margin_verbs]
#' - [Grouping set identifiers][margin_verbs]
#' - [Margin order][margin_verbs]
#' - [Display labels and grouping identity][margin_verbs]
#'
#' @param .key A non-missing string naming the list column. Unlike
#'   [nest_with_margins()] and [tidyr::nest()], `NULL` is not converted to
#'   `"data"`; this follows [dplyr::nest_by()].
#' @return A row-wise data frame, a tibble subclass whatever the class of
#'   `.data`, with one row per group per grouping set. Its columns are the
#'   fixed `.by` keys, then the Margin dimensions, then `.id` when supplied,
#'   then the list column named by `.key`; it is grouped row-wise by all but
#'   the list column. See *[Result metadata][margin_verbs]* in
#'   [margin_verbs].
#'
#'   With a character `.margin_label`, the default, an outer Margin
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
#' # every nested detail table. `.sort` puts each subtotal after its stores.
#' nested_by <- january_sales |>
#'   nest_by_with_margins(
#'     .grouping = rollup(region, store),
#'     .sort = "last"
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
#' # An empty input keeps one empty nested table only when the call names no
#' # key; a Margin dimension gives zero rows.
#' empty_sales <- january_sales[0, ]
#' nrow(nest_by_with_margins(.data = empty_sales))
#' nrow(nest_by_with_margins(.data = empty_sales, .grouping = rollup(region)))
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
#'   print(class(nested_by_dt))
#'   print(
#'     dplyr::mutate(
#'       nested_by_dt,
#'       records = nrow(data),
#'       revenue = sum(data$revenue)
#'     )
#'   )
#'   nested_by_dt$data[[1]]
#' }
nest_by_with_margins <- function(.data,
                                 .by = NULL,
                                 .grouping = NULL,
                                 .margin_label = "Total",
                                 .margin_label_position = c("last", "first"),
                                 .check_margin_label = is.data.frame(.data) ||
                                   inherits(.data, "dtplyr_step"),
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
