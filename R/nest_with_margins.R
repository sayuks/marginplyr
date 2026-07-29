#' Nest data with SQL-style grouping margins
#'
#' [nest_with_margins()] creates one nested data frame for every group in every
#' grouping set. It works with local data frames and `dtplyr` steps.
#'
#' @inheritParams summarize_with_margins
#' @inheritSection summarize_with_margins Fixed columns and grouping dimensions
#' @inheritSection summarize_with_margins Grouped and row-wise inputs
#' @inheritSection summarize_with_margins Backend extension design
#' @param .data A local data frame or a `dtplyr` step. Other lazy tables are
#'   not supported because nesting creates list columns.
#' @param .sort A logical scalar. If `TRUE` (the default), sort by `.by`
#'   followed by grouping dimensions.
#' @param .key A string naming the list column. As in [tidyr::nest()],
#'   `NULL` uses `"data"`.
#' @param .keep Should fixed `.by` columns and grouping dimensions also be kept
#'   inside each nested data frame? If `TRUE`, the nested columns contain their
#'   original, pre-margin values rather than `.margin_label`.
#' @param .duplicates `"error"` or `"drop"`. The `"keep"` policy available in
#'   [summarize_with_margins()] and [expand_with_margins()] is rejected because
#'   duplicate grouping sets would create indistinguishable outer keys.
#'
#' @section Relationship to tidyr and dplyr:
#' These functions are margin-aware counterparts, not drop-in replacements,
#' and do not implement every upstream feature. [nest_with_margins()] resembles
#' [tidyr::nest()] with `.by`: it supports `.key`, returns an ungrouped data
#' frame, and puts all non-key columns into one list column. Setting
#' `.keep = TRUE` also puts the original grouping keys in that inner data
#' frame. This corresponds to selecting those keys in [tidyr::nest()] (for
#' example, `nest(data = everything(), .by = region)`), but uses a logical
#' argument because margin dimensions must remain visible outside to identify
#' detail, subtotal, and total rows.
#'
#' [nest_with_margins()] does not implement the full `...` column
#' specification, multiple list columns, or `.names_sep`. Existing grouping
#' columns become implicit fixed keys, as they do for [tidyr::nest()], but
#' [nest_with_margins()] returns an ungrouped result instead of preserving the
#' input grouping. With the default `.sort = TRUE`, keys are sorted rather than
#' kept in first-appearance order.
#'
#' [nest_by_with_margins()] resembles [dplyr::nest_by()] and, like it, provides
#' `.key` and `.keep`, but selects fixed keys with `.by` rather than `...`.
#' With `.keep = TRUE`, the nested data contains the original, pre-margin
#' values of every outer key. Thus an outer `East / Total` subtotal retains
#' the original store values inside the nested data instead of replacing them
#' with `"Total"`. This is the closest margin-aware analogue of dplyr's rule
#' that `.keep` retains grouping columns.
#'
#' The list column is a regular list of data frames; its exact `vctrs_list_of`
#' subclass is not part of the API. [nest_with_margins()] follows
#' [tidyr::nest()] for an empty ungrouped input and returns zero outer rows.
#' [nest_by_with_margins()] follows [dplyr::nest_by()] and returns one row
#' containing the empty input when there are no grouping keys.
#'
#' No input column name is reserved for internal bookkeeping. Temporary
#' grouping-set and `.keep` columns are generated collision-free and removed
#' before the result is returned.
#'
#' @return For a local input, an ungrouped data frame with one list column. A
#'   `dtplyr` input returns a lazy `dtplyr` step until collected.
#' @family summarize and expand data with margins
#' @export
#' @examples
#' # Keep the source rows behind each store, region subtotal, and company
#' # total so they can feed separate report sections.
#' january_sales <- dplyr::filter(
#'   retail_sales,
#'   year == 2026L,
#'   month == "Jan"
#' )
#' nested <- nest_with_margins(
#'   january_sales,
#'   .grouping = rollup(region, store)
#' )
#' # Convert to a tibble so nested data frames print compactly.
#' nested |>
#'   dplyr::as_tibble() |>
#'   head()
#' nested$data[[1]]
#'
#' # Keep original region and store values inside each nested table as well.
#' # The outer columns still identify the margin level.
#' nested_with_keys <- nest_with_margins(
#'   january_sales,
#'   .grouping = rollup(region, store),
#'   .keep = TRUE
#' )
#' names(nested_with_keys$data[[1]])
#' total <- dplyr::filter(
#'   nested_with_keys,
#'   region == "Total",
#'   store == "Total"
#' )
#' unique(total$data[[1]][c("region", "store")])
#'
#' # NULL uses the same default list-column name as tidyr::nest().
#' names(nest_with_margins(january_sales, .by = region, .key = NULL))
#'
#' # Existing groups become fixed outer keys, while the result itself is
#' # ungrouped.
#' grouped_nested <- retail_sales |>
#'   dplyr::group_by(year) |>
#'   nest_with_margins(
#'     .grouping = rollup(region, store)
#'   )
#' dplyr::group_vars(grouped_nested)
#'
#' # The same operation stays lazy for a dtplyr input until collect().
#' if (requireNamespace("dtplyr", quietly = TRUE)) {
#'   nested_dt <- january_sales |>
#'     dtplyr::lazy_dt() |>
#'     nest_with_margins(
#'       .grouping = rollup(region, store)
#'     )
#'   print(dplyr::show_query(nested_dt))
#'   dplyr::collect(nested_dt)
#' }
nest_with_margins <- function(.data,
                              .by = NULL,
                              .grouping = NULL,
                              .margin_label = "Total",
                              .check_margin_label = TRUE,
                              .duplicates = c("error", "drop"),
                              .sort = TRUE,
                              .key = "data",
                              .keep = FALSE) {
  call <- rlang::current_call()
  if (is.null(.key)) {
    .key <- "data"
  }
  grouping_quo <- rlang::enquo(.grouping)
  by_quo <- rlang::enquo(.by)

  nest_margin_pipeline(
    .data = .data,
    by_quo = by_quo,
    grouping_quo = grouping_quo,
    .margin_label = .margin_label,
    .check_margin_label = .check_margin_label,
    .duplicates = .duplicates,
    .sort = .sort,
    .key = .key,
    .keep = .keep,
    call = call
  )
}

nest_margin_pipeline <- function(.data,
                                 by_quo,
                                 grouping_quo,
                                 .margin_label,
                                 .check_margin_label,
                                 .duplicates,
                                 .sort,
                                 .key,
                                 .keep,
                                 call) {
  stopifnot(rlang::is_quosure(by_quo), rlang::is_quosure(grouping_quo))

  with_margin_error_call(
    {
      assert_nest_possible(.data)
      assert_logical_scalar(.check_margin_label)
      assert_logical_scalar(.sort)
      assert_logical_scalar(.keep)
      assert_string_scalar(.key)
      if (is.na(.key)) {
        stop("`.key` must not be missing.", call. = FALSE)
      }
      if (!nzchar(.key)) {
        stop("`.key` must not be empty.", call. = FALSE)
      }
      .margin_label <- normalize_margin_label(.margin_label)
      if (identical(.duplicates, c("error", "drop"))) {
        .duplicates <- "error"
      }
      .duplicates <- match.arg(.duplicates, c("error", "drop", "keep"))
      if (identical(.duplicates, "keep")) {
        stop(
          "Nesting does not support `.duplicates = \"keep\"` because ",
          "duplicate grouping sets have no distinct visible key. Use ",
          "`\"error\"` or `\"drop\"`.",
          call. = FALSE
        )
      }
    },
    call = call
  )

  operation <- prepare_margin_operation(
    .data,
    by_quo = by_quo,
    grouping_quo = grouping_quo,
    .margin_label = .margin_label,
    .check_margin_label = .check_margin_label,
    .duplicates = .duplicates,
    .sort = .sort,
    call = call
  )
  result <- execute_margin_nest(
    operation,
    .key = .key,
    .keep = .keep
  )
  finalize_margin_operation(operation, result)
}

execute_margin_nest <- function(operation, .key, .keep) {
  check_margin_operation(operation)
  with_margin_error_call(
    {
      plan <- operation$plan
      group_cols <- c(plan$by, plan$dimensions)
      if (.key %in% group_cols) {
        stop(
          sprintf("`.key` (`%s`) must not be a grouping column.", .key),
          call. = FALSE
        )
      }

      internal_names <- new_margin_internal_names(
        1L + if (.keep) length(group_cols) else 0L,
        used_names = unique(c(operation$data_vars, .key)),
        prefix = "..marginplyr_nest_"
      )
      set_col <- internal_names[[1L]]
      keep_cols <- if (.keep && length(group_cols) > 0L) {
        stats::setNames(internal_names[-1L], group_cols)
      } else {
        character()
      }
      data <- operation$data
      if (length(keep_cols) > 0L) {
        keep_exprs <- lapply(
          group_cols,
          function(var) rlang::expr(.data[[!!var]])
        )
        names(keep_exprs) <- unname(keep_cols)
        data <- dplyr::mutate(data, !!!keep_exprs)
      }

      validate_margin_operation(operation)

      expanded <- expand_margin_union(
        data,
        plan = plan,
        .margin_label = operation$margin_label,
        column_info = operation$column_info,
        set_id_name = set_col
      )

      nest_expanded_margins(
        expanded,
        group_cols = group_cols,
        set_col = set_col,
        keep_cols = keep_cols,
        .key = .key,
        .keep = .keep
      )
    },
    call = operation$call
  )
}

nest_expanded_margins <- function(.data,
                                  group_cols,
                                  set_col,
                                  keep_cols,
                                  .key,
                                  .keep) {
  if (.keep && length(group_cols) > 0L) {
    result <- dplyr::summarize(
      .data,
      "{.key}" := list({
        nested <- dplyr::rename(
          dplyr::pick(dplyr::everything()),
          dplyr::all_of(keep_cols)
        )
        dplyr::relocate(nested, dplyr::all_of(group_cols))
      }),
      .by = dplyr::all_of(c(group_cols, set_col))
    )
  } else {
    result <- dplyr::summarize(
      .data,
      "{.key}" := list(dplyr::pick(dplyr::everything())),
      .by = dplyr::all_of(c(group_cols, set_col))
    )
  }

  dplyr::select(result, -dplyr::all_of(set_col))
}

utils::globalVariables(":=")
