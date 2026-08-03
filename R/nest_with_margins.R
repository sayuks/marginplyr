#' Nest data with SQL-style grouping margins
#'
#' [nest_with_margins()] creates one nested data frame for every group in every
#' grouping set. It works with local data frames and `dtplyr` steps.
#'
#' @inheritParams summarize_with_margins
#' @inheritSection summarize_with_margins Fixed columns and grouping dimensions
#' @inheritSection summarize_with_margins Grouped and row-wise inputs
#' @inheritSection summarize_with_margins Grouping set identifiers
#' @inheritSection summarize_with_margins Display labels and grouping identity
#' @inheritSection summarize_with_margins Backend extension design
#' @param .data A local data frame or a `dtplyr` step. Other lazy tables are
#'   not supported because nesting creates list columns.
#' @param .key A string naming the list column. As in [tidyr::nest()],
#'   `NULL` uses `"data"`.
#' @param .keep Should fixed `.by` columns and grouping dimensions also be kept
#'   inside each nested data frame? If `TRUE`, the nested columns contain their
#'   original, pre-margin values rather than `.margin_label`.
#' @param .duplicates `"error"` or `"drop"`. Nesting does not support the
#'   `"keep"` policy available in [summarize_with_margins()] and
#'   [expand_with_margins()].
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
#' input grouping.
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
#'   `dtplyr` input returns a lazy `dtplyr` step until collected. Result row
#'   order is unspecified; use [dplyr::arrange()] when presentation order
#'   matters.
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
#'   .data = january_sales,
#'   .grouping = rollup(region, store)
#' )
#' # `january_sales` is a plain data frame and the outer class is preserved,
#' # so `nested` prints its list column as flattened values. A tibble prints
#' # each nested table as its dimensions instead.
#' nested |>
#'   dplyr::as_tibble() |>
#'   head()
#' nested$data[[1]]
#'
#' # Keep original region and store values inside each nested table as well.
#' # The outer columns still identify the margin level.
#' nested_with_keys <- nest_with_margins(
#'   .data = january_sales,
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
#' names(nest_with_margins(
#'   .data = january_sales,
#'   .by = region,
#'   .key = NULL
#' ))
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
                              .margin_label_position = c("last", "first"),
                              .check_margin_label = is.data.frame(.data),
                              .duplicates = c("error", "drop"),
                              .id = NULL,
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
    .margin_label_position = .margin_label_position,
    .check_margin_label = .check_margin_label,
    .duplicates = .duplicates,
    .id = .id,
    .key = .key,
    .keep = .keep,
    call = call
  )
}

# The nesting verbs narrow the Margin `.duplicates` vocabulary: nested
# duplicate sets would share indistinguishable outer keys, so `"keep"` is not
# offered. See the note in R/margin-operation.R on why the formals still spell
# it out.
nest_duplicates_choices <- c("error", "drop")

nest_margin_pipeline <- function(.data,
                                 by_quo,
                                 grouping_quo,
                                 .margin_label,
                                 .margin_label_position,
                                 .check_margin_label,
                                 .duplicates,
                                 .id,
                                 .key,
                                 .keep,
                                 call) {
  stopifnot(rlang::is_quosure(by_quo), rlang::is_quosure(grouping_quo))

  with_margin_error_call(
    {
      assert_nest_possible(.data)
      assert_logical_scalar(.keep)
      assert_string_scalar(.key)
      if (is.na(.key)) {
        abort_marginplyr(
          "`.key` must not be missing."
        )
      }
      if (!nzchar(.key)) {
        abort_marginplyr(
          "`.key` must not be empty."
        )
      }
      # The nesting verbs' `.duplicates` formal is the whole vocabulary, so
      # receiving it unchanged means the caller left the argument at its
      # default. Resolve it here because the shared normalizer matches against
      # the wider Margin vocabulary.
      left_at_default <- identical(
        .duplicates,
        nest_duplicates_choices
      )
      if (left_at_default) {
        .duplicates <- "error"
      }
      options <- normalize_margin_options(
        .margin_label = .margin_label,
        .margin_label_position = .margin_label_position,
        .check_margin_label = .check_margin_label,
        .duplicates = .duplicates,
        .id = .id
      )
      set_id_name <- options$set_id_name
      .margin_label <- options$margin_label
      .margin_label_position <- options$margin_label_position
      .check_margin_label <- options$check_margin_label
      .duplicates <- options$duplicates
      check_margin_id_collision(set_id_name, .key, "nesting `.key`")
      if (identical(.duplicates, "keep")) {
        abort_marginplyr(
          paste0(
            "Nesting does not support `.duplicates = \"keep\"`. Use ",
            "`\"error\"` or `\"drop\"`."
          )
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
    .margin_label_position = .margin_label_position,
    .check_margin_label = .check_margin_label,
    .duplicates = .duplicates,
    .id = .id,
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
        abort_marginplyr(
          sprintf("`.key` (`%s`) must not be a grouping column.", .key)
        )
      }

      internal_names <- new_margin_internal_names(
        as.integer(is.null(operation$set_id_name)) +
          if (.keep) length(group_cols) else 0L,
        used_names = unique(c(
          operation$data_vars,
          operation$set_id_name,
          .key
        )),
        prefix = "..marginplyr_nest_"
      )
      set_col <- if (is.null(operation$set_id_name)) {
        internal_names[[1L]]
      } else {
        operation$set_id_name
      }
      keep_cols <- if (.keep && length(group_cols) > 0L) {
        stats::setNames(
          utils::tail(internal_names, length(group_cols)),
          group_cols
        )
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
        margin_labels = operation$margin_labels,
        column_info = operation$column_info,
        set_id_name = set_col
      )

      nest_expanded_margins(
        expanded,
        group_cols = group_cols,
        set_col = set_col,
        keep_cols = keep_cols,
        .key = .key,
        .keep = .keep,
        set_id_name = operation$set_id_name
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
                                  .keep,
                                  set_id_name = NULL) {
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

  if (is.null(set_id_name)) {
    result <- dplyr::select(result, -dplyr::all_of(set_col))
  }
  result
}

utils::globalVariables(":=")
