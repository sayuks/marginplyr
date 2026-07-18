#' Nest data with SQL-style grouping margins
#'
#' `nest_with_margins()` creates one nested data frame for every group in every
#' grouping set. It works with local data frames and `dtplyr` steps.
#'
#' @inheritParams summarize_with_margins
#' @param .key A non-missing string naming the list column.
#' @param .keep Should grouping columns also be kept inside each nested data
#'   frame?
#'
#' @return An ungrouped data frame with one list column.
#' @family summarize and expand data with margins
#' @export
#' @examples
#' nest_with_margins(
#'   mtcars,
#'   .grouping = rollup(cyl, vs)
#' )
nest_with_margins <- function(.data,
                              .by = NULL,
                              .grouping = NULL,
                              .margin_label = "Total",
                              .check_margin_label = TRUE,
                              .duplicates = c("error", "drop", "keep"),
                              .sort = TRUE,
                              .key = "data",
                              .keep = FALSE) {
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
  if (identical(.key, ".marginplyr_set_id")) {
    stop(
      "`.key` must not use the reserved name `.marginplyr_set_id`.",
      call. = FALSE
    )
  }

  .margin_label <- normalize_margin_label(.margin_label)
  .duplicates <- match.arg(.duplicates)
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
  group_cols <- c(plan$by, plan$dimensions)

  if (.key %in% group_cols) {
    stop(
      sprintf("`.key` (`%s`) must not be a grouping column.", .key),
      call. = FALSE
    )
  }
  if (".marginplyr_set_id" %in% data_vars) {
    stop(
      "Input column `.marginplyr_set_id` conflicts with an internal column.",
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

  expanded <- expand_margin_union(
    .data,
    plan = plan,
    .margin_label = .margin_label,
    column_info = column_info,
    include_set_id = TRUE
  )

  nested <- nest_expanded_margins(
    expanded,
    group_cols = group_cols,
    .key = .key,
    .keep = .keep,
    data_vars = data_vars
  )

  finish_margin_result(
    nested,
    plan = plan,
    factor_info = column_info$factors,
    .margin_label = .margin_label,
    .sort = .sort
  )
}

nest_expanded_margins <- function(.data,
                                  group_cols,
                                  .key,
                                  .keep,
                                  data_vars) {
  set_col <- ".marginplyr_set_id"
  temp_cols <- paste0(group_cols, "_COPY__MARGINPLYR_")

  conflicts <- intersect(temp_cols, data_vars)
  if (.keep && length(conflicts) > 0L) {
    stop(
      "`.keep = TRUE` requires temporary column names that conflict with: ",
      paste0("`", conflicts, "`", collapse = ", "),
      call. = FALSE
    )
  }

  if (.keep && length(group_cols) > 0L) {
    .data <- dplyr::mutate(
      .data,
      dplyr::across(
        dplyr::all_of(group_cols),
        identity,
        .names = "{.col}_COPY__MARGINPLYR_"
      )
    )
  }

  grouped <- dplyr::group_by(
    .data,
    dplyr::pick(dplyr::all_of(c(group_cols, set_col)))
  )

  if (.keep && length(group_cols) > 0L) {
    rename_map <- stats::setNames(temp_cols, group_cols)
    result <- dplyr::summarize(
      grouped,
      "{.key}" := list({
        nested <- dplyr::rename(
          dplyr::pick(dplyr::everything()),
          dplyr::all_of(rename_map)
        )
        dplyr::relocate(nested, dplyr::all_of(group_cols))
      }),
      .groups = "drop"
    )
  } else {
    result <- dplyr::summarize(
      grouped,
      "{.key}" := list(dplyr::pick(dplyr::everything())),
      .groups = "drop"
    )
  }

  dplyr::select(result, -dplyr::all_of(set_col))
}

utils::globalVariables(":=")
