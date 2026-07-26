prepare_margin_input <- function(.data, by_quo) {
  stopifnot(rlang::is_quosure(by_quo))

  if (inherits(.data, "rowwise_df")) {
    stop(
      "`rowwise()` input is not supported. Call `dplyr::ungroup()` first.",
      call. = FALSE
    )
  }

  if (
    inherits(.data, "grouped_df") &&
      !dplyr::group_by_drop_default(.data)
  ) {
    stop(
      "Grouped input created with `.drop = FALSE` is not supported. ",
      "Call `dplyr::ungroup()` first.",
      call. = FALSE
    )
  }

  input_groups <- dplyr::group_vars(.data)
  if (length(input_groups) > 0L && !rlang::quo_is_null(by_quo)) {
    stop(
      "Can't supply `.by` when `.data` is grouped. ",
      "Call `dplyr::ungroup()` first.",
      call. = FALSE
    )
  }

  .data <- dplyr::ungroup(.data)
  by <- if (length(input_groups) > 0L) {
    input_groups
  } else {
    rlang::inject(get_col_names(.data, !!by_quo))
  }

  list(data = .data, by = by)
}
