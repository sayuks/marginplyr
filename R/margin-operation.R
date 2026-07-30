new_margin_operation <- function(data,
                                 backend,
                                 data_vars,
                                 data_proxy,
                                 plan,
                                 column_info,
                                 margin_label,
                                 check_margin_label,
                                 call) {
  structure(
    list(
      data = data,
      backend = backend,
      data_vars = data_vars,
      data_proxy = data_proxy,
      plan = plan,
      column_info = column_info,
      margin_label = margin_label,
      check_margin_label = check_margin_label,
      call = call
    ),
    class = "marginplyr_margin_operation"
  )
}

check_margin_operation <- function(operation) {
  stopifnot(inherits(operation, "marginplyr_margin_operation"))
  invisible(operation)
}

with_margin_error_call <- function(expr, call) {
  tryCatch(
    expr,
    error = function(cnd) {
      cnd$call <- call
      stop(cnd)
    }
  )
}

normalize_margin_options <- function(.margin_label,
                                     .check_margin_label,
                                     .duplicates) {
  assert_logical_scalar(.check_margin_label)

  list(
    margin_label = normalize_margin_label(.margin_label),
    check_margin_label = .check_margin_label,
    duplicates = match.arg(
      .duplicates,
      choices = c("error", "drop", "keep")
    )
  )
}

normalize_margin_input <- function(.data, by_quo) {
  stopifnot(rlang::is_quosure(by_quo))

  if (inherits(.data, "rowwise_df")) {
    stop(
      "`rowwise()` input is not supported. Call `dplyr::ungroup()` first.",
      call. = FALSE
    )
  }

  if (!dplyr::group_by_drop_default(.data)) {
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

prepare_margin_operation <- function(.data,
                                     by_quo,
                                     grouping_quo,
                                     .margin_label,
                                     .check_margin_label,
                                     .duplicates,
                                     call = rlang::caller_call()) {
  stopifnot(rlang::is_quosure(by_quo), rlang::is_quosure(grouping_quo))

  with_margin_error_call(
    {
      options <- normalize_margin_options(
        .margin_label = .margin_label,
        .check_margin_label = .check_margin_label,
        .duplicates = .duplicates
      )
      .margin_label <- options$margin_label
      .check_margin_label <- options$check_margin_label
      .duplicates <- options$duplicates

      grouping_spec <- rlang::eval_tidy(grouping_quo)
      validate_grouping_spec_early(grouping_spec)

      input <- normalize_margin_input(.data, by_quo)
      data <- input$data
      by <- input$by
      backend <- grouping_backend(data)
      data_vars <- get_col_names(data, dplyr::everything())
      preflight <- preflight_grouping_spec(grouping_spec, data_vars)
      grouping_spec <- preflight$spec
      if (preflight$name_only) {
        # Reject name-only plan errors before acquiring typed metadata. The
        # canonical plan is compiled from the typed snapshot below.
        compile_grouping_spec_impl(
          grouping_spec,
          data_vars = data_vars,
          data_proxy = grouping_name_proxy(data_vars),
          .by = by,
          .duplicates = .duplicates
        )
      }
      data_proxy <- grouping_selection_proxy(data, backend = backend)
      plan <- compile_grouping_spec_impl(
        grouping_spec,
        data_vars = data_vars,
        data_proxy = data_proxy,
        .by = by,
        .duplicates = .duplicates
      )
      column_info <- margin_column_info(
        data_proxy,
        plan$dimensions,
        backend = backend
      )

      new_margin_operation(
        data = data,
        backend = backend,
        data_vars = data_vars,
        data_proxy = data_proxy,
        plan = plan,
        column_info = column_info,
        margin_label = .margin_label,
        check_margin_label = .check_margin_label,
        call = call
      )
    },
    call = call
  )
}

validate_margin_operation <- function(operation) {
  check_margin_operation(operation)
  with_margin_error_call(
    validate_margin_label(
      operation$data,
      dimensions = operation$plan$dimensions,
      .margin_label = operation$margin_label,
      .check_margin_label = operation$check_margin_label,
      column_info = operation$column_info,
      backend = operation$backend
    ),
    call = operation$call
  )
}

finalize_margin_operation <- function(operation, result) {
  check_margin_operation(operation)
  result <- dplyr::ungroup(result)
  result <- restore_margin_factors(
    result,
    factor_info = operation$column_info$factors,
    .margin_label = operation$margin_label
  )
  margin_cols <- c(operation$plan$by, operation$plan$dimensions)
  result <- dplyr::select(
    result,
    dplyr::all_of(margin_cols),
    dplyr::everything()
  )

  result
}
